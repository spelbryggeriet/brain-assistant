mod expr;
mod parse;
mod reduce;

use std::{
    borrow::Cow,
    io::{self, Stdout, Write},
};

use anyhow::Context;
use clap::{command, Arg, ArgAction};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyModifiers},
    execute, queue,
    style::{Color as CColor, Print, SetForegroundColor},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use once_cell::sync::Lazy;
use ratatui::{
    backend::CrosstermBackend,
    style::{Color, Style},
    text::{Line, Span, Text},
    widgets::Paragraph,
    Terminal,
};

fn main() {
    let matches = command!()
        .arg(Arg::new("expression").action(ArgAction::Append))
        .get_matches();

    Lazy::force(&reduce::RULES);

    let expression: String = matches
        .get_many::<String>("expression")
        .unwrap_or_default()
        .map(|s| s.as_str())
        .collect();

    if !expression.is_empty() {
        process_expr(&expression, &mut None);
    } else {
        let mut terminal = match setup_terminal() {
            Ok(terminal) => terminal,
            Err(err) => {
                report_fatal_error(err, &mut None);
                return;
            }
        };

        match repl(&mut terminal) {
            Ok(()) => (),
            Err(err) => report_fatal_error(err, &mut None),
        }

        match restore_terminal(terminal) {
            Ok(()) => (),
            Err(err) => report_fatal_error(err, &mut None),
        }
    }
}

fn setup_terminal() -> anyhow::Result<Terminal<CrosstermBackend<Stdout>>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    Ok(Terminal::new(backend)?)
}

fn restore_terminal(mut terminal: Terminal<CrosstermBackend<Stdout>>) -> anyhow::Result<()> {
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;
    Ok(())
}

macro_rules! style {
    (@impl (#$color:ident($e:expr) $(, $($rest:tt)*)?) => [$($parsed:tt)*]) => {
        style!(@impl ($($($rest)*)?) => [$($parsed)* Span::styled($e, Style::default().fg(Color::$color)),])
    };

    (@impl ($e:expr $(, $($rest:tt)*)?) => [$($parsed:tt)*]) => {
        style!(@impl ($($($rest)*)?) => [$($parsed)* Span::raw($e),])
    };

    (@impl () => [$($parsed:tt)*]) => {
       Line { spans: vec![$($parsed)*], ..Default::default() }
    };

    ($($tokens:tt)*) => {
        style!(@impl ($($tokens)*) => [])
    };
}

fn repl(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> anyhow::Result<()> {
    let mut text = Text::default();

    macro_rules! draw_repl {
        () => {{
            terminal
                .draw(|f| {
                    f.render_widget(Paragraph::new(text.clone()), f.size());
                })
                .context("drawing repl")?;
        }};
    }

    loop {
        println(
            style!(#Yellow(">>"), " ", Cow::Owned(String::new()), "▏"),
            &mut Some(&mut text.lines),
        );

        draw_repl!();

        let input = loop {
            if let Event::Key(key) = event::read().context("reading input")? {
                let line = text.lines.last_mut().unwrap();
                let input = match &mut line.spans[2].content {
                    Cow::Owned(input) => input,
                    _ => panic!("expected owned input"),
                };

                match key.code {
                    KeyCode::Esc => return Ok(()),
                    KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                        return Ok(());
                    }
                    KeyCode::Char(c) => input.push(c),
                    KeyCode::Backspace if !input.is_empty() => {
                        input.pop();
                    }
                    KeyCode::Enter => {
                        line.spans.pop();
                        break &line.spans.last().unwrap().content;
                    }
                    _ => continue,
                }

                draw_repl!();
            }
        };

        if input.trim().is_empty() {
            continue;
        }

        process_expr(&input.clone(), &mut Some(&mut text.lines));
    }
}

fn process_expr(input: &str, buffer: &mut Option<&mut Vec<Line>>) {
    let mut expr = match parse::expr(input) {
        Ok(expr) => expr,
        Err(err) => {
            report_user_error(err, buffer);
            return;
        }
    };

    match expr.reduce() {
        Ok(()) => (),
        Err(err) => {
            report_user_error(err, buffer);
            return;
        }
    };

    println(style!(#Blue(expr.to_string())), buffer);
}

fn report_fatal_error(err: anyhow::Error, buffer: &mut Option<&mut Vec<Line>>) {
    println(style!(#Red("An unexpected error occurred:")), buffer);
    for (i, cause) in (1..).zip(err.chain()) {
        println(style!(#Red(format!("    {i}: {cause}"))), buffer);
    }
}

fn report_user_error(err: anyhow::Error, buffer: &mut Option<&mut Vec<Line>>) {
    println(style!(#Red("Invalid input:")), buffer);
    for (i, cause) in (1..).zip(err.chain()) {
        println(style!(#Red(format!("    {i}: {cause}"))), buffer);
    }
}

fn println<'a>(line: Line<'a>, buffer: &mut Option<&mut Vec<Line<'a>>>) {
    if let Some(buffer) = buffer {
        buffer.push(line);
    } else {
        let mut stdout = io::stdout();

        let mut fg = Color::Reset;
        for span in line.spans {
            let new_fg = span.style.fg.unwrap_or(Color::Reset);
            if new_fg != fg {
                let color = match new_fg {
                    Color::Reset => CColor::Reset,
                    Color::Black => CColor::Black,
                    Color::Red => CColor::DarkRed,
                    Color::Green => CColor::DarkGreen,
                    Color::Yellow => CColor::DarkYellow,
                    Color::Blue => CColor::DarkBlue,
                    Color::Magenta => CColor::DarkMagenta,
                    Color::Cyan => CColor::DarkCyan,
                    Color::Gray => CColor::Grey,
                    Color::DarkGray => CColor::DarkGrey,
                    Color::LightRed => CColor::Red,
                    Color::LightGreen => CColor::Green,
                    Color::LightBlue => CColor::Blue,
                    Color::LightYellow => CColor::Yellow,
                    Color::LightMagenta => CColor::Magenta,
                    Color::LightCyan => CColor::Cyan,
                    Color::White => CColor::White,
                    Color::Indexed(i) => CColor::AnsiValue(i),
                    Color::Rgb(r, g, b) => CColor::Rgb { r, g, b },
                };
                queue!(stdout, SetForegroundColor(color)).unwrap();
                fg = new_fg;
            }

            queue!(stdout, Print(span.content)).unwrap();
        }

        if fg != Color::Reset {
            queue!(stdout, SetForegroundColor(CColor::Reset)).unwrap();
        }

        queue!(stdout, Print('\n')).unwrap();

        stdout.flush().unwrap();
    }
}
