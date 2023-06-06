mod expr;
mod parse;
mod reduce;

use std::{
    borrow::{Borrow, Cow},
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
use expr::Expr;
use once_cell::sync::Lazy;
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Style},
    text::{Line, Span, Text},
    widgets::Paragraph,
    Terminal,
};

#[derive(Default)]
struct AppData {
    history: Vec<(String, Option<anyhow::Result<Expr>>)>,
    input: Option<String>,
    info: Option<ExprInfo>,
}

impl AppData {
    fn begin_input(&mut self) {
        self.input = Some(String::new());
    }

    fn finish_input(&mut self) {
        self.history.push((self.input.take().unwrap(), None));
    }

    fn last_submitted_input(&self) -> Option<&str> {
        self.history.last().map(|(i, _)| i.as_str())
    }

    fn finish_result(&mut self, result: anyhow::Result<Expr>) {
        let last_result = &mut self.history.last_mut().unwrap().1;
        assert!(
            last_result.replace(result).is_none(),
            "expected empty result"
        );
    }
}

struct ExprInfo {
    steps: Vec<Expr>,
}

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
        let mut expr = match parse::expr(&expression) {
            Ok(expr) => expr,
            Err(err) => {
                print_text(user_error_text(err));
                return;
            }
        };

        match expr.reduce() {
            Ok(()) => (),
            Err(err) => {
                print_text(user_error_text(err));
                return;
            }
        };

        print_text(expr_text(expr))
    } else {
        let mut terminal = match setup_terminal() {
            Ok(terminal) => terminal,
            Err(err) => {
                print_text(fatal_error_text(err));
                return;
            }
        };

        match repl(&mut terminal) {
            Ok(()) => (),
            Err(err) => print_text(fatal_error_text(err)),
        }

        match restore_terminal(terminal) {
            Ok(()) => (),
            Err(err) => print_text(fatal_error_text(err)),
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

fn repl(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> anyhow::Result<()> {
    let mut app_data = AppData::default();

    loop {
        app_data.begin_input();

        draw_repl(terminal, &mut app_data)?;

        loop {
            match event::read().context("reading input")? {
                Event::Key(key) => {
                    let input = app_data.input.as_mut().unwrap();

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
                            break;
                        }
                        _ => continue,
                    }

                    draw_repl(terminal, &mut app_data)?;
                }
                Event::Resize(_, _) => draw_repl(terminal, &mut app_data)?,
                _ => (),
            }
        }

        app_data.finish_input();

        let last_input = app_data.last_submitted_input().unwrap();
        if last_input.trim().is_empty() {
            continue;
        }

        let expr = parse::expr(last_input).and_then(|mut expr| {
            expr.reduce()?;
            Ok(expr)
        });

        app_data.finish_result(expr);
    }
}

fn draw_repl(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    app_data: &mut AppData,
) -> anyhow::Result<()> {
    terminal
        .draw(|f| {
            const INPUT_PREFIX: &str = ">>";

            let chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Min(50), Constraint::Ratio(3, 4)])
                .split(f.size());

            let info_pane = chunks[0];
            let repl_pane = chunks[1];

            let mut repl_text = Text {
                lines: vec![Line::default(); repl_pane.height as usize],
            };

            let mut last_index = repl_pane.height as usize;

            'build_repl_text: {
                let yellow = Style::default().fg(Color::Yellow);
                if last_index == 0 {
                    break 'build_repl_text;
                }

                if let Some(input) = app_data.input.as_deref() {
                    let input_line = Line::from(vec![
                        Span::styled(INPUT_PREFIX, yellow),
                        Span::raw(format!(" {input}▏")),
                    ]);

                    let input_text = if input_line.width() > repl_pane.width as usize {
                        split_line(input_line, repl_pane.width as usize)
                    } else {
                        Text::from(input_line)
                    };

                    last_index = append_text_from_back(input_text, &mut repl_text, last_index);

                    if last_index == 0 {
                        break 'build_repl_text;
                    }
                }

                for (input, output) in app_data.history.iter().rev() {
                    let output_text = match output {
                        Some(Ok(expr)) => expr_text(expr),
                        Some(Err(err)) => user_error_text(err),
                        None => Text::default(),
                    };

                    for line in output_text.lines.into_iter().rev() {
                        let line_text = if line.width() > repl_pane.width as usize {
                            split_line(line, repl_pane.width as usize)
                        } else {
                            Text::from(line)
                        };

                        last_index = append_text_from_back(line_text, &mut repl_text, last_index);

                        if last_index == 0 {
                            break 'build_repl_text;
                        }
                    }

                    let input_line = Line::from(vec![
                        Span::styled(INPUT_PREFIX, yellow),
                        Span::raw(format!(" {input}")),
                    ]);
                    let input_text = if input_line.width() > repl_pane.width as usize {
                        split_line(input_line, repl_pane.width as usize)
                    } else {
                        Text::from(input_line)
                    };

                    last_index = append_text_from_back(input_text, &mut repl_text, last_index);

                    if last_index == 0 {
                        break 'build_repl_text;
                    }
                }

                let diff =
                    (repl_pane.height as usize).saturating_sub(repl_text.height() - last_index);
                if diff > 0 {
                    repl_text.lines.rotate_left(diff);
                }
            }

            f.render_widget(Paragraph::new(repl_text), repl_pane);
        })
        .context("drawing repl")
        .map(|_| ())
}

fn split_line(input_line: Line, width: usize) -> Text {
    if width == 0 {
        return Text::from(input_line);
    }

    let mut text = Text::from(Line::default());
    for mut span in input_line.spans.into_iter() {
        let line = text.lines.last_mut().unwrap();
        let mut span_width = span.width();
        let mut left_on_line = width - line.width();

        if span_width <= left_on_line {
            line.spans.push(span);
            continue;
        }

        if left_on_line == 0 {
            text.lines.push(Line::default());
            left_on_line = width;
        }

        while span_width > left_on_line {
            let line = text.lines.last_mut().unwrap();

            let span_split = Span {
                content: Cow::Owned(span.content[..left_on_line].to_owned()),
                ..span
            };
            span.content = Cow::Owned(span.content[left_on_line..].to_owned());
            span_width = span.width();

            line.spans.push(span_split);
            text.lines.push(Line::default());
            left_on_line = width;
        }

        let input_line = text.lines.last_mut().unwrap();
        input_line.spans.push(span);
    }

    text
}

fn append_text_from_back<'a>(
    mut text: Text<'a>,
    buffer: &mut Text<'a>,
    mut last_index: usize,
) -> usize {
    let size = last_index - last_index.saturating_sub(text.height());
    last_index -= size;
    let mut lines = text.lines.drain(text.height() - size..);
    buffer.lines[last_index..last_index + size].fill_with(|| lines.next().unwrap());
    last_index
}

fn expr_text(expr: impl Borrow<Expr>) -> Text<'static> {
    Text::styled(expr.borrow().to_string(), Style::default().fg(Color::Blue))
}

fn fatal_error_text(err: impl Borrow<anyhow::Error>) -> Text<'static> {
    let red = Style::default().fg(Color::Red);
    let mut text = Text::styled("An unexpected error occurred:", red);
    text.extend(error_text(err));
    text
}

fn user_error_text(err: impl Borrow<anyhow::Error>) -> Text<'static> {
    let red = Style::default().fg(Color::Red);
    let mut text = Text::styled("Invalid input:", red);
    text.extend(error_text(err));
    text
}

fn error_text(err: impl Borrow<anyhow::Error>) -> Text<'static> {
    let red = Style::default().fg(Color::Red);
    let mut text = Text::default();
    for (i, cause) in (1..).zip(err.borrow().chain()) {
        text.extend(Text::styled(format!("    {i}: {cause}"), red));
    }
    text
}

fn print_text<'a>(text: impl Borrow<Text<'a>>) {
    for line in &text.borrow().lines {
        let mut stdout = io::stdout();

        let mut fg = Color::Reset;
        for span in &line.spans {
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

            queue!(stdout, Print(&span.content)).unwrap();
        }

        if fg != Color::Reset {
            queue!(stdout, SetForegroundColor(CColor::Reset)).unwrap();
        }

        #[cfg(not(windows))]
        queue!(stdout, Print('\n')).unwrap();
        #[cfg(windows)]
        queue!(stdout, Print("\r\n")).unwrap();

        stdout.flush().unwrap();
    }
}
