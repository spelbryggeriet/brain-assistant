mod expr;
mod parse;
mod reduce;
mod text_composer;

use std::{
    borrow::Borrow,
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
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span, Text},
    widgets::{Block, BorderType, Borders, Paragraph},
    Terminal,
};

use crate::text_composer::TextComposer;

#[derive(Default)]
struct AppData {
    history: Vec<(String, Option<anyhow::Result<Expr>>)>,
    input: Option<String>,
    info: ExprInfo,
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

    fn finish_result(&mut self, result: anyhow::Result<(Expr, Vec<Expr>)>) {
        let result = match result {
            Ok((expr, steps)) => {
                self.info.steps.replace(steps);
                Ok(expr)
            }
            Err(err) => {
                self.info.steps.take();
                Err(err)
            }
        };

        let last_result = &mut self.history.last_mut().unwrap().1;
        assert!(
            last_result.replace(result).is_none(),
            "expected empty result"
        );
    }

    fn finish_no_result(&mut self) {
        self.info.steps.take();
    }
}

#[derive(Default)]
struct ExprInfo {
    steps: Option<Vec<Expr>>,
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
            app_data.finish_no_result();
            continue;
        }

        let result = parse::expr(last_input).and_then(|mut expr| {
            let steps = expr.reduce_with_steps()?;
            Ok((expr, steps))
        });

        app_data.finish_result(result);
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
                .constraints([Constraint::Max(50), Constraint::Ratio(3, 4)])
                .split(f.size());

            let mut info_pane = chunks[0];
            let repl_pane = chunks[1];

            let mut repl_text_composer = TextComposer::new(repl_pane);

            'build_repl_text: {
                if repl_text_composer.available_lines() == 0 {
                    break 'build_repl_text;
                }

                let yellow = Style::default().fg(Color::Yellow);

                if let Some(input) = app_data.input.as_deref() {
                    let has_lines_left =
                        repl_text_composer.extend_text_from_back(Text::from(Line::from(vec![
                            Span::styled(INPUT_PREFIX, yellow),
                            Span::raw(format!(" {input}▏")),
                        ])));

                    if !has_lines_left {
                        break 'build_repl_text;
                    }
                }

                for (input, output) in app_data.history.iter().rev() {
                    let output_text = match output {
                        Some(Ok(expr)) => expr_text(expr),
                        Some(Err(err)) => user_error_text(err),
                        None => Text::default(),
                    };

                    if !repl_text_composer.extend_text_from_back(output_text) {
                        break 'build_repl_text;
                    }

                    let has_lines_left =
                        repl_text_composer.extend_text_from_back(Text::from(Line::from(vec![
                            Span::styled(INPUT_PREFIX, yellow),
                            Span::raw(format!(" {input}")),
                        ])));

                    if !has_lines_left {
                        break 'build_repl_text;
                    }
                }
            }

            let repl_text = repl_text_composer.finish();
            f.render_widget(
                Paragraph::new(repl_text).block(
                    Block::default()
                        .borders(Borders::LEFT)
                        .border_type(BorderType::Thick),
                ),
                repl_pane,
            );

            if let Some(steps) = &app_data.info.steps {
                let mut steps_text_composer = TextComposer::new(Rect {
                    height: info_pane.height.saturating_sub(1),
                    ..info_pane
                });

                for (i, step) in (1..).zip(steps) {
                    let has_lines_left =
                        steps_text_composer.extend_text(Text::from(Line::from(vec![
                            Span::styled(
                                format!("({i}) "),
                                Style::default().fg(Color::Rgb(75, 75, 75)),
                            ),
                            Span::styled(step.to_string(), Style::default().fg(Color::Blue)),
                        ])));

                    if !has_lines_left {
                        break;
                    }
                }

                let steps_text = steps_text_composer.finish();
                let height = (steps_text.height() + 1).min(info_pane.height as usize) as u16;

                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([Constraint::Length(height), Constraint::Min(0)])
                    .split(info_pane);

                f.render_widget(
                    Paragraph::new(steps_text).block(
                        Block::default()
                            .title("Steps")
                            .title_alignment(Alignment::Center)
                            .borders(Borders::TOP),
                    ),
                    chunks[0],
                );

                #[allow(unused)]
                {
                    info_pane = chunks[1];
                }
            }
        })
        .context("drawing repl")
        .map(|_| ())
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
