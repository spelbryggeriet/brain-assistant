mod expr;
mod parse;
mod reduce;

use std::io::{stdin, stdout, Write};

use anyhow::Context;
use clap::{command, Arg, ArgAction};
use colored::Colorize;
use once_cell::sync::Lazy;

fn main() {
    #[cfg(windows)]
    colored::control::set_virtual_terminal(true).unwrap();

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
        process_expr(&expression);
    } else {
        match repl() {
            Ok(()) => (),
            Err(err) => report_fatal_error(err),
        }
    }
}

fn repl() -> anyhow::Result<()> {
    let mut input = String::new();
    loop {
        print!("{} ", ">>".yellow());
        stdout().flush().context("flushing standard output")?;

        input.clear();
        stdin()
            .read_line(&mut input)
            .context("reading standard input")?;

        if input.trim().is_empty() {
            continue;
        }

        if !input.ends_with('\n') {
            println!();
        }

        let input: String = input.chars().filter(|c| !c.is_whitespace()).collect();

        process_expr(&input);
    }
}

fn process_expr(input: &str) {
    let mut expr = match parse::expr(input) {
        Ok(expr) => expr,
        Err(err) => {
            report_user_error(err);
            return;
        }
    };

    match expr.reduce() {
        Ok(()) => (),
        Err(err) => {
            report_user_error(err);
            return;
        }
    };

    println!("{}", expr.to_string().blue());
}

fn report_fatal_error(err: anyhow::Error) {
    println!("{}", "\nAn unexpected error occurred:".red());
    for (i, cause) in (1..).zip(err.chain()) {
        println!("{}", format!("    {i}: {cause}").red());
    }
}

fn report_user_error(err: anyhow::Error) {
    println!("{}", "Invalid input:".red());
    for (i, cause) in (1..).zip(err.chain()) {
        println!("{}", format!("    {i}: {cause}").red());
    }
}
