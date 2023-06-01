mod expr;
mod parse;
mod reduce;

use std::io::{stdin, stdout, Write};

use anyhow::Context;
use colored::Colorize;
use once_cell::sync::Lazy;

fn main() {
    Lazy::force(&reduce::RULES);

    match run() {
        Ok(()) => (),
        Err(err) => report_fatal_error(err),
    }
}

fn run() -> anyhow::Result<()> {
    let mut input = String::new();
    loop {
        print!("{} ", ">>".yellow());
        stdout().flush().context("flushing standard output")?;

        input.clear();
        stdin()
            .read_line(&mut input)
            .context("reading standard input")?;
        if !input.ends_with('\n') {
            println!();
        }
        let input: String = input.chars().filter(|c| !c.is_whitespace()).collect();

        let mut expr = match parse::expr(&input) {
            Ok(expr) => expr,
            Err(err) => {
                report_user_error(err);
                continue;
            }
        };

        match expr.reduce() {
            Ok(()) => (),
            Err(err) => {
                report_user_error(err);
                continue;
            }
        };

        println!("{}", expr.to_string().blue());
    }
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
