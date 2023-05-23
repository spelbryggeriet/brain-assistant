mod cmp;
mod expr;
mod template;

use std::fmt::{Debug, Display};

use anyhow::{anyhow, Context};
use colored::Colorize;
use nom::{
    character::complete::space0, combinator::opt, error::ParseError, sequence::delimited, AsChar,
    Err, IResult, InputTakeAtPosition, Parser,
};

use crate::expr::{Expr, TmplExpr};

pub fn template_expr(s: &str) -> anyhow::Result<TmplExpr> {
    map_result(s, template::parse_tmpl_expr(s))
}

pub fn expr(s: &str) -> anyhow::Result<Expr> {
    map_result(s, expr::parse_expr(s))
}

fn map_result<T: Display + Debug>(s: &str, res: IResult<&str, T>) -> anyhow::Result<T> {
    res.map_err(|err| match err {
        Err::Incomplete(_) => anyhow!("incomplete input"),
        Err::Error(err) | Err::Failure(err) => anyhow!("{err}"),
    })
    .and_then(|(remaining, e)| {
        if remaining.is_empty() {
            Ok(e)
        } else {
            Err(anyhow!(
                "unrecognized trailing characters: {}",
                remaining.white(),
            ))
        }
    })
    .with_context(|| format!("parsing expression: {}", s.white()))
}

pub fn spaced<I, O, E: ParseError<I>, F>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E>,
{
    delimited(space0, f, space0)
}

fn peek<'a, P, T>(p: P) -> impl FnOnce(&'a str) -> Result<bool, Err<nom::error::Error<&'a str>>>
where
    P: Parser<&'a str, T, nom::error::Error<&'a str>>,
{
    |s| Ok(opt(p)(s)?.1.is_some())
}
