use std::{fmt::Display, str::FromStr};

use anyhow::{anyhow, Context};
use colored::Colorize;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::{map, map_res, opt},
    error::{ErrorKind, FromExternalError, ParseError},
    sequence::{delimited, pair},
    Err, IResult, Parser,
};
use num_bigint::{BigInt, ParseBigIntError};

use crate::expr::{BinOp, Expr, ExprBinOp, ExprUnOp, Literal, Precedence, UnOp};

pub struct ExprError(anyhow::Error);

impl<I: Display> ParseError<I> for ExprError {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        let inner = match kind {
            ErrorKind::Digit => anyhow!("not a valid integer: {}", input.to_string().white()),
            _ => anyhow!("{}", kind.description()),
        };
        Self(inner)
    }

    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        Self(other.0.context(Self::from_error_kind(input, kind).0))
    }
}

impl<I> FromExternalError<I, ParseBigIntError> for ExprError {
    fn from_external_error(_: I, _: ErrorKind, e: ParseBigIntError) -> Self {
        Self(e.into())
    }
}

pub fn parse(s: &str) -> anyhow::Result<Expr> {
    parse_expr(s)
        .map_err(|err| match err {
            Err::Incomplete(_) => anyhow!("incomplete input"),
            Err::Error(err) => err.0,
            Err::Failure(err) => err.0,
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

fn parse_expr(s: &str) -> IResult<&str, Expr, ExprError> {
    let (s, unary) = parse_unary(s)?;
    expr_helper(s, unary, Precedence::Any)
}

fn expr_helper(
    mut s: &str,
    mut lhs: Expr,
    base_precedence: Precedence,
) -> IResult<&str, Expr, ExprError> {
    let mut parse_bin_op_unary_pair = opt(pair(parse_bin_op, parse_unary));

    while let (s_ahead, Some((bin_op, mut rhs))) = parse_bin_op_unary_pair(s)? {
        if bin_op.precedence() < base_precedence {
            break;
        }

        s = s_ahead;

        let next_precedence = opt(parse_bin_op)(s)?
            .1
            .map_or(Precedence::Any, |bin_op| bin_op.precedence());
        if next_precedence > bin_op.precedence()
            || next_precedence == bin_op.precedence() && bin_op.is_right_associative()
        {
            (s, rhs) = expr_helper(s, rhs, bin_op.precedence())?;
        }

        lhs = Expr::BinOp(ExprBinOp {
            bin_op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });
    }

    Ok((s, lhs))
}

fn parse_unary(s: &str) -> IResult<&str, Expr, ExprError> {
    let (s, prefix_op) = opt(parse_prefix_un_op)(s)?;

    let (s, mut expr) = if peek(tag("("))(s)? {
        delimited(tag("("), parse_expr, tag(")"))(s)?
    } else {
        map(parse_literal, Expr::Literal)(s)?
    };

    let (s, postfix_op) = opt(parse_postfix_un_op)(s)?;

    if let Some(un_op) = postfix_op {
        expr = Expr::UnOp(ExprUnOp {
            un_op,
            operand: Box::new(expr),
        });
    }

    if let Some(un_op) = prefix_op {
        expr = Expr::UnOp(ExprUnOp {
            un_op,
            operand: Box::new(expr),
        });
    }

    Ok((s, expr))
}

fn parse_literal(s: &str) -> IResult<&str, Literal, ExprError> {
    alt((
        map(map_res(digit1, BigInt::from_str), Literal::from),
        map(alpha1, Literal::from),
    ))(s)
}

fn parse_prefix_un_op(s: &str) -> IResult<&str, UnOp, ExprError> {
    map(tag("-"), |_| UnOp::Negate)(s)
}

fn parse_postfix_un_op(s: &str) -> IResult<&str, UnOp, ExprError> {
    map(tag("!"), |_| UnOp::Factorial)(s)
}

fn parse_bin_op(s: &str) -> IResult<&str, BinOp, ExprError> {
    alt((
        map(tag("+"), |_| BinOp::Add),
        map(tag("-"), |_| BinOp::Sub),
        map(tag("*"), |_| BinOp::Mul),
        map(tag("/"), |_| BinOp::Div),
        map(tag("^"), |_| BinOp::Pow),
    ))(s)
}

fn peek<'a, P, T>(p: P) -> impl FnOnce(&'a str) -> Result<bool, Err<ExprError>>
where
    P: Parser<&'a str, T, ExprError>,
{
    |s| Ok(opt(p)(s)?.1.is_some())
}
