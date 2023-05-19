use std::{fmt::Display, str::FromStr};

use anyhow::{anyhow, Context};
use colored::Colorize;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, map_res, opt},
    error::{ErrorKind, FromExternalError, ParseError},
    sequence::pair,
    Err, IResult,
};
use num_bigint::{BigInt, ParseBigIntError};

use crate::number::Value;

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

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Value),
    UnOp(ExprUnOp),
    BinOp(ExprBinOp),
}

impl Expr {
    pub fn evaluate(self) -> anyhow::Result<Value> {
        match self {
            Self::Literal(number) => Ok(number),
            Self::UnOp(expr) => {
                let operand = expr.operand.evaluate()?;
                match expr.un_op {
                    UnOp::Factorial => Ok(operand.factorial()),
                }
            }
            Self::BinOp(expr) => {
                let lhs = expr.lhs.evaluate()?;
                let rhs = expr.rhs.evaluate()?;
                match expr.bin_op {
                    BinOp::Add => Ok(lhs + rhs),
                    BinOp::Sub => Ok(lhs - rhs),
                    BinOp::Mul => Ok(lhs * rhs),
                    BinOp::Div => Ok(lhs / rhs),
                    BinOp::Pow => lhs.pow(rhs),
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprUnOp {
    un_op: UnOp,
    operand: Box<Expr>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum UnOp {
    Factorial,
}

#[derive(Clone, Debug)]
pub struct ExprBinOp {
    bin_op: BinOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl BinOp {
    fn precedence(self) -> Precedence {
        match self {
            Self::Add | Self::Sub => Precedence::Term,
            Self::Mul | Self::Div | Self::Pow => Precedence::Factor,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Any,
    Term,
    Factor,
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
            || next_precedence == bin_op.precedence() && bin_op == BinOp::Pow
        {
            (s, rhs) = expr_helper(s, rhs, next_precedence)?;
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
    let (s, literal) = map(parse_number, Expr::Literal)(s)?;
    if let (s, Some(un_op)) = opt(map(tag("!"), |_| UnOp::Factorial))(s)? {
        Ok((
            s,
            Expr::UnOp(ExprUnOp {
                un_op,
                operand: Box::new(literal),
            }),
        ))
    } else {
        Ok((s, literal))
    }
}

fn parse_number(s: &str) -> IResult<&str, Value, ExprError> {
    map(map_res(digit1, BigInt::from_str), Value::from)(s)
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
