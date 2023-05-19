use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Mul, Sub},
    str::FromStr,
};

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
use num_traits::{One, Zero};

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
    Literal(Rational),
    BinOp(ExprBinOp),
}

impl Expr {
    pub fn evaluate(self) -> Rational {
        match self {
            Self::Literal(rational) => rational,
            Self::BinOp(expr) => {
                let lhs = expr.lhs.evaluate();
                let rhs = expr.rhs.evaluate();
                match expr.bin_op {
                    BinOp::Add => lhs + rhs,
                    BinOp::Sub => lhs - rhs,
                    BinOp::Mul => lhs * rhs,
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Rational {
    numerator: BigInt,
    denominator: BigInt,
}

impl Add for Rational {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.denominator == rhs.denominator {
            Self {
                numerator: self.numerator + rhs.numerator,
                ..self
            }
        } else {
            Self {
                numerator: self.numerator * rhs.denominator.clone()
                    + rhs.numerator * self.denominator.clone(),
                denominator: self.denominator * rhs.denominator,
            }
        }
    }
}

impl Sub for Rational {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if self.denominator == rhs.denominator {
            Self {
                numerator: self.numerator - rhs.numerator,
                ..self
            }
        } else {
            Self {
                numerator: self.numerator * rhs.denominator.clone()
                    - rhs.numerator * self.denominator.clone(),
                denominator: self.denominator * rhs.denominator,
            }
        }
    }
}

impl Mul for Rational {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            numerator: self.numerator * rhs.numerator,
            denominator: self.denominator * rhs.denominator,
        }
    }
}

impl Display for Rational {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.denominator == BigInt::zero() {
            return write!(f, "∞");
        }

        write!(f, "{}", self.numerator)?;
        if self.denominator != BigInt::one() {
            write!(f, "/{}", self.denominator)?;
        }

        Ok(())
    }
}

impl From<BigInt> for Rational {
    fn from(value: BigInt) -> Self {
        Rational {
            numerator: value,
            denominator: BigInt::one(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprBinOp {
    bin_op: BinOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

#[derive(Clone, Copy, Debug)]
enum BinOp {
    Add,
    Sub,
    Mul,
}

impl BinOp {
    fn precedence(self) -> Precedence {
        match self {
            Self::Add | Self::Sub => Precedence::Term,
            Self::Mul => Precedence::Factor,
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
    let mut bin_op_unary_pair = opt(pair(parse_bin_op, parse_unary));

    while let (s_ahead, Some((bin_op, mut rhs))) = bin_op_unary_pair(s)? {
        let precedence = bin_op.precedence();
        if precedence < base_precedence {
            return Ok((s, lhs));
        }

        (s, rhs) = expr_helper(s_ahead, rhs, precedence)?;

        lhs = Expr::BinOp(ExprBinOp {
            bin_op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });
    }

    Ok((s, lhs))
}

fn parse_unary(s: &str) -> IResult<&str, Expr, ExprError> {
    map(parse_integer, Expr::Literal)(s)
}

fn parse_integer(s: &str) -> IResult<&str, Rational, ExprError> {
    map(map_res(digit1, BigInt::from_str), Rational::from)(s)
}

fn parse_bin_op(s: &str) -> IResult<&str, BinOp, ExprError> {
    alt((
        map(tag("+"), |_| BinOp::Add),
        map(tag("-"), |_| BinOp::Sub),
        map(tag("*"), |_| BinOp::Mul),
    ))(s)
}
