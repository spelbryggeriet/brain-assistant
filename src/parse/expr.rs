use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1},
    combinator::{map, map_parser, map_res, not, opt},
    sequence::{delimited, pair},
    IResult,
};
use num_bigint::BigInt;

use crate::expr::{BinOp, Expr, ExprBinOp, ExprUnOp, Literal, Precedence, UnOp};

use super::{peek, spaced};

pub(super) fn parse_expr(s: &str) -> IResult<&str, Expr> {
    let (s, unary) = parse_unary(s)?;
    expr_helper(s, unary, Precedence::Any)
}

fn expr_helper(mut s: &str, mut lhs: Expr, base_precedence: Precedence) -> IResult<&str, Expr> {
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

fn parse_unary(s: &str) -> IResult<&str, Expr> {
    let (s, prefix_op) = opt(parse_prefix_un_op)(s)?;

    let (s, mut expr) = if peek(spaced(tag("(")))(s)? {
        delimited(spaced(tag("(")), parse_expr, spaced(tag(")")))(s)?
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

fn parse_literal(s: &str) -> IResult<&str, Literal> {
    spaced(alt((
        map(map_res(digit1, BigInt::from_str), Literal::from),
        map(
            map_parser(alpha1, |s| {
                not(tag("if"))(s)?;
                Ok((s, s))
            }),
            Literal::from,
        ),
    )))(s)
}

fn parse_prefix_un_op(s: &str) -> IResult<&str, UnOp> {
    spaced(map(tag("-"), |_| UnOp::Negate))(s)
}

fn parse_postfix_un_op(s: &str) -> IResult<&str, UnOp> {
    spaced(map(tag("!"), |_| UnOp::Factorial))(s)
}

fn parse_bin_op(s: &str) -> IResult<&str, BinOp> {
    spaced(alt((
        map(tag("+"), |_| BinOp::Add),
        map(tag("-"), |_| BinOp::Sub),
        map(tag("*"), |_| BinOp::Mul),
        map(tag("/"), |_| BinOp::Div),
        map(tag("^"), |_| BinOp::Pow),
    )))(s)
}
