use nom::{branch::alt, bytes::complete::tag, combinator::map, sequence::tuple, IResult};

use crate::expr::{CmpExpr, CmpOp};

use super::spaced;

pub(super) fn parse_cmp_expr(s: &str) -> IResult<&str, CmpExpr> {
    map(
        tuple((
            super::expr::parse_expr,
            parse_cmp_op,
            super::expr::parse_expr,
        )),
        |(lhs, cmp_op, rhs)| CmpExpr { lhs, cmp_op, rhs },
    )(s)
}

fn parse_cmp_op(s: &str) -> IResult<&str, CmpOp> {
    spaced(alt((
        map(tag("<"), |_| CmpOp::Lt),
        map(tag("<="), |_| CmpOp::Lte),
        map(tag("=="), |_| CmpOp::Eq),
        map(tag("!="), |_| CmpOp::Neq),
        map(tag(">"), |_| CmpOp::Gt),
        map(tag(">="), |_| CmpOp::Gte),
    )))(s)
}
