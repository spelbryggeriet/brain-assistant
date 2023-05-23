use nom::{bytes::complete::tag, combinator::map, sequence::pair, IResult};

use crate::expr::TmplExpr;

use super::{peek, spaced};

pub(super) fn parse_tmpl_expr(s: &str) -> IResult<&str, TmplExpr> {
    let (s, expr) = super::expr::parse_expr(s)?;
    let (s, cmp_expr) = if peek(spaced(tag("if")))(s)? {
        map(
            pair(spaced(tag("if")), super::cmp::parse_cmp_expr),
            |(_, cmp_expr)| Some(cmp_expr),
        )(s)?
    } else {
        (s, None)
    };

    Ok((
        s,
        TmplExpr {
            expr,
            cond: cmp_expr,
        },
    ))
}
