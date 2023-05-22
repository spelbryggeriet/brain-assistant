use std::fmt::{self, Display, Formatter};

use anyhow::ensure;
use lazy_regex::{regex, Captures};
use once_cell::sync::Lazy;
use uuid::Uuid;

use crate::{
    expr::{Expr, ExprBinOp, ExprUnOp, Literal, Number, Variable},
    parser,
};

type RuleFn = dyn Fn(Expr) -> Result<anyhow::Result<Expr>, Expr> + Send + Sync;
type RuleConst = (Variable, Option<(Variable, Box<[Variable]>)>);

pub static RULES: Lazy<Box<[Box<RuleFn>]>> = Lazy::new(parse_rules);

fn parse_rules() -> Box<[Box<RuleFn>]> {
    let src: String = include_str!("../data/rules.txt")
        .chars()
        .filter(|c| *c == '\n' || !c.is_whitespace())
        .collect();

    let lines = src.lines().filter(|l| !l.trim().is_empty());
    let mut rules = Vec::with_capacity(lines.clone().count());
    for line in lines {
        let (tmpl_src, rule_src) = line.split_once('=').unwrap();

        let re = regex!("#([a-z])");
        let tmpl_consts: Box<[(Variable, Option<Number>)]> = re
            .captures_iter(tmpl_src)
            .map(|caps| (caps.get(1).unwrap().as_str().to_owned(), None))
            .collect();

        let expanded_tmpl_src = re.replace_all(tmpl_src, |caps: &Captures| {
            caps.get(1).unwrap().as_str().to_owned()
        });

        let re = regex!("(.?)([a-z])");
        let tmpl_vars: Box<[(Variable, Option<Expr>)]> = re
            .captures_iter(tmpl_src)
            .filter(|caps| !matches!(caps.get(1).map(|m| m.as_str()), Some("#")))
            .map(|caps| (caps.get(2).unwrap().as_str().to_owned(), None))
            .collect();

        let tmpl = parser::parse(&expanded_tmpl_src).unwrap();

        let re = regex!(r"#([a-z]+)(?:\((?: *([a-z])(?: *, *([a-z]) *)*)?\))?");
        let mut rule_consts = Vec::<RuleConst>::new();
        let mut expanded_rule_src = String::with_capacity(rule_src.len());
        let mut prev_end = 0;
        for caps in re.captures_iter(rule_src) {
            let m = caps.get(0).unwrap();
            let expr_name = caps.get(1).unwrap().as_str();

            expanded_rule_src += &rule_src[prev_end..m.range().start];
            if caps.iter().flatten().count() == 2 {
                expanded_rule_src += expr_name;

                rule_consts.push((expr_name.to_owned(), None));
            } else {
                let mut key = format!("{expr_name}{}", UuidFormatter(Uuid::new_v4()));
                while rule_consts.iter().any(|(k, ..)| k == &key) {
                    key = format!("{expr_name}{}", UuidFormatter(Uuid::new_v4()));
                }

                expanded_rule_src += &key;

                rule_consts.push((
                    key,
                    Some((
                        expr_name.to_owned(),
                        caps.iter()
                            .skip(2)
                            .flatten()
                            .map(|m| m.as_str().to_owned())
                            .collect(),
                    )),
                ));
            }

            prev_end = m.range().end;
        }
        expanded_rule_src += &rule_src[prev_end..];
        let rule_consts = rule_consts.into_boxed_slice();

        let rule_expr = parser::parse(&expanded_rule_src).unwrap();

        let rule: Box<RuleFn> = Box::new(move |expr| {
            let mut tmpl_consts = tmpl_consts.clone();
            let mut tmpl_vars = tmpl_vars.clone();
            if match_template(&expr, &tmpl, &mut tmpl_consts, &mut tmpl_vars) {
                Ok(apply_rule(
                    &rule_expr,
                    &rule_consts,
                    &tmpl_consts,
                    &tmpl_vars,
                ))
            } else {
                Err(expr)
            }
        });
        rules.push(rule);
    }

    rules.into_boxed_slice()
}

fn match_template(
    expr: &Expr,
    tmpl_expr: &Expr,
    tmpl_consts: &mut [(Variable, Option<Number>)],
    tmpl_vars: &mut [(Variable, Option<Expr>)],
) -> bool {
    match (expr, tmpl_expr) {
        (Expr::Literal(Literal::Undefined), Expr::Literal(Literal::Variable(tmpl_var))) => {
            tmpl_var == "undefined"
        }
        (expr, Expr::Literal(Literal::Variable(tmpl_var))) => {
            if let Some((_, c)) = tmpl_consts.iter_mut().find(|(k, _)| k == tmpl_var) {
                if let Expr::Literal(Literal::Number(num)) = expr {
                    if let Some(c) = c.as_ref() {
                        c == num
                    } else {
                        *c = Some(num.clone());
                        true
                    }
                } else {
                    false
                }
            } else if let Some((_, e)) = tmpl_vars.iter_mut().find(|(k, _)| k == tmpl_var) {
                if let Some(e) = e.as_ref() {
                    e == expr
                } else {
                    *e = Some(expr.clone());
                    true
                }
            } else {
                expr == tmpl_expr
            }
        }
        (Expr::UnOp(expr), Expr::UnOp(tmpl_expr)) if expr.un_op == tmpl_expr.un_op => {
            match_template(&expr.operand, &tmpl_expr.operand, tmpl_consts, tmpl_vars)
        }
        (Expr::BinOp(expr), Expr::BinOp(tmpl_expr)) if expr.bin_op == tmpl_expr.bin_op => {
            match_template(&expr.lhs, &tmpl_expr.lhs, tmpl_consts, tmpl_vars)
                && match_template(&expr.rhs, &tmpl_expr.rhs, tmpl_consts, tmpl_vars)
        }
        _ => expr == tmpl_expr,
    }
}

fn apply_rule(
    rule_expr: &Expr,
    rule_consts: &[RuleConst],
    tmpl_consts: &[(Variable, Option<Number>)],
    tmpl_vars: &[(Variable, Option<Expr>)],
) -> anyhow::Result<Expr> {
    let expr = match rule_expr {
        Expr::Literal(Literal::Variable(rule_var)) if rule_var == "undefined" => {
            Expr::Literal(Literal::Undefined)
        }
        Expr::Literal(Literal::Variable(rule_var)) => {
            if let Some((_, const_expr)) = rule_consts.iter().find(|(k, ..)| k == rule_var) {
                if let Some((const_expr, args)) = const_expr {
                    match (&**const_expr, &**args) {
                        ("sum", [a, b]) => Expr::Literal(Literal::Number(
                            get_const(a, tmpl_consts) + get_const(b, tmpl_consts),
                        )),
                        ("difference", [a, b]) => Expr::Literal(Literal::Number(
                            get_const(a, tmpl_consts) - get_const(b, tmpl_consts),
                        )),
                        ("product", [a, b]) => Expr::Literal(Literal::Number(
                            get_const(a, tmpl_consts) * get_const(b, tmpl_consts),
                        )),
                        ("pow", [a, b]) => {
                            let b = get_const(b, tmpl_consts);
                            ensure!(b.bits() <= 32, "exponent too large: {}", b);
                            let b = b.iter_u32_digits().next().unwrap_or(0);

                            Expr::Literal(Literal::Number(get_const(a, tmpl_consts).pow(b)))
                        }
                        (const_expr, args) => panic!(
                            "unknown constant expression '{const_expr}' with {} argument{}",
                            args.len(),
                            if args.len() != 1 { "s" } else { "" }
                        ),
                    }
                } else {
                    Expr::Literal(Literal::Number(get_const(rule_var, tmpl_consts).clone()))
                }
            } else {
                let (_, e) = tmpl_vars.iter().find(|(k, _)| k == rule_var).unwrap();
                e.clone().unwrap()
            }
        }
        Expr::Literal(lit) => Expr::Literal(lit.clone()),
        Expr::UnOp(expr) => Expr::UnOp(ExprUnOp {
            un_op: expr.un_op,
            operand: Box::new(apply_rule(
                &expr.operand,
                rule_consts,
                tmpl_consts,
                tmpl_vars,
            )?),
        }),
        Expr::BinOp(expr) => Expr::BinOp(ExprBinOp {
            bin_op: expr.bin_op,
            lhs: Box::new(apply_rule(&expr.lhs, rule_consts, tmpl_consts, tmpl_vars)?),
            rhs: Box::new(apply_rule(&expr.rhs, rule_consts, tmpl_consts, tmpl_vars)?),
        }),
    };

    Ok(expr)
}

fn get_const<'a>(var: &str, consts: &'a [(Variable, Option<Number>)]) -> &'a Number {
    consts
        .iter()
        .find(|(k, _)| k == var)
        .unwrap()
        .1
        .as_ref()
        .unwrap()
}

struct UuidFormatter(Uuid);

impl Display for UuidFormatter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for byte in self.0.as_bytes() {
            for nibble in [byte >> 4, *byte] {
                write!(f, "{}", (b'a' + (nibble & 0x0f)) as char)?;
            }
        }

        Ok(())
    }
}