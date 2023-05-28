use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
};

use anyhow::ensure;
use lazy_regex::{regex, Captures};
use num_traits::One;
use once_cell::sync::Lazy;
use uuid::Uuid;

use crate::{
    expr::{CmpExpr, CmpOp, Expr, ExprBinOp, ExprUnOp, Literal, Number, Variable},
    parse,
};

type RuleFn = dyn Fn(&mut Expr) -> anyhow::Result<bool> + Send + Sync;

pub static RULES: Lazy<Box<[Box<RuleFn>]>> = Lazy::new(parse_rules);

#[derive(Clone)]
struct FixedMap<V>(Box<[(String, V)]>);

impl<V> FixedMap<V> {
    fn get(&self, var: &str) -> Option<&V> {
        self.0.iter().find(|(k, _)| k == var).map(|(_, v)| v)
    }

    fn get_mut(&mut self, var: &str) -> Option<&mut V> {
        self.0.iter_mut().find(|(k, _)| k == var).map(|(_, v)| v)
    }
}

impl<V> From<Vec<(String, V)>> for FixedMap<V> {
    fn from(value: Vec<(String, V)>) -> Self {
        Self(value.into_boxed_slice())
    }
}

impl<V> FromIterator<(String, V)> for FixedMap<V> {
    fn from_iter<T: IntoIterator<Item = (String, V)>>(iter: T) -> Self {
        Self(Box::from_iter(iter))
    }
}

#[derive(Clone)]
struct TmplData {
    consts: FixedMap<Option<Number>>,
    vars: FixedMap<Option<Variable>>,
    sub_exprs: FixedMap<Option<Expr>>,
}

impl TmplData {
    fn new(
        consts: FixedMap<Option<Number>>,
        vars: FixedMap<Option<Variable>>,
        sub_exprs: FixedMap<Option<Expr>>,
    ) -> Self {
        Self {
            consts,
            vars,
            sub_exprs,
        }
    }

    fn get_const<'a>(&'a self, var: &str) -> &'a Number {
        self.consts
            .get(var)
            .unwrap_or_else(|| panic!("{var} not defined"))
            .as_ref()
            .unwrap_or_else(|| panic!("{var} not set"))
    }

    fn replace_consts(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Literal(Literal::Variable(var)) if var == "undefined" => {
                Expr::Literal(Literal::Undefined)
            }
            Expr::Literal(Literal::Variable(var)) => {
                Expr::Literal(Literal::Number(self.get_const(var).clone()))
            }
            Expr::UnOp(expr) => Expr::UnOp(ExprUnOp {
                un_op: expr.un_op,
                operand: Box::new(self.replace_consts(&expr.operand)),
            }),
            Expr::BinOp(expr) => Expr::BinOp(ExprBinOp {
                bin_op: expr.bin_op,
                lhs: Box::new(self.replace_consts(&expr.lhs)),
                rhs: Box::new(self.replace_consts(&expr.rhs)),
            }),
            _ => expr.clone(),
        }
    }

    fn match_template(
        &mut self,
        expr: &Expr,
        tmpl_expr: &Expr,
        tmpl_cond: Option<&CmpExpr>,
    ) -> anyhow::Result<bool> {
        let did_match = match (expr, tmpl_expr) {
            (Expr::Literal(Literal::Undefined), Expr::Literal(Literal::Variable(tmpl_ident))) => {
                tmpl_ident == "undefined"
            }
            (expr, Expr::Literal(Literal::Variable(tmpl_ident))) => {
                if let Some(tmpl_num) = self.consts.get_mut(tmpl_ident) {
                    if let Expr::Literal(Literal::Number(num)) = expr {
                        if let Some(tmpl_num) = tmpl_num.as_ref() {
                            tmpl_num == num
                        } else {
                            *tmpl_num = Some(num.clone());
                            true
                        }
                    } else {
                        false
                    }
                } else if let Some(tmpl_var) = self.vars.get_mut(tmpl_ident) {
                    if let Expr::Literal(Literal::Variable(var)) = expr {
                        if let Some(tmpl_var) = tmpl_var.as_ref() {
                            tmpl_var == var
                        } else {
                            *tmpl_var = Some(var.clone());
                            true
                        }
                    } else {
                        false
                    }
                } else if let Some(tmpl_sub_expr) = self.sub_exprs.get_mut(tmpl_ident) {
                    if let Some(tmpl_sub_expr) = tmpl_sub_expr.as_ref() {
                        tmpl_sub_expr == expr
                    } else {
                        *tmpl_sub_expr = Some(expr.clone());
                        true
                    }
                } else {
                    expr == tmpl_expr
                }
            }
            (Expr::UnOp(expr), Expr::UnOp(tmpl_expr)) if expr.un_op == tmpl_expr.un_op => {
                self.match_template(&expr.operand, &tmpl_expr.operand, None)?
            }
            (Expr::BinOp(expr), Expr::BinOp(tmpl_expr)) if expr.bin_op == tmpl_expr.bin_op => {
                self.match_template(&expr.lhs, &tmpl_expr.lhs, None)?
                    && self.match_template(&expr.rhs, &tmpl_expr.rhs, None)?
            }
            _ => expr == tmpl_expr,
        };

        let cond_is_true = if did_match {
            if let Some(cmp_expr) = tmpl_cond {
                let mut lhs = self.replace_consts(&cmp_expr.lhs);
                let mut rhs = self.replace_consts(&cmp_expr.rhs);
                lhs.reduce()?;
                rhs.reduce()?;

                let (Expr::Literal(Literal::Number(lhs)), Expr::Literal(Literal::Number(rhs))) = (&lhs, &rhs) else {
                panic!("irreducible comparison exprssion: {lhs} {} {rhs}", cmp_expr.cmp_op);
            };

                match cmp_expr.cmp_op {
                    CmpOp::Lt => lhs < rhs,
                    CmpOp::Lte => lhs <= rhs,
                    CmpOp::Eq => lhs == rhs,
                    CmpOp::Neq => lhs != rhs,
                    CmpOp::Gt => lhs > rhs,
                    CmpOp::Gte => lhs >= rhs,
                }
            } else {
                true
            }
        } else {
            true
        };

        Ok(did_match && cond_is_true)
    }

    fn apply_rule(
        &self,
        rule_expr: &Expr,
        rule_macros: &FixedMap<(Variable, Box<[Variable]>)>,
    ) -> anyhow::Result<Expr> {
        let expr = match rule_expr {
            Expr::Literal(Literal::Variable(rule_var)) if rule_var == "undefined" => {
                Expr::Literal(Literal::Undefined)
            }
            Expr::Literal(Literal::Variable(rule_var)) => {
                if let Some((const_expr, args)) = rule_macros.get(rule_var) {
                    match (&**const_expr, &**args) {
                        ("sum", [a, b]) => {
                            Expr::Literal(Literal::Number(self.get_const(a) + self.get_const(b)))
                        }
                        ("difference", [a, b]) => {
                            Expr::Literal(Literal::Number(self.get_const(a) - self.get_const(b)))
                        }
                        ("product", [a, b]) => {
                            Expr::Literal(Literal::Number(self.get_const(a) * self.get_const(b)))
                        }
                        ("pow", [a, b]) => {
                            let b = self.get_const(b);
                            ensure!(b.bits() <= 32, "exponent too large: {}", b);
                            let b = b.iter_u32_digits().next().unwrap_or(0);

                            Expr::Literal(Literal::Number(self.get_const(a).pow(b)))
                        }
                        ("negate", [a]) => Expr::Literal(Literal::Number(-self.get_const(a))),
                        ("factorial", [a]) => {
                            let a = self.get_const(a);

                            let two = Number::from(2);
                            let n = match a.cmp(&two) {
                                Ordering::Less => Number::one(),
                                Ordering::Equal => a.clone(),
                                Ordering::Greater => {
                                    let mut a = a.clone();
                                    let mut n = Number::one();
                                    while a > two {
                                        n *= &a;
                                        a -= Number::one();
                                    }
                                    n * a
                                }
                            };

                            Expr::Literal(Literal::Number(n))
                        }
                        (const_expr, args) => panic!(
                            "unknown constant expression '{const_expr}' with {} argument{}",
                            args.len(),
                            if args.len() != 1 { "s" } else { "" }
                        ),
                    }
                } else if let Some(tmpl_const) = self.consts.get(rule_var) {
                    Expr::Literal(Literal::Number(tmpl_const.clone().unwrap()))
                } else if let Some(tmpl_var) = self.vars.get(rule_var) {
                    Expr::Literal(Literal::Variable(tmpl_var.clone().unwrap()))
                } else {
                    self.sub_exprs
                        .get(rule_var)
                        .unwrap_or_else(|| panic!("{rule_var} not defined"))
                        .clone()
                        .unwrap()
                }
            }
            Expr::Literal(lit) => Expr::Literal(lit.clone()),
            Expr::UnOp(expr) => Expr::UnOp(ExprUnOp {
                un_op: expr.un_op,
                operand: Box::new(self.apply_rule(&expr.operand, rule_macros)?),
            }),
            Expr::BinOp(expr) => Expr::BinOp(ExprBinOp {
                bin_op: expr.bin_op,
                lhs: Box::new(self.apply_rule(&expr.lhs, rule_macros)?),
                rhs: Box::new(self.apply_rule(&expr.rhs, rule_macros)?),
            }),
        };

        Ok(expr)
    }
}

fn parse_rules() -> Box<[Box<RuleFn>]> {
    let src = include_str!("../data/rules.txt");

    let lines = src.lines().filter(|l| !l.trim().is_empty());
    let mut rules = Vec::with_capacity(lines.clone().count());
    for line in lines {
        let (tmpl_src, rule_src) = line.rsplit_once('=').unwrap();

        let re = regex!("#([a-z])");
        let tmpl_consts: FixedMap<Option<Number>> = re
            .captures_iter(tmpl_src)
            .map(|caps| (caps.get(1).unwrap().as_str().to_owned(), None))
            .collect();

        let re = regex!("@([a-z])");
        let tmpl_sub_exprs: FixedMap<Option<Expr>> = re
            .captures_iter(tmpl_src)
            .map(|caps| (caps.get(1).unwrap().as_str().to_owned(), None))
            .collect();

        let re = regex!("([#@a-z]?)([a-z])([a-z]?)");
        let tmpl_vars: FixedMap<Option<Variable>> = re
            .captures_iter(tmpl_src)
            .filter(|caps| caps.get(1).unwrap().is_empty() && caps.get(3).unwrap().is_empty())
            .map(|caps| (caps.get(2).unwrap().as_str().to_owned(), None))
            .collect();

        let re = regex!("(?:#|@)([a-z])");
        let expanded_tmpl_src = re.replace_all(tmpl_src, |caps: &Captures| {
            caps.get(1).unwrap().as_str().to_owned()
        });

        let tmpl_expr = parse::template_expr(&expanded_tmpl_src).unwrap();

        let re = regex!(r"@([a-z])|#([a-z]+)(\((?: *([a-z])(?: *, *([a-z]) *)*)?\))?");
        let mut rule_macros = Vec::<(String, (Variable, Box<[Variable]>))>::new();
        let mut expanded_rule_src = String::with_capacity(rule_src.len());
        let mut prev_end = 0;
        for caps in re.captures_iter(rule_src) {
            let m = caps.get(0).unwrap();
            let expr_name = caps.get(1).or_else(|| caps.get(2)).unwrap().as_str();

            expanded_rule_src += &rule_src[prev_end..m.range().start];
            if caps.iter().flatten().count() == 2 {
                expanded_rule_src += expr_name;
            } else {
                let mut key = format!("{expr_name}{}", UuidFormatter(Uuid::new_v4()));
                while rule_macros.iter().any(|(k, ..)| k == &key) {
                    key = format!("{expr_name}{}", UuidFormatter(Uuid::new_v4()));
                }

                expanded_rule_src += &key;

                rule_macros.push((
                    key,
                    (
                        expr_name.to_owned(),
                        caps.iter()
                            .flatten()
                            .skip(3)
                            .map(|m| m.as_str().to_owned())
                            .collect(),
                    ),
                ));
            }

            prev_end = m.range().end;
        }
        expanded_rule_src += &rule_src[prev_end..];
        let rule_macros = FixedMap::from(rule_macros);

        let rule_expr = parse::expr(&expanded_rule_src).unwrap();

        let rule: Box<RuleFn> = Box::new(move |expr| {
            let mut tmpl_data = TmplData::new(
                tmpl_consts.clone(),
                tmpl_vars.clone(),
                tmpl_sub_exprs.clone(),
            );

            if tmpl_data.match_template(expr, &tmpl_expr.expr, tmpl_expr.cond.as_ref())? {
                *expr = tmpl_data.apply_rule(&rule_expr, &rule_macros)?;
                Ok(true)
            } else {
                Ok(false)
            }
        });
        rules.push(rule);
    }

    rules.into_boxed_slice()
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

#[cfg(test)]
mod tests {
    use crate::parse;

    macro_rules! cases {
        ($($name:ident, $e:expr => $e_reduced:expr),* $(,)?) => {
            $(
            #[test]
            fn $name() {
                let mut expr = parse::expr(stringify!($e)).unwrap();
                expr.reduce().unwrap();
                let expr_reduced = parse::expr(stringify!($e_reduced)).unwrap();
                assert_eq!(expr, expr_reduced);
            }
            )*
        };
    }

    cases! {
        one_plus_one, 1 + 1     => 2,
        x2_plus_x3,   x^2 + x^3 => x^3 + x^2,
    }
}
