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
    expr::{CmpExpr, CmpOp, Expr, ExprBinOp, ExprUnOp, Literal, Number, TmplExpr, Variable},
    parse,
};

type RuleFn = dyn Fn(&mut Expr) -> anyhow::Result<bool> + Send + Sync;

pub static RULES: Lazy<Box<[Box<RuleFn>]>> = Lazy::new(parse_rules);

#[derive(Clone, Debug)]
struct TmplPrimitive<T> {
    data: Option<T>,
    optional: Option<(Option<Expr>, Option<Expr>)>,
}

impl<T> TmplPrimitive<T> {
    fn new(optional: Option<(Option<Expr>, Option<Expr>)>) -> Self {
        Self {
            data: None,
            optional,
        }
    }

    fn assign(&mut self, data: &T) -> bool
    where
        T: Clone + Eq,
    {
        if let Some(self_data) = &self.data {
            self_data == data
        } else {
            self.data = Some(data.clone());
            true
        }
    }
}

#[derive(Clone, Debug)]
struct FixedMap<V>(Box<[(String, V)]>);

impl<V> FixedMap<V> {
    fn iter_mut(&mut self) -> impl Iterator<Item = (&str, &mut V)> {
        self.0.iter_mut().map(|(k, v)| (&**k, v))
    }

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

#[derive(Clone, Debug)]
struct TmplData {
    consts: FixedMap<TmplPrimitive<Number>>,
    sub_exprs: FixedMap<TmplPrimitive<Expr>>,
    vars: FixedMap<TmplPrimitive<Variable>>,
}

impl TmplData {
    fn new(
        consts: FixedMap<TmplPrimitive<Number>>,
        sub_exprs: FixedMap<TmplPrimitive<Expr>>,
        vars: FixedMap<TmplPrimitive<Variable>>,
    ) -> Self {
        Self {
            consts,
            sub_exprs,
            vars,
        }
    }

    fn generate_permutations(
        self,
        tmpl_expr: TmplExpr,
        rule_expr: Expr,
    ) -> Vec<(Self, TmplExpr, Expr)> {
        let mut permutations = Vec::new();
        self.generate_permutations_helper(tmpl_expr, rule_expr, &mut permutations);
        permutations
    }

    fn generate_permutations_helper(
        mut self,
        mut tmpl_expr: TmplExpr,
        mut rule_expr: Expr,
        permutations: &mut Vec<(Self, TmplExpr, Expr)>,
    ) {
        macro_rules! find_optionals {
            ($map:expr) => {
                $map.iter_mut().find_map(|(k, c)| {
                    if let Some(optional) = c.optional.take() {
                        Some((k.to_owned(), optional))
                    } else {
                        None
                    }
                })
            };
        }

        let optionals = find_optionals!(self.consts)
            .or_else(|| find_optionals!(self.sub_exprs))
            .or_else(|| find_optionals!(self.vars));

        if let Some((ident, (tmpl_optional, rule_optional))) = optionals {
            self.clone().generate_permutations_helper(
                tmpl_expr.clone(),
                rule_expr.clone(),
                permutations,
            );

            tmpl_expr.expr =
                replace_identifier(&ident, tmpl_optional.as_ref(), tmpl_expr.expr).unwrap();
            tmpl_expr.cond = tmpl_expr.cond.map(|mut cond| {
                cond.lhs = replace_identifier(&ident, tmpl_optional.as_ref(), cond.lhs).unwrap();
                cond.rhs = replace_identifier(&ident, tmpl_optional.as_ref(), cond.rhs).unwrap();
                cond
            });
            rule_expr = replace_identifier(&ident, rule_optional.as_ref(), rule_expr).unwrap();

            self.generate_permutations_helper(tmpl_expr, rule_expr, permutations);
        } else {
            permutations.push((self, tmpl_expr, rule_expr));
        }
    }

    fn get_const<'a>(&'a self, var: &str) -> &'a Number {
        self.consts
            .get(var)
            .unwrap_or_else(|| panic!("{var} not defined"))
            .data
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
            (expr, Expr::Literal(Literal::Variable(tmpl_ident))) => {
                if tmpl_ident == "undefined" {
                    matches!(expr, Expr::Literal(Literal::Undefined))
                } else if let Some(tmpl_num) = self.consts.get_mut(tmpl_ident) {
                    if let Expr::Literal(Literal::Number(num)) = expr {
                        tmpl_num.assign(num)
                    } else {
                        false
                    }
                } else if let Some(tmpl_var) = self.vars.get_mut(tmpl_ident) {
                    if let Expr::Literal(Literal::Variable(var)) = expr {
                        tmpl_var.assign(var)
                    } else {
                        false
                    }
                } else if let Some(tmpl_sub_expr) = self.sub_exprs.get_mut(tmpl_ident) {
                    tmpl_sub_expr.assign(expr)
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
                panic!("irreducible comparison expression: {lhs} {} {rhs}", cmp_expr.cmp_op);
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
                    Expr::Literal(Literal::Number(tmpl_const.data.clone().unwrap()))
                } else if let Some(tmpl_var) = self.vars.get(rule_var) {
                    Expr::Literal(Literal::Variable(tmpl_var.data.clone().unwrap()))
                } else {
                    self.sub_exprs
                        .get(rule_var)
                        .unwrap_or_else(|| panic!("{rule_var} not defined"))
                        .data
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

fn replace_identifier(ident: &str, optional: Option<&Expr>, expr: Expr) -> Option<Expr> {
    match expr {
        Expr::Literal(Literal::Variable(var)) if var == ident => optional.cloned(),
        Expr::UnOp(expr_un_op) => Some(Expr::UnOp(ExprUnOp {
            operand: Box::new(replace_identifier(ident, optional, *expr_un_op.operand)?),
            ..expr_un_op
        })),
        Expr::BinOp(expr_bin_op) => match (
            replace_identifier(ident, optional, *expr_bin_op.lhs),
            replace_identifier(ident, optional, *expr_bin_op.rhs),
        ) {
            (Some(lhs), Some(rhs)) => Some(Expr::BinOp(ExprBinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                ..expr_bin_op
            })),
            (Some(expr), None) | (None, Some(expr)) => Some(expr),
            (None, None) => None,
        },
        expr => Some(expr),
    }
}

fn parse_rules() -> Box<[Box<RuleFn>]> {
    let src = include_str!("../data/rules.txt");

    let lines = src.lines().filter(|l| !l.trim().is_empty());
    let mut rules = Vec::with_capacity(lines.clone().count());
    for line in lines {
        let (tmpl_src, rule_src) = line.rsplit_once('=').unwrap();

        let mut tmpl_consts = Vec::new();
        let mut tmpl_sub_exprs = Vec::new();
        let mut tmpl_vars = Vec::new();

        let re = regex!(r#"(#|@|[a-z]?)([a-z])(\?(?:\{[^~]*~[^}]*\})?|[a-z]?)"#);
        for caps in re.captures_iter(tmpl_src) {
            let ident = caps.get(2).unwrap().as_str();

            let optionals = match caps.get(3).unwrap().as_str() {
                s if s.starts_with('?') => {
                    if let Some(s) = s.strip_prefix("?{").and_then(|s| s.strip_suffix('}')) {
                        let (lhs, rhs) = s.split_once('~').unwrap();
                        let lhs = if lhs.is_empty() {
                            None
                        } else {
                            Some(parse::expr(lhs).unwrap())
                        };
                        let rhs = if rhs.is_empty() {
                            None
                        } else {
                            Some(parse::expr(rhs).unwrap())
                        };
                        Some((lhs, rhs))
                    } else {
                        Some((None, None))
                    }
                }
                "" => None,
                _ => continue,
            };

            match caps.get(1).unwrap().as_str() {
                "#" => tmpl_consts.push((ident.to_owned(), TmplPrimitive::new(optionals))),
                "@" => tmpl_sub_exprs.push((ident.to_owned(), TmplPrimitive::new(optionals))),
                "" => tmpl_vars.push((ident.to_owned(), TmplPrimitive::new(optionals))),
                _ => continue,
            }
        }

        let tmpl_consts = FixedMap::from(tmpl_consts);
        let tmpl_sub_exprs = FixedMap::from(tmpl_sub_exprs);
        let tmpl_vars = FixedMap::from(tmpl_vars);

        let re = regex!(r#"(?:#|@)([a-z])(?:\?(?:\{[^}]+\})?)?"#);
        let expanded_tmpl_src = re.replace_all(tmpl_src, |caps: &Captures| {
            caps.get(1).unwrap().as_str().to_owned()
        });

        let re = regex!(r"#([a-z]+) *\((?: *([a-z])(?: *, *([a-z]) *)*)?\)");
        let mut rule_macros = Vec::<(String, _)>::new();
        let mut expanded_rule_src = String::with_capacity(rule_src.len());
        let mut prev_end = 0;
        for caps in re.captures_iter(rule_src) {
            let m = caps.get(0).unwrap();
            let expr_name = caps.get(1).unwrap().as_str();

            let mut key = format!("{expr_name}{}", UuidFormatter(Uuid::new_v4()));
            while rule_macros.iter().any(|(k, _)| k == &key) {
                key = format!("{expr_name}{}", UuidFormatter(Uuid::new_v4()));
            }

            expanded_rule_src += &rule_src[prev_end..m.range().start];
            expanded_rule_src += &key;
            prev_end = m.range().end;

            rule_macros.push((
                key,
                (
                    expr_name.to_owned(),
                    caps.iter()
                        .flatten()
                        .skip(2)
                        .map(|m| m.as_str().to_owned())
                        .collect(),
                ),
            ));
        }

        let rule_macros = FixedMap::from(rule_macros);

        expanded_rule_src += &rule_src[prev_end..];

        let tmpl_expr = parse::template_expr(&expanded_tmpl_src).unwrap();
        let rule_expr = parse::expr(&expanded_rule_src).unwrap();
        let tmpl_data = TmplData::new(tmpl_consts, tmpl_sub_exprs, tmpl_vars);

        for (tmpl_data, tmpl_expr, rule_expr) in
            tmpl_data.generate_permutations(tmpl_expr, rule_expr)
        {
            let rule_macros = rule_macros.clone();
            let rule: Box<RuleFn> = Box::new(move |expr| {
                let mut tmpl_data = tmpl_data.clone();
                if tmpl_data.match_template(expr, &tmpl_expr.expr, tmpl_expr.cond.as_ref())? {
                    *expr = tmpl_data.apply_rule(&rule_expr, &rule_macros)?;
                    Ok(true)
                } else {
                    Ok(false)
                }
            });
            rules.push(rule);
        }
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
        case_1_plus_1,     1 + 1         => 2,
        case_x2_plus_y3,   x^2 + y^3     => y^3 + x^2,
        case_2x2_plus_y3,  2*x^2 + y^3   => y^3 + 2*x^2,
        case_x2_plus_3y3,  x^2 + 3*y^3   => 3*y^3 + x^2,
        case_2x2_plus_3y3, 2*x^2 + 3*y^3 => 3*y^3 + 2*x^2,
    }
}
