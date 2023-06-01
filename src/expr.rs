use std::fmt::{self, Display, Formatter};

use num_bigint::BigInt;

use crate::reduce;

#[derive(Clone, Debug)]
pub struct TmplExpr {
    pub expr: Expr,
    pub cond: Option<CmpExpr>,
}

impl Display for TmplExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)?;
        if let Some(cond) = &self.cond {
            write!(f, " if {cond}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct CmpExpr {
    pub cmp_op: CmpOp,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl Display for CmpExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.cmp_op, self.rhs)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CmpOp {
    Lt,
    Lte,
    Eq,
    Neq,
    Gt,
    Gte,
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Lt => write!(f, "<"),
            Self::Lte => write!(f, "<="),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Gt => write!(f, ">"),
            Self::Gte => write!(f, ">="),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    Literal(Literal),
    UnOp(ExprUnOp),
    BinOp(ExprBinOp),
}

impl Expr {
    pub fn reduce(&mut self) -> anyhow::Result<()> {
        self.reduce_helper(None, false)?;
        Ok(())
    }

    #[allow(unused)]
    pub fn reduce_with_steps(&mut self) -> anyhow::Result<Vec<Self>> {
        let mut reductions = vec![self.clone()];
        self.reduce_helper(Some(&mut reductions), false)?;
        Ok(reductions)
    }

    fn reduce_helper(
        &mut self,
        mut reductions: Option<&mut Vec<Self>>,
        reduce_once: bool,
    ) -> anyhow::Result<bool> {
        macro_rules! iterate_inner {
            ($expr:ident, $op_type:ident, $op_field:ident) => {
                if $expr.$op_field.reduce_helper(None, true)? {
                    if reduce_once {
                        return Ok(true);
                    }

                    loop {
                        if let Some(r) = &mut reductions {
                            r.push(self.clone());
                        }

                        let Expr::$op_type(expr) = self else {
                            panic!(concat!("expected Expr::", stringify!($op_type)));
                        };

                        if !expr.$op_field.reduce_helper(None, true)? {
                            break;
                        }
                    }
                }
            };
        }

        'outer: loop {
            match self {
                Expr::UnOp(expr) => {
                    iterate_inner!(expr, UnOp, operand);
                }
                Expr::BinOp(expr) => {
                    iterate_inner!(expr, BinOp, lhs);

                    let Expr::BinOp(expr) = self else {
                        panic!("expected Expr::BinOp");
                    };

                    iterate_inner!(expr, BinOp, rhs);
                }
                _ => (),
            }

            for rule in reduce::RULES.iter() {
                if rule(self)? {
                    if let Some(r) = &mut reductions {
                        r.push(self.clone());
                    }

                    if reduce_once {
                        return Ok(true);
                    }

                    continue 'outer;
                }
            }

            break 'outer;
        }

        Ok(false)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Literal(value) => value.fmt(f),
            Self::UnOp(expr) => {
                let mut prefix = "";
                let mut suffix = "";

                if matches!(&*expr.operand, Expr::UnOp(sub_expr) if sub_expr.un_op.precedence() < expr.un_op.precedence())
                    || matches!(&*expr.operand, Expr::BinOp(_))
                {
                    prefix = "(";
                    suffix = ")";
                }

                if expr.un_op.precedence() == Precedence::Prefix {
                    write!(f, "{}{prefix}{}{suffix}", expr.un_op, expr.operand)
                } else {
                    write!(f, "{prefix}{}{suffix}{}", expr.operand, expr.un_op)
                }
            }
            Self::BinOp(expr) => {
                let mut lhs_prefix = "";
                let mut lhs_suffix = "";
                let mut rhs_prefix = "";
                let mut rhs_suffix = "";

                match &*expr.lhs {
                    Expr::BinOp(sub_expr)
                        if sub_expr.bin_op.precedence() < expr.bin_op.precedence() =>
                    {
                        lhs_prefix = "(";
                        lhs_suffix = ")";
                    }
                    _ => (),
                }

                match &*expr.rhs {
                    Expr::BinOp(sub_expr)
                        if sub_expr.bin_op.precedence() < expr.bin_op.precedence()
                            || sub_expr.bin_op.precedence() == expr.bin_op.precedence() =>
                    {
                        rhs_prefix = "(";
                        rhs_suffix = ")";
                    }
                    _ => (),
                }

                write!(
                    f,
                    "{lhs_prefix}{lhs}{lhs_suffix}{op}{rhs_prefix}{rhs}{rhs_suffix}",
                    lhs = expr.lhs,
                    op = expr.bin_op,
                    rhs = expr.rhs,
                )
            }
        }
    }
}

pub type Variable = String;
pub type Number = BigInt;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Literal {
    Variable(Variable),
    Number(Number),
    Undefined,
}

impl From<&str> for Literal {
    fn from(value: &str) -> Self {
        Self::Variable(value.to_owned())
    }
}

impl From<BigInt> for Literal {
    fn from(value: BigInt) -> Self {
        Self::Number(value)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Variable(v) => v.fmt(f),
            Self::Number(r) => r.fmt(f),
            Self::Undefined => write!(f, "undefined"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExprUnOp {
    pub un_op: UnOp,
    pub operand: Box<Expr>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnOp {
    Negate,
    Factorial,
}

impl UnOp {
    fn precedence(self) -> Precedence {
        match self {
            Self::Negate => Precedence::Prefix,
            Self::Factorial => Precedence::Postfix,
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Negate => write!(f, "-"),
            Self::Factorial => write!(f, "!"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExprBinOp {
    pub bin_op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl BinOp {
    pub fn precedence(self) -> Precedence {
        match self {
            Self::Add | Self::Sub => Precedence::Term,
            Self::Mul | Self::Div => Precedence::Factor,
            Self::Pow => Precedence::Exponent,
        }
    }

    pub fn is_right_associative(self) -> bool {
        match self {
            Self::Pow => true,
            Self::Add | Self::Mul | Self::Sub | Self::Div => false,
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Pow => write!(f, "^"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Any,
    Term,
    Factor,
    Exponent,
    Prefix,
    Postfix,
}

#[cfg(test)]
mod tests {
    use crate::parse;

    macro_rules! cases {
        ($($name:ident, $e:literal),* $(,)?) => {
            $(
            #[test]
            fn $name() {
                let expr = parse::expr($e).unwrap();
                assert_eq!($e, expr.to_string());
            }
            )*
        };
    }

    cases! {
        display_identity_neg_fact_a, "-a!",
        display_identity_fact_neg_a, "(-a)!",
        display_identity_a_over_b_fact, "(a/b)!",
    }
}
