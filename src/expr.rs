use std::fmt::{self, Display, Formatter};

use crate::number::Value;

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Value),
    UnOp(ExprUnOp),
    BinOp(ExprBinOp),
}

impl Expr {
    pub fn reduce(self) -> Self {
        match self {
            Self::UnOp(expr) => {
                let operand = expr.operand.reduce();
                match expr.un_op {
                    _ => Expr::UnOp(ExprUnOp {
                        operand: Box::new(operand),
                        ..expr
                    }),
                }
            }
            Self::BinOp(expr) => {
                let lhs = expr.lhs.reduce();
                let rhs = expr.rhs.reduce();
                match expr.bin_op {
                    _ => Expr::BinOp(ExprBinOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        ..expr
                    }),
                }
            }
            expr => expr,
        }
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
                    write!(f, "{prefix}{}{}{suffix}", expr.un_op, expr.operand)
                } else {
                    write!(f, "{prefix}{}{}{suffix}", expr.operand, expr.un_op)
                }
            }
            Self::BinOp(expr) => {
                let mut lhs_prefix = "";
                let mut lhs_suffix = "";
                let mut rhs_prefix = "";
                let mut rhs_suffix = "";

                if matches!(&*expr.lhs, Expr::BinOp(sub_expr) if sub_expr.bin_op.precedence() < expr.bin_op.precedence())
                {
                    lhs_prefix = "(";
                    lhs_suffix = ")";
                }

                if matches!(&*expr.rhs, Expr::BinOp(sub_expr) if sub_expr.bin_op.precedence() < expr.bin_op.precedence())
                {
                    rhs_prefix = "(";
                    rhs_suffix = ")";
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
            Self::Mul | Self::Div | Self::Pow => Precedence::Factor,
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
    Prefix,
    Postfix,
}
