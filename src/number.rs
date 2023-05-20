use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Neg, Sub},
};

use anyhow::ensure;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, Signed, Zero};

#[derive(Clone, Debug)]
pub enum Value {
    Ratio(Ratio),
    Undefined,
}

impl Value {
    pub fn pow(self, rhs: Self) -> anyhow::Result<Self> {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => lhs.pow(rhs),
            _ => Ok(Self::Undefined),
        }
    }

    pub fn factorial(self) -> Self {
        match self {
            Self::Ratio(operand) => operand.factorial(),
            _ => Self::Undefined,
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Ratio(operand) => Self::Ratio(-operand),
            _ => Self::Undefined,
        }
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => Self::Ratio(lhs + rhs),
            _ => Self::Undefined,
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => Self::Ratio(lhs - rhs),
            _ => Self::Undefined,
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => Self::Ratio(lhs * rhs),
            _ => Self::Undefined,
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => lhs / rhs,
            _ => Self::Undefined,
        }
    }
}

impl From<BigInt> for Value {
    fn from(value: BigInt) -> Self {
        Self::Ratio(Ratio::from(value))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Ratio(r) => r.fmt(f),
            Self::Undefined => write!(f, "undefined"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Ratio {
    numerator: BigInt,
    denominator: BigInt,
}

impl Ratio {
    fn is_negative(&self) -> bool {
        (self.numerator.sign() == Sign::Minus) != (self.denominator.sign() == Sign::Minus)
    }

    fn pow(self, rhs: Self) -> anyhow::Result<Value> {
        let (mut numerator, mut denominator) = if rhs.numerator == BigInt::zero() {
            (BigInt::one(), BigInt::one())
        } else if rhs.numerator.magnitude() == &BigUint::one() {
            (self.numerator, self.denominator)
        } else {
            ensure!(
                rhs.numerator.bits() <= 32,
                "exponent too large: {}",
                rhs.numerator,
            );

            let exponent = rhs.numerator.iter_u32_digits().next().unwrap();
            (self.numerator.pow(exponent), self.denominator.pow(exponent))
        };

        if rhs.is_negative() {
            (numerator, denominator) = (denominator, numerator);
        }

        if rhs.denominator.magnitude() > &BigUint::one() {
            ensure!(
                rhs.denominator.bits() <= 32,
                "root degree too large: {}",
                rhs.denominator,
            );

            let exponent = rhs.denominator.iter_u32_digits().next().unwrap();
            (numerator, denominator) =
                (numerator.nth_root(exponent), denominator.nth_root(exponent));
        }

        Ok(Value::Ratio(Ratio {
            numerator,
            denominator,
        }))
    }

    fn factorial(mut self) -> Value {
        if self.is_negative() {
            return Value::Undefined;
        }

        if self.numerator.is_negative() {
            self.numerator = self.numerator.neg();
        }

        if self.denominator != BigInt::one() {
            // TODO: implement gamma function.
            return Value::Undefined;
        }

        if self.numerator == BigInt::zero() {
            return Value::from(BigInt::one());
        }

        let two = BigInt::from(2_u32);
        if self.numerator <= two {
            return Value::from(self.numerator);
        }

        let mut res = BigInt::one();
        while self.numerator > two {
            res *= self.numerator.clone();
            self.numerator -= BigInt::one();
        }
        res *= self.numerator;

        Value::from(res)
    }
}

impl Neg for Ratio {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            numerator: -self.numerator,
            denominator: self.denominator,
        }
    }
}

impl Add for Ratio {
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

impl Sub for Ratio {
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

impl Mul for Ratio {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            numerator: self.numerator * rhs.numerator,
            denominator: self.denominator * rhs.denominator,
        }
    }
}

impl Div for Ratio {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.numerator == BigInt::zero() {
            Value::Undefined
        } else {
            Value::Ratio(Self {
                numerator: self.numerator * rhs.denominator,
                denominator: self.denominator * rhs.numerator,
            })
        }
    }
}

impl From<BigInt> for Ratio {
    fn from(value: BigInt) -> Self {
        Self {
            numerator: value,
            denominator: BigInt::one(),
        }
    }
}

impl Display for Ratio {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.numerator)?;

        if self.denominator != BigInt::one() {
            write!(f, "/{}", self.denominator)?;
        }

        Ok(())
    }
}
