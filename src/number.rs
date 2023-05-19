use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Div, Mul, Sub},
};

use anyhow::ensure;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, Zero};

#[derive(Clone, Debug)]
pub enum Number {
    Ratio(Ratio),
    Undefined,
}

impl Number {
    pub fn pow(self, rhs: Self) -> anyhow::Result<Self> {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => lhs.pow(rhs),
            _ => Ok(Self::Undefined),
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => Self::Ratio(lhs + rhs),
            _ => Self::Undefined,
        }
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => Self::Ratio(lhs - rhs),
            _ => Self::Undefined,
        }
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => Self::Ratio(lhs * rhs),
            _ => Self::Undefined,
        }
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Ratio(lhs), Self::Ratio(rhs)) => lhs / rhs,
            _ => Self::Undefined,
        }
    }
}

impl From<BigInt> for Number {
    fn from(value: BigInt) -> Self {
        Number::Ratio(Ratio {
            numerator: value,
            denominator: BigInt::one(),
        })
    }
}

impl Display for Number {
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

    fn pow(self, rhs: Self) -> anyhow::Result<Number> {
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

        if rhs.denominator == BigInt::zero() {
            return Ok(Number::Undefined);
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

        Ok(Number::Ratio(Ratio {
            numerator,
            denominator,
        }))
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
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        if rhs.numerator == BigInt::zero() {
            Number::Undefined
        } else {
            Number::Ratio(Self {
                numerator: self.numerator * rhs.denominator,
                denominator: self.denominator * rhs.numerator,
            })
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
