use std::{
    borrow::Cow,
    collections::BTreeSet,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use anyhow::{anyhow, Result};
use float_ord::FloatOrd;
use md4::Digest;
use nom::multi::many0;

use crate::printf::{parse_format_argument, Format};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum StackEntry<'a> {
    Number(FloatOrd<f64>),
    String(Cow<'a, str>),
    Set(BTreeSet<Self>),
    Md4,
    Sha256,
}

// Spagetti I guess
impl<'a> Add<StackEntry<'a>> for StackEntry<'a> {
    type Output = Option<StackEntry<'a>>;

    fn add(self, rhs: StackEntry<'a>) -> Self::Output {
        use StackEntry::*;
        match (self, rhs) {
            (Number(x), Number(y)) => Some(Number(FloatOrd(x.0 + y.0))),
            (String(x), String(y)) => Some(String(x + y)),
            (Set(x), Set(y)) => Some(Set(&x | &y)),
            _ => None,
        }
    }
}

impl<'a> Sub<StackEntry<'a>> for StackEntry<'a> {
    type Output = Option<StackEntry<'a>>;

    fn sub(self, rhs: StackEntry<'a>) -> Self::Output {
        use StackEntry::*;
        match (self, rhs) {
            (Number(x), Number(y)) => Some(Number(FloatOrd(x.0 - y.0))),
            (Set(x), Set(y)) => Some(Set(&x - &y)),
            _ => None,
        }
    }
}

impl<'a> Mul<StackEntry<'a>> for StackEntry<'a> {
    type Output = Option<StackEntry<'a>>;

    fn mul(self, rhs: StackEntry<'a>) -> Self::Output {
        use StackEntry::*;
        match (self, &rhs) {
            (Number(x), Number(y)) => Some(Number(FloatOrd(x.0 * y.0))),
            (String(x), Number(FloatOrd(n))) if n > &0.0 => rhs
                .as_int()
                .map(|n| String(Cow::Owned(x.repeat(n as usize)))),
            _ => None,
        }
    }
}

impl<'a> Div<StackEntry<'a>> for StackEntry<'a> {
    type Output = Option<StackEntry<'a>>;

    fn div(self, rhs: StackEntry<'a>) -> Self::Output {
        use StackEntry::*;
        match (self, rhs) {
            (Number(x), Number(y)) => Some(Number(FloatOrd(x.0 / y.0))),
            (Set(x), Set(y)) => Some(Set(&x & &y)),
            _ => None,
        }
    }
}

impl<'a> Display for StackEntry<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(n) = self.as_int() {
            return write!(f, "{}", n);
        }
        use StackEntry::*;
        match self {
            Number(FloatOrd(n)) => {
                write!(f, "{}", n)
            }
            String(s) => write!(f, "{s}"),
            Set(s) => write!(
                f,
                "{}{}{}",
                if f.alternate() { "" } else { "[" },
                s.iter()
                    .rev()
                    .fold(std::string::String::new(), |acc, x| format!(
                        "{x}{}{acc}",
                        if acc.is_empty() {
                            ""
                        } else if f.alternate() {
                            " "
                        } else {
                            ", "
                        }
                    )),
                if f.alternate() { "" } else { "]" },
            ),
            Md4 => write!(f, "MD4"),
            Sha256 => write!(f, "SHA256"),
        }
    }
}

impl<'a> StackEntry<'a> {
    pub fn num(n: f64) -> Self {
        Self::Number(FloatOrd(n))
    }

    pub fn as_int(&self) -> Option<isize> {
        self.try_as_int().ok()
    }

    pub fn try_as_int(&self) -> Result<isize> {
        self.try_as_number().and_then(|n| {
            if n.fract() == 0.0 {
                // SAFETY: checked for whole number, returns an isize, which is guaranteed to fit
                // every whole number between f64::MIN and f64::MAX
                Ok(unsafe { n.to_int_unchecked() })
            } else {
                Err(anyhow!("Failed to convert float into integer."))
            }
        })
    }

    pub fn bool(self) -> bool {
        self.as_int().is_some_and(|f| f != &0)
    }

    pub fn hash<D: Digest>(&self) -> Vec<u8> {
        let mut hasher = D::new();

        hasher.update(self.to_string());
        hasher.finalize().to_vec()
    }

    pub fn parse_printf(self) -> Result<super::printf::Format> {
        let str = self.try_into_string()?;
        // Bruh the E type has references
        let (_, fmt) =
            many0(parse_format_argument)(&str).map_err(|x| anyhow!("printf: {}", x.to_string()))?;
        Ok(Format(fmt))
    }

    pub fn try_as_number(&self) -> Result<f64> {
        if let Self::Number(FloatOrd(v)) = self {
            Ok(*v)
        } else {
            Err(anyhow!("Invalid argument: expected Number, got {:?}", self))
        }
    }

    pub fn try_into_string(self) -> Result<String> {
        if let Self::String(v) = self {
            Ok(v.to_string())
        } else {
            Err(anyhow!("Invalid argument: expected String, got {:?}", self))
        }
    }

    // Maybe needed at some point in the future
    // pub fn try_into_set(self) -> Result<BTreeSet<Self>> {
    //     if let Self::Set(v) = self {
    //         Ok(v)
    //     } else {
    //         Err(anyhow!("Invalid argument: expected Set, got {:?}", self))
    //     }
    // }
}
