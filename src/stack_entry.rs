use std::{
    borrow::Cow,
    collections::BTreeSet,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use float_ord::FloatOrd;
use md4::Digest;

use crate::printf::parse_printf;

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
            (String(x), Number(FloatOrd(n))) if n > &0.0 => {
                rhs.int().map(|n| String(Cow::Owned(x.repeat(n as usize))))
            }
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
        if let Some(n) = self.int() {
            return write!(f, "{}", n.to_string());
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
                        } else {
                            if f.alternate() {
                                " "
                            } else {
                                ", "
                            }
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
    pub fn flt(&self) -> Option<f64> {
        if let Self::Number(x) = self {
            Some(x.0)
        } else {
            None
        }
    }
    pub fn int(&self) -> Option<isize> {
        self.flt().and_then(|n| {
            if n.fract() == 0.0 {
                // SAFETY: checked for whole number, returns an isize, which is guaranteed to fit
                // every whole number between f64::MIN and f64::MAX
                Some(unsafe { n.to_int_unchecked() })
            } else {
                None
            }
        })
    }
    pub fn bool(&self) -> bool {
        self.int().is_some_and(|f| f != &0)
    }
    pub fn hash<D: Digest>(&self) -> Vec<u8> {
        let mut hasher = D::new();

        hasher.update(self.to_string());
        hasher.finalize().to_vec()
    }
    pub fn to_printf(self) -> Option<super::printf::Format> {
        if let StackEntry::String(format) = self {
            match parse_printf(&format) {
                Ok((_, format)) => return Some(format),
                Err(e) => {
                    eprintln!("rpn: Couldn't parse printf: {e}");
                    return None;
                }
            }
        }
        None
    }
}
