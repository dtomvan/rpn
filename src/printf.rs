use std::{
    fmt::{Display, LowerHex, Octal, Pointer, UpperHex},
    num::ParseIntError,
};

use either::Either;
use float_ord::FloatOrd;
use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::{char, digit1, one_of},
    combinator::{map_opt, map_res, opt},
    multi::many0,
    sequence::tuple,
    IResult,
};
use num_traits::Float;
use strfmt::{strfmt, DisplayStr};
use thiserror::Error;

use crate::stack_entry::StackEntry;

const MANTISSA_OFFSET: i16 = f64::MANTISSA_DIGITS as i16 - 1;

/// strtod(3) Format: [sign]0x{sign_bit}.{hexable_mantissa:x}p{offset_exponent}
struct HexFloat {
    hexable_mantissa: u64,
    sign_bit: u8,
    sign: i8,
    offset_exponent: i16,
}

impl From<f64> for HexFloat {
    fn from(n: f64) -> Self {
        let (mantissa, exponent, sign) = n.integer_decode();
        let sign_bit = (mantissa & 0x1) as u8;
        let hexable_mantissa = (mantissa << 12) & 0xFFFFFFFFFFFFFFF0;
        let offset_exponent = exponent + MANTISSA_OFFSET;
        Self {
            hexable_mantissa,
            sign_bit,
            sign,
            offset_exponent,
        }
    }
}

macro_rules! upperlower_impl {
    ($what:ty, LowerHex) => {
        impl LowerHex for $what {
            upperlower_impl! { "{}0x{}.{:x}p{}" }
        }
    };
    ($what:ty, UpperHex) => {
        impl UpperHex for $what {
            upperlower_impl! { "{}0x{}.{:X}p{}" }
        }
    };
    ($how:literal) => {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let Self {
                hexable_mantissa,
                sign_bit,
                sign,
                offset_exponent,
            } = self;
            write!(
                f,
                $how,
                if sign.is_positive() {
                    if f.sign_plus() {
                        "+"
                    } else {
                        ""
                    }
                } else {
                    "-"
                },
                sign_bit,
                hexable_mantissa,
                offset_exponent,
            )
        }
    };
}

upperlower_impl!(HexFloat, LowerHex);
upperlower_impl!(HexFloat, UpperHex);

pub enum Primitive {
    UInt(usize),
    Int(isize),
    Float(f64),
    Char(char),
    String(String),
}

impl Primitive {
    pub fn as_uint(&self) -> Option<&usize> {
        if let Self::UInt(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::UInt(u) => Display::fmt(u, f),
            Primitive::Int(u) => Display::fmt(u, f),
            Primitive::Float(u) => Display::fmt(u, f),
            Primitive::Char(u) => Display::fmt(u, f),
            Primitive::String(u) => Display::fmt(u, f),
        }
    }
}

impl DisplayStr for Primitive {
    fn display_str(&self, f: &mut strfmt::Formatter) -> strfmt::Result<()> {
        match self {
            Primitive::UInt(u) => f.usize(*u),
            Primitive::Int(u) => f.isize(*u),
            Primitive::Float(u) => f.f64(*u),
            Primitive::Char(u) => f.str(u.encode_utf8(&mut [0; 8])),
            Primitive::String(u) => f.str(u.as_str()),
        }
    }
}

impl Pointer for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::UInt(x) => write!(f, "{x:p}"),
            Primitive::Int(x) => write!(f, "{x:p}"),
            Primitive::Float(x) => write!(f, "{x:p}"),
            Primitive::Char(x) => write!(f, "{x:p}"),
            Primitive::String(x) => write!(f, "{x:p}"),
        }
    }
}

impl LowerHex for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::UInt(x) => write!(f, "{x:#x}"),
            Primitive::Int(x) => write!(f, "{x:#x}"),
            Primitive::Float(x) => write!(f, "{:#x}", x.to_bits()),
            Primitive::Char(x) => write!(f, "{:#x}", *x as u8),
            Primitive::String(x) => write!(f, "0x{}", hex::encode(x)),
        }
    }
}

impl UpperHex for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::UInt(x) => write!(f, "{x:#x}"),
            Primitive::Int(x) => write!(f, "{x:#x}"),
            Primitive::Float(x) => write!(f, "{:#x}", x.to_bits()),
            Primitive::Char(x) => write!(f, "{:#x}", *x as u8),
            Primitive::String(x) => write!(f, "0x{}", hex::encode_upper(x)),
        }
    }
}

impl Octal for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::UInt(x) => write!(f, "{x:#o}"),
            Primitive::Int(x) => write!(f, "{x:#o}"),
            Primitive::Float(x) => write!(f, "{:#o}", x.to_bits()),
            Primitive::Char(x) => write!(f, "{:#o}", *x as u8),
            Primitive::String(x) => {
                write!(f, "0o")?;
                for ch in x.bytes() {
                    write!(f, "{}", ch)?;
                }
                Ok(())
            }
        }
    }
}

impl From<StackEntry<'_>> for Primitive {
    fn from(e: StackEntry) -> Self {
        match e {
            StackEntry::Number(FloatOrd(f)) => {
                if f.fract() == 0.0 && f.is_finite() {
                    let int = unsafe { f.to_int_unchecked() };
                    if int >= 0 {
                        Self::UInt(int as usize)
                    } else {
                        Self::Int(int)
                    }
                } else {
                    Self::Float(f)
                }
            }
            StackEntry::String(s) if s.len() == 1 => Self::Char(s.chars().next().unwrap()),
            StackEntry::String(s) => Self::String(s.to_string()),
            StackEntry::Set(_) | StackEntry::Md4 | StackEntry::Sha256 => {
                Self::String(format!("{e:#}"))
            }
        }
    }
}

impl Flags {
    fn from_vec(input: &[char]) -> Self {
        let mut flags = Flags::empty();
        for char in input {
            match char {
                '-' => flags |= Flags::LEFT_ALIGN,
                '+' => flags |= Flags::PREPEND_PLUS,
                ' ' => flags |= Flags::PREPEND_SPACE,
                '0' => flags |= Flags::PREPEND_ZERO,
                '\'' => flags |= Flags::THOUSANDS_GROUPING,
                '#' => flags |= Flags::ALTERNATE_FORM,
                _ => unreachable!("Previously parsed to only include one of the flags."),
            };
        }
        flags
    }
}

bitflags::bitflags! {
    pub struct Flags: u8 {
        const LEFT_ALIGN = 1;
        const PREPEND_PLUS = 1 << 1;
        const PREPEND_SPACE = 1 << 2;
        const PREPEND_ZERO = 1 << 3;
        const THOUSANDS_GROUPING = 1 << 4;
        const ALTERNATE_FORM = 1 << 5;
    }
}

#[derive(Debug, Error)]
pub enum FormatError {
    #[error("Arguments were exhausted")]
    Exhausted,
    #[error("Error while formatting")]
    StrFmt(#[from] strfmt::FmtError),
}

type FormatArgument = Either<String, Argument>;

#[derive(Debug, Clone, PartialEq)]
pub struct Format(Vec<FormatArgument>);

impl Format {
    pub fn restrict(mut self, args_n: usize) -> Self {
        let mut occurences = 0;
        for (i, x) in self.0.iter().enumerate() {
            if x.is_right() {
                occurences += 1;
            }
            if occurences >= args_n {
                let rest = self
                    .0
                    .drain((i + 1)..)
                    .collect_vec()
                    .into_iter()
                    .fuse()
                    .filter(|x| x.is_left());
                self.0.extend(rest);
                break;
            }
        }
        self
    }

    pub fn args_needed(&self) -> usize {
        let mut res = 0;
        for tag in self.0.iter().filter(|x| x.is_right()) {
            if let Width::Arg = tag.as_ref().unwrap_right().width {
                res += 2;
            } else {
                res += 1;
            }
        }
        res
    }

    pub fn format(mut self, mut args: Vec<Primitive>) -> Result<String, FormatError> {
        let mut res = String::new();
        while !self.0.is_empty() {
            let ex = self.0.pop().unwrap();
            match (ex, args.is_empty()) {
                (Either::Left(s), _) => {
                    res.push_str(&s);
                }
                (Either::Right(ex), false) => {
                    let gi = args.pop().unwrap();
                    let mut get_arg = || args.pop().and_then(|x| x.as_uint().copied());
                    let width = if ex.width.is_arg() { get_arg() } else { None };
                    let precision = if ex.precision.is_arg() {
                        get_arg()
                    } else {
                        None
                    };
                    ex.format(gi, width, precision)?;
                }
                (Either::Right(_), true) => {
                    return Err(FormatError::Exhausted);
                }
            }
        }
        Ok(res)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Width {
    /// Not specified
    Empty,
    /// When filling in this specifier, grab the next argument to get an int
    /// Specified as `*`
    Arg,
    /// Specified as an integer
    Int(usize),
}

impl Width {
    pub fn from_str(str: &str) -> Result<Self, ParseIntError> {
        if str.is_empty() {
            Ok(Self::Empty)
        } else if str == "*" {
            Ok(Self::Arg)
        } else {
            str.parse().map(Self::Int)
        }
    }

    /// Returns `true` if the width is [`Arg`].
    ///
    /// [`Arg`]: Width::Arg
    #[must_use]
    pub fn is_arg(&self) -> bool {
        matches!(self, Self::Arg)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub specifier: Specifier,
    pub flags: Flags,
    pub width: Width,
    pub precision: Width,
}

impl Argument {
    fn format(
        &self,
        inp: Primitive,
        width_arg: Option<usize>,
        precision_arg: Option<usize>,
    ) -> Result<String, strfmt::FmtError> {
        let mut num_type = None;
        let mut format_option = None;
        let mut format_arg = None;
        match (self.specifier, &inp) {
            (Specifier::Hex, _) => {
                num_type = Some("x");
            }
            (Specifier::UpperHex, _) => {
                num_type = Some("X");
            }
            (Specifier::Octal, _) => {
                num_type = Some("o");
            }
            (Specifier::Pointer, _) => {
                num_type = Some("p");
            }
            (Specifier::Double(n), Primitive::Float(f)) => match n {
                Notation::Auto => todo!(),
                Notation::Normal => {
                    num_type = Some("");
                }
                Notation::Scientific => {
                    num_type = Some("e");
                }
                Notation::Hex => {
                    let hf = HexFloat::from(*f);
                    let res = if self.flags.contains(Flags::PREPEND_PLUS) {
                        format!("{hf:+x}")
                    } else {
                        format!("{hf:x}")
                    };
                    format_arg = Some(Primitive::String(res));
                }
            },
            (Specifier::UpperDouble(n), Primitive::Float(f)) => match n {
                Notation::Auto => todo!(),
                Notation::Normal => {
                    num_type = Some("");
                }
                Notation::Scientific => {
                    num_type = Some("E");
                }
                Notation::Hex => {
                    let hf = HexFloat::from(*f);
                    let res = if self.flags.contains(Flags::PREPEND_PLUS) {
                        format!("{hf:+X}")
                    } else {
                        format!("{hf:X}")
                    };
                    format_arg = Some(Primitive::String(res));
                }
            },
            (Specifier::Char, Primitive::Int(_) | Primitive::UInt(_) | Primitive::Char(_))
            | (Specifier::Int | Specifier::UInt | Specifier::String, _) => {
                format_option = Some(String::new());
            }
            (Specifier::Char, Primitive::String(s)) => {
                format_option = Some(String::new());
                format_arg = Some(Primitive::Char(s.chars().next().unwrap_or_else(|| '\0')));
            }
            _ => {
                format_option = Some(String::new());
                format_arg = Some(Primitive::String(String::from("\0")));
            }
        };
        if let Some(num_type) = num_type {
            let precision = match self.precision {
                Width::Empty => String::new(),
                Width::Arg => precision_arg.map(|x| format!(".{x}")).unwrap_or_default(),
                Width::Int(i) => format!(".{i}"),
            };
            let width = match self.width {
                Width::Empty => String::new(),
                Width::Arg => width_arg.map(|x| x.to_string()).unwrap_or_default(),
                Width::Int(i) => i.to_string(),
            };
            format_option = Some(format!(":{precision}{width}{}", num_type));
        }
        let _ = format_arg.get_or_insert(inp);
        let format_arg = format_arg.expect("All cases should have been handled by now.");
        let format_option = format_option.expect("All cases should have been handled by now.");
        let fmt = format!("{{format_arg{}}}", format_option);
        strfmt!(fmt.as_str(), format_arg => format_arg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Specifier {
    Int,
    Hex,
    UpperHex,
    UInt,
    Octal,
    Double(Notation),
    UpperDouble(Notation),
    String,
    Char,
    Pointer,
}

impl Specifier {
    fn from_char(c: char) -> Option<Self> {
        use Notation::{Hex, *};
        use Specifier::*;
        match c {
            'd' | 'i' => Some(Int),
            'x' => Some(Self::Hex),
            'X' => Some(UpperHex),
            'u' => Some(UInt),
            'o' => Some(Octal),
            'f' => Some(Double(Normal)),
            'F' => Some(UpperDouble(Normal)),
            'e' => Some(Double(Scientific)),
            'E' => Some(UpperDouble(Scientific)),
            'g' => Some(Double(Auto)),
            'G' => Some(UpperDouble(Auto)),
            'a' => Some(Double(Hex)),
            'A' => Some(UpperDouble(Hex)),
            's' => Some(String),
            'c' => Some(Char),
            'p' => Some(Pointer),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Notation {
    Auto,
    Normal,
    Scientific,
    Hex,
}

fn format_argument<'a>(s: &'a str) -> IResult<&str, FormatArgument> {
    match alt::<_, _, nom::error::Error<_>, _>((tag("%%"), tag("%")))(s) {
        Ok((s, lit)) if lit == "%%" => Ok((s, FormatArgument::Left(lit.to_string()))),
        Ok((s, _)) => {
            let (s, flags) = many0(one_of("-+ 0\'#"))(s)?;
            let flags = Flags::from_vec(&flags[..]);
            let mut width_p = map_res(alt((digit1, tag("*"))), Width::from_str);
            let (s, width) = opt(&mut width_p)(s)?;
            let (s, precision) = opt(tuple((tag("."), &mut width_p)))(s)?;
            let precision = precision.map(|(_, wid)| wid);

            // We don't care about this field
            let (s, _) = opt(alt((
                tag("hh"),
                tag("h"),
                tag("l"),
                tag("ll"),
                tag("z"),
                tag("j"),
                tag("t"),
            )))(s)?;

            let (s, specifier) = map_opt(
                alt((
                    char('d'),
                    char('i'),
                    char('u'),
                    char('f'),
                    char('F'),
                    char('e'),
                    char('E'),
                    char('g'),
                    char('G'),
                    char('x'),
                    char('X'),
                    char('o'),
                    char('s'),
                    char('c'),
                    char('p'),
                    char('a'),
                    char('A'),
                )),
                Specifier::from_char,
            )(s)?;

            Ok((
                s,
                FormatArgument::Right(Argument {
                    specifier,
                    flags,
                    width: width.unwrap_or(Width::Empty),
                    precision: precision.unwrap_or(Width::Empty),
                }),
            ))
        }
        Err(_) => {
            let (s, until) = take_till1(|x| x == '%')(s)?;
            Ok((s, FormatArgument::Left(until.to_string())))
        }
    }
}

pub fn parse_printf(s: &str) -> IResult<&str, Format> {
    let (s, out) = many0(format_argument)(s)?;
    debug_assert!(s.is_empty());
    Ok((s, Format(out)))
}
