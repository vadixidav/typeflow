use std::ops::*;
use std::rc::Rc;

use std::fmt;
use crate::Parameter;

pub use Primitive::*;

pub const STYPE: &str = "s";
pub const FTYPE: &str = "f";
pub const ITYPE: &str = "i";
pub const UTYPE: &str = "u";

#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
    S(Rc<str>),
    F(f64),
    I(i64),
    U(u64),
}

impl Primitive {
    pub fn ty(&self) -> &str {
        match self {
            S(_) => STYPE,
            F(_) => FTYPE,
            I(_) => ITYPE,
            U(_) => UTYPE,
        }
    }

    pub fn implicit(&self, ty: &str) -> Option<Primitive> {
        match ty {
            STYPE => Some(match self {
                S(s) => S(s.clone()),
                F(f) => S(f.to_string().into()),
                I(i) => S(i.to_string().into()),
                U(u) => S(u.to_string().into()),
            }),
            FTYPE => match self {
                F(f) => Some(F(*f)),
                I(i) => Some(F(*i as f64)),
                U(u) => Some(F(*u as f64)),
                _ => None,
            },
            ITYPE => match self {
                I(i) => Some(I(*i)),
                U(u) => Some(I(*u as i64)),
                _ => None,
            },
            UTYPE => match self {
                U(u) => Some(U(*u)),
                _ => None,
            },
            _ => None,
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            S(a) => write!(f, "{}", a),
            F(a) => write!(f, "{}", a),
            I(a) => write!(f, "{}", a),
            U(a) => write!(f, "{}", a),
        }
    }
}

pub fn prim_types() -> impl DoubleEndedIterator<Item = &'static str> {
    [STYPE, FTYPE, ITYPE, UTYPE].into_iter().cloned()
}

pub fn prim_pos(p: &str) -> Option<usize> {
    prim_types().position(|s| s == p)
}

pub fn implict_prim_ty(from: &str, to: &str) -> bool {
    prim_pos(from)
        .and_then(|a| prim_pos(to).map(|b| a >= b))
        .unwrap_or(false)
}

pub fn is_prim_type(ty: &str) -> bool {
    prim_types().any(|s| s == ty)
}

pub fn s<S: Into<Rc<str>>>(s: S) -> Parameter {
    S(s.into()).into()
}

pub fn f(f: f64) -> Parameter {
    F(f).into()
}

pub fn i(i: i64) -> Parameter {
    I(i).into()
}

pub fn u(u: u64) -> Parameter {
    U(u).into()
}

impl Add for Primitive {
    type Output = Primitive;

    fn add(self, rhs: Primitive) -> Primitive {
        match self {
            S(a) => match rhs {
                S(b) => S(format!("{}{}", a, b).into()),
                F(b) => S(format!("{}{}", a, b).into()),
                I(b) => S(format!("{}{}", a, b).into()),
                U(b) => S(format!("{}{}", a, b).into()),
            },
            F(a) => match rhs {
                S(b) => S(format!("{}{}", a, b).into()),
                F(b) => F(a + b),
                I(b) => F(a + b as f64),
                U(b) => F(a + b as f64),
            },
            I(a) => match rhs {
                S(b) => S(format!("{}{}", a, b).into()),
                F(b) => F(a as f64 + b),
                I(b) => I(a + b),
                U(b) => I(a + b as i64),
            },
            U(a) => match rhs {
                S(b) => S(format!("{}{}", a, b).into()),
                F(b) => F(a as f64 + b),
                I(b) => I(a as i64 + b),
                U(b) => U(a + b),
            },
        }
    }
}
