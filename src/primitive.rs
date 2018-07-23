use std::borrow::Cow;
use std::ops::*;
use Instance;

#[derive(Clone, Debug, PartialEq)]
pub enum Primitive {
    String(Cow<'static, str>),
    F(f64),
    I(i64),
    U(u64),
}

impl Primitive {
    pub fn ty(&self) -> &str {
        use self::Primitive::*;
        match self {
            String(_) => "str",
            F(_) => "f",
            I(_) => "i",
            U(_) => "u",
        }
    }

    pub fn implicit(&self, ty: &str) -> Option<Primitive> {
        use Primitive::*;
        match ty {
            "str" => Some(match self {
                String(s) => String(s.clone()),
                F(f) => String(Cow::Owned(f.to_string())),
                I(i) => String(Cow::Owned(i.to_string())),
                U(u) => String(Cow::Owned(u.to_string())),
            }),
            "f" => match self {
                F(f) => Some(F(*f)),
                I(i) => Some(F(*i as f64)),
                U(u) => Some(F(*u as f64)),
                _ => None,
            },
            "i" => match self {
                I(i) => Some(I(*i)),
                U(u) => Some(I(*u as i64)),
                _ => None,
            },
            "u" => match self {
                U(u) => Some(U(*u)),
                _ => None,
            },
            _ => None,
        }
    }
}

pub fn s<S: Into<Cow<'static, str>>>(s: S) -> Instance {
    Instance::Primitive(Primitive::String(s.into()))
}

pub fn f(f: f64) -> Instance {
    Instance::Primitive(Primitive::F(f))
}

pub fn i(i: i64) -> Instance {
    Instance::Primitive(Primitive::I(i))
}

pub fn u(u: u64) -> Instance {
    Instance::Primitive(Primitive::U(u))
}

pub fn prim_types() -> impl Iterator<Item = &'static str> {
    ["str", "f", "i", "u"].into_iter().cloned()
}

pub fn is_prim_type(ty: &str) -> bool {
    prim_types().any(|s| s == ty)
}

impl Add for Primitive {
    type Output = Primitive;

    fn add(self, rhs: Primitive) -> Primitive {
        use Primitive::*;
        match self {
            String(a) => match rhs {
                String(b) => String(Cow::Owned(format!("{}{}", a, b))),
                F(b) => String(Cow::Owned(format!("{}{}", a, b))),
                I(b) => String(Cow::Owned(format!("{}{}", a, b))),
                U(b) => String(Cow::Owned(format!("{}{}", a, b))),
            },
            F(a) => match rhs {
                String(b) => String(Cow::Owned(format!("{}{}", a, b))),
                F(b) => F(a + b),
                I(b) => F(a + b as f64),
                U(b) => F(a + b as f64),
            },
            I(a) => match rhs {
                String(b) => String(Cow::Owned(format!("{}{}", a, b))),
                F(b) => F(a as f64 + b),
                I(b) => I(a + b),
                U(b) => I(a + b as i64),
            },
            U(a) => match rhs {
                String(b) => String(Cow::Owned(format!("{}{}", a, b))),
                F(b) => F(a as f64 + b),
                I(b) => I(a as i64 + b),
                U(b) => U(a + b),
            },
        }
    }
}
