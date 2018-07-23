use std::borrow::Cow;
use std::ops::*;

#[derive(Clone, Debug)]
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
