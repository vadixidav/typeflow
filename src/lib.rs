#[macro_use]
extern crate nom;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone, Debug)]
enum Instance {
    I64(i64),
    U64(u64),
    I32(i32),
    U32(u32),
    I16(i16),
    U16(u16),
    I8(i8),
    U8(u8),
    F64(f64),
    F32(f32),
    Compound(Rc<Compound>),
}

#[derive(Clone, Debug)]
struct Compound {
    ty: String,
    contained: Vec<Instance>,
}

impl Instance {
    fn is_type(&self, ty: &str) -> bool {
        use Instance::*;
        ty == match self {
            I64(_) => "i64",
            U64(_) => "u64",
            I32(_) => "i32",
            U32(_) => "u32",
            I16(_) => "i16",
            U16(_) => "u16",
            I8(_) => "i8",
            U8(_) => "u8",
            F64(_) => "f64",
            F32(_) => "f32",
            Compound(c) => &c.ty,
        }
    }
}

/// A definition is `ltype = expr`.
#[derive(Clone, Debug)]
struct Definiton {
    ltype: String,
    expr: Expression,
}

/// An expression is a series of parameters that produce an output environment.
#[derive(Clone, Debug)]
struct Expression {
    parameters: Vec<Parameter>,
}

/// A parameter is a single implicit or explicit type conversion/capture.
/// This may include `!` to capture the whole environment.
#[derive(Clone, Debug)]
enum Parameter {
    Explicit(Explicit),
    Implicit(String),
}

/// Defines an explicit conversion.
#[derive(Clone, Debug)]
struct Explicit {
    target: String,
    expr: Expression,
}

/// An Environment contains all of the definitions and instances available to a given explicit conversion.
#[derive(Clone, Debug)]
struct Environment<'a> {
    definitions: Vec<Definiton>,
    instances: Vec<Instance>,
    /// Parent instances are drawn from after this environment.
    /// Parent definitions are tried after this environment's definitions, but are tried for all instances.
    parents: Vec<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn resolve_param(&self, param: Parameter) -> Option<Instance> {
        match param {
            Parameter::Explicit(_ep) => unimplemented!(),
            Parameter::Implicit(ty) => self
                .instances
                .iter()
                .chain(self.parents.iter().flat_map(|p| p.instances.iter()))
                .find(|ins| ins.is_type(&ty))
                .cloned(),
        }
    }
}
