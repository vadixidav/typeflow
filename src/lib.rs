#[macro_use]
extern crate nom;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type Type = String;

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
    Compound { ty: Type, contained: Vec<Instance> },
}

/// A definition is `ltype = expr`.
#[derive(Clone, Debug)]
struct Definiton {
    ltype: Type,
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
    Implicit(Type),
}

/// Defines an explicit conversion.
#[derive(Clone, Debug)]
struct Explicit {
    target: Type,
}

/// An Environment contains all of the definitions and instances available to a given explicit conversion.
#[derive(Clone, Debug)]
struct Environment {
    definitions: Vec<Definiton>,
    instances: Vec<Instance>,
    /// Parent instances are drawn from after this environment.
    /// Parent definitions are tried after this environment's definitions, but are tried for all instances.
    parents: Rc<Environment>,
}
