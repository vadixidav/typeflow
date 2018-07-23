#[macro_use]
extern crate nom;
extern crate boolinator;

use boolinator::Boolinator;
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

impl Instance {
    fn is_type(&self, ty: &str) -> bool {
        ty == self.ty()
    }

    fn ty<'a>(&'a self) -> &'a str {
        use Instance::*;
        match self {
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

    /// Try and extract an instance of a type from this instance.
    fn extract(&self, ty: &str) -> Option<Instance> {
        (ty == self.ty()).as_some_from(|| self.clone()).or_else(|| {
            if let Instance::Compound(c) = self {
                c.extract(ty)
            } else {
                None
            }
        })
    }
}

#[derive(Clone, Debug)]
struct Compound {
    ty: String,
    contained: Vec<Instance>,
}

impl Compound {
    fn extract(&self, ty: &str) -> Option<Instance> {
        self.contained
            .iter()
            .filter_map(|ins| ins.extract(ty))
            .next()
    }
}

/// A definition is `ltype = expr`.
#[derive(Clone, Debug)]
struct Definiton {
    ltype: String,
    expr: Expression,
}

impl Definiton {
    /// Checks if a type can be implicitly produced from the `ltype` of this `Definition`.
    fn is_implicit(&self, ty: &str) -> bool {
        self.expr.produces().find(|&s| s == ty).is_some()
    }
}

/// An expression is a series of parameters that produce an output environment.
#[derive(Clone, Debug)]
struct Expression {
    parameters: Vec<Parameter>,
}

impl Expression {
    fn resolve<'a>(&'a self, env: &'a Environment) -> impl Iterator<Item = Option<Instance>> + 'a {
        self.parameters.iter().map(move |p| p.resolve(env))
    }

    /// Get the types that can be produced by this expression.
    fn produces<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.parameters.iter().map(Parameter::produces)
    }
}

/// A parameter is a single implicit or explicit type conversion/capture.
/// This may include `!` to capture the whole environment.
#[derive(Clone, Debug)]
enum Parameter {
    Explicit(Explicit),
    Implicit(String),
}

impl Parameter {
    fn resolve(&self, env: &Environment) -> Option<Instance> {
        use Parameter::*;
        match self {
            Explicit(ep) => ep.resolve(env),
            Implicit(ty) => env.implicit(ty),
        }
    }

    fn produces<'a>(&'a self) -> &'a str {
        use Parameter::*;
        match self {
            Explicit(ep) => &ep.target,
            Implicit(ip) => &ip,
        }
    }
}

/// Defines an explicit conversion.
#[derive(Clone, Debug)]
struct Explicit {
    target: String,
    expr: Expression,
}

impl Explicit {
    fn resolve(&self, env: &Environment) -> Option<Instance> {
        self.expr
            .resolve(env)
            .collect::<Option<Vec<Instance>>>()
            .map(|v| {
                Instance::Compound(Rc::new(Compound {
                    ty: self.target.clone(),
                    contained: v,
                }))
            })
    }
}

/// An Environment contains all of the definitions and instances available to a given explicit conversion.
#[derive(Clone, Debug)]
struct Environment<'a> {
    definitions: Vec<Definiton>,
    instances: Vec<Instance>,
    parents: Vec<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    fn iter_instances(&'a self) -> impl Iterator<Item = &'a Instance> {
        self.instances
            .iter()
            .chain(self.parents.iter().flat_map(|p| p.instances.iter()))
    }

    fn iter_definitons(&'a self) -> impl Iterator<Item = &'a Definiton> {
        self.definitions
            .iter()
            .chain(self.parents.iter().flat_map(|p| p.definitions.iter()))
    }

    fn find_type(&self, ty: &str) -> Option<Instance> {
        self.iter_instances().find(|ins| ins.is_type(&ty)).cloned()
    }

    fn implicit(&self, ty: &str) -> Option<Instance> {
        self.find_type(ty).or_else(|| {
            self.iter_definitons()
                .filter(|d| d.is_implicit(ty))
                .filter_map(|d| self.implicit(&d.ltype).and_then(|ins| ins.extract(ty)))
                .next()
        })
    }
}
