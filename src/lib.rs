#[macro_use]
extern crate nom;
extern crate boolinator;

mod primitive;
use primitive::Primitive;

use boolinator::Boolinator;
use std::rc::Rc;

#[derive(Clone, Debug)]
enum Instance {
    Primitive(Primitive),
    Compound(Rc<Compound>),
}

impl Instance {
    fn is_type(&self, ty: &str) -> bool {
        ty == self.ty()
    }

    fn ty<'a>(&'a self) -> &'a str {
        use Instance::*;
        match self {
            Primitive(p) => p.ty(),
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

    /// Tries to extract a type implicitly using this definition
    fn implicit(&self, ty: &str, env: &Environment) -> Option<Instance> {
        self.is_implicit(ty)
            .and_option_from(|| env.implicit(&self.ltype).and_then(|ins| ins.extract(ty)))
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
    Literal(Primitive),
}

impl Parameter {
    fn resolve(&self, env: &Environment) -> Option<Instance> {
        use Parameter::*;
        match self {
            Explicit(ep) => ep.resolve(env),
            Implicit(ty) => env.implicit(ty),
            Literal(p) => Some(Instance::Primitive(p.clone())),
        }
    }

    fn produces<'a>(&'a self) -> &'a str {
        use Parameter::*;
        match self {
            Explicit(ep) => &ep.target,
            Implicit(ip) => &ip,
            Literal(p) => p.ty(),
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

    fn implicit(&self, ty: &str) -> Option<Instance> {
        self.find_type(ty).or_else(|| {
            self.iter_definitons()
                .filter_map(|d| d.implicit(ty, self))
                .next()
        })
    }

    fn find_type(&self, ty: &str) -> Option<Instance> {
        self.iter_instances().find(|ins| ins.is_type(&ty)).cloned()
    }
}
