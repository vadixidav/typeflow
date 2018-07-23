extern crate boolinator;
extern crate itertools;

mod primitive;
pub use primitive::*;

use boolinator::Boolinator;
use itertools::Itertools;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Instance {
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

    /// Try and implicitly convert this `Instance` to a primitive.
    fn try_primitive(&self, env: &Environment) -> Option<Primitive> {
        if let Instance::Primitive(p) = self {
            Some(p.clone())
        } else {
            let local_env = Environment {
                definitions: vec![],
                instances: vec![self.clone()],
                parents: vec![env],
            };

            prim_types()
                .filter_map(|ty| local_env.implicit(ty))
                .map(|ins| ins.must_be_primitive())
                .next()
        }
    }

    fn must_be_primitive(&self) -> Primitive {
        match self {
            Instance::Primitive(p) => p.clone(),
            _ => unreachable!(),
        }
    }

    fn must_be_compound(&self) -> Rc<Compound> {
        match self {
            Instance::Compound(c) => c.clone(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Compound {
    pub ty: String,
    pub contained: Vec<Instance>,
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
pub struct Definiton {
    pub ltype: String,
    pub expr: Expression,
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
pub struct Expression {
    pub parameters: Vec<Parameter>,
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
pub enum Parameter {
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
pub struct Explicit {
    pub target: String,
    pub expr: Expression,
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
#[derive(Clone, Debug, Default)]
pub struct Environment<'a> {
    pub definitions: Vec<Definiton>,
    pub instances: Vec<Instance>,
    pub parents: Vec<&'a Environment<'a>>,
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

    pub fn implicit(&self, ty: &str) -> Option<Instance> {
        self.find_type(ty)
            .or_else(|| {
                self.iter_definitons()
                    .filter_map(|d| d.implicit(ty, self))
                    .next()
            })
            .or_else(|| self.try_builtin(ty))
    }

    fn find_type(&self, ty: &str) -> Option<Instance> {
        self.iter_instances().find(|ins| ins.is_type(&ty)).cloned()
    }

    /// Try to use built-in implicit conversions.
    fn try_builtin(&self, ty: &str) -> Option<Instance> {
        if is_prim_type(ty) {
            self.implicit("+")
                .map(|ins| ins.must_be_compound())
                .and_then(|c| {
                    ordered_types()
                        .map(|ty| c.extract(&ty))
                        .while_some()
                        .map(|ins| ins.try_primitive(self))
                        .fold1(|a, b| a.and_then(|a| b.map(|b| a + b)))
                        .and_then(|a| a)
                        .and_then(|p| p.implicit(ty))
                        .map(Instance::Primitive)
                })
        } else {
            None
        }
    }
}

fn ordered_types() -> impl Iterator<Item = String> {
    (0..).map(|n| format!("@{}", n))
}
