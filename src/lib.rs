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

    fn must_be_compound(self) -> Rc<Compound> {
        match self {
            Instance::Compound(c) => c.clone(),
            _ => unreachable!(),
        }
    }

    fn try_primitive(&self) -> Option<Primitive> {
        match self {
            Instance::Primitive(p) => Some(p.clone()),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Compound {
    pub ty: Rc<str>,
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
pub struct Definition {
    pub ltype: Rc<str>,
    pub expr: Expression,
}

impl Definition {
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
    pub params: Vec<Parameter>,
}

impl Expression {
    fn resolve<'a>(&'a self, env: &'a Environment) -> impl Iterator<Item = Option<Instance>> + 'a {
        self.params.iter().map(move |p| p.resolve(env))
    }

    /// Get the types that can be produced by this expression.
    fn produces<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.params.iter().map(Parameter::produces)
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
    pub target: Rc<str>,
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
    pub definitions: Vec<Definition>,
    pub instances: Vec<Instance>,
    pub parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    /// Iterate over instances. We don't inherit instances from parents.
    fn iter_instances(&self) -> impl Iterator<Item = &Instance> {
        self.instances.iter()
    }

    /// Iterate over definitions. We inherit all definitions from parents.
    fn iter_definitions(&'a self) -> impl Iterator<Item = &'a Definition> {
        self.definitions
            .iter()
            .chain(self.parent.iter().flat_map(|p| p.definitions.iter()))
    }

    pub fn implicit(&self, ty: &str) -> Option<Instance> {
        self.find_type(ty)
            .or_else(|| {
                self.iter_definitions()
                    .filter_map(|d| d.implicit(ty, self))
                    .next()
            })
            .or_else(|| self.try_builtin(ty))
    }

    fn find_type(&self, ty: &str) -> Option<Instance> {
        self.iter_instances().find(|ins| ins.is_type(&ty)).cloned()
    }

    /// Try and implicitly convert this `Environment` to a primitive.
    fn try_primitive(&self) -> Option<Primitive> {
        prim_types()
            .rev()
            .filter_map(|ty| self.implicit(ty))
            .filter_map(|ins| ins.try_primitive())
            .next()
    }

    /// Try to use built-in implicit conversions.
    fn try_builtin(&self, ty: &str) -> Option<Instance> {
        if is_prim_type(ty) {
            let add = self
                .implicit("+")
                .map(|ins| ins.must_be_compound())
                .and_then(|c| {
                    ordered_types()
                        .map(|ty| c.extract(&ty))
                        .while_some()
                        .map(Instance::must_be_compound)
                        .map(|c| e(self).all_ins(c.contained.clone()).try_primitive())
                        .while_some()
                        .fold1(|a, b| a + b)
                        .and_then(|p| p.implicit(ty))
                        .map(Instance::Primitive)
                });
            add
        } else {
            None
        }
    }

    pub fn def(mut self, def: Definition) -> Self {
        self.definitions.push(def);
        self
    }

    pub fn ins(mut self, ins: Instance) -> Self {
        self.instances.push(ins);
        self
    }

    pub fn all_ins(mut self, ins: Vec<Instance>) -> Self {
        self.instances = ins;
        self
    }
}

fn ordered_types() -> impl Iterator<Item = String> {
    (0..).map(|n| format!("@{}", n))
}

pub fn c(ty: &str, contained: Vec<Instance>) -> Instance {
    Instance::Compound(Rc::new(Compound {
        ty: ty.into(),
        contained,
    }))
}

pub fn e<'a, E: Into<Option<&'a Environment<'a>>>>(e: E) -> Environment<'a> {
    Environment {
        definitions: vec![],
        instances: vec![],
        parent: e.into(),
    }
}

pub fn d(ltype: &str, params: Vec<Parameter>) -> Definition {
    Definition {
        ltype: ltype.into(),
        expr: Expression { params },
    }
}
