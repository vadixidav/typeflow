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

    fn ty(&self) -> &str {
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

impl From<Compound> for Instance {
    fn from(c: Compound) -> Instance {
        Instance::Compound(Rc::new(c))
    }
}

impl From<Primitive> for Instance {
    fn from(p: Primitive) -> Instance {
        Instance::Primitive(p)
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
        self.expr.produces().any(|s| s == ty)
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
    fn resolve<'a>(
        &'a self,
        env: &'a Environment,
    ) -> impl DoubleEndedIterator<Item = Option<Instance>> + 'a {
        self.params.iter().map(move |p| p.resolve(env))
    }

    /// Get the types that can be produced by this expression.
    fn produces(&self) -> impl Iterator<Item = &str> {
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

    fn produces(&self) -> &str {
        use Parameter::*;
        match self {
            Explicit(ep) => &ep.target,
            Implicit(ip) => &ip,
            Literal(p) => p.ty(),
        }
    }
}

impl From<Primitive> for Parameter {
    fn from(p: Primitive) -> Parameter {
        Parameter::Literal(p)
    }
}

impl From<Explicit> for Parameter {
    fn from(e: Explicit) -> Parameter {
        Parameter::Explicit(e)
    }
}

/// Defines an explicit conversion.
#[derive(Clone, Debug)]
pub struct Explicit {
    pub target: Rc<str>,
    pub expr: Expression,
}

impl Explicit {
    pub fn resolve(&self, env: &Environment) -> Option<Instance> {
        // We need to reverse the iterator so the first parameters have highest priority.
        self.expr
            .resolve(env)
            .rev()
            .collect::<Option<Vec<Instance>>>()
            .map(|v| {
                Compound {
                    ty: self.target.clone(),
                    contained: v,
                }.into()
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
        self.definitions.iter().rev().chain(
            self.parent.iter().flat_map(|p| {
                Box::new(p.iter_definitions()) as Box<Iterator<Item = &'a Definition>>
            }),
        )
    }

    pub fn run(&mut self, e: &Explicit) {
        let res = e.resolve(self);
        self.instances.extend(res);
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
            self.implicit("+")
                .map(|ins| ins.must_be_compound())
                .and_then(|c| {
                    ordered_types()
                        .map(|ty| c.extract(&ty))
                        .while_some()
                        .map(Instance::must_be_compound)
                        .map(|c| env(self).all_ins(c.contained.clone()).try_primitive())
                        .while_some()
                        .fold1(|a, b| a + b)
                        .and_then(|p| p.implicit(ty))
                        .map(Instance::Primitive)
                })
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

pub fn otype(n: usize) -> String {
    ordered_types().nth(n).unwrap()
}

pub fn oins(n: usize, ins: Instance) -> Instance {
    Compound {
        ty: otype(n).into(),
        contained: vec![ins],
    }.into()
}

pub fn oexp(n: usize, param: Parameter) -> Parameter {
    Explicit {
        target: otype(n).into(),
        expr: Expression {
            params: vec![param],
        },
    }.into()
}

pub fn c(ty: &str, contained: Vec<Instance>) -> Instance {
    Compound {
        ty: ty.into(),
        contained,
    }.into()
}

pub fn e(target: &str, params: Vec<Parameter>) -> Explicit {
    Explicit {
        target: target.into(),
        expr: Expression { params },
    }
}

pub fn env<'a, E: Into<Option<&'a Environment<'a>>>>(e: E) -> Environment<'a> {
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
