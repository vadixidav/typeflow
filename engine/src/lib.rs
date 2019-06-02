mod primitive;
pub use primitive::*;

use boolinator::Boolinator;
use either::Either;
use itertools::Itertools;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use log::*;

use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Instance {
    Primitive(Primitive),
    Compound(Rc<Compound>),
}

impl Instance {
    fn is_type(&self, ty: &str) -> bool {
        ty == self.ty() || implict_prim_ty(self.ty(), ty)
    }

    fn ty(&self) -> &str {
        use Instance::*;
        match self {
            Primitive(p) => p.ty(),
            Compound(c) => &c.ty,
        }
    }

    /// Try and extract a type if it is this instance.
    fn extract_self(&self, ty: &str) -> Option<Instance> {
        self.is_type(ty).as_some_from(|| self.clone())
    }

    /// Try and extract a type if it is inside of this instance.
    fn extract(&self, ty: &str) -> Option<Instance> {
        self.try_compound().and_then(|c| c.extract(ty))
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

    fn try_compound(&self) -> Option<Rc<Compound>> {
        match self {
            Instance::Compound(c) => Some(c.clone()),
            _ => None,
        }
    }
}

impl<'a> From<Compound> for Instance {
    fn from(c: Compound) -> Instance {
        Instance::Compound(Rc::new(c))
    }
}

impl From<Primitive> for Instance {
    fn from(p: Primitive) -> Instance {
        Instance::Primitive(p)
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instance::Compound(c) => c.fmt(f),
            Instance::Primitive(p) => p.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Compound {
    pub ty: Rc<str>,
    pub env: Environment,
}

impl Compound {
    fn extract(&self, ty: &str) -> Option<Instance> {
        self.env
            .data
            .instances
            .iter()
            .rev()
            .filter_map(|ins| ins.extract_self(ty))
            .next()
    }
}

impl fmt::Display for Compound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ty)?;
        let nins = self.env.data.instances.len();
        if nins != 0 {
            write!(f, "(")?;
            for i in 0..nins - 1 {
                write!(f, "{} ", self.env.data.instances[i])?;
            }
            write!(f, "{}", self.env.data.instances[nins - 1])?;
            write!(f, ")")?;
        }
        Ok(())
    }
}

/// A definition is `target params`.
#[derive(Clone, Debug, PartialEq)]
pub struct Definition {
    pub target: Rc<str>,
    pub params: Vec<Parameter>,
}

impl Definition {
    /// Checks if a type can be implicitly produced from the `target` of this `Definition`.
    fn is_implicit(&self, ty: &str) -> bool {
        self.produces().any(|s| s == ty || implict_prim_ty(s, ty))
    }

    /// Checks if a type can be explicitly produced by this definition.
    fn is_explicit(&self, ty: &str) -> bool {
        let res = &*self.target == ty;
        trace!("def::is_explicit {} -> {}", ty, res);
        res
    }

    /// Tries to extract a type implicitly using this definition
    fn implicit(&self, lex: &Lexicon, data: &Data, ty: &str) -> Option<Instance> {
        self.is_implicit(ty).and_option_from(|| {
            trace!("def::implicit {} -> {}", self.target, ty);
            lex.implicit(data, self.target.clone())
                .and_then(|ins| ins.extract(ty))
        })
    }

    /// Get the types that can be produced by this Arguments.
    fn produces(&self) -> impl Iterator<Item = &str> {
        self.params.iter().map(|p| p.produces())
    }
}

/// Arguments is a series of expressions that produce an output Data.
/// 
/// The arguments have inputs and may produce several outputs.
#[derive(Clone, Debug, PartialEq)]
pub struct Arguments {
    pub exprs: Vec<Expression>,
}

impl Arguments {
    fn resolve(&self, lex: &Lexicon, data: &Data) -> Option<Environment> {
        let mut scope = Scope::default();
        self.exprs
            .iter()
            .rev()
            .filter_map(|a| match a {
                Expression::Definition(d) => {
                    scope.def(d.clone());
                    None
                }
                Expression::Parameter(p) => Some(p),
            }).map(|p| p.resolve(lex, &data))
            .collect::<Option<Vec<Instance>>>()
            .map(|v| Environment {
                data: v.into(),
                scope,
            })
    }
}

/// An Expression is either a Definition or a Parameter.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Definition(Definition),
    Parameter(Parameter),
}

impl From<Definition> for Expression {
    fn from(d: Definition) -> Expression {
        Expression::Definition(d)
    }
}

impl From<Parameter> for Expression {
    fn from(p: Parameter) -> Expression {
        Expression::Parameter(p)
    }
}

impl From<Primitive> for Expression {
    fn from(p: Primitive) -> Expression {
        Expression::Parameter(Parameter::Literal(p))
    }
}

/// A parameter is a single implicit or explicit type conversion/capture.
/// This may include `!` to capture the whole Data.
#[derive(Clone, Debug, PartialEq)]
pub enum Parameter {
    Explicit(Explicit),
    Implicit(Rc<str>),
    Literal(Primitive),
}

impl Parameter {
    fn resolve(&self, lex: &Lexicon, data: &Data) -> Option<Instance> {
        use Parameter::*;
        match self {
            Explicit(ep) => ep.resolve(lex, data),
            Implicit(ty) => (&**ty == "!")
                .as_some_from(|| {
                    Compound {
                        ty: "!".into(),
                        env: Environment {
                            data: data.clone(),
                            ..Default::default()
                        },
                    }.into()
                }).or_else(|| lex.implicit(data, ty.clone())),
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
#[derive(Clone, Debug, PartialEq)]
pub struct Explicit {
    pub target: Rc<str>,
    pub args: Arguments,
}

impl Explicit {
    fn resolve(&self, lex: &Lexicon, data: &Data) -> Option<Instance> {
        trace!("exp::resolve {} {{", self.target);
        let res = is_builtin_raw(&self.target)
            .and_option_from(|| {
                self.args.resolve(lex, data).map(|env| {
                    Compound {
                        ty: self.target.clone(),
                        env,
                    }.into()
                })
            }).or_else(|| {
                self.args
                    .resolve(lex, data)
                    .and_then(|env| env.explicit(lex, self.target.clone()))
            });
        trace!("}} -> {}", res.is_some());
        res
    }
}

/// A scope contains definitions in a given scope.
#[derive(Clone, Debug, Default, PartialEq)]
struct Scope {
    definitions: Vec<Definition>,
}

impl Scope {
    pub fn def(&mut self, d: Definition) {
        self.definitions.push(d);
    }
}

impl From<Vec<Definition>> for Scope {
    fn from(definitions: Vec<Definition>) -> Scope {
        Scope { definitions }
    }
}

/// A lexicon contains all the definitions known in a given context.
#[derive(Clone, Debug)]
struct Lexicon<'a> {
    scopes: Vec<&'a Scope>,
}

impl<'a> Lexicon<'a> {
    fn new(scope: &'a Scope) -> Self {
        Lexicon {
            scopes: vec![scope],
        }
    }

    /// Iterate over definitions. We inherit all definitions from parents.
    fn iter_definitions(&'a self) -> impl Iterator<Item = &'a Definition> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|s| s.definitions.iter().rev())
    }

    fn implicit<S: Into<Rc<str>>>(&self, data: &Data, ty: S) -> Option<Instance> {
        let ty = ty.into();
        trace!("lex::implicit {} {{", ty);
        let res = data
            .find_type(&ty)
            .or_else(|| {
                self.iter_definitions()
                    .filter_map(|d| d.implicit(self, data, &ty))
                    .next()
            }).or_else(|| self.try_builtin(data, &ty));
        trace!("}} -> {}", res.is_some());
        res
    }

    /// Try to use built-in implicit conversions.
    fn try_builtin(&self, data: &Data, ty: &str) -> Option<Instance> {
        if is_prim_type(ty) {
            self.implicit(data, "+")
                .map(|ins| ins.must_be_compound())
                .and_then(|c| {
                    let lex = self.with(&c.env.scope);
                    ordered_types()
                        .map(|ty| lex.implicit(&c.env.data, ty))
                        .while_some()
                        .map(Instance::must_be_compound)
                        .map(|c| lex.try_primitive(&c.env.data))
                        .fold1(|a, b| a.and_then(|a| b.map(|b| a + b)))
                        .and_then(|p| p.and_then(|p| p.implicit(ty)))
                        .map(Instance::Primitive)
                })
        } else {
            None
        }
    }

    /// Try and implicitly convert this `Data` to a primitive.
    fn try_primitive(&self, data: &Data) -> Option<Primitive> {
        prim_types()
            .rev()
            .filter_map(|ty| self.implicit(&data, ty))
            .filter_map(|ins| ins.try_primitive())
            .next()
    }

    /// Erases the lifetime of the Scope reference internally since
    /// it will be forgotten when this is dropped.
    fn with<'b>(&'b self, scope: &Scope) -> ScopedLex<'a> {
        unsafe {
            (*(&self.scopes as *const Vec<&Scope> as *mut Vec<&Scope>))
                .push(::std::mem::transmute(scope));
            ScopedLex {
                lex: &mut *(self as *const Lexicon<'a> as *mut Lexicon<'a>),
            }
        }
    }
}

/// A `ScopedLex` scopes a `Scope` being added to a lexicon to its lifetime.
/// This is helpful to ensure a sanitary `Lexicon` in the engine.
struct ScopedLex<'a> {
    lex: &'a mut Lexicon<'a>,
}

impl<'a> Deref for ScopedLex<'a> {
    type Target = Lexicon<'a>;

    fn deref(&self) -> &Lexicon<'a> {
        self.lex
    }
}

impl<'a> DerefMut for ScopedLex<'a> {
    fn deref_mut(&mut self) -> &mut Lexicon<'a> {
        self.lex
    }
}

impl<'a> Drop for ScopedLex<'a> {
    fn drop(&mut self) {
        self.lex.scopes.pop();
    }
}

/// A `Data` contains all of the instances available.
#[derive(Clone, Debug, Default, PartialEq)]
struct Data {
    instances: Vec<Instance>,
}

impl Data {
    fn iter_instances(&self) -> impl Iterator<Item = &Instance> {
        self.instances.iter()
    }

    fn find_type(&self, ty: &str) -> Option<Instance> {
        use std::iter::once;
        self.iter_instances()
            .flat_map(|ins| match ins {
                Instance::Compound(c) if &*c.ty == "!" => Either::Left(c.env.data.instances.iter()),
                ins => Either::Right(once(ins)),
            }).find(|ins| ins.is_type(ty))
            .cloned()
    }
}

impl From<Vec<Instance>> for Data {
    fn from(instances: Vec<Instance>) -> Data {
        Data { instances }
    }
}

/// Represents `Data` and `Scope`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Environment {
    data: Data,
    scope: Scope,
}

impl Environment {
    /// Append an `Expression` to the `Environment`.
    pub fn run(mut self, e: Expression) -> Self {
        use Expression::*;
        match e {
            Definition(d) => {
                trace!("~run definiton");
                self.scope.def(d)
            }
            Parameter(p) => {
                trace!("~run parameter");
                let res = p.resolve(&Lexicon::new(&self.scope), &self.data);
                self.data.instances.extend(res);
            }
        }
        self
    }

    /// Try to implictly get a type.
    pub fn implicit<S: Into<Rc<str>>>(
        &self,
        ty: S,
    ) -> Option<Instance> {
        let ty = ty.into();
        trace!("~implicit: {:?}", ty);
        Lexicon::new(&self.scope).implicit(&self.data, ty)
    }

    /// Use this environment to try and explicitly resolve a definition for a type.
    fn explicit<S: Into<Rc<str>>>(&self, lex: &Lexicon, ty: S) -> Option<Instance> {
        let lex = lex.with(&self.scope);
        let ty = ty.into();
        trace!("env::explicit {} {{", ty);
        let res = lex
            .iter_definitions()
            .filter(|d| d.is_explicit(&ty))
            .filter_map(|d| {
                d.params
                    .iter()
                    .map(|p| p.resolve(&lex, &self.data))
                    .collect::<Option<Vec<Instance>>>()
                    .map(|instances| {
                        Compound {
                            ty: ty.clone(),
                            env: Environment {
                                data: Data { instances },
                                ..Default::default()
                            },
                        }.into()
                    })
            }).next();
        trace!("}} -> {}", res.is_some());
        res
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let nins = self.data.instances.len();
        if nins != 0 {
            for i in 0..nins - 1 {
                write!(f, "{} ", self.data.instances[i])?;
            }
            write!(f, "{}", self.data.instances[nins - 1])?;
        }
        Ok(())
    }
}

fn is_builtin_implicit(ty: &str) -> bool {
    ty.chars().next().map(|c| c == '@').unwrap_or(false)
}

fn is_builtin_raw(ty: &str) -> bool {
    match ty {
        "+" => true,
        _ => is_builtin_implicit(ty),
    }
}

pub fn ordered_types() -> impl Iterator<Item = Rc<str>> {
    (0..).map(|n| format!("@{}", n).into())
}

pub fn otype(n: usize) -> Rc<str> {
    ordered_types().nth(n).unwrap()
}

pub fn oe<P: Into<Parameter>>(n: usize, param: P) -> Expression {
    Expression::Parameter(
        Explicit {
            target: otype(n),
            args: Arguments {
                exprs: vec![Expression::Parameter(param.into())],
            },
        }.into(),
    )
}

pub fn ops<S: Into<Rc<str>>, I: IntoIterator<Item = Parameter>>(target: S, params: I) -> Parameter {
    e(
        target,
        params
            .into_iter()
            .enumerate()
            .map(|(ix, p)| oe(ix, p))
            .collect(),
    )
}

pub fn oxp<S: Into<Rc<str>>, I: IntoIterator<Item = Parameter>>(
    target: S,
    params: I,
) -> Expression {
    ops(target, params).into()
}

pub fn env() -> Environment {
    Environment::default()
}

pub fn e<S: Into<Rc<str>>>(target: S, exprs: Vec<Expression>) -> Parameter {
    Explicit {
        target: target.into(),
        args: Arguments { exprs },
    }.into()
}

pub fn exp<S: Into<Rc<str>>>(target: S, exprs: Vec<Expression>) -> Expression {
    Expression::Parameter(e(target, exprs))
}

pub fn d<S: Into<Rc<str>>>(ltype: S, params: Vec<Parameter>) -> Expression {
    Definition {
        target: ltype.into(),
        params,
    }.into()
}

pub fn imp<S: Into<Rc<str>>>(ty: S) -> Parameter {
    Parameter::Implicit(ty.into())
}
