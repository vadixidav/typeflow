#[macro_use]
extern crate combine;
extern crate typeflow_engine as tf;

use combine::char::{char, digit, spaces};
use combine::combinator::recognize;
use combine::error::StreamError;
use combine::stream::StreamErrorFor;
use combine::{
    between, many1, none_of, optional, sep_by, skip_many, skip_many1, token, value, ParseError,
    Parser, Stream,
};

use std::rc::Rc;

const RESERVED_TOKENS: &str = " ,()";

fn param<I>() -> impl Parser<Input = I, Output = tf::Parameter>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    primitive()
        .map(tf::Parameter::Literal)
        .or(ltoken().then(move |t: String| {
            let t: Rc<str> = t.into();
            explicit(t.clone()).or(value(tf::Parameter::Implicit(t)))
        }))
}

fn exp<I>() -> impl Parser<Input = I, Output = tf::Expression>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    combine::try(add())
        .map(tf::Expression::Parameter)
        .or(primitive().map(|p| p.into()))
        .or(ltoken().then(move |t: String| {
            let t: Rc<str> = t.into();
            explicit(t.clone())
                .map(tf::Expression::Parameter)
                .or(many1(param()).map({
                    let t = t.clone();
                    move |p: Vec<tf::Parameter>| tf::d(t.clone(), p)
                })).or(value(tf::Parameter::Implicit(t).into()))
        }))
}

fn primitive<I>() -> impl Parser<Input = I, Output = tf::Primitive>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let string = || {
        between(token('"'), token('"'), many1(none_of(Some('"')))).map(|s: String| tf::S(s.into()))
    };
    let float = || {
        recognize((
            skip_many1(digit()),
            optional((token('.'), skip_many(digit()))),
        )).and_then(|s: String| {
            s.parse()
                .map_err(|_| StreamErrorFor::<I>::expected_static_message("float"))
        }).map(tf::F)
    };
    let int = || {
        token('-')
            .with(many1(digit()))
            .and_then(|s: String| {
                s.parse::<i64>()
                    .map_err(|_| StreamErrorFor::<I>::expected_static_message("int"))
            }).map(|i| tf::I(-i))
    };
    let uint = || {
        many1(digit())
            .and_then(|s: String| {
                s.parse()
                    .map_err(|_| StreamErrorFor::<I>::expected_static_message("unsigned int"))
            }).map(tf::U)
    };
    uint().or(int()).or(float()).or(string())
}

fn lchar<I>(c: char) -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    char(c).skip(spaces())
}

fn ltoken<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(none_of(RESERVED_TOKENS.chars())).skip(spaces())
}

fn explicit<I, S: Into<Rc<str>>>(target: S) -> impl Parser<Input = I, Output = tf::Parameter>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let target = target.into();
    between(lchar('('), lchar(')'), exps())
        .map(move |e: Vec<tf::Expression>| tf::e(target.clone(), e))
}

fn add<I>() -> impl Parser<Input = I, Output = tf::Parameter>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    sep_by(param_(), lchar('+')).and_then(|params: Vec<tf::Parameter>| {
        if params.len() >= 2 {
            Ok(tf::ops("+", params))
        } else {
            Err(StreamErrorFor::<I>::expected_static_message(
                "add expression",
            ))
        }
    })
}

pub fn exps<I>() -> impl Parser<Input = I, Output = Vec<tf::Expression>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let lchar = |c| char(c).skip(spaces());
    sep_by(exp_(), lchar(','))
}

parser!{
    fn exp_[I]()(I) -> tf::Expression
    where [I: Stream<Item = char>]
    {
        exp()
    }
}

parser!{
    fn param_[I]()(I) -> tf::Parameter
    where [I: Stream<Item = char>]
    {
        param()
    }
}
