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

const RESERVED_TOKENS: &str = " ,()";

fn exp_<I>() -> impl Parser<Input = I, Output = tf::Expression>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let ltoken = || many1(none_of(RESERVED_TOKENS.chars())).skip(spaces());
    let lchar = |c| char(c).skip(spaces());
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
        })
            .map(tf::F)
    };
    let int = || {
        token('-')
            .with(many1(digit()))
            .and_then(|s: String| {
                s.parse::<i64>()
                    .map_err(|_| StreamErrorFor::<I>::expected_static_message("int"))
            })
            .map(|i| tf::I(-i))
    };
    let uint = || {
        many1(digit())
            .and_then(|s: String| {
                s.parse()
                    .map_err(|_| StreamErrorFor::<I>::expected_static_message("unsigned int"))
            })
            .map(tf::U)
    };
    let primitive = move || {
        uint()
            .or(int())
            .or(float())
            .or(string())
            .map(tf::Parameter::Literal)
    };
    let param = move || {
        primitive().or(ltoken().then(move |t: String| {
            let t2 = t.clone();
            between(lchar('('), lchar(')'), sep_by(exp(), lchar(',')))
                .map(move |e: Vec<tf::Expression>| tf::e(&t, e))
                .or(value(tf::Parameter::Implicit(t2)))
        }))
    };
    primitive()
        .map(|p| p.into())
        .or(ltoken().then(move |t: String| {
            let t2 = t.clone();
            let t3 = t.clone();
            between(lchar('('), lchar(')'), exps())
                .map(move |e: Vec<tf::Expression>| tf::exp(&t, e))
                .or(many1(param()).map(move |p: Vec<tf::Parameter>| tf::d(&t2, p)))
                .or(value(tf::Parameter::Implicit(t3).into()))
        }))
}

parser!{
    pub fn exp[I]()(I) -> tf::Expression
    where [I: Stream<Item = char>]
    {
        exp_()
    }
}

parser!{
    pub fn exps[I]()(I) -> Vec<tf::Expression>
    where [I: Stream<Item = char>]
    {
        let lchar = |c| char(c).skip(spaces());
        sep_by(exp(), lchar(','))
    }
}
