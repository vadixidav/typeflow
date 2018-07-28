#[macro_use]
extern crate combine;
extern crate typeflow_engine as tf;

use combine::char::{alpha_num, char, digit, letter, space, spaces};
use combine::combinator::recognize;
use combine::{
    any, between, many1, none_of, optional, sep_by, skip_many, skip_many1, token, value,
    ParseError, Parser, Stream,
};

use std::rc::Rc;

const reserved_tokens: &str = " ,()";

fn param<I>() -> impl Parser<Input = I, Output = tf::Parameter>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let ltoken = many1(none_of(reserved_tokens.chars()));
    let lchar = |c| char(c).skip(spaces());
    let params = between(token('('), token(')'), sep_by(param(), lchar(',')));
    let string = between(token('"'), token('"'), any()).map(|s: String| tf::S(s.into()));
    let float = recognize((
        skip_many1(digit()),
        optional((token('.'), skip_many(digit()))),
    )).and_then(|s: String| s.parse())
        .map(tf::F);
    let int = token('-')
        .with(many1(digit()))
        .and_then(|s: String| s.parse::<i64>())
        .map(|i| tf::I(-i));
    let uint = many1(digit()).and_then(|s: String| s.parse()).map(tf::U);
    let primitive = uint
        .or(int)
        .or(float)
        .or(string)
        .map(tf::Parameter::Literal);
    primitive.or(ltoken.then(|t: String| {
        params
            .map(|p: Vec<tf::Parameter>| tf::e(&t, p))
            .or(value(tf::Parameter::Implicit(t)))
    }))
}
