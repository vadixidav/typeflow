#[macro_use]
extern crate combine;
extern crate typeflow_engine as tf;

use combine::char::{char, digit, spaces};
use combine::combinator::recognize;
use combine::error::StreamError;
use combine::stream::StreamErrorFor;
use combine::{
    any, between, many1, none_of, optional, sep_by, skip_many, skip_many1, token, value, Stream,
};

const RESERVED_TOKENS: &str = " ,()";

#[cfg_attr(feature = "cargo-clippy", allow(unneeded_field_pattern))]
#[cfg_attr(feature = "cargo-clippy", allow(toplevel_ref_arg))]
parser!{
    pub fn param[I]()(I) -> tf::Parameter
    where [I: Stream<Item = char>]
    {
        let ltoken = many1(none_of(RESERVED_TOKENS.chars()));
        let lchar = |c| char(c).skip(spaces());
        let string = between(token('"'), token('"'), many1(any())).map(|s: String| tf::S(s.into()));
        let float = recognize((
            skip_many1(digit()),
            optional((token('.'), skip_many(digit()))),
        )).and_then(|s: String| {
            s.parse()
                .map_err(|_| StreamErrorFor::<I>::expected_static_message("float"))
        })
            .map(tf::F);
        let int = token('-')
            .with(many1(digit()))
            .and_then(|s: String| {
                s.parse::<i64>()
                    .map_err(|_| StreamErrorFor::<I>::expected_static_message("int"))
            })
            .map(|i| tf::I(-i));
        let uint = many1(digit())
            .and_then(|s: String| {
                s.parse()
                    .map_err(|_| StreamErrorFor::<I>::expected_static_message("unsigned int"))
            })
            .map(tf::U);
        let primitive = uint
            .or(int)
            .or(float)
            .or(string)
            .map(tf::Parameter::Literal);
        primitive.or(ltoken.then(move |t: String| {
            let t2 = t.clone();
            between(token('('), token(')'), sep_by(param(), lchar(',')))
                .map(move |p: Vec<tf::Parameter>| tf::e(&t, p).into())
                .or(value(tf::Parameter::Implicit(t2)))
        }))
    }
}
