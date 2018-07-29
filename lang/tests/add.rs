extern crate combine;
extern crate typeflow_engine as tf;
extern crate typeflow_lang as tfl;

use combine::Parser;
use tf::{env, f};

fn parse(s: &str) -> tf::Environment {
    tfl::exps()
        .easy_parse(s)
        .unwrap_or_else(|e| panic!("{}", e))
        .0
        .into_iter()
        .fold(env(), |en, ex| en.run(ex))
}

#[test]
fn add() {
    let env = parse("+(@0(2), @1(3))");
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_newtype() {
    let env = parse("a u, b u, +(@0(a(2)), @1(b(3)))");
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_newtype_prim_upcast() {
    let env = parse("a i, b u, +(@0(a(2)), @1(b(3)))");
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}
