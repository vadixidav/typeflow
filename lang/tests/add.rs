extern crate combine;
extern crate typeflow_engine as tf;
extern crate typeflow_lang as tfl;

use combine::Parser;
use tf::{env, f};

fn parse(s: &str) -> tf::Expression {
    tfl::exp()
        .easy_parse(s)
        .unwrap_or_else(|e| panic!("{}", e))
        .0
}

#[test]
fn add() {
    let env = env().run(parse("+(@0(2), @1(3))"));
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}
