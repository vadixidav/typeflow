extern crate combine;
extern crate typeflow_engine as tf;
extern crate typeflow_lang as tfl;

use combine::Parser;
use tf::{env, f};

#[test]
fn add() {
    let expression = tfl::exp()
        .easy_parse("+(@0(2), @1(3))")
        .unwrap_or_else(|e| panic!("{}", e))
        .0;
    let env = env().run(expression);
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}
