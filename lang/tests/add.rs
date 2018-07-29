extern crate combine;
extern crate typeflow_engine as tf;
extern crate typeflow_lang as tfl;

use combine::Parser;
use tf::{env, f};

#[test]
fn add() {
    let param = tfl::param()
        .easy_parse("+(@0 2, @1 3)")
        .unwrap_or_else(|e| panic!("{}", e))
        .0;
    let mut env = env(None);
    env.run(&param);
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}
