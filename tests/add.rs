extern crate typeflow as tf;

use tf::{c, e, f, i, u};

fn main() {
    let env = e().ins(c("+", vec![i(2), u(5)]));
    assert_eq!(env.implicit("i"), Some(f(5.0)));
}
