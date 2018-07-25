extern crate typeflow as tf;

use tf::{c, e, f, i, u};

#[test]
fn add() {
    let env = e().ins(c("+", vec![c("@0", vec![i(2)]), c("@1", vec![u(3)])]));
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}
