extern crate typeflow as tf;

use tf::{c, e, env, f, i, oexp, oins, u, I, U};

#[test]
fn add_upcasting() {
    let env = env(None).ins(c("+", vec![oins(0, i(2)), oins(1, u(3))]));
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_resolve() {
    assert_eq!(
        e("+", vec![oexp(0, I(2).into()), oexp(1, U(3).into())])
            .resolve(&env(None))
            .and_then(|ins| env(None).ins(ins).implicit("f")),
        Some(f(5.0))
    )
}
