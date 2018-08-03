#[macro_use]
extern crate slog;
extern crate typeflow_engine as tf;

mod logger;
use logger::*;

use tf::{d, e, env, exp, f, imp, oe, I, U};

#[test]
fn add_upcasting() {
    let log = logger();
    // +(2,3)
    let env = env().run(log.clone(), exp("+", vec![oe(0, I(2)), oe(1, U(3))]));
    assert_eq!(env.implicit(log.clone(), "f"), Some(f(5.0)));
}

#[test]
fn add_newtype() {
    let log = logger();
    // a i, b u, +(a(2),b(3))
    let env = env()
        .run(log.clone(), d("a", vec![imp("i")]))
        .run(log.clone(), d("b", vec![imp("u")]))
        .run(
            log.clone(),
            e(
                "+",
                vec![
                    oe(0, e("a", vec![I(2).into()])),
                    oe(1, e("b", vec![U(3).into()])),
                ],
            ).into(),
        );

    assert_eq!(env.implicit(log.clone(), "f"), Some(f(5.0)));
}

#[test]
fn add_newtype_nodef() {
    let log = logger();
    // a i, +(a(2),b(3))
    let env = env().run(log.clone(), d("a", vec![imp("i")])).run(
        log.clone(),
        e(
            "+",
            vec![
                oe(0, e("a", vec![I(2).into()])),
                oe(1, e("b", vec![U(3).into()])),
            ],
        ).into(),
    );

    assert_eq!(env.implicit(log.clone(), "f"), None);
}
