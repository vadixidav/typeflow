#[macro_use]
extern crate slog;
extern crate typeflow_engine as tf;

mod logger;
use logger::*;

use tf::{d, e, env, i, imp, oxp, u, F};

#[test]
fn add_upcasting() {
    let log = logger();
    // +(2,3)
    let env = env().run(log.clone(), oxp("+", vec![i(2), u(3)]));
    assert_eq!(env.implicit(log.clone(), "f"), Some(F(5.0).into()));
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
            oxp(
                "+",
                vec![e("a", vec![i(2).into()]), e("b", vec![u(3).into()])],
            ),
        );

    assert_eq!(env.implicit(log.clone(), "f"), Some(F(5.0).into()));
}

#[test]
fn add_newtype_nodef() {
    let log = logger();
    // a i, +(a(2),b(3))
    let env = env().run(log.clone(), d("a", vec![imp("i")])).run(
        log.clone(),
        oxp(
            "+",
            vec![e("a", vec![i(2).into()]), e("b", vec![u(3).into()])],
        ),
    );

    assert_eq!(env.implicit(log.clone(), "f"), None);
}
