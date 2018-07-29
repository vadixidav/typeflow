extern crate typeflow_engine as tf;

use tf::{d, e, env, exp, f, oe, Parameter::Implicit, I, U};

#[test]
fn add_upcasting() {
    // +(2,3)
    let env = env().run(exp("+", vec![oe(0, I(2)), oe(1, U(3))]));
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_newtype() {
    // a i, b u, +(a(2),b(3))
    let env = env()
        .run(d("a", vec![Implicit("i".into())]))
        .run(d("b", vec![Implicit("u".into())]))
        .run(
            e(
                "+",
                vec![
                    oe(0, e("a", vec![I(2).into()])),
                    oe(1, e("b", vec![U(3).into()])),
                ],
            ).into(),
        );

    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_newtype_nodef() {
    // a i, +(a(2),b(3))
    let env = env().run(d("a", vec![Implicit("i".into())])).run(
        e(
            "+",
            vec![
                oe(0, e("a", vec![I(2).into()])),
                oe(1, e("b", vec![U(3).into()])),
            ],
        ).into(),
    );

    // FIXME: This currently gives back `2.0` but it should give back nothing.
    // It should give back nothing because `@1` exists but can't be converted to a primitive.
    // It currently stops at `@1` and returns the result, but it should fail the operation instead.
    assert_eq!(env.implicit("f"), None);
}
