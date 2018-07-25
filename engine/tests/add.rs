extern crate typeflow as tf;

use tf::{c, d, e, env, f, i, oexp, oins, u, Parameter::Implicit, I, U};

#[test]
fn add_upcasting() {
    let env = env(None).ins(c("+", vec![oins(0, i(2)), oins(1, u(3))]));
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_resolve() {
    let mut env = env(None);
    // +(2,3)
    env.run(&e("+", vec![oexp(0, I(2).into()), oexp(1, U(3).into())]));
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_newtype() {
    let env = env(None)
        .def(d("a", vec![Implicit("i".into())]))
        .def(d("b", vec![Implicit("u".into())]))
        .ins(c(
            "+",
            vec![oins(0, c("a", vec![i(2)])), oins(1, c("b", vec![u(3)]))],
        ));
    assert_eq!(env.implicit("f"), Some(f(5.0)));
}

#[test]
fn add_newtype_resolve() {
    let mut env = env(None)
        .def(d("a", vec![Implicit("i".into())]))
        .def(d("b", vec![Implicit("u".into())]));

    // +(a(2),b(3))
    env.run(&e(
        "+",
        vec![
            oexp(0, e("a", vec![I(2).into()]).into()),
            oexp(1, e("b", vec![U(3).into()]).into()),
        ],
    ));

    assert_eq!(env.implicit("f"), Some(f(5.0)));
}
