mod logger;

use tf::{d, e, env, i, imp, oxp, u, F};

#[test]
fn add_upcasting() {
    logger::log();
    // +(2,3)
    let env = env().run(oxp("+", vec![i(2), u(3)]));
    assert_eq!(env.implicit("f"), Some(F(5.0).into()));
}

#[test]
fn add_newtype() {
    logger::log();
    // a i, b u, +(a(2),b(3))
    let env = env()
        .run(d("a", vec![imp("i")]))
        .run(d("b", vec![imp("u")]))
        .run(
            oxp(
                "+",
                vec![e("a", vec![i(2).into()]), e("b", vec![u(3).into()])],
            ),
        );

    assert_eq!(env.implicit("f"), Some(F(5.0).into()));
}

#[test]
fn add_newtype_nodef() {
    logger::log();
    // a i, +(a(2),b(3))
    let env = env().run(d("a", vec![imp("i")])).run(
        oxp(
            "+",
            vec![e("a", vec![i(2).into()]), e("b", vec![u(3).into()])],
        ),
    );

    assert_eq!(env.implicit("f"), None);
}
