mod logger;

use tf::{d, e, env, exp, imp, oe, Environment, I};

fn bank(env: Environment) -> Environment {
    logger::log();
    env.run(d("m", vec![imp("i")]))
        .run(d("bank", vec![imp("m")]))
        .run(d("deposit_amount", vec![imp("m")]))
        .run(
            d(
                "deposit",
                vec![e(
                    "bank",
                    vec![exp(
                        "m",
                        vec![exp(
                            "+",
                            vec![oe(0, imp("bank")), oe(1, imp("deposit_amount"))],
                        )],
                    )],
                )],
            ),
        )
}

#[test]
fn deposit() {
    logger::log();

    // a i, b u, +(a(2),b(3))
    let env = bank(env()).run(
        e(
            "deposit",
            vec![
                exp("bank", vec![exp("m", vec![I(3).into()])]),
                exp("deposit_amount", vec![exp("m", vec![I(4).into()])]),
            ],
        ).into(),
    );

    assert_eq!(env.implicit("i"), Some(I(7).into()));
}
