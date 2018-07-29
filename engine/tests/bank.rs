extern crate typeflow_engine as tf;

use tf::{d, e, env, exp, i, imp, oe, Environment, I};

fn bank(env: Environment) -> Environment {
    env.run(d("m", vec![imp("i")]))
        .run(d("bank", vec![imp("m")]))
        .run(d("deposit_amount", vec![imp("m")]))
        .run(d(
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
        ))
}

#[test]
fn deposit() {
    // a i, b u, +(a(2),b(3))
    let env = bank(env()).run(
        e(
            "deposit",
            vec![
                exp("bank", vec![exp("money", vec![I(3).into()])]),
                exp("deposit_money", vec![exp("money", vec![I(4).into()])]),
            ],
        ).into(),
    );

    assert_eq!(env.implicit("i"), Some(i(7)));
}
