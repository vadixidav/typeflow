#[macro_use]
extern crate slog;
extern crate typeflow_engine as tf;

mod logger;
use logger::*;

use tf::{d, e, env, exp, i, imp, oe, Environment, I};

fn bank(log: &Logger, env: Environment) -> Environment {
    env.run(log.clone(), d("m", vec![imp("i")]))
        .run(log.clone(), d("bank", vec![imp("m")]))
        .run(log.clone(), d("deposit_amount", vec![imp("m")]))
        .run(
            log.clone(),
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
    let log = logger();

    // a i, b u, +(a(2),b(3))
    let env = bank(&log, env()).run(
        log.clone(),
        e(
            "deposit",
            vec![
                exp("bank", vec![exp("m", vec![I(3).into()])]),
                exp("deposit_amount", vec![exp("m", vec![I(4).into()])]),
            ],
        ).into(),
    );

    assert_eq!(env.implicit(log.clone(), "i"), Some(i(7)));
}
