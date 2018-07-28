extern crate combine;
extern crate typeflow_engine as tf;

use tf::{c, d, e, env, f, i, oexp, oins, u, Parameter::Implicit, I, U};

#[test]
fn add_upcasting() -> combine::ParseResult<(), &str> {
    let env = env_parse("+(@0 2, @1 3)")?;
    assert_eq!(env.implicit("f"), Some(f(5.0)));
    Ok(())
}
