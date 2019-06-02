use combine::Parser;
use tf::{env, F, S};

fn parse(s: &str) -> tf::Environment {
    tfl::exps()
        .easy_parse(s)
        .unwrap_or_else(|e| panic!("{}", e))
        .0
        .into_iter()
        .fold(env(), |en, ex| {
            println!("{:?}", ex);
            en.run(ex)
        })
}

#[test]
fn add() {
    let env = parse("+(@0(2), @1(3))");
    assert_eq!(env.implicit("f"), Some(F(5.0).into()));
}

#[test]
fn add_infix() {
    let env = parse("2 + 3");
    assert_eq!(env.implicit("f"), Some(F(5.0).into()));
}

#[test]
fn add_newtype() {
    let env = parse("a u, b u, +(@0(a(2)), @1(b(3)))");
    assert_eq!(env.implicit("f"), Some(F(5.0).into()));
}

#[test]
fn add_newtype_prim_upcast() {
    let env = parse("a i, b u, +(@0(a(2)), @1(b(3)))");
    assert_eq!(env.implicit("f"), Some(F(5.0).into()));
}

#[test]
fn add_strings() {
    let env = parse("+(@0(\"hello\"), @1(\" world\"))");
    assert_eq!(
        env.implicit("s"),
        Some(S("hello world".into()).into())
    );
}
