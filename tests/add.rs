extern crate typeflow as tf;

use std::rc::Rc;
use tf::{Compound, Environment, Instance, Primitive};

fn main() {
    let mut env = tf::Environment::default();
    let a = Instance::Primitive(Primitive::U(2));
    let b = Instance::Primitive(Primitive::U(3));
    env.instances.push(Instance::Compound(Rc::new(Compound {
        ty: "+".to_owned(),
        contained: vec![a, b],
    })));
    assert_eq!(
        env.implicit("i"),
        Some(Instance::Primitive(Primitive::I(5)))
    );
}
