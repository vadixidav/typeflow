extern crate typeflow as tf;

use std::rc::Rc;
use tf::{i, u, Compound, Environment, Instance, Primitive};

fn main() {
    let env = Environment {
        instances: vec![Instance::Compound(Rc::new(Compound {
            ty: "+".to_owned(),
            contained: vec![u(2), u(3)],
        }))],
        ..Default::default()
    };
    assert_eq!(env.implicit("i"), Some(i(5)));
}
