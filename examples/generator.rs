#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_imports)]

use std::cell::RefCell;
use std::rc::Rc;

#[generator]
fn gen() -> Box<Iterator<Item=usize>> {
    let x = 1;
    1;
    yield_!(x);
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
