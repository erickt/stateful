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
    yield_!(1);
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
