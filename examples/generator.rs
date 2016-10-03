#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

use std::cell::RefCell;
use std::rc::Rc;

#[generator]
fn gen(cond: bool) -> Box<Iterator<Item=usize>> {
    if cond == true {
        yield_!(2)
    } else {
        yield_!(3)
    }
}

fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
