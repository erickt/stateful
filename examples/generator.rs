#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_imports)]

use std::cell::RefCell;
use std::rc::Rc;

#[generator]
fn gen(cond: bool) -> Box<Iterator<Item=usize>> {
    match cond {
        true => yield_!(1),
        false => yield_!(2),
    }
}

fn main() {
    for value in gen(false) {
        println!("{}", value);
    }
}
