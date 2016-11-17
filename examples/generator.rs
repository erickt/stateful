#![feature(plugin)]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::iter::Iterator;

#[generator]
fn gen() -> Box<Iterator<Item=usize>> {
    for item in 1..5 {
        yield_!(item);
    }
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
