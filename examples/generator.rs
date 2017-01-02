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
fn gen(pred: bool) -> Box<Iterator<Item = usize>> {
    let a;
    if true {
        a = 1;
    } else {
        a = 2;
    }

    yield_!(a);
}

fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
