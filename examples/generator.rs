#![feature(plugin)]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::iter::Iterator;
use std::mem;

#[generator]
fn gen(pred: bool) -> Box<Iterator<Item = usize>> {
    yield_!(1);
}

fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
