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
fn gen() -> Box<Iterator<Item = usize>> {
    let a = 1;
    {
        let b = 2;
    }
    let c = 3;
    {
        let d = 4;
    }
}



fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
