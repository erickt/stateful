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
fn gen() -> Box<Iterator<Item = usize>> {
    let items: Vec<usize> = vec![1, 2, 3];
    let mut iter = moved!(items).into_iter();

    while let Some(item) = iter.next() {
        println!("hello {:?}", item);
    }

    loop {
        break;
    };

    println!("esfs");
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
