#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_imports)]

use std::slice;
use std::iter::Iterator;

/*
#[generator]
fn gen<'a, T>(items: &'a [T]) -> Box<Iterator<Item=&'a T> + 'a> {
    let mut iter = moved!(items).into_iter();
    let item = Iterator::next(&mut iter);
    loop {
        break;
    }
}

fn main() {
    let items = &[1, 2, 3];
    for value in gen(items) {
        println!("{}", value);
    }
}
*/

#[generator]
fn gen() -> Box<Iterator<Item=usize>> {
    if true { } else { }
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
