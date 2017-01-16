#![feature(plugin)]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::iter::Iterator;
use std::marker::PhantomData;
use std::mem;

struct Empty<T>(PhantomData<T>);

impl<T> Empty<T> {
    fn new() -> Self {
        Empty(PhantomData)
    }
}

impl<T> IntoIterator for Empty<T> {
    type Item = T;
    type IntoIter = EmptyIterator<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        EmptyIterator::new()
    }
}

struct EmptyIterator<T>(PhantomData<T>);

impl<T> EmptyIterator<T> {
    fn new() -> Self {
        EmptyIterator(PhantomData)
    }
}

impl<T> Iterator for EmptyIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

#[generator]
fn gen(pred: bool) -> Box<Iterator<Item = usize>> {
    /*
    let a = Empty::new();
    let mut b = Empty::new();
    let c;
    let d;

    if pred {
        mem::drop(moved!(a));
        b = Empty::new();
    } else {
        mem::drop(moved!(b));
        d = Empty::new();
    }

    c = Empty::new();
    */

    /*
    let x = 1;
    if true {
        mem::drop(moved!(x));
    } else {
    }
    let x = 1;
    if true {}
    println!("{:?}", x);
    yield_!(1);
    */
}

fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
