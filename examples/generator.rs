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

struct S;

#[generator]
fn gen(pred: bool) -> Box<Iterator<Item=usize>> {
    let a = 1; let mut b = 2; let c; let d;

    if pred {
        mem::drop(a);
        b = 3;
    } else {
        mem::drop(b);
        d = 4;
    }

    c = 5;
}

fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
