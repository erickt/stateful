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

struct S(u32);

fn foo() {}

#[generator]
fn gen(pred: bool) -> Box<Iterator<Item=usize>> {
    let a = S(1); let mut b = S(2); let c; let d;

    if pred {
        //mem::drop(a);
        b = S(3);
        c = S(40);
    } else {
        //mem::drop(b);
        b = S(30);
        c = S(4);
    }

    d = S(5);

    foo();
}

fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
