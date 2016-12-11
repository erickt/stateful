#![feature(plugin)]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::iter::Iterator;

fn foo() -> usize { 5 }

#[generator]
fn gen(a: usize) -> Box<Iterator<Item=usize>> {
    let while_result = while false {
        let b = 1;
    };

    let c = 2;
    /*
    let result = if a == 0 {
        let b = 2;
        b
    } else {
        let c = 3;
        c
    };
    let d = 4;
    */
}

fn main() {
    for value in gen(5) {
        println!("{}", value);
    }
}
