#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

extern crate stateful;

#[state_machine]
fn yield_() -> usize {
    let x = 1;
    ::stateful::yield_(1, x);
    return x;
}

/*
fn foo() {
    let mut iter = items.iter();
    loop {
        match iter.next() {
            Some(item) => { return item; }
            None => { break; }
        };
    };
}
*/

fn main() {
    for value in yield_().take(20) {
        println!("yield_: {:?}", value);
    }
}
