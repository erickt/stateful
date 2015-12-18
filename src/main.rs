#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

fn a() -> usize { 0 }
fn b(_: usize) -> usize { 1 }
fn c(_: usize) -> usize { 2 }

#[state_machine]
fn yield_() -> usize {
    let x = a();
    {
        let y = b(x);
        return x + y;

        let z = c(y);
        return x + y + z;
    };
}

fn main() {
    for value in yield_() {
        println!("yield_: {:?}", value);
    }
}
