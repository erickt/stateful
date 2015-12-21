#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

fn increment(x: usize) -> usize { x + 1 }

#[state_machine]
fn yield_(b: bool, a: usize, b: usize) -> usize {
    loop {
        let x = 1;
        return a;
    };
}

    /*
    if b {
        return a;
    } else {
        return b;
    };
    */

fn main() {
    for value in yield_(1).take(20) {
        println!("yield_: {:?}", value);
    }
}
