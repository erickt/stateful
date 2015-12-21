#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

fn increment(x: usize) -> usize { x + 1 }

#[state_machine]
fn yield_(b: bool, then: usize, else_: usize) -> usize {
    if b {
        let x = then + 1;
        return then;
        return x;
    } else {
        return else_;
    };
}

fn main() {
    for value in yield_(true, 0, 1).take(20) {
        println!("yield_: {:?}", value);
    }

    println!("");

    for value in yield_(false, 0, 1).take(20) {
        println!("yield_: {:?}", value);
    }
}
