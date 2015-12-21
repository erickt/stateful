#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

fn increment(x: usize) -> usize { x + 1 }

#[state_machine]
fn yield_(a: usize) -> usize {
    let x = 1;
    loop {
        return a;
    };
    let y = 2;
    return y;
}

fn main() {
    for value in yield_(1).take(20) {
        println!("yield_: {:?}", value);
    }
}
