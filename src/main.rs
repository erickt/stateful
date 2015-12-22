#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

fn increment(x: usize) -> usize { x + 1 }

#[state_machine]
fn yield_(mut i: usize, then: usize, else_: usize) -> usize {
    loop {
        if i < 5 {
            i += 1;
        } else {
            break;
        };
    };

    loop {
        if i < 10 {
            break;
        } else {
            i += 1;
            return else_;
        };
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
