#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

#[state_machine]
fn yield_(mut i: usize, then: usize, else_: usize) -> usize {
    loop {
        if i < 10 {
            i += 1;
            return i;
        } else {
            break;
        };
    };
}

fn main() {
    for value in yield_(0, 0, 1).take(20) {
        println!("yield_: {:?}", value);
    }
}
