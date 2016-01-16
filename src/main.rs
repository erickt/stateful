#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

#[state_machine]
fn yield_() -> usize {
    let x = 1;
    {
        2;
    };
    3;
}

fn main() {
    for value in yield_(&[1, 2, 3]).take(20) {
        println!("yield_: {:?}", value);
    }
}
