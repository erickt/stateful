#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

#[state_machine]
fn yield_() -> usize {
    let a = 0;

    {
        let a = 1;
    };

    a + 1;
}

fn main() {
    for value in yield_(&[1, 2, 3]).take(20) {
        println!("yield_: {:?}", value);
    }
}
