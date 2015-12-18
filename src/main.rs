#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

fn increment(x: usize) -> usize { x + 1 }

#[state_machine]
fn yield_(a: usize) -> usize {
    let x = increment(a);

    {
        let y = increment(x);
        return x + y;

        let z = increment(y);
        return x + y + z;
    };

    return x;
}

fn main() {
    for value in yield_(1) {
        println!("yield_: {:?}", value);
    }
}
