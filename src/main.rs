#![feature(plugin)]
#![plugin(stateful)]

#[state_machine]
fn yield_() -> usize {
    let x = 0;
    return x;

    let y = 1;
    return x + y;

    let z = 2;
    return x + y + z;
}

fn main() {
    for value in yield_() {
        println!("yield_: {:?}", value);
    }
}
