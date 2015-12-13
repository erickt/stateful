#![feature(plugin)]
#![plugin(stateful)]

#[state_machine]
fn yield_() -> usize {
    let x = 0;
    return x;

    let y = 1;
    return x + y;

    return x;
}

fn main() {
    for value in yield_() {
        println!("yield_: {:?}", value);
    }
}
