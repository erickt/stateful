#![feature(plugin)]
#![plugin(stateful)]

#[state_machine]
fn yield_() -> usize {
    let x = 0;
    return 0;

    let y = 1;
    return 1;

    let z = 2;
    return 2;
}

fn main() {
    for value in yield_3() {
        println!("yield_3: {:?}", value);
    }
}
