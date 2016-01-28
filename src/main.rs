#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

#[generator]
fn yield_() -> usize {
    let mut a = 0;

    if true {
        yield_!(1);
        let a = 1;
    };

    a + 1;
}

fn main() {
    for value in yield_(&[1, 2, 3]).take(20) {
        println!("yield_: {:?}", value);
    }
}
