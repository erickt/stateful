#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

/*
#[state_machine]
fn yield_() -> usize {
    yield_!(1);
    yield_!(2);
    yield_!(3);
}
*/

#[state_machine]
fn yield_(items: &[usize]) -> usize {
    let mut iter = items.iter();
    match iter.next() {
        Some(y) => {
            yield_!(y);
        }
        None => { }
    };
}

fn main() {
    for value in yield_(&[1, 2, 3]).take(20) {
        println!("yield_: {:?}", value);
    }
}
