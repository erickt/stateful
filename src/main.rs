#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

#[state_machine]
fn yield_(items: &[usize]) -> usize {
    let mut iter = items.iter();
    loop {
        match iter.next() {
            Some(item) => { return item; }
            None => { break; }
        };
    };
}

fn main() {
    for value in yield_(&[1, 2, 3, 4, 5]).take(20) {
        println!("yield_: {:?}", value);
    }
}
