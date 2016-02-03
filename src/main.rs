#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn yield_<'a, T>(items: &'a [T]) -> &'a T {
    let mut iter = items.iter();
    loop {
        match iter.next() {
            Some(item) => {
                yield_!(item);
            }
            None => {
                break;
            }
        };
    };
}

fn main() {
    let items = &[1, 2, 3];
    for value in yield_(items).take(20) {
        println!("yield_: {:?}", value);
    }
}
