#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn gen<'a, T>(items: &'a [T]) -> &'a T {
    let mut iter = items.iter();
    match iter.next() {
        Some(item) => {
            yield_!(item);
        }
        None => { }
    };
}

fn main() {
    let items = &[1, 2, 3];
    for value in gen(items) {
        println!("{}", value);
    }
}
