#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn gen<T: 'static>(items: Vec<T>) -> T {
    for item in moved!(items).into_iter() {
        yield_!(moved!(item));
    }
}

fn main() {
    let items = vec![1, 2, 3];
    for value in gen(items) {
        println!("{}", value);
    }
}
