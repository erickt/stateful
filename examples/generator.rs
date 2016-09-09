#![feature(plugin)]
#![cfg_attr(feature = "impl_trait", feature(conservative_impl_trait))]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn gen<T: 'static>(items: Vec<T>) -> T {
    for item in moved!(items) {
        yield_!(moved!(item));
    }
}

fn main() {
    for value in gen(vec![1, 2, 3]) {
        println!("{}", value);
    }
}
