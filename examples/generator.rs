#![feature(plugin)]
#![cfg_attr(feature = "impl_trait", feature(conservative_impl_trait))]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn gen() -> usize {
    let _x: () = yield_!(1);
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
