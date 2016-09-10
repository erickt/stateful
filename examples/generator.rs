#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn gen() -> Box<Iterator<Item=usize>> {
    let _x: () = yield_!(1);
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
