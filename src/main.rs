#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[derive(Copy, Clone)]
enum Either {
    Left(usize),
    Right(usize),
}

#[generator]
fn yield_(either: Either) -> usize {
    match either {
        Either::Left(x) => {
            yield_!(x);
        }
        Either::Right(y) => {
            yield_!(y);
        }
    };
}

fn main() {
    for value in yield_(Either::Left(0)).take(20) {
        println!("yield_: {:?}", value);
    }
}
