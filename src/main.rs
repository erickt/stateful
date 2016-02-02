#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn yield_(a: usize) -> usize {
    let a = 0;
    yield_!(a);

    {
        let a = 1;
        yield_!(a);
    };

    yield_!(a);

    /*
    let a = 2;
    let a = 3;
    yield_!(a);
    */


    /*
    if true {
        yield_!(1);
    } else if false {
        yield_!(2);
    } else {
        yield_!(3);
    };

    a + 1;
    */
}

fn main() {
    for value in yield_(0).take(20) {
        println!("yield_: {:?}", value);
    }
}
