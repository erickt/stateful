#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[generator]
fn yield_() -> usize {
    let a = 0;

    {
        let b = 1;
        yield_!(b);
    };

    let c = 2;
    yield_!(c);


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
    for value in yield_().take(20) {
        println!("yield_: {:?}", value);
    }
}
