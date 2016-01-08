#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]

/*
#[state_machine]
fn yield_(items: &[usize]) -> usize {
    let x = 1;
    let y = 2;
    let z = 3;
    loop {
    yield_!(1 + x);
    yield_!(2 + y);
    yield_!(3 + z);
    };
}
*/

/*
#[state_machine]
fn yield_() -> usize {
    loop {
        let x = 1;
        return x;

        let y = 2;
        return x + y;

        let z = 3;
        return x + y + z;

        break;
    };

    return 3;
}
*/

#[state_machine]
fn yield_(items: &[usize]) -> usize {
    let mut iter = items.iter();
    match iter.next() {
        Some(item) => {
            yield_!(*item, iter);
        }
        None => { }
    };
}

fn main() {
    for value in yield_(&[1, 2, 3]).take(20) {
        println!("yield_: {:?}", value);
    }
}
