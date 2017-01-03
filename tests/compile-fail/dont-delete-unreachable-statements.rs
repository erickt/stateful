#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_mut)]

#[generator]
fn gen<'a, T>(items: &'a [T]) -> Box<Iterator<Item=&'a T> + 'a> {
    let mut iter = items.iter();
    loop {
        match iter.next() {
            Some(item) => {
                yield_!(item);
            }
            None => {
                break;
                let x = y; //~ ERROR unresolved value `y` [E0425]
            }
        };
    };
}

fn main() {
    let items = &[1, 2, 3];
    for value in gen(items) {
        println!("{}", value);
    }
}
