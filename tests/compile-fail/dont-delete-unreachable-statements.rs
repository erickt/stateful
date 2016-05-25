#![feature(plugin)]
#![plugin(stateful)]

#[generator]
fn gen<'a, T>(items: &'a [T]) -> &'a T {
    let mut iter = items.iter();
    loop {
        match iter.next() {
            Some(item) => {
                yield_!(item);
            }
            None => {
                break;
                let x = y; //~ ERROR error: unresolved name `y`
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
