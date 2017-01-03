#![feature(plugin)]
#![plugin(stateful)]

fn do_move<T>(_item: T) {}

#[generator]
fn gen(item: String) -> Box<Iterator<Item=String>> { //~ ERROR use of possibly uninitialized variable: `item` [E0381]
    yield_!(moved!(item));
    yield_!(moved!(item));
}

fn main() {
    for value in gen(String::from("wee")) {
        println!("{}", value);
    }
}
