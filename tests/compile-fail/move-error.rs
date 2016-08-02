#![feature(plugin)]
#![plugin(stateful)]

fn do_move<T>(_item: T) {}

#[generator]
fn gen(item: String) -> String {
    yield_!(moved!(item));
    yield_!(moved!(item)); //~ ERROR capture of moved value: `item`
                           //~^ ERROR cannot move out of captured outer variable in an `Fn` closure
}

fn main() {
    for value in gen(String::from("wee")) {
        println!("{}", value);
    }
}
