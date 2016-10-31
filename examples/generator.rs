#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_imports)]

use std::cell::RefCell;
use std::rc::Rc;

struct OnDrop {
    counter: Rc<RefCell<usize>>,
}

impl Drop for OnDrop {
    fn drop(&mut self) {
        let mut counter = self.counter.borrow_mut();
        *counter += 1;
    }
}

#[generator]
fn gen() -> Box<Iterator<Item=usize>> {
    let counter1 = Rc::new(RefCell::new(0));
    let counter2 = counter1.clone();

    let x = OnDrop { counter: moved!(counter1) };
    yield_!(1);
    let x = OnDrop { counter: moved!(counter2) };
    yield_!(1);
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
