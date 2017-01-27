#![feature(plugin)]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::iter::Iterator;
use std::marker::PhantomData;
use std::mem;

struct Empty<T>(PhantomData<T>);

impl<T> Empty<T> {
    fn new() -> Self {
        Empty(PhantomData)
    }
}

impl<T> IntoIterator for Empty<T> {
    type Item = T;
    type IntoIter = EmptyIterator<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        EmptyIterator::new()
    }
}

struct EmptyIterator<T>(PhantomData<T>);

impl<T> EmptyIterator<T> {
    fn new() -> Self {
        EmptyIterator(PhantomData)
    }
}

impl<T> Iterator for EmptyIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

#[generator]
fn gen() -> Box<Iterator<Item=String>> {
        let x = format!("a");
        {
            //yield_!(x.clone());
            let x = format!("b");
            {
                yield_!(x.clone());
                let x = format!("c");
                yield_!(moved!(x));
            }
            //yield_!(moved!(x));
        }
        //yield_!(moved!(x));

        /*
    let x = format!("a");
    {
        yield_!(x.clone());
        if true {
            let x = format!("b");
            yield_!(x.clone());
        }
    }
    //yield_!(x.clone());
    */

    /*
    let a = Empty::<usize>::new();

    let mut b = Empty::<usize>::new();
    let c;
    let d;

    if pred {
        mem::drop(moved!(a));
        b = Empty::<usize>::new();
    } else {
        mem::drop(moved!(b));
        d = Empty::<usize>::new();
    }

    c = Empty::<usize>::new();
    */
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
