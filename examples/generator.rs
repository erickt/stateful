#![feature(plugin)]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::slice;
use std::iter::Iterator;

struct Thing;

/*
#[generator]
fn gen<'a, T>(items: &'a [T]) -> Box<Iterator<Item=&'a T> + 'a> {
    let mut iter = moved!(items).into_iter();
    let item = Iterator::next(&mut iter);
    loop {
        break;
    }
}

fn main() {
    let items = &[1, 2, 3];
    for value in gen(items) {
        println!("{}", value);
    }
}
*/

#[generator]
fn gen() -> Box<Iterator<Item=usize>> {
    let range = 1..5;

    /*
    let mut iter = range.into_iter();

    loop {
        match ::std::iter::Iterator::next(&mut iter) {
            Some(item) => {
                println!("item: {}", item);
            }
            None => {
                break;
            }
        }
    }
    */

    /*
    for item in range {
        println!("item: {}", item);
    }
    */

    {
        let mut __stateful_iter = ::std::iter::IntoIterator::into_iter(range);
        loop  {
            match ::std::iter::Iterator::next(&mut __stateful_iter) {
                ::std::option::Option::Some(item) => {
                    println!("item: {}" , item);
                }
                ::std::option::Option::None => break ,
            }
        }
    }
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
