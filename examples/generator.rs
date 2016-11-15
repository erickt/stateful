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
    let name1 = Thing;

    /*
    let mut iter = range.into_iter();
    */

    loop {
        break;

        /*
        match copied!(iter).next() {
            Some(item) => {
                println!("item: {}", item);
            }
            None => {
                break;
            }
        }
        */
    }

    let name2 = name1;
}

fn main() {
    for value in gen() {
        println!("{}", value);
    }
}
