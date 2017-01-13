#![feature(plugin)]
#![cfg_attr(feature = "impl_trait", feature(conservative_impl_trait))]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::iter::{IntoIterator, Iterator};
use std::marker::PhantomData;

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

/*
#[test]
fn test_empty() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}

#[test]
fn test_empty_if() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        if true { }
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}

#[test]
fn test_empty_if_let() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let items = Empty::<usize>::new();
        let mut iter = moved!(items).into_iter();
        if let Some(_) = iter.next() { }
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}

#[test]
fn test_empty_loop() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        loop {
            break;
        }
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}

#[test]
fn test_empty_while() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        while false { }
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}

#[test]
fn test_empty_while_let() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let items = Empty::<usize>::new();
        let mut iter = moved!(items).into_iter();
        while let Some(_) = iter.next() { }
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}

/*
#[test]
fn test_empty_for() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let iter = Empty::<usize>::new();
        for _ in iter { }
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}
*/

#[should_panic]
#[test]
fn test_empty_with_macro() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        assert_eq!(true, false);
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}
*/

#[test]
fn test_break_value() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let x = loop {
            break 5;
        };
        yield_!(x);
    }

    let mut gen = gen();
    assert_eq!(gen.next(), None);
}

/*
#[test]
fn test_ints() {
    #[generator]
    fn gen_ints() -> Box<Iterator<Item=usize>> {
        let x = {
            yield_!(1);
            let y = 3;
            yield_!(2);
            y
        };
        yield_!(x);
    }

    let mut gen = gen_ints();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), None);
}

/*
#[test]
fn test_item_slice() {
    #[generator]
    fn gen_item_slice<'a, T>(items: &'a [T]) -> Box<Iterator<Item=&'a T> + 'a> {
        for item in items.iter() {
            yield_!(item);
        }
    }

    let items = &[1, 2, 3];
    let mut gen = gen_item_slice(items);
    assert_eq!(gen.next(), Some(&1));
    assert_eq!(gen.next(), Some(&2));
    assert_eq!(gen.next(), Some(&3));
    assert_eq!(gen.next(), None);
}

#[test]
fn test_moved() {
    #[generator]
    fn gen<T: 'static>(items: Vec<T>) -> Box<Iterator<Item=T>> {
        for item in items {
            yield_!(item);
        }
    }

    let mut gen = gen(vec![1, 2, 3]);
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), None);
}

#[test]
fn test_partial_decl() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        {
            let c;
            yield_!(1);
            {
                yield_!(2);
                c = 4;
                yield_!(3);
            }
            let b = c;
            yield_!(b);
        };
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), Some(4));
    assert_eq!(gen.next(), None);
}

#[test]
fn test_partial_decl_nested() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        {
            let c;
            yield_!(1);
            {
                yield_!(2);
                let d;
                {
                    yield_!(3);
                    d = 6;
                }
                yield_!(4);
                c = d;
                yield_!(5);
            }
            let b = c;
            yield_!(b);
        };
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), Some(4));
    assert_eq!(gen.next(), Some(5));
    assert_eq!(gen.next(), Some(6));
    assert_eq!(gen.next(), None);
}

#[test]
fn test_yield_expr() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        yield_!(1);
        yield_!(2);
        yield_!(3)
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), None);
}

#[test]
fn test_yield_in_assign() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let _x: () = yield_!(1);
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), None);
}

#[test]
fn test_shadowing() {
    #[generator]
    fn gen() -> Box<Iterator<Item=String>> {
        let x = format!("a");
        {
            yield_!(x.clone());
            let x = format!("b");
            {
                yield_!(x.clone());
                let x = format!("c");
                yield_!(moved!(x));
            }
            yield_!(moved!(x));
        }
        yield_!(moved!(x));
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(format!("a")));
    assert_eq!(gen.next(), Some(format!("b")));
    assert_eq!(gen.next(), Some(format!("c")));
    assert_eq!(gen.next(), Some(format!("b")));
    assert_eq!(gen.next(), Some(format!("a")));
    assert_eq!(gen.next(), None);
}

#[cfg(feature = "impl_trait")]
#[test]
fn test_impl_trait() {
    #[generator]
    fn gen() -> impl Iterator<Item=usize> {
        let x = 1;
        {
            yield_!(x);
            let x = 2;
            {
                yield_!(x);
                let x = 3;
                yield_!(x);
            }
            yield_!(x);
        }
        yield_!(x);
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), None);
}

#[test]
fn test_if_yield() {
    #[generator]
    fn gen(cond: bool) -> Box<Iterator<Item=usize>> {
        if { yield_!(1); cond } {
            yield_!(2)
        } else {
            yield_!(3)
        }
    }

    let mut gen = gen(true);
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(2));
    assert_eq!(gen.next(), None);

    let mut gen = gen(false);
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), Some(3));
    assert_eq!(gen.next(), None);
}
*/
*/
