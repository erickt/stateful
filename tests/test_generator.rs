use super::empty::Empty;

#[test]
fn test_empty() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> { }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[test]
fn test_empty_if() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        if true { }
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[test]
fn test_empty_if_let() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let items = Empty::<usize>::new();
        let mut iter = moved!(items).into_iter();
        if let Some(_) = iter.next() { }
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[test]
fn test_empty_loop() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        loop {
            break;
        }
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[test]
fn test_empty_while() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        while false { }
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[test]
fn test_empty_while_let() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let items = Empty::<usize>::new();
        let mut iter = moved!(items).into_iter();
        while let Some(_) = iter.next() { }
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[test]
fn test_empty_for() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let iter = Empty::<usize>::new();
        for _ in moved!(iter) { }
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[should_panic]
#[test]
fn test_empty_with_macro() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        assert_eq!(true, false);
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}

#[test]
fn test_break_value() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let x = loop {
            break 5;
        };
        yield_!(x);
    }

    let mut iter = gen();
    assert_eq!(iter.next(), Some(5));
    assert_eq!(iter.next(), None);
}

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

    let mut iter = gen_ints();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_item_slice() {
    #[generator]
    fn gen_item_slice<'a, T: 'a>(items: &'a [T]) -> Box<Iterator<Item=&'a T> + 'a> {
        for item in items.iter() {
            yield_!(item);
        }
    }

    let items = &[1, 2, 3];
    let mut iter = gen_item_slice(items);
    assert_eq!(iter.next(), Some(&1));
    assert_eq!(iter.next(), Some(&2));
    assert_eq!(iter.next(), Some(&3));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_moved() {
    #[generator]
    fn gen<T: 'static>(items: Vec<T>) -> Box<Iterator<Item=T>> {
        for item in moved!(items) {
            yield_!(item);
        }
    }

    let mut iter = gen(vec![1, 2, 3]);
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), None);
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

    let mut iter = gen();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), Some(4));
    assert_eq!(iter.next(), None);
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

    let mut iter = gen();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), Some(4));
    assert_eq!(iter.next(), Some(5));
    assert_eq!(iter.next(), Some(6));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_yield_expr() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        yield_!(1);
        yield_!(2);
        yield_!(3)
    }

    let mut iter = gen();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_yield_in_assign() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let _x: () = yield_!(1);
    }

    let mut iter = gen();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), None);
}

/*
#[test]
fn test_shadowing() {
    #[generator]
    fn gen() -> Box<Iterator<Item=String>> {
        let x = format!("a");
        {
            yield_!(moved!(x.clone()));
            let x = format!("b");
            {
                yield_!(moved!(x.clone()));
                let x = format!("c");
                yield_!(moved!(x));
            }
            yield_!(moved!(x));
        }
        yield_!(moved!(x));
    }

    let mut iter = gen();
    assert_eq!(iter.next(), Some(format!("a")));
    assert_eq!(iter.next(), Some(format!("b")));
    assert_eq!(iter.next(), Some(format!("c")));
    assert_eq!(iter.next(), Some(format!("b")));
    assert_eq!(iter.next(), Some(format!("a")));
    assert_eq!(iter.next(), None);
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

    let mut iter = gen();
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), None);
}
*/

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

    let mut iter = gen(true);
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), None);

    let mut iter = gen(false);
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.next(), None);
}

/*
#[test]
fn test_let_assign() {
    #[generator]
    fn gen() -> Box<Iterator<Item=usize>> {
        let x;
        x = format!("a");
        let x;
        x = format!("b");
    }

    let mut iter = gen();
    assert_eq!(iter.next(), None);
}
*/
