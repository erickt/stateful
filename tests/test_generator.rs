#![feature(plugin)]
#![cfg_attr(feature = "impl_trait", feature(conservative_impl_trait))]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

#[test]
fn test_ints() {
    #[generator]
    fn gen_ints() -> usize {
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

#[test]
fn test_item_slice() {
    #[generator]
    fn gen_item_slice<'a, T>(items: &'a [T]) -> &'a T {
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
    fn gen<T: 'static>(items: Vec<T>) -> T {
        for item in moved!(items) {
            yield_!(moved!(item));
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
    fn gen() -> usize {
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
    fn gen() -> usize {
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
    fn gen() -> usize {
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
    fn gen() -> usize {
        let _x: () = yield_!(1);
    }

    let mut gen = gen();
    assert_eq!(gen.next(), Some(1));
    assert_eq!(gen.next(), None);
}
