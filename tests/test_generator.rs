#![feature(plugin)]
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
