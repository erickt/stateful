#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

enum Poll<T> {
    Ok(T),
    NotReady,
}

trait Future {
    type Item;

    fn poll(&mut self) -> Poll<T>;
}

#[async]
fn count(count: u32) -> u32 {
    let mut sum = 0;

    for i in 0..count {
        println!("{} of {}", i, count);

        sum += 1;
        yield_!();
    }

    sum
}

#[async]
fn outer(count: u32) -> u32 {
    let sum = await!(inner(count));
    sum * sum
}

fn main() {
    let mut sm = outer(5);

    loop {
        match sm.poll() {
            Poll::Ok(result) => {
                println!("done: {}", result);
                break;
            }
            Poll::NotReady => {
                println!("not ready");
            }
        }
    }
}
