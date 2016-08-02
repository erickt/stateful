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

    fn poll(&mut self) -> Poll<Self::Item>;
}

#[async]
fn async_counter(count: u32) -> u32 {
    suspend!();
    1
    /*
    let mut sum = 0;

    for i in 0..count {
        println!("{} of {}", i, count);

        sum += 1;
        suspend!();
    }

    sum
    */
}

/*
fn async_counter(count: u32) -> ::std::boxed::Box<Future<Item = u32>> {
    struct Wrapper<S, F> {
        state: S,
        next: F,
    }
    impl<S, T, F> Wrapper<S, F> where F: Fn(S) -> (Poll<T>, S) {
        fn new(initial_state: S, next: F) -> Self {
            Wrapper {
                state: initial_state,
                next: next,
            }
        }
    }
    impl <S, T, F> Future for Wrapper<S, F> where S: Default, F: Fn(S) -> (Poll<T>, S) {
        type Item = T;

        fn poll(&mut self) -> Poll<Self::Item> {
            let old_state = ::std::mem::replace(&mut self.state, S::default());
            let (value, next_state) = (self.next)(old_state);
            self.state = next_state;
            value
        }
    }

    enum State<T0, T1, T2, T3, T4> {
        State0Start {
            count: T0,
        },
        State1End {
            return_value: T4,
        },
        State2Loop {
            count: T0,
            sum: T1,
            __stateful_iter: T2,
        },
        State3Arm {
            count: T0,
            sum: T1,
            __stateful_iter: T2,
            i: T3,
        },
        State4EndScope {
            count: T0,
            sum: T1,
        },
        Illegal,
    }
    impl <T0, T1, T2, T3, T4> Default for State<T0, T1, T2, T3, T4> {
        fn default() -> Self { State::Illegal }
    }

    Box::new(Wrapper::new(State::State0Start { count: count }, |mut state| {
        loop {
            match state {
                State::State0Start { count } => {
                    let mut sum = 0;
                    let mut __stateful_iter = ::std::iter::IntoIterator::into_iter(0..count);
                    state = State::State2Loop {
                        count: count,
                        sum: sum,
                        __stateful_iter: __stateful_iter,
                    };
                    continue
                }
                State::State1End { return_value } => {
                    return (Poll::Ok(return_value), State::Illegal);
                }
                State::State2Loop { count, mut sum, mut __stateful_iter } => {
                    match __stateful_iter.next() {
                        ::std::option::Option::Some(i) => {
                            state = State::State3Arm {
                                count: count,
                                sum: sum,
                                __stateful_iter: __stateful_iter,
                                i: i,
                            };
                            continue
                        }
                        ::std::option::Option::None => {
                            state = State::State4EndScope{
                                count: count,
                                sum: sum,
                            };
                            continue
                        }
                    }
                }
                State::State3Arm { count, mut sum, mut __stateful_iter, i } => {
                    println!("{} of {}" , i , count);
                    sum += 1;
                    return (
                        ::Poll::NotReady,
                        State::State2Loop{
                            count: count,
                            sum: sum,
                            __stateful_iter: __stateful_iter,
                        },
                    );
                }
                State::State4EndScope { count, mut sum } => {
                    state = State::State1End { return_value: sum };
                    continue
                }
                State::Illegal => {
                    panic!("this state should never happen")

                }
            }
        }
    }))
}
*/

fn main() {
    let mut counter = async_counter(5);

    loop {
        match counter.poll() {
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
