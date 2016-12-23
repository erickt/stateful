#![feature(plugin)]
#![plugin(stateful)]
#![allow(dead_code)]
#![allow(non_shorthand_field_patterns)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_variables)]

use std::iter::Iterator;
use std::mem;

struct S(u32);

struct Empty;

fn foo() {}

#[generator]
fn gen(pred: bool) -> Box<Iterator<Item = usize>> {
    yield_!(1);
}

fn gen2(pred: bool) -> Box<Iterator<Item = usize>> {
    struct StateMachine<S, F> {
        state: S,
        resume: F,
    }
    impl<S, F, Item> StateMachine<S, F>
        where S: ::std::default::Default,
              F: Fn(S, ()) -> (::std::option::Option<Item>, S)
    {
        fn new(initial_state: S, resume: F) -> Self {
            StateMachine {
                state: initial_state,
                resume: resume,
            }
        }
    }
    impl<S, F, Item> ::std::iter::Iterator for StateMachine<S, F>
        where S: ::std::default::Default,
              F: Fn(S, ()) -> (::std::option::Option<Item>, S)
    {
        type Item = Item;
        fn next(&mut self) -> ::std::option::Option<Item> {
            let state = ::std::mem::replace(&mut self.state, S::default());
            let (value, state) = (self.resume)(state, ());
            self.state = state;
            value
        }
    }
    enum CoroutineState<T1, T2> {
        Illegal,
        State0Start((T1,)),
        State2Suspend((T1, T2)),
    }
    impl<T1, T2> ::std::default::Default for CoroutineState<T1, T2> {
        fn default() -> Self {
            CoroutineState::Illegal
        }
    }
    enum InternalState<T1, T3, T2, T0> {
        Illegal,
        State0Start((T1,)),
        State1AfterCall((T1, T3)),
        State2Suspend((T1, T2)),
        State3Drop((T1, T2)),
        State4Drop((T1,)),
        State5Drop((T0,)),
        State6End((T0,)),
    }
    ::std::boxed::Box::new(StateMachine::new(CoroutineState::State0Start(scope1),
                                             |mut coroutine_state, args| {
        let mut state = match coroutine_state {
            CoroutineState::State0Start(scope1) => InternalState::State0Start(scope1),
            CoroutineState::State2Suspend(scope1) => InternalState::State2Suspend(scope1),
            CoroutineState::Illegal => panic!("illegal state"),
        };
        loop {
            match state {
                InternalState::State0Start(scope1) => {
                    let (pred,) = scope1;
                    {
                        temp_expr3 = Some(1);
                        state = InternalState::State1AfterCall((pred, temp_expr3));
                        continue;
                    }
                }
                InternalState::State1AfterCall(scope1) => {
                    let (pred, mut temp_expr3) = scope1;
                    {
                        return (temp_expr3, CoroutineState::State2Suspend(scope1));
                    }
                }
                InternalState::State2Suspend(scope1) => {
                    let (pred, mut temp_stmt_expr2) = scope1;
                    {
                        state = InternalState::State3Drop((pred, temp_stmt_expr2));
                        continue;
                    }
                }
                InternalState::State3Drop(scope1) => {
                    let (pred, mut temp_stmt_expr2) = scope1;
                    {
                        state = InternalState::State4Drop((pred,));
                        continue;
                    }
                }
                InternalState::State4Drop(scope1) => {
                    let (pred,) = scope1;
                    {
                        let mut return_pointer;
                        return_pointer = ();
                        state = InternalState::State5Drop((return_pointer,));
                        continue;
                    }
                }
                InternalState::State5Drop(scope0) => {
                    let (mut return_pointer,) = scope0;
                    {
                        state = InternalState::State6End((return_pointer,));
                        continue;
                    }
                }
                InternalState::State6End(scope0) => {
                    let (mut return_pointer,) = scope0;
                    {
                        return (::std::option::Option::None, CoroutineState::Illegal);
                    }
                }
            }
        }
    }))
}

// #[generator]
// fn gen(pred: bool) -> Box<Iterator<Item = usize>> {
// let a = S(1); let mut b = S(2); let c; let d;
//
// if pred {
// mem::drop(a);
// b = S(3);
// c = S(40);
// } else {
// mem::drop(b);
// b = S(30);
// c = S(4);
// }
//
// d = S(5);
//
// foo();
// }
//


fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
