#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

use std::cell::RefCell;
use std::rc::Rc;

/*
#[generator]
fn gen(cond: bool) -> Box<Iterator<Item=usize>> {
    if cond == true {
        yield_!(2);
    } else {
        yield_!(3);
    }
}
*/

fn gen(cond: bool) -> Box<Iterator<Item = usize>> {
    struct StateMachine<S, F> {
        state: S,
        next: F,
    }
    impl <S, F, Item> StateMachine<S, F> where S: ::std::default::Default,
    F: Fn(S) -> (::std::option::Option<Item>, S) {
        fn new(initial_state: S, next: F) -> Self {
            StateMachine{state: initial_state, next: next,}
        }
    }
    impl <S, F, Item> ::std::iter::Iterator for StateMachine<S, F> where
        S: ::std::default::Default, F: Fn(S) -> (::std::option::Option<Item>, S)
        {
            type
                Item
                =
                Item;
            fn next(&mut self) -> ::std::option::Option<Item> {
                let state = ::std::mem::replace(&mut self.state, S::default());
                let (value, state) = (self.next)(state);
                self.state = state;
                value
            }
        }
    enum State<T1, T0> {
        Illegal,
        State0Start {
            cond: T1,
        },
        State1Then {
            cond: T1,
        },
        State2Else {
            cond: T1,
        },
        State3AfterSuspend {
            cond: T1,
        },
        State4EndScope {
            return_: T0,
            cond: T1,
        },
        State5AfterSuspend {
            return_: T0,
            cond: T1,
        },
        State6EndScope {
            return_: T0,
            cond: T1,
        },
        State7IfJoin {
            return_: T0,
            cond: T1,
        },
        State8EndScope {
            return_: T0,
            cond: T1,
        },
        State9End {
            return_: T0,
            cond: T1,
        },
    }
    impl <T1, T0> ::std::default::Default for State<T1, T0> {
        fn default() -> Self { State::Illegal }
    }
    ::std::boxed::Box::new(StateMachine::new(State::State0Start{cond: cond,},
                                             |mut state| {
                                                 loop  {
                                                     match state {
                                                         State::State0Start { cond
                                                         } => {
                                                             let if_cond2;
                                                             if_cond2 =
                                                                 cond == true;
                                                             if if_cond2 {
                                                                 state =
                                                                     State::State1Then{cond:
                                                                         cond,};
                                                                 continue ;
                                                             } else {
                                                                 state =
                                                                     State::State2Else{cond:
                                                                         cond,};
                                                                 continue ;
                                                             }
                                                         }
                                                         State::State1Then { cond
                                                         } => {
                                                             return (::std::option::Option::Some(2),
                                                             State::State3AfterSuspend{cond:
                                                                 cond,});
                                                         }
                                                         State::State2Else { cond
                                                         } => {
                                                             return (::std::option::Option::Some(3),
                                                             State::State5AfterSuspend{return_:
                                                                 return_,
                                                                 cond:
                                                                     cond,});
                                                         }
                                                         State::State3AfterSuspend {
                                                             cond } => {
                                                                 let _stmt_result_temp =
                                                                     ();
                                                                 let return_;
                                                                 return_ = ();
                                                                 state =
                                                                     State::State4EndScope{return_:
                                                                         return_,
                                                                         cond:
                                                                             cond,};
                                                                 continue ;
                                                             }
                                                         State::State4EndScope {
                                                             return_, cond } => {
                                                                 state =
                                                                     State::State7IfJoin{return_:
                                                                         return_,
                                                                         cond:
                                                                             cond,};
                                                                 continue ;
                                                             }
                                                         State::State5AfterSuspend {
                                                             return_, cond } => {
                                                                 let _stmt_result_temp =
                                                                     ();
                                                                 let return_;
                                                                 return_ = ();
                                                                 state =
                                                                     State::State6EndScope{return_:
                                                                         return_,
                                                                         cond:
                                                                             cond,};
                                                                 continue ;
                                                             }
                                                         State::State6EndScope {
                                                             return_, cond } => {
                                                                 state =
                                                                     State::State7IfJoin{return_:
                                                                         return_,
                                                                         cond:
                                                                             cond,};
                                                                 continue ;
                                                             }
                                                         State::State7IfJoin {
                                                             return_, cond } => {
                                                                 state =
                                                                     State::State8EndScope{return_:
                                                                         return_,
                                                                         cond:
                                                                             cond,};
                                                                 continue ;
                                                             }
                                                         State::State8EndScope {
                                                             return_, cond } => {
                                                                 state =
                                                                     State::State9End{return_:
                                                                         return_,
                                                                         cond:
                                                                             cond,};
                                                                 continue ;
                                                             }
                                                         State::State9End {
                                                             return_, cond } => {
                                                                 ::std::mem::drop(cond);
                                                                 ::std::mem::drop(return_);
                                                                 return (::std::option::Option::None,
                                                                         State::Illegal);
                                                             }
                                                         State::Illegal => {
                                                             unreachable!("illegal state")
                                                         }
                                                     }
                                                 } }))
}

fn main() {
    for value in gen(true) {
        println!("{}", value);
    }
}
