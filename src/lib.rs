extern crate time;

use std::ops;
use std::mem;

/*
yield fn state_machine() -> yield usize {
    println!("before");
    yield 1;
    yield 2;
    println!("after");
}
*/

struct StateMachine {
    state: usize,
}

impl StateMachine {
    fn next(&mut self) -> Option<usize> {
        loop {
            match self.state {
                0 => {
                    println!("before");
                    self.state = 1;
                }
                1 => {
                    self.state = 2;
                    return Some(1);
                }
                2 => {
                    self.state = 3;
                    return Some(2);
                }
                _ => {
                    println!("after");
                    return None;
                }
            }
        }
    }
}

/*
async fn get_string_async() {
    let mut is_loading: bool = true;

    let result: String = await get_string();

    is_loading = false;
}

fn get_string() -> Task<String> {
    await sleep(2000);
    String::new()
*/

enum AsyncResult<T, E> {
    Ok(T),
    Err(E),
    Running,
}

struct Immutable<T> {
    value: T,
}

impl<T> Immutable<T> {
    fn new(value: T) -> Self {
        Immutable {
            value: value,
        }
    }

    fn unwrap(self) -> T {
        self.value
    }
}

impl<T> ops::Deref for Immutable<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

struct Mutable<T> {
    value: T,
}

impl<T> Mutable<T> {
    fn new(value: T) -> Self {
        Mutable {
            value: value,
        }
    }

    fn set(&mut self, value: T) {
        self.value = value;
    }

    fn unwrap(self) -> T {
        self.value
    }
}

impl<'a, T> ops::Deref for Mutable<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'a, T> ops::DerefMut for Mutable<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

enum Void {}

//////////////////////////////////////////////////////////////////////////////

struct GetStringAsync {
    state: GetStringAsyncState,
}

enum GetStringAsyncState {
    State0(GetStringAsyncData, GetString),
    State1(GetStringAsyncData, String),
    Error,
}

struct GetStringAsyncData {
    is_loading: Mutable<bool>,
}

impl GetStringAsync {
    fn new() -> Self {
        let is_loading = Mutable::new(true);
        let get_string = GetString::new();

        GetStringAsync {
            state: GetStringAsyncState::State0(
                GetStringAsyncData {
                    is_loading: is_loading,
                },
                get_string,
            ),
        }
    }

    fn next(&mut self) -> AsyncResult<String, Void> {
        loop {
            let mut state = GetStringAsyncState::Error;
            mem::swap(&mut state, &mut self.state);

            match state {
                GetStringAsyncState::State0(data, mut awaitable) => {
                    match awaitable.next() {
                        AsyncResult::Ok(result) => {
                            self.state = GetStringAsyncState::State1(data, result);
                        }
                        AsyncResult::Running => {
                            self.state = GetStringAsyncState::State0(data, awaitable);
                            return AsyncResult::Running;
                        }
                        AsyncResult::Err(err) => {
                            return AsyncResult::Err(err);
                        }
                    }
                }
                GetStringAsyncState::State1(mut data, result) => {
                    data.is_loading.set(false);
                    return AsyncResult::Ok(result);
                }
                GetStringAsyncState::Error => {
                    unreachable!()
                }
            }
        }
    }
}

struct GetString {
    state: GetStringState,
}

enum GetStringState {
    State0(GetStringData, Sleep),
    State1(GetStringData),
    Error,
}

struct GetStringData;

impl GetString {
    fn new() -> Self {
        let data = GetStringData;
        let async_sleep = Sleep::new(2000);

        GetString {
            state: GetStringState::State0(data, async_sleep),
        }
    }

    fn next(&mut self) -> AsyncResult<String, Void> {
        loop {
            let mut state = GetStringState::Error;
            mem::swap(&mut state, &mut self.state);

            match state {
                GetStringState::State0(data, mut async) => {
                    match async.next() {
                        AsyncResult::Ok(()) => {
                            self.state = GetStringState::State1(data);
                        }
                        AsyncResult::Running => {
                            self.state = GetStringState::State0(data, async);
                            return AsyncResult::Running;
                        }
                        AsyncResult::Err(err) => {
                            return AsyncResult::Err(err);
                        }
                    }
                }
                GetStringState::State1(data) => {
                    let result = String::new();
                    return AsyncResult::Ok(result);
                }
                GetStringState::Error => {
                    unreachable!()
                }
            }
        }
    }
}

struct Sleep {
    end: time::Tm,
}

impl Sleep {
    fn new(time: i64) -> Self {
        Sleep {
            end: time::now() + time::Duration::milliseconds(time),
        }
    }

    fn next(&mut self) -> AsyncResult<(), Void> {
        if time::now() <= self.end {
            AsyncResult::Ok(())
        } else {
            AsyncResult::Running
        }
    }
}

fn main() {
}
