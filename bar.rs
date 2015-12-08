use std::ops;

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

    let result = await get_string();

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

struct GetStringAsync {
    state: usize,
    data: GetStringAsyncData,
    call: Option<GetString>,
}

struct GetStringAsyncData {
    is_loading: Mutable<bool>,
}

impl GetStringAsync {
    fn new() -> Self {
        let is_loading = Mutable::new(true);

        GetStringAsync {
            state: 0,
            data: GetStringAsyncData {
                is_loading: is_loading,
            }
        }
    }

    fn next(&mut self) {
        match self.state {
            0 => {

            }
        }
    }
}

struct GetString {
    state: GetStringState,
    async_sleep: Option<Sleep>,
    data: GetStringData,
}

enum GetStringState {
    State0(Sleep),
    State1,
}

struct GetStringData;

impl GetString {
    fn new() -> Self {
        let async_sleep = Sleep::new(2000);

        GetString {
            state: GetStringState::State0(async_sleep),
            data: GetStringData,
        }
    }

    fn next(&mut self) -> AsyncResult<()> {
        match self.state {
            GetStringState::State0(ref mut async) => {
                match async.next() {
                    AsyncResult::Ok(()) => {
                        self.state = GetStringState::State0;
                    }
                    AsyncResult::Running => {
                        return AsyncResult::Running;
                    }
                    AsyncResult::Err(err) => {
                        return AsyncResult::Err(err);
                    }
                }
            }
            GetStringState::State1 => {
                AsyncResult::Ok(())
            }
        }
    }
}

struct Sleep {
    end: usize,
}

impl Sleep {
    fn new(time: usize) -> Self {
        Sleep {
            end: time::now() + time,
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
