#![feature(plugin)]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

extern crate futures;
extern crate futures_cpupool;
extern crate tokio_timer;

use futures::*;
use futures_cpupool::CpuPool;
use std::time::*;
use tokio_timer::*;

/*
#[async]
fn short_running_future(timer: Timer) -> Box<Future<Item=(), Error=TimerError> + Send> {
    println!("short sleep");

    await!(timer.sleep(Duration::from_millis(20)));

    ()
}
*/

#[async]
fn long_running_future(timer: Timer,
                       times: u64,
                       a: u64) -> Box<Future<Item=u64, Error=TimerError> + Send> {
    await!(timer.sleep(Duration::from_millis(20)));

    /*
    await!(short_running_future(timer.clone()));

    for i in 0 .. times {
        if i % 2 == 0 {
            let sleep_time = a * i;
            print!("sleep {} {}", i, sleep_time);
            await!(timer.sleep(Duration::from_millis(sleep_time)));
        } else {
            await!(short_running_future(timer.clone()));
        }
    }
    */

    println!("done sleeping");

    a
}

fn main() {
    // Create a worker thread pool with four threads
    let pool = CpuPool::new(4);

    let timer = Timer::default();

    // Execute some work on the thread pool, optionally closing over data.
    let a = pool.spawn(long_running_future(timer.clone(), 10, 100));
    let b = pool.spawn(long_running_future(timer.clone(), 5, 200));

    // Express some further computation once the work is completed on the thread
    // pool.
    let c = a.join(b).map(|(a, b)| a + b).wait().unwrap();

    // Print out the result
    println!("{:?}", c);
}
