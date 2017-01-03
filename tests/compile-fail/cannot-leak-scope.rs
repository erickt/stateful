#![feature(plugin)]
#![plugin(stateful)]

fn a() -> usize { 0 }
fn b(_: usize) -> usize { 1 }
fn c(_: usize) -> usize { 2 }
fn d(_: usize) -> usize { 3 }
fn e(_: usize) -> usize { 4 }

#[generator]
fn yield_() -> Box<Iterator<Item=usize>> {
    let x = a();
    {
        let y = b(x);
        yield_!(x + y);

        let z = c(y);
        yield_!(x + y + z);

        let w = d(z);
    };
    e(w); //~ ERROR unresolved value `w` [E0425]
}

fn main() {
    for value in yield_() {
        println!("yield_: {:?}", value);
    }
}
