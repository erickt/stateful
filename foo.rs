/*
async! {
    fn foo(x: usize) -> usize {
        let y = x + 1;
        let z = await foo();
        x + y + z
    }
}

struct fooFn {
    x: usize,
    y: usize,
    z: usize,
}

struct 

fn foo_Fn



fn foo() {
    state_machine!(
    )
}
*/

trait Invoke<Args> {
    type Output;

    fn invoke(self, args: Args) -> Self::Output;
}

struct State1<A> {
    x: A,
}

/*
fn range(start: usize, end: usize) -> yield usize {
    for i in start .. end {
        yield i;
    }
}
*/

struct RangeState {
    start: usize,
    end: usize,
}

struct Range {
    start: usize,
    end
}


struct Function {
    state_fn: fn(State)
    state: 
}



fn foo() -> usize {
    || {}
}

fn main() {
}
