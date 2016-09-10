#![feature(plugin)]
#![plugin(stateful)]

#[generator]
fn yield_() -> Box<Iterator<Item=usize>> {
    let _x: usize;
    {
        yield_!(1);
        _x = 1_f64; //~ ERROR mismatched types [E0308]
                    //~^ expected usize, found f64
    };
}

fn main() {
    for value in yield_() {
        println!("yield_: {:?}", value);
    }
}
