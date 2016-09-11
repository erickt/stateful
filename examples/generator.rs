#![feature(plugin)]
#![cfg_attr(feature = "impl_trait", feature(conservative_impl_trait))]
#![plugin(stateful)]
#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(non_shorthand_field_patterns)]

fn main() {
    #[generator]
    fn gen() -> String {
        let x = format!("a");
        {
            yield_!(x.clone());
            let x = format!("b");
            yield_!(moved!(x));
        }
        yield_!(moved!(x));
    }

    for value in gen() {
        println!("{}", value);
    }
}
