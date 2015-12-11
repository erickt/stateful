#![feature(plugin)]
#![plugin(stateful)]

#[test]
fn test() {
    #[hello_world]
    fn foo() {
        yield foo;
    }
}
