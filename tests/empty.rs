use std::iter::{IntoIterator, Iterator};
use std::marker::PhantomData;

pub struct Empty<T>(PhantomData<T>);

impl<T> Empty<T> {
    pub fn new() -> Self {
        Empty(PhantomData)
    }
}

impl<T> IntoIterator for Empty<T> {
    type Item = T;
    type IntoIter = Iter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        Iter::new()
    }
}

pub struct Iter<T>(PhantomData<T>);

impl<T> Iter<T> {
    fn new() -> Self {
        Iter(PhantomData)
    }
}

impl<T> Iterator for Iter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}
