#![feature(test)]

extern crate test;

/*
state_machine! range(start: usize, end: usize) -> Option<usize> {
    machine: {
        start: start,
        end: end,
    }

    start: {
        if machine.start == machine.end {
            goto end;
        } else {
            return Some(machine.start);
        }
    }

    end: {
        return None;
    }
}
*/

mod range {
    pub struct Range {
        state: State,
        data: RangeData,
    }

    struct RangeData {
        start: usize,
        end: usize,
    }

    enum State {
        Start,
        Done,
    }

    pub fn new(start: usize, end: usize) -> Range {
        Range {
            state: State::Start,
            data: RangeData {
                start: start,
                end: end,
            },
        }
    }

    impl Iterator for Range {
        type Item = usize;

        fn next(&mut self) -> Option<Self::Item> {
            loop {
                match self.state {
                    State::Start => {
                        if self.data.start == self.data.end {
                            self.state = State::Done;
                        } else {
                            self.data.start += 1;
                            return Some(self.data.start);
                        }
                    }

                    State::Done => {
                        return None;
                    }
                }
            }
        }
    }

    type StateFn = fn(&mut Range) -> State;
}

#[bench]
fn bench_iter(b: &mut test::Bencher) {
    b.iter(|| {
        for i in 0 .. 1000000 {
            test::black_box(i);
        }
    })
}

#[bench]
fn bench_range(b: &mut test::Bencher) {
    b.iter(|| {
        for i in range::new(0, 1000000) {
            test::black_box(i);
        }
    })
}

/*
state_machine! alternate<Lhs, Rhs>(lhs: Lhs, rhs: Rhs) -> Option<usize>
where Lhs: Iterator,
      Rhs: Iterator<Item=Lhs::Item> {
    machine: {
        use_lhs: true,
        lhs: Lhs,
        rhs: Rhs,
    }

    start: {
        if machine.use_lhs {
            machine.use_lhs = false;

            match machine.lhs.next() {
                Some(value) => {
                    return Some(value);
                }
                None => None
            }
        } else {
            match machine.rhs.next() {
                Some(value) => {
                    machine.use_lhs = true;
                    Some(value)
                }
                None => None
            }
        }
        goto lhs;
    }

    lhs: {
        match machine.lhs.next() {
            Some(value) => {
                machine.next_state = rhs;
                return Some(value);
            }
            None => {
                goto end;
            }
        }
    }

    rhs: {
        match machine.rhs.next() {
            Some(value) => {
                machine.next_state = lhs;
                return Some(value);
            }
            None => {
                goto end;
            }
        }
    }

    end: {
        return None;
    }
}
*/


struct AlternateIterator<Lhs, Rhs> {
    use_lhs: bool,
    lhs: Lhs,
    rhs: Rhs,
}

impl<Lhs, Rhs> Iterator for AlternateIterator<Lhs, Rhs>
    where Lhs: Iterator,
          Rhs: Iterator<Item=Lhs::Item>,
{
    type Item = Lhs::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.use_lhs {
            match self.lhs.next() {
                Some(value) => {
                    self.use_lhs = false;
                    Some(value)
                }
                None => None
            }
        } else {
            match self.rhs.next() {
                Some(value) => {
                    self.use_lhs = true;
                    Some(value)
                }
                None => None
            }
        }
    }
}


mod alternate {
    fn new<Lhs, Rhs>(lhs: Lhs, rhs: Rhs) -> Machine<Lhs, Rhs>
        where Lhs: Iterator,
              Rhs: Iterator<Item=Lhs::Item>
    {
        Machine {
            state: State::Start,
            data: Data {
                lhs: lhs,
                rhs: rhs,
            },
        }
    }

    pub struct Machine<Lhs, Rhs>
        where Lhs: Iterator,
              Rhs: Iterator<Item=Lhs::Item>
    {
        state: State,
        data: Data<Lhs, Rhs>,
    }

    impl<Lhs, Rhs> Iterator for Machine<Lhs, Rhs>
        where Lhs: Iterator,
              Rhs: Iterator<Item=Lhs::Item>
    {
        type Item = Lhs::Item;

        fn next(&mut self) -> Option<Self::Item> {
            loop {
                match self.state {
                    State::Start => {
                        self.state = State::Lhs;
                    }
                    State::Lhs => {
                        let machine = &mut self.data;

                        {
                            match machine.lhs.next() {
                                Some(value) => {
                                    self.state = State::Rhs;
                                    return Some(value);
                                }
                                None => {
                                    self.state = State::End;
                                }
                            }
                        }
                    }

                    State::Rhs => {
                        let machine = &mut self.data;

                        {
                            match machine.rhs.next() {
                                Some(value) => {
                                    self.state = State::Lhs;
                                    return Some(value);
                                }
                                None => {
                                    self.state = State::End;
                                }
                            }
                        }
                    }

                    State::End => {
                        return None;
                    }
                }
            }
        }
    }

    enum State {
        Start,
        Lhs,
        Rhs,
        End,
    }

    struct Data<Lhs, Rhs>
        where Lhs: Iterator,
              Rhs: Iterator<Item=Lhs::Item>
    {
        lhs: Lhs,
        rhs: Rhs,
    }
}


/*
state_machine! iter_tuple(data: &(usize, Vec<usize>, usize)) -> Option<usize> {
    state: struct Data<'a> {
        x: usize = data.0,
        y: slice::Iter<'a, usize> = data.1.iter(),
        z: usize = data.2,
    }

    machine: {
        let mut value = self.x;
        value += 1;
        yield Some(value);

        /*
        for item in self.y {
            yield Some(*item);
        }
        */

        loop {
            match self.y.next() {
                Some(item) => {
                    yield Some(*item);
                }
                None => {
                    break;
                }
            }
        }

        yield Some(self.z);

        None


        /*
        for item in machine.1.iter() {
            yield item;
        }

        let iter = machine.1.iter();
        loop {
            match iter.next() {
                Some(item) => {
                    yield Some(item);
                }
                None => {
                    break;
                }
            }
        }
        */

        yield Some(machine.2);
        None
    }
}
*/

mod iter_tuple {
    use std::slice;

    pub fn new<'a>(data: &'a (usize, Vec<usize>, usize)) -> Machine<'a> {
        Machine {
            state: State::Entry,
            data: Data::new(data)
        }
    }

    pub struct Machine<'a> {
        state: State,
        data: Data<'a>,
    }

    struct Data<'a> {
        x: usize,
        y: slice::Iter<'a, usize>,
        z: usize,
    }

    impl<'a> Data<'a> {
        fn new(data: &'a (usize, Vec<usize>, usize)) -> Self {
            Data {
                x: data.0,
                y: data.1.iter(),
                z: data.2,
            }
        }
    }

    enum State {
        Entry,
        State0,
        State1,
        State2,
        State3,
        Exit,
    }

    impl<'a> Machine<'a> {
        fn yield_(&mut self, value: Option<usize>, next_state: State) -> Option<usize> {
            self.state = next_state;
            value
        }
    }

    impl<'a> Iterator for Machine<'a> {
        type Item = usize;

        fn next(&mut self) -> Option<Self::Item> {
            loop {
                match self.state {
                    State::Entry => {
                        let mut machine = &mut self.data;

                        let mut value = machine.x;
                        value += 1;

                        self.state = State::State0;
                        return Some(value);
                    }

                    State::State0 => {
                        let mut machine = &mut self.data;

                        self.state = State::State1;
                    }

                    State::State1 => {
                        let mut machine = &mut self.data;

                        match machine.y.next() {
                            Some(item) => {
                                self.state = State::State1;
                                return Some(*item);
                            }
                            None => {
                                self.state = State::State2;
                            }
                        }
                    }

                    State::State2 => {
                        let mut machine = &mut self.data;

                        self.state = State::State1;
                    }

                    State::State3 => {
                        let mut machine = &mut self.data;

                        self.state = State::Exit;
                        return Some(machine.z);
                    }

                    State::Exit => {
                        return None;
                    }
                }
            }
        }
    }
}

fn main() {
    let iter = range::new(0, 10);

    for item in iter {
        println!("item: {}", item);
    }
}
