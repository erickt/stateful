use mar::repr::*;

pub trait MarPass {
    fn run_pass(&mut self, mar: &mut Mar);
}
