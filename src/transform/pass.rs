use mar::Mar;

pub trait MarPass {
    fn run_pass(&mut self, mar: &mut Mar);
}
