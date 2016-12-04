use mir::Mir;

pub trait MirPass {
    fn run_pass(&mut self, mir: &mut Mir);
}
