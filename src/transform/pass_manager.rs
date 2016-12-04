use mir::Mir;
use transform::pass::MirPass;

pub struct PassManager {
    passes: Vec<Box<MirPass>>,
}

impl PassManager {
    pub fn new() -> Self {
        PassManager {
            passes: vec![],
        }
    }

    pub fn add_pass(&mut self, pass: Box<MirPass>) {
        self.passes.push(pass);
    }

    pub fn run(&mut self, mir: &mut Mir) {
        for pass in &mut self.passes {
            pass.run_pass(mir);
        }
    }
}

impl Default for PassManager {
    fn default() -> Self {
        PassManager::new()
    }
}
