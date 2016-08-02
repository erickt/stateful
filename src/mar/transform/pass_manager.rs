use mar::repr::*;
use super::pass::MarPass;

pub struct PassManager {
    passes: Vec<Box<MarPass>>,
}

impl PassManager {
    pub fn new() -> Self {
        PassManager {
            passes: vec![],
        }
    }

    pub fn add_pass(&mut self, pass: Box<MarPass>) {
        self.passes.push(pass);
    }

    pub fn run(&mut self, mar: &mut Mar) {
        for pass in &mut self.passes {
            pass.run_pass(mar);
        }
    }
}

impl Default for PassManager {
    fn default() -> Self {
        PassManager::new()
    }
}
