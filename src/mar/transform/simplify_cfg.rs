// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use mar::repr::*;
use super::pass::MarPass;
use super::remove_dead_blocks::RemoveDeadBlocks;

#[derive(Debug)]
pub struct SimplifyCfg;

impl SimplifyCfg {
    pub fn new() -> Self {
        SimplifyCfg
    }

    fn remove_goto_chains(&self, mar: &mut Mar) -> bool {
        // Find the target at the end of the jump chain, return None if there is a loop
        fn final_target(mar: &Mar, mut target: BasicBlock) -> Option<BasicBlock> {
            // Keep track of already seen blocks to detect loops
            let mut seen: Vec<BasicBlock> = Vec::with_capacity(8);

            while mar.basic_block_data(target).statements.is_empty() {
                // NB -- terminator may have been swapped with `None`
                // below, in which case we have a cycle and just want
                // to stop
                if let Some(ref terminator) = mar.basic_block_data(target).terminator {
                    match terminator.kind {
                        TerminatorKind::Goto { target: next } => {
                            if seen.contains(&next) {
                                return None;
                            }
                            seen.push(next);
                            target = next;
                        }
                        _ => break
                    }
                } else {
                    break
                }
            }

            Some(target)
        }

        let mut changed = false;
        for bb in mar.all_basic_blocks() {
            // Temporarily take ownership of the terminator we're modifying to keep borrowck happy
            let mut terminator = mar.basic_block_data_mut(bb).terminator.take()
                                    .expect("invalid terminator state");

            debug!("remove_goto_chains: bb={:?} terminator={:?}", bb, terminator);

            for target in terminator.successors_mut() {
                let new_target = match final_target(mar, *target) {
                    Some(new_target) => new_target,
                    None if mar.basic_block_data(bb).statements.is_empty() => bb,
                    None => continue
                };
                changed |= *target != new_target;
                *target = new_target;
            }
            mar.basic_block_data_mut(bb).terminator = Some(terminator);
        }
        changed
    }
}

impl MarPass for SimplifyCfg {
    fn run_pass(&mut self, mar: &mut Mar) {
        let mut changed = true;

        while changed {
            changed = self.remove_goto_chains(mar);
            RemoveDeadBlocks.run_pass(mar);
        }
    }
}
