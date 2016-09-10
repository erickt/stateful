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
use std::collections::BTreeSet;
use super::pass::MarPass;

pub struct RemoveDeadBlocks;

impl RemoveDeadBlocks {
    pub fn new() -> Self {
        RemoveDeadBlocks
    }
}

impl MarPass for RemoveDeadBlocks {
    fn run_pass(&mut self, mar: &mut Mar) {
        let mut seen = BTreeSet::new();

        // This block is always required.
        seen.insert(START_BLOCK.index());

        let mut worklist = Vec::with_capacity(4);
        worklist.push(START_BLOCK);

        // We can't delete blocks that have statements since we haven't type checked them yet.
        for bb in mar.all_basic_blocks() {
            if !mar.basic_block_data(bb).statements.is_empty() && seen.insert(bb.index()) {
                worklist.push(bb);
            }
        }

        while let Some(bb) = worklist.pop() {
            for succ in &mar.basic_block_data(bb).terminator().successors() {
                if seen.insert(succ.index()) {
                    worklist.push(*succ);
                }
            }
        }

        retain_basic_blocks(mar, &seen);
    }
}

impl Default for RemoveDeadBlocks {
    fn default() -> Self {
        RemoveDeadBlocks::new()
    }
}

/// Mass removal of basic blocks to keep the ID-remapping cheap.
fn retain_basic_blocks(mar: &mut Mar, keep: &BTreeSet<usize>) {
    let num_blocks = mar.basic_blocks.len();

    let mut replacements: Vec<_> = (0..num_blocks).map(BasicBlock::new).collect();
    let mut used_blocks = 0;
    for alive_index in keep.iter() {
        replacements[*alive_index] = BasicBlock::new(used_blocks);
        if *alive_index != used_blocks {
            // Swap the next alive block data with the current available slot. Since alive_index is
            // non-decreasing this is a valid operation.
            mar.basic_blocks.swap(*alive_index, used_blocks);
        }
        used_blocks += 1;
    }

    mar.basic_blocks.truncate(used_blocks);

    // Fix up all of the interior edges.
    for bb in mar.all_basic_blocks() {
        // Update the terminators to point at the new block ids.
        for target in mar.basic_block_data_mut(bb).terminator_mut().successors_mut() {
            *target = replacements[target.index()];
        }
    }
}
