// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use bit_vec::BitVec;
use data_structures::indexed_vec::{Idx, IndexVec};
use mar::*;
use transform::pass::MarPass;
use traversal;

#[derive(Debug)]
pub struct SimplifyCfg;

impl SimplifyCfg {
    pub fn new() -> Self {
        SimplifyCfg
    }
}

impl MarPass for SimplifyCfg {
    fn run_pass(&mut self, mar: &mut Mar) {
        CfgSimplifier::new(mar).simplify();
        remove_dead_blocks(mar);
        
        // FIXME: Should probably be moved into some kind of pass manager
        mar.basic_blocks_mut().raw.shrink_to_fit();
    }
}

impl Default for SimplifyCfg {
    fn default() -> Self {
        SimplifyCfg::new()
    }
}

pub struct CfgSimplifier<'a> {
    basic_blocks: &'a mut IndexVec<BasicBlock, BasicBlockData>,
    pred_count: IndexVec<BasicBlock, u32>
}

impl<'a> CfgSimplifier<'a> {
    fn new(mar: &'a mut Mar) -> Self {
        let mut pred_count = IndexVec::from_elem(0u32, mar.basic_blocks());

        // we can't use mar.predecessors() here because that counts
        // dead blocks, which we don't want to.
        for (_, data) in traversal::preorder(mar) {
            if let Some(ref term) = data.terminator {
                for tgt in term.successors() {
                    pred_count[tgt] += 1;
                }
            }
        }

        let basic_blocks = mar.basic_blocks_mut();

        CfgSimplifier {
            basic_blocks: basic_blocks,
            pred_count: pred_count
        }
    }

    fn simplify(mut self) {
        loop {
            let mut changed = false;

            for bb in self.basic_blocks.indices() {
                if self.pred_count[bb] == 0 {
                    continue
                }

                debug!("simplifying {:?}", bb);

                let mut terminator = self.basic_blocks[bb].terminator.take()
                    .expect("invalid terminator state");

                for successor in terminator.successors_mut() {
                    self.collapse_goto_chain(successor, &mut changed);
                }

                let mut new_stmts = vec![];
                let mut inner_changed = true;
                while inner_changed {
                    inner_changed = false;
                    inner_changed |= self.simplify_branch(&mut terminator);
                    inner_changed |= self.merge_successor(&mut new_stmts, &mut terminator);
                    changed |= inner_changed;
                }

                self.basic_blocks[bb].statements.extend(new_stmts);
                self.basic_blocks[bb].terminator = Some(terminator);

                changed |= inner_changed;
            }

            if !changed { break }
        }
    }

    // Collapse a goto chain starting from `start`
    fn collapse_goto_chain(&mut self, start: &mut BasicBlock, changed: &mut bool) {
        let mut terminator = match self.basic_blocks[*start] {
            BasicBlockData {
                ref statements,
                terminator: ref mut terminator @ Some(Terminator {
                    // FIXME: We cannot currently optimize end scopes or else we might not properly
                    // drop copyable types.
                    kind: TerminatorKind::Goto {
                        end_scope: false,
                        ..
                    },
                    ..
                }), ..
            } if statements.is_empty() => terminator.take(),
            // if `terminator` is None, this means we are in a loop. In that
            // case, let all the loop collapse to its entry.
            _ => return
        };

        let target = match terminator {
            Some(Terminator {
                kind: TerminatorKind::Goto {
                    ref mut target,
                    end_scope: false,
                },
                ..
            }) => {
                self.collapse_goto_chain(target, changed);
                *target
            }
            _ => unreachable!()
        };
        self.basic_blocks[*start].terminator = terminator;

        debug!("collapsing goto chain from {:?} to {:?}", *start, target);

        *changed |= *start != target;
        self.pred_count[target] += 1;
        self.pred_count[*start] -= 1;
        *start = target;
    }

    // merge a block with 1 `goto` predecessor to its parent
    fn merge_successor(&mut self,
                       new_stmts: &mut Vec<Statement>,
                       terminator: &mut Terminator)
                       -> bool
    {
        let target = match terminator.kind {
            // FIXME: We cannot currently optimize end scopes or else we might not properly drop
            // copyable types.
            TerminatorKind::Goto { target, end_scope: false }
                if self.pred_count[target] == 1
                => target,
            _ => return false
        };

        debug!("merging block {:?} into {:?}", target, terminator);
        *terminator = match self.basic_blocks[target].terminator.take() {
            Some(terminator) => terminator,
            None => {
                // unreachable loop - this should not be possible, as we
                // don't strand blocks, but handle it correctly.
                return false
            }
        };
        new_stmts.extend(self.basic_blocks[target].statements.drain(..));
        self.pred_count[target] = 0;

        true
    }

    // turn a branch with all successors identical to a goto
    fn simplify_branch(&mut self, terminator: &mut Terminator) -> bool {
        match terminator.kind {
            TerminatorKind::If { .. } |
            TerminatorKind::Match { .. } => {},
            _ => return false
        };

        let first_succ = {
            let successors = terminator.successors();
            if let Some(&first_succ) = terminator.successors().get(0) {
                if successors.iter().all(|s| *s == first_succ) {
                    self.pred_count[first_succ] -= (successors.len()-1) as u32;
                    first_succ
                } else {
                    return false
                }
            } else {
                return false
            }
        };

        debug!("simplifying branch {:?}", terminator);
        terminator.kind = TerminatorKind::Goto {
            target: first_succ,
            end_scope: false,
        };
        true
    }
}

fn remove_dead_blocks(mar: &mut Mar) {
    let mut seen = BitVec::from_elem(mar.basic_blocks().len(), false);
    for (bb, _) in traversal::preorder(mar) {
        seen.set(bb.index(), true);
    }

    let basic_blocks = mar.basic_blocks_mut();

    let num_blocks = basic_blocks.len();
    let mut replacements : Vec<_> = (0..num_blocks).map(BasicBlock::new).collect();
    let mut used_blocks = 0;
    for (alive_index, set) in seen.iter().enumerate() {
        if !set {
            continue;
        }

        replacements[alive_index] = BasicBlock::new(used_blocks);
        if alive_index != used_blocks {
            // Swap the next alive block data with the current available slot. Since alive_index is
            // non-decreasing this is a valid operation.
            basic_blocks.raw.swap(alive_index, used_blocks);
        }
        used_blocks += 1;
    }
    basic_blocks.raw.truncate(used_blocks);

    for block in basic_blocks {
        for target in block.terminator_mut().successors_mut() {
            *target = replacements[target.index()];
        }
    }
}
