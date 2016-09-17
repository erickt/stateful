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
use mar::indexed_vec::Idx;
use mar::repr::*;

/// Preorder traversal of a graph.
///
/// Preorder traversal is when each node is visited before an of it's
/// successors
///
/// ```text
///
///         A
///        / \
///       /   \
///      B     C
///       \   /
///        \ /
///         D
/// ```
///
/// A preorder traversal of this graph is either `A B D C` or `A C D B`
#[derive(Clone)]
pub struct Preorder<'a> {
    mar: &'a Mar,
    visited: BitVec,
    worklist: Vec<BasicBlock>,
}

impl<'a> Preorder<'a> {
    pub fn new(mar: &'a Mar, root: BasicBlock) -> Preorder<'a> {
        let worklist = vec![root];

        Preorder {
            mar: mar,
            visited: BitVec::from_elem(mar.basic_blocks().len(), false),
            worklist: worklist
        }
    }
}

pub fn preorder(mar: &Mar) -> Preorder {
    let mut preorder = Preorder::new(mar, START_BLOCK);

    // MAR: We need to also visit nodes with statements so they can be type checked.
    preorder.worklist.extend(
        mar.basic_blocks().iter_enumerated().filter_map(|(bb, block)| {
            if block.statements.is_empty() {
                None
            } else {
                Some(bb)
            }
        })
    );

    preorder
}

impl<'a> Iterator for Preorder<'a> {
    type Item = (BasicBlock, &'a BasicBlockData);

    fn next(&mut self) -> Option<(BasicBlock, &'a BasicBlockData)> {
        while let Some(idx) = self.worklist.pop() {
            if self.visited.get(idx.index()).unwrap_or(false) {
                continue;
            }
            self.visited.set(idx.index(), true);

            let data = &self.mar[idx];

            if let Some(ref term) = data.terminator {
                for succ in term.successors() {
                    self.worklist.push(succ);
                }
            }

            return Some((idx, data));
        }

        None
    }
}

/*
/// Postorder traversal of a graph.
///
/// Postorder traversal is when each node is visited after all of it's
/// successors, except when the successor is only reachable by a back-edge
///
///
/// ```text
///
///         A
///        / \
///       /   \
///      B     C
///       \   /
///        \ /
///         D
/// ```
///
/// A Postorder traversal of this graph is `D B C A` or `D C B A`
pub struct Postorder<'a: 'a> {
    mar: &'a Mar,
    visited: BitVec,
    visit_stack: Vec<(BasicBlock, vec::IntoIter<BasicBlock>)>
}

impl<'a> Postorder<'a> {
    pub fn new(mar: &'a Mar, root: BasicBlock) -> Postorder<'a> {
        let mut po = Postorder {
            mar: mar,
            visited: BitVec::from_elem(mar.basic_blocks().len(), false),
            visit_stack: Vec::new()
        };


        let data = &po.mar[root];

        if let Some(ref term) = data.terminator {
            po.visited.set(root.index(), true);

            let succs = term.successors().into_iter();

            po.visit_stack.push((root, succs));
            po.traverse_successor();
        }

        po
    }

    fn traverse_successor(&mut self) {
        // This is quite a complex loop due to 1. the borrow checker not liking it much
        // and 2. what exactly is going on is not clear
        //
        // It does the actual traversal of the graph, while the `next` method on the iterator
        // just pops off of the stack. `visit_stack` is a stack containing pairs of nodes and
        // iterators over the sucessors of those nodes. Each iteration attempts to get the next
        // node from the top of the stack, then pushes that node and an iterator over the
        // successors to the top of the stack. This loop only grows `visit_stack`, stopping when
        // we reach a child that has no children that we haven't already visited.
        //
        // For a graph that looks like this:
        //
        //         A
        //        / \
        //       /   \
        //      B     C
        //      |     |
        //      |     |
        //      D     |
        //       \   /
        //        \ /
        //         E
        //
        // The state of the stack starts out with just the root node (`A` in this case);
        //     [(A, [B, C])]
        //
        // When the first call to `traverse_sucessor` happens, the following happens:
        //
        //     [(B, [D]),  // `B` taken from the successors of `A`, pushed to the
        //                 // top of the stack along with the successors of `B`
        //      (A, [C])]
        //
        //     [(D, [E]),  // `D` taken from successors of `B`, pushed to stack
        //      (B, []),
        //      (A, [C])]
        //
        //     [(E, []),   // `E` taken from successors of `D`, pushed to stack
        //      (D, []),
        //      (B, []),
        //      (A, [C])]
        //
        // Now that the top of the stack has no successors we can traverse, each item will
        // be popped off during iteration until we get back to `A`. This yeilds [E, D, B].
        //
        // When we yield `B` and call `traverse_successor`, we push `C` to the stack, but
        // since we've already visited `E`, that child isn't added to the stack. The last
        // two iterations yield `C` and finally `A` for a final traversal of [E, D, B, C, A]
        loop {
            let bb = if let Some(&mut (_, ref mut iter)) = self.visit_stack.last_mut() {
                if let Some(bb) = iter.next() {
                    bb
                } else {
                    break;
                }
            } else {
                break;
            };

            if !self.visited.get(bb.index()).unwrap_or(false) {
                self.visited.set(bb.index(), true);

                if let Some(ref term) = self.mar[bb].terminator {
                    let succs = term.successors().into_iter();
                    self.visit_stack.push((bb, succs));
                }
            }
        }
    }
}

pub fn postorder<'a>(mar: &'a Mar) -> Postorder<'a> {
    Postorder::new(mar, START_BLOCK)
}

impl<'a> Iterator for Postorder<'a> {
    type Item = (BasicBlock, &'a BasicBlockData);

    fn next(&mut self) -> Option<(BasicBlock, &'a BasicBlockData)> {
        let next = self.visit_stack.pop();
        if next.is_some() {
            self.traverse_successor();
        }

        next.map(|(bb, _)| (bb, &self.mar[bb]))
    }
}

/// Reverse postorder traversal of a graph
///
/// Reverse postorder is the reverse order of a postorder traversal.
/// This is different to a preorder traversal and represents a natural
/// linearisation of control-flow.
///
/// ```text
///
///         A
///        / \
///       /   \
///      B     C
///       \   /
///        \ /
///         D
/// ```
///
/// A reverse postorder traversal of this graph is either `A B C D` or `A C B D`
/// Note that for a graph containing no loops (i.e. A DAG), this is equivalent to
/// a topological sort.
///
/// Construction of a `ReversePostorder` traversal requires doing a full
/// postorder traversal of the graph, therefore this traversal should be
/// constructed as few times as possible. Use the `reset` method to be able
/// to re-use the traversal
#[derive(Clone)]
pub struct ReversePostorder<'a: 'a> {
    mar: &'a Mar,
    blocks: Vec<BasicBlock>,
    idx: usize
}

impl<'a> ReversePostorder<'a> {
    pub fn new(mar: &'a Mar, root: BasicBlock) -> ReversePostorder<'a> {
        let blocks : Vec<_> = Postorder::new(mar, root).map(|(bb, _)| bb).collect();

        let len = blocks.len();

        ReversePostorder {
            mar: mar,
            blocks: blocks,
            idx: len
        }
    }

    pub fn reset(&mut self) {
        self.idx = self.blocks.len();
    }
}


pub fn reverse_postorder(mar: &Mar) -> ReversePostorder {
    ReversePostorder::new(mar, START_BLOCK)
}

impl<'a> Iterator for ReversePostorder<'a> {
    type Item = (BasicBlock, &'a BasicBlockData);

    fn next(&mut self) -> Option<(BasicBlock, &'a BasicBlockData)> {
        if self.idx == 0 { return None; }
        self.idx -= 1;

        self.blocks.get(self.idx).map(|&bb| (bb, &self.mar[bb]))
    }
}
*/
