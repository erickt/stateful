// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! This pass just dumps MIR at a specified point.

use data_structures::indexed_vec::Idx;
use mir::Mir;
use std::borrow::Cow;
use super::{MirPassHook, Pass};
use ty::TyCtxt;

pub struct Validate;

impl MirPassHook for Validate {
    fn on_mir_pass<'a, 'tcx>(
        &mut self,
        tcx: TyCtxt<'a, 'tcx>,
        mir: &Mir,
        _pass: &Pass,
        _is_after: bool)
    {
        let basic_blocks = mir.basic_blocks();
        for (bb, block) in basic_blocks.iter_enumerated() {
            let terminator = block.terminator();

            for succ in terminator.successors() {
                if succ.index() >= basic_blocks.len() {
                    span_bug!(
                        &tcx,
                        mir.span,
                        "block {:?} terminator does not exist: {:?} len: {:?}",
                        bb,
                        terminator.kind,
                        basic_blocks.len());
                }
            }
        }
    }
}

impl<'b> Pass for Validate {
    fn name(&self) -> Cow<'static, str> { 
        Cow::from("Validate")
    }
}
