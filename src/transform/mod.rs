// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use mir::Mir;
use std::borrow::Cow;
use std::fmt;
use ty::TyCtxt;

pub mod dump_mir;
pub mod simplify_cfg;
pub mod validate;

/// Various information about pass.
pub trait Pass {
    fn name(&self) -> Cow<'static, str>;
    fn disambiguator<'a>(&'a self) -> Option<Box<fmt::Display+'a>> { None }
}

/// A pass which inspects the whole Mir map.
pub trait MirMapPass: Pass {
    fn run_pass<'a, 'tcx>(
        &mut self,
        tcx: TyCtxt<'a, 'tcx>,
        mir: &mut Mir,
        hooks: &mut [Box<MirPassHook>]);
}

pub trait MirPassHook: Pass {
    fn on_mir_pass<'a, 'tcx>(
        &mut self,
        tcx: TyCtxt<'a, 'tcx>,
        mir: &Mir,
        pass: &Pass,
        is_after: bool
    );
}

/// A pass which inspects Mir of functions in isolation.
pub trait MirPass: Pass {
    fn run_pass<'a, 'tcx>(&mut self,
                    tcx: TyCtxt<'a, 'tcx>,
                    mir: &mut Mir);
}

impl<T: MirPass> MirMapPass for T {
    fn run_pass<'a, 'tcx>(&mut self,
                          tcx: TyCtxt<'a, 'tcx>,
                          mir: &mut Mir,
                          hooks: &mut [Box<MirPassHook>])
    {
        for hook in &mut *hooks {
            hook.on_mir_pass(tcx, mir, self, false);
        }
        MirPass::run_pass(self, tcx, mir);
        for hook in &mut *hooks {
            hook.on_mir_pass(tcx, mir, self, true);
        }
    }
}

/// A manager for MIR passes.
pub struct Passes {
    passes: Vec<Box<MirMapPass>>,
    pass_hooks: Vec<Box<MirPassHook>>,
}

impl Passes {
    pub fn new() -> Self {
        Passes {
            passes: vec![],
            pass_hooks: vec![],
        }
    }

    pub fn run_passes<'a, 'tcx>(&mut self,
                                tcx: TyCtxt<'a, 'tcx>,
                                mir: &mut Mir) {
        for pass in &mut self.passes {
            pass.run_pass(tcx, mir, &mut self.pass_hooks);
        }
    }

    pub fn push_pass(&mut self, pass: Box<MirMapPass>) {
        self.passes.push(pass);
    }

    pub fn push_hook(&mut self, hook: Box<MirPassHook>) {
        self.pass_hooks.push(hook);
    }
}
