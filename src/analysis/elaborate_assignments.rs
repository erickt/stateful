// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::{BTreeMap, BTreeSet};
use super::dataflow::DataflowResults;
use super::dataflow::DefinitelyInitializedLvals;
use super::gather_moves::HasMoveData;
use super::MoveDataParamEnv;
use super::{MoveData, MovePathIndex};
use syntax::codemap::Span;
use ty::TyCtxt;

pub struct DefiniteAssignment {
    initialized: BTreeMap<BasicBlock, BTreeSet<Local>>,
    assigned_on_entry: BTreeMap<BasicBlock, BTreeSet<Local>>,
}

impl DefiniteAssignment {
    /// Return all the locals that were initialized in this block.
    pub fn initialized(&self, block: BasicBlock) -> Option<&BTreeSet<Local>> {
        self.initialized.get(&block)
    }

    /// Return all the locals that were alive on entry to this block.
    pub fn on_entry(&self, block: BasicBlock) -> Option<&BTreeSet<Local>> {
        self.assigned_on_entry.get(&block)
    }
}

/// Use the definite-assignment algorithm to find all the locations where a local
pub fn analyze_assignments<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx>,
                                     mir: &'a Mir) -> DefiniteAssignment {
    let move_data = MoveData::gather_moves(mir, tcx);
    let env = MoveDataParamEnv { move_data: move_data };

    // Figure out when variables are initialized or assigned.
    /*
    let flow_maybe_inits = super::do_dataflow(tcx, mir,
                                              MaybeInitializedLvals::new(tcx, mir, &env),
                                              |bd, p| &bd.move_data().move_paths[p]);
    */
    let flow_inits = super::do_dataflow(tcx, mir,
                                        DefinitelyInitializedLvals::new(tcx, mir, &env),
                                        |bd, p| &bd.move_data().move_paths[p]);

    /*
    // `flow_inits` works with `MoveIndex`s not locals, so we need a mapping from one to the other.
    let move_indices = mir.local_decls
        .indices()
        .filter_map(|local| {
            let lvalue = Lvalue::Local(local);

            match env.move_data.rev_lookup.find(&lvalue) {
                LookupResult::Exact(idx) |
                LookupResult::Parent(Some(idx)) => Some((idx, local)),
                LookupResult::Parent(None) => {
                    span_bug!(tcx, mir.span, "analysis thinks {:?} is a static?", local);
                }
            }
        })
        .collect::<Vec<_>>();

    for (block, block_data) in mir.basic_blocks().iter_enumerated() {
        let entry_set = flow_inits.sets().on_entry_set_for(block.index());
        let gen_set = flow_inits.sets().gen_set_for(block.index());
        let kill_set = flow_inits.sets().kill_set_for(block.index());

        println!("init entry: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if entry_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("init gen: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if gen_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("init kill: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if kill_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        /*
        let entry_set = flow_uninits.sets().on_entry_set_for(block.index());
        let gen_set = flow_uninits.sets().gen_set_for(block.index());
        let kill_set = flow_uninits.sets().kill_set_for(block.index());

        println!("uninit entry: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if entry_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("uninit gen: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if gen_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("uninit kill: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if kill_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());
        */

        let entry_set = flow_definits.sets().on_entry_set_for(block.index());
        let gen_set = flow_definits.sets().gen_set_for(block.index());
        let kill_set = flow_definits.sets().kill_set_for(block.index());

        println!("definit entry: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if entry_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("definit gen: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if gen_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("definit kill: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if kill_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());
        println!();
    }
    */

    ElaborateAssignmentsCtxt {
        tcx: tcx,
        mir: mir,
        env: &env,
        //flow_maybe_inits: flow_maybe_inits,
        flow_inits: flow_inits,
        initialized: BTreeMap::new(),
        assigned_on_entry: BTreeMap::new(),
    }.elaborate()
}

struct ElaborateAssignmentsCtxt<'a, 'tcx: 'a> {
    tcx: TyCtxt<'a, 'tcx>,
    mir: &'a Mir,
    env: &'a MoveDataParamEnv,
    //flow_maybe_inits: DataflowResults<MaybeInitializedLvals<'a, 'tcx>>,
    flow_inits: DataflowResults<DefinitelyInitializedLvals<'a, 'tcx>>,
    initialized: BTreeMap<BasicBlock, BTreeSet<Local>>,
    assigned_on_entry: BTreeMap<BasicBlock, BTreeSet<Local>>,
}

impl<'b, 'tcx> ElaborateAssignmentsCtxt<'b, 'tcx> {
    fn move_data(&self) -> &'b MoveData { &self.env.move_data }

    fn elaborate(mut self) -> DefiniteAssignment {
        self.elaborate_assignments();

        debug!("initialized: {:#?}", self.initialized);
        debug!("assigned: {:#?}", self.assigned_on_entry);

        DefiniteAssignment {
            initialized: self.initialized,
            assigned_on_entry: self.assigned_on_entry,
        }
    }

    fn get_path_local(&self, span: Span, path: MovePathIndex) -> Local {
        let move_path = &self.move_data().move_paths[path];
        assert!(move_path.parent.is_none());

        match move_path.lvalue {
            Lvalue::Local(local) => local,
            _ => {
                span_bug!(&self.tcx, span, "no local in move path {:?}", move_path);
            }
        }
    }

    fn elaborate_assignments(&mut self) {
        // FIXME: We aren't yet properly handling dynamic drops properly. For example, say we
        // had this code:
        //
        // ```rust
        // {
        //     let x = String::new();
        //     if pred {
        //         mem::drop(x);
        //     } else {
        //         // place 1
        //     }
        //
        //     ...
        //
        //     // place 2
        // }
        // ```
        //
        // In regular Rust, the compiler conditionally drop the variable `x` at "place 2" if the
        // `pred` is false. Unfortunately the way Stateful is written it will actually insert the
        // drop in "place 1". To do this properly, the simplest way of doing this is to just
        // reimplement the dynamic drop semantics, where we add an `Option` type that will
        // conditionally have `x` moved into it, and have it be dropped at the end of the scope.
        //
        // We can do this by way of using the `flow_maybe_inits`. It has a bit set for all locals
        // that could possibly be initialized at any given point. If we take that set and subtract
        // `flow_inits` from it then we'll get all the locals that need conditional dropping.

        for (block, block_data) in self.mir.basic_blocks().iter_enumerated() {
            /*
            let maybe_init_entry_set = self.flow_maybe_inits.sets().on_entry_set_for(block.index());
            let maybe_init_gen_set = self.flow_maybe_inits.sets().gen_set_for(block.index());
            let maybe_init_kill_set = self.flow_maybe_inits.sets().kill_set_for(block.index());
            */

            let init_entry_set = self.flow_inits.sets().on_entry_set_for(block.index());
            let init_gen_set = self.flow_inits.sets().gen_set_for(block.index());
            let init_kill_set = self.flow_inits.sets().kill_set_for(block.index());

            // First, add all the locals that are defined on entry into this block.
            for path in init_entry_set.iter() {
                let local = self.get_path_local(block_data.span, path);
                self.assigned_on_entry.entry(block)
                    .or_insert_with(BTreeSet::new)
                    .insert(local);
            }

            for path in init_gen_set.iter() {
                let local = self.get_path_local(block_data.span, path);
                self.initialized.entry(block)
                    .or_insert_with(BTreeSet::new)
                    .insert(local);
            }

            for path in init_kill_set.iter() {
                if init_entry_set.contains(&path) { continue; }

                let local = self.get_path_local(block_data.span, path);
                let lvalue = Lvalue::Local(local);

                let mut assigned = false;
                for stmt in block_data.statements() {
                    match stmt.kind {
                        StatementKind::Stmt(_) |
                        StatementKind::StorageLive(_) |
                        StatementKind::StorageDead(_) => {}
                        StatementKind::Let { lvalues: ref destinations, .. } => {
                            for destination in destinations {
                                assigned |= lvalue == *destination;
                            }
                        }
                        StatementKind::Assign(ref destination, _) |
                        StatementKind::Call { ref destination, .. } |
                        StatementKind::MethodCall { ref destination, .. } => {
                            assigned |= lvalue == *destination;
                        }
                    }
                }

                if assigned {
                    self.initialized.entry(block)
                        .or_insert_with(BTreeSet::new)
                        .insert(local);
                }
            }
        }
    }
}