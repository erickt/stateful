use super::{DefinitelyInitializedLvals, do_dataflow};
use super::{LookupResult, MoveData, MoveDataParamEnv};
use super::gather_moves::HasMoveData;
use data_structures::indexed_vec::{IndexVec, Idx};
use mir::*;
use std::collections::{BTreeMap, BTreeSet};
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
    let flow_inits = do_dataflow(tcx, mir,
                                 DefinitelyInitializedLvals::new(tcx, mir, &env),
                                 |bd, p| &bd.move_data().move_paths[p]);

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

    let mut predecessors = IndexVec::from_elem(vec![], mir.basic_blocks());
    for (bb, data) in mir.basic_blocks().iter_enumerated() {
        if let Some(ref term) = data.terminator {
            for &tgt in term.successors().iter() {
                predecessors[tgt].push(bb);
            }
        }
    }

    // Now we're ready. Use the flow data to figure out when a local is first initialized, and all
    // the blocks for which it is alive. In order to figure this out, we need the ENTRY set and the
    // GEN set from the dataflow. In our case, for a given block:
    //
    // * ENTRY has a bit set for each local that was initialized in a parent block.
    // * GEN has a bit set for each local initialized in this block.
    let mut initialized = BTreeMap::new();
    let mut assigned = BTreeMap::new();

    for (block, block_data) in mir.basic_blocks().iter_enumerated() {
        let entry_set = flow_inits.sets().on_entry_set_for(block.index());
        let gen_set = flow_inits.sets().gen_set_for(block.index());
        let kill_set = flow_inits.sets().kill_set_for(block.index());

        // FIXME(stateful): We need to push down the fact that match arm lvalues have been defined
        // by pulling apart the pattern. This would be best expressed implicitly though the
        // dataflow, but, well, I couldn't figure out how to get that to work.
        if let TerminatorKind::Match { ref arms, .. } = block_data.terminator().kind {
            for arm in arms {
                for lvalue in &arm.lvalues {
                    match *lvalue {
                        Lvalue::Local(local) => {
                            assigned.entry(arm.block)
                                .or_insert_with(BTreeSet::new)
                                .insert(local);
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }

        for &(idx, local) in &move_indices {
            let entry = entry_set.contains(&idx);
            let gen = gen_set.contains(&idx);
            let kill = kill_set.contains(&idx);

            // If a local is both in the entry and the gen set, then that means that the local was
            // generated externally from the state machine, such as for `Suspend`. If this is the
            // case, only mark this variable as initialized, not assigned.
            if gen {
                initialized.entry(block).or_insert_with(BTreeSet::new).insert(local);
            } else if entry {
                assigned.entry(block).or_insert_with(BTreeSet::new).insert(local);
            }

            // It's possible that if the local was killed in this block it was also initialized in
            // this block. This can happen in code like this:
            //
            // ```rust
            // {
            //     let x = String::new();
            //     // `x` is dropped
            // }
            // ```
            //
            // However, we also have the case where if a local was moved in a prior block, we still
            // might end up still having a killed local in our set. This can happen in code like
            // this:
            //
            // ```rust
            // {
            //     let x = String::new();
            //     // `x` is marked live
            //
            //     if true {
            //         mem::drop(x);
            //     } else {
            //         mem::drop(x);
            //     }
            //
            //     // `x` is marked dead at the end of the scope
            // }
            // ```
            //
            // So to initialize the variable in the first case but not the second case, just check
            // if we have a matching `StorageLive` in this block. If we do, we must have
            // initialized the local. Otherwise ignore it.
            if !entry && kill {
                for stmt in block_data.statements() {
                    match stmt.kind {
                        StatementKind::StorageLive(ref lvalue) if Lvalue::Local(local) == *lvalue => {
                            initialized.entry(block).or_insert_with(BTreeSet::new).insert(local);
                            break;
                        }
                        _ => {}
                    }
                }

                /*
                if !found {
                }



                println!("might be live or dead!: bb={:?} local={:?}", block, local);
                let mut live = true;
                for predecessor in &predecessors[block] {
                    println!("pred: {:?}", predecessor);
                    let pred_entry_set = flow_inits.sets().on_entry_set_for(predecessor.index());
                    let pred_gen_set = flow_inits.sets().gen_set_for(predecessor.index());
                    let pred_kill_set = flow_inits.sets().kill_set_for(predecessor.index());

                    println!("pred: entry: {:?}", pred_entry_set.contains(&idx));
                    println!("pred: gen: {:?}", pred_gen_set.contains(&idx));
                    println!("pred: kill: {:?}", pred_kill_set.contains(&idx));

                    if pred_kill_set.contains(&idx) {
                        println!("pred: {:?} is killed", local);
                        live = false;
                        break;
                    } else {
                        println!("pred: {:?} is not killed", local);
                    }
                }

                if live {
                    //initialized.entry(block).or_insert_with(BTreeSet::new).insert(local);
                //}
            }
                */
            }
        }

        println!("entry: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if entry_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("gen: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if gen_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());

        println!("kill: {:?} => {:?}",
                 block,
                 move_indices.iter()
                    .filter_map(|&(idx, local)| {
                        if kill_set.contains(&idx) { Some(local) } else { None }
                    })
                    .collect::<Vec<_>>());
        println!();
    }

    println!("initialized: {:#?}", initialized);
    println!("assigned: {:#?}", assigned);

    DefiniteAssignment {
        initialized: initialized,
        assigned_on_entry: assigned,
    }
}
