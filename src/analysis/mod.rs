use mir::{self, Mir, Location};
use std::env;
use std::fmt::Debug;
use ty::TyCtxt;
use data_structures::indexed_vec::Idx;

use self::dataflow::{BitDenotation};
use self::dataflow::{DataflowOperator};
use self::dataflow::{Dataflow, DataflowAnalysis, DataflowResults};
use self::dataflow::{MaybeInitializedLvals, MaybeUninitializedLvals};
use self::dataflow::{DefinitelyInitializedLvals};
use self::gather_moves::{MoveData, MovePathIndex, LookupResult};

mod abs_domain;
mod dataflow;
mod gather_moves;
//mod move_data;

pub fn analyze(tcx: TyCtxt, mir: &Mir) {
    let move_data = MoveData::gather_moves(mir, tcx);
    let mdpe = MoveDataParamEnv { move_data: move_data };

    let flow_inits = do_dataflow(tcx, mir, &mdpe, MaybeInitializedLvals::new(tcx, mir));
    /*
    let flow_uninits = do_dataflow(tcx, mir, &(), MaybeUninitializedLvals::new(tcx, mir));
    let flow_def_inits=  do_dataflow(tcx, mir, &(), DefinitelyInitializedLvals::new(tcx, mir));
    */

    for bb in mir.basic_blocks().indices() {
        let live = flow_inits.sets().on_entry_set_for(bb.index());
        println!("live: {:?}", live);
        /*
        let dead = flow_uninits.sets().on_entry_set_for(bb.index());
        let def_live = flow_def_inits.sets().on_entry_set_for(bb.index());
        */

        /*
        println!("maybe live: {:?} => {:?}",
                 bb,
                 mir.local_decls.iter_enumerated()
                    .filter(|&(local, _)| live.contains(&local))
                    .map(|(_, local_data)| local_data.name)
                    .collect::<Vec<_>>());
                    */

        /*
        println!("maybe dead: {:?} => {:?}",
                 bb,
                 mir.local_decls.iter_enumerated()
                    .filter(|&(local, _)| dead.contains(&local))
                    .map(|(_, local_data)| local_data.name)
                    .collect::<Vec<_>>());

        println!("live: {:?} => {:?}",
                 bb,
                 mir.local_decls.iter_enumerated()
                    .filter(|&(local, _)| live.contains(&local) && dead.contains(&local))
                    .map(|(_, local_data)| local_data.name)
                    .collect::<Vec<_>>());

        println!("def live: {:?} => {:?}",
                 bb,
                 mir.local_decls.iter_enumerated()
                    .filter(|&(local, _)| def_live.contains(&local))
                    .map(|(_, local_data)| local_data.name)
                    .collect::<Vec<_>>());
        */

        println!("");
    }
    panic!("analysis done");
}

fn do_dataflow<BD>(tcx: TyCtxt,
                   mir: &Mir,
                   ctxt: &BD::Ctxt,
                   bd: BD) -> DataflowResults<BD>
    where BD: BitDenotation<Idx=MovePathIndex, Ctxt=MoveDataParamEnv> + DataflowOperator
{
    let print_preflow_to = env::var("STATEFUL_BORROWCK_GRAPHVIZ_PREFLOW").ok();
    let print_postflow_to = env::var("STATEFUL_BORROWCK_GRAPHVIZ_POSTFLOW").ok();

    let mut mbcx = MirBorrowckCtxtPreDataflow {
        print_preflow_to: print_preflow_to,
        print_postflow_to: print_postflow_to,
        flow_state: DataflowAnalysis::new(mir, ctxt, bd),
    };

    mbcx.dataflow(|ctxt, i| &ctxt.move_data.move_paths[i]);
    mbcx.flow_state.results()
}

pub struct MirBorrowckCtxtPreDataflow<'a, BD>
    where BD: BitDenotation, BD::Ctxt: 'a
{

    flow_state: DataflowAnalysis<'a, BD>,
    print_preflow_to: Option<String>,
    print_postflow_to: Option<String>,
}

pub struct MoveDataParamEnv {
    move_data: MoveData,
    //param_env: ty::ParameterEnvironment<'tcx>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum DropFlagState {
    Present, // i.e. initialized
    Absent, // i.e. deinitialized or "moved"
}

impl DropFlagState {
    fn value(self) -> bool {
        match self {
            DropFlagState::Present => true,
            DropFlagState::Absent => false
        }
    }
}

fn drop_flag_effects_for_function_entry<'a, 'tcx, F>(
    tcx: TyCtxt<'a, 'tcx>,
    mir: &Mir,
    ctxt: &MoveDataParamEnv,
    mut callback: F)
    where F: FnMut(MovePathIndex, DropFlagState)
{
    let move_data = &ctxt.move_data;
    for arg in mir.args_iter() {
        let lvalue = mir::Lvalue::Local(arg);
        let lookup_result = move_data.rev_lookup.find(&lvalue);
        on_lookup_result_bits(tcx, mir, move_data,
                              lookup_result,
                              |moi| callback(moi, DropFlagState::Present));
    }
}

/// When enumerating the child fragments of a path, don't recurse into
/// paths (1.) past arrays, slices, and pointers, nor (2.) into a type
/// that implements `Drop`.
///
/// Lvalues behind references or arrays are not tracked by elaboration
/// and are always assumed to be initialized when accessible. As
/// references and indexes can be reseated, trying to track them can
/// only lead to trouble.
///
/// Lvalues behind ADT's with a Drop impl are not tracked by
/// elaboration since they can never have a drop-flag state that
/// differs from that of the parent with the Drop impl.
///
/// In both cases, the contents can only be accessed if and only if
/// their parents are initialized. This implies for example that there
/// is no need to maintain separate drop flags to track such state.
///
/// FIXME: we have to do something for moving slice patterns.
fn lvalue_contents_drop_state_cannot_differ<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx>,
                                                      mir: &Mir,
                                                      lv: &mir::Lvalue) -> bool {
    false
    /*
    let ty = lv.ty(mir, tcx).to_ty(tcx);
    match ty.sty {
        ty::TyArray(..) | ty::TySlice(..) | ty::TyRef(..) | ty::TyRawPtr(..) => {
            debug!("lvalue_contents_drop_state_cannot_differ lv: {:?} ty: {:?} refd => true",
                   lv, ty);
            true
        }
        ty::TyAdt(def, _) if def.has_dtor() || def.is_union() => {
            debug!("lvalue_contents_drop_state_cannot_differ lv: {:?} ty: {:?} Drop => true",
                   lv, ty);
            true
        }
        _ => {
            false
        }
    }
    */
}

fn on_lookup_result_bits<'a, 'tcx, F>(
    tcx: TyCtxt<'a, 'tcx>,
    mir: &Mir,
    move_data: &MoveData,
    lookup_result: LookupResult,
    each_child: F)
    where F: FnMut(MovePathIndex)
{
    match lookup_result {
        LookupResult::Parent(..) => {
            // access to untracked value - do not touch children
        }
        LookupResult::Exact(e) => {
            on_all_children_bits(tcx, mir, move_data, e, each_child)
        }
    }
}

fn on_all_children_bits<'a, 'tcx, F>(
    tcx: TyCtxt<'a, 'tcx>,
    mir: &Mir,
    move_data: &MoveData,
    move_path_index: MovePathIndex,
    mut each_child: F)
    where F: FnMut(MovePathIndex)
{
    fn is_terminal_path<'a, 'tcx>(
        tcx: TyCtxt<'a, 'tcx>,
        mir: &Mir,
        move_data: &MoveData,
        path: MovePathIndex) -> bool
    {
        lvalue_contents_drop_state_cannot_differ(
            tcx, mir, &move_data.move_paths[path].lvalue)
    }

    fn on_all_children_bits<'a, 'tcx, F>(
        tcx: TyCtxt<'a, 'tcx>,
        mir: &Mir,
        move_data: &MoveData,
        move_path_index: MovePathIndex,
        each_child: &mut F)
        where F: FnMut(MovePathIndex)
    {
        each_child(move_path_index);

        if is_terminal_path(tcx, mir, move_data, move_path_index) {
            return
        }

        let mut next_child_index = move_data.move_paths[move_path_index].first_child;
        while let Some(child_index) = next_child_index {
            on_all_children_bits(tcx, mir, move_data, child_index, each_child);
            next_child_index = move_data.move_paths[child_index].next_sibling;
        }
    }
    on_all_children_bits(tcx, mir, move_data, move_path_index, &mut each_child);
}

fn drop_flag_effects_for_location<'a, 'tcx, F>(
    tcx: TyCtxt<'a, 'tcx>,
    mir: &Mir,
    ctxt: &MoveDataParamEnv,
    loc: Location,
    mut callback: F)
    where F: FnMut(MovePathIndex, DropFlagState)
{
    let move_data = &ctxt.move_data;
    //let param_env = &ctxt.param_env;
    debug!("drop_flag_effects_for_location({:?})", loc);

    // first, move out of the RHS
    for mi in &move_data.loc_map[loc] {
        let path = mi.move_path_index(move_data);
        debug!("moving out of path {:?}", move_data.move_paths[path]);

        /*
        // don't move out of non-Copy things
        let lvalue = &move_data.move_paths[path].lvalue;
        let ty = lvalue.ty(mir, tcx).to_ty(tcx);
        if !ty.moves_by_default(tcx, param_env, DUMMY_SP) {
            continue;
        }
        */

        on_all_children_bits(tcx, mir, move_data,
                             path,
                             |moi| callback(moi, DropFlagState::Absent))
    }

    let block = &mir[loc.block];
    match block.statements.get(loc.statement_index) {
        Some(stmt) => match stmt.kind {
            /*
            mir::StatementKind::SetDiscriminant{ .. } => {
                span_bug!(&self.tcx,
                          stmt.source_info.span,
                          "SetDiscrimant should not exist during borrowck");
            }
            */
            mir::StatementKind::Assign(ref lvalue, _) => {
                debug!("drop_flag_effects: assignment {:?}", stmt);
                 on_lookup_result_bits(tcx, mir, move_data,
                                       move_data.rev_lookup.find(lvalue),
                                       |moi| callback(moi, DropFlagState::Present))
            }
            /*
            mir::StatementKind::StorageLive(_) |
            mir::StatementKind::StorageDead(_) |
            mir::StatementKind::Nop => {}
            */
            _ => {}
        },
        None => {
            debug!("drop_flag_effects: replace {:?}", block.terminator());
            /*
            match block.terminator().kind {
                mir::TerminatorKind::DropAndReplace { ref location, .. } => {
                    on_lookup_result_bits(tcx, mir, move_data,
                                          move_data.rev_lookup.find(location),
                                          |moi| callback(moi, DropFlagState::Present))
                }
                _ => {
                    // other terminators do not contain move-ins
                }
            }
            */
        }
    }
}
