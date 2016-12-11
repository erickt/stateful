use mir::{self, Mir};
use std::fmt::Debug;
use ty::TyCtxt;
use data_structures::indexed_vec::Idx;

use self::dataflow::{BitDenotation};
use self::dataflow::{DataflowOperator};
use self::dataflow::{Dataflow, DataflowAnalysis, DataflowResults};
use self::dataflow::{MaybeInitializedLvals, MaybeUninitializedLvals};
use self::dataflow::{DefinitelyInitializedLvals};

mod abs_domain;
mod gather_moves;
mod dataflow;

pub fn analyze(tcx: TyCtxt, mir: &Mir) {
    let flow_inits = do_dataflow(tcx, mir, &(), MaybeInitializedLvals::new(tcx, mir));
    let flow_uninits = do_dataflow(tcx, mir, &(), MaybeUninitializedLvals::new(tcx, mir));
    let flow_def_inits=  do_dataflow(tcx, mir, &(), DefinitelyInitializedLvals::new(tcx, mir));

    println!("{:#?}", flow_inits.sets());

    for bb in mir.basic_blocks().indices() {
        let live = flow_inits.sets().on_entry_set_for(bb.index());
        let dead = flow_uninits.sets().on_entry_set_for(bb.index());
        let def_live = flow_def_inits.sets().on_entry_set_for(bb.index());

        println!("maybe live: {:?} => {:?}",
                 bb,
                 mir.local_decls.iter_enumerated()
                    .filter(|&(local, _)| live.contains(&local))
                    .map(|(_, local_data)| local_data.name)
                    .collect::<Vec<_>>());

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

        println!("");
    }
    panic!("analysis done");
}

fn do_dataflow<BD>(tcx: TyCtxt,
                   mir: &Mir,
                   ctxt: &BD::Ctxt,
                   bd: BD) -> DataflowResults<BD>
    where BD: BitDenotation<Idx=mir::Local, Ctxt=()> + DataflowOperator
{
    let mut mbcx = MirBorrowckCtxtPreDataflow {
        flow_state: DataflowAnalysis::new(mir, ctxt, bd),
    };

    mbcx.dataflow(|ctxt, i| ctxt); //&ctxt.basic_blocks[i]);
    mbcx.flow_state.results()
}

struct MirBorrowckCtxtPreDataflow<'a, BD>
    where BD: BitDenotation, BD::Ctxt: 'a
{

    flow_state: DataflowAnalysis<'a, BD>,
}
