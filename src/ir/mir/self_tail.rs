//! Which functions run their tail calls to themselves as a loop.
//!
//! A function whose body has a tail call to itself reuses its frame: the
//! call stores its arguments into the parameters and branches back to the
//! function's entry. This pass marks such functions on
//! [`MirFnRepr::self_tail_loop`](super::program::MirFnRepr::self_tail_loop);
//! the wasm-gc emitter (`codegen/wasm_gc/self_tail_loop.rs`) lowers the
//! marked ones to the loop and the certificate plan carries the mark
//! (`Grammar.FnPlan.selfLoop`), so neither decides again.

use super::expr::{MirExpr, walk_children};
use super::program::MirProgram;
use crate::ir::FnId;

/// Mark every function of `program` that tail-calls itself. With
/// `AVER_NO_SELF_TAIL_LOOPS` set, none is.
pub fn mark(program: &mut MirProgram) {
    let off = std::env::var_os("AVER_NO_SELF_TAIL_LOOPS").is_some();
    for (id, f) in program.fns.iter_mut() {
        f.repr.self_tail_loop = !off && tail_calls(&f.body.node, *id);
    }
}

fn tail_calls(e: &MirExpr, target: FnId) -> bool {
    if let MirExpr::TailCall(tc) = e
        && tc.node.target == target
    {
        return true;
    }
    let mut found = false;
    walk_children(e, &mut |c| found = found || tail_calls(c, target));
    found
}
