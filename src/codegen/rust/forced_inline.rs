//! Which thin functions the Rust backend forces into their callers.
//!
//! An effect-free thin function gets an inline hint. `#[inline(always)]`
//! overrides LLVM's cost model, so it is given only where the code it pastes
//! into a caller is bounded: the function's body, with the bodies of the
//! forced functions it calls pasted in, must stay within
//! [`FORCED_INLINE_BUDGET`] MIR nodes. Every other thin function gets
//! `#[inline]`, and LLVM inlines it where its cost model finds it pays.
//!
//! Measuring the pasted body, not the function's own body, is what stops the
//! cascade: forcing a body that calls two forced functions, each calling two
//! more, copies the bottom of the chain once per path through it, which
//! doubles per level. The budget caps what one call site can receive, however
//! the chain is shaped.

use std::collections::{HashMap, HashSet};

use crate::codegen::CodegenContext;
use crate::ir::hir::classify_thin_fn_def_resolved;
use crate::ir::mir::{MirCallee, MirExpr, MirProgram};
use crate::ir::{FnId, thin_kind_is_parent_thin_candidate};

/// The most MIR nodes a forced function may paste into one call site,
/// counting the forced functions it calls in turn. The per-pixel kernel of
/// `tools/edge/bench.av` (escape time, dither, palette index) pastes 124 and
/// runs 7% slower when left to LLVM, so the budget keeps a kernel of that size
/// forced; a dispatch `match` over many arms, or a chain that fans out over
/// a few levels, passes it.
pub(super) const FORCED_INLINE_BUDGET: usize = 128;

/// The thin functions to mark `#[inline(always)]`.
pub(super) fn compute(ctx: &CodegenContext) -> HashSet<FnId> {
    let Some(program) = ctx.mir_program.as_ref() else {
        return HashSet::new();
    };
    let is_user_type = |name: &str| crate::codegen::common::is_user_type(name, ctx);
    let candidates: HashSet<FnId> = program
        .iter()
        .filter_map(|(id, _)| {
            let fd = ctx.resolved_program.fn_by_id(*id)?;
            let thin = classify_thin_fn_def_resolved(fd, &is_user_type)
                .is_some_and(|plan| thin_kind_is_parent_thin_candidate(plan.kind));
            (fd.effects.is_empty() && thin).then_some(*id)
        })
        .collect();
    let mut sizes = Sizes {
        program,
        candidates: &candidates,
        memo: HashMap::new(),
    };
    candidates
        .iter()
        .copied()
        .filter(|id| sizes.pasted(*id).is_some())
        .collect()
}

enum Size {
    /// Being measured: a call back into it is a cycle, never forced.
    Open,
    /// Measured: the pasted size when forced, `None` when not forced.
    Done(Option<usize>),
}

struct Sizes<'a> {
    program: &'a MirProgram,
    candidates: &'a HashSet<FnId>,
    memo: HashMap<FnId, Size>,
}

impl Sizes<'_> {
    /// The nodes forcing `id` pastes into a caller, or `None` when `id` is
    /// not forced.
    fn pasted(&mut self, id: FnId) -> Option<usize> {
        match self.memo.get(&id) {
            Some(Size::Done(size)) => return *size,
            Some(Size::Open) => return None,
            None => {}
        }
        if !self.candidates.contains(&id) {
            self.memo.insert(id, Size::Done(None));
            return None;
        }
        self.memo.insert(id, Size::Open);
        let size = self.measure(id);
        self.memo.insert(id, Size::Done(size));
        size
    }

    fn measure(&mut self, id: FnId) -> Option<usize> {
        let body = &self.program.fn_by_id(id)?.body.node;
        let mut nodes = 0;
        let mut callees = Vec::new();
        let mut stack = vec![body];
        while let Some(node) = stack.pop() {
            nodes += 1;
            if nodes > FORCED_INLINE_BUDGET {
                return None;
            }
            match node {
                MirExpr::Call(call) => {
                    if let MirCallee::Fn(callee) = call.node.callee {
                        callees.push(callee);
                    }
                }
                MirExpr::TailCall(call) => callees.push(call.node.target),
                _ => {}
            }
            crate::ir::mir::expr::walk_children(node, &mut |child| stack.push(child));
        }
        for callee in callees {
            nodes += self.pasted(callee).unwrap_or(0);
            if nodes > FORCED_INLINE_BUDGET {
                return None;
            }
        }
        Some(nodes)
    }
}
