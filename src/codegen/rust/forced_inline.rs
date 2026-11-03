//! Which functions the Rust backend forces into their callers.
//!
//! An effect-free thin function gets an inline hint. `#[inline(always)]`
//! overrides LLVM's cost model, so it is given only where the code it pastes
//! into a caller is bounded: the function's body, with the bodies of the
//! forced functions it calls pasted in, must stay within
//! [`FORCED_INLINE_BUDGET`] MIR nodes. Every other thin function gets
//! `#[inline]`, and LLVM inlines it where its cost model finds it pays.
//!
//! A leaf classifier ([`is_leaf_classifier`]) is forced on the same budget
//! though it is not thin: a function that reads a character or a byte,
//! compares, and answers a literal, such as a digit's value or whether a
//! position holds a brace. Its body is a match over many literals, which
//! LLVM's cost model prices as a large function and leaves out of line, so
//! a reader pays a call per character it classifies. Pasted in, the match
//! meets the caller's own tests on the same character, and its arms are
//! branches on a value already in a register. Nothing it pastes allocates
//! or calls back into the program, so the budget bounds the paste exactly.
//!
//! Measuring the pasted body, not the function's own body, is what stops the
//! cascade: forcing a body that calls two forced functions, each calling two
//! more, copies the bottom of the chain once per path through it, which
//! doubles per level. The budget caps what one call site can receive, however
//! the chain is shaped.

use std::collections::{HashMap, HashSet};

use crate::codegen::CodegenContext;
use crate::ir::hir::classify_thin_fn_def_resolved;
use crate::ir::mir::{MirCallee, MirExpr};
use crate::ir::{FnId, thin_kind_is_parent_thin_candidate};

/// The most MIR nodes a forced function may paste into one call site,
/// counting the forced functions it calls in turn. The per-pixel kernel of
/// `tools/edge/bench.av` (escape time, dither, palette index) pastes 124 and
/// runs 7% slower when left to LLVM, so the budget keeps a kernel of that size
/// forced; a dispatch `match` over many arms, or a chain that fans out over
/// a few levels, passes it.
pub(super) const FORCED_INLINE_BUDGET: usize = 128;

/// The thin functions to mark `#[inline(always)]`.
///
/// A function on a cycle through two or more thin functions is never forced:
/// inlining cannot finish there. A call of a function to itself pastes
/// nothing (a self tail call is a loop in the emitted Rust). Every other size
/// is a fold down an acyclic graph, so the answer does not depend on the
/// order the functions are looked at, and the generated code is the same on
/// every run.
pub(super) fn compute(ctx: &CodegenContext) -> HashSet<FnId> {
    let Some(program) = ctx.mir_program.as_ref() else {
        return HashSet::new();
    };
    let is_user_type = |name: &str| crate::codegen::common::is_user_type(name, ctx);
    let mut candidates: Vec<FnId> = program
        .iter()
        .filter_map(|(id, f)| {
            let fd = ctx.resolved_program.fn_by_id(*id)?;
            let thin = classify_thin_fn_def_resolved(fd, &is_user_type)
                .is_some_and(|plan| thin_kind_is_parent_thin_candidate(plan.kind));
            let leaf = is_leaf_classifier(&f.body.node, &program.builtins);
            (fd.effects.is_empty() && (thin || leaf)).then_some(*id)
        })
        .collect();
    candidates.sort();
    let candidate_set: HashSet<FnId> = candidates.iter().copied().collect();
    let bodies: HashMap<FnId, Body> = candidates
        .iter()
        .filter_map(|id| Some((*id, Body::of(&program.fn_by_id(*id)?.body.node))))
        .collect();
    let graph: HashMap<FnId, Vec<FnId>> = bodies
        .iter()
        .map(|(&id, body)| {
            let callees = body
                .callees
                .iter()
                .copied()
                .filter(|callee| *callee != id && candidate_set.contains(callee))
                .collect();
            (id, callees)
        })
        .collect();
    let cyclic: HashSet<FnId> = crate::scc::tarjan_sccs(&candidates, &graph)
        .into_iter()
        .filter(|component| component.len() > 1)
        .flatten()
        .collect();
    let mut sizes = Sizes {
        bodies: &bodies,
        cyclic: &cyclic,
        memo: HashMap::new(),
    };
    candidates
        .into_iter()
        .filter(|id| sizes.pasted(*id).is_some())
        .collect()
}

/// Is `body` a leaf classifier: a value computed from character and byte
/// reads, integer arithmetic, comparisons, matches and literals alone? It
/// calls no function of the program, builds nothing on the heap, and runs
/// no effect, so pasting it into a caller copies only branches.
pub(super) fn is_leaf_classifier(body: &MirExpr, builtins: &[String]) -> bool {
    use crate::ir::hir::BuiltinIntrinsic as I;
    let mut leaf = true;
    let mut stack = vec![body];
    while let Some(node) = stack.pop() {
        let fine = match node {
            MirExpr::Literal(_)
            | MirExpr::Local(_)
            | MirExpr::Let(_)
            | MirExpr::BinOp(_)
            | MirExpr::Neg(_)
            | MirExpr::Match(_)
            | MirExpr::IfThenElse(_)
            | MirExpr::Project(_)
            | MirExpr::Box(_)
            | MirExpr::Unbox(_) => true,
            MirExpr::Call(call) => match &call.node.callee {
                MirCallee::Intrinsic(
                    I::IntDivEuclid
                    | I::IntModEuclid
                    | I::BitsShiftLeft
                    | I::BitsShiftRight
                    | I::BitsLow
                    | I::BitsPopCount
                    | I::StrCursorEnd
                    | I::StrCursorHead
                    | I::StrCursorNext
                    | I::StrCursorCode
                    | I::StrCode1
                    | I::StrCode1Lower
                    | I::StrCode1Upper
                    | I::StrFoldLower
                    | I::StrFoldUpper
                    | I::StrIndexCodeAt
                    | I::StrIndexLen
                    | I::StrIndexPosition
                    | I::PackedByteAt,
                ) => true,
                MirCallee::Builtin(id) => matches!(
                    builtins.get(id.0 as usize).map(String::as_str),
                    Some(
                        "Int.min"
                            | "Int.max"
                            | "Int.abs"
                            | "Int.mod"
                            | "Int.div"
                            | "Bits.and"
                            | "Bits.or"
                            | "Bits.xor"
                            | "Bits.not"
                            | "Bool.and"
                            | "Bool.or"
                            | "Bool.not"
                            | "String.len"
                    )
                ),
                _ => false,
            },
            MirExpr::TailCall(_)
            | MirExpr::Construct(_)
            | MirExpr::RecordCreate(_)
            | MirExpr::RecordUpdate(_)
            | MirExpr::Try(_)
            | MirExpr::List(_)
            | MirExpr::Tuple(_)
            | MirExpr::MapLiteral(_)
            | MirExpr::InterpolatedStr(_)
            | MirExpr::IndependentProduct(_)
            | MirExpr::Return(_)
            | MirExpr::FnValue(_)
            | MirExpr::IntWord(_) => false,
        };
        if !fine {
            leaf = false;
            break;
        }
        crate::ir::mir::expr::walk_children(node, &mut |child| stack.push(child));
    }
    leaf
}

/// A candidate's own body: its MIR nodes (counting stops past the budget) and
/// the Aver functions it calls, once per call.
struct Body {
    nodes: usize,
    callees: Vec<FnId>,
}

impl Body {
    fn of(body: &MirExpr) -> Body {
        let mut nodes = 0;
        let mut callees = Vec::new();
        let mut stack = vec![body];
        while let Some(node) = stack.pop() {
            nodes += 1;
            if nodes > FORCED_INLINE_BUDGET {
                break;
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
        Body { nodes, callees }
    }
}

struct Sizes<'a> {
    bodies: &'a HashMap<FnId, Body>,
    cyclic: &'a HashSet<FnId>,
    memo: HashMap<FnId, Option<usize>>,
}

impl Sizes<'_> {
    /// The nodes forcing `id` pastes into a caller, or `None` when `id` is
    /// not forced.
    fn pasted(&mut self, id: FnId) -> Option<usize> {
        if let Some(size) = self.memo.get(&id) {
            return *size;
        }
        let size = self.measure(id);
        self.memo.insert(id, size);
        size
    }

    fn measure(&mut self, id: FnId) -> Option<usize> {
        if self.cyclic.contains(&id) {
            return None;
        }
        let body = self.bodies.get(&id)?;
        let mut nodes = body.nodes;
        if nodes > FORCED_INLINE_BUDGET {
            return None;
        }
        for callee in body.callees.iter().filter(|callee| **callee != id) {
            nodes += self.pasted(*callee).unwrap_or(0);
            if nodes > FORCED_INLINE_BUDGET {
                return None;
            }
        }
        Some(nodes)
    }
}
