//! IR-level analysis pass — derives policy-independent (body classification,
//! locals count) and policy-parametrized (alloc info) facts about each
//! `FnDef`. Runs as the last stage of the canonical pipeline so consumers
//! (codegen, dump, future inliner) can read derived metadata from one
//! place instead of recomputing it.
//!
//! Why a unified analysis stage instead of ad-hoc calls scattered through
//! VM compilation and WASM emission: every backend that wanted "is this
//! fn no-alloc" or "what's its body shape" had its own call into
//! `compute_alloc_info` / `classify_thin_fn_def`. Same input, same
//! computation, repeated. Centralising means: one walk per program, one
//! place to extend with new analyses, and `aver compile --emit-ir` /
//! `--explain-passes` see the same numbers the codegen does.
//!
//! Backend-specific bits stay backend-specific: `AllocPolicy` is provided
//! by the caller (`VmAllocPolicy`, `WasmAllocPolicy`, or the dump's
//! conservative `DumpAllocPolicy`). Policy-independent facts (`body_shape`,
//! `thin_kind`, `local_count`) are computed unconditionally.
//!
//! ## Scope: per-module by design (Aver module DAG invariant)
//!
//! `analyze` runs on one module at a time — the entry items, or a single
//! dep module loaded from disk. It does NOT cross module boundaries to
//! discover, say, a mutual-TCO SCC that spans two modules. This is correct
//! by Aver's module-graph invariant, not a limitation:
//!
//! - `module M depends [A, B, ...]` — depends is a static list of names,
//!   no dynamic resolution.
//! - Module loading (`load_module_recursive`) enforces a DAG via the
//!   `loaded: HashSet` cycle guard — a re-entered module name returns
//!   early, so the dep graph is acyclic by construction.
//! - In a DAG, all calls flow from dependents to dependencies: module A
//!   calls into module B if A depends on B (transitively); B never calls
//!   back into A. Mutually-recursive call chains require a cycle in the
//!   call graph; a cycle requires either (i) a self-call within one
//!   module, or (ii) a cycle in the module DAG — and the latter is
//!   forbidden.
//!
//! Consequence: every SCC of mutually-recursive functions lives entirely
//! within a single module. Per-module analysis covers every real case;
//! "cross-module mutual TCO" is mathematically impossible in Aver.
//! Backends that want a global `mutual_tco_members` set just union the
//! per-module sets they get from each module's `AnalysisResult`.
//!
//! Same logic for `recursive_fns`, `recursive_call_count`, and the
//! tail-call SCC computation that drives trampoline emission. None of
//! these need to see cross-module data.

use std::collections::{HashMap, HashSet};

use crate::ast::{FnBody, FnDef, TopLevel};
use crate::call_graph::{find_recursive_fns, tailcall_scc_components};
use crate::ir::{
    AllocPolicy, BodyExprPlan, BodyPlan, CallLowerCtx, ThinKind, classify_thin_fn_def,
};

/// Backend-neutral allocation policy useful for diagnostic dumps. Mirrors
/// the VM/WASM "pure non-alloc builtins" whitelist exactly so the
/// `[no_alloc]` annotation reads the same on both runtime backends.
/// Constructors with payloads are treated as allocating; nullary
/// constructors (`Option.None`, `Result.Ok` with primitive seed) are not.
///
/// Codegen pipelines should use the backend-specific policy
/// (`VmAllocPolicy`, `WasmAllocPolicy`); this type is for `--emit-ir`,
/// `aver bench`, and other tools that want a sensible answer without
/// committing to a target.
pub struct NeutralAllocPolicy;

impl AllocPolicy for NeutralAllocPolicy {
    fn builtin_allocates(&self, name: &str) -> bool {
        !matches!(
            name,
            "Int.abs"
                | "Int.min"
                | "Int.max"
                | "Int.toFloat"
                | "Float.abs"
                | "Float.floor"
                | "Float.ceil"
                | "Float.round"
                | "Float.min"
                | "Float.max"
                | "Float.sin"
                | "Float.cos"
                | "Float.sqrt"
                | "Float.pow"
                | "Float.atan2"
                | "Float.pi"
                | "Float.fromInt"
                | "Char.toCode"
                | "String.len"
                | "String.byteLength"
                | "String.startsWith"
                | "String.endsWith"
                | "String.contains"
                | "List.len"
                | "List.contains"
                | "Vector.len"
                | "Map.size"
                | "Map.contains"
                | "Set.size"
                | "Set.contains"
                | "Bool.and"
                | "Bool.or"
                | "Bool.not"
        )
    }
    fn constructor_allocates(&self, _name: &str, has_payload: bool) -> bool {
        has_payload
    }
}

#[derive(Debug, Clone, Default)]
pub struct AnalysisResult {
    pub fn_analyses: HashMap<String, FnAnalysis>,
    /// Names of fns that participate in a multi-member tail-call SCC
    /// (mutual recursion). Backends emit a trampoline + thin wrappers
    /// for each member; singleton self-recursive fns go through plain
    /// TCO and aren't in this set.
    pub mutual_tco_members: HashSet<String>,
    /// Names of fns that call themselves directly or transitively.
    /// Used by the type checker's flow analysis, the proof exporters,
    /// and the memo-eligibility check.
    pub recursive_fns: HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct FnAnalysis {
    /// `Some(true)` = proven to allocate under the supplied `AllocPolicy`;
    /// `Some(false)` = proven not to; `None` = no policy was configured.
    pub allocates: Option<bool>,
    pub thin_kind: Option<ThinKind>,
    pub body_shape: BodyShape,
    pub local_count: Option<u16>,
    /// `true` if this fn participates in a mutual-TCO SCC (not a singleton
    /// self-recursive fn — those are plain TCO and don't need trampoline
    /// codegen). Backends use this to gate trampoline emission.
    pub mutual_tco_member: bool,
    /// `true` if the fn calls itself directly or transitively. Used by
    /// the type checker's flow analysis, the proof exporters, and the
    /// memo-eligibility check.
    pub recursive: bool,
    /// Number of recursive call sites in the fn body. `0` for non-recursive
    /// fns. Memo eligibility requires `>= 2` (single-call recursion is
    /// linear and the memo overhead would dominate).
    pub recursive_call_count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BodyShape {
    /// `BodyPlan::SingleExpr` whose head is a leaf op (literal, ident,
    /// resolved local, etc.) — the fastest possible body shape.
    LeafExpr,
    /// `BodyPlan::SingleExpr` of any other kind (call, match, ctor, …).
    SingleExpr,
    /// `BodyPlan::Block { stmts: N, .. }` — a multi-stmt block (let
    /// bindings + tail expr).
    Block(usize),
    /// Body didn't match any of the classifier's recognised shapes;
    /// usually means the body is a `match` or some other top-level
    /// expression the thin-body classifier doesn't model.
    Unclassified(usize),
}

/// Run the analysis on all `TopLevel::FnDef` items in `items`. The
/// `alloc_policy` is optional — when `None`, `FnAnalysis::allocates`
/// stays `None` for every fn (every other field is still computed).
pub fn analyze(
    items: &[TopLevel],
    alloc_policy: Option<&dyn AllocPolicy>,
    ctx: &impl CallLowerCtx,
) -> AnalysisResult {
    let fn_defs: Vec<&FnDef> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    let alloc_info = alloc_policy.map(|policy| {
        // The trait object can't be passed straight to the generic
        // `compute_alloc_info<P: AllocPolicy>`, so wrap it in a
        // monomorphic adapter that re-dispatches through `dyn`.
        struct PolicyRef<'a>(&'a dyn AllocPolicy);
        impl AllocPolicy for PolicyRef<'_> {
            fn builtin_allocates(&self, name: &str) -> bool {
                self.0.builtin_allocates(name)
            }
            fn constructor_allocates(&self, name: &str, has_payload: bool) -> bool {
                self.0.constructor_allocates(name, has_payload)
            }
        }
        crate::ir::compute_alloc_info(&fn_defs, &PolicyRef(policy))
    });

    // Mutual-TCO membership — split out from `find_tco_groups` because
    // backends want to know "is this fn in a multi-member SCC" specifically
    // (singletons go through plain TCO and don't need trampoline codegen).
    // Identical computation to what every codegen backend (`codegen::mod`,
    // `codegen::rust::mod`) was running on its own; centralising means
    // they read from `FnAnalysis` instead.
    let entry_fns_for_scc: Vec<&FnDef> = fn_defs
        .iter()
        .filter(|fd| fd.name != "main")
        .copied()
        .collect();
    let mut mutual_tco_set: HashSet<String> = HashSet::new();
    for group in tailcall_scc_components(&entry_fns_for_scc) {
        if group.len() < 2 {
            continue; // singleton self-recursive fn — handled by plain TCO
        }
        for fd in group {
            mutual_tco_set.insert(fd.name.clone());
        }
    }

    let recursive_set = find_recursive_fns(items);
    let recursive_call_counts = crate::call_graph::recursive_callsite_counts(items);

    let mut fn_analyses: HashMap<String, FnAnalysis> = HashMap::with_capacity(fn_defs.len());
    for fd in &fn_defs {
        let plan = classify_thin_fn_def(fd, ctx);
        let body_shape = match &plan {
            Some(p) => match &p.body {
                BodyPlan::SingleExpr(BodyExprPlan::Leaf(_)) => BodyShape::LeafExpr,
                BodyPlan::SingleExpr(_) => BodyShape::SingleExpr,
                BodyPlan::Block { stmts, .. } => BodyShape::Block(stmts.len()),
            },
            None => {
                let FnBody::Block(stmts) = fd.body.as_ref();
                BodyShape::Unclassified(stmts.len())
            }
        };
        let analysis = FnAnalysis {
            allocates: alloc_info.as_ref().and_then(|m| m.get(&fd.name).copied()),
            thin_kind: plan.as_ref().map(|p| p.kind),
            body_shape,
            local_count: fd.resolution.as_ref().map(|r| r.local_count),
            mutual_tco_member: mutual_tco_set.contains(&fd.name),
            recursive: recursive_set.contains(&fd.name),
            recursive_call_count: recursive_call_counts.get(&fd.name).copied().unwrap_or(0),
        };
        fn_analyses.insert(fd.name.clone(), analysis);
    }

    AnalysisResult {
        fn_analyses,
        mutual_tco_members: mutual_tco_set,
        recursive_fns: recursive_set,
    }
}
