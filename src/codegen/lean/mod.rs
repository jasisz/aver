/// Lean 4 backend for the Aver transpiler.
///
/// Transpiles only pure core logic: functions without effects, type definitions,
/// verify blocks (as `example` proofs), and decision blocks (as comments).
/// Effectful functions and `main` are skipped.
mod builtins;
mod crypto;
mod decl_order;
mod expr;
mod law_auto;
pub mod lemma_calc;
mod pattern;
mod prelude;
pub(crate) mod recurrence;
mod sample_literal;
mod shared;
mod syntax;
pub mod tactic_ir;
#[cfg(test)]
mod tests;
pub(in crate::codegen::lean) mod toplevel;
mod transpile;
mod types;
pub mod untranslate;

// Crate-wide re-export: committed-lemma handling (`codegen::lemma_discovery`)
// and the law-auto rungs map Aver fn names to their Lean spelling through the
// same helper the program defs are emitted with, so a cited lemma references
// `decode` / `Run` / `++` exactly as generated.
pub(crate) use expr::aver_name_to_lean;

// `law_auto` cites the prelude spec lemma names via `super::`; the
// `transpile*` entry points below drive the unified emitter.
pub(crate) use prelude::prelude_spec_lemmas_for_builtins;

/// `aver proof --explain` residual probe — turns an emitted main law theorem's
/// source lines into a normalization-only twin so Lean reports its residual
/// (`unsolved goals`). Used by the `--check` harness in the `aver` binary.
pub use law_auto::{residual_probe_body, residual_probe_body_dump};
#[cfg(test)]
use prelude::generate_prelude;
use transpile::{LeanEmitMode, transpile_unified};

use std::collections::HashSet;

use crate::ast::{FnDef, Spanned};
use crate::codegen::{CodegenContext, ProjectOutput};

/// Statement-class channel for emitted law theorems.
///
/// `aver proof --check`'s `universal` metric must know, per law theorem,
/// whether the emitted STATEMENT is genuinely universal or bounded: for a
/// `when`-law over non-refinement-lifted givens, `law_theorem_prop` prepends
/// sampled-domain disjunction premises (`a = 0 ∨ a = 1 ∨ …`) — the theorem
/// then only claims the law on the finite sample domain, even when it is
/// proven by real tactics with a kernel-clean axiom profile. Refinement-lifted
/// `when`-laws drop those premises (the Subtype carries the invariant) and
/// stay genuinely universal.
///
/// Only the code that BUILDS the statement knows which premises it prepended,
/// so the emitter records the class as one structured marker comment per
/// emitted law theorem, preceding it in the generated `.lean` source
/// (self-contained artifact — the classification travels with the export):
///
/// ```text
/// -- aver:law-class <theorem_name> universal
/// -- aver:law-class <theorem_name> bounded-domain
/// ```
///
/// The checker (`lean_universal_proof` in the CLI) consumes these markers and
/// must NEVER re-derive the class from theorem names or by re-parsing
/// statements. A law theorem without a marker earns no universal credit
/// (fail-closed for stale/foreign export dirs).
pub const LAW_CLASS_MARKER_PREFIX: &str = "-- aver:law-class ";
/// Marker class tag: no sampled-domain premises — the `∀`-statement is the
/// law's genuine universal claim.
pub const LAW_CLASS_UNIVERSAL: &str = "universal";
/// Marker class tag: sampled-domain disjunction premises bound the statement
/// to the finite sample domain.
pub const LAW_CLASS_BOUNDED_DOMAIN: &str = "bounded-domain";

/// How verify blocks should be emitted in generated Lean.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VerifyEmitMode {
    /// `example : lhs = rhs := by native_decide`
    NativeDecide,
    /// `example : lhs = rhs := by sorry`
    Sorry,
    /// Named theorem stubs:
    /// `theorem <fn>_verify_<n> : lhs = rhs := by`
    /// `  sorry`
    TheoremSkeleton,
}

// RecursionPlan / ProofModeIssue moved to shared `crate::codegen::recursion`
// so the Dafny backend can reuse the same classifier. Re-export here so
// existing `lean::RecursionPlan` / `lean::ProofModeIssue` call sites keep
// working without churn.
pub use crate::codegen::recursion::{ProofModeIssue, RecursionPlan};

pub use toplevel::PROOF_FUEL_EXHAUSTED_MSG;

/// The marker every Lean runtime panic prints into captured build output.
/// Empirically pinned against real `lake build` transcripts (both panic
/// sites below produce it):
///
/// ```text
/// info: ././././FuelProbe.lean:27:0: PANIC at stepSum__fuel FuelProbe:8:9: Aver proof fuel exhausted
/// PANIC at stepSumAcc__fuel FuelProbe:19:9: Aver proof fuel exhausted
/// info: ././././PanicProbe.lean:11:0: PANIC at Char.toCode AverCommon:11:12: Char.toCode: string is empty
/// ```
///
/// (lake prefixes the first diagnostic of a build step with `info: <loc>:`,
/// later ones print raw — both carry `PANIC at `.)
pub const LEAN_PANIC_LINE_MARKER: &str = "PANIC at ";

/// Count model panic lines in captured `lake build` output.
///
/// Lean's `panic!` does NOT abort: at `native_decide` evaluation time it
/// prints `PANIC at <fn> <file>:<line>: <msg>` (to lake's captured output)
/// and returns the result type's `default` value. When BOTH sides of a
/// bounded sample route through the model (`verify f(x) => g(x)` cases,
/// every `_sample_N` / `_checked_domain` theorem), a panicking evaluation
/// reduces both sides to `default` and the kernel certifies a vacuous —
/// possibly FALSE — equation while `lake` exits 0 with zero sorries. The
/// panic line in the build output is the only trace, so `aver proof --check`
/// charges ANY hit as a hard failure.
///
/// Scans for the generic `PANIC at ` line marker, not a per-site message:
/// the emitted exports contain `panic!` only at compiler-generated sites —
/// the fuel wrappers' exhaustion arm ([`PROOF_FUEL_EXHAUSTED_MSG`]) and
/// partial prelude builtins (e.g. `Char.toCode` on an empty string) — and
/// every one of them shares the same panic-returns-`default` vacuity vector.
/// A green check has no legitimate panic, and the generic marker also
/// catches panics raised inside Lean's own stdlib (e.g. a `get!` deep in a
/// future prelude helper) that a per-message scan could never enumerate.
/// Counts matching LINES (one panic prints exactly one line; the same site
/// can fire on several theorems).
///
/// Known false-positive boundary, accepted: on an ALREADY-FAILING build, a
/// native_decide error can echo the false proposition, and a user string
/// literal containing `PANIC at ` inside it would inflate the count (and
/// set `model_panicked`). The verdict stays correct — the build already
/// failed on exit status — so a spurious match can only turn a red redder,
/// never a green red.
pub fn count_model_panic_lines(output: &str) -> usize {
    output
        .lines()
        .filter(|l| l.contains(LEAN_PANIC_LINE_MARKER))
        .count()
}

pub(crate) fn pure_fns(ctx: &CodegenContext) -> Vec<&FnDef> {
    ctx.modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| toplevel::is_pure_fn(fd))
        .collect()
}

pub(crate) fn recursive_type_names(ctx: &CodegenContext) -> HashSet<String> {
    ctx.modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .filter(|td| toplevel::is_recursive_type_def(td))
        .map(|td| toplevel::type_def_name(td).to_string())
        .collect()
}

pub(crate) fn recursive_pure_fn_names(ctx: &CodegenContext) -> HashSet<String> {
    // `ctx.recursive_fns` is the single source of truth — populated
    // by `build_context` from analyze in production, by
    // `refresh_facts()` (called from each `transpile*` entry point)
    // in test stubs. After phase C it's keyed by opaque `FnId`;
    // project pure-fn ids back through the symbol table and surface
    // bare names for Lean's downstream scope-local classifiers.
    let symbols = &ctx.symbol_table;
    let pure_ids: HashSet<crate::ir::FnId> = pure_fns(ctx)
        .into_iter()
        .filter_map(|fd| crate::codegen::common::fn_id_for_decl(ctx, fd))
        .collect();
    ctx.recursive_fns
        .intersection(&pure_ids)
        .map(|id| symbols.fn_entry(*id).key.name.clone())
        .collect()
}

fn verify_counter_key(vb: &crate::ast::VerifyBlock) -> String {
    // Shared with the CLI's VM ground-truth collection — see the doc on
    // `verify_block_counter_key` for why the two sides must not drift.
    crate::codegen::common::verify_block_counter_key(vb)
}

fn lean_project_name(ctx: &CodegenContext) -> String {
    crate::codegen::common::entry_basename(ctx)
}

pub(super) fn bound_expr_to_lean(expr: &Spanned<crate::ir::hir::ResolvedExpr>) -> String {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match &expr.node {
        ResolvedExpr::Literal(crate::ast::Literal::Int(n)) => format!("{}", n),
        ResolvedExpr::Ident(name) | ResolvedExpr::Resolved { name, .. } => {
            expr::aver_name_to_lean(name)
        }
        ResolvedExpr::Call(callee, args) => {
            // Bound expressions are linear arithmetic over the param —
            // typically `List.len(xs)` or an int constant. The resolver
            // lifts `List.len(...)` to `ResolvedCallee::Builtin`; user-fn
            // refs in a bound expression would be a malformed metric
            // anyway, so we only render builtins and fall through to
            // `"0"` (the same fallback the original AST helper used for
            // shapes it couldn't classify).
            let dotted = match callee {
                ResolvedCallee::Builtin(name) => Some(name.clone()),
                _ => None,
            };
            if let Some(dotted) = dotted {
                // List.len(xs) → xs.length in Lean
                if dotted == "List.len" && args.len() == 1 {
                    return format!("{}.length", bound_expr_to_lean(&args[0]));
                }
                let lean_args: Vec<String> = args.iter().map(bound_expr_to_lean).collect();
                format!(
                    "({} {})",
                    expr::aver_name_to_lean(&dotted),
                    lean_args.join(" ")
                )
            } else {
                "0".to_string()
            }
        }
        ResolvedExpr::Attr(obj, field) => format!(
            "{}.{}",
            bound_expr_to_lean(obj),
            expr::aver_name_to_lean(field)
        ),
        _ => "0".to_string(),
    }
}

pub(crate) use crate::codegen::recursion::detect::sizeof_measure_param_indices;

/// Proof-mode diagnostics for Lean transpilation.
///
/// Returns human-readable notices for recursive shapes that still fall back to
/// regular `partial` Lean defs instead of total proof-mode emission.
pub fn proof_mode_findings(ctx: &CodegenContext) -> Vec<ProofModeIssue> {
    // ProofIR carries `unclassified_fns` populated by the ContractLower
    // pipeline stage — same data analyze_plans used to return, just
    // read off the IR instead of re-running the classifier.
    ctx.proof_ir
        .unclassified_fns
        .iter()
        .map(|uf| ProofModeIssue {
            line: uf.line,
            message: uf.message.clone(),
        })
        .collect()
}

pub fn proof_mode_issues(ctx: &CodegenContext) -> Vec<String> {
    proof_mode_findings(ctx)
        .into_iter()
        .map(|issue| issue.message)
        .collect()
}

/// Transpile an Aver program to a Lean 4 project.
///
/// Takes `&mut ctx` so it can run `ctx.refresh_facts()` upfront — keeps
/// the derived sets (`mutual_tco_members`, `recursive_fns`) in sync with
/// the current items + modules. Idempotent: production callers go through
/// `build_context` which already populated them, so refresh recomputes
/// the same answer; test stubs that build the ctx piecewise (push items
/// in-place, bypass `build_context`) get the fresh sets they need.
pub fn transpile(ctx: &mut CodegenContext) -> ProjectOutput {
    transpile_with_verify_mode(ctx, VerifyEmitMode::NativeDecide)
}

/// Proof-mode transpilation.
///
/// Uses recursion plans validated by `proof_mode_issues` and emits supported
/// recursive functions without `partial`, adding `termination_by` scaffolding.
pub fn transpile_for_proof_mode(
    ctx: &mut CodegenContext,
    verify_mode: VerifyEmitMode,
) -> ProjectOutput {
    // No refresh_facts call here: production callers go through
    // build_codegen_context → pipeline, which populates every derived
    // fact (recursive_fns, mutual_tco_members, proof_ir) once.
    // Synthetic-AST tests that bypass the pipeline call refresh_facts
    // themselves before reaching this fn.
    transpile_unified(ctx, verify_mode, LeanEmitMode::Proof, false)
}

/// Proof-mode transpilation for an artifact CERTIFICATE's reused model
/// modules.
///
/// Same model DEFINITIONS as [`transpile_for_proof_mode`], but emitted
/// without the proof-only executable machinery that the `aver proof`
/// sample checks rely on: the `@[implemented_by]`/`unsafe` `DecidableEq`
/// shims (user recursive types and `Float`), the `verify` sample-check
/// `example … := by native_decide` blocks, and the `@[simp]` string-prelude
/// spec lemmas. A certificate never elaborates those — it carries its own
/// decode-to-`Int`/bytes anti-vacuity guards — and the checker's
/// elaboration-executes-code wall rejects the `unsafe`/`implemented_by`/
/// `@[` tokens they contain. The mode drops them at the source so the
/// certificate's model data files stay kernel-clean, instead of stripping
/// them out of already-emitted text. The `aver proof` emission is
/// untouched (this is a distinct entry point).
pub fn transpile_for_cert_model(ctx: &mut CodegenContext) -> ProjectOutput {
    transpile_unified(ctx, VerifyEmitMode::NativeDecide, LeanEmitMode::Proof, true)
}

/// Transpile an Aver program to a Lean 4 project with configurable verify proof mode.
///
/// - `NativeDecide` emits `example ... := by native_decide`
/// - `Sorry` emits `example ... := by sorry`
/// - `TheoremSkeleton` emits named theorem skeletons with `sorry`
pub fn transpile_with_verify_mode(
    ctx: &mut CodegenContext,
    verify_mode: VerifyEmitMode,
) -> ProjectOutput {
    // No refresh_facts call here — same reasoning as
    // `transpile_for_proof_mode`. Synthetic-AST tests refresh
    // themselves; production paths come pre-populated.
    transpile_unified(ctx, verify_mode, LeanEmitMode::Standard, false)
}
