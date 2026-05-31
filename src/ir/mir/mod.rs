//! Core MIR — executable middle-end (0.24 epic #252).
//!
//! Sits **after** the existing frontend / normalization pipeline and
//! **before** every runtime backend. Same role for execution that
//! [`proof_ir`](crate::ir::proof_ir) plays for proof export: a single
//! canonical IR that every consumer reads instead of re-deriving
//! semantics from `ResolvedExpr` shape.
//!
//! ```text
//! AST → frontend / normalization → Typed Resolved HIR
//!                                       ↓
//!                                  +---------+
//!                                  |   MIR   |   (this module)
//!                                  +---------+
//!                                       ↓
//!                            VM / Rust / wasm-gc / wasip2
//! ```
//!
//! # Scope (0.24)
//!
//! Phase 1 of #252 (this commit) is **doc-only** — module skeleton,
//! pinned design decisions, and the RFC in `RFC.md` next to this
//! file. No types yet, no lowering, no consumer. Phase 2 lands the
//! data model + dump, Phase 3 lands HIR → MIR lowering in widening
//! waves, Phase 4 wires the VM as the first vertical-slice
//! consumer.
//!
//! # Decisions pinned before any lowering lands
//!
//! - **`Try` stays a node, not desugared.** `let x = step()?` lowers
//!   to a `Try` MIR node; backends pick the final shape. Rust will
//!   eventually emit `?` native; the VM emits a tag check + early
//!   return. This is the concrete fix for the lesson from
//!   [stage 8b of #232][shape8b] where the post-pipeline AST had
//!   already collapsed `?` to nested match, forcing the
//!   `ResultPipelineChain` recognizer to smuggle the step list
//!   through a side-channel on `ModulePattern`.
//! - **`match` stays structured.** Flattening to terminator-style
//!   basic blocks is a separate later step (Flat MIR / LIR), not
//!   Phase 1–4.
//! - **Identity is typed.** MIR refers to user declarations via
//!   `FnId` / `TypeId` / `CtorId` / `ModuleId` — never by string.
//!   `SymbolTable` is an input.
//! - **ProofIR is separate.** MIR answers "what does this program
//!   execute"; ProofIR answers "what does this program prove". They
//!   don't merge.
//! - **No flag-day backend rewrite.** Phase 4 migrates the VM only.
//!   Other backends consume HIR until their own slice ships in
//!   later releases.
//!
//! [shape8b]: https://github.com/jasisz/aver/pull/245
//!
//! # Inputs / outputs (Phase 1 contract)
//!
//! Input: `ResolvedProgramView` + `SymbolTable` produced by the
//! existing pipeline. No new frontend.
//!
//! Output: `MirProgram` keyed by `FnId`. Backends consume via
//! `mir_program.fn_by_id(id)`. Per-fn body shape is documented in
//! `RFC.md` and made concrete in Phase 2.
//!
//! # Out of scope for the 0.24 epic
//!
//! - Phase 5 (Rust / wasm-gc / wasip2 migration) — later releases.
//! - Phase 6 (DCE / inliner / monomorphization / SRoA / effect
//!   scheduling) — same.
//! - Moving `interp_lower` / `buffer_build` / `escape` to MIR —
//!   only on a per-pass concrete-win basis, not as a 0.24
//!   deliverable.
//! - Try-auto proof harness (#222 second half) — independent
//!   track.
//!
//! See `RFC.md` next to this file for the full design write-up,
//! open questions, and the per-phase checklist.

pub mod dump;
pub mod expr;
pub mod lower;
pub mod optimize;
pub mod program;
pub mod stats;

pub use crate::ir::hir::BuiltinCtor;
pub use expr::{
    MirBinOp, MirCall, MirCallee, MirConstruct, MirCtor, MirEffectAnnotation, MirExpr,
    MirIndependentProduct, MirLet, MirLocal, MirMatch, MirMatchArm, MirPattern, MirProject,
    MirRecordCreate, MirRecordField, MirRecordUpdate, MirStrPart, MirTailCall,
};
pub use lower::lower_program;
pub use optimize::const_fold;
pub use program::{LocalId, MirFn, MirParam, MirProgram};
pub use stats::{LowerStats, SkipReason};
