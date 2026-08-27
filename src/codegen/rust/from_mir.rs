//! Rust backend: emit expressions from Core MIR.
//!
//! This is the SOLE Rust runtime codegen path. The HIR `ResolvedExpr`
//! walker was deleted in rust-on-MIR W6/Stage-3; the MIR walker here
//! owns all runtime codegen, the same deduplication MIR brought to the
//! VM (#339) and wasm-gc (#384): one semantic walker per construct lives
//! in MIR, and every backend reads from it instead of forking
//! `ResolvedExpr`.
//!
//! [`emit_mir_expr`] is the dispatcher; [`coverage_report`] measures how
//! much of a program it can render standalone. [`emit_mir_fn_body`] wraps
//! it into the full single-expr-plan body format, and
//! [`emit_mir_fn_body_routed`] is the production wire-up: it builds the
//! per-fn borrow policy and renders the body. A construct the walker
//! returns `None` for surfaces as a hard codegen diagnostic at the call
//! site (the only residual is the verify-only Oracle/trace shapes that
//! never built on the Rust backend).
//!
//! ## Covered constructs
//!
//! `Literal`, `Local`, `Neg`, `BinOp` (numeric ops, plus `Str` `+`
//! concat — the right side borrowed for `AverStr`'s `Add<&AverStr>` —
//! disambiguated from numeric add by the operands' type stamps),
//! `Call` (`Fn` / `Builtin` / `Intrinsic` / `LocalSlot` — the last a
//! first-class fn-pointer call `name(args…)`, post-#379 always a plain
//! fn-pointer since `Type::Fn` is param-only), `Return`, `TailCall`
//! (emitted as a plain call; the HIR self-TCO `continue` rewrite needs
//! `ectx`, so the wire-up's parity check is the safety net), `Try` (`?`),
//! `Tuple`, `List`, `MapLiteral`, `Let` (block-expr `{ let x = …; … }`),
//! `Project`, `RecordCreate` / `RecordUpdate`, `Construct` (built-in and
//! user ctors, including dep-module records resolved through
//! `module_prefixes`), `IfThenElse`, `IndependentProduct`, and `FnValue`
//! (a fn referenced as a value — the `StaticRef` shape).
//!
//! `Match` (Wave 2) — `MirExpr::Match` emits through [`emit_mir_match`],
//! mirroring HIR's `emit_match` / `emit_dispatch_table_match` /
//! `emit_list_match` selection byte-for-byte. The shared classifiers
//! (`classify_match_dispatch_plan_resolved` etc.) + `emit_pattern` +
//! the dispatch/list emitters are reused directly by translating each
//! `MirPattern` → `ResolvedPattern` and feeding a `body_for_arm`
//! closure that renders the matching arm's MIR body. Bool two-arm
//! matches never reach this arm — the MIR optimizer's `bool_match_to_if`
//! pass already rewrote them to `IfThenElse` (handled above).
//!
//! `InterpolatedStr` never reaches the walker — `interp_lower` lowers it
//! away before codegen runs. Every reachable MIR construct has a walker
//! arm; the only `None` cases are the verify-only Oracle/trace shapes
//! that never built on the Rust backend (they hard-error at the call
//! site).

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::ast::{BinOp, Spanned, Type};
use crate::codegen::CodegenContext;
use crate::codegen::common::module_prefix_to_rust_path;
use crate::ir::hir::{
    BuiltinCtor, BuiltinIntrinsic, ResolvedCtor, ResolvedMatchArm, ResolvedPattern,
    classify_match_dispatch_plan_resolved,
};
use crate::ir::mir::{
    LocalId, MirCallee, MirCtor, MirExpr, MirLocal, MirMatch, MirPattern, MirProgram,
};
use crate::ir::{MatchDispatchPlan, SymbolTable};

use super::emit_ctx::{is_copy_type, should_borrow_param};
use super::expr::{
    callee_borrow_mask, constructor_boxed_positions, emit_dispatch_table_match, emit_list_match,
    emit_literal, emit_parallel_result_tuple_unwrap, emit_pattern_rebindings,
    emit_ref_match_rebindings, emit_result_tuple_unwrap, emit_tuple_from_vars,
    explicit_binding_pattern, explicit_parameter_pattern, has_list_patterns,
    has_string_literal_patterns,
};
use super::pattern::emit_pattern;
use super::syntax::aver_name_to_rust;

/// Walker-side emit context. Holds the slice of the
/// `CodegenContext` the MIR-to-Rust walker reads — kept explicit
/// so future `CodegenContext` refactors don't ripple through the
/// walker, and so other backends (wasm-gc, wasip2) can introduce
/// their own emit-ctx structs without inheriting Rust-specific
/// fields.
///
/// Two distinct shapes share this struct:
///
/// - **coverage / test** (`for_test`): only `symbol_table` +
///   `module_prefixes` are populated; `codegen` is `None` and the
///   borrow fields are empty. The coverage walk only asks "does
///   this fn emit `Some`", so it never needs the borrow machinery
///   or the full `CodegenContext`.
/// - **production parity gate** (`for_fn`): carries the full
///   `&CodegenContext` plus the per-fn borrow policy
///   (`local_types` / `rc_wrapped` / `borrowed_params` /
///   `loop_carried_params` /
///   `current_module_scope`), recomputed from the `ResolvedFnDef`
///   the HIR walker uses. This is the slice of
///   [`super::emit_ctx::EmitCtx`] the covered arms need so their
///   clone / borrow / `Arc::new` decisions match HIR byte-for-byte.
#[derive(Clone, Copy)]
pub struct MirEmitCtx<'a> {
    pub symbol_table: &'a SymbolTable,
    pub module_prefixes: &'a HashSet<String>,
    /// Full codegen context — `Some` only on the production parity
    /// gate path. `constructor_boxed_positions` /
    /// `callee_borrow_mask` need it; the coverage walk leaves it
    /// `None` (no borrow decisions, just structural reach).
    pub codegen: Option<&'a CodegenContext>,
    /// Local variable types (fn params + let bindings) for
    /// copy-type elision. Empty on the coverage path.
    pub local_types: &'a HashMap<String, Type>,
    /// Params passed as `Rc<T>` (self-TCO) / `&T` (mutual-TCO).
    pub rc_wrapped: &'a HashSet<String>,
    /// Whether `rc_wrapped` params are represented as `&T`. This is true
    /// only while emitting mutual-TCO trampoline arms; self-TCO wraps the
    /// same policy class in `Arc<T>` instead. Equality uses this distinction
    /// to align an owned operand with a borrowed invariant without changing
    /// clone policy for either TCO shape.
    pub rc_wrapped_are_borrowed_refs: bool,
    /// Params emitted as `&T` (borrow-by-default for non-Copy,
    /// non-Str params).
    pub borrowed_params: &'a HashSet<String>,
    /// Self-TCO params whose identity rebind is elided for the tail call
    /// currently being emitted. Their source value remains live in the next
    /// loop iteration, so a syntactic `last_use` inside another tail-call
    /// argument is not a real move point. Owning positions clone these
    /// params; non-owning reads remain allocation-free.
    pub loop_carried_params: &'a HashSet<String>,
    /// Collection (`Vector`/`Map`) params that the `own_param` MIR pass
    /// PROVED uniquely owned (cleared their `aliased_slots` bit). These
    /// are emitted owned-by-value (`mut p: T`, NOT `&T`) and, at a
    /// last-use read, skip the `.clone()` so an in-place `Rc::make_mut`
    /// runs on a refcount-1 backing (native O(n) mutate instead of the
    /// O(n²) borrow+clone COW). SOUNDNESS: a name is in this set only
    /// when `own_param` cleared its bit — never broadened past what the
    /// pass proved. Disjoint from `borrowed_params` by construction.
    pub owned_params: &'a HashSet<String>,
    /// Owning module prefix for the fn whose body this ctx emits.
    pub current_module_scope: Option<&'a str>,
    /// Interned built-in fn names, indexed by `BuiltinId`
    /// (`MirProgram.builtins`). The `Call(Builtin(id))` arm resolves
    /// `id` → dotted name through this slice, mirroring wasm-gc's
    /// `ctx.mir_builtins`. Empty on the coverage / test path — a
    /// `BuiltinId` then resolves to nothing (`None` → HIR fallback),
    /// which is fine because that path only inspects `Some` vs `None`.
    pub mir_builtins: &'a [String],
    /// Per-`LocalId` bare-`i64` representation facts for the fn whose body
    /// this ctx emits — the Int "unboxing" analysis output. A slot proven
    /// `Bare` emits native `i64` (raw literal / raw arithmetic / `i64`
    /// param-return signature); every other slot keeps `aver_rt::AverInt`.
    /// Empty (all-`Boxed`) on the coverage / test / free-standing paths.
    /// SOUNDNESS: a slot is read here as `Bare` only when the analysis
    /// proved `raw_i64_eligible && !escapes`; a missing fact is `Boxed`.
    pub bare: &'a crate::ir::mir::FnBareFacts,
    /// Verify-case emission only: render `?` with a `map_err` that Debug-
    /// formats the error into a `String`. The generated `#[cfg(test)]` fn
    /// returns `Result<(), String>`, while generated fns error with
    /// whatever type the source declared (`AverStr` for `String`, a
    /// generated struct/enum for user error types) — none of which
    /// implement `From` into `String`, so a bare `?` fails E0277. Every
    /// generated error shape implements `Debug` (aver-rt value types and
    /// derived record/sum types), which makes the Debug-format conversion
    /// total. False everywhere except `emit_mir_verify_expr`.
    pub try_err_to_string: bool,
}

impl<'a> MirEmitCtx<'a> {
    /// Construct a minimal walker ctx for the coverage walk /
    /// tests. Caller supplies a hand-built symbol table;
    /// `module_prefixes` defaults to the caller's owned empty set
    /// (or a populated one when the test needs to exercise
    /// module-scoped name resolution). No `CodegenContext`, no
    /// borrow policy — the covered arms emit conservative output
    /// (no clone / borrow / `Arc::new`), which is fine because the
    /// coverage walk only inspects `Some` vs `None`.
    pub fn for_test(symbol_table: &'a SymbolTable, module_prefixes: &'a HashSet<String>) -> Self {
        static EMPTY_TYPES: std::sync::OnceLock<HashMap<String, Type>> = std::sync::OnceLock::new();
        static EMPTY_SET: std::sync::OnceLock<HashSet<String>> = std::sync::OnceLock::new();
        Self {
            symbol_table,
            module_prefixes,
            codegen: None,
            local_types: EMPTY_TYPES.get_or_init(HashMap::new),
            rc_wrapped: EMPTY_SET.get_or_init(HashSet::new),
            rc_wrapped_are_borrowed_refs: false,
            borrowed_params: EMPTY_SET.get_or_init(HashSet::new),
            loop_carried_params: EMPTY_SET.get_or_init(HashSet::new),
            owned_params: EMPTY_SET.get_or_init(HashSet::new),
            current_module_scope: None,
            // No builtin table on the coverage path: `Call(Builtin)`
            // resolves to `None` and the fn reports as HIR-fallback,
            // matching the pre-Wave-3a coverage walk's reach.
            mir_builtins: &[],
            bare: empty_bare_facts(),
            try_err_to_string: false,
        }
    }

    /// Construct a **program-level** walker ctx for free-standing
    /// expressions that belong to no `ResolvedFnDef` — verify cases
    /// (this wave) and, next wave, `main` / top-level statements. The
    /// MIR mirror of `EmitCtx::empty()`: carries the full
    /// `&CodegenContext` (so ctor boxing / `callee_borrow_mask` / match
    /// emission work, unlike the coverage `for_test` path which leaves
    /// `codegen` `None`), but with an **empty per-fn policy** — no
    /// params, no locals, nothing borrowed-by-default. Every name a
    /// covered arm sees is treated owned / non-Copy, exactly as
    /// `EmitCtx::empty()` does for the HIR walker on these same
    /// free-standing exprs.
    ///
    /// Shared infra: both the verify wire-up and the next-wave
    /// main/top-stmt wire-up build their `MirEmitCtx` from here, so the
    /// "no-anchor" emit policy lives in one place.
    ///
    /// `mir_builtins` is passed explicitly rather than read off
    /// `ctx.mir_program`: free-standing exprs are lowered against a
    /// *clone* of the entry program (so builtin / instantiation table
    /// growth stays local), and `Call(Builtin(id))` must resolve `id`
    /// through that grown clone's table — not the entry program's,
    /// which may lack a builtin the lowering just interned. The caller
    /// owns the clone and lends its `builtins` slice here.
    pub(super) fn program_level(
        ctx: &'a CodegenContext,
        policy: &'a MirFnEmitPolicy,
        mir_builtins: &'a [String],
    ) -> Self {
        Self {
            symbol_table: &ctx.symbol_table,
            module_prefixes: &ctx.module_prefixes,
            codegen: Some(ctx),
            local_types: &policy.local_types,
            rc_wrapped: &policy.rc_wrapped,
            rc_wrapped_are_borrowed_refs: false,
            borrowed_params: &policy.borrowed_params,
            loop_carried_params: empty_string_set(),
            owned_params: &policy.owned_params,
            current_module_scope: policy.current_module_scope.as_deref(),
            mir_builtins,
            bare: &policy.bare,
            try_err_to_string: false,
        }
    }

    /// Construct a borrow-aware walker ctx for the production
    /// parity gate. `policy` is the [`MirFnEmitPolicy`] recomputed
    /// per-fn from the `ResolvedFnDef` (the same inputs
    /// `build_fn_ectx_from_resolved` feeds the HIR walker), and
    /// `ctx` is the full codegen context the borrow helpers query.
    pub(super) fn for_fn(ctx: &'a CodegenContext, policy: &'a MirFnEmitPolicy) -> Self {
        Self {
            symbol_table: &ctx.symbol_table,
            module_prefixes: &ctx.module_prefixes,
            codegen: Some(ctx),
            local_types: &policy.local_types,
            rc_wrapped: &policy.rc_wrapped,
            rc_wrapped_are_borrowed_refs: false,
            borrowed_params: &policy.borrowed_params,
            loop_carried_params: empty_string_set(),
            owned_params: &policy.owned_params,
            current_module_scope: policy.current_module_scope.as_deref(),
            // The builtin table the parity gate already built into the
            // `CodegenContext`. `Call(Builtin(id))` resolves `id`
            // through it; if the ctx carries no MIR program (it always
            // does on the gate path, but be defensive) builtins just
            // won't resolve → HIR fallback.
            mir_builtins: ctx
                .mir_program
                .as_ref()
                .map(|p| p.builtins.as_slice())
                .unwrap_or(&[]),
            bare: &policy.bare,
            try_err_to_string: false,
        }
    }

    /// Is this local a Copy type in Rust (i64 / f64 / bool / ())?
    fn is_copy(&self, name: &str) -> bool {
        self.local_types.get(name).is_some_and(is_copy_type)
    }

    fn is_rc_wrapped(&self, name: &str) -> bool {
        self.rc_wrapped.contains(name)
    }

    fn is_borrowed_param(&self, name: &str) -> bool {
        self.borrowed_params.contains(name)
    }

    fn is_loop_carried_param(&self, name: &str) -> bool {
        self.loop_carried_params.contains(name)
    }

    fn is_owned_param(&self, name: &str) -> bool {
        self.owned_params.contains(name)
    }
}

fn empty_string_set() -> &'static HashSet<String> {
    static EMPTY: std::sync::OnceLock<HashSet<String>> = std::sync::OnceLock::new();
    EMPTY.get_or_init(HashSet::new)
}

/// A shared empty `FnBareFacts` (all-`Boxed`) for the coverage / test /
/// free-standing emit paths that have no per-fn unboxing facts. Returning
/// a `'static` reference keeps `MirEmitCtx` `Copy`.
fn empty_bare_facts() -> &'static crate::ir::mir::FnBareFacts {
    static EMPTY: std::sync::OnceLock<crate::ir::mir::FnBareFacts> = std::sync::OnceLock::new();
    EMPTY.get_or_init(crate::ir::mir::FnBareFacts::default)
}

/// Does `expr` evaluate to a provably-bare `i64` value (so it may be
/// emitted with native integer arithmetic rather than `AverInt` methods)?
///
/// - A `Local` whose slot the analysis proved `Bare`.
/// - An `Int` literal (an exact constant — always representable as `i64`
///   in a bare arithmetic context).
/// - A `Neg` / `Add` / `Sub` / `Mul` over bare operands WHOSE RESULT
///   INTERVAL provably fits `i64` (a tree like `n + i64::MAX` whose operands
///   are each bare but whose result leaves `i64` is NOT bare — emitting raw
///   `i64` there would silently wrap with `overflow-checks = false`).
///
/// SINGLE SOURCE OF TRUTH: this is a thin delegate to
/// [`crate::ir::mir::FnBareFacts::expr_is_bare_i64`] — the SAME interval-
/// checked predicate the analysis's `tail_value_is_bare` uses — so codegen
/// never re-decides bareness structurally and can never disagree with the
/// analysis that drove the signature.
///
/// Fail-closed: any other shape (a boxed `Local`, a call result, a
/// non-Int operand, an overflowing compound) is NOT bare, so the emit
/// keeps `AverInt`.
fn mir_expr_is_bare_i64(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> bool {
    ctx.bare.expr_is_bare_i64(&expr.node)
}

/// Emit `expr` as a raw `i64` expression, assuming [`mir_expr_is_bare_i64`]
/// already returned `true`. Literals become the bare `{N}i64` form,
/// arithmetic uses native operators, and a bare `Local` is its plain
/// ident. `None` only if a nested operand can't render (e.g. a synthetic
/// unnamed local) — the caller then falls back to the boxed path. Takes no
/// `ctx`: the bare gate (`mir_expr_is_bare_i64`) already consulted it, and
/// the emit itself is purely structural.
fn emit_bare_i64(expr: &Spanned<MirExpr>) -> Option<String> {
    match &expr.node {
        MirExpr::Literal(l) => match l.node {
            crate::ast::Literal::Int(k) => Some(format!("{}i64", k)),
            _ => None,
        },
        MirExpr::Local(local) => {
            let name = &local.node.name;
            if name.is_empty() {
                return None;
            }
            Some(aver_name_to_rust(name))
        }
        MirExpr::Neg(_) => {
            // Sound only when the interval excludes `i64::MIN`; the
            // analysis proves the operand fits `i64`, but `-i64::MIN`
            // wraps. We bail to the boxed path for a bare `Neg` (the
            // const-fold pass collapses `Neg(Literal)` before here, so a
            // real bare `Neg` is rare) — keeping `AverInt` is always sound.
            None
        }
        MirExpr::BinOp(b) => {
            let op = match b.node.op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                _ => return None,
            };
            let l = emit_bare_i64(&b.node.lhs)?;
            let r = emit_bare_i64(&b.node.rhs)?;
            Some(format!("({} {} {})", l, op, r))
        }
        _ => None,
    }
}

/// Per-fn borrow policy for the MIR walker — the slice of
/// [`super::emit_ctx::EmitCtx`] the covered arms read, owned so a
/// borrowing [`MirEmitCtx`] can be built from it. Recomputed per
/// fn from the `ResolvedFnDef`, mirroring `for_fn` /
/// `for_fn_no_borrow` on `EmitCtx`.
pub(super) struct MirFnEmitPolicy {
    pub local_types: HashMap<String, Type>,
    pub rc_wrapped: HashSet<String>,
    pub borrowed_params: HashSet<String>,
    /// Collection params `own_param` proved uniquely owned — see the
    /// `MirEmitCtx::owned_params` doc. Default empty (no own_param facts
    /// applied); populated by [`Self::apply_own_param`].
    pub owned_params: HashSet<String>,
    /// Per-`LocalId` bare-`i64` facts for this fn (the Int unboxing
    /// analysis output), owned so the borrowing `MirEmitCtx` can reference
    /// it. Default empty (all-`Boxed`); populated by
    /// [`Self::apply_bare_i64`] from the `CodegenContext`'s program-wide
    /// `BareI64Facts`.
    pub bare: crate::ir::mir::FnBareFacts,
    pub current_module_scope: Option<String>,
}

impl MirFnEmitPolicy {
    /// The empty / no-anchor borrow policy — no params, no locals,
    /// nothing borrowed-by-default. Feeds [`MirEmitCtx::program_level`]
    /// for free-standing expressions (verify cases, main / top-level
    /// statements). The MIR mirror of `EmitCtx::empty()`.
    pub(super) fn empty() -> Self {
        Self {
            local_types: HashMap::new(),
            rc_wrapped: HashSet::new(),
            borrowed_params: HashSet::new(),
            owned_params: HashSet::new(),
            bare: crate::ir::mir::FnBareFacts::default(),
            current_module_scope: None,
        }
    }

    /// Build the borrow policy from a `ResolvedFnDef`'s param
    /// types. `borrow_by_default` mirrors `EmitCtx::for_fn` (true)
    /// vs `EmitCtx::for_fn_no_borrow` (false, the TCO path):
    /// when false, no param is borrowed-by-default. `rc_wrapped`
    /// starts empty (set later for TCO pass-through, which the
    /// covered subset doesn't graduate).
    pub(super) fn from_resolved(
        resolved: &crate::ir::hir::ResolvedFnDef,
        scope: Option<&str>,
        borrow_by_default: bool,
    ) -> Self {
        let local_types: HashMap<String, Type> = resolved
            .params
            .iter()
            .map(|(name, ty)| (name.clone(), ty.clone()))
            .collect();
        let borrowed_params = if borrow_by_default {
            local_types
                .iter()
                .filter(|(_, ty)| should_borrow_param(ty))
                .map(|(name, _)| name.clone())
                .collect()
        } else {
            HashSet::new()
        };
        Self {
            local_types,
            rc_wrapped: HashSet::new(),
            borrowed_params,
            owned_params: HashSet::new(),
            bare: crate::ir::mir::FnBareFacts::default(),
            current_module_scope: scope.map(String::from),
        }
    }

    /// Apply the Int "unboxing" facts to this policy: clone the per-fn
    /// `FnBareFacts` slice out of the program-wide `BareI64Facts` so the
    /// body emit and the signature emit read the SAME per-`LocalId`
    /// representation decisions. A bare `Int` param is ALSO dropped from
    /// `borrowed_params` — a bare `i64` is `Copy`, taken by value, never
    /// borrowed. Fail-closed: a fn with no facts keeps the default empty
    /// (all-`Boxed`).
    pub(super) fn apply_bare_i64(&mut self, fn_id: crate::ir::FnId, ctx: &CodegenContext) {
        if let Some(facts) = ctx.bare_i64.for_fn(fn_id) {
            self.bare = facts.clone();
        }
    }

    /// Apply the analysis facts, then restrict them to the representation
    /// tags carried by the rewritten MIR function itself. Mutual-TCO pins the
    /// shared trampoline ABI boxed, but may still retain independently proven
    /// compiler-owned codepoint binders as local `i64` slots; the trampoline
    /// arm must see exactly that restricted view when it chooses literal-match
    /// representation.
    pub(super) fn apply_rewritten_bare_i64(
        &mut self,
        mir_fn: &crate::ir::mir::MirFn,
        ctx: &CodegenContext,
    ) {
        self.apply_bare_i64(mir_fn.fn_id, ctx);
        self.bare
            .values
            .retain(|slot, _| mir_fn.repr.slot_is_bare(*slot));
        self.bare
            .carrier_slots
            .retain(|slot, _| mir_fn.repr.slot_is_bare_carrier(*slot));
        self.bare.bare_params = mir_fn.repr.bare_params.clone();
        self.bare.bare_return = mir_fn.repr.bare_return;
    }

    /// Apply the MIR ownership facts to this policy. A `Vector`/`Map` param
    /// graduates when `own_param` cleared its alias bit. A named record param
    /// graduates when its returned successor is a `RecordUpdate` rooted in
    /// that param: Rust can move the record and its replaced field while the
    /// caller still clones at a non-last-use call site.
    ///
    /// SOUNDNESS (the #383 corruption class): a collection param is
    /// graduated ONLY when `own_param` cleared its bit. `own_param`'s
    /// RULE 1 flags EVERY `Vector`/`Map` param `true` up front and only
    /// clears the bit on a whole-program proof of unique ownership
    /// (every visible call site passes a fresh / linearly-threaded
    /// value, captured-into-aggregate slots stay flagged, multi-module
    /// returns early leaving every bit set). So a cleared bit on a
    /// collection param is exactly the pass's proof — never a heuristic.
    /// A missing bit defaults to flagged (`true`) → not graduated
    /// (conservative). Params still flagged keep borrow-by-default.
    pub(super) fn apply_own_param(&mut self, mir_fn: &crate::ir::mir::MirFn) {
        for (i, param) in mir_fn.params.iter().enumerate() {
            // Only collection params are candidates (the only thing
            // `own_param`'s RULE 1 ever flags). A non-collection param is
            // never owned-graduated by this pass, so leave it untouched.
            // Check the REAL `Type` (from the policy's `local_types`,
            // sourced from `ResolvedFnDef`) — the `MirParam.ty` is a
            // `format!("{ty:?}")` Debug string (`Vector(Int)`), fragile to
            // parse.
            let rust_name = aver_name_to_rust(&param.name);
            let Some(ty) = self.local_types.get(&rust_name) else {
                continue;
            };
            let returned_record_successor = matches!(ty, Type::Named { .. })
                && result_contains_record_successor_of(&mir_fn.body.node, param.local);
            // `own_param`'s `prone`/clearing both index `aliased_slots`
            // by PARAM POSITION `i` (its `(0..nparams).filter(|&i| …)`),
            // matching `MirParam.local = LocalId(i)`; match that exactly.
            //
            // Cleared bit ⟺ own_param proved unique ownership. Missing →
            // treat as flagged (conservative). Still-flagged → keep the
            // existing borrow-by-default decision (do not graduate).
            let collection_graduated = is_owned_collection_candidate(ty)
                && !mir_fn.aliased_slots.get(i).copied().unwrap_or(true);
            if !collection_graduated && !returned_record_successor {
                continue;
            }
            // Graduate: owned-by-value. On the borrow-by-default path the
            // param was in `borrowed_params`; remove it. On the TCO
            // no-borrow path it was never borrowed (already `mut`-owned),
            // but it still needs to land in `owned_params` so the body's
            // clone-skip fires.
            self.borrowed_params.remove(&rust_name);
            self.owned_params.insert(rust_name);
        }
    }
}

/// Is this the type of a param `own_param` can prove owned — a `Vector`
/// or `Map`? These are the only param shapes `alias.rs` RULE 1 flags and
/// thus the only ones `own_param` ever clears; nothing else is a sound
/// clone-skip candidate. (`List` is an `Rc`-COW persistent list whose
/// clone is cheap and is NOT flagged by RULE 1, so it stays borrowed.)
fn is_owned_collection_candidate(ty: &Type) -> bool {
    matches!(ty, Type::Vector(_) | Type::Map(_, _))
}

/// Does a function result contain a record successor whose base is `slot`?
///
/// Only result positions are transparent. A record update in a `let` value or
/// call argument does not qualify because moving its base could invalidate a
/// later read in the same function. `Construct` covers `Result.Ok(update)`;
/// branches and the body side of `Let` preserve result position.
fn result_contains_record_successor_of(expr: &MirExpr, slot: LocalId) -> bool {
    match expr {
        MirExpr::RecordUpdate(update) => {
            local_of(&update.node.base.node).is_some_and(|base| base.slot == slot)
        }
        MirExpr::RecordCreate(record) => record.node.fields.iter().any(|field| {
            let MirExpr::Call(call) = &field.value.node else {
                return false;
            };
            let Some(target) = call.node.args.first() else {
                return false;
            };
            let MirExpr::Project(project) = &target.node else {
                return false;
            };
            local_of(&project.node.base.node).is_some_and(|base| {
                base.slot == slot && base.last_use && project.node.field == field.name
            })
        }),
        MirExpr::Let(let_expr) => {
            result_contains_record_successor_of(&let_expr.node.body.node, slot)
        }
        MirExpr::Match(match_expr) => match_expr
            .node
            .arms
            .iter()
            .any(|arm| result_contains_record_successor_of(&arm.body.node, slot)),
        MirExpr::IfThenElse(branches) => {
            result_contains_record_successor_of(&branches.node.then_branch.node, slot)
                || result_contains_record_successor_of(&branches.node.else_branch.node, slot)
        }
        MirExpr::Return(inner)
        | MirExpr::Try(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner)
        | MirExpr::Neg(inner) => result_contains_record_successor_of(&inner.node, slot),
        MirExpr::Construct(construct) => construct
            .node
            .args
            .iter()
            .any(|arg| result_contains_record_successor_of(&arg.node, slot)),
        MirExpr::Tuple(items) => items
            .iter()
            .any(|item| result_contains_record_successor_of(&item.node, slot)),
        _ => false,
    }
}

/// The Rust-mangled names of a fn's `Vector`/`Map` params that
/// `own_param` PROVED uniquely owned (cleared `aliased_slots` bit) — the
/// set the non-TCO SIGNATURE emits owned-by-value (`mut p: T`). The
/// `param_types` are the `ResolvedFnDef` param `(name, Type)` pairs (real
/// `Type`, not the `MirParam.ty` Debug string); the `mir_fn` supplies the
/// optimized `aliased_slots`. Computed exactly as
/// [`MirFnEmitPolicy::apply_own_param`] (same param-position indexing,
/// same RULE-1 candidate filter, same missing-bit-is-flagged default) so
/// signature and body never disagree.
pub(super) fn owned_collection_param_names(
    mir_fn: &crate::ir::mir::MirFn,
    param_types: &[(String, Type)],
) -> HashSet<String> {
    let mut out = HashSet::new();
    for (i, (name, ty)) in param_types.iter().enumerate() {
        let returned_record_successor = matches!(ty, Type::Named { .. })
            && mir_fn.params.get(i).is_some_and(|param| {
                result_contains_record_successor_of(&mir_fn.body.node, param.local)
            });
        let collection_graduated = is_owned_collection_candidate(ty)
            && !mir_fn.aliased_slots.get(i).copied().unwrap_or(true);
        if !collection_graduated && !returned_record_successor {
            continue;
        }
        out.insert(aver_name_to_rust(name));
    }
    out
}

/// The legacy representation-backed `Tcp.Connection` carrier maps to its
/// flat root alias. Capability-owned represented records such as
/// `Terminal.Size` take the ordinary module-qualified path below.
fn builtin_dotted_record_rename(type_name: &str) -> Option<&'static str> {
    match type_name {
        "Tcp.Connection" => Some("Tcp_Connection"),
        _ => None,
    }
}

/// Mirror of `RustSourceCallCtx::resolve_module_call` in
/// `toplevel.rs`: find the longest registered module prefix
/// inside a dotted name. Returns `(prefix, suffix)` on hit,
/// `None` when no registered prefix matches.
fn resolve_module_call<'a>(
    dotted: &'a str,
    module_prefixes: &HashSet<String>,
) -> Option<(&'a str, &'a str)> {
    let mut best: Option<(&str, &str)> = None;
    for (dot_idx, _) in dotted.match_indices('.') {
        let prefix = &dotted[..dot_idx];
        let suffix = &dotted[dot_idx + 1..];
        if module_prefixes.contains(prefix)
            && best.is_none_or(|existing| prefix.len() > existing.0.len())
        {
            best = Some((prefix, suffix));
        }
    }
    best
}

/// Resolve a bare record `type_name` (`"Note"`) to the Rust path that
/// names its struct. For a type defined in a `depends`-ed module the
/// symbol table carries a scoped [`TypeKey`] (`scope = "Apps.Notepad.
/// Store"`), so the canonical name is dotted and routes through
/// [`resolve_module_call`] to the module-mangled path
/// (`crate::aver_generated::apps::notepad::store::Note`). For an
/// entry-scope type (`scope = None`) the canonical name is bare and
/// no qualification is needed, so this returns `None` and the caller
/// keeps the verbatim name (resolved in scope by the entry module's
/// own `use`).
///
/// This is the verify-test-path sibling of the `Construct(User)`
/// emit's module-path mangling: a `RecordCreate`/`RecordUpdate` of a
/// cross-module type inside a `#[cfg(test)]` verify module has no
/// glob `use` bringing the dep type into scope, so the reference must
/// be fully qualified. Reuses [`resolve_module_call`] +
/// [`module_prefix_to_rust_path`], the same helpers the runtime
/// cross-module ctor / fn-ref emit uses.
///
/// Identity comes only from `MirRecordCreate.type_id` (the resolver's precise
/// handle — robust against two dep modules sharing a bare type name). A node
/// without identity keeps its source spelling; codegen never searches the
/// whole program by bare name.
fn qualify_record_type(
    type_id: Option<crate::ir::TypeId>,
    _type_name: &str,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let entry = ctx.symbol_table.type_entry_if_present(type_id?)?;
    let canonical = entry.key.canonical();
    let (prefix, suffix) = resolve_module_call(&canonical, ctx.module_prefixes)?;
    Some(format!(
        "{}::{}",
        module_prefix_to_rust_path(prefix),
        suffix
    ))
}

/// Pick the Rust type name for a `RecordCreate` / `RecordUpdate`.
/// Precedence, mirroring HIR's verbatim-type-name shape plus the
/// new cross-module qualification:
///  1. built-in dotted record rename (`Tcp.Connection` →
///     `Tcp_Connection`),
///  2. module-qualified path for a `depends`-ed user record (so a
///     verify-test reference compiles without a glob `use`),
///  3. the verbatim source `type_name` (entry-scope records, in
///     scope via the entry module's own `use`).
fn mir_record_rust_type(
    type_id: Option<crate::ir::TypeId>,
    type_name: &str,
    ctx: &MirEmitCtx<'_>,
) -> String {
    if let Some(renamed) = builtin_dotted_record_rename(type_name) {
        return renamed.to_string();
    }
    if let Some(qualified) = qualify_record_type(type_id, type_name, ctx) {
        return qualified;
    }
    type_name.to_string()
}

/// Does this nominal type carry its `List<Int>` inhabitants as packed
/// bytes? A `Some(false)` and a `None` emit differently — `None` means the
/// question could not be answered, and the caller must refuse rather than
/// pick a representation.
///
/// The answer comes from the type's identity. When the identity is missing
/// the bare source name is a usable key ONLY while it misses the packed
/// table: `packed_sequence_layout_table` drops a name two modules disagree
/// about, so a name that is absent from it is absent for every declaration
/// of that name in the program, and "not packed" is the one answer it can
/// have. A name that HITS the table with no identity behind it is the
/// dangerous direction: the value would be re-typed as `PackedU8` on the
/// strength of a string, and the emitted `.to_int_list()` does not exist on
/// anything else. That is answered `None`, never `Some(true)`.
fn packed_u8_carrier(
    type_id: Option<crate::ir::TypeId>,
    type_name: &str,
    ctx: &MirEmitCtx<'_>,
) -> Option<bool> {
    let Some(codegen) = ctx.codegen else {
        return Some(false);
    };
    if let Some(id) = type_id {
        let bare = ctx.symbol_table.type_entry(id).key.name.as_str();
        return Some(super::uses_packed_u8(codegen, bare));
    }
    let bare = type_name.rsplit('.').next().unwrap_or(type_name);
    if super::uses_packed_u8(codegen, bare) {
        return None;
    }
    Some(false)
}

/// The refusal text for a representation question the backend could not
/// answer. Loud by construction: a recorded `compile_error!` fails
/// `aver compile --target rust` with the type named, where guessing used to
/// emit a method call on a value that has no such method and say nothing.
fn unresolved_packed_carrier(type_name: &str, ctx: &MirEmitCtx<'_>) -> String {
    let message = format!(
        "the type `{type_name}` here has no resolved identity, and a type of that \
         name is stored as packed bytes — the Rust backend will not guess a value's \
         representation from its name; qualify the type or name its module in \
         `depends [...]`"
    );
    match ctx.codegen {
        Some(codegen) => super::toplevel::emit_codegen_error_expr(codegen, message),
        None => message,
    }
}

fn pack_u8_list(value: String) -> String {
    format!(
        "aver_rt::into_packed_u8({value}).expect(\"proof-packed U8 construction escaped its refinement gate\")"
    )
}

fn type_is_int_list(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::List(inner)) if **inner == Type::Int)
}

fn type_is_int_vector(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Vector(inner)) if **inner == Type::Int)
}

/// How many fns the MIR walker can emit
/// standalone vs how many need HIR fallback. Pre-wire-up signal
/// so callers can track walker reach across the shipped corpus
/// without altering the codegen path.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CoverageReport {
    /// Total fn count in the lowered program.
    pub total: usize,
    /// Fns whose entire body the walker emits standalone
    /// (no `None` anywhere in the recursive walk).
    pub mir_covered: usize,
    /// Fns the walker can't emit — the recursive walk hit at
    /// least one variant that returned `None`. Caller would
    /// fall back to the HIR walker in a wire-up.
    pub hir_fallback: usize,
}

impl CoverageReport {
    /// Walker reach as a percentage of total fns. `0.0` when
    /// the program is empty (no fns lowered).
    pub fn ratio(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.mir_covered as f64 / self.total as f64
        }
    }
}

/// Walk every fn in `program` and report walker reach. For each
/// fn, calls [`emit_mir_expr`] on the body and counts
/// `Some` / `None`. Suitable for `--explain-mir-coverage`–style
/// diagnostics; the codegen path itself is untouched.
pub fn coverage_report(program: &MirProgram, emit_ctx: &MirEmitCtx<'_>) -> CoverageReport {
    coverage_report_with_blockers(program, emit_ctx).0
}

/// Same reach measurement as [`coverage_report`], plus a histogram
/// of the *first* construct that blocked each HIR-fallback fn.
///
/// For every fn the walker can't emit, `first_blocker` does the same
/// recursive `emit_mir_expr`-shaped walk but, instead of building a
/// string, returns a stable label for the first `MirExpr` variant /
/// `MirCallee` kind that would have returned `None`. Counting those
/// labels gives a per-wave roadmap: "lower `Match` next" reads
/// straight off the dominant bucket. The returned map is keyed by
/// label and ordered (BTreeMap) for deterministic report output.
///
/// This is diagnostic-only — it does not touch the production emit
/// path, and the walk is the exact mirror of [`emit_mir_expr`] so the
/// blocker it names is the one the wired-up backend would hit.
pub fn coverage_report_with_blockers(
    program: &MirProgram,
    emit_ctx: &MirEmitCtx<'_>,
) -> (
    CoverageReport,
    std::collections::BTreeMap<&'static str, usize>,
) {
    let mut report = CoverageReport::default();
    let mut blockers: std::collections::BTreeMap<&'static str, usize> =
        std::collections::BTreeMap::new();
    for (_, mir_fn) in program.iter() {
        report.total += 1;
        if emit_mir_expr(&mir_fn.body, emit_ctx).is_some() {
            report.mir_covered += 1;
        } else {
            report.hir_fallback += 1;
            let label = first_blocker(&mir_fn.body, emit_ctx).unwrap_or("Unknown");
            *blockers.entry(label).or_insert(0) += 1;
        }
    }
    (report, blockers)
}

/// Recursively find the first construct that makes [`emit_mir_expr`]
/// return `None` for `expr`, and name it with a stable label. Returns
/// `None` only when the whole subtree emits cleanly (the caller treats
/// that as "no blocker"). The traversal order matches
/// `emit_mir_expr`'s argument-evaluation order exactly so the label
/// pins the *same* node the emit walk would have bailed on.
fn first_blocker(expr: &Spanned<MirExpr>, emit_ctx: &MirEmitCtx<'_>) -> Option<&'static str> {
    // Leaf check: if this node emits cleanly on its own, no blocker
    // lives at-or-below it.
    if emit_mir_expr(expr, emit_ctx).is_some() {
        return None;
    }
    // The node (or one of its children) blocks. Recurse into children
    // first so we report the deepest / leftmost actual blocker, not the
    // wrapper that merely propagated a child's `None`.
    match &expr.node {
        MirExpr::Neg(inner) | MirExpr::Return(inner) | MirExpr::Try(inner) => {
            first_blocker(inner, emit_ctx).or(Some(label_for(&expr.node)))
        }
        MirExpr::BinOp(b) => first_blocker(&b.node.lhs, emit_ctx)
            .or_else(|| first_blocker(&b.node.rhs, emit_ctx))
            .or(Some("BinOp")),
        MirExpr::Call(c) => {
            // `Fn`, `Builtin`, `Intrinsic` and `LocalSlot` callees can all
            // emit cleanly (Wave 3a graduated the pure builtins +
            // intrinsics; W6/Stage-0 graduated the first-class `LocalSlot`
            // fn-pointer call), so recurse into the args first and only
            // report the callee kind when every arg emits but the call as a
            // whole still returned `None` (an effectful / unresolved
            // builtin, or a shape the walker can't render).
            for a in &c.node.args {
                if let Some(b) = first_blocker(a, emit_ctx) {
                    return Some(b);
                }
            }
            match &c.node.callee {
                MirCallee::Builtin(_) => Some("Call(Builtin)"),
                MirCallee::Intrinsic(_) => Some("Call(Intrinsic)"),
                MirCallee::Fn(_) => Some("Call(Fn)"),
                MirCallee::LocalSlot { .. } => Some("Call(LocalSlot)"),
            }
        }
        MirExpr::TailCall(tc) => {
            for a in &tc.node.args {
                if let Some(b) = first_blocker(a, emit_ctx) {
                    return Some(b);
                }
            }
            Some("TailCall")
        }
        MirExpr::Tuple(items) | MirExpr::List(items) => {
            for item in items {
                if let Some(b) = first_blocker(item, emit_ctx) {
                    return Some(b);
                }
            }
            Some(label_for(&expr.node))
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                if let Some(b) = first_blocker(k, emit_ctx) {
                    return Some(b);
                }
                if let Some(b) = first_blocker(v, emit_ctx) {
                    return Some(b);
                }
            }
            Some("MapLiteral")
        }
        MirExpr::Let(l) => first_blocker(&l.node.value, emit_ctx)
            .or_else(|| first_blocker(&l.node.body, emit_ctx))
            .or(Some("Let(synthetic)")),
        MirExpr::Project(p) => first_blocker(&p.node.base, emit_ctx).or(Some("Project")),
        MirExpr::RecordCreate(r) => {
            for f in &r.node.fields {
                if let Some(b) = first_blocker(&f.value, emit_ctx) {
                    return Some(b);
                }
            }
            Some("RecordCreate(builtin/Tcp)")
        }
        MirExpr::RecordUpdate(u) => {
            if let Some(b) = first_blocker(&u.node.base, emit_ctx) {
                return Some(b);
            }
            for f in &u.node.updates {
                if let Some(b) = first_blocker(&f.value, emit_ctx) {
                    return Some(b);
                }
            }
            Some("RecordUpdate(builtin/Tcp)")
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                if let Some(b) = first_blocker(a, emit_ctx) {
                    return Some(b);
                }
            }
            Some("Construct")
        }
        MirExpr::IfThenElse(ite) => first_blocker(&ite.node.cond, emit_ctx)
            .or_else(|| first_blocker(&ite.node.then_branch, emit_ctx))
            .or_else(|| first_blocker(&ite.node.else_branch, emit_ctx))
            .or(Some("IfThenElse")),
        MirExpr::Match(m) => {
            if let Some(b) = first_blocker(&m.node.subject, emit_ctx) {
                return Some(b);
            }
            for arm in &m.node.arms {
                if let Some(b) = first_blocker(&arm.body, emit_ctx) {
                    return Some(b);
                }
            }
            // Subject + every arm body emit cleanly, yet the Match as a
            // whole returned `None` — the blocker is the match shape
            // itself (an untranslatable pattern, or a dispatch shape the
            // walker can't reproduce byte-identically yet).
            Some("Match")
        }
        // Variants `emit_mir_expr` never recurses into (it returns
        // `None` immediately): they are themselves the blocker.
        other => Some(label_for(other)),
    }
}

/// Stable histogram label for a `MirExpr` variant. Kept short and
/// variant-named so the report reads as a worklist.
fn label_for(expr: &MirExpr) -> &'static str {
    match expr {
        MirExpr::Literal(_) => "Literal",
        MirExpr::Local(_) => "Local(synthetic)",
        MirExpr::Let(_) => "Let(synthetic)",
        MirExpr::Call(_) => "Call",
        MirExpr::TailCall(_) => "TailCall",
        MirExpr::BinOp(_) => "BinOp",
        MirExpr::Neg(_) => "Neg",
        MirExpr::Match(_) => "Match",
        MirExpr::Construct(_) => "Construct",
        MirExpr::RecordCreate(_) => "RecordCreate",
        MirExpr::RecordUpdate(_) => "RecordUpdate",
        MirExpr::Project(_) => "Project",
        MirExpr::IfThenElse(_) => "IfThenElse",
        MirExpr::Try(_) => "Try",
        MirExpr::List(_) => "List",
        MirExpr::Tuple(_) => "Tuple",
        MirExpr::MapLiteral(_) => "MapLiteral",
        MirExpr::InterpolatedStr(_) => "InterpolatedStr",
        MirExpr::IndependentProduct(_) => "IndependentProduct",
        MirExpr::Return(_) => "Return",
        MirExpr::FnValue(_) => "FnValue",
        MirExpr::Box(_) => "Box",
        MirExpr::Unbox(_) => "Unbox",
    }
}

/// Try to emit Rust source for `expr` directly from MIR.
/// Returns `None` for any variant outside the renderable subset — the
/// signal for the caller to emit a hard codegen diagnostic (the
/// verify-only Oracle/trace residual).
///
/// The per-fn borrow policy (`local_types` / `rc_wrapped` /
/// `borrowed_params`) is threaded through the [`MirEmitCtx`] so the
/// clone / borrow / `Arc::new` decisions are correct for the fn whose
/// body this renders. This is the sole Rust runtime codegen walker.
pub(super) fn emit_mir_expr(expr: &Spanned<MirExpr>, emit_ctx: &MirEmitCtx<'_>) -> Option<String> {
    match &expr.node {
        MirExpr::Literal(lit) => {
            // The MIR const-fold pass collapses `Neg(Literal(273.15))`
            // → `Literal(-273.15)`. HIR never folds — it keeps the
            // `Neg` node and emits `(-273.15f64)` (the `Neg` arm's
            // `(-{inner})` wrapper). Re-introduce that wrapper for a
            // negative numeric literal at expression position so the
            // folded form matches HIR byte-for-byte. (Literal *patterns*
            // don't reach here — they translate to `ResolvedPattern` and
            // emit through the shared `emit_pattern` / dispatch path.)
            match &lit.node {
                // A negative folded Int literal emits the already-signed
                // value directly: `emit_literal` produces
                // `AverInt::from_i64(-N)` (a `const fn`), and `AverInt` has
                // no std `Neg`, so the old `(-{lit})` re-wrap would not
                // compile. `from_i64` accepts `i64::MIN` verbatim, so no
                // `checked_neg` guard is needed.
                crate::ast::Literal::Int(_) => Some(emit_literal(&lit.node)),
                crate::ast::Literal::Float(f) if f.is_sign_negative() => Some(format!(
                    "(-{})",
                    emit_literal(&crate::ast::Literal::Float(-f))
                )),
                _ => Some(emit_literal(&lit.node)),
            }
        }
        MirExpr::Local(spanned_local) => {
            let name = &spanned_local.node.name;
            if name.is_empty() {
                // Synthetic locals (intermediate stmt-chain
                // effectful expressions) carry no source name —
                // the Rust backend can't emit them as idents.
                // Caller falls back to HIR.
                return None;
            }
            Some(aver_name_to_rust(name))
        }
        MirExpr::Neg(inner) => {
            // `Int` negation is `AverInt::neg()` (non-wrapping; `-i64::MIN`
            // promotes to `Big`). `Float` negation keeps the raw `-`. The
            // const-fold pass usually collapses `Neg(Literal)` to a folded
            // literal (handled in the `Literal` arm), so a real `Neg` here is
            // over a non-literal operand carrying an `Int` stamp; an Int
            // literal inner (no stamp) emits an `AverInt`, which also needs
            // `.neg()` (there is no `-` on `AverInt`).
            let inner_is_int = ty_is_int(inner.ty())
                || matches!(
                    &inner.node,
                    MirExpr::Literal(l)
                        if matches!(
                            l.node,
                            crate::ast::Literal::Int(_) | crate::ast::Literal::BigInt(_)
                        )
                );
            let code = emit_mir_expr(inner, emit_ctx)?;
            if inner_is_int {
                Some(format!("{}.neg()", code))
            } else {
                Some(format!("(-{})", code))
            }
        }
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            let l = emit_mir_expr(&bop.lhs, emit_ctx)?;
            let r = emit_mir_expr(&bop.rhs, emit_ctx)?;
            // `Int` arithmetic lowers to the non-wrapping `AverInt` methods
            // (`+ - *` → `.add/.sub/.mul(&rhs)`, `/` → `.div_trunc(&rhs)`),
            // since `AverInt` has no operator-trait impls. Comparisons stay
            // raw operators (`AverInt` derives `PartialEq`/`PartialOrd`).
            // Detected from the operand stamps: arithmetic over `Int`
            // operands carries an `Int` stamp on both sides.
            let int_arith = matches!(bop.op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div)
                && (ty_is_int(bop.lhs.ty()) || ty_is_int(bop.rhs.ty()));
            if int_arith {
                // Int unboxing: emit native integer arithmetic ONLY when the
                // WHOLE result is proven bare `i64`. SOUNDNESS (BUG 2): the
                // gate is `mir_expr_is_bare_i64(expr)` — the interval-checked
                // predicate over the ENTIRE `Add`/`Sub`/`Mul` tree, NOT a
                // per-operand check. `n + i64::MAX` has each operand bare but
                // a result interval OUTSIDE `i64`, so it fails this gate and
                // falls through to the boxed `AverInt` path below (which
                // converts each bare operand via `from_i64`). A missing fact
                // also keeps the boxed path. For `Add`/`Sub`/`Mul` we check
                // the whole expression; `Div` is not modelled by the interval
                // analysis (raw `/` over `Int` is a source type error — the
                // builtin is `Int.div`), so it keeps the per-operand check
                // plus the zero-divisor trap.
                let whole_bare = match bop.op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul => mir_expr_is_bare_i64(expr, emit_ctx),
                    BinOp::Div => {
                        mir_expr_is_bare_i64(&bop.lhs, emit_ctx)
                            && mir_expr_is_bare_i64(&bop.rhs, emit_ctx)
                    }
                    _ => false,
                };
                if whole_bare
                    && let Some(lb) = emit_bare_i64(&bop.lhs)
                    && let Some(rb) = emit_bare_i64(&bop.rhs)
                {
                    match bop.op {
                        BinOp::Add => return Some(format!("({} + {})", lb, rb)),
                        BinOp::Sub => return Some(format!("({} - {})", lb, rb)),
                        BinOp::Mul => return Some(format!("({} * {})", lb, rb)),
                        BinOp::Div => {
                            // Truncating `/` over `i64`, keeping the
                            // zero-divisor trap (the `AverInt::div_trunc`
                            // parity). Bind the divisor once so it is
                            // evaluated a single time.
                            return Some(format!(
                                "{{ let __d = {}; if __d == 0i64 {{ panic!(\"divide by zero\") }} else {{ ({}) / __d }} }}",
                                rb, lb
                            ));
                        }
                        _ => {}
                    }
                }
                // Mixed-representation boundary: this is the boxed
                // `AverInt` arithmetic path, but ONE operand may be a bare
                // `i64` (e.g. `acc * n` where `acc` stays boxed and `n`
                // went bare). ETAP-2 SLICE 1: the bare→`AverInt` boundary is
                // now an EXPLICIT `Box(n)` node the rewrite inserted, so
                // `emit_mir_expr` already produced the `from_i64(n)` text for
                // `l` / `r` — no codegen-side coercion here.
                let method = match bop.op {
                    BinOp::Add => "add",
                    BinOp::Sub => "sub",
                    BinOp::Mul => "mul",
                    // Raw `/` over `Int` is a type error in source (Int.div
                    // is the builtin), so this is normally unreachable; if a
                    // bare `Div` does reach here it takes the truncating
                    // semantics of the `/` operator. `div_trunc` returns
                    // `Option` (None only on zero divisor); a bare `/` over
                    // ℤ otherwise can't fail, so `.expect` mirrors the
                    // runtime trap a zero divisor would raise.
                    BinOp::Div => {
                        return Some(format!(
                            "{}.div_trunc(&{}).expect(\"divide by zero\")",
                            l, r
                        ));
                    }
                    _ => unreachable!("int_arith only set for Add/Sub/Mul/Div"),
                };
                // The method receiver is borrowed for the duration of the
                // call. If a nested RHS expression consumes that local (for
                // example `n * factorial(n - 1)` in
                // `examples/core/big_integers.av`), Rust rejects the receiver
                // borrow followed by the move inside the argument. Clone only
                // for that overlap; a direct `x + x` RHS is itself borrowed by
                // this method call and remains `x.add(&x)`.
                let lhs_rhs_move_overlap = local_of(&bop.lhs.node).is_some_and(|lhs| {
                    !matches!(&bop.rhs.node, MirExpr::Local(_))
                        && mir_expr_contains_last_use_of_slot(&bop.rhs.node, lhs.slot)
                });
                let l = if lhs_rhs_move_overlap {
                    mir_maybe_clone(l, &bop.lhs.node, emit_ctx)
                } else {
                    l
                };
                return Some(format!("{}.{}(&{})", l, method, r));
            }
            // Int unboxing: a comparison (`==`, `<`, `<=`, `>`, `>=`, `!=`)
            // between two proven-bare `i64` operands emits a raw operator
            // over `i64` (rather than `&AverInt == &AverInt`). `AverInt`
            // derives `PartialEq`/`PartialOrd`, and so does `i64`, so the
            // boxed path below also works — but on a bare subject the boxed
            // path would compare a bare `i64` against an `AverInt`, a type
            // error. So when both sides are bare we MUST take the raw path.
            if matches!(
                bop.op,
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
            ) && mir_expr_is_bare_i64(&bop.lhs, emit_ctx)
                && mir_expr_is_bare_i64(&bop.rhs, emit_ctx)
                && let Some(lb) = emit_bare_i64(&bop.lhs)
                && let Some(rb) = emit_bare_i64(&bop.rhs)
            {
                let op = match bop.op {
                    BinOp::Eq => "==",
                    BinOp::Neq => "!=",
                    BinOp::Lt => "<",
                    BinOp::Gt => ">",
                    BinOp::Lte => "<=",
                    BinOp::Gte => ">=",
                    _ => unreachable!(),
                };
                return Some(format!("({} {} {})", lb, op, rb));
            }
            let op_str = match bop.op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            // Read type stamps to disambiguate
            // numeric `+` from `AverStr` concat. Same shape HIR
            // walker takes via `ectx.expr_is_numeric`. HIR's
            // disambiguation is `expr_is_numeric(lhs) ||
            // expr_is_numeric(rhs)` → plain add; otherwise the
            // `AverStr` concat path, where the LHS is run through
            // `maybe_clone` (it's consumed by `Add`, the RHS is
            // borrowed via `&` for `Add<&AverStr>`). Mirror that
            // exactly so Str + Str matches byte-for-byte.
            //
            // GENUINE DIVERGENCE (Wave 4 boundary — left on HIR
            // fallback by design): the MIR walker reads the operand's
            // *type stamp* (correct for let-bound locals + match
            // bindings + user-fn-call returns), while HIR's
            // `expr_is_numeric` reads `ectx.local_types`, which only
            // carries *params*. So for `left + right` where `left` /
            // `right` are `Int`s bound by `let left = leftRes?` (not
            // params), HIR misclassifies them as non-numeric and emits
            // the concat-shaped `(left + &right)`; MIR correctly emits
            // `(left + right)`. Both COMPILE and produce identical
            // results (`i64: Add<&i64>` exists in std), so neither is
            // unsound — MIR is just cleaner. Matching HIR here would
            // mean deliberately ignoring MIR's correct stamps, so these
            // fns (`applyEvalOp`, `validateAndCombine[NoOp]`, `size`,
            // `sumDirect`, `countS`'s `&str` deref, …) stay on HIR
            // fallback. The eventual HIR retirement fixes HIR (give it
            // let-local types), not MIR.
            if matches!(bop.op, BinOp::Add)
                && !ty_is_numeric(bop.lhs.ty())
                && !ty_is_numeric(bop.rhs.ty())
            {
                let l = mir_maybe_clone(l, &bop.lhs.node, emit_ctx);
                Some(format!("({} + &{})", l, r))
            } else if matches!(bop.op, BinOp::Eq | BinOp::Neq) {
                // HIR derefs `AverStr` (Rc<str>) to `&str` when one
                // side is a string literal, since `Rc<str>` doesn't
                // impl `PartialEq<&str>`. Mirror that so string
                // equality matches.
                Some(mir_emit_equality(bop, &l, &r, op_str, emit_ctx))
            } else {
                Some(format!("({} {} {})", l, op_str, r))
            }
        }
        MirExpr::Call(spanned_call) => {
            let call = &spanned_call.node;
            match &call.callee {
                MirCallee::Fn(fn_id) => {
                    // Resolve canonical name through the same
                    // symbol table the HIR walker uses, then emit
                    // the call exactly as HIR's
                    // `emit_named_function_call` does: each arg goes
                    // through `borrow_arg` (when the callee's i-th
                    // param is borrowed-by-default `&T`) or
                    // `clone_arg` (owned), and the module-qualified
                    // head is path-mangled via `resolve_module_call`.
                    let name = emit_ctx.symbol_table.fn_entry(*fn_id).key.canonical();
                    emit_named_call_to(&name, Some(*fn_id), &call.args, emit_ctx)
                }
                // Resolve the interned dotted name and dispatch. Every host
                // operation is capability-backed; the remaining builtin
                // table is pure.
                // An out-of-range id (a lowering-invariant violation we
                // tolerate defensively) returns `None` → HIR fallback.
                MirCallee::Builtin(id) => {
                    let name = emit_ctx.mir_builtins.get(id.0 as usize)?.as_str();
                    if let Some(operation) = emit_ctx
                        .codegen
                        .and_then(|codegen| codegen.capabilities.operation(name))
                    {
                        return emit_mir_capability_call(operation, &call.args, emit_ctx);
                    }
                    emit_mir_builtin_call(name, &call.args, emit_ctx)
                }
                // Wave 3a: the 5 deforestation intrinsics (buffer build
                // + `__to_str`). Args are by-value (no clone / borrow),
                // mirroring `emit_builtin_call_inner`'s intrinsic arms.
                // The Rust backend deforests differently, so a buffered
                // fn's MIR shape may not byte-match HIR — the parity
                // gate then falls back safely.
                MirCallee::Intrinsic(intrinsic) => emit_mir_intrinsic_call(
                    *intrinsic,
                    &call.args,
                    expr.ty(),
                    mir_expr_is_bare_i64(expr, emit_ctx),
                    emit_ctx,
                ),
                // First-class fn value held in a slot — calling a `Fn(..)`
                // param. Post-#379 the slot holds a plain fn-pointer (no
                // closures / `dyn Fn` — `Type::Fn` is param-only), so this
                // emits the direct call-by-name `name(args…)`. Mirror of
                // HIR's `CallPlan::Dynamic` (`emit_fn_call_with_options`):
                // the head is `aver_name_to_rust(name)` and every arg goes
                // through `clone_arg`.
                MirCallee::LocalSlot { name, .. } => {
                    let func = aver_name_to_rust(name);
                    let mut arg_strs = Vec::with_capacity(call.args.len());
                    for a in &call.args {
                        arg_strs.push(mir_clone_arg(
                            emit_mir_expr(a, emit_ctx)?,
                            &a.node,
                            emit_ctx,
                        ));
                    }
                    Some(format!("{}({})", func, arg_strs.join(", ")))
                }
            }
        }
        MirExpr::Return(inner) => Some(format!("return {}", emit_mir_expr(inner, emit_ctx)?)),
        MirExpr::TailCall(spanned_tc) => {
            // Tail call outside a self-TCO loop
            // emits as a regular function call — mirror of HIR's
            // `ResolvedExpr::TailCall` outside-loop branch
            // (which the resolver leaves intact and the emitter
            // routes through `emit_named_function_call`). When
            // the surrounding fn IS in a TCO loop, HIR rewrites
            // it to `continue` + param assigns — the walker
            // can't see that without `ectx`, so the wire-up
            // layer's parity check is the safety net (mismatch
            // → fall back to HIR).
            let tc = &spanned_tc.node;
            let name = emit_ctx.symbol_table.fn_entry(tc.target).key.canonical();
            emit_named_call(&name, &tc.args, emit_ctx)
        }
        MirExpr::Try(inner) => {
            // `value?` propagation. `?` (the `Try` trait) is implemented
            // for an owned `Result<T, E>`, not a borrowed `&Result`. When
            // the inner is a borrowed-by-default `Result`-typed param
            // (`fn foldNote(acc: &Result<…>, …)` then `acc?`), append `?`
            // to a *cloned* owned value rather than the `&Result` read —
            // otherwise rustc rejects `&Result` as not implementing
            // `Try`. `mir_clone_arg` produces the right owning shape
            // (`.clone()` for a borrowed param, `(*x).clone()` for an
            // rc-wrapped pass-through), and leaves an owned last-use local
            // a bare move — exactly what `?` consumes. Mirror of HIR's
            // `ErrorProp` once the inner is in an owning position.
            //
            // Verify-case ctx (`try_err_to_string`): the enclosing
            // `#[test]` fn returns `Result<(), String>` while the operand
            // errors with a generated type (`AverStr`, a user error
            // struct/enum, …) that has no `From` into `String` — a bare
            // `?` fails E0277. Debug-format the error at the `?` boundary
            // so every generated error shape converts.
            let inner_code = emit_mir_expr(inner, emit_ctx)?;
            let owned = mir_clone_arg(inner_code, &inner.node, emit_ctx);
            if emit_ctx.try_err_to_string {
                Some(format!(
                    "{}.map_err(|err| format!(\"{{:?}}\", err))?",
                    owned
                ))
            } else {
                Some(format!("{}?", owned))
            }
        }
        MirExpr::Tuple(items) => {
            // `(a, b, c)` tuple literal. Mirror
            // of HIR's `ResolvedExpr::Tuple` emit — each element
            // routed through `clone_arg` for ownership.
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(mir_clone_arg(
                    emit_mir_expr(item, emit_ctx)?,
                    &item.node,
                    emit_ctx,
                ));
            }
            Some(format!("({})", parts.join(", ")))
        }
        MirExpr::List(items) => {
            // `[a, b, c]` list literal. Mirror
            // of HIR's `ResolvedExpr::List` — empty case folds
            // to `aver_rt::AverList::empty()`, non-empty to
            // `from_vec(vec![...])` with `clone_arg` elements.
            let list_type = if type_is_int_list(expr.ty()) {
                "aver_rt::AverIntList"
            } else {
                "aver_rt::AverList"
            };
            if items.is_empty() {
                return Some(format!("{list_type}::empty()"));
            }
            let mut parts = Vec::with_capacity(items.len());
            for item in items {
                parts.push(mir_clone_arg(
                    emit_mir_expr(item, emit_ctx)?,
                    &item.node,
                    emit_ctx,
                ));
            }
            Some(format!("{list_type}::from_vec(vec![{}])", parts.join(", ")))
        }
        MirExpr::MapLiteral(entries) => {
            // `{"k" => v, …}` map literal.
            // Mirror of HIR's `ResolvedExpr::MapLiteral` — empty
            // → `HashMap::new()`, non-empty →
            // `vec![(k, v), …].into_iter().collect::<HashMap<_, _>>()`,
            // keys + values routed through `clone_arg`.
            if entries.is_empty() {
                return Some("HashMap::new()".to_string());
            }
            let mut parts = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                let key_str = mir_clone_arg(emit_mir_expr(k, emit_ctx)?, &k.node, emit_ctx);
                let val_str = mir_clone_arg(emit_mir_expr(v, emit_ctx)?, &v.node, emit_ctx);
                parts.push(format!("({}, {})", key_str, val_str));
            }
            Some(format!(
                "vec![{}].into_iter().collect::<HashMap<_, _>>()",
                parts.join(", ")
            ))
        }
        MirExpr::Let(spanned_let) => {
            // `let binding = value; body` →
            // Rust block-expression `{ let x @ _ = value; body }`.
            // A discarded intermediate (an effectful `Stmt::Expr` at
            // non-tail position, or a `_ = effect()` discard) carries
            // `binding_name.is_empty()` — there's no source ident to
            // bind, so the value is emitted as a bare statement
            // (`{ value; body }`), evaluated for its effect with the
            // result dropped. Mirror of HIR's discarded-`Stmt::Expr`
            // shape.
            let let_node = &spanned_let.node;
            // ETAP-2 SLICE 1: the binding-crossing representation BOUNDARY
            // (defect esc_match — a raw value bound into a boxed slot) is now
            // an EXPLICIT `Box`/`Unbox` node the `bare_i64_rewrite` pass
            // wrapped the value in. Codegen lowers it via the `Box`/`Unbox`
            // arms — no `from_i64` decision here. It still RENDERS a bare
            // binding's value raw (representation): a `i64` binding takes a
            // native `i64` literal / leaf / arithmetic.
            let value = if emit_ctx.bare.is_bare(let_node.binding) {
                emit_bare_i64(&let_node.value)
                    .or_else(|| emit_mir_expr(&let_node.value, emit_ctx))?
            } else {
                // The binding is an owning position — see
                // [`emit_mir_binding_value`].
                emit_mir_binding_value(&let_node.value, emit_ctx)?
            };
            let body = emit_mir_expr(&let_node.body, emit_ctx)?;
            if let_node.binding_name.is_empty() {
                Some(format!("{{ {}; {} }}", value, body))
            } else {
                let name = explicit_binding_pattern(&let_node.binding_name);
                Some(format!("{{ let {} = {}; {} }}", name, value, body))
            }
        }
        MirExpr::Project(spanned_proj) => {
            // `base.field` projection. Mirror of
            // HIR's `ResolvedLeafOp::FieldAccess` emit shape —
            // emit_expr(base) + "." + aver_name_to_rust(field).
            // No clone insertion here; the HIR walker handles
            // that via `maybe_clone` at outer call sites.
            let proj = &spanned_proj.node;
            // A cross-module first-class fn reference used as a value
            // (`HttpServer.listen(port, Apps.Notepad.Routes.handleRequest)`)
            // lowers to a `Project` chain over a `FnValue` head
            // (`Project(Project(FnValue("Apps"), "Notepad"), "Routes"), "handleRequest")`)
            // because the resolver leaves the leading segment an `Ident`
            // and the rest dotted `Attr`. Collapse such a chain back to
            // the canonical dotted name and, when it names a registered
            // module-qualified symbol, emit the path-mangled static ref
            // (`crate::aver_generated::apps::notepad::routes::handleRequest`)
            // exactly as the `MirCallee::Fn` call path does — instead of
            // the verbatim `Apps.Notepad.Routes.handleRequest` field
            // access, which is not a valid Rust path. The HIR walker saw
            // this as a single `StaticRef(full_name)`; on MIR the chain is
            // re-flattened here.
            if let Some(dotted) = collapse_fnvalue_projection(&expr.node)
                && (dotted == "BranchPath.Root"
                    || resolve_module_call(&dotted, emit_ctx.module_prefixes).is_some())
            {
                return Some(emit_mir_static_ref(&dotted, emit_ctx));
            }
            let base = emit_mir_expr(&proj.base, emit_ctx)?;
            let projected = format!("{}.{}", base, aver_name_to_rust(&proj.field));
            let Some(Type::Named { id, name }) = proj.base.ty() else {
                return Some(projected);
            };
            match packed_u8_carrier(*id, name, emit_ctx) {
                Some(true) => Some(format!("({projected}).to_int_list()")),
                Some(false) => Some(projected),
                None => Some(unresolved_packed_carrier(name, emit_ctx)),
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            // `T { field = v, … }` record literal.
            // Mirror of HIR's `ResolvedExpr::RecordCreate` emit
            // shape exactly — HIR reads the source-level
            // `type_name` string (verbatim on `MirRecordCreate`)
            // and only special-cases `Tcp.Connection` → the
            // re-exported `Tcp_Connection` struct. Fields route
            // through `clone_arg`.
            let rec = &spanned_rec.node;
            let rust_type = mir_record_rust_type(rec.type_id, &rec.type_name, emit_ctx);
            let Some(packed_u8) = packed_u8_carrier(rec.type_id, &rec.type_name, emit_ctx) else {
                return Some(unresolved_packed_carrier(&rec.type_name, emit_ctx));
            };
            let mut parts = Vec::with_capacity(rec.fields.len());
            for f in &rec.fields {
                let mut val = match emit_owned_record_field_successor(None, f, emit_ctx) {
                    Some(val) => val,
                    None => {
                        mir_clone_arg(emit_mir_expr(&f.value, emit_ctx)?, &f.value.node, emit_ctx)
                    }
                };
                if packed_u8 {
                    val = pack_u8_list(val);
                }
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            Some(format!("{} {{ {} }}", rust_type, parts.join(", ")))
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            // `T.update(base, field = v, …)` →
            // `{type_name} { field: value, …, ..base }`. Same
            // verbatim-type-name + Tcp.Connection rename as
            // RecordCreate; base + updates route through
            // `clone_arg`.
            let upd = &spanned_upd.node;
            let rust_type = mir_record_rust_type(upd.type_id, &upd.type_name, emit_ctx);
            let Some(packed_u8) = packed_u8_carrier(upd.type_id, &upd.type_name, emit_ctx) else {
                return Some(unresolved_packed_carrier(&upd.type_name, emit_ctx));
            };
            let mut parts = Vec::with_capacity(upd.updates.len());
            let mut moved_replaced_field = false;
            for f in &upd.updates {
                let update_base = local_of(&upd.base.node).map(|base| base.slot);
                let val = match emit_owned_record_field_successor(update_base, f, emit_ctx) {
                    Some(val) => {
                        moved_replaced_field = true;
                        val
                    }
                    None => {
                        mir_clone_arg(emit_mir_expr(&f.value, emit_ctx)?, &f.value.node, emit_ctx)
                    }
                };
                let val = if packed_u8 { pack_u8_list(val) } else { val };
                parts.push(format!("{}: {}", aver_name_to_rust(&f.name), val));
            }
            // A specialized field successor partially moves the replaced
            // field from an owned record. The `..base` update then moves only
            // the remaining fields, which Rust permits. Cloning `base` here
            // would both be unnecessary and fail after the partial move.
            let emitted_base = emit_mir_expr(&upd.base, emit_ctx)?;
            let base = if moved_replaced_field {
                emitted_base
            } else {
                mir_clone_arg(emitted_base, &upd.base.node, emit_ctx)
            };
            Some(format!(
                "{} {{ {}, ..{} }}",
                rust_type,
                parts.join(", "),
                base
            ))
        }
        MirExpr::Construct(spanned_ctor) => {
            // Built-in ctors emit Result / Option wrappers; user
            // ctors resolve through the symbol table for
            // module-qualified path mangling. Both mirror HIR's
            // `clone_arg` on every arg; the User-ctor path also
            // wraps recursive (self-typed) fields in
            // `std::sync::Arc::new(...)` via
            // `constructor_boxed_positions` so recursive types
            // (`Tree.Node(left: Tree, …)`) emit byte-identical to
            // HIR's `emit_type_constructor_call`.
            let con = &spanned_ctor.node;
            match con.ctor {
                MirCtor::Builtin(builtin) => {
                    let (name, takes_arg) = match builtin {
                        BuiltinCtor::ResultOk => ("Ok", true),
                        BuiltinCtor::ResultErr => ("Err", true),
                        BuiltinCtor::OptionSome => ("Some", true),
                        BuiltinCtor::OptionNone => ("None", false),
                    };
                    if !takes_arg {
                        // `Option.None` — no args, no parens.
                        return Some(name.to_string());
                    }
                    let mut args = Vec::with_capacity(con.args.len());
                    for a in &con.args {
                        args.push(mir_clone_arg(
                            emit_mir_expr(a, emit_ctx)?,
                            &a.node,
                            emit_ctx,
                        ));
                    }
                    Some(format!("{}({})", name, args.join(", ")))
                }
                MirCtor::User(ctor_id) => {
                    // Resolve `CtorId` → owning type → variant name
                    // via the symbol table, then route the
                    // qualified type name through
                    // `resolve_module_call` for module-path
                    // mangling. Mirror of HIR's
                    // `emit_type_constructor_call`, including the
                    // boxed-position `Arc::new` on recursive fields
                    // (queried via `constructor_boxed_positions`,
                    // keyed by the `Type.Variant` name).
                    let ctor_entry = emit_ctx.symbol_table.ctor_entry(ctor_id);
                    let variant_name = ctor_entry.name.clone();
                    let type_entry = emit_ctx.symbol_table.type_entry(ctor_entry.owning_type);
                    let qualified = type_entry.key.canonical();
                    let boxed_positions = match emit_ctx.codegen {
                        Some(cg) => {
                            let ctor_name = format!("{}.{}", qualified, variant_name);
                            constructor_boxed_positions(&ctor_name, cg)
                        }
                        // Coverage path: no ctx → no boxed-position
                        // info. The parity gate isn't active here
                        // (coverage only reads Some/None), so an
                        // empty set is fine.
                        None => HashSet::new(),
                    };
                    let mut args = Vec::with_capacity(con.args.len());
                    for (idx, a) in con.args.iter().enumerate() {
                        let arg = mir_clone_arg(emit_mir_expr(a, emit_ctx)?, &a.node, emit_ctx);
                        if boxed_positions.contains(&idx) {
                            args.push(format!("std::sync::Arc::new({})", arg));
                        } else {
                            args.push(arg);
                        }
                    }
                    let args_str = args.join(", ");
                    // HIR emits a nullary variant as a unit variant
                    // (`E::Point`, no parens). Mirror that so
                    // zero-arg ctors match.
                    let head = if let Some((prefix, suffix)) =
                        resolve_module_call(&qualified, emit_ctx.module_prefixes)
                    {
                        format!("{}::{}", module_prefix_to_rust_path(prefix), suffix)
                    } else {
                        qualified
                    };
                    if con.args.is_empty() {
                        Some(format!("{}::{}", head, variant_name))
                    } else {
                        Some(format!("{}::{}({})", head, variant_name, args_str))
                    }
                }
            }
        }
        MirExpr::IfThenElse(spanned_ite) => emit_mir_if_then_else(&spanned_ite.node, emit_ctx),
        MirExpr::Match(spanned_match) => emit_mir_match(&spanned_match.node, emit_ctx),
        MirExpr::IndependentProduct(spanned_ip) => {
            emit_mir_independent_product(&spanned_ip.node, emit_ctx)
        }
        // A fn referenced as a *value* (`callWith(dbl)` passes `dbl`).
        // Post-#379, a fn value only ever enters through a `Fn(..)` param,
        // so the name is always a plain fn name — but mirror HIR's
        // `ResolvedLeafOp::StaticRef` in full (incl. the variant-vs-fn
        // refinement + module-path mangling) so the emit is byte-identical.
        // The VM does the same (`compile_ident` → `symbol_ref`).
        MirExpr::FnValue(name) => Some(emit_mir_static_ref(name, emit_ctx)),
        // ETAP-2 representation boundaries (inserted by the
        // `bare_i64::rewrite_for_rust` MIR->MIR pass). Codegen no longer
        // DECIDES where a boundary goes — the rewrite already inserted it —
        // it just lowers the node:
        //   Box(x)   — x evaluates to a raw `i64`; box it into `AverInt`.
        //   Unbox(x) — x evaluates to an `AverInt`; narrow to raw `i64`
        //              via the checked `to_i64()` (the analysis proved the
        //              value fits, so the `expect` never fires).
        MirExpr::Box(inner) => {
            let raw = emit_mir_expr(inner, emit_ctx)?;
            Some(format!("aver_rt::AverInt::from_i64({})", raw))
        }
        MirExpr::Unbox(inner) => {
            let boxed = emit_mir_expr(inner, emit_ctx)?;
            Some(format!(
                "{}.to_i64().expect(\"Int out of i64 range\")",
                boxed
            ))
        }
        _ => None,
    }
}

/// Emit a persistent Map successor by partially moving the field replaced by
/// an owned record update.
///
/// `Store.update(store, values = Map.set(store.values, ...))` may become
/// `Store { values: store.values.insert_owned(...), ..store }`: the explicit
/// `values` initializer consumes that field, while Rust's struct-update tail
/// moves only the remaining fields. The exact same-field guard is load-bearing;
/// moving `store.values` to initialize a *different* field would leave
/// `..store` needing an already-moved field.
fn emit_owned_record_field_successor(
    required_base: Option<LocalId>,
    field: &crate::ir::mir::MirRecordField,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let MirExpr::Call(call) = &field.value.node else {
        return None;
    };
    let MirCallee::Builtin(builtin) = call.node.callee else {
        return None;
    };
    let name = ctx
        .mir_builtins
        .get(builtin.0 as usize)
        .map(String::as_str)?;
    if !matches!(name, "Map.set" | "Map.remove") {
        return None;
    }

    let target = call.node.args.first()?;
    let MirExpr::Project(project) = &target.node else {
        return None;
    };
    let target_base = local_of(&project.node.base.node)?;
    if required_base.is_some_and(|base| target_base.slot != base)
        || !ctx.is_owned_param(&target_base.name)
        || !target_base.last_use
        || project.node.field != field.name
    {
        return None;
    }

    let receiver = emit_mir_expr(target, ctx)?;
    match name {
        "Map.set" if call.node.args.len() == 3 => {
            let key = mir_clone_arg(
                emit_mir_expr(&call.node.args[1], ctx)?,
                &call.node.args[1].node,
                ctx,
            );
            let value = mir_clone_arg(
                emit_mir_expr(&call.node.args[2], ctx)?,
                &call.node.args[2].node,
                ctx,
            );
            Some(format!("{}.insert_owned({}, {})", receiver, key, value))
        }
        "Map.remove" if call.node.args.len() == 2 => {
            let key = emit_mir_expr(&call.node.args[1], ctx)?;
            Some(format!("{}.remove_owned(&{})", receiver, key))
        }
        _ => None,
    }
}

/// Emit one program-defined or standard capability call through the
/// native provider registry generated for this Rust artifact.
///
/// Policy parity with the VM: when the project carries an `aver.toml`
/// policy (and the replay runtime is not the active door), an operation
/// in a policy-guarded namespace (`Http.`/`Disk.`/`Env.`) checks its
/// first String argument through `aver_policy` BEFORE the provider
/// invoke. The check runs on the encoded `ProviderValue` so the
/// argument is still evaluated exactly once.
fn emit_mir_capability_call(
    operation: &crate::capability::CapabilityOperation,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let return_type = match ctx.codegen {
        Some(codegen) => super::types::type_to_rust_scoped(
            &operation.return_type,
            codegen,
            ctx.current_module_scope,
        ),
        None => super::types::type_to_rust(&operation.return_type),
    };
    let minted = operation
        .minted_resource
        .as_deref()
        .map(|name| format!("Some({name:?})"))
        .unwrap_or_else(|| "None".to_string());
    let expected = operation.return_type.display();

    // The policy door only exists when the project renders `aver_policy`
    // and the replay runtime is not the active wrap.
    let policy_helper = ctx
        .codegen
        .filter(|codegen| codegen.policy.is_some() && !codegen.emit_replay_runtime)
        .and_then(|_| super::builtins::policy_check_helper(&operation.canonical_name));

    let mut owned_args = Vec::with_capacity(args.len());
    for arg in args {
        owned_args.push(mir_clone_arg(emit_mir_expr(arg, ctx)?, &arg.node, ctx));
    }
    let owned_arg_types = operation
        .params
        .iter()
        .map(|(_, ty)| match ctx.codegen {
            Some(codegen) => {
                super::types::type_to_rust_scoped(ty, codegen, ctx.current_module_scope)
            }
            None => super::types::type_to_rust(ty),
        })
        .collect::<Vec<_>>();

    let invoke_with = |names: &[String]| {
        let encoded = names
            .iter()
            .map(|name| {
                format!(
                    "crate::provider_support::encode({}, {:?})",
                    name, operation.module
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "crate::provider_support::invoke::<{}>({:?}, {:?}, vec![{}], {}, {:?})",
            return_type, operation.module, operation.canonical_name, encoded, minted, expected
        )
    };

    if let Some(helper) = policy_helper.filter(|_| !owned_args.is_empty()) {
        // Argument 0 is evaluated, encoded once, policy-checked as the
        // encoded String, then moved into the invoke vec — one value
        // from one evaluation, exactly like the builtin seam's shared
        // temp. It skips `encode` on the invoke because it is already
        // a `ProviderValue`.
        let names = (0..owned_args.len())
            .map(|index| format!("__provider_arg{index}"))
            .collect::<Vec<_>>();
        let mut lines = vec!["{".to_string()];
        lines.push("    crate::cancel_checkpoint();".to_string());
        for ((name, value), ty) in names.iter().zip(&owned_args).zip(&owned_arg_types) {
            lines.push(format!("    let {name}: {ty} = {value};"));
        }
        lines.push(format!(
            "    let {} = crate::provider_support::encode({}, {:?});",
            names[0], names[0], operation.module
        ));
        lines.push(format!(
            "    if let aver_rt::provider::ProviderValue::String(__policy_path) = &{} {{ {}({:?}, __policy_path).expect(\"aver.toml policy violation\"); }}",
            names[0], helper, operation.canonical_name
        ));
        let mut invoke_args = vec![names[0].clone()];
        invoke_args.extend(names[1..].iter().map(|name| {
            format!(
                "crate::provider_support::encode({name}, {:?})",
                operation.module
            )
        }));
        lines.push(format!(
            "    crate::provider_support::invoke::<{}>({:?}, {:?}, vec![{}], {}, {:?})",
            return_type,
            operation.module,
            operation.canonical_name,
            invoke_args.join(", "),
            minted,
            expected
        ));
        lines.push("}".to_string());
        return Some(lines.join("\n"));
    }

    if !operation.is_effectful()
        || !ctx
            .codegen
            .is_some_and(|codegen| codegen.emit_replay_runtime)
    {
        let names = (0..owned_args.len())
            .map(|index| format!("__provider_arg{index}"))
            .collect::<Vec<_>>();
        let mut lines = vec!["{".to_string()];
        // Effectful provider calls cross a host boundary, so they take
        // the same cancel checkpoint every effectful builtin takes —
        // without it, a cancelled `!`/`?!` branch could no longer
        // interrupt a service that moved onto a provider.
        if operation.is_effectful() {
            lines.push("    crate::cancel_checkpoint();".to_string());
        }
        for ((name, value), ty) in names.iter().zip(&owned_args).zip(&owned_arg_types) {
            lines.push(format!("    let {name}: {ty} = {value};"));
        }
        lines.push(format!("    {}", invoke_with(&names)));
        lines.push("}".to_string());
        return Some(lines.join("\n"));
    }

    let replay = operation.replay?.as_str();
    let names = (0..owned_args.len())
        .map(|index| format!("__provider_arg{index}"))
        .collect::<Vec<_>>();
    let json_args = names
        .iter()
        .map(|name| format!("crate::aver_replay::ReplayValue::to_replay_json(&{name})"))
        .collect::<Vec<_>>()
        .join(", ");
    let mut lines = vec!["{".to_string()];
    for ((name, value), ty) in names.iter().zip(&owned_args).zip(&owned_arg_types) {
        lines.push(format!("    let {name}: {ty} = {value};"));
    }
    lines.push("    crate::cancel_checkpoint();".to_string());
    lines.push(format!(
        "    crate::aver_replay::invoke_capability_effect({:?}, {:?}, vec![{}], || {})",
        operation.canonical_name,
        replay,
        json_args,
        invoke_with(&names)
    ));
    lines.push("}".to_string());
    Some(lines.join("\n"))
}

/// Reconstruct the dotted source name of a `Project` chain whose head
/// is a `FnValue` — e.g. `Project(Project(FnValue("Apps"), "Notepad"),
/// "Routes")` → `"Apps.Notepad.Routes"`. Returns `None` for any chain
/// whose head is not a `FnValue` (a genuine record-field access). Used
/// to recover a cross-module first-class fn reference that the resolver
/// split into an `Ident` head plus dotted `Attr` tail.
fn collapse_fnvalue_projection(expr: &MirExpr) -> Option<String> {
    match expr {
        MirExpr::FnValue(name) => Some(name.clone()),
        MirExpr::Project(p) => {
            let base = collapse_fnvalue_projection(&p.node.base.node)?;
            Some(format!("{}.{}", base, p.node.field))
        }
        _ => None,
    }
}

/// Mirror of HIR's `ResolvedLeafOp::StaticRef` emit
/// (`src/codegen/rust/expr.rs`): a fn / variant referenced as a value.
/// Refines a dotted name that resolves to a known user-defined variant to
/// the Rust enum-variant form (`Shape::Point`); otherwise emits the
/// module-mangled fn reference (`Fibonacci::fib`) or the bare
/// `aver_name_to_rust(name)`. `Option.None` / `None` collapse to `None`.
///
/// `is_user_type` needs the full `CodegenContext`; on the coverage /
/// test path (`codegen` is `None`) the variant refinement is skipped —
/// the parity gate isn't active there, so the conservative fn-reference
/// shape is fine (coverage only inspects `Some` vs `None`).
fn emit_mir_static_ref(name: &str, ctx: &MirEmitCtx<'_>) -> String {
    if name == "Option.None" || name == "None" {
        return "None".to_string();
    }
    // `BranchPath.Root` is the canonical-root nullary value (Oracle
    // structural addressing). It lowers to a `FnValue` rather than a
    // call, so it surfaces here — emit the `aver_rt` root constructor.
    if name == "BranchPath.Root" {
        return "aver_rt::BranchPath::root()".to_string();
    }
    if let Some((type_name, variant_name)) = name.rsplit_once('.')
        && let Some(cg) = ctx.codegen
    {
        let is_user = |n: &str| crate::codegen::common::is_user_type(n, cg);
        if is_user(type_name) {
            return if let Some((prefix, _)) = resolve_module_call(name, ctx.module_prefixes) {
                let module_path = module_prefix_to_rust_path(prefix);
                let bare_type = type_name
                    .rsplit_once('.')
                    .map(|(_, t)| t)
                    .unwrap_or(type_name);
                format!("{}::{}::{}", module_path, bare_type, variant_name)
            } else {
                format!("{}::{}", type_name, variant_name)
            };
        }
        if let Some((_, bare_type)) = type_name.rsplit_once('.')
            && is_user(bare_type)
        {
            return if let Some((prefix, _)) = resolve_module_call(name, ctx.module_prefixes) {
                let module_path = module_prefix_to_rust_path(prefix);
                format!("{}::{}::{}", module_path, bare_type, variant_name)
            } else {
                format!("{}::{}", bare_type, variant_name)
            };
        }
    }
    let static_ref = if let Some((prefix, bare)) = resolve_module_call(name, ctx.module_prefixes) {
        let module_path = module_prefix_to_rust_path(prefix);
        format!("{}::{}", module_path, aver_name_to_rust(bare))
    } else {
        aver_name_to_rust(name)
    };

    adapt_first_class_fn_ref(name, static_ref, ctx)
}

/// Adapt a user function's optimized Rust ABI to Aver's canonical `Fn` ABI.
///
/// A source-level `Fn(Shape) -> Int` is a value-taking function pointer:
/// `fn(Shape) -> AverInt`. Ordinary generated Rust functions deliberately use
/// borrow-by-default and therefore expose `fn(&Shape) -> AverInt` internally.
/// Passing that function item directly produces E0308 even though both sides
/// have the same Aver signature (issue #952).
///
/// A non-capturing closure is itself coercible to a plain fn pointer, so use it
/// as a zero-state ABI adapter whenever the target borrows at least one
/// parameter. The closure owns canonical arguments and lends only the positions
/// required by the target's internal signature. Scalar-only and self-TCO
/// functions keep the bare function item fast path.
fn adapt_first_class_fn_ref(name: &str, static_ref: String, ctx: &MirEmitCtx<'_>) -> String {
    let Some(cg) = ctx.codegen else {
        return static_ref;
    };
    let lookup = if name.contains('.') {
        name.to_string()
    } else if let Some(scope) = ctx.current_module_scope {
        format!("{scope}.{name}")
    } else {
        name.to_string()
    };
    let Some(fn_id) = crate::codegen::common::fn_id_for_dotted_name(cg, &lookup)
        .or_else(|| crate::codegen::common::fn_id_for_dotted_name(cg, name))
    else {
        return static_ref;
    };
    let Some(resolved) = cg.resolved_program.fn_by_id(fn_id) else {
        return static_ref;
    };

    // A lone self-TCO function is emitted with owned mutable params, so its
    // item already has the canonical value-taking ABI. Mutual-TCO members have
    // borrow-by-default public wrappers and therefore still need this adapter.
    let is_mutual_tco = cg.mutual_tco_members.contains(&fn_id);
    let is_lone_self_tco =
        !is_mutual_tco && super::toplevel::resolved_fn_has_self_tailcall(resolved);
    if is_lone_self_tco {
        return static_ref;
    }

    let borrow_mask: Vec<bool> = resolved
        .params
        .iter()
        .map(|(_, ty)| should_borrow_param(ty))
        .collect();
    if !borrow_mask.iter().any(|borrowed| *borrowed) {
        return static_ref;
    }

    let params: Vec<String> = (0..resolved.params.len())
        .map(|i| format!("__aver_fn_arg{i}"))
        .collect();
    let args: Vec<String> = params
        .iter()
        .zip(borrow_mask)
        .map(|(param, borrowed)| {
            if borrowed {
                format!("&{param}")
            } else {
                param.clone()
            }
        })
        .collect();
    format!(
        "|{}| {}({})",
        params.join(", "),
        static_ref,
        args.join(", ")
    )
}

/// Render one free-standing `verify`-case expression through the MIR
/// walker. `resolved` is the already-lifted `ResolvedExpr`. Lowers it via
/// `lower_top_level_value` against a clone of the entry `MirProgram` (the
/// same isolation the VM uses for top-level statements: builtin /
/// instantiation table growth stays local to the clone), then emits it
/// with a **program-level** [`MirEmitCtx`] (no params / locals — verify
/// exprs have no fn anchor).
///
/// Returns `None` when the expr is outside the lowerable subset OR the
/// walker can't render it — the per-expr signal for the caller to emit a
/// hard codegen diagnostic (the verify-only Oracle/trace residual that
/// never built on the Rust backend). The `#[test]` / `assert_eq!` /
/// Result-`?` scaffolding is unaffected; only the expression string
/// changes.
pub(super) fn emit_mir_verify_expr(
    resolved: &Spanned<crate::ir::hir::ResolvedExpr>,
    ctx: &CodegenContext,
) -> Option<String> {
    let base = ctx.mir_program.as_ref()?;
    // Clone so the lowerer's builtin / instantiation table growth stays
    // local to this expression (mirrors the VM top-level path #338).
    let mut prog = base.clone();
    let lowered = crate::ir::mir::lower_top_level_value(resolved, &mut prog).ok()?;
    let policy = MirFnEmitPolicy::empty();
    // Lend the grown clone's builtin table (it backs `Call(Builtin(id))`
    // resolution and may carry a builtin the lowering just interned) plus
    // the full `ctx` for the borrow / ctor helpers.
    let mut emit_ctx = MirEmitCtx::program_level(ctx, &policy, &prog.builtins);
    // The enclosing `#[test]` fn returns `Result<(), String>`; generated
    // fns error with generated types, so `?` must Debug-format the error
    // into a `String` at each propagation site (see `try_err_to_string`).
    emit_ctx.try_err_to_string = true;
    emit_mir_expr(&lowered, &emit_ctx)
}

/// Render the **`main` fn body** through the MIR walker. `main` is the
/// one entry-point that DOES carry a `ResolvedFnDef` (reachable via
/// `fn_id_for_decl` → `resolved_program.fn_by_id` → `mir_program.fn_by_id`),
/// so — unlike the free-standing verify / top-stmt exprs — its body has a
/// real fn anchor: we build the borrow policy from the resolved main
/// (`from_resolved`, borrow-by-default, the non-TCO shape) and emit via
/// the same `for_fn` ctx + `emit_mir_fn_body` every other fn uses.
///
/// `fn_id` is the resolved-main FnId the caller already computed
/// (`entry_module_sections` runs `fn_id_for_decl` for every fn). Returns
/// `None` when there's no MIR program, the main FnId has no lowered
/// `MirFn`, or the walker can't render the body — the signal for the
/// caller to emit a hard codegen diagnostic. The `fn main()` /
/// `-> Result<…>` signature and the guest/replay wrappers are unaffected;
/// only the body string moves onto MIR.
pub(super) fn emit_mir_main_body(fn_id: crate::ir::FnId, ctx: &CodegenContext) -> Option<String> {
    let mir_fn = ctx.mir_program.as_ref()?.fn_by_id(fn_id)?;
    let resolved = ctx.resolved_program.fn_by_id(fn_id)?;
    // Main lives in the entry module → no module scope. Borrow-by-default
    // matches the non-TCO shape the main body uses.
    let mut policy =
        MirFnEmitPolicy::from_resolved(resolved, None, /* borrow_by_default */ true);
    policy.apply_bare_i64(fn_id, ctx);
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);
    emit_mir_fn_body(&mir_fn.body, &emit_ctx)
}

/// Render a **guest-entry fn's inner body** through the MIR walker. The
/// guest-entry fn (the self-host's `runGuestCliProgram`) has its body
/// wrapped in the `aver_replay::with_guest_scope[_args][_result]` (replay
/// scope) and `crate::self_host_support::with_program_fn_store` (self-host
/// state) templates — pure string wrappers the caller keeps unchanged —
/// while the INNER body string is rendered here.
///
/// Unlike `main`, the caller already holds the `&ResolvedFnDef` (and its
/// `fn_id`), so this takes the resolved fn directly rather than looking it
/// up by `FnId`. The borrow policy is rebuilt with
/// `build_fn_ectx_from_resolved`'s rules (borrow-by-default, the non-TCO
/// shape; guest-entry returns before the `has_tco` branch). `scope` is the
/// owning module prefix (`None` for the entry-module guest-entry).
///
/// Returns `None` when there's no MIR program, the guest-entry FnId has
/// no lowered `MirFn`, or the walker can't render the body — the signal
/// for the caller to emit a hard codegen diagnostic. Only the body
/// string moves onto MIR; the replay / self-host-state wrappers stay
/// template text.
pub(super) fn emit_mir_guest_entry_body(
    resolved_fd: &crate::ir::hir::ResolvedFnDef,
    scope: Option<&str>,
    ctx: &CodegenContext,
) -> Option<String> {
    let mir_fn = ctx.mir_program.as_ref()?.fn_by_id(resolved_fd.fn_id)?;
    let mut policy =
        MirFnEmitPolicy::from_resolved(resolved_fd, scope, /* borrow_by_default */ true);
    policy.apply_bare_i64(resolved_fd.fn_id, ctx);
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);
    emit_mir_fn_body(&mir_fn.body, &emit_ctx)
}

/// Render every **top-level statement value** through the MIR walker,
/// all-or-nothing. Free-standing module-scope statements (`x = expr` / a
/// bare `expr`) belong to no `ResolvedFnDef`, so this mirrors the VM
/// top-level path (#338): clone the entry `MirProgram` ONCE (so the
/// lowerer's builtin / instantiation table growth stays consistent
/// across all the statements that share it), lower each statement's
/// already-resolved value via `lower_top_level_value`, and **pre-check**
/// that every value both lowers AND the walker renders it — deciding
/// before emitting anything so a mid-walk reject never leaves a
/// half-written main body (exactly what the VM `compile_top_level` does
/// with `mir_expr_compilable`).
///
/// Returns the rendered value strings in statement order on full success
/// (the caller wraps each in the explicit `let {name} @ _ = …;` /
/// bare-expr-discard
/// `…;` templating), or `None` if there's no MIR program or ANY
/// statement falls outside the lowerable / renderable subset — the
/// signal for the caller to emit a hard codegen diagnostic for the block
/// (the verify-only Oracle/trace residual).
pub(super) fn emit_mir_top_stmt_values(
    resolved_values: &[&Spanned<crate::ir::hir::ResolvedExpr>],
    ctx: &CodegenContext,
) -> Option<Vec<String>> {
    let base = ctx.mir_program.as_ref()?;
    // One clone shared across every statement: the lowerer grows its
    // builtin / instantiation tables in place, so all the `Call(Builtin)`
    // ids the walker resolves key off the same grown table (mirrors the
    // VM lowering one `prog` for the whole `__top_level__` chunk).
    let mut prog = base.clone();
    let lowered: Vec<Spanned<MirExpr>> = resolved_values
        .iter()
        .map(|value| crate::ir::mir::lower_top_level_value(value, &mut prog).ok())
        .collect::<Option<_>>()?;
    let policy = MirFnEmitPolicy::empty();
    let emit_ctx = MirEmitCtx::program_level(ctx, &policy, &prog.builtins);
    // All-or-nothing: render every value before returning any, so a
    // single un-renderable statement falls the WHOLE block back to HIR
    // rather than leaving a half-MIR / half-HIR main body.
    lowered
        .iter()
        .map(|low| emit_mir_expr(low, &emit_ctx))
        .collect::<Option<Vec<_>>>()
}

/// Emit `MirExpr::IndependentProduct` (`(a, b, c)!` / `(a, b, c)?!`)
/// byte-identical to HIR's `ResolvedExpr::IndependentProduct` arm
/// (`super::expr`). The Rust backend is the one target that truly
/// PARALLELIZES the product (the VM and wasm-gc lower it sequentially):
/// each element runs on its own `std::thread::scope` thread.
///
/// Mirror notes (the three behaviors this arm must preserve to stay
/// byte-equal under the parity gate):
///
/// 1. **`?!` (`unwrap_results == true`).** A shared `__cancel_flag`
///    (`Arc<AtomicBool>`) is threaded into every branch via
///    `run_cancelable_branch`; a branch that produces `Err` sets the
///    flag so siblings can short-circuit (the *cancel* independence
///    mode — `complete` ignores the flag, but the emitted shape is the
///    same; the runtime decides). Joined branches are folded by
///    `emit_parallel_result_tuple_unwrap` (which unwraps the
///    `ParallelBranch::Completed` wrapper, then propagates the first
///    `Err` with `?`).
/// 2. **`!` (`unwrap_results == false`).** Same `thread::scope`/`spawn`,
///    but no cancel flag and no unwrap — joined branch values fold
///    straight into a tuple via `emit_tuple_from_vars` (a bare product
///    of `Result`s, preserved positionally).
/// 3. **Replay sequential fallback.** When `emit_replay_runtime` is on,
///    the parallel body is wrapped in
///    `if is_effect_tracking_active() { <sequential replay groups> }
///    else { <parallel> }`. The sequential arm uses
///    `enter_effect_group` / `set_effect_branch(i)` / `exit_effect_group`
///    so per-branch effects record/replay deterministically on one
///    thread; the parallel arm additionally captures + re-installs the
///    parallel scope context per spawned branch.
///
/// Each element is rendered through `mir_clone_arg` (the byte-identical
/// mirror of HIR's `clone_arg`). The `run_cancelable_branch` /
/// `ParallelBranch` / parallel-scope runtime is emitted UNCONDITIONALLY
/// by `super::runtime`, so no new runtime is needed.
fn emit_mir_independent_product(
    ip: &crate::ir::mir::MirIndependentProduct,
    emit_ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut parts: Vec<String> = Vec::with_capacity(ip.items.len());
    for it in &ip.items {
        parts.push(mir_clone_arg(
            emit_mir_expr(it, emit_ctx)?,
            &it.node,
            emit_ctx,
        ));
    }

    let n = parts.len();
    // The replay flag lives on the full `CodegenContext`; the coverage /
    // test path has none → treat as no replay (mirror of HIR's
    // `ctx.emit_replay_runtime`, conservative on the coverage walk).
    let has_replay = emit_ctx.codegen.is_some_and(|c| c.emit_replay_runtime);
    let unwrap = ip.unwrap_results;

    let mut code = String::new();
    if has_replay {
        // Runtime branch: if recording/replaying, execute sequentially
        // with replay groups (thread_local state stays on one thread).
        code.push_str("if crate::aver_replay::is_effect_tracking_active() { ");
        code.push_str("crate::aver_replay::enter_effect_group(); ");
        for (i, part) in parts.iter().enumerate() {
            code.push_str(&format!(
                "crate::aver_replay::set_effect_branch({i}); let _r{i} = {part}; "
            ));
        }
        code.push_str("crate::aver_replay::exit_effect_group(); ");
        if unwrap {
            code.push_str(&emit_result_tuple_unwrap("_r", "__v", n));
            code.push('?');
        } else {
            code.push_str(&emit_tuple_from_vars("_r", n));
        }
        code.push_str(" } else { ");
    }

    if unwrap {
        code.push_str("{ ");
        if has_replay {
            code.push_str(
                "let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); ",
            );
        }
        code.push_str(
            "let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); ",
        );
        code.push_str("std::thread::scope(|_s| { ");
        for (i, (item, part)) in ip.items.iter().zip(parts.iter()).enumerate() {
            if has_replay {
                code.push_str(&format!(
                    "let __parallel_scope{i} = __parallel_scope.clone(); "
                ));
            }
            code.push_str(&format!("let __cancel_flag{i} = __cancel_flag.clone(); "));
            let has_captures = emit_parallel_capture_bindings(&mut code, i, &item.node, emit_ctx);
            if has_replay {
                code.push_str(&format!(
                    "crate::aver_replay::with_parallel_scope_context(__parallel_scope{i}.clone(), move || "
                ));
            }
            code.push_str("{ crate::run_cancelable_branch(__cancel_flag");
            code.push_str(&i.to_string());
            code.push_str(".clone(), move || { let __result = ");
            code.push_str(part);
            code.push_str("; if let Err(_) = &__result { __cancel_flag");
            code.push_str(&i.to_string());
            code.push_str(".store(true, std::sync::atomic::Ordering::Relaxed); } __result }) }");
            if has_replay {
                code.push(')');
            }
            code.push_str(if has_captures { ") }; " } else { "); " });
        }
        for i in 0..n {
            code.push_str(&format!("let _b{i} = _h{i}.join().unwrap(); "));
        }
        code.push_str(&emit_parallel_result_tuple_unwrap("_b", "_r", "__v", n));
        code.push_str(" })? }");
    } else {
        if has_replay {
            code.push_str(
                "let __parallel_scope = crate::aver_replay::capture_parallel_scope_context(); ",
            );
        }
        code.push_str("std::thread::scope(|_s| { ");
        for (i, (item, part)) in ip.items.iter().zip(parts.iter()).enumerate() {
            if has_replay {
                code.push_str(&format!(
                    "let __parallel_scope{i} = __parallel_scope.clone(); "
                ));
                let has_captures =
                    emit_parallel_capture_bindings(&mut code, i, &item.node, emit_ctx);
                code.push_str(&format!(
                    "crate::aver_replay::with_parallel_scope_context(__parallel_scope{i}.clone(), move || {part}))"
                ));
                code.push_str(if has_captures { " }; " } else { "; " });
            } else {
                let has_captures =
                    emit_parallel_capture_bindings(&mut code, i, &item.node, emit_ctx);
                code.push_str(part);
                code.push_str(if has_captures { ") }; " } else { "); " });
            }
        }
        for i in 0..n {
            code.push_str(&format!("let _r{i} = _h{i}.join().unwrap(); "));
        }
        code.push_str(&emit_tuple_from_vars("_r", n));
        code.push_str(" }) ");
    }

    if has_replay {
        code.push('}');
    }
    Some(code)
}

/// Start one scoped-thread binding and, when the branch reads source locals,
/// shadow every free local with a branch-owned capture before `spawn(move …)`.
///
/// A `.clone()` written *inside* a move closure does not protect the outer
/// value: constructing that closure has already moved the value. This was the
/// #1019 failure mode — branch 0 called `height.clone()` but captured `height`
/// itself, leaving branch 1 with an E0382 use-after-move. The lexical block
/// keeps each shadow local to one spawn expression, so every closure owns an
/// independent value while the function's original remains available for all
/// sibling captures.
///
/// Returns whether a capture block was opened; the caller closes either
/// `_s.spawn(…) };` or the capture-free `_s.spawn(…);` shape. Keeping the old
/// shape for capture-free branches avoids churn in generated Rust.
fn emit_parallel_capture_bindings(
    code: &mut String,
    branch: usize,
    item: &MirExpr,
    ctx: &MirEmitCtx<'_>,
) -> bool {
    let captures = mir_free_locals(item);
    code.push_str(&format!("let _h{branch} = "));
    if captures.is_empty() {
        code.push_str("_s.spawn(move || ");
        return false;
    }

    code.push_str("{ ");
    for local in captures.values() {
        let source_name = local.name.as_str();
        let rust_name = aver_name_to_rust(source_name);
        let capture = if ctx.is_borrowed_param(source_name)
            || ctx.is_copy(source_name)
            || ctx.bare.is_bare(local.slot)
        {
            rust_name.clone()
        } else {
            format!("{rust_name}.clone()")
        };
        code.push_str(&format!("let {rust_name} = {capture}; "));
    }
    code.push_str("_s.spawn(move || ");
    true
}

/// Free source locals read by one independent-product branch, keyed by MIR
/// identity for deterministic emission. `Let` and match-pattern slots are
/// branch-internal bindings, not captures; MIR slots are unique per function,
/// so collecting all branch-local binders first safely excludes them from the
/// subsequent read walk even when source names shadow one another.
fn mir_free_locals(item: &MirExpr) -> BTreeMap<LocalId, MirLocal> {
    fn pattern_bindings(pattern: &MirPattern, bound: &mut HashSet<LocalId>) {
        match pattern {
            MirPattern::Bind(slot, _) => {
                bound.insert(*slot);
            }
            MirPattern::Cons { head, tail, .. } => {
                bound.insert(*head);
                bound.insert(*tail);
            }
            MirPattern::Tuple(items) => {
                for item in items {
                    pattern_bindings(item, bound);
                }
            }
            MirPattern::Ctor { bindings, .. } => {
                bound.extend(bindings.iter().copied());
            }
            MirPattern::Wildcard | MirPattern::Literal(_) | MirPattern::EmptyList => {}
        }
    }

    fn collect_bound(expr: &MirExpr, bound: &mut HashSet<LocalId>) {
        match expr {
            MirExpr::Let(let_node) => {
                bound.insert(let_node.node.binding);
            }
            MirExpr::Match(match_node) => {
                for arm in &match_node.node.arms {
                    pattern_bindings(&arm.pattern, bound);
                }
            }
            _ => {}
        }
        crate::ir::mir::optimize::bare_i64::visit_children(expr, &mut |child| {
            collect_bound(child, bound);
        });
    }

    fn collect_reads(
        expr: &MirExpr,
        bound: &HashSet<LocalId>,
        captures: &mut BTreeMap<LocalId, MirLocal>,
    ) {
        if let MirExpr::Local(local) = expr
            && !local.node.name.is_empty()
            && !bound.contains(&local.node.slot)
        {
            captures
                .entry(local.node.slot)
                .or_insert_with(|| local.node.clone());
        }
        if let MirExpr::Call(call) = expr
            && let MirCallee::LocalSlot {
                slot,
                name,
                last_use,
            } = &call.node.callee
        {
            let slot = LocalId(u32::from(*slot));
            if !name.is_empty() && !bound.contains(&slot) {
                captures.entry(slot).or_insert_with(|| MirLocal {
                    slot,
                    last_use: *last_use,
                    name: name.clone(),
                });
            }
        }
        crate::ir::mir::optimize::bare_i64::visit_children(expr, &mut |child| {
            collect_reads(child, bound, captures);
        });
    }

    let mut bound = HashSet::new();
    collect_bound(item, &mut bound);
    let mut captures = BTreeMap::new();
    collect_reads(item, &bound, &mut captures);
    captures
}

/// Emit `MirExpr::IfThenElse` byte-identical to HIR's
/// `try_emit_bool_if_else` (the only producer of `IfThenElse` is the
/// MIR `bool_match_to_if` pass, which rewrites the exact two-arm bool
/// matches HIR routes through `try_emit_bool_if_else`).
///
/// Two HIR behaviors are mirrored here that the naive `if cond { then }
/// else { else }` emit misses:
///
/// 1. **Condition canonicalization.** HIR's
///    `classify_bool_subject_plan_resolved` never emits `>=` / `<=` /
///    `!=` in the condition: it rewrites `>=`→`<`, `<=`→`>`, `!=`→`==`
///    and *swaps* the then/else branches (`invert`). The MIR pass keeps
///    the source operator + branch order, so a `code >= 48` subject
///    renders as `if (code >= 48) { then } else { else }` where HIR
///    renders `if (code < 48) { else } else { then }`. Re-apply HIR's
///    rewrite so the two match.
/// 2. **Branch clone.** HIR runs each branch through `maybe_clone`
///    (owning position). Mirror with `mir_maybe_clone` (a no-op for the
///    already-graduated cases, exact for the rest).
fn emit_mir_if_then_else(
    ite: &crate::ir::mir::MirIfThenElse,
    emit_ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // HIR's `classify_bool_subject_plan_resolved` maps a comparison
    // subject to a canonical operator + an `invert` flag:
    //   ==  →  "==", keep ;  !=  →  "==", invert
    //   <   →  "<",  keep ;  >=  →  "<",  invert
    //   >   →  ">",  keep ;  <=  →  ">",  invert
    // `invert == true` swaps the then/else branches. Crucially, HIR's
    // `try_emit_bool_if_else` renders the condition operands with a
    // *plain* `emit_expr` — it does NOT apply the `BinOp` arm's
    // string-literal `&*x == "lit"` deref. So a `match name == "_"`
    // subject emits `name == AverStr::from("_")` in the condition, not
    // `&*name == "_"`. Mirror that by emitting the comparison cond
    // directly here from the raw operand renders, bypassing the
    // deref-applying `BinOp` arm.
    let (cond, then_src, else_src) = mir_if_cond_and_branches(ite, emit_ctx)?;

    let then_branch = mir_maybe_clone(emit_mir_expr(then_src, emit_ctx)?, &then_src.node, emit_ctx);
    let else_branch = mir_maybe_clone(emit_mir_expr(else_src, emit_ctx)?, &else_src.node, emit_ctx);
    Some(format!(
        "if {} {{ {} }} else {{ {} }}",
        cond, then_branch, else_branch
    ))
}

// ── Match (Wave 2) ──────────────────────────────────────────────────────
//
// `MirExpr::Match` → Rust source byte-identical to HIR's `emit_match`
// (`src/codegen/rust/expr.rs`). The strategy is to reuse the *shared*
// recognition + emit machinery the HIR walker already routes through:
//
//   1. Translate each `MirPattern` → `ResolvedPattern` (resolving ctor
//      identity through the symbol table, exactly as the resolver
//      stamped it). Build synthetic `ResolvedMatchArm`s carrying those
//      patterns + neutral bodies.
//   2. Pre-render every arm body via the MIR walker (`emit_mir_expr` +
//      `mir_maybe_clone`). If any arm body can't render, the whole
//      match falls back to HIR. The dispatch/list emitters take a
//      `body_for_arm` closure; we map each synthetic arm back to its
//      pre-rendered MIR body by pointer offset into the synthetic slice.
//   3. Drive the SAME selection ladder `emit_match` uses (single-arm
//      irrefutable → `let`; borrowed-param `match_on_ref`; list match;
//      dispatch table; generic `match`) using the SAME shared
//      classifier (`classify_match_dispatch_plan_resolved`) and the
//      SAME `emit_dispatch_table_match` / `emit_list_match` /
//      `emit_pattern` / `emit_pattern_rebindings` functions.
//
// Bool two-arm matches never reach here — the MIR optimizer's
// `bool_match_to_if` already rewrote them to `MirExpr::IfThenElse`
// (handled by the dedicated arm in `emit_mir_expr`). So this arm only
// ever sees list / dispatch-table / generic shapes, exactly the
// non-bool subset HIR's `emit_match` reaches after its own bool short
// circuit. Any shape the walker can't reproduce byte-identically
// returns `None` and the parity gate falls back safely.

/// Mirror of HIR's `is_irrefutable_pattern` over `ResolvedPattern`.
fn resolved_pattern_is_irrefutable(pat: &ResolvedPattern) -> bool {
    match pat {
        ResolvedPattern::Wildcard | ResolvedPattern::Ident(_) => true,
        ResolvedPattern::Tuple(pats) => pats.iter().all(resolved_pattern_is_irrefutable),
        _ => false,
    }
}

/// Translate a `MirPattern` → `ResolvedPattern`, resolving ctor
/// identity through the symbol table the same way the resolver pass
/// stamped it (so `emit_pattern` / `emit_pattern_rebindings` /
/// `classify_*` see the exact `ResolvedPattern` shape the HIR walker
/// would have). Returns `None` for any pattern shape the walker can't
/// translate yet (none currently — every `MirPattern` maps).
fn mir_pattern_to_resolved(pat: &MirPattern, ctx: &MirEmitCtx<'_>) -> Option<ResolvedPattern> {
    Some(match pat {
        MirPattern::Wildcard => ResolvedPattern::Wildcard,
        MirPattern::Literal(lit) => ResolvedPattern::Literal(lit.clone()),
        // A `Bind` is HIR's `Ident` binding (`x -> …`). The source
        // binder name is what HIR emits.
        MirPattern::Bind(_, name) => ResolvedPattern::Ident(name.clone()),
        MirPattern::EmptyList => ResolvedPattern::EmptyList,
        MirPattern::Cons {
            head_name,
            tail_name,
            ..
        } => ResolvedPattern::Cons(head_name.clone(), tail_name.clone()),
        MirPattern::Tuple(sub) => {
            let mut parts = Vec::with_capacity(sub.len());
            for p in sub {
                parts.push(mir_pattern_to_resolved(p, ctx)?);
            }
            ResolvedPattern::Tuple(parts)
        }
        MirPattern::Ctor {
            ctor,
            binding_names,
            ..
        } => {
            let resolved_ctor = match ctor {
                MirCtor::Builtin(b) => ResolvedCtor::Builtin(*b),
                MirCtor::User(ctor_id) => {
                    // Resolve `CtorId` → owning type + variant name,
                    // exactly as the resolver stamped a user
                    // `ResolvedCtor::User`. `semantic_constructor_from_resolved_ctor`
                    // (used downstream by `emit_pattern` /
                    // `emit_pattern_rebindings`) reads `type_id` + `name`.
                    let entry = ctx.symbol_table.ctor_entry(*ctor_id);
                    ResolvedCtor::User {
                        ctor_id: *ctor_id,
                        type_id: entry.owning_type,
                        name: entry.name.clone(),
                    }
                }
            };
            ResolvedPattern::Ctor(resolved_ctor, binding_names.clone())
        }
    })
}

/// Build a neutral-bodied [`ResolvedMatchArm`] carrying just `pattern`.
/// The dispatch/list emitters only read `arm.pattern` + call the
/// `body_for_arm` closure; they never touch `arm.body`, so a `Unit`
/// literal placeholder is safe and the real MIR-rendered body is
/// supplied through the closure.
fn synthetic_arm(pattern: ResolvedPattern) -> ResolvedMatchArm {
    ResolvedMatchArm {
        pattern,
        body: Box::new(Spanned {
            node: crate::ir::hir::ResolvedExpr::Literal(crate::ast::Literal::Unit),
            line: 0,
            ty: std::sync::OnceLock::new(),
        }),
        binding_slots: std::sync::OnceLock::new(),
    }
}

/// Emit Rust for a `MirExpr::Match`, byte-identical to HIR's
/// `emit_match`. Returns `None` (→ HIR fallback) when the subject or
/// any arm body can't render, when a pattern can't translate, or when
/// the match shape isn't one the walker reproduces yet.
fn emit_mir_match(m: &MirMatch, emit_ctx: &MirEmitCtx<'_>) -> Option<String> {
    // Default (non-TCO) arm-body renderer: emit the arm body through
    // the MIR walker, then `maybe_clone` for the owning position —
    // exactly HIR's per-arm
    // `maybe_clone(emit_expr(&arm.body.node, …), &arm.body.node, …)`.
    emit_mir_match_with(m, emit_ctx, &|arm_body, ctx| {
        let body = emit_mir_expr(arm_body, ctx)?;
        Some(mir_maybe_clone(body, &arm_body.node, ctx))
    })
}

/// Core of [`emit_mir_match`], parameterized over how each arm body is
/// rendered. `render_arm` turns one arm's `Spanned<MirExpr>` body into
/// Rust source (or `None` → fall back). The default path renders bodies
/// as values (`maybe_clone`); the Wave-5 self-TCO loop path renders them
/// in tail position (self-`TailCall` → rebind + `continue`, value arm →
/// `return <expr>;`), so the same dispatch/list/generic machinery is
/// reused for TCO matches instead of forking the recognition.
fn emit_mir_match_with(
    m: &MirMatch,
    emit_ctx: &MirEmitCtx<'_>,
    render_arm: &dyn Fn(&Spanned<MirExpr>, &MirEmitCtx<'_>) -> Option<String>,
) -> Option<String> {
    // Translate patterns up front — bail if any pattern can't map.
    let mut arms: Vec<ResolvedMatchArm> = Vec::with_capacity(m.arms.len());
    for arm in &m.arms {
        arms.push(synthetic_arm(mir_pattern_to_resolved(
            &arm.pattern,
            emit_ctx,
        )?));
    }

    // Pre-render every arm body, in arm order. `body_for_arm` (below)
    // maps a `&ResolvedMatchArm` back to its index by pointer offset
    // into `arms`, then reads the matching pre-rendered string.
    let mut arm_bodies: Vec<String> = Vec::with_capacity(m.arms.len());
    for arm in &m.arms {
        arm_bodies.push(render_arm(&arm.body, emit_ctx)?);
    }

    let body_for_arm = |arm: &ResolvedMatchArm| -> String {
        // The dispatch/list emitters always hand back a reference to an
        // element of `arms` (they index `&arms[i]`), so identity match
        // by address recovers the arm's position → its pre-rendered MIR
        // body. Falls back to an empty body only if an emitter ever
        // passed a foreign reference (it doesn't), which the parity
        // gate would then reject as a mismatch.
        arms.iter()
            .position(|candidate| std::ptr::eq(candidate, arm))
            .map(|idx| arm_bodies[idx].clone())
            .unwrap_or_default()
    };

    // ── 1. Single-arm irrefutable → `let` destructuring. ──
    // Mirror of `emit_match`'s first branch.
    if arms.len() == 1 && resolved_pattern_is_irrefutable(&arms[0].pattern) {
        let subj_code = emit_mir_expr(&m.subject, emit_ctx)?;
        // Int unboxing boundary (defect esc_match): a `match (n - 1) { x -> … }`
        // binds the subject to `x`. When the analysis marked the bound slot
        // BOXED (because `x` later escapes — `[x, x]`) but the subject renders
        // bare `i64`, box it at the binding crossing with `from_i64`, so the
        // bound `x` is the `AverInt` its later uses expect. A bare bound slot
        // keeps the raw subject. (Only the `Bind` arm carries a slot; a
        // wildcard / destructuring pattern is unaffected.)
        // ETAP-2 SLICE 1: the bind-crossing boundary is now an EXPLICIT
        // `Box`/`Unbox` node the rewrite wrapped the subject in (it rewrites
        // the subject in the binding's representation context). `subj_code`
        // already carries the right representation — no codegen-side coercion.
        let _ = &m.arms[0].pattern;
        let subj = mir_clone_arg(subj_code, &m.subject.node, emit_ctx);
        let codegen = emit_ctx.codegen?;
        let pat = emit_pattern(&arms[0].pattern, false, codegen);
        let body = arm_bodies[0].clone();
        return Some(match &arms[0].pattern {
            ResolvedPattern::Wildcard => body,
            ResolvedPattern::Ident(name) => {
                let name = aver_name_to_rust(name);
                format!("{{ let {} = {}; {} }}", name, subj, body)
            }
            _ => format!("{{ let {} = {}; {} }}", pat, subj, body),
        });
    }

    // The shared dispatch/list/pattern emitters all need a real
    // `CodegenContext` (boxed-field lookup, module-prefix mangling).
    // The coverage walk runs without one — there the match only needs
    // to report "would emit", so we still translate + recurse but bail
    // before the ctx-dependent emit. (Production parity always has a
    // ctx; coverage only reads Some/None and matches will fall into the
    // None bucket on the coverage path, which is conservative + fine.)
    let codegen = emit_ctx.codegen?;

    // An `Int`-literal pattern (top-level or nested in a tuple) cannot be a
    // Rust `match` pattern — `AverInt` is not a literal. Such matches lower
    // to an if/else-if equality-guard chain (`try_emit_int_literal_match`),
    // mirroring the dispatch-table guard path. They must NOT take the
    // borrow-by-reference path below: a guard `&AverInt == AverInt` does not
    // typecheck. So the subject is always cloned by VALUE for these.
    let any_int_literal_pattern = arms.iter().any(|arm| pattern_has_int_literal(&arm.pattern));

    // ── 2. Borrowed-param subject → match on the reference. ──
    // Mirror of `emit_match`'s `match_on_ref` special case: only when
    // no arm has pattern bindings AND no arm has an Int-literal subpattern.
    let no_bindings = arms
        .iter()
        .all(|arm| crate::ir::vars::resolved_pattern_bindings(&arm.pattern).is_empty());
    let match_on_ref = no_bindings
        && !any_int_literal_pattern
        && mir_expr_is_borrowed_param_read(&m.subject.node, emit_ctx);

    // Int unboxing: a bare-`i64` subject (a proven-bare counter) is `Copy`,
    // so it is read by value WITHOUT `.clone()` and the Int-literal guards
    // compare against raw `{N}i64` rather than `AverInt::from_i64(N)`.
    let subject_is_bare = any_int_literal_pattern && mir_expr_is_bare_i64(&m.subject, emit_ctx);

    let subj = if subject_is_bare {
        // A bare i64 subject: emit the plain value (no clone — Copy).
        emit_bare_i64(&m.subject).or_else(|| emit_mir_expr(&m.subject, emit_ctx))?
    } else if match_on_ref {
        emit_mir_expr(&m.subject, emit_ctx)?
    } else {
        mir_clone_arg(
            emit_mir_expr(&m.subject, emit_ctx)?,
            &m.subject.node,
            emit_ctx,
        )
    };

    let dispatch_plan = classify_match_dispatch_plan_resolved(&arms);

    // Bool match → if/else is unreachable here: the MIR optimizer
    // already rewrote two-arm bool matches into `IfThenElse`. If a
    // `Bool` plan somehow survived (hand-built MIR in a test), fall
    // back rather than re-implement `try_emit_bool_if_else` (which
    // needs the subject's `ResolvedExpr` form for the compare-invert
    // rewrite the MIR walker can't reproduce).
    if matches!(dispatch_plan.as_ref(), Some(MatchDispatchPlan::Bool(_))) {
        return None;
    }

    // ── 3. List match. ──
    if has_list_patterns(&arms) {
        let list_shape = match dispatch_plan.as_ref() {
            Some(MatchDispatchPlan::List(shape)) => Some(*shape),
            _ => None,
        };
        return Some(emit_list_match(
            subj,
            &arms,
            list_shape,
            true,
            codegen,
            body_for_arm,
        ));
    }

    // ── 4. Dispatch table (literals / wrapper tags). ──
    if let Some(MatchDispatchPlan::Table(shape)) = dispatch_plan.as_ref() {
        return Some(emit_dispatch_table_match(
            subj,
            &arms,
            shape,
            subject_is_bare,
            body_for_arm,
        ));
    }

    // ── 4b. Int-literal match → if/else-if equality-guard chain. ──
    // Any match that reaches here with an Int-literal subpattern (a single
    // top-level Int literal that didn't form a ≥2-entry dispatch table, or a
    // tuple carrying Int literals) can't be a Rust `match` — `AverInt` is not
    // a pattern literal. Lower it to guards instead. When such a pattern is
    // present the generic `match` path below is NOT a valid fallback (it
    // would emit `AverInt::from_i64(N)` as a pattern), so the guard emitter
    // is REQUIRED to render; if it can't, return `None` (hard diagnostic).
    if any_int_literal_pattern {
        return try_emit_int_literal_match(&subj, &arms, &arm_bodies, subject_is_bare, codegen);
    }

    // ── 5. Generic `match`. ──
    // Mirror of `emit_match`'s tail. `needs_as_str` is always `true`
    // in HIR (`subject_might_be_string` is a `true` stub), so the
    // string-literal-pattern case derefs the subject to `&str`.
    let needs_as_str = true;
    let match_expr = if needs_as_str && has_string_literal_patterns(&arms) {
        format!("&*{}", subj)
    } else {
        subj
    };

    let mut arm_strs = Vec::with_capacity(arms.len());
    for (idx, arm) in arms.iter().enumerate() {
        let pat = emit_pattern(&arm.pattern, needs_as_str, codegen);
        let body = arm_bodies[idx].clone();
        let mut rebindings = emit_pattern_rebindings(&arm.pattern, codegen);
        if match_on_ref {
            let ref_rebinds = emit_ref_match_rebindings(&arm.pattern);
            if !ref_rebinds.is_empty() {
                rebindings = format!("{}{}", ref_rebinds, rebindings);
            }
        }
        arm_strs.push(format!(
            "        {} => {{\n            {}{}\n        }}",
            pat, rebindings, body
        ));
    }

    Some(format!(
        "match {} {{\n{}\n    }}",
        match_expr,
        arm_strs.join(",\n")
    ))
}

/// Does this pattern contain an `Int`-literal subpattern (top-level or
/// nested inside a tuple)? Such a pattern cannot become a Rust `match`
/// arm — `AverInt` is not a literal — so it forces the equality-guard
/// lowering in [`try_emit_int_literal_match`] and excludes the by-reference
/// `match_on_ref` path (where the guard would compare `&AverInt`).
fn pattern_has_int_literal(pat: &ResolvedPattern) -> bool {
    match pat {
        // A big-int literal pattern is also routed through the equality-guard
        // chain (an `AverInt` cannot be a Rust `match` literal) — it compares via
        // `AverInt: PartialEq`, just like the i64 case but parsed from digits.
        ResolvedPattern::Literal(crate::ast::Literal::Int(_) | crate::ast::Literal::BigInt(_)) => {
            true
        }
        ResolvedPattern::Tuple(pats) => pats.iter().any(pattern_has_int_literal),
        _ => false,
    }
}

/// Lower a match that carries `Int`-literal patterns into an if/else-if
/// equality-guard chain (`AverInt: PartialEq`), since `AverInt` cannot be a
/// Rust `match` pattern literal. Mirrors the dispatch-table guard path, but
/// also handles a single Int-literal arm (too few for a dispatch table) and
/// tuple subjects with Int-literal subpatterns.
///
/// `subj` is the already-emitted, by-VALUE subject expression. The supported
/// arm shapes (after list / table / bool matches are peeled off upstream):
/// top-level `Literal(Int)` / `Wildcard` / `Ident`, and `Tuple(..)` whose
/// elements are `Literal(Int)` / `Wildcard` / `Ident` (or nested tuples of
/// the same). Returns `None` for any other shape (e.g. a non-Int literal or
/// a ctor mixed in) — the caller then emits a hard diagnostic.
fn try_emit_int_literal_match(
    subj: &str,
    arms: &[ResolvedMatchArm],
    arm_bodies: &[String],
    subject_is_bare: bool,
    codegen: &CodegenContext,
) -> Option<String> {
    // Bind the subject once so a non-trivial expr is evaluated a single time
    // and the guards / bindings reference the temp.
    let subject_name = "__int_match_subject";

    // Collect every binding name used anywhere in the match, so the fresh
    // tuple-element temporaries (`__litN`) can be chosen to never collide
    // with a real pattern binding (hygiene — fix #4).
    let mut used_names: HashSet<String> = HashSet::new();
    for arm in arms {
        for b in crate::ir::vars::resolved_pattern_bindings(&arm.pattern) {
            used_names.insert(aver_name_to_rust(&b));
        }
    }
    used_names.insert(subject_name.to_string());

    // Pre-render the per-element tuple temp names, fresh + non-colliding.
    // The same N temps serve every tuple arm (the subject is one tuple), so
    // pick them once up front.
    let tuple_arity = arms.iter().find_map(|arm| match &arm.pattern {
        ResolvedPattern::Tuple(pats) => Some(pats.len()),
        _ => None,
    });
    let tuple_temps: Vec<String> = match tuple_arity {
        Some(n) => {
            let mut temps = Vec::with_capacity(n);
            let mut counter = 0usize;
            for _ in 0..n {
                let name = loop {
                    let candidate = format!("__lit{}", counter);
                    counter += 1;
                    if !used_names.contains(&candidate) {
                        break candidate;
                    }
                };
                used_names.insert(name.clone());
                temps.push(name);
            }
            temps
        }
        None => Vec::new(),
    };

    // For each arm, build (Option<condition>, bindings-prelude). A `None`
    // condition is the catch-all (`else`). The last such arm closes the
    // chain; any arm after it is dead but harmless.
    enum ArmPlan {
        Guard { cond: String, prelude: String },
        Default { prelude: String },
    }

    let mut plans: Vec<(ArmPlan, &str)> = Vec::with_capacity(arms.len());
    for (idx, arm) in arms.iter().enumerate() {
        let body = arm_bodies[idx].as_str();
        match &arm.pattern {
            ResolvedPattern::Wildcard => {
                plans.push((
                    ArmPlan::Default {
                        prelude: String::new(),
                    },
                    body,
                ));
            }
            ResolvedPattern::Ident(name) => {
                // Catch-all binding: bind the whole subject by value. A bare
                // `i64` subject is `Copy` (no `.clone()` needed and binding
                // it keeps the bare repr the body's arithmetic reads).
                let rust = aver_name_to_rust(name);
                let prelude = if subject_is_bare {
                    format!("let {} = {}; ", rust, subject_name)
                } else {
                    format!("let {} = {}.clone(); ", rust, subject_name)
                };
                plans.push((ArmPlan::Default { prelude }, body));
            }
            ResolvedPattern::Literal(crate::ast::Literal::Int(n)) => {
                // Int unboxing: a bare subject compares against a raw `i64`
                // literal; a boxed subject compares against `AverInt`.
                let cond = if subject_is_bare {
                    format!("{} == {}i64", subject_name, n)
                } else {
                    format!("{} == aver_rt::AverInt::from_i64({})", subject_name, n)
                };
                plans.push((
                    ArmPlan::Guard {
                        cond,
                        prelude: String::new(),
                    },
                    body,
                ));
            }
            ResolvedPattern::Literal(crate::ast::Literal::BigInt(s)) => {
                // A big-int literal pattern: compare the subject against the
                // exact `AverInt` parsed from the digits. A bare i64 subject can
                // never equal a `>i64` value, but boxing it keeps the comparison
                // well-typed (and correctly false at runtime).
                let lhs = if subject_is_bare {
                    format!("aver_rt::AverInt::from_i64({})", subject_name)
                } else {
                    subject_name.to_string()
                };
                let cond = format!("{} == {:?}.parse::<aver_rt::AverInt>().unwrap()", lhs, s);
                plans.push((
                    ArmPlan::Guard {
                        cond,
                        prelude: String::new(),
                    },
                    body,
                ));
            }
            ResolvedPattern::Tuple(pats) => {
                // Tuple subjects are never bare in this slice (only scalar
                // counters go bare); the bare guard machinery does not
                // model tuple-element reps. Bail rather than mis-emit.
                if subject_is_bare {
                    return None;
                }
                if pats.len() != tuple_temps.len() {
                    return None;
                }
                let mut conds: Vec<String> = Vec::new();
                let mut prelude = String::new();
                for (pat, temp) in pats.iter().zip(tuple_temps.iter()) {
                    // Each top-level temp `__litN` is a reference to the
                    // tuple element (`&Elem`), so its value place is `*temp`.
                    // Recurse into the element, destructuring nested tuples
                    // to arbitrary depth via field-index access expressions.
                    let place = format!("(*{})", temp);
                    if !lower_int_literal_subpatterns(pat, &place, &mut conds, &mut prelude) {
                        // A non-Int literal, ctor, or other shape nested
                        // anywhere in the tuple is out of scope here.
                        return None;
                    }
                }
                if conds.is_empty() {
                    // No literal constraints: this tuple arm is a catch-all
                    // (pure bindings / wildcards) — it closes the chain.
                    plans.push((ArmPlan::Default { prelude }, body));
                } else {
                    let cond = conds.join(" && ");
                    plans.push((ArmPlan::Guard { cond, prelude }, body));
                }
            }
            // Any other top-level shape is out of scope for this lowering.
            _ => return None,
        }
    }

    // Build the if/else-if chain from the back. The chain MUST end in a
    // default arm (the Aver typechecker rejects a non-exhaustive Int match,
    // so a `_`/binding arm is always present); if none was found, emit an
    // `unreachable!` tail so the generated `match` is still total.
    let mut chain =
        String::from("unreachable!(\"Aver Rust codegen: non-exhaustive Int-literal match\")");
    for (plan, body) in plans.into_iter().rev() {
        match plan {
            ArmPlan::Default { prelude } => {
                chain = format!("{{ {}{} }}", prelude, body);
            }
            ArmPlan::Guard { cond, prelude } => {
                chain = format!("if {} {{ {}{} }} else {}", cond, prelude, body, chain);
            }
        }
    }

    // Tuple subjects need the element temps bound (by reference, so the
    // guards compare `&AverInt == &AverInt`). A non-tuple subject is used
    // directly.
    let setup = if tuple_temps.is_empty() {
        format!("let {} = {};", subject_name, subj)
    } else {
        // Destructure `&(AverInt, …)`: match ergonomics bind each element as
        // `&AverInt`, so the guards compare `&AverInt == &AverInt` and a
        // binding element clones the `&AverInt` to an owned value. A
        // single-element tuple needs the trailing comma (`(x,)`).
        let elems = if tuple_temps.len() == 1 {
            format!("{},", tuple_temps[0])
        } else {
            tuple_temps.join(", ")
        };
        format!(
            "let {} = {}; let ({}) = &{};",
            subject_name, subj, elems, subject_name
        )
    };

    // `codegen` is threaded only to keep the signature uniform with the
    // other emitters; the guard lowering needs no ctx lookups.
    let _ = codegen;
    Some(format!("{{ {} {} }}", setup, chain))
}

/// Recursively lower one tuple-subpattern of an Int-literal match against a
/// `place` expression (a Rust expression denoting the *value place* of the
/// element, e.g. `(*__lit0)` or `(*__lit1).0`). Appends an equality guard for
/// every Int-literal LEAF (at any depth), binds identifier leaves into
/// `prelude`, and ignores wildcards. Nested tuples destructure via field-index
/// access (`{place}.{i}`) — no fresh `match` bindings, so hygiene is automatic.
///
/// Returns `false` if any leaf is an unsupported shape (a non-Int literal, a
/// ctor, …); the caller then bails to the hard codegen diagnostic. This is the
/// arbitrarily-nested generalization of the one-level element loop above.
fn lower_int_literal_subpatterns(
    pat: &ResolvedPattern,
    place: &str,
    conds: &mut Vec<String>,
    prelude: &mut String,
) -> bool {
    match pat {
        ResolvedPattern::Literal(crate::ast::Literal::Int(n)) => {
            // `place` is a value place; `&{place}` is `&AverInt`, comparable to
            // the literal reference.
            conds.push(format!("&{} == &aver_rt::AverInt::from_i64({})", place, n));
            true
        }
        ResolvedPattern::Literal(crate::ast::Literal::BigInt(s)) => {
            // Nested tuple elements are always boxed `AverInt`s; compare against
            // the exact value parsed from the digits.
            conds.push(format!(
                "&{} == &{:?}.parse::<aver_rt::AverInt>().unwrap()",
                place, s
            ));
            true
        }
        ResolvedPattern::Wildcard => true,
        ResolvedPattern::Ident(name) if name == "_" => true,
        ResolvedPattern::Ident(name) => {
            let rust = aver_name_to_rust(name);
            // Clone the value place into an owned binding.
            prelude.push_str(&format!("let {} = {}.clone(); ", rust, place));
            true
        }
        ResolvedPattern::Tuple(pats) => {
            for (i, sub) in pats.iter().enumerate() {
                // Field `i` of the tuple at `place` is itself a value place.
                let sub_place = format!("{}.{}", place, i);
                if !lower_int_literal_subpatterns(sub, &sub_place, conds, prelude) {
                    return false;
                }
            }
            true
        }
        // A non-Int literal, ctor, or any other shape is out of scope.
        _ => false,
    }
}

/// Emit `lhs op rhs` for `==` / `!=` so that both Rust operands have the
/// same shape. Every comparison site must go through here: the plain
/// expression walker and the `if` lowering of a boolean `match` used to
/// build the operator text separately, and the second site missed the
/// borrow alignment (#1037: `match script == made()` compiled to
/// `(script == made())`, a `&T == T` comparison Rust rejects).
fn mir_emit_equality(
    bop: &crate::ir::mir::MirBinOp,
    l: &str,
    r: &str,
    op_str: &str,
    emit_ctx: &MirEmitCtx<'_>,
) -> String {
    // `AverStr` (Rc<str>) does not implement `PartialEq<&str>`: deref the
    // non-literal side when the other side is a string literal.
    if let MirExpr::Literal(lit) = &bop.rhs.node
        && let crate::ast::Literal::Str(s) = &lit.node
    {
        return format!("(&*{} {} {:?})", l, op_str, s);
    }
    if let MirExpr::Literal(lit) = &bop.lhs.node
        && let crate::ast::Literal::Str(s) = &lit.node
    {
        return format!("({:?} {} &*{})", s, op_str, r);
    }
    // Borrow-by-default and mutual-TCO invariant hoisting make collection /
    // wrapper / named-type parameters `&T`, while calls and constructors still
    // produce an owned `T`. Rust does not provide `PartialEq<&T> for T`, so
    // compare two references whenever exactly one side is a borrowed
    // parameter. Borrowing the owned expression is temporary, allocation-free,
    // and preserves evaluation order.
    let lhs_borrowed = mir_expr_is_borrowed_param_read(&bop.lhs.node, emit_ctx);
    let rhs_borrowed = mir_expr_is_borrowed_param_read(&bop.rhs.node, emit_ctx);
    match (lhs_borrowed, rhs_borrowed) {
        (false, true) => format!("(&({}) {} {})", l, op_str, r),
        (true, false) => format!("({} {} &({}))", l, op_str, r),
        _ => format!("({} {} {})", l, op_str, r),
    }
}

/// Is this expression a direct read whose emitted Rust representation is `&T`?
///
/// This includes both ordinary borrow-by-default params and mutual-TCO
/// invariants hoisted out of the state enum. A direct read emits as `name`,
/// whose Rust type is `&T`. Projections and compound expressions have their own
/// ownership rules and deliberately do not count as borrowed-param reads here.
fn mir_expr_is_borrowed_param_read(expr: &MirExpr, emit_ctx: &MirEmitCtx<'_>) -> bool {
    local_of(expr).is_some_and(|local| {
        emit_ctx.is_borrowed_param(&local.name)
            || (emit_ctx.rc_wrapped_are_borrowed_refs && emit_ctx.is_rc_wrapped(&local.name))
    })
}

/// Emit the FULL function body the MIR walker produces, in the
/// `emit_fn_body` format — the leading
/// `    crate::cancel_checkpoint();\n    ` then the body expression.
/// Returns `None` when the walker can't render the body (any uncovered
/// construct anywhere in the tree), the signal for the caller to emit a
/// hard codegen diagnostic.
///
/// The tail expression is a return position, so it goes through
/// [`emit_mir_tail_value`] to materialise an owned value.
///
/// A top-level `Let` chain (the MIR shape a `Block` body with `let`
/// bindings lowers to) is emitted as flat statement lines —
/// `    let a = …;\n    let b = …;\n    <final-expr>` — instead of the
/// nested block-expr `{ let a = …; { let b = …; … } }` `emit_mir_expr`
/// renders for an inline `Let`. See [`emit_mir_let_chain_flat`].
pub(super) fn emit_mir_fn_body(
    body: &Spanned<MirExpr>,
    emit_ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // A top-level `Let` is a multi-statement body, emitted as flat
    // statement lines (named binding → `let …;`, discarded intermediate
    // `Stmt::Expr` → bare `…;`) then the final expression on its own
    // line — never a nested block-expr. The chain handles both named and
    // empty-`binding_name` (discarded) bindings, so no first-binding
    // guard is needed.
    if let MirExpr::Let(spanned_let) = &body.node
        && let Some(lines) = emit_mir_let_chain_flat(&spanned_let.node, emit_ctx)
    {
        return Some(format!("    crate::cancel_checkpoint();\n    {}", lines));
    }

    // Int unboxing: a non-TCO fn whose return is bare `i64` must emit its
    // tail value bare. The tail is the whole body (or, for a `Match` /
    // `IfThenElse`, every arm value). `emit_bare_return_tail` handles those
    // shapes; a `None` means the tail isn't a renderable-bare shape, in
    // which case we fall through to the boxed emit (which would be a type
    // error — but `bare_return` was only set when `tail_value_is_bare`
    // proved every leaf bare, so this renders for the shapes that earned
    // the bare return).
    if emit_ctx.bare.bare_return
        && let Some(tail) = emit_bare_return_tail(body, emit_ctx)
    {
        return Some(format!("    crate::cancel_checkpoint();\n    {}", tail));
    }

    // ETAP-2 SLICE 1: the boxed-return tail boundary (defect Q5 / subj_ret)
    // is now EXPLICIT — the `bare_i64_rewrite` pass already `Box`ed every
    // bare leaf reaching a boxed return, and `emit_mir_expr` lowers those
    // `Box` nodes. So the default emit below renders the boxed-return tail
    // correctly without a codegen-side boxing pass.

    let code = emit_mir_tail_value(body, emit_ctx)?;
    Some(format!("    crate::cancel_checkpoint();\n    {}", code))
}

/// Emit a fn body's tail expression as an OWNED value.
///
/// The tail is a return position, so whatever lands there has to own what
/// it hands back — the same rule [`mir_maybe_clone`] already applies to a
/// call argument, a match arm and a TCO base-case return. Two shapes reach
/// it needing the wrapper, because `emit_mir_expr` renders both bare: a
/// read of a borrowed param (`&T` against a by-value return type) and a
/// field read through one at any depth (a move out of a shared reference).
///
/// The tail applies a WIDER rule than [`emit_mir_binding_value`] does, and
/// the difference is deliberate rather than incidental: a bare `Local` tail
/// goes through the full [`mir_maybe_clone`] policy, which also covers an
/// `rc_wrapped` local (`(*x).clone()`) and a non-last-use owned local
/// (`x.clone()`) — neither of which is a borrowed-param read. Only the
/// non-`Local` shapes fall back to the narrow root-of-a-projection rule.
/// The extra reach is inert today (`rc_wrapped` is populated only on the
/// TCO paths, and a top-level tail read is by definition a last use), which
/// is why the binding position was not widened to match: it would change no
/// emitted byte and would cost a second policy to keep honest. A future
/// merge of the TCO and non-TCO policies would make the difference live, so
/// widen the binding rule at the same time if that ever happens.
fn emit_mir_tail_value(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    let code = emit_mir_expr(expr, ctx)?;
    if local_of(&expr.node).is_some() {
        return Some(mir_maybe_clone(code, &expr.node, ctx));
    }
    Some(mir_own_borrowed_read(code, &expr.node, ctx))
}

/// Emit a `let` binding's value as an OWNED value when it reads a BORROWED
/// PARAM — and only then.
///
/// Naming a read of a borrowed param does not change what a function may do
/// with it: `p.y` in return position and `y = p.y` then `y` are the same
/// program. `emit_mir_expr` renders a binding value raw, so the second one
/// used to bind `&T` (or move a field out of a shared reference) and the
/// generated project would not build. The binding is an owning position for
/// the same reason the tail is — the name outlives the expression that
/// produced it — so it goes through the same materialisation step.
///
/// KNOWN GAP (pre-existing, not closed here): the rule fires only when
/// [`mir_projection_root_local`] bottoms out in a borrowed param. A binding
/// whose value is an OWNED local — a plain `let`, or an `own_param`-
/// graduated by-value collection param — is still emitted as a raw move, so
/// naming it and then reading the original does change what the fn may do:
///
/// ```text
/// fn twice(n: Int) -> Int
///     b = Vector.new(n, 1)
///     a = b
///     Vector.len(a) + Vector.len(b)
/// ```
///
/// emits `let a = b;` and rustc rejects `b.len()` after it (E0382). The wide
/// rule would close it for free — [`mir_maybe_clone`] emits `.clone()` for a
/// local read that is not `last_use`, which is exactly this shape, and
/// [`emit_mir_tail_value`] already uses it for a bare `Local` tail. It is
/// left alone because widening here changes emitted bytes for every fn that
/// names a live local, and the corpus has no instance of the shape
/// (`examples/` and `self_hosted/` contain no bare `name = name` binding).
/// Close it together with the tail/binding rule merge, not on its own.
fn emit_mir_binding_value(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    let code = emit_mir_expr(expr, ctx)?;
    Some(mir_own_borrowed_read(code, &expr.node, ctx))
}

/// Materialise an owned value from a read that bottoms out in a borrowed
/// param, leaving every other expression untouched.
///
/// The base chain is walked to its ROOT local rather than checked one level
/// down: `s.piece.kind` is behind the same shared reference `s.piece` is, so
/// a depth-1 rule leaves every nested read raw. Anything whose root is not a
/// borrowed param — a call result, a constructor, an owned local — already
/// owns what it produces and is returned verbatim, which is also what keeps
/// this from double-cloning an argument (`mir_clone_arg` runs inside
/// `emit_mir_expr`, on the arguments, never on the whole call).
fn mir_own_borrowed_read(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    match mir_projection_root_local(expr) {
        Some(local) if ctx.is_borrowed_param(&local.name) => mir_maybe_clone(code, expr, ctx),
        _ => code,
    }
}

/// The local a chain of field reads bottoms out in: `p` for `p`, and `s` for
/// both `s.piece` and `s.piece.kind`. `None` for anything else — a call, a
/// constructor, an index, a literal — which owns its result already.
fn mir_projection_root_local(expr: &MirExpr) -> Option<&MirLocal> {
    match expr {
        MirExpr::Local(_) => local_of(expr),
        MirExpr::Project(p) => mir_projection_root_local(&p.node.base.node),
        _ => None,
    }
}

/// Emit a non-TCO body's tail value as a bare `i64` expression. The tail
/// is the value the fn returns; for `Match` / `IfThenElse` it is every arm
/// value (each rendered bare). A bare leaf (`Local` / literal / arithmetic)
/// renders via [`emit_bare_i64`]. Returns `None` for any shape the bare
/// path can't render (the caller then falls back to the boxed emit). Only
/// invoked when the analysis proved `bare_return` (every tail leaf
/// bare-eligible), so the supported shapes cover the bare-return cases.
fn emit_bare_return_tail(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    match &expr.node {
        // A directly bare-eligible leaf / arithmetic tail.
        _ if mir_expr_is_bare_i64(expr, ctx) => {
            emit_bare_i64(expr).or_else(|| emit_mir_expr(expr, ctx))
        }
        // A `Match` over Int literals whose arm bodies are bare tails. Reuse
        // the Int-literal guard chain but render each arm's tail bare.
        MirExpr::Match(m) => emit_mir_match_with(&m.node, ctx, &|arm_body, ctx| {
            emit_bare_return_tail(arm_body, ctx)
        }),
        MirExpr::IfThenElse(ite) => {
            let (cond, then_src, else_src) = mir_if_cond_and_branches(&ite.node, ctx)?;
            let then_b = emit_bare_return_tail(then_src, ctx)?;
            let else_b = emit_bare_return_tail(else_src, ctx)?;
            Some(format!(
                "if {} {{ {} }} else {{ {} }}",
                cond, then_b, else_b
            ))
        }
        MirExpr::Let(l) => {
            // A let-chain ending in a bare tail: render the bindings via the
            // flat emitter, then the bare final. Fall back to None if the
            // chain isn't flat-renderable.
            let _ = l;
            None
        }
        _ => None,
    }
}

/// Emit a top-level `Let` chain as flat Rust statement lines: each
/// binding becomes `let {name} @ _ = {value};`, one per line, 4-space
/// indented and `\n`-joined, terminated by the chain's final expression
/// on its own line.
///
/// Both the binding value and the final expression are owning positions,
/// so each goes through its materialisation step —
/// [`emit_mir_binding_value`] and [`emit_mir_tail_value`]. A chain that
/// ends in one of its own params returns it by value, and one that names
/// a param (or a field read through one) part-way through owns what it
/// named.
///
/// The chain is the run of directly-nested `Let` nodes: each one emits
/// its statement line and continues into its body until a body that
/// isn't a `Let` becomes the final expression. A named binding emits
/// `let {name} @ _ = {value};`; an empty-`binding_name` binding (a
/// discarded intermediate `Stmt::Expr` or a `_ = effect()` discard)
/// emits a bare `{value};` statement (the value evaluated for its
/// effects, result dropped). Returns `None` only when a binding value or
/// the final expression can't render.
fn emit_mir_let_chain_flat(
    let_node: &crate::ir::mir::MirLet,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut lines: Vec<String> = Vec::new();
    let mut current = let_node;
    loop {
        let value = emit_mir_binding_value(&current.value, ctx)?;
        if current.binding_name.is_empty() {
            // Discarded intermediate (`Stmt::Expr` at non-tail position,
            // or a `_ = effect()` discard binding). No source ident to
            // bind — emit the value as a bare statement and drop it, the
            // exact mirror of HIR's non-last `ResolvedStmt::Expr` arm
            // (`{expr};`). Typically an effectful builtin call
            // (`Console.print(…)`) evaluated for its effect.
            lines.push(format!("{};", value));
        } else {
            let name = explicit_binding_pattern(&current.binding_name);
            lines.push(format!("let {} = {};", name, value));
        }

        // Continue the chain when the body is another `Let` (named or a
        // discarded intermediate); the first non-`Let` body is the final
        // expression. Both binder shapes lower to flat statement lines,
        // so the nested-block shape never needs to appear.
        match &current.body.node {
            MirExpr::Let(next) => {
                current = &next.node;
            }
            _ => {
                // Int unboxing: a bare-return fn's let-chain tail emits the
                // final value bare so the returned expression's type is
                // `i64`.
                let final_expr = if ctx.bare.bare_return
                    && let Some(bare) = emit_bare_return_tail(&current.body, ctx)
                {
                    bare
                } else {
                    emit_mir_tail_value(&current.body, ctx)?
                };
                lines.push(final_expr);
                break;
            }
        }
    }
    Some(lines.join("\n    "))
}

// ── Production body emit (MIR is the sole codegen path) ─────────────────
//
// The HIR walker was deleted in rust-on-MIR W6/Stage-3, so there is no
// byte-parity gate left: the MIR walker OWNS all runtime codegen. This
// helper builds the per-fn `MirFnEmitPolicy` (param types /
// borrow-by-default) exactly as the HIR `build_fn_ectx_from_resolved`
// did, wraps it in a `MirEmitCtx`, and renders the body. A `None`
// propagates to the caller, which emits a hard codegen diagnostic — the
// only constructs that hit it are the verify-only Oracle/trace residual
// that never built on the Rust backend.

/// Render a non-TCO fn body via the MIR walker. `resolved` supplies the
/// borrow policy (param types / borrow-by-default), recomputed exactly
/// as `build_fn_ectx_from_resolved` does. Returns the body string in the
/// `emit_fn_body` format (`    crate::cancel_checkpoint();\n    …`), or
/// `None` when the walker can't render the body.
pub(super) fn emit_mir_fn_body_routed(
    mir_fn: &crate::ir::mir::MirFn,
    resolved: &crate::ir::hir::ResolvedFnDef,
    scope: Option<&str>,
    borrow_by_default: bool,
    post_checkpoint_prologue: Option<&str>,
    ctx: &CodegenContext,
) -> Option<String> {
    let mut policy = MirFnEmitPolicy::from_resolved(resolved, scope, borrow_by_default);
    // Graduate own_param-proven collection params to owned-by-value so
    // the body skips the `.clone()` at last-use mutation sites. The
    // SIGNATURE (`emit_fn_def_with_visibility`) computes the SAME owned
    // set from the same `mir_fn.aliased_slots` and emits `mut p: T`, so
    // body and signature agree on which params are owned.
    policy.apply_own_param(mir_fn);
    // Apply the Int unboxing facts so a proven-bare slot emits native
    // `i64`. Same per-fn slice the signature emit reads (via
    // `bare_fn_facts`), so body and signature agree on which params /
    // return are bare.
    policy.apply_bare_i64(mir_fn.fn_id, ctx);
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);
    let body = emit_mir_fn_body(&mir_fn.body, &emit_ctx)?;
    let Some(prologue) = post_checkpoint_prologue else {
        return Some(body);
    };

    // `emit_mir_fn_body` owns this prefix as part of its documented output
    // contract. Keep backend-specific representation fast paths after the
    // cooperative cancellation point but before any source-level work.
    const CHECKPOINT: &str = "    crate::cancel_checkpoint();\n";
    let tail = body.strip_prefix(CHECKPOINT)?;
    Some(format!("{CHECKPOINT}{prologue}\n{tail}"))
}

/// Is the type stamp a primitive numeric?
/// `Int` / `Float` count; everything else (incl. `Str`)
/// doesn't. Mirror of HIR's `EmitCtx::expr_is_numeric` for the
/// MIR walker's `+` dispatch.
fn ty_is_numeric(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Int | Type::Float))
}

/// Is the type stamp exactly `Int`? Drives the `AverInt`-method lowering
/// of arithmetic (`+ - * /`, unary `-`) — `Int` operands take the
/// non-wrapping method calls, everything else (notably `Float`) keeps the
/// raw Rust operator.
fn ty_is_int(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Int))
}

// ── TCO loop / trampoline synthesis from MIR ────────────────────────────
//
// Rust has no TCO primitive — the VM emits a `TAIL_CALL` opcode and
// wasm-gc a `return_call`, both flat instructions. In generated Rust the
// loop (self-recursive) and the trampoline (mutual-recursive) STRUCTURE
// is synthesized in source from `MirExpr::TailCall` (a self-`TailCall`
// arm becomes `continue` after rebinding the loop's mutable params; a
// value arm becomes `return`).
//
// The MIR walker emits its OWN correct loop / trampoline, verified
// BEHAVIORALLY (build + run vs VM + self-host regen):
//
//   * **Always-snapshot param rebind.** For every rebound param, emit
//     `let __tcoN = <arg>;` for ALL of them first, then
//     `param = __tcoN;` in order, then `continue;`. Strictly correct
//     (no read-after-write clobber), no substring heuristic. Identity
//     rebinds (`arg == param`) and pass-through (rc) params are skipped.
//   * **No loop-invariant hoisting** (correctness needs none).
//
// The ownership / borrow facts (rc pass-through params Arc-wrapped on
// the self-loop / `&T` extra trampoline args; non-rc owned params `mut`
// with NO borrow-by-default) come from resolved HIR for both self- and
// mutual-TCO. Tail-call membership is therefore always an `FnId` decision.
// Get the ownership wrong → rustc rejects, which the build gate catches.

/// Emit a self-TCO fn entirely from MIR: the public signature
/// (`mut`-owned params, rc params Arc-wrapped before the loop) + the
/// `loop { cancel_checkpoint(); <tco-body> }` wrapper, where the body
/// renders self-`TailCall` arms as `{ rebind; continue }` and value arms
/// as `return <expr>;`.
///
/// `resolved_fd` supplies identity and typed parameters for rc/pass-through
/// computation; `fd` is retained only for source metadata used by the emitted
/// signature. `mir_fn.body` is walked in tail position. Returns `None` when a
/// sub-expression cannot render.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_mir_tco_fn(
    fd: &crate::ast::FnDef,
    resolved_fd: &crate::ir::hir::ResolvedFnDef,
    mir_fn: &crate::ir::mir::MirFn,
    fn_name: &str,
    ret_type: &str,
    visibility: &str,
    scope: Option<&str>,
    ctx: &CodegenContext,
) -> Option<String> {
    use super::toplevel::{compute_resolved_rc_params, compute_resolved_self_passthrough_params};

    let passthrough_indices = compute_resolved_self_passthrough_params(resolved_fd);
    let rc_indices = compute_resolved_rc_params(std::slice::from_ref(&resolved_fd));
    let rc_names: HashSet<String> = rc_indices
        .iter()
        .filter_map(|&i| resolved_fd.params.get(i).map(|(name, _)| name.clone()))
        .collect();

    // Borrow policy: no borrow-by-default (owned `mut` params), rc
    // params wrapped (`(*x).clone()` on read). Mirror of
    // `emit_tco_fn`'s `build_fn_ectx_no_borrow_from_resolved` +
    // `with_rc_wrapped`.
    let mut policy = MirFnEmitPolicy::from_resolved(resolved_fd, scope, /* borrow */ false);
    policy.rc_wrapped = rc_names.clone();
    // own_param-proven collection params are already `mut`-owned in the
    // TCO signature (`emit_tco_params_mir`); graduating them only flips
    // the body's clone-skip on. A pass-through param Arc-wrapped via
    // `rc_wrapped` keeps its `&T` / `(*x).clone()` shape — rc-wrapping is
    // a structural TCO decision that takes precedence, so drop any
    // rc-wrapped name back out of `owned_params` to keep signature and
    // body consistent.
    policy.apply_own_param(mir_fn);
    // Int unboxing: a bare `i64` counter param is `Copy`-by-value, so it
    // is never rc-wrapped — a param bare in the summary is disjoint from
    // `rc_wrapped` by construction (rc only wraps non-Copy pass-through
    // collection params). The signature emit (`emit_tco_params_mir`)
    // reads the same per-fn facts so both agree.
    policy.apply_bare_i64(mir_fn.fn_id, ctx);
    for n in &rc_names {
        policy.owned_params.remove(n);
    }
    let emit_ctx = MirEmitCtx::for_fn(ctx, &policy);

    // Render the body in tail position FIRST — bail before emitting any
    // signature if the walker can't render it.
    let body_code = emit_mir_tco_body(
        &mir_fn.body,
        mir_fn.fn_id,
        &fd.params,
        &passthrough_indices,
        &emit_ctx,
    )?;

    // Int unboxing: a bare-`i64` param/return emits `i64` in the signature
    // (the load-bearing cross-frame change), read off the SAME per-fn facts
    // the body emit applied (`apply_bare_i64`). Every caller converts at the
    // boundary, so the ABI stays self-consistent.
    let bare_facts = ctx.bare_i64.for_fn(mir_fn.fn_id);
    let params = emit_tco_params_mir(&fd.params, &rc_indices, bare_facts, ctx, scope);
    let ret_type = bare_return_type(ret_type, bare_facts);
    let mut lines = Vec::new();
    lines.push(format!(
        "{}fn {}({}) -> {} {{",
        visibility, fn_name, params, ret_type
    ));
    // Wrap pass-through params in Arc before the loop (shadowing the
    // original binding). Mirror of `emit_tco_fn`. Emit in source-param
    // order: `rc_indices` is a HashSet, and iterating it directly made
    // checked-in self-host regeneration differ across processes.
    lines.extend(emit_self_tco_rc_wraps(&fd.params, &rc_indices));
    lines.push("    loop {".to_string());
    lines.push(body_code);
    lines.push("    }".to_string());
    lines.push("}".to_string());
    Some(lines.join("\n"))
}

fn emit_self_tco_rc_wraps(params: &[(String, String)], rc_indices: &HashSet<usize>) -> Vec<String> {
    assert!(
        rc_indices.iter().all(|index| *index < params.len()),
        "self-TCO rc parameter index must refer to a declared parameter"
    );
    params
        .iter()
        .enumerate()
        .filter(|(i, _)| rc_indices.contains(i))
        .map(|(_, (name, _))| {
            let rust_name = aver_name_to_rust(name);
            let binding = explicit_binding_pattern(name);
            format!("    let {} = std::sync::Arc::new({});", binding, rust_name)
        })
        .collect()
}

/// Self-TCO param signature: non-rc params are `mut T` (rebound in the
/// loop), rc params are plain `T` (shadowed by the Arc::new binding).
/// Mirror of `emit_fn_params_tco`. A param the unboxing analysis proved
/// bare (`bare_facts.param_is_bare(i)`) emits `mut p: i64` instead of
/// `mut p: aver_rt::AverInt`.
fn emit_tco_params_mir(
    params: &[(String, String)],
    rc_indices: &std::collections::HashSet<usize>,
    bare_facts: Option<&crate::ir::mir::FnBareFacts>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, (name, type_ann))| {
            let is_bare = bare_facts.is_some_and(|f| f.param_is_bare(i));
            let rust_type = if is_bare {
                "i64".to_string()
            } else {
                super::types::type_annotation_to_rust_scoped(type_ann, ctx, scope)
            };
            if rc_indices.contains(&i) {
                format!("{}: {}", explicit_parameter_pattern(name, false), rust_type)
            } else {
                format!("{}: {}", explicit_parameter_pattern(name, true), rust_type)
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// The return type for a fn whose return the unboxing analysis proved bare:
/// `i64`, else the original `ret_type` string. Used by the self-TCO and
/// non-TCO signature emit.
fn bare_return_type(ret_type: &str, bare_facts: Option<&crate::ir::mir::FnBareFacts>) -> String {
    if bare_facts.is_some_and(|f| f.bare_return) {
        "i64".to_string()
    } else {
        ret_type.to_string()
    }
}

/// Emit the self-TCO loop body (inside `loop { … }`). Leads with
/// `cancel_checkpoint();`, then renders the MIR body in tail position. A
/// top-level `Let` chain (leading bindings) emits flat `let x @ _ = v;` lines
/// then recurses into the chain's final expression as a tail expr.
fn emit_mir_tco_body(
    body: &Spanned<MirExpr>,
    self_fn: crate::ir::FnId,
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut lines = Vec::new();
    lines.push("        crate::cancel_checkpoint();".to_string());

    // Walk the leading `Let` chain as plain statements, then the final
    // expression as a tail expr. A named binding emits `let x @ _ = v;`; an
    // empty-`binding_name` binding (a discarded intermediate `Stmt::Expr`
    // or a `_ = effect()` discard) emits a bare `v;` statement (the value
    // evaluated for its effect, result dropped) — the mirror of HIR's
    // non-last `Stmt::Expr` arm.
    let mut current = body;
    while let MirExpr::Let(spanned_let) = &current.node {
        let let_node = &spanned_let.node;
        let value = emit_mir_expr(&let_node.value, ctx)?;
        if let_node.binding_name.is_empty() {
            lines.push(format!("        {};", value));
        } else {
            let name = explicit_binding_pattern(&let_node.binding_name);
            lines.push(format!("        let {} = {};", name, value));
        }
        current = &let_node.body;
    }

    let tail = emit_mir_tco_tail_expr(current, self_fn, params, passthrough, ctx)?;
    lines.push(format!("        {}", tail));
    Some(lines.join("\n"))
}

/// Emit a MIR expression in self-TCO tail position. Self-`TailCall` →
/// `{ rebind; continue }`; `Match` / `IfThenElse` recurse into arms
/// (still tail position); anything else is a base-case value → `return
/// <expr>;`.
fn emit_mir_tco_tail_expr(
    expr: &Spanned<MirExpr>,
    self_fn: crate::ir::FnId,
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match &expr.node {
        MirExpr::TailCall(spanned_tc) => {
            let tc = &spanned_tc.node;
            if tc.target == self_fn && tc.args.len() == params.len() {
                emit_mir_self_tco_continue(&tc.args, params, passthrough, ctx)
            } else {
                // Tail call to a DIFFERENT fn (out of this self-loop):
                // emit a plain call + return. The leverage note's
                // module-DAG invariant means a self-TCO body's tail
                // calls target itself; a foreign target here is rare but
                // handled correctly.
                let name = ctx.symbol_table.fn_entry(tc.target).key.canonical();
                Some(format!(
                    "return {};",
                    emit_named_call(&name, &tc.args, ctx)?
                ))
            }
        }
        MirExpr::Match(spanned_match) => {
            emit_mir_match_with(&spanned_match.node, ctx, &|arm_body, ctx| {
                emit_mir_tco_tail_expr(arm_body, self_fn, params, passthrough, ctx)
            })
        }
        MirExpr::IfThenElse(spanned_ite) => {
            emit_mir_tco_if_then_else(&spanned_ite.node, self_fn, params, passthrough, ctx)
        }
        // Base-case value (or `?` / let-bound value): `return <expr>;`.
        _ => Some(format!("return {};", emit_mir_value_return(expr, ctx)?)),
    }
}

/// Render a MIR `IfThenElse` in TCO tail position — both branches stay
/// in tail position (recurse). Reuses the condition canonicalization
/// from [`emit_mir_if_then_else`] would be ideal, but that helper
/// renders branches as values; here branches are tail exprs, so we
/// re-derive the condition the same way (the MIR `bool_match_to_if` pass
/// is the only producer).
fn emit_mir_tco_if_then_else(
    ite: &crate::ir::mir::MirIfThenElse,
    self_fn: crate::ir::FnId,
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let (cond, then_src, else_src) = mir_if_cond_and_branches(ite, ctx)?;
    let then_branch = emit_mir_tco_tail_expr(then_src, self_fn, params, passthrough, ctx)?;
    let else_branch = emit_mir_tco_tail_expr(else_src, self_fn, params, passthrough, ctx)?;
    Some(format!(
        "if {} {{ {} }} else {{ {} }}",
        cond, then_branch, else_branch
    ))
}

/// Render a value expression for a `return` in a TCO / trampoline base
/// case. Mirror of `emit_mir_expr` + the owning-position `maybe_clone`,
/// plus the HIR `emit_tco_expr` `_` arm's bare-rc-ident deref-clone:
/// returning a pass-through param (Arc<T> / &T) needs `(*x).clone()` to
/// yield an owned `T`.
fn emit_mir_value_return(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    // Int unboxing: when the fn's return type is bare `i64`, the base-case
    // value must be emitted bare too (a bare `Local`, a bare literal, or
    // bare arithmetic) so the returned expression's type is `i64`. The
    // analysis only set `bare_return` when EVERY tail leaf is bare-eligible
    // (`tail_value_is_bare`), so this path renders.
    if ctx.bare.bare_return && mir_expr_is_bare_i64(expr, ctx) {
        return emit_bare_i64(expr).or_else(|| emit_mir_expr(expr, ctx));
    }
    // ETAP-2 SLICE 1: the boxed-return tail boundary (defects Q5 / subj_ret)
    // is now an EXPLICIT `Box` node the `bare_i64_rewrite` pass inserted
    // around each bare leaf reaching a boxed return. `emit_mir_expr` lowers
    // those `Box` nodes, so the default path renders the boxed return
    // correctly without a codegen-side boxing pass.
    let code = emit_mir_expr(expr, ctx)?;
    Some(mir_maybe_clone(code, &expr.node, ctx))
}

/// Emit the self-TCO `{ rebind; continue }` block from the tail-call
/// args, using the always-snapshot rule. Pass-through (rc) params and
/// identity rebinds (`arg == param`) are skipped; every other rebound
/// param gets a `let __tcoN = <arg>;` snapshot first (avoiding
/// read-after-write clobber), then `param = __tcoN;` in order, then
/// `continue;`.
fn emit_mir_self_tco_continue(
    args: &[Spanned<MirExpr>],
    params: &[(String, String)],
    passthrough: &std::collections::HashSet<usize>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // An identity arg is not emitted at all: the loop keeps the current
    // binding. That binding is nevertheless live into the next iteration.
    // Make that hidden loop-edge liveness visible to the ordinary owning-
    // position clone policy while rendering the other args. This matters
    // when a later arg moves the same param into a helper call.
    let identity: Vec<bool> = params
        .iter()
        .enumerate()
        .map(|(i, (name, _))| {
            passthrough.contains(&i)
                || local_of(&args[i].node).is_some_and(|local| local.name == *name)
        })
        .collect();
    let loop_carried_params: HashSet<String> = params
        .iter()
        .enumerate()
        .filter(|(i, _)| identity[*i])
        .map(|(_, (name, _))| name.clone())
        .collect();
    let mut tail_ctx = *ctx;
    tail_ctx.loop_carried_params = &loop_carried_params;

    let mut arg_strs = vec![None; args.len()];
    for (i, a) in args.iter().enumerate() {
        if identity[i] {
            continue;
        }
        // ETAP-2 SLICE 1: the rebind-value representation boundary is now
        // EXPLICIT — the `bare_i64_rewrite` pass already wrapped each
        // self-tail-call arg for the self-fn's i-th param representation
        // (`Box` for a raw arg into a boxed loop var, `Unbox` for a boxed
        // arg into a bare `i64` loop var). Codegen renders the arg:
        //   - bare param (`mut p: i64`): a raw leaf / compound / literal
        //     renders bare via `emit_bare_i64`; an `Unbox`-wrapped boxed arg
        //     falls through to `emit_mir_expr` (lowering `Unbox`).
        //   - boxed param: `emit_mir_expr` (lowering any `Box` node), then
        //     the clone policy.
        if ctx.bare.param_is_bare(i) {
            let rendered = emit_bare_i64(a).or_else(|| emit_mir_expr(a, &tail_ctx));
            arg_strs[i] = Some(rendered?);
        } else {
            let code = emit_mir_expr(a, &tail_ctx)?;
            arg_strs[i] = Some(mir_clone_arg(code, &a.node, &tail_ctx));
        }
    }

    let mut lines = Vec::new();
    lines.push("{".to_string());
    // Phase 1: snapshot ALL rebound args into temps (always-snapshot).
    for (i, arg_str) in arg_strs.iter().enumerate() {
        if let Some(arg_str) = arg_str {
            lines.push(format!("            let __tco{} = {};", i, arg_str));
        }
    }
    // Phase 2: assign temps back to params, in order.
    for (i, (name, _)) in params.iter().enumerate() {
        if arg_strs[i].is_some() {
            lines.push(format!(
                "            {} = __tco{};",
                aver_name_to_rust(name),
                i
            ));
        }
    }
    lines.push("            continue;".to_string());
    lines.push("        }".to_string());
    Some(lines.join("\n"))
}

/// Recompute the canonicalized condition + the (possibly swapped) tail
/// branches for a MIR `IfThenElse`. Shared by the value emitter
/// ([`emit_mir_if_then_else`]) and the TCO emitter — extracted so the
/// condition-rewrite logic lives in one place.
fn mir_if_cond_and_branches<'a>(
    ite: &'a crate::ir::mir::MirIfThenElse,
    ctx: &MirEmitCtx<'_>,
) -> Option<(String, &'a Spanned<MirExpr>, &'a Spanned<MirExpr>)> {
    let canonical_compare = |op: BinOp| -> Option<(&'static str, bool)> {
        match op {
            BinOp::Eq => Some(("==", false)),
            BinOp::Neq => Some(("==", true)),
            BinOp::Lt => Some(("<", false)),
            BinOp::Gte => Some(("<", true)),
            BinOp::Gt => Some((">", false)),
            BinOp::Lte => Some((">", true)),
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => None,
        }
    };
    match &ite.cond.node {
        MirExpr::BinOp(spanned_binop) if canonical_compare(spanned_binop.node.op).is_some() => {
            let bop = &spanned_binop.node;
            let (op_str, invert) = canonical_compare(bop.op).expect("checked by guard");
            // Int unboxing: a comparison between two bare `i64` operands
            // emits raw `i64` operands (a bare counter `i == 0` compares
            // `i64 == 0i64`, not `i64 == AverInt`). Mirror of the
            // comparison bare path in `emit_mir_expr`.
            let (l, r) = if mir_expr_is_bare_i64(&bop.lhs, ctx)
                && mir_expr_is_bare_i64(&bop.rhs, ctx)
                && let Some(lb) = emit_bare_i64(&bop.lhs)
                && let Some(rb) = emit_bare_i64(&bop.rhs)
            {
                (lb, rb)
            } else {
                (emit_mir_expr(&bop.lhs, ctx)?, emit_mir_expr(&bop.rhs, ctx)?)
            };
            let cond = if matches!(bop.op, BinOp::Eq | BinOp::Neq)
                && !(mir_expr_is_bare_i64(&bop.lhs, ctx) && mir_expr_is_bare_i64(&bop.rhs, ctx))
            {
                mir_emit_equality(bop, &l, &r, op_str, ctx)
            } else {
                format!("({} {} {})", l, op_str, r)
            };
            if invert {
                Some((cond, &ite.else_branch, &ite.then_branch))
            } else {
                Some((cond, &ite.then_branch, &ite.else_branch))
            }
        }
        _ => {
            let cond = emit_mir_expr(&ite.cond, ctx)?;
            Some((cond, &ite.then_branch, &ite.else_branch))
        }
    }
}

// ── mutual-recursion trampoline from MIR ────────────────────────────────

/// Emit a mutual-TCO block from MIR: a state enum (one variant per
/// member, payload = non-rc param values), a trampoline dispatch loop
/// (member-`TailCall` bounces to a new enum variant, a value `return`s),
/// and thin wrapper fns. The member bodies are walked from MIR
/// (`MirFn.body`).
///
/// `group_fns` is the resolved-HIR SCC; `mir_fns` are the matching `MirFn`s
/// in the same order. Returns `None`
/// (→ the caller emits a hard codegen diagnostic for the whole block)
/// when any member body can't render — the block is all-or-nothing
/// because the members share one trampoline.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_mir_mutual_tco_block(
    group_id: usize,
    group_fns: &[&crate::ir::hir::ResolvedFnDef],
    mir_fns: &[&crate::ir::mir::MirFn],
    ctx: &CodegenContext,
    scope: Option<&str>,
    visibility: &str,
) -> Option<String> {
    use super::toplevel::{compute_resolved_rc_params, fn_name_to_variant};

    if group_fns.is_empty() {
        return None;
    }
    let enum_name = format!("__MutualTco{}", group_id);
    let trampoline_name = format!("__mutual_tco_trampoline_{}", group_id);
    let ret_type = super::types::type_to_rust_scoped(&group_fns[0].return_type, ctx, scope);

    let member_fn_ids: HashSet<crate::ir::FnId> = mir_fns.iter().map(|m| m.fn_id).collect();
    let rc_indices = compute_resolved_rc_params(group_fns);
    let rc_names: HashSet<String> = rc_indices
        .iter()
        .filter_map(|&i| group_fns[0].params.get(i).map(|(name, _)| name.clone()))
        .collect();

    // Render every member's trampoline-arm body FIRST — bail before
    // emitting anything if a member can't render (all-or-nothing block).
    let mut arm_bodies: Vec<String> = Vec::with_capacity(group_fns.len());
    for (i, mir_fn) in mir_fns.iter().enumerate() {
        // Trampoline arm policy: no borrow-by-default, rc params wrapped.
        // NB: own_param graduation is deliberately NOT applied to the
        // mutual-TCO path — graduating a collection param to owned here
        // would require coordinating the trampoline enum payload type, the
        // wrapper signatures, and the arg passing across every member, a
        // far larger and riskier change for no measured win (the perf
        // flagship `vector_ops` is self-TCO, handled in `emit_mir_tco_fn`).
        // Keeping borrow-by-default is always sound — not graduating never
        // skips a clone.
        let mut policy = MirFnEmitPolicy::from_resolved(group_fns[i], scope, false);
        policy.rc_wrapped = rc_names.clone();
        // The trampoline signature stays boxed, while the rewritten member
        // body may contain raw compiler-owned character locals. Use the MIR's
        // explicit restricted tags rather than the pre-rewrite whole-function
        // facts so match subjects and their literals agree on `i64`.
        policy.apply_rewritten_bare_i64(mir_fn, ctx);
        let mut arm_ctx = MirEmitCtx::for_fn(ctx, &policy);
        // Mutual invariants are `rc_wrapped` for owning reads, but unlike
        // self-TCO's `Arc<T>` representation they are extra `&T` trampoline
        // parameters. Tell equality's operand-shape classifier about that
        // representation; no general clone policy changes.
        arm_ctx.rc_wrapped_are_borrowed_refs = true;
        let body = emit_mir_trampoline_body(
            &mir_fn.body,
            &member_fn_ids,
            &enum_name,
            &rc_names,
            &arm_ctx,
        )?;
        arm_bodies.push(body);
    }

    let mut sections = Vec::new();

    // 1. Enum — one variant per member, payload = non-rc param types.
    let mut enum_lines = Vec::new();
    enum_lines.push("#[allow(non_camel_case_types)]".to_string());
    enum_lines.push(format!("enum {} {{", enum_name));
    for fd in group_fns {
        let variant = fn_name_to_variant(&fd.name);
        let param_types: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(_, ty)| super::types::type_to_rust_scoped(ty, ctx, scope))
            .collect();
        if param_types.is_empty() {
            enum_lines.push(format!("    {},", variant));
        } else {
            enum_lines.push(format!("    {}({}),", variant, param_types.join(", ")));
        }
    }
    enum_lines.push("}".to_string());
    sections.push(enum_lines.join("\n"));

    // 2. Trampoline fn — rc params are extra `&T` args.
    let rc_extra_params: String = mutual_rc_param_sig(group_fns[0], &rc_names, ctx, scope);
    let mut tramp_lines = Vec::new();
    tramp_lines.push(format!(
        "fn {}(mut __state: {}{}) -> {} {{",
        trampoline_name, enum_name, rc_extra_params, ret_type
    ));
    tramp_lines.push("    loop {".to_string());
    tramp_lines.push("        __state = match __state {".to_string());
    for (fd, arm_body) in group_fns.iter().zip(&arm_bodies) {
        let variant = fn_name_to_variant(&fd.name);
        let param_bindings: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(name, _)| explicit_parameter_pattern(name, true))
            .collect();
        let binding = if param_bindings.is_empty() {
            format!("{}::{}", enum_name, variant)
        } else {
            format!("{}::{}({})", enum_name, variant, param_bindings.join(", "))
        };
        tramp_lines.push(format!("            {} => {{", binding));
        tramp_lines.push(arm_body.clone());
        tramp_lines.push("            }".to_string());
    }
    tramp_lines.push("        };".to_string());
    tramp_lines.push("    }".to_string());
    tramp_lines.push("}".to_string());
    sections.push(tramp_lines.join("\n"));

    // 3. Wrapper fns — borrow-by-default params, clone borrowed into the
    //    enum variant, pass rc params as `&T` extra trampoline args.
    for fd in group_fns {
        let fn_name = aver_name_to_rust(&fd.name);
        let variant = fn_name_to_variant(&fd.name);
        let params = emit_resolved_fn_params(&fd.params, ctx, scope);
        let variant_arg_names: Vec<String> = fd
            .params
            .iter()
            .filter(|(name, _)| !rc_names.contains(name))
            .map(|(name, ty)| {
                let rust_name = aver_name_to_rust(name);
                if should_borrow_param(ty) {
                    format!("{}.clone()", rust_name)
                } else {
                    rust_name
                }
            })
            .collect();
        let variant_call = if variant_arg_names.is_empty() {
            format!("{}::{}", enum_name, variant)
        } else {
            format!(
                "{}::{}({})",
                enum_name,
                variant,
                variant_arg_names.join(", ")
            )
        };
        let rc_extra_args: String = {
            let parts: Vec<String> = fd
                .params
                .iter()
                .filter(|(name, _)| rc_names.contains(name))
                .map(|(name, _)| format!("&{}", aver_name_to_rust(name)))
                .collect();
            if parts.is_empty() {
                String::new()
            } else {
                format!(", {}", parts.join(", "))
            }
        };
        let mut wrapper = Vec::new();
        if let Some(desc) = &fd.desc {
            wrapper.push(format!("/// {}", desc));
        }
        wrapper.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        wrapper.push(format!(
            "    {}({}{})",
            trampoline_name, variant_call, rc_extra_args
        ));
        wrapper.push("}".to_string());
        sections.push(wrapper.join("\n"));
    }

    Some(sections.join("\n\n"))
}

/// Build the rc-param extra `&T` argument list for the mutual
/// trampoline signature (`, x: &T, y: &U`), or empty when no rc params.
fn mutual_rc_param_sig(
    fd: &crate::ir::hir::ResolvedFnDef,
    rc_names: &HashSet<String>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    if rc_names.is_empty() {
        return String::new();
    }
    let parts: Vec<String> = fd
        .params
        .iter()
        .filter(|(name, _)| rc_names.contains(name))
        .map(|(name, ty)| {
            format!(
                "{}: &{}",
                explicit_parameter_pattern(name, false),
                super::types::type_to_rust_scoped(ty, ctx, scope)
            )
        })
        .collect();
    if parts.is_empty() {
        String::new()
    } else {
        format!(", {}", parts.join(", "))
    }
}

fn emit_resolved_fn_params(
    params: &[(String, crate::types::Type)],
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    params
        .iter()
        .map(|(name, ty)| {
            let rust_type = super::types::type_to_rust_scoped(ty, ctx, scope);
            if should_borrow_param(ty) {
                format!("{}: &{rust_type}", explicit_parameter_pattern(name, false))
            } else {
                format!("{}: {rust_type}", explicit_parameter_pattern(name, false))
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Emit one trampoline arm body from MIR: leads with
/// `cancel_checkpoint();`, walks the leading `Let` chain as plain `let`
/// statements, then renders the final expression in trampoline tail
/// position (member-`TailCall` → enum variant bounce, value → `return`).
fn emit_mir_trampoline_body(
    body: &Spanned<MirExpr>,
    members: &HashSet<crate::ir::FnId>,
    enum_name: &str,
    rc_names: &HashSet<String>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut lines = Vec::new();
    lines.push("                crate::cancel_checkpoint();".to_string());

    let mut current = body;
    while let MirExpr::Let(spanned_let) = &current.node {
        let let_node = &spanned_let.node;
        let value = emit_mir_expr(&let_node.value, ctx)?;
        if let_node.binding_name.is_empty() {
            // Discarded intermediate (`Stmt::Expr` / `_ = effect()`)
            // — bare statement, result dropped.
            lines.push(format!("                {};", value));
        } else {
            let name = explicit_binding_pattern(&let_node.binding_name);
            lines.push(format!("                let {} = {};", name, value));
        }
        current = &let_node.body;
    }

    let tail = emit_mir_trampoline_tail_expr(current, members, enum_name, rc_names, ctx)?;
    lines.push(format!("                {}", tail));
    Some(lines.join("\n"))
}

/// Render a MIR expression in trampoline tail position. A `TailCall` to
/// a group member becomes an enum-variant bounce (excluding rc args); a
/// `TailCall` to a non-member, or any base-case value, becomes a
/// `return`. `Match` / `IfThenElse` recurse (still tail position).
fn emit_mir_trampoline_tail_expr(
    expr: &Spanned<MirExpr>,
    members: &HashSet<crate::ir::FnId>,
    enum_name: &str,
    rc_names: &HashSet<String>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match &expr.node {
        MirExpr::TailCall(spanned_tc) => {
            let tc = &spanned_tc.node;
            if members.contains(&tc.target) {
                // Bounce → enum variant for the TARGET member, excluding
                // its rc (pass-through) args. The target's param names
                // drive which positional args are rc — read them off the
                // target fn entry's source-level signature so the rc
                // filter matches the target, not the caller.
                let target_name = ctx.symbol_table.fn_entry(tc.target).key.name.clone();
                let variant = super::toplevel::fn_name_to_variant(&target_name);
                let mut arg_strs = Vec::new();
                for a in &tc.args {
                    // Skip rc args by the arg's source-level name: a
                    // pass-through arg is a bare local read whose name is
                    // in `rc_names` (shared across the SCC by name+type).
                    if let Some(local) = local_of(&a.node)
                        && rc_names.contains(&local.name)
                    {
                        continue;
                    }
                    arg_strs.push(mir_clone_arg(emit_mir_expr(a, ctx)?, &a.node, ctx));
                }
                if arg_strs.is_empty() {
                    Some(format!("{}::{}", enum_name, variant))
                } else {
                    Some(format!(
                        "{}::{}({})",
                        enum_name,
                        variant,
                        arg_strs.join(", ")
                    ))
                }
            } else {
                let name = ctx.symbol_table.fn_entry(tc.target).key.canonical();
                Some(format!("return {}", emit_named_call(&name, &tc.args, ctx)?))
            }
        }
        MirExpr::Match(spanned_match) => {
            emit_mir_match_with(&spanned_match.node, ctx, &|arm_body, ctx| {
                emit_mir_trampoline_tail_expr(arm_body, members, enum_name, rc_names, ctx)
            })
        }
        MirExpr::IfThenElse(spanned_ite) => {
            let (cond, then_src, else_src) = mir_if_cond_and_branches(&spanned_ite.node, ctx)?;
            let t = emit_mir_trampoline_tail_expr(then_src, members, enum_name, rc_names, ctx)?;
            let e = emit_mir_trampoline_tail_expr(else_src, members, enum_name, rc_names, ctx)?;
            Some(format!("if {} {{ {} }} else {{ {} }}", cond, t, e))
        }
        _ => Some(format!("return {}", emit_mir_value_return(expr, ctx)?)),
    }
}

// ── MIR-side borrow / clone machinery ───────────────────────────────────
//
// Mirror of the HIR walker's `expr_skip_clone` / `maybe_clone` /
// `clone_arg` / `borrow_arg` (emit_ctx.rs + expr.rs), keyed off
// `MirLocal` (slot + `last_use` + source `name`) instead of
// `ResolvedExpr::Resolved`. The covered arms route every arg /
// field / element / base through these so their output matches HIR
// byte-for-byte on the borrow decisions. When the walker has no
// `CodegenContext` (coverage path), the local-name lookups still
// work off the (empty) policy fields and degrade to the
// conservative `last_use ? move : clone` shape — which is fine
// because the coverage walk only inspects `Some` vs `None`.

/// `&MirExpr` reference to a source-named local, if any. Synthetic
/// locals (empty name) are excluded — the walker already bails on
/// them upstream.
fn local_of(expr: &MirExpr) -> Option<&MirLocal> {
    match expr {
        MirExpr::Local(l) if !l.node.name.is_empty() => Some(&l.node),
        _ => None,
    }
}

/// Whether a nested expression contains a final read of `slot`. Such reads
/// may be emitted in an owning position, so a method receiver using the same
/// local must be detached before evaluating that expression.
fn mir_expr_contains_last_use_of_slot(expr: &MirExpr, slot: LocalId) -> bool {
    if let MirExpr::Local(local) = expr {
        return local.node.slot == slot && local.node.last_use;
    }

    let mut found = false;
    crate::ir::mir::optimize::bare_i64::visit_children(expr, &mut |child| {
        if !found && mir_expr_contains_last_use_of_slot(child, slot) {
            found = true;
        }
    });
    found
}

/// Whether evaluating `expr` in an owning (`true`) or borrowing (`false`)
/// argument position may move the final read of `slot`. Named calls can be
/// inspected recursively through their callee borrow masks; unfamiliar
/// compound shapes stay conservative.
fn mir_expr_may_move_slot(
    expr: &MirExpr,
    slot: LocalId,
    owning_position: bool,
    ctx: &MirEmitCtx<'_>,
) -> bool {
    match expr {
        MirExpr::Local(local) => owning_position && local.node.slot == slot && local.node.last_use,
        MirExpr::Project(project) => {
            // A projection used as a named-call argument is either borrowed
            // in place or cloned by `mir_clone_arg`; neither moves its base.
            // Still inspect a compound base because evaluating it may contain
            // an owning nested call before the projection happens.
            mir_expr_may_move_slot(&project.node.base.node, slot, false, ctx)
        }
        MirExpr::Call(call) => {
            let MirCallee::Fn(fn_id) = &call.node.callee else {
                return mir_expr_contains_last_use_of_slot(expr, slot);
            };
            let Some(cg) = ctx.codegen else {
                return mir_expr_contains_last_use_of_slot(expr, slot);
            };
            let name = ctx.symbol_table.fn_entry(*fn_id).key.canonical();
            let nested_mask = callee_borrow_mask(&name, call.node.args.len(), cg);
            call.node.args.iter().enumerate().any(|(index, arg)| {
                mir_expr_may_move_slot(
                    &arg.node,
                    slot,
                    !nested_mask.get(index).copied().unwrap_or(false),
                    ctx,
                )
            })
        }
        MirExpr::TailCall(call) => {
            let Some(cg) = ctx.codegen else {
                return mir_expr_contains_last_use_of_slot(expr, slot);
            };
            let name = ctx.symbol_table.fn_entry(call.node.target).key.canonical();
            let nested_mask = callee_borrow_mask(&name, call.node.args.len(), cg);
            call.node.args.iter().enumerate().any(|(index, arg)| {
                mir_expr_may_move_slot(
                    &arg.node,
                    slot,
                    !nested_mask.get(index).copied().unwrap_or(false),
                    ctx,
                )
            })
        }
        _ => mir_expr_contains_last_use_of_slot(expr, slot),
    }
}

/// Whether borrowing call argument `arg_index` directly would overlap a move
/// from the same local while Rust evaluates a later argument. A direct later
/// argument whose callee position is also borrowed cannot move the local; for
/// a compound later expression, a final read may occur in an owning nested
/// position, so conservatively detach the earlier borrow.
fn mir_call_borrow_overlaps_later_move(
    arg_index: usize,
    args: &[Spanned<MirExpr>],
    borrow_mask: &[bool],
    ctx: &MirEmitCtx<'_>,
) -> bool {
    let Some(local) = args.get(arg_index).and_then(|arg| local_of(&arg.node)) else {
        return false;
    };
    let name = local.name.as_str();
    // None of these representations can be moved by a later owning use:
    // Copy values copy, borrowed / pass-through params clone their referent,
    // and a loop-carried param retains a hidden next-iteration use that makes
    // every visible owning position clone as well.
    if ctx.is_copy(name)
        || ctx.is_borrowed_param(name)
        || ctx.is_rc_wrapped(name)
        || ctx.is_loop_carried_param(name)
    {
        return false;
    }

    args.iter()
        .enumerate()
        .skip(arg_index + 1)
        .any(|(later_index, later)| {
            mir_expr_may_move_slot(
                &later.node,
                local.slot,
                !borrow_mask.get(later_index).copied().unwrap_or(false),
                ctx,
            )
        })
}

/// Should `.clone()` be skipped for this MIR expr? Mirror of HIR's
/// `expr_skip_clone`. A local read skips clone on its last use or
/// when Copy; `rc_wrapped` / `borrowed_params` never skip (they
/// need the special clone paths in `mir_maybe_clone`). A name that
/// isn't a known local is treated as a global / namespace and
/// always skips. Non-locals (literals, nested exprs) never need a
/// clone wrapper here.
fn mir_expr_skip_clone(expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> bool {
    match local_of(expr) {
        Some(local) => {
            let name = local.name.as_str();
            if ctx.is_rc_wrapped(name) || ctx.is_borrowed_param(name) {
                return false;
            }
            // A self-TCO identity arg is absent from the emitted Rust, so
            // MIR's syntactic `last_use` does not include the next loop
            // iteration. Preserve that hidden use only at owning positions;
            // bare i64 and genuinely Copy params still need no clone.
            if ctx.is_loop_carried_param(name)
                && !ctx.is_copy(name)
                && !ctx.bare.is_bare(local.slot)
            {
                return false;
            }
            local.last_use || ctx.is_copy(name)
        }
        None => true,
    }
}

/// Mirror of HIR's `maybe_clone`: wrap a local read in the right
/// clone shape for an owning position (arg, return, ctor field,
/// tuple / list / map element). `code` is the already-emitted
/// expression text for `expr`.
fn mir_maybe_clone(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    if let Some(local) = local_of(expr) {
        let name = local.name.as_str();
        return if mir_expr_skip_clone(expr, ctx) {
            code
        } else if ctx.is_rc_wrapped(name) {
            // Pass-through param (Rc<T> / &T): deref then clone.
            format!("(*{}).clone()", code)
        } else {
            // Borrowed param or plain owned local: clone to own.
            format!("{}.clone()", code)
        };
    }
    // Field access (`Project`): emit_mir_expr produces `base.field`
    // without clone; clone here for ownership. Matches HIR's
    // `maybe_clone` `Attr` arm — builtin namespace access never
    // reaches the MIR walker (it lowers to a `Call`), so no
    // namespace special-case is needed.
    if matches!(expr, MirExpr::Project(_)) {
        return format!("{}.clone()", code);
    }
    code
}

/// Mirror of HIR's `clone_arg` (`clone_arg_with_options`): emit an
/// expression as an owning argument. HIR elides the `.clone()` on a
/// record field access whose field type is Copy
/// (`attr_result_is_copy`); Wave 4 ports that elision here via
/// [`mir_attr_result_is_copy`], reading the base local's stamped type.
/// For the common case (non-`Project` args) this delegates to
/// `mir_maybe_clone`, matching HIR exactly.
fn mir_clone_arg(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    if let MirExpr::Project(p) = expr
        && mir_attr_result_is_copy(&p.node, ctx)
    {
        // Copy-typed record field: HIR returns the bare field access
        // (no `.clone()`). Mirror that.
        return code;
    }
    mir_maybe_clone(code, expr, ctx)
}

/// Mirror of HIR's `attr_result_is_copy` over a `MirProject`: the
/// field access result is Copy iff the projection base is a
/// `Type::Named` local and the projected field's declared type is a
/// Copy type. Reads the base's type from `local_types` (params + let
/// bindings — the MIR walker has richer coverage than HIR here, but the
/// guard `obj is a Named local` is the same), then defers to the shared
/// `record_field_is_copy` for the field-type lookup. Returns `false`
/// (HIR's conservative "needs a clone") when there's no `CodegenContext`
/// (coverage path) or the base isn't a Named local.
fn mir_attr_result_is_copy(proj: &crate::ir::mir::MirProject, ctx: &MirEmitCtx<'_>) -> bool {
    let Some(cg) = ctx.codegen else {
        return false;
    };
    let Some(local) = local_of(&proj.base.node) else {
        return false;
    };
    let Some(named_ty) = ctx
        .local_types
        .get(&local.name)
        .filter(|t| matches!(t, Type::Named { .. }))
    else {
        return false;
    };
    super::expr::record_field_is_copy(named_ty, &proj.field, cg)
}

/// Emit a named user-function call (`Call(Fn)` /
/// outside-loop `TailCall`). Mirror of HIR's
/// `emit_named_function_call`: per-arg `borrow_arg` (when the
/// callee's i-th param is borrowed-by-default `&T`) or `clone_arg`
/// (owned), and `resolve_module_call` head path-mangling.
///
/// `callee_borrow_mask` needs the full `CodegenContext`; on the
/// coverage path (`codegen == None`) there's no mask, so every arg
/// rides `clone_arg` (conservative — coverage only reads Some/None,
/// and the production parity gate never runs without a ctx).
fn emit_named_call(name: &str, args: &[Spanned<MirExpr>], ctx: &MirEmitCtx<'_>) -> Option<String> {
    emit_named_call_to(name, None, args, ctx)
}

/// Like [`emit_named_call`] but threads the callee's `FnId` so the Int
/// unboxing facts can convert each arg at the call boundary: when the
/// callee's i-th param is bare `i64`, the arg is emitted as a raw `i64`
/// (an already-bare arg directly; a boxed `AverInt` arg narrowed via a
/// checked `to_i64`). A `None` `callee` (a foreign tail-call helper with
/// no facts) keeps every arg boxed.
fn emit_named_call_to(
    name: &str,
    callee: Option<crate::ir::FnId>,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let borrow_mask = match ctx.codegen {
        Some(cg) => callee_borrow_mask(name, args.len(), cg),
        None => vec![false; args.len()],
    };
    // ETAP-2 SLICE 1: the call-arg representation BOUNDARY (the `from_i64` /
    // `to_i64` CONVERSION for a value whose representation differs from the
    // param's) is now an EXPLICIT `Box`/`Unbox` node the `bare_i64_rewrite`
    // pass inserted, lowered by `emit_mir_expr`. Codegen no longer decides
    // those conversions here. It still RENDERS a bare-param arg raw — that
    // is representation, not a boundary: a literal / bare leaf / bare
    // arithmetic tree at a `i64` param emits its native `i64` form
    // (`emit_bare_i64`), and an `Unbox`-wrapped boxed arg falls through to
    // `emit_mir_expr` (which lowers the `Unbox`).
    let callee_bare = callee.and_then(|id| ctx.codegen.and_then(|cg| cg.bare_i64.for_fn(id)));
    let mut arg_strs = Vec::with_capacity(args.len());
    for (i, a) in args.iter().enumerate() {
        if callee_bare.is_some_and(|f| f.param_is_bare(i)) {
            // Bare `i64` param: render the arg in its raw form. A raw leaf /
            // compound / literal renders via `emit_bare_i64`; anything else
            // (an `Unbox` node the rewrite inserted for a genuinely-boxed
            // arg) falls through to `emit_mir_expr`.
            let rendered = emit_bare_i64(a).or_else(|| emit_mir_expr(a, ctx));
            arg_strs.push(rendered?);
            continue;
        }
        let code = emit_mir_expr(a, ctx)?;
        let s = if borrow_mask.get(i).copied().unwrap_or(false) {
            // Rust keeps this borrow live through evaluation of every later
            // argument. Detach it when a later nested call consumes the same
            // local (for example `f(x, g(x))` where `f` borrows and `g` owns),
            // otherwise rustc rejects the valid Aver program with E0505.
            let code = if mir_call_borrow_overlaps_later_move(i, args, &borrow_mask, ctx) {
                mir_maybe_clone(code, &a.node, ctx)
            } else {
                code
            };
            mir_borrow_arg(code, &a.node, ctx)
        } else {
            mir_clone_arg(code, &a.node, ctx)
        };
        arg_strs.push(s);
    }
    if let Some((prefix, suffix)) = resolve_module_call(name, ctx.module_prefixes) {
        Some(format!(
            "{}::{}({})",
            module_prefix_to_rust_path(prefix),
            aver_name_to_rust(suffix),
            arg_strs.join(", ")
        ))
    } else {
        Some(format!(
            "{}({})",
            aver_name_to_rust(name),
            arg_strs.join(", ")
        ))
    }
}

/// Mirror of HIR's `borrow_arg`: emit an expression for passing to
/// a user fn whose param is `&T`. `code` is the already-emitted
/// text for `expr`.
fn mir_borrow_arg(code: String, expr: &MirExpr, ctx: &MirEmitCtx<'_>) -> String {
    let Some(local) = local_of(expr) else {
        // Complex expression: borrow the temporary.
        return format!("&{}", code);
    };
    let name = local.name.as_str();
    if ctx.is_copy(name) {
        // Copy type: by value.
        code
    } else if matches!(ctx.local_types.get(name), Some(Type::Str)) {
        // AverStr (Rc<str>): by value; last-use moves, else clone.
        if local.last_use {
            code
        } else if ctx.is_rc_wrapped(name) {
            format!("(*{}).clone()", code)
        } else {
            format!("{}.clone()", code)
        }
    } else if ctx.is_borrowed_param(name) {
        // Already `&T` — pass directly.
        code
    } else if ctx.is_rc_wrapped(name) {
        // Pass-through TCO param: deref to `&T`.
        format!("&*{}", code)
    } else {
        // Owned local: borrow it (last-use and non-last-use both
        // emit `&code` in the HIR walker).
        format!("&{}", code)
    }
}

// ── Wave 3a: PURE builtin calls + deforestation intrinsics ──────────────
//
// Mirror of the HIR oracle `emit_builtin_call` / `emit_builtin_call_inner`
// (`builtins.rs`) for the ~88 PURE builtins (Result / Option / Int /
// Float / String / List / Map / Vector / Bool / Char). The
// EFFECTFUL families (Args / Console / Http / Disk / Env /
// Random / Tcp / Terminal / Time) are split off at the
// `Call(Builtin)` arm to `emit_mir_effectful_builtin_call` (Wave 3b,
// below) — they are NOT handled here.
//
// Each arm copies its HIR sibling's shape verbatim, substituting:
//   `emit_arg(i)`                  → `emit_mir_expr(&args[i], ctx)?`
//   `clone_arg(&args[i].node, …)`  → `mir_clone_arg(emit_mir_expr(…)?, …)`
//   `emit_str_arg_or_deref(…)`     → `mir_str_arg_or_deref(&args[i], ctx)?`
// then runs the `builtin_needs_str_conversion` `.into_aver()` post-step
// that `emit_builtin_call` applies (Int.mod, Int/Float.fromString,
// String.* returning String). The byte-parity
// gate is the safety net: any arm whose output diverges from HIR blocks
// graduation and the fn falls back to HIR.

/// Mirror of HIR's `emit_str_arg_or_deref`: emit a string-accepting
/// argument (`String.contains` / `startsWith` / `endsWith`) as a bare
/// `"foo"` literal (no allocation) or, for any other expression, the
/// deref form `&*code`. Returns `None` when the inner expr can't emit.
fn mir_str_arg_or_deref(expr: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    if let MirExpr::Literal(lit) = &expr.node
        && let crate::ast::Literal::Str(s) = &lit.node
    {
        return Some(format!("{:?}", s));
    }
    let code = emit_mir_expr(expr, ctx)?;
    Some(format!("&*{}", code))
}

/// Resolve a nested expression that is itself a `Call(Builtin(id))` to
/// its canonical dotted name + arg slice. MIR lowering wipes the
/// syntactic shape the HIR `ResolvedLeafOp` classifiers key off
/// (`Option.withDefault` / `Result.withDefault` / `Vector.get` over a
/// nested builtin), so the fusion recognizers
/// ([`try_emit_mir_fusion`]) re-match the pattern over this resolved
/// `(name, args)` form instead. Returns `None` for any non-`Call`, a
/// non-`Builtin` callee, or an out-of-range / unresolved `BuiltinId`
/// (the same defensive fallthrough the `Call(Builtin)` arm takes).
fn mir_builtin_call_parts<'a, 'c>(
    expr: &'a MirExpr,
    ctx: &MirEmitCtx<'c>,
) -> Option<(&'c str, &'a [Spanned<MirExpr>])> {
    let MirExpr::Call(spanned_call) = expr else {
        return None;
    };
    let call = &spanned_call.node;
    let MirCallee::Builtin(id) = &call.callee else {
        return None;
    };
    let name = ctx.mir_builtins.get(id.0 as usize)?.as_str();
    Some((name, &call.args))
}

/// Two MIR exprs that name the SAME source local. Used by the
/// `VectorSetOrDefaultSameVector` fusion's same-vector guard (HIR's
/// `default_expr.node != inner_args[0].node` check). Compares by slot,
/// not the whole `MirLocal`, because the two reads can carry different
/// `last_use` flags (the outer default read is typically the last use
/// of the slot, the inner `Vector.set` read is not) yet still denote
/// the same vector. Synthetic / unnamed locals never match.
fn mir_same_local(a: &MirExpr, b: &MirExpr) -> bool {
    match (local_of(a), local_of(b)) {
        (Some(la), Some(lb)) => la.slot == lb.slot,
        _ => false,
    }
}

/// Re-recognize the three codegen FUSIONS the HIR walker performs over
/// pre-lowering `ResolvedLeafOp` shapes but MIR lowering flattens into
/// nested builtin `Call`s. The HIR classifiers
/// (`classify_vector_set_or_default` / `classify_int_mod_or_default` /
/// `classify_list_index_get` in `ir::hir::classify`) match the
/// syntactic AST; here we re-match the equivalent `MirExpr::Call`
/// nesting and emit the EXACT fused Rust form the HIR `ResolvedLeafOp`
/// emitter (`emit_leaf_op_with_options`, `expr.rs`) produces, so the
/// byte-parity gate graduates these fns instead of falling back to the
/// (un-fused, slower) generic builtin emit. Returns `None` when the
/// outer call isn't one of the three fusion heads or the nested shape
/// doesn't match — the caller then emits the generic builtin form.
fn try_emit_mir_fusion(
    name: &str,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match name {
        // Fusion #1: `Option.withDefault(Vector.set(v, i, x), v)` where
        // both `v` are the SAME local → in-place bounds-checked set.
        // HIR: `ResolvedLeafOp::VectorSetOrDefaultSameVector`.
        "Option.withDefault" if args.len() == 2 => {
            let (inner_name, inner_args) = mir_builtin_call_parts(&args[0].node, ctx)?;
            if inner_name != "Vector.set" || inner_args.len() != 3 {
                return None;
            }
            // Same-vector guard: the default arm (`args[1]`) must be the
            // same local as the vector being set (`inner_args[0]`).
            if !mir_same_local(&args[1].node, &inner_args[0].node) {
                return None;
            }
            // own_param self-keep collapse (the perf flagship): when the
            // set-target is an OWNED collection param (its `aliased_slots`
            // bit was cleared by `own_param` → in `ctx.owned_params`) and
            // the slot is dead after this fusion, MOVE it into `__vec`
            // instead of cloning. The fusion consumes the slot exactly
            // once (it returns either the mutated handle or the same
            // handle), so liveness is the OR of the two `v` occurrences'
            // `last_use` bits — the inner `Vector.set` read carries
            // `last_use=false` (the textually-last read is the default
            // arm), so without the OR we would wrongly clone and the
            // refcount-2 `Rc::make_mut` would deep-copy every iteration
            // (the O(n²) the VM/own_param fix already eliminated). This is
            // the exact mirror of own_param's `Option.withDefault` self-
            // keep shape + the VM fusion-collapse in `vm/compiler/mir.rs`.
            let set_local = local_of(&inner_args[0].node);
            let default_local = local_of(&args[1].node);
            let move_vec = match (set_local, default_local) {
                (Some(sv), Some(dv)) => {
                    ctx.owned_params.contains(sv.name.as_str())
                        && !ctx.is_borrowed_param(&sv.name)
                        && !ctx.is_rc_wrapped(&sv.name)
                        && (sv.last_use || dv.last_use)
                }
                _ => false,
            };
            let vector = if move_vec {
                // Owned + dead-after: move (no `.clone()`) → in-place
                // `set_unchecked` on a refcount-1 `Rc`.
                emit_mir_expr(&inner_args[0], ctx)?
            } else {
                // HIR: vector via `clone_arg` (borrowed / not-proven-owned).
                mir_clone_arg(
                    emit_mir_expr(&inner_args[0], ctx)?,
                    &inner_args[0].node,
                    ctx,
                )
            };
            let index = emit_mir_expr(&inner_args[1], ctx)?;
            let value = mir_clone_arg(
                emit_mir_expr(&inner_args[2], ctx)?,
                &inner_args[2].node,
                ctx,
            );
            Some(format!(
                "{{ let __vec = {}; match ({}).to_usize() {{ Some(__idx) if __idx < __vec.len() => __vec.set_unchecked(__idx, {}), _ => __vec }} }}",
                vector, index, value
            ))
        }
        // Fusion #2: `Result.withDefault(Int.mod(a, b), default)` and the
        // parallel `Result.withDefault(Int.div(a, b), default)` → skip the
        // `Result` allocation. HIR:
        // `ResolvedLeafOp::IntModOrDefaultLiteral` /
        // `ResolvedLeafOp::IntDivOrDefaultLiteral`.
        "Result.withDefault" if args.len() == 2 => {
            let (inner_name, inner_args) = mir_builtin_call_parts(&args[0].node, ctx)?;
            // `Int.mod` fuses to `rem_euclid`; `Int.div` to `div_euclid`
            // (both Euclidean — see the emission below).
            let op = match inner_name {
                "Int.mod" => "rem_euclid",
                "Int.div" => "div",
                _ => return None,
            };
            if inner_args.len() != 2 {
                return None;
            }
            // The default arm must be a literal (HIR's
            // `classify_int_mod_or_default` requires a literal default).
            let MirExpr::Literal(default_lit) = &args[1].node else {
                return None;
            };
            let a = &inner_args[0];
            let b = &inner_args[1];
            let a_str = emit_mir_expr(a, ctx)?;
            let default = emit_literal(&default_lit.node);
            match op {
                // Euclidean division (partner of Euclidean `Int.mod`).
                // `AverInt::div_euclid` is `None` ONLY on a zero divisor
                // (the `i64::MIN / -1` edge promotes to `Big` over ℤ), so
                // `.unwrap_or(default)` yields the default exactly when the
                // divisor is zero — matching the VM.
                "div" => {
                    let b_str = emit_mir_expr(b, ctx)?;
                    Some(format!(
                        "({}).div_euclid(&({})).unwrap_or({})",
                        a_str, b_str, default
                    ))
                }
                // `rem_euclid` is total except on a zero divisor, so a
                // non-zero literal divisor can skip the runtime zero check —
                // the `.unwrap()` is total there (divisor proven non-zero).
                _ => {
                    if let MirExpr::Literal(b_lit) = &b.node
                        && let crate::ast::Literal::Int(n) = &b_lit.node
                        && *n != 0
                    {
                        let b_str = emit_literal(&crate::ast::Literal::Int(*n));
                        Some(format!("({}).rem_euclid(&({})).unwrap()", a_str, b_str))
                    } else {
                        let b_str = emit_mir_expr(b, ctx)?;
                        Some(format!(
                            "{{ let __b = {}; if __b.is_zero() {{ {} }} else {{ ({}).rem_euclid(&__b).unwrap() }} }}",
                            b_str, default, a_str
                        ))
                    }
                }
            }
        }
        // Fusion #3: `Vector.get(Vector.fromList(list), index)` → index
        // the materialized `Vec` directly, skipping the intermediate
        // `AverVector::from_vec` (an extra `Rc::new`). HIR:
        // `ResolvedLeafOp::ListIndexGet`.
        "Vector.get" if args.len() == 2 => {
            let (inner_name, inner_args) = mir_builtin_call_parts(&args[0].node, ctx)?;
            if inner_name != "Vector.fromList" || inner_args.len() != 1 {
                return None;
            }
            let list = emit_mir_expr(&inner_args[0], ctx)?;
            let index = emit_mir_expr(&args[1], ctx)?;
            // Index lookup: out-of-`usize` index → `None` (matches the
            // unfused `Vector.get`).
            Some(format!(
                "({}).to_usize().and_then(|__i| {}.to_vec().get(__i).cloned())",
                index, list
            ))
        }
        _ => None,
    }
}

/// Emit a PURE builtin call from MIR, byte-identical to the HIR
/// oracle's `emit_builtin_call` (minus the effectful / replay / policy
/// branches, which never reach here). Returns `None` for any builtin
/// the oracle doesn't cover here (→ HIR fallback). `name` is already
/// known non-effectful (the `Call(Builtin)` arm gated it).
fn emit_mir_builtin_call(
    name: &str,
    args: &[Spanned<MirExpr>],
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    // FUSIONS first: the HIR walker recognizes these
    // `Option.withDefault` / `Result.withDefault` / `Vector.get` over a
    // nested builtin shapes PRE-lowering and emits a fused form. MIR
    // lowering flattens the shape, so re-recognize it here before the
    // generic per-builtin arms below produce the un-fused (slower)
    // output. Anything that doesn't match falls through to the generic
    // emit, byte-identical to HIR's non-fused path.
    if let Some(fused) = try_emit_mir_fusion(name, args, ctx) {
        return Some(fused);
    }

    // `emit_arg(i)`: raw emit (HIR's `emit_expr(&args[i].node, …)`).
    macro_rules! arg {
        ($i:expr) => {
            emit_mir_expr(&args[$i], ctx)?
        };
    }
    // `clone_arg(&args[i].node, …)`: owning clone.
    macro_rules! clone {
        ($i:expr) => {
            mir_clone_arg(emit_mir_expr(&args[$i], ctx)?, &args[$i].node, ctx)
        };
    }

    let result = match name {
        // ---- Result ----
        "Result.Ok" => format!("Ok({})", clone!(0)),
        "Result.Err" => format!("Err({})", clone!(0)),
        "Result.withDefault" => format!("{}.unwrap_or({})", clone!(0), clone!(1)),

        // ---- Option ----
        "Option.Some" => format!("Some({})", clone!(0)),
        "Option.withDefault" => format!("{}.unwrap_or({})", clone!(0), clone!(1)),
        "Result.fromOption" => format!("{}.ok_or({})", clone!(0), clone!(1)),

        // ---- Int ----
        // `AverInt::abs` promotes `|i64::MIN|` to `Big` (no wrap/panic).
        "Int.abs" => format!("{}.abs()", arg!(0)),
        // Float→Int truncation MUST funnel through `from_f64_trunc` (NOT
        // `from_i64(f as i64)`, which saturates a huge finite float to
        // `i64::MAX`). Mirrors the VM's `float_to_aver_int`.
        "Int.fromFloat" => format!("aver_rt::AverInt::from_f64_trunc({})", arg!(0)),
        "Int.fromString" => {
            // Match the VM's Err message BYTE-FOR-BYTE: `Int.fromString`
            // in `src/types/int.rs` returns `Cannot parse '{input}' as
            // Int`, not rustc's native `parse` error ("invalid digit
            // found in string"). A program that reads the `Result.Err`
            // string (and verify cases asserting it) must see identical
            // bytes on rust and the VM. Bind a *reference* to the input
            // (parse + the message both borrow), so a non-trivial arg
            // expr is evaluated once and the original owned value stays
            // available to surrounding code. `AverInt::from_str` parses
            // arbitrary-length integers (the no-wrap guarantee).
            let s = arg!(0);
            format!(
                "{{ let __s = &({s}); __s.parse::<aver_rt::AverInt>().map_err(|_| format!(\"Cannot parse '{{}}' as Int\", __s)) }}"
            )
        }
        // `AverInt` has no by-value `Ord::min`/`max`; use the borrowing
        // `min_ref`/`max_ref` (which keep the small-int clone cheap).
        "Int.min" => format!("{}.min_ref(&{})", arg!(0), arg!(1)),
        "Int.max" => format!("{}.max_ref(&{})", arg!(0), arg!(1)),
        "Int.mod" => {
            let a = arg!(0);
            let b = arg!(1);
            // Euclidean remainder over ℤ. `rem_euclid` returns `None` only
            // on a zero divisor; the error string is verbatim from the VM
            // (`src/types/int.rs`) so the boxed `Result.Err` is
            // byte-identical across backends.
            format!(
                "match ({a}).rem_euclid(&({b})) {{ Some(__r) => Ok(__r), None => Err(\"division by zero\".to_string()) }}"
            )
        }
        "Int.div" => {
            let a = arg!(0);
            let b = arg!(1);
            // Euclidean division over ℤ (partner of Euclidean `Int.mod`).
            // `div_euclid` is `None` ONLY for a zero divisor — the
            // `i64::MIN / -1` "overflow" edge promotes to `Big` (it is just
            // `i64::MAX + 1`), so the VM's old "division overflow" branch is
            // DEAD here (VM agrees: both are ℤ now). Keep the
            // "division by zero" string byte-identical to the VM.
            format!(
                "match ({a}).div_euclid(&({b})) {{ Some(__q) => Ok(__q), None => Err(\"division by zero\".to_string()) }}"
            )
        }

        // ---- Float ----
        "Float.abs" => format!("{}.abs()", arg!(0)),
        // The VM applies `float_to_aver_int(f.round())` etc. — round/floor/
        // ceil the f64, then truncate into ℤ via `from_f64_trunc` (NOT
        // `as i64`, which saturates huge finite floats to `i64::MAX`).
        "Float.round" => format!("aver_rt::AverInt::from_f64_trunc({}.round())", arg!(0)),
        "Float.floor" => format!("aver_rt::AverInt::from_f64_trunc({}.floor())", arg!(0)),
        "Float.ceil" => format!("aver_rt::AverInt::from_f64_trunc({}.ceil())", arg!(0)),
        "Float.fromString" => {
            // Match the VM's Err message BYTE-FOR-BYTE: `Float.fromString`
            // in `src/types/float.rs` returns `Cannot parse '{input}' as
            // Float`, not rustc's native `parse` error.
            let s = arg!(0);
            format!(
                "{{ let __s = &({s}); __s.parse::<f64>().map_err(|_| format!(\"Cannot parse '{{}}' as Float\", __s)) }}"
            )
        }
        "Float.sqrt" => format!("{}.sqrt()", arg!(0)),
        "Float.pow" => format!("{}.powf({})", arg!(0), arg!(1)),
        "Float.min" => format!("{}.min({})", arg!(0), arg!(1)),
        "Float.max" => format!("{}.max({})", arg!(0), arg!(1)),
        "Float.sin" => format!("{}.sin()", arg!(0)),
        "Float.cos" => format!("{}.cos()", arg!(0)),
        "Float.atan2" => format!("{}.atan2({})", arg!(0), arg!(1)),
        "Float.pi" => "std::f64::consts::PI".to_string(),
        // The arg is now `AverInt`; `to_f64` saturates huge magnitudes to
        // `±∞` (never `NaN`), mirroring the VM's `AverInt::to_f64`.
        "Float.fromInt" => format!("{}.to_f64()", arg!(0)),

        // ---- String ----
        // `AverInt: Display` renders Small and Big byte-identically to the
        // VM's `Value::Int` formatting.
        "String.fromInt" => format!("{}.to_string()", arg!(0)),
        "String.fromFloat" => format!("{}.to_string()", arg!(0)),
        "String.fromBool" => format!("{}.to_string()", arg!(0)),
        "String.charAt" => {
            let s = arg!(0);
            let idx = arg!(1);
            // Index lookup: an out-of-`usize` index is out of range → `None`
            // (charAt already returns `Option<String>`), never a wrapped
            // index. `to_usize()` is the checked conversion.
            format!(
                "({}).to_usize().and_then(|__i| {}.chars().nth(__i).map(|c| c.to_string()))",
                idx, s
            )
        }
        // Producer: wrap the `usize` length in `AverInt`.
        "String.len" => format!(
            "aver_rt::AverInt::from_i64({}.chars().count() as i64)",
            arg!(0)
        ),
        "String.slice" => {
            let s = arg!(0);
            let from = arg!(1);
            let to = arg!(2);
            // `string_slice` clamps internally (`from.max(0)`, end past the
            // length saturates) and takes `i64` bounds. A Big bound is out of
            // any string's range, so saturate it sign-aware to `i64::MIN`/
            // `i64::MAX` (`aver_int_clamp_i64`): a huge positive clamps to the
            // string end, a huge negative clamps to 0 — behaviorally identical
            // to the VM.
            format!(
                "aver_rt::string_slice(&{}, crate::aver_int_clamp_i64(&{}), crate::aver_int_clamp_i64(&{}))",
                s, from, to
            )
        }
        "String.contains" => {
            let s = arg!(0);
            let sub = mir_str_arg_or_deref(&args[1], ctx)?;
            format!("{}.contains({})", s, sub)
        }
        "String.startsWith" => {
            let s = arg!(0);
            let prefix = mir_str_arg_or_deref(&args[1], ctx)?;
            format!("{}.starts_with({})", s, prefix)
        }
        "String.endsWith" => {
            let s = arg!(0);
            let suffix = mir_str_arg_or_deref(&args[1], ctx)?;
            format!("{}.ends_with({})", s, suffix)
        }
        "String.trim" => format!("{}.trim().to_string()", arg!(0)),
        "String.toUpper" => format!("{}.to_uppercase()", arg!(0)),
        "String.toLower" => format!("{}.to_lowercase()", arg!(0)),
        "String.split" => {
            let s = arg!(0);
            let delim = arg!(1);
            format!(
                "aver_rt::AverList::from_vec({}.split(&*{}).map(|s| s.to_string()).collect::<Vec<_>>())",
                s, delim
            )
        }
        "String.join" => {
            let parts = arg!(0);
            let delim = arg!(1);
            format!("aver_rt::string_join(&{}, &{})", parts, delim)
        }
        "String.replace" => {
            let s = arg!(0);
            let from = arg!(1);
            let to = arg!(2);
            format!("{}.replace(&*{}, &*{})", s, from, to)
        }
        "String.chars" => format!(
            "aver_rt::AverList::from_vec({}.chars().map(|c| c.to_string()).collect::<Vec<_>>())",
            arg!(0)
        ),
        "String.byteLength" => {
            format!("aver_rt::AverInt::from_i64({}.len() as i64)", arg!(0))
        }
        // `String` already stores valid UTF-8. Encoding therefore copies its
        // bytes exactly once into the proof-selected Bytes carrier; it never
        // walks Unicode scalar values or inflates octets into `AverInt`s.
        "String.toUtf8" => {
            let text = arg!(0);
            if ctx
                .codegen
                .is_some_and(|codegen| super::uses_packed_u8(codegen, "Bytes"))
            {
                format!(
                    "crate::aver_generated::bytes::Bytes {{ values: aver_rt::AverPackedU8::from_vec(({text}).as_bytes().to_vec()) }}"
                )
            } else {
                format!(
                    "crate::aver_generated::bytes::Bytes {{ values: aver_rt::AverIntList::from_packed(aver_rt::AverPackedU8::from_vec(({text}).as_bytes().to_vec())) }}"
                )
            }
        }
        // Validate directly over packed storage. The normal proof-selected
        // path borrows the byte slice (no intermediate Vec); the fallback
        // clones its hybrid carrier cheaply when it is packed and validates a
        // wide carrier once before decoding.
        "String.fromUtf8" => {
            let bytes = arg!(0);
            if ctx
                .codegen
                .is_some_and(|codegen| super::uses_packed_u8(codegen, "Bytes"))
            {
                format!(
                    "std::str::from_utf8(({bytes}).values.as_slice()).map(aver_rt::AverStr::from).map_err(|_| aver_rt::AverStr::from(\"invalid UTF-8\"))"
                )
            } else {
                format!(
                    "{{ let __bytes = ({bytes}).values.clone().into_packed().expect(\"Bytes invariant violated\"); std::str::from_utf8(__bytes.as_slice()).map(aver_rt::AverStr::from).map_err(|_| aver_rt::AverStr::from(\"invalid UTF-8\")) }}"
                )
            }
        }

        // ---- List ----
        "List.len" => {
            if let MirExpr::List(items) = &args[0].node
                && items.is_empty()
            {
                "aver_rt::AverInt::from_i64(0)".to_string()
            } else {
                // Producer: wrap the `usize` length in `AverInt`.
                format!("aver_rt::AverInt::from_i64({}.len() as i64)", arg!(0))
            }
        }
        "List.prepend" => {
            let list_type = if type_is_int_list(args[1].ty()) {
                "aver_rt::AverIntList"
            } else {
                "aver_rt::AverList"
            };
            format!("{list_type}::prepend({}, &{})", clone!(0), clone!(1))
        }
        "List.take" => {
            let list = arg!(0);
            let count = arg!(1);
            // `clamp_list_count` is the same clamp the VM and the interpreter
            // apply: a negative count takes nothing, a count past `usize`
            // takes everything. Reading `to_usize().unwrap_or(usize::MAX)`
            // straight had those two backwards, so a negative count took the
            // whole list here and nothing everywhere else.
            if type_is_int_list(args[0].ty()) {
                format!(
                    "{{ let __n = aver_rt::clamp_list_count(&({count})); ({list}).take_first(__n) }}"
                )
            } else {
                format!(
                    "{{ let __n = aver_rt::clamp_list_count(&({count})); aver_rt::AverList::from_vec(({list}).iter().take(__n).cloned().collect::<Vec<_>>()) }}"
                )
            }
        }
        "List.drop" => {
            let list = arg!(0);
            let count = arg!(1);
            // `drop_first` shares the body and advances the offset, so the
            // step costs what it steps over rather than what is left behind.
            format!(
                "{{ let __n = aver_rt::clamp_list_count(&({count})); ({list}).drop_first(__n) }}"
            )
        }
        "List.concat" => {
            let list_type = if type_is_int_list(args[0].ty()) {
                "aver_rt::AverIntList"
            } else {
                "aver_rt::AverList"
            };
            format!("{list_type}::concat(&{}, &{})", clone!(0), clone!(1))
        }
        "List.reverse" => format!("{}.reverse()", arg!(0)),
        "List.contains" => {
            let list = arg!(0);
            let item = arg!(1);
            format!("{}.contains(&{})", list, item)
        }
        "List.zip" => {
            let a = arg!(0);
            let b = arg!(1);
            let a_iter = if type_is_int_list(args[0].ty()) {
                format!("({a}).iter_cloned()")
            } else {
                format!("({a}).iter().cloned()")
            };
            let b_iter = if type_is_int_list(args[1].ty()) {
                format!("({b}).iter_cloned()")
            } else {
                format!("({b}).iter().cloned()")
            };
            format!("aver_rt::AverList::from_vec({a_iter}.zip({b_iter}).collect::<Vec<_>>())")
        }
        "List.fromVector" => {
            if type_is_int_vector(args[0].ty()) {
                format!("aver_rt::AverIntList::from_vec({}.to_vec())", arg!(0))
            } else {
                format!("{}.to_list()", arg!(0))
            }
        }

        // ---- Map ----
        "Map.fromList" => format!(
            "{{ let mut m = HashMap::new(); for (k, v) in {}.iter().cloned() {{ m = m.insert_owned(k, v); }} m }}",
            clone!(0)
        ),
        "Map.entries" => format!(
            "{{ let mut es: Vec<_> = {}.iter().map(|(k, v)| (k.clone(), v.clone())).collect(); es.sort_by(|a, b| a.0.cmp(&b.0)); aver_rt::AverList::from_vec(es) }}",
            arg!(0)
        ),
        "Map.get" => {
            let map = arg!(0);
            let key = arg!(1);
            format!("{}.get(&{}).cloned()", map, key)
        }
        "Map.set" => format!("{}.insert_owned({}, {})", clone!(0), clone!(1), clone!(2)),
        "Map.has" => {
            let map = arg!(0);
            let key = arg!(1);
            format!("{}.contains_key(&{})", map, key)
        }
        "Map.remove" => {
            let map = clone!(0);
            let key = arg!(1);
            format!("{}.remove_owned(&{})", map, key)
        }
        "Map.keys" => {
            let list_type = if matches!(args[0].ty(), Some(Type::Map(key, _)) if **key == Type::Int)
            {
                "aver_rt::AverIntList"
            } else {
                "aver_rt::AverList"
            };
            format!(
                "{{ let mut ks: Vec<_> = {}.keys().cloned().collect(); ks.sort(); {list_type}::from_vec(ks) }}",
                arg!(0)
            )
        }
        // Sorted by key, like `Map.keys` and `Map.entries` above and like
        // the VM. Walking `HashMap::values()` directly gave a per-process
        // order, so two runs of the same binary could disagree and
        // `keys[i]` did not pair with `values[i]`.
        "Map.values" => {
            let list_type = if matches!(args[0].ty(), Some(Type::Map(_, value)) if **value == Type::Int)
            {
                "aver_rt::AverIntList"
            } else {
                "aver_rt::AverList"
            };
            format!(
                "{{ let mut es: Vec<_> = {}.iter().map(|(k, v)| (k.clone(), v.clone())).collect(); es.sort_by(|a, b| a.0.cmp(&b.0)); {list_type}::from_vec(es.into_iter().map(|(_, v)| v).collect::<Vec<_>>()) }}",
                arg!(0)
            )
        }
        "Map.len" => format!("aver_rt::AverInt::from_i64({}.len() as i64)", arg!(0)),

        // ---- Crypto ----
        // `Bytes` and `Digest32` are nominal source-defined records, but the
        // builtin is allowed to cross their opaque boundary. The generated
        // Rust stays on the public semantic shape: validated octets in,
        // exactly 32 validated octets out. The builtin is total, so a
        // malformed carrier (a value outside 0..=255 behind the `Bytes`
        // boundary) has no error channel — it must panic rather than
        // silently truncate into a wrong digest.
        "Crypto.sha256" => {
            let bytes = arg!(0);
            if ctx
                .codegen
                .is_some_and(|codegen| super::uses_packed_u8(codegen, "Bytes"))
            {
                format!(
                    "{{ let __digest = aver_rt::crypto::sha256(({bytes}).values.as_slice()); crate::aver_generated::crypto::digest32::Digest32 {{ bytes: crate::aver_generated::bytes::Bytes {{ values: aver_rt::AverPackedU8::from_vec(__digest.to_vec()) }} }} }}"
                )
            } else {
                format!(
                    "{{ let __input: Vec<u8> = ({bytes}).values.iter_cloned().map(|__b| __b.to_u32().and_then(|__b| u8::try_from(__b).ok()).expect(\"Bytes invariant violated\")).collect(); let __digest = aver_rt::crypto::sha256(&__input); crate::aver_generated::crypto::digest32::Digest32 {{ bytes: crate::aver_generated::bytes::Bytes {{ values: aver_rt::AverIntList::from_vec(__digest.into_iter().map(|__b| aver_rt::AverInt::from_i64(__b as i64)).collect()) }} }} }}"
                )
            }
        }

        // ---- Bits ----
        // A bit-level VIEW of `Int` under infinite two's complement, on the
        // same `AverInt` routines the VM calls (`src/types/bits.rs`), so
        // there is one implementation of the semantics and two callers.
        // `and` / `or` / `xor` / `not` are total. Count-taking operations
        // refuse negatives; only paths that may grow/materialize a result
        // apply the fixed bound (`shiftLeft`, plus `low` for negative input).
        // `shiftRight` reaches the 0/-1 sign tail without allocating.
        "Bits.and" => format!("({}).bit_and(&({}))", arg!(0), arg!(1)),
        "Bits.or" => format!("({}).bit_or(&({}))", arg!(0), arg!(1)),
        "Bits.xor" => format!("({}).bit_xor(&({}))", arg!(0), arg!(1)),
        "Bits.not" => format!("({}).bit_not()", arg!(0)),
        "Bits.shiftLeft" => emit_bits_counted(
            "shift_left",
            arg!(0),
            arg!(1),
            "negative shift count",
            "shift_count_too_large_message",
        ),
        "Bits.shiftRight" => emit_bits_counted(
            "shift_right",
            arg!(0),
            arg!(1),
            "negative shift count",
            "shift_count_too_large_message",
        ),
        "Bits.low" => emit_bits_counted(
            "low_bits",
            arg!(0),
            arg!(1),
            "negative bit width",
            "bit_width_too_large_message",
        ),

        // ---- Bool ----
        "Bool.or" => format!("({} || {})", arg!(0), arg!(1)),
        "Bool.and" => format!("({} && {})", arg!(0), arg!(1)),
        "Bool.not" => format!("(!{})", arg!(0)),

        // ---- Unicode code points ----
        "String.firstCodePoint" => format!(
            "({}).chars().next().map(|c| aver_rt::AverInt::from_i64(c as i64))",
            arg!(0)
        ),
        // Index-like lookup: an out-of-`u32` (or invalid) code → `None`.
        "String.fromCodePoint" => format!(
            "({}).to_u32().and_then(char::from_u32).map(|c| c.to_string())",
            arg!(0)
        ),

        // ---- Vector ----
        "Vector.new" => {
            let size = arg!(0);
            let default = clone!(1);
            format!(
                "match aver_rt::checked_vector_size(&({})) {{ Some(__n) => Ok(aver_rt::AverVector::new(__n, {})), None => Err(aver_rt::AverStr::from(aver_rt::vector_size_error_message())) }}",
                size, default
            )
        }
        "Vector.get" => {
            let vec = arg!(0);
            let idx = arg!(1);
            // Index lookup: an out-of-`usize` index → `None` (Vector.get
            // already returns `Option`).
            format!(
                "({}).to_usize().and_then(|__i| {}.get(__i).cloned())",
                idx, vec
            )
        }
        "Vector.set" => {
            let vec = clone!(0);
            let idx = arg!(1);
            let val = clone!(2);
            // `set_owned` returns `Option<Vector>` (None on out-of-range),
            // mirroring the VM. An out-of-`usize` index is likewise `None`,
            // via the checked conversion — never a wrapped index.
            format!(
                "({}).to_usize().and_then(|__i| {}.set_owned(__i, {}))",
                idx, vec, val
            )
        }
        "Vector.len" => format!("aver_rt::AverInt::from_i64({}.len() as i64)", arg!(0)),
        "Vector.fromList" => format!("aver_rt::AverVector::from_vec({}.to_vec())", arg!(0)),

        // ---- BranchPath ----
        // Oracle structural-addressing constructors. The `aver_rt`
        // `BranchPath` struct (+ `root`/`child`/`parse` impls) is
        // re-exported into the generated crate. `BranchPath.Root` is a
        // nullary value, not a call — it lowers to a `FnValue` and is
        // handled in `emit_mir_static_ref`. `.child` / `.parse` are
        // builtin method calls and land here.
        //
        // `child(path: &BranchPath, idx: &AverInt)`: the path arg goes
        // through `mir_borrow_arg` so a borrowed-param `&BranchPath` is
        // passed directly while a fresh owned value (e.g. a nested
        // `BranchPath.child(...)` or `BranchPath.Root`) gets a `&`.
        "BranchPath.child" => {
            let path = mir_borrow_arg(emit_mir_expr(&args[0], ctx)?, &args[0].node, ctx);
            let idx = arg!(1);
            format!("aver_rt::BranchPath::child({}, &({}))", path, idx)
        }
        // `parse(raw: &str)`: `mir_str_arg_or_deref` yields a bare
        // string literal or the `&*` deref form, both `&str`.
        "BranchPath.parse" => {
            let raw = mir_str_arg_or_deref(&args[0], ctx)?;
            format!("aver_rt::BranchPath::parse({})", raw)
        }

        // Not a covered pure builtin (effectful builtins never reach
        // here — gated at the call arm). HIR fallback.
        _ => return None,
    };

    // Mirror of `emit_builtin_call`'s `.into_aver()` post-step for
    // String-returning pure builtins (and Int.mod / Int.fromString /
    // Float.fromString / String.fromCodePoint).
    if super::builtins::builtin_needs_str_conversion(name) {
        Some(format!("({}).into_aver()", result))
    } else {
        Some(result)
    }
}

/// Emit one of the 5 deforestation intrinsics from MIR, byte-identical
/// to the HIR oracle's `emit_builtin_call_inner` intrinsic arms. Args
/// are by-value (raw `emit_mir_expr`, no clone / borrow), matching the
/// loop-rebind shape the deforestation synthesizer emits. The Rust
/// backend deforests differently, so a buffered fn's MIR shape may not
/// byte-match HIR — the parity gate then falls back safely.
fn emit_mir_intrinsic_call(
    intrinsic: BuiltinIntrinsic,
    args: &[Spanned<MirExpr>],
    result_ty: Option<&Type>,
    raw_result: bool,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    match intrinsic {
        BuiltinIntrinsic::BufNew => {
            let cap = emit_mir_expr(&args[0], ctx)?;
            // The capacity is a pure allocation HINT (no semantic effect): a
            // Big / out-of-`usize` value just falls back to 0 (the Vec grows
            // on demand). This is the one site where `unwrap_or(0)` is sound
            // because the value never reaches output — not a silent wrong
            // value, a sizing hint.
            Some(format!(
                "aver_rt::Buffer::with_capacity(({}).to_usize().unwrap_or(0))",
                cap
            ))
        }
        BuiltinIntrinsic::BufAppend => {
            let buf = emit_mir_expr(&args[0], ctx)?;
            let s = emit_mir_expr(&args[1], ctx)?;
            Some(format!(
                "{{ let mut __b = {}; __b.push_str(&{}); __b }}",
                buf, s
            ))
        }
        BuiltinIntrinsic::BufAppendSepUnlessFirst => {
            let buf = emit_mir_expr(&args[0], ctx)?;
            let sep = emit_mir_expr(&args[1], ctx)?;
            Some(format!(
                "{{ let mut __b = {}; if !__b.is_empty() {{ __b.push_str(&{}); }} __b }}",
                buf, sep
            ))
        }
        BuiltinIntrinsic::BufFinalize => {
            let buf = emit_mir_expr(&args[0], ctx)?;
            Some(format!("aver_rt::AverStr::from({})", buf))
        }
        BuiltinIntrinsic::ToStr => {
            let arg = emit_mir_expr(&args[0], ctx)?;
            Some(format!(
                "aver_rt::AverStr::from(aver_rt::aver_display(&({})))",
                arg
            ))
        }
        // Const-divisor Euclidean div/mod (0.24 "Divide"). The HIR
        // resolver's literal-divisor discharge and the MIR const-fold pass
        // only emit these for a literal NON-ZERO divisor, so
        // `AverInt::div_euclid` / `rem_euclid` (which are `None` only on a
        // zero divisor) are always `Some` here — the `.unwrap()` is total.
        // Same routines `Int.div` / `Int.mod` use in `src/types/int.rs`.
        BuiltinIntrinsic::IntDivEuclid => {
            let a = emit_mir_expr(&args[0], ctx)?;
            let b = emit_mir_expr(&args[1], ctx)?;
            Some(format!("({}).div_euclid(&({})).unwrap()", a, b))
        }
        BuiltinIntrinsic::IntModEuclid => {
            let a = emit_mir_expr(&args[0], ctx)?;
            let b = emit_mir_expr(&args[1], ctx)?;
            Some(format!("({}).rem_euclid(&({})).unwrap()", a, b))
        }
        // Const-count bit-level view. The literal-count discharge only emits
        // these for a syntactic bounded non-negative integer literal, so
        // neither `ShiftCountError` arm is reachable and `.unwrap()` is total.
        BuiltinIntrinsic::BitsShiftLeft
        | BuiltinIntrinsic::BitsShiftRight
        | BuiltinIntrinsic::BitsLow => {
            let method = match intrinsic {
                BuiltinIntrinsic::BitsShiftLeft => "shift_left",
                BuiltinIntrinsic::BitsShiftRight => "shift_right",
                _ => "low_bits",
            };
            let x = emit_mir_expr(&args[0], ctx)?;
            let n = emit_mir_expr(&args[1], ctx)?;
            Some(format!("({}).{}(&({})).unwrap()", x, method, n))
        }
        BuiltinIntrinsic::VectorNew => {
            let size = emit_mir_expr(&args[0], ctx)?;
            let fill = emit_mir_expr(&args[1], ctx)?;
            Some(format!(
                "aver_rt::AverVector::new(aver_rt::checked_vector_size(&({})).expect(\"invalid discharged Vector.new size\"), {})",
                size, fill
            ))
        }
        BuiltinIntrinsic::BranchPathChild => {
            let path = mir_borrow_arg(emit_mir_expr(&args[0], ctx)?, &args[0].node, ctx);
            let index = emit_mir_expr(&args[1], ctx)?;
            Some(format!(
                "aver_rt::BranchPath::child_literal({}, &({}))",
                path, index
            ))
        }
        BuiltinIntrinsic::BranchPathParse => {
            let raw = mir_str_arg_or_deref(&args[0], ctx)?;
            Some(format!("aver_rt::BranchPath::parse_literal({})", raw))
        }
        BuiltinIntrinsic::ResultProven => {
            let result = emit_mir_expr(&args[0], ctx)?;
            Some(format!(
                "({}).unwrap_or_else(|__error| panic!(\"provider contract violated: discharged Result returned Err({{}})\", __error))",
                result
            ))
        }
        // Codepoint cursor (chars fusion). Each one is a call into
        // `aver_rt::strcursor` — the VM executes the same routines, so
        // a fused loop cannot answer differently on the two backends.
        // A negative or out-of-`usize` offset is past the end of every
        // string, which is exactly where the end test stops the loop,
        // so `unwrap_or(usize::MAX)` is a saturation and not a silent
        // wrong index.
        BuiltinIntrinsic::StrCursorEnd
        | BuiltinIntrinsic::StrCursorHead
        | BuiltinIntrinsic::StrCursorNext => {
            let s = emit_mir_expr(&args[0], ctx)?;
            let i = emit_mir_expr(&args[1], ctx)?;
            let call =
                |f: &str| format!("aver_rt::{f}(&{s}, ({i}).to_usize().unwrap_or(usize::MAX))");
            Some(match intrinsic {
                BuiltinIntrinsic::StrCursorEnd => call("str_cursor_end"),
                BuiltinIntrinsic::StrCursorHead => {
                    format!("aver_rt::AverStr::from({})", call("str_cursor_head"))
                }
                _ => format!(
                    "aver_rt::AverInt::from_i64({} as i64)",
                    call("str_cursor_next")
                ),
            })
        }
        BuiltinIntrinsic::StrCursorCode => {
            let s = emit_mir_expr(&args[0], ctx)?;
            let i = emit_mir_expr(&args[1], ctx)?;
            let call =
                format!("aver_rt::str_cursor_code(&{s}, ({i}).to_usize().unwrap_or(usize::MAX))");
            Some(if raw_result {
                call
            } else {
                format!("aver_rt::AverInt::from_i64({call})")
            })
        }
        // Codepoint-level case fold. A code outside i64 is outside the
        // scalar range, and the fold answers -1 for every non-scalar —
        // the same wildcard arm the string route would take.
        BuiltinIntrinsic::StrFoldLower | BuiltinIntrinsic::StrFoldUpper => {
            let f = match intrinsic {
                BuiltinIntrinsic::StrFoldLower => "str_fold_lower",
                _ => "str_fold_upper",
            };
            let c = emit_mir_expr(&args[0], ctx)?;
            let call = format!("aver_rt::{f}({c})");
            Some(if raw_result {
                call
            } else {
                format!("aver_rt::AverInt::from_i64({call})")
            })
        }
        BuiltinIntrinsic::StrCode1
        | BuiltinIntrinsic::StrCode1Lower
        | BuiltinIntrinsic::StrCode1Upper => {
            let f = match intrinsic {
                BuiltinIntrinsic::StrCode1 => "str_code1",
                BuiltinIntrinsic::StrCode1Lower => "str_code1_lower",
                _ => "str_code1_upper",
            };
            let s = emit_mir_expr(&args[0], ctx)?;
            Some(format!("aver_rt::AverInt::from_i64(aver_rt::{f}(&{s}))"))
        }
        BuiltinIntrinsic::StrIndexBuild => {
            let s = emit_mir_expr(&args[0], ctx)?;
            Some(format!("aver_rt::string_index_build(&{s})"))
        }
        BuiltinIntrinsic::StrIndexCharAt => {
            let s = emit_mir_expr(&args[0], ctx)?;
            let index = emit_mir_expr(&args[1], ctx)?;
            let position = emit_mir_expr(&args[2], ctx)?;
            Some(format!(
                "aver_rt::string_index_char_at(&{s}, &{index}, &{position})"
            ))
        }
        BuiltinIntrinsic::StrIndexCodeAt => {
            let s = emit_mir_expr(&args[0], ctx)?;
            let index = emit_mir_expr(&args[1], ctx)?;
            let position = emit_mir_expr(&args[2], ctx)?;
            let call = format!("aver_rt::string_index_code_at(&{s}, &{index}, &{position})");
            Some(if raw_result {
                call
            } else {
                format!("aver_rt::AverInt::from_i64({call})")
            })
        }
        BuiltinIntrinsic::StrIndexSlice => {
            let s = emit_mir_expr(&args[0], ctx)?;
            let index = emit_mir_expr(&args[1], ctx)?;
            let from = emit_mir_expr(&args[2], ctx)?;
            let to = emit_mir_expr(&args[3], ctx)?;
            Some(format!(
                "aver_rt::string_index_slice(&{s}, &{index}, &{from}, &{to})"
            ))
        }
        // List builder (list-build fusion). The builder IS the
        // `AverList<T>` the accumulator always was, so the synthesized
        // variant keeps the signature it was built from and nothing here
        // has a type to invent. The capacity is a hint; a value too big
        // for `usize` just means "no hint", the same reading `__buf_new`
        // gives it.
        BuiltinIntrinsic::LstNew => {
            let capacity = emit_mir_expr(&args[0], ctx)?;
            let builder = if type_is_int_list(result_ty) {
                "int_list_builder_new"
            } else {
                "list_builder_new"
            };
            Some(format!(
                "aver_rt::{builder}(({}).to_usize().unwrap_or(0))",
                capacity,
            ))
        }
        BuiltinIntrinsic::LstPush => {
            let builder = emit_mir_expr(&args[0], ctx)?;
            let item = emit_mir_expr(&args[1], ctx)?;
            let push = if type_is_int_list(result_ty) {
                "int_list_builder_push"
            } else {
                "list_builder_push"
            };
            Some(format!("aver_rt::{push}({}, {})", builder, item))
        }
        BuiltinIntrinsic::LstFinalize => {
            let builder = emit_mir_expr(&args[0], ctx)?;
            let finalize = if type_is_int_list(result_ty) {
                "int_list_builder_finalize"
            } else {
                "list_builder_finalize"
            };
            Some(format!("aver_rt::{finalize}({})", builder))
        }
        // Byte builder (byte-sink retarget). Same aver-rt routines the
        // VM's opcodes mirror, so the two backends cannot answer
        // differently — the finalizer IS `Bytes.fromList` over the
        // pushed elements, error message included, and the synthesized
        // AST around the call wraps its `Ok` into the `Bytes` record.
        BuiltinIntrinsic::BytNew => {
            let capacity = emit_mir_expr(&args[0], ctx)?;
            Some(format!(
                "aver_rt::byte_builder_new(({}).to_usize().unwrap_or(0))",
                capacity
            ))
        }
        BuiltinIntrinsic::BytPush => {
            let builder = emit_mir_expr(&args[0], ctx)?;
            let item = emit_mir_expr(&args[1], ctx)?;
            Some(format!("aver_rt::byte_builder_push({}, {})", builder, item))
        }
        BuiltinIntrinsic::BytFinalize => {
            let builder = emit_mir_expr(&args[0], ctx)?;
            Some(format!("aver_rt::byte_builder_finalize({})", builder))
        }
    }
}

/// One shape for the three `Bits` operations that take a count. Both refusal
/// messages stay catchable and come from `aver-rt`, so generated Rust cannot
/// drift from the VM when the shared materialization boundary changes.
fn emit_bits_counted(
    method: &str,
    x: String,
    n: String,
    negative_message: &str,
    too_large_message_fn: &str,
) -> String {
    format!(
        "match ({x}).{method}(&({n})) {{ Ok(__v) => Ok(__v), Err(aver_rt::ShiftCountError::Negative) => Err(\"{negative_message}\".into()), Err(aver_rt::ShiftCountError::TooLarge) => Err(aver_rt::{too_large_message_fn}()) }}"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::SymbolTable;
    use crate::ir::mir::{LocalId, MirBinOp, MirCall, MirExpr, MirLocal};
    use std::sync::OnceLock;

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: OnceLock::new(),
        }
    }

    fn span_ty<T>(node: T, ty: Type) -> Spanned<T> {
        let stamp = OnceLock::new();
        let _ = stamp.set(ty);
        Spanned {
            node,
            line: 0,
            ty: stamp,
        }
    }

    /// Empty `MirEmitCtx` with statically-borrowed empty symbol
    /// table + empty module-prefix set. `OnceLock`s give us a
    /// `'static` lifetime so tests can pass `&empty_ctx()`
    /// inline without juggling local owners.
    fn empty_ctx() -> MirEmitCtx<'static> {
        use std::sync::OnceLock;
        static SYMBOLS: OnceLock<SymbolTable> = OnceLock::new();
        static PREFIXES: OnceLock<HashSet<String>> = OnceLock::new();
        MirEmitCtx::for_test(
            SYMBOLS.get_or_init(SymbolTable::default),
            PREFIXES.get_or_init(HashSet::new),
        )
    }

    #[test]
    fn self_tco_rc_wraps_follow_source_parameter_order() {
        let params = vec![
            ("first".to_string(), "List<Int>".to_string()),
            ("second".to_string(), "Map<String,Int>".to_string()),
            ("third".to_string(), "Vector<Int>".to_string()),
        ];
        let rc_indices = HashSet::from([2, 0, 1]);
        assert_eq!(
            emit_self_tco_rc_wraps(&params, &rc_indices),
            [
                "    let first @ _ = std::sync::Arc::new(first);",
                "    let second @ _ = std::sync::Arc::new(second);",
                "    let third @ _ = std::sync::Arc::new(third);",
            ]
        );
    }

    #[test]
    fn emits_int_literal_as_i64_suffix() {
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        assert_eq!(
            emit_mir_expr(&lit, &empty_ctx()).as_deref(),
            Some("aver_rt::AverInt::from_i64(42)")
        );
    }

    #[test]
    fn emits_local_via_aver_name_to_rust() {
        let local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let expr = span(MirExpr::Local(span(local)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("local should emit");
        assert!(
            emit.contains("x"),
            "local emit should reference `x`: {emit}"
        );
    }

    #[test]
    fn returns_none_for_synthetic_local() {
        let local = MirLocal {
            slot: LocalId(7),
            last_use: false,
            name: String::new(),
        };
        let expr = span(MirExpr::Local(span(local)));
        assert!(emit_mir_expr(&expr, &empty_ctx()).is_none());
    }

    #[test]
    fn empty_fn_policy_has_no_anchor() {
        // The shared no-anchor policy: no params/locals, nothing
        // borrowed-by-default — the MIR mirror of `EmitCtx::empty()`.
        let policy = MirFnEmitPolicy::empty();
        assert!(policy.local_types.is_empty());
        assert!(policy.rc_wrapped.is_empty());
        assert!(policy.borrowed_params.is_empty());
        assert!(policy.current_module_scope.is_none());
    }

    #[test]
    fn program_level_ctx_renders_free_expr() {
        // A program-level ctx (empty policy + a real symbol table /
        // codegen) renders a free-standing literal — the verify-case
        // shape (no fn anchor). We can't build a full `CodegenContext`
        // cheaply here, so assert the policy/ctx wiring via the
        // walker on a literal that needs no `codegen`.
        let policy = MirFnEmitPolicy::empty();
        use std::sync::OnceLock;
        static SYMBOLS: OnceLock<SymbolTable> = OnceLock::new();
        static PREFIXES: OnceLock<HashSet<String>> = OnceLock::new();
        static BUILTINS: OnceLock<Vec<String>> = OnceLock::new();
        // `program_level` needs a `&CodegenContext`; the literal arm
        // never reads it, so exercise the borrow-field plumbing via
        // `for_test` + the empty policy's slices instead (same shapes).
        let ctx = MirEmitCtx {
            symbol_table: SYMBOLS.get_or_init(SymbolTable::default),
            module_prefixes: PREFIXES.get_or_init(HashSet::new),
            codegen: None,
            local_types: &policy.local_types,
            rc_wrapped: &policy.rc_wrapped,
            rc_wrapped_are_borrowed_refs: false,
            borrowed_params: &policy.borrowed_params,
            loop_carried_params: empty_string_set(),
            owned_params: &policy.owned_params,
            current_module_scope: policy.current_module_scope.as_deref(),
            mir_builtins: BUILTINS.get_or_init(Vec::new),
            bare: &policy.bare,
            try_err_to_string: false,
        };
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        assert_eq!(
            emit_mir_expr(&lit, &ctx).as_deref(),
            Some("aver_rt::AverInt::from_i64(7)")
        );
    }

    #[test]
    fn main_body_policy_borrows_by_default_like_hir() {
        // `emit_mir_main_body` builds its policy from the resolved-main
        // via `from_resolved(.., borrow_by_default = true)` — the same
        // non-TCO borrow rules the HIR main body uses (`build_fn_ectx`).
        // A `List<Int>` param borrows; an `Int` param does not. (Main
        // usually has no params, but the policy must honour the same
        // rule so a `main(args: List<String>)`-style entry borrows
        // identically to every other fn.)
        let resolved = crate::ir::hir::ResolvedFnDef {
            fn_id: crate::ir::FnId(0),
            name: "main".to_string(),
            line: 1,
            params: vec![
                ("xs".to_string(), Type::List(Box::new(Type::Int))),
                ("n".to_string(), Type::Int),
            ],
            return_type: Type::Unit,
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ir::hir::ResolvedFnBody::Block(vec![])),
            resolution: None,
        };
        let policy = MirFnEmitPolicy::from_resolved(&resolved, None, true);
        assert!(policy.borrowed_params.contains("xs"));
        assert!(!policy.borrowed_params.contains("n"));
        assert!(policy.current_module_scope.is_none());
    }

    #[test]
    fn emits_int_binop_add_as_averint_method() {
        let x = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let bop = MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span_ty(MirExpr::Local(span(x.clone())), Type::Int)),
            rhs: Box::new(span_ty(MirExpr::Local(span(x)), Type::Int)),
        };
        let expr = span(MirExpr::BinOp(span(bop)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("binop should emit");
        // Int arithmetic lowers to the non-wrapping `AverInt::add(&rhs)`.
        assert_eq!(emit, "x.add(&x)");
    }

    #[test]
    fn emits_str_binop_add_as_concat() {
        // When both operands are stamped `Str`,
        // the BinOp::Add path emits `(l + &r)` to match HIR's
        // `AverStr` concat shape.
        let s = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "s".to_string(),
        };
        let bop = MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span_ty(MirExpr::Local(span(s.clone())), Type::Str)),
            rhs: Box::new(span_ty(MirExpr::Local(span(s)), Type::Str)),
        };
        let expr = span(MirExpr::BinOp(span(bop)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("binop should emit");
        assert!(
            emit.contains(" + &"),
            "Str+Str should emit `+ &` for AverStr concat: {emit}"
        );
    }

    #[test]
    fn emits_neg_as_paren_minus_inner() {
        // An Int operand negates via `AverInt::neg()` (no std `-`).
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Neg(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("neg should emit");
        assert_eq!(emit, "aver_rt::AverInt::from_i64(7).neg()");
    }

    #[test]
    fn returns_none_for_builtin_call_without_table() {
        // On the coverage / test path the `mir_builtins` table is
        // empty, so a `BuiltinId` resolves to nothing → HIR fallback.
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Str(
                "hello".to_string(),
            ))))],
        };
        let expr = span(MirExpr::Call(span(call)));
        assert!(emit_mir_expr(&expr, &empty_ctx()).is_none());
    }

    /// `MirEmitCtx` carrying a one-entry builtin table so `Call(Builtin)`
    /// resolves `BuiltinId(0)` → `name`. Leaks the backing `Vec` to give
    /// it a `'static` lifetime (test-only).
    fn ctx_with_builtin(name: &str) -> MirEmitCtx<'static> {
        use std::sync::OnceLock;
        static SYMBOLS: OnceLock<SymbolTable> = OnceLock::new();
        static PREFIXES: OnceLock<HashSet<String>> = OnceLock::new();
        let builtins: &'static [String] = Box::leak(vec![name.to_string()].into_boxed_slice());
        let mut ctx = MirEmitCtx::for_test(
            SYMBOLS.get_or_init(SymbolTable::default),
            PREFIXES.get_or_init(HashSet::new),
        );
        ctx.mir_builtins = builtins;
        ctx
    }

    fn int_lit(n: i64) -> Spanned<MirExpr> {
        span_ty(
            MirExpr::Literal(span(crate::ast::Literal::Int(n))),
            Type::Int,
        )
    }

    #[test]
    fn emits_pure_builtin_int_mod_with_into_aver() {
        // `Int.mod` is a covered PURE builtin; it carries the
        // `.into_aver()` post-step (`builtin_needs_str_conversion`).
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![int_lit(7), int_lit(3)],
        };
        let expr = span(MirExpr::Call(span(call)));
        let emit = emit_mir_expr(&expr, &ctx_with_builtin("Int.mod")).expect("Int.mod emits");
        assert_eq!(
            emit,
            "(match (aver_rt::AverInt::from_i64(7)).rem_euclid(&(aver_rt::AverInt::from_i64(3))) { Some(__r) => Ok(__r), None => Err(\"division by zero\".to_string()) }).into_aver()"
        );
    }

    #[test]
    fn emits_pure_builtin_bool_or() {
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![
                span_ty(
                    MirExpr::Literal(span(crate::ast::Literal::Bool(true))),
                    Type::Bool,
                ),
                span_ty(
                    MirExpr::Literal(span(crate::ast::Literal::Bool(false))),
                    Type::Bool,
                ),
            ],
        };
        let expr = span(MirExpr::Call(span(call)));
        let emit = emit_mir_expr(&expr, &ctx_with_builtin("Bool.or")).expect("Bool.or emits");
        assert_eq!(emit, "(true || false)");
    }

    #[test]
    fn capability_operation_returns_none_without_codegen_ctx() {
        // Capability operations need their source contract from the
        // production CodegenContext and cannot fall through to a legacy
        // host-builtin emitter.
        let call = MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Str(
                "hi".to_string(),
            ))))],
        };
        let expr = span(MirExpr::Call(span(call)));
        assert!(
            emit_mir_expr(&expr, &ctx_with_builtin("Console.print")).is_none(),
            "capability-backed Console.print needs a CodegenContext"
        );
    }

    #[test]
    fn emits_buf_finalize_intrinsic() {
        // `__buf_finalize(buf)` → `aver_rt::AverStr::from(buf)`.
        let buf = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "b".to_string(),
        };
        let call = MirCall {
            callee: MirCallee::Intrinsic(BuiltinIntrinsic::BufFinalize),
            args: vec![span(MirExpr::Local(span(buf)))],
        };
        let expr = span(MirExpr::Call(span(call)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("__buf_finalize emits");
        assert_eq!(emit, "aver_rt::AverStr::from(b)");
    }

    #[test]
    fn emits_return_keyword() {
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Return(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("return should emit");
        assert_eq!(emit, "return aver_rt::AverInt::from_i64(7)");
    }

    fn symbols_with_one_type(name: &str, scope: Option<&str>) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        let key = scope.map_or_else(
            || TypeKey::entry(name),
            |module| TypeKey::in_module(module, name),
        );
        st.types.push(TypeEntry {
            key,
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![],
            is_product: true,
            is_capability_resource: false,
        });
        st
    }

    #[test]
    fn emits_record_create_unscoped() {
        // `Point { x: 1, y: 2 }`. HIR-parity: the walker emits the
        // verbatim source-level `type_name` (`MirRecordCreate.type_name`),
        // the same string the HIR walker reads — not a symbol-table
        // lookup. The resolver leaves the user-typed name bare.
        let field_x = crate::ir::mir::MirRecordField {
            name: "x".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(1)))),
        };
        let field_y = crate::ir::mir::MirRecordField {
            name: "y".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(2)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Point".to_string(),
            fields: vec![field_x, field_y],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Point", None);
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("record create should emit");
        assert_eq!(
            emit,
            "Point { x: aver_rt::AverInt::from_i64(1), y: aver_rt::AverInt::from_i64(2) }"
        );
    }

    #[test]
    fn emits_tcp_connection_record_with_rename() {
        // `Tcp.Connection` is the lone hardcoded special-case: HIR
        // renames it to the re-exported `Tcp_Connection` struct
        // inline. The MIR walker mirrors that exactly (no bounce) so
        // the output is byte-identical to HIR.
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Tcp.Connection".to_string(),
            fields: vec![],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Connection", Some("Tcp"));
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("tcp connection record should emit");
        assert_eq!(emit, "Tcp_Connection {  }");
    }

    #[test]
    fn emits_terminal_size_record_through_capability_module() {
        let field_w = crate::ir::mir::MirRecordField {
            name: "width".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(80)))),
        };
        let field_h = crate::ir::mir::MirRecordField {
            name: "height".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(24)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Terminal.Size".to_string(),
            fields: vec![field_w, field_h],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        let st = symbols_with_one_type("Size", Some("Terminal"));
        let prefixes = HashSet::from(["Terminal".to_string()]);
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("terminal size record should emit");
        assert_eq!(
            emit,
            "crate::aver_generated::terminal::Size { width: aver_rt::AverInt::from_i64(80), height: aver_rt::AverInt::from_i64(24) }"
        );
    }

    #[test]
    fn emits_record_create_dep_module_as_bare_name() {
        // A dep-module record emits the bare type name the user
        // typed (`Expr { … }`) — the resolver doesn't dot-prefix
        // `RecordCreate.type_name`, and the consumer module's import
        // makes `Expr` resolve. HIR-parity via the verbatim
        // `type_name` string.
        let field = crate::ir::mir::MirRecordField {
            name: "tag".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(1)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Expr".to_string(),
            fields: vec![field],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        st.types.push(TypeEntry {
            key: TypeKey::in_module("ast", "Expr"),
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![],
            is_product: true,
            is_capability_resource: false,
        });
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("dep-module record should emit");
        assert_eq!(emit, "Expr { tag: aver_rt::AverInt::from_i64(1) }");
    }

    #[test]
    fn emits_record_create_dep_module_qualified_when_prefix_registered() {
        // Residual-2 fix: when the owning module's prefix IS registered
        // in `module_prefixes` (the verify-test codegen path, where the
        // `#[cfg(test)]` module has no glob `use` bringing the dep type
        // into scope), a cross-module `RecordCreate` must emit the
        // module-mangled Rust path — not the bare name. This is the
        // sibling of the `Construct(User)` ctor mangling.
        let field = crate::ir::mir::MirRecordField {
            name: "id".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(1)))),
        };
        let rec = crate::ir::mir::MirRecordCreate {
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Note".to_string(),
            fields: vec![field],
        };
        let expr = span(MirExpr::RecordCreate(span(rec)));
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        st.types.push(TypeEntry {
            key: TypeKey::in_module("Apps.Notepad.Store", "Note"),
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![],
            is_product: true,
            is_capability_resource: false,
        });
        let mut prefixes = HashSet::new();
        prefixes.insert("Apps.Notepad.Store".to_string());
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("qualified dep-module record should emit");
        assert_eq!(
            emit,
            "crate::aver_generated::apps::notepad::store::Note { id: aver_rt::AverInt::from_i64(1) }"
        );
    }

    #[test]
    fn emits_record_update_unscoped() {
        // `T { field: v, ..base }`. Verbatim `type_name`; `base`
        // routed through `clone_arg` (here the empty borrow policy
        // means a non-last-use local clones).
        let base = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "base".to_string(),
        };
        let update = crate::ir::mir::MirRecordField {
            name: "x".to_string(),
            value: span(MirExpr::Literal(span(crate::ast::Literal::Int(9)))),
        };
        let upd = crate::ir::mir::MirRecordUpdate {
            base: Box::new(span(MirExpr::Local(span(base)))),
            type_id: Some(crate::ir::TypeId(0)),
            type_name: "Point".to_string(),
            updates: vec![update],
        };
        let expr = span(MirExpr::RecordUpdate(span(upd)));
        let st = symbols_with_one_type("Point", None);
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("record update should emit");
        // `base` is a non-last-use, non-Copy local → `clone_arg`
        // clones it, exactly as HIR's `maybe_clone` does for a
        // `Resolved { last_use: false }` non-Copy local. (A
        // `MirLocal` is always a local read — the resolver's
        // global-Ident passthrough doesn't apply.)
        assert_eq!(
            emit,
            "Point { x: aver_rt::AverInt::from_i64(9), ..base.clone() }"
        );
    }

    fn symbols_with_one_fn(name: &str) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::FnKey;
        use crate::ir::symbol_table::{FnEntry, ModuleEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        st.fns.push(FnEntry {
            key: FnKey::entry(name),
            module: ModuleId(0),
            index_in_module: 0,
        });
        st
    }

    #[test]
    fn emits_tail_call_as_regular_call() {
        // Outside-loop `TailCall` mirrors HIR's
        // regular-call emit shape — `name(args)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let tc = span(MirExpr::TailCall(span(crate::ir::mir::MirTailCall {
            target: crate::ir::FnId(0),
            args: vec![arg],
        })));
        let st = symbols_with_one_fn("loop_step");
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&tc, &ctx).expect("tail call should emit");
        assert_eq!(emit, "loop_step(aver_rt::AverInt::from_i64(7))");
    }

    #[test]
    fn returns_none_for_unsupported_variant() {
        // Pick a variant the walker doesn't cover — `InterpolatedStr`.
        // (The pipeline contract guarantees `ir::interp_lower` rewrites it
        // away before Rust codegen, so the walker deliberately leaves it in
        // the `_ => None` catch-all; reaching it raw signals fall back to
        // HIR.)
        let interp = span(MirExpr::InterpolatedStr(vec![
            crate::ir::mir::MirStrPart::Literal("x".to_string()),
        ]));
        assert!(emit_mir_expr(&interp, &empty_ctx()).is_none());
    }

    #[test]
    fn emits_empty_map_as_hashmap_new() {
        // Empty map literal.
        let expr = span(MirExpr::MapLiteral(vec![]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("map should emit");
        assert_eq!(emit, "HashMap::new()");
    }

    #[test]
    fn emits_nonempty_map_as_vec_into_iter_collect() {
        // Non-empty map literal.
        let k1 = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let v1 = span(MirExpr::Literal(span(crate::ast::Literal::Int(10))));
        let k2 = span(MirExpr::Literal(span(crate::ast::Literal::Int(2))));
        let v2 = span(MirExpr::Literal(span(crate::ast::Literal::Int(20))));
        let expr = span(MirExpr::MapLiteral(vec![(k1, v1), (k2, v2)]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("map should emit");
        assert_eq!(
            emit,
            "vec![(aver_rt::AverInt::from_i64(1), aver_rt::AverInt::from_i64(10)), (aver_rt::AverInt::from_i64(2), aver_rt::AverInt::from_i64(20))].into_iter().collect::<HashMap<_, _>>()"
        );
    }

    #[test]
    fn emits_try_as_question_mark() {
        // `Try(inner)` → `inner?`.
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Try(Box::new(inner)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("try should emit");
        assert_eq!(emit, "aver_rt::AverInt::from_i64(7)?");
    }

    #[test]
    fn emits_try_with_map_err_in_verify_ctx() {
        // Verify-case ctx: the `#[test]` fn returns `Result<(), String>`
        // while generated fns error with generated types (`AverStr`, user
        // error structs/enums), so `Try` must Debug-format the error into
        // a `String` at the `?` boundary — for every error shape, not
        // just `AverStr`.
        let inner = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let expr = span(MirExpr::Try(Box::new(inner)));
        let mut ctx = empty_ctx();
        ctx.try_err_to_string = true;
        let emit = emit_mir_expr(&expr, &ctx).expect("try should emit");
        assert_eq!(
            emit,
            "aver_rt::AverInt::from_i64(7).map_err(|err| format!(\"{:?}\", err))?"
        );
    }

    #[test]
    fn emits_tuple_literal_as_paren_list() {
        // `(7, 9)` tuple.
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(9))));
        let expr = span(MirExpr::Tuple(vec![a, b]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("tuple should emit");
        assert_eq!(
            emit,
            "(aver_rt::AverInt::from_i64(7), aver_rt::AverInt::from_i64(9))"
        );
    }

    #[test]
    fn emits_bare_independent_product_as_parallel_tuple() {
        // `(7, 9)!` — bare product (no unwrap). No replay (empty ctx),
        // so the parallel `thread::scope` body folds straight into a
        // tuple via `emit_tuple_from_vars`. Byte-identical to HIR's
        // `ResolvedExpr::IndependentProduct` `!` arm.
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(9))));
        let expr = span(MirExpr::IndependentProduct(span(
            crate::ir::mir::MirIndependentProduct {
                items: vec![a, b],
                unwrap_results: false,
            },
        )));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("bare product should emit");
        assert_eq!(
            emit,
            "std::thread::scope(|_s| { let _h0 = _s.spawn(move || aver_rt::AverInt::from_i64(7)); \
             let _h1 = _s.spawn(move || aver_rt::AverInt::from_i64(9)); let _r0 = _h0.join().unwrap(); \
             let _r1 = _h1.join().unwrap(); (_r0, _r1) }) "
        );
    }

    #[test]
    fn emits_unwrap_independent_product_with_cancel_flag() {
        // `(7, 9)?!` — unwrap product. No replay (empty ctx), so the
        // `?!` path emits the shared `__cancel_flag`, one
        // `run_cancelable_branch` spawn per element, joins, then the
        // `emit_parallel_result_tuple_unwrap` fold + trailing `?`.
        // Byte-identical to HIR's `ResolvedExpr::IndependentProduct`
        // `?!` arm.
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(9))));
        let expr = span(MirExpr::IndependentProduct(span(
            crate::ir::mir::MirIndependentProduct {
                items: vec![a, b],
                unwrap_results: true,
            },
        )));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("unwrap product should emit");
        assert!(
            emit.starts_with(
                "{ let __cancel_flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false)); \
                 std::thread::scope(|_s| { "
            ),
            "got: {emit}"
        );
        assert!(
            emit.contains("crate::run_cancelable_branch(__cancel_flag0"),
            "got: {emit}"
        );
        assert!(
            emit.contains("crate::run_cancelable_branch(__cancel_flag1"),
            "got: {emit}"
        );
        assert!(
            emit.contains("crate::ParallelBranch::Completed"),
            "got: {emit}"
        );
        assert!(emit.trim_end().ends_with("})? }"), "got: {emit}");
    }

    #[test]
    fn independent_product_clones_each_non_copy_capture_before_spawn() {
        let first = span(MirExpr::Local(span(MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "height".to_string(),
        })));
        let second = span(MirExpr::Local(span(MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "height".to_string(),
        })));
        let expr = span(MirExpr::IndependentProduct(span(
            crate::ir::mir::MirIndependentProduct {
                items: vec![first, second],
                unwrap_results: false,
            },
        )));

        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("product should emit");
        assert_eq!(
            emit.matches("let height = height.clone();").count(),
            2,
            "every move closure needs a capture cloned outside the closure: {emit}"
        );
        assert!(
            emit.contains("let _h0 = { let height = height.clone(); _s.spawn(move ||"),
            "branch 0 capture must be scoped to its spawn: {emit}"
        );
        assert!(
            emit.contains("let _h1 = { let height = height.clone(); _s.spawn(move ||"),
            "branch 1 capture must be scoped to its spawn: {emit}"
        );
    }

    #[test]
    fn emits_empty_list_as_averlist_empty() {
        let expr = span(MirExpr::List(vec![]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("list should emit");
        assert_eq!(emit, "aver_rt::AverList::empty()");
    }

    #[test]
    fn emits_nonempty_list_as_from_vec() {
        let a = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let b = span(MirExpr::Literal(span(crate::ast::Literal::Int(2))));
        let expr = span(MirExpr::List(vec![a, b]));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("list should emit");
        assert_eq!(
            emit,
            "aver_rt::AverList::from_vec(vec![aver_rt::AverInt::from_i64(1), aver_rt::AverInt::from_i64(2)])"
        );
    }

    #[test]
    fn emits_project_as_dotted_field() {
        // `base.field` projection.
        let local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "user".to_string(),
        };
        let base = span(MirExpr::Local(span(local)));
        let proj = crate::ir::mir::MirProject {
            base: Box::new(base),
            field: "name".to_string(),
        };
        let expr = span(MirExpr::Project(span(proj)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("project should emit");
        assert!(
            emit.ends_with(".name"),
            "project should end with `.name`, got: {emit}"
        );
    }

    #[test]
    fn emits_result_ok_as_ok_call() {
        // BuiltinCtor::ResultOk → `Ok(arg)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::Builtin(BuiltinCtor::ResultOk),
            args: vec![arg],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("construct should emit");
        assert_eq!(emit, "Ok(aver_rt::AverInt::from_i64(42))");
    }

    #[test]
    fn emits_option_none_as_bare_none() {
        // BuiltinCtor::OptionNone has no args
        // and emits `None` without parens.
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
            args: vec![],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("construct should emit");
        assert_eq!(emit, "None");
    }

    #[test]
    fn emits_let_as_block_expr() {
        // `let x = 7; x` → `{ let x @ _ = aver_rt::AverInt::from_i64(7); x }`.
        let value = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let body_local = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "x".to_string(),
        };
        let body = span(MirExpr::Local(span(body_local)));
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(value),
            body: Box::new(body),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("let should emit");
        assert_eq!(emit, "{ let x @ _ = aver_rt::AverInt::from_i64(7); x }");
    }

    #[test]
    fn synthetic_let_emits_bare_statement_not_none() {
        // A synthetic Let (intermediate effectful `Stmt::Expr` at non-tail
        // position, or a `_ = effect()` discard) carries an empty
        // `binding_name`. Stage-3 closes the former None gap: the walker
        // now emits the value as a bare statement (`{ value; body }`,
        // result dropped) instead of bailing to HIR.
        let value = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let body = span(MirExpr::Literal(span(crate::ast::Literal::Int(0))));
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(7),
            binding_name: String::new(),
            value: Box::new(value),
            body: Box::new(body),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        assert_eq!(
            emit_mir_expr(&expr, &empty_ctx()).as_deref(),
            Some("{ aver_rt::AverInt::from_i64(7); aver_rt::AverInt::from_i64(0) }")
        );
    }

    /// Build a symbol table holding one type + one variant ctor.
    /// `scope_prefix == Some("foo")` for module-scoped types.
    fn symbols_with_one_user_ctor(
        scope_prefix: Option<&str>,
        type_name: &str,
        variant_name: &str,
    ) -> SymbolTable {
        use crate::ir::ModuleId;
        use crate::ir::identity::TypeKey;
        use crate::ir::symbol_table::{CtorEntry, ModuleEntry, TypeEntry};
        let mut st = SymbolTable::default();
        st.modules.push(ModuleEntry { prefix: None });
        let key = match scope_prefix {
            Some(p) => TypeKey::in_module(p, type_name),
            None => TypeKey::entry(type_name),
        };
        st.types.push(TypeEntry {
            key,
            module: ModuleId(0),
            index_in_module: 0,
            variants: vec![crate::ir::CtorId(0)],
            is_product: false,
            is_capability_resource: false,
        });
        st.ctors.push(CtorEntry {
            owning_type: crate::ir::TypeId(0),
            name: variant_name.to_string(),
        });
        st
    }

    #[test]
    fn emits_user_ctor_unscoped() {
        // `Shape.Circle(r)` (bare type) →
        // `Shape::Circle(r)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::User(crate::ir::CtorId(0)),
            args: vec![arg],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let st = symbols_with_one_user_ctor(None, "Shape", "Circle");
        let prefixes = HashSet::new();
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("user ctor should emit");
        assert_eq!(emit, "Shape::Circle(aver_rt::AverInt::from_i64(7))");
    }

    #[test]
    fn emits_user_ctor_scoped_via_module_prefix() {
        // Dep-module ctor resolved through
        // `module_prefixes` + `module_prefix_to_rust_path`.
        // `ast.Expr.App(x)` → `crate::aver_generated::ast::Expr::App(x)`.
        let arg = span(MirExpr::Literal(span(crate::ast::Literal::Int(1))));
        let con = crate::ir::mir::MirConstruct {
            ctor: MirCtor::User(crate::ir::CtorId(0)),
            args: vec![arg],
        };
        let expr = span(MirExpr::Construct(span(con)));
        let st = symbols_with_one_user_ctor(Some("ast"), "Expr", "App");
        let mut prefixes = HashSet::new();
        prefixes.insert("ast".to_string());
        let ctx = MirEmitCtx::for_test(&st, &prefixes);
        let emit = emit_mir_expr(&expr, &ctx).expect("scoped user ctor should emit");
        assert_eq!(
            emit,
            "crate::aver_generated::ast::Expr::App(aver_rt::AverInt::from_i64(1))"
        );
    }

    #[test]
    fn first_blocker_names_a_top_level_match() {
        // A bare `Match` is an uncovered variant — `first_blocker`
        // must name it "Match" so the coverage histogram reads as a
        // worklist.
        let m = span(MirExpr::Match(span(crate::ir::mir::MirMatch {
            subject: Box::new(span(MirExpr::Literal(span(crate::ast::Literal::Int(0))))),
            arms: vec![],
        })));
        assert!(emit_mir_expr(&m, &empty_ctx()).is_none());
        assert_eq!(first_blocker(&m, &empty_ctx()), Some("Match"));
    }

    #[test]
    fn first_blocker_recurses_to_deepest_builtin_call() {
        // `return (builtinCall(...))` — the outer Return emits cleanly
        // over a covered child, so the blocker the histogram reports
        // must be the *builtin call kind*, not the Return wrapper.
        let call = span(MirExpr::Call(span(MirCall {
            callee: MirCallee::Builtin(crate::ir::BuiltinId(0)),
            args: vec![span(MirExpr::Literal(span(crate::ast::Literal::Int(1))))],
        })));
        let ret = span(MirExpr::Return(Box::new(call)));
        assert!(emit_mir_expr(&ret, &empty_ctx()).is_none());
        assert_eq!(first_blocker(&ret, &empty_ctx()), Some("Call(Builtin)"));
    }

    #[test]
    fn first_blocker_is_none_for_fully_covered_body() {
        // A clean integer literal has no blocker.
        let lit = span(MirExpr::Literal(span(crate::ast::Literal::Int(42))));
        assert!(first_blocker(&lit, &empty_ctx()).is_none());
    }

    /// Minimal `MirFn` carrying just a body — every other field is a
    /// neutral default so the coverage walk (which only reads `body`)
    /// has something well-formed to traverse.
    fn fn_with_body(fn_id: crate::ir::FnId, body: Spanned<MirExpr>) -> crate::ir::mir::MirFn {
        crate::ir::mir::MirFn {
            fn_id,
            name: String::new(),
            params: vec![],
            return_type: String::new(),
            effects: vec![],
            body,
            local_count: 0,
            aliased_slots: std::sync::Arc::new(vec![]),
            repr: crate::ir::mir::MirFnRepr::default(),
        }
    }

    #[test]
    fn coverage_with_blockers_counts_and_buckets() {
        // Build a two-fn program: one emits (a literal), one blocks on
        // Match. The report must read 1 covered / 1 fallback with a
        // single "Match" bucket of count 1.
        let mut program = MirProgram::default();
        let covered_body = span(MirExpr::Literal(span(crate::ast::Literal::Int(7))));
        let blocked_body = span(MirExpr::Match(span(crate::ir::mir::MirMatch {
            subject: Box::new(span(MirExpr::Literal(span(crate::ast::Literal::Int(0))))),
            arms: vec![],
        })));
        program.fns.insert(
            crate::ir::FnId(0),
            fn_with_body(crate::ir::FnId(0), covered_body),
        );
        program.fns.insert(
            crate::ir::FnId(1),
            fn_with_body(crate::ir::FnId(1), blocked_body),
        );

        let (report, blockers) = coverage_report_with_blockers(&program, &empty_ctx());
        assert_eq!(report.total, 2);
        assert_eq!(report.mir_covered, 1);
        assert_eq!(report.hir_fallback, 1);
        assert_eq!(blockers.get("Match"), Some(&1));
    }

    // ── Wave 4 ──────────────────────────────────────────────────────

    /// Build `let a = <a_val>; let b = <b_val>; <body>` as a nested
    /// MIR `Let` chain.
    fn let_chain(
        a: (&str, Spanned<MirExpr>),
        b: (&str, Spanned<MirExpr>),
        body: Spanned<MirExpr>,
    ) -> Spanned<MirExpr> {
        let inner = MirExpr::Let(span(crate::ir::mir::MirLet {
            binding: LocalId(1),
            binding_name: b.0.to_string(),
            value: Box::new(b.1),
            body: Box::new(body),
        }));
        span(MirExpr::Let(span(crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: a.0.to_string(),
            value: Box::new(a.1),
            body: Box::new(span(inner)),
        })))
    }

    #[test]
    fn fn_body_emits_let_chain_as_flat_statement_lines() {
        // A top-level `Let` chain must render as flat `let …;`-lines —
        // the format HIR's `Block` body arm produces — NOT the nested
        // block-expr `{ let a = …; { let b = …; … } }` that an inline
        // `Let` renders. This is the Wave-4 multi-statement boundary.
        let a_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "a".to_string(),
        };
        let body = let_chain(
            ("a", int_lit(1)),
            ("b", int_lit(2)),
            span(MirExpr::Local(span(a_local))),
        );
        let emit = emit_mir_fn_body(&body, &empty_ctx()).expect("let chain emits");
        assert_eq!(
            emit,
            "    crate::cancel_checkpoint();\n    let a @ _ = aver_rt::AverInt::from_i64(1);\n    let b @ _ = aver_rt::AverInt::from_i64(2);\n    a"
        );
    }

    #[test]
    fn fn_body_emits_discarded_intermediate_as_bare_statement() {
        // A discarded intermediate (`Stmt::Expr` at non-tail position, or
        // a `_ = effect()` discard) is modeled as a `Let` with an EMPTY
        // `binding_name`. It must render as a bare `value;` statement (the
        // value evaluated, result dropped) — the mirror of HIR's non-last
        // `ResolvedStmt::Expr` arm — NOT fall back to HIR. This is the
        // dominant Stage-3 None gap.
        //
        // Shape: `g = <1>; <2 discarded>; g`
        let g_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "g".to_string(),
        };
        let body = let_chain(
            ("g", int_lit(1)),
            ("", int_lit(2)), // discarded intermediate — empty binding_name
            span(MirExpr::Local(span(g_local))),
        );
        let emit = emit_mir_fn_body(&body, &empty_ctx()).expect("discarded stmt emits");
        assert_eq!(
            emit,
            "    crate::cancel_checkpoint();\n    let g @ _ = aver_rt::AverInt::from_i64(1);\n    aver_rt::AverInt::from_i64(2);\n    g"
        );
    }

    #[test]
    fn fn_body_emits_leading_discarded_statement() {
        // A body whose FIRST statement is a discard (empty binding_name)
        // must still take the flat path (no first-binding guard) and emit
        // the leading bare statement.
        //
        // Shape: `<1 discarded>; g = <2>; g`
        let g_local = MirLocal {
            slot: LocalId(1),
            last_use: true,
            name: "g".to_string(),
        };
        let body = let_chain(
            ("", int_lit(1)), // leading discard
            ("g", int_lit(2)),
            span(MirExpr::Local(span(g_local))),
        );
        let emit = emit_mir_fn_body(&body, &empty_ctx()).expect("leading discard emits");
        assert_eq!(
            emit,
            "    crate::cancel_checkpoint();\n    aver_rt::AverInt::from_i64(1);\n    let g @ _ = aver_rt::AverInt::from_i64(2);\n    g"
        );
    }

    // ── Owning positions over a borrowed param ──────────────────────
    //
    // A borrowed param is `&T` in the signature, so every position that
    // has to hand back an owned value materialises one. The differential
    // suite proves the emitted project builds and agrees with the VM;
    // these are the same rules at walker resolution, cheap enough to run
    // on every `cargo test`.
    //
    // WHAT THESE CAN AND CANNOT PIN. They run against `ctx_over`, which
    // is NOT the ctx production uses: production always goes through
    // `MirEmitCtx::for_fn` with `codegen: Some(ctx)` and the fn's params
    // in `local_types`, while `ctx_over` leaves `codegen` `None` and
    // takes `local_types` from an empty policy. That is sound today
    // because the three rules under test — `mir_projection_root_local`,
    // `mir_own_borrowed_read` and the `mir_maybe_clone` path they reach
    // — read neither field. It stops being sound the moment either
    // owning position starts consulting a rule that does: the nearest
    // one is `mir_clone_arg`'s Copy-field elision, whose
    // `mir_attr_result_is_copy` returns `false` whenever `codegen` is
    // `None` or the base type is missing from `local_types`. Route the
    // binding or the tail through that and these tests stay green while
    // the emitted bytes change. The net that would see it is the
    // build-and-run differential, not this module.

    /// A read of the named local (`last_use`, so nothing but the borrow
    /// policy can ask for a clone).
    fn read(name: &str, slot: u32) -> Spanned<MirExpr> {
        span(MirExpr::Local(span(MirLocal {
            slot: LocalId(slot),
            last_use: true,
            name: name.to_string(),
        })))
    }

    /// `base.field`.
    fn project(base: Spanned<MirExpr>, field: &str) -> Spanned<MirExpr> {
        span(MirExpr::Project(span(crate::ir::mir::MirProject {
            base: Box::new(base),
            field: field.to_string(),
        })))
    }

    /// A policy whose only borrowed param is `name`.
    fn borrowed_param_policy(name: &str) -> MirFnEmitPolicy {
        let mut policy = MirFnEmitPolicy::empty();
        policy.borrowed_params.insert(name.to_string());
        policy
    }

    /// A ctx over `policy` — the borrow-field plumbing of `empty_ctx`
    /// with a real `borrowed_params` set behind it. `codegen` stays
    /// `None` and `local_types` stays empty; see the section note above
    /// for what that costs.
    fn ctx_over(policy: &MirFnEmitPolicy) -> MirEmitCtx<'_> {
        use std::sync::OnceLock;
        static SYMBOLS: OnceLock<SymbolTable> = OnceLock::new();
        static PREFIXES: OnceLock<HashSet<String>> = OnceLock::new();
        static BUILTINS: OnceLock<Vec<String>> = OnceLock::new();
        MirEmitCtx {
            symbol_table: SYMBOLS.get_or_init(SymbolTable::default),
            module_prefixes: PREFIXES.get_or_init(HashSet::new),
            codegen: None,
            local_types: &policy.local_types,
            rc_wrapped: &policy.rc_wrapped,
            rc_wrapped_are_borrowed_refs: false,
            borrowed_params: &policy.borrowed_params,
            loop_carried_params: empty_string_set(),
            owned_params: &policy.owned_params,
            current_module_scope: policy.current_module_scope.as_deref(),
            mir_builtins: BUILTINS.get_or_init(Vec::new),
            bare: &policy.bare,
            try_err_to_string: false,
        }
    }

    fn list_literal_one() -> Spanned<MirExpr> {
        span_ty(
            MirExpr::List(vec![int_lit(1)]),
            Type::List(Box::new(Type::Int)),
        )
    }

    fn borrowed_list_read(name: &str, slot: u32) -> Spanned<MirExpr> {
        span_ty(
            MirExpr::Local(span(MirLocal {
                slot: LocalId(slot),
                last_use: false,
                name: name.to_string(),
            })),
            Type::List(Box::new(Type::Int)),
        )
    }

    #[test]
    fn equality_borrows_owned_lhs_when_rhs_is_a_borrowed_param() {
        let policy = borrowed_param_policy("other");
        let ctx = ctx_over(&policy);
        let expr = span(MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Eq,
            lhs: Box::new(list_literal_one()),
            rhs: Box::new(borrowed_list_read("other", 0)),
        })));

        assert_eq!(
            emit_mir_expr(&expr, &ctx).expect("equality emits"),
            "(&(aver_rt::AverIntList::from_vec(vec![aver_rt::AverInt::from_i64(1)])) == other)"
        );
    }

    #[test]
    fn inequality_borrows_owned_rhs_when_lhs_is_a_borrowed_param() {
        let policy = borrowed_param_policy("other");
        let ctx = ctx_over(&policy);
        let expr = span(MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Neq,
            lhs: Box::new(borrowed_list_read("other", 0)),
            rhs: Box::new(list_literal_one()),
        })));

        assert_eq!(
            emit_mir_expr(&expr, &ctx).expect("inequality emits"),
            "(other != &(aver_rt::AverIntList::from_vec(vec![aver_rt::AverInt::from_i64(1)])))"
        );
    }

    #[test]
    fn equality_between_two_borrowed_params_stays_direct() {
        let mut policy = borrowed_param_policy("left");
        policy.borrowed_params.insert("right".to_string());
        let ctx = ctx_over(&policy);
        let expr = span(MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Eq,
            lhs: Box::new(borrowed_list_read("left", 0)),
            rhs: Box::new(borrowed_list_read("right", 1)),
        })));

        assert_eq!(
            emit_mir_expr(&expr, &ctx).expect("equality emits"),
            "(left == right)"
        );
    }

    #[test]
    fn tail_field_read_through_a_borrowed_param_clones_at_any_depth() {
        // `s.piece.kind` is behind the same shared reference `s.piece` is,
        // so a rule that only looks one level down leaves the nested read
        // raw and rustc rejects the move out of `&S`.
        let policy = borrowed_param_policy("s");
        let ctx = ctx_over(&policy);
        let one = project(read("s", 0), "piece");
        assert_eq!(
            emit_mir_fn_body(&one, &ctx).expect("depth-1 tail emits"),
            "    crate::cancel_checkpoint();\n    s.piece.clone()"
        );
        let two = project(project(read("s", 0), "piece"), "kind");
        assert_eq!(
            emit_mir_fn_body(&two, &ctx).expect("depth-2 tail emits"),
            "    crate::cancel_checkpoint();\n    s.piece.kind.clone()"
        );
    }

    #[test]
    fn let_binding_of_a_borrowed_param_read_owns_what_it_names() {
        // Naming the value does not change what the fn may do with it:
        // `kept = outcome` then `kept` has to return by value exactly as
        // `outcome` alone does, and `k = s.piece.kind` has to own the
        // field it named.
        let policy = borrowed_param_policy("s");
        let ctx = ctx_over(&policy);
        let body = let_chain(
            ("kept", read("s", 0)),
            ("k", project(project(read("s", 0), "piece"), "kind")),
            read("kept", 1),
        );
        assert_eq!(
            emit_mir_fn_body(&body, &ctx).expect("binding chain emits"),
            "    crate::cancel_checkpoint();\n    let kept @ _ = s.clone();\n    let k @ _ = s.piece.kind.clone();\n    kept"
        );
    }

    #[test]
    fn owning_positions_leave_a_non_borrowed_read_alone() {
        // The rule keys on the ROOT local being a borrowed param. A read
        // rooted anywhere else — here a param the policy does not borrow —
        // already owns its result and must stay verbatim, which is what
        // keeps the emitted bytes of every other fn unchanged.
        let policy = borrowed_param_policy("other");
        let ctx = ctx_over(&policy);
        let body = let_chain(
            ("kept", read("s", 0)),
            ("k", project(project(read("s", 0), "piece"), "kind")),
            project(read("s", 0), "tag"),
        );
        assert_eq!(
            emit_mir_fn_body(&body, &ctx).expect("binding chain emits"),
            "    crate::cancel_checkpoint();\n    let kept @ _ = s;\n    let k @ _ = s.piece.kind;\n    s.tag"
        );
    }

    #[test]
    fn inline_discarded_let_renders_as_bare_block_statement() {
        // An inline `Let` with an empty binding_name (discard not at the
        // body top-level) renders as `{ value; body }` — bare statement,
        // result dropped — not a `let _ = …`.
        let value = int_lit(7);
        let body_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "x".to_string(),
        };
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: String::new(),
            value: Box::new(value),
            body: Box::new(span(MirExpr::Local(span(body_local)))),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("inline discard emits");
        assert_eq!(emit, "{ aver_rt::AverInt::from_i64(7); x }");
    }

    #[test]
    fn inline_let_still_renders_as_block_expr() {
        // An inline `Let` (NOT at top-level body position) still renders
        // as a nested block-expr — only the fn-body path flattens.
        let value = int_lit(7);
        let body_local = MirLocal {
            slot: LocalId(0),
            last_use: true,
            name: "x".to_string(),
        };
        let let_node = crate::ir::mir::MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(value),
            body: Box::new(span(MirExpr::Local(span(body_local)))),
        };
        let expr = span(MirExpr::Let(span(let_node)));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("inline let emits");
        assert_eq!(emit, "{ let x @ _ = aver_rt::AverInt::from_i64(7); x }");
    }

    #[test]
    fn neg_folded_int_literal_emits_signed_from_i64() {
        // `const_fold` collapses `Neg(Int(5))` → `Literal(-5)`; the walker
        // emits the already-signed `AverInt::from_i64(-5)` directly — no
        // `(-…)` re-wrap, since `AverInt` has no std `Neg`.
        let expr = span(MirExpr::Literal(span(crate::ast::Literal::Int(-5))));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("neg int literal emits");
        assert_eq!(emit, "aver_rt::AverInt::from_i64(-5)");
    }

    #[test]
    fn neg_folded_float_literal_re_wraps_like_hir_neg() {
        // `Neg(Float(273.15))` folds to `Literal(-273.15)`; re-wrap to
        // `(-273.15f64)` to match HIR's `(-273.15f64)`.
        let expr = span(MirExpr::Literal(span(crate::ast::Literal::Float(-273.15))));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("neg float literal emits");
        assert_eq!(emit, "(-273.15f64)");
    }

    #[test]
    fn positive_literals_unchanged_by_neg_rewrap() {
        // Positive literals are never wrapped.
        let i = span(MirExpr::Literal(span(crate::ast::Literal::Int(5))));
        assert_eq!(
            emit_mir_expr(&i, &empty_ctx()).as_deref(),
            Some("aver_rt::AverInt::from_i64(5)")
        );
        let f = span(MirExpr::Literal(span(crate::ast::Literal::Float(1.5))));
        assert_eq!(emit_mir_expr(&f, &empty_ctx()).as_deref(), Some("1.5f64"));
    }

    /// Build an `IfThenElse` with a comparison `cond` of the given op
    /// over two named Int locals, and `Int` literal branches.
    fn if_compare(op: BinOp) -> Spanned<MirExpr> {
        let lhs = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "code".to_string(),
        };
        let cond = MirExpr::BinOp(span(crate::ir::mir::MirBinOp {
            op,
            lhs: Box::new(span_ty(MirExpr::Local(span(lhs)), Type::Int)),
            rhs: Box::new(int_lit(48)),
        }));
        span(MirExpr::IfThenElse(span(crate::ir::mir::MirIfThenElse {
            cond: Box::new(span(cond)),
            then_branch: Box::new(int_lit(1)),
            else_branch: Box::new(int_lit(0)),
        })))
    }

    #[test]
    fn if_then_else_keeps_lt_canonical_no_swap() {
        // `<` is canonical (invert=false): keep operator, branches in
        // source order.
        let emit = emit_mir_expr(&if_compare(BinOp::Lt), &empty_ctx()).expect("if emits");
        assert_eq!(
            emit,
            "if (code < aver_rt::AverInt::from_i64(48)) { aver_rt::AverInt::from_i64(1) } else { aver_rt::AverInt::from_i64(0) }"
        );
    }

    #[test]
    fn if_then_else_inverts_gte_to_lt_and_swaps_branches() {
        // `>=` → HIR canonicalizes to `<` + invert (swap branches):
        // `if (code < 48) { else_branch } else { then_branch }`.
        let emit = emit_mir_expr(&if_compare(BinOp::Gte), &empty_ctx()).expect("if emits");
        assert_eq!(
            emit,
            "if (code < aver_rt::AverInt::from_i64(48)) { aver_rt::AverInt::from_i64(0) } else { aver_rt::AverInt::from_i64(1) }"
        );
    }

    #[test]
    fn if_then_else_inverts_lte_to_gt_and_swaps_branches() {
        let emit = emit_mir_expr(&if_compare(BinOp::Lte), &empty_ctx()).expect("if emits");
        assert_eq!(
            emit,
            "if (code > aver_rt::AverInt::from_i64(48)) { aver_rt::AverInt::from_i64(0) } else { aver_rt::AverInt::from_i64(1) }"
        );
    }

    #[test]
    fn if_then_else_inverts_neq_to_eq_and_swaps_branches() {
        let emit = emit_mir_expr(&if_compare(BinOp::Neq), &empty_ctx()).expect("if emits");
        assert_eq!(
            emit,
            "if (code == aver_rt::AverInt::from_i64(48)) { aver_rt::AverInt::from_i64(0) } else { aver_rt::AverInt::from_i64(1) }"
        );
    }

    #[test]
    fn if_then_else_cond_aligns_string_literal_like_expression_equality() {
        // The `if` lowering of a boolean match goes through the same
        // equality emitter as a plain expression (#1037 / #1060), so a
        // string literal on one side derefs the other side exactly as
        // `name == "_"` does outside a match subject.
        let name = MirLocal {
            slot: LocalId(0),
            last_use: false,
            name: "name".to_string(),
        };
        let cond = MirExpr::BinOp(span(crate::ir::mir::MirBinOp {
            op: BinOp::Eq,
            lhs: Box::new(span_ty(MirExpr::Local(span(name)), Type::Str)),
            rhs: Box::new(span_ty(
                MirExpr::Literal(span(crate::ast::Literal::Str("_".to_string()))),
                Type::Str,
            )),
        }));
        let expr = span(MirExpr::IfThenElse(span(crate::ir::mir::MirIfThenElse {
            cond: Box::new(span(cond)),
            then_branch: Box::new(int_lit(1)),
            else_branch: Box::new(int_lit(0)),
        })));
        let emit = emit_mir_expr(&expr, &empty_ctx()).expect("if emits");
        assert_eq!(
            emit,
            "if (&*name == \"_\") { aver_rt::AverInt::from_i64(1) } else { aver_rt::AverInt::from_i64(0) }"
        );
    }
}
