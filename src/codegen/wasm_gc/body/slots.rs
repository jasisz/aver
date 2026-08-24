//! Per-fn slot table + slot-allocation pre-pass over the resolver's
//! local layout. Step 3 deleted the parallel `infer_expr_wasm_type` /
//! `lookup_var_type` / `collect_binding_types` ad-hoc inference layer
//! — slot wasm types now come straight from `Spanned::ty()` (set by
//! the type checker in Step 0). Schema-only AST predicates
//! (`expr_needs_scratch` etc.) survive because they classify pattern
//! shape, not type.

use std::collections::{HashMap, HashSet};

use wasm_encoder::ValType;

use crate::ast::{Literal, Spanned};
use crate::ir::hir::{
    ResolvedCallee, ResolvedExpr, ResolvedFnBody, ResolvedFnDef, ResolvedPattern, ResolvedStmt,
    ResolvedStrPart,
};
use crate::types::Type;

use super::super::WasmGcError;
use super::super::types::{TypeRegistry, aver_to_wasm, struct_ref};
use super::FnMap;
use super::infer::{
    arm_is_option_pattern_resolved, arm_is_result_pattern_resolved, aver_type_str_of,
};

/// Per-fn slot table — one entry per local (param or binding) in
/// resolver-allocation order. Slot N maps to `wasm local N`.
pub(super) struct SlotTable {
    /// Element index = slot number; element value = wasm ValType.
    pub(super) by_slot: Vec<ValType>,
    /// Optional scratch slot of `(ref null eq)` reserved for multi-arm
    /// variant dispatch — holds the subject so `ref.test` and
    /// `ref.cast` can read it across arms without recomputing the
    /// match-subject expression. Allocated when the body contains at
    /// least one multi-arm Constructor match. Slot index, when set,
    /// is always the last slot in `by_slot`.
    pub(super) subject_scratch: Option<u32>,
    /// Optional 4-tuple of scratch slots reserved for inline
    /// `Args.get()` expansion: `(i, len, acc, s)`. `Args.get()` lowers
    /// to `args_len + loop args_get(i) cons` — no host-side
    /// args_get_all import. Allocated only when the body actually
    /// reaches `Args.get()` with no args. `i, len` are i64; `acc` is
    /// `(ref null $List_String)`; `s` is `(ref null $string)`.
    pub(super) args_get_scratch: Option<[u32; 4]>,
    /// Per-`Vector<T>` scratch local for the clone-on-write `Vector.set`
    /// emit. Maps the whitespace-stripped canonical (`Vector<Int>`,
    /// `Vector<Vector<Int>>`, …) to a local of type
    /// `(ref null $vec_T)`. Pre-allocated here because wasm-gc lays
    /// out function locals in one shot before body emit, so each
    /// `Vector.set` site needs a slot reserved up front. The clone-
    /// on-write shape is unsound without it: `Vector.new(n, inner)`
    /// produces N elements aliasing the same `inner` ref, and the
    /// previous `array.set` in place silently rewrote every alias.
    pub(super) vector_set_scratch: HashMap<String, u32>,
    /// Per-`Vector<T>` pair `[size, fill]` used by the dynamic
    /// `Vector.new(size, fill) -> Result<Vector<T>, String>` lowering.
    /// Both arguments are evaluated once, left-to-right, before the size
    /// guard, matching VM semantics even when either argument is effectful.
    pub(super) vector_new_scratch: HashMap<String, [u32; 2]>,
    /// Scratch i32 slot for the wasip2 `Console.{print, error, warn}`
    /// call-site lowering. Holds two i32 slots: `[len, offset]`.
    /// `len` stores the byte count returned from `__rt_string_to_lm`;
    /// `offset` is the chunk-loop cursor that walks LM[0..len] in
    /// 4096-byte slices because wasmtime-wasi caps each call to
    /// `blocking-write-and-flush` at 4096 bytes (caught by the
    /// 5KB stress fixture on 2026-05-09 — without chunking, every
    /// `Console.print` over 4096 bytes traps with "Buffer too
    /// large for blocking-write-and-flush"). Allocated when the
    /// body contains at least one `Console.{print, error, warn}`
    /// call site, regardless of `TargetMode`. On `AverBridge`
    /// both slots are allocated but unused — two i32 locals are
    /// cheaper than branching `SlotTable::build_for_fn` on target.
    /// Phase 1.2b1.5 / chunked in 1.5.7.
    pub(super) console_print_wasip2_scratch: Option<[u32; 2]>,
    /// Scratch i32 slot holding the canonical-ABI retptr that
    /// `cabi_realloc` returns for the wasip2 `Args.get()` call site
    /// (and any later list-returning effect that hands the retptr
    /// off to the shared `__rt_canonical_decode_list_string` helper).
    /// Allocated when the fn body contains `Args.get()` (no-args).
    /// On `AverBridge` the slot is allocated but unused — same
    /// over-allocation trade-off as `console_print_wasip2_scratch`.
    /// Phase 1.3.2.
    pub(super) args_get_wasip2_retptr_scratch: Option<u32>,
    /// Scratch i32 pair `[retptr, key_len]` for the wasip2
    /// `Env.get(name)` call site. `retptr` holds the
    /// `cabi_realloc(0, 0, 4, 8)` return; `key_len` holds the byte
    /// count returned from `__rt_string_to_lm(name)` so the lookup
    /// helper can compare lengths in O(1) before walking bytes.
    /// Allocated when the fn body contains at least one `Env.get`
    /// call. Phase 1.3.3.
    pub(super) env_get_wasip2_scratch: Option<[u32; 2]>,
    /// Scratch i64 pair for validated wasip2 effects. `Random.int`
    /// keeps both eager-evaluated bounds here before it invokes WASI
    /// randomness; `Time.sleep` uses the first slot after validating
    /// its duration. This keeps argument effects left-to-right and
    /// prevents either expression from being evaluated twice.
    pub(super) validated_effect_wasip2_i64_scratch: Option<[u32; 2]>,
    /// `$AverInt` counterparts of the pair above. Present only with
    /// bignum enabled: arguments are stashed as refs first so the
    /// lowering can turn an out-of-i64 value into `Result.Err`
    /// instead of entering the trapping host conversion helper.
    pub(super) validated_effect_wasip2_aint_scratch: Option<[u32; 2]>,
    /// Scratch `(ref null $AverInt)` slot for the const-compare
    /// specialization (`$AverInt` compared against an i64-fitting
    /// literal). The non-literal operand is stashed here once so the
    /// tag-branch can read its `$magf` (Small/Big discriminant) and
    /// then its `$small`/`$sign` field without re-emitting the operand
    /// expression — which would double-run any side effect in it.
    /// Reserved whenever `bignum` is active (one unused ref local when
    /// no const-comparison is present — cheaper than walking the MIR
    /// body here, which would couple slot allocation to the body walk
    /// and break the differential-gate invariant that `build_for_fn`
    /// reads only the resolver tables, not the MIR body).
    pub(super) const_cmp_scratch: Option<u32>,
    /// Scratch `(ref null $AverInt)` TRIPLE for a guarded builtin whose
    /// operands must outlive the guard: `Bits.shiftLeft/shiftRight/low`
    /// (value, count), boxed `Int.div`/`Int.mod` (a, b), and the fused
    /// `Result.withDefault(Int.div/mod(a, b), default)` (a, b, default).
    ///
    /// These lowerings test one operand to pick an arm, and both arms need
    /// the operands again. Re-emitting them per arm evaluates some twice and
    /// others not at all — `Int.div(value(), count(n))` ran the count three
    /// times and, on the `Bits` error path, skipped the value entirely. The
    /// VM evaluates each argument exactly once, left to right, whichever way
    /// the branch goes, and Aver's effects are eager, so the difference is
    /// observable rather than merely wasteful.
    ///
    /// USE ORDER MATTERS. Emit every operand onto the stack first, then pop
    /// them into these slots in REVERSE. Writing each slot right after its
    /// own operand looks equivalent and is not: a nested call
    /// (`Bits.shiftLeft(x, Bits.low(a, w))`) reuses the same slots, so the
    /// inner call would clobber an already-stashed outer operand. Draining
    /// the stack afterwards means every nested use has finished before the
    /// enclosing one writes anything.
    ///
    /// Reserved only when the body actually reaches one of those guarded
    /// builtins — the same body-conditional shape `args_get_scratch` and
    /// `console_print_wasip2_scratch` use, and deliberately NOT the
    /// unconditional shape of `const_cmp_scratch`. Locals are part of a
    /// function body, so reserving these for every bignum function would
    /// change the emitted bytes of every bignum program, and a certificate
    /// pins the artifact hash: a program that only adds would have had its
    /// certificate invalidated by a fix to division it never calls.
    pub(super) aint_operand_scratch: Option<[u32; 3]>,
}

impl SlotTable {
    /// Pre-scan a fn's full local layout: params, then every binding
    /// produced by `Stmt::Binding` or pattern-bind in `match`. Slot
    /// indices must match what the resolver assigned, since
    /// `Resolved.slot` and `Pattern::Constructor` bindings reference
    /// slot numbers directly.
    ///
    /// Walks the body, reads each binding's wasm type from the typed
    /// AST (`Spanned::ty()`), builds a dense `Vec<ValType>` indexed by
    /// slot number.
    pub(super) fn build_for_fn(
        fd: &ResolvedFnDef,
        registry: &TypeRegistry,
        _fn_map: &FnMap,
        repr: &crate::ir::mir::MirFnRepr,
    ) -> Result<Self, WasmGcError> {
        // Read the per-slot Aver type table the resolver built post-
        // typecheck (`FnResolution.local_slot_types`) and translate
        // each entry into the matching wasm `ValType`. One source of
        // truth for slot indices ↔ types — every backend that needs
        // typed locals consumes the same table instead of re-walking
        // patterns.
        // ETAP-2 SLICE 2a: per-slot Int unboxing seam. When a slot is
        // tagged bare (`repr.bare_slots`) AND its Aver type is `Int`, the
        // slot would carry a scalar `i64` local instead of the `$AverInt`
        // ref. GATED OFF in 2a (`ENABLE_BARE_SLOTS == false`) so every slot
        // falls through to `aver_to_wasm` (→ `$AverInt`); the branch is
        // present, type-checks, and keys directly off the `by_slot` index
        // (`LocalId(i)`, which the audit established is 1:1 with the
        // resolver slot and the wasm local index). 2b flips the gate.
        const ENABLE_BARE_SLOTS: bool = true;
        let mut by_slot: Vec<ValType> = Vec::new();
        if let Some(resolution) = fd.resolution.as_ref() {
            for (i, ty) in resolution.local_slot_types.iter().enumerate() {
                let aver_str = ty.display();
                if ENABLE_BARE_SLOTS
                    && aver_str.trim() == "Int"
                    && repr.slot_is_bare(crate::ir::mir::LocalId(i as u32))
                {
                    // 2b: bare Int slot → raw scalar i64 local.
                    by_slot.push(ValType::I64);
                    continue;
                }
                // `Unit` (and any other type without a wasm
                // representation) still occupies a slot index in the
                // resolver's `local_slots` map — pushing an `i32`
                // placeholder keeps `by_slot[i]` aligned with the
                // resolver's `i`. Otherwise a `_data: Unit = …?`
                // binding would let the next slot read the wrong
                // wasm type and trip validator with `expected eqref,
                // found i32` (and friends).
                //
                // `Invalid` shows up for unused / never-assigned
                // resolver slots and for the typecheck's gradual
                // recovery path — same i32 placeholder so the slot
                // is reserved and indices stay aligned.
                let v = aver_to_wasm(&aver_str, Some(registry))
                    .unwrap_or(None)
                    .unwrap_or(ValType::I32);
                by_slot.push(v);
            }
        } else {
            // Resolution absent — fall back to params-only slots so
            // we at least build a valid (if incomplete) function. A
            // body that touches anything beyond the parameters will
            // surface the gap as a wasm validation error.
            for (_, ty) in &fd.params {
                let v = aver_to_wasm(&ty.display(), Some(registry))
                    .unwrap_or(None)
                    .unwrap_or(ValType::I32);
                by_slot.push(v);
            }
        }
        // If this fn has any multi-arm Constructor match, reserve a
        // scratch slot at the end for stashing the subject. (ref null eq)
        // is the universal carrier — every wasm-gc struct subtypes it.
        let needs_scratch = fn_needs_subject_scratch(fd, registry);
        let subject_scratch = if needs_scratch {
            let scratch_ty = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Eq,
                },
            });
            let idx = by_slot.len() as u32;
            by_slot.push(scratch_ty);
            Some(idx)
        } else {
            None
        };
        // Reserve 4 scratch slots for inline `Args.get()` expansion
        // when reachable. Order matches the inline emit's local-set
        // sequence: i (i64), len (i64), acc (ref List<String>), s
        // (ref string). Allocated once per fn body (multiple Args.get
        // call sites within the same fn share these slots — Args.get
        // is non-reentrant relative to itself, the inline expansion
        // is straight-line).
        let args_get_scratch = if fn_needs_args_get_scratch(fd) {
            let i64_ty = ValType::I64;
            let list_ref = registry.list_type_idx("List<String>").map(|idx| {
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(idx),
                })
            });
            let str_ref = registry.string_array_type_idx.map(|idx| {
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(idx),
                })
            });
            match (list_ref, str_ref) {
                (Some(list_ty), Some(s_ty)) => {
                    let i_idx = by_slot.len() as u32;
                    by_slot.push(i64_ty);
                    let len_idx = by_slot.len() as u32;
                    by_slot.push(i64_ty);
                    let acc_idx = by_slot.len() as u32;
                    by_slot.push(list_ty);
                    let s_idx = by_slot.len() as u32;
                    by_slot.push(s_ty);
                    Some([i_idx, len_idx, acc_idx, s_idx])
                }
                _ => {
                    return Err(WasmGcError::Validation(
                        "Args.get() requires List<String> and String slots in registry — \
                         pre-register them by ensuring the program reaches a List<String> \
                         literal or String value first"
                            .into(),
                    ));
                }
            }
        } else {
            None
        };
        // Allocate one scratch local per unique `Vector<T>` instantiation
        // that appears as the first argument of any `Vector.set` call in
        // this fn body. The clone-on-write emit (`emit_vector_set_*`)
        // builds the new vector via `array.new_default` + `array.copy`
        // and conditionally writes the changed cell on the copy — that
        // requires a typed local to hold the copy ref between
        // `array.copy` and the subsequent `array.set`.
        let mut vector_set_canonicals: HashSet<String> = HashSet::new();
        let mut vector_new_canonicals: HashSet<String> = HashSet::new();
        collect_vector_scratch_canonicals(
            fd,
            &mut vector_set_canonicals,
            &mut vector_new_canonicals,
        );
        let mut vector_set_scratch: HashMap<String, u32> = HashMap::new();
        let mut sorted: Vec<String> = vector_set_canonicals.into_iter().collect();
        sorted.sort(); // deterministic local order
        for canonical in sorted {
            if let Some(vec_idx) = registry.vector_type_idx(&canonical) {
                let ty = ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(vec_idx),
                });
                let local_idx = by_slot.len() as u32;
                by_slot.push(ty);
                vector_set_scratch.insert(canonical, local_idx);
            }
        }
        let mut vector_new_scratch: HashMap<String, [u32; 2]> = HashMap::new();
        let mut sorted: Vec<String> = vector_new_canonicals.into_iter().collect();
        sorted.sort();
        for canonical in sorted {
            let Some(elem) = TypeRegistry::vector_element_type(&canonical) else {
                continue;
            };
            let Some(size_ty) = aver_to_wasm("Int", Some(registry))? else {
                continue;
            };
            let fill_ty = aver_to_wasm(elem, Some(registry))?.unwrap_or(ValType::I32);
            let size_slot = by_slot.len() as u32;
            by_slot.push(size_ty);
            let fill_slot = by_slot.len() as u32;
            by_slot.push(fill_ty);
            vector_new_scratch.insert(canonical, [size_slot, fill_slot]);
        }
        // Phase 1.2b1.5 — two i32 scratch slots for the wasip2
        // Console.* call-site glue: [len, offset]. `len` caches the
        // byte length returned from `__rt_string_to_lm`; `offset` is
        // the chunk-loop cursor that walks LM[0..len] in 4096-byte
        // slices (wasmtime-wasi caps blocking-write-and-flush at
        // 4096 bytes per call — see slots.rs doc-comment).
        let console_print_wasip2_scratch = if fn_needs_console_print_wasip2_scratch(fd) {
            let len_idx = by_slot.len() as u32;
            by_slot.push(ValType::I32);
            let off_idx = by_slot.len() as u32;
            by_slot.push(ValType::I32);
            Some([len_idx, off_idx])
        } else {
            None
        };
        // Phase 1.3.2 — single i32 retptr scratch for the wasip2
        // `Args.get()` call. The shared
        // `__rt_canonical_decode_list_string` helper takes the
        // retptr and does the entire list<string> → List<String>
        // walk, so the call site only needs to remember which i32
        // came back from `cabi_realloc(0, 0, 4, 8)`.
        let args_get_wasip2_retptr_scratch = if fn_needs_args_get_scratch(fd) {
            let idx = by_slot.len() as u32;
            by_slot.push(ValType::I32);
            Some(idx)
        } else {
            None
        };
        let env_get_wasip2_scratch = if fn_needs_env_get_wasip2_scratch(fd) {
            let retptr = by_slot.len() as u32;
            by_slot.push(ValType::I32);
            let key_len = by_slot.len() as u32;
            by_slot.push(ValType::I32);
            Some([retptr, key_len])
        } else {
            None
        };
        let needs_validated_effect_scratch = fn_needs_validated_effect_wasip2_scratch(fd);
        let validated_effect_wasip2_i64_scratch = if needs_validated_effect_scratch {
            let first = by_slot.len() as u32;
            by_slot.push(ValType::I64);
            let second = by_slot.len() as u32;
            by_slot.push(ValType::I64);
            Some([first, second])
        } else {
            None
        };
        let validated_effect_wasip2_aint_scratch =
            match (registry.aint_struct_idx, needs_validated_effect_scratch) {
                (Some(aint_idx), true) if registry.bignum => {
                    let first = by_slot.len() as u32;
                    by_slot.push(struct_ref(aint_idx));
                    let second = by_slot.len() as u32;
                    by_slot.push(struct_ref(aint_idx));
                    Some([first, second])
                }
                _ => None,
            };
        // Const-compare specialization scratch: a single `(ref null
        // $AverInt)` slot to stash the non-literal operand of an
        // `$AverInt`-vs-i64-constant comparison. Reserved whenever
        // bignum is active so the slot index is a pure function of the
        // registry flag (not the MIR body), preserving the invariant
        // that `build_for_fn` keys only off the resolver tables.
        let const_cmp_scratch = if registry.bignum {
            if let Some(aint_idx) = registry.aint_struct_idx {
                let idx = by_slot.len() as u32;
                by_slot.push(struct_ref(aint_idx));
                Some(idx)
            } else {
                None
            }
        } else {
            None
        };
        // Guarded-builtin operand triple — see the field docs.
        let aint_operand_scratch = match (registry.aint_struct_idx, fn_needs_aint_operands(fd)) {
            (Some(aint_idx), true) if registry.bignum => {
                let mut slot = || {
                    let idx = by_slot.len() as u32;
                    by_slot.push(struct_ref(aint_idx));
                    idx
                };
                Some([slot(), slot(), slot()])
            }
            _ => None,
        };
        Ok(Self {
            by_slot,
            subject_scratch,
            args_get_scratch,
            vector_set_scratch,
            vector_new_scratch,
            console_print_wasip2_scratch,
            args_get_wasip2_retptr_scratch,
            env_get_wasip2_scratch,
            validated_effect_wasip2_i64_scratch,
            validated_effect_wasip2_aint_scratch,
            const_cmp_scratch,
            aint_operand_scratch,
        })
    }

    pub(super) fn extra_locals(&self, params_count: usize) -> Vec<ValType> {
        self.by_slot.iter().skip(params_count).copied().collect()
    }
}

/// True if the body reaches a builtin whose lowering GUARDS one operand and
/// then needs the operands again in both arms: boxed `Int.div` / `Int.mod`
/// (guarded on a zero divisor) and the three count-taking `Bits` operations
/// (guarded on the shared valid-count range).
///
/// `Bits.and` / `.or` / `.xor` / `.not` are absent: they have no guard, so
/// emitting their operands inline already evaluates each exactly once. The
/// discharged intrinsics are absent for the same reason.
fn fn_needs_aint_operands(fd: &ResolvedFnDef) -> bool {
    fn guarded(callee: &ResolvedCallee) -> bool {
        matches!(
            callee,
            ResolvedCallee::Builtin(name)
                if matches!(
                    name.as_str(),
                    "Int.div"
                        | "Int.mod"
                        | "Bits.shiftLeft"
                        | "Bits.shiftRight"
                        | "Bits.low"
                )
        )
    }
    fn walk(e: &ResolvedExpr) -> bool {
        let any = |xs: &[Spanned<ResolvedExpr>]| xs.iter().any(|x| walk(&x.node));
        match e {
            ResolvedExpr::Call(callee, args) => guarded(callee) || any(args),
            ResolvedExpr::Match { subject, arms } => {
                walk(&subject.node) || arms.iter().any(|a| walk(&a.body.node))
            }
            ResolvedExpr::BinOp(_, l, r) => walk(&l.node) || walk(&r.node),
            ResolvedExpr::Neg(inner) | ResolvedExpr::ErrorProp(inner) => walk(&inner.node),
            ResolvedExpr::Attr(obj, _) => walk(&obj.node),
            ResolvedExpr::TailCall { args, .. } => any(args),
            ResolvedExpr::Ctor(_, args) => any(args),
            ResolvedExpr::List(xs)
            | ResolvedExpr::Tuple(xs)
            | ResolvedExpr::IndependentProduct(xs, _) => any(xs),
            ResolvedExpr::MapLiteral(pairs) => {
                pairs.iter().any(|(k, v)| walk(&k.node) || walk(&v.node))
            }
            ResolvedExpr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
                ResolvedStrPart::Parsed(inner) => walk(&inner.node),
                ResolvedStrPart::Literal(_) => false,
            }),
            ResolvedExpr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| walk(&e.node)),
            ResolvedExpr::RecordUpdate { base, updates, .. } => {
                walk(&base.node) || updates.iter().any(|(_, e)| walk(&e.node))
            }
            ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {
                false
            }
        }
    }
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|stmt| match stmt {
        ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => walk(&e.node),
    })
}

/// True if the body reaches an `Args.get()` call (no args). The inline
/// expansion needs four scratch slots; they're only worth reserving
/// when actually used.
pub(super) fn fn_needs_args_get_scratch(fd: &ResolvedFnDef) -> bool {
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(stmt_reaches_args_get_no_args)
}

fn stmt_reaches_args_get_no_args(stmt: &ResolvedStmt) -> bool {
    match stmt {
        ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => {
            expr_reaches_args_get_no_args(&value.node)
        }
    }
}

/// Shared walker over a [`ResolvedExpr`] that returns `true` when any
/// reachable [`ResolvedExpr::Call`] matches the predicate. Builtin
/// callees are recognised by their `ResolvedCallee::Builtin(name)`
/// canonical string; user fns / intrinsics never match the
/// short-circuit predicate (they don't expose Args/Console/Env/Random
/// builtins). Replaces the four near-identical walkers from the
/// pre-Phase-E shape.
fn expr_reaches_builtin_call<F>(expr: &ResolvedExpr, matches: &F) -> bool
where
    F: Fn(&str, &[Spanned<ResolvedExpr>]) -> bool,
{
    match expr {
        ResolvedExpr::Call(callee, args) => {
            if let ResolvedCallee::Builtin(name) = callee
                && matches(name.as_str(), args)
            {
                return true;
            }
            args.iter()
                .any(|a| expr_reaches_builtin_call(&a.node, matches))
        }
        ResolvedExpr::BinOp(_, l, r) => {
            expr_reaches_builtin_call(&l.node, matches)
                || expr_reaches_builtin_call(&r.node, matches)
        }
        ResolvedExpr::Neg(inner) => expr_reaches_builtin_call(&inner.node, matches),
        ResolvedExpr::Match { subject, arms } => {
            expr_reaches_builtin_call(&subject.node, matches)
                || arms
                    .iter()
                    .any(|a| expr_reaches_builtin_call(&a.body.node, matches))
        }
        ResolvedExpr::TailCall { args, .. } => args
            .iter()
            .any(|a| expr_reaches_builtin_call(&a.node, matches)),
        ResolvedExpr::Attr(obj, _) => expr_reaches_builtin_call(&obj.node, matches),
        ResolvedExpr::ErrorProp(inner) => expr_reaches_builtin_call(&inner.node, matches),
        ResolvedExpr::Ctor(_, args) => args
            .iter()
            .any(|a| expr_reaches_builtin_call(&a.node, matches)),
        ResolvedExpr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, e)| expr_reaches_builtin_call(&e.node, matches)),
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            expr_reaches_builtin_call(&base.node, matches)
                || updates
                    .iter()
                    .any(|(_, e)| expr_reaches_builtin_call(&e.node, matches))
        }
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => items
            .iter()
            .any(|e| expr_reaches_builtin_call(&e.node, matches)),
        ResolvedExpr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_reaches_builtin_call(&k.node, matches)
                || expr_reaches_builtin_call(&v.node, matches)
        }),
        ResolvedExpr::InterpolatedStr(parts) => parts.iter().any(|p| {
            if let ResolvedStrPart::Parsed(inner) = p {
                expr_reaches_builtin_call(&inner.node, matches)
            } else {
                false
            }
        }),
        _ => false,
    }
}

fn expr_reaches_args_get_no_args(expr: &ResolvedExpr) -> bool {
    expr_reaches_builtin_call(expr, &|name, args| name == "Args.get" && args.is_empty())
}

/// True if the body reaches at least one `Console.print` /
/// `Console.error` / `Console.warn` call. Used to gate the i32 scratch
/// slot that the wasip2 call-site lowering uses to cache the
/// `__rt_string_to_lm` byte-length return for the retptr computation.
pub(super) fn fn_needs_console_print_wasip2_scratch(fd: &ResolvedFnDef) -> bool {
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(stmt_reaches_console_print)
}

fn stmt_reaches_console_print(stmt: &ResolvedStmt) -> bool {
    match stmt {
        ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => {
            expr_reaches_console_print(&value.node)
        }
    }
}

fn expr_reaches_console_print(expr: &ResolvedExpr) -> bool {
    expr_reaches_builtin_call(expr, &|name, _| {
        matches!(name, "Console.print" | "Console.error" | "Console.warn")
    })
}

/// True if the body reaches at least one `Env.get(name)` call.
/// Used to gate the wasip2 `[retptr, key_len]` scratch pair.
pub(super) fn fn_needs_env_get_wasip2_scratch(fd: &ResolvedFnDef) -> bool {
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(stmt_reaches_env_get)
}

fn stmt_reaches_env_get(stmt: &ResolvedStmt) -> bool {
    match stmt {
        ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => {
            expr_reaches_env_get(&value.node)
        }
    }
}

fn expr_reaches_env_get(expr: &ResolvedExpr) -> bool {
    expr_reaches_builtin_call(expr, &|name, _| name == "Env.get")
}

/// True if the body reaches a validated wasip2 effect whose arguments
/// must survive a range check before the host operation is invoked.
pub(super) fn fn_needs_validated_effect_wasip2_scratch(fd: &ResolvedFnDef) -> bool {
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(stmt_reaches_validated_effect)
}

fn stmt_reaches_validated_effect(stmt: &ResolvedStmt) -> bool {
    match stmt {
        ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => {
            expr_reaches_builtin_call(&value.node, &|name, _| {
                matches!(name, "Random.int" | "Time.sleep")
            })
        }
    }
}

/// True if the body has at least one multi-arm `match` whose arms are
/// `Pattern::Constructor` against a non-newtype variant. Single-arm
/// matches and newtype matches don't need a scratch (the cast is
/// elided), so we only allocate when really necessary.
pub(super) fn fn_needs_subject_scratch(fd: &ResolvedFnDef, registry: &TypeRegistry) -> bool {
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|s| stmt_needs_scratch(s, registry))
}

pub(super) fn stmt_needs_scratch(stmt: &ResolvedStmt, registry: &TypeRegistry) -> bool {
    match stmt {
        ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => {
            expr_needs_scratch(&value.node, registry)
        }
    }
}

#[allow(clippy::only_used_in_recursion)]
pub(super) fn expr_needs_scratch(expr: &ResolvedExpr, registry: &TypeRegistry) -> bool {
    match expr {
        ResolvedExpr::Match { subject, arms } => {
            if expr_needs_scratch(&subject.node, registry) {
                return true;
            }
            // Built-in Option dispatch needs a scratch (subject ref is
            // read multiple times: tag check, value extraction).
            if arms.iter().any(arm_is_option_pattern_resolved) {
                return true;
            }
            if arms.iter().any(arm_is_result_pattern_resolved) {
                return true;
            }
            if arms.iter().any(|a| {
                matches!(
                    &a.pattern,
                    ResolvedPattern::EmptyList | ResolvedPattern::Cons(_, _)
                )
            }) {
                return true;
            }
            if arms
                .iter()
                .any(|a| matches!(&a.pattern, ResolvedPattern::Tuple(_)))
            {
                return true;
            }
            // String-subject match (`match s { "literal" -> ... }`)
            // stashes the subject ref in scratch and tests it against
            // each literal — needs a scratch slot.
            if arms
                .iter()
                .any(|a| matches!(&a.pattern, ResolvedPattern::Literal(Literal::Str(_))))
            {
                return true;
            }
            // Reserve a scratch any time the arms include a
            // Constructor pattern. Earlier we tried to be clever —
            // skip when all variants reduce to newtypes, since the
            // newtype unwrap doesn't need the scratch — but multi-
            // module flatten can land variants whose registry entry
            // isn't visible at slot-allocation time (rogue's
            // `EntityKind.WildIfElse` resolves through `types.av`
            // flattened from a different module). The cost of an
            // unused scratch local is one wasm value; the cost of a
            // missing one is `emit_variant_dispatch` crashing with
            // "no scratch reserved".
            if arms
                .iter()
                .any(|a| matches!(&a.pattern, ResolvedPattern::Ctor(_, _)))
            {
                return true;
            }
            arms.iter()
                .any(|a| expr_needs_scratch(&a.body.node, registry))
        }
        ResolvedExpr::BinOp(_, l, r) => {
            expr_needs_scratch(&l.node, registry) || expr_needs_scratch(&r.node, registry)
        }
        ResolvedExpr::Neg(inner) => expr_needs_scratch(&inner.node, registry),
        ResolvedExpr::Call(callee, args) => {
            // `Option.withDefault(opt, default)` / `Result.withDefault`
            // / `Result.fromOption` — conservatively reserve scratch; the
            // boxed emitters stash the Option/Result for tag inspection.
            if let ResolvedCallee::Builtin(name) = callee
                && matches!(
                    name.as_str(),
                    "Option.withDefault" | "Result.withDefault" | "Result.fromOption"
                )
            {
                return true;
            }
            args.iter().any(|a| expr_needs_scratch(&a.node, registry))
        }
        ResolvedExpr::TailCall { args, .. } => {
            args.iter().any(|a| expr_needs_scratch(&a.node, registry))
        }
        ResolvedExpr::Attr(obj, _) => expr_needs_scratch(&obj.node, registry),
        // `subject?` stashes the Result in scratch, reads tag, and
        // either unwraps field 1 or returns the whole subject.
        ResolvedExpr::ErrorProp(_) => true,
        ResolvedExpr::Ctor(_, args) => args.iter().any(|a| expr_needs_scratch(&a.node, registry)),
        ResolvedExpr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, e)| expr_needs_scratch(&e.node, registry)),
        // List literal with elements uses the scratch slot for the
        // running tail during the right-fold; empty literal lowers to
        // a single ref.null and doesn't need it.
        ResolvedExpr::List(items) if !items.is_empty() => true,
        ResolvedExpr::List(items) => items.iter().any(|e| expr_needs_scratch(&e.node, registry)),
        ResolvedExpr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_needs_scratch(&k.node, registry) || expr_needs_scratch(&v.node, registry)
        }),
        // `(...)?!` (unwrap=true) stashes each element's Result in the
        // scratch slot to read its tag, fall through to the Err return
        // path, or pull the Ok payload — same shape as `Expr::ErrorProp`,
        // just one per element. Bare `(...)!` (unwrap=false) doesn't
        // need scratch — elements are emitted positionally into the
        // tuple struct.
        ResolvedExpr::IndependentProduct(items, unwrap) => {
            *unwrap || items.iter().any(|e| expr_needs_scratch(&e.node, registry))
        }
        ResolvedExpr::Tuple(items) => items.iter().any(|e| expr_needs_scratch(&e.node, registry)),
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            expr_needs_scratch(&base.node, registry)
                || updates
                    .iter()
                    .any(|(_, e)| expr_needs_scratch(&e.node, registry))
        }
        ResolvedExpr::InterpolatedStr(parts) => parts.iter().any(|p| {
            if let ResolvedStrPart::Parsed(inner) = p {
                expr_needs_scratch(&inner.node, registry)
            } else {
                false
            }
        }),
        _ => false,
    }
}

pub(super) fn count_value_params(params: &[(String, Type)]) -> usize {
    params
        .iter()
        .filter(|(_, ty)| ty.display().trim() != "Unit")
        .count()
}

/// Walks the fn body collecting whitespace-stripped `Vector<T>`
/// canonicals — one per call to `Vector.set` whose first argument
/// has a `Vector<T>` type stamp. Drives the per-fn allocation of
/// scratch locals consumed by the clone-on-write emit in
/// `emit_vector_set_or_default` / `emit_vector_set_boxed`.
fn collect_vector_scratch_canonicals(
    fd: &ResolvedFnDef,
    set_out: &mut HashSet<String>,
    new_out: &mut HashSet<String>,
) {
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        match stmt {
            ResolvedStmt::Binding { value, .. } | ResolvedStmt::Expr(value) => {
                walk_expr_for_vector_scratch(value, set_out, new_out)
            }
        }
    }
}

fn walk_expr_for_vector_scratch(
    expr: &Spanned<ResolvedExpr>,
    set_out: &mut HashSet<String>,
    new_out: &mut HashSet<String>,
) {
    match &expr.node {
        ResolvedExpr::Call(callee, args) => {
            // Direct `Vector.set(v, i, x)`.
            if let ResolvedCallee::Builtin(name) = callee
                && name == "Vector.set"
                && args.len() == 3
            {
                let vec_aver = aver_type_str_of(&args[0]);
                let canonical: String = vec_aver.chars().filter(|c| !c.is_whitespace()).collect();
                if canonical.starts_with("Vector<") {
                    set_out.insert(canonical);
                }
            }
            if let ResolvedCallee::Builtin(name) = callee
                && name == "Vector.new"
                && args.len() == 2
                && let Some(fill_ty) = args[1].ty()
            {
                new_out.insert(
                    format!("Vector<{}>", fill_ty.display())
                        .chars()
                        .filter(|c| !c.is_whitespace())
                        .collect(),
                );
            }
            for a in args {
                walk_expr_for_vector_scratch(a, set_out, new_out);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            walk_expr_for_vector_scratch(l, set_out, new_out);
            walk_expr_for_vector_scratch(r, set_out, new_out);
        }
        ResolvedExpr::Neg(inner) => walk_expr_for_vector_scratch(inner, set_out, new_out),
        ResolvedExpr::Match { subject, arms } => {
            walk_expr_for_vector_scratch(subject, set_out, new_out);
            for arm in arms {
                walk_expr_for_vector_scratch(&arm.body, set_out, new_out);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                walk_expr_for_vector_scratch(a, set_out, new_out);
            }
        }
        ResolvedExpr::Attr(obj, _) => walk_expr_for_vector_scratch(obj, set_out, new_out),
        ResolvedExpr::ErrorProp(inner) => walk_expr_for_vector_scratch(inner, set_out, new_out),
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                walk_expr_for_vector_scratch(a, set_out, new_out);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                walk_expr_for_vector_scratch(e, set_out, new_out);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            walk_expr_for_vector_scratch(base, set_out, new_out);
            for (_, e) in updates {
                walk_expr_for_vector_scratch(e, set_out, new_out);
            }
        }
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => {
            for e in items {
                walk_expr_for_vector_scratch(e, set_out, new_out);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                walk_expr_for_vector_scratch(k, set_out, new_out);
                walk_expr_for_vector_scratch(v, set_out, new_out);
            }
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            for p in parts {
                if let ResolvedStrPart::Parsed(inner) = p {
                    walk_expr_for_vector_scratch(inner, set_out, new_out);
                }
            }
        }
        _ => {}
    }
}
