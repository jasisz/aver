use super::{CallTarget, CompileError, FnCompiler};
use crate::ast::{Literal, Spanned};
use crate::ir::hir::{BuiltinCtor, BuiltinIntrinsic, ResolvedCallee, ResolvedCtor, ResolvedExpr};
use crate::ir::hir::{
    ForwardSlot, ResolvedLeafOp, classify_forward_call_resolved, classify_leaf_op_resolved,
    resolved_to_dotted,
};
use crate::ir::identity::{FnId, FnKey};
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::*;
use crate::vm::symbol::VmSymbolTable;

type SpannedResolvedPair<'a> = (&'a Spanned<ResolvedExpr>, &'a Spanned<ResolvedExpr>);
type SpannedResolvedTriple<'a> = (
    &'a Spanned<ResolvedExpr>,
    &'a Spanned<ResolvedExpr>,
    &'a Spanned<ResolvedExpr>,
);

/// Map a typed [`BuiltinIntrinsic`] to its VM opcode + expected
/// arity. Returns `None` for `ToStr`, which lowers to a CONCAT-with-
/// empty trick instead of a dedicated opcode. The arity is checked
/// at the callsite so a future intrinsic with a different shape
/// can't accidentally re-use the wrong opcode.
fn buffer_intrinsic_opcode(intrinsic: BuiltinIntrinsic) -> Option<(u8, usize)> {
    match intrinsic {
        BuiltinIntrinsic::BufNew => Some((BUFFER_NEW, 1)),
        BuiltinIntrinsic::BufAppend => Some((BUFFER_APPEND_STR, 2)),
        BuiltinIntrinsic::BufAppendSepUnlessFirst => Some((BUFFER_APPEND_SEP_UNLESS_FIRST, 2)),
        BuiltinIntrinsic::BufFinalize => Some((BUFFER_FINALIZE, 1)),
        BuiltinIntrinsic::ToStr => None,
    }
}

impl<'a> FnCompiler<'a> {
    pub(super) fn try_compile_leaf_expr(
        &mut self,
        expr: &ResolvedExpr,
    ) -> Result<bool, CompileError> {
        // Snapshot the closure context the leaf classifier needs.
        // The closure captures `self.arena` only via `find_type_id`,
        // which can't run while `self` is mutably borrowed — pre-
        // resolve the membership check up front and pass the result
        // through the function pointer.
        let is_user_type = |name: &str| self.arena.find_type_id(name).is_some();
        let leaf_kind: Option<ResolvedLeafOpKind> =
            classify_leaf_op_resolved(expr, &is_user_type).map(leaf_into_owned_kind);

        if let Some(kind) = leaf_kind {
            self.compile_leaf_op_kind(kind, expr)?;
            return Ok(true);
        }
        Ok(false)
    }

    /// Convenience used by the Independent-Product lowering: push a
    /// callable value (function chunk symbol, builtin symbol, or
    /// local-slot fn ref) onto the stack as a single value. Used
    /// before emitting `CALL_PAR` per branch.
    pub(super) fn compile_callee_as_value(
        &mut self,
        callee: &ResolvedCallee,
    ) -> Result<(), CompileError> {
        match callee {
            ResolvedCallee::Fn(fn_id) => {
                let name = self.canonical_fn_name(*fn_id)?;
                let symbol_id = self.symbols.find(&name).ok_or_else(|| CompileError {
                    msg: format!("missing VM symbol for fn: {}", name),
                })?;
                let idx = self.add_constant(VmSymbolTable::symbol_ref(symbol_id));
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
                Ok(())
            }
            ResolvedCallee::Builtin(name) => {
                let symbol_id = self.symbols.find(name).ok_or_else(|| CompileError {
                    msg: format!("missing VM symbol for builtin: {}", name),
                })?;
                let idx = self.add_constant(VmSymbolTable::symbol_ref(symbol_id));
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
                Ok(())
            }
            // Synthesis-only — intrinsics are never first-class
            // values. The only sites that compile a callee as a
            // value are independent-product branches, which can
            // only contain user-visible call shapes.
            ResolvedCallee::Intrinsic(kind) => Err(CompileError {
                msg: format!(
                    "intrinsic {} cannot be used as a first-class value",
                    kind.name()
                ),
            }),
            ResolvedCallee::LocalSlot { slot, last_use, .. } => {
                self.emit_op(if last_use.0 { MOVE_LOCAL } else { LOAD_LOCAL });
                self.emit_u8(*slot as u8);
                Ok(())
            }
            ResolvedCallee::Unresolved { callee } => self.compile_expr(callee),
        }
    }

    pub(super) fn resolve_type_id(&self, name: &str) -> Option<u32> {
        self.arena.find_type_id(name)
    }

    fn resolve_fn_id_by_name(&self, name: &str) -> Option<u32> {
        self.module_scope()
            .get(name)
            .copied()
            .or_else(|| self.code_store.find(name))
    }

    /// Look up the canonical source-level name for a resolved fn
    /// identity. Mirrors what the pre-Phase-E `compile_call` did
    /// implicitly via `expr_to_dotted_name(callee)` — same dotted
    /// shape, but the input is opaque so the lookup goes through
    /// the resolver's symbol table instead of re-walking strings.
    pub(super) fn canonical_fn_name(&self, fn_id: FnId) -> Result<String, CompileError> {
        let entry = self.symbol_table.fn_entry(fn_id);
        Ok(canonical_name_from_key(&entry.key))
    }

    /// Look up the canonical source-level name for a user-defined
    /// type identity (record name or sum-type name). Used by ctor
    /// emission and pattern matching to recover the qualified
    /// `Module.Type` form the arena was registered with.
    pub(super) fn canonical_type_name(
        &self,
        type_id: crate::ir::identity::TypeId,
    ) -> Result<String, CompileError> {
        let entry = self.symbol_table.type_entry(type_id);
        let key = &entry.key;
        Ok(match key.scope_str() {
            Some(scope) => format!("{}.{}", scope, key.name),
            None => key.name.clone(),
        })
    }

    pub(super) fn compile_call(
        &mut self,
        callee: &ResolvedCallee,
        args: &[Spanned<ResolvedExpr>],
    ) -> Result<(), CompileError> {
        // 0.15 Traversal: deforestation buffer intrinsics. The synth
        // pass replaces canonical `String.join(<fn>(args, []), sep)`
        // shapes with `__buf_finalize(<fn>__buffered(args.., __buf_new(...), sep))`,
        // and `<fn>__buffered`'s body is built from `__buf_append` /
        // `__buf_append_sep_unless_first`. None of these are user-visible —
        // they only ever appear synthesized — so we recognise them via
        // [`ResolvedCallee::Intrinsic`] and emit dedicated opcodes
        // backed by `vm.buffer_pool` (a host-side `Vec<Option<String>>`).
        if let ResolvedCallee::Intrinsic(intrinsic) = callee {
            return self.compile_intrinsic_call(*intrinsic, args);
        }

        if let Some(plan) = classify_forward_call_resolved(callee, args)
            && let Some(target) = self.resolve_call_target(callee, args.len())?
        {
            for slot in plan.forward_slots {
                let ForwardSlot::Local { slot } = slot;
                self.emit_op(LOAD_LOCAL);
                self.emit_u8(slot as u8);
            }
            return self.emit_resolved_call_after_loaded_args(target, args.len(), 0);
        }

        if let Some(target) = self.resolve_call_target(callee, args.len())? {
            return self.compile_resolved_call(target, args);
        }
        // Fallback: dynamic dispatch (LocalSlot / Unresolved / Wrapper
        // mismatch). Push the callee, then args, then CALL_VALUE.
        self.compile_callee_as_value(callee)?;
        for arg in args {
            self.compile_expr(arg)?;
        }
        self.emit_op(CALL_VALUE);
        self.emit_u8(args.len() as u8);
        Ok(())
    }

    /// Compile one of the synthesised buffer / coercion intrinsics
    /// down to its dedicated opcode shape. Intrinsics are emitted by
    /// `interp_lower` / `buffer_build`; user source cannot reach
    /// this branch.
    fn compile_intrinsic_call(
        &mut self,
        intrinsic: BuiltinIntrinsic,
        args: &[Spanned<ResolvedExpr>],
    ) -> Result<(), CompileError> {
        if let Some((opcode, expected_arity)) = buffer_intrinsic_opcode(intrinsic) {
            if args.len() != expected_arity {
                return Err(CompileError {
                    msg: format!(
                        "intrinsic {} expects {} arg(s), got {}",
                        intrinsic.name(),
                        expected_arity,
                        args.len()
                    ),
                });
            }
            for arg in args {
                self.compile_expr(arg)?;
            }
            self.emit_op(opcode);
            return Ok(());
        }
        // `__to_str(x)`: coerce any value to its string repr. Used by
        // the interpolation lowering pass to produce a string before
        // a `__buf_append`. Reuses the existing CONCAT-against-empty
        // trick — CONCAT calls `NanValue::repr` on both sides, so any
        // value lowers to its display string.
        debug_assert!(matches!(intrinsic, BuiltinIntrinsic::ToStr));
        if args.len() != 1 {
            return Err(CompileError {
                msg: format!(
                    "intrinsic {} expects 1 arg, got {}",
                    intrinsic.name(),
                    args.len()
                ),
            });
        }
        self.compile_expr(&args[0])?;
        let empty_nv = NanValue::new_string_value("", self.arena);
        let empty_const = self.add_constant(empty_nv);
        self.emit_op(LOAD_CONST);
        self.emit_u16(empty_const);
        self.emit_op(CONCAT);
        Ok(())
    }

    /// Promote a [`ResolvedCallee`] to a [`CallTarget`] when the
    /// callee shape can be lowered through a direct dispatch
    /// opcode (CALL_KNOWN / WRAP / VARIANT_NEW / CALL_BUILTIN). Returns
    /// `None` for dynamic shapes that have to go through CALL_VALUE.
    fn resolve_call_target(
        &self,
        callee: &ResolvedCallee,
        argc: usize,
    ) -> Result<Option<CallTarget>, CompileError> {
        match callee {
            ResolvedCallee::Fn(fn_id) => {
                let name = self.canonical_fn_name(*fn_id)?;
                Ok(Some(match self.resolve_fn_id_by_name(&name) {
                    Some(id) => CallTarget::KnownFn(id),
                    None => CallTarget::UnknownQualified(name),
                }))
            }
            ResolvedCallee::Builtin(name) => Ok(Some(
                self.resolve_builtin_target(name)
                    .unwrap_or_else(|| CallTarget::UnknownQualified(name.clone())),
            )),
            // Intrinsics never reach this path — `compile_call`
            // short-circuits them via `compile_intrinsic_call`
            // before any plan / target classification happens.
            ResolvedCallee::Intrinsic(_) => Ok(None),
            ResolvedCallee::LocalSlot { .. } => Ok(None),
            ResolvedCallee::Unresolved { callee } => {
                // Best-effort name reconstruction so a known-shape
                // dispatch (`Module.Variant(arg)`, `Builtin.method`,
                // user fn referenced by dotted path) still lands on
                // a CALL_KNOWN-class opcode. The post-Phase-E zero-
                // unresolved invariant means well-typed input never
                // gets here, but the resolver still emits Unresolved
                // for typecheck-error recovery so we keep the
                // shape-aware fallback rather than blanket
                // CALL_VALUE.
                let dotted = match resolved_to_dotted(&callee.node) {
                    Some(name) => name,
                    None => return Ok(None),
                };
                Ok(self.resolve_dotted_call_target(&dotted, argc))
            }
        }
    }

    fn resolve_builtin_target(&self, name: &str) -> Option<CallTarget> {
        let symbol_id = self.symbols.find(name)?;
        self.symbols
            .resolve_builtin(symbol_id)
            .map(CallTarget::Builtin)
    }

    /// Last-resort path for `ResolvedCallee::Unresolved { Attr(Ident(M), n) }`:
    /// reconstruct the dotted form and ask the existing name-based
    /// dispatcher. Mirrors the pre-Phase-E `expr_to_dotted_name`-then-
    /// `classify_call_plan` flow at the very edges of the resolver's
    /// coverage (typecheck-error recovery only).
    fn resolve_dotted_call_target(&self, dotted: &str, argc: usize) -> Option<CallTarget> {
        // Wrapper / NoneValue / user variant ctor lookup via the
        // shared classifier on raw names — the resolver already
        // covers the well-typed case, this branch only runs on
        // recovery shapes.
        match dotted {
            "Result.Ok" if argc == 1 => return Some(CallTarget::Wrapper(0)),
            "Result.Err" if argc == 1 => return Some(CallTarget::Wrapper(1)),
            "Option.Some" if argc == 1 => return Some(CallTarget::Wrapper(2)),
            "Option.None" if argc == 0 => return Some(CallTarget::None_),
            _ => {}
        }

        if let Some(id) = self.resolve_fn_id_by_name(dotted) {
            return Some(CallTarget::KnownFn(id));
        }
        if let Some(target) = self.resolve_builtin_target(dotted) {
            return Some(target);
        }
        if let Some((type_name, variant_name)) = dotted.rsplit_once('.')
            && let Some(type_id) = self.resolve_type_id(type_name)
            && let Some(variant_id) = self.arena.find_variant_id(type_id, variant_name)
        {
            return Some(CallTarget::Variant(type_id, variant_id));
        }
        if dotted.contains('.') {
            Some(CallTarget::UnknownQualified(dotted.to_string()))
        } else {
            None
        }
    }

    fn emit_resolved_call_after_loaded_args(
        &mut self,
        target: CallTarget,
        argc: usize,
        owned_mask: u8,
    ) -> Result<(), CompileError> {
        match target {
            CallTarget::KnownFn(fn_id) => {
                self.emit_op(CALL_KNOWN);
                self.emit_u16(fn_id as u16);
                self.emit_u8(argc as u8);
            }
            CallTarget::Wrapper(kind) => {
                if argc == 0 {
                    self.emit_op(LOAD_UNIT);
                }
                self.emit_op(WRAP);
                self.emit_u8(kind);
            }
            CallTarget::None_ => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            CallTarget::Variant(type_id, variant_id) => {
                self.emit_op(VARIANT_NEW);
                self.emit_u16(type_id as u16);
                self.emit_u16(variant_id);
                self.emit_u8(argc as u8);
            }
            CallTarget::Builtin(builtin) => {
                self.emit_builtin_after_args(builtin, argc, owned_mask)?;
            }
            CallTarget::UnknownQualified(qualified) => {
                return Err(CompileError {
                    msg: format!("unknown builtin or namespace member: {}", qualified),
                });
            }
        }
        Ok(())
    }

    fn compile_resolved_call(
        &mut self,
        target: CallTarget,
        args: &[Spanned<ResolvedExpr>],
    ) -> Result<(), CompileError> {
        // Compute owned mask before compiling args (we need the AST).
        let arg_refs: Vec<&Spanned<ResolvedExpr>> = args.iter().collect();
        let owned_mask = self.compute_builtin_owned_mask(&arg_refs);
        for arg in args {
            self.compile_expr(arg)?;
        }
        self.emit_resolved_call_after_loaded_args(target, args.len(), owned_mask)
    }

    pub(super) fn compile_tail_call(
        &mut self,
        target: FnId,
        args: &[Spanned<ResolvedExpr>],
    ) -> Result<(), CompileError> {
        for arg in args {
            self.compile_expr(arg)?;
        }

        // Derive owned mask from last_use annotations on the tail call args.
        // Bit i set = arg i contains a last-use reference to param i.
        let owned_mask: u8 = args.iter().enumerate().take(8).fold(0u8, |mask, (i, arg)| {
            if contains_last_use_slot(&arg.node, i as u16) {
                mask | (1 << i)
            } else {
                mask
            }
        });

        let target_name = self.canonical_fn_name(target)?;
        if target_name == self.name() {
            self.emit_op(TAIL_CALL_SELF);
            self.emit_u8(args.len() as u8);
            self.emit_u8(owned_mask);
            return Ok(());
        }
        if let Some(fn_id) = self.resolve_fn_id_by_name(&target_name) {
            self.emit_op(TAIL_CALL_KNOWN);
            self.emit_u16(fn_id as u16);
            self.emit_u8(args.len() as u8);
            self.emit_u8(owned_mask);
            return Ok(());
        }
        Err(CompileError {
            msg: format!("unknown tail call target: {}", target_name),
        })
    }

    /// Compile a constructor expression (`Shape.Circle(1.0)`,
    /// `Shape.Rect(3.0, 4.0)`, `Result.Ok(42)`, `Option.None`).
    /// Mirrors the pre-Phase-E `compile_constructor` but consumes a
    /// typed [`ResolvedCtor`] instead of re-deriving the kind from a
    /// `&str` name. The arg-count discriminator is whatever the
    /// resolver lifted from the source FnCall — user variants
    /// preserve all their fields; built-in wrappers always pass a
    /// single arg (or none, for `Option.None`).
    pub(super) fn compile_ctor(
        &mut self,
        ctor: &ResolvedCtor,
        args: &[Spanned<ResolvedExpr>],
    ) -> Result<(), CompileError> {
        match ctor {
            ResolvedCtor::Builtin(BuiltinCtor::ResultOk) => {
                self.compile_constructor_arg(args.first())?;
                self.emit_op(WRAP);
                self.emit_u8(0);
                Ok(())
            }
            ResolvedCtor::Builtin(BuiltinCtor::ResultErr) => {
                self.compile_constructor_arg(args.first())?;
                self.emit_op(WRAP);
                self.emit_u8(1);
                Ok(())
            }
            ResolvedCtor::Builtin(BuiltinCtor::OptionSome) => {
                self.compile_constructor_arg(args.first())?;
                self.emit_op(WRAP);
                self.emit_u8(2);
                Ok(())
            }
            ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
                Ok(())
            }
            ResolvedCtor::User {
                type_id,
                name: variant_name,
                ..
            } => {
                let qualified_type_name = self.canonical_type_name(*type_id)?;
                if let Some(arena_type_id) = self.resolve_type_id(&qualified_type_name)
                    && let Some(variant_id) =
                        self.arena.find_variant_id(arena_type_id, variant_name)
                {
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    self.emit_op(VARIANT_NEW);
                    self.emit_u16(arena_type_id as u16);
                    self.emit_u16(variant_id);
                    self.emit_u8(args.len() as u8);
                    return Ok(());
                }
                Err(CompileError {
                    msg: format!(
                        "unknown constructor: {}.{}",
                        qualified_type_name, variant_name
                    ),
                })
            }
            ResolvedCtor::Unresolved { name } => {
                // Recovery / cross-module fallback. The resolver
                // emits `Unresolved` when its symbol table doesn't
                // know the owning type — typically a cross-module
                // ctor inside an entry whose pipeline wasn't given
                // `dep_modules`. The dotted name still routes
                // correctly through the arena (modules register
                // their types under the qualified `Module.Type`
                // alias inside `integrate_module`).
                if let Some((type_name, variant_name)) = name.rsplit_once('.')
                    && let Some(arena_type_id) = self.resolve_type_id(type_name)
                    && let Some(variant_id) =
                        self.arena.find_variant_id(arena_type_id, variant_name)
                {
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    self.emit_op(VARIANT_NEW);
                    self.emit_u16(arena_type_id as u16);
                    self.emit_u16(variant_id);
                    self.emit_u8(args.len() as u8);
                    return Ok(());
                }
                Err(CompileError {
                    msg: format!("unknown constructor: {}", name),
                })
            }
        }
    }

    fn compile_constructor_arg(
        &mut self,
        arg: Option<&Spanned<ResolvedExpr>>,
    ) -> Result<(), CompileError> {
        if let Some(a) = arg {
            self.compile_expr(a)
        } else {
            self.emit_op(LOAD_UNIT);
            Ok(())
        }
    }

    fn compile_leaf_op_kind(
        &mut self,
        kind: ResolvedLeafOpKind,
        original: &ResolvedExpr,
    ) -> Result<(), CompileError> {
        match kind {
            ResolvedLeafOpKind::FieldAccess => {
                // Re-walk to extract the inner expression structurally
                // — we know it's a top-level Attr because that's what
                // classify_leaf_op_resolved matched.
                let ResolvedExpr::Attr(obj, field) = original else {
                    return Err(CompileError {
                        msg: "leaf op shape mismatch".to_string(),
                    });
                };
                self.compile_attr(obj, field)
            }
            ResolvedLeafOpKind::MapGet => {
                let (a, b) = expect_two_args(original)?;
                self.compile_expr(a)?;
                self.compile_expr(b)?;
                self.emit_builtin_after_args(VmBuiltin::MapGet, 2, 0)?;
                Ok(())
            }
            ResolvedLeafOpKind::MapSet => {
                let (a, b, c) = expect_three_args(original)?;
                let owned_mask = self.compute_builtin_owned_mask(&[a, b, c]);
                self.compile_expr(a)?;
                self.compile_expr(b)?;
                self.compile_expr(c)?;
                self.emit_builtin_after_args(VmBuiltin::MapSet, 3, owned_mask)?;
                Ok(())
            }
            ResolvedLeafOpKind::VectorNew => {
                let (a, b) = expect_two_args(original)?;
                self.compile_expr(a)?;
                self.compile_expr(b)?;
                self.emit_builtin_after_args(VmBuiltin::VectorNew, 2, 0)?;
                Ok(())
            }
            ResolvedLeafOpKind::VectorGetOrDefaultLiteral { default_literal } => {
                // Outer call: `Option.withDefault(vector_get_call, default_literal)`.
                let (vector_get_call, _default_expr) = expect_two_args(original)?;
                let (vector, index) = expect_two_args(&vector_get_call.node)?;
                self.compile_expr(vector)?;
                self.compile_expr(index)?;
                let default_value = self.nan_literal(&default_literal);
                let const_idx = self.add_constant(default_value);
                self.emit_op(VECTOR_GET_OR);
                self.emit_u16(const_idx);
                Ok(())
            }
            ResolvedLeafOpKind::VectorSetOrDefaultSameVector => {
                // Outer call: `Option.withDefault(vector_set_call, vector)`.
                let (vector_set_call, _default_expr) = expect_two_args(original)?;
                let (vector, index, value) = expect_three_args(&vector_set_call.node)?;
                // In the fused VECTOR_SET_OR_KEEP opcode the default is
                // implicitly the same vector — it's not loaded separately,
                // so the vec appears once on the stack. In-place mutation
                // (vec_owned=1) is unsafe when the slot is alias-prone:
                // the arena entry might be shared with another live
                // binding and rewriting it would mutate that one too.
                // `ir::alias` flags such slots on `FnResolution.aliased_slots`;
                // the slow-path (clone backing items, push fresh arena
                // entry) keeps shared entries intact.
                let owned = match &vector.node {
                    ResolvedExpr::Resolved { slot, last_use, .. } => {
                        last_use.0 && !self.is_aliased_slot(*slot)
                    }
                    _ => true,
                };
                self.compile_expr(vector)?;
                self.compile_expr(index)?;
                self.compile_expr(value)?;
                self.emit_op(VECTOR_SET_OR_KEEP);
                self.emit_u8(if owned { 1 } else { 0 });
                Ok(())
            }
            ResolvedLeafOpKind::ListIndexGet => {
                let (vector_from_list_call, index) = expect_two_args(original)?;
                let list = expect_one_arg(&vector_from_list_call.node)?;
                self.compile_expr(list)?;
                self.emit_builtin_after_args(VmBuiltin::VectorFromList, 1, 0)?;
                self.compile_expr(index)?;
                self.emit_op(VECTOR_GET);
                Ok(())
            }
            ResolvedLeafOpKind::IntModOrDefaultLiteral { default_literal } => {
                let (int_mod_call, _default_expr) = expect_two_args(original)?;
                let (a, b) = expect_two_args(&int_mod_call.node)?;
                self.compile_expr(a)?;
                self.compile_expr(b)?;
                self.emit_builtin_after_args(VmBuiltin::IntMod, 2, 0)?;
                let default_value = self.nan_literal(&default_literal);
                let const_idx = self.add_constant(default_value);
                self.emit_op(LOAD_CONST);
                self.emit_u16(const_idx);
                self.emit_op(UNWRAP_RESULT_OR);
                Ok(())
            }
            ResolvedLeafOpKind::NoneValue => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
                Ok(())
            }
            ResolvedLeafOpKind::StaticRef(name) => {
                // Static function/builtin reference in value position.
                // Resolve via namespace path lookup (same as compile_attr).
                if let Some(dot) = name.rfind('.') {
                    let ns_path = &name[..dot];
                    let member = &name[dot + 1..];
                    if let Some(symbol_id) = self.symbols.resolve_namespace_path(ns_path) {
                        let idx = self.add_constant(VmSymbolTable::symbol_ref(symbol_id));
                        self.emit_op(LOAD_CONST);
                        self.emit_u16(idx);
                        let field_symbol_id = self.symbols.intern_name(member);
                        self.emit_op(RECORD_GET_NAMED);
                        self.emit_u32(field_symbol_id);
                        return Ok(());
                    }
                }
                Err(CompileError {
                    msg: format!("unresolved static reference: {}", name),
                })
            }
        }
    }

    fn emit_builtin_after_args(
        &mut self,
        builtin: VmBuiltin,
        argc: usize,
        owned_mask: u8,
    ) -> Result<(), CompileError> {
        match builtin {
            VmBuiltin::ListLen => self.emit_op(LIST_LEN),
            VmBuiltin::ListPrepend => self.emit_op(LIST_PREPEND),
            VmBuiltin::VectorGet => self.emit_op(VECTOR_GET),
            VmBuiltin::VectorSet if owned_mask != 0 => {
                // Owned path: go through CALL_BUILTIN_OWNED for take optimization
                let symbol_id = self.symbols.intern_builtin(builtin)?;
                self.emit_op(CALL_BUILTIN_OWNED);
                self.emit_u32(symbol_id);
                self.emit_u8(argc as u8);
                self.emit_u8(owned_mask);
            }
            VmBuiltin::VectorSet => self.emit_op(VECTOR_SET),
            VmBuiltin::OptionWithDefault => self.emit_op(UNWRAP_OR),
            VmBuiltin::ResultWithDefault => self.emit_op(UNWRAP_RESULT_OR),
            _ => {
                let symbol_id = self.symbols.intern_builtin(builtin)?;
                if owned_mask != 0 {
                    self.emit_op(CALL_BUILTIN_OWNED);
                    self.emit_u32(symbol_id);
                    self.emit_u8(argc as u8);
                    self.emit_u8(owned_mask);
                } else {
                    self.emit_op(CALL_BUILTIN);
                    self.emit_u32(symbol_id);
                    self.emit_u8(argc as u8);
                }
            }
        }
        Ok(())
    }

    /// Compute owned bitmask for builtin args by checking if any arg
    /// is a last-use local reference (annotated by `ir::last_use`)
    /// AND the slot is not flagged alias-prone (annotated by
    /// `ir::alias`). Both annotations live on `FnResolution` —
    /// `last_use.0` on the `Resolved` node, `aliased_slots` on the
    /// resolution itself.
    fn compute_builtin_owned_mask(&self, arg_exprs: &[&Spanned<ResolvedExpr>]) -> u8 {
        let mut mask = 0u8;
        for (i, arg) in arg_exprs.iter().enumerate().take(8) {
            if let ResolvedExpr::Resolved { slot, last_use, .. } = &arg.node
                && last_use.0
                && !self.is_aliased_slot(*slot)
            {
                mask |= 1 << i;
            }
        }
        mask
    }

    fn nan_literal(&mut self, lit: &Literal) -> NanValue {
        match lit {
            Literal::Int(i) => NanValue::new_int(*i, self.arena),
            Literal::Float(f) => NanValue::new_float(*f),
            Literal::Bool(true) => NanValue::TRUE,
            Literal::Bool(false) => NanValue::FALSE,
            Literal::Unit => NanValue::UNIT,
            Literal::Str(s) => NanValue::new_string_value(s, self.arena),
        }
    }

    pub(super) fn compile_attr(
        &mut self,
        obj: &Spanned<ResolvedExpr>,
        field: &str,
    ) -> Result<(), CompileError> {
        if let Some(path) = resolved_to_dotted(&obj.node)
            && let Some(symbol_id) = self.symbols.resolve_namespace_path(&path)
        {
            let idx = self.add_constant(VmSymbolTable::symbol_ref(symbol_id));
            self.emit_op(LOAD_CONST);
            self.emit_u16(idx);
            let field_symbol_id = self.symbols.intern_name(field);
            self.emit_op(RECORD_GET_NAMED);
            self.emit_u32(field_symbol_id);
            return Ok(());
        }

        if let Some(field_idx) = self
            .infer_record_field_idx(&obj.node, field)
            .or_else(|| self.resolve_record_field_idx(&obj.node, field))
        {
            self.compile_expr(obj)?;
            self.emit_op(RECORD_GET);
            self.emit_u8(field_idx);
            return Ok(());
        }

        self.compile_expr(obj)?;
        let field_symbol_id = self.symbols.intern_name(field);
        self.emit_op(RECORD_GET_NAMED);
        self.emit_u32(field_symbol_id);
        Ok(())
    }

    fn infer_record_field_idx(&self, obj: &ResolvedExpr, field: &str) -> Option<u8> {
        let type_name = match obj {
            ResolvedExpr::RecordCreate { type_name, .. }
            | ResolvedExpr::RecordUpdate { type_name, .. } => type_name.as_str(),
            _ => return None,
        };
        let type_id = self.resolve_type_id(type_name)?;
        let fields = self.arena.get_field_names(type_id);
        fields
            .iter()
            .position(|name| name == field)
            .map(|idx| idx as u8)
    }

    fn resolve_record_field_idx(&self, obj: &ResolvedExpr, field: &str) -> Option<u8> {
        let field_symbol_id = self.code_store.symbols.find(field)?;
        match obj {
            ResolvedExpr::Ident(type_name)
                if type_name.chars().next().is_some_and(|c| c.is_uppercase()) =>
            {
                let type_id = self.resolve_type_id(type_name)?;
                self.code_store
                    .record_field_slots
                    .get(&(type_id, field_symbol_id))
                    .copied()
            }
            _ => None,
        }
    }
}

fn canonical_name_from_key(key: &FnKey) -> String {
    key.canonical()
}

/// Check if an expression tree contains a `Resolved { slot, last_use: true }`
/// for a specific slot. Used to derive tail-call owned_mask from last_use
/// annotations instead of the old `TailCallData.owned` mechanism.
fn contains_last_use_slot(expr: &ResolvedExpr, target_slot: u16) -> bool {
    match expr {
        ResolvedExpr::Resolved { slot, last_use, .. } => *slot == target_slot && last_use.0,
        ResolvedExpr::Call(_, args) => args
            .iter()
            .any(|a| contains_last_use_slot(&a.node, target_slot)),
        ResolvedExpr::BinOp(_, left, right) => {
            contains_last_use_slot(&left.node, target_slot)
                || contains_last_use_slot(&right.node, target_slot)
        }
        ResolvedExpr::Attr(obj, _) => contains_last_use_slot(&obj.node, target_slot),
        ResolvedExpr::ErrorProp(inner) | ResolvedExpr::Neg(inner) => {
            contains_last_use_slot(&inner.node, target_slot)
        }
        ResolvedExpr::Ctor(_, args) => args
            .iter()
            .any(|a| contains_last_use_slot(&a.node, target_slot)),
        ResolvedExpr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ir::hir::ResolvedStrPart::Parsed(e) => {
                contains_last_use_slot(&e.node, target_slot)
            }
            _ => false,
        }),
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => items
            .iter()
            .any(|e| contains_last_use_slot(&e.node, target_slot)),
        ResolvedExpr::TailCall { args, .. } => args
            .iter()
            .any(|a| contains_last_use_slot(&a.node, target_slot)),
        _ => false,
    }
}

/// Owned-shape mirror of [`ResolvedLeafOp`]. The leaf classifier
/// returns borrows into the source `ResolvedExpr`; this enum
/// captures only the per-variant scalar data so the compiler can
/// re-borrow the expression tree by structure (avoiding the
/// lifetime conflict with `self.arena` mutation).
enum ResolvedLeafOpKind {
    FieldAccess,
    MapGet,
    MapSet,
    VectorNew,
    VectorSetOrDefaultSameVector,
    VectorGetOrDefaultLiteral { default_literal: Literal },
    IntModOrDefaultLiteral { default_literal: Literal },
    ListIndexGet,
    NoneValue,
    StaticRef(String),
}

fn leaf_into_owned_kind(leaf: ResolvedLeafOp<'_>) -> ResolvedLeafOpKind {
    match leaf {
        ResolvedLeafOp::FieldAccess { .. } => ResolvedLeafOpKind::FieldAccess,
        ResolvedLeafOp::MapGet { .. } => ResolvedLeafOpKind::MapGet,
        ResolvedLeafOp::MapSet { .. } => ResolvedLeafOpKind::MapSet,
        ResolvedLeafOp::VectorNew { .. } => ResolvedLeafOpKind::VectorNew,
        ResolvedLeafOp::VectorSetOrDefaultSameVector { .. } => {
            ResolvedLeafOpKind::VectorSetOrDefaultSameVector
        }
        ResolvedLeafOp::VectorGetOrDefaultLiteral {
            default_literal, ..
        } => ResolvedLeafOpKind::VectorGetOrDefaultLiteral {
            default_literal: default_literal.clone(),
        },
        ResolvedLeafOp::IntModOrDefaultLiteral {
            default_literal, ..
        } => ResolvedLeafOpKind::IntModOrDefaultLiteral {
            default_literal: default_literal.clone(),
        },
        ResolvedLeafOp::ListIndexGet { .. } => ResolvedLeafOpKind::ListIndexGet,
        ResolvedLeafOp::NoneValue => ResolvedLeafOpKind::NoneValue,
        ResolvedLeafOp::VariantConstructor { .. } => {
            // The leaf classifier currently does not lift nullary
            // user variant ctors to LeafOp (the resolver routes them
            // through `ResolvedExpr::Ctor` instead). Treat as a
            // "shouldn't happen but be safe" fallback so the surrounding
            // tree walks: we pretend it's a NoneValue placeholder,
            // and the caller's expression-shape check will surface
            // a CompileError if it tries to lower it. In practice
            // this branch is unreachable.
            ResolvedLeafOpKind::NoneValue
        }
        ResolvedLeafOp::StaticRef(name) => ResolvedLeafOpKind::StaticRef(name),
    }
}

fn expect_one_arg(expr: &ResolvedExpr) -> Result<&Spanned<ResolvedExpr>, CompileError> {
    let ResolvedExpr::Call(_, args) = expr else {
        return Err(CompileError {
            msg: "leaf-op inner call shape mismatch".to_string(),
        });
    };
    if args.len() != 1 {
        return Err(CompileError {
            msg: "leaf-op arity mismatch".to_string(),
        });
    }
    Ok(&args[0])
}

fn expect_two_args(expr: &ResolvedExpr) -> Result<SpannedResolvedPair<'_>, CompileError> {
    let ResolvedExpr::Call(_, args) = expr else {
        return Err(CompileError {
            msg: "leaf-op outer call shape mismatch".to_string(),
        });
    };
    if args.len() != 2 {
        return Err(CompileError {
            msg: "leaf-op arity mismatch".to_string(),
        });
    }
    Ok((&args[0], &args[1]))
}

fn expect_three_args(expr: &ResolvedExpr) -> Result<SpannedResolvedTriple<'_>, CompileError> {
    let ResolvedExpr::Call(_, args) = expr else {
        return Err(CompileError {
            msg: "leaf-op outer call shape mismatch".to_string(),
        });
    };
    if args.len() != 3 {
        return Err(CompileError {
            msg: "leaf-op arity mismatch".to_string(),
        });
    }
    Ok((&args[0], &args[1], &args[2]))
}
