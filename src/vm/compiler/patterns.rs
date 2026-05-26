use super::resolved_classify::{
    ResolvedBoolSubjectPlan, classify_bool_subject_plan_resolved,
    classify_dispatch_pattern_resolved, classify_match_dispatch_plan_resolved,
};
use super::{CompileError, FnCompiler};
use crate::ast::{Literal, Spanned};
use crate::ir::hir::{BuiltinCtor, ResolvedCtor, ResolvedExpr, ResolvedMatchArm, ResolvedPattern};
use crate::ir::{
    BoolCompareOp, BoolMatchShape, DispatchArmPlan, DispatchBindingPlan, DispatchLiteral,
    DispatchTableShape, MatchDispatchPlan, SemanticDispatchPattern, WrapperKind,
};
use crate::nan_value::NanValue;
use crate::vm::opcode::*;

const QNAN: u64 = 0x7FFC_0000_0000_0000;
const TAG_SHIFT: u32 = 46;
const TAG_SOME: u64 = 4;
const TAG_OK: u64 = 6;
const TAG_ERR: u64 = 7;

const DISPATCH_KIND_EXACT: u8 = 0;
const DISPATCH_KIND_TAG: u8 = 1;
const DISPATCH_KIND_STRING: u8 = 2;

fn wrapper_tag_kind(kind: WrapperKind) -> u8 {
    match kind {
        WrapperKind::ResultOk => 0,
        WrapperKind::ResultErr => 1,
        WrapperKind::OptionSome => 2,
    }
}

fn wrapper_tag_bits(kind: WrapperKind) -> u64 {
    let tag = match kind {
        WrapperKind::ResultOk => TAG_OK,
        WrapperKind::ResultErr => TAG_ERR,
        WrapperKind::OptionSome => TAG_SOME,
    };
    QNAN | (tag << TAG_SHIFT)
}

/// Info about a pattern that can be dispatched via MATCH_DISPATCH.
struct DispatchableArm {
    kind: u8,      // DISPATCH_KIND_EXACT or DISPATCH_KIND_TAG
    expected: u64, // bits to compare
    arm_index: usize,
}

impl<'a> FnCompiler<'a> {
    fn compile_unwrap_pattern(&mut self, kind: u8, binding: Option<&String>) -> Vec<usize> {
        self.emit_op(MATCH_UNWRAP);
        self.emit_u8(kind);
        let fail_patch = self.code().len();
        self.emit_i16(0);
        if let Some(binding) = binding {
            self.dup_and_bind_top_to_local(binding);
        }
        vec![fail_patch]
    }

    fn compile_extracted_subpattern<F>(
        &mut self,
        emit_subject: F,
        pattern: &ResolvedPattern,
    ) -> Result<Vec<usize>, CompileError>
    where
        F: FnOnce(&mut Self),
    {
        emit_subject(self);
        let inner_fail_patches = self.compile_pattern(pattern)?;
        self.emit_op(POP);

        if inner_fail_patches.is_empty() {
            return Ok(Vec::new());
        }

        let success_skip_cleanup = self.emit_jump(JUMP);
        let cleanup_target = self.offset();
        for patch in inner_fail_patches {
            self.patch_jump_to(patch, cleanup_target);
        }
        self.emit_op(POP);
        let outer_fail = self.emit_jump(JUMP);
        self.patch_jump(success_skip_cleanup);
        Ok(vec![outer_fail])
    }

    fn compile_tuple_pattern(
        &mut self,
        patterns: &[ResolvedPattern],
    ) -> Result<Vec<usize>, CompileError> {
        self.emit_op(MATCH_TUPLE);
        self.emit_u8(patterns.len() as u8);
        let tuple_fail = self.code().len();
        self.emit_i16(0);

        let mut fail_patches = vec![tuple_fail];
        for (i, pattern) in patterns.iter().enumerate() {
            let mut nested = self.compile_extracted_subpattern(
                |this| {
                    this.emit_op(EXTRACT_TUPLE_ITEM);
                    this.emit_u8(i as u8);
                },
                pattern,
            )?;
            fail_patches.append(&mut nested);
        }
        Ok(fail_patches)
    }

    /// Try to classify a pattern for MATCH_DISPATCH.
    fn classify_dispatchable(
        &mut self,
        pattern: &ResolvedPattern,
        arm_index: usize,
    ) -> Option<DispatchableArm> {
        match classify_dispatch_pattern_resolved(pattern)? {
            SemanticDispatchPattern::Literal(lit) => {
                let (kind, bits) = match lit {
                    DispatchLiteral::Int(i) => {
                        (DISPATCH_KIND_EXACT, NanValue::new_int(i, self.arena).bits())
                    }
                    DispatchLiteral::Float(f) => {
                        let value = f.parse::<f64>().ok()?;
                        (DISPATCH_KIND_EXACT, NanValue::new_float(value).bits())
                    }
                    DispatchLiteral::Bool(b) => (DISPATCH_KIND_EXACT, NanValue::new_bool(b).bits()),
                    DispatchLiteral::Str(s) => (
                        DISPATCH_KIND_STRING,
                        NanValue::new_string_value(&s, self.arena).bits(),
                    ),
                    DispatchLiteral::Unit => (DISPATCH_KIND_EXACT, NanValue::UNIT.bits()),
                };
                Some(DispatchableArm {
                    kind,
                    expected: bits,
                    arm_index,
                })
            }
            SemanticDispatchPattern::EmptyList => Some(DispatchableArm {
                kind: DISPATCH_KIND_EXACT,
                expected: NanValue::EMPTY_LIST.bits(),
                arm_index,
            }),
            SemanticDispatchPattern::NoneValue => Some(DispatchableArm {
                kind: DISPATCH_KIND_EXACT,
                expected: NanValue::NONE.bits(),
                arm_index,
            }),
            SemanticDispatchPattern::WrapperTag(kind) => Some(DispatchableArm {
                kind: DISPATCH_KIND_TAG,
                expected: wrapper_tag_bits(kind),
                arm_index,
            }),
        }
    }

    /// Emit the arm-body prologue for a dispatched arm.
    /// Subject is on TOS. For tag-match arms with bindings,
    /// unwraps inner value and binds it. Then pops subject.
    fn emit_dispatch_arm_prologue(&mut self, entry: &DispatchArmPlan) {
        if let (
            SemanticDispatchPattern::WrapperTag(kind),
            DispatchBindingPlan::WrapperPayload(name),
        ) = (&entry.pattern, &entry.binding)
        {
            // MATCH_UNWRAP replaces TOS with inner; offset 0 = no-op (already matched).
            self.emit_op(MATCH_UNWRAP);
            self.emit_u8(wrapper_tag_kind(*kind));
            self.emit_i16(0);
            self.dup_and_bind_top_to_local(name);
        }
    }

    /// Try to evaluate an expression to a compile-time constant NanValue.
    fn try_const_expr(&mut self, expr: &ResolvedExpr) -> Option<u64> {
        match expr {
            ResolvedExpr::Literal(lit) => {
                let nv = match lit {
                    Literal::Int(i) => NanValue::new_int(*i, self.arena),
                    Literal::Float(f) => NanValue::new_float(*f),
                    Literal::Bool(b) => NanValue::new_bool(*b),
                    Literal::Unit => NanValue::UNIT,
                    Literal::Str(s) => NanValue::new_string_value(s, self.arena),
                };
                Some(nv.bits())
            }
            _ => None,
        }
    }

    /// Unconditionally extract bindings from a constructor pattern (last arm, exhaustive).
    /// Subject is on TOS.  For Result.Ok/Err/Option.Some: unwrap inner + bind.
    /// For user variants: extract fields + bind.
    fn emit_constructor_bindings_unconditional(
        &mut self,
        ctor: &ResolvedCtor,
        bindings: &[String],
    ) -> Result<(), CompileError> {
        match ctor {
            ResolvedCtor::Builtin(BuiltinCtor::ResultOk) if !bindings.is_empty() => {
                self.emit_op(MATCH_UNWRAP);
                self.emit_u8(wrapper_tag_kind(WrapperKind::ResultOk));
                self.emit_i16(0); // no-fail (we know it matches)
                self.dup_and_bind_top_to_local(&bindings[0]);
            }
            ResolvedCtor::Builtin(BuiltinCtor::ResultErr) if !bindings.is_empty() => {
                self.emit_op(MATCH_UNWRAP);
                self.emit_u8(wrapper_tag_kind(WrapperKind::ResultErr));
                self.emit_i16(0);
                self.dup_and_bind_top_to_local(&bindings[0]);
            }
            ResolvedCtor::Builtin(BuiltinCtor::OptionSome) if !bindings.is_empty() => {
                self.emit_op(MATCH_UNWRAP);
                self.emit_u8(wrapper_tag_kind(WrapperKind::OptionSome));
                self.emit_i16(0);
                self.dup_and_bind_top_to_local(&bindings[0]);
            }
            ResolvedCtor::Builtin(_) => {}
            ResolvedCtor::User { .. } | ResolvedCtor::Unresolved { .. } => {
                // User variant — or a cross-module ctor the resolver
                // couldn't classify against the entry's symbol table
                // (treated as a user variant on the arena fallback
                // path). Extract fields unconditionally; the exhaustive
                // match guarantee covers correctness.
                for (i, b) in bindings.iter().enumerate() {
                    self.emit_op(EXTRACT_FIELD);
                    self.emit_u8(i as u8);
                    self.bind_top_to_local(b);
                }
            }
        }
        Ok(())
    }

    pub(super) fn compile_match(
        &mut self,
        subject: &Spanned<ResolvedExpr>,
        arms: &[ResolvedMatchArm],
    ) -> Result<(), CompileError> {
        if let Some(plan) = classify_match_dispatch_plan_resolved(arms) {
            match plan {
                MatchDispatchPlan::Bool(shape) => {
                    self.compile_bool_match_with_shape(subject, arms, shape)?;
                    return Ok(());
                }
                MatchDispatchPlan::Table(shape) => {
                    if let Some(result) =
                        self.try_compile_match_dispatch_with_shape(subject, arms, &shape)?
                    {
                        return Ok(result);
                    }
                }
                MatchDispatchPlan::List(_) => {}
            }
        }

        // --- Fallback: original linear match compilation ---
        self.compile_expr(subject)?;

        let mut end_jumps = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;
            let saved = self.install_arm_slots(arm);

            // Match is exhaustive — last arm always matches, skip pattern check.
            let fail_patches = if is_last {
                // Bind if needed (Ident pattern), otherwise treat as wildcard.
                match &arm.pattern {
                    ResolvedPattern::Ident(name) => self.dup_and_bind_top_to_local(name),
                    ResolvedPattern::Ctor(ctor, bindings) => {
                        // Last arm constructor: bindings still need extracting.
                        self.emit_constructor_bindings_unconditional(ctor, bindings)?;
                    }
                    ResolvedPattern::Cons(head, tail) => {
                        self.emit_op(DUP);
                        self.emit_op(LIST_HEAD_TAIL);
                        self.bind_top_to_local(head);
                        self.bind_top_to_local(tail);
                    }
                    ResolvedPattern::Tuple(patterns) => {
                        for (idx, pat) in patterns.iter().enumerate() {
                            self.emit_op(EXTRACT_TUPLE_ITEM);
                            self.emit_u8(idx as u8);
                            if let ResolvedPattern::Ident(name) = pat {
                                self.bind_top_to_local(name);
                            } else {
                                self.emit_op(POP);
                            }
                        }
                    }
                    _ => {}
                }
                Vec::new()
            } else {
                match &arm.pattern {
                    ResolvedPattern::Wildcard => Vec::new(),
                    ResolvedPattern::Ident(name) => {
                        self.dup_and_bind_top_to_local(name);
                        Vec::new()
                    }
                    pat => self.compile_pattern(pat)?,
                }
            };

            self.emit_op(POP);
            self.compile_expr(&arm.body)?;
            self.restore_local_slots(saved);

            if !is_last {
                end_jumps.push(self.emit_jump(JUMP));
                if !fail_patches.is_empty() {
                    let fail_cleanup = self.offset();
                    for patch in fail_patches {
                        self.patch_jump_to(patch, fail_cleanup);
                    }
                }
            }
        }

        for patch in end_jumps {
            self.patch_jump(patch);
        }

        Ok(())
    }

    /// Compile `match <expr>: true → A, false → B` as JUMP_IF_FALSE.
    /// Avoids MATCH_DISPATCH overhead for the most common Aver branch pattern.
    fn compile_bool_match_with_shape(
        &mut self,
        subject: &Spanned<ResolvedExpr>,
        arms: &[ResolvedMatchArm],
        shape: BoolMatchShape,
    ) -> Result<(), CompileError> {
        let true_body = &arms[shape.true_arm_index].body;
        let false_body = &arms[shape.false_arm_index].body;

        if let ResolvedBoolSubjectPlan::Compare {
            lhs,
            rhs,
            op,
            invert,
        } = classify_bool_subject_plan_resolved(&subject.node)
        {
            // Pick a typed compare opcode when both operands resolve
            // to the same primitive — bypasses `eq_in` / `compare_lt`
            // tag dispatch on the hot `match X > Y { … }` shape that
            // dominates numeric loops (Mandelbrot inner step etc.).
            let both_int = matches!(
                (lhs.ty(), rhs.ty()),
                (Some(crate::ast::Type::Int), Some(crate::ast::Type::Int))
            );
            let both_float = matches!(
                (lhs.ty(), rhs.ty()),
                (Some(crate::ast::Type::Float), Some(crate::ast::Type::Float))
            );
            self.compile_expr(lhs)?;
            self.compile_expr(rhs)?;
            self.emit_op(match op {
                BoolCompareOp::Eq => {
                    if both_int {
                        EQ_INT
                    } else {
                        EQ
                    }
                }
                BoolCompareOp::Lt => {
                    if both_int {
                        LT_INT
                    } else if both_float {
                        LT_FLOAT
                    } else {
                        LT
                    }
                }
                BoolCompareOp::Gt => {
                    if both_int {
                        GT_INT
                    } else if both_float {
                        GT_FLOAT
                    } else {
                        GT
                    }
                }
            });

            if invert {
                let true_jump = self.emit_jump(JUMP_IF_FALSE);
                self.compile_expr(false_body)?;
                let end_jump = self.emit_jump(JUMP);
                self.patch_jump(true_jump);
                self.compile_expr(true_body)?;
                self.patch_jump(end_jump);
            } else {
                let false_jump = self.emit_jump(JUMP_IF_FALSE);
                self.compile_expr(true_body)?;
                let end_jump = self.emit_jump(JUMP);
                self.patch_jump(false_jump);
                self.compile_expr(false_body)?;
                self.patch_jump(end_jump);
            }
            return Ok(());
        }

        // Normal path: subject, JUMP_IF_FALSE → false_body, true_body, JUMP → end, false_body
        self.compile_expr(subject)?;
        let false_jump = self.emit_jump(JUMP_IF_FALSE);
        self.compile_expr(true_body)?;
        let end_jump = self.emit_jump(JUMP);
        self.patch_jump(false_jump);
        self.compile_expr(false_body)?;
        self.patch_jump(end_jump);

        Ok(())
    }

    /// Try to compile a match as a MATCH_DISPATCH table from a shared IR shape.
    fn try_compile_match_dispatch_with_shape(
        &mut self,
        subject: &Spanned<ResolvedExpr>,
        arms: &[ResolvedMatchArm],
        shape: &DispatchTableShape,
    ) -> Result<Option<()>, CompileError> {
        if shape.entries.len() > 255 {
            return Ok(None);
        }

        let mut entries = Vec::new();
        for entry in &shape.entries {
            if let Some(lowered) =
                self.classify_dispatchable(&arms[entry.arm_index].pattern, entry.arm_index)
            {
                entries.push(lowered);
            } else {
                return Ok(None);
            }
        }

        let has_default = shape.default_arm.is_some();

        // --- Check if ALL dispatchable arms have const bodies (no bindings) ---
        let all_const = entries.iter().all(|e| {
            let arm = &arms[e.arm_index];
            // Must be exact match (not tag prefix — those need unwrap/bind).
            (e.kind == DISPATCH_KIND_EXACT || e.kind == DISPATCH_KIND_STRING)
                && self.try_const_expr(&arm.body.node).is_some()
        });

        if all_const {
            return self.emit_match_dispatch_const(
                &entries,
                arms,
                subject,
                shape.default_arm.as_ref().map(|arm| arm.arm_index),
            );
        }

        // --- Emit MATCH_DISPATCH (jump-based) ---
        self.compile_expr(subject)?;

        self.emit_op(MATCH_DISPATCH);
        self.emit_u8(entries.len() as u8);
        let default_offset_patch = self.code().len();
        self.emit_i16(0); // default_offset — patched later

        // Emit table entries with placeholder offsets.
        let mut entry_offset_patches = Vec::new();
        for entry in &entries {
            self.emit_u8(entry.kind);
            self.emit_u64(entry.expected);
            entry_offset_patches.push(self.code().len());
            self.emit_i16(0); // offset — patched later
        }

        let table_end = self.offset(); // all offsets relative to here

        // Emit arm bodies.
        let mut end_jumps = Vec::new();

        for (table_idx, (entry, plan_entry)) in entries.iter().zip(shape.entries.iter()).enumerate()
        {
            let arm = &arms[entry.arm_index];

            // Patch this entry's offset to point here.
            let arm_start = self.offset();
            let rel = (arm_start as isize - table_end as isize) as i16;
            let bytes = (rel as u16).to_be_bytes();
            self.code_mut()[entry_offset_patches[table_idx]] = bytes[0];
            self.code_mut()[entry_offset_patches[table_idx] + 1] = bytes[1];

            let saved = self.install_arm_slots(arm);

            // Prologue: unwrap/bind for tag-match arms.
            self.emit_dispatch_arm_prologue(plan_entry);

            // Pop subject, compile body.
            self.emit_op(POP);
            self.compile_expr(&arm.body)?;
            self.restore_local_slots(saved);

            end_jumps.push(self.emit_jump(JUMP));
        }

        // Default arm (wildcard/ident or exhaustive fallthrough).
        let default_start = self.offset();
        let default_rel = (default_start as isize - table_end as isize) as i16;
        let default_bytes = (default_rel as u16).to_be_bytes();
        self.code_mut()[default_offset_patch] = default_bytes[0];
        self.code_mut()[default_offset_patch + 1] = default_bytes[1];

        if has_default {
            let default_plan = shape.default_arm.as_ref().unwrap();
            let default_arm = &arms[default_plan.arm_index];
            let saved = self.install_arm_slots(default_arm);
            if let Some(name) = &default_plan.binding_name {
                self.dup_and_bind_top_to_local(name);
            }
            self.emit_op(POP);
            self.compile_expr(&default_arm.body)?;
            self.restore_local_slots(saved);
        } else {
            // Match is exhaustive by Aver's type system — no MATCH_FAIL needed.
        }

        for patch in end_jumps {
            self.patch_jump(patch);
        }

        Ok(Some(()))
    }

    /// Emit MATCH_DISPATCH_CONST — all dispatchable entries have inline const results.
    fn emit_match_dispatch_const(
        &mut self,
        entries: &[DispatchableArm],
        arms: &[ResolvedMatchArm],
        subject: &Spanned<ResolvedExpr>,
        default_arm_index: Option<usize>,
    ) -> Result<Option<()>, CompileError> {
        self.compile_expr(subject)?;
        let has_default = default_arm_index.is_some();

        self.emit_op(MATCH_DISPATCH_CONST);
        self.emit_u8(entries.len() as u8);
        let default_offset_patch = self.code().len();
        self.emit_i16(0); // default_offset — patched later

        // Emit table entries with inline results.
        for entry in entries {
            let arm = &arms[entry.arm_index];
            let result_bits = self.try_const_expr(&arm.body.node).unwrap();
            self.emit_u8(entry.kind);
            self.emit_u64(entry.expected);
            self.emit_u64(result_bits);
        }

        let table_end = self.offset();

        // On hit: opcode pushes result and ip lands here.
        // Emit a JUMP to skip past the default arm body.
        let hit_skip_jump = if has_default {
            Some(self.emit_jump(JUMP))
        } else {
            None
        };

        // Default arm starts here — patch offset so miss lands after the JUMP.
        let default_start = self.offset();
        let default_rel = (default_start as isize - table_end as isize) as i16;
        let default_bytes = (default_rel as u16).to_be_bytes();
        self.code_mut()[default_offset_patch] = default_bytes[0];
        self.code_mut()[default_offset_patch + 1] = default_bytes[1];

        if has_default {
            // Default arm body — subject was popped by opcode on miss,
            // then pushed back. Compile normally.
            let default_arm = &arms[default_arm_index.unwrap()];
            let saved = self.install_arm_slots(default_arm);
            if let ResolvedPattern::Ident(name) = &default_arm.pattern {
                self.dup_and_bind_top_to_local(name);
            }
            self.emit_op(POP);
            self.compile_expr(&default_arm.body)?;
            self.restore_local_slots(saved);
        }

        // Patch the hit-skip JUMP to land here (after the default body).
        if let Some(patch) = hit_skip_jump {
            self.patch_jump(patch);
        }

        Ok(Some(()))
    }

    /// Compile a pattern. Subject is on top of stack (peeked, not consumed).
    /// Returns a Vec of fail-jump patch positions.
    fn compile_pattern(&mut self, pattern: &ResolvedPattern) -> Result<Vec<usize>, CompileError> {
        match pattern {
            ResolvedPattern::Wildcard => Ok(Vec::new()),
            ResolvedPattern::Ident(name) => {
                self.dup_and_bind_top_to_local(name);
                Ok(Vec::new())
            }
            ResolvedPattern::Literal(lit) => {
                if let crate::ast::Literal::Int(v) = lit {
                    // Fused: peek + i64-compare + branch in one
                    // dispatch. Replaces DUP + LOAD_CONST + EQ +
                    // JUMP_IF_FALSE — typecheck guarantees the subject
                    // is `Int` (a `Pattern::Literal(Int)` arm only
                    // type-checks against an `Int` subject), so the
                    // peek-and-compare is sound. Big win on
                    // `match n { 0 -> …; _ -> … }` recursion shape.
                    self.emit_op(MATCH_INT_LITERAL);
                    self.emit_i64(*v);
                    let patch = self.code().len();
                    self.emit_i16(0);
                    return Ok(vec![patch]);
                }
                self.emit_op(DUP);
                self.compile_literal(lit)?;
                self.emit_op(EQ);
                let patch = self.emit_jump(JUMP_IF_FALSE);
                Ok(vec![patch])
            }
            ResolvedPattern::EmptyList => {
                self.emit_op(MATCH_NIL);
                let patch = self.code().len();
                self.emit_i16(0);
                Ok(vec![patch])
            }
            ResolvedPattern::Cons(head, tail) => {
                self.emit_op(MATCH_CONS);
                let fail_patch = self.code().len();
                self.emit_i16(0);

                self.emit_op(DUP);
                self.emit_op(LIST_HEAD_TAIL);
                self.bind_top_to_local(head);
                self.bind_top_to_local(tail);

                Ok(vec![fail_patch])
            }
            ResolvedPattern::Ctor(ctor, bindings) => {
                self.compile_constructor_pattern(ctor, bindings)
            }
            ResolvedPattern::Tuple(patterns) => self.compile_tuple_pattern(patterns),
        }
    }

    fn compile_constructor_pattern(
        &mut self,
        ctor: &ResolvedCtor,
        bindings: &[String],
    ) -> Result<Vec<usize>, CompileError> {
        match ctor {
            ResolvedCtor::Builtin(BuiltinCtor::ResultOk) => Ok(self
                .compile_unwrap_pattern(wrapper_tag_kind(WrapperKind::ResultOk), bindings.first())),
            ResolvedCtor::Builtin(BuiltinCtor::ResultErr) => Ok(self.compile_unwrap_pattern(
                wrapper_tag_kind(WrapperKind::ResultErr),
                bindings.first(),
            )),
            ResolvedCtor::Builtin(BuiltinCtor::OptionSome) => Ok(self.compile_unwrap_pattern(
                wrapper_tag_kind(WrapperKind::OptionSome),
                bindings.first(),
            )),
            ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => {
                self.emit_op(DUP);
                let none_const = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(none_const);
                self.emit_op(EQ);
                let fail_patch = self.emit_jump(JUMP_IF_FALSE);
                Ok(vec![fail_patch])
            }
            ResolvedCtor::User {
                type_id,
                name: variant,
                ..
            } => {
                let qualified_type_name = self.canonical_type_name(*type_id)?;
                if let Some(arena_type_id) = self.resolve_type_id(&qualified_type_name)
                    && let Some(variant_id) = self.arena.find_variant_id(arena_type_id, variant)
                    && let Some(ctor_id) = self.arena.find_ctor_id(arena_type_id, variant_id)
                {
                    if ctor_id > u16::MAX as u32 {
                        return Err(CompileError {
                            msg: format!(
                                "constructor id too large for VM pattern match: {}.{}",
                                qualified_type_name, variant
                            ),
                        });
                    }
                    let mut patches = Vec::new();
                    self.emit_op(MATCH_VARIANT);
                    self.emit_u16(ctor_id as u16);
                    let variant_fail = self.code().len();
                    self.emit_i16(0);
                    patches.push(variant_fail);

                    for (i, b) in bindings.iter().enumerate() {
                        self.emit_op(EXTRACT_FIELD);
                        self.emit_u8(i as u8);
                        self.bind_top_to_local(b);
                    }

                    return Ok(patches);
                }

                Err(CompileError {
                    msg: format!(
                        "unknown constructor pattern: {}.{}",
                        qualified_type_name, variant
                    ),
                })
            }
            ResolvedCtor::Unresolved { name } => {
                // Recovery / cross-module fallback — same shape as
                // `compile_ctor`'s Unresolved branch. The resolver
                // emits Unresolved for ctor names whose owning type
                // isn't in the (entry-only) symbol table; the dep's
                // qualified arena alias covers the dispatch.
                if let Some((type_name, variant_name)) = name.rsplit_once('.')
                    && let Some(arena_type_id) = self.resolve_type_id(type_name)
                    && let Some(variant_id) =
                        self.arena.find_variant_id(arena_type_id, variant_name)
                    && let Some(ctor_id) = self.arena.find_ctor_id(arena_type_id, variant_id)
                {
                    if ctor_id > u16::MAX as u32 {
                        return Err(CompileError {
                            msg: format!("constructor id too large for VM pattern match: {}", name),
                        });
                    }
                    let mut patches = Vec::new();
                    self.emit_op(MATCH_VARIANT);
                    self.emit_u16(ctor_id as u16);
                    let variant_fail = self.code().len();
                    self.emit_i16(0);
                    patches.push(variant_fail);

                    for (i, b) in bindings.iter().enumerate() {
                        self.emit_op(EXTRACT_FIELD);
                        self.emit_u8(i as u8);
                        self.bind_top_to_local(b);
                    }

                    return Ok(patches);
                }
                Err(CompileError {
                    msg: format!("unknown constructor pattern: {}", name),
                })
            }
        }
    }
}
