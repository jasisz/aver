use super::{CompileError, FnCompiler};
use crate::ast::{Expr, Literal, MatchArm, Pattern};
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
        let fail_patch = self.code.len();
        self.emit_i16(0);
        if let Some(binding) = binding {
            self.dup_and_bind_top_to_local(binding);
        }
        vec![fail_patch]
    }

    fn compile_extracted_subpattern<F>(
        &mut self,
        emit_subject: F,
        pattern: &Pattern,
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

    fn compile_tuple_pattern(&mut self, patterns: &[Pattern]) -> Result<Vec<usize>, CompileError> {
        self.emit_op(MATCH_TUPLE);
        self.emit_u8(patterns.len() as u8);
        let tuple_fail = self.code.len();
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
        pattern: &Pattern,
        arm_index: usize,
    ) -> Option<DispatchableArm> {
        match pattern {
            Pattern::Literal(lit) => {
                let (kind, bits) = match lit {
                    Literal::Int(i) => (
                        DISPATCH_KIND_EXACT,
                        NanValue::new_int(*i, self.arena).bits(),
                    ),
                    Literal::Float(f) => (DISPATCH_KIND_EXACT, NanValue::new_float(*f).bits()),
                    Literal::Bool(b) => (DISPATCH_KIND_EXACT, NanValue::new_bool(*b).bits()),
                    Literal::Unit => (DISPATCH_KIND_EXACT, NanValue::UNIT.bits()),
                    Literal::Str(s) => (
                        DISPATCH_KIND_STRING,
                        NanValue::new_string_value(s, self.arena).bits(),
                    ),
                };
                Some(DispatchableArm {
                    kind,
                    expected: bits,
                    arm_index,
                })
            }
            Pattern::EmptyList => Some(DispatchableArm {
                kind: DISPATCH_KIND_EXACT,
                expected: NanValue::EMPTY_LIST.bits(),
                arm_index,
            }),
            Pattern::Constructor(name, bindings) => match name.as_str() {
                "Option.None" if bindings.is_empty() => Some(DispatchableArm {
                    kind: DISPATCH_KIND_EXACT,
                    expected: NanValue::NONE.bits(),
                    arm_index,
                }),
                "Result.Ok" if bindings.len() <= 1 => Some(DispatchableArm {
                    kind: DISPATCH_KIND_TAG,
                    expected: QNAN | (TAG_OK << TAG_SHIFT),
                    arm_index,
                }),
                "Result.Err" if bindings.len() <= 1 => Some(DispatchableArm {
                    kind: DISPATCH_KIND_TAG,
                    expected: QNAN | (TAG_ERR << TAG_SHIFT),
                    arm_index,
                }),
                "Option.Some" if bindings.len() <= 1 => Some(DispatchableArm {
                    kind: DISPATCH_KIND_TAG,
                    expected: QNAN | (TAG_SOME << TAG_SHIFT),
                    arm_index,
                }),
                _ => None,
            },
            _ => None,
        }
    }

    /// Emit the arm-body prologue for a dispatched arm.
    /// Subject is on TOS. For tag-match arms with bindings,
    /// unwraps inner value and binds it. Then pops subject.
    fn emit_dispatch_arm_prologue(&mut self, pattern: &Pattern) {
        if let Pattern::Constructor(name, bindings) = pattern
            && matches!(name.as_str(), "Result.Ok" | "Result.Err" | "Option.Some")
            && !bindings.is_empty()
        {
            let kind = match name.as_str() {
                "Result.Ok" => 0,
                "Result.Err" => 1,
                _ => 2,
            };
            // MATCH_UNWRAP replaces TOS with inner; offset 0 = no-op (already matched).
            self.emit_op(MATCH_UNWRAP);
            self.emit_u8(kind);
            self.emit_i16(0);
            self.dup_and_bind_top_to_local(&bindings[0]);
        }
    }

    /// Try to evaluate an expression to a compile-time constant NanValue.
    fn try_const_expr(&mut self, expr: &Expr) -> Option<u64> {
        match expr {
            Expr::Literal(lit) => {
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
        name: &str,
        bindings: &[String],
    ) -> Result<(), CompileError> {
        match name {
            "Result.Ok" | "Result.Err" | "Option.Some" if !bindings.is_empty() => {
                let kind = match name {
                    "Result.Ok" => 0,
                    "Result.Err" => 1,
                    _ => 2,
                };
                self.emit_op(MATCH_UNWRAP);
                self.emit_u8(kind);
                self.emit_i16(0); // no-fail (we know it matches)
                self.dup_and_bind_top_to_local(&bindings[0]);
            }
            "Option.None" => {} // no bindings to extract
            _ => {
                // User variant: extract fields unconditionally.
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
        subject: &Expr,
        arms: &[MatchArm],
        line: usize,
    ) -> Result<(), CompileError> {
        if let Some((list_expr, index_expr, some_binding, some_body, none_body)) =
            self.try_match_list_get_arms(subject, arms)
        {
            return self.compile_list_get_match(
                list_expr,
                index_expr,
                some_binding,
                some_body,
                none_body,
            );
        }

        // --- Try bool match → JUMP_IF_FALSE optimization ---
        if let Some(result) = self.try_compile_bool_match(subject, arms)? {
            return Ok(result);
        }

        // --- Try MATCH_DISPATCH optimization ---
        if let Some(result) = self.try_compile_match_dispatch(subject, arms, line)? {
            return Ok(result);
        }

        // --- Fallback: original linear match compilation ---
        self.compile_expr(subject)?;

        let mut end_jumps = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;

            // Match is exhaustive — last arm always matches, skip pattern check.
            let fail_patches = if is_last {
                // Bind if needed (Ident pattern), otherwise treat as wildcard.
                if let Pattern::Ident(name) = &arm.pattern {
                    self.dup_and_bind_top_to_local(name);
                } else if let Pattern::Constructor(name, bindings) = &arm.pattern {
                    // Last arm constructor: bindings still need extracting.
                    self.emit_constructor_bindings_unconditional(name, bindings)?;
                } else if let Pattern::Cons(head, tail) = &arm.pattern {
                    self.emit_op(DUP);
                    self.emit_op(LIST_HEAD_TAIL);
                    self.bind_top_to_local(head);
                    self.bind_top_to_local(tail);
                } else if let Pattern::Tuple(patterns) = &arm.pattern {
                    // Last arm tuple: extract and bind each element.
                    for (idx, pat) in patterns.iter().enumerate() {
                        self.emit_op(EXTRACT_TUPLE_ITEM);
                        self.emit_u8(idx as u8);
                        if let Pattern::Ident(name) = pat {
                            self.bind_top_to_local(name);
                        } else {
                            self.emit_op(POP);
                        }
                    }
                }
                Vec::new()
            } else {
                match &arm.pattern {
                    Pattern::Wildcard => Vec::new(),
                    Pattern::Ident(name) => {
                        self.dup_and_bind_top_to_local(name);
                        Vec::new()
                    }
                    pat => self.compile_pattern(pat)?,
                }
            };

            self.emit_op(POP);
            self.compile_expr(&arm.body)?;

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
    fn try_compile_bool_match(
        &mut self,
        subject: &Expr,
        arms: &[MatchArm],
    ) -> Result<Option<()>, CompileError> {
        if arms.len() != 2 {
            return Ok(None);
        }

        let (true_body, false_body) = match (&arms[0].pattern, &arms[1].pattern) {
            (Pattern::Literal(Literal::Bool(true)), Pattern::Literal(Literal::Bool(false))) => {
                (&arms[0].body, &arms[1].body)
            }
            (Pattern::Literal(Literal::Bool(false)), Pattern::Literal(Literal::Bool(true))) => {
                (&arms[1].body, &arms[0].body)
            }
            // Also handle `true -> A, _ -> B` (wildcard/ident as false).
            (Pattern::Literal(Literal::Bool(true)), Pattern::Wildcard | Pattern::Ident(_)) => {
                (&arms[0].body, &arms[1].body)
            }
            _ => return Ok(None),
        };

        // Optimization: if subject is a negated comparison (>=, <=, !=),
        // emit the base comparison and swap branches to eliminate NOT.
        if let Expr::BinOp(op, lhs, rhs) = subject {
            use crate::ast::BinOp;
            let inverted_op = match op {
                BinOp::Gte => Some(LT),
                BinOp::Lte => Some(GT),
                BinOp::Neq => Some(EQ),
                _ => None,
            };
            if let Some(base_op) = inverted_op {
                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?;
                self.emit_op(base_op);
                // Swapped: LT=true means NOT(>=), so jump to true_body (the >= case)
                let true_jump = self.emit_jump(JUMP_IF_FALSE);
                self.compile_expr(false_body)?;
                let end_jump = self.emit_jump(JUMP);
                self.patch_jump(true_jump);
                self.compile_expr(true_body)?;
                self.patch_jump(end_jump);
                return Ok(Some(()));
            }
        }

        // Normal path: subject, JUMP_IF_FALSE → false_body, true_body, JUMP → end, false_body
        self.compile_expr(subject)?;
        let false_jump = self.emit_jump(JUMP_IF_FALSE);
        self.compile_expr(true_body)?;
        let end_jump = self.emit_jump(JUMP);
        self.patch_jump(false_jump);
        self.compile_expr(false_body)?;
        self.patch_jump(end_jump);

        Ok(Some(()))
    }

    /// Try to compile a match as a MATCH_DISPATCH table.
    /// Returns Some(()) if successful, None if the match doesn't qualify.
    fn try_compile_match_dispatch(
        &mut self,
        subject: &Expr,
        arms: &[MatchArm],
        _line: usize,
    ) -> Result<Option<()>, CompileError> {
        if arms.len() < 2 {
            return Ok(None);
        }

        // Classify arms. Last arm may be wildcard/ident (default).
        let has_default = matches!(
            arms.last().map(|a| &a.pattern),
            Some(Pattern::Wildcard | Pattern::Ident(_))
        );
        let dispatchable_end = if has_default {
            arms.len() - 1
        } else {
            arms.len()
        };

        let mut entries = Vec::new();
        for (i, arm) in arms[..dispatchable_end].iter().enumerate() {
            if let Some(entry) = self.classify_dispatchable(&arm.pattern, i) {
                entries.push(entry);
            } else {
                return Ok(None); // non-dispatchable arm found → bail
            }
        }

        // Need at least 2 dispatchable arms to be worth it.
        if entries.len() < 2 {
            return Ok(None);
        }

        // Limit to 255 entries (count is u8).
        if entries.len() > 255 {
            return Ok(None);
        }

        // --- Check if ALL dispatchable arms have const bodies (no bindings) ---
        let all_const = entries.iter().all(|e| {
            let arm = &arms[e.arm_index];
            // Must be exact match (not tag prefix — those need unwrap/bind).
            (e.kind == DISPATCH_KIND_EXACT || e.kind == DISPATCH_KIND_STRING)
                && self.try_const_expr(&arm.body).is_some()
        });

        if all_const {
            return self.emit_match_dispatch_const(&entries, arms, subject, has_default);
        }

        // --- Emit MATCH_DISPATCH (jump-based) ---
        self.compile_expr(subject)?;

        self.emit_op(MATCH_DISPATCH);
        self.emit_u8(entries.len() as u8);
        let default_offset_patch = self.code.len();
        self.emit_i16(0); // default_offset — patched later

        // Emit table entries with placeholder offsets.
        let mut entry_offset_patches = Vec::new();
        for entry in &entries {
            self.emit_u8(entry.kind);
            self.emit_u64(entry.expected);
            entry_offset_patches.push(self.code.len());
            self.emit_i16(0); // offset — patched later
        }

        let table_end = self.offset(); // all offsets relative to here

        // Emit arm bodies.
        let mut end_jumps = Vec::new();

        for (table_idx, entry) in entries.iter().enumerate() {
            let arm = &arms[entry.arm_index];

            // Patch this entry's offset to point here.
            let arm_start = self.offset();
            let rel = (arm_start as isize - table_end as isize) as i16;
            let bytes = (rel as u16).to_be_bytes();
            self.code[entry_offset_patches[table_idx]] = bytes[0];
            self.code[entry_offset_patches[table_idx] + 1] = bytes[1];

            // Prologue: unwrap/bind for tag-match arms.
            self.emit_dispatch_arm_prologue(&arm.pattern);

            // Pop subject, compile body.
            self.emit_op(POP);
            self.compile_expr(&arm.body)?;

            end_jumps.push(self.emit_jump(JUMP));
        }

        // Default arm (wildcard/ident or exhaustive fallthrough).
        let default_start = self.offset();
        let default_rel = (default_start as isize - table_end as isize) as i16;
        let default_bytes = (default_rel as u16).to_be_bytes();
        self.code[default_offset_patch] = default_bytes[0];
        self.code[default_offset_patch + 1] = default_bytes[1];

        if has_default {
            let default_arm = arms.last().unwrap();
            if let Pattern::Ident(name) = &default_arm.pattern {
                self.dup_and_bind_top_to_local(name);
            }
            self.emit_op(POP);
            self.compile_expr(&default_arm.body)?;
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
        arms: &[MatchArm],
        subject: &Expr,
        has_default: bool,
    ) -> Result<Option<()>, CompileError> {
        self.compile_expr(subject)?;

        self.emit_op(MATCH_DISPATCH_CONST);
        self.emit_u8(entries.len() as u8);
        let default_offset_patch = self.code.len();
        self.emit_i16(0); // default_offset — patched later

        // Emit table entries with inline results.
        for entry in entries {
            let arm = &arms[entry.arm_index];
            let result_bits = self.try_const_expr(&arm.body).unwrap();
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
        self.code[default_offset_patch] = default_bytes[0];
        self.code[default_offset_patch + 1] = default_bytes[1];

        if has_default {
            // Default arm body — subject was popped by opcode on miss,
            // then pushed back. Compile normally.
            let default_arm = arms.last().unwrap();
            if let Pattern::Ident(name) = &default_arm.pattern {
                self.dup_and_bind_top_to_local(name);
            }
            self.emit_op(POP);
            self.compile_expr(&default_arm.body)?;
        }

        // Patch the hit-skip JUMP to land here (after the default body).
        if let Some(patch) = hit_skip_jump {
            self.patch_jump(patch);
        }

        Ok(Some(()))
    }

    fn try_match_list_get_arms<'b>(
        &self,
        subject: &'b Expr,
        arms: &'b [MatchArm],
    ) -> Option<(&'b Expr, &'b Expr, Option<&'b str>, &'b Expr, &'b Expr)> {
        if arms.len() != 2 {
            return None;
        }
        let Expr::FnCall(fn_expr, args) = subject else {
            return None;
        };
        if args.len() != 2 {
            return None;
        }
        let Some(super::CallTarget::Builtin(crate::vm::builtin::VmBuiltin::ListGet)) =
            self.resolve_call_target(fn_expr)
        else {
            return None;
        };

        let mut some_binding = None;
        let mut some_body = None;
        let mut none_body = None;

        for arm in arms {
            match &arm.pattern {
                Pattern::Constructor(name, bindings) if name == "Option.Some" => {
                    if some_body.is_some() || bindings.len() > 1 {
                        return None;
                    }
                    some_binding = bindings.first().map(|s| s.as_str());
                    some_body = Some(arm.body.as_ref());
                }
                Pattern::Constructor(name, bindings)
                    if name == "Option.None" && bindings.is_empty() =>
                {
                    if none_body.is_some() {
                        return None;
                    }
                    none_body = Some(arm.body.as_ref());
                }
                _ => return None,
            }
        }

        Some((&args[0], &args[1], some_binding, some_body?, none_body?))
    }

    fn compile_list_get_match(
        &mut self,
        list_expr: &Expr,
        index_expr: &Expr,
        some_binding: Option<&str>,
        some_body: &Expr,
        none_body: &Expr,
    ) -> Result<(), CompileError> {
        self.compile_expr(list_expr)?;
        self.compile_expr(index_expr)?;
        self.emit_op(LIST_GET_MATCH);

        let none_jump = self.emit_jump(JUMP_IF_FALSE);

        if let Some(binding) = some_binding {
            self.dup_and_bind_top_to_local(binding);
        }
        self.emit_op(POP);
        self.compile_expr(some_body)?;
        let end_jump = self.emit_jump(JUMP);

        self.patch_jump(none_jump);
        self.compile_expr(none_body)?;
        self.patch_jump(end_jump);
        Ok(())
    }

    /// Compile a pattern. Subject is on top of stack (peeked, not consumed).
    /// Returns a Vec of fail-jump patch positions.
    fn compile_pattern(&mut self, pattern: &Pattern) -> Result<Vec<usize>, CompileError> {
        match pattern {
            Pattern::Wildcard => Ok(Vec::new()),
            Pattern::Ident(name) => {
                self.dup_and_bind_top_to_local(name);
                Ok(Vec::new())
            }
            Pattern::Literal(lit) => {
                self.emit_op(DUP);
                self.compile_literal(lit)?;
                self.emit_op(EQ);
                let patch = self.emit_jump(JUMP_IF_FALSE);
                Ok(vec![patch])
            }
            Pattern::EmptyList => {
                self.emit_op(MATCH_NIL);
                let patch = self.code.len();
                self.emit_i16(0);
                Ok(vec![patch])
            }
            Pattern::Cons(head, tail) => {
                self.emit_op(MATCH_CONS);
                let fail_patch = self.code.len();
                self.emit_i16(0);

                self.emit_op(DUP);
                self.emit_op(LIST_HEAD_TAIL);
                self.bind_top_to_local(head);
                self.bind_top_to_local(tail);

                Ok(vec![fail_patch])
            }
            Pattern::Constructor(name, bindings) => {
                self.compile_constructor_pattern(name, bindings)
            }
            Pattern::Tuple(patterns) => self.compile_tuple_pattern(patterns),
        }
    }

    fn compile_constructor_pattern(
        &mut self,
        name: &str,
        bindings: &[String],
    ) -> Result<Vec<usize>, CompileError> {
        match name {
            "Result.Ok" => Ok(self.compile_unwrap_pattern(0, bindings.first())),
            "Result.Err" => Ok(self.compile_unwrap_pattern(1, bindings.first())),
            "Option.Some" => Ok(self.compile_unwrap_pattern(2, bindings.first())),
            "Option.None" => {
                self.emit_op(DUP);
                let none_const = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(none_const);
                self.emit_op(EQ);
                let fail_patch = self.emit_jump(JUMP_IF_FALSE);
                Ok(vec![fail_patch])
            }
            _ => {
                if let Some((type_name, variant_name)) = name.rsplit_once('.')
                    && let Some(type_id) = self.resolve_type_id(type_name)
                    && let Some(variant_id) = self.arena.find_variant_id(type_id, variant_name)
                    && let Some(ctor_id) = self.arena.find_ctor_id(type_id, variant_id)
                {
                    if ctor_id > u16::MAX as u32 {
                        return Err(CompileError {
                            msg: format!("constructor id too large for VM pattern match: {}", name),
                        });
                    }
                    let mut patches = Vec::new();
                    self.emit_op(MATCH_VARIANT);
                    self.emit_u16(ctor_id as u16);
                    let variant_fail = self.code.len();
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
