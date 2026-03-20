use super::{CompileError, FnCompiler};
use crate::ast::{Expr, MatchArm, Pattern};
use crate::nan_value::NanValue;
use crate::vm::opcode::*;

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

        self.compile_expr(subject)?;

        let mut end_jumps = Vec::new();
        let mut last_arm_fail_patches = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;

            let fail_patches = match &arm.pattern {
                Pattern::Wildcard => Vec::new(),
                Pattern::Ident(name) => {
                    self.emit_op(DUP);
                    if let Some(&slot) = self.local_slots.get(name) {
                        self.emit_op(STORE_LOCAL);
                        self.emit_u8(slot as u8);
                    } else {
                        self.emit_op(POP);
                    }
                    Vec::new()
                }
                pat => self.compile_pattern(pat)?,
            };

            self.emit_op(POP);
            self.compile_expr(&arm.body)?;

            if is_last {
                last_arm_fail_patches = fail_patches;
            } else {
                end_jumps.push(self.emit_jump(JUMP));
                if !fail_patches.is_empty() {
                    let fail_cleanup = self.offset();
                    for patch in fail_patches {
                        self.patch_jump_to(patch, fail_cleanup);
                    }
                }
            }
        }

        let last_refutable = arms
            .last()
            .is_none_or(|a| !matches!(a.pattern, Pattern::Wildcard | Pattern::Ident(_)));
        if last_refutable && !last_arm_fail_patches.is_empty() {
            end_jumps.push(self.emit_jump(JUMP));
            let fail_target = self.offset();
            for patch in last_arm_fail_patches {
                self.patch_jump_to(patch, fail_target);
            }
            self.emit_op(POP);
            self.emit_op(MATCH_FAIL);
            self.emit_u16(line as u16);
        }

        for patch in end_jumps {
            self.patch_jump(patch);
        }

        Ok(())
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
