/// Match-related emission for ExprEmitter.
///
/// All match patterns: bool match, list match, dispatch table,
/// generic match fallback, variant patterns.
use wasm_encoder::Instruction;

use crate::ast::{Literal, MatchArm, Pattern, Spanned};
use crate::ir::{
    self, BoolSubjectPlan, DispatchBindingPlan, MatchDispatchPlan, SemanticConstructor,
    WrapperKind, classify_constructor_name, classify_match_dispatch_plan,
};
use crate::types::Type;

use super::super::types::{WasmType, aver_type_to_wasm};
use super::super::value;
use super::ExprEmitter;

impl<'a> ExprEmitter<'a> {
    pub(super) fn emit_match(&mut self, subject: &Spanned<crate::ast::Expr>, arms: &[MatchArm]) {
        let ir_ctx = self.ir_ctx();
        let plan = classify_match_dispatch_plan(arms, &ir_ctx);

        match plan {
            Some(MatchDispatchPlan::Bool(shape)) => {
                self.emit_bool_match(subject, arms, &shape);
            }
            Some(MatchDispatchPlan::List(shape)) => {
                self.emit_list_match(subject, arms, &shape);
            }
            Some(MatchDispatchPlan::Table(table)) => {
                self.emit_dispatch_table(subject, arms, &table);
            }
            None => {
                // Fallback: generic match via old pattern approach
                self.emit_generic_match(subject, arms);
            }
        }
    }

    fn emit_bool_match(
        &mut self,
        subject: &Spanned<crate::ast::Expr>,
        arms: &[MatchArm],
        shape: &ir::BoolMatchShape,
    ) {
        let result_type = self.infer_match_result_type(arms);
        let result_local = self.alloc_local(result_type);
        self.emit_default_init(result_local, result_type);

        let subject_plan = ir::classify_bool_subject_plan(&subject.node);

        match subject_plan {
            BoolSubjectPlan::Compare {
                lhs,
                rhs,
                op,
                invert,
            } => {
                // Emit comparison directly
                let lhs_type = self.infer_expr_type(&lhs.node);
                let rhs_type = self.infer_expr_type(&rhs.node);
                let lhs_aver_type = self.infer_aver_type(&lhs.node);
                let cmp_type = if lhs_type == WasmType::F64 || rhs_type == WasmType::F64 {
                    WasmType::F64
                } else {
                    lhs_type
                };

                if matches!(op, ir::BoolCompareOp::Eq)
                    && cmp_type == WasmType::I32
                    && matches!(lhs_aver_type, Some(Type::Str))
                {
                    self.emit_expr(&lhs.node);
                    self.emit_expr(&rhs.node);
                    self.instructions.push(Instruction::Call(self.rt.str_eq));
                } else {
                    self.emit_expr(&lhs.node);
                    if cmp_type == WasmType::F64 && lhs_type == WasmType::I64 {
                        self.instructions.push(Instruction::F64ConvertI64S);
                    }
                    self.emit_expr(&rhs.node);
                    if cmp_type == WasmType::F64 && rhs_type == WasmType::I64 {
                        self.instructions.push(Instruction::F64ConvertI64S);
                    }

                    let cmp_instr = match (op, cmp_type) {
                        (ir::BoolCompareOp::Eq, WasmType::I64) => Instruction::I64Eq,
                        (ir::BoolCompareOp::Eq, WasmType::F64) => Instruction::F64Eq,
                        (ir::BoolCompareOp::Eq, WasmType::I32) => Instruction::I32Eq,
                        (ir::BoolCompareOp::Lt, WasmType::I64) => Instruction::I64LtS,
                        (ir::BoolCompareOp::Lt, WasmType::F64) => Instruction::F64Lt,
                        (ir::BoolCompareOp::Lt, WasmType::I32) => Instruction::I32LtS,
                        (ir::BoolCompareOp::Gt, WasmType::I64) => Instruction::I64GtS,
                        (ir::BoolCompareOp::Gt, WasmType::F64) => Instruction::F64Gt,
                        (ir::BoolCompareOp::Gt, WasmType::I32) => Instruction::I32GtS,
                    };
                    self.instructions.push(cmp_instr);
                }

                // Invert for Neq/Gte/Lte
                let true_arm = if invert {
                    shape.false_arm_index
                } else {
                    shape.true_arm_index
                };
                let false_arm = if invert {
                    shape.true_arm_index
                } else {
                    shape.false_arm_index
                };

                self.emit_if(wasm_encoder::BlockType::Empty);
                self.emit_expr(&arms[true_arm].body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                self.emit_else();
                self.emit_expr(&arms[false_arm].body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                self.emit_end();
            }
            BoolSubjectPlan::Expr(_) => {
                self.emit_expr(&subject.node);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.emit_expr(&arms[shape.true_arm_index].body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                self.emit_else();
                self.emit_expr(&arms[shape.false_arm_index].body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                self.emit_end();
            }
        }

        self.instructions.push(Instruction::LocalGet(result_local));
    }

    fn emit_list_match(
        &mut self,
        subject: &Spanned<crate::ast::Expr>,
        arms: &[MatchArm],
        shape: &ir::ListMatchShape,
    ) {
        let result_type = self.infer_match_result_type(arms);
        let result_local = self.alloc_local(result_type);
        self.emit_default_init(result_local, result_type);

        let subj_type = self.infer_expr_type(&subject.node);
        let subj_aver_type = self.infer_aver_type(&subject.node);
        self.emit_expr(&subject.node);
        let subj_local = self.alloc_local(subj_type);
        if let Some(at) = &subj_aver_type {
            self.local_aver_types.insert(subj_local, at.clone());
        }
        self.instructions.push(Instruction::LocalSet(subj_local));

        // Check ptr == 0 (empty)
        self.instructions.push(Instruction::LocalGet(subj_local));
        self.instructions.push(Instruction::I32Eqz);
        self.emit_if(wasm_encoder::BlockType::Empty);

        // Empty arm
        self.emit_expr(&arms[shape.empty_arm_index].body.node);
        self.instructions.push(Instruction::LocalSet(result_local));
        self.emit_else();

        // Cons arm -- bind head and tail
        let cons_arm = &arms[shape.cons_arm_index];
        if let Pattern::Cons(head_name, tail_name) = &cons_arm.pattern {
            let elem_aver_type = subj_aver_type
                .as_ref()
                .and_then(|t| {
                    if let Type::List(inner) = t {
                        Some(inner.as_ref().clone())
                    } else {
                        None
                    }
                })
                .unwrap_or(Type::Int);
            let elem_wasm_type = aver_type_to_wasm(&elem_aver_type);

            let head_local = self.alloc_local(elem_wasm_type);
            self.locals.insert(head_name.clone(), head_local);
            self.local_aver_types
                .insert(head_local, elem_aver_type.clone());
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::I32Const(0));
            match elem_wasm_type {
                WasmType::F64 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.obj_field_f64));
                }
                WasmType::I32 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.obj_field_i32));
                }
                WasmType::I64 => {
                    self.instructions.push(Instruction::Call(self.rt.obj_field));
                }
            }
            self.instructions.push(Instruction::LocalSet(head_local));

            let tail_local = self.alloc_local(WasmType::I32);
            self.locals.insert(tail_name.clone(), tail_local);
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::I32Const(1));
            self.instructions
                .push(Instruction::Call(self.rt.obj_field_i32));
            self.instructions.push(Instruction::LocalSet(tail_local));
            if let Some(at) = &subj_aver_type {
                self.local_aver_types.insert(tail_local, at.clone());
            }
        }

        self.emit_expr(&cons_arm.body.node);
        self.instructions.push(Instruction::LocalSet(result_local));
        self.emit_end();

        self.instructions.push(Instruction::LocalGet(result_local));
    }

    fn emit_dispatch_table(
        &mut self,
        subject: &Spanned<crate::ast::Expr>,
        arms: &[MatchArm],
        table: &ir::DispatchTableShape,
    ) {
        let result_type = self.infer_match_result_type(arms);
        let result_local = self.alloc_local(result_type);
        self.emit_default_init(result_local, result_type);

        let subj_type = self.infer_expr_type(&subject.node);
        self.emit_expr(&subject.node);
        let subj_local = self.alloc_local(subj_type);
        let subj_aver_type = self.infer_aver_type(&subject.node);
        if let Some(at) = &subj_aver_type {
            self.local_aver_types.insert(subj_local, at.clone());
        }
        self.instructions.push(Instruction::LocalSet(subj_local));

        // Emit nested if/else chain for dispatch entries
        let num_entries = table.entries.len();
        for (i, entry) in table.entries.iter().enumerate() {
            self.emit_dispatch_check(subj_local, &entry.pattern);
            self.emit_if(wasm_encoder::BlockType::Empty);

            if let DispatchBindingPlan::WrapperPayload(binding_name) = &entry.binding {
                self.emit_wrapper_binding(subj_local, binding_name);
            }

            self.emit_expr(&arms[entry.arm_index].body.node);
            self.instructions.push(Instruction::LocalSet(result_local));

            // Open else for next entry or default
            let has_more = i < num_entries - 1 || table.default_arm.is_some();
            if has_more {
                self.emit_else();
            }
        }

        // Default arm (inside the last else)
        if let Some(default) = &table.default_arm {
            if let Some(ref binding_name) = default.binding_name {
                let bind_local = self.alloc_local(subj_type);
                self.locals.insert(binding_name.clone(), bind_local);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::LocalSet(bind_local));
                if let Some(at) = &subj_aver_type {
                    self.local_aver_types.insert(bind_local, at.clone());
                }
            }
            self.emit_expr(&arms[default.arm_index].body.node);
            self.instructions.push(Instruction::LocalSet(result_local));
        }

        // Close all if blocks (one End per entry)
        for _ in 0..num_entries {
            self.emit_end();
        }

        self.instructions.push(Instruction::LocalGet(result_local));
    }

    pub(super) fn emit_dispatch_check(
        &mut self,
        subj_local: u32,
        pattern: &ir::SemanticDispatchPattern,
    ) {
        match pattern {
            ir::SemanticDispatchPattern::Literal(lit) => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                match lit {
                    ir::DispatchLiteral::Int(n) => {
                        self.instructions.push(Instruction::I64Const(*n));
                        self.instructions.push(Instruction::I64Eq);
                    }
                    ir::DispatchLiteral::Str(s) => {
                        self.emit_string_literal(s);
                        self.instructions.push(Instruction::Call(self.rt.str_eq));
                    }
                    ir::DispatchLiteral::Bool(b) => {
                        self.instructions
                            .push(Instruction::I32Const(if *b { 1 } else { 0 }));
                        self.instructions.push(Instruction::I32Eq);
                    }
                    _ => {
                        self.instructions.push(Instruction::Drop);
                        self.instructions.push(Instruction::I32Const(0));
                    }
                }
            }
            ir::SemanticDispatchPattern::EmptyList => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I32Eqz);
            }
            ir::SemanticDispatchPattern::NoneValue => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions
                    .push(Instruction::I32Const(value::NONE_SENTINEL));
                self.instructions.push(Instruction::I32Eq);
            }
            ir::SemanticDispatchPattern::WrapperTag(kind) => {
                let expected_tag = match kind {
                    WrapperKind::ResultOk => value::WRAP_OK,
                    WrapperKind::ResultErr => value::WRAP_ERR,
                    WrapperKind::OptionSome => value::WRAP_SOME,
                };
                // Wrapper match must agree on both kind and tag.
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I32Const(0));
                self.instructions.push(Instruction::I32GtS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_WRAPPER as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_WRAPPER_F64 as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::I32Or);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_WRAPPER_I32 as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::I32Or);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_tag));
                self.instructions
                    .push(Instruction::I32Const(expected_tag as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::I32And);
                self.emit_else();
                self.instructions.push(Instruction::I32Const(0)); // false
                self.emit_end();
            }
        }
    }

    pub(super) fn emit_wrapper_binding(&mut self, subj_local: u32, binding_name: &str) {
        // Determine inner type STATICALLY from the subject's aver type
        let subj_aver_type = self.local_aver_types.get(&subj_local).cloned();
        let inner_type = match &subj_aver_type {
            Some(Type::Result(ok, _)) => aver_type_to_wasm(ok),
            Some(Type::Option(inner)) => aver_type_to_wasm(inner),
            _ => WasmType::I64, // fallback
        };

        let inner_aver_type = match &subj_aver_type {
            Some(Type::Result(ok, _)) => Some(ok.as_ref().clone()),
            Some(Type::Option(inner)) => Some(inner.as_ref().clone()),
            _ => None,
        };

        let bind_local = self.alloc_local(inner_type);
        self.locals.insert(binding_name.to_string(), bind_local);
        if let Some(at) = inner_aver_type {
            self.local_aver_types.insert(bind_local, at);
        }

        self.instructions.push(Instruction::LocalGet(subj_local));
        match inner_type {
            WasmType::F64 => self
                .instructions
                .push(Instruction::Call(self.rt.unwrap_f64)),
            WasmType::I32 => self
                .instructions
                .push(Instruction::Call(self.rt.unwrap_i32)),
            WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.unwrap)),
        }
        self.instructions.push(Instruction::LocalSet(bind_local));
    }

    /// Generic match fallback for patterns not handled by IR dispatch plans.
    /// Handles variant patterns (Shape.Circle(r)), constructor patterns with user types.
    pub(super) fn emit_generic_match(
        &mut self,
        subject: &Spanned<crate::ast::Expr>,
        arms: &[MatchArm],
    ) {
        let subj_type = self.infer_expr_type(&subject.node);
        let subj_aver_type = self.infer_aver_type(&subject.node);
        self.emit_expr(&subject.node);
        let subj_local = self.alloc_local(subj_type);
        if let Some(at) = subj_aver_type {
            self.local_aver_types.insert(subj_local, at);
        }
        self.instructions.push(Instruction::LocalSet(subj_local));

        let result_type = self.infer_match_result_type(arms);
        let result_local = self.alloc_local(result_type);
        self.emit_default_init(result_local, result_type);

        self.emit_generic_arms(subj_local, subj_type, result_local, arms, 0);

        self.instructions.push(Instruction::LocalGet(result_local));
    }

    pub(super) fn emit_generic_arms(
        &mut self,
        subj_local: u32,
        subj_type: WasmType,
        result_local: u32,
        arms: &[MatchArm],
        idx: usize,
    ) {
        if idx >= arms.len() {
            return;
        }
        let arm = &arms[idx];
        let is_last = idx == arms.len() - 1;

        match &arm.pattern {
            Pattern::Wildcard => {
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
            }
            Pattern::Ident(name) => {
                let bind_local = self.alloc_local(subj_type);
                self.locals.insert(name.clone(), bind_local);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::LocalSet(bind_local));
                if let Some(at) = self.local_aver_types.get(&subj_local).cloned() {
                    self.local_aver_types.insert(bind_local, at);
                }
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
            }
            Pattern::Literal(lit) => {
                self.emit_literal_pattern(subj_local, subj_type, result_local, lit, arm, arms, idx);
            }
            Pattern::Constructor(ctor_name, bindings) => {
                self.emit_constructor_pattern_void(
                    subj_local,
                    subj_type,
                    result_local,
                    ctor_name,
                    bindings,
                    arm,
                    arms,
                    idx,
                );
            }
            Pattern::EmptyList => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I32Eqz);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                if !is_last {
                    self.emit_else();
                    self.emit_generic_arms(subj_local, subj_type, result_local, arms, idx + 1);
                }
                self.emit_end();
            }
            Pattern::Cons(_, _) | Pattern::Tuple(_) => {
                // Should be handled by MatchDispatchPlan::List
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_literal_pattern(
        &mut self,
        subj_local: u32,
        subj_type: WasmType,
        result_local: u32,
        lit: &Literal,
        arm: &MatchArm,
        arms: &[MatchArm],
        idx: usize,
    ) {
        let is_last = idx == arms.len() - 1;

        self.instructions.push(Instruction::LocalGet(subj_local));
        match lit {
            Literal::Int(n) => {
                self.instructions.push(Instruction::I64Const(*n));
                self.instructions.push(Instruction::I64Eq);
            }
            Literal::Str(s) => {
                self.emit_string_literal(s);
                self.instructions.push(Instruction::Call(self.rt.str_eq));
            }
            Literal::Bool(b) => {
                self.instructions
                    .push(Instruction::I32Const(if *b { 1 } else { 0 }));
                self.instructions.push(Instruction::I32Eq);
            }
            _ => {
                self.instructions.push(Instruction::Drop);
                self.instructions.push(Instruction::I32Const(0));
            }
        }
        self.emit_if(wasm_encoder::BlockType::Empty);
        self.emit_expr(&arm.body.node);
        self.instructions.push(Instruction::LocalSet(result_local));
        if !is_last {
            self.emit_else();
            self.emit_generic_arms(subj_local, subj_type, result_local, arms, idx + 1);
        }
        self.emit_end();
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_constructor_pattern_void(
        &mut self,
        subj_local: u32,
        subj_type: WasmType,
        result_local: u32,
        ctor_name: &str,
        bindings: &[String],
        arm: &MatchArm,
        arms: &[MatchArm],
        idx: usize,
    ) {
        let is_last = idx == arms.len() - 1;
        let ctor = classify_constructor_name(ctor_name, &self.ir_ctx());
        match ctor {
            SemanticConstructor::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => {
                self.emit_variant_pattern(
                    subj_local,
                    subj_type,
                    result_local,
                    &qualified_type_name,
                    &variant_name,
                    bindings,
                    arm,
                    arms,
                    idx,
                );
            }
            SemanticConstructor::NoneValue => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions
                    .push(Instruction::I32Const(value::NONE_SENTINEL));
                self.instructions.push(Instruction::I32Eq);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                if !is_last {
                    self.emit_else();
                    self.emit_generic_arms(subj_local, subj_type, result_local, arms, idx + 1);
                }
                self.emit_end();
            }
            SemanticConstructor::Wrapper(kind) => {
                let expected_tag = match kind {
                    WrapperKind::ResultOk => value::WRAP_OK,
                    WrapperKind::ResultErr => value::WRAP_ERR,
                    WrapperKind::OptionSome => value::WRAP_SOME,
                };
                // Wrapper match must agree on both kind and tag.
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I32Const(0));
                self.instructions.push(Instruction::I32GtS);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_WRAPPER as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_WRAPPER_F64 as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::I32Or);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_WRAPPER_I32 as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::I32Or);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_tag));
                self.instructions
                    .push(Instruction::I32Const(expected_tag as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions.push(Instruction::I32And);
                self.instructions.push(Instruction::I32And);
                self.emit_if(wasm_encoder::BlockType::Empty);
                if let Some(binding_name) = bindings.first() {
                    self.emit_wrapper_binding(subj_local, binding_name);
                }
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                if !is_last {
                    self.emit_else();
                    self.emit_generic_arms(subj_local, subj_type, result_local, arms, idx + 1);
                }
                self.emit_end();
            }
            SemanticConstructor::Unknown(_) => {}
        }
    }

    /// Emit pattern match for user-defined variant: Shape.Circle(r) -> ...
    #[allow(clippy::too_many_arguments)]
    pub(super) fn emit_variant_pattern(
        &mut self,
        subj_local: u32,
        subj_type: WasmType,
        result_local: u32,
        type_name: &str,
        variant_name: &str,
        bindings: &[String],
        arm: &MatchArm,
        arms: &[MatchArm],
        idx: usize,
    ) {
        let is_last = idx == arms.len() - 1;

        let info = self
            .variant_registry
            .get(&(type_name.to_string(), variant_name.to_string()));
        let expected_tag = info.map(|i| i.tag).unwrap_or(0);
        let field_type_names: Vec<String> = info.map(|i| i.field_types.clone()).unwrap_or_default();

        // Variant match must agree on both kind and tag.
        self.instructions.push(Instruction::LocalGet(subj_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions.push(Instruction::I32GtS);
        self.instructions.push(Instruction::LocalGet(subj_local));
        self.instructions.push(Instruction::Call(self.rt.obj_kind));
        self.instructions
            .push(Instruction::I32Const(value::OBJ_VARIANT as i32));
        self.instructions.push(Instruction::I32Eq);
        self.instructions.push(Instruction::I32And);
        self.instructions.push(Instruction::LocalGet(subj_local));
        self.instructions.push(Instruction::Call(self.rt.obj_tag));
        self.instructions
            .push(Instruction::I32Const(expected_tag as i32));
        self.instructions.push(Instruction::I32Eq);
        self.instructions.push(Instruction::I32And);
        self.emit_if(wasm_encoder::BlockType::Empty);

        // Bind fields
        for (i, binding_name) in bindings.iter().enumerate() {
            if binding_name == "_" {
                continue;
            }
            let field_type_name = field_type_names.get(i).map(|s| s.as_str()).unwrap_or("Int");
            let field_wasm_type = self.type_str_to_wasm(field_type_name);
            // For heap-allocated inner types (String, user types), load as i64 then convert
            let bind_local = self.alloc_local(field_wasm_type);
            self.locals.insert(binding_name.clone(), bind_local);
            // Set aver type
            let aver_ty = match field_type_name {
                "Float" => Type::Float,
                "Bool" => Type::Bool,
                "String" | "Str" => Type::Str,
                "Int" => Type::Int,
                other => Type::Named(other.to_string()),
            };
            self.local_aver_types.insert(bind_local, aver_ty);

            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::I32Const(i as i32));
            match field_wasm_type {
                WasmType::F64 => {
                    // Load as i64 then reinterpret as f64 (stored as reinterpreted i64)
                    self.instructions.push(Instruction::Call(self.rt.obj_field));
                    self.instructions.push(Instruction::F64ReinterpretI64);
                }
                WasmType::I32 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.obj_field_i32));
                }
                WasmType::I64 => {
                    self.instructions.push(Instruction::Call(self.rt.obj_field));
                }
            }
            self.instructions.push(Instruction::LocalSet(bind_local));
        }

        self.emit_expr(&arm.body.node);
        self.instructions.push(Instruction::LocalSet(result_local));

        if !is_last {
            self.emit_else();
            self.emit_generic_arms(subj_local, subj_type, result_local, arms, idx + 1);
        }
        self.emit_end();
    }
}
