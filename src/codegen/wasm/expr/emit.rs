/// Core expression emission for ExprEmitter.
///
/// emit_expr, emit_block, emit_binop, emit_fn_call, emit_wrap,
/// emit_variant_constructor, emit_constructor, emit_error_prop,
/// emit_list, emit_tuple, emit_record_create, emit_field_access,
/// emit_map_literal, emit_interpolated_str, emit_str_part,
/// emit_tailcall, emit_literal, emit_string_literal, emit_default_init.
use std::collections::HashMap;

use wasm_encoder::Instruction;

use crate::ast::{BinOp, Expr, Literal, Spanned, Stmt, StrPart, TailCallData};
use crate::ir::{
    CallPlan, LeafOp, SemanticConstructor, WrapperKind, classify_call_plan,
    classify_constructor_name, classify_leaf_op,
};
use crate::types::Type;

use super::super::types::{WasmType, aver_type_to_wasm};
use super::super::value;
use super::ExprEmitter;

impl<'a> ExprEmitter<'a> {
    pub(super) fn emit_block(&mut self, stmts: &[Stmt]) {
        if stmts.is_empty() {
            self.instructions.push(Instruction::I32Const(0));
            return;
        }

        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            match stmt {
                Stmt::Binding(name, _type_ann, expr) => {
                    let wt = self.infer_expr_type(&expr.node);
                    let at = self.infer_aver_type(&expr.node);
                    self.emit_expr(&expr.node);
                    let idx = self.alloc_local(wt);
                    self.locals.insert(name.clone(), idx);
                    if let Some(at) = at {
                        self.local_aver_types.insert(idx, at);
                    }
                    self.instructions.push(Instruction::LocalSet(idx));
                    if is_last {
                        self.instructions.push(Instruction::I32Const(0));
                    }
                }
                Stmt::Expr(expr) => {
                    self.emit_expr(&expr.node);
                    if !is_last {
                        self.instructions.push(Instruction::Drop);
                    }
                }
            }
        }
    }

    pub(super) fn emit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(lit) => self.emit_literal(lit),
            Expr::Ident(name) | Expr::Resolved { name, .. } => {
                if let Some(&idx) = self.locals.get(name) {
                    self.instructions.push(Instruction::LocalGet(idx));
                } else {
                    self.codegen_error(format!(
                        "unresolved local identifier `{}` in WASM codegen",
                        name
                    ));
                    self.emit_default_value(self.infer_expr_type(expr));
                }
            }
            Expr::BinOp(op, lhs, rhs) => self.emit_binop(op, lhs, rhs),
            Expr::FnCall(callee, args) => self.emit_fn_call(callee, args),
            Expr::Match { subject, arms } => self.emit_match(subject, arms),
            Expr::Constructor(name, inner) => self.emit_constructor(name, inner),
            Expr::ErrorProp(inner) => self.emit_error_prop(inner),
            Expr::InterpolatedStr(parts) => self.emit_interpolated_str(parts),
            Expr::List(items) => self.emit_list(items),
            Expr::Tuple(items) => self.emit_tuple(items),
            Expr::RecordCreate { type_name, fields } => {
                self.emit_record_create(type_name, fields);
            }
            Expr::Attr(_, _) => {
                let leaf = {
                    let ctx = self.ir_ctx();
                    classify_leaf_op(expr, &ctx)
                };
                match leaf {
                    Some(LeafOp::NoneValue) => {
                        self.instructions
                            .push(Instruction::I32Const(value::NONE_SENTINEL));
                    }
                    Some(LeafOp::VariantConstructor {
                        qualified_type_name,
                        variant_name,
                    }) => {
                        self.emit_variant_constructor(&qualified_type_name, &variant_name, &[]);
                    }
                    Some(LeafOp::StaticRef(name)) => {
                        self.codegen_error(format!(
                            "static path `{}` in value position not supported in WASM",
                            name
                        ));
                        self.emit_default_value(WasmType::I32);
                    }
                    Some(LeafOp::FieldAccess {
                        object, field_name, ..
                    }) => {
                        self.emit_field_access(object, field_name);
                    }
                    Some(other) => {
                        unreachable!(
                            "classify_leaf_op returned unexpected variant for Expr::Attr: {:?}",
                            other
                        );
                    }
                    None => {
                        unreachable!("classify_leaf_op returned None for Expr::Attr");
                    }
                }
            }
            Expr::TailCall(tc) => self.emit_tailcall(tc),
            Expr::MapLiteral(entries) => {
                self.emit_map_literal(entries);
            }
            Expr::IndependentProduct(items, unwrap) => {
                self.emit_independent_product(items, *unwrap);
            }
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                self.emit_record_update(type_name, base, updates);
            }
        }
    }

    // -----------------------------------------------------------------------
    // BinOp -- native WASM arithmetic + promotion
    // -----------------------------------------------------------------------

    fn emit_binop(&mut self, op: &BinOp, lhs: &Spanned<Expr>, rhs: &Spanned<Expr>) {
        let lhs_type = self.infer_expr_type(&lhs.node);
        let rhs_type = self.infer_expr_type(&rhs.node);
        let operand_type = if lhs_type == WasmType::F64 || rhs_type == WasmType::F64 {
            WasmType::F64
        } else {
            lhs_type
        };

        // String concatenation: + on strings → str_concat
        if matches!(op, BinOp::Add) && operand_type == WasmType::I32 {
            let lhs_aver = self.infer_aver_type(&lhs.node);
            if matches!(lhs_aver, Some(Type::Str)) {
                self.emit_expr(&lhs.node);
                self.emit_expr(&rhs.node);
                self.instructions
                    .push(Instruction::Call(self.rt.str_concat));
                return;
            }
        }

        // Heap object equality (I32 pointers) or I64 values that might be
        // heap pointers (parameters of Named types default to I64).
        if matches!(op, BinOp::Eq | BinOp::Neq) && operand_type != WasmType::F64 {
            let lhs_aver = self.infer_aver_type(&lhs.node);

            // String equality: content comparison via str_eq runtime
            if matches!(lhs_aver, Some(Type::Str)) {
                self.emit_expr(&lhs.node);
                if lhs_type != WasmType::I32 {
                    self.instructions.push(Instruction::I32WrapI64);
                }
                self.emit_expr(&rhs.node);
                if rhs_type != WasmType::I32 {
                    self.instructions.push(Instruction::I32WrapI64);
                }
                self.instructions.push(Instruction::Call(self.rt.str_eq));
                if matches!(op, BinOp::Neq) {
                    self.instructions.push(Instruction::I32Eqz);
                }
                return;
            }

            // Heap object equality: compare headers instead of pointers.
            // Works for Named types, variants, records — any I32 heap pointer.
            // Also handles I64 operands (function parameters of Named types
            // are stored as I64 in the WASM calling convention).
            if !matches!(
                lhs_aver,
                Some(Type::Int) | Some(Type::Bool) | Some(Type::Float)
            ) {
                let a_local = self.alloc_local(WasmType::I32);
                let b_local = self.alloc_local(WasmType::I32);
                self.emit_expr(&lhs.node);
                if lhs_type != WasmType::I32 {
                    self.instructions.push(Instruction::I32WrapI64);
                }
                self.instructions.push(Instruction::LocalSet(a_local));
                self.emit_expr(&rhs.node);
                if rhs_type != WasmType::I32 {
                    self.instructions.push(Instruction::I32WrapI64);
                }
                self.instructions.push(Instruction::LocalSet(b_local));
                // Fast path: same pointer
                self.instructions.push(Instruction::LocalGet(a_local));
                self.instructions.push(Instruction::LocalGet(b_local));
                self.instructions.push(Instruction::I32Eq);
                // Slow path: compare headers (i64 at offset 0)
                self.instructions.push(Instruction::LocalGet(a_local));
                self.instructions
                    .push(Instruction::I64Load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::LocalGet(b_local));
                self.instructions
                    .push(Instruction::I64Load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::I64Eq);
                // Either pointer match OR header match
                self.instructions.push(Instruction::I32Or);
                if matches!(op, BinOp::Neq) {
                    self.instructions.push(Instruction::I32Eqz);
                }
                return;
            }
        }

        self.emit_expr(&lhs.node);
        if operand_type == WasmType::F64 && lhs_type == WasmType::I64 {
            self.instructions.push(Instruction::F64ConvertI64S);
        }
        self.emit_expr(&rhs.node);
        if operand_type == WasmType::F64 && rhs_type == WasmType::I64 {
            self.instructions.push(Instruction::F64ConvertI64S);
        }

        let instr = match (op, operand_type) {
            (BinOp::Add, WasmType::I64) => Some(Instruction::I64Add),
            (BinOp::Add, WasmType::F64) => Some(Instruction::F64Add),
            (BinOp::Sub, WasmType::I64) => Some(Instruction::I64Sub),
            (BinOp::Sub, WasmType::F64) => Some(Instruction::F64Sub),
            (BinOp::Mul, WasmType::I64) => Some(Instruction::I64Mul),
            (BinOp::Mul, WasmType::F64) => Some(Instruction::F64Mul),
            (BinOp::Div, WasmType::I64) => Some(Instruction::I64DivS),
            (BinOp::Div, WasmType::F64) => Some(Instruction::F64Div),
            (BinOp::Eq, WasmType::I64) => Some(Instruction::I64Eq),
            (BinOp::Eq, WasmType::F64) => Some(Instruction::F64Eq),
            (BinOp::Eq, WasmType::I32) => Some(Instruction::I32Eq),
            (BinOp::Neq, WasmType::I64) => Some(Instruction::I64Ne),
            (BinOp::Neq, WasmType::F64) => Some(Instruction::F64Ne),
            (BinOp::Neq, WasmType::I32) => Some(Instruction::I32Ne),
            (BinOp::Lt, WasmType::I64) => Some(Instruction::I64LtS),
            (BinOp::Lt, WasmType::F64) => Some(Instruction::F64Lt),
            (BinOp::Lt, WasmType::I32) => Some(Instruction::I32LtS),
            (BinOp::Gt, WasmType::I64) => Some(Instruction::I64GtS),
            (BinOp::Gt, WasmType::F64) => Some(Instruction::F64Gt),
            (BinOp::Gt, WasmType::I32) => Some(Instruction::I32GtS),
            (BinOp::Lte, WasmType::I64) => Some(Instruction::I64LeS),
            (BinOp::Lte, WasmType::F64) => Some(Instruction::F64Le),
            (BinOp::Lte, WasmType::I32) => Some(Instruction::I32LeS),
            (BinOp::Gte, WasmType::I64) => Some(Instruction::I64GeS),
            (BinOp::Gte, WasmType::F64) => Some(Instruction::F64Ge),
            (BinOp::Gte, WasmType::I32) => Some(Instruction::I32GeS),
            _ => None,
        };
        if let Some(instr) = instr {
            self.instructions.push(instr);
        } else {
            let result_type = match op {
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    WasmType::I32
                }
                _ => operand_type,
            };
            self.codegen_error(format!(
                "unsupported binary operation `{:?}` for WASM operand type `{:?}`",
                op, operand_type
            ));
            self.instructions.push(Instruction::Drop);
            self.instructions.push(Instruction::Drop);
            self.emit_default_value(result_type);
        }
    }

    // -----------------------------------------------------------------------
    // Function calls -- via IR CallPlan
    // -----------------------------------------------------------------------

    fn emit_fn_call(&mut self, callee: &Spanned<Expr>, args: &[Spanned<Expr>]) {
        let plan = classify_call_plan(&callee.node, &self.ir_ctx());

        match plan {
            CallPlan::Function(ref name) => {
                let ret_type = self.infer_call_return_type(callee, args);
                let resolved_name = self.resolve_user_fn_name(name);
                for arg in args {
                    self.emit_expr(&arg.node);
                }
                if let Some(&fn_idx) = self.fn_indices.get(resolved_name.as_str()) {
                    self.instructions.push(Instruction::Call(fn_idx));
                } else {
                    self.codegen_error(format!(
                        "missing function index for call to `{}`",
                        resolved_name
                    ));
                    for _ in args {
                        self.instructions.push(Instruction::Drop);
                    }
                    self.emit_default_value(ret_type);
                }
            }

            CallPlan::Wrapper(kind) => {
                if args.len() == 1 {
                    self.emit_expr(&args[0].node);
                    self.emit_wrap(kind, &args[0]);
                } else {
                    self.codegen_error("wrapper call with invalid arity");
                    self.emit_default_value(WasmType::I32);
                }
            }

            CallPlan::NoneValue => {
                for arg in args {
                    self.emit_expr(&arg.node);
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions
                    .push(Instruction::I32Const(value::NONE_SENTINEL));
            }

            CallPlan::TypeConstructor {
                ref qualified_type_name,
                ref variant_name,
            } => {
                self.emit_variant_constructor(qualified_type_name, variant_name, args);
            }

            CallPlan::Builtin(ref name) => {
                // Fast path: Option.withDefault(Vector.set(v, i, x), v) where
                // `v` is a `Resolved` slot whose `last_use` is true. Same
                // shape as the VM's owned-arg dispatch — last-use analysis
                // guarantees no further read of the slot after this
                // expression, so the heap object behind the slot has no
                // observer once we're done. Mutate the cell in place,
                // return the same slot. Zero allocations.
                if name == "Option.withDefault"
                    && args.len() == 2
                    && self.try_emit_vec_set_owned_keep(&args[0], &args[1])
                {
                    return;
                }
                // Fast path: Option.withDefault(Vector.get(v, i), default_literal)
                // → inline bounds check + direct load, no wrapper allocation.
                if name == "Option.withDefault"
                    && args.len() == 2
                    && self.try_emit_vec_get_or_default(&args[0], &args[1])
                {
                    return;
                }
                for arg in args {
                    self.emit_expr(&arg.node);
                }
                self.emit_builtin_call(name, args);
            }

            CallPlan::Dynamic => {
                let ret_type = self.infer_call_return_type(callee, args);
                self.codegen_error("dynamic function calls are not supported in the WASM backend");
                for arg in args {
                    self.emit_expr(&arg.node);
                }
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.emit_default_value(ret_type);
            }
        }
    }

    /// Emit a wrapper constructor. Value is already on the stack.
    fn emit_wrap(&mut self, kind: WrapperKind, arg: &Spanned<Expr>) {
        let inner_type = self.infer_expr_type(&arg.node);
        let inner_is_ptr = self.expr_is_heap_ptr(&arg.node);
        let wrap_tag = match kind {
            WrapperKind::ResultOk => value::WRAP_OK,
            WrapperKind::ResultErr => value::WRAP_ERR,
            WrapperKind::OptionSome => value::WRAP_SOME,
        };
        let tmp = self.alloc_local(inner_type);
        self.instructions.push(Instruction::LocalSet(tmp));
        self.instructions
            .push(Instruction::I32Const(wrap_tag as i32));
        self.instructions.push(Instruction::LocalGet(tmp));
        match inner_type {
            WasmType::I64 => {
                self.instructions
                    .push(Instruction::I32Const(if inner_is_ptr { 1 } else { 0 }));
                self.instructions.push(Instruction::Call(self.rt.wrap));
            }
            WasmType::F64 => self.instructions.push(Instruction::Call(self.rt.wrap_f64)),
            WasmType::I32 => {
                self.instructions
                    .push(Instruction::I32Const(if inner_is_ptr { 1 } else { 0 }));
                self.instructions.push(Instruction::Call(self.rt.wrap_i32));
            }
        }
    }

    /// Emit user-defined variant constructor: Shape.Circle(5.0)
    pub(super) fn emit_variant_constructor(
        &mut self,
        type_name: &str,
        variant_name: &str,
        args: &[Spanned<Expr>],
    ) {
        let info = self
            .variant_registry
            .get(&(type_name.to_string(), variant_name.to_string()));
        let tag = info.map(|i| i.tag).unwrap_or(0);
        let field_count = args.len();

        let size = 8 + field_count * 8;

        let ptr_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(size as i32));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(ptr_local));

        // Header: kind=OBJ_VARIANT, variant_tag=tag, field_count
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions
            .push(Instruction::I64Const(value::make_header(
                value::OBJ_VARIANT,
                tag as u64,
                self.ptr_mask_for_exprs(args.iter()) as u64,
                field_count as u64,
            ) as i64));
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));

        // Store fields (all as i64 -- convert if needed)
        for (i, arg) in args.iter().enumerate() {
            let field_type = self.infer_expr_type(&arg.node);
            self.instructions.push(Instruction::LocalGet(ptr_local));
            self.emit_expr(&arg.node);
            match field_type {
                WasmType::I64 => {}
                WasmType::F64 => {
                    self.instructions.push(Instruction::I64ReinterpretF64);
                }
                WasmType::I32 => {
                    self.instructions.push(Instruction::I64ExtendI32S);
                }
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: (8 + i * 8) as u64,
                    align: 3,
                    memory_index: 0,
                }));
        }

        self.instructions.push(Instruction::LocalGet(ptr_local));
    }

    // -----------------------------------------------------------------------
    // Error propagation
    // -----------------------------------------------------------------------

    fn emit_error_prop(&mut self, inner: &Spanned<Expr>) {
        self.emit_expr(&inner.node);
        let val_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::LocalSet(val_local));

        let inner_aver_type = self.infer_aver_type(&inner.node);
        let ok_wasm_type = match &inner_aver_type {
            Some(Type::Result(ok, _)) => aver_type_to_wasm(ok),
            _ => WasmType::I64,
        };

        let result_bt = wasm_encoder::BlockType::Result(ok_wasm_type.to_val_type());

        self.instructions.push(Instruction::LocalGet(val_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions.push(Instruction::I32GtS);
        self.emit_if(result_bt);
        self.instructions.push(Instruction::LocalGet(val_local));
        self.instructions.push(Instruction::Call(self.rt.obj_tag));
        self.instructions
            .push(Instruction::I32Const(value::WRAP_ERR as i32));
        self.instructions.push(Instruction::I32Eq);
        self.emit_if(result_bt);
        self.instructions.push(Instruction::LocalGet(val_local));
        self.emit_boundary_return_from_stack(self.fn_return_type, self.fn_return_is_heap);
        self.emit_else();
        self.instructions.push(Instruction::LocalGet(val_local));
        match ok_wasm_type {
            WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.unwrap)),
            WasmType::F64 => self
                .instructions
                .push(Instruction::Call(self.rt.unwrap_f64)),
            WasmType::I32 => self
                .instructions
                .push(Instruction::Call(self.rt.unwrap_i32)),
        }
        self.emit_end();
        self.emit_else();
        match ok_wasm_type {
            WasmType::I32 => self.instructions.push(Instruction::I32Const(0)),
            WasmType::I64 => self.instructions.push(Instruction::I64Const(0)),
            WasmType::F64 => self.instructions.push(Instruction::F64Const(0.0)),
        }
        self.emit_end();
    }

    // -----------------------------------------------------------------------
    // Constructors (Expr::Constructor from AST, not FnCall)
    // -----------------------------------------------------------------------

    fn emit_constructor(&mut self, name: &str, inner: &Option<Box<Spanned<Expr>>>) {
        let ctor = classify_constructor_name(name, &self.ir_ctx());
        match ctor {
            SemanticConstructor::Wrapper(kind) => {
                let wrap_tag = match kind {
                    WrapperKind::ResultOk => value::WRAP_OK,
                    WrapperKind::ResultErr => value::WRAP_ERR,
                    WrapperKind::OptionSome => value::WRAP_SOME,
                };
                if let Some(expr) = inner {
                    let inner_type = self.infer_expr_type(&expr.node);
                    self.instructions
                        .push(Instruction::I32Const(wrap_tag as i32));
                    self.emit_expr(&expr.node);
                    match inner_type {
                        WasmType::I64 => {
                            self.instructions.push(Instruction::I32Const(
                                if self.expr_is_heap_ptr(&expr.node) {
                                    1
                                } else {
                                    0
                                },
                            ));
                            self.instructions.push(Instruction::Call(self.rt.wrap));
                        }
                        WasmType::F64 => {
                            self.instructions.push(Instruction::Call(self.rt.wrap_f64));
                        }
                        WasmType::I32 => {
                            self.instructions.push(Instruction::I32Const(
                                if self.expr_is_heap_ptr(&expr.node) {
                                    1
                                } else {
                                    0
                                },
                            ));
                            self.instructions.push(Instruction::Call(self.rt.wrap_i32));
                        }
                    }
                } else {
                    self.instructions.push(Instruction::I32Const(0));
                }
            }
            SemanticConstructor::NoneValue => {
                self.instructions
                    .push(Instruction::I32Const(value::NONE_SENTINEL));
            }
            SemanticConstructor::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => {
                // Variant with single inner from Expr::Constructor syntax
                let mut args_vec = Vec::new();
                if let Some(expr) = inner {
                    args_vec.push(expr.as_ref().clone());
                }
                self.emit_variant_constructor(&qualified_type_name, &variant_name, &args_vec);
            }
            SemanticConstructor::Unknown(_) => {
                if let Some(expr) = inner {
                    self.emit_expr(&expr.node);
                } else {
                    self.instructions.push(Instruction::I32Const(0));
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // List / Tuple / Record / InterpolatedStr / FieldAccess / TailCall
    // -----------------------------------------------------------------------

    fn emit_list(&mut self, items: &[Spanned<Expr>]) {
        if items.is_empty() {
            self.instructions
                .push(Instruction::I32Const(value::EMPTY_LIST));
            return;
        }
        let elem_type = self.infer_expr_type(&items[0].node);
        let elem_is_ptr = self.expr_is_heap_ptr(&items[0].node);
        self.instructions
            .push(Instruction::I32Const(value::EMPTY_LIST));
        for item in items.iter().rev() {
            let tail_local = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::LocalSet(tail_local));
            self.emit_expr(&item.node);
            self.instructions.push(Instruction::LocalGet(tail_local));
            match elem_type {
                WasmType::F64 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.list_cons_f64));
                }
                WasmType::I32 => {
                    let tmp = self.alloc_local(WasmType::I32);
                    self.instructions.push(Instruction::LocalSet(tmp));
                    self.instructions.push(Instruction::I64ExtendI32S);
                    self.instructions.push(Instruction::LocalGet(tmp));
                    self.instructions
                        .push(Instruction::I32Const(if elem_is_ptr { 1 } else { 0 }));
                    self.instructions.push(Instruction::Call(self.rt.list_cons));
                }
                _ => {
                    self.instructions
                        .push(Instruction::I32Const(if elem_is_ptr { 1 } else { 0 }));
                    self.instructions.push(Instruction::Call(self.rt.list_cons));
                }
            }
        }
    }

    fn emit_tuple(&mut self, items: &[Spanned<Expr>]) {
        if items.is_empty() {
            self.instructions.push(Instruction::I32Const(0));
            return;
        }
        let count = items.len();
        let size = 8 + count * 8;
        let ptr_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(size as i32));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(ptr_local));
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions
            .push(Instruction::I64Const(value::make_header(
                value::OBJ_TUPLE,
                0,
                self.ptr_mask_for_exprs(items.iter()) as u64,
                count as u64,
            ) as i64));
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        for (i, item) in items.iter().enumerate() {
            let item_type = self.infer_expr_type(&item.node);
            self.instructions.push(Instruction::LocalGet(ptr_local));
            self.emit_expr(&item.node);
            match item_type {
                WasmType::I64 => {}
                WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: (8 + i * 8) as u64,
                    align: 3,
                    memory_index: 0,
                }));
        }
        self.instructions.push(Instruction::LocalGet(ptr_local));
    }

    fn emit_tuple_from_locals(&mut self, items: &[(u32, WasmType)]) {
        if items.is_empty() {
            self.instructions.push(Instruction::I32Const(0));
            return;
        }
        let ptr_mask = items
            .iter()
            .enumerate()
            .fold(0u16, |mask, (idx, (local, _))| {
                if idx < 16
                    && self
                        .local_aver_types
                        .get(local)
                        .is_some_and(|ty| self.is_heap_type(ty))
                {
                    mask | (1u16 << idx)
                } else {
                    mask
                }
            });
        let count = items.len();
        let size = 8 + count * 8;
        let ptr_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(size as i32));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(ptr_local));
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions
            .push(Instruction::I64Const(value::make_header(
                value::OBJ_TUPLE,
                0,
                ptr_mask as u64,
                count as u64,
            ) as i64));
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));

        for (i, (local, item_type)) in items.iter().enumerate() {
            self.instructions.push(Instruction::LocalGet(ptr_local));
            self.instructions.push(Instruction::LocalGet(*local));
            match item_type {
                WasmType::I64 => {}
                WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: (8 + i * 8) as u64,
                    align: 3,
                    memory_index: 0,
                }));
        }

        self.instructions.push(Instruction::LocalGet(ptr_local));
    }

    pub(super) fn emit_record_from_locals(&mut self, fields: &[(u32, WasmType)]) {
        if fields.is_empty() {
            self.instructions.push(Instruction::I32Const(0));
            return;
        }
        let ptr_mask = fields
            .iter()
            .enumerate()
            .fold(0u16, |mask, (idx, (local, _))| {
                if idx < 16
                    && self
                        .local_aver_types
                        .get(local)
                        .is_some_and(|ty| self.is_heap_type(ty))
                {
                    mask | (1u16 << idx)
                } else {
                    mask
                }
            });

        let count = fields.len();
        let size = 8 + count * 8;
        let ptr_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(size as i32));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(ptr_local));
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions
            .push(Instruction::I64Const(value::make_header(
                value::OBJ_RECORD,
                0,
                ptr_mask as u64,
                count as u64,
            ) as i64));
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));

        for (i, (local, field_type)) in fields.iter().enumerate() {
            self.instructions.push(Instruction::LocalGet(ptr_local));
            self.instructions.push(Instruction::LocalGet(*local));
            match field_type {
                WasmType::I64 => {}
                WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: (8 + i * 8) as u64,
                    align: 3,
                    memory_index: 0,
                }));
        }

        self.instructions.push(Instruction::LocalGet(ptr_local));
    }

    fn emit_independent_product(&mut self, items: &[Spanned<Expr>], unwrap: bool) {
        if !unwrap {
            self.emit_tuple(items);
            return;
        }

        let mut tuple_locals = Vec::with_capacity(items.len());

        for item in items {
            self.emit_expr(&item.node);
            let result_local = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::LocalSet(result_local));

            self.instructions.push(Instruction::LocalGet(result_local));
            self.instructions.push(Instruction::I32Const(0));
            self.instructions.push(Instruction::I32GtS);
            self.emit_if(wasm_encoder::BlockType::Empty);
            self.instructions.push(Instruction::LocalGet(result_local));
            self.instructions.push(Instruction::Call(self.rt.obj_tag));
            self.instructions
                .push(Instruction::I32Const(value::WRAP_ERR as i32));
            self.instructions.push(Instruction::I32Eq);
            self.emit_if(wasm_encoder::BlockType::Empty);
            self.instructions.push(Instruction::LocalGet(result_local));
            self.emit_boundary_return_from_stack(self.fn_return_type, self.fn_return_is_heap);
            self.emit_end();
            self.emit_else();
            self.instructions.push(Instruction::LocalGet(result_local));
            self.emit_boundary_return_from_stack(self.fn_return_type, self.fn_return_is_heap);
            self.emit_end();

            let ok_type = match self.infer_aver_type(&item.node) {
                Some(Type::Result(ok, _)) => *ok,
                _ => Type::Unknown,
            };
            let ok_wasm_type = aver_type_to_wasm(&ok_type);
            let ok_local = self.alloc_local(ok_wasm_type);
            self.instructions.push(Instruction::LocalGet(result_local));
            match ok_wasm_type {
                WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.unwrap)),
                WasmType::F64 => self
                    .instructions
                    .push(Instruction::Call(self.rt.unwrap_f64)),
                WasmType::I32 => self
                    .instructions
                    .push(Instruction::Call(self.rt.unwrap_i32)),
            }
            self.instructions.push(Instruction::LocalSet(ok_local));
            tuple_locals.push((ok_local, ok_wasm_type));
        }

        self.emit_tuple_from_locals(&tuple_locals);
    }

    fn emit_record_create(&mut self, type_name: &str, fields: &[(String, Spanned<Expr>)]) {
        // Fetch-bridge lowering: `HttpResponse(status, body, headers)`
        // construction becomes a host call into the JS bootstrap,
        // which builds and stashes the actual Response. Returns an
        // opaque handle (i32) that the user fn passes back through.
        //
        // `headers` is `Map<String, List<String>>`. We walk its
        // entries and call `response_set_header(name, value)` once
        // per (name, value) pair — multi-value headers (Set-Cookie,
        // Vary, …) come through as separate calls. Then call
        // `response_text(status, body)` to finalize.
        if matches!(self.rt.adapter, super::super::WasmAdapter::Fetch)
            && type_name == "HttpResponse"
            && let Some(&import_idx) = self.host_import_indices.get("response_text")
        {
            let status = fields.iter().find(|(n, _)| n == "status").map(|(_, e)| e);
            let body = fields.iter().find(|(n, _)| n == "body").map(|(_, e)| e);
            let headers = fields.iter().find(|(n, _)| n == "headers").map(|(_, e)| e);
            if let (Some(status_expr), Some(body_expr)) = (status, body) {
                let body_local = self.alloc_local(WasmType::I32);

                // ── Walk headers Map first (so all `set_header` calls
                // ── happen before the `response_text` finalize). The
                // ── host stashes them on the pending Response.
                if let (Some(headers_expr), Some(&set_header_idx)) =
                    (headers, self.host_import_indices.get("response_set_header"))
                {
                    self.emit_fetch_apply_headers(&headers_expr.node, set_header_idx);
                }

                // status: Int → i64 → i32
                self.emit_expr(&status_expr.node);
                self.instructions.push(Instruction::I32WrapI64);
                // body: OBJ_STRING ptr → save, push ptr+len pair
                self.emit_expr(&body_expr.node);
                self.instructions.push(Instruction::LocalSet(body_local));
                self.instructions.push(Instruction::LocalGet(body_local));
                self.instructions.push(Instruction::I32Const(8));
                self.instructions.push(Instruction::I32Add); // body_ptr
                self.instructions.push(Instruction::LocalGet(body_local));
                self.instructions
                    .push(Instruction::I64Load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
                self.instructions.push(Instruction::I64And);
                self.instructions.push(Instruction::I32WrapI64); // body_len
                self.instructions.push(Instruction::Call(import_idx));
                return;
            }
        }

        let count = fields.len();
        let size = 8 + count * 8;
        let ptr_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(size as i32));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(ptr_local));
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions
            .push(Instruction::I64Const(value::make_header(
                value::OBJ_RECORD,
                0,
                self.ptr_mask_for_exprs(fields.iter().map(|(_, expr)| expr)) as u64,
                count as u64,
            ) as i64));
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        for (i, (_name, expr)) in fields.iter().enumerate() {
            let field_type = self.infer_expr_type(&expr.node);
            self.instructions.push(Instruction::LocalGet(ptr_local));
            self.emit_expr(&expr.node);
            match field_type {
                WasmType::I64 => {}
                WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: (8 + i * 8) as u64,
                    align: 3,
                    memory_index: 0,
                }));
        }
        self.instructions.push(Instruction::LocalGet(ptr_local));
    }

    /// Lower an `Http.<method>(...)` call to the generic `http_send`
    /// host import.
    ///
    /// `method` is the literal verb ("GET", "POST", …); the data
    /// section is force-interned to carry it (see `string_literals`
    /// setup in the emitter). `has_body` distinguishes the no-body
    /// verbs (`GET` / `HEAD` / `DELETE`, args = `[url]`) from the
    /// body-bearing ones (`POST` / `PUT` / `PATCH`, args = `[url,
    /// body, contentType, headers: Map<String, List<String>>]`).
    ///
    /// The shape of every call is the same:
    ///   1. Walk the request-headers Map (body verbs only) and push
    ///      each `(name, value)` pair via
    ///      `http_add_request_header` after a clear.
    ///   2. Push (method_ptr, method_len, url_ptr, url_len,
    ///      body_ptr, body_len, ct_ptr, ct_len) and call
    ///      `http_send` — three results land on the stack:
    ///      `(status: i64, body: i32, err: i32)`.
    ///   3. Branch on `err != 0` to wrap as `Result.Err(msg)` /
    ///      `Result.Ok(HttpResponse{status, body,
    ///      headers = empty})`.
    pub(super) fn emit_http_send(
        &mut self,
        method: &'static str,
        args: &[Spanned<Expr>],
        has_body: bool,
    ) {
        use wasm_encoder::{BlockType, MemArg, ValType};

        let send_idx = match self.host_import_indices.get("http_send").copied() {
            Some(idx) => idx,
            None => {
                self.codegen_error("missing host import `http_send`");
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions.push(Instruction::I32Const(0));
                return;
            }
        };

        // Stash the arg OBJ_STRING / Map handles into locals so we
        // can push (ptr, len) pairs in any order.
        let url_local;
        let body_local;
        let ct_local;
        let headers_local;
        if has_body {
            // args = [url, body, contentType, headers]
            // Stack order at entry: url, body, ct, headers (top)
            headers_local = self.alloc_local(WasmType::I32);
            ct_local = self.alloc_local(WasmType::I32);
            body_local = self.alloc_local(WasmType::I32);
            url_local = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::LocalSet(headers_local));
            self.instructions.push(Instruction::LocalSet(ct_local));
            self.instructions.push(Instruction::LocalSet(body_local));
            self.instructions.push(Instruction::LocalSet(url_local));
        } else {
            url_local = self.alloc_local(WasmType::I32);
            body_local = 0;
            ct_local = 0;
            headers_local = 0;
            self.instructions.push(Instruction::LocalSet(url_local));
        }

        // Reset the host's pending request-headers list before each
        // call so previous-request headers don't bleed through. Then
        // walk the headers map and push entries via
        // `http_add_request_header`.
        if let Some(&clear_idx) = self.host_import_indices.get("http_clear_request_headers") {
            self.instructions.push(Instruction::Call(clear_idx));
        }
        if has_body && let Some(&add_idx) = self.host_import_indices.get("http_add_request_header")
        {
            self.emit_http_walk_headers(headers_local, add_idx);
        }

        let header_load = MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        };
        let push_str_pair = |this: &mut Self, str_local: u32| {
            this.instructions.push(Instruction::LocalGet(str_local));
            this.instructions.push(Instruction::I32Const(8));
            this.instructions.push(Instruction::I32Add); // ptr
            this.instructions.push(Instruction::LocalGet(str_local));
            this.instructions.push(Instruction::I64Load(header_load));
            this.instructions.push(Instruction::I64Const(0xFFFFFFFF));
            this.instructions.push(Instruction::I64And);
            this.instructions.push(Instruction::I32WrapI64); // len
        };

        // method (interned in the data section, always present)
        if let Some(&(offset, len)) = self.string_literals.get(method) {
            self.instructions
                .push(Instruction::I32Const(offset as i32 + 8));
            self.instructions.push(Instruction::I32Const(len as i32));
        } else {
            self.codegen_error(format!(
                "internal: HTTP method literal `{}` not interned",
                method
            ));
            self.instructions.push(Instruction::I32Const(0));
            self.instructions.push(Instruction::I32Const(0));
        }
        push_str_pair(self, url_local);
        if has_body {
            push_str_pair(self, body_local);
            push_str_pair(self, ct_local);
        } else {
            // body, contentType — empty for the no-body verbs.
            self.instructions.push(Instruction::I32Const(0));
            self.instructions.push(Instruction::I32Const(0));
            self.instructions.push(Instruction::I32Const(0));
            self.instructions.push(Instruction::I32Const(0));
        }
        self.instructions.push(Instruction::Call(send_idx));
        // Stack: [status: i64, body: i32, headers: i32, err: i32]
        let err_local = self.alloc_local(WasmType::I32);
        let resp_headers_local = self.alloc_local(WasmType::I32);
        let resp_body_local = self.alloc_local(WasmType::I32);
        let status_local = self.alloc_local(WasmType::I64);
        self.instructions.push(Instruction::LocalSet(err_local));
        self.instructions
            .push(Instruction::LocalSet(resp_headers_local));
        self.instructions
            .push(Instruction::LocalSet(resp_body_local));
        self.instructions.push(Instruction::LocalSet(status_local));

        // Branch on err != 0 → Result.Err / Result.Ok.
        self.instructions.push(Instruction::LocalGet(err_local));
        self.emit_if(BlockType::Result(ValType::I32));
        // Result.Err(err_string)
        self.instructions
            .push(Instruction::I32Const(super::super::value::WRAP_ERR as i32));
        self.instructions.push(Instruction::LocalGet(err_local));
        self.instructions.push(Instruction::I64ExtendI32U);
        self.instructions.push(Instruction::I32Const(1)); // ptr_flag
        self.instructions.push(Instruction::Call(self.rt.wrap));
        self.emit_else();
        // Result.Ok(HttpResponse{status, body, headers = empty Map})
        let response_size: i32 = 8 + 3 * 8;
        let resp_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(response_size));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(resp_local));
        // Header: OBJ_RECORD, ptr_flags = bits 1+2 (body & headers).
        self.instructions.push(Instruction::LocalGet(resp_local));
        self.instructions
            .push(Instruction::I64Const(super::super::value::make_header(
                super::super::value::OBJ_RECORD,
                0,
                0b110,
                3,
            ) as i64));
        self.instructions.push(Instruction::I64Store(header_load));
        // Field 0: status (i64)
        self.instructions.push(Instruction::LocalGet(resp_local));
        self.instructions.push(Instruction::LocalGet(status_local));
        self.instructions.push(Instruction::I64Store(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
        // Field 1: body (OBJ_STRING ptr extended to i64)
        self.instructions.push(Instruction::LocalGet(resp_local));
        self.instructions
            .push(Instruction::LocalGet(resp_body_local));
        self.instructions.push(Instruction::I64ExtendI32U);
        self.instructions.push(Instruction::I64Store(MemArg {
            offset: 16,
            align: 3,
            memory_index: 0,
        }));
        // Field 2: headers — `Map<String, List<String>>` handle the
        // host bulk-transferred from the upstream `Response.headers`
        // (or `0` when the host returned nothing — older bridges).
        self.instructions.push(Instruction::LocalGet(resp_local));
        self.instructions
            .push(Instruction::LocalGet(resp_headers_local));
        self.instructions.push(Instruction::I64ExtendI32U);
        self.instructions.push(Instruction::I64Store(MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));
        // Wrap as Result.Ok
        self.instructions
            .push(Instruction::I32Const(super::super::value::WRAP_OK as i32));
        self.instructions.push(Instruction::LocalGet(resp_local));
        self.instructions.push(Instruction::I64ExtendI32U);
        self.instructions.push(Instruction::I32Const(1));
        self.instructions.push(Instruction::Call(self.rt.wrap));
        self.emit_end();
    }

    /// Walk the headers Map<String, List<String>> in `headers_local`
    /// and push each `(name, value)` via the host import at
    /// `add_idx`. Same shape as `emit_fetch_apply_headers` but
    /// targets the request-side import.
    fn emit_http_walk_headers(&mut self, headers_local: u32, add_idx: u32) {
        use wasm_encoder::{BlockType, MemArg};

        let entries_local = self.alloc_local(WasmType::I32);
        let entry_local = self.alloc_local(WasmType::I32);
        let key_local = self.alloc_local(WasmType::I32);
        let values_local = self.alloc_local(WasmType::I32);
        let value_local = self.alloc_local(WasmType::I32);

        let header_load = MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        };

        self.instructions.push(Instruction::LocalGet(headers_local));
        self.instructions
            .push(Instruction::Call(self.rt.map_entries));
        self.instructions.push(Instruction::LocalSet(entries_local));

        self.instructions.push(Instruction::Block(BlockType::Empty));
        self.instructions.push(Instruction::Loop(BlockType::Empty));

        self.instructions.push(Instruction::LocalGet(entries_local));
        self.instructions.push(Instruction::I32Eqz);
        self.instructions.push(Instruction::BrIf(1));

        // entry = head(entries)
        self.instructions.push(Instruction::LocalGet(entries_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(entry_local));

        // key = entry.field[0]
        self.instructions.push(Instruction::LocalGet(entry_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(key_local));

        // values = entry.field[1]
        self.instructions.push(Instruction::LocalGet(entry_local));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(values_local));

        // for each value: http_add_request_header(name, value)
        self.instructions.push(Instruction::Block(BlockType::Empty));
        self.instructions.push(Instruction::Loop(BlockType::Empty));
        self.instructions.push(Instruction::LocalGet(values_local));
        self.instructions.push(Instruction::I32Eqz);
        self.instructions.push(Instruction::BrIf(1));

        self.instructions.push(Instruction::LocalGet(values_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(value_local));

        self.instructions.push(Instruction::LocalGet(key_local));
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::I32Add);
        self.instructions.push(Instruction::LocalGet(key_local));
        self.instructions.push(Instruction::I64Load(header_load));
        self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
        self.instructions.push(Instruction::I64And);
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::LocalGet(value_local));
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::I32Add);
        self.instructions.push(Instruction::LocalGet(value_local));
        self.instructions.push(Instruction::I64Load(header_load));
        self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
        self.instructions.push(Instruction::I64And);
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::Call(add_idx));

        self.instructions.push(Instruction::LocalGet(values_local));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(values_local));

        self.instructions.push(Instruction::Br(0));
        self.instructions.push(Instruction::End);
        self.instructions.push(Instruction::End);

        self.instructions.push(Instruction::LocalGet(entries_local));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(entries_local));

        self.instructions.push(Instruction::Br(0));
        self.instructions.push(Instruction::End);
        self.instructions.push(Instruction::End);
    }

    /// Walk a `Map<String, List<String>>` headers value and emit one
    /// `response_set_header(name_ptr, name_len, value_ptr, value_len)`
    /// host call per `(name, value)` pair. Multi-value entries
    /// (Set-Cookie with multiple cookies, Vary with multiple field
    /// names, …) come through as separate calls — the bridge bootstrap
    /// is expected to preserve order in the pending response headers.
    fn emit_fetch_apply_headers(&mut self, headers_expr: &Expr, set_header_idx: u32) {
        use wasm_encoder::{BlockType, MemArg};

        let entries_local = self.alloc_local(WasmType::I32);
        let entry_local = self.alloc_local(WasmType::I32);
        let key_local = self.alloc_local(WasmType::I32);
        let values_local = self.alloc_local(WasmType::I32);
        let value_local = self.alloc_local(WasmType::I32);

        let header_load = MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        };

        // entries = rt_map_entries(headers)
        self.emit_expr(headers_expr);
        self.instructions
            .push(Instruction::Call(self.rt.map_entries));
        self.instructions.push(Instruction::LocalSet(entries_local));

        // outer block + loop over entries
        self.instructions.push(Instruction::Block(BlockType::Empty));
        self.instructions.push(Instruction::Loop(BlockType::Empty));

        // if entries == 0 → break out of outer block
        self.instructions.push(Instruction::LocalGet(entries_local));
        self.instructions.push(Instruction::I32Eqz);
        self.instructions.push(Instruction::BrIf(1));

        // entry = head(entries) — tuple (name_str, values_list)
        self.instructions.push(Instruction::LocalGet(entries_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(entry_local));

        // key = entry.field[0] (OBJ_STRING ptr)
        self.instructions.push(Instruction::LocalGet(entry_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(key_local));

        // values = entry.field[1] (List<String> ptr; 0 == empty list)
        self.instructions.push(Instruction::LocalGet(entry_local));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(values_local));

        // inner block + loop over values
        self.instructions.push(Instruction::Block(BlockType::Empty));
        self.instructions.push(Instruction::Loop(BlockType::Empty));

        // if values == 0 → break out of inner block
        self.instructions.push(Instruction::LocalGet(values_local));
        self.instructions.push(Instruction::I32Eqz);
        self.instructions.push(Instruction::BrIf(1));

        // value = head(values) (OBJ_STRING ptr)
        self.instructions.push(Instruction::LocalGet(values_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(value_local));

        // call response_set_header(name_ptr, name_len, value_ptr, value_len)
        self.instructions.push(Instruction::LocalGet(key_local));
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::I32Add); // name_ptr = key+8
        self.instructions.push(Instruction::LocalGet(key_local));
        self.instructions.push(Instruction::I64Load(header_load));
        self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
        self.instructions.push(Instruction::I64And);
        self.instructions.push(Instruction::I32WrapI64); // name_len
        self.instructions.push(Instruction::LocalGet(value_local));
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::I32Add); // value_ptr = value+8
        self.instructions.push(Instruction::LocalGet(value_local));
        self.instructions.push(Instruction::I64Load(header_load));
        self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
        self.instructions.push(Instruction::I64And);
        self.instructions.push(Instruction::I32WrapI64); // value_len
        self.instructions.push(Instruction::Call(set_header_idx));

        // values = tail(values)
        self.instructions.push(Instruction::LocalGet(values_local));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(values_local));

        // continue inner loop
        self.instructions.push(Instruction::Br(0));
        self.instructions.push(Instruction::End); // end inner Loop
        self.instructions.push(Instruction::End); // end inner Block

        // entries = tail(entries)
        self.instructions.push(Instruction::LocalGet(entries_local));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(entries_local));

        // continue outer loop
        self.instructions.push(Instruction::Br(0));
        self.instructions.push(Instruction::End); // end outer Loop
        self.instructions.push(Instruction::End); // end outer Block
    }

    fn emit_record_update(
        &mut self,
        type_name: &str,
        base: &Spanned<Expr>,
        updates: &[(String, Spanned<Expr>)],
    ) {
        let Some(fields) = self.record_fields(type_name).map(|fields| fields.to_vec()) else {
            self.emit_expr(&base.node);
            return;
        };

        let base_local = self.alloc_local(WasmType::I32);
        self.emit_expr(&base.node);
        self.instructions.push(Instruction::LocalSet(base_local));

        let mut update_locals = HashMap::with_capacity(updates.len());
        for (field_name, expr) in updates {
            let field_type = self.infer_expr_type(&expr.node);
            let field_local = self.alloc_local(field_type);
            self.emit_expr(&expr.node);
            self.instructions.push(Instruction::LocalSet(field_local));
            update_locals.insert(field_name.as_str(), (field_local, field_type));
        }

        let count = fields.len();
        let field_ptr_mask = self.ptr_mask_for_types(
            &fields
                .iter()
                .map(|(_, ty)| crate::types::parse_type_str(ty))
                .collect::<Vec<_>>(),
        );
        let size = 8 + count * 8;
        let ptr_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(size as i32));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(ptr_local));
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions
            .push(Instruction::I64Const(value::make_header(
                value::OBJ_RECORD,
                0,
                field_ptr_mask as u64,
                count as u64,
            ) as i64));
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));

        for (i, (field_name, _)) in fields.iter().enumerate() {
            self.instructions.push(Instruction::LocalGet(ptr_local));
            if let Some(&(field_local, field_type)) = update_locals.get(field_name.as_str()) {
                self.instructions.push(Instruction::LocalGet(field_local));
                match field_type {
                    WasmType::I64 => {}
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                }
            } else {
                self.instructions.push(Instruction::LocalGet(base_local));
                self.instructions
                    .push(Instruction::I64Load(wasm_encoder::MemArg {
                        offset: (8 + i * 8) as u64,
                        align: 3,
                        memory_index: 0,
                    }));
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: (8 + i * 8) as u64,
                    align: 3,
                    memory_index: 0,
                }));
        }

        self.instructions.push(Instruction::LocalGet(ptr_local));
    }

    fn emit_map_literal(&mut self, entries: &[(Spanned<Expr>, Spanned<Expr>)]) {
        // Build a `List<(K, V)>` from the entries (regular OBJ_LIST_CONS
        // with an OBJ_TUPLE in each head), then fold it into a HAMT via
        // `rt_map_from_list`. Pre-HAMT we used to chain OBJ_MAP_ENTRY
        // cells and treat the chain itself as the map; the HAMT
        // runtime only walks `OBJ_HAMT` roots, so we have to convert.
        //
        // Key kind / value-ptr flag come from the static types of the
        // first entry (if any). All entries in a literal share the
        // same K/V via the inference pass, so this is correct for the
        // whole map.
        let (key_kind, value_ptr_flag) = match entries.first() {
            Some((k, v)) => {
                let k_kind = self.kind_for_aver_type(self.infer_aver_type(&k.node).as_ref());
                let v_ptr = self.value_is_heap_aver_type(self.infer_aver_type(&v.node).as_ref());
                (k_kind, v_ptr)
            }
            None => (4, 1),
        };

        self.instructions.push(Instruction::I32Const(0)); // empty list tail
        for (key, val) in entries.iter().rev() {
            let list_tmp = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::LocalSet(list_tmp));

            // Build tuple(key, value)
            let tuple_ptr = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::I32Const(24)); // 8 header + 2*8 fields
            self.instructions.push(Instruction::Call(self.rt.alloc));
            self.instructions.push(Instruction::LocalSet(tuple_ptr));
            self.instructions.push(Instruction::LocalGet(tuple_ptr));
            self.instructions
                .push(Instruction::I64Const(value::make_header(
                    value::OBJ_TUPLE,
                    0,
                    self.ptr_mask_for_types(&[
                        self.infer_aver_type(&key.node).unwrap_or(Type::Unknown),
                        self.infer_aver_type(&val.node).unwrap_or(Type::Unknown),
                    ]) as u64,
                    2,
                ) as i64));
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            // Field 0: key
            self.instructions.push(Instruction::LocalGet(tuple_ptr));
            self.emit_expr(&key.node);
            let key_type = self.infer_expr_type(&key.node);
            match key_type {
                WasmType::I64 => {}
                WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32U),
                WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }));
            // Field 1: value
            self.instructions.push(Instruction::LocalGet(tuple_ptr));
            self.emit_expr(&val.node);
            let val_type = self.infer_expr_type(&val.node);
            match val_type {
                WasmType::I64 => {}
                WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
            }
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: 16,
                    align: 3,
                    memory_index: 0,
                }));

            // Cons cell `(tuple, list_tmp)` — uses regular `rt_list_cons`
            // semantics so the runtime helper can walk it the same way
            // it walks a `Map.fromList([...])` argument.
            self.instructions.push(Instruction::LocalGet(tuple_ptr));
            self.instructions.push(Instruction::I64ExtendI32U); // head as i64
            self.instructions.push(Instruction::LocalGet(list_tmp)); // tail i32
            self.instructions.push(Instruction::I32Const(1)); // head_ptr_flag
            self.instructions.push(Instruction::Call(self.rt.list_cons));
        }

        // Fold the list of tuples into a HAMT.
        self.instructions.push(Instruction::I32Const(key_kind));
        self.instructions
            .push(Instruction::I32Const(value_ptr_flag));
        self.instructions
            .push(Instruction::Call(self.rt.map_from_list));
    }

    fn emit_field_access(&mut self, base_expr: &Spanned<Expr>, field_name: &str) {
        // Runtime field access on a record object.
        // Uppercase dotted paths (None, variant ctors, static refs) are handled
        // by classify_leaf_op in emit_expr before reaching here.
        self.emit_expr(&base_expr.node);

        // Resolve field index using base expression's type for disambiguation
        let base_type_name = self.infer_aver_type(&base_expr.node).and_then(|t| match t {
            Type::Named(name) => Some(name),
            _ => None,
        });

        // Fetch-bridge lowering: a `req.method` style read on
        // `HttpRequest` becomes a host call into the JS bootstrap
        // (which has the actual Request object). Receiver is
        // evaluated and discarded — the host has ambient request
        // state, the wasm-side handle is opaque and never deref'd.
        if matches!(self.rt.adapter, super::super::WasmAdapter::Fetch)
            && base_type_name.as_deref() == Some("HttpRequest")
        {
            let import_name = match field_name {
                "method" => Some("request_method"),
                "url" | "path" => Some("request_url"),
                "body" => Some("request_body"),
                _ => None,
            };
            if let Some(name) = import_name
                && let Some(&idx) = self.host_import_indices.get(name)
            {
                self.instructions.push(Instruction::Drop);
                self.instructions.push(Instruction::Call(idx));
                return;
            }
            // `req.headers` under the Fetch bridge — bulk-transfer
            // the host's `Headers` into a guest
            // `Map<String, List<String>>` via a single host crossing.
            // The bootstrap walks `pending.req.headers`, allocates
            // OBJ_STRINGs, and folds them through `rt_map_from_list`.
            // Multi-value entries (Set-Cookie via `getSetCookie()`,
            // Vary, …) keep separate values in the value list.
            if field_name == "headers" {
                self.instructions.push(Instruction::Drop);
                if let Some(&idx) = self.host_import_indices.get("request_headers_load") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.codegen_error("missing host import `request_headers_load`");
                    self.instructions.push(Instruction::I32Const(0));
                }
                return;
            }
            // Unknown HttpRequest field under fetch bridge — fall
            // through to standard record field access (will trap or
            // return garbage at runtime).
        }

        let field_idx = if let Some(ref type_name) = base_type_name {
            // Exact match: (type_name, field_name) → index
            self.type_fields
                .get(&(type_name.clone(), field_name.to_string()))
                .copied()
                .unwrap_or_else(|| {
                    // Fallback: first field with this name
                    self.type_fields
                        .iter()
                        .find(|((_, f), _)| f == field_name)
                        .map(|(_, &idx)| idx)
                        .unwrap_or(0)
                })
        } else {
            self.type_fields
                .iter()
                .find(|((_, f), _)| f == field_name)
                .map(|(_, &idx)| idx)
                .unwrap_or(0)
        };

        // Determine field type from type_defs
        let field_wasm_type = self.infer_record_field_type(base_expr, field_name);

        self.instructions
            .push(Instruction::I32Const(field_idx as i32));
        match field_wasm_type {
            WasmType::F64 => {
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
    }

    fn emit_interpolated_str(&mut self, parts: &[StrPart]) {
        if parts.is_empty() {
            self.emit_string_literal("");
            return;
        }
        if parts.len() == 1 {
            match &parts[0] {
                StrPart::Literal(s) => self.emit_string_literal(s),
                StrPart::Parsed(expr) => {
                    // Convert expression to string
                    self.emit_value_to_str(&expr.node);
                }
            }
            return;
        }

        // Multi-part: emit first part, then concat remaining
        self.emit_str_part(&parts[0]);
        for part in &parts[1..] {
            self.emit_str_part(part);
            self.instructions
                .push(Instruction::Call(self.rt.str_concat));
        }
    }

    fn emit_str_part(&mut self, part: &StrPart) {
        match part {
            StrPart::Literal(s) => self.emit_string_literal(s),
            StrPart::Parsed(expr) => self.emit_value_to_str(&expr.node),
        }
    }

    fn emit_tailcall(&mut self, tc: &TailCallData) {
        let fn_name = &tc.target;
        let args = tc.args.as_slice();
        let resolved_name = self.resolve_user_fn_name(fn_name);
        if let (Some(loop_depth), Some(dispatch_local), Some((member_id, param_slots))) = (
            self.tco_loop_depth,
            self.mutual_tco_dispatch_local,
            self.mutual_tco_targets.get(&resolved_name).cloned(),
        ) {
            // Uniform-signature mutual-TCO + no-alloc body: the trampoline
            // keeps one shared slot row, and there is no GC compaction to
            // run between iterations, so the standard
            // "eval → tmp → target" double copy collapses into a single
            // reverse-order write directly into the target slots. Reads
            // inside arg expressions still see the *old* slot values
            // because we evaluate every arg (push to wasm stack) before
            // any LocalSet, then drain the stack in reverse.
            let direct_to_slots =
                self.mutual_tco_uniform && self.is_no_alloc && param_slots.len() == args.len();

            for arg in args {
                self.emit_expr(&arg.node);
            }

            let tmp_base = if direct_to_slots {
                u32::MAX // unused
            } else {
                let base = self.next_local;
                for arg in args {
                    let wt = self.infer_expr_type(&arg.node);
                    self.alloc_local(wt);
                }
                for i in (0..args.len()).rev() {
                    self.instructions
                        .push(Instruction::LocalSet(base + i as u32));
                }
                base
            };
            // Mutual TCO: adaptive compaction based on garbage accumulation.
            // Compact when heap has grown >16KB beyond the post-compaction
            // watermark. This is safe against the drawRows truncation issue
            // (which masks per-iteration growth from iter_mark) because
            // watermark tracks absolute growth since last compaction, not
            // per-iteration delta.
            //
            // Pure no-alloc groups (mandelStep ↔ mandelIter, etc.) elide
            // this entirely — `is_no_alloc` is set up front by
            // `emit_mutual_tco_trampoline`, and `iter_mark_local` is left
            // None alongside it, so the branches below all fall through.
            if self.is_no_alloc {
                // intentionally empty
            } else if let Some(iter_mark) = self.iter_mark_local {
                let fn_mark = self.boundary_mark_local.unwrap_or(iter_mark);
                if let Some(watermark) = self.gc_watermark_local {
                    // if (heap_ptr - watermark > 8192) → compact + reset watermark
                    self.instructions.push(Instruction::GlobalGet(0));
                    self.instructions.push(Instruction::LocalGet(watermark));
                    self.instructions.push(Instruction::I32Sub);
                    self.instructions.push(Instruction::I32Const(16384));
                    self.instructions.push(Instruction::I32GtU);
                    self.emit_if(wasm_encoder::BlockType::Empty);
                    self.instructions.push(Instruction::LocalGet(fn_mark));
                    self.emit_tco_compaction(args, tmp_base);
                    // Update watermark to post-compaction heap_ptr
                    self.instructions.push(Instruction::GlobalGet(0));
                    self.instructions.push(Instruction::LocalSet(watermark));
                    self.emit_end();
                } else {
                    // Fallback: original yard heuristic
                    self.instructions.push(Instruction::GlobalGet(0));
                    self.instructions.push(Instruction::LocalGet(iter_mark));
                    self.instructions.push(Instruction::I32Sub);
                    self.instructions.push(Instruction::I32Const(256));
                    self.instructions.push(Instruction::I32GtU);
                    self.emit_if(wasm_encoder::BlockType::Empty);
                    self.instructions.push(Instruction::LocalGet(fn_mark));
                    self.emit_tco_compaction(args, tmp_base);
                    self.emit_end();
                }
            } else if let Some(mark_local) = self.boundary_mark_local {
                self.instructions.push(Instruction::LocalGet(mark_local));
                self.emit_tco_compaction(args, tmp_base);
            }
            if direct_to_slots {
                // Args are still on the wasm stack, in left-to-right order.
                // Drain to target slots in reverse so the rightmost arg
                // pops first.
                for slot in param_slots.iter().rev() {
                    self.instructions.push(Instruction::LocalSet(*slot));
                }
            } else {
                for (i, slot) in param_slots.iter().enumerate() {
                    self.instructions
                        .push(Instruction::LocalGet(tmp_base + i as u32));
                    self.instructions.push(Instruction::LocalSet(*slot));
                }
            }
            self.instructions
                .push(Instruction::I32Const(member_id as i32));
            self.instructions
                .push(Instruction::LocalSet(dispatch_local));

            let br_depth = self.block_depth - loop_depth;
            self.instructions.push(Instruction::Br(br_depth));
            self.instructions.push(Instruction::Unreachable);
            return;
        }

        // Only use TCO loop for SELF-calls, not mutual calls
        if let Some(loop_depth) = self
            .tco_loop_depth
            .filter(|_| resolved_name == self.current_fn_name)
        {
            for arg in args {
                self.emit_expr(&arg.node);
            }
            let arg_count = args.len();
            let tmp_base = self.next_local;
            for arg in args.iter() {
                let wt = self.infer_expr_type(&arg.node);
                self.alloc_local(wt);
            }
            for i in (0..arg_count).rev() {
                self.instructions
                    .push(Instruction::LocalSet(tmp_base + i as u32));
            }
            // Yard semantics: compact when this iteration's growth
            // exceeds the threshold. The original 256-byte cutoff was
            // calibrated for the linked-list map era (~24B per cons
            // cell) and fires *every* iteration once HAMT or any other
            // structurally-sharing data type allocates 4-5 nodes per
            // step (~600B) — death by a thousand compactions. 16384
            // matches the mutual-TCO branch's watermark threshold.
            //
            // Pure no-alloc self-recursive fns skip the whole compaction
            // pass — they don't generate garbage to begin with.
            if self.is_no_alloc {
                // intentionally empty
            } else if let Some(iter_mark) = self.iter_mark_local {
                let fn_mark = self.boundary_mark_local.unwrap_or(iter_mark);
                self.instructions.push(Instruction::GlobalGet(0));
                self.instructions.push(Instruction::LocalGet(iter_mark));
                self.instructions.push(Instruction::I32Sub);
                self.instructions.push(Instruction::I32Const(16384));
                self.instructions.push(Instruction::I32GtU);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.instructions.push(Instruction::LocalGet(fn_mark));
                self.emit_tco_compaction(args, tmp_base);
                self.emit_end();
            } else if let Some(mark_local) = self.boundary_mark_local {
                self.instructions.push(Instruction::LocalGet(mark_local));
                self.emit_tco_compaction(args, tmp_base);
            }
            for i in 0..arg_count {
                self.instructions
                    .push(Instruction::LocalGet(tmp_base + i as u32));
                self.instructions.push(Instruction::LocalSet(i as u32));
            }
            let br_depth = self.block_depth - loop_depth;
            self.instructions.push(Instruction::Br(br_depth));
            self.instructions.push(Instruction::Unreachable);
        } else {
            for arg in args {
                self.emit_expr(&arg.node);
            }
            if let Some(&fn_idx) = self.fn_indices.get(resolved_name.as_str()) {
                self.instructions.push(Instruction::Call(fn_idx));
            } else {
                self.codegen_error(format!(
                    "missing function index for tail call to `{}`",
                    resolved_name
                ));
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.emit_default_value(self.fn_return_type);
            }
        }
    }

    // -----------------------------------------------------------------------
    /// Try to emit `Option.withDefault(Vector.set(v, i, x), v)` as an
    /// inline owned-mutate + return-the-same-slot, when `v` is a
    /// `Resolved` local whose `last_use` flag is set. Mirrors the VM's
    /// `vec_set_nv_owned` path: the heap object behind the slot has no
    /// observer past this expression (last-use analysis guarantees it),
    /// so we can skip the copy and just `i64.store` the new cell.
    ///
    /// Returns the original slot pointer regardless of whether the index
    /// was in bounds — that's how the fused `Option.withDefault(_, v)`
    /// shape is meant to behave (out-of-bounds keeps the original).
    fn try_emit_vec_set_owned_keep(
        &mut self,
        option_expr: &Spanned<Expr>,
        default_expr: &Spanned<Expr>,
    ) -> bool {
        // option_expr must be `Vector.set(v, i, x)` — three args.
        let Expr::FnCall(callee, inner_args) = &option_expr.node else {
            return false;
        };
        if inner_args.len() != 3 {
            return false;
        }
        let inner_plan = classify_call_plan(&callee.node, &self.ir_ctx());
        if !matches!(inner_plan, CallPlan::Builtin(ref n) if n == "Vector.set") {
            return false;
        }

        // The first arg of `Vector.set` and the `default_expr` of
        // `Option.withDefault` must be the SAME local slot — that's
        // what makes the fused shape meaningful.
        let vec_arg = &inner_args[0].node;
        if vec_arg != &default_expr.node {
            return false;
        }

        // The vec arg must be a `Resolved` slot with `last_use = true`.
        // Anything else (composite expression, non-last-use) means the
        // heap object is potentially observed elsewhere — bail out to
        // the regular copy-on-write path.
        let Expr::Resolved { name, last_use, .. } = vec_arg else {
            return false;
        };
        if !last_use.0 {
            return false;
        }
        let Some(&vec_local_idx) = self.locals.get(name) else {
            return false;
        };

        // Allocate scratch locals for the mutate. We don't reuse the
        // resolved local for `idx` / `val` because the source-side
        // expressions might themselves load from other locals.
        let idx_local = self.alloc_local(WasmType::I64);
        let val_local = self.alloc_local(WasmType::I64);
        let len_local = self.alloc_local(WasmType::I32);
        let i_local = self.alloc_local(WasmType::I32);

        // Evaluate idx (i64) and val (i64) onto stack, stash into locals.
        self.emit_expr(&inner_args[1].node);
        self.instructions.push(Instruction::LocalSet(idx_local));
        self.emit_expr(&inner_args[2].node);
        match self.infer_expr_type(&inner_args[2].node) {
            WasmType::I64 => {}
            WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
            WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
        }
        self.instructions.push(Instruction::LocalSet(val_local));

        // len = header & 0xFFFFFFFF.
        self.instructions.push(Instruction::LocalGet(vec_local_idx));
        self.instructions
            .push(Instruction::I64Load(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
        self.instructions.push(Instruction::I64And);
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::LocalSet(len_local));
        // i = i32(idx).
        self.instructions.push(Instruction::LocalGet(idx_local));
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::LocalSet(i_local));

        // if (i >= 0 && i < len): vec[i] = val
        self.instructions.push(Instruction::LocalGet(i_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions.push(Instruction::I32GeS);
        self.instructions.push(Instruction::LocalGet(i_local));
        self.instructions.push(Instruction::LocalGet(len_local));
        self.instructions.push(Instruction::I32LtS);
        self.instructions.push(Instruction::I32And);
        self.emit_if(wasm_encoder::BlockType::Empty);
        // addr = vec + i*8
        self.instructions.push(Instruction::LocalGet(vec_local_idx));
        self.instructions.push(Instruction::LocalGet(i_local));
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::I32Mul);
        self.instructions.push(Instruction::I32Add);
        self.instructions.push(Instruction::LocalGet(val_local));
        // Store with offset 8 to skip the 8-byte object header.
        self.instructions
            .push(Instruction::I64Store(wasm_encoder::MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }));
        self.emit_end();

        // Result of the whole expression: the (possibly-mutated) vec.
        self.instructions.push(Instruction::LocalGet(vec_local_idx));
        true
    }

    /// Try to emit `Option.withDefault(Vector.get(v, i), default)` as inline
    /// bounds check + direct load, avoiding the Option wrapper allocation.
    /// Returns true if the pattern was matched and code was emitted.
    fn try_emit_vec_get_or_default(
        &mut self,
        option_expr: &Spanned<Expr>,
        default_expr: &Spanned<Expr>,
    ) -> bool {
        // Check: option_expr is FnCall(Vector.get, [vec, idx])
        let Expr::FnCall(callee, inner_args) = &option_expr.node else {
            return false;
        };
        if inner_args.len() != 2 {
            return false;
        }
        let inner_plan = classify_call_plan(&callee.node, &self.ir_ctx());
        if !matches!(inner_plan, CallPlan::Builtin(ref n) if n == "Vector.get") {
            return false;
        }
        // Check: default is a literal (Int or Bool)
        let Expr::Literal(ref default_lit) = default_expr.node else {
            return false;
        };

        let result_type = self.infer_expr_type(&default_expr.node);

        // Emit: vec, idx (evaluate Vector.get's args but don't call vec_get)
        self.emit_expr(&inner_args[0].node); // vec: i32
        self.emit_expr(&inner_args[1].node); // idx: i64

        let vec_local = self.alloc_local(WasmType::I32);
        let idx_local = self.alloc_local(WasmType::I64);
        let len_local = self.alloc_local(WasmType::I32);
        let i_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::LocalSet(idx_local));
        self.instructions.push(Instruction::LocalSet(vec_local));

        // len = header & 0xFFFFFFFF
        self.instructions.push(Instruction::LocalGet(vec_local));
        self.instructions
            .push(Instruction::I64Load(wasm_encoder::MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
        self.instructions.push(Instruction::I64And);
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::LocalSet(len_local));
        // i = i32(idx)
        self.instructions.push(Instruction::LocalGet(idx_local));
        self.instructions.push(Instruction::I32WrapI64);
        self.instructions.push(Instruction::LocalSet(i_local));
        // Bounds check: i < 0 || i >= len → default
        self.instructions.push(Instruction::LocalGet(i_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions.push(Instruction::I32LtS);
        self.instructions.push(Instruction::LocalGet(i_local));
        self.instructions.push(Instruction::LocalGet(len_local));
        self.instructions.push(Instruction::I32GeS);
        self.instructions.push(Instruction::I32Or);
        self.emit_if(wasm_encoder::BlockType::Result(result_type.to_val_type()));
        // Out of bounds: default literal
        self.emit_literal(default_lit);
        self.emit_else();
        // In bounds: load vec[i] directly
        self.instructions.push(Instruction::LocalGet(vec_local));
        self.instructions.push(Instruction::LocalGet(i_local));
        self.instructions.push(Instruction::I32Const(8));
        self.instructions.push(Instruction::I32Mul);
        self.instructions.push(Instruction::I32Add);
        self.instructions
            .push(Instruction::I64Load(wasm_encoder::MemArg {
                offset: 8,
                align: 3,
                memory_index: 0,
            }));
        // Convert i64 to result type if needed
        match result_type {
            WasmType::I64 => {}
            WasmType::F64 => self.instructions.push(Instruction::F64ReinterpretI64),
            WasmType::I32 => self.instructions.push(Instruction::I32WrapI64),
        }
        self.emit_end();
        true
    }

    // Helpers
    // -----------------------------------------------------------------------

    pub(super) fn emit_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Int(i) => self.instructions.push(Instruction::I64Const(*i)),
            Literal::Float(f) => self
                .instructions
                .push(Instruction::F64Const(f64::from_bits(f.to_bits()))),
            Literal::Bool(b) => {
                self.instructions
                    .push(Instruction::I32Const(if *b { 1 } else { 0 }))
            }
            Literal::Str(s) => self.emit_string_literal(s),
            Literal::Unit => self.instructions.push(Instruction::I32Const(0)),
        }
    }

    pub(super) fn emit_string_literal(&mut self, s: &str) {
        if let Some(&(offset, _len)) = self.string_literals.get(s) {
            self.instructions.push(Instruction::I32Const(offset as i32));
        } else {
            self.codegen_error(format!("missing interned string literal `{}`", s));
            self.emit_default_value(WasmType::I32);
        }
    }

    pub(super) fn emit_default_init(&mut self, local: u32, wt: WasmType) {
        self.emit_default_value(wt);
        self.instructions.push(Instruction::LocalSet(local));
    }

    /// Emit collect_begin / retain / collect_end / rebase for TCO branch.
    /// The mark i32 must already be on the WASM stack when this is called.
    fn emit_tco_compaction(&mut self, args: &[Spanned<Expr>], tmp_base: u32) {
        self.instructions
            .push(Instruction::Call(self.rt.collect_begin));
        for (arg_idx, arg) in args.iter().enumerate() {
            if self.expr_is_heap_ptr(&arg.node) {
                self.instructions
                    .push(Instruction::LocalGet(tmp_base + arg_idx as u32));
                self.instructions
                    .push(Instruction::Call(self.rt.retain_i32));
                self.instructions
                    .push(Instruction::LocalSet(tmp_base + arg_idx as u32));
            }
        }
        self.instructions
            .push(Instruction::Call(self.rt.collect_end));
        for (arg_idx, arg) in args.iter().enumerate() {
            if self.expr_is_heap_ptr(&arg.node) {
                self.instructions
                    .push(Instruction::LocalGet(tmp_base + arg_idx as u32));
                self.instructions
                    .push(Instruction::Call(self.rt.rebase_i32));
                self.instructions
                    .push(Instruction::LocalSet(tmp_base + arg_idx as u32));
            }
        }
    }
}
