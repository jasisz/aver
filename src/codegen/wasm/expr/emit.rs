/// Core expression emission for ExprEmitter.
///
/// emit_expr, emit_block, emit_binop, emit_fn_call, emit_wrap,
/// emit_variant_constructor, emit_constructor, emit_error_prop,
/// emit_list, emit_tuple, emit_record_create, emit_field_access,
/// emit_map_literal, emit_interpolated_str, emit_str_part,
/// emit_tailcall, emit_literal, emit_string_literal, emit_default_init.
use std::collections::HashMap;

use wasm_encoder::Instruction;

use crate::ast::{BinOp, Expr, Literal, Spanned, Stmt, StrPart};
use crate::ir::{
    CallPlan, SemanticConstructor, WrapperKind, classify_call_plan, classify_constructor_name,
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
            Expr::Ident(name) => {
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
            Expr::Resolved(slot) => {
                self.instructions.push(Instruction::LocalGet(*slot as u32));
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
            Expr::Attr(base_expr, field_name) => {
                self.emit_field_access(base_expr, field_name);
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

    fn emit_record_create(&mut self, _type_name: &str, fields: &[(String, Spanned<Expr>)]) {
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
        // Build association list from entries: each entry is a (key, value) tuple in a cons cell
        // Start with empty map
        self.instructions.push(Instruction::I32Const(0)); // empty
        for (key, val) in entries.iter().rev() {
            let map_tmp = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::LocalSet(map_tmp));
            // Build tuple(key, value)
            let tuple_ptr = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::I32Const(24)); // 8 header + 2*8 fields
            self.instructions.push(Instruction::Call(self.rt.alloc));
            self.instructions.push(Instruction::LocalSet(tuple_ptr));
            // Header
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
            // Map entry cons cell with OBJ_MAP_ENTRY kind
            let entry_ptr = self.alloc_local(WasmType::I32);
            self.instructions.push(Instruction::I32Const(24));
            self.instructions.push(Instruction::Call(self.rt.alloc));
            self.instructions.push(Instruction::LocalSet(entry_ptr));
            self.instructions.push(Instruction::LocalGet(entry_ptr));
            self.instructions
                .push(Instruction::I64Const(
                    value::make_header(value::OBJ_MAP_ENTRY, 0, 0, 2) as i64,
                ));
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            self.instructions.push(Instruction::LocalGet(entry_ptr));
            self.instructions.push(Instruction::LocalGet(tuple_ptr));
            self.instructions.push(Instruction::I64ExtendI32U);
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }));
            self.instructions.push(Instruction::LocalGet(entry_ptr));
            self.instructions.push(Instruction::LocalGet(map_tmp));
            self.instructions.push(Instruction::I64ExtendI32S);
            self.instructions
                .push(Instruction::I64Store(wasm_encoder::MemArg {
                    offset: 16,
                    align: 3,
                    memory_index: 0,
                }));
            self.instructions.push(Instruction::LocalGet(entry_ptr));
        }
    }

    fn emit_field_access(&mut self, base_expr: &Spanned<Expr>, field_name: &str) {
        // Check if this is an uppercase dotted path (type/namespace reference, not field access)
        if let Expr::Ident(base_name) = &base_expr.node
            && base_name.chars().next().is_some_and(|c| c.is_uppercase())
        {
            let qualified = format!("{}.{}", base_name, field_name);
            let ctor = classify_constructor_name(&qualified, &self.ir_ctx());
            match ctor {
                SemanticConstructor::NoneValue => {
                    self.instructions
                        .push(Instruction::I32Const(value::NONE_SENTINEL));
                    return;
                }
                SemanticConstructor::TypeConstructor {
                    qualified_type_name,
                    variant_name,
                } => {
                    self.emit_variant_constructor(&qualified_type_name, &variant_name, &[]);
                    return;
                }
                _ => {}
            }
        }

        // Runtime field access on a record object
        self.emit_expr(&base_expr.node);

        // Resolve field index using base expression's type for disambiguation
        let base_type_name = self.infer_aver_type(&base_expr.node).and_then(|t| match t {
            Type::Named(name) => Some(name),
            _ => None,
        });
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

    fn emit_tailcall(&mut self, tc: &(String, Vec<Spanned<Expr>>)) {
        let (fn_name, args) = tc;
        let resolved_name = self.resolve_user_fn_name(fn_name);
        if let (Some(loop_depth), Some(dispatch_local), Some((member_id, param_slots))) = (
            self.tco_loop_depth,
            self.mutual_tco_dispatch_local,
            self.mutual_tco_targets.get(&resolved_name).cloned(),
        ) {
            for arg in args {
                self.emit_expr(&arg.node);
            }

            let tmp_base = self.next_local;
            for arg in args {
                let wt = self.infer_expr_type(&arg.node);
                self.alloc_local(wt);
            }
            for i in (0..args.len()).rev() {
                self.instructions
                    .push(Instruction::LocalSet(tmp_base + i as u32));
            }
            // Yard semantics: skip compaction when heap barely grew (accumulator
            // pattern like stepLoop — O(1)), full GC from fn_mark otherwise
            // (replacement pattern like gameLoop — collects dead old values).
            if let Some(iter_mark) = self.iter_mark_local {
                let fn_mark = self.boundary_mark_local.unwrap_or(iter_mark);
                // if (heap_ptr - iter_mark > 256) → full GC, else skip
                self.instructions.push(Instruction::GlobalGet(0));
                self.instructions.push(Instruction::LocalGet(iter_mark));
                self.instructions.push(Instruction::I32Sub);
                self.instructions.push(Instruction::I32Const(256));
                self.instructions.push(Instruction::I32GtU);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.instructions.push(Instruction::LocalGet(fn_mark));
                self.emit_tco_compaction(args, tmp_base);
                self.emit_end();
            } else if let Some(mark_local) = self.boundary_mark_local {
                self.instructions.push(Instruction::LocalGet(mark_local));
                self.emit_tco_compaction(args, tmp_base);
            }
            for (i, slot) in param_slots.iter().enumerate() {
                self.instructions
                    .push(Instruction::LocalGet(tmp_base + i as u32));
                self.instructions.push(Instruction::LocalSet(*slot));
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
            // Yard semantics: skip when heap barely grew, full GC otherwise.
            if let Some(iter_mark) = self.iter_mark_local {
                let fn_mark = self.boundary_mark_local.unwrap_or(iter_mark);
                self.instructions.push(Instruction::GlobalGet(0));
                self.instructions.push(Instruction::LocalGet(iter_mark));
                self.instructions.push(Instruction::I32Sub);
                self.instructions.push(Instruction::I32Const(256));
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
