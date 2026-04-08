/// Core expression emission for ExprEmitter.
///
/// emit_expr, emit_block, emit_binop, emit_fn_call, emit_wrap,
/// emit_variant_constructor, emit_constructor, emit_error_prop,
/// emit_list, emit_tuple, emit_record_create, emit_field_access,
/// emit_map_literal, emit_interpolated_str, emit_str_part,
/// emit_tailcall, emit_literal, emit_string_literal, emit_default_init.
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
                    self.instructions.push(Instruction::I32Const(0));
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
            Expr::RecordUpdate { .. } | Expr::IndependentProduct(_, _) => {
                self.instructions.push(Instruction::I32Const(0));
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

        // String equality: use str_eq runtime function for content comparison
        if matches!(op, BinOp::Eq | BinOp::Neq) && operand_type == WasmType::I32 {
            let lhs_aver = self.infer_aver_type(&lhs.node);
            if matches!(lhs_aver, Some(Type::Str)) {
                self.emit_expr(&lhs.node);
                self.emit_expr(&rhs.node);
                self.instructions.push(Instruction::Call(self.rt.str_eq));
                if matches!(op, BinOp::Neq) {
                    self.instructions.push(Instruction::I32Eqz); // invert
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
            (BinOp::Add, WasmType::I64) => Instruction::I64Add,
            (BinOp::Add, WasmType::F64) => Instruction::F64Add,
            (BinOp::Sub, WasmType::I64) => Instruction::I64Sub,
            (BinOp::Sub, WasmType::F64) => Instruction::F64Sub,
            (BinOp::Mul, WasmType::I64) => Instruction::I64Mul,
            (BinOp::Mul, WasmType::F64) => Instruction::F64Mul,
            (BinOp::Div, WasmType::I64) => Instruction::I64DivS,
            (BinOp::Div, WasmType::F64) => Instruction::F64Div,
            (BinOp::Eq, WasmType::I64) => Instruction::I64Eq,
            (BinOp::Eq, WasmType::F64) => Instruction::F64Eq,
            (BinOp::Eq, WasmType::I32) => Instruction::I32Eq,
            (BinOp::Neq, WasmType::I64) => Instruction::I64Ne,
            (BinOp::Neq, WasmType::F64) => Instruction::F64Ne,
            (BinOp::Neq, WasmType::I32) => Instruction::I32Ne,
            (BinOp::Lt, WasmType::I64) => Instruction::I64LtS,
            (BinOp::Lt, WasmType::F64) => Instruction::F64Lt,
            (BinOp::Lt, WasmType::I32) => Instruction::I32LtS,
            (BinOp::Gt, WasmType::I64) => Instruction::I64GtS,
            (BinOp::Gt, WasmType::F64) => Instruction::F64Gt,
            (BinOp::Gt, WasmType::I32) => Instruction::I32GtS,
            (BinOp::Lte, WasmType::I64) => Instruction::I64LeS,
            (BinOp::Lte, WasmType::F64) => Instruction::F64Le,
            (BinOp::Lte, WasmType::I32) => Instruction::I32LeS,
            (BinOp::Gte, WasmType::I64) => Instruction::I64GeS,
            (BinOp::Gte, WasmType::F64) => Instruction::F64Ge,
            (BinOp::Gte, WasmType::I32) => Instruction::I32GeS,
            _ => Instruction::I64Add, // fallback
        };
        self.instructions.push(instr);
    }

    // -----------------------------------------------------------------------
    // Function calls -- via IR CallPlan
    // -----------------------------------------------------------------------

    fn emit_fn_call(&mut self, callee: &Spanned<Expr>, args: &[Spanned<Expr>]) {
        let plan = classify_call_plan(&callee.node, &self.ir_ctx());

        match plan {
            CallPlan::Function(ref name) => {
                for arg in args {
                    self.emit_expr(&arg.node);
                }
                if let Some(&fn_idx) = self.fn_indices.get(name.as_str()) {
                    self.instructions.push(Instruction::Call(fn_idx));
                } else {
                    for _ in args {
                        self.instructions.push(Instruction::Drop);
                    }
                    self.instructions.push(Instruction::I32Const(0));
                }
            }

            CallPlan::Wrapper(kind) => {
                if args.len() == 1 {
                    self.emit_expr(&args[0].node);
                    self.emit_wrap(kind, &args[0]);
                } else {
                    self.instructions.push(Instruction::I32Const(0));
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
                for arg in args {
                    self.emit_expr(&arg.node);
                }
                self.emit_builtin_call(name, args);
            }

            CallPlan::Dynamic => {
                for arg in args {
                    self.emit_expr(&arg.node);
                }
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions.push(Instruction::I32Const(0));
            }
        }
    }

    /// Emit a wrapper constructor. Value is already on the stack.
    fn emit_wrap(&mut self, kind: WrapperKind, arg: &Spanned<Expr>) {
        let inner_type = self.infer_expr_type(&arg.node);
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
            WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.wrap)),
            WasmType::F64 => self.instructions.push(Instruction::Call(self.rt.wrap_f64)),
            WasmType::I32 => self.instructions.push(Instruction::Call(self.rt.wrap_i32)),
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
                0,
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
        self.instructions.push(Instruction::Return);
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
                            self.instructions.push(Instruction::Call(self.rt.wrap));
                        }
                        WasmType::F64 => {
                            self.instructions.push(Instruction::Call(self.rt.wrap_f64));
                        }
                        WasmType::I32 => {
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
                    self.instructions.push(Instruction::Call(self.rt.list_cons));
                }
                _ => {
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
            .push(Instruction::I64Const(
                value::make_header(value::OBJ_TUPLE, 0, 0, count as u64) as i64,
            ));
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

    fn emit_record_create(&mut self, _type_name: &str, fields: &[(String, Spanned<Expr>)]) {
        let count = fields.len();
        let size = 8 + count * 8;
        let ptr_local = self.alloc_local(WasmType::I32);
        self.instructions.push(Instruction::I32Const(size as i32));
        self.instructions.push(Instruction::Call(self.rt.alloc));
        self.instructions.push(Instruction::LocalSet(ptr_local));
        self.instructions.push(Instruction::LocalGet(ptr_local));
        self.instructions
            .push(Instruction::I64Const(
                value::make_header(value::OBJ_RECORD, 0, 0, count as u64) as i64,
            ));
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
                .push(Instruction::I64Const(
                    value::make_header(value::OBJ_TUPLE, 0, 0, 2) as i64,
                ));
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
            // Cons(tuple, map)
            self.instructions.push(Instruction::LocalGet(tuple_ptr));
            self.instructions.push(Instruction::I64ExtendI32U);
            self.instructions.push(Instruction::LocalGet(map_tmp));
            self.instructions.push(Instruction::Call(self.rt.list_cons));
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
        let field_idx = self
            .type_fields
            .iter()
            .find(|((_, f), _)| f == field_name)
            .map(|(_, &idx)| idx)
            .unwrap_or(0);

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
        // Only use TCO loop for SELF-calls, not mutual calls
        if let Some(loop_depth) = self
            .tco_loop_depth
            .filter(|_| fn_name == &self.current_fn_name)
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
            if let Some(&fn_idx) = self.fn_indices.get(fn_name.as_str()) {
                self.instructions.push(Instruction::Call(fn_idx));
            } else {
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions.push(Instruction::I32Const(0));
            }
        }
    }

    // -----------------------------------------------------------------------
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
            self.instructions.push(Instruction::I32Const(0));
        }
    }

    pub(super) fn emit_default_init(&mut self, local: u32, wt: WasmType) {
        match wt {
            WasmType::I32 => self.instructions.push(Instruction::I32Const(0)),
            WasmType::I64 => self.instructions.push(Instruction::I64Const(0)),
            WasmType::F64 => self.instructions.push(Instruction::F64Const(0.0)),
        }
        self.instructions.push(Instruction::LocalSet(local));
    }
}
