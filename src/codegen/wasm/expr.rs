/// Expression emission: walks the Aver AST and emits WASM instructions.
///
/// Uses the shared `ir::` lowering infrastructure (CallPlan, MatchDispatchPlan,
/// SemanticConstructor) instead of raw AST pattern matching.
///
/// Typed ABI: Int→i64, Float→f64, Bool→i32, String→i32(ptr),
/// Result/Option/List/Record/Variant→i32(ptr). No tagged values for scalars.
use std::collections::HashMap;

use wasm_encoder::Instruction;

use crate::ast::{BinOp, Expr, FnBody, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart};
use crate::codegen::CodegenContext;
use crate::codegen::common::is_user_type;
use crate::ir::{
    self, BoolSubjectPlan, CallLowerCtx, CallPlan, DispatchBindingPlan, MatchDispatchPlan,
    SemanticConstructor, WrapperKind, classify_call_plan, classify_constructor_name,
    classify_match_dispatch_plan,
};
use crate::types::Type;

use super::runtime::RuntimeFuncIndices;
use super::types::{WasmType, aver_type_to_wasm};
use super::value;

/// Interned string literal: (data_offset_in_memory, byte_length).
pub(super) type StringLiteral = (u32, u32);

// ---------------------------------------------------------------------------
// IR context adapter
// ---------------------------------------------------------------------------

/// Adapter for the shared IR lowering layer.
struct WasmCallCtx<'a> {
    ctx: &'a CodegenContext,
    locals: &'a HashMap<String, u32>,
}

impl CallLowerCtx for WasmCallCtx<'_> {
    fn is_local_value(&self, name: &str) -> bool {
        self.locals.contains_key(name)
    }

    fn is_user_type(&self, name: &str) -> bool {
        is_user_type(name, self.ctx)
    }

    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
        let mut best = None;
        for (dot_idx, _) in dotted.match_indices('.') {
            let prefix = &dotted[..dot_idx];
            let suffix = &dotted[dot_idx + 1..];
            if self.ctx.module_prefixes.contains(prefix)
                && best.is_none_or(|existing: (&str, &str)| prefix.len() > existing.0.len())
            {
                best = Some((prefix, suffix));
            }
        }
        best
    }
}

// ---------------------------------------------------------------------------
// Variant registry: type_name → [(variant_name, tag, field_type_names)]
// ---------------------------------------------------------------------------

/// Info about a single variant in a sum type.
#[derive(Debug, Clone)]
pub(super) struct VariantInfo {
    pub tag: u32,
    pub field_types: Vec<String>, // type annotation strings from AST
}

/// Build variant registry from type_defs.
pub(super) fn build_variant_registry(
    ctx: &CodegenContext,
) -> HashMap<(String, String), VariantInfo> {
    let mut registry = HashMap::new();
    let mut process_td = |td: &crate::ast::TypeDef, prefix: Option<&str>| {
        if let crate::ast::TypeDef::Sum { name, variants, .. } = td {
            for (tag, variant) in variants.iter().enumerate() {
                let qualified_type = match prefix {
                    Some(p) => format!("{}.{}", p, name),
                    None => name.clone(),
                };
                registry.insert(
                    (qualified_type.clone(), variant.name.clone()),
                    VariantInfo {
                        tag: tag as u32,
                        field_types: variant.fields.clone(),
                    },
                );
                // Also register bare name
                if prefix.is_some() {
                    registry.insert(
                        (name.clone(), variant.name.clone()),
                        VariantInfo {
                            tag: tag as u32,
                            field_types: variant.fields.clone(),
                        },
                    );
                }
            }
        }
    };

    for td in &ctx.type_defs {
        process_td(td, None);
    }
    for module in &ctx.modules {
        for td in &module.type_defs {
            process_td(td, Some(&module.prefix));
        }
    }
    registry
}

/// Context for emitting expressions within a single function body.
pub(super) struct ExprEmitter<'a> {
    pub locals: HashMap<String, u32>,
    pub next_local: u32,
    pub fn_indices: &'a HashMap<String, u32>,
    pub rt: &'a RuntimeFuncIndices,
    pub instructions: Vec<Instruction<'a>>,
    pub string_literals: &'a HashMap<String, StringLiteral>,
    pub type_fields: &'a HashMap<(String, String), u32>,
    pub block_depth: u32,
    pub tco_loop_depth: Option<u32>,
    pub fn_sigs: &'a HashMap<String, (Vec<Type>, Type, Vec<String>)>,
    pub local_types: HashMap<u32, WasmType>,
    pub local_aver_types: HashMap<u32, Type>,
    pub ctx: &'a CodegenContext,
    pub variant_registry: &'a HashMap<(String, String), VariantInfo>,
    /// Current function's return type (set by emitter before body emission).
    pub fn_return_type: WasmType,
    /// ABI host import indices: import_name → function index.
    pub host_import_indices: HashMap<String, u32>,
    /// Current function name (for self-TCO check).
    pub current_fn_name: String,
}

impl<'a> ExprEmitter<'a> {
    pub fn new(
        fn_indices: &'a HashMap<String, u32>,
        rt: &'a RuntimeFuncIndices,
        string_literals: &'a HashMap<String, StringLiteral>,
        type_fields: &'a HashMap<(String, String), u32>,
        fn_sigs: &'a HashMap<String, (Vec<Type>, Type, Vec<String>)>,
        ctx: &'a CodegenContext,
        variant_registry: &'a HashMap<(String, String), VariantInfo>,
    ) -> Self {
        ExprEmitter {
            locals: HashMap::new(),
            next_local: 0,
            fn_indices,
            rt,
            instructions: Vec::new(),
            string_literals,
            type_fields,
            block_depth: 0,
            tco_loop_depth: None,
            fn_sigs,
            local_types: HashMap::new(),
            local_aver_types: HashMap::new(),
            ctx,
            variant_registry,
            fn_return_type: WasmType::I32,
            current_fn_name: String::new(),
            host_import_indices: HashMap::new(),
        }
    }

    fn ir_ctx(&self) -> WasmCallCtx<'_> {
        WasmCallCtx {
            ctx: self.ctx,
            locals: &self.locals,
        }
    }

    pub fn enable_tco_loop(&mut self) {
        self.tco_loop_depth = Some(self.block_depth);
    }

    pub fn add_params(&mut self, params: &[(String, String)], param_types: &[Type]) {
        for (i, (name, _type_ann)) in params.iter().enumerate() {
            let idx = self.next_local;
            self.locals.insert(name.clone(), idx);
            if let Some(ty) = param_types.get(i) {
                self.local_types.insert(idx, aver_type_to_wasm(ty));
                self.local_aver_types.insert(idx, ty.clone());
            } else {
                self.local_types.insert(idx, WasmType::I64);
            }
            self.next_local += 1;
        }
    }

    fn alloc_local(&mut self, wt: WasmType) -> u32 {
        let idx = self.next_local;
        self.next_local += 1;
        self.local_types.insert(idx, wt);
        idx
    }

    // -----------------------------------------------------------------------
    // Type inference
    // -----------------------------------------------------------------------

    pub fn infer_expr_type(&self, expr: &Expr) -> WasmType {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Int(_) => WasmType::I64,
                Literal::Float(_) => WasmType::F64,
                Literal::Bool(_) => WasmType::I32,
                Literal::Str(_) | Literal::Unit => WasmType::I32,
            },
            Expr::Ident(name) => {
                if let Some(&idx) = self.locals.get(name) {
                    self.local_types.get(&idx).copied().unwrap_or(WasmType::I64)
                } else {
                    WasmType::I64
                }
            }
            Expr::Resolved(slot) => self
                .local_types
                .get(&(*slot as u32))
                .copied()
                .unwrap_or(WasmType::I64),
            Expr::BinOp(op, lhs, rhs) => match op {
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    WasmType::I32
                }
                _ => {
                    let lt = self.infer_expr_type(&lhs.node);
                    let rt = self.infer_expr_type(&rhs.node);
                    if lt == WasmType::F64 || rt == WasmType::F64 {
                        WasmType::F64
                    } else {
                        lt
                    }
                }
            },
            Expr::FnCall(callee, _) => self.infer_call_return_type(callee),
            Expr::Constructor(name, _) => {
                let ctor = classify_constructor_name(name, &self.ir_ctx());
                match ctor {
                    SemanticConstructor::NoneValue => WasmType::I32,
                    SemanticConstructor::Wrapper(_)
                    | SemanticConstructor::TypeConstructor { .. } => WasmType::I32,
                    SemanticConstructor::Unknown(_) => WasmType::I32,
                }
            }
            Expr::Match { arms, .. } => self.infer_match_result_type(arms),
            Expr::ErrorProp(inner) => {
                let inner_type = self.infer_aver_type(&inner.node);
                match inner_type {
                    Some(Type::Result(ok_type, _)) => aver_type_to_wasm(&ok_type),
                    _ => WasmType::I64,
                }
            }
            Expr::List(_) | Expr::Tuple(_) | Expr::RecordCreate { .. } => WasmType::I32,
            Expr::InterpolatedStr(_) => WasmType::I32,
            Expr::Attr(base, field) => {
                if let Expr::Ident(base_name) = &base.node
                    && base_name.chars().next().is_some_and(|c| c.is_uppercase())
                {
                    return WasmType::I32;
                }
                self.infer_record_field_type(base, field)
            }
            Expr::TailCall(tc) => {
                if let Some((_, ret, _)) = self.fn_sigs.get(tc.0.as_str()) {
                    aver_type_to_wasm(ret)
                } else {
                    self.fn_return_type
                }
            }
            _ => WasmType::I64,
        }
    }

    fn infer_aver_type(&self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Int(_) => Some(Type::Int),
                Literal::Float(_) => Some(Type::Float),
                Literal::Bool(_) => Some(Type::Bool),
                Literal::Str(_) => Some(Type::Str),
                Literal::Unit => Some(Type::Unit),
            },
            Expr::Ident(name) => {
                if let Some(&idx) = self.locals.get(name) {
                    self.local_aver_types.get(&idx).cloned()
                } else {
                    None
                }
            }
            Expr::Resolved(slot) => self.local_aver_types.get(&(*slot as u32)).cloned(),
            Expr::BinOp(op, lhs, _) => match op {
                BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                    Some(Type::Bool)
                }
                _ => self.infer_aver_type(&lhs.node),
            },
            Expr::FnCall(callee, _) => self.infer_call_aver_return_type(callee),
            Expr::Constructor(name, inner) => {
                let ctor = classify_constructor_name(name, &self.ir_ctx());
                match ctor {
                    SemanticConstructor::NoneValue => Some(Type::Option(Box::new(Type::Unknown))),
                    SemanticConstructor::Wrapper(WrapperKind::ResultOk) => {
                        let inner_ty = inner
                            .as_ref()
                            .and_then(|e| self.infer_aver_type(&e.node))
                            .unwrap_or(Type::Unknown);
                        Some(Type::Result(Box::new(inner_ty), Box::new(Type::Unknown)))
                    }
                    SemanticConstructor::Wrapper(WrapperKind::ResultErr) => {
                        let inner_ty = inner
                            .as_ref()
                            .and_then(|e| self.infer_aver_type(&e.node))
                            .unwrap_or(Type::Unknown);
                        Some(Type::Result(Box::new(Type::Unknown), Box::new(inner_ty)))
                    }
                    SemanticConstructor::Wrapper(WrapperKind::OptionSome) => {
                        let inner_ty = inner
                            .as_ref()
                            .and_then(|e| self.infer_aver_type(&e.node))
                            .unwrap_or(Type::Unknown);
                        Some(Type::Option(Box::new(inner_ty)))
                    }
                    _ => None,
                }
            }
            Expr::Match { arms, .. } => arms
                .first()
                .and_then(|a| self.infer_aver_type(&a.body.node)),
            Expr::ErrorProp(inner) => match self.infer_aver_type(&inner.node) {
                Some(Type::Result(ok_type, _)) => Some(*ok_type),
                _ => None,
            },
            Expr::List(items) => {
                let elem_ty = items
                    .first()
                    .and_then(|e| self.infer_aver_type(&e.node))
                    .unwrap_or(Type::Unknown);
                Some(Type::List(Box::new(elem_ty)))
            }
            Expr::InterpolatedStr(_) => Some(Type::Str),
            _ => None,
        }
    }

    fn infer_match_result_type(&self, arms: &[MatchArm]) -> WasmType {
        // Try arms with patterns that don't introduce new bindings
        for arm in arms.iter() {
            match &arm.pattern {
                Pattern::Wildcard | Pattern::EmptyList | Pattern::Literal(_) => {
                    return self.infer_expr_type(&arm.body.node);
                }
                Pattern::Ident(_) => {
                    return self.infer_expr_type(&arm.body.node);
                }
                _ => {}
            }
        }
        self.fn_return_type
    }

    fn infer_call_return_type(&self, callee: &Spanned<Expr>) -> WasmType {
        let plan = classify_call_plan(&callee.node, &self.ir_ctx());
        match plan {
            CallPlan::Function(name) => {
                if let Some((_, ret_type, _)) = self.fn_sigs.get(name.as_str()) {
                    aver_type_to_wasm(ret_type)
                } else {
                    WasmType::I64
                }
            }
            CallPlan::Builtin(name) => {
                if let Some((_, ret_type, _)) = self.fn_sigs.get(name.as_str()) {
                    aver_type_to_wasm(ret_type)
                } else {
                    // Console.print etc → Unit
                    WasmType::I32
                }
            }
            CallPlan::Wrapper(_) | CallPlan::TypeConstructor { .. } | CallPlan::NoneValue => {
                WasmType::I32
            }
            CallPlan::Dynamic => WasmType::I64,
        }
    }

    fn infer_call_aver_return_type(&self, callee: &Spanned<Expr>) -> Option<Type> {
        let plan = classify_call_plan(&callee.node, &self.ir_ctx());
        match plan {
            CallPlan::Function(name) | CallPlan::Builtin(name) => self
                .fn_sigs
                .get(name.as_str())
                .map(|(_, ret, _)| ret.clone()),
            _ => None,
        }
    }

    // -----------------------------------------------------------------------
    // Body / block emission
    // -----------------------------------------------------------------------

    pub fn emit_body(&mut self, body: &FnBody) {
        match body {
            FnBody::Block(stmts) => self.emit_block(stmts),
        }
    }

    fn emit_block(&mut self, stmts: &[Stmt]) {
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

    fn emit_expr(&mut self, expr: &Expr) {
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
    // BinOp — native WASM arithmetic + promotion
    // -----------------------------------------------------------------------

    fn emit_binop(&mut self, op: &BinOp, lhs: &Spanned<Expr>, rhs: &Spanned<Expr>) {
        let lhs_type = self.infer_expr_type(&lhs.node);
        let rhs_type = self.infer_expr_type(&rhs.node);
        let operand_type = if lhs_type == WasmType::F64 || rhs_type == WasmType::F64 {
            WasmType::F64
        } else {
            lhs_type
        };

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
    // Function calls — via IR CallPlan
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
    fn emit_variant_constructor(
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

        // Store fields (all as i64 — convert if needed)
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

    /// Emit a builtin call (Console.print, List.len, Float.fromInt etc.)
    fn emit_builtin_call(&mut self, name: &str, args: &[Spanned<Expr>]) {
        // Args are already on the stack
        match name {
            "Console.print" | "Console.error" | "Console.warn" => {
                self.emit_console_print(args);
            }
            "List.prepend" if args.len() == 2 => {
                self.emit_list_prepend(args);
            }
            "List.len" if args.len() == 1 => {
                self.emit_list_len();
            }
            "List.take" if args.len() == 2 => {
                // args on stack: [list(i32), n(i64)]
                self.instructions.push(Instruction::I32WrapI64); // n: i64→i32
                self.instructions.push(Instruction::Call(self.rt.list_take));
            }
            "List.drop" if args.len() == 2 => {
                self.instructions.push(Instruction::I32WrapI64);
                self.instructions.push(Instruction::Call(self.rt.list_drop));
            }
            "List.concat" if args.len() == 2 => {
                self.instructions
                    .push(Instruction::Call(self.rt.list_concat));
            }
            "List.reverse" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.list_reverse));
            }
            "List.contains" if args.len() == 2 => {
                // args on stack: [list(i32), val(?)]
                // list_contains expects (i32, i64) — convert val to i64 if needed
                let val_type = self.infer_expr_type(&args[1].node);
                if val_type == WasmType::I32 {
                    self.instructions.push(Instruction::I64ExtendI32S);
                }
                self.instructions
                    .push(Instruction::Call(self.rt.list_contains));
            }
            "List.zip" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.list_zip));
            }
            "Float.fromInt" if args.len() == 1 => {
                self.instructions.push(Instruction::F64ConvertI64S);
            }
            "Int.toFloat" if args.len() == 1 => {
                self.instructions.push(Instruction::F64ConvertI64S);
            }
            "Int.toString" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            "Float.toString" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.f64_to_str_obj));
            }
            "Map.empty" if args.is_empty() => {
                // Empty map = empty association list = null ptr
                self.instructions.push(Instruction::I32Const(0));
            }
            "Map.get" if args.len() == 2 => {
                // args: [map(i32), key(i32)]
                self.instructions.push(Instruction::Call(self.rt.map_get));
            }
            "Map.set" if args.len() == 3 => {
                // args: [map(i32), key(i32), value(?)]
                // value needs to be i64 for map_set
                let val_type = self.infer_expr_type(&args[2].node);
                match val_type {
                    WasmType::I64 => {} // already i64
                    WasmType::I32 => self.instructions.push(Instruction::I64ExtendI32S),
                    WasmType::F64 => self.instructions.push(Instruction::I64ReinterpretF64),
                }
                self.instructions.push(Instruction::Call(self.rt.map_set));
            }
            "Map.has" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.map_has));
            }
            "Map.keys" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.map_keys));
            }
            "Map.entries" if args.len() == 1 => {
                // Map IS a list of tuples — identity
            }
            "Map.fromList" if args.len() == 1 => {
                // Identity — list of tuples IS a map
            }
            "Option.withDefault" if args.len() == 2 => {
                // args: [option(i32), default]
                // Check if option == NONE_SENTINEL → return default, else unwrap
                let opt_local = self.alloc_local(WasmType::I32);
                let result_type = self.infer_expr_type(&args[1].node);
                self.instructions.push(Instruction::LocalSet(opt_local)); // save default
                // Wait — args already on stack: [option, default]
                // Need to reorder. Actually let me save both.
                let def_local = self.alloc_local(result_type);
                // Stack has [option, default] — save default first (TOS)
                // Actually emit_builtin_call receives args already on stack in order.
                // Stack: [arg0=option, arg1=default]
                // But WASM stack is LIFO. After emitting args left to right,
                // TOS = default, below = option.
                // Save default (TOS)
                self.instructions.push(Instruction::LocalSet(def_local));
                // Now TOS = option
                self.instructions.push(Instruction::LocalSet(opt_local));
                // Check
                self.instructions.push(Instruction::LocalGet(opt_local));
                self.instructions
                    .push(Instruction::I32Const(super::value::NONE_SENTINEL));
                self.instructions.push(Instruction::I32Eq);
                self.emit_if(wasm_encoder::BlockType::Result(result_type.to_val_type()));
                self.instructions.push(Instruction::LocalGet(def_local));
                self.emit_else();
                // Unwrap
                self.instructions.push(Instruction::LocalGet(opt_local));
                match result_type {
                    WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.unwrap)),
                    WasmType::F64 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_f64)),
                    WasmType::I32 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_i32)),
                }
                self.emit_end();
            }
            "Vector.fromList" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.vec_from_list));
            }
            "Vector.get" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.vec_get));
            }
            "Vector.len" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.vec_len));
            }
            "Vector.set" if args.len() == 3 => {
                self.instructions.push(Instruction::Call(self.rt.vec_set));
            }
            "Vector.new" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.vec_new));
            }
            "Vector.toList" if args.len() == 1 => {
                // Convert vector back to list — TODO full implementation
                // For now drop and return empty list
                self.instructions.push(Instruction::Drop);
                self.instructions.push(Instruction::I32Const(0));
            }
            "String.len" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::I64Load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::I64Const(0xFFFFFFFF));
                self.instructions.push(Instruction::I64And);
            }
            "String.charAt" if args.len() == 2 => {
                // args: [str_ptr(i32), idx(i64)] → returns String (single char)
                // Load byte at str_ptr + 8 + idx, create 1-char string
                let idx_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32WrapI64); // idx → i32
                self.instructions.push(Instruction::LocalSet(idx_local));
                let str_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(str_local));
                // Alloc 1-char string object: 8 header + 8 padded
                let ptr = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32Const(16));
                self.instructions.push(Instruction::Call(self.rt.alloc));
                self.instructions.push(Instruction::LocalSet(ptr));
                // Header: OBJ_STRING, len=1
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::I64Const(
                    (value::OBJ_STRING << value::HDR_KIND_SHIFT | 1) as i64,
                ));
                self.instructions
                    .push(Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                // Copy byte
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::LocalGet(str_local));
                self.instructions.push(Instruction::I32Const(8));
                self.instructions.push(Instruction::I32Add);
                self.instructions.push(Instruction::LocalGet(idx_local));
                self.instructions.push(Instruction::I32Add);
                self.instructions
                    .push(Instruction::I32Load8U(wasm_encoder::MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions
                    .push(Instruction::I32Store8(wasm_encoder::MemArg {
                        offset: 8,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::LocalGet(ptr));
            }
            "String.trim" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.str_trim));
            }
            "String.slice" if args.len() == 3 => {
                // args on stack: [str_ptr(i32), start(i64), end(i64)]
                // Convert i64 args to i32 for runtime function
                let end_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32WrapI64); // end
                self.instructions.push(Instruction::LocalSet(end_local));
                self.instructions.push(Instruction::I32WrapI64); // start → now TOS
                // Stack: [str_ptr, start_i32], need to push end_local
                let start_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(start_local));
                // Stack: [str_ptr]
                self.instructions.push(Instruction::LocalGet(start_local));
                self.instructions.push(Instruction::LocalGet(end_local));
                self.instructions.push(Instruction::Call(self.rt.str_slice));
            }
            "String.chars" if args.len() == 1 => {
                self.instructions.push(Instruction::Call(self.rt.str_chars));
            }
            "String.join" if args.len() == 2 => {
                self.instructions.push(Instruction::Call(self.rt.str_join));
            }
            "String.startsWith" | "String.endsWith" | "String.contains" | "String.replace"
            | "String.split" | "String.toUpper" | "String.toLower" | "String.byteLength"
                if !args.is_empty() =>
            {
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions.push(Instruction::I32Const(0));
            }
            "String.fromInt" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            "String.fromFloat" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.f64_to_str_obj));
            }
            "String.fromBool" if args.len() == 1 => {
                // bool i32 → "true"/"false" string
                // Simplified: convert to int then to string
                self.instructions.push(Instruction::I64ExtendI32S);
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            "Int.mod" if args.len() == 2 => {
                // args: [a(i64), b(i64)] → Result<Int, String>
                // Simplified: just return a % b wrapped in Ok
                let b_local = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(b_local));
                self.instructions.push(Instruction::LocalGet(b_local));
                self.instructions.push(Instruction::I64RemS);
                // Wrap in Result.Ok
                let result = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(result));
                self.instructions
                    .push(Instruction::I32Const(value::WRAP_OK as i32));
                self.instructions.push(Instruction::LocalGet(result));
                self.instructions.push(Instruction::Call(self.rt.wrap));
            }
            "Int.abs" if args.len() == 1 => {
                // if val < 0 then -val else val
                let v = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(v));
                self.instructions.push(Instruction::LocalGet(v));
                self.instructions.push(Instruction::I64Const(0));
                self.instructions.push(Instruction::I64LtS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                self.instructions.push(Instruction::I64Const(0));
                self.instructions.push(Instruction::LocalGet(v));
                self.instructions.push(Instruction::I64Sub);
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(v));
                self.emit_end();
            }
            "Int.min" if args.len() == 2 => {
                let b = self.alloc_local(WasmType::I64);
                let a = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(b));
                self.instructions.push(Instruction::LocalSet(a));
                self.instructions.push(Instruction::LocalGet(a));
                self.instructions.push(Instruction::LocalGet(b));
                self.instructions.push(Instruction::I64LeS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                self.instructions.push(Instruction::LocalGet(a));
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(b));
                self.emit_end();
            }
            "Int.max" if args.len() == 2 => {
                let b = self.alloc_local(WasmType::I64);
                let a = self.alloc_local(WasmType::I64);
                self.instructions.push(Instruction::LocalSet(b));
                self.instructions.push(Instruction::LocalSet(a));
                self.instructions.push(Instruction::LocalGet(a));
                self.instructions.push(Instruction::LocalGet(b));
                self.instructions.push(Instruction::I64GeS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                self.instructions.push(Instruction::LocalGet(a));
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(b));
                self.emit_end();
            }
            "Int.fromString" if args.len() == 1 => {
                self.instructions
                    .push(Instruction::Call(self.rt.int_from_str));
            }
            "Int.fromFloat" if args.len() == 1 => {
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.abs" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Abs);
            }
            "Float.floor" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Floor);
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.ceil" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Ceil);
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.round" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Nearest);
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Float.sqrt" if args.len() == 1 => {
                self.instructions.push(Instruction::F64Sqrt);
            }
            "Float.sin" if args.len() == 1 => {
                // Host import — no native WASM instruction
                if let Some(&idx) = self.host_import_indices.get("math_sin") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    // Fallback: return 0.0
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::F64Const(0.0));
                }
            }
            "Float.cos" if args.len() == 1 => {
                if let Some(&idx) = self.host_import_indices.get("math_cos") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::F64Const(1.0));
                }
            }
            "Float.atan2" if args.len() == 2 => {
                if let Some(&idx) = self.host_import_indices.get("math_atan2") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::Drop);
                    self.instructions.push(Instruction::F64Const(0.0));
                }
            }
            "Float.pow" if args.len() == 2 => {
                if let Some(&idx) = self.host_import_indices.get("math_pow") {
                    self.instructions.push(Instruction::Call(idx));
                } else {
                    self.instructions.push(Instruction::Drop);
                }
            }
            "Float.min" if args.len() == 2 => {
                self.instructions.push(Instruction::F64Min);
            }
            "Float.max" if args.len() == 2 => {
                self.instructions.push(Instruction::F64Max);
            }
            "Float.pi" if args.is_empty() => {
                self.instructions
                    .push(Instruction::F64Const(std::f64::consts::PI));
            }
            "Float.toInt" if args.len() == 1 => {
                self.instructions.push(Instruction::I64TruncF64S);
            }
            "Bool.and" if args.len() == 2 => {
                self.instructions.push(Instruction::I32And);
            }
            "Bool.or" if args.len() == 2 => {
                self.instructions.push(Instruction::I32Or);
            }
            "Bool.not" if args.len() == 1 => {
                self.instructions.push(Instruction::I32Eqz);
            }
            "Char.fromCode" if args.len() == 1 => {
                // Int(i64) → single-char String
                // Alloc 1-char string, store byte
                let code = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32WrapI64);
                self.instructions.push(Instruction::LocalSet(code));
                let ptr = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::I32Const(16));
                self.instructions.push(Instruction::Call(self.rt.alloc));
                self.instructions.push(Instruction::LocalSet(ptr));
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::I64Const(
                    (value::OBJ_STRING << value::HDR_KIND_SHIFT | 1) as i64,
                ));
                self.instructions
                    .push(Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::LocalGet(ptr));
                self.instructions.push(Instruction::LocalGet(code));
                self.instructions
                    .push(Instruction::I32Store8(wasm_encoder::MemArg {
                        offset: 8,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::LocalGet(ptr));
            }
            "Char.toCode" if args.len() == 1 => {
                // String → Int (first byte code)
                self.instructions
                    .push(Instruction::I32Load8U(wasm_encoder::MemArg {
                        offset: 8,
                        align: 0,
                        memory_index: 0,
                    }));
                self.instructions.push(Instruction::I64ExtendI32U);
            }
            "Random.int" if args.len() == 2 => {
                // Stub: return min (first arg)
                self.instructions.push(Instruction::Drop); // drop max
                // min stays
            }
            "Console.readLine" if args.is_empty() => {
                // Stub: return empty string
                self.emit_string_literal("");
            }
            "Time.unixMs" if args.is_empty() => {
                self.instructions.push(Instruction::I64Const(0));
            }
            "Time.sleep" if args.len() == 1 => {
                self.instructions.push(Instruction::Drop);
                self.instructions.push(Instruction::I32Const(0));
            }
            "Result.withDefault" if args.len() == 2 => {
                // Same as Option.withDefault
                let result_type = self.infer_expr_type(&args[1].node);
                let def_local = self.alloc_local(result_type);
                let opt_local = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(def_local));
                self.instructions.push(Instruction::LocalSet(opt_local));
                // Check if err or none
                self.instructions.push(Instruction::LocalGet(opt_local));
                self.instructions.push(Instruction::I32Const(0));
                self.instructions.push(Instruction::I32GtS);
                self.emit_if(wasm_encoder::BlockType::Result(result_type.to_val_type()));
                // Check tag: Ok (0) = unwrap, Err (1) = default
                self.instructions.push(Instruction::LocalGet(opt_local));
                self.instructions.push(Instruction::Call(self.rt.obj_tag));
                self.instructions.push(Instruction::I32Eqz); // tag == 0 = Ok
                self.emit_if(wasm_encoder::BlockType::Result(result_type.to_val_type()));
                self.instructions.push(Instruction::LocalGet(opt_local));
                match result_type {
                    WasmType::I64 => self.instructions.push(Instruction::Call(self.rt.unwrap)),
                    WasmType::F64 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_f64)),
                    WasmType::I32 => self
                        .instructions
                        .push(Instruction::Call(self.rt.unwrap_i32)),
                }
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(def_local));
                self.emit_end();
                self.emit_else();
                self.instructions.push(Instruction::LocalGet(def_local));
                self.emit_end();
            }
            _ => {
                // Unknown builtin — drop args, return default for inferred type
                let ret_type = self.infer_call_return_type(&crate::ast::Spanned {
                    node: crate::ast::Expr::Ident(name.to_string()),
                    line: 0,
                });
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                match ret_type {
                    WasmType::I64 => self.instructions.push(Instruction::I64Const(0)),
                    WasmType::F64 => self.instructions.push(Instruction::F64Const(0.0)),
                    WasmType::I32 => self.instructions.push(Instruction::I32Const(0)),
                }
            }
        }
    }

    fn emit_console_print(&mut self, args: &[Spanned<Expr>]) {
        let arg_aver_type = self.infer_aver_type(&args[0].node);
        match &arg_aver_type {
            Some(Type::Int) => {
                self.instructions.push(Instruction::Call(self.rt.print_i64));
            }
            Some(Type::Float) => {
                self.instructions.push(Instruction::Call(self.rt.print_f64));
            }
            Some(Type::Bool) => {
                self.instructions
                    .push(Instruction::Call(self.rt.print_bool));
            }
            Some(Type::Str) => {
                self.instructions
                    .push(Instruction::Call(self.rt.print_string));
            }
            Some(Type::Unit) => {
                self.instructions.push(Instruction::Drop);
            }
            _ => {
                let wt = self.infer_expr_type(&args[0].node);
                match wt {
                    WasmType::I64 => {
                        self.instructions.push(Instruction::Call(self.rt.print_i64));
                    }
                    WasmType::F64 => {
                        self.instructions.push(Instruction::Call(self.rt.print_f64));
                    }
                    WasmType::I32 => {
                        self.instructions
                            .push(Instruction::Call(self.rt.print_heap));
                    }
                }
            }
        }
        // Newline
        self.instructions
            .push(Instruction::I32Const(super::runtime::NEWLINE_ADDR as i32));
        self.instructions.push(Instruction::I32Const(b'\n' as i32));
        self.instructions
            .push(Instruction::I32Store8(wasm_encoder::MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            }));
        self.instructions
            .push(Instruction::I32Const(super::runtime::NEWLINE_ADDR as i32));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.fd_write_buf));
        self.instructions.push(Instruction::I32Const(0)); // Unit
    }

    fn emit_list_prepend(&mut self, args: &[Spanned<Expr>]) {
        let elem_type = self.infer_expr_type(&args[0].node);
        match elem_type {
            WasmType::F64 => {
                self.instructions
                    .push(Instruction::Call(self.rt.list_cons_f64));
            }
            WasmType::I32 => {
                let tail_tmp = self.alloc_local(WasmType::I32);
                self.instructions.push(Instruction::LocalSet(tail_tmp));
                self.instructions.push(Instruction::I64ExtendI32S);
                self.instructions.push(Instruction::LocalGet(tail_tmp));
                self.instructions.push(Instruction::Call(self.rt.list_cons));
            }
            _ => {
                self.instructions.push(Instruction::Call(self.rt.list_cons));
            }
        }
    }

    fn emit_list_len(&mut self) {
        // List pointer on stack (i32). Count cons cells.
        let ptr = self.alloc_local(WasmType::I32);
        let count = self.alloc_local(WasmType::I64);
        self.instructions.push(Instruction::LocalSet(ptr));
        self.instructions.push(Instruction::I64Const(0));
        self.instructions.push(Instruction::LocalSet(count));
        // Loop
        self.instructions
            .push(Instruction::Block(wasm_encoder::BlockType::Empty));
        self.instructions
            .push(Instruction::Loop(wasm_encoder::BlockType::Empty));
        self.instructions.push(Instruction::LocalGet(ptr));
        self.instructions.push(Instruction::I32Eqz);
        self.instructions.push(Instruction::BrIf(1));
        // count++
        self.instructions.push(Instruction::LocalGet(count));
        self.instructions.push(Instruction::I64Const(1));
        self.instructions.push(Instruction::I64Add);
        self.instructions.push(Instruction::LocalSet(count));
        // ptr = tail (field[1] as i32)
        self.instructions.push(Instruction::LocalGet(ptr));
        self.instructions.push(Instruction::I32Const(1));
        self.instructions
            .push(Instruction::Call(self.rt.obj_field_i32));
        self.instructions.push(Instruction::LocalSet(ptr));
        self.instructions.push(Instruction::Br(0));
        self.instructions.push(Instruction::End); // loop
        self.instructions.push(Instruction::End); // block
        self.instructions.push(Instruction::LocalGet(count));
    }

    // -----------------------------------------------------------------------
    // Match — via IR MatchDispatchPlan
    // -----------------------------------------------------------------------

    fn emit_match(&mut self, subject: &Spanned<Expr>, arms: &[MatchArm]) {
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
        subject: &Spanned<Expr>,
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
                let cmp_type = if lhs_type == WasmType::F64 || rhs_type == WasmType::F64 {
                    WasmType::F64
                } else {
                    lhs_type
                };

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
        subject: &Spanned<Expr>,
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

        // Cons arm — bind head and tail
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
        subject: &Spanned<Expr>,
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

    fn emit_dispatch_check(&mut self, subj_local: u32, pattern: &ir::SemanticDispatchPattern) {
        match pattern {
            ir::SemanticDispatchPattern::Literal(lit) => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                match lit {
                    ir::DispatchLiteral::Int(n) => {
                        self.instructions.push(Instruction::I64Const(*n));
                        self.instructions.push(Instruction::I64Eq);
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
                // Short-circuit: check ptr > 0 first, only then call obj_tag
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I32Const(0));
                self.instructions.push(Instruction::I32GtS);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I32));
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_tag));
                self.instructions
                    .push(Instruction::I32Const(expected_tag as i32));
                self.instructions.push(Instruction::I32Eq);
                self.emit_else();
                self.instructions.push(Instruction::I32Const(0)); // false
                self.emit_end();
            }
        }
    }

    fn emit_wrapper_binding(&mut self, subj_local: u32, binding_name: &str) {
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
    fn emit_generic_match(&mut self, subject: &Spanned<Expr>, arms: &[MatchArm]) {
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

    fn emit_generic_arms(
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
                self.instructions.push(Instruction::LocalGet(subj_local));
                match lit {
                    Literal::Int(n) => {
                        self.instructions.push(Instruction::I64Const(*n));
                        self.instructions.push(Instruction::I64Eq);
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
            Pattern::Constructor(ctor_name, bindings) => {
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
                            self.emit_generic_arms(
                                subj_local,
                                subj_type,
                                result_local,
                                arms,
                                idx + 1,
                            );
                        }
                        self.emit_end();
                    }
                    SemanticConstructor::Wrapper(kind) => {
                        let expected_tag = match kind {
                            WrapperKind::ResultOk => value::WRAP_OK,
                            WrapperKind::ResultErr => value::WRAP_ERR,
                            WrapperKind::OptionSome => value::WRAP_SOME,
                        };
                        // Combined check: ptr > 0 && obj_tag == expected
                        self.instructions.push(Instruction::LocalGet(subj_local));
                        self.instructions.push(Instruction::I32Const(0));
                        self.instructions.push(Instruction::I32GtS);
                        self.instructions.push(Instruction::LocalGet(subj_local));
                        self.instructions.push(Instruction::Call(self.rt.obj_tag));
                        self.instructions
                            .push(Instruction::I32Const(expected_tag as i32));
                        self.instructions.push(Instruction::I32Eq);
                        self.instructions.push(Instruction::I32And);
                        self.emit_if(wasm_encoder::BlockType::Empty);
                        if let Some(binding_name) = bindings.first() {
                            self.emit_wrapper_binding(subj_local, binding_name);
                        }
                        self.emit_expr(&arm.body.node);
                        self.instructions.push(Instruction::LocalSet(result_local));
                        if !is_last {
                            self.emit_else();
                            self.emit_generic_arms(
                                subj_local,
                                subj_type,
                                result_local,
                                arms,
                                idx + 1,
                            );
                        }
                        self.emit_end();
                    }
                    SemanticConstructor::Unknown(_) => {}
                }
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

    /// Emit pattern match for user-defined variant: Shape.Circle(r) -> ...
    #[allow(clippy::too_many_arguments)]
    fn emit_variant_pattern(
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

        // Combined check: ptr > 0 && obj_tag == expected_tag
        self.instructions.push(Instruction::LocalGet(subj_local));
        self.instructions.push(Instruction::I32Const(0));
        self.instructions.push(Instruction::I32GtS);
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

    /// Infer the WASM type of a record field from type definitions.
    fn infer_record_field_type(&self, base_expr: &Spanned<Expr>, field_name: &str) -> WasmType {
        // Try to find the record type from the base expression's aver type
        let base_aver_type = self.infer_aver_type(&base_expr.node);
        let type_name = match &base_aver_type {
            Some(Type::Named(name)) => Some(name.as_str()),
            _ => None,
        };

        if let Some(type_name) = type_name {
            // Look up field type in type_defs
            for td in &self.ctx.type_defs {
                if let crate::ast::TypeDef::Product { name, fields, .. } = td
                    && name == type_name
                {
                    for (fname, ftype) in fields {
                        if fname == field_name {
                            return self.type_str_to_wasm(ftype);
                        }
                    }
                }
            }
            for module in &self.ctx.modules {
                for td in &module.type_defs {
                    if let crate::ast::TypeDef::Product { name, fields, .. } = td
                        && name == type_name
                    {
                        for (fname, ftype) in fields {
                            if fname == field_name {
                                return self.type_str_to_wasm(ftype);
                            }
                        }
                    }
                }
            }
        }

        // Fallback: try matching field name across all product types
        for td in &self.ctx.type_defs {
            if let crate::ast::TypeDef::Product { fields, .. } = td {
                for (fname, ftype) in fields {
                    if fname == field_name {
                        return self.type_str_to_wasm(ftype);
                    }
                }
            }
        }

        WasmType::I64 // default
    }

    fn type_str_to_wasm(&self, type_str: &str) -> WasmType {
        match type_str {
            "Float" => WasmType::F64,
            "Bool" => WasmType::I32,
            "String" | "Str" => WasmType::I32,
            "Int" => WasmType::I64,
            "Unit" => WasmType::I32,
            // User-defined types and unknown types are heap-allocated → I32
            _ => WasmType::I32,
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

    /// Convert a value to a string object pointer (i32).
    fn emit_value_to_str(&mut self, expr: &Expr) {
        let wt = self.infer_expr_type(expr);
        let at = self.infer_aver_type(expr);
        self.emit_expr(expr);
        match at {
            Some(Type::Str) => {} // already a string pointer
            Some(Type::Int) => {
                self.instructions
                    .push(Instruction::Call(self.rt.i64_to_str_obj));
            }
            Some(Type::Float) => {
                self.instructions
                    .push(Instruction::Call(self.rt.f64_to_str_obj));
            }
            _ => match wt {
                WasmType::I64 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.i64_to_str_obj));
                }
                WasmType::F64 => {
                    self.instructions
                        .push(Instruction::Call(self.rt.f64_to_str_obj));
                }
                WasmType::I32 => {} // assume string pointer
            },
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

    fn emit_literal(&mut self, lit: &Literal) {
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

    fn emit_string_literal(&mut self, s: &str) {
        if let Some(&(offset, _len)) = self.string_literals.get(s) {
            self.instructions.push(Instruction::I32Const(offset as i32));
        } else {
            self.instructions.push(Instruction::I32Const(0));
        }
    }

    fn emit_default_init(&mut self, local: u32, wt: WasmType) {
        match wt {
            WasmType::I32 => self.instructions.push(Instruction::I32Const(0)),
            WasmType::I64 => self.instructions.push(Instruction::I64Const(0)),
            WasmType::F64 => self.instructions.push(Instruction::F64Const(0.0)),
        }
        self.instructions.push(Instruction::LocalSet(local));
    }

    fn emit_if(&mut self, bt: wasm_encoder::BlockType) {
        self.instructions.push(Instruction::If(bt));
        self.block_depth += 1;
    }

    fn emit_else(&mut self) {
        self.instructions.push(Instruction::Else);
    }

    pub fn emit_end(&mut self) {
        self.instructions.push(Instruction::End);
        if self.block_depth > 0 {
            self.block_depth -= 1;
        }
    }
}
