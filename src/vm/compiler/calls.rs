use super::{CallTarget, CompileError, FnCompiler};
use crate::ast::{Expr, Literal, Spanned};
use crate::ir::{
    CallLowerCtx, CallPlan, ForwardArg, LeafOp, SemanticConstructor, TailCallPlan, WrapperKind,
    classify_call_plan, classify_constructor_name, classify_forward_call_parts, classify_leaf_op,
    classify_tail_call_plan,
};
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::*;
use crate::vm::symbol::VmSymbolTable;

struct VmCallCtx<'compiler, 'a> {
    compiler: &'compiler FnCompiler<'a>,
}

impl CallLowerCtx for VmCallCtx<'_, '_> {
    fn is_local_value(&self, name: &str) -> bool {
        self.compiler.local_slots.contains_key(name)
    }

    fn is_user_type(&self, name: &str) -> bool {
        self.compiler.resolve_type_id(name).is_some()
    }

    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
        let mut best = None;

        for (dot_idx, _) in dotted.match_indices('.') {
            let prefix = &dotted[..dot_idx];
            let suffix = &dotted[dot_idx + 1..];
            if suffix.is_empty()
                || self
                    .compiler
                    .symbols
                    .resolve_namespace_path(prefix)
                    .is_none()
            {
                continue;
            }

            let is_module_function = self.compiler.resolve_fn_id(dotted).is_some();
            let is_module_ctor = suffix.rsplit_once('.').is_some_and(|(type_name, _)| {
                self.compiler.resolve_type_id(type_name).is_some()
                    || self
                        .compiler
                        .resolve_type_id(&format!("{prefix}.{type_name}"))
                        .is_some()
            });

            if is_module_function || is_module_ctor {
                best = Some((prefix, suffix));
            }
        }

        best
    }
}

impl<'a> FnCompiler<'a> {
    pub(super) fn try_compile_leaf_expr(&mut self, expr: &Expr) -> Result<bool, CompileError> {
        let leaf = {
            let call_ctx = VmCallCtx { compiler: self };
            classify_leaf_op(expr, &call_ctx)
        };

        match leaf {
            Some(leaf) => {
                self.compile_leaf_op(leaf)?;
                Ok(true)
            }
            None => Ok(false),
        }
    }

    pub(super) fn classify_constructor_semantics(&self, name: &str) -> SemanticConstructor {
        let call_ctx = VmCallCtx { compiler: self };
        classify_constructor_name(name, &call_ctx)
    }

    fn resolve_builtin_target(&self, name: &str) -> Option<CallTarget> {
        let symbol_id = self.symbols.find(name)?;
        self.symbols
            .resolve_builtin(symbol_id)
            .map(CallTarget::Builtin)
    }

    fn flatten_path(&self, expr: &Spanned<Expr>) -> Option<String> {
        match &expr.node {
            Expr::Ident(name) => Some(name.clone()),
            Expr::Attr(inner, field) => Some(format!("{}.{}", self.flatten_path(inner)?, field)),
            _ => Option::None,
        }
    }

    pub(super) fn resolve_type_id(&self, name: &str) -> Option<u32> {
        self.arena.find_type_id(name).or_else(|| {
            name.rsplit('.')
                .next()
                .filter(|short| *short != name)
                .and_then(|short| self.arena.find_type_id(short))
        })
    }

    fn resolve_fn_id(&self, name: &str) -> Option<u32> {
        self.module_scope
            .get(name)
            .copied()
            .or_else(|| self.code_store.find(name))
    }

    pub(super) fn resolve_call_target(&self, expr: &Expr) -> Option<CallTarget> {
        let call_ctx = VmCallCtx { compiler: self };
        self.call_plan_to_target(classify_call_plan(expr, &call_ctx))
    }

    fn call_plan_to_target(&self, plan: CallPlan) -> Option<CallTarget> {
        match plan {
            CallPlan::Dynamic => None,
            CallPlan::Function(name) => {
                self.resolve_fn_id(&name)
                    .map(CallTarget::KnownFn)
                    .or_else(|| {
                        if name.contains('.') {
                            Some(CallTarget::UnknownQualified(name))
                        } else {
                            None
                        }
                    })
            }
            CallPlan::Builtin(name) => self
                .resolve_builtin_target(&name)
                .or(Some(CallTarget::UnknownQualified(name))),
            CallPlan::Wrapper(kind) => {
                let wrap_kind = match kind {
                    WrapperKind::ResultOk => 0,
                    WrapperKind::ResultErr => 1,
                    WrapperKind::OptionSome => 2,
                };
                Some(CallTarget::Wrapper(wrap_kind))
            }
            CallPlan::NoneValue => Some(CallTarget::None_),
            CallPlan::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => {
                let type_id = self.resolve_type_id(&qualified_type_name)?;
                if let Some(variant_id) = self.arena.find_variant_id(type_id, &variant_name) {
                    Some(CallTarget::Variant(type_id, variant_id))
                } else {
                    Some(CallTarget::UnknownQualified(format!(
                        "{}.{}",
                        qualified_type_name, variant_name
                    )))
                }
            }
        }
    }

    fn compile_forward_arg(&mut self, arg: ForwardArg) -> Result<(), CompileError> {
        match arg {
            ForwardArg::Slot(slot) => {
                self.emit_op(LOAD_LOCAL);
                self.emit_u8(slot as u8);
                Ok(())
            }
            ForwardArg::Local(name) => {
                let Some(&slot) = self.local_slots.get(&name) else {
                    return Err(CompileError {
                        msg: format!("forwarded local '{}' is not a known slot", name),
                    });
                };
                self.emit_op(LOAD_LOCAL);
                self.emit_u8(slot as u8);
                Ok(())
            }
        }
    }

    pub(super) fn compile_call(
        &mut self,
        fn_expr: &Spanned<Expr>,
        args: &[Spanned<Expr>],
    ) -> Result<(), CompileError> {
        let call_ctx = VmCallCtx { compiler: self };
        // Skip forward-call optimization when in owned context — the
        // forward path doesn't propagate owned_mask to CALL_BUILTIN_OWNED.
        // Fall through to compile_resolved_call which does.
        if self.owned_params.is_empty()
            && let Some(plan) = classify_forward_call_parts(&fn_expr.node, args, &call_ctx)
        {
            let Some(target) = self.call_plan_to_target(plan.target.clone()) else {
                return Err(CompileError {
                    msg: "dynamic call cannot lower through ForwardCallPlan".to_string(),
                });
            };
            for arg in plan.args {
                self.compile_forward_arg(arg)?;
            }
            return self.emit_resolved_call_after_loaded_args(target, args.len(), 0);
        }

        if let Some(target) = self.resolve_call_target(&fn_expr.node) {
            return self.compile_resolved_call(target, args);
        }
        self.compile_expr(fn_expr)?;
        for arg in args {
            self.compile_expr(arg)?;
        }
        self.emit_op(CALL_VALUE);
        self.emit_u8(args.len() as u8);
        Ok(())
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
            CallTarget::Builtin(builtin) => self.emit_builtin_after_args(builtin, argc, owned_mask),
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
        args: &[Spanned<Expr>],
    ) -> Result<(), CompileError> {
        // Compute owned mask before compiling args (we need the AST).
        let owned_mask = if !self.owned_params.is_empty() {
            let arg_refs: Vec<&Spanned<Expr>> = args.iter().collect();
            self.compute_builtin_owned_mask(&arg_refs)
        } else {
            0
        };
        for arg in args {
            self.compile_expr(arg)?;
        }
        self.emit_resolved_call_after_loaded_args(target, args.len(), owned_mask)
    }

    pub(super) fn compile_tail_call(
        &mut self,
        target: &str,
        args: &[Spanned<Expr>],
        owned: &[bool],
    ) -> Result<(), CompileError> {
        // Set owned_params context so compile_call can emit CALL_BUILTIN_OWNED
        // for builtins whose arguments reference sole-owned parameters.
        self.owned_params.clear();
        for (i, &is_owned) in owned.iter().enumerate() {
            if is_owned {
                self.owned_params.insert(i as u16);
            }
        }

        for arg in args {
            self.compile_expr(arg)?;
        }

        // Clear owned context after compiling args
        self.owned_params.clear();

        // Pack owned flags into a bitmask (up to 8 args).
        // Bit i is set if arg i is sole-owned and can be reused in-place.
        let owned_mask: u8 = owned
            .iter()
            .enumerate()
            .take(8)
            .fold(
                0u8,
                |mask, (i, &is_owned)| {
                    if is_owned { mask | (1 << i) } else { mask }
                },
            );

        let call_ctx = VmCallCtx { compiler: self };
        match classify_tail_call_plan(target, &self.name, &call_ctx) {
            TailCallPlan::SelfCall => {
                self.emit_op(TAIL_CALL_SELF);
                self.emit_u8(args.len() as u8);
                self.emit_u8(owned_mask);
            }
            TailCallPlan::KnownFunction(name) => {
                if let Some(fn_id) = self.resolve_fn_id(&name) {
                    self.emit_op(TAIL_CALL_KNOWN);
                    self.emit_u16(fn_id as u16);
                    self.emit_u8(args.len() as u8);
                    self.emit_u8(owned_mask);
                } else {
                    return Err(CompileError {
                        msg: format!("unknown tail call target: {}", name),
                    });
                }
            }
            TailCallPlan::Unknown(name) => {
                return Err(CompileError {
                    msg: format!("unknown tail call target: {}", name),
                });
            }
        }
        Ok(())
    }

    pub(super) fn compile_constructor(
        &mut self,
        name: &str,
        arg: Option<&Spanned<Expr>>,
    ) -> Result<(), CompileError> {
        let normalized_name = match name {
            "Ok" => "Result.Ok",
            "Err" => "Result.Err",
            "Some" => "Option.Some",
            "None" => "Option.None",
            other => other,
        };

        match self.classify_constructor_semantics(normalized_name) {
            SemanticConstructor::Wrapper(kind) => {
                self.compile_constructor_arg(arg)?;
                self.emit_op(WRAP);
                self.emit_u8(match kind {
                    WrapperKind::ResultOk => 0,
                    WrapperKind::ResultErr => 1,
                    WrapperKind::OptionSome => 2,
                });
            }
            SemanticConstructor::NoneValue => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            SemanticConstructor::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => {
                if let Some(type_id) = self.resolve_type_id(&qualified_type_name)
                    && let Some(variant_id) = self.arena.find_variant_id(type_id, &variant_name)
                {
                    let field_count = if let Some(a) = arg {
                        self.compile_expr(a)?;
                        1u8
                    } else {
                        0u8
                    };
                    self.emit_op(VARIANT_NEW);
                    self.emit_u16(type_id as u16);
                    self.emit_u16(variant_id);
                    self.emit_u8(field_count);
                    return Ok(());
                }
                return Err(CompileError {
                    msg: format!("unknown constructor: {}", name),
                });
            }
            SemanticConstructor::Unknown(_) => {
                return Err(CompileError {
                    msg: format!("unknown constructor: {}", name),
                });
            }
        }
        Ok(())
    }

    fn compile_constructor_arg(&mut self, arg: Option<&Spanned<Expr>>) -> Result<(), CompileError> {
        if let Some(a) = arg {
            self.compile_expr(a)
        } else {
            self.emit_op(LOAD_UNIT);
            Ok(())
        }
    }

    fn compile_leaf_op(&mut self, leaf: LeafOp<'_>) -> Result<(), CompileError> {
        match leaf {
            LeafOp::FieldAccess { object, field_name } => self.compile_attr(object, field_name),
            LeafOp::MapGet { map, key } => {
                self.compile_expr(map)?;
                self.compile_expr(key)?;
                self.emit_builtin_after_args(VmBuiltin::MapGet, 2, 0);
                Ok(())
            }
            LeafOp::MapSet { map, key, value } => {
                let owned_mask = self.compute_builtin_owned_mask(&[map, key, value]);
                self.compile_expr(map)?;
                self.compile_expr(key)?;
                self.compile_expr(value)?;
                self.emit_builtin_after_args(VmBuiltin::MapSet, 3, owned_mask);
                Ok(())
            }
            LeafOp::VectorNew { size, fill } => {
                self.compile_expr(size)?;
                self.compile_expr(fill)?;
                self.emit_builtin_after_args(VmBuiltin::VectorNew, 2, 0);
                Ok(())
            }
            LeafOp::VectorGetOrDefaultLiteral {
                vector,
                index,
                default_literal,
            } => {
                self.compile_expr(vector)?;
                self.compile_expr(index)?;
                let default_value = self.nan_literal(default_literal);
                let const_idx = self.add_constant(default_value);
                self.emit_op(VECTOR_GET_OR);
                self.emit_u16(const_idx);
                Ok(())
            }
            LeafOp::VectorSetOrDefaultSameVector {
                vector,
                index,
                value,
            } => {
                // Note: owned path cannot use CALL_BUILTIN_OWNED here because
                // vec_set_nv_owned returns Option<Vector> while the fused
                // VECTOR_SET_OR_KEEP unwraps to raw Vector. Direct Vector.set
                // calls still get the owned path via emit_builtin_after_args.
                self.compile_expr(vector)?;
                self.compile_expr(index)?;
                self.compile_expr(value)?;
                self.emit_op(VECTOR_SET_OR_KEEP);
                Ok(())
            }
            LeafOp::ListIndexGet { list, index } => {
                // Decompose: Vector.fromList(list) then Vector.get(_, index)
                self.compile_expr(list)?;
                self.emit_builtin_after_args(VmBuiltin::VectorFromList, 1, 0);
                self.compile_expr(index)?;
                self.emit_op(VECTOR_GET);
                Ok(())
            }
            LeafOp::IntModOrDefaultLiteral {
                a,
                b,
                default_literal,
            } => {
                self.compile_expr(a)?;
                self.compile_expr(b)?;
                self.emit_builtin_after_args(VmBuiltin::IntMod, 2, 0);
                let default_value = self.nan_literal(default_literal);
                let const_idx = self.add_constant(default_value);
                self.emit_op(LOAD_CONST);
                self.emit_u16(const_idx);
                self.emit_op(UNWRAP_RESULT_OR);
                Ok(())
            }
        }
    }

    fn emit_builtin_after_args(&mut self, builtin: VmBuiltin, argc: usize, owned_mask: u8) {
        match builtin {
            VmBuiltin::ListLen => self.emit_op(LIST_LEN),
            VmBuiltin::ListPrepend => self.emit_op(LIST_PREPEND),
            VmBuiltin::VectorGet => self.emit_op(VECTOR_GET),
            VmBuiltin::VectorSet if owned_mask != 0 => {
                // Owned path: go through CALL_BUILTIN_OWNED for take optimization
                let symbol_id = self.symbols.intern_builtin(builtin);
                self.emit_op(CALL_BUILTIN_OWNED);
                self.emit_u32(symbol_id);
                self.emit_u8(argc as u8);
                self.emit_u8(owned_mask);
            }
            VmBuiltin::VectorSet => self.emit_op(VECTOR_SET),
            VmBuiltin::OptionWithDefault => self.emit_op(UNWRAP_OR),
            VmBuiltin::ResultWithDefault => self.emit_op(UNWRAP_RESULT_OR),
            _ => {
                let symbol_id = self.symbols.intern_builtin(builtin);
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
    }

    /// Compute owned bitmask for builtin args by checking if any arg
    /// is a simple local reference to an owned parameter.
    fn compute_builtin_owned_mask(&self, arg_exprs: &[&Spanned<Expr>]) -> u8 {
        if self.owned_params.is_empty() {
            return 0;
        }
        let mut mask = 0u8;
        for (i, arg) in arg_exprs.iter().enumerate().take(8) {
            match &arg.node {
                Expr::Ident(name) => {
                    if let Some(&slot) = self.local_slots.get(name)
                        && self.owned_params.contains(&slot)
                    {
                        mask |= 1 << i;
                    }
                }
                Expr::Resolved(slot) => {
                    if self.owned_params.contains(slot) {
                        mask |= 1 << i;
                    }
                }
                _ => {}
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
        obj: &Spanned<Expr>,
        field: &str,
    ) -> Result<(), CompileError> {
        if let Some(path) = self.flatten_path(obj)
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

    fn infer_record_field_idx(&self, obj: &Expr, field: &str) -> Option<u8> {
        let type_name = match obj {
            Expr::RecordCreate { type_name, .. } | Expr::RecordUpdate { type_name, .. } => {
                type_name.as_str()
            }
            _ => return None,
        };
        let type_id = self.resolve_type_id(type_name)?;
        let fields = self.arena.get_field_names(type_id);
        fields
            .iter()
            .position(|name| name == field)
            .map(|idx| idx as u8)
    }

    fn resolve_record_field_idx(&self, obj: &Expr, field: &str) -> Option<u8> {
        let field_symbol_id = self.code_store.symbols.find(field)?;
        match obj {
            Expr::Ident(type_name)
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
