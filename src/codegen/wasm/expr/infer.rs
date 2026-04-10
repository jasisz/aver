/// Type inference methods for ExprEmitter.
use std::collections::HashMap;

use crate::ast::{BinOp, Expr, Literal, MatchArm, Pattern, Spanned};
use crate::codegen::CodegenContext;
use crate::ir::{
    CallPlan, SemanticConstructor, WrapperKind, classify_call_plan, classify_constructor_name,
};
use crate::types::{Type, parse_type_str};

use super::super::types::{WasmType, aver_type_to_wasm};
use super::ExprEmitter;

// ---------------------------------------------------------------------------
// Variant registry: type_name -> [(variant_name, tag, field_type_names)]
// ---------------------------------------------------------------------------

/// Info about a single variant in a sum type.
#[derive(Debug, Clone)]
pub(in crate::codegen::wasm) struct VariantInfo {
    pub tag: u32,
    pub field_types: Vec<String>, // type annotation strings from AST
    /// For nullary variants: a unique negative sentinel (i32) to avoid heap allocation.
    /// None for variants with fields.
    pub nullary_sentinel: Option<i32>,
}

/// Build variant registry from type_defs.
pub(in crate::codegen::wasm) fn build_variant_registry(
    ctx: &CodegenContext,
) -> HashMap<(String, String), VariantInfo> {
    let mut registry = HashMap::new();
    // Sentinel counter: -1 is Option.None, user nullary variants start at -2.
    let mut next_sentinel: i32 = -2;
    let mut process_td = |td: &crate::ast::TypeDef, prefix: Option<&str>, next_sentinel: &mut i32| {
        if let crate::ast::TypeDef::Sum { name, variants, .. } = td {
            for (tag, variant) in variants.iter().enumerate() {
                let qualified_type = match prefix {
                    Some(p) => format!("{}.{}", p, name),
                    None => name.clone(),
                };
                let sentinel = if variant.fields.is_empty() {
                    let s = *next_sentinel;
                    *next_sentinel -= 1;
                    Some(s)
                } else {
                    None
                };
                registry.insert(
                    (qualified_type.clone(), variant.name.clone()),
                    VariantInfo {
                        tag: tag as u32,
                        field_types: variant.fields.clone(),
                        nullary_sentinel: sentinel,
                    },
                );
                // Also register bare name
                if prefix.is_some() {
                    registry.insert(
                        (name.clone(), variant.name.clone()),
                        VariantInfo {
                            tag: tag as u32,
                            field_types: variant.fields.clone(),
                            nullary_sentinel: sentinel,
                        },
                    );
                }
            }
        }
    };

    for td in &ctx.type_defs {
        process_td(td, None, &mut next_sentinel);
    }
    for module in &ctx.modules {
        for td in &module.type_defs {
            process_td(td, Some(&module.prefix), &mut next_sentinel);
        }
    }
    registry
}

// ---------------------------------------------------------------------------
// Type inference
// ---------------------------------------------------------------------------

impl<'a> ExprEmitter<'a> {
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
            Expr::FnCall(callee, args) => self.infer_call_return_type(callee, args),
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
            Expr::List(_)
            | Expr::Tuple(_)
            | Expr::RecordCreate { .. }
            | Expr::RecordUpdate { .. } => WasmType::I32,
            Expr::IndependentProduct(_, _) => WasmType::I32,
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
                let resolved_name = self.resolve_user_fn_name(tc.0.as_str());
                if let Some((_, ret, _)) = self.fn_sigs.get(resolved_name.as_str()) {
                    aver_type_to_wasm(ret)
                } else {
                    self.fn_return_type
                }
            }
            _ => WasmType::I64,
        }
    }

    pub(super) fn infer_aver_type(&self, expr: &Expr) -> Option<Type> {
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
            Expr::FnCall(callee, args) => self.infer_call_aver_return_type(callee, args),
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
                    SemanticConstructor::TypeConstructor {
                        qualified_type_name,
                        ..
                    } => Some(Type::Named(qualified_type_name)),
                    SemanticConstructor::Unknown(_) => None,
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
            Expr::Tuple(items) => Some(Type::Tuple(
                items
                    .iter()
                    .map(|item| self.infer_aver_type(&item.node).unwrap_or(Type::Unknown))
                    .collect(),
            )),
            Expr::IndependentProduct(items, unwrap) => Some(Type::Tuple(
                items
                    .iter()
                    .map(|item| {
                        let item_ty = self.infer_aver_type(&item.node).unwrap_or(Type::Unknown);
                        if *unwrap {
                            match item_ty {
                                Type::Result(ok, _) => *ok,
                                other => other,
                            }
                        } else {
                            item_ty
                        }
                    })
                    .collect(),
            )),
            Expr::RecordCreate { type_name, .. } | Expr::RecordUpdate { type_name, .. } => {
                Some(Type::Named(type_name.clone()))
            }
            Expr::Attr(base, field) => {
                if let Expr::Ident(base_name) = &base.node
                    && base_name.chars().next().is_some_and(|c| c.is_uppercase())
                {
                    let qualified = format!("{}.{}", base_name, field);
                    return match classify_constructor_name(&qualified, &self.ir_ctx()) {
                        SemanticConstructor::NoneValue => {
                            Some(Type::Option(Box::new(Type::Unknown)))
                        }
                        SemanticConstructor::TypeConstructor {
                            qualified_type_name,
                            ..
                        } => Some(Type::Named(qualified_type_name)),
                        SemanticConstructor::Wrapper(_) | SemanticConstructor::Unknown(_) => None,
                    };
                }
                self.infer_record_field_aver_type(base, field)
            }
            Expr::InterpolatedStr(_) => Some(Type::Str),
            _ => None,
        }
    }

    pub(super) fn infer_match_result_type(&self, arms: &[MatchArm]) -> WasmType {
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

    pub(super) fn infer_call_return_type(
        &self,
        callee: &Spanned<Expr>,
        args: &[Spanned<Expr>],
    ) -> WasmType {
        let plan = classify_call_plan(&callee.node, &self.ir_ctx());
        match plan {
            CallPlan::Function(name) => {
                let resolved_name = self.resolve_user_fn_name(name.as_str());
                if let Some((_, ret_type, _)) = self.fn_sigs.get(resolved_name.as_str()) {
                    aver_type_to_wasm(ret_type)
                } else {
                    WasmType::I64
                }
            }
            CallPlan::Builtin(name) => {
                if let Some(ret_type) =
                    self.infer_builtin_call_aver_return_type(name.as_str(), args)
                {
                    return aver_type_to_wasm(&ret_type);
                }
                if let Some((_, ret_type, _)) = self.fn_sigs.get(name.as_str()) {
                    aver_type_to_wasm(ret_type)
                } else {
                    // Console.print etc -> Unit
                    WasmType::I32
                }
            }
            CallPlan::Wrapper(_) | CallPlan::TypeConstructor { .. } | CallPlan::NoneValue => {
                WasmType::I32
            }
            CallPlan::Dynamic => WasmType::I64,
        }
    }

    pub(super) fn infer_call_aver_return_type(
        &self,
        callee: &Spanned<Expr>,
        args: &[Spanned<Expr>],
    ) -> Option<Type> {
        let plan = classify_call_plan(&callee.node, &self.ir_ctx());
        match plan {
            CallPlan::Function(name) => {
                let resolved_name = self.resolve_user_fn_name(name.as_str());
                self.fn_sigs
                    .get(resolved_name.as_str())
                    .map(|(_, ret, _)| ret.clone())
            }
            CallPlan::Builtin(name) => self
                .infer_builtin_call_aver_return_type(name.as_str(), args)
                .or_else(|| {
                    self.fn_sigs
                        .get(name.as_str())
                        .map(|(_, ret, _)| ret.clone())
                }),
            CallPlan::TypeConstructor {
                qualified_type_name,
                ..
            } => Some(Type::Named(qualified_type_name)),
            CallPlan::Wrapper(WrapperKind::ResultOk)
            | CallPlan::Wrapper(WrapperKind::ResultErr) => {
                let inner_ty = args
                    .first()
                    .and_then(|a| self.infer_aver_type(&a.node))
                    .unwrap_or(Type::Unknown);
                Some(Type::Result(Box::new(inner_ty), Box::new(Type::Unknown)))
            }
            CallPlan::Wrapper(WrapperKind::OptionSome) => {
                let inner_ty = args
                    .first()
                    .and_then(|a| self.infer_aver_type(&a.node))
                    .unwrap_or(Type::Unknown);
                Some(Type::Option(Box::new(inner_ty)))
            }
            CallPlan::NoneValue => Some(Type::Option(Box::new(Type::Unknown))),
            _ => None,
        }
    }

    fn prefer_known_type(&self, current: Type, fallback: Type) -> Type {
        if matches!(current, Type::Unknown) {
            fallback
        } else {
            current
        }
    }

    fn infer_list_elem_aver_type(&self, expr: &Spanned<Expr>) -> Option<Type> {
        match self.infer_aver_type(&expr.node) {
            Some(Type::List(elem)) => Some(*elem),
            _ => None,
        }
    }

    fn infer_map_parts(&self, expr: &Spanned<Expr>) -> Option<(Type, Type)> {
        match self.infer_aver_type(&expr.node) {
            Some(Type::Map(key, value)) => Some((*key, *value)),
            _ => None,
        }
    }

    fn infer_vector_elem_aver_type(&self, expr: &Spanned<Expr>) -> Option<Type> {
        match self.infer_aver_type(&expr.node) {
            Some(Type::Vector(elem)) => Some(*elem),
            _ => None,
        }
    }

    fn infer_option_inner_aver_type(&self, expr: &Spanned<Expr>) -> Option<Type> {
        match self.infer_aver_type(&expr.node) {
            Some(Type::Option(inner)) => Some(*inner),
            _ => None,
        }
    }

    fn infer_result_ok_aver_type(&self, expr: &Spanned<Expr>) -> Option<Type> {
        match self.infer_aver_type(&expr.node) {
            Some(Type::Result(ok, _)) => Some(*ok),
            _ => None,
        }
    }

    fn infer_builtin_call_aver_return_type(
        &self,
        name: &str,
        args: &[Spanned<Expr>],
    ) -> Option<Type> {
        match name {
            "Map.get" if args.len() == 2 => {
                let value_ty = self
                    .infer_map_parts(&args[0])
                    .map(|(_, value)| value)
                    .unwrap_or(Type::Unknown);
                Some(Type::Option(Box::new(value_ty)))
            }
            "Map.set" if args.len() == 3 => {
                let key_ty = self.infer_aver_type(&args[1].node).unwrap_or(Type::Unknown);
                let value_ty = self.infer_aver_type(&args[2].node).unwrap_or(Type::Unknown);
                let (map_key, map_value) = self
                    .infer_map_parts(&args[0])
                    .unwrap_or((Type::Unknown, Type::Unknown));
                Some(Type::Map(
                    Box::new(self.prefer_known_type(map_key, key_ty)),
                    Box::new(self.prefer_known_type(map_value, value_ty)),
                ))
            }
            "Map.remove" if args.len() == 2 => {
                let key_ty = self.infer_aver_type(&args[1].node).unwrap_or(Type::Unknown);
                let (map_key, map_value) = self
                    .infer_map_parts(&args[0])
                    .unwrap_or((Type::Unknown, Type::Unknown));
                Some(Type::Map(
                    Box::new(self.prefer_known_type(map_key, key_ty)),
                    Box::new(map_value),
                ))
            }
            "Map.keys" if args.len() == 1 => {
                let key_ty = self
                    .infer_map_parts(&args[0])
                    .map(|(key, _)| key)
                    .unwrap_or(Type::Unknown);
                Some(Type::List(Box::new(key_ty)))
            }
            "Map.values" if args.len() == 1 => {
                let value_ty = self
                    .infer_map_parts(&args[0])
                    .map(|(_, value)| value)
                    .unwrap_or(Type::Unknown);
                Some(Type::List(Box::new(value_ty)))
            }
            "Map.entries" if args.len() == 1 => {
                let (key_ty, value_ty) = self
                    .infer_map_parts(&args[0])
                    .unwrap_or((Type::Unknown, Type::Unknown));
                Some(Type::List(Box::new(Type::Tuple(vec![key_ty, value_ty]))))
            }
            "Map.fromList" if args.len() == 1 => {
                let (key_ty, value_ty) = match self.infer_list_elem_aver_type(&args[0]) {
                    Some(Type::Tuple(items)) if items.len() == 2 => {
                        (items[0].clone(), items[1].clone())
                    }
                    _ => (Type::Unknown, Type::Unknown),
                };
                Some(Type::Map(Box::new(key_ty), Box::new(value_ty)))
            }
            "Vector.fromList" if args.len() == 1 => {
                let elem_ty = self
                    .infer_list_elem_aver_type(&args[0])
                    .unwrap_or(Type::Unknown);
                Some(Type::Vector(Box::new(elem_ty)))
            }
            "Vector.get" if args.len() == 2 => {
                let elem_ty = self
                    .infer_vector_elem_aver_type(&args[0])
                    .unwrap_or(Type::Unknown);
                Some(Type::Option(Box::new(elem_ty)))
            }
            "Vector.set" if args.len() == 3 => {
                let elem_ty = self
                    .infer_vector_elem_aver_type(&args[0])
                    .unwrap_or_else(|| {
                        self.infer_aver_type(&args[2].node).unwrap_or(Type::Unknown)
                    });
                Some(Type::Option(Box::new(Type::Vector(Box::new(elem_ty)))))
            }
            "Vector.new" if args.len() == 2 => {
                let elem_ty = self.infer_aver_type(&args[1].node).unwrap_or(Type::Unknown);
                Some(Type::Vector(Box::new(elem_ty)))
            }
            "Vector.toList" if args.len() == 1 => {
                let elem_ty = self
                    .infer_vector_elem_aver_type(&args[0])
                    .unwrap_or(Type::Unknown);
                Some(Type::List(Box::new(elem_ty)))
            }
            "Option.withDefault" if args.len() == 2 => Some(
                self.prefer_known_type(
                    self.infer_option_inner_aver_type(&args[0])
                        .unwrap_or(Type::Unknown),
                    self.infer_aver_type(&args[1].node).unwrap_or(Type::Unknown),
                ),
            ),
            "Result.withDefault" if args.len() == 2 => Some(
                self.prefer_known_type(
                    self.infer_result_ok_aver_type(&args[0])
                        .unwrap_or(Type::Unknown),
                    self.infer_aver_type(&args[1].node).unwrap_or(Type::Unknown),
                ),
            ),
            "Option.toResult" if args.len() == 2 => Some(Type::Result(
                Box::new(
                    self.infer_option_inner_aver_type(&args[0])
                        .unwrap_or(Type::Unknown),
                ),
                Box::new(self.infer_aver_type(&args[1].node).unwrap_or(Type::Unknown)),
            )),
            "Byte.toHex" if args.len() == 1 => {
                Some(Type::Result(Box::new(Type::Str), Box::new(Type::Str)))
            }
            "Byte.fromHex" if args.len() == 1 => {
                Some(Type::Result(Box::new(Type::Int), Box::new(Type::Str)))
            }
            "Terminal.readKey" if args.is_empty() => Some(Type::Option(Box::new(Type::Str))),
            "Terminal.size" if args.is_empty() => Some(Type::Named("Terminal.Size".to_string())),
            _ => None,
        }
    }

    /// Infer the WASM type of a record field from type definitions.
    pub(super) fn infer_record_field_type(
        &self,
        base_expr: &Spanned<Expr>,
        field_name: &str,
    ) -> WasmType {
        // Try to find the record type from the base expression's aver type
        let base_aver_type = self.infer_aver_type(&base_expr.node);
        let type_name = match &base_aver_type {
            Some(Type::Named(name)) => Some(name.as_str()),
            _ => None,
        };

        if let Some(type_name) = type_name
            && type_name == "Terminal.Size"
        {
            return WasmType::I64;
        }

        if let Some(type_name) = type_name
            && let Some(fields) = self.record_fields(type_name)
        {
            for (fname, ftype) in fields {
                if fname == field_name {
                    return self.type_str_to_wasm(ftype);
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

    pub(super) fn infer_record_field_aver_type(
        &self,
        base_expr: &Spanned<Expr>,
        field_name: &str,
    ) -> Option<Type> {
        let base_type_name = match self.infer_aver_type(&base_expr.node) {
            Some(Type::Named(name)) => Some(name),
            _ => None,
        };

        if let Some(type_name) = base_type_name.as_deref()
            && type_name == "Terminal.Size"
        {
            return match field_name {
                "width" | "height" => Some(Type::Int),
                _ => None,
            };
        }

        if let Some(type_name) = base_type_name.as_deref()
            && let Some(fields) = self.record_fields(type_name)
        {
            for (fname, ftype) in fields {
                if fname == field_name {
                    return Some(parse_type_str(ftype));
                }
            }
        }

        for td in &self.ctx.type_defs {
            if let crate::ast::TypeDef::Product { fields, .. } = td {
                for (fname, ftype) in fields {
                    if fname == field_name {
                        return Some(parse_type_str(ftype));
                    }
                }
            }
        }

        None
    }

    pub(super) fn type_str_to_wasm(&self, type_str: &str) -> WasmType {
        match type_str {
            "Float" => WasmType::F64,
            "Bool" => WasmType::I32,
            "String" | "Str" => WasmType::I32,
            "Int" => WasmType::I64,
            "Unit" => WasmType::I32,
            // User-defined types and unknown types are heap-allocated -> I32
            _ => WasmType::I32,
        }
    }

    pub(super) fn record_fields(&self, type_name: &str) -> Option<&[(String, String)]> {
        for td in &self.ctx.type_defs {
            if let crate::ast::TypeDef::Product { name, fields, .. } = td
                && name == type_name
            {
                return Some(fields.as_slice());
            }
        }

        for module in &self.ctx.modules {
            for td in &module.type_defs {
                if let crate::ast::TypeDef::Product { name, fields, .. } = td
                    && (name == type_name || format!("{}.{}", module.prefix, name) == type_name)
                {
                    return Some(fields.as_slice());
                }
            }
        }

        None
    }
}
