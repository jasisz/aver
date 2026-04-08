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
}

/// Build variant registry from type_defs.
pub(in crate::codegen::wasm) fn build_variant_registry(
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
            Expr::Tuple(items) => Some(Type::Tuple(
                items
                    .iter()
                    .map(|item| self.infer_aver_type(&item.node).unwrap_or(Type::Unknown))
                    .collect(),
            )),
            Expr::RecordCreate { type_name, .. } => Some(Type::Named(type_name.clone())),
            Expr::Attr(base, field) => {
                if let Expr::Ident(base_name) = &base.node
                    && base_name.chars().next().is_some_and(|c| c.is_uppercase())
                {
                    return None;
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

    pub(super) fn infer_call_return_type(&self, callee: &Spanned<Expr>) -> WasmType {
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

    pub(super) fn infer_call_aver_return_type(&self, callee: &Spanned<Expr>) -> Option<Type> {
        let plan = classify_call_plan(&callee.node, &self.ir_ctx());
        match plan {
            CallPlan::Function(name) | CallPlan::Builtin(name) => self
                .fn_sigs
                .get(name.as_str())
                .map(|(_, ret, _)| ret.clone()),
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

    pub(super) fn infer_record_field_aver_type(
        &self,
        base_expr: &Spanned<Expr>,
        field_name: &str,
    ) -> Option<Type> {
        let base_type_name = match self.infer_aver_type(&base_expr.node) {
            Some(Type::Named(name)) => Some(name),
            _ => None,
        };

        if let Some(type_name) = base_type_name.as_deref() {
            for td in &self.ctx.type_defs {
                if let crate::ast::TypeDef::Product { name, fields, .. } = td
                    && name == type_name
                {
                    for (fname, ftype) in fields {
                        if fname == field_name {
                            return Some(parse_type_str(ftype));
                        }
                    }
                }
            }

            for module in &self.ctx.modules {
                for td in &module.type_defs {
                    if let crate::ast::TypeDef::Product { name, fields, .. } = td
                        && (name == type_name || format!("{}.{}", module.prefix, name) == type_name)
                    {
                        for (fname, ftype) in fields {
                            if fname == field_name {
                                return Some(parse_type_str(ftype));
                            }
                        }
                    }
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
}
