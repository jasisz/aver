/// Type readers for ExprEmitter.
///
/// After Step 2 (the typed-ABI refactor), the legacy WASM backend no longer
/// performs ad-hoc inference. Every `Spanned<Expr>` reaching codegen has its
/// type stamped by the type checker (`Spanned::ty()`). The accessors here
/// are thin readers that panic if the type was not stamped — that means
/// either the type checker did not run before codegen, or a synthesised AST
/// node skipped `Spanned::set_ty(...)` (interp_lower / buffer_build are the
/// usual suspects). Both are bugs to fix at the source, not to paper over
/// with a fallback here.
///
/// Schema-level helpers that were previously co-located with inference
/// (variant registry, record field lookup by name) stay in this module —
/// they describe the program's static shape, not its inferred types.
use std::collections::HashMap;

use crate::ast::{Expr, Spanned};
use crate::codegen::CodegenContext;
use crate::types::Type;

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
// Typed-AST accessors
// ---------------------------------------------------------------------------

impl<'a> ExprEmitter<'a> {
    /// Inferred Aver type for a `Spanned<Expr>`. Panics if the type checker
    /// did not stamp this node — that is a pipeline bug, not a recoverable
    /// codegen condition (see module doc).
    #[track_caller]
    pub(super) fn aver_type_of<'b>(&self, expr: &'b Spanned<Expr>) -> &'b Type {
        expr.ty().unwrap_or_else(|| {
            panic!(
                "legacy WASM codegen: missing Spanned::ty() for {:?} \
                 (typecheck must run before codegen; synthesised nodes \
                 must call Spanned::set_ty)",
                expr.node
            )
        })
    }

    /// WASM machine type for a `Spanned<Expr>`. Same panic contract as
    /// `aver_type_of`.
    #[track_caller]
    pub(super) fn wasm_type_of(&self, expr: &Spanned<Expr>) -> WasmType {
        aver_type_to_wasm(self.aver_type_of(expr))
    }

    /// Convenience: `expr_is_heap_ptr` working from a `Spanned`.
    pub(super) fn expr_is_heap_ptr_spanned(&self, expr: &Spanned<Expr>) -> bool {
        self.is_heap_type(self.aver_type_of(expr))
    }

    /// Map a type-annotation string (as written in source) to the WASM
    /// machine type used by the typed ABI. Schema-level — used by match
    /// emission when reading variant field types out of the AST as
    /// strings (variants store annotations, not parsed `Type` values).
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
