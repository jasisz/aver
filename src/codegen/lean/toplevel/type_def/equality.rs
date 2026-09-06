//! Eligibility for kernel-derived reflection of executable structural equality.
//! Float and composites containing it retain their IEEE Boolean semantics.

use crate::codegen::CodegenContext;
use crate::types::Type;

pub(super) fn reflects_equality(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> bool {
    fn visit(ty: &Type, ctx: &CodegenContext, scope: Option<&str>, seen: &mut Vec<String>) -> bool {
        match ty {
            Type::Int | Type::Str | Type::Bool | Type::Unit => true,
            Type::List(t) | Type::Vector(t) | Type::Option(t) => visit(t, ctx, scope, seen),
            Type::Result(a, b) | Type::Map(a, b) => {
                visit(a, ctx, scope, seen) && visit(b, ctx, scope, seen)
            }
            Type::Tuple(ts) => ts.iter().all(|t| visit(t, ctx, scope, seen)),
            Type::Named { name, .. } => {
                let Some((td, owner)) = super::find_type_def_scoped(ctx, name, scope) else {
                    return false;
                };
                if crate::codegen::proof_recognize::detect_canonical_peano(td).is_some() {
                    return true;
                }
                let name = crate::codegen::common::type_def_name(td);
                let key = super::canonical_type_name(name, owner);
                // Recursive derived BEq needs a separate inductive reflection proof.
                if seen.contains(&key) {
                    return false;
                }
                seen.push(key);
                let mut field = |annotation: &str| {
                    visit(&crate::types::parse_type_str(annotation), ctx, owner, seen)
                };
                let result = if let Some(refined) =
                    crate::codegen::common::find_refined_type_scoped(ctx, name, owner)
                {
                    field(&refined.carrier_type)
                } else {
                    match td {
                        crate::ast::TypeDef::Product { fields, .. } => {
                            fields.iter().all(|(_, ty)| field(ty))
                        }
                        crate::ast::TypeDef::Sum { variants, .. } => {
                            variants.iter().all(|v| v.fields.iter().all(|ty| field(ty)))
                        }
                    }
                };
                seen.pop();
                result
            }
            Type::Float | Type::Fn(..) | Type::Var(_) | Type::Invalid => false,
        }
    }
    visit(ty, ctx, scope, &mut Vec::new())
}
