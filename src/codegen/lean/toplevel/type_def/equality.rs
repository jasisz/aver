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
            Type::Named { .. } => {
                let Some(name) = crate::codegen::common::backend_named_type_key(ctx, ty) else {
                    return false;
                };
                let Some((td, owner)) = super::find_type_def_scoped(ctx, &name, scope) else {
                    return false;
                };
                if crate::codegen::proof_recognize::detect_canonical_peano(td).is_some() {
                    return true;
                }
                let name = crate::codegen::common::type_def_name(td);
                let key = super::canonical_type_name(name, owner);
                // A type met again inside itself: Lean's `LawfulBEq` handler
                // derives a nested or mutual inductive's reflection for none
                // of them.
                if seen.contains(&key) {
                    return false;
                }
                seen.push(key.clone());
                // A field that is the type itself, directly, is the
                // recursion the handler proves by induction, unless a field
                // is a refinement (a `Subtype`) directly: the handler then
                // cannot connect the derived `BEq` to it (see
                // `equality_lines`).
                let (mut itself_held, mut refined_held) = (false, false);
                let mut field = |annotation: &str| {
                    refined_held |= super::is_direct_refined_field(annotation, ctx, owner);
                    let ty = crate::types::parse_type_str(annotation);
                    let itself = matches!(ty, Type::Named { .. })
                        && crate::codegen::common::backend_named_type_key(ctx, &ty)
                            .and_then(|n| super::find_type_def_scoped(ctx, &n, owner))
                            .is_some_and(|(found, at)| {
                                super::canonical_type_name(
                                    crate::codegen::common::type_def_name(found),
                                    at,
                                ) == key
                            });
                    itself_held |= itself;
                    itself || visit(&ty, ctx, owner, seen)
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
                result && !(itself_held && refined_held)
            }
            Type::Float | Type::Fn(..) | Type::Var(_) | Type::Invalid => false,
        }
    }
    visit(ty, ctx, scope, &mut Vec::new())
}
