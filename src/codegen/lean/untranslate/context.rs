//! Source-backed inverse metadata. Owner-qualified emitted names are display
//! keys, derived from declarations and the emitter's naming functions; bare
//! names are never used to choose between records from different modules.

use super::{PeanoCtx, UntranslateCtx};
use crate::ast::{TypeDef, VerifyLaw};
use crate::codegen::CodegenContext;

/// Build inverse display metadata for a law in its actual defining module.
/// `scope = None` denotes the entry file. Projection metadata excludes the
/// exact refinements that the emitter lowers to `Subtype`, whose indices no
/// longer denote source record fields. Peano inversion is enabled only when
/// the law's givens resolve to one unambiguous canonical-Peano declaration.
pub fn context_for_law(
    ctx: &CodegenContext,
    scope: Option<&str>,
    law: &VerifyLaw,
) -> UntranslateCtx {
    let mut result = UntranslateCtx::default();
    let mut peanos = Vec::new();
    let mut register = |types: &[TypeDef], owner: Option<&str>, namespace: &str| {
        for td in types {
            if let TypeDef::Product { name, fields, .. } = td
                && crate::codegen::common::find_refined_type_scoped(ctx, name, owner).is_none()
            {
                let emitted = format!(
                    "{namespace}.{}",
                    super::super::expr::aver_name_to_lean(name)
                );
                result.record_fields.insert(
                    emitted,
                    fields.iter().map(|(field, _)| field.clone()).collect(),
                );
            }
            if let Some(peano) = crate::codegen::proof_recognize::detect_canonical_peano(td)
                && let Some(id) = ctx.symbol_table.resolve_type_id_in(&peano.type_name, owner)
            {
                let source_type = owner
                    .map(|owner| format!("{owner}.{}", peano.type_name))
                    .unwrap_or(peano.type_name);
                peanos.push((
                    id,
                    PeanoCtx {
                        type_name: source_type,
                        zero_ctor: peano.base_ctor,
                        succ_ctor: peano.succ_ctor,
                    },
                ));
            }
        }
    };
    register(&ctx.type_defs, None, &super::super::lean_project_name(ctx));
    for module in &ctx.modules {
        register(
            &module.type_defs,
            Some(&module.prefix),
            &super::super::syntax::aver_path_to_lean(&module.prefix),
        );
    }
    let mut found = None;
    for given in &law.givens {
        for token in super::type_name_tokens(&given.type_name) {
            let Some(id) = ctx.symbol_table.resolve_type_id_in(&token, scope) else {
                continue;
            };
            let Some((_, peano)) = peanos.iter().find(|(candidate, _)| *candidate == id) else {
                continue;
            };
            if let Some((previous, _)) = &found {
                if *previous != id {
                    return result;
                }
            } else {
                found = Some((id, peano.clone()));
            }
        }
    }
    result.peano = found.map(|(_, peano)| peano);
    result
}
