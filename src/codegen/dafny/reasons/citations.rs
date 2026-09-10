//! Resolve selected laws by canonical function identity, retaining the source
//! module for validation and resolving expressions before rendering at a caller.
use super::super::{
    expr::emit_expr,
    toplevel::{resolve_rewrite_output, type_to_dafny_in_scope},
};
use super::{label, lemma_name};
use crate::ast::{Expr, Spanned, Type, VerifyBlock, VerifyKind, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use std::collections::BTreeMap;

pub(super) type Key = (FnId, String);
#[derive(Clone, Copy)]
pub(super) struct Citation<'a> {
    pub block: &'a VerifyBlock,
    pub law: &'a VerifyLaw,
    pub scope: Option<&'a str>,
}

pub(super) fn key(
    block: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Result<Key, String> {
    let scope = ctx.active_module_scope();
    let id = ctx
        .symbol_table
        .resolve_fn_id_in(&block.fn_name, scope.as_deref())
        .ok_or_else(|| format!("unresolved law target {}", block.fn_name))?;
    Ok((id, law.name.clone()))
}

pub(super) fn index<'a>(
    ctx: &'a CodegenContext,
    local: &[&'a VerifyBlock],
) -> Result<BTreeMap<Key, Citation<'a>>, String> {
    let active = ctx.active_module_scope();
    let local_scope = active
        .as_deref()
        .map(|scope| {
            ctx.modules
                .iter()
                .find(|m| m.prefix == scope)
                .map(|m| m.prefix.as_str())
                .ok_or_else(|| format!("unknown law module {scope}"))
        })
        .transpose()?;
    let mut result = BTreeMap::new();
    let mut insert = |block: &'a VerifyBlock, scope: Option<&'a str>| -> Result<(), String> {
        let VerifyKind::Law(law) = &block.kind else {
            return Ok(());
        };
        // Unrelated builtin/effect law targets need not have a user FnId.
        // A selected or current law still requires an exact resolvable target.
        let Ok(key) = ctx.with_module_scope(scope, || key(block, law, ctx)) else {
            return Ok(());
        };
        if result.insert(key, Citation { block, law, scope }).is_some() {
            return Err(format!(
                "ambiguous source law identity {}",
                label(block, law)
            ));
        }
        Ok(())
    };
    for block in local {
        insert(block, local_scope)?;
    }
    for module in &ctx.modules {
        if Some(module.prefix.as_str()) != active.as_deref() {
            for block in &module.verify_blocks {
                insert(block, Some(&module.prefix))?;
            }
        }
    }
    Ok(result)
}

pub(super) fn select<'a>(
    laws: &BTreeMap<Key, Citation<'a>>,
    name: &str,
    ctx: &CodegenContext,
) -> Result<(Key, Citation<'a>), String> {
    let (function, law) = name
        .rsplit_once('.')
        .ok_or_else(|| format!("invalid citation {name}"))?;
    let scope = ctx.active_module_scope();
    let id = ctx
        .symbol_table
        .resolve_fn_id_in(function, scope.as_deref())
        .ok_or_else(|| format!("citation {name} has no visible function"))?;
    let key = (id, law.to_string());
    let found = laws
        .get(&key)
        .copied()
        .ok_or_else(|| format!("citation {name} is not an available law"))?;
    Ok((key, found))
}

pub(super) fn call_name(citation: Citation<'_>, ctx: &CodegenContext) -> String {
    let name = lemma_name(&label(citation.block, citation.law));
    match citation.scope {
        Some(scope) if Some(scope) != ctx.active_module_scope().as_deref() => {
            format!("Aver_{}.{}", scope.replace('.', "_"), name)
        }
        _ => name,
    }
}

pub(super) fn expression(
    expr: &Spanned<Expr>,
    owner: Option<&str>,
    ctx: &CodegenContext,
) -> String {
    let resolved = ctx.with_module_scope(owner, || resolve_rewrite_output(expr, ctx));
    emit_expr(&resolved, ctx)
}

fn canonical(ty: Type, owner: Option<&str>, ctx: &CodegenContext) -> Result<Type, String> {
    Ok(match ty {
        Type::Named { id, name } => {
            let id = id
                .or_else(|| ctx.symbol_table.resolve_type_id_in(&name, owner))
                .ok_or_else(|| format!("unresolved citation type {name}"))?;
            Type::named_resolved(id, ctx.symbol_table.type_entry(id).key.canonical())
        }
        Type::List(t) => Type::List(Box::new(canonical(*t, owner, ctx)?)),
        Type::Vector(t) => Type::Vector(Box::new(canonical(*t, owner, ctx)?)),
        Type::Map(a, b) => Type::Map(
            Box::new(canonical(*a, owner, ctx)?),
            Box::new(canonical(*b, owner, ctx)?),
        ),
        Type::Fn(args, result, effects) => Type::Fn(
            args.into_iter()
                .map(|t| canonical(t, owner, ctx))
                .collect::<Result<_, _>>()?,
            Box::new(canonical(*result, owner, ctx)?),
            effects,
        ),
        Type::Option(t) => Type::Option(Box::new(canonical(*t, owner, ctx)?)),
        Type::Result(a, b) => Type::Result(
            Box::new(canonical(*a, owner, ctx)?),
            Box::new(canonical(*b, owner, ctx)?),
        ),
        Type::Tuple(ts) => Type::Tuple(
            ts.into_iter()
                .map(|t| canonical(t, owner, ctx))
                .collect::<Result<_, _>>()?,
        ),
        other => other,
    })
}

pub(super) fn binders(citation: Citation<'_>, ctx: &CodegenContext) -> Result<String, String> {
    citation
        .law
        .givens
        .iter()
        .map(|given| {
            let ty =
                crate::types::parse_type_str_strict(&given.type_name).map_err(|e| e.to_string())?;
            let ty = canonical(ty, citation.scope, ctx)?;
            Ok(format!(
                "{}: {}",
                super::super::expr::aver_name_to_dafny(&given.name),
                type_to_dafny_in_scope(&ty, ctx.active_module_scope().as_deref())
            ))
        })
        .collect::<Result<Vec<_>, String>>()
        .map(|parts| parts.join(", "))
}

pub(super) fn conclusion(citation: Citation<'_>, ctx: &CodegenContext) -> String {
    let (left, right) = ctx.with_module_scope(citation.scope, || {
        (
            resolve_rewrite_output(&citation.law.lhs, ctx),
            resolve_rewrite_output(&citation.law.rhs, ctx),
        )
    });
    super::equality(&left, &right, ctx)
}

/// Plain source laws keep their original backend strategy. A selected plain
/// supplier also gets a checked universal restatement in its caller. When the
/// ordinary emitter supplies the same universal contract, the restatement calls
/// it; finite-domain and opaque legacy lemmas must still be proved afresh.
pub(super) fn supplier_name(citation: Citation<'_>, parent: &str, ctx: &CodegenContext) -> String {
    if citation.law.using.is_some() || !citation.law.because.is_empty() {
        return call_name(citation, ctx);
    }
    let key = ctx
        .with_module_scope(citation.scope, || key(citation.block, citation.law, ctx))
        .expect("validated citation");
    let canonical = ctx.symbol_table.fn_entry(key.0).key.canonical();
    format!(
        "{parent}_citation_{}",
        lemma_name(&format!("{canonical}.{}", key.1))
    )
}

pub(super) fn plain_supplier(
    citation: Citation<'_>,
    parent: &str,
    consumer: &VerifyLaw,
    ctx: &CodegenContext,
    recursion: &super::super::toplevel::LawRecursion<'_>,
) -> Result<Option<String>, String> {
    if citation.law.using.is_some() || !citation.law.because.is_empty() {
        return Ok(None);
    }
    let name = supplier_name(citation, parent, ctx);
    let scope = ctx.active_module_scope();
    let functions = match scope.as_deref() {
        Some(scope) => ctx
            .modules
            .iter()
            .find(|m| m.prefix == scope)
            .map(|m| m.fn_defs.as_slice())
            .unwrap_or_default(),
        None => ctx.fn_defs.as_slice(),
    };
    if functions
        .iter()
        .any(|f| super::super::expr::aver_name_to_dafny(&f.name) == name)
        || citation
            .law
            .givens
            .iter()
            .chain(&consumer.givens)
            .any(|g| super::super::expr::aver_name_to_dafny(&g.name) == name)
    {
        return Err("generated citation lemma collides with a source name".to_string());
    }
    let params = binders(citation, ctx)?;
    let goal = conclusion(citation, ctx);
    let reuse = ctx.with_module_scope(citation.scope, || {
        super::super::law_search::reusable_ordinary_law(
            citation.block,
            citation.law,
            ctx,
            recursion,
        )
    });
    let induction = ctx
        .with_module_scope(citation.scope, || {
            super::super::law_induction::plan(citation.block, citation.law, ctx)
        })
        .filter(|_| !reuse);
    let source_key =
        ctx.with_module_scope(citation.scope, || key(citation.block, citation.law, ctx))?;
    let source_id = format!(
        "{}.{}",
        ctx.symbol_table.fn_entry(source_key.0).key.canonical(),
        source_key.1
    );
    let mut lines = vec![
        format!("// aver:dafny-citation {name} {source_id}"),
        format!(
            "// Checked universal citation: {}",
            label(citation.block, citation.law)
        ),
        format!("lemma {{:induction false}} {name}({params})"),
    ];
    if let Some(guard) = &citation.law.when {
        lines.push(format!(
            "  requires {}",
            expression(guard, citation.scope, ctx)
        ));
    }
    lines.push(format!("  ensures {goal}"));
    if let Some(plan) = induction {
        lines.push(format!(
            "  decreases {}",
            super::super::law_induction::measure(plan)
        ));
    }
    lines.push("{".to_string());
    if reuse {
        let ordinary = format!(
            "{}_{}",
            super::super::expr::aver_name_to_dafny(&citation.block.fn_name),
            super::super::expr::aver_name_to_dafny(&citation.law.name),
        );
        let ordinary = match citation.scope {
            Some(owner) if Some(owner) != ctx.active_module_scope().as_deref() => {
                format!("Aver_{}.{}", owner.replace('.', "_"), ordinary)
            }
            _ => ordinary,
        };
        lines.push(format!("  {ordinary}({});", super::arguments(citation.law)));
    } else {
        ctx.with_module_scope(citation.scope, || {
            lines.extend(super::super::law_induction::sequence_identities(
                citation.law,
                ctx,
            ));
            if let Some(plan) = induction {
                lines.extend(super::super::law_induction::calls(
                    plan,
                    &name,
                    &[],
                    ctx,
                    recursion,
                ));
            }
        });
    }
    lines.extend([format!("  assert {goal};"), "}".to_string()]);
    Ok(Some(lines.join("\n")))
}
