//! Sound Lean treatment of provider-owned pure capability operations.
//!
//! A capability operation is exported as `noncomputable opaque`.  Lean's
//! `opaque` needs a `Nonempty` result type, and the export establishes it
//! with a proposition-only witness where Lean core has none (records, sums,
//! handles) — never `Inhabited`, which would hand the declaration a
//! fabricated default.  The explicit modifier matters even then: a global
//! `Inhabited` instance (Int, Bool, String, …) lets Lean compile a plain
//! `opaque` through `default`, and `native_decide` would decide claims about
//! the provider by that default.  This module also tracks the transitive
//! caller cone so the cone is emitted noncomputable and every sampled claim
//! reaching it is refused before either evaluator sees it.

use std::collections::{BTreeSet, HashMap, HashSet};

use crate::ast::{Spanned, Type, TypeDef};
use crate::codegen::CodegenContext;
use crate::ir::FnId;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedStmt};

#[derive(Clone, Default)]
pub(super) struct CapabilityOpacity {
    operations_by_fn: HashMap<FnId, BTreeSet<String>>,
    unsupported_operations: BTreeSet<String>,
}

impl CapabilityOpacity {
    pub(super) fn analyze(ctx: &CodegenContext) -> Self {
        let mut calls_by_fn = HashMap::<FnId, HashSet<FnId>>::new();
        let mut operations_by_fn = HashMap::<FnId, BTreeSet<String>>::new();

        for fd in ctx.resolved_program.entry_fns().chain(
            ctx.resolved_program
                .modules
                .iter()
                .flat_map(|module| module.fn_defs.iter()),
        ) {
            let mut calls = HashSet::new();
            let mut operations = BTreeSet::new();
            for stmt in fd.body.stmts() {
                let (ResolvedStmt::Expr(expr) | ResolvedStmt::Binding { value: expr, .. }) = stmt;
                collect_dependencies(expr, ctx, &mut calls, &mut operations);
            }
            calls_by_fn.insert(fd.fn_id, calls);
            operations_by_fn.insert(fd.fn_id, operations);
        }

        // Reverse-call closure by fixed point.  SCC members converge together,
        // so a mutual component is either wholly computable or wholly in the
        // provider-owned cone.
        loop {
            let snapshot = operations_by_fn.clone();
            let mut changed = false;
            for (fn_id, callees) in &calls_by_fn {
                let reached = operations_by_fn.entry(*fn_id).or_default();
                let before = reached.len();
                for callee in callees {
                    if let Some(operations) = snapshot.get(callee) {
                        reached.extend(operations.iter().cloned());
                    }
                }
                changed |= reached.len() != before;
            }
            if !changed {
                break;
            }
        }

        let unsupported_operations = ctx
            .capabilities
            .operations()
            .filter(|operation| !operation.is_effectful())
            .filter(|operation| {
                nonempty_witness(&operation.return_type, &operation.module, ctx, &mut vec![])
                    .is_none()
            })
            .map(|operation| operation.canonical_name.clone())
            .collect();

        Self {
            operations_by_fn,
            unsupported_operations,
        }
    }

    pub(super) fn component_is_noncomputable(
        &self,
        component: &[&crate::ast::FnDef],
        ctx: &CodegenContext,
    ) -> bool {
        component.iter().any(|fd| {
            crate::codegen::common::fn_id_for_decl(ctx, fd).is_some_and(|fn_id| {
                self.operations_by_fn
                    .get(&fn_id)
                    .is_some_and(|operations| !operations.is_empty())
            })
        })
    }

    pub(super) fn unsupported_component_dependencies(
        &self,
        component: &[&crate::ast::FnDef],
        ctx: &CodegenContext,
    ) -> Vec<String> {
        let mut operations = BTreeSet::new();
        for fd in component {
            let Some(fn_id) = crate::codegen::common::fn_id_for_decl(ctx, fd) else {
                continue;
            };
            if let Some(reached) = self.operations_by_fn.get(&fn_id) {
                operations.extend(
                    reached
                        .iter()
                        .filter(|operation| self.unsupported_operations.contains(*operation))
                        .cloned(),
                );
            }
        }
        operations.into_iter().collect()
    }

    /// The refusal for a claim whose `roots` — everything the claim
    /// evaluates — reach a capability operation, or `None` when they reach
    /// none.  An operation without a witness was not exported at all, which
    /// is the reason given when one is in the cone.
    pub(super) fn decline_reason(
        &self,
        roots: &[&Spanned<crate::ast::Expr>],
        ctx: &CodegenContext,
    ) -> Option<String> {
        let scope = ctx.active_module_scope();
        let mut calls = HashSet::new();
        let mut operations = BTreeSet::new();
        for root in roots {
            let resolved = ctx.resolve_expr(root, scope.as_deref());
            collect_dependencies(&resolved, ctx, &mut calls, &mut operations);
        }
        for fn_id in calls {
            if let Some(reached) = self.operations_by_fn.get(&fn_id) {
                operations.extend(reached.iter().cloned());
            }
        }
        if operations.is_empty() {
            return None;
        }
        let unsupported = operations
            .iter()
            .filter(|operation| self.unsupported_operations.contains(*operation))
            .cloned()
            .collect::<Vec<_>>();
        Some(if unsupported.is_empty() {
            format!(
                "the Lean call cone reaches provider-owned capability operation(s) {}, which are opaque and noncomputable; evaluating them could fabricate a result, so this claim was not exported",
                operations.into_iter().collect::<Vec<_>>().join(", ")
            )
        } else {
            format!(
                "the Lean call cone reaches capability operation(s) {} whose result type has no sound Nonempty witness; the operation and this claim were not exported",
                unsupported.join(", ")
            )
        })
    }
}

pub(super) fn emit_operation(
    operation: &crate::capability::CapabilityOperation,
    name: &str,
    ty: &str,
    ctx: &CodegenContext,
) -> String {
    let Some(witness) =
        nonempty_witness(&operation.return_type, &operation.module, ctx, &mut vec![])
    else {
        return format!(
            "-- capability operation {name} was not exported: no sound Nonempty witness can be established for {}",
            super::types::type_to_lean(&operation.return_type)
        );
    };
    let declaration = format!("noncomputable opaque {name} : {ty}");
    if witness.core {
        declaration
    } else {
        format!(
            "local instance : Nonempty {} := ⟨{}⟩\n{declaration}",
            super::types::type_to_lean_atom(&operation.return_type),
            witness.term
        )
    }
}

fn collect_dependencies(
    expr: &Spanned<ResolvedExpr>,
    ctx: &CodegenContext,
    calls: &mut HashSet<FnId>,
    operations: &mut BTreeSet<String>,
) {
    super::decl_order::for_each_resolved_callee(expr, &mut |callee| match callee {
        ResolvedCallee::Fn(fn_id) => {
            calls.insert(*fn_id);
        }
        ResolvedCallee::Builtin(name)
            if ctx
                .capabilities
                .operation(name)
                .is_some_and(|operation| !operation.is_effectful()) =>
        {
            operations.insert(name.clone());
        }
        _ => {}
    });
}

/// A term inhabiting a capability codomain, and whether Lean already knows
/// the type is `Nonempty` without it: core scalars and containers, the
/// instance `Bytes.lean` carries, and `Except`/products/functions over those.
struct Witness {
    term: String,
    core: bool,
}

fn nonempty_witness(
    ty: &Type,
    scope: &str,
    ctx: &CodegenContext,
    visiting: &mut Vec<String>,
) -> Option<Witness> {
    let core = |term: &str| {
        Some(Witness {
            term: term.to_string(),
            core: true,
        })
    };
    match ty {
        Type::Unit => core("()"),
        Type::Bool => core("false"),
        Type::Int => core("0"),
        Type::Float => core("(0.0 : Float)"),
        Type::Str => core("\"\""),
        Type::List(_) | Type::Map(_, _) => core("[]"),
        Type::Vector(_) => core("#[]"),
        Type::Option(_) => core("Option.none"),
        Type::Result(ok, err) => {
            let ok = nonempty_witness(ok, scope, ctx, visiting);
            let err = nonempty_witness(err, scope, ctx, visiting);
            let core = ok.as_ref().is_some_and(|witness| witness.core)
                || err.as_ref().is_some_and(|witness| witness.core);
            let term = err
                .map(|witness| format!("Except.error ({})", witness.term))
                .or_else(|| ok.map(|witness| format!("Except.ok ({})", witness.term)))?;
            Some(Witness { term, core })
        }
        Type::Tuple(items) => {
            let witnesses = items
                .iter()
                .map(|item| nonempty_witness(item, scope, ctx, visiting))
                .collect::<Option<Vec<_>>>()?;
            let core = witnesses.iter().all(|witness| witness.core);
            let terms = witnesses
                .into_iter()
                .map(|witness| witness.term)
                .collect::<Vec<_>>();
            let term = match terms.as_slice() {
                [] => "()".to_string(),
                [only] => only.clone(),
                _ => format!("({})", terms.join(", ")),
            };
            Some(Witness { term, core })
        }
        Type::Fn(params, result, _) => {
            let result = nonempty_witness(result, scope, ctx, visiting)?;
            Some(Witness {
                term: format!("{}{}", "fun _ => ".repeat(params.len()), result.term),
                core: result.core,
            })
        }
        Type::Named { name, .. } => named_witness(name, scope, ctx, visiting),
        Type::Var(_) | Type::Invalid => None,
    }
}

fn named_witness(
    name: &str,
    scope: &str,
    ctx: &CodegenContext,
    visiting: &mut Vec<String>,
) -> Option<Witness> {
    if name == "Bytes" || name == "Bytes.Bytes" {
        return Some(Witness {
            term: "⟨[], by simp [Bytes.allInRange]⟩".to_string(),
            core: true,
        });
    }
    if ctx.capabilities.resource_types().any(|canonical| {
        canonical == name
            || canonical
                .rsplit_once('.')
                .is_some_and(|(module, bare)| module == scope && bare == name)
    }) {
        return Some(Witness {
            term: "{ id := 0 }".to_string(),
            core: false,
        });
    }

    let (td, owner) = find_type_def(name, scope, ctx)?;
    let canonical = format!("{owner}.{}", crate::codegen::common::type_def_name(td));
    if visiting.contains(&canonical)
        || crate::codegen::common::find_refined_type_scoped(ctx, name, Some(owner)).is_some()
    {
        return None;
    }
    visiting.push(canonical);
    let term = match td {
        TypeDef::Product { fields, .. } => {
            let assignments = fields
                .iter()
                .map(|(field, annotation)| {
                    let field_ty = crate::types::parse_type_str(annotation);
                    nonempty_witness(&field_ty, owner, ctx, visiting).map(|witness| {
                        format!(
                            "{} := {}",
                            super::syntax::aver_name_to_lean(field),
                            witness.term
                        )
                    })
                })
                .collect::<Option<Vec<_>>>();
            assignments.map(|assignments| {
                if assignments.is_empty() {
                    "{}".to_string()
                } else {
                    format!("{{ {} }}", assignments.join(", "))
                }
            })
        }
        TypeDef::Sum {
            name: type_name,
            variants,
            ..
        } => variants.iter().find_map(|variant| {
            let fields = variant
                .fields
                .iter()
                .map(|annotation| {
                    nonempty_witness(
                        &crate::types::parse_type_str(annotation),
                        owner,
                        ctx,
                        visiting,
                    )
                })
                .collect::<Option<Vec<_>>>()?;
            let type_name = super::syntax::aver_name_to_lean(type_name);
            let ctor = super::syntax::lean_ctor_name(&variant.name);
            let args = fields
                .iter()
                .map(|field| format!(" ({})", field.term))
                .collect::<String>();
            Some(format!("{type_name}.{ctor}{args}"))
        }),
    };
    visiting.pop();
    term.map(|term| Witness { term, core: false })
}

fn find_type_def<'a>(
    name: &str,
    scope: &str,
    ctx: &'a CodegenContext,
) -> Option<(&'a TypeDef, &'a str)> {
    fn named<'a>(defs: &'a [TypeDef], bare: &str) -> Option<&'a TypeDef> {
        defs.iter()
            .find(|td| crate::codegen::common::type_def_name(td) == bare)
    }

    if let Some((prefix, bare)) = name.rsplit_once('.') {
        let module = ctx.modules.iter().find(|module| module.prefix == prefix)?;
        return named(&module.type_defs, bare).map(|td| (td, module.prefix.as_str()));
    }
    if let Some(module) = ctx.modules.iter().find(|module| module.prefix == scope)
        && let Some(td) = named(&module.type_defs, name)
    {
        return Some((td, module.prefix.as_str()));
    }
    ctx.modules
        .iter()
        .find_map(|module| named(&module.type_defs, name).map(|td| (td, module.prefix.as_str())))
}
