//! The declared types a script mentions, for the kernel's type checks:
//! every sum with its constructors' field types and every record with its
//! fields' types, reached from the types the script states (its givens,
//! its definitions' signatures, its constants) and from the type of every
//! term it writes, transitively through their fields.

use crate::ast::{Type, TypeDef};
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::hir::ResolvedCtor;
use crate::ir::identity::TypeId;
use crate::ir::proof_steps::term::{self, Term};
use crate::ir::proof_steps::{Decls, RecDecl, Script, SumDecl};

/// Every term `script` writes: its statement, its definitions and
/// constants, the laws it cites, and each step of its proof.
fn each_term(script: &Script, f: &mut dyn FnMut(&Term)) {
    f(&script.obligation.lhs);
    f(&script.obligation.rhs);
    script.obligation.premise.iter().for_each(&mut *f);
    for d in &script.defs {
        f(&d.body);
        d.lets.iter().for_each(|(_, v)| f(v));
    }
    script.consts.iter().for_each(|c| f(&c.value));
    for l in &script.laws {
        f(&l.lhs);
        f(&l.rhs);
        l.premise.iter().for_each(&mut *f);
    }
    script.proof.each_term(f);
}

/// Give `script` what the kernel types its terms by: the signature of
/// every function it calls without opening, and every sum and record its
/// types mention.
pub(crate) fn declare_types(inputs: &ProofLowerInputs, script: &mut Script) {
    script.sigs = signatures(inputs, script);
    script.decls = collect(inputs, script);
}

/// The declarations `script` needs.
fn collect(inputs: &ProofLowerInputs, script: &Script) -> Decls {
    let mut types: Vec<Type> = Vec::new();
    let stated = script
        .obligation
        .types
        .iter()
        .chain(script.laws.iter().flat_map(|l| &l.types));
    types.extend(stated.map(|(_, t)| t.clone()));
    for d in &script.defs {
        types.extend(d.param_types.iter().cloned());
        types.push(d.ret.clone());
    }
    types.extend(script.consts.iter().map(|c| c.ty.clone()));
    for (_, params, ret) in &script.sigs {
        types.extend(params.iter().cloned());
        types.push(ret.clone());
    }
    split_types(&script.proof, &mut types);
    each_term(script, &mut |t| term_types(t, &mut types));
    let mut out = Decls::default();
    let mut seen: Vec<TypeId> = Vec::new();
    for t in &types {
        declare(inputs, t, None, &mut seen, &mut out);
    }
    out
}

/// The signature of every function a term of `script` calls without the
/// script opening it: the kernel types such a call by it.
fn signatures(
    inputs: &ProofLowerInputs,
    script: &Script,
) -> Vec<(crate::ir::identity::FnId, Vec<Type>, Type)> {
    let mut ids: Vec<crate::ir::identity::FnId> = Vec::new();
    each_term(script, &mut |t| callees(t, &mut ids));
    ids.retain(|id| !script.defs.iter().any(|d| d.fn_id == *id));
    ids.into_iter()
        .filter_map(|id| {
            let symbols = inputs.symbol_table;
            let key = symbols.fn_entry(id).key.clone();
            let scope = key.scope_str();
            let fd = match scope {
                Some(prefix) => inputs
                    .dep_modules
                    .iter()
                    .find(|m| m.prefix == prefix)?
                    .fn_defs
                    .iter()
                    .find(|f| f.name == key.name)?,
                None => inputs.entry_items.iter().find_map(|item| match item {
                    crate::ast::TopLevel::FnDef(f) if f.name == key.name => Some(f),
                    _ => None,
                })?,
            };
            let ty =
                |t: &str| super::finite::qualify(inputs, &crate::types::parse_type_str(t), scope);
            Some((
                id,
                fd.params.iter().map(|(_, t)| ty(t)).collect(),
                ty(&fd.return_type),
            ))
        })
        .collect()
}

fn callees(t: &Term, out: &mut Vec<crate::ir::identity::FnId>) {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Fn(id), _)
        | ResolvedExpr::TailCall { target: id, .. }
            if !out.contains(id) =>
        {
            out.push(*id)
        }
        _ => {}
    }
    let _ = term::map_children(t, &mut |c| {
        callees(c, out);
        Ok(c.clone())
    });
}

/// The declared type a constructor belongs to.
fn ctor_type(c: &ResolvedCtor) -> Option<Type> {
    match c {
        ResolvedCtor::User { type_id, .. } => Some(Type::Named {
            id: Some(*type_id),
            name: String::new(),
        }),
        _ => None,
    }
}

/// The sum each split of `p` is along.
fn split_types(p: &crate::ir::proof_steps::Proof, out: &mut Vec<Type>) {
    use crate::ir::proof_steps::{Proof, SplitCtor};
    if let Proof::Split { cases, .. } = p {
        for c in cases {
            if let SplitCtor::Ctor(c) = &c.ctor {
                out.extend(ctor_type(c));
            }
        }
    }
    super::each_part(p, &mut |q| split_types(q, out));
}

/// The type of every node of `t`, and the record or sum a literal of one
/// names: a term the producers built carries no type of its own.
fn term_types(t: &Term, out: &mut Vec<Type>) {
    use crate::ir::hir::ResolvedExpr;
    if let Some(ty) = t.ty() {
        out.push(ty.clone());
    }
    match &t.node {
        ResolvedExpr::RecordCreate {
            type_id, type_name, ..
        }
        | ResolvedExpr::RecordUpdate {
            type_id, type_name, ..
        } => out.push(Type::Named {
            id: *type_id,
            name: type_name.clone(),
        }),
        ResolvedExpr::Ctor(c, _) => out.extend(ctor_type(c)),
        ResolvedExpr::Match { arms, .. } => {
            for a in arms {
                if let crate::ir::hir::ResolvedPattern::Ctor(c, _) = &a.pattern {
                    out.extend(ctor_type(c));
                }
            }
        }
        _ => {}
    }
    let _ = term::map_children(t, &mut |c| {
        term_types(c, out);
        Ok(c.clone())
    });
}

/// Declare the named types inside `ty`, from module `scope`, and the
/// types their fields mention.
fn declare(
    inputs: &ProofLowerInputs,
    ty: &Type,
    scope: Option<&str>,
    seen: &mut Vec<TypeId>,
    out: &mut Decls,
) {
    match ty {
        Type::Named { id, name } => {
            let symbols = inputs.symbol_table;
            let by_bare = || {
                let bare = name.rsplit('.').next().unwrap_or(name);
                match symbols.type_ids_named(bare).as_slice() {
                    [one] => Some(*one),
                    _ => None,
                }
            };
            let Some(id) = id
                .or_else(|| symbols.resolve_type_id_in(name, scope))
                .or_else(by_bare)
            else {
                return;
            };
            if seen.contains(&id) {
                return;
            }
            seen.push(id);
            let Some(entry) = symbols.type_entry_if_present(id) else {
                return;
            };
            if entry.is_capability_resource {
                return;
            }
            let owner = entry.key.scope_str();
            let eq = symbols.beq_is_equality(&Type::Named {
                id: Some(id),
                name: entry.key.name.clone(),
            });
            let qualified = match owner {
                Some(s) => format!("{s}.{}", entry.key.name),
                None => entry.key.name.clone(),
            };
            match super::finite::type_def(inputs, owner, &entry.key.name) {
                Some(TypeDef::Sum { variants, .. }) => {
                    let ctors: Vec<(ResolvedCtor, Vec<Type>)> = entry
                        .variants
                        .iter()
                        .map(|c| {
                            let ctor_name = symbols.ctor_entry(*c).name.clone();
                            let fields: Vec<Type> = variants
                                .iter()
                                .find(|v| v.name == ctor_name)
                                .map(|v| {
                                    v.fields
                                        .iter()
                                        .map(|f| {
                                            super::finite::qualify(
                                                inputs,
                                                &crate::types::parse_type_str(f),
                                                owner,
                                            )
                                        })
                                        .collect()
                                })
                                .unwrap_or_default();
                            (
                                ResolvedCtor::User {
                                    ctor_id: *c,
                                    type_id: id,
                                    name: ctor_name,
                                },
                                fields,
                            )
                        })
                        .collect();
                    for (_, fields) in &ctors {
                        for f in fields {
                            declare(inputs, f, owner, seen, out);
                        }
                    }
                    out.sums.push(SumDecl {
                        name: qualified,
                        eq,
                        ctors,
                    });
                }
                Some(TypeDef::Product { fields, .. }) => {
                    let fields: Vec<(String, Type)> = fields
                        .iter()
                        .map(|(n, t)| {
                            (
                                n.clone(),
                                super::finite::qualify(
                                    inputs,
                                    &crate::types::parse_type_str(t),
                                    owner,
                                ),
                            )
                        })
                        .collect();
                    for (_, f) in &fields {
                        declare(inputs, f, owner, seen, out);
                    }
                    out.recs.push(RecDecl {
                        name: qualified,
                        eq,
                        fields,
                    });
                }
                None => {}
            }
        }
        Type::List(a) | Type::Option(a) | Type::Vector(a) => declare(inputs, a, scope, seen, out),
        Type::Result(a, b) | Type::Map(a, b) => {
            declare(inputs, a, scope, seen, out);
            declare(inputs, b, scope, seen, out);
        }
        Type::Tuple(xs) => {
            for x in xs {
                declare(inputs, x, scope, seen, out);
            }
        }
        _ => {}
    }
}
