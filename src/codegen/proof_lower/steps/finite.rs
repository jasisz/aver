//! Finite domains: a given whose type has finitely many values (`Bool`, a
//! sum type whose variants carry nothing, records and tuples of those) is
//! split into every one of its values, and each case is proved on its own.
//! That is a whole proof, not a sample: the kernel recomputes the values
//! from the type and checks that every one has its case.

use crate::ast::{Type, TypeDef};
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::hir::ResolvedCtor;
use crate::ir::proof_steps::term::{self, Term};
use crate::ir::proof_steps::{Eqn, Finite, Proof};
use crate::ir::{LawTheorem, QuantifierType};

use super::env::Env;

/// Largest number of cases a split may produce, over all its givens.
const MAX_CASES: usize = 64;

fn type_def<'a>(
    inputs: &'a ProofLowerInputs,
    scope: Option<&str>,
    name: &str,
) -> Option<&'a TypeDef> {
    let defs: Vec<&TypeDef> = match scope {
        Some(prefix) => inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)?
            .type_defs
            .iter()
            .collect(),
        None => inputs
            .entry_items
            .iter()
            .filter_map(|item| match item {
                crate::ast::TopLevel::TypeDef(td) => Some(td),
                _ => None,
            })
            .collect(),
    };
    defs.into_iter().find(|td| match td {
        TypeDef::Sum { name: n, .. } | TypeDef::Product { name: n, .. } => n == name,
    })
}

/// The finite type `ty` names in module `scope`, if it is one.
fn finite_of(
    inputs: &ProofLowerInputs,
    ty: &Type,
    scope: Option<&str>,
    depth: usize,
) -> Option<Finite> {
    if depth == 0 {
        return None;
    }
    match ty {
        Type::Bool => Some(Finite::Bool),
        Type::Tuple(parts) => Some(Finite::Tuple(
            parts
                .iter()
                .map(|p| finite_of(inputs, p, scope, depth - 1))
                .collect::<Option<_>>()?,
        )),
        Type::Named { id, name } => {
            let symbols = inputs.symbol_table;
            let id = match id {
                Some(id) => *id,
                None => symbols.resolve_type_id_in(name, scope)?,
            };
            let entry = symbols.type_entry_if_present(id)?;
            if entry.is_capability_resource {
                return None;
            }
            let owner = entry.key.scope_str();
            match type_def(inputs, owner, &entry.key.name)? {
                TypeDef::Sum { variants, .. } => {
                    if variants.iter().any(|v| !v.fields.is_empty())
                        || variants.len() != entry.variants.len()
                    {
                        return None;
                    }
                    Some(Finite::Sum(
                        entry
                            .variants
                            .iter()
                            .map(|c| ResolvedCtor::User {
                                ctor_id: *c,
                                type_id: id,
                                name: symbols.ctor_entry(*c).name.clone(),
                            })
                            .collect(),
                    ))
                }
                TypeDef::Product { name, fields, .. } => Some(Finite::Record {
                    type_id: id,
                    type_name: name.clone(),
                    fields: fields
                        .iter()
                        .map(|(n, t)| {
                            let t = crate::types::parse_type_str(t);
                            Some((n.clone(), finite_of(inputs, &t, owner, depth - 1)?))
                        })
                        .collect::<Option<_>>()?,
                }),
            }
        }
        _ => None,
    }
}

/// The givens of law `t` whose types are finite, with those types.
pub(crate) fn finite_givens(inputs: &ProofLowerInputs, t: &LawTheorem) -> Vec<(String, Finite)> {
    let scope = inputs.symbol_table.fn_entry(t.fn_id).key.scope_str();
    t.quantifiers
        .iter()
        .filter_map(|q| {
            let QuantifierType::Plain(ty) = &q.binder_type;
            let ty = crate::types::parse_type_str(ty);
            Some((q.name.clone(), finite_of(inputs, &ty, scope, 4)?))
        })
        .collect()
}

/// The givens whose type is a `List`.
/// The givens of type Int: what an Int induction may count down.
pub(crate) fn int_givens(t: &LawTheorem) -> Vec<String> {
    t.quantifiers
        .iter()
        .filter(|q| {
            let QuantifierType::Plain(ty) = &q.binder_type;
            matches!(crate::types::parse_type_str(ty), crate::ast::Type::Int)
        })
        .map(|q| q.name.clone())
        .collect()
}

pub(crate) fn list_givens(t: &LawTheorem) -> Vec<String> {
    t.quantifiers
        .iter()
        .filter(|q| {
            let QuantifierType::Plain(ty) = &q.binder_type;
            matches!(crate::types::parse_type_str(ty), crate::ast::Type::List(_))
        })
        .map(|q| q.name.clone())
        .collect()
}

impl Env<'_> {
    /// Prove `lhs = rhs` by splitting every given in `split` into its
    /// values, then evaluating each case.
    pub(crate) fn prove_by_cases(
        &mut self,
        split: &[(String, Finite)],
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Proof, String> {
        let cases = split
            .iter()
            .fold(1usize, |n, (_, f)| n.saturating_mul(f.count()));
        if cases > MAX_CASES {
            return Err(format!("{cases} cases is more than a split may write"));
        }
        self.split(split, lhs, rhs, depth)
    }

    /// A proof that `true` and `false` are equal, when a hypothesis in
    /// scope says a term is one and the term evaluates to the other: the
    /// case cannot happen.
    fn refute_a_hypothesis(&mut self) -> Result<Option<Proof>, String> {
        for (name, e) in self.hyps.clone() {
            let Some(said) = term::bool_value(&e.rhs) else {
                continue;
            };
            // Evaluated without the hypotheses that state it, which would
            // only give their own value back.
            let saved = self.hyps.clone();
            let lhs = term::canon(&e.lhs);
            self.hyps.retain(|(_, h)| term::canon(&h.lhs) != lhs);
            let ev = self.whnf(&e.lhs);
            self.hyps = saved;
            let ev = ev?;
            if term::bool_value(ev.chain.cur()) != Some(!said) {
                continue;
            }
            let (value, to_value) = ev.chain.finish();
            return Ok(Some(Proof::Trans {
                terms: vec![value, term::canon(&e.lhs), term::canon(&e.rhs)],
                steps: vec![Proof::Symm(Box::new(to_value)), Proof::Hyp(name)],
            }));
        }
        Ok(None)
    }

    fn split(
        &mut self,
        split: &[(String, Finite)],
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Proof, String> {
        let Some(((var, ty), rest)) = split.split_first() else {
            if let Some(contradiction) = self.refute_a_hypothesis()? {
                return Ok(Proof::Absurd {
                    contradiction: Box::new(contradiction),
                    lhs: term::canon(lhs),
                    rhs: term::canon(rhs),
                });
            }
            return self.prove_by_evaluation(lhs, rhs, depth);
        };
        let mut cases = Vec::new();
        for value in ty.values() {
            let at = [(var.clone(), value)];
            let saved = self.hyps.clone();
            let scoped: Result<Vec<(String, Eqn)>, String> = saved
                .iter()
                .map(|(n, e)| {
                    Ok((
                        n.clone(),
                        Eqn::new(term::subst(&e.lhs, &at)?, term::subst(&e.rhs, &at)?),
                    ))
                })
                .collect();
            self.hyps = scoped?;
            let case = self.split(
                rest,
                &term::subst(lhs, &at)?,
                &term::subst(rhs, &at)?,
                depth,
            );
            self.hyps = saved;
            cases.push(case?);
        }
        Ok(Proof::Enum {
            var: var.clone(),
            lhs: term::canon(lhs),
            rhs: term::canon(rhs),
            cases,
        })
    }
}
