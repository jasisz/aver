//! Division by a divisor that is not a literal. `Int.div(a, k)` and
//! `Int.mod(a, k)` are `Result.Ok` of the Euclidean quotient and remainder
//! where `k` is not 0 (`int.div.ok`, `int.mod.ok`), which linear arithmetic
//! shows from the comparisons in scope or a cited law (`pow2(k) >= 1`);
//! where it also shows `k > 0`, linear arithmetic reads the quotient and the
//! remainder by the same rules as for a literal divisor
//! (`int.div_mod_recompose`, `int.mod_range`).

use crate::ast::BinOp;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Proof, WallRule};

use super::env::Env;

impl Env<'_> {
    /// One step of `Int.div(a, k)` or `Int.mod(a, k)` for a divisor `k`
    /// that is not a literal, to `Result.Ok` of the Euclidean quotient or
    /// remainder, where `k > 0` or, from the hypotheses, `k < 0` shows
    /// `k == 0` false.
    pub(super) fn division_ok_step(&mut self, cur: &Term) -> Option<(Proof, Term)> {
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return None;
        };
        let rule = match name.as_str() {
            "Int.div" => WallRule::DivOk,
            "Int.mod" => WallRule::ModOk,
            _ => return None,
        };
        let [a, k] = args.as_slice() else {
            return None;
        };
        if term::int_value(k).is_some() {
            return None;
        }
        let (a, k) = (canon(a), canon(k));
        let zero = || term::int(&0.into());
        let premise = match self.divisor_positive(&k) {
            Some(positive) => Proof::Rule {
                rule: WallRule::EqFalseOfGt,
                subst: vec![("a".to_string(), k.clone()), ("b".to_string(), zero())],
                premises: vec![positive],
            },
            None => {
                let negative = canon(&term::binop(BinOp::Lt, k.clone(), zero()));
                Proof::Rule {
                    rule: WallRule::EqFalseOfLt,
                    subst: vec![("a".to_string(), k.clone()), ("b".to_string(), zero())],
                    premises: vec![self.plain_linear(&negative, true)?],
                }
            }
        };
        let subst = vec![("a".to_string(), a), ("k".to_string(), k)];
        let (_, concl) = rule.instantiate(&subst)?;
        Some((
            Proof::Rule {
                rule,
                subst,
                premises: vec![premise],
            },
            concl.rhs,
        ))
    }

    /// `(k > 0) = true` for a divisor: by computation for a literal, by
    /// linear arithmetic otherwise, from the hypotheses alone where they
    /// do, else with the cited laws too, once per divisor and hypotheses (a
    /// divisor inside it is not read again).
    pub(super) fn divisor_positive(&mut self, k: &Term) -> Option<Proof> {
        if term::int_value(k).is_some() {
            return super::eval::literal_positive(k);
        }
        if self.naming_divisor {
            return None;
        }
        if let Some((_, _, p)) = self
            .divisors
            .iter()
            .find(|(d, hyps, _)| d == k && *hyps == self.hyps)
        {
            return p.clone();
        }
        let goal = canon(&term::binop(BinOp::Gt, k.clone(), term::int(&0.into())));
        let proof = match self.plain_linear(&goal, true) {
            Some(p) => Some(p),
            None => {
                self.naming_divisor = true;
                let p = self.linear_proof(&goal, true);
                self.naming_divisor = false;
                p
            }
        };
        if self.divisors.len() >= 64 {
            self.divisors.remove(0);
        }
        self.divisors
            .push((k.clone(), self.hyps.clone(), proof.clone()));
        proof
    }
}
