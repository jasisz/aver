//! Building rewrite chains: a start term and steps that each prove the
//! previous term equal to the next.

use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof};

#[derive(Debug, Clone)]
pub(crate) struct Chain {
    terms: Vec<Term>,
    steps: Vec<Proof>,
}

impl Chain {
    pub(crate) fn new(start: &Term) -> Self {
        Self {
            terms: vec![canon(start)],
            steps: Vec::new(),
        }
    }

    pub(crate) fn cur(&self) -> &Term {
        self.terms.last().expect("a chain has a start")
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    /// Record `proof : cur = to`.
    pub(crate) fn push(&mut self, proof: Proof, to: Term) {
        match proof {
            Proof::Trans { terms, steps } => {
                for (t, s) in terms.into_iter().skip(1).zip(steps) {
                    self.terms.push(canon(&t));
                    self.steps.push(s);
                }
            }
            proof => {
                self.terms.push(canon(&to));
                self.steps.push(proof);
            }
        }
    }

    /// Rewrite the subterm at `path` of the current term with `proof : sub = to`.
    pub(crate) fn push_at(&mut self, path: &[usize], proof: Proof, to: &Term) {
        if path.is_empty() {
            self.push(proof, to.clone());
            return;
        }
        let cur = self.cur().clone();
        let ctx = term::context_at(&cur, path);
        let new = term::plug(&ctx, to);
        self.push(
            Proof::Congr {
                ctx,
                inner: Box::new(proof),
            },
            new,
        );
    }

    /// The end term and one proof of `start = end`.
    pub(crate) fn finish(self) -> (Term, Proof) {
        let end = self.cur().clone();
        let proof = match self.steps.len() {
            0 => Proof::Refl(end.clone()),
            1 => self.steps.into_iter().next().expect("one step"),
            _ => Proof::Trans {
                terms: self.terms,
                steps: self.steps,
            },
        };
        (end, proof)
    }
}

/// `a = c` from `a = b` and `c = b`.
pub(crate) fn meet(left: Chain, right: Chain) -> Proof {
    let (_, back) = right.clone().finish();
    let mut out = left;
    if right.is_empty() {
        return out.finish().1;
    }
    let mut rterms = right.terms.clone();
    rterms.reverse();
    let rhs_start = rterms.last().cloned().expect("a chain has a start");
    out.push(Proof::Symm(Box::new(back)), rhs_start);
    out.finish().1
}

pub(crate) fn eqn_true(t: &Term) -> Eqn {
    Eqn::new(canon(t), term::boolean(true))
}
