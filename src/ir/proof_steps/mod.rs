//! Proof steps: proofs as data, independent of any proof backend.
//!
//! `ProofIR` says *what* a law claims and which strategy family it belongs
//! to; this layer says *how* the claim follows, one rule application at a
//! time. A producer (a proof-lowering rung that recognised a law's shape)
//! writes a [`Proof`] tree; the Lean renderer turns it into an explicit
//! term checked by the Lean kernel, and the Aver replayer in
//! `tools/proof-kernel/` re-checks the very same data without Lean.
//!
//! Rules of the format (checked by review, enforced by the replayer):
//! - terms are Aver expressions ([`Term`]), never backend text;
//! - every step names its rule from a closed enumeration ([`WallRule`],
//!   [`Proof::Unfold`], [`Proof::Arm`], …) and writes out every
//!   instantiation, intermediate term and rewrite position;
//! - there are no backend lemma names, simp sets, heartbeats, and no step
//!   that relies on a backend unfolding a definition by itself: a definition
//!   is opened only by [`Proof::Unfold`] at an explicit arm.
//!
//! The serialised form ([`sexpr`]) is versioned by [`FORMAT_VERSION`].

pub mod check;
pub mod rules;
pub mod sexpr;
pub mod term;

pub use rules::WallRule;
pub use term::Term;

use crate::ir::identity::FnId;

/// Version of the step data. Bump on any change a replayer could observe.
pub const FORMAT_VERSION: u32 = 1;

/// An equation `lhs = rhs` between two terms.
#[derive(Debug, Clone, PartialEq)]
pub struct Eqn {
    pub lhs: Term,
    pub rhs: Term,
}

impl Eqn {
    pub fn new(lhs: Term, rhs: Term) -> Self {
        Self { lhs, rhs }
    }
}

/// An earlier, separately proved law a step may cite. The replayer takes
/// its statement as given (it is that law's own obligation); the citation
/// instantiates every given explicitly and proves every premise.
#[derive(Debug, Clone, PartialEq)]
pub struct LawRef {
    /// `fn.law` as written in the source, qualified by module when the
    /// law lives in a dependency (`Domain.Message.littleEndian.lowDigitStep`).
    pub key: String,
    pub givens: Vec<String>,
    /// The law's `when`, as one Bool term that must equal `true`.
    pub premise: Option<Term>,
    pub lhs: Term,
    pub rhs: Term,
}

/// A source definition an [`Proof::Unfold`] step opens. The body is the
/// function's single expression with its parameters as variables.
#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub fn_id: FnId,
    /// Canonical qualified name (`Domain.LockTime.reached`).
    pub name: String,
    pub params: Vec<String>,
    pub body: Term,
}

/// One proof step. Each variant proves exactly one equation, computable
/// from the variant's own data plus the definitions, cited laws and
/// hypotheses in scope; the equation is written out wherever the rule
/// alone does not determine it.
#[derive(Debug, Clone, PartialEq)]
pub enum Proof {
    /// `t = t`.
    Refl(Term),
    /// From `a = b`, `b = a`.
    Symm(Box<Proof>),
    /// `terms[0] = terms[n]` from `steps[i] : terms[i] = terms[i+1]`.
    /// Every intermediate term is written out.
    Trans { terms: Vec<Term>, steps: Vec<Proof> },
    /// From `a = b`, `ctx[a] = ctx[b]`. `ctx` holds exactly one hole
    /// ([`term::hole`]): the rewrite position.
    Congr { ctx: Term, inner: Box<Proof> },
    /// Equation `arm` of definition `fn_id` at explicit arguments.
    /// Arm 0 is the whole body: `f(args) = body[params := args]`. When the
    /// body is a `match`, arm `k ≥ 1` is `f(args) = arm_k[params := args,
    /// pattern vars := binders]` and `premise` proves
    /// `subject[params := args] = pattern_k[pattern vars := binders]`.
    Unfold {
        fn_id: FnId,
        arm: u32,
        args: Vec<Term>,
        binders: Vec<Term>,
        premise: Option<Box<Proof>>,
    },
    /// Arm `arm` (1-based) of the explicit `match` term `subject_match`:
    /// `match s { … } = arm_k[pattern vars := binders]`, with `premise`
    /// proving `s = pattern_k[pattern vars := binders]`.
    Arm {
        term: Term,
        arm: u32,
        binders: Vec<Term>,
        premise: Box<Proof>,
    },
    /// A record literal's field: `R(…, f = v, …).f = v`. `term` is the
    /// projection.
    Proj { term: Term },
    /// A hypothesis in scope, by name (`when` is the law's own premise).
    Hyp(String),
    /// A wall rule instance; `subst` binds every rule binder, `premises`
    /// prove the rule's premises in order.
    Rule {
        rule: WallRule,
        subst: Vec<(String, Term)>,
        premises: Vec<Proof>,
    },
    /// An earlier law instance; `subst` binds every given, `premise`
    /// proves the law's `when` at that instance.
    Law {
        law: String,
        subst: Vec<(String, Term)>,
        premise: Option<Box<Proof>>,
    },
    /// `lhs = rhs` for closed terms, decided by evaluation.
    Compute { lhs: Term, rhs: Term },
    /// Case split on a Bool term: `if_true` is checked with hypothesis
    /// `hyp : on = true`, `if_false` with `hyp : on = false`; both must
    /// prove the same equation.
    Cases {
        on: Term,
        hyp: String,
        if_true: Box<Proof>,
        if_false: Box<Proof>,
    },
}

/// The obligation a step script closes: a law's claim under its givens.
#[derive(Debug, Clone, PartialEq)]
pub struct Obligation {
    pub key: String,
    pub givens: Vec<String>,
    pub premise: Option<Term>,
    pub lhs: Term,
    pub rhs: Term,
}

/// Everything a checker needs: the obligation, the proof, and the
/// definitions and laws the proof refers to.
#[derive(Debug, Clone, PartialEq)]
pub struct Script {
    pub obligation: Obligation,
    pub defs: Vec<Def>,
    pub laws: Vec<LawRef>,
    pub proof: Proof,
}

impl Script {
    pub fn def(&self, fn_id: FnId) -> Option<&Def> {
        self.defs.iter().find(|d| d.fn_id == fn_id)
    }
    pub fn law(&self, key: &str) -> Option<&LawRef> {
        self.laws.iter().find(|l| l.key == key)
    }
}

impl Proof {
    /// Number of nodes, for reports.
    pub fn size(&self) -> usize {
        1 + match self {
            Proof::Refl(_) | Proof::Proj { .. } | Proof::Hyp(_) | Proof::Compute { .. } => 0,
            Proof::Symm(p) => p.size(),
            Proof::Trans { steps, .. } => steps.iter().map(Proof::size).sum(),
            Proof::Congr { inner, .. } => inner.size(),
            Proof::Unfold { premise, .. } => premise.as_ref().map_or(0, |p| p.size()),
            Proof::Arm { premise, .. } => premise.size(),
            Proof::Rule { premises, .. } => premises.iter().map(Proof::size).sum(),
            Proof::Law { premise, .. } => premise.as_ref().map_or(0, |p| p.size()),
            Proof::Cases {
                if_true, if_false, ..
            } => if_true.size() + if_false.size(),
        }
    }
}

#[cfg(test)]
mod tests;
