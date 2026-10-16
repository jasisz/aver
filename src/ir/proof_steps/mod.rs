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
//!   is opened only by [`Proof::Unfold`] at an explicit arm, and a
//!   module-level binding only by [`Proof::UnfoldConst`].
//!
//! The serialised form ([`sexpr`]) is versioned by [`FORMAT_VERSION`].

pub mod claim;
pub mod facts;
pub mod induct;
pub mod kernel_builtins;
pub mod linear;
pub mod read;
pub mod ring;
pub mod rules;
pub mod sexpr;
pub mod show;
pub mod term;
pub mod trust;

pub use rules::WallRule;
pub use term::Term;

use crate::ir::identity::FnId;

/// Version of the step data. Bump on any change a replayer could observe.
pub const FORMAT_VERSION: u32 = 16;

/// The types a script declares for the kernel's type checks: every sum
/// (each constructor with its field types) and every record (each field
/// with its type) that a type in the script mentions, transitively.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Decls {
    pub sums: Vec<SumDecl>,
    pub recs: Vec<RecDecl>,
}

/// A declared sum: its name as the data writes it, whether `==` between
/// two of its values is equality
/// ([`crate::ir::SymbolTable::beq_is_equality`]), and each constructor with
/// its field types.
#[derive(Debug, Clone, PartialEq)]
pub struct SumDecl {
    pub name: String,
    pub eq: bool,
    pub ctors: Vec<(crate::ir::hir::ResolvedCtor, Vec<crate::ast::Type>)>,
}

/// A declared record, as [`SumDecl`]: each field with its type.
#[derive(Debug, Clone, PartialEq)]
pub struct RecDecl {
    pub name: String,
    pub eq: bool,
    pub fields: Vec<(String, crate::ast::Type)>,
}

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
/// its statement as given (it is that law's own obligation), except for a
/// builtin fact, which carries its proof and is checked first; the
/// citation instantiates every given explicitly and proves every premise.
#[derive(Debug, Clone, PartialEq)]
pub struct LawRef {
    /// `fn.law` as written in the source, qualified by module when the
    /// law lives in a dependency (`Domain.Message.littleEndian.lowDigitStep`).
    pub key: String,
    pub givens: Vec<String>,
    /// The type of each given, by name; a given without one is written
    /// as a hole (a builtin fact's list given, which holds anything).
    pub types: Vec<(String, crate::ast::Type)>,
    /// What the law assumes, as one Bool term that must equal `true`: a
    /// line for each given of a refined type, stating what the type holds
    /// of it, then the law's `when`, joined with `Bool.and`.
    pub premise: Option<Term>,
    /// The leading lines of `premise` that are about the givens of refined
    /// types ([`own_when`]), in order.
    pub refined: Vec<Term>,
    pub lhs: Term,
    pub rhs: Term,
    /// For a builtin fact ([`facts`]), its own script: the checkers check
    /// it, and that it states this law, before any step may cite it.
    pub fact: Option<Box<Script>>,
}

/// A source definition an [`Proof::Unfold`] step opens: the function's
/// local bindings, in source order, then its final expression, with its
/// parameters as variables.
#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub fn_id: FnId,
    /// Canonical qualified name (`Domain.LockTime.reached`).
    pub name: String,
    pub params: Vec<String>,
    /// The type of each parameter, and of the result: the kernel types a
    /// call by them.
    pub param_types: Vec<crate::ast::Type>,
    pub ret: crate::ast::Type,
    /// `name = value` bindings before the final expression. Each value may
    /// read the parameters and the bindings before it.
    pub lets: Vec<(String, Term)>,
    pub body: Term,
}

impl Def {
    /// The substitution that opens the definition at `args`: each parameter
    /// to its argument, then each binding, in order, to its value with
    /// everything before it already substituted. A later name shadows an
    /// earlier one, so it comes first.
    pub fn outer(&self, args: &[Term]) -> Result<Vec<(String, Term)>, String> {
        if self.params.len() != args.len() {
            return Err(format!(
                "{} takes {} arguments",
                self.name,
                self.params.len()
            ));
        }
        let mut map: Vec<(String, Term)> = self
            .params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .rev()
            .collect();
        for (name, value) in &self.lets {
            let v = term::subst(value, &map)?;
            map.insert(0, (name.clone(), v));
        }
        Ok(map)
    }
}

/// A module-level binding (`base = 40` outside any fn) an
/// [`Proof::UnfoldConst`] step opens. A term reads it as the variable
/// `name`, which always has a dot: `Lib.base` for a dependency's binding,
/// `.base` for the entry module's own. Two modules' `base` stay apart, and
/// so do a binding and a local spelled like it: the language lets a given,
/// a parameter or a name a pattern binds take a module-level binding's
/// name, and none of them can have a dot.
#[derive(Debug, Clone, PartialEq)]
pub struct Const {
    pub name: String,
    pub ty: crate::ast::Type,
    pub value: Term,
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
    /// The value of a module-level binding: `name = value`, a constant
    /// with no parameters and no arms.
    UnfoldConst { name: String },
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
    /// A list literal with at least one element is its first element in
    /// front of the rest: `[x, …rest] = List.prepend(x, […rest])`. `list`
    /// is the literal.
    Cell { list: Term },
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
    /// Case split on any term `on`: one case per constructor of its type,
    /// in declaration order, `[]` before `[h, ..t]` for a list, each
    /// checked with hypothesis `hyp : on = C_i(binders)`, its binders
    /// fresh. Or, with literal cases of its type and a last one, one case
    /// per literal `k` under `hyp : on = k`, the last under `(on == k) =
    /// false` for every literal, each named by one of its binders (see
    /// [`split_hyps`]). Every case proves the same equation, which
    /// mentions no binder.
    Split {
        on: Term,
        hyp: String,
        cases: Vec<SplitCase>,
    },
    /// A cut: `proof` proves `fact = true` under the hypotheses in scope,
    /// and `body` is checked with hypothesis `name : fact = true` added;
    /// the step proves what `body` proves. The ordered `because` lines of
    /// a law are proved this way, each with the earlier ones in scope.
    Have {
        name: String,
        fact: Term,
        proof: Box<Proof>,
        body: Box<Proof>,
    },
    /// Any equation, from a proof that `true` and `false` are equal: the
    /// case the step is in cannot happen.
    Absurd {
        contradiction: Box<Proof>,
        lhs: Term,
        rhs: Term,
    },
    /// Induction following the recursion of `fn_id`, which the claim
    /// `lhs = rhs` applies to `args`: one case per arm of its `match`, in
    /// order (see [`induct`]). The hypotheses named in `carried` stay in
    /// scope at each case's pattern, and a recursive call's hypothesis
    /// holds once they are proved at the call's arguments; any other
    /// hypothesis that mentions what the induction varies is out of scope.
    Induct {
        fn_id: FnId,
        args: Vec<Term>,
        lhs: Term,
        rhs: Term,
        carried: Vec<String>,
        cases: Vec<InductCase>,
    },
    /// Induction on a given of list type, apart from any function's
    /// recursion, for every value of the givens in `general`: `nil` proves
    /// the claim `lhs = rhs` at `var = []`, and `cons` proves it at
    /// `var = List.prepend(head, tail)` under one hypothesis per entry of
    /// `ihs`, the claim at `var = tail` and at that entry's values of
    /// `general`. The other givens stay fixed. The hypotheses named in
    /// `carried` stay in scope in both cases at the case's value of `var`,
    /// and an entry's hypothesis holds once they are proved at its values
    /// (an entry's `carry`); any other hypothesis that mentions `var` or a
    /// given in `general` is out of scope.
    InductList {
        var: String,
        lhs: Term,
        rhs: Term,
        carried: Vec<String>,
        nil: Box<Proof>,
        head: String,
        tail: String,
        general: Vec<String>,
        ihs: Vec<IhAt>,
        cons: Box<Proof>,
    },
    /// Case split on a list variable (a given of list type, or the tail an
    /// enclosing split named): `nil` proves the claim `lhs = rhs` with
    /// `var` replaced by `[]`, `cons` with `var` replaced by
    /// `List.prepend(head, tail)`, the hypotheses in scope replaced the same
    /// way. `head` and `tail` are fresh for everything in scope, and `tail`
    /// may be split in turn.
    ListCases {
        var: String,
        lhs: Term,
        rhs: Term,
        nil: Box<Proof>,
        head: String,
        tail: String,
        cons: Box<Proof>,
    },
    /// `goal = value` for an Int comparison `goal`: its opposite and the
    /// hypotheses `hyps`, each read as `p >= 0`, then the rows `cuts`
    /// rounds from them (each over the comparisons and the cuts before
    /// it), weighted by `weights` (the opposite first, the cuts last), add
    /// up to a negative constant (see [`linear`]).
    Linear {
        goal: Term,
        value: bool,
        hyps: Vec<String>,
        weights: Vec<num_bigint::BigInt>,
        cuts: Vec<linear::Cut>,
    },
    /// `lhs = rhs` for two Int terms that are the same polynomial over
    /// their atoms (see [`ring`]).
    Ring { lhs: Term, rhs: Term },
    /// Case split on every value of a given of finite type: `cases[i]`
    /// proves `lhs = rhs` with `var` replaced by value `i` (also in the
    /// hypotheses in scope), in the order [`Finite::values`] lists them.
    Enum {
        var: String,
        lhs: Term,
        rhs: Term,
        cases: Vec<Proof>,
    },
}

/// A type with finitely many values, every one of which a step can write
/// out: `Bool`, a sum type whose variants carry nothing, and records and
/// tuples of those.
#[derive(Debug, Clone, PartialEq)]
pub enum Finite {
    Bool,
    /// The variants in declaration order.
    Sum(Vec<crate::ir::hir::ResolvedCtor>),
    Record {
        type_id: crate::ir::identity::TypeId,
        type_name: String,
        fields: Vec<(String, Finite)>,
    },
    Tuple(Vec<Finite>),
}

impl Finite {
    /// Every value of the type, in the order a case split lists them:
    /// `false` before `true`, variants in declaration order, and records
    /// and tuples with their first part varying slowest.
    pub fn values(&self) -> Vec<Term> {
        use crate::ast::Spanned;
        use crate::ir::hir::ResolvedExpr;
        let product = |parts: Vec<Vec<Term>>| -> Vec<Vec<Term>> {
            parts.into_iter().fold(vec![Vec::new()], |acc, part| {
                acc.iter()
                    .flat_map(|prefix| {
                        part.iter().map(move |v| {
                            let mut next = prefix.clone();
                            next.push(v.clone());
                            next
                        })
                    })
                    .collect()
            })
        };
        match self {
            Finite::Bool => vec![term::boolean(false), term::boolean(true)],
            Finite::Sum(ctors) => ctors
                .iter()
                .map(|c| Spanned::bare(ResolvedExpr::Ctor(c.clone(), Vec::new())))
                .collect(),
            Finite::Record {
                type_id,
                type_name,
                fields,
            } => product(fields.iter().map(|(_, f)| f.values()).collect())
                .into_iter()
                .map(|vs| {
                    Spanned::bare(ResolvedExpr::RecordCreate {
                        type_id: Some(*type_id),
                        type_name: type_name.clone(),
                        fields: fields.iter().map(|(n, _)| n.clone()).zip(vs).collect(),
                    })
                })
                .collect(),
            Finite::Tuple(parts) => product(parts.iter().map(Finite::values).collect())
                .into_iter()
                .map(|vs| Spanned::bare(ResolvedExpr::Tuple(vs)))
                .collect(),
        }
    }

    /// How many values the type has, without listing them.
    pub fn count(&self) -> usize {
        match self {
            Finite::Bool => 2,
            Finite::Sum(ctors) => ctors.len(),
            Finite::Record { fields, .. } => fields
                .iter()
                .fold(1usize, |n, (_, f)| n.saturating_mul(f.count())),
            Finite::Tuple(parts) => parts
                .iter()
                .fold(1usize, |n, f| n.saturating_mul(f.count())),
        }
    }
}

/// One induction hypothesis at chosen values of the generalised givens: its
/// name, the value of each generalised given (in the order they vary), and
/// the proofs of the carried hypotheses there (in the order of `carried`).
/// A [`Proof::InductList`] step lists them; an [`InductCase`] names further
/// ones at the part a recursive call recurses on.
#[derive(Debug, Clone, PartialEq)]
pub struct IhAt {
    pub name: String,
    pub at: Vec<Term>,
    pub carry: Vec<Proof>,
}

/// One case of an [`Proof::Induct`] step: fresh names for the arm's
/// pattern variables, one hypothesis name per recursive call in the arm
/// (in the order [`induct::self_calls`] lists them; `_` for a call whose
/// hypothesis the case does without), for each the proofs of the carried
/// hypotheses at that call's arguments, further hypotheses each at the part
/// one of those calls recurses on and other values of the varied givens
/// (the call's place and the values, in the order the givens vary), and
/// the proof of the claim at the arm's pattern under all of them.
#[derive(Debug, Clone, PartialEq)]
pub struct InductCase {
    pub binders: Vec<String>,
    pub ihs: Vec<String>,
    pub carry: Vec<Vec<Proof>>,
    pub more: Vec<(usize, IhAt)>,
    pub proof: Proof,
}

/// The constructor a case of a [`Proof::Split`] is about.
#[derive(Debug, Clone, PartialEq)]
pub enum SplitCtor {
    /// `[]`.
    Nil,
    /// `[h, ..t]`, two binders.
    Cons,
    /// A constructor of a sum type, one binder per field.
    Ctor(crate::ir::hir::ResolvedCtor),
    /// A literal arm: the subject is the literal, no binders.
    Lit(crate::ast::Literal),
    /// The catch-all arm after literal arms: the subject is none of them,
    /// one binder per literal naming that hypothesis.
    Other,
}

/// One case of a [`Proof::Split`]: the constructor, fresh names for its
/// fields, and the proof under `on = C(binders)`.
#[derive(Debug, Clone, PartialEq)]
pub struct SplitCase {
    pub ctor: SplitCtor,
    pub binders: Vec<String>,
    pub proof: Proof,
}

impl SplitCase {
    /// The constructor applied to the binders, or the literal: what the
    /// case's hypothesis says the split term is. `None` for the catch-all
    /// case of a literal split.
    pub fn value(&self) -> Option<Term> {
        use crate::ast::Spanned;
        use crate::ir::hir::ResolvedExpr;
        let vars: Vec<Term> = self.binders.iter().map(|b| term::var(b)).collect();
        Some(match &self.ctor {
            SplitCtor::Nil => term::nil(),
            SplitCtor::Cons => term::builtin("List.prepend", vars, None),
            SplitCtor::Ctor(c) => Spanned::bare(ResolvedExpr::Ctor(c.clone(), vars)),
            SplitCtor::Lit(l) => Spanned::bare(ResolvedExpr::Literal(l.clone())),
            SplitCtor::Other => return None,
        })
    }
}

/// The hypotheses each case of a split on `on` is checked under: `hyp : on
/// = value`, or for the catch-all case of a literal split, `(on == k) =
/// false` for each literal case, named by its binders in order.
pub fn split_hyps(on: &Term, hyp: &str, cases: &[SplitCase]) -> Vec<Vec<(String, Eqn)>> {
    let lits: Vec<Term> = cases
        .iter()
        .filter(|c| matches!(c.ctor, SplitCtor::Lit(_)))
        .filter_map(SplitCase::value)
        .collect();
    cases
        .iter()
        .map(|c| match c.value() {
            Some(v) => vec![(hyp.to_string(), Eqn::new(on.clone(), v))],
            None => c
                .binders
                .iter()
                .zip(&lits)
                .map(|(n, k)| (n.clone(), unequal(on, k)))
                .collect(),
        })
        .collect()
}

/// `(on == k) = false`.
pub fn unequal(on: &Term, k: &Term) -> Eqn {
    Eqn::new(
        term::binop(crate::ast::BinOp::Eq, on.clone(), k.clone()),
        term::boolean(false),
    )
}

/// A `premise` without its `refined` leading lines: the law's own `when`.
/// A value of a refined type satisfies what its type holds (the smart
/// constructor checked it), but a step term may write the record with any
/// value in its field, so a script states it as a premise: a hypothesis of
/// the law's own proof, and something a citation proves of the value it
/// puts for the given. Lean states such a given by a type that carries the
/// proof, so its theorem has only the `when`.
pub fn own_when<'a>(premise: &'a Option<Term>, refined: &[Term]) -> Option<&'a Term> {
    let mut at = premise.as_ref()?;
    for (k, line) in refined.iter().enumerate() {
        // The last refined line, with no `when` after it.
        if k + 1 == refined.len() && at == line {
            return None;
        }
        match &at.node {
            crate::ir::hir::ResolvedExpr::Call(_, args) if args.len() == 2 && args[0] == *line => {
                at = &args[1]
            }
            _ => return Some(at),
        }
    }
    Some(at)
}

impl LawRef {
    /// The law's own `when` ([`own_when`]).
    pub fn when(&self) -> Option<&Term> {
        own_when(&self.premise, &self.refined)
    }
}

impl Obligation {
    /// The law's own `when` ([`own_when`]).
    pub fn when(&self) -> Option<&Term> {
        own_when(&self.premise, &self.refined)
    }
}

/// The obligation a step script closes: a law's claim under its givens.
#[derive(Debug, Clone, PartialEq)]
pub struct Obligation {
    pub key: String,
    pub givens: Vec<String>,
    /// The type of each given, by name; a given without one is written
    /// as a hole.
    pub types: Vec<(String, crate::ast::Type)>,
    /// The givens of a finite type, with that type: what a
    /// [`Proof::Enum`] split may enumerate.
    pub finite: Vec<(String, Finite)>,
    /// The givens of a list type: what a [`Proof::InductList`] step may
    /// induct on.
    pub lists: Vec<String>,
    /// The givens of type Int: what a [`Proof::Induct`] along a function
    /// that counts an Int toward zero may induct on.
    pub ints: Vec<String>,
    /// What the claim assumes, as [`LawRef::premise`] states it. An
    /// obligation of a `because` chain also assumes the reasons before it,
    /// between the lines about the givens and the `when`.
    pub premise: Option<Term>,
    /// As [`LawRef::refined`].
    pub refined: Vec<Term>,
    pub lhs: Term,
    pub rhs: Term,
}

/// Everything a checker needs: the obligation, the proof, and the
/// definitions and laws the proof refers to.
#[derive(Debug, Clone, PartialEq)]
pub struct Script {
    pub obligation: Obligation,
    pub defs: Vec<Def>,
    pub consts: Vec<Const>,
    pub laws: Vec<LawRef>,
    /// The sums and records the script's types mention, with their field
    /// types: what the kernel types constructors and fields by.
    pub decls: Decls,
    /// The signature of every function a term calls without the script
    /// opening it: what the kernel types such a call by.
    pub sigs: Vec<(FnId, Vec<crate::ast::Type>, crate::ast::Type)>,
    pub proof: Proof,
    /// The project proof plan that wrote `proof`, when a law named one with
    /// `by`: recorded in the script for audit, never read by a checker.
    pub plan: Option<PlanUse>,
}

/// Which proof plan wrote a script's proof: the plan's full name and the
/// sha256 of its plans module's source and the project modules it depends
/// on (hex). Audit only; the kernel checks the proof the same way whoever
/// wrote it.
#[derive(Debug, Clone, PartialEq)]
pub struct PlanUse {
    pub name: String,
    pub hash: String,
}

impl PlanUse {
    /// The first twelve hex digits of the hash, for reports.
    pub fn short_hash(&self) -> &str {
        &self.hash[..self.hash.len().min(12)]
    }

    /// How a report names a proof this plan wrote:
    /// `proof by Plans.Stack.openByLength (b55bd85f06be), 228 steps`.
    pub fn describe(&self, steps: usize) -> String {
        format!(
            "proof by {} ({}), {steps} steps",
            self.name,
            self.short_hash()
        )
    }

    /// [`Self::describe`] read back from the comment line a step script
    /// starts with, when a plan wrote it.
    pub fn describe_comment(script_text: &str) -> Option<String> {
        let line = script_text.lines().next()?.strip_prefix("; proof by ")?;
        let (name, rest) = line.split_once(" sha256:")?;
        let (hash, steps) = rest.split_once(", ")?;
        let steps: usize = steps.strip_suffix(" steps")?.parse().ok()?;
        Some(
            PlanUse {
                name: name.to_string(),
                hash: hash.to_string(),
            }
            .describe(steps),
        )
    }
}

impl Script {
    pub fn def(&self, fn_id: FnId) -> Option<&Def> {
        self.defs.iter().find(|d| d.fn_id == fn_id)
    }
    pub fn constant(&self, name: &str) -> Option<&Const> {
        self.consts.iter().find(|c| c.name == name)
    }
    pub fn law(&self, key: &str) -> Option<&LawRef> {
        self.laws.iter().find(|l| l.key == key)
    }
}

impl Proof {
    /// Every term the proof writes, in any step, handed to `f`.
    pub fn each_term(&self, f: &mut dyn FnMut(&Term)) {
        let all = |ts: &[Term], f: &mut dyn FnMut(&Term)| ts.iter().for_each(f);
        match self {
            Proof::Refl(t) | Proof::Proj { term: t } | Proof::Cell { list: t } => f(t),
            Proof::Hyp(_) | Proof::UnfoldConst { .. } => {}
            Proof::Symm(p) => p.each_term(f),
            Proof::Trans { terms, steps } => {
                all(terms, f);
                steps.iter().for_each(|p| p.each_term(f));
            }
            Proof::Congr { ctx, inner } => {
                f(ctx);
                inner.each_term(f);
            }
            Proof::Unfold {
                args,
                binders,
                premise,
                ..
            } => {
                all(args, f);
                all(binders, f);
                premise.iter().for_each(|p| p.each_term(f));
            }
            Proof::Arm {
                term,
                binders,
                premise,
                ..
            } => {
                f(term);
                all(binders, f);
                premise.each_term(f);
            }
            Proof::Rule {
                subst, premises, ..
            } => {
                subst.iter().for_each(|(_, t)| f(t));
                premises.iter().for_each(|p| p.each_term(f));
            }
            Proof::Law { subst, premise, .. } => {
                subst.iter().for_each(|(_, t)| f(t));
                premise.iter().for_each(|p| p.each_term(f));
            }
            Proof::Compute { lhs, rhs } | Proof::Ring { lhs, rhs } => {
                f(lhs);
                f(rhs);
            }
            Proof::Cases {
                on,
                if_true,
                if_false,
                ..
            } => {
                f(on);
                if_true.each_term(f);
                if_false.each_term(f);
            }
            Proof::Split { on, cases, .. } => {
                f(on);
                cases.iter().for_each(|c| c.proof.each_term(f));
            }
            Proof::Have {
                fact, proof, body, ..
            } => {
                f(fact);
                proof.each_term(f);
                body.each_term(f);
            }
            Proof::Absurd {
                contradiction,
                lhs,
                rhs,
            } => {
                contradiction.each_term(f);
                f(lhs);
                f(rhs);
            }
            Proof::Induct {
                args,
                lhs,
                rhs,
                cases,
                ..
            } => {
                all(args, f);
                f(lhs);
                f(rhs);
                for c in cases {
                    c.carry.iter().flatten().for_each(|p| p.each_term(f));
                    for (_, ih) in &c.more {
                        all(&ih.at, f);
                        ih.carry.iter().for_each(|p| p.each_term(f));
                    }
                    c.proof.each_term(f);
                }
            }
            Proof::InductList {
                lhs,
                rhs,
                nil,
                ihs,
                cons,
                ..
            } => {
                f(lhs);
                f(rhs);
                nil.each_term(f);
                for ih in ihs {
                    all(&ih.at, f);
                    ih.carry.iter().for_each(|p| p.each_term(f));
                }
                cons.each_term(f);
            }
            Proof::ListCases {
                lhs,
                rhs,
                nil,
                cons,
                ..
            } => {
                f(lhs);
                f(rhs);
                nil.each_term(f);
                cons.each_term(f);
            }
            Proof::Linear { goal, .. } => f(goal),
            Proof::Enum {
                lhs, rhs, cases, ..
            } => {
                f(lhs);
                f(rhs);
                cases.iter().for_each(|p| p.each_term(f));
            }
        }
    }

    /// Number of nodes, for reports.
    pub fn size(&self) -> usize {
        1 + match self {
            Proof::Refl(_)
            | Proof::Proj { .. }
            | Proof::Cell { .. }
            | Proof::Hyp(_)
            | Proof::Compute { .. }
            | Proof::UnfoldConst { .. }
            | Proof::Ring { .. }
            | Proof::Linear { .. } => 0,
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
            Proof::Split { cases, .. } => cases.iter().map(|c| c.proof.size()).sum(),
            Proof::Have { proof, body, .. } => proof.size() + body.size(),
            Proof::Enum { cases, .. } => cases.iter().map(Proof::size).sum(),
            Proof::Absurd { contradiction, .. } => contradiction.size(),
            Proof::Induct { cases, .. } => cases
                .iter()
                .map(|c| c.proof.size() + c.carry.iter().flatten().map(Proof::size).sum::<usize>())
                .sum(),
            Proof::InductList { nil, cons, .. } | Proof::ListCases { nil, cons, .. } => {
                nil.size() + cons.size()
            }
        }
    }
}

#[cfg(test)]
mod tests;
