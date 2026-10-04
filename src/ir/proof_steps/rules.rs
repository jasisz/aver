//! The wall: the closed set of rules a step may apply, each an equation
//! schema over named binders with equational premises.
//!
//! The same table exists three times, on purpose: here (producers
//! instantiate it), in the Aver replayer (`tools/proof-kernel/`, which
//! re-derives every instance), and as the Lean lemmas the renderer names
//! (`codegen::lean::proof_steps`). Rule identifiers are stable strings;
//! changing a rule's meaning means a new identifier and a new
//! [`super::FORMAT_VERSION`].

use crate::ast::BinOp;
use crate::ir::hir::BuiltinIntrinsic;

use super::Eqn;
use super::term::{self, Term, binop, bool_and, bool_not, bool_or, boolean, var};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WallRule {
    // Bool connectives at a literal argument.
    AndTrueL,
    AndFalseL,
    AndTrueR,
    AndFalseR,
    OrTrueL,
    OrFalseL,
    OrTrueR,
    OrFalseR,
    NotTrue,
    NotFalse,
    // Projections of a true conjunction.
    AndElimL,
    AndElimR,
    // A decided Int comparison decides its complement.
    LeOfNotGt,
    LeFalseOfGt,
    GeOfNotLt,
    GeFalseOfLt,
    LtOfNotGe,
    LtFalseOfGe,
    GtOfNotLe,
    GtFalseOfLe,
    EqOfNotNe,
    EqFalseOfNe,
    NeOfNotEq,
    NeFalseOfEq,
    // Ring laws of Int.
    AddComm,
    MulComm,
    AddAssoc,
    MulAssoc,
    AddZero,
    ZeroAdd,
    MulOne,
    OneMul,
    SubZero,
    // Euclidean division by a positive divisor.
    DivModRecompose,
    DivRange,
}

impl WallRule {
    pub const ALL: [WallRule; 35] = [
        WallRule::AndTrueL,
        WallRule::AndFalseL,
        WallRule::AndTrueR,
        WallRule::AndFalseR,
        WallRule::OrTrueL,
        WallRule::OrFalseL,
        WallRule::OrTrueR,
        WallRule::OrFalseR,
        WallRule::NotTrue,
        WallRule::NotFalse,
        WallRule::AndElimL,
        WallRule::AndElimR,
        WallRule::LeOfNotGt,
        WallRule::LeFalseOfGt,
        WallRule::GeOfNotLt,
        WallRule::GeFalseOfLt,
        WallRule::LtOfNotGe,
        WallRule::LtFalseOfGe,
        WallRule::GtOfNotLe,
        WallRule::GtFalseOfLe,
        WallRule::EqOfNotNe,
        WallRule::EqFalseOfNe,
        WallRule::NeOfNotEq,
        WallRule::NeFalseOfEq,
        WallRule::AddComm,
        WallRule::MulComm,
        WallRule::AddAssoc,
        WallRule::MulAssoc,
        WallRule::AddZero,
        WallRule::ZeroAdd,
        WallRule::MulOne,
        WallRule::OneMul,
        WallRule::SubZero,
        WallRule::DivModRecompose,
        WallRule::DivRange,
    ];

    /// Stable identifier, shared with the replayer.
    pub fn id(self) -> &'static str {
        match self {
            WallRule::AndTrueL => "bool.and.true_l",
            WallRule::AndFalseL => "bool.and.false_l",
            WallRule::AndTrueR => "bool.and.true_r",
            WallRule::AndFalseR => "bool.and.false_r",
            WallRule::OrTrueL => "bool.or.true_l",
            WallRule::OrFalseL => "bool.or.false_l",
            WallRule::OrTrueR => "bool.or.true_r",
            WallRule::OrFalseR => "bool.or.false_r",
            WallRule::NotTrue => "bool.not.true",
            WallRule::NotFalse => "bool.not.false",
            WallRule::AndElimL => "bool.and.elim_l",
            WallRule::AndElimR => "bool.and.elim_r",
            WallRule::LeOfNotGt => "int.le.of_not_gt",
            WallRule::LeFalseOfGt => "int.le.false_of_gt",
            WallRule::GeOfNotLt => "int.ge.of_not_lt",
            WallRule::GeFalseOfLt => "int.ge.false_of_lt",
            WallRule::LtOfNotGe => "int.lt.of_not_ge",
            WallRule::LtFalseOfGe => "int.lt.false_of_ge",
            WallRule::GtOfNotLe => "int.gt.of_not_le",
            WallRule::GtFalseOfLe => "int.gt.false_of_le",
            WallRule::EqOfNotNe => "int.eq.of_not_ne",
            WallRule::EqFalseOfNe => "int.eq.false_of_ne",
            WallRule::NeOfNotEq => "int.ne.of_not_eq",
            WallRule::NeFalseOfEq => "int.ne.false_of_eq",
            WallRule::AddComm => "int.add_comm",
            WallRule::MulComm => "int.mul_comm",
            WallRule::AddAssoc => "int.add_assoc",
            WallRule::MulAssoc => "int.mul_assoc",
            WallRule::AddZero => "int.add_zero",
            WallRule::ZeroAdd => "int.zero_add",
            WallRule::MulOne => "int.mul_one",
            WallRule::OneMul => "int.one_mul",
            WallRule::SubZero => "int.sub_zero",
            WallRule::DivModRecompose => "int.div_mod_recompose",
            WallRule::DivRange => "int.div_range",
        }
    }

    pub fn from_id(id: &str) -> Option<WallRule> {
        WallRule::ALL.iter().copied().find(|r| r.id() == id)
    }

    /// Binder names, in the order a substitution lists them.
    pub fn binders(self) -> &'static [&'static str] {
        match self {
            WallRule::AndTrueL | WallRule::AndFalseL | WallRule::OrTrueL | WallRule::OrFalseL => {
                &["b"]
            }
            WallRule::AndTrueR | WallRule::AndFalseR | WallRule::OrTrueR | WallRule::OrFalseR => {
                &["a"]
            }
            WallRule::NotTrue | WallRule::NotFalse => &[],
            WallRule::AddZero
            | WallRule::ZeroAdd
            | WallRule::MulOne
            | WallRule::OneMul
            | WallRule::SubZero => &["a"],
            WallRule::AddAssoc | WallRule::MulAssoc => &["a", "b", "c"],
            WallRule::DivModRecompose => &["a", "k"],
            WallRule::DivRange => &["a", "k", "m", "n"],
            _ => &["a", "b"],
        }
    }

    /// The rule's premises and conclusion over its binders.
    pub fn schema(self) -> (Vec<Eqn>, Eqn) {
        let a = || var("a");
        let b = || var("b");
        let c = || var("c");
        let t = || boolean(true);
        let f = || boolean(false);
        let i = |n: i64| term::int(&n.into());
        let cmp = |op: BinOp, x: Term, y: Term| binop(op, x, y);
        let is = |x: Term, v: bool| Eqn::new(x, boolean(v));
        // Complement rules: premise `(a P b) = v`, conclusion `(a Q b) = w`.
        let compl = |p: BinOp, v: bool, q: BinOp, w: bool| {
            (vec![is(cmp(p, a(), b()), v)], is(cmp(q, a(), b()), w))
        };
        let div = |x: Term, k: Term| term::intrinsic(BuiltinIntrinsic::IntDivEuclid, vec![x, k]);
        let modu = |x: Term, k: Term| term::intrinsic(BuiltinIntrinsic::IntModEuclid, vec![x, k]);
        match self {
            WallRule::AndTrueL => (vec![], Eqn::new(bool_and(t(), b()), b())),
            WallRule::AndFalseL => (vec![], Eqn::new(bool_and(f(), b()), f())),
            WallRule::AndTrueR => (vec![], Eqn::new(bool_and(a(), t()), a())),
            WallRule::AndFalseR => (vec![], Eqn::new(bool_and(a(), f()), f())),
            WallRule::OrTrueL => (vec![], Eqn::new(bool_or(t(), b()), t())),
            WallRule::OrFalseL => (vec![], Eqn::new(bool_or(f(), b()), b())),
            WallRule::OrTrueR => (vec![], Eqn::new(bool_or(a(), t()), t())),
            WallRule::OrFalseR => (vec![], Eqn::new(bool_or(a(), f()), a())),
            WallRule::NotTrue => (vec![], Eqn::new(bool_not(t()), f())),
            WallRule::NotFalse => (vec![], Eqn::new(bool_not(f()), t())),
            WallRule::AndElimL => (vec![is(bool_and(a(), b()), true)], is(a(), true)),
            WallRule::AndElimR => (vec![is(bool_and(a(), b()), true)], is(b(), true)),
            WallRule::LeOfNotGt => compl(BinOp::Gt, false, BinOp::Lte, true),
            WallRule::LeFalseOfGt => compl(BinOp::Gt, true, BinOp::Lte, false),
            WallRule::GeOfNotLt => compl(BinOp::Lt, false, BinOp::Gte, true),
            WallRule::GeFalseOfLt => compl(BinOp::Lt, true, BinOp::Gte, false),
            WallRule::LtOfNotGe => compl(BinOp::Gte, false, BinOp::Lt, true),
            WallRule::LtFalseOfGe => compl(BinOp::Gte, true, BinOp::Lt, false),
            WallRule::GtOfNotLe => compl(BinOp::Lte, false, BinOp::Gt, true),
            WallRule::GtFalseOfLe => compl(BinOp::Lte, true, BinOp::Gt, false),
            WallRule::EqOfNotNe => compl(BinOp::Neq, false, BinOp::Eq, true),
            WallRule::EqFalseOfNe => compl(BinOp::Neq, true, BinOp::Eq, false),
            WallRule::NeOfNotEq => compl(BinOp::Eq, false, BinOp::Neq, true),
            WallRule::NeFalseOfEq => compl(BinOp::Eq, true, BinOp::Neq, false),
            WallRule::AddComm => (
                vec![],
                Eqn::new(binop(BinOp::Add, a(), b()), binop(BinOp::Add, b(), a())),
            ),
            WallRule::MulComm => (
                vec![],
                Eqn::new(binop(BinOp::Mul, a(), b()), binop(BinOp::Mul, b(), a())),
            ),
            WallRule::AddAssoc => (
                vec![],
                Eqn::new(
                    binop(BinOp::Add, binop(BinOp::Add, a(), b()), c()),
                    binop(BinOp::Add, a(), binop(BinOp::Add, b(), c())),
                ),
            ),
            WallRule::MulAssoc => (
                vec![],
                Eqn::new(
                    binop(BinOp::Mul, binop(BinOp::Mul, a(), b()), c()),
                    binop(BinOp::Mul, a(), binop(BinOp::Mul, b(), c())),
                ),
            ),
            WallRule::AddZero => (vec![], Eqn::new(binop(BinOp::Add, a(), i(0)), a())),
            WallRule::ZeroAdd => (vec![], Eqn::new(binop(BinOp::Add, i(0), a()), a())),
            WallRule::MulOne => (vec![], Eqn::new(binop(BinOp::Mul, a(), i(1)), a())),
            WallRule::OneMul => (vec![], Eqn::new(binop(BinOp::Mul, i(1), a()), a())),
            WallRule::SubZero => (vec![], Eqn::new(binop(BinOp::Sub, a(), i(0)), a())),
            WallRule::DivModRecompose => (
                vec![is(cmp(BinOp::Gt, var("k"), i(0)), true)],
                Eqn::new(
                    binop(
                        BinOp::Add,
                        binop(BinOp::Mul, div(a(), var("k")), var("k")),
                        modu(a(), var("k")),
                    ),
                    a(),
                ),
            ),
            WallRule::DivRange => (
                vec![
                    is(
                        bool_and(cmp(BinOp::Lte, i(0), a()), cmp(BinOp::Lt, a(), var("m"))),
                        true,
                    ),
                    is(
                        bool_and(
                            cmp(BinOp::Gt, var("k"), i(0)),
                            cmp(BinOp::Eq, var("m"), binop(BinOp::Mul, var("n"), var("k"))),
                        ),
                        true,
                    ),
                ],
                is(
                    bool_and(
                        cmp(BinOp::Lte, i(0), div(a(), var("k"))),
                        cmp(BinOp::Lt, div(a(), var("k")), var("n")),
                    ),
                    true,
                ),
            ),
        }
    }

    /// Premises and conclusion at a substitution; `None` when the
    /// substitution does not bind exactly the rule's binders.
    pub fn instantiate(self, subst: &[(String, Term)]) -> Option<(Vec<Eqn>, Eqn)> {
        let binders = self.binders();
        if subst.len() != binders.len() || subst.iter().zip(binders).any(|((k, _), b)| k != b) {
            return None;
        }
        let (premises, concl) = self.schema();
        let inst = |e: &Eqn| -> Option<Eqn> {
            Some(Eqn::new(
                term::subst(&e.lhs, subst).ok()?,
                term::subst(&e.rhs, subst).ok()?,
            ))
        };
        Some((
            premises.iter().map(inst).collect::<Option<Vec<_>>>()?,
            inst(&concl)?,
        ))
    }
}
