//! Lean rendering of proof steps (`crate::ir::proof_steps`).
//!
//! A step script becomes one explicit term: `Eq.trans`, `Eq.symm`,
//! `congrArg`, and an application of one lemma per rule. The Lean-specific
//! parts are exactly two tables: wall rule → `AverSteps.*` lemma name (the
//! lemmas live in the prelude, proved once), and law → theorem name.
//! Definitions are opened through `__aver_unfold_<n>` lemmas stated next
//! to the law, one per (function, arm), never through Lean's own equation
//! numbering. Every intermediate equation is stated with `show`, so the
//! kernel checks the data and elaboration never searches.

use std::collections::BTreeMap;

use crate::ast::{BinOp, Spanned, VerifyKind};
use crate::codegen::CodegenContext;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedPattern};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::check::{Hyps, arm_equation, conclusion};
use crate::ir::proof_steps::term::{self, HOLE, Term};
use crate::ir::proof_steps::{Eqn, Proof, Script, WallRule};

use super::expr::{aver_name_to_lean, emit_expr};
use super::types::type_to_lean;

/// The prelude section the rendered terms rely on. Each lemma is the Lean
/// statement of one wall rule (`crate::ir::proof_steps::rules`).
pub(crate) const LEAN_PRELUDE_AVER_STEPS: &str = r#"namespace AverSteps
theorem bool_cases {P : Prop} (c : Bool) (hf : c = false → P) (ht : c = true → P) : P := by
  cases c
  · exact hf rfl
  · exact ht rfl
theorem and_true_l (b : Bool) : (true && b) = b := rfl
theorem and_false_l (b : Bool) : (false && b) = false := rfl
theorem and_true_r (a : Bool) : (a && true) = a := by cases a <;> rfl
theorem and_false_r (a : Bool) : (a && false) = false := by cases a <;> rfl
theorem or_true_l (b : Bool) : (true || b) = true := rfl
theorem or_false_l (b : Bool) : (false || b) = b := rfl
theorem or_true_r (a : Bool) : (a || true) = true := by cases a <;> rfl
theorem or_false_r (a : Bool) : (a || false) = a := by cases a <;> rfl
theorem not_true : (!true) = false := rfl
theorem not_false : (!false) = true := rfl
theorem and_elim_l (a b : Bool) (h : (a && b) = true) : a = true := by
  cases a <;> cases b <;> first | rfl | exact absurd h (by decide)
theorem and_elim_r (a b : Bool) (h : (a && b) = true) : b = true := by
  cases a <;> cases b <;> first | rfl | exact absurd h (by decide)
theorem le_of_not_gt (a b : Int) (h : decide (a > b) = false) : decide (a <= b) = true :=
  decide_eq_true (Int.not_lt.mp (of_decide_eq_false h))
theorem le_false_of_gt (a b : Int) (h : decide (a > b) = true) : decide (a <= b) = false :=
  decide_eq_false (Int.not_le.mpr (of_decide_eq_true h))
theorem ge_of_not_lt (a b : Int) (h : decide (a < b) = false) : decide (a >= b) = true :=
  decide_eq_true (Int.not_lt.mp (of_decide_eq_false h))
theorem ge_false_of_lt (a b : Int) (h : decide (a < b) = true) : decide (a >= b) = false :=
  decide_eq_false (Int.not_le.mpr (of_decide_eq_true h))
theorem lt_of_not_ge (a b : Int) (h : decide (a >= b) = false) : decide (a < b) = true :=
  decide_eq_true (Int.not_le.mp (of_decide_eq_false h))
theorem lt_false_of_ge (a b : Int) (h : decide (a >= b) = true) : decide (a < b) = false :=
  decide_eq_false (Int.not_lt.mpr (of_decide_eq_true h))
theorem gt_of_not_le (a b : Int) (h : decide (a <= b) = false) : decide (a > b) = true :=
  decide_eq_true (Int.not_le.mp (of_decide_eq_false h))
theorem gt_false_of_le (a b : Int) (h : decide (a <= b) = true) : decide (a > b) = false :=
  decide_eq_false (Int.not_lt.mpr (of_decide_eq_true h))
theorem eq_of_not_ne (a b : Int) (h : (a != b) = false) : (a == b) = true := by
  simpa [bne] using h
theorem eq_false_of_ne (a b : Int) (h : (a != b) = true) : (a == b) = false := by
  simpa [bne] using h
theorem ne_of_not_eq (a b : Int) (h : (a == b) = false) : (a != b) = true := by
  simp [bne, h]
theorem ne_false_of_eq (a b : Int) (h : (a == b) = true) : (a != b) = false := by
  simp [bne, h]
theorem add_comm (a b : Int) : a + b = b + a := Int.add_comm a b
theorem mul_comm (a b : Int) : a * b = b * a := Int.mul_comm a b
theorem add_assoc (a b c : Int) : a + b + c = a + (b + c) := Int.add_assoc a b c
theorem mul_assoc (a b c : Int) : a * b * c = a * (b * c) := Int.mul_assoc a b c
theorem add_zero (a : Int) : a + 0 = a := Int.add_zero a
theorem zero_add (a : Int) : 0 + a = a := Int.zero_add a
theorem mul_one (a : Int) : a * 1 = a := Int.mul_one a
theorem one_mul (a : Int) : 1 * a = a := Int.one_mul a
theorem sub_zero (a : Int) : a - 0 = a := Int.sub_zero a
theorem div_mod_recompose (a k : Int) (_h : decide (k > 0) = true) : a / k * k + a % k = a :=
  Int.ediv_mul_add_emod a k
theorem div_range (a k m n : Int) (h1 : (decide (0 <= a) && decide (a < m)) = true)
    (h2 : (decide (k > 0) && (m == n * k)) = true) :
    (decide (0 <= a / k) && decide (a / k < n)) = true := by
  simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at h1 h2 ⊢
  obtain ⟨h0, hm⟩ := h1
  obtain ⟨hk, rfl⟩ := h2
  exact ⟨Int.ediv_nonneg h0 (Int.le_of_lt hk), (Int.ediv_lt_iff_lt_mul hk).mpr hm⟩
theorem list_concat_nil {α : Type} (b : List α) : (([] : List α) ++ b) = b := rfl
theorem list_concat_cons {α : Type} (x : α) (a b : List α) : ((x :: a) ++ b) = (x :: (a ++ b)) := rfl
theorem list_len_nil {α : Type} : ((([] : List α)).length : Int) = 0 := rfl
theorem list_len_cons {α : Type} (x : α) (a : List α) : (((x :: a)).length : Int) = (a.length : Int) + 1 := rfl
theorem list_take_nil {α : Type} (n : Int) : ([] : List α).take (Int.toNat n) = [] := by
  cases Int.toNat n <;> rfl
theorem list_take_cons_le {α : Type} (x : α) (a : List α) (n : Int) (h : decide (n <= 0) = true) :
    (x :: a).take (Int.toNat n) = [] := by
  rw [Int.toNat_of_nonpos (of_decide_eq_true h)]; rfl
theorem list_take_cons_gt {α : Type} (x : α) (a : List α) (n : Int) (h : decide (n > 0) = true) :
    (x :: a).take (Int.toNat n) = (x :: a.take (Int.toNat (n - 1))) := by
  have e : Int.toNat n = Int.toNat (n - 1) + 1 := by have := of_decide_eq_true h; omega
  rw [e]; rfl
theorem list_drop_nil {α : Type} (n : Int) : ([] : List α).drop (Int.toNat n) = [] := by
  cases Int.toNat n <;> rfl
theorem list_drop_cons_le {α : Type} (x : α) (a : List α) (n : Int) (h : decide (n <= 0) = true) :
    (x :: a).drop (Int.toNat n) = (x :: a) := by
  rw [Int.toNat_of_nonpos (of_decide_eq_true h)]; rfl
theorem list_drop_cons_gt {α : Type} (x : α) (a : List α) (n : Int) (h : decide (n > 0) = true) :
    (x :: a).drop (Int.toNat n) = a.drop (Int.toNat (n - 1)) := by
  have e : Int.toNat n = Int.toNat (n - 1) + 1 := by have := of_decide_eq_true h; omega
  rw [e]; rfl
theorem list_reverse_nil {α : Type} : ([] : List α).reverse = [] := rfl
theorem list_reverse_cons {α : Type} (x : α) (a : List α) : (x :: a).reverse = (a.reverse ++ [x]) :=
  List.reverse_cons
end AverSteps"#;

/// The only rule-specific Lean table.
fn lemma(rule: WallRule) -> &'static str {
    match rule {
        WallRule::AndTrueL => "AverSteps.and_true_l",
        WallRule::AndFalseL => "AverSteps.and_false_l",
        WallRule::AndTrueR => "AverSteps.and_true_r",
        WallRule::AndFalseR => "AverSteps.and_false_r",
        WallRule::OrTrueL => "AverSteps.or_true_l",
        WallRule::OrFalseL => "AverSteps.or_false_l",
        WallRule::OrTrueR => "AverSteps.or_true_r",
        WallRule::OrFalseR => "AverSteps.or_false_r",
        WallRule::NotTrue => "AverSteps.not_true",
        WallRule::NotFalse => "AverSteps.not_false",
        WallRule::AndElimL => "AverSteps.and_elim_l",
        WallRule::AndElimR => "AverSteps.and_elim_r",
        WallRule::LeOfNotGt => "AverSteps.le_of_not_gt",
        WallRule::LeFalseOfGt => "AverSteps.le_false_of_gt",
        WallRule::GeOfNotLt => "AverSteps.ge_of_not_lt",
        WallRule::GeFalseOfLt => "AverSteps.ge_false_of_lt",
        WallRule::LtOfNotGe => "AverSteps.lt_of_not_ge",
        WallRule::LtFalseOfGe => "AverSteps.lt_false_of_ge",
        WallRule::GtOfNotLe => "AverSteps.gt_of_not_le",
        WallRule::GtFalseOfLe => "AverSteps.gt_false_of_le",
        WallRule::EqOfNotNe => "AverSteps.eq_of_not_ne",
        WallRule::EqFalseOfNe => "AverSteps.eq_false_of_ne",
        WallRule::NeOfNotEq => "AverSteps.ne_of_not_eq",
        WallRule::NeFalseOfEq => "AverSteps.ne_false_of_eq",
        WallRule::AddComm => "AverSteps.add_comm",
        WallRule::MulComm => "AverSteps.mul_comm",
        WallRule::AddAssoc => "AverSteps.add_assoc",
        WallRule::MulAssoc => "AverSteps.mul_assoc",
        WallRule::AddZero => "AverSteps.add_zero",
        WallRule::ZeroAdd => "AverSteps.zero_add",
        WallRule::MulOne => "AverSteps.mul_one",
        WallRule::OneMul => "AverSteps.one_mul",
        WallRule::SubZero => "AverSteps.sub_zero",
        WallRule::DivModRecompose => "AverSteps.div_mod_recompose",
        WallRule::DivRange => "AverSteps.div_range",
        WallRule::ConcatNil => "AverSteps.list_concat_nil",
        WallRule::ConcatCons => "AverSteps.list_concat_cons",
        WallRule::LenNil => "AverSteps.list_len_nil",
        WallRule::LenCons => "AverSteps.list_len_cons",
        WallRule::TakeNil => "AverSteps.list_take_nil",
        WallRule::TakeConsLe => "AverSteps.list_take_cons_le",
        WallRule::TakeConsGt => "AverSteps.list_take_cons_gt",
        WallRule::DropNil => "AverSteps.list_drop_nil",
        WallRule::DropConsLe => "AverSteps.list_drop_cons_le",
        WallRule::DropConsGt => "AverSteps.list_drop_cons_gt",
        WallRule::ReverseNil => "AverSteps.list_reverse_nil",
        WallRule::ReverseCons => "AverSteps.list_reverse_cons",
    }
}

/// A rendered step proof: the local unfold lemmas (`have …`) and the term
/// that proves the obligation once the givens and the `when` hypothesis are
/// introduced.
pub(crate) struct Rendered {
    pub support: Vec<String>,
    pub term: String,
}

impl Rendered {
    /// The tactic text of the step branch: every local lemma, then the term.
    pub(crate) fn tactic(&self) -> String {
        let mut s = String::new();
        for h in &self.support {
            s.push_str(h);
            s.push_str("; ");
        }
        s.push_str("exact ");
        s.push_str(&self.term);
        s
    }
}

struct Renderer<'a> {
    script: &'a Script,
    ctx: &'a CodegenContext,
    /// Lean theorem names of cited laws, by law key.
    laws: BTreeMap<String, String>,
    /// Emitted unfold lemmas, by (function, arm).
    /// Emitted unfold lemmas, by (function, arm, chosen value of a
    /// catch-all arm).
    unfolds: BTreeMap<(FnId, u32, String), String>,
    support: Vec<String>,
    /// Rendering a builtin fact over `List α`: an empty list is `[] : List α`.
    generic: bool,
}

/// `t` with every empty list that has no type typed `List α`.
fn over_alpha(t: &Term) -> Term {
    if let ResolvedExpr::List(xs) = &t.node
        && xs.is_empty()
        && t.ty().is_none()
    {
        let out = t.clone();
        out.set_ty(crate::ast::Type::List(Box::new(crate::ast::Type::named(
            "α",
        ))));
        return out;
    }
    term::map_children(t, &mut |c| Ok(over_alpha(c))).expect("total")
}

fn is_prop_comparison(t: &Term) -> bool {
    matches!(
        t.node,
        ResolvedExpr::BinOp(BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte, ..)
    )
}

fn is_bool_term(t: &Term) -> bool {
    matches!(t.ty(), Some(crate::ast::Type::Bool))
        || is_prop_comparison(t)
        || term::bool_value(t).is_some()
        || matches!(&t.node, ResolvedExpr::BinOp(BinOp::Eq | BinOp::Neq, ..))
        || matches!(&t.node, ResolvedExpr::Call(ResolvedCallee::Builtin(b), _)
            if matches!(b.as_str(), "Bool.and" | "Bool.or" | "Bool.not"))
}

impl Renderer<'_> {
    fn expr(&self, t: &Term) -> String {
        // A dependency's binding reads as `Lib.base`; Lean spells each
        // segment of that path on its own (`Lib.local'`).
        let qualified: Vec<(String, Term)> = self
            .script
            .consts
            .iter()
            .filter(|c| c.name.contains('.'))
            .map(|c| {
                let read = term::var(&super::syntax::aver_path_to_lean(&c.name));
                if let Some(ty) = c.value.ty() {
                    read.set_ty(ty.clone());
                }
                (c.name.clone(), read)
            })
            .collect();
        let generic;
        let t = if self.generic {
            generic = over_alpha(t);
            &generic
        } else {
            t
        };
        let renamed;
        let t = if qualified.is_empty() {
            t
        } else {
            renamed = term::subst(t, &qualified).unwrap_or_else(|_| t.clone());
            &renamed
        };
        let s = emit_expr(t, self.ctx);
        if is_bool_term(t) {
            format!("({s} : Bool)")
        } else {
            format!("({s})")
        }
    }

    fn eqn(&self, e: &Eqn) -> String {
        format!("{} = {}", self.expr(&e.lhs), self.expr(&e.rhs))
    }

    fn hyp_name(&self, name: &str) -> String {
        if name == "when" {
            "h_when".to_string()
        } else {
            name.to_string()
        }
    }

    fn concl(&self, p: &Proof, hyps: &Hyps) -> Result<Eqn, String> {
        conclusion(p, self.script, hyps)
    }

    fn proof(&mut self, p: &Proof, hyps: &Hyps) -> Result<String, String> {
        let eq = self.concl(p, hyps)?;
        let body = match p {
            // A module-level binding is a Lean `def` with no parameters;
            // its value is its definitional unfolding.
            // `[x, y]` is notation for `x :: [y]`.
            Proof::Refl(_)
            | Proof::Proj { .. }
            | Proof::Cell { .. }
            | Proof::UnfoldConst { .. } => "rfl".to_string(),
            Proof::Symm(inner) => format!("Eq.symm {}", self.proof(inner, hyps)?),
            Proof::Trans { steps, .. } => {
                let mut parts = Vec::new();
                for s in steps {
                    parts.push(self.proof(s, hyps)?);
                }
                let mut acc = parts.pop().expect("trans has steps");
                while let Some(prev) = parts.pop() {
                    acc = format!("(Eq.trans {prev} {acc})");
                }
                acc
            }
            Proof::Congr { ctx, inner } => {
                let inner_eq = self.concl(inner, hyps)?;
                let binder = match inner_eq
                    .lhs
                    .ty()
                    .filter(|ty| crate::types::checker::type_is_fully_concrete(ty))
                {
                    Some(ty) => format!("({HOLE} : {})", type_to_lean(ty)),
                    None => HOLE.to_string(),
                };
                let body = self.expr(ctx);
                format!(
                    "congrArg (fun {binder} => {body}) {}",
                    self.proof(inner, hyps)?
                )
            }
            Proof::Unfold {
                fn_id,
                arm,
                args,
                binders,
                premise,
            } => {
                let (name, extra) = self.unfold_lemma(*fn_id, *arm, binders)?;
                let mut s = name;
                for a in args.iter().chain(&extra) {
                    s.push(' ');
                    s.push_str(&self.expr(a));
                }
                if let Some(p) = premise {
                    s.push(' ');
                    s.push_str(&self.proof(p, hyps)?);
                }
                s
            }
            Proof::Arm {
                term: t,
                arm,
                binders,
                premise,
            } => {
                let ResolvedExpr::Match { arms, .. } = &t.node else {
                    return Err("arm: not a match".into());
                };
                let pat = &arms[*arm as usize - 1].pattern;
                let literal;
                let pat = match binders.as_slice() {
                    [v] if crate::ir::proof_steps::check::is_catch_all(pat) => {
                        match term::bool_value(v) {
                            Some(b) => {
                                literal = ResolvedPattern::Literal(crate::ast::Literal::Bool(b));
                                &literal
                            }
                            None => pat,
                        }
                    }
                    _ => pat,
                };
                let tactic = arm_tactic(pat, t, "h_arm")?;
                format!("(fun h_arm => by {tactic}) {}", self.proof(premise, hyps)?)
            }
            // A `when` that is a bare comparison is stated `(a < b) = true`,
            // which Lean reads as an equation of Props; recover the Bool
            // equation the steps use.
            Proof::Hyp(name)
                if name == "when"
                    && self
                        .script
                        .obligation
                        .premise
                        .as_ref()
                        .is_some_and(is_prop_comparison) =>
            {
                "decide_eq_true (of_eq_true (h_when.trans (eq_self true)))".to_string()
            }
            Proof::Hyp(name) => self.hyp_name(name),
            Proof::Rule {
                rule,
                subst,
                premises,
            } => {
                let mut s = lemma(*rule).to_string();
                for (_, v) in subst {
                    s.push(' ');
                    s.push_str(&self.expr(v));
                }
                for p in premises {
                    s.push(' ');
                    s.push_str(&self.proof(p, hyps)?);
                }
                s
            }
            Proof::Law {
                law,
                subst,
                premise,
            } => {
                let mut s = self
                    .laws
                    .get(law)
                    .cloned()
                    .ok_or_else(|| format!("law {law} has no theorem to cite"))?;
                for (_, v) in subst {
                    s.push(' ');
                    s.push_str(&self.expr(v));
                }
                let cited = self.script.law(law);
                if let Some(p) = premise {
                    let p = self.proof(p, hyps)?;
                    // A bare comparison as `when` is a Prop in the theorem.
                    let prop = cited
                        .and_then(|l| l.premise.as_ref())
                        .is_some_and(is_prop_comparison);
                    s.push(' ');
                    if prop {
                        s.push_str(&format!(
                            "(propext ⟨fun _ => rfl, fun _ => of_decide_eq_true {p}⟩)"
                        ));
                    } else {
                        s.push_str(&p);
                    }
                }
                match cited {
                    // A fact is stated as the equation of Bools the steps
                    // read.
                    Some(l) if l.fact.is_some() => s,
                    Some(l) => from_statement(&l.lhs, &l.rhs, format!("({s})")),
                    None => s,
                }
            }
            Proof::Compute { .. } => "by decide".to_string(),
            Proof::Cases {
                on,
                hyp,
                if_true,
                if_false,
            } => {
                let with = |v: bool| {
                    let mut h = hyps.clone();
                    h.push((hyp.clone(), Eqn::new(on.clone(), term::boolean(v))));
                    h
                };
                let pf = self.proof(if_false, &with(false))?;
                let pt = self.proof(if_true, &with(true))?;
                format!(
                    "AverSteps.bool_cases {} (fun {hyp} => {pf}) (fun {hyp} => {pt})",
                    self.expr(on)
                )
            }
            // `f.induct`, the functional induction principle Lean derives
            // from `f`'s own recursion, with the claim as its motive and
            // one explicit function per case: the parameters that are not
            // matched, the arm's pattern variables, then one hypothesis per
            // recursive call.
            Proof::Induct {
                fn_id, args, cases, ..
            } => {
                let def = self.script.def(*fn_id).ok_or("induct: no definition")?;
                let j = crate::ir::proof_steps::induct::structural_param(def)?
                    .ok_or("induct: the function does not recurse")?;
                let (v, general) = crate::ir::proof_steps::induct::varied(
                    args,
                    j,
                    &self.script.obligation.givens,
                )?;
                let ResolvedExpr::Match { arms, .. } = &def.body.node else {
                    return Err("induct: the body is not a match".into());
                };
                let lean = super::syntax::aver_name_to_lean;
                let place = |k: usize| -> String {
                    if k == j {
                        lean(&v)
                    } else {
                        general
                            .iter()
                            .find(|(g, _)| *g == k)
                            .map(|(_, n)| lean(n))
                            .unwrap_or_else(|| "_".to_string())
                    }
                };
                let f_name = emit_expr(
                    &Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(*fn_id), Vec::new())),
                    self.ctx,
                );
                // Lean leaves out of `f.induct` every parameter each
                // recursive call passes on unchanged.
                let fixed: Vec<bool> = (0..args.len())
                    .map(|k| {
                        k != j
                            && crate::ir::proof_steps::induct::self_calls(&def.body, *fn_id)
                                .iter()
                                .all(|(call, inner)| {
                                    matches!(&call[k].node, ResolvedExpr::Ident(n)
                                        if *n == def.params[k] && !inner.contains(n))
                                })
                    })
                    .collect();
                let varies = |k: &usize| !fixed[*k];
                let motive_binders: Vec<String> =
                    (0..args.len()).filter(varies).map(place).collect();
                let mut s = format!(
                    "({f_name}.induct (motive := fun {} => {})",
                    motive_binders.join(" "),
                    self.eqn(&eq)
                );
                let mut scope_hyps: Vec<Hyps> = Vec::new();
                for (arm, case) in arms.iter().zip(cases) {
                    let (_, ihs) = crate::ir::proof_steps::induct::case(
                        def,
                        args,
                        j,
                        &v,
                        &general,
                        arm,
                        &case.binders,
                        &case.ihs,
                        &eq.lhs,
                        &eq.rhs,
                    )?;
                    let mut scope = hyps.clone();
                    scope.extend(ihs);
                    scope_hyps.push(scope);
                }
                for (case, scope) in cases.iter().zip(&scope_hyps) {
                    let mut binders: Vec<String> = (0..args.len())
                        .filter(|k| *k != j && varies(k))
                        .map(place)
                        .collect();
                    binders.extend(case.binders.iter().map(|b| lean(b)));
                    binders.extend(case.ihs.iter().cloned());
                    let body = self.proof(&case.proof, scope)?;
                    if binders.is_empty() {
                        s.push_str(&format!(" ({body})"));
                    } else {
                        s.push_str(&format!(" (fun {} => {body})", binders.join(" ")));
                    }
                }
                for (k, a) in args.iter().enumerate() {
                    if varies(&k) {
                        s.push(' ');
                        s.push_str(&self.expr(a));
                    }
                }
                s.push(')');
                s
            }
            // `List.rec` with the claim as its motive: the empty-list case,
            // then the cell case over the head, the tail and the claim at
            // the tail.
            Proof::InductList {
                var,
                nil,
                head,
                tail,
                ih,
                cons,
                lhs,
                rhs,
            } => {
                let lean = super::syntax::aver_name_to_lean;
                let at_tail = [(var.clone(), term::var(tail))];
                let mut scope = hyps.clone();
                scope.push((
                    ih.clone(),
                    Eqn::new(term::subst(lhs, &at_tail)?, term::subst(rhs, &at_tail)?),
                ));
                let base = self.proof(nil, hyps)?;
                let step = self.proof(cons, &scope)?;
                format!(
                    "(List.rec (motive := fun {v} => {}) ({base}) (fun {} {} {ih} => {step}) {v})",
                    self.eqn(&eq),
                    lean(head),
                    lean(tail),
                    v = lean(var),
                )
            }
            // Each decided comparison becomes a Prop fact and Lean's core
            // decision procedure for linear integer arithmetic closes the
            // goal from them alone; the kernel written in Aver checks the
            // weights instead.
            Proof::Linear {
                goal,
                value,
                hyps: names,
                ..
            } => {
                let mut facts = String::new();
                for (i, n) in names.iter().enumerate() {
                    let (_, e) = hyps
                        .iter()
                        .rev()
                        .find(|(h, _)| h == n)
                        .ok_or("linear: no such hypothesis")?;
                    let proof = self.proof(&Proof::Hyp(n.clone()), hyps)?;
                    let fact = match term::bool_value(&e.rhs) {
                        Some(true) => format!("of_decide_eq_true {proof}"),
                        _ => format!("of_decide_eq_false {proof}"),
                    };
                    facts.push_str(&format!("have steps_fact{i} := {fact}; "));
                }
                // The comparison as the Prop `omega` proves.
                let prop = emit_expr(goal, self.ctx);
                if *value {
                    format!("decide_eq_true (show {prop} from by {facts}omega)")
                } else {
                    format!("decide_eq_false (show ¬ ({prop}) from by {facts}omega)")
                }
            }
            // Both sides read as `Lean.Grind.CommRing.Expr` over one list of
            // atoms; the normaliser Lean's core proves sound
            // (`CommRing.norm_int`) maps both to one polynomial, which the
            // kernel checks by evaluation.
            Proof::Ring { lhs, rhs } => {
                let mut atoms: Vec<Term> = Vec::new();
                let el = reify(lhs, &mut atoms);
                let er = reify(rhs, &mut atoms);
                let rendered: Vec<String> = atoms.iter().map(|a| self.expr(a)).collect();
                let ctx = rarray(&rendered);
                format!(
                    "((Lean.Grind.CommRing.norm_int {ctx} {el} (Lean.Grind.CommRing.Expr.toPoly_k {er}) rfl).trans (Lean.Grind.CommRing.norm_int {ctx} {er} (Lean.Grind.CommRing.Expr.toPoly_k {er}) rfl).symm)"
                )
            }
            // `true = false` (or the other way round) is refuted by `decide`.
            Proof::Absurd { contradiction, .. } => {
                format!("absurd {} (by decide)", self.proof(contradiction, hyps)?)
            }
            // A term-mode `match` on the given, one arm per value, with
            // every hypothesis that mentions the given matched alongside
            // so it is read at that value. A missing or wrong case is an
            // elaboration error of the step term, which then falls back to
            // the portfolio.
            Proof::Enum { var, cases, .. } => {
                let (_, ty) = self
                    .script
                    .obligation
                    .finite
                    .iter()
                    .find(|(n, _)| n == var)
                    .ok_or_else(|| format!("enum: {var} is not of finite type"))?;
                let dependent: Vec<String> = hyps
                    .iter()
                    .filter(|(_, e)| {
                        let mut fv = Vec::new();
                        term::free_vars(&e.lhs, &mut fv);
                        term::free_vars(&e.rhs, &mut fv);
                        fv.contains(var)
                    })
                    .map(|(n, _)| self.hyp_name(n))
                    .fold(Vec::new(), |mut acc, n| {
                        if !acc.contains(&n) {
                            acc.push(n);
                        }
                        acc
                    });
                let mut discriminants = vec![super::syntax::aver_name_to_lean(var)];
                discriminants.extend(dependent.iter().cloned());
                let mut s = format!("(match {} with", discriminants.join(", "));
                let patterns = self.match_patterns(ty);
                for ((value, pattern), case) in ty.values().iter().zip(patterns).zip(cases) {
                    let at = [(var.clone(), value.clone())];
                    let scoped: Hyps = hyps
                        .iter()
                        .map(|(n, e)| {
                            Ok((
                                n.clone(),
                                Eqn::new(term::subst(&e.lhs, &at)?, term::subst(&e.rhs, &at)?),
                            ))
                        })
                        .collect::<Result<_, String>>()?;
                    let mut heads = vec![pattern];
                    heads.extend(dependent.iter().cloned());
                    s.push_str(&format!(
                        " | {} => {}",
                        heads.join(", "),
                        self.proof(case, &scoped)?
                    ));
                }
                s.push(')');
                s
            }
        };
        Ok(format!("(show {} from {body})", self.eqn(&eq)))
    }

    /// One Lean pattern per value of a finite type, in the order
    /// [`crate::ir::proof_steps::Finite::values`] lists the values.
    fn match_patterns(&self, ty: &crate::ir::proof_steps::Finite) -> Vec<String> {
        use crate::ir::proof_steps::Finite;
        let product = |parts: Vec<Vec<String>>| -> Vec<Vec<String>> {
            parts.into_iter().fold(vec![Vec::new()], |acc, part| {
                acc.iter()
                    .flat_map(|prefix| {
                        part.iter().map(move |p| {
                            let mut next = prefix.clone();
                            next.push(p.clone());
                            next
                        })
                    })
                    .collect()
            })
        };
        match ty {
            Finite::Bool => vec!["false".into(), "true".into()],
            Finite::Sum(_) => ty.values().iter().map(|v| emit_expr(v, self.ctx)).collect(),
            Finite::Record { fields, .. } => {
                product(fields.iter().map(|(_, f)| self.match_patterns(f)).collect())
                    .into_iter()
                    .map(|ps| format!("⟨{}⟩", ps.join(", ")))
                    .collect()
            }
            Finite::Tuple(parts) => product(parts.iter().map(|f| self.match_patterns(f)).collect())
                .into_iter()
                .map(|ps| format!("({})", ps.join(", ")))
                .collect(),
        }
    }

    /// A local `have __aver_unfold_<n> : ∀ (x…) (y…) [h], f x… = arm`,
    /// proved once inside the step branch: if Lean cannot prove it, only
    /// that branch fails and the law falls back to its portfolio.
    ///
    /// A catch-all arm holds only for values the earlier arms exclude, so
    /// its lemma is stated for the one value the step chose (its free
    /// variables quantified); the returned terms are what the step passes
    /// after the arguments.
    fn unfold_lemma(
        &mut self,
        fn_id: FnId,
        arm: u32,
        chosen: &[Term],
    ) -> Result<(String, Vec<Term>), String> {
        let catch_all = self
            .script
            .def(fn_id)
            .and_then(|d| match &d.body.node {
                ResolvedExpr::Match { arms, .. } if arm > 0 => arms
                    .get(arm as usize - 1)
                    .map(|a| crate::ir::proof_steps::check::is_catch_all(&a.pattern)),
                _ => None,
            })
            .unwrap_or(false);
        let (value_key, extra) = match (catch_all, chosen) {
            (true, [v]) => {
                let mut fv = Vec::new();
                term::free_vars(v, &mut fv);
                (
                    self.expr(v),
                    fv.iter().map(|n| term::var(n)).collect::<Vec<_>>(),
                )
            }
            (true, _) => return Err("unfold: a catch-all arm takes its value".into()),
            (false, _) => (String::new(), chosen.to_vec()),
        };
        if let Some(name) = self.unfolds.get(&(fn_id, arm, value_key.clone())) {
            return Ok((name.clone(), extra));
        }
        let def = self
            .script
            .def(fn_id)
            .ok_or_else(|| "unfold: no definition".to_string())?;
        let fd = fn_def(self.ctx, fn_id)
            .ok_or_else(|| format!("unfold: {} has no source definition", def.name))?;
        let xs: Vec<Term> = (0..def.params.len())
            .map(|i| term::var(&format!("x{i}")))
            .collect();
        let call = Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(fn_id), xs.clone()));
        let outer = def.outer(&xs)?;
        let mut binders = String::new();
        let mut names: Vec<String> = Vec::new();
        for (i, (_, ty)) in fd.params.iter().enumerate() {
            binders.push_str(&format!(
                " (x{i} : {})",
                super::types::type_annotation_to_lean(ty)
            ));
            names.push(format!("x{i}"));
        }
        let f_name = emit_expr(
            &Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(fn_id), Vec::new())),
            self.ctx,
        );
        let name = format!("__aver_unfold_{}", self.unfolds.len());
        let statement;
        let tactic;
        if arm == 0 {
            let body = term::subst(&def.body, &outer)?;
            statement = format!("{} = {}", self.expr(&call), self.expr(&body));
            tactic = format!("first | rfl | {SAME_MATCHES}");
        } else {
            let ResolvedExpr::Match { subject, arms } = &def.body.node else {
                return Err("unfold: not a match".into());
            };
            let pat = &arms[arm as usize - 1].pattern;
            let n = if catch_all {
                extra.len()
            } else {
                term::pattern_binders(pat).len()
            };
            let ys: Vec<Term> = (0..n).map(|i| term::var(&format!("y{i}"))).collect();
            for i in 0..n {
                binders.push_str(&format!(" (y{i} : _)"));
                names.push(format!("y{i}"));
            }
            // A catch-all arm's value, with its free variables renamed to
            // the lemma's own binders.
            let value = match (catch_all, chosen) {
                (true, [v]) => {
                    let rename: Vec<(String, Term)> = extra
                        .iter()
                        .zip(&ys)
                        .map(|(x, y)| match &x.node {
                            ResolvedExpr::Ident(n) => (n.clone(), y.clone()),
                            _ => unreachable!("free variables are names"),
                        })
                        .collect();
                    Some(term::subst(v, &rename)?)
                }
                _ => None,
            };
            let selected = match &value {
                Some(v) => vec![v.clone()],
                None => ys.clone(),
            };
            let (premise, body) = arm_equation(subject, arms, arm, &outer, &selected)?;
            binders.push_str(&format!(" (h : {})", self.eqn(&premise)));
            names.push("h".to_string());
            statement = format!("{} = {}", self.expr(&call), self.expr(&body));
            let subject = term::subst(subject, &outer)?;
            // A catch-all chosen for a Bool is the branch of that literal.
            let as_literal;
            let pat = match value.as_ref().and_then(term::bool_value) {
                Some(b) => {
                    as_literal = ResolvedPattern::Literal(crate::ast::Literal::Bool(b));
                    &as_literal
                }
                None => pat,
            };
            // A match the statement writes out is elaborated apart from the
            // definition's, so the two need not be equal by `rfl` alone.
            tactic = match arm_tactic(pat, &subject, "h")?.as_str() {
                "subst h; rfl" => format!("subst h; first | rfl | {SAME_MATCHES}"),
                "rw [h]; try rfl" => format!("rw [h]; all_goals first | rfl | {SAME_MATCHES}"),
                other => other.to_string(),
            };
        }
        let intro = if names.is_empty() {
            String::new()
        } else {
            format!("intro {}; ", names.join(" "))
        };
        let quantified = if binders.is_empty() {
            statement
        } else {
            format!("∀{binders}, {statement}")
        };
        // Only the left side is opened: the right side of an arm of a
        // recursive function calls the function again. `conv` may close the
        // goal itself, hence `all_goals`.
        self.support.push(format!(
            "have {name} : {quantified} := (by {intro}(conv => lhs; unfold {f_name}); all_goals ({tactic}))"
        ));
        self.unfolds.insert((fn_id, arm, value_key), name.clone());
        Ok((name, extra))
    }
}

/// The tactic that selects a known arm of the unfolded body, given
/// `h : subject = pattern`.
fn arm_tactic(pat: &ResolvedPattern, subject: &Term, h: &str) -> Result<String, String> {
    Ok(match pat {
        ResolvedPattern::Literal(crate::ast::Literal::Bool(v)) => {
            let positive = *v;
            let evidence = if is_prop_comparison(subject) {
                if positive {
                    format!("(of_decide_eq_true {h})")
                } else {
                    format!("(of_decide_eq_false {h})")
                }
            } else if positive {
                h.to_string()
            } else {
                format!("(by simp [{h}])")
            };
            if positive {
                format!("exact ite_eq_left {evidence}")
            } else {
                format!("exact ite_eq_right {evidence}")
            }
        }
        // A variable subject is replaced outright; otherwise rewrite it.
        // Either way the selected arm is then a definitional reduction.
        _ if matches!(subject.node, ResolvedExpr::Ident(_)) => format!("subst {h}; rfl"),
        _ => format!("rw [{h}]; try rfl"),
    })
}

fn fn_def(ctx: &CodegenContext, id: FnId) -> Option<&crate::ast::FnDef> {
    let key = &ctx.symbol_table.fn_entry(id).key;
    match key.scope_str() {
        Some(prefix) => ctx
            .modules
            .iter()
            .find(|m| m.prefix == prefix)?
            .fn_defs
            .iter()
            .find(|f| f.name == key.name),
        None => ctx.items.iter().find_map(|item| match item {
            crate::ast::TopLevel::FnDef(f) if f.name == key.name => Some(f),
            _ => None,
        }),
    }
}

fn same_file_blocks(ctx: &CodegenContext) -> Vec<&crate::ast::VerifyBlock> {
    match ctx.active_module_scope().as_deref() {
        Some(prefix) => ctx
            .modules
            .iter()
            .find(|m| m.prefix == prefix)
            .map(|m| m.verify_blocks.iter().collect())
            .unwrap_or_default(),
        None => ctx
            .items
            .iter()
            .filter_map(|item| match item {
                crate::ast::TopLevel::Verify(vb) => Some(vb),
                _ => None,
            })
            .collect(),
    }
}

/// The Lean theorem a law key names, as `dependencies` in the reason
/// emitter finds it.
/// Each builtin fact `body` cites, as one theorem over every element type,
/// stated from the fact's own terms and proved by its steps.
pub(crate) fn render_cited_facts(body: &str, ctx: &CodegenContext) -> Result<String, String> {
    let all = crate::ir::proof_steps::facts::all();
    // The cited facts and every fact their proofs cite.
    let mut needed: Vec<String> = all
        .iter()
        .filter(|f| body.contains(&fact_theorem(f)))
        .map(|f| f.key.to_string())
        .collect();
    for fact in all.iter().rev() {
        if needed.iter().any(|k| k == fact.key) {
            for l in &fact.script.laws {
                if !needed.contains(&l.key) {
                    needed.push(l.key.clone());
                }
            }
        }
    }
    let mut out = Vec::new();
    for fact in &all {
        if !needed.iter().any(|k| k == fact.key) {
            continue;
        }
        let script = &fact.script;
        let laws = script
            .laws
            .iter()
            .filter_map(|l| {
                let cited = crate::ir::proof_steps::facts::named(&l.key)?;
                Some((l.key.clone(), fact_theorem(&cited)))
            })
            .collect();
        let mut r = Renderer {
            script,
            ctx,
            laws,
            unfolds: BTreeMap::new(),
            support: Vec::new(),
            generic: true,
        };
        let ob = &script.obligation;
        let statement = r.eqn(&Eqn::new(ob.lhs.clone(), ob.rhs.clone()));
        let term = r.proof(&script.proof, &Hyps::new())?;
        if !r.support.is_empty() || ob.givens != ob.lists {
            return Err(format!("fact {}: not a fact over lists alone", fact.key));
        }
        out.push(format!(
            "set_option autoImplicit false in\ntheorem {} {{α : Type}} ({} : List α) :\n    {statement} :=\n  {term}",
            fact_theorem(fact),
            ob.givens.join(" ")
        ));
    }
    Ok(out.join("\n\n"))
}

/// The Lean theorem a builtin fact is stated as, in `AverCommon`.
pub(crate) fn fact_theorem(fact: &crate::ir::proof_steps::facts::Fact) -> String {
    format!("AverFacts.{}", fact.lean)
}

fn law_theorem(key: &str, ctx: &CodegenContext) -> Option<String> {
    if let Some(fact) = crate::ir::proof_steps::facts::named(key) {
        return Some(fact_theorem(&fact));
    }
    let own = ctx.active_module_scope();
    let local_key = |prefix: Option<&str>, fn_name: &str, law: &str| match prefix {
        Some(p) => format!("{p}.{fn_name}.{law}"),
        None => format!("{fn_name}.{law}"),
    };
    for b in same_file_blocks(ctx) {
        let VerifyKind::Law(l) = &b.kind else {
            continue;
        };
        if key == local_key(own.as_deref(), &b.fn_name, &l.name) {
            return super::toplevel::law_as_lemma_statement(b, l, ctx).map(|(name, _)| name);
        }
    }
    for module in &ctx.modules {
        for block in &module.verify_laws {
            let VerifyKind::Law(l) = &block.kind else {
                continue;
            };
            if key == local_key(Some(&module.prefix), &block.fn_name, &l.name) {
                return ctx.with_module_scope(Some(&module.prefix), || {
                    super::toplevel::law_as_lemma_statement(block, l, ctx)
                        .map(|(name, _)| format!("{}.{}", aver_name_to_lean(&module.prefix), name))
                });
            }
        }
    }
    None
}

/// `t` as a `Lean.Grind.CommRing.Expr`, the way [`crate::ir::proof_steps::ring`]
/// reads it: ring operations on Int, every other subterm an atom.
fn reify(t: &Term, atoms: &mut Vec<Term>) -> String {
    use crate::ast::Type;
    let e = "Lean.Grind.CommRing.Expr";
    if let Some(v) = term::int_value(t) {
        return format!("({e}.num ({v}))");
    }
    let int_op = !matches!(t.ty(), Some(Type::Str | Type::Float));
    match &t.node {
        ResolvedExpr::BinOp(BinOp::Add, a, b) if int_op => {
            format!("({e}.add {} {})", reify(a, atoms), reify(b, atoms))
        }
        ResolvedExpr::BinOp(BinOp::Sub, a, b) if int_op => {
            format!("({e}.sub {} {})", reify(a, atoms), reify(b, atoms))
        }
        ResolvedExpr::BinOp(BinOp::Mul, a, b) if int_op => {
            format!("({e}.mul {} {})", reify(a, atoms), reify(b, atoms))
        }
        ResolvedExpr::Neg(a) if int_op => format!("({e}.neg {})", reify(a, atoms)),
        _ => {
            let c = term::canon(t);
            let i = match atoms.iter().position(|x| *x == c) {
                Some(i) => i,
                None => {
                    atoms.push(c);
                    atoms.len() - 1
                }
            };
            format!("({e}.var {i})")
        }
    }
}

/// A `Lean.RArray` of the rendered atoms, index `i` at position `i`.
fn rarray(items: &[String]) -> String {
    fn build(items: &[String], from: usize) -> String {
        match items.len() {
            0 => "(Lean.RArray.leaf (0 : Int))".to_string(),
            1 => format!("(Lean.RArray.leaf {})", items[0]),
            n => {
                let mid = n / 2;
                format!(
                    "(Lean.RArray.branch {} {} {})",
                    from + mid,
                    build(&items[..mid], from),
                    build(&items[mid..], from + mid)
                )
            }
        }
    }
    build(items, 0)
}

/// Whether `t` builds or reads the inside of a refined record, which Lean
/// models as a `Subtype` the step terms cannot spell.
fn touches_refinement(t: &Term, ctx: &CodegenContext) -> bool {
    let refined = ctx.proof_ir.refined_types.values();
    let here = match &t.node {
        ResolvedExpr::RecordCreate { type_name, .. }
        | ResolvedExpr::RecordUpdate { type_name, .. } => {
            crate::codegen::common::find_refined_type(ctx, type_name).is_some()
        }
        ResolvedExpr::Attr(_, field) => refined.clone().any(|d| d.carrier_field == *field),
        _ => false,
    };
    if here {
        return true;
    }
    if let ResolvedExpr::Match { arms, .. } = &t.node
        && arms.iter().any(|a| touches_refinement(&a.body, ctx))
    {
        return true;
    }
    term::children(t)
        .into_iter()
        .any(|c| touches_refinement(c, ctx))
}

/// Render `script` as a term for its law's theorem, after `intro` of the
/// givens (and `h_when`).
pub(crate) fn render(script: &Script, ctx: &CodegenContext) -> Result<Rendered, String> {
    // Every term of the proof comes from these, by unfolding, rewriting
    // and computing; the Lean model of a refined record is a `Subtype`,
    // which the step terms do not spell, so such a law keeps its tactics.
    let ob = &script.obligation;
    let mut sources: Vec<&Term> = vec![&ob.lhs, &ob.rhs];
    sources.extend(ob.premise.iter());
    for d in &script.defs {
        sources.push(&d.body);
        sources.extend(d.lets.iter().map(|(_, v)| v));
    }
    sources.extend(script.consts.iter().map(|c| &c.value));
    for l in &script.laws {
        sources.extend([&l.lhs, &l.rhs]);
        sources.extend(l.premise.iter());
    }
    if sources.into_iter().any(|t| touches_refinement(t, ctx)) {
        return Err("the proof reads the inside of a refined record".into());
    }
    let mut laws = BTreeMap::new();
    for l in &script.laws {
        if let Some(name) = law_theorem(&l.key, ctx) {
            laws.insert(l.key.clone(), name);
        }
    }
    let mut r = Renderer {
        script,
        ctx,
        laws,
        unfolds: BTreeMap::new(),
        support: Vec::new(),
        generic: false,
    };
    let mut hyps = Hyps::new();
    if let Some(p) = &script.obligation.premise {
        hyps.push(("when".into(), Eqn::new(p.clone(), term::boolean(true))));
    }
    let term = r.proof(&script.proof, &hyps)?;
    let term = to_statement(&script.obligation, term);
    Ok(Rendered {
        support: r.support,
        term,
    })
}

/// A cited law's theorem states a comparison as a Prop; the steps read it
/// as an equation of Bools, every comparison through `decide`.
fn from_statement(lhs: &Term, rhs: &Term, h: String) -> String {
    match (is_prop_comparison(lhs), is_prop_comparison(rhs)) {
        (false, false) => h,
        // `decide A = R` from `A = (R = true)`.
        (true, false) => format!(
            "(Bool.eq_iff_iff.mpr ⟨fun d => Eq.mp {h} (of_decide_eq_true d), fun r => decide_eq_true (Eq.mpr {h} r)⟩)"
        ),
        // `L = decide B` from `(L = true) = B`.
        (false, true) => format!(
            "(Bool.eq_iff_iff.mpr ⟨fun l => decide_eq_true (Eq.mp {h} l), fun d => Eq.mpr {h} (of_decide_eq_true d)⟩)"
        ),
        // `decide A = decide B` from `A = B`.
        (true, true) => format!(
            "(Bool.eq_iff_iff.mpr ⟨fun d => decide_eq_true (Eq.mp {h} (of_decide_eq_true d)), fun d => decide_eq_true (Eq.mpr {h} (of_decide_eq_true d))⟩)"
        ),
    }
}

/// The step term proves the claim as an equation of Bools, every Int
/// comparison read through `decide`. Lean states a comparison as a Prop, so
/// a claim with a comparison on one side, or on both, is an equation of
/// Props: carry the proof across.
fn to_statement(ob: &crate::ir::proof_steps::Obligation, t: String) -> String {
    match (is_prop_comparison(&ob.lhs), is_prop_comparison(&ob.rhs)) {
        (false, false) => t,
        // `A = (R = true)` from `decide A = R`.
        (true, false) => format!(
            "(propext ⟨fun h => ({t}).symm.trans (decide_eq_true h), fun h => of_decide_eq_true (({t}).trans h)⟩)"
        ),
        // `(L = true) = B` from `L = decide B`.
        (false, true) => format!(
            "(propext ⟨fun h => of_decide_eq_true (({t}).symm.trans h), fun h => ({t}).trans (decide_eq_true h)⟩)"
        ),
        // `A = B` from `decide A = decide B`.
        (true, true) => format!(
            "(propext ⟨fun h => of_decide_eq_true (({t}).symm.trans (decide_eq_true h)), fun h => of_decide_eq_true (({t}).trans (decide_eq_true h))⟩)"
        ),
    }
}

/// Closes `body = body'` where both write the same matches, elaborated
/// apart: split every match and compare the branches.
const SAME_MATCHES: &str = "(dsimp only; (repeat' split) <;> simp_all)";

/// The marker a rejected step proof leaves in the build log.
pub(crate) const STEPS_REJECTED_MARKER: &str = "AVER_STEPS_REJECTED:";

/// Wrap a portfolio so the step term is tried first:
/// `first | (intro …; exact <term>) | (trace <rejected>; <portfolio>)`.
pub(crate) fn lead_portfolio(
    rendered: &Rendered,
    intro: &[String],
    label: &str,
    portfolio: crate::codegen::lean::tactic_ir::Tactic,
) -> crate::codegen::lean::tactic_ir::Tactic {
    use crate::codegen::lean::tactic_ir::Tactic;
    let intro_line = if intro.is_empty() {
        String::new()
    } else {
        format!("intro {}; ", intro.join(" "))
    };
    // Leaves may carry a baked-in indent; new leaves take the same one so
    // the rendered block stays aligned.
    let pad = " ".repeat(portfolio.leaf_min_indent().unwrap_or(0));
    Tactic::First(vec![
        Tactic::Leaf(format!("{pad}{intro_line}{}", rendered.tactic())),
        Tactic::Seq(vec![
            Tactic::Leaf(format!("{pad}trace \"{STEPS_REJECTED_MARKER}{label}\"")),
            portfolio,
        ]),
    ])
}

/// One `proof_steps/<law>.steps` file per law with a step proof: the
/// serialised script the Aver replayer checks. A law no producer wrote
/// steps for gets `proof_steps/<law>.refused` instead, with where the
/// producer stopped on its first line and one `hint: …` line per builtin
/// fact that would rewrite a part of it.
pub(crate) fn step_files(ctx: &CodegenContext) -> Vec<(String, String)> {
    ctx.proof_ir
        .law_theorems
        .iter()
        .filter_map(|t| {
            if let Some(script) = t.steps.as_ref() {
                let text = crate::ir::proof_steps::sexpr::script(script, &ctx.symbol_table).ok()?;
                return Some((format!("proof_steps/{}.steps", script.obligation.key), text));
            }
            let why = t.steps_refusal.as_ref()?;
            let key = crate::ir::proof_steps::sexpr::Names::fn_name(&ctx.symbol_table, t.fn_id);
            let mut text = format!("{why}\n");
            for hint in &t.steps_hints {
                text.push_str(&format!("hint: {hint}\n"));
            }
            Some((format!("proof_steps/{key}.{}.refused", t.law_name), text))
        })
        .collect()
}
