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
use crate::ir::proof_steps::claim::{Hyps, arm_equation, claim};
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
theorem eq_of_le_ge (a b : Int) (h1 : decide (a <= b) = true) (h2 : decide (a >= b) = true) :
    (a == b) = true :=
  beq_iff_eq.mpr (Int.le_antisymm (of_decide_eq_true h1) (of_decide_eq_true h2))
theorem eq_false_of_lt (a b : Int) (h : decide (a < b) = true) : (a == b) = false :=
  beq_eq_false_iff_ne.mpr (Int.ne_of_lt (of_decide_eq_true h))
theorem eq_false_of_gt (a b : Int) (h : decide (a > b) = true) : (a == b) = false :=
  beq_eq_false_iff_ne.mpr (Int.ne_of_gt (of_decide_eq_true h))
theorem eq_of_beq {α : Type} [BEq α] [LawfulBEq α] (a b : α) (h : (a == b) = true) : a = b :=
  beq_iff_eq.mp h
theorem beq_refl {α : Type} [BEq α] [ReflBEq α] (a : α) : (a == a) = true := beq_self_eq_true a
theorem mod_range (a k : Int) (h : decide (k > 0) = true) :
    (decide (0 <= a % k) && decide (a % k < k)) = true := by
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h ⊢
  exact ⟨Int.emod_nonneg a (by omega), Int.emod_lt_of_pos a h⟩
theorem int_measure_induct {P : Int → Prop} (g : Int → Bool)
    (on_true : ∀ n, g n = true → (∀ m, m.toNat < n.toNat → P m) → P n)
    (on_false : ∀ n, g n = false → (∀ m, m.toNat < n.toNat → P m) → P n) (n : Int) : P n := by
  have key : ∀ (k : Nat) (x : Int), x.toNat ≤ k → P x := by
    intro k
    induction k with
    | zero =>
      intro x hx
      have below : ∀ m, m.toNat < x.toNat → P m := fun m hm => absurd hm (by omega)
      cases h : g x
      · exact on_false x h below
      · exact on_true x h below
    | succ k ih =>
      intro x hx
      have below : ∀ m, m.toNat < x.toNat → P m := fun m hm => ih m (by omega)
      cases h : g x
      · exact on_false x h below
      · exact on_true x h below
  exact key n.toNat n (Nat.le_refl _)
theorem pos_of_le_false {n : Int} (h : decide (n <= 0) = false) : 0 < n := by
  have := of_decide_eq_false h
  omega
theorem pos_of_gt_true {n : Int} (h : decide (n > 0) = true) : 0 < n := of_decide_eq_true h
theorem sub_one_lt {n : Int} (h : 0 < n) : (n - 1).toNat < n.toNat := by omega
theorem ediv_lt {n : Int} (k : Int) (hk : decide (k >= 2) = true) (h : 0 < n) :
    (n / k).toNat < n.toNat := by
  have k2 : 2 ≤ k := of_decide_eq_true hk
  have h0 : 0 ≤ n / k := Int.ediv_nonneg (by omega) (by omega)
  have h1 : n / k < n := Int.ediv_lt_of_lt_mul (by omega) (by
    have : n * 1 < n * k := Int.mul_lt_mul_of_pos_left (by omega) h
    omega)
  omega
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
theorem vector_to_list_of_list {α : Type} (l : List α) : l.toArray.toList = l := List.toList_toArray
theorem vector_of_list_to_list {α : Type} (v : Array α) : v.toList.toArray = v := Array.toArray_toList
theorem vector_len_as_list {α : Type} (v : Array α) : (v.size : Int) = (v.toList.length : Int) := by simp
theorem vector_get_negative {α : Type} (v : Array α) (i : Int) (h : decide (i < 0) = true) :
    (if i < 0 then Option.none else v[Int.toNat i]?) = Option.none := by
  simp [of_decide_eq_true h]
theorem vector_get_past_end {α : Type} (v : Array α) (i : Int) (h : decide (i >= (v.size : Int)) = true) :
    (if i < 0 then Option.none else v[Int.toNat i]?) = Option.none := by
  have := of_decide_eq_true h
  split
  · rfl
  · apply Array.getElem?_eq_none; omega
theorem vector_set_out_of_range {α : Type} (v : Array α) (i : Int) (x : α)
    (h : (decide (i < 0) || decide (i >= (v.size : Int))) = true) :
    (if i < 0 then Option.none else if i < v.size then Option.some (v.set! (Int.toNat i) x) else Option.none) = Option.none := by
  simp only [Bool.or_eq_true, decide_eq_true_eq] at h
  split
  · rfl
  · split
    · omega
    · rfl
theorem vector_get_set_same {α : Type} (v : Array α) (i : Int) (x : α)
    (h : (decide (0 <= i) && decide (i < (v.size : Int))) = true) :
    (if i < 0 then Option.none else ((if i < 0 then Option.none else if i < v.size then Option.some (v.set! (Int.toNat i) x) else Option.none).getD v)[Int.toNat i]?) = Option.some x := by
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  have h0 : ¬ i < 0 := by omega
  have h1 : (i < v.size) := h.2
  have h2 : i.toNat < v.size := by omega
  simp [h0, h1, Array.set!, h2]
theorem vector_get_set_other {α : Type} (v : Array α) (i : Int) (x : α) (j : Int)
    (h : (decide (0 <= i) && decide (i < (v.size : Int))) = true) (hj : (i != j) = true) :
    (if j < 0 then Option.none else ((if i < 0 then Option.none else if i < v.size then Option.some (v.set! (Int.toNat i) x) else Option.none).getD v)[Int.toNat j]?) = (if j < 0 then Option.none else v[Int.toNat j]?) := by
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  have hne : i ≠ j := by simpa using hj
  have h0 : ¬ i < 0 := by omega
  have h1 : (i < v.size) := h.2
  have hne' : ¬ j < 0 → i.toNat ≠ j.toNat := by intro hj0; omega
  by_cases hj0 : j < 0
  · simp [hj0]
  · simp [h0, h1, hj0, Array.set!, hne' hj0]
theorem vector_len_set {α : Type} (v : Array α) (i : Int) (x : α) :
    ((((if i < 0 then Option.none else if i < v.size then Option.some (v.set! (Int.toNat i) x) else Option.none).getD v).size : Int)) = (v.size : Int) := by
  split
  · rfl
  · split <;> simp
theorem vector_len_new {α : Type} (n : Int) (x : α) (h : decide (0 <= n) = true) :
    ((Array.replicate (Int.toNat n) x).size : Int) = n := by
  have := of_decide_eq_true h
  simp; omega
theorem vector_get_new {α : Type} (n : Int) (x : α) (i : Int)
    (h : (decide (0 <= i) && decide (i < n)) = true) :
    (if i < 0 then Option.none else (Array.replicate (Int.toNat n) x)[Int.toNat i]?) = Option.some x := by
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  have h0 : ¬ i < 0 := by omega
  have h1 : i.toNat < n.toNat := by omega
  simp [h0, h1]
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
        WallRule::EqOfLeGe => "AverSteps.eq_of_le_ge",
        WallRule::EqFalseOfLt => "AverSteps.eq_false_of_lt",
        WallRule::EqFalseOfGt => "AverSteps.eq_false_of_gt",
        WallRule::EqOfBeq => "AverSteps.eq_of_beq",
        WallRule::BeqRefl => "AverSteps.beq_refl",
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
        WallRule::ModRange => "AverSteps.mod_range",
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
        // The Map rules sit with the map model they read.
        WallRule::MapGetEmpty => "AverMap.step_get_empty",
        WallRule::MapGetSetSame => "AverMap.step_get_set_same",
        WallRule::MapGetSetOther => "AverMap.step_get_set_other",
        WallRule::MapHasEmpty => "AverMap.step_has_empty",
        WallRule::MapHasSetSame => "AverMap.step_has_set_same",
        WallRule::MapHasSetOther => "AverMap.step_has_set_other",
        WallRule::MapLenEmpty => "AverMap.step_len_empty",
        WallRule::MapLenSetPresent => "AverMap.step_len_set_present",
        WallRule::MapLenSetAbsent => "AverMap.step_len_set_absent",
        WallRule::VecToListOfList => "AverSteps.vector_to_list_of_list",
        WallRule::VecOfListToList => "AverSteps.vector_of_list_to_list",
        WallRule::VecLenToList => "AverSteps.vector_len_as_list",
        WallRule::VecGetNegative => "AverSteps.vector_get_negative",
        WallRule::VecGetPastEnd => "AverSteps.vector_get_past_end",
        WallRule::VecSetOutOfRange => "AverSteps.vector_set_out_of_range",
        WallRule::VecGetSetSame => "AverSteps.vector_get_set_same",
        WallRule::VecGetSetOther => "AverSteps.vector_get_set_other",
        WallRule::VecLenSet => "AverSteps.vector_len_set",
        WallRule::VecLenNew => "AverSteps.vector_len_new",
        WallRule::VecGetNew => "AverSteps.vector_get_new",
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
    /// Hypotheses the theorem introduces whose term is a bare comparison:
    /// Lean states them `(a < b) = true`, an equation of Props.
    prop_hyps: Vec<String>,
    /// Whether an induction was rendered by a type's recursor.
    by_recursor: bool,
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

    /// Induction along a function whose arms split further around a
    /// recursive call, where Lean's `f.induct` has more cases than the
    /// arms, or under hypotheses it carries: the recursor of the matched
    /// given's type, one case per constructor (the arm that matches it),
    /// with the motive the claim for every value of the varied givens and
    /// under the carried hypotheses. Each hypothesis of a case is the
    /// induction hypothesis of the part its call recurses on, applied to
    /// the call's arguments at the varied places and to the proofs of the
    /// carried hypotheses there.
    #[allow(clippy::too_many_arguments)]
    fn induct_by_cases_of_type(
        &mut self,
        def: &crate::ir::proof_steps::Def,
        rec: &crate::ir::proof_steps::induct::Recursion,
        args: &[Term],
        v: &str,
        general: &[(usize, String)],
        arms: &[crate::ir::hir::ResolvedMatchArm],
        carried: &[String],
        cases: &[crate::ir::proof_steps::InductCase],
        eq: &Eqn,
        hyps: &Hyps,
    ) -> Result<String, String> {
        use crate::ir::proof_steps::induct;
        let lean = super::syntax::aver_name_to_lean;
        let j = rec.at;
        let varying: Vec<String> = std::iter::once(v.to_string())
            .chain(general.iter().map(|(_, g)| g.clone()))
            .collect();
        let mentions = |e: &Eqn| {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            fv.iter().any(|n| varying.contains(n))
        };
        let mut stated: Vec<(String, Eqn)> = Vec::new();
        for name in carried {
            if self.prop_hyps.contains(name) {
                return Err(format!("induct: {name} is a comparison, not carried"));
            }
            let (_, e) = hyps
                .iter()
                .rev()
                .find(|(h, _)| h == name)
                .ok_or_else(|| format!("induct: {name} is not in scope"))?;
            stated.push((name.clone(), e.clone()));
        }
        let kept: Hyps = hyps.iter().filter(|(_, e)| !mentions(e)).cloned().collect();
        let gs: Vec<String> = general.iter().map(|(_, g)| lean(g)).collect();
        let hs: Vec<String> = carried.iter().map(|n| self.hyp_name(n)).collect();
        let mut motive = String::new();
        if !gs.is_empty() {
            motive.push_str(&format!("∀ {}, ", gs.join(" ")));
        }
        for (_, e) in &stated {
            motive.push_str(&format!("{} → ", self.eqn(e)));
        }
        motive.push_str(&self.eqn(eq));
        self.by_recursor = true;
        let (recursor, order) = self.recursor(&args[j])?;
        let mut minors: Vec<Option<String>> = vec![None; order.len()];
        for (arm, case) in arms.iter().zip(cases) {
            let (ctor, recursive) = self.constructor_fields(&arm.pattern)?;
            let at = order
                .iter()
                .position(|c| *c == ctor)
                .ok_or("induct: an arm matches no constructor of the type")?;
            if recursive.len() != case.binders.len() {
                return Err("induct: the pattern does not bind every field".into());
            }
            let field_ih = |k: usize| format!("steps_rec_{}", lean(&case.binders[k]));
            let mut names: Vec<String> = case.binders.iter().map(|b| lean(b)).collect();
            names.extend((0..recursive.len()).filter(|k| recursive[*k]).map(field_ih));
            names.extend(gs.iter().cloned());
            names.extend(hs.iter().cloned());
            let stated_case = induct::case(
                def,
                rec,
                args,
                v,
                general,
                arm,
                &case.binders,
                &case.ihs,
                &eq.lhs,
                &eq.rhs,
            )?;
            let here = [(v.to_string(), stated_case.value.clone())];
            let mut scope = kept.clone();
            for (name, e) in &stated {
                scope.push((
                    name.clone(),
                    Eqn::new(term::subst(&e.lhs, &here)?, term::subst(&e.rhs, &here)?),
                ));
            }
            let sources = induct::ih_sources(def, args, j, general, arm, &case.binders)?;
            let mut haves = String::new();
            let mut taken = Vec::new();
            for (k, ((ih, e), (part, at))) in stated_case.ihs.iter().zip(&sources).enumerate() {
                if ih == "_" {
                    continue;
                }
                if !recursive[*part] {
                    return Err("induct: a call recurses on a field of another type".into());
                }
                let mut app = field_ih(*part);
                for a in at {
                    app.push_str(&format!(" ({})", self.expr(a)));
                }
                for p in case.carry.get(k).into_iter().flatten() {
                    app.push_str(&format!(" {}", self.proof(p, &scope)?));
                }
                haves.push_str(&format!("have {ih} : {} := ({app}); ", self.eqn(e)));
                taken.push((ih.clone(), e.clone()));
            }
            // The further hypotheses: the same field's hypothesis at other
            // values of the varied givens.
            for (k, ih) in &case.more {
                let (part, _) = sources.get(*k).ok_or("induct: no such recursive call")?;
                if !recursive[*part] {
                    return Err("induct: a call recurses on a field of another type".into());
                }
                let mut at = vec![(v.to_string(), term::var(&case.binders[*part]))];
                at.extend(
                    general
                        .iter()
                        .map(|(_, g)| g.clone())
                        .zip(ih.at.iter().cloned()),
                );
                let e = Eqn::new(term::subst(&eq.lhs, &at)?, term::subst(&eq.rhs, &at)?);
                let mut app = field_ih(*part);
                for a in &ih.at {
                    app.push_str(&format!(" ({})", self.expr(a)));
                }
                for p in &ih.carry {
                    app.push_str(&format!(" {}", self.proof(p, &scope)?));
                }
                haves.push_str(&format!(
                    "have {} : {} := ({app}); ",
                    self.hyp_name(&ih.name),
                    self.eqn(&e)
                ));
                taken.push((ih.name.clone(), e));
            }
            let mut inner = scope.clone();
            inner.extend(taken);
            let body = self.proof(&case.proof, &inner)?;
            minors[at] = Some(if names.is_empty() {
                format!("({haves}{body})")
            } else {
                format!("(fun {} => {haves}{body})", names.join(" "))
            });
        }
        let minors: Vec<String> = minors
            .into_iter()
            .map(|m| m.ok_or_else(|| "induct: a constructor has no arm".to_string()))
            .collect::<Result<_, _>>()?;
        let mut s = format!(
            "({recursor} (motive := fun {} => {motive}) {} {}",
            lean(v),
            minors.join(" "),
            lean(v)
        );
        for g in &gs {
            s.push_str(&format!(" {g}"));
        }
        for name in carried {
            s.push_str(&format!(
                " {}",
                self.proof(&Proof::Hyp(name.clone()), hyps)?
            ));
        }
        s.push(')');
        Ok(s)
    }

    /// The recursor of the type of the given `v`, and its constructors in
    /// the order the recursor takes their cases.
    fn recursor(&self, v: &Term) -> Result<(String, Vec<String>), String> {
        use crate::ast::Type;
        use crate::codegen::proof_recognize::peano_type_named;
        match v.ty() {
            Some(Type::List(_)) => Ok(("List.rec".into(), vec!["nil".into(), "cons".into()])),
            Some(Type::Option(_)) => Ok(("Option.rec".into(), vec!["none".into(), "some".into()])),
            Some(Type::Result(..)) => Ok(("Except.rec".into(), vec!["error".into(), "ok".into()])),
            Some(ty @ Type::Named { .. }) => {
                use crate::codegen::common::{backend_named_type_key, backend_type_def_key};
                let key = backend_named_type_key(self.ctx, ty)
                    .ok_or("induct: the matched type has no name")?;
                // The Peano registry is keyed by the bare name, as the Lean
                // surface is flat.
                let bare = key.rsplit('.').next().unwrap_or(&key);
                if peano_type_named(self.ctx, bare).is_some() {
                    return Ok(("Nat.rec".into(), vec!["zero".into(), "succ".into()]));
                }
                let variants = self
                    .ctx
                    .type_defs
                    .iter()
                    .chain(self.ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
                    .find_map(|t| match t {
                        crate::ast::TypeDef::Sum { variants, .. }
                            if backend_type_def_key(self.ctx, t) == key =>
                        {
                            Some(variants)
                        }
                        _ => None,
                    })
                    .ok_or("induct: no definition of the matched type")?;
                Ok((
                    format!("{}.rec", type_to_lean(ty)),
                    variants
                        .iter()
                        .map(|w| super::syntax::lean_ctor_name(&w.name))
                        .collect(),
                ))
            }
            _ => Err("induct: the matched given is not of a list or a sum type".into()),
        }
    }

    /// The name of the Lean `induction` alternative for an arm's pattern,
    /// and which of its fields are of the matched type itself, each of
    /// which brings an induction hypothesis.
    fn constructor_fields(&self, pat: &ResolvedPattern) -> Result<(String, Vec<bool>), String> {
        use crate::codegen::proof_recognize::{PeanoCtor, peano_ctor_role};
        use crate::ir::hir::ResolvedCtor;
        match pat {
            ResolvedPattern::EmptyList => Ok(("nil".into(), vec![])),
            ResolvedPattern::Cons(_, _) => Ok(("cons".into(), vec![false, true])),
            ResolvedPattern::Ctor(ResolvedCtor::User { type_id, name, .. }, _) => {
                let type_name = self.ctx.symbol_table.type_entry(*type_id).key.name.clone();
                match peano_ctor_role(self.ctx, &type_name, name) {
                    Some(PeanoCtor::Zero) => return Ok(("zero".into(), vec![])),
                    Some(PeanoCtor::Succ) => return Ok(("succ".into(), vec![true])),
                    None => {}
                }
                let variant = self
                    .ctx
                    .type_defs
                    .iter()
                    .chain(self.ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
                    .find_map(|t| match t {
                        crate::ast::TypeDef::Sum {
                            name: n, variants, ..
                        } if *n == type_name => variants.iter().find(|w| w.name == *name),
                        _ => None,
                    })
                    .ok_or("induct: no definition of the matched type")?;
                let mentions = |f: &str| {
                    f.split(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
                        .any(|w| w == type_name)
                };
                if variant
                    .fields
                    .iter()
                    .any(|f| f != &type_name && mentions(f))
                {
                    return Err("induct: the type nests itself inside another type".into());
                }
                Ok((
                    super::syntax::lean_ctor_name(name),
                    variant.fields.iter().map(|f| *f == type_name).collect(),
                ))
            }
            _ => Err("induct: an arm is not one constructor".into()),
        }
    }

    /// Induction along a function that counts an Int toward zero:
    /// `AverSteps.int_measure_induct`, strong induction on the Int's
    /// `toNat`, split on the comparison the function matches on, with the
    /// claim for every value of the varied givens and under the carried
    /// hypotheses as its motive. Each case binds the given, the comparison's
    /// value, the claim at every Int whose `toNat` is smaller, the varied
    /// givens and the carried hypotheses; each hypothesis of a recursive
    /// call is that claim at the call's Int, which a prelude lemma shows
    /// smaller from the comparison (`n - 1` or `n / k` where `n > 0`).
    #[allow(clippy::too_many_arguments)]
    fn induct_toward_zero(
        &mut self,
        def: &crate::ir::proof_steps::Def,
        rec: &crate::ir::proof_steps::induct::Recursion,
        args: &[Term],
        v: &str,
        general: &[(usize, String)],
        arms: &[crate::ir::hir::ResolvedMatchArm],
        carried: &[String],
        cases: &[crate::ir::proof_steps::InductCase],
        eq: &Eqn,
        hyps: &Hyps,
    ) -> Result<String, String> {
        use crate::ir::proof_steps::induct::{self, Descent};
        let lean = super::syntax::aver_name_to_lean;
        let guard = rec
            .guard
            .as_ref()
            .ok_or("induct: not a count toward zero")?;
        let p = &def.params[rec.at];
        let varying: Vec<String> = std::iter::once(v.to_string())
            .chain(general.iter().map(|(_, g)| g.clone()))
            .collect();
        let mentions = |e: &Eqn| {
            let mut fv = Vec::new();
            term::free_vars(&e.lhs, &mut fv);
            term::free_vars(&e.rhs, &mut fv);
            fv.iter().any(|n| varying.contains(n))
        };
        let mut stated: Vec<(String, Eqn)> = Vec::new();
        for name in carried {
            let (_, e) = hyps
                .iter()
                .rev()
                .find(|(h, _)| h == name)
                .ok_or_else(|| format!("induct: {name} is not in scope"))?;
            stated.push((name.clone(), e.clone()));
        }
        let kept: Hyps = hyps.iter().filter(|(_, e)| !mentions(e)).cloned().collect();
        let gs: Vec<String> = general.iter().map(|(_, g)| lean(g)).collect();
        let hs: Vec<String> = carried.iter().map(|n| self.hyp_name(n)).collect();
        let mut motive = String::new();
        if !gs.is_empty() {
            motive.push_str(&format!("∀ {}, ", gs.join(" ")));
        }
        for (_, e) in &stated {
            motive.push_str(&format!("{} → ", self.eqn(e)));
        }
        motive.push_str(&self.eqn(eq));
        // The carried hypotheses as they stand outside, before the cases
        // rebind them as the equations of Bools the steps read.
        let outer: Vec<String> = carried
            .iter()
            .map(|n| self.proof(&Proof::Hyp(n.clone()), hyps))
            .collect::<Result<_, _>>()?;
        let all = format!("steps_below_{}", lean(v));
        let saved = self.prop_hyps.clone();
        self.prop_hyps.retain(|h| !carried.contains(h));
        for c in cases {
            self.prop_hyps.retain(|h| {
                !c.binders.contains(h)
                    && !c.ihs.contains(h)
                    && !c.more.iter().any(|(_, m)| &m.name == h)
            });
        }
        let rendered = (|| -> Result<(String, String, String), String> {
            let mut on = [String::new(), String::new()];
            let mut subject = String::new();
            for (arm, case) in arms.iter().zip(cases) {
                let stated_case = induct::case(
                    def,
                    rec,
                    args,
                    v,
                    general,
                    arm,
                    &case.binders,
                    &case.ihs,
                    &eq.lhs,
                    &eq.rhs,
                )?;
                let (g, at) = stated_case
                    .guard
                    .clone()
                    .ok_or("induct: a case without the comparison")?;
                let value =
                    term::bool_value(&at.rhs).ok_or("induct: the comparison has no value")?;
                subject = self.expr(&at.lhs);
                let mut scope = kept.clone();
                scope.push((g.clone(), at.clone()));
                scope.extend(stated.iter().cloned());
                let g_name = self.hyp_name(&g);
                let positive = if guard.stop {
                    format!("(AverSteps.pos_of_le_false {g_name})")
                } else {
                    format!("(AverSteps.pos_of_gt_true {g_name})")
                };
                let calls = induct::self_calls(&arm.body, def.fn_id);
                // The claim at the Int recursive call `k` passes and at
                // `values` of the varied givens, under the name `name`.
                let mut haves = String::new();
                let mut taken: Hyps = Vec::new();
                let mut instance = |this: &mut Self,
                                    k: usize,
                                    name: &str,
                                    values: &[Term],
                                    carry: &[Proof]|
                 -> Result<(), String> {
                    let (call, _) = calls.get(k).ok_or("induct: no such recursive call")?;
                    let below = match induct::descent(&call[rec.at], p) {
                        Some(Descent::Less) => format!("(AverSteps.sub_one_lt {positive})"),
                        Some(Descent::Divide(d)) => {
                            format!("(AverSteps.ediv_lt {d} (by decide) {positive})")
                        }
                        None => return Err("induct: a recursive call does not descend".into()),
                    };
                    let smaller = &stated_case.at[k][0].1;
                    let mut at = vec![(v.to_string(), smaller.clone())];
                    at.extend(
                        general
                            .iter()
                            .map(|(_, g)| g.clone())
                            .zip(values.iter().cloned()),
                    );
                    let e = Eqn::new(term::subst(&eq.lhs, &at)?, term::subst(&eq.rhs, &at)?);
                    let mut app = format!("{all} ({}) {below}", this.expr(smaller));
                    for t in values {
                        app.push_str(&format!(" ({})", this.expr(t)));
                    }
                    for q in carry {
                        app.push_str(&format!(" {}", this.proof(q, &scope)?));
                    }
                    // In parentheses: the arguments may sit on a line that
                    // starts left of the `have`.
                    haves.push_str(&format!(
                        "have {} : {} := ({app}); ",
                        this.hyp_name(name),
                        this.eqn(&e)
                    ));
                    taken.push((name.to_string(), e));
                    Ok(())
                };
                for (k, (ih, _)) in stated_case.ihs.iter().enumerate() {
                    if ih == "_" {
                        continue;
                    }
                    let values: Vec<Term> = stated_case.at[k][1..]
                        .iter()
                        .map(|(_, t)| t.clone())
                        .collect();
                    let carry = case.carry.get(k).cloned().unwrap_or_default();
                    instance(self, k, ih, &values, &carry)?;
                }
                for (k, more) in &case.more {
                    instance(self, *k, &more.name, &more.at, &more.carry)?;
                }
                let mut with_ihs = scope.clone();
                with_ihs.extend(taken);
                let body = self.proof(&case.proof, &with_ihs)?;
                let mut names = vec![
                    lean(v),
                    format!("({g_name} : {})", self.eqn(&at)),
                    all.clone(),
                ];
                names.extend(gs.iter().cloned());
                names.extend(hs.iter().cloned());
                on[usize::from(!value)] = format!("(fun {} => ({haves}{body}))", names.join(" "));
            }
            Ok((subject, on[0].clone(), on[1].clone()))
        })();
        self.prop_hyps = saved;
        let (subject, on_true, on_false) = rendered?;
        let mut applied: Vec<String> = gs.clone();
        applied.extend(outer);
        Ok(format!(
            "(AverSteps.int_measure_induct (P := fun {v} => {motive}) (fun {v} => {subject}) {on_true} {on_false} {v} {})",
            applied.join(" "),
            v = lean(v),
        ))
    }

    fn hyp_name(&self, name: &str) -> String {
        if name == "when" {
            "h_when".to_string()
        } else {
            name.to_string()
        }
    }

    fn concl(&self, p: &Proof, hyps: &Hyps) -> Result<Eqn, String> {
        claim(p, self.script, hyps)
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
                    Some(ty) => format!("({HOLE} : {})", super::types::term_type_to_lean(ty)),
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
                let (name, extra, unequal) = self.unfold_lemma(*fn_id, *arm, binders)?;
                let mut s = name;
                for a in args.iter().chain(&extra) {
                    s.push(' ');
                    s.push_str(&self.expr(a));
                }
                if let Some(p) = premise {
                    s.push(' ');
                    s.push_str(&self.proof(p, hyps)?);
                }
                // The hypotheses that rule out the earlier literal arms.
                for e in &unequal {
                    let lhs = term::canon(&e.lhs);
                    let (h, _) = hyps
                        .iter()
                        .rev()
                        .find(|(_, h)| {
                            term::canon(&h.lhs) == lhs && term::bool_value(&h.rhs) == Some(false)
                        })
                        .ok_or("unfold: no hypothesis rules out an earlier arm")?;
                    s.push(' ');
                    s.push_str(&self.hyp_name(h));
                }
                s
            }
            Proof::Arm {
                term: t,
                arm,
                binders,
                premise,
            } => {
                let ResolvedExpr::Match { subject, arms } = &t.node else {
                    return Err("arm: not a match".into());
                };
                let pat = &arms[*arm as usize - 1].pattern;
                let literal;
                let pat = match binders.as_slice() {
                    [v] if crate::ir::proof_steps::claim::is_catch_all(pat) => {
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
                // The evidence is read off the subject: a comparison is a
                // Prop in the model, so its Bool premise goes through
                // `decide`.
                let tactic = arm_tactic(pat, subject, "h_arm")?;
                format!("(fun h_arm => by {tactic}) {}", self.proof(premise, hyps)?)
            }
            // A `when` that is a bare comparison is stated `(a < b) = true`,
            // which Lean reads as an equation of Props; recover the Bool
            // equation the steps use.
            Proof::Hyp(name) if self.prop_hyps.contains(name) => format!(
                "decide_eq_true (of_eq_true ({}.trans (eq_self true)))",
                self.hyp_name(name)
            ),
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
            // `T.casesOn` with the motive `on = x → claim`, applied to `on`
            // and `rfl`: one function per constructor, over its fields and
            // the hypothesis that `on` is it.
            // A split on the arms of a literal match: one `bool_cases` on
            // `on == k` per literal, whose true branch is that literal's
            // case under `hyp : on = k` (from `eq_of_beq`) and whose false
            // branch names the catch-all case's hypothesis `(on == k) =
            // false`; the catch-all case is innermost.
            Proof::Split { on, hyp, cases, .. }
                if matches!(
                    cases.last().map(|c| &c.ctor),
                    Some(crate::ir::proof_steps::SplitCtor::Other)
                ) =>
            {
                let lean = super::syntax::aver_name_to_lean;
                let hss = crate::ir::proof_steps::split_hyps(on, hyp, cases);
                let (other, lits) = cases.split_last().ok_or("split: no cases")?;
                if other.binders.len() != lits.len() {
                    return Err("split: one hypothesis per literal".into());
                }
                let mut scope = hyps.clone();
                scope.extend(hss[lits.len()].iter().cloned());
                let mut acc = self.proof(&other.proof, &scope)?;
                let h = self.hyp_name(hyp);
                for (i, case) in lits.iter().enumerate().rev() {
                    let k = case
                        .value()
                        .ok_or("split: a literal case has its literal")?;
                    let mut scope = hyps.clone();
                    scope.extend(hss[i].iter().cloned());
                    let body = self.proof(&case.proof, &scope)?;
                    let cond = term::binop(BinOp::Eq, on.clone(), k.clone());
                    acc = format!(
                        "(AverSteps.bool_cases {} (fun {} => {acc}) (fun {h}_beq => (fun {h} => {body}) (AverSteps.eq_of_beq {} {} {h}_beq)))",
                        self.expr(&cond),
                        lean(&other.binders[i]),
                        self.expr(on),
                        self.expr(&k),
                    );
                }
                acc
            }
            Proof::Split { on, hyp, cases, .. } => {
                let (recursor, order) = self.recursor(on)?;
                let cases_on = recursor
                    .strip_suffix(".rec")
                    .ok_or("split: no recursor")?
                    .to_string()
                    + ".casesOn";
                if order.len() != cases.len() {
                    return Err("split: one case per constructor".into());
                }
                self.by_recursor = true;
                let lean = super::syntax::aver_name_to_lean;
                let h = self.hyp_name(hyp);
                let mut s = format!(
                    "({cases_on} (motive := fun steps_split => {} = steps_split → {}) ({})",
                    self.expr(on),
                    self.eqn(&eq),
                    self.expr(on)
                );
                for (case, hs) in cases
                    .iter()
                    .zip(crate::ir::proof_steps::split_hyps(on, hyp, cases))
                {
                    let mut scope = hyps.clone();
                    scope.extend(hs);
                    let body = self.proof(&case.proof, &scope)?;
                    let mut names: Vec<String> = case.binders.iter().map(|b| lean(b)).collect();
                    names.push(h.clone());
                    s.push_str(&format!(" (fun {} => {body})", names.join(" ")));
                }
                s.push_str(" rfl)");
                s
            }
            // `f.induct`, the functional induction principle Lean derives
            // from `f`'s own recursion, with the claim as its motive and
            // one explicit function per case: the parameters that are not
            // matched, the arm's pattern variables, then one hypothesis per
            // recursive call.
            Proof::Induct {
                fn_id,
                args,
                carried,
                cases,
                ..
            } => {
                let def = self.script.def(*fn_id).ok_or("induct: no definition")?;
                let rec = crate::ir::proof_steps::induct::recursion(def)?
                    .ok_or("induct: the function does not recurse")?;
                let j = rec.at;
                let (v, general) = crate::ir::proof_steps::induct::varied(
                    args,
                    j,
                    &self.script.obligation.givens,
                )?;
                let ResolvedExpr::Match { arms, .. } = &def.body.node else {
                    return Err("induct: the body is not a match".into());
                };
                if rec.guard.is_some() {
                    self.induct_toward_zero(
                        def, &rec, args, &v, &general, arms, carried, cases, &eq, hyps,
                    )?
                } else if crate::ir::proof_steps::induct::nested_split(def)
                    || !carried.is_empty()
                    || cases.iter().any(|c| !c.more.is_empty())
                {
                    self.induct_by_cases_of_type(
                        def, &rec, args, &v, &general, arms, carried, cases, &eq, hyps,
                    )?
                } else {
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
                        let ihs = crate::ir::proof_steps::induct::case(
                            def,
                            &rec,
                            args,
                            &v,
                            &general,
                            arm,
                            &case.binders,
                            &case.ihs,
                            &eq.lhs,
                            &eq.rhs,
                        )?
                        .ihs;
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
            }
            // `List.rec` with the claim, for every value of the generalised
            // givens, as its motive: the empty-list case, then the cell case
            // over the head, the tail and the claim at the tail, applied at
            // each hypothesis's values.
            Proof::InductList {
                var,
                nil,
                head,
                tail,
                general,
                ihs,
                cons,
                lhs,
                rhs,
            } => {
                let lean = super::syntax::aver_name_to_lean;
                let gs: Vec<String> = general.iter().map(|g| lean(g)).collect();
                let mut stated = Vec::new();
                for ih in ihs {
                    let mut at = vec![(var.clone(), term::var(tail))];
                    at.extend(general.iter().cloned().zip(ih.at.iter().cloned()));
                    let e = Eqn::new(term::subst(lhs, &at)?, term::subst(rhs, &at)?);
                    stated.push((ih.name.clone(), e));
                }
                let mut scope = hyps.clone();
                scope.extend(stated.iter().cloned());
                let base = self.proof(nil, hyps)?;
                let step = self.proof(cons, &scope)?;
                let all = format!("steps_all_{}", lean(tail));
                let mut haves = String::new();
                for (ih, (name, e)) in ihs.iter().zip(&stated) {
                    let mut app = all.clone();
                    for t in &ih.at {
                        app.push_str(&format!(" ({})", self.expr(t)));
                    }
                    haves.push_str(&format!(
                        "have {} : {} := ({app}); ",
                        self.hyp_name(name),
                        self.eqn(e)
                    ));
                }
                let quantified = if gs.is_empty() {
                    String::new()
                } else {
                    format!("∀ {}, ", gs.join(" "))
                };
                let bound = if gs.is_empty() {
                    String::new()
                } else {
                    format!("fun {} => ", gs.join(" "))
                };
                format!(
                    "(List.rec (motive := fun {v} => {quantified}{}) ({bound}{base}) (fun {} {} {all} => {bound}{haves}{step}) {v}{})",
                    self.eqn(&eq),
                    lean(head),
                    lean(tail),
                    gs.iter().map(|g| format!(" {g}")).collect::<String>(),
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
                // The comparison as the Prop `omega` proves. The kernel
                // written in Aver reads each product of givens as one atom
                // after normalising; `omega` does not multiply out, so a
                // product of givens goes to `grind`, which normalises the
                // ring first.
                let prop = emit_expr(goal, self.ctx);
                let close = "first | omega | grind";
                if *value {
                    format!("decide_eq_true (show {prop} from by {facts}{close})")
                } else {
                    format!("decide_eq_false (show ¬ ({prop}) from by {facts}{close})")
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
            // A cut: a term-mode `have` of the Bool equation, in scope for
            // the rest.
            Proof::Have {
                name,
                fact,
                proof: inner,
                body,
            } => {
                let stated = Eqn::new(fact.clone(), term::boolean(true));
                let pf = self.proof(inner, hyps)?;
                let mut scoped = hyps.clone();
                scoped.push((name.clone(), stated.clone()));
                let pb = self.proof(body, &scoped)?;
                // A fact that is a hypothesis already in scope is that
                // hypothesis: writing its type again would elaborate a
                // `match` in it afresh, generalised over the hypotheses
                // that mention its subject, which no longer fits.
                if let Proof::Hyp(h) = inner.as_ref()
                    && !self.prop_hyps.contains(h)
                {
                    return Ok(format!(
                        "(show {} from (have {} := {}; {pb}))",
                        self.eqn(&eq),
                        self.hyp_name(name),
                        self.hyp_name(h)
                    ));
                }
                format!(
                    "(have {} : {} := {pf}; {pb})",
                    self.hyp_name(name),
                    self.eqn(&stated)
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
    /// variables quantified), and an earlier literal arm `k` the value does
    /// not exclude by its shape is ruled out by a premise `(value == k) =
    /// false`; the returned terms are what the step passes after the
    /// arguments, and the equations those premises state at the value.
    fn unfold_lemma(
        &mut self,
        fn_id: FnId,
        arm: u32,
        chosen: &[Term],
    ) -> Result<(String, Vec<Term>, Vec<Eqn>), String> {
        let catch_all = self
            .script
            .def(fn_id)
            .and_then(|d| match &d.body.node {
                ResolvedExpr::Match { arms, .. } if arm > 0 => arms
                    .get(arm as usize - 1)
                    .map(|a| crate::ir::proof_steps::claim::is_catch_all(&a.pattern)),
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
        // The earlier literal arms the chosen value does not exclude.
        let ruled: Vec<Term> = match (
            catch_all,
            chosen,
            self.script.def(fn_id).map(|d| &d.body.node),
        ) {
            (true, [v], Some(ResolvedExpr::Match { arms, .. })) => arms[..arm as usize - 1]
                .iter()
                .filter(|a| !crate::ir::proof_steps::claim::excludes(&a.pattern, v))
                .map(|a| match &a.pattern {
                    ResolvedPattern::Literal(l) => {
                        let k = Spanned::bare(ResolvedExpr::Literal(l.clone()));
                        if let Some(ty) = v.ty() {
                            k.set_ty(ty.clone());
                        }
                        Ok(k)
                    }
                    _ => Err("unfold: an earlier arm can also match".to_string()),
                })
                .collect::<Result<_, _>>()?,
            _ => Vec::new(),
        };
        let unequal: Vec<Eqn> = match chosen {
            [v] => ruled
                .iter()
                .map(|k| crate::ir::proof_steps::unequal(v, k))
                .collect(),
            _ => Vec::new(),
        };
        if let Some(name) = self.unfolds.get(&(fn_id, arm, value_key.clone())) {
            return Ok((name.clone(), extra, unequal));
        }
        // The lemma's `match`es are stated without generalising: a `match`
        // on a pattern variable of the arm (`y0`) would otherwise take the
        // premise `h`, which mentions it, along, and no longer be the
        // definition's.
        let ctx = self.ctx;
        let _fixed = FixedMatches::new(&ctx.lean_match_fixed);
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
                super::types::fn_annotation_to_lean(ty, fn_id)
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
                term::pattern_parts(pat).len()
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
            if let Some(v) = &value {
                for (i, k) in ruled.iter().enumerate() {
                    let e = crate::ir::proof_steps::unequal(v, k);
                    binders.push_str(&format!(" (h_ne{i} : {})", self.eqn(&e)));
                    names.push(format!("h_ne{i}"));
                }
            }
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
                // Each earlier literal arm is refuted by its premise.
                _ if !ruled.is_empty() => "rw [h]; split <;> simp_all".to_string(),
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
        Ok((name, extra, unequal))
    }
}

/// The tactic that selects a known arm of the unfolded body, given
/// `h : subject = pattern`.
/// Every `match` emitted while this lives is stated with `(generalizing :=
/// false)`; the setting before it comes back when it is dropped.
struct FixedMatches<'c> {
    cell: &'c std::cell::Cell<bool>,
    previous: bool,
}

impl<'c> FixedMatches<'c> {
    fn new(cell: &'c std::cell::Cell<bool>) -> Self {
        let previous = cell.replace(true);
        FixedMatches { cell, previous }
    }
}

impl Drop for FixedMatches<'_> {
    fn drop(&mut self) {
        self.cell.set(self.previous);
    }
}

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
                // A connective as subject (`Bool.and(a, b == false)`)
                // normalises differently in `h` and in the goal.
                format!("(by first | (simp [{h}]; done) | simp_all)")
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
            prop_hyps: Vec::new(),
            by_recursor: false,
        };
        let ob = &script.obligation;
        let statement = r.eqn(&Eqn::new(ob.lhs.clone(), ob.rhs.clone()));
        let mut hyps = Hyps::new();
        let mut when = String::new();
        if let Some(p) = &ob.premise {
            // Stated as a law states its `when`: a bare comparison as a
            // Prop, anything else as a Bool.
            let stated = if is_prop_comparison(p) {
                format!("({})", emit_expr(&over_alpha(p), ctx))
            } else {
                r.expr(p)
            };
            when = format!(" (h_when : {stated} = true)");
            hyps.push(("when".into(), Eqn::new(p.clone(), term::boolean(true))));
            if is_prop_comparison(p) {
                r.prop_hyps.push("when".into());
            }
        }
        let term = r.proof(&script.proof, &hyps)?;
        if !r.support.is_empty() {
            return Err(format!("fact {}: a fact opens no definition", fact.key));
        }
        use crate::ir::proof_steps::facts::Sort;
        // Every element, key and value type: a key with the equality and
        // order the map model reads, never a fallback instance.
        let over_maps = fact
            .sorts
            .iter()
            .any(|(_, s)| matches!(s, Sort::Map | Sort::Key | Sort::Value));
        let types = if over_maps {
            "{α β : Type} [DecidableEq α] [AverKeyOrder α] [BEq α] [LawfulBEq α]".to_string()
        } else {
            "{α : Type}".to_string()
        };
        let binders: Vec<String> = fact
            .sorts
            .iter()
            .map(|(g, s)| {
                let ty = match s {
                    Sort::List => "List α",
                    Sort::Map => "List (α × β)",
                    Sort::Key | Sort::Elem => "α",
                    Sort::Value => "β",
                    Sort::Vector => "Array α",
                    Sort::Int => "Int",
                };
                format!("({} : {ty})", super::syntax::aver_name_to_lean(g))
            })
            .collect();
        out.push(format!(
            "set_option autoImplicit false in\ntheorem {} {types} {}{when} :\n    {statement} :=\n  {term}",
            fact_theorem(fact),
            binders.join(" ")
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
    render_with(script, &[], ctx)
}

/// One cut of a law's script: a reason `because<k>` (with `k` from 1),
/// or another cut in scope from where it stands on (a `when` line, an
/// opened hypothesis).
enum Cut<'a> {
    Reason(usize, &'a Term, &'a Proof),
    Other(&'a String, &'a Term, &'a Proof),
}

/// The cuts of a law's script in scope order, and the proof of the claim
/// under all of them.
fn reason_chain(p: &Proof) -> (Vec<Cut<'_>>, &Proof) {
    let mut cuts = Vec::new();
    let mut reasons = 0;
    let mut at = p;
    while let Proof::Have {
        name,
        fact,
        proof,
        body,
    } = at
    {
        if *name == format!("because{}", reasons + 1) {
            reasons += 1;
            cuts.push(Cut::Reason(reasons, fact, proof.as_ref()));
        } else {
            cuts.push(Cut::Other(name, fact, proof.as_ref()));
        }
        at = body;
    }
    (cuts, at)
}

/// Render the part of a law's script that proves one obligation of its
/// `because` chain, the way the Lean export states it: `index < n` is
/// `because<index+1>` and `index == n` the implication, after `intro` of
/// the givens, `h_reason0 … h_reason<index-1>` and `h_when`. The earlier
/// reasons are those hypotheses, not their proofs; the other cuts before
/// the obligation keep their proofs.
pub(crate) fn render_reason(
    script: &Script,
    index: usize,
    ctx: &CodegenContext,
) -> Result<Rendered, String> {
    let (cuts, claim_proof) = reason_chain(&script.proof);
    let reasons: Vec<&Term> = cuts
        .iter()
        .filter_map(|c| match c {
            Cut::Reason(_, fact, _) => Some(*fact),
            Cut::Other(..) => None,
        })
        .collect();
    if index > reasons.len() {
        return Err(format!(
            "the script proves {} reasons, not {}",
            reasons.len(),
            index
        ));
    }
    // The cuts before the obligation, and what it proves under them.
    let mut before = Vec::new();
    let mut proof = claim_proof.clone();
    for c in &cuts {
        match c {
            Cut::Reason(k, _, p) if *k == index + 1 => {
                proof = (*p).clone();
                break;
            }
            c => before.push(c),
        }
    }
    for c in before.into_iter().rev() {
        proof = match c {
            Cut::Reason(k, fact, _) => Proof::Have {
                name: format!("because{k}"),
                fact: (*fact).clone(),
                proof: Box::new(Proof::Hyp(format!("h_reason{}", k - 1))),
                body: Box::new(proof),
            },
            Cut::Other(name, fact, p) => Proof::Have {
                name: (*name).clone(),
                fact: (*fact).clone(),
                proof: Box::new((*p).clone()),
                body: Box::new(proof),
            },
        };
    }
    let mut part = script.clone();
    if index < reasons.len() {
        part.obligation.lhs = reasons[index].clone();
        part.obligation.rhs = term::boolean(true);
    }
    part.proof = proof;
    let earlier: Vec<(String, Term)> = (0..index)
        .map(|k| (format!("h_reason{k}"), reasons[k].clone()))
        .collect();
    render_with(&part, &earlier, ctx)
}

/// Render the script of one obligation of a `because` chain
/// ([`crate::ir::ObligationSteps`]) for its Lean theorem, after `intro` of
/// the givens, `h_reason0 … h_reason<index-1>` and `h_when`. The script
/// states what it assumes as one premise and takes it apart first; Lean
/// introduces the parts instead, so those cuts are left out.
pub(crate) fn render_obligation(
    script: &Script,
    index: usize,
    ctx: &CodegenContext,
) -> Result<Rendered, String> {
    let mut at = &script.proof;
    let mut guard = None;
    let mut earlier: Vec<(String, Term)> = Vec::new();
    let mut stripped = false;
    while let Proof::Have {
        name, fact, body, ..
    } = at
    {
        if name == "when" {
            guard = Some(fact.clone());
        } else if name.starts_with("h_reason") {
            earlier.push((name.clone(), fact.clone()));
        } else {
            break;
        }
        stripped = true;
        at = body;
    }
    if !stripped {
        guard = script.obligation.premise.clone();
    }
    if earlier.len() != index {
        return Err(format!(
            "the obligation assumes {} reasons, not {index}",
            earlier.len()
        ));
    }
    let mut part = script.clone();
    part.obligation.premise = guard;
    part.proof = at.clone();
    render_with(&part, &earlier, ctx)
}

/// [`render`], with `introduced` hypotheses (name, Bool term that is
/// `true`) in scope besides `h_when`.
fn render_with(
    script: &Script,
    introduced: &[(String, Term)],
    ctx: &CodegenContext,
) -> Result<Rendered, String> {
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
        prop_hyps: Vec::new(),
        by_recursor: false,
    };
    let mut hyps = Hyps::new();
    if let Some(p) = &script.obligation.premise {
        hyps.push(("when".into(), Eqn::new(p.clone(), term::boolean(true))));
        if is_prop_comparison(p) {
            r.prop_hyps.push("when".into());
        }
    }
    for (name, t) in introduced {
        hyps.push((name.clone(), Eqn::new(t.clone(), term::boolean(true))));
        if is_prop_comparison(t) {
            r.prop_hyps.push(name.clone());
        }
    }
    let saved = (r.laws.clone(), r.prop_hyps.clone());
    let mut term = r.proof(&script.proof, &hyps);
    // An induction by a type's recursor: every `match` of the proof,
    // the opened definitions' included, without generalising.
    if r.by_recursor {
        let previous = ctx.lean_match_fixed.replace(true);
        (r.laws, r.prop_hyps) = saved;
        r.unfolds.clear();
        r.support.clear();
        term = r.proof(&script.proof, &hyps);
        ctx.lean_match_fixed.set(previous);
    }
    let term = to_statement(&script.obligation, term?);
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
    let mut out = Vec::new();
    for t in &ctx.proof_ir.law_theorems {
        // Each obligation of a `because` chain that closed, on its own.
        for ob in &t.obligation_steps {
            if let Some(script) = &ob.script
                && let Ok(text) = crate::ir::proof_steps::sexpr::script(script, &ctx.symbol_table)
            {
                out.push((format!("proof_steps/{}.steps", ob.key), text));
            }
        }
        if let Some(script) = t.steps.as_ref() {
            if let Ok(text) = crate::ir::proof_steps::sexpr::script(script, &ctx.symbol_table) {
                out.push((format!("proof_steps/{}.steps", script.obligation.key), text));
            }
            continue;
        }
        let Some(why) = t.steps_refusal.as_ref() else {
            continue;
        };
        let key = crate::ir::proof_steps::sexpr::Names::fn_name(&ctx.symbol_table, t.fn_id);
        let mut text = format!("{why}\n");
        for hint in &t.steps_hints {
            text.push_str(&format!("hint: {hint}\n"));
        }
        out.push((format!("proof_steps/{key}.{}.refused", t.law_name), text));
    }
    out
}
