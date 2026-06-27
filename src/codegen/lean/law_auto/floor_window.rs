//! Lean renderer for the `FloorDivWindow` strategy family.
//!
//! Each figure renders a self-contained support stack (lemma names
//! prefixed with the law's theorem base, so two laws in one file
//! never collide) followed by the law theorem in TRUE universal form
//! — `∀ givens, <when> = true -> claim` with NO sampled-domain
//! disjunctions — proved from the stack. The templates were
//! validated end-to-end against the emitted artifacts on Lean 4.15
//! core (kernel-genuine: axioms within {propext, Classical.choice,
//! Quot.sound}; no `native_decide`, no `sorry`):
//!
//! - power algebra by functional induction over the well-founded
//!   power-of-two def (`<pow>.induct` + `<pow>.eq_def` + `omega`);
//! - the binary-exponent window characterization by functional
//!   induction over the floor-halving def, with the wrapper bridge
//!   `halve a = a / 2` discharged by `simp [halve,
//!   Except.withDefault]`;
//! - the floor bridges `Int.le_ediv_iff_mul_le` /
//!   `Int.ediv_lt_iff_lt_mul` (core, no Mathlib) to move window
//!   bounds through the literal-guarded division, plus the AC
//!   regroup `simp [Int.mul_comm, Int.mul_left_comm, Int.mul_assoc]`
//!   — scoped to this emission only (the permutational rewrites are
//!   never merged into shared simp sets).
//!
//! Because the rendered statement is universal, the caller marks the
//! law-class channel `universal` for this strategy (see the marker
//! site in `lean::toplevel`) — and the credit stays fail-closed: the
//! `#print axioms` whitelist still decides.

use super::AutoProof;
use super::aver_name_to_lean;
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::FloorWindowFigure;

/// Render the figure pinned on `(vb, law)`. Returns the complete
/// emission (support stack + universal theorem) as `support_lines`
/// with `replaces_theorem: true`.
pub(super) fn emit_floor_window_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let Some(crate::ir::ProofStrategy::FloorDivWindow { figure }) =
        super::law_strategy_for(ctx, &vb.fn_name, &law.name)
    else {
        return None;
    };
    let render = |e: &crate::ast::Spanned<crate::ast::Expr>| {
        super::super::expr::emit_expr_legacy(e, ctx, None)
    };
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = law.when.as_ref().map(render);
    let givens: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();

    let text = match &figure {
        FloorWindowFigure::PowPositive { pow_fn } => render_pow_positive(
            theorem_base,
            quant_params,
            &lhs,
            &rhs,
            &givens,
            &aver_name_to_lean(pow_fn),
        ),
        FloorWindowFigure::PowSumSplit { pow_fn } => render_pow_sum_split(
            theorem_base,
            quant_params,
            when.as_deref()?,
            &lhs,
            &rhs,
            &givens,
            &aver_name_to_lean(pow_fn),
        ),
        FloorWindowFigure::SigWindow {
            pow_fn,
            halve_fn,
            exp_fn,
            sig_fn,
            window_fn,
        } => render_sig_window(
            theorem_base,
            quant_params,
            when.as_deref()?,
            &lhs,
            &rhs,
            &givens,
            &aver_name_to_lean(pow_fn),
            &aver_name_to_lean(halve_fn),
            &aver_name_to_lean(exp_fn),
            &aver_name_to_lean(sig_fn),
            &aver_name_to_lean(window_fn),
        ),
        FloorWindowFigure::ProductWindow {
            pow_fn,
            fits_fn,
            claim_fn,
        } => render_product_window(
            theorem_base,
            quant_params,
            when.as_deref()?,
            &lhs,
            &rhs,
            &givens,
            &aver_name_to_lean(pow_fn),
            &aver_name_to_lean(fits_fn),
            &aver_name_to_lean(claim_fn),
        ),
        FloorWindowFigure::FloorPow2Window {
            pow_fn,
            floor_fn,
            window_fn,
        } => render_floor_pow2_window(
            theorem_base,
            quant_params,
            &lhs,
            &rhs,
            &givens,
            &aver_name_to_lean(pow_fn),
            &aver_name_to_lean(floor_fn),
            &aver_name_to_lean(window_fn),
        ),
    };
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

/// The generic **power-of-two-of-a-nonneg-linear-form normalizer**: a
/// self-contained support stack about a power-of-two function `pow`
/// (any [`is_pow2_shape`](super::super::proof_lower::floor_window) fn —
/// recognized by shape, not by name) that lets `grind` canonicalize
/// `pow` of an ARBITRARY nonneg linear form into a product of `pow` of
/// its atoms. Used by the keystone pool-composition rung when the cited
/// laws-as-lemmas pool reasons about such a `pow`.
///
/// It opens with the def equations and the sum homomorphism, then adds
/// the successor step `pow (n+1) = 2 * pow n`, and finally registers two
/// `grind_pattern`s that are the whole point:
///   * `__pow_add => pow m * pow n` — the homomorphism keyed on the
///     PRODUCT side. When `grind` sees any `pow p * pow q` it learns
///     `pow (p+q) = pow p * pow q`; its linear-arith normalizer then
///     identifies `p+q` with any other exponent equal to it (e.g.
///     `(w_a-1)+(w_b-1)` with `w_a+w_b-2`), so a regrouped/shifted
///     exponent unifies by congruence — the exact match `grind`'s
///     default LHS (`pow (m+n)`) pattern misses on a subtraction.
///   * `__pow_succ => pow (n+1)` — peels a `+1` constant shift, so an
///     odd offset (`w_a+w_b-1`, `e_a+e_b+1`) reduces to the even case.
///
/// `base` is the CONSUMER law's fresh prefix, so the stack and its
/// patterns are scoped to one emission — no global `grind_pattern`
/// collision, no dependence on the cited law's own support names.
pub(in crate::codegen::lean) fn pow2_linear_form_normalizer_support(
    base: &str,
    pow: &str,
) -> String {
    let equations = pow_equation_lemmas(base, pow);
    let add = pow_add_lemma(base, pow);
    format!(
        r#"{equations}
{add}
theorem {base}__pow_succ (n : Int) (h : 0 <= n) : {pow} (n + 1) = 2 * {pow} n := by
  rw [{base}__pow_of_pos (n + 1) (by omega), show n + 1 - 1 = n by omega]
grind_pattern {base}__pow_add => {pow} m * {pow} n
grind_pattern {base}__pow_succ => {pow} (n + 1)"#
    )
}

/// The power-of-two POSITIVITY support stack — the equation pair plus the
/// `<base>__pow_pos` functional-induction positivity theorem — scoped to a
/// fresh `base` prefix. The generic positivity fact (`0 < pow n` for every
/// integer `n`) the rational-over-floor sign/magnitude keystone arm supplies
/// as `have`s so its `aver_int_order` close can discharge the power-of-two
/// factor leaves. Shape-keyed on the `pow` fn (any `is_pow2_shape`), never a
/// per-figure template — it is the SAME positivity every floor-window figure
/// proves, lifted out for reuse.
pub(in crate::codegen::lean) fn pow_pos_support(base: &str, pow: &str) -> String {
    let equations = pow_equation_lemmas(base, pow);
    let pos = pow_pos_lemma(base, pow);
    format!("{equations}\n{pos}")
}

/// The power-of-two equation pair every figure's stack opens with:
/// `<base>__pow_of_nonpos` / `<base>__pow_of_pos`.
fn pow_equation_lemmas(base: &str, pow: &str) -> String {
    format!(
        r#"theorem {base}__pow_of_nonpos (n : Int) (h : n <= 0) : {pow} n = 1 := by
  rw [{pow}.eq_def, if_pos h]
theorem {base}__pow_of_pos (n : Int) (h : ¬n <= 0) : {pow} n = 2 * {pow} (n - 1) := by
  rw [{pow}.eq_def, if_neg h]"#
    )
}

/// `<base>__pow_pos` — positivity by functional induction.
fn pow_pos_lemma(base: &str, pow: &str) -> String {
    format!(
        r#"theorem {base}__pow_pos (n : Int) : 0 < {pow} n := by
  induction n using {pow}.induct with
  | case1 n h => rw [{base}__pow_of_nonpos n h]; omega
  | case2 n h ih => rw [{base}__pow_of_pos n h]; omega"#
    )
}

/// `<base>__pow_add` — the sum homomorphism by functional induction
/// on the first exponent.
fn pow_add_lemma(base: &str, pow: &str) -> String {
    format!(
        r#"theorem {base}__pow_add (m n : Int) (hn : 0 <= n) (hm : 0 <= m) :
    {pow} (m + n) = {pow} m * {pow} n := by
  induction m using {pow}.induct with
  | case1 m h =>
      have hm0 : m = 0 := Int.le_antisymm h hm
      subst hm0
      rw [show (0 : Int) + n = n by omega, {base}__pow_of_nonpos 0 (by omega)]
      omega
  | case2 m h ih =>
      rw [{base}__pow_of_pos (m + n) (by omega), {base}__pow_of_pos m (by omega),
          show m + n - 1 = (m - 1) + n by omega, ih (by omega)]
      rw [Int.mul_assoc]"#
    )
}

fn render_pow_positive(
    base: &str,
    quant_params: &str,
    lhs: &str,
    rhs: &str,
    givens: &[String],
    pow: &str,
) -> String {
    let g0 = &givens[0];
    let equations = pow_equation_lemmas(base, pow);
    let pos = pow_pos_lemma(base, pow);
    format!(
        r#"{equations}
{pos}
theorem {base} : ∀ {quant_params}, {lhs} = {rhs} := by
  intro {g0}
  have hpos := {base}__pow_pos {g0}
  simp only [eq_self_iff_true, eq_iff_iff, iff_true]
  omega"#
    )
}

fn render_pow_sum_split(
    base: &str,
    quant_params: &str,
    when: &str,
    lhs: &str,
    rhs: &str,
    givens: &[String],
    pow: &str,
) -> String {
    let (g0, g1) = (&givens[0], &givens[1]);
    let equations = pow_equation_lemmas(base, pow);
    let add = pow_add_lemma(base, pow);
    format!(
        r#"{equations}
{add}
theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = {rhs} := by
  intro {g0} {g1} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h_when
  exact {base}__pow_add {g0} {g1} h_when.2 h_when.1"#
    )
}

#[allow(clippy::too_many_arguments)]
fn render_sig_window(
    base: &str,
    quant_params: &str,
    when: &str,
    lhs: &str,
    rhs: &str,
    givens: &[String],
    pow: &str,
    halve: &str,
    exp: &str,
    sig: &str,
    window: &str,
) -> String {
    let (g0, g1, g2) = (&givens[0], &givens[1], &givens[2]);
    let equations = pow_equation_lemmas(base, pow);
    let pos = pow_pos_lemma(base, pow);
    let add = pow_add_lemma(base, pow);
    format!(
        r#"{equations}
{pos}
theorem {base}__pow_nonneg (n : Int) : 0 <= {pow} n := Int.le_of_lt ({base}__pow_pos n)
theorem {base}__pow_succ (n : Int) (hn : 0 <= n) : {pow} (n + 1) = 2 * {pow} n := by
  rw [{base}__pow_of_pos (n + 1) (by omega), show n + 1 - 1 = n by omega]
{add}
theorem {base}__halve_eq (a : Int) : {halve} a = a / 2 := by
  simp [{halve}, Except.withDefault]
theorem {base}__exp_of_low (a b : Int) (h : b < 1) : {exp} a b = 0 := by
  rw [{exp}.eq_def, if_pos h]
theorem {base}__exp_of_small (a b : Int) (h1 : ¬b < 1) (h2 : a < 2 * b) : {exp} a b = 0 := by
  rw [{exp}.eq_def, if_neg h1, if_pos h2]
theorem {base}__exp_of_big (a b : Int) (h1 : ¬b < 1) (h2 : ¬a < 2 * b) :
    {exp} a b = 1 + {exp} ({halve} a) b := by
  rw [{exp}.eq_def, if_neg h1, if_neg h2]
theorem {base}__exp_nonneg (a b : Int) : 0 <= {exp} a b := by
  induction a using {exp}.induct (b := b) with
  | case1 a h => rw [{base}__exp_of_low a b h]; omega
  | case2 a h1 h2 => rw [{base}__exp_of_small a b h1 h2]; omega
  | case3 a h1 h2 ih => rw [{base}__exp_of_big a b h1 h2]; omega
theorem {base}__exp_window : ∀ a b : Int, 1 <= b -> b <= a ->
    {pow} ({exp} a b) * b <= a ∧ a < {pow} ({exp} a b + 1) * b := by
  intro a b
  induction a using {exp}.induct (b := b) with
  | case1 a h =>
      intro hb _
      exact absurd hb (by omega)
  | case2 a h1 h2 =>
      intro hb hab
      rw [{base}__exp_of_small a b h1 h2]
      have hp1 : {pow} ((0 : Int) + 1) = 2 := by
        rw [show ((0 : Int) + 1) = 1 by omega, {base}__pow_of_pos 1 (by omega),
            show ((1 : Int) - 1) = 0 by omega, {base}__pow_of_nonpos 0 (by omega)]
        omega
      have hp0 : {pow} (0 : Int) = 1 := {base}__pow_of_nonpos 0 (by omega)
      constructor
      · rw [hp0]; omega
      · rw [hp1]; omega
  | case3 a h1 h2 ih =>
      intro hb hab
      have hhalf : {halve} a = a / 2 := {base}__halve_eq a
      have hba2 : b <= {halve} a := by rw [hhalf]; omega
      obtain ⟨ihlo, ihhi⟩ := ih hb hba2
      rw [{base}__exp_of_big a b h1 h2]
      have hE : 0 <= {exp} ({halve} a) b := {base}__exp_nonneg ({halve} a) b
      rw [hhalf] at ihlo ihhi
      constructor
      · rw [show (1 : Int) + {exp} ({halve} a) b = {exp} ({halve} a) b + 1 by omega,
            {base}__pow_succ ({exp} ({halve} a) b) hE, Int.mul_assoc]
        omega
      · rw [show (1 : Int) + {exp} ({halve} a) b + 1 = ({exp} ({halve} a) b + 1) + 1 by omega,
            {base}__pow_succ ({exp} ({halve} a) b + 1) (by omega), Int.mul_assoc]
        omega
theorem {base}__sig_window (a b n : Int) (hb : 1 <= b) (hab : b <= a) (hn : 1 <= n) :
    {pow} (n - 1) <= {sig} a b n ∧ {sig} a b n < {pow} n := by
  obtain ⟨hlo, hhi⟩ := {base}__exp_window a b hb hab
  have he0 : 0 <= {exp} a b := {base}__exp_nonneg a b
  have hbpos : (0 : Int) < b := by omega
  have hbz : ¬((b == 0) = true) := by simp only [beq_iff_eq]; omega
  simp only [{sig}]
  generalize he_def : {exp} a b = e
  rw [he_def] at hlo hhi he0
  by_cases hs : (n - 1) - e >= 0
  · rw [if_pos hs, if_neg hbz]
    simp only [Except.withDefault]
    constructor
    · rw [Int.le_ediv_iff_mul_le hbpos]
      have hmul : ({pow} e * b) * {pow} (n - 1 - e) <= a * {pow} (n - 1 - e) :=
        Int.mul_le_mul_of_nonneg_right hlo ({base}__pow_nonneg _)
      have hsplit : {pow} (e + (n - 1 - e)) = {pow} e * {pow} (n - 1 - e) :=
        {base}__pow_add e (n - 1 - e) (by omega) he0
      rw [show e + (n - 1 - e) = n - 1 by omega] at hsplit
      have hac : {pow} (n - 1) * b = ({pow} e * b) * {pow} (n - 1 - e) := by
        rw [hsplit]
        simp [Int.mul_comm, Int.mul_left_comm, Int.mul_assoc]
      rw [hac]
      exact hmul
    · rw [Int.ediv_lt_iff_lt_mul hbpos]
      have hmul : a * {pow} (n - 1 - e) < ({pow} (e + 1) * b) * {pow} (n - 1 - e) :=
        Int.mul_lt_mul_of_pos_right hhi ({base}__pow_pos _)
      have hsplit : {pow} ((e + 1) + (n - 1 - e)) = {pow} (e + 1) * {pow} (n - 1 - e) :=
        {base}__pow_add (e + 1) (n - 1 - e) (by omega) (by omega)
      rw [show (e + 1) + (n - 1 - e) = n by omega] at hsplit
      have hac : {pow} n * b = ({pow} (e + 1) * b) * {pow} (n - 1 - e) := by
        rw [hsplit]
        simp [Int.mul_comm, Int.mul_left_comm, Int.mul_assoc]
      rw [hac]
      exact hmul
  · have hdpos : (0 : Int) < b * {pow} (0 - (n - 1 - e)) :=
      Int.mul_pos hbpos ({base}__pow_pos _)
    have hdz : ¬((b * {pow} (0 - (n - 1 - e)) == 0) = true) := by
      simp only [beq_iff_eq]
      omega
    rw [if_neg hs, if_neg hdz]
    simp only [Except.withDefault]
    constructor
    · rw [Int.le_ediv_iff_mul_le hdpos]
      have hsplit : {pow} ((n - 1) + (0 - (n - 1 - e))) = {pow} (n - 1) * {pow} (0 - (n - 1 - e)) :=
        {base}__pow_add (n - 1) (0 - (n - 1 - e)) (by omega) (by omega)
      rw [show (n - 1) + (0 - (n - 1 - e)) = e by omega] at hsplit
      have hac : {pow} (n - 1) * (b * {pow} (0 - (n - 1 - e))) = {pow} e * b := by
        rw [hsplit]
        simp [Int.mul_comm, Int.mul_left_comm, Int.mul_assoc]
      rw [hac]
      exact hlo
    · rw [Int.ediv_lt_iff_lt_mul hdpos]
      have hsplit : {pow} (n + (0 - (n - 1 - e))) = {pow} n * {pow} (0 - (n - 1 - e)) :=
        {base}__pow_add n (0 - (n - 1 - e)) (by omega) (by omega)
      rw [show n + (0 - (n - 1 - e)) = e + 1 by omega] at hsplit
      have hac : {pow} n * (b * {pow} (0 - (n - 1 - e))) = {pow} (e + 1) * b := by
        rw [hsplit]
        simp [Int.mul_comm, Int.mul_left_comm, Int.mul_assoc]
      rw [hac]
      exact hhi
theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = {rhs} := by
  intro {g0} {g1} {g2} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h_when
  obtain ⟨⟨hw0, hw1⟩, hw2⟩ := h_when
  obtain ⟨hr0, hr1⟩ := {base}__sig_window {g0} {g1} {g2} (by omega) (by omega) (by omega)
  simp only [{window}, Bool.and_eq_true, decide_eq_true_eq]
  exact ⟨hr0, hr1⟩"#
    )
}

/// The recursive-expo-FREE Euclidean floor window over a power-of-two
/// divisor. Generic over the numerator `N` and exponent `E` inside the
/// window predicate: the support stack proves the floor characterization
/// for an ARBITRARY `(a, d)` with `0 < d` from the core ediv bridge, and
/// the main theorem discharges the predicate by `exact … _ _ (pow_pos _)`
/// — Lean infers `N` and `pow E` by unification, so the renderer never
/// names them. This is what makes one figure close both a bare-given
/// `floorDivWindow(a, k)` and a compound `truncFitsWindow(f, i)`.
#[allow(clippy::too_many_arguments)]
fn render_floor_pow2_window(
    base: &str,
    quant_params: &str,
    lhs: &str,
    rhs: &str,
    givens: &[String],
    pow: &str,
    floor: &str,
    window: &str,
) -> String {
    let equations = pow_equation_lemmas(base, pow);
    let pos = pow_pos_lemma(base, pow);
    let intro = givens.join(" ");
    format!(
        r#"{equations}
{pos}
theorem {base}__floordiv_eq (a d : Int) (hd : 0 < d) : {floor} a d = a / d := by
  have hne : ¬((d == 0) = true) := by simp only [beq_iff_eq]; omega
  simp only [{floor}]
  rw [if_neg hne]
  simp only [Except.withDefault]
theorem {base}__floor_window (a d : Int) (hd : 0 < d) :
    d * ({floor} a d) <= a ∧ a < d * ({floor} a d + 1) := by
  rw [{base}__floordiv_eq a d hd]
  have hd0 : d ≠ 0 := by omega
  have heq := Int.ediv_add_emod a d
  have h0 := Int.emod_nonneg a hd0
  have h1 := Int.emod_lt_of_pos a hd
  have hexp : d * (a / d + 1) = d * (a / d) + d := by rw [Int.mul_add, Int.mul_one]
  refine ⟨by omega, ?_⟩
  rw [hexp]; omega
theorem {base} : ∀ {quant_params}, {lhs} = {rhs} := by
  intro {intro}
  simp only [{window}, Bool.and_eq_true, decide_eq_true_eq]
  exact {base}__floor_window _ _ ({base}__pow_pos _)"#
    )
}

#[allow(clippy::too_many_arguments)]
fn render_product_window(
    base: &str,
    quant_params: &str,
    when: &str,
    lhs: &str,
    rhs: &str,
    givens: &[String],
    pow: &str,
    fits: &str,
    claim: &str,
) -> String {
    let (g0, g1, g2, g3) = (&givens[0], &givens[1], &givens[2], &givens[3]);
    let equations = pow_equation_lemmas(base, pow);
    let pos = pow_pos_lemma(base, pow);
    let add = pow_add_lemma(base, pow);
    format!(
        r#"{equations}
{pos}
theorem {base}__pow_nonneg (n : Int) : 0 <= {pow} n := Int.le_of_lt ({base}__pow_pos n)
{add}
theorem {base}__window_product (j k m n : Int)
    (hj1 : {pow} (m - 1) <= j) (hj2 : j < {pow} m)
    (hk1 : {pow} (n - 1) <= k) (hk2 : k < {pow} n) :
    {pow} (m + n - 2) <= j * k ∧ j * k < {pow} (m + n) := by
  have hjpos : 0 < j := Int.lt_of_lt_of_le ({base}__pow_pos _) hj1
  have hkpos : 0 < k := Int.lt_of_lt_of_le ({base}__pow_pos _) hk1
  have hm1 : 1 <= m := by
    by_cases hcon : m <= 0
    · have hpm : {pow} m = 1 := {base}__pow_of_nonpos m hcon
      rw [hpm] at hj2
      omega
    · omega
  have hn1 : 1 <= n := by
    by_cases hcon : n <= 0
    · have hpn : {pow} n = 1 := {base}__pow_of_nonpos n hcon
      rw [hpn] at hk2
      omega
    · omega
  constructor
  · have h := Int.mul_le_mul hj1 hk1 ({base}__pow_nonneg _) (Int.le_of_lt hjpos)
    have hsplit : {pow} ((m - 1) + (n - 1)) = {pow} (m - 1) * {pow} (n - 1) :=
      {base}__pow_add (m - 1) (n - 1) (by omega) (by omega)
    rw [show (m - 1) + (n - 1) = m + n - 2 by omega] at hsplit
    rw [hsplit]
    exact h
  · have h1 : j * k < {pow} m * k := Int.mul_lt_mul_of_pos_right hj2 hkpos
    have h2 : {pow} m * k < {pow} m * {pow} n := Int.mul_lt_mul_of_pos_left hk2 ({base}__pow_pos m)
    have hsplit : {pow} (m + n) = {pow} m * {pow} n :=
      {base}__pow_add m n (by omega) (by omega)
    rw [hsplit]
    exact Int.lt_of_lt_of_le h1 (Int.le_of_lt h2)
theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = {rhs} := by
  intro {g0} {g1} {g2} {g3} h_when
  simp only [{fits}, Bool.and_eq_true, decide_eq_true_eq] at h_when
  obtain ⟨⟨hw0, hw1⟩, hw2, hw3⟩ := h_when
  obtain ⟨hr0, hr1⟩ := {base}__window_product {g0} {g1} {g2} {g3} hw0 hw1 hw2 hw3
  simp only [{claim}, Bool.and_eq_true, decide_eq_true_eq]
  constructor
  · exact hr0
  · exact hr1"#
    )
}
