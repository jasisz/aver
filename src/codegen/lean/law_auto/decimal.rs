//! Lean renderer for [`crate::ir::ProofStrategy::IntDecimalRoundtrip`] —
//! the canonical decimal-Int parse/serialize roundtrip
//! (`examples/data/json.av` `parseNumber.fromIntRoundtrip`).
//!
//! The proof skeleton is ported VERBATIM from the verified json hand
//! proof (closed kernel-clean on Lean 4.15, #print axioms ⊆ [propext,
//! Classical.choice, Quot.sound]) with the fn names parameterized from
//! the IR pin:
//!
//! 1. `hts` — the serializer reduces to `String.fromInt n` by `rfl`
//!    (the mutual-SCC fuel is computable from the ADT measure of the
//!    constructor, so `rfl` forces it even for symbolic `n`), with a
//!    `simp` fallback;
//! 2. `hfin` — the finish leaf: full-range slice identity (inlined
//!    verbatim — the prelude's `String.slice_full` is `s.length`-keyed
//!    while this goal is `s.toList.length`-keyed) + the prelude's
//!    `Int.fromString_fromInt` roundtrip;
//! 3. `rcases n with Int.ofNat m | Int.negSucc m` sign split; zero
//!    sub-case computes by `rfl` on the closed string `"0"`;
//! 4. head-char dispatch via `String.ofList`-form rewrites (NOT simp under
//!    `getElem` — simp cannot discharge the getElem validity
//!    side-goal) and `split` + `digitChar` `Char.toString`
//!    disequalities for the dead `"-"` / `"0"` arms;
//! 5. the scanner's auto-synthesized `<fn>__fuel_scan` companion lemma
//!    (emitted at the scanner's def site by the SAME shape gate the
//!    detector validated, so the citation cannot dangle) runs the
//!    all-digit suffix to the end of the string;
//! 6. `exact hfin`.
//!
//! HONEST FLOOR: the whole script is one `first | (… done) | sorry`
//! alternative — a non-closing case degrades to a caught `sorry`,
//! never an unsolved-goals build error, and `native_decide` never
//! appears. The single support lemma (digit-pred: the predicate
//! accepts all ten `digitChar` images) carries its own per-goal
//! `first | decide | omega | sorry` floor for the same reason.

use super::super::expr::aver_name_to_lean;
use super::AutoProof;
use crate::ast::{Expr, Spanned, VerifyLaw};
use crate::codegen::CodegenContext;

/// Render the `IntDecimalRoundtrip` proof. Returns `None` (caller
/// falls through to the bare-sorry universal) only when the rendered
/// law text doesn't carry the serializer sub-term — a belt-and-braces
/// guard; the detector's gates make it unreachable today.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_int_decimal_roundtrip_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    parse_fn: &str,
    neg_fn: &str,
    pos_fn: &str,
    sign_fn: &str,
    scanner_fn: &str,
    predicate_fn: &str,
    finish_fn: &str,
    finish_int_fn: &str,
    serializer_fn: &str,
) -> Option<AutoProof> {
    let emit = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);

    // The detector validated `lhs = parse(ser(C(n)), 0)`.
    let Expr::FnCall(_, lhs_args) = &law.lhs.node else {
        return None;
    };
    let ser_arg = lhs_args.first()?;
    let n = aver_name_to_lean(&law.givens.first()?.name);
    let ser_text = emit(ser_arg);
    let rhs_text = emit(&law.rhs).replace('\n', " ");
    if !rhs_text.contains(&ser_text) {
        return None;
    }
    // Goal rhs after `rw [hts]`: every serializer occurrence becomes
    // `String.fromInt n`. Text-level replace is exact because both
    // renders come from the same emitter over the same ASTs.
    let rhs = rhs_text.replace(&ser_text, &format!("(String.fromInt {n})"));

    let parse = aver_name_to_lean(parse_fn);
    let neg = aver_name_to_lean(neg_fn);
    let posf = aver_name_to_lean(pos_fn);
    let sign = aver_name_to_lean(sign_fn);
    let scan = aver_name_to_lean(scanner_fn);
    let pred = aver_name_to_lean(predicate_fn);
    let finish = aver_name_to_lean(finish_fn);
    let fint = aver_name_to_lean(finish_int_fn);
    let scan_lemma = format!(
        "{}_scan",
        crate::codegen::recursion::fuel_helper_name(scanner_fn)
    );
    let pred_lemma = format!("{theorem_base}_digit_pred");

    // hts fallback simp set: serializer wrapper + its fuel/measure
    // helper names (probed from the actual proof-mode emission — a
    // serializer that graduated to native termination contributes its
    // wrapper name only, so no `unknown constant`).
    let mut ser_simp: Vec<String> = vec![aver_name_to_lean(serializer_fn)];
    for name in super::super::toplevel::law_fuel_simp_names(serializer_fn, ctx) {
        if !ser_simp.contains(&name) {
            ser_simp.push(name);
        }
    }
    let ser_simp = ser_simp.join(", ");

    let support_lines = vec![format!(
        r#"private theorem {pred_lemma} : ∀ d : Nat, d < 10 → {pred} (Char.toString (AverDigits.digitChar d)) = true := by
  intro d h
  rcases d with _|_|_|_|_|_|_|_|_|_|d
  all_goals first | decide | omega | sorry"#
    )];

    // One sign case's digits-scan tail (everything after the head-char
    // facts), shared by the positive and negative branches.
    let scan_tail = |case_int: &str, scan_pos: &str| -> String {
        format!(
            r#"have hfuel : averStringPosFuel (String.fromInt {case_int}) {scan_pos} 1
    = ((String.fromInt {case_int}).toList.length - (({scan_pos} : Int)).toNat) + 1 := by
  simp [averStringPosFuel]"#
        )
    };

    let positive = format!(
        r#"· by_cases hm : m = 0
  · subst hm
    have h0 : String.fromInt (Int.ofNat 0) = "0" := by
      show String.ofList (AverDigits.natDigitsChars 0) = "0"
      unfold AverDigits.natDigitsChars
      rw [AverDigits.natDigits.eq_1]
      decide
    rw [h0]
    rfl
  · have hsl : (String.fromInt (Int.ofNat m)).toList = (AverDigits.natDigits m).map AverDigits.digitChar := by
      show (String.ofList (AverDigits.natDigitsChars m)).toList = _
      rw [String.toList_ofList, AverDigits.natDigitsChars]
    rcases hnd : AverDigits.natDigits m with _ | ⟨d, ds⟩
    · exact absurd hnd (AverDigits.natDigits_nonempty m)
    · have hd10 : d < 10 := AverDigits.natDigits_digits_lt_ten m d (by rw [hnd]; exact List.mem_cons_self)
      have hdne0 : d ≠ 0 := AverDigits.natDigits_head_ne_zero m hm d ds hnd
      have hlen : (String.fromInt (Int.ofNat m)).toList.length = ds.length + 1 := by
        rw [hsl, hnd]; simp
      have hmk : String.fromInt (Int.ofNat m) = String.ofList ((d :: ds).map AverDigits.digitChar) := by
        show String.ofList (AverDigits.natDigitsChars m) = _
        rw [AverDigits.natDigitsChars, hnd]
      have hch : String.charAtAv (String.fromInt (Int.ofNat m)) 0
          = some (Char.toString (AverDigits.digitChar d)) := by
        rw [hmk]
        simp [String.charAtAv, String.toList_ofList]
      have hds10 : ∀ x ∈ ds, x < 10 := fun x hx =>
        AverDigits.natDigits_digits_lt_ten m x (by rw [hnd]; exact List.mem_cons_of_mem _ hx)
      have hdigits : ∀ ch ∈ (String.fromInt (Int.ofNat m)).toList.drop ((1 : Int)).toNat,
          {pred} (Char.toString ch) = true := by
        intro ch hc
        rw [hsl, hnd] at hc
        simp at hc
        rcases hc with ⟨x, hx, rfl⟩
        exact {pred_lemma} x (hds10 x hx)
      {hfuel_pos}
      have hdig : {pred} (Char.toString (AverDigits.digitChar d)) = true := {pred_lemma} d hd10
      have hdm : (AverDigits.digitChar d).toString ≠ "-" := AverDigits.digitChar_toString_ne_minus d hd10
      have hd0 : (AverDigits.digitChar d).toString ≠ "0" := AverDigits.digitChar_toString_ne_zero d hd10 hdne0
      have hred : {parse} (String.fromInt (Int.ofNat m)) 0
          = {scan} (String.fromInt (Int.ofNat m)) 1 0 false := by
        simp only [{parse}, hch, {posf}]
        split <;> rename_i heq <;>
          simp_all [Char.toString_eq_singleton, reduceCtorEq]
      rw [hred]
      simp only [{scan}]
      rw [{scan_lemma} (averStringPosFuel (String.fromInt (Int.ofNat m)) 1 1)
            (String.fromInt (Int.ofNat m)) 1 0 (by omega) (by omega)
            (by rw [hfuel]; omega) hdigits]
      exact hfin"#,
        hfuel_pos = indent_block(&scan_tail("(Int.ofNat m)", "1"), 6),
    );

    let negative = format!(
        r#"· have hsl : (String.fromInt (Int.negSucc m)).toList = '-' :: (AverDigits.natDigits (m + 1)).map AverDigits.digitChar := by
    show (String.ofList ('-' :: AverDigits.natDigitsChars (m + 1))).toList = _
    rw [String.toList_ofList, AverDigits.natDigitsChars]
  rcases hnd : AverDigits.natDigits (m + 1) with _ | ⟨d, ds⟩
  · exact absurd hnd (AverDigits.natDigits_nonempty (m + 1))
  · have hd10 : d < 10 := AverDigits.natDigits_digits_lt_ten (m + 1) d (by rw [hnd]; exact List.mem_cons_self)
    have hdne0 : d ≠ 0 := AverDigits.natDigits_head_ne_zero (m + 1) (by omega) d ds hnd
    have hlen : (String.fromInt (Int.negSucc m)).toList.length = ds.length + 2 := by
      rw [hsl, hnd]; simp
    have hch0 : String.charAtAv (String.fromInt (Int.negSucc m)) 0 = some "-" := by
      have h := String.charAt_eq_of_lt (String.fromInt (Int.negSucc m)) 0 (by omega) (by rw [hsl, hnd]; simp)
      simpa [hsl, show Char.toString '-' = "-" from rfl] using h
    have hch1 : String.charAtAv (String.fromInt (Int.negSucc m)) 1
        = some (Char.toString (AverDigits.digitChar d)) := by
      have h := String.charAt_eq_of_lt (String.fromInt (Int.negSucc m)) 1 (by omega) (by rw [hsl, hnd]; simp)
      simpa [hsl, hnd] using h
    have hds10 : ∀ x ∈ ds, x < 10 := fun x hx =>
      AverDigits.natDigits_digits_lt_ten (m + 1) x (by rw [hnd]; exact List.mem_cons_of_mem _ hx)
    have hdigits : ∀ ch ∈ (String.fromInt (Int.negSucc m)).toList.drop ((2 : Int)).toNat,
        {pred} (Char.toString ch) = true := by
      intro ch hc
      rw [hsl, hnd] at hc
      simp at hc
      rcases hc with ⟨x, hx, rfl⟩
      exact {pred_lemma} x (hds10 x hx)
    {hfuel_neg}
    have hdisp1 : {parse} (String.fromInt (Int.negSucc m)) 0
        = {neg} (String.fromInt (Int.negSucc m)) 1 0 := by
      simp only [{parse}]
      rw [hch0]
      rfl
    have hdig : {pred} (Char.toString (AverDigits.digitChar d)) = true := {pred_lemma} d hd10
    have hd0 : (AverDigits.digitChar d).toString ≠ "0" := AverDigits.digitChar_toString_ne_zero d hd10 hdne0
    rw [hdisp1]
    have hred : {neg} (String.fromInt (Int.negSucc m)) 1 0
        = {scan} (String.fromInt (Int.negSucc m)) 2 0 false := by
      simp only [{neg}, hch1, {sign}]
      split <;> rename_i heq <;>
        simp_all [Char.toString_eq_singleton, reduceCtorEq]
    rw [hred]
    simp only [{scan}]
    rw [{scan_lemma} (averStringPosFuel (String.fromInt (Int.negSucc m)) 2 1)
          (String.fromInt (Int.negSucc m)) 2 0 (by omega) (by omega)
          (by rw [hfuel]; omega) hdigits]
    exact hfin"#,
        hfuel_neg = indent_block(&scan_tail("(Int.negSucc m)", "2"), 4),
    );

    let inner = format!(
        r#"have hts : {ser_text} = String.fromInt {n} := by
  first
    | rfl
    | simp [{ser_simp}]
rw [hts]
have hfin : {finish} (String.fromInt {n}) 0 ((String.fromInt {n}).toList.length : Int) false
    = {rhs} := by
  have h1 : ¬ (((String.fromInt {n}).toList.length : Int) < 0) := by omega
  have hslice : String.sliceAv (String.fromInt {n}) 0 ((String.fromInt {n}).toList.length : Int) = String.fromInt {n} := by
    simp [String.sliceAv, h1]
  have hlen0 : (String.fromInt {n}).toList.length = (String.fromInt {n}).length := rfl
  have h2 : {finish} (String.fromInt {n}) 0 ((String.fromInt {n}).toList.length : Int) false
      = {fint} (String.sliceAv (String.fromInt {n}) 0 ((String.fromInt {n}).toList.length : Int)) ((String.fromInt {n}).toList.length : Int) := by
    simp [{finish}]
  rw [h2, hslice]
  simp [{fint}, Int.fromString_fromInt {n}, hlen0]
rcases {n} with m | m
{positive}
{negative}
done"#
    );

    // Wrap as one `first` alternative: a parenthesized tacticSeq whose
    // lines all sit at the column of its first tactic.
    let mut proof_lines = vec![format!("  intro {n}"), "  first".to_string()];
    for (i, line) in inner.lines().enumerate() {
        if i == 0 {
            proof_lines.push(format!("  | ({line}"));
        } else if line.is_empty() {
            proof_lines.push(String::new());
        } else {
            proof_lines.push(format!("     {line}"));
        }
    }
    let last = proof_lines.last_mut()?;
    last.push(')');
    proof_lines.push("  | sorry".to_string());

    Some(AutoProof {
        support_lines,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

/// Re-indent a multi-line block so it can be spliced at `spaces` depth
/// (the first line is spliced in place; later lines get the prefix).
fn indent_block(block: &str, spaces: usize) -> String {
    let pad = " ".repeat(spaces);
    block
        .lines()
        .enumerate()
        .map(|(i, l)| {
            if i == 0 || l.is_empty() {
                l.to_string()
            } else {
                format!("{pad}{l}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
