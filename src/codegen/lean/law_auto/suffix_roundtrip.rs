//! Lean renderer for [`crate::ir::ProofStrategy::StringEscapeRoundtrip`] —
//! the escaped-string parse/serialize roundtrip
//! (`examples/data/json.av` `escapeJsonString.parseStringRoundtrip` /
//! `parseStringChunk.escapedStringRoundtrip`).
//!
//! The proof skeleton is ported from the verified json hand proof
//! (closed kernel-clean on Lean 4.15, #print axioms = [propext,
//! Quot.sound]) with the fn names, constructor names, escape table and
//! literals parameterized from the IR pin. Structure, in emission
//! order (every generated name carries the law's `theorem_base` as a
//! prefix so two laws over the same pair — wrapper-entry and
//! scanner-entry — emit independent, collision-free stacks):
//!
//! 1. string/char data prelude (`str_ext`, `data_append`,
//!    `String.intercalate ""` algebra, …) — static text;
//! 2. suffix-cursor algebra in drop-form (`drop_succ_of_drop`,
//!    `charAt_of_drop`, `slice_snoc`) — the composition-friendly form
//!    of the shipped index-arithmetic scan lemmas;
//! 3. producer fold normalization: the acc-free view `escL` and the
//!    accumulator homomorphism `fold cs acc = acc ++ fold cs ""` (one
//!    induction generalizing `acc`);
//! 4. per-fuel-arm consumer step lemmas, one per validated match arm
//!    (`simp [<fn>__fuel, <arm hypotheses>]` each), incl. the
//!    `readHex` micro-stepper and the producer classifier's
//!    hypothesis-guarded unfolds;
//! 5. the chunk invariant `Inv cs` with the carried scanner state
//!    (`segmentStart`, `chunks`) universally quantified, closed by
//!    one branch lemma per producer char class (terminator-exit /
//!    two-char escape / hex control escape / printable passthrough)
//!    and a `by_cases` classification ladder driven by the pinned
//!    escape table;
//! 6. law glue: unfold the entry wrapper onto the scanner's fuel form
//!    (the `averStringPosFuel` rank is probed from the actual
//!    emission), instantiate the invariant at `pos = segStart = 1,
//!    chunks = []`, discharge fuel adequacy by `omega`.
//!
//! HONEST FLOOR: every synthesized theorem (support stack AND the law
//! proof) carries a `first | (…; done) | sorry` alternative — a
//! template regression degrades to caught honest sorries (the budget
//! pin flips loud-red), never an unsolved-goals build error, and
//! `native_decide` never appears in this path.

use super::super::expr::aver_name_to_lean;
use super::AutoProof;
use crate::ast::{Expr, Spanned, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::ir::StringEscapeRoundtripPin;

/// Lean escape for one char inside a literal. `in_string` selects
/// which quote needs escaping.
fn lean_escape_char(c: char, in_string: bool) -> String {
    match c {
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '"' if in_string => "\\\"".to_string(),
        '\'' if !in_string => "\\'".to_string(),
        c if (c as u32) < 0x20 || (c as u32) == 0x7f => format!("\\x{:02x}", c as u32),
        c => c.to_string(),
    }
}

/// Lean char literal (`'\\n'`).
fn lean_char(c: char) -> String {
    format!("'{}'", lean_escape_char(c, false))
}

/// Lean string literal (`"\\n"`).
fn lean_str(s: &str) -> String {
    let inner: String = s.chars().map(|c| lean_escape_char(c, true)).collect();
    format!("\"{inner}\"")
}

/// Source ctor name (`ParseResult.Ok`) → Lean ctor name
/// (`ParseResult.ok`) — Lean convention lowercases the variant leaf;
/// both segments pass through the reserved-token guard.
fn lean_ctor(name: &str) -> String {
    match name.rsplit_once('.') {
        Some((ty, variant)) => {
            format!(
                "{}.{}",
                crate::codegen::lean::syntax::aver_path_to_lean(ty),
                crate::codegen::lean::syntax::lean_ctor_name(variant)
            )
        }
        None => crate::codegen::lean::syntax::lean_ctor_name(name),
    }
}

/// Wrap a tactic block (lines at base indent 0) in the honest floor:
///
/// ```text
/// <decl> := by
///   first
///   | (<line 1>
///      <line 2>
///      …
///      done)
///   | sorry
/// ```
fn floored(decl: &str, body: &str) -> String {
    let mut out = String::new();
    out.push_str(decl);
    out.push_str(" := by\n  first\n");
    for (i, line) in body.lines().enumerate() {
        if i == 0 {
            out.push_str(&format!("  | ({line}\n"));
        } else if line.is_empty() {
            out.push('\n');
        } else {
            out.push_str(&format!("     {line}\n"));
        }
    }
    out.push_str("     done)\n  | sorry");
    out
}

/// Render the `StringEscapeRoundtrip` proof. Returns `None` (caller
/// falls through to the bare-sorry universal) when the probed
/// emission facts don't line up with the pin — the scanner SCC must
/// be fuel-emitted with a string-pos wrapper of rank >= 2 (the
/// invariant's fuel slope), and the producer fold must NOT be
/// fuel-emitted (the homomorphism unfolds its native equations).
pub(super) fn emit_string_escape_roundtrip_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    pin: &StringEscapeRoundtripPin,
) -> Option<AutoProof> {
    let emit = |e: &Spanned<Expr>| {
        super::super::expr::emit_expr(
            &super::super::expr::resolve_rewrite_output(e, ctx, None),
            ctx,
        )
    };

    // ---- law-side text (must byte-match the emitted statement) ------
    let Expr::FnCall(_, lhs_args) = &law.lhs.node else {
        return None;
    };
    let subject = lhs_args.first()?;
    let Expr::BinOp(crate::ast::BinOp::Add, open_prod, _term) = &subject.node else {
        return None;
    };
    let Expr::BinOp(crate::ast::BinOp::Add, _open, prod_call) = &open_prod.node else {
        return None;
    };
    let n = aver_name_to_lean(&law.givens.first()?.name);
    let subj = format!("({})", emit(subject));
    let prod_text = emit(prod_call);
    // Belt-and-braces: the rhs must carry the producer sub-term the
    // glue's position rewrite keys on.
    if !emit(&law.rhs).replace('\n', " ").contains(&prod_text) {
        return None;
    }

    // ---- emission facts (probed, not assumed) ------------------------
    // The scanner must be fuel-emitted with a string-pos wrapper; its
    // rank must cover the invariant's fuel slope (2 ticks per consumed
    // char). The producer fold must have native equations (no fuel).
    let rank = super::super::toplevel::law_string_pos_rank(&pin.scan_fn, ctx)?;
    if rank < 2 {
        return None;
    }
    for consumer in [
        &pin.scan_fn,
        &pin.escape_fn,
        &pin.validate_fn,
        &pin.unicode_fn,
        &pin.codepoint_fn,
        &pin.apply_fn,
        &pin.read_hex_fn,
    ] {
        if super::super::toplevel::law_fuel_simp_names(consumer, ctx).is_empty() {
            return None;
        }
    }
    if !super::super::toplevel::law_fuel_simp_names(&pin.fold_fn, ctx).is_empty() {
        return None;
    }

    // ---- names --------------------------------------------------------
    let p = theorem_base;
    let fuel = |f: &str| crate::codegen::recursion::fuel_helper_name(f);
    let scan_f = fuel(&pin.scan_fn);
    let esc_f = fuel(&pin.escape_fn);
    let val_f = fuel(&pin.validate_fn);
    let uni_f = fuel(&pin.unicode_fn);
    let cp_f = fuel(&pin.codepoint_fn);
    let app_f = fuel(&pin.apply_fn);
    let hex_f = fuel(&pin.read_hex_fn);
    let hex_w = aver_name_to_lean(&pin.read_hex_fn);
    let fin = aver_name_to_lean(&pin.finish_fn);
    let hexval = aver_name_to_lean(&pin.hex_val_fn);
    let hi_fn = aver_name_to_lean(&pin.high_surrogate_fn);
    let lo_fn = aver_name_to_lean(&pin.low_surrogate_fn);
    let prod = aver_name_to_lean(&pin.producer_fn);
    let fold = aver_name_to_lean(&pin.fold_fn);
    let cls = aver_name_to_lean(&pin.classifier_fn);
    let ctl = aver_name_to_lean(&pin.control_fn);
    let cce = aver_name_to_lean(&pin.control_escape_fn);
    let okc = lean_ctor(&pin.ok_ctor);
    let strc = lean_ctor(&pin.str_ctor);
    let term_c = lean_char(pin.terminator);
    let term_s = lean_str(&pin.terminator.to_string());
    let esc_c = lean_char(pin.escape_char);
    let esc_s = lean_str(&pin.escape_char.to_string());
    let uni_c = lean_char(pin.unicode_letter);
    let uni_s = lean_str(&pin.unicode_letter.to_string());
    let prefix_s = lean_str(&format!("{}{}00", pin.escape_char, pin.unicode_letter));
    let t = pin.control_threshold;
    let hi_min = pin.high_surrogate_min;
    let lo_min = pin.low_surrogate_min;

    // ---- escape-table orderings ----------------------------------------
    // 1-based index over the pin's pair vec (classifier arms in arm
    // order, then control-ladder arms in ladder order) names the
    // per-pair step lemmas and the by_cases hypotheses consistently.
    let pairs: Vec<(usize, &crate::ir::EscapePairSpec)> = pin
        .pairs
        .iter()
        .enumerate()
        .map(|(i, e)| (i + 1, e))
        .collect();
    let cls_pairs: Vec<&(usize, &crate::ir::EscapePairSpec)> = pairs
        .iter()
        .filter(|(_, e)| !e.from_control_ladder)
        .collect();
    let ladder_pairs: Vec<&(usize, &crate::ir::EscapePairSpec)> = pairs
        .iter()
        .filter(|(_, e)| e.from_control_ladder)
        .collect();
    let cls_low: Vec<&(usize, &crate::ir::EscapePairSpec)> = cls_pairs
        .iter()
        .filter(|(_, e)| (e.decoded as i64) < t)
        .copied()
        .collect();
    let cls_high: Vec<&(usize, &crate::ir::EscapePairSpec)> = cls_pairs
        .iter()
        .filter(|(_, e)| (e.decoded as i64) >= t)
        .copied()
        .collect();
    let idx_term = cls_pairs
        .iter()
        .find(|(_, e)| e.decoded == pin.terminator)?
        .0;
    let idx_esc = cls_pairs
        .iter()
        .find(|(_, e)| e.decoded == pin.escape_char)?
        .0;

    let mut support: Vec<String> = Vec::new();
    let mut push = |s: String| support.push(s);

    // ================= A. string data basics (static) =================
    push(floored(
        &format!("private theorem {p}_str_ext {{a b : String}} (h : a.toList = b.toList) : a = b"),
        "exact String.toList_injective h",
    ));
    push(floored(
        &format!(
            "private theorem {p}_data_append (a b : String) : (a ++ b).toList = a.toList ++ b.toList"
        ),
        "simp [String.toList_append]",
    ));
    push(floored(
        &format!(
            "private theorem {p}_data_add (a b : String) : (a + b).toList = a.toList ++ b.toList"
        ),
        "show (a ++ b).toList = a.toList ++ b.toList\nsimp [String.toList_append]",
    ));
    push(floored(
        &format!("private theorem {p}_empty_data : (\"\" : String).toList = ([] : List Char)"),
        "simp",
    ));
    push(floored(
        &format!("private theorem {p}_ts_data (c : Char) : (Char.toString c).toList = [c]"),
        "simp [Char.toString]",
    ));
    push(floored(
        &format!(
            "private theorem {p}_mk_cons (c : Char) (cs : List Char) :\n    Char.toString c ++ String.ofList cs = String.ofList (c :: cs)"
        ),
        &format!(
            "apply {p}_str_ext\nsimp [String.toList_append, String.toList_ofList, Char.toString]"
        ),
    ));
    push(floored(
        &format!("private theorem {p}_mk_data_self (s : String) : String.ofList s.toList = s"),
        "simp [String.ofList_toList]",
    ));
    push(floored(
        &format!("private theorem {p}_str_append_empty (s : String) : s ++ \"\" = s"),
        &format!("exact {p}_str_ext (by simp [{p}_data_append, {p}_empty_data])"),
    ));
    push(floored(
        &format!("private theorem {p}_str_empty_append (s : String) : \"\" ++ s = s"),
        &format!("exact {p}_str_ext (by simp [{p}_data_append, {p}_empty_data])"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_str_append_assoc (a b c : String) : (a ++ b) ++ c = a ++ (b ++ c)"
        ),
        &format!("exact {p}_str_ext (by simp [{p}_data_append, List.append_assoc])"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_toString_inj {{a b : Char}} (h : Char.toString a = Char.toString b) : a = b"
        ),
        &format!(
            "have h2 : (Char.toString a).toList = (Char.toString b).toList := by rw [h]\nsimpa [{p}_ts_data] using h2"
        ),
    ));
    push(format!(
        "private theorem {p}_ts_ne {{a b : Char}} (h : a ≠ b) : Char.toString a ≠ Char.toString b :=\n  fun hh => h ({p}_toString_inj hh)"
    ));
    push(floored(
        &format!(
            "private theorem {p}_toCode_toString (c : Char) : Option.getD (String.firstCodePointAv (Char.toString c)) 0 = (c.toNat : Int)"
        ),
        "simp [String.firstCodePointAv, Char.toString]",
    ));
    push(floored(
        &format!(
            "private theorem {p}_inter_snoc (xs : List String) (a : String) :\n    String.intercalate \"\" (xs ++ [a]) = String.intercalate \"\" xs ++ a"
        ),
        &format!(
            "induction xs with\n| nil => simp [String.intercalate_singleton, {p}_str_empty_append]\n| cons x xs ih =>\n    cases xs with\n    | nil =>\n        rw [List.cons_append, List.nil_append]\n        rw [String.intercalate_cons_cons, String.intercalate_singleton, String.intercalate_singleton]\n        simp [{p}_str_empty_append, String.append_assoc]\n    | cons y ys =>\n        have hne : (y :: ys) ≠ [] := by simp\n        have hne2 : ((y :: ys) ++ [a]) ≠ [] := by simp\n        rw [List.cons_append, String.intercalate_cons_of_ne_nil hne2,\n            String.intercalate_cons_of_ne_nil hne, ih]\n        simp [{p}_str_empty_append, String.append_assoc]"
        ),
    ));
    push(floored(
        &format!("private theorem {p}_inter_single (a : String) : String.intercalate \"\" [a] = a"),
        "simp [String.intercalate_singleton]",
    ));
    push(floored(
        &format!(
            "private theorem {p}_inter_nil : String.intercalate \"\" ([] : List String) = \"\""
        ),
        "simp [String.intercalate_nil]",
    ));
    push(floored(
        &format!(
            "private theorem {p}_char_eq_of_toNat {{c : Char}} {{m : Nat}} (h : c.toNat = m) : c = Char.ofNat m"
        ),
        "rw [← h, Char.ofNat_toNat]",
    ));

    // ================= B/C. drop-form suffix cursor (static) ==========
    push(floored(
        &format!(
            "private theorem {p}_getElem_of_drop : ∀ {{l : List Char}} {{m : Nat}} {{c : Char}} {{rest : List Char}},\n    l.drop m = c :: rest → l[m]? = some c"
        ),
        "intro l\ninduction l with\n| nil => intro m c rest h; simp at h\n| cons a l ih =>\n    intro m c rest h\n    cases m with\n    | zero =>\n        simp only [List.drop_zero] at h\n        injection h with h1 h2\n        simp [h1]\n    | succ m =>\n        simp only [List.drop_succ_cons] at h\n        simpa using ih h",
    ));
    push(floored(
        &format!(
            "private theorem {p}_drop_succ_of_drop {{l : List Char}} {{m : Nat}} {{c : Char}} {{rest : List Char}}\n    (h : l.drop m = c :: rest) : l.drop (m + 1) = rest"
        ),
        "have key : (l.drop m).drop 1 = l.drop (m + 1) := by rw [List.drop_drop]\nrw [← key, h]\nrfl",
    ));
    push(floored(
        &format!(
            "private theorem {p}_take_succ_of_drop : ∀ {{l : List Char}} {{m : Nat}} {{c : Char}} {{rest : List Char}},\n    l.drop m = c :: rest → l.take (m + 1) = l.take m ++ [c]"
        ),
        "intro l\ninduction l with\n| nil => intro m c rest h; simp at h\n| cons a l ih =>\n    intro m c rest h\n    cases m with\n    | zero =>\n        simp only [List.drop_zero] at h\n        injection h with h1 h2\n        simp [h1]\n    | succ m =>\n        simp only [List.drop_succ_cons] at h\n        simp [List.take_succ_cons, ih h]",
    ));
    push(floored(
        &format!(
            "private theorem {p}_charAt_of_drop {{s : String}} {{pos : Int}} {{c : Char}} {{rest : List Char}}\n    (h0 : 0 ≤ pos) (h : s.toList.drop pos.toNat = c :: rest) :\n    String.charAtAv s pos = some (Char.toString c)"
        ),
        &format!(
            "have hneg : ¬ pos < 0 := by omega\nsimp only [String.charAtAv, hneg, if_false]\nrw [{p}_getElem_of_drop h]\nrfl"
        ),
    ));
    push(floored(
        &format!(
            "private theorem {p}_slice_self (s : String) (a : Int) : String.sliceAv s a a = \"\""
        ),
        &format!("apply {p}_str_ext\nsimp [String.sliceAv, {p}_empty_data]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_slice_snoc {{s : String}} {{a b : Int}} {{c : Char}} {{rest : List Char}}\n    (h0 : 0 ≤ a) (hab : a ≤ b) (h : s.toList.drop b.toNat = c :: rest) :\n    String.sliceAv s a (b + 1) = String.sliceAv s a b ++ Char.toString c"
        ),
        &format!(
            "apply {p}_str_ext\nhave ha : ¬ a < 0 := by omega\nhave hb : ¬ b < 0 := by omega\nhave hb1 : ¬ b + 1 < 0 := by omega\nsimp [String.sliceAv, ha, hb, hb1, {p}_data_append, {p}_ts_data, String.toList_ofList]\nhave h1 : (b + 1).toNat - a.toNat = (b.toNat - a.toNat) + 1 := by omega\nrw [h1]\nhave h2 : (s.toList.drop a.toNat).drop (b.toNat - a.toNat) = c :: rest := by\n  rw [List.drop_drop]\n  have heq : a.toNat + (b.toNat - a.toNat) = b.toNat := by omega\n  rw [heq]\n  exact h\nexact {p}_take_succ_of_drop h2"
        ),
    ));

    // ================= D. producer fold normalization =================
    push(format!(
        "private def {p}_escL (cs : List Char) : String := {fold} (cs.map Char.toString) \"\""
    ));
    push(floored(
        &format!(
            "private theorem {p}_acc_hom (cs : List String) (acc : String) :\n    {fold} cs acc = acc ++ {fold} cs \"\""
        ),
        &format!(
            "induction cs generalizing acc with\n| nil => simp [{fold}, {p}_str_append_empty]\n| cons c rest ih =>\n    rw [{fold}, {fold}]\n    rw [ih (acc + {cls} c), ih (\"\" + {cls} c)]\n    apply {p}_str_ext\n    simp [{p}_data_append, {p}_data_add, {p}_empty_data, List.append_assoc]"
        ),
    ));
    push(floored(
        &format!("private theorem {p}_escL_nil : {p}_escL [] = \"\""),
        &format!("simp [{p}_escL, {fold}]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_escL_cons (c : Char) (cs : List Char) :\n    {p}_escL (c :: cs) = {cls} (Char.toString c) ++ {p}_escL cs"
        ),
        &format!(
            "unfold {p}_escL\nrw [List.map_cons, {fold}, {p}_acc_hom]\napply {p}_str_ext\nsimp [{p}_data_append, {p}_data_add, {p}_empty_data]"
        ),
    ));
    push(format!(
        "private theorem {p}_producer_eq (s : String) : {prod} s = {p}_escL s.toList := rfl"
    ));

    // ================= E. consumer step lemmas =========================
    push(floored(
        &format!(
            "private theorem {p}_step_finish {{s : String}} {{pos segStart : Int}} {{chunks : List String}} {{f : Nat}}\n    (hAt : String.charAtAv s pos = some {term_s}) :\n    {scan_f} (f + 1) s pos segStart chunks =\n      {fin} s (pos + 1) segStart chunks"
        ),
        &format!("simp [{scan_f}, hAt]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_step_escape {{s : String}} {{pos segStart : Int}} {{chunks : List String}} {{f : Nat}}\n    (hAt : String.charAtAv s pos = some {esc_s}) :\n    {scan_f} (f + 1) s pos segStart chunks =\n      {esc_f} f s (pos + 1) pos segStart chunks"
        ),
        &format!("simp [{scan_f}, hAt]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_step_default {{s : String}} {{pos segStart : Int}} {{chunks : List String}} {{f : Nat}} {{c : Char}}\n    (hAt : String.charAtAv s pos = some (Char.toString c))\n    (h1 : Char.toString c ≠ {term_s}) (h2 : Char.toString c ≠ {esc_s}) :\n    {scan_f} (f + 1) s pos segStart chunks =\n      {val_f} f s pos segStart chunks (Char.toString c)"
        ),
        &format!(
            "simp only [Char.toString_eq_singleton] at h1 h2\nsimp [{scan_f}, hAt, Char.toString_eq_singleton, h1, h2]"
        ),
    ));
    push(floored(
        &format!(
            "private theorem {p}_step_validate {{s : String}} {{pos segStart : Int}} {{chunks : List String}} {{f : Nat}} {{c : Char}}\n    (hge : ¬ ((c.toNat : Int) < {t})) :\n    {val_f} (f + 1) s pos segStart chunks (Char.toString c) =\n      {scan_f} f s (pos + 1) segStart chunks"
        ),
        &format!("simp only [{val_f}]\nrw [{p}_toCode_toString]\nsimp [hge]"),
    ));
    for (idx, pair) in &pairs {
        let letter_s = lean_str(&pair.letter.to_string());
        let dec_s = lean_str(&pair.decoded.to_string());
        push(floored(
            &format!(
                "private theorem {p}_step_esc_{idx} {{s : String}} {{pos slashPos segStart : Int}} {{chunks : List String}} {{f : Nat}}\n    (hAt : String.charAtAv s pos = some {letter_s}) :\n    {esc_f} (f + 1) s pos slashPos segStart chunks =\n      {scan_f} f s (pos + 1) (pos + 1)\n        ((chunks ++ [String.sliceAv s segStart slashPos]) ++ [{dec_s}])"
            ),
            &format!("simp [{esc_f}, hAt]"),
        ));
    }
    push(floored(
        &format!(
            "private theorem {p}_step_esc_uni {{s : String}} {{pos slashPos segStart : Int}} {{chunks : List String}} {{f : Nat}}\n    (hAt : String.charAtAv s pos = some {uni_s}) :\n    {esc_f} (f + 1) s pos slashPos segStart chunks =\n      {uni_f} f s (pos + 1) (pos + 1)\n        (chunks ++ [String.sliceAv s segStart slashPos])"
        ),
        &format!("simp [{esc_f}, hAt]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_step_unicode {{s : String}} {{pos escapePos : Int}} {{chunks : List String}} {{f : Nat}} {{cp : Int}}\n    (hread : {hex_w} s pos 0 0 = some cp) :\n    {uni_f} (f + 1) s pos escapePos chunks =\n      {cp_f} f s (pos + 4) escapePos chunks cp"
        ),
        &format!("simp [{uni_f}, hread]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_high_false {{cp : Int}} (h : ¬ cp ≥ {hi_min}) : {hi_fn} cp = false"
        ),
        &format!("simp [{hi_fn}, h]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_low_false {{cp : Int}} (h : ¬ cp ≥ {lo_min}) : {lo_fn} cp = false"
        ),
        &format!("simp [{lo_fn}, h]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_step_codepoint {{s : String}} {{pos escapePos : Int}} {{chunks : List String}} {{f : Nat}} {{cp : Int}}\n    (hh : {hi_fn} cp = false) (hl : {lo_fn} cp = false) :\n    {cp_f} (f + 1) s pos escapePos chunks cp =\n      {app_f} f s pos escapePos chunks cp"
        ),
        &format!("simp [{cp_f}, hh, hl]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_step_apply {{s : String}} {{pos escapePos : Int}} {{chunks : List String}} {{f : Nat}} {{cp : Int}} {{str : String}}\n    (hfc : String.fromCodePointAv cp = some str) :\n    {app_f} (f + 1) s pos escapePos chunks cp =\n      {scan_f} f s pos pos (chunks ++ [str])"
        ),
        &format!("simp [{app_f}, hfc]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_fromCodePoint_small {{m : Nat}} (h : m < 55296) :\n    String.fromCodePointAv (m : Int) = some (Char.toString (Char.ofNat m))"
        ),
        "have h1 : ¬ ((m : Int) < 0) := by omega\nhave h2 : ¬ ((m : Int) > 1114111) := by omega\nhave h3 : ¬ ((m : Int) ≥ 55296) := by omega\nsimp [String.fromCodePointAv, h1, h2, h3]",
    ));
    push(floored(
        &format!(
            "private theorem {p}_readhex_step {{fuel : Nat}} {{s : String}} {{pos acc count : Int}} {{c : Char}} {{rest : List Char}} {{d : Int}}\n    (hdrop : s.toList.drop pos.toNat = c :: rest)\n    (hv : {hexval} (Char.toString c) = some d)\n    (hcount : (count == 4) = false)\n    (h0 : 0 ≤ pos) :\n    {hex_f} (fuel + 1) s pos acc count =\n      {hex_f} fuel s (pos + 1) (acc * 16 + d) (count + 1)"
        ),
        &format!(
            "simp only [Char.toString_eq_singleton] at hv\nsimp [{hex_f}, hcount, {p}_charAt_of_drop h0 hdrop, Char.toString_eq_singleton, hv]"
        ),
    ));
    push(floored(
        &format!(
            "private theorem {p}_readhex_done {{fuel : Nat}} {{s : String}} {{pos acc : Int}} :\n    {hex_f} (fuel + 1) s pos acc 4 = some acc"
        ),
        &format!("simp [{hex_f}]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_hexDigit_data :\n    ∀ d : Nat, d < 16 → (Bytes.hexDigit (d : Int)).toList = [(Bytes.hexDigit (d : Int)).toList.head!]"
        ),
        "decide",
    ));
    push(floored(
        &format!(
            "private theorem {p}_hexVal_round :\n    ∀ d : Nat, d < 16 → {hexval} (Char.toString ((Bytes.hexDigit (d : Int)).toList.head!)) = some (d : Int)"
        ),
        "decide",
    ));

    // Producer classifier unfolds (hypothesis-guarded).
    let ladder_ne_shows: String = ladder_pairs
        .iter()
        .map(|(_, e)| {
            format!(
                "\n  show ¬ ((c.toNat : Int) = {}) from by omega,",
                e.decoded as u32
            )
        })
        .collect();
    push(floored(
        &format!(
            "private theorem {p}_control_print {{c : Char}} (hge : ¬ (c.toNat < {t})) :\n    {ctl} (Char.toString c) = Char.toString c"
        ),
        &format!(
            "simp only [{ctl}]\nrw [{p}_toCode_toString]\nsimp [{ladder_ne_shows}\n  show ¬ ((c.toNat : Int) < {t}) from by omega]"
        ),
    ));
    {
        let hyps: String = cls_pairs
            .iter()
            .map(|(i, e)| {
                format!(
                    " (h{i} : Char.toString c ≠ {})",
                    lean_str(&e.decoded.to_string())
                )
            })
            .collect();
        let at_hyps: String = cls_pairs.iter().map(|(i, _)| format!(" h{i}")).collect();
        let simp_hyps: String = cls_pairs.iter().map(|(i, _)| format!(", h{i}")).collect();
        push(floored(
            &format!(
                "private theorem {p}_classifier_default {{c : Char}}{hyps} :\n    {cls} (Char.toString c) = {ctl} (Char.toString c)"
            ),
            &format!(
                "simp only [Char.toString_eq_singleton] at{at_hyps}\nsimp [{cls}, Char.toString_eq_singleton{simp_hyps}]"
            ),
        ));
    }
    // The control escape's hex payload, definitionally. `Bytes.byteToHex`
    // is `hexDigit (v / 16) + hexDigit (v % 16)` after `Bytes.highNibble`
    // / `Bytes.lowNibble` unfold to the total literal-divisor intrinsics,
    // so the whole encoder chain collapses by `simp` for ANY octet — the
    // only side conditions are the `Bytes.fromList` octet bounds, both
    // omega-derivable from `c.toNat < T` with `T <= 256`.
    {
        let ladder_hyps: String = ladder_pairs
            .iter()
            .map(|(i, e)| format!(" (hk{i} : c.toNat ≠ {})", e.decoded as u32))
            .collect();
        push(floored(
            &format!(
                "private theorem {p}_control_u {{c : Char}} (h32 : c.toNat < {t}){ladder_hyps} :\n    {ctl} (Char.toString c) =\n      {prefix_s} + (Bytes.hexDigit ((c.toNat : Int) / 16) + Bytes.hexDigit ((c.toNat : Int) % 16))"
            ),
            &format!(
                "simp only [{ctl}]\nrw [{p}_toCode_toString]\nsimp [{ladder_ne_shows}\n  show ((c.toNat : Int) < {t}) from by omega,\n  {cce}, Bytes.fromList, Bytes.allInRange, Bytes.toHex, Bytes.octets, Bytes.hexParts, Bytes.byteToHex, Bytes.highNibble, Bytes.lowNibble,\n  show ((c.toNat : Int) ≥ 0) from by omega,\n  show ((c.toNat : Int) ≤ 255) from by omega]"
            ),
        ));
    }
    push(floored(
        &format!(
            "private theorem {p}_finish_shape (s : String) (pos segStart : Int) (chunks : List String) :\n    {fin} s (pos + 1) segStart chunks =\n      {okc}\n        ({strc} (String.intercalate \"\" (chunks ++ [String.sliceAv s segStart pos])))\n        (pos + 1)"
        ),
        &format!("simp [{fin}, Int.add_sub_cancel]"),
    ));
    push(floored(
        &format!(
            "private theorem {p}_ok_eq {{v1 v2 : String}} {{p1 p2 : Int}} (hv : v1 = v2) (hp : p1 = p2) :\n    {okc} ({strc} v1) p1 = {okc} ({strc} v2) p2"
        ),
        "rw [hv, hp]",
    ));

    // ================= F. the suffix invariant =========================
    push(format!(
        "private def {p}_inv (cs : List Char) : Prop :=\n  ∀ (s : String) (pos segStart : Int) (chunks : List String) (fuel : Nat),\n    0 ≤ segStart → segStart ≤ pos →\n    s.toList.drop pos.toNat = ({p}_escL cs).toList ++ [{term_c}] →\n    2 * ({p}_escL cs).toList.length + 2 ≤ fuel →\n    {scan_f} fuel s pos segStart chunks =\n      {okc}\n        ({strc} (String.intercalate \"\" (chunks ++ [String.sliceAv s segStart pos]) ++ String.ofList cs))\n        (pos + ({p}_escL cs).toList.length + 1)"
    ));
    push(floored(
        &format!("private theorem {p}_nil_branch : {p}_inv []"),
        &format!(
            "intro s pos segStart chunks fuel h0 hsp hdrop hfuel\nhave hpos : (0:Int) ≤ pos := by omega\nrw [{p}_escL_nil] at hdrop\nsimp only [{p}_empty_data, List.nil_append] at hdrop\nobtain ⟨f1, rfl⟩ : ∃ f1, fuel = f1 + 1 := ⟨fuel - 1, by omega⟩\nrw [{p}_step_finish ({p}_charAt_of_drop hpos hdrop)]\nrw [{p}_finish_shape]\napply {p}_ok_eq\n· rw [show String.ofList ([] : List Char) = \"\" from rfl, {p}_str_append_empty]\n· have hl0 : ({p}_escL ([] : List Char)).toList.length = 0 := by rw [{p}_escL_nil]; rfl\n  omega"
        ),
    ));
    push(floored(
        &format!(
            "private theorem {p}_pair_branch (c eL : Char) (dec : String) {{cs : List Char}}\n    (ih : {p}_inv cs)\n    (hesc : ({cls} (Char.toString c)).toList = [{esc_c}, eL])\n    (hdec : dec = Char.toString c)\n    (hstep : ∀ (s : String) (pos slashPos segStart : Int) (chunks : List String) (f : Nat),\n        String.charAtAv s pos = some (Char.toString eL) →\n        {esc_f} (f + 1) s pos slashPos segStart chunks =\n          {scan_f} f s (pos + 1) (pos + 1)\n            ((chunks ++ [String.sliceAv s segStart slashPos]) ++ [dec])) :\n    {p}_inv (c :: cs)"
        ),
        &format!(
            "intro s pos segStart chunks fuel h0 hsp hdrop hfuel\nhave hpos : (0:Int) ≤ pos := by omega\nhave hlen : ({p}_escL (c :: cs)).toList.length = 2 + ({p}_escL cs).toList.length := by\n  rw [{p}_escL_cons, {p}_data_append, List.length_append, hesc]\n  rfl\nrw [{p}_escL_cons, {p}_data_append, hesc] at hdrop\nsimp only [List.cons_append, List.nil_append, List.append_assoc] at hdrop\nhave hn1 : s.toList.drop (pos.toNat + 1) = eL :: (({p}_escL cs).toList ++ [{term_c}]) := {p}_drop_succ_of_drop hdrop\nhave hn2 : s.toList.drop (pos.toNat + 1 + 1) = ({p}_escL cs).toList ++ [{term_c}] := {p}_drop_succ_of_drop hn1\nhave hi1 : s.toList.drop (pos + 1).toNat = eL :: (({p}_escL cs).toList ++ [{term_c}]) := by\n  rw [show (pos + 1).toNat = pos.toNat + 1 from by omega]; exact hn1\nhave hi2 : s.toList.drop ((pos + 1) + 1).toNat = ({p}_escL cs).toList ++ [{term_c}] := by\n  rw [show ((pos + 1) + 1).toNat = pos.toNat + 1 + 1 from by omega]; exact hn2\nhave hAt0 : String.charAtAv s pos = some (Char.toString {esc_c}) := {p}_charAt_of_drop hpos hdrop\nhave hAt1 : String.charAtAv s (pos + 1) = some (Char.toString eL) := {p}_charAt_of_drop (by omega) hi1\nobtain ⟨f1, rfl⟩ : ∃ f1, fuel = f1 + 1 := ⟨fuel - 1, by omega⟩\nobtain ⟨f2, rfl⟩ : ∃ f2, f1 = f2 + 1 := ⟨f1 - 1, by omega⟩\nrw [{p}_step_escape hAt0]\nrw [hstep s (pos + 1) pos segStart chunks f2 hAt1]\nrw [ih s ((pos + 1) + 1) ((pos + 1) + 1) ((chunks ++ [String.sliceAv s segStart pos]) ++ [dec]) f2\n    (by omega) (by omega) hi2 (by omega)]\napply {p}_ok_eq\n· rw [{p}_slice_self, hdec]\n  simp only [{p}_inter_snoc, {p}_inter_nil, {p}_str_append_empty, {p}_str_empty_append, {p}_str_append_assoc, {p}_mk_cons]\n· omega"
        ),
    ));

    // printable branch — generated hypothesis bundle per classifier arm.
    {
        let hyp_params: String = cls_high
            .iter()
            .map(|(i, e)| format!(" (hq{i} : c ≠ {})", lean_char(e.decoded)))
            .collect();
        let mut body = String::new();
        body.push_str("intro s pos segStart chunks fuel h0 hsp hdrop hfuel\n");
        body.push_str("have hpos : (0:Int) ≤ pos := by omega\n");
        for (i, e) in &cls_low {
            body.push_str(&format!(
                "have hq{i} : c ≠ {} := fun h => by subst h; exact absurd h32 (by decide)\n",
                lean_char(e.decoded)
            ));
        }
        for (i, e) in &cls_pairs {
            body.push_str(&format!(
                "have e{i} : Char.toString c ≠ {} := {p}_ts_ne hq{i}\n",
                lean_str(&e.decoded.to_string())
            ));
        }
        let e_args: String = cls_pairs.iter().map(|(i, _)| format!(" e{i}")).collect();
        body.push_str(&format!(
            "have hesc : {cls} (Char.toString c) = Char.toString c := by\n  rw [{p}_classifier_default{e_args}, {p}_control_print h32]\n"
        ));
        body.push_str(&format!(
            "have hlen : ({p}_escL (c :: cs)).toList.length = 1 + ({p}_escL cs).toList.length := by\n  rw [{p}_escL_cons, hesc, {p}_data_append, List.length_append, {p}_ts_data]\n  rfl\n"
        ));
        body.push_str(&format!(
            "rw [{p}_escL_cons, hesc, {p}_data_append, {p}_ts_data] at hdrop\n"
        ));
        body.push_str(
            "simp only [List.cons_append, List.nil_append, List.append_assoc] at hdrop\n",
        );
        body.push_str(&format!(
            "have hn1 : s.toList.drop (pos.toNat + 1) = ({p}_escL cs).toList ++ [{term_c}] := {p}_drop_succ_of_drop hdrop\n"
        ));
        body.push_str(&format!(
            "have hi1 : s.toList.drop (pos + 1).toNat = ({p}_escL cs).toList ++ [{term_c}] := by\n  rw [show (pos + 1).toNat = pos.toNat + 1 from by omega]; exact hn1\n"
        ));
        body.push_str(&format!(
            "have hAt0 : String.charAtAv s pos = some (Char.toString c) := {p}_charAt_of_drop hpos hdrop\n"
        ));
        body.push_str("obtain ⟨f1, rfl⟩ : ∃ f1, fuel = f1 + 1 := ⟨fuel - 1, by omega⟩\n");
        body.push_str("obtain ⟨f2, rfl⟩ : ∃ f2, f1 = f2 + 1 := ⟨f1 - 1, by omega⟩\n");
        body.push_str(&format!(
            "rw [{p}_step_default hAt0 e{idx_term} e{idx_esc}]\n"
        ));
        body.push_str(&format!(
            "rw [{p}_step_validate (by omega : ¬ ((c.toNat : Int) < {t}))]\n"
        ));
        body.push_str("rw [ih s (pos + 1) segStart chunks f2 h0 (by omega) hi1 (by omega)]\n");
        body.push_str(&format!(
            "apply {p}_ok_eq\n· rw [{p}_slice_snoc h0 hsp hdrop]\n  simp only [{p}_inter_snoc, {p}_inter_nil, {p}_str_append_empty, {p}_str_empty_append, {p}_str_append_assoc, {p}_mk_cons]\n· omega"
        ));
        push(floored(
            &format!(
                "private theorem {p}_printable_branch {{c : Char}} {{cs : List Char}} (ih : {p}_inv cs)\n    (h32 : ¬ c.toNat < {t}){hyp_params} : {p}_inv (c :: cs)"
            ),
            &body,
        ));
    }

    // control branch — the hex-escape path (6 consumed chars, 5 scan
    // ticks + the separately-fueled readHex micro-run).
    {
        let ladder_params: String = ladder_pairs
            .iter()
            .map(|(i, e)| format!(" (hk{i} : c.toNat ≠ {})", e.decoded as u32))
            .collect();
        let low_params: String = cls_low
            .iter()
            .map(|(i, e)| format!(" (hq{i} : c ≠ {})", lean_char(e.decoded)))
            .collect();
        let mut body = String::new();
        body.push_str("intro s pos segStart chunks fuel h0 hsp hdrop hfuel\n");
        body.push_str("have hpos : (0:Int) ≤ pos := by omega\n");
        for (i, e) in &cls_high {
            body.push_str(&format!(
                "have hq{i} : c ≠ {} := fun h => by subst h; exact absurd h32 (by decide)\n",
                lean_char(e.decoded)
            ));
        }
        for (i, e) in &cls_pairs {
            body.push_str(&format!(
                "have e{i} : Char.toString c ≠ {} := {p}_ts_ne hq{i}\n",
                lean_str(&e.decoded.to_string())
            ));
        }
        body.push_str(
            "have hdiv : ((c.toNat : Int)) / 16 = ((c.toNat / 16 : Nat) : Int) := by omega\n",
        );
        body.push_str(
            "have hmod : ((c.toNat : Int)) % 16 = ((c.toNat % 16 : Nat) : Int) := by omega\n",
        );
        let e_args: String = cls_pairs.iter().map(|(i, _)| format!(" e{i}")).collect();
        let hk_args: String = ladder_pairs
            .iter()
            .map(|(i, _)| format!(" hk{i}"))
            .collect();
        body.push_str(&format!(
            "have hesc : {cls} (Char.toString c) =\n    {prefix_s} + (Bytes.hexDigit ((c.toNat / 16 : Nat) : Int) + Bytes.hexDigit ((c.toNat % 16 : Nat) : Int)) := by\n  rw [{p}_classifier_default{e_args}, {p}_control_u h32{hk_args}, hdiv, hmod]\n"
        ));
        body.push_str(&format!(
            "have hA := {p}_hexDigit_data (c.toNat / 16) (by omega)\n"
        ));
        body.push_str(&format!(
            "have hB := {p}_hexDigit_data (c.toNat % 16) (by omega)\n"
        ));
        body.push_str(&format!(
            "have hdataE : ({cls} (Char.toString c)).toList\n    = {esc_c} :: {uni_c} :: '0' :: '0' ::\n      (Bytes.hexDigit ((c.toNat / 16 : Nat) : Int)).toList.head! ::\n      (Bytes.hexDigit ((c.toNat % 16 : Nat) : Int)).toList.head! :: [] := by\n  rw [hesc, {p}_data_add, {p}_data_add, hA, hB]\n  rfl\n"
        ));
        body.push_str(&format!(
            "have hlen : ({p}_escL (c :: cs)).toList.length = 6 + ({p}_escL cs).toList.length := by\n  rw [{p}_escL_cons, {p}_data_append, List.length_append, hdataE]\n  rfl\n"
        ));
        body.push_str(&format!(
            "rw [{p}_escL_cons, {p}_data_append, hdataE] at hdrop\n"
        ));
        body.push_str(
            "simp only [List.cons_append, List.nil_append, List.append_assoc] at hdrop\n",
        );
        for k in 1..=6 {
            let prev = if k == 1 {
                "hdrop".to_string()
            } else {
                format!("hn{}", k - 1)
            };
            body.push_str(&format!("have hn{k} := {p}_drop_succ_of_drop {prev}\n"));
        }
        let hex_a = "(Bytes.hexDigit ((c.toNat / 16 : Nat) : Int)).toList.head!";
        let hex_b = "(Bytes.hexDigit ((c.toNat % 16 : Nat) : Int)).toList.head!";
        let tail = format!("(({p}_escL cs).toList ++ [{term_c}])");
        let suffixes: [String; 5] = [
            format!("{uni_c} :: '0' :: '0' :: {hex_a} :: {hex_b} :: {tail}"),
            format!("'0' :: '0' :: {hex_a} :: {hex_b} :: {tail}"),
            format!("'0' :: {hex_a} :: {hex_b} :: {tail}"),
            format!("{hex_a} :: {hex_b} :: {tail}"),
            format!("{hex_b} :: {tail}"),
        ];
        let pos_forms: [&str; 5] = [
            "(pos + 1)",
            "((pos + 1) + 1)",
            "(((pos + 1) + 1) + 1)",
            "((((pos + 1) + 1) + 1) + 1)",
            "(((((pos + 1) + 1) + 1) + 1) + 1)",
        ];
        for k in 1..=5usize {
            let nat_form = (0..k).map(|_| " + 1").collect::<String>();
            body.push_str(&format!(
                "have hi{k} : s.toList.drop {}.toNat = {} := by\n  rw [show {}.toNat = pos.toNat{nat_form} from by omega]; exact hn{k}\n",
                pos_forms[k - 1],
                suffixes[k - 1],
                pos_forms[k - 1],
            ));
        }
        body.push_str(&format!(
            "have hi6 : s.toList.drop (((pos + 1) + 1) + 4).toNat = ({p}_escL cs).toList ++ [{term_c}] := by\n  rw [show (((pos + 1) + 1) + 4).toNat = pos.toNat + 1 + 1 + 1 + 1 + 1 + 1 from by omega]; exact hn6\n"
        ));
        body.push_str(&format!(
            "have hAt0 : String.charAtAv s pos = some (Char.toString {esc_c}) := {p}_charAt_of_drop hpos hdrop\n"
        ));
        body.push_str(&format!(
            "have hAt1 : String.charAtAv s (pos + 1) = some (Char.toString {uni_c}) := {p}_charAt_of_drop (by omega) hi1\n"
        ));
        for k in 1..=5 {
            let prev = if k == 1 {
                "fuel".to_string()
            } else {
                format!("f{}", k - 1)
            };
            body.push_str(&format!(
                "obtain ⟨f{k}, rfl⟩ : ∃ f{k}, {prev} = f{k} + 1 := ⟨{prev} - 1, by omega⟩\n"
            ));
        }
        body.push_str(&format!("rw [{p}_step_escape hAt0]\n"));
        body.push_str(&format!("rw [{p}_step_esc_uni hAt1]\n"));
        body.push_str(&format!(
            "have hread : {hex_w} s ((pos + 1) + 1) 0 0 = some ((c.toNat : Int)) := by\n  unfold {hex_w}\n  rw [{p}_readhex_step hi2 (by decide : {hexval} (Char.toString '0') = some 0)\n      (by decide) (by omega : (0:Int) ≤ (pos + 1) + 1)]\n  rw [show Int.natAbs (4 - (0:Int)) = 3 + 1 from by decide]\n  rw [show ((0:Int) + 1) = 1 from by decide]\n  rw [{p}_readhex_step hi3 (by decide : {hexval} (Char.toString '0') = some 0)\n      (by decide) (by omega : (0:Int) ≤ ((pos + 1) + 1) + 1)]\n  rw [show ((3:Nat)) = 2 + 1 from by decide]\n  rw [show ((1:Int) + 1) = 2 from by decide]\n  rw [{p}_readhex_step hi4 ({p}_hexVal_round (c.toNat / 16) (by omega))\n      (by decide) (by omega : (0:Int) ≤ (((pos + 1) + 1) + 1) + 1)]\n  rw [show ((2:Nat)) = 1 + 1 from by decide]\n  rw [show ((2:Int) + 1) = 3 from by decide]\n  rw [{p}_readhex_step hi5 ({p}_hexVal_round (c.toNat % 16) (by omega))\n      (by decide) (by omega : (0:Int) ≤ ((((pos + 1) + 1) + 1) + 1) + 1)]\n  rw [show ((1:Nat)) = 0 + 1 from by decide]\n  rw [show ((3:Int) + 1) = 4 from by decide]\n  rw [{p}_readhex_done]\n  congr 1\n  omega\n"
        ));
        body.push_str(&format!("rw [{p}_step_unicode hread]\n"));
        body.push_str(&format!(
            "rw [{p}_step_codepoint ({p}_high_false (by omega)) ({p}_low_false (by omega))]\n"
        ));
        body.push_str(&format!(
            "rw [{p}_step_apply ({p}_fromCodePoint_small (by omega : c.toNat < 55296))]\n"
        ));
        body.push_str("rw [Char.ofNat_toNat]\n");
        body.push_str(
            "rw [ih s (((pos + 1) + 1) + 4) (((pos + 1) + 1) + 4)\n    ((chunks ++ [String.sliceAv s segStart pos]) ++ [Char.toString c]) f5\n    (by omega) (by omega) hi6 (by omega)]\n",
        );
        body.push_str(&format!(
            "apply {p}_ok_eq\n· rw [{p}_slice_self]\n  simp only [{p}_inter_snoc, {p}_inter_nil, {p}_str_append_empty, {p}_str_empty_append, {p}_str_append_assoc, {p}_mk_cons]\n· omega"
        ));
        // TEST-ONLY (`AVER_PROOF_ESCAPE_SABOTAGE`): replace the
        // control branch's tactic block with a no-op so the floor's
        // behaviour is observable end-to-end — the branch theorem
        // falls to its caught `sorry` (budget pin flips loud-red,
        // `lake build` still SUCCEEDS), proving a template regression
        // can never be a hard build error. Mirrors the quarantine
        // lane's `AVER_PROOF_LANE_SABOTAGE` hook.
        let body = if std::env::var("AVER_PROOF_ESCAPE_SABOTAGE")
            .map(|v| !v.is_empty())
            .unwrap_or(false)
        {
            "skip".to_string()
        } else {
            body
        };
        push(floored(
            &format!(
                "private theorem {p}_control_branch {{c : Char}} {{cs : List Char}} (ih : {p}_inv cs)\n    (h32 : c.toNat < {t}){ladder_params}{low_params} : {p}_inv (c :: cs)"
            ),
            &body,
        ));
    }

    // chunk invariant — the by_cases classification ladder over the
    // pinned escape table.
    {
        let mut body = String::new();
        body.push_str("intro cs\n");
        body.push_str("induction cs with\n");
        body.push_str(&format!("| nil => exact {p}_nil_branch\n"));
        body.push_str("| cons c cs ih =>\n");
        for (i, e) in &cls_pairs {
            body.push_str(&format!(
                "    by_cases hq{i} : c = {}\n    · subst hq{i}\n      exact {p}_pair_branch {} {} {} ih (by decide) rfl\n        (fun _ _ _ _ _ _ hAt => {p}_step_esc_{i} hAt)\n",
                lean_char(e.decoded),
                lean_char(e.decoded),
                lean_char(e.letter),
                lean_str(&e.decoded.to_string()),
            ));
        }
        for (i, e) in &ladder_pairs {
            body.push_str(&format!(
                "    by_cases hk{i} : c.toNat = {}\n    · have hc : c = {} := by rw [{p}_char_eq_of_toNat hk{i}]\n      subst hc\n      exact {p}_pair_branch {} {} {} ih (by decide) rfl\n        (fun _ _ _ _ _ _ hAt => {p}_step_esc_{i} hAt)\n",
                e.decoded as u32,
                lean_char(e.decoded),
                lean_char(e.decoded),
                lean_char(e.letter),
                lean_str(&e.decoded.to_string()),
            ));
        }
        let hk_args: String = ladder_pairs
            .iter()
            .map(|(i, _)| format!(" hk{i}"))
            .collect();
        let low_args: String = cls_low.iter().map(|(i, _)| format!(" hq{i}")).collect();
        let high_args: String = cls_high.iter().map(|(i, _)| format!(" hq{i}")).collect();
        body.push_str(&format!(
            "    by_cases hctl : c.toNat < {t}\n    · exact {p}_control_branch ih hctl{hk_args}{low_args}\n    · exact {p}_printable_branch ih hctl{high_args}"
        ));
        push(floored(
            &format!("private theorem {p}_chunk_inv : ∀ cs : List Char, {p}_inv cs"),
            &body,
        ));
    }

    // ================= G. law glue (the proof body) ====================
    let glue = format!(
        "have hdata : {subj}.toList = {open_c} :: (({p}_escL {n}.toList).toList ++ [{term_c}]) := by\n  rw [{p}_data_add, {p}_data_add, {p}_producer_eq]\n  rfl\nshow {scan_f} (averStringPosFuel {subj} 1 {rank}) {subj} 1 1 [] = _\nrw [{p}_chunk_inv {n}.toList {subj} 1 1 [] (averStringPosFuel {subj} 1 {rank})\n    (by omega) (by omega)\n    (by rw [hdata]; rfl)\n    (by simp [averStringPosFuel, hdata]; omega)]\nrw [{p}_slice_self]\nhave hval : String.intercalate \"\" ([] ++ [(\"\" : String)]) ++ String.ofList {n}.toList = {n} := by\n  simp [{p}_inter_single, {p}_str_empty_append, {p}_mk_data_self]\nrw [hval]\nhave hpos : ((1 : Int) + (({p}_escL {n}.toList).toList.length : Int) + 1) =\n    ((({prod_text}).length : Int) + 2) := by\n  rw [{p}_producer_eq]\n  show (1 : Int) + (({p}_escL {n}.toList).toList.length : Int) + 1 = ((({p}_escL {n}.toList).toList.length : Int) + 2)\n  omega\nrw [hpos]",
        open_c = lean_char(open_char(law)?),
    );
    let mut proof_lines = vec![format!("  intro {n}"), "  first".to_string()];
    for (i, line) in glue.lines().enumerate() {
        if i == 0 {
            proof_lines.push(format!("  | ({line}"));
        } else if line.is_empty() {
            proof_lines.push(String::new());
        } else {
            proof_lines.push(format!("     {line}"));
        }
    }
    proof_lines.push("     done)".to_string());
    proof_lines.push("  | sorry".to_string());

    Some(AutoProof {
        support_lines: support,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

/// The opening char of the law's subject string (`<open> + escape(s)
/// + <terminator>`). The detector validated it's a 1-char literal.
fn open_char(law: &VerifyLaw) -> Option<char> {
    let Expr::FnCall(_, lhs_args) = &law.lhs.node else {
        return None;
    };
    let Expr::BinOp(crate::ast::BinOp::Add, open_prod, _) = &lhs_args.first()?.node else {
        return None;
    };
    let Expr::BinOp(crate::ast::BinOp::Add, open_lit, _) = &open_prod.node else {
        return None;
    };
    let Expr::Literal(crate::ast::Literal::Str(s)) = &open_lit.node else {
        return None;
    };
    s.chars().next()
}
