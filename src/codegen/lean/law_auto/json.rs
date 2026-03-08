use crate::ast::{Expr, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::shared::{call_named_args, callee_matches_name, matches_ident};

fn matches_result_ok(expr: &Expr, arg_name: &str) -> bool {
    match expr {
        Expr::Constructor(name, Some(inner)) => {
            name == "Result.Ok" && matches_ident(inner, arg_name)
        }
        Expr::FnCall(callee, args) => {
            args.len() == 1
                && callee_matches_name(callee, "Result.Ok")
                && matches_ident(&args[0], arg_name)
        }
        _ => false,
    }
}

fn is_json_roundtrip_law(vb: &VerifyBlock, law: &VerifyLaw) -> bool {
    if vb.fn_name != "toString" || law.givens.len() != 1 || law.givens[0].type_name != "Json" {
        return false;
    }

    let given_name = &law.givens[0].name;

    let lhs_matches = call_named_args(&law.lhs, "fromString")
        .and_then(|args| args.first())
        .and_then(|arg| call_named_args(arg, "toString"))
        .is_some_and(|args| args.len() == 1 && matches_ident(&args[0], given_name));

    let when_matches = law
        .when
        .as_ref()
        .and_then(|when_expr| call_named_args(when_expr, "jsonRoundtripSafe"))
        .is_some_and(|args| args.len() == 1 && matches_ident(&args[0], given_name));

    lhs_matches && when_matches && matches_result_ok(&law.rhs, given_name)
}

pub(super) fn emit_json_roundtrip_support_theorems(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    _ctx: &CodegenContext,
    theorem_base: &str,
) -> Option<Vec<String>> {
    if !is_json_roundtrip_law(vb, law) {
        return None;
    }

    let digit_ws = format!("{theorem_base}__digitChar_not_ws");
    let get_elem = format!("{theorem_base}__getElem_append_at_length");
    let char_at = format!("{theorem_base}__charAt_append_at_length");
    let scan_digits = format!("{theorem_base}__scanIntTail_digit_suffix");
    let skip_ws = format!("{theorem_base}__skipWs_fromInt_zero");
    let slice_full = format!("{theorem_base}__string_slice_full");
    let check_eof = format!("{theorem_base}__checkEof_at_end");
    let finish_int = format!("{theorem_base}__finishInt_fromInt");
    let finish_number = format!("{theorem_base}__finishNumber_fromInt");
    let null_rt = format!("{theorem_base}__null_roundtrip");
    let bool_rt = format!("{theorem_base}__bool_roundtrip");

    Some(vec![
        format!("set_option maxRecDepth 4096 in private theorem {} : ∀ d : Nat, d < 10 ->", digit_ws),
        "    Char.toString (AverDigits.digitChar d) ≠ \" \" ∧".to_string(),
        "    Char.toString (AverDigits.digitChar d) ≠ \"\\t\" ∧".to_string(),
        "    Char.toString (AverDigits.digitChar d) ≠ \"\\n\" ∧".to_string(),
        "    Char.toString (AverDigits.digitChar d) ≠ \"\\r\" := by".to_string(),
        "  intro d hd".to_string(),
        "  simpa using AverDigits.digitChar_not_ws d hd".to_string(),
        String::new(),
        format!("private theorem {} : ∀ (pre : List Char) (x : Char) (xs : List Char),", get_elem),
        "    (pre ++ x :: xs)[pre.length] = x".to_string(),
        "  | [], x, xs => by simp".to_string(),
        "  | _ :: pre, x, xs => by".to_string(),
        format!("      simp [{} pre x xs]", get_elem),
        String::new(),
        format!("private theorem {} (pre : List Char) (x : Char) (xs : List Char) :", char_at),
        "    String.charAt (({ data := pre ++ x :: xs } : String)) pre.length = some (Char.toString x) := by".to_string(),
        "  unfold String.charAt".to_string(),
        "  have hs : ¬ ((pre.length : Int) < 0) := by omega".to_string(),
        format!("  simp [hs, {}]", get_elem),
        String::new(),
        format!("private theorem {} :", scan_digits),
        "    ∀ (pre rest : List Char),".to_string(),
        "      (∀ c ∈ rest, isDigit (Char.toString c) = true) ->".to_string(),
        "      scanIntTail__fuel (rest.length + 1) (({ data := pre ++ rest } : String)) pre.length 0 false =".to_string(),
        "        finishNumber (({ data := pre ++ rest } : String)) 0 (pre.length + rest.length) false".to_string(),
        "  | pre, [], hdigits => by".to_string(),
        "      have hchar : String.charAt (({ data := pre ++ [] } : String)) pre.length = none := by".to_string(),
        "        simpa using (String.charAt_length_none (({ data := pre } : String)))".to_string(),
        "      rw [scanIntTail__fuel.eq_def]".to_string(),
        "      rw [hchar]".to_string(),
        "      simp [finishNumber]".to_string(),
        "  | pre, x :: xs, hdigits => by".to_string(),
        "      have hx : isDigit (Char.toString x) = true := hdigits x (by simp)".to_string(),
        "      have hxs : ∀ c ∈ xs, isDigit (Char.toString c) = true := by".to_string(),
        "        intro c hc".to_string(),
        "        exact hdigits c (by simp [hc])".to_string(),
        "      rw [scanIntTail__fuel.eq_def]".to_string(),
        format!("      rw [{} pre x xs]", char_at),
        "      simp [hx]".to_string(),
        format!("      have hrec := {} (pre ++ [x]) xs hxs", scan_digits),
        "      simpa [Int.add_assoc, Int.add_comm, Int.add_left_comm] using hrec".to_string(),
        String::new(),
        format!("set_option maxRecDepth 4096 in private theorem {} : ∀ n : Int, skipWs (String.fromInt n) 0 = 0 := by", skip_ws),
        "  intro n".to_string(),
        "  cases n with".to_string(),
        "  | ofNat m =>".to_string(),
        "      cases h : AverDigits.natDigits m with".to_string(),
        "      | nil =>".to_string(),
        "          exfalso".to_string(),
        "          exact AverDigits.natDigits_nonempty m h".to_string(),
        "      | cons d ds =>".to_string(),
        "          have hd : d < 10 := AverDigits.natDigits_digits_lt_ten m d (by simp [h])".to_string(),
        format!("          rcases {} d hd with ⟨hspace, htab, hnl, hcr⟩", digit_ws),
        "          simp [skipWs, skipWs__fuel.eq_def, averStringPosFuel, String.fromInt, String.charAt, AverDigits.natDigitsChars, h, hspace, htab, hnl, hcr]".to_string(),
        "  | negSucc m =>".to_string(),
        "      have hspace : Char.toString '-' ≠ \" \" := by decide".to_string(),
        "      have htab : Char.toString '-' ≠ \"\\t\" := by decide".to_string(),
        "      have hnl : Char.toString '-' ≠ \"\\n\" := by decide".to_string(),
        "      have hcr : Char.toString '-' ≠ \"\\r\" := by decide".to_string(),
        "      simp [skipWs, skipWs__fuel.eq_def, averStringPosFuel, String.fromInt, String.charAt, hspace, htab, hnl, hcr]".to_string(),
        String::new(),
        format!("private theorem {} (s : String) : String.slice s 0 s.length = s := by", slice_full),
        "  unfold String.slice".to_string(),
        "  simp".to_string(),
        "  change { data := List.take s.data.length s.data } = s".to_string(),
        "  simp".to_string(),
        String::new(),
        format!("private theorem {} (s : String) (j : Json) : checkEof s j s.length = Except.ok j := by", check_eof),
        "  have hchar : String.charAt s s.length = none := String.charAt_length_none s".to_string(),
        "  unfold checkEof".to_string(),
        "  cases hs : String.charAt s s.length with".to_string(),
        "  | none => rfl".to_string(),
        "  | some c =>".to_string(),
        "      have : False := by".to_string(),
        "          simp [hs] at hchar".to_string(),
        "      exact False.elim this".to_string(),
        String::new(),
        format!("private theorem {} : ∀ n : Int,", finish_int),
        "    finishInt (String.fromInt n) (String.fromInt n).length =".to_string(),
        "      ParseResult.ok (Json.jsonInt n) (String.fromInt n).length := by".to_string(),
        "  intro n".to_string(),
        "  rw [finishInt, Int.fromString_fromInt]".to_string(),
        String::new(),
        format!("private theorem {} : ∀ n : Int,", finish_number),
        "    finishNumber (String.fromInt n) 0 (String.fromInt n).length false =".to_string(),
        "      ParseResult.ok (Json.jsonInt n) (String.fromInt n).length := by".to_string(),
        "  intro n".to_string(),
        format!("  rw [finishNumber, {}, {}]", slice_full, finish_int),
        "  simp".to_string(),
        String::new(),
        format!(
            "private theorem {} : fromString (toString Json.jsonNull) = Except.ok Json.jsonNull := by",
            null_rt
        ),
        "  native_decide".to_string(),
        String::new(),
        format!(
            "private theorem {} : ∀ b : Bool, fromString (toString (Json.jsonBool b)) = Except.ok (Json.jsonBool b) := by",
            bool_rt
        ),
        "  intro b".to_string(),
        "  cases b <;> native_decide".to_string(),
        String::new(),
    ])
}
