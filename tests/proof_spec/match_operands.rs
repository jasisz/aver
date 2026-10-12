use super::*;

const FIXTURE: &str = "tests/fixtures/lean_match_operands.av";

/// Every operator must apply to the completed match, including a match in
/// the law itself and the division/modulo intrinsics after Int anchoring.
#[test]
fn proof_export_match_operands_are_closed_before_following_syntax() {
    let output_dir = temp_output_dir("aver-proof-match-operands-export");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["proof", "--examples", FIXTURE, "-o"])
        .arg(&output_dir)
        .output()
        .expect("export operand fixture");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("LeanMatchOperands.lean")).unwrap();
    let flat = lean.split_whitespace().collect::<Vec<_>>().join(" ");
    for (name, op, ty) in [
        ("shifted", "+", "Int"),
        ("minus", "-", "Int"),
        ("times", "*", "Int"),
        ("equal", "==", "Bool"),
        ("unequal", "!=", "Bool"),
        ("below", "<", "Bool"),
        ("above", ">", "Bool"),
        ("atMost", "<=", "Bool"),
        ("atLeast", ">=", "Bool"),
    ] {
        let expected = format!(
            "def {name} (x : Int) (y : Int) : {ty} := ((match x with | 0 => (1 : Int) | _ => (2 : Int)) {op} y)"
        );
        assert!(flat.contains(&expected), "missing `{expected}`:\n{lean}");
    }
    for expected in [
        "def same (x : Int) (b : Bool) : Bool := ((match x with | 0 => true | _ => false) == b)",
        "def divided (x : Int) (y : Float) : Float := ((match x with | 0 => 8.0 | _ => 12.0) / y)",
        "def anchored (x : Int) : Int := ((match x with | 0 => (1 : Int) | _ => (2 : Int) : Int) + match x with | 0 => (3 : Int) | _ => (4 : Int))",
        "def prepend (x : Int) (xs : List Int) : List Int := ((match x with | 0 => (1 : Int) | _ => (2 : Int)) :: xs)",
        "def concat (x : Int) (suffix : String) : String := ((match x with | 0 => \"a\" | _ => \"b\") + suffix)",
        "def subject (x : Int) : Int := match (match x with | 0 => (1 : Int) | _ => (2 : Int)) with",
        "def condition (x : Int) : Int := (if (match x with | 0 => true | _ => false) then",
        "def updated (x : Int) : Boxed := { (match x with | 0 => { value := 1 : Boxed } | _ => { value := 2 : Boxed }) with value := 3 }",
        "def quotient (x : Int) : Int := ((match x with | 0 => (8 : Int) | _ => (12 : Int)) / (2 : Int))",
        "def remainder (x : Int) : Int := ((match x with | 0 => (8 : Int) | _ => (12 : Int)) % (3 : Int))",
        "theorem same_law_operandInClaim : ∀ (x : Int) (b : Bool), same x b = ((match x with | 0 => true | _ => false) == b)",
        "theorem same_law_matchOnLeft : ∀ (x : Int), (match x with | 0 => true | _ => false) = same x true",
        "theorem same_law_matchPremise : ∀ (x : Int), (match x with | 0 => true | _ => false) = true -> same x true = true",
        "theorem same_law_matchPremise_sample_1 : (match (0 : Int) with | 0 => true | _ => false) = true -> same 0 true = true",
        "example : same 0 false = false := by decide +kernel",
        "example : shifted 0 10 = (11 : Int) := by decide +kernel",
        // Already safe slots retain their exact spelling, including a bare
        // terminal right operand and the if produced by a Boolean match.
        "def rightOperand (x : Int) (y : Int) : Int := (y + match x with | 0 => (1 : Int) | _ => (2 : Int))",
        "def ifOperand (b : Bool) (y : Int) : Int := ((if b then (1 : Int) else (2 : Int)) + y)",
        "def projected (x : Int) : Int := (match x with | 0 => { value := 1 : Boxed } | _ => { value := 2 : Boxed }).value",
        "def boolOperands (x : Int) (b : Bool) : Bool := (((match x with | 0 => true | _ => false) && b) || (!(match x with | 0 => true | _ => false)))",
    ] {
        assert!(flat.contains(expected), "missing `{expected}`:\n{lean}");
    }
    let _ = std::fs::remove_dir_all(&output_dir);
}

#[test]
fn proof_match_operand_examples_agree_with_vm() {
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["verify", FIXTURE])
        .output()
        .expect("verify operand fixture");
    assert!(run.status.success(), "{}", format_output(&run));
}

/// Run by the Proof CI workflow; intentionally not needed by export tests.
#[test]
fn proof_match_operand_examples_hold_under_lean() {
    assert_proof_builds(FIXTURE, "aver-proof-match-operands-lean");
}
