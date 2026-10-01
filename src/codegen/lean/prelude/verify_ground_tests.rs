//! Native sampled-case infrastructure is opt-in and outside the certificate wall.

use super::build_common_lean;

const CASE_BODY: &str = "example : True := by simp [_root_.__AverProofCases.nativeGround]";

#[test]
fn case_ground_normalizer_is_demand_driven_and_not_a_global_simp_rule() {
    let common = build_common_lean(CASE_BODY, false);
    assert!(common.starts_with("import Lean\n"));
    assert!(common.contains("namespace __AverProofCases"));
    assert!(common.contains("simproc_decl nativeGround"));
    assert!(common.contains("simproc_decl nativeGroundValue"));
    assert!(!common.contains("simproc nativeGround"));

    let plain = build_common_lean("def identity (n : Int) := n", false);
    assert!(!plain.contains("__AverProofCases"));
    assert!(!plain.contains("import Lean"));
}

#[test]
fn certificate_models_never_carry_the_native_case_normalizer() {
    let common = build_common_lean(CASE_BODY, true);
    assert!(!common.contains("__AverProofCases"));
    assert!(!common.contains("import Lean"));
    assert!(!common.contains("unsafe evalExpr"));
    assert!(!common.contains("nativeEqTrue"));
}
