use super::*;

#[test]
fn mutual_phase_contracts_preserve_parameter_names_and_worker_offset() {
    let source = r#"module Phases
    intent = "The same typed termination fact feeds both mutual members."
fn scan(label: String, position: Int, target: Int) -> Int
    match position >= target
        true -> position
        false -> step(label, position, target)
fn step(label: String, here: Int, end: Int) -> Int
    scan(label, here + 1, end)
"#;
    let ctx = build_ctx(source);
    for (name, expected_param, expected_bound, expected_worker) in [
        ("scan", "position", "target", false),
        ("step", "here", "end", true),
    ] {
        let contract = fn_contract(&ctx, name).unwrap();
        assert!(
            matches!(contract.recursion.as_ref(), Some(RecursionContract::WellFoundedIntPhase { param, bound: Some(bound), worker })
            if param == expected_param && bound == expected_bound && *worker == expected_worker),
            "{contract:?}"
        );
    }
    assert!(ctx.proof_ir.unclassified_fns.is_empty());
    let mut ctx = build_ctx(source);
    let output = aver::codegen::lean::transpile_for_proof_mode(
        &mut ctx,
        aver::codegen::lean::VerifyEmitMode::NativeDecide,
    );
    let lean = &output
        .files
        .iter()
        .find(|(name, _)| name == "Phases.lean")
        .unwrap()
        .1;
    assert!(
        lean.contains("termination_by ((target - position).toNat, 0)"),
        "{lean}"
    );
    assert!(
        lean.contains("termination_by ((end' - here - 1).toNat, 1)"),
        "{lean}"
    );
    assert!(!lean.contains("partial def"), "{lean}");
}

#[test]
fn a_mutual_phase_does_not_guess_a_changing_bound() {
    let source = r#"module Phases
    intent = "A moving target does not bound the walk."
fn scan(position: Int, target: Int) -> Int
    match position >= target
        true -> position
        false -> step(position, target)
fn step(here: Int, end: Int) -> Int
    scan(here + 1, end + 1)
"#;
    let ctx = build_ctx(source);
    for name in ["scan", "step"] {
        assert!(!matches!(
            fn_contract(&ctx, name).and_then(|c| c.recursion.as_ref()),
            Some(RecursionContract::WellFoundedIntPhase { .. })
        ));
    }
}
