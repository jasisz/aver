use super::*;
use crate::codegen::lean::waterfall::{self, Candidate};

fn candidates(text: &str) -> Vec<Candidate> {
    text.lines()
        .filter_map(|line| line.strip_prefix(waterfall::BEGIN))
        .map(|json| serde_json::from_str(json).unwrap())
        .collect()
}

#[test]
fn waterfall_is_opt_in_and_never_bypasses_reason_assembly() {
    let mut ctx = ctx_from_source(
        include_str!("../../../../tests/fixtures/law_reasons.av"),
        "LawReasons",
    );
    let baseline = generated_lean_file(&transpile_for_proof_mode(
        &mut ctx,
        VerifyEmitMode::NativeDecide,
    ));
    assert!(candidates(&baseline).is_empty());
    {
        let _guard = waterfall::enable();
        let annotated = generated_lean_file(&transpile_for_proof_mode(
            &mut ctx,
            VerifyEmitMode::NativeDecide,
        ));
        let proposals = candidates(&annotated);
        assert!(!proposals.is_empty());
        assert!(proposals.iter().all(|c| c.obligation && c.hints.is_empty()));
        assert!(
            proposals
                .iter()
                .any(|c| c.label == "identity.badReasonCannotHideBehindEasyGoal.because1")
        );
        let stripped = annotated
            .lines()
            .filter(|line| !line.starts_with(waterfall::BEGIN) && *line != waterfall::END)
            .collect::<Vec<_>>()
            .join("\n");
        assert_eq!(stripped.trim_end(), baseline.trim_end());
    }
    assert!(!waterfall::enabled());
    assert_eq!(
        generated_lean_file(&transpile_for_proof_mode(
            &mut ctx,
            VerifyEmitMode::NativeDecide
        )),
        baseline
    );
}

#[test]
fn waterfall_keeps_guards_and_selected_citations() {
    let source = r#"
module W
    intent = "Preserve the source claim and citation scope."
    effects []
fn identity(n: Int) -> Int
    n
verify identity law helper
    given n: Int = [0, 1]
    identity(n) => n
verify identity law guarded
    given n: Int = [1, 2]
    when n > 0
    because identity(n) == n
    using [identity.helper]
    identity(n) > 0 holds
"#;
    let mut ctx = ctx_from_source(source, "W");
    let _guard = waterfall::enable();
    let output = generated_lean_file(&transpile_for_proof_mode(
        &mut ctx,
        VerifyEmitMode::NativeDecide,
    ));
    for c in candidates(&output).iter().filter(|c| c.obligation) {
        assert_eq!(c.hints, ["identity_law_helper"]);
        assert!(c.statement.contains("n > 0"), "{}", c.statement);
        assert!(!c.statement.contains("n = 1 ∨"));
    }
}

#[test]
fn waterfall_generalizes_only_the_generated_sample_domain() {
    let mut ctx = ctx_from_source(
        include_str!("../../../../tests/fixtures/waterfall_guarded.av"),
        "WaterfallGuarded",
    );
    let _guard = waterfall::enable();
    let output = generated_lean_file(&transpile_for_proof_mode(
        &mut ctx,
        VerifyEmitMode::NativeDecide,
    ));
    let proposals = candidates(&output);
    let c = &proposals[0];
    assert!(!c.baseline_universal);
    assert!(output.contains("nonNeg_law_positiveIsNonnegative bounded-domain"));
    assert_eq!(
        c.statement,
        "∀ (x : Fraction), lessF zeroF x = true -> nonNeg x = true"
    );
}
