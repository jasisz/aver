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
    assert!(output.contains("nonNeg_law_positiveIsNonnegative attempt"));
    assert_eq!(
        c.statement,
        "∀ (x : Fraction), lessF zeroF x = true -> nonNeg x = true"
    );
}

#[test]
fn a_law_with_a_proof_rule_takes_no_tactic_and_no_waterfall() {
    // No rule runs here (the test context has no project root), so each law
    // is left with its rule's refusal: a bare `sorry`, also with `using`,
    // and no waterfall region to replace it.
    let source = r#"
module W
    intent = "Laws that name a proof rule."
    effects []
fn identity(n: Int) -> Int
    n
verify identity law helper
    given n: Int = [0, 1]
    identity(n) => n
verify identity law ruled
    given n: Int = [0, 1]
    by Rules.Same.same
    identity(n) => n
verify identity law ruledUsing
    given n: Int = [0, 1]
    using [identity.helper]
    by Rules.Same.same
    identity(n) => n
"#;
    let mut ctx = ctx_from_source(source, "W");
    let _guard = waterfall::enable();
    let lean = generated_lean_file(&transpile_for_proof_mode(
        &mut ctx,
        VerifyEmitMode::NativeDecide,
    ));
    let proposals = candidates(&lean);
    assert!(
        proposals.iter().all(|c| !c.label.contains("ruled")),
        "{proposals:?}"
    );
    for theorem in ["identity_law_ruled", "identity_law_ruledUsing"] {
        let start = lean
            .find(&format!("theorem {theorem} :"))
            .unwrap_or_else(|| panic!("no theorem {theorem}\n{lean}"));
        let body: Vec<&str> = lean[start..]
            .lines()
            .skip(1)
            .take_while(|l| l.starts_with(' '))
            .map(str::trim)
            .collect();
        assert_eq!(body, ["sorry"], "{theorem}:\n{}", &lean[start..]);
    }
}
