//! Warning: plain cases-form `verify fn` on an effectful fn whose effect
//! list includes at least one generative (or generative+output) effect.
//!
//! Plain cases-form runs the fn at verify time with real effects; any
//! generative effect produces a fresh value per call, so the case's
//! expected RHS is effectively being compared against a non-deterministic
//! value. The test will flap. A law-form `verify fn law <name>` with
//! `given` stubs, or a trace-form `verify fn trace`, is what the user
//! almost always wants.
//!
//! Rare legitimate case: the user wants the fn executed for its output
//! side-effects and doesn't care about the return value; this warning
//! points that out instead of silently blessing it. Output-only and
//! snapshot effects do not trigger — output effects don't affect the
//! return value, and snapshot effects are stable within one process.

use crate::ast::{TopLevel, VerifyBlock, VerifyKind};
use crate::types::checker::effect_classification::{EffectDimension, classify};

use super::{CheckFinding, FnSigMap};

pub fn collect_plain_cases_effectful_warnings(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
) -> Vec<CheckFinding> {
    let mut warnings = Vec::new();
    for item in items {
        if let TopLevel::Verify(vb) = item
            && let Some(finding) = warning_for_verify_block(vb, fn_sigs)
        {
            warnings.push(finding);
        }
    }
    warnings
}

pub fn collect_plain_cases_effectful_warnings_in(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
    file: Option<&str>,
) -> Vec<CheckFinding> {
    let mut warnings = collect_plain_cases_effectful_warnings(items, fn_sigs);
    if let Some(file) = file {
        for warning in &mut warnings {
            warning.file = Some(file.to_string());
        }
    }
    warnings
}

fn warning_for_verify_block(vb: &VerifyBlock, fn_sigs: &FnSigMap) -> Option<CheckFinding> {
    if !matches!(vb.kind, VerifyKind::Cases) || vb.trace {
        return None;
    }
    let (_, _, effects) = fn_sigs.get(&vb.fn_name)?;
    if effects.is_empty() {
        return None;
    }
    let generative: Vec<String> = effects
        .iter()
        .filter(|e| is_generative_like(e))
        .cloned()
        .collect();
    if generative.is_empty() {
        return None;
    }
    Some(CheckFinding {
        line: vb.line,
        module: None,
        file: None,
        fn_name: Some(vb.fn_name.clone()),
        message: format!(
            "plain 'verify {fn}' runs the fn with real effects, but {fn} uses generative \
             effect(s): {effects}. Each case compares against a freshly-produced value so the \
             test will flap. Use 'verify {fn} law <spec>' with 'given' stubs for a formal proof, \
             or 'verify {fn} trace' with 'given' stubs to assert on the emitted trace. Keep the \
             plain form only if you deliberately want a one-shot runtime smoke check and accept \
             the non-determinism.",
            fn = vb.fn_name,
            effects = generative.join(", "),
        ),
        extra_spans: vec![],
    })
}

fn is_generative_like(effect: &str) -> bool {
    match classify(effect) {
        Some(c) => matches!(
            c.dimension,
            EffectDimension::Generative | EffectDimension::GenerativeOutput
        ),
        // Unclassified effects are already rejected for trace / law forms
        // by the typechecker. For plain cases, treat them as generative-
        // like because a spooky result here is at least as bad as for a
        // classified generative effect.
        None => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::parse_source;
    use crate::types::checker::run_type_check_full;

    fn warnings_for(src: &str) -> Vec<CheckFinding> {
        let items = parse_source(src).expect("parse");
        let tc = run_type_check_full(&items, None);
        collect_plain_cases_effectful_warnings(&items, &tc.fn_sigs)
    }

    #[test]
    fn plain_cases_on_generative_fn_warns_and_names_effect() {
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn roll() -> Int\n\
             \x20   ! [Random.int]\n\
             \x20   Random.int(1, 6)\n\
             verify roll\n\
             \x20   roll() => 4\n";
        let ws = warnings_for(src);
        assert_eq!(ws.len(), 1, "expected 1 warning, got: {:?}", ws);
        assert!(ws[0].message.contains("Random.int"), "msg: {}", ws[0].message);
        assert!(ws[0].message.contains("law"), "msg suggests law form");
        assert!(ws[0].message.contains("trace"), "msg suggests trace form");
    }

    #[test]
    fn plain_cases_on_mixed_effects_names_only_generative_ones() {
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn noisyRoll() -> Int\n\
             \x20   ! [Random.int, Console.print]\n\
             \x20   n = Random.int(1, 6)\n\
             \x20   Console.print(\"rolled\")\n\
             \x20   n\n\
             verify noisyRoll\n\
             \x20   noisyRoll() => 3\n";
        let ws = warnings_for(src);
        assert_eq!(ws.len(), 1);
        assert!(ws[0].message.contains("Random.int"));
        assert!(
            !ws[0].message.contains("Console.print"),
            "Console.print is Output, should not be listed; msg: {}",
            ws[0].message
        );
    }

    #[test]
    fn plain_cases_on_pure_fn_does_not_warn() {
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn double(x: Int) -> Int\n\
             \x20   x + x\n\
             verify double\n\
             \x20   double(2) => 4\n";
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn plain_cases_on_output_only_fn_does_not_warn() {
        // Output effect doesn't affect the return value: `tag` always
        // returns `n` regardless of the side-effecting print.
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn tag(n: Int) -> Int\n\
             \x20   ! [Console.print]\n\
             \x20   Console.print(\"tagging\")\n\
             \x20   n\n\
             verify tag\n\
             \x20   tag(5) => 5\n";
        assert!(
            warnings_for(src).is_empty(),
            "Output-only effect should not flap the return value"
        );
    }

    #[test]
    fn plain_cases_on_snapshot_only_fn_does_not_warn() {
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn env(k: String) -> Option<String>\n\
             \x20   ! [Env.get]\n\
             \x20   Env.get(k)\n\
             verify env\n\
             \x20   env(\"HOME\") => Option.None\n";
        assert!(
            warnings_for(src).is_empty(),
            "Snapshot effect is stable within the process, no warning"
        );
    }

    #[test]
    fn law_form_does_not_warn() {
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn roll() -> Int\n\
             \x20   ! [Random.int]\n\
             \x20   Random.int(1, 6)\n\
             verify roll law rollSpec\n\
             \x20   given rnd: Random.int = [stub]\n\
             \x20   roll() => stub(BranchPath.Root, 0, 1, 6)\n\
             fn stub(path: BranchPath, k: Int, lo: Int, hi: Int) -> Int\n\
             \x20   lo\n";
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn trace_form_does_not_warn() {
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn roll() -> Int\n\
             \x20   ! [Random.int]\n\
             \x20   Random.int(1, 6)\n\
             verify roll trace\n\
             \x20   given rnd: Random.int = [stub]\n\
             \x20   roll().result => 1\n\
             fn stub(path: BranchPath, k: Int, lo: Int, hi: Int) -> Int\n\
             \x20   lo\n";
        assert!(warnings_for(src).is_empty());
    }
}
