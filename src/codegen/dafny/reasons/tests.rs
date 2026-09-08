use super::*;
use crate::ast::VerifyKind;
use crate::codegen::dafny::tests::ctx_from_source;

fn emitted(source: &str, wanted: &str) -> Result<String, String> {
    let ctx = ctx_from_source(source, "Guidance");
    let blocks = local_blocks(&ctx);
    let (block, law) = blocks
        .iter()
        .find_map(|block| match &block.kind {
            VerifyKind::Law(law) if label(block, law) == wanted => Some((*block, law.as_ref())),
            _ => None,
        })
        .expect("fixture law");
    emit(block, law, &ctx)
}

const POSITIVE: &str = r#"fn positive(x: Int) -> Bool
    x > 0
verify positive law guarded
    given x: Int = [1]
    when x > 0
    because x >= 1
    using []
    positive(x) holds
fn advance(x: Int) -> Int
    x + 2
verify advance law explained
    given x: Int = [1, 2]
    when x > 0
    because positive(x)
    because x + 1 > 1
    using [positive.guarded]
    advance(x) >= 3 holds
"#;

#[test]
fn guarded_steps_remain_separate_and_parent_calls_every_obligation() {
    let text = emitted(POSITIVE, "advance.explained").unwrap();
    let name = lemma_name("advance.explained");
    assert_eq!(text.matches("// aver:dafny-obligation ").count(), 3);
    assert_eq!(text.matches("// aver:dafny-law ").count(), 1);
    for step in ["because1", "because2", "implication"] {
        assert!(
            text.contains(&format!("{name}_{step} advance.explained.{step}")),
            "{text}"
        );
        assert!(text.contains(&format!("  {name}_{step}(x);")), "{text}");
    }
    let first = &text[..text
        .find(&format!("// aver:dafny-obligation {name}_because2"))
        .unwrap()];
    assert!(
        !first.contains("requires positive(x)"),
        "reason must not become its own guard: {first}"
    );
    assert!(text.contains("requires positive(x)"), "{text}");
    assert_eq!(text.matches("requires (x > 0)").count(), 4, "{text}");
    assert!(text.contains("forall x: int | (x > 0) ensures"), "{text}");
    assert!(
        text.contains(&format!("{}(x);", lemma_name("positive.guarded"))),
        "{text}"
    );
    assert!(!text.contains("assume"));
    assert!(!text.contains("{:axiom}"));
}

#[test]
fn singleton_guided_statements_are_admitted_without_claiming_they_hold() {
    let source = "fn identity(x: Int) -> Int\n    x\nverify identity law falseReason\n    given x: Int = [0]\n    because false\n    using []\n    identity(x) => x\n";
    let text = emitted(source, "identity.falseReason").unwrap();
    assert!(text.contains("ensures false"));
    assert!(text.contains("assert false;"));
    assert!(text.contains("requires false"));
    assert!(text.contains(&format!(
        "  {}_because1(x);",
        lemma_name("identity.falseReason")
    )));
    assert!(!text.contains("assume"));
}

#[test]
fn unsupported_cones_decline_before_any_guided_obligation_is_emitted() {
    for (body, reason) in [("Int.div(x, 2)", "unsupported call Int.div")] {
        let source = format!(
            "fn f(x: Int) -> Int\n    {body}\nverify f law explained\n    given x: Int = [1, 2]\n    because x == x\n    using []\n    f(x) => f(x)\n"
        );
        assert!(
            emitted(&source, "f.explained")
                .unwrap_err()
                .contains(reason)
        );
    }
    let recursion = "fn down(n: Int) -> Int\n    match n <= 0\n        true -> 0\n        false -> down(n + 1)\nverify down law explained\n    given n: Int = [0, 1]\n    because down(n) == down(n)\n    using []\n    down(n) => down(n)\n";
    assert!(
        emitted(recursion, "down.explained")
            .unwrap_err()
            .contains("recursive")
    );
    let record = "record Box\n    value: Int\nfn f(box: Box) -> Int\n    box.value\nverify f law explained\n    given box: Box = [Box(value = 1)]\n    because true\n    using []\n    f(box) => f(box)\n";
    assert!(
        emitted(record, "f.explained")
            .unwrap_err()
            .contains("plain Int/Bool")
    );
}

#[test]
fn selected_unsupported_helper_declines_the_consumer_too() {
    let source = "fn square(x: Int) -> Int\n    Int.div(x, 2)\nverify square law reflexive\n    given x: Int = [0, 1]\n    using []\n    square(x) => square(x)\nfn identity(x: Int) -> Int\n    x\nverify identity law consumer\n    given x: Int = [0, 1]\n    because x == x\n    using [square.reflexive]\n    identity(x) => x\n";
    let error = emitted(source, "identity.consumer").unwrap_err();
    assert!(error.contains("citation square.reflexive"), "{error}");
    assert!(error.contains("unsupported call Int.div"), "{error}");
}

#[test]
fn nonlinear_products_are_emitted_as_checked_arithmetic() {
    let source = "fn square(x: Int) -> Int\n    x * x\nverify square law nonnegative\n    given x: Int = [-2, 0, 3]\n    because x * x >= 0\n    using []\n    square(x) >= 0 holds\n";
    let text = emitted(source, "square.nonnegative").unwrap();
    assert!(text.contains("(x * x)"), "{text}");
    assert!(text.contains("assert"), "{text}");
    assert!(!text.contains("assume"), "{text}");
}

#[test]
fn recursive_admission_checks_hidden_calls_and_mutual_cycles() {
    let tail = "verify down law reflexive\n    given n: Int = [0, 2]\n    using []\n    down(n) => down(n)\n";
    for (definition, expected) in [
        (
            "fn down(n: Int) -> Int\n    match n <= 0\n        true -> Int.div(n, 2)\n        false -> down(n - 1)\n",
            "unsupported call Int.div",
        ),
        (
            "fn down(n: Int) -> Int\n    match n <= 0\n        true -> 0\n        false -> down(n - 1) + other(n - 1)\nfn other(n: Int) -> Int\n    match n <= 0\n        true -> 0\n        false -> down(n - 1)\n",
            "recurs",
        ),
    ] {
        let error = emitted(&format!("{definition}{tail}"), "down.reflexive").unwrap_err();
        assert!(error.contains(expected), "{error}");
    }
}

#[test]
fn recursive_integer_equations_do_not_request_unnecessary_induction() {
    let source = "fn down(n: Int) -> Int\n    match n <= 0\n        true -> 0\n        false -> down(n - 1)\nverify down law successor\n    given n: Int = [0, 2]\n    when n >= 0\n    because down(n + 1) == down(n)\n    using []\n    down(n + 1) => down(n)\n";
    let text = emitted(source, "down.successor").unwrap();
    assert!(!text.contains("{:induction n}"), "{text}");
    assert_eq!(text.matches("{:induction false}").count(), 3, "{text}");
}

#[test]
fn guarded_countdown_with_binding_keeps_its_negative_input_domain() {
    let source = "fn down(n: Int) -> Int\n    zero = 0\n    match n <= 0\n        true -> zero\n        false -> down(n - 1)\nverify down law negativeBase\n    given n: Int = [-2, 0]\n    when n <= 0\n    because down(n) == 0\n    using []\n    down(n) => 0\n";
    let ctx = ctx_from_source(source, "Guidance");
    let project = crate::codegen::dafny::transpile(&ctx);
    let text = project
        .files
        .iter()
        .filter(|(name, _)| name.ends_with(".dfy"))
        .map(|(_, text)| text.as_str())
        .collect::<Vec<_>>()
        .join("\n");
    assert!(text.contains("decreases if n >= 0 then n else 0"), "{text}");
    assert!(!text.contains("requires n >= 0"), "{text}");
    assert!(!text.contains("{:axiom}"), "{text}");
}

#[test]
fn automatic_selection_and_unguarded_legacy_citation_are_not_silently_enabled() {
    let automatic = POSITIVE.replace("    using [positive.guarded]\n", "");
    assert!(
        emitted(&automatic, "advance.explained")
            .unwrap_err()
            .contains("explicit using")
    );
    let legacy = POSITIVE.replace("    using []\n", "");
    assert!(
        emitted(&legacy, "advance.explained")
            .unwrap_err()
            .contains("citation positive.guarded")
    );
}

#[test]
fn literal_scaling_and_bool_branches_stay_in_the_supported_subset() {
    let source = "fn f(x: Int, flag: Bool) -> Int\n    match flag\n        true -> 2 * x\n        false -> 0 - x\nverify f law reflexive\n    given x: Int = [0, 1]\n    given flag: Bool = [true, false]\n    because Bool.or(flag, Bool.not(flag))\n    using []\n    f(x, flag) => f(x, flag)\n";
    assert!(emitted(source, "f.reflexive").is_ok());
}

#[test]
fn generated_names_and_escaped_variables_cannot_capture_lemma_calls() {
    let collision = format!(
        "fn {}_because1(x: Int) -> Int\n    x\n{POSITIVE}",
        lemma_name("advance.explained")
    );
    assert!(
        emitted(&collision, "advance.explained")
            .unwrap_err()
            .contains("collides with source name")
    );
    let variables = "fn f(_x: Int, aver_x: Int) -> Int\n    _x + aver_x\nverify f law explained\n    given _x: Int = [0, 1]\n    given aver_x: Int = [0, 1]\n    using []\n    f(_x, aver_x) => f(_x, aver_x)\n";
    assert!(
        emitted(variables, "f.explained")
            .unwrap_err()
            .contains("ambiguous emitted variable")
    );
}
