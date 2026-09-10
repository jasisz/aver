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
    let empty = std::collections::HashSet::new();
    let recursion = super::super::toplevel::LawRecursion {
        opaque_fns: &empty,
        native_members: &empty,
        native_callers: &empty,
        termination_opaque: &empty,
    };
    emit(block, law, &ctx, &recursion)
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
fn slice_reasons_use_shared_guards_and_recursive_premises() {
    let source = include_str!("../../../../tests/fixtures/law_reasons_slices.av");
    let ctx = ctx_from_source(source, "SliceReasons");
    for theorem in &ctx.proof_ir.law_theorems {
        let plan = theorem.reason_inductions[0].as_ref().expect("reason plan");
        assert_eq!(plan.driver, "xs");
        let call = &plan.calls[0];
        let expression = |e| super::super::expr::emit_expr(e, &ctx);
        assert!(expression(call.branch_guard.as_ref().unwrap()).contains('n'));
        assert!(
            expression(call.premise.as_ref().unwrap()).contains("nonnegative(averInductionPart")
        );
        assert_eq!(expression(&call.arguments[1]), "(n - 1)");
    }
    // The second step may only invoke itself with the first step established
    // at the recursive arguments, even if that premise is unrelated to lists.
    let source = source.replace(
        "    because takeReason",
        "    because n >= 0\n    because takeReason",
    );
    let ctx = ctx_from_source(&source, "SliceReasons");
    let plan = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "takePreserves")
        .unwrap()
        .reason_inductions[1]
        .as_ref()
        .unwrap();
    let premise = super::super::expr::emit_expr(plan.calls[0].premise.as_ref().unwrap(), &ctx);
    assert!(premise.contains("(n - 1) >= 0"), "{premise}");
}

#[test]
fn nested_reason_guards_use_pattern_scope_and_ambiguous_paths_stay_unplanned() {
    let source = include_str!("../../../../tests/fixtures/law_reasons_slices.av");
    let (functions, laws) = source
        .split_once("verify nonnegative law dropPreserves")
        .unwrap();
    // The source head and the law's counter share a spelling, but belong to
    // different scopes. The branch uses the head; recursion decrements the counter.
    let shadowed = format!(
        "{}verify nonnegative law dropPreserves{}",
        functions.replace("match n > 0", "match x > 0"),
        laws.replace("given n:", "given x:")
            .replace("(xs, n)", "(xs, x)")
    );
    let ctx = ctx_from_source(&shadowed, "SliceReasons");
    let plan = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "takePreserves")
        .unwrap()
        .reason_inductions[0]
        .as_ref()
        .unwrap();
    let call = &plan.calls[0];
    let head = call.list_case.as_ref().unwrap().head.as_ref().unwrap();
    let guard = super::super::expr::emit_expr(call.branch_guard.as_ref().unwrap(), &ctx);
    assert_eq!(guard, format!("({head} > 0)"));
    assert_eq!(
        super::super::expr::emit_expr(&call.arguments[1], &ctx),
        "(x - 1)"
    );

    let ambiguous = source.replace(
        "false -> nonnegative(List.take(xs, n))",
        "false -> takeReason(rest, n + 1)",
    );
    let ctx = ctx_from_source(&ambiguous, "SliceReasons");
    let theorem = ctx
        .proof_ir
        .law_theorems
        .iter()
        .find(|t| t.law_name == "takePreserves")
        .unwrap();
    assert!(theorem.reason_inductions[0].is_none());
}

#[test]
fn mutual_admission_requires_every_native_member_and_checks_its_body() {
    let source = "fn scan(xs: List<Int>, acc: List<Int>) -> List<Int>\n    match xs\n        [] -> acc\n        [head, ..tail] -> step(head, tail, acc)\nfn step(head: Int, rest: List<Int>, acc: List<Int>) -> List<Int>\n    scan(rest, List.prepend(head, acc))\nverify scan law identity\n    given xs: List<Int> = [[]]\n    given acc: List<Int> = [[]]\n    because true\n    using []\n    scan(xs, acc) => scan(xs, acc)\n";
    for (native_names, unsupported, admitted) in [
        (vec![], false, false),
        (vec!["scan"], false, false),
        (vec!["scan", "step"], false, true),
        (vec!["scan", "step"], true, false),
    ] {
        let source = if unsupported {
            source.replace(
                "    scan(rest, List.prepend(head, acc))",
                "    hidden = String.byteLength(\"unsupported\")\n    scan(rest, List.prepend(head, acc))",
            )
        } else {
            source.to_string()
        };
        let ctx = ctx_from_source(&source, "Guidance");
        let native = native_names
            .iter()
            .map(|name| ctx.symbol_table.resolve_fn_id_in(name, None).unwrap())
            .collect();
        let blocks = local_blocks(&ctx);
        let block = blocks[0];
        let VerifyKind::Law(law) = &block.kind else {
            panic!("expected law");
        };
        let empty = std::collections::HashSet::new();
        let recursion = super::super::toplevel::LawRecursion {
            opaque_fns: &empty,
            native_members: &native,
            native_callers: &native,
            termination_opaque: &empty,
        };
        let result = emit(block, law, &ctx, &recursion);
        assert_eq!(result.is_ok(), admitted, "{native_names:?}: {result:?}");
        if unsupported {
            assert!(result.unwrap_err().contains("String.byteLength"));
        }
    }
}

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
    let source = "fn f(x: Int) -> Int\n    String.byteLength(\"unsupported\")\nverify f law explained\n    given x: Int = [1, 2]\n    because x == x\n    using []\n    f(x) => f(x)\n";
    assert!(
        emitted(source, "f.explained")
            .unwrap_err()
            .contains("unsupported call String.byteLength")
    );
    let recursion = "fn down(n: Int) -> Int\n    match n <= 0\n        true -> 0\n        false -> down(n + 1)\nverify down law explained\n    given n: Int = [0, 1]\n    because down(n) == down(n)\n    using []\n    down(n) => down(n)\n";
    assert!(
        emitted(recursion, "down.explained")
            .unwrap_err()
            .contains("recursive")
    );
    let record = "record Box\n    value: Float\nfn f(box: Box) -> Float\n    box.value\nverify f law explained\n    given box: Box = [Box(value = 1.0)]\n    because true\n    using []\n    f(box) => f(box)\n";
    assert!(
        emitted(record, "f.explained")
            .unwrap_err()
            .contains("unsupported first-order type Float")
    );
}

#[test]
fn selected_unsupported_helper_declines_the_consumer_too() {
    let source = "fn square(x: Int) -> Int\n    String.byteLength(\"unsupported\")\nverify square law reflexive\n    given x: Int = [0, 1]\n    using []\n    square(x) => square(x)\nfn identity(x: Int) -> Int\n    x\nverify identity law consumer\n    given x: Int = [0, 1]\n    because x == x\n    using [square.reflexive]\n    identity(x) => x\n";
    let error = emitted(source, "identity.consumer").unwrap_err();
    assert!(error.contains("citation square.reflexive"), "{error}");
    assert!(
        error.contains("unsupported call String.byteLength"),
        "{error}"
    );
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
            "fn down(n: Int) -> Int\n    match n <= 0\n        true -> String.byteLength(\"unsupported\")\n        false -> down(n - 1)\n",
            "unsupported call String.byteLength",
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
fn automatic_selection_declines_and_plain_suppliers_get_checked_restatements() {
    let automatic = POSITIVE.replace("    using [positive.guarded]\n", "");
    assert!(
        emitted(&automatic, "advance.explained")
            .unwrap_err()
            .contains("explicit using")
    );
    // An explained supplier still requires an explicit selection policy.
    let missing_selection = POSITIVE.replace("    using []\n", "");
    assert!(
        emitted(&missing_selection, "advance.explained")
            .unwrap_err()
            .contains("citation positive.guarded")
    );
    // A plain source law is re-proved as a universal statement, even if its
    // ordinary backend strategy only proved the finite supplied examples.
    let plain = missing_selection.replace("    because x >= 1\n", "");
    let text = emitted(&plain, "advance.explained").unwrap();
    assert!(
        text.contains("// Checked universal citation: positive.guarded"),
        "{text}"
    );
    assert!(text.contains("assert (positive(x)) == (true);"), "{text}");
    assert!(text.contains("requires (x > 0)"), "{text}");
    assert!(
        !text.contains(&format!("\n    {}(x);", lemma_name("positive.guarded"))),
        "{text}"
    );
    assert!(!text.contains("assume"), "{text}");
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

#[test]
fn local_record_construction_update_and_projection_are_checked() {
    let source = r#"record Box
    value: Int
fn advance(box: Box) -> Box
    Box.update(box, value = box.value + 1)
verify advance law increment
    given box: Box = [Box(value = 0)]
    because advance(box).value == box.value + 1
    using []
    advance(box).value => box.value + 1
"#;
    let text = emitted(source, "advance.increment").unwrap();
    assert!(text.contains("// aver:dafny-law"), "{text}");
    assert!(!text.contains("{:axiom}"), "{text}");
}

#[test]
fn nested_unsupported_fields_decline_even_when_samples_do_not_construct_them() {
    let source = r#"record Payload
    measurement: Float
record Wrapper
    payload: Option<Payload>
fn identity(w: Wrapper) -> Wrapper
    w
verify identity law reflexive
    given w: Wrapper = [Wrapper(payload = Option.None)]
    because true
    using []
    identity(w) => w
"#;
    let error = emitted(source, "identity.reflexive").unwrap_err();
    assert!(
        error.contains("unsupported first-order type Float"),
        "{error}"
    );
}

#[test]
fn algebraic_matches_and_result_branches_are_admitted() {
    let source = r#"type Op
    Push(Int)
    Fail
fn interpret(op: Op) -> Result<Int, String>
    match op
        Op.Push(value) -> Result.Ok(value)
        Op.Fail -> Result.Err("failed")
fn success(op: Op) -> Bool
    match interpret(op)
        Result.Ok(value) -> value == value
        Result.Err(_) -> true
verify success law cases
    given op: Op = [Op.Push(1), Op.Fail]
    because success(op)
    using []
    success(op) holds
"#;
    assert!(emitted(source, "success.cases").is_ok());
}

#[test]
fn native_list_descent_validates_the_entire_recursive_body() {
    let source = r#"fn lengthFrom(xs: List<Int>, acc: Int) -> Int
    match xs
        [] -> acc
        [_, ..rest] -> lengthFrom(rest, acc + 1)
verify lengthFrom law reflexive
    given xs: List<Int> = [[], [1, 2]]
    given acc: Int = [0, 4]
    because lengthFrom(xs, acc) == lengthFrom(xs, acc)
    using []
    lengthFrom(xs, acc) => lengthFrom(xs, acc)
"#;
    assert!(emitted(source, "lengthFrom.reflexive").is_ok());
    let hidden = source.replace("[] -> acc", "[] -> String.byteLength(\"unsupported\")");
    let error = emitted(&hidden, "lengthFrom.reflexive").unwrap_err();
    assert!(
        error.contains("unsupported call String.byteLength"),
        "{error}"
    );
}

#[test]
fn list_arguments_do_not_make_unchecked_recursion_native() {
    let source = r#"fn loop(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [_, ..rest] -> loop(xs)
verify loop law reflexive
    given xs: List<Int> = [[]]
    because true
    using []
    loop(xs) => loop(xs)
"#;
    let error = emitted(source, "loop.reflexive").unwrap_err();
    assert!(error.contains("recursive"), "{error}");
}

#[test]
fn unsupported_builtins_in_unused_bindings_still_decline() {
    let source = r#"fn identity(xs: List<Int>) -> List<Int>
    ignored = String.toUtf8("abc")
    xs
verify identity law reflexive
    given xs: List<Int> = [[], [1]]
    because true
    using []
    identity(xs) => xs
"#;
    let error = emitted(source, "identity.reflexive").unwrap_err();
    assert!(error.contains("unsupported call String.toUtf8"), "{error}");
}
