use super::{
    VerifyEmitMode, generate_prelude, proof_mode_issues, recurrence, transpile,
    transpile_for_cert_model, transpile_for_proof_mode, transpile_with_verify_mode,
};
use crate::ast::{
    BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, TailCallData, TopLevel,
    TypeDef, TypeVariant, VerifyBlock, VerifyGiven, VerifyGivenDomain, VerifyKind, VerifyLaw,
};

/// Shorthand: wrap an Expr in Spanned with line=0.
fn sb(e: Expr) -> Spanned<Expr> {
    Spanned::bare(e)
}
/// Shorthand: wrap an Expr in Box<Spanned> with line=0.
fn sbb(e: Expr) -> Box<Spanned<Expr>> {
    Box::new(Spanned::bare(e))
}
use crate::codegen::{CodegenContext, build_context};
use crate::source::parse_source;

use std::collections::{HashMap, HashSet};
use std::sync::Arc as Rc;

/// Populate `ctx.proof_ir` from the current items. Synthetic-AST
/// tests bypass the pipeline (where ProofLower runs automatically);
/// IR-pinned law strategies rely on this being populated so the
/// backend's IR-pin lookups can fire.
fn populate_proof_ir(ctx: &mut CodegenContext) {
    // Mirror the production order: BuildSymbols → proof_lower. The
    // populate side asserts the symbol table is present (it's a
    // hard prerequisite for FnId-keyed fn_contracts), so synthetic-
    // ctx tests have to build it the same way `refresh_facts` does.
    ctx.symbol_table = crate::ir::SymbolTable::build(&ctx.items, &ctx.modules);
    let resolved_items = crate::ir::hir::resolve_program(&ctx.symbol_table, &ctx.items);
    ctx.resolved_program = crate::codegen::program_view::ResolvedProgramView::build(
        resolved_items,
        &ctx.modules,
        &ctx.symbol_table,
    );
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    ctx.proof_ir = crate::codegen::proof_lower::lower(&inputs);
}

fn empty_ctx() -> CodegenContext {
    CodegenContext {
        items: vec![],
        type_defs: vec![],
        fn_defs: vec![],
        project_name: "verify_mode".to_string(),
        modules: vec![],
        capabilities: Default::default(),
        module_prefixes: HashSet::new(),
        policy: None,
        emit_replay_runtime: false,
        runtime_policy_from_env: false,
        guest_entry: None,
        emit_self_host_support: false,
        mutual_tco_members: HashSet::<crate::ir::FnId>::new(),
        recursive_fns: HashSet::<crate::ir::FnId>::new(),
        buffer_build_sinks: HashMap::new(),
        buffer_fusion_sites: Vec::new(),
        synthesized_buffered_fns: Vec::new(),
        packed_sequence_layouts: HashMap::new(),
        proof_ir: crate::ir::ProofIR::default(),
        symbol_table: crate::ir::SymbolTable::default(),
        current_module_scope: std::cell::RefCell::new(None),
        lean_do_block: std::cell::Cell::new(false),
        declined_claims: std::cell::RefCell::new(std::collections::BTreeMap::new()),
        substituted_compile_errors: std::cell::RefCell::new(Vec::new()),
        omitted_verify_cases: std::cell::RefCell::new(Vec::new()),
        resolved_program: crate::codegen::program_view::ResolvedProgramView::default(),
        program_shape: None,
        mir_program: None,
        bare_i64: Default::default(),
        discovered_lemmas: Vec::new(),
        sample_expected: std::collections::HashMap::new(),
        declined_cases: std::collections::HashMap::new(),
        allow_mathlib: false,
        hand_proofs: Default::default(),
    }
}

fn ctx_from_source(source: &str, project_name: &str) -> CodegenContext {
    let mut items = parse_source(source).expect("source should parse");
    // Proof-mode minimal pipeline: only the stages a proof
    // exporter actually consumes. Resolve / last_use / escape /
    // interp_lower / buffer_build all rewrite item shapes in
    // ways that break the recursion classifier's source-level
    // pattern matching (e.g. escape inlines a record into the
    // caller, dropping the recursive call's structural shape).
    // Analyze stays on — `recursive_fns` is what `proof_lower`
    // reads to decide which fns to classify. The flags shape the
    // runtime-facing half of this run only: the proof view below
    // is snapshotted before those passes either way.
    let mut pipeline_result = crate::ir::pipeline::run(
        &mut items,
        crate::ir::PipelineConfig {
            run_tco: true,
            typecheck: Some(crate::ir::TypecheckMode::Full {
                base_dir: Some(env!("CARGO_MANIFEST_DIR")),
            }),
            run_interp_lower: false,
            run_buffer_build: false,
            run_chars_fusion: false,
            run_string_index: false,
            run_list_build: false,
            run_resolve: false,
            run_last_use: false,
            run_analyze: true,
            run_escape: false,
            run_refinement_lower: true,
            run_interval_analyze: false,
            run_contract_lower: true,
            run_law_lower: true,
            // BuildSymbols is needed for fn_contracts lookup
            // (keyed by opaque FnId resolved through the symbol
            // table since the FnKey → FnId migration).
            run_build_symbols: true,
            dep_modules: &[],
            alloc_policy: None,
            call_ctx: None,
            on_after_pass: None,
        },
    );
    let tc = pipeline_result
        .typecheck
        .take()
        .expect("typecheck requested");
    assert!(
        tc.errors.is_empty(),
        "source should typecheck without errors: {:?}",
        tc.errors
    );
    let proof_ir = pipeline_result.proof_ir.take();
    // Assemble through `codegen_view`, the way every proof-facing
    // caller does: with the proof stages on it hands back the AST as
    // of the snapshot point, never the post-pass `items`.
    let view = pipeline_result.codegen_view(items);
    let mut ctx = build_context(
        view.items,
        &tc,
        view.analysis.as_ref(),
        project_name.to_string(),
        vec![],
        view.symbol_table,
        view.resolved_items,
    );
    if let Some(ir) = proof_ir {
        ctx.proof_ir = ir;
    }
    ctx
}

/// Concatenate every emitted `.lean` source (entry + per-module +
/// `AverCommon`) into a single string for content assertions. The
/// unified emitter splits prelude (`AverCommon.lean`) and body
/// (`<Project>.lean`) into separate files; tests originally checked
/// for substrings against the legacy single-file output, so the
/// helper now returns the concatenation so those substring assertions
/// keep working regardless of which file the content lands in.
fn generated_lean_file(out: &crate::codegen::ProjectOutput) -> String {
    out.files
        .iter()
        .filter_map(|(name, content)| {
            (name.ends_with(".lean") && name != "lakefile.lean").then_some(content.as_str())
        })
        .collect::<Vec<&str>>()
        .join("\n")
}

#[test]
fn nested_error_prop_lowers_to_monadic_bind_not_with_default() {
    let mut ctx = ctx_from_source(
        r#"
module Nested
    effects []

fn addOne(x: Int) -> Int
    x + 1

fn boundForm(n: Int, d: Int) -> Result<Int, String>
    ? "Propagate division errors before adding one."
    q = Int.div(n, d)?
    Result.Ok(addOne(q))

fn nestedArg(n: Int, d: Int) -> Result<Int, String>
    ? "Propagate division errors from a nested argument."
    Result.Ok(addOne(Int.div(n, d)?))
"#,
        "Nested",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    let nested_arg = lean
        .split_once("def nestedArg")
        .map(|(_, body)| body)
        .expect("nestedArg definition should be emitted");

    assert!(
        nested_arg.contains("(<- "),
        "nested `?` must lower to a monadic bind:\n{lean}"
    );
    assert!(
        !lean.contains("withDefault default"),
        "function-body `?` must not swallow errors:\n{lean}"
    );
    assert!(
        lean.contains("| .ok q =>")
            && lean.contains("| .error __qm_err_0 => Except.error __qm_err_0"),
        "statement-level `?` lowering must retain its short-circuiting match:\n{lean}"
    );
}

#[test]
fn cert_result_discharge_prelude_has_no_elaboration_active_attribute() {
    let mut ctx = ctx_from_source(
        r#"
module CertDischarge
    effects []

fn checkedHalf(n: Int) -> Result<Int, String>
    Result.Ok(Int.div(n, 2))
"#,
        "CertDischarge",
    );
    let out = transpile_for_cert_model(&mut ctx);
    let common = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "AverCommon.lean").then_some(content))
        .expect("certificate model should emit AverCommon.lean");

    assert!(common.contains("theorem proven_ok"), "{common}");
    assert!(
        !common.contains("@["),
        "certificate DATA must stay free of elaboration-active attributes:\n{common}"
    );
}

#[test]
fn nested_error_prop_in_match_arm_monadifies_the_branch() {
    let mut ctx = ctx_from_source(
        r#"
module NestedMatch
    effects []

fn divL(n: Int, d: Int) -> Result<Int, String>
    ? "Return a labeled error for a zero divisor."
    match d == 0
        true -> Result.Err("left")
        false -> Result.Ok(Int.div(n, d)?)
"#,
        "NestedMatch",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(
        lean.contains("(<- (do"),
        "a `?` inside a match arm needs a nested monadic action:\n{lean}"
    );
    assert!(
        !lean.contains("withDefault default"),
        "match-arm `?` must not swallow errors:\n{lean}"
    );
}

#[test]
fn a_mutual_group_passing_a_computed_value_does_not_claim_a_sizeof_measure() {
    let source = r#"
module Climb
    effects []

fn pairsOf(level: List<Int>, acc: List<Int>) -> List<Int>
    ? "Halve the level, two entries at a time."
    match level
        [] -> List.reverse(acc)
        [left, ..rest] -> pairsOf(rest, List.concat(acc, [left]))

fn rootFrom(level: List<Int>) -> Result<Int, String>
    ? "Fold the level down until one entry is left."
    match level
        [] -> Result.Err("empty")
        [only, ..rest] -> settleOrClimb(only, rest, level)

fn settleOrClimb(only: Int, rest: List<Int>, level: List<Int>) -> Result<Int, String>
    ? "One entry is the answer; more than one means another level."
    match rest
        [] -> Result.Ok(only)
        [head, ..tail] -> climb(level)

fn climb(level: List<Int>) -> Result<Int, String>
    ? "One level of pairing, then look again."
    next = pairsOf(level, [])
    rootFrom(next)

fn checked(level: List<Int>) -> Result<Int, String>
    ? "Delegate to the fuel-lowered group."
    rootFrom(level)

verify checked
    checked([7]) => Result.Ok(7)

verify checked law singleton
    given n: Int = 0..1
    checked([n]) => Result.Ok(n)
"#;
    let mut ctx = ctx_from_source(source, "Climb");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    // `next` is a value `climb` COMPUTED, so whether it is smaller than
    // `level` is a fact about `pairsOf`, not about the shape of the call.
    // Claiming `sizeOf` anyway states a `decreasing_by` obligation nothing can
    // close, and the module loses every claim in it.
    assert!(
        !lean.contains("termination_by (sizeOf level"),
        "a computed step must not be claimed as a plain `sizeOf` decrease:\n{lean}"
    );
    assert!(
        lean.contains("climb__fuel"),
        "the group must still be lowered, on the fuel path:\n{lean}"
    );
    assert!(
        lean.contains("-- verify checked: the Lean call cone reaches fuel-lowered function(s)"),
        "a sampled claim over the fuel group must be refused explicitly:\n{lean}"
    );
    assert!(
        !lean.contains("example : checked [7]"),
        "the fuel-bounded sample must never reach native_decide:\n{lean}"
    );
    let declined = ctx.declined_claims.borrow();
    let refusal = declined
        .get("cases:checked")
        .expect("the fuel refusal must reach the driver's structured channel");
    assert!(
        refusal.reason.contains("rootFrom") && refusal.reason.contains("default value"),
        "the refusal must name both the fuel function and the soundness risk: {refusal:?}"
    );
    let law_refusal = declined
        .get("law:checked.singleton")
        .expect("a law over the same fuel call cone must also be refused");
    assert!(law_refusal.reason.contains("rootFrom"));
    assert!(
        !lean.contains("theorem checked_law_singleton"),
        "neither the universal nor sampled law theorem may survive the fuel gate:\n{lean}"
    );

    // Explicitly unproven modes remain useful authoring tools: they never ask
    // native evaluation for a truth value, so the soundness refusal must not
    // suppress the obligation a human intends to fill.
    let mut skeleton_ctx = ctx_from_source(source, "Climb");
    let skeleton_out = transpile_for_proof_mode(&mut skeleton_ctx, VerifyEmitMode::TheoremSkeleton);
    let skeleton = generated_lean_file(&skeleton_out);
    assert!(skeleton.contains("theorem checked_verify_1"));
    assert!(skeleton.contains("theorem checked_law_singleton"));
    assert!(skeleton_ctx.declined_claims.borrow().is_empty());
}

#[test]
fn a_record_carrying_a_refined_type_derives_no_inhabited() {
    let mut ctx = ctx_from_source(
        r#"
module Carrier
    effects []

record Natural
    value: Int

fn fromInt(n: Int) -> Result<Natural, String>
    ? "Smart constructor: admits only non-negative ints."
    match n >= 0
        true -> Result.Ok(Natural(value = n))
        false -> Result.Err("must be non-negative")

record Held
    payload: Natural
    line: Int

fn lineOf(held: Held) -> Int
    held.line
"#,
        "Carrier",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    // A refined type is emitted as a `Subtype` with deliberately no
    // `Inhabited`, so a record carrying one has no witness either. Claiming
    // the instance anyway fails the derive and takes every claim in the
    // module with it -- including claims that never mention the field.
    assert!(
        lean.contains("abbrev Natural :="),
        "sanity: the fixture's carrier must be emitted as a refined type:\n{lean}"
    );
    let held = lean
        .split_once("structure Held where")
        .map(|(_, rest)| rest)
        .expect("Held is emitted");
    let derives = held
        .lines()
        .find(|line| line.trim_start().starts_with("deriving"))
        .expect("Held carries a deriving clause");
    assert!(
        !derives.contains("Inhabited"),
        "a record carrying a refined type must not claim an `Inhabited` it has no witness for, got `{derives}`:\n{lean}"
    );
}

#[test]
fn error_prop_on_the_expected_side_is_bound_not_defaulted() {
    let mut ctx = ctx_from_source(
        r#"
module ExpectedSide
    effects []

fn quotient(n: Int, d: Int) -> Result<Int, String>
    ? "Divide two integers."
    Int.div(n, d)

verify quotient
    quotient(10, 2)? => quotient(20, 4)?
"#,
        "ExpectedSide",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    // Both sides carry `?`, so both are `Except` actions. The expected side
    // used to be emitted with no `do` context, where `?` fell back to
    // `withDefault default` -- a value the model cannot stand behind: it asks
    // for an `Inhabited` instance the type may not have (a refined type such
    // as `Bytes` deliberately has none, and then nothing in the module
    // builds), and where the instance exists, a case that fails at run time is
    // still true as a theorem whenever the expected value equals that default.
    assert!(
        lean.contains("= (do pure ((<- quotient 20 4)))"),
        "a `?` on the expected side must be bound, not defaulted:\n{lean}"
    );
    assert!(
        !lean.contains("withDefault default"),
        "the expected side must not substitute a default on the error path:\n{lean}"
    );
}

#[test]
fn error_prop_in_verify_case_states_an_except_action() {
    let mut ctx = ctx_from_source(
        r#"
module VerifyCase
    effects []

fn addOne(x: Int) -> Int
    x + 1

fn quotient(n: Int, d: Int) -> Result<Int, String>
    ? "Divide two integers."
    Int.div(n, d)

verify quotient
    addOne(quotient(10, 2)?) => 6
    quotient(10, 2) => Result.Ok(5)
"#,
        "VerifyCase",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    let examples: Vec<&str> = lean
        .lines()
        .filter(|line| line.starts_with("example :"))
        .collect();

    assert!(
        examples.iter().any(
            |line| line.contains("(do pure (addOne (<- quotient 10 2)))")
                && line.contains("= Except.ok (6)")
        ),
        "a verify case carrying `?` must state an `Except` action:\n{lean}"
    );
    assert!(
        !lean.contains("withDefault default"),
        "verify-case `?` must not substitute a default on the error path:\n{lean}"
    );
    // The `do` context is scoped to the cases that need it: a case without
    // `?` keeps the plain equation, neither wrapped nor lifted.
    assert!(
        examples
            .iter()
            .any(|line| line.starts_with("example : quotient 10 2 = Except.ok 5 ")),
        "a verify case without `?` must keep its plain equation:\n{lean}"
    );
}

#[test]
fn error_prop_in_law_states_an_except_action_in_all_three_statements() {
    let mut ctx = ctx_from_source(
        r#"
module LawStatement
    effects []

fn addOne(x: Int) -> Int
    x + 1

fn safe(n: Int) -> Result<Int, String>
    ? "Fails for negative input."
    match n < 0
        true -> Result.Err("negative")
        false -> Result.Ok(n)

verify safe law shifts
    given a: Int = 1..2
    addOne(safe(a)?) => a + 1
"#,
        "LawStatement",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    // A law emits three kinds of statement, and all three must say the same
    // thing about `?`: the quantified universal, the checked-domain
    // conjunction, and one theorem per sample.
    assert!(
        lean.contains(
            "theorem safe_law_shifts : ∀ (a : Int), \
             (do pure (addOne (<- safe a))) = Except.ok ((a + 1))"
        ),
        "the universal law theorem must state an `Except` action:\n{lean}"
    );
    assert!(
        lean.contains(
            "theorem safe_law_shifts_checked_domain : \
             ((do pure (addOne (<- safe 1))) = Except.ok ((1 + 1))) ∧ \
             ((do pure (addOne (<- safe 2))) = Except.ok ((2 + 1)))"
        ),
        "the checked-domain conjunction must state `Except` actions:\n{lean}"
    );
    assert!(
        lean.contains(
            "theorem safe_law_shifts_sample_1 : \
             (do pure (addOne (<- safe 1))) = Except.ok ((1 + 1))"
        ),
        "each sample theorem must state an `Except` action:\n{lean}"
    );
    assert!(
        !lean.contains("withDefault default"),
        "law `?` must not substitute a default on the error path:\n{lean}"
    );
}

#[test]
fn law_without_error_prop_keeps_its_plain_statements() {
    let mut ctx = ctx_from_source(
        r#"
module PlainLaw
    effects []

fn addOne(x: Int) -> Int
    x + 1

verify addOne law shifts
    given a: Int = 1..2
    addOne(a) => a + 1
"#,
        "PlainLaw",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    // The `do` context is scoped to the statements that need it: a law
    // without `?` is neither wrapped nor lifted.
    assert!(
        lean.contains("theorem addOne_law_shifts : ∀ (a : Int), addOne a = (a + 1)"),
        "a law without `?` must keep its plain universal:\n{lean}"
    );
    assert!(
        lean.contains("theorem addOne_law_shifts_sample_1 : addOne 1 = (1 + 1)"),
        "a law without `?` must keep its plain samples:\n{lean}"
    );
    assert!(
        !lean.contains("do pure"),
        "a law without `?` must not gain a `do` context:\n{lean}"
    );
}

fn empty_ctx_with_verify_case() -> CodegenContext {
    let mut ctx = empty_ctx();
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "f".to_string(),
        line: 1,
        cases: vec![(
            sb(Expr::Literal(Literal::Int(1))),
            sb(Expr::Literal(Literal::Int(1))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Cases,
        trace: false,
        cases_givens: vec![],
    }));
    ctx
}

fn empty_ctx_with_two_verify_blocks_same_fn() -> CodegenContext {
    let mut ctx = empty_ctx();
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "f".to_string(),
        line: 1,
        cases: vec![(
            sb(Expr::Literal(Literal::Int(1))),
            sb(Expr::Literal(Literal::Int(1))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Cases,
        trace: false,
        cases_givens: vec![],
    }));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "f".to_string(),
        line: 2,
        cases: vec![(
            sb(Expr::Literal(Literal::Int(2))),
            sb(Expr::Literal(Literal::Int(2))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Cases,
        trace: false,
        cases_givens: vec![],
    }));
    ctx
}

fn empty_ctx_with_verify_law() -> CodegenContext {
    let mut ctx = empty_ctx();
    let add = FnDef {
        name: "add".to_string(),
        line: 1,
        params: vec![
            ("a".to_string(), "Int".to_string()),
            ("b".to_string(), "Int".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::BinOp(
            BinOp::Add,
            sbb(Expr::Ident("a".to_string())),
            sbb(Expr::Ident("b".to_string())),
        )))),
        resolution: None,
    };
    ctx.fn_defs.push(add.clone());
    ctx.items.push(TopLevel::FnDef(add));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "add".to_string(),
        line: 1,
        cases: vec![
            (
                sb(Expr::FnCall(
                    sbb(Expr::Ident("add".to_string())),
                    vec![
                        sb(Expr::Literal(Literal::Int(1))),
                        sb(Expr::Literal(Literal::Int(2))),
                    ],
                )),
                sb(Expr::FnCall(
                    sbb(Expr::Ident("add".to_string())),
                    vec![
                        sb(Expr::Literal(Literal::Int(2))),
                        sb(Expr::Literal(Literal::Int(1))),
                    ],
                )),
            ),
            (
                sb(Expr::FnCall(
                    sbb(Expr::Ident("add".to_string())),
                    vec![
                        sb(Expr::Literal(Literal::Int(2))),
                        sb(Expr::Literal(Literal::Int(3))),
                    ],
                )),
                sb(Expr::FnCall(
                    sbb(Expr::Ident("add".to_string())),
                    vec![
                        sb(Expr::Literal(Literal::Int(3))),
                        sb(Expr::Literal(Literal::Int(2))),
                    ],
                )),
            ),
        ],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "commutative".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "a".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::IntRange { start: 1, end: 2 },
                },
                VerifyGiven {
                    name: "b".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![
                        sb(Expr::Literal(Literal::Int(2))),
                        sb(Expr::Literal(Literal::Int(3))),
                    ]),
                },
            ],
            when: None,
            lhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Ident("a".to_string())),
                    sb(Expr::Ident("b".to_string())),
                ],
            )),
            rhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Ident("b".to_string())),
                    sb(Expr::Ident("a".to_string())),
                ],
            )),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));
    ctx
}

#[test]
fn prelude_normalizes_float_string_format() {
    let prelude = generate_prelude();
    assert!(
        prelude.contains("private def normalizeFloatString (s : String) : String :="),
        "missing normalizeFloatString helper in prelude"
    );
    assert!(
        prelude.contains(
            "def String.fromFloat (f : Float) : String := normalizeFloatString (toString f)"
        ),
        "String.fromFloat should normalize Lean float formatting"
    );
}

#[test]
fn prelude_validates_char_from_code_unicode_bounds() {
    let prelude = generate_prelude();
    assert!(
        prelude.contains("if n < 0 || n > 1114111 then none"),
        "String.fromCodePoint should reject code points above Unicode max"
    );
    assert!(
        prelude.contains("else if n >= 55296 && n <= 57343 then none"),
        "String.fromCodePoint should reject surrogate code points"
    );
}

#[test]
fn prelude_includes_map_set_helper_lemmas() {
    let prelude = generate_prelude();
    assert!(
        prelude.contains("theorem has_set_self [DecidableEq α]"),
        "missing AverMap.has_set_self helper theorem"
    );
    assert!(
        prelude.contains("theorem get_set_self [DecidableEq α]"),
        "missing AverMap.get_set_self helper theorem"
    );
}

#[test]
fn lean_output_without_map_usage_omits_map_prelude() {
    let mut ctx = ctx_from_source(
        r#"
module NoMap
    intent = "Simple pure program without maps."

fn addOne(n: Int) -> Int
    n + 1

verify addOne
    addOne(1) => 2
"#,
        "nomap",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(
        !lean.contains("namespace AverMap"),
        "did not expect AverMap prelude in program without map usage:\n{}",
        lean
    );
}

#[test]
fn reserved_lean_tokens_as_type_names_are_escaped() {
    // `Type` / `Prop` are legal Aver type names (the lexer reserves only
    // lowercase keywords) but Lean syntax tokens — declarations AND every
    // reference (signatures, match subjects, ctor spellings) must carry the
    // trailing-quote guard. A variant named `Match` lowers to Lean's
    // reserved `match` and needs the same guard.
    let mut ctx = ctx_from_source(
        r#"
module Reserved
    intent = "Reserved Lean tokens as user type names."

type Type
    Scalar
    Match
    Vector(Int)

record Prop
    value: Int

fn describe(t: Type) -> Int
    match t
        Type.Scalar -> 0
        Type.Match -> 1
        Type.Vector(n) -> n

fn propValue(p: Prop) -> Int
    p.value

fn mkMatch() -> Type
    Type.Match
"#,
        "reserved",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(
        lean.contains("inductive Type' where"),
        "sum type named `Type` must escape its declaration:\n{}",
        lean
    );
    assert!(
        lean.contains("structure Prop' where"),
        "record named `Prop` must escape its declaration:\n{}",
        lean
    );
    assert!(
        lean.contains("(t : Type')") && lean.contains("(p : Prop')"),
        "signature references to reserved type names must escape:\n{}",
        lean
    );
    assert!(
        lean.contains("| match'"),
        "variant `Match` lowers to reserved `match` and must escape:\n{}",
        lean
    );
    assert!(
        lean.contains("Type'.match'"),
        "constructor references must escape both segments:\n{}",
        lean
    );
    assert!(
        !lean.contains("inductive Type where") && !lean.contains("structure Prop where"),
        "raw reserved declarations must not survive:\n{}",
        lean
    );
}

#[test]
#[should_panic(
    expected = "variants `Accept` and `accept` of type `Verdict` both lower to Lean constructor `accept`"
)]
fn case_colliding_variants_are_rejected_at_lean_emission() {
    // `Accept` and `accept` are distinct, legal Aver variants (the
    // parser takes any Ident; PascalCase is only a lint warning), but
    // `lean_ctor_name` lowercases the first letter, so both would emit
    // the SAME Lean constructor — pre-fix the generated inductive
    // carried a duplicate constructor and the resulting Lean error
    // pointed at the generated project, far from the Aver source. The
    // emitter must fail loudly and name both variants and the type.
    let mut ctx = ctx_from_source(
        r#"
module CtorCase
    intent = "Two variants differing only in first-letter case."

type Verdict
    Accept
    accept

fn pick() -> Verdict
    Verdict.Accept
"#,
        "ctorcase",
    );
    let _ = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
}

#[test]
fn transpile_emits_native_decide_for_verify_by_default() {
    let mut ctx = empty_ctx_with_verify_case();
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("example : 1 = 1 := by native_decide"));
}

#[test]
fn transpile_can_emit_sorry_for_verify_when_requested() {
    let mut ctx = empty_ctx_with_verify_case();
    let out = transpile_with_verify_mode(&mut ctx, VerifyEmitMode::Sorry);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("example : 1 = 1 := by sorry"));
}

#[test]
fn transpile_can_emit_theorem_skeletons_for_verify() {
    let mut ctx = empty_ctx_with_verify_case();
    let out = transpile_with_verify_mode(&mut ctx, VerifyEmitMode::TheoremSkeleton);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("theorem f_verify_1 : 1 = 1 := by"));
    assert!(lean.contains("  sorry"));
    assert!(lean.contains("namespace Verify_mode"));
    assert!(lean.contains("end Verify_mode"));
}

#[test]
fn theorem_skeleton_numbering_is_global_per_function_across_verify_blocks() {
    let mut ctx = empty_ctx_with_two_verify_blocks_same_fn();
    let out = transpile_with_verify_mode(&mut ctx, VerifyEmitMode::TheoremSkeleton);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("theorem f_verify_1 : 1 = 1 := by"));
    assert!(lean.contains("theorem f_verify_2 : 2 = 2 := by"));
}

#[test]
fn transpile_emits_named_theorems_for_verify_law() {
    let mut ctx = empty_ctx_with_verify_law();
    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("-- verify law add.commutative (2 cases)"));
    assert!(lean.contains("-- given a: Int = 1..2"));
    assert!(lean.contains("-- given b: Int = [2, 3]"));
    assert!(
        lean.contains(
            "theorem add_law_commutative : ∀ (a : Int) (b : Int), add a b = add b a := by"
        )
    );
    assert!(lean.contains("  intro a b"));
    assert!(lean.contains("  simp [add, Int.add_comm]"));
    assert!(
        lean.contains(
            "theorem add_law_commutative_sample_1 : add 1 2 = add 2 1 := by native_decide"
        )
    );
    assert!(
        lean.contains(
            "theorem add_law_commutative_sample_2 : add 2 3 = add 3 2 := by native_decide"
        )
    );
}

#[test]
fn generate_prelude_emits_int_roundtrip_theorem() {
    let lean = generate_prelude();
    assert!(lean.contains(
        "theorem Int.fromString_fromInt : ∀ n : Int, Int.fromString (String.fromInt n) = .ok n"
    ));
    assert!(lean.contains("theorem String.intercalate_empty_chars (s : String) :"));
    assert!(lean.contains("def splitOnCharGo"));
    assert!(lean.contains("theorem split_single_char_append"));
    assert!(lean.contains("theorem split_intercalate_trailing_single_char"));
    assert!(lean.contains("namespace AverDigits"));
    assert!(lean.contains("theorem String.charAt_length_none (s : String)"));
    assert!(lean.contains("theorem digitChar_not_ws : ∀ d : Nat, d < 10 ->"));
}

#[test]
fn transpile_emits_guarded_theorems_for_verify_law_when_clause() {
    let mut ctx = ctx_from_source(
        r#"
module GuardedLaw
    intent =
        "verify law with precondition"

fn pickGreater(a: Int, b: Int) -> Int
    match a > b
        true -> a
        false -> b

verify pickGreater law ordered
    given a: Int = [1, 2]
    given b: Int = [1, 2]
    when a > b
    pickGreater(a, b) => a
"#,
        "guarded_law",
    );
    let out = transpile_with_verify_mode(&mut ctx, VerifyEmitMode::TheoremSkeleton);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("-- when (a > b)"));
    assert!(lean.contains(
            "theorem pickGreater_law_ordered : ∀ (a : Int) (b : Int), a = 1 ∨ a = 2 -> b = 1 ∨ b = 2 -> (a > b) = true -> pickGreater a b = a := by"
        ));
    // Sample guards Int-ascribe their substituted numerals: a bare
    // `(1 - 2 > 0)` premise would elaborate over Nat, where truncated
    // subtraction can make the theorem FALSE AS STATED.
    assert!(lean.contains(
            "theorem pickGreater_law_ordered_sample_1 : ((1 : Int) > (1 : Int)) = true -> pickGreater 1 1 = 1 := by"
        ));
    assert!(lean.contains(
            "theorem pickGreater_law_ordered_sample_4 : ((2 : Int) > (2 : Int)) = true -> pickGreater 2 2 = 2 := by"
        ));
}

#[test]
fn transpile_uses_spec_theorem_names_for_declared_spec_laws() {
    let mut ctx = ctx_from_source(
        r#"
module SpecDemo
    intent =
        "spec demo"

fn absVal(x: Int) -> Int
    match x < 0
        true -> 0 - x
        false -> x

fn absValSpec(x: Int) -> Int
    match x < 0
        true -> 0 - x
        false -> x

verify absVal law absValSpec
    given x: Int = [-2, -1, 0, 1, 2]
    absVal(x) => absValSpec(x)
"#,
        "spec_demo",
    );
    let out = transpile_with_verify_mode(&mut ctx, VerifyEmitMode::TheoremSkeleton);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("-- verify law absVal.spec absValSpec (5 cases)"));
    assert!(
        lean.contains("theorem absVal_eq_absValSpec : ∀ (x : Int), absVal x = absValSpec x := by")
    );
    assert!(lean.contains("theorem absVal_eq_absValSpec_checked_domain :"));
    assert!(lean.contains("theorem absVal_eq_absValSpec_sample_1 :"));
    assert!(!lean.contains("theorem absVal_law_absValSpec :"));
}

#[test]
fn transpile_keeps_noncanonical_spec_laws_as_regular_law_names() {
    let mut ctx = ctx_from_source(
        r#"
module SpecLawShape
    intent =
        "shape probe"

fn foo(x: Int) -> Int
    x + 1

fn fooSpec(seed: Int, x: Int) -> Int
    x + seed

verify foo law fooSpec
    given x: Int = [1, 2]
    foo(x) => fooSpec(1, x)
"#,
        "spec_law_shape",
    );
    let out = transpile_with_verify_mode(&mut ctx, VerifyEmitMode::TheoremSkeleton);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("-- verify law foo.fooSpec (2 cases)"));
    assert!(lean.contains("theorem foo_law_fooSpec : ∀ (x : Int), foo x = fooSpec 1 x := by"));
    assert!(!lean.contains("theorem foo_eq_fooSpec :"));
}

#[test]
fn transpile_auto_proves_linear_int_canonical_spec_law_in_auto_mode() {
    let mut ctx = ctx_from_source(
        r#"
module SpecGap
    intent =
        "nontrivial canonical spec law"

fn inc(x: Int) -> Int
    x + 1

fn incSpec(x: Int) -> Int
    x + 2 - 1

verify inc law incSpec
    given x: Int = [0, 1, 2]
    inc(x) => incSpec(x)
"#,
        "spec_gap",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("-- verify law inc.spec incSpec (3 cases)"));
    assert!(lean.contains("theorem inc_eq_incSpec : ∀ (x : Int), inc x = incSpec x := by"));
    assert!(lean.contains("change (x + 1) = ((x + 2) - 1)"));
    assert!(lean.contains("omega"));
    assert!(!lean.contains(
        "-- universal theorem inc_eq_incSpec omitted: sampled law shape is not auto-proved yet"
    ));
    assert!(lean.contains("theorem inc_eq_incSpec_checked_domain :"));
}

#[test]
fn transpile_auto_proves_guarded_canonical_spec_law_in_auto_mode() {
    let mut ctx = ctx_from_source(
        r#"
module GuardedSpecGap
    intent =
        "guarded canonical spec law"

fn clampNonNegative(x: Int) -> Int
    match x < 0
        true -> 0
        false -> x

fn clampNonNegativeSpec(x: Int) -> Int
    match x < 0
        true -> 0
        false -> x

verify clampNonNegative law clampNonNegativeSpec
    given x: Int = [-2, -1, 0, 1, 2]
    when x >= 0
    clampNonNegative(x) => clampNonNegativeSpec(x)
"#,
        "guarded_spec_gap",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("-- when (x >= 0)"));
    assert!(lean.contains(
            "theorem clampNonNegative_eq_clampNonNegativeSpec : ∀ (x : Int), x = (-2) ∨ x = (-1) ∨ x = 0 ∨ x = 1 ∨ x = 2 -> (x >= 0) = true -> clampNonNegative x = clampNonNegativeSpec x := by"
        ));
    assert!(lean.contains("intro x h_x h_when"));
    assert!(lean.contains("simpa [clampNonNegative, clampNonNegativeSpec]"));
    assert!(!lean.contains(
            "-- universal theorem clampNonNegative_eq_clampNonNegativeSpec omitted: sampled law shape is not auto-proved yet"
        ));
    assert!(!lean.contains("cases h_x"));
}

#[test]
fn transpile_proves_conditional_comparison_bridge_law_as_universal() {
    // `prop_70 leSucc`: `when le(m, n) -> le(m, S n) => true`. The conditional
    // comparison-bridge emit closes it as the TRUE-universal
    // `∀ m n, le m n = true -> le m (n + 1) = true` — the sampled-domain
    // disjunctions are dropped (`omit_domain`) so the law is classed `universal`,
    // and the premise + goal are bridged to `≤` and discharged by `omega`.
    let mut ctx = ctx_from_source(
        r#"
module CondCmpBridge
    intent = "conditional comparison bridge: le(m,n) implies le(m, S n)"

type Nat
    Z
    S(Nat)

fn le(x: Nat, y: Nat) -> Bool
    match x
        Nat.Z -> true
        Nat.S(z) -> match y
            Nat.Z -> false
            Nat.S(x2) -> le(z, x2)

verify le law leSucc
    given m: Nat = [Nat.Z, Nat.S(Nat.Z)]
    given n: Nat = [Nat.Z, Nat.S(Nat.Z)]
    when le(m, n)
    le(m, Nat.S(n)) => true
"#,
        "cond_cmp_bridge",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    // Classed `universal`, NOT `bounded-domain`.
    assert!(lean.contains("-- aver:law-class le_law_leSucc universal le.leSucc"));
    // The TRUE-universal conditional statement: no `m = 0 ∨ …` sampled-domain
    // premise, the `when` survives as the `= true ->` implication.
    assert!(lean.contains(
        "theorem le_law_leSucc : ∀ (m : Nat) (n : Nat), le m n = true -> le m (n + 1) = true := by"
    ));
    // Law-scoped `≤` bridge support theorem (uid `le_leSucc`, no `_law_` so the
    // audit never mistakes it for a creditable main theorem).
    assert!(lean.contains("theorem le_leSucc_le_isNatLe : ∀ a b, (le a b = true) = (a ≤ b) := by"));
    // Premise introduced as `h_when`; bridged at hypothesis and goal, `omega` closes.
    assert!(lean.contains("intro m n h_when"));
    assert!(lean.contains("simp only [le_leSucc_le_isNatLe] at h_when ⊢ <;> omega"));
}

#[test]
fn transpile_renders_bounded_int_domain_from_proof_ir_plan() {
    let mut ctx = ctx_from_source(
        r#"
module BoundedInt
    intent = "finite guarded integer law"

fn inRange(i: Int) -> Bool
    i >= 0

verify inRange law seedTable
    given i: Int = [0, 1, 2, 3]
    when Bool.and(i >= 0, i < 4)
    inRange(i) => true
"#,
        "bounded_int",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("-- aver:law-class inRange_law_seedTable universal inRange.seedTable"));
    assert!(lean.contains(
        "theorem inRange_law_seedTable : ∀ (i : Int), 0 ≤ i → i < 4 → inRange i = true := by"
    ));
    assert!(lean.contains("exact (by decide : ∀ m : Nat, m < 4 → inRange (0 + (m : Int)) = true)"));
}

#[test]
fn transpile_proves_conditional_inductive_membership_law_as_universal() {
    // `prop_36 elemConcat`: `when elem(x,y) -> elem(x, y ++ z) => true` (append
    // monotonicity). The GENERIC conditional-inductive driver (no bespoke
    // membership emitter) closes it as the TRUE-universal `∀ x y z, elem x y =
    // true -> elem x (y ++ z) = true` by induction on the first list given,
    // threading the premise into the cons arm. Admitted with no helper-pool gate;
    // the partner list `z` is intro'd before the premise (`intro z h_when`).
    let mut ctx = ctx_from_source(
        r#"
module CondIndMembership
    intent = "conditional inductive membership: elem x y implies elem x (y ++ z)"

type Nat
    Z
    S(Nat)

fn eqNat(x: Nat, y: Nat) -> Bool
    match x
        Nat.Z -> match y
            Nat.Z -> true
            Nat.S(z) -> false
        Nat.S(x2) -> match y
            Nat.Z -> false
            Nat.S(y2) -> eqNat(x2, y2)

fn barbar(x: Bool, y: Bool) -> Bool
    match x
        true -> true
        false -> y

fn elem(x: Nat, y: List<Nat>) -> Bool
    match y
        [] -> false
        [z, ..xs] -> barbar(eqNat(x, z), elem(x, xs))

verify elem law elemConcat
    given x: Nat = [Nat.Z, Nat.S(Nat.Z)]
    given y: List<Nat> = [[Nat.Z], [Nat.S(Nat.Z)]]
    given z: List<Nat> = [[], [Nat.Z]]
    when elem(x, y)
    elem(x, List.concat(y, z)) => true
"#,
        "cond_ind_membership",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    // Classed `universal`, NOT `bounded-domain` (the sampled-domain disjunctions
    // are dropped via `omit_domain`).
    assert!(lean.contains("-- aver:law-class elem_law_elemConcat universal elem.elemConcat"));
    // The TRUE-universal conditional statement (no `y = [..] ∨ …` domain premise).
    assert!(lean.contains(
        "theorem elem_law_elemConcat : ∀ (x : Nat) (y : List Nat) (z : List Nat), elem x y = true -> elem x (y ++ z) = true := by"
    ));
    // Induction on the first list given `y`, with the partner list + premise
    // threaded INSIDE the cons arm (`intro z h_when` after `induction`, so the IH
    // carries the antecedent and generalizes over the partner).
    assert!(lean.contains("induction y with"));
    assert!(lean.contains("| cons hd tl ih =>"));
    assert!(lean.contains("intro z h_when"));
}

#[test]
fn transpile_proves_zip_rev_via_generic_decomposition() {
    // `prop_85 zipRev` closes through the DECOMPOSITION path, NOT a hardcoded
    // Lean template: a `zipSnocDistribution` helper `verify ... law` supplies the
    // algebraic content, and the GENERIC conditional-inductive driver proves
    // zipRev by list induction + `simp_all` over the fn defs AND that earlier
    // helper (the laws-as-lemmas pool). The premise `when` law is cited as the
    // conditional rewrite `… = true -> …`.
    let mut ctx = ctx_from_source(
        r#"
module ZipRev
    intent = "if len xs = len ys then zip (rev xs) (rev ys) = revPair (zip xs ys)"

type Nat
    Z
    S(Nat)

fn natEq(a: Nat, b: Nat) -> Bool
    match a
        Nat.Z -> match b
            Nat.Z -> true
            Nat.S(y) -> false
        Nat.S(x) -> match b
            Nat.Z -> false
            Nat.S(y) -> natEq(x, y)

fn len(xs: List<Int>) -> Nat
    match xs
        [] -> Nat.Z
        [y, ..ys] -> Nat.S(len(ys))

fn appendInt(xs: List<Int>, ys: List<Int>) -> List<Int>
    match xs
        [] -> ys
        [z, ..zs] -> List.concat([z], appendInt(zs, ys))

fn appendPair(xs: List<Tuple<Int, Int>>, ys: List<Tuple<Int, Int>>) -> List<Tuple<Int, Int>>
    match xs
        [] -> ys
        [z, ..zs] -> List.concat([z], appendPair(zs, ys))

fn rev(xs: List<Int>) -> List<Int>
    match xs
        [] -> []
        [y, ..ys] -> appendInt(rev(ys), [y])

fn revPair(xs: List<Tuple<Int, Int>>) -> List<Tuple<Int, Int>>
    match xs
        [] -> []
        [y, ..ys] -> appendPair(revPair(ys), [y])

fn zip(xs: List<Int>, ys: List<Int>) -> List<Tuple<Int, Int>>
    match xs
        [] -> []
        [z, ..x2] -> match ys
            [] -> []
            [x3, ..x4] -> List.concat([(z, x3)], zip(x2, x4))

verify len law lenSnocSucc
    given a: List<Int> = [[], [1], [1, 2]]
    given x: Int = [9]
    len(appendInt(a, [x])) => Nat.S(len(a))

verify zip law zipSnocDistribution
    given as: List<Int> = [[], [1]]
    given bs: List<Int> = [[], [4]]
    given x: Int = [7]
    given y: Int = [9]
    when natEq(len(as), len(bs))
    zip(appendInt(as, [x]), appendInt(bs, [y])) => appendPair(zip(as, bs), [(x, y)])

verify zip law zipRev
    given xs: List<Int> = [[], [1], [1, 2]]
    given ys: List<Int> = [[], [4], [4, 5]]
    when natEq(len(xs), len(ys))
    zip(rev(xs), rev(ys)) => revPair(zip(xs, ys))
"#,
        "zip_rev",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    // zipRev is classed `universal` and proved by the GENERIC driver: list
    // induction threading the premise, with the snoc-distribution HELPER LAW
    // cited from the pool inside the proof's `simp_all` set.
    assert!(lean.contains("-- aver:law-class zip_law_zipRev universal zip.zipRev"));
    assert!(lean.contains(
        "theorem zip_law_zipRev : ∀ (xs : List Int) (ys : List Int), natEq (len xs) (len ys) = true -> zip (rev xs) (rev ys) = revPair (zip xs ys) := by"
    ));
    assert!(lean.contains("induction xs with"));
    assert!(lean.contains("zip_law_zipSnocDistribution"));
    // No hardcoded per-figure template (the old `zip_rev_supports_body` path).
    assert!(!lean.contains("zip_zipRev_snoc"));
}

#[test]
fn transpile_auto_proves_simp_normalized_canonical_spec_law_in_auto_mode() {
    let mut ctx = ctx_from_source(
        r#"
module SpecGapNonlinear
    intent =
        "nonlinear canonical spec law"

fn square(x: Int) -> Int
    x * x

fn squareSpec(x: Int) -> Int
    x * x + 0

verify square law squareSpec
    given x: Int = [0, 1, 2]
    square(x) => squareSpec(x)
"#,
        "spec_gap_nonlinear",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("-- verify law square.spec squareSpec (3 cases)"));
    assert!(
        lean.contains("theorem square_eq_squareSpec : ∀ (x : Int), square x = squareSpec x := by")
    );
    assert!(lean.contains("simp [square, squareSpec]"));
    assert!(!lean.contains(
            "-- universal theorem square_eq_squareSpec omitted: sampled law shape is not auto-proved yet"
        ));
    assert!(lean.contains("theorem square_eq_squareSpec_checked_domain :"));
    assert!(lean.contains("theorem square_eq_squareSpec_sample_1 :"));
}

#[test]
fn transpile_auto_proves_reflexive_law_with_rfl() {
    let mut ctx = empty_ctx();
    // After the phase-E3 migration LawTheorem.fn_id is resolved
    // through the symbol table at populate time, so the law's
    // target fn must exist as a FnDef in `ctx.items`. Pre-fix
    // the verify block alone was enough (FnKey was constructed
    // from the bare name without checking).
    let id_law = FnDef {
        name: "idLaw".to_string(),
        line: 1,
        params: vec![("x".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Ident("x".to_string())))),
        resolution: None,
    };
    ctx.fn_defs.push(id_law.clone());
    ctx.items.push(TopLevel::FnDef(id_law));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "idLaw".to_string(),
        line: 1,
        cases: vec![(
            sb(Expr::Literal(Literal::Int(1))),
            sb(Expr::Literal(Literal::Int(1))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "reflexive".to_string(),
            givens: vec![VerifyGiven {
                name: "x".to_string(),
                type_name: "Int".to_string(),
                domain: VerifyGivenDomain::IntRange { start: 1, end: 2 },
            }],
            when: None,
            lhs: sb(Expr::Ident("x".to_string())),
            rhs: sb(Expr::Ident("x".to_string())),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));
    // Synthetic-AST test bypasses the parser + pipeline, so the
    // LawLower stage hasn't run. refresh_facts populates
    // ProofIR.law_theorems with Reflexive on `x => x` — backend's
    // Step-24 reader checks the IR to emit rfl.
    ctx.refresh_facts();
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("theorem idLaw_law_reflexive : ∀ (x : Int), x = x := by"));
    assert!(lean.contains("  intro x"));
    assert!(lean.contains("  rfl"));
}

#[test]
fn transpile_auto_proves_identity_law_for_int_add_wrapper() {
    let mut ctx = empty_ctx_with_verify_law();
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "add".to_string(),
        line: 10,
        cases: vec![(
            sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(1))),
                    sb(Expr::Literal(Literal::Int(0))),
                ],
            )),
            sb(Expr::Literal(Literal::Int(1))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "identityZero".to_string(),
            givens: vec![VerifyGiven {
                name: "a".to_string(),
                type_name: "Int".to_string(),
                domain: VerifyGivenDomain::Explicit(vec![
                    sb(Expr::Literal(Literal::Int(0))),
                    sb(Expr::Literal(Literal::Int(1))),
                ]),
            }],
            when: None,
            lhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Ident("a".to_string())),
                    sb(Expr::Literal(Literal::Int(0))),
                ],
            )),
            rhs: sb(Expr::Ident("a".to_string())),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));
    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("theorem add_law_identityZero : ∀ (a : Int), add a 0 = a := by"));
    assert!(lean.contains("  intro a"));
    assert!(lean.contains("  simp [add]"));
}

#[test]
fn transpile_auto_proves_associative_law_for_int_add_wrapper() {
    let mut ctx = empty_ctx_with_verify_law();
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "add".to_string(),
        line: 20,
        cases: vec![(
            sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::FnCall(
                        sbb(Expr::Ident("add".to_string())),
                        vec![
                            sb(Expr::Literal(Literal::Int(1))),
                            sb(Expr::Literal(Literal::Int(2))),
                        ],
                    )),
                    sb(Expr::Literal(Literal::Int(3))),
                ],
            )),
            sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(1))),
                    sb(Expr::FnCall(
                        sbb(Expr::Ident("add".to_string())),
                        vec![
                            sb(Expr::Literal(Literal::Int(2))),
                            sb(Expr::Literal(Literal::Int(3))),
                        ],
                    )),
                ],
            )),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "associative".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "a".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(1)))]),
                },
                VerifyGiven {
                    name: "b".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(2)))]),
                },
                VerifyGiven {
                    name: "c".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(3)))]),
                },
            ],
            when: None,
            lhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::FnCall(
                        sbb(Expr::Ident("add".to_string())),
                        vec![
                            sb(Expr::Ident("a".to_string())),
                            sb(Expr::Ident("b".to_string())),
                        ],
                    )),
                    sb(Expr::Ident("c".to_string())),
                ],
            )),
            rhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Ident("a".to_string())),
                    sb(Expr::FnCall(
                        sbb(Expr::Ident("add".to_string())),
                        vec![
                            sb(Expr::Ident("b".to_string())),
                            sb(Expr::Ident("c".to_string())),
                        ],
                    )),
                ],
            )),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));
    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains(
            "theorem add_law_associative : ∀ (a : Int) (b : Int) (c : Int), add (add a b) c = add a (add b c) := by"
        ));
    assert!(lean.contains("  intro a b c"));
    assert!(lean.contains("  simp [add, Int.add_assoc]"));
}

#[test]
fn transpile_auto_proves_sub_laws() {
    let mut ctx = empty_ctx();
    let sub = FnDef {
        name: "sub".to_string(),
        line: 1,
        params: vec![
            ("a".to_string(), "Int".to_string()),
            ("b".to_string(), "Int".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::BinOp(
            BinOp::Sub,
            sbb(Expr::Ident("a".to_string())),
            sbb(Expr::Ident("b".to_string())),
        )))),
        resolution: None,
    };
    ctx.fn_defs.push(sub.clone());
    ctx.items.push(TopLevel::FnDef(sub));

    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "sub".to_string(),
        line: 10,
        cases: vec![(
            sb(Expr::FnCall(
                sbb(Expr::Ident("sub".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(2))),
                    sb(Expr::Literal(Literal::Int(0))),
                ],
            )),
            sb(Expr::Literal(Literal::Int(2))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "rightIdentity".to_string(),
            givens: vec![VerifyGiven {
                name: "a".to_string(),
                type_name: "Int".to_string(),
                domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(2)))]),
            }],
            when: None,
            lhs: sb(Expr::FnCall(
                sbb(Expr::Ident("sub".to_string())),
                vec![
                    sb(Expr::Ident("a".to_string())),
                    sb(Expr::Literal(Literal::Int(0))),
                ],
            )),
            rhs: sb(Expr::Ident("a".to_string())),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "sub".to_string(),
        line: 20,
        cases: vec![(
            sb(Expr::FnCall(
                sbb(Expr::Ident("sub".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(2))),
                    sb(Expr::Literal(Literal::Int(1))),
                ],
            )),
            sb(Expr::Neg(sbb(Expr::FnCall(
                sbb(Expr::Ident("sub".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(1))),
                    sb(Expr::Literal(Literal::Int(2))),
                ],
            )))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "antiCommutative".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "a".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(2)))]),
                },
                VerifyGiven {
                    name: "b".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(1)))]),
                },
            ],
            when: None,
            lhs: sb(Expr::FnCall(
                sbb(Expr::Ident("sub".to_string())),
                vec![
                    sb(Expr::Ident("a".to_string())),
                    sb(Expr::Ident("b".to_string())),
                ],
            )),
            rhs: sb(Expr::Neg(sbb(Expr::FnCall(
                sbb(Expr::Ident("sub".to_string())),
                vec![
                    sb(Expr::Ident("b".to_string())),
                    sb(Expr::Ident("a".to_string())),
                ],
            )))),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));

    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("theorem sub_law_rightIdentity : ∀ (a : Int), sub a 0 = a := by"));
    assert!(lean.contains("  simp [sub]"));
    assert!(lean.contains(
        "theorem sub_law_antiCommutative : ∀ (a : Int) (b : Int), sub a b = (-sub b a) := by"
    ));
    assert!(lean.contains("  simpa [sub] using (Int.neg_sub b a).symm"));
}

#[test]
fn transpile_auto_proves_unary_wrapper_equivalence_law() {
    let mut ctx = empty_ctx();
    let add = FnDef {
        name: "add".to_string(),
        line: 1,
        params: vec![
            ("a".to_string(), "Int".to_string()),
            ("b".to_string(), "Int".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::BinOp(
            BinOp::Add,
            sbb(Expr::Ident("a".to_string())),
            sbb(Expr::Ident("b".to_string())),
        )))),
        resolution: None,
    };
    let add_one = FnDef {
        name: "addOne".to_string(),
        line: 2,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::BinOp(
            BinOp::Add,
            sbb(Expr::Ident("n".to_string())),
            sbb(Expr::Literal(Literal::Int(1))),
        )))),
        resolution: None,
    };
    ctx.fn_defs.push(add.clone());
    ctx.fn_defs.push(add_one.clone());
    ctx.items.push(TopLevel::FnDef(add));
    ctx.items.push(TopLevel::FnDef(add_one));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "addOne".to_string(),
        line: 3,
        cases: vec![(
            sb(Expr::FnCall(
                sbb(Expr::Ident("addOne".to_string())),
                vec![sb(Expr::Literal(Literal::Int(2)))],
            )),
            sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(2))),
                    sb(Expr::Literal(Literal::Int(1))),
                ],
            )),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "identityViaAdd".to_string(),
            givens: vec![VerifyGiven {
                name: "n".to_string(),
                type_name: "Int".to_string(),
                domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(2)))]),
            }],
            when: None,
            lhs: sb(Expr::FnCall(
                sbb(Expr::Ident("addOne".to_string())),
                vec![sb(Expr::Ident("n".to_string()))],
            )),
            rhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Ident("n".to_string())),
                    sb(Expr::Literal(Literal::Int(1))),
                ],
            )),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));
    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(
        lean.contains("theorem addOne_law_identityViaAdd : ∀ (n : Int), addOne n = add n 1 := by")
    );
    assert!(lean.contains("  simp [addOne, add]"));
}

#[test]
fn transpile_auto_proves_direct_map_set_laws() {
    let mut ctx = empty_ctx();

    // Stub FnDef for the verify target — see analogous note in
    // `transpile_auto_proves_reflexive_law_with_rfl`.
    let map_fn = FnDef {
        name: "map".to_string(),
        line: 1,
        params: vec![],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Literal(Literal::Int(0))))),
        resolution: None,
    };
    ctx.fn_defs.push(map_fn.clone());
    ctx.items.push(TopLevel::FnDef(map_fn));

    let map_set = |m: Spanned<Expr>, k: Spanned<Expr>, v: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "set".to_string(),
            )),
            vec![m, k, v],
        ))
    };
    let map_has = |m: Spanned<Expr>, k: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "has".to_string(),
            )),
            vec![m, k],
        ))
    };
    let map_get = |m: Spanned<Expr>, k: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "get".to_string(),
            )),
            vec![m, k],
        ))
    };
    let some = |v: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "Some".to_string(),
            )),
            vec![v],
        ))
    };
    let map_empty = || {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "empty".to_string(),
            )),
            vec![],
        ))
    };

    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "map".to_string(),
        line: 1,
        cases: vec![(
            map_has(
                map_set(
                    sb(Expr::Ident("m".to_string())),
                    sb(Expr::Ident("k".to_string())),
                    sb(Expr::Ident("v".to_string())),
                ),
                sb(Expr::Ident("k".to_string())),
            ),
            sb(Expr::Literal(Literal::Bool(true))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "setHasKey".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "m".to_string(),
                    type_name: "Map<String, Int>".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![map_empty()]),
                },
                VerifyGiven {
                    name: "k".to_string(),
                    type_name: "String".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Str(
                        "a".to_string(),
                    )))]),
                },
                VerifyGiven {
                    name: "v".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(1)))]),
                },
            ],
            when: None,
            lhs: map_has(
                map_set(
                    sb(Expr::Ident("m".to_string())),
                    sb(Expr::Ident("k".to_string())),
                    sb(Expr::Ident("v".to_string())),
                ),
                sb(Expr::Ident("k".to_string())),
            ),
            rhs: sb(Expr::Literal(Literal::Bool(true))),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));

    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "map".to_string(),
        line: 2,
        cases: vec![(
            map_get(
                map_set(
                    sb(Expr::Ident("m".to_string())),
                    sb(Expr::Ident("k".to_string())),
                    sb(Expr::Ident("v".to_string())),
                ),
                sb(Expr::Ident("k".to_string())),
            ),
            some(sb(Expr::Ident("v".to_string()))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "setGetKey".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "m".to_string(),
                    type_name: "Map<String, Int>".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![map_empty()]),
                },
                VerifyGiven {
                    name: "k".to_string(),
                    type_name: "String".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Str(
                        "a".to_string(),
                    )))]),
                },
                VerifyGiven {
                    name: "v".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(1)))]),
                },
            ],
            when: None,
            lhs: map_get(
                map_set(
                    sb(Expr::Ident("m".to_string())),
                    sb(Expr::Ident("k".to_string())),
                    sb(Expr::Ident("v".to_string())),
                ),
                sb(Expr::Ident("k".to_string())),
            ),
            rhs: some(sb(Expr::Ident("v".to_string()))),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));

    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("simpa using AverMap.has_set_self m k v"));
    assert!(lean.contains("simpa using AverMap.get_set_self m k v"));
}

#[test]
fn transpile_auto_proves_direct_recursive_sum_law_by_structural_induction() {
    let mut ctx = ctx_from_source(
        r#"
module Mirror
    intent =
        "direct recursive sum induction probe"

type Tree
    Leaf(Int)
    Node(Tree, Tree)

fn mirror(t: Tree) -> Tree
    match t
        Tree.Leaf(v) -> Tree.Leaf(v)
        Tree.Node(left, right) -> Tree.Node(mirror(right), mirror(left))

verify mirror law involutive
    given t: Tree = [Tree.Leaf(1), Tree.Node(Tree.Leaf(1), Tree.Leaf(2))]
    mirror(mirror(t)) => t
"#,
        "mirror",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(
        lean.contains("theorem mirror_law_involutive : ∀ (t : Tree), mirror (mirror t) = t := by")
    );
    assert!(lean.contains("  induction t with"));
    assert!(
        lean.contains(
            "  | leaf f0 => first | (simp [Mirror.mirror]; done) | (simp [Mirror.mirror]; omega) | sorry"
        )
    );
    assert!(lean.contains(
        "  | node f0 f1 ih0 ih1 => first | (simp_all [Mirror.mirror]; done) | (simp_all [Mirror.mirror]; omega) | sorry"
        ));
    assert!(!lean.contains(
            "-- universal theorem mirror_law_involutive omitted: sampled law shape is not auto-proved yet"
        ));
}

#[test]
fn transpile_auto_proves_map_update_laws() {
    let mut ctx = empty_ctx();

    let map_get = |m: Spanned<Expr>, k: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "get".to_string(),
            )),
            vec![m, k],
        ))
    };
    let map_set = |m: Spanned<Expr>, k: Spanned<Expr>, v: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "set".to_string(),
            )),
            vec![m, k, v],
        ))
    };
    let map_has = |m: Spanned<Expr>, k: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "has".to_string(),
            )),
            vec![m, k],
        ))
    };
    let option_some = |v: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "Some".to_string(),
            )),
            vec![v],
        ))
    };
    let option_with_default = |opt: Spanned<Expr>, def: Spanned<Expr>| {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "withDefault".to_string(),
            )),
            vec![opt, def],
        ))
    };
    let map_empty = || {
        sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "empty".to_string(),
            )),
            vec![],
        ))
    };

    let add_one = FnDef {
        name: "addOne".to_string(),
        line: 1,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::BinOp(
            BinOp::Add,
            sbb(Expr::Ident("n".to_string())),
            sbb(Expr::Literal(Literal::Int(1))),
        )))),
        resolution: None,
    };
    ctx.fn_defs.push(add_one.clone());
    ctx.items.push(TopLevel::FnDef(add_one));

    let inc_count = FnDef {
        name: "incCount".to_string(),
        line: 2,
        params: vec![
            ("counts".to_string(), "Map<String, Int>".to_string()),
            ("word".to_string(), "String".to_string()),
        ],
        return_type: "Map<String, Int>".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::Block(vec![
            Stmt::Binding(
                "current".to_string(),
                None,
                map_get(
                    sb(Expr::Ident("counts".to_string())),
                    sb(Expr::Ident("word".to_string())),
                ),
            ),
            Stmt::Expr(sb(Expr::Match {
                subject: sbb(Expr::Ident("current".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Option.Some".to_string(),
                            vec!["n".to_string()],
                        ),
                        body: Box::new(map_set(
                            sb(Expr::Ident("counts".to_string())),
                            sb(Expr::Ident("word".to_string())),
                            sb(Expr::BinOp(
                                BinOp::Add,
                                sbb(Expr::Ident("n".to_string())),
                                sbb(Expr::Literal(Literal::Int(1))),
                            )),
                        )),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                    MatchArm {
                        pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
                        body: Box::new(map_set(
                            sb(Expr::Ident("counts".to_string())),
                            sb(Expr::Ident("word".to_string())),
                            sb(Expr::Literal(Literal::Int(1))),
                        )),
                        binding_slots: std::sync::OnceLock::new(),
                    },
                ],
            })),
        ])),
        resolution: None,
    };
    ctx.fn_defs.push(inc_count.clone());
    ctx.items.push(TopLevel::FnDef(inc_count));

    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "incCount".to_string(),
        line: 10,
        cases: vec![(
            map_has(
                sb(Expr::FnCall(
                    sbb(Expr::Ident("incCount".to_string())),
                    vec![
                        sb(Expr::Ident("counts".to_string())),
                        sb(Expr::Ident("word".to_string())),
                    ],
                )),
                sb(Expr::Ident("word".to_string())),
            ),
            sb(Expr::Literal(Literal::Bool(true))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "keyPresent".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "counts".to_string(),
                    type_name: "Map<String, Int>".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![map_empty()]),
                },
                VerifyGiven {
                    name: "word".to_string(),
                    type_name: "String".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Str(
                        "a".to_string(),
                    )))]),
                },
            ],
            when: None,
            lhs: map_has(
                sb(Expr::FnCall(
                    sbb(Expr::Ident("incCount".to_string())),
                    vec![
                        sb(Expr::Ident("counts".to_string())),
                        sb(Expr::Ident("word".to_string())),
                    ],
                )),
                sb(Expr::Ident("word".to_string())),
            ),
            rhs: sb(Expr::Literal(Literal::Bool(true))),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));

    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "incCount".to_string(),
        line: 20,
        cases: vec![(
            map_get(
                sb(Expr::FnCall(
                    sbb(Expr::Ident("incCount".to_string())),
                    vec![
                        sb(Expr::Ident("counts".to_string())),
                        sb(Expr::Literal(Literal::Str("a".to_string()))),
                    ],
                )),
                sb(Expr::Literal(Literal::Str("a".to_string()))),
            ),
            option_some(sb(Expr::FnCall(
                sbb(Expr::Ident("addOne".to_string())),
                vec![option_with_default(
                    map_get(
                        sb(Expr::Ident("counts".to_string())),
                        sb(Expr::Literal(Literal::Str("a".to_string()))),
                    ),
                    sb(Expr::Literal(Literal::Int(0))),
                )],
            ))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "existingKeyIncrements".to_string(),
            givens: vec![VerifyGiven {
                name: "counts".to_string(),
                type_name: "Map<String, Int>".to_string(),
                domain: VerifyGivenDomain::Explicit(vec![map_empty()]),
            }],
            when: None,
            lhs: map_get(
                sb(Expr::FnCall(
                    sbb(Expr::Ident("incCount".to_string())),
                    vec![
                        sb(Expr::Ident("counts".to_string())),
                        sb(Expr::Literal(Literal::Str("a".to_string()))),
                    ],
                )),
                sb(Expr::Literal(Literal::Str("a".to_string()))),
            ),
            rhs: option_some(sb(Expr::FnCall(
                sbb(Expr::Ident("addOne".to_string())),
                vec![option_with_default(
                    map_get(
                        sb(Expr::Ident("counts".to_string())),
                        sb(Expr::Literal(Literal::Str("a".to_string()))),
                    ),
                    sb(Expr::Literal(Literal::Int(0))),
                )],
            ))),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));

    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(
        lean.contains("cases h : AverMap.get counts word <;> simp [AverMap.has_set_self]"),
        "expected keyPresent auto-proof with has_set_self"
    );
    assert!(
        lean.contains(
            "cases h : AverMap.get counts \"a\" <;> simp [AverMap.get_set_self, incCount, addOne]"
        ),
        "expected existingKeyIncrements auto-proof with get_set_self"
    );
}

#[test]
fn transpile_parenthesizes_negative_int_call_args_in_law_samples() {
    let mut ctx = empty_ctx();
    let add = FnDef {
        name: "add".to_string(),
        line: 1,
        params: vec![
            ("a".to_string(), "Int".to_string()),
            ("b".to_string(), "Int".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::BinOp(
            BinOp::Add,
            sbb(Expr::Ident("a".to_string())),
            sbb(Expr::Ident("b".to_string())),
        )))),
        resolution: None,
    };
    ctx.fn_defs.push(add.clone());
    ctx.items.push(TopLevel::FnDef(add));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "add".to_string(),
        line: 1,
        cases: vec![(
            sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(-2))),
                    sb(Expr::Literal(Literal::Int(-1))),
                ],
            )),
            sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Literal(Literal::Int(-1))),
                    sb(Expr::Literal(Literal::Int(-2))),
                ],
            )),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "commutative".to_string(),
            givens: vec![
                VerifyGiven {
                    name: "a".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(-2)))]),
                },
                VerifyGiven {
                    name: "b".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(-1)))]),
                },
            ],
            when: None,
            lhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Ident("a".to_string())),
                    sb(Expr::Ident("b".to_string())),
                ],
            )),
            rhs: sb(Expr::FnCall(
                sbb(Expr::Ident("add".to_string())),
                vec![
                    sb(Expr::Ident("b".to_string())),
                    sb(Expr::Ident("a".to_string())),
                ],
            )),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));

    populate_proof_ir(&mut ctx);
    let out = transpile(&mut ctx);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains(
        "theorem add_law_commutative_sample_1 : add (-2) (-1) = add (-1) (-2) := by native_decide"
    ));
}

#[test]
fn verify_law_numbering_is_scoped_per_law_name() {
    let mut ctx = empty_ctx();
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("x".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Ident("x".to_string())))),
        resolution: None,
    };
    ctx.fn_defs.push(f.clone());
    ctx.items.push(TopLevel::FnDef(f));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "f".to_string(),
        line: 1,
        cases: vec![(
            sb(Expr::Literal(Literal::Int(1))),
            sb(Expr::Literal(Literal::Int(1))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Cases,
        trace: false,
        cases_givens: vec![],
    }));
    ctx.items.push(TopLevel::Verify(VerifyBlock {
        fn_name: "f".to_string(),
        line: 2,
        cases: vec![(
            sb(Expr::Literal(Literal::Int(2))),
            sb(Expr::Literal(Literal::Int(2))),
        )],
        case_spans: vec![],
        case_givens: vec![],
        case_hostile_origins: vec![],
        case_hostile_profiles: vec![],
        case_reverse_order: vec![],
        kind: VerifyKind::Law(Box::new(VerifyLaw {
            name: "identity".to_string(),
            givens: vec![VerifyGiven {
                name: "x".to_string(),
                type_name: "Int".to_string(),
                domain: VerifyGivenDomain::Explicit(vec![sb(Expr::Literal(Literal::Int(2)))]),
            }],
            when: None,
            lhs: sb(Expr::Ident("x".to_string())),
            rhs: sb(Expr::Ident("x".to_string())),
            sample_guards: vec![],
        })),
        trace: false,
        cases_givens: vec![],
    }));
    populate_proof_ir(&mut ctx);
    let out = transpile_with_verify_mode(&mut ctx, VerifyEmitMode::TheoremSkeleton);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("theorem f_verify_1 : 1 = 1 := by"));
    assert!(lean.contains("theorem f_law_identity : ∀ (x : Int), x = x := by"));
    assert!(lean.contains("theorem f_law_identity_sample_1 : 2 = 2 := by"));
    assert!(!lean.contains("theorem f_law_identity_sample_2 : 2 = 2 := by"));
}

#[test]
fn proof_mode_accepts_single_int_countdown_recursion() {
    let mut ctx = empty_ctx();
    let down = FnDef {
        name: "down".to_string(),
        line: 1,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::Ident("n".to_string())),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Int(0)),
                    body: sbb(Expr::Literal(Literal::Int(0))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "down".to_string(),
                        vec![sb(Expr::BinOp(
                            BinOp::Sub,
                            sbb(Expr::Ident("n".to_string())),
                            sbb(Expr::Literal(Literal::Int(1))),
                        ))],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(down.clone()));
    ctx.fn_defs.push(down);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected Int countdown recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    // No `Module` declaration in `ctx.items` ⇒ closed-world by the
    // entry-script rule in `is_closed_world_pure_fn`. No external
    // caller for `down` in this synthetic ctx, so the
    // single-caller-predicate extractor returns empty and the Lean
    // emitter defaults to `(h_dom : n ≥ 0)` — same fallback the
    // legacy fibTR path used. Body has the canonical `match n { 0 ->
    // 0; _ -> down(n-1) }` shape, so we land on the native guarded
    // emit instead of fuel.
    assert!(
        lean.contains("def down__aux (n : Int) (h_dom : n ≥ 0) : Int :="),
        "expected native aux def with default precondition, got:\n{}",
        lean
    );
    assert!(
        lean.contains("else down__aux (n - 1) (by omega)"),
        "expected aux recursive call with omega proof, got:\n{}",
        lean
    );
    assert!(lean.contains("termination_by Int.natAbs n"));
    assert!(lean.contains("def down (n : Int) : Int :="));
    assert!(lean.contains("if h_dom : n ≥ 0 then down__aux n h_dom"));
    assert!(!lean.contains("def down__fuel"));
}

/// Shared module body for the scan-lemma gate probes: `isDigit` is
/// the canonical single-String-param Bool predicate.
const SCAN_GATE_PRELUDE: &str = r#"module ScanGate
    intent = "scan-lemma synthesis gate probes"
    effects []

fn isDigit(c: String) -> Bool
    code = Option.withDefault(String.firstCodePoint(c), 0)
    match code >= 48
        true -> code <= 57
        false -> false
"#;

fn lean_for_scan_gate(scanner: &str) -> String {
    let source = format!("{SCAN_GATE_PRELUDE}\n{scanner}");
    let mut ctx = ctx_from_source(&source, "scan_gate");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    out.files
        .iter()
        .map(|(_, content)| content.as_str())
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn scan_lemma_emitted_for_canonical_pos_returning_scanner() {
    // Positive control for the gates below: EXIT mentions `pos`, so
    // the `<fn>__fuel_scan` companion is synthesized.
    let lean = lean_for_scan_gate(
        r#"fn scanEnd(s: String, pos: Int) -> Int
    match String.charAt(s, pos)
        Option.None -> pos
        Option.Some(c) -> match isDigit(c)
            true -> scanEnd(s, pos + 1)
            false -> 0 - 1
"#,
    );
    assert!(
        lean.contains("scanEnd__fuel_scan"),
        "expected the scan companion lemma for the canonical shape, got:\n{lean}"
    );
}

#[test]
fn scan_lemma_not_emitted_for_pos_free_exit_scanner() {
    // EXIT without any `pos` occurrence (a constant-exit validator):
    // the lemma template's `rw [hpos]` would have no work to do and
    // FAIL — a build error in the export. The recognizer must
    // decline so nothing is synthesized.
    let lean = lean_for_scan_gate(
        r#"fn scanAll(s: String, pos: Int) -> Bool
    match String.charAt(s, pos)
        Option.None -> true
        Option.Some(c) -> match isDigit(c)
            true -> scanAll(s, pos + 1)
            false -> false
"#,
    );
    assert!(
        !lean.contains("__fuel_scan"),
        "pos-free EXIT must not get a scan lemma, got:\n{lean}"
    );
    assert!(
        lean.contains("def scanAll__fuel"),
        "the scanner itself still gets its fuel emission, got:\n{lean}"
    );
}

#[test]
fn proof_mode_when_stronger_than_refinement_invariant_stays_in_theorem() {
    // Before fix: `when` was dropped unconditionally whenever a
    // given was refinement-lifted, on the assumption that `when`
    // restated the type's invariant. A user-written stronger
    // predicate (`when a >= 10` over `Natural` whose invariant is
    // `a.val >= 0`) would silently disappear from the emitted
    // theorem — the proof artifact would universally quantify
    // over ALL Naturals while the user's source claim was
    // restricted to `a >= 10`. After fix: `when_is_redundant_
    // with_refinement_lifts` compares user's predicate to the
    // type's invariant (via commutator-relaxed compare). Drop
    // only fires when they match.
    let src = "module Stronger\n\
             \x20   intent = \"t\"\n\
             \n\
             record Natural\n\
             \x20   value: Int\n\
             \n\
             fn fromInt(n: Int) -> Result<Natural, String>\n\
             \x20   match n >= 0\n\
             \x20       true  -> Result.Ok(Natural(value = n))\n\
             \x20       false -> Result.Err(\"must be >= 0\")\n\
             \n\
             fn identity(a: Natural) -> Natural\n\
             \x20   a\n\
             \n\
             verify identity law selfEq\n\
             \x20   given a: Int = [10, 20, 30]\n\
             \x20   when a >= 10\n\
             \x20   identity(Natural(value = a)) => identity(Natural(value = a))\n";
    let mut ctx = ctx_from_source(src, "stronger");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    // `when a >= 10` is STRONGER than Natural's `n >= 0` invariant
    // so the universal theorem must keep it as a premise — AND
    // project `a` to `a.val` because the quantifier is now over
    // the Subtype carrier, not the underlying Int. The bare-`a`
    // shape would fail `lake build` with `failed to synthesize LE
    // Natural / OfNat Natural 10`.
    let universal_theorem = lean
        .lines()
        .find(|l| l.contains("theorem identity_law_selfEq"))
        .unwrap_or_else(|| panic!("expected universal theorem line, got:\n{}", lean));
    assert!(
        universal_theorem.contains("a.val >= 10"),
        "expected `when a.val >= 10` (projected) in universal theorem premise, got:\n{}",
        universal_theorem
    );
    assert!(
        !universal_theorem.contains(" a >= 10"),
        "must NOT emit bare `a >= 10` — Subtype carrier has no `LE Natural` instance, got:\n{}",
        universal_theorem
    );
}

#[test]
fn proof_mode_when_compound_equivalent_to_compound_invariant_drops_cleanly() {
    // Regression guard for `examples/refinement/int_range/int_range.av`:
    // the refinement predicate itself is `Bool.and(n >= 0, n <=
    // 100)`. A naive bijective match (lift `[Bool.and(a >= 0, a <=
    // 100), Bool.and(b >= 0, b <= 100)]` against flattened `when`
    // `[a >= 0, a <= 100, b >= 0, b <= 100]`) would length-
    // mismatch and keep the redundant premise — re-introducing
    // the type-mismatch shape that pre-fix already worked around.
    // The fix flattens BOTH sides; this test pins that.
    let src = "module IR\n\
             \x20   intent = \"t\"\n\
             \n\
             record IntRange\n\
             \x20   value: Int\n\
             \n\
             fn fromInt(n: Int) -> Result<IntRange, String>\n\
             \x20   match Bool.and(n >= 0, n <= 100)\n\
             \x20       true  -> Result.Ok(IntRange(value = n))\n\
             \x20       false -> Result.Err(\"oob\")\n\
             \n\
             fn identity(a: IntRange) -> IntRange\n\
             \x20   a\n\
             \n\
             verify identity law selfEq\n\
             \x20   given a: Int = [0, 50, 100]\n\
             \x20   when Bool.and(a >= 0, a <= 100)\n\
             \x20   identity(IntRange(value = a)) => identity(IntRange(value = a))\n";
    let mut ctx = ctx_from_source(src, "ir");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    let universal_theorem = lean
        .lines()
        .find(|l| l.contains("theorem identity_law_selfEq"))
        .unwrap_or_else(|| panic!("expected universal theorem line, got:\n{}", lean));
    assert!(
        !universal_theorem.contains("a >= 0"),
        "expected compound `when` to be dropped when it matches compound invariant, got:\n{}",
        universal_theorem
    );
    assert!(
        !universal_theorem.contains("a <= 100"),
        "expected compound `when` to be dropped when it matches compound invariant, got:\n{}",
        universal_theorem
    );
    assert!(
        universal_theorem.contains("∀ (a : IntRange)"),
        "expected universal to quantify over IntRange, got:\n{}",
        universal_theorem
    );
}

#[test]
fn proof_mode_when_equivalent_to_refinement_invariant_drops_cleanly() {
    // Regression: the typical natural.av-style case — `when a >=
    // 0` over `Natural` (invariant `n >= 0`) — must continue to
    // drop the redundant `when` so the universal theorem is `∀ (a
    // : Natural), ...` not `∀ (a : Natural), a.val >= 0 -> ...`
    // (which Lean type-mismatches against the Subtype carrier).
    let src = "module Equiv\n\
             \x20   intent = \"t\"\n\
             \n\
             record Natural\n\
             \x20   value: Int\n\
             \n\
             fn fromInt(n: Int) -> Result<Natural, String>\n\
             \x20   match n >= 0\n\
             \x20       true  -> Result.Ok(Natural(value = n))\n\
             \x20       false -> Result.Err(\"must be >= 0\")\n\
             \n\
             fn identity(a: Natural) -> Natural\n\
             \x20   a\n\
             \n\
             verify identity law selfEq\n\
             \x20   given a: Int = [0, 1, 2]\n\
             \x20   when a >= 0\n\
             \x20   identity(Natural(value = a)) => identity(Natural(value = a))\n";
    let mut ctx = ctx_from_source(src, "equiv");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    let universal_theorem = lean
        .lines()
        .find(|l| l.contains("theorem identity_law_selfEq"))
        .unwrap_or_else(|| panic!("expected universal theorem line, got:\n{}", lean));
    // Predicate equivalent to invariant → drop.
    assert!(
        !universal_theorem.contains("a >= 0"),
        "expected redundant `when a >= 0` to be dropped from universal theorem, got:\n{}",
        universal_theorem
    );
    assert!(
        universal_theorem.contains("∀ (a : Natural)"),
        "expected universal to quantify over Natural, got:\n{}",
        universal_theorem
    );
}

#[test]
fn proof_mode_refinement_lift_escapes_reserved_type_binder() {
    // The refinement-lift quantifier binder interpolated the IR's canonical
    // refined-type key as raw Lean type text. A refined record named `Type`
    // (legal Aver — the lexer reserves only lowercase keywords) then emitted
    // `∀ (a : Type), …` against a declaration escaped to `Type'`, so the
    // theorem referenced Lean's `Type` sort instead of the record. The
    // binder must route through the same spelling helper as every other
    // type reference.
    let src = "module Lift\n\
             \x20   intent = \"t\"\n\
             \n\
             record Type\n\
             \x20   value: Int\n\
             \n\
             fn fromInt(n: Int) -> Result<Type, String>\n\
             \x20   match n >= 0\n\
             \x20       true  -> Result.Ok(Type(value = n))\n\
             \x20       false -> Result.Err(\"must be >= 0\")\n\
             \n\
             fn identity(a: Type) -> Type\n\
             \x20   a\n\
             \n\
             verify identity law selfEq\n\
             \x20   given a: Int = [0, 1, 2]\n\
             \x20   when a >= 0\n\
             \x20   identity(Type(value = a)) => identity(Type(value = a))\n";
    let mut ctx = ctx_from_source(src, "lift");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    let universal_theorem = lean
        .lines()
        .find(|l| l.contains("theorem identity_law_selfEq"))
        .unwrap_or_else(|| panic!("expected universal theorem line, got:\n{}", lean));
    assert!(
        universal_theorem.contains("∀ (a : Type')"),
        "refinement-lifted binder must escape the reserved type name, got:\n{}",
        universal_theorem
    );
    assert!(
        !universal_theorem.contains("(a : Type)"),
        "the raw reserved-token binder must not survive, got:\n{}",
        universal_theorem
    );
}

#[test]
fn proof_mode_non_zero_base_literal_falls_back_to_fuel() {
    // Conservative guard: native IntCountdownGuarded emit assumes
    // the aux's default `(h_dom : p ≥ 0)` precondition, under
    // which only `match p { 0 -> ... }` proves preservation
    // (wildcard arm gives `p ≠ 0`, with `p ≥ 0` that's `p ≥ 1`,
    // so `p - 1 ≥ 0`). A non-zero base literal like `match p { 5
    // -> ... }` would let `p = 0` reach the wildcard arm and
    // recurse with `p - 1 = -1`, breaking the precondition.
    // `omega` would rightly reject it at lake build. Compiler
    // must reject the shape upfront — generalising to arbitrary
    // literals needs a real preservation check that doesn't
    // exist yet (follow-up). Falls back to fuel encoding.
    let src = "module Worker\n\
             \x20   intent = \"t\"\n\
             \n\
             fn worker(n: Int) -> Int\n\
             \x20   match n\n\
             \x20       3 -> n\n\
             \x20       _ -> worker(n - 1)\n\
             \n\
             fn caller(n: Int) -> Int\n\
             \x20   match n > 2\n\
             \x20       true -> match n < 500\n\
             \x20           true -> worker(n)\n\
             \x20           false -> 0\n\
             \x20       false -> 0\n";
    let mut ctx = ctx_from_source(src, "worker");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(
        !lean.contains("worker__aux"),
        "must NOT emit native aux when base literal != 0 — preservation isn't provable without a real linear-int check; got:\n{}",
        lean
    );
    assert!(
        lean.contains("def worker__fuel"),
        "expected fuel fallback for non-zero base literal, got:\n{}",
        lean
    );
}

#[test]
fn proof_mode_exposed_int_countdown_falls_back_to_fuel() {
    // Closed-world check: a fn that lives in a module with an
    // explicit `exposes [...]` listing the fn is open-world, so the
    // native-guarded path is unsafe (callers outside this artifact
    // could pass negative ints). The classifier must keep the fuel
    // encoding for these.
    let src = "module Down\n\
             \x20   intent = \"t\"\n\
             \x20   exposes [down]\n\
             \n\
             fn down(n: Int) -> Int\n\
             \x20   match n\n\
             \x20       0 -> 0\n\
             \x20       _ -> down(n - 1)\n";
    let mut ctx = ctx_from_source(src, "downmod");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(
        lean.contains("def down__fuel"),
        "expected fuel emission for exposed fn, got:\n{}",
        lean
    );
    assert!(
        !lean.contains("down__aux"),
        "should not emit native aux for exposed fn, got:\n{}",
        lean
    );
}

#[test]
fn proof_mode_accepts_single_int_countdown_on_nonfirst_param() {
    let mut ctx = empty_ctx();
    let repeat_like = FnDef {
        name: "repeatLike".to_string(),
        line: 1,
        params: vec![
            ("char".to_string(), "String".to_string()),
            ("n".to_string(), "Int".to_string()),
        ],
        return_type: "List<String>".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::BinOp(
                BinOp::Lte,
                sbb(Expr::Ident("n".to_string())),
                sbb(Expr::Literal(Literal::Int(0))),
            )),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: sbb(Expr::List(vec![])),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(false)),
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "repeatLike".to_string(),
                        vec![
                            sb(Expr::Ident("char".to_string())),
                            sb(Expr::BinOp(
                                BinOp::Sub,
                                sbb(Expr::Ident("n".to_string())),
                                sbb(Expr::Literal(Literal::Int(1))),
                            )),
                        ],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(repeat_like.clone()));
    ctx.fn_defs.push(repeat_like);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected non-first Int countdown recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("def repeatLike__fuel"));
    assert!(lean.contains("def repeatLike (char : String) (n : Int) : List String :="));
    assert!(lean.contains("repeatLike__fuel ((Int.natAbs n) + 1) char n"));
}

#[test]
fn proof_mode_accepts_negative_guarded_int_ascent() {
    let mut ctx = empty_ctx();
    let normalize = FnDef {
        name: "normalize".to_string(),
        line: 1,
        params: vec![("angle".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::BinOp(
                BinOp::Lt,
                sbb(Expr::Ident("angle".to_string())),
                sbb(Expr::Literal(Literal::Int(0))),
            )),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "normalize".to_string(),
                        vec![sb(Expr::BinOp(
                            BinOp::Add,
                            sbb(Expr::Ident("angle".to_string())),
                            sbb(Expr::Literal(Literal::Int(360))),
                        ))],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(false)),
                    body: sbb(Expr::Ident("angle".to_string())),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(normalize.clone()));
    ctx.fn_defs.push(normalize);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected negative-guarded Int ascent recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = out
        .files
        .iter()
        .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
        .expect("expected generated Lean file");
    assert!(lean.contains("def normalize__fuel"));
    assert!(lean.contains("normalize__fuel ((Int.natAbs angle) + 1) angle"));
}

#[test]
fn proof_mode_accepts_single_list_structural_recursion() {
    let mut ctx = empty_ctx();
    let len = FnDef {
        name: "len".to_string(),
        line: 1,
        params: vec![("xs".to_string(), "List<Int>".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::Ident("xs".to_string())),
            arms: vec![
                MatchArm {
                    pattern: Pattern::EmptyList,
                    body: sbb(Expr::Literal(Literal::Int(0))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Cons("h".to_string(), "t".to_string()),
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "len".to_string(),
                        vec![sb(Expr::Ident("t".to_string()))],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(len.clone()));
    ctx.fn_defs.push(len);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected List structural recursion to be accepted, got: {:?}",
        issues
    );
}

#[test]
fn proof_mode_accepts_single_list_structural_recursion_on_nonfirst_param() {
    let mut ctx = empty_ctx();
    let len_from = FnDef {
        name: "lenFrom".to_string(),
        line: 1,
        params: vec![
            ("count".to_string(), "Int".to_string()),
            ("xs".to_string(), "List<Int>".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::Ident("xs".to_string())),
            arms: vec![
                MatchArm {
                    pattern: Pattern::EmptyList,
                    body: sbb(Expr::Ident("count".to_string())),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Cons("h".to_string(), "t".to_string()),
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "lenFrom".to_string(),
                        vec![
                            sb(Expr::BinOp(
                                BinOp::Add,
                                sbb(Expr::Ident("count".to_string())),
                                sbb(Expr::Literal(Literal::Int(1))),
                            )),
                            sb(Expr::Ident("t".to_string())),
                        ],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(len_from.clone()));
    ctx.fn_defs.push(len_from);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected non-first List structural recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("termination_by xs.length"));
    assert!(!lean.contains("partial def lenFrom"));
}

#[test]
fn proof_mode_accepts_single_string_pos_advance_recursion() {
    let mut ctx = empty_ctx();
    let skip_ws = FnDef {
        name: "skipWs".to_string(),
        line: 1,
        params: vec![
            ("s".to_string(), "String".to_string()),
            ("pos".to_string(), "Int".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::FnCall(
                sbb(Expr::Attr(
                    sbb(Expr::Ident("String".to_string())),
                    "charAt".to_string(),
                )),
                vec![
                    sb(Expr::Ident("s".to_string())),
                    sb(Expr::Ident("pos".to_string())),
                ],
            )),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
                    body: sbb(Expr::Ident("pos".to_string())),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "skipWs".to_string(),
                        vec![
                            sb(Expr::Ident("s".to_string())),
                            sb(Expr::BinOp(
                                BinOp::Add,
                                sbb(Expr::Ident("pos".to_string())),
                                sbb(Expr::Literal(Literal::Int(1))),
                            )),
                        ],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(skip_ws.clone()));
    ctx.fn_defs.push(skip_ws);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected String+pos recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("def skipWs__fuel"));
    assert!(!lean.contains("partial def skipWs"));
}

#[test]
fn proof_mode_graduates_simple_string_pos_skipper_to_native() {
    // The simple string-position SKIP shape (skipWs family) graduates to a
    // native fuel-free `def` with a `termination_by` measure, so Lean derives
    // a real `.induct`. This SUBSUMES the #643 single-fn fuel-stability lemma:
    // the graduated shape has no `__fuel` wrapper and no `_stable` lemma
    // (native `.induct` is strictly stronger). The stability-lemma emission
    // path is now unreachable for single fns — see the `walk`-shape decline
    // test below, which pins that a NON-skip string-position fn stays fueled.
    let source = r#"module FuelStable
    intent = "simple string-position graduation"
    effects []

fn skipSpaces(s: String, pos: Int) -> Int
    match String.charAt(s, pos)
        Option.None -> pos
        Option.Some(c) -> match c
            " " -> skipSpaces(s, pos + 1)
            _ -> pos
"#;
    let mut ctx = ctx_from_source(source, "fuel_stable");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(
        lean.contains("def skipSpaces (s : String) (pos : Int) : Int :="),
        "expected native fuel-free skipSpaces def:\n{lean}"
    );
    assert!(
        lean.contains("termination_by (s.toList.length - pos.toNat)"),
        "expected the StringLenMinusPos measure as termination_by:\n{lean}"
    );
    assert!(
        lean.contains("String.charAt_some_bounds"),
        "decreasing_by must cite the prelude bounds lemma:\n{lean}"
    );
    assert!(
        !lean.contains("def skipSpaces__fuel"),
        "graduated skipper must NOT emit a fuel wrapper:\n{lean}"
    );
    assert!(
        !lean.contains("skipSpaces__fuel_stable"),
        "graduated skipper must NOT emit the fuel-stability lemma (subsumed by native):\n{lean}"
    );
}

#[test]
fn proof_mode_declines_stability_lemma_for_non_skip_string_pos_shape() {
    let source = r#"module FuelStableDecline
    intent = "string-position recursion outside the stability shape"
    effects []

fn walk(s: String, pos: Int) -> Int
    match String.charAt(s, pos)
        Option.None -> pos
        Option.Some(_) -> walk(s, pos + 1)
"#;
    let mut ctx = ctx_from_source(source, "fuel_stable_decline");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("def walk__fuel"));
    assert!(
        !lean.contains("walk__fuel_stable"),
        "out-of-shape string-position recursion must not get an unprobed stability lemma:\n{lean}"
    );
}

#[test]
fn proof_mode_declines_graduation_for_non_unit_advance_arm() {
    // FAIL-CLOSED graduation: a skip scanner whose inner char-match mixes the
    // exact `self(s, pos + 1)` advance with a NON-unit `self(s, pos + 2)` arm
    // must stay FUELED, not graduate — a native `def` cannot sorry-floor a
    // `decreasing_by`, so a step shape the recognizer cannot affirmatively
    // classify must never reach native emission. The upstream StringPosAdvance
    // classifier admits `pos + k` for any k >= 1, so this shape reaches the
    // gate; before the fix the `pos + 2` arm fell through the detector's
    // `_ => {}` and the sibling `pos + 1` arm graduated the whole fn. A fuel
    // wrapper always builds.
    let source = r#"module NonUnitAdvance
    intent = "mixed advance skip scanner"
    effects []

fn skipMixed(s: String, pos: Int) -> Int
    match String.charAt(s, pos)
        Option.None -> pos
        Option.Some(c) -> match c
            " " -> skipMixed(s, pos + 1)
            "x" -> skipMixed(s, pos + 2)
            _ -> pos
"#;
    let mut ctx = ctx_from_source(source, "non_unit_advance");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(
        lean.contains("def skipMixed__fuel"),
        "the mixed-advance scanner must stay fueled:\n{lean}"
    );
    assert!(
        !lean.contains("String.charAt_some_bounds")
            && !lean.contains("termination_by (s.toList.length - pos.toNat)"),
        "the mixed-advance scanner must NOT graduate to a native def:\n{lean}"
    );
}

#[test]
fn graduation_gate_declines_unclassifiable_arms() {
    // Direct fail-closed net on the graduation recognizer. The upstream
    // classifier rejects some of these shapes to `partial def`, so the gate
    // is exercised in isolation: a self-call arm that is NOT the exact
    // `self(s, pos + 1)` advance, or an outer arm that is not the exact
    // charAt none/some pair, must make `detect_simple_string_pos_skip_literal`
    // decline — anything it accepts is emitted as a native `def` whose
    // `decreasing_by` cannot be sorry-floored.
    use crate::codegen::lean::toplevel::fuel::detect_simple_string_pos_skip_literal;
    fn fd_named<'a>(ctx: &'a CodegenContext, name: &str) -> &'a FnDef {
        ctx.fn_defs
            .iter()
            .find(|fd| fd.name == name)
            .unwrap_or_else(|| panic!("fn {name} not found"))
    }

    // Positive control: the canonical uniform `pos + 1` skip shape graduates.
    let good = ctx_from_source(
        "module GateGood\n    effects []\n\n\
         fn skipWs(s: String, pos: Int) -> Int\n    match String.charAt(s, pos)\n        Option.None -> pos\n        Option.Some(c) -> match c\n            \" \" -> skipWs(s, pos + 1)\n            _ -> pos\n",
        "gate_good",
    );
    assert!(
        detect_simple_string_pos_skip_literal(fd_named(&good, "skipWs")).is_some(),
        "the canonical uniform pos+1 skipper must still be recognized"
    );

    // BLOCKER 1 (:691): a non-advancing `self(s, pos)` arm alongside a real
    // `self(s, pos + 1)` arm — `decreasing_by` would fail on the non-advancing
    // call, so decline.
    let non_adv = ctx_from_source(
        "module GateNonAdv\n    effects []\n\n\
         fn skipMixed(s: String, pos: Int) -> Int\n    match String.charAt(s, pos)\n        Option.None -> pos\n        Option.Some(c) -> match c\n            \" \" -> skipMixed(s, pos + 1)\n            \"x\" -> skipMixed(s, pos)\n            _ -> pos\n",
        "gate_non_adv",
    );
    assert!(
        detect_simple_string_pos_skip_literal(fd_named(&non_adv, "skipMixed")).is_none(),
        "a non-advancing self-call arm must decline graduation"
    );

    // BLOCKER 1 (:691): the advance routed through a helper `self(s, bump(pos))`
    // — an opaque step `decreasing_by` cannot discharge, so decline.
    let helper = ctx_from_source(
        "module GateHelper\n    effects []\n\n\
         fn bump(pos: Int) -> Int\n    pos + 1\n\n\
         fn skipMixed(s: String, pos: Int) -> Int\n    match String.charAt(s, pos)\n        Option.None -> pos\n        Option.Some(c) -> match c\n            \" \" -> skipMixed(s, pos + 1)\n            \"x\" -> skipMixed(s, bump(pos))\n            _ -> pos\n",
        "gate_helper",
    );
    assert!(
        detect_simple_string_pos_skip_literal(fd_named(&helper, "skipMixed")).is_none(),
        "a helper-routed advance arm must decline graduation"
    );

    // BLOCKER 2 (:621): an outer catch-all `_` on the charAt Option. The native
    // emitter cannot name a `= some c` discriminant off a wildcard, so a
    // graduated def would hard-error — the outer match must be exactly the
    // none/some pair.
    let outer_wild = ctx_from_source(
        "module GateOuterWild\n    effects []\n\n\
         fn skipR(s: String, pos: Int) -> Int\n    match String.charAt(s, pos)\n        Option.None -> pos\n        Option.Some(c) -> match c\n            \" \" -> skipR(s, pos + 1)\n            _ -> pos\n        _ -> pos\n",
        "gate_outer_wild",
    );
    assert!(
        detect_simple_string_pos_skip_literal(fd_named(&outer_wild, "skipR")).is_none(),
        "an outer wildcard on the charAt Option must decline graduation"
    );
}

#[test]
fn proof_mode_accepts_mutual_int_countdown_recursion() {
    let mut ctx = empty_ctx();
    let even = FnDef {
        name: "even".to_string(),
        line: 1,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Bool".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::Ident("n".to_string())),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Int(0)),
                    body: sbb(Expr::Literal(Literal::Bool(true))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "odd".to_string(),
                        vec![sb(Expr::BinOp(
                            BinOp::Sub,
                            sbb(Expr::Ident("n".to_string())),
                            sbb(Expr::Literal(Literal::Int(1))),
                        ))],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    let odd = FnDef {
        name: "odd".to_string(),
        line: 2,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Bool".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::Ident("n".to_string())),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Int(0)),
                    body: sbb(Expr::Literal(Literal::Bool(false))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "even".to_string(),
                        vec![sb(Expr::BinOp(
                            BinOp::Sub,
                            sbb(Expr::Ident("n".to_string())),
                            sbb(Expr::Literal(Literal::Int(1))),
                        ))],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(even.clone()));
    ctx.items.push(TopLevel::FnDef(odd.clone()));
    ctx.fn_defs.push(even);
    ctx.fn_defs.push(odd);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected mutual Int countdown recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("def even__fuel"));
    assert!(lean.contains("def odd__fuel"));
    assert!(lean.contains("def even (n : Int) : Bool :="));
    assert!(lean.contains("even__fuel ((Int.natAbs n) + 1) n"));
}

#[test]
fn proof_mode_accepts_mutual_string_pos_recursion_with_ranked_same_edges() {
    let mut ctx = empty_ctx();
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![
            ("s".to_string(), "String".to_string()),
            ("pos".to_string(), "Int".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::BinOp(
                BinOp::Gte,
                sbb(Expr::Ident("pos".to_string())),
                sbb(Expr::Literal(Literal::Int(3))),
            )),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: sbb(Expr::Ident("pos".to_string())),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "g".to_string(),
                        vec![
                            sb(Expr::Ident("s".to_string())),
                            sb(Expr::Ident("pos".to_string())),
                        ],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    let g = FnDef {
        name: "g".to_string(),
        line: 2,
        params: vec![
            ("s".to_string(), "String".to_string()),
            ("pos".to_string(), "Int".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::BinOp(
                BinOp::Gte,
                sbb(Expr::Ident("pos".to_string())),
                sbb(Expr::Literal(Literal::Int(3))),
            )),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: sbb(Expr::Ident("pos".to_string())),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "f".to_string(),
                        vec![
                            sb(Expr::Ident("s".to_string())),
                            sb(Expr::BinOp(
                                BinOp::Add,
                                sbb(Expr::Ident("pos".to_string())),
                                sbb(Expr::Literal(Literal::Int(1))),
                            )),
                        ],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(f.clone()));
    ctx.items.push(TopLevel::FnDef(g.clone()));
    ctx.fn_defs.push(f);
    ctx.fn_defs.push(g);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected mutual String+pos recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("def f__fuel"));
    assert!(lean.contains("def g__fuel"));
    assert!(!lean.contains("partial def f"));
}

#[test]
fn proof_mode_accepts_mutual_ranked_sizeof_recursion() {
    let mut ctx = empty_ctx();
    let f = FnDef {
        name: "f".to_string(),
        line: 1,
        params: vec![("xs".to_string(), "List<Int>".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::TailCall(Box::new(
            TailCallData::new(
                "g".to_string(),
                vec![
                    sb(Expr::Literal(Literal::Str("acc".to_string()))),
                    sb(Expr::Ident("xs".to_string())),
                ],
            ),
        ))))),
        resolution: None,
    };
    let g = FnDef {
        name: "g".to_string(),
        line: 2,
        params: vec![
            ("acc".to_string(), "String".to_string()),
            ("xs".to_string(), "List<Int>".to_string()),
        ],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::Match {
            subject: sbb(Expr::Ident("xs".to_string())),
            arms: vec![
                MatchArm {
                    pattern: Pattern::EmptyList,
                    body: sbb(Expr::Literal(Literal::Int(0))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Cons("h".to_string(), "t".to_string()),
                    body: sbb(Expr::TailCall(Box::new(TailCallData::new(
                        "f".to_string(),
                        vec![sb(Expr::Ident("t".to_string()))],
                    )))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        }))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(f.clone()));
    ctx.items.push(TopLevel::FnDef(g.clone()));
    ctx.fn_defs.push(f);
    ctx.fn_defs.push(g);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected mutual ranked-sizeOf recursion to be accepted, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    // After the native-decreases path landed for mutual sizeOf
    // SCCs (PR #84), this group emits as a plain `mutual ...
    // end` block with `termination_by` instead of the older
    // `__fuel` helper-and-wrapper pair. The classifier still
    // recognises the recursion (no proof-mode issues raised),
    // which is what this test pins.
    assert!(lean.contains("mutual"));
    assert!(lean.contains("def f"));
    assert!(lean.contains("def g"));
    assert!(lean.contains("termination_by"));
    assert!(!lean.contains("partial def f"));
    assert!(!lean.contains("partial def g"));
}

#[test]
fn proof_mode_rejects_recursive_pure_functions() {
    let mut ctx = empty_ctx();
    let recursive_fn = FnDef {
        name: "loop".to_string(),
        line: 1,
        params: vec![("n".to_string(), "Int".to_string())],
        return_type: "Int".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(sb(Expr::FnCall(
            sbb(Expr::Ident("loop".to_string())),
            vec![sb(Expr::Ident("n".to_string()))],
        )))),
        resolution: None,
    };
    ctx.items.push(TopLevel::FnDef(recursive_fn.clone()));
    ctx.fn_defs.push(recursive_fn);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.iter().any(|i| i.contains("outside proof subset")),
        "expected recursive function blocker, got: {:?}",
        issues
    );
}

#[test]
fn proof_mode_allows_recursive_types() {
    let mut ctx = empty_ctx();
    let recursive_type = TypeDef::Sum {
        name: "Node".to_string(),
        variants: vec![TypeVariant {
            name: "Cons".to_string(),
            fields: vec!["Node".to_string()],
        }],
        line: 1,
    };
    ctx.items.push(TopLevel::TypeDef(recursive_type.clone()));
    ctx.type_defs.push(recursive_type);

    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues
            .iter()
            .all(|i| !i.contains("recursive types require unsafe DecidableEq shim")),
        "did not expect recursive type blocker, got: {:?}",
        issues
    );
}

/// The certificate model's `Inhabited` witness for a sum type seeds the first
/// constructor in declaration order whose arguments can all be defaulted; a
/// constructor carrying the type itself cannot seed, a self-mention beneath a
/// top-level `List`/`Option`/`Map` can (their Lean defaults need no argument
/// instance), and a type where no constructor bottoms out gets no instance.
#[test]
fn inhabited_witness_seeds_first_constructor_that_bottoms_out() {
    let ctx = empty_ctx();
    let sum = |name: &str, variants: Vec<(&str, Vec<&str>)>| TypeDef::Sum {
        name: name.to_string(),
        variants: variants
            .into_iter()
            .map(|(variant_name, fields)| TypeVariant {
                name: variant_name.to_string(),
                fields: fields.into_iter().map(str::to_string).collect(),
            })
            .collect(),
        line: 1,
    };

    // Declaration order: the first defaultable constructor wins, even when a
    // nullary one follows — the constructor `deriving Inhabited` tries first.
    let ordered = sum("Op", vec![("Add", vec!["Int"]), ("Zero", vec![])]);
    assert_eq!(
        super::toplevel::emit_inhabited_instance(&ordered, &ctx, None),
        "instance : Inhabited Op := ⟨Op.add default⟩"
    );

    // A constructor whose argument is the type itself cannot seed the
    // instance; the scan moves on to the base case.
    let recursive = sum(
        "Chain",
        vec![("More", vec!["Chain"]), ("Stop", vec!["Int"])],
    );
    assert_eq!(
        super::toplevel::emit_inhabited_instance(&recursive, &ctx, None),
        "instance : Inhabited Chain := ⟨Chain.stop default⟩"
    );

    // A self-mention beneath a top-level List defaults fine: `[]` needs no
    // `Inhabited` on the element type.
    let wrapped = sum(
        "Forest",
        vec![("Grow", vec!["Forest"]), ("Pack", vec!["List<Forest>"])],
    );
    assert_eq!(
        super::toplevel::emit_inhabited_instance(&wrapped, &ctx, None),
        "instance : Inhabited Forest := ⟨Forest.pack default⟩"
    );

    // No constructor bottoms out (a tuple's default needs both components):
    // no instance, the same conservative shape as the other underivable
    // types.
    let hopeless = sum(
        "Loop",
        vec![("Step", vec!["Loop"]), ("Pair", vec!["Tuple<Loop, Int>"])],
    );
    assert_eq!(
        super::toplevel::emit_inhabited_instance(&hopeless, &ctx, None),
        ""
    );
}

/// A constructor argument naming ANOTHER sum is defaultable exactly when that
/// sum states an `Inhabited` instance of its own (declarations are emitted in
/// dependency order, so the referenced instance always precedes the one being
/// stated); a family of sums reachable only through one another is the same
/// self-ask one level up, and none of them states an instance.
#[test]
fn inhabited_witness_follows_cross_sum_references() {
    let mut ctx = empty_ctx();
    let sum = |name: &str, variants: Vec<(&str, Vec<&str>)>| TypeDef::Sum {
        name: name.to_string(),
        variants: variants
            .into_iter()
            .map(|(variant_name, fields)| TypeVariant {
                name: variant_name.to_string(),
                fields: fields.into_iter().map(str::to_string).collect(),
            })
            .collect(),
        line: 1,
    };
    ctx.type_defs = vec![
        sum(
            "Chain",
            vec![("More", vec!["Chain"]), ("Stop", vec!["Int"])],
        ),
        sum("Carrier", vec![("Hold", vec!["Chain"]), ("Empty", vec![])]),
        sum("Ping", vec![("ToPong", vec!["Pong"])]),
        sum("Pong", vec![("ToPing", vec!["Ping"])]),
    ];

    // `Chain` states its own instance (seeded from `Stop`), so `Hold(Chain)`
    // bottoms out and seeds first.
    assert_eq!(
        super::toplevel::emit_inhabited_instance(&ctx.type_defs[1], &ctx, None),
        "instance : Inhabited Carrier := ⟨Carrier.hold default⟩"
    );

    // The mutual family: neither sum can state an instance, so neither gets
    // one — the conservative no-instance shape, declined at the Lean build
    // only when a model actually demands it.
    assert_eq!(
        super::toplevel::emit_inhabited_instance(&ctx.type_defs[2], &ctx, None),
        ""
    );
    assert_eq!(
        super::toplevel::emit_inhabited_instance(&ctx.type_defs[3], &ctx, None),
        ""
    );
}

/// A constructor whose argument names a refined type cannot seed the witness
/// either: the refined record emits as a `Subtype` abbrev with NO `Inhabited`
/// instance, so `default` on that argument fails to synthesize
/// `Inhabited Natural` and declines the whole certificate. The scan must skip
/// it — as Lean's own `deriving Inhabited` skips uninhabitable constructors —
/// and seed the later nullary constructor instead.
#[test]
fn inhabited_witness_skips_constructor_with_refined_argument() {
    let ctx = ctx_from_source(
        r#"
module RefinedSum
    intent = "t"

record Natural
    value: Int

fn fromInt(n: Int) -> Result<Natural, String>
    match n >= 0
        true  -> Result.Ok(Natural(value = n))
        false -> Result.Err("must be non-negative")

type Payload
  Raw(Natural)
  Empty
"#,
        "refinedsum",
    );
    let payload = ctx
        .type_defs
        .iter()
        .find(|td| matches!(td, TypeDef::Sum { name, .. } if name == "Payload"))
        .expect("Payload sum should be among the entry type defs");
    assert_eq!(
        super::toplevel::emit_inhabited_instance(payload, &ctx, None),
        "instance : Inhabited Payload := ⟨Payload.empty⟩"
    );
}

#[test]
fn law_auto_example_exports_real_proof_artifacts() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/formal/law_auto.av"),
        "law_auto",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("theorem add_law_commutative :"));
    assert!(lean.contains("theorem id'_law_reflexive : ∀ (x : Int), x = x := by"));
    assert!(lean.contains("theorem incCount_law_keyPresent :"));
    assert!(lean.contains("AverMap.has_set_self"));
    assert!(lean.contains("theorem add_law_commutative_sample_1 :"));
    assert!(lean.contains(":= by native_decide"));
}

#[test]
fn json_example_stays_inside_proof_subset() {
    let mut ctx = ctx_from_source(include_str!("../../../examples/data/json.av"), "json");
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected json example to stay inside proof subset, got: {:?}",
        issues
    );
}

#[test]
fn json_example_uses_total_defs_and_domain_guarded_laws_in_proof_mode() {
    let mut ctx = ctx_from_source(include_str!("../../../examples/data/json.av"), "json");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(!lean.contains("partial def"));
    // skipWs is a single simple-skip scanner: it graduates to a native
    // fuel-free `def` with a `termination_by` measure (real `.induct`).
    assert!(lean.contains("def skipWs (s : String) (pos : Int) : Int :="));
    assert!(lean.contains("termination_by (s.toList.length - pos.toNat)"));
    assert!(!lean.contains("def skipWs__fuel"));
    // parseValue sits in a mutual parser clique — mutual groups stay fueled.
    assert!(lean.contains("def parseValue__fuel"));
    assert!(lean.contains("def toString' (j : Json) : String :="));
    assert!(
        lean.contains("def averMeasureJsonEntries_String (items : List (String × Json)) : Nat :=")
    );
    assert!(
        lean.contains(
            "| .jsonObject x0 => (averMeasureJsonEntries_String (AverMap.entries x0)) + 1"
        )
    );
    assert!(
        ctx.declined_claims.borrow().is_empty(),
        "the JSON proof's bounded parser fuel must not be classified as unbounded: {:?}",
        ctx.declined_claims.borrow()
    );
    assert!(lean.contains("-- when jsonRoundtripSafe j"));
    assert!(!lean.contains("-- hint: verify law '"));
    assert!(!lean.contains("private theorem toString'_law_parseRoundtrip_aux"));
    assert!(
        lean.contains("theorem toString'_law_parseRoundtrip : ∀ (j : Json), j = Json.jsonNull ∨")
    );
    assert!(
        lean.contains("jsonRoundtripSafe j = true -> fromString (toString' j) = Except.ok j := by")
    );
    assert!(lean.contains("theorem finishFloat_law_fromCanonicalFloat : ∀ (f : Float), f = 3.5 ∨"));
    assert!(lean.contains("theorem finishInt_law_fromCanonicalInt_checked_domain :"));
    assert!(
        lean.contains(
            "theorem toString'_law_parseValueRoundtrip : ∀ (j : Json), j = Json.jsonNull ∨"
        )
    );
    assert!(lean.contains("theorem toString'_law_parseRoundtrip_sample_1 :"));
    assert!(
        lean.contains(
            "example : fromString \"null\" = Except.ok Json.jsonNull := by native_decide"
        )
    );
}

#[test]
fn transpile_injects_builtin_network_types_and_vector_get_support() {
    let mut ctx = ctx_from_source(
        r#"
fn firstOrMissing(xs: Vector<String>) -> Result<String, String>
    Result.fromOption(Vector.get(xs, 0), "missing")

fn defaultHeaders() -> Map<String, List<String>>
    {"content-type" => ["application/json"]}

fn mkResponse(body: String) -> Http.Response
    Http.Response(status = 200, body = body, headers = defaultHeaders())

fn requestPath(req: HttpRequest) -> String
    req.path

fn echoConn(conn: Tcp.Connection) -> Tcp.Connection
    conn
"#,
        "network_helpers",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("structure Http.Response where"));
    assert!(lean.contains("structure HttpRequest where"));
    // `Tcp.Connection` is a provider-owned resource. Lean models only its
    // identity inside the capability namespace; the old represented carrier
    // (and its host/port fields) must not leak back into the proof model.
    assert!(
        lean.contains(
            "structure Tcp.Connection where\n  id : Nat\n  deriving Repr, BEq, DecidableEq"
        ),
        "{lean}"
    );
    assert!(!lean.contains("Tcp_Connection"));
    assert!(!lean.contains("port : Int"));
    // Headers field renders as the Map shape (Lean uses List of pairs).
    assert!(lean.contains("List (String × List String)"));
}

#[test]
fn law_auto_example_has_no_sorry_in_proof_mode() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/formal/law_auto.av"),
        "law_auto",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(
        !lean.contains("sorry"),
        "expected law_auto proof export to avoid sorry, got:\n{}",
        lean
    );
}

#[test]
fn map_example_has_no_sorry_in_proof_mode() {
    let mut ctx = ctx_from_source(include_str!("../../../examples/data/map.av"), "map");
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected map example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    // After codegen change: universal theorems that can't be auto-proved get sorry
    assert!(lean.contains("theorem incCount_law_trackedCountStepsByOne :"));
    assert!(lean.contains("sorry"));
    // Universal theorems that can't be auto-proved now get sorry instead of being omitted
    assert!(lean.contains("theorem countWords_law_presenceMatchesContains_sample_1 :"));
    assert!(lean.contains("theorem countWords_law_trackedWordCount_sample_1 :"));
    assert!(lean.contains("AverMap.has_set_self"));
    assert!(lean.contains("AverMap.get_set_self"));
}

#[test]
fn spec_laws_example_has_no_sorry_in_proof_mode() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/formal/spec_laws.av"),
        "spec_laws",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected spec_laws example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(
        !lean.contains("sorry"),
        "expected spec_laws proof export to avoid sorry, got:\n{}",
        lean
    );
    assert!(lean.contains("theorem absVal_eq_absValSpec :"));
    assert!(lean.contains("theorem clampNonNegative_eq_clampNonNegativeSpec :"));
}

#[test]
fn rle_example_exports_sampled_roundtrip_laws_without_sorry() {
    let mut ctx = ctx_from_source(include_str!("../../../examples/data/rle.av"), "rle");
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(
        lean.contains("sorry"),
        "expected rle proof export to contain sorry for unproved universal theorems"
    );
    assert!(lean.contains(
        "theorem encode_law_roundtrip_sample_1 : decode (encode []) = [] := by native_decide"
    ));
    assert!(lean.contains(
            "theorem encodeString_law_string_roundtrip_sample_1 : decodeString (encodeString \"\") = \"\" := by native_decide"
        ));
}

#[test]
fn fibonacci_example_uses_native_guarded_int_countdown_in_proof_mode() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/data/fibonacci.av"),
        "fibonacci",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    // Native guarded emission: an aux fn with the precondition
    // derived from `fib`'s `match (n < 0) { false -> fibTR(n,...) }`
    // arm (flipped to positive form `n >= 0` at extract time) +
    // termination on `n.natAbs`, plus a thin wrapper preserving the
    // source signature. Replaces the historical `def fibTR__fuel`
    // path so Lean can `simp`/`decide` through recursive calls
    // instead of treating the helper as opaque.
    assert!(
        lean.contains("def fibTR__aux (n : Int) (a : Int) (b : Int) (h_dom : ((n >= 0))) : Int :="),
        "expected fibTR aux with caller-derived precondition, got:\n{}",
        lean
    );
    assert!(
        lean.contains("if h_zero : n = 0 then a"),
        "expected dependent-if on literal 0, got:\n{}",
        lean
    );
    assert!(
        lean.contains("else fibTR__aux (n - 1) b (a + b) (by omega)"),
        "expected recursive call carrying (by omega), got:\n{}",
        lean
    );
    assert!(lean.contains("termination_by Int.natAbs n"));
    assert!(lean.contains("def fibTR (n : Int) (a : Int) (b : Int) : Int :="));
    assert!(lean.contains("if h_dom : ((n >= 0)) then fibTR__aux n a b h_dom"));
    assert!(!lean.contains("def fibTR__fuel"));
    assert!(!lean.contains("partial def fibTR"));
}

#[test]
fn fibonacci_example_stays_inside_proof_subset() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/data/fibonacci.av"),
        "fibonacci",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected fibonacci example to stay inside proof subset, got: {:?}",
        issues
    );
}

#[test]
fn fibonacci_example_matches_general_linear_recurrence_shapes() {
    let ctx = ctx_from_source(
        include_str!("../../../examples/data/fibonacci.av"),
        "fibonacci",
    );
    let fib = ctx.fn_defs.iter().find(|fd| fd.name == "fib").unwrap();
    let fib_tr = ctx.fn_defs.iter().find(|fd| fd.name == "fibTR").unwrap();
    let fib_spec = ctx.fn_defs.iter().find(|fd| fd.name == "fibSpec").unwrap();

    assert!(recurrence::detect_tailrec_int_linear_pair_wrapper(fib).is_some());
    assert!(recurrence::detect_tailrec_int_linear_pair_worker(fib_tr).is_some());
    assert!(recurrence::detect_second_order_int_linear_recurrence(fib_spec).is_some());
}

#[test]
fn fibonacci_example_auto_proves_general_linear_recurrence_spec_law() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/data/fibonacci.av"),
        "fibonacci",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    assert!(lean.contains("private def fibSpec__nat : Nat -> Int"));
    assert!(!lean.contains("partial def fibSpec"));
    assert!(lean.contains("private theorem fib_eq_fibSpec__worker_nat_shift"));
    assert!(lean.contains("private theorem fib_eq_fibSpec__helper_nat"));
    assert!(lean.contains("private theorem fib_eq_fibSpec__helper_seed"));
    assert!(lean.contains("theorem fib_eq_fibSpec : ∀ (n : Int), fib n = fibSpec n := by"));
    assert!(!lean.contains(
        "-- universal theorem fib_eq_fibSpec omitted: sampled law shape is not auto-proved yet"
    ));
}

#[test]
fn pell_like_example_auto_proves_same_general_shape() {
    let mut ctx = ctx_from_source(
        r#"
module Pell
    intent =
        "linear recurrence probe"

fn pellTR(n: Int, a: Int, b: Int) -> Int
    match n
        0 -> a
        _ -> pellTR(n - 1, b, a + 2 * b)

fn pell(n: Int) -> Int
    match n < 0
        true -> 0
        false -> pellTR(n, 0, 1)

fn pellSpec(n: Int) -> Int
    match n < 0
        true -> 0
        false -> match n
            0 -> 0
            1 -> 1
            _ -> pellSpec(n - 2) + 2 * pellSpec(n - 1)

verify pell law pellSpec
    given n: Int = [0, 1, 2, 3]
    pell(n) => pellSpec(n)
"#,
        "pell",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected pell example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("private def pellSpec__nat : Nat -> Int"));
    assert!(lean.contains("private theorem pell_eq_pellSpec__worker_nat_shift"));
    assert!(lean.contains("theorem pell_eq_pellSpec : ∀ (n : Int), pell n = pellSpec n := by"));
    assert!(!lean.contains(
        "-- universal theorem pell_eq_pellSpec omitted: sampled law shape is not auto-proved yet"
    ));
}

#[test]
fn nonlinear_pair_state_recurrence_is_not_auto_proved_as_linear_shape() {
    let mut ctx = ctx_from_source(
        r#"
module WeirdRec
    intent =
        "reject nonlinear pair-state recurrence from linear recurrence prover"

fn weirdTR(n: Int, a: Int, b: Int) -> Int
    match n
        0 -> a
        _ -> weirdTR(n - 1, b, a * b)

fn weird(n: Int) -> Int
    match n < 0
        true -> 0
        false -> weirdTR(n, 0, 1)

fn weirdSpec(n: Int) -> Int
    match n < 0
        true -> 0
        false -> match n
            0 -> 0
            1 -> 1
            _ -> weirdSpec(n - 1) * weirdSpec(n - 2)

verify weird law weirdSpec
    given n: Int = [0, 1, 2, 3]
    weird(n) => weirdSpec(n)
"#,
        "weirdrec",
    );
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);

    // After codegen change: emit sorry instead of omitting universal theorems
    assert!(lean.contains("sorry"));
    assert!(!lean.contains("private theorem weird_eq_weirdSpec__worker_nat_shift"));
    assert!(lean.contains("theorem weird_eq_weirdSpec_sample_1 :"));
}

#[test]
fn date_example_stays_inside_proof_subset() {
    let mut ctx = ctx_from_source(include_str!("../../../examples/data/date.av"), "date");
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected date example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(!lean.contains("partial def"));
    assert!(lean.contains("def parseIntSlice (s : String) (from' : Int) (to : Int) : Int :="));
}

#[test]
fn temperature_example_stays_inside_proof_subset() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/core/temperature.av"),
        "temperature",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected temperature example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(!lean.contains("partial def"));
    assert!(
        lean.contains("example : celsiusToFahr 0.0 = 32.0 := by native_decide"),
        "expected verify examples to survive proof export, got:\n{}",
        lean
    );
}

#[test]
fn quicksort_example_stays_inside_proof_subset() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/data/quicksort.av"),
        "quicksort",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected quicksort example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("def isOrderedFrom"));
    assert!(!lean.contains("partial def isOrderedFrom"));
    assert!(lean.contains("termination_by xs.length"));
}

#[test]
fn grok_s_language_example_uses_total_ranked_sizeof_mutual_recursion() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/core/grok_s_language.av"),
        "grok_s_language",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected grok_s_language example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("mutual"));
    assert!(lean.contains("def parseListItems__fuel"));
    assert!(!lean.contains("partial def eval"));
    // The `eval` SCC stays FUEL: it has MULTI-`Sexpr`-carrier members
    // (`evalOpWithPair(_, leftExpr: Sexpr, rightExpr: Sexpr, _)`). Multi-carrier
    // ADT sum measures are native-closable for some shapes but not all (a
    // red-black-tree SCC's `decreasing_by` fails), so multi-carrier ADT stays
    // on fuel — only SINGLE-carrier structural ADT goes native.
    assert!(lean.contains("def eval__fuel"));
    assert!(lean.contains("def evalOpWithPair__fuel"));
    // The SINGLE-carrier structural `Sexpr` fns (`toString'`, `validSymbolNames`)
    // DO emit native `termination_by (sizeOf e, _)` now (no fuel) — structural
    // recursion on the ADT, Lean's own termination check accepts it.
    assert!(lean.contains("termination_by (sizeOf e,"));
    assert!(!lean.contains("def toString'__fuel"));
    assert!(lean.contains("-- when validSymbolNames e"));
    assert!(!lean.contains("private theorem toString'_law_parseRoundtrip_aux"));
    assert!(
        lean.contains(
            "theorem toString'_law_parseRoundtrip : ∀ (e : Sexpr), e = Sexpr.atomNum 42 ∨"
        )
    );
    assert!(lean.contains("validSymbolNames e = true -> parse (toString' e) = Except.ok e := by"));
    assert!(lean.contains("theorem toString'_law_parseSexprRoundtrip :"));
    assert!(lean.contains("theorem toString'_law_parseRoundtrip_sample_1 :"));
}

#[test]
fn lambda_example_keeps_only_eval_outside_proof_subset() {
    let mut ctx = ctx_from_source(include_str!("../../../examples/core/lambda.av"), "lambda");
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert_eq!(
            issues,
            vec!["recursive function 'eval' is outside proof subset (currently supported: Int countdown, guard-validated Int floor-division countdown by a literal divisor, second-order affine Int recurrences with pair-state worker, structural recursion on List/recursive ADTs, String+position, mutual Int countdown, mutual String+position, and ranked sizeOf recursion)".to_string()]
        );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    // termToString / subst / countS recurse structurally on the `Term`
    // ADT, so they emit as plain structural defs (Lean infers termination,
    // definitional unfolds) — the fuel retirement for SizeOfStructural. No
    // `__fuel` helper. `eval` stays outside the proof subset (`partial def`);
    // step/stepApp remain a mutual-fuel SCC (computed-arg recursion).
    assert!(lean.contains("def termToString (t : Term)"));
    assert!(lean.contains("def subst (x : String) (s : Term) (t : Term)"));
    assert!(lean.contains("def countS (t : Term)"));
    assert!(!lean.contains("def termToString__fuel"));
    assert!(!lean.contains("def subst__fuel"));
    assert!(!lean.contains("def countS__fuel"));
    assert!(!lean.contains("partial def termToString"));
    assert!(!lean.contains("partial def subst"));
    assert!(!lean.contains("partial def countS"));
    assert!(lean.contains("partial def eval"));
}

#[test]
fn mission_control_example_stays_inside_proof_subset() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/apps/mission_control.av"),
        "mission_control",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected mission_control example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(!lean.contains("partial def normalizeAngle"));
    assert!(lean.contains("def normalizeAngle__fuel"));
}

#[test]
fn notepad_store_example_stays_inside_proof_subset() {
    let mut ctx = ctx_from_source(
        include_str!("../../../examples/apps/notepad/store.av"),
        "notepad_store",
    );
    ctx.refresh_facts();
    let issues = proof_mode_issues(&ctx);
    assert!(
        issues.is_empty(),
        "expected notepad/store example to stay inside proof subset, got: {:?}",
        issues
    );

    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("def deserializeLine (line : String) : Except String Note :="));
    assert!(lean.contains("Except String (List Note)"));
    assert!(!lean.contains("partial def deserializeLine"));
    assert!(lean.contains("-- when noteRoundtripSafe note"));
    assert!(lean.contains("-- when notesRoundtripSafe notes"));
    assert!(lean.contains(
            "theorem serializeLine_law_lineRoundtrip : ∀ (note : Note), note = { id' := 1, title := \"Hello\", body := \"World\" : Note } ∨"
        ));
    assert!(lean.contains(
        "theorem serializeLines_law_notesRoundtrip : ∀ (notes : List Note), notes = [] ∨"
    ));
    assert!(lean.contains("notesRoundtripSafe notes = true ->"));
    assert!(lean.contains("parseNotes (s!\"{String.intercalate \"\\n\" (serializeLines notes)}\\n\") = Except.ok notes"));
    assert!(lean.contains("theorem serializeLine_law_lineRoundtrip_sample_1 :"));
    assert!(lean.contains("theorem serializeLines_law_notesRoundtrip_sample_1 :"));
}

/// Shape-gated `grind` rung, ADMIT side: a flat nonlinear arithmetic
/// law (`x * x >= 0`) whose cone is a single NON-recursive pure fn and
/// whose default ladder ends in an honest `sorry` (no `omega`/`rfl`
/// guaranteed closer for `var * var`) must lead with the cone-aware
/// `grind` rung — `grind`'s ring/order subsolvers close such flat goals
/// without induction. Codegen-only (no lake): asserts the emitted tactic.
#[test]
fn grind_rung_admitted_on_flat_nonrecursive_law() {
    // A flat, sorry-floored, non-recursive POLYNOMIAL IDENTITY (a Yields
    // equality `grind`'s ring subsolver closes but no pinned strategy
    // claims) must gain the cone-aware grind rung. The nonnegativity /
    // order shapes that used to exercise this now route to the dedicated
    // `NonlinearNonneg` closer (`aver_int_order`), so this uses a ring
    // identity — `x² + 2x + 1 = (x + 1)²` — instead.
    let src = "\
module FlatGrind
    effects []

fn lhsPoly(x: Int) -> Int
    ? \"x squared plus twice x plus one.\"
    x * x + 2 * x + 1

fn rhsPoly(x: Int) -> Int
    ? \"x plus one, squared.\"
    (x + 1) * (x + 1)

verify lhsPoly law squareExpansion
    given x: Int = [-7, -1, 0, 1, 9]
    lhsPoly(x) => rhsPoly(x)
";
    let mut ctx = ctx_from_source(src, "FlatGrind");
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);
    assert!(
        lean.contains("grind ["),
        "a flat, sorry-floored, non-recursive law must gain the cone-aware \
         grind rung:\n{lean}"
    );
}

/// Shape-gated `grind` rung, SKIP side (the "not worse" invariant): an
/// INDUCTIVE law whose cone contains a user-recursive pure fn
/// (`len`/`append`, a list-length monoid identity) routes to the
/// structural-induction path `grind` cannot close. The gate MUST NOT
/// emit `grind` there — emitting it would only burn ~1.2s per proof for
/// a guaranteed fall-through. Codegen-only: asserts NO `grind` anywhere
/// in the inductive law's module.
#[test]
fn grind_rung_skipped_on_inductive_recursive_law() {
    let src = "\
module IndNoGrind
    effects []

fn len(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [x, ..rest] -> 1 + len(rest)

fn append(xs: List<Int>, ys: List<Int>) -> List<Int>
    match xs
        [] -> ys
        [x, ..rest] -> List.concat([x], append(rest, ys))

verify len law lenAppend
    given xs: List<Int> = [[], [1], [1, 2, 3]]
    given ys: List<Int> = [[], [4], [5, 6]]
    len(append(xs, ys)) => len(xs) + len(ys)
";
    let mut ctx = ctx_from_source(src, "IndNoGrind");
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);
    assert!(
        !lean.contains("grind"),
        "an inductive law over user-recursive fns (len/append) must NOT \
         gain a grind rung — grind cannot close a structural-induction \
         goal, so emitting it is pure waste:\n{lean}"
    );
}

/// Locate a `verify <fn> law <name>` block in a built context.
fn find_clique_law<'a>(
    ctx: &'a CodegenContext,
    fn_name: &str,
    law_name: &str,
) -> (&'a VerifyBlock, &'a VerifyLaw) {
    for item in &ctx.items {
        if let TopLevel::Verify(vb) = item
            && vb.fn_name == fn_name
            && let VerifyKind::Law(law) = &vb.kind
            && law.name == law_name
        {
            return (vb, law);
        }
    }
    panic!("law {fn_name}.{law_name} not found");
}

#[test]
fn clique_cursor_monotonicity_recognizes_self_contained_but_declines_on_missing_pool_law() {
    // A self-contained two-member string-position clique (no external cursor
    // edges) is recognized: its advance law will be proven universally by the
    // rung's own conjunction.
    let self_contained = r#"
type R
    Ok(Int, Int)
    Err(String)

fn a(s: String, pos: Int) -> R
    ? "a"
    match String.charAt(s, pos)
        Option.None -> R.Err("e")
        Option.Some(c) -> b(s, pos + 1)

fn b(s: String, pos: Int) -> R
    ? "b"
    match String.charAt(s, pos)
        Option.None -> R.Ok(0, pos)
        Option.Some(c) -> a(s, pos + 1)

verify a law aAdvances
    given s: String = ["x"]
    given pos: Int = [0]
    given v: Int = [0]
    given p: Int = [0]
    when a(s, pos) == R.Ok(v, p)
    p >= pos => true
"#;
    let ctx = ctx_from_source(self_contained, "SelfContained");
    let (vb, law) = find_clique_law(&ctx, "a", "aAdvances");
    assert!(
        super::law_auto::recognize_clique_position_monotonicity(vb, law, &ctx),
        "a self-contained clique must be recognized"
    );

    // The SAME clique, but `a` now scrutinizes a NON-clique sub-parser `ext`
    // whose returned position feeds the semantic edge. `ext` has NO universal
    // advance law in the pool, so the rung DECLINES (fail-closed) — the law
    // keeps its bounded proof rather than a silently weaker universal. This is
    // the decline-on-missing-pool-law path.
    let missing_pool_law = r#"
type R
    Ok(Int, Int)
    Err(String)

fn ext(s: String, pos: Int) -> R
    ? "ext"
    R.Ok(0, pos)

fn a(s: String, pos: Int) -> R
    ? "a"
    match ext(s, pos)
        R.Err(m) -> R.Err(m)
        R.Ok(v, p) -> b(s, p)

fn b(s: String, pos: Int) -> R
    ? "b"
    match String.charAt(s, pos)
        Option.None -> R.Err("e")
        Option.Some(c) -> a(s, pos + 1)

verify a law aAdvances
    given s: String = ["x"]
    given pos: Int = [0]
    given v: Int = [0]
    given p: Int = [0]
    when a(s, pos) == R.Ok(v, p)
    p >= pos => true
"#;
    let ctx2 = ctx_from_source(missing_pool_law, "MissingPoolLaw");
    let (vb2, law2) = find_clique_law(&ctx2, "a", "aAdvances");
    assert!(
        !super::law_auto::recognize_clique_position_monotonicity(vb2, law2, &ctx2),
        "a clique reaching a sub-parser with no universal advance law must decline"
    );
}

#[test]
fn generative_call_nested_in_args_gets_the_higher_oracle_index() {
    // Regression — the lifted proof must charge oracle call indices in the
    // order the VM charges them.
    //
    // The VM evaluates a call's arguments eagerly and only then takes its
    // oracle coordinates (`dispatch_oracle_stub` receives already-evaluated
    // args and calls `take_oracle_coordinates()` afterwards), so a generative
    // call nested inside the arguments consumes the LOWER index. Pre-fix the
    // lifter did the opposite: it claimed its own counter before lifting its
    // arguments, so `Random.int(0, Random.int(0, 6) + 2)` assigned the
    // outer and inner oracle indices in the opposite order from the VM.
    //
    // The inversion was invisible at both ends and fail-closed in both
    // directions: the law that held at runtime exported a theorem
    // `native_decide` refutes, and the law matching the export failed
    // `verify`. A correct program was simply unprovable. See the VM half of
    // this invariant in tests/regression_oracle_counter_order.rs.
    let mut ctx = ctx_from_source(
        r#"module CounterOrder
    intent = "Oracle indices must be charged in evaluation order."
    effects [Random]

fn indexStub(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>
    ? "Returns the call index so the oracle coordinate is observable."
    Result.Ok(n)

fn nested() -> Result<Int, String>
    ? "Outer Random.int whose upper bound is itself a Random.int."
    ! [Random.int]
    Random.int(0, Random.int(0, 6) + 2)

verify nested law indexOrder
    given rnd: Random.int = [indexStub]
    nested() => Result.Ok(1)
"#,
        "counter_order",
    );
    let out = transpile(&mut ctx);
    let lean = generated_lean_file(&out);

    assert!(
        lean.contains("rnd_Random_int path 0 0 6") && lean.contains("rnd_Random_int path 1 0"),
        "the nested generative call must take index 0 and the surrounding one index 1, \
         matching the VM's eager argument evaluation even through Result propagation; \
         emitted body was:\n{lean}"
    );
}

/// The termination measure of a three-member cycle counts, for each member,
/// every list that travels around the cycle — and orders the one call that
/// hands both tails on unchanged.
#[test]
fn three_member_cycle_is_measured_by_what_travels_around_it() {
    let mut ctx = ctx_from_source(
        include_str!("../../../tests/fixtures/mutual_cycle_three_lists.av"),
        "mutual_cycle_three_lists",
    );
    ctx.refresh_facts();
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(lean.contains("termination_by (sizeOf left + sizeOf right, 2)"));
    assert!(lean.contains("termination_by (sizeOf tail + sizeOf right, 1)"));
    assert!(lean.contains("termination_by (sizeOf tail + sizeOf rest, 3)"));
    assert!(
        !lean.contains("__fuel"),
        "the cycle must not fall back to fuel:\n{lean}"
    );
}

/// The pair the forwarded-parameter measure already handled emits the same
/// measure under the call edge analysis: `tail` and `items`, ranked 2 and 1,
/// nothing added (not the returned `head`, not the `Int` position).
#[test]
fn forwarded_pair_measure_is_unchanged_by_the_edge_analysis() {
    let mut ctx = ctx_from_source(
        include_str!("../../../tests/fixtures/mutual_measure_forwarded.av"),
        "mutual_measure_forwarded",
    );
    ctx.refresh_facts();
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    let measures: Vec<&str> = lean
        .lines()
        .filter(|line| line.trim_start().starts_with("termination_by"))
        .collect();
    assert_eq!(
        measures,
        vec![
            "  termination_by (sizeOf tail, 2)",
            "  termination_by (sizeOf items, 1)"
        ],
        "{lean}"
    );
    assert!(lean.contains(
        "  decreasing_by all_goals (first | decreasing_tactic | (simp_wf; (try simp only [AverMap.entries, AverMap.fromList]); (try simp_all); first | omega | (constructor <;> first | rfl | omega)))"
    ));
}

/// A method application that is itself the receiver of a method application
/// is parenthesised; a receiver that is one bracketed group, or one followed
/// by projections, stays as it was.
#[test]
fn chained_method_receivers_are_parenthesised() {
    let mut ctx = ctx_from_source(
        include_str!("../../../tests/fixtures/lean_chained_receivers.av"),
        "lean_chained_receivers",
    );
    ctx.refresh_facts();
    let out = transpile_for_proof_mode(&mut ctx, VerifyEmitMode::NativeDecide);
    let lean = generated_lean_file(&out);
    assert!(
        lean.contains("((doubled xs).drop (Int.toNat 1)).take (Int.toNat 32)"),
        "{lean}"
    );
    assert!(
        lean.contains("((xs ++ ys).drop (Int.toNat 1)).take (Int.toNat 2)"),
        "{lean}"
    );
    assert!(
        lean.contains("((doubled xs).reverse.drop (Int.toNat 1)).take (Int.toNat 2)"),
        "{lean}"
    );
}
