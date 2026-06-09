//! Stage 6a of #232 — pin `ModulePattern::RefinementSmartConstructor`
//! detection on the canonical `examples/refinement/*` corpus. Every
//! shipped refinement module must produce exactly one pattern, and
//! its typed payload must agree with what
//! `codegen::common::refinement_info_for` already returns (so the
//! stage 6b adapter migration is behavior-preserving).

use aver::analysis::shape::{ModulePattern, collect_inductable_sum_types, detect_module_patterns};

fn detect_in_file(path: &str) -> Vec<ModulePattern> {
    let source = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("read {path}: {e}"));
    let items = aver::source::parse_source(&source).unwrap_or_else(|e| panic!("parse {path}: {e}"));
    let module_root = std::path::Path::new(path)
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or(".");
    let deps = aver::source::load_compile_deps(&items, module_root)
        .unwrap_or_else(|e| panic!("deps: {e}"));
    detect_module_patterns(&items, &deps)
}

#[test]
fn natural_module_emits_one_refinement_pattern() {
    let patterns = detect_in_file("examples/refinement/natural/natural.av");
    let refinements: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::RefinementSmartConstructor { .. }))
        .collect();
    assert_eq!(
        refinements.len(),
        1,
        "expected exactly one RefinementSmartConstructor on Natural; got {patterns:?}"
    );
    let ModulePattern::RefinementSmartConstructor {
        type_name,
        carrier_field,
        carrier_type,
        constructor_fn,
        ..
    } = refinements[0]
    else {
        unreachable!()
    };
    assert_eq!(type_name, "Natural");
    assert_eq!(carrier_field, "value");
    assert_eq!(carrier_type, "Int");
    assert_eq!(constructor_fn, "fromInt");
}

#[test]
fn positive_module_emits_one_refinement_pattern() {
    let patterns = detect_in_file("examples/refinement/positive/positive.av");
    let refinements: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::RefinementSmartConstructor { .. }))
        .collect();
    assert_eq!(refinements.len(), 1);
    let ModulePattern::RefinementSmartConstructor { type_name, .. } = refinements[0] else {
        unreachable!()
    };
    assert_eq!(type_name, "Positive");
}

#[test]
fn int_range_module_emits_one_refinement_pattern() {
    let patterns = detect_in_file("examples/refinement/int_range/int_range.av");
    let refinements: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::RefinementSmartConstructor { .. }))
        .collect();
    assert_eq!(refinements.len(), 1);
    let ModulePattern::RefinementSmartConstructor { type_name, .. } = refinements[0] else {
        unreachable!()
    };
    assert_eq!(type_name, "IntRange");
}

#[test]
fn nonneg_float_module_emits_one_refinement_pattern() {
    let patterns = detect_in_file("examples/refinement/nonneg_float/nonneg_float.av");
    let refinements: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::RefinementSmartConstructor { .. }))
        .collect();
    assert_eq!(refinements.len(), 1);
    let ModulePattern::RefinementSmartConstructor {
        type_name,
        carrier_type,
        ..
    } = refinements[0]
    else {
        unreachable!()
    };
    assert_eq!(type_name, "NonNegFloat");
    assert_eq!(carrier_type, "Float");
}

#[test]
fn module_with_no_refinement_emits_no_refinement_pattern() {
    // Plain orchestration module — no opaque + smart constructor pair.
    // It may still emit other module patterns (e.g. RendererFormatter
    // for `showListInt`); the contract here is only that
    // `RefinementSmartConstructor` does not fire.
    let patterns = detect_in_file("examples/data/quicksort.av");
    let refinements: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::RefinementSmartConstructor { .. }))
        .collect();
    assert!(
        refinements.is_empty(),
        "quicksort has no refinement record; got {refinements:?}"
    );
}

#[test]
fn detection_payload_matches_refinement_info_for() {
    // The point of stage 6a: the new typed pattern carries the same
    // payload `codegen::common::refinement_info_for` already returns.
    // Stage 6b will retire the legacy fn — this test guards that
    // they agree before the migration lands.
    let path = "examples/refinement/natural/natural.av";
    let patterns = detect_in_file(path);
    let pattern = patterns
        .iter()
        .find(|p| matches!(p, ModulePattern::RefinementSmartConstructor { .. }))
        .expect("Natural module must produce a RefinementSmartConstructor");
    let ModulePattern::RefinementSmartConstructor {
        type_name,
        carrier_field,
        carrier_type,
        param_name,
        ..
    } = pattern
    else {
        unreachable!()
    };

    // The legacy adapter takes `ProofLowerInputs`; mirror what
    // codegen does to build one for a single file.
    let source = std::fs::read_to_string(path).unwrap();
    let items = aver::source::parse_source(&source).unwrap();
    let module_root = std::path::Path::new(path).parent().unwrap();
    let deps = aver::source::load_compile_deps(&items, module_root.to_str().unwrap()).unwrap();
    let module_prefixes: std::collections::HashSet<String> =
        deps.iter().map(|m| m.prefix.clone()).collect();
    let recursive_fns: std::collections::HashSet<aver::ir::FnId> = std::collections::HashSet::new();
    let symbol_table = aver::ir::SymbolTable::default();
    let inputs = aver::codegen::proof_lower::ProofLowerInputs {
        entry_items: &items,
        dep_modules: &deps,
        module_prefixes: &module_prefixes,
        recursive_fns: &recursive_fns,
        symbol_table: &symbol_table,
        program_shape: None,
    };
    let legacy = aver::codegen::common::refinement_info_for(type_name, &inputs)
        .expect("legacy refinement_info_for must match the new pattern detector");
    assert_eq!(legacy.carrier_field, carrier_field);
    assert_eq!(legacy.carrier_type, carrier_type);
    assert_eq!(legacy.param_name, param_name);
    // `predicate` is `Spanned<Expr>` — comparing the rendered
    // structure is enough to confirm agreement without bringing in
    // a full AST equality impl.
    assert_eq!(format!("{:?}", legacy.predicate), {
        let ModulePattern::RefinementSmartConstructor { predicate, .. } = pattern else {
            unreachable!()
        };
        format!("{:?}", predicate)
    });
}

// ─── Stage 6c: WrapperOverRecursion ─────────────────────────────────────────

#[test]
fn fibonacci_module_pins_fib_over_fibtr_wrapper() {
    let patterns = detect_in_file("examples/data/fibonacci.av");
    let wrappers: Vec<_> = patterns
        .iter()
        .filter_map(|p| match p {
            ModulePattern::WrapperOverRecursion {
                wrapper_fn,
                inner_fn,
                wrapper_scope,
                inner_scope,
            } => Some((
                wrapper_scope.clone(),
                wrapper_fn.clone(),
                inner_scope.clone(),
                inner_fn.clone(),
            )),
            _ => None,
        })
        .collect();
    // `fib(n) -> fibTR(n, 0, 1)` is the canonical match. The
    // accumulator-record wrapper `buildFibStats -> buildFibStatsTR`
    // doesn't qualify under stage 6c's literal-Ident rule (its inner
    // args are `n + 1` and a record literal, not bare params).
    assert!(
        wrappers.contains(&(None, "fib".to_string(), None, "fibTR".to_string())),
        "expected fib→fibTR wrapper; got {wrappers:?}"
    );
}

// ─── AccumulatorFold (role-bearing refinement of WrapperOverRecursion) ───────

/// `(wrapper, loop, step_fn, step_op, finish_fn)` for each AccumulatorFold.
fn accumulator_folds(
    path: &str,
) -> Vec<(
    String,
    String,
    Option<String>,
    Option<String>,
    Option<String>,
)> {
    detect_in_file(path)
        .iter()
        .filter_map(|p| match p {
            ModulePattern::AccumulatorFold {
                wrapper_fn,
                loop_fn,
                step_fn,
                step_op,
                finish_fn,
                ..
            } => Some((
                wrapper_fn.clone(),
                loop_fn.clone(),
                step_fn.clone(),
                step_op.map(|o| format!("{o:?}")),
                finish_fn.clone(),
            )),
            _ => None,
        })
        .collect()
}

#[test]
fn codec_fold_pins_named_step_and_finish() {
    // rle's `encode → encodeLoop` is an AccumulatorFold with a NAMED step
    // (`encodeFold`) and a NAMED finish (`flushAcc`) — the codec flavor.
    let folds = accumulator_folds("examples/data/rle.av");
    assert!(
        folds.contains(&(
            "encode".to_string(),
            "encodeLoop".to_string(),
            Some("encodeFold".to_string()),
            None,
            Some("flushAcc".to_string()),
        )),
        "expected encode→encodeLoop codec fold; got {folds:?}"
    );
}

#[test]
fn monoidal_fold_pins_inline_op_and_identity_finish() {
    // sum_acc's `sum → sumTR` is an AccumulatorFold with an INLINE additive
    // step (`acc + h` → step_op = Add) and an IDENTITY finish (nil arm returns
    // `acc`, so finish_fn = None) — the monoidal flavor of the same schema.
    let folds = accumulator_folds("examples/data/sum_acc.av");
    assert!(
        folds.contains(&(
            "sum".to_string(),
            "sumTR".to_string(),
            None,
            Some("Add".to_string()),
            None,
        )),
        "expected sum→sumTR monoidal fold; got {folds:?}"
    );
}

#[test]
fn non_list_wrapper_is_not_an_accumulator_fold() {
    // `fib → fibTR` is a WrapperOverRecursion but NOT an AccumulatorFold — its
    // inner recurs on an `Int`, not a `match list` fold. The two are distinct.
    let folds = accumulator_folds("examples/data/fibonacci.av");
    assert!(
        !folds.iter().any(|(w, _, _, _, _)| w == "fib"),
        "fib must not be an AccumulatorFold; got {folds:?}"
    );
}

// ─── Stage 6d: ResultPipelineChain ──────────────────────────────────────────

#[test]
fn result_pipeline_module_pins_validate_and_combine_chain() {
    let patterns = detect_in_file("examples/core/result_pipeline.av");
    let chains: Vec<_> = patterns
        .iter()
        .filter_map(|p| match p {
            ModulePattern::ResultPipelineChain {
                fn_name,
                step_count,
                scope,
                ..
            } => Some((scope.clone(), fn_name.clone(), *step_count)),
            _ => None,
        })
        .collect();
    assert!(
        chains.contains(&(None, "validateAndCombine".to_string(), 6)),
        "expected validateAndCombine with 6 `?` steps; got {chains:?}"
    );
    // The single-step smart constructors (parsePositive, doubled,
    // capAtThousand) don't qualify — body is one match, not a
    // binding chain. The manual `match Result.Err -> Err` version
    // (`validateAndCombineNoOp`) has zero `?` bindings.
    assert!(
        chains
            .iter()
            .all(|(_, name, _)| name == "validateAndCombine"),
        "no other fn should match in this module; got {chains:?}"
    );
}

#[test]
fn refinement_module_emits_no_pipeline_chain() {
    let patterns = detect_in_file("examples/refinement/natural/natural.av");
    let chains: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::ResultPipelineChain { .. }))
        .collect();
    assert!(
        chains.is_empty(),
        "smart-constructor module has no `?` chains; got {chains:?}"
    );
}

// ─── Stage 6e: RendererFormatter ────────────────────────────────────────────

#[test]
fn rle_module_pins_show_run_as_renderer() {
    let patterns = detect_in_file("examples/data/rle.av");
    let renderers: Vec<_> = patterns
        .iter()
        .filter_map(|p| match p {
            ModulePattern::RendererFormatter { fn_name, scope } => {
                Some((scope.clone(), fn_name.clone()))
            }
            _ => None,
        })
        .collect();
    // `showRun` is a non-recursive pure interpolation.
    assert!(
        renderers.contains(&(None, "showRun".to_string())),
        "expected showRun renderer; got {renderers:?}"
    );
    // `showRuns` is recursive (self-call in tail arm) — must NOT be
    // emitted by this pattern.
    assert!(
        !renderers.contains(&(None, "showRuns".to_string())),
        "recursive showRuns must not match RendererFormatter; got {renderers:?}"
    );
}

// ─── Stage 6f: MatchDispatcherFold ──────────────────────────────────────────

#[test]
fn fibonacci_module_pins_nth_or_zero_fold() {
    let patterns = detect_in_file("examples/data/fibonacci.av");
    let folds: Vec<_> = patterns
        .iter()
        .filter_map(|p| match p {
            ModulePattern::MatchDispatcherFold {
                fn_name,
                list_param,
                scope,
            } => Some((scope.clone(), fn_name.clone(), list_param.clone())),
            _ => None,
        })
        .collect();
    assert!(
        folds.contains(&(None, "nthOrZero".to_string(), "xs".to_string())),
        "expected nthOrZero fold; got {folds:?}"
    );
    // `showListIntInner` also folds over a list but nests its match —
    // the outer subject is `xs`, both nil and cons arms exist, and
    // the fn self-recurses. This is intentional: the pattern claims
    // *structural* list-fold shape, not "single match arm depth".
    assert!(
        folds.contains(&(None, "showListIntInner".to_string(), "xs".to_string())),
        "expected showListIntInner fold; got {folds:?}"
    );
}

#[test]
fn refinement_module_emits_no_fold_pattern() {
    let patterns = detect_in_file("examples/refinement/natural/natural.av");
    let folds: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::MatchDispatcherFold { .. }))
        .collect();
    assert!(
        folds.is_empty(),
        "smart-constructor module has no list folds; got {folds:?}"
    );
}

#[test]
fn refinement_module_emits_no_wrapper_pattern() {
    let patterns = detect_in_file("examples/refinement/natural/natural.av");
    let wrappers: Vec<_> = patterns
        .iter()
        .filter(|p| matches!(p, ModulePattern::WrapperOverRecursion { .. }))
        .collect();
    assert!(
        wrappers.is_empty(),
        "refinement module has no recursive inner fns; got {wrappers:?}"
    );
}

// ─── Stage 7: inductable_sum_types ──────────────────────────────────────────

fn inductable_in_file(path: &str) -> std::collections::HashSet<String> {
    let source = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("read {path}: {e}"));
    let items = aver::source::parse_source(&source).unwrap_or_else(|e| panic!("parse {path}: {e}"));
    let module_root = std::path::Path::new(path)
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or(".");
    let deps = aver::source::load_compile_deps(&items, module_root)
        .unwrap_or_else(|e| panic!("deps: {e}"));
    collect_inductable_sum_types(&items, &deps)
}

#[test]
fn red_black_tree_module_pins_tree_as_inductable() {
    let s = inductable_in_file("examples/data/red_black_tree.av");
    assert!(
        s.contains("Tree"),
        "Tree is direct-recursive (Empty | Red(Tree,_,Tree) | Black(Tree,_,Tree)); got {s:?}"
    );
}

#[test]
fn refinement_module_emits_no_inductable_sum_types() {
    let s = inductable_in_file("examples/refinement/natural/natural.av");
    assert!(s.is_empty(), "Natural carries no sum types; got {s:?}");
}
