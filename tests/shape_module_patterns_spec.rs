//! Stage 6a of #232 — pin `ModulePattern::RefinementSmartConstructor`
//! detection on the canonical `examples/refinement/*` corpus. Every
//! shipped refinement module must produce exactly one pattern, and
//! its typed payload must agree with what
//! `codegen::common::refinement_info_for` already returns (so the
//! stage 6b adapter migration is behavior-preserving).

use aver::analysis::shape::{ModulePattern, detect_module_patterns};
use aver::ast::TopLevel;

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
    assert_eq!(
        patterns.len(),
        1,
        "expected exactly one RefinementSmartConstructor on Natural; got {patterns:?}"
    );
    let ModulePattern::RefinementSmartConstructor {
        type_name,
        carrier_field,
        carrier_type,
        constructor_fn,
        ..
    } = &patterns[0];
    assert_eq!(type_name, "Natural");
    assert_eq!(carrier_field, "value");
    assert_eq!(carrier_type, "Int");
    assert_eq!(constructor_fn, "fromInt");
}

#[test]
fn positive_module_emits_one_refinement_pattern() {
    let patterns = detect_in_file("examples/refinement/positive/positive.av");
    assert_eq!(patterns.len(), 1);
    let ModulePattern::RefinementSmartConstructor { type_name, .. } = &patterns[0];
    assert_eq!(type_name, "Positive");
}

#[test]
fn int_range_module_emits_one_refinement_pattern() {
    let patterns = detect_in_file("examples/refinement/int_range/int_range.av");
    assert_eq!(patterns.len(), 1);
    let ModulePattern::RefinementSmartConstructor { type_name, .. } = &patterns[0];
    assert_eq!(type_name, "IntRange");
}

#[test]
fn nonneg_float_module_emits_one_refinement_pattern() {
    let patterns = detect_in_file("examples/refinement/nonneg_float/nonneg_float.av");
    assert_eq!(patterns.len(), 1);
    let ModulePattern::RefinementSmartConstructor {
        type_name,
        carrier_type,
        ..
    } = &patterns[0];
    assert_eq!(type_name, "NonNegFloat");
    assert_eq!(carrier_type, "Float");
}

#[test]
fn module_with_no_refinement_emits_no_pattern() {
    // Plain orchestration module — no opaque + smart constructor pair.
    let patterns = detect_in_file("examples/data/quicksort.av");
    assert!(
        patterns.is_empty(),
        "quicksort is not a refinement module; got {patterns:?}"
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
    let ModulePattern::RefinementSmartConstructor {
        type_name,
        carrier_field,
        carrier_type,
        param_name,
        ..
    } = &patterns[0];

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
        let ModulePattern::RefinementSmartConstructor { predicate, .. } = &patterns[0];
        format!("{:?}", predicate)
    });
}
