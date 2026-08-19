//! Cross-module same-bare-name `TypeDef` regression.
//!
//! Epic #180 Phase 6 — pins the canonical-key behavior of
//! `backend_named_type_key` / `backend_type_def_key`. Two dep
//! modules each declare a `Shape` sum with different variants;
//! the helpers must hand each declaration a distinct canonical
//! key (`"Round.Shape"`, `"Sharp.Shape"`) so backend registry
//! lookups route to the right TypeDef instead of
//! first-match-wins.

use aver::ast::TypeDef;
use aver::codegen::common::{backend_named_type_key, backend_type_def_key};
use aver::codegen::{ModuleInfo, build_context};
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::nan_value::{Arena, NanValueConvert};
use aver::source::{LoadedModule, parse_source};
use aver::types::Type;
use aver::value::Value;
use aver::vm;
use std::collections::HashMap;
use std::path::PathBuf;

const ENTRY_SRC: &str = r#"module Entry
    intent = "cross-module same-bare-name type regression"
    depends [Round, Sharp]

fn main() -> Int
    1
"#;

const ROUND_SRC: &str = r#"module Round
    intent = "round shapes"
    exposes [Shape]
    depends []

type Shape
    Circle(Int)
    Disc
"#;

const SHARP_SRC: &str = r#"module Sharp
    intent = "sharp shapes"
    exposes [Shape]
    depends []

type Shape
    Triangle
    Square(Int)
"#;

const REEXPORT_LOW_SRC: &str = r#"module Low
    intent = "Own the type handed through an API module."
    exposes [Item, sized]

record Item
    key: String

fn sized(item: Item) -> Int
    ? "Return the key length."
    String.len(item.key)

verify sized
    sized(Item(key = "ab")) => 2
"#;

const REEXPORT_HIGH_SRC: &str = r#"module High
    intent = "Explicitly re-export the dependency type."
    exposes [Item, doubled]
    depends [Low.Low]

fn doubled(item: Item) -> Int
    ? "Double the key length."
    Low.Low.sized(item) * 2

verify doubled
    doubled(Item(key = "ab")) => 4
"#;

const REEXPORT_MID_SRC: &str = r#"module Mid
    intent = "Consume the explicit type re-export."
    exposes [tripled]
    depends [High.High]

fn tripled(item: Item) -> Int
    ? "Add one to the doubled length."
    High.High.doubled(item) + 1

verify tripled
    tripled(Item(key = "ab")) => 5
"#;

const REEXPORT_MAIN_SRC: &str = r#"module Main
    intent = "Reach the re-export consumer as a dependency."
    depends [Mid.Mid, Low.Low]

fn main() -> Int
    Mid.Mid.tripled(Low.Low.Item(key = "ab"))
"#;

const QUALIFIED_REEXPORT_MAIN_SRC: &str = r#"module Main
    intent = "Name the facade's re-export explicitly."
    depends [High.High, Low.Low]

fn accepts(item: High.High.Item) -> Int
    Low.Low.sized(item)

fn main() -> Int
    accepts(Low.Low.Item(key = "ab"))
"#;

const CHAIN_REEXPORT_MAIN_SRC: &str = r#"module Main
    intent = "Consume a type handed through two explicit facades."
    depends [Mid.Mid]

fn main() -> Int
    Mid.Mid.tripled(Item(key = "ab"))
"#;

fn build_three_module_ctx() -> aver::codegen::CodegenContext {
    let mut entry_items = parse_source(ENTRY_SRC).expect("entry parse");
    let loaded: Vec<LoadedModule> = vec![
        LoadedModule {
            dep_name: "Round".to_string(),
            items: parse_source(ROUND_SRC).expect("Round parse"),
            path: PathBuf::from("Round.av"),
        },
        LoadedModule {
            dep_name: "Sharp".to_string(),
            items: parse_source(SHARP_SRC).expect("Sharp parse"),
            path: PathBuf::from("Sharp.av"),
        },
    ];
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let result = pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_build_symbols: true,
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck");
    assert!(
        tc.errors.is_empty(),
        "multi-module fixture should typecheck: {:?}",
        tc.errors
    );
    build_context(
        entry_items.clone(),
        tc,
        result.analysis.as_ref(),
        "test".to_string(),
        modules,
        result.symbol_table,
        result.resolved_items,
    )
}

fn reexport_fixture_files(high_source: &str) -> HashMap<String, String> {
    HashMap::from([
        ("low/low.av".to_string(), REEXPORT_LOW_SRC.to_string()),
        ("high/high.av".to_string(), high_source.to_string()),
        ("mid/mid.av".to_string(), REEXPORT_MID_SRC.to_string()),
    ])
}

fn typecheck_virtual_entry(entry_source: &str, files: &HashMap<String, String>) -> Vec<String> {
    let mut entry_items = parse_source(entry_source).expect("entry parse");
    let root_deps = entry_items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::Module(module) => Some(module.depends.clone()),
            _ => None,
        })
        .expect("entry module declaration");
    let loaded = aver::source::load_module_tree_from_map(&root_deps, files)
        .expect("load virtual dependency tree");
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let result = pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_build_symbols: true,
            dep_modules: &modules,
            ..Default::default()
        },
    );
    result
        .typecheck
        .expect("typecheck result")
        .errors
        .into_iter()
        .map(|error| error.message)
        .collect()
}

fn run_virtual_main(entry_source: &str, files: &HashMap<String, String>) -> Value {
    let mut entry_items = parse_source(entry_source).expect("entry parse");
    let root_deps = entry_items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::Module(module) => Some(module.depends.clone()),
            _ => None,
        })
        .expect("entry module declaration");
    let loaded = aver::source::load_module_tree_from_map(&root_deps, files)
        .expect("load virtual dependency tree");
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let result = pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_build_symbols: true,
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let typecheck = result.typecheck.as_ref().expect("typecheck result");
    assert!(
        typecheck.errors.is_empty(),
        "runtime fixture should typecheck: {:?}",
        typecheck.errors
    );

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_loaded_modules(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        loaded,
        "<test>",
        result.analysis.as_ref(),
    )
    .expect("compile runtime fixture");
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run_top_level().expect("run top level");
    machine
        .run_named_function("main", &[])
        .expect("run main")
        .to_value(&machine.arena)
}

#[test]
fn explicit_type_reexport_survives_when_reexporter_is_a_dependency() {
    let files = reexport_fixture_files(REEXPORT_HIGH_SRC);

    let direct_errors = typecheck_virtual_entry(REEXPORT_MID_SRC, &files);
    assert!(
        direct_errors.is_empty(),
        "Mid checks directly against High's explicit re-export: {direct_errors:?}"
    );

    let nested_errors = typecheck_virtual_entry(REEXPORT_MAIN_SRC, &files);
    assert!(
        nested_errors.is_empty(),
        "the same Mid source must keep the re-export when loaded as a dependency: {nested_errors:?}"
    );
}

#[test]
fn explicit_type_reexport_program_compiles_and_runs_on_the_vm() {
    let files = reexport_fixture_files(REEXPORT_HIGH_SRC);
    assert_eq!(run_virtual_main(REEXPORT_MAIN_SRC, &files), Value::int(5));
}

#[test]
fn dependency_type_stays_hidden_without_an_explicit_reexport() {
    let hidden_high = REEXPORT_HIGH_SRC.replace("exposes [Item, doubled]", "exposes [doubled]");
    let files = reexport_fixture_files(&hidden_high);
    let errors = typecheck_virtual_entry(REEXPORT_MID_SRC, &files);
    assert!(
        errors
            .iter()
            .any(|error| error.contains("Unknown type 'Item'")),
        "a dependency must not gain transitive visibility without `exposes [Item]`: {errors:?}"
    );
}

#[test]
fn qualified_reexport_name_resolves_to_the_original_type_identity() {
    let files = reexport_fixture_files(REEXPORT_HIGH_SRC);
    let errors = typecheck_virtual_entry(QUALIFIED_REEXPORT_MAIN_SRC, &files);
    assert!(
        errors.is_empty(),
        "High.High.Item should name the same nominal type as Low.Low.Item: {errors:?}"
    );
}

#[test]
fn reexport_chain_preserves_the_public_record_shape() {
    let mut files = reexport_fixture_files(REEXPORT_HIGH_SRC);
    files.insert(
        "mid/mid.av".to_string(),
        REEXPORT_MID_SRC.replace("exposes [tripled]", "exposes [Item, tripled]"),
    );
    let errors = typecheck_virtual_entry(CHAIN_REEXPORT_MAIN_SRC, &files);
    assert!(
        errors.is_empty(),
        "a second explicit facade should preserve Low.Item's identity and fields: {errors:?}"
    );
}

#[test]
fn reexport_does_not_make_the_declaring_module_a_qualified_dependency() {
    let files = reexport_fixture_files(REEXPORT_HIGH_SRC);
    let errors = typecheck_virtual_entry(
        r#"module Main
    intent = "The facade is imported, its implementation dependency is not."
    depends [High.High]

fn illegal(item: Low.Low.Item) -> Int
    High.High.doubled(item)
"#,
        &files,
    );
    assert!(
        errors.iter().any(|error| {
            error.contains("Low.Low.Item")
                && (error.contains("not exposed")
                    || error.contains("not visible")
                    || error.contains("Unknown type"))
        }),
        "re-exporting Item must not silently import the Low.Low qualifier: {errors:?}"
    );
}

#[test]
fn backend_type_def_key_distinguishes_cross_module_same_bare_name() {
    let ctx = build_three_module_ctx();

    let round_shape = ctx
        .modules
        .iter()
        .find(|m| m.prefix == "Round")
        .and_then(|m| {
            m.type_defs
                .iter()
                .find(|td| matches!(td, TypeDef::Sum { name, .. } if name == "Shape"))
        })
        .expect("Round.Shape exists");
    let sharp_shape = ctx
        .modules
        .iter()
        .find(|m| m.prefix == "Sharp")
        .and_then(|m| {
            m.type_defs
                .iter()
                .find(|td| matches!(td, TypeDef::Sum { name, .. } if name == "Shape"))
        })
        .expect("Sharp.Shape exists");

    let round_key = backend_type_def_key(&ctx, round_shape);
    let sharp_key = backend_type_def_key(&ctx, sharp_shape);

    assert_eq!(
        round_key, "Round.Shape",
        "Round.Shape should get its module-qualified canonical key, got {round_key:?}"
    );
    assert_eq!(
        sharp_key, "Sharp.Shape",
        "Sharp.Shape should get its module-qualified canonical key, got {sharp_key:?}"
    );
    assert_ne!(
        round_key, sharp_key,
        "cross-module dups must produce distinct registry keys"
    );
}

#[test]
fn backend_named_type_key_routes_id_stamped_named_to_canonical() {
    let ctx = build_three_module_ctx();

    let round_id = ctx
        .symbol_table
        .type_id_of(&aver::ir::TypeKey::in_module("Round", "Shape"))
        .expect("Round.Shape TypeId");
    let sharp_id = ctx
        .symbol_table
        .type_id_of(&aver::ir::TypeKey::in_module("Sharp", "Shape"))
        .expect("Sharp.Shape TypeId");

    let round_named = Type::Named {
        id: Some(round_id),
        name: "Shape".to_string(),
    };
    let sharp_named = Type::Named {
        id: Some(sharp_id),
        name: "Shape".to_string(),
    };
    let round_key = backend_named_type_key(&ctx, &round_named).expect("Named is Some");
    let sharp_key = backend_named_type_key(&ctx, &sharp_named).expect("Named is Some");

    assert_eq!(round_key, "Round.Shape");
    assert_eq!(sharp_key, "Sharp.Shape");
    assert_ne!(round_key, sharp_key);
}

#[test]
/// User-source nominal types must have a typed identity; builtin and host types
/// such as `HttpResponse` are deliberately id-less and retain the name fallback.
fn backend_named_type_key_falls_back_to_name_for_types_with_no_typed_identity() {
    let ctx = build_three_module_ctx();
    let unresolved = Type::Named {
        id: None,
        name: "HttpResponse".to_string(),
    };
    let key = backend_named_type_key(&ctx, &unresolved).expect("Named is Some");
    assert_eq!(
        key, "HttpResponse",
        "types with no typed identity should fall back to the source-faithful name"
    );
}

#[test]
fn backend_named_type_key_returns_none_for_compound_types() {
    let ctx = build_three_module_ctx();

    let list_of_int = Type::List(Box::new(Type::Int));
    assert!(
        backend_named_type_key(&ctx, &list_of_int).is_none(),
        "compound types have no canonical Named key"
    );

    let result_int_str = Type::Result(Box::new(Type::Int), Box::new(Type::Str));
    assert!(backend_named_type_key(&ctx, &result_int_str).is_none());
}
