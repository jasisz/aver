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
use aver::source::{LoadedModule, parse_source};
use aver::types::Type;
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
