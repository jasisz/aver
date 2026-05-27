//! Typed-HIR foundation gate (epic #180 Phase 2).
//!
//! Pins the contract every later epic phase builds on:
//!
//! > **For every reachable expression in a well-typed Aver program,
//! > `Spanned<ResolvedExpr>::ty().is_some()` and equals the type the
//! > typechecker inferred at the corresponding source `Spanned<Expr>`.**
//!
//! Aver already has the inline type slot on `Spanned<T>` (the
//! `OnceLock<Type>` field) and `NameResolve` transfers stamps from
//! `Spanned<ast::Expr>` to `Spanned<ResolvedExpr>` at lift time
//! (`src/ir/hir/resolve.rs:283-284`). The `wasm_gc` and VM backends
//! already consume `.ty()` heavily. What's missing for the rest of
//! the epic is a regression net that says: yes, every node in
//! every reachable expression gets stamped. Without that gate, the
//! Phase 3+ migration of Rust / Lean / Dafny onto `.ty()` would be
//! building on sand — a missed stamp in some corner shape would
//! silently surface as `None` and the backend would have to fall
//! back to the side-channel anyway.
//!
//! ## What "reachable" means
//!
//! - Entry-scope and dep-module top-level fns (`ResolvedTopLevel::FnDef`).
//! - Every nested `Spanned<ResolvedExpr>` inside each fn's body
//!   (statements, sub-expressions, match arms, call args, etc.).
//! - Passthrough `TopLevel` variants (verify blocks, decisions,
//!   typedefs) are skipped — they're not on the typed-HIR hot path
//!   and the contract above scopes to fn bodies.
//!
//! ## Corpus
//!
//! A small hand-picked set of single-module + multi-module
//! examples that exercise the canonical type-bearing expression
//! shapes: literals, binops, calls (builtin + user), ctors
//! (builtin + user), pattern matches, record-creates, refinement
//! lifts, cross-module qualified calls. Goal is COVERAGE of shape
//! variants, not exhaustive examples — the test is a foundation
//! gate, not an emit smoke test (we have those elsewhere).
//!
//! ## When this test fails
//!
//! Some `Spanned<ResolvedExpr>` survived the pipeline without a
//! `.ty()` stamp. Two cases:
//!
//! 1. The typechecker missed a stamp on the source `Spanned<Expr>`
//!    — fix in `src/types/checker/infer/expr.rs` to call
//!    `.set_ty()` for that shape.
//! 2. `NameResolve` produced a fresh `Spanned<ResolvedExpr>` whose
//!    `.ty` slot wasn't seeded from the source span — fix in
//!    `src/ir/hir/resolve.rs::resolve_spanned`.

#![cfg(feature = "runtime")]

use aver::ir::hir::{ResolvedExpr, ResolvedFnBody, ResolvedStmt, ResolvedTopLevel};
use aver::ir::{PipelineConfig, TypecheckMode, pipeline};
use aver::source::{LoadedModule, parse_source};

/// Single-module Aver source — entry items only, no deps.
fn run_single(source: &str) -> Vec<String> {
    let mut items = parse_source(source).unwrap_or_else(|e| panic!("parse: {e}"));
    let result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(
        tc.errors.is_empty(),
        "typecheck must pass for stamp-invariant fixtures: {:?}",
        tc.errors
    );
    let mut violations = Vec::new();
    for item in &result.resolved_items {
        if let ResolvedTopLevel::FnDef(rfd) = item {
            check_fn_body(&rfd.name, rfd.body.as_ref(), &mut violations);
        }
    }
    violations
}

/// Multi-module Aver — entry + dep modules wrapped as `LoadedModule`.
fn run_multi(entry_source: &str, deps: &[(&str, &str)]) -> Vec<String> {
    let mut entry_items = parse_source(entry_source).unwrap_or_else(|e| panic!("entry parse: {e}"));
    let loaded: Vec<LoadedModule> = deps
        .iter()
        .map(|(prefix, src)| LoadedModule {
            dep_name: prefix.to_string(),
            items: parse_source(src).unwrap_or_else(|e| panic!("dep '{prefix}' parse: {e}")),
            path: std::path::PathBuf::from(format!("{prefix}.av")),
        })
        .collect();
    let result = pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(
        tc.errors.is_empty(),
        "typecheck must pass for multi-module fixture: {:?}",
        tc.errors
    );
    let mut violations = Vec::new();
    for item in &result.resolved_items {
        if let ResolvedTopLevel::FnDef(rfd) = item {
            check_fn_body(&rfd.name, rfd.body.as_ref(), &mut violations);
        }
    }
    violations
}

fn check_fn_body(fn_name: &str, body: &ResolvedFnBody, violations: &mut Vec<String>) {
    match body {
        ResolvedFnBody::Block(stmts) => {
            for stmt in stmts {
                check_stmt(fn_name, stmt, violations);
            }
        }
    }
}

fn check_stmt(fn_name: &str, stmt: &ResolvedStmt, violations: &mut Vec<String>) {
    match stmt {
        ResolvedStmt::Binding { value, .. } => check_expr(fn_name, value, violations),
        ResolvedStmt::Expr(e) => check_expr(fn_name, e, violations),
    }
}

fn check_expr(
    fn_name: &str,
    expr: &aver::ast::Spanned<ResolvedExpr>,
    violations: &mut Vec<String>,
) {
    if expr.ty().is_none() {
        violations.push(format!(
            "fn `{}` line {}: Spanned<ResolvedExpr> has no .ty() stamp — node shape {}",
            fn_name,
            expr.line,
            expr_shape_label(&expr.node)
        ));
    }
    // Recurse into children whether or not the current node was
    // stamped — every child must also carry a stamp, and reporting
    // multiple violations at once is more useful than bailing out.
    match &expr.node {
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
        ResolvedExpr::Attr(obj, _) => check_expr(fn_name, obj, violations),
        ResolvedExpr::Call(_, args) => {
            for a in args {
                check_expr(fn_name, a, violations);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            check_expr(fn_name, l, violations);
            check_expr(fn_name, r, violations);
        }
        ResolvedExpr::Neg(inner) | ResolvedExpr::ErrorProp(inner) => {
            check_expr(fn_name, inner, violations)
        }
        ResolvedExpr::Match { subject, arms } => {
            check_expr(fn_name, subject, violations);
            for arm in arms {
                check_expr(fn_name, &arm.body, violations);
            }
        }
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                check_expr(fn_name, a, violations);
            }
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            use aver::ir::hir::ResolvedStrPart;
            for p in parts {
                if let ResolvedStrPart::Parsed(inner) = p {
                    check_expr(fn_name, inner, violations);
                }
            }
        }
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => {
            for item in items {
                check_expr(fn_name, item, violations);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                check_expr(fn_name, k, violations);
                check_expr(fn_name, v, violations);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                check_expr(fn_name, v, violations);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            check_expr(fn_name, base, violations);
            for (_, v) in updates {
                check_expr(fn_name, v, violations);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                check_expr(fn_name, a, violations);
            }
        }
    }
}

fn expr_shape_label(node: &ResolvedExpr) -> &'static str {
    match node {
        ResolvedExpr::Literal(_) => "Literal",
        ResolvedExpr::Ident(_) => "Ident",
        ResolvedExpr::Resolved { .. } => "Resolved (local slot)",
        ResolvedExpr::Attr(_, _) => "Attr",
        ResolvedExpr::Call(_, _) => "Call",
        ResolvedExpr::BinOp(_, _, _) => "BinOp",
        ResolvedExpr::Neg(_) => "Neg",
        ResolvedExpr::Match { .. } => "Match",
        ResolvedExpr::Ctor(_, _) => "Ctor",
        ResolvedExpr::ErrorProp(_) => "ErrorProp",
        ResolvedExpr::InterpolatedStr(_) => "InterpolatedStr",
        ResolvedExpr::List(_) => "List",
        ResolvedExpr::Tuple(_) => "Tuple",
        ResolvedExpr::IndependentProduct(_, _) => "IndependentProduct",
        ResolvedExpr::MapLiteral(_) => "MapLiteral",
        ResolvedExpr::RecordCreate { .. } => "RecordCreate",
        ResolvedExpr::RecordUpdate { .. } => "RecordUpdate",
        ResolvedExpr::TailCall { .. } => "TailCall",
    }
}

fn assert_no_violations(label: &str, violations: Vec<String>) {
    if !violations.is_empty() {
        panic!(
            "typed-HIR foundation gate tripped on `{label}` ({} node(s) without .ty() stamp):\n{}",
            violations.len(),
            violations.join("\n")
        );
    }
}

// ────────────────────────────────────────────────────────────────────
// Shape-coverage fixtures
//
// Each test targets one or more ResolvedExpr variants that must
// reach the typechecker stamp path. Skip TailCall — TCO transforms
// happen after typecheck and the inserted TailCall nodes are
// synthesised post-stamp; resolve.rs documents the carve-out.
// ────────────────────────────────────────────────────────────────────

#[test]
fn literal_and_binop_stamps() {
    let violations = run_single(
        r#"module Tmp
    intent = "literal + binop"
    depends []

fn main() -> Int
    (1 + 2) * 3
"#,
    );
    assert_no_violations("literal_and_binop_stamps", violations);
}

#[test]
fn match_arms_stamps() {
    let violations = run_single(
        r#"module Tmp
    intent = "match arm bodies"
    depends []

fn classify(n: Int) -> Int
    match n
        0 -> 0
        _ -> n + 1

fn main() -> Int
    classify(5)
"#,
    );
    assert_no_violations("match_arms_stamps", violations);
}

#[test]
fn list_and_tuple_stamps() {
    let violations = run_single(
        r#"module Tmp
    intent = "list and tuple"
    depends []

fn pair() -> Tuple<Int, Int>
    (1, 2)

fn list() -> List<Int>
    [1, 2, 3]

fn main() -> Int
    List.len(list())
"#,
    );
    assert_no_violations("list_and_tuple_stamps", violations);
}

#[test]
fn record_and_attr_stamps() {
    let violations = run_single(
        r#"module Tmp
    intent = "record create + attr access"
    depends []

record Point
    x: Int
    y: Int

fn origin() -> Point
    Point(x = 0, y = 0)

fn main() -> Int
    origin().x + origin().y
"#,
    );
    assert_no_violations("record_and_attr_stamps", violations);
}

#[test]
fn ctor_and_option_stamps() {
    let violations = run_single(
        r#"module Tmp
    intent = "Option ctor + match"
    depends []

fn wrap(n: Int) -> Option<Int>
    Option.Some(n)

fn unwrap(o: Option<Int>) -> Int
    match o
        Option.None     -> -1
        Option.Some(n)  -> n

fn main() -> Int
    unwrap(wrap(42))
"#,
    );
    assert_no_violations("ctor_and_option_stamps", violations);
}

#[test]
fn string_interp_stamps() {
    let violations = run_single(
        r#"module Tmp
    intent = "interpolated string"
    depends []

fn greet(name: String) -> String
    "hello {name}"

fn main() -> Int
    String.len(greet("world"))
"#,
    );
    assert_no_violations("string_interp_stamps", violations);
}

#[test]
fn cross_module_call_stamps() {
    let entry = r#"module Entry
    intent = "cross-module call"
    depends [Worker]

fn main() -> Int
    Worker.compute(7) + 1
"#;
    let worker = r#"module Worker
    intent = "callable from entry"
    exposes [compute]
    depends []

fn compute(n: Int) -> Int
    n * 2
"#;
    let violations = run_multi(entry, &[("Worker", worker)]);
    assert_no_violations("cross_module_call_stamps", violations);
}
