//! Integration suite for the fuzz-only unparser.
//!
//! Two guarantees we test:
//!   1. `parse → unparse → parse` succeeds on every committed
//!      corpus file (the seeds AFL fuzz_parse_bytes / typecheck
//!      targets feed from). If the unparser misses an AST shape,
//!      one of these files breaks.
//!   2. `parse → small mutate → unparse → parse` succeeds on the
//!      same corpus, modeled as a stand-in for the AFL custom
//!      mutator: parse, swap one expression's children, unparse,
//!      re-parse. Tests the unparser against AST shapes the mutator
//!      will produce, not just ones the user wrote.
//!
//! No `Debug`-shape comparison or AST equality across roundtrips —
//! the unparser doesn't guarantee semantic identity, only parser
//! acceptance. That contract is enough for the AFL mutator to
//! trust its output.

use aver::ast::{Expr, TopLevel};
use aver::lexer::Lexer;
use aver::parser::Parser;
use aver_fuzz_mutator::unparse;
use std::path::{Path, PathBuf};

fn corpus_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("corpus")
        .join("parser")
}

fn parse_source(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| format!("lex: {e}"))?;
    let mut parser = Parser::new(tokens);
    parser.parse().map_err(|e| format!("parse: {e}"))
}

fn assert_roundtrip(path: &Path) {
    let source = std::fs::read_to_string(path).expect("read corpus file");
    let original = match parse_source(&source) {
        Ok(items) => items,
        Err(e) => {
            // `malformed_*.av` corpus seeds are intentionally
            // un-parseable. The unparser has nothing to do with
            // them — skip rather than fail.
            if path
                .file_name()
                .and_then(|s| s.to_str())
                .is_some_and(|n| n.starts_with("malformed_") || n == "empty.av")
            {
                eprintln!("skip {} (parse error: {e})", path.display());
                return;
            }
            panic!("corpus seed {} doesn't parse: {e}", path.display());
        }
    };
    let unparsed = unparse(&original).unwrap_or_else(|e| panic!("unparse {}: {e}", path.display()));
    let _reparsed = parse_source(&unparsed).unwrap_or_else(|e| {
        panic!(
            "roundtrip failed for {}: {e}\n--- unparsed source ---\n{unparsed}",
            path.display()
        )
    });
}

#[test]
fn corpus_roundtrips_through_unparse() {
    let dir = corpus_dir();
    let mut tested = 0usize;
    for entry in walkdir::WalkDir::new(&dir)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("av") {
            continue;
        }
        assert_roundtrip(path);
        tested += 1;
    }
    assert!(
        tested > 0,
        "no .av corpus files found under {} — test scaffold broken",
        dir.display()
    );
    eprintln!("corpus_roundtrips_through_unparse: {tested} files");
}

/// Walk the AST and swap the two operands of the first `BinOp` we
/// find. Returns `true` if a swap happened. Models the simplest
/// mutation the AFL mutator will perform.
fn swap_first_binop(items: &mut [TopLevel]) -> bool {
    fn visit_expr(expr: &mut Expr) -> bool {
        match expr {
            Expr::BinOp(_, lhs, rhs) => {
                std::mem::swap(lhs, rhs);
                true
            }
            Expr::Neg(inner) => visit_expr(&mut inner.node),
            Expr::Attr(inner, _) => visit_expr(&mut inner.node),
            Expr::FnCall(callee, args) => {
                if visit_expr(&mut callee.node) {
                    return true;
                }
                for a in args {
                    if visit_expr(&mut a.node) {
                        return true;
                    }
                }
                false
            }
            Expr::Constructor(_, Some(inner)) => visit_expr(&mut inner.node),
            Expr::Constructor(_, None) => false,
            Expr::Match { subject, arms } => {
                if visit_expr(&mut subject.node) {
                    return true;
                }
                for arm in arms {
                    if visit_expr(&mut arm.body.node) {
                        return true;
                    }
                }
                false
            }
            Expr::ErrorProp(inner) => visit_expr(&mut inner.node),
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                for item in items {
                    if visit_expr(&mut item.node) {
                        return true;
                    }
                }
                false
            }
            Expr::MapLiteral(entries) => {
                for (k, v) in entries {
                    if visit_expr(&mut k.node) {
                        return true;
                    }
                    if visit_expr(&mut v.node) {
                        return true;
                    }
                }
                false
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, val) in fields {
                    if visit_expr(&mut val.node) {
                        return true;
                    }
                }
                false
            }
            Expr::RecordUpdate { base, updates, .. } => {
                if visit_expr(&mut base.node) {
                    return true;
                }
                for (_, val) in updates {
                    if visit_expr(&mut val.node) {
                        return true;
                    }
                }
                false
            }
            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let aver::ast::StrPart::Parsed(e) = part
                        && visit_expr(&mut e.node)
                    {
                        return true;
                    }
                }
                false
            }
            // Leaves + post-parse-only variants we won't see.
            _ => false,
        }
    }

    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let body = std::sync::Arc::make_mut(&mut fd.body);
            for stmt in body.stmts_mut() {
                let expr = match stmt {
                    aver::ast::Stmt::Expr(e) => e,
                    aver::ast::Stmt::Binding(_, _, e) => e,
                };
                if visit_expr(&mut expr.node) {
                    return true;
                }
            }
        }
    }
    false
}

#[test]
fn corpus_roundtrips_after_one_mutation() {
    let dir = corpus_dir();
    let mut mutated = 0usize;
    let mut no_binop = 0usize;
    for entry in walkdir::WalkDir::new(&dir)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("av") {
            continue;
        }
        let source = std::fs::read_to_string(path).expect("read corpus");
        let Ok(mut items) = parse_source(&source) else {
            // Intentionally malformed seeds — skip, matches the
            // round-trip test's policy.
            continue;
        };
        if !swap_first_binop(&mut items) {
            no_binop += 1;
            continue;
        }
        let unparsed = unparse(&items)
            .unwrap_or_else(|e| panic!("unparse after mutate {}: {e}", path.display()));
        let _reparsed = parse_source(&unparsed).unwrap_or_else(|e| {
            panic!(
                "post-mutation roundtrip failed for {}: {e}\n--- unparsed source ---\n{unparsed}",
                path.display()
            )
        });
        mutated += 1;
    }
    assert!(mutated > 0, "no corpus file contained a BinOp to mutate");
    eprintln!(
        "corpus_roundtrips_after_one_mutation: {mutated} mutated, {no_binop} skipped (no binop)"
    );
}
