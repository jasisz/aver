//! Per-strategy mutation tests. For each `mutations::apply` index
//! we exercise: parse → mutate → unparse → parse on every corpus
//! seed. Failure modes we surface:
//!
//!   - strategy panics (parser interaction has a stack overflow,
//!     etc.) → test panics
//!   - strategy produces an AST shape the unparser doesn't handle
//!     → `UnparseError::Unsupported(_)` with a diagnosable tag
//!   - strategy produces unparseable source → re-parse fails with
//!     a parse error message naming the broken offset
//!
//! The point is to give each strategy its own quick gate before
//! the AFL custom mutator ships them to the fuzz campaigns.
//! Running the suite under `cargo test -p aver-fuzz-mutator
//! --test mutations` exits non-zero the first time a strategy
//! breaks round-trip on the curated corpus — exactly the signal
//! we want before workflow yaml flips the mutator on.

use aver::ast::TopLevel;
use aver::lexer::Lexer;
use aver::parser::Parser;
use aver_fuzz_mutator::{mutations, unparse};
use rand::SeedableRng;
use rand::rngs::SmallRng;
use std::path::PathBuf;

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

/// Try every parseable corpus file through the given strategy
/// index. Returns the count of files where the strategy:
///   - actually mutated → went through unparse → re-parsed cleanly.
fn exercise_strategy(strategy_index: u32, mut rng: SmallRng) -> usize {
    let dir = corpus_dir();
    let mut applied = 0usize;
    for entry in walkdir::WalkDir::new(&dir)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("av") {
            continue;
        }
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        let Ok(mut items) = parse_source(&source) else {
            // Malformed seeds — skip, matches roundtrip test policy.
            continue;
        };
        // Mutator must never panic, regardless of input shape.
        let mutated = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            mutations::apply(strategy_index, &mut rng, &mut items)
        }))
        .unwrap_or_else(|_| panic!("strategy {strategy_index} panicked on {}", path.display()));
        if !mutated {
            continue;
        }
        // Unparse + re-parse — must always succeed for a mutation
        // that the strategy reported as "applied".
        let unparsed = unparse(&items).unwrap_or_else(|e| {
            panic!(
                "strategy {strategy_index} produced shape unparse rejected on {}: {e}",
                path.display()
            )
        });
        parse_source(&unparsed).unwrap_or_else(|e| {
            panic!(
                "strategy {strategy_index} produced unparseable source on {}: {e}\n--- unparsed ---\n{unparsed}",
                path.display()
            )
        });
        applied += 1;
    }
    applied
}

/// Each strategy index gets its own test fn. A panic in any one
/// names the strategy and the corpus seed that broke it; the
/// `applied > 0` lower bound catches strategies that became
/// silent no-ops after a refactor.
macro_rules! strategy_test {
    ($name:ident, $idx:expr, $min_applied:expr) => {
        #[test]
        #[allow(unused_comparisons)]
        fn $name() {
            // Seed 1 — deterministic, picks the first site each
            // strategy collects. Lower-bound count of "this
            // strategy is actually doing something on the
            // corpus" so a future regression that breaks
            // applicability shows up here. Strategies with
            // `min_applied == 0` only test the no-panic contract;
            // the comparison is still emitted so the bound stays
            // visible in source even when the type-limits lint
            // would silently drop it.
            let rng = SmallRng::seed_from_u64(1);
            let applied = exercise_strategy($idx, rng);
            let min_applied: usize = $min_applied;
            assert!(
                applied >= min_applied,
                "strategy {} applied to {} files, expected at least {}",
                $idx,
                applied,
                min_applied
            );
        }
    };
}

// Lower bounds are conservative — most strategies fire on 5+ of
// the 9 valid corpus seeds, but some (record-update mutation,
// when guard) only fire on the 2 seeds that exercise that shape.
strategy_test!(strategy_0_swap_binop, 0, 1);
strategy_test!(strategy_1_int_boundary, 1, 1);
strategy_test!(strategy_2_inject_error_prop, 2, 1);
// strategy 3 (remove_error_prop) lower bound is 0: the corpus
// only has one `?` use site in result_option.av; if the rng
// targeting moves it off that file the strategy is a no-op.
strategy_test!(strategy_3_remove_error_prop, 3, 0_usize);
strategy_test!(strategy_4_swap_match_arms, 4, 1);
// strategy 5 (swap_bang_element) lower bound is 0: only seeds
// that exercise `(a, b)!` independent products will fire the
// permutation. The corpus has at most a handful, so a strict
// per-seed lower bound would be brittle.
strategy_test!(strategy_5_swap_bang_element, 5, 0_usize);
strategy_test!(strategy_6_swap_type_annotation, 6, 1);
strategy_test!(strategy_7_rename_param, 7, 1);
// strategy 8 (swap_adjacent_statements) lower bound is 0:
// most corpus fns are single-expression bodies.
strategy_test!(strategy_8_swap_statements, 8, 0_usize);
strategy_test!(strategy_9_negate_expression, 9, 1);
strategy_test!(strategy_10_bare_namespace, 10, 1);
strategy_test!(strategy_11_toggle_effects, 11, 1);
// strategy 12 (reverse_match_arms) needs `match` with ≥ 2 arms.
// Several corpus seeds (Result match, list match) have that
// shape, so 1 is a safe lower bound.
strategy_test!(strategy_12_reverse_match_arms, 12, 1);
// strategy 13 (shift_map_entries) lower bound is 0: the corpus
// includes map literals on at most a couple of seeds, so a
// strict per-seed bound would be brittle to corpus churn.
strategy_test!(strategy_13_shift_map_entries, 13, 0_usize);
// strategy 14 (replace-fn-body) needs at least one pure FnDef
// whose return type the type-driven generator can parse. Every
// non-empty corpus seed has at least one such fn (math, hello,
// effects' `dump` is pure-Result, types' `area` is Float).
strategy_test!(strategy_14_replace_fn_body, 14, 1);
// strategy 15 (inject-fn) appends a fresh fn — always applies to
// any corpus seed that parses, because the generator falls back
// to a literal of one of the three supported return types when
// scope is empty.
strategy_test!(strategy_15_inject_fn, 15, 1);
// strategy 16 (insert-depends) fires on every seed that has a
// `module` declaration, which is every non-empty corpus seed in
// the parser dir bar `empty.av`.
strategy_test!(strategy_16_insert_depends, 16, 1);
// strategy 17 (inject-cross-module-call) needs at least one
// expression site in a fn body — every non-empty seed qualifies.
strategy_test!(strategy_17_inject_cross_call, 17, 1);
// strategy 18 (attempt-opaque-violation) lower bound is 0: only
// seeds with `exposes opaque [T]` (the new multi-module ones)
// will fire. Original corpus has no opaque exposures.
strategy_test!(strategy_18_opaque_violation, 18, 0_usize);
// strategy 19 (swap-exposes-acl) fires on any seed declaring a
// non-empty `exposes [...]`. Most non-trivial seeds do.
strategy_test!(strategy_19_swap_exposes_acl, 19, 1);

/// `strategy_name` is the label AFL embeds into queue + crash
/// filenames via the custom mutator's `describe` hook, so it has to
/// be unique per strategy (otherwise triage can't tell two
/// strategies apart) and filename-safe (no `/`, no NUL, no spaces).
#[test]
fn strategy_names_are_unique_and_filename_safe() {
    use std::collections::HashSet;
    let mut seen: HashSet<&'static str> = HashSet::new();
    for idx in 0..aver_fuzz_mutator::mutations::STRATEGY_COUNT {
        let name = aver_fuzz_mutator::mutations::strategy_name(idx);
        assert!(
            !name.is_empty(),
            "strategy {} returned empty label",
            idx
        );
        assert!(
            name.bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'-' || b == b'_'),
            "strategy {} label `{}` contains a filename-hostile character (AFL splices it into queue filenames)",
            idx,
            name,
        );
        assert!(
            seen.insert(name),
            "strategy {} label `{}` collides with an earlier strategy",
            idx,
            name,
        );
    }
    assert_eq!(seen.len() as u32, aver_fuzz_mutator::mutations::STRATEGY_COUNT);
}
