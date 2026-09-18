//! Executable source/protocol observations are independent of effect providers.
#[path = "support/aver_cmd.rs"]
mod aver_cmd;
use aver_cmd::{aver_bin, format_output, repo_root};
use std::process::Command;

fn fixture() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    for name in ["main.av", "pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(
            repo_root()
                .join("tests/fixtures/yield_request_traces")
                .join(name),
            dir.path().join(name),
        )
        .unwrap();
    }
    let main = dir.path().join("main.av");
    let text = std::fs::read_to_string(&main).unwrap();
    std::fs::write(main, format!("{text}{}", r#"
fn prefix() -> Tuple<__CountTraceCountResult, __CountTraceCountResult, __CountTraceCountResult>
    answer = __CountTraceInput.AnswerClaim(Option.Some(10))
    atYield = __countSourceTrace(2, [answer])
    resumed = __countSourceTrace(2, [answer, __CountTraceInput.Advance])
    wrong = __countSourceTrace(2, [answer, answer])
    (atYield, resumed, wrong)

fn observesPrefix() -> Bool
    match prefix()
        (atYield, resumed, wrong) -> Bool.and(atYield.position == 1, Bool.and(atYield.consumed == 1, Bool.and(atYield.pending == Option.Some(__CountTraceQuery.Yield), Bool.and(resumed.position == 1, Bool.and(resumed.consumed == 2, Bool.and(resumed.pending == Option.Some(__CountTraceQuery.Claim(10)), Bool.and(wrong.position == 1, Bool.and(wrong.consumed == 1, Bool.and(Bool.not(wrong.valid), List.len(wrong.remaining) == 1)))))))))


fn completed() -> Bool
    inputs = [__CountTraceInput.AnswerClaim(Option.None), __CountTraceInput.Advance]
    observed = __countSourceTrace(2, inputs)
    Bool.and(observed.value == Option.Some(0), Bool.and(observed.position == 1, observed.remaining == [__CountTraceInput.Advance]))

fn earlyError() -> Bool
    inputs = [__MixedTraceInput.AnswerClaim(Option.Some(2)), __MixedTraceInput.AnswerClaim(Option.Some(3)), __MixedTraceInput.AnswerNotice(Unit), __MixedTraceInput.AnswerFinish(Result.Err("stop")), __MixedTraceInput.Advance]
    observed = __mixedSourceTrace(1, inputs)
    expected: Result<Int, String> = Result.Err("stop")
    Bool.and(observed.value == Option.Some(expected), Bool.and(observed.position == 4, Bool.and(observed.consumed == 4, observed.remaining == [__MixedTraceInput.Advance])))

fn timeOrder() -> Bool
    inputs = [__TimedTraceInput.AnswerHostTimeUnixMs(100), __TimedTraceInput.AnswerClaim(Option.Some(5)), __TimedTraceInput.AnswerHostTimeUnixMs(110)]
    observed = __timedSourceTraceFrom(3, inputs, 7, [], 12)
    Bool.and(observed.events == [__TimedTraceEvent.ObservedHostTimeUnixMs(7, 100), __TimedTraceEvent.ObservedClaim(8, 3, Option.Some(5)), __TimedTraceEvent.ObservedHostTimeUnixMs(9, 110)], Bool.and(observed.position == 10, observed.consumed == 15))

verify observesPrefix
    observesPrefix() => true
verify completed
    completed() => true
verify earlyError
    earlyError() => true
verify timeOrder
    timeOrder() => true
"#)).unwrap();
    dir
}

fn check(args: &[&str]) {
    let dir = fixture();
    let out = Command::new(aver_bin())
        .arg("verify")
        .arg(dir.path().join("main.av"))
        .arg("--module-root")
        .arg(dir.path())
        .args(args)
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        format_output(&out).contains("35/35 cases passed"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn prefixes_preserve_positions_remainders_and_in_place_effects() {
    check(&[]);
}

#[cfg(feature = "wasm")]
#[test]
fn generated_observers_run_on_wasm_gc() {
    check(&["--wasm-gc"]);
}

#[test]
fn original_private_source_is_retained_under_its_module() {
    let dir = fixture();
    let source = std::fs::read_to_string(dir.path().join("main.av")).unwrap();
    let mut items = aver::source::parse_source(&source).unwrap();
    let original = items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::FnDef(fd) if fd.name == "read" => Some(fd.clone()),
            _ => None,
        })
        .unwrap();
    let len = items.len();
    let base = dir.path().to_str().unwrap();
    let result = aver::ir::pipeline::front_gate(
        &mut items,
        &aver::ir::TypecheckMode::Full {
            base_dir: Some(base),
        },
        len,
        &aver::config::MarkedCapabilities::for_project_dir(Some(base)),
    );
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    let module = items
        .iter()
        .find_map(|item| match item {
            aver::ast::TopLevel::Module(m) => Some(m),
            _ => None,
        })
        .unwrap();
    assert_eq!(module.name, "RequestTraces");
    assert!(!module.exposes.iter().any(|n| n == "read"));
    let retained = module
        .yield_sources
        .iter()
        .find(|fd| fd.name == "read")
        .unwrap();
    assert_eq!(retained.line, original.line);
    assert_eq!(retained.body, original.body);
    assert!(
        !items
            .iter()
            .any(|item| matches!(item, aver::ast::TopLevel::FnDef(fd) if fd.name == "read"))
    );
}

#[test]
fn imported_observers_keep_private_helpers_in_the_owning_module() {
    let root = repo_root().join("tests/fixtures/yield_request_trace_imports");
    let mut backends = vec![vec![]];
    if cfg!(feature = "wasm") {
        backends.push(vec!["--wasm-gc"]);
    }
    for args in backends {
        let out = Command::new(aver_bin())
            .arg("verify")
            .arg(root.join("main.av"))
            .arg("--module-root")
            .arg(&root)
            .args(args)
            .output()
            .unwrap();
        assert!(out.status.success(), "{}", format_output(&out));
    }
}

#[test]
fn recursive_imports_without_owning_module_contracts_are_explicitly_rejected() {
    let dir = tempfile::tempdir().unwrap();
    let source = repo_root().join("tests/fixtures/yield_module_helpers");
    for name in ["looper.av", "pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(source.join(name), dir.path().join(name)).unwrap();
    }
    std::fs::write(dir.path().join("main.av"), "module Client\n    depends [Looper, Pool, Pooled]\n\nfn parent(id: Int) -> Int\n    ! [Pool.claim, yield]\n    Looper.loop(id, 0)\n\nverify __parentSourceTrace law correspondence\n    given id: Int = [1]\n    given inputs: List<__ParentTraceInput> = [[]]\n    using []\n    __parentSourceTrace(id, inputs) == __parentProtocolTrace(id, inputs) holds\n").unwrap();
    let out = Command::new(aver_bin())
        .arg("check")
        .arg(dir.path().join("main.av"))
        .arg("--module-root")
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(!out.status.success(), "{}", format_output(&out));
    let text = format_output(&out);
    assert!(
        text.contains("No trace-equivalence claim was generated"),
        "{text}"
    );
    assert!(text.contains("compositional subtrace theorem"), "{text}");
}

#[test]
fn tail_helper_boundaries_preserve_answers_and_align_imported_prefixes() {
    let root = repo_root().join("tests/fixtures/yield_tail_traces");
    let mut backends = vec![vec![]];
    if cfg!(feature = "wasm") {
        backends.push(vec!["--wasm-gc"]);
    }
    for args in backends {
        let out = Command::new(aver_bin())
            .arg("verify")
            .arg(root.join("main.av"))
            .arg("--module-root")
            .arg(&root)
            .args(args)
            .output()
            .unwrap();
        assert!(out.status.success(), "{}", format_output(&out));
    }
}

#[test]
fn recursive_helpers_preserve_subtraces_across_calls_and_tail_entry() {
    let root = repo_root().join("tests/fixtures/yield_recursive_traces");
    let mut backends = vec![vec![]];
    if cfg!(feature = "wasm") {
        backends.push(vec!["--wasm-gc"]);
    }
    for entry in ["main.av", "mixed.av"] {
        for args in &backends {
            let out = Command::new(aver_bin())
                .arg("verify")
                .arg(root.join(entry))
                .arg("--module-root")
                .arg(&root)
                .args(args)
                .output()
                .unwrap();
            assert!(out.status.success(), "{}", format_output(&out));
        }
    }
}

#[test]
fn importing_a_finite_parent_uses_its_private_recursive_helper_contract() {
    let dir = tempfile::tempdir().unwrap();
    let fixture = repo_root().join("tests/fixtures/yield_recursive_traces");
    for name in ["pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(fixture.join(name), dir.path().join(name)).unwrap();
    }
    let leaf = std::fs::read_to_string(fixture.join("main.av"))
        .unwrap()
        .replace("module RecursiveTraces", "module Leaf");
    std::fs::write(dir.path().join("leaf.av"), leaf).unwrap();
    std::fs::write(
        dir.path().join("main.av"),
        r#"
module Client
    depends [Leaf, Pool, Pooled]

fn client(n: Int) -> Int
    ! [Pool.claim, yield]
    Leaf.parent(n)

verify __clientSourceTrace law correspondence
    given n: Int = [0]
    given inputs: List<__ClientTraceInput> = [[]]
    using []
    __clientSourceTrace(n, inputs) == __clientProtocolTrace(n, inputs) holds
"#,
    )
    .unwrap();
    let out = Command::new(aver_bin())
        .arg("verify")
        .arg(dir.path().join("main.av"))
        .arg("--module-root")
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", format_output(&out));
}

#[test]
fn local_recursion_with_a_finite_import_requires_the_owning_splice_law() {
    let fixture = repo_root().join("tests/fixtures/yield_recursive_traces");
    let dir = tempfile::tempdir().unwrap();
    for name in ["pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(fixture.join(name), dir.path().join(name)).unwrap();
    }
    std::fs::write(
        dir.path().join("leaf.av"),
        r#"module Leaf
    depends [Pool, Pooled]
    exposes [read]
    effects [Pool.claim, yield]
fn read(n: Int) -> Int
    ! [Pool.claim, yield]
    match Pool.claim(n)
        Option.None -> n
        Option.Some(value) -> value
"#,
    )
    .unwrap();
    let text = std::fs::read_to_string(fixture.join("main.av"))
        .unwrap()
        .replace("depends [Pool, Pooled]", "depends [Pool, Pooled, Leaf]")
        .replace("value + 10", "Leaf.read(value) + 10");
    std::fs::write(dir.path().join("main.av"), text).unwrap();
    let out = Command::new(aver_bin())
        .arg("check")
        .arg(dir.path().join("main.av"))
        .arg("--module-root")
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(!out.status.success());
    assert!(
        format_output(&out).contains("import composition needs an owning-module cursor law"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn imported_recursive_contracts_preserve_repeated_calls_private_helpers_and_foreign_tokens() {
    let root = repo_root().join("tests/fixtures/yield_recursive_imports");
    let mut backends = vec![vec![]];
    if cfg!(feature = "wasm") {
        backends.push(vec!["--wasm-gc"]);
    }
    for (entry, count) in [
        ("main.av", "40/40 cases passed"),
        ("private.av", "19/19 cases passed"),
    ] {
        for args in &backends {
            let out = Command::new(aver_bin())
                .arg("verify")
                .arg(root.join(entry))
                .arg("--module-root")
                .arg(&root)
                .args(args)
                .output()
                .unwrap();
            assert!(out.status.success(), "{}", format_output(&out));
            assert!(
                format_output(&out).contains(count),
                "{}",
                format_output(&out)
            );
        }
    }
}
