//! Project proof rules: a `rules [...]` module and a law's `by Module.rule`
//! line. The rule writes the law's proof as steps; the kernel written in Aver
//! checks them like any other, and Lean checks them again. A rule that
//! refuses, a rule whose proof the kernel refuses and a rule that never
//! returns all leave the law open, with the reason.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const FIXTURES: &str = "tests/fixtures/proof_rules";

/// The laws of `shuffles.av`, each closed by `Rules.Stack.openByLength`.
const SHUFFLE_LAWS: [&str; 3] = [
    "shuffled.rotThreeTimesIsTheTopThree",
    "shuffled.swapTwiceIsTheTopPair",
    "shuffled.twoSwapTwiceIsTheTopFour",
];

fn aver_in(dir: &Path, args: &[&str], env: &[(&str, &str)]) -> Output {
    let mut cmd = Command::new(aver_bin());
    cmd.args(args).current_dir(dir);
    for (k, v) in env {
        cmd.env(k, v);
    }
    cmd.output().expect("aver runs")
}

fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-rules-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    dir
}

fn fixtures() -> PathBuf {
    repo_root().join(FIXTURES)
}

/// The last JSON line of a `--check-json` run.
fn summary(out: &Output) -> serde_json::Value {
    let text = String::from_utf8_lossy(&out.stdout);
    let line = text
        .lines()
        .rev()
        .find(|l| l.starts_with('{'))
        .unwrap_or_else(|| panic!("no JSON summary\n{}", format_output(out)));
    serde_json::from_str(line).expect("JSON summary")
}

#[test]
fn a_rule_closes_laws_the_automatic_steps_leave_open() {
    let dir = fixtures();
    let out = scratch("closes");
    let by_rule = aver_in(
        &dir,
        &[
            "proof",
            "shuffles.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
        &[],
    );
    assert!(by_rule.status.success(), "{}", format_output(&by_rule));
    let text = String::from_utf8_lossy(&by_rule.stdout);
    for law in SHUFFLE_LAWS {
        let line = text
            .lines()
            .find(|l| l.contains(&format!("{law}:")))
            .unwrap_or_else(|| panic!("no line for {law}\n{text}"));
        assert!(
            line.contains("closed by steps (proof by Rules.Stack.openByLength ("),
            "{line}"
        );
        assert!(line.ends_with(" steps)"), "{line}");
    }
    // Every script records the rule and its source hash, and the kernel
    // run on the VM accepts the scripts as written to disk.
    let files: Vec<PathBuf> = SHUFFLE_LAWS
        .iter()
        .map(|law| out.join("proof_steps").join(format!("{law}.steps")))
        .collect();
    for f in &files {
        let script = fs::read_to_string(f).unwrap();
        assert!(
            script.starts_with("; proof by Rules.Stack.openByLength sha256:"),
            "{}",
            &script[..script.len().min(200)]
        );
    }
    let mut args = vec![
        "run".to_string(),
        "main.av".to_string(),
        "--module-root".to_string(),
        ".".to_string(),
        "--".to_string(),
    ];
    args.extend(files.iter().map(|f| f.to_string_lossy().to_string()));
    let refs: Vec<&str> = args.iter().map(String::as_str).collect();
    let replay = aver_in(&repo_root().join("tools/proof-kernel"), &refs, &[]);
    assert!(replay.status.success(), "{}", format_output(&replay));
    for law in SHUFFLE_LAWS {
        assert!(
            String::from_utf8_lossy(&replay.stdout).contains(&format!("accepted {law}")),
            "{}",
            format_output(&replay)
        );
    }
    let _ = fs::remove_dir_all(out);

    // The same laws without their `by` lines: the automatic steps stop.
    let bare = scratch("bare");
    fs::create_dir_all(&bare).unwrap();
    let source = fs::read_to_string(dir.join("shuffles.av")).unwrap();
    let without: String = source
        .lines()
        .filter(|l| !l.trim_start().starts_with("by "))
        .map(|l| format!("{l}\n"))
        .collect();
    fs::write(bare.join("shuffles.av"), without).unwrap();
    let open = aver_in(
        &bare,
        &[
            "proof",
            "shuffles.av",
            "--backend",
            "aver",
            "-o",
            bare.join("out").to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "10",
        ],
        &[],
    );
    let found = summary(&open);
    for law in SHUFFLE_LAWS {
        assert_eq!(found["closed_by"][law], "open", "{law}");
    }
    let _ = fs::remove_dir_all(bare);
}

#[test]
fn a_rule_that_refuses_errs_or_never_returns_leaves_the_law_open() {
    let out = scratch("wrong");
    let result = aver_in(
        &fixtures(),
        &[
            "proof",
            "wrong.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
        // A low step limit, so the rule that never returns stops quickly.
        &[("AVER_RULE_STEP_LIMIT", "2000000")],
    );
    // An open law is not a kernel refusal of a producer's script: the run
    // itself succeeds.
    assert!(result.status.success(), "{}", format_output(&result));
    let text = String::from_utf8_lossy(&result.stdout);
    let line = |law: &str| {
        text.lines()
            .find(|l| l.contains(&format!("{law}:")))
            .unwrap_or_else(|| panic!("no line for {law}\n{text}"))
            .to_string()
    };
    // A false law: the rule refuses, in its own words.
    assert!(
        line("swapped.swapOnceIsTheTopPair").contains(
            "not closed by this backend (steps: rule Rules.Stack.openByLength refused: the two sides evaluate to different terms)"
        ),
        "{text}"
    );
    // A rule whose proof proves something else: the kernel refuses it.
    let wrong = line("swapped.swapIsTheSwap");
    assert!(
        wrong.contains("rule Rules.Broken.claimsTheWhen (")
            && wrong.contains("the kernel refused its proof"),
        "{wrong}"
    );
    // A rule that never returns: not checked, never accepted.
    assert!(
        line("swapped.swapAgain").contains(
            "rule Rules.Broken.spins: not checked: the rule ran out of its 2000000 steps"
        ),
        "{text}"
    );
    assert!(
        text.contains("0 of 6 law(s) closed by steps"),
        "{}",
        format_output(&result)
    );
    let _ = fs::remove_dir_all(out);
}

/// Write `files` into a fresh project directory and run `aver check` on
/// `entry` there.
fn check_project(name: &str, files: &[(&str, &str)], entry: &str) -> Output {
    let dir = scratch(name);
    for (path, text) in files {
        let at = dir.join(path);
        fs::create_dir_all(at.parent().unwrap()).unwrap();
        fs::write(at, text).unwrap();
    }
    let out = aver_in(&dir, &["check", entry], &[]);
    let _ = fs::remove_dir_all(dir);
    out
}

const GOOD_RULE: &str = "module Same\n    intent = \"A rule.\"\n    depends [Kernel.Term, Kernel.Proof, Kernel.Lib]\n    rules [same]\n\nfn same(goal: Goal) -> Result<Proof, String>\n    ? \"Refuses.\"\n    Result.Err(\"no\")\n\nverify same\n    same(Goal(obligation = Law(key = \"k\", givens = [], premise = [], lhs = Term.TInt(1), rhs = Term.TInt(1)), finite = [], lists = [], ints = [], defs = [], consts = [], sums = [], laws = [], facts = [])) => Result.Err(\"no\")\n";

#[test]
fn a_rules_module_has_the_rule_signature_and_no_effects() {
    let ok = check_project("good", &[("same.av", GOOD_RULE)], "same.av");
    assert!(ok.status.success(), "{}", format_output(&ok));
    // The qualified spelling of the kernel types is the same signature.
    let qualified = GOOD_RULE.replace(
        "fn same(goal: Goal) -> Result<Proof, String>",
        "fn same(goal: Kernel.Proof.Goal) -> Result<Kernel.Proof.Proof, String>",
    );
    let ok = check_project("qualified", &[("same.av", &qualified)], "same.av");
    assert!(ok.status.success(), "{}", format_output(&ok));

    let wrong_signature = GOOD_RULE
        .replace(
            "fn same(goal: Goal) -> Result<Proof, String>",
            "fn same(goal: Goal, extra: Int) -> Result<Proof, String>",
        )
        .replace("facts = [])) =>", "facts = []), 1) =>");
    let out = check_project("signature", &[("same.av", &wrong_signature)], "same.av");
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("rule 'same' must have the rule signature"),
        "{}",
        format_output(&out)
    );

    let missing = GOOD_RULE.replace("rules [same]", "rules [same, other]");
    let out = check_project("missing", &[("same.av", &missing)], "same.av");
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("lists rule 'other'"),
        "{}",
        format_output(&out)
    );

    let effect_line = GOOD_RULE.replace(
        "    rules [same]\n",
        "    rules [same]\n    effects [Console.print]\n",
    );
    let out = check_project("effects-line", &[("same.av", &effect_line)], "same.av");
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("a proof rule is pure"),
        "{}",
        format_output(&out)
    );

    let effect_use = GOOD_RULE.replace(
        "    ? \"Refuses.\"\n    Result.Err(\"no\")\n",
        "    ? \"Refuses.\"\n    ! [Console.print]\n    Console.print(\"x\")\n    Result.Err(\"no\")\n",
    );
    let out = check_project("effects-use", &[("same.av", &effect_use)], "same.av");
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("uses effect 'Console.print'"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn a_program_module_may_not_depend_on_a_rules_module() {
    let program = "module App\n    intent = \"A program.\"\n    depends [Same]\n    effects []\n\nfn one() -> Int\n    ? \"One.\"\n    1\n\nverify one\n    one() => 1\n";
    let out = check_project(
        "depends",
        &[("same.av", GOOD_RULE), ("app.av", program)],
        "app.av",
    );
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout)
            .contains("module 'App' depends on rules module 'Same'"),
        "{}",
        format_output(&out)
    );
}

/// Run `aver check laws.av` in a project with the rules module
/// `rules/same.av` (rule `same`), the law's extra lines being `lines`.
fn check_law(name: &str, lines: &str) -> Output {
    let law = format!(
        "module Laws\n    intent = \"Laws.\"\n    effects []\n\nfn f(x: Int) -> Int\n    ? \"Itself.\"\n    x\n\nverify f\n    f(1) => 1\n\nverify f law same\n    given x: Int = [1, 2]\n{lines}    f(x) => x\n"
    );
    let plain = "module Plain\n    intent = \"Not rules.\"\n    effects []\n\nfn one() -> Int\n    ? \"One.\"\n    1\n\nverify one\n    one() => 1\n";
    check_project(
        name,
        &[
            ("laws.av", &law),
            ("rules/same.av", GOOD_RULE),
            ("plain.av", plain),
        ],
        "laws.av",
    )
}

fn output_text(out: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

#[test]
fn a_by_line_names_one_rule_of_the_project_and_stands_alone() {
    let ok = check_law("one-by", "    by Rules.Same.same\n");
    assert!(ok.status.success(), "{}", format_output(&ok));
    for (name, lines, message) in [
        (
            "two-by",
            "    by Rules.Same.same\n    by Rules.Same.other\n",
            "A law may have only one 'by' line",
        ),
        (
            "by-induction",
            "    by Rules.Same.same\n    induction x\n",
            "it cannot also name an 'induction'",
        ),
        (
            "induction-by",
            "    induction x\n    by Rules.Same.same\n",
            "it cannot also name an 'induction'",
        ),
        (
            "by-because",
            "    because x >= 1\n    by Rules.Same.same\n",
            "it cannot also have 'because' lines",
        ),
        (
            "because-after-by",
            "    by Rules.Same.same\n    because x >= 1\n",
            "it cannot also have 'because' lines",
        ),
        (
            "no-module",
            "    by Rules.Missing.same\n",
            "this project has no module Rules.Missing",
        ),
        (
            "no-rule",
            "    by Rules.Same.other\n",
            "rules module Rules.Same does not list `other`",
        ),
        (
            "not-rules",
            "    by Plain.one\n",
            "module Plain is not a rules module",
        ),
        (
            "kernel",
            "    by Kernel.Lib.evaluate\n",
            "Kernel.Lib is a module of the proof kernel",
        ),
    ] {
        let out = check_law(name, lines);
        assert!(!out.status.success(), "{name}: {}", format_output(&out));
        let text = output_text(&out);
        assert!(text.contains(message), "{name}: {text}");
    }
}

#[test]
fn kernel_module_names_are_reserved_for_the_kernel() {
    // A project file named like a kernel module is an error, never the
    // module a dependency gets.
    let fake = "module Lib\n    intent = \"Not the kernel's.\"\n    effects []\n\nfn one() -> Int\n    ? \"One.\"\n    1\n\nverify one\n    one() => 1\n";
    let out = check_project(
        "reserved",
        &[("same.av", GOOD_RULE), ("kernel/lib.av", fake)],
        "same.av",
    );
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        output_text(&out).contains("'Kernel.Lib' is a module name reserved for the proof kernel"),
        "{}",
        format_output(&out)
    );
    // Only a rules module depends on the kernel.
    let program = "module App\n    intent = \"A program.\"\n    depends [Kernel.Lib]\n    effects []\n\nfn one() -> Int\n    ? \"One.\"\n    1\n\nverify one\n    one() => 1\n";
    let out = check_project("program-kernel", &[("app.av", program)], "app.av");
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        output_text(&out).contains(
            "module 'App' depends on 'Kernel.Lib': only a rules module may depend on the proof kernel's modules"
        ),
        "{}",
        format_output(&out)
    );
    // And only on its public modules.
    let private = GOOD_RULE.replace(
        "depends [Kernel.Term, Kernel.Proof, Kernel.Lib]",
        "depends [Kernel.Term, Kernel.Proof, Kernel.Lib, Kernel.Subst]",
    );
    let out = check_project("rules-private", &[("same.av", &private)], "same.av");
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        output_text(&out).contains("rules module 'Same' depends on 'Kernel.Subst'"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn the_helper_library_keeps_the_kernels_conventions() {
    let kernel = repo_root().join("tools/proof-kernel");
    for file in ["lib/lib.av", "lib/wire.av", "lib/conventions.av"] {
        let out = aver_in(&kernel, &["verify", file, "--module-root", "."], &[]);
        assert!(out.status.success(), "{file}: {}", format_output(&out));
        assert!(
            String::from_utf8_lossy(&out.stdout).contains(" 0 failed"),
            "{file}: {}",
            format_output(&out)
        );
    }
    let rules = aver_in(&fixtures(), &["verify", "rules/stack.av"], &[]);
    assert!(rules.status.success(), "{}", format_output(&rules));
}

#[test]
fn lean_closes_the_rule_laws_by_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("lean");
    let result = aver_in(
        &fixtures(),
        &[
            "proof",
            "shuffles.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
        &[],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let found = summary(&result);
    assert_eq!(found["steps_rejected"], serde_json::json!([]));
    for law in SHUFFLE_LAWS {
        assert_eq!(found["closed_by"][law], "steps", "{law}");
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_leaves_a_law_open_when_its_rule_does_not_close_it() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("lean-open");
    let result = aver_in(
        &fixtures(),
        &[
            "proof",
            "wrong.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "10",
            "--declined-budget",
            "10",
        ],
        &[("AVER_RULE_STEP_LIMIT", "2000000")],
    );
    let found = summary(&result);
    // Four of these laws are true, and Lean's tactics would close them;
    // a law that names its rule is closed by that rule or not at all.
    assert_eq!(found["universal_laws"], 0, "{found}");
    // A law without a `when` that did not close is open the way any such
    // law is: a sorry, not a declined attempt.
    assert_eq!(
        found["sorry_laws"],
        serde_json::json!(["swapped.swapAgainUsingNothing"]),
        "{found}"
    );
    let declined = found["declined_claims"]
        .as_array()
        .unwrap_or_else(|| panic!("{found}"));
    // Each also with `using []`, which would otherwise take the reason
    // ladder of a guided law (the never-returning one without a `when`,
    // checked above).
    for law in [
        "swapped.swapAgain",
        "swapped.swapIsTheSwap",
        "swapped.swapOnceIsTheTopPair",
        "swapped.swapIsTheSwapUsingNothing",
        "swapped.swapOnceUsingNothing",
    ] {
        let entry = declined
            .iter()
            .find(|d| d["claim"] == law)
            .unwrap_or_else(|| panic!("{law} not declined: {found}"));
        assert!(
            entry["reason"]
                .as_str()
                .is_some_and(|r| r.starts_with("rule ")),
            "{law}: {entry}"
        );
    }
    let _ = fs::remove_dir_all(out);
}
