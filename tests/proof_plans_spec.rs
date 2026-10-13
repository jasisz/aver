//! Project proof plans: a `plans [...]` module and a law's `by Module.plan`
//! line. The plan writes the law's proof as steps; the kernel written in Aver
//! checks them like any other, and Lean checks them again. A plan that
//! refuses, a plan whose proof the kernel refuses and a plan that never
//! returns all leave the law open, with the reason.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const FIXTURES: &str = "tests/fixtures/proof_plans";

/// The laws of `shuffles.av`, each closed by `Plans.Stack.openByLength`.
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
    let dir = std::env::temp_dir().join(format!("aver-plans-{name}-{}", std::process::id()));
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
fn a_plan_closes_laws_the_automatic_steps_leave_open() {
    let dir = fixtures();
    let out = scratch("closes");
    let by_plan = aver_in(
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
    assert!(by_plan.status.success(), "{}", format_output(&by_plan));
    let text = String::from_utf8_lossy(&by_plan.stdout);
    for law in SHUFFLE_LAWS {
        let line = text
            .lines()
            .find(|l| l.contains(&format!("{law}:")))
            .unwrap_or_else(|| panic!("no line for {law}\n{text}"));
        assert!(
            line.contains("closed by steps (proof by Plans.Stack.openByLength ("),
            "{line}"
        );
        assert!(line.ends_with(" steps)"), "{line}");
    }
    // Every script records the plan and its source hash, and the kernel
    // run on the VM accepts the scripts as written to disk.
    let files: Vec<PathBuf> = SHUFFLE_LAWS
        .iter()
        .map(|law| out.join("proof_steps").join(format!("{law}.steps")))
        .collect();
    for f in &files {
        let script = fs::read_to_string(f).unwrap();
        assert!(
            script.starts_with("; proof by Plans.Stack.openByLength sha256:"),
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
fn a_plan_that_refuses_errs_or_never_returns_leaves_the_law_open() {
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
        // A low step limit, so the plan that never returns stops quickly.
        &[("AVER_PLAN_STEP_LIMIT", "2000000")],
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
    // A false law: the plan refuses, in its own words.
    assert!(
        line("swapped.swapOnceIsTheTopPair").contains(
            "not closed by this backend (steps: plan Plans.Stack.openByLength refused: the two sides evaluate to different terms)"
        ),
        "{text}"
    );
    // A plan whose proof proves something else: the kernel refuses it.
    let wrong = line("swapped.swapIsTheSwap");
    assert!(
        wrong.contains("plan Plans.Broken.claimsTheWhen (")
            && wrong.contains("the kernel refused its proof"),
        "{wrong}"
    );
    // A plan that never returns: not checked, never accepted.
    assert!(
        line("swapped.swapAgain").contains(
            "plan Plans.Broken.spins: not checked: the plan ran out of its 2000000 steps"
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

/// The hint `aver proof` gives for an open law a plan of the project closes.
const STACK_HINT: &str =
    "plan Plans.Stack.openByLength closes this law; add `by Plans.Stack.openByLength`";

#[test]
fn an_open_law_hints_the_plan_that_closes_it_and_stays_open() {
    let out = scratch("hinted");
    let hinted = aver_in(
        &fixtures(),
        &[
            "proof",
            "hinted.av",
            "--backend",
            "aver",
            "-o",
            out.join("text").to_str().unwrap(),
        ],
        &[],
    );
    assert!(hinted.status.success(), "{}", format_output(&hinted));
    let text = String::from_utf8_lossy(&hinted.stdout);
    let lines: Vec<&str> = text.lines().collect();
    let at = |law: &str| {
        lines
            .iter()
            .position(|l| l.contains(&format!("{law}:")))
            .unwrap_or_else(|| panic!("no line for {law}\n{text}"))
    };
    // The law that names the plan is closed by it.
    let named = at("shuffled.swapTwiceNamesItsPlan");
    assert!(
        lines[named].contains("closed by steps (proof by Plans.Stack.openByLength ("),
        "{text}"
    );
    // The plan closes the law, but nothing is credited from a hint.
    let closes = at("shuffled.swapTwiceIsTheTopPair");
    assert!(
        lines[closes].contains("not closed by this backend"),
        "{text}"
    );
    assert_eq!(
        lines[closes + 1],
        format!("    hint: {STACK_HINT}"),
        "{text}"
    );
    // No plan closes the other law: the Stack plan refuses its `when`, and
    // Plans.Broken, which no `by` line names, is never tried.
    let none = at("shuffled.swapTwiceIsTheTopPairPastOne");
    assert!(lines[none].contains("not closed by this backend"), "{text}");
    assert!(
        !lines[none..].iter().any(|l| l.contains("hint: plan")),
        "{text}"
    );
    assert!(text.contains("1 of 3 law(s) closed by steps"), "{text}");

    let json = aver_in(
        &fixtures(),
        &[
            "proof",
            "hinted.av",
            "--backend",
            "aver",
            "-o",
            out.join("json").to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "2",
        ],
        &[],
    );
    let found = summary(&json);
    assert_eq!(found["closed_by"]["shuffled.swapTwiceIsTheTopPair"], "open");
    assert_eq!(
        found["steps_hints"]["shuffled.swapTwiceIsTheTopPair"][0],
        STACK_HINT
    );
    let other = &found["steps_hints"]["shuffled.swapTwiceIsTheTopPairPastOne"];
    assert!(
        !other.to_string().contains("plan "),
        "{}",
        format_output(&json)
    );
    let _ = fs::remove_dir_all(out);

    // No `by` line, no plans module: no plan is tried and none is named.
    let bare = scratch("hinted-bare");
    fs::create_dir_all(&bare).unwrap();
    let source = fs::read_to_string(fixtures().join("hinted.av")).unwrap();
    let without: String = source
        .lines()
        .filter(|l| !l.trim_start().starts_with("by "))
        .map(|l| format!("{l}\n"))
        .collect();
    fs::write(bare.join("hinted.av"), without).unwrap();
    let alone = aver_in(
        &bare,
        &[
            "proof",
            "hinted.av",
            "--backend",
            "aver",
            "-o",
            bare.join("out").to_str().unwrap(),
        ],
        &[],
    );
    assert!(alone.status.success(), "{}", format_output(&alone));
    assert!(
        !String::from_utf8_lossy(&alone.stdout).contains("hint: plan"),
        "{}",
        format_output(&alone)
    );
    let _ = fs::remove_dir_all(bare);
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

const GOOD_PLAN: &str = "module Same\n    intent = \"A plan.\"\n    depends [Kernel.Term, Kernel.Proof, Kernel.Lib]\n    plans [same]\n\nfn same(goal: Goal) -> Result<Proof, String>\n    ? \"Refuses.\"\n    Result.Err(\"no\")\n\nverify same\n    same(Kernel.Lib.goalFor(Law(key = \"k\", givens = [], premise = [], lhs = Term.TInt(1), rhs = Term.TInt(1)))) => Result.Err(\"no\")\n";

#[test]
fn a_plans_module_has_the_plan_signature_and_no_effects() {
    let ok = check_project("good", &[("same.av", GOOD_PLAN)], "same.av");
    assert!(ok.status.success(), "{}", format_output(&ok));
    // The qualified spelling of the kernel types is the same signature.
    let qualified = GOOD_PLAN.replace(
        "fn same(goal: Goal) -> Result<Proof, String>",
        "fn same(goal: Kernel.Proof.Goal) -> Result<Kernel.Proof.Proof, String>",
    );
    let ok = check_project("qualified", &[("same.av", &qualified)], "same.av");
    assert!(ok.status.success(), "{}", format_output(&ok));
    // A plan may return a script for the standard library to run instead.
    let scripted = GOOD_PLAN
        .replace("Kernel.Lib]", "Kernel.Lib, Kernel.Std]")
        .replace(
            "fn same(goal: Goal) -> Result<Proof, String>",
            "fn same(goal: Goal) -> Result<Tactic, String>",
        );
    let ok = check_project("scripted", &[("same.av", &scripted)], "same.av");
    assert!(ok.status.success(), "{}", format_output(&ok));

    let wrong_signature = GOOD_PLAN
        .replace(
            "fn same(goal: Goal) -> Result<Proof, String>",
            "fn same(goal: Goal, extra: Int) -> Result<Proof, String>",
        )
        .replace("facts = [])) =>", "facts = []), 1) =>");
    let out = check_project("signature", &[("same.av", &wrong_signature)], "same.av");
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("plan 'same' must have the plan signature"),
        "{}",
        format_output(&out)
    );

    let missing = GOOD_PLAN.replace("plans [same]", "plans [same, other]");
    let out = check_project("missing", &[("same.av", &missing)], "same.av");
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("lists plan 'other'"),
        "{}",
        format_output(&out)
    );

    let effect_line = GOOD_PLAN.replace(
        "    plans [same]\n",
        "    plans [same]\n    effects [Console.print]\n",
    );
    let out = check_project("effects-line", &[("same.av", &effect_line)], "same.av");
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("a proof plan is pure"),
        "{}",
        format_output(&out)
    );

    let effect_use = GOOD_PLAN.replace(
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
fn a_program_module_may_not_depend_on_a_plans_module() {
    let program = "module App\n    intent = \"A program.\"\n    depends [Same]\n    effects []\n\nfn one() -> Int\n    ? \"One.\"\n    1\n\nverify one\n    one() => 1\n";
    let out = check_project(
        "depends",
        &[("same.av", GOOD_PLAN), ("app.av", program)],
        "app.av",
    );
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout)
            .contains("module 'App' depends on plans module 'Same'"),
        "{}",
        format_output(&out)
    );
}

/// Run `aver check laws.av` in a project with the plans module
/// `plans/same.av` (plan `same`), the law's extra lines being `lines`.
fn check_law(name: &str, lines: &str) -> Output {
    let law = format!(
        "module Laws\n    intent = \"Laws.\"\n    effects []\n\nfn f(x: Int) -> Int\n    ? \"Itself.\"\n    x\n\nverify f\n    f(1) => 1\n\nverify f law same\n    given x: Int = [1, 2]\n{lines}    f(x) => x\n"
    );
    let plain = "module Plain\n    intent = \"Not plans.\"\n    effects []\n\nfn one() -> Int\n    ? \"One.\"\n    1\n\nverify one\n    one() => 1\n";
    check_project(
        name,
        &[
            ("laws.av", &law),
            ("plans/same.av", GOOD_PLAN),
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
fn a_by_line_names_one_plan_of_the_project_and_stands_alone() {
    let ok = check_law("one-by", "    by Plans.Same.same\n");
    assert!(ok.status.success(), "{}", format_output(&ok));
    let std = check_law("std-by", "    by Kernel.Std.cases\n");
    assert!(std.status.success(), "{}", format_output(&std));
    for (name, lines, message) in [
        (
            "two-by",
            "    by Plans.Same.same\n    by Plans.Same.other\n",
            "A law may have only one 'by' line",
        ),
        (
            "by-induction",
            "    by Plans.Same.same\n    induction x\n",
            "it cannot also name an 'induction'",
        ),
        (
            "induction-by",
            "    induction x\n    by Plans.Same.same\n",
            "it cannot also name an 'induction'",
        ),
        (
            "by-because",
            "    because x >= 1\n    by Plans.Same.same\n",
            "it cannot also have 'because' lines",
        ),
        (
            "because-after-by",
            "    by Plans.Same.same\n    because x >= 1\n",
            "it cannot also have 'because' lines",
        ),
        (
            "no-module",
            "    by Plans.Missing.same\n",
            "this project has no module Plans.Missing",
        ),
        (
            "no-plan",
            "    by Plans.Same.other\n",
            "plans module Plans.Same does not list `other`",
        ),
        (
            "not-plans",
            "    by Plain.one\n",
            "module Plain is not a plans module",
        ),
        (
            "kernel",
            "    by Kernel.Lib.evaluate\n",
            "Kernel.Lib is a module of the proof kernel",
        ),
        (
            "std-no-plan",
            "    by Kernel.Std.nope\n",
            "Kernel.Std has no plan `nope`; its plans are cases",
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
        &[("same.av", GOOD_PLAN), ("kernel/lib.av", fake)],
        "same.av",
    );
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        output_text(&out).contains("'Kernel.Lib' is a module name reserved for the proof kernel"),
        "{}",
        format_output(&out)
    );
    // Only a plans module depends on the kernel.
    let program = "module App\n    intent = \"A program.\"\n    depends [Kernel.Lib]\n    effects []\n\nfn one() -> Int\n    ? \"One.\"\n    1\n\nverify one\n    one() => 1\n";
    let out = check_project("program-kernel", &[("app.av", program)], "app.av");
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        output_text(&out).contains(
            "module 'App' depends on 'Kernel.Lib': only a plans module may depend on the proof kernel's modules"
        ),
        "{}",
        format_output(&out)
    );
    // And only on its public modules.
    let private = GOOD_PLAN.replace(
        "depends [Kernel.Term, Kernel.Proof, Kernel.Lib]",
        "depends [Kernel.Term, Kernel.Proof, Kernel.Lib, Kernel.Subst]",
    );
    let out = check_project("plans-private", &[("same.av", &private)], "same.av");
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        output_text(&out).contains("plans module 'Same' depends on 'Kernel.Subst'"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn the_helper_library_keeps_the_kernels_conventions() {
    let kernel = repo_root().join("tools/proof-kernel");
    for file in [
        "lib/lib.av",
        "lib/wire.av",
        "lib/conventions.av",
        "lib/std.av",
        "embed_std.av",
    ] {
        let out = aver_in(&kernel, &["verify", file, "--module-root", "."], &[]);
        assert!(out.status.success(), "{file}: {}", format_output(&out));
        assert!(
            String::from_utf8_lossy(&out.stdout).contains(" 0 failed"),
            "{file}: {}",
            format_output(&out)
        );
    }
    for file in ["plans/stack.av", "plans/viastd.av", "plans/tactics.av"] {
        let plans = aver_in(&fixtures(), &["verify", file], &[]);
        assert!(plans.status.success(), "{file}: {}", format_output(&plans));
    }
}

/// `lights.av`: a law closed by the standard plan `Kernel.Std.cases`, one by
/// a project plan that calls it, and one it refuses.
#[test]
fn a_standard_plan_closes_a_law_like_a_project_plan() {
    let out = scratch("std");
    let run = aver_in(
        &fixtures(),
        &[
            "proof",
            "lights.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
        &[],
    );
    let text = String::from_utf8_lossy(&run.stdout).to_string();
    let line = |law: &str| {
        text.lines()
            .find(|l| l.contains(&format!("{law}:")))
            .unwrap_or_else(|| panic!("no line for {law}\n{}", format_output(&run)))
            .to_string()
    };
    assert!(
        line("next.comesBackAfterThree").contains("closed by steps (proof by Kernel.Std.cases ("),
        "{text}"
    );
    assert!(
        line("flip.twiceIsItself").contains("closed by steps (proof by Plans.ViaStd.cases ("),
        "{text}"
    );
    assert!(
        line("steps.nothingToSplit")
            .contains("plan Kernel.Std.cases refused: no given of finite type"),
        "{text}"
    );
    let script =
        fs::read_to_string(out.join("proof_steps/next.comesBackAfterThree.steps")).unwrap();
    assert!(
        script.starts_with("; proof by Kernel.Std.cases sha256:"),
        "{script}"
    );
    let _ = fs::remove_dir_all(out);
}

/// Nothing a standard plan writes is trusted: the plan compiled into the
/// binary answers a goal, and once one step of its proof is wrong the
/// kernel refuses the script, while it accepts the proof as written.
#[test]
fn the_kernel_refuses_a_wrong_proof_from_a_standard_plan() {
    let goal = "(steps 14\n (obligation next.comesBackAfterThree ((l (named Light))) (none) (call next (call next (call next (v l)))) (v l))\n (defs\n  (def next ((l (named Light))) (named Light) () (match (v l) (arm (pc Light.Red) (ctor Light.Green)) (arm (pc Light.Green) (ctor Light.Amber)) (arm (pc Light.Amber) (ctor Light.Red)))))\n (consts)\n (sums\n  (sum Light eq (Light.Red) (Light.Amber) (Light.Green)))\n (recs)\n (sigs)\n (laws)\n (proof (refl (v l))))";
    let answer = aver::proof_std::run("cases", goal).unwrap();
    let proof = answer
        .strip_prefix("proof\n")
        .unwrap_or_else(|| panic!("the plan did not write a proof: {answer}"));
    let script = goal.replace("(proof (refl (v l)))", &format!("(proof {proof})"));
    assert!(
        aver::proof_kernel::verdict(&script).is_ok(),
        "{:?}",
        aver::proof_kernel::verdict(&script)
    );
    // The first case opens `next` at the arm for Light.Red; claim the
    // second arm instead.
    let right = "(unfold next 1 ((ctor Light.Red))";
    assert!(script.contains(right), "{script}");
    let wrong = script.replacen(right, "(unfold next 2 ((ctor Light.Red))", 1);
    let refused = aver::proof_kernel::verdict(&wrong).expect_err("a wrong step must be refused");
    assert!(refused.starts_with("step proof/enum.0"), "{refused}");
    // A plan the library does not have refuses, it does not guess.
    let none = aver::proof_std::run("nope", goal).unwrap();
    assert_eq!(none, "refused\nKernel.Std has no plan nope");
}

/// The laws of `scripted.av` that plans written as scripts close: the
/// stack laws `shuffles.av` closes with a hand-written plan, and a byte check
/// over a concatenation, by induction with a split on each head.
const SCRIPTED_LAWS: [&str; 5] = [
    "shuffled.rotThreeTimesIsTheTopThree",
    "shuffled.swapTwiceIsTheTopPair",
    "shuffled.twoSwapTwiceIsTheTopFour",
    "validBytes.concatenation",
    "validBytes.concatenationByPlan",
];

/// `scripted.av`: plans that return a script (`Kernel.Std.Tactic`). The
/// standard library runs it, the automatic steps close its Auto nodes, a
/// Plan node runs another plan of the module, and the kernel checks the
/// whole proof: a script whose Plan node writes a wrong proof leaves its law
/// open, and a script that stops says where.
#[test]
fn a_plan_may_return_a_script_whose_auto_nodes_the_automatic_steps_close() {
    let out = scratch("scripted-run");
    let run = aver_in(
        &fixtures(),
        &[
            "proof",
            "scripted.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
        &[],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    let text = String::from_utf8_lossy(&run.stdout).to_string();
    let line = |law: &str| {
        text.lines()
            .find(|l| l.contains(&format!("{law}:")))
            .unwrap_or_else(|| panic!("no line for {law}\n{}", format_output(&run)))
            .to_string()
    };
    for law in SCRIPTED_LAWS {
        assert!(
            line(law).contains("closed by steps (proof by Plans.Tactics."),
            "{text}"
        );
    }
    // The script stops at an Auto node: the refusal gives its place in the
    // script, and the terms as Aver writes them.
    assert!(
        line("validBytes.reversal").contains(
            "plan Plans.Tactics.bytesByInduction refused: Induction(bytes)/cons/Auto: evaluation stops at `validBytes(List.concat(List.reverse(t), List.prepend(h, [])))`"
        ),
        "{text}"
    );
    // A Plan node whose plan proves something else: the kernel refuses.
    let claimed = line("validBytes.concatenationClaimed");
    assert!(
        claimed.contains("plan Plans.Tactics.splitThenClaim (")
            && claimed.contains("the kernel refused its proof"),
        "{claimed}"
    );
    assert!(text.contains("5 of 7 law(s) closed by steps"), "{text}");
    let script =
        fs::read_to_string(out.join("proof_steps/shuffled.swapTwiceIsTheTopPair.steps")).unwrap();
    assert!(
        script.starts_with("; proof by Plans.Tactics.openByLength sha256:"),
        "{}",
        &script[..script.len().min(200)]
    );
    let _ = fs::remove_dir_all(out);
}

/// One run of a script by the compiled standard library: it asks for each
/// Auto node it has no answer for, takes a closed node as the hypothesis
/// `#N` the compiler fills in, and says where it stops.
#[test]
fn the_standard_library_runs_a_script_by_asking_for_its_auto_nodes() {
    let goal = "(steps 14 (obligation k ((xs (list int))) (none) (v xs) (v xs)) (defs) (consts) (sums) (recs) (sigs) (laws) (proof (refl (v xs))))";
    let script = "(split xs h t (auto) (auto))";
    let first = aver::proof_std::run_script(goal, script, "(answers)").unwrap();
    assert_eq!(
        first,
        "ask\nauto\n(sub () (from) (hyps) (none) (list) (list))"
    );
    let answers = "(answers (a \"auto\\n(sub () (from) (hyps) (none) (list) (list))\" (closed 0)))";
    let second = aver::proof_std::run_script(goal, script, answers).unwrap();
    assert_eq!(
        second,
        "ask\nauto\n(sub ((h int) (t (list int))) (from (h head xs) (t tail xs)) (hyps) (none) (bi List.prepend (v h) (v t)) (bi List.prepend (v h) (v t)))"
    );
    let refused =
        aver::proof_std::run_script(goal, "(split xs xs t (auto) (auto))", "(answers)").unwrap();
    assert_eq!(refused, "refused\nSplit(xs): the name xs is already taken");
}

#[test]
fn lean_closes_the_plan_laws_by_steps() {
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
    let _ = fs::remove_dir_all(&out);
    // The standard plan's proof, and the project plan's that calls it.
    let result = aver_in(
        &fixtures(),
        &[
            "proof",
            "lights.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "1",
        ],
        &[],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let found = summary(&result);
    assert_eq!(found["steps_rejected"], serde_json::json!([]));
    for law in ["next.comesBackAfterThree", "flip.twiceIsItself"] {
        assert_eq!(found["closed_by"][law], "steps", "{law}");
    }
    let _ = fs::remove_dir_all(&out);
    // The laws closed by scripts, their Auto nodes by the automatic steps.
    let result = aver_in(
        &fixtures(),
        &[
            "proof",
            "scripted.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "2",
        ],
        &[],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let found = summary(&result);
    assert_eq!(found["steps_rejected"], serde_json::json!([]));
    for law in SCRIPTED_LAWS {
        assert_eq!(found["closed_by"][law], "steps", "{law}");
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_leaves_a_law_open_when_its_plan_does_not_close_it() {
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
        &[("AVER_PLAN_STEP_LIMIT", "2000000")],
    );
    let found = summary(&result);
    // Four of these laws are true, and Lean's tactics would close them;
    // a law that names its plan is closed by that plan or not at all.
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
                .is_some_and(|r| r.starts_with("plan ")),
            "{law}: {entry}"
        );
    }
    let _ = fs::remove_dir_all(out);
}
