//! Proofs as data: the step scripts proof lowering writes are replayed by
//! the Aver replayer in `tools/proof-kernel` (on the VM) and checked by the
//! Lean kernel, and a mutated script is refused by both.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const FIXTURES: &str = "tests/fixtures/proof_steps";
const KERNEL: &str = "tools/proof-kernel";

fn aver_in(dir: &Path, args: &[&str]) -> Output {
    Command::new(aver_bin())
        .args(args)
        .current_dir(dir)
        .output()
        .expect("aver runs")
}

fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-steps-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    dir
}

/// Export a fixture's proof project (statements only, so no Lean runs) and
/// return the step files it wrote, by law.
fn export_steps(fixture: &str, out: &Path) -> Vec<(String, PathBuf)> {
    let dir = repo_root().join(FIXTURES);
    let result = aver_in(
        &dir,
        &[
            "proof",
            fixture,
            "-o",
            out.to_str().unwrap(),
            "--verify-mode",
            "sorry",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let mut files: Vec<(String, PathBuf)> = fs::read_dir(out.join("proof_steps"))
        .unwrap_or_else(|e| panic!("no proof_steps in {}: {e}", out.display()))
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "steps"))
        .map(|p| {
            let law = p.file_stem().unwrap().to_string_lossy().to_string();
            (law, p)
        })
        .collect();
    files.sort();
    files
}

fn replay(files: &[PathBuf]) -> Output {
    let kernel = repo_root().join(KERNEL);
    let mut args = vec![
        "run".to_string(),
        "main.av".to_string(),
        "--module-root".to_string(),
        ".".to_string(),
        "--".to_string(),
    ];
    args.extend(files.iter().map(|f| f.to_string_lossy().to_string()));
    let refs: Vec<&str> = args.iter().map(String::as_str).collect();
    aver_in(&kernel, &refs)
}

#[test]
fn the_replayer_passes_its_own_verify_blocks_and_laws() {
    let out = aver_in(
        &repo_root().join(KERNEL),
        &["verify", "main.av", "--module-root", "."],
    );
    assert!(out.status.success(), "{}", format_output(&out));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(text.contains("0 failed"), "{}", format_output(&out));
}

#[test]
fn the_producers_write_steps_for_the_shapes_they_know() {
    let out = scratch("shapes");
    let lock: Vec<String> = export_steps("lock.av", &out)
        .into_iter()
        .map(|(law, _)| law)
        .collect();
    assert_eq!(
        lock,
        [
            "add.associates",
            "add.commutes",
            "add.zeroIsIdentity",
            "lockTimeChecked.agreesWithSpec",
            "mul.oneIsIdentity",
            "pick.positiveIsOne",
            "sequenceChecked.agreesWithSpec",
        ]
    );
    let bytes_out = scratch("shapes-bytes");
    let bytes: Vec<String> = export_steps("bytes.av", &bytes_out)
        .into_iter()
        .map(|(law, _)| law)
        .collect();
    assert_eq!(bytes, ["decode.eightReadBack"]);
    let lets_out = scratch("shapes-lets");
    let lets: Vec<String> = export_steps("lets.av", &lets_out)
        .into_iter()
        .map(|(law, _)| law)
        .collect();
    assert_eq!(
        lets,
        [
            "bumped.positiveStays",
            "score.nothingClearedScoresNothing",
            "sumAndDouble.isTwiceTheSum",
        ]
    );
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(bytes_out);
    let _ = fs::remove_dir_all(lets_out);
}

#[test]
fn the_replayer_accepts_every_emitted_proof() {
    let out = scratch("accept");
    let mut files: Vec<PathBuf> = export_steps("lock.av", &out)
        .into_iter()
        .map(|(_, p)| p)
        .collect();
    let bytes_out = scratch("accept-bytes");
    files.extend(
        export_steps("bytes.av", &bytes_out)
            .into_iter()
            .map(|(_, p)| p),
    );
    let lets_out = scratch("accept-lets");
    files.extend(
        export_steps("lets.av", &lets_out)
            .into_iter()
            .map(|(_, p)| p),
    );
    let result = replay(&files);
    assert!(result.status.success(), "{}", format_output(&result));
    let text = String::from_utf8_lossy(&result.stdout);
    assert_eq!(
        text.lines().filter(|l| l.starts_with("accepted ")).count(),
        files.len(),
        "{}",
        format_output(&result)
    );
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(bytes_out);
    let _ = fs::remove_dir_all(lets_out);
}

/// Where the parenthesised form opening at `open` closes.
fn closing(text: &str, open: usize) -> usize {
    let mut depth = 0usize;
    let mut quoted = false;
    for (i, c) in text[open..].char_indices() {
        match c {
            '"' => quoted = !quoted,
            '(' if !quoted => depth += 1,
            ')' if !quoted => {
                depth -= 1;
                if depth == 0 {
                    return open + i;
                }
            }
            _ => {}
        }
    }
    panic!("unbalanced form at {open}")
}

/// The direct sub-forms of the form opening at `open`, as byte ranges.
fn sub_forms(text: &str, open: usize) -> Vec<(usize, usize)> {
    let end = closing(text, open);
    let mut out = Vec::new();
    let mut i = open + 1;
    while i < end {
        if text[i..].starts_with('(') {
            let j = closing(text, i);
            out.push((i, j + 1));
            i = j + 1;
        } else {
            i += 1;
        }
    }
    out
}

/// The script with the cases of its first `(enum …)` rearranged: `keep`
/// lists, by index, the cases that remain and their order.
fn recase(text: &str, keep: &[usize]) -> String {
    let open = text.find("(enum ").expect("an enum step");
    let forms = sub_forms(text, open);
    // (enum VAR LHS RHS CASE…): the first two sub-forms are the claim.
    let cases = &forms[2..];
    let rebuilt: Vec<&str> = keep
        .iter()
        .map(|&k| &text[cases[k].0..cases[k].1])
        .collect();
    let (from, to) = (cases[0].0, cases[cases.len() - 1].1);
    format!("{}{}{}", &text[..from], rebuilt.join(" "), &text[to..])
}

/// Replace the first occurrence of `from` after the proof starts.
fn mutate_proof(text: &str, from: &str, to: &str) -> String {
    let start = text.find("(proof ").expect("a script has a proof");
    let at = start
        + text[start..]
            .find(from)
            .unwrap_or_else(|| panic!("`{from}` does not occur in the proof"));
    format!("{}{}{}", &text[..at], to, &text[at + from.len()..])
}

#[test]
fn the_replayer_refuses_mutated_proofs_and_names_the_step() {
    let out = scratch("mutate");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("lock.av", &out).into_iter().collect();
    let lock = fs::read_to_string(&files["lockTimeChecked.agreesWithSpec"]).unwrap();
    let comm = fs::read_to_string(&files["add.commutes"]).unwrap();
    let mutants = [
        (
            "wrong premise",
            mutate_proof(&lock, "(hyp h_steps2)", "(hyp h_steps1)"),
        ),
        (
            "wrong literal",
            mutate_proof(&lock, "(i 500000000)", "(i 500000001)"),
        ),
        (
            "wrong substitution",
            mutate_proof(
                &comm,
                "(rule int.add_comm ((a (v a)) (b (v b))))",
                "(rule int.add_comm ((a (v b)) (b (v a))))",
            ),
        ),
        (
            "wrong arm number",
            mutate_proof(&lock, "(unfold continued 2", "(unfold continued 1"),
        ),
        (
            "hypothesis out of scope",
            mutate_proof(&lock, "(hyp h_steps3)", "(hyp h_steps7)"),
        ),
        (
            "unknown rule",
            mutate_proof(&lock, "(rule bool.and.true_l", "(rule bool.and.simp"),
        ),
    ];
    for (kind, text) in mutants {
        let path = out.join("mutant.steps");
        fs::write(&path, &text).unwrap();
        let result = replay(std::slice::from_ref(&path));
        let stdout = String::from_utf8_lossy(&result.stdout);
        assert!(
            !result.status.success(),
            "{kind}: {}",
            format_output(&result)
        );
        assert!(
            stdout.contains("refused ") && stdout.contains(": step proof"),
            "{kind}: the refusal must name the step\n{}",
            format_output(&result)
        );
    }
    let _ = fs::remove_dir_all(out);
}

/// The step term of one theorem, as rendered after its `first`.
fn exact_line<'a>(lean: &'a str, theorem: &str) -> &'a str {
    let start = lean
        .find(&format!("theorem {theorem} :"))
        .unwrap_or_else(|| panic!("no theorem {theorem}"));
    lean[start..]
        .lines()
        .find(|l| l.contains("exact (show"))
        .expect("the steps branch")
}

fn lean_refuses(dir: &Path, lean_file: &str, lean: &str, law: &str) -> bool {
    fs::write(dir.join(lean_file), lean).unwrap();
    let out = Command::new("lake")
        .args(["env", "lean", lean_file])
        .current_dir(dir)
        .output()
        .expect("lake runs");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    text.contains(&format!("AVER_STEPS_REJECTED:{law}"))
}

#[test]
fn lean_accepts_the_step_terms_and_refuses_mutated_ones() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("lean");
    let dir = repo_root().join(FIXTURES);
    let result = aver_in(
        &dir,
        &[
            "proof",
            "lock.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("Lock.lean")).unwrap();
    let theorem = "lockTimeChecked_law_agreesWithSpec";
    let law = "lockTimeChecked.agreesWithSpec";
    assert!(
        !lean_refuses(&out, "Lock.lean", &lean, law),
        "the emitted step term must close the law"
    );
    let line = exact_line(&lean, theorem).to_string();
    // The two arms of `continued`, to send a step to the wrong one.
    let arm_lemma = |ctor: &str| {
        line.split("have ")
            .skip(1)
            .find(|clause| clause.contains(&format!("(Step.{ctor} y0))")))
            .and_then(|clause| clause.split_whitespace().next())
            .unwrap_or_else(|| panic!("no unfold lemma for {ctor}"))
            .to_string()
    };
    let (stop, cont) = (arm_lemma("stop"), arm_lemma("continue'"));
    // Mutate the term only, after the local unfold lemmas.
    let first_replaced = |from: &str, to: &str| {
        let at = line.find("exact (show").expect("the step term");
        let (lemmas, term) = line.split_at(at);
        assert!(term.contains(from), "`{from}` is not in the step term");
        format!("{lemmas}{}", term.replacen(from, to, 1))
    };
    let mutants = [
        (
            "wrong premise",
            first_replaced("from h_steps2)", "from h_steps1)"),
        ),
        ("wrong literal", first_replaced("500000000", "500000001")),
        (
            "wrong arm number",
            first_replaced(&format!("{stop} "), &format!("{cont} ")),
        ),
        (
            "hypothesis out of scope",
            first_replaced("from h_steps3)", "from h_steps7)"),
        ),
    ];
    for (kind, mutated) in mutants {
        let text = lean.replacen(&line, &mutated, 1);
        assert!(
            lean_refuses(&out, "Lock.lean", &text, law),
            "{kind}: Lean must refuse the mutated step term"
        );
    }
    let comm = exact_line(&lean, "add_law_commutes").to_string();
    let swapped = comm.replacen(
        "AverSteps.add_comm (a) (b)",
        "AverSteps.add_comm (b) (a)",
        1,
    );
    assert_ne!(comm, swapped);
    assert!(
        lean_refuses(
            &out,
            "Lock.lean",
            &lean.replacen(&comm, &swapped, 1),
            "add.commutes"
        ),
        "wrong substitution: Lean must refuse the mutated step term"
    );
    let _ = fs::remove_dir_all(out);
}

fn aver_backend_json(
    fixture: &str,
    out: &Path,
    path_env: Option<&str>,
) -> (Output, serde_json::Value) {
    let mut command = Command::new(aver_bin());
    command
        .args([
            "proof",
            fixture,
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
        ])
        .current_dir(repo_root().join(FIXTURES));
    if let Some(path) = path_env {
        command.env("PATH", path);
    }
    let result = command.output().expect("aver runs");
    let line = String::from_utf8_lossy(&result.stdout)
        .lines()
        .rev()
        .find(|l| l.starts_with('{'))
        .map(str::to_string)
        .unwrap_or_else(|| panic!("{}", format_output(&result)));
    (result, serde_json::from_str(&line).unwrap())
}

fn closed_by_steps(summary: &serde_json::Value) -> Vec<String> {
    summary["closed_by"]
        .as_object()
        .unwrap()
        .iter()
        .filter(|(law, by)| by.as_str() == Some("steps") && !law.ends_with(".implication"))
        .map(|(law, _)| law.clone())
        .collect()
}

#[test]
fn the_aver_backend_closes_by_steps_without_lean_on_the_path() {
    let out = scratch("aver-backend");
    // An empty PATH: no `lean`, no `lake`; the kernel runs in process.
    let (result, summary) = aver_backend_json("lock.av", &out, Some(""));
    assert!(result.status.success(), "{}", format_output(&result));
    assert_eq!(summary["backend"], "aver");
    assert_eq!(summary["passed"], true);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    assert_eq!(
        closed_by_steps(&summary),
        [
            "add.associates",
            "add.commutes",
            "add.zeroIsIdentity",
            "lockTimeChecked.agreesWithSpec",
            "mul.oneIsIdentity",
            "pick.positiveIsOne",
            "sequenceChecked.agreesWithSpec",
        ]
    );
    // A law whose steps cite a law the backend did not close is not closed.
    let (_, bytes) = aver_backend_json("bytes.av", &out, Some(""));
    assert_eq!(bytes["closed_by"]["decode.eightReadBack"], "open");
    let _ = fs::remove_dir_all(out);
}

#[test]
fn both_backends_close_the_same_laws_by_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("both-aver");
    let (_, aver) = aver_backend_json("lock.av", &out, None);
    let lean_out = scratch("both-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "lock.av",
            "-o",
            lean_out.to_str().unwrap(),
            "--check-json",
        ],
    );
    let line = String::from_utf8_lossy(&result.stdout)
        .lines()
        .rev()
        .find(|l| l.starts_with('{'))
        .map(str::to_string)
        .unwrap_or_else(|| panic!("{}", format_output(&result)));
    let lean: serde_json::Value = serde_json::from_str(&line).unwrap();
    assert_eq!(lean["backend"], "lean");
    assert_eq!(closed_by_steps(&aver), closed_by_steps(&lean));
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(lean_out);
}

#[test]
fn the_embedded_kernel_refuses_mutated_scripts() {
    let out = scratch("embedded");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("lock.av", &out).into_iter().collect();
    let lock = fs::read_to_string(&files["lockTimeChecked.agreesWithSpec"]).unwrap();
    let started = std::time::Instant::now();
    assert_eq!(
        aver::proof_kernel::verdict(&lock),
        Ok("lockTimeChecked.agreesWithSpec".to_string())
    );
    assert!(started.elapsed() < std::time::Duration::from_secs(2));
    for (from, to) in [
        ("(hyp h_steps2)", "(hyp h_steps1)"),
        ("(i 500000000)", "(i 500000001)"),
        ("(unfold continued 2", "(unfold continued 1"),
        ("(hyp h_steps3)", "(hyp h_steps7)"),
        ("(rule bool.and.true_l", "(rule bool.and.simp"),
    ] {
        let refused = aver::proof_kernel::verdict(&mutate_proof(&lock, from, to));
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{from} -> {to}: {refused:?}"
        );
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn both_kernels_unfold_through_local_bindings_and_refuse_mutations() {
    let out = scratch("lets");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("lets.av", &out).into_iter().collect();
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let sum = read("sumAndDouble.isTwiceTheSum");
    assert!(sum.contains("((total (op + (v a) (v b))) (twice (op + (v total) (v total))))"));
    let bumped = read("bumped.positiveStays");
    for (kind, text) in [
        (
            "arguments swapped",
            mutate_proof(&sum, "((v a) (v b))", "((v b) (v a))"),
        ),
        (
            "the other arm",
            mutate_proof(&bumped, "(unfold bumped 1", "(unfold bumped 2"),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{kind}: {refused:?}"
        );
        let path = out.join("mutant.steps");
        fs::write(&path, &text).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(
            !result.status.success(),
            "{kind}: {}",
            format_output(&result)
        );
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn both_kernels_split_a_finite_given_into_every_value_and_refuse_mutations() {
    let out = scratch("finite");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("finite.av", &out).into_iter().collect();
    assert_eq!(
        files.keys().cloned().collect::<Vec<_>>(),
        [
            "agree.symmetric",
            "lit.offIsDark",
            "next.cyclesInThree",
            "safe.notSafeMeansNextIsAmber",
        ]
    );
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let cycle = read("next.cyclesInThree");
    let agree = read("agree.symmetric");
    let safe = read("safe.notSafeMeansNextIsAmber");
    assert!(safe.contains("(absurd "), "{safe}");
    for (kind, text) in [
        ("a missing case", recase(&cycle, &[0, 1])),
        ("two cases swapped", recase(&cycle, &[1, 0, 2])),
        ("a case repeated", recase(&cycle, &[0, 1, 1])),
        (
            "the wrong given",
            mutate_proof(&agree, "(enum b ", "(enum a "),
        ),
        (
            "a split of a given that is not finite",
            agree.replace("((a (tbool)) (b (tbool)))", "((a (tbool)) b)"),
        ),
        ("an absurd case that is not", {
            let open = safe.find("(absurd ").unwrap();
            let (from, to) = sub_forms(&safe, open)[0];
            format!("{}(refl (b true)){}", &safe[..from], &safe[to..])
        }),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{kind}: {refused:?}"
        );
        let path = out.join("mutant.steps");
        fs::write(&path, &text).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(
            !result.status.success(),
            "{kind}: {}",
            format_output(&result)
        );
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_splits_a_finite_given_and_refuses_a_missing_case() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("finite-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "finite.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&result.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    for law in [
        "agree.symmetric",
        "lit.offIsDark",
        "next.cyclesInThree",
        "safe.notSafeMeansNextIsAmber",
    ] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}");
    }
    let lean = fs::read_to_string(out.join("Finite.lean")).unwrap();
    let law = "next.cyclesInThree";
    let line = exact_line(&lean, "next_law_cyclesInThree").to_string();
    let case = |ctor: &str| {
        let head = format!(" | Light.{ctor} => ");
        let at = line.find(&head).unwrap_or_else(|| panic!("no case {ctor}"));
        let start = at + head.len();
        (at, start, closing(&line, start) + 1)
    };
    // Two cases' proofs swapped: each proves the other value's claim.
    let (_, red_from, red_to) = case("red");
    let (_, amber_from, amber_to) = case("amber");
    let swapped = format!(
        "{}{}{}{}{}",
        &line[..red_from],
        &line[amber_from..amber_to],
        &line[red_to..amber_from],
        &line[red_from..red_to],
        &line[amber_to..]
    );
    assert!(
        lean_refuses(&out, "Finite.lean", &lean.replacen(&line, &swapped, 1), law),
        "two cases swapped: Lean must refuse the step term"
    );
    // A missing case: Lean reports the match as incomplete. That error is
    // logged rather than thrown, so the law fails instead of falling back;
    // read it with the file's message filter for this law removed.
    let (green_at, _, green_to) = case("green");
    let missing = format!("{}{}", &line[..green_at], &line[green_to..]);
    let unfiltered = lean.replacen(&line, &missing, 1).replacen(
        &format!(
            "#guard_msgs (drop error, pass warning, pass info, pass trace) in\n-- verify law {law} "
        ),
        &format!("-- verify law {law} "),
        1,
    );
    fs::write(out.join("Finite.lean"), &unfiltered).unwrap();
    let run = Command::new("lake")
        .args(["env", "lean", "Finite.lean"])
        .current_dir(&out)
        .output()
        .expect("lake runs");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );
    assert!(
        text.contains("Missing cases"),
        "a missing case: Lean must refuse the step term\n{text}"
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_unfolds_through_local_bindings_and_refuses_a_mutation() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("lets-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "lets.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("Lets.lean")).unwrap();
    let law = "sumAndDouble.isTwiceTheSum";
    assert!(!lean_refuses(&out, "Lets.lean", &lean, law));
    let line = exact_line(&lean, "sumAndDouble_law_isTwiceTheSum").to_string();
    let at = line.find("exact (show").expect("the step term");
    let (lemmas, term) = line.split_at(at);
    let swapped = term.replacen("__aver_unfold_0 (a) (b)", "__aver_unfold_0 (b) (a)", 1);
    assert_ne!(term, swapped, "{term}");
    let mutated = lean.replacen(&line, &format!("{lemmas}{swapped}"), 1);
    assert!(
        lean_refuses(&out, "Lets.lean", &mutated, law),
        "arguments swapped: Lean must refuse the mutated step term"
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn a_using_list_is_a_set_and_ambiguous_or_looping_rewrites_are_refused_by_name() {
    let out = scratch("using");
    let result = Command::new(aver_bin())
        .args([
            "proof",
            "using.av",
            "-o",
            out.to_str().unwrap(),
            "--verify-mode",
            "sorry",
        ])
        .env("AVER_STEPS_DEBUG", "1")
        .current_dir(repo_root().join(FIXTURES))
        .output()
        .expect("aver runs");
    assert!(result.status.success(), "{}", format_output(&result));
    let log = String::from_utf8_lossy(&result.stderr);
    let line = |law: &str| {
        log.lines()
            .find(|l| l.starts_with(&format!("steps: {law}: ")))
            .unwrap_or_else(|| panic!("no steps line for {law}\n{log}"))
            .to_string()
    };
    // The order of the list changes nothing.
    let read =
        |law: &str| fs::read_to_string(out.join(format!("proof_steps/{law}.steps"))).unwrap();
    assert_eq!(
        read("g.throughHOneWay").replace("throughHOneWay", "_"),
        read("g.throughHOtherWay").replace("throughHOtherWay", "_")
    );
    assert!(
        line("g.overlapping")
            .contains("law f.isG and law f.isH both rewrite `f(x)`, to different terms"),
        "{log}"
    );
    assert!(
        line("add.swapsOne").contains(
            "law add.commutes rewrites a term into one it applies to again, so rewriting with it never stops"
        ),
        "{log}"
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn the_aver_backend_says_where_the_steps_producer_stopped() {
    let out = scratch("where");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "using.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let text = String::from_utf8_lossy(&result.stdout);
    for expected in [
        "  f.isH: closed by steps",
        "  f.isG: not closed by this backend (steps: evaluation stops at `x + 1` and `1 + x`",
        "  g.overlapping: not closed by this backend (steps: law f.isG and law f.isH both rewrite",
    ] {
        assert!(text.contains(expected), "missing `{expected}`\n{text}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The sub-form `index` of the first form opening with `head`.
fn nth_form(text: &str, head: &str, index: usize) -> (usize, usize) {
    let open = text.find(head).unwrap_or_else(|| panic!("no `{head}`"));
    sub_forms(text, open)[index]
}

const INDUCTION_LAWS: [&str; 4] = [
    "app.lengthAdds",
    "plus.succRight",
    "plus.zeroRight",
    "revOnto.isReverseThenAppend",
];

#[test]
fn both_kernels_induct_along_the_laws_function_and_refuse_mutations() {
    let out = scratch("induct");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("induction.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), INDUCTION_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let law = read("app.lengthAdds");
    assert!(law.contains("(proof (induct app "), "{law}");
    // (induct FN (ARG…) LHS RHS CASE…): the cases are sub-forms 3 and 4,
    // each (case (NAME…) (IH…) PROOF).
    let (base_from, base_to) = nth_form(&law, "(induct ", 3);
    let (step_from, step_to) = nth_form(&law, "(induct ", 4);
    let part = |from: usize, to: usize, i: usize| {
        let (a, b) = sub_forms(&law[from..to], 0)[i];
        (from + a, from + b)
    };
    let (names_from, names_to) = part(step_from, step_to, 0);
    let (ihs_from, ihs_to) = part(step_from, step_to, 1);
    let (proof_from, proof_to) = part(base_from, base_to, 2);
    let ih = law[ihs_from + 1..ihs_to - 1].trim().to_string();
    let names: Vec<&str> = law[names_from + 1..names_to - 1]
        .split_whitespace()
        .collect();
    assert_eq!(names.len(), 2, "{law}");
    for (kind, text) in [
        (
            "the hypothesis used in the base case",
            format!("{}(hyp {ih}){}", &law[..proof_from], &law[proof_to..]),
        ),
        (
            "the hypothesis at the wrong list",
            format!(
                "{}({} {}){}",
                &law[..names_from],
                names[1],
                names[0],
                &law[names_to..]
            ),
        ),
        (
            "a missing case",
            format!("{}{}", law[..step_from].trim_end(), &law[step_to..]),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{kind}: {refused:?}\n{text}"
        );
        let path = out.join("mutant.steps");
        fs::write(&path, &text).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(
            !result.status.success(),
            "{kind}: {}",
            format_output(&result)
        );
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn induction_follows_only_the_function_the_law_is_about() {
    let out = scratch("induct-which");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction_refused.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&result.stdout);
    assert!(
        text.contains("double does not recurse but len does"),
        "{}",
        format_output(&result)
    );
    // A claim that recurses on two givens of the same function is a
    // choice the law has to make, not one the producer guesses.
    let both = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&both.stdout);
    assert!(
        text.contains("app recurses on a in one call and on b in another"),
        "{}",
        format_output(&both)
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_inducts_with_the_functional_induction_principle_and_refuses_mutations() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("induct-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&result.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap(),
    )
    .unwrap();
    for law in INDUCTION_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Induction.lean")).unwrap();
    let law = "app.lengthAdds";
    let line = exact_line(&lean, "app_law_lengthAdds").to_string();
    let at = line
        .find("app.induct (motive :=")
        .unwrap_or_else(|| panic!("{line}"));
    // app.induct (motive := …) BASE STEP xs: the base case and the step
    // case are the two forms after the motive.
    let motive_open = at + "app.induct ".len();
    let base_open = closing(&line, motive_open) + 2;
    let base_close = closing(&line, base_open);
    let step_open = base_close + 2;
    let step_close = closing(&line, step_open);
    let missing = format!("{}{}", &line[..base_close + 1], &line[step_close + 1..]);
    assert!(
        lean_refuses(
            &out,
            "Induction.lean",
            &lean.replacen(&line, &missing, 1),
            law
        ),
        "a missing case: Lean must refuse the step term"
    );
    let ih = line[step_open..step_close]
        .trim_start_matches("(fun ")
        .split(" =>")
        .next()
        .unwrap()
        .split_whitespace()
        .last()
        .unwrap()
        .to_string();
    let base_uses_ih = format!("{}({ih}){}", &line[..base_open], &line[base_close + 1..]);
    assert!(
        lean_refuses(
            &out,
            "Induction.lean",
            &lean.replacen(&line, &base_uses_ih, 1),
            law
        ),
        "the hypothesis used in the base case: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn the_embedded_kernel_is_generated_from_the_aver_source() {
    let out = Command::new("python3")
        .args([
            "tools/regenerate_proof_kernel.py",
            "--check",
            "--aver-bin",
            aver_bin(),
        ])
        .current_dir(repo_root())
        .output()
        .expect("python3 runs");
    assert!(out.status.success(), "{}", format_output(&out));
}
