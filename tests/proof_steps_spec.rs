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
    assert_eq!(
        bytes,
        [
            "decode.addsLowByte",
            "decode.eightReadBack",
            "decode.fourReadBack",
            "decodeFrom.lastDigit",
            "encode.peelsLowByte",
        ]
    );
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
    // Such a law is still one instance: it proves an equation it matches
    // whole, either way round.
    assert!(line("sameLen.againstOne").ends_with(" nodes"), "{log}");
    // Evaluation decides the comparison under `Bool.not` without the law.
    assert!(
        line("sameLen.negatedAgainstOne").ends_with(" nodes"),
        "{log}"
    );
    assert!(
        line("sameLen.negatedAgainstPadded").contains(
            "law sameLen.commutes rewrites a term into one it applies to again, so rewriting with it never stops"
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
        "  f.isG: not closed by this backend (steps: evaluation stops at `x + \"!\"` and `(\"\" + x) + \"!\"`",
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

const INDUCTION_LAWS: [&str; 5] = [
    "app.lengthAdds",
    "plus.succRight",
    "plus.zeroRight",
    "revOnto.accumulatorLast",
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
    let last = read("revOnto.accumulatorLast");
    assert!(
        last.contains("((0 ih2 ((bi List.prepend (v h) (list))) ()))"),
        "{last}"
    );
    // (induct FN (ARG…) LHS RHS CASE…): the cases are sub-forms 3 and 4,
    // each (case (NAME…) (IH…) (MORE…) PROOF).
    let (base_from, base_to) = nth_form(&law, "(induct ", 3);
    let (step_from, step_to) = nth_form(&law, "(induct ", 4);
    let part = |from: usize, to: usize, i: usize| {
        let (a, b) = sub_forms(&law[from..to], 0)[i];
        (from + a, from + b)
    };
    let (names_from, names_to) = part(step_from, step_to, 0);
    let (ihs_from, ihs_to) = part(step_from, step_to, 1);
    let (proof_from, proof_to) = part(base_from, base_to, 3);
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
        // The right side's call passes `[h]` where the left side's passes
        // `List.prepend(h, acc)`: a second hypothesis at the same tail.
        (
            "a further hypothesis at the accumulator the claim has",
            last.replacen(
                "((0 ih2 ((bi List.prepend (v h) (list))) ()))",
                "((0 ih2 ((v acc)) ()))",
                1,
            ),
        ),
        (
            "a further hypothesis on a recursive call the arm does not make",
            last.replacen("((0 ih2 ", "((1 ih2 ", 1),
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
fn an_induction_line_names_the_given_the_induction_follows() {
    let out = scratch("induct-named");
    let (result, summary) = aver_backend_json("induction_named.av", &out, None);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    let closed = &summary["closed_by"];
    // `append` recurses on `x` in one call and on `y` in another: the law
    // that names `x` closes, and the others are refused.
    assert_eq!(closed["append.assoc"], "steps", "{summary}");
    for law in [
        "append.assocUnnamed",
        "append.assocAlongY",
        "append.assocAlongZ",
    ] {
        assert_eq!(closed[law], "open", "{law}: {summary}");
    }
    let text = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction_named.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&text.stdout);
    let line = |law: &str| {
        text.lines()
            .find(|l| l.trim_start().starts_with(&format!("{law}:")))
            .unwrap_or_else(|| panic!("no line for {law}: {text}"))
            .to_string()
    };
    assert!(
        line("append.assocUnnamed").contains(
            "append recurses on x in one call and on y in another; which to follow is a choice the law has to make: name one with `induction x` or `induction y`"
        ),
        "{}",
        format_output(&result)
    );
    assert!(
        line("append.assocAlongZ")
            .contains("the law names z, but no call of append passes z where append recurses"),
        "{text}"
    );
    assert!(
        line("append.assocAlongY").contains("induction along append, case 1"),
        "{text}"
    );
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("induction_named.av", &out)
            .into_iter()
            .collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), ["append.assoc"]);
    let law = fs::read_to_string(&files["append.assoc"]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&law),
        Ok("append.assoc".to_string())
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

const COUNTDOWN_LAWS: [&str; 9] = [
    "below.nonnegativeFactor",
    "below.nonnegativeFactor.because1",
    "below.nonnegativeFactor.implication",
    "climbs.nonnegative",
    "digitsInto.accumulatorFirst",
    "digitsInto.length",
    "pow2.positive",
    "scaled.pinnedToZero",
    "shifted.atThree",
];

/// `text` with the sub-form `index` of its `case`-th `(case …)` replaced:
/// (case (NAME) (IH…) (MORE…) PROOF), the comparison's hypothesis sub-form
/// 0, the hypotheses of the recursive calls 1, the further ones 2, the
/// proof 3.
fn replace_in_case(text: &str, case: usize, index: usize, to: &str) -> String {
    let open = text
        .match_indices("(case ")
        .nth(case)
        .unwrap_or_else(|| panic!("no case {case}"))
        .0;
    let (from, end) = sub_forms(text, open)[index];
    format!("{}{to}{}", &text[..from], &text[end..])
}

#[test]
fn both_kernels_induct_on_an_int_down_to_zero_and_refuse_mutations() {
    let out = scratch("countdown");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("countdown.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), COUNTDOWN_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let pow2 = read("pow2.positive");
    let reason = read("below.nonnegativeFactor.because1");
    let at_three = read("shifted.atThree");
    let digits = read("digitsInto.accumulatorFirst");
    // One induction along the function, its case where n is at most 0
    // first, without a hypothesis.
    assert!(pow2.contains("(proof (induct pow2 ((v n)) "), "{pow2}");
    assert!(pow2.contains("(case (h_steps1) () () "), "{pow2}");
    assert!(pow2.contains("((n (tint)))"), "{pow2}");
    // The `when` and its lines about `a`, `b` and `c` are carried down
    // with `c`, `a` and `b` generalised.
    assert!(reason.contains(" (carry when when1 when2) "), "{reason}");
    assert!(at_three.contains("(rule int.eq.of_beq "), "{at_three}");
    // The accumulator and the value change with the recursive call, so the
    // claim holds for every value of them; the right side's call needs the
    // claim at another accumulator than the left side's.
    assert!(
        digits.contains(
            "(case (h_steps2) (ih1) ((0 ih2 ((bi __int_div_euclid (v value) (i 10)) (bi List.prepend"
        ),
        "{digits}"
    );
    for (kind, text) in [
        (
            "a hypothesis in the case where n is at most 0",
            replace_in_case(&pow2, 0, 1, "(ih1)"),
        ),
        (
            "the hypothesis read in the case where n is at most 0",
            replace_in_case(&pow2, 0, 3, "(hyp ih1)"),
        ),
        (
            "the hypothesis of the case where n is above 0 read as the claim",
            replace_in_case(&pow2, 1, 3, "(hyp ih1)"),
        ),
        (
            "the definition counting down by two",
            pow2.replacen("(op - (v n) (i 1))", "(op - (v n) (i 2))", 1),
        ),
        (
            "the definition counting down by nothing",
            pow2.replacen("(op - (v n) (i 1))", "(op - (v n) (i 0))", 1),
        ),
        (
            "the comparison read the other way round",
            pow2.replacen("(op <= (v n) (i 0))", "(op >= (v n) (i 0))", 1),
        ),
        (
            "the carried hypotheses left out",
            reason.replacen(" (carry when when1 when2) ", " ", 1),
        ),
        (
            "a further hypothesis at the value the claim has, not the recursive call's",
            digits.replacen(
                "((0 ih2 ((bi __int_div_euclid (v value) (i 10))",
                "((0 ih2 ((v value)",
                1,
            ),
        ),
        (
            "induction on a given not known to be an Int",
            pow2.replacen("((n (tint)))", "(n)", 1),
        ),
        (
            "an equality used for another value",
            mutate_proof(&at_three, "(b (i 3))", "(b (i 4))"),
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
fn steps_open_only_a_countdown_by_one_to_zero() {
    let out = scratch("countdown-gate");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "countdown.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&result.stdout);
    assert!(
        text.contains("steps do not open `halves`"),
        "{}",
        format_output(&result)
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_inducts_on_an_int_down_to_zero_and_refuses_a_mutation() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("countdown-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "countdown.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "1",
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
    for law in COUNTDOWN_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Countdown.lean")).unwrap();
    let law = "pow2.positive";
    // The claim is a comparison, so the step term sits inside `propext`.
    let start = lean
        .find("theorem pow2_law_positive :")
        .expect("the law's theorem");
    let line = lean[start..]
        .lines()
        .find(|l| l.contains("AverSteps.int_measure_induct (P :="))
        .expect("the steps branch")
        .to_string();
    let at = line
        .find("AverSteps.int_measure_induct (P :=")
        .unwrap_or_else(|| panic!("{line}"));
    // AverSteps.int_measure_induct (P := …) (fun n => GUARD) ON_TRUE
    // ON_FALSE n: the two cases are the forms after the comparison.
    let motive_open = at + "AverSteps.int_measure_induct ".len();
    let guard_open = closing(&line, motive_open) + 2;
    let base_open = closing(&line, guard_open) + 2;
    let base_close = closing(&line, base_open);
    let step_open = base_close + 2;
    let step_close = closing(&line, step_open);
    let swapped = format!(
        "{}{} {}{}",
        &line[..base_open],
        &line[step_open..=step_close],
        &line[base_open..=base_close],
        &line[step_close + 1..]
    );
    assert!(
        lean_refuses(
            &out,
            "Countdown.lean",
            &lean.replacen(&line, &swapped, 1),
            law
        ),
        "the cases swapped: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

const CITED_ORDER_LAWS: [&str; 5] = [
    "positivePower.holds",
    "pow2.positive",
    "twoPow.agrees",
    "twoPow.agrees.because1",
    "twoPow.agrees.implication",
];

#[test]
fn both_kernels_check_cited_orders_and_equal_calls_and_refuse_mutations() {
    let out = scratch("cited-order");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("cited_order.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), CITED_ORDER_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let positive = read("positivePower.holds");
    let agrees = read("twoPow.agrees.because1");
    // The cited order at the goal's own atom, then linear arithmetic.
    let instance = "(law pow2.positive ((n (op - (i 0) (v k)))))";
    assert!(positive.contains(instance), "{positive}");
    // The hypothesis about `twoPow`, read through its one unfolding.
    let equal = "(rule int.eq.of_beq ((a (call twoPow (op - (v n) (i 1)))) (b (call pow2 (op - (v n) (i 1)))))";
    assert!(agrees.contains(equal), "{agrees}");
    for (kind, text) in [
        (
            "the cited law at another argument",
            mutate_proof(&positive, instance, "(law pow2.positive ((n (v k))))"),
        ),
        (
            "the equality used for another call",
            mutate_proof(
                &agrees,
                equal,
                "(rule int.eq.of_beq ((a (call twoPow (op - (v n) (i 1)))) (b (call pow2 (v n))))",
            ),
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
fn lean_closes_cited_orders_and_equal_calls_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("cited-order-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "cited_order.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
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
    for law in ["positivePower.holds", "pow2.positive", "twoPow.agrees"] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const CITED_WHEN_LAWS: [&str; 12] = [
    "multiplyLe.nonnegativeFactor",
    "multiplyLe.nonnegativeFactor.because1",
    "multiplyLe.nonnegativeFactor.implication",
    "multiplyLt.positiveFactor",
    "multiplyLt.positiveFactor.because1",
    "multiplyLt.positiveFactor.implication",
    "positiveProduct.positiveFactors",
    "positiveProduct.positiveFactors.because1",
    "positiveProduct.positiveFactors.implication",
    "pow2.positive",
    "productOfPowers.positive",
    "scaledPower.positiveScale",
];

#[test]
fn both_kernels_check_cited_orders_under_a_proved_when_and_refuse_mutations() {
    let out = scratch("cited-when");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("cited_when.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), CITED_WHEN_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let product = read("productOfPowers.positive");
    // The cited law at the goal's own product, its `when` proved there.
    let instance = "(law positiveProduct.positiveFactors ((a (call pow2 (op - (i 0) (v k)))) (b (call pow2 (v n))))";
    assert!(product.contains(instance), "{product}");
    let mutant = mutate_proof(
        &product,
        instance,
        "(law positiveProduct.positiveFactors ((a (v k)) (b (call pow2 (v n))))",
    );
    let verdict = aver::proof_kernel::verdict(&mutant);
    assert!(
        verdict
            .as_ref()
            .is_err_and(|why| why.starts_with("step proof")),
        "{verdict:?}\n{mutant}"
    );
    let path = out.join("mutant.steps");
    fs::write(&path, &mutant).unwrap();
    let result = replay(std::slice::from_ref(&path));
    assert!(!result.status.success(), "{}", format_output(&result));
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_cited_orders_under_a_proved_when_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("cited-when-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "cited_when.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
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
    for law in ["productOfPowers.positive", "scaledPower.positiveScale"] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const HALVING_LAWS: [&str; 9] = [
    "digits.accumulatorAppends",
    "digits.exponentAbove",
    "digits.fourDigitsReadBack",
    "digits.nonpositiveKeepsAcc",
    "digits.oneDigit",
    "digits.positiveStep",
    "digits.readingEquals",
    "digits.readingReadBack",
    "digits.twoDigitsReadBack",
];

/// A definition that divides an Int down to zero opens where its guard is
/// decided, a law about it is proved by induction along it, and Euclidean
/// division by a literal is pinned by linear arithmetic; both kernels
/// refuse a definition that divides by one, opened or followed, a
/// hypothesis in the case where the value is at most 0, and a quotient
/// bound the dividend's range does not give.
#[test]
fn both_kernels_open_a_division_down_to_zero_and_refuse_mutations() {
    let out = scratch("halving");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("halving.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), HALVING_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let step = read("digits.positiveStep");
    let one = read("digits.oneDigit");
    let appends = read("digits.accumulatorAppends");
    assert!(step.contains("(unfold digits 1 "), "{step}");
    assert!(one.contains("(rule int.div_range "), "{one}");
    assert!(
        appends.contains("(proof (induct digits ((v value) (v acc)) "),
        "{appends}"
    );
    let mutants = [
        // The definition the induction follows divides by one: the claim
        // at `value / 1` is the claim itself.
        (
            &appends,
            appends.replacen(
                "(call digits (bi __int_div_euclid (v value) (i 256))",
                "(call digits (bi __int_div_euclid (v value) (i 1))",
                1,
            ),
        ),
        // A further hypothesis in the case where value is at most 0.
        (
            &appends,
            replace_in_case(&appends, 1, 2, "((0 ih9 ((list)) ()))"),
        ),
        // The dividing case without its call's hypothesis name.
        (
            &appends,
            appends.replacen("(case (h_steps1) (ih1) ", "(case (h_steps1) () ", 1),
        ),
        // The definition divides by one, everywhere it is written.
        (&step, step.replace("(i 256)", "(i 1)")),
        // A quotient bound read from a range the dividend is not shown in.
        (
            &one,
            one.replacen("(m (i 256)) (n (i 1))", "(m (i 512)) (n (i 2))", 1),
        ),
        // A bound that is not the range divided by the divisor.
        (
            &one,
            one.replacen("(m (i 256)) (n (i 1))", "(m (i 256)) (n (i 2))", 1),
        ),
    ];
    for (i, (source, mutant)) in mutants.iter().enumerate() {
        assert_ne!(mutant, *source, "mutant {i} changed nothing");
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(
            verdict
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "mutant {i}: {verdict:?}\n{mutant}"
        );
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "halving.av",
            "--backend",
            "aver",
            "-o",
            out.join("aver").to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&result.stdout);
    assert!(
        text.contains("steps do not open `stays`"),
        "{}",
        format_output(&result)
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_a_division_down_to_zero_by_its_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("halving-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "halving.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "1",
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
    for law in HALVING_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const DESCENTS_LAWS: [&str; 2] = ["mixed.atLeastAcc", "splits.nonnegative"];

/// An Int counted toward zero whose recursive calls descend differently:
/// two calls dividing by different literals, and `n - 1` beside `n / 4`
/// with an accumulator. Each call gives its own hypothesis at its own Int.
#[test]
fn both_kernels_induct_along_calls_that_descend_differently_and_refuse_mutations() {
    let out = scratch("descents");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("descents.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), DESCENTS_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let splits = read("splits.nonnegative");
    let mixed = read("mixed.atLeastAcc");
    assert!(splits.contains("(proof (induct splits "), "{splits}");
    assert!(mixed.contains("(proof (induct mixed "), "{mixed}");
    let mutants = [
        // One of the two divisions divides by one, everywhere it is written.
        (&splits, splits.replace("(v n) (i 3))", "(v n) (i 1))")),
        // The dividing case without the second call's hypothesis.
        (
            &splits,
            splits.replacen("(case (h_steps1) (ih1 ih2) ", "(case (h_steps1) (ih1) ", 1),
        ),
        // The step down subtracts nothing, everywhere it is written.
        (
            &mixed,
            mixed.replace("(op - (v n) (i 1))", "(op - (v n) (i 0))"),
        ),
        // The carried hypothesis of the dividing call proved at another value.
        (
            &mixed,
            mixed.replacen(
                "(ih2 (compute (op >= (i 0) (i 0)) (b true)))",
                "(ih2 (compute (op >= (i 1) (i 0)) (b true)))",
                1,
            ),
        ),
    ];
    for (i, (source, mutant)) in mutants.iter().enumerate() {
        assert_ne!(mutant, *source, "mutant {i} changed nothing");
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(
            verdict
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "mutant {i}: {verdict:?}\n{mutant}"
        );
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_calls_that_descend_differently_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("descents-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "descents.av",
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
    for law in DESCENTS_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// One table for the termination gate steps open a definition through and
/// the Lean classifier that decides which definitions Lean models as a
/// well-founded def on `n.toNat`: each recursive arm below sits under
/// `match n > 0`, and both read the same source. Where the gate accepts,
/// Lean models; where it refuses, Lean does not, except a countdown by a
/// literal above one, which Lean has modelled since before steps existed.
#[test]
fn the_gate_and_the_lean_classifier_agree_on_ints_counted_toward_zero() {
    // (the recursive arm, whether the gate opens it, whether Lean models it)
    let table: [(&str, bool, bool); 10] = [
        ("1 + f(Int.div(n, 2)) + f(Int.div(n, 3))", true, true),
        ("1 + f(n - 1) + f(Int.div(n, 4))", true, true),
        ("1 + f(Int.div(n, 256))", true, true),
        ("1 + f(n - 1)", true, true),
        ("1 + f(Int.div(n, 1)) + f(n - 1)", false, false),
        (
            "1 + f(Int.div(n, 2)) + f(Result.withDefault(Int.div(n, n), 0))",
            false,
            false,
        ),
        ("1 + f(n - 0) + f(Int.div(n, 2))", false, false),
        ("1 + f(n + 1) + f(Int.div(n, 2))", false, false),
        (
            "1 + f(Result.withDefault(Int.div(n, 0 - 2), 0))",
            false,
            false,
        ),
        ("1 + f(n - 2)", false, true),
    ];
    let out = scratch("descent-table");
    for (i, (arm, gate, lean)) in table.iter().enumerate() {
        let dir = out.join(format!("row{i}"));
        fs::create_dir_all(&dir).unwrap();
        let source = format!(
            "module Table\n    intent = \"One row of the descent table.\"\n    exposes [f]\n    effects []\n\n\
             fn f(n: Int) -> Int\n    ? \"A row.\"\n    match n > 0\n        true -> {arm}\n        false -> 0\n\n\
             verify f law nonnegative\n    given n: Int = [-1, 0]\n    f(n) >= 0 holds\n"
        );
        fs::write(dir.join("table.av"), source).unwrap();
        let steps = aver_in(&dir, &["proof", "table.av", "--backend", "aver"]);
        let text = String::from_utf8_lossy(&steps.stdout).to_string();
        assert_eq!(
            text.contains("f.nonnegative: closed by steps"),
            *gate,
            "row {i} `{arm}`, the gate: {}",
            format_output(&steps)
        );
        assert_eq!(
            text.contains("steps do not open `f`"),
            !gate,
            "row {i} `{arm}`, the gate: {}",
            format_output(&steps)
        );
        let lean_out = dir.join("lean");
        let emitted = aver_in(
            &dir,
            &["proof", "table.av", "-o", lean_out.to_str().unwrap()],
        );
        assert!(emitted.status.success(), "{}", format_output(&emitted));
        let model = fs::read_to_string(lean_out.join("Table.lean")).unwrap();
        assert_eq!(
            model.contains("termination_by n.toNat"),
            *lean,
            "row {i} `{arm}`, Lean:\n{model}"
        );
    }
    let _ = fs::remove_dir_all(out);
}

/// A definition two functions recurse through together is refused by the
/// gate, and the classifier for one function counting an Int toward zero
/// does not model it either: a mutual group with a division is outside
/// what Lean models.
#[test]
fn neither_the_gate_nor_the_classifier_takes_mutual_recursion_with_a_division() {
    let out = scratch("descent-mutual");
    fs::create_dir_all(&out).unwrap();
    let source = "module Pair\n    intent = \"Two functions that call each other.\"\n    exposes [ping, pong]\n    effects []\n\n\
                  fn ping(n: Int) -> Int\n    ? \"Ping.\"\n    match n > 0\n        true -> 1 + pong(Int.div(n, 2))\n        false -> 0\n\n\
                  fn pong(n: Int) -> Int\n    ? \"Pong.\"\n    match n > 0\n        true -> 1 + ping(n - 1)\n        false -> 0\n\n\
                  verify ping law nonnegative\n    given n: Int = [-1, 0]\n    ping(n) >= 0 holds\n";
    fs::write(out.join("pair.av"), source).unwrap();
    let steps = aver_in(&out, &["proof", "pair.av", "--backend", "aver"]);
    let text = String::from_utf8_lossy(&steps.stdout).to_string();
    assert!(
        !text.contains("ping.nonnegative: closed by steps"),
        "{}",
        format_output(&steps)
    );
    let lean_out = out.join("lean");
    let emitted = aver_in(
        &out,
        &["proof", "pair.av", "-o", lean_out.to_str().unwrap()],
    );
    let model = fs::read_to_string(lean_out.join("Pair.lean")).unwrap_or_default();
    assert!(
        !model.contains("termination_by n.toNat"),
        "{}\n{model}",
        format_output(&emitted)
    );
    let _ = fs::remove_dir_all(out);
}

const CITED_LOOP_LAWS: [&str; 3] = [
    "flat.prepend",
    "intoChunks.accumulates",
    "pushBack.reverseOnto",
];

/// A cited law whose right side holds a call that opens straight back to
/// the term it rewrote is not applied there, so evaluation goes on instead
/// of opening and rewriting the same term forever; both kernels accept what
/// it gives.
#[test]
fn both_kernels_accept_steps_past_a_cited_law_that_opens_back() {
    let out = scratch("cited-loop");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("cited_loop.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), CITED_LOOP_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let paths: Vec<PathBuf> = files.values().cloned().collect();
    let result = replay(&paths);
    assert!(result.status.success(), "{}", format_output(&result));
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_steps_past_a_cited_law_that_opens_back() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("cited-loop-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "cited_loop.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "1",
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
    for law in CITED_LOOP_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const CTOR_SPLIT_LAWS: [&str; 4] = [
    "bump.keepsPresence",
    "double.keepsLength",
    "switchOff.staysDark",
    "twice.keepsSuccess",
];

/// Where a `match` stops at a subject of a sum type (with fields, behind a
/// catch-all arm), an Option, a Result, or a list a hypothesis reads, the
/// proof goes on in one case per constructor; both kernels accept what it
/// gives.
#[test]
fn both_kernels_accept_a_split_on_a_constructor() {
    let out = scratch("split");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("split.av", &out).into_iter().collect();
    let mut expected: Vec<String> = CTOR_SPLIT_LAWS.iter().map(|l| l.to_string()).collect();
    expected.push("double.keepsLength.because1".into());
    expected.push("double.keepsLength.implication".into());
    expected.sort();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), expected);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert!(read(law).contains("(split "), "{law}: no split");
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let paths: Vec<PathBuf> = files.values().cloned().collect();
    let result = replay(&paths);
    assert!(result.status.success(), "{}", format_output(&result));

    // `switchOff` reads `Light.Off` and a catch-all: a split that covers
    // only `Light.Off`, the one constructor an arm names, leaves `Dim` and
    // `Full` out, which the program declares.
    let dark = read("switchOff.staysDark");
    let open = dark.find("(split ").expect("a split");
    // (split FN (ARGS) ON HYP CASE…): the first two sub-forms are the
    // arguments and the subject.
    let cases = sub_forms(&dark, open)[2..].to_vec();
    assert_eq!(cases.len(), 3, "{dark}");
    let only_named = format!("{}{}", &dark[..cases[1].0], &dark[cases[2].1..]);
    for (i, mutant) in [only_named].iter().enumerate() {
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(
            verdict
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "mutant {i}: {verdict:?}\n{mutant}"
        );
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_a_split_on_a_constructor() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("split-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "split.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "1",
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
    for law in CTOR_SPLIT_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const MATCH_ARMS_LAWS: [&str; 7] = [
    "height.ofASquare",
    "isBox.unlessADot",
    "isMany.exceptZeroAndOne",
    "okOr.errorIsZero",
    "restLength.afterOne",
    "width.sameAsComparisons",
    "widthBits.eightPerByte",
];

/// The split follows the arms of a `match` whatever their patterns: one
/// case per arm of Int literals and a last catch-all, the catch-all chosen
/// where hypotheses rule out every literal arm before it (from the split or
/// from Bool splits on `==`), and a wildcard field of a constructor or a
/// cell taking its place among the binders. The kernel refuses the
/// catch-all case one hypothesis short, a literal case turned into a second
/// catch-all, and the catch-all case's proof without the split around it.
#[test]
fn both_kernels_split_on_the_arms_of_a_match_and_refuse_mutations() {
    let out = scratch("match-arms");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("match_arms.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), MATCH_ARMS_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let paths: Vec<PathBuf> = files.values().cloned().collect();
    let result = replay(&paths);
    assert!(result.status.success(), "{}", format_output(&result));

    let bits = read("widthBits.eightPerByte");
    let other = "(case else (h_steps2 h_steps3 h_steps4) ";
    assert!(bits.contains("(proof (split widthBits "), "{bits}");
    let at = bits.find(other).expect("the catch-all case");
    let body_at = at + other.len();
    let body = &bits[body_at..=closing(&bits, body_at)];
    let proof_at = bits.find("(proof ").unwrap();
    let mutants = [
        // The catch-all case without the hypothesis that rules out 78.
        bits.replacen(other, "(case else (h_steps2 h_steps3) ", 1),
        // The first literal case read as a second catch-all.
        bits.replacen("(case lit () ", "(case else () ", 1),
        // The catch-all arm chosen with no literal arm ruled out.
        format!("{}(proof {body}))", &bits[..proof_at]),
    ];
    for (i, mutant) in mutants.iter().enumerate() {
        assert_ne!(*mutant, bits, "mutant {i} changed nothing");
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(
            verdict
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "mutant {i}: {verdict:?}\n{mutant}"
        );
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_a_split_on_the_arms_of_a_match() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("match-arms-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "match_arms.av",
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
    for law in MATCH_ARMS_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const CONSTRUCTOR_LAWS: [&str; 3] = [
    "refusal.longSilenceIsRefused",
    "verdictOf.preservesFlag",
    "verdictOf.sameVerdict",
];

/// `==` and `!=` between two different constructors of one type compute,
/// whatever the constructors hold; the kernel refuses a pair it cannot tell
/// apart by name alone: one spelled without its type, two of different
/// types, and one constructor with fields on both sides.
#[test]
fn both_kernels_tell_constructors_apart_and_refuse_mutations() {
    let out = scratch("constructors");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("constructors.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), CONSTRUCTOR_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let flag = read("verdictOf.preservesFlag");
    let silence = read("refusal.longSilenceIsRefused");
    let compute = "(compute (op == (ctor Verdict.Invalid) (ctor Verdict.Valid)) (b false))";
    assert!(flag.contains(compute), "{flag}");
    assert!(
        silence.contains("(compute (op != (ctor Option.Some "),
        "{silence}"
    );
    let mutants = [
        // One constructor spelled without its type, everywhere it is written.
        flag.replace("Verdict.Invalid", "Invalid"),
        // The two constructors belong to different types.
        flag.replace("Verdict.Invalid", "Other.Invalid"),
        // The wrong value.
        flag.replace(compute, &compute.replace("(b false)", "(b true)")),
        // The same constructor on both sides, holding something.
        silence.replace("(ctor Option.None)", "(ctor Option.Some (i 0))"),
    ];
    for (i, mutant) in mutants.iter().enumerate() {
        assert!(
            mutant != &flag && mutant != &silence,
            "mutant {i} changed nothing"
        );
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(
            verdict
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "mutant {i}: {verdict:?}\n{mutant}"
        );
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_tells_constructors_apart_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("constructors-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "constructors.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "1",
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
    for law in CONSTRUCTOR_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const SHAPES_LAWS: [&str; 8] = [
    "fill.oneStep",
    "implied.always",
    "offBy.otherSize",
    "readAll.lowDigitStep",
    "readDigits.finalDigit",
    "remainderFits.always",
    "same.reflexive",
    "under.belowCap",
];

/// A list literal read as cells, a countdown under the complement of its
/// guard, a constant in a `when`, a comparison inside `Bool.or`, a value
/// equal to itself and a remainder's range: the kernel accepts each script,
/// and refuses a remainder range at the wrong divisor and a reflexive
/// equality between two different values.
#[test]
fn both_kernels_check_the_btc_shapes_and_refuse_mutations() {
    let out = scratch("btc-shapes");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("btc_shapes.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), SHAPES_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let cells = read("readDigits.finalDigit");
    assert!(cells.contains("(cell "), "{cells}");
    let mutants = [
        (
            read("remainderFits.always"),
            "(rule int.mod_range ((a (v a)) (k (i 256)))",
            "(rule int.mod_range ((a (v a)) (k (i 255)))",
        ),
        (
            read("same.reflexive"),
            "(rule bool.beq.refl ((a (v p))))",
            "(rule bool.beq.refl ((a (v q))))",
        ),
    ];
    for (i, (text, from, to)) in mutants.iter().enumerate() {
        assert!(text.contains(from), "{text}");
        let mutant = mutate_proof(text, from, to);
        let verdict = aver::proof_kernel::verdict(&mutant);
        assert!(
            verdict
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{verdict:?}\n{mutant}"
        );
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, &mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_the_btc_shapes_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("btc-shapes-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "btc_shapes.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
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
    for law in SHAPES_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const ARITH_LAWS: [&str; 6] = [
    "clamp.positiveStaysPositive",
    "next.staysAboveOne",
    "overshoots.oppositeRemainder",
    "sqSum.expands",
    "sumTR.isAccPlusTotal",
    "twice.isDouble",
];

#[test]
fn both_kernels_check_ring_and_linear_steps_and_refuse_mutations() {
    let out = scratch("arith");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("arith.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), ARITH_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let next = read("next.staysAboveOne");
    let square = read("sqSum.expands");
    assert!(next.contains("(linear "), "{next}");
    assert!(square.contains("(ring "), "{square}");
    for (kind, text) in [
        (
            "a weight that does not add up",
            mutate_proof(&next, "true (when) (1 1)", "true (when) (1 2)"),
        ),
        (
            "a negative weight",
            mutate_proof(&next, "true (when) (1 1)", "true (when) (1 -1)"),
        ),
        (
            "the opposite value",
            mutate_proof(&next, "true (when) (1 1)", "false (when) (1 1)"),
        ),
        (
            "a different polynomial",
            square.replacen("(op * (i 2) (v a))", "(op * (i 3) (v a))", 2),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}");
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
fn joining_texts_is_never_read_as_int_arithmetic() {
    // `+` on texts does not commute: written as `++`, no Int rule or ring
    // step applies to it, even in a script made by hand.
    let script = |op: &str| {
        format!(
            "(steps 13 (obligation k (a b) (none) (op {op} (v a) (v b)) (op {op} (v b) (v a))) (defs) (consts) (sums) (laws) (proof (ring (op {op} (v a) (v b)) (op {op} (v b) (v a)))))"
        )
    };
    assert_eq!(
        aver::proof_kernel::verdict(&script("+")),
        Ok("k".to_string())
    );
    assert!(aver::proof_kernel::verdict(&script("++")).is_err());
    assert!(aver::proof_kernel::verdict(&script("+.")).is_err());
}

#[test]
fn lean_checks_ring_and_linear_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("arith-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "arith.av",
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
    for law in ARITH_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Arith.lean")).unwrap();
    let law = "sqSum.expands";
    let line = exact_line(&lean, "sqSum_law_expands").to_string();
    let wrong = line.replacen("Expr.num (2)", "Expr.num (3)", 1);
    assert_ne!(line, wrong, "{line}");
    assert!(
        lean_refuses(&out, "Arith.lean", &lean.replacen(&line, &wrong, 1), law),
        "a different polynomial: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

const LIST_LAWS: [&str; 9] = [
    "oneIfPositive.isLenOfTakeOfOne",
    "pushed.dropNothing",
    "pushed.dropOneMore",
    "pushed.growsByOne",
    "pushed.joinsInFront",
    "pushed.literalWithVariables",
    "pushed.reversedEndsWithHead",
    "pushed.takeKeepsHead",
    "pushed.takeNothing",
];

#[test]
fn both_kernels_check_the_list_rules_and_refuse_mutations() {
    let out = scratch("lists");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("lists.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), LIST_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "a positive count read as none",
            swap(
                "pushed.takeKeepsHead",
                "list.take.cons_gt",
                "list.take.cons_le",
            ),
        ),
        (
            "no count read as a positive one",
            swap(
                "pushed.dropNothing",
                "list.drop.cons_le",
                "list.drop.cons_gt",
            ),
        ),
        (
            "the empty list's length for a longer one",
            swap(
                "pushed.growsByOne",
                "(rule list.len.cons ((x (v x)) (a (v xs))))",
                "(rule list.len.nil ())",
            ),
        ),
        (
            "the head joined at the wrong end",
            swap("pushed.joinsInFront", "list.concat.cons", "list.concat.nil"),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}");
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
fn lean_checks_the_list_rules_and_refuses_a_mutation() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("lists-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "lists.av",
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
    for law in LIST_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Lists.lean")).unwrap();
    let law = "pushed.takeKeepsHead";
    let line = exact_line(&lean, "pushed_law_takeKeepsHead").to_string();
    let wrong = line.replacen(
        "AverSteps.list_take_cons_gt",
        "AverSteps.list_take_cons_le",
        1,
    );
    assert_ne!(line, wrong, "{line}");
    assert!(
        lean_refuses(&out, "Lists.lean", &lean.replacen(&line, &wrong, 1), law),
        "a positive count read as none: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

const FACT_LAWS: [&str; 15] = [
    "batch.sizeAdds",
    "batch.threeBatches",
    "joined.droppingShortens",
    "joined.dropsNothingBelowOne",
    "joined.dropsTheFront",
    "joined.emptyOnTheRight",
    "joined.lengthAdds",
    "joined.regroups",
    "joined.splitsAnywhere",
    "joined.takesNothingBelowOne",
    "joined.takesTheFront",
    "reversed.keepsTheLength",
    "reversed.lengthIsNeverNegative",
    "reversed.ofJoined",
    "reversed.twiceIsTheSame",
];

#[test]
fn a_cited_builtin_fact_is_checked_with_the_law_and_refused_when_mutated() {
    let out = scratch("facts");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("facts.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), FACT_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        let text = read(law);
        assert!(text.contains("\n  (fact List."), "{text}");
        assert_eq!(aver::proof_kernel::verdict(&text), Ok(law.clone()));
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "a wrong step inside the fact's proof",
            swap(
                "joined.lengthAdds",
                "(symm (rule list.len.nil ()))",
                "(rule list.len.nil ())",
            ),
        ),
        (
            "a fact stated with a different right side",
            swap(
                "joined.lengthAdds",
                "(fact List.len.ofConcat ((a (tlist)) (b (tlist))) (none) (bi List.len (bi List.concat (v a) (v b))) (op + (bi List.len (v a)) (bi List.len (v b)))",
                "(fact List.len.ofConcat ((a (tlist)) (b (tlist))) (none) (bi List.len (bi List.concat (v a) (v b))) (op + (bi List.len (v a)) (bi List.len (v a)))",
            ),
        ),
        (
            "a fact whose induction is on a given not declared a list",
            swap(
                "joined.regroups",
                "(fact List.concat.assoc ((a (tlist))",
                "(fact List.concat.assoc (a",
            ),
        ),
        (
            "a fact's induction hypothesis at the count it splits, not one less",
            swap(
                "joined.splitsAnywhere",
                "(x t) (n) ((ih ((op - (v n) (i 1))) ()))",
                "(x t) (n) ((ih ((v n)) ()))",
            ),
        ),
        (
            "a fact on a count stated without its when",
            swap(
                "joined.takesNothingBelowOne",
                "(fact List.take.nonPositive ((l (tlist)) n) (op <= (v n) (i 0))",
                "(fact List.take.nonPositive ((l (tlist)) n) (none)",
            ),
        ),
        ("a fact cited before a fact its proof cites", {
            // Move `List.concat.rightIdentity` after the fact that cites it.
            let text = read("reversed.twiceIsTheSame");
            let from = text.find("\n  (fact List.concat.rightIdentity ").unwrap();
            let to = text[from + 1..].find("\n  (fact ").unwrap() + from + 1;
            let entry = text[from..to].to_string();
            let rest = format!("{}{}", &text[..from], &text[to..]);
            let at = rest.find("\n  (fact List.reverse.involutive ").unwrap();
            format!("{}{entry}{}", &rest[..at], &rest[at..])
        }),
        ("a fact cited without its proof", {
            let text = read("joined.regroups");
            let from = text.find("\n  (fact ").unwrap();
            let to = text[from..].find("\n (proof ").unwrap() + from;
            format!("{}){}", &text[..from], &text[to..])
        }),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}\n{text}");
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
fn a_using_name_in_the_list_namespace_must_be_a_builtin_fact() {
    let dir = scratch("facts-reserved");
    fs::create_dir_all(&dir).unwrap();
    fs::write(
        dir.join("reserved.av"),
        "module Reserved\n    intent = \"A law citing a fact that does not exist.\"\n    exposes [joined]\n    effects []\n\nfn joined(xs: List<Int>, ys: List<Int>) -> List<Int>\n    ? \"Both lists.\"\n    List.concat(xs, ys)\n\nverify joined law lengthAdds\n    given xs: List<Int> = [[], [1]]\n    given ys: List<Int> = [[], [2]]\n    using [List.len.ofJoin]\n    List.len(joined(xs, ys)) => List.len(xs) + List.len(ys)\n",
    )
    .unwrap();
    let result = aver_in(&dir, &["check", "reserved.av"]);
    let text = format_output(&result);
    assert!(!result.status.success(), "{text}");
    assert!(
        text.contains("'List.len.ofJoin', which is not a builtin fact"),
        "{text}"
    );
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn lean_states_each_cited_fact_once_for_every_element_type() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("facts-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "facts.av",
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
    for law in FACT_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let common = fs::read_to_string(out.join("AverCommon.lean")).unwrap();
    for theorem in [
        "theorem AverFacts.list_len_ofConcat {α : Type} (a : List α) (b : List α) :",
        "theorem AverFacts.list_concat_assoc {α : Type} (a : List α) (b : List α) (c : List α) :",
    ] {
        assert_eq!(common.matches(theorem).count(), 1, "{common}");
    }
    assert!(!common.contains("List Unit"), "{common}");
    // A wrong step in a fact is a build error of the shared file: there is
    // no fallback for it.
    let wrong = common.replacen(
        "AverSteps.list_concat_cons (x) (t) (b)",
        "AverSteps.list_concat_nil (b)",
        1,
    );
    assert_ne!(wrong, common);
    fs::write(out.join("AverCommon.lean"), &wrong).unwrap();
    let built = Command::new("lake")
        .args(["env", "lean", "AverCommon.lean"])
        .current_dir(&out)
        .output()
        .expect("lake runs");
    assert!(!built.status.success(), "{}", format_output(&built));
    let _ = fs::remove_dir_all(out);
}

/// A law that does not cite the fact that would close it stays open, and
/// the report names the fact and the part of the stuck term it rewrites:
/// in `--backend aver` text and JSON, and in the exported `.refused` file
/// the Lean check reads.
#[test]
fn a_stuck_law_is_hinted_the_facts_that_rewrite_where_it_stopped() {
    let dir = repo_root().join(FIXTURES);
    let text = aver_in(&dir, &["proof", "hints.av", "--backend", "aver"]);
    let out = String::from_utf8_lossy(&text.stdout).to_string();
    assert!(
        out.contains("0 of 11 law(s) closed by steps"),
        "{}",
        format_output(&text)
    );
    assert!(
        out.contains("  joined.lengthAdds: not closed by this backend (steps: evaluation stops at `List.len(List.concat(xs, ys))`")
            && out.contains("    hint: `List.len.ofConcat` rewrites `List.len(List.concat(xs, ys))`; add it to `using`"),
        "{out}"
    );
    let json = aver_in(
        &dir,
        &["proof", "hints.av", "--backend", "aver", "--check-json"],
    );
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&json.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(
        summary["steps_hints"]["reversed.twiceIsTheSame"],
        serde_json::json!([
            "`List.reverse.involutive` rewrites `List.reverse(List.reverse(xs))`; add it to `using`"
        ]),
        "{summary}"
    );
    assert_eq!(summary["closed_by"]["reversed.twiceIsTheSame"], "open");
    let export = scratch("hints");
    let files = export_steps("hints.av", &export);
    assert!(files.is_empty(), "{files:?}");
    let refused =
        fs::read_to_string(export.join("proof_steps/batch.threeBatches.refused")).unwrap();
    assert!(
        refused.lines().nth(1)
            == Some(
                "hint: `List.concat.assoc` rewrites `List.concat(List.concat(a, b), c)`; add it to `using`"
            ),
        "{refused}"
    );
    let _ = fs::remove_dir_all(export);
}

#[test]
fn aver_facts_lists_the_facts_with_their_statements() {
    let dir = repo_root();
    let text = aver_in(&dir, &["facts", "List.len"]);
    let out = String::from_utf8_lossy(&text.stdout).to_string();
    assert!(text.status.success(), "{}", format_output(&text));
    assert!(
        out.contains("List.len.ofConcat\n    given a, b\n    List.len(List.concat(a, b)) => List.len(a) + List.len(b)\n"),
        "{out}"
    );
    assert!(!out.contains("List.concat.assoc"), "{out}");
    let json = aver_in(&dir, &["facts", "--json"]);
    let list: serde_json::Value =
        serde_json::from_str(String::from_utf8_lossy(&json.stdout).trim()).unwrap();
    let names: Vec<&str> = list
        .as_array()
        .unwrap()
        .iter()
        .map(|f| f["name"].as_str().unwrap())
        .collect();
    assert!(names.contains(&"List.reverse.involutive"), "{names:?}");
    let involutive = list
        .as_array()
        .unwrap()
        .iter()
        .find(|f| f["name"] == "List.reverse.involutive")
        .unwrap();
    assert_eq!(
        involutive["cites"],
        serde_json::json!(["List.reverse.ofConcat"])
    );
}

const SPLIT_LAWS: [&str; 3] = ["double.keepsPositive", "ins.growsByOne", "minus.minusSelf"];

/// Induction along a function whose arm splits again around its recursive
/// call (a `match` on a comparison, a `match` on another parameter), and
/// under a `when` about the given the induction varies, which every case
/// carries. Both kernels accept the step scripts, and the kernel written in
/// Aver refuses a hypothesis taken without proving the carried `when` at
/// the call, one taken on a wrong proof of it, and one the case uses after
/// doing without it.
#[test]
fn both_kernels_induct_through_a_split_arm_and_under_a_carried_when() {
    let out = scratch("split-arm");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("induction_split.av", &out)
            .into_iter()
            .collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), SPLIT_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let law = read("double.keepsPositive");
    assert!(law.contains(" (carry when) "), "{law}");
    // The hypothesis of the cell case with the proof of the carried `when`
    // at its call: `(ih1 PROOF)`.
    let ih_open = law.find("((ih1 ").expect("a carried hypothesis") + 1;
    let ih_close = closing(&law, ih_open);
    let (proof_from, proof_to) = sub_forms(&law, ih_open)[0];
    for (kind, text) in [
        (
            "the hypothesis without the carried proof",
            format!("{}ih1{}", &law[..ih_open], &law[ih_close + 1..]),
        ),
        ("nothing carried", law.replacen(" (carry when)", "", 1)),
        (
            "a wrong proof of the carried `when`",
            format!("{}(hyp when){}", &law[..proof_from], &law[proof_to..]),
        ),
        (
            "the hypothesis done without but used",
            format!("{}(_{}", &law[..ih_open], &law[ih_open + 4..]),
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
fn lean_inducts_through_a_split_arm_and_under_a_carried_when() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("split-arm-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction_split.av",
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
    for law in SPLIT_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("InductionSplit.lean")).unwrap();
    let line = exact_line(&lean, "double_law_keepsPositive").to_string();
    // The recursor's motive is the claim under the carried `when`, and the
    // recursor is applied to the `when` itself at the end.
    let carried = " xs (show (allPos xs : Bool) = (true : Bool) from h_when))";
    assert!(
        line.contains("List.rec (motive := fun xs => (allPos xs : Bool) = (true : Bool) → ")
            && line.contains(carried),
        "{line}"
    );
    let unapplied = line.replacen(carried, " xs)", 1);
    assert!(
        lean_refuses(
            &out,
            "InductionSplit.lean",
            &lean.replacen(&line, &unapplied, 1),
            "double.keepsPositive"
        ),
        "the `when` left unapplied: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

const MAP_LAWS: [&str; 8] = [
    "count.otherWordsUnchanged",
    "place.otherPointsUnchanged",
    "place.readsBackAtAPoint",
    "put.holdsTheKey",
    "put.leavesOtherKeys",
    "put.readsBack",
    "put.sizeOfOneEntry",
    "put.sizeWhenPresent",
];

#[test]
fn both_kernels_check_the_map_rules_and_refuse_mutations() {
    let out = scratch("maps");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("maps.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), MAP_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "another key read as the key just set",
            swap(
                "put.leavesOtherKeys",
                "(rule map.get.set_other",
                "(rule map.get.set_same",
            ),
        ),
        (
            "the key just set read as another",
            swap(
                "put.readsBack",
                "(rule map.get.set_same",
                "(rule map.get.set_other",
            ),
        ),
        (
            "a held key counted as new",
            swap(
                "put.sizeWhenPresent",
                "(rule map.len.set_present",
                "(rule map.len.set_absent",
            ),
        ),
        (
            "a new key counted as held",
            swap(
                "put.sizeOfOneEntry",
                "(rule map.len.set_absent",
                "(rule map.len.set_present",
            ),
        ),
        (
            "membership read from the map before the set",
            swap(
                "count.otherWordsUnchanged",
                "(rule map.has.set_other",
                "(rule map.has.set_same",
            ),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}");
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
fn lean_checks_the_map_rules_over_int_string_and_record_keys() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("maps-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "maps.av",
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
    for law in MAP_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Maps.lean")).unwrap();
    let law = "place.otherPointsUnchanged";
    let line = exact_line(&lean, "place_law_otherPointsUnchanged").to_string();
    let wrong = line.replacen("AverMap.step_get_set_other", "AverMap.step_get_set_same", 1);
    assert_ne!(line, wrong, "{line}");
    assert!(
        lean_refuses(&out, "Maps.lean", &lean.replacen(&line, &wrong, 1), law),
        "another key read as the key just set: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

const MAP_FACT_LAWS: [&str; 5] = [
    "place.otherPointsUnchanged",
    "store.holdsTheKey",
    "store.keepsTheSize",
    "store.leavesOtherKeys",
    "store.readsBack",
];

#[test]
fn a_cited_map_fact_is_checked_with_its_when_and_refused_when_mutated() {
    let out = scratch("map-facts");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("map_facts.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), MAP_FACT_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        let text = read(law);
        assert!(text.contains("\n  (fact Map."), "{text}");
        assert_eq!(aver::proof_kernel::verdict(&text), Ok(law.clone()));
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "a fact stated without its when",
            swap(
                "store.leavesOtherKeys",
                "(fact Map.get.afterSetOther (m k v k2) (op != (v k) (v k2))",
                "(fact Map.get.afterSetOther (m k v k2) (none)",
            ),
        ),
        (
            "a fact proved by the rule for the key just set",
            swap(
                "store.leavesOtherKeys",
                "(rule map.get.set_other",
                "(rule map.get.set_same",
            ),
        ),
        (
            "a size fact proved for a key the map does not hold",
            swap(
                "store.keepsTheSize",
                "(rule map.len.set_present",
                "(rule map.len.set_absent",
            ),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}\n{text}");
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
fn a_key_order_fact_about_maps_is_not_a_builtin_fact() {
    let dir = scratch("map-key-order");
    fs::create_dir_all(&dir).unwrap();
    fs::write(
        dir.join("keys.av"),
        "module Keys\n    intent = \"A law citing a fact about the order of keys.\"\n    exposes [store]\n    effects []\n\nfn store(m: Map<Int, Int>, k: Int) -> Map<Int, Int>\n    ? \"Set one entry.\"\n    Map.set(m, k, 0)\n\nverify store law keysGrow\n    given m: Map<Int, Int> = [{1 => 1}]\n    given k: Int = [2]\n    using [Map.keys.afterSet]\n    List.len(Map.keys(store(m, k))) >= List.len(Map.keys(m)) => true\n",
    )
    .unwrap();
    let result = aver_in(&dir, &["check", "keys.av"]);
    let text = format_output(&result);
    assert!(!result.status.success(), "{text}");
    assert!(
        text.contains("'Map.keys.afterSet', which is not a builtin fact"),
        "{text}"
    );
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn lean_states_each_cited_map_fact_once_for_every_key_and_value_type() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("map-facts-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "map_facts.av",
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
    for law in MAP_FACT_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let common = fs::read_to_string(out.join("AverCommon.lean")).unwrap();
    assert_eq!(
        common
            .matches("theorem AverFacts.map_get_afterSetOther {α β : Type} [DecidableEq α] [AverKeyOrder α] [BEq α] [LawfulBEq α] (m : List (α × β)) (k : α) (v : β) (k2 : α) (h_when :")
            .count(),
        1,
        "{common}"
    );
    let _ = fs::remove_dir_all(out);
}

/// A step into an arm of a `match` on a comparison: the model writes the
/// comparison as a Prop, so the arm's Bool premise reaches it through
/// `decide`. The nested `match sample < -32768` used to be refused by Lean.
#[test]
fn lean_takes_an_arm_of_a_nested_match_on_a_comparison() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("nested-comparison");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "nested_comparison.av",
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
    assert_eq!(
        summary["steps_rejected"],
        serde_json::json!([]),
        "{summary}"
    );
    assert_eq!(
        summary["closed_by"]["clamp16.fitsSixteenBits"], "steps",
        "{summary}"
    );
    let _ = fs::remove_dir_all(out);
}

const VECTOR_LAWS: [&str; 10] = [
    "cells.keepTheList",
    "cells.lengthOfTheList",
    "cells.literalSize",
    "cells.literalSizeRead",
    "write.keepsTheLength",
    "write.leavesOtherIndices",
    "write.noWriteOutOfRange",
    "write.nothingBelowZero",
    "write.nothingPastTheEnd",
    "write.readsBack",
];

#[test]
fn both_kernels_check_the_vector_rules_and_refuse_mutations() {
    let out = scratch("vectors");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("vectors.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), VECTOR_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "a read past the end taken as below zero",
            swap(
                "write.nothingPastTheEnd",
                "(rule vector.get.past_end",
                "(rule vector.get.negative",
            ),
        ),
        (
            "a read after a write taken as a read below zero",
            swap(
                "write.readsBack",
                "(rule vector.get.set_same",
                "(rule vector.get.negative",
            ),
        ),
        (
            "the length of a new vector taken as after a write",
            swap(
                "cells.literalSize",
                "(rule vector.len.new",
                "(rule vector.len.set",
            ),
        ),
        (
            "a write in range taken as out of range",
            swap(
                "write.noWriteOutOfRange",
                "(rule vector.set.out_of_range",
                "(rule vector.get.past_end",
            ),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}");
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
fn lean_checks_the_vector_rules_and_refuses_a_mutation() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("vectors-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "vectors.av",
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
    for law in VECTOR_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Vectors.lean")).unwrap();
    let law = "write.nothingPastTheEnd";
    let line = exact_line(&lean, "write_law_nothingPastTheEnd").to_string();
    let wrong = line.replacen(
        "AverSteps.vector_get_past_end",
        "AverSteps.vector_get_negative",
        1,
    );
    assert_ne!(line, wrong, "{line}");
    assert!(
        lean_refuses(&out, "Vectors.lean", &lean.replacen(&line, &wrong, 1), law),
        "a read past the end taken as below zero: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

const VECTOR_FACT_LAWS: [&str; 4] = [
    "grid.listBack",
    "grid.nothingPastTheEnd",
    "grid.otherCellKept",
    "grid.sizeOfTheList",
];

#[test]
fn a_cited_vector_fact_is_checked_and_refused_when_mutated() {
    let out = scratch("vector-facts");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("vector_facts.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), VECTOR_FACT_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        let text = read(law);
        assert!(text.contains("\n  (fact Vector."), "{text}");
        assert_eq!(aver::proof_kernel::verdict(&text), Ok(law.clone()));
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "the index condition read off the wrong conjunct",
            swap(
                "grid.otherCellKept",
                "(rule bool.and.elim_l",
                "(rule bool.and.elim_r",
            ),
        ),
        (
            "a read past the end proved by the rule below zero",
            swap(
                "grid.nothingPastTheEnd",
                "(rule vector.get.past_end",
                "(rule vector.get.negative",
            ),
        ),
        (
            "the length of a list's vector without reading the list",
            swap(
                "grid.sizeOfTheList",
                "(rule vector.to_list.of_list",
                "(rule vector.of_list.to_list",
            ),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}");
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
fn lean_states_each_cited_vector_fact_once_for_every_element_type() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("vector-facts-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "vector_facts.av",
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
    for law in VECTOR_FACT_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let common = fs::read_to_string(out.join("AverCommon.lean")).unwrap();
    assert_eq!(
        common
            .matches(
                "theorem AverFacts.vector_get_pastEnd {α : Type} (v : Array α) (i : Int) (h_when :"
            )
            .count(),
        1,
        "{common}"
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

#[test]
fn both_kernels_check_because_chains_and_refuse_mutations() {
    let out = scratch("because");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("because.av", &out).into_iter().collect();
    assert_eq!(
        files.keys().cloned().collect::<Vec<_>>(),
        [
            "bothPositive.fromBoth",
            "bothPositive.shifted",
            "count.doubledNonNegative",
            "count.doubledNonNegative.because1",
            "count.doubledNonNegative.implication",
            "count.nonNegative",
            "far.outsideFive",
            "mix.commutes",
            "mix.shiftedCommutes",
            "sum2.staysPositive",
            "twice.aboveAtLeastOne",
            "twice.grows",
            "twice.grows.because1",
            "twice.grows.because2",
            "twice.grows.implication",
            "twice.needsMore.implication",
        ]
    );
    // A reason no producer proves names itself, and the law keeps its
    // tactics.
    let refused =
        fs::read_to_string(out.join("proof_steps").join("twice.needsMore.refused")).unwrap();
    assert!(refused.starts_with("reason 1 of 1: "), "{refused}");
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let doubled = read("count.doubledNonNegative");
    let positive = read("sum2.staysPositive");
    // The cited law's two-line `when`, one conjunct at a time: a linear
    // step for `x - 1 > 0`, a computation for `1 > 0`.
    let shifted = read("bothPositive.shifted");
    assert!(shifted.contains("(rule bool.and.true_l "), "{shifted}");
    assert!(doubled.contains("(have because1 "), "{doubled}");
    // A `when` that calls a predicate, opened where a linear step reads
    // the comparison inside it.
    let opened = read("twice.aboveAtLeastOne");
    assert!(
        opened.contains("(have h_steps1 (op >= (v x) (i 1)) "),
        "{opened}"
    );
    // An obligation closes on its own, stating what it assumes: the
    // claim of `needsMore` from its reason, which stays open.
    let assumed = read("twice.needsMore.implication");
    assert!(
        assumed.contains("(bi Bool.and (op > (v x) (i 5)) (op > (v x) (i 0)))"),
        "{assumed}"
    );
    // One instance of a cited law that would loop as a rewrite rule, its
    // sides matched up to the ring.
    let comm = read("mix.shiftedCommutes");
    assert!(
        comm.contains("(law mix.commutes ") && comm.contains("(ring "),
        "{comm}"
    );
    // A disjunction in the `when`: where its left side is false, the right
    // side is cut in, and a branch the comparisons rule out is absurd.
    let far = read("far.outsideFive");
    assert!(
        far.contains("(rule bool.or.false_l ") && far.contains("(absurd "),
        "{far}"
    );
    assert!(positive.contains("(have when1 "), "{positive}");
    for (kind, text) in [
        (
            "a reason stated as another fact",
            mutate_proof(
                &doubled,
                "(have because1 (op >= (call count (v n)) (i 0))",
                "(have because1 (op >= (call count (v n)) (i 1))",
            ),
        ),
        (
            "a reason read before its cut",
            mutate_proof(&doubled, "(have because1 ", "(have because2 "),
        ),
        (
            "the conjunction put back together by the wrong rule",
            mutate_proof(&shifted, "(rule bool.and.true_l ", "(rule bool.and.true_r "),
        ),
        (
            "a conjunct settled with a weight that does not add up",
            mutate_proof(&shifted, "true (when) (1 1)", "true (when) (1 2)"),
        ),
        (
            "a hypothesis opened to another body",
            mutate_proof(
                &opened,
                "(have h_steps1 (op >= (v x) (i 1))",
                "(have h_steps1 (op >= (v x) (i 0))",
            ),
        ),
        (
            "an obligation that assumes less than it uses",
            assumed.replacen(
                "(bi Bool.and (op > (v x) (i 5)) (op > (v x) (i 0)))",
                "(op > (v x) (i 0))",
                1,
            ),
        ),
        (
            "a cited law instance at other givens",
            mutate_proof(&comm, "(law mix.commutes ((a ", "(law mix.commutes ((b "),
        ),
        (
            "the other side of the disjunction",
            mutate_proof(&far, "(rule bool.or.false_l ", "(rule bool.or.false_r "),
        ),
        (
            "the other line of the when",
            mutate_proof(&positive, "(rule bool.and.elim_l", "(rule bool.and.elim_r"),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}");
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
fn lean_checks_each_obligation_of_a_because_chain_by_its_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("because-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "because.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "10",
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
    for obligation in [
        "far.outsideFive",
        "mix.shiftedCommutes",
        "mix.shiftedCommutes.implication",
        "twice.aboveAtLeastOne",
        "twice.needsMore.implication",
        "bothPositive.fromBoth",
        "bothPositive.shifted",
        "count.doubledNonNegative",
        "count.doubledNonNegative.because1",
        "count.doubledNonNegative.implication",
        "count.nonNegative",
        "sum2.staysPositive",
        "twice.aboveAtLeastOne",
        "twice.grows",
        "twice.grows.because1",
        "twice.grows.because2",
        "twice.grows.implication",
    ] {
        assert_eq!(
            summary["closed_by"][obligation], "steps",
            "{obligation}: {summary}"
        );
    }
    assert_eq!(
        summary["steps_rejected"],
        serde_json::json!([]),
        "{summary}"
    );
    // The implication reads the reason as the hypothesis its theorem
    // introduces; a cut that states another fact does not elaborate.
    let lean = fs::read_to_string(out.join("Because.lean")).unwrap();
    let start = lean
        .find("theorem __aver_reason_count_law_doubledNonNegative_implication :")
        .expect("the implication");
    let line = lean[start..]
        .lines()
        .find(|l| l.contains("have because1 :"))
        .expect("the steps branch")
        .to_string();
    let wrong = line.replace(
        "have because1 : ((count n >= 0)",
        "have because1 : ((count n >= 1)",
    );
    assert_ne!(line, wrong, "{line}");
    assert!(
        lean_refuses(
            &out,
            "Because.lean",
            &lean.replacen(&line, &wrong, 1),
            "count.doubledNonNegative"
        ),
        "a reason stated as another fact: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}
