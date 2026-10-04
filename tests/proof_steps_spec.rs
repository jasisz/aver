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
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(bytes_out);
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
