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
/// The step scripts of a fixture, each with its shared parts written out
/// (`aver::ir::proof_steps::shared::unshare`), so a test can mutate one
/// place of a script; the kernel reads either text the same.
fn export_steps(fixture: &str, out: &Path) -> Vec<(String, PathBuf)> {
    let files = export_shared_steps(fixture, out);
    for (_, p) in &files {
        let text = fs::read_to_string(p).unwrap();
        fs::write(p, aver::ir::proof_steps::shared::unshare(&text).unwrap()).unwrap();
    }
    files
}

/// The step scripts of a fixture as the compiler writes them.
fn export_shared_steps(fixture: &str, out: &Path) -> Vec<(String, PathBuf)> {
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
            "encodeInto.prefixAccumulator",
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
            "wrong ring step",
            mutate_proof(
                &comm,
                "(ring (op + (v a) (v b)) (op + (v b) (v a)))",
                "(ring (op + (v a) (v b)) (op + (v a) (v a)))",
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
        ("wrong premise", first_replaced("(h_steps2)", "(h_steps1)")),
        ("wrong literal", first_replaced("500000000", "500000001")),
        (
            "wrong arm number",
            first_replaced(&format!("{stop} "), &format!("{cont} ")),
        ),
        (
            "hypothesis out of scope",
            first_replaced("(h_steps3)", "(h_steps7)"),
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
        "(show ((a + b)) = ((b + a)) from by omega)",
        "(show ((a + b)) = ((a + a)) from by omega)",
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
        "wrong ring step: Lean must refuse the mutated step term"
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
    let (_, using) = aver_backend_json("using.av", &out, Some(""));
    assert_eq!(using["closed_by"]["g.overlapping"], "open");
    let _ = fs::remove_dir_all(out);
}

/// `trust` says what each law closed by steps stands on: the wall rules
/// its steps apply, including inside the proofs of the facts it cites,
/// whether it uses the kernel's evaluator or a decision procedure, and the
/// laws and facts it cites. A law left open has no entry.
#[test]
fn the_aver_backend_reports_what_each_closed_law_stands_on() {
    let out = scratch("trust");
    let names = |v: &serde_json::Value| -> Vec<String> {
        v.as_array()
            .unwrap()
            .iter()
            .map(|n| n.as_str().unwrap().to_string())
            .collect()
    };
    let (_, lock) = aver_backend_json("lock.av", &out, None);
    let commutes = &lock["trust"]["add.commutes"];
    assert!(names(&commutes["rules"]).is_empty(), "{lock}");
    assert_eq!(commutes["ring"], true, "{lock}");
    for procedure in ["compute", "linear"] {
        assert_eq!(commutes[procedure], false, "{lock}");
    }
    let sequence = &lock["trust"]["sequenceChecked.agreesWithSpec"];
    assert_eq!(sequence["compute"], true, "{lock}");
    assert_eq!(sequence["linear"], true, "{lock}");
    assert!(names(&sequence["rules"]).contains(&"bits.and_bit".to_string()));
    let (_, facts) = aver_backend_json("facts.av", &out, None);
    let batches = &facts["trust"]["batch.threeBatches"];
    assert_eq!(names(&batches["facts"]), ["List.concat.assoc"], "{facts}");
    assert_eq!(
        names(&batches["rules"]),
        ["list.concat.cons", "list.concat.nil"],
        "{facts}"
    );
    assert_eq!(names(&batches["laws"]), Vec::<String>::new());
    let (_, using) = aver_backend_json("using.av", &out, None);
    assert_eq!(
        names(&using["trust"]["sameLen.againstOne"]["laws"]),
        ["sameLen.commutes"],
        "{using}"
    );
    assert_eq!(using["closed_by"]["g.overlapping"], "open");
    assert!(using["trust"].get("g.overlapping").is_none(), "{using}");
    let _ = fs::remove_dir_all(out);
}

/// A script as one line with its cited laws ahead of its cited facts, the
/// order the kernel keeps them in.
fn canonical(text: &str) -> String {
    use aver::ir::proof_steps::read::{Sx, parse};
    let Sx::List(mut parts) = aver::ir::proof_steps::shared::expand(parse(text).unwrap()).unwrap()
    else {
        panic!("not a script: {text}");
    };
    for part in &mut parts {
        if let Sx::List(xs) = part
            && matches!(xs.first(), Some(Sx::Atom(a)) if a == "laws")
        {
            let is_fact = |x: &Sx| matches!(x, Sx::List(f) if matches!(f.first(), Some(Sx::Atom(a)) if a == "fact"));
            let (facts, laws): (Vec<Sx>, Vec<Sx>) = xs.drain(1..).partition(is_fact);
            xs.extend(laws);
            xs.extend(facts);
        }
    }
    Sx::List(parts).print()
}

/// The kernel reads a script as the compiler meant it: each script,
/// decoded by the kernel (`tools/proof-kernel/kernel/decode.av`) and written
/// out again (`tools/proof-kernel/reprint.av`), is the script the compiler
/// wrote, item for item. Blanks between items are free. The sample is the
/// smallest fixture script with each kind of step, and the scripts of a
/// corpus file.
#[test]
fn the_kernel_reads_back_each_script_as_the_compiler_wrote_it() {
    let out = scratch("round-trip");
    let mut written: Vec<PathBuf> = Vec::new();
    for fixture in [
        "lock.av",
        "facts.av",
        "case_hypotheses.av",
        "binding_names.av",
        "defaults_and_updates.av",
        "induction_on_list.av",
        "finite.av",
        "case_contradictions.av",
        "induction.av",
        "because.av",
        "remainders.av",
        "maps.av",
        "vectors.av",
    ] {
        written.extend(
            export_shared_steps(fixture, &out.join(fixture))
                .into_iter()
                .map(|(_, p)| p),
        );
    }
    written.push(repo_root().join(FIXTURES).join("list_cases.steps"));
    let size = |p: &PathBuf| fs::metadata(p).unwrap().len();
    let mut sample: Vec<PathBuf> = Vec::new();
    let shared = written
        .iter()
        .filter(|p| fs::read_to_string(p).unwrap().contains("(shared ("))
        .min_by_key(|p| size(p))
        .expect("no script names a shared part");
    sample.push(shared.clone());
    for kind in [
        "refl",
        "symm",
        "trans",
        "congr",
        "unfold",
        "const",
        "arm",
        "proj",
        "cell",
        "hyp",
        "rule",
        "law",
        "fact",
        "compute",
        "cases",
        "split",
        "have",
        "enum",
        "absurd",
        "induct",
        "listinduct",
        "listcases",
        "ring",
        "linear",
        "cut",
        "carry",
        "rec",
        "upd",
        "match",
        "sum",
    ] {
        let tag = format!("({kind} ");
        let smallest = written
            .iter()
            .filter(|p| fs::read_to_string(p).unwrap().contains(&tag))
            .min_by_key(|p| size(p))
            .unwrap_or_else(|| panic!("no script writes {tag}"));
        if !sample.contains(smallest) {
            sample.push(smallest.clone());
        }
    }
    let corpus = out.join("corpus");
    let exported = aver_in(
        &repo_root(),
        &[
            "proof",
            "examples/formal/law_auto.av",
            "--module-root",
            "examples/formal",
            "-o",
            corpus.to_str().unwrap(),
            "--verify-mode",
            "sorry",
        ],
    );
    assert!(exported.status.success(), "{}", format_output(&exported));
    let mut from_corpus: Vec<PathBuf> = fs::read_dir(corpus.join("proof_steps"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "steps"))
        .collect();
    from_corpus.sort();
    assert!(!from_corpus.is_empty());
    sample.extend(from_corpus);
    let kernel = repo_root().join(KERNEL);
    let mut args = vec![
        "run".to_string(),
        "roundtrip.av".to_string(),
        "--module-root".to_string(),
        ".".to_string(),
        "--".to_string(),
    ];
    args.extend(sample.iter().map(|f| f.to_string_lossy().to_string()));
    let refs: Vec<&str> = args.iter().map(String::as_str).collect();
    let result = aver_in(&kernel, &refs);
    assert!(result.status.success(), "{}", format_output(&result));
    let stdout = String::from_utf8_lossy(&result.stdout);
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines.len(), 2 * sample.len(), "{}", format_output(&result));
    for (file, pair) in sample.iter().zip(lines.chunks(2)) {
        assert_eq!(pair[0], file.to_string_lossy());
        let wrote = canonical(&fs::read_to_string(file).unwrap());
        assert_eq!(pair[1], wrote, "{}", file.display());
    }
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
            "next.tellsLightsApart",
            "safe.notSafeMeansNextIsAmber",
        ]
    );
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let cycle = read("next.cyclesInThree");
    // Two givens, split one inside the other: no other step closes it
    // (`agree.symmetric` the table of equal terms closes by cases).
    let apart = read("next.tellsLightsApart");
    let safe = read("safe.notSafeMeansNextIsAmber");
    assert!(safe.contains("(absurd "), "{safe}");
    for (kind, text) in [
        ("a missing case", recase(&cycle, &[0, 1])),
        ("two cases swapped", recase(&cycle, &[1, 0, 2])),
        ("a case repeated", recase(&cycle, &[0, 1, 1])),
        (
            "the wrong given",
            mutate_proof(&apart, "(enum b ", "(enum a "),
        ),
        (
            "a split of a given that is not finite",
            apart.replace(
                "((a (named Light)) (b (named Light)))",
                "((a (named Light)) (b _))",
            ),
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

/// A hand-written script: no producer writes a list split yet, but a rule
/// a project ships may. `xs` is split into `[]`, where the `when` read at
/// `[]` is false, and `[hd, ..tl]`, where `isEmpty` takes its second arm.
#[test]
fn both_kernels_split_a_list_given_into_its_two_cases_and_refuse_mutations() {
    let out = scratch("list-cases");
    fs::create_dir_all(&out).unwrap();
    let path = repo_root().join(FIXTURES).join("list_cases.steps");
    let script = fs::read_to_string(&path).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&script),
        Ok("isEmpty.falseWhenLong".to_string())
    );
    let result = replay(std::slice::from_ref(&path));
    assert!(result.status.success(), "{}", format_output(&result));
    let open = script.find("(listcases ").unwrap();
    let forms = sub_forms(&script, open);
    let (nil_from, nil_to) = forms[2];
    let (cons_from, cons_to) = forms[4];
    for (kind, text, why) in [
        (
            "a split of a given that is not a list",
            script.replace("((xs (list int)))", "((xs _))"),
            "? is not List<",
        ),
        (
            "a head named like the variable split",
            script.replace("(hd tl)", "(xs tl)"),
            "the names are not fresh",
        ),
        (
            "a tail named like a hypothesis in scope",
            script.replace("(hd tl)", "(hd when)"),
            "the names are not fresh",
        ),
        (
            "the two cases swapped",
            format!(
                "{}{}{}{}{}",
                &script[..nil_from],
                &script[cons_from..cons_to],
                &script[nil_to..cons_from],
                &script[nil_from..nil_to],
                &script[cons_to..]
            ),
            // The cell's names are not in scope where the list is empty.
            "hd is not a variable in scope",
        ),
        (
            "a missing case",
            format!("{}{}", &script[..cons_from], &script[cons_to..]),
            "malformed listcases",
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(
            refused.as_ref().is_err_and(|e| e.contains(why)),
            "{kind}: {refused:?}"
        );
        let mutant = out.join("mutant.steps");
        fs::write(&mutant, &text).unwrap();
        let result = replay(std::slice::from_ref(&mutant));
        assert!(
            !result.status.success(),
            "{kind}: {}",
            format_output(&result)
        );
    }
    // The tail `t` of the first split no longer occurs in the claim, but it
    // stays a list variable: a later split may not name an element `t`,
    // which a split on `t` would then take for a list.
    let reused = "(steps 16\n (obligation k ((xs (list int))) (none) (b true) (b true))\n (defs)\n (consts)\n (sums)\n (recs)\n (sigs)\n (laws)\n (proof (listcases xs (b true) (b true) (refl (b true)) (h t) (listcases xs (b true) (b true) (refl (b true)) (t u) (refl (b true))))))\n";
    let refused = aver::proof_kernel::verdict(reused);
    assert!(
        refused
            .as_ref()
            .is_err_and(|e| e.contains("the names are not fresh")),
        "a tail's name reused for a head: {refused:?}"
    );
    let mutant = out.join("reused.steps");
    fs::write(&mutant, reused).unwrap();
    let result = replay(std::slice::from_ref(&mutant));
    assert!(!result.status.success(), "{}", format_output(&result));
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
fn a_using_list_is_a_set_overlapping_rewrites_take_the_smallest_and_looping_ones_are_refused() {
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
    // Two cited laws rewrite `f(x)`, to `g(x)` and `h(x)`: the results are
    // as large, so the law defined first wins, whatever order the list
    // names them in.
    assert!(line("g.overlapping").ends_with(" nodes"), "{log}");
    assert!(read("g.overlapping").contains("(law f.isG "), "{log}");
    // Such a law is still one instance: it proves an equation it matches
    // whole, either way round.
    assert!(line("sameLen.againstOne").ends_with(" nodes"), "{log}");
    // Evaluation decides the comparison under `Bool.not` without the law.
    assert!(
        line("sameLen.negatedAgainstOne").ends_with(" nodes"),
        "{log}"
    );
    // A law that rewrites a term into one it applies to again is never a
    // rewrite rule: the claim it would loop on closes by opening the
    // definition, with at most one instance of it.
    assert!(
        line("sameLen.negatedAgainstPadded").ends_with(" nodes"),
        "{log}"
    );
    let padded = read("sameLen.negatedAgainstPadded");
    let padded_proof = &padded[padded.find("\n (proof ").expect("a proof")..];
    assert!(
        padded_proof.matches("(law sameLen.commutes ").count() <= 1,
        "{padded}"
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
        "  g.overlapping: not closed by this backend (it cites f.isG, which is not)",
    ] {
        assert!(text.contains(expected), "missing `{expected}`\n{text}");
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn the_catch_all_case_of_a_covered_int_split_is_absurd_and_both_kernels_check_it() {
    let out = scratch("literal-cover");
    let (result, summary) = aver_backend_json("literal_cover.av", &out, None);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    assert_eq!(
        summary["closed_by"]["width.wideIsPositive"],
        "steps",
        "{}",
        format_output(&result)
    );
    // One value past the literals: the catch-all case can happen.
    assert_eq!(summary["closed_by"]["width.widerIsPositive"], "open");
    let _ = fs::remove_dir_all(&out);
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("literal_cover.av", &out).into_iter().collect();
    let text = fs::read_to_string(&files["width.wideIsPositive"]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&text),
        Ok("width.wideIsPositive".to_string())
    );
    // Each literal in turn, from the smallest: at most it, the value is it.
    assert!(
        text.contains("(cases (op <= (v code) (i 76)) ")
            && text.contains("(cases (op <= (v code) (i 78)) "),
        "{text}"
    );
    let mut paths = Vec::new();
    for (kind, mutant) in [
        (
            "the value read as the next literal",
            mutate_proof(
                &text,
                "(rule int.eq.of_le_ge ((a (v code)) (b (i 77))) ",
                "(rule int.eq.of_le_ge ((a (v code)) (b (i 78))) ",
            ),
        ),
        (
            "the case read as another literal",
            mutate_proof(&text, "(hyp h_steps16)", "(hyp h_steps15)"),
        ),
    ] {
        assert!(aver::proof_kernel::verdict(&mutant).is_err(), "{kind}");
        let path = out.join(format!("mutant{}.steps", paths.len()));
        fs::write(&path, &mutant).unwrap();
        paths.push(path);
    }
    let result = replay(&paths);
    let replayed = String::from_utf8_lossy(&result.stdout);
    for path in &paths {
        assert!(
            replayed.contains(&format!("refused {}: ", path.display())),
            "{}",
            format_output(&result)
        );
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn a_law_that_cites_its_obligations_closes_only_with_what_they_cite() {
    let out = scratch("because-cites-open");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "because_cites_open.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let text = String::from_utf8_lossy(&result.stdout);
    for expected in [
        "  g.sameAsF: not closed by this backend (it cites f.isG, which is not)",
        "    obligation g.sameAsF.because1: open (it cites f.isG, which is not)",
    ] {
        assert!(text.contains(expected), "missing `{expected}`\n{text}");
    }
    let script = fs::read_to_string(out.join("proof_steps").join("g.sameAsF.steps")).unwrap();
    assert!(script.contains("g.sameAsF.because1"), "{script}");
    let _ = fs::remove_dir_all(out);
}

/// The sub-form `index` of the first form opening with `head`.
fn nth_form(text: &str, head: &str, index: usize) -> (usize, usize) {
    let open = text.find(head).unwrap_or_else(|| panic!("no `{head}`"));
    sub_forms(text, open)[index]
}

const INDUCTION_LAWS: [&str; 6] = [
    "app.assoc",
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

/// A refusal at a recursive call steps leave closed says why: a recursion
/// they do not follow names what is wrong and the shapes they do follow; a
/// division down to zero, which they follow, is not called one they do not,
/// in the attempt that keeps it whole.
#[test]
fn a_closed_recursive_call_says_why_it_stays_closed() {
    let out = scratch("closed-recursion");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "closed_recursion.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&result.stdout);
    let line = |law: &str| {
        text.lines()
            .find(|l| l.trim_start().starts_with(&format!("{law}: ")))
            .unwrap_or_else(|| panic!("no line for {law}\n{}", format_output(&result)))
            .to_string()
    };
    let count = line("count.nonnegative");
    assert!(
        count.contains("steps do not open `count`: it recurses on something that is not a part of the value it matches; steps open a recursive function only when its body is one `match` on a parameter"),
        "{count}"
    );
    assert!(
        count.contains("each recursive call passes `n - 1` or `n / k`"),
        "{count}"
    );
    let bytes = line("bytesOf.belowTheValue");
    assert!(
        bytes.contains("with the call that divides down to zero kept whole"),
        "{bytes}"
    );
    assert!(!bytes.contains("steps do not open"), "{bytes}");
    assert!(
        bytes.contains(
            "induction: bytesOf divides down to zero, and this attempt keeps its calls whole"
        ),
        "{bytes}"
    );
    assert!(!bytes.contains("which does not recurse"), "{bytes}");
    assert!(!text.contains("checker limit"), "{text}");
    let _ = fs::remove_dir_all(out);
}

/// A recursion through a large group of helpers is read in bounded time:
/// each helper of `helper_group.av` calls the next four ways, so reading
/// every path of calls back to the function never ends. A pair of helpers
/// that call each other without the function is refused before any body
/// is read, and the chain alone stops at the cap on helper bodies read;
/// both laws stay open with that reason instead of the proof never ending.
#[test]
fn a_recursion_through_a_large_group_of_helpers_ends_with_a_reason() {
    use std::io::Read;
    use std::time::{Duration, Instant};
    let out = scratch("helper-group");
    let mut child = Command::new(aver_bin())
        .args([
            "proof",
            "helper_group.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ])
        .current_dir(repo_root().join(FIXTURES))
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .spawn()
        .expect("aver runs");
    let mut stdout = child.stdout.take().expect("stdout is piped");
    let reader = std::thread::spawn(move || {
        let mut text = String::new();
        let _ = stdout.read_to_string(&mut text);
        text
    });
    let started = Instant::now();
    loop {
        if child.try_wait().expect("aver is waited for").is_some() {
            break;
        }
        if started.elapsed() > Duration::from_secs(120) {
            let _ = child.kill();
            let _ = child.wait();
            panic!("aver proof of helper_group.av ran past two minutes");
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    let text = reader.join().expect("stdout is read");
    let line = |law: &str| {
        text.lines()
            .find(|l| l.trim_start().starts_with(&format!("{law}: ")))
            .unwrap_or_else(|| panic!("no line for {law}\n{text}"))
            .to_string()
    };
    let walk = line("walk.nonNegative");
    assert!(
        walk.contains("walk recurses through spin, which reaches itself without walk"),
        "{walk}"
    );
    let total = line("total.nonNegative");
    assert!(
        total.contains("total recurses through more than 1024 readings of helper bodies"),
        "{total}"
    );
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
    // `double` does not recurse: the law closes by induction on its list
    // given itself, and is told the line.
    assert!(
        text.contains("double.isTwiceTheLength: closed by steps\n    hint: steps closed it by trying each given in turn; add `induction xs` to go straight to it"),
        "{}",
        format_output(&result)
    );
    // A `when` that mentions the list is carried by that induction; the
    // false law of the same shape stays open, and its refusal names the
    // function that does recurse.
    assert!(
        text.contains("double.isTwiceTheLengthWhenShort: closed by steps\n    hint: steps closed it by trying each given in turn; add `induction xs` to go straight to it"),
        "{}",
        format_output(&result)
    );
    assert!(
        text.contains("double does not recurse but len does"),
        "{}",
        format_output(&result)
    );
    // A claim that recurses on two givens of the same function, with no
    // `induction` line: steps try each and hint the one that closed.
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
        text.contains("app.assoc: closed by steps\n    hint: steps closed it by trying each given in turn; add `induction a` to go straight to it"),
        "{}",
        format_output(&both)
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn induction_goes_along_another_function_and_on_a_list_for_every_count() {
    let out = scratch("induct-others");
    let (result, summary) = aver_backend_json("induction_along_others.av", &out, None);
    let closed = &summary["closed_by"];
    // `lt` recurses where the claim passes no given; `add` recurses on `i`.
    assert_eq!(closed["lt.belowSuccessorOfSum"], "steps", "{summary}");
    assert_eq!(closed["lt.belowSuccessorOfSumNamed"], "steps", "{summary}");
    // The induction on `xs` holds for every `n`, so the cell case takes the
    // claim at the tail with `n - 1`.
    assert_eq!(closed["taken.takeTwice"], "steps", "{summary}");
    // False laws of the same shapes stay open.
    for law in ["lt.belowSum", "taken.takeTwiceFurther"] {
        assert_eq!(closed[law], "open", "{law}: {}", format_output(&result));
    }
    let text = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction_along_others.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&text.stdout);
    assert!(
        text.contains("lt.belowSuccessorOfSum: closed by steps\n    hint: steps closed it by following the recursion of add; add `induction i` to go straight to it"),
        "{text}"
    );
    assert!(
        text.contains("lt.belowSuccessorOfSumNamed: closed by steps\n"),
        "{text}"
    );
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("induction_along_others.av", &out)
            .into_iter()
            .collect();
    let law = fs::read_to_string(&files["taken.takeTwice"]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&law),
        Ok("taken.takeTwice".to_string())
    );
    let ih = "((op - (v n) (i 1))) ())";
    assert!(
        law.contains(ih) && law.contains("(xsHead xsTail) (n) ("),
        "{law}"
    );
    for (kind, mutated) in [
        (
            "the claim at the tail at another count",
            law.replace(ih, "((op - (v n) (i 2))) ())"),
        ),
        (
            "values for a given the induction does not generalise",
            law.replace("(xsHead xsTail) (n) (", "(xsHead xsTail) () ("),
        ),
    ] {
        assert_ne!(mutated, law, "{kind}");
        assert!(
            aver::proof_kernel::verdict(&mutated).is_err(),
            "{kind}: the kernel accepted it"
        );
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn a_recursion_through_a_helper_is_read_as_if_the_helper_stood_in_its_place() {
    let out = scratch("induct-helper");
    let (result, summary) = aver_backend_json("induction_helper.av", &out, None);
    let closed = &summary["closed_by"];
    // `anyNonZero` calls `nonZeroOrOn`, which calls it back on the tail.
    assert_eq!(closed["anyNonZero.setHeadIsEnough"], "steps", "{summary}");
    assert_eq!(closed["anyNonZero.concatenation"], "steps", "{summary}");
    // `evenLength` and `oddLength` each descend on their own list: a
    // mutual recursion steps still do not open.
    assert_eq!(
        closed["evenLength.twoMoreKeepsIt"],
        "open",
        "{}",
        format_output(&result)
    );
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("induction_helper.av", &out)
            .into_iter()
            .collect();
    let law = fs::read_to_string(&files["anyNonZero.concatenation"]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&law),
        Ok("anyNonZero.concatenation".to_string())
    );
    let back = "(arm (pl (b false)) (call anyNonZero (v tail)))";
    let call = "(call nonZeroOrOn (v head) (v tail))";
    assert!(law.contains(back) && law.contains(call), "{law}");
    for (kind, mutated) in [
        (
            "the helper recurses on its own",
            law.replace(
                back,
                "(arm (pl (b false)) (call nonZeroOrOn (v head) (call anyNonZero (v tail))))",
            ),
        ),
        (
            "the helper is called with the whole list",
            law.replace(call, "(call nonZeroOrOn (v head) (v bytes))"),
        ),
        (
            "the helper rebinds the part it passes back",
            law.replace(
                back,
                "(arm (pl (b false)) (match (v head) (arm (pv tail) (call anyNonZero (v tail)))))",
            ),
        ),
    ] {
        assert!(
            aver::proof_kernel::verdict(&mutated).is_err(),
            "{kind}: {mutated}"
        );
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn an_induction_line_names_the_given_the_induction_follows() {
    let out = scratch("induct-named");
    let (result, summary) = aver_backend_json("induction_named.av", &out, None);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    let closed = &summary["closed_by"];
    // `append` recurses on `x` in one call and on `y` in another: the law
    // that names `x` closes, and so does the one that names neither, by
    // trying each; the laws that name `y` or `z` follow only it.
    assert_eq!(closed["append.assoc"], "steps", "{summary}");
    assert_eq!(closed["append.assocUnnamed"], "steps", "{summary}");
    for law in ["append.assocAlongY", "append.assocAlongZ"] {
        assert_eq!(closed[law], "open", "{law}: {summary}");
    }
    // The law closed by trying each given is told the line that goes
    // straight to the one that closed.
    assert_eq!(
        summary["steps_hints"]["append.assocUnnamed"],
        serde_json::json!([
            "steps closed it by trying each given in turn; add `induction x` to go straight to it"
        ]),
        "{summary}"
    );
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
        line("append.assocUnnamed").contains("closed by steps"),
        "{}",
        format_output(&result)
    );
    // `append` never recurses on `z`, a list: the line means induction on
    // `z` itself, which does not close this claim.
    assert!(
        line("append.assocAlongZ").contains("induction on z: evaluation stops at"),
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
    assert_eq!(
        files.keys().cloned().collect::<Vec<_>>(),
        ["append.assoc", "append.assocUnnamed"]
    );
    let law = fs::read_to_string(&files["append.assoc"]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&law),
        Ok("append.assoc".to_string())
    );
    let _ = fs::remove_dir_all(out);
}

/// Without an `induction` line, steps try each given the law's function
/// recurses on: the true law closes and is told the line that closed it,
/// and a false law of the same shape stays open under every given.
#[test]
fn without_an_induction_line_steps_try_each_given_and_a_false_law_stays_open() {
    let out = scratch("induct-each");
    let (result, summary) = aver_backend_json("induction_each.av", &out, None);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    let closed = &summary["closed_by"];
    assert_eq!(
        closed["append.assoc"],
        "steps",
        "{}",
        format_output(&result)
    );
    assert_eq!(
        summary["steps_hints"]["append.assoc"],
        serde_json::json!([
            "steps closed it by trying each given in turn; add `induction x` to go straight to it"
        ]),
        "{summary}"
    );
    assert_eq!(closed["append.swapsFirstTwo"], "open", "{summary}");
    let text = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction_each.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&text.stdout);
    assert!(
        text.contains(
            "append recurses on x in one call and on y in another, and steps close every case along neither"
        ),
        "{text}"
    );
    assert!(!text.contains("induction y` to go straight"), "{text}");
    let _ = fs::remove_dir_all(out);
}

/// A claim whose function recurses only on a list the claim builds is
/// proved by induction on the list given itself: without a line steps try
/// it after induction along the function and hint `induction xs`; with the
/// line they go straight to it. A false law of the same shape stays open,
/// and the kernel refuses the proof once its hypothesis is not the claim
/// at the tail.
#[test]
fn induction_on_a_list_given_the_function_does_not_recurse_on() {
    let out = scratch("induct-list");
    let (result, summary) = aver_backend_json("induction_on_list.av", &out, None);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    let closed = &summary["closed_by"];
    for law in ["sum.ofOneMore", "sum.ofOneMoreNamed"] {
        assert_eq!(closed[law], "steps", "{law}: {}", format_output(&result));
    }
    for law in ["sum.offByOne", "sum.offByOneNamed"] {
        assert_eq!(closed[law], "open", "{law}: {summary}");
    }
    assert_eq!(
        summary["steps_hints"]["sum.ofOneMore"],
        serde_json::json!([
            "steps closed it by trying each given in turn; add `induction xs` to go straight to it"
        ]),
        "{summary}"
    );
    assert!(
        summary["steps_hints"]["sum.ofOneMoreNamed"].is_null(),
        "{summary}"
    );
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("induction_on_list.av", &out)
            .into_iter()
            .collect();
    let law = fs::read_to_string(&files["sum.ofOneMoreNamed"]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&law),
        Ok("sum.ofOneMoreNamed".to_string())
    );
    assert!(law.contains("(listinduct xs "), "{law}");
    let mutated = law.replacen("(listinduct xs ", "(listinduct y ", 1);
    assert!(aver::proof_kernel::verdict(&mutated).is_err(), "{mutated}");
    let _ = fs::remove_dir_all(out);
}

const LIST_WHEN_LAWS: [&str; 2] = ["sum.ofOneMoreWhenPositive", "sum.ofOneMoreWhenShort"];

/// Induction on a list given under a `when` about the list: the `when` is
/// carried, so it holds in each case at the case's value of the list, and
/// the cell case takes the tail's hypothesis once the `when` is proved at
/// the tail. False laws of the same shape stay open. Both kernels accept
/// the scripts, and the kernel written in Aver and the replayer refuse a
/// hypothesis taken without proving the `when` at the tail, one taken on
/// the `when` at the cell instead, and the `when` left uncarried.
#[test]
fn both_kernels_induct_on_a_list_under_a_carried_when_and_refuse_mutations() {
    let out = scratch("list-when");
    let (result, summary) = aver_backend_json("induction_on_list_when.av", &out, None);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    for law in LIST_WHEN_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    for law in ["sum.offByOneWhenPositive", "sum.exceedsWhenPositive"] {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    assert_eq!(
        summary["steps_hints"]["sum.ofOneMoreWhenShort"],
        serde_json::json!([
            "steps closed it by trying each given in turn; add `induction xs` to go straight to it"
        ]),
        "{summary}"
    );
    let _ = fs::remove_dir_all(&out);
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("induction_on_list_when.av", &out)
            .into_iter()
            .collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), LIST_WHEN_LAWS);
    for (law, path) in &files {
        let text = fs::read_to_string(path).unwrap();
        assert_eq!(aver::proof_kernel::verdict(&text), Ok(law.clone()));
        assert!(text.contains("(listinduct xs "), "{text}");
        assert!(text.contains(" (carry when) "), "{text}");
        // (IH () (PROOF…)): the hypothesis at the tail with the proof of
        // the carried `when` there.
        let ih_open = text.find("((ih").expect("a hypothesis at the tail") + 1;
        let (proofs_from, proofs_to) = sub_forms(&text, ih_open)[1];
        for (kind, mutant) in [
            (
                "the `when` not carried",
                text.replacen(" (carry when)", "", 1),
            ),
            (
                "the hypothesis without the proof of the `when` at the tail",
                format!("{}(){}", &text[..proofs_from], &text[proofs_to..]),
            ),
            (
                "the hypothesis on the `when` at the cell",
                format!("{}((hyp when)){}", &text[..proofs_from], &text[proofs_to..]),
            ),
        ] {
            let refused = aver::proof_kernel::verdict(&mutant);
            assert!(
                refused
                    .as_ref()
                    .is_err_and(|why| why.starts_with("step proof")),
                "{law}, {kind}: {refused:?}\n{mutant}"
            );
            let path = out.join("mutant.steps");
            fs::write(&path, &mutant).unwrap();
            let result = replay(std::slice::from_ref(&path));
            assert!(
                !result.status.success(),
                "{law}, {kind}: {}",
                format_output(&result)
            );
        }
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_inducts_on_a_list_under_a_carried_when_and_refuses_mutations() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("list-when-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction_on_list_when.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
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
    for law in LIST_WHEN_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    for law in ["sum.offByOneWhenPositive", "sum.exceedsWhenPositive"] {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("InductionOnListWhen.lean")).unwrap();
    let law = "sum.ofOneMoreWhenPositive";
    let line = exact_line(
        &lean,
        "__aver_reason_sum_law_ofOneMoreWhenPositive_implication",
    )
    .to_string();
    // The recursor's motive is the claim under the carried `when`, and the
    // recursor is applied to the `when` itself at the end.
    let carried = " xs (h_when))";
    assert!(
        line.contains("List.rec (motive := fun xs => (allPos xs : Bool) = (true : Bool) → ")
            && line.contains(carried),
        "{line}"
    );
    let unapplied = line.replacen(carried, " xs)", 1);
    assert!(
        lean_refuses(
            &out,
            "InductionOnListWhen.lean",
            &lean.replacen(&line, &unapplied, 1),
            law
        ),
        "the `when` left unapplied: Lean must refuse the step term"
    );
    // The tail's hypothesis applied to the `when` at the cell instead of
    // its proof at the tail.
    let ih_open = line
        .find("(steps_all_xsTail ")
        .unwrap_or_else(|| panic!("{line}"));
    let at_cell = format!(
        "{}(steps_all_xsTail h_when){}",
        &line[..ih_open],
        &line[closing(&line, ih_open) + 1..]
    );
    assert!(
        lean_refuses(
            &out,
            "InductionOnListWhen.lean",
            &lean.replacen(&line, &at_cell, 1),
            law
        ),
        "the hypothesis on the `when` at the cell: Lean must refuse the step term"
    );
    let _ = fs::remove_dir_all(out);
}

/// Without an `induction` line a list given is inducted on only when a
/// call of a function that recurses holds it where it recurses, in the
/// claim as written or behind one call of a function that does not
/// recurse: otherwise the hypothesis has nowhere to apply, and each such
/// attempt would evaluate the whole claim over again for nothing. An
/// attempt that ran leaves `induction on <given>: …` in the refusal, so
/// the refusals say which ran. The author's line is followed as written.
#[test]
fn a_list_given_no_recursion_walks_is_not_inducted_on() {
    let out = scratch("induct-not-walked");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "induction_not_walked.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&result.stdout);
    let refusal = |law: &str| {
        text.lines()
            .find(|l| l.trim_start().starts_with(&format!("{law}:")))
            .unwrap_or_else(|| panic!("{law}: {}", format_output(&result)))
            .to_string()
    };
    let attempts = |line: &str| line.matches("induction on ").count();
    // `len` walks `xs`; `ys` is only read once by a function that does
    // not recurse.
    let length_only = refusal("tagged.lengthOnly");
    assert!(length_only.contains("induction on xs: "), "{length_only}");
    assert_eq!(attempts(&length_only), 1, "{length_only}");
    // `twice` is a name for a claim in which `len` walks `xs`.
    let is_one = refusal("twice.isOne");
    assert!(is_one.contains("induction on xs: "), "{is_one}");
    // Neither the claim nor `wrapped`, opened once, calls anything that
    // recurses.
    let is_zero = refusal("wrapped.isZero");
    assert!(
        is_zero.contains("the law is about wrapped, which does not recurse"),
        "{is_zero}"
    );
    assert_eq!(attempts(&is_zero), 0, "{is_zero}");
    // The same claim under `induction xs`.
    let named = refusal("wrapped.isZeroNamed");
    assert!(named.contains("induction on xs: "), "{named}");
    assert_eq!(attempts(&named), 1, "{named}");
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
    assert!(pow2.contains("((n int))"), "{pow2}");
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
            pow2.replacen("((n int))", "((n _))", 1),
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
    // An equality of calls read from the hypothesis about `twoPow`.
    let start = agrees.find("(proof ").expect("a script has a proof");
    let at = start
        + agrees[start..]
            .find("(rule int.eq.of_beq ((a ")
            .unwrap_or_else(|| panic!("{agrees}"));
    let b = at + agrees[at..].find(" (b ").expect("the rule's right side") + 1;
    let equal = &agrees[at..=closing(&agrees, b)];
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
                &format!("{}(b (call pow2 (v n)))", &agrees[at..b]),
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

/// Export a fixture to Lean with `--check-json` and return its summary, or
/// `None` when `lake` is not available.
fn lean_summary(fixture: &str, name: &str) -> Option<serde_json::Value> {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return None;
    }
    let out = scratch(name);
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            fixture,
            "-o",
            out.to_str().unwrap(),
            "--check-json",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let summary = serde_json::from_str(
        String::from_utf8_lossy(&result.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap(),
    )
    .unwrap();
    let _ = fs::remove_dir_all(out);
    Some(summary)
}

/// The Lean file a fixture's proof export writes for its module.
fn exported_lean(fixture: &str, module: &str, name: &str) -> String {
    let out = scratch(name);
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &["proof", fixture, "-o", out.to_str().unwrap()],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join(format!("{module}.lean"))).unwrap();
    let _ = fs::remove_dir_all(out);
    lean
}

/// A law under a `when` with no `using` or `because` line that its steps
/// close is a theorem a later law cites: the citing law has the fact, and
/// Lean closes both by their steps.
#[test]
fn a_when_law_its_steps_close_is_cited_without_a_using_line() {
    let lean = exported_lean("cited_bare_when.av", "CitedBareWhen", "cited-bare-when");
    assert!(
        lean.contains("have _fact0 := sign_law_positiveIsOne"),
        "{lean}"
    );
    assert!(
        !lean.contains("dependency has no available theorem"),
        "{lean}"
    );
    let Some(summary) = lean_summary("cited_bare_when.av", "cited-bare-when-lean") else {
        return;
    };
    for law in ["sign.positiveIsOne", "doubledSign.positiveIsTwo"] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    assert_eq!(summary["sorries"], 0, "{summary}");
}

/// A rule over `[]` writes its `[]` with no type; in Lean it is the list
/// it stands for, so the steps of a law about `List.take([], n)` and
/// `List.drop([], n)` check.
#[test]
fn an_empty_list_a_rule_writes_keeps_its_element_type_in_lean() {
    let lean = exported_lean("take_of_empty.av", "TakeOfEmpty", "take-of-empty");
    assert!(!lean.contains("List Unit"), "{lean}");
    let Some(summary) = lean_summary("take_of_empty.av", "take-of-empty-lean") else {
        return;
    };
    for law in ["takenCount.ofEmpty", "droppedCount.ofEmpty"] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    assert_eq!(
        summary["steps_rejected"],
        serde_json::json!([]),
        "{summary}"
    );
}

/// The `[]` a list rule takes as an argument is written with the type of
/// the list it stands for. Untyped, Lean unfolds the list functions
/// around it while it unifies each step with the next, and the steps that
/// read four bytes back through `[] ++ [d]` ran out of time in Lean.
#[test]
fn an_empty_list_a_step_takes_as_an_argument_is_typed_in_lean() {
    let lean = exported_lean(
        "rule_argument_lists.av",
        "RuleArgumentLists",
        "rule-argument-lists",
    );
    // The arguments of each list rule, up to the next step.
    let untyped: Vec<&str> = lean
        .split("AverSteps.list_")
        .skip(1)
        .map(|s| {
            ["Eq.trans", "__aver_unfold", "AverSteps."]
                .iter()
                .filter_map(|end| s.find(end))
                .min()
                .map_or(s, |i| &s[..i])
        })
        .filter(|args| args.contains("([])") || args.contains("([] ++"))
        .collect();
    assert!(untyped.is_empty(), "{}", untyped.join("\n"));
    let Some(summary) = lean_summary("rule_argument_lists.av", "rule-argument-lists-lean") else {
        return;
    };
    assert_eq!(summary["closed_by"]["wide.fourBytes"], "steps", "{summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
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
    // A digit read back is pinned by the division step itself; two read
    // back bound their quotient by `int.div_range`.
    let two = read("digits.twoDigitsReadBack");
    let appends = read("digits.accumulatorAppends");
    assert!(step.contains("(unfold digits 1 "), "{step}");
    assert!(two.contains("(rule int.div_range "), "{two}");
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
            &two,
            two.replacen("(m (i 256)) (n (i 1))", "(m (i 512)) (n (i 2))", 1),
        ),
        // A bound that is not the range divided by the divisor.
        (
            &two,
            two.replacen("(m (i 256)) (n (i 1))", "(m (i 256)) (n (i 2))", 1),
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

const REMAINDER_LAWS: [&str; 7] = [
    "base.ignoresBitSeven",
    "base.lowFiveBitsDecide",
    "bitSeven.complementIsArithmetic",
    "bitSeven.isTheMask",
    "bitSeven.shiftMultiplies",
    "digit.readsTheDigit",
    "readBack.roundTrip",
];

/// Division by a literal read over the integers, and masks of bits read as
/// arithmetic: both kernels accept the steps and refuse a cut that divides
/// by a number that is not a divisor, a step without its cuts, a remainder
/// by 0 or by a name, and a mask or shift read with the wrong power of two.
/// Laws that hold on their samples only (a remainder by 32 below 31, a
/// quotient of a negative number rounded toward zero) do not close.
#[test]
fn both_kernels_read_remainders_over_the_integers_and_refuse_mutations() {
    let out = scratch("remainders");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("remainders.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), REMAINDER_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let digit = read("digit.readsTheDigit");
    let low = read("base.lowFiveBitsDecide");
    let bit = read("bitSeven.isTheMask");
    assert!(digit.contains("(cut 256 "), "{digit}");
    assert!(low.contains("(rule bits.and_low "), "{low}");
    assert!(bit.contains("(rule bits.and_bit "), "{bit}");
    let range = "(rule int.mod_range ((a (op + (op * (v prefix) (i 256)) (v d))) (k (i 256))) (compute (op > (i 256) (i 0)) (b true)))";
    assert!(digit.contains(range), "{digit}");
    let without_cuts = {
        let mut text = digit.clone();
        while let Some(at) = text.find(" (cut ") {
            let close = text[at..].find("))").unwrap() + at + 2;
            text.replace_range(at..close, "");
        }
        text
    };
    let mutants = [
        // A cut by a number that does not divide the coefficients.
        (&digit, digit.replacen("(cut 256 ", "(cut 255 ", 1)),
        // The rounded rows dropped: over the rationals nothing follows.
        (&digit, without_cuts),
        // A remainder by 0, and by a name, has no range.
        (
            &digit,
            digit.replacen(
                range,
                "(rule int.mod_range ((a (op + (op * (v prefix) (i 256)) (v d))) (k (i 0))) (compute (op > (i 0) (i 0)) (b true)))",
                1,
            ),
        ),
        (
            &digit,
            digit.replacen(
                range,
                "(rule int.mod_range ((a (op + (op * (v prefix) (i 256)) (v d))) (k (v d))) (compute (op > (v d) (i 0)) (b true)))",
                1,
            ),
        ),
        // A mask read by the wrong power of two, or one that is not one less
        // than a power of two.
        (&bit, bit.replacen("(k (i 128)) (n (i 7))", "(k (i 128)) (n (i 6))", 1)),
        (
            &low,
            low.replacen("(m (i 31)) (k (i 32))", "(m (i 30)) (k (i 32))", 1),
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
    let refused = scratch("remainders-refused");
    assert!(export_steps("remainders_refused.av", &refused).is_empty());
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(refused);
}

#[test]
fn lean_closes_remainders_and_masks_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("remainders-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "remainders.av",
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
    for law in REMAINDER_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Remainders.lean")).unwrap();
    for lemma in ["bits_and_low", "bits_and_bit", "bits_not", "bits_shl"] {
        assert!(lean.contains(&format!("AverSteps.{lemma}")), "{lemma}");
    }
    let _ = fs::remove_dir_all(out);
}

const DIVISION_LAWS: [&str; 5] = [
    "carried.belowTheBase",
    "digits.oneDigitMore",
    "phase.repeatsBackwards",
    "phase.repeatsEveryWindow",
    "split.readsTheDigits",
];

/// Euclidean division by a literal inside a larger term takes a step where
/// linear arithmetic over the integers pins it: a remainder repeats with
/// its period (`(h + 2016) % 2016` is `h % 2016`), and digits written in
/// base 256 read back. Both kernels accept the steps and refuse them with
/// a cut by a number that does not divide the row, or without their cuts.
/// Laws that hold on their samples only (a period one short, a digit that
/// may be 256 or below zero) do not close.
#[test]
fn both_kernels_pin_a_division_inside_a_term_and_refuse_mutations() {
    let out = scratch("divisions");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("divisions.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), DIVISION_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let window = read("phase.repeatsEveryWindow");
    let period = "(rule int.eq.of_beq ((a (bi __int_mod_euclid (op + (v height) (i 2016)) (i 2016))) (b (bi __int_mod_euclid (v height) (i 2016))))";
    assert!(window.contains(period), "{window}");
    assert!(window.contains("(cut 2016 "), "{window}");
    let without_cuts = {
        let mut text = window.clone();
        while let Some(at) = text.find(" (cut ") {
            let close = text[at..].find("))").unwrap() + at + 2;
            text.replace_range(at..close, "");
        }
        text
    };
    let mutants = [
        window.replacen("(cut 2016 ", "(cut 2015 ", 1),
        without_cuts,
        // The period one short: the remainders differ.
        window.replace(
            "(bi __int_mod_euclid (op + (v height) (i 2016)) (i 2016))",
            "(bi __int_mod_euclid (op + (v height) (i 2015)) (i 2016))",
        ),
    ];
    for (i, mutant) in mutants.iter().enumerate() {
        assert_ne!(*mutant, window, "mutant {i} changed nothing");
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(verdict.is_err(), "mutant {i}: {verdict:?}\n{mutant}");
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let refused = scratch("divisions-refused");
    assert!(export_steps("divisions_refused.av", &refused).is_empty());
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(refused);
}

#[test]
fn lean_closes_divisions_inside_a_term_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("divisions-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "divisions.av",
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
    for law in DIVISION_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const PRODUCT_LAWS: [&str; 3] = [
    "area.nonnegative",
    "bothPositive.positive",
    "scaledBelow.nonnegativeScale",
];

/// Comparisons about products settled by linear arithmetic once the
/// product of two nonnegative comparisons in scope is read as nonnegative
/// (`int.mul_nonneg`): both kernels accept the steps and refuse a factor
/// that is not shown nonnegative, a product read with its factors swapped
/// for ones the comparisons do not give, and a product read as strictly
/// positive. Laws that hold on their samples only (one factor of unknown
/// sign, factors of opposite signs, a product of nonnegatives read as
/// positive) do not close.
#[test]
fn both_kernels_read_products_of_nonnegatives_and_refuse_mutations() {
    let out = scratch("products");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("products.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), PRODUCT_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let scaled = read("scaledBelow.nonnegativeScale");
    let positive = read("bothPositive.positive");
    let rule = "(rule int.mul_nonneg ((a (op - (v b) (v a))) (b (v c)))";
    assert!(scaled.contains(rule), "{scaled}");
    let factor = "(linear (op <= (i 0) (op - (v b) (v a))) true (when1) (1 1))";
    assert!(scaled.contains(factor), "{scaled}");
    let product = "(have h_steps1 (op <= (i 0) (op * (op - (v b) (v a)) (v c)))";
    assert!(scaled.contains(product), "{scaled}");
    let mutants = [
        // A factor the comparison in scope does not make nonnegative.
        (
            &scaled,
            scaled.replacen(
                factor,
                "(linear (op <= (i 0) (op - (v a) (v b))) true (when1) (1 1))",
                1,
            ),
        ),
        // The product of a negative factor: the rule's premise is not the
        // comparison its proof ends at.
        (
            &scaled,
            scaled
                .replacen(
                    rule,
                    "(rule int.mul_nonneg ((a (op - (v a) (v b))) (b (v c)))",
                    1,
                )
                .replacen(
                    product,
                    "(have h_steps1 (op <= (i 0) (op * (op - (v a) (v b)) (v c)))",
                    1,
                ),
        ),
        // A product of nonnegatives read as positive.
        (
            &scaled,
            scaled.replacen(
                product,
                "(have h_steps1 (op < (i 0) (op * (op - (v b) (v a)) (v c)))",
                1,
            ),
        ),
        (
            &positive,
            positive.replacen("(op <= (i 0) (op * ", "(op < (i 0) (op * ", 1),
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
    let refused = scratch("products-refused");
    assert!(export_steps("products_refused.av", &refused).is_empty());
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(refused);
}

/// Lean closes the same laws by their steps, the product by
/// `AverSteps.mul_nonneg`, and refuses a factor weakened past what its
/// comparison gives and a product of nonnegatives read as positive.
#[test]
fn lean_closes_products_by_their_steps_and_refuses_mutated_ones() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("products-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "products.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("Products.lean")).unwrap();
    let law = "bothPositive.positive";
    assert!(
        !lean_refuses(&out, "Products.lean", &lean, law),
        "the emitted step term must close the law"
    );
    let line = exact_line(&lean, "bothPositive_law_positive").to_string();
    assert!(
        line.contains("AverSteps.mul_nonneg ((a - 1)) ((b - 1))"),
        "{line}"
    );
    let mutants = [
        ("a factor weakened", line.replace("(a - 1)", "(a - 2)")),
        (
            "a product read as positive",
            line.replace(
                "(0 : Int) <= ((a - 1) * (b - 1))",
                "(0 : Int) < ((a - 1) * (b - 1))",
            ),
        ),
    ];
    for (kind, mutated) in mutants {
        assert_ne!(mutated, line, "{kind}: changed nothing");
        let text = lean.replacen(&line, &mutated, 1);
        assert!(
            lean_refuses(&out, "Products.lean", &text, law),
            "{kind}: Lean must refuse the mutated step term"
        );
    }
    let _ = fs::remove_dir_all(out);
}

const NAMED_DIVISOR_LAWS: [&str; 6] = [
    "floorDiv.powerWindow",
    "floorDiv.window",
    "floorDiv.windowAbove",
    "pow2.positive",
    "restBy.belowTheDivisor",
    "restBy.recomposes",
];

/// `Int.div` and `Int.mod` by a name: once linear arithmetic shows the
/// divisor is not 0 (from a `when`, or from a cited `pow2(n) >= 1`), the
/// call is `Result.Ok` of the Euclidean quotient or remainder
/// (`int.div.ok`, `int.mod.ok`), and where the divisor is positive the
/// window follows by the rules for a literal divisor. Both kernels accept
/// the steps and refuse the rule at a divisor of 0, without its premise,
/// and the remainder's rule written for the quotient. Laws that hold on
/// their samples only (no fact about the divisor, a divisor that may be
/// 0, a negative divisor) do not close.
#[test]
fn both_kernels_read_division_by_a_name_and_refuse_mutations() {
    let out = scratch("named-divisors");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("named_divisors.av", &out)
            .into_iter()
            .filter(|(law, _)| !law.ends_with(".implication"))
            .collect();
    assert_eq!(
        files.keys().cloned().collect::<Vec<_>>(),
        NAMED_DIVISOR_LAWS
    );
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let window = read("floorDiv.window");
    let ok = "(rule int.div.ok ((a (v a)) (k (v d))) (rule int.eq.false_of_gt ((a (v d)) (b (i 0))) (linear (op > (v d) (i 0)) true (when) (1 1))))";
    assert!(window.contains(ok), "{window}");
    let mutants = [
        // A divisor of 0.
        window.replacen(
            ok,
            "(rule int.div.ok ((a (v a)) (k (i 0))) (rule int.eq.false_of_gt ((a (i 0)) (b (i 0))) (linear (op > (i 0) (i 0)) true (when) (1 1))))",
            1,
        ),
        // The rule without its premise.
        window.replacen(ok, "(rule int.div.ok ((a (v a)) (k (v d))))", 1),
        // The remainder's rule where the quotient stands.
        window.replacen("(rule int.div.ok ", "(rule int.mod.ok ", 1),
    ];
    for (i, mutant) in mutants.iter().enumerate() {
        assert_ne!(*mutant, window, "mutant {i} changed nothing");
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
    let refused = scratch("named-divisors-refused");
    assert!(export_steps("named_divisors_refused.av", &refused).is_empty());
    let _ = fs::remove_dir_all(out);
    let _ = fs::remove_dir_all(refused);
}

/// Lean closes the same laws by their steps, `Int.div` by a name by
/// `AverSteps.int_div_ok`, and refuses it at a divisor of 0 and the
/// remainder's lemma where the quotient stands.
#[test]
fn lean_closes_division_by_a_name_and_refuses_mutated_ones() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("named-divisors-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "named_divisors.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("NamedDivisors.lean")).unwrap();
    let law = "restBy.recomposes";
    assert!(
        !lean_refuses(&out, "NamedDivisors.lean", &lean, law),
        "the emitted step term must close the law"
    );
    let line = exact_line(&lean, "restBy_law_recomposes").to_string();
    assert!(line.contains("AverSteps.int_div_ok (a) (d)"), "{line}");
    let mutants = [
        (
            "a divisor of 0",
            line.replacen(
                "AverSteps.int_div_ok (a) (d)",
                "AverSteps.int_div_ok (a) ((0 : Int))",
                1,
            ),
        ),
        (
            "the remainder's lemma",
            line.replacen("AverSteps.int_div_ok", "AverSteps.int_mod_ok", 1),
        ),
    ];
    for (kind, mutated) in mutants {
        assert_ne!(mutated, line, "{kind}: changed nothing");
        let text = lean.replacen(&line, &mutated, 1);
        assert!(
            lean_refuses(&out, "NamedDivisors.lean", &text, law),
            "{kind}: Lean must refuse the mutated step term"
        );
    }
    let _ = fs::remove_dir_all(out);
}

const DROPPING_LAWS: [&str; 4] = [
    "pairsLeft.cellStep",
    "pairsLeft.nonNegative",
    "walk.neverBelowAcc",
    "walk.readsStep",
];

/// A recursion that passes `List.drop` of the matched list's tail, directly
/// and through a helper whose local binding drops, opens and is followed
/// by induction on the list's length; both kernels refuse a descent that
/// does not shorten the list, a local binding that calls back, and a
/// hypothesis in the case of the empty list.
#[test]
fn both_kernels_follow_a_recursion_on_a_dropped_tail_and_refuse_mutations() {
    let out = scratch("dropping");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("dropping.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), DROPPING_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let pairs = read("pairsLeft.nonNegative");
    let walk = read("walk.neverBelowAcc");
    assert!(pairs.contains("(proof (induct pairsLeft "), "{pairs}");
    assert!(walk.contains("(proof (induct walk "), "{walk}");
    let mutants = [
        // A call on the whole cell again, dropping nothing: it never stops.
        (
            &pairs,
            pairs.replace(
                "(bi List.drop (v t) (i 1))",
                "(bi List.drop (bi List.prepend (v h) (v t)) (i 0))",
            ),
        ),
        // A call on a list taken from the tail rather than dropped from it.
        (
            &pairs,
            pairs.replace("(bi List.drop (v t) (i 1))", "(bi List.take (v t) (i 1))"),
        ),
        // The helper drops from a list it grew first.
        (
            &walk,
            walk.replace(
                "(bi List.drop (v rest) (v n))",
                "(bi List.drop (bi List.prepend (v n) (v rest)) (i 0))",
            ),
        ),
        // The helper's local binding calls the definition back.
        (
            &walk,
            walk.replace(
                "((left (bi List.drop (v rest) (v n))))",
                "((left (bi List.drop (v rest) (call walk (v rest) (v acc)))))",
            ),
        ),
        // A hypothesis in the case of the empty list, which makes no call.
        (&walk, replace_in_case(&walk, 0, 1, "(ih1)")),
    ];
    for (i, (source, mutant)) in mutants.iter().enumerate() {
        assert_ne!(mutant, *source, "mutant {i} changed nothing");
        // Refused by a step, or before any step by the gate every
        // recursive definition of the script passes.
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(
            verdict
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")
                    || why.ends_with("is part of a mutual recursion")),
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
fn lean_closes_a_recursion_on_a_dropped_tail_by_its_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("dropping-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "dropping.av",
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
    for law in DROPPING_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("Dropping.lean")).unwrap();
    assert!(lean.contains("AverSteps.list_length_induct"), "{lean}");
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
        // A law with a `because` line cites its obligations, which split.
        if law == "double.keepsLength" {
            assert!(
                read(law).contains("(law double.keepsLength.because1 "),
                "{law}: does not cite its reason"
            );
        } else {
            assert!(read(law).contains("(split "), "{law}: no split");
        }
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
    // (split ON HYP (LITERALS) CASE…): the first two sub-forms are the
    // subject and the literals.
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
    assert!(bits.contains("(proof (split "), "{bits}");
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

const COMPARISON_CALLS_LAWS: [&str; 2] = ["inBand.sumOfTwo", "widthFits.always"];

/// The calls inside an Int comparison in scope open: `maxInt(8, 8)` is
/// evaluated and cut in as `== 8`, `minInt(x, y)` and `maxInt(x, y)` are
/// split on their guards, `width(code)` into the arms its `match` reads;
/// both kernels refuse a cut-in value that is wrong and an arm its guard
/// does not select.
#[test]
fn both_kernels_open_the_calls_inside_a_comparison_and_refuse_mutations() {
    let out = scratch("comparison-calls");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("comparison_calls.av", &out)
            .into_iter()
            .collect();
    assert_eq!(
        files.keys().cloned().collect::<Vec<_>>(),
        COMPARISON_CALLS_LAWS
    );
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let paths: Vec<PathBuf> = files.values().cloned().collect();
    let result = replay(&paths);
    assert!(result.status.success(), "{}", format_output(&result));

    let band = read("inBand.sumOfTwo");
    let folded = "(op == (call maxInt (i 8) (i 8)) (i 8))";
    assert!(band.contains(&format!("{folded} ")), "{band}");
    assert!(band.contains("(cases (op <= (v base) (v y)) "), "{band}");
    assert!(band.contains("(unfold minInt 2 "), "{band}");
    let mutants = [
        // `maxInt(8, 8)` cut in with the wrong value.
        band.replacen(folded, "(op == (call maxInt (i 8) (i 8)) (i 9))", 1),
        // The `false` arm of `minInt` where its guard was split true.
        band.replacen("(unfold minInt 1 ", "(unfold minInt 2 ", 1),
    ];
    for (i, mutant) in mutants.iter().enumerate() {
        assert_ne!(*mutant, band, "mutant {i} changed nothing");
        let verdict = aver::proof_kernel::verdict(mutant);
        assert!(verdict.is_err(), "mutant {i}: {verdict:?}\n{mutant}");
        let path = out.join(format!("mutant{i}.steps"));
        fs::write(&path, mutant).unwrap();
        let result = replay(std::slice::from_ref(&path));
        assert!(!result.status.success(), "{}", format_output(&result));
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_closes_the_calls_opened_inside_a_comparison() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("comparison-calls-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "comparison_calls.av",
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
    for law in COMPARISON_CALLS_LAWS {
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

const SHAPES_LAWS: [&str; 11] = [
    "fill.oneStep",
    "implied.always",
    "offBy.otherSize",
    "readAll.lowDigitStep",
    "readDigits.finalDigit",
    "readDigits.largerStaysLarger",
    "readDigits.largerStaysLarger.because1",
    "readDigits.largerStaysLarger.implication",
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

/// The step files of a Lean export that Lean was never given a step term
/// for. Each rendered step term is followed by its fallback, which names
/// the script (`AVER_STEPS_REJECTED:<key>`); a law whose `because`
/// obligations have scripts of their own is stated through them. A script
/// with neither left its claim to the tactics without a word.
fn unrendered_step_files(out: &Path) -> Vec<String> {
    fn lean_sources(dir: &Path, text: &mut String) {
        for entry in fs::read_dir(dir).into_iter().flatten().flatten() {
            let path = entry.path();
            if path.is_dir() {
                if path.file_name().is_some_and(|n| n != ".lake") {
                    lean_sources(&path, text);
                }
            } else if path.extension().is_some_and(|e| e == "lean") {
                text.push_str(&fs::read_to_string(&path).unwrap());
            }
        }
    }
    let mut text = String::new();
    lean_sources(out, &mut text);
    let keys: Vec<String> = fs::read_dir(out.join("proof_steps"))
        .into_iter()
        .flatten()
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "steps"))
        .map(|p| p.file_stem().unwrap().to_string_lossy().to_string())
        .collect();
    let obligation_of = |key: &str, law: &str| {
        key.strip_prefix(law)
            .and_then(|rest| rest.strip_prefix('.'))
            .is_some_and(|step| {
                step == "implication"
                    || step
                        .strip_prefix("because")
                        .is_some_and(|k| !k.is_empty() && k.bytes().all(|b| b.is_ascii_digit()))
            })
    };
    let mut missing: Vec<String> = keys
        .iter()
        .filter(|key| !text.contains(&format!("AVER_STEPS_REJECTED:{key}\"")))
        .filter(|key| !keys.iter().any(|k| obligation_of(k, key)))
        .cloned()
        .collect();
    missing.sort();
    missing
}

/// Every step script a fixture's producer wrote reaches Lean as a step
/// term. A shape the Lean renderer cannot spell falls back to the tactics
/// with nothing in the report, so the second judge would never check the
/// steps the kernel written in Aver accepted; this keeps such a shape
/// from landing unnoticed. The export needs no Lean.
#[test]
fn lean_is_given_every_step_script_the_fixtures_write() {
    let dir = repo_root().join(FIXTURES);
    let mut fixtures: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "av"))
        .collect();
    fixtures.sort();
    let mut missing = Vec::new();
    for fixture in fixtures {
        let name = fixture.file_name().unwrap().to_string_lossy().to_string();
        let out = scratch(&format!("rendered-{name}"));
        let result = aver_in(&dir, &["proof", &name, "-o", out.to_str().unwrap()]);
        assert!(
            result.status.success(),
            "{name}: {}",
            format_output(&result)
        );
        missing.extend(
            unrendered_step_files(&out)
                .into_iter()
                .map(|key| format!("{name}: {key}")),
        );
        let _ = fs::remove_dir_all(out);
    }
    assert!(
        missing.is_empty(),
        "step scripts Lean was not given (run with AVER_STEPS_DEBUG=1 for why):\n{}",
        missing.join("\n")
    );
}

/// Lean closes every btc shape by the steps the kernel accepted, each
/// obligation of a `because` chain too: an induction along a list that
/// carries a comparison `when` takes it as a Prop and hands the recursor
/// its Bool equation.
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
    // `+` on texts does not commute and on Floats does not associate: a
    // ring step takes two Int terms, so it reads neither, even in a script
    // made by hand.
    let script = |ty: &str| {
        format!(
            "(steps 16 (obligation k ((a {ty}) (b {ty})) (none) (op + (v a) (v b)) (op + (v b) (v a))) (defs) (consts) (sums) (recs) (sigs) (laws) (proof (ring (op + (v a) (v b)) (op + (v b) (v a)))))"
        )
    };
    assert_eq!(
        aver::proof_kernel::verdict(&script("int")),
        Ok("k".to_string())
    );
    for ty in ["str", "float"] {
        assert_eq!(
            aver::proof_kernel::verdict(&script(ty)),
            Err("step proof: ring needs two Int terms".to_string()),
            "{ty}"
        );
    }
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
    // A linear step (`x + x` to `2 * x`) goes to `omega` and keeps the law
    // off `Classical.choice`; only a product of givens multiplied out goes
    // through the ring normaliser, whose soundness proof uses it.
    fs::write(
        out.join("Axioms.lean"),
        "import Arith\n#print axioms Arith.twice_law_isDouble\n#print axioms Arith.sqSum_law_expands\n",
    )
    .unwrap();
    let printed = Command::new("lake")
        .args(["env", "lean", "Axioms.lean"])
        .current_dir(&out)
        .output()
        .expect("lake runs");
    let printed = String::from_utf8_lossy(&printed.stdout).replace('\n', " ");
    let axioms_of = |theorem: &str| {
        if printed.contains(&format!("'{theorem}' does not depend on any axioms")) {
            return String::new();
        }
        let start = printed
            .find(&format!("'{theorem}' depends on axioms: ["))
            .unwrap_or_else(|| panic!("no axioms for {theorem}: {printed}"));
        let rest = &printed[start..];
        rest[..rest.find(']').unwrap()].to_string()
    };
    assert!(
        !axioms_of("Arith.twice_law_isDouble").contains("Classical.choice"),
        "{printed}"
    );
    assert!(
        axioms_of("Arith.sqSum_law_expands").contains("Classical.choice"),
        "{printed}"
    );
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

/// `==` on an Int is a decided test, so a law about it needs no
/// `Classical.choice`. Lean 4.35's own `BEq Int` is a projection of `Int`'s
/// order package, whose proofs use that axiom; the prelude decides `==` on
/// an Int with an instance that uses none, also inside a list (#1558).
#[test]
fn lean_proves_int_equality_laws_without_classical_choice() {
    if !lean_required::lake_available() {
        eprintln!("skipping: `lake` is not available");
        return;
    }
    let out = scratch("int-equality-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "int_equality.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let manifest: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(out.join("proof_manifest.json")).unwrap())
            .unwrap();
    let laws = manifest["laws"].as_array().unwrap();
    for law in ["same.symmetric", "sameList.reflexive", "gap.noneWhenEqual"] {
        let record = laws
            .iter()
            .find(|record| record["law"] == law)
            .unwrap_or_else(|| panic!("no record for {law}: {manifest}"));
        assert_eq!(record["tier"], "universal", "{law}: {record}");
        let axioms = record["axioms"].as_array().unwrap();
        assert!(
            !axioms.iter().any(|axiom| axiom == "Classical.choice"),
            "{law}: {record}"
        );
    }
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
                "(compute (bi List.len (list)) (i 0))",
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
                "(symm (compute (bi List.len (list)) (i 0)))",
                "(compute (bi List.len (list)) (i 0))",
            ),
        ),
        (
            "a fact stated with a different right side",
            swap(
                "joined.lengthAdds",
                "(fact List.len.ofConcat ((a (list (var T))) (b (list (var T)))) (none) (bi List.len (bi List.concat (v a) (v b))) (op + (bi List.len (v a)) (bi List.len (v b)))",
                "(fact List.len.ofConcat ((a (list (var T))) (b (list (var T)))) (none) (bi List.len (bi List.concat (v a) (v b))) (op + (bi List.len (v a)) (bi List.len (v a)))",
            ),
        ),
        (
            "a fact whose induction is on a given not declared a list",
            swap(
                "joined.regroups",
                "(fact List.concat.assoc ((a (list (var T)))",
                "(fact List.concat.assoc ((a int)",
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
                "(fact List.take.nonPositive ((l (list (var T))) (n int)) (op <= (v n) (i 0))",
                "(fact List.take.nonPositive ((l (list (var T))) (n int)) (none)",
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

/// `--check-json` says where the run spent its time, per law and summed by
/// phase, and the report is the same apart from those times.
#[test]
fn the_aver_backend_reports_the_time_of_each_phase_per_law() {
    let dir = repo_root().join(FIXTURES);
    let summary = || -> serde_json::Value {
        let json = aver_in(
            &dir,
            &["proof", "hints.av", "--backend", "aver", "--check-json"],
        );
        serde_json::from_str(
            String::from_utf8_lossy(&json.stdout)
                .lines()
                .rev()
                .find(|l| l.starts_with('{'))
                .unwrap(),
        )
        .unwrap()
    };
    let mut first = summary();
    let timing = first["timing"].clone();
    let laws = timing["laws"].as_object().unwrap();
    let keys: Vec<&String> = laws.keys().collect();
    let closed: Vec<&String> = first["closed_by"].as_object().unwrap().keys().collect();
    assert_eq!(keys, closed);
    for phase in [
        "frontend",
        "produce",
        "automatic",
        "kernel_in_producer",
        "kernel_final",
        "hint_using",
    ] {
        assert!(
            timing["phases_ms"][phase].as_f64().is_some(),
            "{phase}: {timing}"
        );
    }
    // A closed law tries no hint; the open one finds its `using` line.
    assert_eq!(
        laws["reversed.twiceIsTheSame"]["hints"],
        serde_json::json!({})
    );
    assert!(
        laws["reversed.twiceIsTheSame"]["kernel_calls"]
            .as_u64()
            .unwrap()
            >= 1
    );
    let open = &laws["reversed.vectorLengthIsNeverNegative"];
    assert_eq!(open["hints"]["using"]["found"], 1, "{open}");
    assert!(open["hints"]["using"]["ms"].as_f64().is_some(), "{open}");
    // Apart from the times, a second run reports the same.
    let mut second = summary();
    for s in [&mut first, &mut second] {
        let obj = s.as_object_mut().unwrap();
        obj.remove("timing");
        obj.remove("kernel_ms");
    }
    assert_eq!(first, second);
}

/// The list facts of `facts::NORMALIZING` rewrite where evaluation stops
/// although no law cites them: the laws close, and each script carries the
/// facts it used, with their proofs, so the kernel checks them. A law they
/// do not close is hinted the `using` line that closes it, in
/// `--backend aver` text and JSON.
#[test]
fn laws_close_by_the_list_facts_they_do_not_cite_and_a_stuck_one_is_hinted() {
    let dir = repo_root().join(FIXTURES);
    let text = aver_in(&dir, &["proof", "hints.av", "--backend", "aver"]);
    let out = String::from_utf8_lossy(&text.stdout).to_string();
    assert!(
        out.contains("11 of 12 law(s) closed by steps"),
        "{}",
        format_output(&text)
    );
    assert!(
        out.contains("  joined.lengthAdds: closed by steps"),
        "{out}"
    );
    assert!(
        out.contains("    hint: steps close this law with `using [Vector.len.asList]`"),
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
    assert_eq!(summary["closed_by"]["reversed.twiceIsTheSame"], "steps");
    // Closed by a case split whose false case `List.len.nonneg` rules out.
    assert_eq!(
        summary["closed_by"]["reversed.lengthIsNeverNegative"],
        "steps"
    );
    assert_eq!(
        summary["closed_by"]["reversed.vectorLengthIsNeverNegative"],
        "open"
    );
    let export = scratch("hints");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("hints.av", &export).into_iter().collect();
    assert_eq!(files.len(), 11, "{files:?}");
    let three = fs::read_to_string(&files["batch.threeBatches"]).unwrap();
    assert!(three.contains("(fact List.concat.assoc "), "{three}");
    for (law, path) in &files {
        let text = fs::read_to_string(path).unwrap();
        assert_eq!(
            aver::proof_kernel::verdict(&text),
            Ok(law.clone()),
            "{text}"
        );
    }
    let _ = fs::remove_dir_all(export);
}

const NORMAL_FORM_LAWS: [&str; 9] = [
    "allOn.isSpec",
    "inc.eitherWay",
    "inc.isFirst",
    "inc.isPadded",
    "incFirst.isInc",
    "incPadded.goesRound",
    "onto.accumulatorComesFirst",
    "onto.regroupsTheEnd",
    "onto.reverseKeepsTheMark",
];

/// Where evaluation stops at two different terms, both are rewritten to a
/// normal form with the cited laws and the list facts, a literal a rewrite
/// leaves behind computed again (`List.reverse([7])`); two cited laws that
/// rewrite one term give the smaller result; an induction case uses the
/// claim at another accumulator and the facts; a `match` on a Bool the
/// kernel knows only from the arms is split on `Bool.and(head, true)`.
/// Each script is the kernel's to accept, a mutated one is refused, and
/// two cited laws that rewrite each other back are never followed round:
/// their law closes by opening the definitions, applying neither.
#[test]
fn both_sides_are_rewritten_to_a_normal_form_and_a_rewrite_loop_is_not_followed() {
    let out = scratch("normal-form");
    let (result, summary) = aver_backend_json("normal_form.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in NORMAL_FORM_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    let export = scratch("normal-form-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("normal_form.av", &export)
            .into_iter()
            .collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), NORMAL_FORM_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
    }
    let round = read("incPadded.goesRound");
    let round_proof = &round[round.find("\n (proof ").expect("a proof")..];
    for looping in ["(law inc.isFirst ", "(law incFirst.isInc "] {
        assert!(!round_proof.contains(looping), "{round}");
    }
    let mark = read("onto.reverseKeepsTheMark");
    assert!(mark.contains("(fact List.reverse.ofConcat "), "{mark}");
    assert!(mark.contains("(compute "), "{mark}");
    assert!(read("inc.eitherWay").contains("(law inc.isFirst "));
    let spec = read("allOn.isSpec");
    assert!(spec.contains("(rule bool.and.true_r "), "{spec}");
    // The fact's instance at another list: the kernel refuses it.
    let instance = "(law List.reverse.ofConcat ((a (v xs))";
    assert!(mark.contains(instance), "{mark}");
    let mutant = mutate_proof(&mark, instance, "(law List.reverse.ofConcat ((a (list))");
    assert!(aver::proof_kernel::verdict(&mutant).is_err(), "{mutant}");
    let _ = fs::remove_dir_all(export);
}

#[test]
fn lean_closes_the_normal_form_laws_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("normal-form-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "normal_form.av",
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
    for law in NORMAL_FORM_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// Empty lists of two element types meet in one table of equal terms: each
/// keeps its own type, so Lean takes every step term the kernel accepted.
#[test]
fn lean_keeps_each_empty_list_at_its_own_type() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("empty-list-types-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "empty_list_types.av",
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
    assert_eq!(
        summary["steps_rejected"],
        serde_json::json!([]),
        "{summary}"
    );
    for law in [
        "withoutEach.oneDeletionCommutes",
        "withoutEach.oneDeletionCommutes.because1",
        "withoutEach.oneDeletionCommutes.implication",
    ] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// A hint names only a law the stuck law could cite: not a law of a
/// function its imported module does not expose, and not a law that
/// already cites the stuck one, which would make a cycle. The automatic
/// pass takes its laws from the same set.
#[test]
fn a_hint_never_names_an_unexposed_law_or_one_that_would_cite_back() {
    let dir = repo_root().join(FIXTURES).join("hint_scope");
    let out = scratch("hint-scope");
    let result = aver_in(
        &dir,
        &[
            "proof",
            "main.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
        ],
    );
    let _ = fs::remove_dir_all(&out);
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&result.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{}", format_output(&result))),
    )
    .unwrap();
    let hints = |law: &str| summary["steps_hints"][law].to_string();
    assert_eq!(summary["closed_by"]["total.ofJoined"], "open", "{summary}");
    assert!(
        !hints("total.ofJoined").contains("Lib.hidden.ofJoined"),
        "{summary}"
    );
    // The module's own law about its helper is used inside the module.
    assert_eq!(
        summary["automatic_using"]["Lib.shown.splits"],
        serde_json::json!(["Lib.hidden.ofJoined"]),
        "{summary}"
    );
    // Without a `using` line, the law closes on the automatic pass with
    // the earlier law it could cite, never with `sum.ofJoined`, which
    // cites it back.
    assert_eq!(
        summary["closed_by"]["sumTwice.doubles"], "steps",
        "{summary}"
    );
    assert_eq!(
        summary["automatic_using"]["sumTwice.doubles"],
        serde_json::json!(["sum.ofJoinedAgain"]),
        "{summary}"
    );
}

/// The laws of a hint are found where the steps stopped, not only in the
/// two sides: in a subject split on, in the comparison of a `holds` law
/// decided by cases, in a case of an induction, and as an earlier law the
/// law does not cite. A fact whose `when` is false where its left side
/// matches is no hint, and neither is one the law does not close with.
/// Those laws stay open; a law steps close by trying each given to induct
/// on is told the `induction` line that goes straight there.
#[test]
fn a_stuck_law_is_hinted_where_the_steps_stopped_and_never_a_false_when() {
    let out = scratch("hints_where");
    let (result, summary) = aver_backend_json("hints_where.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    let hints = |law: &str| -> Vec<String> {
        summary["steps_hints"][law]
            .as_array()
            .map(|a| a.iter().map(|h| h.as_str().unwrap().to_string()).collect())
            .unwrap_or_default()
    };
    // `List.reverse.involutive` rewrites without being cited.
    assert_eq!(summary["closed_by"]["first.ofTwiceReversed"], "steps");
    // The table of equal terms reads the builtin fact
    // `List.len.ofDropAtMost` at `List.drop(xs, n)` without a citation, as
    // evaluation reads the facts it rewrites with, so this law no longer
    // needs the hint it was given.
    assert_eq!(summary["closed_by"]["tail.neverLonger"], "steps");
    for law in ["firstTwo.twiceIsOnce", "count.ofReversed"] {
        assert_eq!(
            summary["closed_by"][law],
            "open",
            "{law}: {}",
            format_output(&result)
        );
    }
    let has = |law: &str, hint: &str| {
        assert!(
            hints(law).iter().any(|h| h.starts_with(hint)),
            "{law}: {hint}: {summary}"
        )
    };
    // Two givens to follow: steps try each, close it and say so.
    assert_eq!(summary["closed_by"]["count.ofJoined"], "steps");
    has(
        "count.ofJoined",
        "steps closed it by trying each given in turn; add `induction xs` to go straight to it",
    );
    has(
        "count.ofReversed",
        "steps close this law with `using [count.ofJoined]`",
    );
    // `List.take.nonPositive` needs `2 <= 0` at `List.take(xs, 2)`.
    assert!(hints("firstTwo.twiceIsOnce").is_empty(), "{summary}");
    // Only the line the law closes with is hinted: no other earlier law or
    // fact that matches where the steps stopped.
    assert_eq!(hints("count.ofReversed").len(), 1, "{summary}");
}

/// A stuck law is hinted a `using` line only when the steps close it with
/// that line. A law that needs two earlier laws, neither enough alone, is
/// hinted both in one line, and closes with the line pasted. A law that
/// matches an earlier law whose `when` it cannot prove is hinted nothing,
/// and neither is one that matches an earlier law that is itself open.
#[test]
fn a_using_hint_is_the_whole_line_the_law_closes_with_or_nothing() {
    let out = scratch("using-hints");
    let (result, summary) = aver_backend_json("using_hints.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    let stuck = [
        "measure.backwardsIsForwards",
        // Its left side matches `size.ofJoinedLong`, `when size(ys) > 2`.
        "size.ofThreeAnyLength",
        "weight.ofReversed",
        // Its left side matches `weight.ofReversed`, which is open.
        "weight.ofReversedJoined",
    ];
    for law in stuck {
        assert_eq!(
            summary["closed_by"][law],
            "open",
            "{law}: {}",
            format_output(&result)
        );
    }
    for law in ["count.ofReversed", "total.ofReversed", "size.ofJoinedLong"] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    assert_eq!(
        summary["steps_hints"],
        serde_json::json!({
            "measure.backwardsIsForwards":
                ["steps close this law with `using [count.ofReversed, total.ofReversed]`"]
        }),
        "{summary}"
    );
    // The line, pasted; and each of its laws alone, which does not close.
    let source = fs::read_to_string(repo_root().join(FIXTURES).join("using_hints.av")).unwrap();
    let at = "    using []\n    measure(xs, ys)";
    assert!(source.contains(at));
    for (line, closes) in [
        ("using [count.ofReversed, total.ofReversed]", "steps"),
        ("using [count.ofReversed]", "open"),
        ("using [total.ofReversed]", "open"),
    ] {
        let dir = scratch("using-hints-applied");
        fs::create_dir_all(&dir).unwrap();
        fs::write(
            dir.join("using_hints.av"),
            source.replace(at, &format!("    {line}\n    measure(xs, ys)")),
        )
        .unwrap();
        let applied = aver_in(
            &dir,
            &[
                "proof",
                "using_hints.av",
                "--backend",
                "aver",
                "--check-json",
                "--sorry-budget",
                "99",
            ],
        );
        let summary: serde_json::Value = serde_json::from_str(
            String::from_utf8_lossy(&applied.stdout)
                .lines()
                .rev()
                .find(|l| l.starts_with('{'))
                .unwrap_or_else(|| panic!("{}", format_output(&applied))),
        )
        .unwrap();
        let _ = fs::remove_dir_all(&dir);
        assert_eq!(
            summary["closed_by"]["measure.backwardsIsForwards"], closes,
            "{line}: {summary}"
        );
    }
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
    let carried = " xs (h_when))";
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
                "(fact Map.get.afterSetOther ((m (map (var K eq) (var V))) (k (var K eq)) (v (var V)) (k2 (var K eq))) (op != (v k) (v k2))",
                "(fact Map.get.afterSetOther ((m (map (var K eq) (var V))) (k (var K eq)) (v (var V)) (k2 (var K eq))) (none)",
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

const CONTAINS_FACT_LAWS: [&str; 4] = [
    "has.keptByConcat",
    "has.readsThroughReverse",
    "has.reversedFront",
    "hasTx.eitherBatch",
];

/// The `List.contains` facts close laws over Int and record elements,
/// cited or where evaluation stops, and are checked with the law; the
/// kernel refuses one proved by the rule for another element or stated at
/// a list of Float, and the steps do not use them over Floats.
#[test]
fn the_contains_facts_close_laws_where_equality_is_equality_and_nowhere_else() {
    let out = scratch("contains-facts");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("contains_facts.av", &out)
            .into_iter()
            .collect();
    assert_eq!(
        files.keys().cloned().collect::<Vec<_>>(),
        CONTAINS_FACT_LAWS
    );
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in files.keys() {
        let text = read(law);
        assert!(text.contains("\n  (fact List.contains."), "{text}");
        assert_eq!(aver::proof_kernel::verdict(&text), Ok(law.clone()));
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "the element in front found where it is another",
            swap(
                "has.keptByConcat",
                "(rule list.contains.cons_other",
                "(rule list.contains.cons_found",
            ),
        ),
        (
            "a fact stated at a list of Float",
            swap(
                "has.readsThroughReverse",
                "((a (list (var K eq))) (y (var K eq)))",
                "((a (list float)) (y float))",
            ),
        ),
    ] {
        let refused = aver::proof_kernel::verdict(&text);
        assert!(refused.is_err(), "{kind}: {refused:?}\n{text}");
    }
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "contains_facts.av",
            "--backend",
            "aver",
            "--check-json",
        ],
    );
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&result.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{}", format_output(&result))),
    )
    .unwrap();
    for law in CONTAINS_FACT_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    assert_eq!(
        summary["closed_by"]["hasFloat.readsThroughReverse"], "open",
        "{summary}"
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_states_each_contains_fact_once_for_every_type_with_a_beq() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("contains-facts-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "contains_facts.av",
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
    for law in CONTAINS_FACT_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let common = fs::read_to_string(out.join("AverCommon.lean")).unwrap();
    for theorem in [
        "theorem AverFacts.list_contains_ofConcat {α : Type} [BEq α] (a : List α) (b : List α) (y : α) :",
        "theorem AverFacts.list_contains_ofReverse {α : Type} [BEq α] (a : List α) (y : α) :",
    ] {
        assert_eq!(common.matches(theorem).count(), 1, "{common}");
    }
    let _ = fs::remove_dir_all(out);
}

const NONEMPTY_LAWS: [&str; 9] = [
    "drained.aCellNeedsACount",
    "init.ofSnoc",
    "last.ofSnoc",
    "one.cellIsNeverEmptied",
    "one.cellIsNotEmpty",
    "one.emptyIsNotACell",
    "one.emptyIsNotASnoc",
    "one.snocIsNotEmpty",
    "one.txCellIsNotEmpty",
];

const NONEMPTY_OPEN_LAWS: [&str; 4] = [
    "isEmptyFloat.never",
    "last.falseOfConcat",
    "one.falseConcatIsNotEmpty",
    "one.falseDropIsNotEmpty",
];

/// A list with a cell in it is not `[]`: the builtin facts say so by its
/// length, for every element type whose `==` is equality, cited, where
/// evaluation stops and where a hypothesis says otherwise; and a `match`
/// that waits on a list builtin over a list variable reads that variable
/// as `[]` and as a cell. The laws true on their samples only stay open,
/// so does the one over Floats, and both kernels refuse a fact stated at
/// Float, a drop read past a count it does not pass, and a split of a
/// variable that is not a list.
#[test]
fn a_cell_is_not_empty_and_a_match_on_a_list_builtin_splits_its_list() {
    let out = scratch("nonempty-facts");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("nonempty_facts.av", &out)
            .into_iter()
            .collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), NONEMPTY_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    let mut all = String::new();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
        all.push_str(&read(law));
    }
    for used in [
        "(fact List.prepend.notEmpty ",
        "(fact List.prepend.notEmptySwapped ",
        "(fact List.concat.notEmpty ",
        "(fact List.concat.notEmptySwapped ",
        "(listcases xsTail ",
    ] {
        assert!(all.contains(used), "{used} is not used");
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "a fact stated at a list of Float",
            swap(
                "one.cellIsNotEmpty",
                "((x (var K eq)) (t (list (var K eq))))",
                "((x float) (t (list float)))",
            ),
        ),
        (
            "a drop read past a count it does not pass",
            swap(
                "one.cellIsNeverEmptied",
                "(rule list.drop.cons_le",
                "(rule list.drop.cons_gt",
            ),
        ),
        (
            "a split of a variable that is not a list",
            swap("last.ofSnoc", "(listcases xsTail ", "(listcases x "),
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
    let summary = steps_summary("nonempty_facts.av", &["--backend", "aver", "--check-json"]);
    for law in NONEMPTY_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    for law in NONEMPTY_OPEN_LAWS {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_checks_the_nonempty_facts_and_the_list_splits() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("nonempty-facts-lean");
    let summary = steps_summary(
        "nonempty_facts.av",
        &[
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "100",
        ],
    );
    for law in NONEMPTY_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let common = fs::read_to_string(out.join("AverCommon.lean")).unwrap();
    for theorem in [
        "theorem AverFacts.list_prepend_notEmpty {α : Type} [BEq α] [LawfulBEq α] (x : α) (t : List α) :",
        "theorem AverFacts.list_concat_notEmpty {α : Type} [BEq α] [LawfulBEq α] (a : List α) (x : α) (b : List α) :",
    ] {
        assert_eq!(common.matches(theorem).count(), 1, "{common}");
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

/// A step taking a `match` arm states its premise with `have`: applied to
/// the premise, a function's type is unified with the equation before a
/// `match` on either side is elaborated, and Lean took the premise for
/// that `match`. A pattern that only names a variable subject leaves
/// `h : x = x`, which `subst` does not take. The export needs no Lean.
#[test]
fn an_arm_step_states_its_premise_with_have() {
    let out = scratch("arm-have");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &["proof", "arm_of_a_match.av", "-o", out.to_str().unwrap()],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("ArmOfAMatch.lean")).unwrap();
    // The law is closed by the steps branch, not the tactics.
    exact_line(&lean, "front_law_frontIsThePoint");
    assert!(!lean.contains("fun h_arm"), "{lean}");
    assert!(
        lean.contains("(have h_arm := (show (n) = (n) from rfl); by (try subst h_arm); rfl)"),
        "{lean}"
    );
    assert!(
        lean.matches("(have h_arm := ").count() >= 3,
        "an arm on a name, one on a call and one on a field: {lean}"
    );
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_takes_an_arm_whose_body_is_a_match() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("arm-of-a-match");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "arm_of_a_match.av",
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
        summary["closed_by"]["front.frontIsThePoint"], "steps",
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

const PICK_LAWS: [&str; 22] = [
    "differs.commutes",
    "differs.swappedEquality",
    "differs.swappedWhen",
    "distance.absNonNegative",
    "distance.absOfLiteral",
    "distance.asAbs",
    "has.emptyHoldsNothing",
    "has.findsAnEqualHead",
    "has.findsItsHead",
    "has.findsPastADifferentHead",
    "has.literalHoldsAnOpenHead",
    "has.literalHoldsNothingLarger",
    "has.ofConcat",
    "has.ofReverse",
    "has.skipsADifferentHead",
    "has.skipsAnother",
    "larger.asMax",
    "larger.maxOfEqual",
    "larger.minPlusMax",
    "smaller.asMin",
    "smaller.minAtMostFirst",
    "smaller.minCommutes",
];

/// The false laws of the fixture: the smaller, the larger and the absolute
/// value are not always the first argument, the element in front of a
/// list counts for `List.contains`, and a list of one Float does not always
/// hold it (`0.0 / 0.0` is not equal to itself).
const PICK_FALSE_LAWS: [&str; 5] = [
    "dropsHead.headNeverCounts",
    "holdsItself.alwaysFindsSelf",
    "smallerIsFirst.absIsIdentity",
    "smallerIsFirst.alwaysFirst",
    "smallerIsFirst.maxAlwaysFirst",
];

/// A script for the false law that a list of one element always holds it,
/// with nothing saying what the element is. It reads the element in front as
/// found, through a rule `list.contains.cons` and `x == x`; for a Float
/// (`0.0 / 0.0`) the VM answers otherwise. The rule that finds the element
/// in front, like `x == x`, holds only at a type whose `==` is equality, so
/// both kernels refuse it.
const CONTAINS_FINDS_ITSELF: &str = "(steps 16
  (obligation containsFloat.alwaysFindsSelf ((x float)) (none)
    (bi List.contains (bi List.prepend (v x) (list)) (v x))
    (b true))
  (defs) (consts) (sums) (recs) (sigs) (laws)
  (proof
    (trans
      ((bi List.contains (bi List.prepend (v x) (list)) (v x))
       (bi Bool.or (op == (v x) (v x)) (bi List.contains (list) (v x)))
       (bi Bool.or (b true) (bi List.contains (list) (v x)))
       (b true))
      (rule list.contains.cons ((x (v x)) (a (list)) (y (v x))))
      (congr (bi Bool.or (hole) (bi List.contains (list) (v x)))
        (rule bool.beq.refl ((a (v x)))))
      (rule bool.or.true_l ((b (bi List.contains (list) (v x))))))))
";

/// `Int.min`, `Int.max` and `Int.abs` are read by the comparison that picks
/// their value, and `List.contains` on `[]` and past an element in front
/// that the value looked for is or is not, each by a wall rule; a false law
/// about them gets no step proof, and both kernels refuse a rule taken for
/// another, and any script that reads the element in front of a list of
/// Float as found.
#[test]
fn both_kernels_read_min_max_abs_and_contains_by_their_rules_and_refuse_mutations() {
    let out = scratch("picks");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("builtin_picks.av", &out).into_iter().collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), PICK_LAWS);
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    let mut all = String::new();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
        all.push_str(&read(law));
    }
    for rule in [
        "int.min.le",
        "int.min.gt",
        "int.max.le",
        "int.max.gt",
        "int.abs.neg",
        "int.abs.nonneg",
        "list.contains.nil",
        "list.contains.cons_found",
        "list.contains.cons_other",
    ] {
        assert!(
            all.contains(&format!("(rule {rule} ")),
            "{rule} is not used"
        );
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "the smaller taken as the second where the first is at most it",
            swap("smaller.asMin", "(rule int.min.le", "(rule int.min.gt"),
        ),
        (
            "the smaller taken as the larger",
            swap("smaller.asMin", "(rule int.min.le", "(rule int.max.le"),
        ),
        (
            "the larger taken as the first where the first is at most the second",
            swap("larger.asMax", "(rule int.max.le", "(rule int.max.gt"),
        ),
        (
            "the absolute value of a negative taken as itself",
            swap(
                "distance.absNonNegative",
                "(rule int.abs.neg",
                "(rule int.abs.nonneg",
            ),
        ),
        (
            "the absolute value of a negative left negated twice",
            swap(
                "distance.absNonNegative",
                "(op >= (op - (i 0) (v a)) (i 0)) (b true))",
                "(op >= (v a) (i 0)) (b true))",
            ),
        ),
        (
            "the empty list holding the element",
            swap("has.emptyHoldsNothing", "(b false)", "(b true)"),
        ),
        (
            "an element in front skipped with nothing showing the value looked for differs",
            swap(
                "has.skipsAnother",
                "(compute (op == (i 2) (i 1)) (b false))",
                "(compute (bi Bool.not (b true)) (b false))",
            ),
        ),
        (
            "an element in front skipped though it is the one looked for",
            swap(
                "has.skipsAnother",
                "(rule list.contains.cons_other ((x (i 1)) (a (v acc)) (y (i 2))) (compute (op == (i 2) (i 1)) (b false)))",
                "(rule list.contains.cons_other ((x (i 2)) (a (v acc)) (y (i 2))) (compute (op == (i 2) (i 2)) (b false)))",
            ),
        ),
        (
            "an element in front skipped by the comparison the other way round, unswapped",
            swap(
                "has.skipsADifferentHead",
                "(trans ((op == (v y) (v h)) (op == (v h) (v y)) (b false)) (rule bool.beq.symm ((a (v y)) (b (v h)))) (cases ",
                "(trans ((op == (v y) (v h)) (op == (v h) (v y)) (b false)) (refl (op == (v y) (v h))) (cases ",
            ),
        ),
        (
            "a list with an element in front read as the empty list",
            swap(
                "has.skipsAnother",
                "(rule list.contains.cons_other ((x (i 1)) (a (v acc)) (y (i 2))) (compute (op == (i 2) (i 1)) (b false)))",
                "(rule list.contains.nil ((y (i 2))))",
            ),
        ),
        (
            "the element in front read as found, by x == x of a term of any type",
            CONTAINS_FINDS_ITSELF.to_string(),
        ),
        (
            "the element in front read as found, by the rule that skips it",
            CONTAINS_FINDS_ITSELF.replace(
                "(rule list.contains.cons ((x (v x)) (a (list)) (y (v x))))",
                "(rule list.contains.cons_other ((x (v x)) (a (list)) (y (v x))) (rule bool.beq.refl ((a (v x)))))",
            ),
        ),
        (
            "the element in front of a list of Float read as found by its rule",
            format!(
                "{}(proof (rule list.contains.cons_found ((x (v x)) (a (list)) (y (v x))) (rule bool.beq.refl ((a (v x)))))))\n",
                &CONTAINS_FINDS_ITSELF[..CONTAINS_FINDS_ITSELF.find("(proof").unwrap()]
            ),
        ),
        (
            "an element in front that is not the one looked for read as found",
            swap(
                "has.skipsADifferentHead",
                "(rule list.contains.cons_other",
                "(rule list.contains.cons_found",
            ),
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
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "builtin_picks.av",
            "--backend",
            "aver",
            "--check-json",
        ],
    );
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&result.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{}", format_output(&result))),
    )
    .unwrap();
    for law in PICK_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    for law in PICK_FALSE_LAWS {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_reads_min_max_abs_and_contains_by_their_rules_and_refuses_a_mutation() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("picks-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "builtin_picks.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "5",
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
    for law in PICK_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    for law in PICK_FALSE_LAWS {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("BuiltinPicks.lean")).unwrap();
    for lemma in [
        "int_min_le",
        "int_min_gt",
        "int_max_le",
        "int_max_gt",
        "int_abs_neg",
        "int_abs_nonneg",
        "list_contains_nil",
        "list_contains_cons_other",
    ] {
        assert!(lean.contains(&format!("AverSteps.{lemma}")), "{lemma}");
    }
    let law = "distance.asAbs";
    let line = exact_line(&lean, "distance_law_asAbs").to_string();
    let wrong = line.replacen("AverSteps.int_abs_neg", "AverSteps.int_abs_nonneg", 1);
    assert_ne!(line, wrong, "{line}");
    assert!(
        lean_refuses(
            &out,
            "BuiltinPicks.lean",
            &lean.replacen(&line, &wrong, 1),
            law
        ),
        "the absolute value of a negative taken as itself: Lean must refuse the step term"
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
fn the_embedded_kernel_and_standard_plans_are_generated_from_the_aver_source() {
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
    // The law's own script proves nothing again: it cites its obligations,
    // each at the givens as they stand, with the reasons before it.
    let composed = read("count.doubledNonNegative");
    assert!(
        composed.contains("(law count.doubledNonNegative.because1 ((n (v n))))")
            && composed
                .contains("(law count.doubledNonNegative.implication ((n (v n))) (hyp h_reason0))")
            && !composed.contains("(linear "),
        "{composed}"
    );
    let grows = read("twice.grows");
    assert!(
        grows.contains("(law twice.grows.because2 ")
            && !grows.contains("(unfold ")
            && !grows.contains("(linear "),
        "{grows}"
    );
    let doubled = read("count.doubledNonNegative.implication");
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
            "a reason cut in from the other obligation",
            mutate_proof(
                &composed,
                "(law count.doubledNonNegative.because1 ",
                "(law count.doubledNonNegative.implication ",
            ),
        ),
        (
            "the implication cited without the reason it assumes",
            mutate_proof(&composed, " (hyp h_reason0))", ")"),
        ),
        (
            "a reason cited at other givens",
            mutate_proof(
                &grows,
                "(law twice.grows.because2 ((x (v x)))",
                "(law twice.grows.because2 ((x (i 1)))",
            ),
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

/// The laws of `case_hypotheses.av` that close by steps only once a
/// hypothesis is read in the form the case split and evaluation gave it.
const CASE_HYPOTHESIS_LAWS: [&str; 2] = ["inRange.dropStaysInRange", "keep.idempotent"];

/// An induction hypothesis (or a line of one) is stated about the terms as
/// they stood before the case split; the claim has evaluated further. The
/// producer states each such line again in its normal form under the case's
/// hypotheses, and the kernel checks that evaluation. The normal form of
/// the case where the head is kept, stated where the head is dropped, is
/// refused; and a law that does not hold stays open.
#[test]
fn both_kernels_read_a_hypothesis_as_the_case_evaluated_it_and_refuse_the_wrong_case() {
    let out = scratch("case-hypotheses");
    let (result, summary) = aver_backend_json("case_hypotheses.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in CASE_HYPOTHESIS_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    assert_eq!(summary["closed_by"]["keep.keepsEverything"], "open");
    let export = scratch("case-hypotheses-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("case_hypotheses.av", &export)
            .into_iter()
            .collect();
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in CASE_HYPOTHESIS_LAWS {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.to_string()));
    }
    // The hypothesis where the head `h_c0` is kept, in its normal form,
    // and the form it has where the head is dropped.
    let law = read("keep.idempotent.because1");
    let kept = "(op == (bi List.prepend (v h_c0) (call keep (call keep (v rest_c0) (v t)) (v t))) \
                (bi List.prepend (v h_c0) (call keep (v rest_c0) (v t))))";
    let dropped =
        "(op == (call keep (call keep (v rest_c0) (v t)) (v t)) (call keep (v rest_c0) (v t)))";
    let at = law
        .find(&format!(" {kept} "))
        .expect("the kept case's hypothesis");
    assert!(
        law[..at]
            .trim_end()
            .rsplit(' ')
            .next()
            .unwrap()
            .starts_with("h_steps")
    );
    let mutant = format!("{} {dropped} {}", &law[..at], &law[at + kept.len() + 2..]);
    let refused = aver::proof_kernel::verdict(&mutant);
    assert!(
        refused
            .as_ref()
            .is_err_and(|why| why.starts_with("step proof")),
        "{refused:?}"
    );
    let path = export.join("mutant.steps");
    fs::write(&path, &mutant).unwrap();
    let replayed = replay(std::slice::from_ref(&path));
    assert!(!replayed.status.success(), "{}", format_output(&replayed));
    let _ = fs::remove_dir_all(export);
}

/// The kernel checks a script on a stack of its own, so the thread that
/// asks does not decide whether a deep proof is checked: these scripts need
/// more than the 512 KiB the asking thread has here, and are accepted, and
/// a wrong one is still refused.
#[test]
fn the_kernel_checks_a_deep_proof_for_a_thread_with_a_small_stack() {
    let export = scratch("small-stack-export");
    let scripts: Vec<(String, String)> = export_steps("case_hypotheses.av", &export)
        .into_iter()
        .map(|(law, path)| (law, fs::read_to_string(path).unwrap()))
        .collect();
    let _ = fs::remove_dir_all(export);
    for law in CASE_HYPOTHESIS_LAWS {
        assert!(scripts.iter().any(|(key, _)| key == law), "{law}");
    }
    let verdicts = std::thread::Builder::new()
        .stack_size(512 * 1024)
        .spawn(move || {
            scripts
                .into_iter()
                .map(|(law, text)| {
                    // A variable no case introduces.
                    let wrong = text.replacen("(v h_c0)", "(v h_c9)", 1);
                    (
                        law,
                        aver::proof_kernel::verdict(&text),
                        aver::proof_kernel::verdict(&wrong),
                    )
                })
                .collect::<Vec<_>>()
        })
        .unwrap()
        .join()
        .expect("the kernel does not run on the asking thread's stack");
    let mut refusals = 0;
    for (law, accepted, of_wrong) in verdicts {
        assert_eq!(accepted, Ok(law.clone()));
        if of_wrong != accepted {
            assert!(of_wrong.is_err(), "{law}: {of_wrong:?}");
            refusals += 1;
        }
    }
    assert!(refusals > 0, "no script was mutated");
}

#[test]
fn lean_closes_the_case_hypothesis_laws_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("case-hypotheses-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "case_hypotheses.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
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
    for law in CASE_HYPOTHESIS_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The laws of `swapped_equality.av` that close by steps only once an
/// equality is read with its sides swapped (`bool.beq.symm`).
const SWAPPED_EQUALITY_LAWS: [&str; 2] = ["order.symmetric", "orderTag.symmetric"];

/// A case split states `b == a`, the claim evaluates `a == b`: the steps
/// swap the sides with `bool.beq.symm` and read the hypothesis, for either
/// value it states. Not for a record that holds a Float, whose `==` Lean
/// does not read as equality. Both kernels refuse the rule at the swapped
/// substitution, and the step that would read a `false` hypothesis as
/// `true`.
#[test]
fn both_kernels_swap_the_sides_of_an_equality_and_refuse_a_wrong_orientation() {
    let out = scratch("swapped-equality");
    let (result, summary) = aver_backend_json("swapped_equality.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in SWAPPED_EQUALITY_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    assert_eq!(summary["closed_by"]["orderReading.symmetric"], "open");
    let export = scratch("swapped-equality-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("swapped_equality.av", &export)
            .into_iter()
            .collect();
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in SWAPPED_EQUALITY_LAWS {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.to_string()));
    }
    let law = read("order.symmetric");
    let rule = "(rule bool.beq.symm ((a (v a)) (b (v b))))";
    assert!(law.contains(rule), "{law}");
    let swapped = law.replacen(rule, "(rule bool.beq.symm ((a (v b)) (b (v a))))", 1);
    let false_case = "(op == (v b) (v a)) (b false))";
    assert!(law.contains(false_case), "{law}");
    let as_true = law.replacen(false_case, "(op == (v b) (v a)) (b true))", 1);
    for (i, mutant) in [swapped, as_true].into_iter().enumerate() {
        let refused = aver::proof_kernel::verdict(&mutant);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{refused:?}"
        );
        let path = export.join(format!("mutant{i}.steps"));
        fs::write(&path, &mutant).unwrap();
        let replayed = replay(std::slice::from_ref(&path));
        assert!(!replayed.status.success(), "{}", format_output(&replayed));
    }
    let _ = fs::remove_dir_all(export);
}

#[test]
fn lean_closes_the_swapped_equality_laws_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("swapped-equality-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "swapped_equality.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
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
    for law in SWAPPED_EQUALITY_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The laws of `case_contradictions.av` that close only because a case
/// their hypotheses rule out is closed as impossible, each by one reading:
/// the lengths of a list equation, a `when` that evaluates to `false`, two
/// hypotheses with one normal form and different values, and two different
/// constructors that hold a variable.
const CASE_CONTRADICTION_LAWS: [&str; 4] = [
    "hasTop.nonEmpty",
    "place.growsWhenItStays",
    "checked.zeroWhenOn",
    "refusal.heldIsRefused",
];

/// A false law of the same shape as each of [`CASE_CONTRADICTION_LAWS`].
const CASE_CONTRADICTION_FALSE_LAWS: [&str; 4] = [
    "hasTop.anyList",
    "place.growsAlways",
    "checked.levelWhenOn",
    "refusal.anythingIsRefused",
];

/// `text` with the first `(cases ON HYP TRUE FALSE)` step's two cases
/// swapped.
fn swap_first_cases(text: &str) -> String {
    let start = text.find("(proof ").expect("a script has a proof");
    let open = start + text[start..].find("(cases ").expect("a cases step");
    let forms = sub_forms(text, open);
    // (cases ON HYP TRUE FALSE): ON and the two cases are forms.
    let (t, f) = (forms[1], forms[2]);
    format!(
        "{}{}{}{}{}",
        &text[..t.0],
        &text[f.0..f.1],
        &text[t.1..f.0],
        &text[t.0..t.1],
        &text[f.1..]
    )
}

/// After a case split the hypotheses are read again together: each reading
/// is a step both kernels check, a wrong one is refused, and a false law of
/// the same shape stays open.
#[test]
fn both_kernels_close_a_case_its_hypotheses_rule_out_and_refuse_a_wrong_reading() {
    let out = scratch("case-contradictions");
    let (result, summary) = aver_backend_json("case_contradictions.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in CASE_CONTRADICTION_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    for law in CASE_CONTRADICTION_FALSE_LAWS {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    let export = scratch("case-contradictions-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("case_contradictions.av", &export)
            .into_iter()
            .collect();
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in CASE_CONTRADICTION_LAWS {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.to_string()));
    }
    // A wrong length, a connective read the wrong way, the wrong
    // hypothesis, and two different constructors read as equal.
    let refusal = read("refusal.heldIsRefused");
    let clash = refusal
        .find("(compute (op == (ctor Option.Some")
        .expect("the constructors decided by compute");
    let end = closing(&refusal, clash);
    let ctor_equal = format!(
        "{}{}{}",
        &refusal[..clash],
        refusal[clash..=end].replace("(b false))", "(b true))"),
        &refusal[end + 1..]
    );
    let mutants = [
        mutate_proof(
            &read("hasTop.nonEmpty"),
            "(op == (bi List.len (v xs)) (i 0))",
            "(op == (bi List.len (v xs)) (i 1))",
        ),
        mutate_proof(
            &read("place.growsWhenItStays"),
            "(compute (bi Bool.not (b true)) (b false))",
            "(compute (bi Bool.not (b true)) (b true))",
        ),
        mutate_proof(
            &read("checked.zeroWhenOn"),
            "(hyp h_steps7))) (hyp when))",
            "(hyp h_steps7))) (hyp h_steps1))",
        ),
        ctor_equal,
    ];
    for (i, mutant) in mutants.into_iter().enumerate() {
        let refused = aver::proof_kernel::verdict(&mutant);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "mutant {i}: {refused:?}"
        );
        let path = export.join(format!("mutant{i}.steps"));
        fs::write(&path, &mutant).unwrap();
        let replayed = replay(std::slice::from_ref(&path));
        assert!(!replayed.status.success(), "{}", format_output(&replayed));
    }
    let _ = fs::remove_dir_all(export);
}

#[test]
fn lean_closes_the_case_contradiction_laws_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("case-contradictions-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "case_contradictions.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
            "--declined-budget",
            "99",
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
    for law in CASE_CONTRADICTION_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The laws of `open_orders.av`: sides that differ only in the order of a
/// `Bool.or` or a `Bool.and`, and a countdown stopped at a guard nothing in
/// scope decides, in a side or in a hypothesis.
const OPEN_ORDER_LAWS: [&str; 4] = [
    "inserted.bothInFront",
    "inserted.keepsEveryValue",
    "nthFrom.firstOrZero",
    "nthFrom.repeatedHead",
];

/// Steps split on the left argument of the connective, or on the guard of
/// the countdown, with the existing `cases` step, and each case evaluates:
/// both kernels accept the scripts and refuse them with the two cases
/// swapped. Laws of the same shapes that hold on their samples only stay
/// open.
#[test]
fn both_kernels_split_on_an_open_order_and_refuse_swapped_cases() {
    let out = scratch("open-orders");
    let (result, summary) = aver_backend_json("open_orders.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in OPEN_ORDER_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    let export = scratch("open-orders-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("open_orders.av", &export)
            .into_iter()
            .collect();
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in OPEN_ORDER_LAWS {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.to_string()));
    }
    for (i, law) in ["inserted.bothInFront", "nthFrom.repeatedHead"]
        .into_iter()
        .enumerate()
    {
        let text = read(law);
        let mutant = swap_first_cases(&text);
        assert_ne!(mutant, text, "{law}: the mutant changed nothing");
        let refused = aver::proof_kernel::verdict(&mutant);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{law}: {refused:?}"
        );
        let path = export.join(format!("mutant{i}.steps"));
        fs::write(&path, &mutant).unwrap();
        let replayed = replay(std::slice::from_ref(&path));
        assert!(!replayed.status.success(), "{}", format_output(&replayed));
    }
    let _ = fs::remove_dir_all(export);
    let refused = scratch("open-orders-refused");
    assert!(export_steps("open_orders_refused.av", &refused).is_empty());
    let _ = fs::remove_dir_all(refused);
}

#[test]
fn lean_closes_the_open_order_laws_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("open-orders-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "open_orders.av",
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
    for law in OPEN_ORDER_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The laws of `stated_empty.av`: a `when` says whether a list given is
/// empty, and nothing else in scope reads the list.
const STATED_EMPTY_LAWS: [&str; 3] = [
    "firstOr.emptyOnTheLeft",
    "firstOr.notEmptyHasALength",
    "middle.withinAList",
];

/// Steps split the list into `[]` and a cell with the existing `listcases`
/// step, the `when` is false in one case and the length of the cell reads
/// in the other: both kernels accept the scripts and refuse them with the
/// empty case read wrong. Laws of the same shapes that hold on their
/// samples only stay open.
#[test]
fn both_kernels_split_a_list_a_hypothesis_calls_empty_and_refuse_a_wrong_case() {
    let out = scratch("stated-empty");
    let (result, summary) = aver_backend_json("stated_empty.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in STATED_EMPTY_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    let export = scratch("stated-empty-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("stated_empty.av", &export)
            .into_iter()
            .collect();
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for (i, law) in STATED_EMPTY_LAWS.into_iter().enumerate() {
        let text = read(law);
        assert_eq!(aver::proof_kernel::verdict(&text), Ok(law.to_string()));
        assert!(text.contains("(listcases values "), "{law}: {text}");
        // The empty case read as if the `when` held there.
        let wrong = "(compute (op != (list) (list)) (b false))";
        assert!(text.contains(wrong), "{law}: {text}");
        let mutant = text.replacen(wrong, "(compute (op != (list) (list)) (b true))", 1);
        let refused = aver::proof_kernel::verdict(&mutant);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "{law}: {refused:?}"
        );
        let path = export.join(format!("mutant{i}.steps"));
        fs::write(&path, &mutant).unwrap();
        let replayed = replay(std::slice::from_ref(&path));
        assert!(!replayed.status.success(), "{}", format_output(&replayed));
    }
    let _ = fs::remove_dir_all(export);
    let refused = scratch("stated-empty-refused");
    assert!(export_steps("stated_empty_refused.av", &refused).is_empty());
    let _ = fs::remove_dir_all(refused);
}

#[test]
fn lean_closes_the_stated_empty_laws_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("stated-empty-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "stated_empty.av",
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
    for law in STATED_EMPTY_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The laws of `evaluated_hypotheses.av` that close only once the
/// hypotheses of a case are evaluated the way its claim is: a `when` that
/// evaluates to the claim, a line of an evaluated `when` that decides what
/// the claim stops at, and a `when` that evaluates to `false` where another
/// line used to answer for it.
const EVALUATED_HYPOTHESIS_LAWS: [&str; 3] = [
    "has.keptWasThere",
    "sorted.consUnderItsHead",
    "has.nothingKeepsNone",
];

/// A false law of the same shape as each of [`EVALUATED_HYPOTHESIS_LAWS`]:
/// its hypothesis evaluates to something close to what the claim needs, but
/// not to it.
const EVALUATED_HYPOTHESIS_FALSE_LAWS: [&str; 3] = [
    "has.keptOtherWasThere",
    "sorted.consUnderAnotherHead",
    "has.allKeepsSome",
];

/// Where a case stays open, its hypotheses are evaluated the way the claim
/// was and the claim is tried again under their evaluated forms: each form
/// is cut in with the evaluation that proves it, which both kernels check;
/// a wrong evaluation is refused, and a false law of the same shape stays
/// open.
#[test]
fn both_kernels_read_a_hypothesis_as_evaluated_and_refuse_a_wrong_evaluation() {
    let out = scratch("evaluated-hypotheses");
    let (result, summary) = aver_backend_json("evaluated_hypotheses.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in EVALUATED_HYPOTHESIS_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    for law in EVALUATED_HYPOTHESIS_FALSE_LAWS {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    let export = scratch("evaluated-hypotheses-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("evaluated_hypotheses.av", &export)
            .into_iter()
            .collect();
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    for law in EVALUATED_HYPOTHESIS_LAWS {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.to_string()));
    }
    // The evaluated `when` is cut in as the claim itself.
    let kept = read("has.keptWasThere");
    assert!(
        kept.contains("(have h_read1 (call has (v x) (v xs)) "),
        "{kept}"
    );
    // The wrong arm of `kept`, the other line of the evaluated `when`, and
    // `Bool.and(false, b)` read as `b`.
    let mutants = [
        mutate_proof(
            &kept,
            "(unfold kept 1 ((ctor Mode.All) (v xs))",
            "(unfold kept 2 ((ctor Mode.All) (v xs))",
        ),
        mutate_proof(
            &read("sorted.consUnderItsHead"),
            "(rule bool.and.elim_l ((a (call lessEq (v z) (v a_c0)))",
            "(rule bool.and.elim_r ((a (call lessEq (v z) (v a_c0)))",
        ),
        mutate_proof(
            &read("has.nothingKeepsNone"),
            "(rule bool.and.false_l ((b (call has (v x) (v ys)))))",
            "(rule bool.and.true_l ((b (call has (v x) (v ys)))))",
        ),
    ];
    for (i, mutant) in mutants.into_iter().enumerate() {
        let refused = aver::proof_kernel::verdict(&mutant);
        assert!(
            refused
                .as_ref()
                .is_err_and(|why| why.starts_with("step proof")),
            "mutant {i}: {refused:?}"
        );
        let path = export.join(format!("mutant{i}.steps"));
        fs::write(&path, &mutant).unwrap();
        let replayed = replay(std::slice::from_ref(&path));
        assert!(!replayed.status.success(), "{}", format_output(&replayed));
    }
    let _ = fs::remove_dir_all(export);
}

/// Every law the evaluated hypotheses close, in the fixtures and in the
/// corpus, with the file it is in and the root its modules resolve from.
const EVALUATED_HYPOTHESIS_ROOM: [(&str, &str, &[&str]); 10] = [
    (
        "tests/fixtures/proof_steps/evaluated_hypotheses.av",
        "tests/fixtures/proof_steps",
        &EVALUATED_HYPOTHESIS_LAWS,
    ),
    (
        "examples/formal/affine_wrapper_law.av",
        "examples/formal",
        &["boundedScale.returnsCore"],
    ),
    (
        "projects/durable_promise/domain/promise.av",
        "projects/durable_promise",
        &["observe.timeoutMonotone"],
    ),
    (
        "proof-corpus/decomposed/isaplanner/prop_77.av",
        "proof-corpus/decomposed/isaplanner",
        &["insort.leHeadInsort", "sorted.sortedCons"],
    ),
    (
        "proof-corpus/decomposed/isaplanner/prop_78.av",
        "proof-corpus/decomposed/isaplanner",
        &["insort.leHeadInsort", "sorted.sortedCons"],
    ),
    (
        "proof-corpus/decomposed/prod/lemma_09.av",
        "proof-corpus/decomposed/prod",
        &["rev.revAppendCons", "rev.revAppendDistributes"],
    ),
    (
        "proof-corpus/decomposed/prod/lemma_12.av",
        "proof-corpus/decomposed/prod",
        &["insert.leHeadInsert", "sorted.sortedCons"],
    ),
    (
        "proof-corpus/decomposed/prod/prop_10.av",
        "proof-corpus/decomposed/prod",
        &["rev.revAppendDistributes"],
    ),
    (
        "proof-corpus/decomposed/prod/prop_11.av",
        "proof-corpus/decomposed/prod",
        &["rev.revAppDistributes"],
    ),
    (
        "proof-corpus/decomposed/prod/prop_14.av",
        "proof-corpus/decomposed/prod",
        &["insert.leHeadInsert", "sorted.sortedCons"],
    ),
];

/// A line of a `when` of two lines that the whole `when` answers is read
/// through its body, as a last attempt: the law of `hypothesis_body.av`
/// closes by the lines that body gives, which both the kernel and the
/// replayer check; taking the wrong line of the body is refused, and a false
/// law of the same shape stays open.
#[test]
fn a_hypothesis_its_restatement_answers_is_read_through_its_body() {
    let out = scratch("hypothesis-body");
    let (result, summary) = aver_backend_json("hypothesis_body.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    assert_eq!(
        summary["closed_by"]["parse.exactUnderBoth"],
        "steps",
        "{}",
        format_output(&result)
    );
    assert_eq!(
        summary["closed_by"]["parse.reversedUnderBoth"], "open",
        "{summary}"
    );
    let export = scratch("hypothesis-body-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("hypothesis_body.av", &export)
            .into_iter()
            .collect();
    let script = fs::read_to_string(&files["parse.exactUnderBoth"]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&script),
        Ok("parse.exactUnderBoth".to_string())
    );
    // The body of `parseReason`, opened at the call `when2` states, gives
    // its two lines, cut in by the `Bool.and` eliminations.
    let read = script
        .find("(have h_steps1 ")
        .unwrap_or_else(|| panic!("{script}"));
    assert!(
        script[read..].contains("(unfold parseReason 0 ((v bytes) (list))"),
        "{script}"
    );
    let at = read + script[read..].find("(rule bool.and.elim_l ").unwrap();
    let mutant = format!(
        "{}(rule bool.and.elim_r {}",
        &script[..at],
        &script[at + "(rule bool.and.elim_l ".len()..]
    );
    let refused = aver::proof_kernel::verdict(&mutant);
    assert!(
        refused
            .as_ref()
            .is_err_and(|why| why.starts_with("step proof")),
        "{refused:?}"
    );
    let path = export.join("mutant.steps");
    fs::write(&path, &mutant).unwrap();
    let replayed = replay(std::slice::from_ref(&path));
    assert!(!replayed.status.success(), "{}", format_output(&replayed));
    let _ = fs::remove_dir_all(export);
}

/// The budgets for evaluating hypotheses have room: every law those
/// readings close still closes with each budget at half its size
/// (`AVER_STEPS_READ_BUDGET_PERCENT=50`). A change that makes a reading
/// cost more fails here, by name, while the laws still close at the full
/// budgets, instead of reopening one in the corpus later. No clock is read.
#[test]
fn the_budgets_for_evaluated_hypotheses_leave_half_of_themselves_unused() {
    for (i, (entry, root, laws)) in EVALUATED_HYPOTHESIS_ROOM.iter().enumerate() {
        let out = scratch(&format!("evaluated-hypotheses-room-{i}"));
        let result = Command::new(aver_bin())
            .args([
                "proof",
                entry,
                "--module-root",
                root,
                "--backend",
                "aver",
                "-o",
                out.to_str().unwrap(),
                "--check-json",
                "--sorry-budget",
                "99",
                "--declined-budget",
                "99",
            ])
            .env("AVER_STEPS_READ_BUDGET_PERCENT", "50")
            .current_dir(repo_root())
            .output()
            .expect("aver runs");
        let _ = fs::remove_dir_all(&out);
        let summary: serde_json::Value = serde_json::from_str(
            String::from_utf8_lossy(&result.stdout)
                .lines()
                .rev()
                .find(|l| l.starts_with('{'))
                .unwrap_or_else(|| panic!("{entry}: {}", format_output(&result))),
        )
        .unwrap();
        for law in *laws {
            assert_eq!(
                summary["closed_by"][law], "steps",
                "{entry}: {law} needs more than half of a budget for evaluating hypotheses"
            );
        }
    }
}

#[test]
fn lean_closes_the_evaluated_hypothesis_laws_by_their_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("evaluated-hypotheses-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "evaluated_hypotheses.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
            "--declined-budget",
            "99",
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
    for law in EVALUATED_HYPOTHESIS_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The hint the steps give for `total.isPlain` of `accumulator_hint.av`,
/// word for word.
const ACCUMULATOR_HINT: &str =
    "steps close this law once the accumulator of `totalFrom` is a given; add this law before it:

    verify totalFrom law accGeneralizes
        given xs: List<Int> = [[], [1], [1, 2, 3]]
        given acc: Int = [0, 1, 5]
        totalFrom(xs, acc) => acc + plain(xs)

    and this line to `total.isPlain`:

        using [totalFrom.accGeneralizes]";

/// `source` with an accumulator hint for law `law` (`fn.name`) applied as
/// it says, nothing reworded: the hinted law before the law, the `using`
/// line after the law's givens.
fn apply_accumulator_hint(source: &str, law: &str, hint: &str) -> String {
    let parts: Vec<&str> = hint.split("\n\n").collect();
    assert_eq!(parts.len(), 4, "{hint}");
    let block: Vec<&str> = parts[1]
        .lines()
        .map(|l| l.strip_prefix("    ").expect("an indented block"))
        .collect();
    let using = parts[3].trim();
    let (f, name) = law.split_once('.').expect("fn.law");
    let header = format!("verify {f} law {name}");
    let mut out = Vec::new();
    let mut inside = false;
    for line in source.lines() {
        if line == header {
            out.extend(block.iter().map(|l| l.to_string()));
            out.push(String::new());
            inside = true;
        } else if inside && !line.trim_start().starts_with("given ") {
            out.push(format!("    {using}"));
            inside = false;
        }
        out.push(line.to_string());
    }
    out.join("\n") + "\n"
}

/// A law about a wrapper that starts an accumulator at a constant stays
/// open, and the steps hint the law about the recursion, with the
/// accumulator as a given, that they proved and under which the wrapper's
/// law closes. Pasted as the hint says, both close. A false law of the same
/// shape gets no hint, nor does one whose start is no neutral element.
#[test]
fn a_wrapper_over_an_accumulator_is_hinted_the_law_that_closes_it() {
    let out = scratch("accumulator-hint");
    let (result, summary) = aver_backend_json("accumulator_hint.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in ["total.isPlain", "total.isFirst", "shifted.isPlainShifted"] {
        assert_eq!(
            summary["closed_by"][law],
            "open",
            "{law}: {}",
            format_output(&result)
        );
    }
    let hints = |law: &str| -> Vec<String> {
        summary["steps_hints"][law]
            .as_array()
            .map(|hs| {
                hs.iter()
                    .filter_map(|h| h.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default()
    };
    assert!(
        hints("total.isPlain").iter().any(|h| h == ACCUMULATOR_HINT),
        "{:#?}",
        hints("total.isPlain")
    );
    for law in ["total.isFirst", "shifted.isPlainShifted"] {
        assert!(
            !hints(law).iter().any(|h| h.contains("accGeneralizes")),
            "{law}: {:#?}",
            hints(law)
        );
    }
    // The hint, pasted.
    let source =
        fs::read_to_string(repo_root().join(FIXTURES).join("accumulator_hint.av")).unwrap();
    let dir = scratch("accumulator-hint-applied");
    fs::create_dir_all(&dir).unwrap();
    fs::write(
        dir.join("accumulator_hint.av"),
        apply_accumulator_hint(&source, "total.isPlain", ACCUMULATOR_HINT),
    )
    .unwrap();
    let applied_out = dir.join("out");
    let applied = aver_in(
        &dir,
        &[
            "proof",
            "accumulator_hint.av",
            "--backend",
            "aver",
            "-o",
            applied_out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
        ],
    );
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&applied.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{}", format_output(&applied))),
    )
    .unwrap();
    let _ = fs::remove_dir_all(&dir);
    for law in ["totalFrom.accGeneralizes", "total.isPlain"] {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    assert_eq!(summary["closed_by"]["total.isFirst"], "open", "{summary}");
}

/// A law whose `because` line the steps cannot prove, and that closes by
/// its own induction without it, is hinted to drop the line; pasted, the
/// law closes. A law that needs its reason is hinted nothing, and neither
/// is one that closes without it only by citing a law that is open.
#[test]
fn a_because_line_the_law_closes_without_is_hinted_away() {
    let out = scratch("because-hint");
    let (result, summary) = aver_backend_json("because_hint.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    let open = [
        "total.ofJoined",
        "twice.squareNonNegative",
        "square.nonNegative",
        "square.stillNonNegative",
    ];
    for law in open {
        assert_eq!(
            summary["closed_by"][law],
            "open",
            "{law}: {}",
            format_output(&result)
        );
    }
    assert_eq!(
        summary["steps_hints"],
        serde_json::json!({
            "total.ofJoined":
                ["steps close this law without its `because` line; remove it"]
        }),
        "{summary}"
    );
    // The hint, pasted.
    let source = fs::read_to_string(repo_root().join(FIXTURES).join("because_hint.av")).unwrap();
    let line = "    because total(xs) * total(xs) >= 0\n";
    assert!(source.contains(line));
    let dir = scratch("because-hint-applied");
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("because_hint.av"), source.replace(line, "")).unwrap();
    let applied_out = dir.join("out");
    let applied = aver_in(
        &dir,
        &[
            "proof",
            "because_hint.av",
            "--backend",
            "aver",
            "-o",
            applied_out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
        ],
    );
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&applied.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{}", format_output(&applied))),
    )
    .unwrap();
    let _ = fs::remove_dir_all(&dir);
    assert_eq!(summary["closed_by"]["total.ofJoined"], "steps", "{summary}");
}

/// A law the steps leave open for want of two lemmas about `plus` is hinted
/// both, stated from where the steps stop; pasted as the hint writes them,
/// the lemmas and the law close by steps. A law citing an open law and a
/// false law, whose only candidate fails on samples, get no hint.
#[test]
fn a_missing_lemma_stated_where_the_steps_stop_is_hinted() {
    let out = scratch("lemma-hint");
    let (result, summary) = aver_backend_json("lemma_hint.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in ["plus.commutes", "plus.commutesBack", "plus.overshoots"] {
        assert_eq!(
            summary["closed_by"][law],
            "open",
            "{law}: {}",
            format_output(&result)
        );
    }
    let hints = summary["steps_hints"].as_object().expect("hints");
    assert_eq!(
        hints.keys().collect::<Vec<_>>(),
        ["plus.commutes"],
        "{summary}"
    );
    let hint = hints["plus.commutes"][0].as_str().unwrap();
    assert!(
        hint.contains("        plus(y, Nat.Z) => y\n")
            && hint.contains("        plus(y, Nat.S(z)) => Nat.S(plus(y, z))\n"),
        "{hint}"
    );
    // The hint, pasted: the laws before the law, the line into it.
    let (laws, line) = hint
        .split_once(":\n\n")
        .and_then(|(_, rest)| rest.split_once("    and this line to `plus.commutes`:\n\n"))
        .expect("the hint's parts");
    let laws: String = laws
        .lines()
        .map(|l| format!("{}\n", l.strip_prefix("    ").unwrap_or(l)))
        .collect();
    let line = format!("    {}\n", line.trim());
    let source = fs::read_to_string(repo_root().join(FIXTURES).join("lemma_hint.av")).unwrap();
    let law = "verify plus law commutes\n";
    let claim = "    plus(x, y) => plus(y, x)\n";
    assert!(source.contains(law) && source.contains(claim));
    let pasted = source.replacen(law, &format!("{laws}{law}"), 1).replacen(
        claim,
        &format!("{line}{claim}"),
        1,
    );
    let dir = scratch("lemma-hint-applied");
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("lemma_hint.av"), &pasted).unwrap();
    let applied_out = dir.join("out");
    let applied = aver_in(
        &dir,
        &[
            "proof",
            "lemma_hint.av",
            "--backend",
            "aver",
            "-o",
            applied_out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
        ],
    );
    let summary: serde_json::Value = serde_json::from_str(
        String::from_utf8_lossy(&applied.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{}", format_output(&applied))),
    )
    .unwrap();
    let _ = fs::remove_dir_all(&dir);
    for law in ["plus.commutesLemma", "plus.commutesLemma2", "plus.commutes"] {
        assert_eq!(
            summary["closed_by"][law], "steps",
            "{law}: {pasted}\n{summary}"
        );
    }
}

/// The law of `cited_when_split.av` that closes only by a split on a line
/// of a cited law's `when` that stays open where the law matches.
const CITED_WHEN_SPLIT_LAW: &str = "smallSize.oneDigitRange";

/// Two cited laws match `List.len(digits(n))`, one for `n < 10` and one for
/// `n >= 10`: the steps split on the open line, each case rewrites with the
/// law that now applies, and both kernels check it. The cases swapped are
/// refused, and a false law with the same citations stays open.
#[test]
fn both_kernels_split_on_a_cited_when_and_refuse_the_cases_swapped() {
    let out = scratch("cited-when-split");
    let (result, summary) = aver_backend_json("cited_when_split.av", &out, None);
    let _ = fs::remove_dir_all(&out);
    for law in [
        "digits.smallCount",
        "digits.countStep",
        "digits.positiveHasDigits",
        CITED_WHEN_SPLIT_LAW,
    ] {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    assert_eq!(
        summary["closed_by"]["smallSize.oneDigitWrongRange"], "open",
        "{summary}"
    );
    let export = scratch("cited-when-split-export");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("cited_when_split.av", &export)
            .into_iter()
            .collect();
    let law = fs::read_to_string(&files[CITED_WHEN_SPLIT_LAW]).unwrap();
    assert_eq!(
        aver::proof_kernel::verdict(&law),
        Ok(CITED_WHEN_SPLIT_LAW.to_string())
    );
    assert!(law.contains("(proof (cases (op < (v n) (i 10))"), "{law}");
    let mutant = swap_first_cases(&law);
    let refused = aver::proof_kernel::verdict(&mutant);
    assert!(
        refused
            .as_ref()
            .is_err_and(|why| why.starts_with("step proof")),
        "{refused:?}"
    );
    let path = export.join("mutant.steps");
    fs::write(&path, &mutant).unwrap();
    let replayed = replay(std::slice::from_ref(&path));
    assert!(!replayed.status.success(), "{}", format_output(&replayed));
    let _ = fs::remove_dir_all(export);
}

#[test]
fn lean_closes_the_cited_when_split_law_by_its_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("cited-when-split-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "cited_when_split.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
            "--declined-budget",
            "99",
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
        summary["closed_by"][CITED_WHEN_SPLIT_LAW], "steps",
        "{summary}"
    );
    let _ = fs::remove_dir_all(out);
}

/// The step files `aver proof --backend aver` writes for `fixture` with
/// `AVER_STEPS_MEMO` set to `memo`, by name, and the laws it closes by
/// steps.
fn steps_with_memo(fixture: &str, memo: &str) -> (Vec<(String, String)>, Vec<String>) {
    let out = scratch(&format!("memo-{memo}-{}", fixture.trim_end_matches(".av")));
    let result = Command::new(aver_bin())
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
        .env("AVER_STEPS_MEMO", memo)
        .current_dir(repo_root().join(FIXTURES))
        .output()
        .expect("aver runs");
    let line = String::from_utf8_lossy(&result.stdout)
        .lines()
        .rev()
        .find(|l| l.starts_with('{'))
        .map(str::to_string)
        .unwrap_or_else(|| panic!("AVER_STEPS_MEMO={memo}: {}", format_output(&result)));
    let summary: serde_json::Value = serde_json::from_str(&line).unwrap();
    let mut files: Vec<(String, String)> = fs::read_dir(out.join("proof_steps"))
        .map(|dir| {
            dir.map(|e| e.unwrap().path())
                .map(|p| {
                    let name = p.file_name().unwrap().to_string_lossy().to_string();
                    (name, fs::read_to_string(&p).unwrap())
                })
                .collect()
        })
        .unwrap_or_default();
    files.sort();
    let _ = fs::remove_dir_all(out);
    (files, closed_by_steps(&summary))
}

/// Evaluations the producer remembers are replayed exactly: with the
/// memo on, off, or checked against evaluating again (which stops on any
/// difference), every law closes the same way with the same step file.
#[test]
fn remembered_evaluations_give_the_same_proofs() {
    for fixture in [
        "btc_shapes.av",
        "case_contradictions.av",
        "case_hypotheses.av",
        "cited_when_split.av",
        "countdown.av",
        "finite.av",
        "halving.av",
        "lock.av",
        "normal_form.av",
        "split.av",
    ] {
        let (on, closed) = steps_with_memo(fixture, "on");
        assert!(!on.is_empty(), "{fixture}: no step files");
        assert_eq!(
            steps_with_memo(fixture, "off"),
            (on.clone(), closed.clone()),
            "{fixture}: memo off"
        );
        assert_eq!(
            steps_with_memo(fixture, "check"),
            (on, closed),
            "{fixture}: memo checked"
        );
    }
}

/// A module's `using [k.isF]` names its own law `k.isF`, never the entry's
/// law of the same name: the entry's law cites the module's back, and each
/// script would check with the other law taken as given, so the pair would
/// count as closed with nothing proved.
#[test]
fn a_module_law_never_cites_an_entry_law_of_the_same_name() {
    let dir = repo_root().join(FIXTURES).join("citation_cycle");
    let out = scratch("citation-cycle");
    let result = aver_in(
        &dir,
        &[
            "proof",
            "main.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let _ = fs::remove_dir_all(&out);
    assert!(result.status.success(), "{}", format_output(&result));
    let text = String::from_utf8_lossy(&result.stdout);
    for expected in [
        "  Lib.k.isF: closed by steps",
        "  Lib.f.isG: not closed by this backend (steps: ",
        "  k.isF: not closed by this backend (it cites Lib.f.isG, which is not)",
    ] {
        assert!(text.contains(expected), "missing `{expected}`\n{text}");
    }
}

/// A law without a `using` line that the steps leave open is tried again
/// with the earlier laws they closed: it closes, the report names exactly
/// the laws it used, and the script carries them for the kernel. The same
/// law with `using []` stays open. An earlier law with a `when` applies
/// only where its `when` is proved, so a law without that guard stays
/// open and one with it closes. Two laws that undo each other are both
/// left out, so a law that would need them neither loops nor closes.
#[test]
fn a_law_without_using_closes_with_the_earlier_laws_the_steps_closed() {
    let out = scratch("auto-using");
    let (result, summary) = aver_backend_json("auto_using.av", &out, None);
    assert_eq!(
        summary["steps_rejected"],
        serde_json::json!([]),
        "{summary}"
    );
    let closed = &summary["closed_by"];
    for (law, by) in [
        ("count.ofJoined", "steps"),
        ("count.ofThree", "steps"),
        ("count.ofThreeWithNothing", "open"),
        ("size.ofJoinedLong", "steps"),
        ("size.ofThreeAnyLength", "open"),
        ("size.ofThreeLong", "steps"),
    ] {
        assert_eq!(closed[law], by, "{law}: {}", format_output(&result));
    }
    assert_eq!(
        summary["automatic_using"],
        serde_json::json!({
            "count.ofThree": ["count.ofJoined"],
            "size.ofThreeLong": ["size.ofJoinedLong"],
        }),
        "{summary}"
    );
    assert_eq!(summary["closed_with_automatic_using"], 2, "{summary}");
    assert_eq!(summary["closed_with_pinned_using"], 0, "{summary}");
    let script = fs::read_to_string(out.join("proof_steps").join("count.ofThree.steps")).unwrap();
    assert!(script.contains("count.ofJoined"), "{script}");
    let text = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "auto_using.av",
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&text.stdout);
    assert!(
        text.contains(
            "  count.ofThree: closed by steps\n    hint: steps closed it with earlier laws they chose; add `using [count.ofJoined]` to go straight to it\n"
        ),
        "{text}"
    );

    let (result, summary) = aver_backend_json("auto_using_inverse.av", &out, None);
    assert!(result.status.success(), "{}", format_output(&result));
    assert_eq!(summary["closed_by"]["count.ofJoined"], "steps", "{summary}");
    assert_eq!(summary["closed_by"]["count.ofParts"], "steps", "{summary}");
    assert_eq!(summary["closed_by"]["count.ofThree"], "open", "{summary}");
    assert!(summary.get("automatic_using").is_none(), "{summary}");
    let _ = fs::remove_dir_all(out);
}

/// The laws of `fixture` (in `dir`) the Aver backend closes by steps and
/// the ones it leaves open, with the step scripts it wrote, by law.
fn closed_and_open(
    dir: &Path,
    fixture: &str,
    name: &str,
) -> (Vec<String>, Vec<String>, Vec<(String, String)>) {
    let out = scratch(name);
    let result = aver_in(
        dir,
        &[
            "proof",
            fixture,
            "--backend",
            "aver",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    let text = String::from_utf8_lossy(&result.stdout).to_string();
    let law = |line: &str, how: &str| {
        line.strip_prefix("  ")
            .and_then(|l| l.split_once(how))
            .map(|(law, _)| law.to_string())
    };
    let closed: Vec<String> = text
        .lines()
        .filter_map(|l| law(l, ": closed by steps"))
        .collect();
    let open: Vec<String> = text
        .lines()
        .filter_map(|l| law(l, ": not closed by this backend"))
        .collect();
    assert!(
        !closed.is_empty() && !open.is_empty(),
        "{}",
        format_output(&result)
    );
    let mut scripts: Vec<(String, String)> = fs::read_dir(out.join("proof_steps"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .map(|p| {
            let text = fs::read_to_string(&p).unwrap();
            (
                p.file_stem().unwrap().to_string_lossy().to_string(),
                aver::ir::proof_steps::shared::unshare(&text).unwrap_or(text),
            )
        })
        .collect();
    scripts.sort();
    let _ = fs::remove_dir_all(out);
    (closed, open, scripts)
}

/// A given, a parameter and a name a pattern binds may each be spelled like
/// a module-level binding, and then hide it. A step script reads the binding
/// under a name none of them can have (`.n`), so unfolding a function that
/// reads the binding does not turn it into the local: the false laws of the
/// fixture stay open, the true ones close.
#[test]
fn a_local_spelled_like_a_module_binding_is_not_read_as_the_binding() {
    let (closed, open, scripts) = closed_and_open(
        &repo_root().join(FIXTURES),
        "binding_names.av",
        "binding-names",
    );
    assert_eq!(
        closed,
        [
            "addN.addsSeven",
            "addPayload.someAddsSeven",
            "countN.addsSevenInFront",
            "pick.isSeven",
            "seven.isSeven"
        ]
    );
    // `pick.isItsArgument` is the same claim left to a plan
    // (`by Kernel.Std.cases`), which gets the definitions with their
    // binding reads spelled the same way.
    assert_eq!(
        open,
        [
            "addN.isDouble",
            "addPayload.someIsTwice",
            "countN.isTotal",
            "pick.isItsArgument",
            "seven.isItsArgument"
        ]
    );
    let script = &scripts
        .iter()
        .find(|(law, _)| law == "seven.isSeven")
        .expect("a script for seven.isSeven")
        .1;
    for expected in [
        "(obligation seven.isSeven ((n int)) (none) (call seven (v n)) (i 7))",
        "(def readsN () int () (v .n))",
        "(const .n int (i 7))",
    ] {
        assert!(script.contains(expected), "missing `{expected}`\n{script}");
    }
    // Every module-level binding a script declares has a dot in its name.
    for (law, script) in &scripts {
        for decl in script.split("(const ").skip(1) {
            let name = decl.split([' ', ')']).next().unwrap();
            assert!(name.contains('.'), "{law}: binding `{name}`\n{script}");
        }
    }
}

/// A value of a refined type satisfies what its smart constructor checks:
/// the type is listed under `exposes opaque`, so only its own module can
/// write the record. A step term may still write it with any value. A
/// script therefore states what the type holds of each given of a refined
/// type as a premise: the law's own proof may use it, and a law that cites
/// it has to prove it of the value it puts for the given. The laws of the
/// fixture that cite at a record the constructor would refuse stay open,
/// and no script is written for them; the true ones close.
#[test]
fn a_law_about_a_refined_given_is_cited_only_at_a_value_its_type_admits() {
    let (mut closed, open, scripts) = closed_and_open(
        &repo_root().join("tests/fixtures"),
        "refined_givens.av",
        "refined-givens",
    );
    // The obligations of the `because` chain are listed under their law.
    closed.retain(|law| !law.starts_with(' '));
    assert_eq!(
        closed,
        [
            "atMost.underCap",
            "atMost.underCapBecause",
            "holds.ofNonNegative",
            "holds.underCapOfNonNegative",
            "isNonNegative.always",
            "same.staysNonNegative"
        ]
    );
    assert_eq!(
        open,
        [
            "holds.ofAnyInt",
            "holds.ofMinusOne",
            "holds.underCapOfAnyInt"
        ]
    );
    for law in &open {
        assert!(
            !scripts.iter().any(|(key, _)| key == law),
            "a script was written for {law}"
        );
    }
    let script = |law: &str| -> &str {
        &scripts
            .iter()
            .find(|(key, _)| key == law)
            .unwrap_or_else(|| panic!("a script for {law}"))
            .1
    };
    // The law's own script assumes what `Natural` holds of `n`, and proves
    // the claim from it.
    let holds = "(op >= (get (v n) value) (i 0))";
    let always = script("isNonNegative.always");
    for expected in [
        format!(
            "(obligation isNonNegative.always ((n (named Natural))) {holds} (call isNonNegative (v n)) (b true))"
        ),
        format!("(have refined1 {holds} (hyp when) "),
    ] {
        assert!(always.contains(&expected), "missing `{expected}`\n{always}");
    }
    // With a `when` of its own, the line about the given comes first.
    let under_cap = script("atMost.underCap");
    let expected = format!(
        "(obligation atMost.underCap ((n (named Natural)) (cap int)) (bi Bool.and {holds} (op <= (get (v n) value) (v cap))) "
    );
    assert!(
        under_cap.contains(&expected),
        "missing `{expected}`\n{under_cap}"
    );
    // A citing script states the cited law with that premise.
    let cited = format!(
        "(law isNonNegative.always ((n (named Natural))) {holds} (call isNonNegative (v n)) (b true))"
    );
    let citing = script("holds.ofNonNegative");
    assert!(citing.contains(&cited), "missing `{cited}`\n{citing}");
    // The kernel holds a citation to it: the law at `Natural(value = -1)`,
    // with no proof of the premise, and with a computation that it holds.
    let minus_one = "(rec Natural (value (neg (i 1))))";
    let at_minus_one = |law: &str, premise: &str| {
        format!(
            "(steps 16\n (obligation holds.ofMinusOne ((x int)) (none) (call isNonNegative {minus_one}) (b true))\n (defs)\n (consts)\n (sums)\n (recs\n  (rec Natural eq (value int)))\n (sigs\n  (isNonNegative ((named Natural)) bool))\n (laws\n  {law})\n (proof (law isNonNegative.always ((n {minus_one})){premise})))\n"
        )
    };
    let computed = format!(" (compute (op >= (get {minus_one} value) (i 0)) (b true))");
    for premise in ["", computed.as_str()] {
        let verdict = aver::proof_kernel::verdict(&at_minus_one(&cited, premise));
        assert!(verdict.is_err(), "premise `{premise}`: {verdict:?}");
    }
    // Stated without the premise, as it was before, the same citation is
    // accepted: the premise is what keeps the false law open.
    let unstated = "(law isNonNegative.always ((n (named Natural))) (none) (call isNonNegative (v n)) (b true))";
    assert_eq!(
        aver::proof_kernel::verdict(&at_minus_one(unstated, "")),
        Ok("holds.ofMinusOne".to_string())
    );
}

/// A record with a validating constructor that is not listed under
/// `exposes opaque` is an ordinary record: any module may write
/// `Natural(value = -1)`, so the constructor's check holds of no given. A
/// law about such a given is about every record: the false one stays open,
/// no script gets a premise the law did not write, and Lean states the
/// type as a plain structure.
#[test]
fn a_record_any_module_may_construct_is_not_a_refined_type() {
    let fixtures = repo_root().join("tests/fixtures");
    let (closed, open, scripts) =
        closed_and_open(&fixtures, "refined_givens_open.av", "refined-givens-open");
    assert_eq!(closed, ["isNonNegative.whenChecked", "same.isItself"]);
    assert_eq!(open, ["isNonNegative.always"]);
    let obligations: Vec<&str> = scripts
        .iter()
        .map(|(_, script)| script.lines().nth(1).unwrap().trim())
        .collect();
    assert_eq!(
        obligations,
        [
            "(obligation isNonNegative.whenChecked ((n (named Natural))) (op >= (get (v n) value) (i 0)) (call isNonNegative (v n)) (b true))",
            "(obligation same.isItself ((n (named Natural))) (none) (call same (v n)) (v n))"
        ]
    );
    let out = scratch("refined-givens-open-lean");
    let result = aver_in(
        &fixtures,
        &[
            "proof",
            "refined_givens_open.av",
            "-o",
            out.to_str().unwrap(),
        ],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("RefinedGivensOpen.lean")).unwrap();
    assert!(lean.contains("structure Natural where"), "{lean}");
    assert!(!lean.contains("abbrev Natural"), "{lean}");
    // Nothing here reads a type that carries a proof, so every script
    // reaches Lean as a step term.
    assert_eq!(unrendered_step_files(&out), Vec::<String>::new());
    let _ = fs::remove_dir_all(out);
}

/// Lean states a given of a refined type by a type that carries the proof
/// of what the type holds, so the premise a step script writes for it is
/// not a hypothesis of the theorem: the theorem has the law's own `when`
/// and nothing else. A step proof that reads the premise, or the inside of
/// a refined record, is not handed to Lean as a term (which is why the
/// fixture is not among those every script of which reaches Lean). The
/// export needs no Lean.
#[test]
fn lean_states_a_refined_given_by_its_type_and_not_by_a_premise() {
    let out = scratch("refined-givens-lean");
    let result = aver_in(
        &repo_root().join("tests/fixtures"),
        &["proof", "refined_givens.av", "-o", out.to_str().unwrap()],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("RefinedGivens.lean")).unwrap();
    for expected in [
        "abbrev Natural := { n : Int // (n >= 0) }",
        "theorem isNonNegative_law_always : ∀ (n : Natural), isNonNegative n = true := by",
        "theorem atMost_law_underCap : ∀ (n : Natural) (cap : Int), (n.val <= cap) = true -> atMost n cap = true := by",
        "theorem same_law_staysNonNegative : ∀ (n : Natural), isNonNegative (same n) = true := by",
    ] {
        assert!(lean.contains(expected), "missing `{expected}`\n{lean}");
    }
    assert_eq!(
        unrendered_step_files(&out),
        [
            "atMost.underCap",
            "atMost.underCapBecause.because1",
            "atMost.underCapBecause.implication",
            "holds.ofNonNegative",
            "holds.underCapOfNonNegative",
            "isNonNegative.always",
            "same.staysNonNegative"
        ]
    );
    let _ = fs::remove_dir_all(out);
}

/// A law over `given x: Int` is a law about every Int, also where it writes
/// a refined record from `x`. The Lean export used to restate such a law
/// over `x : Natural`, with or without a `when` that gives what `Natural`
/// holds; `holds.ofAnyInt` has none, so its theorem said less than the law
/// (which is false at `x = -1`). The given keeps its type, and the record is
/// written with a proof Lean has to find. The export needs no Lean.
#[test]
fn lean_states_an_int_given_as_an_int_where_the_law_wraps_it_in_a_refined_record() {
    let out = scratch("refined-givens-int-lean");
    let result = aver_in(
        &repo_root().join("tests/fixtures"),
        &["proof", "refined_givens.av", "-o", out.to_str().unwrap()],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("RefinedGivens.lean")).unwrap();
    let theorem = |name: &str| {
        lean.lines()
            .find(|line| line.starts_with(&format!("theorem holds_law_{name} :")))
            .unwrap_or_else(|| panic!("the theorem of holds.{name}\n{lean}"))
    };
    let record = "isNonNegative (⟨x, by ";
    let any = theorem("ofAnyInt");
    assert!(any.contains("∀ (x : Int), isNonNegative (⟨x, by "), "{any}");
    assert!(!any.contains("(x : Natural)"), "{any}");
    let non_negative = theorem("ofNonNegative");
    assert!(
        non_negative.contains(&format!("∀ (x : Int), (x >= 0) = true -> {record}")),
        "{non_negative}"
    );
    for name in ["underCapOfNonNegative", "underCapOfAnyInt"] {
        let line = theorem(name);
        assert!(line.contains("∀ (x : Int), "), "{line}");
        assert!(!line.contains("x.val"), "{line}");
    }
    let _ = fs::remove_dir_all(out);
}

/// A law about values of a refined type says `given a: Natural`, and Lean
/// states it over the type. The theorem and its proof are the text the
/// export gave when the law was written over `given a: Int` with
/// `when a >= 0` and `Natural(value = a)`, and the export restated it.
#[test]
fn lean_states_a_law_over_refined_givens_over_the_type() {
    let out = scratch("natural-lean");
    let result = aver_in(
        &repo_root().join("examples/refinement/natural"),
        &["proof", "natural.av", "-o", out.to_str().unwrap()],
    );
    assert!(result.status.success(), "{}", format_output(&result));
    let lean = fs::read_to_string(out.join("Natural.lean")).unwrap();
    for expected in [
        "-- aver:law-class add_law_commutative universal add.commutative\n\
         theorem add_law_commutative : ∀ (a : Natural) (b : Natural), add a b = add b a := by\n\
         \x20 intro a b\n\
         \x20 unfold add fromInt\n\
         \x20 simp [Int.add_comm, Int.mul_comm]\n",
        "-- aver:law-class mul_law_commutative universal mul.commutative\n\
         theorem mul_law_commutative : ∀ (a : Natural) (b : Natural), mul a b = mul b a := by\n\
         \x20 intro a b\n\
         \x20 unfold mul fromInt\n\
         \x20 simp [Int.add_comm, Int.mul_comm]\n",
    ] {
        assert!(lean.contains(expected), "missing `{expected}`\n{lean}");
    }
    let _ = fs::remove_dir_all(out);
}

/// The same in a module another one imports: its parameter `n` is not its
/// binding `n`, in its own laws and in a law of the importing module that
/// reaches the function.
#[test]
fn a_local_of_an_imported_module_is_not_read_as_its_binding() {
    let (closed, open, _) = closed_and_open(
        &repo_root().join(FIXTURES).join("binding_names_dep"),
        "main.av",
        "binding-names-dep",
    );
    assert_eq!(closed, ["Lib.same.isItsArgument", "viaLib.isItsArgument"]);
    assert_eq!(
        open,
        [
            "Lib.same.givenIsSeven",
            "Lib.same.isSeven",
            "viaLib.isSeven"
        ]
    );
}

const DEFAULT_UPDATE_LAWS: [&str; 10] = [
    "depthA.sameAsDepthB",
    "lenA.sameAsLenB",
    "okOrZero.errIsZero",
    "okOrZero.okIsItsValue",
    "orZero.noneIsZero",
    "orZero.someIsItsValue",
    "pick.positiveDefault",
    "setX.keepsY",
    "setX.readsThroughTwoUpdates",
    "setX.writesX",
];

/// The false laws of the fixture: a `Some` or an `Ok` read as the default,
/// an update read as writing the field it leaves and as leaving the field it
/// writes, and a length taken as its own square.
const DEFAULT_UPDATE_FALSE_LAWS: [&str; 5] = [
    "lenA.lengthIsItsSquare",
    "wrongDefault.okIsTheDefault",
    "wrongDefault.someIsTheDefault",
    "wrongField.keepsX",
    "wrongField.writesY",
];

fn steps_summary(fixture: &str, extra: &[&str]) -> serde_json::Value {
    let mut args = vec!["proof", fixture];
    args.extend_from_slice(extra);
    // The run fails while a law stays open; the summary says which.
    let result = aver_in(&repo_root().join(FIXTURES), &args);
    serde_json::from_str(
        String::from_utf8_lossy(&result.stdout)
            .lines()
            .rev()
            .find(|l| l.starts_with('{'))
            .unwrap_or_else(|| panic!("{}", format_output(&result))),
    )
    .unwrap()
}

/// `Option.withDefault` and `Result.withDefault` on a constructor, a field
/// of a record update and an induction along a function whose pattern has a
/// wildcard part each close laws by steps; the false laws of those shapes
/// stay open, and both kernels refuse a rule taken for another, an update
/// read as writing the field it leaves, and a case that names a wildcard
/// part `_` or leaves it unnamed.
#[test]
fn defaults_update_fields_and_wildcard_parts_close_by_steps_and_refuse_mutations() {
    let out = scratch("defaults-updates");
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("defaults_and_updates.av", &out)
            .into_iter()
            .collect();
    assert_eq!(
        files.keys().cloned().collect::<Vec<_>>(),
        DEFAULT_UPDATE_LAWS
    );
    let read = |law: &str| fs::read_to_string(&files[law]).unwrap();
    let mut all = String::new();
    for law in files.keys() {
        assert_eq!(aver::proof_kernel::verdict(&read(law)), Ok(law.clone()));
        all.push_str(&read(law));
    }
    for used in [
        "(rule option.with_default.some ",
        "(rule option.with_default.none ",
        "(rule result.with_default.ok ",
        "(rule result.with_default.err ",
        "(proj (get (upd Point ",
        "(arm (pcons _ t) ",
    ] {
        assert!(all.contains(used), "{used} is not used");
    }
    let swap = |law: &str, from: &str, to: &str| {
        let text = read(law);
        assert!(text.contains(from), "{law}: {text}");
        text.replacen(from, to, 1)
    };
    for (kind, text) in [
        (
            "the default taken for the value a Some holds",
            swap(
                "orZero.someIsItsValue",
                "(rule option.with_default.some",
                "(rule option.with_default.none",
            ),
        ),
        (
            "the value taken for the default of an Err",
            swap(
                "okOrZero.errIsZero",
                "(rule result.with_default.err",
                "(rule result.with_default.ok",
            ),
        ),
        (
            "an update read as writing the field it leaves",
            read("setX.keepsY").replace("(get (v p) y)", "(v v)"),
        ),
        (
            "a wildcard part left unnamed",
            swap("lenA.sameAsLenB", "(case (x t) ", "(case (t) "),
        ),
        (
            "a wildcard part named _",
            swap("lenA.sameAsLenB", "(case (x t) ", "(case (_ t) "),
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
    let summary = steps_summary(
        "defaults_and_updates.av",
        &["--backend", "aver", "--check-json"],
    );
    for law in DEFAULT_UPDATE_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    for law in DEFAULT_UPDATE_FALSE_LAWS {
        assert_eq!(summary["closed_by"][law], "open", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

#[test]
fn lean_checks_defaults_update_fields_and_wildcard_parts_by_steps() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("defaults-updates-lean");
    let summary = steps_summary(
        "defaults_and_updates.av",
        &[
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "5",
        ],
    );
    for law in DEFAULT_UPDATE_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let _ = fs::remove_dir_all(out);
}

const LENGTH_LAWS: [&str; 2] = ["skips.always", "walk.staysNonnegative"];

/// Lean reads an induction on a list's length as strong induction on a
/// bound of it (`AverSteps.list_measure_induct`), the law cited in the
/// steps a `have` over the shorter lists, and refuses the step term once
/// the law is applied to the bound alone or at another count.
#[test]
fn lean_inducts_on_a_lists_length_and_refuses_mutations() {
    if !lean_required::lake_available() {
        eprintln!("skipping the Lean half: `lake` is not available");
        return;
    }
    let out = scratch("length-lean");
    let result = aver_in(
        &repo_root().join(FIXTURES),
        &[
            "proof",
            "length_induction.av",
            "-o",
            out.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "99",
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
    for law in LENGTH_LAWS {
        assert_eq!(summary["closed_by"][law], "steps", "{law}: {summary}");
    }
    let lean = fs::read_to_string(out.join("LengthInduction.lean")).unwrap();
    for (law, theorem, from, to) in [
        (
            "skips.always",
            "skips_law_always",
            "steps_below_xs xs steps_p",
            "steps_below_xs xs (AverSteps.and_elim_l _ _ steps_p)",
        ),
        (
            "walk.staysNonnegative",
            "walk_law_staysNonnegative",
            "(AverSteps.and_elim_l _ _ steps_p) acc",
            "(AverSteps.and_elim_l _ _ steps_p) (acc + 1)",
        ),
    ] {
        let line = exact_line(&lean, theorem).to_string();
        assert!(
            line.contains("AverSteps.list_measure_induct (motive := fun xs =>")
                && line.contains(from),
            "{law}: {line}"
        );
        assert!(
            lean_refuses(
                &out,
                "LengthInduction.lean",
                &lean.replacen(&line, &line.replacen(from, to, 1), 1),
                law
            ),
            "{law}: Lean must refuse the step term with {to}"
        );
    }
    let _ = fs::remove_dir_all(out);
}

/// A function that binds its match on a list to a name and calls itself on
/// what it drops from the list's tail is opened by the gate because every
/// call shortens the list, and a law about it is proved by induction on the
/// list's length: the law at every shorter list, cited where a call stands
/// with a linear proof that its list is shorter. The kernel refuses the
/// script once the induction carries less, quantifies less, renames its law
/// or takes a name in scope, and the law cited at the list itself.
#[test]
fn both_kernels_induct_on_a_lists_length_and_refuse_mutations() {
    let out = scratch("length");
    let (result, summary) = aver_backend_json("length_induction.av", &out, None);
    assert_eq!(summary["steps_rejected"], serde_json::json!([]));
    for law in LENGTH_LAWS {
        assert_eq!(
            summary["closed_by"][law],
            "steps",
            "{law}: {}",
            format_output(&result)
        );
    }
    assert_eq!(
        summary["closed_by"]["walk.staysNonnegativeBelow"], "open",
        "{summary}"
    );
    let _ = fs::remove_dir_all(&out);
    let files: std::collections::BTreeMap<String, PathBuf> =
        export_steps("length_induction.av", &out)
            .into_iter()
            .collect();
    assert_eq!(files.keys().cloned().collect::<Vec<_>>(), LENGTH_LAWS);
    let mut mutant_paths: Vec<(String, PathBuf)> = Vec::new();
    for (law, path) in &files {
        let text = fs::read_to_string(path).unwrap();
        assert_eq!(aver::proof_kernel::verdict(&text), Ok(law.clone()));
        let open = text.find("(leninduct xs ").expect("a length induction");
        // (leninduct xs LHS RHS [(carry …)] (GENERAL…) (BOUND HYP LAW) BODY)
        let forms = sub_forms(&text, open);
        let (names_from, names_to) = forms[forms.len() - 2];
        let names: Vec<&str> = text[names_from + 1..names_to - 1].split(' ').collect();
        let [bound, hyp, ih] = names[..] else {
            panic!("{text}");
        };
        let cited = format!("(law {ih} ((xs ");
        assert!(text.contains(&cited), "{law}: the law is cited\n{text}");
        let renamed =
            |names: &str| format!("{}({names}){}", &text[..names_from], &text[names_to..]);
        // (xs TERM), the list the law is cited at.
        let binding = text.find(&cited).unwrap() + cited.len() - "(xs ".len();
        let at_itself = format!(
            "{}(xs (v xs)){}",
            &text[..binding],
            &text[closing(&text, binding) + 1..]
        );
        let mut mutants = vec![
            ("the law renamed", renamed(&format!("{bound} {hyp} {ih}x"))),
            (
                "the bound named as the list",
                renamed(&format!("xs {hyp} {ih}")),
            ),
            ("the law cited at the list itself", at_itself),
        ];
        if *law == "walk.staysNonnegative" {
            let (general_from, general_to) = forms[forms.len() - 3];
            mutants.push((
                "the `when` not carried",
                text.replacen(" (carry when)", "", 1),
            ));
            mutants.push((
                "the count not generalised",
                format!("{}(){}", &text[..general_from], &text[general_to..]),
            ));
        }
        for (kind, mutant) in mutants {
            let refused = aver::proof_kernel::verdict(&mutant);
            assert!(
                refused
                    .as_ref()
                    .is_err_and(|why| why.starts_with("step proof")),
                "{law}, {kind}: {refused:?}\n{mutant}"
            );
            let path = out.join(format!("mutant{}.steps", mutant_paths.len()));
            fs::write(&path, &mutant).unwrap();
            mutant_paths.push((format!("{law}, {kind}"), path));
        }
    }
    // The replayer on the VM refuses every mutant too, all in one run.
    let paths: Vec<PathBuf> = mutant_paths.iter().map(|(_, p)| p.clone()).collect();
    let result = replay(&paths);
    let text = String::from_utf8_lossy(&result.stdout);
    assert!(!result.status.success(), "{}", format_output(&result));
    for (kind, path) in &mutant_paths {
        assert!(
            text.contains(&format!("refused {}: ", path.display())),
            "{kind}: {}",
            format_output(&result)
        );
    }
    let _ = fs::remove_dir_all(out);
}
