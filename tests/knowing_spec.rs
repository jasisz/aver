//! `lazy f(…) knowing f.law(…)`: `aver check` and `aver compile` let a
//! deferred call know a value only from a law of the shape `P(f(…)) => R`
//! that the proof kernel closes, or, with `--trust-laws`, one whose samples
//! pass; they name what is wrong otherwise.

use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// A one-module program: a shift of terms that carry a word, deferring each
/// part below the one it makes. `LAW` is the law, `CALL` the deferred call
/// in `main`.
const PROGRAM: &str = r#"module Shift
    intent = "Terms that carry a word, and a shift that knows the word of what it defers."
    effects [Console.print]

type Term
    Var(Int, Int)
    App(Term, Term, Int)

fn wordOf(t: Term) -> Int
    ? "The word a term carries."
    match t
        Term.Var(_, w) -> w
        Term.App(_, _, w) -> w

verify wordOf
    wordOf(Term.Var(0, 5)) => 5

fn wordUnder(t: Term, by: Int) -> Int
    ? "The word of `t` shifted by `by`."
    wordOf(t) * 31 + by

verify wordUnder
    wordUnder(Term.Var(0, 5), 2) => 157

fn shift(t: Term, by: Int) -> Term
    ? "Every variable moved by `by`, the parts below deferred."
    match t
        Term.Var(n, _) -> Term.Var(n + by, wordUnder(t, by))
        Term.App(f, a, _) -> Term.App(lazy shift(f, by), lazy shift(a, by), wordUnder(t, by))

LAW

fn main() -> Unit
    ? "Shifts a term and prints its word."
    ! [Console.print]
    t = Term.App(Term.Var(0, 1), Term.Var(1, 2), 3)
    later = CALL
    Console.print("{wordOf(later)}")
"#;

const PROVED: &str = "verify shift law wordKnown\n    given t: Term = [Term.Var(0, 5), Term.App(Term.Var(1, 2), Term.Var(2, 3), 9)]\n    given by: Int = [0, 1, 7]\n    wordOf(shift(t, by)) => wordUnder(t, by)";

/// True on its samples (`by` is 0 there) and false for every other `by`.
const UNPROVED: &str = "verify shift law wordGuess\n    given t: Term = [Term.Var(0, 5), Term.App(Term.Var(1, 2), Term.Var(2, 3), 9)]\n    given by: Int = [0]\n    wordOf(shift(t, by)) => wordOf(t) * 31";

struct Project {
    dir: PathBuf,
}

impl Project {
    fn new(tag: &str, law: &str, call: &str) -> Self {
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let dir = std::env::temp_dir().join(format!("aver_knowing_{tag}_{nanos}"));
        fs::create_dir_all(&dir).expect("create fixture dir");
        let source = PROGRAM.replace("LAW", law).replace("CALL", call);
        fs::write(dir.join("main.av"), source).expect("write main.av");
        Project { dir }
    }

    fn run(&self, args: &[&str]) -> (bool, String) {
        let out = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(args)
            .current_dir(&self.dir)
            .output()
            .expect("spawn aver");
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        (out.status.success(), text)
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

#[test]
fn a_proved_law_lets_the_call_know_its_value() {
    let p = Project::new(
        "proved",
        PROVED,
        "lazy shift(t, 2) knowing shift.wordKnown(t, 2)",
    );
    let (ok, out) = p.run(&["check", "main.av"]);
    assert!(ok, "{out}");
    let (ok, out) = p.run(&["compile", "main.av", "-o", "out"]);
    assert!(ok, "{out}");
    assert!(!out.contains("not proved"), "{out}");
}

#[test]
fn a_law_the_kernel_does_not_close_is_refused_unless_trusted() {
    let p = Project::new(
        "unproved",
        UNPROVED,
        "lazy shift(t, 2) knowing shift.wordGuess(t, 2)",
    );
    for command in [
        &["check", "main.av"][..],
        &["compile", "main.av", "-o", "out"],
    ] {
        let (ok, out) = p.run(command);
        assert!(!ok, "{command:?}: {out}");
        assert!(
            out.contains("law `shift.wordGuess`, which a `knowing` stands on, is not closed")
                && out.contains("--trust-laws"),
            "{command:?}: {out}"
        );
    }
    let (ok, out) = p.run(&["check", "main.av", "--trust-laws"]);
    assert!(ok, "{out}");
    let (ok, out) = p.run(&["compile", "main.av", "-o", "out", "--trust-laws"]);
    assert!(ok, "{out}");
    assert!(
        out.contains(
            "built with laws that are not proved, checked while the program runs: shift.wordGuess"
        ),
        "{out}"
    );
    let (ok, out) = p.run(&[
        "compile",
        "main.av",
        "-o",
        "out",
        "--trust-laws",
        "--target",
        "wasm-gc",
        "--certify",
    ]);
    assert!(!ok && out.contains("--trust-laws"), "{out}");
}

#[test]
fn a_trusted_law_whose_samples_fail_is_refused() {
    let failing = UNPROVED.replace("given by: Int = [0]", "given by: Int = [0, 1]");
    let p = Project::new(
        "failing",
        &failing,
        "lazy shift(t, 2) knowing shift.wordGuess(t, 2)",
    );
    let (ok, out) = p.run(&["check", "main.av", "--trust-laws"]);
    assert!(!ok, "{out}");
    assert!(
        out.contains("law `shift.wordGuess` is not proved, and 2 of its samples fail"),
        "{out}"
    );
}

#[test]
fn a_law_of_another_shape_or_another_call_is_refused() {
    let cases = [
        (
            PROVED,
            "lazy shift(t, 2) knowing shift.nope(t, 2)",
            "`shift` has no law `nope`",
        ),
        (
            "verify shift law sameSize\n    given t: Term = [Term.Var(0, 5)]\n    given by: Int = [0]\n    shift(shift(t, by), 0) => shift(t, by)",
            "lazy shift(t, 2) knowing shift.sameSize(t, 2)",
            "does not say what a function of one argument gives on the result of `shift`",
        ),
        (
            "verify shift law wordOther\n    given t: Term = [Term.Var(0, 5)]\n    given by: Int = [0]\n    given k: Int = [3]\n    wordOf(shift(t, by)) => wordUnder(t, by) + k - k",
            "lazy shift(t, 2) knowing shift.wordOther(t, 2, 3)",
            "every given is one of the arguments as it is",
        ),
        (
            PROVED,
            "lazy shift(t, 2) knowing shift.wordKnown(t)",
            "law `shift.wordKnown` has 2 given(s) (t, by), and `knowing` gives 1",
        ),
        (
            PROVED,
            "lazy shift(t, 2) knowing shift.wordKnown(t, 3)",
            "argument 2 of `shift` is not what law `shift.wordKnown` has there",
        ),
    ];
    for (i, (law, call, says)) in cases.into_iter().enumerate() {
        let p = Project::new(&format!("shape{i}"), law, call);
        let (ok, out) = p.run(&["check", "main.av"]);
        assert!(!ok && out.contains(says), "{call}: {out}");
    }
}
