//! The Vector rules and Vector facts of the proof steps against every place
//! that runs a vector: each is instantiated at sample vectors (empty, one
//! element, three elements) and at indices below zero, at zero, at the last
//! element, just past the end and far past it, and both sides are run on
//! the VM, on wasm-gc and in the Lean model (`aver proof` samples, decided by
//! Lean). The kernel written in Aver cannot evaluate vectors, so it does not
//! take part: it checks each rule as an equation schema, which the
//! proof-step tests cover.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver::ir::hir::{ResolvedCallee, ResolvedExpr};
use aver::ir::proof_steps::facts;
use aver::ir::proof_steps::sexpr::BuiltinsOnly;
use aver::ir::proof_steps::term::{self, Term};
use aver::ir::proof_steps::{Eqn, WallRule};
use aver_cmd::{aver_bin, format_output};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn int(v: i64) -> Term {
    term::int(&v.into())
}

fn list(vs: &[i64]) -> Term {
    term::list(vs.iter().map(|v| int(*v)).collect())
}

/// A sample vector, written as the list it is made from, with its length.
fn vectors() -> Vec<(Term, i64)> {
    [vec![], vec![5], vec![5, 6, 7]]
        .into_iter()
        .map(|xs: Vec<i64>| {
            let n = xs.len() as i64;
            (term::builtin("Vector.fromList", vec![list(&xs)], None), n)
        })
        .collect()
}

/// Sample values for a binder, by the role its name has in the rules: a
/// vector, an index (below zero, zero, the last of three, just past the
/// end, far past it), an element, a list, or a literal size.
fn samples(binder: &str) -> Vec<Term> {
    match binder {
        "v" => vectors().into_iter().map(|(v, _)| v).collect(),
        "i" | "j" => [-1, 0, 2, 3, 9].iter().map(|i| int(*i)).collect(),
        "x" => vec![int(4)],
        "l" => vec![list(&[]), list(&[1, 2])],
        "n" => vec![int(0), int(3)],
        other => panic!("no samples for {other}"),
    }
}

struct Schema {
    name: String,
    binders: Vec<String>,
    premises: Vec<Eqn>,
    concl: Eqn,
}

fn schemas() -> Vec<Schema> {
    let mut out: Vec<Schema> = WallRule::ALL
        .iter()
        .filter(|r| r.id().starts_with("vector."))
        .map(|r| {
            let (premises, concl) = r.schema();
            Schema {
                name: r.id().to_string(),
                binders: r.binders().iter().map(|b| b.to_string()).collect(),
                premises,
                concl,
            }
        })
        .collect();
    for f in facts::all()
        .into_iter()
        .filter(|f| f.key.starts_with("Vector."))
    {
        let ob = &f.script.obligation;
        out.push(Schema {
            name: f.key.to_string(),
            binders: ob.givens.clone(),
            premises: ob
                .premise
                .iter()
                .map(|p| Eqn::new(p.clone(), term::boolean(true)))
                .collect(),
            concl: Eqn::new(ob.lhs.clone(), ob.rhs.clone()),
        });
    }
    out
}

fn substitutions(binders: &[String]) -> Vec<Vec<(String, Term)>> {
    binders.iter().fold(vec![Vec::new()], |acc, b| {
        acc.iter()
            .flat_map(|prefix| {
                samples(b).into_iter().map(move |v| {
                    let mut next = prefix.clone();
                    next.push((b.clone(), v));
                    next
                })
            })
            .collect()
    })
}

/// `t` with each `Vector.len` of a sample vector replaced by its length,
/// so the compiler's evaluator can decide a premise.
fn lengths_known(t: &Term) -> Term {
    if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node
        && b == "Vector.len"
        && let Some((_, n)) = vectors().into_iter().find(|(v, _)| *v == args[0])
    {
        return int(n);
    }
    term::map_children(t, &mut |c| Ok(lengths_known(c))).expect("total")
}

fn holds(premise: &Eqn, sub: &[(String, Term)]) -> bool {
    let at = lengths_known(&term::subst(&premise.lhs, sub).unwrap());
    term::eval_closed(&at).as_ref() == Some(&premise.rhs)
}

fn aver(t: &Term) -> String {
    aver::ir::proof_steps::show::term(t, &BuiltinsOnly)
}

/// The Lean-literal size a `Vector.new` needs: `__vector_new` is how a
/// literal-size `Vector.new` reads in the step data.
fn source(t: &Term) -> String {
    aver(t).replace("__vector_new(", "Vector.new(")
}

/// The anchor a side's value type goes through.
fn anchor(t: &Term) -> &'static str {
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Builtin(b), _) => match b.as_str() {
            "Vector.get" => "reads",
            "Vector.set" => "writes",
            "Vector.len" => "ints",
            "List.fromVector" => "lists",
            "Vector.fromList" => "vectors",
            other => panic!("no anchor for {other}"),
        },
        _ => panic!("no anchor for a non-call"),
    }
}

/// A module whose laws state every sampled instance, both sides, one list
/// per rule or fact.
fn program() -> String {
    let mut src = String::from(
        "module VectorSamples\n    intent = \"Every sampled instance of the Vector rules and facts.\"\n    exposes [reads, writes, ints, lists, vectors]\n    effects []\n\nfn reads(xs: List<Option<Int>>) -> List<Option<Int>>\n    ? \"Anchor for reads.\"\n    xs\n\nfn writes(xs: List<Option<Vector<Int>>>) -> List<Option<Vector<Int>>>\n    ? \"Anchor for writes.\"\n    xs\n\nfn ints(xs: List<Int>) -> List<Int>\n    ? \"Anchor for lengths.\"\n    xs\n\nfn lists(xs: List<List<Int>>) -> List<List<Int>>\n    ? \"Anchor for lists.\"\n    xs\n\nfn vectors(xs: List<Vector<Int>>) -> List<Vector<Int>>\n    ? \"Anchor for vectors.\"\n    xs\n",
    );
    for (n, s) in schemas().iter().enumerate() {
        let mut lhs = Vec::new();
        let mut rhs = Vec::new();
        for sub in substitutions(&s.binders) {
            if !s.premises.iter().all(|p| holds(p, &sub)) {
                continue;
            }
            lhs.push(source(&term::subst(&s.concl.lhs, &sub).unwrap()));
            rhs.push(source(&term::subst(&s.concl.rhs, &sub).unwrap()));
        }
        assert!(!lhs.is_empty(), "{}: no sample", s.name);
        let a = anchor(&s.concl.lhs);
        src.push_str(&format!(
            "\n// {}\nverify {a} law sample{n}\n    given k: Int = [0]\n    {a}([{}]) => [{}]\n",
            s.name,
            lhs.join(", "),
            rhs.join(", ")
        ));
    }
    src
}

fn scratch(name: &str) -> PathBuf {
    let dir =
        std::env::temp_dir().join(format!("aver-vector-samples-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("vector_samples.av"), program()).unwrap();
    dir
}

fn aver_in(dir: &Path, args: &[&str]) -> Output {
    Command::new(aver_bin())
        .args(args)
        .current_dir(dir)
        .output()
        .expect("aver runs")
}

#[test]
fn the_vm_runs_every_vector_rule_instance_to_the_same_value() {
    let dir = scratch("vm");
    let out = aver_in(&dir, &["verify", "vector_samples.av"]);
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && text.contains(" 0 failed") && !text.contains("not checked"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_runs_every_vector_rule_instance_to_the_same_value() {
    let dir = scratch("wasm");
    let out = aver_in(&dir, &["verify", "vector_samples.av", "--wasm-gc"]);
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && text.contains(" 0 failed") && !text.contains("not checked"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn the_lean_model_evaluates_every_vector_rule_instance_to_the_same_value() {
    if !lean_required::lake_available() {
        eprintln!("skipping: `lake` is not available");
        return;
    }
    let dir = scratch("lean");
    let out = aver_in(
        &dir,
        &["proof", "vector_samples.av", "-o", "lean", "--check-json"],
    );
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && text.contains("\"build_errors\":0"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}
