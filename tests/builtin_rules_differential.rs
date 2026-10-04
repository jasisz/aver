//! The list rules of the proof steps against every place the list builtins
//! are defined: each rule (and each builtin fact) is instantiated at sample
//! values (the empty list, duplicates, counts below zero, zero, and far past
//! the end) and both sides are evaluated by the compiler's own evaluator,
//! the kernel written in Aver, the VM, wasm-gc and the Lean model. A count
//! below zero lives in four encodings (`term::eval_closed`, `eval.av`,
//! `Int.toNat` in Lean, the VM's `List.take`); this test is what keeps them
//! saying the same thing.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver::ir::proof_steps::sexpr::{BuiltinsOnly, script};
use aver::ir::proof_steps::term::{self, Term};
use aver::ir::proof_steps::{Eqn, Obligation, Proof, Script, WallRule, facts};
use aver_cmd::{aver_bin, format_output};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const BIG: i64 = 1 << 40;

fn int(v: i64) -> Term {
    term::int(&v.into())
}

fn list(vs: &[i64]) -> Term {
    term::list(vs.iter().map(|v| int(*v)).collect())
}

/// Sample values for a binder, by the role its name has in the rules.
fn samples(binder: &str) -> Vec<Term> {
    match binder {
        "x" => vec![int(0), int(7)],
        "n" => [-BIG, -5, -1, 0, 1, 2, 3, 9, BIG]
            .iter()
            .map(|v| int(*v))
            .collect(),
        _ => vec![list(&[]), list(&[1]), list(&[2, 2]), list(&[3, 1, 2])],
    }
}

/// Every substitution of samples for `binders`.
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

/// One equation schema to test: a name, its binders, its premises (each
/// `p = true`) and its conclusion.
struct Schema {
    name: String,
    binders: Vec<String>,
    premises: Vec<Eqn>,
    concl: Eqn,
}

fn schemas() -> Vec<Schema> {
    let mut out: Vec<Schema> = WallRule::ALL
        .iter()
        .filter(|r| r.id().starts_with("list."))
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
    for f in facts::all() {
        let ob = &f.script.obligation;
        out.push(Schema {
            name: f.key.to_string(),
            binders: ob.givens.clone(),
            premises: Vec::new(),
            concl: Eqn::new(ob.lhs.clone(), ob.rhs.clone()),
        });
    }
    out
}

/// A sampled instance: both sides and the value the compiler's evaluator
/// gives them.
struct Instance {
    lhs: Term,
    rhs: Term,
    value: Term,
}

fn instances(s: &Schema) -> Vec<Instance> {
    let mut out = Vec::new();
    for sub in substitutions(&s.binders) {
        let at = |t: &Term| term::subst(t, &sub).unwrap();
        let holds = s
            .premises
            .iter()
            .all(|p| term::eval_closed(&at(&p.lhs)).as_ref() == Some(&p.rhs));
        if !holds {
            continue;
        }
        let (lhs, rhs) = (at(&s.concl.lhs), at(&s.concl.rhs));
        let l = term::eval_closed(&lhs).unwrap_or_else(|| panic!("{}: lhs is not closed", s.name));
        let r = term::eval_closed(&rhs).unwrap_or_else(|| panic!("{}: rhs is not closed", s.name));
        assert_eq!(
            l,
            r,
            "{}: the compiler's evaluator disagrees with the rule at {}",
            s.name,
            aver(&lhs)
        );
        out.push(Instance { lhs, rhs, value: l });
    }
    assert!(
        !out.is_empty(),
        "{}: no sample satisfies the premises",
        s.name
    );
    out
}

fn aver(t: &Term) -> String {
    aver::ir::proof_steps::show::term(t, &BuiltinsOnly)
}

fn is_list(t: &Term) -> bool {
    matches!(t.node, aver::ir::hir::ResolvedExpr::List(_))
}

/// `t = value` checked by the kernel written in Aver, by evaluation.
fn kernel_computes(t: &Term, value: &Term) -> Result<String, String> {
    let s = Script {
        obligation: Obligation {
            key: "k".into(),
            givens: Vec::new(),
            finite: Vec::new(),
            lists: Vec::new(),
            premise: None,
            lhs: t.clone(),
            rhs: value.clone(),
        },
        defs: Vec::new(),
        consts: Vec::new(),
        laws: Vec::new(),
        proof: Proof::Compute {
            lhs: t.clone(),
            rhs: value.clone(),
        },
    };
    aver::proof_kernel::verdict(&script(&s, &BuiltinsOnly).unwrap())
}

#[test]
fn the_kernel_evaluates_every_rule_instance_as_the_compiler_does() {
    for s in schemas() {
        for i in instances(&s) {
            for side in [&i.lhs, &i.rhs] {
                assert_eq!(
                    kernel_computes(side, &i.value),
                    Ok("k".to_string()),
                    "{}: {} = {}",
                    s.name,
                    aver(side),
                    aver(&i.value)
                );
            }
        }
    }
}

/// A module whose laws state every sampled instance, both sides, as one
/// list per rule; `k` is a given only so that each law has one sample.
fn program() -> String {
    let mut src = String::from(
        "module RuleSamples\n    intent = \"Every sampled instance of the list rules and facts.\"\n    exposes [lists, ints]\n    effects []\n\nfn lists(xs: List<List<Int>>) -> List<List<Int>>\n    ? \"Anchor for list-valued instances.\"\n    xs\n\nfn ints(xs: List<Int>) -> List<Int>\n    ? \"Anchor for Int-valued instances.\"\n    xs\n",
    );
    for (n, s) in schemas().iter().enumerate() {
        let is = instances(s);
        let anchor = if is_list(&is[0].value) {
            "lists"
        } else {
            "ints"
        };
        let mut sides = Vec::new();
        let mut values = Vec::new();
        for i in &is {
            sides.push(aver(&i.lhs));
            sides.push(aver(&i.rhs));
            values.push(aver(&i.value));
            values.push(aver(&i.value));
        }
        src.push_str(&format!(
            "\n// {}\nverify {anchor} law rule{n}\n    given k: Int = [0]\n    {anchor}([{}]) => [{}]\n",
            s.name,
            sides.join(", "),
            values.join(", ")
        ));
    }
    src
}

fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-rule-samples-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("rule_samples.av"), program()).unwrap();
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
fn the_vm_runs_every_rule_instance_to_the_same_value() {
    let dir = scratch("vm");
    let out = aver_in(&dir, &["verify", "rule_samples.av"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains(" 0 failed"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_runs_every_rule_instance_to_the_same_value() {
    let dir = scratch("wasm");
    let out = aver_in(&dir, &["verify", "rule_samples.av", "--wasm-gc"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        text.contains(" 0 failed") && !text.contains("not checked"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn the_lean_model_evaluates_every_rule_instance_to_the_same_value() {
    if !lean_required::lake_available() {
        eprintln!("skipping: `lake` is not available");
        return;
    }
    let dir = scratch("lean");
    let out = aver_in(
        &dir,
        &["proof", "rule_samples.av", "-o", "lean", "--check-json"],
    );
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && text.contains("\"build_errors\":0"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}
