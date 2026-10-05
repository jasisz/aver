//! The Map rules and Map facts of the proof steps against every place that
//! runs a map: each is instantiated at sample maps (empty, one entry, two
//! entries), at keys that collide with an entry and keys that do not, for
//! Int, String and record keys, and both sides are run on the VM, on wasm-gc
//! and in the Lean model (`aver proof` samples, decided by Lean). The
//! kernel written in Aver cannot evaluate maps, so it does not take part:
//! it checks each rule as an equation schema, which the proof-step tests
//! cover.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver::ast::{Literal, Spanned};
use aver::ir::hir::ResolvedExpr;
use aver::ir::proof_steps::facts;
use aver::ir::proof_steps::sexpr::BuiltinsOnly;
use aver::ir::proof_steps::term::{self, Term};
use aver::ir::proof_steps::{Eqn, WallRule};
use aver_cmd::{aver_bin, format_output};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// One key type: how to write its sample keys, three of them.
struct Keys {
    name: &'static str,
    keys: [Term; 3],
}

fn int(v: i64) -> Term {
    term::int(&v.into())
}

fn text(s: &str) -> Term {
    Spanned::bare(ResolvedExpr::Literal(Literal::Str(s.to_string())))
}

fn point(x: i64, y: i64) -> Term {
    Spanned::bare(ResolvedExpr::RecordCreate {
        type_id: None,
        type_name: "Pt".into(),
        fields: vec![("x".into(), int(x)), ("y".into(), int(y))],
    })
}

fn key_types() -> Vec<Keys> {
    vec![
        Keys {
            name: "Int",
            keys: [int(1), int(2), int(0 - 7)],
        },
        Keys {
            name: "String",
            keys: [text("a"), text("b"), text("")],
        },
        Keys {
            name: "Pt",
            keys: [point(0, 0), point(1, 2), point(0, 1)],
        },
    ]
}

fn map(entries: Vec<(Term, i64)>) -> Term {
    Spanned::bare(ResolvedExpr::MapLiteral(
        entries.into_iter().map(|(k, v)| (k, int(v))).collect(),
    ))
}

/// A sample map with the keys it holds.
fn maps(keys: &Keys) -> Vec<(Term, Vec<Term>)> {
    let [a, b, _] = keys.keys.clone();
    vec![
        (map(vec![]), vec![]),
        (map(vec![(a.clone(), 10)]), vec![a.clone()]),
        (map(vec![(a.clone(), 10), (b.clone(), 20)]), vec![a, b]),
    ]
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
        .filter(|r| r.id().starts_with("map."))
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
        .filter(|f| f.key.starts_with("Map."))
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

/// Every substitution of samples for a schema's binders, with what each
/// sample map holds.
fn substitutions(binders: &[String], keys: &Keys) -> Vec<(Vec<(String, Term)>, Vec<Term>)> {
    let mut out: Vec<(Vec<(String, Term)>, Vec<Term>)> = vec![(Vec::new(), Vec::new())];
    for b in binders {
        let mut next = Vec::new();
        for (sub, held) in &out {
            let values: Vec<(Term, Vec<Term>)> = match b.as_str() {
                "m" => maps(keys),
                "v" => vec![(int(5), vec![])],
                _ => keys.keys.iter().map(|k| (k.clone(), vec![])).collect(),
            };
            for (value, holds) in values {
                let mut s = sub.clone();
                s.push((b.clone(), value));
                let mut h = held.clone();
                h.extend(holds);
                next.push((s, h));
            }
        }
        out = next;
    }
    out
}

/// Whether a premise holds at a substitution: `k != k2`, or whether the
/// sample map holds a key.
fn holds(premise: &Eqn, sub: &[(String, Term)], held: &[Term]) -> bool {
    let want = term::bool_value(&premise.rhs).expect("a premise `p = true` or `p = false`");
    let at = |t: &Term| term::subst(t, sub).unwrap();
    let value = match &premise.lhs.node {
        ResolvedExpr::BinOp(aver::ast::BinOp::Neq, a, b) => at(a) != at(b),
        ResolvedExpr::Call(_, args) => held.contains(&at(&args[1])),
        _ => panic!("an unexpected premise"),
    };
    value == want
}

fn aver(t: &Term) -> String {
    aver::ir::proof_steps::show::term(t, &BuiltinsOnly)
}

/// The anchor a side's value type goes through: Option, Bool or Int.
fn anchor(t: &Term) -> &'static str {
    match &t.node {
        ResolvedExpr::Call(aver::ir::hir::ResolvedCallee::Builtin(b), _) if b == "Map.get" => {
            "opts"
        }
        ResolvedExpr::Call(aver::ir::hir::ResolvedCallee::Builtin(b), _) if b == "Map.has" => {
            "bools"
        }
        _ => "ints",
    }
}

/// A module whose laws state every sampled instance, both sides, one list
/// per rule and key type.
fn program() -> String {
    let mut src = String::from(
        "module MapSamples\n    intent = \"Every sampled instance of the Map rules and facts.\"\n    exposes [Pt, opts, bools, ints]\n    effects []\n\nrecord Pt\n    x: Int\n    y: Int\n\nfn opts(xs: List<Option<Int>>) -> List<Option<Int>>\n    ? \"Anchor for reads.\"\n    xs\n\nfn bools(xs: List<Bool>) -> List<Bool>\n    ? \"Anchor for membership.\"\n    xs\n\nfn ints(xs: List<Int>) -> List<Int>\n    ? \"Anchor for sizes.\"\n    xs\n",
    );
    let mut n = 0;
    for s in schemas() {
        for keys in key_types() {
            let mut lhs = Vec::new();
            let mut rhs = Vec::new();
            for (sub, held) in substitutions(&s.binders, &keys) {
                if !s.premises.iter().all(|p| holds(p, &sub, &held)) {
                    continue;
                }
                lhs.push(aver(&term::subst(&s.concl.lhs, &sub).unwrap()));
                rhs.push(aver(&term::subst(&s.concl.rhs, &sub).unwrap()));
            }
            assert!(!lhs.is_empty(), "{} over {}: no sample", s.name, keys.name);
            let a = anchor(&s.concl.lhs);
            src.push_str(&format!(
                "\n// {} over {} keys\nverify {a} law sample{n}\n    given k: Int = [0]\n    {a}([{}]) => [{}]\n",
                s.name,
                keys.name,
                lhs.join(", "),
                rhs.join(", ")
            ));
            n += 1;
        }
    }
    src
}

fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-map-samples-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("map_samples.av"), program()).unwrap();
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
fn the_vm_runs_every_map_rule_instance_to_the_same_value() {
    let dir = scratch("vm");
    let out = aver_in(&dir, &["verify", "map_samples.av"]);
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
fn wasm_gc_runs_every_map_rule_instance_to_the_same_value() {
    let dir = scratch("wasm");
    let out = aver_in(&dir, &["verify", "map_samples.av", "--wasm-gc"]);
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && text.contains(" 0 failed") && !text.contains("not checked"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn the_lean_model_evaluates_every_map_rule_instance_to_the_same_value() {
    if !lean_required::lake_available() {
        eprintln!("skipping: `lake` is not available");
        return;
    }
    let dir = scratch("lean");
    let out = aver_in(
        &dir,
        &["proof", "map_samples.av", "-o", "lean", "--check-json"],
    );
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && text.contains("\"build_errors\":0"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}
