//! The rules of the proof steps for `Int.min`, `Int.max`, `Int.abs`, the
//! sign of a product (`int.mul_nonneg`) and `List.contains` against every place that runs those builtins: each rule
//! is instantiated at sample values (negative, zero, equal and unequal
//! arguments; the empty list, a list that holds the element in front, further
//! back, twice, and not at all) and both sides are run on the VM, on wasm-gc
//! and in the Lean model (`aver proof` samples, decided by Lean). The kernel
//! written in Aver does not evaluate these builtins, so it does not take
//! part: it checks each rule as an equation schema, which the proof-step
//! tests cover.
//!
//! `List.contains` is also run on lists of Float, with `0.0 / 0.0`, which is
//! not equal to itself: the VM and wasm-gc must skip an element in front
//! wherever `y == x` is false there too, and must not find `0.0 / 0.0` in a
//! list that holds it, which is why the rules on an element in front bind
//! it only at a type whose `==` is equality. A Float is outside the step format, so these samples are written
//! as source, with the comparison computed here as IEEE 754 does.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/lean_required.rs"]
mod lean_required;

use aver::ir::proof_steps::WallRule;
use aver::ir::proof_steps::sexpr::BuiltinsOnly;
use aver::ir::proof_steps::term::{self, Term};
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

fn rules() -> Vec<WallRule> {
    WallRule::ALL
        .iter()
        .copied()
        .filter(|r| {
            [
                "int.min.",
                "int.max.",
                "int.abs.",
                "int.mul_nonneg",
                "list.contains.",
            ]
            .iter()
            .any(|p| r.id().starts_with(p))
        })
        .collect()
}

/// Sample values for a binder of a rule: Ints for the Int rules; for
/// `List.contains` an element in front `x`, the rest `a` and the element
/// looked for `y`.
fn samples(rule: WallRule, binder: &str) -> Vec<Term> {
    if rule.id().starts_with("list.") {
        return match binder {
            "a" => vec![list(&[]), list(&[2]), list(&[7, 2]), list(&[2, 2, 3])],
            _ => vec![int(2), int(3), int(-1)],
        };
    }
    [-BIG, -5, -1, 0, 1, 5, BIG]
        .iter()
        .map(|v| int(*v))
        .collect()
}

fn substitutions(rule: WallRule) -> Vec<Vec<(String, Term)>> {
    rule.binders().iter().fold(vec![Vec::new()], |acc, b| {
        acc.iter()
            .flat_map(|prefix| {
                samples(rule, b).into_iter().map(move |v| {
                    let mut next = prefix.clone();
                    next.push((b.to_string(), v));
                    next
                })
            })
            .collect()
    })
}

fn aver(t: &Term) -> String {
    aver::ir::proof_steps::show::term(t, &BuiltinsOnly)
}

/// A module whose laws state every sampled instance whose premise holds,
/// both sides, one list per rule; `k` is a given only so that each law has
/// one sample.
fn program() -> String {
    let mut src = String::from(
        "module PickSamples\n    intent = \"Every sampled instance of the rules for Int.min, Int.max, Int.abs, the sign of a product and List.contains.\"\n    exposes [ints, bools]\n    effects []\n\nfn ints(xs: List<Int>) -> List<Int>\n    ? \"Anchor for Int-valued instances.\"\n    xs\n\nfn bools(xs: List<Bool>) -> List<Bool>\n    ? \"Anchor for Bool-valued instances.\"\n    xs\n",
    );
    for (n, rule) in rules().into_iter().enumerate() {
        let (premises, concl) = rule.schema();
        let mut lhs = Vec::new();
        let mut rhs = Vec::new();
        for sub in substitutions(rule) {
            let at = |t: &Term| term::subst(t, &sub).unwrap();
            let holds = premises
                .iter()
                .all(|p| term::eval_closed(&at(&p.lhs)).as_ref() == Some(&p.rhs));
            if !holds {
                continue;
            }
            lhs.push(aver(&at(&concl.lhs)));
            rhs.push(aver(&at(&concl.rhs)));
        }
        assert!(
            lhs.len() >= 3,
            "{}: too few samples satisfy the premise",
            rule.id()
        );
        let anchor = if rule.id().starts_with("list.") || term::bool_value(&concl.rhs).is_some() {
            "bools"
        } else {
            "ints"
        };
        src.push_str(&format!(
            "\n// {}\nverify {anchor} law sample{n}\n    given k: Int = [0]\n    {anchor}([{}]) => [{}]\n",
            rule.id(),
            lhs.join(", "),
            rhs.join(", ")
        ));
    }
    src
}

/// A Float sample as Aver source.
fn float(v: f64) -> String {
    if v.is_nan() {
        "(0.0 / 0.0)".to_string()
    } else {
        format!("{v:?}")
    }
}

/// `List.contains` on lists of Float: both sides of `list.contains.nil`, of
/// `list.contains.cons_found` at every sample where `y == x` is true and of
/// `list.contains.cons_other` where it is false, and the value of `List.contains(List.prepend(x, []), x)`, which is false
/// for `0.0 / 0.0`.
fn float_program() -> String {
    let values = [0.0, 1.5, -2.0, f64::NAN];
    let rests: [&[f64]; 4] = [&[], &[1.5], &[f64::NAN], &[-2.0, f64::NAN, 1.5]];
    let list = |xs: &[f64]| {
        format!(
            "[{}]",
            xs.iter().map(|v| float(*v)).collect::<Vec<_>>().join(", ")
        )
    };
    let (mut lhs, mut rhs) = (Vec::new(), Vec::new());
    for y in values {
        lhs.push(format!("List.contains([], {})", float(y)));
        rhs.push("false".to_string());
        for x in values {
            // `(y == x)` as IEEE 754 computes it decides between
            // `list.contains.cons_found` and `list.contains.cons_other`.
            for a in rests {
                lhs.push(format!(
                    "List.contains(List.prepend({}, {}), {})",
                    float(x),
                    list(a),
                    float(y)
                ));
                rhs.push(if y == x {
                    "true".to_string()
                } else {
                    format!("List.contains({}, {})", list(a), float(y))
                });
            }
        }
    }
    assert!(
        lhs.iter()
            .any(|l| l.contains("List.prepend((0.0 / 0.0), [(0.0 / 0.0)]), (0.0 / 0.0))"))
    );
    let itself: Vec<String> = values
        .iter()
        .map(|x| {
            format!(
                "List.contains(List.prepend({}, []), {})",
                float(*x),
                float(*x)
            )
        })
        .collect();
    let found: Vec<String> = values.iter().map(|x| (!x.is_nan()).to_string()).collect();
    format!(
        "module PickFloatSamples\n    intent = \"List.contains on lists of Float, with a value that is not equal to itself.\"\n    exposes [bools]\n    effects []\n\nfn bools(xs: List<Bool>) -> List<Bool>\n    ? \"Anchor for Bool-valued instances.\"\n    xs\n\nverify bools law rules\n    given k: Int = [0]\n    bools([{}]) => bools([{}])\n\nverify bools law holdsItself\n    given k: Int = [0]\n    bools([{}]) => [{}]\n",
        lhs.join(", "),
        rhs.join(", "),
        itself.join(", "),
        found.join(", ")
    )
}

fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-pick-samples-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("pick_samples.av"), program()).unwrap();
    fs::write(dir.join("pick_float_samples.av"), float_program()).unwrap();
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
fn every_rule_is_sampled() {
    assert_eq!(rules().len(), 10);
    let src = program();
    for rule in rules() {
        assert!(src.contains(&format!("// {}\n", rule.id())), "{src}");
    }
}

#[test]
fn the_vm_runs_every_pick_rule_instance_to_the_same_value() {
    let dir = scratch("vm");
    for file in ["pick_samples.av", "pick_float_samples.av"] {
        let out = aver_in(&dir, &["verify", file]);
        let text = String::from_utf8_lossy(&out.stdout);
        assert!(
            out.status.success() && text.contains(" 0 failed") && !text.contains("not checked"),
            "{file}: {}",
            format_output(&out)
        );
    }
    let _ = fs::remove_dir_all(dir);
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_runs_every_pick_rule_instance_to_the_same_value() {
    let dir = scratch("wasm");
    for file in ["pick_samples.av", "pick_float_samples.av"] {
        let out = aver_in(&dir, &["verify", file, "--wasm-gc"]);
        let text = String::from_utf8_lossy(&out.stdout);
        assert!(
            out.status.success() && text.contains(" 0 failed") && !text.contains("not checked"),
            "{file}: {}",
            format_output(&out)
        );
    }
    let _ = fs::remove_dir_all(dir);
}

#[test]
fn the_lean_model_evaluates_every_pick_rule_instance_to_the_same_value() {
    if !lean_required::lake_available() {
        eprintln!("skipping: `lake` is not available");
        return;
    }
    let dir = scratch("lean");
    let out = aver_in(
        &dir,
        &["proof", "pick_samples.av", "-o", "lean", "--check-json"],
    );
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && text.contains("\"build_errors\":0"),
        "{}",
        format_output(&out)
    );
    let _ = fs::remove_dir_all(dir);
}
