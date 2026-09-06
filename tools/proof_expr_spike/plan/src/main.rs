//! Experimental source-plan extraction. No changes to the production exporter.
use aver::ast::{Expr, FnBody, FnDef, Spanned, Stmt, TopLevel, VerifyKind};
use aver::codegen::expr_walk::walk;
use serde_json::{Value, json};
use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;

fn expressions(f: &FnDef) -> impl Iterator<Item = &Spanned<Expr>> {
    let FnBody::Block(stmts) = f.body.as_ref();
    stmts.iter().map(|s| match s {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => e,
    })
}

fn calls(e: &Spanned<Expr>) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    walk(e, &mut |e| {
        if let Expr::FnCall(callee, _) = &e.node
            && let Expr::Ident(name) = &callee.node
        {
            names.insert(name.clone());
        }
    });
    names
}

fn shape(
    e: &Spanned<Expr>,
    functions: &BTreeMap<&str, &FnDef>,
    first: &str,
) -> Result<(String, String, Vec<usize>), Box<dyn Error>> {
    let Expr::FnCall(callee, args) = &e.node else {
        return Ok(("plain".into(), String::new(), vec![]));
    };
    let Expr::Ident(name) = &callee.node else {
        return Ok(("plain".into(), String::new(), vec![]));
    };
    let Some(f) = functions.get(name.as_str()) else {
        return Ok(("plain".into(), String::new(), vec![]));
    };
    let Some(body) = expressions(f).next() else {
        return Err("empty body".into());
    };
    let Expr::Match { subject, arms } = &body.node else {
        return Ok(("plain".into(), String::new(), vec![]));
    };
    let parameter = f.params.first().ok_or("match reason needs a parameter")?;
    // Intentionally narrow spike: preserve all remaining parameters under induction.
    if !matches!(&subject.node, Expr::Ident(n) if n == &parameter.0)
        || !matches!(args.first().map(|e| &e.node), Some(Expr::Ident(n)) if n == first)
    {
        return Err("spike supports a match on the first given parameter only".into());
    }
    let recursive = expressions(f).any(|e| calls(e).contains(name));
    if recursive && !parameter.1.starts_with("List<") {
        return Err("spike supports recursive List reasons only".into());
    }
    Ok((
        if recursive { "induction" } else { "cases" }.into(),
        name.clone(),
        arms.iter().map(|a| a.body.line).collect(),
    ))
}

fn main() -> Result<(), Box<dyn Error>> {
    let source = std::env::args().nth(1).ok_or("expected corpus.av")?;
    let ast = aver::source::parse_source(&std::fs::read_to_string(&source)?)?;
    let errors = aver::types::checker::run_type_check(&ast);
    if !errors.is_empty() {
        return Err(format!("type errors: {errors:?}").into());
    }
    let module = ast
        .iter()
        .find_map(|x| match x {
            TopLevel::Module(m) => Some(m.name.as_str()),
            _ => None,
        })
        .ok_or("expected module")?;
    let functions: BTreeMap<_, _> = ast
        .iter()
        .filter_map(|x| match x {
            TopLevel::FnDef(f) => Some((f.name.as_str(), f)),
            _ => None,
        })
        .collect();
    let mut laws = Vec::new();
    for item in &ast {
        let TopLevel::Verify(v) = item else { continue };
        let VerifyKind::Law(law) = &v.kind else {
            continue;
        };
        if law.using.as_ref().is_none_or(|xs| !xs.is_empty()) {
            return Err("spike requires explicit using []".into());
        }
        let first = &law.givens.first().ok_or("expected given")?.name;
        let name = format!("{}_law_{}", v.fn_name, law.name);
        let mut cone = BTreeSet::new();
        for e in law
            .because
            .iter()
            .chain([&law.lhs, &law.rhs])
            .chain(law.when.iter())
        {
            cone.extend(calls(e));
        }
        loop {
            let old = cone.clone();
            for name in &old {
                if let Some(f) = functions.get(name.as_str()) {
                    for e in expressions(f) {
                        cone.extend(calls(e))
                    }
                }
            }
            if old == cone {
                break;
            }
        }
        let definitions: Vec<_> = cone
            .iter()
            .filter(|n| functions.contains_key(n.as_str()))
            .map(|n| format!("{module}.{n}"))
            .collect();
        let mut stages: Vec<Value> = Vec::new();
        for (i, reason) in law.because.iter().enumerate() {
            let (shape, unfold, branches) = shape(reason, &functions, first)?;
            stages.push(json!({
                "theorem": format!("{module}.__aver_reason_{name}_because{}", i + 1),
                "label": format!("{}.{}.because{}", v.fn_name, law.name, i + 1),
                "line": reason.line, "shape": shape,
                "unfold": if unfold.is_empty() { unfold } else { format!("{module}.{unfold}") },
                "branches": branches
            }));
        }
        // The implication needs no new induction hypothesis. A structural case split
        // exposes the already proved reason in a constructor branch.
        let implication_shape = if stages.iter().any(|s| s["shape"] != "plain") {
            "cases"
        } else {
            "plain"
        };
        stages.push(json!({
            "theorem": format!("{module}.__aver_reason_{name}_implication"),
            "label": format!("{}.{}.implication", v.fn_name, law.name),
            "line": law.lhs.line, "shape": implication_shape, "unfold": "", "branches": []
        }));
        laws.push(json!({
            "theorem": format!("{module}.{name}"),
            "given_count": law.givens.len(), "guard": law.when.is_some(),
            "definitions": definitions, "stages": stages
        }));
    }
    println!(
        "{}",
        serde_json::to_string_pretty(&json!({
            "source": source, "module": module, "laws": laws
        }))?
    );
    Ok(())
}
