//! `warning[serve-path]`: an effectful loop that runs to completion inside
//! one turn of a `Tcp.poll` loop.
//!
//! A function that calls `Tcp.poll` directly is a turn of an event loop:
//! between two waits it serves whatever became ready. If, inside that turn,
//! it reaches a recursive function that performs `Disk.*` or `Tcp.*` work,
//! that function runs to completion before the next wait, and every peer
//! that became ready in the meantime is stalled until it returns.
//!
//! The condition is purely structural, over the module's own call graph:
//!
//! - `F` calls `Tcp.poll` directly;
//! - `G` is reachable from `F` along a path whose intermediate nodes are not
//!   `F` and not members of `F`'s own recursive SCC — re-entering that SCC is
//!   the next turn, not this one;
//! - `G` belongs to a recursive SCC of its own and declares an effect set
//!   that includes a `Disk.*` or `Tcp.*` operation.
//!
//! One warning per `(F, G)` pair, where `G` is the first member of its SCC
//! reached from `F`; the other members of that SCC are the same loop, so
//! they are not reported again for the same `F`. The warning sits on `F`'s
//! call into the path, the place where a turn hands control to the loop.

use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt, StrPart, TailCallData, TopLevel};
use crate::call_graph;

use super::{CheckFinding, FindingSpan, FnSigMap, dotted_name};

const POLL: &str = "Tcp.poll";

pub fn collect_serve_path_warnings(items: &[TopLevel], fn_sigs: &FnSigMap) -> Vec<CheckFinding> {
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let user_fns: HashSet<&str> = fns.iter().map(|fd| fd.name.as_str()).collect();
    let def_lines: HashMap<&str, usize> =
        fns.iter().map(|fd| (fd.name.as_str(), fd.line)).collect();

    // Sorted callee lists so the walk, and with it the `G` a warning names,
    // is the same on every run.
    let mut callees: HashMap<&str, Vec<String>> = HashMap::new();
    for fd in &fns {
        let mut set = HashSet::new();
        call_graph::collect_callees_body(&fd.body, &mut set);
        let mut list: Vec<String> = set.into_iter().collect();
        list.sort();
        callees.insert(fd.name.as_str(), list);
    }
    let scc_ids = call_graph::recursive_scc_ids(items);

    let mut warnings = Vec::new();
    for fd in &fns {
        let direct = &callees[fd.name.as_str()];
        if !direct.iter().any(|callee| callee == POLL) {
            continue;
        }
        let own_scc = scc_ids.get(&fd.name).copied();
        let in_own_loop = |name: &str| {
            name == fd.name || (own_scc.is_some() && scc_ids.get(name).copied() == own_scc)
        };

        let mut reported: BTreeSet<usize> = BTreeSet::new();
        for entry in direct {
            if !user_fns.contains(entry.as_str()) || in_own_loop(entry) {
                continue;
            }
            let mut queue = VecDeque::from([entry.clone()]);
            let mut seen: HashSet<String> = HashSet::new();
            while let Some(name) = queue.pop_front() {
                if !seen.insert(name.clone()) {
                    continue;
                }
                if let Some(&scc) = scc_ids.get(&name)
                    && !reported.contains(&scc)
                    && let Some(effects) = blocking_effects(fn_sigs, &name)
                {
                    reported.insert(scc);
                    warnings.push(CheckFinding {
                        line: find_call_line(&fd.body, entry).unwrap_or(fd.line),
                        module: None,
                        file: None,
                        fn_name: Some(fd.name.clone()),
                        message: format!(
                            "`{name}` is an effectful loop that runs to completion inside one turn of `{f}`; peers waiting on `Tcp.poll` are not served until it returns. Do one step of `{name}` per turn, or run `{name}` as its own command.",
                            f = fd.name
                        ),
                        extra_spans: vec![FindingSpan {
                            line: def_lines.get(name.as_str()).copied().unwrap_or(fd.line),
                            col: 0,
                            len: 0,
                            label: format!("`{name}` is recursive and uses {effects}"),
                        }],
                    });
                }
                if let Some(next) = callees.get(name.as_str()) {
                    for callee in next {
                        if user_fns.contains(callee.as_str()) && !in_own_loop(callee) {
                            queue.push_back(callee.clone());
                        }
                    }
                }
            }
        }
    }
    warnings
}

pub fn collect_serve_path_warnings_in(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
    file: Option<&str>,
) -> Vec<CheckFinding> {
    let mut warnings = collect_serve_path_warnings(items, fn_sigs);
    if let Some(file) = file {
        for warning in &mut warnings {
            warning.file = Some(file.to_string());
        }
    }
    warnings
}

/// The `Disk.*` / `Tcp.*` effects `name` declares, rendered for the label,
/// or `None` when it declares none of them.
fn blocking_effects(fn_sigs: &FnSigMap, name: &str) -> Option<String> {
    let (_, _, effects) = fn_sigs.get(name)?;
    let blocking: Vec<&str> = effects
        .iter()
        .map(String::as_str)
        .filter(|effect| is_blocking_effect(effect))
        .collect();
    (!blocking.is_empty()).then(|| blocking.join(", "))
}

fn is_blocking_effect(effect: &str) -> bool {
    ["Disk", "Tcp"]
        .iter()
        .any(|namespace| effect == *namespace || effect.starts_with(&format!("{namespace}.")))
}

fn find_call_line(body: &FnBody, callee: &str) -> Option<usize> {
    body.stmts().iter().find_map(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => call_line_in_expr(expr, callee),
    })
}

fn call_line_in_expr(expr: &Spanned<Expr>, callee: &str) -> Option<usize> {
    let mut children: Vec<&Spanned<Expr>> = Vec::new();
    match &expr.node {
        Expr::FnCall(func, args) => {
            if dotted_name(func).as_deref() == Some(callee) {
                return Some(expr.line);
            }
            children.push(func);
            children.extend(args.iter());
        }
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if target == callee {
                return Some(expr.line);
            }
            children.extend(args.iter());
        }
        Expr::BinOp(_, left, right) => children.extend([left.as_ref(), right.as_ref()]),
        Expr::Neg(inner) | Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            children.push(inner);
        }
        Expr::Constructor(_, inner) => children.extend(inner.iter().map(Box::as_ref)),
        Expr::Match { subject, arms, .. } => {
            children.push(subject);
            children.extend(arms.iter().map(|arm| arm.body.as_ref()));
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    children.push(inner);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            children.extend(items.iter());
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                children.extend([key, value]);
            }
        }
        Expr::RecordCreate { fields, .. } => children.extend(fields.iter().map(|(_, value)| value)),
        Expr::RecordUpdate { base, updates, .. } => {
            children.push(base);
            children.extend(updates.iter().map(|(_, value)| value));
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
    children
        .into_iter()
        .find_map(|child| call_line_in_expr(child, callee))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn warnings_for(src: &str) -> Vec<CheckFinding> {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = Parser::new(tokens);
        let items = parser.parse().expect("parse failed");
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors.is_empty(),
            "unexpected type errors: {:?}",
            tc.errors
        );
        collect_serve_path_warnings(&items, &tc.fn_sigs)
    }

    const DRAIN: &str = r#"
fn drain(n: Int) -> Result<Unit, String>
    ! [Disk.appendText]
    match n < 1
        true -> Result.Ok(Unit)
        false -> drainStep(n)

fn drainStep(n: Int) -> Result<Unit, String>
    ! [Disk.appendText]
    _written = Disk.appendText("out.log", "line")?
    drain(n - 1)
"#;

    #[test]
    fn warns_once_per_loop_at_the_call_into_it() {
        let src = format!(
            r#"{DRAIN}
fn flush(n: Int) -> Result<Unit, String>
    ! [Disk.appendText]
    drain(n)

fn turn(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.appendText]
    sockets: Map<Int, Tcp.Socket> = {{}}
    ready = Tcp.poll(sockets, 100)?
    flush(n)
"#
        );
        let warnings = warnings_for(&src);
        assert_eq!(warnings.len(), 1, "warnings={warnings:?}");
        let warning = &warnings[0];
        assert_eq!(warning.fn_name.as_deref(), Some("turn"));
        assert!(
            warning.message.starts_with(
                "`drain` is an effectful loop that runs to completion inside one turn of `turn`"
            ),
            "message={}",
            warning.message
        );
        // The line of `flush(n)` inside `turn`, not the line of the loop.
        let flush_call_line = src
            .lines()
            .position(|line| line.trim() == "flush(n)")
            .expect("fixture has the call")
            + 1;
        assert_eq!(warning.line, flush_call_line);
        assert_eq!(warning.extra_spans.len(), 1);
        assert!(warning.extra_spans[0].label.contains("Disk.appendText"));
    }

    #[test]
    fn no_warning_without_a_poll_on_the_path() {
        let src = format!(
            r#"{DRAIN}
fn flush(n: Int) -> Result<Unit, String>
    ! [Disk.appendText]
    drain(n)
"#
        );
        assert!(warnings_for(&src).is_empty());
    }

    #[test]
    fn the_polling_function_may_be_its_own_effectful_loop() {
        let src = r#"
fn serve(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.appendText]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    _written = Disk.appendText("out.log", "line")?
    match n < 1
        true -> Result.Ok(Unit)
        false -> serve(n - 1)
"#;
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn a_pure_loop_inside_the_turn_is_not_reported() {
        let src = r#"
fn spin(n: Int, acc: Int) -> Int
    match n < 1
        true -> acc
        false -> spin(n - 1, acc + 1)

fn turn(n: Int) -> Result<Int, String>
    ! [Tcp.poll]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    Result.Ok(spin(n, 0))
"#;
        assert!(warnings_for(src).is_empty());
    }
}
