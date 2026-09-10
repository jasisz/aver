//! `warning[serve-path]`: an effectful loop that runs to completion inside
//! one turn of a `Tcp.poll` loop.
//!
//! A function that calls `Tcp.poll` directly is a turn of an event loop:
//! between two waits it serves whatever became ready. If, inside that turn,
//! it reaches a loop that reads from `Disk` or `Tcp` on every step and
//! whose recursion is not bounded by the list it was handed, that loop runs
//! to completion before the next wait, and every peer that became ready in
//! the meantime is stalled until it returns.
//!
//! The condition is purely structural, over the module's own call graph:
//!
//! - `F` calls `Tcp.poll` directly.
//! - The recursive components are computed on the call graph WITHOUT `F`'s
//!   node. A cycle that passes through `F` passes through its wait, so it
//!   disappears with `F` and is never a stall; a cycle that avoids `F`'s
//!   wait survives the cut and is one.
//! - The walk starts at `F`'s callees, never re-enters `F`, and stops at
//!   any function that itself calls `Tcp.poll` directly: that is the next
//!   turn boundary, not a stall. `Tcp.poll` is never a blocking effect for
//!   this check.
//! - `G` is reached, is recursive in the reduced graph, and declares an
//!   INPUT operation: any `Disk.read*`, `Disk.size`, `Tcp.read*`,
//!   `Tcp.accept`, `Tcp.dialled`, `Tcp.peerAddress`, or the bare namespaces
//!   `Disk` / `Tcp`. Writes alone (`Disk.write*`, `Disk.append*`,
//!   `Tcp.write*`, `Tcp.close`) do not qualify: a loop that only writes
//!   what it already holds is bounded by this turn's data, and the turn
//!   budget in `verify` covers its length.
//! - `G` is excluded when its recursion is a bounded walk over a list that
//!   arrived as a parameter: every recursive call inside `G`'s component
//!   passes, at the position of a parameter the caller matched with
//!   `[_, ..rest]`, that `rest`. Serving each ready key once is the correct
//!   server shape; a loop that recurses on a counter, on a value read from
//!   the world, or on anything else is the stall the warning is for.
//!
//! One warning per `(F, component)`, naming the first member of the
//! component reached from `F`; the other members are the same loop. The
//! warning sits on `F`'s call into the path, the place where a turn hands
//! control to the loop.

use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::ast::{Expr, FnBody, FnDef, Pattern, Spanned, Stmt, StrPart, TailCallData, TopLevel};
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
    let defs: HashMap<&str, &FnDef> = fns.iter().map(|fd| (fd.name.as_str(), *fd)).collect();

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
    let polls_directly = |name: &str| {
        callees
            .get(name)
            .is_some_and(|list| list.iter().any(|callee| callee == POLL))
    };

    let mut warnings = Vec::new();
    for fd in &fns {
        if !polls_directly(&fd.name) {
            continue;
        }
        // The loops that survive without `F`: a cycle through `F` passes
        // through its wait and is gone; a cycle that avoids it stays.
        let sccs = recursive_sccs_without(&user_fns, &callees, &fd.name);
        let scc_ids: HashMap<&str, usize> = sccs
            .iter()
            .enumerate()
            .flat_map(|(id, scc)| scc.iter().map(move |name| (name.as_str(), id)))
            .collect();
        // The shrinking-list test is a property of the component, computed once.
        let mut bounded_walks: HashMap<usize, bool> = HashMap::new();
        let mut is_bounded_walk = |scc: usize| -> bool {
            *bounded_walks.entry(scc).or_insert_with(|| {
                let members: Vec<&FnDef> =
                    sccs[scc].iter().map(|name| defs[name.as_str()]).collect();
                is_shrinking_list_scc(&members)
            })
        };
        // A callee the walk does not enter: `F` itself, or another turn
        // boundary.
        let stops_at = |name: &str| name == fd.name || polls_directly(name);

        let mut reported: BTreeSet<usize> = BTreeSet::new();
        let mut seen: HashSet<String> = HashSet::new();
        for entry in &callees[fd.name.as_str()] {
            if !user_fns.contains(entry.as_str()) || stops_at(entry) {
                continue;
            }
            let mut queue = VecDeque::from([entry.clone()]);
            while let Some(name) = queue.pop_front() {
                if !seen.insert(name.clone()) {
                    continue;
                }
                if let Some(&scc) = scc_ids.get(name.as_str())
                    && !reported.contains(&scc)
                    && let Some(effects) = input_effects(fn_sigs, &name)
                    && !is_bounded_walk(scc)
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
                            label: format!("`{name}` is recursive and reads through {effects}"),
                        }],
                    });
                }
                if let Some(next) = callees.get(name.as_str()) {
                    for callee in next {
                        if user_fns.contains(callee.as_str()) && !stops_at(callee) {
                            queue.push_back(callee.clone());
                        }
                    }
                }
            }
        }
    }
    warnings
}

/// The recursive components of the module-local call graph with `removed`
/// taken out: every user function but `removed` is a node, and an edge into
/// `removed` is dropped. A component is recursive when it has more than one
/// member or a member that calls itself. Members are sorted, and so are the
/// components, by their first member.
fn recursive_sccs_without(
    user_fns: &HashSet<&str>,
    callees: &HashMap<&str, Vec<String>>,
    removed: &str,
) -> Vec<Vec<String>> {
    let mut nodes: Vec<String> = user_fns
        .iter()
        .filter(|name| **name != removed)
        .map(|name| name.to_string())
        .collect();
    nodes.sort();
    let adj: HashMap<String, Vec<String>> = nodes
        .iter()
        .map(|name| {
            let edges = callees
                .get(name.as_str())
                .map(|list| {
                    list.iter()
                        .filter(|callee| {
                            user_fns.contains(callee.as_str()) && callee.as_str() != removed
                        })
                        .cloned()
                        .collect()
                })
                .unwrap_or_default();
            (name.clone(), edges)
        })
        .collect();
    let mut sccs: Vec<Vec<String>> = crate::scc::tarjan_sccs(&nodes, &adj)
        .into_iter()
        .filter(|scc| scc.len() > 1 || scc.first().is_some_and(|name| adj[name].contains(name)))
        .collect();
    for scc in &mut sccs {
        scc.sort();
    }
    sccs.sort();
    sccs
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

/// The input operations `name` declares, rendered for the label, or `None`
/// when it declares none of them.
fn input_effects(fn_sigs: &FnSigMap, name: &str) -> Option<String> {
    let (_, _, effects) = fn_sigs.get(name)?;
    let inputs: Vec<&str> = effects
        .iter()
        .map(String::as_str)
        .filter(|effect| is_input_effect(effect))
        .collect();
    (!inputs.is_empty()).then(|| inputs.join(", "))
}

/// An effect that reads the world: `Disk.read*`, `Disk.size`, `Tcp.read*`,
/// `Tcp.accept`, `Tcp.dialled`, `Tcp.peerAddress`, or a bare `Disk` / `Tcp`
/// namespace. Writes alone do not make a loop a stall: what they write is
/// bounded by this turn's data. `Tcp.poll` is a wait, never an input.
fn is_input_effect(effect: &str) -> bool {
    match effect.split_once('.') {
        None => effect == "Disk" || effect == "Tcp",
        Some(("Disk", op)) => op.starts_with("read") || op == "size",
        Some(("Tcp", op)) => {
            op.starts_with("read") || matches!(op, "accept" | "dialled" | "peerAddress")
        }
        Some(_) => false,
    }
}

// ---------------------------------------------------------------------------
// The shrinking-list test
// ---------------------------------------------------------------------------

/// True when the component's recursion is a bounded walk over a list that
/// arrived as a parameter: every recursive call in every member passes, at
/// the position of a parameter the caller matched with `[_, ..rest]`, that
/// `rest`. The position is compared between the caller's parameter and the
/// callee's argument, so a sibling that carries the list at another position
/// does not count as shrinking it.
fn is_shrinking_list_scc(members: &[&FnDef]) -> bool {
    let names: HashSet<&str> = members.iter().map(|fd| fd.name.as_str()).collect();
    members
        .iter()
        .all(|fd| every_recursive_call_shrinks(fd, &names))
}

fn every_recursive_call_shrinks(fd: &FnDef, scc: &HashSet<&str>) -> bool {
    // Parameters still naming what the caller passed: a binding or a
    // pattern that reuses the name takes the parameter out of the set.
    let mut live: HashMap<&str, usize> = fd
        .params
        .iter()
        .enumerate()
        .map(|(idx, (name, _))| (name.as_str(), idx))
        .collect();
    let mut tails: Vec<(usize, &str)> = Vec::new();
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                if !calls_shrink(expr, &live, &mut tails, scc) {
                    return false;
                }
                live.remove(name.as_str());
            }
            Stmt::Expr(expr) => {
                if !calls_shrink(expr, &live, &mut tails, scc) {
                    return false;
                }
            }
        }
    }
    true
}

/// Walks `expr` with `tails` holding, for every enclosing `[_, ..rest]` arm
/// on a live parameter, that parameter's position and the name of `rest`.
/// False as soon as a call into the component passes no such `rest` at its
/// parameter's position.
fn calls_shrink<'a>(
    expr: &'a Spanned<Expr>,
    live: &HashMap<&'a str, usize>,
    tails: &mut Vec<(usize, &'a str)>,
    scc: &HashSet<&str>,
) -> bool {
    let shrinks = |args: &[Spanned<Expr>], tails: &[(usize, &str)]| {
        args.iter().enumerate().any(|(position, arg)| {
            matches!(&arg.node, Expr::Ident(name)
                if tails.iter().any(|(param, rest)| *param == position && rest == name))
        })
    };
    match &expr.node {
        Expr::FnCall(func, args) => {
            if dotted_name(func).is_some_and(|name| scc.contains(name.as_str()))
                && !shrinks(args, tails)
            {
                return false;
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData { target, args } = boxed.as_ref();
            if scc.contains(target.as_str()) && !shrinks(args, tails) {
                return false;
            }
        }
        Expr::Match { subject, arms } => {
            if !calls_shrink(subject, live, tails, scc) {
                return false;
            }
            let matched_param = match &subject.node {
                Expr::Ident(name) => live.get(name.as_str()).copied(),
                _ => None,
            };
            for arm in arms {
                let mut bound = Vec::new();
                pattern_bindings(&arm.pattern, &mut bound);
                let mut arm_live = live.clone();
                for name in &bound {
                    arm_live.remove(name);
                }
                let pushed = match (matched_param, &arm.pattern) {
                    (Some(param), Pattern::Cons(_, rest)) => {
                        tails.push((param, rest.as_str()));
                        true
                    }
                    _ => false,
                };
                let ok = calls_shrink(&arm.body, &arm_live, tails, scc);
                if pushed {
                    tails.pop();
                }
                if !ok {
                    return false;
                }
            }
            return true;
        }
        _ => {}
    }
    children(expr)
        .into_iter()
        .all(|child| calls_shrink(child, live, tails, scc))
}

fn pattern_bindings<'a>(pattern: &'a Pattern, out: &mut Vec<&'a str>) {
    match pattern {
        Pattern::Ident(name) => out.push(name),
        Pattern::Cons(head, tail) => out.extend([head.as_str(), tail.as_str()]),
        Pattern::Tuple(items) => items.iter().for_each(|item| pattern_bindings(item, out)),
        Pattern::Constructor(_, names) => out.extend(names.iter().map(String::as_str)),
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
}

// ---------------------------------------------------------------------------
// Locating the call into the path
// ---------------------------------------------------------------------------

fn find_call_line(body: &FnBody, callee: &str) -> Option<usize> {
    body.stmts().iter().find_map(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => call_line_in_expr(expr, callee),
    })
}

fn call_line_in_expr(expr: &Spanned<Expr>, callee: &str) -> Option<usize> {
    match &expr.node {
        Expr::FnCall(func, _) if dotted_name(func).as_deref() == Some(callee) => {
            return Some(expr.line);
        }
        Expr::TailCall(boxed) if boxed.target == callee => return Some(expr.line),
        _ => {}
    }
    children(expr)
        .into_iter()
        .find_map(|child| call_line_in_expr(child, callee))
}

/// Every immediate sub-expression of `expr`, match arm bodies included.
fn children(expr: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    let mut children: Vec<&Spanned<Expr>> = Vec::new();
    match &expr.node {
        Expr::FnCall(func, args) => {
            children.push(func);
            children.extend(args.iter());
        }
        Expr::TailCall(boxed) => children.extend(boxed.args.iter()),
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
    ! [Disk.readText]
    match n < 1
        true -> Result.Ok(Unit)
        false -> drainStep(n)

fn drainStep(n: Int) -> Result<Unit, String>
    ! [Disk.readText]
    _line = Disk.readText("queue.log")?
    drain(n - 1)
"#;

    #[test]
    fn warns_once_per_loop_at_the_call_into_it() {
        let src = format!(
            r#"{DRAIN}
fn flush(n: Int) -> Result<Unit, String>
    ! [Disk.readText]
    drain(n)

fn turn(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
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
        assert!(warning.extra_spans[0].label.contains("Disk.readText"));
    }

    #[test]
    fn no_warning_without_a_poll_on_the_path() {
        let src = format!(
            r#"{DRAIN}
fn flush(n: Int) -> Result<Unit, String>
    ! [Disk.readText]
    drain(n)
"#
        );
        assert!(warnings_for(&src).is_empty());
    }

    #[test]
    fn the_polling_function_may_be_its_own_effectful_loop() {
        let src = r#"
fn serve(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    _line = Disk.readText("queue.log")?
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

    #[test]
    fn a_shrinking_walk_over_the_ready_list_is_the_server_shape() {
        let src = r#"
fn dispatch(sockets: Map<Int, Tcp.Socket>, ready: List<Int>) -> Result<Unit, String>
    ! [Disk.readText]
    match ready
        [] -> Result.Ok(Unit)
        [_, ..rest] -> match Disk.readText("queue.log")
            Result.Err(reason) -> Result.Err(reason)
            Result.Ok(_) -> dispatch(sockets, rest)

fn serve(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    _served = dispatch(sockets, ready)?
    match n < 1
        true -> Result.Ok(Unit)
        false -> serve(n - 1)
"#;
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn a_walk_that_recurses_on_something_else_is_a_stall() {
        // The list shrinks, but the recursion is driven by the counter: one
        // arm passes `rest` at the wrong position, the other ignores it.
        let src = r#"
fn walk(ready: List<Int>, n: Int) -> Result<Unit, String>
    ! [Disk.readText]
    match ready
        [] -> Result.Ok(Unit)
        [_, ..rest] -> match Disk.readText("queue.log")
            Result.Err(reason) -> Result.Err(reason)
            Result.Ok(_) -> walk(ready, n - 1)

fn serve(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    _served = walk(ready, n)?
    match n < 1
        true -> Result.Ok(Unit)
        false -> serve(n - 1)
"#;
        let warnings = warnings_for(src);
        assert_eq!(warnings.len(), 1, "warnings={warnings:?}");
        assert!(
            warnings[0]
                .message
                .starts_with("`walk` is an effectful loop")
        );
    }

    #[test]
    fn a_sibling_of_the_poller_is_walked_and_its_loop_reported() {
        let src = format!(
            r#"{DRAIN}
fn serve(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {{}}
    ready = Tcp.poll(sockets, 100)?
    handle(n, ready)

fn handle(n: Int, ready: List<Int>) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    _drained = drain(List.len(ready))?
    match n < 1
        true -> Result.Ok(Unit)
        false -> serve(n - 1)
"#
        );
        let warnings = warnings_for(&src);
        assert_eq!(warnings.len(), 1, "warnings={warnings:?}");
        let warning = &warnings[0];
        assert_eq!(warning.fn_name.as_deref(), Some("serve"));
        assert!(warning.message.starts_with("`drain` is an effectful loop"));
    }

    #[test]
    fn a_poller_that_hands_off_to_another_poller_is_the_next_turn() {
        let src = r#"
fn run(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {}
    first = Tcp.poll(sockets, 0)?
    pollEven(n + List.len(first))

fn pollEven(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    _line = Disk.readText("even.log")?
    match n < 1
        true -> Result.Ok(Unit)
        false -> pollOdd(n - 1)

fn pollOdd(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    _line = Disk.readText("odd.log")?
    match n < 1
        true -> Result.Ok(Unit)
        false -> pollEven(n - 1)
"#;
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn a_loop_that_only_writes_what_it_holds_is_not_reported() {
        // Same shape as DRAIN, but every step appends instead of reading:
        // the loop is bounded by this turn's data, and the turn budget in
        // `verify` covers its length.
        let src = r#"
fn drain(n: Int) -> Result<Unit, String>
    ! [Disk.appendText, Tcp.close]
    match n < 1
        true -> Result.Ok(Unit)
        false -> match Disk.appendText("out.log", "line")
            Result.Err(reason) -> Result.Err(reason)
            Result.Ok(_) -> drain(n - 1)

fn turn(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.appendText, Tcp.close]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    drain(n + List.len(ready))
"#;
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn a_handler_cycle_that_avoids_the_wait_is_reported() {
        // `handle` recurses through `serve` (that cycle passes the wait and
        // vanishes without `serve`) and through itself (that one survives):
        // the self-loop is a stall inside `serve`'s turn.
        let src = r#"
fn serve(n: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    sockets: Map<Int, Tcp.Socket> = {}
    ready = Tcp.poll(sockets, 100)?
    handle(n, List.len(ready))

fn handle(n: Int, pending: Int) -> Result<Unit, String>
    ! [Tcp.poll, Disk.readText]
    match pending < 1
        true -> serve(n - 1)
        false -> match Disk.readText("queue.log")
            Result.Err(reason) -> Result.Err(reason)
            Result.Ok(_) -> handle(n, pending - 1)
"#;
        let warnings = warnings_for(src);
        assert_eq!(warnings.len(), 1, "warnings={warnings:?}");
        assert_eq!(warnings[0].fn_name.as_deref(), Some("serve"));
        assert!(
            warnings[0].message.starts_with(
                "`handle` is an effectful loop that runs to completion inside one turn of `serve`"
            ),
            "message={}",
            warnings[0].message
        );
    }
}
