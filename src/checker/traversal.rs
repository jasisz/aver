#![allow(clippy::doc_lazy_continuation)]
//! Traversal-pass lints — surfaces antipatterns that the buffer-build
//! deforestation pass deliberately *does not* fuse.
//!
//! The fuse-vs-warn split is intentional: silent fusion fits when Aver
//! forces the idiom (string interpolation has no user-level alternative;
//! `String.join(<sink>(args, []), sep)` has no exposed `String.Builder`
//! to write directly). It does NOT fit when Aver already publishes a
//! more direct primitive — fusing then would hide both the cost and
//! the better idiom from the programmer.
//!
//! Three patterns land here:
//!
//! 1. `Vector.fromList(<sink>(args, []))` — Aver has `Vector.new(N, default)`
//!    + `Option.withDefault(Vector.set(v, i, x), v)` (the owned-mutate
//!    fast path published in 0.14.0). Building a `List<T>` only to flatten
//!    it into a `Vector<T>` allocates ~2N cons cells (N from prepend, N
//!    from the implicit reverse-during-fromList walk) plus the final
//!    `Vec<T>`. The direct-fill version skips the whole list intermediate.
//!
//! 2. `Map.fromList(<sink>(args, []))` — same pattern with `{}` (empty
//!    map literal) + `Map.set` chain. Marginal in practice (~6 sites
//!    in-tree) but treated uniformly with Vector for consistency of
//!    advice.
//!
//! 3. Standalone `List.reverse(<sink>(args, []))` whose result is *not*
//!    fed straight into `String.join` (when it is, the external-reverse
//!    fusion path already eliminates the explicit reverse). When the
//!    result is consumed as a `List<T>`, the user has the option to
//!    write the sink so that the base case returns `acc` already in the
//!    right order — typically by accumulating with append-via-tail-cons
//!    instead of prepend, or by computing index-based offsets.
//!
//! All three reuse the same sink-shape detector `compute_buffer_build_sinks`
//! the deforestation pass uses, so the lint and the optimization stay
//! in lockstep — what we don't fuse, we warn about.

use std::collections::HashMap;

use crate::ast::{Expr, FnDef, Pattern, Spanned, Stmt, TopLevel};
use crate::ir::{BufferBuildShape, compute_buffer_build_sinks};

use super::CheckFinding;

pub fn collect_traversal_warnings_in(items: &[TopLevel], file: Option<&str>) -> Vec<CheckFinding> {
    let fn_refs: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let sinks = compute_buffer_build_sinks(&fn_refs);
    if sinks.is_empty() {
        return Vec::new();
    }

    let module_name = items.iter().find_map(|item| match item {
        TopLevel::Module(m) => Some(m.name.clone()),
        _ => None,
    });

    let mut warnings = Vec::new();
    for fd in &fn_refs {
        for stmt in fd.body.stmts() {
            match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => walk(
                    &expr.node,
                    expr.line,
                    /* inside_string_join = */ false,
                    fd,
                    &sinks,
                    &module_name,
                    file,
                    &mut warnings,
                ),
            }
        }
    }
    warnings
}

#[allow(clippy::too_many_arguments)]
fn walk(
    expr: &Expr,
    fallback_line: usize,
    inside_string_join: bool,
    fd: &FnDef,
    sinks: &HashMap<String, BufferBuildShape>,
    module_name: &Option<String>,
    file: Option<&str>,
    out: &mut Vec<CheckFinding>,
) {
    if let Expr::FnCall(callee, args) = expr {
        // Vector.fromList(<sink>(args, []))
        if is_dotted(&callee.node, "Vector", "fromList")
            && args.len() == 1
            && let Some(name) = sink_call_with_empty_acc(&args[0].node, sinks)
        {
            out.push(make_finding(
                fallback_line,
                module_name,
                file,
                fd,
                format!(
                    "Vector.fromList over a recursive list builder ({}). \
                     Consider Vector.new(N, default) + Option.withDefault(Vector.set(v, i, x), v) \
                     when the target size is known — skips ~2N cons-cell allocations and the \
                     list-to-vector copy.",
                    name
                ),
            ));
        }

        // Map.fromList(<sink>(args, []))
        if is_dotted(&callee.node, "Map", "fromList")
            && args.len() == 1
            && let Some(name) = sink_call_with_empty_acc(&args[0].node, sinks)
        {
            out.push(make_finding(
                fallback_line,
                module_name,
                file,
                fd,
                format!(
                    "Map.fromList over a recursive list builder ({}). \
                     Consider threading a Map directly via {{}} + Map.set in the \
                     accumulator slot — skips the cons-cell intermediate and the fold.",
                    name
                ),
            ));
        }

        // Standalone List.reverse(<sink>(args, [])) — only flag when
        // NOT inside a `String.join(...)`. Inside, the external-reverse
        // fusion already handles it.
        if !inside_string_join
            && is_dotted(&callee.node, "List", "reverse")
            && args.len() == 1
            && let Some(name) = sink_call_with_empty_acc(&args[0].node, sinks)
        {
            out.push(make_finding(
                fallback_line,
                module_name,
                file,
                fd,
                format!(
                    "List.reverse over a recursive prepend-builder ({}) used as a list result. \
                     The reverse pass allocates a second N cons cells. If the result feeds \
                     `String.join`, the deforestation pass eliminates both this reverse and \
                     the intermediate list — nothing to do. Otherwise consider building the \
                     list in forward order directly (e.g. by computing positions).",
                    name
                ),
            ));
        }

        // Recurse into args. Track whether we're inside a String.join's
        // first arg so the standalone-reverse warning skips that case.
        let is_string_join = is_dotted(&callee.node, "String", "join") && args.len() == 2;
        for (i, arg) in args.iter().enumerate() {
            walk(
                &arg.node,
                line_of(arg, fallback_line),
                inside_string_join || (is_string_join && i == 0),
                fd,
                sinks,
                module_name,
                file,
                out,
            );
        }
        // Recurse through the callee too (e.g. partial-application
        // shapes; rare in Aver but harmless to cover).
        walk(
            &callee.node,
            line_of(callee, fallback_line),
            inside_string_join,
            fd,
            sinks,
            module_name,
            file,
            out,
        );
        return;
    }

    visit_subexprs(
        expr,
        fallback_line,
        inside_string_join,
        fd,
        sinks,
        module_name,
        file,
        out,
    );
}

#[allow(clippy::too_many_arguments)]
fn visit_subexprs(
    expr: &Expr,
    fallback_line: usize,
    inside_string_join: bool,
    fd: &FnDef,
    sinks: &HashMap<String, BufferBuildShape>,
    module_name: &Option<String>,
    file: Option<&str>,
    out: &mut Vec<CheckFinding>,
) {
    let mut recurse = |sub: &Spanned<Expr>| {
        walk(
            &sub.node,
            line_of(sub, fallback_line),
            inside_string_join,
            fd,
            sinks,
            module_name,
            file,
            out,
        );
    };
    match expr {
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::Resolved { .. }
        | Expr::Constructor(_, None)
        | Expr::InterpolatedStr(_) => {}
        Expr::Constructor(_, Some(inner)) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
            recurse(inner);
        }
        Expr::FnCall(_, _) => {
            // Handled by the FnCall arm in `walk` directly; not reached
            // through `visit_subexprs` because `walk` returns early.
        }
        Expr::TailCall(data) => {
            for a in data.args.iter() {
                recurse(a);
            }
        }
        Expr::BinOp(_, l, r) => {
            recurse(l);
            recurse(r);
        }
        Expr::Match { subject, arms } => {
            recurse(subject);
            for arm in arms.iter() {
                recurse(&arm.body);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items.iter() {
                recurse(it);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries.iter() {
                recurse(k);
                recurse(v);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields.iter() {
                recurse(v);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            recurse(base);
            for (_, v) in updates.iter() {
                recurse(v);
            }
        }
    }
}

/// Match `<sink>(args, ...)` where `args[acc_idx]` is a literal empty
/// `List`. Mirrors the precondition the buffer-build rewrite uses for
/// the corresponding fusion sites — a non-empty initial accumulator
/// would pre-load elements the warning's suggested rewrite couldn't
/// silently drop, so we skip in that case (the user's code may be
/// doing something deliberate we don't want to second-guess).
fn sink_call_with_empty_acc(
    expr: &Expr,
    sinks: &HashMap<String, BufferBuildShape>,
) -> Option<String> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    let Expr::Ident(name) = &callee.node else {
        return None;
    };
    let shape = sinks.get(name)?;
    let acc_arg = args.get(shape.acc_param_idx)?;
    if !matches!(&acc_arg.node, Expr::List(items) if items.is_empty()) {
        return None;
    }
    Some(name.clone())
}

fn is_dotted(expr: &Expr, module: &str, member: &str) -> bool {
    let Expr::Attr(base, attr) = expr else {
        return false;
    };
    if attr != member {
        return false;
    }
    matches!(&base.node, Expr::Ident(name) if name == module)
}

fn line_of(s: &Spanned<Expr>, fallback: usize) -> usize {
    if s.line > 0 { s.line } else { fallback }
}

fn make_finding(
    line: usize,
    module_name: &Option<String>,
    file: Option<&str>,
    fd: &FnDef,
    message: String,
) -> CheckFinding {
    let _ = Pattern::Wildcard; // touch the import so a future extension that
    // pattern-matches doesn't trip the unused-import lint
    CheckFinding {
        line,
        module: module_name.clone(),
        file: file.map(|s| s.to_string()),
        fn_name: Some(fd.name.clone()),
        message,
        extra_spans: vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::parse_source;
    use crate::tco;

    fn parse_and_lower(src: &str) -> Vec<TopLevel> {
        let mut items = parse_source(src)
            .unwrap_or_else(|e| panic!("parse error: {}\n--- source ---\n{}\n--- end ---", e, src));
        crate::ir::pipeline::tco(&mut items);
        items
    }

    const PURE_HEADER: &str =
        "module M\n    intent = \"t\"\n    exposes [main]\n    effects []\n\n";

    const CONSOLE_HEADER: &str =
        "module M\n    intent = \"t\"\n    exposes [main]\n    effects [Console]\n\n";

    #[test]
    fn warns_on_vector_fromlist_of_builder() {
        let body = "fn build(n: Int, acc: List<Int>) -> List<Int>\n    match n <= 0\n        true  -> List.reverse(acc)\n        false -> build(n - 1, List.prepend(n, acc))\n\nfn main() -> Vector<Int>\n    Vector.fromList(build(10, []))\n";
        let src = format!("{PURE_HEADER}{body}");
        let items = parse_and_lower(&src);
        let warnings = collect_traversal_warnings_in(&items, None);
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("Vector.fromList") && w.message.contains("build")),
            "expected Vector.fromList warning; got: {:?}",
            warnings.iter().map(|w| &w.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn warns_on_map_fromlist_of_builder() {
        let body = "fn build(n: Int, acc: List<Tuple<String, Int>>) -> List<Tuple<String, Int>>\n    match n <= 0\n        true  -> List.reverse(acc)\n        false -> build(n - 1, List.prepend((\"k\", n), acc))\n\nfn main() -> Map<String, Int>\n    Map.fromList(build(10, []))\n";
        let src = format!("{PURE_HEADER}{body}");
        let items = parse_and_lower(&src);
        let warnings = collect_traversal_warnings_in(&items, None);
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("Map.fromList") && w.message.contains("build")),
            "expected Map.fromList warning"
        );
    }

    #[test]
    fn warns_on_standalone_listreverse_of_builder() {
        let body = "fn build(xs: List<Int>, acc: List<Int>) -> List<Int>\n    match xs\n        []        -> acc\n        [h, ..t]  -> build(t, List.prepend(h * 2, acc))\n\nfn main() -> List<Int>\n    List.reverse(build([1, 2, 3], []))\n";
        let src = format!("{PURE_HEADER}{body}");
        let items = parse_and_lower(&src);
        let warnings = collect_traversal_warnings_in(&items, None);
        assert!(
            warnings.iter().any(
                |w| w.message.contains("List.reverse") && w.message.contains("prepend-builder")
            ),
            "expected standalone List.reverse warning"
        );
    }

    #[test]
    fn does_not_warn_when_listreverse_inside_string_join() {
        // The external-reverse fusion handles this — no warning needed.
        let body = "fn build(xs: List<Int>, acc: List<String>) -> List<String>\n    match xs\n        []        -> acc\n        [h, ..t]  -> build(t, List.prepend(\"x\", acc))\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(String.join(List.reverse(build([1, 2, 3], [])), \",\"))\n";
        let src = format!("{CONSOLE_HEADER}{body}");
        let items = parse_and_lower(&src);
        let warnings = collect_traversal_warnings_in(&items, None);
        assert!(
            warnings.iter().all(|w| !w.message.contains("List.reverse")),
            "should NOT warn — String.join wrapper means the fusion path takes over"
        );
    }

    #[test]
    fn does_not_warn_on_non_empty_initial_acc() {
        // The user is starting with a seeded accumulator, which the
        // suggested rewrite would silently lose. Don't second-guess.
        let body = "fn build(n: Int, acc: List<Int>) -> List<Int>\n    match n <= 0\n        true  -> List.reverse(acc)\n        false -> build(n - 1, List.prepend(n, acc))\n\nfn main() -> Vector<Int>\n    Vector.fromList(build(10, [99]))\n";
        let src = format!("{PURE_HEADER}{body}");
        let items = parse_and_lower(&src);
        let warnings = collect_traversal_warnings_in(&items, None);
        assert!(
            warnings.is_empty(),
            "non-empty initial acc should suppress the warning"
        );
    }
}
