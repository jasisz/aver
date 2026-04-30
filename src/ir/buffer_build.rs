//! Buffer-build sink detection.
//!
//! Identifies user fns that match the canonical functional list-builder
//! shape consumed by `String.join`:
//!
//! ```aver
//! fn build(..., acc: List<T>) -> List<T>
//!     match <cond>
//!         true  -> List.reverse(acc)
//!         false -> build(..., List.prepend(<elem>, acc))
//! ```
//!
//! When such a fn is called from `String.join(build(..., []), sep)`, the
//! whole pipeline is semantically equivalent to a single buffer-write
//! loop — Wadler 1990 shortcut fusion / deforestation. This module is
//! Phase 1 of the deforestation work for 0.15 "Traversal": it detects
//! candidate fns. Lowering (rewriting matched fns + their `String.join`
//! call sites) lives in a separate pass.
//!
//! Detection is intentionally local — the analyzer looks only at the fn
//! body, not its call sites. A matched fn may or may not actually be
//! consumed by `String.join`; the lowering pass cross-references call
//! sites separately and only fuses when both ends of the pipeline agree.

use std::collections::HashMap;

use crate::ast::{Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Stmt};

/// Information about a fn that matches the buffer-build sink shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BufferBuildShape {
    /// 0-based index of the `acc: List<T>` parameter in the fn signature.
    /// Identifies which arg in tail-call positions threads the
    /// accumulator and which `Ident` in the `true` arm is the reversed
    /// return value.
    pub acc_param_idx: usize,
    /// The accumulator parameter's binding name (looked up in tail-call
    /// args and in the `List.reverse(<name>)` return).
    pub acc_param_name: String,
}

/// What the matched builder feeds into. Different consumers compile
/// to different buffer types and finalizers, but all share the same
/// underlying deforestation: skip the intermediate List, write
/// elements straight to the consumer's storage.
///
/// Phase 2 implements `StringJoin` only — the canonical case from the
/// fractal demo. Future variants land as separate phases:
/// `VectorFromList` (already half-fused via `Vector.set` owned-mutate
/// in 0.14.0; deforestation closes the cons-cell side), and `ListFold`
/// for stream-fusion-style consumer rewrites.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConsumerKind {
    /// `String.join(builder(...), sep)` — write each element + sep
    /// directly into a `Vec<u8>`-shaped buffer in linear memory.
    StringJoin,
}

/// One detected fusion site: a builder call whose result is consumed
/// by a known sink (currently just `String.join`). Lowering rewrites
/// the producer + consumer pair into a single buffer-write loop.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FusionSite {
    /// Name of the enclosing user fn that contains the call.
    pub enclosing_fn: String,
    /// Line of the consumer call.
    pub line: usize,
    /// The matched buffer-build fn being wrapped.
    pub sink_fn: String,
    /// What's consuming the builder's result.
    pub consumer: ConsumerKind,
}

/// Walk all fns in `fns`, return a map from fn name to detected shape
/// for fns that match the buffer-build sink pattern. Fns that don't
/// match are absent from the result.
pub fn compute_buffer_build_sinks(fns: &[&FnDef]) -> HashMap<String, BufferBuildShape> {
    let mut out = HashMap::new();
    for fd in fns {
        if let Some(shape) = match_buffer_build_shape(fd) {
            out.insert(fd.name.clone(), shape);
        }
    }
    out
}

/// Walk every expression in every fn body looking for fusion sites:
/// `String.join(matched_fn(...), sep)` calls where `matched_fn` is a
/// key in `sinks`. Returns one `FusionSite` per call. The lowering
/// pass rewrites each site to call a buffered variant of `matched_fn`
/// directly into a pre-allocated buffer.
pub fn find_fusion_sites(
    fns: &[&FnDef],
    sinks: &HashMap<String, BufferBuildShape>,
) -> Vec<FusionSite> {
    let mut out = Vec::new();
    for fd in fns {
        for stmt in fd.body.stmts() {
            match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                    walk_expr_for_fusion_sites(&expr.node, expr.line, &fd.name, sinks, &mut out);
                }
            }
        }
    }
    out
}

/// Recursively walk an expression tree, recording any fusion site we
/// find. The fallback `expr_line` is used when a sub-expression has no
/// own line info.
fn walk_expr_for_fusion_sites(
    expr: &Expr,
    expr_line: usize,
    enclosing_fn: &str,
    sinks: &HashMap<String, BufferBuildShape>,
    out: &mut Vec<FusionSite>,
) {
    if let Expr::FnCall(callee, args) = expr {
        // Is this `String.join(<inner>, _)`?
        if is_dotted_ident(&callee.node, "String", "join") && args.len() == 2 {
            // Is the first argument a call to one of the matched sinks?
            if let Expr::FnCall(inner_callee, _) = &args[0].node {
                if let Expr::Ident(inner_name) = &inner_callee.node {
                    if sinks.contains_key(inner_name) {
                        out.push(FusionSite {
                            enclosing_fn: enclosing_fn.to_string(),
                            line: expr_line,
                            sink_fn: inner_name.clone(),
                            consumer: ConsumerKind::StringJoin,
                        });
                    }
                }
            }
        }
    }
    // Recurse into all sub-expressions regardless of whether this node
    // matched (a fusion site can sit inside another fusion site's args
    // — rare but valid; we'd record both and let the lowering decide).
    visit_subexprs(expr, expr_line, enclosing_fn, sinks, out);
}

/// Helper: recurse into the sub-expressions of `expr`. Mirrors the
/// shape coverage of `expr_allocates` in `alloc_info.rs` so we don't
/// miss any node kind.
fn visit_subexprs(
    expr: &Expr,
    fallback_line: usize,
    enclosing_fn: &str,
    sinks: &HashMap<String, BufferBuildShape>,
    out: &mut Vec<FusionSite>,
) {
    let line_of = |s: &crate::ast::Spanned<Expr>| {
        if s.line > 0 {
            s.line
        } else {
            fallback_line
        }
    };
    match expr {
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::Resolved { .. }
        | Expr::Constructor(_, None) => {}
        Expr::Constructor(_, Some(inner)) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
            walk_expr_for_fusion_sites(&inner.node, line_of(inner), enclosing_fn, sinks, out);
        }
        Expr::FnCall(callee, args) => {
            walk_expr_for_fusion_sites(&callee.node, line_of(callee), enclosing_fn, sinks, out);
            for a in args {
                walk_expr_for_fusion_sites(&a.node, line_of(a), enclosing_fn, sinks, out);
            }
        }
        Expr::TailCall(data) => {
            for a in &data.args {
                walk_expr_for_fusion_sites(&a.node, line_of(a), enclosing_fn, sinks, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_expr_for_fusion_sites(&l.node, line_of(l), enclosing_fn, sinks, out);
            walk_expr_for_fusion_sites(&r.node, line_of(r), enclosing_fn, sinks, out);
        }
        Expr::Match { subject, arms } => {
            walk_expr_for_fusion_sites(
                &subject.node,
                line_of(subject),
                enclosing_fn,
                sinks,
                out,
            );
            for arm in arms {
                walk_expr_for_fusion_sites(
                    &arm.body.node,
                    line_of(&arm.body),
                    enclosing_fn,
                    sinks,
                    out,
                );
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items {
                walk_expr_for_fusion_sites(&it.node, line_of(it), enclosing_fn, sinks, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                walk_expr_for_fusion_sites(&k.node, line_of(k), enclosing_fn, sinks, out);
                walk_expr_for_fusion_sites(&v.node, line_of(v), enclosing_fn, sinks, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                walk_expr_for_fusion_sites(&v.node, line_of(v), enclosing_fn, sinks, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_expr_for_fusion_sites(&base.node, line_of(base), enclosing_fn, sinks, out);
            for (_, v) in updates {
                walk_expr_for_fusion_sites(&v.node, line_of(v), enclosing_fn, sinks, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    walk_expr_for_fusion_sites(
                        &inner.node,
                        line_of(inner),
                        enclosing_fn,
                        sinks,
                        out,
                    );
                }
            }
        }
    }
}

/// Pattern-match a single fn against the buffer-build shape.
fn match_buffer_build_shape(fd: &FnDef) -> Option<BufferBuildShape> {
    // The accumulator must be a parameter of type `List<...>`. The
    // params vector stores type strings, not parsed `Type` values, so we
    // match the textual form. Aver's surface syntax accepts both
    // `List<T>` and (rarely) `[T]`-like sugar; canonical form is
    // `List<T>`.
    let (acc_idx, acc_name) = fd
        .params
        .iter()
        .enumerate()
        .find(|(_, (_, ty))| is_list_type_str(ty))
        .map(|(i, (name, _))| (i, name.clone()))?;

    // Body must be a single expression statement holding the match.
    let match_expr = single_match_body(&fd.body)?;
    let (subject_expr, arms) = match match_expr {
        Expr::Match { subject, arms } => (subject, arms),
        _ => return None,
    };

    // Subject is some boolean condition; we don't constrain its shape,
    // only that the two arms cover `true` and `false`.
    let _ = subject_expr;
    let (true_body, false_body) = pair_bool_arms(arms)?;

    // True arm: `List.reverse(<acc>)`.
    if !is_list_reverse_of(true_body, &acc_name) {
        return None;
    }

    // False arm: tail-call to self with one arg being
    // `List.prepend(<anything>, <acc>)`.
    if !is_self_tail_with_prepend_acc(false_body, &fd.name, &acc_name) {
        return None;
    }

    Some(BufferBuildShape {
        acc_param_idx: acc_idx,
        acc_param_name: acc_name,
    })
}

/// True if a parameter type-string parses as `List<...>`.
fn is_list_type_str(ty: &str) -> bool {
    let t = ty.trim();
    t.starts_with("List<") && t.ends_with('>')
}

/// Extract the single match expression that forms a fn's entire body.
/// Returns `None` if the body is empty, has multiple statements, or its
/// single statement isn't a match expression.
fn single_match_body(body: &FnBody) -> Option<&Expr> {
    let stmts = body.stmts();
    if stmts.len() != 1 {
        return None;
    }
    match &stmts[0] {
        Stmt::Expr(spanned) => match &spanned.node {
            Expr::Match { .. } => Some(&spanned.node),
            _ => None,
        },
        Stmt::Binding(_, _, _) => None,
    }
}

/// If `arms` is exactly two arms with `Bool(true)` / `Bool(false)`
/// patterns, return `(true_body, false_body)` references. Order in
/// source doesn't matter — we sort by pattern.
fn pair_bool_arms(arms: &[MatchArm]) -> Option<(&Expr, &Expr)> {
    if arms.len() != 2 {
        return None;
    }
    let mut t = None;
    let mut f = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                if t.is_some() {
                    return None;
                }
                t = Some(&arm.body.node);
            }
            Pattern::Literal(Literal::Bool(false)) => {
                if f.is_some() {
                    return None;
                }
                f = Some(&arm.body.node);
            }
            _ => return None,
        }
    }
    Some((t?, f?))
}

/// True if `expr` is `List.reverse(<Ident(acc_name)>)`.
fn is_list_reverse_of(expr: &Expr, acc_name: &str) -> bool {
    let (callee, args) = match expr {
        Expr::FnCall(c, a) => (c, a),
        _ => return false,
    };
    if !is_dotted_ident(&callee.node, "List", "reverse") {
        return false;
    }
    if args.len() != 1 {
        return false;
    }
    matches!(&args[0].node, Expr::Ident(name) if name == acc_name)
}

/// True if `expr` is a tail-call to `self_name` whose argument list
/// contains `List.prepend(<anything>, <Ident(acc_name)>)` in any
/// position. The position should match the `acc_param_idx` but the
/// caller may have other params before it; we only require the
/// `prepend` to terminate in the expected accumulator binding.
fn is_self_tail_with_prepend_acc(expr: &Expr, self_name: &str, acc_name: &str) -> bool {
    let data = match expr {
        Expr::TailCall(data) => data,
        _ => return false,
    };
    if data.target != self_name {
        return false;
    }
    data.args
        .iter()
        .any(|arg| is_list_prepend_to_acc(&arg.node, acc_name))
}

/// True if `expr` is `List.prepend(<anything>, <Ident(acc_name)>)`.
fn is_list_prepend_to_acc(expr: &Expr, acc_name: &str) -> bool {
    let (callee, args) = match expr {
        Expr::FnCall(c, a) => (c, a),
        _ => return false,
    };
    if !is_dotted_ident(&callee.node, "List", "prepend") {
        return false;
    }
    if args.len() != 2 {
        return false;
    }
    matches!(&args[1].node, Expr::Ident(name) if name == acc_name)
}

/// True if `expr` is `<Module>.<Member>` access (the un-called callee
/// shape of `Module.member(...)`).
fn is_dotted_ident(expr: &Expr, module: &str, member: &str) -> bool {
    let (base, attr) = match expr {
        Expr::Attr(b, a) => (b, a),
        _ => return false,
    };
    if attr != member {
        return false;
    }
    matches!(&base.node, Expr::Ident(name) if name == module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, FnBody, FnDef, Literal, Spanned, TailCallData};
    use std::sync::Arc;

    fn sp<T>(value: T) -> Spanned<T> {
        Spanned {
            node: value,
            line: 1,
        }
    }

    fn ident(name: &str) -> Spanned<Expr> {
        sp(Expr::Ident(name.to_string()))
    }

    fn dotted(module: &str, member: &str) -> Spanned<Expr> {
        sp(Expr::Attr(
            Box::new(ident(module)),
            member.to_string(),
        ))
    }

    fn call(callee: Spanned<Expr>, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        sp(Expr::FnCall(Box::new(callee), args))
    }

    /// Build a canonical buffer-build fn: takes (col: Int, acc: List<Int>),
    /// matches col >= 10, true → reverse(acc), false → tail-call self
    /// with prepend(col, acc).
    fn canonical_builder(name: &str) -> FnDef {
        let true_body = call(dotted("List", "reverse"), vec![ident("acc")]);
        let prepend = call(
            dotted("List", "prepend"),
            vec![ident("col"), ident("acc")],
        );
        let false_body = sp(Expr::TailCall(Box::new(TailCallData {
            target: name.to_string(),
            args: vec![
                sp(Expr::BinOp(
                    BinOp::Add,
                    Box::new(ident("col")),
                    Box::new(sp(Expr::Literal(Literal::Int(1)))),
                )),
                prepend,
            ],
        })));
        let match_expr = sp(Expr::Match {
            subject: Box::new(sp(Expr::BinOp(
                BinOp::Gte,
                Box::new(ident("col")),
                Box::new(sp(Expr::Literal(Literal::Int(10)))),
            ))),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(true_body),
                },
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(false_body),
                },
            ],
        });
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![
                ("col".to_string(), "Int".to_string()),
                ("acc".to_string(), "List<Int>".to_string()),
            ],
            return_type: "List<Int>".to_string(),
            effects: vec![],
            desc: None,
            body: Arc::new(FnBody::Block(vec![Stmt::Expr(match_expr)])),
            resolution: None,
        }
    }

    #[test]
    fn matches_canonical_buffer_build() {
        let fd = canonical_builder("build");
        let info = compute_buffer_build_sinks(&[&fd]);
        let shape = info.get("build").expect("expected match");
        assert_eq!(shape.acc_param_idx, 1);
        assert_eq!(shape.acc_param_name, "acc");
    }

    #[test]
    fn rejects_fn_without_list_param() {
        let mut fd = canonical_builder("build");
        // Strip the List<...> param.
        fd.params = vec![("col".to_string(), "Int".to_string())];
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(info.is_empty(), "fn without List param should not match");
    }

    #[test]
    fn rejects_when_true_arm_isnt_reverse() {
        let mut fd = canonical_builder("build");
        // Replace true arm body with a different expression.
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    arms[0].body = Box::new(ident("acc"));
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "fn returning bare acc instead of reverse should not match"
        );
    }

    #[test]
    fn rejects_when_false_arm_uses_append_not_prepend() {
        let mut fd = canonical_builder("build");
        // Swap List.prepend → List.append in the false arm tail call.
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    let false_body = arms[1].body.as_mut();
                    if let Expr::TailCall(data) = &mut false_body.node {
                        if let Expr::FnCall(callee, _) = &mut data.args[1].node {
                            if let Expr::Attr(_, attr) = &mut callee.node {
                                *attr = "append".to_string();
                            }
                        }
                    }
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "fn using List.append instead of prepend should not match"
        );
    }

    #[test]
    fn rejects_tail_call_to_different_fn() {
        let mut fd = canonical_builder("build");
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    let false_body = arms[1].body.as_mut();
                    if let Expr::TailCall(data) = &mut false_body.node {
                        data.target = "someone_else".to_string();
                    }
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "fn whose recursive call targets a different name should not match"
        );
    }

    #[test]
    fn rejects_match_with_non_bool_arms() {
        let mut fd = canonical_builder("build");
        if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body) {
            if let Stmt::Expr(spanned) = &mut stmts[0] {
                if let Expr::Match { arms, .. } = &mut spanned.node {
                    arms[0].pattern = Pattern::Literal(Literal::Int(0));
                }
            }
        }
        let info = compute_buffer_build_sinks(&[&fd]);
        assert!(
            info.is_empty(),
            "match on non-bool patterns should not be detected as buffer-build"
        );
    }

    /// End-to-end: parse a small Aver source, run TCO, then detect.
    /// The TCO transform is what produces `Expr::TailCall` nodes from
    /// raw `Expr::FnCall` self-recursion; detection runs on the post-TCO
    /// AST.
    #[test]
    fn detects_via_parser_after_tco() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let info = compute_buffer_build_sinks(&fns);
        let shape = info
            .get("build")
            .expect("expected end-to-end shape match for canonical builder");
        assert_eq!(shape.acc_param_idx, 1);
        assert_eq!(shape.acc_param_name, "acc");
    }

    /// End-to-end fusion-site detection: builder + caller `String.join`
    /// site recognised, line recorded, sink name attached.
    #[test]
    fn finds_fusion_site_via_parser() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))

fn main() -> String
    String.join(build(5, []), ",")
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let sinks = compute_buffer_build_sinks(&fns);
        let sites = find_fusion_sites(&fns, &sinks);
        assert_eq!(sites.len(), 1, "expected one fusion site, got {sites:?}");
        let site = &sites[0];
        assert_eq!(site.enclosing_fn, "main");
        assert_eq!(site.sink_fn, "build");
        assert!(site.line > 0, "expected real line info, got 0");
    }

    /// Caller passes the matched fn's result to a non-`String.join`
    /// destination — should NOT register as a fusion site (no buffer
    /// to write into).
    #[test]
    fn ignores_call_when_not_wrapped_in_string_join() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    build(5, [])
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let sinks = compute_buffer_build_sinks(&fns);
        let sites = find_fusion_sites(&fns, &sinks);
        assert!(
            sites.is_empty(),
            "build called outside String.join must not be a fusion site, got {sites:?}"
        );
    }

    /// Counter-test: a recursive fn that returns `acc` directly (no
    /// reverse) — semantically valid Aver, but its result order is
    /// reversed relative to natural read order, so deforestation can't
    /// safely rewrite to a forward-emit buffer loop without explicit
    /// authorisation. Detector must reject it.
    #[test]
    fn rejects_via_parser_when_true_arm_returns_bare_acc() {
        let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> acc
        false -> build(n - 1, List.prepend(n, acc))
"#;
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        let mut items = parser.parse().expect("parse");
        crate::tco::transform_program(&mut items);
        let fns: Vec<&FnDef> = items
            .iter()
            .filter_map(|it| match it {
                crate::ast::TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let info = compute_buffer_build_sinks(&fns);
        assert!(
            info.is_empty(),
            "fn returning bare acc must not be detected as a deforestation candidate"
        );
    }

    #[test]
    fn detects_acc_param_at_arbitrary_index() {
        // Builder where the List<T> param is first, not last.
        let mut fd = canonical_builder("build");
        fd.params = vec![
            ("acc".to_string(), "List<Int>".to_string()),
            ("col".to_string(), "Int".to_string()),
        ];
        let info = compute_buffer_build_sinks(&[&fd]);
        let shape = info.get("build").expect("expected match");
        assert_eq!(shape.acc_param_idx, 0);
        assert_eq!(shape.acc_param_name, "acc");
    }
}
