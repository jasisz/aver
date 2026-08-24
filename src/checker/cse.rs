use std::collections::HashSet;

use crate::ast::{BinOp, Expr, FnDef, MatchArm, Spanned, Stmt, TailCallData, TopLevel};

use super::CheckFinding;

mod body_duplicates;

use body_duplicates::check_fn_body_duplicates;

const PURE_NAMESPACE_PREFIXES: &[&str] = &[
    "List.", "Vector.", "Map.", "String.", "Int.", "Float.", "Bool.",
];

/// Collect warnings for repeated arithmetic subexpressions across match condition
/// and arm bodies. When a non-trivial arithmetic subtree (e.g. `zr * zr`) appears
/// in both the match subject and an arm body, LLVM cannot CSE it because they end
/// up in different basic blocks separated by a branch.
pub fn collect_cse_warnings(items: &[TopLevel]) -> Vec<CheckFinding> {
    let mut warnings = Vec::new();
    // A user fn is pure iff it declares no effects — the type checker
    // propagates effects, so an empty `! [...]` is sound proof of purity.
    // Builtins are handled by the body-wide check below; user fns need this
    // set because their purity is only knowable here.
    let pure_fns: HashSet<String> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) if fd.effects.is_empty() => Some(fd.name.clone()),
            _ => None,
        })
        .collect();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            collect_cse_warnings_in_fn(fd, &pure_fns, &mut warnings);
        }
    }
    warnings
}

pub fn collect_cse_warnings_in(items: &[TopLevel], file: Option<&str>) -> Vec<CheckFinding> {
    let mut warnings = collect_cse_warnings(items);
    if let Some(f) = file {
        for w in &mut warnings {
            w.file = Some(f.to_string());
        }
    }
    warnings
}

fn collect_cse_warnings_in_fn(
    fd: &FnDef,
    pure_fns: &HashSet<String>,
    warnings: &mut Vec<CheckFinding>,
) {
    let before = warnings.len();
    for stmt in fd.body.stmts() {
        let spanned = match stmt {
            Stmt::Expr(e) => e,
            Stmt::Binding(_, _, e) => e,
        };
        collect_cse_warnings_in_spanned(spanned, warnings);
    }
    // Collect match-CSE warned subtree strings so we can skip them in body-wide check
    let match_warned: HashSet<String> = warnings[before..]
        .iter()
        .map(|w| w.message.clone())
        .collect();
    check_fn_body_duplicates(fd, &match_warned, warnings);
    check_pure_user_call_dups(fd, pure_fns, warnings);
    // Stamp fn_name on all newly added warnings
    for w in &mut warnings[before..] {
        w.fn_name = Some(fd.name.clone());
    }
}

// ---------------------------------------------------------------------------
// Match-CSE: subject subtree repeated in arm body
// ---------------------------------------------------------------------------

fn collect_cse_warnings_in_spanned(spanned: &Spanned<Expr>, warnings: &mut Vec<CheckFinding>) {
    match &spanned.node {
        Expr::Match { subject, arms } => {
            check_match_cse(subject, arms, warnings);
            for arm in arms {
                collect_cse_warnings_in_spanned(&arm.body, warnings);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_cse_warnings_in_spanned(left, warnings);
            collect_cse_warnings_in_spanned(right, warnings);
        }
        Expr::FnCall(callee, args) => {
            collect_cse_warnings_in_spanned(callee, warnings);
            for arg in args {
                collect_cse_warnings_in_spanned(arg, warnings);
            }
        }
        Expr::Constructor(_, Some(inner)) => {
            collect_cse_warnings_in_spanned(inner, warnings);
        }
        Expr::ErrorProp(inner) => {
            collect_cse_warnings_in_spanned(inner, warnings);
        }
        _ => {}
    }
}

fn check_match_cse(subject: &Spanned<Expr>, arms: &[MatchArm], warnings: &mut Vec<CheckFinding>) {
    let mut subject_subtrees: Vec<&Spanned<Expr>> = Vec::new();
    collect_nontrivial_subtrees(subject, &mut subject_subtrees);

    if subject_subtrees.is_empty() {
        return;
    }

    for subtree in &subject_subtrees {
        for arm in arms {
            if spanned_contains_subtree(&arm.body, &subtree.node) {
                let subtree_str = expr_to_short_str(&subtree.node);
                warnings.push(CheckFinding {
                    line: subtree.line,
                    module: None,
                    file: None,
                    fn_name: None,
                    message: format!(
                        "`{}` is computed in both the match condition and an arm body — consider extracting to a binding before the match",
                        subtree_str
                    ),
                    extra_spans: vec![],
                });
                break;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Pure user-function call duplicates (within one branch-free expression)
// ---------------------------------------------------------------------------

/// Flag a pure *user-function* call computed more than once within a single
/// branch-free expression. Builtin pure calls are handled body-wide above;
/// user fns are not, because (a) their purity is only knowable here and
/// (b) unlike builtins they may not terminate — hoisting one across a branch
/// could change termination. Restricting to a `match`/`?`-free expression
/// keeps it sound: every subexpression there is always evaluated (Aver is
/// eager, no short-circuit), so deduplicating a repeated pure call is
/// semantically identical, divergence included.
fn check_pure_user_call_dups(
    fd: &FnDef,
    pure_fns: &HashSet<String>,
    warnings: &mut Vec<CheckFinding>,
) {
    for stmt in fd.body.stmts() {
        let spanned = match stmt {
            Stmt::Expr(e) => e,
            Stmt::Binding(_, _, e) => e,
        };
        // Soundness gate: skip any expression containing a `match` or `?`,
        // where an occurrence could sit on a conditional path.
        if expr_has_branch(&spanned.node) {
            continue;
        }
        let mut calls: Vec<&Spanned<Expr>> = Vec::new();
        collect_pure_user_calls(spanned, pure_fns, &mut calls);

        let mut counts: Vec<(&Spanned<Expr>, usize)> = Vec::new();
        for c in &calls {
            if let Some(entry) = counts.iter_mut().find(|(e, _)| e.node == c.node) {
                entry.1 += 1;
            } else {
                counts.push((c, 1));
            }
        }
        for (call, count) in &counts {
            if *count >= 2 {
                let s = expr_to_short_str(&call.node);
                warnings.push(CheckFinding {
                    line: call.line,
                    module: None,
                    file: None,
                    fn_name: None,
                    message: format!(
                        "`{}` is computed {} times in this function — consider extracting to a binding",
                        s, count
                    ),
                    extra_spans: vec![],
                });
            }
        }
    }
}

/// Does `expr` contain a `match` or error-propagation `?` anywhere? Those are
/// the only constructs that put a subexpression on a conditional path.
fn expr_has_branch(expr: &Expr) -> bool {
    match expr {
        Expr::Match { .. } | Expr::ErrorProp(_) => true,
        Expr::BinOp(_, l, r) => expr_has_branch(&l.node) || expr_has_branch(&r.node),
        Expr::Neg(i) => expr_has_branch(&i.node),
        Expr::FnCall(callee, args) => {
            expr_has_branch(&callee.node) || args.iter().any(|a| expr_has_branch(&a.node))
        }
        Expr::Attr(o, _) => expr_has_branch(&o.node),
        Expr::Constructor(_, Some(i)) => expr_has_branch(&i.node),
        Expr::List(it) | Expr::Tuple(it) | Expr::IndependentProduct(it, _) => {
            it.iter().any(|e| expr_has_branch(&e.node))
        }
        Expr::MapLiteral(p) => p
            .iter()
            .any(|(k, v)| expr_has_branch(&k.node) || expr_has_branch(&v.node)),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(e) => expr_has_branch(&e.node),
            _ => false,
        }),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_has_branch(&e.node)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_has_branch(&base.node) || updates.iter().any(|(_, e)| expr_has_branch(&e.node))
        }
        Expr::TailCall(boxed) => {
            let TailCallData { args, .. } = boxed.as_ref();
            args.iter().any(|a| expr_has_branch(&a.node))
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            false
        }
    }
}

/// Collect *maximal* pure user-function calls (a counted call is not descended
/// into — the repeat is on the whole call). The caller guarantees the tree is
/// `match`/`?`-free, so every node here is on the always-evaluated path.
fn collect_pure_user_calls<'a>(
    spanned: &'a Spanned<Expr>,
    pure_fns: &HashSet<String>,
    out: &mut Vec<&'a Spanned<Expr>>,
) {
    if is_pure_user_call(&spanned.node, pure_fns) {
        out.push(spanned);
        return;
    }
    match &spanned.node {
        Expr::BinOp(_, l, r) => {
            collect_pure_user_calls(l, pure_fns, out);
            collect_pure_user_calls(r, pure_fns, out);
        }
        Expr::Neg(i) => collect_pure_user_calls(i, pure_fns, out),
        Expr::FnCall(callee, args) => {
            collect_pure_user_calls(callee, pure_fns, out);
            for a in args {
                collect_pure_user_calls(a, pure_fns, out);
            }
        }
        Expr::Attr(o, _) => collect_pure_user_calls(o, pure_fns, out),
        Expr::Constructor(_, Some(i)) => collect_pure_user_calls(i, pure_fns, out),
        Expr::List(it) | Expr::Tuple(it) | Expr::IndependentProduct(it, _) => {
            for e in it {
                collect_pure_user_calls(e, pure_fns, out);
            }
        }
        Expr::MapLiteral(p) => {
            for (k, v) in p {
                collect_pure_user_calls(k, pure_fns, out);
                collect_pure_user_calls(v, pure_fns, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_pure_user_calls(e, pure_fns, out);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_pure_user_calls(e, pure_fns, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_pure_user_calls(base, pure_fns, out);
            for (_, e) in updates {
                collect_pure_user_calls(e, pure_fns, out);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData { args, .. } = boxed.as_ref();
            for a in args {
                collect_pure_user_calls(a, pure_fns, out);
            }
        }
        _ => {}
    }
}

/// A pure user-function call: a bare-ident callee that declares no effects,
/// every argument also side-effect-free (so the whole call is pure).
fn is_pure_user_call(expr: &Expr, pure_fns: &HashSet<String>) -> bool {
    if let Expr::FnCall(callee, args) = expr
        && let Expr::Ident(name) = &callee.node
    {
        return pure_fns.contains(name)
            && args.iter().all(|a| is_side_effect_free(&a.node, pure_fns));
    }
    false
}

/// Side-effect-free expression (no effectful call hiding in it).
fn is_side_effect_free(expr: &Expr, pure_fns: &HashSet<String>) -> bool {
    match expr {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) => true,
        Expr::BinOp(_, l, r) => {
            is_side_effect_free(&l.node, pure_fns) && is_side_effect_free(&r.node, pure_fns)
        }
        Expr::Neg(i) => is_side_effect_free(&i.node, pure_fns),
        Expr::FnCall(callee, args) => {
            cse_callee_is_pure(&callee.node, pure_fns)
                && args.iter().all(|a| is_side_effect_free(&a.node, pure_fns))
        }
        Expr::Attr(o, _) => is_side_effect_free(&o.node, pure_fns),
        Expr::Constructor(_, Some(i)) => is_side_effect_free(&i.node, pure_fns),
        Expr::Tuple(it) | Expr::List(it) => {
            it.iter().all(|e| is_side_effect_free(&e.node, pure_fns))
        }
        _ => false,
    }
}

/// Is `callee` a pure target — a pure builtin namespace or an effect-free user fn?
fn cse_callee_is_pure(callee: &Expr, pure_fns: &HashSet<String>) -> bool {
    match callee {
        Expr::Attr(obj, _) => {
            if let Expr::Ident(ns) = &obj.node {
                PURE_NAMESPACE_PREFIXES
                    .iter()
                    .any(|p| *p == format!("{}.", ns))
            } else {
                false
            }
        }
        Expr::Ident(name) => pure_fns.contains(name),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Subtree collection — always collects &Spanned<Expr>
// ---------------------------------------------------------------------------

/// Collect nontrivial arithmetic / pure-FnCall subtrees from a spanned expression.
fn collect_nontrivial_subtrees<'a>(spanned: &'a Spanned<Expr>, out: &mut Vec<&'a Spanned<Expr>>) {
    match &spanned.node {
        Expr::BinOp(op, left, right) => {
            if is_arithmetic_op(op) && is_nontrivial_arithmetic(&spanned.node) {
                out.push(spanned);
            }
            collect_nontrivial_subtrees(left, out);
            collect_nontrivial_subtrees(right, out);
        }
        Expr::FnCall(callee, args) => {
            if is_nontrivial_pure_fncall(&spanned.node) {
                out.push(spanned);
            }
            collect_nontrivial_subtrees(callee, out);
            for arg in args {
                collect_nontrivial_subtrees(arg, out);
            }
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn is_nontrivial_arithmetic(expr: &Expr) -> bool {
    match expr {
        Expr::BinOp(op, left, right) => {
            if !is_arithmetic_op(op) {
                return false;
            }
            let both_trivial = is_trivial_operand(&left.node) && is_trivial_operand(&right.node);
            !both_trivial || matches!((&left.node, &right.node), (Expr::Ident(_), Expr::Ident(_)))
        }
        _ => false,
    }
}

fn is_trivial_operand(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(_))
}

fn is_arithmetic_op(op: &BinOp) -> bool {
    matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div)
}

fn callee_dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Attr(obj, field) => {
            if let Expr::Ident(ns) = &obj.node {
                Some(format!("{}.{}", ns, field))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn is_nontrivial_pure_fncall(expr: &Expr) -> bool {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Some(name) = callee_dotted_name(&callee.node) {
                let is_pure = PURE_NAMESPACE_PREFIXES
                    .iter()
                    .any(|prefix| name.starts_with(prefix));
                let has_non_literal = args.iter().any(|a| !matches!(&a.node, Expr::Literal(_)));
                // Don't suggest extraction if any arg contains a user fn call —
                // hoisting would eagerly evaluate the call even when the branch
                // doesn't need it (Aver is eager, not lazy).
                let args_have_user_call = args.iter().any(|a| expr_contains_user_call(&a.node));
                is_pure && has_non_literal && !args_have_user_call
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if an expression contains a call to a user-defined function
/// (i.e. not a pure namespace builtin like List.len, String.join, etc.).
fn expr_contains_user_call(expr: &Expr) -> bool {
    match expr {
        Expr::FnCall(callee, args) => {
            let is_namespace = callee_dotted_name(&callee.node).is_some_and(|name| {
                PURE_NAMESPACE_PREFIXES
                    .iter()
                    .any(|prefix| name.starts_with(prefix))
            });
            if !is_namespace {
                return true;
            }
            args.iter().any(|a| expr_contains_user_call(&a.node))
        }
        Expr::BinOp(_, left, right) => {
            expr_contains_user_call(&left.node) || expr_contains_user_call(&right.node)
        }
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => expr_contains_user_call(&inner.node),
        _ => false,
    }
}

/// Check if a spanned expression tree contains a structurally equal subtree.
fn spanned_contains_subtree(haystack: &Spanned<Expr>, needle: &Expr) -> bool {
    if haystack.node == *needle {
        return true;
    }
    match &haystack.node {
        Expr::BinOp(_, left, right) => {
            spanned_contains_subtree(left, needle) || spanned_contains_subtree(right, needle)
        }
        Expr::Neg(inner) => spanned_contains_subtree(inner, needle),
        Expr::FnCall(callee, args) => {
            spanned_contains_subtree(callee, needle)
                || args.iter().any(|a| spanned_contains_subtree(a, needle))
        }
        Expr::Match { subject, arms } => {
            spanned_contains_subtree(subject, needle)
                || arms
                    .iter()
                    .any(|arm| spanned_contains_subtree(&arm.body, needle))
        }
        Expr::Constructor(_, Some(inner)) => spanned_contains_subtree(inner, needle),
        Expr::ErrorProp(inner) => spanned_contains_subtree(inner, needle),
        Expr::List(elements) => elements.iter().any(|e| spanned_contains_subtree(e, needle)),
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|e| spanned_contains_subtree(e, needle))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(e) => spanned_contains_subtree(e, needle),
            _ => false,
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, e)| spanned_contains_subtree(e, needle)),
        Expr::RecordUpdate { base, updates, .. } => {
            spanned_contains_subtree(base, needle)
                || updates
                    .iter()
                    .any(|(_, e)| spanned_contains_subtree(e, needle))
        }
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: _, args, ..
            } = boxed.as_ref();
            args.iter().any(|a| spanned_contains_subtree(a, needle))
        }
        Expr::Attr(obj, _) => spanned_contains_subtree(obj, needle),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::MapLiteral(_) => false,
        Expr::Constructor(_, None) => false,
    }
}

fn expr_to_short_str(expr: &Expr) -> String {
    match expr {
        Expr::Literal(lit) => match lit {
            crate::ast::Literal::Int(i) => i.to_string(),
            crate::ast::Literal::BigInt(s) => s.clone(),
            crate::ast::Literal::Float(f) => f.to_string(),
            crate::ast::Literal::Str(s) => format!("\"{}\"", s),
            crate::ast::Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            crate::ast::Literal::Unit => "Unit".to_string(),
        },
        Expr::Ident(name) => name.clone(),
        Expr::BinOp(op, left, right) => {
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            format!(
                "{} {} {}",
                expr_to_short_str(&left.node),
                op_str,
                expr_to_short_str(&right.node)
            )
        }
        Expr::FnCall(callee, args) => {
            let args_str: Vec<String> = args.iter().map(|a| expr_to_short_str(&a.node)).collect();
            format!(
                "{}({})",
                expr_to_short_str(&callee.node),
                args_str.join(", ")
            )
        }
        Expr::Attr(obj, field) => format!("{}.{}", expr_to_short_str(&obj.node), field),
        _ => "...".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Literal, SourceLine, Spanned};
    use crate::source::parse_source;

    fn spanned(node: Expr) -> Spanned<Expr> {
        Spanned::new(node, 1 as SourceLine)
    }

    fn spanned_at(node: Expr, line: usize) -> Spanned<Expr> {
        Spanned::new(node, line as SourceLine)
    }

    fn int(i: i64) -> Expr {
        Expr::Literal(Literal::Int(i))
    }

    fn ident(name: &str) -> Expr {
        Expr::Ident(name.to_string())
    }

    fn binop(op: BinOp, left: Expr, right: Expr) -> Expr {
        Expr::BinOp(op, Box::new(spanned(left)), Box::new(spanned(right)))
    }

    fn namespace_call(namespace: &str, method: &str, arg: Expr) -> Expr {
        Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(ident(namespace))),
                method.to_string(),
            ))),
            vec![spanned(arg)],
        )
    }

    #[test]
    fn detects_cse_via_top_level_collection() {
        let zr_sq = binop(BinOp::Mul, ident("zr"), ident("zr"));
        let zi_sq = binop(BinOp::Mul, ident("zi"), ident("zi"));
        let sum = binop(BinOp::Add, zr_sq.clone(), zi_sq.clone());
        let subject = binop(BinOp::Lt, sum, Expr::Literal(Literal::Float(4.0)));

        let body = binop(BinOp::Sub, zr_sq, zi_sq);

        let match_expr = Expr::Match {
            subject: Box::new(spanned(subject)),
            arms: vec![
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(spanned(body)),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(ident("i"))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        };

        let fd = FnDef {
            name: "step".to_string(),
            line: 1,
            params: vec![("zr".to_string(), "Float".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(
                match_expr,
            ))])),
            resolution: None,
        };

        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_cse_warnings(&items);
        assert!(
            warnings.len() >= 2,
            "expected warnings for zr * zr and zi * zi, got {:?}",
            warnings
        );
        assert!(warnings.iter().any(|w| w.message.contains("zr * zr")));
        assert!(warnings.iter().any(|w| w.message.contains("zi * zi")));
    }

    #[test]
    fn no_warning_when_no_repeated_subtree() {
        let subject = binop(BinOp::Lt, ident("x"), int(10));
        let body = binop(BinOp::Add, ident("x"), int(1));

        let match_expr = Expr::Match {
            subject: Box::new(spanned(subject)),
            arms: vec![
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(spanned(body)),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(ident("x"))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        };

        let fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(
                match_expr,
            ))])),
            resolution: None,
        };

        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_cse_warnings(&items);
        assert!(
            warnings.is_empty(),
            "expected no warnings, got {:?}",
            warnings
        );
    }

    #[test]
    fn no_warning_for_single_ident_repeated() {
        let subject = binop(BinOp::Eq, ident("x"), int(0));

        let match_expr = Expr::Match {
            subject: Box::new(spanned(subject)),
            arms: vec![
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(spanned(ident("x"))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(ident("x"))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        };

        let fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(
                match_expr,
            ))])),
            resolution: None,
        };

        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_cse_warnings(&items);
        assert!(
            warnings.is_empty(),
            "expected no warnings for trivial ident, got {:?}",
            warnings
        );
    }

    #[test]
    fn detects_repeated_pure_fncall_in_body() {
        let len_call = Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(Expr::Ident("String".to_string()))),
                "len".to_string(),
            ))),
            vec![spanned(ident("s"))],
        );

        let fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("s".to_string(), "String".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![
                Stmt::Binding("a".to_string(), None, spanned(len_call.clone())),
                Stmt::Expr(spanned(len_call)),
            ])),
            resolution: None,
        };

        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_cse_warnings(&items);
        assert!(
            !warnings.is_empty(),
            "expected warning for repeated String.len(s)"
        );
        assert!(warnings.iter().any(|w| w.message.contains("String.len")));
    }

    #[test]
    fn no_warning_for_builtin_call_in_mutually_exclusive_match_arms() {
        let source = r#"type Bag
    Memory(Map<String, Int>)
    Logged(String, Map<String, Int>)

fn size(bag: Bag) -> Int
    match bag
        Bag.Memory(entries) -> Map.len(entries)
        Bag.Logged(path, entries) -> Map.len(entries)
"#;
        let items = parse_source(source).expect("parse #991 regression");

        let warnings = collect_cse_warnings(&items);

        assert!(
            warnings.is_empty(),
            "mutually exclusive match arms do not duplicate work: {warnings:?}"
        );
    }

    #[test]
    fn warns_for_builtin_call_repeated_within_one_match_arm() {
        let len_call = namespace_call("Map", "len", ident("entries"));
        let match_expr = Expr::Match {
            subject: Box::new(spanned(ident("bag"))),
            arms: vec![
                MatchArm {
                    pattern: crate::ast::Pattern::Ident("memory".to_string()),
                    body: Box::new(spanned(binop(BinOp::Add, len_call.clone(), len_call))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Wildcard,
                    body: Box::new(spanned(int(0))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        };
        let fd = caller_fn("size", vec![], match_expr);

        let warnings = collect_cse_warnings(&[TopLevel::FnDef(fd)]);

        assert!(
            warnings.iter().any(|warning| warning
                .message
                .contains("`Map.len(entries)` is computed 2 times")),
            "a duplicate inside one reachable arm must still warn: {warnings:?}"
        );
    }

    #[test]
    fn no_warning_for_effectful_call() {
        let print_call = Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(Expr::Ident("Console".to_string()))),
                "print".to_string(),
            ))),
            vec![spanned(ident("x"))],
        );

        let fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "String".to_string())],
            return_type: "Unit".to_string(),
            effects: vec![crate::ast::Spanned::new("Console.print".to_string(), 0)],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![
                Stmt::Expr(spanned(print_call.clone())),
                Stmt::Expr(spanned(print_call)),
            ])),
            resolution: None,
        };

        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_cse_warnings(&items);
        assert!(
            warnings.is_empty(),
            "should not warn about effectful calls, got {:?}",
            warnings
        );
    }

    #[test]
    fn duplicate_reports_first_occurrence_line() {
        let len_call = Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(Expr::Ident("List".to_string()))),
                "len".to_string(),
            ))),
            vec![spanned(ident("xs"))],
        );

        let fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("xs".to_string(), "List<Int>".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![
                Stmt::Binding("a".to_string(), None, spanned_at(len_call.clone(), 10)),
                Stmt::Expr(spanned_at(len_call, 20)),
            ])),
            resolution: None,
        };

        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_cse_warnings(&items);
        assert!(!warnings.is_empty());
        // Should report line 10 (first occurrence), not fn line 1
        assert_eq!(warnings[0].line, 10);
    }

    // ---- pure user-function call duplicates ----

    fn pure_int_fn(name: &str) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(binop(
                BinOp::Mul,
                ident("x"),
                ident("x"),
            )))])),
            resolution: None,
        }
    }

    fn caller_fn(name: &str, effects: Vec<Spanned<String>>, body: Expr) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects,
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(body))])),
            resolution: None,
        }
    }

    fn user_call(name: &str) -> Expr {
        Expr::FnCall(Box::new(spanned(ident(name))), vec![spanned(ident("x"))])
    }

    #[test]
    fn detects_repeated_pure_user_call() {
        // `cost(x) + cost(x)` with `cost` a pure user fn -> warn.
        let body = binop(BinOp::Add, user_call("cost"), user_call("cost"));
        let items = vec![
            TopLevel::FnDef(pure_int_fn("cost")),
            TopLevel::FnDef(caller_fn("f", vec![], body)),
        ];
        let warnings = collect_cse_warnings(&items);
        assert!(
            warnings.iter().any(|w| w.message.contains("cost(x)")),
            "expected warning for repeated pure user call, got {:?}",
            warnings
        );
    }

    #[test]
    fn no_warning_pure_user_call_in_separate_match_arms() {
        // Same call in two arms — only one runs, so hoisting would eager-eval
        // it on the other path. Must NOT warn (branch gate).
        let match_expr = Expr::Match {
            subject: Box::new(spanned(binop(BinOp::Gt, ident("x"), int(0)))),
            arms: vec![
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(spanned(user_call("cost"))),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(user_call("cost"))),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        };
        let items = vec![
            TopLevel::FnDef(pure_int_fn("cost")),
            TopLevel::FnDef(caller_fn("f", vec![], match_expr)),
        ];
        let warnings = collect_cse_warnings(&items);
        assert!(
            warnings.is_empty(),
            "must NOT warn across match arms, got {:?}",
            warnings
        );
    }

    #[test]
    fn no_warning_effectful_user_call() {
        // `loud` declares an effect -> not pure -> calling it twice is
        // intentional, never flagged.
        let loud = FnDef {
            name: "loud".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![Spanned::new("Console.print".to_string(), 0)],
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(ident(
                "x",
            )))])),
            resolution: None,
        };
        let body = binop(BinOp::Add, user_call("loud"), user_call("loud"));
        let items = vec![
            TopLevel::FnDef(loud),
            TopLevel::FnDef(caller_fn(
                "f",
                vec![Spanned::new("Console.print".to_string(), 0)],
                body,
            )),
        ];
        let warnings = collect_cse_warnings(&items);
        assert!(
            warnings.is_empty(),
            "must NOT flag effectful user calls, got {:?}",
            warnings
        );
    }
}
