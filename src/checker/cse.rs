use crate::ast::{BinOp, Expr, FnDef, MatchArm, Spanned, Stmt, TopLevel};

use super::CheckFinding;

const PURE_NAMESPACE_PREFIXES: &[&str] = &[
    "List.", "Vector.", "Map.", "String.", "Int.", "Float.", "Bool.", "Char.", "Byte.",
];

/// Collect warnings for repeated arithmetic subexpressions across match condition
/// and arm bodies. When a non-trivial arithmetic subtree (e.g. `zr * zr`) appears
/// in both the match subject and an arm body, LLVM cannot CSE it because they end
/// up in different basic blocks separated by a branch.
pub fn collect_cse_warnings(items: &[TopLevel]) -> Vec<CheckFinding> {
    let mut warnings = Vec::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            collect_cse_warnings_in_fn(fd, &mut warnings);
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

fn collect_cse_warnings_in_fn(fd: &FnDef, warnings: &mut Vec<CheckFinding>) {
    let before = warnings.len();
    for stmt in fd.body.stmts() {
        let spanned = match stmt {
            Stmt::Expr(e) => e,
            Stmt::Binding(_, _, e) => e,
        };
        collect_cse_warnings_in_spanned(spanned, warnings);
    }
    // Collect match-CSE warned subtree strings so we can skip them in body-wide check
    let match_warned: std::collections::HashSet<String> = warnings[before..]
        .iter()
        .map(|w| w.message.clone())
        .collect();
    check_fn_body_duplicates(fd, &match_warned, warnings);
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
// Body-wide duplicate detection
// ---------------------------------------------------------------------------

fn check_fn_body_duplicates(
    fd: &FnDef,
    match_warned_messages: &std::collections::HashSet<String>,
    warnings: &mut Vec<CheckFinding>,
) {
    let mut all_subtrees: Vec<&Spanned<Expr>> = Vec::new();
    for stmt in fd.body.stmts() {
        let spanned = match stmt {
            Stmt::Expr(e) => e,
            Stmt::Binding(_, _, e) => e,
        };
        collect_all_nontrivial_from_spanned(spanned, &mut all_subtrees);
    }

    // Count occurrences via structural equality (Expr::PartialEq ignores line).
    // Track first spanned for each unique subtree.
    let mut counts: Vec<(&Spanned<Expr>, usize)> = Vec::new();
    for subtree in &all_subtrees {
        if let Some(entry) = counts.iter_mut().find(|(e, _)| e.node == subtree.node) {
            entry.1 += 1;
        } else {
            counts.push((subtree, 1));
        }
    }

    for (subtree, count) in &counts {
        if *count >= 2 {
            let subtree_str = expr_to_short_str(&subtree.node);
            let msg = format!(
                "`{}` is computed {} times in this function — consider extracting to a binding",
                subtree_str, count
            );
            let already_warned = match_warned_messages
                .iter()
                .any(|m| m.contains(&subtree_str));
            if !already_warned {
                warnings.push(CheckFinding {
                    line: subtree.line,
                    module: None,
                    file: None,
                    fn_name: None,
                    message: msg,
                    extra_spans: vec![],
                });
            }
        }
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

/// Collect all nontrivial subtrees from a spanned expression, recursing into
/// all AST nodes (not just BinOp/FnCall).
fn collect_all_nontrivial_from_spanned<'a>(
    spanned: &'a Spanned<Expr>,
    out: &mut Vec<&'a Spanned<Expr>>,
) {
    collect_nontrivial_subtrees(spanned, out);
    match &spanned.node {
        Expr::Match { subject, arms } => {
            collect_all_nontrivial_from_spanned(subject, out);
            for arm in arms {
                collect_all_nontrivial_from_spanned(&arm.body, out);
            }
        }
        Expr::Constructor(_, Some(inner)) => {
            collect_all_nontrivial_from_spanned(inner, out);
        }
        Expr::ErrorProp(inner) => {
            collect_all_nontrivial_from_spanned(inner, out);
        }
        Expr::List(elements) => {
            for e in elements {
                collect_all_nontrivial_from_spanned(e, out);
            }
        }
        Expr::Tuple(items) | Expr::EffectTuple(items, _) => {
            for e in items {
                collect_all_nontrivial_from_spanned(e, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_all_nontrivial_from_spanned(e, out);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_all_nontrivial_from_spanned(e, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_all_nontrivial_from_spanned(base, out);
            for (_, e) in updates {
                collect_all_nontrivial_from_spanned(e, out);
            }
        }
        Expr::TailCall(boxed) => {
            let (_, args) = boxed.as_ref();
            for a in args {
                collect_all_nontrivial_from_spanned(a, out);
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
        Expr::Tuple(items) | Expr::EffectTuple(items, _) => {
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
            let (_, args) = boxed.as_ref();
            args.iter().any(|a| spanned_contains_subtree(a, needle))
        }
        Expr::Attr(obj, _) => spanned_contains_subtree(obj, needle),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) | Expr::MapLiteral(_) => false,
        Expr::Constructor(_, None) => false,
    }
}

fn expr_to_short_str(expr: &Expr) -> String {
    match expr {
        Expr::Literal(lit) => match lit {
            crate::ast::Literal::Int(i) => i.to_string(),
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

    fn spanned(node: Expr) -> Spanned<Expr> {
        Spanned {
            node,
            line: 1 as SourceLine,
        }
    }

    fn spanned_at(node: Expr, line: usize) -> Spanned<Expr> {
        Spanned {
            node,
            line: line as SourceLine,
        }
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
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(ident("i"))),
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
            body: std::rc::Rc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(
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
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(ident("x"))),
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
            body: std::rc::Rc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(
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
                },
                MatchArm {
                    pattern: crate::ast::Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(ident("x"))),
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
            body: std::rc::Rc::new(crate::ast::FnBody::Block(vec![Stmt::Expr(spanned(
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
            body: std::rc::Rc::new(crate::ast::FnBody::Block(vec![
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
            effects: vec![crate::ast::Spanned {
                node: "Console.print".to_string(),
                line: 0,
            }],
            desc: None,
            body: std::rc::Rc::new(crate::ast::FnBody::Block(vec![
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
            body: std::rc::Rc::new(crate::ast::FnBody::Block(vec![
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
}
