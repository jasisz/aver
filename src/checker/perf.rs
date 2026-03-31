use std::collections::HashSet;

use crate::ast::{BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt, TopLevel};

use super::CheckFinding;

const PURE_PREFIXES: &[&str] = &[
    "List.", "Vector.", "Map.", "String.", "Int.", "Float.", "Bool.", "Char.", "Byte.",
];

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

pub fn collect_perf_warnings(items: &[TopLevel]) -> Vec<CheckFinding> {
    let mut warnings = Vec::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            check_list_len_comparison(fd, &mut warnings);
            // String concat in tail-call args (acc + c) is now O(1) amortized
            // in all backends (interpreter: in-place push_str, codegen: String append).
            // No longer flagged as a performance issue.
            check_nested_eq_match(fd, &mut warnings);
            check_loop_invariant(fd, &mut warnings);
        }
    }
    warnings
}

pub fn collect_perf_warnings_in(items: &[TopLevel], file: Option<&str>) -> Vec<CheckFinding> {
    let mut warnings = collect_perf_warnings(items);
    if let Some(f) = file {
        for w in &mut warnings {
            w.file = Some(f.to_string());
        }
    }
    warnings
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

/// Minimal pretty-printer for warning messages.
fn expr_to_short_str(expr: &Expr) -> String {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Str(s) => format!("\"{}\"", s),
            Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            Literal::Unit => "Unit".to_string(),
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

/// Check whether an expression mentions only identifiers from `allowed` and literals.
/// Namespace prefixes (e.g. `Int` in `Int.toFloat(...)`) are treated as constants,
/// not as variable references that need to be in `allowed`.
fn expr_uses_only(expr: &Expr, allowed: &HashSet<String>) -> bool {
    match expr {
        Expr::Ident(name) => allowed.contains(name),
        Expr::Literal(_) => true,
        Expr::BinOp(_, left, right) => {
            expr_uses_only(&left.node, allowed) && expr_uses_only(&right.node, allowed)
        }
        Expr::FnCall(callee, args) => {
            let callee_ok = match &callee.node {
                // Namespace call like `Int.toFloat(...)` — the namespace is a constant
                Expr::Attr(obj, _) => {
                    if let Expr::Ident(ns) = &obj.node {
                        // Check if this is a known pure namespace prefix
                        let prefix = format!("{}.", ns);
                        PURE_PREFIXES.iter().any(|p| *p == prefix)
                    } else {
                        expr_uses_only(&obj.node, allowed)
                    }
                }
                other => expr_uses_only(other, allowed),
            };
            callee_ok && args.iter().all(|a| expr_uses_only(&a.node, allowed))
        }
        Expr::Attr(obj, _) => expr_uses_only(&obj.node, allowed),
        _ => false,
    }
}

/// Is this expression trivial (a single Ident or single Literal)?
fn is_trivial(expr: &Expr) -> bool {
    matches!(expr, Expr::Ident(_) | Expr::Literal(_))
}

/// Is this a pure namespace function call?
fn is_pure_fncall(expr: &Expr) -> bool {
    if let Expr::FnCall(callee, _) = expr
        && let Some(name) = callee_dotted_name(&callee.node)
    {
        return PURE_PREFIXES.iter().any(|p| name.starts_with(p));
    }
    false
}

/// Is this a non-trivial expression worth warning about for loop invariance?
/// (BinOp arithmetic or pure namespace FnCall, but not a single ident/literal)
fn is_nontrivial_candidate(expr: &Expr) -> bool {
    if is_trivial(expr) {
        return false;
    }
    match expr {
        Expr::BinOp(op, _, _) => matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div),
        Expr::FnCall(_, _) => is_pure_fncall(expr),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Recursive expression walkers
// ---------------------------------------------------------------------------

fn walk_expr<F: FnMut(&Spanned<Expr>)>(spanned: &Spanned<Expr>, f: &mut F) {
    f(spanned);
    walk_expr_inner(&spanned.node, f);
}

fn walk_expr_inner<F: FnMut(&Spanned<Expr>)>(expr: &Expr, f: &mut F) {
    match expr {
        Expr::BinOp(_, left, right) => {
            walk_expr(left, f);
            walk_expr(right, f);
        }
        Expr::FnCall(callee, args) => {
            walk_expr(callee, f);
            for arg in args {
                walk_expr(arg, f);
            }
        }
        Expr::Match { subject, arms } => {
            walk_expr(subject, f);
            for arm in arms {
                walk_expr(&arm.body, f);
            }
        }
        Expr::Constructor(_, Some(inner)) => walk_expr(inner, f),
        Expr::ErrorProp(inner) => walk_expr(inner, f),
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    walk_expr(e, f);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                walk_expr(item, f);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                walk_expr(k, f);
                walk_expr(v, f);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                walk_expr(e, f);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_expr(base, f);
            for (_, e) in updates {
                walk_expr(e, f);
            }
        }
        Expr::TailCall(boxed) => {
            let (_, args) = boxed.as_ref();
            for arg in args {
                walk_expr(arg, f);
            }
        }
        Expr::Attr(obj, _) => walk_expr(obj, f),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) | Expr::Constructor(_, None) => {}
    }
}

fn walk_stmts<F: FnMut(&Spanned<Expr>)>(stmts: &[Stmt], f: &mut F) {
    for stmt in stmts {
        match stmt {
            Stmt::Expr(e) => walk_expr(e, f),
            Stmt::Binding(_, _, e) => walk_expr(e, f),
        }
    }
}

// ---------------------------------------------------------------------------
// B1: List.len(xs) == 0  →  use pattern match
// ---------------------------------------------------------------------------

fn check_list_len_comparison(fd: &FnDef, warnings: &mut Vec<CheckFinding>) {
    walk_stmts(fd.body.stmts(), &mut |spanned| {
        if is_list_len_comparison(&spanned.node) {
            warnings.push(CheckFinding {
                line: spanned.line,
                module: None,
                file: None,
                fn_name: Some(fd.name.clone()),
                message: "`List.len(xs) == 0` traverses the entire list — use `match xs: [] -> ...` instead".to_string(),
                extra_spans: vec![],
            });
        }
    });
}

/// Detect: BinOp(Eq, FnCall("List.len", [_]), Literal(Int(0|1))) or reversed,
///         BinOp(Gt, FnCall("List.len", [_]), Literal(Int(0))) or reversed.
fn is_list_len_comparison(expr: &Expr) -> bool {
    let Expr::BinOp(op, left, right) = expr else {
        return false;
    };

    match op {
        BinOp::Eq => {
            is_list_len_call(&left.node) && is_small_int_literal(&right.node)
                || is_list_len_call(&right.node) && is_small_int_literal(&left.node)
        }
        BinOp::Gt => {
            is_list_len_call(&left.node) && is_zero_literal(&right.node)
                || is_list_len_call(&right.node) && is_zero_literal(&left.node)
        }
        _ => false,
    }
}

fn is_list_len_call(expr: &Expr) -> bool {
    if let Expr::FnCall(callee, args) = expr
        && args.len() == 1
        && let Some(name) = callee_dotted_name(&callee.node)
    {
        return name == "List.len";
    }
    false
}

fn is_small_int_literal(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Literal::Int(0 | 1)))
}

fn is_zero_literal(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Literal::Int(0)))
}

// ---------------------------------------------------------------------------
// B3: Nested match x == literal  →  direct match
// ---------------------------------------------------------------------------

fn check_nested_eq_match(fd: &FnDef, warnings: &mut Vec<CheckFinding>) {
    walk_stmts(fd.body.stmts(), &mut |spanned| {
        if let Some(inner_line) = nested_eq_match_inner_line(&spanned.node) {
            warnings.push(CheckFinding {
                line: spanned.line,
                module: None,
                file: None,
                fn_name: Some(fd.name.clone()),
                message: "nested `match x == <literal>` — use direct `match x: 0 -> ..., 1 -> ..., _ -> ...`".to_string(),
                extra_spans: vec![super::FindingSpan {
                    line: inner_line,
                    col: 0,
                    len: 0,
                    label: String::new(),
                }],
            });
        }
    });
}

/// Detect: Match { subject: BinOp(Eq, expr, Literal(_)), arms } where the
/// false-arm body is another Match { subject: BinOp(Eq, same_expr, Literal(_)), ... }.
/// Returns the line number of the inner match if found.
fn nested_eq_match_inner_line(expr: &Expr) -> Option<usize> {
    let Expr::Match { subject, arms } = expr else {
        return None;
    };

    let compared_expr = extract_eq_comparison_target(&subject.node)?;

    // Find the false-arm (Pattern::Literal(Bool(false)) or Pattern::Wildcard)
    let false_arm = arms.iter().find(|arm| {
        matches!(
            &arm.pattern,
            Pattern::Literal(Literal::Bool(false)) | Pattern::Wildcard
        )
    })?;

    // Check if the inner expr is also a match on BinOp(Eq, same_expr, Literal(_))
    let Expr::Match {
        subject: inner_subject,
        ..
    } = &false_arm.body.node
    else {
        return None;
    };

    let inner_compared_expr = extract_eq_comparison_target(&inner_subject.node)?;

    // PartialEq on Spanned ignores lines, so structural equality
    if compared_expr == inner_compared_expr {
        Some(false_arm.body.line)
    } else {
        None
    }
}

/// For BinOp(Eq, expr, Literal(_)) or BinOp(Eq, Literal(_), expr), return expr.
fn extract_eq_comparison_target(expr: &Expr) -> Option<&Expr> {
    let Expr::BinOp(BinOp::Eq, left, right) = expr else {
        return None;
    };
    if matches!(&right.node, Expr::Literal(_)) {
        Some(&left.node)
    } else if matches!(&left.node, Expr::Literal(_)) {
        Some(&right.node)
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// B4: Loop-invariant computation in recursion
// ---------------------------------------------------------------------------

/// Collect all TailCall arg lists from the fn body.
fn collect_tailcall_args(stmts: &[Stmt]) -> Vec<&Vec<Spanned<Expr>>> {
    let mut result = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::Expr(e) => collect_tailcall_args_expr(&e.node, &mut result),
            Stmt::Binding(_, _, e) => collect_tailcall_args_expr(&e.node, &mut result),
        }
    }
    result
}

fn collect_tailcall_args_expr<'a>(expr: &'a Expr, out: &mut Vec<&'a Vec<Spanned<Expr>>>) {
    match expr {
        Expr::TailCall(boxed) => {
            let (_, args) = boxed.as_ref();
            out.push(args);
        }
        Expr::BinOp(_, left, right) => {
            collect_tailcall_args_expr(&left.node, out);
            collect_tailcall_args_expr(&right.node, out);
        }
        Expr::FnCall(callee, args) => {
            collect_tailcall_args_expr(&callee.node, out);
            for arg in args {
                collect_tailcall_args_expr(&arg.node, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_tailcall_args_expr(&subject.node, out);
            for arm in arms {
                collect_tailcall_args_expr(&arm.body.node, out);
            }
        }
        Expr::Constructor(_, Some(inner)) => collect_tailcall_args_expr(&inner.node, out),
        Expr::ErrorProp(inner) => collect_tailcall_args_expr(&inner.node, out),
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_tailcall_args_expr(&e.node, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_tailcall_args_expr(&item.node, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_tailcall_args_expr(&e.node, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_tailcall_args_expr(&base.node, out);
            for (_, e) in updates {
                collect_tailcall_args_expr(&e.node, out);
            }
        }
        Expr::Attr(obj, _) => collect_tailcall_args_expr(&obj.node, out),
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                collect_tailcall_args_expr(&k.node, out);
                collect_tailcall_args_expr(&v.node, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) | Expr::Constructor(_, None) => {}
    }
}

fn check_loop_invariant(fd: &FnDef, warnings: &mut Vec<CheckFinding>) {
    if fd.params.is_empty() {
        return;
    }

    // 1. Collect ALL TailCall arg lists from fn body
    let tailcall_args = collect_tailcall_args(fd.body.stmts());

    if tailcall_args.is_empty() {
        return;
    }

    // 2-3. For each TailCall, check which params are forwarded unchanged.
    // Invariant params = intersection across ALL TailCall sites.
    let param_names: Vec<&str> = fd.params.iter().map(|(name, _)| name.as_str()).collect();
    let mut invariant: Option<HashSet<String>> = None;

    for args in &tailcall_args {
        let mut forwarded = HashSet::new();
        for (i, param_name) in param_names.iter().enumerate() {
            if let Some(arg) = args.get(i)
                && let Expr::Ident(name) = &arg.node
                && name == *param_name
            {
                forwarded.insert(param_name.to_string());
            }
        }
        invariant = Some(match invariant {
            None => forwarded,
            Some(prev) => prev.intersection(&forwarded).cloned().collect(),
        });
    }

    let invariant = match invariant {
        Some(set) if !set.is_empty() => set,
        _ => return,
    };

    // 4-6. Collect non-trivial expressions in fn body bindings that use ONLY invariant params and literals.
    // We report each qualifying binding expression.
    let mut already_warned: HashSet<String> = HashSet::new();

    for stmt in fd.body.stmts() {
        if let Stmt::Binding(_, _, bind_expr) = stmt
            && is_nontrivial_candidate(&bind_expr.node)
            && expr_uses_only(&bind_expr.node, &invariant)
        {
            let short = expr_to_short_str(&bind_expr.node);
            if already_warned.contains(&short) {
                continue;
            }
            already_warned.insert(short.clone());

            // Find one invariant param name used in the expression for the message
            let used_param = first_ident_in(&bind_expr.node, &invariant)
                .unwrap_or_else(|| invariant.iter().next().unwrap().clone());

            warnings.push(CheckFinding {
                line: bind_expr.line,
                module: None,
                file: None,
                fn_name: Some(fd.name.clone()),
                message: format!(
                    "`{}` is recomputed every recursive call but `{}` doesn't change — extract to a binding before the recursion",
                    short, used_param
                ),
                extra_spans: vec![],
            });
        }
    }
}

/// Find the first Ident in expr that is in `set`.
fn first_ident_in(expr: &Expr, set: &HashSet<String>) -> Option<String> {
    match expr {
        Expr::Ident(name) if set.contains(name) => Some(name.clone()),
        Expr::BinOp(_, left, right) => {
            first_ident_in(&left.node, set).or_else(|| first_ident_in(&right.node, set))
        }
        Expr::FnCall(callee, args) => first_ident_in(&callee.node, set)
            .or_else(|| args.iter().find_map(|a| first_ident_in(&a.node, set))),
        Expr::Attr(obj, _) => first_ident_in(&obj.node, set),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FnBody, Literal, MatchArm, SourceLine, Spanned};

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

    fn ident(name: &str) -> Expr {
        Expr::Ident(name.to_string())
    }

    fn int(i: i64) -> Expr {
        Expr::Literal(Literal::Int(i))
    }

    fn bool_lit(b: bool) -> Expr {
        Expr::Literal(Literal::Bool(b))
    }

    fn binop(op: BinOp, left: Expr, right: Expr) -> Expr {
        Expr::BinOp(op, Box::new(spanned(left)), Box::new(spanned(right)))
    }

    fn list_len_call(arg: Expr) -> Expr {
        Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(Expr::Ident("List".to_string()))),
                "len".to_string(),
            ))),
            vec![spanned(arg)],
        )
    }

    fn make_fn(
        name: &str,
        params: Vec<(&str, &str)>,
        return_type: &str,
        stmts: Vec<Stmt>,
    ) -> FnDef {
        FnDef {
            name: name.to_string(),
            line: 1,
            params: params
                .into_iter()
                .map(|(n, t)| (n.to_string(), t.to_string()))
                .collect(),
            return_type: return_type.to_string(),
            effects: vec![],
            desc: None,
            body: std::rc::Rc::new(FnBody::Block(stmts)),
            resolution: None,
        }
    }

    // -----------------------------------------------------------------------
    // B1: List.len(xs) == 0
    // -----------------------------------------------------------------------

    #[test]
    fn b1_list_len_eq_zero_warns() {
        // List.len(xs) == 0
        let expr = binop(BinOp::Eq, list_len_call(ident("xs")), int(0));
        let fd = make_fn(
            "isEmpty",
            vec![("xs", "List<Int>")],
            "Bool",
            vec![Stmt::Expr(spanned(expr))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("List.len(xs) == 0"));
    }

    #[test]
    fn b1_list_len_eq_zero_reversed_warns() {
        // 0 == List.len(xs)
        let expr = binop(BinOp::Eq, int(0), list_len_call(ident("xs")));
        let fd = make_fn(
            "isEmpty",
            vec![("xs", "List<Int>")],
            "Bool",
            vec![Stmt::Expr(spanned(expr))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert_eq!(warnings.len(), 1);
    }

    #[test]
    fn b1_list_len_gt_zero_warns() {
        // List.len(xs) > 0
        let expr = binop(BinOp::Gt, list_len_call(ident("xs")), int(0));
        let fd = make_fn(
            "nonEmpty",
            vec![("xs", "List<Int>")],
            "Bool",
            vec![Stmt::Expr(spanned(expr))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert_eq!(warnings.len(), 1);
    }

    #[test]
    fn b1_list_len_eq_one_warns() {
        // List.len(xs) == 1
        let expr = binop(BinOp::Eq, list_len_call(ident("xs")), int(1));
        let fd = make_fn(
            "isSingleton",
            vec![("xs", "List<Int>")],
            "Bool",
            vec![Stmt::Expr(spanned(expr))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert_eq!(warnings.len(), 1);
    }

    #[test]
    fn b1_list_len_add_no_warning() {
        // List.len(xs) + 1  — not a comparison, no warning
        let expr = binop(BinOp::Add, list_len_call(ident("xs")), int(1));
        let fd = make_fn(
            "lenPlusOne",
            vec![("xs", "List<Int>")],
            "Int",
            vec![Stmt::Expr(spanned(expr))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings.is_empty(),
            "expected no warnings for List.len + 1, got {:?}",
            warnings
        );
    }

    #[test]
    fn b1_list_len_eq_large_no_warning() {
        // List.len(xs) == 42  — only 0 and 1 are flagged
        let expr = binop(BinOp::Eq, list_len_call(ident("xs")), int(42));
        let fd = make_fn(
            "has42",
            vec![("xs", "List<Int>")],
            "Bool",
            vec![Stmt::Expr(spanned(expr))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings.is_empty(),
            "expected no warning for == 42, got {:?}",
            warnings
        );
    }

    // -----------------------------------------------------------------------
    // B2: String concat in TailCall argument
    // -----------------------------------------------------------------------

    #[test]
    fn b2_string_concat_in_tailcall_warns() {
        // fn build(acc: String, n: Int) -> String
        //   ...
        //   TailCall("build", [acc + "x", n - 1])
        let tailcall = Expr::TailCall(Box::new((
            "build".to_string(),
            vec![
                spanned(binop(
                    BinOp::Add,
                    ident("acc"),
                    Expr::Literal(Literal::Str("x".to_string())),
                )),
                spanned(binop(BinOp::Sub, ident("n"), int(1))),
            ],
        )));
        let fd = make_fn(
            "build",
            vec![("acc", "String"), ("n", "Int")],
            "String",
            vec![Stmt::Expr(spanned(tailcall))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("string concatenation with `acc`")),
            "expected string concat warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn b2_non_string_param_no_warning() {
        // fn count(n: Int, m: Int) -> Int
        //   TailCall("count", [n + 1, m - 1])
        let tailcall = Expr::TailCall(Box::new((
            "count".to_string(),
            vec![
                spanned(binop(BinOp::Add, ident("n"), int(1))),
                spanned(binop(BinOp::Sub, ident("m"), int(1))),
            ],
        )));
        let fd = make_fn(
            "count",
            vec![("n", "Int"), ("m", "Int")],
            "Int",
            vec![Stmt::Expr(spanned(tailcall))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("string concatenation")),
            "expected no string concat warning, got {:?}",
            warnings
        );
    }

    // -----------------------------------------------------------------------
    // B3: Nested match x == literal
    // -----------------------------------------------------------------------

    fn eq_match(subject: Expr, true_body: Expr, false_body: Expr) -> Expr {
        Expr::Match {
            subject: Box::new(spanned(subject)),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(spanned(true_body)),
                },
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(false)),
                    body: Box::new(spanned(false_body)),
                },
            ],
        }
    }

    #[test]
    fn b3_nested_eq_match_warns() {
        // match x == 0:
        //   true -> "zero"
        //   false -> match x == 1:
        //     true -> "one"
        //     false -> "other"
        let inner = eq_match(
            binop(BinOp::Eq, ident("x"), int(1)),
            Expr::Literal(Literal::Str("one".to_string())),
            Expr::Literal(Literal::Str("other".to_string())),
        );
        let outer = eq_match(
            binop(BinOp::Eq, ident("x"), int(0)),
            Expr::Literal(Literal::Str("zero".to_string())),
            inner,
        );
        let fd = make_fn(
            "classify",
            vec![("x", "Int")],
            "String",
            vec![Stmt::Expr(spanned(outer))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("nested `match x == <literal>`")),
            "expected nested eq match warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn b3_nested_eq_match_reversed_warns() {
        // match 0 == x:
        //   true -> "zero"
        //   false -> match 1 == x:  (reversed comparison)
        //     true -> "one"
        //     false -> "other"
        let inner = eq_match(
            binop(BinOp::Eq, int(1), ident("x")),
            Expr::Literal(Literal::Str("one".to_string())),
            Expr::Literal(Literal::Str("other".to_string())),
        );
        let outer = eq_match(
            binop(BinOp::Eq, int(0), ident("x")),
            Expr::Literal(Literal::Str("zero".to_string())),
            inner,
        );
        let fd = make_fn(
            "classify",
            vec![("x", "Int")],
            "String",
            vec![Stmt::Expr(spanned(outer))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("nested `match x == <literal>`")),
            "expected nested eq match warning for reversed comparison, got {:?}",
            warnings
        );
    }

    #[test]
    fn b3_single_eq_match_no_warning() {
        // match x == 0:
        //   true -> "zero"
        //   false -> "other"  (no nested match)
        let outer = eq_match(
            binop(BinOp::Eq, ident("x"), int(0)),
            Expr::Literal(Literal::Str("zero".to_string())),
            Expr::Literal(Literal::Str("other".to_string())),
        );
        let fd = make_fn(
            "classify",
            vec![("x", "Int")],
            "String",
            vec![Stmt::Expr(spanned(outer))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("nested `match x == <literal>`")),
            "expected no warning for single eq match, got {:?}",
            warnings
        );
    }

    #[test]
    fn b3_nested_match_different_subjects_no_warning() {
        // match x == 0:
        //   true -> "zero"
        //   false -> match y == 1:   (different variable!)
        //     true -> "one"
        //     false -> "other"
        let inner = eq_match(
            binop(BinOp::Eq, ident("y"), int(1)),
            Expr::Literal(Literal::Str("one".to_string())),
            Expr::Literal(Literal::Str("other".to_string())),
        );
        let outer = eq_match(
            binop(BinOp::Eq, ident("x"), int(0)),
            Expr::Literal(Literal::Str("zero".to_string())),
            inner,
        );
        let fd = make_fn(
            "classify",
            vec![("x", "Int"), ("y", "Int")],
            "String",
            vec![Stmt::Expr(spanned(outer))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("nested `match x == <literal>`")),
            "expected no warning for different subjects, got {:?}",
            warnings
        );
    }

    #[test]
    fn b3_wildcard_false_arm_warns() {
        // match x == 0:
        //   true -> "zero"
        //   _ -> match x == 1:   (wildcard as false-arm)
        //     true -> "one"
        //     _ -> "other"
        let inner = Expr::Match {
            subject: Box::new(spanned(binop(BinOp::Eq, ident("x"), int(1)))),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(spanned(Expr::Literal(Literal::Str("one".to_string())))),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: Box::new(spanned(Expr::Literal(Literal::Str("other".to_string())))),
                },
            ],
        };
        let outer = Expr::Match {
            subject: Box::new(spanned(binop(BinOp::Eq, ident("x"), int(0)))),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Literal(Literal::Bool(true)),
                    body: Box::new(spanned(Expr::Literal(Literal::Str("zero".to_string())))),
                },
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: Box::new(spanned(inner)),
                },
            ],
        };
        let fd = make_fn(
            "classify",
            vec![("x", "Int")],
            "String",
            vec![Stmt::Expr(spanned(outer))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("nested `match x == <literal>`")),
            "expected nested eq match warning with wildcard false arm, got {:?}",
            warnings
        );
    }

    // -----------------------------------------------------------------------
    // B4: Loop-invariant computation in recursion
    // -----------------------------------------------------------------------

    #[test]
    fn b4_loop_invariant_warns() {
        // fn draw(x: Int, y: Int, width: Int) -> Int
        //   w = Int.toFloat(width)
        //   ...
        //   TailCall("draw", [x + 1, y, width])
        //
        // `width` is forwarded unchanged → `Int.toFloat(width)` is loop-invariant
        let int_to_float_call = Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(ident("Int"))),
                "toFloat".to_string(),
            ))),
            vec![spanned(ident("width"))],
        );
        let tailcall = Expr::TailCall(Box::new((
            "draw".to_string(),
            vec![
                spanned(binop(BinOp::Add, ident("x"), int(1))),
                spanned(ident("y")),
                spanned(ident("width")),
            ],
        )));
        let fd = make_fn(
            "draw",
            vec![("x", "Int"), ("y", "Int"), ("width", "Int")],
            "Int",
            vec![
                Stmt::Binding("w".to_string(), None, spanned(int_to_float_call)),
                Stmt::Expr(spanned(tailcall)),
            ],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings
                .iter()
                .any(|w| w.message.contains("Int.toFloat(width)")
                    && w.message.contains("doesn't change")),
            "expected loop-invariant warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn b4_changed_param_no_warning() {
        // fn draw(x: Int, width: Int) -> Int
        //   w = Int.toFloat(width)
        //   TailCall("draw", [x + 1, width - 1])  ← width is NOT forwarded unchanged
        let int_to_float_call = Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(ident("Int"))),
                "toFloat".to_string(),
            ))),
            vec![spanned(ident("width"))],
        );
        let tailcall = Expr::TailCall(Box::new((
            "draw".to_string(),
            vec![
                spanned(binop(BinOp::Add, ident("x"), int(1))),
                spanned(binop(BinOp::Sub, ident("width"), int(1))),
            ],
        )));
        let fd = make_fn(
            "draw",
            vec![("x", "Int"), ("width", "Int")],
            "Int",
            vec![
                Stmt::Binding("w".to_string(), None, spanned(int_to_float_call)),
                Stmt::Expr(spanned(tailcall)),
            ],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            !warnings.iter().any(|w| w.message.contains("Int.toFloat")),
            "expected no warning when width changes, got {:?}",
            warnings
        );
    }

    #[test]
    fn b4_trivial_binding_no_warning() {
        // fn f(x: Int, y: Int) -> Int
        //   z = y          ← trivial (single ident)
        //   TailCall("f", [x + 1, y])
        let tailcall = Expr::TailCall(Box::new((
            "f".to_string(),
            vec![
                spanned(binop(BinOp::Add, ident("x"), int(1))),
                spanned(ident("y")),
            ],
        )));
        let fd = make_fn(
            "f",
            vec![("x", "Int"), ("y", "Int")],
            "Int",
            vec![
                Stmt::Binding("z".to_string(), None, spanned(ident("y"))),
                Stmt::Expr(spanned(tailcall)),
            ],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings.is_empty(),
            "expected no warning for trivial binding, got {:?}",
            warnings
        );
    }

    #[test]
    fn b4_arithmetic_invariant_warns() {
        // fn f(x: Int, y: Int, z: Int) -> Int
        //   w = y * z      ← nontrivial arithmetic, both y and z are invariant
        //   TailCall("f", [x + 1, y, z])
        let arith = binop(BinOp::Mul, ident("y"), ident("z"));
        let tailcall = Expr::TailCall(Box::new((
            "f".to_string(),
            vec![
                spanned(binop(BinOp::Add, ident("x"), int(1))),
                spanned(ident("y")),
                spanned(ident("z")),
            ],
        )));
        let fd = make_fn(
            "f",
            vec![("x", "Int"), ("y", "Int"), ("z", "Int")],
            "Int",
            vec![
                Stmt::Binding("w".to_string(), None, spanned(arith)),
                Stmt::Expr(spanned(tailcall)),
            ],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings.iter().any(|w| w.message.contains("y * z")),
            "expected loop-invariant warning for y * z, got {:?}",
            warnings
        );
    }

    #[test]
    fn b4_no_tailcall_no_warning() {
        // fn f(x: Int, y: Int) -> Int
        //   w = Int.toFloat(y)
        //   w  (no TailCall → not a recursive fn in TCO sense)
        let int_to_float_call = Expr::FnCall(
            Box::new(spanned(Expr::Attr(
                Box::new(spanned(ident("Int"))),
                "toFloat".to_string(),
            ))),
            vec![spanned(ident("y"))],
        );
        let fd = make_fn(
            "f",
            vec![("x", "Int"), ("y", "Int")],
            "Int",
            vec![
                Stmt::Binding("w".to_string(), None, spanned(int_to_float_call)),
                Stmt::Expr(spanned(ident("w"))),
            ],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings(&items);
        assert!(
            warnings.is_empty(),
            "expected no warning without TailCall, got {:?}",
            warnings
        );
    }

    // -----------------------------------------------------------------------
    // collect_perf_warnings_in: file field propagation
    // -----------------------------------------------------------------------

    #[test]
    fn warnings_in_sets_file_field() {
        let expr = binop(BinOp::Eq, list_len_call(ident("xs")), int(0));
        let fd = make_fn(
            "isEmpty",
            vec![("xs", "List<Int>")],
            "Bool",
            vec![Stmt::Expr(spanned(expr))],
        );
        let items = vec![TopLevel::FnDef(fd)];
        let warnings = collect_perf_warnings_in(&items, Some("test.av"));
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].file, Some("test.av".to_string()));
    }
}
