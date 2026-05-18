use std::collections::BTreeSet;

use crate::ast::{Expr, FnDef, Spanned, Stmt, StrPart, TailCallData, TopLevel};

use super::{CheckFinding, FindingSpan, FnSigMap, dotted_name};

pub fn collect_independence_warnings(items: &[TopLevel], fn_sigs: &FnSigMap) -> Vec<CheckFinding> {
    let mut warnings = Vec::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            collect_independence_warnings_in_fn(fd, fn_sigs, &mut warnings);
        }
    }
    warnings
}

pub fn collect_independence_warnings_in(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
    file: Option<&str>,
) -> Vec<CheckFinding> {
    let mut warnings = collect_independence_warnings(items, fn_sigs);
    if let Some(file) = file {
        for warning in &mut warnings {
            warning.file = Some(file.to_string());
        }
    }
    warnings
}

fn collect_independence_warnings_in_fn(
    fd: &FnDef,
    fn_sigs: &FnSigMap,
    warnings: &mut Vec<CheckFinding>,
) {
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                collect_independence_warnings_in_expr(expr, fd, fn_sigs, warnings);
            }
        }
    }
}

fn collect_independence_warnings_in_expr(
    expr: &Spanned<Expr>,
    fd: &FnDef,
    fn_sigs: &FnSigMap,
    warnings: &mut Vec<CheckFinding>,
) {
    if let Expr::IndependentProduct(items, _) = &expr.node {
        check_independent_product(expr, items, fd, fn_sigs, warnings);
    }

    match &expr.node {
        Expr::FnCall(callee, args) => {
            collect_independence_warnings_in_expr(callee, fd, fn_sigs, warnings);
            for arg in args {
                collect_independence_warnings_in_expr(arg, fd, fn_sigs, warnings);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: _, args, ..
            } = boxed.as_ref();
            for arg in args {
                collect_independence_warnings_in_expr(arg, fd, fn_sigs, warnings);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_independence_warnings_in_expr(left, fd, fn_sigs, warnings);
            collect_independence_warnings_in_expr(right, fd, fn_sigs, warnings);
        }
        Expr::Neg(inner) => {
            collect_independence_warnings_in_expr(inner, fd, fn_sigs, warnings);
        }
        Expr::Match { subject, arms, .. } => {
            collect_independence_warnings_in_expr(subject, fd, fn_sigs, warnings);
            for arm in arms {
                collect_independence_warnings_in_expr(&arm.body, fd, fn_sigs, warnings);
            }
        }
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            collect_independence_warnings_in_expr(inner, fd, fn_sigs, warnings);
        }
        Expr::Constructor(_, Some(inner)) => {
            collect_independence_warnings_in_expr(inner, fd, fn_sigs, warnings);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_independence_warnings_in_expr(inner, fd, fn_sigs, warnings);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_independence_warnings_in_expr(item, fd, fn_sigs, warnings);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_independence_warnings_in_expr(key, fd, fn_sigs, warnings);
                collect_independence_warnings_in_expr(value, fd, fn_sigs, warnings);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_independence_warnings_in_expr(value, fd, fn_sigs, warnings);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_independence_warnings_in_expr(base, fd, fn_sigs, warnings);
            for (_, value) in updates {
                collect_independence_warnings_in_expr(value, fd, fn_sigs, warnings);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

fn check_independent_product(
    product: &Spanned<Expr>,
    items: &[Spanned<Expr>],
    fd: &FnDef,
    fn_sigs: &FnSigMap,
    warnings: &mut Vec<CheckFinding>,
) {
    if items.len() < 2 {
        return;
    }

    let branch_effects: Vec<BTreeSet<String>> = items
        .iter()
        .map(|item| collect_used_effects(item, fn_sigs))
        .collect();

    for left_idx in 0..items.len() {
        for right_idx in left_idx + 1..items.len() {
            let mut conflicting = BTreeSet::new();
            let mut hazard_kinds = BTreeSet::new();
            for left in &branch_effects[left_idx] {
                for right in &branch_effects[right_idx] {
                    if let Some(kind) = effect_hazard(left, right) {
                        conflicting.insert(left.clone());
                        conflicting.insert(right.clone());
                        hazard_kinds.insert(kind.label());
                    }
                }
            }

            if conflicting.is_empty() {
                continue;
            }

            let left_branch = left_idx + 1;
            let right_branch = right_idx + 1;
            let shared = conflicting.into_iter().collect::<Vec<_>>().join(", ");
            let hazard_note = hazard_kinds.into_iter().collect::<Vec<_>>().join(", ");
            let left_effects = describe_effects(&branch_effects[left_idx]);
            let right_effects = describe_effects(&branch_effects[right_idx]);
            warnings.push(CheckFinding {
                line: product.line,
                module: None,
                file: None,
                fn_name: Some(fd.name.clone()),
                message: format!(
                    "Independent product branches {} and {} use potentially conflicting effects [{}] ({}) — independent products may reorder or overlap these effects; keep them sequential or suppress with [[check.suppress]] reason if this independence is intentional",
                    left_branch, right_branch, shared, hazard_note
                ),
                extra_spans: vec![
                    FindingSpan {
                        line: items[left_idx].line,
                        col: 0,
                        len: 0,
                        label: format!("branch {} uses {}", left_branch, left_effects),
                    },
                    FindingSpan {
                        line: items[right_idx].line,
                        col: 0,
                        len: 0,
                        label: format!("branch {} uses {}", right_branch, right_effects),
                    },
                ],
            });
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum HazardKind {
    SharedOutput,
    SharedTransport,
    SharedServerLifecycle,
    DiskMutation,
    HttpMutation,
    EnvMutation,
}

impl HazardKind {
    fn label(self) -> &'static str {
        match self {
            HazardKind::SharedOutput => "shared terminal/output hazard",
            HazardKind::SharedTransport => "shared tcp/socket hazard",
            HazardKind::SharedServerLifecycle => "shared server lifecycle hazard",
            HazardKind::DiskMutation => "disk mutation hazard",
            HazardKind::HttpMutation => "http mutation hazard",
            HazardKind::EnvMutation => "environment mutation hazard",
        }
    }
}

fn describe_effects(effects: &BTreeSet<String>) -> String {
    if effects.is_empty() {
        "no effects".to_string()
    } else {
        effects.iter().cloned().collect::<Vec<_>>().join(", ")
    }
}

fn collect_used_effects(expr: &Spanned<Expr>, fn_sigs: &FnSigMap) -> BTreeSet<String> {
    let mut used = BTreeSet::new();
    collect_used_effects_expr(expr, fn_sigs, &mut used);
    used
}

fn collect_used_effects_expr(expr: &Spanned<Expr>, fn_sigs: &FnSigMap, out: &mut BTreeSet<String>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(callee_name) = dotted_name(callee)
                && let Some((_, _, effects)) = fn_sigs.get(&callee_name)
            {
                for effect in effects {
                    out.insert(effect.clone());
                }
            }
            collect_used_effects_expr(callee, fn_sigs, out);
            for arg in args {
                collect_used_effects_expr(arg, fn_sigs, out);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if let Some((_, _, effects)) = fn_sigs.get(target) {
                for effect in effects {
                    out.insert(effect.clone());
                }
            }
            for arg in args {
                collect_used_effects_expr(arg, fn_sigs, out);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_used_effects_expr(left, fn_sigs, out);
            collect_used_effects_expr(right, fn_sigs, out);
        }
        Expr::Neg(inner) => collect_used_effects_expr(inner, fn_sigs, out),
        Expr::Match { subject, arms, .. } => {
            collect_used_effects_expr(subject, fn_sigs, out);
            for arm in arms {
                collect_used_effects_expr(&arm.body, fn_sigs, out);
            }
        }
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            collect_used_effects_expr(inner, fn_sigs, out);
        }
        Expr::Constructor(_, Some(inner)) => collect_used_effects_expr(inner, fn_sigs, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_used_effects_expr(inner, fn_sigs, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_used_effects_expr(item, fn_sigs, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_used_effects_expr(key, fn_sigs, out);
                collect_used_effects_expr(value, fn_sigs, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_used_effects_expr(value, fn_sigs, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_used_effects_expr(base, fn_sigs, out);
            for (_, value) in updates {
                collect_used_effects_expr(value, fn_sigs, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

fn effect_hazard(left: &str, right: &str) -> Option<HazardKind> {
    if shared_output_effect(left, right) {
        Some(HazardKind::SharedOutput)
    } else if same_namespace_any(left, right, "Tcp") {
        Some(HazardKind::SharedTransport)
    } else if same_namespace_any(left, right, "HttpServer") {
        Some(HazardKind::SharedServerLifecycle)
    } else if same_namespace_with_mutation(left, right, "Disk", is_disk_mutating) {
        Some(HazardKind::DiskMutation)
    } else if same_namespace_with_mutation(left, right, "Http", is_http_mutating) {
        Some(HazardKind::HttpMutation)
    } else if same_namespace_with_mutation(left, right, "Env", is_env_mutating) {
        Some(HazardKind::EnvMutation)
    } else {
        None
    }
}

fn same_namespace_any(left: &str, right: &str, namespace: &str) -> bool {
    belongs_to_namespace(left, namespace) && belongs_to_namespace(right, namespace)
}

fn shared_output_effect(left: &str, right: &str) -> bool {
    belongs_to_output_namespace(left) && belongs_to_output_namespace(right)
}

fn same_namespace_with_mutation(
    left: &str,
    right: &str,
    namespace: &str,
    is_mutating: fn(&str) -> bool,
) -> bool {
    belongs_to_namespace(left, namespace)
        && belongs_to_namespace(right, namespace)
        && (is_mutating(left) || is_mutating(right))
}

fn belongs_to_namespace(effect: &str, namespace: &str) -> bool {
    effect == namespace || effect.starts_with(&format!("{namespace}."))
}

fn belongs_to_output_namespace(effect: &str) -> bool {
    belongs_to_namespace(effect, "Console") || belongs_to_namespace(effect, "Terminal")
}

fn is_disk_mutating(effect: &str) -> bool {
    [
        "Disk.writeText",
        "Disk.appendText",
        "Disk.delete",
        "Disk.deleteDir",
        "Disk.makeDir",
    ]
    .iter()
    .any(|required| crate::effects::effect_satisfies(effect, required))
}

fn is_http_mutating(effect: &str) -> bool {
    ["Http.post", "Http.put", "Http.patch", "Http.delete"]
        .iter()
        .any(|required| crate::effects::effect_satisfies(effect, required))
}

fn is_env_mutating(effect: &str) -> bool {
    crate::effects::effect_satisfies(effect, "Env.set")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse_items(src: &str) -> Vec<TopLevel> {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect("parse failed")
    }

    #[test]
    fn warns_on_console_effects_in_independent_product() {
        let items = parse_items(
            r#"
fn left() -> Unit
    ! [Console.print]
    Console.print("a")

fn right() -> Unit
    ! [Console.error]
    Console.error("b")

fn demo() -> Tuple<Unit, Unit>
    ! [Console.print, Console.error]
    (left(), right())!
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors.is_empty(),
            "unexpected type errors: {:?}",
            tc.errors
        );

        let warnings = collect_independence_warnings(&items, &tc.fn_sigs);
        assert_eq!(warnings.len(), 1, "warnings={warnings:?}");
        assert!(
            warnings[0]
                .message
                .contains("Independent product branches 1 and 2")
        );
        assert!(warnings[0].message.contains("Console.error"));
        assert!(warnings[0].message.contains("Console.print"));
        assert_eq!(warnings[0].fn_name.as_deref(), Some("demo"));
    }

    #[test]
    fn no_warning_for_independent_read_only_effects() {
        let items = parse_items(
            r#"
fn left() -> Bool
    ! [Disk.exists]
    Disk.exists("settings.json")

fn right() -> Option<String>
    ! [Env.get]
    Env.get("USER")

fn demo() -> Tuple<Bool, Option<String>>
    ! [Disk.exists, Env.get]
    (left(), right())!
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors.is_empty(),
            "unexpected type errors: {:?}",
            tc.errors
        );

        let warnings = collect_independence_warnings(&items, &tc.fn_sigs);
        assert!(
            warnings.is_empty(),
            "did not expect independence warning, got {warnings:?}"
        );
    }

    #[test]
    fn broad_namespace_effects_still_warn() {
        let items = parse_items(
            r#"
fn left() -> Unit
    ! [Console]
    Console.print("a")

fn right() -> Unit
    ! [Console.warn]
    Console.warn("b")

fn demo() -> Tuple<Unit, Unit>
    ! [Console]
    (left(), right())!
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors.is_empty(),
            "unexpected type errors: {:?}",
            tc.errors
        );

        let warnings = collect_independence_warnings(&items, &tc.fn_sigs);
        assert_eq!(warnings.len(), 1, "warnings={warnings:?}");
        assert!(warnings[0].message.contains("Console"));
    }

    #[test]
    fn warns_on_console_terminal_cross_namespace_hazard() {
        let items = parse_items(
            r#"
fn left() -> Unit
    ! [Console.print]
    Console.print("a")

fn right() -> Unit
    ! [Terminal.flush]
    Terminal.flush()

fn demo() -> Tuple<Unit, Unit>
    ! [Console.print, Terminal.flush]
    (left(), right())!
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors.is_empty(),
            "unexpected type errors: {:?}",
            tc.errors
        );

        let warnings = collect_independence_warnings(&items, &tc.fn_sigs);
        assert_eq!(warnings.len(), 1, "warnings={warnings:?}");
        assert!(warnings[0].message.contains("Console.print"));
        assert!(warnings[0].message.contains("Terminal.flush"));
        assert!(
            warnings[0]
                .message
                .contains("shared terminal/output hazard")
        );
    }
}
