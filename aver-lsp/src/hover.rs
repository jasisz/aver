use tower_lsp_server::ls_types::{Hover, HoverContents, MarkupContent, MarkupKind};

use aver::ast::{
    DecisionBlock, DecisionImpact, Expr, FnBody, FnDef, Stmt, TopLevel, TypeDef, VerifyKind,
};
use aver::call_graph::{find_recursive_fns, recursive_callsite_counts};
use aver::checker::{index_decisions, merge_verify_blocks};
use aver::tco;
use aver::types::Type;
use aver::types::checker::run_type_check_full;

use crate::completion;
use crate::modules;
use crate::position::utf16_col_to_byte_idx;

/// Try to produce hover info for a word at the cursor position.
pub fn hover_for_word(word: &str, source: &str, base_dir: Option<&str>) -> Option<Hover> {
    let items = completion::parse_items(source);

    // Check if it's a qualified name like "List.map" or "Weather.fetch"
    if let Some(last_dot) = word.rfind('.') {
        let prefix = &word[..last_dot];
        let member = &word[last_dot + 1..];

        // Built-in namespaces
        let completions = completion::namespace_completions(prefix);
        if let Some(item) = completions.iter().find(|c| c.label == member) {
            let detail = item.detail.as_deref().unwrap_or("");
            let content = format!("```aver\n{}.{}: {}\n```", prefix, member, detail);
            return Some(make_hover(content));
        }

        // Cross-module functions
        if let Some(base) = base_dir {
            let deps = modules::resolve_dependencies(source, base);
            for dep in &deps {
                let dep_short = dep.name.rsplit('.').next().unwrap_or(&dep.name);
                if dep.name == prefix || dep_short == prefix {
                    let dep_items = completion::parse_items(&dep.source);
                    for fd in modules::exported_fns(dep) {
                        if fd.name == member {
                            let content = build_fn_hover(fd, &dep_items, Some(base));
                            return Some(make_hover(format!(
                                "{}\n\n*from module {}*",
                                content, dep.name
                            )));
                        }
                    }
                }
            }
        }

        // User-defined types
        for item in &items {
            if let TopLevel::TypeDef(td) = item {
                match td {
                    TypeDef::Sum { name, variants, .. } if name == prefix => {
                        if let Some(v) = variants.iter().find(|v| v.name == member) {
                            let detail = if v.fields.is_empty() {
                                name.clone()
                            } else {
                                format!("fn({}) -> {}", v.fields.join(", "), name)
                            };
                            let content = format!("```aver\n{}.{}: {}\n```", name, member, detail);
                            return Some(make_hover(content));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Decisions
    for item in &items {
        if let TopLevel::Decision(decision) = item
            && decision.name == word
        {
            return Some(make_hover(build_decision_hover(decision)));
        }
    }

    // Namespace names
    let namespaces = completion::all_namespaces();
    if let Some(ns) = namespaces.iter().find(|c| c.label == word) {
        let detail = ns.detail.as_deref().unwrap_or("");
        let content = format!("**{}** — {}", word, detail);
        return Some(make_hover(content));
    }

    // User-defined functions
    for item in &items {
        if let TopLevel::FnDef(fd) = item
            && fd.name == word
        {
            let content = build_fn_hover(fd, &items, base_dir);
            return Some(make_hover(content));
        }
    }

    // User-defined types
    for item in &items {
        if let TopLevel::TypeDef(td) = item {
            match td {
                TypeDef::Sum { name, variants, .. } if name == word => {
                    let variant_strs: Vec<String> = variants
                        .iter()
                        .map(|v| {
                            if v.fields.is_empty() {
                                format!("  {}", v.name)
                            } else {
                                format!("  {}({})", v.name, v.fields.join(", "))
                            }
                        })
                        .collect();
                    let content =
                        format!("```aver\ntype {}\n{}\n```", name, variant_strs.join("\n"));
                    return Some(make_hover(content));
                }
                TypeDef::Product { name, fields, .. } if name == word => {
                    let field_strs: Vec<String> = fields
                        .iter()
                        .map(|(fname, ftype)| format!("  {}: {}", fname, ftype))
                        .collect();
                    let content =
                        format!("```aver\nrecord {}\n{}\n```", name, field_strs.join("\n"));
                    return Some(make_hover(content));
                }
                _ => {}
            }
        }
    }

    // Variable bindings
    for item in &items {
        match item {
            TopLevel::Stmt(Stmt::Binding(name, type_ann, _)) if name == word => {
                let ty = type_ann.as_deref().unwrap_or("(inferred)");
                let content = format!("```aver\n{}: {}\n```", name, ty);
                return Some(make_hover(content));
            }
            TopLevel::FnDef(fd) => {
                let FnBody::Block(stmts) = fd.body.as_ref();
                for stmt in stmts {
                    if let Stmt::Binding(name, type_ann, _) = stmt
                        && name == word
                    {
                        let ty = type_ann.as_deref().unwrap_or("(inferred)");
                        let content = format!("```aver\n{}: {}\n```\n*in {}()*", name, ty, fd.name);
                        return Some(make_hover(content));
                    }
                }
            }
            _ => {}
        }
    }

    None
}

fn build_fn_hover(fd: &FnDef, items: &[TopLevel], base_dir: Option<&str>) -> String {
    let mut parts = Vec::new();

    parts.push(format!("```aver\n{}\n```", format_signature(fd)));
    if let Some(desc) = &fd.desc {
        parts.push(format!("*{}*", desc));
    }

    let summaries = [
        verify_summary(fd, items),
        api_role_summary(fd, items),
        analysis_summary(fd, items, base_dir),
        decision_summary(fd, items),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>();

    if !summaries.is_empty() {
        parts.push(summaries.join(" | "));
    }

    parts.join("\n")
}

fn verify_summary(fd: &FnDef, items: &[TopLevel]) -> Option<String> {
    let verifies: Vec<_> = merge_verify_blocks(items)
        .into_iter()
        .filter(|vb| vb.fn_name == fd.name)
        .collect();
    if verifies.is_empty() {
        return None;
    }

    let case_count: usize = verifies
        .iter()
        .filter(|vb| matches!(vb.kind, VerifyKind::Cases))
        .map(|vb| vb.cases.len())
        .sum();
    let laws: Vec<String> = verifies
        .iter()
        .filter_map(|vb| match &vb.kind {
            VerifyKind::Law(law) => Some(law.name.clone()),
            VerifyKind::Cases => None,
        })
        .collect();

    let mut details = Vec::new();
    if case_count > 0 {
        let label = if case_count == 1 { "case" } else { "cases" };
        details.push(format!("{} {}", case_count, label));
    }
    if !laws.is_empty() {
        let label = if laws.len() == 1 { "law" } else { "laws" };
        details.push(format!("{} `{}`", label, laws.join("`, `")));
    }
    Some(format!("verify {}", details.join(", ")))
}

fn api_role_summary(fd: &FnDef, items: &[TopLevel]) -> Option<String> {
    let exposed = items.iter().find_map(|item| {
        if let TopLevel::Module(module) = item {
            let mut set: std::collections::HashSet<&str> =
                module.exposes.iter().map(|name| name.as_str()).collect();
            for name in &module.exposes_opaque {
                set.insert(name.as_str());
            }
            Some(set)
        } else {
            None
        }
    })?;

    let role = if exposed.contains(fd.name.as_str()) {
        "exposed"
    } else {
        "internal"
    };
    Some(role.to_string())
}

fn decision_summary(fd: &FnDef, items: &[TopLevel]) -> Option<String> {
    let decisions: Vec<&DecisionBlock> = index_decisions(items)
        .into_iter()
        .filter(|decision| {
            decision.impacts.iter().any(
                |impact| matches!(&impact.node, DecisionImpact::Symbol(symbol) if symbol == &fd.name),
            )
        })
        .collect();
    if decisions.is_empty() {
        return None;
    }

    let names = decisions
        .iter()
        .map(|decision| decision.name.as_str())
        .collect::<Vec<_>>()
        .join("`, `");
    let label = if decisions.len() == 1 {
        "decision"
    } else {
        "decisions"
    };
    Some(format!("{} `{}`", label, names))
}

fn analysis_summary(fd: &FnDef, items: &[TopLevel], base_dir: Option<&str>) -> Option<String> {
    let mut transformed = items.to_vec();
    tco::transform_program(&mut transformed);

    let auto_tco = transformed.iter().any(|item| match item {
        TopLevel::FnDef(candidate) if candidate.name == fd.name => fn_has_tail_call(candidate),
        _ => false,
    });

    let tc_result = run_type_check_full(&transformed, base_dir);
    let auto_memo = if tc_result.errors.is_empty() {
        let recursive = find_recursive_fns(&transformed);
        let recursive_calls = recursive_callsite_counts(&transformed);
        recursive.contains(&fd.name)
            && tc_result
                .fn_sigs
                .get(&fd.name)
                .is_some_and(|(params, _ret, effects)| {
                    effects.is_empty()
                        && recursive_calls.get(&fd.name).copied().unwrap_or(0) >= 2
                        && params
                            .iter()
                            .all(|ty| is_memo_safe_type(ty, &tc_result.memo_safe_types))
                })
    } else {
        false
    };

    let mut labels = Vec::new();
    if auto_memo {
        labels.push("auto memo");
    }
    if auto_tco {
        labels.push("tco");
    }

    (!labels.is_empty()).then(|| labels.join(", "))
}

fn fn_has_tail_call(fd: &FnDef) -> bool {
    fd.body.stmts().iter().any(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_has_tail_call(expr),
    })
}

fn expr_has_tail_call(expr: &aver::ast::Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::TailCall(_) => true,
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => false,
        Expr::Attr(obj, _) => expr_has_tail_call(obj),
        Expr::FnCall(callee, args) => {
            expr_has_tail_call(callee) || args.iter().any(expr_has_tail_call)
        }
        Expr::BinOp(_, left, right) => expr_has_tail_call(left) || expr_has_tail_call(right),
        Expr::Match { subject, arms, .. } => {
            expr_has_tail_call(subject) || arms.iter().any(|arm| expr_has_tail_call(&arm.body))
        }
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => expr_has_tail_call(inner),
        Expr::Constructor(_, None) => false,
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            aver::ast::StrPart::Literal(_) => false,
            aver::ast::StrPart::Parsed(e) => expr_has_tail_call(e),
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::EffectTuple(items, _) => {
            items.iter().any(expr_has_tail_call)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(key, value)| expr_has_tail_call(key) || expr_has_tail_call(value)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_has_tail_call(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_has_tail_call(base) || updates.iter().any(|(_, e)| expr_has_tail_call(e))
        }
    }
}

fn is_memo_safe_type(ty: &Type, safe_named: &std::collections::HashSet<String>) -> bool {
    match ty {
        Type::Int | Type::Float | Type::Bool | Type::Unit => true,
        Type::Str => false,
        Type::Tuple(items) => items.iter().all(|item| is_memo_safe_type(item, safe_named)),
        Type::List(_) | Type::Vector(_) | Type::Map(_, _) | Type::Fn(_, _, _) | Type::Unknown => {
            false
        }
        Type::Result(_, _) | Type::Option(_) => false,
        Type::Named(name) => safe_named.contains(name),
    }
}

fn build_decision_hover(decision: &DecisionBlock) -> String {
    let mut parts = Vec::new();
    parts.push(format!("```aver\ndecision {}\n```", decision.name));
    parts.push(format!("- chosen: `{}`", decision.chosen.node.text()));
    if !decision.rejected.is_empty() {
        parts.push(format!(
            "- rejected: `{}`",
            decision
                .rejected
                .iter()
                .map(|s| s.node.text())
                .collect::<Vec<_>>()
                .join("`, `")
        ));
    }
    if !decision.impacts.is_empty() {
        parts.push(format!(
            "- impacts: `{}`",
            decision
                .impacts
                .iter()
                .map(|s| s.node.text())
                .collect::<Vec<_>>()
                .join("`, `")
        ));
    }
    if let Some(author) = &decision.author {
        parts.push(format!("- author: `{}`", author));
    }
    if !decision.date.is_empty() {
        parts.push(format!("- date: `{}`", decision.date));
    }
    if !decision.reason.is_empty() {
        parts.push(format!("\n{}", decision.reason));
    }
    parts.join("\n")
}

fn format_signature(fd: &FnDef) -> String {
    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(name, ty)| {
            if ty.is_empty() {
                name.clone()
            } else {
                format!("{}: {}", name, ty)
            }
        })
        .collect();

    let mut sig = format!("fn {}({})", fd.name, params.join(", "));

    if !fd.return_type.is_empty() {
        sig.push_str(&format!(" -> {}", fd.return_type));
    }

    if !fd.effects.is_empty() {
        sig.push_str(&format!(
            " ! [{}]",
            fd.effects
                .iter()
                .map(|e| e.node.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }

    sig
}

fn make_hover(content: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content,
        }),
        range: None,
    }
}

/// Extract the word at a given position from source text.
/// Handles dotted names like "List.map".
pub fn word_at_position(source: &str, line: usize, character: u32) -> Option<String> {
    let target_line = source.lines().nth(line)?;
    let character = utf16_col_to_byte_idx(target_line, character);

    let bytes = target_line.as_bytes();
    let mut start = character;
    while start > 0 && is_word_char(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = character;
    while end < bytes.len() && is_word_char(bytes[end]) {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(target_line[start..end].to_string())
}

fn is_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'.'
}

#[cfg(test)]
mod tests {
    use super::hover_for_word;
    use tower_lsp_server::ls_types::HoverContents;

    #[test]
    fn function_hover_includes_verify_and_decisions() {
        let source = r#"module Demo
    exposes [inc]

decision KeepCorePure
    date = "2026-03-10"
    reason =
        "test"
    chosen = "PureCore"
    rejected = ["ImpureCore"]
    impacts = [inc]

fn inc(x: Int) -> Int
    x + 1

verify inc
    inc(1) => 2

verify inc law grows
    given x: Int = 0..1
    inc(x) => x + 1
"#;

        let hover = hover_for_word("inc", source, None).unwrap();
        let text = match hover.contents {
            HoverContents::Markup(markup) => markup.value,
            _ => String::new(),
        };

        assert!(text.contains("fn inc(x: Int) -> Int"));
        assert!(text.contains("verify 1 case, law `grows`"));
        assert!(text.contains("exposed"));
        assert!(text.contains("decision `KeepCorePure`"));
        assert!(!text.contains("x + 1"));
    }

    #[test]
    fn recursive_hover_includes_analysis_flags() {
        let source = r#"module Demo

fn fib(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
"#;

        let hover = hover_for_word("fib", source, None).unwrap();
        let text = match hover.contents {
            HoverContents::Markup(markup) => markup.value,
            _ => String::new(),
        };

        assert!(text.contains("auto memo"));
    }

    #[test]
    fn decision_hover_lists_choice_and_impacts() {
        let source = r#"module Demo
decision KeepCorePure
    date = "2026-03-10"
    reason =
        "State transition logic should stay pure."
    chosen = "PureCore"
    rejected = ["ImpureCore"]
    impacts = [runPlan, main]
"#;

        let hover = hover_for_word("KeepCorePure", source, None).unwrap();
        let text = match hover.contents {
            HoverContents::Markup(markup) => markup.value,
            _ => String::new(),
        };

        assert!(text.contains("chosen: `PureCore`"));
        assert!(text.contains("impacts: `runPlan`, `main`"));
    }
}
