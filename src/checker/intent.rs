use std::collections::BTreeSet;

use crate::ast::{
    DecisionBlock, DecisionImpact, Expr, FnDef, Spanned, Stmt, StrPart, TailCallData, TopLevel,
    TypeDef, VerifyKind,
};
use crate::verify_law::{canonical_spec_ref, named_law_function};

use super::{CheckFinding, FnSigMap, ModuleCheckFindings, dotted_name, verify_case_calls_target};

/// Returns true if a function requires a ? description annotation.
/// All functions except main() require one.
fn fn_needs_desc(f: &FnDef) -> bool {
    f.name != "main"
}

/// Missing verify warning policy:
/// - skip `main`
/// - skip effectful functions (covered either by Oracle trace/laws or replay)
/// - skip trivial pure pass-through wrappers
/// - skip trivial single-expression bodies without branching/arithmetic
/// - require verify for the rest (pure, non-trivial logic)
fn fn_needs_verify(f: &FnDef) -> bool {
    if f.name == "main" {
        return false;
    }
    if !f.effects.is_empty() {
        return false;
    }
    !is_trivial_passthrough_wrapper(f) && !is_trivial_body(f)
}

/// A function body is trivial when it is a single expression that contains
/// no branching (match) and no arithmetic/comparison (binop).
/// Examples: constructor calls, literals, field access, simple fn calls
/// with literal/ident args.
fn is_trivial_body(f: &FnDef) -> bool {
    if f.body.stmts().len() != 1 {
        return false;
    }
    match f.body.stmts().first() {
        Some(Stmt::Expr(expr)) => !expr_has_branching_or_binop(expr),
        _ => false,
    }
}

fn expr_has_branching_or_binop(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Match { .. } => true,
        Expr::BinOp(_, _, _) => true,
        Expr::Neg(_) => true,
        Expr::FnCall(callee, args) => {
            expr_has_branching_or_binop(callee) || args.iter().any(expr_has_branching_or_binop)
        }
        Expr::Constructor(_, Some(arg)) => expr_has_branching_or_binop(arg),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_has_branching_or_binop)
        }
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => expr_has_branching_or_binop(inner),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, v)| expr_has_branching_or_binop(v))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(e) => expr_has_branching_or_binop(e),
            _ => false,
        }),
        _ => false,
    }
}

fn is_trivial_passthrough_wrapper(f: &FnDef) -> bool {
    let param_names: Vec<&str> = f.params.iter().map(|(name, _)| name.as_str()).collect();

    if f.body.stmts().len() != 1 {
        return false;
    }
    f.body
        .tail_expr()
        .is_some_and(|expr| expr_is_passthrough(expr, &param_names))
}

fn expr_is_passthrough(expr: &Spanned<Expr>, param_names: &[&str]) -> bool {
    match &expr.node {
        // `fn id(x) = x`
        Expr::Ident(name) => param_names.len() == 1 && name == param_names[0],
        // `fn wrap(a,b) = inner(a,b)` (no argument transformation)
        Expr::FnCall(_, args) => args_match_params(args, param_names),
        // `fn some(x) = Option.Some(x)` style
        Expr::Constructor(_, Some(arg)) => {
            if param_names.len() != 1 {
                return false;
            }
            matches!(&arg.node, Expr::Ident(name) if name == param_names[0])
        }
        _ => false,
    }
}

fn args_match_params(args: &[Spanned<Expr>], param_names: &[&str]) -> bool {
    if args.len() != param_names.len() {
        return false;
    }
    args.iter()
        .zip(param_names.iter())
        .all(|(arg, expected)| matches!(&arg.node, Expr::Ident(name) if name == *expected))
}

fn collect_used_effects_expr(expr: &Spanned<Expr>, fn_sigs: &FnSigMap, out: &mut BTreeSet<String>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(callee_name) = dotted_name(callee)
                && let Some((params, _, effects)) = fn_sigs.get(&callee_name)
            {
                for effect in effects {
                    out.insert(effect.clone());
                }
                for (param, argument) in params.iter().zip(args.iter()) {
                    let crate::types::Type::Fn(_, _, callback_slot_effects) = param else {
                        continue;
                    };
                    if !crate::effects::forwards_callback_effects(callback_slot_effects) {
                        continue;
                    }
                    if let Some(callback_name) = dotted_name(argument)
                        && let Some((_, _, callback_effects)) = fn_sigs.get(&callback_name)
                    {
                        for effect in callback_effects {
                            out.insert(effect.clone());
                        }
                    }
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
        Expr::ErrorProp(inner) => collect_used_effects_expr(inner, fn_sigs, out),
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
        Expr::Attr(obj, _) => collect_used_effects_expr(obj, fn_sigs, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_used_effects_expr(expr, fn_sigs, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_used_effects_expr(base, fn_sigs, out);
            for (_, expr) in updates {
                collect_used_effects_expr(expr, fn_sigs, out);
            }
        }
        Expr::Constructor(_, Some(inner)) => collect_used_effects_expr(inner, fn_sigs, out),
        Expr::InterpolatedStr(parts) => {
            // `"x = {fn_call()}"` must descend into the parsed segment
            // so transitive effects of `fn_call` count as used by the
            // enclosing fn. Otherwise the unused-effect lint false-
            // positives whenever a declared effect propagates only
            // through a string interpolation call site.
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_used_effects_expr(inner, fn_sigs, out);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

fn collect_used_effects(f: &FnDef, fn_sigs: &FnSigMap) -> BTreeSet<String> {
    let mut used = BTreeSet::new();
    for stmt in f.body.stmts() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                collect_used_effects_expr(expr, fn_sigs, &mut used)
            }
        }
    }
    used
}

fn collect_declared_symbols(items: &[TopLevel]) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    for item in items {
        match item {
            TopLevel::FnDef(f) => {
                out.insert(f.name.clone());
            }
            TopLevel::Module(m) => {
                out.insert(m.name.clone());
            }
            TopLevel::TypeDef(t) => match t {
                crate::ast::TypeDef::Sum { name, .. }
                | crate::ast::TypeDef::Product { name, .. } => {
                    out.insert(name.clone());
                }
            },
            TopLevel::Decision(d) => {
                out.insert(d.name.clone());
            }
            // Operations and the capability resource are declared names: a
            // `? "..."` justification anywhere in the file may point at
            // them, and omitting them makes the justification
            // bookkeeping report a real symbol as undeclared.
            TopLevel::Capability(item) => {
                out.insert(item.name().to_string());
            }
            TopLevel::Verify(_) | TopLevel::Stmt(_) => {}
        }
    }
    out
}

fn collect_known_effect_symbols(fn_sigs: Option<&FnSigMap>) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    for effect_namespace in ["Console", "Http", "Disk", "Tcp"] {
        out.insert(effect_namespace.to_string());
    }
    if let Some(sigs) = fn_sigs {
        for (_, _, effects) in sigs.values() {
            for effect in effects {
                out.insert(effect.clone());
            }
        }
    }
    out
}

fn decision_symbol_known(
    name: &str,
    declared_symbols: &std::collections::HashSet<String>,
    known_effect_symbols: &std::collections::HashSet<String>,
    dep_modules: &std::collections::HashSet<String>,
) -> bool {
    if declared_symbols.contains(name) || known_effect_symbols.contains(name) {
        return true;
    }
    // Allow qualified names like "Logic.GameState" when "Logic" is a known dependency.
    if let Some(prefix) = name.split('.').next()
        && dep_modules.contains(prefix)
    {
        return true;
    }
    false
}

pub fn check_module_intent(items: &[TopLevel]) -> ModuleCheckFindings {
    check_module_intent_with_sigs(items, None)
}

pub fn check_module_intent_with_sigs(
    items: &[TopLevel],
    fn_sigs: Option<&FnSigMap>,
) -> ModuleCheckFindings {
    check_module_intent_with_sigs_in(items, fn_sigs, None)
}

pub fn check_module_intent_with_sigs_in(
    items: &[TopLevel],
    fn_sigs: Option<&FnSigMap>,
    source_file: Option<&str>,
) -> ModuleCheckFindings {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let declared_symbols = collect_declared_symbols(items);
    let known_effect_symbols = collect_known_effect_symbols(fn_sigs);
    let dep_modules: std::collections::HashSet<String> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::Module(m) = item {
                Some(m.depends.iter().map(|d| {
                    // "Data.Fibonacci" → last segment "Fibonacci" is the namespace name
                    d.rsplit('.').next().unwrap_or(d).to_string()
                }))
            } else {
                None
            }
        })
        .flatten()
        .collect();
    let module_name = items.iter().find_map(|item| {
        if let TopLevel::Module(m) = item {
            Some(m.name.clone())
        } else {
            None
        }
    });

    // Aver files are module-scoped. A file with top-level declarations
    // (fn, type, verify, decision) but no `module Name` header is not
    // a valid Aver source — the CLI's `require_module_declaration`
    // enforces this, and the canonical analyzer should too so audit /
    // playground / LSP all agree.
    if module_name.is_none() {
        let has_top_level = items.iter().any(|item| {
            matches!(
                item,
                TopLevel::FnDef(_)
                    | TopLevel::TypeDef(_)
                    | TopLevel::Verify(_)
                    | TopLevel::Decision(_)
            )
        });
        if has_top_level {
            errors.push(CheckFinding {
                line: 1,
                module: None,
                file: source_file.map(|s| s.to_string()),
                fn_name: None,
                message: "File must declare `module <Name>` as the first top-level item"
                    .to_string(),
                extra_spans: vec![],
            });
        }
    }

    let mut verified_fns: std::collections::HashSet<&str> = std::collections::HashSet::new();
    let mut spec_fns: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut empty_verify_fns: std::collections::HashSet<&str> = std::collections::HashSet::new();
    let mut invalid_verify_fns: std::collections::HashSet<&str> = std::collections::HashSet::new();
    for item in items {
        if let TopLevel::Verify(v) = item {
            if v.cases.is_empty() {
                errors.push(CheckFinding {
                    line: v.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message: format!(
                        "Verify block '{}' must contain at least one case",
                        v.fn_name
                    ),
                    extra_spans: vec![],
                });
                empty_verify_fns.insert(v.fn_name.as_str());
            } else {
                let mut block_valid = true;
                if matches!(v.kind, VerifyKind::Cases) {
                    for (idx, (left, _right)) in v.cases.iter().enumerate() {
                        if !verify_case_calls_target(left, &v.fn_name) {
                            errors.push(CheckFinding {
                                line: v.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: None,
                                message: format!(
                                    "Verify block '{}' case #{} must call '{}' on the left side",
                                    v.fn_name,
                                    idx + 1,
                                    v.fn_name
                                ),
                                extra_spans: vec![],
                            });
                            block_valid = false;
                        }
                    }
                    for (idx, (_left, right)) in v.cases.iter().enumerate() {
                        if verify_case_calls_target(right, &v.fn_name) {
                            // Use right-hand side line; finding will underline
                            // after `=>` via the `=>` prefix hint in the message.
                            let rhs_line = right.line;
                            errors.push(CheckFinding {
                                line: if rhs_line > 0 { rhs_line } else { v.line },
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: Some(v.fn_name.clone()),
                                message: format!(
                                    "case #{} must not call `{}` on the right side of `=>`",
                                    idx + 1,
                                    v.fn_name
                                ),
                                extra_spans: vec![],
                            });
                            block_valid = false;
                        }
                    }
                }
                if let VerifyKind::Law(law) = &v.kind
                    && let Some(sigs) = fn_sigs
                    && let Some(named_fn) = named_law_function(law, sigs)
                {
                    if !named_fn.is_pure {
                        errors.push(CheckFinding {
                            line: v.line,
                            module: module_name.clone(),
                            file: source_file.map(|s| s.to_string()),
                            fn_name: None,
                            message: format!(
                                "Verify law '{}.{}' resolves to effectful function '{}'; spec functions must be pure",
                                v.fn_name, law.name, named_fn.name
                            ),
                            extra_spans: vec![],
                        });
                        block_valid = false;
                    } else if let Some(spec_ref) = canonical_spec_ref(&v.fn_name, law, sigs) {
                        spec_fns.insert(spec_ref.spec_fn_name);
                    } else {
                        warnings.push(CheckFinding {
                            line: v.line,
                            module: module_name.clone(),
                            file: source_file.map(|s| s.to_string()),
                            fn_name: None,
                            message: format!(
                                "Verify law '{}.{}' names pure function '{}' but the law body never calls it; use '{}' in the assertion or rename the law",
                                v.fn_name, law.name, named_fn.name, named_fn.name
                            ),
                            extra_spans: vec![],
                        });
                    }
                }
                if block_valid {
                    verified_fns.insert(v.fn_name.as_str());
                } else {
                    invalid_verify_fns.insert(v.fn_name.as_str());
                }
            }
        }
    }

    for item in items {
        match item {
            TopLevel::Module(m) => {
                if m.intent.is_empty() {
                    warnings.push(CheckFinding {
                        line: m.line,
                        module: Some(m.name.clone()),
                        file: source_file.map(|s| s.to_string()),
                        fn_name: None,
                        message: format!("Module '{}' has no intent block", m.name),
                        extra_spans: vec![],
                    });
                }
                // Validate exposes_opaque: each name must be a TypeDef.
                if !m.exposes_opaque.is_empty() {
                    let type_names: std::collections::HashSet<&str> = items
                        .iter()
                        .filter_map(|item| match item {
                            TopLevel::TypeDef(TypeDef::Sum { name, .. })
                            | TopLevel::TypeDef(TypeDef::Product { name, .. }) => {
                                Some(name.as_str())
                            }
                            _ => None,
                        })
                        .collect();
                    let exposed_set: std::collections::HashSet<&str> =
                        m.exposes.iter().map(|s| s.as_str()).collect();
                    for opaque_name in &m.exposes_opaque {
                        if !type_names.contains(opaque_name.as_str()) {
                            errors.push(CheckFinding {
                                line: m.line,
                                module: Some(m.name.clone()),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: None,
                                message: format!(
                                    "'{}' in exposes opaque is not a type defined in this module",
                                    opaque_name
                                ),
                                extra_spans: vec![],
                            });
                        }
                        if exposed_set.contains(opaque_name.as_str()) {
                            errors.push(CheckFinding {
                                line: m.line,
                                module: Some(m.name.clone()),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: None,
                                message: format!(
                                    "'{}' cannot be in both exposes and exposes opaque",
                                    opaque_name
                                ),
                                extra_spans: vec![],
                            });
                        }
                    }
                }
            }
            TopLevel::FnDef(f) => {
                if f.desc.is_none() && fn_needs_desc(f) {
                    warnings.push(CheckFinding {
                        line: f.line,
                        module: module_name.clone(),
                        file: source_file.map(|s| s.to_string()),
                        fn_name: None,
                        message: format!("Function '{}' has no description (?)", f.name),
                        extra_spans: vec![],
                    });
                }
                if let Some(sigs) = fn_sigs
                    && let Some((_, _, declared_effects)) = sigs.get(&f.name)
                    && !declared_effects.is_empty()
                {
                    let used_effects = collect_used_effects(f, sigs);
                    let unused_effects: Vec<String> = declared_effects
                        .iter()
                        .filter(|declared| {
                            !used_effects
                                .iter()
                                .any(|used| crate::effects::effect_satisfies(declared, used))
                        })
                        .cloned()
                        .collect();
                    if !unused_effects.is_empty() {
                        let used = if used_effects.is_empty() {
                            "none".to_string()
                        } else {
                            used_effects.iter().cloned().collect::<Vec<_>>().join(", ")
                        };
                        for unused in &unused_effects {
                            // Find the line from the spanned effects in the AST
                            let effect_line = f
                                .effects
                                .iter()
                                .find(|e| e.node == *unused)
                                .map(|e| e.line)
                                .unwrap_or(f.line);
                            warnings.push(CheckFinding {
                                line: effect_line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: Some(f.name.clone()),
                                message: format!("unused effect `{}` (used: {})", unused, used),
                                extra_spans: vec![],
                            });
                        }
                    }
                    // Suggest granular effects when namespace shorthand could be narrowed
                    for declared in declared_effects {
                        if !declared.contains('.') {
                            let prefix = format!("{}.", declared);
                            let mut matching: Vec<&str> = used_effects
                                .iter()
                                .filter(|u| u.starts_with(&prefix))
                                .map(|s| s.as_str())
                                .collect();
                            matching.sort();
                            if !matching.is_empty() && !used_effects.contains(declared) {
                                warnings.push(CheckFinding {
                                    line: f.line,
                                    module: module_name.clone(),
                                    file: source_file.map(|s| s.to_string()),
                                    fn_name: None,
                                    message: format!(
                                        "Function '{}' declares '{}' — only uses {}; consider granular `! [{}]`",
                                        f.name,
                                        declared,
                                        matching.join(", "),
                                        matching.join(", ")
                                    ),
                                    extra_spans: vec![],
                                });
                            }
                        }
                    }
                }
                if fn_needs_verify(f)
                    && !verified_fns.contains(f.name.as_str())
                    && !spec_fns.contains(&f.name)
                    && !empty_verify_fns.contains(f.name.as_str())
                    && !invalid_verify_fns.contains(f.name.as_str())
                {
                    errors.push(CheckFinding {
                        line: f.line,
                        module: module_name.clone(),
                        file: source_file.map(|s| s.to_string()),
                        fn_name: None,
                        message: format!("Function '{}' has no verify block", f.name),
                        extra_spans: vec![],
                    });
                }
            }
            TopLevel::Decision(d) => {
                if let DecisionImpact::Symbol(name) = &d.chosen.node
                    && !decision_symbol_known(
                        name,
                        &declared_symbols,
                        &known_effect_symbols,
                        &dep_modules,
                    )
                {
                    errors.push(CheckFinding {
                            line: d.chosen.line,
                            module: module_name.clone(),
                            file: source_file.map(|s| s.to_string()),
                            fn_name: Some(d.name.clone()),
                            message: format!(
                                "Decision '{}' references unknown chosen symbol '{}'. Use quoted string for semantic chosen value.",
                                d.name, name
                            ),
                            extra_spans: vec![],
                        });
                }
                for rejected in &d.rejected {
                    if let DecisionImpact::Symbol(name) = &rejected.node
                        && !decision_symbol_known(
                            name,
                            &declared_symbols,
                            &known_effect_symbols,
                            &dep_modules,
                        )
                    {
                        errors.push(CheckFinding {
                                line: rejected.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: Some(d.name.clone()),
                                message: format!(
                                    "Decision '{}' references unknown rejected symbol '{}'. Use quoted string for semantic rejected value.",
                                    d.name, name
                                ),
                                extra_spans: vec![],
                            });
                    }
                }
                for impact in &d.impacts {
                    if let DecisionImpact::Symbol(name) = &impact.node
                        && !decision_symbol_known(
                            name,
                            &declared_symbols,
                            &known_effect_symbols,
                            &dep_modules,
                        )
                    {
                        errors.push(CheckFinding {
                                line: impact.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: Some(d.name.clone()),
                                message: format!(
                                    "unknown impact symbol `{}` — use quoted string for semantic impact",
                                    name
                                ),
                                extra_spans: vec![],
                            });
                    }
                }
            }
            _ => {}
        }
    }

    ModuleCheckFindings { errors, warnings }
}

pub fn index_decisions(items: &[TopLevel]) -> Vec<&DecisionBlock> {
    items
        .iter()
        .filter_map(|item| {
            if let TopLevel::Decision(d) = item {
                Some(d)
            } else {
                None
            }
        })
        .collect()
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
    fn no_verify_warning_for_effectful_function() {
        let items = parse_items(
            r#"
fn log(x: Int) -> Unit
    ! [Console]
    Console.print(x)
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings
                .warnings
                .iter()
                .any(|w| w.message.contains("no verify block"))
                && !findings
                    .errors
                    .iter()
                    .any(|e| e.message.contains("no verify block")),
            "unexpected findings: errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn warns_on_unused_declared_effects() {
        let items = parse_items(
            r#"
fn log(x: Int) -> Unit
    ! [Console.print, Http.get]
    Console.print("{x}")
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
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            findings.warnings.iter().any(|w| {
                w.message.contains("unused effect")
                    && w.message.contains("Http")
                    && w.message.contains("used: Console.print")
            }),
            "expected unused-effect warning, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn no_unused_effect_warning_when_declared_effects_are_minimal() {
        let items = parse_items(
            r#"
fn log(x: Int) -> Unit
    ! [Console.print]
    Console.print("{x}")
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
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            !findings
                .warnings
                .iter()
                .any(|w| w.message.contains("unused effect")),
            "did not expect unused-effect warning, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
        assert!(
            !findings
                .warnings
                .iter()
                .any(|w| w.message.contains("declares broad effect")),
            "did not expect broad-effect warning, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn no_granular_warning_when_namespace_effect_is_also_required_transitively() {
        let items = parse_items(
            r#"
fn inner() -> Unit
    ! [Console]
    Unit

fn outer() -> Unit
    ! [Console]
    Console.print("hi")
    inner()
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
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message.contains("Function 'outer' declares 'Console'")),
            "did not expect granular suggestion for outer, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn no_verify_warning_for_trivial_passthrough_wrapper() {
        let items = parse_items(
            r#"
fn passthrough(x: Int) -> Int
    inner(x)
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings
                .warnings
                .iter()
                .any(|w| w.message.contains("no verify block"))
                && !findings
                    .errors
                    .iter()
                    .any(|e| e.message.contains("no verify block")),
            "unexpected findings: errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn verify_error_for_pure_non_trivial_logic() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings
                .errors
                .iter()
                .any(|e| e.message == "Function 'add1' has no verify block"),
            "expected verify error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn empty_verify_block_is_rejected() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

verify add1
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings
                .errors
                .iter()
                .any(|e| e.message == "Verify block 'add1' must contain at least one case"),
            "expected empty verify error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message == "Function 'add1' has no verify block"),
            "expected no duplicate missing-verify error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn verify_case_must_call_verified_function_on_left_side() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

verify add1
    true => true
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings.errors.iter().any(|e| {
                e.message
                    .contains("Verify block 'add1' case #1 must call 'add1' on the left side")
            }),
            "expected verify-case-call error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message == "Function 'add1' has no verify block"),
            "expected no duplicate missing-verify error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn verify_case_must_not_call_verified_function_on_right_side() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

verify add1
    add1(1) => add1(1)
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings.errors.iter().any(|e| {
                e.message
                    .contains("case #1 must not call `add1` on the right side of `=>`")
            }),
            "expected verify-case-rhs error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn verify_law_skips_left_right_call_heuristics() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

verify add1 law reflexive
    given x: Int = [1, 2, 3]
    x => x
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message.contains("must call 'add1' on the left side")),
            "did not expect lhs-call heuristic for law verify, got errors={:?}",
            findings.errors
        );
        assert!(
            !findings.errors.iter().any(|e| e
                .message
                .contains("must not call `add1` on the right side of `=>`")),
            "did not expect rhs-call heuristic for law verify, got errors={:?}",
            findings.errors
        );
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message == "Function 'add1' has no verify block"),
            "law verify should satisfy verify requirement, got errors={:?}",
            findings.errors
        );
    }

    #[test]
    fn verify_law_when_must_have_bool_type() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

verify add1 law ordered
    given x: Int = [1, 2]
    when add1(x)
    x => x
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors
                .iter()
                .any(|e| e.message.contains("when condition must have type Bool")),
            "expected Bool type error for when, got errors={:?}",
            tc.errors
        );
    }

    #[test]
    fn verify_law_when_must_be_pure() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

fn noisyPositive(x: Int) -> Bool
    ! [Console.print]
    Console.print("{x}")
    x > 0

verify add1 law ordered
    given x: Int = [1, 2]
    when noisyPositive(x)
    x => x
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        assert!(
            tc.errors.iter().any(|e| e.message.contains(
                "Function '<verify:add1>' calls 'noisyPositive' which has effect 'Console.print'"
            )),
            "expected purity error for when, got errors={:?}",
            tc.errors
        );
    }

    #[test]
    fn verify_law_named_effectful_function_is_an_error() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

fn specFn(x: Int) -> Int
    ! [Console.print]
    Console.print("{x}")
    x

verify add1 law specFn
    given x: Int = [1, 2]
    add1(x) => add1(x)
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            findings.errors.iter().any(|e| e.message.contains(
                "Verify law 'add1.specFn' resolves to effectful function 'specFn'; spec functions must be pure"
            )),
            "expected effectful-spec error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn verify_law_named_pure_function_must_appear_in_law_body() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

fn add1Spec(x: Int) -> Int
    x + 1

verify add1 law add1Spec
    given x: Int = [1, 2]
    add1(x) => x + 1
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            findings.warnings.iter().any(|w| w.message.contains(
                "Verify law 'add1.add1Spec' names pure function 'add1Spec' but the law body never calls it"
            )),
            "expected unused-spec warning, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn canonical_spec_function_does_not_need_its_own_verify_block() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    x + 1

fn add1Spec(x: Int) -> Int
    x + 1

verify add1 law add1Spec
    given x: Int = [1, 2]
    add1(x) => add1Spec(x)
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message == "Function 'add1Spec' has no verify block"),
            "spec function should not need its own verify block, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn decision_unknown_symbol_impact_is_error() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

fn existing() -> Int
    1

verify existing
    existing() => 1

decision D
    date = "2026-03-05"
    reason =
        "x"
    chosen = "ExistingChoice"
    rejected = []
    impacts = [existing, missingThing]
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings
                .errors
                .iter()
                .any(|e| e.message.contains("unknown impact symbol `missingThing`")),
            "expected unknown-impact error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn decision_semantic_string_impact_is_allowed() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

fn existing() -> Int
    1

verify existing
    existing() => 1

decision D
    date = "2026-03-05"
    reason =
        "x"
    chosen = "ExistingChoice"
    rejected = []
    impacts = [existing, "error handling strategy"]
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message.contains("unknown impact symbol")),
            "did not expect unknown-impact error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn decision_unknown_chosen_symbol_is_error() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

fn existing() -> Int
    1

verify existing
    existing() => 1

decision D
    date = "2026-03-05"
    reason =
        "x"
    chosen = MissingChoice
    rejected = []
    impacts = [existing]
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings
                .errors
                .iter()
                .any(|e| e.message.contains("unknown chosen symbol 'MissingChoice'")),
            "expected unknown-chosen error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn decision_unknown_rejected_symbol_is_error() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

fn existing() -> Int
    1

verify existing
    existing() => 1

decision D
    date = "2026-03-05"
    reason =
        "x"
    chosen = "Keep"
    rejected = [MissingAlternative]
    impacts = [existing]
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings.errors.iter().any(|e| e
                .message
                .contains("unknown rejected symbol 'MissingAlternative'")),
            "expected unknown-rejected error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn decision_semantic_string_chosen_and_rejected_are_allowed() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

fn existing() -> Int
    1

verify existing
    existing() => 1

decision D
    date = "2026-03-05"
    reason =
        "x"
    chosen = "Keep explicit context"
    rejected = ["Closure capture", "Global mutable state"]
    impacts = [existing]
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message.contains("unknown chosen symbol")
                    || e.message.contains("unknown rejected symbol")),
            "did not expect chosen/rejected symbol errors, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn decision_builtin_effect_impact_is_allowed() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

fn existing() -> Int
    1

verify existing
    existing() => 1

decision D
    date = "2026-03-05"
    reason =
        "x"
    chosen = "ExistingChoice"
    rejected = []
    impacts = [existing, Tcp]
"#,
        );
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
        );
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.message.contains("references unknown impact symbol 'Tcp'")),
            "did not expect Tcp impact error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn decision_removed_effect_alias_impact_is_error() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

fn existing() -> Int
    1

verify existing
    existing() => 1

decision D
    date = "2026-03-05"
    reason =
        "x"
    chosen = "ExistingChoice"
    rejected = []
    impacts = [existing, AppIO]
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings
                .errors
                .iter()
                .any(|e| e.message.contains("unknown impact symbol `AppIO`")),
            "expected AppIO impact error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }
}
