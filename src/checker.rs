use colored::Colorize;
use std::collections::BTreeSet;

use crate::ast::{
    DecisionBlock, DecisionImpact, Expr, FnDef, Pattern, Stmt, TopLevel, VerifyBlock,
    VerifyGivenDomain, VerifyKind,
};
use crate::interpreter::{Interpreter, aver_repr};
use crate::types::{Type, parse_type_str_strict};
use crate::value::{RuntimeError, Value};
use crate::verify_law::{canonical_spec_ref, named_law_function};

pub struct VerifyResult {
    #[allow(dead_code)]
    pub fn_name: String,
    pub passed: usize,
    pub failed: usize,
    pub skipped: usize,
    #[allow(dead_code)]
    pub failures: Vec<(String, String, String)>, // (expr_src, expected, actual)
}

pub struct ModuleCheckFindings {
    pub errors: Vec<CheckFinding>,
    pub warnings: Vec<CheckFinding>,
}

type FnSigSummary = (Vec<Type>, Type, Vec<String>);
type FnSigMap = std::collections::HashMap<String, FnSigSummary>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckFinding {
    pub line: usize,
    pub module: Option<String>,
    pub file: Option<String>,
    pub message: String,
}

fn module_name_for_items(items: &[TopLevel]) -> Option<String> {
    items.iter().find_map(|item| {
        if let TopLevel::Module(m) = item {
            Some(m.name.clone())
        } else {
            None
        }
    })
}

fn dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => {
            let mut prefix = dotted_name(base)?;
            prefix.push('.');
            prefix.push_str(field);
            Some(prefix)
        }
        _ => None,
    }
}

fn normalize_constructor_tag(path: &str) -> Option<String> {
    let mut parts = path.split('.').collect::<Vec<_>>();
    if parts.len() < 2 {
        return None;
    }
    let variant = parts.pop()?;
    let type_name = parts.pop()?;
    Some(format!("{}.{}", type_name, variant))
}

fn constructor_tag_from_pattern(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Constructor(path, _) => normalize_constructor_tag(path),
        _ => None,
    }
}

fn constructor_tag_from_expr(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Attr(_, _) => normalize_constructor_tag(&dotted_name(expr)?),
        Expr::FnCall(callee, _) => normalize_constructor_tag(&dotted_name(callee)?),
        Expr::Constructor(name, _) => normalize_constructor_tag(name),
        _ => None,
    }
}

fn collect_target_call_args<'a>(
    expr: &'a Expr,
    fn_name: &str,
    arg_index: usize,
    out: &mut Vec<&'a Expr>,
) {
    match expr {
        Expr::FnCall(callee, args) => {
            if callee_is_target(callee, fn_name)
                && let Some(arg) = args.get(arg_index)
            {
                out.push(arg);
            }
            collect_target_call_args(callee, fn_name, arg_index, out);
            for arg in args {
                collect_target_call_args(arg, fn_name, arg_index, out);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_target_call_args(left, fn_name, arg_index, out);
            collect_target_call_args(right, fn_name, arg_index, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_target_call_args(subject, fn_name, arg_index, out);
            for arm in arms {
                collect_target_call_args(&arm.body, fn_name, arg_index, out);
            }
        }
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            collect_target_call_args(inner, fn_name, arg_index, out);
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_target_call_args(item, fn_name, arg_index, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_target_call_args(key, fn_name, arg_index, out);
                collect_target_call_args(value, fn_name, arg_index, out);
            }
        }
        Expr::Attr(obj, _) => collect_target_call_args(obj, fn_name, arg_index, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_target_call_args(value, fn_name, arg_index, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_target_call_args(base, fn_name, arg_index, out);
            for (_, value) in updates {
                collect_target_call_args(value, fn_name, arg_index, out);
            }
        }
        Expr::TailCall(boxed) => {
            let (target, args) = boxed.as_ref();
            if target == fn_name
                && let Some(arg) = args.get(arg_index)
            {
                out.push(arg);
            }
            for arg in args {
                collect_target_call_args(arg, fn_name, arg_index, out);
            }
        }
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::InterpolatedStr(_)
        | Expr::Resolved(_)
        | Expr::Constructor(_, None) => {}
    }
}

fn expr_is_result_err_case(expr: &Expr) -> bool {
    match expr {
        Expr::FnCall(callee, _) => dotted_name(callee)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Result.Err"),
        Expr::Constructor(name, _) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Result.Err")
        }
        _ => false,
    }
}

fn expr_is_option_none_case(expr: &Expr) -> bool {
    match expr {
        Expr::Attr(_, _) => dotted_name(expr)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Option.None"),
        Expr::Constructor(name, None) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Option.None")
        }
        _ => false,
    }
}

fn verify_cases_block_is_well_formed(block: &VerifyBlock) -> bool {
    matches!(block.kind, VerifyKind::Cases)
        && !block.cases.is_empty()
        && block.cases.iter().all(|(left, right)| {
            verify_case_calls_target(left, &block.fn_name)
                && !verify_case_calls_target(right, &block.fn_name)
        })
}

fn enum_match_coverage_target(f: &FnDef) -> Option<(usize, Vec<String>)> {
    let Expr::Match { subject, arms, .. } = f.body.tail_expr()? else {
        return None;
    };
    let Expr::Ident(subject_name) = subject.as_ref() else {
        return None;
    };
    let param_index = f.params.iter().position(|(name, _)| name == subject_name)?;
    let constructors: BTreeSet<String> = arms
        .iter()
        .filter_map(|arm| constructor_tag_from_pattern(&arm.pattern))
        .collect();
    if constructors.is_empty() {
        None
    } else {
        Some((param_index, constructors.into_iter().collect()))
    }
}

pub fn collect_verify_coverage_warnings(items: &[TopLevel]) -> Vec<CheckFinding> {
    collect_verify_coverage_warnings_in(items, None)
}

pub fn collect_verify_coverage_warnings_in(
    items: &[TopLevel],
    source_file: Option<&str>,
) -> Vec<CheckFinding> {
    let module_name = module_name_for_items(items);
    let fn_defs: std::collections::HashMap<&str, &FnDef> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(f) = item {
                Some((f.name.as_str(), f))
            } else {
                None
            }
        })
        .collect();

    let mut warnings = Vec::new();
    for block in merge_verify_blocks(items) {
        if !verify_cases_block_is_well_formed(&block) {
            continue;
        }
        let Some(f) = fn_defs.get(block.fn_name.as_str()).copied() else {
            continue;
        };

        if let Ok(ret_ty) = parse_type_str_strict(&f.return_type) {
            if matches!(ret_ty, Type::Result(_, _))
                && !block
                    .cases
                    .iter()
                    .any(|(_, right)| expr_is_result_err_case(right))
            {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    message: format!(
                        "verify examples for {} do not include any Result.Err case",
                        block.fn_name
                    ),
                });
            }

            if matches!(ret_ty, Type::Option(_))
                && !block
                    .cases
                    .iter()
                    .any(|(_, right)| expr_is_option_none_case(right))
            {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    message: format!(
                        "verify examples for {} do not include any Option.None case",
                        block.fn_name
                    ),
                });
            }
        }

        if let Some((param_index, constructors)) = enum_match_coverage_target(f) {
            let mut covered = BTreeSet::new();
            for (left, _) in &block.cases {
                let mut args = Vec::new();
                collect_target_call_args(left, &block.fn_name, param_index, &mut args);
                for arg in args {
                    if let Some(tag) = constructor_tag_from_expr(arg)
                        && constructors.contains(&tag)
                    {
                        covered.insert(tag);
                    }
                }
            }
            if covered.len() < constructors.len() {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    message: format!(
                        "verify examples for {} cover {}/{} enum constructors",
                        block.fn_name,
                        covered.len(),
                        constructors.len()
                    ),
                });
            }
        }
    }

    warnings
}

fn verify_given_domain_to_str(domain: &VerifyGivenDomain) -> String {
    match domain {
        VerifyGivenDomain::IntRange { start, end } => format!("{start}..{end}"),
        VerifyGivenDomain::Explicit(values) => {
            let parts: Vec<String> = values.iter().map(expr_to_str).collect();
            format!("[{}]", parts.join(", "))
        }
    }
}

pub fn run_verify(block: &VerifyBlock, interp: &mut Interpreter) -> VerifyResult {
    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;
    let mut failures = Vec::new();
    let is_law = matches!(block.kind, VerifyKind::Law(_));

    match &block.kind {
        VerifyKind::Cases => println!("Verify: {}", block.fn_name.cyan()),
        VerifyKind::Law(law) => {
            if let Ok(Value::Fn { effects, .. }) = interp.lookup(&law.name)
                && effects.is_empty()
                && crate::verify_law::canonical_spec_shape(&block.fn_name, law, &law.name)
            {
                println!("Verify: {} spec {}", block.fn_name.cyan(), law.name.cyan());
            } else {
                println!("Verify: {} law {}", block.fn_name.cyan(), law.name.cyan());
            }
            for given in &law.givens {
                println!(
                    "  {} {}: {} = {}",
                    "given".dimmed(),
                    given.name,
                    given.type_name,
                    verify_given_domain_to_str(&given.domain)
                );
            }
            if let Some(when_expr) = &law.when {
                println!("  {} {}", "when".dimmed(), expr_to_str(when_expr));
            }
            println!(
                "  {} {} == {}",
                "law".dimmed(),
                expr_to_str(&law.lhs),
                expr_to_str(&law.rhs)
            );
            println!("  {} {}", "cases".dimmed(), block.cases.len());
        }
    }

    for (idx, (left_expr, right_expr)) in block.cases.iter().enumerate() {
        let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));
        let case_label = if is_law {
            format!("case {}/{}", idx + 1, block.cases.len())
        } else {
            case_str.clone()
        };
        let failure_case = if is_law {
            format!("{} [{}]", case_label, case_str)
        } else {
            case_str.clone()
        };

        if let VerifyKind::Law(law) = &block.kind
            && let Some(sample_guard) = law.sample_guards.get(idx)
        {
            match interp.eval_expr(sample_guard) {
                Ok(Value::Bool(true)) => {}
                Ok(Value::Bool(false)) => {
                    skipped += 1;
                    println!("  {} {} (when false)", "·".dimmed(), case_label.dimmed());
                    continue;
                }
                Ok(other) => {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    println!("      when did not evaluate to Bool: {}", aver_repr(&other));
                    failures.push((
                        failure_case,
                        "Bool".to_string(),
                        format!("when produced {}", aver_repr(&other)),
                    ));
                    continue;
                }
                Err(RuntimeError::ErrProp(err_val)) => {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    println!("      when ? hit Result.Err({})", aver_repr(&err_val));
                    failures.push((
                        failure_case,
                        String::new(),
                        format!("when ? hit Result.Err({})", aver_repr(&err_val)),
                    ));
                    continue;
                }
                Err(e) => {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    println!("      when error: {}", e);
                    failures.push((failure_case, String::new(), format!("WHEN ERROR: {}", e)));
                    continue;
                }
            }
        }

        let left_result = interp.eval_expr(left_expr);
        let right_result = interp.eval_expr(right_expr);

        match (left_result, right_result) {
            (Ok(left_val), Ok(right_val)) => {
                if interp.aver_eq(&left_val, &right_val) {
                    passed += 1;
                    if !is_law {
                        println!("  {} {}", "✓".green(), case_label);
                    }
                } else {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_label);
                    if is_law {
                        println!("      expanded: {}", case_str);
                    }
                    let expected = aver_repr(&right_val);
                    let actual = aver_repr(&left_val);
                    println!("      expected: {}", expected);
                    println!("      got:      {}", actual);
                    failures.push((failure_case, expected, actual));
                }
            }
            // `?` in a verify case hitting Err produces ErrProp — treat as test failure.
            (Err(RuntimeError::ErrProp(err_val)), _) | (_, Err(RuntimeError::ErrProp(err_val))) => {
                failed += 1;
                println!("  {} {}", "✗".red(), case_label);
                if is_law {
                    println!("      expanded: {}", case_str);
                }
                println!("      ? hit Result.Err({})", aver_repr(&err_val));
                failures.push((
                    failure_case,
                    String::new(),
                    format!("? hit Result.Err({})", aver_repr(&err_val)),
                ));
            }
            (Err(e), _) | (_, Err(e)) => {
                failed += 1;
                println!("  {} {}", "✗".red(), case_label);
                if is_law {
                    println!("      expanded: {}", case_str);
                }
                println!("      error: {}", e);
                failures.push((failure_case, String::new(), format!("ERROR: {}", e)));
            }
        }
    }

    let total = passed + failed;
    if is_law && failed == 0 {
        if skipped == 0 {
            println!(
                "  {} all {} generated case(s) passed",
                "✓".green(),
                block.cases.len()
            );
        } else {
            println!(
                "  {} all {} active generated case(s) passed ({} skipped by when)",
                "✓".green(),
                passed,
                skipped
            );
        }
    }
    if failed == 0 && skipped == 0 {
        println!("  {}", format!("{}/{} passed", passed, total).green());
    } else if failed == 0 {
        println!(
            "  {}",
            format!("{}/{} passed, {} skipped", passed, total + skipped, skipped).green()
        );
    } else if skipped == 0 {
        println!("  {}", format!("{}/{} passed", passed, total).red());
    } else {
        println!(
            "  {}",
            format!("{}/{} passed, {} skipped", passed, total + skipped, skipped).red()
        );
    }

    VerifyResult {
        fn_name: block.fn_name.clone(),
        passed,
        failed,
        skipped,
        failures,
    }
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

pub fn merge_verify_blocks(items: &[TopLevel]) -> Vec<VerifyBlock> {
    let mut merged: Vec<VerifyBlock> = Vec::new();
    let mut by_fn_cases: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();

    for item in items {
        let TopLevel::Verify(vb) = item else {
            continue;
        };
        match &vb.kind {
            VerifyKind::Cases => {
                if let Some(&idx) = by_fn_cases.get(&vb.fn_name) {
                    merged[idx].cases.extend(vb.cases.clone());
                } else {
                    by_fn_cases.insert(vb.fn_name.clone(), merged.len());
                    merged.push(vb.clone());
                }
            }
            VerifyKind::Law(_) => {
                merged.push(vb.clone());
            }
        }
    }

    merged
}

/// Returns true if a function requires a ? description annotation.
/// All functions except main() require one.
fn fn_needs_desc(f: &FnDef) -> bool {
    f.name != "main"
}

/// Missing verify warning policy:
/// - skip `main`
/// - skip effectful functions (tested through replay/recording flow)
/// - skip trivial pure pass-through wrappers
/// - require verify for the rest (pure, non-trivial logic)
fn fn_needs_verify(f: &FnDef) -> bool {
    if f.name == "main" {
        return false;
    }
    if !f.effects.is_empty() {
        return false;
    }
    !is_trivial_passthrough_wrapper(f)
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

fn expr_is_passthrough(expr: &Expr, param_names: &[&str]) -> bool {
    match expr {
        // `fn id(x) = x`
        Expr::Ident(name) => param_names.len() == 1 && name == param_names[0],
        // `fn wrap(a,b) = inner(a,b)` (no argument transformation)
        Expr::FnCall(_, args) => args_match_params(args, param_names),
        // `fn some(x) = Option.Some(x)` style
        Expr::Constructor(_, Some(arg)) => {
            if param_names.len() != 1 {
                return false;
            }
            matches!(arg.as_ref(), Expr::Ident(name) if name == param_names[0])
        }
        _ => false,
    }
}

fn args_match_params(args: &[Expr], param_names: &[&str]) -> bool {
    if args.len() != param_names.len() {
        return false;
    }
    args.iter()
        .zip(param_names.iter())
        .all(|(arg, expected)| matches!(arg, Expr::Ident(name) if name == *expected))
}

fn verify_case_calls_target(left: &Expr, fn_name: &str) -> bool {
    match left {
        Expr::FnCall(callee, args) => {
            callee_is_target(callee, fn_name)
                || verify_case_calls_target(callee, fn_name)
                || args
                    .iter()
                    .any(|arg| verify_case_calls_target(arg, fn_name))
        }
        Expr::BinOp(_, left_expr, right_expr) => {
            verify_case_calls_target(left_expr, fn_name)
                || verify_case_calls_target(right_expr, fn_name)
        }
        Expr::Match { subject, arms, .. } => {
            verify_case_calls_target(subject, fn_name)
                || arms
                    .iter()
                    .any(|arm| verify_case_calls_target(&arm.body, fn_name))
        }
        Expr::Constructor(_, Some(inner)) => verify_case_calls_target(inner, fn_name),
        Expr::ErrorProp(inner) => verify_case_calls_target(inner, fn_name),
        Expr::List(elems) => elems
            .iter()
            .any(|elem| verify_case_calls_target(elem, fn_name)),
        Expr::Tuple(items) => items
            .iter()
            .any(|item| verify_case_calls_target(item, fn_name)),
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            verify_case_calls_target(k, fn_name) || verify_case_calls_target(v, fn_name)
        }),
        Expr::Attr(obj, _) => verify_case_calls_target(obj, fn_name),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, expr)| verify_case_calls_target(expr, fn_name)),
        Expr::RecordUpdate { base, updates, .. } => {
            verify_case_calls_target(base, fn_name)
                || updates
                    .iter()
                    .any(|(_, expr)| verify_case_calls_target(expr, fn_name))
        }
        Expr::TailCall(boxed) => {
            boxed.0 == fn_name
                || boxed
                    .1
                    .iter()
                    .any(|arg| verify_case_calls_target(arg, fn_name))
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::InterpolatedStr(_) | Expr::Resolved(_) => false,
        Expr::Constructor(_, None) => false,
    }
}

fn callee_is_target(callee: &Expr, fn_name: &str) -> bool {
    matches!(callee, Expr::Ident(name) if name == fn_name)
}

fn collect_used_effects_expr(expr: &Expr, fn_sigs: &FnSigMap, out: &mut BTreeSet<String>) {
    match expr {
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
            let (target, args) = boxed.as_ref();
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
        Expr::Match { subject, arms, .. } => {
            collect_used_effects_expr(subject, fn_sigs, out);
            for arm in arms {
                collect_used_effects_expr(&arm.body, fn_sigs, out);
            }
        }
        Expr::ErrorProp(inner) => collect_used_effects_expr(inner, fn_sigs, out),
        Expr::List(items) | Expr::Tuple(items) => {
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
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::InterpolatedStr(_)
        | Expr::Resolved(_)
        | Expr::Constructor(_, None) => {}
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

fn collect_broad_effect_replacements(
    declared_effects: &[String],
    used_effects: &BTreeSet<String>,
) -> Vec<(String, Vec<String>)> {
    let declared_unique: BTreeSet<String> = declared_effects.iter().cloned().collect();
    let mut out = Vec::new();
    for declared in declared_unique {
        if declared.contains('.') {
            continue;
        }
        let prefix = format!("{}.", declared);
        let matched_children: Vec<String> = used_effects
            .iter()
            .filter(|used| used.starts_with(&prefix))
            .cloned()
            .collect();
        if !matched_children.is_empty() {
            out.push((declared, matched_children));
        }
    }
    out
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
            TopLevel::EffectSet { name, .. } => {
                out.insert(name.clone());
            }
            TopLevel::Verify(_) | TopLevel::Stmt(_) => {}
        }
    }
    out
}

fn collect_known_effect_symbols(fn_sigs: Option<&FnSigMap>) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    for builtin in ["Console", "Http", "Disk", "Tcp", "HttpServer"] {
        out.insert(builtin.to_string());
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
) -> bool {
    declared_symbols.contains(name) || known_effect_symbols.contains(name)
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
    let module_name = items.iter().find_map(|item| {
        if let TopLevel::Module(m) = item {
            Some(m.name.clone())
        } else {
            None
        }
    });

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
                    message: format!(
                        "Verify block '{}' must contain at least one case",
                        v.fn_name
                    ),
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
                                message: format!(
                                    "Verify block '{}' case #{} must call '{}' on the left side",
                                    v.fn_name,
                                    idx + 1,
                                    v.fn_name
                                ),
                            });
                            block_valid = false;
                        }
                    }
                    for (idx, (_left, right)) in v.cases.iter().enumerate() {
                        if verify_case_calls_target(right, &v.fn_name) {
                            errors.push(CheckFinding {
                                line: v.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                message: format!(
                                    "Verify block '{}' case #{} must not call '{}' on the right side",
                                    v.fn_name,
                                    idx + 1,
                                    v.fn_name
                                ),
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
                            message: format!(
                                "Verify law '{}.{}' resolves to effectful function '{}'; spec functions must be pure",
                                v.fn_name, law.name, named_fn.name
                            ),
                        });
                        block_valid = false;
                    } else if let Some(spec_ref) = canonical_spec_ref(&v.fn_name, law, sigs) {
                        spec_fns.insert(spec_ref.spec_fn_name);
                    } else {
                        warnings.push(CheckFinding {
                            line: v.line,
                            module: module_name.clone(),
                            file: source_file.map(|s| s.to_string()),
                            message: format!(
                                "Verify law '{}.{}' names pure function '{}' but the law body never calls it; use '{}' in the assertion or rename the law",
                                v.fn_name, law.name, named_fn.name, named_fn.name
                            ),
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
                        message: format!("Module '{}' has no intent block", m.name),
                    });
                }
            }
            TopLevel::FnDef(f) => {
                if f.desc.is_none() && fn_needs_desc(f) {
                    warnings.push(CheckFinding {
                        line: f.line,
                        module: module_name.clone(),
                        file: source_file.map(|s| s.to_string()),
                        message: format!("Function '{}' has no description (?)", f.name),
                    });
                }
                if let Some(sigs) = fn_sigs
                    && let Some((_, _, declared_effects)) = sigs.get(&f.name)
                    && !declared_effects.is_empty()
                {
                    let used_effects = collect_used_effects(f, sigs);
                    let broad_replacements =
                        collect_broad_effect_replacements(declared_effects, &used_effects);
                    let unused_effects: Vec<String> = declared_effects
                        .iter()
                        .filter(|declared| {
                            // A declared effect is "used" if it satisfies any used effect
                            // e.g. declared "Console" satisfies used "Console.print"
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
                            used_effects.into_iter().collect::<Vec<_>>().join(", ")
                        };
                        warnings.push(CheckFinding {
                            line: f.line,
                            module: module_name.clone(),
                            file: source_file.map(|s| s.to_string()),
                            message: format!(
                                "Function '{}' declares unused effect(s): {} (used: {})",
                                f.name,
                                unused_effects.join(", "),
                                used
                            ),
                        });
                    }
                    for (parent, children) in broad_replacements {
                        warnings.push(CheckFinding {
                                    line: f.line,
                                    module: module_name.clone(),
                                    file: source_file.map(|s| s.to_string()),
                                    message: format!(
                                        "Function '{}' declares broad effect '{}'. Prefer granular sub-effects: {}",
                                        f.name,
                                        parent,
                                        children.join(", ")
                                    ),
                                });
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
                        message: format!("Function '{}' has no verify block", f.name),
                    });
                }
            }
            TopLevel::Decision(d) => {
                if let DecisionImpact::Symbol(name) = &d.chosen
                    && !decision_symbol_known(name, &declared_symbols, &known_effect_symbols)
                {
                    errors.push(CheckFinding {
                            line: d.line,
                            module: module_name.clone(),
                            file: source_file.map(|s| s.to_string()),
                            message: format!(
                                "Decision '{}' references unknown chosen symbol '{}'. Use quoted string for semantic chosen value.",
                                d.name, name
                            ),
                        });
                }
                for rejected in &d.rejected {
                    if let DecisionImpact::Symbol(name) = rejected
                        && !decision_symbol_known(name, &declared_symbols, &known_effect_symbols)
                    {
                        errors.push(CheckFinding {
                                line: d.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                message: format!(
                                    "Decision '{}' references unknown rejected symbol '{}'. Use quoted string for semantic rejected value.",
                                    d.name, name
                                ),
                            });
                    }
                }
                for impact in &d.impacts {
                    if let DecisionImpact::Symbol(name) = impact
                        && !decision_symbol_known(name, &declared_symbols, &known_effect_symbols)
                    {
                        errors.push(CheckFinding {
                                line: d.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                message: format!(
                                    "Decision '{}' references unknown impact symbol '{}'. Use quoted string for semantic impact.",
                                    d.name, name
                                ),
                            });
                    }
                }
            }
            _ => {}
        }
    }

    ModuleCheckFindings { errors, warnings }
}

pub fn expr_to_str(expr: &crate::ast::Expr) -> String {
    use crate::ast::Expr;
    use crate::ast::Literal;

    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Str(s) => format!("\"{}\"", s),
            Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            Literal::Unit => "Unit".to_string(),
        },
        Expr::Ident(name) => name.clone(),
        Expr::FnCall(fn_expr, args) => {
            let fn_str = expr_to_str(fn_expr);
            let args_str = args.iter().map(expr_to_str).collect::<Vec<_>>().join(", ");
            format!("{}({})", fn_str, args_str)
        }
        Expr::Constructor(name, arg) => match arg {
            None => name.clone(),
            Some(a) => format!("{}({})", name, expr_to_str(a)),
        },
        Expr::BinOp(op, left, right) => {
            use crate::ast::BinOp;
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
            format!("{} {} {}", expr_to_str(left), op_str, expr_to_str(right))
        }
        Expr::InterpolatedStr(parts) => {
            use crate::ast::StrPart;
            let mut inner = String::new();
            for part in parts {
                match part {
                    StrPart::Literal(s) => inner.push_str(s),
                    StrPart::Parsed(e) => {
                        inner.push('{');
                        inner.push_str(&expr_to_str(e));
                        inner.push('}');
                    }
                }
            }
            format!("\"{}\"", inner)
        }
        Expr::List(elements) => {
            let parts: Vec<String> = elements.iter().map(expr_to_str).collect();
            format!("[{}]", parts.join(", "))
        }
        Expr::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(expr_to_str).collect();
            format!("({})", parts.join(", "))
        }
        Expr::MapLiteral(entries) => {
            let parts = entries
                .iter()
                .map(|(key, value)| format!("{} => {}", expr_to_str(key), expr_to_str(value)))
                .collect::<Vec<_>>();
            format!("{{{}}}", parts.join(", "))
        }
        Expr::ErrorProp(inner) => format!("{}?", expr_to_str(inner)),
        Expr::Attr(obj, field) => format!("{}.{}", expr_to_str(obj), field),
        Expr::RecordCreate { type_name, fields } => {
            let flds: Vec<String> = fields
                .iter()
                .map(|(name, expr)| format!("{} = {}", name, expr_to_str(expr)))
                .collect();
            format!("{}({})", type_name, flds.join(", "))
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            let upds: Vec<String> = updates
                .iter()
                .map(|(name, expr)| format!("{} = {}", name, expr_to_str(expr)))
                .collect();
            format!(
                "{}.update({}, {})",
                type_name,
                expr_to_str(base),
                upds.join(", ")
            )
        }
        Expr::TailCall(boxed) => {
            let (target, args) = boxed.as_ref();
            let a = args.iter().map(expr_to_str).collect::<Vec<_>>().join(", ");
            format!("<tail-call:{}>({})", target, a)
        }
        Expr::Resolved(_) => "<resolved>".to_string(),
        Expr::Match { subject, arms, .. } => {
            let s = expr_to_str(subject);
            let arms_str: Vec<String> = arms
                .iter()
                .map(|arm| format!("{:?} -> {}", arm.pattern, expr_to_str(&arm.body)))
                .collect();
            format!("match {} {}", s, arms_str.join(", "))
        }
    }
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
    ! [Console, Http]
    Console.print(x)
"#,
        );
        let tc = crate::types::checker::run_type_check_full(&items, None);
        assert!(
            tc.errors.is_empty(),
            "unexpected type errors: {:?}",
            tc.errors
        );
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            findings.warnings.iter().any(|w| {
                w.message.contains("declares unused effect(s)")
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
    Console.print(x)
"#,
        );
        let tc = crate::types::checker::run_type_check_full(&items, None);
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
                .any(|w| w.message.contains("declares unused effect(s)")),
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
    fn warns_on_broad_effects_when_sub_effects_are_used() {
        let items = parse_items(
            r#"
fn fetch(url: String) -> Result<HttpResponse, String>
    ! [Http]
    Http.get(url)
"#,
        );
        let tc = crate::types::checker::run_type_check_full(&items, None);
        assert!(
            tc.errors.is_empty(),
            "unexpected type errors: {:?}",
            tc.errors
        );
        let findings = check_module_intent_with_sigs(&items, Some(&tc.fn_sigs));
        assert!(
            findings.warnings.iter().any(|w| {
                w.message.contains("declares broad effect 'Http'") && w.message.contains("Http.get")
            }),
            "expected broad-effect warning, got errors={:?}, warnings={:?}",
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
                    .contains("Verify block 'add1' case #1 must not call 'add1' on the right side")
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
            !findings
                .errors
                .iter()
                .any(|e| e.message.contains("must not call 'add1' on the right side")),
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
        let tc = crate::types::checker::run_type_check_full(&items, None);
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
        let tc = crate::types::checker::run_type_check_full(&items, None);
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
        let tc = crate::types::checker::run_type_check_full(&items, None);
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
        let tc = crate::types::checker::run_type_check_full(&items, None);
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
        let tc = crate::types::checker::run_type_check_full(&items, None);
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
    fn coverage_warns_when_result_verify_has_no_err_example() {
        let items = parse_items(
            r#"
fn mayFail(n: Int) -> Result<Int, String>
    match n
        0 -> Result.Err("zero")
        _ -> Result.Ok(n)

verify mayFail
    mayFail(1) => Result.Ok(1)
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message == "verify examples for mayFail do not include any Result.Err case"
            }),
            "expected missing-err warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_warns_when_option_verify_has_no_none_example() {
        let items = parse_items(
            r#"
fn maybe(n: Int) -> Option<Int>
    match n
        0 -> Option.None
        _ -> Option.Some(n)

verify maybe
    maybe(1) => Option.Some(1)
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message == "verify examples for maybe do not include any Option.None case"
            }),
            "expected missing-none warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_warns_when_enum_match_examples_cover_subset_of_constructors() {
        let items = parse_items(
            r#"
type Input
    Help
    Exit
    Echo(String)

fn dispatch(input: Input) -> String
    match input
        Input.Help -> "help"
        Input.Exit -> "exit"
        Input.Echo(value) -> value

verify dispatch
    dispatch(Input.Help) => "help"
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings
                .iter()
                .any(|w| w.message == "verify examples for dispatch cover 1/3 enum constructors"),
            "expected enum-coverage warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_does_not_warn_when_enum_and_output_examples_are_present() {
        let items = parse_items(
            r#"
type Input
    Help
    Exit

fn run(input: Input) -> Result<String, String>
    match input
        Input.Help -> Result.Ok("help")
        Input.Exit -> Result.Err("exit")

verify run
    run(Input.Help) => Result.Ok("help")
    run(Input.Exit) => Result.Err("exit")
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(warnings.is_empty(), "unexpected warnings: {:?}", warnings);
    }

    #[test]
    fn merge_verify_blocks_coalesces_cases_by_function() {
        let items = parse_items(
            r#"
fn f(x: Int) -> Int
    x

verify f
    f(1) => 1

verify f
    f(2) => 2
"#,
        );
        let merged = merge_verify_blocks(&items);
        assert_eq!(merged.len(), 1);
        assert_eq!(merged[0].fn_name, "f");
        assert_eq!(merged[0].cases.len(), 2);
    }

    #[test]
    fn merge_verify_blocks_keeps_law_blocks_separate() {
        let items = parse_items(
            r#"
fn f(x: Int) -> Int
    x

verify f
    f(1) => 1

verify f law l1
    given x: Int = [1]
    x => x

verify f law l2
    given x: Int = [2]
    x => x

verify f
    f(2) => 2
"#,
        );
        let merged = merge_verify_blocks(&items);
        assert_eq!(merged.len(), 3);
        assert!(matches!(merged[0].kind, VerifyKind::Cases));
        assert_eq!(merged[0].cases.len(), 2);
        assert!(matches!(merged[1].kind, VerifyKind::Law(_)));
        assert!(matches!(merged[2].kind, VerifyKind::Law(_)));
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
            findings.errors.iter().any(|e| e
                .message
                .contains("Decision 'D' references unknown impact symbol 'missingThing'")),
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
                .any(|e| e.message.contains("references unknown impact symbol")),
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
        let tc = crate::types::checker::run_type_check_full(&items, None);
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
    fn decision_effect_alias_impact_is_allowed() {
        let items = parse_items(
            r#"
module M
    intent =
        "x"

effects AppIO = [Console, Disk]

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
            !findings.errors.iter().any(|e| e
                .message
                .contains("references unknown impact symbol 'AppIO'")),
            "did not expect AppIO impact error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }
}
