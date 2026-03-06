use colored::Colorize;
use std::collections::BTreeSet;

use crate::ast::{
    DecisionBlock, DecisionImpact, Expr, FnBody, FnDef, Stmt, TopLevel, VerifyBlock,
    VerifyGivenDomain, VerifyKind,
};
use crate::interpreter::{Interpreter, aver_repr};
use crate::types::{Type, parse_type_str_strict};
use crate::value::{RuntimeError, Value};

pub struct VerifyResult {
    #[allow(dead_code)]
    pub fn_name: String,
    pub passed: usize,
    pub failed: usize,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VerifyOutputShape {
    BoolTrue,
    BoolFalse,
    Some,
    None,
    Ok,
    Err,
    Variant(String),
}

impl VerifyOutputShape {
    fn display(&self) -> String {
        match self {
            VerifyOutputShape::BoolTrue => "true".to_string(),
            VerifyOutputShape::BoolFalse => "false".to_string(),
            VerifyOutputShape::Some => "Option.Some".to_string(),
            VerifyOutputShape::None => "Option.None".to_string(),
            VerifyOutputShape::Ok => "Result.Ok".to_string(),
            VerifyOutputShape::Err => "Result.Err".to_string(),
            VerifyOutputShape::Variant(name) => name.clone(),
        }
    }
}

struct VerifyShapeContract {
    return_type: Type,
    expected: Vec<VerifyOutputShape>,
    seen: std::collections::HashSet<VerifyOutputShape>,
}

impl VerifyShapeContract {
    fn observe(&mut self, value: &Value) {
        if let Some(shape) = observed_output_shape_for_type(&self.return_type, value) {
            self.seen.insert(shape);
        }
    }

    fn observe_shape(&mut self, shape: VerifyOutputShape) {
        self.seen.insert(shape);
    }

    fn missing(&self) -> Vec<VerifyOutputShape> {
        self.expected
            .iter()
            .filter(|shape| !self.seen.contains(*shape))
            .cloned()
            .collect()
    }
}

fn build_verify_shape_contract(
    block: &VerifyBlock,
    interp: &Interpreter,
) -> Option<VerifyShapeContract> {
    let fn_val = interp.lookup(&block.fn_name).ok()?;
    let Value::Fn {
        return_type, body, ..
    } = fn_val
    else {
        return None;
    };
    let ret_ty = parse_type_str_strict(&return_type).ok()?;

    let all_shapes = expected_output_shapes_for_type(&ret_ty, interp)?;
    let mut declared_shapes = std::collections::HashSet::new();
    collect_declared_output_shapes_from_body(body.as_ref(), &ret_ty, &mut declared_shapes);
    let expected: Vec<VerifyOutputShape> = all_shapes
        .into_iter()
        .filter(|shape| declared_shapes.contains(shape))
        .collect();
    if expected.len() < 2 {
        return None;
    }

    Some(VerifyShapeContract {
        return_type: ret_ty,
        expected,
        seen: std::collections::HashSet::new(),
    })
}

fn expected_output_shapes_for_type(
    ty: &Type,
    interp: &Interpreter,
) -> Option<Vec<VerifyOutputShape>> {
    match ty {
        Type::Bool => Some(vec![
            VerifyOutputShape::BoolTrue,
            VerifyOutputShape::BoolFalse,
        ]),
        Type::Option(_) => Some(vec![VerifyOutputShape::Some, VerifyOutputShape::None]),
        Type::Result(_, _) => Some(vec![VerifyOutputShape::Ok, VerifyOutputShape::Err]),
        Type::Named(type_name) => {
            let ns = interp.lookup(type_name).ok()?;
            let Value::Namespace { members, .. } = ns else {
                return None;
            };

            let ctor_prefix = format!("__ctor:{}:", type_name);
            let mut variants = std::collections::BTreeSet::new();

            for (member_name, member_value) in members {
                match member_value {
                    Value::Variant { type_name: t, .. } if t == type_name.as_str() => {
                        variants.insert(member_name);
                    }
                    Value::Builtin(builtin_name) if builtin_name.starts_with(&ctor_prefix) => {
                        variants.insert(member_name);
                    }
                    _ => {}
                }
            }

            if variants.is_empty() {
                return None;
            }

            Some(
                variants
                    .into_iter()
                    .map(VerifyOutputShape::Variant)
                    .collect(),
            )
        }
        _ => None,
    }
}

fn observed_output_shape_for_type(ty: &Type, value: &Value) -> Option<VerifyOutputShape> {
    match ty {
        Type::Bool => match value {
            Value::Bool(true) => Some(VerifyOutputShape::BoolTrue),
            Value::Bool(false) => Some(VerifyOutputShape::BoolFalse),
            _ => None,
        },
        Type::Option(_) => match value {
            Value::Some(_) => Some(VerifyOutputShape::Some),
            Value::None => Some(VerifyOutputShape::None),
            _ => None,
        },
        Type::Result(_, _) => match value {
            Value::Ok(_) => Some(VerifyOutputShape::Ok),
            Value::Err(_) => Some(VerifyOutputShape::Err),
            _ => None,
        },
        Type::Named(type_name) => match value {
            Value::Variant {
                type_name: actual_type,
                variant,
                ..
            } if actual_type == type_name => Some(VerifyOutputShape::Variant(variant.clone())),
            _ => None,
        },
        _ => None,
    }
}

fn collect_declared_output_shapes_from_body(
    body: &FnBody,
    ret_ty: &Type,
    out: &mut std::collections::HashSet<VerifyOutputShape>,
) {
    match body {
        FnBody::Expr(expr) => collect_declared_output_shapes_from_tail_expr(expr, ret_ty, out),
        FnBody::Block(stmts) => {
            if let Some(last) = stmts.last() {
                match last {
                    Stmt::Expr(expr) => {
                        collect_declared_output_shapes_from_tail_expr(expr, ret_ty, out)
                    }
                    Stmt::Binding(_, _, _) => {}
                }
            }
        }
    }
}

fn collect_declared_output_shapes_from_tail_expr(
    expr: &Expr,
    ret_ty: &Type,
    out: &mut std::collections::HashSet<VerifyOutputShape>,
) {
    match expr {
        Expr::Match { arms, .. } => {
            for arm in arms {
                collect_declared_output_shapes_from_tail_expr(&arm.body, ret_ty, out);
            }
        }
        _ => {
            if let Some(shape) = declared_output_shape_from_expr(ret_ty, expr) {
                out.insert(shape);
            }
        }
    }
}

fn declared_output_shape_from_expr(ret_ty: &Type, expr: &Expr) -> Option<VerifyOutputShape> {
    match ret_ty {
        Type::Bool => match expr {
            Expr::Literal(crate::ast::Literal::Bool(true)) => Some(VerifyOutputShape::BoolTrue),
            Expr::Literal(crate::ast::Literal::Bool(false)) => Some(VerifyOutputShape::BoolFalse),
            _ => None,
        },
        Type::Option(_) => match expr {
            Expr::FnCall(callee, _) => match dotted_name(callee) {
                Some(path) if path == "Option.Some" => Some(VerifyOutputShape::Some),
                _ => None,
            },
            _ => match dotted_name(expr) {
                Some(path) if path == "Option.None" => Some(VerifyOutputShape::None),
                _ => None,
            },
        },
        Type::Result(_, _) => match expr {
            Expr::FnCall(callee, _) => match dotted_name(callee) {
                Some(path) if path == "Result.Ok" => Some(VerifyOutputShape::Ok),
                Some(path) if path == "Result.Err" => Some(VerifyOutputShape::Err),
                _ => None,
            },
            _ => None,
        },
        Type::Named(type_name) => {
            let prefix = format!("{}.", type_name);
            match expr {
                Expr::Attr(_, _) => {
                    let path = dotted_name(expr)?;
                    let variant = path.strip_prefix(&prefix)?;
                    if variant.is_empty() {
                        None
                    } else {
                        Some(VerifyOutputShape::Variant(variant.to_string()))
                    }
                }
                Expr::FnCall(callee, _) => {
                    let path = dotted_name(callee)?;
                    let variant = path.strip_prefix(&prefix)?;
                    if variant.is_empty() {
                        None
                    } else {
                        Some(VerifyOutputShape::Variant(variant.to_string()))
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
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

fn verify_case_uses_error_prop_on_target(expr: &Expr, fn_name: &str) -> bool {
    match expr {
        Expr::ErrorProp(inner) => {
            verify_case_calls_target(inner, fn_name)
                || verify_case_uses_error_prop_on_target(inner, fn_name)
        }
        Expr::FnCall(callee, args) => {
            verify_case_uses_error_prop_on_target(callee, fn_name)
                || args
                    .iter()
                    .any(|arg| verify_case_uses_error_prop_on_target(arg, fn_name))
        }
        Expr::BinOp(_, left, right) => {
            verify_case_uses_error_prop_on_target(left, fn_name)
                || verify_case_uses_error_prop_on_target(right, fn_name)
        }
        Expr::Match { subject, arms, .. } => {
            verify_case_uses_error_prop_on_target(subject, fn_name)
                || arms
                    .iter()
                    .any(|arm| verify_case_uses_error_prop_on_target(&arm.body, fn_name))
        }
        Expr::Constructor(_, Some(inner)) => verify_case_uses_error_prop_on_target(inner, fn_name),
        Expr::List(elems) => elems
            .iter()
            .any(|elem| verify_case_uses_error_prop_on_target(elem, fn_name)),
        Expr::Tuple(items) => items
            .iter()
            .any(|item| verify_case_uses_error_prop_on_target(item, fn_name)),
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            verify_case_uses_error_prop_on_target(k, fn_name)
                || verify_case_uses_error_prop_on_target(v, fn_name)
        }),
        Expr::Attr(obj, _) => verify_case_uses_error_prop_on_target(obj, fn_name),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, expr)| verify_case_uses_error_prop_on_target(expr, fn_name)),
        Expr::RecordUpdate { base, updates, .. } => {
            verify_case_uses_error_prop_on_target(base, fn_name)
                || updates
                    .iter()
                    .any(|(_, expr)| verify_case_uses_error_prop_on_target(expr, fn_name))
        }
        Expr::TailCall(boxed) => {
            boxed.0 == fn_name
                || boxed
                    .1
                    .iter()
                    .any(|arg| verify_case_uses_error_prop_on_target(arg, fn_name))
        }
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::InterpolatedStr(_)
        | Expr::Resolved(_)
        | Expr::Constructor(_, None) => false,
    }
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
    let mut failures = Vec::new();
    let is_law = matches!(block.kind, VerifyKind::Law(_));
    let mut shape_contract = if is_law {
        None
    } else {
        build_verify_shape_contract(block, interp)
    };

    match &block.kind {
        VerifyKind::Cases => println!("Verify: {}", block.fn_name.cyan()),
        VerifyKind::Law(law) => {
            println!("Verify: {} law {}", block.fn_name.cyan(), law.name.cyan());
            for given in &law.givens {
                println!(
                    "  {} {}: {} = {}",
                    "given".dimmed(),
                    given.name,
                    given.type_name,
                    verify_given_domain_to_str(&given.domain)
                );
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
    if !is_law {
        interp.start_verify_match_coverage(&block.fn_name);
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

        let left_result = interp.eval_expr(left_expr);
        let right_result = interp.eval_expr(right_expr);

        if let Ok(left_val) = &left_result
            && let Some(contract) = shape_contract.as_mut()
        {
            contract.observe(left_val);
        }
        if verify_case_uses_error_prop_on_target(left_expr, &block.fn_name)
            && let Some(contract) = shape_contract.as_mut()
            && matches!(contract.return_type, Type::Result(_, _))
        {
            match &left_result {
                Ok(_) => contract.observe_shape(VerifyOutputShape::Ok),
                Err(RuntimeError::ErrProp(_)) => contract.observe_shape(VerifyOutputShape::Err),
                Err(_) => {}
            }
        }

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

    if !is_law {
        let coverage_misses = interp.finish_verify_match_coverage();
        for miss in coverage_misses {
            failed += 1;
            let missing_1_based: Vec<String> = miss
                .missing_arms
                .iter()
                .map(|idx| (idx + 1).to_string())
                .collect();
            let msg = format!(
                "match at line {} missing covered arm(s): {} (of {})",
                miss.line,
                missing_1_based.join(", "),
                miss.total_arms
            );
            println!("  {} {}", "✗".red(), msg);
            failures.push((
                format!("match-coverage:{}", miss.line),
                format!("all {} arms covered", miss.total_arms),
                msg,
            ));
        }
    }

    if let Some(contract) = shape_contract {
        let missing = contract.missing();
        if !missing.is_empty() {
            failed += 1;
            let missing_labels: Vec<String> =
                missing.iter().map(VerifyOutputShape::display).collect();
            let expected_labels: Vec<String> = contract
                .expected
                .iter()
                .map(VerifyOutputShape::display)
                .collect();
            let msg = format!(
                "missing output shape(s) for {}: {}",
                contract.return_type.display(),
                missing_labels.join(", ")
            );
            println!("  {} {}", "✗".red(), msg);
            failures.push((
                format!("shape-coverage:{}", block.fn_name),
                format!("covered output shapes: {}", expected_labels.join(", ")),
                msg,
            ));
        }
    }

    let total = passed + failed;
    if is_law && failed == 0 {
        println!(
            "  {} all {} generated case(s) passed",
            "✓".green(),
            block.cases.len()
        );
    }
    if failed == 0 {
        println!("  {}", format!("{}/{} passed", passed, total).green());
    } else {
        println!("  {}", format!("{}/{} passed", passed, total).red());
    }

    VerifyResult {
        fn_name: block.fn_name.clone(),
        passed,
        failed,
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

    match f.body.as_ref() {
        FnBody::Expr(expr) => expr_is_passthrough(expr, &param_names),
        FnBody::Block(stmts) => {
            if stmts.len() != 1 {
                return false;
            }
            match &stmts[0] {
                Stmt::Expr(expr) => expr_is_passthrough(expr, &param_names),
                Stmt::Binding(_, _, _) => false,
            }
        }
    }
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
    match f.body.as_ref() {
        FnBody::Expr(expr) => collect_used_effects_expr(expr, fn_sigs, &mut used),
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                        collect_used_effects_expr(expr, fn_sigs, &mut used)
                    }
                }
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
