use colored::Colorize;

use crate::ast::{DecisionBlock, Expr, FnBody, FnDef, Stmt, TopLevel, VerifyBlock, VerifyKind};
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
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
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
        Expr::Pipe(left, right) | Expr::BinOp(_, left, right) => {
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
            println!("Verify: {} law {}", block.fn_name.cyan(), law.name.cyan())
        }
    }
    if !is_law {
        interp.start_verify_match_coverage(&block.fn_name);
    }

    for (left_expr, right_expr) in &block.cases {
        let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));

        let left_result = interp.eval_expr(left_expr);
        let right_result = interp.eval_expr(right_expr);

        if let Ok(left_val) = &left_result {
            if let Some(contract) = shape_contract.as_mut() {
                contract.observe(left_val);
            }
        }
        if verify_case_uses_error_prop_on_target(left_expr, &block.fn_name) {
            if let Some(contract) = shape_contract.as_mut() {
                if matches!(contract.return_type, Type::Result(_, _)) {
                    match &left_result {
                        Ok(_) => contract.observe_shape(VerifyOutputShape::Ok),
                        Err(RuntimeError::ErrProp(_)) => {
                            contract.observe_shape(VerifyOutputShape::Err)
                        }
                        Err(_) => {}
                    }
                }
            }
        }

        match (left_result, right_result) {
            (Ok(left_val), Ok(right_val)) => {
                if interp.aver_eq(&left_val, &right_val) {
                    passed += 1;
                    println!("  {} {}", "✓".green(), case_str);
                } else {
                    failed += 1;
                    println!("  {} {}", "✗".red(), case_str);
                    let expected = aver_repr(&right_val);
                    let actual = aver_repr(&left_val);
                    println!("      expected: {}", expected);
                    println!("      got:      {}", actual);
                    failures.push((case_str, expected, actual));
                }
            }
            // `?` in a verify case hitting Err produces ErrProp — treat as test failure.
            (Err(RuntimeError::ErrProp(err_val)), _) | (_, Err(RuntimeError::ErrProp(err_val))) => {
                failed += 1;
                println!("  {} {}", "✗".red(), case_str);
                println!("      ? hit Result.Err({})", aver_repr(&err_val));
                failures.push((
                    case_str,
                    String::new(),
                    format!("? hit Result.Err({})", aver_repr(&err_val)),
                ));
            }
            (Err(e), _) | (_, Err(e)) => {
                failed += 1;
                println!("  {} {}", "✗".red(), case_str);
                println!("      error: {}", e);
                failures.push((case_str, String::new(), format!("ERROR: {}", e)));
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
        Expr::Pipe(left_expr, right_expr) => {
            pipe_target_is_target(right_expr, fn_name)
                || verify_case_calls_target(left_expr, fn_name)
                || verify_case_calls_target(right_expr, fn_name)
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

fn pipe_target_is_target(target: &Expr, fn_name: &str) -> bool {
    match target {
        Expr::Ident(name) => name == fn_name,
        Expr::FnCall(callee, _) => callee_is_target(callee, fn_name),
        _ => false,
    }
}

pub fn check_module_intent(items: &[TopLevel]) -> ModuleCheckFindings {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    let mut verified_fns: std::collections::HashSet<&str> = std::collections::HashSet::new();
    let mut empty_verify_fns: std::collections::HashSet<&str> = std::collections::HashSet::new();
    let mut invalid_verify_fns: std::collections::HashSet<&str> = std::collections::HashSet::new();
    for item in items {
        if let TopLevel::Verify(v) = item {
            if v.cases.is_empty() {
                errors.push(format!(
                    "Verify block '{}' must contain at least one case",
                    v.fn_name
                ));
                empty_verify_fns.insert(v.fn_name.as_str());
            } else {
                let mut block_valid = true;
                if matches!(v.kind, VerifyKind::Cases) {
                    for (idx, (left, _right)) in v.cases.iter().enumerate() {
                        if !verify_case_calls_target(left, &v.fn_name) {
                            errors.push(format!(
                                "line {}: Verify block '{}' case #{} must call '{}' on the left side",
                                v.line,
                                v.fn_name,
                                idx + 1,
                                v.fn_name
                            ));
                            block_valid = false;
                        }
                    }
                    for (idx, (_left, right)) in v.cases.iter().enumerate() {
                        if verify_case_calls_target(right, &v.fn_name) {
                            errors.push(format!(
                                "line {}: Verify block '{}' case #{} must not call '{}' on the right side",
                                v.line,
                                v.fn_name,
                                idx + 1,
                                v.fn_name
                            ));
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
                    warnings.push(format!("Module '{}' has no intent block", m.name));
                }
            }
            TopLevel::FnDef(f) => {
                if f.desc.is_none() && fn_needs_desc(f) {
                    warnings.push(format!("Function '{}' has no description (?)", f.name));
                }
                if fn_needs_verify(f)
                    && !verified_fns.contains(f.name.as_str())
                    && !empty_verify_fns.contains(f.name.as_str())
                    && !invalid_verify_fns.contains(f.name.as_str())
                {
                    errors.push(format!("Function '{}' has no verify block", f.name));
                }
            }
            _ => {}
        }
    }

    ModuleCheckFindings { errors, warnings }
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
    = Console.print(x)
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings
                .warnings
                .iter()
                .any(|w| w.contains("no verify block"))
                && !findings
                    .errors
                    .iter()
                    .any(|e| e.contains("no verify block")),
            "unexpected findings: errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn no_verify_warning_for_trivial_passthrough_wrapper() {
        let items = parse_items(
            r#"
fn passthrough(x: Int) -> Int
    = inner(x)
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings
                .warnings
                .iter()
                .any(|w| w.contains("no verify block"))
                && !findings
                    .errors
                    .iter()
                    .any(|e| e.contains("no verify block")),
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
    = x + 1
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings
                .errors
                .iter()
                .any(|e| e == "Function 'add1' has no verify block"),
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
    = x + 1

verify add1
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings
                .errors
                .iter()
                .any(|e| e == "Verify block 'add1' must contain at least one case"),
            "expected empty verify error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e == "Function 'add1' has no verify block"),
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
    = x + 1

verify add1
    true => true
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings.errors.iter().any(|e| {
                e.contains("Verify block 'add1' case #1 must call 'add1' on the left side")
            }),
            "expected verify-case-call error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e == "Function 'add1' has no verify block"),
            "expected no duplicate missing-verify error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn verify_case_pipe_into_target_is_allowed() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    = x + 1

verify add1
    41 |> add1 => 42
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            !findings.errors.iter().any(|e| e.contains("case #")),
            "did not expect verify-case-call error, got errors={:?}, warnings={:?}",
            findings.errors,
            findings.warnings
        );
    }

    #[test]
    fn verify_case_must_not_call_verified_function_on_right_side() {
        let items = parse_items(
            r#"
fn add1(x: Int) -> Int
    = x + 1

verify add1
    add1(1) => add1(1)
"#,
        );
        let findings = check_module_intent(&items);
        assert!(
            findings.errors.iter().any(|e| {
                e.contains("Verify block 'add1' case #1 must not call 'add1' on the right side")
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
    = x + 1

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
                .any(|e| e.contains("must call 'add1' on the left side")),
            "did not expect lhs-call heuristic for law verify, got errors={:?}",
            findings.errors
        );
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e.contains("must not call 'add1' on the right side")),
            "did not expect rhs-call heuristic for law verify, got errors={:?}",
            findings.errors
        );
        assert!(
            !findings
                .errors
                .iter()
                .any(|e| e == "Function 'add1' has no verify block"),
            "law verify should satisfy verify requirement, got errors={:?}",
            findings.errors
        );
    }

    #[test]
    fn merge_verify_blocks_coalesces_cases_by_function() {
        let items = parse_items(
            r#"
fn f(x: Int) -> Int
    = x

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
    = x

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
        Expr::Pipe(left, right) => format!("{} |> {}", expr_to_str(left), expr_to_str(right)),
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
