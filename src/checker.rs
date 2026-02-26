use colored::Colorize;

use crate::ast::{DecisionBlock, FnDef, TopLevel, VerifyBlock};
use crate::interpreter::{aver_repr, Interpreter};
use crate::value::RuntimeError;

pub struct VerifyResult {
    #[allow(dead_code)]
    pub fn_name: String,
    pub passed: usize,
    pub failed: usize,
    #[allow(dead_code)]
    pub failures: Vec<(String, String, String)>, // (expr_src, expected, actual)
}

pub fn run_verify(block: &VerifyBlock, interp: &mut Interpreter) -> VerifyResult {
    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();

    println!("Verify: {}", block.fn_name.cyan());

    for (left_expr, right_expr) in &block.cases {
        let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));

        let left_result = interp.eval_expr(left_expr);
        let right_result = interp.eval_expr(right_expr);

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

/// Returns true if a function requires a ? description annotation.
/// All functions except main() require one.
fn fn_needs_desc(f: &FnDef) -> bool {
    f.name != "main"
}

pub fn check_module_intent(items: &[TopLevel]) -> Vec<String> {
    let mut warnings = Vec::new();

    let verified_fns: std::collections::HashSet<&str> = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::Verify(v) = item {
                Some(v.fn_name.as_str())
            } else {
                None
            }
        })
        .collect();

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
                if f.name != "main" && !verified_fns.contains(f.name.as_str()) {
                    warnings.push(format!("Function '{}' has no verify block", f.name));
                }
            }
            _ => {}
        }
    }

    warnings
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
        Expr::TypeAscription(inner, ty) => format!("{}: {}", expr_to_str(inner), ty),
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
        Expr::TailCall(boxed) => {
            let (target, args) = boxed.as_ref();
            let a = args.iter().map(expr_to_str).collect::<Vec<_>>().join(", ");
            format!("<tail-call:{}>({})", target, a)
        }
        Expr::Resolved(_) => "<resolved>".to_string(),
        Expr::Match(subject, arms) => {
            let s = expr_to_str(subject);
            let arms_str: Vec<String> = arms
                .iter()
                .map(|arm| format!("{:?} -> {}", arm.pattern, expr_to_str(&arm.body)))
                .collect();
            format!("match {} {}", s, arms_str.join(", "))
        }
    }
}
