use std::collections::BTreeSet;

use crate::ast::{Expr, FnDef, Literal, Pattern, TopLevel, VerifyBlock};
use crate::call_graph::find_recursive_fns;
use crate::types::Type;
use crate::types::parse_type_str_strict;

use super::coverage_flow::flow_covered_fns;
use super::{
    CheckFinding, collect_target_call_args, constructor_tag_from_expr, expr_is_bool_case,
    expr_is_empty_list_case, expr_is_empty_string_case, expr_is_int_literal_case,
    expr_is_non_empty_list_case, expr_is_option_none_case, expr_is_option_some_case,
    expr_is_result_err_case, expr_is_result_ok_case, local_sum_type_constructors,
    merge_verify_blocks, module_name_for_items, verify_case_unwraps_target,
    verify_cases_block_is_well_formed,
};

fn direct_match_target(f: &FnDef) -> Option<(usize, &[crate::ast::MatchArm])> {
    let Expr::Match { subject, arms, .. } = &f.body.tail_expr()?.node else {
        return None;
    };
    let Expr::Ident(subject_name) = &subject.node else {
        return None;
    };
    let param_index = f.params.iter().position(|(name, _)| name == subject_name)?;
    Some((param_index, arms.as_slice()))
}

fn enum_match_coverage_target(f: &FnDef) -> Option<(usize, Vec<String>)> {
    let (param_index, arms) = direct_match_target(f)?;
    let constructors: BTreeSet<String> = arms
        .iter()
        .filter_map(|arm| super::constructor_tag_from_pattern(&arm.pattern))
        .collect();
    if constructors.is_empty() {
        None
    } else {
        Some((param_index, constructors.into_iter().collect()))
    }
}

fn bool_match_coverage_target(f: &FnDef) -> Option<usize> {
    let (param_index, arms) = direct_match_target(f)?;
    let (_, param_ty) = f.params.get(param_index)?;
    let Ok(Type::Bool) = parse_type_str_strict(param_ty) else {
        return None;
    };
    arms.iter()
        .any(|arm| matches!(arm.pattern, Pattern::Literal(Literal::Bool(_))))
        .then_some(param_index)
}

fn list_match_coverage_target(f: &FnDef) -> Option<usize> {
    let (param_index, arms) = direct_match_target(f)?;
    let (_, param_ty) = f.params.get(param_index)?;
    let Ok(Type::List(_)) = parse_type_str_strict(param_ty) else {
        return None;
    };
    arms.iter()
        .any(|arm| matches!(arm.pattern, Pattern::EmptyList | Pattern::Cons(_, _)))
        .then_some(param_index)
}

/// The return-shape gaps in one verify block, as diagnostic messages.
///
/// `None` when the return type carries no shape this lint enumerates. That is
/// distinct from `Some(vec![])`: a function with no shaped return has no arms,
/// so its examples cannot discharge a callee's obligation either.
pub(super) fn return_shape_gaps(
    f: &FnDef,
    block: &VerifyBlock,
    items: &[TopLevel],
) -> Option<Vec<String>> {
    let name = block.fn_name.as_str();
    let mut gaps = Vec::new();
    match parse_type_str_strict(&f.return_type).ok()? {
        Type::Result(_, _) => {
            if !block.cases.iter().any(|(left, right)| {
                expr_is_result_ok_case(right) || verify_case_unwraps_target(left, name)
            }) {
                gaps.push(format!(
                    "verify examples for {} do not include any Result.Ok case",
                    name
                ));
            }
            if !block
                .cases
                .iter()
                .any(|(_, right)| expr_is_result_err_case(right))
            {
                gaps.push(format!(
                    "verify examples for {} do not include any Result.Err case",
                    name
                ));
            }
        }
        Type::Option(_) => {
            if !block
                .cases
                .iter()
                .any(|(_, right)| expr_is_option_some_case(right))
            {
                gaps.push(format!(
                    "verify examples for {} do not include any Option.Some case",
                    name
                ));
            }
            if !block
                .cases
                .iter()
                .any(|(_, right)| expr_is_option_none_case(right))
            {
                gaps.push(format!(
                    "verify examples for {} do not include any Option.None case",
                    name
                ));
            }
        }
        Type::Bool => {
            if !block
                .cases
                .iter()
                .any(|(_, right)| expr_is_bool_case(right, true))
            {
                gaps.push(format!(
                    "verify examples for {} do not include any `true` result",
                    name
                ));
            }
            if !block
                .cases
                .iter()
                .any(|(_, right)| expr_is_bool_case(right, false))
            {
                gaps.push(format!(
                    "verify examples for {} do not include any `false` result",
                    name
                ));
            }
        }
        Type::Named {
            name: type_name, ..
        } => {
            let constructors = local_sum_type_constructors(items, &type_name)?;
            let mut covered = BTreeSet::new();
            for (_, right) in &block.cases {
                if let Some(tag) = constructor_tag_from_expr(right)
                    && constructors.contains(&tag)
                {
                    covered.insert(tag);
                }
            }
            if covered.len() < constructors.len() {
                gaps.push(format!(
                    "verify examples for {} cover {}/{} output constructors",
                    name,
                    covered.len(),
                    constructors.len()
                ));
            }
        }
        _ => return None,
    }
    Some(gaps)
}

pub fn collect_verify_coverage_warnings(items: &[TopLevel]) -> Vec<CheckFinding> {
    collect_verify_coverage_warnings_in(items, None)
}

pub fn collect_verify_coverage_warnings_in(
    items: &[TopLevel],
    source_file: Option<&str>,
) -> Vec<CheckFinding> {
    let module_name = module_name_for_items(items);
    let recursive_fns = find_recursive_fns(items);
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
    let flow_covered = flow_covered_fns(items);

    let mut warnings = Vec::new();
    for block in merge_verify_blocks(items) {
        if !verify_cases_block_is_well_formed(&block) {
            continue;
        }
        // Verify-trace blocks route RHS through oracle-bound stubs:
        // `target().result => stub(path, k, args)`. Syntactically the RHS
        // is a FnCall to the stub, not a Result.Ok/.Err or Option
        // constructor — but the stub *does* produce those values at run
        // time. Coverage heuristics here are pure syntactic shape checks,
        // so skip them for trace blocks to avoid spurious "no Result.Ok"
        // warnings on correctly-written verify-trace examples.
        if block.trace {
            continue;
        }
        let Some(f) = fn_defs.get(block.fn_name.as_str()).copied() else {
            continue;
        };

        // A private helper with a single covered caller inherits that caller's
        // return-shape coverage. Only the return shape: the caller's vectors
        // say nothing about the helper's own argument domain, which is what
        // the input-shape checks below claim.
        if !flow_covered.contains(block.fn_name.as_str())
            && let Some(gaps) = return_shape_gaps(f, &block, items)
        {
            for message in gaps {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message,
                    extra_spans: vec![],
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
                    fn_name: None,
                    message: format!(
                        "verify examples for {} cover {}/{} enum constructors",
                        block.fn_name,
                        covered.len(),
                        constructors.len()
                    ),
                    extra_spans: vec![],
                });
            }
        }

        if let Some(param_index) = bool_match_coverage_target(f) {
            let param_name = &f.params[param_index].0;
            let mut args = Vec::new();
            for (left, _) in &block.cases {
                collect_target_call_args(left, &block.fn_name, param_index, &mut args);
            }
            if !args.iter().any(|arg| expr_is_bool_case(arg, true)) {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message: format!(
                        "verify examples for {} do not cover `{}` = `true`",
                        block.fn_name, param_name
                    ),
                    extra_spans: vec![],
                });
            }
            if !args.iter().any(|arg| expr_is_bool_case(arg, false)) {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message: format!(
                        "verify examples for {} do not cover `{}` = `false`",
                        block.fn_name, param_name
                    ),
                    extra_spans: vec![],
                });
            }
        }

        if let Some(param_index) = list_match_coverage_target(f) {
            let param_name = &f.params[param_index].0;
            let mut args = Vec::new();
            for (left, _) in &block.cases {
                collect_target_call_args(left, &block.fn_name, param_index, &mut args);
            }
            if !args.iter().any(|arg| expr_is_empty_list_case(arg)) {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message: format!(
                        "verify examples for {} do not cover empty list input for `{}`",
                        block.fn_name, param_name
                    ),
                    extra_spans: vec![],
                });
            }
            if !args.iter().any(|arg| expr_is_non_empty_list_case(arg)) {
                warnings.push(CheckFinding {
                    line: block.line,
                    module: module_name.clone(),
                    file: source_file.map(|s| s.to_string()),
                    fn_name: None,
                    message: format!(
                        "verify examples for {} do not cover non-empty list input for `{}`",
                        block.fn_name, param_name
                    ),
                    extra_spans: vec![],
                });
            }
        }

        if recursive_fns.contains(&block.fn_name) && f.params.len() == 1 {
            let (param_name, param_ty) = &f.params[0];
            let Ok(param_ty) = parse_type_str_strict(param_ty) else {
                continue;
            };
            let mut args = Vec::new();
            for (left, _) in &block.cases {
                collect_target_call_args(left, &block.fn_name, 0, &mut args);
            }

            match param_ty {
                Type::Int => {
                    let has_zero = args.iter().any(|arg| expr_is_int_literal_case(arg, 0));
                    let has_one = args.iter().any(|arg| expr_is_int_literal_case(arg, 1));
                    if !has_zero && !has_one {
                        warnings.push(CheckFinding {
                                line: block.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: None,
                                message: format!(
                                    "verify examples for recursive function {} may not include a numeric base-case input for `{}` (`0` or `1`)",
                                    block.fn_name, param_name
                                ),
                                extra_spans: vec![],
                            });
                    }
                }
                Type::List(_) if !args.iter().any(|arg| expr_is_empty_list_case(arg)) => {
                    warnings.push(CheckFinding {
                                line: block.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: None,
                                message: format!(
                                    "verify examples for recursive function {} may not include an empty list input for `{}`",
                                    block.fn_name, param_name
                                ),
                                extra_spans: vec![],
                            });
                }
                Type::Str if !args.iter().any(|arg| expr_is_empty_string_case(arg)) => {
                    warnings.push(CheckFinding {
                                line: block.line,
                                module: module_name.clone(),
                                file: source_file.map(|s| s.to_string()),
                                fn_name: None,
                                message: format!(
                                    "verify examples for recursive function {} may not include an empty string input for `{}`",
                                    block.fn_name, param_name
                                ),
                                extra_spans: vec![],
                            });
                }
                _ => {}
            }
        }

        let parser_like = f.name.starts_with("parse") || f.name == "fromString";
        if parser_like {
            let Ok(ret_ty) = parse_type_str_strict(&f.return_type) else {
                continue;
            };
            if matches!(ret_ty, Type::Result(_, _) | Type::Option(_))
                && let Some((param_name, param_ty)) = f.params.first()
            {
                let Ok(Type::Str) = parse_type_str_strict(param_ty) else {
                    continue;
                };
                let mut args = Vec::new();
                for (left, _) in &block.cases {
                    collect_target_call_args(left, &block.fn_name, 0, &mut args);
                }
                if !args.iter().any(|arg| expr_is_empty_string_case(arg)) {
                    warnings.push(CheckFinding {
                        line: block.line,
                        module: module_name.clone(),
                        file: source_file.map(|s| s.to_string()),
                        fn_name: None,
                        message: format!(
                            "verify examples for {} may not include an empty string input for `{}`",
                            block.fn_name, param_name
                        ),
                        extra_spans: vec![],
                    });
                }
            }
        }
    }

    warnings
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
    fn coverage_warns_when_result_verify_has_no_ok_example() {
        let items = parse_items(
            r#"
fn alwaysFail(n: Int) -> Result<Int, String>
    Result.Err("nope")

verify alwaysFail
    alwaysFail(1) => Result.Err("nope")
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message == "verify examples for alwaysFail do not include any Result.Ok case"
            }),
            "expected missing-ok warning, got {:?}",
            warnings
        );
        assert!(
            warnings.iter().any(|w| w.line == 5),
            "expected verify-block line number, got {:?}",
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
    fn coverage_warns_when_option_verify_has_no_some_example() {
        let items = parse_items(
            r#"
fn nope(n: Int) -> Option<Int>
    Option.None

verify nope
    nope(1) => Option.None
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message == "verify examples for nope do not include any Option.Some case"
            }),
            "expected missing-some warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_warns_when_bool_verify_has_only_one_result_shape() {
        let items = parse_items(
            r#"
fn isZero(n: Int) -> Bool
    n == 0

verify isZero
    isZero(0) => true
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(
                |w| w.message == "verify examples for isZero do not include any `false` result"
            ),
            "expected missing-false warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_warns_when_sum_output_constructors_are_partial() {
        let items = parse_items(
            r#"
type Outcome
    Win(String)
    Lose(String)
    Continue

fn classify(n: Int) -> Outcome
    match n
        0 -> Outcome.Continue
        1 -> Outcome.Win("yay")
        _ -> Outcome.Lose("no")

verify classify
    classify(0) => Outcome.Continue
    classify(1) => Outcome.Win("yay")
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message == "verify examples for classify cover 2/3 output constructors"
            }),
            "expected output-constructor coverage warning, got {:?}",
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
    fn coverage_warns_when_bool_match_input_examples_miss_a_branch() {
        let items = parse_items(
            r#"
fn flagName(flag: Bool) -> String
    match flag
        true -> "yes"
        false -> "no"

verify flagName
    flagName(true) => "yes"
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings
                .iter()
                .any(|w| w.message == "verify examples for flagName do not cover `flag` = `false`"),
            "expected bool-input coverage warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_warns_when_list_match_input_examples_miss_empty_case() {
        let items = parse_items(
            r#"
fn headOrZero(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [head, ..tail] -> head

verify headOrZero
    headOrZero([1, 2]) => 1
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message == "verify examples for headOrZero do not cover empty list input for `xs`"
            }),
            "expected empty-list coverage warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_merges_multiple_verify_blocks_before_checking_shapes() {
        let items = parse_items(
            r#"
fn maybe(n: Int) -> Option<Int>
    match n
        0 -> Option.None
        _ -> Option.Some(n)

verify maybe
    maybe(0) => Option.None

verify maybe
    maybe(1) => Option.Some(1)
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            !warnings.iter().any(|w| w.message.contains("Option.None"))
                && !warnings.iter().any(|w| w.message.contains("Option.Some")),
            "expected merged verify blocks to satisfy coverage, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_soft_warns_when_recursive_int_fn_has_no_zero_or_one_example() {
        let items = parse_items(
            r#"
fn fib(n: Int) -> Int
    match n == 0
        true -> 0
        false -> match n == 1
            true -> 1
            false -> fib(n - 1) + fib(n - 2)

verify fib
    fib(5) => 5
    fib(6) => 8
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message == "verify examples for recursive function fib may not include a numeric base-case input for `n` (`0` or `1`)"
            }),
            "expected recursive boundary warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_soft_warns_when_recursive_list_fn_has_no_empty_input_example() {
        let items = parse_items(
            r#"
fn sum(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [head, ..tail] -> head + sum(tail)

verify sum
    sum([1, 2, 3]) => 6
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message
                    == "verify examples for recursive function sum may not include an empty list input for `xs`"
            }),
            "expected recursive empty-list warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_soft_warns_when_parser_like_fn_has_no_empty_string_example() {
        let items = parse_items(
            r#"
fn fromString(s: String) -> Result<Int, String>
    match s == ""
        true -> Result.Err("empty")
        false -> Result.Ok(1)

verify fromString
    fromString("42") => Result.Ok(1)
    fromString("x") => Result.Err("empty")
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            warnings.iter().any(|w| {
                w.message
                    == "verify examples for fromString may not include an empty string input for `s`"
            }),
            "expected parser empty-string warning, got {:?}",
            warnings
        );
    }

    #[test]
    fn coverage_soft_does_not_warn_when_recursive_base_case_is_split_across_verify_blocks() {
        let items = parse_items(
            r#"
fn fib(n: Int) -> Int
    match n == 0
        true -> 0
        false -> match n == 1
            true -> 1
            false -> fib(n - 1) + fib(n - 2)

verify fib
    fib(0) => 0

verify fib
    fib(5) => 5
"#,
        );
        let warnings = collect_verify_coverage_warnings(&items);
        assert!(
            !warnings
                .iter()
                .any(|w| w.message.contains("numeric base-case input")),
            "expected merged verify blocks to silence recursive boundary warning, got {:?}",
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
}
