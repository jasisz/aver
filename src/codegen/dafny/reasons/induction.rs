//! A checked recursive lemma call can instantiate a list induction hypothesis
//! where SMT trigger selection cannot find it. This is only a proof hint: Dafny
//! must establish the called step's guard, prior reasons, and strict decrease.

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{Expr, Pattern, Spanned, Stmt, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::dafny::expr::{aver_name_to_dafny, emit_expr};
use crate::codegen::dafny::toplevel::{replace_ident_words, resolve_rewrite_output};

fn identifier(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

/// Return only calls that have an unambiguous source-to-law substitution. More
/// complicated source scopes keep the normal checked proof path without hints.
pub(super) fn emit_list_calls(
    expr: &Spanned<Expr>,
    law: &VerifyLaw,
    step_name: &str,
    ctx: &CodegenContext,
) -> Option<Vec<String>> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let scope = ctx.active_module_scope();
    let name = crate::checker::expr_to_str(callee);
    let id = ctx.symbol_table.resolve_fn_id_in(&name, scope.as_deref())?;
    let key = &ctx.symbol_table.fn_entry(id).key;
    if key.scope_str() != scope.as_deref() {
        return None;
    }
    let fd = ctx.fn_def_by_name(&key.name, key.scope_str())?;
    if fd.return_type != "Bool" || fd.params.len() != args.len() || args.len() != law.givens.len() {
        return None;
    }
    let list_index = super::subset::list_parameter(fd, ctx)?;
    let mut substitutions = BTreeMap::new();
    let mut given_positions = BTreeMap::new();
    for (index, ((formal, _), actual)) in fd.params.iter().zip(args).enumerate() {
        let given = identifier(actual)?;
        if !law.givens.iter().any(|g| g.name == given)
            || given_positions.insert(given, index).is_some()
        {
            return None;
        }
        substitutions.insert(formal.as_str(), aver_name_to_dafny(given));
    }
    let driver = aver_name_to_dafny(identifier(args.get(list_index)?)?);
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    if identifier(subject)? != fd.params[list_index].0 || arms.len() != 2 {
        return None;
    }
    let empty = arms
        .iter()
        .find(|arm| matches!(arm.pattern, Pattern::EmptyList))?;
    let cons = arms
        .iter()
        .find(|arm| matches!(arm.pattern, Pattern::Cons(_, _)))?;
    let Pattern::Cons(head, tail) = &cons.pattern else {
        return None;
    };
    // A nested match may condition the self-call or introduce local binders.
    // Such scopes need a richer substitution and guard plan than this hint.
    if crate::codegen::expr_walk::any(&cons.body, &mut |expr| {
        matches!(expr.node, Expr::Match { .. })
    }) {
        return None;
    }
    let mut empty_calls = Vec::new();
    crate::codegen::recursion::detect::collect_calls_from_expr(&empty.body, &mut empty_calls);
    let resolves_self =
        |name: &str| ctx.symbol_table.resolve_fn_id_in(name, scope.as_deref()) == Some(id);
    if empty_calls.iter().any(|(name, _)| resolves_self(name)) {
        return None;
    }
    let mut calls = Vec::new();
    crate::codegen::recursion::detect::collect_calls_from_expr(&cons.body, &mut calls);
    let mut self_calls = calls.iter().filter(|(name, _)| resolves_self(name));
    let (_, recursive_args) = self_calls.next()?;
    if self_calls.next().is_some() || recursive_args.len() != fd.params.len() {
        return None;
    }
    // Pattern binders shadow source parameters. Simultaneous replacement keeps
    // inserted law names from being captured by a second source substitution.
    if head != "_" {
        substitutions.insert(head, format!("{driver}[0]"));
    }
    if tail != "_" {
        substitutions.insert(tail, format!("{driver}[1..]"));
    }
    let emitted: Vec<_> = substitutions
        .iter()
        .map(|(source, target)| (aver_name_to_dafny(source), target.clone()))
        .collect();
    let emitted_names: BTreeSet<_> = emitted.iter().map(|(name, _)| name.as_str()).collect();
    if emitted_names.len() != emitted.len() || emitted_names.iter().any(|name| !name.is_ascii()) {
        return None;
    }
    let translated = recursive_args
        .iter()
        .map(|arg| {
            if !substitution_safe(arg, &substitutions, &emitted_names) {
                return None;
            }
            let rendered = emit_expr(&resolve_rewrite_output(arg, ctx), ctx);
            Some(replace_ident_words(&rendered, &emitted))
        })
        .collect::<Option<Vec<_>>>()?;
    let ordered = law
        .givens
        .iter()
        .map(|given| {
            translated
                .get(*given_positions.get(given.name.as_str())?)
                .cloned()
        })
        .collect::<Option<Vec<_>>>()?;
    Some(vec![
        format!("  if |{driver}| > 0 {{"),
        format!("    {step_name}({});", ordered.join(", ")),
        "  }".to_string(),
    ])
}

/// Validate every free value identifier and rule out accidental textual
/// replacement of a field, constructor, or static callee name. Expressions
/// introducing binders are conservatively omitted rather than guessed at.
fn substitution_safe(
    expr: &Spanned<Expr>,
    substitutions: &BTreeMap<&str, String>,
    emitted_names: &BTreeSet<&str>,
) -> bool {
    let safe = |expr| substitution_safe(expr, substitutions, emitted_names);
    let stable_name = |name: &str| {
        name.split('.')
            .all(|part| !emitted_names.contains(aver_name_to_dafny(part).as_str()))
    };
    match &expr.node {
        Expr::Literal(_) => true,
        Expr::Ident(name) | Expr::Resolved { name, .. } => {
            substitutions.contains_key(name.as_str())
        }
        Expr::Neg(inner) => safe(inner),
        Expr::BinOp(_, left, right) => safe(left) && safe(right),
        Expr::List(items) | Expr::Tuple(items) => items.iter().all(safe),
        Expr::Attr(base, field) => stable_name(field) && safe(base),
        // Calls and constructors may emit helper names absent from the source
        // spelling. Until substitution is typed before emission, do not risk
        // replacing one of those generated callee names with a source value.
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{TopLevel, VerifyKind};
    use crate::codegen::dafny::tests::ctx_from_source;

    const SOURCE: &str = r#"fn step(xs: List<Int>, low: Int, high: Int) -> Bool
    match xs
        [] -> low < high
        [head, ..rest] -> Bool.and(step(rest, low * 2 + head, high * 2 + head), low < high)
verify step law guarded
    given upper: Int = [2]
    given values: List<Int> = [[], [1]]
    given lower: Int = [1]
    when lower < upper
    because step(values, lower, upper)
    using []
    step(values, lower, upper) holds
"#;

    fn hint(source: &str) -> Option<String> {
        let ctx = ctx_from_source(source, "Guidance");
        let law = ctx
            .items
            .iter()
            .find_map(|item| match item {
                TopLevel::Verify(block) => match &block.kind {
                    VerifyKind::Law(law) => Some(law.as_ref()),
                    _ => None,
                },
                _ => None,
            })
            .unwrap();
        emit_list_calls(&law.because[0], law, "checkedStep", &ctx).map(|lines| lines.join("\n"))
    }

    #[test]
    fn recursive_call_reorders_and_renames_arguments_to_the_law_binders() {
        let text = hint(SOURCE).unwrap();
        assert!(text.contains("if |values| > 0"), "{text}");
        assert!(
            text.contains(
                "checkedStep(((upper * 2) + values[0]), values[1..], ((lower * 2) + values[0]));"
            ),
            "{text}"
        );
        assert!(!text.contains("assume"), "{text}");
    }

    #[test]
    fn cons_binder_matching_a_law_given_does_not_cascade_substitution() {
        let source = SOURCE
            .replace("[head, ..rest]", "[head, ..values]")
            .replace("step(rest,", "step(values,");
        let text = hint(&source).unwrap();
        assert!(text.contains("values[1..]"), "{text}");
        assert!(!text.contains("values[1..][1..]"), "{text}");
    }

    #[test]
    fn inserted_law_names_are_not_replaced_as_source_formals() {
        let source = SOURCE
            .replace("upper", "xs")
            .replace("values", "low")
            .replace("lower", "high");
        let text = hint(&source).unwrap();
        assert!(
            text.contains("checkedStep(((xs * 2) + low[0]), low[1..], ((high * 2) + low[0]));"),
            "{text}"
        );
    }

    #[test]
    fn ambiguous_or_nonlocal_recursive_instantiations_do_not_emit_hints() {
        let duplicate_actual =
            SOURCE.replace("step(values, lower, upper)", "step(values, lower, lower)");
        assert!(hint(&duplicate_actual).is_none());
        let call_arg = SOURCE.replace("low * 2 + head", "Int.abs(low * 2 + head)");
        assert!(hint(&call_arg).is_none());
        let nested = SOURCE.replace(
            "Bool.and(step(rest, low * 2 + head, high * 2 + head), low < high)",
            "Bool.and(step(rest, low * 2 + head, high * 2 + head), step(rest, low, high))",
        );
        assert!(hint(&nested).is_none());
        let local = SOURCE.replace("    match xs", "    bound = low\n    match xs");
        assert!(hint(&local).is_none());
    }

    #[test]
    fn identifiers_inside_escaped_string_literals_are_not_substituted() {
        let source = r#"fn step(xs: List<Int>, text: String) -> Bool
    match xs
        [] -> text == text
        [_, ..rest] -> step(rest, "xs \"text\"")
verify step law guarded
    given spelling: String = [""]
    given values: List<Int> = [[], [1]]
    because step(values, spelling)
    using []
    step(values, spelling) holds
"#;
        let text = hint(source).unwrap();
        assert!(
            text.contains(r#"checkedStep("xs \"text\"", values[1..]);"#),
            "{text}"
        );
    }

    #[test]
    fn nested_match_scopes_and_field_name_collisions_omit_the_hint() {
        let nested = SOURCE.replace(
            "Bool.and(step(rest, low * 2 + head, high * 2 + head), low < high)",
            "match low < high\n            true -> step(rest, low * 2 + head, high * 2 + head)\n            false -> true",
        );
        assert!(hint(&nested).is_none());
        let collision = r#"record Value
    data: Int
fn step(xs: List<Int>, wrapper: Value, data: Int) -> Bool
    match xs
        [] -> true
        [_, ..rest] -> step(rest, wrapper, wrapper.data)
verify step law guarded
    given values: List<Int> = [[]]
    given boxed: Value = [Value(data = 0)]
    given value: Int = [0]
    because step(values, boxed, value)
    using []
    step(values, boxed, value) holds
"#;
        assert!(hint(collision).is_none());
    }
}
