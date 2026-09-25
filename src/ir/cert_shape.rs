//! Which functions the fabricating passes leave in their source form.
//!
//! A certificate is for the bytes that ship, so `--certify` must not change
//! the compile: the same passes run in both modes. What keeps a function
//! certifiable is one mode-independent rule instead: a fabricating pass
//! (buffer build, chars fusion, byte sink) does not rewrite the body of a
//! function the certificate plan printer (`codegen::cert::plan_from_mir`)
//! could print in its unfused form. The pass may still synthesize variants
//! (`<fn>__cursor`, `<fn>__buffered`, `<fn>__collected`, `<fn>__code`); it
//! only leaves the kept function's own body alone.
//!
//! The printer runs on MIR at emission, after the passes, so its verdict is
//! not available here. [`printer_may_admit`] is a cheap syntactic superset of
//! it, read from the AST before the first fabricating pass: it answers
//! `false` only for a fact on which the printer always declines. A wrong
//! `true` costs a fusion in one function; a wrong `false` would cost a
//! certificate, and `tests/cert_one_build_spec.rs` checks on the corpus that
//! it never happens.

use std::collections::HashSet;

use crate::ast::{Expr, FnDef, Spanned, TopLevel};

/// The builtin calls the plan printer prints (see the `MirCallee::Builtin`
/// arm of `Printer::expr`). The printer consults this list before its own
/// match, so it can never admit a builtin this module counts as declined.
pub const PRINTED_BUILTINS: &[&str] = &[
    "Bool.and",
    "Bool.or",
    "Bool.not",
    "List.prepend",
    "Vector.get",
    "Option.withDefault",
    "Result.withDefault",
    "Int.div",
    "Int.mod",
];

/// Constructor paths spelled like builtin calls; MIR makes them `Construct`.
const CONSTRUCTORS: &[&str] = &["Result.Ok", "Result.Err", "Option.Some", "Option.None"];

/// `false` when the printer is certain to decline `fd`: it declares effects,
/// or its body calls a builtin outside [`PRINTED_BUILTINS`] or a
/// compiler-reserved `__` intrinsic. `true` otherwise.
pub fn printer_may_admit(fd: &FnDef) -> bool {
    fd.effects.is_empty()
        && fd.body.stmts().iter().all(|stmt| {
            let mut ok = true;
            visit(super::chars_fusion::stmt_expr(stmt), &mut ok);
            ok
        })
}

fn visit(expr: &Spanned<Expr>, ok: &mut bool) {
    if !*ok {
        return;
    }
    if let Expr::FnCall(callee, _) = &expr.node
        && let Some(name) = super::calls::expr_to_dotted_name(&callee.node)
    {
        let reserved = name.starts_with("__");
        let unprinted_builtin = name.split_once('.').is_some_and(|(ns, _)| {
            super::calls::is_builtin_namespace(ns)
                && !PRINTED_BUILTINS.contains(&name.as_str())
                && !CONSTRUCTORS.contains(&name.as_str())
        });
        if reserved || unprinted_builtin {
            *ok = false;
            return;
        }
    }
    super::chars_fusion::walk_children(expr, &mut |child| visit(child, ok));
}

/// The names of every function in `items` the passes must leave unfused.
pub fn kept_unfused(items: &[TopLevel]) -> HashSet<String> {
    super::chars_fusion::fn_defs(items)
        .filter(|fd| printer_may_admit(fd))
        .map(|fd| fd.name.clone())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kept(src: &str) -> Vec<String> {
        let items = crate::source::parse_source(src).expect("parse");
        let mut names: Vec<String> = kept_unfused(&items).into_iter().collect();
        names.sort();
        names
    }

    #[test]
    fn a_single_character_classifier_is_kept_and_a_string_walker_is_not() {
        let src = r#"
module K
    intent = "kept-unfused probes"
    exposes [quote, len1, both, loud]
    effects [Console]

fn quote(c: String) -> String
    ? "A classifier the printer prints unfused."
    match c
        "\"" -> "\\\""
        _ -> c

fn len1(s: String) -> Int
    ? "A builtin the printer declines."
    String.len(s)

fn both(p: Bool, q: Bool) -> Bool
    ? "Printed builtins only."
    Bool.and(p, Bool.not(q))

fn loud(s: String) -> Unit
    ? "Effects decline."
    ! [Console.print]
    Console.print(s)
"#;
        assert_eq!(kept(src), vec!["both".to_string(), "quote".to_string()]);
    }
}
