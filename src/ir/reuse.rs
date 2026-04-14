//! Tail-call reuse analysis.
//!
//! For each `TailCall` in a function body, determines which arguments
//! receive a value that was derived from the corresponding parameter
//! with no other live references. Such parameters are "owned" at the
//! tail-call site and their backing allocation can be mutated in-place
//! by the callee (e.g. `Map.set` modifies the HAMT node instead of cloning).
//!
//! The analysis uses `ir::liveness::compute_args_used_after` to determine
//! which variables are live after each argument position, and marks a
//! tail-call argument as reusable when:
//!
//!   1. The argument expression contains the corresponding parameter name
//!   2. The parameter is NOT live after the tail-call (i.e. this is its last use)
//!   3. No other tail-call argument also references the same parameter
//!
//! The result is stored on the `TailCall` AST node so every backend can
//! benefit: VM skips arena finalization, WASM reuses the heap slot,
//! Rust codegen emits `&mut` instead of clone.

use crate::ast::*;
use crate::ir::liveness::compute_args_used_after;
use crate::ir::vars::pattern_bindings;

/// Analyse a function body and annotate each `TailCall` with reuse info.
///
/// `params` — the function's parameter names, in declaration order.
/// `body`   — the function body (block of statements).
///
/// Returns a map from each TailCall's position to a `Vec<bool>` indicating
/// which argument slots are reusable.  We identify TailCall positions by
/// the fact that they only appear in tail position (last expr, or match arm
/// bodies in tail position).
///
/// For simplicity we return a flat Vec of all reuse-flag vectors found,
/// in the order we encounter them during a depth-first walk.  Since a
/// function body typically has one tail-call site per match arm, this
/// is enough for the compiler to zip with the emitted TailCall opcodes.
pub fn analyze_fn_reuse(params: &[String], body: &FnBody) -> Vec<Vec<bool>> {
    let mut results = Vec::new();

    // Collect all variables used in the ENTIRE function body.
    // A parameter that appears in a non-tail-call context (e.g. the base
    // case `0 -> m`) is still "used" but in a different match arm — at
    // runtime, if we take the tail-call arm, those other uses don't exist.
    //
    // So we analyse per-arm: in the specific match arm that contains the
    // tail call, is the parameter's last use within the tail-call expression?

    if let Some(Stmt::Expr(expr)) = body.stmts().last() {
        analyze_tail_expr(params, &expr.node, &mut results);
    }

    results
}

/// Walk an expression in tail position, looking for TailCall nodes.
fn analyze_tail_expr(params: &[String], expr: &Expr, results: &mut Vec<Vec<bool>>) {
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: _target,
                args,
                ..
            } = boxed.as_ref();
            let reuse = compute_reuse_for_tail_call(params, args);
            results.push(reuse);
        }
        Expr::Match { arms, .. } => {
            // Each arm body is in tail position
            for arm in arms {
                // Collect all variables used in THIS arm's body
                // (before reaching the tail call).
                analyze_tail_in_arm(params, arm, results);
            }
        }
        _ => {}
    }
}

/// Analyse a single match arm that may contain a tail call.
/// We need to check that within this arm, the parameter is not used
/// by any binding or expression *before* the tail call, or by
/// the match pattern itself.
fn analyze_tail_in_arm(params: &[String], arm: &MatchArm, results: &mut Vec<Vec<bool>>) {
    let body_expr = &arm.body.node;

    // Variables bound by the pattern shadow params — if a param name
    // is shadowed, it's not reusable (the tail call sees the shadow).
    let pattern_bindings = pattern_bindings(&arm.pattern);

    match body_expr {
        Expr::TailCall(boxed) => {
            let TailCallData {
                target: _target,
                args,
                ..
            } = boxed.as_ref();
            let mut reuse = compute_reuse_for_tail_call(params, args);
            // If any param is shadowed by a pattern binding, it can't be reused
            for (i, param) in params.iter().enumerate() {
                if i < reuse.len() && pattern_bindings.contains(param) {
                    reuse[i] = false;
                }
            }
            results.push(reuse);
        }
        Expr::Match { arms, .. } => {
            // Nested match in tail position
            for inner_arm in arms {
                analyze_tail_in_arm(params, inner_arm, results);
            }
        }
        _ => {
            // Not a tail call arm — no reuse info needed
        }
    }
}

/// For a single TailCall, compute which arguments can reuse their
/// corresponding parameter's allocation.
///
/// Uses `ir::liveness::compute_args_used_after` to determine whether
/// the parameter is at its last use within the tail-call argument list.
fn compute_reuse_for_tail_call(params: &[String], tail_args: &[Spanned<Expr>]) -> Vec<bool> {
    let argc = tail_args.len();
    let mut reusable = vec![false; argc];

    // Compute which variables are live after each argument position.
    // For the tail call, there is nothing after — parent_used_after is empty.
    let used_after = compute_args_used_after(tail_args, &Default::default());

    for i in 0..argc {
        if i >= params.len() {
            continue;
        }
        let param = &params[i];

        // The parameter must NOT be live after this argument position.
        // If it IS live, another later argument also uses it → not sole owner.
        if !used_after[i].is_last_use(param) {
            continue;
        }

        // The argument expression must actually reference the parameter
        // (otherwise the parameter is dead here, which is fine but not "reuse").
        let arg_vars = crate::ir::vars::collect_vars(&tail_args[i].node);
        if arg_vars.contains(param) {
            reusable[i] = true;
        }
    }

    reusable
}

/// Walk the entire program and annotate `TailCallData.owned` on every TailCall node.
/// Called from `tco::transform_program` after tail-call rewriting.
pub fn annotate_program_reuse(items: &mut [TopLevel]) {
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            let params: Vec<String> = fd.params.iter().map(|(name, _)| name.clone()).collect();
            if !params.is_empty() {
                let mut body = fd.body.as_ref().clone();
                if annotate_body_reuse(&params, &mut body) {
                    fd.body = std::sync::Arc::new(body);
                }
            }
        }
    }
}

/// Annotate TailCall nodes in a function body. Returns true if any were annotated.
fn annotate_body_reuse(params: &[String], body: &mut FnBody) -> bool {
    if let Some(Stmt::Expr(expr)) = body.stmts_mut().last_mut() {
        return annotate_expr_reuse(params, &mut expr.node);
    }
    false
}

/// Recursively annotate TailCall nodes in tail-position expressions.
fn annotate_expr_reuse(params: &[String], expr: &mut Expr) -> bool {
    match expr {
        Expr::TailCall(boxed) => {
            let reuse = compute_reuse_for_tail_call(params, &boxed.args);
            if reuse.iter().any(|&r| r) {
                boxed.owned = reuse;
                return true;
            }
            false
        }
        Expr::Match { arms, .. } => {
            let mut any = false;
            for arm in arms.iter_mut() {
                if annotate_arm_reuse(params, arm) {
                    any = true;
                }
            }
            any
        }
        _ => false,
    }
}

/// Annotate a match arm that may contain a tail call.
fn annotate_arm_reuse(params: &[String], arm: &mut MatchArm) -> bool {
    let shadowed = pattern_bindings(&arm.pattern);

    match &mut arm.body.node {
        Expr::TailCall(boxed) => {
            let mut reuse = compute_reuse_for_tail_call(params, &boxed.args);
            for (i, param) in params.iter().enumerate() {
                if i < reuse.len() && shadowed.contains(param) {
                    reuse[i] = false;
                }
            }
            if reuse.iter().any(|&r| r) {
                boxed.owned = reuse;
                return true;
            }
            false
        }
        Expr::Match { arms, .. } => {
            let mut any = false;
            for inner_arm in arms.iter_mut() {
                if annotate_arm_reuse(params, inner_arm) {
                    any = true;
                }
            }
            any
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ident(name: &str) -> Spanned<Expr> {
        Spanned::bare(Expr::Ident(name.to_string()))
    }

    fn call(fn_name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        Spanned::bare(Expr::FnCall(
            Box::new(Spanned::bare(Expr::Ident(fn_name.to_string()))),
            args,
        ))
    }

    fn binop(op: BinOp, left: Spanned<Expr>, right: Spanned<Expr>) -> Spanned<Expr> {
        Spanned::bare(Expr::BinOp(op, Box::new(left), Box::new(right)))
    }

    fn int(n: i64) -> Spanned<Expr> {
        Spanned::bare(Expr::Literal(Literal::Int(n)))
    }

    fn str_lit(s: &str) -> Spanned<Expr> {
        Spanned::bare(Expr::Literal(Literal::Str(s.to_string())))
    }

    fn tail_call(name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        Spanned::bare(Expr::TailCall(Box::new(TailCallData::new(
            name.to_string(),
            args,
        ))))
    }

    fn make_match(subject: Spanned<Expr>, arms: Vec<(Pattern, Spanned<Expr>)>) -> Spanned<Expr> {
        Spanned::bare(Expr::Match {
            subject: Box::new(subject),
            arms: arms
                .into_iter()
                .map(|(pat, body)| MatchArm {
                    pattern: pat,
                    body: Box::new(body),
                })
                .collect(),
        })
    }

    fn body_from_expr(expr: Spanned<Expr>) -> FnBody {
        FnBody::Block(vec![Stmt::Expr(expr)])
    }

    #[test]
    fn test_map_build_pattern() {
        // fn buildMap(n: Int, m: Map<String, Int>) -> Map<String, Int>
        //     match n
        //         0 -> m
        //         _ -> buildMap(n - 1, Map.set(m, Int.toString(n), n))
        let params = vec!["n".to_string(), "m".to_string()];
        let body = body_from_expr(make_match(
            ident("n"),
            vec![
                (Pattern::Literal(Literal::Int(0)), ident("m")),
                (
                    Pattern::Wildcard,
                    tail_call(
                        "buildMap",
                        vec![
                            binop(BinOp::Sub, ident("n"), int(1)),
                            call(
                                "Map.set",
                                vec![
                                    ident("m"),
                                    call("Int.toString", vec![ident("n")]),
                                    ident("n"),
                                ],
                            ),
                        ],
                    ),
                ),
            ],
        ));

        let results = analyze_fn_reuse(&params, &body);
        // One tail call site (the _ arm)
        assert_eq!(results.len(), 1);
        // arg 0 (n-1): n is used in arg 1 too (Int.toString(n), n) — NOT reusable
        assert!(!results[0][0]);
        // arg 1 (Map.set(m, ...)): m is only used here — REUSABLE
        assert!(results[0][1]);
    }

    #[test]
    fn test_countdown_no_heap() {
        // fn countdown(n: Int) -> Int
        //     match n
        //         0 -> 0
        //         _ -> countdown(n - 1)
        let params = vec!["n".to_string()];
        let body = body_from_expr(make_match(
            ident("n"),
            vec![
                (Pattern::Literal(Literal::Int(0)), int(0)),
                (
                    Pattern::Wildcard,
                    tail_call("countdown", vec![binop(BinOp::Sub, ident("n"), int(1))]),
                ),
            ],
        ));

        let results = analyze_fn_reuse(&params, &body);
        assert_eq!(results.len(), 1);
        // n is only used in arg 0 — reusable (though Int is Copy so it doesn't matter)
        assert!(results[0][0]);
    }

    #[test]
    fn test_param_shared_across_args() {
        // fn foo(x: List<Int>, y: Int) -> List<Int>
        //     foo(List.prepend(x, y), List.len(x))
        //     x is used in BOTH args — not reusable
        let params = vec!["x".to_string(), "y".to_string()];
        let body = body_from_expr(tail_call(
            "foo",
            vec![
                call("List.prepend", vec![ident("x"), ident("y")]),
                call("List.len", vec![ident("x")]),
            ],
        ));

        let results = analyze_fn_reuse(&params, &body);
        assert_eq!(results.len(), 1);
        assert!(!results[0][0]); // x used in both args
        assert!(!results[0][1]); // y used only in arg 0, not arg 1
    }

    #[test]
    fn test_pattern_shadows_param() {
        // fn foo(m: Map<String, Int>) -> Map<String, Int>
        //     match m
        //         m -> foo(Map.set(m, "k", 1))
        //     here m is shadowed by the pattern binding
        let params = vec!["m".to_string()];
        let body = body_from_expr(make_match(
            ident("m"),
            vec![(
                Pattern::Ident("m".to_string()),
                tail_call(
                    "foo",
                    vec![call("Map.set", vec![ident("m"), str_lit("k"), int(1)])],
                ),
            )],
        ));

        let results = analyze_fn_reuse(&params, &body);
        assert_eq!(results.len(), 1);
        // m is shadowed by pattern — NOT reusable
        assert!(!results[0][0]);
    }

    #[test]
    fn test_accumulator_pattern() {
        // fn sumDistances(n: Int, acc: Int) -> Int
        //     match n
        //         0 -> acc
        //         _ -> sumDistances(n - 1, acc + distance(Point(x = n, y = n * 2)))
        let params = vec!["n".to_string(), "acc".to_string()];
        let body = body_from_expr(make_match(
            ident("n"),
            vec![
                (Pattern::Literal(Literal::Int(0)), ident("acc")),
                (
                    Pattern::Wildcard,
                    tail_call(
                        "sumDistances",
                        vec![
                            binop(BinOp::Sub, ident("n"), int(1)),
                            binop(BinOp::Add, ident("acc"), call("distance", vec![ident("n")])),
                        ],
                    ),
                ),
            ],
        ));

        let results = analyze_fn_reuse(&params, &body);
        assert_eq!(results.len(), 1);
        // n used in both args — not reusable
        assert!(!results[0][0]);
        // acc used only in arg 1 — reusable
        assert!(results[0][1]);
    }

    #[test]
    fn test_annotate_program_integration() {
        // Parse real Aver code and verify annotation lands on AST
        let src = r#"
module Bench

fn buildMap(n: Int, m: Map<String, Int>) -> Map<String, Int>
    match n
        0 -> m
        _ -> buildMap(n - 1, Map.set(m, Int.toString(n), n))

fn main() -> Int
    Map.len(buildMap(5000, Map.empty()))
"#;
        let mut items = crate::source::parse_source(src).expect("parse");
        crate::tco::transform_program(&mut items);

        // Find buildMap and check its tail call has owned annotation
        let fd = items
            .iter()
            .find_map(|item| {
                if let TopLevel::FnDef(fd) = item
                    && fd.name == "buildMap"
                {
                    return Some(fd);
                }
                None
            })
            .expect("buildMap not found");

        // Walk to the TailCall in the _ arm
        fn find_tail_call(expr: &Expr) -> Option<&TailCallData> {
            match expr {
                Expr::TailCall(tc) => Some(tc),
                Expr::Match { arms, .. } => {
                    arms.iter().find_map(|arm| find_tail_call(&arm.body.node))
                }
                _ => None,
            }
        }

        let tail_expr = fd.body.stmts().last().unwrap();
        let Stmt::Expr(expr) = tail_expr else {
            panic!("expected expr")
        };
        let tc = find_tail_call(&expr.node).expect("no TailCall found");

        assert_eq!(tc.target, "buildMap");
        assert_eq!(tc.owned.len(), 2);
        assert!(!tc.owned[0], "n should NOT be reusable (used in both args)");
        assert!(tc.owned[1], "m should be reusable (sole owner in Map.set)");
    }
}
