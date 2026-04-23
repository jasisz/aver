//! Method-level lifting of effectful impl bodies.
//!
//! Given a function body that invokes classified generative / snapshot
//! effects (`Random.int`, `Args.get`, `Http.get`, ...), produce a pure
//! AST where those invocations are replaced with oracle / capability
//! function applications. This is the transform that turns
//!
//! ```aver
//! fn pickThree() -> (Int, Int, Int)
//!     ! [Random.int]
//!     (Random.int(1, 100), Random.int(1, 100), Random.int(1, 100))
//! ```
//!
//! into a pure form shaped like
//!
//! ```aver
//! fn pickThree_lifted(path: BranchPath, oracle: (BranchPath, Int, Int, Int) -> Int)
//!     -> (Int, Int, Int)
//!     (oracle(path, 0, 1, 100), oracle(path, 1, 1, 100), oracle(path, 2, 1, 100))
//! ```
//!
//! Scope of this v0:
//!
//! - Generative effects: call-site replaced by `oracle(path, counter, args...)`
//!   with a statically-assigned counter (0, 1, 2, …) along the in-order walk.
//! - Snapshot effects: call-site replaced by `capability(args...)` (no path,
//!   no counter — snapshots are schedule-invariant by definition).
//! - Output effects: **left alone** by this transform — they're asserted
//!   about via the trace API in a separate elaboration path, not lifted to
//!   an oracle.
//!
//! Not yet handled (follow-up commits):
//!
//! - `!` / `?!` groups: each branch needs its own counter reset and its
//!   path extended via `BranchPath.child`. For now, bodies that contain a
//!   group abort with [`LiftError::GroupUnsupported`] so the caller can
//!   fail gracefully until branch lifting lands.
//! - User-defined helpers that also emit effects: their lifted form is
//!   needed to close the call graph. v0 doesn't recurse into helpers.

use std::collections::HashMap;

use super::effect_classification::{EffectDimension, classify};
use crate::ast::{Expr, FnBody, Literal, MatchArm, Spanned, Stmt};

/// Configuration describing how each effect maps to a lifted callable.
#[derive(Debug, Clone)]
pub struct LiftConfig {
    /// Identifier name in the generated AST for the current `BranchPath`
    /// parameter. Specs conventionally use `"path"`.
    pub path_name: String,
    /// Map from effect method (`"Random.int"`) → local binding name
    /// (`"rnd"`) the lifted body should call in place of the effect.
    pub oracles: HashMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiftError {
    /// An effect is used in the body but the [`LiftConfig`] has no oracle
    /// binding for it. Proof export should surface this as a diagnostic
    /// pointing the user at the missing `given` clause.
    MissingOracle { method: String },
    /// The body contains a `!` / `?!` group. Branch-aware lifting is
    /// deferred — for now the caller treats this as "can't lift in v0".
    GroupUnsupported,
    /// The body references an unclassified effect. `check_verify_blocks`
    /// already rejects these earlier, but the lifter re-checks to keep
    /// the invariant local.
    UnclassifiedEffect { method: String },
}

/// Lift a function body under the given configuration.
pub fn lift_body(body: &FnBody, cfg: &LiftConfig) -> Result<FnBody, LiftError> {
    let mut counter: u32 = 0;
    let mut new_stmts = Vec::with_capacity(body.stmts().len());
    for stmt in body.stmts() {
        new_stmts.push(lift_stmt(stmt, cfg, &mut counter)?);
    }
    Ok(FnBody::Block(new_stmts))
}

fn lift_stmt(stmt: &Stmt, cfg: &LiftConfig, counter: &mut u32) -> Result<Stmt, LiftError> {
    Ok(match stmt {
        Stmt::Expr(expr) => Stmt::Expr(lift_expr(expr, cfg, counter)?),
        Stmt::Binding(name, ann, expr) => {
            Stmt::Binding(name.clone(), ann.clone(), lift_expr(expr, cfg, counter)?)
        }
    })
}

fn lift_expr(
    expr: &Spanned<Expr>,
    cfg: &LiftConfig,
    counter: &mut u32,
) -> Result<Spanned<Expr>, LiftError> {
    let new_node = match &expr.node {
        // Leaves — nothing to do.
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            expr.node.clone()
        }

        Expr::InterpolatedStr(parts) => {
            let mut new_parts = Vec::with_capacity(parts.len());
            for p in parts {
                new_parts.push(match p {
                    crate::ast::StrPart::Literal(s) => crate::ast::StrPart::Literal(s.clone()),
                    crate::ast::StrPart::Parsed(inner) => {
                        crate::ast::StrPart::Parsed(Box::new(lift_expr(inner, cfg, counter)?))
                    }
                });
            }
            Expr::InterpolatedStr(new_parts)
        }

        Expr::Attr(obj, field) => {
            Expr::Attr(Box::new(lift_expr(obj, cfg, counter)?), field.clone())
        }

        Expr::FnCall(callee, args) => {
            // Resolve the callee name if it's a classified effect. Walk
            // Attr / Ident / Resolved shapes; anything else is a dynamic
            // or higher-order call we pass through after recursing args.
            if let Some(effect_name) = effect_method_name(&callee.node) {
                lift_classified_call(expr, &effect_name, args, cfg, counter)?
            } else {
                let new_callee = lift_expr(callee, cfg, counter)?;
                let new_args = lift_args(args, cfg, counter)?;
                Expr::FnCall(Box::new(new_callee), new_args)
            }
        }

        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(lift_expr(l, cfg, counter)?),
            Box::new(lift_expr(r, cfg, counter)?),
        ),

        Expr::Match { subject, arms } => {
            let new_subject = lift_expr(subject, cfg, counter)?;
            let mut new_arms = Vec::with_capacity(arms.len());
            for arm in arms {
                // v0: counter continues across arms — this is correct for
                // cases-style `match` on a runtime value (only one arm
                // executes, but statically we don't know which). Branch
                // lifting in a later commit gives each arm its own counter
                // under a branch-aware path extension.
                new_arms.push(MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(lift_expr(&arm.body, cfg, counter)?),
                });
            }
            Expr::Match {
                subject: Box::new(new_subject),
                arms: new_arms,
            }
        }

        Expr::Constructor(name, Some(arg)) => {
            Expr::Constructor(name.clone(), Some(Box::new(lift_expr(arg, cfg, counter)?)))
        }

        Expr::ErrorProp(inner) => Expr::ErrorProp(Box::new(lift_expr(inner, cfg, counter)?)),

        Expr::List(elems) => Expr::List(lift_args(elems, cfg, counter)?),
        Expr::Tuple(items) => Expr::Tuple(lift_args(items, cfg, counter)?),

        Expr::IndependentProduct(_, _) => return Err(LiftError::GroupUnsupported),

        Expr::MapLiteral(entries) => {
            let mut new_entries = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                new_entries.push((lift_expr(k, cfg, counter)?, lift_expr(v, cfg, counter)?));
            }
            Expr::MapLiteral(new_entries)
        }

        Expr::RecordCreate { type_name, fields } => {
            let mut new_fields = Vec::with_capacity(fields.len());
            for (name, value) in fields {
                new_fields.push((name.clone(), lift_expr(value, cfg, counter)?));
            }
            Expr::RecordCreate {
                type_name: type_name.clone(),
                fields: new_fields,
            }
        }

        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            let mut new_updates = Vec::with_capacity(updates.len());
            for (name, value) in updates {
                new_updates.push((name.clone(), lift_expr(value, cfg, counter)?));
            }
            Expr::RecordUpdate {
                type_name: type_name.clone(),
                base: Box::new(lift_expr(base, cfg, counter)?),
                updates: new_updates,
            }
        }

        Expr::TailCall(inner) => {
            let new_args = lift_args(&inner.args, cfg, counter)?;
            Expr::TailCall(Box::new(crate::ast::TailCallData {
                target: inner.target.clone(),
                args: new_args,
            }))
        }
    };
    Ok(Spanned {
        node: new_node,
        line: expr.line,
    })
}

fn lift_args(
    args: &[Spanned<Expr>],
    cfg: &LiftConfig,
    counter: &mut u32,
) -> Result<Vec<Spanned<Expr>>, LiftError> {
    let mut out = Vec::with_capacity(args.len());
    for a in args {
        out.push(lift_expr(a, cfg, counter)?);
    }
    Ok(out)
}

fn lift_classified_call(
    original: &Spanned<Expr>,
    effect_name: &str,
    args: &[Spanned<Expr>],
    cfg: &LiftConfig,
    counter: &mut u32,
) -> Result<Expr, LiftError> {
    let classification = match classify(effect_name) {
        Some(c) => c,
        None => {
            // Not a classified effect — treat as a regular call. This
            // covers user-defined helper `Foo.bar` references the
            // `effect_method_name` heuristic falsely matched.
            let new_args = lift_args(args, cfg, counter)?;
            // Reconstruct the original callee expression (we already know
            // the shape because it matched our `Attr` heuristic).
            let callee_expr = rebuild_dotted_callee(effect_name, original);
            return Ok(Expr::FnCall(Box::new(callee_expr), new_args));
        }
    };

    match classification.dimension {
        EffectDimension::Output => {
            // Output effects are not lifted in this transform; they stay
            // as-is and are handled separately by the trace-context
            // elaborator (separate commit).
            let new_args = lift_args(args, cfg, counter)?;
            let callee_expr = rebuild_dotted_callee(effect_name, original);
            Ok(Expr::FnCall(Box::new(callee_expr), new_args))
        }
        EffectDimension::Snapshot => {
            let oracle_name =
                cfg.oracles
                    .get(effect_name)
                    .ok_or_else(|| LiftError::MissingOracle {
                        method: effect_name.to_string(),
                    })?;
            let new_args = lift_args(args, cfg, counter)?;
            Ok(Expr::FnCall(
                Box::new(Spanned {
                    node: Expr::Ident(oracle_name.clone()),
                    line: original.line,
                }),
                new_args,
            ))
        }
        EffectDimension::Generative | EffectDimension::GenerativeOutput => {
            let oracle_name =
                cfg.oracles
                    .get(effect_name)
                    .ok_or_else(|| LiftError::MissingOracle {
                        method: effect_name.to_string(),
                    })?;
            let current_counter = *counter;
            *counter += 1;
            let path_arg = Spanned {
                node: Expr::Ident(cfg.path_name.clone()),
                line: original.line,
            };
            let counter_arg = Spanned {
                node: Expr::Literal(Literal::Int(current_counter as i64)),
                line: original.line,
            };
            let mut new_args = vec![path_arg, counter_arg];
            new_args.extend(lift_args(args, cfg, counter)?);
            Ok(Expr::FnCall(
                Box::new(Spanned {
                    node: Expr::Ident(oracle_name.clone()),
                    line: original.line,
                }),
                new_args,
            ))
        }
    }
}

fn effect_method_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Attr(obj, field) => {
            let head = match &obj.node {
                Expr::Ident(s) => s.clone(),
                _ => return None,
            };
            Some(format!("{}.{}", head, field))
        }
        _ => None,
    }
}

fn rebuild_dotted_callee(full_name: &str, from: &Spanned<Expr>) -> Spanned<Expr> {
    // Rebuild `Namespace.method` as Attr(Ident(Namespace), method).
    let (head, tail) = match full_name.rsplit_once('.') {
        Some((h, t)) => (h, t),
        None => (full_name, ""),
    };
    Spanned {
        node: Expr::Attr(
            Box::new(Spanned {
                node: Expr::Ident(head.to_string()),
                line: from.line,
            }),
            tail.to_string(),
        ),
        line: from.line,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse_body(src: &str) -> FnBody {
        let full = format!("fn __lift_test() -> Unit\n{}\n", src);
        let mut lexer = Lexer::new(&full);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = Parser::new(tokens);
        let items = parser.parse().expect("parse");
        let crate::ast::TopLevel::FnDef(fd) = items.into_iter().next().unwrap() else {
            panic!("expected fn def");
        };
        fd.body.as_ref().clone()
    }

    fn simple_cfg_with(oracles: &[(&str, &str)]) -> LiftConfig {
        LiftConfig {
            path_name: "path".to_string(),
            oracles: oracles
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
        }
    }

    fn assert_looks_like_oracle_call(
        expr: &Expr,
        oracle_name: &str,
        expected_arg_count: usize,
    ) -> Vec<Expr> {
        match expr {
            Expr::FnCall(callee, args) => {
                match &callee.node {
                    Expr::Ident(n) => assert_eq!(n, oracle_name, "wrong oracle name"),
                    other => panic!("expected Ident callee, got {:?}", other),
                }
                assert_eq!(args.len(), expected_arg_count, "oracle call arity");
                args.iter().map(|s| s.node.clone()).collect()
            }
            other => panic!("expected FnCall, got {:?}", other),
        }
    }

    #[test]
    fn lift_single_random_int_call_threads_path_and_counter() {
        let body = parse_body("    Random.int(1, 6)");
        let cfg = simple_cfg_with(&[("Random.int", "rnd")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
            panic!("expected one expr stmt");
        };
        let args = assert_looks_like_oracle_call(&tail.node, "rnd", 4);
        match &args[0] {
            Expr::Ident(n) => assert_eq!(n, "path"),
            other => panic!("expected path Ident, got {:?}", other),
        }
        match &args[1] {
            Expr::Literal(Literal::Int(0)) => {}
            other => panic!("expected counter literal 0, got {:?}", other),
        }
        match &args[2] {
            Expr::Literal(Literal::Int(1)) => {}
            other => panic!("expected min 1, got {:?}", other),
        }
        match &args[3] {
            Expr::Literal(Literal::Int(6)) => {}
            other => panic!("expected max 6, got {:?}", other),
        }
    }

    #[test]
    fn lift_tuple_of_three_random_int_increments_counter() {
        let body = parse_body("    (Random.int(1, 6), Random.int(1, 6), Random.int(1, 6))");
        let cfg = simple_cfg_with(&[("Random.int", "rnd")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
            panic!("expected one expr stmt");
        };
        let Expr::Tuple(elems) = &tail.node else {
            panic!("expected tuple");
        };
        assert_eq!(elems.len(), 3);
        for (i, e) in elems.iter().enumerate() {
            let args = assert_looks_like_oracle_call(&e.node, "rnd", 4);
            match &args[1] {
                Expr::Literal(Literal::Int(n)) => assert_eq!(*n as usize, i),
                other => panic!("counter slot {} had {:?}", i, other),
            }
        }
    }

    #[test]
    fn lift_snapshot_effect_has_no_path_or_counter() {
        let body = parse_body("    Args.get()");
        let cfg = simple_cfg_with(&[("Args.get", "args")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
            panic!("expected one expr stmt");
        };
        // Snapshot: capability reader — no BranchPath, no counter.
        let args = assert_looks_like_oracle_call(&tail.node, "args", 0);
        assert!(args.is_empty());
    }

    #[test]
    fn lift_output_effect_is_left_alone() {
        let body = parse_body("    Console.print(\"hi\")");
        let cfg = simple_cfg_with(&[]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
            panic!("expected one expr stmt");
        };
        // Console.print unchanged — it's output and goes through the
        // trace-context elaborator in a separate pass.
        let Expr::FnCall(callee, _) = &tail.node else {
            panic!("expected FnCall");
        };
        let Expr::Attr(head, field) = &callee.node else {
            panic!("expected Attr callee");
        };
        match &head.node {
            Expr::Ident(n) => assert_eq!(n, "Console"),
            other => panic!("expected Console head, got {:?}", other),
        }
        assert_eq!(field, "print");
    }

    #[test]
    fn lift_non_effect_call_passes_through() {
        let body = parse_body("    someHelper(1, 2, 3)");
        let cfg = simple_cfg_with(&[]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
            panic!("expected one expr stmt");
        };
        let Expr::FnCall(callee, args) = &tail.node else {
            panic!("expected FnCall");
        };
        match &callee.node {
            Expr::Ident(n) => assert_eq!(n, "someHelper"),
            other => panic!("expected Ident callee, got {:?}", other),
        }
        assert_eq!(args.len(), 3);
    }

    #[test]
    fn lift_unmapped_generative_effect_is_an_error() {
        let body = parse_body("    Random.int(1, 6)");
        let cfg = simple_cfg_with(&[]); // no oracle for Random.int
        let err = lift_body(&body, &cfg).unwrap_err();
        assert_eq!(
            err,
            LiftError::MissingOracle {
                method: "Random.int".to_string(),
            }
        );
    }

    #[test]
    fn lift_independent_product_is_unsupported_in_v0() {
        let body = parse_body("    (Random.int(1, 6), Random.int(1, 6))!");
        let cfg = simple_cfg_with(&[("Random.int", "rnd")]);
        let err = lift_body(&body, &cfg).unwrap_err();
        assert_eq!(err, LiftError::GroupUnsupported);
    }

    #[test]
    fn lift_multiple_mixed_dimensions_threads_counter_only_for_generative() {
        let body =
            parse_body("    a = Args.get()\n    b = Random.int(1, 6)\n    Random.int(1, 100)");
        let cfg = simple_cfg_with(&[("Args.get", "argsOracle"), ("Random.int", "rndOracle")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let stmts = lifted.stmts();
        assert_eq!(stmts.len(), 3);

        // let a = argsOracle()   — counter untouched
        let Stmt::Binding(_, _, expr) = &stmts[0] else {
            panic!("first stmt not binding");
        };
        assert_looks_like_oracle_call(&expr.node, "argsOracle", 0);

        // let b = rndOracle(path, 0, 1, 6)
        let Stmt::Binding(_, _, expr) = &stmts[1] else {
            panic!("second stmt not binding");
        };
        let args = assert_looks_like_oracle_call(&expr.node, "rndOracle", 4);
        match &args[1] {
            Expr::Literal(Literal::Int(0)) => {}
            other => panic!("expected counter 0, got {:?}", other),
        }

        // rndOracle(path, 1, 1, 100)   — counter bumped to 1
        let Stmt::Expr(expr) = &stmts[2] else {
            panic!("third stmt not expr");
        };
        let args = assert_looks_like_oracle_call(&expr.node, "rndOracle", 4);
        match &args[1] {
            Expr::Literal(Literal::Int(1)) => {}
            other => panic!("expected counter 1, got {:?}", other),
        }
    }

    #[test]
    fn lift_does_not_recurse_on_unclassified_dotted_helper_calls() {
        // `Foo.bar(x)` looks like an effect method to the name heuristic
        // but isn't classified → treated as a plain helper call, not
        // an oracle substitution or an error.
        let body = parse_body("    Foo.bar(3)");
        let cfg = simple_cfg_with(&[]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
            panic!("expected one expr stmt");
        };
        let Expr::FnCall(callee, _) = &tail.node else {
            panic!("expected FnCall");
        };
        let Expr::Attr(head, field) = &callee.node else {
            panic!("expected Attr callee");
        };
        match &head.node {
            Expr::Ident(n) => assert_eq!(n, "Foo"),
            other => panic!("expected Foo head, got {:?}", other),
        }
        assert_eq!(field, "bar");
    }
}
