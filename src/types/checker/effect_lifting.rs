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
//! Scope:
//!
//! - Generative effects: call-site replaced by `oracle(path, counter, args...)`.
//!   Counter starts at 0 within a branch scope; path is a caller-provided
//!   expression (the top-level `path_name` at the body root, extended via
//!   `BranchPath.child(parent, idx)` when descending into a `!`/`?!`
//!   branch).
//! - Snapshot effects: call-site replaced by `capability(args...)` (no path,
//!   no counter — snapshots are schedule-invariant by definition).
//! - Output effects: **left alone** by this transform — they're asserted
//!   about via the trace API in a separate elaboration path, not lifted to
//!   an oracle.
//! - `!` / `?!` groups: each branch is lifted with a fresh counter and an
//!   extended path via `BranchPath.child`. The `IndependentProduct`
//!   wrapper is preserved so the tuple / error-prop semantics stay
//!   first-class in the lifted AST.
//!
//! Not yet handled (follow-up commits):
//!
//! - User-defined helpers that also emit effects: their lifted form is
//!   needed to close the call graph. v0 doesn't recurse into helpers.

use std::collections::HashMap;
use std::sync::Arc;

use super::effect_classification::{EffectDimension, classify, oracle_signature};
use crate::ast::{Expr, FnBody, FnDef, Literal, MatchArm, SourceLine, Spanned, Stmt};
use crate::types::Type;

/// Configuration describing how each effect maps to a lifted callable.
#[derive(Debug, Clone)]
pub struct LiftConfig {
    /// Identifier name in the generated AST for the current `BranchPath`
    /// parameter. Specs conventionally use `"path"`.
    pub path_name: String,
    /// Map from effect method (`"Random.int"`) → local binding name
    /// (`"rnd"`) the lifted body should call in place of the effect.
    pub oracles: HashMap<String, String>,
    /// Oracle v1: effectful user-defined helpers visible from this
    /// body. Name → list of effect methods the helper declares. Call
    /// sites to these fns in the lifted body get `(path, oracle...)`
    /// args injected so the callee's lifted form sees them. Without
    /// this, proof export emits `helper()` bare and the typechecker
    /// on Lean / Dafny rejects it (arity mismatch). Empty for v0
    /// tests that don't have helpers.
    pub effectful_helpers: HashMap<String, Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiftError {
    /// An effect is used in the body but the [`LiftConfig`] has no oracle
    /// binding for it. Proof export should surface this as a diagnostic
    /// pointing the user at the missing `given` clause.
    MissingOracle { method: String },
    /// The body references an unclassified effect. `check_verify_blocks`
    /// already rejects these earlier, but the lifter re-checks to keep
    /// the invariant local.
    UnclassifiedEffect { method: String },
}

/// Lift a function body under the given configuration.
pub fn lift_body(body: &FnBody, cfg: &LiftConfig) -> Result<FnBody, LiftError> {
    let mut counter: u32 = 0;
    let root_path = spanned(Expr::Ident(cfg.path_name.clone()), 0);
    let new_stmts = lift_stmts(body.stmts(), cfg, &root_path, &mut counter)?;
    Ok(FnBody::Block(new_stmts))
}

/// Lift a sequence of statements, CPS-desugaring `?!` when it
/// appears. Each `?!` splits the stream: the error-propagating
/// product becomes a nested match on the lifted branches, and all
/// subsequent statements land inside the innermost Ok arm as the
/// continuation. First-Err-wins semantics (complete mode per
/// `docs/independence.md`) is preserved on the value side; compiler-
/// level lemma 2 (deterministic aggregation) justifies the
/// sequentialisation at proof level.
///
/// `?!` at a tail position (last stmt) wraps the innermost arm in a
/// `Result.Ok(tuple)` so the fn returns `Result<(T1, …, Tn), E>`.
/// `?!` inside a nested `?!` is rejected in v1 — the outer CPS
/// continuation would need a second-level rewrite; deferred.
fn lift_stmts(
    stmts: &[Stmt],
    cfg: &LiftConfig,
    path_expr: &Spanned<Expr>,
    counter: &mut u32,
) -> Result<Vec<Stmt>, LiftError> {
    let mut out = Vec::with_capacity(stmts.len());
    for (i, stmt) in stmts.iter().enumerate() {
        if let Some(cps) = try_lift_error_prop_stmt(stmt, &stmts[i + 1..], cfg, path_expr, counter)?
        {
            out.push(cps);
            return Ok(out);
        }
        if let Some(cps) =
            try_lift_question_op_stmt(stmt, &stmts[i + 1..], cfg, path_expr, counter)?
        {
            out.push(cps);
            return Ok(out);
        }
        out.push(lift_stmt(stmt, cfg, path_expr, counter)?);
    }
    Ok(out)
}

/// Lower a bare `?` operator at statement position into a `match` on
/// the inner expression's `Result` value. Two shapes:
///   `cmd = parseCommand(line)?; <rest>` becomes
///     `match parseCommand(line) { Ok(cmd) -> <rest>; Err(e) -> Err(e) }`
///   `parseCommand(line)?` (tail) becomes
///     `match parseCommand(line) { Ok(_v) -> Ok(_v); Err(e) -> Err(e) }`
/// (the tail form is equivalent to the inner expression itself but
/// going through the match keeps the Result wrapping uniform regardless
/// of how the surrounding emitter renders it).
fn try_lift_question_op_stmt(
    stmt: &Stmt,
    rest: &[Stmt],
    cfg: &LiftConfig,
    path_expr: &Spanned<Expr>,
    counter: &mut u32,
) -> Result<Option<Stmt>, LiftError> {
    use crate::ast::{MatchArm, Pattern};

    let (bind_name, inner, line, is_tail) = match stmt {
        Stmt::Binding(name, _, expr) => match &expr.node {
            Expr::ErrorProp(inner) => (Some(name.clone()), inner.clone(), expr.line, false),
            _ => return Ok(None),
        },
        Stmt::Expr(expr) if rest.is_empty() => match &expr.node {
            Expr::ErrorProp(inner) => (None, inner.clone(), expr.line, true),
            _ => return Ok(None),
        },
        _ => return Ok(None),
    };

    let lifted_inner = lift_expr(&inner, cfg, path_expr, counter)?;

    let ok_value_name = bind_name.unwrap_or_else(|| "__qm_value".to_string());
    let err_name = format!("__qm_err_{}", *counter);
    *counter += 1;

    let continuation = if is_tail {
        spanned(
            Expr::Constructor(
                "Result.Ok".to_string(),
                Some(Box::new(spanned(Expr::Ident(ok_value_name.clone()), line))),
            ),
            line,
        )
    } else {
        let lifted_rest = lift_stmts(rest, cfg, path_expr, counter)?;
        stmts_to_let_chain(&lifted_rest, line)
    };

    let ok_arm = MatchArm {
        pattern: Pattern::Constructor("Result.Ok".to_string(), vec![ok_value_name]),
        body: Box::new(continuation),
    };
    let err_arm = MatchArm {
        pattern: Pattern::Constructor("Result.Err".to_string(), vec![err_name.clone()]),
        body: Box::new(spanned(
            Expr::Constructor(
                "Result.Err".to_string(),
                Some(Box::new(spanned(Expr::Ident(err_name), line))),
            ),
            line,
        )),
    };

    let match_expr = spanned(
        Expr::Match {
            subject: Box::new(lifted_inner),
            arms: vec![ok_arm, err_arm],
        },
        line,
    );
    Ok(Some(Stmt::Expr(match_expr)))
}

/// If `stmt` is a binding or tail expression whose RHS is
/// `(a, b, …)?!`, return a single CPS-desugared statement folding
/// `rest` into the innermost Ok arm. Otherwise `None`, letting the
/// caller lift the stmt normally.
fn try_lift_error_prop_stmt(
    stmt: &Stmt,
    rest: &[Stmt],
    cfg: &LiftConfig,
    path_expr: &Spanned<Expr>,
    counter: &mut u32,
) -> Result<Option<Stmt>, LiftError> {
    let (bind_name, items, line, is_tail) = match stmt {
        Stmt::Binding(name, _, expr) => match &expr.node {
            Expr::IndependentProduct(items, true) => {
                (Some(name.clone()), items.clone(), expr.line, false)
            }
            _ => return Ok(None),
        },
        Stmt::Expr(expr) if rest.is_empty() => match &expr.node {
            Expr::IndependentProduct(items, true) => (None, items.clone(), expr.line, true),
            _ => return Ok(None),
        },
        _ => return Ok(None),
    };

    // Lift each branch with its own BranchPath.child(path, i) and a
    // fresh per-branch oracle counter.
    let mut lifted_items: Vec<Spanned<Expr>> = Vec::with_capacity(items.len());
    for (i, item) in items.iter().enumerate() {
        let branch_path = branch_child_path(path_expr, i as u32, item.line);
        let mut branch_counter: u32 = 0;
        lifted_items.push(lift_expr(item, cfg, &branch_path, &mut branch_counter)?);
    }

    // Build the continuation: innermost Ok arm.
    let inner_continuation = if is_tail {
        // `(a, b, …)?!` at tail — return `Result.Ok(tuple)`.
        let tuple_items: Vec<Spanned<Expr>> = (0..items.len())
            .map(|i| spanned(Expr::Ident(cps_binding(i)), line))
            .collect();
        let tuple_expr = spanned(Expr::Tuple(tuple_items), line);
        spanned(
            Expr::Constructor("Result.Ok".to_string(), Some(Box::new(tuple_expr))),
            line,
        )
    } else {
        // `pair = (a, b, …)?!; rest` — reconstruct `pair` from the
        // unwrapped bindings, lift `rest`, wrap in a block expr.
        let bind_name = bind_name.expect("binding case has name");
        let tuple_items: Vec<Spanned<Expr>> = (0..items.len())
            .map(|i| spanned(Expr::Ident(cps_binding(i)), line))
            .collect();
        let tuple_expr = spanned(Expr::Tuple(tuple_items), line);
        let reconstruct = Stmt::Binding(bind_name, None, tuple_expr);
        let mut block_stmts = vec![reconstruct];
        let lifted_rest = lift_stmts(rest, cfg, path_expr, counter)?;
        block_stmts.extend(lifted_rest);
        stmts_to_expr(&block_stmts, line)
    };

    // Fold innermost → outermost: for each branch i (right-to-left),
    // wrap `cont` in `match lifted_items[i] with Err(e) -> Err(e) |
    // Ok(cps_binding(i)) -> cont`.
    let mut cont = inner_continuation;
    for (i, lifted) in lifted_items.into_iter().enumerate().rev() {
        cont = result_match_cascade(lifted, cps_binding(i), cont, line);
    }
    Ok(Some(Stmt::Expr(cont)))
}

/// Oracle v1: if `callee` resolves to a known effectful user helper
/// (from `cfg.effectful_helpers`), return its declared effect list.
/// Otherwise `None` — caller proceeds with standard non-injected
/// lowering.
fn callee_helper_effects(callee: &Expr, cfg: &LiftConfig) -> Option<Vec<String>> {
    let name = match callee {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
        _ => return None,
    };
    cfg.effectful_helpers.get(&name).cloned()
}

/// Deterministic binding name for the k-th `?!` branch's unwrapped
/// value. Underscore-prefixed so it never collides with user idents.
fn cps_binding(k: usize) -> String {
    format!("ep_{}", k)
}

/// Oracle v1: build a nested `match` cascade from `items` that is a
/// self-contained `Result<(T1, …, Tn), E>` expression. Used when `?!`
/// appears at expression position (e.g. nested inside another `?!`,
/// or as a branch of an outer `!`). Innermost arm wraps the
/// unwrapped tuple in `Result.Ok`. First-Err-wins.
fn build_error_prop_cascade(items: Vec<Spanned<Expr>>, line: SourceLine) -> Spanned<Expr> {
    let tuple_items: Vec<Spanned<Expr>> = (0..items.len())
        .map(|i| spanned(Expr::Ident(cps_binding(i)), line))
        .collect();
    let tuple_expr = spanned(Expr::Tuple(tuple_items), line);
    let ok_wrap = spanned(
        Expr::Constructor("Result.Ok".to_string(), Some(Box::new(tuple_expr))),
        line,
    );
    let mut cont = ok_wrap;
    for (i, lifted) in items.into_iter().enumerate().rev() {
        cont = result_match_cascade(lifted, cps_binding(i), cont, line);
    }
    cont
}

/// Build `match subject with Result.Err(e) -> Result.Err(e) |
/// Result.Ok(<binding>) -> <body>` as a single Match expression.
fn result_match_cascade(
    subject: Spanned<Expr>,
    binding: String,
    body: Spanned<Expr>,
    line: SourceLine,
) -> Spanned<Expr> {
    use crate::ast::{MatchArm, Pattern};
    let err_arm = MatchArm {
        pattern: Pattern::Constructor("Result.Err".to_string(), vec!["err_".to_string()]),
        body: Box::new(spanned(
            Expr::Constructor(
                "Result.Err".to_string(),
                Some(Box::new(spanned(Expr::Ident("err_".to_string()), line))),
            ),
            line,
        )),
    };
    let ok_arm = MatchArm {
        pattern: Pattern::Constructor("Result.Ok".to_string(), vec![binding]),
        body: Box::new(body),
    };
    spanned(
        Expr::Match {
            subject: Box::new(subject),
            arms: vec![err_arm, ok_arm],
        },
        line,
    )
}

/// Wrap a statement block in an expression. For a single tail
/// expression, return it directly; for `binding; expr; …` use the
/// Aver idiom where the final expression is the block's value —
/// represented as nested bindings terminating in the tail expr. We
/// keep the Vec<Stmt> form and return it as a Match-subject which
/// the emitter treats as a sequential block.
fn stmts_to_expr(stmts: &[Stmt], line: SourceLine) -> Spanned<Expr> {
    // Aver's AST doesn't have a `Block` expression; blocks only
    // appear as `FnBody`. For the CPS-desugar continuation we
    // reconstitute a single expression via a trivial Match on Unit
    // whose one arm's body evaluates the block's stmts in scope.
    // Codegen treats Match with an Ident subject's binding as
    // sequential let-chains; for our purposes the simplest faithful
    // lowering is a synthetic helper fn call — but that would need
    // lift_fn_def cooperation. Instead, use an immediately-destructured
    // Match on `Unit()` with a wildcard arm containing the stmts.
    // Since the AST only permits one expression in `arm.body`, we
    // chain bindings via nested Match arms — equivalent to let*.
    stmts_to_let_chain(stmts, line)
}

/// Nest block stmts as let-chains using Match(Unit, [_ -> body]).
/// Builds equivalent to `let a = va; let b = vb; tail`.
fn stmts_to_let_chain(stmts: &[Stmt], line: SourceLine) -> Spanned<Expr> {
    use crate::ast::{Literal, MatchArm, Pattern};
    // Find the tail expression and the preceding bindings.
    let (tail, bindings) = match stmts.split_last() {
        Some((Stmt::Expr(e), rest)) => (e.clone(), rest),
        Some((Stmt::Binding(_, _, _), _)) => {
            // Last stmt is a binding — unusual for a tail-reachable
            // position. Synthesize Unit tail.
            (spanned(Expr::Literal(Literal::Unit), line), stmts)
        }
        None => (spanned(Expr::Literal(Literal::Unit), line), &[][..]),
    };
    // Wrap each preceding binding (r-to-l) as a Match on the binding
    // value where the single arm binds the name and evaluates the
    // accumulated tail.
    let mut acc = tail;
    for stmt in bindings.iter().rev() {
        match stmt {
            Stmt::Binding(name, _, value) => {
                let arm = MatchArm {
                    pattern: Pattern::Ident(name.clone()),
                    body: Box::new(acc),
                };
                acc = spanned(
                    Expr::Match {
                        subject: Box::new(value.clone()),
                        arms: vec![arm],
                    },
                    line,
                );
            }
            Stmt::Expr(e) => {
                // Non-tail bare expression — sequence it before the
                // accumulated tail by binding to `_` via a wildcard-
                // style Match arm.
                let arm = MatchArm {
                    pattern: Pattern::Wildcard,
                    body: Box::new(acc),
                };
                acc = spanned(
                    Expr::Match {
                        subject: Box::new(e.clone()),
                        arms: vec![arm],
                    },
                    line,
                );
            }
        }
    }
    acc
}

fn spanned(node: Expr, line: SourceLine) -> Spanned<Expr> {
    Spanned::new(node, line)
}

/// Build the AST expression `BranchPath.child(parent_path, idx)`.
fn branch_child_path(parent: &Spanned<Expr>, idx: u32, line: SourceLine) -> Spanned<Expr> {
    let branch_path_ident = spanned(Expr::Ident("BranchPath".to_string()), line);
    let child_callee = spanned(
        Expr::Attr(Box::new(branch_path_ident), "child".to_string()),
        line,
    );
    spanned(
        Expr::FnCall(
            Box::new(child_callee),
            vec![
                parent.clone(),
                spanned(Expr::Literal(Literal::Int(idx as i64)), line),
            ],
        ),
        line,
    )
}

fn lift_stmt(
    stmt: &Stmt,
    cfg: &LiftConfig,
    path_expr: &Spanned<Expr>,
    counter: &mut u32,
) -> Result<Stmt, LiftError> {
    Ok(match stmt {
        Stmt::Expr(expr) => Stmt::Expr(lift_expr(expr, cfg, path_expr, counter)?),
        Stmt::Binding(name, ann, expr) => Stmt::Binding(
            name.clone(),
            ann.clone(),
            lift_expr(expr, cfg, path_expr, counter)?,
        ),
    })
}

fn lift_expr(
    expr: &Spanned<Expr>,
    cfg: &LiftConfig,
    path_expr: &Spanned<Expr>,
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
                    crate::ast::StrPart::Parsed(inner) => crate::ast::StrPart::Parsed(Box::new(
                        lift_expr(inner, cfg, path_expr, counter)?,
                    )),
                });
            }
            Expr::InterpolatedStr(new_parts)
        }

        Expr::Attr(obj, field) => Expr::Attr(
            Box::new(lift_expr(obj, cfg, path_expr, counter)?),
            field.clone(),
        ),

        Expr::FnCall(callee, args) => {
            // Resolve the callee name if it's a classified effect. Walk
            // Attr / Ident / Resolved shapes; anything else is a dynamic
            // or higher-order call we pass through after recursing args.
            if let Some(effect_name) = effect_method_name(&callee.node) {
                lift_classified_call(expr, &effect_name, args, cfg, path_expr, counter)?
            } else if let Some(helper_effects) = callee_helper_effects(&callee.node, cfg) {
                // Oracle v1: call site to an effectful user helper —
                // inject `(path, oracle...)` args so the call matches
                // the helper's lifted arity. Path is the current
                // scope's path expression (no `.child(path, i)` because
                // a helper call is a sequential point, not a fresh
                // branch); the helper's own `!`/`?!` bodies build
                // their own `BranchPath.child` threading.
                let new_callee = lift_expr(callee, cfg, path_expr, counter)?;
                let new_args = lift_args(args, cfg, path_expr, counter)?;
                let mut injected: Vec<Spanned<Expr>> = Vec::new();
                let needs_path = helper_effects.iter().any(|e| {
                    matches!(
                        classify(e).map(|c| c.dimension),
                        Some(EffectDimension::Generative | EffectDimension::GenerativeOutput)
                    )
                });
                if needs_path {
                    injected.push(path_expr.clone());
                }
                let mut seen = std::collections::HashSet::new();
                for e in &helper_effects {
                    if !seen.insert(e.clone()) {
                        continue;
                    }
                    let Some(c) = classify(e) else { continue };
                    if matches!(c.dimension, EffectDimension::Output) {
                        continue;
                    }
                    if let Some(oracle_name) = cfg.oracles.get(e) {
                        injected.push(spanned(Expr::Ident(oracle_name.clone()), expr.line));
                    }
                }
                injected.extend(new_args);
                Expr::FnCall(Box::new(new_callee), injected)
            } else {
                let new_callee = lift_expr(callee, cfg, path_expr, counter)?;
                let new_args = lift_args(args, cfg, path_expr, counter)?;
                Expr::FnCall(Box::new(new_callee), new_args)
            }
        }

        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(lift_expr(l, cfg, path_expr, counter)?),
            Box::new(lift_expr(r, cfg, path_expr, counter)?),
        ),

        Expr::Match { subject, arms } => {
            let new_subject = lift_expr(subject, cfg, path_expr, counter)?;
            let mut new_arms = Vec::with_capacity(arms.len());
            for arm in arms {
                // v0: counter continues across arms — this is correct for
                // cases-style `match` on a runtime value (only one arm
                // executes, but statically we don't know which). Branch
                // lifting in a later commit gives each arm its own counter
                // under a branch-aware path extension.
                new_arms.push(MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(lift_expr(&arm.body, cfg, path_expr, counter)?),
                });
            }
            Expr::Match {
                subject: Box::new(new_subject),
                arms: new_arms,
            }
        }

        Expr::Constructor(name, Some(arg)) => Expr::Constructor(
            name.clone(),
            Some(Box::new(lift_expr(arg, cfg, path_expr, counter)?)),
        ),

        Expr::ErrorProp(inner) => {
            Expr::ErrorProp(Box::new(lift_expr(inner, cfg, path_expr, counter)?))
        }

        Expr::List(elems) => Expr::List(lift_args(elems, cfg, path_expr, counter)?),
        Expr::Tuple(items) => Expr::Tuple(lift_args(items, cfg, path_expr, counter)?),

        Expr::IndependentProduct(elements, is_error_prop) => {
            // Each branch gets: fresh counter starting at 0, and an extended
            // path BranchPath.child(current_path, branch_index). Schedule-
            // invariance lemma 1 (branch locality) follows because each
            // branch's lifted form reads only from its own (path, counter)
            // slot of the oracle.
            let mut new_elements = Vec::with_capacity(elements.len());
            for (i, element) in elements.iter().enumerate() {
                let branch_path = branch_child_path(path_expr, i as u32, element.line);
                let mut branch_counter: u32 = 0;
                let lifted = lift_expr(element, cfg, &branch_path, &mut branch_counter)?;
                new_elements.push(lifted);
            }
            if *is_error_prop {
                // Oracle v1: `?!` at expression position desugars to a
                // nested match cascade whose innermost Ok arm wraps
                // the tuple in `Result.Ok`. Handles nested `?!` inside
                // another `?!` — each one becomes a Result-returning
                // expression that the outer CPS consumes uniformly.
                // Same first-Err semantics as the stmt-level rewrite.
                return Ok(build_error_prop_cascade(new_elements, expr.line));
            }
            Expr::IndependentProduct(new_elements, *is_error_prop)
        }

        Expr::MapLiteral(entries) => {
            let mut new_entries = Vec::with_capacity(entries.len());
            for (k, v) in entries {
                new_entries.push((
                    lift_expr(k, cfg, path_expr, counter)?,
                    lift_expr(v, cfg, path_expr, counter)?,
                ));
            }
            Expr::MapLiteral(new_entries)
        }

        Expr::RecordCreate { type_name, fields } => {
            let mut new_fields = Vec::with_capacity(fields.len());
            for (name, value) in fields {
                new_fields.push((name.clone(), lift_expr(value, cfg, path_expr, counter)?));
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
                new_updates.push((name.clone(), lift_expr(value, cfg, path_expr, counter)?));
            }
            Expr::RecordUpdate {
                type_name: type_name.clone(),
                base: Box::new(lift_expr(base, cfg, path_expr, counter)?),
                updates: new_updates,
            }
        }

        Expr::TailCall(inner) => {
            let new_args = lift_args(&inner.args, cfg, path_expr, counter)?;
            Expr::TailCall(Box::new(crate::ast::TailCallData {
                target: inner.target.clone(),
                args: new_args,
            }))
        }
    };
    Ok(Spanned::new(new_node, expr.line))
}

fn lift_args(
    args: &[Spanned<Expr>],
    cfg: &LiftConfig,
    path_expr: &Spanned<Expr>,
    counter: &mut u32,
) -> Result<Vec<Spanned<Expr>>, LiftError> {
    let mut out = Vec::with_capacity(args.len());
    for a in args {
        out.push(lift_expr(a, cfg, path_expr, counter)?);
    }
    Ok(out)
}

fn lift_classified_call(
    original: &Spanned<Expr>,
    effect_name: &str,
    args: &[Spanned<Expr>],
    cfg: &LiftConfig,
    path_expr: &Spanned<Expr>,
    counter: &mut u32,
) -> Result<Expr, LiftError> {
    let classification = match classify(effect_name) {
        Some(c) => c,
        None => {
            // Not a classified effect — treat as a regular call. This
            // covers user-defined helper `Foo.bar` references the
            // `effect_method_name` heuristic falsely matched.
            let new_args = lift_args(args, cfg, path_expr, counter)?;
            // Reconstruct the original callee expression (we already know
            // the shape because it matched our `Attr` heuristic).
            let callee_expr = rebuild_dotted_callee(effect_name, original);
            return Ok(Expr::FnCall(Box::new(callee_expr), new_args));
        }
    };

    match classification.dimension {
        EffectDimension::Output => {
            // Output effects have no semantic contribution to the
            // proof — they're trace-appending side effects. Replace
            // the call with `Unit` so the lifted body emits as pure
            // math both in Dafny and Lean (Dafny `function` happens
            // to drop non-tail Unit statements; Lean does not, so
            // without this replacement `Console.print(x)` leaks into
            // the emitted proof as an unresolved identifier).
            //
            // Still walk the args so any generative effects nested
            // inside them (e.g. `Console.print(Random.int(1,6))`)
            // advance the oracle counter. The VM evaluates args eagerly
            // and charges oracle reads against the surrounding branch
            // before the output emission; if the proof skipped that
            // counter bump, a subsequent `Random.int(...)` would claim
            // counter 0 while replay recorded it at counter 1, and the
            // theorem would fail to unify.
            for arg in args {
                lift_expr(arg, cfg, path_expr, counter)?;
            }
            Ok(Expr::Literal(crate::ast::Literal::Unit))
        }
        EffectDimension::Snapshot => {
            let oracle_name =
                cfg.oracles
                    .get(effect_name)
                    .ok_or_else(|| LiftError::MissingOracle {
                        method: effect_name.to_string(),
                    })?;
            let new_args = lift_args(args, cfg, path_expr, counter)?;
            Ok(Expr::FnCall(
                Box::new(Spanned::new(
                    Expr::Ident(oracle_name.clone()),
                    original.line,
                )),
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
            let path_arg = path_expr.clone();
            let counter_arg = Spanned::new(
                Expr::Literal(Literal::Int(current_counter as i64)),
                original.line,
            );
            let mut new_args = vec![path_arg, counter_arg];
            new_args.extend(lift_args(args, cfg, path_expr, counter)?);
            Ok(Expr::FnCall(
                Box::new(Spanned::new(
                    Expr::Ident(oracle_name.clone()),
                    original.line,
                )),
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

/// For each classified effect an original function declares, produce the
/// oracle / capability parameter to prepend to the lifted signature.
///
/// Returns a list of `(binding_name, type_annotation_string)` in the order
/// the effects appear in the input. Output-only effects are skipped (they
/// have no oracle; trace-API assertions handle them). Unclassified effects
/// produce an error — callers should reject effectful-law verify blocks
/// earlier, this is a defensive layer.
pub fn oracle_params_for_effects(
    effects: &[Spanned<String>],
) -> Result<Vec<(String, String)>, LiftError> {
    let mut seen = std::collections::HashSet::new();
    let mut out = Vec::new();
    for e in effects {
        let name = &e.node;
        if !seen.insert(name.clone()) {
            continue;
        }
        let Some(classification) = classify(name) else {
            return Err(LiftError::UnclassifiedEffect {
                method: name.clone(),
            });
        };
        match classification.dimension {
            EffectDimension::Output => continue,
            _ => {
                let binding = oracle_binding_name_for(name);
                let type_str = match oracle_signature(name) {
                    Some(t) => type_to_annotation(&t),
                    None => continue,
                };
                out.push((binding, type_str));
            }
        }
    }
    Ok(out)
}

/// Deterministic default oracle binding name from an effect method.
/// `Random.int` → `rnd_Random_int`; `Args.get` → `cap_Args_get`.
/// Callers that use `given name: E.m = [...]` override these by passing
/// their own [`LiftConfig::oracles`] map.
fn oracle_binding_name_for(effect: &str) -> String {
    let prefix = match classify(effect).map(|c| c.dimension) {
        Some(EffectDimension::Snapshot) => "cap",
        Some(EffectDimension::Generative | EffectDimension::GenerativeOutput) => "rnd",
        _ => "eff",
    };
    let sanitized = effect.replace('.', "_");
    format!("{}_{}", prefix, sanitized)
}

/// Render a [`Type`] as an Aver type-annotation string suitable for a
/// `FnDef.params` entry. Only supports the shapes that appear in lifted
/// oracle / capability signatures; richer types would need `type::display`
/// or similar.
pub fn type_to_annotation(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Named(n) => n.clone(),
        Type::Option(inner) => format!("Option<{}>", type_to_annotation(inner)),
        Type::Result(ok, err) => format!(
            "Result<{}, {}>",
            type_to_annotation(ok),
            type_to_annotation(err)
        ),
        Type::List(inner) => format!("List<{}>", type_to_annotation(inner)),
        Type::Vector(inner) => format!("Vector<{}>", type_to_annotation(inner)),
        Type::Map(k, v) => format!("Map<{}, {}>", type_to_annotation(k), type_to_annotation(v)),
        Type::Fn(params, ret, effects) => {
            let ps = params
                .iter()
                .map(type_to_annotation)
                .collect::<Vec<_>>()
                .join(", ");
            let r = type_to_annotation(ret);
            if effects.is_empty() {
                format!("Fn({}) -> {}", ps, r)
            } else {
                format!("Fn({}) -> {} ! [{}]", ps, r, effects.join(", "))
            }
        }
        Type::Unknown => "_".to_string(),
        // These shapes don't appear in v1 oracle/capability signatures —
        // if they show up we fall back to a legible-but-inexact rendering
        // rather than panicking.
        other => format!("/*{:?}*/", other),
    }
}

/// Lift a full [`FnDef`] into its pure, proof-ready form.
///
/// - The oracle / capability parameters for the function's declared
///   classified effects are prepended (plus a leading `path: BranchPath`
///   when any generative / generative-output effect is used).
/// - The body is rewritten via [`lift_body`] under a [`LiftConfig`] whose
///   `oracles` map reflects the prepended parameter names.
/// - The resulting `FnDef` has no effects declared — proof export treats
///   the lifted form as a pure function.
///
/// Returns `None` if the function's effect list is empty (nothing to lift).
pub fn lift_fn_def(fd: &FnDef) -> Result<Option<FnDef>, LiftError> {
    lift_fn_def_with_helpers(fd, &HashMap::new())
}

/// Lower pure `?!` products to explicit Result matches for proof backends.
///
/// Effectful functions use [`lift_fn_def_with_helpers`], which already does
/// this while replacing classified effects with oracle calls. Pure functions
/// can still contain `?!` over ordinary `Result`-returning helpers; proof
/// backends need the same value-level desugaring even though there are no
/// effects to lift.
pub fn lower_pure_question_bang_fn(fd: &FnDef) -> Result<Option<FnDef>, LiftError> {
    if !fd.effects.is_empty() || !body_contains_question_bang(&fd.body) {
        return Ok(None);
    }

    let cfg = LiftConfig {
        path_name: "path".to_string(),
        oracles: HashMap::new(),
        effectful_helpers: HashMap::new(),
    };
    let lowered_body = lift_body(&fd.body, &cfg)?;
    let mut lowered = fd.clone();
    lowered.body = Arc::new(lowered_body);
    Ok(Some(lowered))
}

fn body_contains_question_bang(body: &FnBody) -> bool {
    body.stmts().iter().any(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_contains_question_bang(expr),
    })
}

fn expr_contains_question_bang(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::IndependentProduct(_, true) => true,
        Expr::ErrorProp(_) => true,
        Expr::IndependentProduct(items, false) | Expr::List(items) | Expr::Tuple(items) => {
            items.iter().any(expr_contains_question_bang)
        }
        Expr::FnCall(callee, args) => {
            expr_contains_question_bang(callee) || args.iter().any(expr_contains_question_bang)
        }
        Expr::Attr(base, _) | Expr::Constructor(_, Some(base)) => expr_contains_question_bang(base),
        Expr::BinOp(_, left, right) => {
            expr_contains_question_bang(left) || expr_contains_question_bang(right)
        }
        Expr::Match { subject, arms } => {
            expr_contains_question_bang(subject)
                || arms
                    .iter()
                    .any(|arm| expr_contains_question_bang(&arm.body))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            crate::ast::StrPart::Parsed(expr) => expr_contains_question_bang(expr),
            crate::ast::StrPart::Literal(_) => false,
        }),
        Expr::MapLiteral(entries) => entries.iter().any(|(key, value)| {
            expr_contains_question_bang(key) || expr_contains_question_bang(value)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_contains_question_bang(value)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_contains_question_bang(base)
                || updates
                    .iter()
                    .any(|(_, value)| expr_contains_question_bang(value))
        }
        Expr::TailCall(data) => data.args.iter().any(expr_contains_question_bang),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            false
        }
    }
}

/// Oracle v1: lift an effectful fn with a known set of sibling
/// effectful helpers. Call sites to any helper in `helpers` get
/// `(path, oracle...)` args injected so their lifted arity matches.
/// Empty map = standalone lift (no helpers visible).
pub fn lift_fn_def_with_helpers(
    fd: &FnDef,
    helpers: &HashMap<String, Vec<String>>,
) -> Result<Option<FnDef>, LiftError> {
    if fd.effects.is_empty() {
        return Ok(None);
    }

    let oracle_params = oracle_params_for_effects(&fd.effects)?;

    // Decide whether we need the leading `path: BranchPath` parameter —
    // only generative / generative-output effects depend on it.
    let needs_path = fd.effects.iter().any(|e| {
        matches!(
            classify(&e.node).map(|c| c.dimension),
            Some(EffectDimension::Generative | EffectDimension::GenerativeOutput)
        )
    });

    let path_name = "path".to_string();
    let mut new_params: Vec<(String, String)> = Vec::new();
    if needs_path {
        new_params.push((path_name.clone(), "BranchPath".to_string()));
    }
    for (name, ty) in &oracle_params {
        new_params.push((name.clone(), ty.clone()));
    }
    new_params.extend(fd.params.iter().cloned());

    // Build an oracles map keyed by effect method name → param
    // binding. Dedupe the effect stream FIRST (matching what
    // `oracle_params_for_effects` does), otherwise for a body
    // declaring `! [Args.get, Args.get, Env.get]` the enumerated idx
    // drifts past the deduped param list — `Env.get` maps to the
    // `Random.int` slot that doesn't exist. Failure mode: lifted body
    // references `both` / wrong-typed oracle that the generated
    // Lean / Dafny can't resolve.
    let mut oracles_map: HashMap<String, String> = HashMap::new();
    let mut seen = std::collections::HashSet::new();
    let mut idx = 0usize;
    for e in fd.effects.iter().filter(|e| {
        classify(&e.node)
            .map(|c| !matches!(c.dimension, EffectDimension::Output))
            .unwrap_or(false)
    }) {
        if !seen.insert(e.node.clone()) {
            continue;
        }
        if let Some((name, _)) = oracle_params.get(idx) {
            oracles_map.insert(e.node.clone(), name.clone());
        }
        idx += 1;
    }
    let cfg = LiftConfig {
        path_name,
        oracles: oracles_map,
        effectful_helpers: helpers.clone(),
    };

    let lifted_body = lift_body(&fd.body, &cfg)?;

    Ok(Some(FnDef {
        name: fd.name.clone(),
        line: fd.line,
        params: new_params,
        return_type: fd.return_type.clone(),
        effects: Vec::new(), // lifted form is pure
        desc: fd.desc.clone(),
        body: Arc::new(lifted_body),
        resolution: None,
    }))
}

fn rebuild_dotted_callee(full_name: &str, from: &Spanned<Expr>) -> Spanned<Expr> {
    // Rebuild `Namespace.method` as Attr(Ident(Namespace), method).
    let (head, tail) = match full_name.rsplit_once('.') {
        Some((h, t)) => (h, t),
        None => (full_name, ""),
    };
    Spanned::new(
        Expr::Attr(
            Box::new(Spanned::new(Expr::Ident(head.to_string()), from.line)),
            tail.to_string(),
        ),
        from.line,
    )
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
            effectful_helpers: HashMap::new(),
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
        let [Stmt::Expr(tail)] = lifted.stmts() else {
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
        let [Stmt::Expr(tail)] = lifted.stmts() else {
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
        let [Stmt::Expr(tail)] = lifted.stmts() else {
            panic!("expected one expr stmt");
        };
        // Snapshot: capability reader — no BranchPath, no counter.
        let args = assert_looks_like_oracle_call(&tail.node, "args", 0);
        assert!(args.is_empty());
    }

    #[test]
    fn lift_output_effect_is_replaced_with_unit() {
        let body = parse_body("    Console.print(\"hi\")");
        let cfg = simple_cfg_with(&[]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = lifted.stmts() else {
            panic!("expected one expr stmt");
        };
        // Oracle v1: output effects have no semantic contribution to
        // the proof (they're trace-append only). The lifter replaces
        // them with `Unit` so the emitted body is pure math on both
        // backends. Runtime trace assertions go through the verify-
        // trace collector, not lifted proofs.
        match &tail.node {
            Expr::Literal(crate::ast::Literal::Unit) => {}
            other => panic!("expected Unit, got {:?}", other),
        }
    }

    #[test]
    fn lift_non_effect_call_passes_through() {
        let body = parse_body("    someHelper(1, 2, 3)");
        let cfg = simple_cfg_with(&[]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = lifted.stmts() else {
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
    fn lift_independent_product_threads_child_paths_and_resets_counter() {
        let body = parse_body("    (Random.int(1, 6), Random.int(1, 6))!");
        let cfg = simple_cfg_with(&[("Random.int", "rnd")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = lifted.stmts() else {
            panic!("expected one expr stmt");
        };
        let Expr::IndependentProduct(elems, is_error_prop) = &tail.node else {
            panic!("expected IndependentProduct, got {:?}", tail.node);
        };
        assert!(!is_error_prop, "plain `!`, not `?!`");
        assert_eq!(elems.len(), 2);

        for (i, el) in elems.iter().enumerate() {
            let args = assert_looks_like_oracle_call(&el.node, "rnd", 4);
            // Counter resets to 0 inside each branch (branch locality).
            match &args[1] {
                Expr::Literal(Literal::Int(0)) => {}
                other => panic!("branch {} counter should be 0, got {:?}", i, other),
            }
            // path arg should be BranchPath.child(path, i)
            let Expr::FnCall(callee, cargs) = &args[0] else {
                panic!("expected BranchPath.child(...) call in path arg");
            };
            let Expr::Attr(head, field) = &callee.node else {
                panic!("expected Attr callee");
            };
            match &head.node {
                Expr::Ident(n) => assert_eq!(n, "BranchPath"),
                other => panic!("expected BranchPath head, got {:?}", other),
            }
            assert_eq!(field, "child");
            assert_eq!(cargs.len(), 2);
            match &cargs[0].node {
                Expr::Ident(n) => assert_eq!(n, "path", "parent path should be root `path`"),
                other => panic!("expected path Ident, got {:?}", other),
            }
            match &cargs[1].node {
                Expr::Literal(Literal::Int(n)) => assert_eq!(*n as usize, i),
                other => panic!("branch idx should be {}, got {:?}", i, other),
            }
        }
    }

    #[test]
    fn lift_question_bang_product_cps_desugars_to_nested_match() {
        // Oracle v1: `?!` becomes a nested match cascade on
        // `Result`, not a bare IndependentProduct node. First Err
        // short-circuits; all Ok arm reconstructs the unwrapped
        // tuple. Complete-mode semantics is preserved on the value
        // side via lemma 2 (deterministic aggregation).
        let body = parse_body("    (Http.get(\"a\"), Http.get(\"b\"))?!");
        let cfg = simple_cfg_with(&[("Http.get", "http")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = lifted.stmts() else {
            panic!("expected one expr stmt");
        };
        let Expr::Match { arms, .. } = &tail.node else {
            panic!("expected Match cascade, got {:?}", tail.node);
        };
        assert_eq!(arms.len(), 2, "Err + Ok arms expected");
    }

    #[test]
    fn lift_nested_independent_product_builds_deweyish_child_path() {
        // Outer `!` has 3 elements; branch 2 is a nested `!` with 2 elements.
        // Effect calls inside the nested group must see a path of the shape
        // BranchPath.child(BranchPath.child(path, 2), i).
        let body = parse_body(
            "    (Random.int(1, 6), Random.int(1, 6), (Random.int(1, 6), Random.int(1, 6))!)!",
        );
        let cfg = simple_cfg_with(&[("Random.int", "rnd")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = lifted.stmts() else {
            panic!("expected one expr stmt");
        };
        let Expr::IndependentProduct(outer, _) = &tail.node else {
            panic!("expected outer IndependentProduct");
        };
        let Expr::IndependentProduct(inner, _) = &outer[2].node else {
            panic!("expected inner IndependentProduct in branch 2");
        };
        assert_eq!(inner.len(), 2);
        // The first inner branch's oracle call should have path:
        //   BranchPath.child(BranchPath.child(path, 2), 0)
        let args = assert_looks_like_oracle_call(&inner[0].node, "rnd", 4);
        let Expr::FnCall(outer_child_callee, outer_child_args) = &args[0] else {
            panic!("expected outer .child call");
        };
        let Expr::Attr(h, f) = &outer_child_callee.node else {
            panic!("expected Attr callee on outer .child")
        };
        assert!(matches!(&h.node, Expr::Ident(n) if n == "BranchPath"));
        assert_eq!(f, "child");
        assert_eq!(outer_child_args.len(), 2);
        // First arg to outer .child is the inner .child call:
        //   BranchPath.child(path, 2)
        let Expr::FnCall(inner_child_callee, inner_child_args) = &outer_child_args[0].node else {
            panic!("expected inner .child call");
        };
        let Expr::Attr(h2, f2) = &inner_child_callee.node else {
            panic!("expected Attr callee on inner .child");
        };
        assert!(matches!(&h2.node, Expr::Ident(n) if n == "BranchPath"));
        assert_eq!(f2, "child");
        match (&inner_child_args[0].node, &inner_child_args[1].node) {
            (Expr::Ident(n), Expr::Literal(Literal::Int(2))) => assert_eq!(n, "path"),
            other => panic!("unexpected inner .child args: {:?}", other),
        }
        // Second arg to outer .child is the inner branch index (0).
        match &outer_child_args[1].node {
            Expr::Literal(Literal::Int(0)) => {}
            other => panic!("expected inner branch idx 0, got {:?}", other),
        }
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
        let [Stmt::Expr(tail)] = lifted.stmts() else {
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

    // ---- lift_fn_def -----------------------------------------------------

    fn parse_fn(src: &str) -> FnDef {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = Parser::new(tokens);
        let items = parser.parse().expect("parse");
        let crate::ast::TopLevel::FnDef(fd) = items.into_iter().next().unwrap() else {
            panic!("expected fn def");
        };
        fd
    }

    #[test]
    fn lift_fn_def_returns_none_for_pure_functions() {
        let fd = parse_fn("fn double(x: Int) -> Int\n    x * 2\n");
        let lifted = lift_fn_def(&fd).unwrap();
        assert!(lifted.is_none(), "pure fn should not be lifted");
    }

    #[test]
    fn lift_fn_def_prepends_path_and_oracle_params() {
        let fd = parse_fn("fn roll() -> Int\n    ! [Random.int]\n    Random.int(1, 6)\n");
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        assert_eq!(lifted.name, "roll");
        assert!(lifted.effects.is_empty(), "lifted fn must have no effects");
        // Params: [path: BranchPath, rnd_Random_int: Fn(...)]
        assert_eq!(lifted.params.len(), 2);
        assert_eq!(lifted.params[0].0, "path");
        assert_eq!(lifted.params[0].1, "BranchPath");
        assert_eq!(lifted.params[1].0, "rnd_Random_int");
        assert_eq!(lifted.params[1].1, "Fn(BranchPath, Int, Int, Int) -> Int");
        // Body: oracle call using the prepended names.
        let [Stmt::Expr(tail)] = lifted.body.stmts() else {
            panic!("expected single expr stmt");
        };
        let args = assert_looks_like_oracle_call(&tail.node, "rnd_Random_int", 4);
        match &args[0] {
            Expr::Ident(n) => assert_eq!(n, "path"),
            other => panic!("expected path ident, got {:?}", other),
        }
        match &args[1] {
            Expr::Literal(Literal::Int(0)) => {}
            other => panic!("expected counter 0, got {:?}", other),
        }
    }

    #[test]
    fn lift_fn_def_skips_path_param_when_only_snapshot_effects() {
        let fd = parse_fn("fn readArgs() -> List<String>\n    ! [Args.get]\n    Args.get()\n");
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        // Snapshot effect → capability reader param only, no BranchPath.
        assert_eq!(lifted.params.len(), 1);
        assert_eq!(lifted.params[0].0, "cap_Args_get");
        assert_eq!(lifted.params[0].1, "Fn() -> List<String>");
    }

    #[test]
    fn lift_fn_def_skips_output_only_effects_in_oracle_params() {
        let fd = parse_fn("fn greet() -> Unit\n    ! [Console.print]\n    Console.print(\"hi\")\n");
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        // Output effect: no oracle param; Console.print stays as-is in body.
        assert!(lifted.params.is_empty(), "no oracle/path needed");
    }

    #[test]
    fn lift_fn_def_on_plan_example_3_pick_three() {
        // Sequential Random.int calls should lift to oracle calls with
        // counter 0, 1, 2.
        let fd = parse_fn(
            "fn pickThree() -> (Int, Int, Int)\n\
             \x20   ! [Random.int]\n\
             \x20   (Random.int(1, 100), Random.int(1, 100), Random.int(1, 100))\n",
        );
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        assert_eq!(lifted.params.len(), 2);
        assert_eq!(lifted.params[0].0, "path");
        assert_eq!(lifted.params[1].0, "rnd_Random_int");
        let [Stmt::Expr(tail)] = lifted.body.stmts() else {
            panic!("expected single expr stmt");
        };
        let Expr::Tuple(elems) = &tail.node else {
            panic!("expected tuple body");
        };
        assert_eq!(elems.len(), 3);
        for (i, elem) in elems.iter().enumerate() {
            let args = assert_looks_like_oracle_call(&elem.node, "rnd_Random_int", 4);
            // path arg is the bare `path` ident (flat body, no branches).
            match &args[0] {
                Expr::Ident(n) => assert_eq!(n, "path"),
                other => panic!("slot {} path ident; got {:?}", i, other),
            }
            match &args[1] {
                Expr::Literal(Literal::Int(n)) => assert_eq!(*n as usize, i),
                other => panic!("slot {} counter; got {:?}", i, other),
            }
        }
    }

    #[test]
    fn lift_fn_def_on_plan_example_7_fetch_both() {
        // Plan example 7: `(Http.get(urlA), Http.get(urlB))?!`. The
        // lifter CPS-desugars `?!` at tail position into a nested
        // match: first-Err-wins, innermost Ok arm wraps the unwrapped
        // tuple in `Result.Ok`. Each branch keeps its own
        // `BranchPath.child(path, i)` + fresh counter.
        let fd = parse_fn(
            "fn fetchBoth(urlA: String, urlB: String) -> Result<(String, String), String>\n\
             \x20   ! [Http.get]\n\
             \x20   (Http.get(urlA), Http.get(urlB))?!\n",
        );
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        assert_eq!(lifted.params.len(), 4);
        assert_eq!(lifted.params[0].0, "path");
        assert_eq!(lifted.params[1].0, "rnd_Http_get");
        let [Stmt::Expr(tail)] = lifted.body.stmts() else {
            panic!("expected single expr stmt");
        };
        // Outer match: on branch 0's lifted oracle call. Err arm
        // propagates err_. Ok arm binds ep_0 and contains the inner
        // match.
        let Expr::Match {
            subject: outer_subj,
            arms: outer_arms,
        } = &tail.node
        else {
            panic!("expected outer Match, got {:?}", tail.node);
        };
        let outer_args = assert_looks_like_oracle_call(&outer_subj.node, "rnd_Http_get", 3);
        assert_path_child_idx(&outer_args[0], 0);
        assert_eq!(outer_arms.len(), 2);
        // Inner match under Ok(ep_0).
        let Expr::Match {
            subject: inner_subj,
            arms: inner_arms,
        } = &outer_arms[1].body.node
        else {
            panic!("expected inner Match under outer Ok arm");
        };
        let inner_args = assert_looks_like_oracle_call(&inner_subj.node, "rnd_Http_get", 3);
        assert_path_child_idx(&inner_args[0], 1);
        assert_eq!(inner_arms.len(), 2);
        // Inner Ok arm should be `Result.Ok((ep_0, ep_1))`.
        let Expr::Constructor(ctor, Some(inner_ok_body)) = &inner_arms[1].body.node else {
            panic!("inner Ok arm should wrap tuple in Result.Ok");
        };
        assert!(ctor == "Result.Ok" || ctor == "Ok");
        let Expr::Tuple(items) = &inner_ok_body.node else {
            panic!("inner Ok arm body should be tuple");
        };
        assert_eq!(items.len(), 2);
    }

    fn assert_path_child_idx(expr: &Expr, expected_idx: i64) {
        let Expr::FnCall(callee, child_args) = expr else {
            panic!("expected BranchPath.child call, got {:?}", expr);
        };
        let Expr::Attr(h, f) = &callee.node else {
            panic!("expected Attr callee");
        };
        assert!(matches!(&h.node, Expr::Ident(n) if n == "BranchPath"));
        assert_eq!(f, "child");
        match &child_args[1].node {
            Expr::Literal(Literal::Int(n)) => assert_eq!(*n, expected_idx),
            other => panic!("idx should be {}, got {:?}", expected_idx, other),
        }
    }

    #[test]
    fn lift_output_call_walks_args_to_bump_counter() {
        // Regression: `Console.print(Random.int(1,6))` is an Output
        // call but its args contain a generative effect that the VM
        // evaluates eagerly, bumping the oracle counter before the
        // output emission. The lifter must walk output-call args so
        // the trailing `Random.int(...)` gets counter 1 (not 0),
        // matching VM / replay semantics.
        let fd = parse_fn(
            "fn twoRolls() -> Int\n\
             \x20   ! [Random.int, Console.print]\n\
             \x20   Console.print(\"rolling {Random.int(1, 6)}\")\n\
             \x20   Random.int(1, 6)\n",
        );
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        let stmts = lifted.body.stmts();
        // Last stmt is the tail Random.int(1, 6) → oracle call with
        // counter 1 (the one inside Console.print bumped it from 0).
        let Stmt::Expr(tail) = stmts.last().expect("non-empty body") else {
            panic!("tail should be expression");
        };
        let args = assert_looks_like_oracle_call(&tail.node, "rnd_Random_int", 4);
        match &args[1] {
            Expr::Literal(Literal::Int(n)) => assert_eq!(
                *n, 1,
                "tail Random.int should see counter 1 after the output-nested call; got {}",
                n
            ),
            other => panic!("expected Int counter, got {:?}", other),
        }
    }

    #[test]
    fn lift_fn_def_mixed_dims_orders_params_predictably() {
        let fd = parse_fn(
            "fn mixed(x: Int) -> Int\n    ! [Args.get, Random.int, Console.print]\n    x\n",
        );
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        // Generative effect present → path param first.
        // Then oracles in declaration order: Args.get, Random.int (Console
        // is output, skipped).
        assert_eq!(lifted.params[0].0, "path");
        assert_eq!(lifted.params[1].0, "cap_Args_get");
        assert_eq!(lifted.params[2].0, "rnd_Random_int");
        assert_eq!(lifted.params[3].0, "x");
    }
}
