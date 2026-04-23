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
    let mut new_stmts = Vec::with_capacity(body.stmts().len());
    for stmt in body.stmts() {
        new_stmts.push(lift_stmt(stmt, cfg, &root_path, &mut counter)?);
    }
    Ok(FnBody::Block(new_stmts))
}

fn spanned(node: Expr, line: SourceLine) -> Spanned<Expr> {
    Spanned { node, line }
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
    Ok(Spanned {
        node: new_node,
        line: expr.line,
    })
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
            // The runtime side of trace assertions is handled by the
            // verify-trace collector, not by lifted proofs.
            let _ = (args, cfg, path_expr, counter);
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
            let path_arg = path_expr.clone();
            let counter_arg = Spanned {
                node: Expr::Literal(Literal::Int(current_counter as i64)),
                line: original.line,
            };
            let mut new_args = vec![path_arg, counter_arg];
            new_args.extend(lift_args(args, cfg, path_expr, counter)?);
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

    // Build an oracles map keyed by effect method name → param binding.
    let mut oracles_map: HashMap<String, String> = HashMap::new();
    for (idx, e) in fd
        .effects
        .iter()
        .filter(|e| {
            classify(&e.node)
                .map(|c| !matches!(c.dimension, EffectDimension::Output))
                .unwrap_or(false)
        })
        .enumerate()
    {
        if let Some((name, _)) = oracle_params.get(idx) {
            oracles_map.insert(e.node.clone(), name.clone());
        }
    }
    let cfg = LiftConfig {
        path_name,
        oracles: oracles_map,
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
    fn lift_output_effect_is_replaced_with_unit() {
        let body = parse_body("    Console.print(\"hi\")");
        let cfg = simple_cfg_with(&[]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
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
    fn lift_independent_product_threads_child_paths_and_resets_counter() {
        let body = parse_body("    (Random.int(1, 6), Random.int(1, 6))!");
        let cfg = simple_cfg_with(&[("Random.int", "rnd")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
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
    fn lift_question_bang_product_preserves_error_prop_flag() {
        let body = parse_body("    (Http.get(\"a\"), Http.get(\"b\"))?!");
        let cfg = simple_cfg_with(&[("Http.get", "http")]);
        let lifted = lift_body(&body, &cfg).unwrap();
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
            panic!("expected one expr stmt");
        };
        let Expr::IndependentProduct(_, is_error_prop) = &tail.node else {
            panic!("expected IndependentProduct");
        };
        assert!(*is_error_prop, "`?!` should set the error-prop flag");
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
        let [Stmt::Expr(tail)] = &lifted.stmts()[..] else {
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
        let [Stmt::Expr(tail)] = &lifted.body.stmts()[..] else {
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
        // Example 3 from .claude/plans/oracle.md: sequential Random.int
        // calls. Lifting should produce oracle calls with counter 0, 1, 2.
        let fd = parse_fn(
            "fn pickThree() -> (Int, Int, Int)\n\
             \x20   ! [Random.int]\n\
             \x20   (Random.int(1, 100), Random.int(1, 100), Random.int(1, 100))\n",
        );
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        assert_eq!(lifted.params.len(), 2);
        assert_eq!(lifted.params[0].0, "path");
        assert_eq!(lifted.params[1].0, "rnd_Random_int");
        let [Stmt::Expr(tail)] = &lifted.body.stmts()[..] else {
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
        // Example 7: `(Http.get(urlA), Http.get(urlB))?!`. Lifting should
        // preserve IndependentProduct (error-prop flag set) and give each
        // branch its own counter + BranchPath.child(path, i).
        let fd = parse_fn(
            "fn fetchBoth(urlA: String, urlB: String) -> Result<(String, String), String>\n\
             \x20   ! [Http.get]\n\
             \x20   (Http.get(urlA), Http.get(urlB))?!\n",
        );
        let lifted = lift_fn_def(&fd).unwrap().unwrap();
        // params: path, http oracle, urlA, urlB
        assert_eq!(lifted.params.len(), 4);
        assert_eq!(lifted.params[0].0, "path");
        assert_eq!(lifted.params[1].0, "rnd_Http_get");
        assert_eq!(lifted.params[2].0, "urlA");
        assert_eq!(lifted.params[3].0, "urlB");
        let [Stmt::Expr(tail)] = &lifted.body.stmts()[..] else {
            panic!("expected single expr stmt");
        };
        let Expr::IndependentProduct(branches, is_err_prop) = &tail.node else {
            panic!("expected IndependentProduct, got {:?}", tail.node);
        };
        assert!(*is_err_prop, "?! should preserve error-prop flag");
        assert_eq!(branches.len(), 2);
        for i in 0..2 {
            let args = assert_looks_like_oracle_call(&branches[i].node, "rnd_Http_get", 3);
            // Each branch's counter resets to 0 (branch locality).
            match &args[1] {
                Expr::Literal(Literal::Int(0)) => {}
                other => panic!("branch {} counter should be 0, got {:?}", i, other),
            }
            // Path: BranchPath.child(path, i).
            let Expr::FnCall(callee, child_args) = &args[0] else {
                panic!("branch {} path should be BranchPath.child call", i);
            };
            let Expr::Attr(h, f) = &callee.node else {
                panic!("branch {} child callee should be Attr", i);
            };
            assert!(matches!(&h.node, Expr::Ident(n) if n == "BranchPath"));
            assert_eq!(f, "child");
            match &child_args[1].node {
                Expr::Literal(Literal::Int(n)) => assert_eq!(*n as usize, i),
                other => panic!("branch {} idx should be {}, got {:?}", i, i, other),
            }
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
