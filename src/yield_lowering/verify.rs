//! Cases-form verification of a process through its generated protocol.
//!
//! The driver performs each request as an ordinary capability call. The
//! existing verify runner supplies its exact `given` stub, including the
//! Oracle counter, and refuses unstubbed effects before host dispatch.
//! Self yields only advance the protocol and consume no oracle answer.

use std::collections::HashSet;

use super::{FnSigs, ProcessProtocol, build, error_at};
use crate::ast::*;
use crate::types::checker::TypeError;

pub(super) fn supports(block: &VerifyBlock, process: &str) -> bool {
    block.fn_name == process && matches!(block.kind, VerifyKind::Cases) && !block.trace
}

pub(super) fn generate(
    items: &mut [TopLevel],
    protocols: &[ProcessProtocol],
    fn_sigs: &FnSigs,
) -> Result<Vec<TopLevel>, Vec<TypeError>> {
    let mut generated = Vec::new();
    let mut emitted = HashSet::new();
    let mut errors = Vec::new();
    for item in items.iter_mut() {
        let TopLevel::Verify(block) = item else {
            continue;
        };
        let Some(protocol) = protocols.iter().find(|p| supports(block, &p.fn_name)) else {
            continue;
        };
        let missing: Vec<_> = protocol
            .kinds
            .iter()
            .filter_map(|kind| {
                let operation = kind.operation.as_ref()?;
                (!block
                    .cases_givens
                    .iter()
                    .any(|given| &given.type_name == operation))
                .then_some(operation.as_str())
            })
            .collect();
        if !missing.is_empty() {
            errors.push(error_at(block.line, format!(
                "verify '{}' must supply an exact given stub for every request: {}; add `given answer: {} = [stub]`. The test supplies operation results, not Now/Later replies from the live answer module",
                block.fn_name, missing.join(", "), missing[0]
            )));
            continue;
        }
        let entry = format!("__verifyProcess_{}", protocol.fn_name);
        if emitted.insert(protocol.fn_name.clone()) {
            let effects = fn_sigs
                .get(&protocol.fn_name)
                .map(|sig| {
                    sig.2
                        .iter()
                        .filter(|effect| effect.as_str() != super::YIELD_EFFECT)
                        .cloned()
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            generated.extend(driver(protocol, &entry, &effects, block.line));
        }
        for (lhs, rhs) in &mut block.cases {
            for expression in [lhs, rhs] {
                *expression = crate::ast_rewrite::rewrite_idents_scoped(expression, |name| {
                    (name == protocol.fn_name).then(|| build::ident(&entry, expression.line))
                });
            }
        }
        block.process_verification = Some(ProcessVerification {
            source_fn_name: block.fn_name.clone(),
            driver: format!("__verifyDrive_{}", protocol.fn_name),
        });
        block.fn_name = entry;
    }
    // These entry points exist only to execute cases. Unlike the public
    // process protocol, their request calls have no live provider binding.
    let helpers: HashSet<_> = generated
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(function) => Some(function.name.as_str()),
            _ => None,
        })
        .collect();
    for item in items.iter() {
        let expressions: Vec<_> = match item {
            TopLevel::FnDef(function) => function
                .body
                .stmts()
                .iter()
                .map(|stmt| match stmt {
                    Stmt::Binding(_, _, expression) | Stmt::Expr(expression) => expression,
                })
                .collect(),
            TopLevel::Stmt(Stmt::Binding(_, _, expression) | Stmt::Expr(expression)) => {
                vec![expression]
            }
            _ => Vec::new(),
        };
        for expression in expressions {
            crate::codegen::expr_walk::walk(expression, &mut |node| {
                if let Expr::Ident(name) = &node.node
                    && helpers.contains(name.as_str())
                {
                    errors.push(error_at(node.line, format!("'{name}' is a verify-only process driver; use the process protocol in program code")));
                }
            });
        }
    }
    if errors.is_empty() {
        Ok(generated)
    } else {
        Err(errors)
    }
}

fn driver(
    protocol: &ProcessProtocol,
    entry: &str,
    effects: &[String],
    line: usize,
) -> Vec<TopLevel> {
    let drive = format!("__verifyDrive_{}", protocol.fn_name);
    let mut arms = Vec::new();
    for kind in &protocol.kinds {
        let mut names: Vec<_> = kind
            .arg_types
            .iter()
            .enumerate()
            .map(|(index, _)| format!("__arg{index}"))
            .collect();
        let mut answers = vec![build::ident("__state", line)];
        let mut unit_request = None;
        if let Some(operation) = &kind.operation {
            let mut segments = operation.split('.');
            let base = build::ident(segments.next().expect("operation namespace"), line);
            let callee = segments.fold(base, |base, field| {
                Spanned::new(Expr::Attr(Box::new(base), field.to_string()), line)
            });
            let request = Spanned::new(
                Expr::FnCall(
                    Box::new(callee),
                    names.iter().map(|name| build::ident(name, line)).collect(),
                ),
                line,
            );
            if kind.answer_type.as_deref() == Some("Unit") {
                unit_request = Some(request);
            } else {
                answers.push(request);
            }
        }
        names.push("__state".to_string());
        let next = build::call(
            &drive,
            vec![build::call(&kind.answer_fn, answers, line)],
            line,
        );
        let next = match unit_request {
            Some(request) => {
                build::match_expr(request, vec![MatchArm::new(Pattern::Wildcard, next)], line)
            }
            None => next,
        };
        arms.push(MatchArm::new(
            Pattern::Constructor(format!("{}.{}", protocol.request, kind.name), names),
            next,
        ));
    }
    let step = build::match_expr(
        build::ident("__outcome", line),
        vec![
            MatchArm::new(
                Pattern::Constructor(
                    format!("{}.Done", protocol.outcome),
                    vec!["__value".to_string()],
                ),
                build::ident("__value", line),
            ),
            MatchArm::new(
                Pattern::Constructor(
                    format!("{}.Waiting", protocol.outcome),
                    vec!["__request".to_string()],
                ),
                build::match_expr(build::ident("__request", line), arms, line),
            ),
        ],
        line,
    );
    let start = build::call(
        &drive,
        vec![build::call(
            &protocol.start,
            protocol
                .params
                .iter()
                .map(|(name, _)| build::ident(name, line))
                .collect(),
            line,
        )],
        line,
    );
    [
        build::fn_def(
            entry.to_string(),
            protocol.params.clone(),
            protocol.return_type.clone(),
            Some("Run the generated process with this verify case's request stubs.".to_string()),
            vec![],
            start,
            line,
        ),
        build::fn_def(
            drive,
            vec![("__outcome".to_string(), protocol.outcome.clone())],
            protocol.return_type.clone(),
            Some(
                "Answer a request through the verify oracle, then advance the same protocol."
                    .to_string(),
            ),
            vec![],
            step,
            line,
        ),
    ]
    .into_iter()
    .map(|mut function| {
        function.effects = effects
            .iter()
            .map(|effect| Spanned::new(effect.clone(), line))
            .collect();
        TopLevel::FnDef(function)
    })
    .collect()
}
