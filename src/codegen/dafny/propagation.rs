//! Lower pure `?` and `?!` into explicit, function-result-valued matches.
//!
//! An error leaves the enclosing function, not merely the expression that
//! contains it. Continuation passing keeps subsequent evaluation inside Ok
//! arms and keeps a match arm's work inside that arm. Ordinary operands are
//! evaluated once, from left to right; independent products evaluate their
//! branches before choosing the first error in source order.

use std::collections::HashSet;
use std::sync::Arc;

use crate::ast::{Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart};
use crate::codegen::expr_walk;
use crate::types::Type;

type Expression = Spanned<Expr>;
type Continuation<'a> = dyn Fn(&mut Lowerer, Expression) -> Expression + 'a;
type ValuesContinuation<'a> = dyn Fn(&mut Lowerer, Vec<Expression>) -> Expression + 'a;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct PropagationError {
    pub message: String,
}

/// The caller resolves the returned AST in the original function's module
/// scope. Original expressions retain their type stamps and constructor
/// spellings; new return-position matches and Err constructors carry the
/// declared result type. Effectful functions belong to the oracle lifter.
pub(super) fn lower_pure_fn(fd: &FnDef) -> Result<Option<FnDef>, PropagationError> {
    if !fd.effects.is_empty()
        || !fd
            .body
            .stmts()
            .iter()
            .any(|stmt| contains(statement_expr(stmt)))
    {
        return Ok(None);
    }
    let result_type = crate::codegen::common::parse_type_annotation(&fd.return_type);
    if !matches!(result_type, Type::Result(..)) {
        return Err(PropagationError {
            message: format!(
                "function `{}` uses propagation but does not return Result",
                fd.name
            ),
        });
    }
    let mut lowerer = Lowerer {
        used: HashSet::new(),
        next: 0,
        result_type,
    };
    lowerer.reserve(&fd.name);
    for (name, _) in &fd.params {
        lowerer.reserve(name);
    }
    for stmt in fd.body.stmts() {
        if let Stmt::Binding(name, _, _) = stmt {
            lowerer.reserve(name);
        }
        expr_walk::walk(statement_expr(stmt), &mut |expr| match &expr.node {
            Expr::Ident(name) | Expr::Resolved { name, .. } => lowerer.reserve(name),
            Expr::TailCall(call) => lowerer.reserve(&call.target),
            Expr::Match { arms, .. } => {
                for arm in arms {
                    lowerer.reserve_pattern(&arm.pattern);
                }
            }
            _ => {}
        });
    }
    let body = lowerer.statements(fd.body.stmts());
    let mut lowered = fd.clone();
    lowered.body = Arc::new(FnBody::from_expr(body));
    lowered.resolution = None;
    Ok(Some(lowered))
}

fn statement_expr(stmt: &Stmt) -> &Expression {
    match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr,
    }
}

fn contains(expr: &Expression) -> bool {
    expr_walk::any(expr, &mut |expr| {
        matches!(
            expr.node,
            Expr::ErrorProp(_) | Expr::IndependentProduct(_, true)
        )
    })
}

struct Lowerer {
    used: HashSet<String>,
    next: usize,
    result_type: Type,
}

impl Lowerer {
    fn reserve(&mut self, name: &str) {
        self.used.insert(name.to_string());
        self.used.insert(super::expr::aver_name_to_dafny(name));
    }

    fn reserve_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Ident(name) => self.reserve(name),
            Pattern::Cons(head, tail) => {
                self.reserve(head);
                self.reserve(tail);
            }
            Pattern::Constructor(_, names) => {
                for name in names {
                    self.reserve(name);
                }
            }
            Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.reserve_pattern(pattern);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
        }
    }

    fn fresh(&mut self) -> String {
        loop {
            let name = format!("__aver_prop_{}", self.next);
            self.next += 1;
            let emitted = super::expr::aver_name_to_dafny(&name);
            if !self.used.contains(&name) && !self.used.contains(&emitted) {
                self.used.insert(name.clone());
                self.used.insert(emitted);
                return name;
            }
        }
    }

    fn result_match(&self, subject: Expression, arms: Vec<MatchArm>) -> Expression {
        let expr = Spanned::new(
            Expr::Match {
                subject: Box::new(subject.clone()),
                arms,
            },
            subject.line,
        );
        expr.set_ty(self.result_type.clone());
        expr
    }

    fn bind(&mut self, value: Expression, name: String, then: &Continuation<'_>) -> Expression {
        let reference = Spanned::new(Expr::Ident(name.clone()), value.line);
        if let Some(ty) = value.ty() {
            reference.set_ty(ty.clone());
        }
        let body = then(self, reference);
        let pattern = if name == "_" {
            Pattern::Wildcard
        } else {
            Pattern::Ident(name)
        };
        self.result_match(value, vec![MatchArm::new(pattern, body)])
    }

    fn statements(&mut self, stmts: &[Stmt]) -> Expression {
        let Some((first, rest)) = stmts.split_first() else {
            return Spanned::bare(Expr::Literal(Literal::Unit));
        };
        match first {
            Stmt::Binding(name, annotation, expr) => self.expression(expr, &|this, value| {
                if value.ty().is_none()
                    && let Some(annotation) = annotation
                {
                    value.set_ty(crate::codegen::common::parse_type_annotation(annotation));
                }
                this.bind(value, name.clone(), &|this, _| this.statements(rest))
            }),
            Stmt::Expr(expr) if rest.is_empty() => self.expression(expr, &|_, value| value),
            Stmt::Expr(expr) => self.expression(expr, &|this, value| {
                let name = this.fresh();
                this.bind(value, name, &|this, _| this.statements(rest))
            }),
        }
    }

    fn propagate(
        &mut self,
        value: Expression,
        ok_type: Option<&Type>,
        then: &Continuation<'_>,
    ) -> Expression {
        let ok = self.fresh();
        let err = self.fresh();
        let reference = Spanned::new(Expr::Ident(ok.clone()), value.line);
        if let Some(ty) = ok_type {
            reference.set_ty(ty.clone());
        }
        let error_value = Spanned::new(Expr::Ident(err.clone()), value.line);
        if let Type::Result(_, error_type) = &self.result_type {
            error_value.set_ty(*error_type.clone());
        }
        let error = Spanned::new(
            Expr::Constructor("Result.Err".to_string(), Some(Box::new(error_value))),
            value.line,
        );
        error.set_ty(self.result_type.clone());
        let success = then(self, reference);
        self.result_match(
            value,
            vec![
                MatchArm::new(
                    Pattern::Constructor("Result.Ok".to_string(), vec![ok]),
                    success,
                ),
                MatchArm::new(
                    Pattern::Constructor("Result.Err".to_string(), vec![err]),
                    error,
                ),
            ],
        )
    }

    fn expression(&mut self, expr: &Expression, then: &Continuation<'_>) -> Expression {
        if !contains(expr) {
            return then(self, expr.clone());
        }
        match &expr.node {
            Expr::ErrorProp(inner) => {
                self.expression(inner, &|this, value| this.propagate(value, expr.ty(), then))
            }
            Expr::Match { subject, arms } => self.expression(subject, &|this, value| {
                let arms = arms
                    .iter()
                    .map(|arm| MatchArm::new(arm.pattern.clone(), this.expression(&arm.body, then)))
                    .collect();
                this.result_match(value, arms)
            }),
            Expr::IndependentProduct(items, true) => {
                let refs: Vec<_> = items.iter().collect();
                self.values(&refs, Vec::new(), &|this, values| {
                    this.unwrap_product(&values, Vec::new(), expr, then)
                })
            }
            _ => {
                let children = expr_walk::child_exprs(expr);
                self.values(&children, Vec::new(), &|this, values| {
                    then(this, rebuild(expr, values))
                })
            }
        }
    }

    fn values(
        &mut self,
        pending: &[&Expression],
        done: Vec<Expression>,
        then: &ValuesContinuation<'_>,
    ) -> Expression {
        let Some((first, rest)) = pending.split_first() else {
            return then(self, done);
        };
        self.expression(first, &|this, value| {
            // Names and literals are already values. Keeping static callees
            // in this form also preserves builtin/module call resolution.
            if is_value(&value) {
                let mut done = done.clone();
                done.push(value);
                this.values(rest, done, then)
            } else {
                let name = this.fresh();
                this.bind(value, name, &|this, reference| {
                    let mut done = done.clone();
                    done.push(reference);
                    this.values(rest, done, then)
                })
            }
        })
    }

    fn unwrap_product(
        &mut self,
        pending: &[Expression],
        done: Vec<Expression>,
        original: &Expression,
        then: &Continuation<'_>,
    ) -> Expression {
        let Some((first, rest)) = pending.split_first() else {
            let mut tuple = original.clone();
            tuple.node = Expr::Tuple(done);
            return then(self, tuple);
        };
        let ok_type = match first.ty() {
            Some(Type::Result(ok, _)) => Some(ok.as_ref()),
            _ => None,
        };
        self.propagate(first.clone(), ok_type, &|this, value| {
            let mut done = done.clone();
            done.push(value);
            this.unwrap_product(rest, done, original, then)
        })
    }
}

fn is_value(expr: &Expression) -> bool {
    match &expr.node {
        Expr::Ident(_) | Expr::Resolved { .. } | Expr::Literal(_) | Expr::Constructor(_, None) => {
            true
        }
        Expr::Attr(base, _) => is_value(base),
        _ => false,
    }
}

/// Rebuild eager expression forms in the same child order as expr_walk.
/// Match, propagation and independent unwrapping have dedicated CPS rules.
fn rebuild(original: &Expression, values: Vec<Expression>) -> Expression {
    let mut children = values.into_iter();
    let mut next = || {
        children
            .next()
            .expect("one replacement per expression child")
    };
    let mut result = original.clone();
    result.node = match &original.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            original.node.clone()
        }
        Expr::Attr(_, field) => Expr::Attr(Box::new(next()), field.clone()),
        Expr::Neg(_) => Expr::Neg(Box::new(next())),
        Expr::Constructor(name, Some(_)) => Expr::Constructor(name.clone(), Some(Box::new(next()))),
        Expr::FnCall(_, args) => {
            Expr::FnCall(Box::new(next()), args.iter().map(|_| next()).collect())
        }
        Expr::TailCall(call) => {
            let mut call = call.clone();
            call.args = call.args.iter().map(|_| next()).collect();
            Expr::TailCall(call)
        }
        Expr::BinOp(op, _, _) => Expr::BinOp(*op, Box::new(next()), Box::new(next())),
        Expr::List(items) => Expr::List(items.iter().map(|_| next()).collect()),
        Expr::Tuple(items) => Expr::Tuple(items.iter().map(|_| next()).collect()),
        Expr::IndependentProduct(items, false) => {
            Expr::Tuple(items.iter().map(|_| next()).collect())
        }
        Expr::MapLiteral(entries) => {
            Expr::MapLiteral(entries.iter().map(|_| (next(), next())).collect())
        }
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(name, _)| (name.clone(), next()))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name, updates, ..
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(next()),
            updates: updates
                .iter()
                .map(|(name, _)| (name.clone(), next()))
                .collect(),
        },
        Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
            parts
                .iter()
                .map(|part| match part {
                    StrPart::Literal(text) => StrPart::Literal(text.clone()),
                    StrPart::Parsed(_) => StrPart::Parsed(Box::new(next())),
                })
                .collect(),
        ),
        Expr::Match { .. } | Expr::ErrorProp(_) | Expr::IndependentProduct(_, true) => {
            unreachable!("CPS expression handled before rebuilding")
        }
    };
    assert!(children.next().is_none());
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::TopLevel;
    use crate::nan_value::{Arena, NanValueConvert};
    use crate::value::Value;

    fn evaluate(source: &str, lower: bool) -> Value {
        evaluate_items(crate::source::parse_source(source).unwrap(), lower)
    }

    fn evaluate_items(mut items: Vec<TopLevel>, lower: bool) -> Value {
        if lower {
            for item in &mut items {
                if let TopLevel::FnDef(fd) = item
                    && let Some(lowered) = lower_pure_fn(fd).unwrap()
                {
                    assert!(
                        !lowered
                            .body
                            .stmts()
                            .iter()
                            .any(|stmt| contains(statement_expr(stmt)))
                    );
                    *fd = lowered;
                }
            }
        }
        crate::ir::pipeline::tco(&mut items);
        crate::ir::pipeline::resolve(&mut items);
        let symbols = crate::ir::SymbolTable::build(&items, &[]);
        let resolved = crate::ir::hir::resolve_program(&symbols, &items);
        let mut arena = Arena::new();
        let (code, globals) =
            crate::vm::compile_program(&resolved, &symbols, &mut arena, None).unwrap();
        let mut machine = crate::vm::VM::new(code, globals, arena);
        machine
            .run_named_function("entry", &[])
            .unwrap()
            .to_value(&machine.arena)
    }

    fn differential(source: &str) {
        assert_eq!(evaluate(source, true), evaluate(source, false));
    }

    #[test]
    fn nested_arguments_keep_the_first_error_and_successful_values() {
        for first in ["Result.Ok(3)", "Result.Err(\"first\")"] {
            for second in ["Result.Ok(5)", "Result.Err(\"second\")"] {
                differential(&format!(
                    "fn first() -> Result<Int, String>\n    {first}\nfn second() -> Result<Int, String>\n    {second}\nfn add(a: Int, b: Int) -> Int\n    a + b\nfn entry() -> Result<Int, String>\n    Result.Ok(add(first()?, second()?))\n"
                ));
            }
        }
    }

    #[test]
    fn propagation_stays_in_the_selected_match_arm_and_skips_later_statements() {
        for branch in ["true", "false"] {
            differential(&format!(
                "fn failure() -> Result<Int, String>\n    Result.Err(\"first\")\nfn entry() -> Result<Int, String>\n    value = match {branch}\n        true -> 7\n        false -> failure()?\n    later = Result.Err(\"later\")?\n    Result.Ok(value + later)\n"
            ));
            differential(&format!(
                "fn failure() -> Result<Int, String>\n    Result.Err(\"unused\")\nfn entry() -> Result<Int, String>\n    value = match {branch}\n        true -> 7\n        false -> failure()?\n    Result.Ok(value)\n"
            ));
        }
    }

    #[test]
    fn products_records_updates_lists_and_tuples_propagate_at_any_depth() {
        for answer in ["Result.Ok(4)", "Result.Err(\"nested\")"] {
            differential(&format!(
                "record Box\n    value: Int\nfn read() -> Result<Int, String>\n    {answer}\nfn entry() -> Result<Tuple<List<Int>, Box>, String>\n    base = Box(value = 1)\n    Result.Ok(([2, read()?], Box.update(base, value = read()?)))\n"
            ));
            differential(&format!(
                "fn read() -> Result<Int, String>\n    {answer}\nfn entry() -> Result<Tuple<Int, Int>, String>\n    Result.Ok((read(), read())?!)\n"
            ));
        }
    }

    #[test]
    fn generated_binders_do_not_capture_parameters_locals_or_pattern_names() {
        // __ names are reserved in source; an earlier compiler pass can
        // nevertheless introduce them into the AST before this lowering.
        let mut items = crate::source::parse_source(
            "fn read() -> Result<Int, String>\n    Result.Ok(2)\nfn helper(parameter: Int) -> Result<Int, String>\n    local = 5\n    match Result.Ok(7)\n        Result.Ok(bound) -> Result.Ok(read()? + parameter + local + bound)\n        Result.Err(err) -> Result.Err(err)\nfn entry() -> Result<Int, String>\n    helper(11)\n"
        ).unwrap();
        let TopLevel::FnDef(fd) = &mut items[1] else {
            panic!("helper expected")
        };
        fd.params[0].0 = "__aver_prop_0".to_string();
        for stmt in Arc::make_mut(&mut fd.body).stmts_mut() {
            let expr = match stmt {
                Stmt::Binding(name, _, expr) => {
                    *name = "__aver_prop_1".to_string();
                    expr
                }
                Stmt::Expr(expr) => expr,
            };
            if let Expr::Match { arms, .. } = &mut expr.node
                && let Pattern::Constructor(_, names) = &mut arms[0].pattern
            {
                names[0] = "__aver_prop_2".to_string();
            }
            for (from, to) in [
                ("parameter", "__aver_prop_0"),
                ("local", "__aver_prop_1"),
                ("bound", "__aver_prop_2"),
            ] {
                *expr = crate::codegen::common::substitute_ident_in_expr(expr, from, to);
            }
        }
        assert_eq!(
            evaluate_items(items.clone(), true),
            evaluate_items(items, false)
        );
    }

    #[test]
    fn generated_binders_avoid_legal_names_in_the_emitted_dafny_namespace() {
        let items = crate::source::parse_source(
            "fn read() -> Result<Int, String>\n    Result.Ok(2)\nfn helper(aver_aver_prop_0: Int) -> Result<Int, String>\n    aver_aver_prop_1 = 5\n    match Result.Ok(7)\n        Result.Ok(aver_aver_prop_2) -> Result.Ok(read()? + aver_aver_prop_0 + aver_aver_prop_1 + aver_aver_prop_2)\n        Result.Err(err) -> Result.Err(err)\n",
        ).unwrap();
        let TopLevel::FnDef(fd) = &items[1] else {
            panic!("helper expected");
        };
        let lowered = lower_pure_fn(fd).unwrap().unwrap();
        let mut generated = 0;
        expr_walk::walk(lowered.body.tail_expr().unwrap(), &mut |expr| {
            let Expr::Match { arms, .. } = &expr.node else {
                return;
            };
            for arm in arms {
                let names = match &arm.pattern {
                    Pattern::Ident(name) => std::slice::from_ref(name),
                    Pattern::Constructor(_, names) => names.as_slice(),
                    _ => continue,
                };
                for name in names.iter().filter(|name| name.starts_with("__aver_prop_")) {
                    generated += 1;
                    let emitted = super::super::expr::aver_name_to_dafny(name);
                    assert!(
                        !["aver_aver_prop_0", "aver_aver_prop_1", "aver_aver_prop_2"]
                            .contains(&emitted.as_str()),
                        "generated binder captures a legal source name: {name} -> {emitted}"
                    );
                }
            }
        });
        assert!(generated > 0);
    }

    #[test]
    fn discarded_propagated_values_use_wildcards_and_keep_early_errors() {
        for answer in ["Result.Ok(3)", "Result.Err(\"discarded error\")"] {
            let source = format!(
                "fn read() -> Result<Int, String>\n    {answer}\nfn entry() -> Result<Int, String>\n    _ = read()?\n    Result.Ok(7)\n"
            );
            differential(&source);
            let items = crate::source::parse_source(&source).unwrap();
            let TopLevel::FnDef(fd) = &items[1] else {
                panic!("entry expected");
            };
            let lowered = lower_pure_fn(fd).unwrap().unwrap();
            let mut discard = false;
            expr_walk::walk(lowered.body.tail_expr().unwrap(), &mut |expr| {
                if let Expr::Match { arms, .. } = &expr.node {
                    for arm in arms {
                        assert!(!matches!(&arm.pattern, Pattern::Ident(name) if name == "_"));
                        discard |= arm.pattern == Pattern::Wildcard;
                    }
                }
            });
            assert!(discard, "the source discard must remain a wildcard");
        }
    }

    #[test]
    fn early_error_is_stamped_with_the_enclosing_result_not_the_unwrapped_type() {
        let items = crate::source::parse_source(
            "fn entry() -> Result<List<Owner.Op>, Owner.Error>\n    Result.Ok([read()?])\n",
        )
        .unwrap();
        let TopLevel::FnDef(fd) = &items[0] else {
            panic!("function expected")
        };
        let lowered = lower_pure_fn(fd).unwrap().unwrap();
        let mut errors = 0;
        expr_walk::walk(lowered.body.tail_expr().unwrap(), &mut |expr| {
            if let Expr::Constructor(name, _) = &expr.node
                && name == "Result.Err"
            {
                errors += 1;
                assert_eq!(
                    expr.ty(),
                    Some(&crate::codegen::common::parse_type_annotation(
                        &fd.return_type
                    ))
                );
            }
        });
        assert_eq!(errors, 1);
    }
}
