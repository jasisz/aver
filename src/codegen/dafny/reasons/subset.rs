//! Conservative admission for the first Dafny guidance pilot. Validate source
//! expressions and every called definition instead of trusting a solver label.

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::ast::{
    BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt, VerifyBlock, VerifyKind, VerifyLaw,
};
use crate::codegen::CodegenContext;
use crate::ir::FnId;

use super::{label, lemma_name};
use crate::codegen::dafny::expr::aver_name_to_dafny;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Scalar {
    Int,
    Bool,
}

type Env = BTreeMap<String, Scalar>;
type Citation<'a> = (&'a VerifyBlock, &'a VerifyLaw);

fn scalar(name: &str) -> Result<Scalar, String> {
    match name {
        "Int" => Ok(Scalar::Int),
        "Bool" => Ok(Scalar::Bool),
        _ => Err(format!(
            "only plain Int/Bool types are supported, found {name}"
        )),
    }
}

fn bind(env: &mut Env, name: &str, ty: Scalar) -> Result<(), String> {
    let emitted = aver_name_to_dafny(name);
    if name.chars().all(|c| c == '_') || env.keys().any(|key| aver_name_to_dafny(key) == emitted) {
        return Err(format!("ambiguous emitted variable name {name}"));
    }
    env.insert(name.to_string(), ty);
    Ok(())
}

struct Checker<'a> {
    ctx: &'a CodegenContext,
    functions: HashSet<FnId>,
    checking_functions: Vec<FnId>,
    laws: BTreeMap<String, Citation<'a>>,
    checking_laws: BTreeSet<String>,
    checked_laws: BTreeSet<String>,
}

impl<'a> Checker<'a> {
    fn expression(&mut self, expr: &Spanned<Expr>, env: &Env) -> Result<Scalar, String> {
        match &expr.node {
            Expr::Literal(Literal::Int(_)) => Ok(Scalar::Int),
            Expr::Literal(Literal::Bool(_)) => Ok(Scalar::Bool),
            Expr::Ident(name) | Expr::Resolved { name, .. } => env
                .get(name)
                .copied()
                .ok_or_else(|| format!("only local scalar values are supported: {name}")),
            Expr::Neg(inner) => {
                self.expect(inner, env, Scalar::Int)?;
                Ok(Scalar::Int)
            }
            Expr::BinOp(op, lhs, rhs) => {
                if *op == BinOp::Div {
                    return Err("division is outside the integer arithmetic pilot".to_string());
                }
                let left = self.expression(lhs, env)?;
                let right = self.expression(rhs, env)?;
                if left != right || (!matches!(op, BinOp::Eq | BinOp::Neq) && left != Scalar::Int) {
                    return Err("only scalar arithmetic and comparisons are supported".to_string());
                }
                Ok(if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul) {
                    Scalar::Int
                } else {
                    Scalar::Bool
                })
            }
            Expr::FnCall(callee, args) => {
                let name = crate::checker::expr_to_str(callee);
                if env.contains_key(&name) {
                    return Err("function-valued calls are outside this pilot".to_string());
                }
                let scope = self.ctx.active_module_scope();
                if let Some(id) = self
                    .ctx
                    .symbol_table
                    .resolve_fn_id_in(&name, scope.as_deref())
                {
                    let key = &self.ctx.symbol_table.fn_entry(id).key;
                    if key.scope_str() != scope.as_deref() {
                        return Err(format!("imported function {name} is outside this pilot"));
                    }
                    let fd = self
                        .ctx
                        .fn_def_by_name(&key.name, key.scope_str())
                        .ok_or_else(|| format!("no source definition for {name}"))?;
                    if !fd.effects.is_empty() || fd.params.len() != args.len() {
                        return Err(format!("only pure first-order calls are supported: {name}"));
                    }
                    for ((_, ty), arg) in fd.params.iter().zip(args) {
                        self.expect(arg, env, scalar(ty)?)?;
                    }
                    let result = scalar(&fd.return_type)?;
                    if self.ctx.recursive_fns.contains(&id)
                        && countdown_parameter(fd, self.ctx).is_none()
                    {
                        return Err(format!(
                            "recursive function {name} is outside the guarded Int countdown pilot"
                        ));
                    }
                    // A self-call revisits the body being checked, whose remaining
                    // expressions must still be validated. Any other back edge is
                    // mutual recursion and must not reach Dafny's fuel/opaque lane.
                    if self.checking_functions.contains(&id)
                        && self.checking_functions.last() != Some(&id)
                    {
                        return Err(format!(
                            "mutual recursion through {name} is outside this pilot"
                        ));
                    }
                    if self.functions.insert(id) {
                        self.checking_functions.push(id);
                        let mut locals = Env::new();
                        for (param, ty) in &fd.params {
                            bind(&mut locals, param, scalar(ty)?)?;
                        }
                        let mut body_type = None;
                        for stmt in fd.body.stmts() {
                            match stmt {
                                Stmt::Binding(local, annotation, value) => {
                                    let ty = self.expression(value, &locals)?;
                                    if annotation
                                        .as_deref()
                                        .map(scalar)
                                        .transpose()?
                                        .is_some_and(|ann| ann != ty)
                                    {
                                        return Err("unsupported local binding type".to_string());
                                    }
                                    bind(&mut locals, local, ty)?;
                                    body_type = None;
                                }
                                Stmt::Expr(value) => {
                                    body_type = Some(self.expression(value, &locals)?)
                                }
                            }
                        }
                        self.checking_functions.pop();
                        if body_type != Some(result) {
                            return Err("unsupported function result".to_string());
                        }
                    }
                    return Ok(result);
                }
                let (arity, input, output) = match name.as_str() {
                    "Bool.and" | "Bool.or" => (2, Scalar::Bool, Scalar::Bool),
                    "Bool.not" => (1, Scalar::Bool, Scalar::Bool),
                    "Int.abs" => (1, Scalar::Int, Scalar::Int),
                    "Int.min" | "Int.max" => (2, Scalar::Int, Scalar::Int),
                    _ => return Err(format!("unsupported call {name}")),
                };
                if args.len() != arity {
                    return Err(format!("unsupported arity for {name}"));
                }
                for arg in args {
                    self.expect(arg, env, input)?;
                }
                Ok(output)
            }
            Expr::TailCall(call) => {
                // TCO preserves the exact source call; validate it through the
                // same path, including every argument and the recursive body.
                let callee = Spanned::new(Expr::Ident(call.target.clone()), expr.line);
                let call =
                    Spanned::new(Expr::FnCall(Box::new(callee), call.args.clone()), expr.line);
                self.expression(&call, env)
            }
            Expr::Match { subject, arms } => {
                self.expect(subject, env, Scalar::Bool)?;
                let mut covered = [false; 2];
                let mut result = None;
                for arm in arms {
                    match arm.pattern {
                        Pattern::Literal(Literal::Bool(value)) => {
                            covered[usize::from(value)] = true
                        }
                        Pattern::Wildcard => covered = [true; 2],
                        _ => return Err("only literal Bool matches are supported".to_string()),
                    }
                    let ty = self.expression(&arm.body, env)?;
                    if result.is_some_and(|old| old != ty) {
                        return Err("unsupported match result".to_string());
                    }
                    result = Some(ty);
                }
                if covered != [true; 2] {
                    return Err("Bool match must cover both values".to_string());
                }
                result.ok_or_else(|| "empty match is outside this pilot".to_string())
            }
            _ => Err("expression is outside the Int/Bool guidance pilot".to_string()),
        }
    }

    fn expect(&mut self, expr: &Spanned<Expr>, env: &Env, expected: Scalar) -> Result<(), String> {
        if self.expression(expr, env)? == expected {
            Ok(())
        } else {
            Err("unsupported expression type".to_string())
        }
    }

    fn law(&mut self, id: &str, law: &'a VerifyLaw) -> Result<(), String> {
        if self.checked_laws.contains(id) {
            return Ok(());
        }
        if !self.checking_laws.insert(id.to_string()) {
            return Err(format!("cyclic citation {id}"));
        }
        let selected = law.using.as_ref().ok_or_else(|| {
            "an explicit using list is required (using [] selects no laws)".to_string()
        })?;
        if law.givens.is_empty() {
            return Err("at least one plain Int/Bool given is required".to_string());
        }
        let mut env = Env::new();
        for given in &law.givens {
            bind(&mut env, &given.name, scalar(&given.type_name)?)?;
        }
        if let Some(guard) = &law.when {
            self.expect(guard, &env, Scalar::Bool)?;
        }
        for reason in &law.because {
            self.expect(reason, &env, Scalar::Bool)?;
        }
        let left = self.expression(&law.lhs, &env)?;
        self.expect(&law.rhs, &env, left)?;
        for selected in selected {
            let (_, dependency) = self.laws.get(selected).copied().ok_or_else(|| {
                format!("citation {selected} is not an available same-module law")
            })?;
            self.law(selected, dependency)
                .map_err(|reason| format!("citation {selected}: {reason}"))?;
        }
        self.checking_laws.remove(id);
        self.checked_laws.insert(id.to_string());
        Ok(())
    }
}

/// The same checked subtractive descent used by the proof lowerer. This lane
/// excludes ascent, unguarded recursion, and opaque termination fallbacks.
pub(super) fn countdown_parameter(fd: &FnDef, ctx: &CodegenContext) -> Option<usize> {
    crate::codegen::recursion::detect::single_int_countdown_param_index(fd).filter(|index| {
        crate::codegen::recursion::detect::has_guarded_subtractive_descent(fd, *index)
            && !crate::codegen::dafny::toplevel::termination_guess_unjustified(fd, ctx)
    })
}

pub(super) fn validate<'a>(
    vb: &VerifyBlock,
    law: &'a VerifyLaw,
    ctx: &'a CodegenContext,
    blocks: &[&'a VerifyBlock],
) -> Result<Vec<Citation<'a>>, String> {
    let mut laws = BTreeMap::new();
    for block in blocks {
        if let VerifyKind::Law(law) = &block.kind {
            let id = label(block, law);
            if laws.insert(id.clone(), (*block, law.as_ref())).is_some() {
                return Err(format!("ambiguous source law identity {id}"));
            }
        }
    }
    let mut checker = Checker {
        ctx,
        functions: HashSet::new(),
        checking_functions: Vec::new(),
        laws,
        checking_laws: BTreeSet::new(),
        checked_laws: BTreeSet::new(),
    };
    checker.law(&label(vb, law), law)?;
    // Reserve every generated parent/step name. A source declaration or a
    // quantified citation argument must not capture one of the lemma calls.
    let scope = ctx.active_module_scope();
    let functions = match scope.as_deref() {
        Some(scope) => ctx
            .modules
            .iter()
            .find(|module| module.prefix == scope)
            .map(|module| module.fn_defs.as_slice())
            .unwrap_or_default(),
        None => ctx.fn_defs.as_slice(),
    };
    for id in &checker.checked_laws {
        let name = lemma_name(id);
        let (_, checked) = checker.laws[id];
        let generated = std::iter::once(name.clone())
            .chain(std::iter::once(format!("{name}_implication")))
            .chain((1..=checked.because.len()).map(|index| format!("{name}_because{index}")));
        for generated in generated {
            if functions
                .iter()
                .any(|fd| aver_name_to_dafny(&fd.name) == generated)
                || checker.checked_laws.iter().any(|id| {
                    checker.laws[id]
                        .1
                        .givens
                        .iter()
                        .any(|given| aver_name_to_dafny(&given.name) == generated)
                })
            {
                return Err(format!(
                    "generated lemma name collides with source name for {id}"
                ));
            }
        }
    }
    let mut selected = law
        .using
        .as_ref()
        .expect("validated explicit citations")
        .clone();
    selected.sort();
    selected
        .into_iter()
        .map(|id| {
            checker
                .laws
                .get(&id)
                .copied()
                .ok_or_else(|| format!("unavailable citation {id}"))
        })
        .collect()
}
