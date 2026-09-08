//! Conservative admission for the first Dafny guidance pilot. Validate source
//! expressions and every called definition instead of trusting a solver label.

use std::collections::{BTreeMap, BTreeSet, HashSet};

use crate::ast::{
    BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt, Type, TypeDef, VerifyBlock, VerifyKind,
    VerifyLaw,
};
use crate::codegen::CodegenContext;
use crate::ir::FnId;

use super::{label, lemma_name};
use crate::codegen::dafny::expr::aver_name_to_dafny;

#[path = "types.rs"]
mod types;
use types::{hole, merge};

type Env = BTreeMap<String, Type>;
type Citation<'a> = (&'a VerifyBlock, &'a VerifyLaw);

fn bind(env: &mut Env, name: &str, ty: Type) -> Result<(), String> {
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
    checked_types: HashSet<crate::ir::TypeId>,
}

impl<'a> Checker<'a> {
    fn expression(&mut self, expr: &Spanned<Expr>, env: &Env) -> Result<Type, String> {
        match &expr.node {
            Expr::Literal(Literal::Int(_) | Literal::BigInt(_)) => Ok(Type::Int),
            Expr::Literal(Literal::Bool(_)) => Ok(Type::Bool),
            Expr::Literal(Literal::Str(_)) => Ok(Type::Str),
            Expr::Ident(name) | Expr::Resolved { name, .. } => env
                .get(name)
                .cloned()
                .ok_or_else(|| format!("only local first-order values are supported: {name}")),
            Expr::Neg(inner) => {
                self.expect(inner, env, Type::Int)?;
                Ok(Type::Int)
            }
            Expr::BinOp(op, lhs, rhs) => {
                if *op == BinOp::Div {
                    return Err("division is outside the integer arithmetic pilot".to_string());
                }
                let left = self.expression(lhs, env)?;
                let right = self.expression(rhs, env)?;
                let ty = merge(&left, &right)?;
                if matches!(op, BinOp::Eq | BinOp::Neq) {
                    return Ok(Type::Bool);
                }
                if ty == Type::Str && *op == BinOp::Add {
                    return Ok(Type::Str);
                }
                if ty != Type::Int {
                    return Err("only Int arithmetic and comparisons are supported".to_string());
                }
                Ok(if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul) {
                    Type::Int
                } else {
                    Type::Bool
                })
            }
            Expr::FnCall(callee, args) => {
                let name = crate::checker::expr_to_str(callee);
                if env.contains_key(&name) {
                    return Err("function-valued calls are outside this pilot".to_string());
                }
                if crate::ast::dotted_name_spells_constructor(&name) {
                    return self.constructor(&name, args, env);
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
                        let expected = self.annotation(ty)?;
                        self.expect(arg, env, expected)?;
                    }
                    let result = self.annotation(&fd.return_type)?;
                    if self.ctx.recursive_fns.contains(&id)
                        && countdown_parameter(fd, self.ctx).is_none()
                        && list_parameter(fd, self.ctx).is_none()
                    {
                        return Err(format!(
                            "recursive function {name} requires checked Int or List descent"
                        ));
                    }
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
                            let ty = self.annotation(ty)?;
                            bind(&mut locals, param, ty)?;
                        }
                        let mut body_type = None;
                        for stmt in fd.body.stmts() {
                            match stmt {
                                Stmt::Binding(local, annotation, value) => {
                                    let mut ty = self.expression(value, &locals)?;
                                    if let Some(annotation) = annotation {
                                        ty = merge(&ty, &self.annotation(annotation)?)?;
                                    }
                                    if local != "_" {
                                        bind(&mut locals, local, ty)?;
                                    }
                                    body_type = None;
                                }
                                Stmt::Expr(value) => {
                                    body_type = Some(self.expression(value, &locals)?)
                                }
                            }
                        }
                        self.checking_functions.pop();
                        merge(
                            &body_type.ok_or("unsupported empty function result")?,
                            &result,
                        )?;
                    }
                    return Ok(result);
                }
                self.builtin(&name, args, env)
            }
            Expr::TailCall(call) => {
                let callee = Spanned::new(Expr::Ident(call.target.clone()), expr.line);
                let call =
                    Spanned::new(Expr::FnCall(Box::new(callee), call.args.clone()), expr.line);
                self.expression(&call, env)
            }
            Expr::Match { subject, arms } => {
                let subject = self.expression(subject, env)?;
                let mut covered = BTreeSet::new();
                let mut result = None;
                for arm in arms {
                    let mut locals = env.clone();
                    self.pattern(&arm.pattern, &subject, &mut locals, &mut covered)?;
                    let ty = self.expression(&arm.body, &locals)?;
                    result = Some(match result {
                        Some(old) => merge(&old, &ty)?,
                        None => ty,
                    });
                }
                self.exhaustive(&subject, &covered)?;
                result.ok_or_else(|| "empty match is outside this pilot".to_string())
            }
            Expr::List(items) => {
                let mut inner = hole();
                for item in items {
                    inner = merge(&inner, &self.expression(item, env)?)?;
                }
                Ok(Type::List(Box::new(inner)))
            }
            Expr::Tuple(items) => Ok(Type::Tuple(
                items
                    .iter()
                    .map(|item| self.expression(item, env))
                    .collect::<Result<_, _>>()?,
            )),
            Expr::Constructor(name, arg) => {
                let args: Vec<_> = arg.iter().map(|arg| arg.as_ref().clone()).collect();
                self.constructor(name, &args, env)
            }
            Expr::Attr(base, field) => {
                let name = crate::checker::expr_to_str(expr);
                if crate::ast::dotted_name_spells_constructor(&name) {
                    return self.constructor(&name, &[], env);
                }
                let ty = self.expression(base, env)?;
                self.field(&ty, field)
            }
            Expr::RecordCreate { type_name, fields } => self.record(type_name, None, fields, env),
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => self.record(type_name, Some(base), updates, env),
            _ => Err("expression is outside the first-order guidance subset".to_string()),
        }
    }

    fn expect(&mut self, expr: &Spanned<Expr>, env: &Env, expected: Type) -> Result<(), String> {
        merge(&self.expression(expr, env)?, &expected).map(|_| ())
    }

    fn builtin(&mut self, name: &str, args: &[Spanned<Expr>], env: &Env) -> Result<Type, String> {
        let scalar = match name {
            "Bool.and" | "Bool.or" => Some((2, Type::Bool, Type::Bool)),
            "Bool.not" => Some((1, Type::Bool, Type::Bool)),
            "Int.abs" => Some((1, Type::Int, Type::Int)),
            "Int.min" | "Int.max" => Some((2, Type::Int, Type::Int)),
            "String.len" => Some((1, Type::Str, Type::Int)),
            _ => None,
        };
        if let Some((arity, input, output)) = scalar {
            if args.len() != arity {
                return Err(format!("unsupported arity for {name}"));
            }
            for arg in args {
                self.expect(arg, env, input.clone())?;
            }
            return Ok(output);
        }
        // Only operations with total, definition-backed Dafny lowerings enter
        // this list. Higher-order List helpers and opaque String helpers do not.
        let values = args
            .iter()
            .map(|arg| self.expression(arg, env))
            .collect::<Result<Vec<_>, _>>()?;
        match (name, values.as_slice()) {
            ("List.len", [Type::List(_)]) => Ok(Type::Int),
            ("List.reverse", [list @ Type::List(_)]) => Ok(list.clone()),
            ("List.take" | "List.drop", [list @ Type::List(_), Type::Int]) => Ok(list.clone()),
            ("List.concat", [left @ Type::List(_), right @ Type::List(_)]) => merge(left, right),
            ("List.prepend", [value, Type::List(inner)]) => {
                Ok(Type::List(Box::new(merge(value, inner)?)))
            }
            ("List.contains", [Type::List(inner), value]) => {
                merge(inner, value)?;
                Ok(Type::Bool)
            }
            ("List.zip", [Type::List(left), Type::List(right)]) => {
                Ok(Type::List(Box::new(Type::Tuple(vec![
                    *left.clone(),
                    *right.clone(),
                ]))))
            }
            ("Result.withDefault", [Type::Result(ok, _), default]) => merge(ok, default),
            ("Option.withDefault", [Type::Option(inner), default]) => merge(inner, default),
            _ => Err(format!("unsupported call {name}")),
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
            return Err("at least one first-order given is required".to_string());
        }
        let mut env = Env::new();
        for given in &law.givens {
            let ty = self.annotation(&given.type_name)?;
            bind(&mut env, &given.name, ty)?;
        }
        if let Some(guard) = &law.when {
            self.expect(guard, &env, Type::Bool)?;
        }
        for reason in &law.because {
            self.expect(reason, &env, Type::Bool)?;
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

/// Share the exact native list-descent classifier with function emission.
pub(super) fn list_parameter(fd: &FnDef, ctx: &CodegenContext) -> Option<usize> {
    crate::codegen::recursion::detect::single_list_descent_param(fd)
        .filter(|_| !crate::codegen::dafny::toplevel::termination_guess_unjustified(fd, ctx))
        .map(|(index, _)| index)
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
        checked_types: HashSet::new(),
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
