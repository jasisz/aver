//! Backend-neutral proof-lemma recognizers.
//!
//! Pure Aver-AST walks that identify the algebraic SHAPE a law exercises and
//! return source-name structs — no backend syntax. The Dafny renderer
//! (`codegen::dafny::lemmas`) and the Lean renderer
//! (`codegen::lean::law_auto::induction`) both consume these, so a single
//! recognizer drives a proof on either backend.
use crate::ast::{Expr, FnBody, FnDef, Pattern, Spanned, Stmt, TailCallData, VerifyLaw};
use crate::codegen::CodegenContext;

/// Collect all function names called in an expression (top-level only).
pub(crate) fn collect_called_fns(
    expr: &Spanned<Expr>,
    out: &mut std::collections::BTreeSet<String>,
) {
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(&f.node) {
                // Skip builtins — only user functions need fuel
                if !name.contains('.') {
                    out.insert(name);
                }
            }
            collect_called_fns(f, out);
            for a in args {
                collect_called_fns(a, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_called_fns(l, out);
            collect_called_fns(r, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_called_fns(subject, out);
            for arm in arms {
                collect_called_fns(&arm.body, out);
            }
        }
        Expr::ErrorProp(inner) => collect_called_fns(inner, out),
        Expr::Constructor(_, Some(arg)) => collect_called_fns(arg, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_called_fns(e, out);
            }
        }
        Expr::List(elems) => {
            for e in elems {
                collect_called_fns(e, out);
            }
        }
        Expr::TailCall(tc) => {
            let TailCallData { target, args, .. } = tc.as_ref();
            if !target.contains('.') {
                out.insert(target.clone());
            }
            for a in args {
                collect_called_fns(a, out);
            }
        }
        Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_called_fns(e, out);
            }
        }
        Expr::Attr(obj, _) => collect_called_fns(obj, out),
        Expr::Neg(inner) => collect_called_fns(inner, out),
        _ => {}
    }
}

pub(crate) fn collect_called_fns_in_body(
    body: &FnBody,
    out: &mut std::collections::BTreeSet<String>,
) {
    match body {
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) => collect_called_fns(expr, out),
                    Stmt::Expr(expr) => collect_called_fns(expr, out),
                }
            }
        }
    }
}

pub(crate) fn short_ctor(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// Recognize a recursive left cons-append `fn A(p0, p1) = match p0 { [] -> p1;
/// [h, ..t] -> List.concat([h], A(t, p1)) }` — the canonical `++`. Drives the
/// rev anti-homomorphism's append-associativity / nil-right helper lemmas.
fn is_recursive_left_append(fd: &FnDef, _ctx: &CodegenContext) -> bool {
    if fd.params.len() != 2 {
        return false;
    }
    let p0 = fd.params[0].0.as_str();
    let p1 = fd.params[1].0.as_str();
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);
    let ln = crate::codegen::recursion::detect::local_name_of;
    let Some(tail) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return false;
    };
    if ln(subject) != Some(p0) || arms.len() != 2 {
        return false;
    }
    let mut nil_ok = false;
    let mut cons_ok = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => nil_ok = ln(&arm.body) == Some(p1),
            Pattern::Cons(h, t) => {
                if let Expr::FnCall(callee, args) = &arm.body.node
                    && dotted(callee).as_deref() == Some("List.concat")
                    && args.len() == 2
                    && matches!(&args[0].node, Expr::List(es) if es.len() == 1 && ln(&es[0]) == Some(h.as_str()))
                    && let Expr::FnCall(rc, ra) = &args[1].node
                    && dotted(rc).as_deref().map(short_ctor) == Some(fd.name.as_str())
                    && ra.len() == 2
                    && ln(&ra[0]) == Some(t.as_str())
                    && ln(&ra[1]) == Some(p1)
                {
                    cons_ok = true;
                }
            }
            _ => {}
        }
    }
    nil_ok && cons_ok
}

/// A list-reversing fold `fn R(p0) = match p0 { [] -> []; [h, ..t] ->
/// A(R(t), [h]) }` paired with its left-append `A`. The classic anti-
/// homomorphism: `R(A(a, b)) == A(R(b), R(a))`. This recognizer is
/// backend-neutral (source names only) — the Lean backend consumes it too,
/// via [`collect_rev_ops_in_law`], to render a kernel-checked proof. (TODO:
/// relocate the recognizer to a backend-neutral module; it lives here for now.)
pub(crate) struct RevOp {
    pub rev: String,
    pub append: String,
}

fn detect_rev_fn(fd: &FnDef, ctx: &CodegenContext) -> Option<RevOp> {
    if fd.params.len() != 1 {
        return None;
    }
    let p0 = fd.params[0].0.as_str();
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);
    let ln = crate::codegen::recursion::detect::local_name_of;
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    if ln(subject) != Some(p0) || arms.len() != 2 {
        return None;
    }
    let mut nil_ok = false;
    let mut append_name: Option<String> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => {
                nil_ok = matches!(&arm.body.node, Expr::List(es) if es.is_empty())
            }
            Pattern::Cons(h, t) => {
                if let Expr::FnCall(callee, args) = &arm.body.node
                    && let Some(app) = dotted(callee)
                    && args.len() == 2
                    && let Expr::FnCall(rc, ra) = &args[0].node
                    && dotted(rc).as_deref().map(short_ctor) == Some(fd.name.as_str())
                    && ra.len() == 1
                    && ln(&ra[0]) == Some(t.as_str())
                    && matches!(&args[1].node, Expr::List(es) if es.len() == 1 && ln(&es[0]) == Some(h.as_str()))
                    && ctx
                        .fn_def_by_name(&app, ctx.active_module_scope().as_deref())
                        .is_some_and(|afd| is_recursive_left_append(afd, ctx))
                {
                    append_name = Some(app);
                }
            }
            _ => {}
        }
    }
    if nil_ok {
        append_name.map(|append| RevOp {
            rev: fd.name.clone(),
            append,
        })
    } else {
        None
    }
}

pub(crate) fn collect_rev_ops_in_law(law: &VerifyLaw, ctx: &CodegenContext) -> Vec<RevOp> {
    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    collect_called_fns(&law.lhs, &mut names);
    collect_called_fns(&law.rhs, &mut names);
    let mut transitive: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for f in &names {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            collect_called_fns_in_body(&fd.body, &mut transitive);
        }
    }
    names.extend(transitive);
    let mut seen = std::collections::BTreeSet::new();
    names
        .iter()
        .filter_map(|f| ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()))
        .filter_map(|fd| detect_rev_fn(fd, ctx))
        .filter(|op| seen.insert(op.rev.clone()))
        .collect()
}

/// A list-length fold `fn L(x: List<Int>) = match x { [] -> base; [h, ..t] ->
/// Succ(L(t)) }` where `Succ` is any unary wrapper (typically a `Nat` succ
/// ctor). For this shape the snoc law `L(s ++ [e]) == Succ(L(s))` holds
/// structurally (one extra `Succ` per appended element), which is what a
/// length-preservation proof needs (e.g. `length(rev x) == length x`).
/// `succ` carries the wrapper as written (e.g. `Nat.S`) so the renderer can
/// reproduce it verbatim.
pub(crate) struct LenFold {
    pub name: String,
    pub succ: String,
}

fn detect_len_fold(fd: &FnDef) -> Option<LenFold> {
    if fd.params.len() != 1 || fd.params[0].1.trim() != "List<Int>" {
        return None;
    }
    let p0 = fd.params[0].0.as_str();
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);
    let ln = crate::codegen::recursion::detect::local_name_of;
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    if ln(subject) != Some(p0) || arms.len() != 2 {
        return None;
    }
    let mut has_base = false;
    let mut succ: Option<String> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => has_base = true,
            Pattern::Cons(_h, t) => {
                if let Expr::FnCall(scallee, sargs) = &arm.body.node
                    && sargs.len() == 1
                    && let Some(sname) = dotted(scallee)
                    && let Expr::FnCall(rc, ra) = &sargs[0].node
                    && dotted(rc).as_deref().map(short_ctor) == Some(fd.name.as_str())
                    && ra.len() == 1
                    && ln(&ra[0]) == Some(t.as_str())
                {
                    succ = Some(sname);
                }
            }
            _ => {}
        }
    }
    if has_base {
        succ.map(|succ| LenFold {
            name: fd.name.clone(),
            succ,
        })
    } else {
        None
    }
}

pub(crate) fn collect_len_folds_in_law(law: &VerifyLaw, ctx: &CodegenContext) -> Vec<LenFold> {
    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    collect_called_fns(&law.lhs, &mut names);
    collect_called_fns(&law.rhs, &mut names);
    let mut transitive: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for f in &names {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            collect_called_fns_in_body(&fd.body, &mut transitive);
        }
    }
    names.extend(transitive);
    let mut seen = std::collections::BTreeSet::new();
    names
        .iter()
        .filter_map(|f| ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()))
        .filter_map(detect_len_fold)
        .filter(|fold| seen.insert(fold.name.clone()))
        .collect()
}
