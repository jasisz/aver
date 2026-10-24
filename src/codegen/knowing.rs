//! `lazy f(…) knowing f.law(x…)` (see `docs/language.md`, "Deferred calls
//! that know a value"): which law a deferred call names, whether the law has
//! the one shape such a call may stand on, and whether the call is the one
//! the law is about.
//!
//! The law is `P(f(a1, …, an)) => R`, with `P` a function of one argument and
//! every given one of the `ai` as it is. A call `lazy f(b1, …, bn) knowing
//! f.law(x1, …, xk)` gives the givens the values `x1, …, xk`, and each `bi`
//! must be `ai` with them in place of the givens. Then `P` of the call's
//! result is `R` with the same values, which is what lets the Rust backend
//! keep `R` in the deferred node and answer `P` from it.

use crate::ast::{Expr, FnDef, Knowing, Spanned, Stmt, TopLevel, VerifyKind, VerifyLaw};
use crate::ir::FnId;

use super::CodegenContext;

/// One `knowing` of the program: the deferred call it follows and where.
pub struct Use<'a> {
    /// The module of the function the call is in; `None` for the entry.
    pub scope: Option<&'a str>,
    pub call: &'a Spanned<Expr>,
    pub knowing: &'a Knowing,
}

/// A law a deferred call may name, taken apart.
pub struct Law<'a> {
    /// The law's name as reports and the proof backends give it:
    /// `Module.fn.law`, or `fn.law` in the entry.
    pub key: String,
    /// The module the law is in; `None` for the entry.
    pub scope: Option<String>,
    pub law: &'a VerifyLaw,
    /// The deferred function, `f`.
    pub callee: FnId,
    /// The function of the call's result, `P`.
    pub projection: FnId,
    /// For each given, in order, the argument of the call it is.
    pub given_args: Vec<usize>,
}

/// Every `knowing` in the program's functions.
pub fn uses(ctx: &CodegenContext) -> Vec<Use<'_>> {
    let entry = ctx.items.iter().filter_map(|item| match item {
        TopLevel::FnDef(fd) => Some((None, fd)),
        _ => None,
    });
    let deps = ctx
        .modules
        .iter()
        .flat_map(|m| m.fn_defs.iter().map(|fd| (Some(m.prefix.as_str()), fd)));
    let mut out = Vec::new();
    for (scope, fd) in entry.chain(deps) {
        uses_in(scope, fd, &mut out);
    }
    out
}

/// Whether any function among `items` has a `knowing`.
pub fn in_items(items: &[TopLevel]) -> bool {
    let mut found = Vec::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            uses_in(None, fd, &mut found);
        }
    }
    !found.is_empty()
}

fn uses_in<'a>(scope: Option<&'a str>, fd: &'a FnDef, out: &mut Vec<Use<'a>>) {
    for stmt in fd.body.stmts() {
        let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
        crate::call_graph::walk_spanned(expr, &mut |e| {
            if let Some(knowing) = e.lazy.knowing() {
                out.push(Use {
                    scope,
                    call: e,
                    knowing,
                });
            }
        });
    }
}

fn resolve_fn(ctx: &CodegenContext, name: &str, scope: Option<&str>) -> Option<FnId> {
    let scope = scope.map(String::from).or_else(|| ctx.entry_module_name());
    ctx.symbol_table.resolve_fn_id_in(name, scope.as_deref())
}

/// The law `law` of the function `callee`, if it has the shape a deferred
/// call may stand on; why not, if it has not.
pub fn law<'a>(ctx: &'a CodegenContext, callee: FnId, law: &str) -> Result<Law<'a>, String> {
    let key = &ctx.symbol_table.fn_entry(callee).key;
    let scope = key.scope_str().map(String::from);
    let name = &key.name;
    let blocks: Box<dyn Iterator<Item = &crate::ast::VerifyBlock>> = match &scope {
        None => Box::new(ctx.items.iter().filter_map(|item| match item {
            TopLevel::Verify(vb) => Some(&**vb),
            _ => None,
        })),
        Some(prefix) => {
            let prefix = prefix.clone();
            Box::new(
                ctx.modules
                    .iter()
                    .filter(move |m| m.prefix == prefix)
                    .flat_map(|m| m.verify_blocks.iter()),
            )
        }
    };
    let found = blocks
        .filter(|vb| &vb.fn_name == name)
        .find_map(|vb| match &vb.kind {
            VerifyKind::Law(l) if l.name == law => Some(&**l),
            _ => None,
        })
        .ok_or_else(|| format!("`{name}` has no law `{law}`"))?;
    let law_key = match &scope {
        Some(prefix) => format!("{prefix}.{name}.{law}"),
        None => format!("{name}.{law}"),
    };
    let shape = || {
        format!(
            "law `{law_key}` does not say what a function of one argument gives on the result of \
             `{name}`: `knowing` needs a law `P({name}(a1, …)) => R` in which every given is one \
             of the arguments as it is"
        )
    };
    let Expr::FnCall(projection, outer) = &found.lhs.node else {
        return Err(shape());
    };
    let [inner] = outer.as_slice() else {
        return Err(shape());
    };
    let Expr::FnCall(of, args) = &inner.node else {
        return Err(shape());
    };
    let named = |e: &Expr| crate::ir::expr_to_dotted_name(e);
    if named(&of.node).and_then(|n| resolve_fn(ctx, &n, scope.as_deref())) != Some(callee) {
        return Err(shape());
    }
    let projection = named(&projection.node)
        .and_then(|n| resolve_fn(ctx, &n, scope.as_deref()))
        .ok_or_else(shape)?;
    let given_args = found
        .givens
        .iter()
        .map(|g| {
            args.iter()
                .position(|a| named(&a.node).as_deref() == Some(g.name.as_str()))
        })
        .collect::<Option<Vec<usize>>>()
        .ok_or_else(shape)?;
    let returns = ctx
        .resolved_program
        .fn_by_id(projection)
        .map(|p| (p.params.len(), p.return_type.clone()));
    if returns != Some((1, crate::types::Type::Int)) {
        return Err(format!(
            "law `{law_key}`: the value a deferred call knows is an Int, and `{}` does not \
             return one from one argument",
            named(&found.lhs.node).unwrap_or_default()
        ));
    }
    Ok(Law {
        key: law_key,
        scope,
        law: found,
        callee,
        projection,
        given_args,
    })
}

/// The law `use_` names, if the call is the one the law is about with the
/// givens it gives; why not, if not.
pub fn check<'a>(ctx: &'a CodegenContext, use_: &Use<'_>) -> Result<Law<'a>, String> {
    let Expr::FnCall(callee, args) = &use_.call.node else {
        return Err("`knowing` follows a call".to_string());
    };
    let name = crate::ir::expr_to_dotted_name(&callee.node).unwrap_or_default();
    let callee =
        resolve_fn(ctx, &name, use_.scope).ok_or_else(|| format!("`{name}` is not a function"))?;
    let found = law(ctx, callee, &use_.knowing.law)?;
    let givens = &found.law.givens;
    if givens.len() != use_.knowing.givens.len() {
        return Err(format!(
            "law `{}` has {} given(s) ({}), and `knowing` gives {}",
            found.key,
            givens.len(),
            givens
                .iter()
                .map(|g| g.name.as_str())
                .collect::<Vec<_>>()
                .join(", "),
            use_.knowing.givens.len()
        ));
    }
    let Expr::FnCall(_, outer) = &found.law.lhs.node else {
        unreachable!("checked by `law`");
    };
    let Expr::FnCall(_, law_args) = &outer[0].node else {
        unreachable!("checked by `law`");
    };
    let values: Vec<(&str, &Spanned<Expr>)> = givens
        .iter()
        .map(|g| g.name.as_str())
        .zip(&use_.knowing.givens)
        .collect();
    for (i, (arg, law_arg)) in args.iter().zip(law_args.iter()).enumerate() {
        if !same(arg, law_arg, &values) {
            return Err(format!(
                "argument {} of `{name}` is not what law `{}` has there with the givens \
                 `knowing` gives; write the call as the law does, with the givens in place",
                i + 1,
                found.key
            ));
        }
    }
    Ok(found)
}

/// Whether `arg`, from the call, is `law_arg`, from the law, with each given
/// in `values` in place of its name. Names compare as written.
fn same(arg: &Spanned<Expr>, law_arg: &Spanned<Expr>, values: &[(&str, &Spanned<Expr>)]) -> bool {
    let name = |e: &Expr| match e {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.clone()),
        _ => None,
    };
    if let Some(given) = name(&law_arg.node)
        && let Some((_, value)) = values.iter().find(|(g, _)| *g == given)
    {
        return same(arg, value, &[]);
    }
    let all = |a: &[Spanned<Expr>], b: &[Spanned<Expr>]| {
        a.len() == b.len() && a.iter().zip(b).all(|(x, y)| same(x, y, values))
    };
    match (&arg.node, &law_arg.node) {
        (Expr::Literal(a), Expr::Literal(b)) => a == b,
        (Expr::Attr(a, f), Expr::Attr(b, g)) => f == g && same(a, b, values),
        (Expr::FnCall(f, a), Expr::FnCall(g, b)) => same(f, g, values) && all(a, b),
        (Expr::BinOp(o, a, b), Expr::BinOp(p, c, d)) => {
            o == p && same(a, c, values) && same(b, d, values)
        }
        (Expr::Neg(a), Expr::Neg(b)) => same(a, b, values),
        (Expr::Constructor(c, a), Expr::Constructor(d, b)) => {
            c == d
                && match (a, b) {
                    (Some(a), Some(b)) => same(a, b, values),
                    (None, None) => true,
                    _ => false,
                }
        }
        (Expr::Tuple(a), Expr::Tuple(b)) | (Expr::List(a), Expr::List(b)) => all(a, b),
        (a, b) => name(a).is_some() && name(a) == name(b),
    }
}
