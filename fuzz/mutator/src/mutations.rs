//! Class-A AST mutations: syntax-preserving, type-breaking.
//!
//! Each strategy walks the AST in place, applies one local edit,
//! and returns `true` if it actually changed anything. The caller
//! (`afl_api::custom_fuzz`) then runs the result through `unparse`
//! and the parser to produce AFL output bytes. Mutations that
//! find no site to edit return `false`, which the caller maps to
//! "skip — AFL falls back to byte havoc on this input".
//!
//! These are deliberately small, local edits. The goal is **more
//! valid-Aver inputs reaching the typecheck / resolver / codegen
//! layers** that byte-level fuzzing rarely penetrates. Each
//! strategy is designed so its output:
//!
//!   - parses (guaranteed by post-unparse re-parse in the caller),
//!   - frequently fails to typecheck (the high-value coverage zone),
//!   - occasionally still typechecks (catches semantic-level bugs
//!     too, when the edit happens to preserve types).
//!
//! Strategy indices are stable for the lifetime of the campaign —
//! AFL stores seed-to-strategy mappings inside its queue metadata,
//! so reshuffling the dispatch would invalidate corpus resume.

use aver::ast::{BinOp, Expr, FnDef, Literal, Spanned, Stmt, TopLevel};
use rand::Rng;
use rand::seq::IndexedRandom;

use crate::typed_gen;

/// Total number of strategies available. The dispatcher modulo-
/// indexes into this so callers can pick a strategy uniformly with
/// `rng.random_range(0..STRATEGY_COUNT)`.
pub const STRATEGY_COUNT: u32 = 16;

/// Apply one mutation strategy chosen by `strategy_index`. Returns
/// `true` if the AST was modified, `false` if the strategy found
/// no applicable site (caller treats `false` as "skip this
/// mutation"). Takes a `&mut Vec` so the strategies that grow the
/// program (e.g. `inject-fn`) can push without resorting to a
/// scratch buffer; in-place strategies still see the slice they
/// need via `&mut **items` coercion at the call site.
pub fn apply<R: Rng>(strategy_index: u32, rng: &mut R, items: &mut Vec<TopLevel>) -> bool {
    match strategy_index % STRATEGY_COUNT {
        0 => swap_binop(rng, items.as_mut_slice()),
        1 => replace_int_literal_boundary(rng, items.as_mut_slice()),
        2 => inject_error_prop(rng, items.as_mut_slice()),
        3 => remove_error_prop(rng, items.as_mut_slice()),
        4 => swap_adjacent_match_arms(rng, items.as_mut_slice()),
        5 => swap_bang_element(rng, items.as_mut_slice()),
        6 => swap_type_annotation(rng, items.as_mut_slice()),
        7 => rename_param(rng, items.as_mut_slice()),
        8 => swap_adjacent_statements(rng, items.as_mut_slice()),
        9 => negate_expression(rng, items.as_mut_slice()),
        10 => replace_ident_with_bare_namespace(rng, items.as_mut_slice()),
        11 => toggle_module_effects(rng, items.as_mut_slice()),
        12 => reverse_match_arms(rng, items.as_mut_slice()),
        13 => shift_map_literal_entries(rng, items.as_mut_slice()),
        14 => replace_fn_body(rng, items.as_mut_slice()),
        15 => inject_fn(rng, items),
        _ => unreachable!(),
    }
}

/// Short label per strategy. Used by the AFL custom mutator
/// `describe` hook so the queue filename + crash report name carries
/// the strategy that produced the input. Keep these kebab-case and
/// short — AFL embeds them into filenames. No `/`: AFL splices the
/// describe string into `id:NNN,...,<describe>` and a `/` turns it
/// into a subdir path that AFL doesn't create, aborting the run with
/// a SYSTEM ERROR.
pub fn strategy_name(strategy_index: u32) -> &'static str {
    match strategy_index % STRATEGY_COUNT {
        0 => "swap-binop",
        1 => "int-boundary",
        2 => "inject-err",
        3 => "remove-err",
        4 => "swap-match-arms",
        5 => "swap-bang-element",
        6 => "swap-type-ann",
        7 => "rename-param",
        8 => "swap-stmts",
        9 => "neg-expr",
        10 => "bare-namespace",
        11 => "toggle-effects",
        12 => "reverse-match-arms",
        13 => "shift-map-entries",
        14 => "replace-fn-body",
        15 => "inject-fn",
        _ => unreachable!(),
    }
}

// ---------------------------------------------------------------------------
// Site collection — walk AST, collect mutable references the
// strategies will edit. Each strategy picks one site uniformly at
// random, applies its edit, returns.
// ---------------------------------------------------------------------------

/// Visit every `Spanned<Expr>` reachable from `items`, applying
/// `visitor` to each. Stops on the first `true` return from
/// `visitor` (treats it as "I made my edit, we're done").
fn visit_exprs_mut<F>(items: &mut [TopLevel], mut visitor: F) -> bool
where
    F: FnMut(&mut Spanned<Expr>) -> bool,
{
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            // FnDef.body is Arc<FnBody>; force a clone so we can
            // mutate. Other FnDefs in the same module shared the
            // body via Arc but the parser produces a fresh Arc per
            // fn so this is cheap in practice.
            let body = std::sync::Arc::make_mut(&mut fd.body);
            for stmt in body.stmts_mut() {
                let expr = match stmt {
                    Stmt::Expr(e) => e,
                    Stmt::Binding(_, _, e) => e,
                };
                if visit_expr_mut(expr, &mut visitor) {
                    return true;
                }
            }
        }
    }
    false
}

/// Recursive descent that drives `visitor` against each subexpression.
/// Stops the moment `visitor` returns `true`.
fn visit_expr_mut<F>(expr: &mut Spanned<Expr>, visitor: &mut F) -> bool
where
    F: FnMut(&mut Spanned<Expr>) -> bool,
{
    if visitor(expr) {
        return true;
    }
    match &mut expr.node {
        Expr::BinOp(_, lhs, rhs) => visit_expr_mut(lhs, visitor) || visit_expr_mut(rhs, visitor),
        Expr::Neg(inner) => visit_expr_mut(inner, visitor),
        Expr::Attr(inner, _) => visit_expr_mut(inner, visitor),
        Expr::FnCall(callee, args) => {
            if visit_expr_mut(callee, visitor) {
                return true;
            }
            args.iter_mut().any(|a| visit_expr_mut(a, visitor))
        }
        Expr::Constructor(_, Some(arg)) => visit_expr_mut(arg, visitor),
        Expr::Match { subject, arms } => {
            if visit_expr_mut(subject, visitor) {
                return true;
            }
            arms.iter_mut()
                .any(|arm| visit_expr_mut(&mut arm.body, visitor))
        }
        Expr::ErrorProp(inner) => visit_expr_mut(inner, visitor),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter_mut().any(|it| visit_expr_mut(it, visitor))
        }
        Expr::MapLiteral(entries) => entries
            .iter_mut()
            .any(|(k, v)| visit_expr_mut(k, visitor) || visit_expr_mut(v, visitor)),
        Expr::RecordCreate { fields, .. } => {
            fields.iter_mut().any(|(_, v)| visit_expr_mut(v, visitor))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            if visit_expr_mut(base, visitor) {
                return true;
            }
            updates.iter_mut().any(|(_, v)| visit_expr_mut(v, visitor))
        }
        Expr::InterpolatedStr(parts) => parts.iter_mut().any(|p| match p {
            aver::ast::StrPart::Parsed(e) => visit_expr_mut(e, visitor),
            aver::ast::StrPart::Literal(_) => false,
        }),
        // Leaves + post-parse-only variants.
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Individual strategies
// ---------------------------------------------------------------------------

/// Find every `BinOp` site, pick one, swap its operator with
/// another from the same numeric family. Hits typecheck error
/// paths when `+` becomes `==` (Bool vs numeric), VM dispatch
/// when `*` becomes `/` (divide-by-zero), and codegen when the
/// swap is across Int / Float typed-opcode boundaries.
fn swap_binop<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let mut sites: Vec<*mut BinOp> = Vec::new();
    visit_exprs_mut(items, |expr| {
        if let Expr::BinOp(op, _, _) = &mut expr.node {
            sites.push(op as *mut BinOp);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    // SAFETY: `items` is borrowed mutably for the whole function;
    // the raw pointer was just collected from the same borrow and
    // no other reference into the tree exists between the visit
    // call and this write.
    let current = unsafe { *site };
    let alternatives = match current {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
            &[BinOp::Add, BinOp::Sub, BinOp::Mul, BinOp::Div][..]
        }
        BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => &[
            BinOp::Eq,
            BinOp::Neq,
            BinOp::Lt,
            BinOp::Gt,
            BinOp::Lte,
            BinOp::Gte,
        ][..],
    };
    let new = alternatives
        .iter()
        .copied()
        .filter(|&o| o != current)
        .collect::<Vec<_>>();
    let &chosen = new.choose(rng).unwrap();
    unsafe { *site = chosen };
    true
}

/// Pick one Int literal and replace with a boundary value
/// (overflow / underflow / zero / negative one). Drives `arith_*`
/// codegen overflow paths and Int / Float coercion edges.
fn replace_int_literal_boundary<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    // Boundaries below i64::MAX. The lexer parses Int literals as
    // a positive magnitude with `-` token separate; the magnitude
    // itself must fit in i64. `i64::MIN` therefore can't be
    // emitted as a literal (its positive form is i64::MAX + 1).
    // Stop one short of MAX both directions.
    const BOUNDARIES: &[i64] = &[
        0,
        1,
        -1,
        i64::MAX,
        -(i64::MAX),
        2_147_483_647,
        -2_147_483_648,
    ];
    let mut sites: Vec<*mut i64> = Vec::new();
    visit_exprs_mut(items, |expr| {
        if let Expr::Literal(Literal::Int(i)) = &mut expr.node {
            sites.push(i as *mut i64);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let current = unsafe { *site };
    let new_val = BOUNDARIES
        .iter()
        .copied()
        .filter(|&v| v != current)
        .collect::<Vec<_>>()
        .choose(rng)
        .copied()
        .unwrap_or(0);
    unsafe { *site = new_val };
    true
}

/// Wrap a random non-leaf expression in `Expr::ErrorProp`. Most
/// receivers aren't `Result`-typed; surfaces "`?` on non-Result"
/// typecheck paths. When the receiver happens to be a Result,
/// flushes the Result-unwrap path through resolver + codegen.
fn inject_error_prop<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    // Two filters at once: shape (don't wrap leaves / existing
    // ErrorProps / Match) and context (don't wrap a callee slot,
    // because `(x)?(args)` is not a legal Aver shape — the
    // parser treats `?` as a final postfix and refuses to follow
    // it with another call).
    //
    // Match exclusion is shape-side: the unparser emits the
    // block-form `match x\n    pat -> body\n    ...`, and even
    // wrapped in parens the inner arm bodies carry their own
    // newlines, so `(match ...)?` reparses as `pattern ')'`.
    let sites = collect_sites_with_ctx(items, |expr, ctx| {
        if matches!(ctx, ExprCtx::Callee { .. }) {
            return false;
        }
        matches!(
            &expr.node,
            Expr::FnCall(..) | Expr::Attr(..) | Expr::Ident(_)
        )
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    // SAFETY: see swap_binop. We replace the `node` field's
    // contents via a `std::mem::replace`, leaving the Spanned's
    // line / ty fields intact (line is wrong-but-valid for the
    // new wrapped shape; fuzz doesn't care about source lines).
    let span_ref: &mut Spanned<Expr> = unsafe { &mut *site };
    let line = span_ref.line;
    let placeholder = Spanned::new(Expr::Literal(Literal::Unit), line);
    let inner = std::mem::replace(span_ref, placeholder);
    *span_ref = Spanned::new(Expr::ErrorProp(Box::new(inner)), line);
    true
}

/// Strip one `Expr::ErrorProp`, leaving its inner expression.
/// Opposite shape from `inject_error_prop`. Same typecheck paths,
/// different polarity (now a Result leaks where the caller wanted
/// the unwrapped type).
fn remove_error_prop<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let mut sites: Vec<*mut Spanned<Expr>> = Vec::new();
    visit_exprs_mut(items, |expr| {
        if matches!(&expr.node, Expr::ErrorProp(_)) {
            sites.push(expr as *mut Spanned<Expr>);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let span_ref: &mut Spanned<Expr> = unsafe { &mut *site };
    let line = span_ref.line;
    let placeholder = Spanned::new(Expr::Literal(Literal::Unit), line);
    let outer = std::mem::replace(span_ref, placeholder);
    if let Expr::ErrorProp(inner) = outer.node {
        *span_ref = *inner;
        true
    } else {
        unreachable!("collected sites guaranteed to be ErrorProp")
    }
}

/// Find a `match` with ≥2 arms, swap two of them. Hits
/// exhaustiveness-checker paths (post-Iron the checker is
/// position-independent so most swaps still typecheck, but the
/// edit churns through the per-arm visit code) and uncovers
/// arm-order bugs in codegen jump-table generation.
fn swap_adjacent_match_arms<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    // Collect first, then mutate one — same pattern the other
    // strategies use. The closure-with-skip approach failed
    // because the rng.random_range(0..u32::MAX) skip count
    // almost never decremented to zero before the visitor ran
    // out of sites.
    let mut sites: Vec<*mut Vec<aver::ast::MatchArm>> = Vec::new();
    visit_exprs_mut(items, |expr| {
        if let Expr::Match { arms, .. } = &mut expr.node
            && arms.len() >= 2
        {
            sites.push(arms as *mut Vec<aver::ast::MatchArm>);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let arms: &mut Vec<aver::ast::MatchArm> = unsafe { &mut *site };
    let idx = rng.random_range(0..arms.len() - 1);
    arms.swap(idx, idx + 1);
    true
}

/// Swap two elements inside an `(a, b, c)!` independent product.
/// Aver's typecheck enforces every element is a fn call, so the
/// shape post-swap is still legal — the mutation is always
/// typecheck-clean and reaches codegen + CALL_PAR dispatch.
/// Semantically it's a real shake: with the 0.21.1 order-axis
/// hostile twin, the forward run's positional tuple is `(f(), g())`
/// while a permuted source is `(g(), f())` — different positions,
/// same independence claim, exercising the assumption that source
/// order is purely positional and not load-bearing for the result.
fn swap_bang_element<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let mut sites: Vec<*mut Vec<Spanned<Expr>>> = Vec::new();
    visit_exprs_mut(items, |expr| {
        if let Expr::IndependentProduct(items, _) = &mut expr.node
            && items.len() >= 2
        {
            sites.push(items as *mut Vec<Spanned<Expr>>);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let items: &mut Vec<Spanned<Expr>> = unsafe { &mut *site };
    let i = rng.random_range(0..items.len());
    let j = {
        let mut k = rng.random_range(0..items.len());
        if k == i {
            k = (k + 1) % items.len();
        }
        k
    };
    items.swap(i, j);
    true
}

/// Swap one parameter's type annotation with a different primitive.
/// Parser accepts every uppercase name in type position; typecheck
/// rejects when the body uses the parameter with the original type.
/// Hits the "got T1, expected T2" path in the matcher.
fn swap_type_annotation<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    const TYPES: &[&str] = &["Int", "Float", "String", "Bool", "Unit"];
    let mut fn_sites: Vec<*mut FnDef> = Vec::new();
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            fn_sites.push(fd as *mut FnDef);
        }
    }
    let &fn_site = fn_sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if fn_site.is_null() {
        return false;
    }
    let fd: &mut FnDef = unsafe { &mut *fn_site };
    if fd.params.is_empty() {
        return false;
    }
    let param_idx = rng.random_range(0..fd.params.len());
    let (_, current_ty) = &fd.params[param_idx];
    let new_ty = TYPES
        .iter()
        .copied()
        .filter(|t| *t != current_ty)
        .collect::<Vec<_>>()
        .choose(rng)
        .copied()
        .unwrap_or("Int");
    fd.params[param_idx].1 = new_ty.to_string();
    true
}

/// Rename one parameter. Body retains the old name in any
/// `Expr::Ident` references — surfaces resolver "unknown
/// identifier" paths.
fn rename_param<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    const REPLACEMENTS: &[&str] = &["x", "y", "z", "tmp", "v", "i", "n", "acc", "result"];
    let mut fn_sites: Vec<*mut FnDef> = Vec::new();
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            fn_sites.push(fd as *mut FnDef);
        }
    }
    let &fn_site = fn_sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if fn_site.is_null() {
        return false;
    }
    let fd: &mut FnDef = unsafe { &mut *fn_site };
    if fd.params.is_empty() {
        return false;
    }
    let param_idx = rng.random_range(0..fd.params.len());
    let current_name = fd.params[param_idx].0.clone();
    let new_name = REPLACEMENTS
        .iter()
        .copied()
        .filter(|n| *n != current_name)
        .collect::<Vec<_>>()
        .choose(rng)
        .copied()
        .unwrap_or("x");
    fd.params[param_idx].0 = new_name.to_string();
    true
}

/// Swap two adjacent statements inside one fn body. Hits
/// scope-resolution paths (the second statement may now reference
/// a binding that hasn't been introduced yet) and resolver
/// slot-allocation paths (slots renumber when order changes).
fn swap_adjacent_statements<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let mut fn_sites: Vec<*mut FnDef> = Vec::new();
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            fn_sites.push(fd as *mut FnDef);
        }
    }
    let &fn_site = fn_sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if fn_site.is_null() {
        return false;
    }
    let fd: &mut FnDef = unsafe { &mut *fn_site };
    let body = std::sync::Arc::make_mut(&mut fd.body);
    let stmts = body.stmts_mut();
    if stmts.len() < 2 {
        return false;
    }
    let idx = rng.random_range(0..stmts.len() - 1);
    stmts.swap(idx, idx + 1);
    true
}

/// Wrap a numeric expression in `Expr::Neg`. Doubles a sign
/// (catches `-0.0` preservation paths post-Iron-A1), tests
/// codegen typed-NEG opcode emission (Iron-B1).
fn negate_expression<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let mut sites: Vec<*mut Spanned<Expr>> = Vec::new();
    visit_exprs_mut(items, |expr| {
        // Heuristic: target expressions that *might* be numeric
        // (Ident, FnCall, Literal::Int, Literal::Float).
        // Wrapping a string or bool produces a typecheck error
        // which is the high-coverage case anyway.
        if matches!(
            &expr.node,
            Expr::Ident(_)
                | Expr::FnCall(..)
                | Expr::Literal(Literal::Int(_))
                | Expr::Literal(Literal::Float(_))
        ) {
            sites.push(expr as *mut Spanned<Expr>);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let span_ref: &mut Spanned<Expr> = unsafe { &mut *site };
    let line = span_ref.line;
    let placeholder = Spanned::new(Expr::Literal(Literal::Unit), line);
    let inner = std::mem::replace(span_ref, placeholder);
    *span_ref = Spanned::new(Expr::Neg(Box::new(inner)), line);
    true
}

/// Replace a random `Expr::Ident(name)` with a bare namespace
/// identifier — `Vector`, `List`, `Option`, `Map`, `Result`. These
/// are namespace handles in Aver, not first-class values; the
/// typechecker accepts them as `Type::Var` in some positions but
/// the wasm-gc codegen's `aver_type_of` assert fires on them when
/// they reach codegen unstamped. Iron 0.21 nightly's first
/// fuzz_codegen_wasm_gc run produced 3 such crashes; we mitigated
/// with `catch_unwind` in the target, but a targeted mutator
/// strategy keeps the surface under fuzz pressure so a future
/// real-fix can be checked against actual coverage.
fn replace_ident_with_bare_namespace<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    const NAMESPACES: &[&str] = &["Vector", "List", "Option", "Map", "Result"];
    // Context-aware walk: skip zero-arg callee slots so we never
    // emit `Namespace()` (the parser explicitly rejects this with
    // `Zero-argument constructor call ... is not allowed`). The
    // in-arg / RHS / list-element positions are still in play —
    // those are the ones that flow a bare namespace handle into
    // codegen's `aver_type_of` assert, which is the wasm-gc bug
    // surface this strategy was added to keep under pressure.
    let sites = collect_sites_with_ctx(items, |expr, ctx| {
        let Expr::Ident(name) = &expr.node else {
            return false;
        };
        if NAMESPACES.contains(&name.as_str()) {
            return false;
        }
        !matches!(ctx, ExprCtx::Callee { zero_args: true })
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let span_ref: &mut Spanned<Expr> = unsafe { &mut *site };
    let Expr::Ident(name) = &mut span_ref.node else {
        return false;
    };
    let new_name = NAMESPACES.choose(rng).copied().unwrap_or("Vector");
    *name = new_name.to_string();
    true
}

/// Parent-context flag the context-aware walker hands every site
/// it visits. The variants that show up here mirror the parser
/// positions where post-mutation shape collisions actually occur
/// — strategies that wrap (`?`, `-`) or rewrite a node need to
/// know whether the parent will accept the new shape *before*
/// committing the edit.
#[derive(Copy, Clone, PartialEq)]
enum ExprCtx {
    /// Stand-alone position: statement body, RHS of a binding,
    /// element of a list/tuple/record/map literal, match arm
    /// body, ... — anywhere the parser is willing to read a
    /// fresh expression from the precedence chain entry.
    Free,
    /// Slot of `Expr::FnCall(<here>, args)`. The Aver parser
    /// rejects `Namespace()` and `(x)?(args)`-style shapes here,
    /// because a callee that already encodes a postfix-style
    /// operator can't be re-used as the head of a call list.
    /// `zero_args` is true when the enclosing FnCall has an empty
    /// arg list — strategy_10 cares specifically about that
    /// subset (bare `Namespace` in a `Namespace()` slot), while
    /// strategy_2 wants to skip any callee context to avoid the
    /// `?`-wrap.
    Callee { zero_args: bool },
}

/// Context-aware analogue of `visit_exprs_mut`. The filter
/// callback receives both the candidate node and its parent
/// `ExprCtx` so strategies can express "skip me if I'm in a
/// callee slot" without needing a custom walker per strategy.
/// Returning `true` from the filter records the node's address
/// as a candidate site; the strategy then picks one at random
/// from the returned vec and performs its edit.
fn collect_sites_with_ctx<F>(
    items: &mut [TopLevel],
    mut filter: F,
) -> Vec<*mut Spanned<Expr>>
where
    F: FnMut(&Spanned<Expr>, ExprCtx) -> bool,
{
    let mut sites: Vec<*mut Spanned<Expr>> = Vec::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let body = std::sync::Arc::make_mut(&mut fd.body);
            for stmt in body.stmts_mut() {
                let expr = match stmt {
                    Stmt::Expr(e) => e,
                    Stmt::Binding(_, _, e) => e,
                };
                walk_with_ctx(expr, ExprCtx::Free, &mut filter, &mut sites);
            }
        }
    }
    sites
}

fn walk_with_ctx<F>(
    expr: &mut Spanned<Expr>,
    ctx: ExprCtx,
    filter: &mut F,
    out: &mut Vec<*mut Spanned<Expr>>,
) where
    F: FnMut(&Spanned<Expr>, ExprCtx) -> bool,
{
    if filter(expr, ctx) {
        out.push(expr as *mut Spanned<Expr>);
    }
    match &mut expr.node {
        Expr::FnCall(callee, args) => {
            let cctx = ExprCtx::Callee {
                zero_args: args.is_empty(),
            };
            walk_with_ctx(callee, cctx, filter, out);
            for a in args.iter_mut() {
                walk_with_ctx(a, ExprCtx::Free, filter, out);
            }
        }
        Expr::BinOp(_, lhs, rhs) => {
            walk_with_ctx(lhs, ExprCtx::Free, filter, out);
            walk_with_ctx(rhs, ExprCtx::Free, filter, out);
        }
        Expr::Neg(inner) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
            walk_with_ctx(inner, ExprCtx::Free, filter, out);
        }
        Expr::Constructor(_, Some(arg)) => {
            walk_with_ctx(arg, ExprCtx::Free, filter, out);
        }
        Expr::Match { subject, arms } => {
            walk_with_ctx(subject, ExprCtx::Free, filter, out);
            for arm in arms.iter_mut() {
                walk_with_ctx(&mut arm.body, ExprCtx::Free, filter, out);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items.iter_mut() {
                walk_with_ctx(it, ExprCtx::Free, filter, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries.iter_mut() {
                walk_with_ctx(k, ExprCtx::Free, filter, out);
                walk_with_ctx(v, ExprCtx::Free, filter, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields.iter_mut() {
                walk_with_ctx(v, ExprCtx::Free, filter, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_with_ctx(base, ExprCtx::Free, filter, out);
            for (_, v) in updates.iter_mut() {
                walk_with_ctx(v, ExprCtx::Free, filter, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts.iter_mut() {
                if let aver::ast::StrPart::Parsed(e) = p {
                    walk_with_ctx(e, ExprCtx::Free, filter, out);
                }
            }
        }
        _ => {}
    }
}

/// Cycle the `effects [...]` clause on the first `TopLevel::Module`
/// declaration. Three states form a cycle:
///   * `None` (legacy / no declaration) → `Some(vec![])` (explicit pure)
///   * `Some(vec![])` → `Some(vec!["Console.print".to_string()])`
///   * non-empty → `None`
/// Crosses the boundary the typechecker's effect-surface checker
/// gates on. Iron 0.21 parity nightly surfaced a real
/// `effects []` + `verify` block divergence (`List literal:
/// cannot resolve list instantiation`) that the existing byte
/// havoc only stumbled into by accident; this strategy targets
/// the transition directly.
fn toggle_module_effects<R: Rng>(_rng: &mut R, items: &mut [TopLevel]) -> bool {
    for item in items {
        if let TopLevel::Module(module) = item {
            match &module.effects {
                None => {
                    module.effects = Some(Vec::new());
                }
                Some(effs) if effs.is_empty() => {
                    module.effects = Some(vec!["Console.print".to_string()]);
                }
                Some(_) => {
                    module.effects = None;
                }
            }
            return true;
        }
    }
    false
}

/// Reverse the entire match arm list. Aver match is first-arm-wins
/// (Aver source order), so flipping `[Lit(0) -> ..., _ -> ...]` to
/// `[_ -> ..., Lit(0) -> ...]` is a real semantic shake — the
/// catch-all arm now shadows the literal one. With the 0.21
/// unreachable-arm detection in place, the result is either
/// typecheck-clean (reachable in the new order) or a clean error
/// (an arm became unreachable). Either way the input lands on a
/// post-typecheck shape, unlike the now-deleted `dup-match-arm`
/// which always tripped the new unreachable check. Reversing one
/// arm is `swap-adjacent-match-arms`; reversing the whole list is
/// a strictly different shape, useful for code where the catch-all
/// sits at the end.
fn reverse_match_arms<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let mut sites: Vec<*mut Vec<aver::ast::MatchArm>> = Vec::new();
    visit_exprs_mut(items, |expr| {
        if let Expr::Match { arms, .. } = &mut expr.node
            && arms.len() >= 2
        {
            sites.push(arms as *mut Vec<aver::ast::MatchArm>);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let arms: &mut Vec<aver::ast::MatchArm> = unsafe { &mut *site };
    arms.reverse();
    true
}

/// Rotate the entry list of a map literal by one position. The
/// keys, values, and the entry count stay identical — Map is an
/// unordered collection at the value level — so typecheck and the
/// usual semantic invariants are unaffected. But the IR / codegen
/// preserves source order in the literal initialiser, so the
/// rotation exercises insertion-order paths in the Map builder
/// (hash-table chunking on the WASM/Rust backends, Finset
/// canonicalisation on the Lean proof backend, set-literal flatten
/// on Dafny). Strategies #2–#3 (`inject-err` / `remove-err`)
/// cover the Result-value shake; this one covers the Map-value
/// shake that no current strategy touches.
fn shift_map_literal_entries<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let mut sites: Vec<*mut Vec<(Spanned<Expr>, Spanned<Expr>)>> = Vec::new();
    visit_exprs_mut(items, |expr| {
        if let Expr::MapLiteral(entries) = &mut expr.node
            && entries.len() >= 2
        {
            sites.push(entries as *mut Vec<(Spanned<Expr>, Spanned<Expr>)>);
        }
        false
    });
    let &site = sites.choose(rng).unwrap_or(&std::ptr::null_mut());
    if site.is_null() {
        return false;
    }
    let entries: &mut Vec<(Spanned<Expr>, Spanned<Expr>)> = unsafe { &mut *site };
    let shift = rng.random_range(1..entries.len());
    entries.rotate_left(shift);
    true
}

/// Pick a random pure `FnDef`, throw away its body, and splice in a
/// freshly-generated expression of the declared return type. The
/// generator walks bottom-up against a structural type so the new
/// body typechecks by construction — different polarity from the
/// other strategies, which mostly route inputs into typecheck
/// rejection paths. Effectful fns (declared `! [...]`) are skipped:
/// the generator only emits pure expressions, so splicing one in
/// would produce a declared-effects-but-empty-body shape the parser
/// accepts but every effect-aware downstream pass flags as suspect.
/// That mismatch is its own coverage slice, but it overlaps the
/// existing `toggle-module-effects` shake; the value-add of this
/// strategy is the well-typed expression, not the effect drift.
fn replace_fn_body<R: Rng>(rng: &mut R, items: &mut [TopLevel]) -> bool {
    let snapshot: Vec<TopLevel> = items.to_vec();
    let mut candidates: Vec<usize> = Vec::new();
    for (i, item) in items.iter().enumerate() {
        if let TopLevel::FnDef(fd) = item {
            if fd.effects.is_empty() && typed_gen::parse_type(&fd.return_type).is_some() {
                candidates.push(i);
            }
        }
    }
    let &idx = candidates.choose(rng).unwrap_or(&usize::MAX);
    if idx == usize::MAX {
        return false;
    }
    if let TopLevel::FnDef(fd) = &mut items[idx] {
        typed_gen::replace_body(rng, &snapshot, fd)
    } else {
        false
    }
}

/// Synthesize a fresh pure fn and append it to the program. Uses
/// the type-driven generator to build a body matching the random
/// signature, so the new fn typechecks by construction. Append
/// rather than insert so the `module` declaration (which must be
/// the first top-level) keeps its position.
fn inject_fn<R: Rng>(rng: &mut R, items: &mut Vec<TopLevel>) -> bool {
    let existing_names: Vec<String> = items
        .iter()
        .filter_map(|i| {
            if let TopLevel::FnDef(fd) = i {
                Some(fd.name.clone())
            } else {
                None
            }
        })
        .collect();
    let Some(new_fn) = typed_gen::synthesize_fn(rng, items, &existing_names) else {
        return false;
    };
    items.push(TopLevel::FnDef(new_fn));
    true
}
