//! Pure `Int` expression trees computed on a machine word first.
//!
//! Each `AverInt` operation tests both operands for a heap integer and
//! builds its result, although the values almost always fit an `i64`. A
//! tree of the pure, total operations of [`crate::ir::mir::int_tree`] is
//! emitted twice: on `i64` with `checked_*`, and, for a leaf past `i64` or an
//! overflow, as it is emitted without the tree. The `i64` result becomes an
//! `AverInt` once, or stays a machine value when compared or narrowed. A leaf
//! other than a literal or a read of a local (or of a field of one) is
//! evaluated once, in source order, before both paths, which read it from a
//! binding. wasm-gc does the same in `wasm_gc/body/from_mir/int_tree.rs`.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use crate::ir::mir::int_tree::{self, IntOp, Node, int_literal, is_int};

use super::*;

thread_local! {
    /// Exact paths being emitted: while non-zero no tree is computed on `i64`.
    static EXACT: Cell<u32> = const { Cell::new(0) };
    /// What an exact path reads for each leaf evaluated ahead, by node
    /// address, and whether it read it.
    static HOISTED: RefCell<HashMap<usize, (String, bool)>> = RefCell::new(HashMap::new());
}

fn key(e: Node<'_>) -> usize {
    e as *const _ as usize
}

/// On an exact path, the binding a leaf evaluated ahead is read from.
pub(super) fn hoisted(e: Node<'_>) -> Option<String> {
    HOISTED.with(|h| {
        let mut h = h.borrow_mut();
        let (code, read) = h.get_mut(&key(e))?;
        *read = true;
        Some(code.clone())
    })
}

/// The operation `e` is, when it is one on `AverInt`s (not one the
/// `bare_i64` facts already render raw).
fn op_of<'e>(e: Node<'e>, ctx: &MirEmitCtx<'_>) -> Option<(IntOp, Node<'e>, Option<Node<'e>>)> {
    if mir_expr_is_bare_i64(e, ctx) {
        return None;
    }
    int_tree::op_of(e, ctx.mir_builtins)
}

fn weight(e: Node<'_>, ctx: &MirEmitCtx<'_>) -> usize {
    match op_of(e, ctx) {
        Some((_, a, b)) => 1 + weight(a, ctx) + b.map_or(0, |b| weight(b, ctx)),
        None => 0,
    }
}

fn off() -> bool {
    static OFF: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *OFF.get_or_init(|| std::env::var_os("AVER_NO_INT_TREE").is_some())
}

/// `expr` computed on the machine word first: a tree of two operations or
/// more, or a comparison or narrowing of one.
pub(super) fn try_emit(expr: Node<'_>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    if EXACT.with(Cell::get) > 0 || off() {
        return None;
    }
    match &expr.node {
        MirExpr::BinOp(b) => {
            let op = match b.node.op {
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
                _ if is_int(expr) && weight(expr, ctx) >= 2 => return emit(expr, &[expr], None, ctx),
                _ => return None,
            };
            let roots: [Node<'_>; 2] = [&b.node.lhs, &b.node.rhs];
            let int = |r: &Node<'_>| is_int(r) || int_literal(&r.node).is_some();
            if !roots.iter().all(int) || weight(roots[0], ctx) + weight(roots[1], ctx) == 0 {
                return None;
            }
            emit(expr, &roots, Some(op), ctx)
        }
        MirExpr::Unbox(inner) if weight(inner, ctx) > 0 => emit(expr, &[&**inner], Some(""), ctx),
        MirExpr::Neg(_) | MirExpr::Call(_) if is_int(expr) && weight(expr, ctx) >= 2 => {
            emit(expr, &[expr], None, ctx)
        }
        _ => None,
    }
}

/// The leaves of a tree evaluated ahead: their bindings, what the `i64`
/// path and the exact path read for each, and the locals read in place.
#[derive(Default)]
struct Ahead<'e> {
    lets: String,
    fast: HashMap<usize, String>,
    exact: Vec<(usize, String)>,
    nodes: Vec<Node<'e>>,
    reads: Vec<LocalId>,
}

/// `cmp`: the comparison of the two roots (`""`: the one root narrowed to
/// `i64`), or `None` for the root boxed.
fn emit(
    whole: Node<'_>,
    roots: &[Node<'_>],
    cmp: Option<&str>,
    ctx: &MirEmitCtx<'_>,
) -> Option<String> {
    let mut ahead = Ahead::default();
    for r in roots {
        hoist(r, ctx, &mut ahead)?;
    }
    // A leaf evaluated ahead must not move a local the `i64` path reads.
    if ahead.reads.iter().any(|slot| {
        ahead
            .nodes
            .iter()
            .any(|n| mir_last_use_moves_slot(n, *slot, ctx))
    }) {
        return None;
    }
    let fast = roots
        .iter()
        .map(|r| fast(r, &ahead, ctx))
        .collect::<Option<Vec<_>>>()?;
    HOISTED.with(|h| {
        let mut h = h.borrow_mut();
        h.extend(ahead.exact.iter().map(|(k, code)| (*k, (code.clone(), false))))
    });
    EXACT.with(|d| d.set(d.get() + 1));
    let exact = emit_mir_expr(whole, ctx);
    EXACT.with(|d| d.set(d.get() - 1));
    // An exact path that did not read a leaf from its binding evaluated it
    // again: keep the expression as it is.
    let all_read = HOISTED.with(|h| {
        let mut h = h.borrow_mut();
        ahead
            .exact
            .iter()
            .all(|(k, _)| h.remove(k).is_some_and(|(_, read)| read))
    });
    let exact = exact.filter(|_| all_read)?;
    let (ty, value, word) = match cmp {
        None => ("i64", fast[0].clone(), "aver_rt::AverInt::from_i64(__word)"),
        Some("") => ("i64", fast[0].clone(), "__word"),
        Some(op) => ("bool", format!("{} {op} {}", fast[0], fast[1]), "__word"),
    };
    let lets = ahead.lets;
    Some(format!(
        "({{ {lets}let __word: Option<{ty}> = (|| Some({value}))(); match __word {{ Some(__word) => {word}, None => {exact} }} }})"
    ))
}

/// Bind the leaves of `e` that are evaluated ahead, in source order.
fn hoist<'e>(e: Node<'e>, ctx: &MirEmitCtx<'_>, ahead: &mut Ahead<'e>) -> Option<()> {
    if let Some((_, a, b)) = op_of(e, ctx) {
        hoist(a, ctx, ahead)?;
        return b.map_or(Some(()), |b| hoist(b, ctx, ahead));
    }
    let boxed = match &e.node {
        MirExpr::Box(inner) => Some(&**inner),
        _ => None,
    };
    if int_literal(&e.node).is_some() {
        return Some(());
    }
    if let Some(local) = projection_root_local(&boxed.unwrap_or(e).node) {
        ahead.reads.push(local.slot);
        return Some(());
    }
    let name = format!("{}{}", generated_ident("leaf"), ahead.nodes.len());
    let (binding, fast, exact) = match boxed {
        Some(inner) => (
            format!("let {name}: i64 = {};", emit_mir_expr(inner, ctx)?),
            name.clone(),
            format!("aver_rt::AverInt::from_i64({name})"),
        ),
        None if mir_expr_is_bare_i64(e, ctx) => (
            format!("let {name}: i64 = {};", emit_mir_expr(e, ctx)?),
            name.clone(),
            name.clone(),
        ),
        None => (
            format!("let {name} = &({});", emit_mir_expr(e, ctx)?),
            format!("{name}.to_i64()?"),
            format!("(*{name})"),
        ),
    };
    ahead.lets.push_str(&binding);
    ahead.lets.push(' ');
    ahead.fast.insert(key(e), fast);
    ahead.exact.push((key(e), exact));
    ahead.nodes.push(e);
    Some(())
}

/// The `i64` of `e`, leaving the closure (`?`) on a leaf past `i64` or an
/// overflow.
fn fast(e: Node<'_>, ahead: &Ahead<'_>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    let Some((op, a, b)) = op_of(e, ctx) else {
        if let Some(n) = int_literal(&e.node) {
            return Some(format!("({n}i64)"));
        }
        if let Some(read) = ahead.fast.get(&key(e)) {
            return Some(read.clone());
        }
        return match &e.node {
            MirExpr::Box(inner) => emit_mir_expr(inner, ctx),
            _ if mir_expr_is_bare_i64(e, ctx) => emit_mir_expr(e, ctx),
            _ => Some(format!("({}).to_i64()?", emit_mir_expr(e, ctx)?)),
        };
    };
    let x = fast(a, ahead, ctx)?;
    let y = match b {
        Some(b) => fast(b, ahead, ctx)?,
        None => String::new(),
    };
    Some(match op {
        IntOp::Add => format!("({x}).checked_add({y})?"),
        IntOp::Sub => format!("({x}).checked_sub({y})?"),
        IntOp::Mul => format!("({x}).checked_mul({y})?"),
        IntOp::Neg => format!("({x}).checked_neg()?"),
        IntOp::Abs => format!("({x}).checked_abs()?"),
        // An `i64` IS its infinite two's-complement sequence: exact.
        IntOp::And => format!("(({x}) & ({y}))"),
        IntOp::Or => format!("(({x}) | ({y}))"),
        IntOp::Xor => format!("(({x}) ^ ({y}))"),
        IntOp::Not => format!("(!({x}))"),
        IntOp::Min => format!("({x}).min({y})"),
        IntOp::Max => format!("({x}).max({y})"),
        // By a positive divisor neither overflows; `i64::MIN` by `-1` does.
        IntOp::Div(k) if k > 0 => format!("({x}).div_euclid({k}i64)"),
        IntOp::Div(k) => format!("({x}).checked_div_euclid({k}i64)?"),
        IntOp::Mod(k) if k > 0 => format!("({x}).rem_euclid({k}i64)"),
        IntOp::Mod(k) => format!("({x}).checked_rem_euclid({k}i64)?"),
        IntOp::Shl(n) if n <= 62 => format!("({x}).checked_mul({}i64)?", 1i64 << n),
        IntOp::Shr(n) => format!("(({x}) >> {})", n.min(63)),
        // `i64::MAX >> (63 - w)` is the low `w` bits.
        IntOp::Low(w) if w <= 63 => format!("(({x}) & {}i64)", i64::MAX >> (63 - w)),
        // Past the word: kept exact, the whole tree with it.
        IntOp::Shl(_) | IntOp::Low(_) => return None,
    })
}
