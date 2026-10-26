//! Pure `Int` expression trees computed on the machine word first.
//!
//! An inline `AverInt` is the word `2n + 1`, and each operation's fast path
//! works on the words: it tests its operands for a heap integer, computes,
//! and its result is tested again by the next operation and when dropped.
//! A tree of the pure, total operations of [`crate::ir::mir::int_tree`] is
//! emitted twice: on the words, testing each leaf once, with the operations
//! of those fast paths, and, for a heap integer or a result past the inline
//! range, as it is emitted without the tree. The word becomes an `AverInt`
//! once, or is compared or narrowed as it is. A leaf other than a literal or
//! a read of a local (or of a field of one) is evaluated once, in source
//! order, before both paths, which read it from a binding. wasm-gc does the
//! same on `i64` in `wasm_gc/body/from_mir/int_tree.rs`.

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

/// `expr` computed on the words first: a tree of two operations or more,
/// or a comparison or narrowing of one.
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
                _ if is_int(expr) && weight(expr, ctx) >= 2 => {
                    return emit(expr, &[expr], None, ctx);
                }
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
    // A tree of values evaluated ahead only (`Int.max(f(a), f(b)) + 1`) saves
    // no test of a word the operations would repeat, and a leaf evaluated
    // ahead must not move a local the `i64` path reads.
    if ahead.reads.is_empty()
        || ahead.reads.iter().any(|slot| {
            ahead
                .nodes
                .iter()
                .any(|n| mir_last_use_moves_slot(n, *slot, ctx))
        })
    {
        return None;
    }
    let fast = roots
        .iter()
        .map(|r| fast(r, &ahead, ctx))
        .collect::<Option<Vec<_>>>()?;
    HOISTED.with(|h| {
        let mut h = h.borrow_mut();
        h.extend(
            ahead
                .exact
                .iter()
                .map(|(k, code)| (*k, (code.clone(), false))),
        )
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
        None => (
            "i64",
            fast[0].clone(),
            "aver_rt::AverInt::from_inline_word(__word)",
        ),
        Some("") => ("i64", fast[0].clone(), "(__word >> 1)"),
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
            tagged(&name),
            format!("aver_rt::AverInt::from_i64({name})"),
        ),
        None if mir_expr_is_bare_i64(e, ctx) => (
            format!("let {name}: i64 = {};", emit_mir_expr(e, ctx)?),
            tagged(&name),
            name.clone(),
        ),
        None => (
            format!("let {name} = &({});", emit_mir_expr(e, ctx)?),
            format!("{name}.inline_word()?"),
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

/// The word `2n + 1` of a raw `i64` `n`, leaving the closure past the
/// inline range.
fn tagged(raw: &str) -> String {
    format!("(({raw}).checked_mul(2)? | 1)")
}

/// The word of `e` as `AverInt` holds an inline value, `2n + 1`, leaving
/// the closure (`?`) on a heap integer or a result past the inline range.
/// Each operation is the one `AverInt`'s fast path does on the words; the
/// tree only skips the tests of the words in between.
fn fast(e: Node<'_>, ahead: &Ahead<'_>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    let Some((op, a, b)) = op_of(e, ctx) else {
        if let Some(n) = int_literal(&e.node) {
            let inline = -(1i64 << 62)..(1i64 << 62);
            return inline.contains(&n).then(|| format!("({}i64)", 2 * n + 1));
        }
        if let Some(read) = ahead.fast.get(&key(e)) {
            return Some(read.clone());
        }
        return match &e.node {
            MirExpr::Box(inner) => Some(tagged(&emit_mir_expr(inner, ctx)?)),
            _ if mir_expr_is_bare_i64(e, ctx) => Some(tagged(&emit_mir_expr(e, ctx)?)),
            _ => Some(format!("({}).inline_word()?", emit_mir_expr(e, ctx)?)),
        };
    };
    let x = fast(a, ahead, ctx)?;
    let y = match b {
        Some(b) => fast(b, ahead, ctx)?,
        None => String::new(),
    };
    // The value of a word, and the word of a value that is inline.
    let value = |x: &str| format!("(({x}) >> 1)");
    let word = |v: String| format!("(({v} << 1) | 1)");
    Some(match op {
        // `2a+1 + 2b` is `2(a+b)+1`; an overflow of the word is a result
        // past the inline range.
        IntOp::Add => format!("({x}).checked_add(({y}) ^ 1)?"),
        IntOp::Sub => format!("({x}).checked_sub(({y}) ^ 1)?"),
        IntOp::Mul => format!("({}.checked_mul(({y}) ^ 1)? | 1)", value(&x)),
        IntOp::Neg | IntOp::Div(-1) => format!("(2i64).checked_sub({x})?"),
        IntOp::Abs => format!(
            "({{ let __w = {x}; if __w < 0 {{ (2i64).checked_sub(__w)? }} else {{ __w }} }})"
        ),
        // The words order, `and` and `or` as the values do.
        IntOp::And => format!("(({x}) & ({y}))"),
        IntOp::Or => format!("(({x}) | ({y}))"),
        IntOp::Xor => format!("((({x}) ^ ({y})) | 1)"),
        IntOp::Not => format!("((!({x})) | 1)"),
        IntOp::Min => format!("({x}).min({y})"),
        IntOp::Max => format!("({x}).max({y})"),
        IntOp::Div(1) => x,
        // An inline value divided by `|k| > 1`, or its remainder by a
        // divisor within the inline range, is inline.
        IntOp::Div(k) => word(format!("{}.div_euclid({k}i64)", value(&x))),
        IntOp::Mod(k) if k.unsigned_abs() <= 1 << 62 => {
            word(format!("{}.rem_euclid({k}i64)", value(&x)))
        }
        IntOp::Shl(n) if n <= 62 => format!("((({x}) ^ 1).checked_mul({}i64)? | 1)", 1i64 << n),
        IntOp::Shr(n) => format!("((({x}) >> {}) | 1)", n.min(63)),
        // The low `w + 1` bits of the word are the word of the low `w` bits.
        IntOp::Low(w) if w <= 62 => {
            format!("(({x}) & {}i64)", ((1i128 << (w + 1)) - 1) as i64)
        }
        // Past the inline range: kept exact, the whole tree with it.
        IntOp::Mod(_) | IntOp::Shl(_) | IntOp::Low(_) => return None,
    })
}
