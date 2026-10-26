//! Pure `Int` expression trees computed on a machine word first.
//!
//! Every `Int` on wasm-gc is an `$AverInt` struct, so each `+`, `*`,
//! `Int.mod`, `Bits.and`, … of an expression allocates a new one, although
//! the values almost always fit an `i64`. A tree of pure, total `Int`
//! operations is therefore emitted twice:
//!
//! 1. on `i64`: a leaf is a literal, a raw `i64`, or the `$small` of a Small
//!    `$AverInt`; an operation is the machine operation plus the overflow
//!    test `builtins/bignum.rs` uses on its own fast path. A Big leaf or an
//!    overflow leaves this path for the next one;
//! 2. exactly: the tree's ordinary lowering, unchanged.
//!
//! The first path boxes its result once, or not at all when the tree is
//! compared or narrowed to `i64`. A leaf that is not a plain read (a call, a
//! match, …) is evaluated once, in source order, before both paths, and both
//! read it from a local. The operations between the leaves are pure and
//! total, so computing them after all the leaves, or a second time on the
//! exact path, cannot be observed.
//!
//! A fn the certificate plan printer admits keeps the ordinary lowering
//! (`module.rs` decides, see [`set_allowed`]).

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use wasm_encoder::BlockType;
use wasm_encoder::Instruction as I;

use super::*;
use crate::codegen::wasm_gc::types::struct_ref;
use crate::ir::hir::BuiltinIntrinsic;

thread_local! {
    /// Exact paths being emitted: while non-zero no tree is computed on `i64`.
    static EXACT: Cell<u32> = const { Cell::new(0) };
    /// Leaves evaluated ahead of a tree, by node address: their local and
    /// whether it holds a raw `i64`, which the exact path boxes.
    static HOISTED: RefCell<HashMap<usize, (u32, bool)>> = RefCell::new(HashMap::new());
    /// Whether the fn being emitted may compute trees on `i64`.
    static ALLOWED: Cell<bool> = const { Cell::new(true) };
}

/// Allow or forbid trees in the fn about to be emitted.
pub(crate) fn set_allowed(allowed: bool) {
    ALLOWED.with(|a| a.set(allowed));
}

/// The tree operations. Every one is pure and total on ℤ; `Div`/`Mod` are by
/// a nonzero literal, the `Bits` shifts and `low` by a literal count.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Op {
    Add,
    Sub,
    Mul,
    Neg,
    And,
    Or,
    Xor,
    Not,
    Min,
    Max,
    Abs,
    Div(i64),
    Mod(i64),
    Shl(i64),
    Shr(i64),
    Low(i64),
}

type Node<'e> = &'e Spanned<MirExpr>;

fn seq(func: &mut Function, xs: &[I<'_>]) {
    for x in xs {
        func.instruction(x);
    }
}

fn int_literal(e: &Spanned<MirExpr>) -> Option<i64> {
    match &e.node {
        MirExpr::Literal(l) => match l.node {
            Literal::Int(n) => Some(n),
            _ => None,
        },
        _ => None,
    }
}

fn is_int(e: &Spanned<MirExpr>) -> bool {
    matches!(e.ty(), Some(Type::Int))
}

/// The operation `e` is, with its operands, when `e` is a tree operation on
/// boxed `Int`s. A node the `bare_i64` rewrite already renders raw is not.
fn op_of<'e>(e: Node<'e>, ctx: &EmitCtx<'_>) -> Option<(Op, Node<'e>, Option<Node<'e>>)> {
    match &e.node {
        MirExpr::BinOp(b) => {
            let op = match b.node.op {
                BinOp::Add => Op::Add,
                BinOp::Sub => Op::Sub,
                BinOp::Mul => Op::Mul,
                _ => return None,
            };
            let raw = mir_int_binop_is_raw(&b.node, ctx) || mir_renders_raw_i64(&e.node, ctx);
            (is_int(&b.node.lhs) && !raw).then_some((op, &b.node.lhs, Some(&b.node.rhs)))
        }
        MirExpr::Neg(inner) => (is_int(inner) && !mir_renders_raw_i64(&inner.node, ctx))
            .then_some((Op::Neg, &**inner, None)),
        MirExpr::Call(c) if is_int(e) && c.node.args.iter().all(is_int) => {
            let args = &c.node.args;
            let op = match (&c.node.callee, args.len()) {
                (MirCallee::Builtin(id), n) => {
                    match (ctx.mir_builtins?.get(id.0 as usize)?.as_str(), n) {
                        ("Int.min", 2) => Op::Min,
                        ("Int.max", 2) => Op::Max,
                        ("Int.abs", 1) => Op::Abs,
                        ("Bits.and", 2) => Op::And,
                        ("Bits.or", 2) => Op::Or,
                        ("Bits.xor", 2) => Op::Xor,
                        ("Bits.not", 1) => Op::Not,
                        _ => return None,
                    }
                }
                (MirCallee::Intrinsic(intr), 2) => {
                    let k = int_literal(&args[1])?;
                    match intr {
                        BuiltinIntrinsic::IntDivEuclid if k != 0 => Op::Div(k),
                        BuiltinIntrinsic::IntModEuclid if k != 0 => Op::Mod(k),
                        BuiltinIntrinsic::BitsShiftLeft if k >= 0 => Op::Shl(k),
                        BuiltinIntrinsic::BitsShiftRight if k >= 0 => Op::Shr(k),
                        BuiltinIntrinsic::BitsLow if k >= 0 => Op::Low(k),
                        _ => return None,
                    }
                }
                _ => return None,
            };
            let second = matches!(op, Op::Min | Op::Max | Op::And | Op::Or | Op::Xor);
            Some((op, &args[0], second.then(|| &args[1])))
        }
        _ => None,
    }
}

/// A read with no effect, no trap and no allocation: reading it again, or
/// not at all, cannot be observed.
fn pure_read(e: &MirExpr) -> bool {
    match e {
        MirExpr::Local(_) | MirExpr::Literal(_) => true,
        MirExpr::Project(p) => pure_read(&p.node.base.node),
        MirExpr::Neg(inner) => pure_read(&inner.node),
        MirExpr::BinOp(b) => {
            matches!(b.node.op, BinOp::Add | BinOp::Sub | BinOp::Mul)
                && pure_read(&b.node.lhs.node)
                && pure_read(&b.node.rhs.node)
        }
        _ => false,
    }
}

/// How the `i64` path reads a leaf.
enum Leaf<'e> {
    Lit(i64),
    /// A local holding an `$AverInt`.
    Boxed(u32),
    /// A pure raw-`i64` expression.
    Raw(Node<'e>),
    /// A pure eligible-carrier `.value` read: its native `i64` storage.
    Carrier(Node<'e>),
    /// A pure `$AverInt` read (a record field).
    BoxedRead(Node<'e>),
    /// Anything else, evaluated ahead of the tree. The flag: its value is a
    /// raw `i64` (inside a `Box`, or a carrier's storage).
    Hoist(Node<'e>, bool),
}

fn leaf_of<'e>(e: Node<'e>, slots: &SlotTable, ctx: &EmitCtx<'_>) -> Leaf<'e> {
    match &e.node {
        MirExpr::Literal(l) if matches!(l.node, Literal::Int(_)) => {
            Leaf::Lit(int_literal(e).unwrap_or_default())
        }
        MirExpr::Local(local) if !ctx.slot_is_bare(local.node.slot.0) => {
            Leaf::Boxed(slots.local(local.node.slot.0))
        }
        MirExpr::Box(inner) if mir_renders_raw_i64(&inner.node, ctx) => {
            if pure_read(&inner.node) {
                Leaf::Raw(inner)
            } else {
                Leaf::Hoist(e, true)
            }
        }
        MirExpr::Project(_) if mir_is_eligible_carrier_value_project(&e.node, ctx) => {
            if pure_read(&e.node) {
                Leaf::Carrier(e)
            } else {
                Leaf::Hoist(e, true)
            }
        }
        MirExpr::Project(_) if pure_read(&e.node) => Leaf::BoxedRead(e),
        _ => Leaf::Hoist(e, false),
    }
}

/// The number of operations in the tree at `e`; `None` when a leaf renders a
/// raw `i64` where the tree wants an `$AverInt` (the rewrite boxes those).
fn weight(e: Node<'_>, ctx: &EmitCtx<'_>) -> Option<usize> {
    match op_of(e, ctx) {
        Some((_, a, b)) => Some(1 + weight(a, ctx)? + b.map_or(Some(0), |b| weight(b, ctx))?),
        None => (!mir_renders_raw_i64(&e.node, ctx)).then_some(0),
    }
}

fn enabled(ctx: &EmitCtx<'_>) -> bool {
    ctx.registry.bignum
        && ctx.registry.aint_struct_idx.is_some()
        && EXACT.with(|d| d.get() == 0)
        && ALLOWED.with(|a| a.get())
        && std::env::var_os("AVER_NO_INT_TREE").is_none()
}

/// A hoisted leaf read back on the exact path, instead of evaluated again.
pub(crate) fn emit_hoisted_leaf(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let key = expr as *const _ as usize;
    let Some((local, raw)) = HOISTED.with(|h| h.borrow().get(&key).copied()) else {
        return Ok(None);
    };
    func.instruction(&I::LocalGet(local));
    if raw {
        lift_i64_result_to_aint(func, ctx)?;
    }
    Ok(Some(true))
}

/// A boxed `Int` tree at `expr`, boxed once.
pub(crate) fn try_emit_boxed(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<bool>>, WasmGcError> {
    if !enabled(ctx) || !weight(expr, ctx).is_some_and(|w| w > 0) {
        return Ok(None);
    }
    let r = emit_tree(func, &[expr], None, true, slots, ctx, &mut |f| {
        emit_mir_expr(f, expr, slots, ctx).map(|o| o.map(drop))
    })?;
    Ok(Some(r.map(|()| true)))
}

/// `Unbox` of a tree: the tree's `i64`, never boxed.
pub(crate) fn try_emit_unboxed(
    func: &mut Function,
    unbox: &Spanned<MirExpr>,
    inner: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<bool>>, WasmGcError> {
    if !enabled(ctx) || !weight(inner, ctx).is_some_and(|w| w > 0) {
        return Ok(None);
    }
    let r = emit_tree(func, &[inner], None, false, slots, ctx, &mut |f| {
        emit_mir_expr(f, unbox, slots, ctx).map(|o| o.map(drop))
    })?;
    Ok(Some(r.map(|()| true)))
}

/// A comparison of two boxed `Int`s, one of them a tree: the machine
/// comparison, with no box at all.
pub(crate) fn try_emit_compare(
    func: &mut Function,
    bop: &crate::ir::mir::MirBinOp,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<()>>, WasmGcError> {
    let cmp = match bop.op {
        BinOp::Eq => I::I64Eq,
        BinOp::Neq => I::I64Ne,
        BinOp::Lt => I::I64LtS,
        BinOp::Gt => I::I64GtS,
        BinOp::Lte => I::I64LeS,
        BinOp::Gte => I::I64GeS,
        _ => return Ok(None),
    };
    let tree = matches!((weight(&bop.lhs, ctx), weight(&bop.rhs, ctx)),
        (Some(l), Some(r)) if l + r > 0);
    if !enabled(ctx) || !is_int(&bop.lhs) || mir_int_binop_is_raw(bop, ctx) || !tree {
        return Ok(None);
    }
    let exact = &mut |f: &mut Function| emit_mir_numeric_binop(f, bop, slots, ctx);
    emit_tree(func, &[&bop.lhs, &bop.rhs], Some(cmp), false, slots, ctx, exact).map(Some)
}

/// Both paths around `roots` (one tree, or the two sides of comparison
/// `cmp`). `boxed`: box the `i64` result. `exact` emits the ordinary lowering
/// of the whole expression.
fn emit_tree(
    func: &mut Function,
    roots: &[Node<'_>],
    cmp: Option<I<'static>>,
    boxed: bool,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    exact: &mut dyn FnMut(&mut Function) -> Result<Option<()>, WasmGcError>,
) -> Result<Option<()>, WasmGcError> {
    let aint = struct_ref(ctx.registry.aint_struct_idx.unwrap_or_default());
    let mut hoisted = HashMap::new();
    for r in roots {
        if hoist(func, r, aint, slots, ctx, &mut hoisted)?.is_none() {
            return Ok(None);
        }
    }
    let ty = match (&cmp, boxed) {
        (Some(_), _) => ValType::I32,
        (None, true) => aint,
        (None, false) => ValType::I64,
    };
    let temps = slots.int_tree_temps(aint);
    seq(func, &[I::Block(BlockType::Result(ty)), I::Block(BlockType::Empty)]);
    for r in roots {
        if emit_fast(func, r, &hoisted, temps, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    match cmp {
        Some(cmp) => {
            func.instruction(&cmp);
        }
        None if boxed => lift_i64_result_to_aint(func, ctx)?,
        None => {}
    }
    seq(func, &[I::Br(1), I::End]);
    HOISTED.with(|h| h.borrow_mut().extend(hoisted.iter().map(|(k, v)| (*k, *v))));
    EXACT.with(|d| d.set(d.get() + 1));
    let r = exact(func);
    EXACT.with(|d| d.set(d.get() - 1));
    HOISTED.with(|h| h.borrow_mut().retain(|k, _| !hoisted.contains_key(k)));
    if r?.is_none() {
        return Ok(None);
    }
    func.instruction(&I::End);
    Ok(Some(()))
}

/// Evaluate the leaves of `e` that are not plain reads, in source order.
fn hoist(
    func: &mut Function,
    e: Node<'_>,
    aint: ValType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    out: &mut HashMap<usize, (u32, bool)>,
) -> Result<Option<()>, WasmGcError> {
    if let Some((_, a, b)) = op_of(e, ctx) {
        for x in std::iter::once(a).chain(b) {
            if hoist(func, x, aint, slots, ctx, out)?.is_none() {
                return Ok(None);
            }
        }
        return Ok(Some(()));
    }
    let Leaf::Hoist(leaf, raw) = leaf_of(e, slots, ctx) else {
        return Ok(Some(()));
    };
    let produced = match &leaf.node {
        MirExpr::Box(inner) if raw => emit_mir_int_raw(func, inner, slots, ctx)?,
        MirExpr::Project(_) if raw => emit_mir_carrier_value_raw(func, leaf, slots, ctx)?,
        _ => emit_mir_expr(func, leaf, slots, ctx)?,
    };
    if produced.is_none() {
        return Ok(None);
    }
    let local = slots.fresh_local(if raw { ValType::I64 } else { aint });
    func.instruction(&I::LocalSet(local));
    out.insert(leaf as *const _ as usize, (local, raw));
    Ok(Some(()))
}

/// Push the `$small` of the Small `$AverInt` in `local`, or leave the `i64`
/// path when it is Big.
fn small_of(func: &mut Function, local: u32, ctx: &EmitCtx<'_>) {
    let aint = ctx.registry.aint_struct_idx.unwrap_or_default();
    let field = |field_index| I::StructGet {
        struct_type_index: aint,
        field_index,
    };
    seq(
        func,
        &[I::LocalGet(local), field(1), I::RefIsNull, I::I32Eqz, I::BrIf(0)],
    );
    seq(func, &[I::LocalGet(local), field(0)]);
}

/// Pop an `i64` into `t`, leaving the `i64` path unless it lies in `[lo, hi]`.
fn guard(func: &mut Function, lo: i128, hi: i128, t: u32) {
    let (lo, hi) = (lo.max(i64::MIN.into()), hi.min(i64::MAX.into()));
    func.instruction(&I::LocalSet(t));
    let test: &[I<'_>] = match (lo == i64::MIN.into(), hi == i64::MAX.into()) {
        _ if lo > hi => &[I::Br(0)],
        (true, true) => &[],
        (true, false) => &[I::LocalGet(t), I::I64Const(hi as i64), I::I64GtS, I::BrIf(0)],
        (false, true) => &[I::LocalGet(t), I::I64Const(lo as i64), I::I64LtS, I::BrIf(0)],
        (false, false) => &[
            I::LocalGet(t),
            I::I64Const(lo as i64),
            I::I64Sub,
            I::I64Const((hi - lo) as i64),
            I::I64GtU,
            I::BrIf(0),
        ],
    };
    seq(func, test);
}

/// The `x` for which `x * k` fits an `i64`.
fn mul_range(k: i128) -> (i128, i128) {
    let (min, max) = (i128::from(i64::MIN), i128::from(i64::MAX));
    let floor = |a: i128| {
        let q = a / k;
        if a % k != 0 && (a < 0) != (k < 0) { q - 1 } else { q }
    };
    match k.signum() {
        0 => (min, max),
        1 => (-floor(-min), floor(max)),
        _ => (-floor(-max), floor(min)),
    }
}

fn arith(op: Op) -> I<'static> {
    match op {
        Op::Add => I::I64Add,
        Op::Sub => I::I64Sub,
        _ => I::I64Mul,
    }
}

/// Push the `i64` of `e`, leaving the path (`br 0`) on a Big leaf or an
/// overflow. `temps`: three `i64` locals and one `$AverInt`, live only inside
/// one operation.
fn emit_fast(
    func: &mut Function,
    e: Node<'_>,
    hoisted: &HashMap<usize, (u32, bool)>,
    temps: [u32; 4],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let [t0, t1, t2, ta] = temps;
    let Some((op, a, b)) = op_of(e, ctx) else {
        return emit_fast_leaf(func, e, hoisted, ta, slots, ctx);
    };
    let (min, max) = (i128::from(i64::MIN), i128::from(i64::MAX));
    let lit = |x: Option<Node<'_>>| x.and_then(int_literal).map(i128::from);
    let fast = |func: &mut Function, x: Node<'_>| emit_fast(func, x, hoisted, temps, slots, ctx);
    let lit_b = lit(b);
    if matches!(op, Op::Add | Op::Sub | Op::Mul)
        && let Some((x, k, k_left)) = match (lit(Some(a)), lit_b) {
            (_, Some(k)) => Some((a, k, false)),
            (Some(k), None) => b.map(|b| (b, k, true)),
            _ => None,
        }
    {
        // One side is a literal `k`: the other must lie where the result fits.
        let (lo, hi) = match (op, k_left) {
            (Op::Add, _) => (min - k, max - k),
            (Op::Sub, false) => (min + k, max + k),
            (Op::Sub, true) => (k - max, k - min),
            _ => mul_range(k),
        };
        if fast(func, x)?.is_none() {
            return Ok(None);
        }
        guard(func, lo, hi, t0);
        let k = I::I64Const(k as i64);
        let (l, r) = if k_left { (k, I::LocalGet(t0)) } else { (I::LocalGet(t0), k) };
        seq(func, &[l, r, arith(op)]);
        return Ok(Some(()));
    }
    for x in std::iter::once(a).chain(b) {
        if fast(func, x)?.is_none() {
            return Ok(None);
        }
    }
    let (g0, g1, g2) = (I::LocalGet(t0), I::LocalGet(t1), I::LocalGet(t2));
    // a ∈ {-1, 0}, as `(a + 1) >u 1` is false.
    let general = [g0.clone(), I::I64Const(1), I::I64Add, I::I64Const(1), I::I64GtU];
    match op {
        Op::Add | Op::Sub | Op::Mul => {
            seq(func, &[I::LocalSet(t1), I::LocalSet(t0), g0.clone(), g1.clone()]);
            seq(func, &[arith(op), I::LocalSet(t2)]);
            if op == Op::Mul {
                // As `bignum.rs` tests it: for a ∉ {-1, 0} the product
                // overflowed iff p / a != b; for a = -1 iff b = i64::MIN; for
                // a = 0 never. The divisor is 1 for a ∈ {-1, 0}: no trap.
                seq(func, &[g2.clone(), g0.clone(), I::I64Const(1)]);
                seq(func, &general);
                seq(func, &[I::Select, I::I64DivS, g1.clone(), I::I64Ne]);
                seq(func, &[g0.clone(), I::I64Const(-1), I::I64Eq, g1, I::I64Const(i64::MIN)]);
                seq(func, &[I::I64Eq, I::I32And]);
                seq(func, &general);
                seq(func, &[I::Select]);
            } else {
                // Add: (s ^ a) & (s ^ b) < 0; sub: (a ^ b) & (a ^ d) < 0.
                let (w, x, y, z) = if op == Op::Add {
                    (g2.clone(), g0.clone(), g2.clone(), g1)
                } else {
                    (g0.clone(), g1, g0.clone(), g2.clone())
                };
                seq(func, &[w, x, I::I64Xor, y, z, I::I64Xor, I::I64And]);
                seq(func, &[I::I64Const(0), I::I64LtS]);
            }
            seq(func, &[I::BrIf(0), g2]);
        }
        Op::Neg | Op::Abs => {
            guard(func, min + 1, max, t0);
            seq(func, &[I::I64Const(0), g0.clone(), I::I64Sub]);
            if op == Op::Abs {
                seq(func, &[g0.clone(), g0, I::I64Const(0), I::I64LtS, I::Select]);
            }
        }
        // An `i64` IS its infinite two's-complement sequence: exact.
        Op::And => seq(func, &[I::I64And]),
        Op::Or => seq(func, &[I::I64Or]),
        Op::Xor => seq(func, &[I::I64Xor]),
        Op::Not => seq(func, &[I::I64Const(-1), I::I64Xor]),
        Op::Min | Op::Max => {
            let keep_a = if op == Op::Min { I::I64LeS } else { I::I64GeS };
            seq(func, &[I::LocalSet(t1), I::LocalSet(t0), g0.clone(), g1.clone()]);
            seq(func, &[g0, g1, keep_a, I::Select]);
        }
        Op::Div(1) => {}
        Op::Div(-1) => {
            guard(func, min + 1, max, t0);
            seq(func, &[I::I64Const(0), g0, I::I64Sub]);
        }
        // Euclidean division by a positive power of two is floor division.
        Op::Div(k) if k > 0 && k.count_ones() == 1 => {
            seq(func, &[I::I64Const(k.trailing_zeros().into()), I::I64ShrS]);
        }
        Op::Div(k) => {
            // The truncated quotient, one step further from zero when the
            // truncated remainder is negative.
            let step = if k > 0 { I::I64Add } else { I::I64Sub };
            seq(func, &[I::LocalTee(t0), I::I64Const(k), I::I64DivS, g0]);
            seq(func, &[I::I64Const(k), I::I64RemS, I::I64Const(63), I::I64ShrS, step]);
        }
        Op::Mod(k) if k.unsigned_abs() == 1 => seq(func, &[I::Drop, I::I64Const(0)]),
        // The Euclidean remainder by ±2^n is the low n bits.
        Op::Mod(k) if k.unsigned_abs().is_power_of_two() => {
            seq(func, &[I::I64Const((k.unsigned_abs() - 1) as i64), I::I64And]);
        }
        Op::Mod(k) => {
            // r + (|k| if r < 0), r the truncated remainder.
            let m = I::I64Const(k.unsigned_abs() as i64);
            seq(func, &[I::I64Const(k), I::I64RemS, I::LocalTee(t0), g0]);
            seq(func, &[I::I64Const(63), I::I64ShrS, m, I::I64And, I::I64Add]);
        }
        Op::Shl(n) if n > 62 => {
            guard(func, 0, 0, t0);
            func.instruction(&I::I64Const(0));
        }
        Op::Shl(n) => {
            guard(func, min >> n, max >> n, t0);
            seq(func, &[g0, I::I64Const(n), I::I64Shl]);
        }
        Op::Shr(n) => seq(func, &[I::I64Const(n.min(63)), I::I64ShrS]),
        Op::Low(0) => seq(func, &[I::Drop, I::I64Const(0)]),
        Op::Low(w) if w <= 63 => seq(func, &[I::I64Const(((1u64 << w) - 1) as i64), I::I64And]),
        // A negative value's low bits past 63 do not fit.
        Op::Low(_) => {
            guard(func, 0, max, t0);
            func.instruction(&g0);
        }
    }
    Ok(Some(()))
}

fn emit_fast_leaf(
    func: &mut Function,
    e: Node<'_>,
    hoisted: &HashMap<usize, (u32, bool)>,
    ta: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    match leaf_of(e, slots, ctx) {
        Leaf::Lit(n) => {
            func.instruction(&I::I64Const(n));
        }
        Leaf::Boxed(local) => small_of(func, local, ctx),
        Leaf::Raw(inner) => return Ok(emit_mir_int_raw(func, inner, slots, ctx)?.map(drop)),
        Leaf::Carrier(p) => return Ok(emit_mir_carrier_value_raw(func, p, slots, ctx)?.map(drop)),
        Leaf::BoxedRead(p) => {
            if emit_mir_expr(func, p, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&I::LocalSet(ta));
            small_of(func, ta, ctx);
        }
        Leaf::Hoist(leaf, _) => match hoisted.get(&(leaf as *const _ as usize)) {
            Some(&(local, true)) => {
                func.instruction(&I::LocalGet(local));
            }
            Some(&(local, false)) => small_of(func, local, ctx),
            None => {
                return Err(WasmGcError::Validation(
                    "an Int tree leaf was not evaluated ahead of its tree".into(),
                ));
            }
        },
    }
    Ok(Some(()))
}
