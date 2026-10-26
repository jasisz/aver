//! Pure `Int` expression trees computed on a machine word first.
//!
//! On wasm-gc every `Int` is an `$AverInt` struct, so each `+`, `*`,
//! `Int.mod`, `Bits.and`, … of a tree allocates a fresh one — although the
//! values almost always fit an `i64`. A tree of pure, total `Int` operations
//! is therefore emitted twice:
//!
//! 1. **on `i64`** — each leaf is read as an `i64` (a literal, a raw value,
//!    or the `$small` of a Small `$AverInt`), each operation is the machine
//!    operation plus the overflow test `builtins/bignum.rs` uses on its own
//!    fast path. A Big leaf or an overflow leaves this path;
//! 2. **exactly** — the tree's ordinary lowering, unchanged, which runs when
//!    the first path is left.
//!
//! The first path's result is boxed once, or not at all when the tree is
//! compared or narrowed to `i64`. Leaves that are not plain reads (calls,
//! matches, …) are evaluated once, in source order, before either path, and
//! both paths read them from a local. The operations in between are pure and
//! total, so running them after every leaf rather than interleaved with the
//! leaves is unobservable, and computing them a second time on the exact
//! path is too.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use wasm_encoder::BlockType;

use super::*;
use crate::codegen::wasm_gc::types::struct_ref;
use crate::ir::hir::BuiltinIntrinsic;

thread_local! {
    /// Exact-path emissions in progress. While non-zero no tree is computed
    /// on `i64`: the exact path is the ordinary lowering, byte for byte.
    static EXACT: Cell<u32> = const { Cell::new(0) };
    /// Leaves evaluated ahead of a tree, by node address: the local holding
    /// the value, and whether that local is a raw `i64` the exact path boxes.
    static HOISTED: RefCell<HashMap<usize, (u32, bool)>> = RefCell::new(HashMap::new());
    /// Whether the fn being emitted may compute trees on `i64` at all.
    static ALLOWED: Cell<bool> = const { Cell::new(true) };
}

/// Allow or forbid trees for the fn about to be emitted.
pub(crate) fn set_allowed(allowed: bool) {
    ALLOWED.with(|a| a.set(allowed));
}

/// The operations a tree is made of. Every one is pure and total on ℤ.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    /// Euclidean division by a nonzero literal.
    Div(i64),
    /// Euclidean remainder by a nonzero literal.
    Mod(i64),
    /// `Bits.shiftLeft` by a literal count.
    Shl(i64),
    /// `Bits.shiftRight` by a literal count.
    Shr(i64),
    /// `Bits.low` with a literal width.
    Low(i64),
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

/// The operation `e` is, with its `Int` operands, when `e` is one of the
/// tree operations on boxed `Int`s. Raw-rendering nodes (the `bare_i64`
/// rewrite already computes them on `i64`) are not.
fn op_of<'e>(
    e: &'e Spanned<MirExpr>,
    ctx: &EmitCtx<'_>,
) -> Option<(Op, &'e Spanned<MirExpr>, Option<&'e Spanned<MirExpr>>)> {
    match &e.node {
        MirExpr::BinOp(b) => {
            let bop = &b.node;
            let op = match bop.op {
                BinOp::Add => Op::Add,
                BinOp::Sub => Op::Sub,
                BinOp::Mul => Op::Mul,
                _ => return None,
            };
            if !is_int(&bop.lhs) || mir_int_binop_is_raw(bop, ctx) || mir_renders_raw_i64(&e.node, ctx)
            {
                return None;
            }
            Some((op, &bop.lhs, Some(&bop.rhs)))
        }
        MirExpr::Neg(inner) => {
            if !is_int(inner) || mir_renders_raw_i64(&inner.node, ctx) {
                return None;
            }
            Some((Op::Neg, inner, None))
        }
        MirExpr::Call(c) => {
            let call = &c.node;
            if !is_int(e) || call.args.iter().any(|a| !is_int(a)) {
                return None;
            }
            match &call.callee {
                MirCallee::Builtin(id) => {
                    let dotted = ctx.mir_builtins?.get(id.0 as usize)?.as_str();
                    let op = match (dotted, call.args.len()) {
                        ("Int.min", 2) => Op::Min,
                        ("Int.max", 2) => Op::Max,
                        ("Int.abs", 1) => Op::Abs,
                        ("Bits.and", 2) => Op::And,
                        ("Bits.or", 2) => Op::Or,
                        ("Bits.xor", 2) => Op::Xor,
                        ("Bits.not", 1) => Op::Not,
                        _ => return None,
                    };
                    Some((op, &call.args[0], call.args.get(1)))
                }
                MirCallee::Intrinsic(intr) if call.args.len() == 2 => {
                    let k = int_literal(&call.args[1])?;
                    let op = match intr {
                        BuiltinIntrinsic::IntDivEuclid if k != 0 => Op::Div(k),
                        BuiltinIntrinsic::IntModEuclid if k != 0 => Op::Mod(k),
                        BuiltinIntrinsic::BitsShiftLeft if k >= 0 => Op::Shl(k),
                        BuiltinIntrinsic::BitsShiftRight if k >= 0 => Op::Shr(k),
                        BuiltinIntrinsic::BitsLow if k >= 0 => Op::Low(k),
                        _ => return None,
                    };
                    Some((op, &call.args[0], None))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// A read with no effect, no trap and no allocation the fast path needs:
/// emitting it again, or not at all, is unobservable.
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

/// How a leaf of a tree is read on the `i64` path.
enum Leaf<'e> {
    Lit(i64),
    /// A local holding an `$AverInt`: its `$small` when Small.
    BoxedLocal(u32),
    /// A pure expression rendering a raw `i64`.
    Raw(&'e Spanned<MirExpr>),
    /// A pure eligible-carrier `.value` read: its native `i64` storage.
    CarrierRaw(&'e Spanned<MirExpr>),
    /// A pure expression rendering an `$AverInt` (a record field read).
    BoxedPure(&'e Spanned<MirExpr>),
    /// Anything else, evaluated ahead of the tree into a local.
    Hoist(&'e Spanned<MirExpr>, Hoisted),
}

/// How a hoisted leaf is evaluated and kept.
#[derive(Clone, Copy)]
enum Hoisted {
    /// An `$AverInt`, through the ordinary lowering.
    Boxed,
    /// A raw `i64` (the inside of a `Box`), boxed for the exact path.
    RawBox,
    /// An eligible carrier's native `i64` storage, boxed for the exact path.
    CarrierBox,
}

fn leaf_of<'e>(e: &'e Spanned<MirExpr>, slots: &SlotTable, ctx: &EmitCtx<'_>) -> Leaf<'e> {
    match &e.node {
        MirExpr::Literal(l) => {
            if let Literal::Int(n) = l.node {
                return Leaf::Lit(n);
            }
            Leaf::Hoist(e, Hoisted::Boxed)
        }
        MirExpr::Local(local) if !ctx.slot_is_bare(local.node.slot.0) => {
            Leaf::BoxedLocal(slots.local(local.node.slot.0))
        }
        MirExpr::Box(inner) if mir_renders_raw_i64(&inner.node, ctx) => {
            if pure_read(&inner.node) {
                Leaf::Raw(inner)
            } else {
                Leaf::Hoist(e, Hoisted::RawBox)
            }
        }
        MirExpr::Project(_) if mir_is_eligible_carrier_value_project(&e.node, ctx) => {
            if pure_read(&e.node) {
                Leaf::CarrierRaw(e)
            } else {
                Leaf::Hoist(e, Hoisted::CarrierBox)
            }
        }
        MirExpr::Project(_) if pure_read(&e.node) && !mir_renders_raw_i64(&e.node, ctx) => {
            Leaf::BoxedPure(e)
        }
        _ => Leaf::Hoist(e, Hoisted::Boxed),
    }
}

/// The number of operations of the tree rooted at `e`, or `None` when a
/// leaf renders a raw `i64` where the tree expects an `$AverInt` (the
/// `bare_i64` rewrite boxes those, so it does not happen; refuse anyway).
fn weight(e: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> Option<usize> {
    match op_of(e, ctx) {
        Some((_, a, b)) => {
            let wb = match b {
                Some(b) => weight(b, ctx)?,
                None => 0,
            };
            Some(1 + weight(a, ctx)? + wb)
        }
        None if mir_renders_raw_i64(&e.node, ctx) => None,
        None => Some(0),
    }
}

/// Whether `e` is a tree worth the two paths: at least one operation.
fn is_tree(e: &Spanned<MirExpr>, ctx: &EmitCtx<'_>) -> bool {
    weight(e, ctx).is_some_and(|w| w > 0)
}

fn enabled(ctx: &EmitCtx<'_>) -> bool {
    ctx.registry.bignum
        && ctx.registry.aint_struct_idx.is_some()
        && EXACT.with(|d| d.get() == 0)
        && ALLOWED.with(|a| a.get())
        && std::env::var_os("AVER_NO_INT_TREE").is_none()
}

/// The value of a hoisted leaf, when `expr` is one: a `local.get`, boxed
/// when the local holds a raw `i64`. Read at the top of `emit_mir_expr`, so
/// the exact path of a tree reads its hoisted leaves instead of evaluating
/// them a second time.
pub(crate) fn emit_hoisted_leaf(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let found = HOISTED.with(|h| {
        let h = h.borrow();
        if h.is_empty() {
            None
        } else {
            h.get(&(expr as *const _ as usize)).copied()
        }
    });
    let Some((local, raw)) = found else {
        return Ok(None);
    };
    func.instruction(&Instruction::LocalGet(local));
    if raw {
        lift_i64_result_to_aint(func, ctx)?;
    }
    Ok(Some(true))
}

/// What the tree's value becomes.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Root {
    /// An `$AverInt`, boxed once.
    Boxed,
    /// A raw `i64` (the inside of an `Unbox`).
    Raw,
}

/// A boxed `Int` tree rooted at `expr`: emitted here when it is one.
pub(crate) fn try_emit_boxed(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<bool>>, WasmGcError> {
    if !enabled(ctx) || !is_tree(expr, ctx) {
        return Ok(None);
    }
    let r = emit_tree(func, &[expr], None, Root::Boxed, slots, ctx, &mut |f| {
        emit_mir_expr(f, expr, slots, ctx).map(|o| o.map(|_| ()))
    })?;
    Ok(Some(r.map(|()| true)))
}

/// `Unbox(inner)` over a tree: the tree's `i64` without boxing it.
pub(crate) fn try_emit_unboxed(
    func: &mut Function,
    unbox: &Spanned<MirExpr>,
    inner: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<bool>>, WasmGcError> {
    if !enabled(ctx) || !is_tree(inner, ctx) {
        return Ok(None);
    }
    let r = emit_tree(func, &[inner], None, Root::Raw, slots, ctx, &mut |f| {
        emit_mir_expr(f, unbox, slots, ctx).map(|o| o.map(|_| ()))
    })?;
    Ok(Some(r.map(|()| true)))
}

/// A comparison of two boxed `Int`s at least one of which is a tree: the
/// machine comparison of the two `i64`s, with no box at all.
pub(crate) fn try_emit_compare(
    func: &mut Function,
    bop: &crate::ir::mir::MirBinOp,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<()>>, WasmGcError> {
    let cmp = match bop.op {
        BinOp::Eq => Instruction::I64Eq,
        BinOp::Neq => Instruction::I64Ne,
        BinOp::Lt => Instruction::I64LtS,
        BinOp::Gt => Instruction::I64GtS,
        BinOp::Lte => Instruction::I64LeS,
        BinOp::Gte => Instruction::I64GeS,
        _ => return Ok(None),
    };
    if !enabled(ctx)
        || !is_int(&bop.lhs)
        || mir_int_binop_is_raw(bop, ctx)
        || !matches!(
            (weight(&bop.lhs, ctx), weight(&bop.rhs, ctx)),
            (Some(l), Some(r)) if l + r > 0
        )
    {
        return Ok(None);
    }
    emit_tree(
        func,
        &[&bop.lhs, &bop.rhs],
        Some(cmp),
        Root::Raw,
        slots,
        ctx,
        &mut |f| emit_mir_numeric_binop(f, bop, slots, ctx),
    )
    .map(Some)
}

/// The two paths around the operands `roots` (one tree, or the two sides of
/// a comparison `cmp`). `exact` emits the ordinary lowering of the whole
/// expression; it runs with every hoisted leaf read from its local.
fn emit_tree(
    func: &mut Function,
    roots: &[&Spanned<MirExpr>],
    cmp: Option<Instruction<'static>>,
    root: Root,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    exact: &mut dyn FnMut(&mut Function) -> Result<Option<()>, WasmGcError>,
) -> Result<Option<()>, WasmGcError> {
    // Leaves that are not plain reads, evaluated once, in source order.
    let mut hoisted: Vec<(usize, u32, bool)> = Vec::new();
    for r in roots {
        if hoist_leaves(func, r, slots, ctx, &mut hoisted)?.is_none() {
            return Ok(None);
        }
    }
    let result_ty = if cmp.is_some() {
        ValType::I32
    } else if root == Root::Raw {
        ValType::I64
    } else {
        struct_ref(ctx.registry.aint_struct_idx.ok_or(WasmGcError::Validation(
            "Int tree needs the $AverInt struct slot".into(),
        ))?)
    };
    let temps = slots.int_tree_temps(struct_ref(ctx.registry.aint_struct_idx.unwrap_or(0)));
    let hmap: HashMap<usize, (u32, bool)> =
        hoisted.iter().map(|&(a, l, raw)| (a, (l, raw))).collect();
    func.instruction(&Instruction::Block(BlockType::Result(result_ty)));
    func.instruction(&Instruction::Block(BlockType::Empty));
    for r in roots {
        if emit_fast(func, r, &hmap, temps, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    if let Some(cmp) = cmp {
        func.instruction(&cmp);
    } else if root == Root::Boxed {
        lift_i64_result_to_aint(func, ctx)?;
    }
    func.instruction(&Instruction::Br(1));
    func.instruction(&Instruction::End);
    // The exact path, reading the hoisted leaves.
    HOISTED.with(|h| h.borrow_mut().extend(hmap.iter().map(|(a, v)| (*a, *v))));
    EXACT.with(|d| d.set(d.get() + 1));
    let r = exact(func);
    EXACT.with(|d| d.set(d.get() - 1));
    HOISTED.with(|h| {
        let mut h = h.borrow_mut();
        for a in hmap.keys() {
            h.remove(a);
        }
    });
    if r?.is_none() {
        return Ok(None);
    }
    func.instruction(&Instruction::End);
    Ok(Some(()))
}

fn hoist_leaves(
    func: &mut Function,
    e: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    out: &mut Vec<(usize, u32, bool)>,
) -> Result<Option<()>, WasmGcError> {
    if let Some((_, a, b)) = op_of(e, ctx) {
        if hoist_leaves(func, a, slots, ctx, out)?.is_none() {
            return Ok(None);
        }
        if let Some(b) = b {
            return hoist_leaves(func, b, slots, ctx, out);
        }
        return Ok(Some(()));
    }
    let Leaf::Hoist(leaf, how) = leaf_of(e, slots, ctx) else {
        return Ok(Some(()));
    };
    let (produced, ty, raw) = match how {
        Hoisted::Boxed => (
            emit_mir_expr(func, leaf, slots, ctx)?,
            struct_ref(ctx.registry.aint_struct_idx.unwrap_or(0)),
            false,
        ),
        Hoisted::RawBox => {
            let MirExpr::Box(inner) = &leaf.node else {
                return Ok(None);
            };
            (emit_mir_int_raw(func, inner, slots, ctx)?, ValType::I64, true)
        }
        Hoisted::CarrierBox => (
            emit_mir_carrier_value_raw(func, leaf, slots, ctx)?,
            ValType::I64,
            true,
        ),
    };
    if produced.is_none() {
        return Ok(None);
    }
    let local = slots.fresh_local(ty);
    func.instruction(&Instruction::LocalSet(local));
    out.push((leaf as *const _ as usize, local, raw));
    Ok(Some(()))
}

/// Leave the `i64` of a Small `$AverInt` local, or leave the fast path.
fn small_of(func: &mut Function, local: u32, ctx: &EmitCtx<'_>) -> Result<(), WasmGcError> {
    let aint = ctx.registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "Int tree needs the $AverInt struct slot".into(),
    ))?;
    func.instruction(&Instruction::LocalGet(local));
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint,
        field_index: 1,
    });
    func.instruction(&Instruction::RefIsNull);
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(0));
    func.instruction(&Instruction::LocalGet(local));
    func.instruction(&Instruction::StructGet {
        struct_type_index: aint,
        field_index: 0,
    });
    Ok(())
}

/// Leave the fast path unless the `i64` on the stack lies in `[lo, hi]`;
/// keeps it in `t` and on the stack.
fn in_range(func: &mut Function, lo: i128, hi: i128, t: u32) {
    let lo = lo.max(i64::MIN as i128);
    let hi = hi.min(i64::MAX as i128);
    func.instruction(&Instruction::LocalTee(t));
    if lo > hi {
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::LocalGet(t));
        return;
    }
    match (lo == i64::MIN as i128, hi == i64::MAX as i128) {
        (true, true) => return,
        (true, false) => {
            func.instruction(&Instruction::I64Const(hi as i64));
            func.instruction(&Instruction::I64GtS);
        }
        (false, true) => {
            func.instruction(&Instruction::I64Const(lo as i64));
            func.instruction(&Instruction::I64LtS);
        }
        (false, false) => {
            func.instruction(&Instruction::I64Const(lo as i64));
            func.instruction(&Instruction::I64Sub);
            func.instruction(&Instruction::I64Const((hi - lo) as i64));
            func.instruction(&Instruction::I64GtU);
        }
    }
    func.instruction(&Instruction::BrIf(0));
    func.instruction(&Instruction::LocalGet(t));
}

fn div_floor(a: i128, b: i128) -> i128 {
    let q = a / b;
    if (a % b != 0) && ((a < 0) != (b < 0)) {
        q - 1
    } else {
        q
    }
}

fn div_ceil(a: i128, b: i128) -> i128 {
    -div_floor(-a, b)
}

/// The `x` for which `x * k` fits an `i64`.
fn mul_range(k: i128) -> (i128, i128) {
    let (min, max) = (i64::MIN as i128, i64::MAX as i128);
    match k.cmp(&0) {
        std::cmp::Ordering::Equal => (min, max),
        std::cmp::Ordering::Greater => (div_ceil(min, k), div_floor(max, k)),
        std::cmp::Ordering::Less => (div_ceil(max, k), div_floor(min, k)),
    }
}

/// Emit the `i64` of `e` (an operation or a leaf), leaving the fast path
/// (`br 0`) on a Big leaf or an overflow. `temps` are three `i64` locals and
/// one `$AverInt` local, live only inside one operation.
fn emit_fast(
    func: &mut Function,
    e: &Spanned<MirExpr>,
    hoisted: &HashMap<usize, (u32, bool)>,
    temps: [u32; 4],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let [t0, t1, t2, ta] = temps;
    let Some((op, a, b)) = op_of(e, ctx) else {
        return emit_fast_leaf(func, e, hoisted, ta, slots, ctx);
    };
    macro_rules! fast {
        ($x:expr) => {
            if emit_fast(func, $x, hoisted, temps, slots, ctx)?.is_none() {
                return Ok(None);
            }
        };
    }
    let lit_b = b.and_then(int_literal).map(i128::from);
    let lit_a = int_literal(a).map(i128::from);
    let (min, max) = (i64::MIN as i128, i64::MAX as i128);
    match op {
        Op::Add | Op::Sub | Op::Mul if lit_b.is_some() || lit_a.is_some() => {
            // One side is a literal `k`: the other must lie in the range where
            // the operation fits, one comparison.
            let (x, k, k_left) = match (lit_a, lit_b) {
                (_, Some(k)) => (a, k, false),
                (Some(k), None) => (b.expect("binary"), k, true),
                (None, None) => unreachable!(),
            };
            let (lo, hi) = match (op, k_left) {
                (Op::Add, _) => (min - k, max - k),
                (Op::Sub, false) => (min + k, max + k),
                (Op::Sub, true) => (k - max, k - min),
                _ => mul_range(k),
            };
            fast!(x);
            in_range(func, lo, hi, t0);
            func.instruction(&Instruction::Drop);
            if k_left {
                func.instruction(&Instruction::I64Const(k as i64));
                func.instruction(&Instruction::LocalGet(t0));
            } else {
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Const(k as i64));
            }
            func.instruction(&match op {
                Op::Add => Instruction::I64Add,
                Op::Sub => Instruction::I64Sub,
                _ => Instruction::I64Mul,
            });
        }
        Op::Add | Op::Sub => {
            fast!(a);
            fast!(b.expect("binary"));
            func.instruction(&Instruction::LocalSet(t1));
            func.instruction(&Instruction::LocalSet(t0));
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::LocalGet(t1));
            func.instruction(&if op == Op::Add {
                Instruction::I64Add
            } else {
                Instruction::I64Sub
            });
            func.instruction(&Instruction::LocalTee(t2));
            // ADD: (a^s) & (b^s) < 0;  SUB: (a^b) & (a^d) < 0.
            if op == Op::Add {
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Xor);
                func.instruction(&Instruction::LocalGet(t1));
                func.instruction(&Instruction::LocalGet(t2));
                func.instruction(&Instruction::I64Xor);
            } else {
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Xor);
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::LocalGet(t1));
                func.instruction(&Instruction::I64Xor);
            }
            func.instruction(&Instruction::I64And);
            func.instruction(&Instruction::I64Const(0));
            func.instruction(&Instruction::I64LtS);
            func.instruction(&Instruction::BrIf(0));
            func.instruction(&Instruction::LocalGet(t2));
        }
        Op::Mul => {
            fast!(a);
            fast!(b.expect("binary"));
            func.instruction(&Instruction::LocalSet(t1));
            func.instruction(&Instruction::LocalSet(t0));
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::LocalGet(t1));
            func.instruction(&Instruction::I64Mul);
            func.instruction(&Instruction::LocalSet(t2));
            // Overflow, as `bignum.rs` tests it: for a ∉ {-1, 0} the product
            // overflowed iff p / a != b; for a = -1 iff b = i64::MIN; never
            // for a = 0. The divisor is 1 for a ∈ {-1, 0}, so `div_s` cannot
            // trap.
            func.instruction(&Instruction::LocalGet(t2));
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Add);
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64GtU);
            func.instruction(&Instruction::Select);
            func.instruction(&Instruction::I64DivS);
            func.instruction(&Instruction::LocalGet(t1));
            func.instruction(&Instruction::I64Ne);
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::I64Const(-1));
            func.instruction(&Instruction::I64Eq);
            func.instruction(&Instruction::LocalGet(t1));
            func.instruction(&Instruction::I64Const(i64::MIN));
            func.instruction(&Instruction::I64Eq);
            func.instruction(&Instruction::I32And);
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64Add);
            func.instruction(&Instruction::I64Const(1));
            func.instruction(&Instruction::I64GtU);
            func.instruction(&Instruction::Select);
            func.instruction(&Instruction::BrIf(0));
            func.instruction(&Instruction::LocalGet(t2));
        }
        Op::Neg | Op::Abs => {
            fast!(a);
            in_range(func, min + 1, max, t0);
            if op == Op::Neg {
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I64Const(0));
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Sub);
            } else {
                // select(-x, x, x < 0)
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I64Const(0));
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Sub);
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Const(0));
                func.instruction(&Instruction::I64LtS);
                func.instruction(&Instruction::Select);
            }
        }
        Op::And | Op::Or | Op::Xor => {
            // An `i64` IS its infinite two's-complement sequence (the sign
            // extends), so the machine operation is exact.
            fast!(a);
            fast!(b.expect("binary"));
            func.instruction(&match op {
                Op::And => Instruction::I64And,
                Op::Or => Instruction::I64Or,
                _ => Instruction::I64Xor,
            });
        }
        Op::Not => {
            fast!(a);
            func.instruction(&Instruction::I64Const(-1));
            func.instruction(&Instruction::I64Xor);
        }
        Op::Min | Op::Max => {
            fast!(a);
            fast!(b.expect("binary"));
            func.instruction(&Instruction::LocalSet(t1));
            func.instruction(&Instruction::LocalSet(t0));
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::LocalGet(t1));
            func.instruction(&Instruction::LocalGet(t0));
            func.instruction(&Instruction::LocalGet(t1));
            func.instruction(&if op == Op::Min {
                Instruction::I64LeS
            } else {
                Instruction::I64GeS
            });
            func.instruction(&Instruction::Select);
        }
        Op::Div(k) => {
            fast!(a);
            if k == 1 {
            } else if k == -1 {
                in_range(func, min + 1, max, t0);
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I64Const(0));
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Sub);
            } else if k > 0 && (k as u64).is_power_of_two() {
                // Euclidean division by a positive divisor is floor
                // division: an arithmetic shift for a power of two.
                func.instruction(&Instruction::I64Const(i64::from(k.trailing_zeros())));
                func.instruction(&Instruction::I64ShrS);
            } else {
                // Truncated quotient, moved one step away from zero toward
                // the Euclidean one when the truncated remainder is negative.
                func.instruction(&Instruction::LocalTee(t0));
                func.instruction(&Instruction::I64Const(k));
                func.instruction(&Instruction::I64DivS);
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Const(k));
                func.instruction(&Instruction::I64RemS);
                func.instruction(&Instruction::I64Const(63));
                func.instruction(&Instruction::I64ShrS);
                func.instruction(&if k > 0 {
                    Instruction::I64Add
                } else {
                    Instruction::I64Sub
                });
            }
        }
        Op::Mod(k) => {
            fast!(a);
            let m = k.unsigned_abs();
            if m == 1 {
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I64Const(0));
            } else if m.is_power_of_two() {
                // The Euclidean remainder is in `[0, |k|)`: the low bits.
                func.instruction(&Instruction::I64Const((m - 1) as i64));
                func.instruction(&Instruction::I64And);
            } else {
                // r + (|k| if r < 0), r the truncated remainder.
                func.instruction(&Instruction::I64Const(k));
                func.instruction(&Instruction::I64RemS);
                func.instruction(&Instruction::LocalTee(t0));
                func.instruction(&Instruction::LocalGet(t0));
                func.instruction(&Instruction::I64Const(63));
                func.instruction(&Instruction::I64ShrS);
                func.instruction(&Instruction::I64Const(m as i64));
                func.instruction(&Instruction::I64And);
                func.instruction(&Instruction::I64Add);
            }
        }
        Op::Shl(n) => {
            fast!(a);
            if n > 62 {
                in_range(func, 0, 0, t0);
            } else {
                in_range(func, min >> n, max >> n, t0);
                func.instruction(&Instruction::I64Const(n));
                func.instruction(&Instruction::I64Shl);
            }
        }
        Op::Shr(n) => {
            fast!(a);
            func.instruction(&Instruction::I64Const(n.min(63)));
            func.instruction(&Instruction::I64ShrS);
        }
        Op::Low(w) => {
            fast!(a);
            if w == 0 {
                func.instruction(&Instruction::Drop);
                func.instruction(&Instruction::I64Const(0));
            } else if w <= 63 {
                func.instruction(&Instruction::I64Const(((1u64 << w) - 1) as i64));
                func.instruction(&Instruction::I64And);
            } else {
                // A negative value's low bits past 63 do not fit.
                in_range(func, 0, max, t0);
            }
        }
    }
    Ok(Some(()))
}

fn emit_fast_leaf(
    func: &mut Function,
    e: &Spanned<MirExpr>,
    hoisted: &HashMap<usize, (u32, bool)>,
    ta: u32,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    match leaf_of(e, slots, ctx) {
        Leaf::Lit(n) => {
            func.instruction(&Instruction::I64Const(n));
        }
        Leaf::BoxedLocal(local) => small_of(func, local, ctx)?,
        Leaf::Raw(inner) => {
            if emit_mir_int_raw(func, inner, slots, ctx)?.is_none() {
                return Ok(None);
            }
        }
        Leaf::CarrierRaw(p) => {
            if emit_mir_carrier_value_raw(func, p, slots, ctx)?.is_none() {
                return Ok(None);
            }
        }
        Leaf::BoxedPure(p) => {
            if emit_mir_expr(func, p, slots, ctx)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::LocalSet(ta));
            small_of(func, ta, ctx)?;
        }
        Leaf::Hoist(leaf, _) => {
            let Some(&(local, raw)) = hoisted.get(&(leaf as *const _ as usize)) else {
                return Err(WasmGcError::Validation(
                    "Int tree leaf was not evaluated ahead of the tree".into(),
                ));
            };
            if raw {
                func.instruction(&Instruction::LocalGet(local));
            } else {
                small_of(func, local, ctx)?;
            }
        }
    }
    Ok(Some(()))
}
