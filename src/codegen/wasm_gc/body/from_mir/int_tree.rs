//! Pure `Int` expression trees computed on a machine word first.
//!
//! Every `Int` on wasm-gc is an `$AverInt` struct, so each `+`, `*`,
//! `Int.mod`, `Bits.and`, … allocates one, although the values almost always
//! fit an `i64`. A tree of pure, total `Int` operations is emitted twice: on
//! `i64`, with the overflow tests of `builtins/bignum.rs`, and, for a Big leaf
//! or an overflow, its ordinary lowering, unchanged. The `i64` result is boxed
//! once, or not at all when compared or narrowed. Each leaf but a literal or a
//! local is evaluated once, in source order, ahead of both paths; the
//! operations are pure and total, so running them after the leaves, or twice,
//! cannot be observed.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use wasm_encoder::BlockType;
use wasm_encoder::Instruction as I;

use super::twin;
use super::*;
use crate::codegen::wasm_gc::twins::TwinKind;
use crate::codegen::wasm_gc::types::struct_ref;
use crate::ir::hir::BuiltinIntrinsic;

thread_local! {
    /// Exact paths being emitted: while non-zero no tree is computed on `i64`.
    static EXACT: Cell<u32> = const { Cell::new(0) };
    /// Leaves evaluated ahead of a tree, by node address, and where.
    static HOISTED: RefCell<HashMap<usize, Held>> = RefCell::new(HashMap::new());
    /// Whether the fn being emitted computes trees on `i64` (`module.rs`
    /// decides: not in a fn `kept_by_printer`).
    static ALLOWED: Cell<bool> = const { Cell::new(true) };
    /// The boxed `Int` literals emitted while recording (`module.rs` records
    /// each fn's in its pre-pass), and the global holding each one shared.
    static LITERALS: RefCell<Option<std::collections::BTreeSet<i64>>> = const { RefCell::new(None) };
    static POOL: RefCell<HashMap<i64, u32>> = RefCell::new(HashMap::new());
}

/// Whether the fn being emitted computes on the machine word at all.
pub(crate) fn allowed() -> bool {
    ALLOWED.with(Cell::get)
}

/// Whether an exact path is being emitted.
pub(crate) fn in_exact() -> bool {
    EXACT.with(|d| d.get() > 0)
}

pub(crate) fn set_allowed(allowed: bool) {
    ALLOWED.with(|a| a.set(allowed));
}

/// Back to no fn in particular when a module's emission ends, however it
/// ends: trees allowed, no literal pool, nothing recorded.
pub(crate) struct IntTreeReset;

impl Drop for IntTreeReset {
    fn drop(&mut self) {
        twin::set_calls(false);
        twin::set_self(None);
        set_allowed(true);
        set_literal_pool(HashMap::new());
        record_literals(false);
    }
}

/// Start recording the boxed `Int` literals emitted, or stop and return them.
pub(crate) fn record_literals(on: bool) -> std::collections::BTreeSet<i64> {
    LITERALS
        .with(|l| l.replace(on.then(Default::default)))
        .unwrap_or_default()
}

/// The globals holding the shared boxed literals, empty to stop sharing.
pub(crate) fn set_literal_pool(pool: HashMap<i64, u32>) {
    POOL.with(|p| *p.borrow_mut() = pool);
}

/// The global holding the boxed literal `n`, in a fn allowed to share it.
/// No `$AverInt` is ever written to after it is made, so one box serves
/// every use of the literal.
pub(crate) fn pooled_literal(n: i64) -> Option<u32> {
    LITERALS.with(|l| l.borrow_mut().as_mut().map(|set| set.insert(n)));
    ALLOWED
        .with(|a| a.get())
        .then(|| POOL.with(|p| p.borrow().get(&n).copied()))
        .flatten()
}

/// The tree operations, all pure and total on ℤ. `Div`/`Mod` divide by a
/// nonzero literal; `Shl`/`Shr`/`Low` take a literal count.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Op {
    Add,
    Sub,
    Mul,
    Neg,
    Abs,
    And,
    Or,
    Xor,
    Not,
    Min,
    Max,
    Div(i64),
    Mod(i64),
    Shl(i64),
    Shr(i64),
    Low(i64),
}

type Node<'e> = &'e Spanned<MirExpr>;

/// Where a leaf evaluated ahead is held: an `$AverInt`, a raw `i64` (the
/// exact path boxes it), or an `Int` twin's result `(n, r)`.
#[derive(Clone, Copy)]
enum Held {
    Boxed(u32),
    Raw(u32),
    Twin(u32, u32),
}

type Hoisted = HashMap<usize, Held>;

fn seq(func: &mut Function, xs: &[I<'_>]) {
    for x in xs {
        func.instruction(x);
    }
}

fn int_literal(e: Node<'_>) -> Option<i64> {
    let MirExpr::Literal(l) = &e.node else {
        return None;
    };
    if let Literal::Int(n) = l.node {
        Some(n)
    } else {
        None
    }
}

fn is_int(e: Node<'_>) -> bool {
    matches!(e.ty(), Some(Type::Int))
}

/// The operation `e` is, with its operands, when it is one on boxed `Int`s
/// (not one the `bare_i64` rewrite already renders raw).
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
        MirExpr::Neg(x) if is_int(x) && !mir_renders_raw_i64(&x.node, ctx) => {
            Some((Op::Neg, &**x, None))
        }
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
                (MirCallee::Intrinsic(intr), 2) => match (intr, int_literal(&args[1])?) {
                    (BuiltinIntrinsic::IntDivEuclid, k) if k != 0 => Op::Div(k),
                    (BuiltinIntrinsic::IntModEuclid, k) if k != 0 => Op::Mod(k),
                    (BuiltinIntrinsic::BitsShiftLeft, k) if k >= 0 => Op::Shl(k),
                    (BuiltinIntrinsic::BitsShiftRight, k) if k >= 0 => Op::Shr(k),
                    (BuiltinIntrinsic::BitsLow, k) if k >= 0 => Op::Low(k),
                    _ => return None,
                },
                _ => return None,
            };
            let binary = matches!(op, Op::Min | Op::Max | Op::And | Op::Or | Op::Xor);
            Some((op, &args[0], binary.then(|| &args[1])))
        }
        _ => None,
    }
}

/// How the `i64` path reads a leaf: a literal, a local holding an
/// `$AverInt`, or a value evaluated ahead (`true`: as a raw `i64`, the
/// inside of a `Box` or a carrier's machine word).
enum Leaf {
    Lit(i64),
    Boxed(u32),
    Ahead(bool),
    /// A param a twin took unpacked: its `i64` and Big flag.
    Param(u32, u32),
}

fn leaf_of(e: Node<'_>, slots: &SlotTable, ctx: &EmitCtx<'_>) -> Leaf {
    if let Some(n) = int_literal(e) {
        return Leaf::Lit(n);
    }
    match &e.node {
        MirExpr::Local(l) if !ctx.slot_is_bare(l.node.slot.0) => match twin::held(l.node.slot.0) {
            Some((_, big)) => Leaf::Param(slots.local(l.node.slot.0), big),
            None => Leaf::Boxed(slots.local(l.node.slot.0)),
        },
        MirExpr::Box(inner) => Leaf::Ahead(mir_renders_raw_i64(&inner.node, ctx)),
        _ => Leaf::Ahead(mir_is_eligible_carrier_value_project(&e.node, ctx)),
    }
}

/// The number of operations of the tree at `e`; `None` when a leaf renders a
/// raw `i64` where the tree wants an `$AverInt` (the rewrite boxes those).
fn weight(e: Node<'_>, ctx: &EmitCtx<'_>) -> Option<usize> {
    match op_of(e, ctx) {
        Some((_, a, b)) => Some(1 + weight(a, ctx)? + b.map_or(Some(0), |b| weight(b, ctx))?),
        None => (!mir_renders_raw_i64(&e.node, ctx)).then_some(0),
    }
}

/// Whether the tree at `e` has a leaf that calls an `Int` twin.
fn twin_leaf(e: Node<'_>, ctx: &EmitCtx<'_>) -> bool {
    match op_of(e, ctx) {
        Some((_, a, b)) => twin_leaf(a, ctx) || b.is_some_and(|b| twin_leaf(b, ctx)),
        None => twin::callee(e, ctx).is_some_and(|(_, t)| t.kind == TwinKind::Int),
    }
}

/// `unpacked`: the tree's value is compared or unboxed, not boxed.
fn enabled(ctx: &EmitCtx<'_>, roots: &[Node<'_>], unpacked: bool) -> bool {
    // A comparison of two leaves is worth it too, unless a side is a literal
    // (`emit_aint_cmp_const` already compares that without a call) and the
    // other no slot a twin holds. A leaf that calls a twin is worth it
    // wherever the value is not boxed.
    let twin = || unpacked && roots.iter().any(|r| twin_leaf(r, ctx));
    let held =
        |r: &Node<'_>| matches!(&r.node, MirExpr::Local(l) if twin::held(l.node.slot.0).is_some());
    let leaves = roots.len() == 2
        && (roots.iter().all(|r| int_literal(r).is_none()) || roots.iter().any(held));
    let weights = || roots.iter().map(|r| weight(r, ctx)).sum::<Option<usize>>();
    ctx.registry.bignum
        && ctx.registry.aint_struct_idx.is_some()
        && EXACT.with(|d| d.get() == 0)
        && ALLOWED.with(|a| a.get())
        && weights().is_some_and(|w| w > 0 || leaves || twin())
}

/// On the exact path, a leaf evaluated ahead is read from its local.
pub(crate) fn emit_hoisted_leaf(
    func: &mut Function,
    expr: Node<'_>,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let key = expr as *const _ as usize;
    let Some(held) = HOISTED.with(|h| h.borrow().get(&key).copied()) else {
        return Ok(None);
    };
    match held {
        Held::Boxed(local) => {
            func.instruction(&I::LocalGet(local));
        }
        Held::Raw(local) => {
            func.instruction(&I::LocalGet(local));
            lift_i64_result_to_aint(func, ctx)?;
        }
        Held::Twin(n, r) => twin::pack_held(func, n, r, ctx)?,
    }
    Ok(Some(true))
}

/// A tree at `expr`, boxed; or, with `unbox` (the `Unbox` around `expr`),
/// its `i64`.
pub(crate) fn try_emit_value(
    func: &mut Function,
    expr: Node<'_>,
    unbox: Option<Node<'_>>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Option<bool>>, WasmGcError> {
    // A tail of an `Int` twin returns the machine word unboxed.
    let tail = unbox.is_none() && twin::tail_kind(expr) == Some(TwinKind::Int);
    if tail && twin::callee(expr, ctx).is_some() {
        // The call arm hands the callee twin's result on.
        return Ok(None);
    }
    if !enabled(ctx, &[expr], unbox.is_some() || tail) {
        return Ok(None);
    }
    let whole = unbox.unwrap_or(expr);
    let exact = &mut |f: &mut Function| Ok(emit_mir_expr(f, whole, slots, ctx)?.map(drop));
    let result = match (unbox, tail) {
        (Some(_), _) => Out::Word,
        (None, true) => Out::Returned,
        (None, false) => Out::Boxed,
    };
    let r = emit_tree(func, &[expr], None, result, slots, ctx, exact)?;
    Ok(Some(r.map(|()| true)))
}

/// A comparison with a tree on a side: the machine comparison, unboxed.
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
    let roots: [Node<'_>; 2] = [&bop.lhs, &bop.rhs];
    if !is_int(&bop.lhs) || mir_int_binop_is_raw(bop, ctx) || !enabled(ctx, &roots, true) {
        return Ok(None);
    }
    let exact = &mut |f: &mut Function| emit_mir_numeric_binop(f, bop, slots, ctx);
    emit_tree(func, &roots, Some(cmp), Out::Word, slots, ctx, exact).map(Some)
}

/// What a tree's `i64` path leaves: the machine word (or the comparison of
/// two), the word boxed, or nothing, having returned it from an `Int` twin.
#[derive(Clone, Copy, PartialEq)]
enum Out {
    Word,
    Boxed,
    Returned,
}

/// Whether `e` is a tree worth computing on the machine word for a value
/// wanted unpacked.
pub(crate) fn makes_word(e: Node<'_>, ctx: &EmitCtx<'_>) -> bool {
    enabled(ctx, &[e], true)
}

/// The tree at `e` as its machine word, `exact` leaving the word of the
/// packed value instead.
pub(crate) fn emit_word(
    func: &mut Function,
    expr: Node<'_>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    exact: &mut dyn FnMut(&mut Function) -> Result<Option<()>, WasmGcError>,
) -> Result<Option<()>, WasmGcError> {
    emit_tree(func, &[expr], None, Out::Word, slots, ctx, exact)
}

/// `block $done (block $slow <i64 path> br $done) <exact path>`.
fn emit_tree(
    func: &mut Function,
    roots: &[Node<'_>],
    cmp: Option<I<'static>>,
    result: Out,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    exact: &mut dyn FnMut(&mut Function) -> Result<Option<()>, WasmGcError>,
) -> Result<Option<()>, WasmGcError> {
    let aint = struct_ref(ctx.registry.aint_struct_idx.unwrap_or_default());
    let mut ahead = Hoisted::new();
    for r in roots {
        if hoist(func, r, aint, slots, ctx, &mut ahead)?.is_none() {
            return Ok(None);
        }
    }
    let ty = if cmp.is_some() {
        ValType::I32
    } else if result == Out::Word {
        ValType::I64
    } else {
        aint
    };
    seq(
        func,
        &[I::Block(BlockType::Result(ty)), I::Block(BlockType::Empty)],
    );
    let temps = slots.int_tree_temps();
    for r in roots {
        if emit_fast(func, r, &ahead, temps, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    if let Some(cmp) = cmp {
        func.instruction(&cmp);
    } else if result == Out::Boxed {
        lift_i64_result_to_aint(func, ctx)?;
    } else if result == Out::Returned {
        twin::return_small(func, ctx);
    }
    seq(func, &[I::Br(1), I::End]);
    HOISTED.with(|h| h.borrow_mut().extend(ahead.iter().map(|(k, v)| (*k, *v))));
    EXACT.with(|d| d.set(d.get() + 1));
    let r = exact(func);
    EXACT.with(|d| d.set(d.get() - 1));
    HOISTED.with(|h| h.borrow_mut().retain(|k, _| !ahead.contains_key(k)));
    if r?.is_none() {
        return Ok(None);
    }
    func.instruction(&I::End);
    Ok(Some(()))
}

/// Evaluate the leaves of `e` that are read ahead, in source order.
fn hoist(
    func: &mut Function,
    e: Node<'_>,
    aint: ValType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    out: &mut Hoisted,
) -> Result<Option<()>, WasmGcError> {
    if let Some((_, a, b)) = op_of(e, ctx) {
        for x in std::iter::once(a).chain(b) {
            if hoist(func, x, aint, slots, ctx, out)?.is_none() {
                return Ok(None);
            }
        }
        return Ok(Some(()));
    }
    let Leaf::Ahead(raw) = leaf_of(e, slots, ctx) else {
        return Ok(Some(()));
    };
    if let Some((call, twin)) = twin::callee(e, ctx)
        && twin.kind == TwinKind::Int
    {
        if twin::emit_call(func, call, &twin, slots, ctx)?.is_none() {
            return Ok(None);
        }
        let (n, r) = twin::hold_int(func, slots, ctx);
        out.insert(e as *const _ as usize, Held::Twin(n, r));
        return Ok(Some(()));
    }
    let produced = match &e.node {
        MirExpr::Box(inner) if raw => emit_mir_int_raw(func, inner, slots, ctx)?,
        _ if raw => emit_mir_carrier_value_raw(func, e, slots, ctx)?,
        _ => emit_mir_expr(func, e, slots, ctx)?,
    };
    if produced.is_none() {
        return Ok(None);
    }
    let local = slots.fresh_local(if raw { ValType::I64 } else { aint });
    func.instruction(&I::LocalSet(local));
    let held = if raw {
        Held::Raw(local)
    } else {
        Held::Boxed(local)
    };
    out.insert(e as *const _ as usize, held);
    Ok(Some(()))
}

/// Pop an `i64` into `t`, leaving the `i64` path unless it lies in `[lo, hi]`.
fn guard(func: &mut Function, lo: i128, hi: i128, t: u32) {
    let (lo, hi) = (lo.max(i64::MIN.into()), hi.min(i64::MAX.into()));
    func.instruction(&I::LocalSet(t));
    if lo > hi {
        func.instruction(&I::Br(0));
    } else if hi - lo < i128::from(u64::MAX) {
        let span = I::I64Const((hi - lo) as i64);
        seq(func, &[I::LocalGet(t), I::I64Const(lo as i64), I::I64Sub]);
        seq(func, &[span, I::I64GtU, I::BrIf(0)]);
    }
}

/// The `x` for which `x * k` fits an `i64`: `[ceil(lo / k), floor(hi / k)]`.
fn mul_range(k: i128) -> (i128, i128) {
    let (min, max) = (i128::from(i64::MIN), i128::from(i64::MAX));
    let (lo, hi) = if k < 0 { (max, min) } else { (min, max) };
    let floor = |a: i128| (a * k.signum()).div_euclid(k.abs());
    if k == 0 {
        (min, max)
    } else {
        (-floor(-lo), floor(hi))
    }
}

/// Push the `i64` of `e`, leaving the path (`br 0`) on a Big leaf or an
/// overflow. The three `temps` live only inside one operation.
fn emit_fast(
    func: &mut Function,
    e: Node<'_>,
    ahead: &Hoisted,
    temps: [u32; 3],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let [t0, t1, t2] = temps;
    let (g0, g1, g2) = (I::LocalGet(t0), I::LocalGet(t1), I::LocalGet(t2));
    let Some((op, a, b)) = op_of(e, ctx) else {
        let boxed = match leaf_of(e, slots, ctx) {
            Leaf::Lit(n) => {
                func.instruction(&I::I64Const(n));
                return Ok(Some(()));
            }
            Leaf::Param(n, big) => {
                seq(func, &[I::LocalGet(big), I::BrIf(0), I::LocalGet(n)]);
                return Ok(Some(()));
            }
            Leaf::Boxed(local) => local,
            Leaf::Ahead(_) => match ahead.get(&(e as *const _ as usize)) {
                Some(Held::Boxed(local)) => *local,
                Some(Held::Raw(local)) => {
                    func.instruction(&I::LocalGet(*local));
                    return Ok(Some(()));
                }
                // A twin's `Int` is its `i64` when the ref is null.
                Some(Held::Twin(n, r)) => {
                    seq(func, &[I::LocalGet(*r), I::RefIsNull, I::I32Eqz]);
                    seq(func, &[I::BrIf(0), I::LocalGet(*n)]);
                    return Ok(Some(()));
                }
                None => return unsupported("an Int tree leaf that was not evaluated ahead"),
            },
        };
        // The `$small` of a Small `$AverInt`; a Big one leaves the path.
        let field = |field_index| I::StructGet {
            struct_type_index: ctx.registry.aint_struct_idx.unwrap_or_default(),
            field_index,
        };
        seq(
            func,
            &[I::LocalGet(boxed), field(1), I::RefIsNull, I::I32Eqz],
        );
        seq(func, &[I::BrIf(0), I::LocalGet(boxed), field(0)]);
        return Ok(Some(()));
    };
    let (min, max) = (i128::from(i64::MIN), i128::from(i64::MAX));
    let by_literal = match (int_literal(a), b.and_then(int_literal), b) {
        (_, Some(k), _) => Some((a, k)),
        (Some(k), None, Some(b)) => Some((b, k)),
        _ => None,
    };
    if op == Op::Mul
        && let Some((x, k)) = by_literal
    {
        // By a literal `k`: one range test instead of a division.
        let (lo, hi) = mul_range(k.into());
        if emit_fast(func, x, ahead, temps, slots, ctx)?.is_none() {
            return Ok(None);
        }
        guard(func, lo, hi, t0);
        seq(func, &[g0, I::I64Const(k), I::I64Mul]);
        return Ok(Some(()));
    }
    for x in std::iter::once(a).chain(b) {
        if emit_fast(func, x, ahead, temps, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    let both = [I::LocalSet(t1), I::LocalSet(t0), g0.clone(), g1.clone()];
    // a ∉ {-1, 0}, as `(a + 1) >u 1`.
    let general = [
        g0.clone(),
        I::I64Const(1),
        I::I64Add,
        I::I64Const(1),
        I::I64GtU,
    ];
    match op {
        Op::Add | Op::Sub => {
            seq(func, &both);
            let sum = if op == Op::Add { I::I64Add } else { I::I64Sub };
            seq(func, &[sum, I::LocalTee(t2)]);
            // Add: (s ^ a) & (s ^ b) < 0; sub: (a ^ b) & (a ^ d) < 0.
            let [x, y, z] = if op == Op::Add {
                [g0, g2.clone(), g1]
            } else {
                [g0.clone(), g0, g1]
            };
            seq(func, &[x, I::I64Xor, y, z, I::I64Xor, I::I64And]);
            seq(func, &[I::I64Const(0), I::I64LtS, I::BrIf(0), g2]);
        }
        Op::Mul => {
            // As `bignum.rs` tests it: for a ∉ {-1, 0} the product overflowed
            // iff p / a != b; for a = -1 iff b = i64::MIN; for a = 0 never.
            // The divisor is 1 for a ∈ {-1, 0}, so `div_s` cannot trap.
            seq(func, &both);
            seq(
                func,
                &[I::I64Mul, I::LocalTee(t2), g0.clone(), I::I64Const(1)],
            );
            seq(func, &general);
            seq(func, &[I::Select, I::I64DivS, g1.clone(), I::I64Ne, g0]);
            seq(
                func,
                &[I::I64Const(-1), I::I64Eq, g1, I::I64Const(i64::MIN)],
            );
            seq(func, &[I::I64Eq, I::I32And]);
            seq(func, &general);
            seq(func, &[I::Select, I::BrIf(0), g2]);
        }
        Op::Neg | Op::Abs | Op::Div(-1) => {
            guard(func, min + 1, max, t0);
            seq(func, &[I::I64Const(0), g0.clone(), I::I64Sub]);
            if op == Op::Abs {
                seq(
                    func,
                    &[g0.clone(), g0, I::I64Const(0), I::I64LtS, I::Select],
                );
            }
        }
        // An `i64` IS its infinite two's-complement sequence: exact.
        Op::And => seq(func, &[I::I64And]),
        Op::Or => seq(func, &[I::I64Or]),
        Op::Xor => seq(func, &[I::I64Xor]),
        Op::Not => seq(func, &[I::I64Const(-1), I::I64Xor]),
        Op::Min | Op::Max => {
            let keep_a = if op == Op::Min { I::I64LeS } else { I::I64GeS };
            seq(func, &both);
            seq(func, &[g0, g1, keep_a, I::Select]);
        }
        // By a positive power of two, Euclidean division is a floor: a shift.
        Op::Div(k) if k > 0 && k.count_ones() == 1 => {
            seq(func, &[I::I64Const(k.trailing_zeros().into()), I::I64ShrS]);
        }
        Op::Div(k) => {
            // The truncated quotient, one further from zero when the
            // truncated remainder is negative.
            let step = if k > 0 { I::I64Add } else { I::I64Sub };
            seq(func, &[I::LocalTee(t0), I::I64Const(k), I::I64DivS, g0]);
            seq(
                func,
                &[I::I64Const(k), I::I64RemS, I::I64Const(63), I::I64ShrS],
            );
            func.instruction(&step);
        }
        // By ±2^n, the Euclidean remainder is the low n bits.
        Op::Mod(k) if k.unsigned_abs().is_power_of_two() => {
            let mask = (k.unsigned_abs() - 1) as i64;
            seq(func, &[I::I64Const(mask), I::I64And]);
        }
        Op::Mod(k) => {
            // r + (|k| if r < 0), r the truncated remainder.
            let m = I::I64Const(k.unsigned_abs() as i64);
            seq(func, &[I::I64Const(k), I::I64RemS, I::LocalTee(t0), g0]);
            seq(
                func,
                &[I::I64Const(63), I::I64ShrS, m, I::I64And, I::I64Add],
            );
        }
        Op::Shl(n) => {
            let (lo, hi) = if n > 63 { (0, 0) } else { (min >> n, max >> n) };
            guard(func, lo, hi, t0);
            seq(func, &[g0, I::I64Const(n.min(63)), I::I64Shl]);
        }
        Op::Shr(n) => seq(func, &[I::I64Const(n.min(63)), I::I64ShrS]),
        Op::Low(w) if w <= 63 => seq(func, &[I::I64Const(i64::MAX >> (63 - w)), I::I64And]),
        // A negative value's low bits past the 63rd do not fit.
        Op::Low(_) => {
            guard(func, 0, max, t0);
            func.instruction(&g0);
        }
    }
    Ok(Some(()))
}
