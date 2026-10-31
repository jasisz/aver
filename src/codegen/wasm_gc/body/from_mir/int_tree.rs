//! Lowering the `Int` trees computed on a machine word first.
//!
//! Every `Int` on wasm-gc is an `$AverInt` struct, so each `+`, `*`,
//! `Int.mod`, `Bits.and`, … allocates one, although the values almost always
//! fit an `i64`. Which trees are computed on the word is decided in MIR: the
//! `ir::mir::int_word` pass writes each as a `MirExpr::IntWord` node, and
//! this file lowers the node as it is. A node is emitted twice: on `i64`,
//! with the overflow tests of `builtins/bignum.rs`, and, for a Big leaf or an
//! overflow, as the plain arithmetic it stands for (`MirIntWord::exact`),
//! with its ordinary lowering. The `i64` result is boxed once, or not at all
//! when compared or narrowed. Each leaf the node reads ahead is evaluated
//! once, in source order, ahead of both paths; the operations are pure and
//! total, so running them after the leaves, or twice, cannot be observed.

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use wasm_encoder::BlockType;
use wasm_encoder::Instruction as I;

use super::twin;
use super::*;
use crate::codegen::wasm_gc::twins::TwinKind;
use crate::codegen::wasm_gc::types::struct_ref;
use crate::ir::mir::int_tree::IntOp as Op;
use crate::ir::mir::{IntWordHold, IntWordKind, IntWordNode, MirIntWord};

thread_local! {
    /// Exact paths being emitted: no tail of a twin is among them.
    static EXACT: Cell<u32> = const { Cell::new(0) };
    /// Leaves evaluated ahead of a tree, by node address, and where.
    static HOISTED: RefCell<HashMap<usize, Held>> = RefCell::new(HashMap::new());
    /// Whether the fn being emitted takes the fast forms a `--certify` build
    /// leaves out of a fn `kept_by_printer` (`module.rs` decides): a shared
    /// boxed literal, a builtin's word result held as a word, a twin's word.
    static ALLOWED: Cell<bool> = const { Cell::new(true) };
    /// The boxed `Int` literals emitted while recording (`module.rs` records
    /// each fn's in its pre-pass), and the global holding each one shared.
    static LITERALS: RefCell<Option<std::collections::BTreeSet<i64>>> = const { RefCell::new(None) };
    static POOL: RefCell<HashMap<i64, u32>> = RefCell::new(HashMap::new());
    /// The wasm local of each `IntWord` scratch local of the fn last
    /// emitted, by its index in `MirFnRepr::scratch`: the certificate plan
    /// printer names these locals in the printed nodes.
    static INT_WORD_LOCALS: RefCell<Vec<Option<u32>>> = const { RefCell::new(Vec::new()) };
}

pub(crate) fn forget_int_word_locals() {
    INT_WORD_LOCALS.with(|l| l.borrow_mut().clear());
}

pub(crate) fn note_int_word_local(k: u32, idx: u32) {
    INT_WORD_LOCALS.with(|l| {
        let mut l = l.borrow_mut();
        let k = k as usize;
        if l.len() <= k {
            l.resize(k + 1, None);
        }
        l[k] = Some(idx);
    });
}

/// The wasm local of each `IntWord` scratch local of the fn last emitted.
pub(crate) fn int_word_locals() -> Vec<Option<u32>> {
    INT_WORD_LOCALS.with(|l| l.borrow().clone())
}

/// Whether the fn being emitted takes the fast forms (see `ALLOWED`).
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

/// Where a leaf evaluated ahead is held: an `$AverInt`, a raw `i64` (the
/// exact path boxes it), or an `Int` twin's result `(n, r)`.
#[derive(Clone, Copy)]
enum Held {
    Boxed(u32),
    Raw(u32),
    Twin(u32, u32),
}

/// Leaves evaluated ahead, by the address of their node.
type Hoisted = HashMap<usize, Held>;

fn key<T>(x: &T) -> usize {
    x as *const T as usize
}

fn seq(func: &mut Function, xs: &[I<'_>]) {
    for x in xs {
        func.instruction(x);
    }
}

/// On the exact path, a leaf evaluated ahead is read from its local.
pub(crate) fn emit_hoisted_leaf(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let Some(held) = HOISTED.with(|h| h.borrow().get(&key(expr)).copied()) else {
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

/// What a node's `i64` path leaves: the machine word (or the comparison of
/// two), the word boxed, or nothing, having returned it from an `Int` twin.
#[derive(Clone, Copy, PartialEq)]
enum Out {
    Word,
    Boxed,
    Returned,
}

/// The exact path of a node: given the node as plain arithmetic
/// ([`MirIntWord::exact`]), leave what the node gives.
type Exact<'a> =
    &'a mut dyn FnMut(&mut Function, &Spanned<MirExpr>) -> Result<Option<()>, WasmGcError>;

/// An [`MirExpr::IntWord`] node `expr` where its parent takes it as it is:
/// a value (boxed, or returned unboxed as a tail of an `Int` twin), or a
/// comparison.
pub(crate) fn emit_int_word(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    word: &MirIntWord,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let r = match &word.kind {
        IntWordKind::Value => {
            // A tail of an `Int` twin returns the machine word unboxed.
            let out = if twin::tail_kind(expr) == Some(TwinKind::Int) {
                Out::Returned
            } else {
                Out::Boxed
            };
            let exact = &mut |f: &mut Function, e: &Spanned<MirExpr>| {
                Ok(emit_mir_expr(f, e, slots, ctx)?.map(drop))
            };
            emit_tree(func, word, None, out, slots, ctx, exact)?
        }
        IntWordKind::Compare { op, .. } => {
            let cmp = match op {
                BinOp::Eq => I::I64Eq,
                BinOp::Neq => I::I64Ne,
                BinOp::Lt => I::I64LtS,
                BinOp::Gt => I::I64GtS,
                BinOp::Lte => I::I64LeS,
                BinOp::Gte => I::I64GeS,
                _ => return unsupported("an Int tree compared by a non-comparison"),
            };
            let exact = &mut |f: &mut Function, e: &Spanned<MirExpr>| match &e.node {
                MirExpr::BinOp(b) => emit_mir_numeric_binop(f, &b.node, slots, ctx),
                _ => unsupported("an Int tree comparison that is not a comparison"),
            };
            emit_tree(func, word, Some(cmp), Out::Word, slots, ctx, exact)?
        }
        IntWordKind::Word => return unsupported("an Int tree's machine word outside an Unbox"),
    };
    Ok(r.map(|()| true))
}

/// `unbox`, an `Unbox` of an [`IntWordKind::Word`] node: the tree's
/// machine word.
pub(crate) fn emit_unboxed(
    func: &mut Function,
    unbox: &Spanned<MirExpr>,
    word: &MirIntWord,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let exact = &mut |f: &mut Function, e: &Spanned<MirExpr>| {
        let whole = Spanned::new(MirExpr::Unbox(Box::new(e.clone())), unbox.line);
        if let Some(ty) = unbox.ty() {
            whole.set_ty(ty.clone());
        }
        Ok(emit_mir_expr(f, &whole, slots, ctx)?.map(drop))
    };
    Ok(emit_tree(func, word, None, Out::Word, slots, ctx, exact)?.map(|()| true))
}

/// Whether `e` is a tree computed on the machine word, so that a parent
/// wanting a word can have it unboxed ([`emit_word`]).
pub(crate) fn makes_word(e: &Spanned<MirExpr>) -> bool {
    matches!(&e.node, MirExpr::IntWord(w) if matches!(w.node.kind, IntWordKind::Value))
}

/// The tree at `expr` ([`makes_word`]) as its machine word; `after_exact`
/// follows the exact path's packed value.
pub(crate) fn emit_word(
    func: &mut Function,
    expr: &Spanned<MirExpr>,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    after_exact: &mut dyn FnMut(&mut Function) -> Result<Option<()>, WasmGcError>,
) -> Result<Option<()>, WasmGcError> {
    let MirExpr::IntWord(word) = &expr.node else {
        return unsupported("a machine word of an expression that is no Int tree");
    };
    let exact = &mut |f: &mut Function, e: &Spanned<MirExpr>| {
        if emit_mir_expr(f, e, slots, ctx)?.is_none() {
            return Ok(None);
        }
        after_exact(f)
    };
    emit_tree(func, &word.node, None, Out::Word, slots, ctx, exact)
}

/// Each leaf read ahead stored in its scratch local, then
/// `block $done (block $slow <i64 path> br $done) <exact path>`. The Lean
/// wall lowers a printed value node to the same code (`GrammarLower`,
/// `wordBlock`).
fn emit_tree(
    func: &mut Function,
    word: &MirIntWord,
    cmp: Option<I<'static>>,
    result: Out,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
    exact: Exact<'_>,
) -> Result<Option<()>, WasmGcError> {
    let Some(aint_idx) = ctx.registry.aint_struct_idx.filter(|_| ctx.registry.bignum) else {
        return unsupported("an Int tree on the machine word without the Int carrier");
    };
    let aint = struct_ref(aint_idx);
    let whole = word.exact();
    let mut leaves = Vec::new();
    for t in word.trees() {
        leaf_nodes(t, &mut leaves);
    }
    let sites = word.leaf_sites(&whole);
    let mut ahead = Hoisted::new();
    let mut at_sites = Hoisted::new();
    for (node, site) in leaves.iter().zip(&sites) {
        if let IntWordNode::Ahead(e, k, hold) = node {
            let Some(held) = hoist(func, e, *k, *hold, aint, slots, ctx)? else {
                return Ok(None);
            };
            ahead.insert(key(*node), held);
            at_sites.insert(key(*site), held);
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
    let temps = slots.int_word_words(word.words);
    for t in word.trees() {
        if emit_fast(func, t, &ahead, temps, slots, ctx)?.is_none() {
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
    HOISTED.with(|h| {
        h.borrow_mut()
            .extend(at_sites.iter().map(|(k, v)| (*k, *v)))
    });
    EXACT.with(|d| d.set(d.get() + 1));
    let r = exact(func, &whole);
    EXACT.with(|d| d.set(d.get() - 1));
    HOISTED.with(|h| h.borrow_mut().retain(|k, _| !at_sites.contains_key(k)));
    if r?.is_none() {
        return Ok(None);
    }
    func.instruction(&I::End);
    Ok(Some(()))
}

fn leaf_nodes<'a>(node: &'a IntWordNode, out: &mut Vec<&'a IntWordNode>) {
    match node {
        IntWordNode::Op { args, .. } => {
            for a in args {
                leaf_nodes(a, out);
            }
        }
        leaf => out.push(leaf),
    }
}

/// Evaluate a leaf read ahead into its scratch local `k`, held as `hold`
/// says; a call of an `Int` twin is held as the twin's result, and, outside
/// a fn a `--certify` build keeps, a length or count a builtin computes as
/// a word is held as that word.
#[allow(clippy::too_many_arguments)]
fn hoist(
    func: &mut Function,
    e: &Spanned<MirExpr>,
    k: u32,
    hold: IntWordHold,
    aint: ValType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<Held>, WasmGcError> {
    let (produced, raw) = match (hold, &e.node) {
        (IntWordHold::RawBox, MirExpr::Box(inner)) => {
            (emit_mir_int_raw(func, inner, slots, ctx)?, true)
        }
        (IntWordHold::RawBox, _) => {
            return unsupported("an Int tree leaf held raw that is no Box");
        }
        (IntWordHold::RawField, _) => (emit_mir_carrier_value_raw(func, e, slots, ctx)?, true),
        (IntWordHold::Int, _) => {
            if let Some((call, twin)) = twin::callee(e, ctx)
                && twin.kind == TwinKind::Int
            {
                if twin::emit_call(func, call, &twin, slots, ctx)?.is_none() {
                    return Ok(None);
                }
                let (n, r) = twin::hold_int(func, slots, ctx);
                return Ok(Some(Held::Twin(n, r)));
            }
            if allowed() {
                let (produced, word) =
                    twin::emit_word_result(|| emit_mir_expr(func, e, slots, ctx));
                (produced?, word)
            } else {
                (emit_mir_expr(func, e, slots, ctx)?, false)
            }
        }
    };
    if produced.is_none() {
        return Ok(None);
    }
    let local = slots.int_word_scratch(k, if raw { ValType::I64 } else { aint });
    func.instruction(&I::LocalSet(local));
    Ok(Some(if raw {
        Held::Raw(local)
    } else {
        Held::Boxed(local)
    }))
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

/// Push the `i64` of `node`, leaving the path (`br 0`) on a Big leaf or an
/// overflow. The three `temps` live only inside one operation.
fn emit_fast(
    func: &mut Function,
    node: &IntWordNode,
    ahead: &Hoisted,
    temps: [u32; 3],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let [t0, t1, _] = temps;
    let (g0, g1) = (I::LocalGet(t0), I::LocalGet(t1));
    let (op, args) = match node {
        IntWordNode::Op { op, args, .. } => (*op, args),
        IntWordNode::Lit(_) => {
            let Some(n) = node.literal() else {
                return unsupported("an Int tree literal leaf that is no Int literal");
            };
            func.instruction(&I::I64Const(n));
            return Ok(Some(()));
        }
        IntWordNode::Local(e) => {
            let MirExpr::Local(l) = &e.node else {
                return unsupported("an Int tree leaf read in place that is not a local");
            };
            let slot = l.node.slot.0;
            // A param a twin took unpacked: its `i64` and Big flag.
            if let Some((_, big)) = twin::held(slot) {
                seq(
                    func,
                    &[I::LocalGet(big), I::BrIf(0), I::LocalGet(slots.local(slot))],
                );
            } else {
                emit_small(func, slots.local(slot), ctx);
            }
            return Ok(Some(()));
        }
        IntWordNode::Ahead(..) => {
            match ahead.get(&key(node)) {
                Some(Held::Boxed(local)) => emit_small(func, *local, ctx),
                Some(Held::Raw(local)) => {
                    func.instruction(&I::LocalGet(*local));
                }
                // A twin's `Int` is its `i64` when the ref is null.
                Some(Held::Twin(n, r)) => {
                    seq(func, &[I::LocalGet(*r), I::RefIsNull, I::I32Eqz]);
                    seq(func, &[I::BrIf(0), I::LocalGet(*n)]);
                }
                None => return unsupported("an Int tree leaf that was not evaluated ahead"),
            }
            return Ok(Some(()));
        }
    };
    let (min, max) = (i128::from(i64::MIN), i128::from(i64::MAX));
    let Some(a) = args.first() else {
        return unsupported("an Int tree operation without operands");
    };
    let b = args.get(1);
    let by_literal = match (a.literal(), b.and_then(IntWordNode::literal), b) {
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
    for x in args {
        if emit_fast(func, x, ahead, temps, slots, ctx)?.is_none() {
            return Ok(None);
        }
    }
    let both = [I::LocalSet(t1), I::LocalSet(t0), g0.clone(), g1.clone()];
    match op {
        Op::Add | Op::Sub | Op::Mul => emit_word_op(func, op, temps),
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

/// The `$small` of the Small `$AverInt` in `local`; a Big one leaves the
/// path.
fn emit_small(func: &mut Function, local: u32, ctx: &EmitCtx<'_>) {
    let field = |field_index| I::StructGet {
        struct_type_index: ctx.registry.aint_struct_idx.unwrap_or_default(),
        field_index,
    };
    seq(
        func,
        &[I::LocalGet(local), field(1), I::RefIsNull, I::I32Eqz],
    );
    seq(func, &[I::BrIf(0), I::LocalGet(local), field(0)]);
}

/// The machine `+`, `-` or `*` of the two words on the stack, with the
/// overflow test of `builtins/bignum.rs`: the operands are parked in the
/// first two `temps` and the result in the third; an overflow leaves the
/// `i64` path (`br_if 0`). The Lean wall's `GrammarLower.treeOpW` is this
/// sequence.
fn emit_word_op(func: &mut Function, op: Op, temps: [u32; 3]) {
    let [t0, t1, t2] = temps;
    let (g0, g1, g2) = (I::LocalGet(t0), I::LocalGet(t1), I::LocalGet(t2));
    seq(
        func,
        &[I::LocalSet(t1), I::LocalSet(t0), g0.clone(), g1.clone()],
    );
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
        _ => {
            // As `bignum.rs` tests it: for a ∉ {-1, 0} the product overflowed
            // iff p / a != b; for a = -1 iff b = i64::MIN; for a = 0 never.
            // The divisor is 1 for a ∈ {-1, 0}, so `div_s` cannot trap.
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
    }
}
