//! The pure `Int` operations a backend computes on a machine word first and
//! again exactly, on a Big leaf or an overflow: wasm-gc in
//! `codegen/wasm_gc/body/from_mir/int_tree.rs`, Rust in
//! `codegen/rust/int_tree.rs`. Each is total on ℤ, so running the operations
//! of a tree after all its leaves, or twice, cannot be observed.

use crate::ast::{BinOp, Literal, Spanned, Type};
use crate::ir::hir::BuiltinIntrinsic;

use super::expr::{MirCallee, MirExpr};

/// `Div`/`Mod` divide by a nonzero literal; `Shl`/`Shr`/`Low` take a
/// literal count.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntOp {
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

pub type Node<'e> = &'e Spanned<MirExpr>;

pub fn int_literal(e: &MirExpr) -> Option<i64> {
    match e {
        MirExpr::Literal(l) => match l.node {
            Literal::Int(n) => Some(n),
            _ => None,
        },
        _ => None,
    }
}

pub fn is_int(e: Node<'_>) -> bool {
    matches!(e.ty(), Some(Type::Int))
}

/// The operation `e` is, with its operands; `builtins` names each
/// `BuiltinId`. A backend that already renders some of these on a raw
/// `i64` leaves those out itself.
pub fn op_of<'e>(e: Node<'e>, builtins: &[String]) -> Option<(IntOp, Node<'e>, Option<Node<'e>>)> {
    match &e.node {
        MirExpr::BinOp(b) => {
            let op = match b.node.op {
                BinOp::Add => IntOp::Add,
                BinOp::Sub => IntOp::Sub,
                BinOp::Mul => IntOp::Mul,
                _ => return None,
            };
            is_int(&b.node.lhs).then_some((op, &b.node.lhs, Some(&b.node.rhs)))
        }
        MirExpr::Neg(x) if is_int(x) => Some((IntOp::Neg, &**x, None)),
        MirExpr::Call(c) if is_int(e) && c.node.args.iter().all(is_int) => {
            let args = &c.node.args;
            let op = match (&c.node.callee, args.len()) {
                (MirCallee::Builtin(id), n) => match (builtins.get(id.0 as usize)?.as_str(), n) {
                    ("Int.min", 2) => IntOp::Min,
                    ("Int.max", 2) => IntOp::Max,
                    ("Int.abs", 1) => IntOp::Abs,
                    ("Bits.and", 2) => IntOp::And,
                    ("Bits.or", 2) => IntOp::Or,
                    ("Bits.xor", 2) => IntOp::Xor,
                    ("Bits.not", 1) => IntOp::Not,
                    _ => return None,
                },
                (MirCallee::Intrinsic(intr), 2) => match (intr, int_literal(&args[1].node)?) {
                    (BuiltinIntrinsic::IntDivEuclid, k) if k != 0 => IntOp::Div(k),
                    (BuiltinIntrinsic::IntModEuclid, k) if k != 0 => IntOp::Mod(k),
                    (BuiltinIntrinsic::BitsShiftLeft, k) if k >= 0 => IntOp::Shl(k),
                    (BuiltinIntrinsic::BitsShiftRight, k) if k >= 0 => IntOp::Shr(k),
                    (BuiltinIntrinsic::BitsLow, k) if k >= 0 => IntOp::Low(k),
                    _ => return None,
                },
                _ => return None,
            };
            let binary = matches!(
                op,
                IntOp::Min | IntOp::Max | IntOp::And | IntOp::Or | IntOp::Xor
            );
            Some((op, &args[0], binary.then(|| &args[1])))
        }
        _ => None,
    }
}
