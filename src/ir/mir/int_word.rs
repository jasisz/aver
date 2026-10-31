//! The `Int` trees a function computes on the machine word, made explicit.
//!
//! This pass is where the choice is made: it rewrites each chosen tree to a
//! [`MirExpr::IntWord`] node naming its operations, its leaves, where each
//! leaf is read, and the word scratch locals it uses, declared in
//! [`MirFnRepr::scratch`](super::program::MirFnRepr::scratch). The wasm-gc
//! emitter lowers the node as it is, the certificate plan printer prints it
//! as it is, and the checker's wall lowers the printed node and proves it
//! computes the tree's value; none of them chooses again.
//!
//! It runs on the functions a `--certify` wasm-gc build keeps for its
//! certificate. A tree is a nest of `Int` `+`, `-` and `*` whose leaves are
//! `Int` locals (read in place) or fields of records held in locals (read
//! once, ahead of the tree, into a scratch local), of any depth. A tree is
//! chosen when it has a field leaf: a tree of locals only saves no field
//! read, and the emitter's word-record field reads already compute on the
//! word, so a field of a word record is never a leaf.

use crate::ast::{BinOp, Spanned, Type};

use super::expr::{IntWordNode, IntWordOp, MirExpr, MirIntWord, walk_children_mut};
use super::program::{MirFn, MirScratch};

/// The choices that depend on the target's representation.
pub struct Target<'a> {
    /// Whether a field read is of a record the target keeps as the machine
    /// word of its one `Int` field (a word record).
    pub is_word_field: &'a dyn Fn(&Spanned<MirExpr>) -> bool,
}

/// `f` with its chosen trees as [`MirExpr::IntWord`] nodes, or `None` when
/// it has none.
pub fn mark_fn(f: &MirFn, target: &Target<'_>) -> Option<MirFn> {
    let mut out = f.clone();
    let mut pass = Pass {
        f,
        target,
        scratch: Vec::new(),
        found: false,
    };
    pass.walk(&mut out.body);
    if !pass.found {
        return None;
    }
    out.repr.scratch = pass.scratch;
    Some(out)
}

struct Pass<'a> {
    f: &'a MirFn,
    target: &'a Target<'a>,
    scratch: Vec<MirScratch>,
    found: bool,
}

impl Pass<'_> {
    fn walk(&mut self, e: &mut Spanned<MirExpr>) {
        if let Some(node) = self.tree(e) {
            self.found = true;
            let words = self.words();
            let mut ahead = 0;
            let root = self.place(node, &mut ahead);
            let line = e.line;
            let marked = Spanned::new(
                MirExpr::IntWord(Box::new(Spanned::new(MirIntWord { root, words }, line))),
                line,
            );
            marked.set_ty(Type::Int);
            *e = marked;
            return;
        }
        walk_children_mut(&mut e.node, &mut |c| self.walk(c));
    }

    /// The tree rooted at `e`, when one is chosen there: an operation with a
    /// field leaf. Leaves are numbered later (`place`).
    fn tree(&self, e: &Spanned<MirExpr>) -> Option<IntWordNode> {
        let node = self.node(e)?;
        let has_field = node
            .leaves()
            .iter()
            .any(|l| matches!(l.node, MirExpr::Project(_)));
        (matches!(node, IntWordNode::Op(..)) && has_field).then_some(node)
    }

    fn node(&self, e: &Spanned<MirExpr>) -> Option<IntWordNode> {
        if !matches!(e.ty(), Some(Type::Int)) {
            return None;
        }
        match &e.node {
            MirExpr::BinOp(b) => {
                let op = match b.node.op {
                    BinOp::Add => IntWordOp::Add,
                    BinOp::Sub => IntWordOp::Sub,
                    BinOp::Mul => IntWordOp::Mul,
                    _ => return None,
                };
                let l = self.node(&b.node.lhs)?;
                let r = self.node(&b.node.rhs)?;
                Some(IntWordNode::Op(op, Box::new(l), Box::new(r)))
            }
            MirExpr::Local(l) if !self.f.repr.slot_is_bare(l.node.slot) => {
                Some(IntWordNode::Local(Box::new(e.clone())))
            }
            MirExpr::Project(p)
                if matches!(&p.node.base.node, MirExpr::Local(l)
                    if !self.f.repr.slot_is_bare_carrier(l.node.slot))
                    && !(self.target.is_word_field)(e) =>
            {
                // Numbered by `place`.
                Some(IntWordNode::Ahead(Box::new(e.clone()), u32::MAX))
            }
            _ => None,
        }
    }

    /// Give the `k`-th field leaf of the tree the `k`-th `Int` scratch local
    /// of the fn (trees share them: each is dead past its tree).
    fn place(&mut self, node: IntWordNode, k: &mut usize) -> IntWordNode {
        match node {
            IntWordNode::Op(op, a, b) => {
                let a = self.place(*a, k);
                let b = self.place(*b, k);
                IntWordNode::Op(op, Box::new(a), Box::new(b))
            }
            IntWordNode::Ahead(e, _) => {
                let slot = self.int_scratch(*k);
                *k += 1;
                IntWordNode::Ahead(e, slot)
            }
            leaf => leaf,
        }
    }

    /// The three word temporaries every tree of the fn shares: scratch 0..3.
    fn words(&mut self) -> u32 {
        if self.scratch.is_empty() {
            self.scratch.extend([MirScratch::Word; 3]);
        }
        0
    }

    /// The scratch index of the `k`-th `Int` scratch local.
    fn int_scratch(&mut self, k: usize) -> u32 {
        let ints: Vec<usize> = (0..self.scratch.len())
            .filter(|i| self.scratch[*i] == MirScratch::Int)
            .collect();
        if let Some(i) = ints.get(k) {
            return *i as u32;
        }
        self.scratch.push(MirScratch::Int);
        (self.scratch.len() - 1) as u32
    }
}
