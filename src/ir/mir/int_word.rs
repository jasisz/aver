//! The `Int` trees a function computes on the machine word, made explicit.
//!
//! This pass is where the choice is made: it rewrites each chosen tree to a
//! [`MirExpr::IntWord`] node naming its operations, its leaves, where each
//! leaf is read, what the node gives its parent (the `Int`, its machine word
//! under an `Unbox`, or a comparison of two trees), and the scratch locals
//! it uses, declared in
//! [`MirFnRepr::scratch`](super::program::MirFnRepr::scratch). The wasm-gc
//! emitter lowers the node as it is, the certificate plan printer prints it
//! as it is, and the checker's wall lowers the printed node and proves it
//! computes the tree's value; none of them chooses again.
//!
//! A tree is a nest of the pure, total `Int` operations of
//! [`super::int_tree`] (`+`, `-`, `*`, negation, `Int.abs`, `Int.min`,
//! `Int.max`, the `Bits` operations, and division, remainder and shifts by a
//! literal). Its leaves are literals, `Int` locals (read in place), and any
//! other `Int` expression, evaluated once, in source order, ahead of the
//! tree into a scratch local; the operations are pure and total, so running
//! them after the leaves, or twice, cannot be observed.
//!
//! The choice, per [`Target`]:
//!
//! - a value is computed on the word when the tree has an operation;
//! - the operand of an `Unbox` also when it is a call of a fn with an `Int`
//!   twin (its result comes as a word);
//! - a comparison when a side has an operation, or neither side is a
//!   literal (a literal side already compares without a call), or a side is
//!   a local the twin being emitted holds as a word, or a side calls an
//!   `Int` twin.
//!
//! A leaf the target renders as a raw `i64` where the tree wants an `Int`
//! stops the tree. In a function a `--certify` build keeps for its
//! certificate ([`Target::kept`]) the trees are the ones the wall proves:
//! values of `+`, `-` and `*` over locals and fields of records held in
//! locals, with a field among the leaves (a tree of locals only would cost a
//! countdown its termination claim, `GrammarTotal`), a field of a word
//! record never a leaf; any other node is left as it is there.

use crate::ast::{BinOp, Spanned};

use super::expr::{
    IntWordHold, IntWordKind, IntWordNode, IntWordOp, MirBinOp, MirExpr, MirIntWord,
    int_word_shell, walk_children_mut,
};
use super::int_tree::{self, int_literal, is_int};
use super::program::{MirFn, MirScratch};

/// The facts the choice reads about the target's representation.
pub struct Target<'a> {
    /// The fn is kept by a `--certify` build for its certificate: only the
    /// trees the wall proves.
    pub kept: bool,
    /// The name of each `BuiltinId`.
    pub builtins: &'a [String],
    /// Whether a field read is of a record the target keeps as the machine
    /// word of its one `Int` field (a word record).
    pub is_word_field: &'a dyn Fn(&Spanned<MirExpr>) -> bool,
    /// Whether an expression renders a raw `i64` (the `bare_i64` facts).
    pub renders_raw: &'a dyn Fn(&MirExpr) -> bool,
    /// Whether a binary operation is computed on raw `i64`s.
    pub binop_is_raw: &'a dyn Fn(&MirBinOp) -> bool,
    /// Whether a field read reads a word record's machine word.
    pub raw_field: &'a dyn Fn(&MirExpr) -> bool,
    /// Whether an expression calls a fn with an `Int` twin the fn may call.
    pub int_twin_call: &'a dyn Fn(&Spanned<MirExpr>) -> bool,
    /// Whether the twin being emitted holds the local in a slot as a word.
    pub held: &'a dyn Fn(u32) -> bool,
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

type Ops<'e> = (IntWordOp, Vec<&'e Spanned<MirExpr>>);

impl Pass<'_> {
    fn walk(&mut self, e: &mut Spanned<MirExpr>) {
        if let Some((root, kind)) = self.choose(e) {
            let ty = e.ty().cloned();
            *e = self.node(e.line, root, kind);
            if let Some(ty) = ty {
                e.set_ty(ty);
            }
            return;
        }
        if let MirExpr::Unbox(inner) = &mut e.node
            && let Some(root) = self.word_root(inner)
        {
            let ty = inner.ty().cloned();
            **inner = self.node(inner.line, root, IntWordKind::Word);
            if let Some(ty) = ty {
                inner.set_ty(ty);
            }
            return;
        }
        walk_children_mut(&mut e.node, &mut |c| self.walk(c));
    }

    /// The node for `root` and `kind`, its leaves numbered and its
    /// leaves read ahead walked in turn.
    fn node(
        &mut self,
        line: crate::ast::SourceLine,
        root: IntWordNode,
        kind: IntWordKind,
    ) -> Spanned<MirExpr> {
        self.found = true;
        let words = self.words();
        let mut ahead = 0;
        let root = self.place(root, &mut ahead);
        let kind = match kind {
            IntWordKind::Compare { op, rhs, shell } => IntWordKind::Compare {
                op,
                rhs: self.place(rhs, &mut ahead),
                shell,
            },
            k => k,
        };
        let mut word = MirIntWord { root, kind, words };
        self.walk_ahead(&mut word.root);
        if let IntWordKind::Compare { rhs, .. } = &mut word.kind {
            self.walk_ahead(rhs);
        }
        Spanned::new(MirExpr::IntWord(Box::new(Spanned::new(word, line))), line)
    }

    /// A leaf read ahead is emitted as any expression: its own trees.
    fn walk_ahead(&mut self, node: &mut IntWordNode) {
        match node {
            IntWordNode::Op { args, .. } => {
                for a in args {
                    self.walk_ahead(a);
                }
            }
            IntWordNode::Ahead(e, ..) => self.walk(e),
            IntWordNode::Lit(_) | IntWordNode::Local(_) => {}
        }
    }

    /// The tree rooted at `e`, when one is chosen there: a value, or a
    /// comparison of two.
    fn choose(&self, e: &Spanned<MirExpr>) -> Option<(IntWordNode, IntWordKind)> {
        if self.ops(e).is_some() {
            let tree = (self.weight(e)? > 0).then(|| self.tree(e))?;
            // In a kept fn, a tree of locals only is left as it is: the
            // wall's termination grammar (`GrammarTotal.totE`) reads plain
            // arithmetic, so a countdown would lose its totality claim, and
            // the tree saves no field read.
            let field = tree
                .leaves()
                .iter()
                .any(|l| matches!(l.node, MirExpr::Project(_)));
            if self.target.kept && !field {
                return None;
            }
            return Some((tree, IntWordKind::Value));
        }
        let MirExpr::BinOp(b) = &e.node else {
            return None;
        };
        let bop = &b.node;
        let compare = matches!(
            bop.op,
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
        );
        if self.target.kept || !compare || !is_int(&bop.lhs) || (self.target.binop_is_raw)(bop) {
            return None;
        }
        let sides = [&*bop.lhs, &*bop.rhs];
        let w = self.weight(sides[0])? + self.weight(sides[1])?;
        let held = |s: &&Spanned<MirExpr>| matches!(&s.node, MirExpr::Local(l) if (self.target.held)(l.node.slot.0));
        let leaves = sides.iter().all(|s| int_literal(&s.node).is_none()) || sides.iter().any(held);
        let twin = sides.iter().any(|s| self.twin_leaf(s));
        (w > 0 || leaves || twin).then(|| {
            let kind = IntWordKind::Compare {
                op: bop.op,
                rhs: self.tree(sides[1]),
                shell: int_word_shell(e, 2),
            };
            (self.tree(sides[0]), kind)
        })
    }

    /// The tree of the operand of an `Unbox`, when its machine word is
    /// computed on the word.
    fn word_root(&self, inner: &Spanned<MirExpr>) -> Option<IntWordNode> {
        if self.target.kept {
            return None;
        }
        let w = self.weight(inner)?;
        (w > 0 || self.twin_leaf(inner)).then(|| self.tree(inner))
    }

    /// The operation `e` is, with its operands, when the target computes it
    /// on `Int`s (not on raw `i64`s already).
    fn ops<'e>(&self, e: &'e Spanned<MirExpr>) -> Option<Ops<'e>> {
        let (op, a, b) = int_tree::op_of(e, self.target.builtins)?;
        let raw = match &e.node {
            MirExpr::BinOp(b) => {
                (self.target.binop_is_raw)(&b.node) || (self.target.renders_raw)(&e.node)
            }
            MirExpr::Neg(x) => (self.target.renders_raw)(&x.node),
            _ => false,
        };
        if raw || (self.target.kept && op.bin_op().is_none()) {
            return None;
        }
        Some((op, std::iter::once(a).chain(b).collect()))
    }

    /// The number of operations of the tree at `e`; `None` when a leaf
    /// cannot be one.
    fn weight(&self, e: &Spanned<MirExpr>) -> Option<usize> {
        match self.ops(e) {
            Some((_, args)) => args
                .iter()
                .map(|a| self.weight(a))
                .sum::<Option<usize>>()
                .map(|w| w + 1),
            None => self.leaf(e).map(|_| 0),
        }
    }

    fn twin_leaf(&self, e: &Spanned<MirExpr>) -> bool {
        match self.ops(e) {
            Some((_, args)) => args.iter().any(|a| self.twin_leaf(a)),
            None => (self.target.int_twin_call)(e),
        }
    }

    /// `e` as a leaf: `None` when it cannot be one.
    fn leaf(&self, e: &Spanned<MirExpr>) -> Option<IntWordNode> {
        let t = self.target;
        if t.kept {
            return match &e.node {
                MirExpr::Local(l) if !self.f.repr.slot_is_bare(l.node.slot) => {
                    Some(IntWordNode::Local(Box::new(e.clone())))
                }
                MirExpr::Project(p)
                    if matches!(&p.node.base.node, MirExpr::Local(l)
                        if !self.f.repr.slot_is_bare_carrier(l.node.slot))
                        && !(t.is_word_field)(e) =>
                {
                    Some(IntWordNode::Ahead(
                        Box::new(e.clone()),
                        u32::MAX,
                        IntWordHold::Int,
                    ))
                }
                _ => None,
            };
        }
        if (t.renders_raw)(&e.node) {
            return None;
        }
        let ahead = |hold| Some(IntWordNode::Ahead(Box::new(e.clone()), u32::MAX, hold));
        match &e.node {
            _ if int_literal(&e.node).is_some() => Some(IntWordNode::Lit(Box::new(e.clone()))),
            MirExpr::Local(l) if !self.f.repr.slot_is_bare(l.node.slot) => {
                Some(IntWordNode::Local(Box::new(e.clone())))
            }
            MirExpr::Box(inner) if (t.renders_raw)(&inner.node) => ahead(IntWordHold::RawBox),
            _ if (t.raw_field)(&e.node) => ahead(IntWordHold::RawField),
            _ => ahead(IntWordHold::Int),
        }
    }

    /// The tree at `e` (its weight is known).
    fn tree(&self, e: &Spanned<MirExpr>) -> IntWordNode {
        match self.ops(e) {
            Some((op, args)) => IntWordNode::Op {
                op,
                shell: int_word_shell(e, args.len()),
                args: args.into_iter().map(|a| self.tree(a)).collect(),
            },
            None => self.leaf(e).expect("a tree's leaves are leaves"),
        }
    }

    /// Give each leaf read ahead its scratch local: in a kept fn the
    /// `k`-th leaf of a tree the `k`-th `Int` scratch local (trees share
    /// them: each is dead past its tree), elsewhere a scratch local of its
    /// own, of the kind it is held as.
    fn place(&mut self, node: IntWordNode, k: &mut usize) -> IntWordNode {
        match node {
            IntWordNode::Op { op, args, shell } => IntWordNode::Op {
                op,
                args: args.into_iter().map(|a| self.place(a, k)).collect(),
                shell,
            },
            IntWordNode::Ahead(e, _, hold) => {
                let slot = if self.target.kept {
                    self.int_scratch(*k)
                } else {
                    self.scratch.push(match hold {
                        IntWordHold::Int => MirScratch::Int,
                        IntWordHold::RawBox | IntWordHold::RawField => MirScratch::Word,
                    });
                    (self.scratch.len() - 1) as u32
                };
                *k += 1;
                IntWordNode::Ahead(e, slot, hold)
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
