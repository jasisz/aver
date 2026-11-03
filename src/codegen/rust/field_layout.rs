//! Rust backend: the order a thin constructor's fields are laid out in,
//! chosen from how the program reads them.
//!
//! A thin sum keeps each constructor's fields in a struct of its own, in a
//! node behind one pointer (see `representation::is_thin_sum`), and the
//! struct is `repr(C)`: its fields sit in the order they are written. A
//! match over the sum reads a field at the offset its constructor gives it,
//! so when two constructors keep a field at one offset, the arms that read
//! them the same way compile to one load, where different offsets need a
//! table of offsets by tag, or one arm per constructor. Which fields are
//! read the same way is not visible from the declaration: this pass reads
//! it off the program.
//!
//! **Rule.** Two arms of one match, over two constructors of a thin sum,
//! are *alike* when their bodies are the same expression up to the fields
//! they bind (and up to the constructor of the matched type they build, if
//! any). A field of one arm and the field of the other that stands where it
//! stands are then *read in one role*; each such pair is counted once per
//! match, twice in a recursive function, where the match runs once per node
//! of a value. Fields of one role, and of one type, are grouped, heaviest
//! pairs first, and a group gets one position in every constructor that
//! holds one of its fields. A group can only take one position when the
//! fields before it are of the same types in every one of its constructors
//! (`repr(C)` then gives them one offset): a constructor's other fields fill
//! the places before the group, with the heaviest groups taking the lowest
//! positions. The fields no group placed follow: first the scalars whose
//! bytes decide `==` (a `Bool`, a word held as `i64`, a sum without fields;
//! an `Int`, whose one word equal means equal), one block of memory with no
//! padding inside, so that `==` reads the block of one node against the
//! other's as one wide comparison (LLVM merges the adjacent ones); then the
//! rest, those of the widest alignment first, as Rust lays a struct out, so
//! the struct does not grow for being `repr(C)`. The declared order stays
//! what the language sees: `==`, ordering, hashing and printing go by it,
//! and construction evaluates its arguments in it; only the memory order
//! changes, and the order `==` reads the scalars in, which is the memory
//! one.

use std::collections::{HashMap, HashSet};

use crate::ast::TypeDef;
use crate::codegen::CodegenContext;
use crate::ir::hir::ResolvedCtor;
use crate::ir::mir::{IntWordNode, LocalId, MirCtor, MirExpr, MirPattern, MirStrPart};
use crate::ir::{CtorId, TypeId};
use crate::types::{Type, parse_type_str};

use super::syntax::generated_ident;

/// The positions of a thin sum's fields, by constructor: for each, the
/// physical position of every declared field, or `None` for a constructor
/// laid out as declared.
pub(super) type TypeLayout = Vec<Option<Vec<usize>>>;

/// The layouts of every thin sum whose fields do not all stay in their
/// declared order (see the module's rule).
pub(super) fn compute(ctx: &CodegenContext) -> HashMap<TypeId, TypeLayout> {
    let sums = thin_sums(ctx);
    // The weight of each pair of fields read in one role, by type.
    let mut weights: HashMap<TypeId, HashMap<Pair, u64>> = HashMap::new();
    if let Some(program) = ctx.mir_program.as_ref() {
        for (fn_id, mir_fn) in program.iter() {
            // A match in a recursive function runs once per node it visits.
            let weight = match ctx.recursive_fns.contains(fn_id) {
                true => 2,
                false => 1,
            };
            let mut matches = Vec::new();
            collect_matches(&mir_fn.body.node, &mut matches);
            for m in matches {
                count_match(m, weight, ctx, &sums, &mut weights);
            }
        }
    }
    let mut layouts = HashMap::new();
    for (id, sum) in &sums {
        let pairs = weights.get(id).cloned().unwrap_or_default();
        let layout = place(sum, group(sum, pairs));
        if layout.iter().any(Option::is_some) {
            layouts.insert(*id, layout);
        }
    }
    layouts
}

/// The layout of the sum `td`: empty for a sum laid out as declared.
pub(super) fn of_def<'a>(td: &TypeDef, ctx: &'a CodegenContext) -> &'a [Option<Vec<usize>>] {
    let key = crate::codegen::common::type_key_for_decl(ctx, td);
    ctx.symbol_table
        .type_id_of(&key)
        .and_then(|id| ctx.rust_field_layout.get(&id))
        .map_or(&[], Vec::as_slice)
}

/// The positions of the fields of the constructor `ctor_id`, declared index
/// to physical; `None` when they stay in their declared order.
pub(super) fn of_ctor_id(ctor_id: CtorId, ctx: &CodegenContext) -> Option<&[usize]> {
    let entry = ctx.symbol_table.ctor_entry(ctor_id);
    let layout = ctx.rust_field_layout.get(&entry.owning_type)?;
    let td = super::representation::named_type_def(Some(entry.owning_type), "", ctx, None)?;
    let TypeDef::Sum { variants, .. } = td else {
        return None;
    };
    let index = variants.iter().position(|v| v.name == entry.name)?;
    layout.get(index)?.as_deref()
}

/// [`of_ctor_id`] for a resolved constructor pattern.
pub(super) fn of_ctor<'a>(ctor: &ResolvedCtor, ctx: &'a CodegenContext) -> Option<&'a [usize]> {
    match ctor {
        ResolvedCtor::User { ctor_id, .. } => of_ctor_id(*ctor_id, ctx),
        _ => None,
    }
}

/// The physical position of the declared field `declared` under
/// `positions` (see [`of_ctor_id`]).
pub(super) fn at(positions: Option<&[usize]>, declared: usize) -> usize {
    positions
        .and_then(|p| p.get(declared).copied())
        .unwrap_or(declared)
}

/// The physical order of `count` fields under `positions`: the declared
/// index of the field at each physical position.
pub(super) fn order(positions: Option<&[usize]>, count: usize) -> Vec<usize> {
    let mut order: Vec<usize> = (0..count).collect();
    order.sort_by_key(|&d| at(positions, d));
    order
}

/// A reference to each field of the struct `stem` in declared order,
/// `(&stem.p)`, as `representation::variant_field_refs` writes them for a
/// struct laid out as declared.
pub(super) fn field_refs(stem: &str, count: usize, positions: Option<&[usize]>) -> Vec<String> {
    (0..count)
        .map(|d| format!("(&{stem}.{})", at(positions, d)))
        .collect()
}

/// The struct `payload` built from `args`, written in declared order. The
/// arguments are evaluated in that order, so a struct whose fields are laid
/// out otherwise binds them first.
pub(super) fn construction(payload: &str, args: &[String], positions: Option<&[usize]>) -> String {
    let order = order(positions, args.len());
    if order.iter().enumerate().all(|(p, &d)| p == d) {
        return format!("{payload}({})", args.join(", "));
    }
    let name = |d: usize| generated_ident(&format!("field{d}"));
    let bound: Vec<String> = args
        .iter()
        .enumerate()
        .map(|(d, arg)| format!("let {} = {arg};", name(d)))
        .collect();
    let placed: Vec<String> = order.iter().map(|&d| name(d)).collect();
    format!("{{ {} {payload}({}) }}", bound.join(" "), placed.join(", "))
}

/// What the bytes of a field say of its `==`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum Bitwise {
    /// Equal bytes are equal values, and the other way round: a `Bool`, a
    /// unit, a sum without fields, a record held as a raw `i64`.
    Equal,
    /// Equal bytes are equal values, but two values may be equal with
    /// different bytes: an `Int`, one word inline or a pointer to a heap
    /// integer (`aver_rt::AverInt::same_word`).
    Implies,
    /// The bytes say nothing: a `Float` (`NaN`, the two zeros), a string, a
    /// collection, a node.
    No,
}

/// [`Bitwise`] of a field of `ty`, declared in `scope`.
pub(super) fn bitwise(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> Bitwise {
    match ty {
        Type::Bool | Type::Unit => Bitwise::Equal,
        Type::Int => Bitwise::Implies,
        Type::Named { id, name } => {
            if super::carrier::is_i64_carrier(ctx, *id) {
                return Bitwise::Equal;
            }
            let Some(td) = super::representation::named_type_def(*id, name, ctx, scope) else {
                return Bitwise::No;
            };
            match td {
                TypeDef::Sum { variants, .. }
                    if variants.iter().all(|v| v.fields.is_empty())
                        && !super::representation::is_thin_sum(td, ctx) =>
                {
                    Bitwise::Equal
                }
                _ => Bitwise::No,
            }
        }
        _ => Bitwise::No,
    }
}

/// How the `==` of a constructor's payload reads its fields.
pub(super) struct EqPlan {
    /// The fields in the order they are compared, declared indices: the
    /// scalar block first, in memory order, then the others in the cheap-
    /// first order given.
    pub order: Vec<usize>,
    /// How many fields of `order` make the scalar block.
    pub block: usize,
    /// Whether the block's words are compared first, as one block, with the
    /// field-by-field comparison as the fallback: when a field of the block
    /// may be equal with different words (see [`Bitwise::Implies`]) and the
    /// block has two fields or more to read as one.
    pub words_first: bool,
    /// What the bytes of each declared field say of its `==`.
    pub kinds: Vec<Bitwise>,
}

impl EqPlan {
    /// The fields of the block, declared indices, in memory order.
    pub fn block_fields(&self) -> &[usize] {
        &self.order[..self.block]
    }
}

/// The [`EqPlan`] of a payload of fields `types`, laid out by `positions`,
/// whose fields outside the block `==` compares in `cheap` order (see
/// `representation::cheap_fields_first`), or the declared one.
pub(super) fn eq_plan(
    types: &[&str],
    cheap: Option<&[usize]>,
    positions: Option<&[usize]>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> EqPlan {
    let kinds: Vec<Bitwise> = types
        .iter()
        .map(|ty| bitwise(&parse_type_str(ty), ctx, scope))
        .collect();
    let mut block: Vec<usize> = (0..types.len())
        .filter(|&d| kinds[d] != Bitwise::No)
        .collect();
    block.sort_by_key(|&d| at(positions, d));
    let declared: Vec<usize> = (0..types.len()).collect();
    let rest: Vec<usize> = cheap
        .unwrap_or(&declared)
        .iter()
        .copied()
        .filter(|d| !block.contains(d))
        .collect();
    let words_first = block.len() > 1 && block.iter().any(|&d| kinds[d] == Bitwise::Implies);
    let count = block.len();
    let mut order = block;
    order.extend(rest);
    EqPlan {
        order,
        block: count,
        words_first,
        kinds,
    }
}

/// A thin sum the pass may lay out: its constructors' fields, as Rust
/// types, with their alignment and what their bytes say of `==`.
struct Sum {
    variants: Vec<Variant>,
}

struct Variant {
    types: Vec<String>,
    aligns: Vec<usize>,
    bitwise: Vec<Bitwise>,
}

/// Two fields of two constructors: `(constructor, field)` each, the lower
/// constructor first.
type Pair = ((usize, usize), (usize, usize));

fn thin_sums(ctx: &CodegenContext) -> HashMap<TypeId, Sum> {
    let defs = ctx
        .type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()));
    let mut sums = HashMap::new();
    for td in defs {
        let TypeDef::Sum { variants, .. } = td else {
            continue;
        };
        if !super::representation::is_thin_sum(td, ctx) {
            continue;
        }
        let key = crate::codegen::common::type_key_for_decl(ctx, td);
        let Some(id) = ctx.symbol_table.type_id_of(&key) else {
            continue;
        };
        let scope = key.scope_str();
        let variants = variants
            .iter()
            .map(|v| {
                let parsed: Vec<Type> = v.fields.iter().map(|f| parse_type_str(f)).collect();
                Variant {
                    types: v
                        .fields
                        .iter()
                        .map(|f| super::types::type_annotation_to_rust_scoped(f, ctx, scope))
                        .collect(),
                    aligns: parsed.iter().map(|ty| alignment(ty, ctx, scope)).collect(),
                    bitwise: parsed.iter().map(|ty| bitwise(ty, ctx, scope)).collect(),
                }
            })
            .collect();
        sums.insert(id, Sum { variants });
    }
    sums
}

/// The alignment of a field of `ty` in the Rust it is lowered to: a byte
/// for a `Bool`, a unit or a sum without fields, a machine word for
/// everything else, which the runtime holds in words or behind pointers.
fn alignment(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> usize {
    let word = std::mem::size_of::<usize>();
    match ty {
        Type::Bool | Type::Unit => 1,
        Type::Named { id, .. } if super::carrier::is_i64_carrier(ctx, *id) => word,
        Type::Named { .. } if bitwise(ty, ctx, scope) == Bitwise::Equal => 1,
        _ => word,
    }
}

fn collect_matches<'a>(e: &'a MirExpr, out: &mut Vec<&'a crate::ir::mir::MirMatch>) {
    if let MirExpr::Match(m) = e {
        out.push(&m.node);
    }
    crate::ir::mir::expr::walk_children(e, &mut |child| collect_matches(child, out));
}

/// Count the pairs of fields the alike arms of `m` read in one role.
fn count_match(
    m: &crate::ir::mir::MirMatch,
    weight: u64,
    ctx: &CodegenContext,
    sums: &HashMap<TypeId, Sum>,
    weights: &mut HashMap<TypeId, HashMap<Pair, u64>>,
) {
    // The arms over a constructor of a thin sum: the sum, the constructor's
    // index, the locals bound to its fields, and the body.
    let mut arms = Vec::new();
    for arm in &m.arms {
        let MirPattern::Ctor {
            ctor: MirCtor::User(ctor_id),
            bindings,
            ..
        } = &arm.pattern
        else {
            continue;
        };
        let entry = ctx.symbol_table.ctor_entry(*ctor_id);
        let Some(sum) = sums.get(&entry.owning_type) else {
            continue;
        };
        let Some(variant) = variant_index(*ctor_id, ctx) else {
            continue;
        };
        if sum.variants[variant].types.len() != bindings.len() {
            continue;
        }
        arms.push((
            entry.owning_type,
            variant,
            bindings.as_slice(),
            &arm.body.node,
        ));
    }
    for (i, &(id, va, bound_a, body_a)) in arms.iter().enumerate() {
        for &(other, vb, bound_b, body_b) in &arms[i + 1..] {
            if other != id || va == vb {
                continue;
            }
            let sum = &sums[&id];
            let mut alike = Alike {
                ctx,
                matched: id,
                bound: [bound_a, bound_b],
                types: [&sum.variants[va].types, &sum.variants[vb].types],
                pairs: Vec::new(),
                renames: Vec::new(),
            };
            if !alike.expr(body_a, body_b) {
                continue;
            }
            for &(da, db) in &alike.pairs {
                let pair = match va < vb {
                    true => ((va, da), (vb, db)),
                    false => ((vb, db), (va, da)),
                };
                *weights.entry(id).or_default().entry(pair).or_insert(0) += weight;
            }
        }
    }
}

/// The index of the constructor `ctor_id` among its type's.
fn variant_index(ctor_id: CtorId, ctx: &CodegenContext) -> Option<usize> {
    let entry = ctx.symbol_table.ctor_entry(ctor_id);
    let td = super::representation::named_type_def(Some(entry.owning_type), "", ctx, None)?;
    let TypeDef::Sum { variants, .. } = td else {
        return None;
    };
    variants.iter().position(|v| v.name == entry.name)
}

/// Whether two arm bodies are one expression up to the fields each binds
/// (see the module's rule), and which field stands for which.
struct Alike<'a> {
    ctx: &'a CodegenContext,
    /// The matched sum, whose constructors the bodies may build apart.
    matched: TypeId,
    /// The locals bound to the fields of each arm's constructor.
    bound: [&'a [LocalId]; 2],
    /// The Rust types of the fields of each arm's constructor.
    types: [&'a [String]; 2],
    /// The fields the bodies read in one place: declared index in the first
    /// arm's constructor, and in the second's.
    pairs: Vec<(usize, usize)>,
    /// The locals the bodies bind themselves, paired as they are bound.
    renames: Vec<(LocalId, LocalId)>,
}

impl Alike<'_> {
    fn local(&mut self, x: LocalId, y: LocalId) -> bool {
        if let Some(&(_, bound)) = self.renames.iter().rev().find(|(rx, _)| *rx == x) {
            return bound == y;
        }
        if self.renames.iter().any(|(_, ry)| *ry == y) {
            return false;
        }
        let field = |bound: &[LocalId], local| bound.iter().position(|&b| b == local);
        match (field(self.bound[0], x), field(self.bound[1], y)) {
            (Some(i), Some(j)) => {
                if self.types[0][i] != self.types[1][j] {
                    return false;
                }
                match self.pairs.iter().find(|(a, b)| *a == i || *b == j) {
                    Some(&(a, b)) => a == i && b == j,
                    None => {
                        self.pairs.push((i, j));
                        true
                    }
                }
            }
            (None, None) => x == y,
            _ => false,
        }
    }

    fn exprs(
        &mut self,
        a: &[crate::ast::Spanned<MirExpr>],
        b: &[crate::ast::Spanned<MirExpr>],
    ) -> bool {
        a.len() == b.len() && a.iter().zip(b).all(|(x, y)| self.expr(&x.node, &y.node))
    }

    fn pattern(&mut self, p: &MirPattern, q: &MirPattern) -> bool {
        match (p, q) {
            (MirPattern::Wildcard, MirPattern::Wildcard)
            | (MirPattern::EmptyList, MirPattern::EmptyList) => true,
            (MirPattern::Literal(x), MirPattern::Literal(y)) => x == y,
            (MirPattern::Bind(x, _), MirPattern::Bind(y, _)) => {
                self.renames.push((*x, *y));
                true
            }
            (
                MirPattern::Cons {
                    head: ha, tail: ta, ..
                },
                MirPattern::Cons {
                    head: hb, tail: tb, ..
                },
            ) => {
                self.renames.push((*ha, *hb));
                self.renames.push((*ta, *tb));
                true
            }
            (MirPattern::Tuple(xs), MirPattern::Tuple(ys)) => {
                xs.len() == ys.len() && xs.iter().zip(ys).all(|(x, y)| self.pattern(x, y))
            }
            (
                MirPattern::Ctor {
                    ctor: ca,
                    bindings: xs,
                    ..
                },
                MirPattern::Ctor {
                    ctor: cb,
                    bindings: ys,
                    ..
                },
            ) => {
                if ca != cb || xs.len() != ys.len() {
                    return false;
                }
                self.renames
                    .extend(xs.iter().copied().zip(ys.iter().copied()));
                true
            }
            _ => false,
        }
    }

    /// Whether `a` and `b` build one constructor, or two of the matched sum.
    fn ctor(&self, a: MirCtor, b: MirCtor) -> bool {
        a == b
            || match (a, b) {
                (MirCtor::User(x), MirCtor::User(y)) => {
                    let owner = |id| self.ctx.symbol_table.ctor_entry(id).owning_type;
                    owner(x) == self.matched && owner(y) == self.matched
                }
                _ => false,
            }
    }

    fn expr(&mut self, a: &MirExpr, b: &MirExpr) -> bool {
        match (a, b) {
            (MirExpr::Literal(x), MirExpr::Literal(y)) => x.node == y.node,
            (MirExpr::Local(x), MirExpr::Local(y)) => self.local(x.node.slot, y.node.slot),
            (MirExpr::Let(x), MirExpr::Let(y)) => {
                self.expr(&x.node.value.node, &y.node.value.node) && {
                    self.renames.push((x.node.binding, y.node.binding));
                    self.expr(&x.node.body.node, &y.node.body.node)
                }
            }
            (MirExpr::Call(x), MirExpr::Call(y)) => {
                x.node.callee == y.node.callee && self.exprs(&x.node.args, &y.node.args)
            }
            (MirExpr::TailCall(x), MirExpr::TailCall(y)) => {
                x.node.target == y.node.target && self.exprs(&x.node.args, &y.node.args)
            }
            (MirExpr::BinOp(x), MirExpr::BinOp(y)) => {
                x.node.op == y.node.op
                    && self.expr(&x.node.lhs.node, &y.node.lhs.node)
                    && self.expr(&x.node.rhs.node, &y.node.rhs.node)
            }
            (MirExpr::Neg(x), MirExpr::Neg(y))
            | (MirExpr::Try(x), MirExpr::Try(y))
            | (MirExpr::Return(x), MirExpr::Return(y))
            | (MirExpr::Box(x), MirExpr::Box(y))
            | (MirExpr::Unbox(x), MirExpr::Unbox(y)) => self.expr(&x.node, &y.node),
            (MirExpr::Match(x), MirExpr::Match(y)) => {
                self.expr(&x.node.subject.node, &y.node.subject.node)
                    && x.node.arms.len() == y.node.arms.len()
                    && x.node.arms.iter().zip(&y.node.arms).all(|(p, q)| {
                        self.pattern(&p.pattern, &q.pattern)
                            && self.expr(&p.body.node, &q.body.node)
                    })
            }
            (MirExpr::Construct(x), MirExpr::Construct(y)) => {
                self.ctor(x.node.ctor, y.node.ctor) && self.exprs(&x.node.args, &y.node.args)
            }
            (MirExpr::RecordCreate(x), MirExpr::RecordCreate(y)) => {
                x.node.type_name == y.node.type_name
                    && x.node.fields.len() == y.node.fields.len()
                    && x.node
                        .fields
                        .iter()
                        .zip(&y.node.fields)
                        .all(|(p, q)| p.name == q.name && self.expr(&p.value.node, &q.value.node))
            }
            (MirExpr::RecordUpdate(x), MirExpr::RecordUpdate(y)) => {
                x.node.type_name == y.node.type_name
                    && self.expr(&x.node.base.node, &y.node.base.node)
                    && x.node.updates.len() == y.node.updates.len()
                    && x.node
                        .updates
                        .iter()
                        .zip(&y.node.updates)
                        .all(|(p, q)| p.name == q.name && self.expr(&p.value.node, &q.value.node))
            }
            (MirExpr::Project(x), MirExpr::Project(y)) => {
                x.node.field == y.node.field && self.expr(&x.node.base.node, &y.node.base.node)
            }
            (MirExpr::IfThenElse(x), MirExpr::IfThenElse(y)) => {
                self.expr(&x.node.cond.node, &y.node.cond.node)
                    && self.expr(&x.node.then_branch.node, &y.node.then_branch.node)
                    && self.expr(&x.node.else_branch.node, &y.node.else_branch.node)
            }
            (MirExpr::List(xs), MirExpr::List(ys)) | (MirExpr::Tuple(xs), MirExpr::Tuple(ys)) => {
                self.exprs(xs, ys)
            }
            (MirExpr::MapLiteral(xs), MirExpr::MapLiteral(ys)) => {
                xs.len() == ys.len()
                    && xs.iter().zip(ys).all(|((ka, va), (kb, vb))| {
                        self.expr(&ka.node, &kb.node) && self.expr(&va.node, &vb.node)
                    })
            }
            (MirExpr::InterpolatedStr(xs), MirExpr::InterpolatedStr(ys)) => {
                xs.len() == ys.len()
                    && xs.iter().zip(ys).all(|(p, q)| match (p, q) {
                        (MirStrPart::Literal(s), MirStrPart::Literal(t)) => s == t,
                        (MirStrPart::Expr(e), MirStrPart::Expr(f)) => self.expr(&e.node, &f.node),
                        _ => false,
                    })
            }
            (MirExpr::IndependentProduct(x), MirExpr::IndependentProduct(y)) => {
                x.node.unwrap_results == y.node.unwrap_results
                    && self.exprs(&x.node.items, &y.node.items)
            }
            (MirExpr::FnValue(x), MirExpr::FnValue(y)) => x == y,
            (MirExpr::IntWord(x), MirExpr::IntWord(y)) => {
                x.node.words == y.node.words
                    && format!("{:?}", x.node.kind) == format!("{:?}", y.node.kind)
                    && self.word(&x.node.root, &y.node.root)
            }
            _ => false,
        }
    }

    fn word(&mut self, a: &IntWordNode, b: &IntWordNode) -> bool {
        match (a, b) {
            (
                IntWordNode::Op {
                    op: oa,
                    args: xs,
                    shell: sa,
                },
                IntWordNode::Op {
                    op: ob,
                    args: ys,
                    shell: sb,
                },
            ) => {
                format!("{oa:?}") == format!("{ob:?}")
                    && xs.len() == ys.len()
                    && xs.iter().zip(ys).all(|(x, y)| self.word(x, y))
                    && self.expr(&sa.node, &sb.node)
            }
            (IntWordNode::Lit(x), IntWordNode::Lit(y))
            | (IntWordNode::Local(x), IntWordNode::Local(y)) => self.expr(&x.node, &y.node),
            (IntWordNode::Ahead(x, nx, hx), IntWordNode::Ahead(y, ny, hy)) => {
                nx == ny && hx == hy && self.expr(&x.node, &y.node)
            }
            _ => false,
        }
    }
}

/// A set of fields, at most one per constructor, read in one role.
struct Group {
    members: Vec<(usize, usize)>,
    weight: u64,
}

/// The groups of fields of `sum` read in one role, from the weighted
/// pairs: heaviest pairs first, two groups join when no constructor is in
/// both.
fn group(sum: &Sum, pairs: HashMap<Pair, u64>) -> Vec<Group> {
    let mut pairs: Vec<(Pair, u64)> = pairs.into_iter().collect();
    pairs.sort_by(|(pa, wa), (pb, wb)| wb.cmp(wa).then(pa.cmp(pb)));
    let mut groups: Vec<Option<Group>> = Vec::new();
    let mut of: HashMap<(usize, usize), usize> = HashMap::new();
    for ((a, b), weight) in pairs {
        if sum.variants[a.0].types[a.1] != sum.variants[b.0].types[b.1] {
            continue;
        }
        let ga = of.get(&a).copied();
        let gb = of.get(&b).copied();
        match (ga, gb) {
            (Some(x), Some(y)) if x == y => groups[x].as_mut().unwrap().weight += weight,
            (Some(x), Some(y)) => {
                let moved = groups[y].take().unwrap();
                let into = groups[x].as_mut().unwrap();
                let shared = moved
                    .members
                    .iter()
                    .any(|(v, _)| into.members.iter().any(|(w, _)| w == v));
                match shared {
                    true => groups[y] = Some(moved),
                    false => {
                        for m in &moved.members {
                            of.insert(*m, x);
                        }
                        into.members.extend(moved.members);
                        into.weight += moved.weight + weight;
                    }
                }
            }
            (Some(x), None) | (None, Some(x)) => {
                let (new, old) = match ga.is_some() {
                    true => (b, a),
                    false => (a, b),
                };
                let into = groups[x].as_mut().unwrap();
                debug_assert!(into.members.contains(&old));
                if into.members.iter().any(|(v, _)| *v == new.0) {
                    continue;
                }
                into.members.push(new);
                into.weight += weight;
                of.insert(new, x);
            }
            (None, None) => {
                of.insert(a, groups.len());
                of.insert(b, groups.len());
                groups.push(Some(Group {
                    members: vec![a, b],
                    weight,
                }));
            }
        }
    }
    let mut groups: Vec<Group> = groups.into_iter().flatten().collect();
    for g in &mut groups {
        g.members.sort();
    }
    groups.sort_by(|a, b| b.weight.cmp(&a.weight).then(a.members.cmp(&b.members)));
    groups
}

/// The physical positions of the fields of `sum` with `groups` placed (see
/// the module's rule): by constructor, declared index to physical position,
/// `None` where that is the identity.
fn place(sum: &Sum, groups: Vec<Group>) -> TypeLayout {
    let grouped: HashSet<(usize, usize)> = groups
        .iter()
        .flat_map(|g| g.members.iter().copied())
        .collect();
    let mut slots: Vec<Vec<Option<usize>>> = sum
        .variants
        .iter()
        .map(|v| vec![None; v.types.len()])
        .collect();
    let placed = |slots: &[Vec<Option<usize>>], (v, d): (usize, usize)| slots[v].contains(&Some(d));
    for g in groups {
        let mut members: Vec<(usize, usize)> = g
            .members
            .iter()
            .copied()
            .filter(|m| !placed(&slots, *m))
            .collect();
        while members.len() > 1 {
            if let Some((p, fillers)) = position_for(sum, &slots, &grouped, &members) {
                for &(v, d) in &members {
                    slots[v][p] = Some(d);
                }
                for (v, q, d) in fillers {
                    slots[v][q] = Some(d);
                }
                break;
            }
            members.pop();
        }
    }
    sum.variants
        .iter()
        .enumerate()
        .map(|(v, variant)| {
            // The scalars first, one block, then the rest; the widest
            // alignment first in each, then as declared.
            let mut rest: Vec<usize> = (0..variant.types.len())
                .filter(|&d| !placed(&slots, (v, d)))
                .collect();
            rest.sort_by_key(|&d| {
                (
                    variant.bitwise[d] == Bitwise::No,
                    std::cmp::Reverse(variant.aligns[d]),
                    d,
                )
            });
            let mut rest = rest.into_iter();
            let order: Vec<usize> = slots[v]
                .iter()
                .map(|slot| slot.unwrap_or_else(|| rest.next().unwrap()))
                .collect();
            let mut positions = vec![0; order.len()];
            for (p, &d) in order.iter().enumerate() {
                positions[d] = p;
            }
            positions
                .iter()
                .enumerate()
                .any(|(d, &p)| d != p)
                .then_some(positions)
        })
        .collect()
}

/// A field moved into a place a constructor has none at yet: the
/// constructor, the position and the declared field.
type Filler = (usize, usize, usize);

/// The lowest position the fields `members` can share, and the fields that
/// fill the places before it where a constructor has none yet: for each, the
/// constructor, the position and the declared field.
fn position_for(
    sum: &Sum,
    slots: &[Vec<Option<usize>>],
    grouped: &HashSet<(usize, usize)>,
    members: &[(usize, usize)],
) -> Option<(usize, Vec<Filler>)> {
    let ty = |(v, d): (usize, usize)| sum.variants[v].types[d].as_str();
    let width = members
        .iter()
        .map(|&(v, _)| slots[v].len())
        .min()
        .unwrap_or(0);
    let mut fillers: Vec<Filler> = Vec::new();
    for p in 0..width {
        if members.iter().all(|&(v, _)| slots[v][p].is_none()) {
            return Some((p, fillers));
        }
        // Position `p` is taken in some constructor: the group goes after
        // it, and every constructor needs a field of one type at it.
        let filled: Vec<&str> = members
            .iter()
            .filter_map(|&(v, _)| slots[v][p].map(|d| ty((v, d))))
            .collect();
        let template = match filled.first() {
            Some(first) => {
                if filled.iter().any(|t| t != first) {
                    return None;
                }
                first.to_string()
            }
            None => unreachable!("some constructor fills position {p}"),
        };
        for &(v, d) in members {
            if slots[v][p].is_some() {
                continue;
            }
            // A free field of the template's type, one no group wants
            // first, then the first declared.
            let free = (0..slots[v].len()).filter(|&f| {
                f != d
                    && ty((v, f)) == template
                    && !slots[v].contains(&Some(f))
                    && !fillers.iter().any(|&(w, _, g)| w == v && g == f)
            });
            let filler = free.min_by_key(|&f| (grouped.contains(&(v, f)), f))?;
            fillers.push((v, p, filler));
        }
    }
    None
}
