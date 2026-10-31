//! Structural `==` as ordinary functions of the program.
//!
//! Aver's `==` on a record, a sum, an `Option`, a `Result` or a tuple means
//! "same constructor and equal fields". This backend used to write the
//! comparison of each such type straight into bytes. Here it is written as
//! a function of the program instead, one per type, which goes through MIR
//! and the body emitter like every other function, so the certificate plan
//! of a function that compares such values is its MIR as for any other:
//!
//! ```text
//! fn __eq_Level(a: Level, b: Level) -> Bool
//!     match a
//!         Level.Zero -> match b
//!             Level.Zero -> true
//!             _ -> false
//!         Level.Succ(x) -> match b
//!             Level.Succ(y) -> __eqnode_Level(x, y)
//!             _ -> false
//!         ...
//! ```
//!
//! Every `==` and `!=` on such a type becomes a call of its function (`!=`
//! under `Bool.not`), `List.contains` over a list of such a type a call of
//! `__contains_<T>`, and `==` on such a list a call of `__eq_List<T>`.
//!
//! Fields are compared in the order of `equality_order`, cheap ones first,
//! and the comparison stops at the first field that differs. A field of a
//! primitive type compares with `==` on it (a `Float` with `f64.eq`, so a
//! value holding NaN is not equal to itself, and `-0.0 == 0.0`), a job
//! handle by reference, a `Unit` not at all.
//!
//! ## Values that share subterms
//!
//! A value of a recursive type can hold one subterm under many nodes, and
//! two equal values built apart are different objects all the way down: a
//! comparison node by node meets a shared subterm once for every path to
//! it, which is exponential in the depth of the sharing. A type that
//! reaches itself through its fields, directly or through an `Option`
//! (`eq_helpers::self_reaching`), therefore has two functions:
//!
//! - `__eq_<T>`, where a comparison starts, compares the two roots and
//!   holds the comparison of their fields of such types in a scope
//!   (`__eq_scope`), which counts the nodes compared within it from zero,
//!   as the byte helpers gave every comparison its own budget;
//! - `__eqnode_<T>` compares every node after the first, and counts it as
//!   one step of the scope (`__eq_count`). Around the fields of a node that
//!   holds such a type it puts `__eq_memo`: once the scope has counted more
//!   than its budget of nodes, the pair is looked up among
//!   the pairs remembered as equal, and remembered when its fields are
//!   found equal. A pair found there answers `true` without comparing
//!   again, so from then on the comparison costs the graph, not the tree.
//!
//! A sum's constructors are tested in the order of their names, as the byte
//! helpers tested them; a common constructor further down the list costs
//! every comparison of it.
//!
//! The scope that starts outside every other one forgets the remembered
//! pairs when it ends. A pair is remembered only after its fields were
//! found equal, and found again only by `ref.eq` on both nodes; values do
//! not change, so a pair found there is equal. Unlike the byte helpers this
//! replaced, a comparison past its budget does not start again from the
//! roots: it goes on, remembering from there.
//!
//! The certificate wall has no node for `__eq_same_ref`, `__eq_count`,
//! `__eq_scope` or `__eq_memo` yet, so the plan printer declines a function
//! that holds one: `==` on a type that reaches itself is not certified, `==`
//! on any other record, sum or carrier is, as an ordinary function.
//!
//! A type with no `Float` under it also answers `true` for one object
//! against itself (`__eq_same_ref`) before it looks at its fields, at the
//! root and at every node; a type with a `Float` under it never does,
//! because such a value need not equal itself.
//!
//! Only this backend writes these functions. The VM and the Rust backend
//! keep their own structural equality, with their own memory of pairs.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use crate::ast::{AnnotBool, BinOp, FnResolution, Literal, Shared, Spanned, Type};
use crate::ir::hir::{
    BuiltinCtor, BuiltinIntrinsic, ResolveCtx, ResolvedCallee, ResolvedCtor, ResolvedExpr,
    ResolvedFnBody, ResolvedFnDef, ResolvedMatchArm, ResolvedPattern, ResolvedStmt,
};
use crate::ir::mir::{MirCall, MirCallee, MirExpr, MirProgram};
use crate::ir::{FnId, SymbolTable};

use super::body::eq_helpers::{EqHelperRegistry, EqKind};
use super::types::TypeRegistry;

/// Nodes a scope compares before it looks pairs up in the table of
/// remembered ones (`__eq_memo`).
pub(crate) const EQ_BUDGET: i32 = 1024;

/// The functions structural `==` runs as, by the canonical name of the
/// type they compare (whitespace-free, as `eq_helpers` names it).
#[derive(Debug, Default, Clone)]
pub(crate) struct EqFns {
    /// Where a comparison of a value of the type starts.
    pub(crate) entry: BTreeMap<String, FnId>,
    /// For a type that reaches itself: every node after the first, within
    /// the budget.
    pub(crate) node: BTreeMap<String, FnId>,
    /// For a type that reaches itself: every node once the budget is spent,
    /// looked up among and remembered with the pairs found equal.
    pub(crate) mem: BTreeMap<String, FnId>,
    /// `List.contains` over a list of the type, by the element type.
    pub(crate) contains: BTreeMap<String, FnId>,
    /// `==` on `List<T>`, by `List<T>`.
    pub(crate) lists: BTreeMap<String, FnId>,
}

impl EqFns {
    /// Whether any comparison walks a type that reaches itself, so the
    /// table of remembered pairs is needed.
    pub(crate) fn walks(&self) -> bool {
        !self.node.is_empty()
    }
}

/// The functions, as the backend's input takes them: one AST shell (name,
/// signature) and one resolved definition each, position-aligned.
pub(crate) struct Synthesized {
    pub(crate) fn_defs: Vec<crate::ast::FnDef>,
    pub(crate) resolved: Vec<ResolvedFnDef>,
    pub(crate) fns: EqFns,
}

/// What a function compares in: the plain comparison of a type that does
/// not reach itself, or the start or a node of one that does.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Role {
    Plain,
    Entry,
    Node,
    Mem,
}

/// Canonical (whitespace-free) spelling of a type string.
fn canonical(ty: &str) -> String {
    super::types::normalize_compound(ty.trim())
}

/// The types `==`, `!=` and `List.contains` are used on in `program`:
/// the element types of `List.contains` and the `List<T>` operands of
/// `==` / `!=`, as canonical strings.
pub(crate) fn uses_in(program: &MirProgram) -> (BTreeSet<String>, BTreeSet<String>) {
    let mut contains = BTreeSet::new();
    let mut lists = BTreeSet::new();
    let mut fns: Vec<_> = program.fns.values().collect();
    fns.sort_by_key(|f| f.fn_id);
    for f in fns {
        visit(&f.body.node, &mut |e| match e {
            MirExpr::BinOp(b) if matches!(b.node.op, BinOp::Eq | BinOp::Neq) => {
                if let Some(ty @ Type::List(_)) = b.node.lhs.ty() {
                    lists.insert(canonical(&ty.display()));
                }
            }
            MirExpr::Call(c) => {
                if let MirCallee::Builtin(id) = c.node.callee
                    && program.builtin_name(id) == "List.contains"
                    && let Some(Type::List(elem)) = c.node.args.first().and_then(|a| a.ty())
                {
                    contains.insert(canonical(&elem.display()));
                }
            }
            _ => {}
        });
    }
    (contains, lists)
}

fn visit(e: &MirExpr, f: &mut dyn FnMut(&MirExpr)) {
    f(e);
    crate::ir::mir::expr::walk_children(e, &mut |child| visit(child, f));
}

/// What `==` on a field of one type does.
enum FieldEq {
    /// One of the functions written here, by its canonical type.
    Function(String),
    /// Nothing to compare (`Unit`).
    Always,
    /// The same object (a job handle).
    SameRef,
    /// `==` on the field's own type, as the emitter lowers it.
    Op,
}

struct Writer<'a> {
    eq: &'a EqHelperRegistry,
    registry: &'a TypeRegistry,
    symbols: &'a SymbolTable,
    ctx: &'a ResolveCtx<'a>,
    /// The types compared by a function written here.
    covered: BTreeSet<String>,
    /// The types that reach themselves.
    walks: HashSet<String>,
    fns: EqFns,
}

impl Writer<'_> {
    fn ty(&self, text: &str) -> Option<Type> {
        let parsed = crate::types::parse_type_str_strict(text).ok()?;
        Some(crate::ir::hir::resolve::canonicalise_type(self.ctx, parsed))
    }

    fn field_eq(&self, field_ty: &str) -> FieldEq {
        let declared = field_ty.trim();
        if declared == "Unit" {
            return FieldEq::Always;
        }
        if declared == crate::capability::work::WORK_JOB {
            return FieldEq::SameRef;
        }
        if self.registry.is_eligible_carrier(declared)
            || self.registry.newtype_underlying(declared).is_some()
        {
            return FieldEq::Op;
        }
        let key = canonical(declared);
        if self.covered.contains(&key) {
            return FieldEq::Function(key);
        }
        // A qualified spelling of a flattened type's bare name.
        if let Some((_, bare)) = key.rsplit_once('.')
            && !bare.contains('<')
            && self.covered.contains(bare)
        {
            return FieldEq::Function(bare.to_string());
        }
        FieldEq::Op
    }

    /// The function a comparison in `role` calls for a value of `name`.
    fn callee(&self, name: &str, role: Role) -> FnId {
        match (role, self.fns.node.get(name), self.fns.mem.get(name)) {
            (Role::Entry | Role::Node, Some(node), _) => *node,
            (Role::Mem, _, Some(mem)) => *mem,
            _ => self.fns.entry[name],
        }
    }
}

/// One function being written: its slots and their types.
struct Body {
    names: Vec<String>,
    types: Vec<Type>,
}

impl Body {
    fn new(params: &[(&str, Type)]) -> Self {
        let mut body = Body {
            names: Vec::new(),
            types: Vec::new(),
        };
        for (name, ty) in params {
            body.bind(name, ty.clone());
        }
        body
    }

    fn bind(&mut self, prefix: &str, ty: Type) -> u16 {
        let slot = self.names.len() as u16;
        self.names.push(format!("{prefix}{slot}"));
        self.types.push(ty);
        slot
    }

    fn local(&self, slot: u16) -> Spanned<ResolvedExpr> {
        typed(
            ResolvedExpr::Resolved {
                slot,
                name: self.names[slot as usize].clone(),
                last_use: AnnotBool(false),
            },
            self.types[slot as usize].clone(),
        )
    }

    fn resolution(&self) -> FnResolution {
        let n = self.names.len();
        FnResolution {
            local_count: n as u16,
            local_slots: std::sync::Arc::new(
                self.names
                    .iter()
                    .enumerate()
                    .map(|(i, name)| (name.clone(), i as u16))
                    .collect(),
            ),
            stmt_binding_slots: std::sync::Arc::new(Vec::new()),
            local_slot_types: std::sync::Arc::new(self.types.clone()),
            aliased_slots: std::sync::Arc::new(vec![false; n]),
        }
    }
}

/// One slot for each field `keep` marks, none (`_`) for the others.
fn bind_fields(body: &mut Body, prefix: &str, tys: &[Type], keep: &[bool]) -> Vec<Option<u16>> {
    tys.iter()
        .zip(keep)
        .map(|(t, k)| k.then(|| body.bind(prefix, t.clone())))
        .collect()
}

/// The binder names of a constructor pattern, `_` for an unbound field.
fn binder_names(body: &Body, slots: &[Option<u16>]) -> Vec<String> {
    slots
        .iter()
        .map(|s| match s {
            Some(s) => body.names[*s as usize].clone(),
            None => "_".to_string(),
        })
        .collect()
}

/// The resolver slots of a constructor pattern, `u16::MAX` for `_`.
fn bound_slots(slots: &[Option<u16>]) -> Vec<u16> {
    slots.iter().map(|s| s.unwrap_or(u16::MAX)).collect()
}

fn typed(node: ResolvedExpr, ty: Type) -> Spanned<ResolvedExpr> {
    let e = Spanned::bare(node);
    e.set_ty(ty);
    e
}

fn boolean(b: bool) -> Spanned<ResolvedExpr> {
    typed(ResolvedExpr::Literal(Literal::Bool(b)), Type::Bool)
}

fn is_true(e: &Spanned<ResolvedExpr>) -> bool {
    matches!(e.node, ResolvedExpr::Literal(Literal::Bool(true)))
}

fn arm(pattern: ResolvedPattern, slots: Vec<u16>, body: Spanned<ResolvedExpr>) -> ResolvedMatchArm {
    let binding_slots = std::sync::OnceLock::new();
    let _ = binding_slots.set(slots);
    ResolvedMatchArm {
        pattern,
        body: Shared::new(body),
        binding_slots,
    }
}

fn matching(subject: Spanned<ResolvedExpr>, arms: Vec<ResolvedMatchArm>) -> Spanned<ResolvedExpr> {
    typed(
        ResolvedExpr::Match {
            subject: Shared::new(subject),
            arms: Shared::new(arms),
        },
        Type::Bool,
    )
}

/// `then` when `cond` holds, else `otherwise`.
fn choose(
    cond: Spanned<ResolvedExpr>,
    then: Spanned<ResolvedExpr>,
    otherwise: Spanned<ResolvedExpr>,
) -> Spanned<ResolvedExpr> {
    matching(
        cond,
        vec![
            arm(ResolvedPattern::Literal(Literal::Bool(true)), vec![], then),
            arm(
                ResolvedPattern::Literal(Literal::Bool(false)),
                vec![],
                otherwise,
            ),
        ],
    )
}

/// `cond`, then `rest` when it held.
fn and_then(cond: Spanned<ResolvedExpr>, rest: Spanned<ResolvedExpr>) -> Spanned<ResolvedExpr> {
    if is_true(&rest) {
        cond
    } else if is_true(&cond) {
        rest
    } else {
        choose(cond, rest, boolean(false))
    }
}

fn call(target: FnId, args: Vec<Spanned<ResolvedExpr>>) -> Spanned<ResolvedExpr> {
    typed(
        ResolvedExpr::Call(ResolvedCallee::Fn(target), Shared::new(args)),
        Type::Bool,
    )
}

fn intrinsic(which: BuiltinIntrinsic, args: Vec<Spanned<ResolvedExpr>>) -> Spanned<ResolvedExpr> {
    typed(
        ResolvedExpr::Call(ResolvedCallee::Intrinsic(which), Shared::new(args)),
        Type::Bool,
    )
}

/// One field of the two values under comparison: the two reads and the
/// field's declared type.
struct Field {
    left: Spanned<ResolvedExpr>,
    right: Spanned<ResolvedExpr>,
    ty: String,
}

impl Writer<'_> {
    fn compare(&self, field: Field, role: Role) -> Spanned<ResolvedExpr> {
        match self.field_eq(&field.ty) {
            FieldEq::Always => boolean(true),
            FieldEq::SameRef => {
                intrinsic(BuiltinIntrinsic::EqSameRef, vec![field.left, field.right])
            }
            FieldEq::Function(name) => {
                call(self.callee(&name, role), vec![field.left, field.right])
            }
            FieldEq::Op => typed(
                ResolvedExpr::BinOp(BinOp::Eq, Shared::new(field.left), Shared::new(field.right)),
                Type::Bool,
            ),
        }
    }

    /// Whether `==` on a field of `ty` steps to the next node of a value
    /// that reaches itself.
    fn steps(&self, ty: &str) -> bool {
        matches!(self.field_eq(ty), FieldEq::Function(name) if self.walks.contains(&name))
    }

    /// The fields compared in order, each only when every one before it was
    /// equal. At the start of a comparison (`Role::Entry`) the fields from
    /// the first that steps to another node on run in a scope.
    fn fields(&self, fields: Vec<Field>, role: Role) -> Spanned<ResolvedExpr> {
        let types: Vec<&str> = fields.iter().map(|f| f.ty.as_str()).collect();
        let order = super::equality_order::field_order(&types, self.registry);
        let mut ordered: Vec<Option<Field>> = fields.into_iter().map(Some).collect();
        let ordered: Vec<Field> = order
            .into_iter()
            .map(|i| ordered[i].take().expect("field order is a permutation"))
            .collect();
        let scoped_from = match role {
            Role::Entry => ordered.iter().position(|f| self.steps(&f.ty)),
            _ => None,
        };
        let mut acc = boolean(true);
        for (i, field) in ordered.into_iter().enumerate().rev() {
            if Some(i + 1) == scoped_from {
                acc = intrinsic(BuiltinIntrinsic::EqScope, vec![acc]);
            }
            acc = and_then(self.compare(field, role), acc);
        }
        if scoped_from == Some(0) {
            acc = intrinsic(BuiltinIntrinsic::EqScope, vec![acc]);
        }
        acc
    }

    /// `body` with its pair of nodes looked up and remembered once the
    /// scope is past its budget, for a node of a type that reaches itself
    /// whose fields step to another node.
    fn remembered(
        &self,
        (a, b): (Spanned<ResolvedExpr>, Spanned<ResolvedExpr>),
        tag: u32,
        keys: Vec<Spanned<ResolvedExpr>>,
        body: Spanned<ResolvedExpr>,
    ) -> Spanned<ResolvedExpr> {
        let mut args = vec![
            a,
            b,
            typed(ResolvedExpr::Literal(Literal::Int(tag as i64)), Type::Int),
        ];
        args.extend(keys);
        args.push(body);
        intrinsic(BuiltinIntrinsic::EqMemo, args)
    }

    /// The fields of a node a key is made from: those a step reads at once.
    fn is_key(&self, ty: &str) -> bool {
        let declared = ty.trim();
        let resolved = self
            .registry
            .newtype_underlying(declared)
            .unwrap_or(declared);
        self.registry.is_eligible_carrier(declared)
            || matches!(resolved, "Int" | "Float" | "Bool" | "String")
    }

    /// `true` for one object against itself, where the type allows it.
    fn same_or(
        &self,
        name: &str,
        role: Role,
        body: &Body,
        rest: Spanned<ResolvedExpr>,
    ) -> Spanned<ResolvedExpr> {
        if role == Role::Plain {
            return rest;
        }
        // Within the budget, every node after the first counts once, as one
        // step of the scope; past it, the node goes on to the step that
        // remembers pairs, as do all the nodes after it.
        let rest = match (role, self.fns.mem.get(name)) {
            (Role::Node, Some(mem)) => {
                let onward = typed(
                    ResolvedExpr::TailCall {
                        target: *mem,
                        args: Shared::new(vec![body.local(0), body.local(1)]),
                    },
                    Type::Bool,
                );
                intrinsic(
                    BuiltinIntrinsic::EqCount,
                    vec![choose(
                        intrinsic(BuiltinIntrinsic::EqPast, vec![]),
                        onward,
                        rest,
                    )],
                )
            }
            _ => rest,
        };
        if !self.eq.reflexive_type(name, self.registry) {
            return rest;
        }
        choose(
            intrinsic(
                BuiltinIntrinsic::EqSameRef,
                vec![body.local(0), body.local(1)],
            ),
            boolean(true),
            rest,
        )
    }

    /// Whether a node with fields of `field_tys` steps to another node.
    fn holds_walk(&self, field_tys: &[String]) -> bool {
        field_tys.iter().any(|t| self.steps(t))
    }

    fn record_body(
        &self,
        name: &str,
        role: Role,
        body: &mut Body,
    ) -> Option<Spanned<ResolvedExpr>> {
        let fields = self.registry.record_fields.get(name)?.clone();
        let read =
            |body: &Body, slot: u16, field: &str, ty: &str| -> Option<Spanned<ResolvedExpr>> {
                Some(typed(
                    ResolvedExpr::Attr(Shared::new(body.local(slot)), field.to_string()),
                    self.ty(ty)?,
                ))
            };
        let mut compared = Vec::new();
        let mut keys = Vec::new();
        for (field, ty) in &fields {
            compared.push(Field {
                left: read(body, 0, field, ty)?,
                right: read(body, 1, field, ty)?,
                ty: ty.clone(),
            });
            if self.is_key(ty) {
                keys.push(read(body, 0, field, ty)?);
            }
        }
        let tys: Vec<String> = fields.iter().map(|(_, t)| t.clone()).collect();
        let mut rest = self.fields(compared, role);
        if role == Role::Mem && self.holds_walk(&tys) {
            let tag = self.registry.record_type_idx(name)?;
            rest = self.remembered((body.local(0), body.local(1)), tag, keys, rest);
        }
        Some(self.same_or(name, role, body, rest))
    }

    fn sum_body(&self, name: &str, role: Role, body: &mut Body) -> Option<Spanned<ResolvedExpr>> {
        let entry = self
            .symbols
            .types
            .iter()
            .find(|t| t.key.canonical() == name && !t.is_product)?;
        let type_id = self.symbols.type_id_of(&entry.key)?;
        let mut outer = Vec::new();
        let many = entry.variants.len() > 1;
        // Constructors in the order of their names, as the byte helpers
        // tested them: the arms are tested in turn, and moving a common
        // constructor down the list costs every comparison of it.
        let mut ctors = entry.variants.clone();
        ctors.sort_by(|x, y| {
            self.symbols
                .ctor_entry(*x)
                .name
                .cmp(&self.symbols.ctor_entry(*y).name)
        });
        for ctor_id in &ctors {
            let ctor = self.symbols.ctor_entry(*ctor_id);
            let info = self
                .registry
                .variants
                .get(&ctor.name)?
                .iter()
                .find(|v| v.parent == name)?
                .clone();
            let user = ResolvedCtor::User {
                ctor_id: *ctor_id,
                type_id,
                name: ctor.name.clone(),
            };
            let tys: Vec<Type> = info
                .fields
                .iter()
                .map(|t| self.ty(t))
                .collect::<Option<_>>()?;
            let order = super::equality_order::field_order(
                &info.fields.iter().map(String::as_str).collect::<Vec<_>>(),
                self.registry,
            );
            // Where a comparison starts, a value usually differs from the
            // other already in its first field: when that field is a scalar,
            // the node's other fields are read only once it is equal.
            let first = order.first().copied();
            let staged = first.filter(|&f| {
                matches!(role, Role::Plain | Role::Entry)
                    && info.fields.len() > 1
                    && self.is_key(&info.fields[f])
            });
            let all: Vec<bool> = vec![true; tys.len()];
            let (keep_now, keep_later): (Vec<bool>, Vec<bool>) = match staged {
                Some(f) => (
                    (0..tys.len()).map(|i| i == f).collect(),
                    (0..tys.len()).map(|i| i != f).collect(),
                ),
                None => (all.clone(), Vec::new()),
            };
            let xs = bind_fields(body, "x", &tys, &keep_now);
            let ys = bind_fields(body, "y", &tys, &keep_now);
            let compare_kept = |body: &Body, xs: &[Option<u16>], ys: &[Option<u16>]| {
                info.fields
                    .iter()
                    .enumerate()
                    .filter_map(|(i, ty)| {
                        Some(Field {
                            left: body.local(xs[i]?),
                            right: body.local(ys[i]?),
                            ty: ty.clone(),
                        })
                    })
                    .collect::<Vec<_>>()
            };
            let mut same = match staged {
                Some(f) => {
                    let xs2 = bind_fields(body, "x", &tys, &keep_later);
                    let ys2 = bind_fields(body, "y", &tys, &keep_later);
                    let later = self.fields(compare_kept(body, &xs2, &ys2), role);
                    let inner = vec![
                        arm(
                            ResolvedPattern::Ctor(user.clone(), binder_names(body, &ys2)),
                            bound_slots(&ys2),
                            later,
                        ),
                        arm(ResolvedPattern::Wildcard, vec![], boolean(false)),
                    ];
                    let again = matching(
                        body.local(0),
                        vec![
                            arm(
                                ResolvedPattern::Ctor(user.clone(), binder_names(body, &xs2)),
                                bound_slots(&xs2),
                                matching(body.local(1), inner),
                            ),
                            arm(ResolvedPattern::Wildcard, vec![], boolean(false)),
                        ],
                    );
                    let head = self.compare(
                        Field {
                            left: body.local(xs[f].expect("kept")),
                            right: body.local(ys[f].expect("kept")),
                            ty: info.fields[f].clone(),
                        },
                        role,
                    );
                    and_then(head, again)
                }
                None => self.fields(compare_kept(body, &xs, &ys), role),
            };
            if role == Role::Mem && self.holds_walk(&info.fields) {
                let keys = info
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(_, ty)| self.is_key(ty))
                    .filter_map(|(i, _)| xs[i].map(|x| body.local(x)))
                    .collect();
                same = self.remembered((body.local(0), body.local(1)), info.type_idx, keys, same);
            }
            let mut inner = vec![arm(
                ResolvedPattern::Ctor(user.clone(), binder_names(body, &ys)),
                bound_slots(&ys),
                same,
            )];
            if many {
                inner.push(arm(ResolvedPattern::Wildcard, vec![], boolean(false)));
            }
            outer.push(arm(
                ResolvedPattern::Ctor(user, binder_names(body, &xs)),
                bound_slots(&xs),
                matching(body.local(1), inner),
            ));
        }
        let rest = matching(body.local(0), outer);
        Some(self.same_or(name, role, body, rest))
    }

    /// `Option`, `Result` and tuples: their constructors, each with its
    /// pattern and field types.
    fn carrier_body(
        &self,
        name: &str,
        kind: EqKind,
        role: Role,
        body: &mut Body,
    ) -> Option<Spanned<ResolvedExpr>> {
        let ty = self.ty(name)?;
        type Shape = (Option<BuiltinCtor>, Vec<String>);
        let shapes: Vec<Shape> = match (kind, &ty) {
            (EqKind::OptionEq, Type::Option(inner)) => vec![
                (Some(BuiltinCtor::OptionNone), vec![]),
                (Some(BuiltinCtor::OptionSome), vec![inner.display()]),
            ],
            (EqKind::ResultEq, Type::Result(ok, err)) => vec![
                (Some(BuiltinCtor::ResultOk), vec![ok.display()]),
                (Some(BuiltinCtor::ResultErr), vec![err.display()]),
            ],
            (EqKind::TupleEq, Type::Tuple(items)) => {
                vec![(None, items.iter().map(Type::display).collect())]
            }
            _ => return None,
        };
        let many = shapes.len() > 1;
        let mut outer = Vec::new();
        for (ctor, field_tys) in shapes {
            let tys: Vec<Type> = field_tys
                .iter()
                .map(|t| self.ty(t))
                .collect::<Option<_>>()?;
            let xs: Vec<u16> = tys.iter().map(|t| body.bind("x", t.clone())).collect();
            let ys: Vec<u16> = tys.iter().map(|t| body.bind("y", t.clone())).collect();
            let compared = field_tys
                .iter()
                .enumerate()
                .map(|(i, ty)| Field {
                    left: body.local(xs[i]),
                    right: body.local(ys[i]),
                    ty: ty.clone(),
                })
                .collect();
            let same = self.fields(compared, role);
            let pattern = |slots: &[u16]| -> ResolvedPattern {
                let names: Vec<String> = slots
                    .iter()
                    .map(|s| body.names[*s as usize].clone())
                    .collect();
                match ctor {
                    Some(c) => ResolvedPattern::Ctor(ResolvedCtor::Builtin(c), names),
                    None => ResolvedPattern::Tuple(
                        names.into_iter().map(ResolvedPattern::Ident).collect(),
                    ),
                }
            };
            let mut inner = vec![arm(pattern(&ys), ys.clone(), same)];
            if many {
                inner.push(arm(ResolvedPattern::Wildcard, vec![], boolean(false)));
            }
            outer.push(arm(
                pattern(&xs),
                xs.clone(),
                matching(body.local(1), inner),
            ));
        }
        let rest = matching(body.local(0), outer);
        Some(self.same_or(name, role, body, rest))
    }

    fn eq_body(
        &self,
        name: &str,
        kind: EqKind,
        role: Role,
    ) -> Option<(Body, Spanned<ResolvedExpr>)> {
        let ty = self.ty(name)?;
        let mut body = Body::new(&[("a", ty.clone()), ("b", ty)]);
        let expr = match kind {
            EqKind::Record => self.record_body(name, role, &mut body)?,
            EqKind::Sum => self.sum_body(name, role, &mut body)?,
            EqKind::OptionEq | EqKind::ResultEq | EqKind::TupleEq => {
                self.carrier_body(name, kind, role, &mut body)?
            }
            EqKind::PackedSequence => return None,
        };
        Some((body, expr))
    }

    /// `List.contains(xs, x)`: the first element equal to `x`.
    fn contains_body(&self, elem: &str, me: FnId) -> Option<(Body, Spanned<ResolvedExpr>)> {
        let elem_ty = self.ty(elem)?;
        let list_ty = Type::List(Box::new(elem_ty.clone()));
        let mut body = Body::new(&[("xs", list_ty.clone()), ("x", elem_ty.clone())]);
        let head = body.bind("h", elem_ty.clone());
        let tail = body.bind("t", list_ty);
        let found = self.compare(
            Field {
                left: body.local(head),
                right: body.local(1),
                ty: elem.to_string(),
            },
            Role::Plain,
        );
        let next = typed(
            ResolvedExpr::TailCall {
                target: me,
                args: Shared::new(vec![body.local(tail), body.local(1)]),
            },
            Type::Bool,
        );
        let cons = ResolvedPattern::Cons(
            body.names[head as usize].clone(),
            body.names[tail as usize].clone(),
        );
        let expr = matching(
            body.local(0),
            vec![
                arm(ResolvedPattern::EmptyList, vec![], boolean(false)),
                arm(cons, vec![head, tail], choose(found, boolean(true), next)),
            ],
        );
        Some((body, expr))
    }

    /// `==` on `List<T>`: as long as each, element by element.
    fn list_body(&self, elem: &str, me: FnId) -> Option<(Body, Spanned<ResolvedExpr>)> {
        let elem_ty = self.ty(elem)?;
        let list_ty = Type::List(Box::new(elem_ty.clone()));
        let mut body = Body::new(&[("a", list_ty.clone()), ("b", list_ty.clone())]);
        let h1 = body.bind("h", elem_ty.clone());
        let t1 = body.bind("t", list_ty.clone());
        let h2 = body.bind("h", elem_ty);
        let t2 = body.bind("t", list_ty);
        let same = self.compare(
            Field {
                left: body.local(h1),
                right: body.local(h2),
                ty: elem.to_string(),
            },
            Role::Plain,
        );
        let next = typed(
            ResolvedExpr::TailCall {
                target: me,
                args: Shared::new(vec![body.local(t1), body.local(t2)]),
            },
            Type::Bool,
        );
        let cons = |h: u16, t: u16| {
            ResolvedPattern::Cons(
                body.names[h as usize].clone(),
                body.names[t as usize].clone(),
            )
        };
        let expr = matching(
            body.local(0),
            vec![
                arm(
                    ResolvedPattern::EmptyList,
                    vec![],
                    matching(
                        body.local(1),
                        vec![
                            arm(ResolvedPattern::EmptyList, vec![], boolean(true)),
                            arm(ResolvedPattern::Wildcard, vec![], boolean(false)),
                        ],
                    ),
                ),
                arm(
                    cons(h1, t1),
                    vec![h1, t1],
                    matching(
                        body.local(1),
                        vec![
                            arm(cons(h2, t2), vec![h2, t2], and_then(same, next)),
                            arm(ResolvedPattern::Wildcard, vec![], boolean(false)),
                        ],
                    ),
                ),
            ],
        );
        Some((body, expr))
    }
}

/// Writes the functions for every type `eq` registered (except a packed
/// sequence, which keeps its own helper), `List.contains` over the element
/// types `contains` and `==` on the lists `lists`. `None` when a type has a
/// shape these functions do not cover; the backend then keeps comparing in
/// its byte helpers.
pub(super) fn synthesize(
    eq: &EqHelperRegistry,
    registry: &TypeRegistry,
    symbols: &mut SymbolTable,
    current_module: Option<String>,
    contains: &BTreeSet<String>,
    lists: &BTreeSet<String>,
) -> Option<Synthesized> {
    if std::env::var_os("AVER_NO_EQ_FNS").is_some() {
        return None;
    }
    let types: Vec<(String, EqKind)> = eq
        .iter()
        .filter(|(_, kind)| !matches!(kind, EqKind::PackedSequence))
        .filter(|(name, _)| registry.newtype_underlying(name).is_none())
        .map(|(n, k)| (n.to_string(), k))
        .collect();
    let covered: BTreeSet<String> = types.iter().map(|(n, _)| n.clone()).collect();
    let order: Vec<String> = eq.iter().map(|(n, _)| n.to_string()).collect();
    let kinds: HashMap<String, EqKind> = eq.iter().map(|(n, k)| (n.to_string(), k)).collect();
    let walks: HashSet<String> = super::body::eq_helpers::self_reaching(&order, &kinds, registry)
        .into_iter()
        .filter(|n| covered.contains(n))
        .collect();
    let contains: Vec<String> = contains
        .iter()
        .filter(|e| covered.contains(*e))
        .cloned()
        .collect();
    // `==` on a list of a covered type, written in the program or as a field
    // of a covered type.
    let fields = types
        .iter()
        .flat_map(|(name, kind)| super::body::eq_helpers::helper_field_types(name, *kind, registry))
        .map(|t| canonical(&t));
    let lists: Vec<String> = lists
        .iter()
        .cloned()
        .chain(fields)
        .filter_map(|l| {
            let elem = l.strip_prefix("List<")?.strip_suffix('>')?;
            covered.contains(elem).then(|| elem.to_string())
        })
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    if types.is_empty() && contains.is_empty() && lists.is_empty() {
        return None;
    }

    // Identities first: a body calls the functions of its field types.
    let mut fns = EqFns::default();
    for (name, _) in &types {
        fns.entry.insert(
            name.clone(),
            symbols.register_synthetic_fn(&format!("__eq_{name}")),
        );
        if walks.contains(name) {
            fns.node.insert(
                name.clone(),
                symbols.register_synthetic_fn(&format!("__eqnode_{name}")),
            );
            fns.mem.insert(
                name.clone(),
                symbols.register_synthetic_fn(&format!("__eqmem_{name}")),
            );
        }
    }
    for elem in &contains {
        fns.contains.insert(
            elem.clone(),
            symbols.register_synthetic_fn(&format!("__contains_{elem}")),
        );
    }
    for elem in &lists {
        fns.lists.insert(
            format!("List<{elem}>"),
            symbols.register_synthetic_fn(&format!("__eq_List<{elem}>")),
        );
    }

    let symbols: &SymbolTable = symbols;
    let mut ctx = ResolveCtx::new(symbols);
    ctx.current_module = current_module;
    let writer = Writer {
        eq,
        registry,
        symbols,
        ctx: &ctx,
        covered,
        walks,
        fns,
    };

    let mut out = Synthesized {
        fn_defs: Vec::new(),
        resolved: Vec::new(),
        fns: writer.fns.clone(),
    };
    let mut push = |id: FnId, name: String, body: Body, expr: Spanned<ResolvedExpr>| {
        let params: Vec<(String, Type)> = (0..2)
            .map(|i| (body.names[i].clone(), body.types[i].clone()))
            .collect();
        out.fn_defs.push(crate::ast::FnDef {
            name: name.clone(),
            line: 0,
            params: params
                .iter()
                .map(|(n, t)| (n.clone(), t.display()))
                .collect(),
            return_type: "Bool".to_string(),
            effects: Vec::new(),
            desc: None,
            body: std::sync::Arc::new(crate::ast::FnBody::from_expr(Spanned::bare(
                crate::ast::Expr::Literal(Literal::Bool(false)),
            ))),
            resolution: None,
        });
        out.resolved.push(ResolvedFnDef {
            fn_id: id,
            name,
            line: 0,
            params,
            return_type: Type::Bool,
            effects: Vec::new(),
            desc: None,
            body: std::sync::Arc::new(ResolvedFnBody::Block(vec![ResolvedStmt::Expr(expr)])),
            resolution: Some(body.resolution()),
        });
    };
    for (name, kind) in &types {
        let entry_role = if writer.walks.contains(name) {
            Role::Entry
        } else {
            Role::Plain
        };
        let (body, expr) = writer.eq_body(name, *kind, entry_role)?;
        push(writer.fns.entry[name], format!("__eq_{name}"), body, expr);
        if let Some(node) = writer.fns.node.get(name) {
            let (body, expr) = writer.eq_body(name, *kind, Role::Node)?;
            push(*node, format!("__eqnode_{name}"), body, expr);
        }
        if let Some(mem) = writer.fns.mem.get(name) {
            let (body, expr) = writer.eq_body(name, *kind, Role::Mem)?;
            push(*mem, format!("__eqmem_{name}"), body, expr);
        }
    }
    for elem in &contains {
        let me = writer.fns.contains[elem];
        let (body, expr) = writer.contains_body(elem, me)?;
        push(me, format!("__contains_{elem}"), body, expr);
    }
    for elem in &lists {
        let me = writer.fns.lists[&format!("List<{elem}>")];
        let (body, expr) = writer.list_body(elem, me)?;
        push(me, format!("__eq_List<{elem}>"), body, expr);
    }
    Some(out)
}

/// The canonical name `eq_helpers` keys the type of `ty` under.
fn key_of(ty: &Type, symbols: &SymbolTable) -> String {
    match ty {
        Type::Named { id: Some(id), .. } => symbols.type_entry(*id).key.canonical(),
        _ => canonical(&ty.display()),
    }
}

/// Every `==` / `!=` on a type with a function here becomes a call of it
/// (`!=` under `Bool.not`), and every `List.contains` over such a type a
/// call of its `__contains_<T>`.
pub(crate) fn rewrite_program(program: &mut MirProgram, fns: &EqFns, symbols: &SymbolTable) {
    let not = program.intern_builtin("Bool.not");
    let builtins = program.builtins.clone();
    for f in program.fns.values_mut() {
        rewrite(&mut f.body, fns, symbols, not, &builtins);
    }
}

fn rewrite(
    e: &mut Spanned<MirExpr>,
    fns: &EqFns,
    symbols: &SymbolTable,
    not: crate::ir::BuiltinId,
    builtins: &[String],
) {
    crate::ir::mir::expr::walk_children_mut(&mut e.node, &mut |child| {
        rewrite(child, fns, symbols, not, builtins)
    });
    let replacement = match &e.node {
        MirExpr::BinOp(b) if matches!(b.node.op, BinOp::Eq | BinOp::Neq) => {
            let target = b.node.lhs.ty().and_then(|t| {
                let key = key_of(t, symbols);
                fns.entry.get(&key).or_else(|| fns.lists.get(&key)).copied()
            });
            target.map(|target| {
                let called = Spanned {
                    node: MirExpr::Call(retyped(
                        MirCall {
                            callee: MirCallee::Fn(target),
                            args: vec![(*b.node.lhs).clone(), (*b.node.rhs).clone()],
                        },
                        e,
                    )),
                    line: e.line,
                    ty: e.ty.clone(),
                    lazy: Default::default(),
                };
                if b.node.op == BinOp::Neq {
                    MirExpr::Call(retyped(
                        MirCall {
                            callee: MirCallee::Builtin(not),
                            args: vec![called],
                        },
                        e,
                    ))
                } else {
                    called.node
                }
            })
        }
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Builtin(id)
                if builtins.get(id.0 as usize).map(String::as_str) == Some("List.contains") =>
            {
                c.node
                    .args
                    .first()
                    .and_then(|a| match a.ty() {
                        Some(Type::List(elem)) => fns.contains.get(&key_of(elem, symbols)).copied(),
                        _ => None,
                    })
                    .map(|target| {
                        MirExpr::Call(retyped(
                            MirCall {
                                callee: MirCallee::Fn(target),
                                args: c.node.args.clone(),
                            },
                            e,
                        ))
                    })
            }
            _ => None,
        },
        _ => None,
    };
    if let Some(node) = replacement {
        e.node = node;
    }
}

fn retyped<T, U>(node: T, like: &Spanned<U>) -> Spanned<T> {
    Spanned {
        node,
        line: like.line,
        ty: like.ty.clone(),
        lazy: Default::default(),
    }
}
