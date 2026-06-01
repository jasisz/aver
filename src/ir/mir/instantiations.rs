//! Phase 6 wave 12a — type-instantiation discovery on MIR.
//!
//! Walks every `Spanned<MirExpr>` in a `MirProgram` and harvests the
//! concrete instantiations of Aver's built-in generic shapes
//! (`List<T>`, `Map<K, V>`, `Vector<T>`, `Tuple<T₁..Tₙ>`,
//! `Option<T>`, `Result<T, E>`) that appear anywhere — fn
//! signatures, expression `ty()` stamps, constructor / literal
//! shapes. Returns a deduped registry per kind.
//!
//! ## Motivation
//!
//! Each backend currently runs its own discovery pass:
//! `src/codegen/wasm_gc/types_discovery.rs` is 897 lines of AST +
//! type-string walkers populating wasm-gc's `TypeRegistry`. The
//! Rust backend duplicates similar logic. They walk the same shape
//! information; they emit the same instantiations. Each backend
//! does its own walk because pre-Phase-6 there was nowhere to
//! park a shared answer.
//!
//! MIR type stamps (Phase 6 wave 1 — `Spanned<MirExpr>` now
//! carries `Option<Type>` on every node) finally give MIR enough
//! information to do discovery once and hand backends a manifest.
//! This wave adds the discovery API without rewiring any backend;
//! the comparison tests (wave 12b) prove the manifest matches
//! wasm-gc's discovery output before we port wasm-gc onto it
//! (wave 12c).
//!
//! ## What this doesn't do (yet)
//!
//! - **No monomorphisation of user fns.** Aver has no user-level
//!   generics (no `fn id<T>(x: T) -> T`); only the built-in
//!   generic types are polymorphic and only via their wbudowane
//!   signatures (`List.prepend<T>` etc.). So there are no
//!   `MirFn` clones to produce — the manifest is just the set of
//!   concrete instantiations of Aver's built-in generic shapes,
//!   which is what backends need to allocate helpers for.
//! - **No backend wiring.** Wave 12c (separate PR) replaces
//!   `wasm_gc::types_discovery` with calls into this manifest.
//!
//! ## Discovery scope
//!
//! For every `MirExpr` node in every fn body, walk
//! `Spanned::ty()` recursively (it's a `Type` tree — `List<T>`'s
//! `T` may itself be `Map<K, V>` etc.) and register every nested
//! generic at every depth. Plus pick up shape hints from the node
//! itself when the stamp is missing:
//!
//! - `MirExpr::List(items)` — if any item has `ty()`, register
//!   `List<T>` directly so empty-list literals without an outer
//!   stamp still surface their element type via the items.
//! - `MirExpr::Tuple(items)` — same treatment for the tuple
//!   shape; the items' types drive the `Tuple<T₁..Tₙ>` arity.
//! - `MirExpr::MapLiteral(entries)` — first entry's `(K, V)`
//!   seeds the `Map<K, V>` instantiation when no outer stamp
//!   sits on the literal.
//! - `MirExpr::Construct(MirCtor::Builtin(Option/Result))` —
//!   the constructor's arg `ty()` seeds the inner type.

use std::collections::HashSet;

use crate::ast::Type;

use crate::ir::hir::BuiltinCtor;

use super::expr::{MirCtor, MirExpr};
use super::program::MirProgram;

/// Per-kind manifest of concrete generic instantiations found in
/// a `MirProgram`. Each `Vec` carries source-order-unique entries
/// (deduped by canonical textual form), so the order is stable
/// across runs against the same input — useful for snapshot
/// tests and for matching backend output positionally.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct InstantiationRegistry {
    /// Distinct `T`s appearing as `List<T>` anywhere in the
    /// program.
    pub lists: Vec<Type>,
    /// Distinct `(K, V)` pairs appearing as `Map<K, V>`.
    pub maps: Vec<(Type, Type)>,
    /// Distinct `T`s appearing as `Vector<T>`.
    pub vectors: Vec<Type>,
    /// Distinct tuple arities + element-type sequences. Stored
    /// as `Vec<Type>` per tuple so a `(Int, Str)` and a
    /// `(Int, Str, Bool)` stay separate registrations.
    pub tuples: Vec<Vec<Type>>,
    /// Distinct `T`s appearing as `Option<T>`.
    pub options: Vec<Type>,
    /// Distinct `(T, E)` pairs appearing as `Result<T, E>`.
    pub results: Vec<(Type, Type)>,
}

impl InstantiationRegistry {
    /// Total count of instantiations across all kinds. Useful
    /// for quick smoke assertions in tests + diagnostics.
    pub fn total(&self) -> usize {
        self.lists.len()
            + self.maps.len()
            + self.vectors.len()
            + self.tuples.len()
            + self.options.len()
            + self.results.len()
    }
}

/// Walk every fn body in `program` and harvest every concrete
/// instantiation of Aver's built-in generic shapes. Deduplicates
/// by textual canonical form within each kind; order matches
/// first-occurrence in walk order.
pub fn discover_instantiations(program: &MirProgram) -> InstantiationRegistry {
    let mut walker = Walker::default();
    // Walk fns in `FnId` order for stability — `iter()` is
    // `HashMap` order otherwise.
    let mut ordered: Vec<_> = program.iter().collect();
    ordered.sort_by_key(|(fn_id, _)| fn_id.0);
    for (_, mir_fn) in ordered {
        // Register the body's top-level stamp before descending —
        // the `visit_expr` walk only sees children's stamps from
        // their parent nodes; the body root has no parent.
        walker.register_ty(mir_fn.body.ty());
        walker.visit_expr(&mir_fn.body.node);
    }
    walker.registry
}

#[derive(Default)]
struct Walker {
    registry: InstantiationRegistry,
    seen_lists: HashSet<String>,
    seen_maps: HashSet<String>,
    seen_vectors: HashSet<String>,
    seen_tuples: HashSet<String>,
    seen_options: HashSet<String>,
    seen_results: HashSet<String>,
}

impl Walker {
    fn visit_expr(&mut self, expr: &MirExpr) {
        // Pull the type stamp off the wrapping `Spanned` via
        // recursion entry points: every recursive descent starts
        // from `Spanned<MirExpr>`, so we register that level's
        // stamp before recursing into children.
        match expr {
            MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => {}
            MirExpr::Neg(inner) => {
                self.register_ty(inner.ty());
                self.visit_expr(&inner.node);
            }
            MirExpr::BinOp(spanned_bop) => {
                let bop = &spanned_bop.node;
                self.register_ty(bop.lhs.ty());
                self.visit_expr(&bop.lhs.node);
                self.register_ty(bop.rhs.ty());
                self.visit_expr(&bop.rhs.node);
            }
            MirExpr::Let(spanned_let) => {
                let let_node = &spanned_let.node;
                self.register_ty(let_node.value.ty());
                self.visit_expr(&let_node.value.node);
                self.register_ty(let_node.body.ty());
                self.visit_expr(&let_node.body.node);
            }
            MirExpr::Call(spanned_call) => {
                for arg in &spanned_call.node.args {
                    self.register_ty(arg.ty());
                    self.visit_expr(&arg.node);
                }
            }
            MirExpr::TailCall(spanned_tc) => {
                for arg in &spanned_tc.node.args {
                    self.register_ty(arg.ty());
                    self.visit_expr(&arg.node);
                }
            }
            MirExpr::Match(spanned_match) => {
                let m = &spanned_match.node;
                self.register_ty(m.subject.ty());
                self.visit_expr(&m.subject.node);
                for arm in &m.arms {
                    self.register_ty(arm.body.ty());
                    self.visit_expr(&arm.body.node);
                }
            }
            MirExpr::IfThenElse(spanned_ite) => {
                let ite = &spanned_ite.node;
                self.register_ty(ite.cond.ty());
                self.visit_expr(&ite.cond.node);
                self.register_ty(ite.then_branch.ty());
                self.visit_expr(&ite.then_branch.node);
                self.register_ty(ite.else_branch.ty());
                self.visit_expr(&ite.else_branch.node);
            }
            MirExpr::Construct(spanned_ctor) => {
                let con = &spanned_ctor.node;
                // Built-in wrapper constructors (`Result.Ok`,
                // `Option.Some`, `Result.Err`) carry their inner
                // type via the first arg's `ty()`. Seed the
                // outer instantiation from there when the
                // typecheck didn't stamp the outer literal.
                if let MirCtor::Builtin(bc) = con.ctor {
                    self.seed_builtin_ctor(bc, &con.args);
                }
                for arg in &con.args {
                    self.register_ty(arg.ty());
                    self.visit_expr(&arg.node);
                }
            }
            MirExpr::RecordCreate(spanned_rec) => {
                for f in &spanned_rec.node.fields {
                    self.register_ty(f.value.ty());
                    self.visit_expr(&f.value.node);
                }
            }
            MirExpr::RecordUpdate(spanned_upd) => {
                self.register_ty(spanned_upd.node.base.ty());
                self.visit_expr(&spanned_upd.node.base.node);
                for f in &spanned_upd.node.updates {
                    self.register_ty(f.value.ty());
                    self.visit_expr(&f.value.node);
                }
            }
            MirExpr::Project(spanned_proj) => {
                self.register_ty(spanned_proj.node.base.ty());
                self.visit_expr(&spanned_proj.node.base.node);
            }
            MirExpr::Try(inner) | MirExpr::Return(inner) => {
                self.register_ty(inner.ty());
                self.visit_expr(&inner.node);
            }
            MirExpr::List(items) => {
                // Seed `List<T>` from the items even when the
                // outer literal has no stamp (empty-list shape
                // gets covered by Phase 6 wave 1 stamps; this
                // is the belt-and-braces case).
                if let Some(first) = items.first().and_then(|i| i.ty()) {
                    self.register_list_t(first);
                }
                for item in items {
                    self.register_ty(item.ty());
                    self.visit_expr(&item.node);
                }
            }
            MirExpr::Tuple(items) => {
                // Seed `Tuple<T₁..Tₙ>` from the items.
                let tys: Vec<Type> = items.iter().filter_map(|i| i.ty().cloned()).collect();
                if tys.len() == items.len() && !tys.is_empty() {
                    self.register_tuple(tys);
                }
                for item in items {
                    self.register_ty(item.ty());
                    self.visit_expr(&item.node);
                }
            }
            MirExpr::MapLiteral(entries) => {
                if let Some((k_first, v_first)) = entries.first()
                    && let (Some(k_ty), Some(v_ty)) = (k_first.ty(), v_first.ty())
                {
                    self.register_map_kv(k_ty, v_ty);
                }
                for (k, v) in entries {
                    self.register_ty(k.ty());
                    self.visit_expr(&k.node);
                    self.register_ty(v.ty());
                    self.visit_expr(&v.node);
                }
            }
            MirExpr::InterpolatedStr(parts) => {
                for part in parts {
                    if let super::expr::MirStrPart::Expr(e) = part {
                        self.register_ty(e.ty());
                        self.visit_expr(&e.node);
                    }
                }
            }
            MirExpr::IndependentProduct(spanned_ip) => {
                for item in &spanned_ip.node.items {
                    self.register_ty(item.ty());
                    self.visit_expr(&item.node);
                }
            }
        }
    }

    fn seed_builtin_ctor(&mut self, ctor: BuiltinCtor, args: &[crate::ast::Spanned<MirExpr>]) {
        match ctor {
            BuiltinCtor::OptionSome => {
                if let Some(first) = args.first().and_then(|a| a.ty()) {
                    self.register_option_t(first);
                }
            }
            BuiltinCtor::OptionNone => {}
            BuiltinCtor::ResultOk => {
                // Ok(T) carries T; E is unknown from this site.
                // We register it as `Result<T, Invalid>` placeholder
                // so the kind shows up; consumers (backends) merge
                // by T when full pairs aren't observed.
                if let Some(t) = args.first().and_then(|a| a.ty()) {
                    self.register_result_te(t, &Type::Invalid);
                }
            }
            BuiltinCtor::ResultErr => {
                if let Some(e) = args.first().and_then(|a| a.ty()) {
                    self.register_result_te(&Type::Invalid, e);
                }
            }
        }
    }

    /// Top-level type registration — descends the type tree
    /// recursively so nested generics (`List<Map<Str, Int>>`)
    /// surface every layer.
    fn register_ty(&mut self, ty: Option<&Type>) {
        let Some(ty) = ty else { return };
        match ty {
            Type::List(inner) => {
                self.register_list_t(inner);
            }
            Type::Vector(inner) => {
                self.register_vector_t(inner);
            }
            Type::Option(inner) => {
                self.register_option_t(inner);
            }
            Type::Map(k, v) => {
                self.register_map_kv(k, v);
            }
            Type::Result(t, e) => {
                self.register_result_te(t, e);
            }
            Type::Tuple(items) => {
                self.register_tuple(items.clone());
            }
            Type::Fn(args, ret, _effects) => {
                for a in args {
                    self.register_ty(Some(a));
                }
                self.register_ty(Some(ret));
            }
            // Primitives + named user types don't open further
            // sub-instantiations; they may BE inside a generic
            // wrapper but their own walk terminates here.
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid
            | Type::Named { .. } => {}
        }
    }

    fn register_list_t(&mut self, t: &Type) {
        let key = format!("{t:?}");
        if self.seen_lists.insert(key) {
            self.registry.lists.push(t.clone());
        }
        // Recurse into the element type so nested generics land too.
        self.register_ty(Some(t));
    }

    fn register_vector_t(&mut self, t: &Type) {
        let key = format!("{t:?}");
        if self.seen_vectors.insert(key) {
            self.registry.vectors.push(t.clone());
        }
        self.register_ty(Some(t));
    }

    fn register_option_t(&mut self, t: &Type) {
        let key = format!("{t:?}");
        if self.seen_options.insert(key) {
            self.registry.options.push(t.clone());
        }
        self.register_ty(Some(t));
    }

    fn register_map_kv(&mut self, k: &Type, v: &Type) {
        let key = format!("({k:?},{v:?})");
        if self.seen_maps.insert(key) {
            self.registry.maps.push((k.clone(), v.clone()));
        }
        self.register_ty(Some(k));
        self.register_ty(Some(v));
    }

    fn register_result_te(&mut self, t: &Type, e: &Type) {
        let key = format!("({t:?},{e:?})");
        if self.seen_results.insert(key) {
            self.registry.results.push((t.clone(), e.clone()));
        }
        self.register_ty(Some(t));
        self.register_ty(Some(e));
    }

    fn register_tuple(&mut self, items: Vec<Type>) {
        let key = format!("{items:?}");
        if self.seen_tuples.insert(key) {
            for item in &items {
                self.register_ty(Some(item));
            }
            self.registry.tuples.push(items);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Literal, Spanned};
    use crate::ir::FnId;
    use crate::ir::mir::{MirExpr, MirFn};

    fn span_with_ty<T>(node: T, ty: Type) -> Spanned<T> {
        let cell = std::sync::OnceLock::new();
        let _ = cell.set(ty);
        Spanned {
            node,
            line: 0,
            ty: cell,
        }
    }

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: std::sync::OnceLock::new(),
        }
    }

    fn one_fn_program(body: Spanned<MirExpr>) -> MirProgram {
        let mut p = MirProgram::empty();
        p.fns.insert(
            FnId(0),
            MirFn {
                fn_id: FnId(0),
                name: "f".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body,
                local_count: 0,
            },
        );
        p
    }

    #[test]
    fn empty_program_yields_empty_registry() {
        let registry = discover_instantiations(&MirProgram::empty());
        assert_eq!(registry.total(), 0);
    }

    #[test]
    fn list_int_stamp_seeds_list_t() {
        let body = span_with_ty(
            MirExpr::Literal(span(Literal::Int(0))),
            Type::List(Box::new(Type::Int)),
        );
        let registry = discover_instantiations(&one_fn_program(body));
        assert_eq!(registry.lists, vec![Type::Int]);
    }

    #[test]
    fn nested_list_map_str_int_registers_every_layer() {
        // `List<Map<Str, Int>>` — both `List<Map<Str, Int>>` AND
        // its inner `Map<Str, Int>` should show up.
        let inner = Type::Map(Box::new(Type::Str), Box::new(Type::Int));
        let outer = Type::List(Box::new(inner.clone()));
        let body = span_with_ty(MirExpr::Literal(span(Literal::Int(0))), outer);
        let registry = discover_instantiations(&one_fn_program(body));
        assert_eq!(registry.lists, vec![inner.clone()]);
        assert_eq!(registry.maps, vec![(Type::Str, Type::Int)]);
    }

    #[test]
    fn deeply_nested_user_example_registers_all_layers() {
        // `List<Map<UserType, Tuple<InnyTyp, Int>>>` — the
        // shape Szymon highlighted. Every layer should surface:
        // - Tuple<InnyTyp, Int>
        // - Map<UserType, Tuple<InnyTyp, Int>>
        // - List<Map<UserType, Tuple<InnyTyp, Int>>>
        let user_type = Type::named("UserType");
        let inny = Type::named("InnyTyp");
        let tuple = Type::Tuple(vec![inny.clone(), Type::Int]);
        let map = Type::Map(Box::new(user_type.clone()), Box::new(tuple.clone()));
        let list = Type::List(Box::new(map.clone()));

        let body = span_with_ty(MirExpr::Literal(span(Literal::Int(0))), list);
        let registry = discover_instantiations(&one_fn_program(body));

        assert_eq!(registry.lists, vec![map.clone()]);
        assert_eq!(registry.maps, vec![(user_type, tuple.clone())]);
        let Type::Tuple(items) = &tuple else {
            unreachable!()
        };
        assert_eq!(registry.tuples, vec![items.clone()]);
    }

    #[test]
    fn duplicates_collapse_within_kind() {
        // Two unrelated stamps of `List<Int>` should yield ONE
        // registration.
        let l_int = Type::List(Box::new(Type::Int));
        let bop = super::super::expr::MirBinOp {
            op: crate::ast::BinOp::Add,
            lhs: Box::new(span_with_ty(
                MirExpr::Literal(span(Literal::Int(1))),
                l_int.clone(),
            )),
            rhs: Box::new(span_with_ty(
                MirExpr::Literal(span(Literal::Int(2))),
                l_int.clone(),
            )),
        };
        let body = span(MirExpr::BinOp(span(bop)));
        let registry = discover_instantiations(&one_fn_program(body));
        assert_eq!(registry.lists.len(), 1);
    }

    #[test]
    fn distinct_list_t_kinds_register_separately() {
        // `List<Int>` AND `List<Str>` from two stamps → two
        // entries.
        let bop = super::super::expr::MirBinOp {
            op: crate::ast::BinOp::Add,
            lhs: Box::new(span_with_ty(
                MirExpr::Literal(span(Literal::Int(1))),
                Type::List(Box::new(Type::Int)),
            )),
            rhs: Box::new(span_with_ty(
                MirExpr::Literal(span(Literal::Int(2))),
                Type::List(Box::new(Type::Str)),
            )),
        };
        let body = span(MirExpr::BinOp(span(bop)));
        let registry = discover_instantiations(&one_fn_program(body));
        assert_eq!(registry.lists.len(), 2);
        assert!(registry.lists.contains(&Type::Int));
        assert!(registry.lists.contains(&Type::Str));
    }

    #[test]
    fn vector_and_option_kinds_route_to_their_own_buckets() {
        let bop = super::super::expr::MirBinOp {
            op: crate::ast::BinOp::Add,
            lhs: Box::new(span_with_ty(
                MirExpr::Literal(span(Literal::Int(0))),
                Type::Vector(Box::new(Type::Bool)),
            )),
            rhs: Box::new(span_with_ty(
                MirExpr::Literal(span(Literal::Int(0))),
                Type::Option(Box::new(Type::Float)),
            )),
        };
        let body = span(MirExpr::BinOp(span(bop)));
        let registry = discover_instantiations(&one_fn_program(body));
        assert_eq!(registry.vectors, vec![Type::Bool]);
        assert_eq!(registry.options, vec![Type::Float]);
    }

    #[test]
    fn result_pair_registers_under_results() {
        let body = span_with_ty(
            MirExpr::Literal(span(Literal::Int(0))),
            Type::Result(Box::new(Type::Int), Box::new(Type::Str)),
        );
        let registry = discover_instantiations(&one_fn_program(body));
        assert_eq!(registry.results, vec![(Type::Int, Type::Str)]);
    }
}
