//! Trait capabilities of values after lowering to the Rust backend.
//!
//! Surface types do not map one-to-one to Rust carriers: `List<Int>` may be
//! `AverIntList`, an opaque refinement may be `AverPackedU8`, and capability
//! resources are provider-owned handles. Generated composite derives must ask
//! what the selected carrier promises instead of inferring traits from a bare
//! Aver type name or attaching a blanket derive and waiting for rustc.

use std::collections::HashSet;

use crate::ast::TypeDef;
use crate::codegen::CodegenContext;
use crate::types::{Type, parse_type_str};

/// Operations a lowered Rust value promises to generated composite types.
///
/// This is deliberately a conservative promise, not a reflection of every
/// implementation detail on the carrier. In particular a provider resource's
/// host wrapper may implement `Eq`/`Hash` for registry bookkeeping, while its
/// Aver-facing capability set withholds both because provider token identity
/// is unobservable. The typechecker enforces that surface rule before codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RepresentationCapabilities {
    pub clone: bool,
    pub partial_eq: bool,
    pub eq: bool,
    pub hash: bool,
    pub aver_display: bool,
}

/// Complete structural contract consumed by a generated record or sum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RepresentationContract {
    pub capabilities: RepresentationCapabilities,
    pub orderable: bool,
}

impl RepresentationCapabilities {
    const ALL: Self = Self {
        clone: true,
        partial_eq: true,
        eq: true,
        hash: true,
        aver_display: true,
    };

    const NONE: Self = Self {
        clone: false,
        partial_eq: false,
        eq: false,
        hash: false,
        aver_display: false,
    };

    /// Carrier-level equality keeps generated resource holders usable from a
    /// Rust host (and preserves #994), but it is not an Aver equality grant.
    /// The checker rejects `==`, equality-bearing helpers and Map keys for a
    /// resource or any represented wrapper that reaches one.
    const PROVIDER_RESOURCE: Self = Self {
        clone: true,
        partial_eq: true,
        eq: false,
        hash: false,
        aver_display: true,
    };

    fn intersect(self, other: Self) -> Self {
        Self {
            clone: self.clone && other.clone,
            partial_eq: self.partial_eq && other.partial_eq,
            eq: self.eq && other.eq,
            hash: self.hash && other.hash,
            aver_display: self.aver_display && other.aver_display,
        }
    }
}

/// Contract of one declaration's actual emitted representation.
pub(super) fn type_def_contract(td: &TypeDef, ctx: &CodegenContext) -> RepresentationContract {
    RepresentationContract {
        capabilities: capabilities_for_def(td, ctx, &mut HashSet::new()),
        // Keep order beside the five requested traits so Hash bounds for
        // AverMap and generated Ord impls resolve named fields identically.
        orderable: orderable_def(td, ctx, &mut HashSet::new()),
    }
}

/// How much larger than every other constructor of a sum that is not
/// recursive one must be to go behind a `Shared` (see [`shared_variants`]).
/// The pointer costs a value of the large constructor an allocation when it
/// is built and about 48 bytes (pointer, counts, allocator rounding); at a
/// 64-byte difference a value of a small one saves as much, so the shared
/// layout takes less memory whenever fewer than half the values are large.
/// Measured on lists of two million values: building only large ones is 8%
/// slower, only small ones 12% faster; at 32 bytes the large ones were 18%
/// slower and the memory broke even at a quarter of them.
const LARGE_VARIANT_BYTES: usize = 64;

/// Whether a sum is laid out as an `aver_rt::thin_sum!`: one pointer per
/// value, to a node holding the constructor's tag and its fields only.
///
/// A sum is when a value of it can hold a value of the same type with no
/// collection in between: directly, or through records, other sums,
/// `Option`, `Result` or tuples. Such a type has a fixed size only through a
/// pointer somewhere on the cycle, and with the pointer on the value, a value
/// used in many places is one allocation: copying it, or putting it into
/// another constructor, copies the pointer. Recursion through `List`,
/// `Vector` or `Map` already goes through the heap.
pub(super) fn is_thin_sum(td: &TypeDef, ctx: &CodegenContext) -> bool {
    matches!(td, TypeDef::Sum { .. }) && on_cycle(td, ctx)
}

/// Whether a value of `td` can hold a value of `td` with no collection in
/// between: a recursive sum (see [`is_thin_sum`]) or a record on the cycle
/// of one.
pub(super) fn on_cycle(td: &TypeDef, ctx: &CodegenContext) -> bool {
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    reaches_inline(td, &key, ctx, &mut HashSet::new())
}

/// Whether a field of `ty` hands the nodes it holds over when the node or
/// record holding it is freed (`aver_rt::thin::HandOver`): a type
/// [`on_cycle`], or an `Option` of one. A field of any other type is dropped
/// as it is.
pub(super) fn hands_over(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> bool {
    match ty {
        Type::Option(inner) => hands_over(inner, ctx, scope),
        Type::Named { id, name } => {
            named_type_def(*id, name, ctx, scope).is_some_and(|td| on_cycle(td, ctx))
        }
        _ => false,
    }
}

/// The recursive sum (see [`is_thin_sum`]) `callee` returns, which a
/// `lazy callee(…)` defers into a node of (`aver_rt::thin::Lazy`); `None`
/// when it returns anything else.
pub(super) fn deferred_sum(callee: crate::ir::FnId, ctx: &CodegenContext) -> Option<&TypeDef> {
    let resolved = ctx.resolved_program.fn_by_id(callee)?;
    let scope = ctx.symbol_table.fn_entry(callee).key.scope_str();
    match &resolved.return_type {
        Type::Named { id, name } => {
            named_type_def(*id, name, ctx, scope).filter(|td| is_thin_sum(td, ctx))
        }
        _ => None,
    }
}

/// [`is_thin_sum`] for the type `id`.
pub(super) fn type_is_thin(id: crate::ir::TypeId, ctx: &CodegenContext) -> bool {
    named_type_def(Some(id), "", ctx, None).is_some_and(|td| is_thin_sum(td, ctx))
}

/// Which constructors of a sum that is not thin (see [`is_thin_sum`]) keep
/// their fields in one `aver_rt::Shared` (`T::Pair(Shared<T__Pair>)`), one
/// entry per variant in declaration order: only a constructor at least
/// [`LARGE_VARIANT_BYTES`] larger than every other one, since a value of a
/// sum takes the room of its largest constructor: without the pointer a
/// value of a small one would carry that room unused.
pub(super) fn shared_variants(td: &TypeDef, ctx: &CodegenContext) -> Vec<bool> {
    let TypeDef::Sum { variants, .. } = td else {
        return Vec::new();
    };
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    if is_thin_sum(td, ctx) {
        return vec![false; variants.len()];
    }
    let scope = crate::codegen::common::type_key_for_decl(ctx, td);
    let sizes: Vec<usize> = variants
        .iter()
        .map(|variant| {
            variant
                .fields
                .iter()
                .map(|field| {
                    inline_size(
                        &parse_type_str(field),
                        ctx,
                        scope.scope_str(),
                        &mut HashSet::from([key.clone()]),
                    )
                })
                .sum()
        })
        .collect();
    let largest = (0..sizes.len()).max_by_key(|&i| sizes[i]);
    (0..sizes.len())
        .map(|i| {
            Some(i) == largest
                && sizes.len() > 1
                && sizes
                    .iter()
                    .enumerate()
                    .all(|(j, &size)| j == i || size + LARGE_VARIANT_BYTES <= sizes[i])
        })
        .collect()
}

/// How many bytes a record's fields may take inline before the record is
/// kept behind a `Box` (see [`is_boxed_record`]). A record handed on and
/// updated in a loop is faster boxed at any size (60% at 80 bytes, 69% at
/// 272), but one built many times into a list costs an allocation each:
/// 17-20% slower and a third more memory at 80 to 272 bytes. Records up to
/// 128 bytes are mostly such data (eight `Int` fields), state is larger:
/// Pestka's checker state is 176 bytes, and checking Init took 6% less CPU
/// with the box at 128 and at 64 bytes, and nothing less at 256, where the
/// state stayed inline.
const BOXED_RECORD_BYTES: usize = 128;

/// Whether a record is laid out as `pub type T = Box<T__Fields>`: a value
/// is one pointer to its fields. State is handed from function to function
/// in a record, so without the pointer every call and every return copies
/// all of it, and the copy grows with the state. With it an update of the
/// last use writes the field in place. A record on a cycle of a thin sum
/// already sits behind that sum's pointer.
pub(super) fn is_boxed_record(td: &TypeDef, ctx: &CodegenContext) -> bool {
    let TypeDef::Product { name, fields, .. } = td else {
        return false;
    };
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    let scope = crate::codegen::common::type_key_for_decl(ctx, td);
    let size: usize = fields
        .iter()
        .map(|(_, field)| {
            inline_size(
                &parse_type_str(field),
                ctx,
                scope.scope_str(),
                &mut HashSet::from([key.clone()]),
            )
        })
        .sum();
    boxes(size, td, name, ctx)
}

/// [`is_boxed_record`] for a record whose fields take `size` bytes.
fn boxes(size: usize, td: &TypeDef, name: &str, ctx: &CodegenContext) -> bool {
    size > BOXED_RECORD_BYTES && !super::uses_packed_u8(ctx, name) && !on_cycle(td, ctx)
}

/// What a record literal of the Rust type `name` opens and closes with:
/// `Self { … }`, or `Box::new(T__Fields { … })` for a boxed record.
pub(super) fn record_literal_ends(name: &str, boxed: bool) -> (String, &'static str) {
    match boxed {
        true => (format!("Box::new({name}__Fields {{"), "})"),
        false => ("Self {".to_string(), "}"),
    }
}

/// [`is_boxed_record`] for the type `id` (or `name` without one).
pub(super) fn type_is_boxed_record(
    id: Option<crate::ir::TypeId>,
    name: &str,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> bool {
    named_type_def(id, name, ctx, scope).is_some_and(|td| is_boxed_record(td, ctx))
}

/// [`inline_size`] of a param or field of type `ty`, declared in `scope`.
pub(super) fn type_inline_size(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> usize {
    inline_size(ty, ctx, scope, &mut HashSet::new())
}

/// The bytes a value of `ty` takes inline in the Rust it is lowered to, as
/// near as the size of the runtime's carriers allows: the measure
/// [`shared_variants`] compares constructors by and [`is_boxed_record`]
/// weighs a record by.
fn inline_size(
    ty: &Type,
    ctx: &CodegenContext,
    scope: Option<&str>,
    seen: &mut HashSet<String>,
) -> usize {
    const WORD: usize = std::mem::size_of::<usize>();
    match ty {
        Type::Unit => 0,
        Type::Bool => 1,
        Type::Float | Type::Map(..) | Type::Vector(_) => WORD,
        Type::Int | Type::Str | Type::Fn(..) => 2 * WORD,
        Type::List(_) => 3 * WORD,
        Type::Option(inner) => inline_size(inner, ctx, scope, seen) + WORD,
        Type::Result(ok, err) => {
            inline_size(ok, ctx, scope, seen).max(inline_size(err, ctx, scope, seen)) + WORD
        }
        Type::Tuple(items) => items
            .iter()
            .map(|item| inline_size(item, ctx, scope, seen))
            .sum(),
        Type::Named { id, name } => {
            let Some(td) = named_type_def(*id, name, ctx, scope) else {
                return 2 * WORD;
            };
            let key = crate::codegen::common::backend_type_def_key(ctx, td);
            if !seen.insert(key.clone()) {
                return WORD;
            }
            let decl = crate::codegen::common::type_key_for_decl(ctx, td);
            let mut field_size =
                |field: &String| inline_size(&parse_type_str(field), ctx, decl.scope_str(), seen);
            let size = match td {
                TypeDef::Product { name, fields, .. } => {
                    let size = fields.iter().map(|(_, f)| field_size(f)).sum();
                    match boxes(size, td, name, ctx) {
                        true => WORD,
                        false => size,
                    }
                }
                TypeDef::Sum { .. } if is_thin_sum(td, ctx) => WORD,
                TypeDef::Sum { variants, .. } => {
                    let shared = shared_variants(td, ctx);
                    variants
                        .iter()
                        .zip(shared)
                        .map(|(variant, shared)| match shared {
                            true => WORD,
                            false => variant.fields.iter().map(&mut field_size).sum(),
                        })
                        .max()
                        .unwrap_or(0)
                        + WORD
                }
            };
            seen.remove(&key);
            size
        }
        Type::Var(_) | Type::Invalid => WORD,
    }
}

/// What a match on `&self` writes after a constructor, and a reference to
/// each of its fields: `T::Pair(f0, f1)` reads `f0` and `f1`, a shared sum's
/// `T::Pair(f)` reads `(&f.0)` and `(&f.1)`.
pub(super) fn variant_field_refs(shared: bool, stem: &str, count: usize) -> (String, Vec<String>) {
    if shared {
        let refs = (0..count).map(|i| format!("(&{stem}.{i})")).collect();
        (stem.to_string(), refs)
    } else {
        let names: Vec<String> = (0..count).map(|i| format!("{stem}{i}")).collect();
        (names.join(", "), names)
    }
}

/// Shared scalar-first equality order, with nominal names resolved in the
/// declaration's owning module. Rust still chooses the comparison machinery.
pub(super) fn cheap_fields_first(
    types: &[&str],
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> Option<Vec<usize>> {
    let types: Vec<Type> = types.iter().map(|ty| parse_type_str(ty)).collect();
    crate::ir::compare_shape::cheap_fields_first(&types, scope, &|id, name, owner| {
        let td = named_type_def(id, name, ctx, owner)?;
        Some(crate::ir::compare_shape::NominalFields {
            key: crate::codegen::common::type_key_for_decl(ctx, td),
            fields: fields_of(td).map(|field| parse_type_str(field)).collect(),
        })
    })
}

/// [`shared_variants`] for one constructor of the type `id`.
pub(super) fn ctor_is_shared(id: crate::ir::TypeId, variant: &str, ctx: &CodegenContext) -> bool {
    named_type_def(Some(id), "", ctx, None).is_some_and(|td| match td {
        TypeDef::Sum { variants, .. } => variants
            .iter()
            .zip(shared_variants(td, ctx))
            .any(|(v, shared)| shared && v.name == variant),
        TypeDef::Product { .. } => false,
    })
}

/// Whether the generated `==` of `ty` passes the `aver_rt::Walk` of the
/// comparison it is part of down to its fields (`aver_rt::WalkEq`): a type
/// that holds a value of itself (see [`walking_def`]), or an `Option` of
/// one; or a collection, tuple or `Result` holding a type that holds itself
/// through a collection (see [`through_collection`]), whose items the walk
/// then compares (`aver_rt::shared_items`). A field of any other type begins
/// a comparison of its own.
pub(super) fn walks(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> bool {
    match ty {
        Type::Option(inner) => walks(inner, ctx, scope),
        Type::Named { id, name } => {
            named_type_def(*id, name, ctx, scope).is_some_and(|td| walking_def(td, ctx))
        }
        Type::List(item) | Type::Vector(item) | Type::Map(_, item) => {
            walks(item, ctx, scope) && holds_collection_cycle(item, ctx, scope)
        }
        Type::Tuple(items) => {
            holds_collection_cycle(ty, ctx, scope) && items.iter().any(|t| walks(t, ctx, scope))
        }
        Type::Result(ok, err) => {
            holds_collection_cycle(ty, ctx, scope)
                && (walks(ok, ctx, scope) || walks(err, ctx, scope))
        }
        _ => false,
    }
}

/// [`walks`] for a declaration: a type that holds a value of itself, with
/// no collection in between (see [`on_cycle`]) or through one.
pub(super) fn walking_def(td: &TypeDef, ctx: &CodegenContext) -> bool {
    type_def_contract(td, ctx).capabilities.partial_eq
        && (on_cycle(td, ctx) || through_collection(td, ctx))
}

/// Whether the `==` of `ty` is a walk-free `aver_fast_eq` (see
/// `aver_rt::shared_items::top`): a type that holds itself through a
/// collection and is not a thin sum, whose `==` carries a walk.
pub(super) fn compares_fast(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> bool {
    match ty {
        Type::Named { id, name } => named_type_def(*id, name, ctx, scope).is_some_and(|td| {
            walking_def(td, ctx) && !is_thin_sum(td, ctx) && through_collection(td, ctx)
        }),
        _ => false,
    }
}

/// Whether a value of `td` can hold a value of `td` in a `List`, a `Vector`
/// or a `Map`, at any depth. Its `==` and hash then carry one walk through
/// those collections (see `aver_rt::shared_items`): a value holding one
/// subterm in many places is compared and hashed at the cost of the graph,
/// not of the tree it unfolds to.
pub(super) fn through_collection(td: &TypeDef, ctx: &CodegenContext) -> bool {
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    let scope = crate::codegen::common::type_key_for_decl(ctx, td);
    let mut seen = HashSet::new();
    fields_of(td).any(|field| {
        collection_reaches(
            &parse_type_str(field),
            &key,
            false,
            ctx,
            scope.scope_str(),
            &mut seen,
        )
    })
}

/// Whether `ty` reaches the type `target`, through a collection somewhere
/// on the way when `in_collection` is not already.
fn collection_reaches(
    ty: &Type,
    target: &str,
    in_collection: bool,
    ctx: &CodegenContext,
    scope: Option<&str>,
    seen: &mut HashSet<(String, bool)>,
) -> bool {
    let mut on = |ty: &Type, inside: bool| collection_reaches(ty, target, inside, ctx, scope, seen);
    match ty {
        Type::Option(inner) => on(inner, in_collection),
        Type::Result(ok, err) => on(ok, in_collection) || on(err, in_collection),
        Type::Tuple(items) => items.iter().any(|item| on(item, in_collection)),
        Type::List(item) | Type::Vector(item) => on(item, true),
        Type::Map(k, v) => on(k, true) || on(v, true),
        Type::Named { id, name } => named_type_def(*id, name, ctx, scope).is_some_and(|next| {
            let key = crate::codegen::common::backend_type_def_key(ctx, next);
            if in_collection && key == target {
                return true;
            }
            let decl = crate::codegen::common::type_key_for_decl(ctx, next);
            seen.insert((key, in_collection))
                && fields_of(next).any(|field| {
                    collection_reaches(
                        &parse_type_str(field),
                        target,
                        in_collection,
                        ctx,
                        decl.scope_str(),
                        seen,
                    )
                })
        }),
        _ => false,
    }
}

/// Whether `ty` names, at any depth of its type arguments, a type that
/// holds itself through a collection (see [`through_collection`]).
fn holds_collection_cycle(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> bool {
    match ty {
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            holds_collection_cycle(inner, ctx, scope)
        }
        Type::Result(a, b) | Type::Map(a, b) => {
            holds_collection_cycle(a, ctx, scope) || holds_collection_cycle(b, ctx, scope)
        }
        Type::Tuple(items) => items.iter().any(|t| holds_collection_cycle(t, ctx, scope)),
        Type::Named { id, name } => {
            named_type_def(*id, name, ctx, scope).is_some_and(|td| through_collection(td, ctx))
        }
        _ => false,
    }
}

fn reaches_inline(
    td: &TypeDef,
    target: &str,
    ctx: &CodegenContext,
    seen: &mut HashSet<String>,
) -> bool {
    let scope = crate::codegen::common::type_key_for_decl(ctx, td);
    fields_of(td).any(|field| {
        inline_type_reaches(&parse_type_str(field), target, ctx, scope.scope_str(), seen)
    })
}

fn inline_type_reaches(
    ty: &Type,
    target: &str,
    ctx: &CodegenContext,
    scope: Option<&str>,
    seen: &mut HashSet<String>,
) -> bool {
    match ty {
        Type::Option(inner) => inline_type_reaches(inner, target, ctx, scope, seen),
        Type::Result(ok, err) => {
            inline_type_reaches(ok, target, ctx, scope, seen)
                || inline_type_reaches(err, target, ctx, scope, seen)
        }
        Type::Tuple(items) => items
            .iter()
            .any(|item| inline_type_reaches(item, target, ctx, scope, seen)),
        Type::Named { id, name } => named_type_def(*id, name, ctx, scope).is_some_and(|next| {
            let key = crate::codegen::common::backend_type_def_key(ctx, next);
            key == target || (seen.insert(key) && reaches_inline(next, target, ctx, seen))
        }),
        _ => false,
    }
}

fn capabilities_for_def(
    td: &TypeDef,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> RepresentationCapabilities {
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    if !visiting.insert(key.clone()) {
        // Recursive fields are emitted through `Shared`. Assume the cycle's
        // own promise and let every non-recursive field narrow it.
        return RepresentationCapabilities::ALL;
    }

    let capabilities = if super::uses_packed_u8(ctx, crate::codegen::common::type_def_name(td)) {
        // `AverPackedU8` explicitly implements all five operations, with
        // equality/hash compatible with the ordinary `List<Int>` carrier.
        RepresentationCapabilities::ALL
    } else {
        fields_of(td).fold(RepresentationCapabilities::ALL, |caps, field| {
            caps.intersect(capabilities_for_type(
                &parse_type_str(field),
                ctx,
                crate::codegen::common::type_key_for_decl(ctx, td).scope_str(),
                visiting,
            ))
        })
    };

    visiting.remove(&key);
    capabilities
}

fn capabilities_for_type(
    ty: &Type,
    ctx: &CodegenContext,
    scope: Option<&str>,
    visiting: &mut HashSet<String>,
) -> RepresentationCapabilities {
    match ty {
        Type::Int | Type::Bool | Type::Unit | Type::Str => RepresentationCapabilities::ALL,
        Type::Float => RepresentationCapabilities {
            clone: true,
            partial_eq: true,
            eq: false,
            hash: false,
            aver_display: true,
        },
        Type::Result(ok, err) => capabilities_for_type(ok, ctx, scope, visiting)
            .intersect(capabilities_for_type(err, ctx, scope, visiting)),
        Type::Option(inner) => capabilities_for_type(inner, ctx, scope, visiting),
        Type::Tuple(inner) => inner
            .iter()
            .fold(RepresentationCapabilities::ALL, |caps, item| {
                caps.intersect(capabilities_for_type(item, ctx, scope, visiting))
            }),
        Type::List(inner) => {
            let inner = capabilities_for_type(inner, ctx, scope, visiting);
            RepresentationCapabilities {
                // AverList and AverIntList clone their shared carrier.
                clone: true,
                partial_eq: inner.partial_eq,
                eq: inner.eq,
                hash: inner.hash,
                aver_display: inner.aver_display,
            }
        }
        Type::Vector(inner) => {
            let inner = capabilities_for_type(inner, ctx, scope, visiting);
            RepresentationCapabilities {
                // AverVector clones its Rc independently of T.
                clone: true,
                partial_eq: inner.partial_eq,
                eq: inner.eq,
                hash: inner.hash,
                // Its display iterator needs cloned elements.
                aver_display: inner.clone && inner.aver_display,
            }
        }
        Type::Map(key, value) => {
            let key_caps = capabilities_for_type(key, ctx, scope, visiting);
            let value_caps = capabilities_for_type(value, ctx, scope, visiting);
            let key_base = key_caps.clone && key_caps.eq && key_caps.hash;
            RepresentationCapabilities {
                // AverMap clones its Rc independently of K/V.
                clone: true,
                partial_eq: key_base
                    && key_caps.partial_eq
                    && value_caps.clone
                    && value_caps.partial_eq,
                eq: key_base && value_caps.clone && value_caps.eq,
                hash: key_base
                    && orderable_type(key, ctx, scope, &mut HashSet::new())
                    && value_caps.clone
                    && value_caps.hash,
                aver_display: key_base
                    && key_caps.aver_display
                    && orderable_type(key, ctx, scope, &mut HashSet::new())
                    && value_caps.clone
                    && value_caps.aver_display,
            }
        }
        Type::Named { id, name } => {
            if is_provider_resource(*id, name, ctx, scope) {
                return RepresentationCapabilities::PROVIDER_RESOURCE;
            }
            let Some(td) = named_type_def(*id, name, ctx, scope) else {
                // String.Index is compiler plumbing and never a surface field;
                // every other miss is fail-closed instead of borrowing traits
                // from an unrelated same-named declaration.
                return if name == "String.Index" {
                    RepresentationCapabilities {
                        clone: true,
                        ..RepresentationCapabilities::NONE
                    }
                } else {
                    RepresentationCapabilities::NONE
                };
            };
            capabilities_for_def(td, ctx, visiting)
        }
        // Function values cannot inhabit records/sums (the checker restricts
        // Fn to direct parameters). Keep only the carrier's cheap clone fact;
        // a leaked Fn field therefore loses every generated structural trait.
        Type::Fn(..) => RepresentationCapabilities {
            clone: true,
            ..RepresentationCapabilities::NONE
        },
        Type::Var(_) | Type::Invalid => RepresentationCapabilities::NONE,
    }
}

fn orderable_def(td: &TypeDef, ctx: &CodegenContext, visiting: &mut HashSet<String>) -> bool {
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    if !visiting.insert(key.clone()) {
        return true;
    }
    let orderable = if super::uses_packed_u8(ctx, crate::codegen::common::type_def_name(td)) {
        true
    } else {
        fields_of(td).all(|field| {
            orderable_type(
                &parse_type_str(field),
                ctx,
                crate::codegen::common::type_key_for_decl(ctx, td).scope_str(),
                visiting,
            )
        })
    };
    visiting.remove(&key);
    orderable
}

fn orderable_type(
    ty: &Type,
    ctx: &CodegenContext,
    scope: Option<&str>,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        Type::Int | Type::Bool | Type::Unit | Type::Str => true,
        Type::Option(inner) | Type::List(inner) => orderable_type(inner, ctx, scope, visiting),
        Type::Result(ok, err) => {
            orderable_type(ok, ctx, scope, visiting) && orderable_type(err, ctx, scope, visiting)
        }
        Type::Tuple(items) => items
            .iter()
            .all(|item| orderable_type(item, ctx, scope, visiting)),
        Type::Named { id, name } => {
            if is_provider_resource(*id, name, ctx, scope) {
                return false;
            }
            named_type_def(*id, name, ctx, scope).is_some_and(|td| orderable_def(td, ctx, visiting))
        }
        Type::Float
        | Type::Map(_, _)
        | Type::Vector(_)
        | Type::Fn(..)
        | Type::Var(_)
        | Type::Invalid => false,
    }
}

fn named_type_def<'a>(
    id: Option<crate::ir::TypeId>,
    name: &str,
    ctx: &'a CodegenContext,
    scope: Option<&str>,
) -> Option<&'a TypeDef> {
    let key = id
        .map(|id| ctx.symbol_table.type_entry(id).key.clone())
        .unwrap_or_else(|| crate::codegen::common::type_key_for_name(ctx, name, scope));
    match key.scope_str() {
        None => ctx
            .type_defs
            .iter()
            .find(|td| crate::codegen::common::type_def_name(td) == key.name),
        Some(owner) => ctx
            .modules
            .iter()
            .find(|module| module.prefix == owner)?
            .type_defs
            .iter()
            .find(|td| crate::codegen::common::type_def_name(td) == key.name),
    }
}

fn is_provider_resource(
    id: Option<crate::ir::TypeId>,
    name: &str,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> bool {
    let id = id.or_else(|| {
        let key = crate::codegen::common::type_key_for_name(ctx, name, scope);
        ctx.symbol_table.type_id_of(&key)
    });
    if id.is_some_and(|id| ctx.symbol_table.type_entry(id).is_capability_resource) {
        return true;
    }
    ctx.capabilities
        .resource_types()
        .any(|canonical| canonical == name)
}

fn fields_of(td: &TypeDef) -> impl Iterator<Item = &String> {
    let fields: Vec<&String> = match td {
        TypeDef::Sum { variants, .. } => variants
            .iter()
            .flat_map(|variant| variant.fields.iter())
            .collect(),
        TypeDef::Product { fields, .. } => fields.iter().map(|(_, ty)| ty).collect(),
    };
    fields.into_iter()
}
