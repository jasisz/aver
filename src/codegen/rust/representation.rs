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
/// recursive one must be to go behind an `Arc` (see [`shared_variants`]).
/// The pointer costs a value of the large constructor an allocation when it
/// is built and about 48 bytes (pointer, counts, allocator rounding); at a
/// 64-byte difference a value of a small one saves as much, so the shared
/// layout takes less memory whenever fewer than half the values are large.
/// Measured on lists of two million values: building only large ones is 8%
/// slower, only small ones 12% faster; at 32 bytes the large ones were 18%
/// slower and the memory broke even at a quarter of them.
const LARGE_VARIANT_BYTES: usize = 64;

/// Which constructors of a sum keep their fields in one shared `Arc`
/// (`T::Pair(Arc<T__Pair>)`), one entry per variant in declaration order.
///
/// Every constructor does in a sum a value of which can hold a value of the
/// same type with no collection in between: directly, or through records,
/// other sums, `Option`, `Result` or tuples. Such a type has a fixed size
/// only through a pointer somewhere on the cycle, and with the pointer on the
/// constructor a value used in many places is one allocation: copying it, or
/// putting it into another constructor, copies the pointer. Recursion through
/// `List`, `Vector` or `Map` already goes through the heap.
///
/// In any other sum only a constructor at least [`LARGE_VARIANT_BYTES`]
/// larger than every other one does, since a value of a sum takes the room
/// of its largest constructor: without the pointer a value of a small one
/// would carry that room unused.
pub(super) fn shared_variants(td: &TypeDef, ctx: &CodegenContext) -> Vec<bool> {
    let TypeDef::Sum { variants, .. } = td else {
        return Vec::new();
    };
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    if reaches_inline(td, &key, ctx, &mut HashSet::new()) {
        return vec![true; variants.len()];
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

/// The bytes a value of `ty` takes inline in the Rust it is lowered to, as
/// near as the size of the runtime's carriers allows: the measure
/// [`shared_variants`] compares constructors by.
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
                TypeDef::Product { fields, .. } => fields.iter().map(|(_, f)| field_size(f)).sum(),
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

/// The order a generated `==` compares the fields `types` of one record or
/// constructor in, when it is not the declared one: the fields that compare
/// at once (scalars, strings, and anything built only from them, such as a
/// sum whose constructors have no fields) first, then the ones that walk a
/// collection or a value of a sum with fields. A node that keeps its hash
/// after its children then tells two different nodes apart at the hash
/// instead of after walking both subtrees. Every field is still compared and
/// none has an effect, so the answer is the one in declared order.
pub(super) fn cheap_fields_first(
    types: &[&str],
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> Option<Vec<usize>> {
    let cheap: Vec<bool> = types
        .iter()
        .map(|ty| compares_at_once(&parse_type_str(ty), ctx, scope, &mut HashSet::new()))
        .collect();
    let order: Vec<usize> = (0..types.len())
        .filter(|&i| cheap[i])
        .chain((0..types.len()).filter(|&i| !cheap[i]))
        .collect();
    order
        .iter()
        .enumerate()
        .any(|(i, &f)| i != f)
        .then_some(order)
}

fn compares_at_once(
    ty: &Type,
    ctx: &CodegenContext,
    scope: Option<&str>,
    seen: &mut HashSet<String>,
) -> bool {
    match ty {
        Type::Int | Type::Float | Type::Bool | Type::Unit | Type::Str => true,
        Type::Option(inner) => compares_at_once(inner, ctx, scope, seen),
        Type::Result(ok, err) => {
            compares_at_once(ok, ctx, scope, seen) && compares_at_once(err, ctx, scope, seen)
        }
        Type::Tuple(items) => items
            .iter()
            .all(|item| compares_at_once(item, ctx, scope, seen)),
        Type::Named { id, name } => named_type_def(*id, name, ctx, scope).is_some_and(|td| {
            let key = crate::codegen::common::backend_type_def_key(ctx, td);
            if !seen.insert(key.clone()) {
                return false;
            }
            let decl = crate::codegen::common::type_key_for_decl(ctx, td);
            let cheap = fields_of(td)
                .all(|field| compares_at_once(&parse_type_str(field), ctx, decl.scope_str(), seen));
            seen.remove(&key);
            cheap
        }),
        Type::List(_) | Type::Vector(_) | Type::Map(..) | Type::Fn(..) => false,
        Type::Var(_) | Type::Invalid => false,
    }
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
        // Recursive fields are emitted through Arc<Self>. Assume the cycle's
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
