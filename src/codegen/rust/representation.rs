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
