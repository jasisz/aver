//! `[rust] intern`: recursive sums whose constructors give back the node
//! that already holds the same fields (see `aver_rt::intern`).
//!
//! Interning changes how fast a program is, never what it computes, so it is
//! chosen in `aver.toml` and not in the source, and only the Rust backend
//! reads it. A sum named there is laid out as without it, plus the table of
//! its canonical nodes (`aver_rt::thin_sum!` with `interned`), and each
//! constructor's fields say how the table compares them
//! (`aver_rt::intern::InternFields`): a field of an interned type by
//! identity, any other field by value. `==` on two canonical values compares
//! two pointers, and the hash of a value is the one its node keeps.

use std::collections::HashSet;
use std::fmt::Write as _;

use super::representation::{fields_of, is_thin_sum, named_type_def, type_def_contract};
use crate::ast::{TypeDef, TypeVariant};
use crate::codegen::CodegenContext;
use crate::codegen::common::{backend_type_def_key, type_def_name, type_key_for_decl};
use crate::types::{Type, parse_type_str};

/// Every type declaration of the program with its backend key.
fn declarations(ctx: &CodegenContext) -> impl Iterator<Item = (String, &TypeDef)> {
    ctx.type_defs
        .iter()
        .chain(
            ctx.modules
                .iter()
                .flat_map(|module| module.type_defs.iter()),
        )
        .map(|td| (backend_type_def_key(ctx, td), td))
}

/// The sums `[rust] intern` names, by backend key, or why a name cannot be
/// interned: it names no type of the program, a type that has no node of its
/// own on the heap, or one whose `==` is not an identity.
pub(crate) fn resolve(ctx: &CodegenContext, names: &[String]) -> Result<HashSet<String>, String> {
    let mut chosen = HashSet::new();
    let mut refused = Vec::new();
    for name in names {
        let Some((key, td)) = declarations(ctx).find(|(key, _)| key == name) else {
            let bare = name.rsplit('.').next().unwrap_or(name);
            let near: Vec<String> = declarations(ctx)
                .filter(|(_, td)| type_def_name(td) == bare)
                .map(|(key, _)| format!("`{key}`"))
                .collect();
            let hint = match near.is_empty() {
                true => String::new(),
                false => format!(" (did you mean {}?)", near.join(" or ")),
            };
            refused.push(format!(
                "`{name}` is not a type of this program{hint}; a type is named with the module that declares it, as in `Core.Exprs.Expr`"
            ));
            continue;
        };
        if !is_thin_sum(td, ctx) {
            let what = match td {
                TypeDef::Product { .. } => "a record",
                TypeDef::Sum { .. } => "a sum no value of which holds another one of it",
            };
            refused.push(format!(
                "`{name}` is {what}, which the Rust backend keeps inline rather than as a node on the heap; only a recursive sum can be interned"
            ));
            continue;
        }
        let capabilities = type_def_contract(td, ctx).capabilities;
        if !(capabilities.eq && capabilities.hash) {
            refused.push(format!(
                "`{name}` holds a Float, a function or a resource, whose `==` is not an identity, so two equal values cannot be one node"
            ));
            continue;
        }
        chosen.insert(key);
    }
    match refused.is_empty() {
        true => Ok(chosen),
        false => Err(format!("aver.toml: [rust].intern: {}", refused.join("; "))),
    }
}

/// How the table compares one field of a constructor of an interned sum.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Field {
    /// A value of an interned sum, or an `Option` of one: by identity.
    Canon,
    /// Any other value: by `==` and its hash.
    Value,
    /// A value that may hold a call `lazy` deferred, which comparing or
    /// hashing it would make: the constructor is never interned.
    Deferred,
}

fn field_kind(ty: &Type, ctx: &CodegenContext, scope: Option<&str>) -> Field {
    let direct = match ty {
        Type::Option(inner) => inner.as_ref(),
        other => other,
    };
    if let Type::Named { id, name } = direct
        && let Some(td) = named_type_def(*id, name, ctx, scope)
        && ctx
            .rust_interned_sums
            .contains(&backend_type_def_key(ctx, td))
    {
        return Field::Canon;
    }
    match holds_deferred(ty, ctx, scope, &mut HashSet::new()) {
        true => Field::Deferred,
        false => Field::Value,
    }
}

/// Whether a value of `ty` can hold, at any depth, a value of a sum some
/// `lazy f(…)` returns.
fn holds_deferred(
    ty: &Type,
    ctx: &CodegenContext,
    scope: Option<&str>,
    seen: &mut HashSet<String>,
) -> bool {
    let mut on = |ty: &Type| holds_deferred(ty, ctx, scope, seen);
    match ty {
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => on(inner),
        Type::Result(a, b) | Type::Map(a, b) => on(a) || on(b),
        Type::Tuple(items) => items.iter().any(on),
        Type::Named { id, name } => named_type_def(*id, name, ctx, scope).is_some_and(|td| {
            let key = backend_type_def_key(ctx, td);
            if ctx.rust_lazy_sums.contains(&key) {
                return true;
            }
            let decl = type_key_for_decl(ctx, td);
            seen.insert(key)
                && fields_of(td).any(|field| {
                    holds_deferred(&parse_type_str(field), ctx, decl.scope_str(), seen)
                })
        }),
        _ => false,
    }
}

/// The `aver_rt::intern::InternFields` of the field struct `payload` of
/// constructor `variant` of an interned sum.
pub(super) fn intern_fields_impl(
    payload: &str,
    variant: &TypeVariant,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    let kinds: Vec<Field> = variant
        .fields
        .iter()
        .map(|field| field_kind(&parse_type_str(field), ctx, scope))
        .collect();
    let canon = "aver_rt::intern::Canon";
    let settle = match kinds.contains(&Field::Deferred) {
        true => "false".to_string(),
        false => {
            let settled: Vec<String> = (0..kinds.len())
                .filter(|&i| kinds[i] == Field::Canon)
                .map(|i| format!("{canon}::settle(&mut self.{i})"))
                .collect();
            match settled.is_empty() {
                true => "true".to_string(),
                false => settled.join(" && "),
            }
        }
    };
    let mut hash = String::new();
    for (i, kind) in kinds.iter().enumerate() {
        match kind {
            Field::Canon => write!(
                hash,
                "\n        std::hash::Hasher::write_u64(hasher, {canon}::canon_hash(&self.{i}));"
            ),
            Field::Value | Field::Deferred => {
                write!(hash, "\n        std::hash::Hash::hash(&self.{i}, hasher);")
            }
        }
        .unwrap();
    }
    // The pointers first: they tell most nodes apart at once.
    let mut same: Vec<String> = (0..kinds.len())
        .filter(|&i| kinds[i] == Field::Canon)
        .map(|i| format!("{canon}::same(&self.{i}, &other.{i})"))
        .collect();
    same.extend(
        (0..kinds.len())
            .filter(|&i| kinds[i] != Field::Canon)
            .map(|i| format!("self.{i} == other.{i}")),
    );
    let same = match same.is_empty() {
        true => "true".to_string(),
        false => same.join(" && "),
    };
    let hasher = match hash.is_empty() {
        true => "_hasher",
        false => "hasher",
    };
    let other = match same.contains("other") {
        true => "other",
        false => "_other",
    };
    format!(
        "\nimpl aver_rt::intern::InternFields for {payload} {{\n    #[inline]\n    fn settle(&mut self) -> bool {{\n        {settle}\n    }}\n\n    #[inline]\n    fn hash_into(&self, {hasher}: &mut aver_rt::shared_hash::NodeHasher) {{{hash}\n    }}\n\n    #[inline]\n    fn same(&self, {other}: &Self) -> bool {{\n        {same}\n    }}\n}}\n"
    )
}

/// The `Hash` and `WalkHash` of an interned sum: the hash its node keeps.
pub(super) fn hash_impls(name: &str) -> String {
    format!(
        "\nimpl std::hash::Hash for {name} {{\n    #[inline]\n    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {{\n        state.write_u64({name}::canon_hash(self))\n    }}\n}}\n\nimpl aver_rt::WalkHash for {name} {{\n    #[inline]\n    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {{\n        state.write_u64({name}::canon_hash(self));\n        left\n    }}\n\n    #[inline]\n    fn walk_hash(&self, _: aver_rt::HashWalk<'_>) -> u64 {{\n        {name}::canon_hash(self)\n    }}\n}}\n"
    )
}

/// The `==` of an interned sum: one pointer compare when both values are
/// canonical, the walk `walk_eq` otherwise.
pub(super) fn partial_eq_impl(name: &str) -> String {
    format!(
        "\nimpl PartialEq for {name} {{\n    #[inline]\n    fn eq(&self, other: &Self) -> bool {{\n        match {name}::canon_eq(self, other) {{\n            Some(same) => same,\n            None => aver_rt::walk_eq(self, other),\n        }}\n    }}\n}}\n"
    )
}

/// The `WalkEq` of an interned sum: as [`partial_eq_impl`], `test` being
/// the walk.
pub(super) fn walk_eq_impl(name: &str, test: &str, by_node: bool) -> String {
    match by_node {
        true => format!(
            "\nimpl aver_rt::WalkEq for {name} {{\n    #[inline(always)]\n    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {{\n        match {name}::canon_eq(self, other) {{\n            Some(same) => same,\n            None => {name}::ptr_eq(self, other) || self.aver_walk_eq_nodes(other, walk),\n        }}\n    }}\n}}\n\nimpl {name} {{\n    #[inline(never)]\n    fn aver_walk_eq_nodes<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {{\n        use ::aver_rt::WalkEq as _;\n        {test}\n    }}\n}}\n"
        ),
        false => format!(
            "\nimpl aver_rt::WalkEq for {name} {{\n    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {{\n        if let Some(same) = {name}::canon_eq(self, other) {{\n            return same;\n        }}\n        {test}\n    }}\n}}\n"
        ),
    }
}
