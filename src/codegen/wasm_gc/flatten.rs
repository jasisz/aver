//! Single-binary multi-module flattening for the wasm-gc backend.
//!
//! The wasm-gc backend emits one standalone module today. Dependent Aver
//! modules are therefore inlined into the entry module by prefixing function
//! names with their module path and rewriting cross-module calls to those
//! flat function names.
//!
//! Type identity follows two rules:
//!
//! - **Non-colliding types** (a bare name declared by exactly one dep
//!   module and not by the entry module): the dep `TypeDef.name` stays
//!   bare. Entry-side qualified references like
//!   `record Wrapper { status: TmpReviewB.Status }` get their module prefix
//!   stripped to the bare form, matching the registry's bare key. Stamped
//!   expression types from the pre-flatten typechecker also use bare names
//!   and resolve correctly.
//! - **Colliding types** (same bare name declared by two or more dep
//!   modules — see #180 Phase 6 PR 3 — or by a dep module and the entry
//!   module, where the language rule is that the local declaration shadows
//!   the dependency's): dep `TypeDef.name` gets renamed to the canonical
//!   `"Prefix.Name"` form (`Left.Box`, `Right.Box`, `Palette.Colour`) so the
//!   wasm-gc `TypeRegistry` keys them into distinct slots. Dep-internal
//!   bare references to a colliding name (in `TypeDef` field types,
//!   `FnDef` signatures, body `Constructor` / `RecordCreate` / `RecordUpdate`
//!   / pattern heads, let-binding annotations) get rewritten to canonical
//!   so the post-flatten resolver + registry agree. Qualified references
//!   from other modules stay verbatim (no strip) for the same reason.
//!
//!   The reference is canonicalised to the module that declares what the
//!   name means IN THAT MODULE: its own declaration when it has one, else
//!   the single dependency declaration. Rewriting only a module's OWN types
//!   left a bare reference to a sibling dependency's colliding type
//!   untouched, and after flattening it resolved against the entry's
//!   declaration instead — a module the dependency never imported. The
//!   function then failed to lower and shipped as a trap stub.
//!
//! Both rules apply to a **constructor reference** — `Dep.Status.Open` in an
//! expression or as a `match` arm's head — exactly as they apply to a type
//! reference, because the name a constructor is written with is the name of
//! its type plus the variant. A qualified one loses its module path when the
//! declaration kept the bare spelling and keeps it when the declaration was
//! renamed.
//!
//! Treating collision as the trigger keeps the migration narrow: the legacy
//! strip path and bare-name lookups continue to work for every existing
//! single-declarer dep type, and only the genuinely-ambiguous slot pair gets
//! the canonical-key routing.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, Pattern, Spanned, Stmt, TopLevel, TypeDef};
use crate::codegen::ModuleInfo;
use crate::codegen::common::type_def_name;

/// Walks each loaded `ModuleInfo`, prefixes every fn name with
/// `{module_prefix}_`, rewrites bare same-module call sites to use the
/// prefixed name, strips non-colliding module-qualified type refs to their
/// bare form, canonicalises colliding own-type refs to `Prefix.Name`, then
/// appends the dep's renamed `TypeDef`s + prefixed `FnDef`s onto the entry
/// items.
///
/// Returns the identity-preserving qualified type-name aliases: for every
/// dep type whose post-flatten `TypeDef` name stays BARE (sole declarer
/// among the deps, and no entry type shares the bare name), the qualified
/// spelling the entry module may have used (`"Dep.Octets"`) maps to that
/// bare post-flatten name (`"Octets"`). Entry-side local-binding
/// annotations keep their qualified spelling through the pre-flatten
/// typechecker's type stamps, which survive into codegen — the wasm-gc
/// registry registers these aliases so a stamped `Dep.Octets` resolves the
/// SAME proof-derived layout facts as `Octets`. Fail-closed: a bare name
/// declared by two+ deps is collision-renamed (its canonical `TypeDef`
/// name is already the qualified form — exact lookups work, no alias), and
/// a bare name also declared by the entry module gets NO alias because the
/// qualified spelling would be ambiguous against the entry type.
///
/// Component Model is a future separate mode; this single-binary path is the
/// bench-friendly and playground-friendly default.
pub fn flatten_multimodule(
    items: &mut Vec<TopLevel>,
    dep_modules: &[ModuleInfo],
) -> HashMap<String, String> {
    if dep_modules.is_empty() {
        return HashMap::new();
    }

    // Capability operations are host/provider atoms, not dependency function
    // bodies. Keep standard operations such as `Disk.exists`, `Random.int`,
    // and `Time.now` as qualified capability identities so the backend binding
    // table can lower them; rewriting one to a nonexistent flattened function
    // such as `Time_now` would produce a trap stub.
    let prefixes: HashSet<String> = dep_modules
        .iter()
        .filter(|module| module.capability_semantics.is_none())
        .map(|module| module.prefix.clone())
        .collect();

    // Bare-name → owning dep prefix(es) across all dep modules. A bare
    // name that appears in two+ dep entries is "colliding"; the wasm-gc
    // `TypeRegistry` keys those canonically (`Left.Box` / `Right.Box`)
    // so the two slots don't merge under one bare key.
    let mut bare_owners: HashMap<String, Vec<String>> = HashMap::new();
    for dep in dep_modules {
        for td in &dep.type_defs {
            bare_owners
                .entry(type_def_name(td).to_string())
                .or_default()
                .push(dep.prefix.clone());
        }
    }

    // Entry-declared bare type names, collected BEFORE dep typedefs are
    // appended: the entry module owns the bare spelling of any name it
    // declares, so a dep type of that name is just as ambiguous as a
    // dep-dep twin and must be canonicalised too.
    let entry_bare_names: HashSet<String> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::TypeDef(td) => Some(type_def_name(td).to_string()),
            _ => None,
        })
        .collect();

    // A bare name is ambiguous when two or more deps declare it, OR when
    // the entry module declares it as well: a local declaration shadows a
    // dependency's same-named one, so the two are distinct types that need
    // distinct registry slots. The entry keeps the bare spelling (matching
    // the canonical key the symbol table hands `named_type_registry_key`)
    // and the dep declaration is renamed to `Prefix.Name`.
    let colliding_bare_names: HashSet<String> = bare_owners
        .iter()
        .filter(|(bare, owners)| owners.len() > 1 || entry_bare_names.contains(bare.as_str()))
        .map(|(bare, _)| bare.clone())
        .collect();

    let mut type_aliases: HashMap<String, String> = HashMap::new();
    for (bare, owners) in &bare_owners {
        if owners.len() == 1 && !entry_bare_names.contains(bare.as_str()) {
            type_aliases.insert(format!("{}.{}", owners[0], bare), bare.clone());
        }
    }

    let qualified_type_names: HashSet<String> = dep_modules
        .iter()
        .flat_map(|dep| {
            dep.type_defs.iter().map(|td| {
                let name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name.clone(),
                };
                format!("{}.{}", dep.prefix, name)
            })
        })
        .collect();

    let empty_owner: HashMap<String, String> = HashMap::new();
    let empty_set: HashSet<String> = HashSet::new();

    let entry_ctx = RewriteCtx {
        prefixes: &prefixes,
        qualified_type_names: &qualified_type_names,
        same_module_prefix: None,
        same_module_fns: &empty_set,
        // The entry keeps the bare spelling of every name it declares, so
        // nothing in its own body needs canonicalising.
        colliding_owner: &empty_owner,
        dep_prefix: "",
        colliding_bare_names: &colliding_bare_names,
    };
    for item in items.iter_mut() {
        match item {
            TopLevel::FnDef(fd) => {
                rewrite_fn_signature(fd, &qualified_type_names, &colliding_bare_names);
                let body_arc = std::sync::Arc::make_mut(&mut fd.body);
                let FnBody::Block(stmts) = body_arc;
                rewrite_stmts(stmts, &entry_ctx);
            }
            TopLevel::TypeDef(td) => {
                rewrite_type_def(td, &qualified_type_names, &colliding_bare_names);
            }
            _ => {}
        }
    }

    for dep in dep_modules {
        let same_module_fns: HashSet<String> =
            dep.fn_defs.iter().map(|fd| fd.name.clone()).collect();
        let own_types: HashSet<&str> = dep.type_defs.iter().map(type_def_name).collect();
        // What each ambiguous bare name means INSIDE this module — the
        // canonicalisation passes rewrite its references to `Owner.Name`
        // (only ambiguous ones; a name only one module declares keeps
        // resolving against the registry's bare key).
        //
        // Its own declaration first: a module's own type wins its own bare
        // name. Otherwise the single dependency declaration of that name,
        // which is what the module imported it under. The entry's
        // declaration is never a candidate — flattening puts every module
        // in one scope, but a dependency still does not import the entry,
        // and letting the entry claim the name here is what made a
        // dependency's constructor resolve to a record it had never heard
        // of and lower to a trap stub.
        let colliding_owner: HashMap<String, String> = colliding_bare_names
            .iter()
            .filter_map(|bare| {
                if own_types.contains(bare.as_str()) {
                    return Some((bare.clone(), dep.prefix.clone()));
                }
                match bare_owners.get(bare).map(Vec::as_slice) {
                    Some([only]) => Some((bare.clone(), only.clone())),
                    _ => None,
                }
            })
            .collect();
        let dep_ctx = RewriteCtx {
            prefixes: &prefixes,
            qualified_type_names: &qualified_type_names,
            same_module_prefix: Some(&dep.prefix),
            same_module_fns: &same_module_fns,
            colliding_owner: &colliding_owner,
            dep_prefix: &dep.prefix,
            colliding_bare_names: &colliding_bare_names,
        };

        for td in &dep.type_defs {
            let mut new_td = td.clone();
            rewrite_type_def(&mut new_td, &qualified_type_names, &colliding_bare_names);
            // Rename the typedef itself to `Prefix.Name` if it collides
            // with another dep's bare name.
            rename_typedef_if_colliding(&mut new_td, &dep.prefix, &colliding_bare_names);
            items.push(TopLevel::TypeDef(new_td));
        }

        for fd in &dep.fn_defs {
            let mut new_fd = fd.clone();
            rewrite_fn_signature(&mut new_fd, &qualified_type_names, &colliding_bare_names);
            canonicalise_fn_signature_for_own_colliding(&mut new_fd, &colliding_owner);

            let body_arc = std::sync::Arc::make_mut(&mut new_fd.body);
            let FnBody::Block(stmts) = body_arc;
            rewrite_stmts(stmts, &dep_ctx);

            new_fd.name = prefixed(&dep.prefix, &fd.name);
            items.push(TopLevel::FnDef(new_fd));
        }
    }

    type_aliases
}

fn prefixed(prefix: &str, name: &str) -> String {
    format!("{}_{}", prefix.replace('.', "_"), name)
}

fn rewrite_fn_signature(
    fd: &mut crate::ast::FnDef,
    qualified_type_names: &HashSet<String>,
    colliding_bare_names: &HashSet<String>,
) {
    for (_, ty) in fd.params.iter_mut() {
        *ty = strip_non_colliding_prefixes(ty, qualified_type_names, colliding_bare_names);
    }
    fd.return_type =
        strip_non_colliding_prefixes(&fd.return_type, qualified_type_names, colliding_bare_names);
}

fn rewrite_type_def(
    td: &mut TypeDef,
    qualified_type_names: &HashSet<String>,
    colliding_bare_names: &HashSet<String>,
) {
    match td {
        TypeDef::Sum { variants, .. } => {
            for variant in variants {
                for ty in variant.fields.iter_mut() {
                    *ty = strip_non_colliding_prefixes(
                        ty,
                        qualified_type_names,
                        colliding_bare_names,
                    );
                }
            }
        }
        TypeDef::Product { fields, .. } => {
            for (_, ty) in fields.iter_mut() {
                *ty = strip_non_colliding_prefixes(ty, qualified_type_names, colliding_bare_names);
            }
        }
    }
}

fn rename_typedef_if_colliding(
    td: &mut TypeDef,
    dep_prefix: &str,
    colliding_bare_names: &HashSet<String>,
) {
    match td {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => {
            if colliding_bare_names.contains(name.as_str()) {
                *name = format!("{dep_prefix}.{name}");
            }
        }
    }
}

fn canonicalise_fn_signature_for_own_colliding(
    fd: &mut crate::ast::FnDef,
    colliding_owner: &HashMap<String, String>,
) {
    if colliding_owner.is_empty() {
        return;
    }
    for (_, ty) in fd.params.iter_mut() {
        *ty = canonicalise_own_colliding(ty, colliding_owner);
    }
    fd.return_type = canonicalise_own_colliding(&fd.return_type, colliding_owner);
}

/// Strip module prefixes from qualified type references in `type_str`,
/// but leave colliding refs canonical. `Vector<TmpReviewB.Status>` becomes
/// `Vector<Status>`; `Vector<Left.Box>` stays as `Vector<Left.Box>` so
/// downstream `TypeRegistry` lookups land on the right slot.
fn strip_non_colliding_prefixes(
    type_str: &str,
    qualified_type_names: &HashSet<String>,
    colliding_bare_names: &HashSet<String>,
) -> String {
    if qualified_type_names.is_empty() {
        return type_str.to_string();
    }
    let mut out = type_str.to_string();
    for qualified in qualified_type_names {
        let Some((_, bare)) = qualified.rsplit_once('.') else {
            continue;
        };
        if colliding_bare_names.contains(bare) {
            // Two+ deps declare this bare name — keep the qualified form
            // so the wasm-gc registry can disambiguate by canonical key.
            continue;
        }
        out = replace_qualified_type(&out, qualified, bare);
    }
    out
}

/// Rewrite bare colliding type references in `type_str` to `Owner.Name`,
/// where the owner is the module that declares what the name means in the
/// module being walked. Run after the strip pass so the two transforms
/// compose cleanly.
fn canonicalise_own_colliding(type_str: &str, colliding_owner: &HashMap<String, String>) -> String {
    if colliding_owner.is_empty() {
        return type_str.to_string();
    }
    let mut out = String::with_capacity(type_str.len());
    let bytes = type_str.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_alphabetic() || b == b'_' {
            let start = i;
            while i < bytes.len()
                && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'.')
            {
                i += 1;
            }
            let token = &type_str[start..i];
            match colliding_owner.get(token).filter(|_| !token.contains('.')) {
                Some(owner) => {
                    out.push_str(owner);
                    out.push('.');
                    out.push_str(token);
                }
                None => out.push_str(token),
            }
        } else {
            out.push(b as char);
            i += 1;
        }
    }
    out
}

fn replace_qualified_type(input: &str, qualified: &str, bare: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut i = 0;
    let bytes = input.as_bytes();
    while i < bytes.len() {
        let rest = &input[i..];
        if rest.starts_with(qualified) {
            let before_ok = i == 0 || is_type_boundary(bytes[i - 1]);
            let after = i + qualified.len();
            let after_ok = after >= bytes.len() || is_type_boundary(bytes[after]);
            if before_ok && after_ok {
                out.push_str(bare);
                i = after;
                continue;
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

fn is_type_boundary(byte: u8) -> bool {
    matches!(
        byte,
        b'<' | b'>' | b',' | b' ' | b'\t' | b'\n' | b'\r' | b'(' | b')'
    )
}

/// Flatten a chained `Attr` expression into its dotted form. Returns `None`
/// for any non-Attr or any Attr whose root is not an Ident.
fn attr_chain_to_dotted(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(parent, member) => {
            let head = attr_chain_to_dotted(&parent.node)?;
            Some(format!("{head}.{member}"))
        }
        _ => None,
    }
}

/// Invariants for one body-rewriting traversal. `dep_prefix` is empty and
/// `colliding_owner` is empty while the entry module's own items are
/// walked, so the canonicalisation passes keyed on them are inert there.
struct RewriteCtx<'a> {
    /// Every dep module path, used to spot cross-module references.
    prefixes: &'a HashSet<String>,
    /// Every dependency-qualified type spelling that the post-link program
    /// may need to rewrite in annotations and inferred type stamps.
    qualified_type_names: &'a HashSet<String>,
    /// The owning dep's path while its own fn bodies are walked.
    same_module_prefix: Option<&'a str>,
    /// Fn names declared by the module being walked.
    same_module_fns: &'a HashSet<String>,
    /// Ambiguous bare type names as the module being walked reads them:
    /// name → the module that declares what the name means HERE. Its own
    /// declaration when it has one, otherwise the single declaration
    /// elsewhere among the dependencies. The entry's declaration is never a
    /// candidate — a dependency does not import the entry.
    colliding_owner: &'a HashMap<String, String>,
    /// The owning dep's path, or `""` for the entry module.
    dep_prefix: &'a str,
    /// Every ambiguous bare type name in the program — a reference to one
    /// of these through another module's path keeps the qualified spelling.
    colliding_bare_names: &'a HashSet<String>,
}

/// Rewrite both an expression and the typechecker stamp attached to it. The
/// stamp belongs to the pre-flatten symbol table, so its `TypeId` must never
/// cross the link boundary. Its source spelling is rewritten to the same
/// post-link name as signatures and type definitions; the fresh HIR resolver
/// then assigns an id from the flattened table.
fn rewrite_spanned_expr(expr: &mut Spanned<Expr>, ctx: &RewriteCtx<'_>) {
    if let Some(ty) = expr.ty.get_mut() {
        rewrite_stamped_type(ty, ctx);
    }
    rewrite_expr(&mut expr.node, ctx);
}

fn rewrite_stamped_type(ty: &mut crate::ast::Type, ctx: &RewriteCtx<'_>) {
    use crate::ast::Type;
    match ty {
        Type::Named { id, name } => {
            *name = rewrite_type_spelling(name, ctx);
            // TypeIds are table-local. `flatten_multimodule` changes the
            // table, so carrying the old numeric id would misidentify a
            // different declaration (or index past the new table).
            *id = None;
        }
        Type::List(inner) | Type::Vector(inner) | Type::Option(inner) => {
            rewrite_stamped_type(inner, ctx);
        }
        Type::Result(ok, err) | Type::Map(ok, err) => {
            rewrite_stamped_type(ok, ctx);
            rewrite_stamped_type(err, ctx);
        }
        Type::Tuple(items) => {
            for item in items {
                rewrite_stamped_type(item, ctx);
            }
        }
        Type::Fn(params, ret, _) => {
            for param in params {
                rewrite_stamped_type(param, ctx);
            }
            rewrite_stamped_type(ret, ctx);
        }
        _ => {}
    }
}

fn rewrite_type_spelling(type_name: &str, ctx: &RewriteCtx<'_>) -> String {
    let stripped = strip_non_colliding_prefixes(
        type_name,
        ctx.qualified_type_names,
        ctx.colliding_bare_names,
    );
    if ctx.dep_prefix.is_empty() {
        stripped
    } else {
        canonicalise_own_colliding(&stripped, ctx.colliding_owner)
    }
}

fn rewrite_expr(expr: &mut Expr, ctx: &RewriteCtx<'_>) {
    match expr {
        Expr::FnCall(callee, args) => {
            let mut new_callee: Option<Expr> = None;
            if let Expr::Attr(parent, member) = &callee.node {
                if let Expr::Ident(p) = &parent.node
                    && ctx.prefixes.contains(p)
                {
                    new_callee = Some(Expr::Ident(prefixed(p, member)));
                } else if let Some(dotted) = attr_chain_to_dotted(&callee.node) {
                    new_callee =
                        rewrite_dotted_module_ref(&dotted, ctx.prefixes, ctx.colliding_bare_names);
                }
            }
            if new_callee.is_none()
                && let Expr::Ident(name) = &callee.node
                && let Some(prefix) = ctx.same_module_prefix
                && ctx.same_module_fns.contains(name)
            {
                new_callee = Some(Expr::Ident(prefixed(prefix, name)));
            }
            if let Some(rep) = new_callee {
                callee.node = rep;
            }
            rewrite_spanned_expr(callee, ctx);
            for arg in args.iter_mut() {
                rewrite_spanned_expr(arg, ctx);
            }
        }
        Expr::TailCall(boxed) => {
            if let Some(prefix) = ctx.same_module_prefix
                && ctx.same_module_fns.contains(&boxed.target)
            {
                boxed.target = prefixed(prefix, &boxed.target);
            }
            for arg in boxed.args.iter_mut() {
                rewrite_spanned_expr(arg, ctx);
            }
        }
        Expr::BinOp(_, left, right) => {
            rewrite_spanned_expr(left, ctx);
            rewrite_spanned_expr(right, ctx);
        }
        Expr::Neg(inner) => {
            rewrite_spanned_expr(inner, ctx);
        }
        Expr::Match { subject, arms } => {
            rewrite_spanned_expr(subject, ctx);
            for arm in arms.iter_mut() {
                rewrite_pattern(&mut arm.pattern, ctx);
                rewrite_spanned_expr(&mut arm.body, ctx);
            }
        }
        Expr::Attr(_, _) => {
            // Cross-module call shape (`Worker.helper`) first — produces
            // an `Ident("Worker_helper")` for FnCall callees. Doing the
            // own-colliding canonicalisation before this would let the
            // canonicalised form be mis-matched as a cross-module access
            // and unrewritten back to bare.
            let rewrite = attr_chain_to_dotted(expr).and_then(|dotted| {
                rewrite_dotted_module_ref(&dotted, ctx.prefixes, ctx.colliding_bare_names)
            });
            if let Some(new_node) = rewrite {
                *expr = new_node;
                return;
            }
            if !ctx.dep_prefix.is_empty() {
                canonicalise_attr_head_for_own_colliding(expr, ctx.colliding_owner);
            }
            if let Expr::Attr(obj, _) = expr {
                rewrite_spanned_expr(obj, ctx);
            }
        }
        Expr::Constructor(name, payload) => {
            rewrite_constructor_name(name, ctx);
            if let Some(payload) = payload.as_deref_mut() {
                rewrite_spanned_expr(payload, ctx);
            }
        }
        Expr::RecordCreate { type_name, fields } => {
            if !ctx.dep_prefix.is_empty() {
                canonicalise_bare_type_name(type_name, ctx.colliding_owner);
            }
            for (_, expr) in fields.iter_mut() {
                rewrite_spanned_expr(expr, ctx);
            }
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            if !ctx.dep_prefix.is_empty() {
                canonicalise_bare_type_name(type_name, ctx.colliding_owner);
            }
            rewrite_spanned_expr(base, ctx);
            for (_, expr) in updates.iter_mut() {
                rewrite_spanned_expr(expr, ctx);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items.iter_mut() {
                rewrite_spanned_expr(item, ctx);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries.iter_mut() {
                rewrite_spanned_expr(key, ctx);
                rewrite_spanned_expr(value, ctx);
            }
        }
        Expr::ErrorProp(inner) => {
            rewrite_spanned_expr(inner, ctx);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts.iter_mut() {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    rewrite_spanned_expr(inner, ctx);
                }
            }
        }
        _ => {}
    }
}

fn canonicalise_bare_type_name(type_name: &mut String, colliding_owner: &HashMap<String, String>) {
    if type_name.contains('.') {
        return;
    }
    if let Some(owner) = colliding_owner.get(type_name.as_str()) {
        *type_name = format!("{owner}.{type_name}");
    }
}

fn canonicalise_constructor_name(name: &mut String, colliding_owner: &HashMap<String, String>) {
    let head = name.split('.').next().unwrap_or(name);
    if let Some(owner) = colliding_owner.get(head) {
        let rest = &name[head.len()..];
        *name = format!("{owner}.{head}{rest}");
    }
}

fn canonicalise_attr_head_for_own_colliding(
    expr: &mut Expr,
    colliding_owner: &HashMap<String, String>,
) {
    fn walk(e: &mut Expr, colliding_owner: &HashMap<String, String>, depth: u32) {
        if depth > 32 {
            return;
        }
        match e {
            Expr::Attr(inner, _) => walk(&mut inner.node, colliding_owner, depth + 1),
            Expr::Ident(name) if !name.contains('.') => {
                if let Some(owner) = colliding_owner.get(name.as_str()) {
                    *name = format!("{owner}.{name}");
                }
            }
            _ => {}
        }
    }
    walk(expr, colliding_owner, 0);
}

/// Every constructor reference a pattern holds, rewritten the way the
/// module being walked reads it: the module path a qualified reference
/// carries is replaced by the post-flatten spelling of the type it names,
/// and a bare ambiguous name is canonicalised to its owner.
///
/// Expression position gets the first of those through
/// [`rewrite_dotted_module_ref`], since the parser leaves
/// `Dep.Type.Variant` as an `Attr` chain. A pattern head is one string no
/// other pass touches, so `match` over an imported type's variants kept
/// naming `Dep.Type.Variant` after flattening had renamed the declaration
/// to `Type` — nothing resolved it, and the whole function shipped as a
/// trap stub that only traps once the caller reaches it.
fn rewrite_pattern(pat: &mut Pattern, ctx: &RewriteCtx<'_>) {
    match pat {
        Pattern::Tuple(inner) => {
            for pat in inner.iter_mut() {
                rewrite_pattern(pat, ctx);
            }
        }
        Pattern::Constructor(name, _bindings) => {
            rewrite_constructor_name(name, ctx);
        }
        _ => {}
    }
}

/// One constructor reference, in expression or in pattern position.
/// Qualified references lose the module path first — a name that still
/// carries one has nothing for the colliding-name pass to read.
fn rewrite_constructor_name(name: &mut String, ctx: &RewriteCtx<'_>) {
    if let Some(flattened) = flattened_qualified_ctor(name, ctx.prefixes, ctx.colliding_bare_names)
    {
        *name = flattened;
        return;
    }
    if !ctx.dep_prefix.is_empty() {
        canonicalise_constructor_name(name, ctx.colliding_owner);
    }
}

/// The post-flatten spelling of a constructor written with the module path
/// of the type that declares it: `TmpReviewB.Status.Open` → `Status.Open`,
/// and `Left.Box.Full` unchanged when `Box` is ambiguous and its
/// declaration therefore keeps the canonical `Left.Box` name. `None` when
/// nothing before the variant names a dependency module — which is every
/// reference already written in the post-flatten form.
fn flattened_qualified_ctor(
    name: &str,
    prefixes: &HashSet<String>,
    colliding_bare_names: &HashSet<String>,
) -> Option<String> {
    let segments: Vec<&str> = name.split('.').collect();
    let split = dep_prefix_split(&segments, prefixes)?;
    if segments.len() - split < 2 {
        return None;
    }
    let type_name = flattened_type_name(&segments, split, colliding_bare_names);
    Some(format!("{type_name}.{}", segments[segments.len() - 1]))
}

/// How many leading segments of `segments` name a dependency module, taking
/// the longest such run — `Domain.Value` wins over `Domain` when both are
/// module paths. `None` when no leading run names one.
fn dep_prefix_split(segments: &[&str], prefixes: &HashSet<String>) -> Option<usize> {
    (1..segments.len())
        .rev()
        .find(|split| prefixes.contains(&segments[..*split].join(".")))
}

/// The post-flatten name of the type `segments[split..len - 1]` spells.
/// A colliding dep type keeps its canonical `Prefix.Name` spelling
/// post-flatten, so the qualified reference must not collapse to the bare
/// name — that name denotes another module's declaration.
fn flattened_type_name(
    segments: &[&str],
    split: usize,
    colliding_bare_names: &HashSet<String>,
) -> String {
    let type_name = segments[split..segments.len() - 1].join("_");
    if colliding_bare_names.contains(&type_name) {
        return format!("{}.{type_name}", segments[..split].join("."));
    }
    type_name
}

fn rewrite_dotted_module_ref(
    dotted: &str,
    prefixes: &HashSet<String>,
    colliding_bare_names: &HashSet<String>,
) -> Option<Expr> {
    let segments: Vec<&str> = dotted.split('.').collect();
    let split = dep_prefix_split(&segments, prefixes)?;

    Some(match segments.len() - split {
        0 => return None,
        1 => Expr::Ident(prefixed(&segments[..split].join("."), segments[split])),
        _ => {
            let type_name = flattened_type_name(&segments, split, colliding_bare_names);
            let last = segments[segments.len() - 1].to_string();
            Expr::Attr(Box::new(Spanned::bare(Expr::Ident(type_name))), last)
        }
    })
}

fn rewrite_stmts(stmts: &mut [Stmt], ctx: &RewriteCtx<'_>) {
    for stmt in stmts.iter_mut() {
        match stmt {
            Stmt::Binding(_, type_ann, expr) => {
                if let Some(ty) = type_ann.as_mut() {
                    *ty = rewrite_type_spelling(ty, ctx);
                }
                rewrite_spanned_expr(expr, ctx);
            }
            Stmt::Expr(expr) => {
                rewrite_spanned_expr(expr, ctx);
            }
        }
    }
}
