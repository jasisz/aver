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
//!   bare references to the colliding own type (in `TypeDef` field types,
//!   `FnDef` signatures, body `Constructor` / `RecordCreate` / `RecordUpdate`
//!   / pattern heads, let-binding annotations) get rewritten to canonical
//!   so the post-flatten resolver + registry agree. Qualified references
//!   from other modules stay verbatim (no strip) for the same reason.
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

    let prefixes: HashSet<String> = dep_modules.iter().map(|m| m.prefix.clone()).collect();

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

    let empty_own_colliding: HashSet<String> = HashSet::new();
    let empty_set: HashSet<String> = HashSet::new();

    let entry_ctx = RewriteCtx {
        prefixes: &prefixes,
        same_module_prefix: None,
        same_module_fns: &empty_set,
        own_colliding: &empty_own_colliding,
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
        // Own colliding type names — the canonicalisation passes use
        // this set to rewrite dep-internal bare refs to `Prefix.Name`
        // (only colliding ones need the rewrite; non-colliding bare
        // names continue to resolve against the registry's bare key).
        let own_colliding: HashSet<String> = dep
            .type_defs
            .iter()
            .filter_map(|td| {
                let bare = type_def_name(td).to_string();
                if colliding_bare_names.contains(&bare) {
                    Some(bare)
                } else {
                    None
                }
            })
            .collect();
        let dep_ctx = RewriteCtx {
            prefixes: &prefixes,
            same_module_prefix: Some(&dep.prefix),
            same_module_fns: &same_module_fns,
            own_colliding: &own_colliding,
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
            canonicalise_fn_signature_for_own_colliding(&mut new_fd, &dep.prefix, &own_colliding);

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
    dep_prefix: &str,
    own_colliding: &HashSet<String>,
) {
    if own_colliding.is_empty() {
        return;
    }
    for (_, ty) in fd.params.iter_mut() {
        *ty = canonicalise_own_colliding(ty, dep_prefix, own_colliding);
    }
    fd.return_type = canonicalise_own_colliding(&fd.return_type, dep_prefix, own_colliding);
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

/// Rewrite bare own-type references in `type_str` to `Prefix.Name` when
/// the bare name is in `own_colliding`. Run after the strip pass so the
/// two transforms compose cleanly.
fn canonicalise_own_colliding(
    type_str: &str,
    dep_prefix: &str,
    own_colliding: &HashSet<String>,
) -> String {
    if own_colliding.is_empty() {
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
            if !token.contains('.') && own_colliding.contains(token) {
                out.push_str(dep_prefix);
                out.push('.');
                out.push_str(token);
            } else {
                out.push_str(token);
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

/// Invariants for one body-rewriting traversal. `dep_prefix` /
/// `own_colliding` are empty while the entry module's own items are
/// walked, so the canonicalisation passes keyed on them are inert there.
struct RewriteCtx<'a> {
    /// Every dep module path, used to spot cross-module references.
    prefixes: &'a HashSet<String>,
    /// The owning dep's path while its own fn bodies are walked.
    same_module_prefix: Option<&'a str>,
    /// Fn names declared by the module being walked.
    same_module_fns: &'a HashSet<String>,
    /// Bare type names the module being walked declares that are ambiguous
    /// program-wide, so its own references to them need canonicalising.
    own_colliding: &'a HashSet<String>,
    /// The owning dep's path, or `""` for the entry module.
    dep_prefix: &'a str,
    /// Every ambiguous bare type name in the program — a reference to one
    /// of these through another module's path keeps the qualified spelling.
    colliding_bare_names: &'a HashSet<String>,
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
            rewrite_expr(&mut callee.node, ctx);
            for arg in args.iter_mut() {
                rewrite_expr(&mut arg.node, ctx);
            }
        }
        Expr::TailCall(boxed) => {
            if let Some(prefix) = ctx.same_module_prefix
                && ctx.same_module_fns.contains(&boxed.target)
            {
                boxed.target = prefixed(prefix, &boxed.target);
            }
            for arg in boxed.args.iter_mut() {
                rewrite_expr(&mut arg.node, ctx);
            }
        }
        Expr::BinOp(_, left, right) => {
            rewrite_expr(&mut left.node, ctx);
            rewrite_expr(&mut right.node, ctx);
        }
        Expr::Neg(inner) => {
            rewrite_expr(&mut inner.node, ctx);
        }
        Expr::Match { subject, arms } => {
            rewrite_expr(&mut subject.node, ctx);
            for arm in arms.iter_mut() {
                if !ctx.dep_prefix.is_empty() {
                    canonicalise_pattern(&mut arm.pattern, ctx.dep_prefix, ctx.own_colliding);
                }
                rewrite_expr(&mut arm.body.node, ctx);
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
                canonicalise_attr_head_for_own_colliding(expr, ctx.dep_prefix, ctx.own_colliding);
            }
            if let Expr::Attr(obj, _) = expr {
                rewrite_expr(&mut obj.node, ctx);
            }
        }
        Expr::Constructor(name, payload) => {
            if !ctx.dep_prefix.is_empty() {
                canonicalise_constructor_name(name, ctx.dep_prefix, ctx.own_colliding);
            }
            if let Some(payload) = payload.as_deref_mut() {
                rewrite_expr(&mut payload.node, ctx);
            }
        }
        Expr::RecordCreate { type_name, fields } => {
            if !ctx.dep_prefix.is_empty() {
                canonicalise_bare_type_name(type_name, ctx.dep_prefix, ctx.own_colliding);
            }
            for (_, expr) in fields.iter_mut() {
                rewrite_expr(&mut expr.node, ctx);
            }
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            if !ctx.dep_prefix.is_empty() {
                canonicalise_bare_type_name(type_name, ctx.dep_prefix, ctx.own_colliding);
            }
            rewrite_expr(&mut base.node, ctx);
            for (_, expr) in updates.iter_mut() {
                rewrite_expr(&mut expr.node, ctx);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items.iter_mut() {
                rewrite_expr(&mut item.node, ctx);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries.iter_mut() {
                rewrite_expr(&mut key.node, ctx);
                rewrite_expr(&mut value.node, ctx);
            }
        }
        Expr::ErrorProp(inner) => {
            rewrite_expr(&mut inner.node, ctx);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts.iter_mut() {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    rewrite_expr(&mut inner.node, ctx);
                }
            }
        }
        _ => {}
    }
}

fn canonicalise_bare_type_name(
    type_name: &mut String,
    dep_prefix: &str,
    own_colliding: &HashSet<String>,
) {
    if !type_name.contains('.') && own_colliding.contains(type_name.as_str()) {
        *type_name = format!("{dep_prefix}.{type_name}");
    }
}

fn canonicalise_constructor_name(
    name: &mut String,
    dep_prefix: &str,
    own_colliding: &HashSet<String>,
) {
    let head = name.split('.').next().unwrap_or(name);
    if own_colliding.contains(head) {
        let rest = &name[head.len()..];
        *name = format!("{dep_prefix}.{head}{rest}");
    }
}

fn canonicalise_attr_head_for_own_colliding(
    expr: &mut Expr,
    dep_prefix: &str,
    own_colliding: &HashSet<String>,
) {
    fn walk(e: &mut Expr, dep_prefix: &str, own_colliding: &HashSet<String>, depth: u32) {
        if depth > 32 {
            return;
        }
        match e {
            Expr::Attr(inner, _) => walk(&mut inner.node, dep_prefix, own_colliding, depth + 1),
            Expr::Ident(name) if !name.contains('.') && own_colliding.contains(name.as_str()) => {
                *name = format!("{dep_prefix}.{name}");
            }
            _ => {}
        }
    }
    walk(expr, dep_prefix, own_colliding, 0);
}

fn canonicalise_pattern(pat: &mut Pattern, dep_prefix: &str, own_colliding: &HashSet<String>) {
    if let Pattern::Constructor(name, _bindings) = pat {
        let head = name.split('.').next().unwrap_or(name);
        if own_colliding.contains(head) {
            let rest = &name[head.len()..];
            *name = format!("{dep_prefix}.{head}{rest}");
        }
    }
}

fn rewrite_dotted_module_ref(
    dotted: &str,
    prefixes: &HashSet<String>,
    colliding_bare_names: &HashSet<String>,
) -> Option<Expr> {
    let segments: Vec<&str> = dotted.split('.').collect();
    if segments.len() < 2 {
        return None;
    }

    let mut best: Option<usize> = None;
    for split in (1..segments.len()).rev() {
        let candidate = segments[..split].join(".");
        if prefixes.contains(&candidate) {
            best = Some(split);
            break;
        }
    }
    let split = best?;
    let prefix_dotted = segments[..split].join(".");

    Some(match segments.len() - split {
        0 => return None,
        1 => Expr::Ident(prefixed(&prefix_dotted, segments[split])),
        _ => {
            let mut type_name = segments[split..segments.len() - 1].join("_");
            // A colliding dep type keeps its canonical `Prefix.Name` spelling
            // post-flatten, so the qualified reference must not collapse to
            // the bare name — that name denotes another module's declaration.
            if colliding_bare_names.contains(&type_name) {
                type_name = format!("{prefix_dotted}.{type_name}");
            }
            let last = segments[segments.len() - 1].to_string();
            Expr::Attr(Box::new(Spanned::bare(Expr::Ident(type_name))), last)
        }
    })
}

fn rewrite_stmts(stmts: &mut [Stmt], ctx: &RewriteCtx<'_>) {
    for stmt in stmts.iter_mut() {
        match stmt {
            Stmt::Binding(_, type_ann, expr) => {
                if !ctx.dep_prefix.is_empty()
                    && let Some(ty) = type_ann.as_mut()
                {
                    *ty = canonicalise_own_colliding(ty, ctx.dep_prefix, ctx.own_colliding);
                }
                rewrite_expr(&mut expr.node, ctx);
            }
            Stmt::Expr(expr) => {
                rewrite_expr(&mut expr.node, ctx);
            }
        }
    }
}
