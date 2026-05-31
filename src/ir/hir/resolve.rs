//! `Expr → ResolvedExpr` conversion pass.
//!
//! Walks the post-typecheck AST and produces the resolved HIR shape
//! defined in [`crate::ir::hir`]. Identity is the only thing that
//! changes — the lowering is one-to-one in structure, evaluation
//! order, and effect classification.
//!
//! ## What the resolver does
//!
//! 1. Classifies every `Expr::FnCall(callee, args)` callee into a
//!    [`ResolvedCallee`] — user `FnId`, builtin namespace method,
//!    local lambda slot, or `Unresolved` passthrough.
//! 2. Resolves every `Expr::Constructor(name, …)` (and matching
//!    [`crate::ast::Pattern::Constructor`]) into a [`ResolvedCtor`]
//!    — user `CtorId` + owning `TypeId`, the four built-in
//!    Result/Option variants, or `Unresolved` passthrough.
//! 3. Resolves every `Expr::RecordCreate` / `Expr::RecordUpdate`
//!    `type_name` to a `TypeId` against the program's `SymbolTable`
//!    in the current module's resolver context (peer review round 6
//!    on #148 — owner-aware lookup).
//! 4. Resolves every `Expr::TailCall(data)` target name to a `FnId`.
//! 5. Lifts `Stmt::Binding(name, Option<String>, expr)` to
//!    [`ResolvedStmt::Binding`] with an `Option<Type>` annotation
//!    (parsed + canonicalised through the same owner-aware
//!    resolver context).
//!
//! ## What the resolver does NOT do
//!
//! - **No type-checking re-run**. The typechecker is the source of
//!   truth for semantic decisions; the resolver only translates
//!   names. If the typechecker passed, the resolver produces a
//!   well-formed `ResolvedTopLevel` even when individual references
//!   couldn't classify (those land in `Unresolved` passthroughs so
//!   the surrounding tree still walks).
//! - **No `Spanned::ty()` re-stamping**. Type stamps on the original
//!   `Spanned<Expr>` nodes are already canonical (Phase B); the
//!   resolver carries them across unchanged when relevant.
//! - **No backend wiring**. Output is `Vec<ResolvedTopLevel>` —
//!   producer only. Future PRs migrate each backend to consume it.
//!
//! ## Boundary with the typechecker
//!
//! Phase B made the typechecker's matcher operate on opaque IDs;
//! Phase E generalises the same move to the AST itself. The
//! resolver re-uses the symbol-table resolution rules the
//! typechecker already exposes (qualified-then-bare lookups,
//! built-in namespace recognition, builtin ctor distinction) so
//! the two layers stay in lock-step.

use std::sync::Arc;

use crate::ast::{Expr, FnBody, FnDef, MatchArm, Pattern, Spanned, Stmt, TopLevel};
use crate::ir::calls::{expr_to_dotted_name, is_builtin_namespace};
use crate::ir::hir::{
    BuiltinCtor, BuiltinIntrinsic, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedFnBody,
    ResolvedFnDef, ResolvedMatchArm, ResolvedPattern, ResolvedStmt, ResolvedStrPart,
    ResolvedTopLevel,
};
use crate::ir::identity::{FnId, FnKey, TypeId, TypeKey};
use crate::ir::symbol_table::SymbolTable;

/// Resolver state. Carries the symbol table the pass resolves
/// against and the module prefix the current item lives in. The
/// prefix matters for entry items that declare `module Main` — the
/// symbol table stores them under `FnKey::entry` / `TypeKey::entry`,
/// but the source-level references inside their bodies still use
/// the interior name, so the resolver needs both candidates to
/// match Phase B's canonical resolution rules.
pub struct ResolveCtx<'a> {
    pub symbols: &'a SymbolTable,
    pub current_module: Option<String>,
}

impl<'a> ResolveCtx<'a> {
    pub fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            current_module: None,
        }
    }

    /// Resolve a fn reference to a `FnId`. Tries (in order):
    ///   1. Qualified: `Module.fn` → `FnKey::in_module(Module, fn)`.
    ///   2. Current module: `fn` → `FnKey::in_module(current, fn)`.
    ///   3. Current-as-entry: when the qualified prefix matches the
    ///      current module, also probe `FnKey::entry(fn)` (entry
    ///      items live under entry scope in the symbol table even
    ///      when they declare `module X`).
    ///   4. Entry: `fn` → `FnKey::entry(fn)`.
    pub fn resolve_fn_id(&self, name: &str) -> Option<FnId> {
        if let Some((prefix, n)) = name.rsplit_once('.') {
            if let Some(id) = self.symbols.fn_id_of(&FnKey::in_module(prefix, n)) {
                return Some(id);
            }
            if self.current_module.as_deref() == Some(prefix)
                && let Some(id) = self.symbols.fn_id_of(&FnKey::entry(n))
            {
                return Some(id);
            }
        }
        if let Some(prefix) = self.current_module.as_deref()
            && let Some(id) = self.symbols.fn_id_of(&FnKey::in_module(prefix, name))
        {
            return Some(id);
        }
        self.symbols.fn_id_of(&FnKey::entry(name))
    }

    /// Type-side equivalent of [`Self::resolve_fn_id`].
    ///
    /// Resolution order (first match wins):
    /// 1. `prefix.name` matches `TypeKey::in_module(prefix, name)`.
    /// 2. `prefix.name` with `prefix == current_module` matches the
    ///    entry-scope `TypeKey::entry(name)` (Aver lets a
    ///    self-referencing module spell its own type with the
    ///    qualified form).
    /// 3. Bare `name` in `current_module` (`TypeKey::in_module(current, name)`).
    /// 4. Bare `name` in entry scope (`TypeKey::entry(name)`).
    /// 5. Bare `name` searched across every scope via
    ///    [`crate::ir::SymbolTable::type_id_by_bare_name`] — handles
    ///    the cross-module case where module `A` references type
    ///    `Val` declared in module `B` without qualification. Returns
    ///    `None` when ambiguous (caller must qualify).
    pub fn resolve_type_id(&self, name: &str) -> Option<TypeId> {
        if let Some((prefix, n)) = name.rsplit_once('.') {
            if let Some(id) = self.symbols.type_id_of(&TypeKey::in_module(prefix, n)) {
                return Some(id);
            }
            if self.current_module.as_deref() == Some(prefix)
                && let Some(id) = self.symbols.type_id_of(&TypeKey::entry(n))
            {
                return Some(id);
            }
        }
        if let Some(prefix) = self.current_module.as_deref()
            && let Some(id) = self.symbols.type_id_of(&TypeKey::in_module(prefix, name))
        {
            return Some(id);
        }
        if let Some(id) = self.symbols.type_id_of(&TypeKey::entry(name)) {
            return Some(id);
        }
        // Cross-module bare-name fallback. Phase-E ctor resolution
        // for `Val.ValOk(x)` written from a module that doesn't host
        // `Val` (e.g. `Domain.Eval.Core` referencing
        // `Domain.Value.Val`). `type_id_by_bare_name` returns `None`
        // on ambiguity, so two scopes that share a type name still
        // need a qualified reference.
        self.symbols.type_id_by_bare_name(name)
    }

    /// Resolve a `"Type.Variant"` constructor reference to a
    /// `(CtorId, TypeId, variant_name)` triple.
    pub fn resolve_user_ctor(
        &self,
        dotted: &str,
    ) -> Option<(crate::ir::identity::CtorId, TypeId, String)> {
        let (type_name, variant) = dotted.rsplit_once('.')?;
        let type_id = self.resolve_type_id(type_name)?;
        let ctor_id = self.symbols.ctor_id_of(type_id, variant)?;
        Some((ctor_id, type_id, variant.to_string()))
    }
}

/// Resolve a whole program. Walks every top-level item once; items
/// the resolver doesn't promote (verify blocks, decisions, type
/// defs) land in [`ResolvedTopLevel::Passthrough`] so backends can
/// iterate the same shape regardless.
pub fn resolve_program(symbols: &SymbolTable, items: &[TopLevel]) -> Vec<ResolvedTopLevel> {
    let module_name = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m.name.clone()),
        _ => None,
    });
    let mut ctx = ResolveCtx::new(symbols);
    ctx.current_module = module_name;
    items.iter().map(|i| resolve_top_level(&ctx, i)).collect()
}

/// Resolve a single `FnDef` against an existing [`ResolveCtx`]. Mirror
/// of [`resolve_stmt_external`] for callers that need to lift a free-
/// standing fn def into resolved HIR — used by `CodegenContext` to
/// populate `resolved_fn_defs` per-scope (entry + each dep) without
/// re-running the whole program-level resolver.
pub fn resolve_fn_def_external(ctx: &ResolveCtx<'_>, fd: &FnDef) -> Option<ResolvedFnDef> {
    resolve_fn_def(ctx, fd)
}

pub fn resolve_top_level(ctx: &ResolveCtx<'_>, item: &TopLevel) -> ResolvedTopLevel {
    match item {
        TopLevel::Module(m) => ResolvedTopLevel::Module(m.clone()),
        TopLevel::FnDef(fd) => match resolve_fn_def(ctx, fd) {
            Some(rfd) => ResolvedTopLevel::FnDef(rfd),
            // The fn's name didn't resolve through the symbol table.
            // That only happens when the typechecker also rejected
            // the program (or the program never went through
            // build_signatures, e.g. a REPL-style item); either way
            // we can't synthesise a `FnId`, so pass the original
            // AST node through and let downstream consumers
            // continue to handle it via the legacy `Expr` shape.
            None => ResolvedTopLevel::Passthrough(item.clone()),
        },
        // Verify / Decision / TypeDef / top-level Stmt items pass
        // through with their original AST representation. They're
        // not on the runtime hot path; future PRs may promote them
        // once the proof-export passes are ready.
        TopLevel::Verify(_) | TopLevel::Decision(_) | TopLevel::Stmt(_) | TopLevel::TypeDef(_) => {
            ResolvedTopLevel::Passthrough(item.clone())
        }
    }
}

fn resolve_fn_def(ctx: &ResolveCtx<'_>, fd: &FnDef) -> Option<ResolvedFnDef> {
    let fn_id = ctx.resolve_fn_id(&fd.name)?;
    let params: Vec<(String, crate::ast::Type)> = fd
        .params
        .iter()
        .map(|(name, ann)| {
            let ty = match crate::types::parse_type_str_strict(ann) {
                Ok(t) => canonicalise_type(ctx, t),
                Err(_) => crate::ast::Type::Invalid,
            };
            (name.clone(), ty)
        })
        .collect();
    let return_type = match crate::types::parse_type_str_strict(&fd.return_type) {
        Ok(t) => canonicalise_type(ctx, t),
        Err(_) => crate::ast::Type::Invalid,
    };
    let body = resolve_fn_body(ctx, &fd.body);
    Some(ResolvedFnDef {
        fn_id,
        name: fd.name.clone(),
        line: fd.line,
        params,
        return_type,
        effects: fd.effects.clone(),
        desc: fd.desc.clone(),
        body: Arc::new(body),
        resolution: fd.resolution.clone(),
    })
}

fn resolve_fn_body(ctx: &ResolveCtx<'_>, body: &FnBody) -> ResolvedFnBody {
    match body {
        FnBody::Block(stmts) => {
            ResolvedFnBody::Block(stmts.iter().map(|s| resolve_stmt(ctx, s)).collect())
        }
    }
}

/// Resolve a single `Stmt` against a [`ResolveCtx`]. Exposed for
/// callers that need to lift a free-standing statement into resolved
/// HIR — top-level statements bypass the per-`FnDef` resolution path
/// because they live in `TopLevel::Stmt` (which the program-level
/// resolver leaves as `Passthrough` to avoid double-lifting them
/// into a synthetic fn body).
pub fn resolve_stmt_external(ctx: &ResolveCtx<'_>, stmt: &Stmt) -> ResolvedStmt {
    resolve_stmt(ctx, stmt)
}

fn resolve_stmt(ctx: &ResolveCtx<'_>, stmt: &Stmt) -> ResolvedStmt {
    match stmt {
        Stmt::Binding(name, ann, expr) => {
            let ty_ann = ann
                .as_deref()
                .map(|src| match crate::types::parse_type_str_strict(src) {
                    Ok(t) => canonicalise_type(ctx, t),
                    Err(_) => crate::ast::Type::Invalid,
                });
            ResolvedStmt::Binding {
                name: name.clone(),
                ty_ann,
                value: resolve_spanned(ctx, expr),
            }
        }
        Stmt::Expr(expr) => ResolvedStmt::Expr(resolve_spanned(ctx, expr)),
    }
}

fn resolve_spanned(ctx: &ResolveCtx<'_>, expr: &Spanned<Expr>) -> Spanned<ResolvedExpr> {
    let resolved = resolve_expr(ctx, expr);
    let out = Spanned::new(resolved, expr.line);
    if let Some(t) = expr.ty.get() {
        let _ = out.ty.set(t.clone());
    }
    out
}

fn resolve_expr(ctx: &ResolveCtx<'_>, expr: &Spanned<Expr>) -> ResolvedExpr {
    match &expr.node {
        Expr::Literal(l) => ResolvedExpr::Literal(l.clone()),
        Expr::Ident(name) => ResolvedExpr::Ident(name.clone()),
        Expr::Resolved {
            slot,
            name,
            last_use,
        } => ResolvedExpr::Resolved {
            slot: *slot,
            name: name.clone(),
            last_use: *last_use,
        },
        Expr::Attr(obj, field) => {
            // Nullary constructor in value position — `Option.None`,
            // `Color.Black`, etc. These only got recognized in call
            // position (`Expr::FnCall`); in value position they arrived
            // here as a bare `Attr(Ident(Type), Variant)` and stalled MIR
            // lowering with `UnresolvedIdent`. A non-nullary ctor can't
            // appear un-applied as a value (Aver has no first-class
            // partial constructors), so an `Attr` whose `Type.Member`
            // resolves to a ctor is necessarily a nullary reference —
            // lower it to the zero-arg `Ctor` shape both walkers already
            // handle (LOAD_CONST NONE / VARIANT_NEW with no fields).
            if let Expr::Ident(ns) = &obj.node {
                let qualified = format!("{ns}.{field}");
                let ctor = classify_ctor(ctx, &qualified);
                let nullary = matches!(
                    ctor,
                    ResolvedCtor::Builtin(BuiltinCtor::OptionNone) | ResolvedCtor::User { .. }
                );
                if nullary {
                    return ResolvedExpr::Ctor(ctor, vec![]);
                }
            }
            ResolvedExpr::Attr(Box::new(resolve_spanned(ctx, obj)), field.clone())
        }
        Expr::FnCall(callee, args) => {
            let resolved_args: Vec<Spanned<ResolvedExpr>> =
                args.iter().map(|a| resolve_spanned(ctx, a)).collect();
            // Parser shape note: `Shape.Circle(1.0)` and
            // `Result.Ok(42)` both parse as `FnCall(Attr(...), …)`
            // — only the bare `Option.None` variant goes through
            // `Expr::Constructor`. Recognise the ctor shape here so
            // it lands as `ResolvedExpr::Ctor`, not `Call(Builtin,
            // ...)` (which is for namespace methods like `Int.add`).
            if let Some(dotted) = expr_to_dotted_name(&callee.node) {
                if let Some(builtin) = parse_builtin_ctor(&dotted) {
                    return ResolvedExpr::Ctor(ResolvedCtor::Builtin(builtin), resolved_args);
                }
                if let Some((ctor_id, type_id, variant_name)) = ctx.resolve_user_ctor(&dotted) {
                    return ResolvedExpr::Ctor(
                        ResolvedCtor::User {
                            ctor_id,
                            type_id,
                            name: variant_name,
                        },
                        resolved_args,
                    );
                }
            }
            let resolved_callee = classify_callee(ctx, callee);
            ResolvedExpr::Call(resolved_callee, resolved_args)
        }
        Expr::BinOp(op, l, r) => ResolvedExpr::BinOp(
            *op,
            Box::new(resolve_spanned(ctx, l)),
            Box::new(resolve_spanned(ctx, r)),
        ),
        Expr::Neg(inner) => ResolvedExpr::Neg(Box::new(resolve_spanned(ctx, inner))),
        Expr::Match { subject, arms } => ResolvedExpr::Match {
            subject: Box::new(resolve_spanned(ctx, subject)),
            arms: arms.iter().map(|a| resolve_match_arm(ctx, a)).collect(),
        },
        Expr::Constructor(name, arg) => {
            let ctor = classify_ctor(ctx, name);
            let args = match arg {
                Some(a) => vec![resolve_spanned(ctx, a)],
                None => vec![],
            };
            ResolvedExpr::Ctor(ctor, args)
        }
        Expr::ErrorProp(inner) => ResolvedExpr::ErrorProp(Box::new(resolve_spanned(ctx, inner))),
        Expr::InterpolatedStr(parts) => ResolvedExpr::InterpolatedStr(
            parts
                .iter()
                .map(|p| match p {
                    crate::ast::StrPart::Literal(s) => ResolvedStrPart::Literal(s.clone()),
                    crate::ast::StrPart::Parsed(inner) => {
                        ResolvedStrPart::Parsed(Box::new(resolve_spanned(ctx, inner)))
                    }
                })
                .collect(),
        ),
        Expr::List(items) => {
            ResolvedExpr::List(items.iter().map(|i| resolve_spanned(ctx, i)).collect())
        }
        Expr::Tuple(items) => {
            ResolvedExpr::Tuple(items.iter().map(|i| resolve_spanned(ctx, i)).collect())
        }
        Expr::MapLiteral(pairs) => ResolvedExpr::MapLiteral(
            pairs
                .iter()
                .map(|(k, v)| (resolve_spanned(ctx, k), resolve_spanned(ctx, v)))
                .collect(),
        ),
        Expr::RecordCreate { type_name, fields } => ResolvedExpr::RecordCreate {
            type_id: ctx.resolve_type_id(type_name),
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(n, e)| (n.clone(), resolve_spanned(ctx, e)))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => ResolvedExpr::RecordUpdate {
            type_id: ctx.resolve_type_id(type_name),
            type_name: type_name.clone(),
            base: Box::new(resolve_spanned(ctx, base)),
            updates: updates
                .iter()
                .map(|(n, e)| (n.clone(), resolve_spanned(ctx, e)))
                .collect(),
        },
        Expr::TailCall(data) => {
            let target = ctx.resolve_fn_id(&data.target);
            let args = data.args.iter().map(|a| resolve_spanned(ctx, a)).collect();
            match target {
                Some(fn_id) => ResolvedExpr::TailCall {
                    target: fn_id,
                    args,
                },
                // TCO target couldn't resolve to a `FnId`. This is
                // either pre-typecheck recovery state or a bug in
                // the TCO pass; fall back to a `Call` with an
                // `Unresolved` callee so the surrounding tree
                // walks cleanly and the diagnostic stays
                // attributable.
                None => ResolvedExpr::Call(
                    ResolvedCallee::Unresolved {
                        callee: Box::new(Spanned::new(
                            ResolvedExpr::Ident(data.target.clone()),
                            expr.line,
                        )),
                    },
                    args,
                ),
            }
        }
        Expr::IndependentProduct(items, unwrap) => ResolvedExpr::IndependentProduct(
            items.iter().map(|i| resolve_spanned(ctx, i)).collect(),
            *unwrap,
        ),
    }
}

fn resolve_match_arm(ctx: &ResolveCtx<'_>, arm: &MatchArm) -> ResolvedMatchArm {
    let binding_slots = std::sync::OnceLock::new();
    if let Some(slots) = arm.binding_slots.get() {
        let _ = binding_slots.set(slots.clone());
    }
    ResolvedMatchArm {
        pattern: resolve_pattern(ctx, &arm.pattern),
        body: Box::new(resolve_spanned(ctx, &arm.body)),
        binding_slots,
    }
}

fn resolve_pattern(ctx: &ResolveCtx<'_>, pat: &Pattern) -> ResolvedPattern {
    match pat {
        Pattern::Wildcard => ResolvedPattern::Wildcard,
        Pattern::Literal(l) => ResolvedPattern::Literal(l.clone()),
        Pattern::Ident(name) => ResolvedPattern::Ident(name.clone()),
        Pattern::EmptyList => ResolvedPattern::EmptyList,
        Pattern::Cons(head, tail) => ResolvedPattern::Cons(head.clone(), tail.clone()),
        Pattern::Tuple(items) => {
            ResolvedPattern::Tuple(items.iter().map(|p| resolve_pattern(ctx, p)).collect())
        }
        Pattern::Constructor(name, bindings) => {
            ResolvedPattern::Ctor(classify_ctor(ctx, name), bindings.clone())
        }
    }
}

/// Classify a call's callee expression. Branches:
/// `Fn(FnId)` for user fns the symbol table knows about;
/// `Builtin(name)` for namespace methods like `Int.add` /
/// `Console.print`; `Intrinsic(_)` for the synthesised
/// `__buf_*` / `__to_str` shapes emitted by `interp_lower` /
/// `buffer_build`; `LocalSlot` for first-class fn values; the
/// `Unresolved` passthrough for anything else (typechecker already
/// reported it).
fn classify_callee(ctx: &ResolveCtx<'_>, callee: &Spanned<Expr>) -> ResolvedCallee {
    match &callee.node {
        Expr::Resolved {
            slot,
            name,
            last_use,
        } => ResolvedCallee::LocalSlot {
            slot: *slot,
            name: name.clone(),
            last_use: *last_use,
        },
        Expr::Ident(name) => {
            if let Some(intrinsic) = BuiltinIntrinsic::from_name(name) {
                return ResolvedCallee::Intrinsic(intrinsic);
            }
            // Some synthesised / pre-parsed shapes pack a fully
            // qualified namespace method into a single `Ident`
            // (`Ident("List.len")` rather than the parser's regular
            // `Attr(Ident("List"), "len")`). Recognise the builtin
            // namespace prefix so the resolver classifies them as
            // `Builtin` rather than falling to `Unresolved` — keeps
            // the source-shape classifier menu (`is_builtin_namespace`)
            // honoured for both AST shapes.
            if let Some((head, _)) = name.split_once('.')
                && is_builtin_namespace(head)
            {
                return ResolvedCallee::Builtin(name.clone());
            }
            match ctx.resolve_fn_id(name) {
                Some(id) => ResolvedCallee::Fn(id),
                None => ResolvedCallee::Unresolved {
                    callee: Box::new(resolve_spanned(ctx, callee)),
                },
            }
        }
        Expr::Attr(_, _) => {
            let Some(dotted) = expr_to_dotted_name(&callee.node) else {
                return ResolvedCallee::Unresolved {
                    callee: Box::new(resolve_spanned(ctx, callee)),
                };
            };
            // Try user-fn first — module-scoped calls like
            // `Pricing.Discount.percent` route here.
            if let Some(id) = ctx.resolve_fn_id(&dotted) {
                return ResolvedCallee::Fn(id);
            }
            // Builtin namespace method (`Int.add`, `Console.print`).
            // Use the existing classifier knowledge.
            if let Some((head, _rest)) = dotted.split_once('.')
                && is_builtin_namespace(head)
            {
                return ResolvedCallee::Builtin(dotted);
            }
            ResolvedCallee::Unresolved {
                callee: Box::new(resolve_spanned(ctx, callee)),
            }
        }
        _ => ResolvedCallee::Unresolved {
            callee: Box::new(resolve_spanned(ctx, callee)),
        },
    }
}

/// Classify a `"Type.Variant"` (or built-in shortcut) constructor
/// reference. Builtins (`Result.Ok`, `Result.Err`, `Option.Some`,
/// `Option.None`) get the [`BuiltinCtor`] kind; user variants get
/// `(CtorId, owning TypeId, variant name)`; anything else is an
/// `Unresolved` passthrough.
fn classify_ctor(ctx: &ResolveCtx<'_>, name: &str) -> ResolvedCtor {
    if let Some(builtin) = parse_builtin_ctor(name) {
        return ResolvedCtor::Builtin(builtin);
    }
    if let Some((ctor_id, type_id, variant_name)) = ctx.resolve_user_ctor(name) {
        return ResolvedCtor::User {
            ctor_id,
            type_id,
            name: variant_name,
        };
    }
    ResolvedCtor::Unresolved {
        name: name.to_string(),
    }
}

fn parse_builtin_ctor(name: &str) -> Option<BuiltinCtor> {
    match name {
        "Result.Ok" => Some(BuiltinCtor::ResultOk),
        "Result.Err" => Some(BuiltinCtor::ResultErr),
        "Option.Some" => Some(BuiltinCtor::OptionSome),
        // Both spellings — parser produces `Option.None` for the
        // qualified form and just `None` when the expression
        // appeared as a constructor literal without the qualifier.
        "Option.None" | "None" => Some(BuiltinCtor::OptionNone),
        _ => None,
    }
}

/// Canonicalise a parsed `Type` against the resolver's symbol table.
/// Sets `Type::Named { id: Some(_) }` when the name resolves; leaves
/// `id: None` otherwise (builtin records, unresolved typo, …).
/// Recurses through compound shapes so `Result<A.Shape, String>`
/// gets the inner `A.Shape` resolved too.
fn canonicalise_type(ctx: &ResolveCtx<'_>, ty: crate::ast::Type) -> crate::ast::Type {
    use crate::ast::Type;
    match ty {
        // Peer review round 7: `Some(id)` is sacred. The typechecker
        // already stamped real identities; never overwrite.
        Type::Named {
            id: Some(existing),
            name,
        } => Type::Named {
            id: Some(existing),
            name,
        },
        Type::Named { id: None, name } => match ctx.resolve_type_id(&name) {
            Some(id) => Type::Named { id: Some(id), name },
            None => Type::Named { id: None, name },
        },
        Type::List(inner) => Type::List(Box::new(canonicalise_type(ctx, *inner))),
        Type::Vector(inner) => Type::Vector(Box::new(canonicalise_type(ctx, *inner))),
        Type::Option(inner) => Type::Option(Box::new(canonicalise_type(ctx, *inner))),
        Type::Result(ok, err) => Type::Result(
            Box::new(canonicalise_type(ctx, *ok)),
            Box::new(canonicalise_type(ctx, *err)),
        ),
        Type::Map(k, v) => Type::Map(
            Box::new(canonicalise_type(ctx, *k)),
            Box::new(canonicalise_type(ctx, *v)),
        ),
        Type::Tuple(items) => Type::Tuple(
            items
                .into_iter()
                .map(|t| canonicalise_type(ctx, t))
                .collect(),
        ),
        Type::Fn(params, ret, effects) => Type::Fn(
            params
                .into_iter()
                .map(|t| canonicalise_type(ctx, t))
                .collect(),
            Box::new(canonicalise_type(ctx, *ret)),
            effects,
        ),
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Literal};
    use crate::source::parse_source;
    use crate::tco;

    fn build(src: &str) -> (SymbolTable, Vec<TopLevel>) {
        let mut items = parse_source(src).expect("parse failed");
        tco::transform_program(&mut items);
        let symbols = SymbolTable::build(&items, &[]);
        (symbols, items)
    }

    fn first_fn(resolved: &[ResolvedTopLevel]) -> &ResolvedFnDef {
        resolved
            .iter()
            .find_map(|t| match t {
                ResolvedTopLevel::FnDef(f) => Some(f),
                _ => None,
            })
            .expect("expected at least one resolved fn def")
    }

    #[test]
    fn resolves_user_fn_call_to_fn_id() {
        let (symbols, items) = build(
            r#"
fn main() -> Int
    helper(1)

fn helper(n: Int) -> Int
    n + 1
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let main_fn = resolved
            .iter()
            .find_map(|t| match t {
                ResolvedTopLevel::FnDef(f) if f.name == "main" => Some(f),
                _ => None,
            })
            .expect("main");
        let stmts = main_fn.body.stmts();
        let ResolvedStmt::Expr(call) = &stmts[0] else {
            panic!("expected tail Expr stmt, got {:?}", stmts[0]);
        };
        match &call.node {
            ResolvedExpr::Call(ResolvedCallee::Fn(_), args) => {
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected Call(Fn, _), got {:?}", other),
        }
    }

    #[test]
    fn resolves_builtin_namespace_method_call() {
        let (symbols, items) = build(
            r#"
fn add() -> Int
    Int.abs(-3)
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let f = first_fn(&resolved);
        let ResolvedStmt::Expr(call) = &f.body.stmts()[0] else {
            panic!("expected tail Expr");
        };
        match &call.node {
            ResolvedExpr::Call(ResolvedCallee::Builtin(name), _) => {
                assert_eq!(name, "Int.abs");
            }
            other => panic!("expected Call(Builtin, _), got {:?}", other),
        }
    }

    #[test]
    fn resolves_user_ctor_to_ctor_id_and_owning_type() {
        let (symbols, items) = build(
            r#"
type Shape
    Circle(Float)
    Square(Float)

fn make() -> Shape
    Shape.Circle(1.0)
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let f = first_fn(&resolved);
        let ResolvedStmt::Expr(call) = &f.body.stmts()[0] else {
            panic!("expected tail Expr");
        };
        match &call.node {
            ResolvedExpr::Ctor(ResolvedCtor::User { type_id, name, .. }, args) => {
                assert_eq!(name, "Circle");
                assert!(*type_id != crate::ir::identity::TypeId(u32::MAX));
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected Ctor(User, _), got {:?}", other),
        }
    }

    #[test]
    fn resolves_builtin_ctor_result_ok() {
        let (symbols, items) = build(
            r#"
fn make() -> Result<Int, String>
    Result.Ok(42)
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let f = first_fn(&resolved);
        let ResolvedStmt::Expr(expr) = &f.body.stmts()[0] else {
            panic!("expected tail Expr");
        };
        match &expr.node {
            ResolvedExpr::Ctor(ResolvedCtor::Builtin(BuiltinCtor::ResultOk), args) => {
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected Builtin(ResultOk), got {:?}", other),
        }
    }

    #[test]
    fn resolves_record_create_to_type_id() {
        let (symbols, items) = build(
            r#"
record Point
    x: Int
    y: Int

fn origin() -> Point
    Point(x = 0, y = 0)
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let f = first_fn(&resolved);
        let ResolvedStmt::Expr(expr) = &f.body.stmts()[0] else {
            panic!("expected tail Expr");
        };
        match &expr.node {
            ResolvedExpr::RecordCreate {
                type_id, type_name, ..
            } => {
                assert!(type_id.is_some(), "Point should resolve to a TypeId");
                assert_eq!(type_name, "Point");
            }
            other => panic!("expected RecordCreate, got {:?}", other),
        }
    }

    #[test]
    fn resolves_tail_call_target_to_fn_id() {
        let (symbols, items) = build(
            r#"
fn count(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> count(n - 1, acc + 1)
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let f = first_fn(&resolved);
        // Tail-call should be in the recursive arm. Walk the match.
        let ResolvedStmt::Expr(top) = &f.body.stmts()[0] else {
            panic!("expected tail Expr");
        };
        let ResolvedExpr::Match { arms, .. } = &top.node else {
            panic!("expected match");
        };
        // Recursive arm is the second one.
        let recursive_body = &arms[1].body.node;
        match recursive_body {
            ResolvedExpr::TailCall { target, args } => {
                assert_eq!(args.len(), 2);
                // FnId resolved against the symbol table — value
                // depends on build order but it must be present.
                let _ = target;
            }
            other => panic!("expected TailCall, got {:?}", other),
        }
    }

    #[test]
    fn resolves_binding_annotation_to_canonicalised_type() {
        let (symbols, items) = build(
            r#"
fn pair() -> Int
    x: Int = 5
    x
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let f = first_fn(&resolved);
        let stmts = f.body.stmts();
        let ResolvedStmt::Binding { name, ty_ann, .. } = &stmts[0] else {
            panic!("expected Binding stmt, got {:?}", stmts[0]);
        };
        assert_eq!(name, "x");
        assert_eq!(ty_ann.as_ref(), Some(&crate::ast::Type::Int));
    }

    #[test]
    fn binop_passes_through_structurally() {
        let (symbols, items) = build(
            r#"
fn add() -> Int
    1 + 2
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        let f = first_fn(&resolved);
        let ResolvedStmt::Expr(expr) = &f.body.stmts()[0] else {
            panic!("tail expected");
        };
        match &expr.node {
            ResolvedExpr::BinOp(BinOp::Add, l, r) => {
                assert!(matches!(l.node, ResolvedExpr::Literal(Literal::Int(1))));
                assert!(matches!(r.node, ResolvedExpr::Literal(Literal::Int(2))));
            }
            other => panic!("expected BinOp(Add, _, _), got {:?}", other),
        }
    }

    #[test]
    fn passthrough_for_verify_decision_typedef() {
        let (symbols, items) = build(
            r#"
type Tag
    On
    Off

verify alwaysTrue
    1 => 1

fn alwaysTrue() -> Int
    1
"#,
        );
        let resolved = resolve_program(&symbols, &items);
        // TypeDef + Verify pass through; FnDef promotes.
        let mut saw_passthrough = false;
        let mut saw_fn = false;
        for item in &resolved {
            match item {
                ResolvedTopLevel::Passthrough(_) => saw_passthrough = true,
                ResolvedTopLevel::FnDef(_) => saw_fn = true,
                _ => {}
            }
        }
        assert!(saw_passthrough, "expected at least one passthrough item");
        assert!(saw_fn, "expected the FnDef to be promoted");
    }

    #[test]
    fn ty_stamps_preserved_on_resolved_spans() {
        // After typechecker has stamped a `Spanned::ty()`, the
        // resolved span must carry the same stamp. Phase B-era
        // backends that read `expr.ty()` on the resolved AST need
        // this invariant.
        use crate::ast::{Expr as AstExpr, Spanned as AstSpanned};
        let lit = AstSpanned::new(AstExpr::Literal(Literal::Int(7)), 1);
        let _ = lit.ty.set(crate::ast::Type::Int);
        // Build a one-item dummy symbol table — irrelevant for
        // literal stamp propagation, just need a context.
        let symbols = SymbolTable::default();
        let ctx = ResolveCtx::new(&symbols);
        let resolved = resolve_spanned(&ctx, &lit);
        assert_eq!(resolved.ty.get(), Some(&crate::ast::Type::Int));
    }
}
