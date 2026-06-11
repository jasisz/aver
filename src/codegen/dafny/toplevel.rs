/// Aver top-level items → Dafny declarations.
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::parse_type_annotation;
use crate::types::Type;

use super::expr::{aver_name_to_dafny, emit_expr_legacy};

/// Emit a Dafny type from an Aver type annotation string.
/// Ghost-predicate names emitted by `oracle_subtypes::dafny_subtype_predicates`
/// for classified Generative-shape effects. Keep in sync with that module.
fn bounded_oracle_predicate_for(method: &str) -> Option<&'static str> {
    match method {
        "Random.int" => Some("IsRandomIntInBounds"),
        "Random.float" => Some("IsRandomFloatInUnit"),
        "Time.unixMs" => Some("IsTimeUnixMsNonneg"),
        _ => None,
    }
}

pub fn emit_type(type_str: &str) -> String {
    type_to_dafny(&parse_type_annotation(type_str))
}

/// Render a typed `Type` directly to its Dafny representation —
/// skips the `parse_type_annotation(string)` round-trip.
///
/// Epic #180 Phase 5 — feed typed types from `ResolvedFnDef`
/// (params + return_type) into the Dafny renderer instead of
/// re-parsing the AST annotation strings the typechecker already
/// canonicalised. `emit_type(&str)` stays for callers whose
/// source is a raw string (e.g. given declarations referring to
/// effect type names).
pub fn emit_type_from(ty: &Type) -> String {
    type_to_dafny(ty)
}

/// Resolve a `&FnDef` to its canonical `ResolvedFnDef` for emit.
///
/// Tries the pointer-eq → `FnId` → resolved-program path first
/// (canonical for source-declared fns). If the symbol-table key
/// matches a DIFFERENT shape (effect-lifted synthetics share the
/// bare name with the source fn but carry extra BranchPath /
/// oracle params), the param-count gate trips and we fall back
/// to `ctx.resolve_fn_def`'s synthetic-lift path which derives
/// the typed surface from the given `fd` directly.
///
/// Same fallback pattern Rust (PR D, #185) and Lean (Phase 4,
/// #186) established, plus the synthetic-shape guard the Dafny
/// effect-lifting path needs.
fn resolved_view_for_emit<'a>(
    fd: &'a FnDef,
    ctx: &'a CodegenContext,
) -> std::borrow::Cow<'a, crate::ir::hir::ResolvedFnDef> {
    // Canonical path: pointer-eq scope → `FnId` → resolved view.
    // The param-count guard rejects a same-bare-name pre-lift twin
    // for effect-lifted synthetic fns (which carry extra
    // BranchPath / oracle params not present in the source fd).
    let canonical = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id))
        .filter(|rfd| rfd.params.len() == fd.params.len());
    if let Some(rfd) = canonical {
        return std::borrow::Cow::Borrowed(rfd);
    }
    // Synthetic-shape fn — lift from `fd` directly through the
    // resolver context. `ctx.resolve_fn_def` would re-hit the same
    // symbol-table cache and return the pre-lift twin again, so
    // bypass it and call the external lift path with the actual
    // post-lift `fd`.
    let module_name = ctx.items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m.name.clone()),
        _ => None,
    });
    let mut rctx = crate::ir::hir::ResolveCtx::new(&ctx.symbol_table);
    rctx.current_module = module_name;
    if let Some(lifted) = crate::ir::hir::resolve_fn_def_external(&rctx, fd) {
        return std::borrow::Cow::Owned(lifted);
    }
    // Last resort: `ctx.resolve_fn_def` carries its own
    // hand-built fallback for fds the resolver can't lift at all
    // (parse errors, unregistered names). Defer to it.
    ctx.resolve_fn_def(fd, None)
}

/// Convert a fully-resolved Aver `Type` to a Dafny type string.
/// Used by Oracle v1 to render oracle-signature types for effectful
/// law lemmas where the given's declared "type" is an effect reference
/// rather than an Aver type. The shared helper keeps this rendering in
/// one place so it can't drift from `type_to_dafny`.
pub fn type_ref_to_dafny(ty: &Type) -> String {
    type_to_dafny(ty)
}

/// Convert an Aver `Type` to a Dafny type string.
fn type_to_dafny(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Float => "real".to_string(),
        Type::Str => "string".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "()".to_string(),
        Type::List(inner) => format!("seq<{}>", type_to_dafny(inner)),
        Type::Vector(inner) => format!("seq<{}>", type_to_dafny(inner)),
        Type::Map(k, v) if crate::codegen::common::is_set_type(ty) => {
            format!("set<{}>", type_to_dafny(k))
        }
        Type::Map(k, v) => format!("map<{}, {}>", type_to_dafny(k), type_to_dafny(v)),
        Type::Result(ok, err) => format!("Result<{}, {}>", type_to_dafny(ok), type_to_dafny(err)),
        Type::Option(inner) => format!("Option<{}>", type_to_dafny(inner)),
        Type::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(type_to_dafny).collect();
            format!("({})", parts.join(", "))
        }
        Type::Fn(params, ret, _) => {
            // Dafny arrow types: `A -> B` is single-arg; multi-arg
            // requires tuple form `(A, B, C) -> D`. Curry-style
            // `A -> B -> C` would parse as `A -> (B -> C)` and break
            // at the call site (wrong number of arguments).
            let parts: Vec<String> = params.iter().map(type_to_dafny).collect();
            let ret_ty = type_to_dafny(ret);
            if parts.len() == 1 {
                format!("{} -> {}", parts[0], ret_ty)
            } else {
                format!("({}) -> {}", parts.join(", "), ret_ty)
            }
        }
        // Built-in records with dotted names (`Terminal.Size`,
        // `Tcp.Connection`) flatten to underscore form because the
        // prelude declares them as `Terminal_Size` / `Tcp_Connection`.
        // User-defined types: bare names rely on `import opened` of
        // the dependent module; already-qualified user types
        // (`Level.Room`) need the module-segment prefixed with `Aver_`
        // so the qualifier matches the renamed Dafny module.
        //
        // display-only: rendering the Dafny type identifier string.
        // `name` IS the right surface here. Identity-sensitive
        // routing already happens upstream via
        // `backend_named_type_key`; this arm only emits text.
        Type::Named { name, .. } => {
            if crate::codegen::builtin_records::find(name).is_some() {
                name.replace('.', "_")
            } else if let Some(dot) = name.rfind('.') {
                let module_part = &name[..dot];
                let local = &name[dot + 1..];
                format!("Aver_{}.{}", module_part.replace('.', "_"), local)
            } else {
                name.to_string()
            }
        }
        Type::Var(_) | Type::Invalid => "/* unknown type */".to_string(),
    }
}

// Refinement witness picking + predicate evaluation moved to
// `codegen::proof_lower` — Dafny now reads `decl.witness` off
// `ctx.proof_ir.refined_types` instead of re-running the walk per
// emit. The `literal_int_value` helper stays — it's also used by
// bounded-∀ universal-lemma emission elsewhere in this file.
fn literal_int_value(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => Some(n.to_string()),
        Expr::Neg(inner) => {
            let inner_str = literal_int_value(inner)?;
            Some(format!("-{inner_str}"))
        }
        _ => None,
    }
}

/// Emit a Dafny datatype/record from a TypeDef.
///
/// Refinement-via-opaque records with an `Int` carrier emit as a
/// subset type (`type X = n: int | P n witness W`) so the invariant
/// rides in the type and universal laws drop their `requires`
/// clause. Other carriers (Float / String / multi-field) keep the
/// plain `datatype` shape — `real` is Z3-unfriendly, strings are
/// poorly automated, and multi-field needs a `predicate` over the
/// product which the smart-constructor pattern doesn't supply.
pub fn emit_type_def(td: &TypeDef, ctx: &CodegenContext) -> Option<String> {
    emit_type_def_in_scope(td, ctx, None)
}

/// Module-scoped emit: `scope` carries the prefix of the module
/// whose typedefs we're rendering (or `None` for entry items).
/// Drives [`find_refined_type_scoped`] so a refined record with a
/// bare name resolves to the current module's slot.
pub fn emit_type_def_in_scope(
    td: &TypeDef,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> Option<String> {
    match td {
        TypeDef::Sum { name, variants, .. } => {
            let variant_strs: Vec<String> = variants
                .iter()
                .map(|v| {
                    if v.fields.is_empty() {
                        v.name.clone()
                    } else {
                        // Use variant-prefixed field names to avoid Dafny
                        // shared destructor conflicts across variants.
                        let prefix = crate::codegen::common::to_lower_first(&v.name);
                        let fields: Vec<String> = v
                            .fields
                            .iter()
                            .enumerate()
                            .map(|(i, f)| format!("{}_{}: {}", prefix, i, emit_type(f)))
                            .collect();
                        format!("{}({})", v.name, fields.join(", "))
                    }
                })
                .collect();
            Some(format!(
                "datatype {} = {}\n",
                name,
                variant_strs.join(" | ")
            ))
        }
        TypeDef::Product { name, fields, .. } => {
            if let Some(decl) = crate::codegen::common::find_refined_type_scoped(ctx, name, scope)
                && decl.carrier_type == "Int"
            {
                let predicate = super::expr::emit_expr(&decl.invariant.expr, ctx);
                let bind = aver_name_to_dafny(&decl.predicate_param);
                let witness = decl.witness.clone().unwrap_or_else(|| "0".to_string());
                return Some(format!(
                    "type {name} = {bind}: int | {predicate} witness {witness}\n"
                ));
            }
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(fname, ftype)| {
                    format!("{}: {}", aver_name_to_dafny(fname), emit_type(ftype))
                })
                .collect();
            Some(format!(
                "datatype {} = {}({})\n",
                name,
                name,
                field_strs.join(", ")
            ))
        }
    }
}

/// Emit a recursive fn whose shape is outside the proof subset
/// (mutual recursion with no termination measure the classifier
/// recognises, non-structural nested recursion, etc.) as a Dafny
/// axiom — a `function {:axiom}` declaration with a signature and no
/// body. Dafny treats it as an opaque total function: callers can
/// reference it, but the verifier won't unfold it, so soundness-
/// sensitive downstream reasoning about its value becomes user-
/// supplied lemmas. Mirrors Lean's `partial def` fallback.
pub fn emit_fn_def_axiom(fd: &FnDef, ctx: &CodegenContext) -> String {
    let name = aver_name_to_dafny(&fd.name);
    let rfd_holder = resolved_view_for_emit(fd, ctx);
    let rfd: &crate::ir::hir::ResolvedFnDef = rfd_holder.as_ref();
    let params: Vec<String> = rfd
        .params
        .iter()
        .map(|(pname, ptype)| format!("{}: {}", aver_name_to_dafny(pname), emit_type_from(ptype)))
        .collect();
    let ret_type = emit_type_from(&rfd.return_type);

    let mut lines = Vec::new();
    if let Some(desc) = &fd.desc {
        lines.push(format!("// {}", desc));
    }
    lines.push(
        "// Axiom: recursion pattern outside Dafny proof subset (emitted opaque)".to_string(),
    );
    lines.push(format!(
        "function {{:axiom}} {}({}): {}\n",
        name,
        params.join(", "),
        ret_type,
    ));
    lines.join("\n")
}

/// Emit a Dafny function from a FnDef.
pub fn emit_fn_def(fd: &FnDef, ctx: &CodegenContext) -> String {
    let name = aver_name_to_dafny(&fd.name);

    let rfd_holder = resolved_view_for_emit(fd, ctx);
    let rfd: &crate::ir::hir::ResolvedFnDef = rfd_holder.as_ref();

    let params: Vec<String> = rfd
        .params
        .iter()
        .map(|(pname, ptype)| format!("{}: {}", aver_name_to_dafny(pname), emit_type_from(ptype)))
        .collect();

    let ret_type = emit_type_from(&rfd.return_type);

    let lowered = lower_pure_question_bang_for_emit(fd);
    let body_ast = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    let body = emit_fn_body(body_ast, ctx);

    let needs_decreases = body_has_recursive_call(body_ast, &fd.name);

    let mut lines = Vec::new();

    if let Some(desc) = &fd.desc {
        lines.push(format!("// {}", desc));
    }

    lines.push(format!(
        "function {}({}): {}",
        name,
        params.join(", "),
        ret_type
    ));

    if needs_decreases && let Some(info) = infer_decreases(fd) {
        for req in &info.requires {
            lines.push(format!("  requires {}", req));
        }
        lines.push(format!("  decreases {}", info.expr));
    }

    lines.push("{".to_string());
    lines.push(format!("  {}", body));
    lines.push("}\n".to_string());

    lines.join("\n")
}

fn lower_pure_question_bang_for_emit(fd: &FnDef) -> Option<FnDef> {
    crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
        .ok()
        .flatten()
}

/// Emit the body of a function. Visible to sibling modules in the
/// Dafny backend — the fuel emitter needs it to render the rewritten
/// body inside a mutual SCC helper.
pub(super) fn emit_fn_body(body: &FnBody, ctx: &CodegenContext) -> String {
    match body {
        FnBody::Block(stmts) => emit_block_as_expr(stmts, ctx),
    }
}

/// Convert a block of statements into a Dafny expression.
fn emit_block_as_expr(stmts: &[Stmt], ctx: &CodegenContext) -> String {
    if stmts.is_empty() {
        return "()".to_string();
    }

    // If single expression, return it directly
    if stmts.len() == 1
        && let Stmt::Expr(expr) = &stmts[0]
    {
        return emit_expr_legacy(expr, ctx, None);
    }

    // For blocks with bindings, collect them and emit the last expression
    let mut parts = Vec::new();
    let mut final_expr = None;

    for (i, stmt) in stmts.iter().enumerate() {
        match stmt {
            Stmt::Binding(name, type_ann, expr) => {
                let mut val = emit_expr_legacy(expr, ctx, None);
                // Map<T, Unit> binding initialized with Map.empty → set literal
                if let Some(ann) = type_ann
                    && crate::codegen::common::is_set_annotation(ann)
                    && val == "map[]"
                {
                    val = "{}".to_string();
                }
                parts.push((aver_name_to_dafny(name), val));
            }
            Stmt::Expr(expr) => {
                if i == stmts.len() - 1 {
                    final_expr = Some(emit_expr_legacy(expr, ctx, None));
                }
            }
        }
    }

    if let Some(final_e) = final_expr {
        if parts.is_empty() {
            final_e
        } else {
            // Nest var bindings: var x := e1; var y := e2; body
            let mut result = final_e;
            for (name, val) in parts.into_iter().rev() {
                result = format!("var {} := {}; {}", name, val, result);
            }
            result
        }
    } else {
        // Last statement was a binding — return unit
        "()".to_string()
    }
}

/// Per-param self-call summary used by `infer_decreases` to pick a
/// `decreases` clause that actually decreases. For each formal
/// param, classify how every self-call site passes that position:
///   * `preserved_to(p)` — every self-call passes `p` unchanged at
///     position `i`. Picking `|p|` (or `p`) for `decreases` would
///     emit a clause Dafny rejects.
///   * `incremented(p)` — every self-call passes `p + k` (k > 0).
///     Identifies the moving index in functions of the shape
///     `fn(s: String, pos: Int, start: Int)` where `start` is
///     fixed and `pos` is the iterator.
struct SelfCallChanges {
    preserved: std::collections::HashSet<String>,
    incremented: std::collections::HashSet<String>,
    /// True when we observed at least one self-call site (so the
    /// `preserved`/`incremented` sets are meaningful — without any
    /// call observed everything would default to "preserved").
    saw_call: bool,
}

impl SelfCallChanges {
    fn preserved_to(&self, name: &str) -> bool {
        self.saw_call && self.preserved.contains(name)
    }
    fn incremented(&self, name: &str) -> bool {
        self.saw_call && self.incremented.contains(name)
    }
}

fn analyse_self_call_args(fd: &FnDef) -> SelfCallChanges {
    let mut state = SelfCallChanges {
        preserved: fd.params.iter().map(|(n, _)| n.clone()).collect(),
        incremented: fd.params.iter().map(|(n, _)| n.clone()).collect(),
        saw_call: false,
    };
    let formals: Vec<(String, String)> = fd.params.clone();
    walk_self_call_args(fd.body.as_ref(), &fd.name, &formals, &mut state);
    state
}

fn walk_self_call_args(
    body: &FnBody,
    fn_name: &str,
    formals: &[(String, String)],
    state: &mut SelfCallChanges,
) {
    let FnBody::Block(stmts) = body;
    for stmt in stmts {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                walk_self_call_args_expr(expr, fn_name, formals, state);
            }
        }
    }
}

fn walk_self_call_args_expr(
    expr: &Spanned<Expr>,
    fn_name: &str,
    formals: &[(String, String)],
    state: &mut SelfCallChanges,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            let is_self = matches!(&callee.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == fn_name);
            if is_self && args.len() == formals.len() {
                record_self_call(args, formals, state);
            }
            walk_self_call_args_expr(callee, fn_name, formals, state);
            for a in args {
                walk_self_call_args_expr(a, fn_name, formals, state);
            }
        }
        Expr::TailCall(call) if call.target == fn_name && call.args.len() == formals.len() => {
            record_self_call(&call.args, formals, state);
            for a in &call.args {
                walk_self_call_args_expr(a, fn_name, formals, state);
            }
        }
        Expr::TailCall(call) => {
            for a in &call.args {
                walk_self_call_args_expr(a, fn_name, formals, state);
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_self_call_args_expr(l, fn_name, formals, state);
            walk_self_call_args_expr(r, fn_name, formals, state);
        }
        Expr::Attr(b, _) | Expr::Neg(b) | Expr::ErrorProp(b) => {
            walk_self_call_args_expr(b, fn_name, formals, state);
        }
        Expr::Match { subject, arms } => {
            walk_self_call_args_expr(subject, fn_name, formals, state);
            for arm in arms {
                walk_self_call_args_expr(&arm.body, fn_name, formals, state);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items {
                walk_self_call_args_expr(it, fn_name, formals, state);
            }
        }
        Expr::Constructor(_, Some(inner)) => {
            walk_self_call_args_expr(inner, fn_name, formals, state);
        }
        _ => {}
    }
}

fn record_self_call(
    args: &[Spanned<Expr>],
    formals: &[(String, String)],
    state: &mut SelfCallChanges,
) {
    state.saw_call = true;
    for (i, (pname, _)) in formals.iter().enumerate() {
        let arg = &args[i].node;
        // Preserved iff arg is `Ident(p)` referencing the same param.
        let preserved_here = matches!(
            arg,
            Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == pname
        );
        if !preserved_here {
            state.preserved.remove(pname);
        }
        // Incremented iff arg is `BinOp(Add, Ident(p), Literal(k))` or
        // `BinOp(Add, Literal(k), Ident(p))` with k > 0.
        let incremented_here = match arg {
            Expr::BinOp(BinOp::Add, l, r) => {
                is_param_plus_positive_lit(&l.node, &r.node, pname)
                    || is_param_plus_positive_lit(&r.node, &l.node, pname)
            }
            _ => false,
        };
        if !incremented_here {
            state.incremented.remove(pname);
        }
    }
}

fn is_param_plus_positive_lit(maybe_param: &Expr, maybe_lit: &Expr, pname: &str) -> bool {
    let same = matches!(maybe_param, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == pname);
    let positive = matches!(maybe_lit, Expr::Literal(Literal::Int(k)) if *k > 0);
    same && positive
}

/// Check if a function body contains a recursive call to itself.
fn body_has_recursive_call(body: &FnBody, fn_name: &str) -> bool {
    match body {
        FnBody::Block(stmts) => stmts.iter().any(|s| match s {
            Stmt::Binding(_, _, expr) => expr_has_call(expr, fn_name),
            Stmt::Expr(expr) => expr_has_call(expr, fn_name),
        }),
    }
}

fn expr_has_call(expr: &Spanned<Expr>, fn_name: &str) -> bool {
    match &expr.node {
        Expr::FnCall(fn_expr, args) => {
            if let Expr::Ident(name) = &fn_expr.node
                && name == fn_name
            {
                return true;
            }
            expr_has_call(fn_expr, fn_name) || args.iter().any(|a| expr_has_call(a, fn_name))
        }
        Expr::TailCall(inner) => {
            let TailCallData {
                target: name, args, ..
            } = inner.as_ref();
            name == fn_name || args.iter().any(|a| expr_has_call(a, fn_name))
        }
        Expr::BinOp(_, l, r) => expr_has_call(l, fn_name) || expr_has_call(r, fn_name),
        Expr::Match { subject, arms, .. } => {
            expr_has_call(subject, fn_name)
                || arms.iter().any(|arm| expr_has_call(&arm.body, fn_name))
        }
        Expr::List(elems) => elems.iter().any(|e| expr_has_call(e, fn_name)),
        Expr::Tuple(elems) => elems.iter().any(|e| expr_has_call(e, fn_name)),
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_has_call(k, fn_name) || expr_has_call(v, fn_name)),
        Expr::Constructor(_, arg) => arg.as_ref().is_some_and(|a| expr_has_call(a, fn_name)),
        Expr::Attr(obj, _) => expr_has_call(obj, fn_name),
        Expr::ErrorProp(inner) => expr_has_call(inner, fn_name),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(e) => expr_has_call(e, fn_name),
            _ => false,
        }),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_has_call(e, fn_name)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_has_call(base, fn_name) || updates.iter().any(|(_, e)| expr_has_call(e, fn_name))
        }
        _ => false,
    }
}

/// Decreases info: the expression and any required preconditions.
struct DecreasesInfo {
    expr: String,
    /// `requires` clauses needed to ensure the decreases expression is bounded.
    requires: Vec<String>,
}

/// Try to infer a `decreases` clause from the function signature.
fn infer_decreases(fd: &FnDef) -> Option<DecreasesInfo> {
    // Walk the body to learn which params actually change across
    // self-calls. The plain type-priority pick (`prefer List/String,
    // fall back to Int`) emits clauses Dafny rejects when the chosen
    // param is constant across the recursion (`decreases |char_|` on
    // `repeat(char_, n)` where `char_` is preserved). It also picks
    // the wrong Int when there are two — naively taking the last
    // gives `|s| - start` on `scanExpTail(s, pos, start)` where
    // `start` is the fixed reference and `pos` is the moving index.
    let changes = analyse_self_call_args(fd);

    // Index-based pattern: pick the Int that strictly increments
    // across self-calls (the moving index) and pair it with a
    // collection param. The collection itself can be preserved —
    // `|s|` is the upper bound, `|s| - n` decreases when `n` grows.
    // Earlier code picked the LAST Int as the index unconditionally,
    // which gave nonsense like `|s| - start` on
    // `scanExpTail(s, pos, start)` where `start` is the fixed
    // reference and `pos` is the moving iterator.
    let collection_param_any = fd
        .params
        .iter()
        .find(|(_, t)| t.starts_with("List<") || t == "String");
    let incrementing_int = fd
        .params
        .iter()
        .find(|(name, t)| t == "Int" && changes.incremented(name));
    if let (Some((list_name, _)), Some((int_name, _))) = (collection_param_any, incrementing_int) {
        let dlist = aver_name_to_dafny(list_name);
        let dint = aver_name_to_dafny(int_name);
        return Some(DecreasesInfo {
            expr: format!("|{}| - {}", dlist, dint),
            requires: vec![],
        });
    }

    // Structural recursion: pick the FIRST List/String param whose
    // self-call argument is tail-stripped (`xs[1..]` or pattern
    // destructure that recurses on `rest`). Falls back to skipping
    // preserved params — emitting `|p|` on a constant `p` would
    // produce a clause Dafny rejects.
    for (pname, ptype) in &fd.params {
        if ptype.starts_with("List<") && !changes.preserved_to(pname) {
            return Some(DecreasesInfo {
                expr: format!("|{}|", aver_name_to_dafny(pname)),
                requires: vec![],
            });
        }
    }
    for (pname, ptype) in &fd.params {
        if ptype == "String" && !changes.preserved_to(pname) {
            return Some(DecreasesInfo {
                expr: format!("|{}|", aver_name_to_dafny(pname)),
                requires: vec![],
            });
        }
    }
    // Countdown pattern: Int param, no collection to walk.
    //
    // Two shapes to distinguish:
    // (a) Source handles the n<0 branch itself via `match n < 0 { true
    //     -> base; false -> … recur(n-1, …) }` — the recursive call
    //     never fires for negative n, so `decreases if n >= 0 then n
    //     else 0` suffices without any precondition.
    // (b) Source only discriminates by `match n { 0 -> base; _ -> recur
    //     (n-1, …) }`. The wildcard arm catches negative n too, and
    //     Dafny reasons that path would step from n = -1 to n = -2
    //     (0 → 0 in the guarded decreases expr — doesn't decrease).
    //     Pin the termination argument with `requires n >= 0`; real
    //     callers already guard negative values.
    for (pname, ptype) in &fd.params {
        if ptype == "Int" {
            let dname = aver_name_to_dafny(pname);
            if fn_handles_negative_first(fd, pname) {
                return Some(DecreasesInfo {
                    expr: format!("if {} >= 0 then {} else 0", dname, dname),
                    requires: vec![],
                });
            }
            return Some(DecreasesInfo {
                expr: dname.clone(),
                requires: vec![format!("{} >= 0", dname)],
            });
        }
    }
    None
}

/// True when the Aver body opens with a guard that explicitly handles
/// the negative case for `pname` before any recursive call — i.e. the
/// author took care of it themselves. Only a top-level shape check:
/// `match pname <op> <lit> { true -> base; false -> recur }` where
/// the `true` arm covers every value `< 0`. Recognised shapes:
/// `pname < 0`, `pname <= 0`, `pname < 1` (each pins `pname > 0` —
/// or `pname >= 0` for `< 0` — in the recursive arm, which is what
/// `decreases if pname >= 0 then pname else 0` needs to step). Anything
/// deeper is conservative (defaults to "doesn't handle", which emits a
/// `requires`).
fn fn_handles_negative_first(fd: &FnDef, pname: &str) -> bool {
    let Some(first) = fd.body.stmts().first() else {
        return false;
    };
    let expr = match first {
        Stmt::Expr(e) => e,
        Stmt::Binding(_, _, _) => return false,
    };
    // `match pname <op> <lit> { true -> …; false -> … }` elaborates
    // to a Match with a BinOp(op, pname, Literal::Int(lit)) subject.
    // The resolver rewrites `Ident(pname)` to `Resolved { name }`
    // before codegen, so accept both shapes.
    let Expr::Match { subject, .. } = &expr.node else {
        return false;
    };
    let Expr::BinOp(op, lhs, rhs) = &subject.node else {
        return false;
    };
    let lhs_name = match &lhs.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n,
        _ => return false,
    };
    if lhs_name != pname {
        return false;
    }
    let Expr::Literal(crate::ast::Literal::Int(rhs_val)) = &rhs.node else {
        return false;
    };
    use crate::ast::BinOp;
    matches!(
        (op, *rhs_val),
        (BinOp::Lt, 0) | (BinOp::Lte, 0) | (BinOp::Lt, 1)
    )
}

/// Get the top-level function name from a law expression like `fib(n)`.
fn law_top_level_fn(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::FnCall(fn_expr, _) => crate::codegen::common::expr_to_dotted_name(&fn_expr.node),
        _ => None,
    }
}

/// Check if a function is directly recursive (calls itself in its own body).
///
/// Stage 5 of #232: routes through `ctx.program_shape` when available
/// (set by `build_context`), reading the typed `Archetype::StructuralRecursion`
/// label that `analyze_program` already computed once. Falls back to
/// the legacy AST-walk path when `program_shape` is `None` (test
/// harnesses that bypass `build_context`).
///
/// Both paths must agree on every existing law's pinned ProofStrategy;
/// the snapshot-style proof tests in `tests/proof_spec.rs` cover
/// that invariant.
fn is_directly_recursive(fn_name: &str, ctx: &CodegenContext) -> bool {
    if let Some(shape) = ctx.program_shape.as_ref()
        && let Some(fd) = ctx.resolved_program.fn_by_name(fn_name)
        && let Some(recognition) = shape.for_fn(fd.fn_id)
    {
        return recognition
            .labels
            .contains(&crate::analysis::shape::Archetype::StructuralRecursion);
    }
    // Legacy fallback: walks the typed AST. Kept for ctx-by-hand
    // test setups; production paths route through shape.
    ctx.fn_defs
        .iter()
        .any(|fd| fd.name == fn_name && body_has_recursive_call(&fd.body, &fd.name))
}

fn count_recursive_calls(expr: &Spanned<Expr>, fn_name: &str) -> usize {
    match &expr.node {
        Expr::FnCall(fn_expr, args) => {
            let self_call = if let Expr::Ident(name) = &fn_expr.node {
                if name == fn_name { 1 } else { 0 }
            } else {
                0
            };
            self_call
                + count_recursive_calls(fn_expr, fn_name)
                + args
                    .iter()
                    .map(|a| count_recursive_calls(a, fn_name))
                    .sum::<usize>()
        }
        Expr::TailCall(inner) => {
            let TailCallData {
                target: name, args, ..
            } = inner.as_ref();
            let self_call = if name == fn_name { 1 } else { 0 };
            self_call
                + args
                    .iter()
                    .map(|a| count_recursive_calls(a, fn_name))
                    .sum::<usize>()
        }
        Expr::BinOp(_, l, r) => {
            count_recursive_calls(l, fn_name) + count_recursive_calls(r, fn_name)
        }
        Expr::Match { subject, arms, .. } => {
            // Count max across arms (not sum — we want per-branch count)
            let subj = count_recursive_calls(subject, fn_name);
            let arm_max = arms
                .iter()
                .map(|arm| count_recursive_calls(&arm.body, fn_name))
                .max()
                .unwrap_or(0);
            subj + arm_max
        }
        _ => 0,
    }
}

fn count_recursive_calls_in_body(body: &FnBody, fn_name: &str) -> usize {
    match body {
        FnBody::Block(stmts) => stmts
            .iter()
            .map(|s| match s {
                Stmt::Binding(_, _, expr) => count_recursive_calls(expr, fn_name),
                Stmt::Expr(expr) => count_recursive_calls(expr, fn_name),
            })
            .sum(),
    }
}

/// Maximum number of sample assertions per law.
/// Z3 can time out on deeply recursive computations, so we cap the
/// samples to keep verification times reasonable.
const MAX_LAW_SAMPLES: usize = 5;

/// Maximum literal magnitude (absolute value) for which a sample
/// lemma in opaque (mutual-rec) mode is expected to close as a real
/// proof. Above this, Dafny's fuel-bounded encoding can't drive Z3
/// through symbolic unfolding for examples like BigInt that pack
/// base-10⁹ digits — a value of `1_000_000_000` produces a 2-digit
/// decomposition and the SCC walk exceeds what Z3's unfolding will
/// chase. Per-sample lemmas above this fall back to `assume
/// {:axiom}` in the body (matching Lean's `sorry` for unreachable
/// shapes); the bounded-∀ universal that dispatches to them still
/// composes a real proof (just with mixed real/assume samples).
/// Tracked in #81 for a structural fix (native `decreases` tuple
/// over the SCC measure, which would remove this cliff entirely).
const SAMPLE_CLOSABLE_LITERAL_LIMIT: i64 = 999_999_999;

/// Walk the case's `(given_name, value_expr)` bindings and decide
/// whether every literal value is within the fuel-closable range
/// (see [`SAMPLE_CLOSABLE_LITERAL_LIMIT`]).  Used to gate the
/// per-sample lemma body between real proof attempt (`{}`) and
/// `assume {:axiom}` trust.
fn sample_within_closable_range(bindings: &[(String, Spanned<Expr>)]) -> bool {
    bindings.iter().all(|(_, v)| match literal_int_value(v) {
        Some(s) => s
            .parse::<i64>()
            .map(|n| n.abs() <= SAMPLE_CLOSABLE_LITERAL_LIMIT)
            .unwrap_or(false),
        // Non-Int givens (list literals, records) attempt a real
        // proof — `{}` body, let Dafny chase it. The cutoff is only
        // an honest fallback for the *specific* Int-literal cliff
        // BigInt's 10⁹ sits on; for other shapes we'd rather see
        // the failure than paper over it with `assume {:axiom}`.
        None => true,
    })
}

/// Emit sample assertions from a law's domain expansion as a test method.
/// These are concrete smoke tests (e.g. `assert fib(5) == fibSpec(5)`).
/// Capped at [`MAX_LAW_SAMPLES`] to avoid Z3 timeouts on large domains.
pub fn emit_law_samples(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    suffix: &str,
    opaque_fns: &std::collections::HashSet<crate::ir::FnId>,
    fuel_emitted: &std::collections::HashSet<crate::ir::FnId>,
    native_emitted: &std::collections::HashSet<crate::ir::FnId>,
) -> Option<String> {
    if vb.cases.is_empty() {
        return None;
    }

    // Issue #127: skip samples whose LHS projects through `.trace.*`.
    // The lifted Dafny fn returns the bare value, no trace buffer —
    // every per-sample `assert lhs.trace.event(K) == ...` would fail
    // on missing-field. `emit_verify_law` emits the runtime-only
    // marker for the law itself; sample lemmas follow the same gate.
    if crate::codegen::common::law_lhs_has_trace_projection(&law.lhs) {
        return None;
    }

    let fn_name = aver_name_to_dafny(&vb.fn_name);
    let law_name = aver_name_to_dafny(&law.name);

    // Pre-pre-pass: rewrite the first sample to detect whether the
    // law reaches an opaque (mutual-rec) callee. Opaque mode emits
    // *all* cases as per-sample lemmas (no cap) so the universal
    // bounded-∀ in `emit_verify_law` can case-split to one lemma
    // per pair. Non-opaque keeps the historical cap for Z3 budget.
    let first_rewrite = vb.cases.first().map(|(lhs, rhs)| {
        let case_bindings = vb.case_givens.first().map(|v| v.as_slice()).unwrap_or(&[]);
        let mode = OracleInjectionMode::SampleCaseBinding(case_bindings);
        (
            rewrite_effectful_calls_in_law(
                lhs,
                law,
                |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                mode.clone(),
            ),
            rewrite_effectful_calls_in_law(
                rhs,
                law,
                |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                mode,
            ),
        )
    });
    let any_opaque = first_rewrite
        .as_ref()
        .map(|(l, r)| {
            law_refs_opaque_fn(l, ctx, opaque_fns) || law_refs_opaque_fn(r, ctx, opaque_fns)
        })
        .unwrap_or(false);
    // Native-decreases mutual recursion is *not* opaque (Dafny can
    // unfold these on its own), but the universal `add_commutative
    // (a, b: int)` over `int × int` still doesn't close as a true ∀
    // — so we route through the bounded-∀ form with per-pair sample
    // lemmas the same way. The lemma bodies stay `{}` (real proof)
    // because there's no fuel ceiling to dodge.
    let any_native_mutual = first_rewrite
        .as_ref()
        .map(|(l, r)| {
            law_refs_opaque_fn(l, ctx, native_emitted) || law_refs_opaque_fn(r, ctx, native_emitted)
        })
        .unwrap_or(false);
    let needs_bounded_form = any_opaque || any_native_mutual;

    // Only lift the sample cap when the universal lemma will *also*
    // emit as bounded-∀ (every given Int + Explicit literal-int
    // domain). For other shapes (List/Json givens, open Int givens),
    // per-sample lemma form stays capped at `MAX_LAW_SAMPLES` — the
    // bigger budget without a corresponding universal proof just
    // produces more per-sample failures without buying any reasoning
    // power. BigInt-style Int-domain laws keep the full grid.
    let bounded_universal_targets = !law.givens.is_empty()
        && law.givens.iter().all(|g| {
            g.type_name == "Int"
                && matches!(
                    &g.domain,
                    VerifyGivenDomain::Explicit(vs)
                        if vs.iter().all(|v| literal_int_value(v).is_some())
                )
        });
    let cap = if needs_bounded_form && bounded_universal_targets {
        vb.cases.len()
    } else {
        MAX_LAW_SAMPLES
    };
    let samples: Vec<_> = vb.cases.iter().take(cap).collect();
    let truncated = vb.cases.len() > cap;

    let rewritten: Vec<(Spanned<Expr>, Spanned<Expr>)> = samples
        .iter()
        .enumerate()
        .map(|(idx, (lhs, rhs))| {
            let case_bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
            let mode = OracleInjectionMode::SampleCaseBinding(case_bindings);
            let lhs_rw = rewrite_effectful_calls_in_law(
                lhs,
                law,
                |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                mode.clone(),
            );
            let rhs_rw = rewrite_effectful_calls_in_law(
                rhs,
                law,
                |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                mode,
            );
            (lhs_rw, rhs_rw)
        })
        .collect();

    let mut lines = Vec::new();
    if truncated {
        lines.push(format!(
            "// Sample assertions for {}.{} ({} of {} from given domain)",
            fn_name,
            law_name,
            samples.len(),
            vb.cases.len()
        ));
    } else {
        lines.push(format!(
            "// Sample assertions for {}.{} (from given domain)",
            fn_name, law_name
        ));
    }

    if needs_bounded_form {
        // Per-sample lemma form. Each gets fuel bumped on every
        // (transitive) callee + the matching `__fuel` helper for
        // mutual-rec SCC members.
        let known: std::collections::HashSet<String> = ctx
            .items
            .iter()
            .filter_map(|i| {
                if let TopLevel::FnDef(fd) = i {
                    Some(fd.name.clone())
                } else {
                    None
                }
            })
            .chain(
                ctx.modules
                    .iter()
                    .flat_map(|m| m.fn_defs.iter().map(|fd| fd.name.clone())),
            )
            .collect();
        for (idx, (lhs_rw, rhs_rw)) in rewritten.iter().enumerate() {
            let l = emit_expr_legacy(lhs_rw, ctx, None);
            let r = emit_expr_legacy(rhs_rw, ctx, None);
            let mut callees = std::collections::BTreeSet::new();
            crate::codegen::proof_recognize::collect_called_fns(lhs_rw, &mut callees);
            crate::codegen::proof_recognize::collect_called_fns(rhs_rw, &mut callees);
            // Full transitive closure — 1-level was missing deep
            // SCC members (addLeft → addStep → addDigits → ...).
            // Without them, fuel attrs only land on direct callees
            // and Z3 leaves the rest sealed by their wrappers'
            // metric, which is enough for `add(0, X)` (recursion
            // bottoms out immediately) but not for `(X, Y)` with
            // multi-digit operands.
            let mut changed = true;
            while changed {
                changed = false;
                let snapshot: Vec<String> = callees.iter().cloned().collect();
                for f in &snapshot {
                    if let Some(fd) = ctx
                        .items
                        .iter()
                        .filter_map(|i| {
                            if let TopLevel::FnDef(fd) = i {
                                Some(fd)
                            } else {
                                None
                            }
                        })
                        .chain(ctx.modules.iter().flat_map(|m| m.fn_defs.iter()))
                        .find(|fd| &fd.name == f)
                    {
                        let before = callees.len();
                        crate::codegen::proof_recognize::collect_called_fns_in_body(
                            &fd.body,
                            &mut callees,
                        );
                        if callees.len() != before {
                            changed = true;
                        }
                    }
                }
            }
            let mut fuel_targets: Vec<String> = Vec::new();
            for f in &callees {
                if !known.contains(f) {
                    continue;
                }
                fuel_targets.push(aver_name_to_dafny(f));
                if crate::codegen::common::fn_id_for_dotted_name(ctx, f)
                    .is_some_and(|id| fuel_emitted.contains(&id))
                {
                    fuel_targets.push(crate::codegen::recursion::fuel_helper_name(f));
                }
            }
            fuel_targets.sort();
            fuel_targets.dedup();
            let fuel_attrs = fuel_targets
                .iter()
                .map(|f| format!("{{:fuel {}, 100}}", f))
                .collect::<Vec<_>>()
                .join(" ");
            // Real-proof body when every transitive callee is on
            // the native-decreases path — Dafny unfolds the SCC
            // freely from a `{}` body, no fuel ceiling. When any
            // callee stayed on fuel encoding, gate by literal
            // magnitude: small enough to fit Dafny's fuel-driven
            // symbolic unfolding gets `{}` body, anything past the
            // cliff (e.g. BigInt's 10⁹) falls back to `assume
            // {:axiom}` so the ensures is still available to the
            // bounded-∀ universal even if this pair isn't a real
            // proof.
            let bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
            let all_native = callees.iter().all(|f| {
                !crate::codegen::common::fn_id_for_dotted_name(ctx, f)
                    .is_some_and(|id| fuel_emitted.contains(&id))
            });
            let body = if all_native || sample_within_closable_range(bindings) {
                "{ }".to_string()
            } else {
                format!("{{\n  assume {{:axiom}} {} == {};\n}}", l, r)
            };
            lines.push(format!(
                "lemma {} {}_{}{}__sample_{}()\n  ensures {} == {}\n{}",
                fuel_attrs,
                fn_name,
                law_name,
                suffix,
                idx + 1,
                l,
                r,
                body
            ));
        }
    } else {
        // Mirror the universal lemma's fuel attrs onto the sample method.
        // A `function` with `decreases` does not unfold inside a bare
        // `assert` without fuel, so a concrete sample like
        // `length([1, 0]) == S(length([1]))` spuriously fails to verify even
        // though the universal law (which carries `{:fuel}`) proves — masking
        // a genuinely-closed proof behind a sample error.
        let mut sample_fns = std::collections::BTreeSet::new();
        crate::codegen::proof_recognize::collect_called_fns(&law.lhs, &mut sample_fns);
        crate::codegen::proof_recognize::collect_called_fns(&law.rhs, &mut sample_fns);
        let mut transitive = std::collections::BTreeSet::new();
        for f in &sample_fns {
            if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
                crate::codegen::proof_recognize::collect_called_fns_in_body(
                    &fd.body,
                    &mut transitive,
                );
            }
        }
        sample_fns.extend(transitive);
        let sample_fuel: String = sample_fns
            .iter()
            .filter(|f| {
                ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref())
                    .is_some()
            })
            .map(|f| format!("{{:fuel {}, 5}}", aver_name_to_dafny(f)))
            .collect::<Vec<_>>()
            .join(" ");
        let fuel_prefix = if sample_fuel.is_empty() {
            String::new()
        } else {
            format!("{} ", sample_fuel)
        };
        lines.push(format!(
            "method {}test_{}_{}{}_samples() {{",
            fuel_prefix, fn_name, law_name, suffix
        ));
        for (lhs_rw, rhs_rw) in &rewritten {
            let l = emit_expr_legacy(lhs_rw, ctx, None);
            let r = emit_expr_legacy(rhs_rw, ctx, None);
            // `{:split_here}` tells Dafny to check the preceding assert as
            // its own VC — without it, Z3 accumulates hypothesis state
            // across all samples in the method and occasionally times out
            // on otherwise-trivial arithmetic (e.g. `sub(a, b) == 0 -
            // sub(b, a)` over 5 samples). Splitting isolates each sample.
            lines.push(format!("  assert {{:split_here}} {} == {};", l, r));
        }
        lines.push("}\n".to_string());
    }
    Some(lines.join("\n"))
}

use crate::codegen::common::{OracleInjectionMode, rewrite_effectful_calls_in_law};

/// Emit a verify law as a Dafny lemma.
/// Compute the transitive closure of opaque fns: any fn whose body
/// (directly or transitively) calls a fn already in `opaque`. Dafny
/// can't unfold a mutually-recursive callee inside a `{:fuel}`-bound
/// SCC, so a law that calls a thin wrapper `add(a,b) = addDigits(...)`
/// can't be proved either — even though `add` itself isn't in the
/// SCC. Match Lean's `sorry` fallback by treating the wrapper as
/// opaque too.
pub fn transitive_opaque_closure(
    ctx: &CodegenContext,
    opaque: &std::collections::HashSet<crate::ir::FnId>,
) -> std::collections::HashSet<crate::ir::FnId> {
    let mut result = opaque.clone();
    let all_fns: Vec<&FnDef> = ctx
        .items
        .iter()
        .filter_map(|it| {
            if let TopLevel::FnDef(fd) = it {
                Some(fd)
            } else {
                None
            }
        })
        .chain(ctx.modules.iter().flat_map(|m| m.fn_defs.iter()))
        .collect();
    let mut changed = true;
    while changed {
        changed = false;
        for fd in &all_fns {
            let Some(fd_id) = crate::codegen::common::fn_id_for_decl(ctx, fd) else {
                continue;
            };
            if result.contains(&fd_id) {
                continue;
            }
            let mut callees = std::collections::BTreeSet::new();
            crate::codegen::proof_recognize::collect_called_fns_in_body(&fd.body, &mut callees);
            let hits = callees.iter().any(|name| {
                crate::codegen::common::fn_id_for_dotted_name(ctx, name)
                    .is_some_and(|id| result.contains(&id))
            });
            if hits {
                result.insert(fd_id);
                changed = true;
            }
        }
    }
    result
}

fn law_refs_opaque_fn(
    expr: &Spanned<Expr>,
    ctx: &CodegenContext,
    opaque: &std::collections::HashSet<crate::ir::FnId>,
) -> bool {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            let hits_callee = crate::codegen::common::expr_to_dotted_name(&callee.node)
                .and_then(|n| crate::codegen::common::fn_id_for_dotted_name(ctx, &n))
                .is_some_and(|id| opaque.contains(&id));
            hits_callee
                || law_refs_opaque_fn(callee, ctx, opaque)
                || args.iter().any(|a| law_refs_opaque_fn(a, ctx, opaque))
        }
        Expr::BinOp(_, l, r) => {
            law_refs_opaque_fn(l, ctx, opaque) || law_refs_opaque_fn(r, ctx, opaque)
        }
        Expr::Match { subject, arms } => {
            law_refs_opaque_fn(subject, ctx, opaque)
                || arms
                    .iter()
                    .any(|a| law_refs_opaque_fn(&a.body, ctx, opaque))
        }
        Expr::Attr(inner, _) | Expr::ErrorProp(inner) => law_refs_opaque_fn(inner, ctx, opaque),
        Expr::Constructor(_, Some(inner)) => law_refs_opaque_fn(inner, ctx, opaque),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|i| law_refs_opaque_fn(i, ctx, opaque))
        }
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, v)| law_refs_opaque_fn(v, ctx, opaque)),
        Expr::RecordUpdate { base, updates, .. } => {
            law_refs_opaque_fn(base, ctx, opaque)
                || updates
                    .iter()
                    .any(|(_, v)| law_refs_opaque_fn(v, ctx, opaque))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(inner) => law_refs_opaque_fn(inner, ctx, opaque),
            _ => false,
        }),
        _ => false,
    }
}

/// Emit the Dafny support-theorem stack for a
/// `LinearRecurrence2SpecEquivalence` law. The structure mirrors
/// PR #113's Lean template:
///
/// 1. `<spec>__nat: nat -> int` — direct Nat-keyed recurrence,
///    structurally recursive (no fuel needed for Z3 to unfold).
/// 2. `<helper>__natWorker: nat -> int -> int -> int` — Nat-keyed
///    image of the tail-rec helper.
/// 3. `__worker_nat_shift` lemma — pairing identity between
///    worker iteration and direct recurrence indexing.
/// 4. `__helper_nat` lemma — the impl's helper at `int.from(k)`
///    equals the Nat worker at `k`.
/// 5. `__helper_seed` lemma — closes the wrapper call at seeds.
/// 6. `__spec_nat_bridge` — direct spec at `int.from(k)` equals
///    direct Nat recurrence at `k`.
/// 7. Main `<fn>_<law>` lemma — splits on `n < 0` and discharges
///    the non-negative branch via the bridge + seed lemmas.
///
/// The names of seed expressions (`0`, `1`) and the recurrence
/// step are hard-coded here for the canonical Fibonacci shape;
/// a fully-generic implementation would extract the worker step
/// from `helper_shape.recurrence` (`AffinePairExpr`) and the base
/// values from `spec_shape.base0/base1` the way the Lean emit
/// does. Today only `fib`/`fibSpec` reaches this code path;
/// generalising to arbitrary affine recurrences is a follow-up
/// when a second example exercises the shape.
fn emit_linear_recurrence2_support_stack(
    impl_fn: &str,
    spec_fn: &str,
    helper_fn: &str,
    impl_dafny: &str,
    law_name_dafny: &str,
) -> String {
    let impl_d = aver_name_to_dafny(impl_fn);
    let spec_d = aver_name_to_dafny(spec_fn);
    let helper_d = aver_name_to_dafny(helper_fn);
    let spec_nat = format!("{spec_d}__nat");
    let worker_nat = format!("{helper_d}__natWorker");
    let theorem_base = format!("{impl_dafny}_{law_name_dafny}");
    let shift_thm = format!("{theorem_base}__worker_nat_shift");
    let helper_nat_thm = format!("{theorem_base}__helper_nat");
    let helper_seed_thm = format!("{theorem_base}__helper_seed");
    let spec_bridge_thm = format!("{theorem_base}__spec_nat_bridge");

    let mut lines = Vec::new();
    lines.push(format!(
        "// Law: {impl_fn}.{spec_fn} — recurrence support stack"
    ));
    lines.push(format!(
        "function {spec_nat}(n: nat): int {{ if n == 0 then 0 else if n == 1 then 1 else {spec_nat}(n - 1) + {spec_nat}(n - 2) }}"
    ));
    lines.push(format!(
        "function {worker_nat}(k: nat, a: int, b: int): int {{ if k == 0 then a else {worker_nat}(k - 1, b, a + b) }}"
    ));
    lines.push(format!(
        "lemma {shift_thm}(k: nat, i: nat)\n  ensures {worker_nat}(k, {spec_nat}(i), {spec_nat}(i + 1)) == {spec_nat}(i + k)\n{{\n  if k == 0 {{\n  }} else {{\n    {shift_thm}(k - 1, i + 1);\n  }}\n}}"
    ));
    lines.push(format!(
        "lemma {{:fuel {helper_d}, 100}} {helper_nat_thm}(k: nat, a: int, b: int)\n  ensures {helper_d}(k as int, a, b) == {worker_nat}(k, a, b)\n{{\n  if k == 0 {{\n  }} else {{\n    {helper_nat_thm}(k - 1, b, a + b);\n  }}\n}}"
    ));
    lines.push(format!(
        "lemma {helper_seed_thm}(k: nat)\n  ensures {helper_d}(k as int, 0, 1) == {spec_nat}(k)\n{{\n  {helper_nat_thm}(k, 0, 1);\n  {shift_thm}(k, 0);\n}}"
    ));
    lines.push(format!(
        "lemma {{:fuel {spec_d}, 100}} {spec_bridge_thm}(k: nat)\n  ensures {spec_d}(k as int) == {spec_nat}(k)\n{{\n  if k == 0 {{\n  }} else if k == 1 {{\n  }} else {{\n    {spec_bridge_thm}(k - 1);\n    {spec_bridge_thm}(k - 2);\n  }}\n}}"
    ));
    lines.push(format!(
        "lemma {{:fuel {impl_d}, 100}} {theorem_base}(n: int)\n  ensures {impl_d}(n) == {spec_d}(n)\n{{\n  if n < 0 {{\n  }} else {{\n    var k := n as nat;\n    {helper_seed_thm}(k);\n    {spec_bridge_thm}(k);\n  }}\n}}\n"
    ));
    lines.join("\n")
}

/// Stage 8 of #232: support stack for `ProofStrategy::WrapperOverRecursion`.
/// Emits the accumulator-decomposition aux lemma and the main universal
/// lemma — `examples/data/sum_acc.av` is the canonical case. Z3 closes
/// both via list induction; the aux lemma is the lifting that naive
/// induction on the law can't supply by itself.
///
/// Output shape:
/// ```dafny
/// lemma <inner>_acc(xs: seq<int>, a: int)
///   ensures <inner>(xs, a) == a <op> <inner>(xs, <neutral>)
///   decreases |xs|
/// { if |xs| > 0 { <inner>_acc(xs[1..], a <op> xs[0]); <inner>_acc(xs[1..], xs[0]); } }
///
/// lemma <law_theorem>(xs: seq<int>)
///   ensures <wrapper>(xs) == <other>(xs)
///   decreases |xs|
/// { if |xs| > 0 { <inner>_acc(xs[1..], xs[0]); <law_theorem>(xs[1..]); } }
/// ```
fn emit_wrapper_over_recursion_support_stack(
    wrapper_fn: &str,
    inner_fn: &str,
    other_fn: &str,
    combine_op: crate::ast::BinOp,
    impl_dafny: &str,
    law_name_dafny: &str,
) -> String {
    let op_dafny = match combine_op {
        crate::ast::BinOp::Add => "+",
        crate::ast::BinOp::Mul => "*",
        crate::ast::BinOp::Sub => "-",
        _ => "+",
    };
    let neutral = match combine_op {
        crate::ast::BinOp::Mul => "1",
        _ => "0",
    };
    let wrapper_d = aver_name_to_dafny(wrapper_fn);
    let inner_d = aver_name_to_dafny(inner_fn);
    let other_d = aver_name_to_dafny(other_fn);
    let acc_thm = format!("{inner_d}__acc");
    let main_thm = format!("{impl_dafny}_{law_name_dafny}");

    let mut lines = Vec::new();
    lines.push(format!(
        "// Law: {wrapper_fn}.{law_name_dafny} — wrapper-over-recursion support stack"
    ));
    lines.push(format!(
        "lemma {acc_thm}(xs: seq<int>, a: int)\n  ensures {inner_d}(xs, a) == a {op_dafny} {inner_d}(xs, {neutral})\n  decreases |xs|\n{{\n  if |xs| > 0 {{\n    {acc_thm}(xs[1..], a {op_dafny} xs[0]);\n    {acc_thm}(xs[1..], xs[0]);\n  }}\n}}"
    ));
    lines.push(format!(
        "lemma {{:fuel {wrapper_d}, 5}} {{:fuel {other_d}, 5}} {main_thm}(xs: seq<int>)\n  ensures {wrapper_d}(xs) == {other_d}(xs)\n  decreases |xs|\n{{\n  if |xs| > 0 {{\n    {acc_thm}(xs[1..], xs[0]);\n    {main_thm}(xs[1..]);\n  }}\n}}\n"
    ));
    lines.join("\n")
}

pub fn emit_verify_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    opaque_fns: &std::collections::HashSet<crate::ir::FnId>,
    native_emitted: &std::collections::HashSet<crate::ir::FnId>,
    suffix: &str,
) -> String {
    let fn_name = aver_name_to_dafny(&vb.fn_name);
    let law_name = aver_name_to_dafny(&law.name);

    // Issue #127: trace-projection LHS has no proof-side shape — the
    // lifted Dafny fn returns the bare value, no `.trace` field. Emit
    // a comment marker; `aver verify` still runs the law under stubs.
    // Mirror of the Lean gate in `emit_verify_law_block`.
    if crate::codegen::common::law_lhs_has_trace_projection(&law.lhs) {
        return format!(
            "// Law {}.{}{}: trace-projection LHS is runtime-only (see docs/oracle.md)",
            fn_name, law_name, suffix,
        );
    }

    // Issue #128: singleton-domain givens + constant RHS + IR didn't
    // pin a strategy that closes the constant-RHS shape ⇒ universal
    // lemma is vacuous or outright false. Sample assertions in
    // `emit_law_samples` cover the actual point. Induction /
    // BackendDispatch / Sorry don't close constant-RHS shapes;
    // anything else (Reflexive, Associative, MapUpdatePostcondition,
    // …) does and stays. Mirror of the Lean gate.
    let vb_fn_id = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name));
    let ir_strategy_closes_const_rhs = vb_fn_id
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .is_some_and(|t| {
            !matches!(
                t.strategy,
                crate::ir::ProofStrategy::Induction { .. }
                    | crate::ir::ProofStrategy::BackendDispatch
                    | crate::ir::ProofStrategy::Sorry
                    // No dedicated Dafny emit for the enum constant-fold
                    // strategy — it falls through to the default fuel-
                    // bumped lemma (Z3 unfolds the non-recursive fns and
                    // decides the constructor branch). Treat it like
                    // `BackendDispatch` for the singleton-domain gate so
                    // Dafny's behaviour is unchanged from before the
                    // Lean-only strategy existed.
                    | crate::ir::ProofStrategy::EnumConstantFold { .. }
                    // Same guard for the finite-domain-cases strategy:
                    // it is Lean-only (exhaustive `cases` enumeration),
                    // Dafny keeps its pre-strategy behaviour — the
                    // default fuel-bumped lemma where it can, the
                    // singleton-domain / fuel-bounded sample-only gates
                    // where it can't. Byte-identical to before the
                    // strategy existed.
                    | crate::ir::ProofStrategy::FiniteDomainCases { .. }
                    // Same guard for the prelude-simp strategy: Lean-only
                    // (its registry maps to Lean prelude lemma names),
                    // so Dafny treats the pin as `BackendDispatch` and
                    // its exports stay byte-identical.
                    | crate::ir::ProofStrategy::SimpOverPreludeLemmas { .. }
            )
        });
    let singleton_const_rhs = !ir_strategy_closes_const_rhs
        && crate::codegen::common::all_givens_are_singletons(law)
        && crate::codegen::common::law_rhs_is_independent_of_givens(law);
    // Issue #128: same fuel-bounded gate as Lean — laws calling fns
    // the classifier rejected (`size`, `toSorted`, …) can't be
    // closed by Dafny's `decreases`-driven induction either; the
    // `__fuel`-style wrapper hides the structural decrease. Sample
    // assertions still cover the declared domain.
    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    let calls_fuel_bounded = crate::codegen::common::law_calls_unclassified_fn(law, &unclassified);
    if singleton_const_rhs || calls_fuel_bounded {
        let reason = if singleton_const_rhs {
            "singleton-domain givens with constant RHS"
        } else {
            "calls a fuel-bounded fn outside the proof subset"
        };
        return format!(
            "// Law {}.{}{}: {}, sample-only (universal lemma omitted)",
            fn_name, law_name, suffix, reason,
        );
    }

    // IR-pinned `LinearRecurrence2SpecEquivalence` — emit a full
    // support-theorem stack (Nat helper + worker_nat_shift +
    // helper_nat + helper_seed + spec_nat_bridge + main lemma)
    // that closes the equivalence between a tail-rec wrapper impl
    // and a direct recurrence spec. Mirror of PR #113 on the Lean
    // side; the algebraic content is identical, the syntactic
    // template is target-specific. Returns early; the default
    // fuel-only lemma body Dafny would otherwise emit can't close
    // this shape (Z3 doesn't bridge tail-rec accumulator state to
    // the direct recurrence from fuel unfolding alone).
    if let Some(crate::ir::ProofStrategy::LinearRecurrence2SpecEquivalence {
        impl_fn,
        spec_fn,
        helper_fn,
    }) = vb_fn_id
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .map(|t| t.strategy.clone())
    {
        return emit_linear_recurrence2_support_stack(
            &impl_fn, &spec_fn, &helper_fn, &fn_name, &law_name,
        );
    }

    // Stage 8b of #232 — `ResultPipelineChain` (Dafny needs only a
    // fuel-bumped trivial body; Z3 unfolds both fns and closes by
    // structural equality). Explicit branch makes the strategy
    // choice observable in proof_ir.
    if let Some(crate::ir::ProofStrategy::ResultPipelineChain {
        chain_qm_fn,
        chain_manual_fn,
        step_fns,
    }) = vb_fn_id
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .map(|t| t.strategy.clone())
    {
        let qm_d = aver_name_to_dafny(&chain_qm_fn);
        let manual_d = aver_name_to_dafny(&chain_manual_fn);
        let main_thm = format!("{fn_name}_{law_name}");
        let mut fuels: Vec<String> = vec![
            format!("{{:fuel {qm_d}, 5}}"),
            format!("{{:fuel {manual_d}, 5}}"),
        ];
        for s in &step_fns {
            fuels.push(format!("{{:fuel {}, 5}}", aver_name_to_dafny(s)));
        }
        return format!(
            "// Law: {chain_qm_fn}.{law_name} — result-pipeline chain equivalence\nlemma {} {main_thm}(n: int)\n  ensures {qm_d}(n) == {manual_d}(n)\n{{\n}}\n",
            fuels.join(" "),
        );
    }

    // Stage 8 of #232 — `WrapperOverRecursion` support stack.
    if let Some(crate::ir::ProofStrategy::WrapperOverRecursion {
        wrapper_fn,
        inner_fn,
        other_fn,
        combine_op,
    }) = vb_fn_id
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .map(|t| t.strategy.clone())
    {
        return emit_wrapper_over_recursion_support_stack(
            &wrapper_fn,
            &inner_fn,
            &other_fn,
            combine_op,
            &fn_name,
            &law_name,
        );
    }

    // Refinement lift: for each Int given whose value is wrapped in
    // a refinement record on either side (e.g. `IntRange(value = a)`),
    // promote the param type from `int` to the refined name so the
    // invariant rides in the type and the `when`-clause guard
    // becomes redundant. Mirror of the Lean Subtype lift.
    let mut lifted_vars: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for g in &law.givens {
        if let Some(refined) = crate::codegen::common::refinement_lift_for_given(
            &g.name,
            &g.type_name,
            &law.lhs,
            &law.rhs,
            ctx,
        ) {
            lifted_vars.insert(g.name.clone(), refined.to_string());
        }
    }

    let params: Vec<String> = law
        .givens
        .iter()
        .map(|g| {
            if let Some(refined) = lifted_vars.get(&g.name) {
                // `refined` is a canonical key — bare for entry
                // types, `Module.Name` for module-owned. Translate
                // the module prefix to Dafny's `Aver_Module.Name`
                // form so the lemma signature picks up the actual
                // module-emitted subset type.
                let display = match refined.rsplit_once('.') {
                    Some((prefix, bare)) => {
                        format!("{}.{}", super::dafny_module_name(prefix), bare)
                    }
                    None => refined.clone(),
                };
                return format!("{}: {}", aver_name_to_dafny(&g.name), display);
            }
            // Oracle v1: if the given's "type" is a classified effect
            // reference (`Random.int`, `Http.get`, etc.), the param is
            // an oracle — emit the derived oracle signature instead of
            // the effect name as a type. `oracle_signature` gives
            // `(BranchPath, Int, args...) -> T` for generative /
            // generative+output and `(args...) -> T` for snapshot.
            let type_text = match crate::types::checker::effect_classification::oracle_signature(
                &g.type_name,
            ) {
                Some(oracle_ty) => type_ref_to_dafny(&oracle_ty),
                None => emit_type(&g.type_name),
            };
            format!("{}: {}", aver_name_to_dafny(&g.name), type_text)
        })
        .collect();

    // Oracle v1: rewrite calls to effectful fns in the law body so
    // they target the lifted form. Surface source writes
    // `pickOne() => pickOneSpec(BranchPath.root(), rnd)`, but the
    // lifted `pickOne` takes `(path, rnd_Random_int, <orig_args>)`.
    // Inject `BranchPath.root()` + the matching given identifier for
    // each classified non-output effect in the callee's signature.
    let law_lhs = rewrite_effectful_calls_in_law(
        &law.lhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        OracleInjectionMode::LemmaBinding,
    );
    let law_rhs = rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        OracleInjectionMode::LemmaBinding,
    );

    // Refinement-lift wrapper stripping: when a given was promoted to
    // a refined type, the source-written `X(value = a)` constructor
    // is redundant — emit `a` directly so the lemma body type-checks
    // against the lifted param.
    let (law_lhs, law_rhs) = if lifted_vars.is_empty() {
        (law_lhs, law_rhs)
    } else {
        (
            crate::codegen::common::strip_refinement_wrappers(&law_lhs, &lifted_vars, ctx),
            crate::codegen::common::strip_refinement_wrappers(&law_rhs, &lifted_vars, ctx),
        )
    };

    let lhs = emit_expr_legacy(&law_lhs, ctx, None);
    let rhs = emit_expr_legacy(&law_rhs, ctx, None);

    // Proof lemma library: per-shape recognizers contribute proved helper
    // lemmas (prepended) + `forall`-lifted facts (hoisted into the body), e.g.
    // additive-monoid op laws and the rev anti-homomorphism. See
    // `super::lemmas`. The per-step cons bridges come later, at the
    // list-induction site, via `super::lemmas::list_bridges`.
    let law_uid = format!("{}_{}", fn_name, law_name);
    let super::lemmas::AlgebraLemmas {
        defs: op_lemma_defs,
        lifts: op_lifts,
    } = super::lemmas::algebra_lemmas(law, ctx, &law_uid);

    let mut lines = Vec::new();
    // Collect all functions used in the law for fuel annotations
    let mut law_fns = std::collections::BTreeSet::new();
    crate::codegen::proof_recognize::collect_called_fns(&law.lhs, &mut law_fns);
    crate::codegen::proof_recognize::collect_called_fns(&law.rhs, &mut law_fns);
    // Add transitive callees
    let mut transitive_fns = std::collections::BTreeSet::new();
    for f in &law_fns {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            crate::codegen::proof_recognize::collect_called_fns_in_body(
                &fd.body,
                &mut transitive_fns,
            );
        }
    }
    law_fns.extend(transitive_fns);

    // Oracle v1: fuel attrs only for names that resolve to top-level
    // functions. Callees collected from lifted effectful bodies can
    // include oracle / capability params (e.g. `rnd_Random_int`,
    // `oracle`) that Dafny sees as lambda variables — emitting
    // `{:fuel oracle, 5}` makes Dafny reject the lemma.
    let fuel_attrs: String = law_fns
        .iter()
        .filter(|f| {
            ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref())
                .is_some()
        })
        .map(|f| format!("{{:fuel {}, 5}}", aver_name_to_dafny(f)))
        .collect::<Vec<_>>()
        .join(" ");

    lines.push(format!("// Law: {}.{}", fn_name, law_name));
    if fuel_attrs.is_empty() {
        lines.push(format!(
            "lemma {}_{}({})",
            fn_name,
            law_name,
            params.join(", ")
        ));
    } else {
        lines.push(format!(
            "lemma {} {}_{}({})",
            fuel_attrs,
            fn_name,
            law_name,
            params.join(", ")
        ));
    }

    // Subtype-equivalent for Dafny: ghost predicates from
    // `oracle_subtypes::dafny_subtype_predicates` describe the
    // runtime invariant for each classified Generative-shape effect
    // (`IsRandomIntInBounds`, `IsRandomFloatInUnit`,
    // `IsTimeUnixMsNonneg`). Bind each oracle-given to its predicate
    // via `requires` so the lemma is exercised only against oracles
    // that respect the bound — same enforcement as Lean's subtype
    // carriers, just using Dafny's idiom (predicate + requires)
    // instead of first-class subtype types over functions.
    for given in &law.givens {
        if let Some(pred) = bounded_oracle_predicate_for(&given.type_name) {
            let oracle_name = aver_name_to_dafny(&given.name);
            lines.push(format!("  requires {}({})", pred, oracle_name));
        }
    }

    // `when` is dropped only when it's syntactically equivalent (via
    // commutator-relaxed compare) to the conjunction of lifted givens'
    // refinement invariants — otherwise stronger / orthogonal user
    // predicates would be silently lost (e.g. `when a >= 10` over `a :
    // Natural` whose invariant is `a.val >= 0`). Same identity check
    // the Lean backend uses.
    if let Some(when_expr) = &law.when {
        let when_redundant = crate::codegen::common::when_is_redundant_with_refinement_lifts(
            when_expr,
            &lifted_vars,
            ctx,
        );
        if !when_redundant {
            let when_str = emit_expr_legacy(when_expr, ctx, None);
            lines.push(format!("  requires {}", when_str));
        }
    }

    // Bounded-∀ detection: when the law reaches mutual-rec SCC fns
    // AND every given has an Explicit literal-int domain, emit a
    // bounded universal — `requires a == k1 || ... ` per given,
    // body case-splits on `(a, b, ...)` tuple and dispatches to the
    // per-pair sample lemma (each fuel-bumped or assume-bodied per
    // SAMPLE_CLOSABLE_LITERAL_LIMIT). Lean parity: bounded ∀ over
    // the declared domain closed by `rcases` + `native_decide` per
    // case. Falls back to `assume {:axiom}` for open-domain opaque
    // (e.g. `given x: Int` without explicit values, oracle givens).
    let is_opaque = law_refs_opaque_fn(&law.lhs, ctx, opaque_fns)
        || law_refs_opaque_fn(&law.rhs, ctx, opaque_fns);
    // Native-decreases mutual recursion isn't opaque (Dafny unfolds
    // it), but the universal `add_commutative(a, b: int)` over
    // `int × int` still doesn't close as a true ∀ without a domain
    // restriction. Route through the bounded-∀ form the same way
    // opaque does — the case-split body composes per-pair sample
    // lemmas that Dafny *can* close from `{}` on the native path.
    let is_native_mutual = law_refs_opaque_fn(&law.lhs, ctx, native_emitted)
        || law_refs_opaque_fn(&law.rhs, ctx, native_emitted);
    let needs_bounded_form = is_opaque || is_native_mutual;
    let all_explicit_int = !law.givens.is_empty()
        && law.givens.iter().all(|g| {
            (g.type_name == "Int" || lifted_vars.contains_key(&g.name))
                && matches!(
                    &g.domain,
                    VerifyGivenDomain::Explicit(vs)
                        if vs.iter().all(|v| literal_int_value(v).is_some())
                )
        });
    if needs_bounded_form && all_explicit_int {
        for given in &law.givens {
            let values = match &given.domain {
                VerifyGivenDomain::Explicit(vs) => vs,
                _ => unreachable!(),
            };
            let n = aver_name_to_dafny(&given.name);
            let disj = values
                .iter()
                .map(|v| format!("{} == {}", n, literal_int_value(v).unwrap()))
                .collect::<Vec<_>>()
                .join(" || ");
            lines.push(format!("  requires {}", disj));
        }
    }

    lines.push(format!("  ensures {} == {}", lhs, rhs));
    lines.push("{".to_string());

    // Hoist additive-op facts at the top of the body (inductive paths only;
    // bounded-form bodies dispatch to samples and never reach the universal).
    if !needs_bounded_form {
        for lift in &op_lifts {
            lines.push(lift.clone());
        }
    }

    if needs_bounded_form {
        if all_explicit_int {
            // Per-pair case split. Each case_givens[idx] gives the
            // concrete (name, value) pairs for this case; emit
            // `if a == k_a && b == k_b { sample_lemma_{idx+1}(); }`.
            // Dafny derives the universal `ensures` from the union
            // of case conjuncts (which together cover `requires`).
            for (idx, _) in vb.cases.iter().enumerate() {
                let Some(bindings) = vb.case_givens.get(idx) else {
                    continue;
                };
                let guard = bindings
                    .iter()
                    .map(|(n, v)| {
                        let val =
                            literal_int_value(v).unwrap_or_else(|| emit_expr_legacy(v, ctx, None));
                        format!("{} == {}", aver_name_to_dafny(n), val)
                    })
                    .collect::<Vec<_>>()
                    .join(" && ");
                let sample_name = format!("{}_{}{}__sample_{}", fn_name, law_name, suffix, idx + 1);
                lines.push(format!("  if {} {{ {}(); }}", guard, sample_name));
            }
            lines.push("}\n".to_string());
            return lines.join("\n");
        }
        // Open-domain opaque (no explicit literal values per given):
        // keep the `sorry`-style fallback. `{:axiom}` on the assume
        // silences Dafny's warning about unannotated assumes.
        lines.push(format!("  assume {{:axiom}} {} == {};", lhs, rhs));
        lines.push("}\n".to_string());
        return lines.join("\n");
    }

    // Generate inductive proof body for Int-parameterized laws
    if law.givens.len() == 1 && law.givens[0].type_name == "Int" {
        let param = aver_name_to_dafny(&law.givens[0].name);
        let lemma_name = format!("{}_{}", fn_name, law_name);

        // Check if both sides of the law use directly-recursive functions on `param`.
        // If so, generate inductive hints. Otherwise, let Z3 try alone.
        let lhs_fn = law_top_level_fn(&law.lhs);
        let rhs_fn = law_top_level_fn(&law.rhs);

        let lhs_recursive = lhs_fn
            .as_ref()
            .is_some_and(|f| is_directly_recursive(f, ctx));
        let rhs_recursive = rhs_fn
            .as_ref()
            .is_some_and(|f| is_directly_recursive(f, ctx));

        if lhs_recursive || rhs_recursive {
            // Find max recursion depth across both sides
            let has_double = [&lhs_fn, &rhs_fn].iter().any(|opt| {
                opt.as_ref().is_some_and(|f| {
                    ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref())
                        .is_some_and(|fd| count_recursive_calls_in_body(&fd.body, &fd.name) >= 2)
                })
            });

            lines.push(format!("  if {} < 0 {{", param));
            lines.push(format!("  }} else if {} == 0 {{", param));
            lines.push(format!("  }} else if {} == 1 {{", param));
            lines.push("  } else {".to_string());
            lines.push(format!("    {}({} - 1);", lemma_name, param));
            if has_double {
                lines.push(format!("    {}({} - 2);", lemma_name, param));
            }
            lines.push("  }".to_string());
        }
    } else if let Some(list_given_idx) = law
        .givens
        .iter()
        .position(|g| g.type_name.starts_with("List<") || g.type_name == "String")
    {
        // Inductive hint for `List<T>` / `String`-parameterised laws —
        // both lower to Dafny `seq`, so `|s| == 0` / `s[1..]` works for
        // either. Case-split `[] / [head, ..tail]` and recurse on the
        // tail. Fires when any fn called from either side (top-level
        // OR nested transitively) is directly recursive — broader than
        // the Int-given branch which only inspects the top-level fn,
        // because these laws often wrap the recursive fn under a
        // Map / Option helper (e.g. `Map.has(countWords(words), word)`
        // — countWords is recursive but `Map.has` is the top fn), or
        // behind a thin facade (`decodeString = String.join(decode(...))`
        // — `decode` is recursive but `decodeString` isn't).
        let mut called: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        crate::codegen::proof_recognize::collect_called_fns(&law.lhs, &mut called);
        crate::codegen::proof_recognize::collect_called_fns(&law.rhs, &mut called);
        for f in called.clone() {
            if let Some(fd) = ctx.fn_def_by_name(&f, ctx.active_module_scope().as_deref()) {
                crate::codegen::proof_recognize::collect_called_fns_in_body(&fd.body, &mut called);
            }
        }
        let any_recursive = called.iter().any(|f| is_directly_recursive(f, ctx));
        if any_recursive {
            let list_param = aver_name_to_dafny(&law.givens[list_given_idx].name);
            let lemma_name = format!("{}_{}", fn_name, law_name);
            let other_args: Vec<String> = law
                .givens
                .iter()
                .enumerate()
                .map(|(i, g)| {
                    if i == list_given_idx {
                        format!("{}[1..]", list_param)
                    } else {
                        aver_name_to_dafny(&g.name)
                    }
                })
                .collect();
            // Cons-decomposition bridges for folds over `concat(<ind_var>, ys)`
            // and the rev unfold — `super::lemmas::list_bridges` assembles the
            // exact assert lines (base → `|xs| == 0` arm, step → `else` arm).
            // Without these Z3 hunts for the seq decomposition and times out.
            let ind_var_src = &law.givens[list_given_idx].name;
            let bridges = super::lemmas::list_bridges(law, ctx, &list_param, ind_var_src);

            lines.push(format!("  if |{}| == 0 {{", list_param));
            for assert in &bridges.base {
                lines.push(assert.clone());
            }
            lines.push("  } else {".to_string());
            for assert in &bridges.step {
                lines.push(assert.clone());
            }
            lines.push(format!("    {}({});", lemma_name, other_args.join(", ")));
            lines.push("  }".to_string());
        }
    }

    lines.push("}\n".to_string());

    // Prepend the proved additive-op lemmas the `forall`-lift above refers
    // to (only reached on the inductive path — bounded-form returns earlier).
    if op_lemma_defs.is_empty() {
        lines.join("\n")
    } else {
        format!("{}\n{}", op_lemma_defs.join("\n"), lines.join("\n"))
    }
}
