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

    // Guard-validated floor-division countdown (shared classifier —
    // `RecursionContract::WellFoundedToNat { floor_div: Some(_) }`):
    // the classifier proved every self-call site's guard chain
    // implies the shrinking param is >= 1, so the total guarded
    // measure verifies WITHOUT a synthesized `requires` — total
    // callers stay wellformed (a synthesized precondition on a
    // recursive fn breaks every caller that can't prove it).
    let floor_div_param = crate::codegen::common::find_fn_contract_for_fn(ctx, fd).and_then(
        |contract| match &contract.recursion {
            Some(crate::ir::RecursionContract::WellFoundedToNat {
                param,
                floor_div: Some(_),
            }) => Some(param.clone()),
            _ => None,
        },
    );
    if needs_decreases && let Some(param) = floor_div_param {
        let dname = aver_name_to_dafny(&param);
        lines.push(format!(
            "  decreases if {} >= 0 then {} else 0",
            dname, dname
        ));
    } else if needs_decreases && let Some(info) = infer_decreases(fd) {
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
    // Countdown pattern: Int param, no collection to walk. The pick is
    // validated by the shared recursion classifier
    // (`single_int_countdown_param_index`): every self-call must pass
    // the chosen param as `p - k` (literal k >= 1) or match the
    // negative-guarded ascent it folds into the same lane. The previous
    // unvalidated "first Int param" pick GUESSED a measure for
    // recursion outside every recognized pattern (doubling/halving on
    // a rational num/den pair — tests/fixtures/expo_outside_subset.av),
    // emitting a `decreases` Dafny rejects plus a synthesized
    // `requires p >= 0` that poisons every total caller. With no
    // validated pick this returns `None` and the caller routes the fn
    // to the opaque `{:axiom}` form instead.
    //
    // Two shapes to distinguish for the validated param:
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
    let validated_countdown_idx =
        crate::codegen::recursion::detect::single_int_countdown_param_index(fd)
            .or_else(|| div_shrink_param_index(fd));
    if let Some(idx) = validated_countdown_idx
        && let Some((pname, _)) = fd.params.get(idx)
    {
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
    None
}

/// First Int param that EVERY self-call shrinks by a literal-divisor
/// floor division — `Result.withDefault(Int.div(p, k), d)` with a
/// literal k >= 2 (the BigInt base-10⁹ digit peel,
/// `examples/refinement/bigint`). Z3 discharges `decreases p` for this
/// shape directly (`p / k < p` whenever the recursive branch implies
/// `p >= 1`), so it keeps the same countdown emission as the
/// classifier-validated pick instead of declining to an opaque axiom.
/// Deliberately Dafny-LOCAL: the shared classifier
/// (`single_int_countdown_param_index`) also feeds the Lean fuel
/// encoding, where admitting this shape would silently change that
/// backend's emission.
fn div_shrink_param_index(fd: &FnDef) -> Option<usize> {
    use crate::codegen::recursion::detect::{call_matches, collect_calls_from_body};
    let recursive_calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    if recursive_calls.is_empty() {
        return None;
    }
    fd.params
        .iter()
        .enumerate()
        .find_map(|(idx, (param_name, param_ty))| {
            if param_ty != "Int" {
                return None;
            }
            recursive_calls
                .iter()
                .all(|args| {
                    args.get(idx)
                        .copied()
                        .is_some_and(|arg| is_literal_div_shrink(arg, param_name))
                })
                .then_some(idx)
        })
}

/// `Result.withDefault(Int.div(p, k), _)` with literal `k >= 2`.
fn is_literal_div_shrink(expr: &Spanned<Expr>, param_name: &str) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    if crate::codegen::common::expr_to_dotted_name(&callee.node).as_deref()
        != Some("Result.withDefault")
        || args.len() != 2
    {
        return false;
    }
    let Expr::FnCall(div_callee, div_args) = &args[0].node else {
        return false;
    };
    crate::codegen::common::expr_to_dotted_name(&div_callee.node).as_deref() == Some("Int.div")
        && div_args.len() == 2
        && matches!(
            &div_args[0].node,
            Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == param_name
        )
        && matches!(
            &div_args[1].node,
            Expr::Literal(crate::ast::Literal::Int(k)) if *k >= 2
        )
}

/// Whether a `when` premise is a 2-arg call to a recognized canonical Peano
/// comparison fn (`≤` / `<` / `=`). This is the exact condition under which the
/// Nat-given lockstep-peel induction hint's recursive call satisfies its own
/// `requires`: the comparison unfolds `f(S a, S b) == f(a, b)`, so the outer
/// premise on the peeled-successor arguments reduces to the recursive premise.
/// A non-comparison (or otherwise non-lockstep) premise is rejected, so the
/// hint is never emitted where its recursion would violate a precondition.
fn when_is_peano_comparison(when: &Spanned<Expr>, ctx: &CodegenContext) -> bool {
    let Expr::FnCall(callee, args) = &when.node else {
        return false;
    };
    if args.len() != 2 {
        return false;
    }
    let Some(name) = crate::codegen::common::expr_to_dotted_name(&callee.node) else {
        return false;
    };
    let mut names = std::collections::BTreeSet::new();
    names.insert(name);
    !crate::codegen::proof_recognize::collect_nat_compare_ops_for_names(&names, ctx).is_empty()
}

/// Replace every WHOLE-WORD occurrence of identifier `word` in `s` with `repl`
/// (a substring flanked by identifier chars `[A-Za-z0-9_]` is left untouched).
/// Used to slice a `when` premise's induction-target reference to its tail
/// The Dafny-rendered THREADED accumulator argument for the inductive-hint
/// self-call of a list-accumulator-fold lemma — the Dafny counterpart of Lean's
/// `generalizing acc`. `fd` recurses via `match xs { [] -> _; [h, ..t] ->
/// fd(t, <acc_arg>) }`; this renders `<acc_arg>` (e.g. `acc + h`) in the lemma's
/// binders by re-expressing the cons head/tail binders as `<list>[0]` / `<list>
/// [1..]`. With the recursive lemma call at this threaded value (not the
/// unchanged param) the IH lands where the fold actually fed the accumulator, so
/// Z3 closes `fd(xs, acc) == acc <op> spec(xs)`. Returns `None` (caller keeps the
/// unchanged-arg fallback) unless `fd` is a single self-recursive list match
/// whose accumulator given name matches a threaded param.
fn dafny_threaded_accumulator_arg(
    fd: &FnDef,
    acc_given_name: &str,
    list_param_name: &str,
    list_dafny: &str,
    ctx: &CodegenContext,
) -> Option<String> {
    use crate::codegen::recursion::detect::{
        call_matches, collect_calls_from_body, param_threaded_in_recursion,
    };
    // The fn param the accumulator given binds to (by name), and it must be a
    // threaded accumulator of THIS recursion.
    let acc_idx = fd.params.iter().position(|(p, _)| p == acc_given_name)?;
    if !param_threaded_in_recursion(fd, acc_idx) {
        return None;
    }
    // The single self-call's accumulator argument (the threaded value).
    let calls: Vec<Vec<&Spanned<Expr>>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args)
        .collect();
    let acc_arg = calls.first()?.get(acc_idx).copied()?;
    // The list match's head/tail binders, to re-express in the lemma's terms.
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    if !matches!(&subject.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == list_param_name)
    {
        return None;
    }
    let (head, tail) = arms.iter().find_map(|a| match &a.pattern {
        Pattern::Cons(h, t) => Some((h.clone(), t.clone())),
        _ => None,
    })?;
    // Substitute on the DAFNY-RENDERED binder names (`emit_expr_legacy` mangles
    // a leading-underscore / reserved-word binder, so the source spelling would
    // miss), in ONE pass so the cons head/tail rewrites cannot cascade into each
    // other when a binder name collides with the list param. The list param, if
    // the accumulator arg mentions it, already renders as `list_dafny` (the fn's
    // list param shares the given's name — checked above), so no rewrite is
    // needed for it.
    let rendered = emit_expr_legacy(acc_arg, ctx, None);
    let subs = vec![
        (aver_name_to_dafny(&head), format!("{list_dafny}[0]")),
        (aver_name_to_dafny(&tail), format!("{list_dafny}[1..]")),
    ];
    let rendered = replace_ident_words(&rendered, &subs);
    Some(rendered)
}

/// The law given the fold structurally recurses on — its `match` subject mapped
/// back to a law given. `None` when the fn body isn't a single match on a given.
fn datatype_driver_given_name(fd: &FnDef, law: &VerifyLaw) -> Option<String> {
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, .. } = &body.node else {
        return None;
    };
    let subj = match &subject.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n,
        _ => return None,
    };
    law.givens
        .iter()
        .find(|g| &g.name == subj)
        .map(|g| g.name.clone())
}

/// Datatype-induction inductive hint for a law over a recursive USER ADT
/// (`triTR(n, acc) => plus(triSpec(n), acc)`), the Dafny counterpart of Lean's
/// `induction <n> generalizing acc`. Mirrors the verified fn's own `match` on
/// the driver given: each constructor arm that contains a self-call recurses the
/// LEMMA at that self-call's arguments (so the recursion lands on the field
/// predecessor while the accumulator is threaded exactly as the fold feeds it).
/// The cited commutativity/associativity laws (hoisted as `forall` facts above)
/// then discharge the residual. Returns the `match … { … }` body lines, or
/// `None` when the fn does not match the given or an arm is not a constructor
/// pattern. The lemma's params are the law givens in the fn's parameter order
/// (the law's lhs is `fn(givens…)`), so the self-call args render directly.
fn dafny_datatype_inductive_hint(
    fd: &FnDef,
    given_name: &str,
    lemma_name: &str,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<Vec<String>> {
    use crate::codegen::recursion::detect::{call_matches, collect_calls_from_expr};
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    if !matches!(&subject.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == given_name)
    {
        return None;
    }
    // The lemma's params are the law givens in DECLARED order, but the fn's
    // self-call passes its args in FN-PARAM order — which the law's lhs call
    // binds to givens positionally. Build, for each lemma param (given), the
    // fn-param index it occupies, so the recursive lemma call's args are
    // reordered to the lemma signature regardless of how the law declared its
    // givens vs the fn's parameter order. Declines (omit, safe) if the lhs is not
    // a plain `fn(given, …)` call covering every given.
    let ident_of = |e: &Spanned<Expr>| -> Option<String> {
        match &e.node {
            Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.clone()),
            _ => None,
        }
    };
    let Expr::FnCall(_, lhs_args) = &law.lhs.node else {
        return None;
    };
    let lhs_idents: Vec<String> = lhs_args.iter().filter_map(ident_of).collect();
    if lhs_idents.len() != lhs_args.len() {
        return None;
    }
    let given_to_fn_pos: Vec<usize> = law
        .givens
        .iter()
        .map(|g| lhs_idents.iter().position(|n| n == &g.name))
        .collect::<Option<Vec<usize>>>()?;
    let given_dafny = aver_name_to_dafny(given_name);
    let mut lines = vec![format!("  match {} {{", given_dafny)];
    for arm in arms {
        let Pattern::Constructor(cname, binders) = &arm.pattern else {
            return None;
        };
        let short = cname.rsplit('.').next().unwrap_or(cname);
        let binder_str = if binders.is_empty() {
            String::new()
        } else {
            format!(
                "({})",
                binders
                    .iter()
                    .map(|b| aver_name_to_dafny(b))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        // Recurse the lemma at each self-call's arguments (the predecessor driver
        // plus the threaded accumulator), reordered to the lemma's given order.
        let mut calls = Vec::new();
        collect_calls_from_expr(&arm.body, &mut calls);
        let recs: Vec<String> = calls
            .iter()
            .filter(|(n, _)| call_matches(n, &fd.name))
            .filter_map(|(_, args)| {
                let rendered: Vec<String> = given_to_fn_pos
                    .iter()
                    .map(|&i| args.get(i).map(|x| emit_expr_legacy(x, ctx, None)))
                    .collect::<Option<Vec<String>>>()?;
                Some(format!("{}({});", lemma_name, rendered.join(", ")))
            })
            .collect();
        if recs.is_empty() {
            lines.push(format!("    case {}{} =>", short, binder_str));
        } else {
            lines.push(format!(
                "    case {}{} => {}",
                short,
                binder_str,
                recs.join(" ")
            ));
        }
    }
    lines.push("  }".to_string());
    Some(lines)
}

/// (`y` -> `y[1..]`) when guarding a conditional list lemma's recursive call.
/// String-literal aware: an occurrence INSIDE a `"…"` literal (where the same
/// spelling is just text, e.g. a premise comparing against `"y"`) is left
/// untouched — slicing it would corrupt the literal.
fn replace_ident_word(s: &str, word: &str, repl: &str) -> String {
    if word.is_empty() {
        return s.to_string();
    }
    let is_ident = |c: char| c.is_ascii_alphanumeric() || c == '_';
    let chars: Vec<char> = s.chars().collect();
    let wlen = word.chars().count();
    let mut out = String::new();
    let mut i = 0;
    let mut in_string = false;
    while i < chars.len() {
        // Copy string literals verbatim, honoring `\"` / `\\` escapes, so a
        // matching identifier spelled inside a literal is never sliced.
        if in_string {
            out.push(chars[i]);
            if chars[i] == '\\' && i + 1 < chars.len() {
                out.push(chars[i + 1]);
                i += 2;
                continue;
            }
            if chars[i] == '"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        if chars[i] == '"' {
            in_string = true;
            out.push(chars[i]);
            i += 1;
            continue;
        }
        let matches_here = i + wlen <= chars.len()
            && chars[i..i + wlen].iter().collect::<String>() == word
            && (i == 0 || !is_ident(chars[i - 1]))
            && (i + wlen >= chars.len() || !is_ident(chars[i + wlen]));
        if matches_here {
            out.push_str(repl);
            i += wlen;
        } else {
            out.push(chars[i]);
            i += 1;
        }
    }
    out
}

/// Single-pass whole-word identifier substitution: at each identifier boundary
/// the FIRST matching `(from, to)` pair replaces the token, and the inserted
/// text is NOT re-scanned — so substitutions cannot cascade into one another
/// (mapping `h -> xs[0]` then `t -> xs[1..]` leaves an already-inserted `xs[0]`
/// intact even when a binder name collides with the list param). String literals
/// are copied verbatim. Used by `dafny_threaded_accumulator_arg`, where the cons
/// head/tail binder spellings can otherwise collide on a sequential rewrite.
fn replace_ident_words(s: &str, subs: &[(String, String)]) -> String {
    let is_ident = |c: char| c.is_ascii_alphanumeric() || c == '_';
    let chars: Vec<char> = s.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    let mut in_string = false;
    while i < chars.len() {
        if in_string {
            out.push(chars[i]);
            if chars[i] == '\\' && i + 1 < chars.len() {
                out.push(chars[i + 1]);
                i += 2;
                continue;
            }
            if chars[i] == '"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        if chars[i] == '"' {
            in_string = true;
            out.push(chars[i]);
            i += 1;
            continue;
        }
        let at_boundary = i == 0 || !is_ident(chars[i - 1]);
        let mut matched = false;
        if at_boundary {
            for (from, to) in subs {
                let wlen = from.chars().count();
                if wlen > 0
                    && i + wlen <= chars.len()
                    && chars[i..i + wlen].iter().collect::<String>() == *from
                    && (i + wlen >= chars.len() || !is_ident(chars[i + wlen]))
                {
                    out.push_str(to);
                    i += wlen;
                    matched = true;
                    break;
                }
            }
        }
        if !matched {
            out.push(chars[i]);
            i += 1;
        }
    }
    out
}

/// True when a self-recursive fn's termination cannot be justified by
/// any `decreases` pattern this emitter recognizes AND no parameter
/// offers Dafny's default lexicographic measure a structural ordering
/// to fall back on (recursive ADT / collection params keep the bare
/// emission — Dafny proves structural walks natively). Such fns
/// (doubling/halving recursion on a rational num/den pair, binade
/// search by repeated halving) must emit as opaque `{:axiom}`
/// declarations: a guessed measure produces a `decreases` error on a
/// correct function, and a synthesized `requires` breaks every total
/// caller's wellformedness.
pub(super) fn termination_guess_unjustified(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if !body_has_recursive_call(fd.body.as_ref(), &fd.name) {
        return false;
    }
    // Guard-validated floor-division countdown — the shared
    // classifier proved the measure, so the fn emits with a native
    // total-guard `decreases` (see `emit_fn_def`) instead of
    // declining to an opaque `{:axiom}`.
    if crate::codegen::common::find_fn_contract_for_fn(ctx, fd).is_some_and(|contract| {
        matches!(
            &contract.recursion,
            Some(crate::ir::RecursionContract::WellFoundedToNat {
                floor_div: Some(_),
                ..
            })
        )
    }) {
        return false;
    }
    if infer_decreases(fd).is_some() {
        return false;
    }
    fd.params.iter().all(|(_, t)| {
        matches!(
            t.as_str(),
            "Int" | "Float" | "Bool" | "String" | "Char" | "Byte"
        )
    })
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

/// Can the sample method seed each assert with a call to the law's
/// universal lemma instantiated at the sample values?
///
/// Probe finding (tests/fixtures/rational_probe.av): `{:fuel f, 5}`-
/// attributed sample asserts over record-literal arguments push Z3 into SYMBOLIC
/// unfolding instead of literal evaluation — 150 s+ timeouts on ground
/// `4*1 - 0*(-3) == …` arithmetic while the universal lemma itself
/// verifies in ~1 s, and the exit-status gate then fails the whole
/// otherwise-proven file. Calling `<fn>_<law>(<sample values>)` right
/// before the assert hands Z3 the instantiated `ensures` as a
/// hypothesis, so the assert discharges by congruence instead of
/// unfolding (measured: 150 s+ → <1 s). Soundness: if the universal
/// lemma is wrong or unprovable, ITS error already fails the file —
/// the seeded sample never turns a red file green.
///
/// `true` only when `emit_verify_law` will emit the DEFAULT universal
/// lemma `{fn}_{law}{suffix}(givens…)` whose params are exactly the
/// law's givens:
/// - not the trace-projection marker (caller never reaches here),
/// - not the "sample-only (universal lemma omitted)" marker
///   (singleton-domain const-RHS / fuel-bounded fn),
/// - not a special support stack (LinearRecurrence2 / ResultPipeline /
///   WrapperOverRecursion — those lemmas have different signatures),
/// - no refinement-lifted given (lemma param is the refined subset
///   type; sample values are carrier literals),
/// - no oracle-bounded given (lemma adds `requires Is…(oracle)`).
fn sample_seed_lemma_available(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> bool {
    // Mirror of the issue-#128 "universal lemma omitted" gate in
    // `emit_verify_law` — keep in sync.
    let vb_fn_id = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name));
    let pinned_strategy = vb_fn_id.and_then(|fn_id| {
        ctx.proof_ir
            .law_theorems
            .iter()
            .find(|t| t.fn_id == fn_id && t.law_name == law.name)
            .map(|t| &t.strategy)
    });
    let ir_strategy_closes_const_rhs = pinned_strategy.is_some_and(|s| {
        !matches!(
            s,
            crate::ir::ProofStrategy::Induction { .. }
                | crate::ir::ProofStrategy::BackendDispatch
                | crate::ir::ProofStrategy::Sorry
                | crate::ir::ProofStrategy::EnumConstantFold { .. }
                | crate::ir::ProofStrategy::FiniteDomainCases { .. }
                | crate::ir::ProofStrategy::SimpOverPreludeLemmas { .. }
                | crate::ir::ProofStrategy::RingIdentity { .. }
                | crate::ir::ProofStrategy::NonlinearNonneg { .. }
                | crate::ir::ProofStrategy::IntDecimalRoundtrip { .. }
                | crate::ir::ProofStrategy::StringEscapeRoundtrip(_)
        )
    });
    let singleton_const_rhs = !ir_strategy_closes_const_rhs
        && crate::codegen::common::all_givens_are_singletons(law)
        && crate::codegen::common::law_rhs_is_independent_of_givens(law);
    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    // Accumulator-fold reference — kept in sync with the universal-lemma gate in
    // `emit_verify_law` so the seed never references a lemma that gate omitted.
    // A foreign fold, or a self-fold whose universal can't close here (no
    // algebra helpers for its combine fn), stays sample-only.
    if singleton_const_rhs
        || crate::codegen::common::law_calls_unclassified_fn(law, &unclassified)
        || crate::codegen::common::dafny_should_bound_accumulator_fold(ctx, law, &vb.fn_name)
    {
        return false;
    }
    if matches!(
        pinned_strategy,
        Some(
            crate::ir::ProofStrategy::LinearRecurrence2SpecEquivalence { .. }
                | crate::ir::ProofStrategy::ResultPipelineChain { .. }
                | crate::ir::ProofStrategy::WrapperOverRecursion { .. }
        )
    ) {
        return false;
    }
    // Mirror of the floor-division omitted-universal gate in
    // `emit_verify_law`: a law whose cone reaches a guard-validated
    // floor-division countdown fn gets NO universal lemma unless its
    // `FloorDivWindow` figure is pinned — seeding a sample with a
    // call to a lemma that was never emitted would be a parse error.
    if !matches!(
        pinned_strategy,
        Some(crate::ir::ProofStrategy::FloorDivWindow { .. })
    ) && law_reaches_floor_div_fn(law, ctx)
    {
        return false;
    }
    law.givens.iter().all(|g| {
        bounded_oracle_predicate_for(&g.type_name).is_none()
            && crate::codegen::common::refinement_lift_for_given(
                &g.name,
                &g.type_name,
                &law.lhs,
                &law.rhs,
                ctx,
            )
            .is_none()
    })
}

/// Emit sample assertions from a law's domain expansion as a test method.
/// These are concrete smoke tests (e.g. `assert fib(5) == fibSpec(5)`).
/// Capped at [`MAX_LAW_SAMPLES`] to avoid Z3 timeouts on large domains.
#[allow(clippy::too_many_arguments)]
pub fn emit_law_samples(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    suffix: &str,
    opaque_fns: &std::collections::HashSet<crate::ir::FnId>,
    fuel_emitted: &std::collections::HashSet<crate::ir::FnId>,
    native_emitted: &std::collections::HashSet<crate::ir::FnId>,
    termination_opaque: &std::collections::HashSet<crate::ir::FnId>,
) -> Option<String> {
    if vb.cases.is_empty() {
        return None;
    }

    // Laws reaching a fn whose recursion was declined to an opaque
    // `{:axiom}` (termination outside every recognized `decreases`
    // pattern — tests/fixtures/expo_outside_subset.av): the axiom has
    // no body, so a sample assert/lemma about its value can never be
    // proved — emitting one manufactures a guaranteed verification
    // error on a law that may well hold. Report honestly instead;
    // `aver verify` still exercises these cases at runtime.
    if law_refs_opaque_fn(&law.lhs, ctx, termination_opaque)
        || law_refs_opaque_fn(&law.rhs, ctx, termination_opaque)
    {
        return Some(format!(
            "// Sample assertions for {}.{}{} omitted: the law reaches a recursive fn outside the proof subset (emitted as an opaque {{:axiom}}, nothing about its value is provable)",
            aver_name_to_dafny(&vb.fn_name),
            aver_name_to_dafny(&law.name),
            suffix,
        ));
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
            // `when`-laws: guard the per-sample lemma with the
            // instantiated premise (`requires`), mirroring Lean's
            // `_sample_N` hypothesis form — a premise-violating
            // combination from the unfiltered given product would
            // otherwise state a FALSE `ensures` the `{}` body can
            // never prove. The bounded-∀ universal's dispatch call
            // satisfies the `requires` from its own `requires <when>`
            // plus the case-split equalities.
            let requires_guard = law
                .sample_guards
                .get(idx)
                .map(|g| format!("  requires {}\n", emit_expr_legacy(g, ctx, None)))
                .unwrap_or_default();
            lines.push(format!(
                "lemma {} {}_{}{}__sample_{}()\n{}  ensures {} == {}\n{}",
                fuel_attrs,
                fn_name,
                law_name,
                suffix,
                idx + 1,
                requires_guard,
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
        // Seed each sample assert with the universal lemma
        // instantiated at the sample values (see
        // `sample_seed_lemma_available`) — keeps Z3 on congruence
        // instead of symbolic fuel unfolding, which timed out on
        // ground record arithmetic.
        // NB the universal lemma's name carries NO `suffix` (only the
        // sample method / bounded per-sample lemma names do).
        let seed_lemma =
            sample_seed_lemma_available(vb, law, ctx).then(|| format!("{}_{}", fn_name, law_name));
        for (idx, (lhs_rw, rhs_rw)) in rewritten.iter().enumerate() {
            let l = emit_expr_legacy(lhs_rw, ctx, None);
            let r = emit_expr_legacy(rhs_rw, ctx, None);
            let seed_call = seed_lemma.as_ref().and_then(|lemma| {
                let bindings = vb.case_givens.get(idx)?;
                let args = law
                    .givens
                    .iter()
                    .map(|g| {
                        bindings
                            .iter()
                            .find(|(n, _)| n == &g.name)
                            .map(|(_, v)| emit_expr_legacy(v, ctx, None))
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(format!("{}({});", lemma, args.join(", ")))
            });
            // `{:split_here}` tells Dafny to check the preceding assert as
            // its own VC — without it, Z3 accumulates hypothesis state
            // across all samples in the method and occasionally times out
            // on otherwise-trivial arithmetic (e.g. `sub(a, b) == 0 -
            // sub(b, a)` over 5 samples). Splitting isolates each sample.
            //
            // `when`-laws: the sample combination comes from the
            // UNFILTERED given cartesian product, so a combination can
            // violate the premise and make the bare assert FALSE on a
            // law Z3 proves universally (probe: square-monotonicity
            // asserted at e=1, b=0). Mirror Lean's `_sample_N` form —
            // which instantiates the premise as a hypothesis
            // (`<guard> = true -> lhs = rhs`) — by checking the sample
            // under `if <guard> { … }`: asserted exactly where the
            // premise holds, vacuous where it doesn't.
            match law.sample_guards.get(idx) {
                Some(guard) => {
                    let g = emit_expr_legacy(guard, ctx, None);
                    lines.push(format!("  if {} {{", g));
                    if let Some(call) = &seed_call {
                        lines.push(format!("    {}", call));
                    }
                    lines.push(format!("    assert {{:split_here}} {} == {};", l, r));
                    lines.push("  }".to_string());
                }
                None => {
                    if let Some(call) = &seed_call {
                        lines.push(format!("  {}", call));
                    }
                    lines.push(format!("  assert {{:split_here}} {} == {};", l, r));
                }
            }
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

/// The additive monoid fn a Peano `mul` is built on — the outer call of `mul`'s
/// succ arm (`S(z) -> plus(y, mul(z, y))`). Returns its source name.
fn peano_additive_callee(mul_fd: &FnDef) -> Option<String> {
    use crate::ast::{Expr, Pattern};
    let tail = mul_fd.body.tail_expr()?;
    let Expr::Match { arms, .. } = &tail.node else {
        return None;
    };
    for arm in arms {
        if let Pattern::Constructor(_, binders) = &arm.pattern
            && binders.len() == 1
            && let Expr::FnCall(callee, _) = &arm.body.node
        {
            return crate::codegen::common::expr_to_dotted_name(&callee.node);
        }
    }
    None
}

/// Stage 8 (Peano-`Nat` driver): support stack for a `factTR`-shape
/// multiplicative countdown fold. Unlike the `seq<int>` case — where Z3 knows
/// `int` `*` is associative/commutative for free — the user `mul` is a
/// function over a `Nat` datatype, so the monoid laws must be supplied as
/// induction lemmas (`mul_assoc` / `mul_comm` / `mul_one`, in turn resting on
/// the `plus` monoid) the decomposition and main lemmas lean on. Each lemma is
/// discharged by Dafny itself. Returns `None` for shapes outside the
/// value-first multiplicative countdown so the caller falls back to the honest
/// decline.
#[allow(clippy::too_many_arguments)]
fn emit_wrapper_nat_support_stack(
    ctx: &CodegenContext,
    wrapper_fn: &str,
    inner_fn: &str,
    other_fn: &str,
    combine_fn: &str,
    combine_op: crate::ast::BinOp,
    nat_type: &str,
    value_first: bool,
    impl_dafny: &str,
    law_name_dafny: &str,
) -> Option<String> {
    // Value-first countdown over a Peano `Nat`, additive OR multiplicative.
    // The `List`/`seq<int>` driver is the sibling stack; here the close is
    // chosen by the combine op — additive rests on the `plus` monoid alone,
    // multiplicative builds the `mul` monoid on top of it.
    if !value_first || !matches!(combine_op, crate::ast::BinOp::Add | crate::ast::BinOp::Mul) {
        return None;
    }

    // Resolve the Peano constructors (nullary base + unary self-recursive succ)
    // and the additive monoid fn `mul` rests on.
    let bare = nat_type.rsplit('.').next().unwrap_or(nat_type);
    let td = ctx
        .type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
        .find(|td| crate::codegen::common::type_def_name(td) == bare)?;
    let crate::ast::TypeDef::Sum { variants, .. } = td else {
        return None;
    };
    let base = variants.iter().find(|v| v.fields.is_empty())?.name.clone();
    let succ = variants
        .iter()
        .find(|v| v.fields.len() == 1 && v.fields[0].trim() == bare)?
        .name
        .clone();
    // The additive monoid: for a multiplicative combine (`mul`) it is the
    // callee of `mul`'s succ arm (`S(z) -> plus(y, mul(z, y))`); for an
    // additive combine the fn IS that monoid.
    let plus = match combine_op {
        crate::ast::BinOp::Mul => {
            let combine_fd =
                ctx.fn_def_by_name(combine_fn, ctx.active_module_scope().as_deref())?;
            aver_name_to_dafny(&peano_additive_callee(combine_fd)?)
        }
        _ => aver_name_to_dafny(combine_fn),
    };

    let t = bare.to_string();
    let inner = aver_name_to_dafny(inner_fn);
    let other = aver_name_to_dafny(other_fn);
    let wrap = aver_name_to_dafny(wrapper_fn);
    let zero = format!("{t}.{base}");
    let main = format!("{impl_dafny}_{law_name_dafny}");
    let p = format!("{main}__"); // law-scoped helper-lemma prefix

    // `plus` monoid helper-lemma names (shared by both combine ops).
    let (pz, ps, pc, pa) = (
        format!("{p}plus_zero_r"),
        format!("{p}plus_succ_r"),
        format!("{p}plus_comm"),
        format!("{p}plus_assoc"),
    );
    let acc = format!("{inner}__acc");

    let mut lines = Vec::new();
    lines.push(format!(
        "// Law: {wrapper_fn}.{law_name_dafny} — Peano-Nat wrapper-over-recursion support stack"
    ));
    // ── plus monoid (shared) ─────────────────────────────────────────────
    lines.push(format!(
        "lemma {pz}(x: {t})\n  ensures {plus}(x, {zero}) == x\n  decreases x\n{{ match x case {base} => case {succ}(q) => {pz}(q); }}"
    ));
    lines.push(format!(
        "lemma {ps}(x: {t}, y: {t})\n  ensures {plus}(x, {t}.{succ}(y)) == {t}.{succ}({plus}(x, y))\n  decreases x\n{{ match x case {base} => case {succ}(q) => {ps}(q, y); }}"
    ));
    lines.push(format!(
        "lemma {pc}(a: {t}, b: {t})\n  ensures {plus}(a, b) == {plus}(b, a)\n  decreases a\n{{ match a case {base} => {pz}(b); case {succ}(q) => {pc}(q, b); {ps}(b, q); }}"
    ));
    lines.push(format!(
        "lemma {pa}(a: {t}, b: {t}, c: {t})\n  ensures {plus}({plus}(a, b), c) == {plus}(a, {plus}(b, c))\n  decreases a\n{{ match a case {base} => case {succ}(q) => {pa}(q, b, c); }}"
    ));

    match combine_op {
        crate::ast::BinOp::Mul => {
            // ── mul monoid (built on the plus monoid) ────────────────────
            let mul = aver_name_to_dafny(combine_fn);
            let one = format!("{t}.{succ}({t}.{base})");
            let psw = format!("{p}plus_swap");
            let (om, mo, ma, mpd, mc, mz, msr) = (
                format!("{p}one_mul"),
                format!("{p}mul_one"),
                format!("{p}mul_assoc"),
                format!("{p}mul_plus_dist"),
                format!("{p}mul_comm"),
                format!("{p}mul_zero_r"),
                format!("{p}mul_succ_r"),
            );
            lines.push(format!(
                "lemma {psw}(a: {t}, b: {t}, c: {t})\n  ensures {plus}(a, {plus}(b, c)) == {plus}(b, {plus}(a, c))\n{{ {pa}(a, b, c); {pc}(a, b); {pa}(b, a, c); }}"
            ));
            lines.push(format!(
                "lemma {mz}(b: {t})\n  ensures {mul}(b, {zero}) == {zero}\n  decreases b\n{{ match b case {base} => case {succ}(w) => {mz}(w); }}"
            ));
            lines.push(format!(
                "lemma {om}(x: {t})\n  ensures {mul}({one}, x) == x\n{{ {pz}(x); }}"
            ));
            lines.push(format!(
                "lemma {mo}(a: {t})\n  ensures {mul}(a, {one}) == a\n  decreases a\n{{ match a case {base} => case {succ}(z) => {mo}(z); assert {plus}({one}, z) == {t}.{succ}(z); }}"
            ));
            lines.push(format!(
                "lemma {msr}(b: {t}, z: {t})\n  ensures {mul}(b, {t}.{succ}(z)) == {plus}(b, {mul}(b, z))\n  decreases b\n{{ match b case {base} => case {succ}(w) => {msr}(w, z); {psw}(z, w, {mul}(w, z)); }}"
            ));
            lines.push(format!(
                "lemma {mpd}(a: {t}, b: {t}, c: {t})\n  ensures {mul}({plus}(a, b), c) == {plus}({mul}(a, c), {mul}(b, c))\n  decreases a\n{{ match a case {base} => case {succ}(z) => {mpd}(z, b, c); {pa}(c, {mul}(z, c), {mul}(b, c)); }}"
            ));
            lines.push(format!(
                "lemma {ma}(a: {t}, b: {t}, c: {t})\n  ensures {mul}({mul}(a, b), c) == {mul}(a, {mul}(b, c))\n  decreases a\n{{ match a case {base} => case {succ}(z) => {ma}(z, b, c); {mpd}(b, {mul}(z, b), c); }}"
            ));
            lines.push(format!(
                "lemma {mc}(a: {t}, b: {t})\n  ensures {mul}(a, b) == {mul}(b, a)\n  decreases a\n{{ match a case {base} => {mz}(b); case {succ}(z) => {mc}(z, b); {msr}(b, z); }}"
            ));
            // ── accumulator decomposition + main law (multiplicative) ─────
            lines.push(format!(
                "lemma {acc}(n: {t}, a: {t})\n  ensures {inner}(n, a) == {mul}({inner}(n, {one}), a)\n  decreases n\n{{ match n case {base} => {om}(a); case {succ}(m) => {acc}(m, {mul}(n, a)); {acc}(m, {mul}(n, {one})); {mo}(n); {ma}({inner}(m, {one}), n, a); }}"
            ));
            lines.push(format!(
                "lemma {{:fuel {wrap}, 5}} {{:fuel {other}, 5}} {{:fuel {inner}, 5}} {main}(n: {t})\n  ensures {wrap}(n) == {other}(n)\n  decreases n\n{{ match n case {base} => case {succ}(m) => {main}(m); {acc}(m, {mul}(n, {one})); {mo}(n); {mc}({other}(m), n); }}\n"
            ));
        }
        _ => {
            // ── accumulator decomposition + main law (additive) ──────────
            // No `mul` monoid: the close rests on `plus` associativity and
            // commutativity alone, and the neutral is `zero` (the additive
            // identity). `inner n acc = plus (inner n zero) acc`.
            lines.push(format!(
                "lemma {acc}(n: {t}, a: {t})\n  ensures {inner}(n, a) == {plus}({inner}(n, {zero}), a)\n  decreases n\n{{ match n case {base} => case {succ}(m) => {acc}(m, {plus}(n, a)); {acc}(m, {plus}(n, {zero})); {pz}(n); {pa}({inner}(m, {zero}), n, a); }}"
            ));
            lines.push(format!(
                "lemma {{:fuel {wrap}, 5}} {{:fuel {other}, 5}} {{:fuel {inner}, 5}} {main}(n: {t})\n  ensures {wrap}(n) == {other}(n)\n  decreases n\n{{ match n case {base} => case {succ}(m) => {main}(m); {acc}(m, {plus}(n, {zero})); {pz}(n); {pc}({other}(m), n); }}\n"
            ));
        }
    }
    Some(lines.join("\n"))
}

/// TIP prop_35 (`exp x y = qexp x y one`): Dafny support stack for a
/// `TailRecFixedBaseFold` — the tail-recursive-with-fixed-base shape. Reuses the
/// `plus`/`mul` Peano monoid prelude, then the accumulator-decomposition lemma
/// `qexp(x, n, a) == mul(qexp(x, n, one), a)` (the extra `x` is held fixed
/// through the induction on `n`) and the main law `exp(x, n) == qexp(x, n, one)`.
/// The combine multiplies the accumulator by the FIXED base `x` (not the matched
/// subject). Returns `None` for shapes outside the multiplicative/additive Peano
/// combine so the caller keeps the honest omitted decline.
#[allow(clippy::too_many_arguments)]
fn emit_tailrec_fixed_base_support_stack(
    ctx: &CodegenContext,
    spec_fn: &str,
    loop_fn: &str,
    combine_fn: &str,
    combine_op: crate::ast::BinOp,
    nat_type: &str,
    impl_dafny: &str,
    law_name_dafny: &str,
) -> Option<String> {
    if !matches!(combine_op, crate::ast::BinOp::Add | crate::ast::BinOp::Mul) {
        return None;
    }

    // Peano constructors (nullary base + unary self-recursive succ) and the
    // additive monoid fn the combine rests on.
    let bare = nat_type.rsplit('.').next().unwrap_or(nat_type);
    let td = ctx
        .type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
        .find(|td| crate::codegen::common::type_def_name(td) == bare)?;
    let crate::ast::TypeDef::Sum { variants, .. } = td else {
        return None;
    };
    let base = variants.iter().find(|v| v.fields.is_empty())?.name.clone();
    let succ = variants
        .iter()
        .find(|v| v.fields.len() == 1 && v.fields[0].trim() == bare)?
        .name
        .clone();
    let plus = match combine_op {
        crate::ast::BinOp::Mul => {
            let combine_fd =
                ctx.fn_def_by_name(combine_fn, ctx.active_module_scope().as_deref())?;
            aver_name_to_dafny(&peano_additive_callee(combine_fd)?)
        }
        _ => aver_name_to_dafny(combine_fn),
    };

    let t = bare.to_string();
    let spec = aver_name_to_dafny(spec_fn);
    let loop_d = aver_name_to_dafny(loop_fn);
    let zero = format!("{t}.{base}");
    let main = format!("{impl_dafny}_{law_name_dafny}");
    let p = format!("{main}__"); // law-scoped helper-lemma prefix
    let acc = format!("{loop_d}__acc");

    // `plus` monoid helper-lemma names (shared by both combine ops).
    let (pz, ps, pc, pa) = (
        format!("{p}plus_zero_r"),
        format!("{p}plus_succ_r"),
        format!("{p}plus_comm"),
        format!("{p}plus_assoc"),
    );

    let mut lines = Vec::new();
    lines.push(format!(
        "// Law: {spec_fn}.{law_name_dafny} — tail-recursive fixed-base fold support stack"
    ));
    // ── plus monoid (shared) ──
    lines.push(format!(
        "lemma {pz}(x: {t})\n  ensures {plus}(x, {zero}) == x\n  decreases x\n{{ match x case {base} => case {succ}(q) => {pz}(q); }}"
    ));
    lines.push(format!(
        "lemma {ps}(x: {t}, y: {t})\n  ensures {plus}(x, {t}.{succ}(y)) == {t}.{succ}({plus}(x, y))\n  decreases x\n{{ match x case {base} => case {succ}(q) => {ps}(q, y); }}"
    ));
    lines.push(format!(
        "lemma {pc}(a: {t}, b: {t})\n  ensures {plus}(a, b) == {plus}(b, a)\n  decreases a\n{{ match a case {base} => {pz}(b); case {succ}(q) => {pc}(q, b); {ps}(b, q); }}"
    ));
    lines.push(format!(
        "lemma {pa}(a: {t}, b: {t}, c: {t})\n  ensures {plus}({plus}(a, b), c) == {plus}(a, {plus}(b, c))\n  decreases a\n{{ match a case {base} => case {succ}(q) => {pa}(q, b, c); }}"
    ));

    let one = format!("{t}.{succ}({t}.{base})");
    match combine_op {
        crate::ast::BinOp::Mul => {
            // ── mul monoid (built on the plus monoid) ──
            let mul = aver_name_to_dafny(combine_fn);
            let psw = format!("{p}plus_swap");
            let (om, mo, ma, mpd, mc, mz, msr) = (
                format!("{p}one_mul"),
                format!("{p}mul_one"),
                format!("{p}mul_assoc"),
                format!("{p}mul_plus_dist"),
                format!("{p}mul_comm"),
                format!("{p}mul_zero_r"),
                format!("{p}mul_succ_r"),
            );
            lines.push(format!(
                "lemma {psw}(a: {t}, b: {t}, c: {t})\n  ensures {plus}(a, {plus}(b, c)) == {plus}(b, {plus}(a, c))\n{{ {pa}(a, b, c); {pc}(a, b); {pa}(b, a, c); }}"
            ));
            lines.push(format!(
                "lemma {mz}(b: {t})\n  ensures {mul}(b, {zero}) == {zero}\n  decreases b\n{{ match b case {base} => case {succ}(w) => {mz}(w); }}"
            ));
            lines.push(format!(
                "lemma {om}(x: {t})\n  ensures {mul}({one}, x) == x\n{{ {pz}(x); }}"
            ));
            lines.push(format!(
                "lemma {mo}(a: {t})\n  ensures {mul}(a, {one}) == a\n  decreases a\n{{ match a case {base} => case {succ}(z) => {mo}(z); assert {plus}({one}, z) == {t}.{succ}(z); }}"
            ));
            lines.push(format!(
                "lemma {msr}(b: {t}, z: {t})\n  ensures {mul}(b, {t}.{succ}(z)) == {plus}(b, {mul}(b, z))\n  decreases b\n{{ match b case {base} => case {succ}(w) => {msr}(w, z); {psw}(z, w, {mul}(w, z)); }}"
            ));
            lines.push(format!(
                "lemma {mpd}(a: {t}, b: {t}, c: {t})\n  ensures {mul}({plus}(a, b), c) == {plus}({mul}(a, c), {mul}(b, c))\n  decreases a\n{{ match a case {base} => case {succ}(z) => {mpd}(z, b, c); {pa}(c, {mul}(z, c), {mul}(b, c)); }}"
            ));
            lines.push(format!(
                "lemma {ma}(a: {t}, b: {t}, c: {t})\n  ensures {mul}({mul}(a, b), c) == {mul}(a, {mul}(b, c))\n  decreases a\n{{ match a case {base} => case {succ}(z) => {ma}(z, b, c); {mpd}(b, {mul}(z, b), c); }}"
            ));
            lines.push(format!(
                "lemma {mc}(a: {t}, b: {t})\n  ensures {mul}(a, b) == {mul}(b, a)\n  decreases a\n{{ match a case {base} => {mz}(b); case {succ}(z) => {mc}(z, b); {msr}(b, z); }}"
            ));
            // ── accumulator decomposition + main law (multiplicative) ──
            // `x` is fixed through the induction on `n`; the combine is
            // `mul(x, acc)` (fixed base × accumulator).
            lines.push(format!(
                "lemma {acc}(x: {t}, n: {t}, a: {t})\n  ensures {loop_d}(x, n, a) == {mul}({loop_d}(x, n, {one}), a)\n  decreases n\n{{ match n case {base} => {om}(a); case {succ}(m) => {acc}(x, m, {mul}(x, a)); {acc}(x, m, {mul}(x, {one})); {mo}(x); {ma}({loop_d}(x, m, {one}), x, a); }}"
            ));
            lines.push(format!(
                "lemma {{:fuel {spec}, 5}} {{:fuel {loop_d}, 5}} {main}(x: {t}, n: {t})\n  ensures {spec}(x, n) == {loop_d}(x, n, {one})\n  decreases n\n{{ match n case {base} => case {succ}(m) => {main}(x, m); {acc}(x, m, {mul}(x, {one})); {mo}(x); {mc}({spec}(x, m), x); }}\n"
            ));
        }
        _ => {
            // ── accumulator decomposition + main law (additive) ──
            // No `mul` monoid: the close rests on `plus` associativity and
            // commutativity, neutral is `zero`. Combine is `plus(x, acc)`.
            let plus_c = aver_name_to_dafny(combine_fn);
            lines.push(format!(
                "lemma {acc}(x: {t}, n: {t}, a: {t})\n  ensures {loop_d}(x, n, a) == {plus_c}({loop_d}(x, n, {zero}), a)\n  decreases n\n{{ match n case {base} => {pc}({zero}, a); {pz}(a); case {succ}(m) => {acc}(x, m, {plus_c}(x, a)); {acc}(x, m, {plus_c}(x, {zero})); {pz}(x); {pa}({loop_d}(x, m, {zero}), x, a); }}"
            ));
            lines.push(format!(
                "lemma {{:fuel {spec}, 5}} {{:fuel {loop_d}, 5}} {main}(x: {t}, n: {t})\n  ensures {spec}(x, n) == {loop_d}(x, n, {zero})\n  decreases n\n{{ match n case {base} => case {succ}(m) => {main}(x, m); {acc}(x, m, {plus_c}(x, {zero})); {pz}(x); {pc}({spec}(x, m), x); }}\n"
            ));
        }
    }
    Some(lines.join("\n"))
}

/// True when the law's call cone (lhs + rhs, transitively expanded
/// through fn bodies) reaches a fn carrying the guard-validated
/// floor-division countdown contract
/// (`RecursionContract::WellFoundedToNat { floor_div: Some(_) }`).
/// Such a fn verifies its own termination natively, but a DEFAULT
/// empty-body universal lemma over it still hands Z3 an unbounded
/// symbolic unfolding — a guaranteed error or timeout on a law that
/// may well hold — so `emit_verify_law` keeps the honest
/// omitted-universal decline unless the law carries a
/// `FloorDivWindow` strategy (whose support stack proves it).
pub(super) fn law_reaches_floor_div_fn(law: &VerifyLaw, ctx: &CodegenContext) -> bool {
    let mut cone = std::collections::BTreeSet::new();
    crate::codegen::proof_recognize::collect_called_fns(&law.lhs, &mut cone);
    crate::codegen::proof_recognize::collect_called_fns(&law.rhs, &mut cone);
    let mut changed = true;
    while changed {
        changed = false;
        let snapshot: Vec<String> = cone.iter().cloned().collect();
        for name in snapshot {
            if let Some(fd) = ctx.fn_def_by_name(&name, ctx.active_module_scope().as_deref()) {
                let before = cone.len();
                crate::codegen::proof_recognize::collect_called_fns_in_body(&fd.body, &mut cone);
                if cone.len() != before {
                    changed = true;
                }
            }
        }
    }
    cone.iter().any(|name| {
        crate::codegen::common::fn_id_for_dotted_name(ctx, name)
            .and_then(|id| ctx.proof_ir.fn_contracts.get(&id))
            .is_some_and(|contract| {
                matches!(
                    &contract.recursion,
                    Some(crate::ir::RecursionContract::WellFoundedToNat {
                        floor_div: Some(_),
                        ..
                    })
                )
            })
    })
}

/// Render the `FloorDivWindow` support stack + main lemma for one
/// pinned figure. The lemma text was validated end-to-end on the
/// emitted artifact (`dafny verify`: everything PROVED, no `assume`,
/// no `{:axiom}`):
///
/// - `PowPositive` / `PowSumSplit`: the lemma's own one-line
///   self-call induction;
/// - `SigWindow`: the division-window prelude (`div_lower` /
///   `div_upper` / `div_window` from the Euclidean identity +
///   multiplication monotonicity), the power algebra (`pow_pos`
///   auto-induction, `pow_add` self-call induction), the
///   binary-exponent window characterization (self-call on the
///   halving + two literal-div hints), and per-branch significand
///   lemmas that take the window as `requires` — the branch split is
///   what keeps each VC small enough to verify in seconds (the
///   monolithic form times out even fully hinted);
/// - `ProductWindow`: power algebra + the four monotonicity
///   instantiations + two ring-identity hint asserts.
///
/// Support lemma names are prefixed `<fn>_<law>__` so two figures in
/// one file never collide.
fn emit_floor_window_support_stack(
    figure: &crate::ir::FloorWindowFigure,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    fn_name: &str,
    law_name: &str,
) -> String {
    use crate::ir::FloorWindowFigure;
    let d = aver_name_to_dafny;
    let render = |e: &Spanned<Expr>| emit_expr_legacy(e, ctx, None);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let when = law.when.as_ref().map(render).unwrap_or_default();
    let givens: Vec<String> = law.givens.iter().map(|g| d(&g.name)).collect();
    let params: Vec<String> = givens.iter().map(|g| format!("{}: int", g)).collect();
    let params = params.join(", ");

    // Fuel attrs over the law's (transitive) fn cone — same shape as
    // the default universal lemma so unfolding behaves identically.
    let mut law_fns = std::collections::BTreeSet::new();
    crate::codegen::proof_recognize::collect_called_fns(&law.lhs, &mut law_fns);
    crate::codegen::proof_recognize::collect_called_fns(&law.rhs, &mut law_fns);
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
    let fuel_attrs: String = law_fns
        .iter()
        .filter(|f| {
            ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref())
                .is_some()
        })
        .map(|f| format!("{{:fuel {}, 5}}", aver_name_to_dafny(f)))
        .collect::<Vec<_>>()
        .join(" ");
    let main_thm = format!("{}_{}", fn_name, law_name);
    let u = format!("{}__", main_thm);

    match figure {
        FloorWindowFigure::PowPositive { pow_fn } => {
            let g0 = &givens[0];
            format!(
                "// Law: {fn_name}.{law_name} — power-of-two positivity ({pow})\nlemma {fuel_attrs} {main_thm}({params})\n  ensures {lhs} == {rhs}\n  decreases if {g0} >= 0 then {g0} else 0\n{{\n  if {g0} > 0 {{\n    {main_thm}({g0} - 1);\n  }}\n}}\n",
                pow = d(pow_fn),
            )
        }
        FloorWindowFigure::PowSumSplit { pow_fn } => {
            let (g0, g1) = (&givens[0], &givens[1]);
            format!(
                "// Law: {fn_name}.{law_name} — power-of-two sum homomorphism ({pow})\nlemma {fuel_attrs} {main_thm}({params})\n  requires {when}\n  ensures {lhs} == {rhs}\n  decreases {g0}\n{{\n  if {g0} > 0 {{\n    {main_thm}({g0} - 1, {g1});\n  }}\n}}\n",
                pow = d(pow_fn),
            )
        }
        FloorWindowFigure::SigWindow {
            pow_fn,
            halve_fn,
            exp_fn,
            sig_fn,
            ..
        } => {
            let (ga, gb, gn) = (&givens[0], &givens[1], &givens[2]);
            let pow = d(pow_fn);
            let halve = d(halve_fn);
            let exp = d(exp_fn);
            let sig = d(sig_fn);
            let mut s = String::new();
            s.push_str(&format!(
                "// Law: {fn_name}.{law_name} — floor-division window support stack\n"
            ));
            s.push_str(&format!(
                "lemma {u}mul_mono(p: int, q: int, d: int)\n  requires p <= q && d >= 0\n  ensures p * d <= q * d\n{{ }}\n"
            ));
            s.push_str(&format!(
                "lemma {u}mul_mono_left(p: int, q: int, c: int)\n  requires p <= q && c >= 0\n  ensures c * p <= c * q\n{{ }}\n"
            ));
            s.push_str(&format!(
                "lemma {{:vcs_split_on_every_assert}} {u}div_lower(x: int, d: int, k: int)\n  requires d >= 1 && k * d <= x\n  ensures k <= x / d\n{{\n  assert x == d * (x / d) + x % d;\n  if k > x / d {{\n    {u}mul_mono(k, x / d + 1, d);\n    assert false;\n  }}\n}}\n"
            ));
            s.push_str(&format!(
                "lemma {{:vcs_split_on_every_assert}} {u}div_upper(x: int, d: int, k: int)\n  requires d >= 1 && x < k * d\n  ensures x / d < k\n{{\n  assert x == d * (x / d) + x % d;\n  if x / d >= k {{\n    {u}mul_mono(k, x / d, d);\n    assert false;\n  }}\n}}\n"
            ));
            s.push_str(&format!(
                "lemma {u}div_window(x: int, d: int, lo: int, hi: int)\n  requires d >= 1 && lo * d <= x && x < hi * d\n  ensures lo <= x / d < hi\n{{\n  {u}div_lower(x, d, lo);\n  {u}div_upper(x, d, hi);\n}}\n"
            ));
            s.push_str(&format!(
                "lemma {u}pow_pos(n: int)\n  ensures {pow}(n) >= 1\n{{ }}\n"
            ));
            s.push_str(&format!(
                "lemma {{:vcs_split_on_every_assert}} {u}pow_add(m: int, n: int)\n  requires m >= 0 && n >= 0\n  ensures {pow}(m + n) == {pow}(m) * {pow}(n)\n{{\n  if m > 0 {{\n    {u}pow_add(m - 1, n);\n  }}\n}}\n"
            ));
            s.push_str(&format!(
                "lemma {u}exp_nonneg(a: int, b: int)\n  ensures {exp}(a, b) >= 0\n  decreases if a >= 0 then a else 0\n{{ }}\n"
            ));
            s.push_str(&format!(
                "lemma {{:vcs_split_on_every_assert}} {u}exp_window(a: int, b: int)\n  requires b >= 1 && a >= b\n  ensures {pow}({exp}(a, b)) * b <= a < {pow}({exp}(a, b) + 1) * b\n  decreases a\n{{\n  if a >= 2 * b {{\n    {u}exp_window({halve}(a), b);\n    {u}exp_nonneg({halve}(a), b);\n    assert 2 * (a / 2) <= a;\n    assert a <= 2 * (a / 2) + 1;\n  }}\n}}\n"
            ));
            // `{:vcs_split_on_every_assert}` on the two branch lemmas
            // is measured-necessary: their hint chains mix the
            // nonlinear pow products with the division window, and the
            // monolithic VC can time out (deterministically on some
            // declaration orders) while the per-assert split verifies
            // in seconds on every run.
            s.push_str(&format!(
                "lemma {{:vcs_split_on_every_assert}} {u}sig_pos(a: int, b: int, n: int, e: int, s: int)\n  requires b >= 1 && n >= 1 && e >= 0 && s == n - 1 - e && s >= 0\n  requires {pow}(e) * b <= a && a < {pow}(e + 1) * b\n  ensures {pow}(n - 1) <= (a * {pow}(s)) / b < {pow}(n)\n{{\n  {u}pow_pos(s);\n  {u}pow_add(e, s);\n  assert {pow}(n - 1) == {pow}(e) * {pow}(s);\n  {u}pow_add(e + 1, s);\n  assert {pow}(n) == {pow}(e + 1) * {pow}(s);\n  {u}mul_mono({pow}(e) * b, a, {pow}(s));\n  assert {pow}(e) * b * {pow}(s) == ({pow}(e) * {pow}(s)) * b;\n  assert {pow}(n - 1) * b <= a * {pow}(s);\n  {u}mul_mono(a, {pow}(e + 1) * b, {pow}(s));\n  assert {pow}(e + 1) * b * {pow}(s) == ({pow}(e + 1) * {pow}(s)) * b;\n  assert a * {pow}(s) < {pow}(n) * b;\n  {u}div_window(a * {pow}(s), b, {pow}(n - 1), {pow}(n));\n}}\n"
            ));
            s.push_str(&format!(
                "lemma {{:vcs_split_on_every_assert}} {u}sig_neg(a: int, b: int, n: int, e: int, s: int)\n  requires b >= 1 && n >= 1 && e >= 0 && s == n - 1 - e && s < 0\n  requires {pow}(0 - s) >= 1\n  requires {pow}(e) * b <= a && a < {pow}(e + 1) * b\n  ensures {pow}(n - 1) <= a / (b * {pow}(0 - s)) < {pow}(n)\n{{\n  {u}pow_pos(0 - s);\n  {u}pow_add(n - 1, 0 - s);\n  assert {pow}(e) == {pow}(n - 1) * {pow}(0 - s);\n  {u}pow_add(n, 0 - s);\n  assert {pow}(e + 1) == {pow}(n) * {pow}(0 - s);\n  assert {pow}(n - 1) * (b * {pow}(0 - s)) == ({pow}(n - 1) * {pow}(0 - s)) * b;\n  assert {pow}(n) * (b * {pow}(0 - s)) == ({pow}(n) * {pow}(0 - s)) * b;\n  {u}div_window(a, b * {pow}(0 - s), {pow}(n - 1), {pow}(n));\n}}\n"
            ));
            s.push_str(&format!(
                "// Law: {fn_name}.{law_name}\nlemma {{:vcs_split_on_every_assert}} {fuel_attrs} {main_thm}({params})\n  requires {when}\n  ensures {lhs} == {rhs}\n{{\n  {u}exp_window({ga}, {gb});\n  {u}exp_nonneg({ga}, {gb});\n  var e := {exp}({ga}, {gb});\n  var s := {gn} - 1 - e;\n  if s >= 0 {{\n    {u}sig_pos({ga}, {gb}, {gn}, e, s);\n  }} else {{\n    {u}pow_pos(0 - s);\n    {u}sig_neg({ga}, {gb}, {gn}, e, s);\n  }}\n  assert {pow}({gn} - 1) <= {sig}({ga}, {gb}, {gn}) < {pow}({gn});\n}}\n"
            ));
            s
        }
        FloorWindowFigure::ProductWindow { pow_fn, .. } => {
            let (gj, gk, gm, gn) = (&givens[0], &givens[1], &givens[2], &givens[3]);
            let pow = d(pow_fn);
            let mut s = String::new();
            s.push_str(&format!(
                "// Law: {fn_name}.{law_name} — power-of-two window product support stack\n"
            ));
            s.push_str(&format!(
                "lemma {u}mul_mono(p: int, q: int, d: int)\n  requires p <= q && d >= 0\n  ensures p * d <= q * d\n{{ }}\n"
            ));
            s.push_str(&format!(
                "lemma {u}mul_mono_left(p: int, q: int, c: int)\n  requires p <= q && c >= 0\n  ensures c * p <= c * q\n{{ }}\n"
            ));
            s.push_str(&format!(
                "lemma {u}pow_pos(n: int)\n  ensures {pow}(n) >= 1\n{{ }}\n"
            ));
            s.push_str(&format!(
                "lemma {{:vcs_split_on_every_assert}} {u}pow_add(m: int, n: int)\n  requires m >= 0 && n >= 0\n  ensures {pow}(m + n) == {pow}(m) * {pow}(n)\n{{\n  if m > 0 {{\n    {u}pow_add(m - 1, n);\n  }}\n}}\n"
            ));
            s.push_str(&format!(
                "// Law: {fn_name}.{law_name}\nlemma {{:vcs_split_on_every_assert}} {fuel_attrs} {main_thm}({params})\n  requires {when}\n  ensures {lhs} == {rhs}\n{{\n  {u}pow_pos({gm} - 1);\n  {u}pow_pos({gn} - 1);\n  if {gm} <= 0 {{\n    assert {pow}({gm}) == 1;\n    assert false;\n  }}\n  if {gn} <= 0 {{\n    assert {pow}({gn}) == 1;\n    assert false;\n  }}\n  {u}pow_add({gm} - 1, {gn} - 1);\n  assert {pow}({gm} + {gn} - 2) == {pow}({gm} - 1) * {pow}({gn} - 1);\n  {u}pow_add({gm}, {gn});\n  assert {pow}({gm} + {gn}) == {pow}({gm}) * {pow}({gn});\n  {u}mul_mono({pow}({gm} - 1), {gj}, {pow}({gn} - 1));\n  {u}mul_mono_left({pow}({gn} - 1), {gk}, {gj});\n  assert {gj} * {gk} + {gk} == ({gj} + 1) * {gk};\n  {u}mul_mono({gj} + 1, {pow}({gm}), {gk});\n  assert {pow}({gm}) * {gk} + {pow}({gm}) == {pow}({gm}) * ({gk} + 1);\n  {u}mul_mono_left({gk} + 1, {pow}({gn}), {pow}({gm}));\n}}\n"
            ));
            s
        }
        FloorWindowFigure::FloorPow2Window { pow_fn, .. } => {
            // The Euclidean floor window over a power-of-two divisor. Z3
            // carries integer division push-button once it knows the divisor
            // is positive; the only hint needed is `pow(E) >= 1` at the
            // exponent the predicate divides by. `pow_pos` is auto-inducted
            // (empty body); the exponent expression is read off the window
            // predicate body so the hint instantiates the right divisor.
            let pow = d(pow_fn);
            let e_hint = floor_pow2_window_exponent_dafny(fn_name, ctx)
                .map(|e| format!("\n  {u}pow_pos({e});"))
                .unwrap_or_default();
            format!(
                "// Law: {fn_name}.{law_name} — recursive-expo-free floor window over a power-of-two divisor\nlemma {u}pow_pos(n: int)\n  ensures {pow}(n) >= 1\n{{ }}\nlemma {fuel_attrs} {main_thm}({params})\n  ensures {lhs} == {rhs}\n{{{e_hint}\n}}\n",
            )
        }
        FloorWindowFigure::FloorPow2Cancel { pow_fn, .. } => {
            // The exact-division cancel `floor(s·pow(b), pow(a)) = s·pow(b−a)`
            // for `0 <= a <= b`. Z3 carries the division once it knows the
            // dividend is the divisor times the quotient: the homomorphism
            // `pow(b) = pow(a)·pow(b−a)` (auto-inducted `pow_add`) makes
            // `s·pow(b) = pow(a)·(s·pow(b−a))`, and `pow_pos` gives the divisor
            // positive, so the floor returns the exact quotient.
            let pow = d(pow_fn);
            let (_s, a, b) = (&givens[0], &givens[1], &givens[2]);
            format!(
                "// Law: {fn_name}.{law_name} — exact-division cancel over a power-of-two divisor\nlemma {u}pow_pos(n: int)\n  ensures {pow}(n) >= 1\n{{ }}\nlemma {{:vcs_split_on_every_assert}} {u}pow_add(m: int, n: int)\n  requires m >= 0 && n >= 0\n  ensures {pow}(m + n) == {pow}(m) * {pow}(n)\n{{\n  if m > 0 {{\n    {u}pow_add(m - 1, n);\n  }}\n}}\nlemma {fuel_attrs} {main_thm}({params})\n  requires {when}\n  ensures {lhs} == {rhs}\n{{\n  {u}pow_pos({a});\n  {u}pow_add({a}, {b} - {a});\n  assert {pow}({b}) == {pow}({a}) * {pow}({b} - {a});\n}}\n",
            )
        }
    }
}

/// Read the exponent expression `E` the floor window's divisor `pow(E)`
/// uses, off the window predicate body, rendered to Dafny — so the
/// generated lemma can instantiate `pow_pos(E)` at the right divisor.
/// Returns `None` (no hint) if the body does not match the expected shape.
fn floor_pow2_window_exponent_dafny(window_fn: &str, ctx: &CodegenContext) -> Option<String> {
    let fd = ctx.fn_def_by_name(window_fn, ctx.active_module_scope().as_deref())?;
    let body = fd.body.tail_expr()?;
    // body = Bool.and(pow(E) * floor(...) <= N, ...)
    let Expr::FnCall(_, args) = &body.node else {
        return None;
    };
    let lo = args.first()?;
    let Expr::BinOp(BinOp::Lte, lo_l, _) = &lo.node else {
        return None;
    };
    let Expr::BinOp(BinOp::Mul, pow_call, _) = &lo_l.node else {
        return None;
    };
    let Expr::FnCall(_, pow_args) = &pow_call.node else {
        return None;
    };
    let e = pow_args.first()?;
    Some(emit_expr_legacy(e, ctx, None))
}

/// Render a computed instantiation argument (from `cite_instantiate`) to Dafny:
/// the induction placeholders map to the concrete head/tail (`xs[0]` / `xs[1..]`),
/// `List.concat` to seq `+`.
fn render_dafny_arg(e: &Spanned<Expr>, list_param: &str) -> String {
    use crate::codegen::cite_instantiate::{HEAD, TAIL, ident_name};
    if let Some(n) = ident_name(e) {
        return match n {
            HEAD => format!("{list_param}[0]"),
            TAIL => format!("{list_param}[1..]"),
            _ => aver_name_to_dafny(n),
        };
    }
    match &e.node {
        Expr::Literal(lit) => super::expr::emit_literal(lit),
        Expr::List(items) => format!(
            "[{}]",
            items
                .iter()
                .map(|i| render_dafny_arg(i, list_param))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Expr::FnCall(callee, args) => {
            let name =
                crate::codegen::common::expr_to_dotted_name(&callee.node).unwrap_or_default();
            let rendered: Vec<String> = args
                .iter()
                .map(|a| render_dafny_arg(a, list_param))
                .collect();
            if name == "List.concat" && rendered.len() == 2 {
                format!("({} + {})", rendered[0], rendered[1])
            } else {
                format!("{}({})", aver_name_to_dafny(&name), rendered.join(", "))
            }
        }
        Expr::Constructor(name, None) => aver_name_to_dafny(name),
        Expr::Constructor(name, Some(inner)) => {
            format!(
                "{}({})",
                aver_name_to_dafny(name),
                render_dafny_arg(inner, list_param)
            )
        }
        Expr::Attr(inner, field) => format!("{}.{}", render_dafny_arg(inner, list_param), field),
        _ => String::new(),
    }
}

/// Decomposition citation pool — the Dafny analog of the Lean `earlier_law_lemmas`
/// simp pool. For THIS law, find earlier sibling laws in the same file whose
/// universal `ensures` can drive the goal, and emit a `forall`-instantiation
/// block that CALLS each one. Z3 does not auto-apply lemmas, so a decomposed
/// proof whose helper laws are merely emitted (not invoked) fails — each
/// inductive arm closes only once its dependency facts are in context.
///
/// Eligibility mirrors the Lean pool: the shared [`LawProofCone`] ∪ subject
/// gate, restricted to strictly-EARLIER (source order = emit order, so cyclic
/// citation is impossible), universal-form (non-opaque / non-native-mutual —
/// those carry a sampled `requires` so their `ensures` is not a true ∀),
/// unconditional (no `when`) siblings with plain givens (no oracle / refined
/// types the `forall` cannot quantify). Sound by construction: Dafny re-proves
/// every cited lemma and the `forall` body must discharge the asserted fact, so
/// a wrong or sample-only citation fails closed — never a false proof.
/// Earlier sibling laws eligible to be cited into THIS law's proof, shared by the
/// `forall`-citation hoist and the explicit-instantiation engine ([`super::cite_instantiate`]).
///
/// Eligibility mirrors the Lean pool: the shared [`LawProofCone`] ∪ subject gate,
/// restricted to strictly-EARLIER (source order = emit order, so cyclic citation
/// is impossible), universal-form (non-opaque / non-native-mutual — those carry a
/// sampled `requires` so their `ensures` is not a true ∀), unconditional (no
/// `when`) siblings with plain givens (no oracle / refined types). Sound by
/// construction: Dafny re-proves every cited lemma, so a wrong citation fails
/// closed — never a false proof.
fn eligible_cites<'a>(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
    opaque_fns: &std::collections::HashSet<crate::ir::FnId>,
    native_emitted: &std::collections::HashSet<crate::ir::FnId>,
) -> Vec<(String, &'a VerifyLaw)> {
    use std::collections::BTreeSet;

    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let mut scope: BTreeSet<String> = cone.pure_fns().iter().map(|fd| fd.name.clone()).collect();
    scope.insert(vb.fn_name.clone());
    let subject = vb.fn_name.clone();
    let mod_scope = ctx.active_module_scope();

    // Program (user-defined) fns called inside a law side — the cone is
    // expressed in these names, so builtins (`List.concat`, …) are filtered out.
    let program_fns = |expr: &Spanned<Expr>| -> BTreeSet<String> {
        let mut s = BTreeSet::new();
        crate::codegen::proof_recognize::collect_called_fns(expr, &mut s);
        s.into_iter()
            .filter(|n| ctx.fn_def_by_name(n, mod_scope.as_deref()).is_some())
            .collect()
    };

    let mut out = Vec::new();
    for item in &ctx.items {
        let TopLevel::Verify(prev) = item else {
            continue;
        };
        // Only blocks earlier in source are eligible; stop at the consumer.
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        // Unconditional only: a `when` law's universal is `P -> lhs == rhs`.
        if prev_law.when.is_some() {
            continue;
        }
        // Universal-form only: opaque / native-mutual siblings are emitted with
        // a sampled `requires` (or `assume {:axiom}`), so calling them over the
        // whole quantified domain violates their precondition.
        if law_refs_opaque_fn(&prev_law.lhs, ctx, opaque_fns)
            || law_refs_opaque_fn(&prev_law.rhs, ctx, opaque_fns)
            || law_refs_opaque_fn(&prev_law.lhs, ctx, native_emitted)
            || law_refs_opaque_fn(&prev_law.rhs, ctx, native_emitted)
        {
            continue;
        }
        // Plain givens only: an oracle / refinement-lifted given does not map to
        // a bare Dafny binder the `forall` can quantify over.
        if prev_law.givens.iter().any(|g| {
            crate::types::checker::effect_classification::oracle_signature(&g.type_name).is_some()
                || crate::codegen::common::refinement_lift_for_given(
                    &g.name,
                    &g.type_name,
                    &prev_law.lhs,
                    &prev_law.rhs,
                    ctx,
                )
                .is_some()
        }) {
            continue;
        }
        // Cone eligibility — the three Lean admission gates, in Aver-name space:
        // (1) the sibling stays inside this law's cone ∪ subject; (2) it mentions
        // the consumer's SUBJECT and its own subject is in scope (a decomposition
        // that introduces a combinator); (3) its LHS is cone-rooted and its own
        // subject is in scope. Loop safety rides on the strict source ordering.
        let mut mentions = program_fns(&prev_law.lhs);
        mentions.extend(program_fns(&prev_law.rhs));
        if mentions.is_empty() {
            continue;
        }
        let lhs_mentions = program_fns(&prev_law.lhs);
        let prev_subject = prev.fn_name.clone();
        let lhs_rooted = !lhs_mentions.is_empty()
            && lhs_mentions.is_subset(&scope)
            && scope.contains(&prev_subject);
        let eligible = mentions.is_subset(&scope)
            || (mentions.contains(&subject) && scope.contains(&prev_subject))
            || lhs_rooted;
        if !eligible {
            continue;
        }
        let dafny_name = format!(
            "{}_{}",
            aver_name_to_dafny(&prev.fn_name),
            aver_name_to_dafny(&prev_law.name)
        );
        out.push((dafny_name, prev_law.as_ref()));
    }
    out
}

/// Hoist each eligible earlier law as a `forall`-fact at the body top. Z3 does not
/// auto-apply lemmas, so a decomposed proof whose helper laws are merely emitted
/// (not invoked) fails — this makes each one available universally.
fn earlier_law_citations(cites: &[(String, &VerifyLaw)], ctx: &CodegenContext) -> Vec<String> {
    let mut out = Vec::new();
    for (dafny_name, prev_law) in cites {
        let plhs = emit_expr_legacy(&prev_law.lhs, ctx, None);
        let prhs = emit_expr_legacy(&prev_law.rhs, ctx, None);
        // Skip a sibling with an unused given (a binder absent from its own
        // statement): the `forall` would quantify a variable with no trigger,
        // which Dafny rejects under `--allow-warnings false`. Such a law is also
        // weak as a rewrite, so dropping it only reverts to the un-cited proof.
        let words: std::collections::BTreeSet<&str> = plhs
            .split(|c: char| !c.is_alphanumeric() && c != '_')
            .chain(prhs.split(|c: char| !c.is_alphanumeric() && c != '_'))
            .collect();
        if prev_law
            .givens
            .iter()
            .any(|g| !words.contains(aver_name_to_dafny(&g.name).as_str()))
        {
            continue;
        }
        if prev_law.givens.is_empty() {
            out.push(format!("  {}();", dafny_name));
        } else {
            let binders = prev_law
                .givens
                .iter()
                .map(|g| {
                    format!(
                        "{}: {}",
                        aver_name_to_dafny(&g.name),
                        emit_type(&g.type_name)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            let args = prev_law
                .givens
                .iter()
                .map(|g| aver_name_to_dafny(&g.name))
                .collect::<Vec<_>>()
                .join(", ");
            out.push(format!(
                "  forall {binders} ensures {plhs} == {prhs} {{ {dafny_name}({args}); }}"
            ));
        }
    }
    out
}

pub fn emit_verify_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    opaque_fns: &std::collections::HashSet<crate::ir::FnId>,
    native_emitted: &std::collections::HashSet<crate::ir::FnId>,
    termination_opaque: &std::collections::HashSet<crate::ir::FnId>,
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

    // Laws whose cone reaches a fn emitted as an opaque `{:axiom}` by
    // the termination-decline path (recursion outside every recognized
    // `decreases` pattern — tests/fixtures/expo_outside_subset.av):
    // the default empty-body lemma states an `ensures` about a value
    // the verifier cannot unfold, a GUARANTEED error or timeout on a
    // law that may well hold. Report it as an omitted universal
    // instead — same honesty class as the fuel-bounded gate below,
    // and `--check` charges it to the unproven budget.
    if law_refs_opaque_fn(&law.lhs, ctx, termination_opaque)
        || law_refs_opaque_fn(&law.rhs, ctx, termination_opaque)
    {
        return format!(
            "// Law {}.{}{}: reaches a recursive fn outside the proof subset emitted as an opaque axiom (universal lemma omitted)",
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
                    // Same guard for the ring-identity strategy: its
                    // AC-ring lemma package is Lean vocabulary; Z3
                    // already decides these nonlinear identities
                    // push-button on the default universal lemma, so
                    // Dafny treats the pin as `BackendDispatch` and
                    // its exports stay byte-identical.
                    | crate::ir::ProofStrategy::RingIdentity { .. }
                    // Same guard for the nonlinear-nonnegativity strategy:
                    // its closer is the Lean prelude tactic `aver_int_order`;
                    // Z3 carries these `E >= 0` bounds push-button on the
                    // default universal lemma, so Dafny treats the pin as
                    // `BackendDispatch` and its exports stay byte-identical.
                    | crate::ir::ProofStrategy::NonlinearNonneg { .. }
                    // Same guard for the decimal-Int roundtrip strategy:
                    // Lean-only (its skeleton cites the synthesized
                    // `__fuel_scan` lemma and Lean prelude names), so
                    // Dafny treats the pin as `BackendDispatch` and its
                    // exports stay byte-identical.
                    | crate::ir::ProofStrategy::IntDecimalRoundtrip { .. }
                    // Same guard for the escaped-string roundtrip
                    // strategy: Lean-only (its suffix-invariant
                    // skeleton cites `__fuel` step lemmas and Lean
                    // prelude names), so Dafny treats the pin as
                    // `BackendDispatch` and its exports stay
                    // byte-identical.
                    | crate::ir::ProofStrategy::StringEscapeRoundtrip(_)
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
    // TIP prop_35 — `TailRecFixedBaseFold` (the `qexp` shape). The loop is
    // rejected by the recursion classifier (a growing accumulator) so it lands
    // in `unclassified_fns` and would trip the fuel gate below, but the strategy
    // supplies its own complete Peano-monoid support stack that PROVES the
    // universal. Emit it before the fuel gate (mirror of the Lean fuel-gate
    // exception). Falls through only if the stack declines the shape.
    if let Some(crate::ir::ProofStrategy::TailRecFixedBaseFold {
        spec_fn,
        loop_fn,
        combine_fn,
        combine_op,
        type_name,
    }) = vb_fn_id
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .map(|t| t.strategy.clone())
        && let Some(stack) = emit_tailrec_fixed_base_support_stack(
            ctx,
            &spec_fn,
            &loop_fn,
            &combine_fn,
            combine_op,
            &type_name,
            &fn_name,
            &law_name,
        )
    {
        return stack;
    }

    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    let calls_fuel_bounded = crate::codegen::common::law_calls_unclassified_fn(law, &unclassified);
    // An accumulator-fold reference that can't close here: a FOREIGN fold (`fac(x)
    // => qfac(x, 1)` needs `qfac`'s own decomposition lemma), or a Nat self-fold
    // whose combine fn has no commutativity/associativity helper laws (Z3 can't
    // derive ADT algebra). A self-fold WITH those helpers attempts and closes via
    // the datatype-induction hint. The wrapper-over-recursion and tail-rec-fixed-
    // base strategies have already returned their own support stack above, so they
    // never reach this gate.
    let bound_acc_fold =
        crate::codegen::common::dafny_should_bound_accumulator_fold(ctx, law, &vb.fn_name);
    if singleton_const_rhs || calls_fuel_bounded || bound_acc_fold {
        let reason = if singleton_const_rhs {
            "singleton-domain givens with constant RHS"
        } else if calls_fuel_bounded {
            "calls a fuel-bounded fn outside the proof subset"
        } else {
            "references an accumulator-fold fn with no Dafny decomposition lemma"
        };
        return format!(
            "// Law {}.{}{}: {}, sample-only (universal lemma omitted)",
            fn_name, law_name, suffix, reason,
        );
    }

    // Floor-division window family. A law whose cone reaches a
    // guard-validated floor-division countdown fn either carries a
    // recognized `FloorDivWindow` figure — then its validated support
    // stack (division-window prelude + power algebra + branch-split
    // helper lemmas, all PROVED in the emitted file) closes the
    // universal — or it stays an honestly omitted universal: the fn
    // is in the proof subset now, but Z3 cannot close an arbitrary
    // universal over its unbounded symbolic unfolding, and the
    // default empty-body lemma would manufacture a guaranteed error
    // on a law that may well hold.
    let pinned_floor_window_figure = vb_fn_id
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .and_then(|t| match &t.strategy {
            crate::ir::ProofStrategy::FloorDivWindow { figure } => Some(figure.clone()),
            _ => None,
        });
    if let Some(figure) = pinned_floor_window_figure {
        return emit_floor_window_support_stack(&figure, law, ctx, &fn_name, &law_name);
    }
    // The omission applies only where the default path would state an
    // OPEN universal with an empty body. The bounded-∀ form (mutual /
    // opaque cone over all-literal-Int given domains — the same
    // predicates the default path evaluates below) dispatches to
    // per-sample lemmas instead and keeps working exactly as it did
    // before this family existed, so it is excluded here.
    let bounded_form_applies = {
        let is_opaque_cone = law_refs_opaque_fn(&law.lhs, ctx, opaque_fns)
            || law_refs_opaque_fn(&law.rhs, ctx, opaque_fns)
            || law_refs_opaque_fn(&law.lhs, ctx, native_emitted)
            || law_refs_opaque_fn(&law.rhs, ctx, native_emitted);
        let all_literal_int_domains = !law.givens.is_empty()
            && law.givens.iter().all(|g| {
                g.type_name == "Int"
                    && matches!(
                        &g.domain,
                        VerifyGivenDomain::Explicit(vs)
                            if vs.iter().all(|v| literal_int_value(v).is_some())
                    )
            });
        is_opaque_cone && all_literal_int_domains
    };
    if !bounded_form_applies && law_reaches_floor_div_fn(law, ctx) {
        return format!(
            "// Law {}.{}{}: reaches a floor-division recursion whose universal Z3 cannot close push-button, sample-only (universal lemma omitted)",
            fn_name, law_name, suffix,
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
        driver,
        combine_fn,
    }) = vb_fn_id
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .map(|t| t.strategy.clone())
    {
        match driver {
            crate::ir::WrapperDriver::List => {
                return emit_wrapper_over_recursion_support_stack(
                    &wrapper_fn,
                    &inner_fn,
                    &other_fn,
                    combine_op,
                    &fn_name,
                    &law_name,
                );
            }
            crate::ir::WrapperDriver::PeanoNat {
                type_name,
                value_first,
            } => {
                if let Some(combine) = combine_fn.as_deref()
                    && let Some(stack) = emit_wrapper_nat_support_stack(
                        ctx,
                        &wrapper_fn,
                        &inner_fn,
                        &other_fn,
                        combine,
                        combine_op,
                        &type_name,
                        value_first,
                        &fn_name,
                        &law_name,
                    )
                {
                    return stack;
                }
                // Fall through to the default decline when the Peano-Nat
                // shape isn't one the support stack handles.
            }
        }
    }
    // (`TailRecFixedBaseFold` is dispatched earlier — before the fuel gate —
    // since its `qexp` loop is classifier-rejected and would otherwise be
    // omitted; see the early return above.)

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
    // Datatype accumulator-generalization: the structurally-shrinking driver
    // given may not be the lemma's FIRST param (givens can be declared in any
    // order), so Dafny's default lexicographic measure — which tries params
    // left-to-right and would hit the GROWING accumulator first — fails. Pin the
    // measure to the driver. (The datatype-induction hint below recurses on its
    // field predecessor, which decreases this driver.)
    if law.when.is_none()
        && crate::codegen::common::accumulator_fold_fn_names(ctx).contains(&vb.fn_name)
        && let Some(fd) = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())
        && let Some(driver) = datatype_driver_given_name(fd, law)
    {
        lines.push(format!("  decreases {}", aver_name_to_dafny(&driver)));
    }
    lines.push("{".to_string());

    // Earlier sibling laws eligible to be cited into this proof — shared by the
    // `forall` hoist (here) and the explicit-instantiation engine (list-induction
    // step, below).
    let cites = eligible_cites(vb, law, ctx, opaque_fns, native_emitted);

    // Hoist additive-op facts at the top of the body (inductive paths only;
    // bounded-form bodies dispatch to samples and never reach the universal).
    if !needs_bounded_form {
        for lift in &op_lifts {
            lines.push(lift.clone());
        }
        // Decomposition pool: bring earlier sibling laws' universal facts into
        // scope by CALLING them — Z3 will not auto-apply the proved lemmas.
        for citation in earlier_law_citations(&cites, ctx) {
            lines.push(citation);
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
            // A THREADED accumulator given recurses at the value the fold feeds
            // it (`acc + xs[0]`), not the unchanged param — the Dafny counterpart
            // of Lean's `induction generalizing acc`. Without it the IH lands at
            // the wrong accumulator and Z3 cannot close `fold(xs, acc) == acc <op>
            // spec(xs)`. Falls back to the unchanged name for a non-accumulator
            // given (the verified fn is the top fn of the law's lhs/rhs).
            let verified_fd = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref());
            let other_args: Vec<String> = law
                .givens
                .iter()
                .enumerate()
                .map(|(i, g)| {
                    if i == list_given_idx {
                        format!("{}[1..]", list_param)
                    } else if let Some(threaded) = verified_fd.and_then(|fd| {
                        dafny_threaded_accumulator_arg(
                            fd,
                            &g.name,
                            &law.givens[list_given_idx].name,
                            &list_param,
                            ctx,
                        )
                    }) {
                        threaded
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
            // For a CONDITIONAL list law, guard the recursive call with the
            // premise applied to the TAIL (the induction target sliced `[1..]`),
            // so its `requires` holds — the membership analogue of the Lean
            // premise-threading (`prop_36` `when elem(x, y) -> elem(x, y ++ z)`:
            // recurse only when `elem(x, y[1..])`, else Z3 closes the head case
            // from `elem(x, y)` + the cons decomposition). A target-INDEPENDENT
            // premise (e.g. `prop_71`'s `eqNat(x, y)`) substitutes to itself, so
            // the guard is the premise — true in context — and the call fires
            // unconditionally exactly as before (no regression).
            if let Some(when_expr) = &law.when {
                let when_str = emit_expr_legacy(when_expr, ctx, None);
                let when_tail =
                    replace_ident_word(&when_str, &list_param, &format!("{list_param}[1..]"));
                lines.push(format!(
                    "    if {} {{ {}({}); }}",
                    when_tail,
                    lemma_name,
                    other_args.join(", ")
                ));
            } else {
                lines.push(format!("    {}({});", lemma_name, other_args.join(", ")));
            }
            // Engine B: explicit instantiation of the cited lemmas at the exact
            // arguments this inductive step needs. The `forall` hoist suffices when
            // Z3 can instantiate the universal itself (builtin `concat`); for a
            // user-defined operator it cannot materialise the nested term, so we
            // derive the instantiation generically (symbolic unfold + IH + match).
            // Inside the `else` (|xs| > 0), so `xs[0]` / `xs[1..]` are in range.
            if !needs_bounded_form {
                let cited: Vec<&VerifyLaw> = cites.iter().map(|(_, l)| *l).collect();
                let mut seen: std::collections::BTreeSet<String> =
                    std::collections::BTreeSet::new();
                for inst in crate::codegen::cite_instantiate::compute_instantiations(
                    law,
                    ind_var_src,
                    &cited,
                    ctx,
                ) {
                    let args: Vec<String> = inst
                        .args
                        .iter()
                        .map(|a| render_dafny_arg(a, &list_param))
                        .collect();
                    let call = format!("    {}({});", cites[inst.law_index].0, args.join(", "));
                    if seen.insert(call.clone()) {
                        lines.push(call);
                    }
                }
            }
            lines.push("  }".to_string());
        }
    } else if law.when.is_none()
        && crate::codegen::common::accumulator_fold_fn_names(ctx).contains(&vb.fn_name)
        && let Some(fd) = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())
    {
        // User-ADT accumulator-generalization (`triTR(n, acc) => plus(triSpec(n),
        // acc)`). The gate only reaches here when this self-fold is closeable
        // (`dafny_should_bound_accumulator_fold` is false — its combine fn has the
        // commutativity/associativity helpers), so emit the datatype-induction
        // hint mirroring the fold's `match` on its driver given. The cited algebra
        // (forall-hoisted above) discharges the residual.
        let lemma_name = format!("{}_{}", fn_name, law_name);
        if let Some(hint) = law
            .givens
            .iter()
            .find_map(|g| dafny_datatype_inductive_hint(fd, &g.name, &lemma_name, law, ctx))
        {
            lines.extend(hint);
        }
    } else if law
        .when
        .as_ref()
        .is_some_and(|w| when_is_peano_comparison(w, ctx))
        && !law.givens.is_empty()
        && law.givens.iter().all(|g| {
            crate::codegen::proof_recognize::peano_type_named(ctx, g.type_name.trim()).is_some()
        })
    {
        // Conditional Peano-`Nat` comparison law (`prop_70 leSucc`:
        // `requires le(m, n) ensures le(m, S n)`). Z3 cannot discharge the
        // universal from `{}` + fuel alone (auto-`{:induction}` does not close
        // it either — measured), so emit explicit structural induction on EVERY
        // `Nat` given: peel each in lockstep and recurse on the all-predecessors
        // tuple. The zero arms close from the `requires` + `le`-fuel; the
        // all-succ arm's recursive call supplies the induction hypothesis (the
        // `requires le(m', n')` it needs is exactly the unfolded outer
        // `le(S m', S n')`). Sound by construction: Dafny re-checks the
        // recursion and its termination, so a shape the lockstep peel does not
        // actually prove stays an honest verify error, never a false lemma.
        let lemma_name = format!("{}_{}", fn_name, law_name);
        let peeled: Vec<String> = law
            .givens
            .iter()
            .map(|g| format!("{}_p", aver_name_to_dafny(&g.name)))
            .collect();
        for (depth, g) in law.givens.iter().enumerate() {
            let peano = crate::codegen::proof_recognize::peano_type_named(ctx, g.type_name.trim())
                .expect("guarded by the all-Peano-given check above");
            let pad = "  ".repeat(depth + 1);
            lines.push(format!("{}match {} {{", pad, aver_name_to_dafny(&g.name)));
            lines.push(format!("{}  case {} => {{}}", pad, peano.base_ctor));
            lines.push(format!(
                "{}  case {}({}) =>",
                pad, peano.succ_ctor, peeled[depth]
            ));
        }
        let inner_pad = "  ".repeat(law.givens.len() + 1);
        lines.push(format!(
            "{}{}({});",
            inner_pad,
            lemma_name,
            peeled.join(", ")
        ));
        for depth in (0..law.givens.len()).rev() {
            lines.push(format!("{}}}", "  ".repeat(depth + 1)));
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

#[cfg(test)]
mod tests {
    use super::replace_ident_word;

    #[test]
    fn replace_ident_word_slices_whole_words_but_not_substrings() {
        // Whole-word identity is sliced; a substring (`yy`) and a dotted member
        // (`y.len`) keep their `y` untouched on the wrong boundary.
        assert_eq!(
            replace_ident_word("elem(x, y)", "y", "y[1..]"),
            "elem(x, y[1..])"
        );
        assert_eq!(replace_ident_word("yy && y", "y", "y[1..]"), "yy && y[1..]");
    }

    #[test]
    fn replace_ident_word_leaves_string_literals_untouched() {
        // The target spelled INSIDE a `"…"` literal must NOT be sliced — that
        // would corrupt the literal. The bare occurrence still is.
        assert_eq!(
            replace_ident_word("tag(y) == \"y\"", "y", "y[1..]"),
            "tag(y[1..]) == \"y\""
        );
        // Escaped quote inside the literal does not end the string early.
        assert_eq!(
            replace_ident_word("f(y) || s == \"a\\\"y\\\"b\"", "y", "y[1..]"),
            "f(y[1..]) || s == \"a\\\"y\\\"b\""
        );
    }
}
