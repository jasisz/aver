/// Aver top-level items → Dafny declarations.
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::parse_type_annotation;
use crate::types::Type;

use super::expr::{aver_name_to_dafny, emit_expr};

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
        Type::Named(name) => {
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
            if let Some(decl) = ctx.proof_ir.refined_types.get(name)
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
pub fn emit_fn_def_axiom(fd: &FnDef) -> String {
    let name = aver_name_to_dafny(&fd.name);
    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(pname, ptype)| format!("{}: {}", aver_name_to_dafny(pname), emit_type(ptype)))
        .collect();
    let ret_type = emit_type(&fd.return_type);

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

    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(pname, ptype)| format!("{}: {}", aver_name_to_dafny(pname), emit_type(ptype)))
        .collect();

    let ret_type = emit_type(&fd.return_type);

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
        return emit_expr(expr, ctx);
    }

    // For blocks with bindings, collect them and emit the last expression
    let mut parts = Vec::new();
    let mut final_expr = None;

    for (i, stmt) in stmts.iter().enumerate() {
        match stmt {
            Stmt::Binding(name, type_ann, expr) => {
                let mut val = emit_expr(expr, ctx);
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
                    final_expr = Some(emit_expr(expr, ctx));
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
    // Index-based pattern: last param is Int, there is also a List/String param
    // earlier, and the Int is not the first param → decreases |collection| - index.
    let list_param = fd
        .params
        .iter()
        .find(|(_, t)| t.starts_with("List<") || t == "String");
    let last_int = fd.params.iter().rposition(|(_, t)| t == "Int");
    let first_int = fd.params.iter().position(|(_, t)| t == "Int");
    if let (Some((list_name, _)), Some(last_idx)) = (list_param, last_int)
        && let Some(first_idx) = first_int
        && last_idx != first_idx
    // multiple Int params → last is likely index
    {
        let dlist = aver_name_to_dafny(list_name);
        let dint = aver_name_to_dafny(&fd.params[last_idx].0);
        return Some(DecreasesInfo {
            expr: format!("|{}| - {}", dlist, dint),
            requires: vec![],
        });
    }

    // Prefer structural recursion over a List/String param — it
    // matches the common "walk the collection" shape. An Int that
    // isn't an explicit index/countdown is usually a passive
    // argument (pivot, bound, threshold), so picking it for
    // `decreases` emits nonsense like `decreases pivot` on a fn
    // that actually iterates over the list. Fall back to Int only
    // when there's no collection param.
    for (pname, ptype) in &fd.params {
        if ptype.starts_with("List<") {
            return Some(DecreasesInfo {
                expr: format!("|{}|", aver_name_to_dafny(pname)),
                requires: vec![],
            });
        }
    }
    for (pname, ptype) in &fd.params {
        if ptype == "String" {
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
/// `<pname> < 0` (or equivalent) before any recursive call — i.e. the
/// author took care of the negative case themselves. Only a top-level
/// shape check: `match pname < 0` with a base arm for `true`, or an
/// initial stmt of the same shape. Anything deeper is conservative
/// (defaults to "doesn't handle", which emits a `requires`).
fn fn_handles_negative_first(fd: &FnDef, pname: &str) -> bool {
    let Some(first) = fd.body.stmts().first() else {
        return false;
    };
    let expr = match first {
        Stmt::Expr(e) => e,
        Stmt::Binding(_, _, _) => return false,
    };
    // `match pname < 0 { true -> …; false -> … }` elaborates to a
    // Match with a BinOp(Lt, pname, Literal::Int(0)) subject. The
    // resolver rewrites `Ident(pname)` to `Resolved { name: pname }`
    // before codegen, so accept both shapes.
    let Expr::Match { subject, .. } = &expr.node else {
        return false;
    };
    let Expr::BinOp(op, lhs, rhs) = &subject.node else {
        return false;
    };
    if !matches!(op, crate::ast::BinOp::Lt) {
        return false;
    }
    let lhs_name = match &lhs.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n,
        _ => return false,
    };
    if lhs_name != pname {
        return false;
    }
    matches!(&rhs.node, Expr::Literal(crate::ast::Literal::Int(0)))
}

/// Collect all function names called in an expression (top-level only).
fn collect_called_fns(expr: &Spanned<Expr>, out: &mut std::collections::BTreeSet<String>) {
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(&f.node) {
                // Skip builtins — only user functions need fuel
                if !name.contains('.') {
                    out.insert(name);
                }
            }
            collect_called_fns(f, out);
            for a in args {
                collect_called_fns(a, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_called_fns(l, out);
            collect_called_fns(r, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_called_fns(subject, out);
            for arm in arms {
                collect_called_fns(&arm.body, out);
            }
        }
        Expr::ErrorProp(inner) => collect_called_fns(inner, out),
        Expr::Constructor(_, Some(arg)) => collect_called_fns(arg, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_called_fns(e, out);
            }
        }
        Expr::List(elems) => {
            for e in elems {
                collect_called_fns(e, out);
            }
        }
        Expr::TailCall(tc) => {
            let TailCallData { target, args, .. } = tc.as_ref();
            if !target.contains('.') {
                out.insert(target.clone());
            }
            for a in args {
                collect_called_fns(a, out);
            }
        }
        Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_called_fns(e, out);
            }
        }
        Expr::Attr(obj, _) => collect_called_fns(obj, out),
        Expr::Neg(inner) => collect_called_fns(inner, out),
        _ => {}
    }
}

/// Get the top-level function name from a law expression like `fib(n)`.
fn law_top_level_fn(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::FnCall(fn_expr, _) => crate::codegen::common::expr_to_dotted_name(&fn_expr.node),
        _ => None,
    }
}

/// Check if a function is directly recursive (calls itself in its own body).
fn is_directly_recursive(fn_name: &str, ctx: &CodegenContext) -> bool {
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

fn collect_called_fns_in_body(body: &FnBody, out: &mut std::collections::BTreeSet<String>) {
    match body {
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) => collect_called_fns(expr, out),
                    Stmt::Expr(expr) => collect_called_fns(expr, out),
                }
            }
        }
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
    opaque_fns: &std::collections::HashSet<String>,
    fuel_emitted: &std::collections::HashSet<String>,
    native_emitted: &std::collections::HashSet<String>,
) -> Option<String> {
    if vb.cases.is_empty() {
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
                |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
                mode.clone(),
            ),
            rewrite_effectful_calls_in_law(
                rhs,
                law,
                |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
                mode,
            ),
        )
    });
    let any_opaque = first_rewrite
        .as_ref()
        .map(|(l, r)| law_refs_opaque_fn(l, opaque_fns) || law_refs_opaque_fn(r, opaque_fns))
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
            law_refs_opaque_fn(l, native_emitted) || law_refs_opaque_fn(r, native_emitted)
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
                |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
                mode.clone(),
            );
            let rhs_rw = rewrite_effectful_calls_in_law(
                rhs,
                law,
                |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
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
            let l = emit_expr(lhs_rw, ctx);
            let r = emit_expr(rhs_rw, ctx);
            let mut callees = std::collections::BTreeSet::new();
            collect_called_fns(lhs_rw, &mut callees);
            collect_called_fns(rhs_rw, &mut callees);
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
                        collect_called_fns_in_body(&fd.body, &mut callees);
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
                if fuel_emitted.contains(f) {
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
            let all_native = callees.iter().all(|f| !fuel_emitted.contains(f));
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
        lines.push(format!(
            "method test_{}_{}{}_samples() {{",
            fn_name, law_name, suffix
        ));
        for (lhs_rw, rhs_rw) in &rewritten {
            let l = emit_expr(lhs_rw, ctx);
            let r = emit_expr(rhs_rw, ctx);
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
    opaque: &std::collections::HashSet<String>,
) -> std::collections::HashSet<String> {
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
            if result.contains(&fd.name) {
                continue;
            }
            let mut callees = std::collections::BTreeSet::new();
            collect_called_fns_in_body(&fd.body, &mut callees);
            if callees.iter().any(|c| result.contains(c)) {
                result.insert(fd.name.clone());
                changed = true;
            }
        }
    }
    result
}

fn law_refs_opaque_fn(expr: &Spanned<Expr>, opaque: &std::collections::HashSet<String>) -> bool {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            let hits_callee = crate::codegen::common::expr_to_dotted_name(&callee.node)
                .is_some_and(|n| opaque.contains(&n));
            hits_callee
                || law_refs_opaque_fn(callee, opaque)
                || args.iter().any(|a| law_refs_opaque_fn(a, opaque))
        }
        Expr::BinOp(_, l, r) => law_refs_opaque_fn(l, opaque) || law_refs_opaque_fn(r, opaque),
        Expr::Match { subject, arms } => {
            law_refs_opaque_fn(subject, opaque)
                || arms.iter().any(|a| law_refs_opaque_fn(&a.body, opaque))
        }
        Expr::Attr(inner, _) | Expr::ErrorProp(inner) => law_refs_opaque_fn(inner, opaque),
        Expr::Constructor(_, Some(inner)) => law_refs_opaque_fn(inner, opaque),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|i| law_refs_opaque_fn(i, opaque))
        }
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, v)| law_refs_opaque_fn(v, opaque))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            law_refs_opaque_fn(base, opaque)
                || updates.iter().any(|(_, v)| law_refs_opaque_fn(v, opaque))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(inner) => law_refs_opaque_fn(inner, opaque),
            _ => false,
        }),
        _ => false,
    }
}

pub fn emit_verify_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    opaque_fns: &std::collections::HashSet<String>,
    native_emitted: &std::collections::HashSet<String>,
    suffix: &str,
) -> String {
    let fn_name = aver_name_to_dafny(&vb.fn_name);
    let law_name = aver_name_to_dafny(&law.name);

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
                return format!("{}: {}", aver_name_to_dafny(&g.name), refined);
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
        |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
        OracleInjectionMode::LemmaBinding,
    );
    let law_rhs = rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        |n| ctx.fn_defs.iter().find(|fd| fd.name == n),
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
            crate::codegen::common::strip_refinement_wrappers(&law_lhs, &lifted_vars),
            crate::codegen::common::strip_refinement_wrappers(&law_rhs, &lifted_vars),
        )
    };

    let lhs = emit_expr(&law_lhs, ctx);
    let rhs = emit_expr(&law_rhs, ctx);

    let mut lines = Vec::new();
    // Collect all functions used in the law for fuel annotations
    let mut law_fns = std::collections::BTreeSet::new();
    collect_called_fns(&law.lhs, &mut law_fns);
    collect_called_fns(&law.rhs, &mut law_fns);
    // Add transitive callees
    let mut transitive_fns = std::collections::BTreeSet::new();
    for f in &law_fns {
        if let Some(fd) = ctx.fn_defs.iter().find(|fd| &fd.name == f) {
            collect_called_fns_in_body(&fd.body, &mut transitive_fns);
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
        .filter(|f| ctx.fn_defs.iter().any(|fd| &fd.name == *f))
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
            let when_str = emit_expr(when_expr, ctx);
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
    let is_opaque =
        law_refs_opaque_fn(&law.lhs, opaque_fns) || law_refs_opaque_fn(&law.rhs, opaque_fns);
    // Native-decreases mutual recursion isn't opaque (Dafny unfolds
    // it), but the universal `add_commutative(a, b: int)` over
    // `int × int` still doesn't close as a true ∀ without a domain
    // restriction. Route through the bounded-∀ form the same way
    // opaque does — the case-split body composes per-pair sample
    // lemmas that Dafny *can* close from `{}` on the native path.
    let is_native_mutual = law_refs_opaque_fn(&law.lhs, native_emitted)
        || law_refs_opaque_fn(&law.rhs, native_emitted);
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
                        let val = literal_int_value(v).unwrap_or_else(|| emit_expr(v, ctx));
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
                    ctx.fn_defs.iter().any(|fd| {
                        fd.name == *f && count_recursive_calls_in_body(&fd.body, &fd.name) >= 2
                    })
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
        .position(|g| g.type_name.starts_with("List<"))
    {
        // Inductive hint for `List<T>`-parameterised laws — case-split
        // on `[] / [head, ..tail]` and recurse on the tail. Fires when
        // any fn called from either side (top-level OR nested) is
        // directly recursive — broader than the Int-given branch
        // which only inspects the top-level fn, because `List<T>` laws
        // often wrap the recursive fn under a Map / Option helper
        // (e.g. `Map.has(countWords(words), word)` — countWords is
        // recursive but `Map.has` is the top fn).
        let mut called: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
        collect_called_fns(&law.lhs, &mut called);
        collect_called_fns(&law.rhs, &mut called);
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
            lines.push(format!("  if |{}| == 0 {{", list_param));
            lines.push("  } else {".to_string());
            lines.push(format!("    {}({});", lemma_name, other_args.join(", ")));
            lines.push("  }".to_string());
        }
    }

    lines.push("}\n".to_string());

    lines.join("\n")
}
