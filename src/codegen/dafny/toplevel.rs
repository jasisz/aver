/// Aver top-level items → Dafny declarations.
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::parse_type_annotation;
use crate::types::Type;

use super::expr::{aver_name_to_dafny, emit_expr};

/// Emit a Dafny type from an Aver type annotation string.
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
        Type::Named(name) => name.clone(),
        Type::Unknown => "/* unknown type */".to_string(),
    }
}

/// Emit a Dafny datatype/record from a TypeDef.
pub fn emit_type_def(td: &TypeDef) -> Option<String> {
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

/// Emit a Dafny function from a FnDef.
pub fn emit_fn_def(fd: &FnDef, ctx: &CodegenContext) -> String {
    let name = aver_name_to_dafny(&fd.name);

    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(pname, ptype)| format!("{}: {}", aver_name_to_dafny(pname), emit_type(ptype)))
        .collect();

    let ret_type = emit_type(&fd.return_type);

    let body = emit_fn_body(&fd.body, ctx);

    let needs_decreases = body_has_recursive_call(&fd.body, &fd.name);

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

/// Emit the body of a function.
fn emit_fn_body(body: &FnBody, ctx: &CodegenContext) -> String {
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

    // Single Int param or Int-first: countdown pattern.
    for (pname, ptype) in &fd.params {
        if ptype == "Int" {
            let dname = aver_name_to_dafny(pname);
            return Some(DecreasesInfo {
                expr: format!("if {} >= 0 then {} else 0", dname, dname),
                requires: vec![],
            });
        }
    }
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
    None
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

/// Emit sample assertions from a law's domain expansion as a test method.
/// These are concrete smoke tests (e.g. `assert fib(5) == fibSpec(5)`).
/// Capped at [`MAX_LAW_SAMPLES`] to avoid Z3 timeouts on large domains.
pub fn emit_law_samples(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    suffix: &str,
) -> Option<String> {
    if vb.cases.is_empty() {
        return None;
    }

    let fn_name = aver_name_to_dafny(&vb.fn_name);
    let law_name = aver_name_to_dafny(&law.name);

    let samples: Vec<_> = vb.cases.iter().take(MAX_LAW_SAMPLES).collect();
    let truncated = vb.cases.len() > MAX_LAW_SAMPLES;

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
    lines.push(format!(
        "method test_{}_{}{}_samples() {{",
        fn_name, law_name, suffix
    ));

    for (lhs, rhs) in &samples {
        // Oracle v1: rewrite sample assertions the same way the lemma
        // body is rewritten — inject `BranchPath.root()` + given
        // bindings for effectful fns. Surface `pickOne()` can't stay
        // literal; it must become `pickOne(BranchPath.root(), stub)`.
        let lhs_rw =
            rewrite_effectful_calls_in_law(lhs, law, ctx, OracleInjectionMode::SampleValue);
        let rhs_rw =
            rewrite_effectful_calls_in_law(rhs, law, ctx, OracleInjectionMode::SampleValue);
        let l = emit_expr(&lhs_rw, ctx);
        let r = emit_expr(&rhs_rw, ctx);
        lines.push(format!("  assert {} == {};", l, r));
    }

    lines.push("}\n".to_string());
    Some(lines.join("\n"))
}

use crate::codegen::common::{OracleInjectionMode, rewrite_effectful_calls_in_law};

/// Emit a verify law as a Dafny lemma.
pub fn emit_verify_law(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> String {
    let fn_name = aver_name_to_dafny(&vb.fn_name);
    let law_name = aver_name_to_dafny(&law.name);

    let params: Vec<String> = law
        .givens
        .iter()
        .map(|g| {
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
    let law_lhs =
        rewrite_effectful_calls_in_law(&law.lhs, law, ctx, OracleInjectionMode::LemmaBinding);
    let law_rhs =
        rewrite_effectful_calls_in_law(&law.rhs, law, ctx, OracleInjectionMode::LemmaBinding);

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

    let fuel_attrs: String = law_fns
        .iter()
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

    if let Some(when_expr) = &law.when {
        let when_str = emit_expr(when_expr, ctx);
        lines.push(format!("  requires {}", when_str));
    }

    lines.push(format!("  ensures {} == {}", lhs, rhs));
    lines.push("{".to_string());

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
    }

    lines.push("}\n".to_string());

    lines.join("\n")
}
