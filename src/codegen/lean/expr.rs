/// Aver expressions → Lean 4 expression strings.
use super::builtins;
use super::pattern::emit_pattern;
pub(crate) use super::syntax::{aver_name_to_lean, lean_name_to_aver};
use crate::ast::{BinOp, Literal, Spanned};
use crate::codegen::CodegenContext;
use crate::codegen::common::{is_user_type, resolve_module_call};
use crate::ir::hir::{
    BuiltinCtor, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedMatchArm, ResolvedPattern,
    ResolvedStmt, ResolvedStrPart,
};

/// Emit a Lean 4 expression from an Aver Expr.
pub fn emit_expr(expr: &Spanned<ResolvedExpr>, ctx: &CodegenContext) -> String {
    match &expr.node {
        ResolvedExpr::Literal(lit) => emit_literal(lit),
        // Synthetic ident injected by the proof-mode recursion lowerer
        // (`recursion::rewrite_native_guarded_calls`) to mark a position
        // where Lean needs an `(by omega)` proof obligation for the
        // recursive-call precondition. Stays a plain Aver `ResolvedExpr::Ident`
        // through the AST so Dafny's emit path (which doesn't inject this
        // sentinel) and the type checker (already done before codegen)
        // never see it.
        ResolvedExpr::Ident(name) | ResolvedExpr::Resolved { name, .. }
            if name == crate::codegen::recursion::OMEGA_PROOF_SENTINEL =>
        {
            "(by omega)".to_string()
        }
        ResolvedExpr::Ident(name) | ResolvedExpr::Resolved { name, .. } => aver_name_to_lean(name),
        ResolvedExpr::Attr(obj, field) => {
            // Refinement-via-opaque records emit as Lean `Subtype`,
            // so the carrier field projects through `.val` instead
            // of the source-named `.carrier_field`. Detect by the
            // typechecker stamp on the host expression, then look
            // up the lifted-type decision the lowerer already made
            // in `ctx.proof_ir.refined_types`.
            if let Some(ty) = obj.ty()
                && let Some(decl) = crate::codegen::common::find_refined_type_for_named(ctx, ty)
                && field == &decl.carrier_field
            {
                let obj_str = emit_expr(obj, ctx);
                let needs_parens = !matches!(
                    &obj.node,
                    ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. }
                );
                return if needs_parens {
                    format!("({obj_str}).val")
                } else {
                    format!("{obj_str}.val")
                };
            }
            if let ResolvedExpr::Ident(type_name) = &obj.node {
                // Option.None → none
                if type_name == "Option" && field == "None" {
                    return "none".to_string();
                }
                // Oracle v1: `BranchPath.Root` is a nullary value
                // constructor defined in the Lean prelude — emit
                // verbatim so the reference resolves to the prelude
                // definition.
                if type_name == "BranchPath" && field == "Root" {
                    return "BranchPath.Root".to_string();
                }
                // User-defined type variant access: Shape.Point
                if is_user_type(type_name, ctx) {
                    return format!(
                        "{}.{}",
                        aver_name_to_lean(type_name),
                        super::syntax::lean_ctor_name(field)
                    );
                }
            }
            // Check module-qualified reference
            if let Some(full_dotted) = crate::ir::hir::resolved_to_dotted(&expr.node)
                && let Some((prefix, bare)) = resolve_module_call(&full_dotted, ctx)
            {
                if let Some(dot_pos) = bare.find('.') {
                    let type_name = &bare[..dot_pos];
                    let variant = &bare[dot_pos + 1..];
                    if is_user_type(type_name, ctx) {
                        return format!(
                            "{}.{}",
                            aver_name_to_lean(type_name),
                            super::syntax::lean_ctor_name(variant)
                        );
                    }
                }
                let bare_lean = aver_name_to_lean(bare);
                if !ctx.modules.is_empty() {
                    return format!("{}.{}", super::syntax::aver_path_to_lean(prefix), bare_lean);
                }
                return bare_lean;
            }
            let obj_str = emit_expr(obj, ctx);
            let needs_parens =
                !matches!(&obj.node, ResolvedExpr::Ident(_) | ResolvedExpr::Attr(_, _));
            if needs_parens {
                format!("({}).{}", obj_str, aver_name_to_lean(field))
            } else {
                format!("{}.{}", obj_str, aver_name_to_lean(field))
            }
        }
        ResolvedExpr::Call(callee, args) => emit_fn_call(callee, args, ctx),
        ResolvedExpr::Neg(inner) => format!("(-{})", emit_expr(inner, ctx)),
        ResolvedExpr::BinOp(op, left, right) => {
            let l = emit_expr(left, ctx);
            let r = emit_expr(right, ctx);
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            format!("({} {} {})", l, op_str, r)
        }
        ResolvedExpr::Match { subject, arms } => emit_match(subject, arms, expr.line, ctx),
        ResolvedExpr::Ctor(ctor, args) => emit_constructor(ctor, args, ctx),
        ResolvedExpr::ErrorProp(inner) => {
            let inner_str = emit_expr(inner, ctx);
            if ctx.lean_do_block.get() {
                format!("(<- {})", inner_str)
            } else {
                // Law statements are not emitted inside `do`, so they retain
                // the legacy fallback until their propagation semantics can be
                // modeled separately.
                format!("(({}).withDefault default)", inner_str)
            }
        }
        ResolvedExpr::InterpolatedStr(parts) => emit_interpolated_str(parts, ctx),
        ResolvedExpr::List(elements) => {
            if elements.is_empty() {
                "[]".to_string()
            } else {
                let parts: Vec<String> = elements.iter().map(|e| emit_expr(e, ctx)).collect();
                format!("[{}]", parts.join(", "))
            }
        }
        ResolvedExpr::Tuple(items) | ResolvedExpr::IndependentProduct(items, _) => {
            // Oracle v1: passed through as a tuple; `?!` semantic fold
            // is deferred (coordinates with the outer `Result.Ok(...)`
            // wrapper the typechecker forces).
            let parts: Vec<String> = items.iter().map(|e| emit_expr(e, ctx)).collect();
            format!("({})", parts.join(", "))
        }
        ResolvedExpr::MapLiteral(entries) => {
            if entries.is_empty() {
                "[]".to_string()
            } else if entries
                .iter()
                .all(|(_, v)| crate::codegen::common::is_unit_expr_resolved(&v.node))
            {
                // Map<T, Unit> literal → set literal
                let parts: Vec<String> = entries.iter().map(|(k, _)| emit_expr(k, ctx)).collect();
                format!("AverSet.ofList [{}]", parts.join(", "))
            } else {
                let parts: Vec<String> = entries
                    .iter()
                    .map(|(k, v)| format!("({}, {})", emit_expr(k, ctx), emit_expr(v, ctx)))
                    .collect();
                format!("[{}]", parts.join(", "))
            }
        }
        ResolvedExpr::RecordCreate {
            type_name, fields, ..
        } => {
            // Refinement-via-opaque types emit as Lean `Subtype`,
            // so construction is `⟨value, proof⟩`. The proof
            // obligation is whatever predicate the smart
            // constructor branches on; we emit `by omega` because:
            //   * for `if h : pred then ⟨v, _⟩ else …` shapes the
            //     dependent-if binding `h` is in scope and omega
            //     picks it up automatically.
            //   * for literal sample positions (`⟨7, _⟩` in a
            //     theorem about `7 ≥ 0`) omega closes by constant
            //     evaluation.
            //   * for law-quantified positions where the law's
            //     `when` clause guarantees the predicate, the
            //     quant-lifter (toplevel.rs) emits the refined
            //     type directly so RecordCreate never appears
            //     against a free Int — only against concrete
            //     samples and intro-bound values.
            // Refinement records emit as Subtype only when the
            // carrier is `Int` (see emit_product_type for the
            // matching guard). Float-carrier records keep the
            // structure shape and a plain `{ value := … }` record
            // literal, so this fast-path is gated on the carrier
            // matching.
            //   * for a structural carrier the predicate is a
            //     recursive helper compiled by well-founded
            //     recursion, which `decide` cannot evaluate through
            //     the elaborator (see `crypto_model.lean`). Those
            //     goals need the helper's equation lemmas, so a
            //     final `simp [<predicate>]` rung is appended when
            //     the invariant's head is a nameable function — the
            //     name is read off the refinement's own invariant,
            //     never hardcoded. This is the rung that closes the
            //     literal smart-constructor discharge
            //     (`Bytes.fromList([0, 10, 255])` → `⟨[0, 10, 255],
            //     by … simp [Bytes.allInRange]⟩`): the emitted proof
            //     re-establishes, in Lean, exactly the fact the
            //     discharge gate claimed.
            if let Some(decl) = crate::codegen::common::find_refined_type(ctx, type_name)
                && fields.len() == 1
            {
                let (_, value_expr) = &fields[0];
                let value_str = emit_expr(value_expr, ctx);
                let mut ladder =
                    "first | omega | decide | (simp_all; omega) | assumption".to_string();
                if let Some(predicate) = invariant_head_name(&decl.invariant.expr, ctx) {
                    ladder.push_str(&format!(" | simp [{predicate}]"));
                }
                return format!("⟨{value_str}, by {ladder}⟩");
            }
            let parts: Vec<String> = fields
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_lean(name), emit_expr(expr, ctx))
                })
                .collect();
            // Builtin HOST carrier records (`Terminal.Size`,
            // `Tcp.Connection`, …) map to underscored Lean structure
            // names; a USER dep-module record keeps its dotted
            // namespaced path (it is emitted inside `namespace M`).
            // Same translation as `lean::types`'s `Type::Named`.
            let lean_type_name = super::types::lean_named_type_name(type_name);
            format!("{{ {} : {} }}", parts.join(", "), lean_type_name)
        }
        ResolvedExpr::RecordUpdate {
            type_name: _,
            base,
            updates,
            ..
        } => {
            let base_str = emit_expr(base, ctx);
            let parts: Vec<String> = updates
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_lean(name), emit_expr(expr, ctx))
                })
                .collect();
            format!("{{ {} with {} }}", base_str, parts.join(", "))
        }
        ResolvedExpr::TailCall { target, args } => {
            // TailCall is an internal optimization — emit as regular call.
            // Resolve FnId → canonical name via the symbol table, then
            // strip the module prefix because Lean's emit doesn't
            // qualify intra-module recursive calls.
            let target_name = ctx.symbol_table.fn_entry(*target).key.name.clone();
            let parts: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            if parts.is_empty() {
                aver_name_to_lean(&target_name)
            } else {
                format!("{} {}", aver_name_to_lean(&target_name), parts.join(" "))
            }
        }
    }
}

/// Emit an expression wrapped in parens if it's a compound expression.
fn emit_expr_atom(expr: &Spanned<ResolvedExpr>, ctx: &CodegenContext) -> String {
    let s = emit_expr(expr, ctx);
    match &expr.node {
        ResolvedExpr::Literal(Literal::Int(i)) if *i < 0 => format!("({})", s),
        ResolvedExpr::Literal(Literal::Float(f)) if *f < 0.0 => format!("({})", s),
        ResolvedExpr::Literal(_)
        | ResolvedExpr::Ident(_)
        | ResolvedExpr::List(_)
        | ResolvedExpr::Tuple(_)
        | ResolvedExpr::IndependentProduct(_, _) => s,
        _ => {
            if s.starts_with('(')
                || s.starts_with('[')
                || s.starts_with('"')
                || s.starts_with('{')
                || !s.contains(' ')
            {
                s
            } else {
                format!("({})", s)
            }
        }
    }
}

fn emit_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => format!("{}", i),
        // Lean numerals are arbitrary-precision; emit the digits bare and let
        // expected-type elaboration pin them to `Int` (same as the i64 path).
        Literal::BigInt(s) => s.clone(),
        Literal::Float(f) => {
            let s = f.to_string();
            if s.contains('.') {
                s
            } else {
                format!("{}.0", s)
            }
        }
        Literal::Str(s) => format!("\"{}\"", escape_lean_string(s)),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn escape_lean_string(s: &str) -> String {
    crate::codegen::common::escape_string_literal(s)
}

/// Lean name of the function a refinement's invariant applies, when the
/// invariant is a single call (`allInRange xs`). Used to give the
/// Subtype-construction tactic ladder an unfolding rung for predicates
/// the elaborator cannot evaluate — structural helpers compiled by
/// well-founded recursion. A non-call invariant (a bare comparison such
/// as `n >= 0`) has no name to unfold and returns `None`; those goals
/// are already closed by `omega` / `decide`.
fn invariant_head_name(invariant: &Spanned<ResolvedExpr>, ctx: &CodegenContext) -> Option<String> {
    // Only a USER function has equation lemmas worth unfolding; a builtin
    // head (`Bool.and`, a comparison) is already in `simp`'s reach and
    // rendering one here would need its arguments anyway.
    let ResolvedExpr::Call(ResolvedCallee::Fn(fn_id), _) = &invariant.node else {
        return None;
    };
    let entry = ctx.symbol_table.fn_entry(*fn_id);
    let bare = aver_name_to_lean(entry.key.name.as_str());
    Some(match entry.key.scope_str() {
        Some(prefix) if !ctx.modules.is_empty() => {
            format!("{}.{}", super::syntax::aver_path_to_lean(prefix), bare)
        }
        _ => bare,
    })
}

fn emit_fn_call(
    callee: &ResolvedCallee,
    args: &[Spanned<ResolvedExpr>],
    ctx: &CodegenContext,
) -> String {
    // Resolved-form classification — preserves the pre-Phase-E
    // dispatch order: builtin special-cases, Oracle BranchPath
    // hand-shapes, module-qualified, then plain ident.
    match callee {
        ResolvedCallee::Builtin(name) => {
            if let Some(lean_code) = builtins::emit_builtin_call(name, args, ctx) {
                return lean_code;
            }
            // Oracle v1: BranchPath ctors render through structure
            // definitions in LEAN_PRELUDE_BRANCH_PATH.
            let arg_strs_owned: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            match name.as_str() {
                "BranchPath.child" if arg_strs_owned.len() == 2 => {
                    return format!(
                        "BranchPath.child {} {}",
                        arg_strs_owned[0], arg_strs_owned[1]
                    );
                }
                "BranchPath.parse" if arg_strs_owned.len() == 1 => {
                    return format!("BranchPath.parse {}", arg_strs_owned[0]);
                }
                _ => {}
            }
            // Generic builtin fallback: render dotted with each arg
            // as a Lean atom.
            if arg_strs_owned.is_empty() {
                aver_name_to_lean(name)
            } else {
                format!("{} {}", aver_name_to_lean(name), arg_strs_owned.join(" "))
            }
        }
        ResolvedCallee::Intrinsic(intr) => {
            use crate::ir::hir::BuiltinIntrinsic;
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            // Literal-divisor discharge: for a nonzero literal divisor
            // `Int.div` / `Int.mod` are total, and Lean's `/` / `%` are
            // Euclidean on `Int` (`Int.ediv` / `Int.emod` — matching the
            // runtime for every sign combination), so render the bare op.
            // The HIR resolver produces these intrinsics for every
            // discharged source call.
            match intr {
                BuiltinIntrinsic::IntDivEuclid if arg_strs.len() == 2 => {
                    format!("({} / {})", arg_strs[0], arg_strs[1])
                }
                BuiltinIntrinsic::IntModEuclid if arg_strs.len() == 2 => {
                    format!("({} % {})", arg_strs[0], arg_strs[1])
                }
                // Compiler-synthesised `__buf_*` / `__to_str` intrinsics
                // don't reach the Lean backend in practice (Lean emit
                // doesn't see post-interp-lower buffer shapes), but the
                // resolver carries them through; render as bare-name
                // call so the diagnostic stays traceable.
                _ if arg_strs.is_empty() => intr.name().to_string(),
                _ => format!("{} {}", intr.name(), arg_strs.join(" ")),
            }
        }
        ResolvedCallee::Fn(fn_id) => {
            let entry = ctx.symbol_table.fn_entry(*fn_id);
            let bare = entry.key.name.as_str();
            let module_prefix = entry.key.scope_str();
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            let func = match module_prefix {
                Some(prefix) if !ctx.modules.is_empty() => {
                    format!(
                        "{}.{}",
                        super::syntax::aver_path_to_lean(prefix),
                        aver_name_to_lean(bare)
                    )
                }
                _ => aver_name_to_lean(bare),
            };
            if arg_strs.is_empty() {
                func
            } else {
                format!("{} {}", func, arg_strs.join(" "))
            }
        }
        ResolvedCallee::LocalSlot { name, .. } => {
            // First-class fn value bound to a local — curry application.
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            let func = aver_name_to_lean(name);
            if arg_strs.is_empty() {
                func
            } else {
                format!("{} {}", func, arg_strs.join(" "))
            }
        }
        ResolvedCallee::Unresolved { callee: inner } => {
            // Typecheck-rejected callee — render the source-faithful
            // expression as a curry'd call so the surrounding Lean
            // proof still typechecks (verify driver surfaces the
            // missing target separately).
            let func = emit_expr(inner, ctx);
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            if arg_strs.is_empty() {
                func
            } else {
                format!("{} {}", func, arg_strs.join(" "))
            }
        }
    }
}

fn emit_constructor(
    ctor: &ResolvedCtor,
    args: &[Spanned<ResolvedExpr>],
    ctx: &CodegenContext,
) -> String {
    let inner_str = || -> String {
        args.first()
            .map(|a| emit_expr_atom(a, ctx))
            .unwrap_or_else(|| "()".to_string())
    };
    match ctor {
        ResolvedCtor::Builtin(BuiltinCtor::ResultOk) => format!("Except.ok {}", inner_str()),
        ResolvedCtor::Builtin(BuiltinCtor::ResultErr) => {
            format!("Except.error {}", inner_str())
        }
        ResolvedCtor::Builtin(BuiltinCtor::OptionSome) => format!("some {}", inner_str()),
        ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => "none".to_string(),
        ResolvedCtor::User { type_id, name, .. } => {
            let type_name = ctx.symbol_table.type_entry(*type_id).key.name.clone();
            // Canonical Peano type lifted to builtin `Nat`: `Z` → `0`, `S(e)` → `(e + 1)`.
            if let Some(role) =
                crate::codegen::proof_recognize::peano_ctor_role(ctx, &type_name, name)
            {
                return match role {
                    crate::codegen::proof_recognize::PeanoCtor::Zero => "0".to_string(),
                    crate::codegen::proof_recognize::PeanoCtor::Succ => {
                        format!("({} + 1)", inner_str())
                    }
                };
            }
            // User ctor: `Type.variant` in Lean. Lean convention is
            // lowercase variant names; both segments pass through the
            // reserved-token guard (`Type` / `Match` are legal Aver names).
            let type_name = super::syntax::aver_path_to_lean(&type_name);
            let variant = super::syntax::lean_ctor_name(name);
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr_atom(a, ctx)).collect();
            if arg_strs.is_empty() {
                format!("{}.{}", type_name, variant)
            } else {
                format!("{}.{} {}", type_name, variant, arg_strs.join(" "))
            }
        }
        ResolvedCtor::Unresolved { name } => {
            // Typecheck-rejected ctor — surface the source name as a
            // call expression so the surrounding emit still produces
            // a parseable Lean term.
            format!("{} {}", name, inner_str())
        }
    }
}

fn emit_interpolated_str(parts: &[ResolvedStrPart], ctx: &CodegenContext) -> String {
    if parts.is_empty() {
        return "\"\"".to_string();
    }

    let mut result = String::new();
    result.push_str("s!\"");
    for part in parts {
        match part {
            ResolvedStrPart::Literal(s) => {
                result.push_str(&escape_lean_string(s));
            }
            ResolvedStrPart::Parsed(expr) => {
                result.push('{');
                result.push_str(&emit_expr(expr, ctx));
                result.push('}');
            }
        }
    }
    result.push('"');
    result
}

fn emit_match(
    subject: &Spanned<ResolvedExpr>,
    arms: &[ResolvedMatchArm],
    line: usize,
    ctx: &CodegenContext,
) -> String {
    // Bool match → if/then/else (avoids Lean dependent elimination issues)
    if let Some((true_body, false_body)) = extract_bool_arms(arms) {
        let monadify_arms = ctx.lean_do_block.get()
            && (resolved_expr_contains_error_prop(true_body)
                || resolved_expr_contains_error_prop(false_body));
        let cond = emit_expr(subject, ctx);
        let mut t = emit_expr(true_body, ctx);
        let mut f = emit_expr(false_body, ctx);
        if monadify_arms {
            t = format!("(do pure ({t}))");
            f = format!("(do pure ({f}))");
        }
        // Dependent `if h : cond then T else F` ONLY when the true
        // branch contains a refinement-Subtype constructor — those
        // need the predicate as a hypothesis in scope to discharge
        // their `by omega` proof obligation. Plain `if` everywhere
        // else keeps spec-equivalence and other auto-provers (which
        // pattern-match on the plain `if`-shape) working.
        // Parenthesize: an `if/then/else` is greedy, so an unwrapped
        // emission breaks in two places the Bool-match path actually
        // hits — a NESTED match (`if c1 then (if c2 …) else …`, where the
        // inner `else` would otherwise be swallowed) and an appended
        // operator (a `when` premise gets `= true` appended, and
        // `else f = true` would parse as `else (f = true)`). Wrapping is
        // transparent to Lean's elaborator and tactics (same `ite`).
        if true_body_uses_refinement_subtype(true_body, ctx) {
            let hyp = format!("h_{line}");
            if monadify_arms {
                // Lean's `do` notation is layout-sensitive: `if` and `else`
                // must align and remain strictly inside the nested action.
                return format!("(<- (do\n     if {hyp} : {cond} then {t}\n     else {f}))");
            }
            return format!("(if {hyp} : {cond} then {t}\n  else {f})");
        }
        if monadify_arms {
            return format!("(<- (do\n     if {cond} then {t}\n     else {f}))");
        }
        return format!("(if {cond} then {t}\n  else {f})");
    }
    let monadify_arms = ctx.lean_do_block.get()
        && arms
            .iter()
            .any(|arm| resolved_expr_contains_error_prop(&arm.body));
    let subj = emit_expr(subject, ctx);
    let mut arm_strs = Vec::new();
    for arm in arms {
        let pat = emit_pattern(&arm.pattern, ctx);
        let mut body = emit_expr(&arm.body, ctx);
        if monadify_arms {
            body = format!("(do pure ({body}))");
        }
        if body.contains('\n') {
            let body_lines: Vec<&str> = body.lines().collect();
            let mut rendered = vec![format!("  | {} => {}", pat, body_lines[0])];
            rendered.extend(
                body_lines
                    .iter()
                    .skip(1)
                    .map(|line| format!("    {}", line)),
            );
            arm_strs.push(rendered.join("\n"));
        } else {
            arm_strs.push(format!("  | {} => {}", pat, body));
        }
    }
    // Use `match h_NN : <ident> with …` (named form) only when the
    // subject is a local ident — that's where Lean's wf elaboration
    // needs the equation `h_NN : ident = pattern` to relate the
    // outer match's pattern-binder to the inner match's wildcard
    // binder during decreasing-tactic resolution. Concretely: a
    // `ListStructural` fn like `showListIntInner` with nested
    // `match xs / match tail` loses the `tail = x✝` equation under
    // plain `match`, and `decreasing_tactic` can't prove the rec-arg
    // measure decrease.
    //
    // Wrapper-return matches (e.g. `match foo() with | .ok x => ...
    // | .error e => ...`) keep the plain form — their subject is an
    // `Expr::FnCall` whose match equation is opaque to the
    // wf elaborator anyway, and `simp [foo]` still needs to reduce
    // `if`-inside-match cleanly in the auto-proof tactic chain.
    // Three fuel-helper emitters still call `strip_match_eq_binders`;
    // with this guard the strip only fires for the ident path,
    // preserving wrapper-return emit untouched.
    let needs_eq_binder = matches!(
        &subject.node,
        ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } | ResolvedExpr::Attr(_, _)
    );
    let emitted_match = if needs_eq_binder {
        let eq_name = format!("h_{}", line);
        format!("match {} : {} with\n{}", eq_name, subj, arm_strs.join("\n"))
    } else {
        format!("match {} with\n{}", subj, arm_strs.join("\n"))
    };
    if monadify_arms {
        let nested_match = emitted_match
            .lines()
            .map(|line| format!("     {line}"))
            .collect::<Vec<_>>()
            .join("\n");
        format!("(<- (do\n{nested_match}))")
    } else {
        emitted_match
    }
}

pub(super) fn resolved_expr_contains_error_prop(expr: &Spanned<ResolvedExpr>) -> bool {
    match &expr.node {
        ResolvedExpr::ErrorProp(_) => true,
        ResolvedExpr::Attr(obj, _) => resolved_expr_contains_error_prop(obj),
        ResolvedExpr::Call(callee, args) => {
            let callee_contains = match callee {
                ResolvedCallee::Unresolved { callee } => resolved_expr_contains_error_prop(callee),
                _ => false,
            };
            callee_contains || args.iter().any(resolved_expr_contains_error_prop)
        }
        ResolvedExpr::BinOp(_, left, right) => {
            resolved_expr_contains_error_prop(left) || resolved_expr_contains_error_prop(right)
        }
        ResolvedExpr::Neg(inner) => resolved_expr_contains_error_prop(inner),
        ResolvedExpr::Match { subject, arms } => {
            resolved_expr_contains_error_prop(subject)
                || arms
                    .iter()
                    .any(|arm| resolved_expr_contains_error_prop(&arm.body))
        }
        ResolvedExpr::Ctor(_, args) => args.iter().any(resolved_expr_contains_error_prop),
        ResolvedExpr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            ResolvedStrPart::Parsed(expr) => resolved_expr_contains_error_prop(expr),
            ResolvedStrPart::Literal(_) => false,
        }),
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => {
            items.iter().any(resolved_expr_contains_error_prop)
        }
        ResolvedExpr::MapLiteral(entries) => entries.iter().any(|(key, value)| {
            resolved_expr_contains_error_prop(key) || resolved_expr_contains_error_prop(value)
        }),
        ResolvedExpr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, value)| resolved_expr_contains_error_prop(value)),
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            resolved_expr_contains_error_prop(base)
                || updates
                    .iter()
                    .any(|(_, value)| resolved_expr_contains_error_prop(value))
        }
        ResolvedExpr::TailCall { args, .. } => args.iter().any(resolved_expr_contains_error_prop),
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => false,
    }
}

/// True iff `expr` (recursively) contains a `RecordCreate` whose
/// type is a refinement record — i.e. one we'll emit
/// as a Lean Subtype constructor `⟨val, by omega⟩` that needs the
/// surrounding `if`'s predicate as a hypothesis. Used to decide
/// when the enclosing Bool match should emit dependent-`if h :
/// cond then …`.
fn true_body_uses_refinement_subtype(expr: &Spanned<ResolvedExpr>, ctx: &CodegenContext) -> bool {
    match &expr.node {
        ResolvedExpr::RecordCreate { type_name, .. } => {
            crate::codegen::common::find_refined_type(ctx, type_name).is_some()
        }
        ResolvedExpr::Call(_, args) => args
            .iter()
            .any(|a| true_body_uses_refinement_subtype(a, ctx)),
        ResolvedExpr::Ctor(_, args) => args
            .iter()
            .any(|a| true_body_uses_refinement_subtype(a, ctx)),
        ResolvedExpr::Attr(o, _) => true_body_uses_refinement_subtype(o, ctx),
        ResolvedExpr::Match { arms, .. } => arms
            .iter()
            .any(|arm| true_body_uses_refinement_subtype(&arm.body, ctx)),
        _ => false,
    }
}

/// If all arms are `true -> expr` and `false -> expr`, return (true_body, false_body).
fn extract_bool_arms(
    arms: &[ResolvedMatchArm],
) -> Option<(&Spanned<ResolvedExpr>, &Spanned<ResolvedExpr>)> {
    if arms.len() != 2 {
        return None;
    }
    let mut true_body = None;
    let mut false_body = None;
    for arm in arms {
        match &arm.pattern {
            ResolvedPattern::Literal(Literal::Bool(true)) => true_body = Some(arm.body.as_ref()),
            ResolvedPattern::Literal(Literal::Bool(false)) => false_body = Some(arm.body.as_ref()),
            _ => return None,
        }
    }
    Some((true_body?, false_body?))
}

/// Emit a statement as Lean 4 code.
///
/// **Currently unused** after epic #170 Phase 5 PR E2 dropped
/// `emit_stmt_legacy` from the toplevel hot path (`emit_fn_body` /
/// `emit_do_stmt` now inline the resolve + emit pair). Retained as
/// the public resolved-stmt API for future callers (proof rewriters,
/// LSP-mode renderers).
#[allow(dead_code)]
pub fn emit_stmt(stmt: &ResolvedStmt, ctx: &CodegenContext) -> String {
    match stmt {
        ResolvedStmt::Binding {
            name,
            ty_ann: _,
            value,
        } => {
            let val = emit_expr(value, ctx);
            format!("let {} := {}", aver_name_to_lean(name), val)
        }
        ResolvedStmt::Expr(expr) => emit_expr(expr, ctx),
    }
}

/// Source-shape adapter for callers that still hold a raw
/// `Spanned<crate::ast::Expr>` (TCO / mutual-TCO bodies, law_auto
/// proof generation, recursion fuel emit, the various AST walks in
/// `toplevel.rs`). Resolves the expression on demand against the
/// codegen context's symbol table — `scope` carries the owning
/// module prefix when known (`None` for entry-scope code), same
/// shape as PR 9.4's `EmitCtx::current_module_scope` in rust
/// codegen. The migrated `emit_expr` core stays
/// `ResolvedExpr`-only.
pub fn emit_expr_legacy(
    expr: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    let active = ctx.active_module_scope();
    let effective = scope.or(active.as_deref());
    let resolved = ctx.resolve_expr(expr, effective);
    emit_expr(&resolved, ctx)
}

/// Source-shape adapter for [`emit_stmt`]. See [`emit_expr_legacy`] for
/// the scope-fallback rule. **Currently unused** post-PR-E2 — see
/// [`emit_stmt`] doc.
#[allow(dead_code)]
pub fn emit_stmt_legacy(
    stmt: &crate::ast::Stmt,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    let active = ctx.active_module_scope();
    let effective = scope.or(active.as_deref());
    let resolved = ctx.resolve_stmt(stmt, effective);
    emit_stmt(&resolved, ctx)
}

/// Source-shape adapter for [`emit_pattern`]. See [`emit_expr_legacy`]
/// for the scope-fallback rule.
#[allow(dead_code)]
pub fn emit_pattern_legacy(
    pat: &crate::ast::Pattern,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    let active = ctx.active_module_scope();
    let effective = scope.or(active.as_deref());
    let resolved = ctx.resolve_pattern(pat, effective);
    super::pattern::emit_pattern(&resolved, ctx)
}

#[cfg(test)]
mod tests {
    use super::escape_lean_string;

    #[test]
    fn escape_lean_string_escapes_control_chars() {
        assert_eq!(escape_lean_string("\u{0008}\u{000C}"), "\\x08\\x0c");
        assert_eq!(escape_lean_string("a\n\t\"\\z"), "a\\n\\t\\\"\\\\z");
        // Guillemets are emitted raw inside the string literal; the context-aware
        // certificate scanner treats them as inert string bytes, not as an
        // identifier delimiter, so no escaping is needed.
        assert_eq!(escape_lean_string("«ok»"), "«ok»");
    }
}
