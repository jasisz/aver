mod crypto;
/// Aver → Dafny transpiler.
///
/// Single-module sources emit one `.dfy` file. Multi-module sources emit
/// one file per dependent module wrapped in `module M { ... }`, plus a
/// shared `common.dfy` with built-in records/helpers, plus the entry
/// file holding the trust header, top-level items, and verify lemmas.
mod expr;
mod fuel;
mod lemmas;
mod toplevel;

use crate::ast::{FnDef, TopLevel, VerifyKind};
use crate::codegen::{CodegenContext, ProjectOutput};

/// Check if a function body uses the `?` (ErrorProp) operator.
/// Such functions require early-return semantics that Dafny pure functions cannot express.
fn body_uses_error_prop(body: &std::sync::Arc<crate::ast::FnBody>) -> bool {
    match body.as_ref() {
        crate::ast::FnBody::Block(stmts) => stmts.iter().any(|s| match s {
            crate::ast::Stmt::Binding(_, _, expr) => expr_uses_error_prop(expr),
            crate::ast::Stmt::Expr(expr) => expr_uses_error_prop(expr),
        }),
    }
}

// Exhaustive on `Expr`: a variant this misses routes a fn carrying `?`
// to ordinary emission, where the `?` renders as the error marker in
// `expr::emit_expr`.
fn expr_uses_error_prop(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(f, args) => expr_uses_error_prop(f) || args.iter().any(expr_uses_error_prop),
        Expr::BinOp(_, l, r) => expr_uses_error_prop(l) || expr_uses_error_prop(r),
        Expr::Neg(inner) => expr_uses_error_prop(inner),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(subject) || arms.iter().any(|a| expr_uses_error_prop(&a.body))
        }
        Expr::Constructor(_, Some(arg)) => expr_uses_error_prop(arg),
        Expr::List(elems) | Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            elems.iter().any(expr_uses_error_prop)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_uses_error_prop(k) || expr_uses_error_prop(v)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_uses_error_prop(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(base) || updates.iter().any(|(_, e)| expr_uses_error_prop(e))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(e) => expr_uses_error_prop(e),
            crate::ast::StrPart::Literal(_) => false,
        }),
        Expr::Attr(obj, _) => expr_uses_error_prop(obj),
        Expr::TailCall(inner) => inner.args.iter().any(expr_uses_error_prop),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            false
        }
    }
}

/// Does `?` survive the pure `?!`/`?` lowering that every Dafny
/// emission path runs first?
///
/// The lowering rewrites `?` at statement position into a `match`
/// cascade, but recurses *through* a `?` sitting inside an expression
/// and leaves it in place. So the question the emitter has to ask is
/// whether the lowered body still carries a `?`, not whether the
/// lowering produced anything: a residual `?` has no Dafny expression
/// form and the function must be emitted as an opaque axiom instead.
fn body_keeps_error_prop_after_lowering(fd: &FnDef) -> bool {
    if !body_uses_error_prop(&fd.body) {
        return false;
    }
    match crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
        .ok()
        .flatten()
    {
        Some(lowered) => body_uses_error_prop(&lowered.body),
        None => true,
    }
}

/// Transpile an Aver program into a Dafny project.
pub fn transpile(ctx: &CodegenContext) -> ProjectOutput {
    transpile_unified(ctx)
}

/// Translate an Aver module prefix into a Dafny module identifier.
/// Two transformations:
/// - Dotted Aver prefixes (`Models.User`) flatten to underscore form
///   so Dafny doesn't treat them as nested-module cycles when sibling
///   submodules import each other.
/// - Every emitted module name is prefixed with `Aver_` so that
///   user-source `module Foo` (the Aver namespace of fns operating on
///   a record) cannot collide with a `record Foo` declared in some
///   other Aver module — Dafny resolves type and module names in the
///   same namespace, so `Aver_Foo` (module) ≠ `Foo` (datatype).
pub(crate) fn dafny_module_name(prefix: &str) -> String {
    format!("Aver_{}", prefix.replace('.', "_"))
}

/// Multi-file Dafny output: one file per dependent module wrapped in
/// `module M { ... }`, a shared `common.dfy` carrying built-in records
/// and helpers under `module AverCommon`, and an entry `<project>.dfy`
/// with the trust header, top-level items, and verify lemmas.
fn transpile_unified(ctx: &CodegenContext) -> ProjectOutput {
    use std::collections::{HashMap, HashSet};

    fn capability_declarations(ctx: &CodegenContext, module: &str) -> Vec<String> {
        let mut declarations = ctx
            .capabilities
            .opaque_types()
            .filter_map(|canonical| {
                let (owner, name) = canonical.rsplit_once('.')?;
                (owner == module).then(|| {
                    format!(
                        "type {}",
                        crate::codegen::dafny::expr::aver_name_to_dafny(name)
                    )
                })
            })
            .collect::<Vec<_>>();
        declarations.extend(
            ctx.capabilities
                .operations()
                .filter(|operation| !operation.is_effectful() && operation.module == module)
                .map(|operation| {
                    let params = operation
                        .params
                        .iter()
                        .map(|(name, ty)| {
                            format!(
                                "{}: {}",
                                crate::codegen::dafny::expr::aver_name_to_dafny(name),
                                toplevel::emit_type_from(ty)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!(
                        "function {}({}): {}",
                        crate::codegen::dafny::expr::aver_name_to_dafny(&operation.name),
                        params,
                        toplevel::emit_type_from(&operation.return_type)
                    )
                }),
        );
        declarations
    }

    // ProofIR is populated by the ContractLower pipeline stage. Mutual
    // SCC members are exactly the fns whose contract is `Fuel { Lex }`
    // — that's the unifying shape MutualIntCountdown /
    // MutualStringPosAdvance / MutualSizeOfRanked all lower to.
    //
    // Round-6: `fn_contracts` is keyed by canonical `Module.fn` after
    // round-5. The earlier `mutual_planned.contains(&fd.name)` filter
    // (bare) misses every module-owned mutual-recursive SCC. Resolve
    // per-`&FnDef` via pointer-eq scope so module-owned mutual fuel
    // groups land in `mutual_fns_all` correctly.
    let is_mutual_lex = |fd: &FnDef| -> bool {
        crate::codegen::common::find_fn_contract_for_fn(ctx, fd).is_some_and(|c| {
            matches!(
                c.recursion,
                Some(crate::ir::RecursionContract::Fuel {
                    fuel_metric: crate::ir::FuelMetric::Lex { .. },
                })
            )
        })
    };

    let mutual_fns_all: Vec<&FnDef> = ctx
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
        // A fn whose lowered body still carries a `?` has to reach
        // `emit_fn_def_axiom` — and exactly once. Both the fuel and the
        // native-`decreases` emitters lower `?` themselves and would
        // render a body for it here, on top of the axiom
        // `emit_pure_or_axiom` emits below, so keep those fns out of the
        // group selection entirely. Peers left in the SCC still emit as
        // a group; their calls to the excluded fn resolve to its axiom.
        .filter(|fd| is_mutual_lex(fd) && !body_keeps_error_prop_after_lowering(fd))
        .collect();
    let mutual_components =
        crate::call_graph::ordered_fn_components(&mutual_fns_all, &ctx.module_prefixes);

    let mut fuel_per_scope: HashMap<String, Vec<String>> = HashMap::new();
    // Phase E finalization: SCC-membership sets key by opaque
    // `FnId` resolved through the symbol table. Same shape as
    // `ProofIR.fn_contracts` post-#142 — bare-name lookups are gone
    // by construction, so two same-bare-name fns across modules can
    // never collide in these sets even after laws-in-modules lands.
    let mut fuel_emitted: HashSet<crate::ir::FnId> = HashSet::new();
    let mut native_emitted: HashSet<crate::ir::FnId> = HashSet::new();
    let mut axiom_fn_ids: HashSet<crate::ir::FnId> = HashSet::new();

    let insert_fn_ids = |set: &mut HashSet<crate::ir::FnId>, fns: &[&FnDef]| {
        for fd in fns {
            if let Some(id) = crate::codegen::common::fn_id_for_decl(ctx, fd) {
                set.insert(id);
            }
        }
    };

    for component in &mutual_components {
        let scc_fns: Vec<&FnDef> = component.iter().map(|fd| &**fd).collect();
        let scope = scc_fns
            .first()
            .and_then(|fd| crate::codegen::common::fn_owning_scope_for(ctx, fd))
            .map(|s| s.to_string())
            .unwrap_or_default();
        // Mutual SCC emit resolves fn bodies through `emit_fn_body` →
        // `emit_expr_legacy`, which falls back to
        // `ctx.active_module_scope()` when no explicit scope is passed.
        // Wrap the fuel/native dispatch in `with_module_scope` so a
        // module-owned mutual group doesn't resolve as if it were
        // entry-scope (same shape as the pure-fn path in
        // `route_pure_components_per_scope` below).
        let scope_opt = if scope.is_empty() {
            None
        } else {
            Some(scope.as_str())
        };
        ctx.with_module_scope(scope_opt, || {
            // Try native `decreases` tuple first — when every member has a
            // sizeOf-measurable parameter and a classifier rank, the SCC
            // emits as plain mutual functions and proofs over concrete
            // values no longer hit the fuel-encoding's symbolic-unfolding
            // ceiling (BigInt's 10⁹ pairs close as real samples instead of
            // needing the literal-magnitude cutoff). Falls back to fuel
            // when the SCC has a non-sizeOf member.
            if let Some(code) = fuel::emit_mutual_native_decreases_group(&scc_fns, ctx) {
                fuel_per_scope.entry(scope.clone()).or_default().push(code);
                insert_fn_ids(&mut native_emitted, &scc_fns);
            } else {
                match fuel::emit_mutual_fuel_group(&scc_fns, ctx) {
                    Some(code) => {
                        fuel_per_scope.entry(scope.clone()).or_default().push(code);
                        insert_fn_ids(&mut fuel_emitted, &scc_fns);
                    }
                    None => {
                        insert_fn_ids(&mut axiom_fn_ids, &scc_fns);
                    }
                }
            }
        });
    }

    // Self-recursive singletons whose termination no recognized
    // `decreases` pattern justifies (doubling/halving recursion on a
    // rational num/den pair — tests/fixtures/expo_outside_subset.av):
    // guessing a measure emits a function whose own `decreases` fails
    // verification AND a synthesized `requires p >= 0` that breaks
    // every total caller. Route them to the opaque `{:axiom}` form —
    // callers stay wellformed, and laws over them are reported as
    // omitted/unproven instead of erroring. Kept in a SEPARATE set
    // from `axiom_fn_ids` so the law/sample machinery for the
    // pre-existing axiom population (mutual-SCC fallback, `?`-lowering
    // failures) is untouched; this set only (a) switches the fn's own
    // emission to `{:axiom}` and (b) suppresses sample asserts that
    // could never be proved against an opaque body.
    let mut termination_axiom_ids: HashSet<crate::ir::FnId> = HashSet::new();
    {
        let all_pure_fns: Vec<&FnDef> = ctx
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
            .filter(|fd| fd.effects.is_empty() && fd.name != "main")
            .collect();
        for fd in all_pure_fns {
            let Some(id) = crate::codegen::common::fn_id_for_decl(ctx, fd) else {
                continue;
            };
            if fuel_emitted.contains(&id)
                || native_emitted.contains(&id)
                || axiom_fn_ids.contains(&id)
            {
                continue;
            }
            if toplevel::termination_guess_unjustified(fd, ctx) {
                termination_axiom_ids.insert(id);
            }
        }
    }

    let id_in = |set: &HashSet<crate::ir::FnId>, fd: &FnDef| -> bool {
        crate::codegen::common::fn_id_for_decl(ctx, fd).is_some_and(|id| set.contains(&id))
    };
    let emit_pure_or_axiom = |fd: &FnDef| -> String {
        if body_keeps_error_prop_after_lowering(fd) {
            toplevel::emit_fn_def_axiom(fd, ctx)
        } else if id_in(&fuel_emitted, fd) || id_in(&native_emitted, fd) {
            String::new()
        } else if id_in(&axiom_fn_ids, fd) || id_in(&termination_axiom_ids, fd) {
            toplevel::emit_fn_def_axiom(fd, ctx)
        } else {
            toplevel::emit_fn_def(fd, ctx)
        }
    };

    // SCC-route pure fns through the shared per-scope router (each scope
    // independently — same reasoning as Lean). For DAG inputs each
    // component is a singleton emitted via `emit_pure_or_axiom`; the
    // `_or_axiom` half also handles the fuel-emitted/axiom-fallback
    // skip-and-stub cases, so multi-fn SCCs that aren't fuel-handled
    // emit each fn as an axiom and the SCC topology is otherwise
    // ignored at this layer.
    let mut pure_per_scope = crate::codegen::common::route_pure_components_per_scope(
        ctx,
        |fd| fd.effects.is_empty() && fd.name != "main",
        |comp, scope| {
            let scope_opt = if scope.is_empty() { None } else { Some(scope) };
            ctx.with_module_scope(scope_opt, || {
                comp.iter()
                    .map(|fd| emit_pure_or_axiom(fd))
                    .filter(|s| !s.is_empty())
                    .collect()
            })
        },
    );

    let mut module_files: Vec<(String, String)> = Vec::new();
    let mut union_body = String::new();

    // ---- Per-module files (collected into the shared module tree) ----
    for module in &ctx.modules {
        let mut sections = capability_declarations(ctx, &module.prefix);
        ctx.with_module_scope(Some(module.prefix.as_str()), || {
            for td in &module.type_defs {
                if let Some(code) =
                    toplevel::emit_type_def_in_scope(td, ctx, Some(module.prefix.as_str()))
                {
                    sections.push(code);
                }
            }
        });
        sections.extend(pure_per_scope.take(&module.prefix));
        if let Some(fuel) = fuel_per_scope.get(&module.prefix) {
            sections.extend(fuel.clone());
        }
        let body = sections.join("\n");
        let module_uses_crypto_sha256 = body.contains("Aver_Crypto.sha256");
        union_body.push_str(&body);
        union_body.push('\n');

        // Submodules (`Models.User` → `Models/User.dfy`) live inside
        // subdirectories, so `include` paths need `../` prefixes to reach
        // the project root where `common.dfy` and sibling-module files
        // live. Depth = number of segments minus one.
        let depth = module.prefix.chars().filter(|c| *c == '.').count();
        let up = "../".repeat(depth);
        let depends_includes: String = module
            .depends
            .iter()
            .map(|d| {
                format!(
                    "include \"{}{}.dfy\"",
                    up,
                    crate::codegen::common::module_prefix_to_filename(d)
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        let depends_imports: String = module
            .depends
            .iter()
            .map(|d| format!("  import opened {}", dafny_module_name(d)))
            .collect::<Vec<_>>()
            .join("\n");

        let mut header = format!(
            "// Aver-generated module: {}\ninclude \"{}common.dfy\"\n",
            module.prefix, up
        );
        if module_uses_crypto_sha256 {
            header.push_str(&format!("include \"{}Crypto.dfy\"\n", up));
        }
        if !depends_includes.is_empty() {
            header.push_str(&depends_includes);
            header.push('\n');
        }

        let mut module_inner = String::from("  import opened AverCommon\n");
        if module_uses_crypto_sha256 {
            module_inner.push_str("  import opened Aver_Crypto\n");
        }
        if !depends_imports.is_empty() {
            module_inner.push_str(&depends_imports);
            module_inner.push('\n');
        }
        module_inner.push('\n');
        for line in body.lines() {
            if line.is_empty() {
                module_inner.push('\n');
            } else {
                module_inner.push_str("  ");
                module_inner.push_str(line);
                module_inner.push('\n');
            }
        }

        let content = format!(
            "{}\nmodule {} {{\n{}}}\n",
            header,
            dafny_module_name(&module.prefix),
            module_inner
        );
        let path = module.prefix.replace('.', "/");
        module_files.push((format!("{}.dfy", path), content));
    }

    // ---- Entry sections ----
    let mut entry_sections: Vec<String> = Vec::new();
    if let Some(entry_module) = ctx.entry_module_name() {
        let declarations = capability_declarations(ctx, &entry_module);
        if !declarations.is_empty() {
            let body = declarations
                .join("\n")
                .lines()
                .map(|line| format!("  {line}"))
                .collect::<Vec<_>>()
                .join("\n");
            entry_sections.push(format!(
                "module {} {{\n{}\n}}",
                dafny_module_name(&entry_module),
                body
            ));
        }
    }
    for td in &ctx.type_defs {
        if let Some(code) = toplevel::emit_type_def(td, ctx) {
            entry_sections.push(code);
        }
    }
    // Pure fns from entry came out of the shared per-scope router. The
    // closure above already filtered `main` (it has `effects.is_empty()`
    // == false because it lives under `! [...]` in practice; if a `main`
    // ever lands as a pure fn the per-scope router will pick it up like
    // any other and the verify lemmas below will simply not reference it).
    entry_sections.extend(pure_per_scope.take(""));
    if let Some(fuel) = fuel_per_scope.get("") {
        entry_sections.extend(fuel.clone());
    }

    // Lifted effectful fns (entry only — modules don't host effectful fns
    // in the v1 emitter).
    let reachable = crate::codegen::common::verify_reachable_fn_names(&ctx.items);
    let mut helpers: HashMap<String, Vec<String>> = HashMap::new();
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item
            && !fd.effects.is_empty()
            && fd.name != "main"
            && !body_uses_error_prop(&fd.body)
            && reachable.contains(&fd.name)
            && fd.effects.iter().all(|e| {
                crate::types::checker::effect_classification::classify_with_registry(
                    &ctx.capabilities,
                    &e.node,
                )
                .is_some()
            })
        {
            helpers.insert(
                fd.name.clone(),
                fd.effects.iter().map(|e| e.node.clone()).collect(),
            );
        }
    }
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item
            && !fd.effects.is_empty()
            && fd.name != "main"
            && !body_uses_error_prop(&fd.body)
            && reachable.contains(&fd.name)
            && fd.effects.iter().all(|e| {
                crate::types::checker::effect_classification::classify_with_registry(
                    &ctx.capabilities,
                    &e.node,
                )
                .is_some()
            })
            && let Ok(Some(lifted)) =
                crate::types::checker::effect_lifting::lift_fn_def_with_helpers_and_registry(
                    fd,
                    &helpers,
                    &ctx.capabilities,
                )
        {
            entry_sections.push(toplevel::emit_fn_def(&lifted, ctx));
        }
    }

    // Verify lemmas
    let mut law_counter: HashMap<String, usize> = HashMap::new();
    for item in &ctx.items {
        if let TopLevel::Verify(vb) = item
            && let VerifyKind::Law(law) = &vb.kind
        {
            let count = law_counter.entry(vb.fn_name.clone()).or_insert(0);
            *count += 1;
            let suffix = if *count > 1 {
                format!("_{}", count)
            } else {
                String::new()
            };
            let direct_opaque: HashSet<crate::ir::FnId> =
                axiom_fn_ids.union(&fuel_emitted).copied().collect();
            let opaque_fns = toplevel::transitive_opaque_closure(ctx, &direct_opaque);
            // Native mutual-rec members + their transitive callers
            // also need bounded-∀ universal (true ∀ over int doesn't
            // close even with native decreases) — but stays separate
            // from opaque so per-sample bodies on the native path
            // skip the fuel-magnitude cutoff.
            let native_transitive = toplevel::transitive_opaque_closure(ctx, &native_emitted);
            // Truly opaque `{:axiom}` fns from the termination-decline
            // path (and their transitive callers): nothing about their
            // value is provable, so sample asserts/lemmas over them
            // could only fail. Suppressed with a marker comment below.
            let termination_opaque =
                toplevel::transitive_opaque_closure(ctx, &termination_axiom_ids);
            if !vb.cases.is_empty()
                && let Some(code) = toplevel::emit_law_samples(
                    vb,
                    law,
                    ctx,
                    &suffix,
                    &opaque_fns,
                    &fuel_emitted,
                    &native_transitive,
                    &termination_opaque,
                )
            {
                entry_sections.push(code);
            }
            entry_sections.push(toplevel::emit_verify_law(
                vb,
                law,
                ctx,
                &opaque_fns,
                &native_transitive,
                &termination_opaque,
                &suffix,
            ));
        }
    }

    let entry_body = entry_sections.join("\n");
    union_body.push_str(&entry_body);
    union_body.push('\n');
    let uses_crypto_sha256 = union_body.contains("Aver_Crypto.sha256");

    let entry_includes: String = ctx
        .modules
        .iter()
        .map(|m| {
            format!(
                "include \"{}.dfy\"",
                crate::codegen::common::module_prefix_to_filename(&m.prefix)
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let entry_name = crate::codegen::common::entry_basename(ctx);
    let mut entry_parts: Vec<String> = vec![format!(
        "// Aver-generated entry: {}\ninclude \"common.dfy\"\n{}{}",
        entry_name,
        if uses_crypto_sha256 {
            "include \"Crypto.dfy\"\n"
        } else {
            ""
        },
        entry_includes
    )];
    // Open every dependent module + AverCommon so unqualified type names
    // (`Point`, `Tile`) and helpers stay in scope at the top level.
    let mut opens = vec!["import opened AverCommon".to_string()];
    if uses_crypto_sha256 {
        opens.push("import opened Aver_Crypto".to_string());
    }
    for m in &ctx.modules {
        opens.push(format!("import opened {}", dafny_module_name(&m.prefix)));
    }
    if ctx.capabilities.contract(&entry_name).is_some() {
        // An entry capability's representation-less resources live in their
        // own `Aver_<Module>` namespace, just like dependency capabilities.
        // Hostile profiles and lifted wrapper functions are emitted in Dafny's
        // default module, so open the capability namespace there to make the
        // source-faithful bare annotation (`Token`) resolve to the same opaque
        // type as the canonical oracle parameter (`Aver_Mint.Token`).
        opens.push(format!("import opened {}", dafny_module_name(&entry_name)));
    }
    entry_parts.push(opens.join("\n"));
    let declared = crate::codegen::common::collect_declared_effects(ctx);
    let has_ip = union_body.contains("BranchPath");
    let has_classified =
        crate::types::checker::effect_classification::classifications_for_proof_subset()
            .iter()
            .any(|c| declared.includes(c.method))
            || ctx.capabilities.operations().any(|operation| {
                operation.is_effectful() && declared.includes(&operation.canonical_name)
            });
    if has_ip || has_classified || ctx.capabilities.contracts().next().is_some() {
        entry_parts.push(
            crate::types::checker::proof_trust_header::generate_commented_with_registry(
                "// ",
                &declared,
                has_ip,
                &ctx.capabilities,
            ),
        );
    }
    let subtype_block = crate::types::checker::oracle_subtypes::dafny_subtype_predicates(&declared);
    if !subtype_block.is_empty() {
        // Fold subtype block into the union body BEFORE computing
        // `needed_helpers` — the Oracle subtype block introduces
        // `BranchPath` references (e.g. `predicate IsTimeUnixMsNonneg(
        // f: (BranchPath, int) -> int)`) for files that declare
        // classified effects but never spell `BranchPath` in user
        // code. Without this, common.dfy misses the `datatype
        // BranchPath` block and Main.dfy fails verification with
        // `Type or type parameter is not declared in this scope:
        // BranchPath`.
        union_body.push_str(&subtype_block);
        union_body.push('\n');
        entry_parts.push(subtype_block);
    }
    entry_parts.push(entry_body);
    let entry_content = entry_parts.join("\n\n");

    // ---- common.dfy ----
    let common_content = build_common_dafny(&union_body);

    let mut files = module_files;
    files.push((format!("{}.dfy", entry_name), entry_content));
    files.push(("common.dfy".to_string(), common_content));
    if uses_crypto_sha256 {
        files.push(("Crypto.dfy".to_string(), crypto::SOURCE.to_string()));
        files.push((
            "Crypto/Sha256Core.dfy".to_string(),
            crypto::CORE_SOURCE.to_string(),
        ));
    }
    ProjectOutput { files }
}

fn build_common_dafny(union_body: &str) -> String {
    let mut sections: Vec<String> = vec![
        "// Aver-generated shared library: built-in records and helpers".to_string(),
        "module AverCommon {".to_string(),
        DAFNY_PRELUDE_HEAD.to_string(),
    ];
    for record in crate::codegen::builtin_records::needed_records(union_body, false) {
        sections.push(crate::codegen::builtin_records::render_dafny(record));
    }
    sections.push(DAFNY_PRELUDE_CORE_HELPERS.to_string());
    for helper in crate::codegen::builtin_helpers::needed_helpers(union_body, false) {
        if let Some(block) = dafny_helper_block(helper.key) {
            sections.push(block.to_string());
        }
    }
    sections.push("}".to_string());
    sections.join("\n")
}

/// The Dafny text a builtin-helper key contributes to `common.dfy`, or `None`
/// for the keys that are Lean-only (Dafny gets those from native constructs).
fn dafny_helper_block(key: &str) -> Option<&'static str> {
    match key {
        "BranchPath" => Some(DAFNY_HELPER_BRANCH_PATH),
        "AverList" => Some(DAFNY_HELPER_AVER_LIST),
        "StringHelpers" => Some(DAFNY_HELPER_STRING_HELPERS),
        "NumericParse" => Some(DAFNY_HELPER_NUMERIC_PARSE),
        "CharCode" => Some(DAFNY_HELPER_CHAR),
        "AverBits" => Some(DAFNY_HELPER_BITS),
        "AverMap" => Some(DAFNY_HELPER_AVER_MAP),
        "AverMeasure" | "ProofFuel" => None,
        "FloatInstances" | "ExceptInstances" | "StringHadd" => None,
        "ResultDatatype" => Some(DAFNY_HELPER_RESULT_DATATYPE),
        "OptionDatatype" => Some(DAFNY_HELPER_OPTION_DATATYPE),
        "OptionToResult" => Some(DAFNY_HELPER_OPTION_TO_RESULT),
        "BranchPathDatatype" => Some(DAFNY_HELPER_BRANCH_PATH_DATATYPE),
        other => panic!(
            "Dafny backend has no implementation for builtin helper key '{}'. \
             Add a match arm in dafny_helper_block or remove the key from BUILTIN_HELPERS.",
            other
        ),
    }
}

const DAFNY_PRELUDE_HEAD: &str = r#"// --- Prelude: standard types and helpers ---
"#;

const DAFNY_HELPER_RESULT_DATATYPE: &str = r#"
datatype Result<T, E> = Ok(value: T) | Err(error: E)

function ResultWithDefault<T, E>(r: Result<T, E>, d: T): T {
  match r
  case Ok(v) => v
  case Err(_) => d
}
"#;

const DAFNY_HELPER_OPTION_DATATYPE: &str = r#"
datatype Option<T> = None | Some(value: T)

function OptionWithDefault<T>(o: Option<T>, d: T): T {
  match o
  case Some(v) => v
  case None => d
}
"#;

const DAFNY_HELPER_OPTION_TO_RESULT: &str = r#"
function OptionToResult<T, E>(o: Option<T>, err: E): Result<T, E> {
  match o
  case Some(v) => Result.Ok(v)
  case None => Result.Err(err)
}
"#;

const DAFNY_HELPER_BRANCH_PATH_DATATYPE: &str = r#"
// Oracle v1: BranchPath is the proof-side representation of a position
// in the structural tree of `!`/`?!` groups. Dewey-decimal under the hood
// ("", "0", "2.0", …); constructors mirror the Aver-source BranchPath
// opaque builtin (`.root`, `.child`, `.parse`) so the lifted bodies can
// reference them directly without case-splitting at the call site.

datatype BranchPath = BranchPath(dewey: string)
"#;

/// Universal `ToString<T>` opaque — small (1 line), used by interpolation
/// machinery in many shapes, kept always-on to avoid token-detection edge
/// cases for things like `ToString(x)` showing up in nested type args.
const DAFNY_PRELUDE_CORE_HELPERS: &str = r#"
function ToString<T>(v: T): string
"#;

/// `BranchPath` constructors. Emitted only when the body uses Oracle
/// lifting (any `BranchPath` reference); pure-math files don't need
/// them. Note `BranchPath_child` calls `IntToString`, so when this
/// helper is included the StringHelpers piece must come along too —
/// that's enforced via `BUILTIN_HELPERS::depends_on` for `BranchPath`
/// pulling in `NumericParse` (whose tokens cover `IntToString`).
const DAFNY_HELPER_BRANCH_PATH: &str = r#"
const BranchPath_Root: BranchPath := BranchPath("")

function BranchPath_child(p: BranchPath, idx: int): BranchPath
  requires idx >= 0
{
  if |p.dewey| == 0 then BranchPath(IntToString(idx))
  else BranchPath(p.dewey + "." + IntToString(idx))
}

function BranchPath_parse(s: string): BranchPath {
  BranchPath(s)
}
"#;

/// `ListFind` and `ListAny` are here for a rendering the emitter can
/// produce but no source file can reach today: `List.find` / `List.any` are
/// in the codegen name table, so `emit_dafny_builtin` writes
/// `ListFind(xs, p)`, while the typechecker registers no signature for
/// either and rejects the call before codegen runs. Declaring them costs
/// two definitions and removes the state #881 is about — a rendering with
/// nowhere to resolve to — instead of leaving it parked behind an
/// unreachability argument that stops holding the moment either name gets a
/// signature. They mirror Lean's model of the same two builtins
/// (`codegen::lean::builtins` renders `xs.find? p` and `xs.any p`): first
/// match wins, and `ListAny` is the existential, false on the empty list.
const DAFNY_HELPER_AVER_LIST: &str = r#"
function ListReverse<T>(xs: seq<T>): seq<T>
  decreases |xs|
{
  if |xs| == 0 then []
  else ListReverse(xs[1..]) + [xs[0]]
}

function ListHead<T>(xs: seq<T>): Option<T> {
  if |xs| == 0 then None
  else Some(xs[0])
}

function ListTail<T>(xs: seq<T>): seq<T> {
  if |xs| == 0 then []
  else xs[1..]
}

function ListTake<T>(xs: seq<T>, n: int): seq<T> {
  if n <= 0 then []
  else if n >= |xs| then xs
  else xs[..n]
}

function ListDrop<T>(xs: seq<T>, n: int): seq<T> {
  if n <= 0 then xs
  else if n >= |xs| then []
  else xs[n..]
}

function ListZip<A, B>(xs: seq<A>, ys: seq<B>): seq<(A, B)>
  decreases |xs|
{
  if |xs| == 0 || |ys| == 0 then []
  else [(xs[0], ys[0])] + ListZip(xs[1..], ys[1..])
}

function ListFind<T>(xs: seq<T>, p: T -> bool): Option<T>
  decreases |xs|
{
  if |xs| == 0 then None
  else if p(xs[0]) then Some(xs[0])
  else ListFind(xs[1..], p)
}

function ListAny<T>(xs: seq<T>, p: T -> bool): bool
  decreases |xs|
{
  if |xs| == 0 then false
  else p(xs[0]) || ListAny(xs[1..], p)
}
"#;

/// `MapEntries` is declared with a signature and no body on purpose. Dafny's
/// `map` is unordered and this declaration is generic in the key type, so
/// there is no expression here that reproduces the runtime's key-sorted
/// sequence (`types/map.rs::compare_scalar_keys`); leaving it uninterpreted
/// commits the model to nothing about the order, which makes a law that names
/// the sequence itself simply not provable rather than provable-and-wrong.
/// Nothing about the order is assumed either — there is no postcondition and
/// no `{:axiom}` here, so the only facts the verifier has about a map's
/// iteration are the ones the definitions below give it.
///
/// `MapKeys` and `MapValues` are the two projections of that one sequence,
/// which is the part of the runtime's behaviour this model *can* express.
/// Declaring them as three unrelated uninterpreted functions would let the
/// verifier work in a world where a map's keys and values have different
/// lengths, or where `keys[i]` does not belong with `values[i]` — a state the
/// runtime cannot be in, and the exact divergence that compiled Rust was
/// found in when `Map.values` walked the hash map while `Map.keys` beside it
/// sorted. Reading all three off one sequence rules that out, and does it
/// without claiming what the order is.
///
/// All three must be listed in the `AverMap` helper's `body_tokens`
/// (`codegen::builtin_helpers`), or a program that uses only `Map.keys` gets
/// no map helper block at all and the emitted file does not resolve.
const DAFNY_HELPER_AVER_MAP: &str = r#"
function MapGet<K, V>(m: map<K, V>, k: K): Option<V> {
  if k in m then Some(m[k])
  else None
}

function MapEntries<K, V>(m: map<K, V>): seq<(K, V)>

function MapKeys<K, V>(m: map<K, V>): seq<K> {
  var entries := MapEntries(m);
  seq(|entries|, i requires 0 <= i < |entries| => entries[i].0)
}

function MapValues<K, V>(m: map<K, V>): seq<V> {
  var entries := MapEntries(m);
  seq(|entries|, i requires 0 <= i < |entries| => entries[i].1)
}

function MapFromList<K, V>(entries: seq<(K, V)>): map<K, V>
  decreases |entries|
{
  if |entries| == 0 then map[]
  else MapFromList(entries[..|entries|-1])[entries[|entries|-1].0 := entries[|entries|-1].1]
}
"#;

/// `StringHelpers` covers the opaque/ish string utilities. Note Dafny
/// has no AverDigits namespace; the numeric `IntToString`/`FromString`/
/// `FloatToString`/`FromString` opaques live under the `NumericParse`
/// helper key alongside Lean's parsing namespace, since the body-token
/// detection is shared.
const DAFNY_HELPER_STRING_HELPERS: &str = r#"
function StringCharAt(s: string, i: int): Option<string> {
  if 0 <= i < |s| then Option.Some([s[i]]) else Option.None
}

function StringChars(s: string): seq<string> {
  seq(|s|, (i: int) requires 0 <= i < |s| => [s[i]])
}

function StringSlice(s: string, from_: int, to_: int): string
{
  var lo := if from_ < 0 then 0 else if from_ > |s| then |s| else from_;
  var hi := if to_ < 0 then 0 else if to_ > |s| then |s| else to_;
  if lo >= hi then "" else s[lo..hi]
}

function StringJoin(sep: string, parts: seq<string>): string
  decreases |parts|
{
  if |parts| == 0 then ""
  else if |parts| == 1 then parts[0]
  else parts[0] + sep + StringJoin(sep, parts[1..])
}

function StringSplit(s: string, sep: string): seq<string>
function StringContains(s: string, sub: string): bool
function StringStartsWith(s: string, prefix: string): bool
function StringEndsWith(s: string, suffix: string): bool
function StringTrim(s: string): string
function StringReplace(s: string, from_: string, to_: string): string
function StringToUpper(s: string): string
function StringToLower(s: string): string
function StringFromBool(b: bool): string
function StringByteLength(s: string): int

function ListReverseStr(xs: seq<string>): seq<string>
"#;

const DAFNY_HELPER_NUMERIC_PARSE: &str = r#"
function IntToString(n: int): string
function IntFromString(s: string): Result<int, string>
function FloatToString(r: real): string
function FloatFromString(s: string): Result<real, string>
function FloatPi(): real
function FloatSqrt(r: real): real
function FloatPow(base: real, exp: real): real
function FloatToInt(r: real): int
function FloatSin(r: real): real
function FloatCos(r: real): real
function FloatAtan2(y: real, x: real): real

function FloatDiv(a: real, b: real): real
{
  if b == 0.0 then 0.0 else a / b
}
"#;

const DAFNY_HELPER_CHAR: &str = r#"
function CharToCode(c: string): int
function CharFromCode(n: int): Option<string>
"#;

/// The `Bits` namespace's proof model — the Dafny mirror of Lean's
/// `AverBits` prelude, deliberately the same shape so the two provers can be
/// read against each other.
///
/// Dafny has no bitwise operators on `int`; they exist only on fixed-width
/// `bv` types. Translating an unbounded `Int` to a bit-vector would silently
/// impose a width, so these are DEFINED instead: `NatBit*` recurse on the
/// binary expansion of a natural (terminating because each step halves both
/// operands), and the signed wrappers case-split on the two sign tails using
/// the two's-complement magnitude `BitsMag x = if x < 0 then -x-1 else x`.
/// Nothing here is opaque or uninterpreted.
///
/// The shifts are the specification written out. Dafny's `/` and `%` on
/// `int` are Euclidean, and Euclidean division by a positive divisor is
/// floor division — hence the `ensures BitsPow2(n) > 0`, which is both what
/// makes `shiftRight` arithmetic and what discharges the division-by-zero
/// obligation at every call site.
const DAFNY_HELPER_BITS: &str = r#"
function NatBitAnd(a: nat, b: nat): nat
  decreases a + b
{
  if a == 0 || b == 0 then 0
  else 2 * NatBitAnd(a / 2, b / 2) + (if a % 2 == 1 && b % 2 == 1 then 1 else 0)
}

function NatBitOr(a: nat, b: nat): nat
  decreases a + b
{
  if a == 0 then b else if b == 0 then a
  else 2 * NatBitOr(a / 2, b / 2) + (if a % 2 == 1 || b % 2 == 1 then 1 else 0)
}

function NatBitXor(a: nat, b: nat): nat
  decreases a + b
{
  if a == 0 then b else if b == 0 then a
  else 2 * NatBitXor(a / 2, b / 2) + (if a % 2 != b % 2 then 1 else 0)
}

function BitsMag(x: int): nat { if x < 0 then -x - 1 else x }

function BitsAnd(a: int, b: int): int {
  var x: int := BitsMag(a);
  var y: int := BitsMag(b);
  var both: int := NatBitAnd(BitsMag(a), BitsMag(b));
  if a < 0 then
    (if b < 0 then -(NatBitOr(BitsMag(a), BitsMag(b)) as int) - 1 else y - both)
  else
    (if b < 0 then x - both else both)
}

function BitsOr(a: int, b: int): int {
  var x: int := BitsMag(a);
  var y: int := BitsMag(b);
  var both: int := NatBitAnd(BitsMag(a), BitsMag(b));
  if a < 0 then
    (if b < 0 then -both - 1 else -(x - both) - 1)
  else
    (if b < 0 then -(y - both) - 1 else NatBitOr(BitsMag(a), BitsMag(b)))
}

function BitsXor(a: int, b: int): int {
  var d: int := NatBitXor(BitsMag(a), BitsMag(b));
  if (a < 0) == (b < 0) then d else -d - 1
}

function BitsNot(a: int): int { -a - 1 }

function BitsPow2(n: int): int
  ensures BitsPow2(n) > 0
  decreases if n < 0 then 0 else n
{
  if n <= 0 then 1 else 2 * BitsPow2(n - 1)
}

function BitsShiftLeft(x: int, n: int): int { x * BitsPow2(n) }
function BitsShiftRight(x: int, n: int): int { x / BitsPow2(n) }
function BitsLow(x: int, w: int): int { x % BitsPow2(w) }
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::build_context;
    use crate::source::parse_source;

    fn ctx_from_source(src: &str, project_name: &str) -> CodegenContext {
        let mut items = parse_source(src).expect("parse");
        // Proof-mode minimal pipeline — same shape as `lean::tests::
        // ctx_from_source`; see that for why every rewriting stage is
        // off (resolve / escape / interp_lower / buffer_build / last_use
        // would alter source-level recursion shapes the classifier
        // matches against).
        let mut pipeline_result = crate::ir::pipeline::run(
            &mut items,
            crate::ir::PipelineConfig {
                run_tco: true,
                typecheck: Some(crate::ir::TypecheckMode::Full { base_dir: None }),
                run_interp_lower: false,
                run_buffer_build: false,
                run_chars_fusion: false,
                run_list_build: false,
                run_resolve: false,
                run_last_use: false,
                run_analyze: true,
                run_escape: false,
                run_refinement_lower: true,
                run_interval_analyze: false,
                run_contract_lower: true,
                run_law_lower: true,
                // BuildSymbols is needed for fn_contracts lookup
                // (keyed by opaque FnId resolved through the symbol
                // table since the FnKey → FnId migration).
                run_build_symbols: true,
                dep_modules: &[],
                alloc_policy: None,
                call_ctx: None,
                on_after_pass: None,
            },
        );
        let tc = pipeline_result
            .typecheck
            .take()
            .expect("typecheck requested");
        assert!(
            tc.errors.is_empty(),
            "source should typecheck: {:?}",
            tc.errors
        );
        let proof_ir = pipeline_result.proof_ir.take();
        // Assemble through `codegen_view`, the way every proof-facing
        // caller does — see `crate::ir::AstView`.
        let view = pipeline_result.codegen_view(items);
        let mut ctx = build_context(
            view.items,
            &tc,
            view.analysis.as_ref(),
            project_name.to_string(),
            vec![],
            view.symbol_table,
            view.resolved_items,
        );
        if let Some(ir) = proof_ir {
            ctx.proof_ir = ir;
        }
        ctx
    }

    /// Concatenate every emitted `.dfy` source. The unified emitter
    /// splits the program into entry / per-module / `common.dfy`
    /// regardless of how many user modules a source has, so legacy
    /// substring assertions need to look across all generated files.
    fn dafny_output(out: &ProjectOutput) -> String {
        out.files
            .iter()
            .filter_map(|(name, content)| name.ends_with(".dfy").then_some(content.as_str()))
            .collect::<Vec<&str>>()
            .join("\n")
    }

    #[test]
    fn prelude_emits_branch_path_only_when_used() {
        // Pure fn — body has no BranchPath, so neither the datatype
        // declaration nor the constructor helpers are emitted.
        let src = "module M\n    intent = \"t\"\n\nfn pure(x: Int) -> Int\n    x\n";
        let ctx = ctx_from_source(src, "m");
        let out = transpile(&ctx);
        let dfy = dafny_output(&out);
        assert!(!dfy.contains("datatype BranchPath"));
        assert!(!dfy.contains("const BranchPath_Root"));
        assert!(!dfy.contains("function BranchPath_child"));
        assert!(!dfy.contains("function BranchPath_parse"));

        // Effectful fn with a verify block — Oracle lifting reaches the
        // proof body and introduces `BranchPath` references, pulling in
        // both the datatype declaration and the constructor helpers.
        let src_eff = "module M\n    intent = \"t\"\n\n\
                       fn rollMax(path: BranchPath, n: Int, lo: Int, hi: Int) -> Int\n    hi\n\n\
                       fn roll() -> Int\n    ! [Random.int]\n    Random.int(1, 6)\n\n\
                       verify roll law alwaysSix\n    given rnd: Random.int = [rollMax]\n    roll() => 6\n";
        let ctx_eff = ctx_from_source(src_eff, "m");
        let out_eff = transpile(&ctx_eff);
        let dfy_eff = dafny_output(&out_eff);
        assert!(dfy_eff.contains("datatype BranchPath"));
        assert!(dfy_eff.contains("const BranchPath_Root"));
        assert!(dfy_eff.contains("function BranchPath_child"));
        assert!(dfy_eff.contains("function BranchPath_parse"));
    }

    #[test]
    fn effectful_generative_fn_emits_lifted_form() {
        // Plan Example 3 analog: pickOne() ! [Random.int] Random.int(1, 6).
        // Verify block makes pickOne reachable — without it the proof
        // backend skips the fn (nothing to prove about it).
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn pickOne() -> Int\n\
             \x20   ! [Random.int]\n\
             \x20   Random.int(1, 6)\n\
             verify pickOne\n\
             \x20   pickOne() => 1\n";
        let ctx = ctx_from_source(src, "m");
        let out = transpile(&ctx);
        let dfy = dafny_output(&out);
        // Signature carries the lifted params.
        assert!(
            dfy.contains("function pickOne(path: BranchPath"),
            "missing path param:\n{}",
            dfy
        );
        assert!(
            dfy.contains("rnd_Random_int"),
            "missing oracle param:\n{}",
            dfy
        );
        // Body calls oracle with threaded path + counter 0.
        assert!(
            dfy.contains("rnd_Random_int(path, 0, 1, 6)"),
            "missing oracle call:\n{}",
            dfy
        );
    }

    #[test]
    fn pure_functions_still_emit_as_before() {
        // Sanity: pure fn continues to come out of the regular path — no
        // spurious path / oracle params prepended.
        let src = "module M\n    intent = \"t\"\n\nfn double(x: Int) -> Int\n    x + x\n";
        let ctx = ctx_from_source(src, "m");
        let out = transpile(&ctx);
        let dfy = dafny_output(&out);
        assert!(dfy.contains("function double(x: int): int"));
        assert!(!dfy.contains("function double(path: BranchPath"));
    }

    #[test]
    fn effectful_fn_with_unclassified_effect_is_still_skipped() {
        // Env.set is ambient stateful — not in the v1 proof subset (process
        // env is global and read-after-write depends on the whole ambient
        // map, not a per-call oracle). The fn must not appear in the emitted
        // Dafny output.
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn configure(key: String, value: String) -> Unit\n\
             \x20   ! [Env.set]\n\
             \x20   Env.set(key, value)\n";
        let ctx = ctx_from_source(src, "m");
        let out = transpile(&ctx);
        let dfy = dafny_output(&out);
        assert!(
            !dfy.contains("function configure"),
            "stateful effectful fn should be skipped; got:\n{}",
            dfy
        );
    }

    #[test]
    fn bang_product_emits_lifted_tuple_with_child_paths() {
        // Plain `!` lifts to a tuple in the emitted Dafny — the parallel
        // claim is captured by the meta-level schedule-invariance
        // invariant. Verifies that each branch threads BranchPath.child
        // and resets its counter to 0. Verify block makes `pair`
        // reachable for the proof backend.
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn pair() -> Tuple<Int, Int>\n\
             \x20   ! [Random.int]\n\
             \x20   (Random.int(1, 6), Random.int(1, 6))!\n\
             verify pair\n\
             \x20   pair() => (1, 1)\n";
        let ctx = ctx_from_source(src, "m");
        let out = transpile(&ctx);
        let dfy = dafny_output(&out);
        assert!(
            dfy.contains("BranchPath_child(path, 0)"),
            "branch 0 path missing:\n{}",
            dfy
        );
        assert!(
            dfy.contains("BranchPath_child(path, 1)"),
            "branch 1 path missing:\n{}",
            dfy
        );
    }

    #[test]
    fn branch_path_call_renders_with_underscore_names() {
        // Verify the expression-emission bridge: Aver-source BranchPath
        // constructor calls map onto the Dafny underscore-named helpers.
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn mkPath() -> BranchPath\n\
             \x20   BranchPath.child(BranchPath.Root, 2)\n";
        let ctx = ctx_from_source(src, "m");
        let out = transpile(&ctx);
        let dfy = dafny_output(&out);
        assert!(
            dfy.contains("BranchPath_child(BranchPath_Root, 2)"),
            "expected underscore-form call; got:\n{}",
            dfy
        );
    }

    #[test]
    fn int_countdown_guarded_emits_requires_clause() {
        // The `match n { 0 -> 0; _ -> down(n - 1) }` shape is the Lean
        // native-guarded target; on the Dafny side it lands on the
        // single-fn `emit_fn_def` path whose `infer_decreases` already
        // produces `requires n >= 0` + `decreases n`. This test pins the
        // existing Dafny shape so the Lean refactor doesn't accidentally
        // route this fn through a fuel/axiom path on the Dafny side.
        let src = "module M\n\
             \x20   intent = \"t\"\n\
             \n\
             fn down(n: Int) -> Int\n\
             \x20   match n\n\
             \x20       0 -> 0\n\
             \x20       _ -> down(n - 1)\n";
        let ctx = ctx_from_source(src, "m");
        let out = transpile(&ctx);
        let dfy = dafny_output(&out);
        assert!(
            dfy.contains("function down(n: int): int"),
            "expected native Dafny function for countdown, got:\n{}",
            dfy
        );
        assert!(
            dfy.contains("requires n >= 0"),
            "expected `requires n >= 0` clause, got:\n{}",
            dfy
        );
        assert!(
            dfy.contains("decreases n"),
            "expected `decreases n` clause, got:\n{}",
            dfy
        );
        assert!(
            !dfy.contains("down__fuel"),
            "should not emit fuel helper for native shape, got:\n{}",
            dfy
        );
    }

    /// The name the Dafny resolver has to find for every call in `text`: the
    /// leftmost segment of the call target, so `MapKeys(m)` asks for
    /// `MapKeys` and `Result<int, string>.Err(e)` asks for `Result`.
    fn dafny_call_targets(text: &str) -> Vec<String> {
        let chars: Vec<char> = text.chars().collect();
        let is_ident = |c: char| c.is_alphanumeric() || c == '_';
        let ident_start = |end: usize| {
            let mut start = end;
            while start > 0 && is_ident(chars[start - 1]) {
                start -= 1;
            }
            start
        };
        let mut targets = Vec::new();
        for (open, ch) in chars.iter().enumerate() {
            if *ch != '(' {
                continue;
            }
            let mut end = open;
            let mut start = ident_start(end);
            if start == end {
                // A parenthesised group, not a call.
                continue;
            }
            // Step left over `.member` and any `<...>` instantiation between
            // the two, until the segment the resolver looks up first.
            while start > 0 && chars[start - 1] == '.' {
                let mut at = start - 1;
                if at > 0 && chars[at - 1] == '>' {
                    let mut depth = 0usize;
                    while at > 0 {
                        at -= 1;
                        match chars[at] {
                            '>' => depth += 1,
                            '<' => {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                let qualifier = ident_start(at);
                if qualifier == at {
                    break;
                }
                start = qualifier;
                end = at;
            }
            targets.push(chars[start..end].iter().collect::<String>());
        }
        targets
    }

    /// Does `block` declare `name` — as a function, a datatype or one of its
    /// constructors, or a module?
    fn dafny_declares(block: &str, name: &str) -> bool {
        [
            "function ",
            "predicate ",
            "lemma ",
            "method ",
            "datatype ",
            "module ",
            "type ",
            "const ",
            "= ",
            "| ",
        ]
        .iter()
        .any(|prefix| {
            let needle = format!("{}{}", prefix, name);
            block.match_indices(&needle).any(|(at, hit)| {
                !matches!(
                    block[at + hit.len()..].chars().next(),
                    Some(c) if c.is_alphanumeric() || c == '_'
                )
            })
        })
    }

    /// The builtins that have a name in the codegen table — so
    /// `emit_dafny_builtin` renders them — but no signature the typechecker
    /// registers, so no source file that reaches codegen can name one.
    /// `check` rejects `List.find(xs, isBig)` with "Unknown member
    /// 'List.find' (not exposed or missing)" before any backend runs, and
    /// the proof backends only ever see a program that type-checked.
    ///
    /// This is the "provably unreachable" half of #881's dichotomy, and the
    /// guard below asserts it rather than assuming it: the list has to match
    /// the uncallable names exactly, so a builtin that gains a surface
    /// signature — or a new name added to the codegen table with no
    /// signature — lands here for a decision instead of slipping past.
    const UNCALLABLE_BUILTINS: &[&str] = &["List.head", "List.tail", "List.find", "List.any"];

    /// The dotted names a source program can call, as the typechecker sees
    /// them. `TypeCheckResult::fn_sigs` flattens the builtin signature table
    /// (`types::checker::builtins`) together with the program's own
    /// functions, so a module that declares none leaves exactly the builtin
    /// surface behind.
    fn typechecker_builtin_surface() -> std::collections::HashSet<String> {
        let src = "module Probe\n    intent = \"t\"\n    effects []\n";
        let items = parse_source(src).expect("parse");
        let result = crate::types::checker::run_type_check_full(&items, None);
        assert!(
            result.errors.is_empty(),
            "probe module should type-check: {:?}",
            result.errors
        );
        result.fn_sigs.into_keys().collect()
    }

    /// Every builtin the Dafny emitter can render has to resolve in the file
    /// it writes, and two lists have to agree for that to hold: what the
    /// emitter renders the builtin as, and which prelude blocks `common.dfy`
    /// carries — which is decided by matching call forms against the emitted
    /// body (`codegen::builtin_helpers`). `Map.keys` fell out of both at once:
    /// `MapKeys(m)` was declared nowhere and matched no trigger, so `dafny
    /// verify` stopped at `unresolved identifier: MapKeys` before checking a
    /// single obligation (#881), while `aver proof` reported success.
    /// `List.zip` sat in the same state behind it, and nothing was watching
    /// the two lists for drift.
    ///
    /// The set walked here is `Builtin::ALL` — every variant
    /// `emit_dafny_builtin` matches on, which is also every name
    /// `recognize_builtin` accepts, because the enum and the name table are
    /// one list. That is the set the emitter can *produce*; reading a
    /// different table (`VmBuiltin::ALL`, say, which has no entry for the
    /// `Result.Ok` constructors or for `List.find`) silently excuses exactly
    /// the renderings most likely to have been forgotten.
    ///
    /// LIMITATION: each builtin is rendered with placeholder arguments
    /// `a0`..`a3`, so this sees name resolution only. A resolution failure
    /// that depends on the *shape* of an argument stays invisible — most
    /// sharply, an empty list literal passed to a generic helper:
    /// `pairCount([], [])` emits `|ListZip([], [])|` and Dafny answers "type
    /// parameter 'A' (inferred to be '?') could not be determined". Giving
    /// the placeholders types would not catch it either; only emitting from
    /// real call sites would, which is what the `--check` export tests in
    /// `proof_spec` do for the fixtures they carry.
    #[test]
    fn every_builtin_the_emitter_can_render_resolves_in_the_dafny_prelude() {
        let args: Vec<String> = (0..4).map(|i| format!("a{}", i)).collect();
        let mut covered: Vec<(&str, String)> = Vec::new();
        for builtin in crate::codegen::builtins::Builtin::ALL {
            let name = builtin.name();
            let emitted = super::expr::emit_dafny_builtin(*builtin, &args);
            let mut visible: Vec<&str> = vec![DAFNY_PRELUDE_CORE_HELPERS];
            for helper in crate::codegen::builtin_helpers::needed_helpers(&emitted, false) {
                if let Some(block) = dafny_helper_block(helper.key) {
                    visible.push(block);
                }
            }
            if emitted.contains("Aver_Crypto.") {
                // Emitted as its own file, on the same `contains` condition
                // `build_project` uses.
                visible.push(crypto::SOURCE);
            }
            for target in dafny_call_targets(&emitted) {
                if target == "seq" {
                    // Dafny's own sequence constructor.
                    continue;
                }
                assert!(
                    visible.iter().any(|block| dafny_declares(block, &target)),
                    "`{}` emits `{}`, and no helper block the emitted file pulls in \
                     declares `{}`. Dafny stops at an unresolved identifier before it \
                     checks a single obligation. Declare it in a helper block, and list \
                     the call form in that helper's `body_tokens` so the block is \
                     actually included.",
                    name,
                    emitted,
                    target
                );
                covered.push((name, target));
            }
        }
        // The names #881 is about have to be among the ones checked, or this
        // test passes by looking at nothing.
        for (surface, emitted) in [
            ("Map.keys", "MapKeys"),
            ("List.zip", "ListZip"),
            ("List.find", "ListFind"),
            ("List.any", "ListAny"),
            // A constructor: the shape `VmBuiltin::ALL` had no entry for.
            ("Result.Ok", "Result"),
        ] {
            assert!(
                covered.iter().any(|(n, t)| *n == surface && t == emitted),
                "`{}` must reach this check as `{}` — it did not, so the check is \
                 vacuous for the builtins it was written for. Covered: {:?}",
                surface,
                emitted,
                covered
            );
        }

        // The other half of #881's dichotomy: every name that is NOT callable
        // from source is one this file claims is unreachable, and the claim is
        // checked against the typechecker rather than trusted.
        let callable = typechecker_builtin_surface();
        let uncallable: Vec<&str> = crate::codegen::builtins::Builtin::ALL
            .iter()
            .map(|b| b.name())
            .filter(|name| !callable.contains(*name))
            .collect();
        assert_eq!(
            uncallable, UNCALLABLE_BUILTINS,
            "the builtins with a Dafny rendering but no typechecker signature \
             changed. A name that gained one is now callable — check its \
             rendering against the prelude declaration and drop it from \
             UNCALLABLE_BUILTINS. A name that lost one, or a new codegen-table \
             entry with no signature, needs the opposite decision."
        );
    }

    /// The guard above asks whether a declaration EXISTS. This one asks
    /// whether Dafny accepts it, over every helper block at once.
    ///
    /// Nothing else covers the blocks no fixture reaches. `ListFind` /
    /// `ListAny` are the sharpest case — the two builtins the guard above
    /// proves no program can call — but the same hole covers any block whose
    /// trigger no example happens to hit: a malformed body, a call to a name
    /// declared in a block this one does not depend on, or a recursion Dafny
    /// cannot see terminating would sit there until the first user wrote the
    /// program that pulls it in.
    ///
    /// Skips when `dafny` is not installed, the same condition the export
    /// tests in `proof_spec` use. Restricted to `--lib`, so the Proof Export
    /// lane is where it actually runs (`.github/workflows/proof.yml`).
    #[test]
    fn every_dafny_helper_block_verifies_together() {
        if std::process::Command::new("dafny")
            .arg("--version")
            .output()
            .is_err()
        {
            eprintln!("skipping dafny prelude check: `dafny` not available");
            return;
        }
        // `force_all` asks for every helper key; the map from key to Dafny
        // text is the same one `build_common_dafny` uses, so this is the
        // prelude as shipped, not a copy of it.
        let mut sections: Vec<String> = vec![
            "// Aver-generated shared library: built-in records and helpers".to_string(),
            "module AverCommon {".to_string(),
            DAFNY_PRELUDE_HEAD.to_string(),
        ];
        for record in crate::codegen::builtin_records::needed_records("", true) {
            sections.push(crate::codegen::builtin_records::render_dafny(record));
        }
        sections.push(DAFNY_PRELUDE_CORE_HELPERS.to_string());
        for helper in crate::codegen::builtin_helpers::needed_helpers("", true) {
            if let Some(block) = dafny_helper_block(helper.key) {
                sections.push(block.to_string());
            }
        }
        sections.push("}".to_string());
        let prelude = sections.join("\n");
        assert!(
            prelude.contains("function ListFind<") && prelude.contains("function ListAny<"),
            "the forced prelude must carry the blocks no fixture reaches, or \
             this test is vacuous:\n{prelude}"
        );

        let dir = std::env::temp_dir().join(format!("aver-dafny-prelude-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir");
        let path = dir.join("common.dfy");
        std::fs::write(&path, &prelude).expect("write prelude");
        let run = std::process::Command::new("dafny")
            .arg("verify")
            .arg(&path)
            .output()
            .expect("dafny verify should run");
        let stdout = String::from_utf8_lossy(&run.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&run.stderr).into_owned();
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            run.status.success(),
            "every helper block has to resolve and verify on its own — a \
             program that pulls this one in gets no obligation checked \
             otherwise.\n{stdout}\n{stderr}"
        );
    }
}
