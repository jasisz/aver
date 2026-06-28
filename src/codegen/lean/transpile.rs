//! Unified Lean project emission: per-module files, the entry file,
//! `AverCommon.lean`, and Oracle-lifted effectful functions.

use std::collections::{HashMap, HashSet};

use crate::ast::TopLevel;
use crate::codegen::{CodegenContext, ProjectOutput};

use super::prelude::{build_common_lean, generate_lakefile_with_roots, generate_toolchain};
use super::{
    VerifyEmitMode, lean_project_name, pure_fns, recursive_pure_fn_names, recursive_type_names,
    toplevel, verify_counter_key,
};

/// Oracle v1: for each effectful FnDef whose effects are all classified,
/// run effect_lifting::lift_fn_def to produce a pure (lifted) FnDef and
/// emit it via the standard pure-fn path. Effectful functions that use
/// unclassified (stateful / interactive / higher-order-callback) effects
/// are still skipped entirely — matches the pre-Oracle behavior.
///
/// Mutual recursion among effectful fns is out of scope for v0: this
/// emits each lifted fn independently.
fn emit_lifted_effectful_functions(
    ctx: &CodegenContext,
    recursive_fns: &HashSet<String>,
    sections: &mut Vec<String>,
) {
    use crate::types::checker::effect_classification::is_classified;

    // Oracle v1: only emit effectful fns reachable from some verify
    // block. Non-terminating effectful fns (e.g. REPL loops that loop
    // forever on `Console.readLine`) would otherwise make Lean reject
    // the whole module — and if nobody is proving anything about them,
    // that's dead code in the proof output.
    let reachable = crate::codegen::common::verify_reachable_fn_names(&ctx.items);

    // Oracle v1: collect the effect list for every eligible
    // effectful fn *first* — call sites to these helpers in any
    // lifted body get `(path, oracle...)` injected so the arity
    // matches the helper's lifted form.
    let mut helpers: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    for item in &ctx.items {
        let TopLevel::FnDef(fd) = item else { continue };
        if fd.effects.is_empty() || fd.name == "main" {
            continue;
        }
        if !fd.effects.iter().all(|e| is_classified(&e.node)) {
            continue;
        }
        if !reachable.contains(&fd.name) {
            continue;
        }
        helpers.insert(
            fd.name.clone(),
            fd.effects.iter().map(|e| e.node.clone()).collect(),
        );
    }

    let mut lifted_fns: Vec<(String, crate::ast::FnDef)> = Vec::new();
    for item in &ctx.items {
        let TopLevel::FnDef(fd) = item else { continue };
        if fd.effects.is_empty() || fd.name == "main" {
            continue;
        }
        if !fd.effects.iter().all(|e| is_classified(&e.node)) {
            continue;
        }
        if !reachable.contains(&fd.name) {
            continue;
        }
        let Ok(Some(lifted)) =
            crate::types::checker::effect_lifting::lift_fn_def_with_helpers(fd, &helpers)
        else {
            continue;
        };
        lifted_fns.push((fd.name.clone(), lifted));
    }

    // Oracle v1: topologically sort so callees are emitted before
    // callers. Without this, a lifted effectful fn that calls another
    // lifted effectful helper (e.g. `handle(msg) -> printErr(msg)`)
    // can land before the helper and Lean complains about an unknown
    // identifier. The pure-fn emission goes through SCC analysis for
    // the same reason — this is a cheap approximation good enough
    // for non-mutual effectful chains.
    let eligible_names: std::collections::HashSet<String> =
        lifted_fns.iter().map(|(n, _)| n.clone()).collect();
    let mut emitted: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut order: Vec<usize> = Vec::new();
    let mut remaining: Vec<usize> = (0..lifted_fns.len()).collect();
    while !remaining.is_empty() {
        let before = remaining.len();
        remaining.retain(|&idx| {
            let body_calls = collect_called_idents_in_body(&lifted_fns[idx].1.body);
            let ready = body_calls
                .iter()
                .all(|name| !eligible_names.contains(name) || emitted.contains(name));
            if ready {
                emitted.insert(lifted_fns[idx].0.clone());
                order.push(idx);
                false
            } else {
                true
            }
        });
        if remaining.len() == before {
            // Cycle or deadlock — fall back to source order for what's
            // left rather than looping forever. Lean will complain at
            // build time, which is the right signal for users.
            order.append(&mut remaining);
        }
    }

    for idx in order {
        let (_, lifted) = &lifted_fns[idx];
        if let Some(code) = toplevel::emit_fn_def(lifted, recursive_fns, ctx) {
            sections.push(code);
            sections.push(String::new());
        }
    }
}

fn collect_called_idents_in_body(body: &crate::ast::FnBody) -> std::collections::HashSet<String> {
    use crate::ast::{Expr, Spanned, Stmt};
    let mut out = std::collections::HashSet::new();
    fn walk(expr: &Spanned<Expr>, out: &mut std::collections::HashSet<String>) {
        match &expr.node {
            Expr::FnCall(callee, args) => {
                if let Expr::Ident(name) | Expr::Resolved { name, .. } = &callee.node {
                    out.insert(name.clone());
                }
                walk(callee, out);
                for a in args {
                    walk(a, out);
                }
            }
            Expr::BinOp(_, l, r) => {
                walk(l, out);
                walk(r, out);
            }
            Expr::Match { subject, arms } => {
                walk(subject, out);
                for arm in arms {
                    walk(&arm.body, out);
                }
            }
            Expr::Attr(inner, _) | Expr::ErrorProp(inner) => walk(inner, out),
            Expr::Constructor(_, Some(inner)) => walk(inner, out),
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                for i in items {
                    walk(i, out);
                }
            }
            Expr::MapLiteral(pairs) => {
                for (k, v) in pairs {
                    walk(k, out);
                    walk(v, out);
                }
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, v) in fields {
                    walk(v, out);
                }
            }
            Expr::RecordUpdate { base, updates, .. } => {
                walk(base, out);
                for (_, v) in updates {
                    walk(v, out);
                }
            }
            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let crate::ast::StrPart::Parsed(inner) = part {
                        walk(inner, out);
                    }
                }
            }
            _ => {}
        }
    }
    for stmt in body.stmts() {
        match stmt {
            Stmt::Expr(e) => walk(e, &mut out),
            Stmt::Binding(_, _, e) => walk(e, &mut out),
        }
    }
    out
}

#[derive(Clone, Copy)]
pub(super) enum LeanEmitMode {
    Standard,
    Proof,
}

/// `true` iff `fd` is the self-recursive inner loop of a recognized
/// `WrapperOverRecursion` law (`sumTR`, `factTR`). The strategy's
/// accumulator-decomposition lemma rewrites with the inner fn's definitional
/// equations, which a `partial def` would not expose — so such a fn must emit
/// as a terminating structural `def` even when the generic recursion classifier
/// (which conservatively rejects a growing accumulator) leaves it unclassified.
/// Scoped to wrapper inners: an accumulator fn the backend can't prove a law
/// about (no strategy fires) stays `partial`, preserving the honest decline.
fn is_wrapper_over_recursion_inner(ctx: &CodegenContext, fd: &crate::ast::FnDef) -> bool {
    ctx.proof_ir.law_theorems.iter().any(|t| {
        matches!(
            &t.strategy,
            crate::ir::ProofStrategy::WrapperOverRecursion { inner_fn, .. }
                if *inner_fn == fd.name
        ) || matches!(
            &t.strategy,
            // The `TailRecFixedBaseFold` loop (`qexp`) recurses on its DRIVER
            // (2nd param); Lean's equation compiler still infers the structural
            // measure across all params, so it must emit as a terminating `def`
            // for the accumulator-decomposition lemma's `rw` to see its
            // definitional equations — a `partial def` would withhold them.
            crate::ir::ProofStrategy::TailRecFixedBaseFold { loop_fn, .. }
                if *loop_fn == fd.name
        )
    })
}

/// Multi-file Lean output for multi-module Aver projects:
/// - `AverCommon.lean` carries built-in helpers + records (UNION decision
///   over every module + entry body, so a helper is included only if
///   something actually references it).
/// - `<Module>.lean` (one per `depends [...]` entry) wraps that module's
///   types and pure fns in `namespace M ... end M`. Submodules like
///   `Models.User` land at `Models/User.lean` to match Lean's path-as-
///   module-name convention.
/// - `<ProjectName>.lean` is the entry: trust header (here only),
///   top-level entry items, lifted effectful fns, decisions, verify
///   blocks. Imports `AverCommon` plus every dependent module.
pub(super) fn transpile_unified(
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    emit_mode: LeanEmitMode,
) -> ProjectOutput {
    // Read recursion fact from `ctx.recursive_fns` — populated upstream
    // by `refresh_facts()` (test stubs) or `build_context` (production).
    // After phase C the set is keyed by `FnId`; project back to bare
    // names for scope-local `emit_fn_def` consumers via the symbol
    // table (the DAG invariant keeps bare-name unambiguous within a
    // single scope). The proof-mode auto-prove path also needs the
    // same bare-name projection.
    let recursive_fns: HashSet<String> = ctx
        .recursive_fns
        .iter()
        .map(|id| ctx.symbol_table.fn_entry(*id).key.name.clone())
        .collect();
    let recursive_names = recursive_pure_fn_names(ctx);
    let recursive_types = recursive_type_names(ctx);
    // Lift every canonical Peano ADT's type annotations to builtin `Nat` for
    // this emit, matching the value/pattern lift — so a Peano type named other
    // than `Nat` is fully consistent (its binders' types agree with the `Nat`
    // literals its values lift to). The guard clears the set when this returns.
    let _peano_guard = crate::codegen::lean::types::scope_canonical_peano(
        crate::codegen::proof_recognize::collect_peano_types(ctx)
            .into_iter()
            .map(|p| p.type_name)
            .collect(),
    );
    // Pure-fn param types + every type def's field types feed the
    // entries-measure emission scan: an entries-list spelling
    // (`Map<K, T>` / `List<Tuple<K, T>>`) may appear only in fn
    // signatures or in ANOTHER type's fields, never in `T`'s own
    // fields, yet the chooser (`type_measure_expr`) would reference
    // `averMeasure<T>Entries_<K>` for it all the same.
    let measure_sig_type_refs: Vec<String> = pure_fns(ctx)
        .iter()
        .flat_map(|fd| fd.params.iter().map(|(_, ty)| ty.clone()))
        .chain(
            ctx.modules
                .iter()
                .flat_map(|m| m.type_defs.iter())
                .chain(ctx.type_defs.iter())
                .flat_map(|td| match td {
                    crate::ast::TypeDef::Sum { variants, .. } => variants
                        .iter()
                        .flat_map(|v| v.fields.iter().cloned())
                        .collect::<Vec<_>>(),
                    crate::ast::TypeDef::Product { fields, .. } => {
                        fields.iter().map(|(_, ty)| ty.clone()).collect()
                    }
                }),
        )
        .collect();

    // Pure fns are SCC-routed per scope (per dependent module + entry)
    // independently — shared `route_pure_components_per_scope` handles
    // the loop. A global SCC pass would conflate same-bare-name fns from
    // different modules (rogue's `Map.getT` vs `Fov.getT`).
    let pure_per_scope = crate::codegen::common::route_pure_components_per_scope(
        ctx,
        toplevel::is_pure_fn,
        |comp, scope| {
            let scope_opt = if scope.is_empty() { None } else { Some(scope) };
            ctx.with_module_scope(scope_opt, || {
                let mut out: Vec<String> = Vec::new();
                if comp.len() > 1 {
                    let code = match emit_mode {
                        LeanEmitMode::Proof => {
                            let all_supported = comp.iter().all(|fd| {
                                crate::codegen::common::fn_contract_exists_for_fn(ctx, fd)
                            });
                            if all_supported {
                                toplevel::emit_mutual_group_proof(comp, ctx)
                            } else {
                                toplevel::emit_mutual_group(comp, ctx)
                            }
                        }
                        LeanEmitMode::Standard => toplevel::emit_mutual_group(comp, ctx),
                    };
                    out.push(code);
                    out.push(String::new());
                } else if let Some(fd) = comp.first() {
                    let emitted = match emit_mode {
                        LeanEmitMode::Proof => {
                            let is_recursive = recursive_names.contains(&fd.name);
                            // ProofIR's `fn_contracts` holds an entry only for
                            // recursive fns the ContractLower stage could
                            // classify. Recursive fns without a contract land
                            // in `unclassified_fns` and fall through to the
                            // partial/non-recursive emit — EXCEPT the inner loop
                            // of a recognized `WrapperOverRecursion` law (e.g.
                            // `factTR`). Such a fn drives structurally on its
                            // first parameter (Lean's equation compiler infers
                            // the measure), so emit it as a terminating `def`:
                            // the strategy's accumulator-decomposition lemma
                            // needs the definitional equations a `partial def`
                            // would withhold. Scoped to wrapper inners so a bare
                            // accumulator fn outside the strategy (whose law the
                            // backend honestly declines) stays `partial`.
                            if is_recursive
                                && !crate::codegen::common::fn_contract_exists_for_fn(ctx, fd)
                                && !is_wrapper_over_recursion_inner(ctx, fd)
                            {
                                toplevel::emit_fn_def(fd, &recursive_names, ctx)
                            } else {
                                toplevel::emit_fn_def_proof(fd, ctx)
                            }
                        }
                        LeanEmitMode::Standard => toplevel::emit_fn_def(fd, &recursive_fns, ctx),
                    };
                    if let Some(code) = emitted {
                        out.push(code);
                        out.push(String::new());
                    }
                }
                out
            })
        },
    );

    // Lifted effectful fns + decisions + verifies remain entry-only.
    let mut entry_lifted_sections: Vec<String> = Vec::new();
    let lifted_recursive_names = match emit_mode {
        LeanEmitMode::Proof => &recursive_names,
        LeanEmitMode::Standard => &recursive_fns,
    };
    emit_lifted_effectful_functions(ctx, lifted_recursive_names, &mut entry_lifted_sections);

    let mut entry_decision_sections: Vec<String> = Vec::new();
    for item in &ctx.items {
        if let TopLevel::Decision(db) = item {
            entry_decision_sections.push(toplevel::emit_decision(db));
            entry_decision_sections.push(String::new());
        }
    }

    let mut entry_verify_sections: Vec<String> = Vec::new();
    let mut verify_case_counters: HashMap<String, usize> = HashMap::new();
    for item in &ctx.items {
        if let TopLevel::Verify(vb) = item {
            let key = verify_counter_key(vb);
            let start_idx = *verify_case_counters.get(&key).unwrap_or(&0);
            let (emitted, next_idx) = toplevel::emit_verify_block(vb, ctx, verify_mode, start_idx);
            verify_case_counters.insert(key, next_idx);
            entry_verify_sections.push(emitted);
            entry_verify_sections.push(String::new());
        }
    }

    // ---- Per-module file bodies ----
    let mut module_files: Vec<(String, String)> = Vec::new();
    let mut union_body = String::new();

    // Cross-file law pool: the dep-law theorems some consumer law admits.
    // Empty for single-file files (no dep modules) → the dep-law emit loop
    // below is a no-op → byte-identical output. Computed once for every
    // module's emit pass.
    let admitted_dep_laws = if matches!(emit_mode, LeanEmitMode::Proof) {
        super::law_auto::admitted_dep_law_theorems(ctx)
    } else {
        std::collections::HashSet::new()
    };

    for module in &ctx.modules {
        let mut body_sections: Vec<String> = Vec::new();
        ctx.with_module_scope(Some(module.prefix.as_str()), || {
            for td in &module.type_defs {
                body_sections.push(toplevel::emit_type_def_in_scope(
                    td,
                    ctx,
                    Some(module.prefix.as_str()),
                ));
                if toplevel::is_recursive_type_def(td)
                    && crate::codegen::proof_recognize::detect_canonical_peano(td).is_none()
                {
                    body_sections.push(toplevel::emit_recursive_decidable_eq(
                        toplevel::type_def_name(td),
                    ));
                    if matches!(emit_mode, LeanEmitMode::Proof)
                        && let Some(measure) = toplevel::emit_recursive_measure(
                            td,
                            &recursive_types,
                            &measure_sig_type_refs,
                        )
                    {
                        body_sections.push(measure);
                    }
                }
                body_sections.push(String::new());
            }
        });
        if let Some(scope_sections) = pure_per_scope.by_scope.get(&module.prefix) {
            body_sections.extend(scope_sections.clone());
        }
        // Cross-file law pool — EMIT side: a dependency module's proven
        // `verify … law` blocks become `<fn>_law_<name>` theorems INSIDE
        // `namespace M`, so a consumer that imports + opens this module can
        // cite `M.<fn>_law_<name>` as a lemma. Same emit path the entry uses
        // (`emit_verify_block`), under this module's scope so the law's
        // expressions resolve in the dep's namespace.
        //
        // FAIL-CLOSED (MAJOR 4): emit a dep law's theorem ONLY when some
        // consumer law ADMITS it (`admitted_dep_law_theorems`, the SAME gate
        // the CONSUME side runs). A dep law no consumer can cite is a
        // complete no-op for the consumer — emitting it would add its
        // `first | … | sorry` proof to the consumer's file-wide `sorry`
        // count for zero benefit. The dep's OWN standalone export
        // (`aver proof Lib.av`) still emits + proves all its laws, charging
        // any sorry to the dep, not to a consumer that never leaned on it.
        // Proof mode only; runtime/standard emits skip laws.
        if matches!(emit_mode, LeanEmitMode::Proof) {
            ctx.with_module_scope(Some(module.prefix.as_str()), || {
                let mut dep_verify_counters: HashMap<String, usize> = HashMap::new();
                for vb in &module.verify_laws {
                    // Compute this law's theorem base under the dep scope and
                    // emit only if it is in the admitted set. The base is the
                    // one `law_as_lemma_statement` reports when it states the law
                    // as a plain rewrite; otherwise the canonical
                    // `<fn>_law_<name>` the block emits anyway. The second case
                    // covers a `when`-premised universal a consumer cites by name
                    // (the rounded-step triangle rung's rounding bounds) — the
                    // law-as-lemma rewrite gate declines a conditional law, but
                    // the keystone still emits its universal theorem, so the
                    // citation must reach it. A law no consumer admits stays
                    // out of the set and is skipped regardless.
                    let law_ref = match &vb.kind {
                        crate::ast::VerifyKind::Law(l) => l,
                        _ => continue,
                    };
                    let base = toplevel::law_as_lemma_statement(vb, law_ref, ctx)
                        .map(|(base, _)| base)
                        .unwrap_or_else(|| toplevel::law_theorem_base(vb, law_ref, ctx));
                    if !admitted_dep_laws.contains(&(module.prefix.clone(), base)) {
                        continue;
                    }
                    let key = verify_counter_key(vb);
                    let start_idx = *dep_verify_counters.get(&key).unwrap_or(&0);
                    let (emitted, next_idx) =
                        toplevel::emit_verify_block(vb, ctx, verify_mode, start_idx);
                    dep_verify_counters.insert(key, next_idx);
                    body_sections.push(emitted);
                    body_sections.push(String::new());
                }
            });
        }
        let body = body_sections.join("\n");
        union_body.push_str(&body);
        union_body.push('\n');

        let mut imports = vec!["import AverCommon".to_string()];
        for d in &module.depends {
            imports.push(format!("import {}", d));
        }
        // AverCommon has no surrounding namespace (top-level helpers / instances),
        // so `import` already brings them into scope. We `open` only the
        // user-defined dependent modules.
        let opens: Vec<String> = module
            .depends
            .iter()
            .map(|d| format!("open {}", d))
            .collect();

        let opens_str = if opens.is_empty() {
            String::new()
        } else {
            format!("\n{}\n", opens.join("\n"))
        };
        let content = format!(
            "{}\n\nset_option linter.unusedVariables false\nset_option maxRecDepth 1000000\n{}\nnamespace {}\n\n{}\nend {}\n",
            imports.join("\n"),
            opens_str,
            module.prefix,
            body,
            module.prefix
        );
        let path = module.prefix.replace('.', "/");
        module_files.push((format!("{}.lean", path), content));
    }

    // ---- Entry sections ----
    let mut entry_body_sections: Vec<String> = Vec::new();
    for td in &ctx.type_defs {
        entry_body_sections.push(toplevel::emit_type_def(td, ctx));
        if toplevel::is_recursive_type_def(td)
            && crate::codegen::proof_recognize::detect_canonical_peano(td).is_none()
        {
            entry_body_sections.push(toplevel::emit_recursive_decidable_eq(
                toplevel::type_def_name(td),
            ));
            if matches!(emit_mode, LeanEmitMode::Proof)
                && let Some(measure) =
                    toplevel::emit_recursive_measure(td, &recursive_types, &measure_sig_type_refs)
            {
                entry_body_sections.push(measure);
            }
        }
        entry_body_sections.push(String::new());
    }
    if let Some(entry_pure) = pure_per_scope.by_scope.get("") {
        entry_body_sections.extend(entry_pure.clone());
    }
    entry_body_sections.extend(entry_lifted_sections);
    entry_body_sections.extend(entry_decision_sections);
    entry_body_sections.extend(entry_verify_sections);

    let entry_body = entry_body_sections.join("\n");
    union_body.push_str(&entry_body);
    union_body.push('\n');

    let project_name = lean_project_name(ctx);
    let mut entry_imports = vec!["import AverCommon".to_string()];
    for m in &ctx.modules {
        entry_imports.push(format!("import {}", m.prefix));
    }
    let entry_opens: Vec<String> = ctx
        .modules
        .iter()
        .map(|m| format!("open {}", m.prefix))
        .collect();
    let mut entry_parts = vec![entry_imports.join("\n")];
    if !entry_opens.is_empty() {
        entry_parts.push(entry_opens.join("\n"));
    }
    // Silence `unused variable` warnings for the named-match equation
    // binders (`h_NN :`) that the wf elaborator needs but the user-
    // source body never references. Without this every ListStructural
    // recursion would surface a warning per nested match. Per-file
    // because `set_option` is local; AverCommon already has the same
    // option for its prelude defs.
    entry_parts.push("set_option linter.unusedVariables false".to_string());
    // Lean 4.31's `simp` recurses deeper through large rewrite sets (the
    // discovered-law floors cite 40+ lemmas in one `simp [...]`); the
    // default `maxRecDepth` overflows on those. This is a pure
    // elaboration-depth limit (no runtime / soundness effect), raised
    // per-file so the big auto-generated simp blocks elaborate.
    entry_parts.push("set_option maxRecDepth 1000000".to_string());
    let declared = crate::codegen::common::collect_declared_effects(ctx);
    let has_ip = union_body.contains("BranchPath");
    let has_classified =
        crate::types::checker::effect_classification::classifications_for_proof_subset()
            .iter()
            .any(|c| declared.includes(c.method));
    if has_ip || has_classified {
        entry_parts.push(
            crate::types::checker::proof_trust_header::generate_commented("-- ", &declared, has_ip),
        );
    }
    let subtype_block = crate::types::checker::oracle_subtypes::lean_subtypes(&declared);
    if !subtype_block.is_empty() {
        // Fold subtype block into the union body BEFORE computing
        // `needed_helpers` — the Oracle subtype block is what
        // introduces `BranchPath` references (e.g. `abbrev
        // TimeUnixMsOracle := BranchPath → Int → Int`) for files that
        // declare classified effects but never spell `BranchPath` in
        // user code. Without this, AverCommon.lean misses the
        // `structure BranchPath` block and Main.lean fails build with
        // `unknown identifier 'BranchPath'`.
        union_body.push_str(&subtype_block);
        union_body.push('\n');
        entry_parts.push(subtype_block);
    }
    entry_parts.push(entry_body);
    let entry_content = entry_parts.join("\n\n");

    // ---- AverCommon.lean ----
    let common_content = build_common_lean(&union_body);

    // Project files
    let mut extra_roots: Vec<String> = vec!["AverCommon".to_string()];
    for m in &ctx.modules {
        extra_roots.push(m.prefix.clone());
    }
    let lakefile = generate_lakefile_with_roots(&project_name, &extra_roots);
    let toolchain = generate_toolchain();

    let mut files = module_files;
    files.push((format!("{}.lean", project_name), entry_content));
    files.push(("AverCommon.lean".to_string(), common_content));
    files.push(("lakefile.lean".to_string(), lakefile));
    files.push(("lean-toolchain".to_string(), toolchain));
    ProjectOutput { files }
}
