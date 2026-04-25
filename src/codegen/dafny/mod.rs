/// Aver → Dafny transpiler.
///
/// Single-module sources emit one `.dfy` file. Multi-module sources emit
/// one file per dependent module wrapped in `module M { ... }`, plus a
/// shared `common.dfy` with built-in records/helpers, plus the entry
/// file holding the trust header, top-level items, and verify lemmas.
mod expr;
mod fuel;
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

fn expr_uses_error_prop(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(f, args) => expr_uses_error_prop(f) || args.iter().any(expr_uses_error_prop),
        Expr::BinOp(_, l, r) => expr_uses_error_prop(l) || expr_uses_error_prop(r),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(subject) || arms.iter().any(|a| expr_uses_error_prop(&a.body))
        }
        Expr::Constructor(_, Some(arg)) => expr_uses_error_prop(arg),
        Expr::List(elems) | Expr::Tuple(elems) => elems.iter().any(expr_uses_error_prop),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_uses_error_prop(e)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(base) || updates.iter().any(|(_, e)| expr_uses_error_prop(e))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(e) => expr_uses_error_prop(e),
            _ => false,
        }),
        Expr::Attr(obj, _) => expr_uses_error_prop(obj),
        Expr::TailCall(inner) => inner.args.iter().any(expr_uses_error_prop),
        _ => false,
    }
}

/// Transpile an Aver program into a Dafny project.
pub fn transpile(ctx: &CodegenContext) -> ProjectOutput {
    if ctx.modules.is_empty() {
        return transpile_single_file(ctx);
    }
    transpile_multi_file(ctx)
}

fn transpile_single_file(ctx: &CodegenContext) -> ProjectOutput {
    // Oracle v1: emit the trust-assumption header block at the top of the
    // file so any downstream reviewer sees exactly which claims the proof
    // relies on. The generator lives in the checker layer and is shared
    // with the Lean backend to guarantee byte-identical claims.
    // Body sections accumulate the actual generated user code (lifted
    // fns, axiomatic fallbacks, fuel helpers, verify lemmas, etc.).
    // Prelude is computed at the end based on what the body referenced
    // — that's how the trust header and built-in record set become
    // conditional on actual usage.
    let mut sections: Vec<String> = Vec::new();

    // Recursion classifier — same detector the Lean backend uses. Fns
    // with a supported plan emit through the usual `decreases`-guarded
    // path; recursive fns outside the supported patterns (mutual without
    // a termination measure the classifier recognises, non-structural
    // nested recursion, etc.) fall back to a `function {:axiom}`
    // declaration so the whole file still type-checks. This parallels
    // Lean's `partial def` fallback for unsupported shapes.
    let (recursion_plans, _recursion_issues) = crate::codegen::recursion::analyze_plans(ctx);

    // Mutual-recursion SCCs get Lean's mutual fuel-guarded emission
    // ported to Dafny: each fn in the SCC gets a `fn__fuel(fuel: nat,
    // …)` helper with `decreases fuel` and a wrapper supplying a
    // plan-specific fuel metric. Fns whose return type has no obvious
    // total default (Named ADTs without an inferred zero-inhabitant)
    // fall back to `function {:axiom}` — parallel to Lean's
    // `partial def`.
    use crate::codegen::recursion::RecursionPlan;
    let mutual_planned: std::collections::HashSet<String> = recursion_plans
        .iter()
        .filter(|(_, plan)| {
            matches!(
                plan,
                RecursionPlan::MutualIntCountdown
                    | RecursionPlan::MutualStringPosAdvance { .. }
                    | RecursionPlan::MutualSizeOfRanked { .. }
            )
        })
        .map(|(name, _)| name.clone())
        .collect();

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
        .filter(|fd| mutual_planned.contains(&fd.name))
        .collect();
    let mutual_components =
        crate::call_graph::ordered_fn_components(&mutual_fns_all, &ctx.module_prefixes);

    let mut mutual_fuel_sections: Vec<String> = Vec::new();
    let mut fuel_emitted: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut axiom_fn_names: std::collections::HashSet<String> = std::collections::HashSet::new();
    for component in &mutual_components {
        let scc_fns: Vec<&FnDef> = component.iter().map(|fd| &**fd).collect();
        match fuel::emit_mutual_fuel_group(&scc_fns, ctx, &recursion_plans) {
            Some(code) => {
                mutual_fuel_sections.push(code);
                for fd in &scc_fns {
                    fuel_emitted.insert(fd.name.clone());
                }
            }
            None => {
                for fd in &scc_fns {
                    axiom_fn_names.insert(fd.name.clone());
                }
            }
        }
    }

    let emit_pure_fn = |fd: &FnDef| -> String {
        if fuel_emitted.contains(&fd.name) {
            String::new() // emitted through mutual fuel group below
        } else if axiom_fn_names.contains(&fd.name) {
            toplevel::emit_fn_def_axiom(fd)
        } else {
            toplevel::emit_fn_def(fd, ctx)
        }
    };

    // Emit type definitions from dependent modules
    for module in &ctx.modules {
        for td in &module.type_defs {
            if let Some(code) = toplevel::emit_type_def(td) {
                sections.push(code);
            }
        }
    }

    // Emit type definitions from the main module
    for td in &ctx.type_defs {
        if let Some(code) = toplevel::emit_type_def(td) {
            sections.push(code);
        }
    }

    // Emit function definitions. Pure fns go through the normal
    // `emit_fn_def` path (which already lowers `?` via
    // `lower_pure_question_bang_fn` when possible). When the body uses
    // `?` and the lowering doesn't apply, emit an axiom declaration so
    // the callee is still in scope for other Dafny output — without
    // this, a fuel helper in a mutual SCC can reference the fn by name
    // and the whole file loses resolution.
    let needs_axiom_for_error_prop = |fd: &FnDef| -> bool {
        body_uses_error_prop(&fd.body)
            && crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
                .ok()
                .flatten()
                .is_none()
    };
    let emit_pure_or_axiom = |fd: &FnDef| -> String {
        if needs_axiom_for_error_prop(fd) {
            toplevel::emit_fn_def_axiom(fd)
        } else {
            emit_pure_fn(fd)
        }
    };
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            if fd.effects.is_empty() {
                sections.push(emit_pure_or_axiom(fd));
            }
        }
    }
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item
            && fd.effects.is_empty()
            && fd.name != "main"
        {
            sections.push(emit_pure_or_axiom(fd));
        }
    }

    // Mutual fuel-guarded helpers and wrappers — one block per SCC.
    // Emitted after the non-mutual pure fns so the axiom fallback block
    // (if any SCC couldn't be defaulted) sits near the other opaque
    // definitions in the output.
    for section in mutual_fuel_sections {
        sections.push(section);
    }

    // Oracle v1: emit lifted form for effectful functions whose effects are
    // all classified (snapshot / generative / output). The lifter prepends
    // `path: BranchPath` plus one oracle / capability param per non-output
    // effect, rewriting effect-method calls in the body. Unclassified or
    // higher-order-callback effects are still skipped.
    //
    // Only fns reachable from some verify block are emitted — otherwise
    // a non-terminating effectful fn (e.g. a REPL loop) would force
    // Dafny to demand a decreases clause for a fn nobody asked to prove.
    let reachable = crate::codegen::common::verify_reachable_fn_names(&ctx.items);
    //
    // Collect the helper registry first so call sites to effectful
    // user fns in any lifted body get `(path, oracle...)` injected
    // to match the callee's lifted arity.
    let mut helpers: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item
            && !fd.effects.is_empty()
            && fd.name != "main"
            && !body_uses_error_prop(&fd.body)
            && reachable.contains(&fd.name)
            && fd
                .effects
                .iter()
                .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
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
            && fd
                .effects
                .iter()
                .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
            && let Ok(Some(lifted)) =
                crate::types::checker::effect_lifting::lift_fn_def_with_helpers(fd, &helpers)
        {
            sections.push(toplevel::emit_fn_def(&lifted, ctx));
        }
    }

    // Emit verify law blocks: sample assertions + universal lemma.
    let mut law_counter: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
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

            // 1. Sample assertions from the domain expansion (concrete smoke tests)
            if !vb.cases.is_empty()
                && let Some(code) = toplevel::emit_law_samples(vb, law, ctx, &suffix)
            {
                sections.push(code);
            }

            // 2. Universal lemma (when clause becomes requires).
            //    Passing in the set of fns Dafny can't unfold in a
            //    lemma context — axiom-fallback fns have no body at
            //    all, and fuel-guarded wrappers only expand to a
            //    call with a concrete metric that Dafny can't
            //    symbolically reason about. In both cases the lemma
            //    body short-circuits to `assume <ensures>`, matching
            //    Lean's `sorry` fallback for the same shape.
            let opaque_fns: std::collections::HashSet<String> =
                axiom_fn_names.union(&fuel_emitted).cloned().collect();
            sections.push(toplevel::emit_verify_law(vb, law, ctx, &opaque_fns));
        }
    }

    let body = sections.join("\n");

    // Now compose the prelude based on what the body actually referenced.
    let mut prelude_sections: Vec<String> = vec![
        "// Generated by the Aver → Dafny transpiler".to_string(),
        "// Pure core logic plus Oracle-lifted classified effects\n".to_string(),
    ];
    if crate::codegen::builtin_records::needs_trust_header(&body) {
        prelude_sections
            .push(crate::types::checker::proof_trust_header::generate_commented("// ") + "\n");
    }
    prelude_sections.push(DAFNY_PRELUDE_HEAD.to_string());
    for record in crate::codegen::builtin_records::needed_records(&body, false) {
        prelude_sections.push(crate::codegen::builtin_records::render_dafny(record));
    }
    prelude_sections.push(DAFNY_PRELUDE_CORE_HELPERS.to_string());
    // Built-in helpers — same shared decision module Lean uses, with
    // Dafny-native fragment per key.
    for helper in crate::codegen::builtin_helpers::needed_helpers(&body, false) {
        match helper.key {
            "BranchPath" => prelude_sections.push(DAFNY_HELPER_BRANCH_PATH.to_string()),
            "AverList" => prelude_sections.push(DAFNY_HELPER_AVER_LIST.to_string()),
            "StringHelpers" => prelude_sections.push(DAFNY_HELPER_STRING_HELPERS.to_string()),
            "NumericParse" => prelude_sections.push(DAFNY_HELPER_NUMERIC_PARSE.to_string()),
            "CharByte" => prelude_sections.push(DAFNY_HELPER_CHAR_BYTE.to_string()),
            "AverMap" => prelude_sections.push(DAFNY_HELPER_AVER_MAP.to_string()),
            // AverMeasure / ProofFuel — Lean-only termination machinery,
            // Dafny uses native `decreases` clauses; nothing to emit here.
            "AverMeasure" | "ProofFuel" => {}
            // FloatInstances / ExceptInstances / StringHadd — Lean-only
            // typeclass instance bundles. Dafny's type system doesn't
            // need analogues; nothing to emit.
            "FloatInstances" | "ExceptInstances" | "StringHadd" => {}
            // Dafny-side built-in datatypes. Lean has these natively.
            "ResultDatatype" => prelude_sections.push(DAFNY_HELPER_RESULT_DATATYPE.to_string()),
            "OptionDatatype" => prelude_sections.push(DAFNY_HELPER_OPTION_DATATYPE.to_string()),
            "OptionToResult" => prelude_sections.push(DAFNY_HELPER_OPTION_TO_RESULT.to_string()),
            "BranchPathDatatype" => {
                prelude_sections.push(DAFNY_HELPER_BRANCH_PATH_DATATYPE.to_string())
            }
            other => panic!(
                "Dafny backend has no implementation for builtin helper key '{}'. \
                 Add a match arm in emit_project or remove the key from BUILTIN_HELPERS.",
                other
            ),
        }
    }

    let content = format!("{}\n{}", prelude_sections.join("\n"), body);
    let file_name = format!("{}.dfy", ctx.project_name);

    ProjectOutput {
        files: vec![(file_name, content)],
    }
}

/// Multi-file Dafny output: one file per dependent module wrapped in
/// `module M { ... }`, a shared `common.dfy` carrying built-in records
/// and helpers under `module AverCommon`, and an entry `<project>.dfy`
/// with the trust header, top-level items, and verify lemmas.
fn transpile_multi_file(ctx: &CodegenContext) -> ProjectOutput {
    use crate::codegen::recursion::RecursionPlan;
    use std::collections::{HashMap, HashSet};

    let (recursion_plans, _recursion_issues) = crate::codegen::recursion::analyze_plans(ctx);
    let mutual_planned: HashSet<String> = recursion_plans
        .iter()
        .filter(|(_, plan)| {
            matches!(
                plan,
                RecursionPlan::MutualIntCountdown
                    | RecursionPlan::MutualStringPosAdvance { .. }
                    | RecursionPlan::MutualSizeOfRanked { .. }
            )
        })
        .map(|(name, _)| name.clone())
        .collect();

    let fn_scope = crate::codegen::common::fn_owning_scope(ctx);

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
        .filter(|fd| mutual_planned.contains(&fd.name))
        .collect();
    let mutual_components =
        crate::call_graph::ordered_fn_components(&mutual_fns_all, &ctx.module_prefixes);

    let mut fuel_per_scope: HashMap<String, Vec<String>> = HashMap::new();
    let mut fuel_emitted: HashSet<String> = HashSet::new();
    let mut axiom_fn_names: HashSet<String> = HashSet::new();

    for component in &mutual_components {
        let scc_fns: Vec<&FnDef> = component.iter().map(|fd| &**fd).collect();
        let scope = scc_fns
            .first()
            .and_then(|fd| fn_scope.get(&fd.name))
            .cloned()
            .unwrap_or_default();
        match fuel::emit_mutual_fuel_group(&scc_fns, ctx, &recursion_plans) {
            Some(code) => {
                fuel_per_scope.entry(scope).or_default().push(code);
                for fd in &scc_fns {
                    fuel_emitted.insert(fd.name.clone());
                }
            }
            None => {
                for fd in &scc_fns {
                    axiom_fn_names.insert(fd.name.clone());
                }
            }
        }
    }

    let needs_axiom_for_error_prop = |fd: &FnDef| -> bool {
        body_uses_error_prop(&fd.body)
            && crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
                .ok()
                .flatten()
                .is_none()
    };
    let emit_pure_or_axiom = |fd: &FnDef| -> String {
        if needs_axiom_for_error_prop(fd) {
            toplevel::emit_fn_def_axiom(fd)
        } else if fuel_emitted.contains(&fd.name) {
            String::new()
        } else if axiom_fn_names.contains(&fd.name) {
            toplevel::emit_fn_def_axiom(fd)
        } else {
            toplevel::emit_fn_def(fd, ctx)
        }
    };

    let mut files: Vec<(String, String)> = Vec::new();
    let mut union_body = String::new();

    // ---- Per-module files ----
    for module in &ctx.modules {
        let mut sections: Vec<String> = Vec::new();
        for td in &module.type_defs {
            if let Some(code) = toplevel::emit_type_def(td) {
                sections.push(code);
            }
        }
        for fd in &module.fn_defs {
            if fd.effects.is_empty() {
                let code = emit_pure_or_axiom(fd);
                if !code.is_empty() {
                    sections.push(code);
                }
            }
        }
        if let Some(fuel) = fuel_per_scope.get(&module.prefix) {
            sections.extend(fuel.clone());
        }
        let body = sections.join("\n");
        union_body.push_str(&body);
        union_body.push('\n');

        let depends_includes: String = module
            .depends
            .iter()
            .map(|d| {
                format!(
                    "include \"{}.dfy\"",
                    crate::codegen::common::module_prefix_to_filename(d)
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        let depends_imports: String = module
            .depends
            .iter()
            .map(|d| format!("  import opened {}", d))
            .collect::<Vec<_>>()
            .join("\n");

        let mut header = format!(
            "// Aver-generated module: {}\ninclude \"common.dfy\"\n",
            module.prefix
        );
        if !depends_includes.is_empty() {
            header.push_str(&depends_includes);
            header.push('\n');
        }

        let mut module_inner = String::from("  import opened AverCommon\n");
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
            header, module.prefix, module_inner
        );
        files.push((
            format!(
                "{}.dfy",
                crate::codegen::common::module_prefix_to_filename(&module.prefix)
            ),
            content,
        ));
    }

    // ---- Entry sections ----
    let mut entry_sections: Vec<String> = Vec::new();
    for td in &ctx.type_defs {
        if let Some(code) = toplevel::emit_type_def(td) {
            entry_sections.push(code);
        }
    }
    for item in &ctx.items {
        if let TopLevel::FnDef(fd) = item
            && fd.effects.is_empty()
            && fd.name != "main"
        {
            let code = emit_pure_or_axiom(fd);
            if !code.is_empty() {
                entry_sections.push(code);
            }
        }
    }
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
            && fd
                .effects
                .iter()
                .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
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
            && fd
                .effects
                .iter()
                .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
            && let Ok(Some(lifted)) =
                crate::types::checker::effect_lifting::lift_fn_def_with_helpers(fd, &helpers)
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
            if !vb.cases.is_empty()
                && let Some(code) = toplevel::emit_law_samples(vb, law, ctx, &suffix)
            {
                entry_sections.push(code);
            }
            let opaque_fns: HashSet<String> =
                axiom_fn_names.union(&fuel_emitted).cloned().collect();
            entry_sections.push(toplevel::emit_verify_law(vb, law, ctx, &opaque_fns));
        }
    }

    let entry_body = entry_sections.join("\n");
    union_body.push_str(&entry_body);
    union_body.push('\n');

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
    let mut entry_parts: Vec<String> = vec![format!(
        "// Aver-generated entry: {}\ninclude \"common.dfy\"\n{}",
        ctx.project_name, entry_includes
    )];
    // Open every dependent module + AverCommon so unqualified type names
    // (`Point`, `Tile`) and helpers stay in scope at the top level.
    let mut opens = vec!["import opened AverCommon".to_string()];
    for m in &ctx.modules {
        opens.push(format!("import opened {}", m.prefix));
    }
    entry_parts.push(opens.join("\n"));
    if crate::codegen::builtin_records::needs_trust_header(&union_body) {
        entry_parts.push(crate::types::checker::proof_trust_header::generate_commented("// "));
    }
    entry_parts.push(entry_body);
    let entry_content = entry_parts.join("\n\n");
    files.push((format!("{}.dfy", ctx.project_name), entry_content));

    // ---- common.dfy ----
    let common_content = build_common_dafny(&union_body);
    files.push(("common.dfy".to_string(), common_content));

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
        match helper.key {
            "BranchPath" => sections.push(DAFNY_HELPER_BRANCH_PATH.to_string()),
            "AverList" => sections.push(DAFNY_HELPER_AVER_LIST.to_string()),
            "StringHelpers" => sections.push(DAFNY_HELPER_STRING_HELPERS.to_string()),
            "NumericParse" => sections.push(DAFNY_HELPER_NUMERIC_PARSE.to_string()),
            "CharByte" => sections.push(DAFNY_HELPER_CHAR_BYTE.to_string()),
            "AverMap" => sections.push(DAFNY_HELPER_AVER_MAP.to_string()),
            "AverMeasure" | "ProofFuel" => {}
            "FloatInstances" | "ExceptInstances" | "StringHadd" => {}
            "ResultDatatype" => sections.push(DAFNY_HELPER_RESULT_DATATYPE.to_string()),
            "OptionDatatype" => sections.push(DAFNY_HELPER_OPTION_DATATYPE.to_string()),
            "OptionToResult" => sections.push(DAFNY_HELPER_OPTION_TO_RESULT.to_string()),
            "BranchPathDatatype" => sections.push(DAFNY_HELPER_BRANCH_PATH_DATATYPE.to_string()),
            other => panic!(
                "Dafny backend has no implementation for builtin helper key '{}'. \
                 Add a match arm in build_common_dafny or remove the key from BUILTIN_HELPERS.",
                other
            ),
        }
    }
    sections.push("}".to_string());
    sections.join("\n")
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
"#;

const DAFNY_HELPER_AVER_MAP: &str = r#"
function MapGet<K, V>(m: map<K, V>, k: K): Option<V> {
  if k in m then Some(m[k])
  else None
}

function MapEntries<K, V>(m: map<K, V>): seq<(K, V)>
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

function StringJoin(sep: string, parts: seq<string>): string
  decreases |parts|
{
  if |parts| == 0 then ""
  else if |parts| == 1 then parts[0]
  else parts[0] + sep + StringJoin(sep, parts[1..])
}
"#;

const DAFNY_HELPER_NUMERIC_PARSE: &str = r#"
function IntToString(n: int): string
function IntFromString(s: string): Result<int, string>
function FloatToString(r: real): string
function FloatFromString(s: string): Result<real, string>
"#;

const DAFNY_HELPER_CHAR_BYTE: &str = r#"
function CharToCode(c: string): int
function CharFromCode(n: int): Option<string>
function ByteToHex(b: int): Result<string, string>
function ByteFromHex(s: string): Result<int, string>
"#;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::build_context;
    use crate::source::parse_source;
    use crate::tco;
    use crate::types::checker::run_type_check_full;
    use std::collections::HashSet;

    fn ctx_from_source(src: &str, project_name: &str) -> CodegenContext {
        let mut items = parse_source(src).expect("parse");
        tco::transform_program(&mut items);
        let tc = run_type_check_full(&items, None);
        assert!(
            tc.errors.is_empty(),
            "source should typecheck: {:?}",
            tc.errors
        );
        build_context(items, &tc, HashSet::new(), project_name.to_string(), vec![])
    }

    fn dafny_output(out: &ProjectOutput) -> &str {
        out.files
            .iter()
            .find_map(|(name, content)| name.ends_with(".dfy").then_some(content.as_str()))
            .expect("expected a .dfy file")
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
             fn pair() -> (Int, Int)\n\
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
}
