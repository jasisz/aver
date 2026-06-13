//! Browser-facing entry points for the Aver playground.

use std::collections::HashMap;

use crate::ast::TopLevel;
use crate::codegen;
use crate::diagnostics::{AnalyzeOptions, analyze_source};
use crate::ir::{PipelineConfig, TypecheckMode};
use crate::source::{LoadedModule, load_module_tree_from_map, parse_source};

/// Compile Aver source text to WASM bytes via the wasm-gc backend.
/// Playground exclusively targets wasm-gc since 0.16 (engine GC +
/// tail calls + factory exports for structured effect returns); the
/// legacy NaN-boxed emitter and its standalone `aver_runtime.wasm`
/// sidecar aren't reachable from any browser entry point and aren't
/// included in the `playground` feature build.
pub fn compile_to_wasm(source: &str) -> Result<Vec<u8>, String> {
    let mut items = parse_source(source)?;

    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            // wasm-gc backend lowers `Expr::InterpolatedStr` natively
            // (`array.new_fixed` + variadic concat helper). The
            // `__buf_*` pipeline that `interp_lower` produces targets
            // bump-allocator backends; wasm-gc would have to emulate
            // a mutable buffer over `(struct len array)` with
            // grow-on-append, while `array.copy` x2 is the idiomatic
            // shape. Skip both lowering passes — same as the CLI
            // `--target wasm-gc` path in `cmd_compile_wasm_gc`.
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    codegen::wasm_gc::compile_to_wasm_gc(&items, pipeline_result.analysis.as_ref())
        .map_err(|e| format!("{e}"))
}

/// Compile a multi-file Aver project from an in-memory file map.
/// `files` maps `path -> source` (matching what `find_module_file`
/// expects: e.g. `"types.av"`, `"rogue/combat.av"`). `entry` is the
/// key of the file holding `module Main` (the `fn main` live point).
///
/// Mirrors the CLI's multi-file build, minus disk IO — the same
/// type checker, resolver, and codegen are reused verbatim so the
/// browser sees identical semantics.
pub fn compile_project_to_wasm(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<Vec<u8>, String> {
    let entry_source = files
        .get(entry)
        .ok_or_else(|| format!("Entry '{}' not present in file map", entry))?;

    let mut entry_items = parse_source(entry_source)?;

    // `module_depends` only reads `TopLevel::Module` declarations, which
    // TCO never touches — so extracting depends pre-pipeline is safe and
    // lets us load deps before typecheck and run the pipeline in one shot.
    let root_depends = module_depends(&entry_items);
    let loaded = load_module_tree_from_map(&root_depends, files)?;

    // Full wasm-gc pipeline — matches CLI `aver compile --target wasm-gc`
    // for multi-file projects. Keep interpolation/buffer lowering off because
    // wasm-gc lowers strings natively.
    let pipeline_result = crate::ir::pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    let modules: Vec<codegen::ModuleInfo> = loaded
        .into_iter()
        .map(|m| loaded_to_module_info(m, false))
        .collect();

    codegen::wasm_gc::flatten_multimodule(&mut entry_items, &modules);
    crate::ir::pipeline::resolve(&mut entry_items);
    codegen::wasm_gc::compile_to_wasm_gc(&entry_items, pipeline_result.analysis.as_ref())
        .map_err(|e| format!("{e}"))
}

/// Multi-file project compile that targets a synthetic `__entry__`
/// fn instead of `main`. `expr` is parsed via `parse_entry_call`
/// (`add(7, 35)` → `("add", [Int(7), Int(35)])`); we look up the
/// target's signature from the entry source / loaded deps, then
/// inject a no-arg `fn __entry__()` whose body is `target(args…)`
/// with each Aver `Value` re-emitted as the corresponding AST
/// `Literal`. The compiler's `_start` synthesis prefers `__entry__`
/// when present, so the host invocation path stays unchanged —
/// `instance.exports._start()` runs the user expression instead of
/// `main`. Returns `(wasm_bytes, target_fn_name)` so callers can
/// label recordings with the user-facing fn name.
pub fn compile_project_to_wasm_with_entry(
    files: &HashMap<String, String>,
    entry: &str,
    expr: &str,
) -> Result<(Vec<u8>, String), String> {
    let entry_source = files
        .get(entry)
        .ok_or_else(|| format!("Entry '{}' not present in file map", entry))?;
    let mut entry_items = parse_source(entry_source)?;
    let root_depends = module_depends(&entry_items);
    let loaded = load_module_tree_from_map(&root_depends, files)?;

    let (target_fn, args) =
        crate::replay::parse_entry_call(expr).map_err(|e| format!("--expr parse: {}", e))?;
    let (return_type, _effects) = lookup_fn_signature(&entry_items, &loaded, &target_fn)
        .ok_or_else(|| format!("entry fn `{}` not found in project", target_fn))?;

    let synth = build_synth_entry_fn(&target_fn, &args, &return_type)?;
    entry_items.push(synth);

    let pipeline_result = crate::ir::pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    let modules: Vec<codegen::ModuleInfo> = loaded
        .into_iter()
        .map(|m| loaded_to_module_info(m, false))
        .collect();
    codegen::wasm_gc::flatten_multimodule(&mut entry_items, &modules);
    crate::ir::pipeline::resolve(&mut entry_items);
    let bytes =
        codegen::wasm_gc::compile_to_wasm_gc(&entry_items, pipeline_result.analysis.as_ref())
            .map_err(|e| format!("{e}"))?;
    Ok((bytes, target_fn))
}

/// Find a fn def by name across the entry source and any loaded
/// dependency module. Returns `(return_type, effects)` so the
/// synthetic `__entry__` can mirror the target's signature shape.
/// Multi-module flatten happens AFTER synth injection, so dep fns
/// are still siloed under their `LoadedModule.items` here — both
/// places have to be searched.
fn lookup_fn_signature(
    entry_items: &[crate::ast::TopLevel],
    loaded: &[LoadedModule],
    target: &str,
) -> Option<(String, Vec<crate::ast::Spanned<String>>)> {
    let scan =
        |items: &[crate::ast::TopLevel]| -> Option<(String, Vec<crate::ast::Spanned<String>>)> {
            for item in items {
                if let crate::ast::TopLevel::FnDef(fd) = item
                    && fd.name == target
                {
                    return Some((fd.return_type.clone(), fd.effects.clone()));
                }
            }
            None
        };
    if let Some(s) = scan(entry_items) {
        return Some(s);
    }
    for m in loaded {
        if let Some(s) = scan(&m.items) {
            return Some(s);
        }
    }
    None
}

/// Build `fn __entry__() -> <return_type>: target(args…)` as a
/// `TopLevel::FnDef`. Each `Value` arg lowers to the matching
/// `Expr::Literal`. Compound shapes (`List`, `Tuple`, `Variant`,
/// `Record`) raise an error — extending `value_to_literal_expr`
/// to cover them is a follow-up. Effects are declared as
/// `! [target]` so the verify pass sees the user fn in the
/// surface and module-level `effects [...]` lists.
fn build_synth_entry_fn(
    target_fn: &str,
    args: &[crate::value::Value],
    return_type: &str,
) -> Result<crate::ast::TopLevel, String> {
    use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt, TopLevel};
    let arg_exprs: Vec<Spanned<Expr>> = args
        .iter()
        .map(value_to_literal_expr)
        .collect::<Result<_, _>>()?;
    let callee = Spanned::bare(Expr::Ident(target_fn.to_string()));
    let call = Spanned::bare(Expr::FnCall(Box::new(callee), arg_exprs));
    let body = FnBody::Block(vec![Stmt::Expr(call)]);
    Ok(TopLevel::FnDef(FnDef {
        name: "__entry__".to_string(),
        line: 0,
        params: vec![],
        return_type: return_type.to_string(),
        effects: vec![Spanned::bare(target_fn.to_string())],
        desc: None,
        body: std::sync::Arc::new(body),
        resolution: None,
    }))
}

/// Convert a `Value` literal back into its AST shape so the
/// synthetic entry body type-checks under the same path as a
/// hand-written call site. Supported: Int / Float / Bool / Str /
/// Unit. Compound shapes (lists, tuples, variants, records) raise
/// an error — extending the mapper to cover them is a follow-up.
fn value_to_literal_expr(
    v: &crate::value::Value,
) -> Result<crate::ast::Spanned<crate::ast::Expr>, String> {
    use crate::ast::{Expr, Literal, Spanned};
    let lit = match v {
        crate::value::Value::Int(n) => Literal::Int(*n),
        crate::value::Value::Float(f) => Literal::Float(*f),
        crate::value::Value::Str(s) => Literal::Str(s.clone()),
        crate::value::Value::Bool(b) => Literal::Bool(*b),
        crate::value::Value::Unit => Literal::Unit,
        other => {
            return Err(format!(
                "synthetic `__entry__` only supports Int/Float/Bool/String/Unit args today; got {:?}",
                other
            ));
        }
    };
    Ok(Spanned::bare(Expr::Literal(lit)))
}

/// Re-exported for the wasm-bindgen `aver_parse_entry_target` arm.
/// Gated on the `playground` feature so the symbol mirrors the
/// `bindgen` module's visibility — non-playground builds drop both.
#[cfg(feature = "playground")]
fn crate_parse_entry_call(expr: &str) -> Result<(String, Vec<crate::value::Value>), String> {
    crate::replay::parse_entry_call(expr)
}

// ── Proof export & Rust compile entry points ────────────────────────
//
// Single-file source → backend project files (path → content map).
// JS receives `{ "<path>": "<content>", ... }` as JSON and zips it
// in the browser via `buildZip`. Same as what `aver proof --backend
// {lean,dafny}` and `aver compile --target rust` produce on disk.

/// Single-file pipeline runner shared by proof (Lean/Dafny) and Rust
/// target paths. `apply_traversal_lowering` is the proof-vs-runtime
/// distinction: proof exporters consume source-level IR (interp_lower
/// + buffer_build off), the Rust target wants the deforested form.
#[cfg(feature = "runtime")]
fn build_ctx(
    source: &str,
    apply_traversal_lowering: bool,
) -> Result<codegen::CodegenContext, String> {
    let mut items = parse_source(source)?;
    // Proof exporters (Lean / Dafny) consume source-level IR (no
    // traversal lowering) AND need ProofIR populated. Rust target
    // wants the deforested form AND skips proof_lower. The two
    // requirements always co-vary in the playground, so the same
    // flag drives both.
    let proof_target = !apply_traversal_lowering;
    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            run_interp_lower: apply_traversal_lowering,
            run_buffer_build: apply_traversal_lowering,
            run_refinement_lower: proof_target,
            run_contract_lower: proof_target,
            run_law_lower: proof_target,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }
    let proof_ir = pipeline_result.proof_ir;
    let mut ctx = codegen::build_context(
        items,
        &tc_result,
        pipeline_result.analysis.as_ref(),
        "playground".to_string(),
        vec![],
        pipeline_result.symbol_table,
        pipeline_result.resolved_items,
    );
    if let Some(ir) = proof_ir {
        ctx.proof_ir = ir;
    }
    Ok(ctx)
}

/// Single-file Aver source → Lean 4 project files.
#[cfg(feature = "runtime")]
pub fn proof_lean_files(source: &str) -> Result<HashMap<String, String>, String> {
    let mut ctx = build_ctx(source, false)?;
    let output = codegen::lean::transpile(&mut ctx);
    Ok(output.files.into_iter().collect())
}

/// Single-file Aver source → Dafny project files.
#[cfg(feature = "runtime")]
pub fn proof_dafny_files(source: &str) -> Result<HashMap<String, String>, String> {
    let ctx = build_ctx(source, false)?;
    let output = codegen::dafny::transpile(&ctx);
    Ok(output.files.into_iter().collect())
}

/// Single-file Aver source → Rust/Cargo project files. Full pipeline so
/// the Rust output matches CLI `aver compile --target rust`.
#[cfg(feature = "runtime")]
pub fn compile_rust_files(source: &str) -> Result<HashMap<String, String>, String> {
    let mut ctx = build_ctx(source, true)?;
    let output = codegen::rust::transpile(&mut ctx);
    Ok(output.files.into_iter().collect())
}

/// Build a `CodegenContext` for a multi-file project — same semantics
/// as `compile_project_to_wasm`, refactored so the proof and Rust
/// exports can reuse it.
#[cfg(feature = "runtime")]
fn build_project_ctx(
    files: &HashMap<String, String>,
    entry: &str,
    apply_traversal_lowering: bool,
) -> Result<codegen::CodegenContext, String> {
    let entry_source = files
        .get(entry)
        .ok_or_else(|| format!("Entry '{}' not present in file map", entry))?;
    let mut entry_items = parse_source(entry_source)?;

    let root_depends = module_depends(&entry_items);
    let loaded = load_module_tree_from_map(&root_depends, files)?;

    // Build the dep-module list BEFORE pipeline::run so the pipeline
    // sees the full program. `SymbolTable::build` walks dep_modules
    // to assign `FnId`s to module-owned fns; `populate_fn_contracts`
    // then resolves `FnKey { scope: Some("Module"), name }` against
    // those ids. Skipping this step left the multi-file proof path
    // with an entry-only symbol table and module fns silently missing
    // from `proof_ir.fn_contracts`.
    let modules: Vec<codegen::ModuleInfo> = loaded
        .iter()
        .cloned()
        .map(|m| loaded_to_module_info(m, apply_traversal_lowering))
        .collect();

    // Multi-file pipeline. `apply_traversal_lowering` mirrors the CLI
    // proof-vs-runtime distinction at this API boundary: proof export
    // wants source-level IR (no traversal lowering) AND the proof
    // stages populated. BuildSymbols auto-enables alongside proof
    // stages so the FnId-keyed `fn_contracts` / `refined_types` maps
    // are reachable downstream.
    let proof_target = !apply_traversal_lowering;
    let pipeline_result = crate::ir::pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_interp_lower: apply_traversal_lowering,
            run_buffer_build: apply_traversal_lowering,
            run_refinement_lower: proof_target,
            run_contract_lower: proof_target,
            run_law_lower: proof_target,
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        return Err(format_tc_errors(&tc_result.errors));
    }

    let proof_ir = pipeline_result.proof_ir;
    let mut ctx = codegen::build_context(
        entry_items,
        &tc_result,
        pipeline_result.analysis.as_ref(),
        "playground".to_string(),
        modules,
        pipeline_result.symbol_table,
        pipeline_result.resolved_items,
    );
    if let Some(ir) = proof_ir {
        ctx.proof_ir = ir;
    }
    Ok(ctx)
}

/// Multi-file Aver project → Lean 4 project files.
#[cfg(feature = "runtime")]
pub fn proof_lean_files_project(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<HashMap<String, String>, String> {
    let mut ctx = build_project_ctx(files, entry, false)?;
    let output = codegen::lean::transpile(&mut ctx);
    Ok(output.files.into_iter().collect())
}

/// Multi-file Aver project → Dafny project files.
#[cfg(feature = "runtime")]
pub fn proof_dafny_files_project(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<HashMap<String, String>, String> {
    let ctx = build_project_ctx(files, entry, false)?;
    let output = codegen::dafny::transpile(&ctx);
    Ok(output.files.into_iter().collect())
}

/// Multi-file Aver project → Rust/Cargo project files. Full pipeline
/// so the Rust output matches CLI `aver compile --target rust`.
#[cfg(feature = "runtime")]
pub fn compile_rust_files_project(
    files: &HashMap<String, String>,
    entry: &str,
) -> Result<HashMap<String, String>, String> {
    let mut ctx = build_project_ctx(files, entry, true)?;
    let output = codegen::rust::transpile(&mut ctx);
    Ok(output.files.into_iter().collect())
}

fn module_depends(items: &[TopLevel]) -> Vec<String> {
    items
        .iter()
        .find_map(|i| match i {
            TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default()
}

/// Lower a single dep module from the virtual filesystem. `apply_traversal_lowering`
/// mirrors the entry-level decision so the dep modules go through the
/// same pipeline shape as the entry — proof exporters get source-level IR
/// end-to-end, runtime targets get the deforested form.
fn loaded_to_module_info(m: LoadedModule, apply_traversal_lowering: bool) -> codegen::ModuleInfo {
    let mut items = m.items;
    // No typecheck — entry-level typecheck has already validated
    // cross-module refs against `loaded`. The analyze stage runs so the
    // ModuleInfo we publish carries per-module facts for codegen.
    let neutral_policy = crate::ir::NeutralAllocPolicy;
    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            run_interp_lower: apply_traversal_lowering,
            run_buffer_build: apply_traversal_lowering,
            alloc_policy: Some(&neutral_policy),
            ..Default::default()
        },
    );

    let depends = module_depends(&items);
    let type_defs = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::TypeDef(td) => Some(td.clone()),
            _ => None,
        })
        .collect();
    let fn_defs = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
            _ => None,
        })
        .collect();

    codegen::ModuleInfo {
        prefix: m.dep_name,
        depends,
        type_defs,
        fn_defs,
        verify_laws: codegen::collect_verify_laws(&items),
        analysis: pipeline_result.analysis,
    }
}

fn format_tc_errors(errors: &[crate::types::checker::TypeError]) -> String {
    errors
        .iter()
        .map(|e| format!("error[{}:{}]: {}", e.line, e.col, e.message))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Run the single-file analysis pipeline and return the canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) as JSON. Consumers
/// should parse the `diagnostics` array; an empty array means the file
/// passed every enabled check.
pub fn check_source(source: &str) -> String {
    let opts = AnalyzeOptions::new("playground");
    analyze_source(source, &opts).to_json()
}

/// Multi-file variant: builds an `AnalyzeOptions` with dependency
/// modules pre-loaded from the provided virtual fs map, so the type
/// checker sees every `depends [...]` entry without disk IO.
/// Verify execution is skipped for multi-file projects (VM module
/// loader is disk-only today).
fn analyze_project(
    files: &HashMap<String, String>,
    entry: &str,
    make_opts: impl FnOnce(AnalyzeOptions) -> AnalyzeOptions,
) -> String {
    let entry_source = match files.get(entry) {
        Some(s) => s.clone(),
        None => {
            return crate::diagnostics::AnalysisReport::new("playground").to_json();
        }
    };
    let mut opts = AnalyzeOptions::new("playground");
    // Parse once to extract depends; errors are surfaced again inside
    // analyze_source with proper diagnostic formatting.
    if let Ok(items) = parse_source(&entry_source) {
        let depends = module_depends(&items);
        if let Ok(loaded) = crate::source::load_module_tree_from_map(&depends, files) {
            opts = opts.with_loaded_modules(loaded);
        }
    }
    opts = make_opts(opts);
    analyze_source(&entry_source, &opts).to_json()
}

pub fn check_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |o| o)
}

/// Run analysis plus verify block execution and return the canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) as JSON. Verify
/// runs only when the source is typecheck-clean; callers see the same
/// mismatch/runtime-error diagnostics as `aver verify`.
pub fn verify_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    analyze_source(source, &opts).to_json()
}

pub fn verify_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_verify_run = true;
        o
    })
}

/// Run verify under `--hostile` mode: typed `given` domains are expanded
/// with the per-type boundary set and each case is multiplied by the
/// adversarial effect-profile cartesian. Diagnostics flagged
/// `from_hostile = true` indicate failures the law would not catch under
/// declared values alone — a missing `when` precondition or an unpinned
/// effect.
pub fn verify_source_hostile(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    opts.verify_run_hostile = true;
    analyze_source(source, &opts).to_json()
}

pub fn verify_project_hostile(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_verify_run = true;
        o.verify_run_hostile = true;
        o
    })
}

/// Run analysis plus the file-local "why" summary (per-function
/// justification signals) and return the canonical report as JSON.
pub fn why_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_why_summary = true;
    analyze_source(source, &opts).to_json()
}

pub fn why_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_why_summary = true;
        o
    })
}

/// Run analysis plus the file-local context summary (module shape,
/// functions, types, decisions) and return the canonical report as
/// JSON. Dependency bodies are not expanded — the playground sees the
/// entry file only; `depends` carries names for UI.
pub fn context_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_context_summary = true;
    analyze_source(source, &opts).to_json()
}

pub fn context_project(files: &HashMap<String, String>, entry: &str) -> String {
    analyze_project(files, entry, |mut o| {
        o.include_context_summary = true;
        o
    })
}

/// Render the context as markdown (same shape as CLI
/// `aver context --md`). Source → ContextSummary → markdown, no
/// intermediate serialization.
pub fn context_md_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_context_summary = true;
    let report = analyze_source(source, &opts);
    match report.context_summary {
        Some(summary) => crate::diagnostics::context::render_context_md(&summary),
        None => {
            "# Aver Context\n\n_No context available (parse or typecheck failed)._\n".to_string()
        }
    }
}

pub fn context_md_project(files: &HashMap<String, String>, entry: &str) -> String {
    let Some(entry_source) = files.get(entry).cloned() else {
        return format!(
            "# Aver Context\n\n_Entry '{}' not found in project._\n",
            entry
        );
    };
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_context_summary = true;
    if let Ok(items) = parse_source(&entry_source) {
        let deps = module_depends(&items);
        if let Ok(loaded) = crate::source::load_module_tree_from_map(&deps, files) {
            opts = opts.with_loaded_modules(loaded);
        }
    }
    let report = analyze_source(&entry_source, &opts);
    match report.context_summary {
        Some(summary) => crate::diagnostics::context::render_context_md(&summary),
        None => {
            "# Aver Context\n\n_No context available (parse or typecheck failed)._\n".to_string()
        }
    }
}

/// Audit: three-axis health check — static analysis (every enabled
/// collector), verify block execution, and format-check. Equivalent of
/// the CLI `aver audit` but single-file. Returns a canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) bundle with
/// diagnostics + verify_summary.
#[cfg(feature = "runtime")]
pub fn audit_source(source: &str) -> String {
    audit_build_report(source, None, None, None, false).to_json()
}

/// Audit under `--hostile` mode — see `verify_source_hostile` for
/// what the hostile expansion adds. The audit panel surfaces the same
/// dual-run pass/fail breakdown as `aver audit --hostile` from the CLI.
#[cfg(feature = "runtime")]
pub fn audit_source_hostile(source: &str) -> String {
    audit_build_report(source, None, None, None, true).to_json()
}

/// Aver source → `aver shape` JSON report. Renders the module shape
/// vector + Kind + histogram + Layer + per-fn archetype labels +
/// module patterns. Returns a JSON error object on parse / analyze
/// failure so the UI can show the diagnostic alongside the editor.
pub fn shape_source(source: &str) -> String {
    use crate::diagnostics::shape;
    let fingerprints = shape::builtin_v0_layer_fingerprints();
    let report = shape::analyze_source_with(
        source,
        ".",
        "<playground>",
        "Module",
        &fingerprints,
        "built-in v0",
    );
    match report {
        Ok(r) => shape::render_json(&r).to_string(),
        Err(e) => serde_json::json!({ "error": e }).to_string(),
    }
}

#[cfg(feature = "runtime")]
pub fn audit_project(files: &HashMap<String, String>, entry: &str) -> String {
    let Some(entry_source) = files.get(entry) else {
        return crate::diagnostics::AnalysisReport::new("playground").to_json();
    };
    let loaded = parse_source(entry_source)
        .ok()
        .map(|items| module_depends(&items))
        .and_then(|deps| crate::source::load_module_tree_from_map(&deps, files).ok());
    audit_build_report(entry_source, loaded, Some(files), Some(entry), false).to_json()
}

#[cfg(feature = "runtime")]
pub fn audit_project_hostile(files: &HashMap<String, String>, entry: &str) -> String {
    let Some(entry_source) = files.get(entry) else {
        return crate::diagnostics::AnalysisReport::new("playground").to_json();
    };
    let loaded = parse_source(entry_source)
        .ok()
        .map(|items| module_depends(&items))
        .and_then(|deps| crate::source::load_module_tree_from_map(&deps, files).ok());
    audit_build_report(entry_source, loaded, Some(files), Some(entry), true).to_json()
}

#[cfg(feature = "runtime")]
fn audit_build_report(
    source: &str,
    loaded: Option<Vec<LoadedModule>>,
    all_files: Option<&HashMap<String, String>>,
    entry: Option<&str>,
    hostile: bool,
) -> crate::diagnostics::AnalysisReport {
    use crate::diagnostics::needs_format_diagnostic;

    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    opts.verify_run_hostile = hostile;
    if let Some(loaded) = loaded {
        opts = opts.with_loaded_modules(loaded);
    }
    let mut report = analyze_source(source, &opts);

    // Format-check for the entry source (parity with CLI audit).
    #[cfg(feature = "tty-render")]
    if let Ok((formatted, violations)) = crate::format::try_format_source(source)
        && formatted != source
    {
        report
            .diagnostics
            .push(needs_format_diagnostic("playground", &violations, source));
    }

    // Extra pass: format-check every non-entry file in the virtual fs
    // too, so the audit panel's Format section covers the whole
    // project, not just main.av.
    #[cfg(feature = "tty-render")]
    if let (Some(files), Some(entry)) = (all_files, entry) {
        for (path, src) in files {
            if path == entry {
                continue;
            }
            if let Ok((formatted, violations)) = crate::format::try_format_source(src)
                && formatted != *src
            {
                report
                    .diagnostics
                    .push(needs_format_diagnostic(path, &violations, src));
            }
        }
    }

    report
}

/// Format the source and return the rewritten text. Non-mutating by
/// itself — caller (JS) replaces editor contents. Returns the original
/// source unchanged on parse error.
#[cfg(feature = "tty-render")]
pub fn format_source(source: &str) -> String {
    crate::format::try_format_source(source)
        .map(|(text, _violations)| text)
        .unwrap_or_else(|_| source.to_string())
}

#[cfg(feature = "playground")]
mod bindgen {
    use wasm_bindgen::prelude::*;

    // Route Rust panics to console.error via a one-shot hook so the
    // browser console shows the real message instead of "unreachable
    // executed". Installed lazily at the first binding entry; cheap
    // if called repeatedly (Once guard).
    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace = console, js_name = error)]
        fn console_error(s: &str);
    }

    // Called automatically by wasm-bindgen when the module boots
    // (`await mod.default(...)` in JS). Routes Rust panics to the
    // browser's console.error so wasm traps ("unreachable executed")
    // carry the real panic message instead of a generic stub.
    #[wasm_bindgen(start)]
    pub fn init_playground() {
        std::panic::set_hook(Box::new(|info| {
            console_error(&format!("Aver playground panic: {}", info));
        }));
    }

    #[wasm_bindgen]
    pub fn aver_compile(source: &str) -> Result<Vec<u8>, JsError> {
        super::compile_to_wasm(source).map_err(|e| JsError::new(&e))
    }

    /// Compile a multi-file project. `files_json` is a JSON object
    /// mapping path -> source (e.g. `{"types.av": "...", "main.av":
    /// "..."}`). `entry` is the key of the entry file.
    #[wasm_bindgen]
    pub fn aver_compile_project(files_json: &str, entry: &str) -> Result<Vec<u8>, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        super::compile_project_to_wasm(&files, entry).map_err(|e| JsError::new(&e))
    }

    /// Compile a project that targets `expr` (e.g. `add(7, 35)`)
    /// instead of `main`. Wraps the call in a synthetic `__entry__`
    /// fn the codegen wires `_start` through, so the playground
    /// worker can run user expressions on the native wasm-gc path
    /// without any JS-side argument encoder.
    #[wasm_bindgen]
    pub fn aver_compile_project_with_entry(
        files_json: &str,
        entry: &str,
        expr: &str,
    ) -> Result<Vec<u8>, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        let (bytes, _target_fn) = super::compile_project_to_wasm_with_entry(&files, entry, expr)
            .map_err(|e| JsError::new(&e))?;
        Ok(bytes)
    }

    /// Resolve the user-facing fn name that `expr` calls. Returns
    /// just the name half of `parse_entry_call(expr)` so the JS host
    /// can label recordings without re-parsing the call expression.
    #[wasm_bindgen]
    pub fn aver_parse_entry_target(expr: &str) -> Result<String, JsError> {
        let (name, _args) = super::crate_parse_entry_call(expr).map_err(|e| JsError::new(&e))?;
        Ok(name)
    }

    #[wasm_bindgen]
    pub fn aver_check(source: &str) -> String {
        super::check_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_verify(source: &str) -> String {
        super::verify_source(source)
    }

    /// Verify under `--hostile` mode: per-type boundary set substituted
    /// into typed `given` clauses, plus per-classified-effect adversarial
    /// profile cartesian. The returned report carries `from_hostile = true`
    /// on cases that would not surface under `aver_verify`.
    #[wasm_bindgen]
    pub fn aver_verify_hostile(source: &str) -> String {
        super::verify_source_hostile(source)
    }

    #[wasm_bindgen]
    pub fn aver_why(source: &str) -> String {
        super::why_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_context(source: &str) -> String {
        super::context_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_audit(source: &str) -> String {
        super::audit_source(source)
    }

    /// Aver source → `aver shape` JSON report (module shape vector,
    /// derived Kind, histogram, Layer, per-fn archetypes, module
    /// patterns). Same payload as the CLI's `--json` mode; the
    /// playground renders this inside the Audit panel.
    #[wasm_bindgen]
    pub fn aver_shape(source: &str) -> String {
        super::shape_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_audit_hostile(source: &str) -> String {
        super::audit_source_hostile(source)
    }

    #[wasm_bindgen]
    pub fn aver_format(source: &str) -> String {
        super::format_source(source)
    }

    /// Aver source → Lean 4 project files (JSON `{path: content}`).
    /// JS zips the result in the browser. Maps to `aver proof
    /// --backend lean` on the CLI.
    #[wasm_bindgen]
    pub fn aver_proof_lean(source: &str) -> Result<String, JsError> {
        let files = super::proof_lean_files(source).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&files).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Aver source → Dafny project files (JSON `{path: content}`).
    /// Maps to `aver proof --backend dafny` on the CLI.
    #[wasm_bindgen]
    pub fn aver_proof_dafny(source: &str) -> Result<String, JsError> {
        let files = super::proof_dafny_files(source).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&files).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Aver source → Rust/Cargo project files (JSON `{path: content}`).
    /// Maps to `aver compile --target rust` on the CLI.
    #[wasm_bindgen]
    pub fn aver_compile_rust(source: &str) -> Result<String, JsError> {
        let files = super::compile_rust_files(source).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&files).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Multi-file Aver project → Lean 4 project files (JSON).
    #[wasm_bindgen]
    pub fn aver_proof_lean_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        let out = super::proof_lean_files_project(&files, entry).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&out).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Multi-file Aver project → Dafny project files (JSON).
    #[wasm_bindgen]
    pub fn aver_proof_dafny_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        let out = super::proof_dafny_files_project(&files, entry).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&out).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Multi-file Aver project → Rust/Cargo project files (JSON).
    #[wasm_bindgen]
    pub fn aver_compile_rust_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files: std::collections::HashMap<String, String> =
            serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))?;
        let out = super::compile_rust_files_project(&files, entry).map_err(|e| JsError::new(&e))?;
        serde_json::to_string(&out).map_err(|e| JsError::new(&e.to_string()))
    }

    // ── Project (multi-file) analysis bindings ─────────────────────
    // Same semantics as the single-file siblings above, but deps
    // referenced via `depends [...]` resolve against the supplied
    // virtual fs (JSON path → source map) instead of failing with
    // "Unknown identifier".

    fn parse_files(files_json: &str) -> Result<std::collections::HashMap<String, String>, JsError> {
        serde_json::from_str(files_json).map_err(|e| JsError::new(&e.to_string()))
    }

    #[wasm_bindgen]
    pub fn aver_check_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::check_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_verify_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::verify_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_verify_hostile_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::verify_project_hostile(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_why_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::why_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_context_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::context_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_context_md(source: &str) -> String {
        super::context_md_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_context_md_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::context_md_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_audit_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::audit_project(&files, entry))
    }

    #[wasm_bindgen]
    pub fn aver_audit_hostile_project(files_json: &str, entry: &str) -> Result<String, JsError> {
        let files = parse_files(files_json)?;
        Ok(super::audit_project_hostile(&files, entry))
    }

    // Record / replay run on the JS side via `aver_compile_project*`
    // + WebWorker wasm-gc, no Rust-hosted bindings needed.
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn read(path: &str) -> String {
        std::fs::read_to_string(path).unwrap_or_else(|_| panic!("missing {}", path))
    }

    fn load_rogue_files() -> HashMap<String, String> {
        let root = "tools/website/playground/sources/examples/games/rogue";
        let mut files: HashMap<String, String> = HashMap::new();
        for f in [
            "types",
            "map",
            "fov",
            "pathfinding",
            "combat",
            "render",
            "main",
        ] {
            files.insert(format!("{}.av", f), read(&format!("{}/{}.av", root, f)));
        }
        files
    }

    #[test]
    fn proof_lean_emits_files_for_simple_source() {
        let src = "module M\n    intent = \"t\"\n\n\
                   fn add(a: Int, b: Int) -> Int\n    a + b\n\n\
                   verify add\n    add(2, 3) => 5\n";
        let files = proof_lean_files(src).expect("lean files");
        assert!(!files.is_empty(), "Lean export should produce files");
        let any_lean_with_add = files.iter().any(|(k, v)| {
            k.ends_with(".lean")
                && k != "lakefile.lean"
                && (v.contains("def add") || v.contains("add ("))
        });
        assert!(
            any_lean_with_add,
            "generated Lean should mention `add` somewhere; files: {:?}",
            files.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn proof_dafny_emits_files_for_simple_source() {
        let src = "module M\n    intent = \"t\"\n\n\
                   fn add(a: Int, b: Int) -> Int\n    a + b\n\n\
                   verify add\n    add(2, 3) => 5\n";
        let files = proof_dafny_files(src).expect("dafny files");
        assert!(!files.is_empty(), "Dafny export should produce files");
        assert!(
            files.iter().any(|(k, _)| k.ends_with(".dfy")),
            "should include a .dfy"
        );
    }

    #[test]
    fn compile_rust_emits_cargo_project() {
        let src = "module M\n    intent = \"t\"\n\n\
                   fn add(a: Int, b: Int) -> Int\n    a + b\n\n\
                   fn main() -> Unit\n    ! [Console.print]\n    Console.print(\"ok\")\n";
        let files = compile_rust_files(src).expect("rust files");
        assert!(
            files.contains_key("Cargo.toml"),
            "Rust export should include Cargo.toml"
        );
        assert!(
            files
                .iter()
                .any(|(k, _)| k.starts_with("src/") && k.ends_with(".rs")),
            "Rust export should include at least one src/*.rs file"
        );
    }

    #[test]
    fn proof_lean_project_handles_multi_file() {
        let files = load_rogue_files();
        let out = proof_lean_files_project(&files, "main.av")
            .expect("multi-file Lean export should succeed");
        assert!(!out.is_empty(), "Lean project export should produce files");
        assert!(out.iter().any(|(k, _)| k.ends_with(".lean")));
    }

    #[test]
    fn proof_dafny_project_handles_multi_file() {
        let files = load_rogue_files();
        let out = proof_dafny_files_project(&files, "main.av")
            .expect("multi-file Dafny export should succeed");
        assert!(!out.is_empty(), "Dafny project export should produce files");
        assert!(out.iter().any(|(k, _)| k.ends_with(".dfy")));
    }

    #[test]
    fn proof_project_path_populates_fnid_keyed_proof_ir() {
        // Regression for the PR #142 follow-up: `build_project_ctx`
        // used to skip proof stages + drop the symbol table, so
        // `proof_*_files_project` exported multi-file proofs with
        // an empty ProofIR. Existing project tests only assert
        // files are produced; this one asserts the FnId-keyed proof
        // layer actually landed for a recursive fn in a dep module.
        let mut files: HashMap<String, String> = HashMap::new();
        files.insert(
            "helper.av".to_string(),
            r#"module Helper
    intent = "countdown"
    depends []

fn down(n: Int) -> Int
    match n
        0 -> 0
        _ -> down(n - 1)
"#
            .to_string(),
        );
        files.insert(
            "main.av".to_string(),
            r#"module Main
    intent = "use helper"
    depends [Helper]

fn main() -> Int
    Helper.down(3)
"#
            .to_string(),
        );

        let ctx = super::build_project_ctx(&files, "main.av", false)
            .expect("multi-file proof ctx should build");

        // The fix proper: symbol table is plumbed onto the multi-file
        // proof ctx (regression for the previous "build_context without
        // proof_ir / symbol_table" gap).
        let symbols = &ctx.symbol_table;
        let helper_down_id = symbols
            .fn_id_of(&crate::ir::FnKey::in_module("Helper".to_string(), "down"))
            .expect("SymbolTable must carry FnId for Helper.down");

        // Producer side: the FnId-keyed `fn_contracts` map must
        // hold the recursion contract for the dep-module fn.
        // Pre-fix this was empty because the pipeline ran without
        // `dep_modules` and `populate_fn_contracts` had nothing
        // to classify outside the entry scope.
        assert!(
            ctx.proof_ir.fn_contracts.contains_key(&helper_down_id),
            "fn_contracts should hold an entry for Helper.down (FnId={:?}); \
             got {} entries instead",
            helper_down_id,
            ctx.proof_ir.fn_contracts.len()
        );
    }

    #[test]
    fn compile_rust_project_handles_multi_file() {
        let files = load_rogue_files();
        let out = compile_rust_files_project(&files, "main.av")
            .expect("multi-file Rust export should succeed");
        assert!(out.contains_key("Cargo.toml"));
    }

    #[test]
    fn compiles_multi_file_rogue_from_virtual_fs() {
        let files = load_rogue_files();
        let bytes = compile_project_to_wasm(&files, "main.av")
            .expect("rogue project should compile from virtual fs");
        assert!(
            bytes.len() > 1000,
            "emitted wasm looks too small: {}",
            bytes.len()
        );
    }

    #[test]
    fn compiles_multi_file_wasm_gc_with_imported_type_in_record_field() {
        let mut files = HashMap::new();
        files.insert(
            "tmpreviewb.av".to_string(),
            r#"module TmpReviewB
    intent = "dependency with a sum type"
    exposes [Status, open]

type Status
    Open
    Closed

fn open() -> Status
    Status.Open
"#
            .to_string(),
        );
        files.insert(
            "main.av".to_string(),
            r#"module Main
    intent = "entry record stores an imported sum type"
    depends [TmpReviewB]

record Wrapper
    status: TmpReviewB.Status

fn make() -> Wrapper
    Wrapper(status = TmpReviewB.open())

fn main() -> Int
    match make().status
        TmpReviewB.Status.Open -> 1
        TmpReviewB.Status.Closed -> 0
"#
            .to_string(),
        );

        let bytes = compile_project_to_wasm(&files, "main.av")
            .expect("multi-file wasm-gc project should compile");
        let wat = wasmprinter::print_bytes(&bytes).expect("wasm-gc bytes should print");
        assert!(
            wat.contains("(struct"),
            "playground project compile should use wasm-gc, got:\n{}",
            wat
        );
    }

    #[test]
    fn multi_file_check_has_no_unknown_ident_noise() {
        let files = load_rogue_files();
        let report: serde_json::Value =
            serde_json::from_str(&check_project(&files, "main.av")).unwrap();
        let diagnostics = report["diagnostics"]
            .as_array()
            .cloned()
            .unwrap_or_default();
        let unknown_ident_on_deps: Vec<_> = diagnostics
            .iter()
            .filter(|d| d["slug"] == "unknown-ident")
            .filter(|d| {
                let s = d["summary"].as_str().unwrap_or("");
                ["Types", "Map", "Fov", "Combat", "Render", "Pathfinding"]
                    .iter()
                    .any(|name| s.contains(&format!("'{}'", name)))
            })
            .collect();
        assert!(
            unknown_ident_on_deps.is_empty(),
            "multi-file check still reports unknown-ident for declared deps: {:?}",
            unknown_ident_on_deps
        );
    }

    #[test]
    fn reports_missing_dep_clearly() {
        let mut files = HashMap::new();
        files.insert(
            "main.av".to_string(),
            [
                "module Main",
                "    intent = \"demo\"",
                "    depends [Missing]",
                "",
                "fn main() -> Unit",
                "    ! [Console.print]",
                "    Console.print(\"hi\")",
                "",
            ]
            .join("\n"),
        );
        let err = compile_project_to_wasm(&files, "main.av").unwrap_err();
        assert!(
            err.contains("Missing") || err.contains("not found"),
            "expected missing-module error, got: {}",
            err
        );
    }

    #[test]
    #[cfg(feature = "runtime")]
    fn audit_source_hostile_does_not_panic_on_showcase() {
        // Repro for the playground "unreachable executed" panic the
        // user hit when picking the Hostile-clock example and clicking
        // Audit. CLI works; WASM crashed. Run the same source through
        // the same pipeline the playground does so any panic surfaces
        // as a Rust test failure instead of a browser error message.
        let src = r#"module DeadlineCheck
    intent =
        "Demonstrate `aver verify --hostile`: the law passes under real time"
        "but breaks under the saturated-clock adversarial profile. Toggle the"
        "hostile checkbox next to Audit and watch the failure call out a"
        "missing `when` precondition or unpinned `given` for Time.unixMs."
    effects [Time.unixMs]

fn willCompleteBeforeDeadline(deadlineMs: Int) -> Bool
    ? "is the current time still before the deadline?"
    ! [Time.unixMs]
    Time.unixMs() < deadlineMs

verify willCompleteBeforeDeadline law deadlineHolds
    given d: Int = [9999999999999]
    willCompleteBeforeDeadline(d) => true
"#;
        let report = audit_source_hostile(src);
        assert!(
            !report.is_empty(),
            "audit_source_hostile returned empty payload"
        );
        assert!(
            report.contains("verify-hostile-mismatch"),
            "expected the hostile failures in the report; got: {}",
            &report[..report.len().min(400)]
        );
    }
}
