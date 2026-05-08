//! Wasm-gc counterpart of `vm_verify.rs`. Same case-pass/fail outcomes,
//! executed via wasmtime against a wasm-gc module instead of the Rust VM.
//!
//! The trick that keeps this small: equality between LHS and RHS happens
//! INSIDE wasm via a synthesized `__verify_X_check() -> Bool` helper.
//! The wasm-gc backend lowers `==` per-type via the eq_helpers registry,
//! so structural equality on Result/Option/Tuple/Record/List/Map/Vector
//! works without any host-side compound-value decoder. The host only
//! ever decodes a single `i32` per case.
//!
//! Trade-off: failure diagnostics show the source-code expressions
//! (`expr_to_str`) rather than actual runtime values. A future cut can
//! synthesize `__verify_X_left_repr() -> String` helpers and decode
//! Strings on fail; for now the Bool-only cut is enough to ship cross-
//! backend verify equivalence.
//!
//! Limits in this first cut:
//!   - cross-module `depends [...]` not supported (single-file only)
//!   - `.trace.*` projections not supported (would need host-side trace
//!     event capture; not wired through the wasm-gc effect imports yet)
//!   - actual-value rendering on fail (see above)

#![cfg(feature = "wasm")]

use crate::ast::{BinOp, Expr, FnBody, FnDef, Spanned, TopLevel, VerifyBlock, VerifyKind};
use crate::checker::{
    VerifyCaseOutcome, VerifyCaseResult, VerifyResult, expr_to_str, merge_verify_blocks,
};
use crate::config::ProjectConfig;
use crate::verify_law::expand::ExpansionMode;

pub fn run_verify_for_items_wasm_gc(
    items: Vec<TopLevel>,
    config: Option<ProjectConfig>,
    base_dir: Option<&str>,
    source_file: &str,
) -> Result<Vec<VerifyResult>, String> {
    run_verify_for_items_wasm_gc_with_mode(
        items,
        config,
        base_dir,
        source_file,
        ExpansionMode::Declared,
    )
}

pub fn run_verify_for_items_wasm_gc_with_mode(
    mut items: Vec<TopLevel>,
    _config: Option<ProjectConfig>,
    base_dir: Option<&str>,
    _source_file: &str,
    mode: ExpansionMode,
) -> Result<Vec<VerifyResult>, String> {
    use super::vm_verify::{
        apply_hostile_expansion, format_type_errors, inject_hostile_effect_stubs_for_blocks,
    };

    crate::ir::pipeline::tco(&mut items);

    if mode == ExpansionMode::Hostile {
        let preview = merge_verify_blocks(&items);
        inject_hostile_effect_stubs_for_blocks(&mut items, &preview);
    }

    let tc =
        crate::ir::pipeline::typecheck(&items, &crate::ir::TypecheckMode::Full { base_dir });
    if !tc.errors.is_empty() {
        return Err(format_type_errors(&tc.errors));
    }

    let mut blocks = merge_verify_blocks(&items);
    if blocks.is_empty() {
        return Ok(vec![]);
    }

    if mode == ExpansionMode::Hostile {
        for b in &mut blocks {
            apply_hostile_expansion(b, &items)?;
        }
    }

    // Trace mode requires host-side event capture; the wasm-gc effect
    // import wiring doesn't surface a recorder API into verify yet.
    // Reject early with a pointer to VM verify.
    for block in &blocks {
        if block.trace {
            return Err(format!(
                "verify --wasm-gc: trace projections (`.trace.*`) not yet supported (block at line {}). Use `aver verify` (VM) for trace cases.",
                block.line
            ));
        }
    }

    // Effect-Oracle stubs (user-given a non-Output classified effect) need
    // per-case linker import remapping in wasmtime. The VM uses
    // `install_oracle_stubs` runtime override; wasm imports are bound at
    // instantiation, so we'd have to re-instantiate per case (or rewrite
    // call sites to dispatch through a host-controlled table). Out of
    // scope for this cut. Pure verify cases and value-givens work fine.
    use crate::ast::VerifyKind;
    use crate::types::checker::effect_classification::{EffectDimension, classify};
    for block in &blocks {
        let givens = match &block.kind {
            VerifyKind::Law(law) => law.givens.as_slice(),
            VerifyKind::Cases => block.cases_givens.as_slice(),
        };
        for given in givens {
            if let Some(c) = classify(&given.type_name)
                && !matches!(c.dimension, EffectDimension::Output)
            {
                return Err(format!(
                    "verify --wasm-gc: effect Oracle stubs (`given {}: {} = ...`) not yet supported (block at line {}). Use `aver verify` (VM) for cases that mock classified effects.",
                    given.name, given.type_name, block.line
                ));
            }
        }
    }

    let plans = build_verify_wasm_gc_plans(&mut items, &blocks);

    // After helper synthesis, run the full wasm-gc-compatible pipeline:
    // typecheck stamps `ty` on the freshly synthesized `BinOp(Eq, ...)`
    // and `Result.Ok(...)` Spanneds (wasm-gc codegen panics on un-typed
    // nodes), the neutral alloc policy threads alloc info through every
    // fn (including helpers), and resolve maps slots. Pre-existing items
    // are OnceLock-set so the second typecheck pass is a no-op for them.
    let neutral_policy = crate::ir::NeutralAllocPolicy;
    let result = crate::ir::pipeline::run(
        &mut items,
        crate::ir::PipelineConfig {
            typecheck: Some(crate::ir::TypecheckMode::Full { base_dir }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        return Err(format_type_errors(&tc.errors));
    }

    // Cross-module support: load every transitive `depends [...]`
    // module, run them through the same wasm-gc-compatible pipeline,
    // flatten into the entry items so `compile_to_wasm_gc` sees one
    // self-contained AST. Mirrors the `try_run_wasm_gc` setup in
    // `src/main/run_wasm_gc.rs`.
    let dep_modules = if let Some(root) = base_dir {
        load_compile_deps(&items, root)?
    } else {
        Vec::new()
    };
    if !dep_modules.is_empty() {
        crate::codegen::wasm_gc::flatten_multimodule(&mut items, &dep_modules);
        crate::ir::pipeline::resolve(&mut items);
    }

    let bytes = crate::codegen::wasm_gc::compile_to_wasm_gc(&items, result.analysis.as_ref())
        .map_err(|e| format!("wasm-gc compile error: {}", e))?;

    run_verify_cases_in_wasmtime(&bytes, &plans)
}

struct WasmGcVerifyCaseFns {
    /// Synthesized `__verify_X_check() -> Bool` — wasm-side `==`.
    check: String,
    /// Synthesized `__verify_X_guard() -> Bool` if the case has a `when`
    /// clause or hostile-profile rebinding.
    guard: Option<String>,
    /// Source-code rendering of the case (`lhs == rhs`) for diagnostics.
    case_expr: String,
    /// Pre-computed `expected` rendering used in `Mismatch` outcomes.
    expected_str: String,
}

struct WasmGcVerifyPlan {
    block: VerifyBlock,
    cases: Vec<WasmGcVerifyCaseFns>,
}

/// Local Bool-returning helper synth — wasm-gc typechecker rejects
/// the VM's declared-Unit shape because the body type (`Bool`) doesn't
/// match. The VM verify path uses `make_verify_helper` with declared
/// `return_type: "Unit"` and a typecheck mode that's lenient on the
/// mismatch; wasm-gc's full typecheck pass demands the declaration
/// match the body, so we emit `-> Bool` explicitly here.
fn make_verify_bool_helper(name: String, line: usize, body_expr: Spanned<Expr>) -> TopLevel {
    use std::sync::Arc as Rc;
    TopLevel::FnDef(FnDef {
        name,
        line,
        params: vec![],
        return_type: "Bool".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(body_expr)),
        resolution: None,
    })
}

fn build_verify_wasm_gc_plans(
    items: &mut Vec<TopLevel>,
    verify_blocks: &[VerifyBlock],
) -> Vec<WasmGcVerifyPlan> {
    use super::vm_verify::guard_for_case;

    let mut plans = Vec::with_capacity(verify_blocks.len());

    for (block_idx, block) in verify_blocks.iter().enumerate() {
        let mut case_plans = Vec::with_capacity(block.cases.len());
        let sample_guards = match &block.kind {
            VerifyKind::Law(law) => Some(&law.sample_guards),
            VerifyKind::Cases => None,
        };

        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let check_name = format!("{}_check", prefix);

            // `__verify_X_check() -> Bool` returns `lhs == rhs`. Wasm-gc
            // backend lowers the `==` per-type via eq_helpers, so this
            // works for primitive and compound shapes alike.
            let check_expr = Spanned::new(
                Expr::BinOp(
                    BinOp::Eq,
                    Box::new(left_expr.clone()),
                    Box::new(right_expr.clone()),
                ),
                block.line,
            );
            items.push(make_verify_bool_helper(
                check_name.clone(),
                block.line,
                check_expr,
            ));

            // Guard helper, same shape as VM verify.
            let guard_expr = guard_for_case(block, case_idx)
                .or_else(|| sample_guards.and_then(|gs| gs.get(case_idx)).cloned());
            let guard_name = guard_expr.map(|guard_expr| {
                let name = format!("{}_guard", prefix);
                items.push(make_verify_bool_helper(name.clone(), block.line, guard_expr));
                name
            });

            let lhs_str = expr_to_str(&left_expr);
            let rhs_str = expr_to_str(&right_expr);
            let case_expr = format!("{} == {}", lhs_str, rhs_str);

            case_plans.push(WasmGcVerifyCaseFns {
                check: check_name,
                guard: guard_name,
                case_expr,
                expected_str: rhs_str,
            });
        }

        plans.push(WasmGcVerifyPlan {
            block: block.clone(),
            cases: case_plans,
        });
    }

    plans
}

fn run_verify_cases_in_wasmtime(
    bytes: &[u8],
    plans: &[WasmGcVerifyPlan],
) -> Result<Vec<VerifyResult>, String> {
    use wasmtime::{Caller, Config, Engine, ExternType, FuncType, Linker, Module, Store, Val, ValType};

    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    let engine = Engine::new(&config).map_err(|e| format!("wasmtime engine: {}", e))?;

    let module = Module::new(&engine, bytes)
        .map_err(|e| format!("wasm-gc module load failed: {}", e))?;

    let mut store: Store<()> = Store::new(&engine, ());
    let mut linker: Linker<()> = Linker::new(&engine);

    // Stub every declared import as a typed-zero return. Verify cases
    // shouldn't actually invoke real effects — Oracle stubs are Aver-
    // side FnDefs (compiled into the module body). The linker still
    // requires every import resolved before instantiation.
    for import in module.imports() {
        let ExternType::Func(ft) = import.ty() else {
            continue;
        };
        let module_name = import.module().to_string();
        let field_name = import.name().to_string();
        let result_tys: Vec<ValType> = ft.results().collect();
        let func_ty = FuncType::new(&engine, ft.params(), ft.results());
        linker
            .func_new(
                &module_name,
                &field_name,
                func_ty,
                move |_caller: Caller<'_, ()>, _params: &[Val], results: &mut [Val]| {
                    for (slot, ty) in results.iter_mut().zip(result_tys.iter()) {
                        *slot = match ty {
                            ValType::I32 => Val::I32(0),
                            ValType::I64 => Val::I64(0),
                            ValType::F32 => Val::F32(0),
                            ValType::F64 => Val::F64(0),
                            ValType::V128 => Val::V128(0u128.into()),
                            ValType::Ref(_) => Val::AnyRef(None),
                        };
                    }
                    Ok(())
                },
            )
            .map_err(|e| format!("wasm-gc verify: linker stub for `{}::{}`: {}", module_name, field_name, e))?;
    }

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| format!("wasm-gc instantiate: {}", e))?;

    let mut results = Vec::with_capacity(plans.len());

    for plan in plans {
        let mut case_results = Vec::with_capacity(plan.cases.len());
        let mut passed = 0usize;
        let mut failed = 0usize;
        let mut skipped = 0usize;
        let case_total = plan.cases.len();

        for (idx, case) in plan.cases.iter().enumerate() {
            let from_hostile = plan
                .block
                .case_hostile_origins
                .get(idx)
                .copied()
                .unwrap_or(false);

            // Guard — when present and false, skip the case.
            if let Some(gname) = &case.guard {
                match invoke_bool(&mut store, &instance, gname) {
                    Ok(b) => {
                        if !b {
                            skipped += 1;
                            case_results.push(VerifyCaseResult {
                                outcome: VerifyCaseOutcome::Skipped,
                                span: None,
                                case_expr: case.case_expr.clone(),
                                case_index: idx,
                                case_total,
                                law_context: None,
                                from_hostile,
                                hostile_profile: None,
                            });
                            continue;
                        }
                    }
                    Err(e) => {
                        failed += 1;
                        case_results.push(VerifyCaseResult {
                            outcome: VerifyCaseOutcome::RuntimeError {
                                error: format!("guard: {}", e),
                            },
                            span: None,
                            case_expr: case.case_expr.clone(),
                            case_index: idx,
                            case_total,
                            law_context: None,
                            from_hostile,
                            hostile_profile: None,
                        });
                        continue;
                    }
                }
            }

            // Check — wasm computes lhs == rhs natively, host decodes Bool.
            let outcome = match invoke_bool(&mut store, &instance, &case.check) {
                Ok(true) => {
                    passed += 1;
                    VerifyCaseOutcome::Pass
                }
                Ok(false) => {
                    failed += 1;
                    VerifyCaseOutcome::Mismatch {
                        expected: case.expected_str.clone(),
                        actual: "<lhs ≠ rhs (wasm-gc verify reports pass/fail only — actual value rendering is a follow-up)>".to_string(),
                    }
                }
                Err(e) => {
                    failed += 1;
                    VerifyCaseOutcome::RuntimeError { error: e }
                }
            };
            case_results.push(VerifyCaseResult {
                outcome,
                span: None,
                case_expr: case.case_expr.clone(),
                case_index: idx,
                case_total,
                law_context: None,
                from_hostile,
                hostile_profile: None,
            });
        }

        results.push(VerifyResult {
            fn_name: plan.block.fn_name.clone(),
            block_label: plan.block.fn_name.clone(),
            passed,
            failed,
            skipped,
            case_results,
            failures: Vec::new(),
        });
    }

    Ok(results)
}

fn load_compile_deps(
    items: &[TopLevel],
    module_root: &str,
) -> Result<Vec<crate::codegen::ModuleInfo>, String> {
    let module = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m),
        _ => None,
    });
    let Some(module) = module else {
        return Ok(vec![]);
    };
    let mut result = Vec::new();
    let mut loaded = std::collections::HashSet::new();
    for dep_name in &module.depends {
        load_module_recursive(dep_name, module_root, &mut result, &mut loaded)?;
    }
    Ok(result)
}

fn load_module_recursive(
    name: &str,
    module_root: &str,
    result: &mut Vec<crate::codegen::ModuleInfo>,
    loaded: &mut std::collections::HashSet<String>,
) -> Result<(), String> {
    if !loaded.insert(name.to_string()) {
        return Ok(());
    }

    let path = crate::source::find_module_file(name, module_root).ok_or_else(|| {
        format!(
            "Cannot find module '{}' in module root '{}'",
            name, module_root
        )
    })?;
    let source = std::fs::read_to_string(&path)
        .map_err(|e| format!("Read '{}': {}", path.display(), e))?;
    let mut items = crate::source::parse_source(&source)
        .map_err(|e| format!("Parse '{}': {}", path.display(), e))?;
    crate::source::require_module_declaration(&items, path.to_str().unwrap_or(name))?;

    let neutral_policy = crate::ir::NeutralAllocPolicy;
    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        crate::ir::PipelineConfig {
            typecheck: Some(crate::ir::TypecheckMode::Full {
                base_dir: Some(module_root),
            }),
            run_interp_lower: false,
            run_buffer_build: false,
            alloc_policy: Some(&neutral_policy),
            ..Default::default()
        },
    );
    if let Some(tc) = pipeline_result.typecheck.as_ref()
        && !tc.errors.is_empty()
    {
        return Err(format!(
            "Type errors in dependency module '{}':\n{}",
            name,
            tc.errors
                .iter()
                .map(|e| format!("  {}:{}: {}", e.line, e.col, e.message))
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }

    let transitive: Vec<String> = items
        .iter()
        .find_map(|i| match i {
            TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    for dep in &transitive {
        load_module_recursive(dep, module_root, result, loaded)?;
    }

    let depends = transitive;
    let type_defs: Vec<_> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::TypeDef(td) => Some(td.clone()),
            _ => None,
        })
        .collect();
    let fn_defs: Vec<_> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name != "main" => Some(fd.clone()),
            _ => None,
        })
        .collect();

    result.push(crate::codegen::ModuleInfo {
        prefix: name.to_string(),
        depends,
        type_defs,
        fn_defs,
        analysis: pipeline_result.analysis,
    });
    Ok(())
}

fn invoke_bool(
    store: &mut wasmtime::Store<()>,
    instance: &wasmtime::Instance,
    fn_name: &str,
) -> Result<bool, String> {
    let func = instance
        .get_func(&mut *store, fn_name)
        .ok_or_else(|| format!("wasm-gc verify: export `{}` not found", fn_name))?;
    let mut out = vec![wasmtime::Val::I32(0); 1];
    func.call(&mut *store, &[], &mut out)
        .map_err(|e| format!("wasm-gc verify: call `{}` failed: {}", fn_name, e))?;
    match &out[0] {
        wasmtime::Val::I32(n) => Ok(*n != 0),
        v => Err(format!(
            "wasm-gc verify: `{}` returned non-Bool {:?}",
            fn_name, v
        )),
    }
}
