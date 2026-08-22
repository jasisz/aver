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

use crate::ast::{
    BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, TopLevel, VerifyBlock,
    VerifyKind,
};
use crate::checker::{
    VerifyCaseOutcome, VerifyCaseResult, VerifyResult, expr_to_str, merge_verify_blocks,
};
use crate::config::ProjectConfig;
use crate::diagnostics::vm_verify::CaseBudget;
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
    config: Option<ProjectConfig>,
    base_dir: Option<&str>,
    source_file: &str,
    mode: ExpansionMode,
) -> Result<Vec<VerifyResult>, String> {
    use super::vm_verify::{
        apply_hostile_expansion_with_registry, format_type_errors,
        inject_hostile_effect_stubs_for_blocks,
    };

    // Pre-flight rejects for features the wasm-gc backend can't yet
    // compile or dispatch. Each one points the user at VM verify.
    use crate::ast::VerifyKind;
    use crate::types::checker::effect_classification::{EffectDimension, classify};
    let preview_blocks = merge_verify_blocks(&items);
    for block in &preview_blocks {
        if block.trace {
            return Err(format!(
                "verify --wasm-gc: trace projections (`.trace.*`) not yet supported (block at line {}). Use `aver verify` (VM) for trace cases.",
                block.line
            ));
        }
        let givens = match &block.kind {
            VerifyKind::Law(law) => law.givens.as_slice(),
            VerifyKind::Cases => block.cases_givens.as_slice(),
        };
        for given in givens {
            if let Some(c) = classify(&given.type_name)
                && !matches!(c.dimension, EffectDimension::Output)
            {
                return Err(format!(
                    "verify --wasm-gc: effect Oracle stubs (`given {}: {} = ...`) not yet supported (block at line {}). The wasm-gc backend has no `BranchPath` runtime and no per-case linker remap, so classified-effect dispatch can't override the import. Use `aver verify` (VM) for cases that mock classified effects.",
                    given.name, given.type_name, block.line
                ));
            }
        }
        // Cases that mention `BranchPath` directly in expressions can't
        // compile to wasm-gc — the resolver leaves namespace idents like
        // `Ident("BranchPath")` un-Resolved and the wasm-gc emitter
        // explicitly bails on bare Idents (it has no namespace dispatch
        // path; VM uses arena-side namespace lookup at runtime).
        for (lhs, rhs) in &block.cases {
            if expr_mentions_ident(lhs, "BranchPath") || expr_mentions_ident(rhs, "BranchPath") {
                return Err(format!(
                    "verify --wasm-gc: case mentions `BranchPath` (block at line {}). The wasm-gc backend has no namespace-value dispatch — Oracle counter-tracking is VM-only. Use `aver verify` (VM) for this block.",
                    block.line
                ));
            }
        }
    }
    drop(preview_blocks);

    crate::ir::pipeline::tco(&mut items);

    // End of the program the user wrote — see `vm_verify`'s disk path.
    let user_program_len = items.len();

    if mode == ExpansionMode::Hostile {
        let preview = merge_verify_blocks(&items);
        inject_hostile_effect_stubs_for_blocks(&mut items, &preview);
    }

    // The same gate the VM verify door and every other front door use:
    // type errors AND the shadowing ban (#954).
    let tc = crate::ir::pipeline::typecheck_gate(
        &items,
        &crate::ir::TypecheckMode::Full { base_dir },
        &items[..user_program_len],
    );
    if !tc.errors.is_empty() {
        return Err(format_type_errors(&tc.errors));
    }

    let mut blocks = merge_verify_blocks(&items);
    if blocks.is_empty() {
        return Ok(vec![]);
    }

    if mode == ExpansionMode::Hostile {
        // Same ceiling the VM lane uses, resolved per block for the same
        // reason: `[[verify.costly]]` names one function.
        let key = crate::diagnostics::vm_verify::costly_glob_key(source_file, base_dir);
        for b in &mut blocks {
            let max_cases =
                crate::diagnostics::vm_verify::max_cases_for(config.as_ref(), &b.fn_name, &key);
            apply_hostile_expansion_with_registry(b, &items, &tc.capabilities, max_cases)?;
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
            run_chars_fusion: false,
            run_string_index: true,
            run_list_build: false,
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
        crate::source::load_compile_deps(&items, root)?
    } else {
        Vec::new()
    };
    let mut type_aliases = std::collections::HashMap::new();
    if !dep_modules.is_empty() {
        type_aliases = crate::codegen::wasm_gc::flatten_multimodule(
            &mut items,
            &dep_modules,
            &result
                .typecheck
                .as_ref()
                .expect("wasm-gc verify pipeline requested typechecking")
                .capabilities,
            crate::codegen::wasm_gc::CapabilityFunctionSurface::Verification,
        );
        // `_and_reannotate`: a bare post-flatten re-resolve wipes
        // `aliased_slots` and the wasm-gc in-place fast path goes
        // wrong (#950).
        crate::ir::pipeline::resolve_and_reannotate(&mut items);
    }

    let bytes = crate::codegen::wasm_gc::compile_to_wasm_gc_flattened(
        &items,
        result.analysis.as_ref(),
        None,
        crate::codegen::wasm_gc::TargetMode::AverBridge,
        &type_aliases,
    )
    .map(|out| out.bytes)
    .map_err(|e| format!("wasm-gc compile error: {}", e))?;

    // Same resolution the VM lane performs, over the same `aver.toml`, so a
    // case is runnable on both lanes or on neither.
    let key = crate::diagnostics::vm_verify::costly_glob_key(source_file, base_dir);
    let budgets: Vec<CaseBudget> = plans
        .iter()
        .map(|plan| CaseBudget::resolve(config.as_ref(), &plan.block.fn_name, &key))
        .collect();
    let raised_by: Vec<Option<String>> = budgets
        .iter()
        .map(|budget| budget.raised_by_fn(config.as_ref()))
        .collect();

    run_verify_cases_in_wasmtime(&bytes, &plans, &budgets, &raised_by)
}

struct WasmGcVerifyCaseFns {
    /// Synthesized `__verify_X_check() -> Bool` — wasm-side `==`.
    check: String,
    /// Synthesized `__verify_X_guard() -> Bool` if the case has a `when`
    /// clause or hostile-profile rebinding.
    guard: Option<String>,
    /// Synthesized `__verify_X_left_repr() -> String` when the LHS type
    /// is a primitive (Int/Float/Bool/Str) so the host can render the
    /// actual runtime value on fail. None for compound types — falls
    /// back to source-text rendering.
    left_repr: Option<String>,
    right_repr: Option<String>,
    /// Source-code rendering of the case (`lhs == rhs`) for diagnostics.
    case_expr: String,
    /// Source-code rendering of the LHS / RHS expressions, used as a
    /// fallback when no runtime repr helper was synthesized.
    lhs_src: String,
    rhs_src: String,
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
fn make_verify_bool_helper(
    name: String,
    line: usize,
    body_expr: Spanned<Expr>,
    effects: Vec<Spanned<String>>,
) -> TopLevel {
    use std::sync::Arc as Rc;
    TopLevel::FnDef(FnDef {
        name,
        line,
        params: vec![],
        return_type: "Bool".to_string(),
        effects,
        desc: None,
        body: Rc::new(FnBody::from_expr(body_expr)),
        resolution: None,
    })
}

fn make_verify_string_helper(
    name: String,
    line: usize,
    body_expr: Spanned<Expr>,
    effects: Vec<Spanned<String>>,
) -> TopLevel {
    use std::sync::Arc as Rc;
    TopLevel::FnDef(FnDef {
        name,
        line,
        params: vec![],
        return_type: "String".to_string(),
        effects,
        desc: None,
        body: Rc::new(FnBody::from_expr(body_expr)),
        resolution: None,
    })
}

/// Collect the effect declaration of the `verify`-block's target fn so
/// the synthesized helpers (`__verify_X_check`, `__verify_X_guard`,
/// `__verify_X_*_repr`) inherit the same effect surface. Without this,
/// the wasm-gc full typecheck rejects helpers with declared `effects:
/// []` that transitively call effect-y fns (`twoFloatsDistinct` calls
/// `Random.float` → check helper indirectly does too).
fn collect_block_fn_effects(items: &[TopLevel], fn_name: &str) -> Vec<Spanned<String>> {
    items
        .iter()
        .find_map(|item| match item {
            TopLevel::FnDef(fd) if fd.name == fn_name => Some(fd.effects.clone()),
            _ => None,
        })
        .unwrap_or_default()
}

/// Synthesize a `String`-typed expression that renders the user's
/// LHS / RHS expression at runtime. Returns `None` for types we can't
/// stringify with stdlib primitives. Compound types (List/Map/Vector/
/// Variant/Record) fall back to source-text rendering on fail.
fn repr_expr_via_clone(expr: &Spanned<Expr>, line: usize) -> Option<Spanned<Expr>> {
    use crate::types::Type;
    let ty = expr.ty()?;
    let mk_attr_call = |module: &str, method: &str| {
        let callee = Spanned::new(
            Expr::Attr(
                Box::new(Spanned::new(Expr::Ident(module.to_string()), line)),
                method.to_string(),
            ),
            line,
        );
        Spanned::new(Expr::FnCall(Box::new(callee), vec![expr.clone()]), line)
    };
    match ty {
        Type::Int => Some(mk_attr_call("String", "fromInt")),
        Type::Float => Some(mk_attr_call("String", "fromFloat")),
        Type::Str => Some(expr.clone()),
        Type::Bool => Some(Spanned::new(
            Expr::Match {
                subject: Box::new(expr.clone()),
                arms: vec![
                    MatchArm::new(
                        Pattern::Literal(Literal::Bool(true)),
                        Spanned::new(Expr::Literal(Literal::Str("true".to_string())), line),
                    ),
                    MatchArm::new(
                        Pattern::Literal(Literal::Bool(false)),
                        Spanned::new(Expr::Literal(Literal::Str("false".to_string())), line),
                    ),
                ],
            },
            line,
        )),
        _ => None,
    }
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
        let block_effects = collect_block_fn_effects(items, &block.fn_name);

        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let check_name = format!("{}_check", prefix);

            // `__verify_X_check() -> Bool` returns `lhs == rhs`. Wasm-gc
            // backend lowers `==` per-type via eq_helpers, so structural
            // equality on Result/Option/Tuple/Record/List/Map/Vector
            // works without a host-side decoder. Bare BinOp on cloned
            // sub-expressions; the second pipeline pass restamps `ty`.
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
                block_effects.clone(),
            ));

            // Guard helper, same shape as VM verify.
            let guard_expr = guard_for_case(block, case_idx)
                .or_else(|| sample_guards.and_then(|gs| gs.get(case_idx)).cloned());
            let guard_name = guard_expr.map(|guard_expr| {
                let name = format!("{}_guard", prefix);
                items.push(make_verify_bool_helper(
                    name.clone(),
                    block.line,
                    guard_expr,
                    block_effects.clone(),
                ));
                name
            });

            // Repr helpers — only for primitive return types. Compound
            // types fall through to source-text rendering on fail.
            let left_repr = repr_expr_via_clone(&left_expr, block.line).map(|repr_body| {
                let name = format!("{}_left_repr", prefix);
                items.push(make_verify_string_helper(
                    name.clone(),
                    block.line,
                    repr_body,
                    block_effects.clone(),
                ));
                name
            });
            let right_repr = repr_expr_via_clone(&right_expr, block.line).map(|repr_body| {
                let name = format!("{}_right_repr", prefix);
                items.push(make_verify_string_helper(
                    name.clone(),
                    block.line,
                    repr_body,
                    block_effects.clone(),
                ));
                name
            });

            let lhs_src = expr_to_str(&left_expr);
            let rhs_src = expr_to_str(&right_expr);
            let case_expr = format!("{} == {}", lhs_src, rhs_src);

            case_plans.push(WasmGcVerifyCaseFns {
                check: check_name,
                guard: guard_name,
                left_repr,
                right_repr,
                case_expr,
                lhs_src,
                rhs_src,
            });
        }

        plans.push(WasmGcVerifyPlan {
            block: block.clone(),
            cases: case_plans,
        });
    }

    plans
}

/// Wasm instructions one VM opcode is worth.
///
/// The two lanes do not share an instruction set: an Aver opcode the VM
/// dispatches once lowers to a handful of wasm instructions, so equal
/// counters would make the wasm lane the stricter of the two and a case
/// runnable under `aver verify` could be declined under `aver verify
/// --wasm-gc`. One factor, documented here and applied at the single
/// `set_fuel` call below, is what keeps "which cases are runnable" a
/// property of the project's budget rather than of the backend.
///
/// 10 reproduces the fuel the wasm lane used when it carried its own
/// constant (10M against the VM's 1M default), so no case changes lane
/// on the day the two numbers were joined.
pub const WASM_FUEL_PER_VM_STEP: u64 = 10;

/// Fuel for one case, from the step budget that case runs under.
pub fn case_fuel(step_limit: u64) -> u64 {
    step_limit.saturating_mul(WASM_FUEL_PER_VM_STEP)
}

/// Why a wasm-gc verify helper call produced no value. Mirrors the VM
/// lane's `VerifyCallFailure`: running out of fuel means the case was not
/// answered, everything else means it failed.
enum WasmCallFailure {
    OutOfFuel,
    Error(String),
}

impl WasmCallFailure {
    fn message(self) -> String {
        match self {
            WasmCallFailure::OutOfFuel => "wasm-gc verify: out of fuel".to_string(),
            WasmCallFailure::Error(message) => message,
        }
    }

    fn from_call_error(fn_name: &str, error: wasmtime::Error) -> Self {
        if matches!(
            error.downcast_ref::<wasmtime::Trap>(),
            Some(wasmtime::Trap::OutOfFuel)
        ) {
            return WasmCallFailure::OutOfFuel;
        }
        WasmCallFailure::Error(format!(
            "wasm-gc verify: call `{}` failed: {}",
            fn_name, error
        ))
    }
}

fn run_verify_cases_in_wasmtime(
    bytes: &[u8],
    plans: &[WasmGcVerifyPlan],
    budgets: &[CaseBudget],
    raised_by: &[Option<String>],
) -> Result<Vec<VerifyResult>, String> {
    use wasmtime::{
        Caller, Config, Engine, ExternType, FuncType, Linker, Module, Store, Val, ValType,
    };

    let mut config = Config::new();
    config.wasm_gc(true);
    config.wasm_tail_call(true);
    config.wasm_function_references(true);
    config.wasm_reference_types(true);
    config.wasm_multi_value(true);
    config.wasm_bulk_memory(true);
    // Mirror the VM-side `step_limit` cap: per-case fuel budget so a
    // tail-recursive user fn (AFL byte-havoc favourite) bails as a
    // `RuntimeError` instead of pinning wasmtime. Reset before every
    // case below so cases don't share budget.
    config.consume_fuel(true);
    let engine = Engine::new(&config).map_err(|e| format!("wasmtime engine: {}", e))?;

    let module =
        Module::new(&engine, bytes).map_err(|e| format!("wasm-gc module load failed: {}", e))?;

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
            .map_err(|e| {
                format!(
                    "wasm-gc verify: linker stub for `{}::{}`: {}",
                    module_name, field_name, e
                )
            })?;
    }

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| format!("wasm-gc instantiate: {}", e))?;

    let mut results = Vec::with_capacity(plans.len());

    for (plan_idx, plan) in plans.iter().enumerate() {
        let budget = budgets
            .get(plan_idx)
            .copied()
            .unwrap_or_else(CaseBudget::compiled_default);
        let raised = raised_by.get(plan_idx).and_then(|r| r.as_deref());
        let fuel = case_fuel(budget.limit);
        let mut case_results = Vec::with_capacity(plan.cases.len());
        let mut passed = 0usize;
        let mut failed = 0usize;
        let mut skipped = 0usize;
        let mut declined = 0usize;
        let case_total = plan.cases.len();

        for (idx, case) in plan.cases.iter().enumerate() {
            let from_hostile = plan
                .block
                .case_hostile_origins
                .get(idx)
                .copied()
                .unwrap_or(false);

            // Refuel before each case. Guard + check + repr helpers all
            // share this one budget; a single tail-recursive user fn
            // consumes it and triggers `Trap::OutOfFuel` ≈ a few hundred
            // ms instead of looping forever. Mirrors the VM's per-call
            // `step_limit` reset in `Machine::run_named_function`.
            //
            // The single conversion point between the two lanes: the project
            // sets ONE per-case step budget and this is where the wasm lane
            // reads it.
            store
                .set_fuel(fuel)
                .map_err(|e| format!("wasm-gc verify: set_fuel: {}", e))?;

            // Guard — when present and false, skip the case.
            if let Some(gname) = &case.guard {
                match invoke_bool(&mut store, &instance, gname) {
                    Err(WasmCallFailure::OutOfFuel) => {
                        declined += 1;
                        case_results.push(VerifyCaseResult {
                            outcome: VerifyCaseOutcome::Declined {
                                reason: crate::diagnostics::vm_verify::decline_reason(
                                    &budget, raised,
                                ),
                                steps: fuel_used(&mut store, fuel),
                                limit: fuel,
                                raised_by: raised.map(str::to_string),
                            },
                            span: None,
                            case_expr: case.case_expr.clone(),
                            case_index: idx,
                            case_total,
                            law_context: None,
                            from_hostile,
                            hostile_profile: None,
                            expected_value: None,
                            steps: fuel_used(&mut store, fuel),
                        });
                        continue;
                    }
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
                                expected_value: None,
                                steps: fuel_used(&mut store, fuel),
                            });
                            continue;
                        }
                    }
                    Err(e) => {
                        failed += 1;
                        case_results.push(VerifyCaseResult {
                            outcome: VerifyCaseOutcome::RuntimeError {
                                error: format!("guard: {}", e.message()),
                            },
                            span: None,
                            case_expr: case.case_expr.clone(),
                            case_index: idx,
                            case_total,
                            law_context: None,
                            from_hostile,
                            hostile_profile: None,
                            expected_value: None,
                            steps: fuel_used(&mut store, fuel),
                        });
                        continue;
                    }
                }
            }

            // Check — wasm computes lhs == rhs natively, host decodes Bool.
            let outcome = match invoke_bool(&mut store, &instance, &case.check) {
                Err(WasmCallFailure::OutOfFuel) => {
                    declined += 1;
                    VerifyCaseOutcome::Declined {
                        reason: crate::diagnostics::vm_verify::decline_reason(&budget, raised),
                        steps: fuel_used(&mut store, fuel),
                        limit: fuel,
                        raised_by: raised.map(str::to_string),
                    }
                }
                Ok(true) => {
                    passed += 1;
                    VerifyCaseOutcome::Pass
                }
                Ok(false) => {
                    failed += 1;
                    let expected = match &case.right_repr {
                        Some(name) => match invoke_string(&mut store, &instance, name) {
                            Ok(s) => s,
                            Err(_) => case.rhs_src.clone(),
                        },
                        None => case.rhs_src.clone(),
                    };
                    let actual = match &case.left_repr {
                        Some(name) => match invoke_string(&mut store, &instance, name) {
                            Ok(s) => s,
                            Err(_) => format!(
                                "<{}: wasm-gc compound-value repr is a follow-up>",
                                case.lhs_src
                            ),
                        },
                        None => format!(
                            "<{}: wasm-gc compound-value repr is a follow-up>",
                            case.lhs_src
                        ),
                    };
                    VerifyCaseOutcome::Mismatch { expected, actual }
                }
                Err(e) => {
                    failed += 1;
                    VerifyCaseOutcome::RuntimeError { error: e.message() }
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
                expected_value: None,
                steps: fuel_used(&mut store, fuel),
            });
        }

        results.push(VerifyResult {
            fn_name: plan.block.fn_name.clone(),
            is_law: matches!(&plan.block.kind, crate::ast::VerifyKind::Law(_)),
            block_label: plan.block.fn_name.clone(),
            passed,
            failed,
            skipped,
            declined,
            budget: crate::checker::VerifyBudgetInfo {
                limit: fuel,
                default_limit: case_fuel(budget.default_limit),
                raised_by: raised.map(str::to_string),
            },
            case_results,
            failures: Vec::new(),
        });
    }

    Ok(results)
}

/// Recursive scan: does `expr` (or any subexpression) reference the
/// bare ident `name`? Used to gate the BranchPath upfront reject —
/// wasm-gc has no BranchPath primitive.
fn expr_mentions_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n == name,
        Expr::Attr(inner, _) => expr_mentions_ident(inner, name),
        Expr::FnCall(callee, args) => {
            expr_mentions_ident(callee, name) || args.iter().any(|a| expr_mentions_ident(a, name))
        }
        Expr::BinOp(_, a, b) => expr_mentions_ident(a, name) || expr_mentions_ident(b, name),
        Expr::Neg(inner) => expr_mentions_ident(inner, name),
        Expr::Match { subject, arms } => {
            expr_mentions_ident(subject, name)
                || arms.iter().any(|a| expr_mentions_ident(&a.body, name))
        }
        Expr::Constructor(c, payload) => {
            c.split('.').next() == Some(name)
                || payload
                    .as_ref()
                    .map(|p| expr_mentions_ident(p, name))
                    .unwrap_or(false)
        }
        Expr::ErrorProp(inner) => expr_mentions_ident(inner, name),
        Expr::List(items) | Expr::Tuple(items) => {
            items.iter().any(|e| expr_mentions_ident(e, name))
        }
        Expr::MapLiteral(pairs) => pairs
            .iter()
            .any(|(k, v)| expr_mentions_ident(k, name) || expr_mentions_ident(v, name)),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, e)| expr_mentions_ident(e, name))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_mentions_ident(base, name)
                || updates.iter().any(|(_, e)| expr_mentions_ident(e, name))
        }
        Expr::TailCall(tc) => tc.args.iter().any(|a| expr_mentions_ident(a, name)),
        Expr::IndependentProduct(items, _) => items.iter().any(|e| expr_mentions_ident(e, name)),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            crate::ast::StrPart::Parsed(e) => expr_mentions_ident(e, name),
            _ => false,
        }),
        Expr::Literal(_) => false,
    }
}

/// Fuel this case has consumed so far, out of `granted`.
fn fuel_used(store: &mut wasmtime::Store<()>, granted: u64) -> u64 {
    granted.saturating_sub(store.get_fuel().unwrap_or(0))
}

fn invoke_bool(
    store: &mut wasmtime::Store<()>,
    instance: &wasmtime::Instance,
    fn_name: &str,
) -> Result<bool, WasmCallFailure> {
    let func = instance.get_func(&mut *store, fn_name).ok_or_else(|| {
        WasmCallFailure::Error(format!("wasm-gc verify: export `{}` not found", fn_name))
    })?;
    let mut out = vec![wasmtime::Val::I32(0); 1];
    func.call(&mut *store, &[], &mut out)
        .map_err(|e| WasmCallFailure::from_call_error(fn_name, e))?;
    match &out[0] {
        wasmtime::Val::I32(n) => Ok(*n != 0),
        v => Err(WasmCallFailure::Error(format!(
            "wasm-gc verify: `{}` returned non-Bool {:?}",
            fn_name, v
        ))),
    }
}

/// Invoke a `() -> String` exported helper and decode the resulting
/// wasm-gc String AnyRef back into a host `String`. Reuses the
/// `__rt_string_to_lm` runtime export every emitted module carries.
fn invoke_string(
    store: &mut wasmtime::Store<()>,
    instance: &wasmtime::Instance,
    fn_name: &str,
) -> Result<String, String> {
    use wasmtime::Val;
    let func = instance
        .get_func(&mut *store, fn_name)
        .ok_or_else(|| format!("wasm-gc verify: export `{}` not found", fn_name))?;
    let mut out = vec![Val::AnyRef(None); 1];
    func.call(&mut *store, &[], &mut out)
        .map_err(|e| format!("wasm-gc verify: call `{}` failed: {}", fn_name, e))?;
    let to_lm = instance
        .get_func(&mut *store, "__rt_string_to_lm")
        .ok_or_else(|| "wasm-gc verify: missing __rt_string_to_lm export".to_string())?;
    let memory = instance
        .get_memory(&mut *store, "memory")
        .ok_or_else(|| "wasm-gc verify: missing memory export".to_string())?;
    let mut len_out = [Val::I32(0)];
    to_lm
        .call(&mut *store, &out, &mut len_out)
        .map_err(|e| format!("wasm-gc verify: __rt_string_to_lm trap: {}", e))?;
    let len = match len_out[0] {
        Val::I32(n) => n.max(0) as usize,
        _ => 0,
    };
    let mut buf = vec![0u8; len];
    if len > 0 {
        memory
            .read(&store, 0, &mut buf)
            .map_err(|e| format!("wasm-gc verify: memory read for repr: {}", e))?;
    }
    Ok(String::from_utf8_lossy(&buf).into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// One number feeds both lanes, and the factor between them is pinned.
    ///
    /// The wasm lane used to carry its own `WASM_GC_VERIFY_FUEL = 10_000_000`
    /// with a doc comment calling itself "symmetric to VERIFY_VM_STEP_LIMIT"
    /// while being ten times larger, so the two lanes disagreed about which
    /// cases were runnable. They are now the same number times this factor;
    /// this test is what keeps them from drifting apart again silently.
    #[test]
    fn the_wasm_budget_is_the_step_budget_times_one_documented_factor() {
        assert_eq!(WASM_FUEL_PER_VM_STEP, 10);
        assert_eq!(
            case_fuel(crate::config::DEFAULT_VERIFY_STEP_LIMIT),
            10_000_000,
            "the default must reproduce the fuel the wasm lane used before the two numbers were joined"
        );
        assert_eq!(case_fuel(50_000_000), 500_000_000);
    }

    #[test]
    fn a_raised_step_budget_raises_the_wasm_budget_with_it() {
        let raised = crate::diagnostics::vm_verify::CaseBudget::resolve(
            Some(
                &crate::config::ProjectConfig::parse(
                    "[[verify.costly]]\nfn = \"checkScript\"\nstep-limit = 50000000\nreason = \"consensus-max scripts\"\n",
                )
                .expect("fixture config parses"),
            ),
            "checkScript",
            "domain/scriptcases1.av",
        );
        assert_eq!(case_fuel(raised.limit), 500_000_000);
    }
}
