//! Oracle-aware VM verify runner. Shared between `aver verify` (bin) and
//! `aver audit` (lib `diagnostics::analyze`) so every consumer sees the
//! same case-pass/fail outcomes for a given source.
//!
//! Covers the Oracle v1 extras that plain VM eval doesn't: LHS trace-
//! projection stripping, per-case `given` stub installation into the VM,
//! trace-event collection + projection back into the assertion value,
//! and `apply_trace_projection` for `.trace.{length,event,contains}` /
//! `.trace.group(N).*` shapes.

use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::ast::{
    Expr, FnBody, FnDef, SourceSpan, Spanned, TopLevel, VerifyBlock, VerifyGiven, VerifyKind,
};
use crate::checker::{
    VerifyCaseOutcome, VerifyCaseResult, VerifyLawContext, VerifyResult, expr_to_str,
    merge_verify_blocks,
};
use crate::config::ProjectConfig;
use crate::nan_value::{Arena, NanValueConvert};
use crate::resolver;
use crate::tco;
use crate::types::checker::effect_classification::{EffectDimension, classify, is_classified};
use crate::types::checker::hostile_effects::{HostileProfile, hostile_profiles_for};
use crate::types::checker::run_type_check_full;
use crate::value::{Value, aver_repr, list_from_vec};
use crate::verify_law::expand::{ExpandedCase, ExpansionMode, expand_law_cases};
use crate::vm;

/// Replace `block.cases` with the hostile expansion of its law template,
/// then layer effect-side hostile on top: each case is multiplied by the
/// cartesian product of `(method, profile)` adversarial responses for
/// every classified effect the fn declares but the user did not pin via
/// `given`. Both expansions run together when `--hostile` is on; cases
/// the user wrote stay as-is (origin=false, profiles=[]), value-hostile
/// cases get origin=true / profiles=[], effect-hostile cases get
/// origin=true / profiles=[(method, profile), ...].
///
/// No-op for non-law blocks on the value side (hostile only expands
/// `given` *domains*, which exist in law form), but effect-side hostile
/// applies to both forms since classified effects are independent of
/// `given` shape.
fn apply_hostile_expansion(block: &mut VerifyBlock, items: &[TopLevel]) {
    let value_expanded: Vec<(
        Spanned<Expr>,
        Spanned<Expr>,
        bool,
        Vec<(String, Spanned<Expr>)>,
        Option<Spanned<Expr>>,
    )> = match &block.kind {
        VerifyKind::Law(law) => expand_law_cases(law, ExpansionMode::Hostile)
            .into_iter()
            .map(|c: ExpandedCase| (c.lhs, c.rhs, c.from_hostile, c.bindings, c.guard))
            .collect(),
        VerifyKind::Cases => block
            .cases
            .iter()
            .enumerate()
            .map(|(idx, (lhs, rhs))| {
                (
                    lhs.clone(),
                    rhs.clone(),
                    block
                        .case_hostile_origins
                        .get(idx)
                        .copied()
                        .unwrap_or(false),
                    block.case_givens.get(idx).cloned().unwrap_or_default(),
                    None,
                )
            })
            .collect(),
    };

    let effect_combos = collect_effect_profile_combinations(block, items);

    // Hostile-injected cases inherit the law block's source line so
    // diagnostics point to the `verify ... law` declaration instead of
    // 0:0 — IDEs / playground / LSP can highlight the right span and the
    // user understands which law the boundary value broke. Declared cases
    // already had spans pointing at their own source positions, but the
    // post-parse expansion replaces the whole list so we synthesise spans
    // for every case here; the block-line fallback is conservative and
    // good enough until per-case spans for declared values get re-mapped
    // through the new pipeline (separate refactor).
    let block_span = SourceSpan {
        line: block.line,
        col: 1,
        end_line: block.line,
        end_col: 1,
    };

    let mut new_cases: Vec<(Spanned<Expr>, Spanned<Expr>)> = Vec::new();
    let mut new_origins: Vec<bool> = Vec::new();
    let mut new_givens: Vec<Vec<(String, Spanned<Expr>)>> = Vec::new();
    let mut new_spans: Vec<SourceSpan> = Vec::new();
    let mut new_profiles: Vec<Vec<(String, String)>> = Vec::new();
    let mut new_guards: Vec<Spanned<Expr>> = Vec::new();
    let has_when = matches!(&block.kind, VerifyKind::Law(law) if law.when.is_some());

    for (lhs, rhs, from_hostile, bindings, guard) in value_expanded {
        // Always keep the un-effected case (declared or value-hostile).
        new_cases.push((lhs.clone(), rhs.clone()));
        new_origins.push(from_hostile);
        new_givens.push(bindings.clone());
        new_spans.push(block_span.clone());
        new_profiles.push(Vec::new());
        if has_when && let Some(g) = &guard {
            new_guards.push(g.clone());
        }

        // Then add one case per effect-profile cartesian combination.
        for combo in &effect_combos {
            new_cases.push((lhs.clone(), rhs.clone()));
            new_origins.push(true);
            new_givens.push(bindings.clone());
            new_spans.push(block_span.clone());
            new_profiles.push(combo.clone());
            if has_when && let Some(g) = &guard {
                new_guards.push(g.clone());
            }
        }
    }

    block.cases = new_cases;
    block.case_spans = new_spans;
    block.case_givens = new_givens;
    block.case_hostile_origins = new_origins;
    block.case_hostile_profiles = new_profiles;
    if let VerifyKind::Law(law_box) = &mut block.kind {
        law_box.sample_guards = new_guards;
    }
}

/// For a verify block, find the function under test and return the
/// cartesian product of `(method, profile)` pairs across every classified
/// non-`Output` effect the fn declares. Each inner Vec is one adversarial
/// world.
///
/// User-given assignments are *not* a pre-empt — they're the "honest
/// world", run as the un-effected base case alongside the hostile cases.
/// Skipping a user-pinned effect from the cartesian would defeat the
/// point of `--hostile`: the user's stub is itself an *assumption*
/// ("clock always goes backwards", "random rolls 4"), and hostile mode
/// exists to challenge those assumptions with adversarial profiles
/// (frozen, fast-forward, min, max, ...). The runtime stub installer
/// overwrites user-given for the duration of a hostile-profile case so
/// the same law gets evaluated under both worlds.
fn collect_effect_profile_combinations(
    block: &VerifyBlock,
    items: &[TopLevel],
) -> Vec<Vec<(String, String)>> {
    let fn_def = items.iter().find_map(|i| match i {
        TopLevel::FnDef(fd) if fd.name == block.fn_name => Some(fd),
        _ => None,
    });
    let Some(fn_def) = fn_def else {
        return Vec::new();
    };

    let mut per_method: Vec<(String, Vec<HostileProfile>)> = Vec::new();
    for eff in &fn_def.effects {
        let method = eff.node.as_str();
        let Some(c) = classify(method) else { continue };
        if matches!(c.dimension, EffectDimension::Output) {
            continue;
        }
        let profiles = hostile_profiles_for(method);
        if profiles.is_empty() {
            continue;
        }
        per_method.push((method.to_string(), profiles));
    }

    if per_method.is_empty() {
        return Vec::new();
    }

    let mut out: Vec<Vec<(String, String)>> = vec![Vec::new()];
    for (method, profiles) in &per_method {
        let mut next: Vec<Vec<(String, String)>> =
            Vec::with_capacity(out.len() * profiles.len().max(1));
        for partial in &out {
            for profile in profiles {
                let mut extended = partial.clone();
                extended.push((method.clone(), profile.name.to_string()));
                next.push(extended);
            }
        }
        out = next;
    }
    out
}

/// For each verify block, find the function under test and the classified
/// non-Output effects it declares, then parse + inject the corresponding
/// hostile profile bodies as `TopLevel::FnDef`. User-given pins are *not*
/// a pre-empt — hostile profiles always layer on top, since the user's
/// stub is itself an assumption that the adversarial worlds challenge
/// (see `collect_effect_profile_combinations` for the rationale). Runs
/// before type-check so synthetic stubs go through the regular checker
/// and compile to ordinary user-space fns the runner can install as
/// oracle stubs.
fn inject_hostile_effect_stubs_for_blocks(items: &mut Vec<TopLevel>, blocks: &[VerifyBlock]) {
    let existing: std::collections::HashSet<String> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) => Some(fd.name.clone()),
            _ => None,
        })
        .collect();

    let mut to_inject: Vec<&'static str> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    for block in blocks {
        let Some(fn_def) = items.iter().find_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name == block.fn_name => Some(fd),
            _ => None,
        }) else {
            continue;
        };
        for eff in &fn_def.effects {
            let method = eff.node.as_str();
            let Some(c) = classify(method) else { continue };
            if matches!(c.dimension, EffectDimension::Output) {
                continue;
            }
            if !seen.insert(method.to_string()) {
                continue;
            }
            to_inject.push(static_method_name(method));
        }
    }

    let mut new_items: Vec<TopLevel> = Vec::new();
    for method in to_inject {
        for profile in hostile_profiles_for(method) {
            if existing.contains(&profile.stub_fn_name) {
                continue;
            }
            match parse_stub_body(&profile.stub_body) {
                Ok(stub_items) => new_items.extend(stub_items),
                Err(e) => {
                    eprintln!(
                        "warning: hostile profile {} failed to parse: {}",
                        profile.stub_fn_name, e
                    );
                }
            }
        }
    }
    items.extend(new_items);
}

/// `hostile_profiles_for` takes a `&str` and the closed set of known
/// method names is small. This indirection lets us push owned method
/// strings through the loop without leaking the lifetime of the borrow
/// from `fn_def.effects` across the mutable borrow of `items` we need
/// for stub injection.
fn static_method_name(method: &str) -> &'static str {
    match method {
        "Args.get" => "Args.get",
        "Env.get" => "Env.get",
        "Terminal.size" => "Terminal.size",
        "Random.int" => "Random.int",
        "Random.float" => "Random.float",
        "Time.now" => "Time.now",
        "Time.unixMs" => "Time.unixMs",
        "Disk.readText" => "Disk.readText",
        "Disk.exists" => "Disk.exists",
        "Disk.listDir" => "Disk.listDir",
        "Console.readLine" => "Console.readLine",
        "Http.get" => "Http.get",
        "Http.head" => "Http.head",
        "Http.delete" => "Http.delete",
        "Http.post" => "Http.post",
        "Http.put" => "Http.put",
        "Http.patch" => "Http.patch",
        "Disk.writeText" => "Disk.writeText",
        "Disk.appendText" => "Disk.appendText",
        "Disk.delete" => "Disk.delete",
        "Disk.deleteDir" => "Disk.deleteDir",
        "Disk.makeDir" => "Disk.makeDir",
        "Tcp.send" => "Tcp.send",
        "Tcp.ping" => "Tcp.ping",
        "Tcp.connect" => "Tcp.connect",
        "Tcp.readLine" => "Tcp.readLine",
        "Tcp.writeLine" => "Tcp.writeLine",
        "Tcp.close" => "Tcp.close",
        "Terminal.readKey" => "Terminal.readKey",
        _ => "",
    }
}

fn parse_stub_body(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = crate::lexer::Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let mut parser = crate::parser::Parser::new(tokens);
    parser.parse().map_err(|e| e.to_string())
}

/// Render a per-case hostile-effect-profile assignment into a short human
/// label. `[("Time.now", "frozen"), ("Random.int", "min")]` →
/// `Some("Time.now/frozen + Random.int/min")`. `None` for empty / missing.
fn render_hostile_profile_label(combo: Option<&[(String, String)]>) -> Option<String> {
    let combo = combo?;
    if combo.is_empty() {
        return None;
    }
    Some(
        combo
            .iter()
            .map(|(method, profile)| format!("{}/{}", method, profile))
            .collect::<Vec<_>>()
            .join(" + "),
    )
}

/// Run the complete verify pipeline for a set of items: typecheck,
/// synthesize per-case helpers, compile + run in VM with Oracle stub
/// installation and trace collection wired through.
///
/// `config` is an optional pre-loaded aver.toml (CLI loads it; audit /
/// LSP can pass `None` or load their own). `base_dir` is the module
/// root for cross-module resolution. `source_file` labels the VM's
/// compilation unit for diagnostics.
pub fn run_verify_for_items_vm(
    items: Vec<TopLevel>,
    config: Option<ProjectConfig>,
    base_dir: Option<&str>,
    source_file: &str,
) -> Result<Vec<VerifyResult>, String> {
    run_verify_for_items_vm_with_mode(
        items,
        config,
        base_dir,
        source_file,
        ExpansionMode::Declared,
    )
}

/// Same as [`run_verify_for_items_vm`] but lets the caller request hostile
/// boundary-value expansion of `verify ... law` cases. Under
/// `ExpansionMode::Hostile`, each typed `given` clause is augmented with
/// the per-type boundary set (`Int` min/max/0/±1, `Str` empty/long/edge,
/// ...) and the `when` precondition is preserved across the expansion.
/// Cases injected by the hostile pass carry `from_hostile = true` in
/// `VerifyBlock::case_hostile_origins` so the renderer can label them as
/// "outside declared given" when they fail.
pub fn run_verify_for_items_vm_with_mode(
    mut items: Vec<TopLevel>,
    config: Option<ProjectConfig>,
    base_dir: Option<&str>,
    source_file: &str,
    mode: ExpansionMode,
) -> Result<Vec<VerifyResult>, String> {
    tco::transform_program(&mut items);

    if mode == ExpansionMode::Hostile {
        // Inject hostile effect-profile stubs as `TopLevel::FnDef` before
        // type-check so they're visible to the verify pipeline as ordinary
        // user-defined functions. We can't compute case-level
        // `case_hostile_profiles` until after type-check (because the
        // value-side hostile expansion must run on a checked AST), so we
        // pre-scan: find every classified non-Output effect each verify
        // block's fn declares but the user did not pin via `given`, and
        // inject the corresponding hostile profile bodies. The cases are
        // wired up to those stubs in `apply_hostile_expansion` after
        // type-check.
        let preview_blocks = merge_verify_blocks(&items);
        inject_hostile_effect_stubs_for_blocks(&mut items, &preview_blocks);
    }

    let tc_result = run_type_check_full(&items, base_dir);
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    let mut verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return Ok(vec![]);
    }

    if mode == ExpansionMode::Hostile {
        for block in &mut verify_blocks {
            apply_hostile_expansion(block, &items);
        }
    }

    let plans = build_verify_vm_plans(&mut items, &verify_blocks);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        vm::compile_program_with_modules(&items, &mut arena, base_dir, source_file)
            .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    if let Some(cfg) = config {
        machine.set_runtime_policy(cfg);
    }

    let mut results = Vec::new();
    for plan in &plans {
        results.push(run_verify_vm(plan, &mut machine));
    }
    Ok(results)
}

/// Variant for audit / LSP: accept pre-loaded dependency modules
/// (virtual filesystems) instead of resolving from disk.
pub fn run_verify_for_items_vm_with_loaded(
    items: Vec<TopLevel>,
    loaded: Vec<crate::source::LoadedModule>,
    config: Option<ProjectConfig>,
    source_file: &str,
) -> Result<Vec<VerifyResult>, String> {
    run_verify_for_items_vm_with_loaded_and_mode(
        items,
        loaded,
        config,
        source_file,
        ExpansionMode::Declared,
    )
}

pub fn run_verify_for_items_vm_with_loaded_and_mode(
    mut items: Vec<TopLevel>,
    loaded: Vec<crate::source::LoadedModule>,
    config: Option<ProjectConfig>,
    source_file: &str,
    mode: ExpansionMode,
) -> Result<Vec<VerifyResult>, String> {
    tco::transform_program(&mut items);

    if mode == ExpansionMode::Hostile {
        let preview_blocks = merge_verify_blocks(&items);
        inject_hostile_effect_stubs_for_blocks(&mut items, &preview_blocks);
    }

    let tc_result = crate::types::checker::run_type_check_with_loaded(&items, &loaded);
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    let mut verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return Ok(vec![]);
    }

    if mode == ExpansionMode::Hostile {
        for block in &mut verify_blocks {
            apply_hostile_expansion(block, &items);
        }
    }

    let plans = build_verify_vm_plans(&mut items, &verify_blocks);
    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        vm::compile_program_with_loaded_modules(&items, &mut arena, loaded, source_file)
            .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    if let Some(cfg) = config {
        machine.set_runtime_policy(cfg);
    }

    let mut results = Vec::new();
    for plan in &plans {
        results.push(run_verify_vm(plan, &mut machine));
    }
    Ok(results)
}

fn format_type_errors(errors: &[crate::types::checker::TypeError]) -> String {
    let mut out = Vec::new();
    for te in errors {
        out.push(format!("error[{}:{}]: {}", te.line, te.col, te.message));
    }
    out.join("\n")
}

// ─── Plan synthesis ───────────────────────────────────────────────────────────

struct VmVerifyCaseFns {
    left: String,
    right: String,
    guard: Option<String>,
}

struct VmVerifyPlan {
    block: VerifyBlock,
    cases: Vec<VmVerifyCaseFns>,
}

enum VmVerifyEval {
    Value(Value),
    ErrProp(Value),
}

fn make_verify_vm_helper(
    name: String,
    line: usize,
    expr: Spanned<Expr>,
    wrap_result: bool,
) -> TopLevel {
    let body_expr = if wrap_result {
        Spanned::new(
            Expr::Constructor("Result.Ok".to_string(), Some(Box::new(expr))),
            line,
        )
    } else {
        expr
    };

    TopLevel::FnDef(FnDef {
        name,
        line,
        params: vec![],
        return_type: "Unit".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(body_expr)),
        resolution: None,
    })
}

fn build_verify_vm_plans(
    items: &mut Vec<TopLevel>,
    verify_blocks: &[VerifyBlock],
) -> Vec<VmVerifyPlan> {
    let mut plans = Vec::with_capacity(verify_blocks.len());

    for (block_idx, block) in verify_blocks.iter().enumerate() {
        let mut case_plans = Vec::with_capacity(block.cases.len());
        let sample_guards = match &block.kind {
            VerifyKind::Law(law) => Some(&law.sample_guards),
            VerifyKind::Cases => None,
        };

        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let left_name = format!("{}_left", prefix);
            let right_name = format!("{}_right", prefix);
            let left_expr = if block.trace {
                strip_trace_projection(left_expr)
            } else {
                left_expr
            };
            items.push(make_verify_vm_helper(
                left_name.clone(),
                block.line,
                left_expr,
                true,
            ));
            items.push(make_verify_vm_helper(
                right_name.clone(),
                block.line,
                right_expr,
                true,
            ));

            let guard_name = sample_guards
                .and_then(|guards| guards.get(case_idx))
                .cloned()
                .map(|guard_expr| {
                    let name = format!("{}_guard", prefix);
                    items.push(make_verify_vm_helper(
                        name.clone(),
                        block.line,
                        guard_expr,
                        false,
                    ));
                    name
                });

            case_plans.push(VmVerifyCaseFns {
                left: left_name,
                right: right_name,
                guard: guard_name,
            });
        }

        plans.push(VmVerifyPlan {
            block: block.clone(),
            cases: case_plans,
        });
    }

    plans
}

// ─── Trace projection ─────────────────────────────────────────────────────────

fn strip_trace_projection(expr: Spanned<Expr>) -> Spanned<Expr> {
    if let Some(fc) = find_trace_underlying_fn_call(&expr.node) {
        return fc;
    }
    expr
}

fn find_trace_underlying_fn_call(node: &Expr) -> Option<Spanned<Expr>> {
    match node {
        Expr::Attr(inner, field) if field == "result" => {
            if matches!(&inner.node, Expr::FnCall(_, _)) {
                Some((**inner).clone())
            } else {
                None
            }
        }
        Expr::Attr(inner, field) if field == "trace" => {
            if matches!(&inner.node, Expr::FnCall(_, _)) {
                Some((**inner).clone())
            } else {
                None
            }
        }
        Expr::FnCall(callee, _) => {
            let Expr::Attr(inner_attr, _) = &callee.node else {
                return None;
            };
            let (scope, _, _) = peel_tree_nav_stages(inner_attr);
            let Expr::Attr(fn_call_box, trace_field) = &scope.node else {
                return None;
            };
            if trace_field == "trace" && matches!(&fn_call_box.node, Expr::FnCall(_, _)) {
                return Some((**fn_call_box).clone());
            }
            None
        }
        _ => None,
    }
}

fn apply_trace_projection(
    helper_result: Result<VmVerifyEval, String>,
    original_lhs: &Spanned<Expr>,
    collected: &[Value],
    coords: &[crate::vm::runtime::TraceCoord],
    trace_mode: bool,
) -> Result<VmVerifyEval, String> {
    if !trace_mode {
        return helper_result;
    }
    let helper_result = helper_result?;
    if matches!(&original_lhs.node, Expr::Attr(_, f) if f == "result") {
        return Ok(helper_result);
    }
    match &original_lhs.node {
        Expr::Attr(inner, field)
            if field == "trace" && matches!(&inner.node, Expr::FnCall(_, _)) =>
        {
            Ok(VmVerifyEval::Value(build_trace_record(collected)))
        }
        Expr::FnCall(callee, args) => {
            let Expr::Attr(inner_attr, method) = &callee.node else {
                return Ok(helper_result);
            };
            let (scope, group_id, branch_idx) = peel_tree_nav_stages(inner_attr);
            let Expr::Attr(fn_call_box, trace_field) = &scope.node else {
                return Ok(helper_result);
            };
            if trace_field != "trace" || !matches!(&fn_call_box.node, Expr::FnCall(_, _)) {
                return Ok(helper_result);
            }
            let filtered: Vec<&Value> = collected
                .iter()
                .zip(coords.iter())
                .filter(|(_, c)| match group_id {
                    Some(n) => c.group_id == Some(n + 1),
                    None => true,
                })
                .filter(|(_, c)| match branch_idx {
                    Some(b) => c.branch_idx == Some(b),
                    None => true,
                })
                .map(|(ev, _)| ev)
                .collect();
            match method.as_str() {
                "length" => Ok(VmVerifyEval::Value(Value::Int(filtered.len() as i64))),
                "event" => {
                    if args.len() != 1 {
                        return Err(format!("Trace.event(k) expects 1 arg, got {}", args.len()));
                    }
                    let k = match &args[0].node {
                        Expr::Literal(crate::ast::Literal::Int(i)) if *i >= 0 => *i as usize,
                        _ => {
                            return Err(
                                "Trace.event(k) requires a literal non-negative Int for v0"
                                    .to_string(),
                            );
                        }
                    };
                    let v = match filtered.get(k) {
                        Some(ev) => Value::Some(Box::new((**ev).clone())),
                        None => Value::None,
                    };
                    Ok(VmVerifyEval::Value(v))
                }
                "contains" => {
                    if args.len() != 1 {
                        return Err(format!(
                            "Trace.contains(x) expects 1 arg, got {}",
                            args.len()
                        ));
                    }
                    let arg = &args[0];
                    let method_only_name = effect_method_ref_name(arg);
                    let hit = if let Some(method_name) = method_only_name {
                        filtered
                            .iter()
                            .any(|ev| effect_event_has_method(ev, &method_name))
                    } else {
                        let expected = match expr_to_effect_event(arg) {
                            Some(ev) => ev,
                            None => {
                                return Err(
                                    "Trace.contains(x): argument must be either an effect-method \
                                     reference (e.g. `Console.print`) or an effect-method call \
                                     (e.g. `Console.print(\"...\")`) that elaborates to an event literal"
                                        .to_string(),
                                );
                            }
                        };
                        filtered
                            .iter()
                            .any(|ev| effect_event_method_args_eq(ev, &expected))
                    };
                    Ok(VmVerifyEval::Value(Value::Bool(hit)))
                }
                "count" => {
                    // 0.13 Limit nail #3: count laws. `.trace.count(M)` returns
                    // the number of trace events whose method matches `M` (an
                    // effect-method reference like `Time.now` or a call literal
                    // `Console.print("hi")`). Lets users write quantitative
                    // assertions — "this fn calls the API exactly once" —
                    // instead of just "contains" / "length over the whole
                    // trace". The hostile-effect profiles and trace count
                    // laws compose: `count(Time.now) == n` checks how many
                    // times the law's fn reaches for the clock under each
                    // adversarial profile.
                    if args.len() != 1 {
                        return Err(format!(
                            "Trace.count(x) expects 1 arg, got {}",
                            args.len()
                        ));
                    }
                    let arg = &args[0];
                    let method_only_name = effect_method_ref_name(arg);
                    let n = if let Some(method_name) = method_only_name {
                        filtered
                            .iter()
                            .filter(|ev| effect_event_has_method(ev, &method_name))
                            .count()
                    } else {
                        let expected = match expr_to_effect_event(arg) {
                            Some(ev) => ev,
                            None => {
                                return Err(
                                    "Trace.count(x): argument must be either an effect-method \
                                     reference (e.g. `Console.print`) or an effect-method call \
                                     (e.g. `Console.print(\"...\")`) that elaborates to an event literal"
                                        .to_string(),
                                );
                            }
                        };
                        filtered
                            .iter()
                            .filter(|ev| effect_event_method_args_eq(ev, &expected))
                            .count()
                    };
                    Ok(VmVerifyEval::Value(Value::Int(n as i64)))
                }
                _ => Ok(helper_result),
            }
        }
        _ => Ok(helper_result),
    }
}

fn peel_named_int_stage<'a>(
    expr: &'a Spanned<Expr>,
    expected: &str,
) -> Option<(&'a Spanned<Expr>, u32)> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let Expr::Attr(base, method) = &callee.node else {
        return None;
    };
    if method != expected || args.len() != 1 {
        return None;
    }
    match &args[0].node {
        Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 0 => Some((base.as_ref(), *n as u32)),
        _ => None,
    }
}

fn peel_tree_nav_stages(expr: &Spanned<Expr>) -> (&Spanned<Expr>, Option<u32>, Option<u32>) {
    if let Some((after_branch, idx)) = peel_named_int_stage(expr, "branch")
        && let Some((after_group, gid)) = peel_named_int_stage(after_branch, "group")
    {
        return (after_group, Some(gid), Some(idx));
    }
    if let Some((after_group, gid)) = peel_named_int_stage(expr, "group") {
        return (after_group, Some(gid), None);
    }
    (expr, None, None)
}

fn is_trace_projection_shape(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Attr(inner, field) if field == "trace" => {
            matches!(&inner.node, Expr::FnCall(_, _))
        }
        Expr::FnCall(callee, _) => {
            let Expr::Attr(inner_attr, method) = &callee.node else {
                return false;
            };
            if !matches!(method.as_str(), "length" | "event" | "contains" | "count") {
                return false;
            }
            let (scope, _, _) = peel_tree_nav_stages(inner_attr);
            let Expr::Attr(fn_call_box, trace_field) = &scope.node else {
                return false;
            };
            trace_field == "trace" && matches!(&fn_call_box.node, Expr::FnCall(_, _))
        }
        _ => false,
    }
}

fn format_trace_events_tail(events: &[Value]) -> String {
    if events.is_empty() {
        return "  (trace: [] — no effects recorded)".to_string();
    }
    let rendered: Vec<String> = events.iter().map(aver_repr).collect();
    format!("  (trace: [{}])", rendered.join(", "))
}

fn build_trace_record(collected: &[Value]) -> Value {
    Value::Record {
        type_name: crate::types::trace::TYPE_NAME.to_string(),
        fields: vec![(
            crate::types::trace::FIELD_EVENTS.to_string(),
            list_from_vec(collected.to_vec()),
        )]
        .into(),
    }
}

fn effect_event_method_args_eq(recorded: &Value, expected: &Value) -> bool {
    let Value::Record {
        type_name: rt,
        fields: rf,
    } = recorded
    else {
        return false;
    };
    let Value::Record {
        type_name: et,
        fields: ef,
    } = expected
    else {
        return false;
    };
    if rt != et {
        return false;
    }
    let pick = |fields: &[(String, Value)], name: &str| -> Option<Value> {
        fields
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v.clone())
    };
    let r_method = pick(rf, crate::types::effect_event::FIELD_METHOD);
    let e_method = pick(ef, crate::types::effect_event::FIELD_METHOD);
    let r_args = pick(rf, crate::types::effect_event::FIELD_ARGS);
    let e_args = pick(ef, crate::types::effect_event::FIELD_ARGS);
    r_method == e_method && r_args == e_args
}

fn effect_method_ref_name(expr: &Spanned<Expr>) -> Option<String> {
    let (ns, method) = match &expr.node {
        Expr::Attr(ns, method) => {
            let Expr::Ident(ns_name) = &ns.node else {
                return None;
            };
            (ns_name.clone(), method.clone())
        }
        Expr::Resolved { name, .. } => {
            let (ns, method) = name.rsplit_once('.')?;
            (ns.to_string(), method.to_string())
        }
        _ => return None,
    };
    let full = format!("{}.{}", ns, method);
    if is_classified(&full) {
        Some(full)
    } else {
        None
    }
}

fn effect_event_has_method(recorded: &Value, expected_name: &str) -> bool {
    let Value::Record { type_name, fields } = recorded else {
        return false;
    };
    if type_name != crate::types::effect_event::TYPE_NAME {
        return false;
    }
    fields.iter().any(|(name, val)| {
        name == crate::types::effect_event::FIELD_METHOD
            && matches!(val, Value::Str(s) if s == expected_name)
    })
}

fn expr_to_effect_event(expr: &Spanned<Expr>) -> Option<Value> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let Expr::Attr(ns, method) = &callee.node else {
        return None;
    };
    let Expr::Ident(ns_name) = &ns.node else {
        return None;
    };
    let full_name = format!("{}.{}", ns_name, method);
    let arg_vals: Vec<Value> = args.iter().filter_map(literal_expr_to_value).collect();
    if arg_vals.len() != args.len() {
        return None;
    }
    Some(Value::Record {
        type_name: crate::types::effect_event::TYPE_NAME.to_string(),
        fields: vec![
            (
                crate::types::effect_event::FIELD_METHOD.to_string(),
                Value::Str(full_name),
            ),
            (
                crate::types::effect_event::FIELD_ARGS.to_string(),
                list_from_vec(arg_vals),
            ),
        ]
        .into(),
    })
}

fn literal_expr_to_value(expr: &Spanned<Expr>) -> Option<Value> {
    match &expr.node {
        Expr::Literal(crate::ast::Literal::Int(i)) => Some(Value::Int(*i)),
        Expr::Literal(crate::ast::Literal::Float(f)) => Some(Value::Float(*f)),
        Expr::Literal(crate::ast::Literal::Str(s)) => Some(Value::Str(s.clone())),
        Expr::Literal(crate::ast::Literal::Bool(b)) => Some(Value::Bool(*b)),
        Expr::Literal(crate::ast::Literal::Unit) => Some(Value::Unit),
        Expr::InterpolatedStr(parts) => {
            let mut s = String::new();
            for part in parts {
                match part {
                    crate::ast::StrPart::Literal(lit) => s.push_str(lit),
                    crate::ast::StrPart::Parsed(inner) => {
                        let v = literal_expr_to_value(inner)?;
                        s.push_str(&interp_value_to_str(&v)?);
                    }
                }
            }
            Some(Value::Str(s))
        }
        _ => None,
    }
}

fn interp_value_to_str(value: &Value) -> Option<String> {
    match value {
        Value::Str(s) => Some(s.clone()),
        Value::Int(i) => Some(i.to_string()),
        Value::Float(f) => Some(f.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        Value::Unit => Some(String::new()),
        _ => None,
    }
}

// ─── VM helpers + oracle stubs ────────────────────────────────────────────────

fn vm_call_verify_helper(machine: &mut vm::VM, fn_name: &str) -> Result<VmVerifyEval, String> {
    let value = machine
        .run_named_function(fn_name, &[])
        .map_err(|e| e.to_string())?
        .to_value(&machine.arena);

    match value {
        Value::Ok(inner) => Ok(VmVerifyEval::Value(*inner)),
        Value::Err(inner) => Ok(VmVerifyEval::ErrProp(*inner)),
        other => Err(format!(
            "verify helper '{}' returned unexpected shape: {}",
            fn_name,
            aver_repr(&other)
        )),
    }
}

fn vm_call_guard_helper(machine: &mut vm::VM, fn_name: &str) -> Result<Value, String> {
    machine
        .run_named_function(fn_name, &[])
        .map_err(|e| e.to_string())
        .map(|value| value.to_value(&machine.arena))
}

fn dotted_path_from_expr(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(s) => Some(s.clone()),
        Expr::Resolved { name, .. } => Some(name.clone()),
        Expr::Attr(obj, field) => {
            let head = dotted_path_from_expr(&obj.node)?;
            Some(format!("{}.{}", head, field))
        }
        _ => None,
    }
}

fn build_case_oracle_stubs(
    machine: &vm::VM,
    block: &VerifyBlock,
    case_idx: usize,
) -> HashMap<String, u32> {
    let mut out = HashMap::new();
    let givens_source: &[VerifyGiven] = match &block.kind {
        VerifyKind::Law(law) => law.givens.as_slice(),
        VerifyKind::Cases => block.cases_givens.as_slice(),
    };

    if !givens_source.is_empty()
        && let Some(case_bindings) = block.case_givens.get(case_idx)
    {
        for given in givens_source {
            let Some(classification) = classify(&given.type_name) else {
                continue;
            };
            if matches!(classification.dimension, EffectDimension::Output) {
                continue;
            }
            let Some((_, value_expr)) = case_bindings.iter().find(|(n, _)| n == &given.name) else {
                continue;
            };
            let stub_name = match &value_expr.node {
                Expr::Ident(s) => s.clone(),
                Expr::Resolved { name, .. } => name.clone(),
                _ => {
                    let Some(dotted) = dotted_path_from_expr(&value_expr.node) else {
                        continue;
                    };
                    dotted
                }
            };
            let Some(fn_id) = machine.find_fn_id(&stub_name) else {
                continue;
            };
            out.insert(given.type_name.clone(), fn_id);
        }
    }

    // Layer hostile effect-profile stubs on top of (or instead of) the
    // user's `given` stubs. Effect-profile assignments are computed by
    // `apply_hostile_expansion` and only contain effects the user did
    // not pin via `given`, so user-given always wins for the same method
    // — a hostile-expanded case for an un-given effect installs the
    // profile's synthetic stub fn. This keeps the runner agnostic to
    // which kind of stub it is — the VM only cares about (method →
    // fn_id), and `__hostile_<method>_<profile>` lives as an ordinary
    // FnDef thanks to `inject_hostile_effect_stubs_for_blocks`.
    if let Some(combo) = block.case_hostile_profiles.get(case_idx) {
        for (method, profile) in combo {
            let stub_fn_name =
                format!("__hostile_{}_{}", method.replace('.', "_"), profile);
            let Some(fn_id) = machine.find_fn_id(&stub_fn_name) else {
                continue;
            };
            out.insert(method.clone(), fn_id);
        }
    }
    out
}

// ─── Case runner ──────────────────────────────────────────────────────────────

fn run_verify_vm(plan: &VmVerifyPlan, machine: &mut vm::VM) -> VerifyResult {
    let block = &plan.block;
    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;
    let mut failures = Vec::new();
    let mut case_results = Vec::new();
    let is_law = matches!(block.kind, VerifyKind::Law(_));
    let case_total = block.cases.len();

    let law_context_template = if let VerifyKind::Law(law) = &block.kind {
        Some(format!(
            "{} == {}",
            expr_to_str(&law.lhs),
            expr_to_str(&law.rhs)
        ))
    } else {
        None
    };

    for (idx, ((left_expr, right_expr), case_fns)) in
        block.cases.iter().zip(&plan.cases).enumerate()
    {
        let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));
        let span = block.case_spans.get(idx).cloned();
        let from_hostile = block
            .case_hostile_origins
            .get(idx)
            .copied()
            .unwrap_or(false);
        let hostile_profile = render_hostile_profile_label(
            block.case_hostile_profiles.get(idx).map(Vec::as_slice),
        );
        let failure_case = if is_law {
            format!("case {}/{} [{}]", idx + 1, case_total, case_str)
        } else {
            case_str.clone()
        };

        let law_context = if let VerifyKind::Law(_) = &block.kind {
            let givens: Vec<(String, String)> = block
                .case_givens
                .get(idx)
                .map(|gs| {
                    gs.iter()
                        .map(|(name, expr)| (name.clone(), expr_to_str(expr)))
                        .collect()
                })
                .unwrap_or_default();
            Some(VerifyLawContext {
                givens,
                law_expr: law_context_template.clone().unwrap_or_default(),
            })
        } else {
            None
        };

        if let Some(guard_name) = &case_fns.guard {
            match vm_call_guard_helper(machine, guard_name) {
                Ok(Value::Bool(true)) => {}
                Ok(Value::Bool(false)) => {
                    skipped += 1;
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Skipped,
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                    });
                    continue;
                }
                Ok(Value::Err(err_val)) => {
                    failed += 1;
                    let err_repr = format!("Result.Err({})", aver_repr(&err_val));
                    failures.push((failure_case, String::new(), err_repr.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::UnexpectedErr { err_repr },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                    });
                    continue;
                }
                Ok(other) => {
                    failed += 1;
                    let error = format!("when produced {}, expected Bool", aver_repr(&other));
                    failures.push((failure_case, "Bool".to_string(), error.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::RuntimeError { error },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                    });
                    continue;
                }
                Err(e) => {
                    failed += 1;
                    let error = format!("guard error: {}", e);
                    failures.push((failure_case, String::new(), error.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::RuntimeError { error },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                    });
                    continue;
                }
            }
        }

        let oracle_stubs = build_case_oracle_stubs(machine, block, idx);
        let has_stubs = !oracle_stubs.is_empty();
        if has_stubs {
            machine.install_oracle_stubs(oracle_stubs);
        }

        // Always start trace collection, not just for `block.trace`.
        // The generated verify helper (`__verify_foo_left`) declares
        // no effects, so calling an effectful user fn from it would
        // trip the VM's effect gate for any classified effect that
        // isn't stubbed — e.g. a result-only law on a fn with
        // `! [Random.int, Console.print]` has a stub for Random.int
        // but Console.print is Output (no oracle), and the helper
        // would fail with a runtime effect violation on what the
        // typechecker / proof export both accept.
        // `trace_collecting` is exactly the suppression the runtime
        // uses for classified effects (src/vm/runtime.rs:563), so
        // enabling it here keeps non-trace law evaluation consistent
        // with typecheck + proof. We discard the buffered events for
        // non-trace blocks — they're only consumed by the trace
        // projection path below.
        machine.start_trace_collection();
        let root_fn_id = machine.find_fn_id(&block.fn_name);
        machine.set_trace_root_fn_id(root_fn_id);

        let left_result = vm_call_verify_helper(machine, &case_fns.left);
        let (lhs_trace_events, lhs_trace_coords): (
            Vec<Value>,
            Vec<crate::vm::runtime::TraceCoord>,
        ) = if block.trace {
            machine.take_trace_events_with_coords()
        } else {
            // Drain and discard so the buffer stays clean between
            // cases and nothing leaks into the next plan.
            let _ = machine.take_trace_events_with_coords();
            (Vec::new(), Vec::new())
        };

        let left_result = apply_trace_projection(
            left_result,
            &block.cases[idx].0,
            &lhs_trace_events,
            &lhs_trace_coords,
            block.trace,
        );

        let right_result = vm_call_verify_helper(machine, &case_fns.right);

        if has_stubs {
            machine.clear_oracle_stubs();
        }

        match (left_result, right_result) {
            (Ok(VmVerifyEval::Value(left_val)), Ok(VmVerifyEval::Value(right_val))) => {
                if left_val == right_val {
                    passed += 1;
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Pass,
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                    });
                } else {
                    failed += 1;
                    let expected = aver_repr(&right_val);
                    let mut actual = aver_repr(&left_val);
                    if block.trace && is_trace_projection_shape(&block.cases[idx].0) {
                        actual.push_str(&format_trace_events_tail(&lhs_trace_events));
                    }
                    failures.push((failure_case, expected.clone(), actual.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Mismatch { expected, actual },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                    });
                }
            }
            (Ok(VmVerifyEval::ErrProp(err_val)), _) | (_, Ok(VmVerifyEval::ErrProp(err_val))) => {
                failed += 1;
                let err_repr = format!("Result.Err({})", aver_repr(&err_val));
                failures.push((failure_case, String::new(), err_repr.clone()));
                case_results.push(VerifyCaseResult {
                    outcome: VerifyCaseOutcome::UnexpectedErr { err_repr },
                    span,
                    case_expr: case_str,
                    case_index: idx,
                    case_total,
                    law_context,
                    from_hostile,
                    hostile_profile: hostile_profile.clone(),
                });
            }
            (Err(e), _) | (_, Err(e)) => {
                failed += 1;
                let error = e.to_string();
                failures.push((failure_case, String::new(), error.clone()));
                case_results.push(VerifyCaseResult {
                    outcome: VerifyCaseOutcome::RuntimeError { error },
                    span,
                    case_expr: case_str,
                    case_index: idx,
                    case_total,
                    law_context,
                    from_hostile,
                    hostile_profile: hostile_profile.clone(),
                });
            }
        }
    }

    let block_label = match &block.kind {
        VerifyKind::Law(law) => format!("{} spec {}", block.fn_name, law.name),
        VerifyKind::Cases => block.fn_name.clone(),
    };
    VerifyResult {
        fn_name: block.fn_name.clone(),
        block_label,
        passed,
        failed,
        skipped,
        case_results,
        failures,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Literal, Spanned, VerifyGiven, VerifyGivenDomain, VerifyLaw};

    fn parse_source(src: &str) -> Vec<TopLevel> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse().expect("parse")
    }

    #[test]
    fn collect_effect_profile_returns_cartesian_for_unfixed_effects() {
        let src = "module M\n    effects [Time.now, Random.int]\n\nfn f() -> Int\n    ? \"toy\"\n    ! [Time.now, Random.int]\n    1\n";
        let items = parse_source(src);
        let block = VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            kind: VerifyKind::Cases,
            trace: false,
            cases_givens: vec![],
        };
        let combos = collect_effect_profile_combinations(&block, &items);
        // Profile counts grow per effect; what matters here is shape:
        // every combo references one Time.now profile + one Random.int
        // profile, and the cartesian equals the product of the per-method
        // profile counts.
        let time_now_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Time.now").len();
        let random_int_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Random.int").len();
        assert!(time_now_profiles >= 2);
        assert!(random_int_profiles >= 2);
        assert_eq!(combos.len(), time_now_profiles * random_int_profiles);
        for combo in &combos {
            assert_eq!(combo.len(), 2);
        }
    }

    #[test]
    fn collect_effect_profile_includes_user_pinned_givens() {
        // Hostile is *parity* — even when the user pinned `given Time.now`,
        // the profile cartesian still includes Time.now alongside the
        // user's stub. The user-given is one assumption; hostile mode
        // exists to challenge it ("clock always backwards" vs frozen vs
        // fast-forward). The runtime stub installer overwrites the user's
        // given for the duration of a hostile-profile case, so both
        // worlds get evaluated against the same law.
        let src = "module M\n    effects [Time.now, Random.int]\n\nfn f() -> Int\n    ? \"toy\"\n    ! [Time.now, Random.int]\n    1\n";
        let items = parse_source(src);
        let block = VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            kind: VerifyKind::Law(Box::new(VerifyLaw {
                name: "test".to_string(),
                givens: vec![VerifyGiven {
                    name: "clock".to_string(),
                    type_name: "Time.now".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![]),
                }],
                when: None,
                lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                sample_guards: vec![],
            })),
            trace: false,
            cases_givens: vec![],
        };
        let combos = collect_effect_profile_combinations(&block, &items);
        // Cartesian = (Time.now profiles) × (Random.int profiles),
        // regardless of user-given on Time.now.
        let time_now_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Time.now").len();
        let random_int_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Random.int").len();
        assert_eq!(combos.len(), time_now_profiles * random_int_profiles);
        let methods: std::collections::HashSet<&str> = combos
            .iter()
            .flat_map(|c| c.iter().map(|(m, _)| m.as_str()))
            .collect();
        assert!(methods.contains("Time.now"));
        assert!(methods.contains("Random.int"));
    }

    #[test]
    fn collect_effect_profile_excludes_output_effects() {
        // Console.print is Output — no oracle response to vary, so no profiles.
        let src = "module M\n    effects [Console.print]\n\nfn greet() -> Unit\n    ? \"toy\"\n    ! [Console.print]\n    Console.print(\"hi\")\n";
        let items = parse_source(src);
        let block = VerifyBlock {
            fn_name: "greet".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            kind: VerifyKind::Cases,
            trace: false,
            cases_givens: vec![],
        };
        let combos = collect_effect_profile_combinations(&block, &items);
        assert!(combos.is_empty());
    }

    #[test]
    fn inject_hostile_stubs_appends_fn_defs_for_unfixed_classified_effects() {
        let src = "module M\n    effects [Time.now]\n\nfn f() -> String\n    ? \"toy\"\n    ! [Time.now]\n    Time.now()\n";
        let mut items = parse_source(src);
        let blocks = vec![VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            kind: VerifyKind::Cases,
            trace: false,
            cases_givens: vec![],
        }];
        let before = items.len();
        inject_hostile_effect_stubs_for_blocks(&mut items, &blocks);
        let after = items.len();
        let expected =
            crate::types::checker::hostile_effects::hostile_profiles_for("Time.now").len();
        assert_eq!(after - before, expected);
        let fn_names: Vec<String> = items
            .iter()
            .filter_map(|i| match i {
                TopLevel::FnDef(fd) => Some(fd.name.clone()),
                _ => None,
            })
            .collect();
        assert!(fn_names.contains(&"__hostile_Time_now_frozen".to_string()));
        assert!(fn_names.contains(&"__hostile_Time_now_epoch".to_string()));
    }

    #[test]
    fn apply_hostile_expansion_multiplies_cases_by_effect_cartesian() {
        // A 2-case law on a fn that uses Time.now without a given.
        // Hostile expansion produces: 2 declared cases ×
        // (1 un-effected base + 2 Time.now profiles) = 6 cases.
        // Then value-side hostile may add more boundary cases for any
        // typed givens — but this fn has no `given`, so value side is
        // a no-op. Net = 6.
        let src = r#"module M
    effects [Time.now]

fn currentYear() -> String
    ? "current year"
    ! [Time.now]
    Time.now()

verify currentYear
    currentYear() => "any"
    currentYear() => "any"
"#;
        let items = parse_source(src);
        let mut blocks = crate::checker::merge_verify_blocks(&items);
        assert_eq!(blocks.len(), 1);
        let block = &mut blocks[0];
        let declared_count = block.cases.len();
        assert_eq!(declared_count, 2);

        apply_hostile_expansion(block, &items);

        // Per declared case: 1 base + N profile cases (one per Time.now
        // profile), where N grows over time as we add adversarial worlds.
        let time_now_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Time.now").len();
        let expected_total = declared_count * (1 + time_now_profiles);
        assert_eq!(block.cases.len(), expected_total);
        assert_eq!(block.case_hostile_profiles.len(), expected_total);
        let base_count = block
            .case_hostile_profiles
            .iter()
            .filter(|p| p.is_empty())
            .count();
        assert_eq!(base_count, declared_count);
        let profile_count = block
            .case_hostile_profiles
            .iter()
            .filter(|p| !p.is_empty())
            .count();
        assert_eq!(profile_count, declared_count * time_now_profiles);
    }

    #[test]
    fn render_hostile_profile_label_joins_pairs() {
        let combo = vec![
            ("Time.now".to_string(), "frozen".to_string()),
            ("Random.int".to_string(), "min".to_string()),
        ];
        assert_eq!(
            render_hostile_profile_label(Some(&combo)),
            Some("Time.now/frozen + Random.int/min".to_string())
        );
        assert_eq!(render_hostile_profile_label(Some(&[])), None);
        assert_eq!(render_hostile_profile_label(None), None);
    }
}
