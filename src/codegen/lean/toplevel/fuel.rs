use std::collections::{HashMap, HashSet};

use super::expr::{aver_name_to_lean, emit_expr_legacy};
use super::fn_def::{emit_fn_body_for, lower_pure_question_bang_for_emit};
use super::recurrence::{recurrence_nat_helper_name, render_affine_pair_expr};
use super::render::{
    emit_doc_comment, emit_fn_param_names, emit_fn_params, indent_lines, ret_type_or_unit,
};
use super::type_def::type_measure_expr;
use super::types::type_annotation_to_lean;
use super::{
    emit_fn_def, emit_fn_def_proof, emit_mutual_group, emit_mutual_group_proof, is_pure_fn,
    is_recursive_type_def, sizeof_measure_param_indices, type_def_name,
};
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::recursion::{native_aux_name, rewrite_recursive_calls_body};

const STRING_POS_FUEL_VAR: &str = "fuel'";

/// Panic message baked into every fuel wrapper's exhaustion arm. This is a
/// SOUNDNESS marker, not just a diagnostic: Lean's `panic!` does NOT abort
/// evaluation — it prints `PANIC at … <this message>` and returns the type's
/// `default` value, so under `native_decide` an exhausted-fuel sample reduces
/// both sides of a model-vs-model equation to `default` and the kernel
/// certifies a vacuous (possibly FALSE) equality with `lake` still exiting 0.
/// `aver proof --check` therefore scans captured lake output for panic lines
/// ([`crate::codegen::lean::count_model_panic_lines`]) and treats any hit as
/// a hard check failure. The scan keys on Lean's generic `PANIC at ` line
/// marker — every prelude `panic!` site shares the vacuity vector, not just
/// this one — so this constant is purely the emission message; changing it
/// cannot blind the gate.
pub const PROOF_FUEL_EXHAUSTED_MSG: &str = "Aver proof fuel exhausted";

fn fuel_helper_name(name: &str) -> String {
    // Use the shared helper so the name matches what the shared AST
    // rewrite emits into `Expr::Ident(...)` call sites. The `__fuel`
    // suffix keeps the result a plain ASCII identifier regardless of
    // the source name, so no Lean-specific escaping is needed.
    crate::codegen::recursion::fuel_helper_name(name)
}

/// Simp-set names for a fuel-emitted fn cited by the
/// `SimpOverPreludeLemmas` law rung: `<name>__fuel` plus the measure
/// helper names (`averMeasure*` / `averStringPosFuel`) the wrapper's
/// fuel expression references. Rather than re-deriving the
/// plan→emission mapping (which `recognize_lex_list_wf_scc` can flip
/// per-SCC to native `termination_by`, no `__fuel` def at all), this
/// PROBES the proof-mode emission itself: re-emit the fn's SCC group
/// through the exact dispatch `transpile_unified` uses and scan the
/// text. Returns `[]` when the emission carries no `def <name>__fuel`
/// — citing a non-existent constant in `simp [...]` would be a hard
/// `unknown constant` build error, the one failure mode the rung's
/// `first | … | sorry` floor cannot catch. Cost: one re-emit of one
/// SCC per fuel-citing law (string building only, no side effects).
/// Assumes proof-mode emission — every production Lean export goes
/// through `transpile_for_proof_mode`.
pub(in crate::codegen::lean) fn law_fuel_simp_names(
    fn_name: &str,
    ctx: &CodegenContext,
) -> Vec<String> {
    let Some(emitted) = probe_fn_scc_emission(fn_name, ctx) else {
        return Vec::new();
    };
    let fuel = fuel_helper_name(fn_name);
    if !emitted.contains(&format!("def {fuel}")) {
        return Vec::new();
    }
    let mut names = vec![fuel];
    names.extend(scan_measure_helper_names(&emitted));
    names
}

/// Re-emit the SCC group that owns `fn_name` through the exact
/// dispatch `transpile_unified` uses and return the emitted text.
/// Shared probe for [`law_fuel_simp_names`] and
/// [`law_string_pos_rank`] — see the former's doc for why probing the
/// emission beats re-deriving the plan→emission mapping. `None` when
/// the fn isn't a pure fn of any scope.
fn probe_fn_scc_emission(fn_name: &str, ctx: &CodegenContext) -> Option<String> {
    // Locate the fn's owning scope (entry first, then dep modules) and
    // the pure-fn population of that scope — the same component
    // universe `transpile_unified` routes.
    let scopes: Vec<(Option<String>, Vec<&crate::ast::FnDef>)> =
        std::iter::once((None, ctx.fn_defs.iter().collect::<Vec<_>>()))
            .chain(
                ctx.modules
                    .iter()
                    .map(|m| (Some(m.prefix.clone()), m.fn_defs.iter().collect())),
            )
            .collect();
    for (scope, fns) in scopes {
        let pure: Vec<&crate::ast::FnDef> = fns.into_iter().filter(|fd| is_pure_fn(fd)).collect();
        if !pure.iter().any(|fd| fd.name == fn_name) {
            continue;
        }
        let comps = crate::call_graph::ordered_fn_components(&pure, &ctx.module_prefixes);
        let comp = comps
            .into_iter()
            .find(|c| c.iter().any(|fd| fd.name == fn_name))?;
        let emitted = ctx.with_module_scope(scope.as_deref(), || {
            if comp.len() > 1 {
                let all_supported = comp
                    .iter()
                    .all(|fd| crate::codegen::common::fn_contract_exists_for_fn(ctx, fd));
                if all_supported {
                    emit_mutual_group_proof(&comp, ctx)
                } else {
                    emit_mutual_group(&comp, ctx)
                }
            } else if let Some(fd) = comp.first() {
                if crate::codegen::common::fn_contract_exists_for_fn(ctx, fd) {
                    emit_fn_def_proof(fd, ctx).unwrap_or_default()
                } else {
                    emit_fn_def(fd, &std::collections::HashSet::from([fd.name.clone()]), ctx)
                        .unwrap_or_default()
                }
            } else {
                String::new()
            }
        });
        return Some(emitted);
    }
    None
}

/// The `averStringPosFuel` rank literal of `fn_name`'s emitted fuel
/// wrapper (`def <fn> … := <fn>__fuel (averStringPosFuel s pos RANK)
/// …`), probed from the actual proof-mode emission so the
/// `StringEscapeRoundtrip` skeleton's `show`-line quotes the exact
/// fuel expression the wrapper carries. `None` when the fn isn't
/// fuel-emitted with a string-pos wrapper — the renderer declines
/// rather than quoting a fuel expression that doesn't exist.
pub(in crate::codegen::lean) fn law_string_pos_rank(
    fn_name: &str,
    ctx: &CodegenContext,
) -> Option<usize> {
    let emitted = probe_fn_scc_emission(fn_name, ctx)?;
    let fuel = fuel_helper_name(fn_name);
    if !emitted.contains(&format!("def {fuel}")) {
        return None;
    }
    let marker = format!("{fuel} (averStringPosFuel ");
    let idx = emitted.find(&marker)?;
    let rest = &emitted[idx + marker.len()..];
    let mut tokens = rest.split_whitespace();
    let _string_arg = tokens.next()?;
    let _pos_arg = tokens.next()?;
    tokens.next()?.trim_end_matches(')').parse::<usize>().ok()
}

/// Harvest measure-helper identifiers (`averMeasure*`,
/// `averStringPosFuel`) from emitted Lean text. These are the names a
/// fuel wrapper's initial-fuel expression references; the
/// `SimpOverPreludeLemmas` rung needs them in its simp set so the fuel
/// value computes to a `Nat` literal before the `__fuel` equations
/// fire. Sorted + deduped for deterministic emit.
fn scan_measure_helper_names(text: &str) -> Vec<String> {
    let mut found: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for prefix in ["averMeasure", "averStringPosFuel"] {
        for (idx, _) in text.match_indices(prefix) {
            // Reject mid-identifier hits (`xaverMeasure`).
            if idx > 0
                && text[..idx]
                    .chars()
                    .next_back()
                    .is_some_and(|c| c.is_alphanumeric() || c == '_')
            {
                continue;
            }
            let rest = &text[idx..];
            let end = rest
                .find(|c: char| !(c.is_alphanumeric() || c == '_'))
                .unwrap_or(rest.len());
            found.insert(rest[..end].to_string());
        }
    }
    found.into_iter().collect()
}

fn emit_fuel_helper_def(
    helper_name: &str,
    params: &str,
    ret_type: &str,
    body: &str,
    outer_indent: &str,
) -> Vec<String> {
    let branch_indent = format!("{outer_indent}    ");
    [
        vec![format!(
            "{outer_indent}def {} (fuel : Nat) {} : {} :=",
            helper_name, params, ret_type
        )],
        vec![format!("{outer_indent}  match fuel with")],
        vec![format!(
            "{outer_indent}  | 0 => panic! \"{}\"",
            PROOF_FUEL_EXHAUSTED_MSG
        )],
        vec![format!("{outer_indent}  | {} + 1 =>", STRING_POS_FUEL_VAR)],
        indent_lines(body, &branch_indent),
    ]
    .into_iter()
    .flatten()
    .collect()
}

fn emit_string_pos_wrapper(fd: &FnDef, helper_name: &str, rank_budget: usize) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let (s_name, _) = &fd.params[0];
    let (pos_name, _) = &fd.params[1];
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} (averStringPosFuel {} {} {}) {}",
            helper_name,
            aver_name_to_lean(s_name),
            aver_name_to_lean(pos_name),
            rank_budget,
            arg_names
        ),
    ]
}

fn emit_int_countdown_wrapper(fd: &FnDef, helper_name: &str, param_index: usize) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let metric_name = fd
        .params
        .get(param_index)
        .map(|(name, _)| aver_name_to_lean(name))
        .unwrap_or_else(|| "0".to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} ((Int.natAbs {}) + 1) {}",
            helper_name, metric_name, arg_names
        ),
    ]
}

pub(super) fn emit_nat_linear_recurrence_fn(
    fd: &FnDef,
    shape: &super::recurrence::SecondOrderIntLinearRecurrenceShape,
    ctx: &CodegenContext,
) -> String {
    let fn_name = aver_name_to_lean(&fd.name);
    let nat_helper_name = recurrence_nat_helper_name(&fd.name);
    let lean_param = aver_name_to_lean(&shape.param_name);
    let ret_type = ret_type_or_unit(fd);
    let nat_step = render_affine_pair_expr(
        shape.recurrence,
        &format!("{nat_helper_name} n"),
        &format!("{nat_helper_name} (n + 1)"),
    );

    [
        emit_doc_comment(&fd.desc),
        vec![
            format!("private def {} : Nat -> {}", nat_helper_name, ret_type),
            format!("  | 0 => {}", emit_expr_legacy(&shape.base0, ctx, None)),
            format!("  | 1 => {}", emit_expr_legacy(&shape.base1, ctx, None)),
            format!("  | n + 2 => {}", nat_step),
            String::new(),
            format!("def {} ({} : Int) : {} :=", fn_name, lean_param, ret_type),
            format!(
                "  if {} < 0 then {} else {} {}.toNat",
                lean_param,
                emit_expr_legacy(&shape.negative_branch, ctx, None),
                nat_helper_name,
                lean_param
            ),
        ],
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_sizeof_measure_expr(fd: &FnDef, recursive_types: &HashSet<String>) -> Option<String> {
    let measure_terms: Vec<String> = sizeof_measure_param_indices(fd)
        .into_iter()
        .filter_map(|idx| {
            fd.params.get(idx).and_then(|(name, type_name)| {
                type_measure_expr(type_name, &aver_name_to_lean(name), recursive_types, None)
            })
        })
        .collect();

    (!measure_terms.is_empty()).then(|| measure_terms.join(" + "))
}

fn emit_mutual_sizeof_wrapper(
    fd: &FnDef,
    helper_name: &str,
    rank_budget: usize,
    recursive_types: &HashSet<String>,
) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let fuel_expr = emit_sizeof_measure_expr(fd, recursive_types)
        .map(|measure| format!("(({}) + 1) * {}", measure, rank_budget))
        .unwrap_or_else(|| rank_budget.to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!("  {} ({}) {}", helper_name, fuel_expr, arg_names),
    ]
}

pub(super) fn emit_fuelized_string_pos_fn(fd: &FnDef, ctx: &CodegenContext) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = emit_fn_body_for(fd, &rewritten, ctx);

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_string_pos_wrapper(fd, &helper_name, 1),
        emit_string_pos_scan_lemma(fd, &helper_name, ctx)
            .map(|lemma| vec![String::new(), lemma])
            .unwrap_or_default(),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

/// Companion theorem for a fuelized string-position SCANNER — the
/// general crack in the fuel-unfolding barrier (#125 family): when the
/// body matches the canonical shape `match String.charAtAv s pos with |
/// none => EXIT | some c => if P c then SELF(s, pos+1, …) else OTHER`
/// (recognized by `proof_recognize::detect_string_pos_scan`), emit
///
/// ```text
/// theorem <fn>__fuel_scan : ∀ fuel s pos <carried>,
///   0 ≤ pos → pos.toNat ≤ s.toList.length →
///   s.toList.length - pos.toNat < fuel →
///   (∀ ch ∈ s.toList.drop pos.toNat, P (Char.toString ch) = true) →
///   <fn>__fuel fuel s pos <args@pins> = EXIT[pos := ↑s.toList.length]
/// ```
///
/// proved by a FIXED fuel-induction template (`String.charAt_eq_of_lt`
/// / `String.charAt_none_of_ge` + `List.drop_eq_getElem_cons` + omega —
/// ported verbatim from the verified json hand proof). Universal-law
/// emissions (`IntDecimalRoundtrip`) rewrite through this lemma to run
/// a symbolic all-`P` suffix to the end of the string.
///
/// CONSERVATIVELY SHAPE-GATED: when the body does not match the exact
/// recognizer shape, NOTHING is emitted — every emission must be
/// provable by the uniform template BY CONSTRUCTION of the gate (a
/// synthesized lemma that fails to prove would be a build error in the
/// export). The predicate must also resolve to a pure single-`String`-
/// param `Bool` fn (the lemma cites it by name as a hypothesis key; it
/// is never unfolded).
fn emit_string_pos_scan_lemma(
    fd: &FnDef,
    helper_name: &str,
    ctx: &CodegenContext,
) -> Option<String> {
    let shape = crate::codegen::proof_recognize::detect_string_pos_scan(fd)?;
    let scope = ctx.active_module_scope();
    let pred_fd = ctx
        .fn_def_by_name(&shape.predicate_fn, scope.as_deref())
        .or_else(|| ctx.fn_def_by_name(&shape.predicate_fn, None))?;
    if !crate::codegen::proof_recognize::scan_predicate_fn_ok(pred_fd) {
        return None;
    }

    let s = aver_name_to_lean(&fd.params[0].0);
    let pos = aver_name_to_lean(&fd.params[1].0);
    let pred = aver_name_to_lean(&shape.predicate_fn);
    let lemma_name = format!("{helper_name}_scan");

    // Trailing args: carried params stay variables (quantified), pinned
    // params bake their Bool literal into statement + calc steps.
    let mut carried_binders: Vec<String> = Vec::new();
    let mut carried_names: Vec<String> = Vec::new();
    let mut trailing_args: Vec<String> = Vec::new();
    for (i, pin) in shape.param_pins.iter().enumerate() {
        let (name, ty) = &fd.params[i + 2];
        match pin {
            None => {
                let lean = aver_name_to_lean(name);
                carried_binders.push(format!(" ({} : {})", lean, type_annotation_to_lean(ty)));
                carried_names.push(lean.clone());
                trailing_args.push(lean);
            }
            Some(b) => trailing_args.push(b.to_string()),
        }
    }
    let args = trailing_args
        .iter()
        .map(|a| format!(" {a}"))
        .collect::<String>();
    let carried_binder_text: String = carried_binders.concat();
    let carried_intro = carried_names
        .iter()
        .map(|n| format!("{n} "))
        .collect::<String>();

    // EXIT[pos := ↑s.data.length, pinned := literal]: substitute at the
    // AST level (a unique marker stands in for the length cast, which
    // has no Aver-AST form), render through the SAME expr emitter the
    // body used, then swap the marker for the cast.
    const LEN_MARKER: &str = "AVERSCANLEN";
    let mut subst: std::collections::HashMap<String, crate::ast::Expr> =
        std::collections::HashMap::new();
    subst.insert(
        fd.params[1].0.clone(),
        crate::ast::Expr::Ident(LEN_MARKER.to_string()),
    );
    for (i, pin) in shape.param_pins.iter().enumerate() {
        if let Some(b) = pin {
            subst.insert(
                fd.params[i + 2].0.clone(),
                crate::ast::Expr::Literal(crate::ast::Literal::Bool(*b)),
            );
        }
    }
    let exit_subst =
        crate::codegen::proof_recognize::substitute_idents_in_expr(&shape.exit_expr, &subst);
    let exit = emit_expr_legacy(&exit_subst, ctx, None)
        .replace('\n', " ")
        .replace(LEN_MARKER, &format!("(({s}.toList.length : Int))"));

    Some(format!(
        r#"/-- Auto-synthesized scan lemma: an all-`{pred}` suffix scan runs to the
    end of the string. Companion to the `{helper_name}` fuel def; proved by
    the fixed fuel-induction template. -/
theorem {lemma_name} :
    ∀ (fuel : Nat) ({s} : String) ({pos} : Int){carried_binder_text},
      0 ≤ {pos} → {pos}.toNat ≤ {s}.toList.length →
      {s}.toList.length - {pos}.toNat < fuel →
      (∀ ch ∈ {s}.toList.drop {pos}.toNat, {pred} (Char.toString ch) = true) →
      {helper_name} fuel {s} {pos}{args} = {exit} := by
  intro fuel
  induction fuel with
  | zero =>
    intro {s} {pos} {carried_intro}h0 h1 h2 h3
    omega
  | succ fuel ih =>
    intro {s} {pos} {carried_intro}h0 h1 h2 h3
    by_cases hlt : {pos}.toNat < {s}.toList.length
    · have hch := String.charAt_eq_of_lt {s} {pos} h0 hlt
      have hdrop := List.drop_eq_getElem_cons (l := {s}.toList) (i := {pos}.toNat) hlt
      have hdig : {pred} (Char.toString ({s}.toList[{pos}.toNat])) = true := by
        apply h3
        rw [hdrop]
        exact List.mem_cons_self
      have hstep : ∀ ch ∈ {s}.toList.drop (({pos} + 1).toNat), {pred} (Char.toString ch) = true := by
        intro ch hc
        apply h3
        rw [hdrop]
        refine List.mem_cons_of_mem _ ?_
        have he : ({pos} + 1).toNat = {pos}.toNat + 1 := by omega
        rw [he] at hc
        exact hc
      have hrec := ih {s} ({pos} + 1) {carried_intro}(by omega) (by omega) (by omega) hstep
      calc {helper_name} (fuel + 1) {s} {pos}{args}
          = {helper_name} fuel {s} ({pos} + 1){args} := by
            simp only [{helper_name}, hch, hdig]
            simp
        _ = {exit} := hrec
    · have hpos : {pos} = ({s}.toList.length : Int) := by omega
      have hch := String.charAt_none_of_ge {s} {pos} h0 (by omega)
      simp only [{helper_name}, hch]
      rw [hpos]"#
    ))
}

fn strip_match_eq_binders(body: String) -> String {
    body.lines()
        .map(|line| {
            let trimmed = line.trim_start();
            let indent_len = line.len() - trimmed.len();
            let indent = &line[..indent_len];
            let Some(rest) = trimmed.strip_prefix("match h_") else {
                return line.to_string();
            };
            let Some(colon_idx) = rest.find(" : ") else {
                return line.to_string();
            };
            format!("{indent}match {}", &rest[colon_idx + 3..])
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Native `IntCountdown` emission for closed-world fns with the canonical
/// `match p { 0 -> BASE; _ -> rec(p-1, ...) }` shape. Splits the fn into:
///
/// - `<name>__aux` — the real recursion carrying an explicit `(h : p ≥ 0)`
///   precondition. Recursive callsites in its body are rewritten to call
///   `<name>__aux` instead of `<name>` with an extra `(by omega)` proof
///   obligation appended (synthesized via the
///   `OMEGA_PROOF_SENTINEL` ident — see `lean::expr::emit_expr`).
/// - `<name>` — the public wrapper preserving the original Aver signature.
///   Dispatches on `p ≥ 0` to the aux; the `p < 0` branch returns `BASE`
///   (the source's `0` arm). That falls outside the Aver well-formed
///   domain for the issue-84 fibonacci-style targets, but keeping the aux
///   private and total avoids forcing every call site (verify samples,
///   peer fn bodies) to thread proof obligations.
///
/// Lean accepts this because the aux's `termination_by p.natAbs` together
/// with `(h : p ≥ 0)` + the `_` arm's implicit `p ≠ 0` lets `omega`
/// discharge `(p - 1).natAbs < p.natAbs` mechanically.
pub(super) fn emit_native_guarded_int_countdown_fn(
    fd: &FnDef,
    ctx: &CodegenContext,
    param_index: usize,
    base_arm_literal: i64,
    base_arm_body: &Spanned<crate::ir::hir::ResolvedExpr>,
    wildcard_arm_body: &Spanned<crate::ir::hir::ResolvedExpr>,
    precondition: &[Spanned<crate::ir::hir::ResolvedExpr>],
) -> String {
    let aux_name = native_aux_name(&fd.name);
    let main_name = aver_name_to_lean(&fd.name);
    let lean_aux_name = aver_name_to_lean(&aux_name);
    let Some((param_name, _)) = fd.params.get(param_index) else {
        return emit_fuelized_int_countdown_fn(fd, ctx, param_index);
    };
    let lean_pname = aver_name_to_lean(param_name);

    // Precondition: AND of caller-derived predicates, or `(p ≥ 0)`
    // when the artifact has no single external caller (free-standing
    // fns / test fixtures). Same `Spanned<Expr>`-as-predicate path
    // opaque types use, so `emit_expr` is the single emitter — no
    // parallel infrastructure.
    let precond_lean = if precondition.is_empty() {
        format!("{} ≥ 0", lean_pname)
    } else {
        precondition
            .iter()
            .map(|p| format!("({})", super::expr::emit_expr(p, ctx)))
            .collect::<Vec<_>>()
            .join(" ∧ ")
    };

    let aux_params = format!("{} (h_dom : {})", emit_fn_params(&fd.params), precond_lean);
    let main_params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);

    // Emit `if h_zero : n = LITERAL then BASE else REC` rather than
    // `match n with | LITERAL => ... | _ => ...`. The dependent `if h :
    // c then ... else` form puts `h : c` / `h : ¬c` in scope for the
    // corresponding branch, which `omega` needs to discharge `(n - 1)
    // ≥ 0` and `(n - 1).natAbs < n.natAbs` at the recursive callsite +
    // termination check. Plain `match` would leave the case-split
    // implicit (only an unnamed `casesOn` motive carries it) and
    // `omega` can't see it.
    // Resolve the recursive fn's `FnId` via the same pointer-eq path
    // `ProofIR.fn_contracts` was keyed by — `fn_id_for_decl` picks
    // the owning module's prefix when `fd` came from a dep, the
    // entry slot when it sits in `ctx.fn_defs`. Bare-name
    // `FnKey::entry(fd.name)` would collide for any module-owned
    // recursive fn whose bare name also exists at entry (the very
    // class of bug #147 phase E is killing).
    let target_fn_id = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .unwrap_or_else(|| panic!("native-guarded fn {} missing FnId", fd.name));
    let rewritten_wc = crate::codegen::recursion::rewrite_native_guarded_calls_resolved_expr(
        wildcard_arm_body,
        target_fn_id,
        &aux_name,
    );
    let base_str = super::expr::emit_expr(base_arm_body, ctx);
    let rec_str = super::expr::emit_expr(&rewritten_wc, ctx);
    let arg_names = emit_fn_param_names(&fd.params);

    let mut lines = Vec::new();
    lines.extend(emit_doc_comment(&fd.desc));
    lines.push(format!(
        "def {} {} : {} :=",
        lean_aux_name, aux_params, ret_type
    ));
    lines.push(format!(
        "  if h_zero : {} = {} then {}",
        lean_pname, base_arm_literal, base_str
    ));
    lines.push(format!("  else {}", rec_str));
    lines.push(format!("termination_by Int.natAbs {}", lean_pname));
    lines.push("decreasing_by".to_string());
    lines.push("  simp_wf".to_string());
    lines.push("  omega".to_string());
    lines.push(String::new());

    lines.push(format!(
        "def {} {} : {} :=",
        main_name, main_params, ret_type
    ));
    lines.push(format!(
        "  if h_dom : {} then {} {} h_dom",
        precond_lean, lean_aux_name, arg_names
    ));
    lines.push(format!("  else {}", base_str));

    lines.join("\n")
}

pub(super) fn emit_fuelized_int_countdown_fn(
    fd: &FnDef,
    ctx: &CodegenContext,
    param_index: usize,
) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_int_countdown_wrapper(fd, &helper_name, param_index),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

pub(super) fn emit_fuelized_int_ascending_fn(
    fd: &FnDef,
    ctx: &CodegenContext,
    param_index: usize,
    bound_lean: &str,
) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_int_ascending_wrapper(fd, &helper_name, param_index, bound_lean),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_int_ascending_wrapper(
    fd: &FnDef,
    helper_name: &str,
    param_index: usize,
    bound_lean: &str,
) -> Vec<String> {
    let fn_name = super::expr::aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let metric_name = fd
        .params
        .get(param_index)
        .map(|(name, _)| super::expr::aver_name_to_lean(name))
        .unwrap_or_else(|| "0".to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} ((Int.natAbs ({} - {})) + 1) {}",
            helper_name, bound_lean, metric_name, arg_names
        ),
    ]
}

/// Read the rank component of a `Fuel { Lex { .., rank } }` contract.
/// Returns `None` when the fn has no contract or the contract isn't
/// a Lex shape (non-mutual variant or non-recursive).
fn contract_lex_rank(ctx: &CodegenContext, fd: &FnDef) -> Option<usize> {
    contract_lex_params_rank(ctx, fd).map(|(_, rank)| rank)
}

/// Read both the params Vec and rank of a `Fuel { Lex { params, rank } }`
/// contract. Returns `None` for non-Lex / non-recursive / missing
/// contracts. Used by mutual-SCC dispatchers to distinguish:
///
/// - `MutualIntCountdown`: `params.len() == 1`, rank == 0
/// - `MutualStringPosAdvance`: `params.len() == 2`
/// - `MutualSizeOfRanked`: `params.is_empty()`
pub(super) fn contract_lex_params_rank<'a>(
    ctx: &'a CodegenContext,
    fd: &FnDef,
) -> Option<(&'a [String], usize)> {
    let contract = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)?;
    let crate::ir::RecursionContract::Fuel {
        fuel_metric: crate::ir::FuelMetric::Lex { params, rank },
    } = contract.recursion.as_ref()?
    else {
        return None;
    };
    Some((params.as_slice(), *rank))
}

pub(super) fn emit_fuelized_mutual_string_pos_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let max_rank = fns
        .iter()
        .filter_map(|fd| contract_lex_rank(ctx, fd))
        .max()
        .unwrap_or(1);

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = emit_fn_body_for(fd, &rewritten, ctx);

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines = emit_string_pos_wrapper(fd, &helper_name, max_rank);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}

pub(super) fn emit_fuelized_mutual_int_countdown_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines = emit_int_countdown_wrapper(fd, &helper_name, 0);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}

/// Termination measure for Lean's native `termination_by` clause —
/// `name.length` for List/Vector/String params (Lean's stdlib
/// `List.length` is what `decreasing_tactic` knows how to chase),
/// `sizeOf name` fallback for recursive ADTs. Sum across every
/// sizeOf-relevant param.
///
/// Epic #180 Phase 4 — reads param types from the resolved fn def
/// (already typed by the typechecker) instead of re-parsing the
/// AST annotation string.
fn emit_native_termination_measure(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    let indices = crate::codegen::recursion::detect::sizeof_measure_param_indices(fd);
    if indices.is_empty() {
        return None;
    }
    // Pointer-eq scope so a same-bare-name twin never provides
    // these param types. Synthetic / mid-rewrite fns fall back to
    // on-demand resolve.
    let resolved_fd = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id));
    let resolved_owned = match resolved_fd {
        Some(_) => None,
        None => Some(ctx.resolve_fn_def(fd, None)),
    };
    let rfd: &crate::ir::hir::ResolvedFnDef =
        resolved_fd.unwrap_or_else(|| resolved_owned.as_ref().unwrap().as_ref());
    let mut terms: Vec<String> = Vec::new();
    for idx in indices {
        let (name, ty) = rfd.params.get(idx)?;
        let lean_name = aver_name_to_lean(name);
        match ty {
            crate::types::Type::List(_) | crate::types::Type::Vector(_) => {
                // `sizeOf` instead of `.length` so the user measure
                // matches what Lean's mutual-block wf elaboration
                // generates internally — `decreasing_tactic` then
                // closes the chain without `simp_wf` scrambling.
                terms.push(format!("sizeOf {lean_name}"));
            }
            // Skip Named ADTs and String: `sizeOf` decrease on a
            // recursive ADT in a multi-arg measure (`step f` from
            // `stepApp f arg`, measure `sizeOf f + sizeOf arg`)
            // needs the strict-positivity fact `sizeOf arg ≥ 1`
            // which omega doesn't get for free. Fall back to fuel
            // for those SCCs — accumulator-pattern guard already
            // filters them too, so the only effect here is a more
            // conservative measure-existence gate.
            _ => return None,
        }
    }
    (!terms.is_empty()).then(|| terms.join(" + "))
}

/// Native termination emission for mutual-recursion SCCs whose
/// every member has a sizeOf measure (List / Vector / String) and a
/// classifier rank — Lean 4 `mutual ... end` block with one
/// `termination_by` per def, lex tuple `(sizeOf_sum, rank)` from
/// `MutualSizeOfRanked`. Mirrors the Dafny native path from #83.
///
/// Returns `None` when:
/// - SCC isn't fully `MutualSizeOfRanked` (caller picks fuel)
/// - Any member has no inferable sizeOf measure
/// - Growing-accumulator pattern detected (tail-rec `[x] + acc`
///   shapes won't decrease the lex tuple)
pub(super) fn emit_native_mutual_sizeof_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> Option<String> {
    let mut ranks: HashMap<String, usize> = HashMap::new();
    for fd in fns {
        if !is_pure_fn(fd) {
            return None;
        }
        // MutualSizeOfRanked carries `params: vec![]` + rank>=1; any
        // other Lex shape (single-param mutual int-countdown, two-
        // param string-pos) fails this group's pre-conditions.
        match contract_lex_params_rank(ctx, fd) {
            Some(([], rank)) => {
                ranks.insert(fd.name.clone(), rank);
            }
            _ => return None,
        }
    }
    let mut measures: HashMap<String, String> = HashMap::new();
    for fd in fns {
        let measure = emit_native_termination_measure(fd, ctx)?;
        measures.insert(fd.name.clone(), measure);
    }
    if crate::codegen::recursion::detect::scc_has_growing_accumulator(fns) {
        return None;
    }

    let mut lines: Vec<String> = vec!["mutual".to_string()];
    for fd in fns {
        let measure = measures.get(&fd.name).unwrap();
        let rank = ranks.get(&fd.name).unwrap();
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body_fn = lowered.as_ref().unwrap_or(fd);
        let body_ast = lowered
            .as_ref()
            .map(|l| l.body.as_ref())
            .unwrap_or(fd.body.as_ref());
        let body = emit_fn_body_for(body_fn, body_ast, ctx);

        lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        for body_line in body.lines() {
            lines.push(format!("  {body_line}"));
        }
        lines.push(format!("  termination_by ({measure}, {rank})"));
        // Robust tactic chain — `decreasing_tactic` alone bottoms out
        // on simple shapes (BigInt) but Lean elaborator on multi-arg
        // mutual SCCs sometimes needs `simp_wf` to unfold sizeOf
        // before omega can close the arithmetic on lengths.
        lines.push(
            "  decreasing_by all_goals (first | decreasing_tactic | (simp_wf; (try simp_all); first | omega | (constructor <;> first | rfl | omega)))"
                .to_string(),
        );
        lines.push(String::new());
    }
    lines.push("end".to_string());
    Some(lines.join("\n"))
}

pub(super) fn emit_fuelized_mutual_sizeof_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let recursive_types: HashSet<String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .filter(|td| is_recursive_type_def(td))
        .map(|td| type_def_name(td).to_string())
        .collect();
    let rank_budget = fns
        .iter()
        .filter_map(|fd| contract_lex_rank(ctx, fd))
        .max()
        .unwrap_or(1)
        + 1;

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = emit_fn_body_for(fd, &rewritten, ctx);

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines =
                emit_mutual_sizeof_wrapper(fd, &helper_name, rank_budget, &recursive_types);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}
