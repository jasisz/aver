use std::collections::HashSet;

use super::expr::{aver_name_to_lean, emit_expr, resolve_rewrite_output};
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
use crate::codegen::recursion::cycle_measure::{Candidate, MeasureKind};
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
            format!(
                "  | 0 => {}",
                emit_expr(&resolve_rewrite_output(&shape.base0, ctx, None), ctx)
            ),
            format!(
                "  | 1 => {}",
                emit_expr(&resolve_rewrite_output(&shape.base1, ctx, None), ctx)
            ),
            format!("  | n + 2 => {}", nat_step),
            String::new(),
            format!("def {} ({} : Int) : {} :=", fn_name, lean_param, ret_type),
            format!(
                "  if {} < 0 then {} else {} {}.toNat",
                lean_param,
                emit_expr(
                    &resolve_rewrite_output(&shape.negative_branch, ctx, None),
                    ctx
                ),
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

/// The parameters the fuel seed counts — every one with a measure
/// expression — each with its term, in signature order.
fn seed_measure_terms(fd: &FnDef, recursive_types: &HashSet<String>) -> Vec<(usize, String)> {
    sizeof_measure_param_indices(fd)
        .into_iter()
        .filter_map(|idx| {
            let (name, type_name) = fd.params.get(idx)?;
            let term =
                type_measure_expr(type_name, &aver_name_to_lean(name), recursive_types, None)?;
            Some((idx, term))
        })
        .collect()
}

fn emit_sizeof_measure_expr(fd: &FnDef, recursive_types: &HashSet<String>) -> Option<String> {
    let measure_terms: Vec<String> = seed_measure_terms(fd, recursive_types)
        .into_iter()
        .map(|(_, term)| term)
        .collect();

    (!measure_terms.is_empty()).then(|| measure_terms.join(" + "))
}

/// The recursive user types in scope, by bare name: the ones the seed
/// measures by a measure function of their own.
fn recursive_type_names(ctx: &CodegenContext) -> HashSet<String> {
    ctx.modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .filter(|td| is_recursive_type_def(td))
        .map(|td| type_def_name(td).to_string())
        .collect()
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
        emit_simple_string_pos_stability_lemma(fd, &helper_name, 1, &body)
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
    let exit = emit_expr(&resolve_rewrite_output(&exit_subst, ctx, None), ctx)
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

/// Companion stability lemma for a simple string-position fuel SKIPPER —
/// `<fn>__fuel fuel s pos = <fn> s pos` whenever `fuel` meets the wrapper's
/// initial `averStringPosFuel s pos rank` budget. Gated to the skip shape
/// (`match charAt with none => pos | some c => match c with <lit> => self(s,
/// pos+1) … | _ => pos`) by [`detect_simple_string_pos_skip_literal`]; the
/// literal itself is only the shape witness — the proof is literal-COUNT
/// agnostic (handles single- and multi-literal skippers like json's four-way
/// `skipWs` uniformly), so the detected literal value is discarded.
///
/// The recursive branch never case-splits on the character: once
/// `charAt = some c` is fixed, both `<fn>__fuel (fuel+1)` and the wrapper's
/// `<fn>__fuel (measure'+1)` unfold to the SAME inner `match Char.toString c`
/// whose only difference is the recursive fuel argument (`fuel` vs `measure'`),
/// and one `simp only [<fn>__fuel, hchar, hleft]` closes it by rewriting that
/// argument — independent of how many literal arms recurse.
///
/// FAIL-SOFT: the whole proof is wrapped in `first | (<skeleton>) | sorry`
/// (mirroring the off-probe floor of
/// [`crate::codegen::lean::law_auto::transparent_chain`]). A gated shape the
/// skeleton cannot close degrades to a non-fatal `declaration uses 'sorry'`
/// warning instead of a hard `unsolved goals` build error. The lemma is not
/// cited by any law, so its own `sorry` never enters another law's
/// `#print axioms` set — universal credit for laws that do not reference it is
/// untouched. A bare `sorry` (not the `AVERSPEC_SORRY:<id>` trace) is used
/// deliberately: the trace exists so `speculative::parse_failures` can demote a
/// non-closing conditional LAW to bounded, and this support lemma has no law
/// tier to demote and no `speculative::admits` consumer.
fn emit_simple_string_pos_stability_lemma(
    fd: &FnDef,
    helper_name: &str,
    rank_budget: usize,
    emitted_body: &str,
) -> Option<String> {
    // Gate only: the detected literal is the shape witness; the robust proof
    // below is literal-agnostic, so the value is discarded.
    detect_simple_string_pos_skip_literal(fd).or_else(|| {
        detect_simple_string_pos_skip_literal_from_body(fd, helper_name, emitted_body)
    })?;
    let fn_name = aver_name_to_lean(&fd.name);
    let s = aver_name_to_lean(&fd.params.first()?.0);
    let pos = aver_name_to_lean(&fd.params.get(1)?.0);
    let params = emit_fn_params(&fd.params);
    let args = emit_fn_param_names(&fd.params);
    let binders = if params.is_empty() {
        String::new()
    } else {
        format!(" {params}")
    };
    let lemma_name = format!("{helper_name}_stable");

    Some(format!(
        r#"theorem {lemma_name} :
    ∀ (fuel : Nat){binders},
      averStringPosFuel {s} {pos} {rank_budget} ≤ fuel →
      {helper_name} fuel {args} = {fn_name} {args} := by
  first
  | (intro fuel
     induction fuel with
     | zero =>
         intro {args} h
         unfold averStringPosFuel at h
         omega
     | succ fuel ih =>
         intro {args} h
         unfold {fn_name}
         have hmeasure_pos : 0 < averStringPosFuel {s} {pos} {rank_budget} := by
           unfold averStringPosFuel
           omega
         cases hmeasure : averStringPosFuel {s} {pos} {rank_budget} with
         | zero =>
             omega
         | succ measure' =>
             by_cases hneg : {pos} < 0
             · have hchar : String.charAtAv {s} {pos} = none := by
                 unfold String.charAtAv
                 simp [hneg]
               simp [{helper_name}, hchar]
             · have h0 : 0 ≤ {pos} := by omega
               by_cases hlt : {pos}.toNat < {s}.toList.length
               · have hchar := String.charAt_eq_of_lt {s} {pos} h0 hlt
                 have hnext_measure : averStringPosFuel {s} ({pos} + 1) {rank_budget} = measure' := by
                   unfold averStringPosFuel at h hmeasure ⊢
                   omega
                 have hleft : {helper_name} fuel {s} ({pos} + 1) = {helper_name} measure' {s} ({pos} + 1) := by
                   have hstep : {helper_name} fuel {s} ({pos} + 1) = {fn_name} {s} ({pos} + 1) := by
                     apply ih
                     rw [hnext_measure]
                     unfold averStringPosFuel at h hmeasure
                     omega
                   rw [hstep]
                   unfold {fn_name}
                   rw [hnext_measure]
                 simp only [{helper_name}, hchar, hleft]
               · have hchar := String.charAt_none_of_ge {s} {pos} h0 (by omega)
                 simp [{helper_name}, hchar])
  | sorry"#
    ))
}

pub(in crate::codegen::lean) fn detect_simple_string_pos_skip_literal(
    fd: &FnDef,
) -> Option<String> {
    if fd.params.len() != 2 {
        return None;
    }
    let s_name = &fd.params[0].0;
    let pos_name = &fd.params[1].0;
    let [Stmt::Expr(expr)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &expr.node else {
        return None;
    };
    if !is_string_char_at(subject.as_ref(), s_name, pos_name) {
        return None;
    }

    // FAIL-CLOSED: the outer match must be EXACTLY the `charAtAv s pos`
    // none/some pair — `Option.None -> pos` (terminal exit) and
    // `Option.Some(c) -> <inner char match>` (the recursion). Every other
    // outer arm declines graduation so the fn stays fueled:
    //   * a catch-all `_` (BLOCKER 2): the native emitter names the sole
    //     outer discriminant (`match hc_scan : charAtAv …`) so
    //     `decreasing_by` can read `charAtAv s pos = some c`. A wildcard
    //     arm produces no `= some c` equation, so the graduated def
    //     hard-errors on its termination proof — a native def cannot
    //     sorry-floor termination, so this must never be emitted.
    //   * a `None` arm returning anything but `pos`, or an unexpected extra
    //     constructor — not the graduating skip shape.
    let mut saw_none_exit = false;
    let mut recursive_literal = None;
    let mut saw_fallback_exit = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Constructor(name, fields) if name == "Option.None" && fields.is_empty() => {
                if !expr_is_ident(&arm.body, pos_name) {
                    return None;
                }
                saw_none_exit = true;
            }
            Pattern::Constructor(name, fields) if name == "Option.Some" && fields.len() == 1 => {
                let (lit, fallback) = detect_inner_char_match_literal(
                    &arm.body, &fields[0], &fd.name, s_name, pos_name,
                )?;
                recursive_literal = Some(lit);
                saw_fallback_exit = fallback;
            }
            _ => return None,
        }
    }

    match (saw_none_exit, saw_fallback_exit, recursive_literal) {
        (true, true, Some(lit)) => Some(lit),
        _ => None,
    }
}

fn detect_simple_string_pos_skip_literal_from_body(
    fd: &FnDef,
    helper_name: &str,
    body: &str,
) -> Option<String> {
    let s_name = aver_name_to_lean(&fd.params.first()?.0);
    let pos_name = aver_name_to_lean(&fd.params.get(1)?.0);
    let recursive_suffix =
        format!("=> {helper_name} {STRING_POS_FUEL_VAR} {s_name} ({pos_name} + 1)");
    let none_exit = format!("| .none => {pos_name}");
    let fallback_exit = format!("| _ => {pos_name}");
    if !body.lines().any(|line| line.trim() == none_exit)
        || !body.lines().any(|line| line.trim() == fallback_exit)
    {
        return None;
    }
    body.lines().find_map(|line| {
        let trimmed = line.trim();
        if !trimmed.ends_with(&recursive_suffix) {
            return None;
        }
        let rest = trimmed.strip_prefix("| \"")?;
        let end = rest.find("\" =>")?;
        Some(rest[..end].to_string())
    })
}

fn detect_inner_char_match_literal(
    expr: &Spanned<Expr>,
    binding: &str,
    fn_name: &str,
    s_name: &str,
    pos_name: &str,
) -> Option<(String, bool)> {
    let Expr::Match { subject, arms } = &expr.node else {
        return None;
    };
    if !binding.is_empty() && !expr_is_ident(subject, binding) {
        return None;
    }
    let mut recursive_literal = None;
    let mut fallback_exit = false;
    for arm in arms {
        match &arm.pattern {
            // An advancing arm: EXACTLY `self(s, pos + 1)` under the
            // `charAtAv s pos = some c` guard — the only recursive step
            // whose strict decrease of `s.length - pos` the native
            // `decreasing_by` can close.
            Pattern::Literal(Literal::Str(lit))
                if expr_is_self_pos_plus_one_tailcall(&arm.body, fn_name, s_name, pos_name) =>
            {
                recursive_literal = Some(lit.clone());
            }
            // The catch-all terminal exit `_ -> pos`.
            Pattern::Wildcard if expr_is_ident(&arm.body, pos_name) => {
                fallback_exit = true;
            }
            // FAIL-CLOSED: every remaining arm must be a TERMINAL,
            // non-recursive arm (no self-call anywhere in its body). An arm
            // carrying a self-call that is NOT the exact `self(s, pos + 1)`
            // advance — a non-advancing `self(s, pos)`, a non-unit
            // `self(s, pos + 2)`, or an advance routed through a helper
            // `self(s, f(pos))` — graduates a native def whose
            // `decreasing_by` cannot close (or silently changes the proven
            // step shape), so it is unclassifiable: decline and stay fueled.
            _ => {
                if expr_contains_self_call(&arm.body, fn_name) {
                    return None;
                }
            }
        }
    }
    recursive_literal.map(|lit| (lit, fallback_exit))
}

fn is_string_char_at(expr: &Spanned<Expr>, s_name: &str, pos_name: &str) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    if args.len() != 2 || !expr_is_ident(&args[0], s_name) || !expr_is_ident(&args[1], pos_name) {
        return false;
    }
    let Expr::Attr(base, method) = &callee.node else {
        return false;
    };
    method == "charAt" && expr_is_ident(base, "String")
}

fn expr_is_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    // Accept both raw `Ident` (pre-resolution, e.g. the recursion
    // classifier) and the resolver's `Resolved { name, .. }` slot form
    // (fn bodies at proof-emit time are resolved in place). Without the
    // `Resolved` arm this shape detection silently declines on every
    // emit-time body — the reason the graduation gate and the #643
    // stability gate previously depended on the string `_from_body`
    // fallback.
    match &expr.node {
        Expr::Ident(n) => n == name,
        Expr::Resolved { name: n, .. } => n == name,
        _ => false,
    }
}

fn expr_is_self_pos_plus_one_tailcall(
    expr: &Spanned<Expr>,
    fn_name: &str,
    s_name: &str,
    pos_name: &str,
) -> bool {
    let (target, args) = match &expr.node {
        Expr::TailCall(data) => (&data.target, &data.args),
        Expr::FnCall(callee, args) => match &callee.node {
            Expr::Ident(name) => (name, args),
            Expr::Resolved { name, .. } => (name, args),
            _ => return false,
        },
        _ => return false,
    };
    target == fn_name
        && args.len() == 2
        && expr_is_ident(&args[0], s_name)
        && expr_is_pos_plus_one(&args[1], pos_name)
}

fn expr_is_pos_plus_one(expr: &Spanned<Expr>, pos_name: &str) -> bool {
    matches!(
        &expr.node,
        Expr::BinOp(
            BinOp::Add,
            left,
            right
        ) if expr_is_ident(left, pos_name)
            && matches!(&right.node, Expr::Literal(Literal::Int(1)))
    )
}

/// True iff `expr` (recursively) contains a call whose target is the
/// recursive fn itself (`fn_name`). Covers post-TCO `TailCall`s — whose
/// target the generic `recursion::expr_references_ident` walker skips,
/// checking only its args — and both `Ident` and resolver-`Resolved`
/// `FnCall` callees. Used by [`detect_inner_char_match_literal`] to
/// reject an inner-match arm carrying a self-call that is NOT the exact
/// `self(s, pos + 1)` advance: such an arm graduates a native def whose
/// `decreasing_by` cannot close, so the fn must stay fueled. No existing
/// helper reports TailCall targets, so this focused walker is the
/// smallest fail-closed check.
fn expr_contains_self_call(expr: &Spanned<Expr>, fn_name: &str) -> bool {
    match &expr.node {
        Expr::TailCall(data) => {
            data.target == fn_name
                || data
                    .args
                    .iter()
                    .any(|a| expr_contains_self_call(a, fn_name))
        }
        Expr::FnCall(callee, args) => {
            matches!(&callee.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == fn_name)
                || expr_contains_self_call(callee, fn_name)
                || args.iter().any(|a| expr_contains_self_call(a, fn_name))
        }
        Expr::Attr(obj, _) => expr_contains_self_call(obj, fn_name),
        Expr::BinOp(_, l, r) => {
            expr_contains_self_call(l, fn_name) || expr_contains_self_call(r, fn_name)
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => expr_contains_self_call(inner, fn_name),
        Expr::Match { subject, arms } => {
            expr_contains_self_call(subject, fn_name)
                || arms
                    .iter()
                    .any(|a| expr_contains_self_call(&a.body, fn_name))
        }
        Expr::Constructor(_, inner) => inner
            .as_deref()
            .is_some_and(|e| expr_contains_self_call(e, fn_name)),
        Expr::InterpolatedStr(parts) => parts
            .iter()
            .any(|p| matches!(p, StrPart::Parsed(e) if expr_contains_self_call(e, fn_name))),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(|i| expr_contains_self_call(i, fn_name))
        }
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_contains_self_call(k, fn_name) || expr_contains_self_call(v, fn_name)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, v)| expr_contains_self_call(v, fn_name)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_contains_self_call(base, fn_name)
                || updates
                    .iter()
                    .any(|(_, v)| expr_contains_self_call(v, fn_name))
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => false,
    }
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

/// The parameters of `fd` a native `termination_by` measure may count, each
/// with the measure it carries and whether it is a recursive user ADT.
///
/// Lists, vectors and maps (list-backed in the proof model) carry `sizeOf`;
/// so does a recursive ADT, whose constructor sub-terms are structurally
/// smaller. An `Int` carries `Int.toNat`, which a guarded `n - k` strictly
/// decreases. Strings (position recursion, not structural) and every other
/// shape carry nothing; a group that decreases only on one of those stays on
/// fuel.
///
/// Param types come from the resolved fn def (already typed by the
/// typechecker) rather than the AST annotation string. Pointer-eq scope so a
/// same-bare-name twin never provides them. SCC classification only accepts
/// source declarations already present in the resolved program view.
fn native_measure_candidates(fd: &FnDef, ctx: &CodegenContext) -> Vec<(Candidate, bool)> {
    let rfd = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id))
        .expect("Lean native measure candidate must be a resolved source declaration");
    let candidate = |index, kind| Candidate { index, kind };
    rfd.params
        .iter()
        .enumerate()
        .filter_map(|(index, (_, ty))| match ty {
            crate::types::Type::List(_)
            | crate::types::Type::Vector(_)
            | crate::types::Type::Map(_, _) => {
                Some((candidate(index, MeasureKind::Structural), false))
            }
            crate::types::Type::Int => Some((candidate(index, MeasureKind::Countdown), false)),
            // backend-link-stage: name-keyed recursive-type-def lookup (same as
            // the fuel path's `recursive_types`); the measure only needs WHETHER
            // this Named type is recursive, not its `id`, so the bare-name match
            // is sufficient here.
            crate::types::Type::Named { name, .. }
                if user_type_def(ctx, name).is_some_and(is_recursive_type_def) =>
            {
                Some((candidate(index, MeasureKind::Structural), true))
            }
            _ => None,
        })
        .collect()
}

/// A user type def by bare name, across the entry and every dep module.
fn user_type_def<'a>(ctx: &'a CodegenContext, name: &str) -> Option<&'a TypeDef> {
    ctx.modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .find(|td| type_def_name(td) == name)
}

/// One member's native measure: the parameters the call edge analysis
/// counts — each with whether it is a recursive user ADT and its Lean term
/// — and the tie-break rank.
pub(in crate::codegen::lean) struct NativeMeasure {
    params: Vec<(Candidate, bool, String)>,
    rank: usize,
}

impl NativeMeasure {
    /// The `termination_by` sum.
    fn sum(&self) -> String {
        self.params
            .iter()
            .map(|(_, _, term)| term.as_str())
            .collect::<Vec<_>>()
            .join(" + ")
    }

    /// The positions measured by `sizeOf`.
    fn structural_indices(&self) -> Vec<usize> {
        self.params
            .iter()
            .filter(|(c, _, _)| c.kind == MeasureKind::Structural)
            .map(|(c, _, _)| c.index)
            .collect()
    }

    /// Whether the measure sums a recursive ADT with anything else. Such
    /// sums are native-closable for some shapes (an `eval` SCC builds) but
    /// NOT all — a red-black-tree mutual SCC's `decreasing_by` does not
    /// close, hard-failing the build — so they stay on fuel pending a
    /// per-SCC closure check; lifting it blanket regressed
    /// `proof_export_lake_builds_red_black_tree`.
    fn sums_an_adt(&self) -> bool {
        self.params.len() > 1 && self.params.iter().any(|(_, adt, _)| *adt)
    }
}

/// The native measure of every member of a mutual group, chosen by the
/// call edge analysis in [`crate::codegen::recursion::cycle_measure`].
///
/// `Err(reason)` is the analysis's refusal: no measure decreases on every
/// call, and the reason names the call that fails.
pub(in crate::codegen::lean) fn native_cycle_measure(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> Result<Vec<NativeMeasure>, String> {
    let candidates: Vec<Vec<(Candidate, bool)>> = fns
        .iter()
        .map(|fd| native_measure_candidates(fd, ctx))
        .collect();
    let plain: Vec<Vec<Candidate>> = candidates
        .iter()
        .map(|cs| cs.iter().map(|(c, _)| *c).collect())
        .collect();
    let measures = crate::codegen::recursion::cycle_measure::measure_for_cycle(fns, &plain, true)
        .map_err(|refusal| refusal.reason)?;
    Ok(fns
        .iter()
        .zip(measures.iter().zip(&candidates))
        .map(|(fd, (measure, member_candidates))| {
            let params = measure
                .params
                .iter()
                .map(|p| {
                    let adt = member_candidates
                        .iter()
                        .any(|(c, adt)| c.index == p.index && *adt);
                    let lean_name = aver_name_to_lean(&fd.params[p.index].0);
                    let term = match p.kind {
                        // `sizeOf` instead of `.length` so the user measure
                        // matches what Lean's mutual-block wf elaboration
                        // generates internally — `decreasing_tactic` then
                        // closes the chain without `simp_wf` scrambling.
                        MeasureKind::Structural => format!("sizeOf {lean_name}"),
                        MeasureKind::Countdown => format!("Int.toNat {lean_name}"),
                    };
                    (*p, adt, term)
                })
                .collect();
            NativeMeasure {
                params,
                rank: measure.rank,
            }
        })
        .collect())
}

/// Why this backend does not state a measure the analysis found, when it
/// does not: a member whose sum counts a recursive ADT with anything else,
/// a call into the group from inside a string interpolation, or a counted
/// position handed a value the elaborator cannot see shrink (a computed
/// one, or `Map.entries(m)`). Positions the measure does not count are not
/// looked at: what a call passes there never appears in a `decreasing_by`
/// goal.
fn native_measure_back_off(fns: &[&FnDef], measures: &[NativeMeasure]) -> Option<String> {
    use crate::codegen::recursion::detect::{
        scc_computed_measure_arg, scc_member_calling_inside_interpolation,
    };
    if let Some((fd, _)) = fns
        .iter()
        .zip(measures)
        .find(|(_, measure)| measure.params.is_empty())
    {
        // Unreachable by construction — a member counting nothing sits on
        // a cycle of unchanged calls, which the analysis refuses — and
        // cheap to keep out of a `termination_by (, r)`.
        return Some(format!("the measure of `{}` counts nothing", fd.name));
    }
    if let Some((fd, _)) = fns
        .iter()
        .zip(measures)
        .find(|(_, measure)| measure.sums_an_adt())
    {
        return Some(format!(
            "the measure of `{}` counts a recursive type together with another parameter, which the Lean export does not state natively",
            fd.name
        ));
    }
    if let Some(fd) = scc_member_calling_inside_interpolation(fns) {
        return Some(format!(
            "`{}` calls into the group from inside a string interpolation, under which the Lean export does not state a measure",
            fd.name
        ));
    }
    let measured = |fd: &FnDef| -> Vec<usize> {
        fns.iter()
            .position(|peer| peer.name == fd.name)
            .map(|member| measures[member].structural_indices())
            .unwrap_or_default()
    };
    scc_computed_measure_arg(fns, &measured).map(|(caller, callee, slot, arg)| {
        format!(
            "the call from `{}` to `{}` passes `{}` for `{}`, which the Lean export cannot see shrink",
            caller.name,
            callee.name,
            crate::codegen::recursion::cycle_measure::describe(arg),
            callee.params[slot].0
        )
    })
}

/// The parameters the fuel seed of `fd` counts, as candidates for a measure
/// the seed bounds: every one with a measure expression — a list, a map, a
/// recursive type, and a tuple or an option holding one, which the export
/// states no termination measure over — is structural, and every part of
/// one is smaller by that measure (a list's head as much as its tail, a
/// constructor's field, a tuple's component); the `Int` parameters are
/// countdowns, which the seed does not count.
fn seed_measure_candidates(fd: &FnDef, recursive_types: &HashSet<String>) -> Vec<Candidate> {
    let counted: Vec<usize> = seed_measure_terms(fd, recursive_types)
        .into_iter()
        .map(|(index, _)| index)
        .collect();
    fd.params
        .iter()
        .enumerate()
        .filter_map(|(index, (_, type_name))| {
            let kind = if counted.contains(&index) {
                MeasureKind::Structural
            } else if type_name == "Int" {
                MeasureKind::Countdown
            } else {
                return None;
            };
            Some(Candidate { index, kind })
        })
        .collect()
}

/// Why the fuel seed of `fns` — the sizes of the parameters it counts — is
/// not a proven recursion bound, as far as the call edge analysis can say:
/// the sentence the fuel-cone decline cites, so a claim lost behind a fuel
/// fallback says which call the exporter could not see shrink.
///
/// The analysis is run over exactly what the seed counts
/// ([`seed_measure_candidates`]): a measure over those parameters that
/// decreases on every call bounds the calls by the seed. The seed bounds
/// nothing when the analysis refuses — no measure decreases on every call,
/// and the refusal names the one that fails — when a call hands a value the
/// caller computed into a position the seed counts, or when the measure
/// counts an `Int` down, which the seed does not; in the latter two the
/// reason says first why the group is on fuel at all (see
/// [`native_measure_back_off`]) and then why the seed is no bound. `None`
/// when the seed is a bound.
pub(in crate::codegen::lean) fn native_measure_refusal(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> Option<String> {
    use crate::codegen::recursion::cycle_measure::{describe, measure_for_cycle};
    use crate::codegen::recursion::detect::scc_unproven_computed_measure_edge;
    let recursive_types = recursive_type_names(ctx);
    let candidates: Vec<Vec<Candidate>> = fns
        .iter()
        .map(|fd| seed_measure_candidates(fd, &recursive_types))
        .collect();
    let measures = match measure_for_cycle(fns, &candidates, true) {
        Err(refusal) => return Some(refusal.reason),
        Ok(measures) => measures,
    };
    let computed = scc_unproven_computed_measure_edge(fns).map(|(caller, callee, slot, arg)| {
        format!(
            "the call from `{}` to `{}` passes `{}` for `{}`, which the fuel seed counts and the Lean export cannot see shrink",
            caller.name,
            callee.name,
            describe(arg),
            callee.params[slot].0
        )
    });
    let countdown = fns.iter().zip(&measures).find_map(|(fd, measure)| {
        measure
            .params
            .iter()
            .find(|c| c.kind == MeasureKind::Countdown)
            .map(|c| {
                format!(
                    "the recursion is bounded by `{}` of `{}` counting down, which the fuel seed does not count",
                    fd.params[c.index].0, fd.name
                )
            })
    });
    if computed.is_none() && countdown.is_none() {
        return None;
    }
    let back_off = native_cycle_measure(fns, ctx)
        .ok()
        .and_then(|measures| native_measure_back_off(fns, &measures));
    let reasons: Vec<String> = back_off
        .into_iter()
        .chain(computed)
        .chain(countdown)
        .collect();
    Some(reasons.join("; "))
}

/// Native termination emission for mutual-recursion SCCs planned as
/// `MutualSizeOfRanked` — a Lean 4 `mutual ... end` block with one
/// `termination_by` per def, the lex tuple `(measure, rank)` chosen by the
/// call edge analysis so it decreases on every call between the members.
/// Mirrors the Dafny native path from #83.
///
/// Returns `None` when:
/// - SCC isn't fully `MutualSizeOfRanked` (caller picks fuel)
/// - The analysis finds no measure that decreases on every call
/// - This backend backs off from the measure it finds: see
///   [`native_measure_back_off`]
pub(super) fn emit_native_mutual_sizeof_group(
    fns: &[&FnDef],
    ctx: &CodegenContext,
) -> Option<String> {
    for fd in fns {
        if !is_pure_fn(fd) {
            return None;
        }
        // MutualSizeOfRanked carries `params: vec![]` + rank>=1; any
        // other Lex shape (single-param mutual int-countdown, two-
        // param string-pos) fails this group's pre-conditions. The
        // contract's rank is the plan's ordering of same-measure calls;
        // the emitted rank comes from the call edge analysis below, which
        // orders exactly the calls the chosen measure leaves unchanged.
        if !matches!(contract_lex_params_rank(ctx, fd), Some(([], _))) {
            return None;
        }
    }
    // One measure per member that decreases on EVERY call between them, or
    // none: a group the analysis refuses, or this backend backs off from,
    // lowers with fuel and the reason reaches the claims behind it through
    // `native_measure_refusal`.
    let measures = native_cycle_measure(fns, ctx).ok()?;
    if native_measure_back_off(fns, &measures).is_some() {
        return None;
    }

    let mut lines: Vec<String> = vec!["mutual".to_string()];
    for (fd, measure) in fns.iter().zip(&measures) {
        let (measure, rank) = (measure.sum(), measure.rank);
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
        // before omega can close the arithmetic on lengths. The
        // `simp only [AverMap.entries, AverMap.fromList]` step unfolds the
        // list-backed-Map identities so a `objectSafe(m)=entriesSafe(Map.entries m)`
        // delegation's goal reduces `sizeOf (AverMap.entries m)` to `sizeOf m`
        // (then the lex rank closes the same-size step); `AverMap.*` is always
        // in scope via the imported prelude, and `try` no-ops where absent.
        lines.push(
            "  decreasing_by all_goals (first | decreasing_tactic | (simp_wf; (try simp only [AverMap.entries, AverMap.fromList]); (try simp_all); first | omega | (constructor <;> first | rfl | omega)))"
                .to_string(),
        );
        lines.push(String::new());
    }
    lines.push("end".to_string());
    Some(lines.join("\n"))
}

pub(super) fn emit_fuelized_mutual_sizeof_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let recursive_types = recursive_type_names(ctx);
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
