//! Phase 2c — VM-filter (Aver's VM as the test oracle).
//!
//! Runs both sides of each candidate on the Aver VM over sample variable
//! assignments and drops counterexamples. See [`super`] for the pipeline.

use super::*;

// ===========================================================================
// Phase 2c — VM-filter (Aver's VM as the test oracle).
//
// Each enumerated candidate (2b) is a *guess*. The VM-filter instantiates the
// equation's variables with concrete sample values and runs BOTH sides on the
// Aver VM — if any sample makes the sides disagree, the candidate is a
// counterexample and is dropped. Survivors are not theorems (the charter's
// proved-or-dropped gate still demands a kernel proof, 2d), but a single false
// candidate that the VM refutes never reaches the (expensive) prover.
//
// Conservative by construction (charter's semantic-model-mismatch caution): a
// VM error, an unmodeled builtin, or an out-of-guard `Int` magnitude makes a
// sample INCONCLUSIVE — it never refutes. So a backend-true lemma is never
// wrongly dropped because the bounded-`Int` VM diverged from the unbounded
// proof model; the worst case is a false candidate slipping to the prover,
// which then rejects it.
// ===========================================================================

/// Variable instantiations tried per candidate before declaring it
/// counterexample-free on samples.
const VM_FILTER_ROUNDS: usize = 6;
/// Opcode cap per `run_named_function` so a pathological term can't hang.
const VM_STEP_LIMIT: u64 = 1_000_000;
/// `Int` magnitudes at/above this in a result make the sample inconclusive —
/// near i64 range the bounded VM wraps and diverges from the proof model.
const VM_INT_MAGNITUDE_GUARD: u64 = 1 << 40;
/// Recursion bound for the sample-value generator (records → fields → …).
const SAMPLE_DEPTH: usize = 3;

/// Refute or keep every candidate of every (enumerated) law by running both
/// sides on the Aver VM over sample variable assignments. Compiles the
/// program's pure cone ONCE and reuses it across all candidates. On compile
/// failure it leaves every candidate in place (conservative — discovery stays
/// a superset, the prover is the real gate).
pub fn vm_filter(reports: &mut [LawDiscovery], inputs: &ProofLowerInputs) {
    let Some(mut vm) = compile_oracle_vm(inputs) else {
        return;
    };
    for report in reports.iter_mut() {
        if report.stats.skipped_large_cone || report.conjectures.is_empty() {
            continue;
        }
        let samples: Vec<Vec<Value>> = report
            .binders
            .iter()
            .map(|b| sample_values(&b.ty, inputs, SAMPLE_DEPTH))
            .collect();
        let mut survivors = Vec::with_capacity(report.conjectures.len());
        let mut refuted = 0usize;
        for c in &report.conjectures {
            if vm_refutes(c, &samples, &mut vm) {
                refuted += 1;
            } else {
                survivors.push(c.clone());
            }
        }
        report.stats.vm_filtered = true;
        report.stats.candidates_refuted = refuted;
        report.conjectures = survivors;
    }
}

/// `true` iff some sample assignment makes the two sides evaluate to DIFFERENT
/// values (both conclusive). Inconclusive samples (eval error / out-of-guard
/// magnitude) are skipped, never counted as a refutation.
fn vm_refutes(c: &Conjecture, samples: &[Vec<Value>], vm: &mut crate::vm::VM) -> bool {
    for r in 0..VM_FILTER_ROUNDS {
        // Offset by binder index so two same-typed variables usually differ in
        // a round (needed to refute e.g. spurious commutativity and to give
        // distributivity a non-degenerate test).
        let assignment: Vec<Option<Value>> = samples
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if s.is_empty() {
                    None
                } else {
                    Some(s[(r + i) % s.len()].clone())
                }
            })
            .collect();
        let (Some(l), Some(rhs)) = (
            eval_term(&c.lhs, &assignment, vm),
            eval_term(&c.rhs, &assignment, vm),
        ) else {
            continue;
        };
        if !value_within_int_guard(&l) || !value_within_int_guard(&rhs) {
            continue;
        }
        if l != rhs {
            return true;
        }
    }
    false
}

/// Evaluate a term under a variable assignment on the VM. `None` = inconclusive
/// (unassigned variable, eval error, or an unmodeled builtin callee).
fn eval_term(
    node: &TermNode,
    assignment: &[Option<Value>],
    vm: &mut crate::vm::VM,
) -> Option<Value> {
    match node {
        TermNode::Var(i) => assignment.get(*i).cloned().flatten(),
        TermNode::App { callee, args } => {
            let argvals: Option<Vec<Value>> =
                args.iter().map(|a| eval_term(a, assignment, vm)).collect();
            let argvals = argvals?;
            // `List.concat` is the only builtin in the current vocabulary;
            // evaluate it directly on `Value` (no VM call). Other builtins are
            // left unmodeled → inconclusive.
            if callee == "List.concat" {
                if argvals.len() != 2 {
                    return None;
                }
                return crate::value::list_concat(&argvals[0], &argvals[1]);
            }
            // User cone fn: call it on the VM with the constructed arguments.
            let nanargs: Vec<NanValue> = argvals
                .iter()
                .map(|v| NanValue::from_value(v, &mut vm.arena))
                .collect();
            let result = vm.run_named_function(callee, &nanargs).ok()?;
            Some(result.to_value(&vm.arena))
        }
    }
}

/// Compile the program's pure functions into a runnable VM (the oracle).
/// Mirrors the `aver run` / `vm_verify` setup; `None` on any compile failure.
fn compile_oracle_vm(inputs: &ProofLowerInputs) -> Option<crate::vm::VM> {
    let resolved = crate::ir::hir::resolve_program(inputs.symbol_table, inputs.entry_items);
    let mut arena = crate::nan_value::Arena::new();
    let (code, globals) = crate::vm::compile_program_with_mir_fallback(
        &resolved,
        inputs.symbol_table,
        &mut arena,
        None,
    )
    .ok()?;
    let mut vm = crate::vm::VM::new(code, globals, arena);
    vm.set_step_limit(Some(VM_STEP_LIMIT));
    vm.run_top_level().ok()?;
    Some(vm)
}

/// `true` iff no `Int` anywhere in the value is near i64 range — beyond the
/// guard the bounded VM wraps, so the sample can't be trusted against the
/// unbounded proof model.
fn value_within_int_guard(v: &Value) -> bool {
    match v {
        Value::Int(i) => i.unsigned_abs() < VM_INT_MAGNITUDE_GUARD,
        Value::Ok(b) | Value::Err(b) | Value::Some(b) => value_within_int_guard(b),
        Value::Tuple(xs) => xs.iter().all(value_within_int_guard),
        Value::Record { fields, .. } => fields.iter().all(|(_, x)| value_within_int_guard(x)),
        Value::Variant { fields, .. } => fields.iter().all(value_within_int_guard),
        Value::List(_) | Value::Vector(_) => crate::value::list_to_vec(v)
            .map(|xs| xs.iter().all(value_within_int_guard))
            .unwrap_or(true),
        _ => true,
    }
}

/// Type-directed sample-value generator: a few concrete Aver [`Value`]s of a
/// type, for the VM-filter to instantiate equation variables. Kept tiny and
/// low-magnitude (`Int` in `0..2`) so bounded-`Int` wrap can't manufacture a
/// false agreement. Records / variants are built from `inputs.find_type_def`;
/// `depth` bounds recursion (recursive ADTs terminate via the empty-list base
/// or by exhausting variants that reference the type).
fn sample_values(ty: &Type, inputs: &ProofLowerInputs, depth: usize) -> Vec<Value> {
    match ty {
        Type::Int => vec![Value::Int(0), Value::Int(1), Value::Int(2)],
        Type::Float => vec![Value::Float(0.0), Value::Float(1.0)],
        Type::Str => vec![
            Value::Str(String::new()),
            Value::Str("a".to_string()),
            Value::Str("b".to_string()),
        ],
        Type::Bool => vec![Value::Bool(true), Value::Bool(false)],
        Type::Unit => vec![Value::Unit],
        Type::List(elem) => {
            let es = sample_values(elem, inputs, depth);
            let mut out = vec![crate::value::list_from_vec(Vec::new())];
            if let Some(e0) = es.first() {
                out.push(crate::value::list_from_vec(vec![e0.clone()]));
            }
            if es.len() >= 2 {
                out.push(crate::value::list_from_vec(vec![
                    es[0].clone(),
                    es[1].clone(),
                ]));
            }
            out
        }
        Type::Option(inner) => {
            let mut out = vec![Value::None];
            if let Some(v) = sample_values(inner, inputs, depth).into_iter().next() {
                out.push(Value::Some(Box::new(v)));
            }
            out
        }
        Type::Result(ok, err) => {
            let mut out = Vec::new();
            if let Some(v) = sample_values(ok, inputs, depth).into_iter().next() {
                out.push(Value::Ok(Box::new(v)));
            }
            if let Some(v) = sample_values(err, inputs, depth).into_iter().next() {
                out.push(Value::Err(Box::new(v)));
            }
            out
        }
        Type::Tuple(items) => {
            let parts: Option<Vec<Value>> = items
                .iter()
                .map(|t| sample_values(t, inputs, depth).into_iter().next())
                .collect();
            parts.map(|p| vec![Value::Tuple(p)]).unwrap_or_default()
        }
        // syntax-discovery-only: the discovery VM-sampler walks proof-cone types
        // by source name to look up the user TypeDef for value generation —
        // discovery-internal sampling over the pure-fn cone, never backend-routed
        // identity (no codegen output keys off this).
        Type::Named { name, .. } => {
            if depth == 0 {
                return Vec::new();
            }
            match inputs.find_type_def(name) {
                Some(td) => named_sample(td, inputs, depth - 1),
                None => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}

/// Build sample values for a user ADT (`Product` → records, `Sum` → variants),
/// recursing into field types at the (already-decremented) `depth`.
fn named_sample(td: &crate::ast::TypeDef, inputs: &ProofLowerInputs, depth: usize) -> Vec<Value> {
    use std::sync::Arc;
    let field_sample = |fty: &str, pick_last: bool| -> Option<Value> {
        let ty = crate::codegen::common::parse_type_annotation(fty);
        let s = sample_values(&ty, inputs, depth);
        if pick_last {
            s.into_iter().last()
        } else {
            s.into_iter().next()
        }
    };
    match td {
        crate::ast::TypeDef::Product { name, fields, .. } => {
            let build = |pick_last: bool| -> Option<Value> {
                let built: Option<Vec<(String, Value)>> = fields
                    .iter()
                    .map(|(fname, fty)| field_sample(fty, pick_last).map(|v| (fname.clone(), v)))
                    .collect();
                built.map(|f| Value::Record {
                    type_name: name.clone(),
                    fields: Arc::from(f.as_slice()),
                })
            };
            let mut out = Vec::new();
            if let Some(first) = build(false) {
                out.push(first);
            }
            if let Some(second) = build(true)
                && !out.contains(&second)
            {
                out.push(second);
            }
            out
        }
        crate::ast::TypeDef::Sum { name, variants, .. } => {
            let mut out = Vec::new();
            for v in variants {
                let built: Option<Vec<Value>> = v
                    .fields
                    .iter()
                    .map(|fty| field_sample(fty, false))
                    .collect();
                if let Some(fields) = built {
                    out.push(Value::Variant {
                        type_name: name.clone(),
                        variant: v.name.clone(),
                        fields: Arc::from(fields.as_slice()),
                    });
                }
                if out.len() >= 2 {
                    break;
                }
            }
            out
        }
    }
}
