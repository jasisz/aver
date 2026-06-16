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
/// Bound on greedy-shrink sweeps over a refuting assignment. Each sweep is a
/// full pass over every binder's shrink candidates; we stop early at the first
/// sweep that makes no progress, so this only caps a pathological chain. A
/// minimal counterexample over the tiny sample domain needs very few sweeps.
const SHRINK_MAX_SWEEPS: usize = 64;

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
        let mut witnesses = Vec::new();
        for c in &report.conjectures {
            if let Some(w) = vm_refutes(c, &report.binders, &samples, &mut vm) {
                witnesses.push(w);
            } else {
                survivors.push(c.clone());
            }
        }
        report.stats.vm_filtered = true;
        report.stats.candidates_refuted = witnesses.len();
        report.stats.refuted_witnesses = witnesses;
        report.conjectures = survivors;
    }
}

/// `Some(witness)` iff some sample assignment makes the two sides evaluate to
/// DIFFERENT values (both conclusive) — the witness carries the concrete
/// counterexample (per-binder values + both sides) built from data already in
/// hand, no extra evaluation. Inconclusive samples (eval error / out-of-guard
/// magnitude) are skipped, never counted as a refutation (`None`).
///
/// Before surfacing, the refuting assignment is greedily SHRUNK (smaller lists,
/// `Int`s toward 0, simpler fields) so the reported witness is the smallest one
/// we can reach that STILL refutes — see [`shrink_assignment`]. Shrinking is
/// pure presentation: it never changes the verdict (a refutation stays a
/// refutation), only the values shown.
fn vm_refutes(
    c: &Conjecture,
    binders: &[Binder],
    samples: &[Vec<Value>],
    vm: &mut crate::vm::VM,
) -> Option<RefutedWitness> {
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
        if refutes_at(c, &assignment, vm).is_some() {
            // Counterexample found. Greedily minimize it while it still refutes,
            // then surface the (re-verified) shrunk assignment. `refutes_at` is
            // the single source of truth for "refutes": the witness's `l`/`rhs`
            // are recomputed from the *final* assignment, so what we show is
            // exactly what we last verified — never a stale larger pair.
            let assignment = shrink_assignment(c, assignment, vm);
            let (l, rhs) = refutes_at(c, &assignment, vm)
                .expect("shrink_assignment preserves refutation by construction");
            // Restrict the givens to the candidate's own free variables (the
            // ones that actually bind its sides), in binder index order
            // (stable/sorted).
            let mut fvs = std::collections::BTreeSet::new();
            c.lhs.free_vars(&mut fvs);
            c.rhs.free_vars(&mut fvs);
            let givens = fvs
                .iter()
                .filter_map(|&i| {
                    assignment
                        .get(i)
                        .and_then(|v| v.as_ref())
                        .map(|v| (binders[i].name.clone(), crate::value::aver_repr(v)))
                })
                .collect();
            return Some(RefutedWitness {
                candidate: c.render(binders),
                givens,
                lhs: crate::value::aver_repr(&l),
                rhs: crate::value::aver_repr(&rhs),
            });
        }
    }
    None
}

/// The refutation test, factored out so both the initial search and every
/// shrink step gate on the EXACT same predicate. `Some((lhs, rhs))` iff this
/// assignment conclusively refutes `c`: both sides evaluate (no eval error /
/// unmodeled builtin), both stay within the `Int` magnitude guard (out-of-guard
/// is inconclusive, never a refutation), and the two sides differ. `None`
/// otherwise — inconclusive or agreeing. Returning the values lets the caller
/// reuse them without re-evaluating.
fn refutes_at(
    c: &Conjecture,
    assignment: &[Option<Value>],
    vm: &mut crate::vm::VM,
) -> Option<(Value, Value)> {
    let l = eval_term(&c.lhs, assignment, vm)?;
    let rhs = eval_term(&c.rhs, assignment, vm)?;
    if !value_within_int_guard(&l) || !value_within_int_guard(&rhs) {
        return None;
    }
    (l != rhs).then_some((l, rhs))
}

/// Greedily minimize an already-refuting assignment while it STILL refutes.
///
/// QuickCheck/QuickChick-style shrinking: each sweep proposes a few smaller
/// candidates per binder (lists toward `[]`, `Int`s toward 0, records/variants
/// with shrunk fields — see [`shrink_value`]), RE-EVALUATES the lemma via
/// [`refutes_at`], and keeps a candidate only if it still refutes. Sweeps
/// repeat to a fixpoint (a sweep that accepts nothing stops the loop), bounded
/// by [`SHRINK_MAX_SWEEPS`] so it always terminates.
///
/// Soundness: this is pure addition over an already-found counterexample. A
/// shrink step is committed ONLY when `refutes_at` returns `Some` on the
/// candidate, so the returned assignment is guaranteed to still refute — it can
/// never turn a refuted lemma into "not refuted" nor report a non-refuting
/// witness. In the worst case (no shrink re-verifies) the original assignment is
/// returned unchanged.
fn shrink_assignment(
    c: &Conjecture,
    mut assignment: Vec<Option<Value>>,
    vm: &mut crate::vm::VM,
) -> Vec<Option<Value>> {
    // Only the candidate's own free variables can change the verdict; leave the
    // rest untouched (they're filtered out of the witness anyway).
    let mut fvs = std::collections::BTreeSet::new();
    c.lhs.free_vars(&mut fvs);
    c.rhs.free_vars(&mut fvs);
    let fvs: Vec<usize> = fvs.into_iter().collect();
    for _ in 0..SHRINK_MAX_SWEEPS {
        let mut progressed = false;
        for &i in &fvs {
            // Skip binders with no value at this index (out of range or
            // unassigned): nothing to shrink.
            let Some(Some(current)) = assignment.get(i) else {
                continue;
            };
            // Try this binder's shrink candidates smallest-first; accept the
            // first that keeps the lemma refuted, then move on (greedy).
            for cand in shrink_value(current) {
                let prev = assignment[i].replace(cand);
                if refutes_at(c, &assignment, vm).is_some() {
                    progressed = true;
                    break;
                }
                // Rejected: it no longer refutes (or went inconclusive) — undo.
                assignment[i] = prev;
            }
        }
        if !progressed {
            break;
        }
    }
    assignment
}

/// Smaller-than-`v` candidates for a single binder, in roughly smallest-first
/// order. Each is a *proposal*: the caller re-verifies it still refutes and
/// discards it otherwise, so these need only be plausible shrinks, not
/// guaranteed-valid ones. Returns an empty `Vec` for values with no smaller
/// form (already-minimal scalars), which ends shrinking for that binder.
fn shrink_value(v: &Value) -> Vec<Value> {
    match v {
        // Lists: drop toward `[]`, then simplify what's left. Try the empty
        // list, then a halved prefix/suffix (fast-shrink long lists), then each
        // single-element deletion (so any one offending element can be removed),
        // and finally shrink each surviving element in place (so e.g. `[34]`
        // collapses to `[0]`). Length-reducing moves come first. (The sample
        // generator only ever produces `List` values, never `Vector`, so a
        // `Vector` here has no smaller form via `list_to_vec` and is left as-is.)
        Value::List(_) | Value::Vector(_) => {
            let Some(xs) = crate::value::list_to_vec(v) else {
                return Vec::new();
            };
            if xs.is_empty() {
                return Vec::new();
            }
            let mut out = vec![crate::value::list_from_vec(Vec::new())];
            if xs.len() > 1 {
                let half = xs.len() / 2;
                out.push(crate::value::list_from_vec(xs[..half].to_vec()));
                out.push(crate::value::list_from_vec(xs[half..].to_vec()));
            }
            for drop_at in 0..xs.len() {
                let mut shorter = xs.clone();
                shorter.remove(drop_at);
                out.push(crate::value::list_from_vec(shorter));
            }
            out.extend(shrink_one_field(&xs, crate::value::list_from_vec));
            out
        }
        // `Int`: move toward 0 — try 0, then negate (toward a positive), then
        // halve the magnitude. The re-verify gate drops any that stop refuting.
        Value::Int(n) => {
            let mut out = Vec::new();
            if !n.is_zero() {
                out.push(Value::int(0));
            }
            if let Some(i) = n.to_i64() {
                if i < 0 {
                    out.push(Value::Int(n.neg()));
                }
                let half = i / 2;
                if half != i {
                    out.push(Value::int(half));
                }
            }
            out
        }
        // Strings: shrink toward "" the same way as lists (empty, then drop a
        // char). Kept simple — char-level shrinking is enough for readability.
        Value::Str(s) if !s.is_empty() => {
            let mut out = vec![Value::Str(String::new())];
            for drop_at in 0..s.chars().count() {
                let shorter: String = s
                    .chars()
                    .enumerate()
                    .filter_map(|(j, ch)| (j != drop_at).then_some(ch))
                    .collect();
                out.push(Value::Str(shorter));
            }
            out
        }
        // Optionals / wrappers: a `Some`/`Ok`/`Err` shrinks to its shrunk
        // payload (re-wrapped); `Some` also shrinks to `None` (the base case).
        Value::Some(inner) => {
            let mut out = vec![Value::None];
            out.extend(
                shrink_value(inner)
                    .into_iter()
                    .map(|s| Value::Some(Box::new(s))),
            );
            out
        }
        Value::Ok(inner) => shrink_value(inner)
            .into_iter()
            .map(|s| Value::Ok(Box::new(s)))
            .collect(),
        Value::Err(inner) => shrink_value(inner)
            .into_iter()
            .map(|s| Value::Err(Box::new(s)))
            .collect(),
        // Tuples / records / variants: shrink one field at a time, leaving the
        // others fixed (so the structure is preserved while values get simpler).
        Value::Tuple(items) => shrink_one_field(items, Value::Tuple),
        Value::Record { type_name, fields } => {
            let (names, vals): (Vec<String>, Vec<Value>) =
                fields.iter().map(|(n, x)| (n.clone(), x.clone())).unzip();
            let type_name = type_name.clone();
            shrink_one_field(&vals, move |xs| Value::Record {
                type_name: type_name.clone(),
                fields: std::sync::Arc::from(
                    names.iter().cloned().zip(xs).collect::<Vec<_>>().as_slice(),
                ),
            })
        }
        Value::Variant {
            type_name,
            variant,
            fields,
        } => {
            let type_name = type_name.clone();
            let variant = variant.clone();
            shrink_one_field(fields, move |xs| Value::Variant {
                type_name: type_name.clone(),
                variant: variant.clone(),
                fields: std::sync::Arc::from(xs.as_slice()),
            })
        }
        // Already minimal (Bool, Unit, Float, None, …): no smaller form.
        _ => Vec::new(),
    }
}

/// Shrink a fixed-arity field list (tuple / record / variant) by shrinking each
/// field in turn while holding the others fixed, rebuilding the container via
/// `rebuild`. Each emitted candidate differs from the input in exactly one
/// field, so the structural shape (arity, type, variant) is preserved.
fn shrink_one_field(fields: &[Value], rebuild: impl Fn(Vec<Value>) -> Value) -> Vec<Value> {
    let mut out = Vec::new();
    for (i, f) in fields.iter().enumerate() {
        for smaller in shrink_value(f) {
            let mut next = fields.to_vec();
            next[i] = smaller;
            out.push(rebuild(next));
        }
    }
    out
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
        Value::Int(i) => i
            .to_i64()
            .is_some_and(|n| n.unsigned_abs() < VM_INT_MAGNITUDE_GUARD),
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
        Type::Int => vec![Value::int(0), Value::int(1), Value::int(2)],
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A bare VM with no compiled functions — enough to drive `eval_term` /
    /// `refutes_at` for conjectures whose only callee is the directly-modeled
    /// `List.concat` builtin (which never touches the VM's function table).
    fn bare_vm() -> crate::vm::VM {
        crate::vm::VM::new(
            crate::vm::CodeStore::new(),
            Vec::new(),
            crate::nan_value::Arena::new(),
        )
    }

    /// `List.concat(x, x) == x` over a single list binder `x`. False for any
    /// non-empty `x` (the doubled list is strictly longer) and true for `[]`
    /// (both sides empty) — so the shortest refuting `x` is a 1-element list.
    fn self_concat_conjecture() -> Conjecture {
        let x = TermNode::Var(0);
        Conjecture {
            lhs: TermNode::App {
                callee: "List.concat".to_string(),
                args: vec![x.clone(), x.clone()],
            },
            rhs: x,
            ty: Type::List(Box::new(Type::Int)),
        }
    }

    /// `shrink_value` on a list offers `[]` first and a single-element-deletion
    /// candidate for every position, and on a positive `Int` offers 0 — the raw
    /// per-type shrink moves the greedy pass composes.
    #[test]
    fn shrink_value_offers_smaller_candidates() {
        let list = crate::value::list_from_vec(vec![Value::int(7), Value::int(8), Value::int(9)]);
        let cands = shrink_value(&list);
        // Empty list is the smallest move and comes first.
        assert_eq!(
            cands.first(),
            Some(&crate::value::list_from_vec(Vec::new())),
            "list shrink must offer [] first"
        );
        // Every single-element deletion is offered (so any one offender drops).
        for drop_at in 0..3 {
            let mut expect = vec![Value::int(7), Value::int(8), Value::int(9)];
            expect.remove(drop_at);
            assert!(
                cands.contains(&crate::value::list_from_vec(expect)),
                "list shrink must offer dropping element {drop_at}"
            );
        }
        // A positive Int shrinks toward 0 (0 offered as the smallest move).
        assert!(
            shrink_value(&Value::int(42)).contains(&Value::int(0)),
            "Int shrink must offer 0"
        );
        // Already-minimal scalars have no smaller form.
        assert!(shrink_value(&Value::int(0)).is_empty());
        assert!(shrink_value(&Value::Bool(true)).is_empty());
    }

    /// A deliberately-large refuting assignment shrinks to the minimal witness
    /// that STILL refutes: the list collapses to a single element AND that
    /// element's `Int` collapses to 0 — i.e. `[0]`, the shortest/simplest list
    /// for which `List.concat(x, x) != x`. The shrunk assignment is re-verified
    /// to genuinely refute (it never silently "un-refutes").
    #[test]
    fn shrink_assignment_minimizes_a_large_counterexample() {
        let c = self_concat_conjecture();
        let mut vm = bare_vm();
        // Big, noisy starting counterexample: a 5-element list of large Ints.
        let big = crate::value::list_from_vec(vec![
            Value::int(5),
            Value::int(9),
            Value::int(13),
            Value::int(21),
            Value::int(34),
        ]);
        let start = vec![Some(big.clone())];

        // Sanity: the starting assignment really does refute (precondition for
        // shrinking — we only ever shrink an already-found counterexample).
        assert!(
            refutes_at(&c, &start, &mut vm).is_some(),
            "the large starting assignment must refute"
        );

        let shrunk = shrink_assignment(&c, start, &mut vm);

        // The shrunk witness still refutes — the load-bearing soundness property
        // (a shrink step is only ever kept when it re-verifies as a refutation).
        let (l, rhs) =
            refutes_at(&c, &shrunk, &mut vm).expect("the shrunk assignment must still refute");
        assert_ne!(l, rhs, "a refutation has lhs != rhs");

        // And it shrank all the way to the minimal `[0]`: one element, value 0.
        let shrunk_x = shrunk[0].clone().expect("binder 0 stays assigned");
        assert_eq!(
            shrunk_x,
            crate::value::list_from_vec(vec![Value::int(0)]),
            "the 5-element large-Int list must shrink to the minimal [0]"
        );
    }

    /// Shrinking is conservative: when the starting assignment does NOT refute,
    /// `shrink_assignment` leaves it untouched (it never manufactures a smaller
    /// "counterexample" out of an agreeing assignment).
    #[test]
    fn shrink_assignment_leaves_a_non_refuting_assignment_unchanged() {
        let c = self_concat_conjecture();
        let mut vm = bare_vm();
        // `x = []` makes both sides `[]` — agreement, not a refutation.
        let empty = vec![Some(crate::value::list_from_vec(Vec::new()))];
        assert!(
            refutes_at(&c, &empty, &mut vm).is_none(),
            "the empty-list assignment must NOT refute (both sides empty)"
        );
        let out = shrink_assignment(&c, empty.clone(), &mut vm);
        assert_eq!(
            out, empty,
            "a non-refuting assignment is returned unchanged"
        );
    }
}
