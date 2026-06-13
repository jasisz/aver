//! Born-as-Aver discovery — render a VM-survivor `Conjecture` as a legal
//! `verify <fn> law` block and emit only those that clear the HOSTILE-WIDENED
//! forward check (`aver verify --hostile` semantics over a distinguishing
//! domain), so a discovered law flows through the same pipeline as a user law
//! instead of landing in a Lean side-file.
//!
//! This is the `--emit-laws` slice (v1): the `Conjecture` family ONLY (the
//! type-directed enumerator + the homomorphism conjecturer). The
//! structural-bricks / Lean-text path is untouched.
//!
//! # Why a strict re-check before emitting — and why NOT the sampler's domain
//!
//! VM-survival (the 6-round sampler in [`super::vm_filter`]) is necessary but
//! NOT sufficient: the sampler is conservative and can let a false candidate
//! through (e.g. a spurious `concat(flushAcc a, flushAcc b) == concat(flushAcc
//! b, flushAcc a)` "commutativity" the 6 rounds happen not to refute).
//!
//! The load-bearing soundness rule: the emit gate must NOT reuse the sampler's
//! own symmetric per-type domain. When two binders of the same type draw from
//! one small, *degenerate* set (e.g. `RleAcc` values that all `flushAcc` to the
//! empty list, or `Run`s with `count = 0` that `decode` to `""`), `verify`
//! cannot tell a true law from its swapped/commuted false twin — the
//! distinguishing pair is simply absent from the cartesian. That is exactly how
//! the false `concat(flushAcc x, flushAcc y) == concat(flushAcc y, flushAcc x)`
//! and the swapped homomorphism `decode(concat x y) == concat(decode y,
//! decode x)` slipped through when the domain was the sampler's own.
//!
//! So the gate does two things, per the unified hostile-verify policy:
//!
//! 1. It materializes each binder's `given` domain from a DISTINGUISHING
//!    generator ([`distinguishing_sample_values`]) — adversarial, distinct-per-
//!    -seed values (`"a"`/`"b"`, `1`/`2`, `[Run("a",1)]`/`[Run("b",1)]`) that
//!    break commutativity twins, NOT the sampler's degenerate symmetric set.
//! 2. It runs the spliced law through [`run_verify_for_items_vm_with_mode`]
//!    under [`ExpansionMode::Hostile`], which additionally widens every
//!    hostile-reachable typed `given` with the per-type boundary set
//!    (`Int::MIN/MAX/0/±1`, `Str` empty/NUL/multibyte, `List` empty/singleton).
//!
//! A candidate is eligible to emit ONLY if it survives this hostile-widened
//! check with zero counterexamples. Where hostile cannot breach a guard — a law
//! over a `Named` type whose distinguishing generator can produce fewer than two
//! distinct witnesses (so commutativity twins can't be separated) — the
//! candidate is INCONCLUSIVE and is dropped, never emitted as if verified.
//! Kernel credit is a separate, stricter gate (`#print axioms`, in `commands`):
//! a law that clears hostile-verify but does not kernel-prove is labeled
//! honestly ("verified (bounded), kernel proof pending"), never "kernel-proved".
//!
//! # The sidecar
//!
//! Self-contained, parse-legal, runnable `.av`: the original source verbatim
//! (so the cone fns / ADTs / module are all in scope without cross-module
//! qualification) followed by a provenance header and the rendered laws. The
//! user's source file is NEVER mutated — the sidecar is a separate
//! `<file>.discovered.av`. Deterministic: stable law order (discovery order,
//! which is itself stable), stable `given` formatting, byte-reproducible across
//! reruns.

use std::collections::BTreeSet;

use super::*;
use crate::value::Value;

/// Reserved prefix for every emitted law name, so a discovered law can never
/// silently collide with (or cross-wire the `SimpOverLemmas` pool against) a
/// user-written `verify … law <name>`.
pub const DISCOVERED_LAW_PREFIX: &str = "aver_discovered_";

/// One rendered, hostile-check-cleared law ready to write into the sidecar.
/// "Cleared" means it survived [`strict_forward_check`] under hostile expansion
/// (distinguishing + boundary domain) — verified (bounded), NOT kernel-proved.
#[derive(Debug, Clone)]
pub struct EmittedLaw {
    /// The reserved-prefixed, collision-free law name.
    pub name: String,
    /// The subject fn the `verify <fn> law` block anchors to.
    pub subject_fn: String,
    /// The full `verify … law` block text (indented, parse-legal).
    pub block: String,
}

/// Outcome of an emit-laws run over one file's discovery reports: the laws that
/// cleared the hostile-widened forward check, plus the candidates that were
/// dropped and why (so `--discover --emit-laws` can be honest about what it did
/// NOT emit — e.g. the false `flushAcc` commutativity hostile-verify refutes, or
/// an inconclusive Named-guard shape hostile can't breach).
#[derive(Debug, Clone, Default)]
pub struct EmitLawsResult {
    pub emitted: Vec<EmittedLaw>,
    /// `(rendered candidate, reason)` for every survivor that did NOT make it
    /// into the sidecar — refuted by the hostile check, inconclusive,
    /// un-renderable, or a name collision. Kept for the report transcript; never
    /// silently dropped.
    pub dropped: Vec<(String, String)>,
}

/// Render every VM-survivor `Conjecture` across `reports` to a `verify … law`
/// block, run each through the HOSTILE-WIDENED forward check against
/// `source_items`, and return the laws that pass (+ the dropped ones with a
/// reason).
///
/// `existing_law_names` are the user's own `verify … law <name>` names (for
/// dedup); names already chosen in this run are deduped internally.
/// `source_items`/`base_dir`/`source_file` are what the hostile re-check needs
/// to compile the program's cone (mirrors `aver verify --hostile`).
pub fn emit_laws_for_reports(
    reports: &[LawDiscovery],
    inputs: &ProofLowerInputs,
    source_items: &[TopLevel],
    existing_law_names: &BTreeSet<String>,
    base_dir: Option<&str>,
    source_file: &str,
) -> EmitLawsResult {
    let mut result = EmitLawsResult::default();
    let mut used_names = existing_law_names.clone();
    let mut counter = 0usize;

    for report in reports {
        if report.stats.skipped_large_cone {
            continue;
        }
        for c in &report.conjectures {
            let rendered = c.render(&report.binders);
            // Pick a stable, reserved, collision-free name. The candidate's
            // own subject is the anchor when its body mentions it; otherwise
            // any cone fn in the body; otherwise (pure-concat law) any cone fn.
            let name = format!("{DISCOVERED_LAW_PREFIX}lemma_{counter}");
            counter += 1;
            if used_names.contains(&name) {
                result
                    .dropped
                    .push((rendered.clone(), "name collision".to_string()));
                continue;
            }

            let (subject_fn, block) = match render_law_block(c, report, inputs, &name) {
                Ok(pair) => pair,
                Err(reason) => {
                    result.dropped.push((rendered.clone(), reason.to_string()));
                    continue;
                }
            };

            // THE honesty bar: the HOSTILE-WIDENED forward check (aver verify
            // --hostile semantics) over a DISTINGUISHING domain — drops the
            // sampler's symmetric self-selected domain and widens with
            // adversarial, distinct-per-binder, type-boundary values. Splice the
            // rendered law into the source program and run the verify runner;
            // emit only if every case passes (zero counterexamples).
            match strict_forward_check(&block, source_items, base_dir, source_file) {
                Ok(true) => {
                    used_names.insert(name.clone());
                    result.emitted.push(EmittedLaw {
                        name,
                        subject_fn,
                        block,
                    });
                }
                Ok(false) => {
                    result.dropped.push((
                        rendered.clone(),
                        "refuted by hostile-widened forward check (verify --hostile)".to_string(),
                    ));
                }
                Err(reason) => {
                    result.dropped.push((rendered.clone(), reason));
                }
            }
        }
    }

    result
}

/// Render one conjecture to a `(subject_fn, block_text)` pair, or `Err(reason)`
/// when the candidate is dropped — with a PRECISE reason so the report is honest
/// about WHY (un-renderable type vs. inconclusive Named-guard hostile can't
/// breach vs. unsafe literal).
fn render_law_block(
    c: &Conjecture,
    report: &LawDiscovery,
    inputs: &ProofLowerInputs,
    name: &str,
) -> Result<(String, String), &'static str> {
    // Free variables of THIS candidate (binder indices), sorted/stable.
    let mut fvs = BTreeSet::new();
    c.lhs.free_vars(&mut fvs);
    c.rhs.free_vars(&mut fvs);

    let subject_fn = choose_subject(c, report);

    // For each free var, render its `given` domain from the DISTINGUISHING
    // generator — NOT the VM-filter's symmetric sampler. The sampler's domain is
    // the soundness hole (see module docs): two same-typed binders draw from one
    // small degenerate set, so `verify` can't separate a true law from its
    // commuted twin. The distinguishing generator instead emits adversarial,
    // distinct-per-seed values (`"a"`/`"b"`, `1`/`2`, `[Run("a",1)]`/
    // `[Run("b",1)]`) so the cartesian DOES contain the asymmetric pair that
    // refutes a false twin.
    //
    // The bare-`[]` wrinkle (scout Finding A, and its dual on the output side):
    // a bare empty list with no element context infers `List<T>` and either
    // (a) a user fn expecting a concrete element rejects it (`List.concat([],
    // [])` → checker error), or (b) the Lean `_checked_domain` literalizes it as
    // an untyped `[] = []` (`don't know how to synthesize implicit argument α`).
    // Drop the bare `[]` sample from EVERY list-typed binder's domain — the
    // distinguishing non-empty pair carries the refutation, and the kernel proof
    // inducts over nil anyway.
    let mut givens = Vec::new();
    // Track, per type, how many binders of THIS candidate share it: a Named-type
    // binder that shares its type with another binder is at risk of a
    // commutativity twin, so it MUST contribute at least two distinct witnesses
    // or the candidate is inconclusive (hostile can't breach a Named guard).
    let mut type_share: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for &i in &fvs {
        if let Some(b) = report.binders.get(i) {
            // Key on the parse-legal annotation; a non-renderable type drops the
            // whole candidate below, so the key never matters in that case.
            if let Some(t) = render_given_type(&b.ty) {
                *type_share.entry(t).or_insert(0) += 1;
            }
        }
    }
    for &i in &fvs {
        let binder = report
            .binders
            .get(i)
            .ok_or("internal: free var has no binder")?;
        // Drop any candidate whose binder type has no parse-legal `given`
        // annotation (Map/Vector/Fn/…) — never write broken Aver.
        let tystr = render_given_type(&binder.ty)
            .ok_or("binder type has no parse-legal given annotation (Map/Vector/Fn)")?;
        let samples = distinguishing_sample_values(&binder.ty, inputs);
        let drop_empty = matches!(binder.ty, Type::List(_));
        // Distinct, order-stable literals (dedup: the distinguishing generator
        // and the edge cases can coincide on small types).
        let mut seen = BTreeSet::new();
        let mut rendered_vals = Vec::new();
        for v in &samples {
            if !value_is_literal_safe(v) {
                continue;
            }
            let lit = render_value_literal(v);
            if drop_empty && lit == "[]" {
                continue;
            }
            if seen.insert(lit.clone()) {
                rendered_vals.push(lit);
            }
        }
        if rendered_vals.is_empty() {
            return Err("no safe literal witnesses for a binder's domain");
        }
        // Inconclusive guard: a binder of a hostile-unreachable type (`Named`,
        // or a list/option/etc. bottoming out in `Named`) that SHARES its type
        // with another binder, yet offers fewer than two distinct witnesses,
        // cannot have its commutativity twin distinguished. Drop conservatively.
        let hostile_unreachable = !type_hostile_reachable(&binder.ty);
        let shared = type_share.get(&tystr).copied().unwrap_or(1) > 1;
        if hostile_unreachable && shared && rendered_vals.len() < 2 {
            return Err("inconclusive: hostile cannot breach a shared Named-type guard");
        }
        givens.push((binder.name.clone(), tystr, rendered_vals));
    }
    if givens.is_empty() {
        // No free variables — a ground equation. The enumerator never mints
        // these (every term ranges over binders), but guard anyway: a `given`-
        // less law would be a degenerate case the verify runner can't expand.
        return Err("ground equation (no free variables to bind)");
    }

    let lhs = c.lhs.render(&report.binders);
    let rhs = c.rhs.render(&report.binders);

    let mut block = String::new();
    block.push_str(&format!("verify {subject_fn} law {name}\n"));
    for (vname, vty, vals) in &givens {
        block.push_str(&format!(
            "    given {vname}: {vty} = [{}]\n",
            vals.join(", ")
        ));
    }
    block.push_str(&format!("    {lhs} => {rhs}\n"));

    Ok((subject_fn, block))
}

/// Pick the law's nominal subject fn. A `verify <fn> law` needs one subject;
/// the body need not mention it (the law subject is just an anchor). Preference:
/// the report's own subject if the body mentions it, else any user-fn callee in
/// the body, else the report's subject (always a real fn name).
fn choose_subject(c: &Conjecture, report: &LawDiscovery) -> String {
    let mut callees = BTreeSet::new();
    collect_callees(&c.lhs, &mut callees);
    collect_callees(&c.rhs, &mut callees);
    if callees.contains(&report.subject_fn) {
        return report.subject_fn.clone();
    }
    if let Some(first) = callees.iter().find(|f| *f != "List.concat") {
        return first.clone();
    }
    report.subject_fn.clone()
}

/// Collect non-builtin callees referenced in a term.
fn collect_callees(node: &TermNode, out: &mut BTreeSet<String>) {
    if let TermNode::App { callee, args } = node {
        out.insert(callee.clone());
        for a in args {
            collect_callees(a, out);
        }
    }
}

/// Maximum recursion depth for the distinguishing generator (records → fields →
/// …). Matches the VM-filter's `SAMPLE_DEPTH` so recursive ADTs terminate.
const DISTINGUISH_DEPTH: usize = 3;

/// Render a [`Type`] as a PARSE-LEGAL Aver `given` annotation, or `None` if the
/// shape has no legal `given`-domain form (so the candidate is dropped, never
/// written as broken Aver). The crucial divergence from
/// [`super::enumerate::render_type`] (a reporting/dedup renderer): tuples render
/// as `Tuple<A, B>` — the parser REQUIRES the angle-bracket form and REJECTS the
/// paren-tuple annotation `(A, B)`. Types with no literal `given` domain
/// (`Map`/`Vector`/`Fn`/`Var`/`Invalid`) return `None`.
fn render_given_type(ty: &Type) -> Option<String> {
    Some(match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(a, b) => {
            format!(
                "Result<{}, {}>",
                render_given_type(a)?,
                render_given_type(b)?
            )
        }
        Type::Option(a) => format!("Option<{}>", render_given_type(a)?),
        Type::List(a) => format!("List<{}>", render_given_type(a)?),
        // Tuple MUST be `Tuple<...>` (angle form) — the parser rejects `(A, B)`.
        Type::Tuple(items) => {
            let parts: Option<Vec<String>> = items.iter().map(render_given_type).collect();
            format!("Tuple<{}>", parts?.join(", "))
        }
        // display-only: the named type renders to its source `given` annotation
        // by surface name; no routing/identity decision keys off this.
        Type::Named { name, .. } => name.clone(),
        // No literal `given`-domain form: Map/Vector have no source-literal
        // syntax here; Fn/Var/Invalid are not domain-able. Drop the candidate.
        Type::Vector(_) | Type::Map(_, _) | Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => {
            return None;
        }
    })
}

/// `true` iff `aver verify --hostile` can synthesize boundary witnesses for this
/// type. Hostile's `boundary_exprs` covers scalars and compounds bottoming out
/// in scalars; it returns EMPTY for `Map`/`Vector`/`Fn` and — critically — for
/// `Named` (user records / sum types). A law whose binders are all
/// hostile-reachable is fully exercised by the hostile widening; one that
/// bottoms out in a `Named` guard is only as strong as the distinguishing
/// domain we materialize, which is why the inconclusive guard keys off this.
fn type_hostile_reachable(ty: &Type) -> bool {
    match ty {
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit => true,
        Type::List(inner) | Type::Option(inner) | Type::Vector(inner) => {
            type_hostile_reachable(inner)
        }
        Type::Result(a, b) => type_hostile_reachable(a) && type_hostile_reachable(b),
        Type::Tuple(items) => items.iter().all(type_hostile_reachable),
        Type::Named { .. } | Type::Map(_, _) | Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => {
            false
        }
    }
}

/// Type-directed DISTINGUISHING value generator for `given` domains.
///
/// Unlike [`super::vm_filter`]'s `sample_values` (a small, low-magnitude,
/// SYMMETRIC set tuned to avoid bounded-`Int` wrap — and therefore unable to
/// separate a law from its commuted twin), this generator produces, for every
/// type with ≥2 inhabitants, at least two STRUCTURALLY DISTINCT, NON-DEGENERATE
/// witnesses, built from two independent seeds. Two same-typed binders then draw
/// from a domain whose cartesian contains the asymmetric pair that refutes a
/// false commutativity / swapped-homomorphism candidate (e.g. `x = [Run("a",1)]`,
/// `y = [Run("b",1)]` → `decode(x ++ y) = "ab" ≠ "ba" = decode(y ++ x)`).
///
/// Non-degenerate matters: `Run(_, 0)` decodes to `""` and `RleAcc{count:0}`
/// flushes to `[]`, so a domain of count-0 records is symmetric under the codec.
/// The seeds use distinct non-empty letters and counts ≥ 1 so the witnesses do
/// not collapse. Edge values (`""`, `0`, `[]`) are appended too — they round out
/// coverage without re-introducing the symmetry (the distinguishing pair is
/// still present).
fn distinguishing_sample_values(ty: &Type, inputs: &ProofLowerInputs) -> Vec<Value> {
    let mut out = Vec::new();
    // Two distinct seeds → two distinct witnesses for any inhabited type.
    for seed in [Seed::A, Seed::B] {
        out.extend(seeded_value(ty, inputs, seed, DISTINGUISH_DEPTH));
    }
    // Edge values for extra coverage (empty/zero/None) — appended, never
    // replacing the distinguishing pair above.
    out.extend(edge_values(ty, inputs, DISTINGUISH_DEPTH));
    out
}

/// A seed picks distinct, non-degenerate primitive building blocks so two
/// independently-built witnesses of the same type differ structurally.
#[derive(Clone, Copy)]
enum Seed {
    A,
    B,
}

impl Seed {
    fn letter(self) -> &'static str {
        match self {
            Seed::A => "a",
            Seed::B => "b",
        }
    }
    fn count(self) -> i64 {
        match self {
            Seed::A => 1,
            Seed::B => 2,
        }
    }
}

/// One non-degenerate witness of `ty` under `seed`. Recursive ADTs terminate via
/// `depth` (and via the empty-list base for self-referential list fields).
fn seeded_value(ty: &Type, inputs: &ProofLowerInputs, seed: Seed, depth: usize) -> Vec<Value> {
    match ty {
        Type::Int => vec![Value::Int(seed.count())],
        Type::Float => vec![Value::Float(seed.count() as f64)],
        Type::Str => vec![Value::Str(seed.letter().to_string())],
        Type::Bool => vec![Value::Bool(matches!(seed, Seed::A))],
        Type::Unit => vec![Value::Unit],
        Type::List(elem) => seeded_value(elem, inputs, seed, depth)
            .into_iter()
            .next()
            .map(|e| vec![crate::value::list_from_vec(vec![e])])
            .unwrap_or_default(),
        Type::Option(inner) => seeded_value(inner, inputs, seed, depth)
            .into_iter()
            .next()
            .map(|v| vec![Value::Some(Box::new(v))])
            .unwrap_or_else(|| vec![Value::None]),
        Type::Result(ok, err) => match seed {
            Seed::A => seeded_value(ok, inputs, seed, depth)
                .into_iter()
                .next()
                .map(|v| vec![Value::Ok(Box::new(v))])
                .unwrap_or_default(),
            Seed::B => seeded_value(err, inputs, seed, depth)
                .into_iter()
                .next()
                .map(|v| vec![Value::Err(Box::new(v))])
                .unwrap_or_default(),
        },
        Type::Tuple(items) => {
            let parts: Option<Vec<Value>> = items
                .iter()
                .map(|t| seeded_value(t, inputs, seed, depth).into_iter().next())
                .collect();
            parts.map(|p| vec![Value::Tuple(p)]).unwrap_or_default()
        }
        // syntax-discovery-only: walk the named type by source name to look up
        // the user TypeDef and build sample witnesses for the verify domain —
        // discovery-internal sampling over the pure-fn cone, no backend identity.
        Type::Named { name, .. } => {
            if depth == 0 {
                return Vec::new();
            }
            match inputs.find_type_def(name) {
                Some(td) => seeded_named(td, inputs, seed, depth - 1),
                None => Vec::new(),
            }
        }
        _ => Vec::new(),
    }
}

/// Build one non-degenerate witness of a user ADT under `seed`. Product → a
/// record with every field seeded; Sum → the seed-selected variant (A → first,
/// B → second when present, so two seeds pick two distinct variants).
fn seeded_named(
    td: &crate::ast::TypeDef,
    inputs: &ProofLowerInputs,
    seed: Seed,
    depth: usize,
) -> Vec<Value> {
    use std::sync::Arc;
    match td {
        crate::ast::TypeDef::Product { name, fields, .. } => {
            let built: Option<Vec<(String, Value)>> = fields
                .iter()
                .map(|(fname, fty)| {
                    let ty = crate::codegen::common::parse_type_annotation(fty);
                    seeded_value(&ty, inputs, seed, depth)
                        .into_iter()
                        .next()
                        .map(|v| (fname.clone(), v))
                })
                .collect();
            built
                .map(|f| {
                    vec![Value::Record {
                        type_name: name.clone(),
                        fields: Arc::from(f.as_slice()),
                    }]
                })
                .unwrap_or_default()
        }
        crate::ast::TypeDef::Sum { name, variants, .. } => {
            let idx = match seed {
                Seed::A => 0,
                Seed::B => variants.len().min(2).saturating_sub(1),
            };
            let Some(v) = variants.get(idx).or_else(|| variants.first()) else {
                return Vec::new();
            };
            let built: Option<Vec<Value>> = v
                .fields
                .iter()
                .map(|fty| {
                    let ty = crate::codegen::common::parse_type_annotation(fty);
                    seeded_value(&ty, inputs, seed, depth).into_iter().next()
                })
                .collect();
            built
                .map(|fields| {
                    vec![Value::Variant {
                        type_name: name.clone(),
                        variant: v.name.clone(),
                        fields: Arc::from(fields.as_slice()),
                    }]
                })
                .unwrap_or_default()
        }
    }
}

/// Edge witnesses (empty/zero/None) appended after the distinguishing pair —
/// extra coverage that never re-introduces symmetry.
fn edge_values(ty: &Type, inputs: &ProofLowerInputs, depth: usize) -> Vec<Value> {
    match ty {
        Type::Int => vec![Value::Int(0)],
        Type::Str => vec![Value::Str(String::new())],
        Type::List(_) => vec![crate::value::list_from_vec(Vec::new())],
        Type::Option(_) => vec![Value::None],
        // syntax-discovery-only: look up the user TypeDef by source name to build
        // the degenerate edge witness; discovery-internal, no backend identity.
        // For Named/compound the seeded pair is already non-degenerate; a third
        // edge witness would need a third seed and risks the bare-`[]` inference
        // wrinkle on nested lists. Keep edges to the scalar/list/option leaves.
        Type::Named { name, .. } if depth > 0 => match inputs.find_type_def(name) {
            // A "zero" record (all-scalar fields zeroed) adds the degenerate
            // corner WITHOUT removing the distinguishing pair — useful so the
            // domain also exercises the codec's empty-producing edge.
            Some(td) => edge_named(td, inputs, depth - 1),
            None => Vec::new(),
        },
        _ => Vec::new(),
    }
}

/// One all-zero/empty edge witness of a user ADT (scalar fields zeroed, string
/// fields empty), so the domain also covers the degenerate corner.
fn edge_named(td: &crate::ast::TypeDef, inputs: &ProofLowerInputs, depth: usize) -> Vec<Value> {
    use std::sync::Arc;
    if let crate::ast::TypeDef::Product { name, fields, .. } = td {
        let built: Option<Vec<(String, Value)>> = fields
            .iter()
            .map(|(fname, fty)| {
                let ty = crate::codegen::common::parse_type_annotation(fty);
                zero_value(&ty, inputs, depth).map(|v| (fname.clone(), v))
            })
            .collect();
        if let Some(f) = built {
            return vec![Value::Record {
                type_name: name.clone(),
                fields: Arc::from(f.as_slice()),
            }];
        }
    }
    Vec::new()
}

/// The degenerate/zero witness of a type (for the edge record's fields).
fn zero_value(ty: &Type, inputs: &ProofLowerInputs, depth: usize) -> Option<Value> {
    match ty {
        Type::Int => Some(Value::Int(0)),
        Type::Float => Some(Value::Float(0.0)),
        Type::Str => Some(Value::Str(String::new())),
        Type::Bool => Some(Value::Bool(false)),
        Type::Unit => Some(Value::Unit),
        Type::List(_) => Some(crate::value::list_from_vec(Vec::new())),
        Type::Option(_) => Some(Value::None),
        // syntax-discovery-only: look up the user TypeDef by source name to zero
        // a nested record field; discovery-internal sampling, no backend identity.
        Type::Named { name, .. } if depth > 0 => match inputs.find_type_def(name) {
            Some(td) => edge_named(td, inputs, depth - 1).into_iter().next(),
            None => None,
        },
        _ => None,
    }
}

/// Render a [`Value`] to source-legal Aver literal text. Differs from
/// `aver_repr_literal` in ONE load-bearing way: SUM-type variants are qualified
/// with their type name (`Nat.S(Nat.Z)`, not the bare `S(Z)` `aver_repr`
/// produces) — the parser only accepts qualified variant constructors in
/// `given` domains. Records (`Run(...)`) already parse bare, so they pass
/// through unchanged.
fn render_value_literal(v: &Value) -> String {
    match v {
        Value::Str(s) => format!("\"{s}\""),
        Value::Int(i) => i.to_string(),
        Value::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Value::Unit => "Unit".to_string(),
        Value::Ok(inner) => format!("Result.Ok({})", render_value_literal(inner)),
        Value::Err(inner) => format!("Result.Err({})", render_value_literal(inner)),
        Value::Some(inner) => format!("Option.Some({})", render_value_literal(inner)),
        Value::None => "Option.None".to_string(),
        Value::List(items) => {
            let parts: Vec<String> = items.iter().map(render_value_literal).collect();
            format!("[{}]", parts.join(", "))
        }
        Value::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(render_value_literal).collect();
            format!("({})", parts.join(", "))
        }
        Value::Record { type_name, fields } => {
            let parts: Vec<String> = fields
                .iter()
                .map(|(k, x)| format!("{k} = {}", render_value_literal(x)))
                .collect();
            format!("{type_name}({})", parts.join(", "))
        }
        Value::Variant {
            type_name,
            variant,
            fields,
        } => {
            // Qualify the constructor: `Nat.S(...)`. `type_name` is the source
            // ADT name (the sampler built it from `inputs.find_type_def`).
            if fields.is_empty() {
                format!("{type_name}.{variant}")
            } else {
                let parts: Vec<String> = fields.iter().map(render_value_literal).collect();
                format!("{type_name}.{variant}({})", parts.join(", "))
            }
        }
        // Float / Map / Vector / Fn / Builtin / Namespace are filtered out by
        // `value_is_literal_safe` before reaching here; render defensively.
        other => crate::value::aver_repr_literal(other),
    }
}

/// `true` iff every embedded value renders to a literal that round-trips
/// through the lexer+parser verbatim and back to the same value:
/// - strings carry no `"`/`\`/`{`/`}`/control chars (no escape-rendering),
/// - no `Float` (decimal repr doesn't bit-round-trip),
/// - no `Map` (repr spelling isn't the parser's `{k => v}`),
/// - no `Vector`/`Fn`/`Builtin`/`Namespace` (no source literal form here).
fn value_is_literal_safe(v: &Value) -> bool {
    match v {
        Value::Str(s) => s
            .chars()
            .all(|c| !matches!(c, '"' | '\\' | '{' | '}') && !c.is_control()),
        Value::Int(_) | Value::Bool(_) | Value::Unit | Value::None => true,
        Value::Ok(inner) | Value::Err(inner) | Value::Some(inner) => value_is_literal_safe(inner),
        Value::List(items) => items.iter().all(value_is_literal_safe),
        Value::Tuple(items) => items.iter().all(value_is_literal_safe),
        Value::Variant { fields, .. } => fields.iter().all(value_is_literal_safe),
        Value::Record { fields, .. } => fields.iter().all(|(_, x)| value_is_literal_safe(x)),
        // Float / Map / Vector / Fn / Builtin / Namespace are not safely
        // literalizable in a `given` domain.
        _ => false,
    }
}

/// Splice `block` (a rendered `verify … law`) into a copy of the source program
/// and run the HOSTILE-WIDENED forward check (the `aver verify --hostile` VM
/// runner: distinguishing declared domain PLUS per-type boundary widening).
/// Returns `Ok(true)` iff the spliced law parses, typechecks, and EVERY case —
/// declared AND hostile-injected — passes; `Ok(false)` iff it typechecks but a
/// case is refuted; `Err(reason)` iff it fails to parse/typecheck/compile
/// (treated as a drop, not a pass). The name stays `strict_forward_check` (its
/// callers' honesty bar), but the bar is now hostile, not the sampler's domain.
fn strict_forward_check(
    block: &str,
    source_items: &[TopLevel],
    base_dir: Option<&str>,
    source_file: &str,
) -> Result<bool, String> {
    // Parse the rendered block on its own (it is module-free `verify` text), and
    // append its `TopLevel::Verify` items to a fresh copy of the source program.
    let parsed = parse_verify_block(block)?;
    let mut items: Vec<TopLevel> = source_items.to_vec();
    items.extend(parsed);

    // Run under HOSTILE expansion: the declared (distinguishing) domain PLUS the
    // per-type boundary set (`Int::MIN/MAX/0/±1`, `Str` empty/NUL/multibyte,
    // `List` empty/singleton). This is the unified hostile-verify policy — the
    // gate that separates a true law from its commuted twin. The distinguishing
    // domain handles the `Named` guards hostile can't breach; hostile handles the
    // scalar/list-scalar boundaries.
    let results = crate::diagnostics::vm_verify::run_verify_for_items_vm_with_mode(
        items,
        None,
        base_dir,
        source_file,
        crate::verify_law::expand::ExpansionMode::Hostile,
    )
    .map_err(|e| format!("strict check: {e}"))?;

    // Find the verify result for OUR law (the last law block we appended). The
    // runner returns one VerifyResult per block; ours is keyed by `is_law` and
    // the reserved-prefix name embedded in `block_label`.
    let ours = results
        .iter()
        .rev()
        .find(|r| r.is_law && r.block_label.contains(DISCOVERED_LAW_PREFIX));
    match ours {
        Some(r) => Ok(r.failed == 0 && r.passed > 0),
        // The block didn't surface as a verified law (e.g. the typechecker
        // rejected it and the runner returned nothing for it) — drop it.
        None => Err("strict check: law produced no verify result".to_string()),
    }
}

/// Parse a standalone `verify … law` block (no `module` header) into its
/// `TopLevel` items.
fn parse_verify_block(block: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = crate::lexer::Lexer::new(block);
    let tokens = lexer
        .tokenize()
        .map_err(|e| format!("render lex error: {e}"))?;
    crate::parser::Parser::new(tokens)
        .parse()
        .map_err(|e| format!("render parse error: {e}"))
}

/// Build the full sidecar `.av` content: the original source verbatim, a
/// provenance header, and every emitted law block. Deterministic and
/// byte-reproducible. The source is spliced in full so the sidecar is
/// self-contained (cone fns + ADTs + module all in scope) without cross-module
/// qualification — and the user's source file itself is never touched.
pub fn render_sidecar(
    source_text: &str,
    laws: &[EmittedLaw],
    source_file: &str,
    sidecar_path: &str,
    tool_version: &str,
) -> String {
    let mut out = String::new();
    out.push_str(source_text);
    if !source_text.ends_with('\n') {
        out.push('\n');
    }
    out.push('\n');
    // The next-step command references the ACTUAL written path (which may be a
    // `--emit-laws-to` override), not a hardcoded `<source>.discovered.av`.
    out.push_str(&format!(
        "// ===========================================================================\n\
         // Machine-proposed laws: aver proof {source_file} --discover --emit-laws\n\
         // Tool: aver {tool_version}.\n\
         // Each law below was conjectured by the discovery enumerator, survived the\n\
         // VM sampler, AND passed the HOSTILE-WIDENED forward check (aver verify\n\
         // --hostile semantics: distinguishing, distinct-per-binder, type-boundary\n\
         // values — stricter than the sampler's symmetric domain). That is the bar\n\
         // these cleared: verified (bounded), kernel proof PENDING. They are NOT yet\n\
         // kernel-proved. To kernel-prove them (gated on #print axioms — a law that\n\
         // rests on sorryAx earns no credit), run\n\
         //   aver proof {sidecar_path} --check\n\
         // (add --gate <baseline> to ratchet). Names are prefixed {DISCOVERED_LAW_PREFIX}\n\
         // and de-duplicated against the source's own law names. The source above is a\n\
         // verbatim copy — the original file is never mutated; review and merge\n\
         // deliberately.\n\
         // ===========================================================================\n\n",
    ));
    for law in laws {
        out.push_str(&law.block);
        out.push('\n');
    }
    out
}
