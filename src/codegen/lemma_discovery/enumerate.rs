//! Phase 2a-2b — typed-term enumeration and candidate generation.
//!
//! Built the law's variable context and operation vocabulary, then
//! enumerates well-typed terms bottom-up and pairs them into candidate
//! equations. See the crate-level docs in [`super`] for the full pipeline.

use super::*;

/// Run lemma discovery over every `verify ... law` in the entry module: build
/// each law's [`LawProofCone`], a typed variable context, the enumerated
/// terms, and the candidate equations. Pure analysis — no VM, no prover, no
/// file writes.
pub fn run_discovery(inputs: &ProofLowerInputs) -> Vec<LawDiscovery> {
    let mut reports = Vec::new();
    for item in inputs.entry_items {
        let TopLevel::Verify(vb) = item else {
            continue;
        };
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        let cone = LawProofCone::compute(law, &vb.fn_name, inputs);
        let cone_fn_count = cone.pure_fns().len();

        // Gate very large cones out of the naive enumerator (see MAX_CONE_FNS).
        let mut binders = Vec::new();
        let mut conjectures = Vec::new();
        let mut term_count = 0;
        let mut terms_truncated = false;
        let mut conjectures_truncated = false;
        let skipped_large_cone = cone_fn_count > MAX_CONE_FNS;
        if !skipped_large_cone {
            binders = variable_context(&cone);
            let ops = operations(&cone, &binders);
            let (terms, tt) = enumerate_terms(&binders, &ops, MAX_TERM_SIZE);
            let (conj, ct) = conjectures_from_terms(&terms, &binders);
            term_count = terms.len();
            terms_truncated = tt;
            conjectures = conj;
            conjectures_truncated = ct;
        }

        reports.push(LawDiscovery {
            subject_fn: vb.fn_name.clone(),
            law_name: law.name.clone(),
            cone_fns: cone.pure_fns().iter().map(|fd| fd.name.clone()).collect(),
            cone_types: cone
                .types()
                .iter()
                .map(|td| crate::codegen::common::type_def_name(td).to_string())
                .collect(),
            binders,
            proved: Vec::new(),
            stats: DiscoveryStats {
                cone_fn_count,
                term_count,
                conjecture_count: conjectures.len(),
                terms_truncated,
                conjectures_truncated,
                skipped_large_cone,
                vm_filtered: false,
                candidates_refuted: 0,
                max_term_size: MAX_TERM_SIZE,
            },
            conjectures,
        });
    }
    reports
}

/// Mint the shared variable context: up to [`MAX_VARS_PER_TYPE`] variables for
/// each distinct type that appears as a cone fn parameter. Deterministic —
/// types are keyed and ordered by their rendered name, variables are `x0`,
/// `x1`, … in that order.
fn variable_context(cone: &LawProofCone) -> Vec<Binder> {
    let mut param_types: BTreeMap<String, Type> = BTreeMap::new();
    for fd in cone.pure_fns() {
        for (_param_name, annotation) in &fd.params {
            let ty = crate::codegen::common::parse_type_annotation(annotation);
            // Skip unparseable annotations — `Type::Invalid` spuriously
            // unifies with any other `Invalid` under `==` and would mint junk
            // variables. A typecheck-clean cone never hits this.
            if ty == Type::Invalid {
                continue;
            }
            param_types.entry(render_type(&ty)).or_insert(ty);
        }
    }
    let mut binders = Vec::new();
    for ty in param_types.values() {
        for _ in 0..MAX_VARS_PER_TYPE {
            binders.push(Binder {
                name: format!("x{}", binders.len()),
                ty: ty.clone(),
            });
        }
    }
    binders
}

/// The enumeration vocabulary: every cone pure fn as an `Op`, plus a
/// `List.concat` instance for each list element type that occurs anywhere in
/// the variable context or the fns' signatures. `List.concat` is the one
/// builtin `decode_append` needs (`a ++ b`); more builtins join here later.
fn operations(cone: &LawProofCone, binders: &[Binder]) -> Vec<Op> {
    let mut ops = Vec::new();
    for fd in cone.pure_fns() {
        let params: Vec<Type> = fd
            .params
            .iter()
            .map(|(_, ann)| crate::codegen::common::parse_type_annotation(ann))
            .collect();
        let ret = crate::codegen::common::parse_type_annotation(&fd.return_type);
        // Skip fns whose signature didn't parse cleanly — an `Invalid` param
        // or return would unify with any other `Invalid` under `==` and build
        // ill-typed junk terms. A typecheck-clean cone never hits this.
        if ret == Type::Invalid || params.contains(&Type::Invalid) {
            continue;
        }
        ops.push(Op {
            callee: fd.name.clone(),
            params,
            ret,
        });
    }

    // Collect every list element type reachable from the variable types and
    // the fns' parameter/return types, then add a `List.concat` per element.
    let mut elem_types: BTreeMap<String, Type> = BTreeMap::new();
    for b in binders {
        collect_list_elem_types(&b.ty, &mut elem_types);
    }
    for op in &ops {
        for p in &op.params {
            collect_list_elem_types(p, &mut elem_types);
        }
        collect_list_elem_types(&op.ret, &mut elem_types);
    }
    for elem in elem_types.values() {
        let list_ty = Type::List(Box::new(elem.clone()));
        ops.push(Op {
            callee: "List.concat".to_string(),
            params: vec![list_ty.clone(), list_ty.clone()],
            ret: list_ty,
        });
    }
    ops
}

/// Walk a type, recording every `List<elem>` element type by rendered name.
fn collect_list_elem_types(ty: &Type, out: &mut BTreeMap<String, Type>) {
    match ty {
        Type::List(elem) | Type::Vector(elem) => {
            out.entry(render_type(elem))
                .or_insert_with(|| (**elem).clone());
            collect_list_elem_types(elem, out);
        }
        Type::Option(inner) => collect_list_elem_types(inner, out),
        Type::Result(a, b) | Type::Map(a, b) => {
            collect_list_elem_types(a, out);
            collect_list_elem_types(b, out);
        }
        Type::Tuple(items) => {
            for t in items {
                collect_list_elem_types(t, out);
            }
        }
        Type::Fn(args, ret, _) => {
            for a in args {
                collect_list_elem_types(a, out);
            }
            collect_list_elem_types(ret, out);
        }
        _ => {}
    }
}

/// Bottom-up typed enumeration: size-1 terms are the variables; a size-`k`
/// term is an `Op` applied to already-built sub-terms whose sizes sum to
/// `k - 1` and whose types match the op's parameters. Deduplicated by
/// rendering. Returns the terms and whether [`MAX_TERMS`] truncated the run.
fn enumerate_terms(binders: &[Binder], ops: &[Op], max_size: usize) -> (Vec<EnumTerm>, bool) {
    let mut terms: Vec<EnumTerm> = Vec::new();
    let mut by_size: Vec<Vec<usize>> = vec![Vec::new(); max_size + 1];
    let mut seen: HashSet<String> = HashSet::new();
    let mut truncated = false;

    // Size 1: the variables.
    for (i, b) in binders.iter().enumerate() {
        let node = TermNode::Var(i);
        if seen.insert(node.render(binders)) {
            by_size[1].push(terms.len());
            terms.push(EnumTerm {
                node,
                ty: b.ty.clone(),
            });
        }
    }

    'sizes: for size in 2..=max_size {
        for op in ops {
            let arity = op.params.len();
            if arity == 0 || arity > size - 1 {
                continue;
            }
            for comp in compositions(size - 1, arity) {
                // Per-argument candidate pools: terms of the right size whose
                // type matches the op's parameter at that position.
                let mut pools: Vec<Vec<usize>> = Vec::with_capacity(arity);
                let mut any_empty = false;
                for (j, &part) in comp.iter().enumerate() {
                    let pool: Vec<usize> = by_size[part]
                        .iter()
                        .copied()
                        .filter(|&id| terms[id].ty == op.params[j])
                        .collect();
                    if pool.is_empty() {
                        any_empty = true;
                        break;
                    }
                    pools.push(pool);
                }
                if any_empty {
                    continue;
                }
                let (combos, combos_capped) = cartesian(&pools, CARTESIAN_CAP);
                if combos_capped {
                    truncated = true;
                }
                for combo in combos {
                    if terms.len() >= MAX_TERMS {
                        truncated = true;
                        break 'sizes;
                    }
                    let args: Vec<TermNode> =
                        combo.iter().map(|&id| terms[id].node.clone()).collect();
                    let node = TermNode::App {
                        callee: op.callee.clone(),
                        args,
                    };
                    let rendered = node.render(binders);
                    if seen.insert(rendered) {
                        by_size[size].push(terms.len());
                        terms.push(EnumTerm {
                            node,
                            ty: op.ret.clone(),
                        });
                    }
                }
            }
        }
    }

    (terms, truncated)
}

/// Pair terms into candidate equations. Two terms become a candidate iff they
/// have the **same result type** and the **same free-variable SET** (variable
/// multiplicity ignored). For a universally-quantified equational law
/// `∀ vars. L == R` both sides range over the same variables, so this is a
/// sound necessary condition and the QuickSpec-style variable-aware batching
/// that keeps the count tractable.
///
/// What it KEEPS: homomorphism / distributivity (`decode_append`) and also
/// same-set shapes like `f(x, x) == g(x)` ({x} == {x}).
/// What it DROPS, by design for now: pairs whose two sides have DIFFERENT
/// free-var sets — projections / absorptions where one side ignores a
/// variable the other mentions (`head([x, ..xs]) == x`, `xs ++ [] == xs`).
/// The constant-identity ones also need nil/literals, not yet in the
/// vocabulary; a `vars(L) ⊇ vars(R)` relaxation reaches the projection class
/// once constructors/literals land.
/// Out of scope independently of this filter: 3-variable laws (associativity)
/// — only [`MAX_VARS_PER_TYPE`] vars are minted — and cross-module type-name
/// unification (terms are typed by `==` on parsed annotations, which does not
/// collapse `Module.Bare` vs `Bare`; a completeness gap, never unsound).
///
/// Returns the candidates (size-ascending, so the simplest survive a cap) and
/// whether the run was truncated by either the output cap [`MAX_CONJECTURES`]
/// or the work cap [`MAX_PAIRS_EXAMINED`] (which bounds the O(bucket²) pair
/// scan so a large cone can't stall `--discover`).
fn conjectures_from_terms(terms: &[EnumTerm], binders: &[Binder]) -> (Vec<Conjecture>, bool) {
    // Bucket term ids by result type, sorted within each bucket by
    // (size, rendering) so smaller candidates are generated first.
    let mut buckets: BTreeMap<String, Vec<usize>> = BTreeMap::new();
    for (id, t) in terms.iter().enumerate() {
        buckets.entry(render_type(&t.ty)).or_default().push(id);
    }
    for ids in buckets.values_mut() {
        ids.sort_by_key(|&id| (terms[id].node.size(), terms[id].node.render(binders)));
    }

    let mut out = Vec::new();
    let mut seen_pairs: HashSet<(String, String)> = HashSet::new();
    let mut truncated = false;
    let mut pairs_examined = 0usize;

    'buckets: for ids in buckets.values() {
        for a in 0..ids.len() {
            for b in (a + 1)..ids.len() {
                // Work budget: bound the O(bucket²) scan up front, before the
                // free-var / render work, so filtered pairs still count toward
                // termination (the output cap alone can't bound a bucket of
                // mostly different-free-var pairs).
                pairs_examined += 1;
                if pairs_examined >= MAX_PAIRS_EXAMINED {
                    truncated = true;
                    break 'buckets;
                }

                let lt = &terms[ids[a]];
                let rt = &terms[ids[b]];

                let mut lv = BTreeSet::new();
                lt.node.free_vars(&mut lv);
                let mut rv = BTreeSet::new();
                rt.node.free_vars(&mut rv);
                if lv != rv {
                    continue;
                }

                let lr = lt.node.render(binders);
                let rr = rt.node.render(binders);
                if lr == rr {
                    continue;
                }
                let pair = if lr < rr {
                    (lr.clone(), rr.clone())
                } else {
                    (rr.clone(), lr.clone())
                };
                if !seen_pairs.insert(pair) {
                    continue;
                }

                if out.len() >= MAX_CONJECTURES {
                    truncated = true;
                    break 'buckets;
                }
                out.push(Conjecture {
                    lhs: lt.node.clone(),
                    rhs: rt.node.clone(),
                    ty: lt.ty.clone(),
                });
            }
        }
    }

    (out, truncated)
}

/// All length-`parts` compositions of `total` into positive parts (ordered).
fn compositions(total: usize, parts: usize) -> Vec<Vec<usize>> {
    if parts == 0 {
        return if total == 0 { vec![vec![]] } else { vec![] };
    }
    if parts == 1 {
        return if total >= 1 {
            vec![vec![total]]
        } else {
            vec![]
        };
    }
    let mut out = Vec::new();
    for first in 1..=total.saturating_sub(parts - 1) {
        for mut rest in compositions(total - first, parts - 1) {
            let mut v = vec![first];
            v.append(&mut rest);
            out.push(v);
        }
    }
    out
}

/// Cartesian product of the per-argument id pools, capped at `cap` tuples.
/// Returns the tuples and whether the cap truncated the product (so the
/// caller can surface a non-silent partial enumeration).
fn cartesian(pools: &[Vec<usize>], cap: usize) -> (Vec<Vec<usize>>, bool) {
    let mut capped = false;
    let mut acc: Vec<Vec<usize>> = vec![Vec::new()];
    for pool in pools {
        let mut next: Vec<Vec<usize>> = Vec::new();
        'fill: for prefix in &acc {
            for &id in pool {
                let mut v = prefix.clone();
                v.push(id);
                next.push(v);
                if next.len() >= cap {
                    capped = true;
                    break 'fill;
                }
            }
        }
        acc = next;
    }
    (acc, capped)
}

/// Render an Aver [`Type`] back to source-shaped text (`List<Run>`,
/// `Result<T, String>`, `(A, B)`, …) for keying, reporting, and dedup.
pub(super) fn render_type(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(a, b) => format!("Result<{}, {}>", render_type(a), render_type(b)),
        Type::Option(a) => format!("Option<{}>", render_type(a)),
        Type::List(a) => format!("List<{}>", render_type(a)),
        Type::Vector(a) => format!("Vector<{}>", render_type(a)),
        Type::Map(a, b) => format!("Map<{}, {}>", render_type(a), render_type(b)),
        Type::Tuple(items) => format!(
            "({})",
            items.iter().map(render_type).collect::<Vec<_>>().join(", ")
        ),
        Type::Fn(args, ret, _) => format!(
            "({}) -> {}",
            args.iter().map(render_type).collect::<Vec<_>>().join(", "),
            render_type(ret)
        ),
        // display-only: renders the named type into its display string for the
        // discovered-lemma output; `name` IS the surface, no routing decision.
        Type::Named { name, .. } => name.clone(),
        Type::Var(n) => n.clone(),
        Type::Invalid => "<invalid>".to_string(),
    }
}
