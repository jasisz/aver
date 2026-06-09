//! Phase 2d — render a candidate to a Lean theorem, rank candidates,
//! and produce the human-readable discovery report.
//!
//! See [`super`] for the full pipeline.

use super::*;

// ===========================================================================
// Phase 2d — render a candidate to a Lean theorem for kernel proof.
//
// A VM-survivor is still a guess; the proved-or-dropped gate demands a kernel
// proof. These pure helpers render a candidate to standalone Lean theorem text
// using the SAME name/type mapping the program defs are emitted with (so the
// theorem references `decode` / `Run` / `++` exactly as generated). The actual
// `lake build` (needs a CodegenContext + prover process) lives in the CLI.
// ===========================================================================

/// Render a candidate as a standalone Lean theorem with the list-induction
/// template, IF it has a list-typed free variable to induct on (else `None` —
/// the template doesn't apply). Proof:
/// `induction <v> with | nil => simp [fns] | cons .. ih => simp [fns, ih, List.append_assoc]`,
/// which closes the list-homomorphism class (`decode_append` and kin).
pub fn lean_lemma_theorem(c: &Conjecture, binders: &[Binder], name: &str) -> Option<String> {
    let mut fvs = BTreeSet::new();
    c.lhs.free_vars(&mut fvs);
    c.rhs.free_vars(&mut fvs);
    // Induction target: the first list-typed free variable.
    let induct = fvs
        .iter()
        .copied()
        .find(|&i| matches!(binders.get(i).map(|b| &b.ty), Some(Type::List(_))))?;

    // Lean binder declarations for the free variables, in index order.
    let binder_decls: Vec<String> = fvs
        .iter()
        .filter_map(|&i| binders.get(i))
        .map(|b| {
            format!(
                "({} : {})",
                b.name,
                crate::codegen::lean::type_to_lean(&b.ty)
            )
        })
        .collect();

    // User fns referenced (the simp unfold list); `List.concat` is `++`, not a
    // simp lemma, so it's excluded.
    let mut fns = BTreeSet::new();
    collect_user_fns(&c.lhs, &mut fns);
    collect_user_fns(&c.rhs, &mut fns);
    let unfolds: Vec<String> = fns
        .iter()
        .map(|f| crate::codegen::lean::aver_name_to_lean(f))
        .collect();

    let lhs = term_to_lean(&c.lhs, binders);
    let rhs = term_to_lean(&c.rhs, binders);
    let iv = &binders[induct].name;
    // Session-grade list-induction ladder (mirrors `law_auto::induction::
    // emit_list_induction`), so a discovered candidate gets the SAME reach the
    // user-stated-law prover has: `omega` for the linear-arithmetic residual a
    // count/length homomorphism leaves (`1 + (m+n) = (1+m)+n`), and a `split`
    // branch that case-splits a recursive fn's inner Bool/enum `match` (e.g.
    // `count`'s `match eqNat n head`) which plain `simp` leaves stuck.
    //
    // CRUCIAL difference from the session prover: NO trailing `| sorry`.
    // Discovery is proved-or-dropped — a `sorry` builds clean (exit 0) and would
    // FALSELY mark a refuted candidate "proved". Omitting it means a candidate
    // that no branch closes leaves an `unsolved goals` ERROR, `lake build`
    // fails, and the candidate is correctly dropped. `omega`/`split`/`simp` are
    // all sound, so a candidate that DOES close is genuinely proved.
    let nil_defs = unfolds.join(", ");
    let cons_defs = {
        let mut v = unfolds.clone();
        v.push("List.append_assoc".to_string());
        v.join(", ")
    };
    // `simp only [..]` set for the split branch — guard the empty-`unfolds` case
    // so we never emit a leading-comma `simp only [, …]` (a parse error).
    let nil_split = if unfolds.is_empty() {
        "List.cons_append".to_string()
    } else {
        format!("{nil_defs}, List.cons_append")
    };
    let cons_split = format!("{cons_defs}, List.cons_append");
    Some(format!(
        "theorem {name} {binders} : {lhs} = {rhs} := by\n  \
         induction {iv} with\n  \
         | nil => first | (simp [{nil_defs}]; done) | (simp [{nil_defs}]; omega) | (simp only [{nil_split}]; split <;> simp_all [{nil_defs}] <;> omega)\n  \
         | cons head tail ih => first | (simp_all [{cons_defs}]; done) | (simp_all [{cons_defs}]; omega) | (simp only [{cons_split}]; split <;> simp_all [{cons_defs}] <;> omega)\n",
        binders = binder_decls.join(" "),
    ))
}

/// Render a term to a Lean expression: `Var` → binder name, `List.concat` →
/// `(a ++ b)`, any other callee → `(f arg …)` via [`aver_name_to_lean`].
fn term_to_lean(node: &TermNode, binders: &[Binder]) -> String {
    match node {
        TermNode::Var(i) => binders
            .get(*i)
            .map(|b| b.name.clone())
            .unwrap_or_else(|| format!("x{i}")),
        TermNode::App { callee, args } => {
            if callee == "List.concat" && args.len() == 2 {
                return format!(
                    "({} ++ {})",
                    term_to_lean(&args[0], binders),
                    term_to_lean(&args[1], binders)
                );
            }
            let lean_fn = crate::codegen::lean::aver_name_to_lean(callee);
            let rendered: Vec<String> = args.iter().map(|a| term_to_lean(a, binders)).collect();
            format!("({} {})", lean_fn, rendered.join(" "))
        }
    }
}

/// Collect user-fn callees in a term (everything but the `List.concat` builtin).
fn collect_user_fns(node: &TermNode, out: &mut BTreeSet<String>) {
    if let TermNode::App { callee, args } = node {
        if callee != "List.concat" {
            out.insert(callee.clone());
        }
        for a in args {
            collect_user_fns(a, out);
        }
    }
}

/// `true` iff the candidate is the list-homomorphism shape
/// `g(a ++ b) == g(a) ++ g(b)` (either orientation), `g` a user fn, `a`/`b`
/// distinct variables. These are the highest-value proof targets, ranked first.
fn is_homomorphism(c: &Conjecture) -> bool {
    fn oriented(l: &TermNode, r: &TermNode) -> bool {
        let TermNode::App {
            callee: g,
            args: la,
        } = l
        else {
            return false;
        };
        if g == "List.concat" || la.len() != 1 {
            return false;
        }
        let TermNode::App {
            callee: cc,
            args: ca,
        } = &la[0]
        else {
            return false;
        };
        if cc != "List.concat" || ca.len() != 2 {
            return false;
        }
        let (TermNode::Var(a), TermNode::Var(b)) = (&ca[0], &ca[1]) else {
            return false;
        };
        if a == b {
            return false;
        }
        let TermNode::App {
            callee: rc,
            args: ra,
        } = r
        else {
            return false;
        };
        if rc != "List.concat" || ra.len() != 2 {
            return false;
        }
        let (
            TermNode::App {
                callee: g1,
                args: r1,
            },
            TermNode::App {
                callee: g2,
                args: r2,
            },
        ) = (&ra[0], &ra[1])
        else {
            return false;
        };
        g1 == g
            && g2 == g
            && r1.len() == 1
            && r2.len() == 1
            && matches!((&r1[0], &r2[0]), (TermNode::Var(a2), TermNode::Var(b2)) if a2 == a && b2 == b)
    }
    oriented(&c.lhs, &c.rhs) || oriented(&c.rhs, &c.lhs)
}

/// A stable content hash of the discovery surface (every pure fn's signature),
/// used as the committed-lemma cache key: a matching hash means the committed
/// lemmas can be REPLAYED (re-verified) instead of rediscovered. FNV-1a over
/// the sorted canonical signatures — stable across runs/platforms (unlike
/// `DefaultHasher`). This is only a performance key (skip rediscovery); the
/// soundness guard is re-verification every build, never the hash (charter).
pub fn discovery_surface_hash(inputs: &ProofLowerInputs) -> String {
    let mut sigs: Vec<String> = inputs
        .pure_fns()
        .iter()
        .map(|fd| {
            let params: Vec<String> = fd.params.iter().map(|(n, t)| format!("{n}:{t}")).collect();
            format!("{}({})->{}", fd.name, params.join(","), fd.return_type)
        })
        .collect();
    sigs.sort();
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in sigs.join(";").bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("{hash:016x}")
}

/// Candidate indices in proof-attempt order: list-homomorphism shapes first
/// (the highest-value, template-closable class), then smallest-first. The
/// prover (CLI) walks this and attempts the top few within a build budget.
pub fn rank_candidate_indices(report: &LawDiscovery) -> Vec<usize> {
    let mut idx: Vec<usize> = (0..report.conjectures.len()).collect();
    idx.sort_by_key(|&i| {
        let c = &report.conjectures[i];
        let homo = if is_homomorphism(c) { 0 } else { 1 };
        (homo, c.lhs.size() + c.rhs.size(), c.render(&report.binders))
    });
    idx
}

/// Human-readable multi-line report for `aver proof --discover` output. Shows
/// each law's cone, variable legend, stats, a sample of the candidate
/// equations (after the VM-filter, survivors), and any kernel-proved lemmas.
pub fn render_report(reports: &[LawDiscovery]) -> String {
    const SAMPLE: usize = 12;
    let mut out = String::new();
    if reports.is_empty() {
        out.push_str("lemma discovery: no `verify ... law` blocks found\n");
        return out;
    }
    out.push_str(&format!(
        "lemma discovery (skeleton): {} law(s)\n",
        reports.len()
    ));
    for r in reports {
        out.push_str(&format!("\n• verify {} law {}\n", r.subject_fn, r.law_name));
        out.push_str(&format!("    cone fns:   [{}]\n", r.cone_fns.join(", ")));
        out.push_str(&format!("    cone types: [{}]\n", r.cone_types.join(", ")));
        if r.stats.skipped_large_cone {
            out.push_str(&format!(
                "    cone too large ({} fns > {}) — enumeration skipped (size-{} discovery not meaningful at this scope)\n",
                r.stats.cone_fn_count, MAX_CONE_FNS, r.stats.max_term_size
            ));
            continue;
        }
        let legend: Vec<String> = r
            .binders
            .iter()
            .map(|b| format!("{}: {}", b.name, render_type(&b.ty)))
            .collect();
        out.push_str(&format!("    variables:  [{}]\n", legend.join(", ")));
        out.push_str(&format!(
            "    enumerated {} terms (size ≤ {}{}), {} candidate equations{}\n",
            r.stats.term_count,
            r.stats.max_term_size,
            if r.stats.terms_truncated {
                ", TRUNCATED"
            } else {
                ""
            },
            r.stats.conjecture_count,
            if r.stats.conjectures_truncated {
                " (TRUNCATED)"
            } else {
                ""
            },
        ));
        if r.stats.vm_filtered {
            out.push_str(&format!(
                "    VM-filter: {} survived, {} refuted on sample data\n",
                r.conjectures.len(),
                r.stats.candidates_refuted
            ));
        }
        let shown = r.conjectures.len().min(SAMPLE);
        let label = if r.stats.vm_filtered {
            "survivors"
        } else {
            "candidates"
        };
        out.push_str(&format!("    {label} (showing {shown}):\n"));
        for c in r.conjectures.iter().take(SAMPLE) {
            out.push_str(&format!("      {}\n", c.render(&r.binders)));
        }
        if r.conjectures.len() > SAMPLE {
            out.push_str(&format!(
                "      … and {} more\n",
                r.conjectures.len() - SAMPLE
            ));
        }
        if !r.proved.is_empty() {
            out.push_str(&format!(
                "    PROVED (Lean, kernel-checked): {}\n",
                r.proved.len()
            ));
            for p in &r.proved {
                out.push_str(&format!("      ✓ {p}\n"));
            }
        }
    }
    out
}
