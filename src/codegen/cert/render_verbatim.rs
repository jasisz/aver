/// A `Cod := WVal` / `verbatimRepr` match certificate: the shared theorem
/// wrapper (statement, `cases v`, obligation) with the per-shape holes filled by
/// a plan. The widened match (one projected hit + a default) and the variant
/// dispatch (a constant per tag) are two ends of ONE arm -- same face, same
/// `cases v` spine -- differing only in the per-arm leaf.
struct VerbatimWvalPlan {
    doc_title: String,
    doc_comment: String,
    cases_body: String,
    guards_comment: String,
    guards: String,
}

fn render_verbatim_wval_match_cert(c: &Cert) -> String {
    let name = c.name();
    let self_idx = c.self_idx();
    let plan = match c.inner() {
        Cert::VerbatimWidenedMatch { .. } => verbatim_widened_plan(c),
        Cert::VerbatimVariantDispatch { .. } => verbatim_variant_dispatch_plan(c),
        _ => unreachable!(),
    };
    let VerbatimWvalPlan {
        doc_title,
        doc_comment,
        cases_body,
        guards_comment,
        guards,
    } = plan;
    format!(
        r#"{doc_title}

{doc_comment}
theorem {name}_wasm_certified :
    ∀ (fuel : Nat) (v w : WVal),
      wFuncN {name}Code {name}Host (fuel + 1) {self_idx} [v] = some w →
      w = {name}Model v := by
  intro fuel v w hrun
  cases v with
{cases_body}

#print axioms {name}_wasm_certified

{guards_comment}
def {name}HostRef : HostTbl := {name}Host
{guards}
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel v vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      have hc := {name}_wasm_certified f v w hrun
      simpa [AverCert.Schema.verbatimRepr] using hc
"#
    )
}

fn verbatim_widened_plan(c: &Cert) -> VerbatimWvalPlan {
    let Cert::VerbatimWidenedMatch {
        name,
        self_idx,
        carrier,
        hit_variant_idx,
        default,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let hit = hit_variant_idx;
    let other = if *hit_variant_idx == 0 { 1 } else { 0 };
    let default_guard = render_default_guard(default);
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, {name}Model, b32, popArgs, initLocals, List.set"
    );
    VerbatimWvalPlan {
        doc_title: format!(
            "/-! ### {name} — verbatim widened match certificate (carrier type {carrier}) -/"
        ),
        doc_comment: format!(
            "/-- The VERBATIM emitted body reads the first field of variant `{hit}` and\n    \
             returns it as-is, or the null reference for any other value, for ALL inputs\n    \
             `v : WVal` (partial correctness — a trap makes no claim). -/"
        ),
        cases_body: format!(
            "  | i32v n => simp [{evalset}] at hrun\n  \
             | i64v n => simp [{evalset}] at hrun\n  \
             | f64v b => simp [{evalset}] at hrun\n  \
             | null => simp_all [{evalset}]\n  \
             | arr t es =>\n      \
             by_cases ht : t = {hit}\n      \
             · subst ht; simp [{evalset}] at hrun\n      \
             · simp_all [{evalset}]\n  \
             | structv t fs =>\n      \
             by_cases ht : t = {hit}\n      \
             · subst ht\n        \
             cases fs with\n        \
             | nil => simp [{evalset}] at hrun\n        \
             | cons x rest => simp_all [{evalset}]\n      \
             · simp_all [{evalset}]"
        ),
        guards_comment:
            "-- Executable tripwires: the projected variant returns its first field, every\n\
             -- other value returns the byte-derived default literal."
                .to_string(),
        guards: format!(
            "example :\n    \
             (wFuncN {name}Code {name}HostRef 4 {self_idx} [.structv {hit} [.i64v 42]]).bind\n      \
             (fun w => match w with | .i64v n => some n | _ => none) = some 42 := by native_decide\n\
             example :\n    \
             (wFuncN {name}Code {name}HostRef 4 {self_idx} [.structv {other} []]).bind {default_guard} := by\n  \
             native_decide\n"
        ),
    }
}

fn verbatim_variant_dispatch_plan(c: &Cert) -> VerbatimWvalPlan {
    let Cert::VerbatimVariantDispatch {
        name,
        self_idx,
        carrier,
        arms,
        default,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, {name}Model, b32, popArgs, initLocals, List.set"
    );
    let dispatch = verbatim_tag_dispatch(arms, 6, &evalset);
    let default_tag = arms.iter().map(|(t, _)| *t).max().unwrap_or(0) + 1;
    let mut guards = String::new();
    for (tag, konst) in arms {
        guards.push_str(&format!(
            "example :\n    (wFuncN {name}Code {name}HostRef 8 {self_idx} [.structv {tag} []]).bind {} := by\n  native_decide\n",
            render_default_guard(konst),
        ));
    }
    guards.push_str(&format!(
        "example :\n    (wFuncN {name}Code {name}HostRef 8 {self_idx} [.structv {default_tag} []]).bind {} := by\n  native_decide\n",
        render_default_guard(default),
    ));
    VerbatimWvalPlan {
        doc_title: format!(
            "/-! ### {name} — verbatim variant dispatch certificate (carrier type {carrier}) -/"
        ),
        doc_comment:
            "/-- The VERBATIM emitted body dispatches on the variant and returns a byte-derived\n    \
             constant for each tag, for ALL inputs `v : WVal` (partial correctness — a trap\n    \
             makes no claim). -/"
                .to_string(),
        cases_body: format!(
            "  | i32v n => simp_all [{evalset}]\n  \
             | i64v n => simp_all [{evalset}]\n  \
             | f64v b => simp_all [{evalset}]\n  \
             | null => simp_all [{evalset}]\n  \
             | arr t es =>\n{dispatch}\n  \
             | structv t fs =>\n{dispatch}"
        ),
        guards_comment: "-- Executable tripwires: each variant returns its byte-derived constant."
            .to_string(),
        guards,
    }
}

/// The nested `by_cases` block dispatching on a struct/array type index `t`: one
/// `by_cases hI : t = tagI` per arm (each closing `subst; simp_all` to its
/// constant), terminating in `simp_all` for the default. `base` is the leading
/// indent of the first `by_cases`. Mirrors HueSpike.lean's proven shape.
fn verbatim_tag_dispatch(arms: &[(u32, VerbatimDefault)], base: usize, evalset: &str) -> String {
    let mut out = String::new();
    for (i, (tag, _)) in arms.iter().enumerate() {
        let ind = base + i * 2;
        let sp = " ".repeat(ind);
        if i == 0 {
            out.push_str(&sp);
        }
        out.push_str(&format!("by_cases h{i} : t = {tag}\n"));
        out.push_str(&format!("{sp}· subst h{i}\n"));
        out.push_str(&format!("{sp}  simp_all [{evalset}]\n"));
        out.push_str(&format!("{sp}· "));
    }
    out.push_str(&format!("simp_all [{evalset}]"));
    out
}
