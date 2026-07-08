fn render_user_repr_defs(analysis: &Analysis, model_info: &ModelInfo) -> String {
    let mut out = String::new();
    let mut emitted = std::collections::HashSet::new();
    for c in &analysis.certs {
        let Some((ty, indices)) = adt_repr_indices(c, model_info) else {
            continue;
        };
        if !emitted.insert(ty.clone()) {
            continue;
        }
        let Some(ind) = model_info.inductives.get(&ty) else {
            continue;
        };
        out.push_str(&format!(
            "def {ty}Repr (S : CarrierSpec {}) : {ty} → WVal → Prop\n",
            c.carrier()
        ));
        for (i, ctor) in ind.ctors.iter().enumerate() {
            let idx = indices.get(i).copied().unwrap_or(i as u32);
            if ctor.fields.is_empty() {
                out.push_str(&format!("  | .{}, v => v = .structv {idx} []\n", ctor.name));
            } else if ctor.fields.len() == 1 && ctor.fields[0] == "Int" {
                out.push_str(&format!(
                    "  | .{} x, v => ∃ cx, v = .structv {idx} [cx] ∧ S.Repr x cx\n",
                    ctor.name
                ));
            } else {
                out.push_str(&format!("  | .{}, _ => False\n", ctor.name));
            }
        }
        out.push('\n');
    }
    // Per-function domain-representation relations for widened Int matches. Each
    // is keyed on the projected variant's byte-derived struct index: the hit
    // constructor is represented as that struct carrying a single Int carrier,
    // every other constructor as any struct of a DIFFERENT type — enough to make
    // the projection theorem provable and non-vacuous. This is a read
    // declaration (the ADT face is not kernel-re-derived), so its exact shape is
    // untrusted; the checker pins only `Cod = Int`, `codRepr = intRepr` and
    // `Nonempty Dom`.
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::WidenedIntMatch {
            hit_variant_idx, ..
        } = c
        else {
            continue;
        };
        let Some((ty, ind, hit_ctor)) = widened_match_info(c, model_info) else {
            continue;
        };
        out.push_str(&format!(
            "def {name}DomRepr (S : CarrierSpec {carrier}) : {ty} → WVal → Prop\n",
            name = c.name(),
            carrier = c.carrier(),
        ));
        for ctor in &ind.ctors {
            let binders = " _".repeat(ctor.fields.len());
            if ctor.name == hit_ctor {
                out.push_str(&format!(
                    "  | .{ctor} x, v => ∃ cx, v = .structv {hit_variant_idx} [cx] ∧ S.Repr x cx\n",
                    ctor = ctor.name,
                ));
            } else {
                out.push_str(&format!(
                    "  | .{ctor}{binders}, v => ∃ t fs, v = .structv t fs ∧ t ≠ {hit_variant_idx}\n",
                    ctor = ctor.name,
                ));
            }
        }
        out.push('\n');
    }
    // Shared model definitions for verbatim widened matches: a single named
    // function referenced by both the obligation `model` and the certificate
    // proof, so the two match on the SAME compiled term (an inline match would
    // elaborate to two distinct, non-defeq auxiliaries).
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::VerbatimWidenedMatch {
            hit_variant_idx, ..
        } = c
        else {
            continue;
        };
        out.push_str(&format!(
            "def {name}Model : CertPrelude.WVal → CertPrelude.WVal\n  \
             | .structv {hit_variant_idx} (x :: _) => x\n  \
             | _ => {default}\n\n",
            name = c.name(),
            default = match c {
                Cert::VerbatimWidenedMatch { default, .. } => render_wval(default),
                _ => unreachable!(),
            },
        ));
    }
    // Model for a verbatim variant dispatch: each tag maps to its byte-derived
    // constant. The body's `ref.test i` matches the WVal type index for BOTH
    // `.structv i` and `.arr i`, and constant arms never fail on `.arr` (unlike a
    // field projection), so the model must dispatch on the type index for both
    // carriers; anything else falls to the terminal default.
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::VerbatimVariantDispatch { arms, default, .. } = c else {
            continue;
        };
        let mut body = String::new();
        for (tag, konst) in arms {
            body.push_str(&format!("  | .structv {tag} _ => {}\n", render_wval(konst)));
        }
        for (tag, konst) in arms {
            body.push_str(&format!("  | .arr {tag} _ => {}\n", render_wval(konst)));
        }
        out.push_str(&format!(
            "def {name}Model : CertPrelude.WVal → CertPrelude.WVal\n{body}  | _ => {default}\n\n",
            name = c.name(),
            default = render_wval(default),
        ));
    }
    // Model for String-literal dispatch: the String.eq helper is a host
    // contract, so the model uses the audited prelude's byte-array equality
    // predicate over raw `WVal` arrays and returns byte-derived constants.
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::StringEqVerbatimMatch { arms, default, .. } = c else {
            continue;
        };
        let mut body = String::new();
        for (needle, hit) in arms {
            body.push_str(&format!(
                "  if stringEqW v {} then {}\n  else",
                render_wval_arg(needle),
                render_wval(hit)
            ));
        }
        body.push_str(&format!(" {}\n", render_string_eq_default(default, "v")));
        out.push_str(&format!(
            "def {name}Model (v : CertPrelude.WVal) : CertPrelude.WVal :=\n{body}\n",
            name = c.name(),
        ));
    }
    // Model for String.concat verbatim match: use the audited prelude's
    // contract face on the exact container the emitted wasm body builds.
    for c in &analysis.certs {
        let Cert::StringConcatVerbatimMatch {
            name,
            container_ty,
            result_ty,
            prefixes,
            suffixes,
            ..
        } = c.inner()
        else {
            continue;
        };
        let prefix_parts: Vec<String> = prefixes.iter().map(render_wval_qualified).collect();
        let suffix_parts: Vec<String> = suffixes.iter().map(render_wval_qualified).collect();
        let mut container_parts = String::new();
        for p in &prefix_parts {
            container_parts.push_str(&format!("{p}, "));
        }
        container_parts.push_str("v");
        for s in &suffix_parts {
            container_parts.push_str(&format!(", {s}"));
        }
        let body =
            format!("  stringConcatW {result_ty} (WVal.arr {container_ty} [{container_parts}])\n");
        out.push_str(&format!(
            "def {name}Model (v : CertPrelude.WVal) : CertPrelude.WVal :=\n{body}\n",
        ));
    }
    out
}

/// For a widened Int match: the model inductive name, its constructor list, and
/// the name of the single integer-payload constructor the body projects (the
/// unique `fields == ["Int"]` constructor). `None` — so the class declines by a
/// failed render — if the model type is unknown or the projected constructor is
/// not unique.
fn widened_match_info<'a>(
    c: &Cert,
    model_info: &'a ModelInfo,
) -> Option<(String, &'a InductiveInfo, String)> {
    let c = c.inner();
    let Cert::WidenedIntMatch { name, .. } = c else {
        return None;
    };
    let ty = model_info.fns.get(name)?.params.first()?.clone();
    let ind = model_info.inductives.get(&ty)?;
    let mut int_ctors = ind.ctors.iter().filter(|ct| ct.fields == ["Int"]);
    let hit = int_ctors.next()?.name.clone();
    if int_ctors.next().is_some() {
        return None;
    }
    Some((ty, ind, hit))
}

/// Whether an ADT constructor certificate can name its real model type: a
/// single-field constructor whose codomain is a user inductive (so
/// `render_user_repr_defs` emits a `<Ty>Repr` and the model is `<Ty>.<ctor>`).
/// Anything else — a multi-field constructor, or a constructor over a builtin
/// compound codomain like `List (String × Json)` that has no user Repr — is
/// certified as a verbatim pack instead (the dual of a field projection), which
/// makes no claim about a recursive representation (deferred, see the model
/// stop-loss on recursive-type Repr).
fn adt_constructor_uses_model(c: &Cert, model_info: &ModelInfo) -> bool {
    let c = c.inner();
    let Cert::AdtConstructor {
        name,
        field_count,
        arity,
        fields,
        ..
    } = c
    else {
        return false;
    };
    *field_count == 1
        && *arity == 1
        && fields.as_slice() == [ConstructorField::Local(0)]
        && model_info
            .fns
            .get(name)
            .map(|s| model_info.inductives.contains_key(&s.ret))
            .unwrap_or(false)
}

/// `(Dom type, `vs`-shape, struct-field list)` for a verbatim pack constructor
/// of the given field count. The domain is the raw argument `WVal`s (a single
/// value or a pair), and the model packs them into the variant struct verbatim.
fn verbatim_ctor_shape(
    arity: usize,
    fields: &[ConstructorField],
) -> (&'static str, String, String) {
    let args = fields
        .iter()
        .map(|field| match field {
            ConstructorField::Local(0) if arity == 1 => "p".to_string(),
            ConstructorField::Local(0) => "p.1".to_string(),
            ConstructorField::Local(1) => "p.2".to_string(),
            ConstructorField::Local(i) => format!("p.{i}"),
            ConstructorField::Null => ".null".to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    if arity == 1 {
        ("WVal", "[p]".to_string(), format!("[{args}]"))
    } else {
        ("WVal × WVal", "[p.1, p.2]".to_string(), format!("[{args}]"))
    }
}

fn adt_repr_indices(c: &Cert, model_info: &ModelInfo) -> Option<(String, Vec<u32>)> {
    match c.inner() {
        Cert::VariantDispatch { name, arms, .. } => {
            let ty = model_info.fns.get(name)?.params.first()?.clone();
            let ind = model_info.inductives.get(&ty)?;
            // Struct tags are assigned per constructor in declaration order;
            // anchor the base on the smallest dispatched tag. A mis-anchored
            // base renders an unprovable `Repr` and fails the lake build —
            // never a false certificate.
            let base = arms.iter().map(|(t, _)| *t).min()?;
            Some((ty, (0..ind.ctors.len()).map(|i| base + i as u32).collect()))
        }
        Cert::AdtConstructor {
            name, struct_idx, ..
        } => {
            let ty = model_info.fns.get(name)?.ret.clone();
            let ind = model_info.inductives.get(&ty)?;
            let base = *struct_idx;
            let mut indices = Vec::new();
            for i in 0..ind.ctors.len() {
                indices.push(base + i as u32);
            }
            Some((ty, indices))
        }
        _ => None,
    }
}

/// The `And` projection selecting conjunct `pos` of a right-nested `k`-way
/// conjunction: `.1` for the first, `.2.…​.2.1` for the middle, `.2.…​.2` for the
/// last. For the two-member SCC this is `.1` / `.2`.
fn conjunct_proj(pos: usize, k: usize) -> String {
    let mut s = String::new();
    for _ in 0..pos {
        s.push_str(".2");
    }
    if pos + 1 < k {
        s.push_str(".1");
    }
    s
}

/// Evaluate the mutual model at `scc[pos]` on input `n` under partial-correctness
/// semantics (`f n = if n ≤ 0 then base else cross (n-1)`) to obtain the
/// anti-vacuity guard values. Terminates: `n` strictly descends to the base.
fn eval_mutual(scc: &[MutualMember], pos: usize, n: i64) -> i64 {
    if n <= 0 {
        return scc[pos].base_k;
    }
    let cross_pos = scc
        .iter()
        .position(|m| m.self_idx == scc[pos].cross_idx)
        .expect("mutual SCC cross target is a member");
    eval_mutual(scc, cross_pos, n - 1)
}

// Mutual-recursion proof rendering lives in render_mutual.rs.
