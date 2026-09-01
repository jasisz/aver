/// Promotion is an SCC property, never a per-export guess. The classifier only
/// constructs `MutualRecursion` for the exact `n ≤ 0` / `g (n - 1)` grammar;
/// this final fail-closed check additionally requires one closed simple cycle
/// in the shared SCC value carried by every member. Thus a malformed or mixed
/// group yields no witness for any of its obligations.
fn mutual_scc_total_eligible(scc: &[MutualMember]) -> bool {
    if scc.len() < 2
        || scc
            .iter()
            .any(|member| scc.iter().filter(|m| m.self_idx == member.self_idx).count() != 1)
    {
        return false;
    }
    let start = scc[0].self_idx;
    let mut current = start;
    let mut visited = std::collections::HashSet::with_capacity(scc.len());
    for _ in 0..scc.len() {
        if !visited.insert(current) {
            return false;
        }
        let Some(member) = scc.iter().find(|m| m.self_idx == current) else {
            return false;
        };
        current = member.cross_idx;
    }
    current == start && visited.len() == scc.len()
}

impl Cert {
    fn inner(&self) -> &Cert {
        match self {
            Cert::NonRecursive { inner } => inner,
            _ => self,
        }
    }

    /// The total families: unary `n - 1` recursion, the exact two-argument
    /// accumulator, and complete integer-countdown mutual SCCs.  Only unary
    /// multiplication selects the additional Int.mul totality premise.
    fn termination_witness(&self) -> Option<TerminationWitness> {
        match self.inner() {
            Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. } => {
                Some(TerminationWitness {
                measure: TerminationMeasure::IntNatAbs { param_idx: 0 },
                descent: -1,
                })
            }
            Cert::MutualRecursion { scc, .. } if mutual_scc_total_eligible(scc) => {
                Some(TerminationWitness {
                    measure: TerminationMeasure::IntNatAbs { param_idx: 0 },
                    descent: -1,
                })
            }
            _ => None,
        }
    }

    fn policy(&self) -> CertificationPolicy {
        if self.termination_witness().is_some() {
            CertificationPolicy::SimulatesModelTotally
        } else {
            CertificationPolicy::SimulatesModel
        }
    }

    /// Whether this total obligation genuinely executes a byte-pinned
    /// `Int.mul` combine and therefore needs multiplication to be total.
    fn requires_mul_totality(&self) -> bool {
        self.policy() == CertificationPolicy::SimulatesModelTotally
            && matches!(
                self.inner(),
                Cert::Recursive {
                    combinator: Combinator::Mul,
                    ..
                }
            )
    }

    fn totality_role_lean_value(&self) -> &'static str {
        if self.requires_mul_totality() {
            ".mul"
        } else {
            ".addSub"
        }
    }

    /// The straight-line integer face of a host-call expr fragment, when this
    /// cert is an `ExprFragment` whose plan is exactly `add(param0, box(k))`.
    /// Renderers branch on this to state the full-strength integer obligation
    /// and proof (no weakening).
    fn int_add_face(&self) -> Option<FragIntAddFace> {
        match self.inner() {
            Cert::ExprFragment { plan, .. } => expr_fragment_int_add_face(plan),
            _ => None,
        }
    }

    /// The verbatim field-projection face of an ADT-ref expr fragment, when
    /// this cert is an `ExprFragment` whose plan is exactly `struct.get ty
    /// field∈{0,1}` of the single reference parameter. Renderers branch on
    /// this to state the same obligation and proof the legacy field-projection
    /// class ships (no weakening).
    fn project_face(&self) -> Option<FragProjectFace> {
        match self.inner() {
            Cert::ExprFragment { plan, .. } => expr_fragment_project_face(plan),
            _ => None,
        }
    }

    fn tag_dispatch_face(&self) -> Option<FragTagDispatchFace> {
        match self.inner() {
            Cert::ExprFragment { plan, .. } => expr_fragment_tag_dispatch_face(plan),
            _ => None,
        }
    }

    /// The stage-1 record scalar field-read face of an ADT-ref expr fragment,
    /// when this cert is an `ExprFragment` whose plan is exactly
    /// `struct.get structIdx field` yielding a scalar leaf AND whose struct
    /// decoded to a flat scalar record declaration (`record_decl`). Renderers
    /// branch on this to state the wall's record-parameter obligation and
    /// discharge through `recordParam_claim_discharges`. The `record_decl`
    /// guard means a surviving record-projection cert always has its ordered
    /// leaf list available.
    fn record_param_face(&self) -> Option<FragRecordProjFace> {
        match self.inner() {
            Cert::ExprFragment {
                plan,
                record_decl: Some(_),
                ..
            } => expr_fragment_record_proj_face(plan),
            _ => None,
        }
    }

    /// The projected struct index and ordered scalar-leaf field list of a record
    /// projection cert, byte-derived at check time.
    /// The record projection-compute face, when the sidecar recognized one
    /// over an all-Int record. Renderers branch on this to state the wall's
    /// compute obligation (plan-as-claim) and discharge through
    /// `recordCompute_claim_discharges`.
    fn record_compute_face(&self) -> Option<FragRecordComputeFace> {
        match self.inner() {
            Cert::ExprFragment {
                record_compute: Some(face),
                ..
            } => Some(*face),
            _ => None,
        }
    }

    fn record_decl(&self) -> Option<(u32, &[RecordLeaf])> {
        match self.inner() {
            Cert::ExprFragment {
                record_decl: Some((struct_idx, leaves)),
                ..
            } => Some((*struct_idx, leaves.as_slice())),
            _ => None,
        }
    }

    fn vector_get_face(&self) -> Option<FragVectorGetOrDefaultFace> {
        match self.inner() {
            Cert::ExprFragment { plan, .. } => expr_fragment_vector_get_face(plan),
            _ => None,
        }
    }

    /// The Int value-versus-value comparison face (`a >= b`, `a == b`), when
    /// this cert is an `ExprFragment` whose plan is exactly the pinned
    /// comparison node list. The wall fixes the whole meaning of this face —
    /// domain, codomain, both representation relations, the single host slot
    /// AND the model — so renderers emit no bespoke proof for it.
    fn int_cmp_bool_face(&self) -> Option<FragIntCmpFace> {
        match self.inner() {
            Cert::ExprFragment { plan, .. } => expr_fragment_int_cmp_bool_face(plan),
            _ => None,
        }
    }

    /// The Int selection face (`match a < b { true -> a; false -> b }`), the
    /// same way. Its result is a passthrough of an input local.
    fn int_select_face(&self) -> Option<FragIntCmpFace> {
        match self.inner() {
            Cert::ExprFragment { plan, .. } => expr_fragment_int_select_face(plan),
            _ => None,
        }
    }

    /// Either comparison face, for the many sites that only need to know that
    /// the wall owns this claim's meaning.
    fn int_cmp_face(&self) -> Option<FragIntCmpFace> {
        self.int_cmp_bool_face().or_else(|| self.int_select_face())
    }

    fn name(&self) -> &str {
        match self.inner() {
            Cert::Recursive { name, .. }
            | Cert::AccumulatorRecursive { name, .. }
            | Cert::AdtConstructor { name, .. }
            | Cert::FieldProjection { name, .. }
            | Cert::WidenedIntMatch { name, .. }
            | Cert::VerbatimWidenedMatch { name, .. }
            | Cert::VerbatimVariantDispatch { name, .. }
            | Cert::StringEqVerbatimMatch { name, .. }
            | Cert::StringConcatVerbatimMatch { name, .. }
            | Cert::ExprFragment { name, .. }
            | Cert::VariantDispatch { name, .. }
            | Cert::Composition { name, .. }
            | Cert::MutualRecursion { name, .. } => name,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    fn self_idx(&self) -> u32 {
        match self.inner() {
            Cert::Recursive { self_idx, .. }
            | Cert::AccumulatorRecursive { self_idx, .. }
            | Cert::AdtConstructor { self_idx, .. }
            | Cert::FieldProjection { self_idx, .. }
            | Cert::WidenedIntMatch { self_idx, .. }
            | Cert::VerbatimWidenedMatch { self_idx, .. }
            | Cert::VerbatimVariantDispatch { self_idx, .. }
            | Cert::StringEqVerbatimMatch { self_idx, .. }
            | Cert::StringConcatVerbatimMatch { self_idx, .. }
            | Cert::ExprFragment { self_idx, .. }
            | Cert::VariantDispatch { self_idx, .. }
            | Cert::Composition { self_idx, .. }
            | Cert::MutualRecursion { self_idx, .. } => *self_idx,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    /// The carrier type index this certificate's `Obligation` declares. A
    /// String.concat certificate in a module with no Int carrier struct has no
    /// index to declare and uses the reserved `0`, matching the wall's
    /// `CertDecode.carrierState`'s carrierless arm (`decodedCarrierIndex`).
    fn carrier(&self) -> u32 {
        match self.inner() {
            Cert::Recursive { carrier, .. }
            | Cert::AccumulatorRecursive { carrier, .. }
            | Cert::AdtConstructor { carrier, .. }
            | Cert::FieldProjection { carrier, .. }
            | Cert::WidenedIntMatch { carrier, .. }
            | Cert::VerbatimWidenedMatch { carrier, .. }
            | Cert::VerbatimVariantDispatch { carrier, .. }
            | Cert::StringEqVerbatimMatch { carrier, .. }
            | Cert::ExprFragment { carrier, .. }
            | Cert::VariantDispatch { carrier, .. }
            | Cert::Composition { carrier, .. }
            | Cert::MutualRecursion { carrier, .. } => *carrier,
            Cert::StringConcatVerbatimMatch { carrier, .. } => carrier.unwrap_or(0),
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    fn arity(&self) -> usize {
        match self.inner() {
            Cert::Recursive { .. } | Cert::MutualRecursion { .. } => 1,
            Cert::AccumulatorRecursive { .. } => 2,
            Cert::ExprFragment { plan, .. } => plan.arity(),
            Cert::AdtConstructor { arity, .. } => *arity,
            Cert::FieldProjection { .. }
            | Cert::WidenedIntMatch { .. }
            | Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::StringConcatVerbatimMatch { .. }
            | Cert::VariantDispatch { .. }
            | Cert::Composition { .. } => 1,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    /// The qualified Lean identifier of this export's source model function.
    ///
    /// Only valid for a cert that passed `model_citation_gate` against THIS
    /// `model_info` — `analyze` applies that gate to every cert it keeps, and
    /// `write_project` re-checks it before rendering anything, so a resolution
    /// failure here is a producer bug rather than a certificate that should
    /// decline. Citing an unresolved name would be a guess, so this panics
    /// instead of emitting one.
    fn model_lean_name(&self, model_info: &ModelInfo) -> String {
        model_info
            .model_lean_name(self.name())
            .expect("model-citing certificate passed the qualified-name gate")
    }
    /// The Lean expression for the model this export simulates.
    fn model_expr(&self, model_info: &ModelInfo) -> String {
        if let Some(face) = self.int_add_face() {
            return format!("fun ns => ns.headD 0 + ({})", face.k);
        }
        match self.inner() {
            Cert::Recursive { .. }
            | Cert::Composition { .. }
            | Cert::MutualRecursion { .. } => {
                format!("fun ns => {} (ns.headD 0)", self.model_lean_name(model_info))
            }
            Cert::AccumulatorRecursive { .. } => {
                format!(
                    "fun ns => {} (ns.headD 0) ((ns.drop 1).headD 0)",
                    self.model_lean_name(model_info)
                )
            }
            Cert::AdtConstructor { .. }
            | Cert::FieldProjection { .. }
            | Cert::WidenedIntMatch { .. }
            | Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::StringConcatVerbatimMatch { .. }
            | Cert::ExprFragment { .. }
            | Cert::VariantDispatch { .. } => "fun x => x".to_string(),
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    /// The Lean expression for the 4-arg host builder in `Obligation` shape
    /// (`add → sub → mul → stringEq → HostTbl`). Every named host keeps its own arity; this
    /// wraps it to the obligation shape, ignoring the contracts it does not wire.
    fn host_expr(&self) -> String {
        if let Some(_face) = self.int_add_face() {
            let name = self.name();
            return format!("fun add _ _ _ _ _ _ _ => CertModule.{name}Host add");
        }
        if let Some(face) = self.tag_dispatch_face() {
            return format!(
                "AverCert.StandardFace.tagDispatchHost {} {}",
                self.carrier(),
                face.box_idx
            );
        }
        if let Some(face) = self.int_cmp_face() {
            return format!(
                "AverCert.StandardFace.intCmpHost {{ op := {}, helperIdx := {} }}",
                face.op.lean_ctor(),
                face.helper_idx
            );
        }
        if let Some(face) = self.vector_get_face() {
            return format!(
                "AverCert.StandardFace.vectorGetOrDefaultHost {} \
                 {{ arrTy := {}, toIndexIdx := {}, boxIdx := {}, d := ({} : Int) }}",
                self.carrier(),
                face.arr_ty,
                face.to_index_idx,
                face.box_idx,
                face.default
            );
        }
        match self.inner() {
            Cert::Recursive {
                name, combinator, ..
            } => {
                // Draw the combinator slot (`add` or `mul`) from the obligation.
                format!(
                    "fun add sub mul _ _ _ _ _ => CertModule.{name}Host {} sub",
                    combinator.param()
                )
            }
            Cert::AccumulatorRecursive { name, .. } | Cert::Composition { name, .. } => {
                format!("fun add sub _ _ _ _ _ _ => CertModule.{name}Host add sub")
            }
            // The whole SCC shares one host (box + sub only), named after the
            // primary (lowest-`self_idx`) member; every member's obligation points
            // at it. `add`/`mul` are ignored (mutual has no combinator).
            Cert::MutualRecursion { scc, .. } => {
                format!("fun _ sub _ _ _ _ _ _ => CertModule.{}Host sub", scc[0].name)
            }
            Cert::AdtConstructor { name, .. } | Cert::FieldProjection { name, .. } => {
                format!("fun _ _ _ _ _ _ _ _ => CertModule.{name}Host")
            }
            Cert::WidenedIntMatch { name, .. }
            | Cert::VerbatimWidenedMatch { name, .. }
            | Cert::VerbatimVariantDispatch { name, .. } => {
                format!("fun _ _ _ _ _ _ _ _ => CertModule.{name}Host")
            }
            Cert::ExprFragment { name, .. } => {
                format!("fun _ _ _ _ _ _ _ _ => CertModule.{name}Host")
            }
            Cert::StringEqVerbatimMatch { name, .. } => {
                format!("fun _ _ _ stringEq _ _ _ _ => CertModule.{name}Host stringEq")
            }
            Cert::StringConcatVerbatimMatch { name, .. } => {
                format!("fun _ _ _ _ stringConcat _ _ _ => CertModule.{name}Host stringConcat")
            }
            Cert::VariantDispatch { name, .. } => {
                format!("fun add sub _ _ _ _ _ _ => CertModule.{name}Host add sub")
            }
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    /// The source-level `Dom`/`Cod` type names recorded in the manifest JSON so
    /// `aver cert verify`/`explain` can surface WHAT is certified without reading
    /// Lean. Display-only (the semantic content is what the witness pins);
    /// rendered ASCII-safe.
    fn source_dom_cod(&self, model_info: &ModelInfo) -> (String, String) {
        let ascii = |s: &str| ascii_type_name(s);
        if self.int_add_face().is_some() {
            return ("List Int".to_string(), "Int".to_string());
        }
        if self.project_face().is_some() {
            return ("WVal x WVal".to_string(), "WVal".to_string());
        }
        if self.tag_dispatch_face().is_some() {
            return ("Int x WVal".to_string(), "Int".to_string());
        }
        if self.vector_get_face().is_some() {
            return ("List Int x Int".to_string(), "Int".to_string());
        }
        if self.int_cmp_bool_face().is_some() {
            return ("Int x Int".to_string(), "Bool".to_string());
        }
        if self.int_select_face().is_some() {
            return ("Int x Int".to_string(), "Int".to_string());
        }
        match self.inner() {
            Cert::Recursive { .. }
            | Cert::AccumulatorRecursive { .. }
            | Cert::Composition { .. }
            | Cert::MutualRecursion { .. } => ("List Int".to_string(), "Int".to_string()),
            Cert::FieldProjection { .. } => ("WVal x WVal".to_string(), "WVal".to_string()),
            Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::StringConcatVerbatimMatch { .. } => ("WVal".to_string(), "WVal".to_string()),
            Cert::ExprFragment { plan, .. } => (plan.source_dom(), plan.source_cod()),
            Cert::VariantDispatch { name, .. } | Cert::WidenedIntMatch { name, .. } => {
                // Display the qualified inductive name when the written type
                // resolves (identity for entry-level models).
                let dom = model_info
                    .fns
                    .get(name)
                    .and_then(|s| {
                        s.params.first().map(|written| {
                            model_info
                                .resolve_inductive(&s.prefix, written)
                                .map(|(qualified, _)| qualified)
                                .unwrap_or_else(|| written.clone())
                        })
                    })
                    .map(|s| ascii(&s))
                    .unwrap_or_else(|| "Op".to_string());
                (dom, "Int".to_string())
            }
            Cert::AdtConstructor { arity, .. } => {
                if adt_constructor_uses_model(self, model_info) {
                    let cod = model_info
                        .fns
                        .get(self.name())
                        .map(|s| {
                            model_info
                                .resolve_inductive(&s.prefix, &s.ret)
                                .map(|(qualified, _)| qualified)
                                .unwrap_or_else(|| s.ret.clone())
                        })
                        .map(|s| ascii(&s))
                        .unwrap_or_else(|| "Unit".to_string());
                    ("Int".to_string(), cod)
                } else {
                    let dom = if *arity == 1 {
                        "WVal".to_string()
                    } else {
                        "WVal x WVal".to_string()
                    };
                    (dom, "WVal".to_string())
                }
            }
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
}

/// Render a Lean/source type name as printable ASCII for the manifest JSON: the
/// common math glyphs `×`/`→` become `x`/`->`, and any other non-ASCII byte is
/// dropped. Keeps a hostile-free, injection-free label the checker can display.
fn ascii_type_name(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '×' => out.push('x'),
            '→' => out.push_str("->"),
            c if c.is_ascii_graphic() || c == ' ' => out.push(c),
            _ => {}
        }
    }
    out
}
