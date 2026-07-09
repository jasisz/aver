impl Cert {
    fn inner(&self) -> &Cert {
        match self {
            Cert::NonRecursive { inner } => inner,
            _ => self,
        }
    }

    fn name(&self) -> &str {
        match self.inner() {
            Cert::StraightLine { name, .. }
            | Cert::Recursive { name, .. }
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
            Cert::StraightLine { self_idx, .. }
            | Cert::Recursive { self_idx, .. }
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
    fn carrier(&self) -> u32 {
        match self.inner() {
            Cert::StraightLine { carrier, .. }
            | Cert::Recursive { carrier, .. }
            | Cert::AccumulatorRecursive { carrier, .. }
            | Cert::AdtConstructor { carrier, .. }
            | Cert::FieldProjection { carrier, .. }
            | Cert::WidenedIntMatch { carrier, .. }
            | Cert::VerbatimWidenedMatch { carrier, .. }
            | Cert::VerbatimVariantDispatch { carrier, .. }
            | Cert::StringEqVerbatimMatch { carrier, .. }
            | Cert::StringConcatVerbatimMatch { carrier, .. }
            | Cert::ExprFragment { carrier, .. }
            | Cert::VariantDispatch { carrier, .. }
            | Cert::Composition { carrier, .. }
            | Cert::MutualRecursion { carrier, .. } => *carrier,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    fn arity(&self) -> usize {
        match self.inner() {
            Cert::StraightLine { .. } | Cert::Recursive { .. } | Cert::MutualRecursion { .. } => 1,
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
    /// The Lean expression for the model this export simulates.
    fn model_expr(&self) -> String {
        match self.inner() {
            Cert::StraightLine { k, .. } => format!("fun ns => ns.headD 0 + ({k})"),
            Cert::Recursive { name, .. }
            | Cert::Composition { name, .. }
            | Cert::MutualRecursion { name, .. } => {
                format!("fun ns => {name} (ns.headD 0)")
            }
            Cert::AccumulatorRecursive { name, .. } => {
                format!("fun ns => {name} (ns.headD 0) ((ns.drop 1).headD 0)")
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
        match self.inner() {
            Cert::StraightLine { name, .. } => {
                format!("fun add _ _ _ _ => CertModule.{name}Host add")
            }
            Cert::Recursive {
                name, combinator, ..
            } => {
                // Draw the combinator slot (`add` or `mul`) from the obligation.
                format!(
                    "fun add sub mul _ _ => CertModule.{name}Host {} sub",
                    combinator.param()
                )
            }
            Cert::AccumulatorRecursive { name, .. } | Cert::Composition { name, .. } => {
                format!("fun add sub _ _ _ => CertModule.{name}Host add sub")
            }
            // The whole SCC shares one host (box + sub only), named after the
            // primary (lowest-`self_idx`) member; every member's obligation points
            // at it. `add`/`mul` are ignored (mutual has no combinator).
            Cert::MutualRecursion { scc, .. } => {
                format!("fun _ sub _ _ _ => CertModule.{}Host sub", scc[0].name)
            }
            Cert::AdtConstructor { name, .. } | Cert::FieldProjection { name, .. } => {
                format!("fun _ _ _ _ _ => CertModule.{name}Host")
            }
            Cert::WidenedIntMatch { name, .. }
            | Cert::VerbatimWidenedMatch { name, .. }
            | Cert::VerbatimVariantDispatch { name, .. } => {
                format!("fun _ _ _ _ _ => CertModule.{name}Host")
            }
            Cert::ExprFragment { name, .. } => {
                format!("fun _ _ _ _ _ => CertModule.{name}Host")
            }
            Cert::StringEqVerbatimMatch { name, .. } => {
                format!("fun _ _ _ stringEq _ => CertModule.{name}Host stringEq")
            }
            Cert::StringConcatVerbatimMatch { name, .. } => {
                format!("fun _ _ _ _ stringConcat => CertModule.{name}Host stringConcat")
            }
            Cert::VariantDispatch { name, .. } => {
                format!("fun add sub _ _ _ => CertModule.{name}Host add sub")
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
        match self.inner() {
            Cert::StraightLine { .. }
            | Cert::Recursive { .. }
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
                let dom = model_info
                    .fns
                    .get(name)
                    .and_then(|s| s.params.first())
                    .map(|s| ascii(s))
                    .unwrap_or_else(|| "Op".to_string());
                (dom, "Int".to_string())
            }
            Cert::AdtConstructor { arity, .. } => {
                if adt_constructor_uses_model(self, model_info) {
                    let cod = model_info
                        .fns
                        .get(self.name())
                        .map(|s| ascii(&s.ret))
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
