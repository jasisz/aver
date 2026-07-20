fn render_certificate(
    analysis: &Analysis,
    model_roots: &[String],
    _model_info: &ModelInfo,
) -> String {
    let mut s = String::new();
    s.push_str(
        "import CertPrelude\nimport Module\nimport Schema\nimport Manifest\nimport IntDispatchSoundness\nimport DischargeComposition\nimport DischargeExprFragment\nimport DischargeRecursion\n",
    );
    for r in model_roots {
        s.push_str(&format!("import {r}\n"));
    }
    s.push_str(
        "\nset_option linter.unusedSimpArgs false\n\
         set_option linter.unusedVariables false\n\
         set_option maxRecDepth 1000000\n\n\
         namespace CertProofs\nopen CertPrelude CertModule AverCert AverCert.Schema\n\n",
    );
    let struct_table_lean = emit_frag_struct_table_lean(analysis)
        .expect("certified fragment struct table remains consistent");
    // Mutual option-(b) bridges share one concrete SCC/acceptance package.
    // Emit those packages first so source/export order cannot create a forward
    // reference when a non-primary member appears before its primary.
    for c in &analysis.certs {
        if matches!(c.inner(), Cert::MutualRecursion { .. }) {
            s.push_str(&render_mutual_shared_bridge_data(c));
        }
    }
    for c in &analysis.certs {
        match c.inner() {
            Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. }
                if recursion_uses_audited_generic(c) =>
            {
                s.push_str(&render_recursion_semantic_bridge(c))
            }
            Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. } => {
                s.push_str(&render_fueled_recursion_cert(c))
            }
            // Constructor packs (verbatim and named-ADT) are discharged in
            // `Final.cert`: verbatim packs by the canonical option-(c) leaf,
            // named constructors by the declared-envelope face transport.
            Cert::AdtConstructor { .. } => {}
            // The field-projection family is discharged in `Final.cert` by the
            // audited generic theorem plus its canonical option-(c) leaf bridge.
            // Its plan, obligation and claim data remain emitted unchanged.
            Cert::FieldProjection { .. } => {}
            // Int-face dispatch is discharged in `Final.cert` by the
            // declared-envelope face transport; no bespoke bridge is emitted.
            Cert::WidenedIntMatch { .. } | Cert::VariantDispatch { .. } => {}
            Cert::ExprFragment { .. }
                if expr_fragment_uses_audited_generic(c) || c.tag_dispatch_face().is_some() =>
            {
                s.push_str(&render_expr_fragment_semantic_bridge(
                    c,
                    analysis.frag_host_table,
                    &struct_table_lean,
                ))
            }
            Cert::ExprFragment { .. } => s.push_str(&render_expr_fragment_cert(c)),
            // Verbatim and String families are discharged by their audited
            // canonical leaf bridges. Their plans/claims/data remain emitted.
            Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::StringConcatVerbatimMatch { .. } => {}
            Cert::Composition { .. } => {
                s.push_str(&render_composition_semantic_bridge(c, analysis))
            }
            Cert::MutualRecursion { .. } => s.push_str(&render_mutual_semantic_bridge(c)),
            Cert::NonRecursive { .. } => unreachable!(),
        }
        s.push('\n');
    }
    s.push_str("end CertProofs\n");
    s
}
