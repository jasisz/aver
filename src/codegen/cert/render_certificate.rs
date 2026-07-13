fn render_certificate(
    analysis: &Analysis,
    model_roots: &[String],
    model_info: &ModelInfo,
) -> String {
    let mut s = String::new();
    s.push_str(
        "import CertPrelude\nimport Module\nimport Schema\nimport Manifest\nimport V3DispatchCore\nimport V3DischargeRecursion\n",
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
    for c in &analysis.certs {
        match c.inner() {
            Cert::Recursive { .. } if recursion_uses_audited_generic(c) => {
                s.push_str(&render_recursion_semantic_bridge(c))
            }
            Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. } => {
                s.push_str(&render_fueled_recursion_cert(c))
            }
            Cert::AdtConstructor { .. } if adt_constructor_uses_model(c, model_info) => {
                s.push_str(&render_adt_constructor_semantic_bridge(c, model_info))
            }
            // Verbatim constructor packs are option-(c) leaves discharged in
            // `Final.cert`; model-bearing constructors emit only their option-(b)
            // source-model bridge above.
            Cert::AdtConstructor { .. } => {}
            // The field-projection family is discharged in `Final.cert` by the
            // audited v3 generic plus its canonical option-(c) leaf bridge.
            // Its plan, obligation and claim data remain emitted unchanged.
            Cert::FieldProjection { .. } => {}
            Cert::WidenedIntMatch { .. } | Cert::VariantDispatch { .. } => {
                s.push_str(&render_int_dispatch_semantic_bridge(
                    c,
                    model_info,
                    analysis.frag_host_table,
                ))
            }
            Cert::ExprFragment { .. } => s.push_str(&render_expr_fragment_cert(c)),
            // Verbatim and String families are discharged by their audited
            // canonical leaf bridges. Their plans/claims/data remain emitted.
            Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::StringConcatVerbatimMatch { .. } => {}
            Cert::Composition { .. } => s.push_str(&render_composition_cert(c)),
            Cert::MutualRecursion { .. } => s.push_str(&render_mutual_recursion_cert(c)),
            Cert::NonRecursive { .. } => unreachable!(),
        }
        s.push('\n');
    }
    s.push_str("end CertProofs\n");
    s
}
