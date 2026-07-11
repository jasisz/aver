fn render_certificate(
    analysis: &Analysis,
    model_roots: &[String],
    model_info: &ModelInfo,
) -> String {
    let mut s = String::new();
    s.push_str("import CertPrelude\nimport Module\nimport Schema\nimport Manifest\n");
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
            Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. } => {
                s.push_str(&render_fueled_recursion_cert(c))
            }
            Cert::AdtConstructor { .. } => s.push_str(&render_adt_constructor_cert(c, model_info)),
            Cert::FieldProjection {
                name,
                self_idx,
                carrier,
                struct_idx,
                field_idx,
                ..
            } => s.push_str(&render_struct_verbatim_cert(
                name,
                *self_idx,
                *carrier,
                *struct_idx,
                StructVerbatimShape::Project {
                    field_idx: *field_idx,
                },
            )),
            Cert::WidenedIntMatch { .. } | Cert::VariantDispatch { .. } => {
                s.push_str(&render_adt_match_cert(c, model_info))
            }
            Cert::ExprFragment { .. } => s.push_str(&render_expr_fragment_cert(c)),
            Cert::VerbatimWidenedMatch { .. } | Cert::VerbatimVariantDispatch { .. } => {
                s.push_str(&render_verbatim_wval_match_cert(c))
            }
            Cert::StringEqVerbatimMatch { .. } => {
                s.push_str(&render_string_eq_verbatim_match_cert(c))
            }
            Cert::StringConcatVerbatimMatch { .. } => {
                s.push_str(&render_string_concat_verbatim_match_cert(c))
            }
            Cert::Composition { .. } => s.push_str(&render_composition_cert(c)),
            Cert::MutualRecursion { .. } => s.push_str(&render_mutual_recursion_cert(c)),
            Cert::NonRecursive { .. } => unreachable!(),
        }
        s.push('\n');
    }
    s.push_str("end CertProofs\n");
    s
}
