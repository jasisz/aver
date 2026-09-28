// Report computations are proved in bounded declarations before the witness
// pins their results to JSON. These are optional proof terms, never authority
// for the report's statement: the checker supplies that statement and audits
// every axiom reached through the selected proof.

const REPORT_BLOCK: usize = 64;
const REPORT_BLOCKS_PER_MODULE: usize = 4;

fn render_report_blocks(analysis: &Analysis) -> Vec<(String, String)> {
    let mut entries = Vec::new();
    let mut facets = Vec::new();
    let mut policies = Vec::new();
    let mut terminations = Vec::new();
    for c in &analysis.certified {
        entries.push(format!("({}, {})", lean_str(&c.name), lean_str(PLAN_CLASS)));
        facets.push(format!(
            "({}, [{}])",
            lean_str(&c.name),
            c.facets.iter().map(|f| lean_str(f)).collect::<Vec<_>>().join(", ")
        ));
        policies.push(if c.total {
            ".simulatesModelTotally".to_string()
        } else {
            ".simulatesModel".to_string()
        });
        terminations.push(if c.total {
            "some { measure := .intNatAbs 0, descent := -1 }".to_string()
        } else {
            "none".to_string()
        });
    }
    let reports = [
        (
            "entries",
            "String × String",
            "AverCert.manifest.obligations.map (fun o => (o.export_, AverCert.ClaimAxes.planClass))",
            entries,
        ),
        (
            "facets",
            "String × List String",
            "AverCert.ClaimAxes.reportFacetsFast AverCert.manifest.fnPlans",
            facets,
        ),
        (
            "policies",
            "AverCert.Schema.Policy",
            "AverCert.ClaimAxes.policiesFast AverCert.manifest.fnPlans",
            policies,
        ),
        (
            "terminations",
            "Option AverCert.Schema.TerminationWitness",
            "AverCert.ClaimAxes.terminationsFast AverCert.manifest.fnPlans",
            terminations,
        ),
    ];
    // Even an empty report proves its whole tail empty. No producer-chosen
    // length is used to truncate the list that the wall computes.
    let count = analysis.certified.len().div_ceil(REPORT_BLOCK).max(1);
    let mut blocks = vec![String::new(); count];
    let mut joined = String::new();
    for (kind, ty, source, values) in reports {
        let mut names = Vec::new();
        for (b, text) in blocks.iter_mut().enumerate() {
            let k = b * REPORT_BLOCK;
            let end = (k + REPORT_BLOCK).min(values.len());
            let name = format!("report_{kind}_{b}");
            let lhs = if b + 1 == count {
                format!("({source}).drop {k}")
            } else {
                format!("(({source}).drop {k}).take {REPORT_BLOCK}")
            };
            text.push_str(&format!(
                "def {name} : List ({ty}) :=\n  [{}]\n\n\
                 theorem {name}_checked : {lhs} = {name} := by\n  decide +kernel\n\n",
                values[k..end].join(", ")
            ));
            names.push(name);
        }
        let mut proof = format!("{}_checked", names[count - 1]);
        for b in (0..count - 1).rev() {
            proof = format!(
                "AverCert.ClaimAxes.report_cons {}_checked\n    ({proof})",
                names[b]
            );
        }
        joined.push_str(&format!(
            "theorem report_{kind} :\n    {source} = {} :=\n  {proof}\n\n",
            right_nested(&names)
        ));
    }
    let mut files = Vec::new();
    let mut imports = String::new();
    for (m, texts) in blocks.chunks(REPORT_BLOCKS_PER_MODULE).enumerate() {
        let module = format!("ArtifactReportBlocks{m}");
        imports.push_str(&format!("import {module}\n"));
        files.push((
            format!("{module}.lean"),
            format!(
                "-- Report values checked a bounded block at a time.\n\
                 import Manifest\nimport ClaimAxes\n\n\
                 {ARTIFACT_HEADER}{}end AverCert.Artifact\n",
                texts.concat()
            ),
        ));
    }
    files.push((
        "ArtifactReports.lean".to_string(),
        format!(
            "-- Optional report proofs. The checker pins their full statements to JSON.\n\
             {imports}\n{ARTIFACT_HEADER}{joined}end AverCert.Artifact\n"
        ),
    ));
    files
}
