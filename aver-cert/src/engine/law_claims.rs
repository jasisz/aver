// Included from engine/mod.rs (engine feature) — see the include! list there.

/// One law-claim extracted from the emitted model modules: a universal law
/// theorem the certificate's `Laws.lean` corollary cites, keyed by the stable
/// source-level `module.fn.law` label from its `-- aver:law-class` marker.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawClaim {
    /// Stable source identity (`Domain.Rational.plus.commutative`).
    pub label: String,
    /// Dotted namespace the theorem was emitted under (`Domain.Rational`).
    pub prefix: String,
    /// Bare theorem name inside that namespace (`plus_law_commutative`).
    pub theorem: String,
    /// The theorem's verbatim universal statement text — the single line
    /// exactly as emitted between `theorem <name> : ` and ` := by`.
    pub statement: String,
}

impl LawClaim {
    /// Fully qualified Lean name of the model theorem.
    pub fn qualified(&self) -> String {
        if self.prefix.is_empty() {
            self.theorem.clone()
        } else {
            format!("{}.{}", self.prefix, self.theorem)
        }
    }

    /// Name of this claim's corollary theorem inside `AverCert.Laws`: the
    /// label with dots flattened to underscores, the same flattening the
    /// compiler applies to export names.
    pub fn corollary(&self) -> String {
        self.label.replace('.', "_")
    }
}

/// Marker prefix — kept in lockstep with the compiler emitter
/// (`LAW_CLASS_MARKER_PREFIX` in the aver-lang Lean codegen).
const LAW_CLASS_MARKER: &str = "-- aver:law-class ";

/// Scan the emitted model files for universal law theorems.
///
/// The emitter writes, for every exported law, one marker line
/// `-- aver:law-class <theorem> <class> <label>` followed (possibly after
/// support theorems) by the law theorem itself on a single line
/// `theorem <theorem> : <statement> := by`. Only `universal`-classed laws are
/// claims; a marker whose theorem never materializes on a single line (for
/// example a partitioned statement) contributes no claim — a law-claim is
/// additive surface, so omitting one is fail-closed, never wrong.
pub fn extract_law_claims(model_files: &[(String, String)]) -> Vec<LawClaim> {
    let mut claims = Vec::new();
    for (path, content) in model_files {
        if !path.ends_with(".lean") || path == "AverCommon.lean" || path == "lakefile.lean" {
            continue;
        }
        let mut namespaces: Vec<String> = Vec::new();
        let mut pending: Option<(String, String)> = None;
        for line in content.lines() {
            let trimmed = line.trim();
            if let Some(ns) = trimmed.strip_prefix("namespace ") {
                namespaces.push(ns.trim().to_string());
                continue;
            }
            if let Some(ns) = trimmed.strip_prefix("end ")
                && namespaces.last().map(String::as_str) == Some(ns.trim())
            {
                namespaces.pop();
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix(LAW_CLASS_MARKER) {
                let mut fields = rest.split_whitespace();
                pending = match (fields.next(), fields.next(), fields.next()) {
                    (Some(theorem), Some("universal"), Some(label)) => {
                        Some((theorem.to_string(), label.to_string()))
                    }
                    _ => None,
                };
                continue;
            }
            if let Some((theorem, label)) = pending.take() {
                let head = format!("theorem {theorem} : ");
                if let Some(rest) = trimmed.strip_prefix(head.as_str()) {
                    if let Some(statement) = rest.strip_suffix(" := by") {
                        claims.push(LawClaim {
                            label,
                            prefix: namespaces.join("."),
                            theorem,
                            statement: statement.to_string(),
                        });
                    }
                    // A marked theorem that is not single-line yields no claim.
                } else {
                    // Support theorems and proof lines sit between the marker
                    // and the law theorem; keep waiting for the marked name.
                    pending = Some((theorem, label));
                }
            }
        }
    }
    claims
}

/// Render the package's `Laws.lean`: one three-line corollary per claim,
/// conjoining the law's verbatim universal statement with the artifact-level
/// `Holds` fact by citing the model theorem and `AverCert.Final.cert`. One
/// kernel-checked name per claim ties the law to exactly the certified bytes.
pub fn render_laws_lean(claims: &[LawClaim]) -> String {
    let mut s = String::new();
    s.push_str(
        "-- Law-claims of this certificate. Each corollary conjoins one universal\n\
         -- law of the model modules with the artifact-level `Holds` fact, so a\n\
         -- single kernel-checked name ties the law to exactly the certified bytes.\n\
         import Manifest\nimport Final\n\nnamespace AverCert.Laws\n\n",
    );
    for claim in claims {
        let open_line = if claim.prefix.is_empty() {
            String::new()
        } else {
            format!("open {} in\n", claim.prefix)
        };
        s.push_str(&format!(
            "{open_line}/-- law-claim `{}` -/\ntheorem {} :\n    ({}) ∧ (AverCert.Schema.Holds AverCert.manifest) :=\n  ⟨{}, AverCert.Final.cert⟩\n\n",
            claim.label,
            claim.corollary(),
            claim.statement,
            claim.qualified(),
        ));
    }
    s.push_str("end AverCert.Laws\n");
    s
}
