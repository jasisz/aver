// Included from engine/mod.rs (engine feature) — see the include! list there.

/// One law-claim of a certificate package: a universal law theorem of the
/// emitted model modules that the certificate's `Laws.lean` corollary cites,
/// keyed by the stable source-level `module.fn.law` label.
///
/// The producer HANDS these over as structure. The emitter that built the law
/// theorem's statement records the claim at the point it wrote the theorem
/// (`ProjectOutput::law_claims` on the compiler side), so this crate never
/// reads the emitted Lean text to recover what was stated. That also keeps the
/// `-- aver:law-class` marker private to the compiler: nothing here parses it,
/// and `aver-cert verify` reads the manifest's `laws` array, not the model
/// files.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawClaim {
    /// Stable source identity (`Domain.Rational.plus.commutative`).
    pub label: String,
    /// Dotted namespace the theorem was emitted under (`Domain.Rational`).
    pub prefix: String,
    /// Bare theorem name inside that namespace (`plus_law_commutative`).
    pub theorem: String,
    /// The theorem's universal statement, on one line — exactly the text the
    /// emitter wrote between `theorem <name> : ` and ` := by`.
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

/// Mirror of the checker's `validate_law_candidate` gates, kept as a
/// DEFENSIVE check on the rendered statement even though the claim now arrives
/// as structure: the producer must never write a manifest entry its own
/// checker hard-rejects — one refused entry fails candidate parsing for the
/// WHOLE package before Lean even runs. Legitimate compiler output can trip
/// the gates (a record literal `{ field := value }` in a statement carries
/// `:=`; a reserved-word module escapes to `Type'`), so such a law is simply
/// not claimed — the surface is additive and omitting a claim is fail-closed.
fn claim_survives_checker_gates(claim: &LawClaim) -> bool {
    let plain_dotted = |value: &str| {
        !value.is_empty()
            && value.len() <= 200
            && value.split('.').all(|segment| {
                let mut chars = segment.chars();
                matches!(chars.next(), Some(first) if first.is_ascii_alphabetic() || first == '_')
                    && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            })
    };
    if !plain_dotted(&claim.label)
        || !plain_dotted(&claim.qualified())
        || !plain_dotted(&claim.corollary())
    {
        return false;
    }
    let statement = &claim.statement;
    if statement.is_empty()
        || statement.len() > 2000
        || statement.contains('\n')
        || statement.contains(":=")
        || statement.contains("--")
        || statement.contains("/-")
    {
        return false;
    }
    let mut depth: Vec<char> = Vec::new();
    for character in statement.chars() {
        let matched = match character {
            '(' | '[' | '{' | '⟨' => {
                depth.push(character);
                true
            }
            ')' => depth.pop() == Some('('),
            ']' => depth.pop() == Some('['),
            '}' => depth.pop() == Some('{'),
            '⟩' => depth.pop() == Some('⟨'),
            _ => true,
        };
        if !matched {
            return false;
        }
    }
    depth.is_empty()
}

/// Admit the law-claims the producer handed over.
///
/// The compiler's Lean emitter records one claim per exported universal law at
/// the point it writes that law's theorem, so claims arrive as STRUCTURE and
/// nothing here reads the emitted `.lean` text. All this function does is
/// apply the defensive gates above: a claim whose rendered statement or whose
/// identifiers the checker would refuse is dropped, because a single refused
/// entry fails candidate parsing for the whole package before Lean even runs.
/// Each dropped claim comes back as `(label, reason)` so the caller can say
/// what it declined instead of losing it silently.
pub fn admit_law_claims(claims: Vec<LawClaim>) -> (Vec<LawClaim>, Vec<(String, String)>) {
    let mut admitted = Vec::with_capacity(claims.len());
    let mut declined = Vec::new();
    for claim in claims {
        if claim_survives_checker_gates(&claim) {
            admitted.push(claim);
        } else {
            declined.push((
                claim.label.clone(),
                "statement or identifiers would be refused by the checker's law gates".to_string(),
            ));
        }
    }
    (admitted, declined)
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
