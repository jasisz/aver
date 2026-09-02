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

    /// Name of this claim's BRIDGED corollary, emitted beside the plain one
    /// when every model function the statement mentions carries a bridge.
    ///
    /// The two are separate declarations on purpose. The plain corollary says
    /// the law holds of the source model and the bytes simulate the plan; the
    /// bridged one additionally says the plan IS that source model. A bridge
    /// whose script falls to `sorry` therefore costs the bridge and this
    /// corollary, and leaves the plain law's credit exactly where it was.
    pub fn bridged_corollary(&self) -> String {
        format!("{}{LAW_BRIDGED_COROLLARY_SUFFIX}", self.corollary())
    }
}

/// The suffix the bridged corollary carries over the plain one. Checker-owned
/// in the sense that matters: the manifest never declares this name, both sides
/// derive it from the claim's label.
pub const LAW_BRIDGED_COROLLARY_SUFFIX: &str = "_bridged";

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

/// Render the package's `Laws.lean`: one corollary per claim, conjoining the
/// law's universal statement with the artifact-level `Holds` fact by citing
/// the model theorem and `AverCert.Final.cert`. One kernel-checked name per
/// claim ties the law to exactly the certified bytes.
///
/// The statement is re-elaborated INSIDE the model theorem's own namespace,
/// not under an `open <prefix> in` at root. Those two contexts do not agree:
/// inside `namespace Json`, `Json.jsonInt` resolves to the constructor
/// `Json.Json.jsonInt`, while at root the same text reaches the accessor
/// `Json.jsonInt` that an `open` only adds an alias beside. Reproducing the
/// namespace makes the claim text mean exactly what it meant where the
/// emitter wrote it — the namespace is `theorem` minus its last segment, so
/// the manifest names the context it is read in.
///
/// Everything the certificate owns is spelled `_root_.`-qualified, so a model
/// module that declares an `AverCert` sub-namespace cannot shadow the fact
/// being conjoined or the proof term citing it.
///
/// `bridge_statements` carries, per claim, the plan-equals-source bridge
/// statements of every model function that claim's statement mentions — empty
/// when some mentioned function has no bridge.
///
/// A claim whose functions are all bridged gets a SECOND corollary,
/// `AverCert.Laws.<c>_bridged`, which conjoins those bridge statements after
/// `Holds`: one kernel-checked name saying this law holds of the source
/// function, the bytes simulate the plan, AND the plan IS that source function.
/// The two are deliberately separate declarations rather than one wider
/// corollary. A bridge whose fixed tactic script falls to `sorry` taints
/// everything that cites it, so folding the bridges into `Laws.<c>` made one
/// unfinished bridge remove the credit of every law that merely mentions the
/// function — a claim about the SOURCE model, which the bridge has no part in
/// proving. Split, a `sorry` in a bridge costs the bridge and the bridged
/// corollary, and the plain law keeps its credit.
pub fn render_laws_lean(claims: &[LawClaim], bridge_statements: &[Vec<(String, String)>]) -> String {
    let any_bridged = bridge_statements.iter().any(|entry| !entry.is_empty());
    let mut s = String::new();
    s.push_str(
        "-- Law-claims of this certificate. Each corollary conjoins one universal\n\
         -- law of the model modules with the artifact-level `Holds` fact, so a\n\
         -- single kernel-checked name ties the law to exactly the certified bytes.\n\
         -- A law whose every mentioned function carries a plan-equals-source\n\
         -- bridge gets a second `_bridged` corollary conjoining those bridges,\n\
         -- kept apart from the law's own so an unfinished bridge cannot cost\n\
         -- the law its credit.\n\
         -- Each statement is elaborated inside the namespace its model theorem was\n\
         -- emitted in, so the claim text means there what it means in the model.\n\
         import Manifest\n\
         import Final\n",
    );
    if any_bridged {
        s.push_str("import Bridge\n");
    }
    s.push_str("\nset_option autoImplicit false\n\n");
    for (index, claim) in claims.iter().enumerate() {
        let bridges: &[(String, String)] = bridge_statements
            .get(index)
            .map(Vec::as_slice)
            .unwrap_or_default();
        // Concatenated, never interpolated into a format string: a statement
        // carrying `{`/`}` must stay inert text.
        if !claim.prefix.is_empty() {
            s.push_str("namespace ");
            s.push_str(&claim.prefix);
            s.push_str("\n\n");
        }
        s.push_str("/-- law-claim `");
        s.push_str(&claim.label);
        s.push_str("` -/\ntheorem _root_.AverCert.Laws.");
        s.push_str(&claim.corollary());
        s.push_str(" :\n    (");
        s.push_str(&claim.statement);
        s.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  ⟨_root_.");
        s.push_str(&claim.qualified());
        s.push_str(", _root_.AverCert.Final.cert⟩\n\n");
        if !bridges.is_empty() {
            s.push_str("/-- law-claim `");
            s.push_str(&claim.label);
            s.push_str("`, with the plan-equals-source identity of every model\n    \
                        function it mentions. Separate from the corollary above so an\n    \
                        unfinished bridge costs this claim and not the law itself. -/\n\
                        theorem _root_.AverCert.Laws.");
            s.push_str(&claim.bridged_corollary());
            s.push_str(" :\n    (");
            s.push_str(&claim.statement);
            s.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest)");
            for (_, statement) in bridges {
                s.push_str(" ∧\n      (");
                s.push_str(statement);
                s.push(')');
            }
            s.push_str(" :=\n  ⟨_root_.");
            s.push_str(&claim.qualified());
            s.push_str(", _root_.AverCert.Final.cert");
            for (corollary, _) in bridges {
                s.push_str(",\n    (_root_.");
                s.push_str(corollary);
                s.push_str(").1");
            }
            s.push_str("⟩\n\n");
        }
        if !claim.prefix.is_empty() {
            s.push_str("end ");
            s.push_str(&claim.prefix);
            s.push_str("\n\n");
        }
    }
    s
}
