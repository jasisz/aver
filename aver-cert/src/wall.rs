//! Checker-owned Lean soundness wall.
//!
//! These sources are artifact-independent and embedded in the verifier. A
//! certificate names the exact set through [`current_id`]; it never chooses a
//! path, URL, or ambient installation from which the verifier loads code.

use sha2::{Digest, Sha256};
use std::sync::OnceLock;

pub use crate::format::{CURRENT_WALL_ID as CURRENT_ID, FORMAT_VERSION};

pub const LEAN_TOOLCHAIN: &str = include_str!("../assets/wall/current/lean-toolchain");

pub const CERT_PRELUDE: &str = include_str!("../assets/wall/current/CertPrelude.lean");
pub const CERT_DECODE: &str = include_str!("../assets/wall/current/CertDecode.lean");
pub const CERT_SCHEMA: &str = include_str!("../assets/wall/current/Schema.lean");
pub const CERT_SCHEMA_CORE: &str = include_str!("../assets/wall/current/SchemaCore.lean");
pub const CERT_PLAN_CHECK: &str = include_str!("../assets/wall/current/PlanCheck.lean");
pub const CERT_PLAN_LOWER: &str = include_str!("../assets/wall/current/PlanLower.lean");
pub const CERT_PLAN_BYTES: &str = include_str!("../assets/wall/current/PlanBytes.lean");
pub const CERT_WASM_SLICE: &str = include_str!("../assets/wall/current/WasmSlice.lean");
pub const CERT_WASIP2_ENVELOPE: &str = include_str!("../assets/wall/current/Wasip2Envelope.lean");
pub const CERT_EXPR_FRAGMENT_ACCEPTED: &str =
    include_str!("../assets/wall/current/ExprFragmentAccepted.lean");
pub const CERT_ACCEPTED_ARTIFACT: &str =
    include_str!("../assets/wall/current/AcceptedArtifact.lean");
pub const CERT_ACCEPTED_ARTIFACT_CORE: &str =
    include_str!("../assets/wall/current/AcceptedArtifactCore.lean");
pub const CERT_CLAIM_AXES: &str = include_str!("../assets/wall/current/ClaimAxes.lean");
pub const CERT_EXPR_FRAGMENT_SEMANTICS: &str =
    include_str!("../assets/wall/current/ExprFragmentSemantics.lean");
pub const CERT_INTERPRETER_SEQUENCING: &str =
    include_str!("../assets/wall/current/InterpreterSequencing.lean");
pub const CERT_EXPR_FRAGMENT_SOUNDNESS: &str =
    include_str!("../assets/wall/current/ExprFragmentSoundness.lean");
pub const CERT_RECORD_COMPUTE_BRIDGE: &str =
    include_str!("../assets/wall/current/RecordComputeBridge.lean");
pub const CERT_FIELD_PROJECTION_SOUNDNESS: &str =
    include_str!("../assets/wall/current/FieldProjectionSoundness.lean");
pub const CERT_CONSTRUCT_VERBATIM_SOUNDNESS: &str =
    include_str!("../assets/wall/current/ConstructVerbatimSoundness.lean");
pub const CERT_INT_DISPATCH_SOUNDNESS: &str =
    include_str!("../assets/wall/current/IntDispatchSoundness.lean");
pub const CERT_ENVELOPE_LOWERING: &str =
    include_str!("../assets/wall/current/EnvelopeLowering.lean");
pub const CERT_WIDENED_ENVELOPE: &str = include_str!("../assets/wall/current/WidenedEnvelope.lean");
pub const CERT_DECLARED_INDEX_ENVELOPE: &str =
    include_str!("../assets/wall/current/DeclaredIndexEnvelope.lean");
pub const CERT_DECLARED_ENVELOPE_ACCEPT_TRANSPORT: &str =
    include_str!("../assets/wall/current/DeclaredEnvelopeAcceptTransport.lean");
pub const CERT_STRING_SOUNDNESS: &str = include_str!("../assets/wall/current/StringSoundness.lean");
pub const CERT_STANDARD_FACE: &str = include_str!("../assets/wall/current/StandardFace.lean");
pub const CERT_RECURSION_SOUNDNESS: &str =
    include_str!("../assets/wall/current/RecursionSoundness.lean");
pub const CERT_MUTUAL_RECURSION_SOUNDNESS: &str =
    include_str!("../assets/wall/current/MutualRecursionSoundness.lean");
pub const CERT_COMPOSITION_SOUNDNESS: &str =
    include_str!("../assets/wall/current/CompositionSoundness.lean");
pub const CERT_ACCEPTANCE_SOUNDNESS_CORE: &str =
    include_str!("../assets/wall/current/AcceptanceSoundnessCore.lean");
pub const CERT_DISCHARGE_EXPR_FRAGMENT: &str =
    include_str!("../assets/wall/current/DischargeExprFragment.lean");
pub const CERT_DISCHARGE_FIELD_PROJECTION: &str =
    include_str!("../assets/wall/current/DischargeFieldProjection.lean");
pub const CERT_DISCHARGE_CONSTRUCT: &str =
    include_str!("../assets/wall/current/DischargeConstruct.lean");
pub const CERT_DISCHARGE_VERBATIM: &str =
    include_str!("../assets/wall/current/DischargeVerbatim.lean");
pub const CERT_DISCHARGE_STRING: &str = include_str!("../assets/wall/current/DischargeString.lean");
pub const CERT_DISCHARGE_INT_DISPATCH: &str =
    include_str!("../assets/wall/current/DischargeIntDispatch.lean");
pub const CERT_DISCHARGE_RECURSION: &str =
    include_str!("../assets/wall/current/DischargeRecursion.lean");
pub const CERT_DISCHARGE_COMPOSITION: &str =
    include_str!("../assets/wall/current/DischargeComposition.lean");
pub const CERT_ACCEPTANCE_SOUNDNESS: &str =
    include_str!("../assets/wall/current/AcceptanceSoundness.lean");
pub const CERT_ARITH_TEMPLATE_DERISK: &str =
    include_str!("../assets/wall/current/ArithTemplateDerisk.lean");

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Source {
    pub name: &'static str,
    pub contents: &'static str,
}

/// Exact checker-owned source set. Ordering is not part of the identity:
/// [`compute_id`] sorts by filename before hashing.
pub const SOURCES: [Source; 40] = [
    Source {
        name: "AcceptedArtifact.lean",
        contents: CERT_ACCEPTED_ARTIFACT,
    },
    Source {
        name: "ArithTemplateDerisk.lean",
        contents: CERT_ARITH_TEMPLATE_DERISK,
    },
    Source {
        name: "AcceptedArtifactCore.lean",
        contents: CERT_ACCEPTED_ARTIFACT_CORE,
    },
    Source {
        name: "AcceptanceSoundness.lean",
        contents: CERT_ACCEPTANCE_SOUNDNESS,
    },
    Source {
        name: "AcceptanceSoundnessCore.lean",
        contents: CERT_ACCEPTANCE_SOUNDNESS_CORE,
    },
    Source {
        name: "CertDecode.lean",
        contents: CERT_DECODE,
    },
    Source {
        name: "CertPrelude.lean",
        contents: CERT_PRELUDE,
    },
    Source {
        name: "ClaimAxes.lean",
        contents: CERT_CLAIM_AXES,
    },
    Source {
        name: "CompositionSoundness.lean",
        contents: CERT_COMPOSITION_SOUNDNESS,
    },
    Source {
        name: "ConstructVerbatimSoundness.lean",
        contents: CERT_CONSTRUCT_VERBATIM_SOUNDNESS,
    },
    Source {
        name: "DeclaredEnvelopeAcceptTransport.lean",
        contents: CERT_DECLARED_ENVELOPE_ACCEPT_TRANSPORT,
    },
    Source {
        name: "DeclaredIndexEnvelope.lean",
        contents: CERT_DECLARED_INDEX_ENVELOPE,
    },
    Source {
        name: "DischargeComposition.lean",
        contents: CERT_DISCHARGE_COMPOSITION,
    },
    Source {
        name: "DischargeConstruct.lean",
        contents: CERT_DISCHARGE_CONSTRUCT,
    },
    Source {
        name: "DischargeExprFragment.lean",
        contents: CERT_DISCHARGE_EXPR_FRAGMENT,
    },
    Source {
        name: "DischargeFieldProjection.lean",
        contents: CERT_DISCHARGE_FIELD_PROJECTION,
    },
    Source {
        name: "DischargeIntDispatch.lean",
        contents: CERT_DISCHARGE_INT_DISPATCH,
    },
    Source {
        name: "DischargeRecursion.lean",
        contents: CERT_DISCHARGE_RECURSION,
    },
    Source {
        name: "DischargeString.lean",
        contents: CERT_DISCHARGE_STRING,
    },
    Source {
        name: "DischargeVerbatim.lean",
        contents: CERT_DISCHARGE_VERBATIM,
    },
    Source {
        name: "EnvelopeLowering.lean",
        contents: CERT_ENVELOPE_LOWERING,
    },
    Source {
        name: "ExprFragmentAccepted.lean",
        contents: CERT_EXPR_FRAGMENT_ACCEPTED,
    },
    Source {
        name: "ExprFragmentSemantics.lean",
        contents: CERT_EXPR_FRAGMENT_SEMANTICS,
    },
    Source {
        name: "ExprFragmentSoundness.lean",
        contents: CERT_EXPR_FRAGMENT_SOUNDNESS,
    },
    Source {
        name: "RecordComputeBridge.lean",
        contents: CERT_RECORD_COMPUTE_BRIDGE,
    },
    Source {
        name: "FieldProjectionSoundness.lean",
        contents: CERT_FIELD_PROJECTION_SOUNDNESS,
    },
    Source {
        name: "IntDispatchSoundness.lean",
        contents: CERT_INT_DISPATCH_SOUNDNESS,
    },
    Source {
        name: "InterpreterSequencing.lean",
        contents: CERT_INTERPRETER_SEQUENCING,
    },
    Source {
        name: "MutualRecursionSoundness.lean",
        contents: CERT_MUTUAL_RECURSION_SOUNDNESS,
    },
    Source {
        name: "PlanBytes.lean",
        contents: CERT_PLAN_BYTES,
    },
    Source {
        name: "PlanCheck.lean",
        contents: CERT_PLAN_CHECK,
    },
    Source {
        name: "PlanLower.lean",
        contents: CERT_PLAN_LOWER,
    },
    Source {
        name: "RecursionSoundness.lean",
        contents: CERT_RECURSION_SOUNDNESS,
    },
    Source {
        name: "Schema.lean",
        contents: CERT_SCHEMA,
    },
    Source {
        name: "SchemaCore.lean",
        contents: CERT_SCHEMA_CORE,
    },
    Source {
        name: "StandardFace.lean",
        contents: CERT_STANDARD_FACE,
    },
    Source {
        name: "StringSoundness.lean",
        contents: CERT_STRING_SOUNDNESS,
    },
    Source {
        name: "WasmSlice.lean",
        contents: CERT_WASM_SLICE,
    },
    Source {
        name: "Wasip2Envelope.lean",
        contents: CERT_WASIP2_ENVELOPE,
    },
    Source {
        name: "WidenedEnvelope.lean",
        contents: CERT_WIDENED_ENVELOPE,
    },
];

/// Roots whose complete import graph is artifact-independent and can therefore
/// be cached before a certificate is seen.
pub const PRISTINE_ROOTS: [&str; 38] = [
    "CertPrelude",
    "CertDecode",
    "ArithTemplateDerisk",
    "WasmSlice",
    "Wasip2Envelope",
    "SchemaCore",
    "PlanCheck",
    "PlanLower",
    "PlanBytes",
    "ExprFragmentAccepted",
    "AcceptedArtifactCore",
    "ClaimAxes",
    "ExprFragmentSemantics",
    "InterpreterSequencing",
    "ExprFragmentSoundness",
    "RecordComputeBridge",
    "FieldProjectionSoundness",
    "ConstructVerbatimSoundness",
    "IntDispatchSoundness",
    "StringSoundness",
    "EnvelopeLowering",
    "WidenedEnvelope",
    "DeclaredIndexEnvelope",
    "DeclaredEnvelopeAcceptTransport",
    "StandardFace",
    "RecursionSoundness",
    "MutualRecursionSoundness",
    "CompositionSoundness",
    "AcceptanceSoundnessCore",
    "DischargeExprFragment",
    "DischargeFieldProjection",
    "DischargeConstruct",
    "DischargeVerbatim",
    "DischargeString",
    "DischargeIntDispatch",
    "DischargeRecursion",
    "DischargeComposition",
    "AcceptanceSoundness",
];

#[derive(Debug)]
pub struct Wall {
    pub sources: &'static [Source],
    pub pristine_roots: &'static [&'static str],
    pub toolchain: &'static str,
}

pub static CURRENT: Wall = Wall {
    sources: &SOURCES,
    pristine_roots: &PRISTINE_ROOTS,
    toolchain: LEAN_TOOLCHAIN,
};

/// Domain-separated digest of sorted, length-framed filenames and exact bytes.
/// The exact Lean toolchain is part of the wall identity as a synthetic file.
fn compute_id() -> String {
    let mut files = SOURCES
        .iter()
        .map(|source| (source.name, source.contents.as_bytes()))
        .chain(std::iter::once((
            "lean-toolchain",
            LEAN_TOOLCHAIN.as_bytes(),
        )))
        .collect::<Vec<_>>();
    files.sort_unstable_by_key(|(name, _)| *name);

    let mut hash = Sha256::new();
    hash.update(b"aver-certificate-wall\0v1\0");
    hash.update((files.len() as u64).to_be_bytes());
    for (name, contents) in files {
        hash.update((name.len() as u64).to_be_bytes());
        hash.update(name.as_bytes());
        hash.update((contents.len() as u64).to_be_bytes());
        hash.update(contents);
    }
    format!("sha256:{:x}", hash.finalize())
}

/// Identity of the one wall embedded in this pre-public verifier.
pub fn current_id() -> &'static str {
    static VERIFIED: OnceLock<()> = OnceLock::new();
    VERIFIED.get_or_init(|| {
        assert_eq!(
            compute_id(),
            CURRENT_ID,
            "embedded certificate wall changed without updating CURRENT_ID"
        );
    });
    CURRENT_ID
}

/// Resolve only checker-embedded, byte-exact walls. There is intentionally no
/// filesystem, environment, or network fallback.
pub fn resolve(id: &str) -> Option<&'static Wall> {
    (id == current_id()).then_some(&CURRENT)
}

/// Checker-authored Lean module containing the exact core module bytes. A
/// certificate package never supplies this module; production verification
/// and direct-Lake test harnesses materialize it from the artifact under test
/// after target-specific envelope preparation.
pub fn render_artifact_bytes(bytes: &[u8]) -> String {
    render_byte_module(
        "ArtifactBytes",
        "modBytes",
        "modLen",
        "Exact core Wasm module bytes consumed by the certificate wall.",
        bytes,
    )
}

/// Checker-authored Lean module containing the exact delivered target artifact
/// bytes. For wasm-gc this is the same byte string as `ArtifactBytes`; for
/// wasip2 this is the outer component whose declared envelope embeds the core.
pub fn render_artifact_component_bytes(bytes: &[u8]) -> String {
    render_byte_module(
        "ArtifactComponentBytes",
        "componentBytes",
        "componentLen",
        "Exact delivered target artifact bytes consumed by envelope checks.",
        bytes,
    )
}

fn render_byte_module(
    module: &str,
    bytes_name: &str,
    len_name: &str,
    description: &str,
    bytes: &[u8],
) -> String {
    let numeral = if bytes.is_empty() {
        "0".to_string()
    } else {
        let mut numeral = String::with_capacity(2 + bytes.len() * 2);
        numeral.push_str("0x");
        for byte in bytes.iter().rev() {
            numeral.push_str(&format!("{byte:02x}"));
        }
        numeral
    };
    format!(
        "import WasmSlice\n\nset_option maxRecDepth 200000\n\nnamespace AverCert.{module}\n\n/-- {description} -/\ndef {bytes_name} : Nat := {numeral}\ndef {len_name} : Nat := {}\n\nend AverCert.{module}\n",
        bytes.len()
    )
}

#[cfg(test)]
mod byte_binding_lint;

#[cfg(test)]
mod tests {
    use super::*;

    /// The wall as the lint consumes it: `(filename, source text)`.
    fn wall_sources() -> Vec<(&'static str, &'static str)> {
        SOURCES
            .iter()
            .map(|source| (source.name, source.contents))
            .collect()
    }

    const FORMAT_DOC: &str = include_str!("../../docs/certificate-format.md");

    /// Every value the producer declares must be constrained against the module
    /// bytes by some conjunct of `AverCert.AcceptedArtifact.accepted`, or carry
    /// an explicit, reasoned exception.
    ///
    /// This is the mechanised form of a failure that has recurred six times: a
    /// declared value consumed by a proof while nothing pinned it to the bytes.
    /// The most recent was `hostRoleTable.toIndex`, where the byte-derived
    /// binding existed, its comment described exactly the attack it prevented,
    /// and its only consumer had silently left the acceptance path. Reviewers
    /// reading documentation saw a pin that was not there; this test reads the
    /// wall instead.
    #[test]
    fn producer_declared_values_are_bound_to_the_module_bytes() {
        match byte_binding_lint::check(&wall_sources(), FORMAT_DOC) {
            Ok(report) => println!("{report}"),
            Err(failure) => panic!("{failure}"),
        }
    }

    /// The lint must still be able to SEE the historical gap.
    ///
    /// Deleting every conjunct that pins `roles.toIndex` reproduces the pre-fix
    /// acceptance predicate for that field, and the lint must flag it. `box` is
    /// the control: a sibling field of the SAME structure, projected in the SAME
    /// function on ADJACENT lines, which must stay bound. Without that control
    /// the test would pass for a lint that had stopped discriminating and simply
    /// flags everything.
    ///
    /// There are now two such conjuncts, and BOTH have to go. The export-name
    /// equality that commit fd455ade added is the historical one; the template
    /// equality (`arithRoleCheck .toIndex`) came later and pins the bytes at the
    /// declared index. Removing only one leaves the field genuinely bound by the
    /// other — which is correct behaviour, not a lint failure — so the historical
    /// state is reproduced only by removing both.
    ///
    /// This test is the guard on every future precision refinement. Two
    /// plausible generalisations (transitive byte taint, and unrestricted
    /// whole-value propagation through call arguments) each looked like clean
    /// wins and each silently re-bound `toIndex`; this is what caught them.
    #[test]
    fn lint_still_flags_the_historical_index_helper_gap() {
        const NAME_PIN: &str = "(roles.toIndex == CertDecode.AddSub.toIndexIdx n len) &&";
        const TEMPLATE_PIN: &str = "arithRoleCheck n len .toIndex roles.toIndex p &&";
        let core = CERT_ACCEPTED_ARTIFACT_CORE;
        for pin in [NAME_PIN, TEMPLATE_PIN] {
            assert!(
                core.contains(pin),
                "a `toIndex` pin has moved ({pin:?}); this regression test must be \
                 re-aimed rather than deleted"
            );
        }
        let without_pin = core.replace(NAME_PIN, "").replace(TEMPLATE_PIN, "");

        let sources: Vec<(&str, &str)> = wall_sources()
            .into_iter()
            .map(|(name, text)| {
                if name == "AcceptedArtifactCore.lean" {
                    (name, without_pin.as_str())
                } else {
                    (name, text)
                }
            })
            .collect();

        let report = byte_binding_lint::analyse(&sources);
        assert!(
            report.is_flagged("AddSub.Roles", "toIndex"),
            "the lint no longer detects the historical index-helper gap: with both \
             `toIndex` pins removed, `Roles.toIndex` was still considered bound. A \
             precision refinement has blunted the lint past the point of usefulness."
        );
        assert!(
            report.is_bound("AddSub.Roles", "box"),
            "control failed: sibling field `Roles.box` is pinned by its own export \
             name and by its own template equality, on adjacent lines, and must \
             remain bound. The lint is flagging indiscriminately rather than \
             discriminating."
        );

        // Each pin binds the field on its own: removing either one alone must
        // leave `toIndex` bound. This is what makes the two-pin removal above a
        // reproduction of the historical gap rather than a weakened assertion.
        for pin in [NAME_PIN, TEMPLATE_PIN] {
            let one_gone = core.replace(pin, "");
            let sources: Vec<(&str, &str)> = wall_sources()
                .into_iter()
                .map(|(name, text)| {
                    if name == "AcceptedArtifactCore.lean" {
                        (name, one_gone.as_str())
                    } else {
                        (name, text)
                    }
                })
                .collect();
            assert!(
                byte_binding_lint::analyse(&sources).is_bound("AddSub.Roles", "toIndex"),
                "with only {pin:?} removed, `Roles.toIndex` lost its binding — the \
                 surviving pin should still bind it"
            );
        }

        // and with the pin present, the field is bound
        let clean = byte_binding_lint::analyse(&wall_sources());
        assert!(
            clean.is_bound("AddSub.Roles", "toIndex"),
            "`Roles.toIndex` is not bound on the current wall"
        );
    }

    /// Point the lint at an external directory of `.lean` sources, for auditing a
    /// historical or candidate wall. Ignored by default because it needs a tree
    /// that is not in the repository:
    ///
    /// ```text
    /// git archive fd455ade^ aver-cert/assets/wall/current | tar -x -C /tmp/prefix
    /// AVER_WALL_LINT_DIR=/tmp/prefix/aver-cert/assets/wall/current \
    ///   cargo test -p aver-cert --lib external_wall_directory -- --ignored --nocapture
    /// ```
    #[test]
    #[ignore = "requires AVER_WALL_LINT_DIR pointing at a directory of wall sources"]
    fn external_wall_directory() {
        let dir = std::env::var("AVER_WALL_LINT_DIR")
            .expect("set AVER_WALL_LINT_DIR to a directory of .lean wall sources");
        let mut texts: Vec<(String, String)> = Vec::new();
        for entry in std::fs::read_dir(&dir).expect("readable wall directory") {
            let path = entry.expect("readable entry").path();
            if path.extension().and_then(|e| e.to_str()) == Some("lean") {
                let name = path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .expect("utf-8 filename")
                    .to_string();
                texts.push((
                    name,
                    std::fs::read_to_string(&path).expect("readable source"),
                ));
            }
        }
        let sources: Vec<(&str, &str)> = texts
            .iter()
            .map(|(n, t)| (n.as_str(), t.as_str()))
            .collect();
        let report = byte_binding_lint::analyse(&sources);
        println!(
            "{dir}: {} slots, {} bound, {} flagged",
            report.slots.len(),
            report.bound.len(),
            report.flagged.len()
        );
        for (s, f) in &report.flagged {
            println!("  FLAG {s}.{f}");
        }
        // Run the real gate too, so this answers "would CI go red on that wall?"
        // rather than merely listing flags.
        match byte_binding_lint::check(&sources, FORMAT_DOC) {
            Ok(_) => println!("\nGATE: PASS (every flagged field has an Allowance)"),
            Err(failure) => println!("\nGATE: FAIL\n{failure}"),
        }
    }

    #[test]
    fn wall_sources_have_unique_plain_filenames() {
        let mut names = SOURCES.iter().map(|source| source.name).collect::<Vec<_>>();
        names.sort_unstable();
        names.dedup();
        assert_eq!(names.len(), SOURCES.len());
        assert!(
            names
                .iter()
                .all(|name| name.ends_with(".lean") && !name.contains('/'))
        );
    }

    #[test]
    fn current_wall_resolves_only_by_exact_id() {
        assert_eq!(compute_id(), CURRENT_ID);
        assert!(std::ptr::eq(resolve(current_id()).unwrap(), &CURRENT));
        assert!(resolve("sha256:deadbeef").is_none());
    }
}
