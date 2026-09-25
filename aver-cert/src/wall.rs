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
pub const CERT_SCHEMA_BASE: &str = include_str!("../assets/wall/current/SchemaBase.lean");
pub const CERT_SCHEMA: &str = include_str!("../assets/wall/current/Schema.lean");
pub const CERT_SCHEMA_CORE: &str = include_str!("../assets/wall/current/SchemaCore.lean");
pub const CERT_WASM_SLICE: &str = include_str!("../assets/wall/current/WasmSlice.lean");
pub const CERT_WASIP2_ENVELOPE: &str = include_str!("../assets/wall/current/Wasip2Envelope.lean");
pub const CERT_ARITH_TEMPLATE_DERISK: &str =
    include_str!("../assets/wall/current/ArithTemplateDerisk.lean");
pub const CERT_INTERPRETER_SEQUENCING: &str =
    include_str!("../assets/wall/current/InterpreterSequencing.lean");
pub const CERT_GRAMMAR: &str = include_str!("../assets/wall/current/Grammar.lean");
pub const CERT_GRAMMAR_LOWER: &str = include_str!("../assets/wall/current/GrammarLower.lean");
pub const CERT_GRAMMAR_SOUND: &str = include_str!("../assets/wall/current/GrammarSound.lean");
pub const CERT_GRAMMAR_TOTAL: &str = include_str!("../assets/wall/current/GrammarTotal.lean");
pub const CERT_TYPE_TABLE: &str = include_str!("../assets/wall/current/TypeTable.lean");
pub const CERT_ACCEPTED_ARTIFACT_CORE: &str =
    include_str!("../assets/wall/current/AcceptedArtifactCore.lean");
pub const CERT_ACCEPTED_ARTIFACT: &str =
    include_str!("../assets/wall/current/AcceptedArtifact.lean");
pub const CERT_DECLARED_LAYOUT: &str = include_str!("../assets/wall/current/DeclaredLayout.lean");
pub const CERT_BYTE_WINDOW: &str = include_str!("../assets/wall/current/ByteWindow.lean");
pub const CERT_SORTED_KEYS: &str = include_str!("../assets/wall/current/SortedKeys.lean");
pub const CERT_CLAIM_AXES: &str = include_str!("../assets/wall/current/ClaimAxes.lean");
pub const CERT_ACCEPTANCE_SOUNDNESS_CORE: &str =
    include_str!("../assets/wall/current/AcceptanceSoundnessCore.lean");
pub const CERT_ACCEPTANCE_SOUNDNESS: &str =
    include_str!("../assets/wall/current/AcceptanceSoundness.lean");
pub const CERT_GRAMMAR_BRIDGE: &str = include_str!("../assets/wall/current/GrammarBridge.lean");

/// The checker-owned pieces of the certificate source model (`AverBits` with
/// its `@[simp]` equations, the `aver_int_order` tactic): the constructs the
/// token gate refuses in package text, owned and pinned here instead.
pub const CERT_MODEL_PRELUDE: &str = include_str!("../assets/wall/current/ModelPrelude.lean");

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Source {
    pub name: &'static str,
    pub contents: &'static str,
}

/// Exact checker-owned source set. Ordering is not part of the identity:
/// [`compute_id`] sorts by filename before hashing.
pub const SOURCES: [Source; 24] = [
    Source {
        name: "AcceptanceSoundness.lean",
        contents: CERT_ACCEPTANCE_SOUNDNESS,
    },
    Source {
        name: "AcceptanceSoundnessCore.lean",
        contents: CERT_ACCEPTANCE_SOUNDNESS_CORE,
    },
    Source {
        name: "AcceptedArtifact.lean",
        contents: CERT_ACCEPTED_ARTIFACT,
    },
    Source {
        name: "AcceptedArtifactCore.lean",
        contents: CERT_ACCEPTED_ARTIFACT_CORE,
    },
    Source {
        name: "ArithTemplateDerisk.lean",
        contents: CERT_ARITH_TEMPLATE_DERISK,
    },
    Source {
        name: "ByteWindow.lean",
        contents: CERT_BYTE_WINDOW,
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
        name: "DeclaredLayout.lean",
        contents: CERT_DECLARED_LAYOUT,
    },
    Source {
        name: "Grammar.lean",
        contents: CERT_GRAMMAR,
    },
    Source {
        name: "GrammarBridge.lean",
        contents: CERT_GRAMMAR_BRIDGE,
    },
    Source {
        name: "GrammarLower.lean",
        contents: CERT_GRAMMAR_LOWER,
    },
    Source {
        name: "GrammarSound.lean",
        contents: CERT_GRAMMAR_SOUND,
    },
    Source {
        name: "GrammarTotal.lean",
        contents: CERT_GRAMMAR_TOTAL,
    },
    Source {
        name: "InterpreterSequencing.lean",
        contents: CERT_INTERPRETER_SEQUENCING,
    },
    Source {
        name: "ModelPrelude.lean",
        contents: CERT_MODEL_PRELUDE,
    },
    Source {
        name: "Schema.lean",
        contents: CERT_SCHEMA,
    },
    Source {
        name: "SchemaBase.lean",
        contents: CERT_SCHEMA_BASE,
    },
    Source {
        name: "SchemaCore.lean",
        contents: CERT_SCHEMA_CORE,
    },
    Source {
        name: "SortedKeys.lean",
        contents: CERT_SORTED_KEYS,
    },
    Source {
        name: "TypeTable.lean",
        contents: CERT_TYPE_TABLE,
    },
    Source {
        name: "Wasip2Envelope.lean",
        contents: CERT_WASIP2_ENVELOPE,
    },
    Source {
        name: "WasmSlice.lean",
        contents: CERT_WASM_SLICE,
    },
];

/// Roots whose complete import graph is artifact-independent and can therefore
/// be cached before a certificate is seen.
pub const PRISTINE_ROOTS: [&str; 22] = [
    "CertPrelude",
    "CertDecode",
    "ByteWindow",
    "ArithTemplateDerisk",
    "WasmSlice",
    "Wasip2Envelope",
    "SchemaBase",
    "SchemaCore",
    "InterpreterSequencing",
    "Grammar",
    "GrammarLower",
    "GrammarSound",
    "GrammarTotal",
    "TypeTable",
    "AcceptedArtifactCore",
    "DeclaredLayout",
    "SortedKeys",
    "ClaimAxes",
    "AcceptanceSoundnessCore",
    "AcceptanceSoundness",
    "GrammarBridge",
    "ModelPrelude",
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

/// Checker-authored `Module.lean`: the SHA-256 of the delivered artifact that
/// `Schema.Holds` compares the manifest's hash against. The wall's `Schema`
/// imports this module, so it must never come from a certificate package: a
/// package module in the wall's import closure could declare names that the
/// wall's own definitions resolve to. `sha` is the hash the verifier computed
/// from the bytes it read (64 lowercase hex digits).
pub fn render_module(sha: &str) -> String {
    debug_assert!(
        sha.len() == 64 && sha.bytes().all(|b| b.is_ascii_hexdigit()),
        "render_module takes a hex SHA-256"
    );
    format!(
        "-- Authored by aver-cert from the artifact bytes; never accepted from the certificate.\n\
         namespace CertModule\n\n\
         /-- SHA-256 of the delivered artifact, computed by the verifier. -/\n\
         def wasmSha256 : String := \"{sha}\"\n\n\
         end CertModule\n"
    )
}

/// Bytes per hex numeral in a checker-rendered byte module.
const BYTE_NUMERAL_CHUNK: usize = 1024;

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
        // Lean reads a numeral in time quadratic in its length (a 116 KiB
        // module's single hex numeral took 20 s to elaborate, once per byte
        // module). Chunks of `BYTE_NUMERAL_CHUNK` bytes, each shifted to its
        // byte offset and joined by `|||`, denote the same number: the ranges
        // are disjoint, and the kernel evaluates the join with its built-in
        // `Nat` shift and `lor`.
        bytes
            .chunks(BYTE_NUMERAL_CHUNK)
            .enumerate()
            .map(|(index, chunk)| {
                let mut hex = String::with_capacity(2 + chunk.len() * 2);
                hex.push_str("0x");
                for byte in chunk.iter().rev() {
                    hex.push_str(&format!("{byte:02x}"));
                }
                match index {
                    0 => hex,
                    _ => format!("({hex} <<< {})", 8 * BYTE_NUMERAL_CHUNK * index),
                }
            })
            .collect::<Vec<_>>()
            .join(" |||\n  ")
    };
    format!(
        "import WasmSlice\n\nset_option maxRecDepth 200000\n\nnamespace AverCert.{module}\n\n/-- {description} -/\nnoncomputable def {bytes_name} : Nat :=\n  {numeral}\ndef {len_name} : Nat := {}\n\nend AverCert.{module}\n",
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
        const NAME_PIN: &str = "(roles.toIndex == _root_.CertDecode.AddSub.toIndexIdx n len) &&";
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

    /// The wall with one text replacement in `AcceptedArtifactCore.lean`.
    fn wall_with_core_edit(old: &str, new: &str) -> String {
        assert!(
            CERT_ACCEPTED_ARTIFACT_CORE.contains(old),
            "the edited text has moved ({old:?}); re-aim this regression test rather than \
             deleting it"
        );
        CERT_ACCEPTED_ARTIFACT_CORE.replace(old, new)
    }

    fn sources_with_core(core: &str) -> Vec<(&'static str, &str)> {
        wall_sources()
            .into_iter()
            .map(|(name, text)| {
                if name == "AcceptedArtifactCore.lean" {
                    (name, core)
                } else {
                    (name, text)
                }
            })
            .collect()
    }

    /// Removing the code-entry equality must leave the plan payload unbound.
    ///
    /// The equality `exactFuncBindingForExport n len name bytes` (and the
    /// `codeEntry == bytes` filter for internal callees) is the one fact that
    /// makes a plan's lowering the delivered code. An earlier lint counted
    /// `planTyped M e.plan` and `callsOrdered fns e` as binding the plan,
    /// because they sit in a definition whose `match` reads the bytes, and so
    /// stayed green with the equality deleted. The function type pin
    /// (`sigPinned`) is the control: it still binds the signature.
    #[test]
    fn lint_flags_the_removed_code_entry_pin() {
        let core = wall_with_core_edit(
            "        _root_.AverCert.WasmSlice.exactFuncBindingForExport n len (stringBytes e.name) bytes\n      else\n        (_root_.AverCert.WasmSlice.funcBindingByFuncIndex n len e.funcIdx).filter\n          (fun b => b.codeEntry == bytes)\n",
            "        AverCert.WasmSlice.funcBindingForExport n len (stringBytes e.name)\n      else\n        AverCert.WasmSlice.funcBindingByFuncIndex n len e.funcIdx\n",
        );
        let sources = sources_with_core(&core);
        let report = byte_binding_lint::analyse(&sources);
        for field in ["body", "locals", "nslots"] {
            assert!(
                report.is_flagged("FnPlan", field),
                "`FnPlan.{field}` is still considered bound with the code-entry equality \
                 removed"
            );
        }
        assert!(
            report.is_bound("Sig", "params") && report.is_bound("Sig", "ret"),
            "control failed: the function type pin `sigPinned` must still bind the signature"
        );
        assert!(
            byte_binding_lint::check(&sources, FORMAT_DOC).is_err(),
            "the lint gate passes a wall whose plans are not bound to their code"
        );
    }

    /// Rule D must not accept a derivation that is a tautology.
    ///
    /// `startPin m := m.subject = subjectOfManifest m` with
    /// `subjectOfManifest m := m.subject` has a producer value whole on one side
    /// and a wall definition applied on the other, and its right-hand side does
    /// not contain the text `m.subject`; a text-level rule D accepted it and so
    /// bound every leaf of `Subject`, with the start-section pin deleted. The
    /// argument `m` is an ancestor of the value, so D must not fire.
    #[test]
    fn lint_rejects_a_tautological_derivation() {
        let core = wall_with_core_edit(
            "def startAccounted (artifact : ArtifactData) : Bool :=\n  _root_.AverCert.WasmSlice.startFuncIndex artifact.modBytes artifact.modLen ==\n    some artifact.manifest.subject.start\n",
            "def subjectOfManifest (m : AverCert.Schema.Manifest) : AverCert.Schema.Subject := m.subject\n\ndef startPin (m : AverCert.Schema.Manifest) : Prop :=\n  m.subject = subjectOfManifest m\n\ndef startAccounted (artifact : ArtifactData) : Prop :=\n  startPin artifact.manifest\n",
        );
        let core = core.replace(
            "  startAccounted artifact = true ∧\n",
            "  startAccounted artifact ∧\n",
        );
        let sources = sources_with_core(&core);
        let report = byte_binding_lint::analyse(&sources);
        assert!(
            report.is_flagged("Subject", "start"),
            "`Subject.start` is considered bound by a tautological derivation"
        );
        assert!(
            report
                .bound
                .values()
                .all(|e| !(e.rule.starts_with('D') && e.decl.ends_with("startPin"))),
            "rule D fired on `m.subject = subjectOfManifest m`"
        );
        assert!(
            byte_binding_lint::check(&sources, FORMAT_DOC).is_err(),
            "the lint gate passes a wall whose start pin was replaced by a tautology"
        );

        // On the real wall, rule D fires exactly at `obligationsDerived`.
        let clean = byte_binding_lint::analyse(&wall_sources());
        let d_sites: std::collections::BTreeSet<&str> = clean
            .bound
            .values()
            .filter(|e| e.rule.starts_with('D'))
            .map(|e| e.decl.as_str())
            .collect();
        assert_eq!(
            d_sites.into_iter().collect::<Vec<_>>(),
            vec!["AverCert.AcceptedArtifact.obligationsDerived"],
            "rule D fires somewhere other than the derived obligations"
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
