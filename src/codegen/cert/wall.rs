//! Checker-owned Lean soundness wall.
//!
//! These sources are artifact-independent and embedded in the verifier. A
//! certificate names the exact set through [`current_id`]; it never chooses a
//! path, URL, or ambient installation from which the verifier loads code.

use sha2::{Digest, Sha256};
use std::sync::OnceLock;

/// Public certificate-package format understood by this verifier.
pub const FORMAT_VERSION: u32 = 1;
pub const CURRENT_ID: &str =
    "sha256:4d5d1b61e1d6e2544fd0aed769349bd9d98dcc54fd9c27bb8591caa0f4f0dddb";

pub const LEAN_TOOLCHAIN: &str = include_str!("../../../tools/certkit/prelude/lean-toolchain");

pub const CERT_PRELUDE: &str = include_str!("../../../tools/certkit/prelude/CertPrelude.lean");
pub const CERT_DECODE: &str = include_str!("../../../tools/certkit/prelude/CertDecode.lean");
pub const CERT_SCHEMA: &str = include_str!("Schema.lean");
pub const CERT_SCHEMA_CORE: &str = include_str!("SchemaCore.lean");
pub const CERT_PLAN_CHECK: &str = include_str!("PlanCheck.lean");
pub const CERT_PLAN_LOWER: &str = include_str!("PlanLower.lean");
pub const CERT_PLAN_BYTES: &str = include_str!("PlanBytes.lean");
pub const CERT_WASM_SLICE: &str = include_str!("WasmSlice.lean");
pub const CERT_EXPR_FRAGMENT_ACCEPTED: &str = include_str!("ExprFragmentAccepted.lean");
pub const CERT_ACCEPTED_ARTIFACT: &str = include_str!("AcceptedArtifact.lean");
pub const CERT_ACCEPTED_ARTIFACT_CORE: &str = include_str!("AcceptedArtifactCore.lean");
pub const CERT_EXPR_FRAGMENT_SEMANTICS: &str = include_str!("ExprFragmentSemantics.lean");
pub const CERT_INTERPRETER_SEQUENCING: &str = include_str!("InterpreterSequencing.lean");
pub const CERT_EXPR_FRAGMENT_SOUNDNESS: &str = include_str!("ExprFragmentSoundness.lean");
pub const CERT_FIELD_PROJECTION_SOUNDNESS: &str = include_str!("FieldProjectionSoundness.lean");
pub const CERT_CONSTRUCT_VERBATIM_SOUNDNESS: &str = include_str!("ConstructVerbatimSoundness.lean");
pub const CERT_INT_DISPATCH_SOUNDNESS: &str = include_str!("IntDispatchSoundness.lean");
pub const CERT_STRING_SOUNDNESS: &str = include_str!("StringSoundness.lean");
pub const CERT_RECURSION_SOUNDNESS: &str = include_str!("RecursionSoundness.lean");
pub const CERT_MUTUAL_RECURSION_SOUNDNESS: &str = include_str!("MutualRecursionSoundness.lean");
pub const CERT_COMPOSITION_SOUNDNESS: &str = include_str!("CompositionSoundness.lean");
pub const CERT_ACCEPTANCE_SOUNDNESS_CORE: &str = include_str!("AcceptanceSoundnessCore.lean");
pub const CERT_DISCHARGE_EXPR_FRAGMENT: &str = include_str!("DischargeExprFragment.lean");
pub const CERT_DISCHARGE_FIELD_PROJECTION: &str = include_str!("DischargeFieldProjection.lean");
pub const CERT_DISCHARGE_CONSTRUCT: &str = include_str!("DischargeConstruct.lean");
pub const CERT_DISCHARGE_VERBATIM: &str = include_str!("DischargeVerbatim.lean");
pub const CERT_DISCHARGE_STRING: &str = include_str!("DischargeString.lean");
pub const CERT_DISCHARGE_INT_DISPATCH: &str = include_str!("DischargeIntDispatch.lean");
pub const CERT_DISCHARGE_RECURSION: &str = include_str!("DischargeRecursion.lean");
pub const CERT_DISCHARGE_COMPOSITION: &str = include_str!("DischargeComposition.lean");
pub const CERT_ACCEPTANCE_SOUNDNESS: &str = include_str!("AcceptanceSoundness.lean");

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Source {
    pub name: &'static str,
    pub contents: &'static str,
}

/// Exact checker-owned source set. Ordering is not part of the identity:
/// [`compute_id`] sorts by filename before hashing.
pub const SOURCES: [Source; 31] = [
    Source {
        name: "AcceptedArtifact.lean",
        contents: CERT_ACCEPTED_ARTIFACT,
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
        name: "CompositionSoundness.lean",
        contents: CERT_COMPOSITION_SOUNDNESS,
    },
    Source {
        name: "ConstructVerbatimSoundness.lean",
        contents: CERT_CONSTRUCT_VERBATIM_SOUNDNESS,
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
        name: "StringSoundness.lean",
        contents: CERT_STRING_SOUNDNESS,
    },
    Source {
        name: "WasmSlice.lean",
        contents: CERT_WASM_SLICE,
    },
];

/// Roots whose complete import graph is artifact-independent and can therefore
/// be cached before a certificate is seen.
pub const PRISTINE_ROOTS: [&str; 29] = [
    "CertPrelude",
    "CertDecode",
    "WasmSlice",
    "SchemaCore",
    "PlanCheck",
    "PlanLower",
    "PlanBytes",
    "ExprFragmentAccepted",
    "AcceptedArtifactCore",
    "ExprFragmentSemantics",
    "InterpreterSequencing",
    "ExprFragmentSoundness",
    "FieldProjectionSoundness",
    "ConstructVerbatimSoundness",
    "IntDispatchSoundness",
    "StringSoundness",
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

#[cfg(test)]
mod tests {
    use super::*;

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
