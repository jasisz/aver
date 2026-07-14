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
pub const CERT_FIELD_PROJECTION_SOUNDNESS: &str =
    include_str!("../assets/wall/current/FieldProjectionSoundness.lean");
pub const CERT_CONSTRUCT_VERBATIM_SOUNDNESS: &str =
    include_str!("../assets/wall/current/ConstructVerbatimSoundness.lean");
pub const CERT_INT_DISPATCH_SOUNDNESS: &str =
    include_str!("../assets/wall/current/IntDispatchSoundness.lean");
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Source {
    pub name: &'static str,
    pub contents: &'static str,
}

/// Exact checker-owned source set. Ordering is not part of the identity:
/// [`compute_id`] sorts by filename before hashing.
pub const SOURCES: [Source; 33] = [
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
];

/// Roots whose complete import graph is artifact-independent and can therefore
/// be cached before a certificate is seen.
pub const PRISTINE_ROOTS: [&str; 31] = [
    "CertPrelude",
    "CertDecode",
    "WasmSlice",
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
    "FieldProjectionSoundness",
    "ConstructVerbatimSoundness",
    "IntDispatchSoundness",
    "StringSoundness",
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
