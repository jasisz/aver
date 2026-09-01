//! Stage B artifact-certificate emitter for wasm-gc modules and wasip2 components.
//!
//! Emits, next to `<name>.wasm`, an artifact-specific `cert/` package. The
//! package names its checker-owned Lean soundness wall by `format.wall_id`;
//! `aver cert verify` resolves the exact embedded wall and authors a fresh
//! build instead of trusting or duplicating build infrastructure in the cert.
//!
//! Everything else is FAIL-CLOSED: listed in `cert-manifest.json` as
//! `source-level-only` with a reason. No weaker theorem is ever emitted.
//!
//! Certified-function bodies are read back from the module bytes the compiler
//! just emitted, checked against the admitted profiles, and re-rendered as
//! `CertPrelude.WInstr` data. Any body that cannot be bound to an admitted
//! obligation is declined rather than assigned a weaker theorem.
//!
//! `aver cert verify` performs standard Wasm validation, then the accepted-
//! artifact witness uses `CertDecode` to compute code/carrier/struct facts from
//! `ArtifactBytes` in-kernel. Rust classification and rederivation remain
//! producer diagnostics only. Expression plans are emitted once,
//! as Lean data in `Plans.lean`; the checker-owned wall validates and lowers
//! that data against the exact artifact bytes. Redundant text sidecars are not
//! part of the public certificate package.

// This module compiles in two layers. With only the `plans` feature the
// plan-surface files below are compiled: the fragment/sym plan IR types and
// the canonical byte lowering the wasm-gc emitter needs in every build. The
// full certificate engine — byte classifier, rederiver, Lean renderer, and
// the embedded soundness wall re-export — is additionally compiled under the
// `engine` feature (which implies `plans`). External paths are unchanged:
// everything stays a flat `aver_cert::*` item.
#[cfg(feature = "engine")]
use sha2::{Digest, Sha256};
#[cfg(feature = "engine")]
use std::path::Path;

#[cfg(feature = "engine")]
pub use crate::wall;
#[cfg(feature = "engine")]
pub use crate::wall::*;

/// Artifact target identifier recorded in the manifest.
pub const ARTIFACT_TARGET: &str = crate::format::TARGET_WASM_GC;
/// Emitted-fragment profile and runtime ABI identifiers recorded in the
/// manifest. Stable strings the checker verifies; bumped when the certified
/// fragment or the runtime import surface changes.
pub const PROFILE_ID: &str = crate::format::PROFILE_ID;
pub const RUNTIME_ABI: &str = crate::format::RUNTIME_ABI_WASM_GC;
/// Conditional simulation under the runtime contracts named by the claim.
pub const CERT_LEVEL: &str = "L1";
pub const CERT_SCHEMA_VERSION: u32 = crate::format::CERT_SCHEMA_VERSION;
pub const BOX_CONTRACT: &str = "__rt_aint_from_i64 (box i64 -> carrier)";
pub const INT_ADD_CONTRACT: &str =
    "Int.add (carrier add = exact integer addition on represented values; result canonical)";
pub const INT_SUB_CONTRACT: &str =
    "Int.sub (carrier sub = exact integer subtraction on represented values; result canonical)";
pub const INT_MUL_CONTRACT: &str =
    "Int.mul (carrier mul = exact integer multiplication on represented values; result canonical)";
pub const INT_ADD_TOTAL_CONTRACT: &str = "Int.add (carrier add = exact integer addition on represented values; result canonical); total on represented values";
pub const INT_SUB_TOTAL_CONTRACT: &str = "Int.sub (carrier sub = exact integer subtraction on represented values; result canonical); total on represented values";
pub const INT_MUL_TOTAL_CONTRACT: &str = "Int.mul (carrier mul = exact integer multiplication on represented values; result canonical); total on represented values";
pub const STRING_EQ_CONTRACT: &str =
    "String.eq (WVal byte-array equality; non-arrays compare false)";
pub const STRING_CONCAT_CONTRACT: &str =
    "String.concat (container-of-string-arrays -> byte-concatenated array)";
pub const TO_INDEX_CONTRACT: &str =
    "__aint_to_index (carrier -> i32 array index; [0, 2^31) passes, else -1)";
/// The two Int comparison helper contracts. Byte-identical twins of
/// `ClaimAxes.cmpContract` / `ClaimAxes.eqContract`; `contractsMatch` compares
/// the manifest list against the wall's own, so a drift here fails closed.
pub const CMP_CONTRACT: &str =
    "__aint_cmp (canonical carrier pair -> i32 sign; -1 less, 0 equal, 1 greater)";
pub const EQ_CONTRACT: &str =
    "__aint_eq (canonical carrier pair -> i32 boolean; 1 when equal, else 0)";
/// The one approved final-theorem statement line. `aver cert verify` confirms
/// this exact line is present in `Final.lean` (name + `Holds manifest`), which
/// is what pins the statement without matching arbitrary Lean syntax.
pub const FINAL_THEOREM: &str = "AverCert.Final.cert";
pub const FINAL_STATEMENT_LINE: &str =
    "theorem AverCert.Final.cert : AverCert.Schema.Holds manifest";
/// The artifact-level theorem root consumed by the standalone verifier. It
/// packages the final schema theorem through `AcceptedArtifact.accepted`.
pub const ARTIFACT_CERTIFICATE_ROOT: &str = "AverCert.Artifact.certificate";

/// sha256 of a byte slice, lowercase hex.
#[cfg(feature = "engine")]
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex(&h.finalize())
}

// Plan surface (`plans` feature): plan IR types, the SymPlan -> ExprFragmentPlan
// encoder, and the canonical byte lowering the wasm-gc emitter calls at emit
// time.
include!("expr_fragment_defs.rs");
include!("expr_fragment_faces.rs");
include!("sym_plan_defs.rs");
include!("sym_plan_encode.rs");
include!("classify_expr_fragment_lower.rs");

// Full certificate engine (`engine` feature): byte-derived classification,
// rederivation, Lean rendering, and everything that references the wall.
#[cfg(feature = "engine")]
include!("core_wasm.rs");
#[cfg(feature = "engine")]
include!("core_shapes.rs");
#[cfg(feature = "engine")]
include!("sym_plan_render.rs");
#[cfg(feature = "engine")]
include!("cert_defs.rs");
#[cfg(feature = "engine")]
include!("recursion_plan_defs.rs");
#[cfg(feature = "engine")]
include!("mutual_plan_defs.rs");
#[cfg(feature = "engine")]
include!("composition_plan_defs.rs");
#[cfg(feature = "engine")]
include!("verbatim_plan_defs.rs");
#[cfg(feature = "engine")]
include!("int_dispatch_plan_defs.rs");
#[cfg(feature = "engine")]
include!("string_plan_defs.rs");
#[cfg(feature = "engine")]
include!("construct_plan_defs.rs");
#[cfg(feature = "engine")]
include!("field_projection_plan_defs.rs");
#[cfg(feature = "engine")]
include!("cert_methods.rs");
#[cfg(feature = "engine")]
include!("analysis.rs");
#[cfg(feature = "engine")]
include!("module_envelope.rs");
#[cfg(feature = "engine")]
include!("declared_envelope.rs");
include!("law_claims.rs");
#[cfg(feature = "engine")]
include!("rederive.rs");
#[cfg(feature = "engine")]
include!("disasm.rs");
#[cfg(feature = "engine")]
include!("classification.rs");
#[cfg(feature = "engine")]
include!("model_eval.rs");
#[cfg(feature = "engine")]
include!("render.rs");
