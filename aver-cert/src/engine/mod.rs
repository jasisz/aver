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
//! The compiler prints every emitted function's optimized MIR body 1:1 into
//! the one plan grammar (`plan.rs`); the producer here checks each plan
//! against the exact module bytes (`plan_check.rs` twins of the wall's
//! lowering) and declines per function whatever does not match. The offered
//! plans are rendered as Lean data in `Plans.lean`; the checker-owned wall
//! lowers them again and pins the result to the artifact bytes.

// Two layers: with only the `plans` feature, the plan data types the compiler
// prints into; under `engine` (which implies `plans`), the byte facts, the
// producer, the Lean renderer and the embedded wall re-export. Everything
// stays a flat `aver_cert::*` item.
#[cfg(feature = "engine")]
use sha2::{Digest, Sha256};
#[cfg(feature = "engine")]
use std::collections::{BTreeMap, BTreeSet, HashMap};
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
/// The one report class of a certified export.
pub const PLAN_CLASS: &str = crate::format::PLAN_CLASS;
/// The discharge theorem every certified export names.
pub const FN_CLAIM_DISCHARGE_THEOREM: &str = crate::format::FN_CLAIM_DISCHARGE_THEOREM;
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

#[cfg(feature = "engine")]
fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

// Plan surface (`plans` feature): the one-grammar plan data.
include!("plan.rs");

// The producer (`engine` feature).
#[cfg(feature = "engine")]
include!("module_facts.rs");
#[cfg(feature = "engine")]
include!("plan_check.rs");
#[cfg(feature = "engine")]
include!("produce.rs");
#[cfg(feature = "engine")]
include!("module_envelope.rs");
#[cfg(feature = "engine")]
include!("source_bridges.rs");
#[cfg(feature = "engine")]
include!("render_package.rs");
include!("law_claims.rs");
