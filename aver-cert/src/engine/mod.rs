//! Stage B artifact-certificate emitter: `aver compile --target wasm-gc --certify`.
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
//! `aver cert verify` re-runs the audited Rust byte pipeline as a fail-fast,
//! then the accepted-artifact witness uses `CertDecode` to compute
//! non-expression code/carrier/struct facts from `ArtifactBytes` in-kernel.
//! Host/self byte facts are now fully kernel-bound; Rust rederivation and
//! validation remain non-trust-bearing fail-fast checks. Expression
//! fragments emit a canonical plan sidecar under `cert/fragments/`; verify
//! parses that untrusted plan, typechecks it against byte-derived function facts,
//! canonically lowers it to raw code-entry bytes, and only then uses the checked
//! plan to render the witness code/face. The complete disassembler and role
//! classifier are not retired by this S1 step.

use sha2::{Digest, Sha256};
use std::path::Path;

pub use crate::wall;
pub use crate::wall::*;

/// Emitted-fragment profile and runtime ABI identifiers recorded in the
/// manifest. Stable strings the checker echoes; bumped when the certified
/// fragment or the runtime import surface changes.
pub const PROFILE_ID: &str = "AverUserProfile/v0";
pub const RUNTIME_ABI: &str = "aver-wasm-gc/0";
/// Certification level of a v0 artifact certificate: conditional on the named
/// runtime contracts (see the consult level naming L0/L1/L2/L3).
pub const CERT_LEVEL: &str = "L1";
pub const CERT_SCHEMA_VERSION: u32 = 1;
pub const BOX_CONTRACT: &str = "__rt_aint_from_i64 (box i64 -> carrier)";
pub const INT_ADD_CONTRACT: &str =
    "Int.add (carrier add = exact integer addition on represented values)";
pub const INT_SUB_CONTRACT: &str =
    "Int.sub (carrier sub = exact integer subtraction on represented values)";
pub const INT_MUL_CONTRACT: &str =
    "Int.mul (carrier mul = exact integer multiplication on represented values)";
pub const INT_ADD_TOTAL_CONTRACT: &str = "Int.add (carrier add = exact integer addition on represented values); total on represented values";
pub const INT_SUB_TOTAL_CONTRACT: &str = "Int.sub (carrier sub = exact integer subtraction on represented values); total on represented values";
pub const INT_MUL_TOTAL_CONTRACT: &str = "Int.mul (carrier mul = exact integer multiplication on represented values); total on represented values";
pub const STRING_EQ_CONTRACT: &str =
    "String.eq (WVal byte-array equality; non-arrays compare false)";
pub const STRING_CONCAT_CONTRACT: &str =
    "String.concat (container-of-string-arrays -> byte-concatenated array)";
/// The one approved final-theorem statement line. `aver cert verify` confirms
/// this exact line is present in `Final.lean` (name + `Holds manifest`), which
/// is what pins the statement without matching arbitrary Lean syntax.
pub const FINAL_THEOREM: &str = "AverCert.Final.cert";
pub const FINAL_STATEMENT_LINE: &str =
    "theorem AverCert.Final.cert : AverCert.Schema.Holds manifest";
/// The artifact-level theorem root that v2-style consumers should check. It
/// packages the final schema theorem through `AcceptedArtifact.accepted`.
pub const ARTIFACT_CERTIFICATE_ROOT: &str = "AverCert.Artifact.certificate";

/// sha256 of a byte slice, lowercase hex.
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex(&h.finalize())
}

include!("core_wasm.rs");
include!("core_shapes.rs");
include!("expr_fragment_defs.rs");
include!("sym_plan_defs.rs");
include!("sym_plan_render.rs");
include!("sym_plan_encode.rs");
include!("cert_defs.rs");
include!("recursion_plan_defs.rs");
include!("mutual_plan_defs.rs");
include!("composition_plan_defs.rs");
include!("verbatim_plan_defs.rs");
include!("int_dispatch_plan_defs.rs");
include!("string_plan_defs.rs");
include!("construct_plan_defs.rs");
include!("field_projection_plan_defs.rs");
include!("cert_methods.rs");
include!("analysis.rs");
include!("module_envelope.rs");
include!("rederive.rs");
include!("disasm.rs");
include!("classification.rs");
include!("model_eval.rs");
include!("render.rs");
