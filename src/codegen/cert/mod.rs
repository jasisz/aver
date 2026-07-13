//! Stage B artifact-certificate emitter: `aver compile --target wasm-gc --certify`.
//!
//! Emits, next to `<name>.wasm`, a self-contained Lean `cert/` project that
//! `lake build`s green with kernel-clean theorems for the user functions that
//! fall into the three measured classes:
//!
//! * straight-line `Int -> Int` add-a-constant (the `addTwo` kill-fast shape),
//! * single-argument self-recursion of the `sumTo` shape
//!   (`match n <= 0 { true -> 0; false -> n + f(n - 1) }`),
//! * two-argument accumulator self-recursion of the `countDown` shape
//!   (`match n <= 0 { true -> acc; false -> f(n - 1, acc + n) }`).
//!
//! Everything else is FAIL-CLOSED: listed in `cert-manifest.json` as
//! `source-level-only` with a reason. No weaker theorem is ever emitted.
//!
//! The certified-function bodies are read back from the module bytes the
//! compiler just emitted (the same bytes whose sha256 the certificate pins),
//! matched against the two structural templates, and re-rendered as
//! `CertPrelude.WInstr` data. A function whose real emitted body does not match
//! a template is declined — so the `WInstr` data in `Module.lean` is exactly
//! the shape present in the hashed bytes.
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

/// The Stage-A semantics prelude, single source of truth, embedded so the
/// emitter is self-contained.
pub const CERT_PRELUDE: &str = include_str!("../../../tools/certkit/prelude/CertPrelude.lean");
pub const CERT_DECODE: &str = include_str!("../../../tools/certkit/prelude/CertDecode.lean");
pub const LEAN_TOOLCHAIN: &str = include_str!("../../../tools/certkit/prelude/lean-toolchain");

/// The audited statement schema, single source of truth, embedded so both the
/// emitter and the `aver cert verify` checker pin the exact same bytes. The
/// consumer trusts the certificate by checking the final theorem NAME, the
/// manifest LITERAL, and the hashes of the audited core/shim files plus the
/// prelude — never Lean proof syntax. Fixed content (no per-build parts) so
/// every sha256 is known to the checker at compile time.
pub const CERT_SCHEMA: &str = include_str!("Schema.lean");
pub const CERT_SCHEMA_CORE: &str = include_str!("SchemaCore.lean");
pub const CERT_PLAN_CHECK: &str = include_str!("PlanCheck.lean");
pub const CERT_PLAN_LOWER: &str = include_str!("PlanLower.lean");
pub const CERT_PLAN_BYTES: &str = include_str!("PlanBytes.lean");
pub const CERT_WASM_SLICE: &str = include_str!("WasmSlice.lean");
pub const CERT_EXPR_FRAGMENT_ACCEPTED: &str = include_str!("ExprFragmentAccepted.lean");
pub const CERT_ACCEPTED_ARTIFACT: &str = include_str!("AcceptedArtifact.lean");
pub const CERT_ACCEPTED_ARTIFACT_CORE: &str = include_str!("AcceptedArtifactCore.lean");
pub const CERT_V3_EXPR_FRAGMENT_FULL: &str = include_str!("V3ExprFragmentFull.lean");
pub const CERT_V3_STRONG_FUEL: &str = include_str!("V3StrongFuel.lean");
pub const CERT_V3_IF_ELSE: &str = include_str!("V3IfElse.lean");
pub const CERT_V3_GENERIC_CERTIFIED: &str = include_str!("V3GenericCertified.lean");
pub const CERT_V3_FIELD_PROJ: &str = include_str!("V3FieldProj.lean");
pub const CERT_V3_CONSTRUCT_VERBATIM: &str = include_str!("V3ConstructVerbatim.lean");
pub const CERT_V3_DISPATCH_CORE: &str = include_str!("V3DispatchCore.lean");
pub const CERT_V3_STRING: &str = include_str!("V3String.lean");
pub const CERT_V3_REC_SPIKE: &str = include_str!("V3RecSpike.lean");
pub const CERT_V3_MUTUAL_GENERIC: &str = include_str!("V3MutualGeneric.lean");
pub const CERT_V3_COMPOSITION: &str = include_str!("V3Composition.lean");
pub const CERT_V3_MASTER: &str = include_str!("V3Master.lean");
pub const CERT_V3_DISCHARGE_EXPR_FRAGMENT: &str = include_str!("V3DischargeExprFragment.lean");
pub const CERT_V3_DISCHARGE_FIELD_PROJ: &str = include_str!("V3DischargeFieldProj.lean");
pub const CERT_V3_DISCHARGE_CONSTRUCT: &str = include_str!("V3DischargeConstruct.lean");
pub const CERT_V3_DISCHARGE_VERBATIM: &str = include_str!("V3DischargeVerbatim.lean");
pub const CERT_V3_DISCHARGE_STRING: &str = include_str!("V3DischargeString.lean");
pub const CERT_V3_DISCHARGE_INT_DISPATCH: &str = include_str!("V3DischargeIntDispatch.lean");
pub const CERT_V3_DISCHARGE_RECURSION: &str = include_str!("V3DischargeRecursion.lean");
pub const CERT_V3_DISCHARGE_COMPOSITION: &str = include_str!("V3DischargeComposition.lean");
pub const CERT_V3_ACCEPT_SOUND: &str = include_str!("V3AcceptSound.lean");

/// Emitted-fragment profile and runtime ABI identifiers recorded in the
/// manifest. Stable strings the checker echoes; bumped when the certified
/// fragment or the runtime import surface changes.
pub const PROFILE_ID: &str = "AverUserProfile/v0";
pub const RUNTIME_ABI: &str = "aver-wasm-gc/0";
/// Certification level of a v0 artifact certificate: conditional on the named
/// runtime contracts (see the consult level naming L0/L1/L2/L3).
pub const CERT_LEVEL: &str = "L1";
pub const CERT_SCHEMA_VERSION: u32 = 59;
pub const BOX_CONTRACT: &str = "__rt_aint_from_i64 (box i64 -> carrier)";
pub const INT_ADD_CONTRACT: &str =
    "Int.add (carrier add = exact integer addition on represented values)";
pub const INT_SUB_CONTRACT: &str =
    "Int.sub (carrier sub = exact integer subtraction on represented values)";
pub const INT_ADD_TOTAL_CONTRACT: &str = "Int.add (carrier add = exact integer addition on represented values); total on represented values";
pub const INT_SUB_TOTAL_CONTRACT: &str = "Int.sub (carrier sub = exact integer subtraction on represented values); total on represented values";
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

/// Content hashes of the audited Lean files as embedded in THIS binary — the
/// checker's anchor for the exact sources emitted with each certificate.
pub fn audited_schema_sha() -> String {
    sha256_hex(CERT_SCHEMA.as_bytes())
}
pub fn audited_schema_core_sha() -> String {
    sha256_hex(CERT_SCHEMA_CORE.as_bytes())
}
pub fn audited_prelude_sha() -> String {
    sha256_hex(CERT_PRELUDE.as_bytes())
}
pub fn audited_decode_sha() -> String {
    sha256_hex(CERT_DECODE.as_bytes())
}
pub fn audited_plan_check_sha() -> String {
    sha256_hex(CERT_PLAN_CHECK.as_bytes())
}
pub fn audited_plan_lower_sha() -> String {
    sha256_hex(CERT_PLAN_LOWER.as_bytes())
}
pub fn audited_plan_bytes_sha() -> String {
    sha256_hex(CERT_PLAN_BYTES.as_bytes())
}
pub fn audited_wasm_slice_sha() -> String {
    sha256_hex(CERT_WASM_SLICE.as_bytes())
}
pub fn audited_expr_fragment_accepted_sha() -> String {
    sha256_hex(CERT_EXPR_FRAGMENT_ACCEPTED.as_bytes())
}
pub fn audited_accepted_artifact_sha() -> String {
    sha256_hex(CERT_ACCEPTED_ARTIFACT.as_bytes())
}
pub fn audited_accepted_artifact_core_sha() -> String {
    sha256_hex(CERT_ACCEPTED_ARTIFACT_CORE.as_bytes())
}
pub fn audited_v3_expr_fragment_full_sha() -> String {
    sha256_hex(CERT_V3_EXPR_FRAGMENT_FULL.as_bytes())
}
pub fn audited_v3_strong_fuel_sha() -> String {
    sha256_hex(CERT_V3_STRONG_FUEL.as_bytes())
}
pub fn audited_v3_if_else_sha() -> String {
    sha256_hex(CERT_V3_IF_ELSE.as_bytes())
}
pub fn audited_v3_generic_certified_sha() -> String {
    sha256_hex(CERT_V3_GENERIC_CERTIFIED.as_bytes())
}
pub fn audited_v3_field_proj_sha() -> String {
    sha256_hex(CERT_V3_FIELD_PROJ.as_bytes())
}
pub fn audited_v3_construct_verbatim_sha() -> String {
    sha256_hex(CERT_V3_CONSTRUCT_VERBATIM.as_bytes())
}
pub fn audited_v3_dispatch_core_sha() -> String {
    sha256_hex(CERT_V3_DISPATCH_CORE.as_bytes())
}
pub fn audited_v3_string_sha() -> String {
    sha256_hex(CERT_V3_STRING.as_bytes())
}
pub fn audited_v3_rec_spike_sha() -> String {
    sha256_hex(CERT_V3_REC_SPIKE.as_bytes())
}
pub fn audited_v3_mutual_generic_sha() -> String {
    sha256_hex(CERT_V3_MUTUAL_GENERIC.as_bytes())
}
pub fn audited_v3_composition_sha() -> String {
    sha256_hex(CERT_V3_COMPOSITION.as_bytes())
}
pub fn audited_v3_master_sha() -> String {
    sha256_hex(CERT_V3_MASTER.as_bytes())
}
pub fn audited_v3_discharge_expr_fragment_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_EXPR_FRAGMENT.as_bytes())
}
pub fn audited_v3_discharge_field_proj_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_FIELD_PROJ.as_bytes())
}
pub fn audited_v3_discharge_construct_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_CONSTRUCT.as_bytes())
}
pub fn audited_v3_discharge_verbatim_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_VERBATIM.as_bytes())
}
pub fn audited_v3_discharge_string_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_STRING.as_bytes())
}
pub fn audited_v3_discharge_int_dispatch_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_INT_DISPATCH.as_bytes())
}
pub fn audited_v3_discharge_recursion_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_RECURSION.as_bytes())
}
pub fn audited_v3_discharge_composition_sha() -> String {
    sha256_hex(CERT_V3_DISCHARGE_COMPOSITION.as_bytes())
}
pub fn audited_v3_accept_sound_sha() -> String {
    sha256_hex(CERT_V3_ACCEPT_SOUND.as_bytes())
}

include!("core_wasm.rs");
include!("core_shapes.rs");
include!("expr_fragment_defs.rs");
include!("sym_plan_defs.rs");
include!("sym_plan_render.rs");
include!("sym_plan_encode.rs");
include!("expr_fragment_from_mir.rs");
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
