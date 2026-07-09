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
//! `aver cert verify` re-runs the audited byte pipeline on the hash-verified
//! bytes (`rederive_certificate`) and pins the checker-derived
//! `code`/`host`/`self`/`carrier` values plus consumed runtime contracts into
//! its checker-authored witness with `rfl` against the proven manifest — so the
//! `WInstr` data and contracts the kernel theorem actually reasons about are
//! forced to equal byte-bound checker values, not merely trusted. Expression
//! fragments emit a canonical plan sidecar under `cert/fragments/`; verify
//! parses that untrusted plan, typechecks it against byte-derived function facts,
//! canonically lowers it to raw code-entry bytes, and only then uses the checked
//! plan to render the witness code/face. This is trusted via inspection of the
//! disassembler/checker/lowerer, not by an in-kernel wasm decode proof (a full
//! kernel decoder is a deferred residual).

use sha2::{Digest, Sha256};
use std::path::Path;

/// The Stage-A semantics prelude, single source of truth, embedded so the
/// emitter is self-contained.
pub const CERT_PRELUDE: &str = include_str!("../../../tools/certkit/prelude/CertPrelude.lean");
pub const LEAN_TOOLCHAIN: &str = include_str!("../../../tools/certkit/prelude/lean-toolchain");

/// The audited statement schema, single source of truth, embedded so both the
/// emitter and the `aver cert verify` checker pin the exact same bytes. The
/// consumer trusts the certificate by checking the final theorem NAME, the
/// manifest LITERAL, and the hash of THIS file plus the prelude — never Lean
/// proof syntax. Fixed content (no per-build parts) so its sha256 is known to
/// the checker at compile time.
pub const CERT_SCHEMA: &str = include_str!("Schema.lean");
pub const CERT_PLAN_CHECK: &str = include_str!("PlanCheck.lean");
pub const CERT_PLAN_LOWER: &str = include_str!("PlanLower.lean");
pub const CERT_PLAN_BYTES: &str = include_str!("PlanBytes.lean");
pub const CERT_WASM_SLICE: &str = include_str!("WasmSlice.lean");
pub const CERT_EXPR_FRAGMENT_ACCEPTED: &str = include_str!("ExprFragmentAccepted.lean");
pub const CERT_ACCEPTED_ARTIFACT: &str = include_str!("AcceptedArtifact.lean");

/// Emitted-fragment profile and runtime ABI identifiers recorded in the
/// manifest. Stable strings the checker echoes; bumped when the certified
/// fragment or the runtime import surface changes.
pub const PROFILE_ID: &str = "AverUserProfile/v0";
pub const RUNTIME_ABI: &str = "aver-wasm-gc/0";
/// Certification level of a v0 artifact certificate: conditional on the named
/// runtime contracts (see the consult level naming L0/L1/L2/L3).
pub const CERT_LEVEL: &str = "L1";
pub const CERT_SCHEMA_VERSION: u32 = 18;
pub const BOX_CONTRACT: &str = "__rt_aint_from_i64 (box i64 -> carrier)";
pub const INT_ADD_CONTRACT: &str =
    "Int.add (carrier add = exact integer addition on represented values)";
pub const INT_SUB_CONTRACT: &str =
    "Int.sub (carrier sub = exact integer subtraction on represented values)";
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

/// sha256 of a byte slice, lowercase hex.
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex(&h.finalize())
}

/// The content hashes of the audited schema and semantics prelude as embedded
/// in THIS binary — the checker's anchor: a cert whose on-disk `Schema.lean` /
/// `CertPrelude.lean` do not hash to these is not the audited version.
pub fn audited_schema_sha() -> String {
    sha256_hex(CERT_SCHEMA.as_bytes())
}
pub fn audited_prelude_sha() -> String {
    sha256_hex(CERT_PRELUDE.as_bytes())
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

include!("core_wasm.rs");
include!("core_shapes.rs");
include!("expr_fragment_defs.rs");
include!("sym_plan_defs.rs");
include!("expr_fragment_from_mir.rs");
include!("cert_defs.rs");
include!("cert_methods.rs");
include!("analysis.rs");
include!("rederive.rs");
include!("disasm.rs");
include!("classification.rs");
include!("model_eval.rs");
include!("render.rs");
