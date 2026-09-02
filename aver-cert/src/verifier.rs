//! Minimal consumer for Aver artifact certificates.
//!
//! Rust owns transport: it selects the embedded wall, stages untrusted DATA,
//! injects the exact artifact bytes, and runs Lean. It does not disassemble the
//! module or reconstruct an obligation. The checker-owned Lean predicate binds
//! every accepted claim to the bytes and derives its standard face, policy,
//! termination witness, host table, and runtime contracts.

use crate::bridge_statement::{
    self, MAX_BRIDGE_STATEMENT_LEN, SourceEncoder, render_bridge_statement,
    statement_is_root_qualified,
};
use crate::cache::{ArtifactBuildCache, KeyMaterial as ArtifactCacheKeyMaterial};
use crate::lean_process::LeanRunner;
use crate::prelude_cache::PristineWallCache;
use crate::{format, wall};
use colored::Colorize;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

const AXIOM_WHITELIST: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];
const CHECKED_ROOT: &str = "AverCertChecker.checked";
/// Checker-owned name of the law pin for manifest `laws[i]`.
const LAW_PIN_PREFIX: &str = "AverCertChecker.law_pin_";
/// Marker of the machine-readable per-pin axiom-audit line the checker-owned
/// witness logs. Read back from the witness elaboration output only, which
/// elaborates no package text, so no staged file can forge one.
const LAW_AUDIT_MARKER: &str = "AVER_LAW_AUDIT";
/// Checker-owned name of the pin for the BRIDGED corollary of a law-claim that
/// declares bridges, indexed over the bridged claims in manifest order.
const BRIDGED_LAW_PIN_PREFIX: &str = "AverCertChecker.bridged_law_pin_";
/// Marker of the per-bridged-claim axiom-audit line. Distinct from both other
/// markers as a whole string, so no line of one surface can be read as another.
const LAW_BRIDGE_AUDIT_MARKER: &str = "AVER_LAW_BRIDGE_AUDIT";
/// Suffix the package's bridged law corollary carries over the plain one. The
/// manifest never declares it; both sides derive it from the claim's label.
const LAW_BRIDGED_COROLLARY_SUFFIX: &str = "_bridged";
/// Checker-owned name of the pin for manifest `sourceBridges[i]`.
const BRIDGE_PIN_PREFIX: &str = "AverCertChecker.bridge_pin_";
/// Marker of the per-bridge axiom-audit line, read back exactly like the
/// law one.
const BRIDGE_AUDIT_MARKER: &str = "AVER_BRIDGE_AUDIT";
/// Lean namespace every package bridge theorem and corollary must live in, and
/// the suffix the corollary carries. Both are checked exactly, so a bridge
/// entry can only ever name the two declarations this surface defines.
const BRIDGE_NAMESPACE: &str = "AverCert.Bridge";
const BRIDGE_COROLLARY_SUFFIX: &str = "_certified";
const MAX_CANDIDATE_LEN: usize = 200;
const TOOLCHAIN_ROOTS: [&str; 4] = ["Init", "Lake", "Lean", "Std"];
const FRESH_REPLAY_ARGS: [&str; 4] = ["env", "leanchecker", "--fresh", "ArtifactCertificate"];
/// User-facing name of the `lake build` step in timeout and failure messages.
const PROOF_BUILD_PHASE: &str = "certificate proof build";

/// Emitted on a green verdict. All trust-bearing byte facts and claim metadata
/// are checked by the embedded Lean wall; Rust performs no parallel verdict
/// reconstruction.
pub const ARTIFACT_DECODE_LINE: &str =
    "artifact-check: exact bytes and manifest accepted by the checker-owned Lean predicate";

const CODE_EXEC_TOKENS: [&str; 20] = [
    "#eval",
    "run_cmd",
    "run_elab",
    "run_tac",
    "initialize",
    "builtin_initialize",
    "macro",
    "macro_rules",
    "elab",
    "elab_rules",
    "syntax",
    "notation",
    "unsafe",
    "implemented_by",
    "extern",
    "deriving",
    "attribute",
    "@[",
    "«",
    "open Lean",
];

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Verdict {
    Certified {
        summary: String,
        faces: Vec<String>,
        /// One line per declared law-claim that was NOT credited.
        laws: Vec<String>,
        /// One line per bridged law-claim whose bridged corollary was NOT
        /// credited. Independent of `laws`: the law itself can stand while the
        /// identity of the plan with its source function does not.
        bridged_laws: Vec<String>,
        /// One line per declared source-bridge that was NOT credited.
        source_bridges: Vec<String>,
    },
    NoExports(String),
}

/// Developer preflight result. A green value means the checker-owned witness
/// elaborated successfully while trusting the local Lake `.olean` graph. It is
/// deliberately distinct from [`Verdict`]: only [`verify`] performs the final
/// fresh-environment replay required for certification.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CheckVerdict {
    Checked {
        summary: String,
        faces: Vec<String>,
        /// One line per declared law-claim that was NOT credited.
        laws: Vec<String>,
        /// One line per bridged law-claim whose bridged corollary was NOT
        /// credited. Independent of `laws`: the law itself can stand while the
        /// identity of the plan with its source function does not.
        bridged_laws: Vec<String>,
        /// One line per declared source-bridge that was NOT credited.
        source_bridges: Vec<String>,
    },
    NoExports(String),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Explanation {
    Certified,
    NoExports,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ReplayMode {
    Fresh,
    TrustBuiltOleans,
}

struct CertifiedExport {
    name: String,
    policy: String,
    face: String,
    /// Domain disclosure, present only for the faces whose certified domain is
    /// narrower than "any represented value" (today: record projection-compute).
    domain: Option<String>,
    manifest_face: String,
    /// What the certified model IS, for the one face whose obligation model is
    /// the PLAN rather than a source function: `plan`, or `plan ≡ <fn>` once a
    /// credited source-bridge identifies the two. `None` for every other face,
    /// whose obligation already names the source model.
    certified_model: Option<String>,
}

/// The outcome of one declared law-claim. A claim whose pin elaborated but
/// whose axiom closure leaves the whitelist is NOT credited — it is the
/// law-claim analogue of `declaredUncertified` for exports, and it never moves
/// the package verdict, which is the exports' alone. A pin that does not
/// ELABORATE is a different case entirely: it declines the whole package,
/// because the declared statement then is not what the package proves.
#[derive(Debug)]
struct LawOutcome {
    /// Manifest label of the claim (a plain dotted Lean identifier).
    label: String,
    /// Non-whitelisted axioms the pin's proof depends on. Empty means credited.
    offending: Vec<String>,
}

/// The outcome of one declared source-bridge, with the same two outcomes and
/// the same credit semantics as a law-claim: a pin that elaborates but leaves
/// the axiom whitelist loses its own credit, and a pin that does not elaborate
/// declines the package.
#[derive(Debug)]
struct BridgeOutcome {
    /// Certified export the bridge is about.
    export: String,
    /// Source function the bridge identifies the plan with.
    model: String,
    /// The statement the CHECKER rendered from the declared structure and
    /// pinned the package's corollary at. `explain` prints this, never text the
    /// package supplied.
    statement: String,
    /// Non-whitelisted axioms the pin's proof depends on. Empty means credited.
    offending: Vec<String>,
}

struct TrustedReport {
    exports: Vec<CertifiedExport>,
    laws: Vec<LawOutcome>,
    /// One outcome per law-claim that declares bridges, in manifest order.
    /// Reported as its own counter: a bridge that fails its axiom audit costs
    /// this claim, never the plain law beside it.
    bridged_laws: Vec<LawOutcome>,
    source_bridges: Vec<BridgeOutcome>,
    contracts: Vec<String>,
    target: String,
    profile: String,
    abi: String,
    artifact_hash: String,
}

struct CertifiedCandidate {
    name: String,
    class: String,
    policy: String,
    policy_lean: &'static str,
    termination_lean: String,
    dom: String,
    cod: String,
    /// The manifest's declared discharge theorem, read for ONE purpose: to
    /// tell which exports carry the record projection-compute face, whose
    /// certified domain is narrower than the other faces'. Declared-only, like
    /// `dom`/`cod`, so it never reaches the CERTIFIED/CHECKED verdict line.
    theorem: Option<String>,
}

#[derive(Clone, Copy)]
enum StringHostRole {
    Eq,
    Concat,
}

/// The declared role indices in fixed `(box, add, mul, sub, toIndex, cmp, eq)`
/// order — the same order the producer's `FragHostRoleIndices` uses.
type HostRoleTable = (
    Option<u32>,
    Option<u32>,
    Option<u32>,
    Option<u32>,
    Option<u32>,
    Option<u32>,
    Option<u32>,
);

struct ManifestIdentity {
    target: String,
    profile: String,
    abi: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ArtifactTarget {
    WasmGc,
    Wasip2,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Wasip2EnvelopeDeclaration {
    inner: format::Wasip2ComponentEnvelopeDeclaration,
}

#[derive(Debug)]
struct PreparedArtifact<'a> {
    /// Hash of the delivered artifact named by `target`.
    artifact_hash: String,
    /// Delivered target-artifact bytes consumed by target-specific envelope checks.
    target_artifact_bytes: &'a [u8],
    /// Core wasm module bytes consumed by the existing wasm decoder wall.
    core_module_bytes: &'a [u8],
}

/// One manifest law-claim: the checker-owned witness re-elaborates the
/// package's `Laws.lean` corollary at exactly this declared statement and
/// audits its axioms against the kernel whitelist.
struct LawCandidate {
    label: String,
    /// Fully qualified model theorem name (`Domain.Rational.plus_law_commutative`).
    theorem: String,
    /// Verbatim single-line universal statement text.
    statement: String,
    /// Corollary name inside `AverCert.Laws`.
    corollary: String,
    /// Namespace to `open` so the statement elaborates (`theorem` minus its
    /// last segment).
    prefix: String,
    /// Indices into the declared `sourceBridges` whose statements the corollary
    /// conjoins — every model function this law mentions, when all of them are
    /// bridged. Empty otherwise, and then the pin has the two conjuncts this
    /// surface always had.
    bridges: Vec<usize>,
}

/// One manifest source-bridge, as the manifest declares it: STRUCTURE, never
/// statement text.
///
/// The checker renders the statement itself from `(export, model, params,
/// result)` and re-elaborates the package's corollary at exactly that type. A
/// package therefore cannot choose what its bridge says — only which of the
/// statements this checker can render it is claiming — so a tautology, a
/// pointer at another export's plan, or a permuted accessor list is not a
/// weaker credited claim but a pin that does not elaborate.
struct SourceBridgeCandidate {
    /// Certified export the bridge is about, and the plan the statement names.
    export: String,
    /// Fully qualified corollary (`AverCert.Bridge.<export>_certified`).
    corollary: String,
    /// Fully qualified source function the bridge identifies the plan with.
    model: String,
    /// The statement the checker RENDERED from the declared `(export, model,
    /// params, result)`. Nothing in the manifest contributes to it beyond
    /// those; the declared `theorem` name is checked and then discarded,
    /// because the pin cites the corollary.
    statement: String,
}

struct Candidates {
    certified: Vec<CertifiedCandidate>,
    laws: Vec<LawCandidate>,
    source_bridges: Vec<SourceBridgeCandidate>,
    contracts: Vec<String>,
    declared_uncertified: Vec<(String, String)>,
    capabilities: Vec<(String, String)>,
    start: Option<u32>,
    host_role_table: Option<HostRoleTable>,
    string_host_roles: Vec<(u32, StringHostRole)>,
    target: String,
    profile: String,
    abi: String,
    wasip2_component_envelope: Option<format::Wasip2ComponentEnvelopeDeclaration>,
}

pub fn verify(artifact: &Path, cert_dir: &Path) -> Result<Verdict, String> {
    let report = trusted_check(artifact, cert_dir, ReplayMode::Fresh)?;
    let summary = summarize_report(artifact, report, "certified");
    if summary.count == 0 {
        Ok(Verdict::NoExports(summary.text))
    } else {
        Ok(Verdict::Certified {
            summary: summary.text,
            faces: summary.faces,
            laws: summary.uncredited_laws,
            bridged_laws: summary.uncredited_bridged_laws,
            source_bridges: summary.uncredited_bridges,
        })
    }
}

/// Fast developer/CI preflight which trusts locally built or explicitly
/// cached `.olean` imports. Rust validation, `lake build`, and fresh
/// elaboration of the checker-owned witness still run; only the final
/// `leanchecker --fresh` whole-closure replay is omitted.
///
/// This function cannot produce [`Verdict::Certified`]. Release and admission
/// gates must use [`verify`].
pub fn check(artifact: &Path, cert_dir: &Path) -> Result<CheckVerdict, String> {
    let report = trusted_check(artifact, cert_dir, ReplayMode::TrustBuiltOleans)?;
    let summary = summarize_report(artifact, report, "checked");
    if summary.count == 0 {
        Ok(CheckVerdict::NoExports(summary.text))
    } else {
        Ok(CheckVerdict::Checked {
            summary: summary.text,
            faces: summary.faces,
            laws: summary.uncredited_laws,
            bridged_laws: summary.uncredited_bridged_laws,
            source_bridges: summary.uncredited_bridges,
        })
    }
}

struct ReportSummary {
    text: String,
    faces: Vec<String>,
    uncredited_laws: Vec<String>,
    uncredited_bridged_laws: Vec<String>,
    uncredited_bridges: Vec<String>,
    count: usize,
}

fn summarize_report(artifact: &Path, report: TrustedReport, status: &'static str) -> ReportSummary {
    let count = report.exports.len();
    let has_total = report
        .exports
        .iter()
        .any(|export| export.policy == "simulatesModelTotally");
    let has_partial = report
        .exports
        .iter()
        .any(|export| export.policy == "simulatesModel");
    let level = match (has_partial, has_total) {
        (true, true) => "mixed L1/L3",
        (false, true) => "L3",
        _ => "L1",
    };
    // A package with no law-claims prints exactly the line it always printed:
    // the clause appears only once a claim exists to report on.
    let law_clause = if report.laws.is_empty() {
        String::new()
    } else {
        let credited = report
            .laws
            .iter()
            .filter(|law| law.offending.is_empty())
            .count();
        format!("; law-claims: {credited} of {} credited", report.laws.len())
    };
    // The bridged half of the law surface is a counter of its own, and appears
    // only when some claim is bridged. Keeping it apart from `law-claims` is
    // the point: a bridge that loses its axiom audit moves this number and
    // leaves the one above it alone.
    let bridged_law_clause = if report.bridged_laws.is_empty() {
        String::new()
    } else {
        let credited = report
            .bridged_laws
            .iter()
            .filter(|law| law.offending.is_empty())
            .count();
        format!(
            "; bridged-laws: {credited} of {} credited",
            report.bridged_laws.len()
        )
    };
    // Same rule for the bridge surface: a package that declares none prints
    // exactly the line it always printed.
    let bridge_clause = if report.source_bridges.is_empty() {
        String::new()
    } else {
        let credited = report
            .source_bridges
            .iter()
            .filter(|bridge| bridge.offending.is_empty())
            .count();
        format!(
            "; source-bridges: {credited} of {} credited",
            report.source_bridges.len()
        )
    };
    let text = format!(
        "{} ({} {status} export{}, level {}{law_clause}{bridged_law_clause}{bridge_clause})",
        artifact.display(),
        count,
        if count == 1 { "" } else { "s" },
        level,
    );
    let uncredited_laws = report
        .laws
        .iter()
        .filter(|law| !law.offending.is_empty())
        .map(|law| {
            format!(
                "law-claim not credited: {} (proof depends on {})",
                display_safe(&law.label),
                display_safe(&law.offending.join(", ")),
            )
        })
        .collect();
    let uncredited_bridged_laws = report
        .bridged_laws
        .iter()
        .filter(|law| !law.offending.is_empty())
        .map(|law| {
            format!(
                "bridged law-claim not credited: {} (proof depends on {})",
                display_safe(&law.label),
                display_safe(&law.offending.join(", ")),
            )
        })
        .collect();
    let uncredited_bridges = report
        .source_bridges
        .iter()
        .filter(|bridge| !bridge.offending.is_empty())
        .map(|bridge| {
            format!(
                "source-bridge not credited: {} (proof depends on {})",
                display_safe(&bridge.export),
                display_safe(&bridge.offending.join(", ")),
            )
        })
        .collect();
    let faces = report
        .exports
        .into_iter()
        .map(|export| {
            format!(
                "{}  policy: {}  {}",
                export.name, export.policy, export.face
            )
        })
        .collect();
    ReportSummary {
        text,
        faces,
        uncredited_laws,
        uncredited_bridged_laws,
        uncredited_bridges,
        count,
    }
}

/// Developer-only override naming a locally built parallel replayer.
///
/// Read in exactly one place, empty by default. The published contract — CI,
/// releases, and every consumer-facing run — stays on stock `leanchecker
/// --fresh`. The replayer's process exit status is the verdict, the same
/// contract stock replay is held to.
const PARALLEL_REPLAY_ENV: &str = "AVER_CERT_PARALLEL_REPLAY";

/// Arguments for the final kernel replay, or `None` when the mode replays
/// nothing. Stock `leanchecker` unless the developer override names a binary.
fn kernel_replay_args(mode: ReplayMode) -> Option<Vec<String>> {
    let override_binary = std::env::var(PARALLEL_REPLAY_ENV).ok();
    replay_args_for(mode, override_binary.as_deref())
}

/// The dispatch itself, free of environment lookup so it is testable directly.
/// A blank override is treated as absent: an exported-but-empty variable must
/// not select a replayer named by the empty string.
fn replay_args_for(mode: ReplayMode, override_binary: Option<&str>) -> Option<Vec<String>> {
    match mode {
        ReplayMode::Fresh => Some(match override_binary {
            Some(binary) if !binary.trim().is_empty() => vec![
                "env".to_string(),
                binary.to_string(),
                "ArtifactCertificate".to_string(),
                "AverCert.Artifact.certificate".to_string(),
                "replay".to_string(),
                "8".to_string(),
                "32".to_string(),
            ],
            _ => FRESH_REPLAY_ARGS.iter().map(|a| (*a).to_string()).collect(),
        }),
        ReplayMode::TrustBuiltOleans => None,
    }
}

fn trusted_check(
    artifact: &Path,
    cert_dir: &Path,
    replay_mode: ReplayMode,
) -> Result<TrustedReport, String> {
    let bytes = std::fs::read(artifact)
        .map_err(|error| format!("cannot read artifact {}: {error}", artifact.display()))?;
    let manifest = read_manifest(cert_dir)?;

    let schema_version = manifest_u64(&manifest, "schema_version")?;
    if schema_version != format::CERT_SCHEMA_VERSION as u64 {
        return Err(format!(
            "unsupported certificate schema_version {schema_version}; this checker accepts {}",
            format::CERT_SCHEMA_VERSION
        ));
    }

    let identity = read_manifest_identity(&manifest)?;
    let artifact_target = require_supported_identity(&identity)?;
    let target_envelope = read_artifact_target_envelope(artifact_target, &manifest)?;
    let PreparedArtifact {
        artifact_hash: actual_hash,
        target_artifact_bytes,
        core_module_bytes,
    } = prepare_artifact_for_target(artifact_target, &bytes, target_envelope)?;
    let pinned_hash = manifest_str(&manifest, "wasm_sha256")?;
    if pinned_hash != actual_hash {
        return Err(format!(
            "artifact hash mismatch: {} hashes to {actual_hash}, certificate pins {pinned_hash}",
            artifact.display()
        ));
    }

    let format_object = manifest
        .get("format")
        .and_then(Value::as_object)
        .ok_or_else(|| "cert-manifest.json is missing object field `format`".to_string())?;
    let format_version = format_object
        .get("version")
        .and_then(Value::as_u64)
        .ok_or_else(|| "cert-manifest.json `format.version` must be an integer".to_string())?;
    if format_version != format::FORMAT_VERSION as u64 {
        return Err(format!(
            "unsupported certificate format version {format_version}; this checker accepts {}",
            format::FORMAT_VERSION
        ));
    }
    let wall_id = format_object
        .get("wall_id")
        .and_then(Value::as_str)
        .ok_or_else(|| "cert-manifest.json `format.wall_id` must be a string".to_string())?;
    let selected_wall = wall::resolve(wall_id).ok_or_else(|| {
        format!("unsupported certificate wall `{wall_id}`; no embedded wall matches")
    })?;
    let artifact_root = manifest_str(&manifest, "artifact_certificate_root")?;
    if artifact_root != format::ARTIFACT_CERTIFICATE_ROOT {
        return Err(format!(
            "artifact certificate root mismatch: certificate pins {artifact_root}, checker expects {}",
            format::ARTIFACT_CERTIFICATE_ROOT
        ));
    }

    let candidates = read_candidates(&manifest, identity, target_envelope.map(|env| env.inner))?;
    let lean = LeanRunner::new(selected_wall.toolchain)?;
    let build = assemble_build(
        cert_dir,
        core_module_bytes,
        target_artifact_bytes,
        selected_wall,
        lean.memory_limit_mb(),
    )?;
    let cache_pins = [("wasm_sha256", pinned_hash), ("wall_id", wall_id)];
    let mut cache = ArtifactBuildCache::prepare(
        &build.path,
        &ArtifactCacheKeyMaterial {
            schema_version,
            pinned_sha256: &cache_pins,
            toolchain_version: selected_wall.toolchain.trim(),
        },
    );
    let data_cache_hit = cache.was_hit();
    let mut wall_cache = if data_cache_hit {
        PristineWallCache::disabled()
    } else {
        PristineWallCache::prepare(&build.path, selected_wall, &lean)
    };

    let mut data_build = run_lake(&lean, &build.path, PROOF_BUILD_PHASE, &["build"])?;
    if !data_build.status.success() && (data_cache_hit || wall_cache.was_seeded()) {
        if data_cache_hit {
            cache.invalidate(&build.path);
        } else {
            wall_cache.clear_build(&build.path);
        }
        data_build = run_lake(&lean, &build.path, PROOF_BUILD_PHASE, &["build"])?;
        if data_build.status.success() && wall_cache.was_seeded() {
            wall_cache.evict();
        }
    }
    if !data_build.status.success() {
        return Err(format!(
            "certificate data did not build:\n{}",
            surface_build_failure(&data_build.combined, 30)
        ));
    }
    cache.publish(&build.path);

    let witness = checker_witness(&actual_hash, &candidates);
    std::fs::write(build.path.join("CheckerWitness.lean"), witness)
        .map_err(|error| format!("cannot write checker witness: {error}"))?;
    let elaborated = run_lake(
        &lean,
        &build.path,
        "artifact witness check",
        &[
            "env",
            "lean",
            "-o",
            ".lake/build/lib/lean/CheckerWitness.olean",
            "CheckerWitness.lean",
        ],
    )?;
    if !elaborated.status.success() {
        return Err(format!(
            "certificate does not bind to this artifact: the checker-owned Lean witness failed:\n{}",
            tail(&elaborated.combined, 30)
        ));
    }
    // Every pin ELABORATED, so every declared statement is exactly what the
    // package proves. What remains is per-pin credit, read from the witness's
    // own audit trace: a missing or malformed line is a decline, never credit.
    let laws = parse_law_audits(&elaborated.combined, &candidates.laws)?;
    let bridged_laws = parse_bridged_law_audits(&elaborated.combined, &candidates.laws)?;
    let source_bridges = parse_bridge_audits(&elaborated.combined, &candidates.source_bridges)?;
    if let Some(replay_args) = kernel_replay_args(replay_mode) {
        let replay_args: Vec<&str> = replay_args.iter().map(String::as_str).collect();
        let replayed = run_lake(&lean, &build.path, "final kernel replay", &replay_args)?;
        if !replayed.status.success() {
            return Err(format!(
                "certificate failed fresh-environment kernel replay:\n{}",
                tail(&replayed.combined, 30)
            ));
        }
    }

    let exports = candidates
        .certified
        .iter()
        .map(|candidate| CertifiedExport {
            name: candidate.name.clone(),
            policy: candidate.policy.clone(),
            face: report_face(candidate),
            domain: record_compute_domain(candidate).map(str::to_string),
            manifest_face: manifest_face(candidate),
            certified_model: certified_model_line(candidate, &source_bridges),
        })
        .collect();
    Ok(TrustedReport {
        exports,
        laws,
        bridged_laws,
        source_bridges,
        contracts: candidates.contracts,
        target: candidates.target,
        profile: candidates.profile,
        abi: candidates.abi,
        artifact_hash: actual_hash,
    })
}

/// The per-export line printed under a CERTIFIED/CHECKED verdict. Everything
/// on it must be kernel-pinned: the class is rfl-bound to
/// `StandardFace.reportEntries` by the checker witness (like the name, policy,
/// and termination). The manifest's `dom`/`cod` strings are NOT pinned by any
/// witness line, so they must never appear here — `explain` shows them,
/// explicitly labeled as manifest-declared.
fn report_face(candidate: &CertifiedCandidate) -> String {
    let label = match candidate.class.as_str() {
        "expr-fragment-v1" => "expression fragment",
        "verbatim-string-eq" => "String.eq leaf",
        "verbatim-string-concat" => "String.concat leaf",
        "adt-constructor" => "ADT constructor",
        "self-recursive" => "integer recursion",
        "multi-argument self-recursive" => "integer accumulator recursion",
        "mutual-recursive" => "mutual integer recursion",
        "verbatim-dispatch" => "verbatim dispatch",
        "int-dispatch" => "integer ADT dispatch",
        "field-projection" => "field projection",
        "cross-function-composition" => "cross-function composition",
        other => other,
    };
    format!("class: {label}")
}

/// What the export's certified model IS, for the one face whose obligation
/// model is the plan rather than a source function.
///
/// `plan` on its own is the disclosure this face has always owed a reader: the
/// theorem is about the evaluation of the declared plan. `plan ≡ <fn>` is what
/// a CREDITED source-bridge adds — a kernel-checked theorem that the plan's
/// model is the transpiled source function at the face's own encoders. An
/// uncredited bridge says `plan` exactly like no bridge at all; credit is never
/// granted on a declaration.
///
/// The line points at SOURCE-BRIDGES rather than calling itself kernel-checked
/// on its own. What the credit means is that the rendered statement printed
/// there is proven without foreign axioms, and that statement — its encoders
/// included — is what a reader has to read. A name plus a tick is not the
/// claim.
fn certified_model_line(
    candidate: &CertifiedCandidate,
    bridges: &[BridgeOutcome],
) -> Option<String> {
    record_compute_domain(candidate)?;
    let credited = bridges
        .iter()
        .find(|bridge| bridge.export == candidate.name && bridge.offending.is_empty());
    Some(match credited {
        Some(bridge) => format!(
            "model: plan ≡ {} (credited source-bridge; see SOURCE-BRIDGES)",
            display_safe(&bridge.model)
        ),
        None => "model: plan".to_string(),
    })
}

fn manifest_face(candidate: &CertifiedCandidate) -> String {
    format!(
        "manifest face (declared, not kernel-pinned): Dom {}, Cod {}",
        display_safe(&candidate.dom),
        display_safe(&candidate.cod)
    )
}

/// The domain disclosure for the record projection-compute face, or `None` for
/// every other face.
///
/// That face is the one place where canonicity — the runtime's normal form —
/// is a premise about the INPUTS and not only about the helpers: its
/// `StandardFace.recordComputeDomRepr` is built from `SReprAll`, and `SRepr` on
/// an Int carrier is "represented AND canonical", record fields included. A
/// reader of a verdict has to be told, so `explain` says it on the export's own
/// line (section 4.3 of the format spec carries the long form).
///
/// The face is selected by the manifest's declared discharge theorem. That
/// field is informational, so a producer could in principle mislabel it; the
/// failure mode is a missing or a spurious disclosure line in `explain`, never
/// a weaker accepted claim — acceptance reads the single artifact root, and the
/// face itself is pinned in-kernel by `StandardFace.checkedFaces`.
fn record_compute_domain(candidate: &CertifiedCandidate) -> Option<&'static str> {
    match candidate.theorem.as_deref() {
        Some(format::RECORD_COMPUTE_DISCHARGE_THEOREM) => Some(format::RECORD_COMPUTE_DOMAIN_LINE),
        _ => None,
    }
}

/// Indices of the law-claims that declare bridges, in manifest order.
///
/// The witness numbers `bridged_law_pin_<j>` over this list and the readback of
/// its audit lines is keyed on the same list, so the pin numbering and the
/// reported outcomes cannot drift apart.
fn bridged_law_indices(laws: &[LawCandidate]) -> Vec<usize> {
    laws.iter()
        .enumerate()
        .filter(|(_, law)| !law.bridges.is_empty())
        .map(|(index, _)| index)
        .collect()
}

fn checker_witness(sha: &str, candidates: &Candidates) -> String {
    let bridged_law_indices = bridged_law_indices(&candidates.laws);
    let names = lean_str_list(
        &candidates
            .certified
            .iter()
            .map(|candidate| candidate.name.clone())
            .collect::<Vec<_>>(),
    );
    let report_entries = lean_string_pair_list(
        &candidates
            .certified
            .iter()
            .map(|candidate| (candidate.name.clone(), candidate.class.clone()))
            .collect::<Vec<_>>(),
    );
    let policies = format!(
        "[{}]",
        candidates
            .certified
            .iter()
            .map(|candidate| candidate.policy_lean)
            .collect::<Vec<_>>()
            .join(", ")
    );
    let terminations = format!(
        "[{}]",
        candidates
            .certified
            .iter()
            .map(|candidate| candidate.termination_lean.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
    let contracts = lean_str_list(&candidates.contracts);
    let declared = lean_string_pair_list(&candidates.declared_uncertified);
    let capabilities = lean_string_pair_list(&candidates.capabilities);
    let start = lean_option_nat(candidates.start);
    let roles = match candidates.host_role_table {
        Some((box_role, add_role, mul_role, sub_role, to_index_role, cmp_role, eq_role)) => {
            format!(
                "some ({{ box := {}, add := {}, mul := {}, sub := {}, toIndex := {}, \
             cmp := {}, eq := {} }} : \
             CertDecode.AddSub.Roles)",
                lean_option_nat(box_role),
                lean_option_nat(add_role),
                lean_option_nat(mul_role),
                lean_option_nat(sub_role),
                lean_option_nat(to_index_role),
                lean_option_nat(cmp_role),
                lean_option_nat(eq_role),
            )
        }
        None => "(none : Option CertDecode.AddSub.Roles)".to_string(),
    };
    let string_roles = format!(
        "[{}]",
        candidates
            .string_host_roles
            .iter()
            .map(|(index, role)| {
                let role = match role {
                    StringHostRole::Eq => ".eq",
                    StringHostRole::Concat => ".concat",
                };
                format!("({index}, {role})")
            })
            .collect::<Vec<_>>()
            .join(", ")
    );
    let wasip2_component_envelope =
        lean_wasip2_component_envelope(candidates.wasip2_component_envelope);
    let allowed = AXIOM_WHITELIST
        .iter()
        .map(|name| format!("`{name}"))
        .collect::<Vec<_>>()
        .join(", ");
    // Law-claim surface: one type-pinning theorem per claim (built by
    // concatenation, never `format!`, so statement braces stay inert), the
    // conditional `Laws` import, and the corollary roots the axiom audit
    // walks. All fields were validated by `validate_law_candidate`.
    //
    // The statement is re-elaborated inside the model theorem's OWN namespace
    // — the same context the package's `Laws.lean` uses — because `open
    // <prefix> in` at root does not reproduce it: inside `namespace Json` the
    // text `Json.jsonInt` reaches the constructor `Json.Json.jsonInt`, while
    // at root it reaches the accessor `Json.jsonInt` that `open` only adds an
    // alias beside. The pins therefore sit OUTSIDE `namespace AverCertChecker`
    // and name themselves `_root_.AverCertChecker.law_pin_<i>`: nested inside
    // it the current namespace would be `AverCertChecker.<prefix>`, whose
    // resolution is not the model's either.
    let law_import = if candidates.laws.is_empty() {
        String::new()
    } else {
        "import Laws\n".to_string()
    };
    let bridge_import = if candidates.source_bridges.is_empty() {
        String::new()
    } else {
        "import Bridge\n".to_string()
    };
    //
    // A law that declares bridges gets a SECOND pin, at the package's
    // `_bridged` corollary, carrying those bridge statements as further
    // conjuncts. The two are kept apart because their failure modes must be:
    // conjoined into one pin, a bridge whose script fell to `sorry` put
    // `sorryAx` into the closure of every law that merely MENTIONS its
    // function, and removed the credit of a claim about the source model that
    // the bridge plays no part in proving.
    let mut law_pins = String::new();
    let mut bridged_law_root_names = String::new();
    for (index, law) in candidates.laws.iter().enumerate() {
        if !law.prefix.is_empty() {
            law_pins.push_str("namespace ");
            law_pins.push_str(&law.prefix);
            law_pins.push_str("\n\n");
        }
        law_pins.push_str(&format!("theorem _root_.{LAW_PIN_PREFIX}{index} :\n    ("));
        law_pins.push_str(&law.statement);
        law_pins.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest)");
        law_pins.push_str(" :=\n  _root_.AverCert.Laws.");
        law_pins.push_str(&law.corollary);
        law_pins.push_str("\n\n");
        if !law.bridges.is_empty() {
            let bridged_index = bridged_law_indices
                .iter()
                .position(|at| *at == index)
                .expect("every bridged law is enumerated");
            law_pins.push_str(&format!(
                "theorem _root_.{BRIDGED_LAW_PIN_PREFIX}{bridged_index} :\n    ("
            ));
            law_pins.push_str(&law.statement);
            law_pins.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest)");
            // The declared bridges, in the manifest's order. The pin's TYPE
            // forces the package's `_bridged` corollary to prove all of them,
            // and the audit that follows walks that whole closure.
            for bridge in &law.bridges {
                law_pins.push_str(" ∧\n      (");
                law_pins.push_str(&candidates.source_bridges[*bridge].statement);
                law_pins.push(')');
            }
            law_pins.push_str(" :=\n  _root_.AverCert.Laws.");
            law_pins.push_str(&law.corollary);
            law_pins.push_str(LAW_BRIDGED_COROLLARY_SUFFIX);
            law_pins.push_str("\n\n");
            bridged_law_root_names.push_str(&format!("`{BRIDGED_LAW_PIN_PREFIX}{bridged_index}, "));
        }
        if !law.prefix.is_empty() {
            law_pins.push_str("end ");
            law_pins.push_str(&law.prefix);
            law_pins.push_str("\n\n");
        }
    }
    let bridged_law_roots = bridged_law_root_names.trim_end_matches(", ").to_string();
    // Bridge pins need no namespace context: a bridge statement is rendered
    // fully `_root_`-qualified by the producer and validated to be so here, so
    // it means the same at the root as it does in the package's `Bridge.lean`.
    let mut bridge_pins = String::new();
    for (index, bridge) in candidates.source_bridges.iter().enumerate() {
        bridge_pins.push_str(&format!(
            "theorem _root_.{BRIDGE_PIN_PREFIX}{index} :\n    ("
        ));
        bridge_pins.push_str(&bridge.statement);
        bridge_pins
            .push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  _root_.");
        bridge_pins.push_str(&bridge.corollary);
        bridge_pins.push_str("\n\n");
    }
    // The audit walks the CHECKER-NAMED pins, never the package's bare
    // corollary names: the pin's term cites `_root_.AverCert.Laws.<c>` (so an
    // `open <prefix>`-shadowed decoy cannot be substituted), and auditing
    // `AverCertChecker.law_pin_<i>` covers exactly the closure the pin proved.
    let law_roots = (0..candidates.laws.len())
        .map(|index| format!("`{LAW_PIN_PREFIX}{index}"))
        .collect::<Vec<_>>()
        .join(", ");
    let bridge_roots = (0..candidates.source_bridges.len())
        .map(|index| format!("`{BRIDGE_PIN_PREFIX}{index}"))
        .collect::<Vec<_>>()
        .join(", ");
    // The two audits are deliberately asymmetric. The accepted-artifact root
    // THROWS: a non-whitelisted axiom under an export closure is a rejected
    // certificate. A law pin instead LOGS its result, because a law that fails
    // only its axiom audit loses its own credit and must not take the exports
    // down with it — the pin still had to elaborate at the declared statement
    // to get here, which is the integrity half of the claim. Rust reads the
    // logged lines back; a pin with no line is a decline, so a parse miss can
    // never become credit.
    format!(
        "-- Authored by aver-cert; never accepted from the certificate.\n\
         import Lean\n\
         import AcceptedArtifact\n\
         import ArtifactBytes\n\
         import Manifest\n\
         import Artifact\n\
         {law_import}\
         {bridge_import}\
         import ArtifactCertificate\n\n\
         set_option maxRecDepth 200000\n\n\
         set_option autoImplicit false\n\n\
         {bridge_pins}\
         {law_pins}\
         namespace AverCertChecker\n\n\
         example : AverCert.Artifact.data.modBytes = AverCert.ArtifactBytes.modBytes := rfl\n\
         example : AverCert.Artifact.data.modLen = AverCert.ArtifactBytes.modLen := rfl\n\
         example : AverCert.Artifact.data.manifest = AverCert.manifest := rfl\n\
         example : AverCert.Artifact.data.wasip2ComponentEnvelope = {wasip2_component_envelope} := rfl\n\n\
         example : AverCert.manifest.subject.artifactHash = \"{sha}\" := rfl\n\
         example : AverCert.manifest.subject.artifactRoot = \"{}\" := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.export_) = {names} := rfl\n\
         example : AverCert.manifest.subject.exports = {names} := rfl\n\
         example : AverCert.StandardFace.reportEntries AverCert.Artifact.data = some {report_entries} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.policy) = {policies} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.termination?) = {terminations} := rfl\n\
         example : AverCert.manifest.subject.contracts = {contracts} := rfl\n\
         example : AverCert.manifest.subject.declaredUncertified = {declared} := rfl\n\
         example : AverCert.manifest.subject.capabilities = {capabilities} := rfl\n\
         example : AverCert.manifest.subject.start = {start} := rfl\n\
         example : AverCert.manifest.subject.hostRoleTable = {roles} := rfl\n\
         example : AverCert.manifest.subject.stringHostRoles = {string_roles} := rfl\n\
         example : AverCert.manifest.subject.target = \"{}\" := rfl\n\
         example : AverCert.manifest.subject.profile = \"{}\" := rfl\n\
         example : AverCert.manifest.subject.abi = \"{}\" := rfl\n\n\
         theorem checked : AverCert.AcceptedArtifact.accepted AverCert.Artifact.data :=\n\
           AverCert.Artifact.certificate\n\n\
         end AverCertChecker\n\n\
         open Lean in\n\
         run_cmd do\n  \
           let allowed : List Lean.Name := [{allowed}]\n  \
           let axioms ← Lean.collectAxioms `{CHECKED_ROOT}\n  \
           for usedAxiom in axioms do\n    \
             unless allowed.contains usedAxiom do\n      \
               throwError s!\"non-whitelisted axiom: {{usedAxiom}}\"\n  \
           let lawRoots : List Lean.Name := [{law_roots}]\n  \
           for lawRoot in lawRoots do\n    \
             let lawAxioms ← Lean.collectAxioms lawRoot\n    \
             let offending := lawAxioms.filter (fun used => not (allowed.contains used))\n    \
             if offending.isEmpty then\n      \
               logInfo s!\"{LAW_AUDIT_MARKER} {{lawRoot}} ok\"\n    \
             else\n      \
               let names := String.intercalate \",\" (offending.toList.map (fun used => used.toString))\n      \
               logInfo s!\"{LAW_AUDIT_MARKER} {{lawRoot}} axioms {{names}}\"\n  \
           let bridgedLawRoots : List Lean.Name := [{bridged_law_roots}]\n  \
           for bridgedLawRoot in bridgedLawRoots do\n    \
             let bridgedLawAxioms ← Lean.collectAxioms bridgedLawRoot\n    \
             let offending := bridgedLawAxioms.filter (fun used => not (allowed.contains used))\n    \
             if offending.isEmpty then\n      \
               logInfo s!\"{LAW_BRIDGE_AUDIT_MARKER} {{bridgedLawRoot}} ok\"\n    \
             else\n      \
               let names := String.intercalate \",\" (offending.toList.map (fun used => used.toString))\n      \
               logInfo s!\"{LAW_BRIDGE_AUDIT_MARKER} {{bridgedLawRoot}} axioms {{names}}\"\n  \
           let bridgeRoots : List Lean.Name := [{bridge_roots}]\n  \
           for bridgeRoot in bridgeRoots do\n    \
             let bridgeAxioms ← Lean.collectAxioms bridgeRoot\n    \
             let offending := bridgeAxioms.filter (fun used => not (allowed.contains used))\n    \
             if offending.isEmpty then\n      \
               logInfo s!\"{BRIDGE_AUDIT_MARKER} {{bridgeRoot}} ok\"\n    \
             else\n      \
               let names := String.intercalate \",\" (offending.toList.map (fun used => used.toString))\n      \
               logInfo s!\"{BRIDGE_AUDIT_MARKER} {{bridgeRoot}} axioms {{names}}\"\n",
        format::ARTIFACT_CERTIFICATE_ROOT,
        candidates.target,
        candidates.profile,
        candidates.abi,
    )
}

fn read_manifest(cert_dir: &Path) -> Result<Value, String> {
    let path = cert_dir.join("cert-manifest.json");
    let text = std::fs::read_to_string(&path)
        .map_err(|error| format!("cannot read {}: {error}", path.display()))?;
    serde_json::from_str(&text)
        .map_err(|error| format!("cert-manifest.json is not valid JSON: {error}"))
}

fn manifest_str<'a>(manifest: &'a Value, key: &str) -> Result<&'a str, String> {
    manifest
        .get(key)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("cert-manifest.json is missing string field `{key}`"))
}

fn manifest_u64(manifest: &Value, key: &str) -> Result<u64, String> {
    manifest
        .get(key)
        .and_then(Value::as_u64)
        .ok_or_else(|| format!("cert-manifest.json is missing integer field `{key}`"))
}

fn read_manifest_identity(manifest: &Value) -> Result<ManifestIdentity, String> {
    let identity = ManifestIdentity {
        target: manifest_str(manifest, "target")?.to_string(),
        profile: manifest_str(manifest, "profile")?.to_string(),
        abi: manifest_str(manifest, "abi")?.to_string(),
    };
    gate_candidate("target", &identity.target)?;
    gate_candidate("profile", &identity.profile)?;
    gate_candidate("abi", &identity.abi)?;
    Ok(identity)
}

fn require_supported_identity(identity: &ManifestIdentity) -> Result<ArtifactTarget, String> {
    if identity.profile != format::PROFILE_ID {
        return Err(format!(
            "unsupported certificate profile `{}`; this checker accepts {}",
            identity.profile,
            format::PROFILE_ID
        ));
    }

    match identity.target.as_str() {
        format::TARGET_WASM_GC if identity.abi == format::RUNTIME_ABI_WASM_GC => {
            Ok(ArtifactTarget::WasmGc)
        }
        format::TARGET_WASIP2 if identity.abi == format::RUNTIME_ABI_WASIP2 => {
            Ok(ArtifactTarget::Wasip2)
        }
        format::TARGET_WASM_GC | format::TARGET_WASIP2 => Err(format!(
            "unsupported certificate ABI `{}` for target `{}`",
            identity.abi, identity.target
        )),
        _ => Err(format!(
            "unsupported certificate target `{}`; this checker accepts {}, {}",
            identity.target,
            format::TARGET_WASM_GC,
            format::TARGET_WASIP2
        )),
    }
}

fn read_artifact_target_envelope(
    target: ArtifactTarget,
    manifest: &Value,
) -> Result<Option<Wasip2EnvelopeDeclaration>, String> {
    match target {
        ArtifactTarget::WasmGc => {
            if manifest
                .get(format::WASIP2_COMPONENT_ENVELOPE_FIELD)
                .is_some()
            {
                return Err(format!(
                    "cert-manifest.json `{}` is only valid for target `{}`",
                    format::WASIP2_COMPONENT_ENVELOPE_FIELD,
                    format::TARGET_WASIP2
                ));
            }
            Ok(None)
        }
        ArtifactTarget::Wasip2 => read_wasip2_component_envelope(manifest).map(Some),
    }
}

fn prepare_artifact_for_target<'a>(
    target: ArtifactTarget,
    bytes: &'a [u8],
    envelope: Option<Wasip2EnvelopeDeclaration>,
) -> Result<PreparedArtifact<'a>, String> {
    match (target, envelope) {
        (ArtifactTarget::WasmGc, None) => prepare_wasm_gc_artifact(bytes),
        (ArtifactTarget::Wasip2, Some(envelope)) => {
            prepare_wasip2_artifact_with_declared_envelope(bytes, envelope)
        }
        (ArtifactTarget::WasmGc, Some(_)) => Err(format!(
            "{} envelope cannot be used for target `{}`",
            format::TARGET_WASIP2,
            format::TARGET_WASM_GC
        )),
        (ArtifactTarget::Wasip2, None) => Err(format!(
            "target `{}` requires `{}`",
            format::TARGET_WASIP2,
            format::WASIP2_COMPONENT_ENVELOPE_FIELD
        )),
    }
}

fn prepare_wasm_gc_artifact(bytes: &[u8]) -> Result<PreparedArtifact<'_>, String> {
    // The target field is read before validation so target-specific envelopes
    // are selected before byte interpretation. A wasm-gc artifact is a core
    // module, so its delivered artifact bytes and core module bytes coincide.
    wasmparser::Validator::new()
        .validate_all(bytes)
        .map_err(|error| format!("artifact is not valid WebAssembly: {error}"))?;
    if !wasmparser::Parser::is_core_wasm(bytes) {
        return Err("artifact is not a core WebAssembly module".to_string());
    }
    Ok(PreparedArtifact {
        artifact_hash: sha256_hex(bytes),
        target_artifact_bytes: bytes,
        core_module_bytes: bytes,
    })
}

fn read_wasip2_component_envelope(manifest: &Value) -> Result<Wasip2EnvelopeDeclaration, String> {
    let field = format::WASIP2_COMPONENT_ENVELOPE_FIELD;
    let envelope = manifest
        .get(field)
        .ok_or_else(|| format!("cert-manifest.json is missing object field `{field}`"))?;
    exact_object_fields(
        envelope,
        field,
        &[
            format::WASIP2_COMPONENT_ENVELOPE_KIND_FIELD,
            format::WASIP2_COMPONENT_ENVELOPE_PREFIX_LEN_FIELD,
            format::WASIP2_COMPONENT_ENVELOPE_CORE_LEN_FIELD,
            format::WASIP2_COMPONENT_ENVELOPE_SUFFIX_LEN_FIELD,
        ],
    )?;
    let kind = envelope
        .get(format::WASIP2_COMPONENT_ENVELOPE_KIND_FIELD)
        .and_then(Value::as_str)
        .ok_or_else(|| {
            format!(
                "cert-manifest.json `{field}.{}` must be a string",
                format::WASIP2_COMPONENT_ENVELOPE_KIND_FIELD
            )
        })?;
    if kind != format::WASIP2_COMPONENT_ENVELOPE_KIND {
        return Err(format!(
            "unsupported wasip2 component envelope kind `{kind}`; this checker expects {}",
            format::WASIP2_COMPONENT_ENVELOPE_KIND
        ));
    }
    Ok(Wasip2EnvelopeDeclaration {
        inner: format::Wasip2ComponentEnvelopeDeclaration::from_lengths(
            envelope_u64(envelope, format::WASIP2_COMPONENT_ENVELOPE_PREFIX_LEN_FIELD)?,
            envelope_u64(envelope, format::WASIP2_COMPONENT_ENVELOPE_CORE_LEN_FIELD)?,
            envelope_u64(envelope, format::WASIP2_COMPONENT_ENVELOPE_SUFFIX_LEN_FIELD)?,
        ),
    })
}

fn prepare_wasip2_artifact_with_declared_envelope<'a>(
    component_bytes: &'a [u8],
    declaration: Wasip2EnvelopeDeclaration,
) -> Result<PreparedArtifact<'a>, String> {
    if !wasmparser::Parser::is_component(component_bytes) {
        return Err("artifact is not a WebAssembly component".to_string());
    }
    // This is only a well-formedness gate for the delivered target artifact.
    // It must not be used to locate the embedded core; the core slice below is
    // derived solely from the manifest-declared envelope lengths.
    wasmparser::Validator::new()
        .validate_all(component_bytes)
        .map_err(|error| format!("artifact is not a valid WebAssembly component: {error}"))?;

    let declaration = declaration.inner;
    if declaration.embedded_core_module_len == 0 {
        return Err("wasip2 component envelope declares an empty embedded core module".to_string());
    }
    let declared_len = declaration.component_len().ok_or_else(|| {
        "wasip2 component envelope length overflow while summing prefix/core/suffix".to_string()
    })?;
    let actual_len = u64::try_from(component_bytes.len())
        .map_err(|_| "delivered component length does not fit in u64".to_string())?;
    if declared_len != actual_len {
        return Err(format!(
            "wasip2 component envelope length mismatch: declaration totals {declared_len} bytes, delivered component has {actual_len} bytes"
        ));
    }
    let (_prefix, core_module_bytes, _suffix) = declaration
        .split_component(component_bytes)
        .ok_or_else(|| "wasip2 component envelope split failed".to_string())?;
    if !wasmparser::Parser::is_core_wasm(core_module_bytes) {
        return Err("declared embedded core module is not a core WebAssembly module".to_string());
    }
    wasmparser::Validator::new()
        .validate_all(core_module_bytes)
        .map_err(|error| {
            format!("declared embedded core module is not valid WebAssembly: {error}")
        })?;

    Ok(PreparedArtifact {
        artifact_hash: sha256_hex(component_bytes),
        target_artifact_bytes: component_bytes,
        core_module_bytes,
    })
}

fn envelope_u64(envelope: &Value, key: &str) -> Result<u64, String> {
    envelope.get(key).and_then(Value::as_u64).ok_or_else(|| {
        format!(
            "cert-manifest.json `{}.{key}` must be a u64",
            format::WASIP2_COMPONENT_ENVELOPE_FIELD
        )
    })
}

fn read_candidates(
    manifest: &Value,
    identity: ManifestIdentity,
    wasip2_component_envelope: Option<format::Wasip2ComponentEnvelopeDeclaration>,
) -> Result<Candidates, String> {
    let certified_json = manifest
        .get("certified")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `certified`".to_string())?;
    let mut certified = Vec::with_capacity(certified_json.len());
    for entry in certified_json {
        let name = required_string(entry, "name", "certified[]")?;
        let class = required_string(entry, "class", "certified[]")?;
        let policy = required_string(entry, "policy", "certified[]")?;
        let policy_lean = match policy.as_str() {
            "simulatesModel" => ".simulatesModel",
            "simulatesModelTotally" => ".simulatesModelTotally",
            other => {
                return Err(format!(
                    "certified export `{name}` uses unsupported policy `{other}`"
                ));
            }
        };
        let termination_lean = parse_termination(entry.get("termination_witness"), &name)?;
        match (policy.as_str(), entry.get("termination_witness")) {
            ("simulatesModel", None) | ("simulatesModelTotally", Some(_)) => {}
            ("simulatesModel", Some(_)) => {
                return Err(format!(
                    "partial export `{name}` must not carry a termination witness"
                ));
            }
            ("simulatesModelTotally", None) => {
                return Err(format!(
                    "total export `{name}` is missing `termination_witness`"
                ));
            }
            _ => unreachable!(),
        }
        certified.push(CertifiedCandidate {
            name,
            class,
            policy,
            policy_lean,
            termination_lean,
            dom: required_string(entry, "dom", "certified[]")?,
            cod: required_string(entry, "cod", "certified[]")?,
            theorem: entry
                .get("theorem")
                .and_then(Value::as_str)
                .map(str::to_string),
        });
    }

    // The source-bridge surface is read BEFORE the laws: a law entry names the
    // bridges its corollary conjoins by export, and those must resolve to a
    // declared bridge.
    let bridges_json = manifest
        .get("sourceBridges")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `sourceBridges`".to_string())?;
    let certified_names: Vec<&str> = certified
        .iter()
        .map(|candidate| candidate.name.as_str())
        .collect();
    let mut source_bridges: Vec<SourceBridgeCandidate> = Vec::with_capacity(bridges_json.len());
    for (index, entry) in bridges_json.iter().enumerate() {
        let context = format!("sourceBridges[{index}]");
        exact_object_fields(
            entry,
            &context,
            &[
                "export",
                "theorem",
                "corollary",
                "model",
                "params",
                "result",
            ],
        )?;
        let declared_params = entry["params"]
            .as_array()
            .ok_or_else(|| format!("cert-manifest.json `{context}.params` is not an array"))?;
        let mut params = Vec::with_capacity(declared_params.len());
        for (position, declared) in declared_params.iter().enumerate() {
            params.push(read_source_encoder(
                declared,
                &format!("{context}.params[{position}]"),
            )?);
        }
        let bridge = RawSourceBridge {
            export: required_string(entry, "export", &context)?,
            theorem: required_string(entry, "theorem", &context)?,
            corollary: required_string(entry, "corollary", &context)?,
            model: required_string(entry, "model", &context)?,
            params,
            result: read_source_encoder(&entry["result"], &format!("{context}.result"))?,
        };
        let bridge = validate_source_bridge_candidate(bridge)?;
        if !certified_names.contains(&bridge.export.as_str()) {
            return Err(format!(
                "source-bridge names `{}`, which is not a certified export",
                display_safe(&bridge.export)
            ));
        }
        if source_bridges
            .iter()
            .any(|seen| seen.export == bridge.export)
        {
            return Err(format!(
                "source-bridges declare export `{}` twice",
                display_safe(&bridge.export)
            ));
        }
        source_bridges.push(bridge);
    }

    let laws_json = manifest
        .get("laws")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `laws`".to_string())?;
    let mut laws = Vec::with_capacity(laws_json.len());
    for (index, entry) in laws_json.iter().enumerate() {
        let context = format!("laws[{index}]");
        exact_object_fields(
            entry,
            &context,
            &["label", "theorem", "statement", "corollary", "bridges"],
        )?;
        let law = LawCandidate {
            label: required_string(entry, "label", &context)?,
            theorem: required_string(entry, "theorem", &context)?,
            statement: required_string(entry, "statement", &context)?,
            corollary: required_string(entry, "corollary", &context)?,
            prefix: String::new(),
            bridges: Vec::new(),
        };
        let mut law = validate_law_candidate(law)?;
        let declared_bridges = entry
            .get("bridges")
            .and_then(Value::as_array)
            .ok_or_else(|| format!("cert-manifest.json `{context}.bridges` is not an array"))?;
        for value in declared_bridges {
            let export = value.as_str().ok_or_else(|| {
                format!("cert-manifest.json `{context}.bridges[]` is not a string")
            })?;
            let at = source_bridges
                .iter()
                .position(|bridge| bridge.export == export)
                .ok_or_else(|| {
                    format!(
                        "law-claim `{}` cites source-bridge `{}`, which is not declared",
                        display_safe(&law.label),
                        display_safe(export)
                    )
                })?;
            if law.bridges.contains(&at) {
                return Err(format!(
                    "law-claim `{}` cites source-bridge `{}` twice",
                    display_safe(&law.label),
                    display_safe(export)
                ));
            }
            law.bridges.push(at);
        }
        laws.push(law);
    }
    // The label→corollary underscore flattening is not injective; a duplicate
    // corollary would declare the same theorem twice in `Laws.lean` and make
    // the package unverifiable with a confusing Lean error. Reject it here.
    let mut seen_corollaries = std::collections::BTreeSet::new();
    for law in &laws {
        if !seen_corollaries.insert(law.corollary.as_str()) {
            return Err(format!(
                "law-claims declare duplicate corollary `{}`",
                law.corollary
            ));
        }
    }

    let contracts = string_array(manifest, "runtime_contracts")?;
    let declared_uncertified =
        object_pair_array(manifest, "declaredUncertified", "name", "reason")?;
    let capabilities = object_pair_array(manifest, "capabilities", "module", "name")?;
    let start_object = manifest
        .get("start")
        .ok_or_else(|| "cert-manifest.json is missing object field `start`".to_string())?;
    exact_object_fields(start_object, "start", &["present", "function_index"])?;
    let present = start_object
        .get("present")
        .and_then(Value::as_bool)
        .ok_or_else(|| "cert-manifest.json `start.present` is not a boolean".to_string())?;
    let start = match (present, start_object.get("function_index")) {
        (false, Some(Value::Null)) => None,
        (true, Some(value)) => Some(value_u32(value, "start.function_index")?),
        (false, _) => {
            return Err("absent start must use null `function_index`".to_string());
        }
        (true, None) => unreachable!("exact fields checked"),
    };

    let host_roles = manifest
        .get("hostRoleTable")
        .ok_or_else(|| "cert-manifest.json is missing object field `hostRoleTable`".to_string())?;
    // `null` declares the absence of a host-role table (a module without the
    // Int carrier); the Lean witness pins that declaration against the byte
    // decoder returning `none`, so it stays exactly as constraining as the
    // `some`-table case.
    let host_role_table = if host_roles.is_null() {
        None
    } else {
        exact_object_fields(
            host_roles,
            "hostRoleTable",
            &["box", "add", "mul", "sub", "toIndex", "cmp", "eq"],
        )?;
        let optional_index = |key: &str| -> Result<Option<u32>, String> {
            match &host_roles[key] {
                Value::Null => Ok(None),
                value => Ok(Some(value_u32(value, &format!("hostRoleTable.{key}"))?)),
            }
        };
        Some((
            optional_index("box")?,
            optional_index("add")?,
            optional_index("mul")?,
            optional_index("sub")?,
            optional_index("toIndex")?,
            optional_index("cmp")?,
            optional_index("eq")?,
        ))
    };

    let string_roles_json = manifest
        .get("stringHostRoles")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `stringHostRoles`".to_string())?;
    let mut string_host_roles = Vec::with_capacity(string_roles_json.len());
    for (index, entry) in string_roles_json.iter().enumerate() {
        exact_object_fields(
            entry,
            &format!("stringHostRoles[{index}]"),
            &["function_index", "role"],
        )?;
        let function_index = value_u32(
            &entry["function_index"],
            &format!("stringHostRoles[{index}].function_index"),
        )?;
        let role = match entry.get("role").and_then(Value::as_str) {
            Some("stringEq") => StringHostRole::Eq,
            Some("stringConcat") => StringHostRole::Concat,
            _ => {
                return Err(format!(
                    "stringHostRoles[{index}].role must be stringEq or stringConcat"
                ));
            }
        };
        string_host_roles.push((function_index, role));
    }

    let candidates = Candidates {
        certified,
        laws,
        source_bridges,
        contracts,
        declared_uncertified,
        capabilities,
        start,
        host_role_table,
        string_host_roles,
        target: identity.target,
        profile: identity.profile,
        abi: identity.abi,
        wasip2_component_envelope,
    };
    gate_candidates(&candidates)?;
    Ok(candidates)
}

/// Validate one manifest law-claim before any of its fields reach the
/// checker-authored Lean witness. The names must be plain dotted Lean
/// identifiers, the corollary must be exactly the label's underscore
/// flattening, and the statement — which the witness re-elaborates verbatim
/// inside one `example` type — must stay a single term-position line: no
/// newline, no `:=`, no comment openers, so a crafted statement cannot
/// terminate the pin early or smuggle in a further declaration.
fn validate_law_candidate(mut law: LawCandidate) -> Result<LawCandidate, String> {
    let plain_dotted = |value: &str, field: &str| -> Result<(), String> {
        let ok =
            !value.is_empty() && value.len() <= 200 && value.split('.').all(|segment| {
                let mut chars = segment.chars();
                matches!(chars.next(), Some(first) if first.is_ascii_alphabetic() || first == '_')
                    && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            });
        if ok {
            Ok(())
        } else {
            Err(format!(
                "law-claim `{}` field `{field}` is not a plain dotted Lean identifier",
                law.label
            ))
        }
    };
    plain_dotted(&law.label, "label")?;
    plain_dotted(&law.theorem, "theorem")?;
    plain_dotted(&law.corollary, "corollary")?;
    if law.corollary != law.label.replace('.', "_") {
        return Err(format!(
            "law-claim `{}` corollary `{}` is not the label's flattening",
            law.label, law.corollary
        ));
    }
    if !statement_is_single_plain_line(&law.statement) {
        return Err(format!(
            "law-claim `{}` statement is not a single plain term-position line",
            law.label
        ));
    }
    law.prefix = law
        .theorem
        .rsplit_once('.')
        .map(|(prefix, _)| prefix.to_string())
        .unwrap_or_default();
    Ok(law)
}

/// Longest statement any pinned claim surface transports. It bounds what the
/// anti-injection gate has to police inside one pinned type.
const MAX_STATEMENT_LEN: usize = MAX_BRIDGE_STATEMENT_LEN;

/// The statement gate every pinned claim surface applies: one plain
/// term-position line — no newline or other control character, no `:=`, no
/// comment opener — with balanced `()[]{}⟨⟩` whose depth never goes negative.
///
/// Balance is load-bearing, not cosmetic: the witness wraps the statement in
/// one `(...)`, so a statement whose delimiters close more than they open could
/// escape that wrapping and re-associate the pin's `∧ Holds` conjunct.
fn statement_is_single_plain_line(statement: &str) -> bool {
    bridge_statement::statement_is_single_plain_line(statement, MAX_STATEMENT_LEN)
}

/// The raw structure of one manifest `sourceBridges[i]` entry, before the
/// checker renders its statement.
struct RawSourceBridge {
    export: String,
    theorem: String,
    corollary: String,
    model: String,
    params: Vec<SourceEncoder>,
    result: SourceEncoder,
}

/// Turn one manifest source-bridge entry into the candidate the witness pins,
/// RENDERING its statement rather than accepting one.
///
/// This is the whole difference between a bridge that means something and a
/// bridge that only looks like it does. A statement read out of the manifest
/// need only satisfy the gates to be pinned and credited — and `_root_.M.f x =
/// _root_.M.f x` satisfies every one of them while proving nothing. So the
/// manifest declares structure and the checker renders the text: the export
/// fixes the plan, the closed encoder set fixes the representation, and the
/// only freedom a package has is which of the statements this checker can write
/// it claims. The gates below still run — on the RENDERED text — because they
/// are what makes the pin's shape independent of any future encoder.
fn validate_source_bridge_candidate(
    bridge: RawSourceBridge,
) -> Result<SourceBridgeCandidate, String> {
    let plain = |value: &str| {
        !value.is_empty()
            && value.len() <= MAX_CANDIDATE_LEN
            && value.split('.').all(|segment| {
                let mut chars = segment.chars();
                matches!(chars.next(), Some(first) if first.is_ascii_alphabetic() || first == '_')
                    && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            })
    };
    if !plain(&bridge.export) || bridge.export.contains('.') {
        return Err(format!(
            "source-bridge export `{}` is not a plain Lean identifier",
            display_safe(&bridge.export)
        ));
    }
    if !plain(&bridge.model) {
        return Err(format!(
            "source-bridge `{}` model `{}` is not a plain dotted Lean identifier",
            display_safe(&bridge.export),
            display_safe(&bridge.model)
        ));
    }
    let expected_theorem = format!("{BRIDGE_NAMESPACE}.{}", bridge.export);
    let expected_corollary = format!("{expected_theorem}{BRIDGE_COROLLARY_SUFFIX}");
    if bridge.theorem != expected_theorem || bridge.corollary != expected_corollary {
        return Err(format!(
            "source-bridge `{}` must declare theorem `{expected_theorem}` and corollary \
             `{expected_corollary}`",
            display_safe(&bridge.export)
        ));
    }
    // Every name an encoder splices into the rendered statement must be a
    // `_root_.`-qualified plain identifier, and a record's accessors must be
    // fields OF the type it declares. The renderer copies them verbatim.
    for (position, encoder) in bridge
        .params
        .iter()
        .chain(std::iter::once(&bridge.result))
        .enumerate()
    {
        if !encoder.is_well_formed() {
            let what = if position < bridge.params.len() {
                format!("parameter {position}")
            } else {
                "result".to_string()
            };
            return Err(format!(
                "source-bridge `{}` {what} encoder does not name a `_root_`-qualified type \
                 and its own accessors",
                display_safe(&bridge.export)
            ));
        }
    }
    let statement = render_bridge_statement(
        &bridge.export,
        &bridge.model,
        &bridge.params,
        &bridge.result,
    );
    if !statement_is_single_plain_line(&statement) {
        return Err(format!(
            "source-bridge `{}` renders a statement that is not a single plain \
             term-position line",
            display_safe(&bridge.export)
        ));
    }
    if !statement_is_root_qualified(&statement) {
        return Err(format!(
            "source-bridge `{}` renders a statement naming something that is not \
             `_root_`-qualified",
            display_safe(&bridge.export)
        ));
    }
    Ok(SourceBridgeCandidate {
        export: bridge.export,
        corollary: bridge.corollary,
        model: bridge.model,
        statement,
    })
}

/// Read one declared encoder. The kind set is CLOSED and matched exactly, so an
/// unknown kind — or a record entry missing its type or accessors — declines the
/// package instead of being rendered into some default shape.
fn read_source_encoder(value: &Value, context: &str) -> Result<SourceEncoder, String> {
    let kind = value
        .get(bridge_statement::ENCODER_KIND_KEY)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("cert-manifest.json `{context}.kind` is not a string"))?;
    match kind {
        bridge_statement::ENCODER_KIND_INT => {
            exact_object_fields(value, context, &[bridge_statement::ENCODER_KIND_KEY])?;
            Ok(SourceEncoder::Int)
        }
        bridge_statement::ENCODER_KIND_BOOL => {
            exact_object_fields(value, context, &[bridge_statement::ENCODER_KIND_KEY])?;
            Ok(SourceEncoder::Bool)
        }
        bridge_statement::ENCODER_KIND_RECORD => {
            exact_object_fields(
                value,
                context,
                &[
                    bridge_statement::ENCODER_KIND_KEY,
                    bridge_statement::ENCODER_TYPE_KEY,
                    bridge_statement::ENCODER_FIELDS_KEY,
                ],
            )?;
            let lean_type = required_string(value, bridge_statement::ENCODER_TYPE_KEY, context)?;
            let declared = value[bridge_statement::ENCODER_FIELDS_KEY]
                .as_array()
                .ok_or_else(|| format!("cert-manifest.json `{context}.fields` is not an array"))?;
            let mut accessors = Vec::with_capacity(declared.len());
            for field in declared {
                accessors.push(field.as_str().map(str::to_string).ok_or_else(|| {
                    format!("cert-manifest.json `{context}.fields[]` is not a string")
                })?);
            }
            Ok(SourceEncoder::Record {
                lean_type,
                accessors,
            })
        }
        other => Err(format!(
            "cert-manifest.json `{context}` declares unknown source-bridge encoder kind `{}`",
            display_safe(other)
        )),
    }
}

/// Read the checker witness's per-pin axiom audit back out of its elaboration
/// output. Exactly one line per declared claim is required, in the grammar the
/// witness emits:
///
/// ```text
/// AVER_LAW_AUDIT AverCertChecker.law_pin_<i> ok
/// AVER_LAW_AUDIT AverCertChecker.law_pin_<i> axioms <name>[,<name>...]
/// ```
///
/// Everything else — a missing pin, a repeated pin, an unknown index, a
/// malformed tail — is an error, which declines the package. Credit is only
/// ever granted by a well-formed `ok` line, so a Lean version that stopped
/// logging, a truncated pipe, or a renamed marker costs the claims their
/// credit instead of handing them credit for free.
///
/// The `ok` verdict is a keyword in its own field rather than "no axioms
/// listed": a user axiom literally named `ok` would otherwise render a
/// not-credited line that reads exactly like a credited one.
fn parse_law_audits(output: &str, laws: &[LawCandidate]) -> Result<Vec<LawOutcome>, String> {
    let audited = parse_pin_audits(
        output,
        LAW_AUDIT_MARKER,
        LAW_PIN_PREFIX,
        "law-claim",
        laws.len(),
    )?;
    let mut outcomes = Vec::with_capacity(laws.len());
    for (index, (law, audit)) in laws.iter().zip(audited).enumerate() {
        let offending = audit.ok_or_else(|| {
            format!(
                "checker witness reported no axiom audit for law-claim `{}` \
                 (pin {LAW_PIN_PREFIX}{index}); refusing to credit an unaudited claim",
                display_safe(&law.label)
            )
        })?;
        outcomes.push(LawOutcome {
            label: law.label.clone(),
            offending,
        });
    }
    Ok(outcomes)
}

/// The bridged-corollary twin of [`parse_law_audits`], over the law-claims that
/// declare bridges. Same fail-closed rule: only a well-formed `ok` line credits.
///
/// This surface exists so that a bridge losing its axiom audit costs exactly
/// this claim — the law's own pin above is proved without any bridge, so it
/// keeps its credit whatever the bridges do.
fn parse_bridged_law_audits(
    output: &str,
    laws: &[LawCandidate],
) -> Result<Vec<LawOutcome>, String> {
    let bridged = bridged_law_indices(laws);
    let audited = parse_pin_audits(
        output,
        LAW_BRIDGE_AUDIT_MARKER,
        BRIDGED_LAW_PIN_PREFIX,
        "bridged law-claim",
        bridged.len(),
    )?;
    let mut outcomes = Vec::with_capacity(bridged.len());
    for (position, (at, audit)) in bridged.into_iter().zip(audited).enumerate() {
        let law = &laws[at];
        let offending = audit.ok_or_else(|| {
            format!(
                "checker witness reported no axiom audit for bridged law-claim `{}` \
                 (pin {BRIDGED_LAW_PIN_PREFIX}{position}); refusing to credit an unaudited claim",
                display_safe(&law.label)
            )
        })?;
        outcomes.push(LawOutcome {
            label: law.label.clone(),
            offending,
        });
    }
    Ok(outcomes)
}

/// The source-bridge twin of [`parse_law_audits`], on the bridge marker and
/// pin names. Same fail-closed rule: only a well-formed `ok` line credits.
fn parse_bridge_audits(
    output: &str,
    bridges: &[SourceBridgeCandidate],
) -> Result<Vec<BridgeOutcome>, String> {
    let audited = parse_pin_audits(
        output,
        BRIDGE_AUDIT_MARKER,
        BRIDGE_PIN_PREFIX,
        "source-bridge",
        bridges.len(),
    )?;
    let mut outcomes = Vec::with_capacity(bridges.len());
    for (index, (bridge, audit)) in bridges.iter().zip(audited).enumerate() {
        let offending = audit.ok_or_else(|| {
            format!(
                "checker witness reported no axiom audit for source-bridge `{}` \
                 (pin {BRIDGE_PIN_PREFIX}{index}); refusing to credit an unaudited bridge",
                display_safe(&bridge.export)
            )
        })?;
        outcomes.push(BridgeOutcome {
            export: bridge.export.clone(),
            model: bridge.model.clone(),
            statement: bridge.statement.clone(),
            offending,
        });
    }
    Ok(outcomes)
}

/// Shared readback of the witness's per-pin axiom audit lines. `kind` names the
/// surface in error text; `expected` is how many pins must have reported.
fn parse_pin_audits(
    output: &str,
    marker: &str,
    pin_prefix: &str,
    kind: &str,
    expected: usize,
) -> Result<Vec<Option<Vec<String>>>, String> {
    let mut audited: Vec<Option<Vec<String>>> = vec![None; expected];
    for line in output.lines() {
        let Some((_, tail)) = line.split_once(marker) else {
            continue;
        };
        let fields: Vec<&str> = tail.split_whitespace().collect();
        let malformed = || {
            format!(
                "checker witness emitted a malformed {kind} audit line: {}",
                display_safe(line.trim())
            )
        };
        let (pin, offending): (&str, Vec<String>) = match fields.as_slice() {
            [pin, "ok"] => (pin, Vec::new()),
            [pin, "axioms", names] => (pin, names.split(',').map(str::to_string).collect()),
            _ => return Err(malformed()),
        };
        // An empty component means the witness named an axiom it could not
        // render (or rendered a stray separator); that is not an audit.
        if offending.iter().any(String::is_empty) {
            return Err(malformed());
        }
        let index = pin
            .strip_prefix(pin_prefix)
            .and_then(|index| index.parse::<usize>().ok())
            .filter(|index| *index < expected)
            .ok_or_else(|| {
                format!(
                    "checker witness audited an unknown {kind} pin `{}`",
                    display_safe(pin)
                )
            })?;
        if audited[index].is_some() {
            return Err(format!(
                "checker witness audited {kind} pin `{}` more than once",
                display_safe(pin)
            ));
        }
        audited[index] = Some(offending);
    }
    Ok(audited)
}

fn parse_termination(value: Option<&Value>, export: &str) -> Result<String, String> {
    let Some(value) = value else {
        return Ok("none".to_string());
    };
    let measure = value
        .get("measure")
        .and_then(Value::as_object)
        .ok_or_else(|| format!("export `{export}` has malformed termination measure"))?;
    if measure.get("kind").and_then(Value::as_str) != Some("intNatAbs") {
        return Err(format!(
            "export `{export}` uses an unsupported termination measure"
        ));
    }
    let parameter = measure
        .get("param_index")
        .and_then(Value::as_u64)
        .and_then(|value| u32::try_from(value).ok())
        .ok_or_else(|| format!("export `{export}` has invalid termination parameter"))?;
    let descent = value
        .get("descent")
        .and_then(Value::as_i64)
        .ok_or_else(|| format!("export `{export}` has invalid termination descent"))?;
    Ok(format!(
        "some ({{ measure := .intNatAbs {parameter}, descent := ({descent} : Int) }} : AverCert.Schema.TerminationWitness)"
    ))
}

fn required_string(value: &Value, key: &str, context: &str) -> Result<String, String> {
    value
        .get(key)
        .and_then(Value::as_str)
        .map(str::to_string)
        .ok_or_else(|| format!("cert-manifest.json `{context}.{key}` is not a string"))
}

fn string_array(manifest: &Value, key: &str) -> Result<Vec<String>, String> {
    manifest
        .get(key)
        .and_then(Value::as_array)
        .ok_or_else(|| format!("cert-manifest.json is missing array field `{key}`"))?
        .iter()
        .map(|value| {
            value
                .as_str()
                .map(str::to_string)
                .ok_or_else(|| format!("cert-manifest.json `{key}[]` is not a string"))
        })
        .collect()
}

fn object_pair_array(
    manifest: &Value,
    key: &str,
    left: &str,
    right: &str,
) -> Result<Vec<(String, String)>, String> {
    manifest
        .get(key)
        .and_then(Value::as_array)
        .ok_or_else(|| format!("cert-manifest.json is missing array field `{key}`"))?
        .iter()
        .enumerate()
        .map(|(index, value)| {
            exact_object_fields(value, &format!("{key}[{index}]"), &[left, right])?;
            Ok((
                required_string(value, left, &format!("{key}[{index}]"))?,
                required_string(value, right, &format!("{key}[{index}]"))?,
            ))
        })
        .collect()
}

fn exact_object_fields(value: &Value, context: &str, expected: &[&str]) -> Result<(), String> {
    let object = value
        .as_object()
        .ok_or_else(|| format!("cert-manifest.json `{context}` is not an object"))?;
    if object.len() != expected.len() || expected.iter().any(|key| !object.contains_key(*key)) {
        return Err(format!(
            "cert-manifest.json `{context}` must contain exactly fields {}",
            expected.join(", ")
        ));
    }
    Ok(())
}

fn value_u32(value: &Value, context: &str) -> Result<u32, String> {
    value
        .as_u64()
        .and_then(|value| u32::try_from(value).ok())
        .ok_or_else(|| format!("cert-manifest.json `{context}` must be a u32"))
}

fn gate_candidates(candidates: &Candidates) -> Result<(), String> {
    for candidate in &candidates.certified {
        gate_candidate("certified export name", &candidate.name)?;
        gate_candidate("certified class", &candidate.class)?;
        gate_candidate("source domain", &candidate.dom)?;
        gate_candidate("source codomain", &candidate.cod)?;
    }
    for contract in &candidates.contracts {
        gate_candidate("runtime contract", contract)?;
    }
    for (name, reason) in &candidates.declared_uncertified {
        gate_candidate("declared-uncertified name", name)?;
        gate_candidate("declared-uncertified reason", reason)?;
    }
    for (module, name) in &candidates.capabilities {
        gate_candidate("capability module", module)?;
        gate_candidate("capability name", name)?;
    }
    gate_candidate("target", &candidates.target)?;
    gate_candidate("profile", &candidates.profile)?;
    gate_candidate("abi", &candidates.abi)
}

fn gate_candidate(kind: &str, value: &str) -> Result<(), String> {
    let safe = value.len() <= MAX_CANDIDATE_LEN
        && value
            .bytes()
            .all(|byte| (0x20..=0x7e).contains(&byte) && byte != b'"' && byte != b'\\');
    if safe {
        Ok(())
    } else {
        Err(format!(
            "certificate {kind} is outside the allowed printable ASCII subset: {value:?}"
        ))
    }
}

fn is_checker_owned(name: &str, selected_wall: &wall::Wall) -> bool {
    selected_wall
        .sources
        .iter()
        .any(|source| source.name == name)
        || matches!(
            name,
            "ArtifactBytes.lean"
                | "ArtifactComponentBytes.lean"
                | "lakefile.lean"
                | "CheckerWitness.lean"
        )
}

fn assemble_build(
    cert_dir: &Path,
    core_module_bytes: &[u8],
    target_artifact_bytes: &[u8],
    selected_wall: &wall::Wall,
    memory_limit_mb: u64,
) -> Result<BuildDir, String> {
    let build = BuildDir::new()?;
    let mut roots = Vec::new();
    let mut flat_files: Vec<(String, PathBuf)> = Vec::new();
    let mut subdirectories: Vec<(String, PathBuf)> = Vec::new();
    let entries = std::fs::read_dir(cert_dir)
        .map_err(|error| format!("cannot read cert dir {}: {error}", cert_dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|error| format!("cert dir read: {error}"))?;
        let Ok(kind) = entry.file_type() else {
            continue;
        };
        let name = entry.file_name().to_string_lossy().into_owned();
        if kind.is_dir() {
            // Dot-directories (`.lake`, `.git`) are build products or local
            // state, never certificate data; skipping them entirely keeps a
            // shipped cache out of the staged tree.
            if !name.starts_with('.') {
                subdirectories.push((name, entry.path()));
            }
            continue;
        }
        if !kind.is_file() {
            continue;
        }
        if !name.ends_with(".lean") || is_checker_owned(&name, selected_wall) {
            continue;
        }
        flat_files.push((name, entry.path()));
    }

    // Top-level package files stage unconditionally, exactly as before. The
    // import lines of the package's own `Manifest.lean` and `Certificate.lean`
    // are collected from the very bytes being staged — never from a second
    // read — so a concurrent writer cannot desync the nested admission list
    // from the staged tree.
    let mut staged_paths: std::collections::BTreeMap<String, String> =
        std::collections::BTreeMap::new();
    let mut admitted: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for (name, path) in &flat_files {
        let root = lean_module_root(name)?;
        reject_shadowed_root(&root, selected_wall)?;
        note_staged_path(&mut staged_paths, name)?;
        let contents = std::fs::read(path)
            .map_err(|error| format!("cannot read cert file {name}: {error}"))?;
        scan_for_code_exec(name, &contents)?;
        if name == "Manifest.lean" || name == "Certificate.lean" {
            collect_import_lines(&String::from_utf8_lossy(&contents), &mut admitted);
        }
        std::fs::write(build.path.join(name), contents)
            .map_err(|error| format!("cannot stage {name}: {error}"))?;
        roots.push(root);
    }

    // A NESTED `.lean` file stages only when the package's own staged
    // `Manifest.lean` or `Certificate.lean` imports its dotted module name
    // (the producer imports every model root from those two files, so one
    // level suffices — no transitive closure). That admission list is
    // authored by the UNTRUSTED producer, so it is build-set minimization —
    // it keeps decoy trees and stray sidecars out of the build — NOT a
    // security boundary. Safety rests on the per-file gates every staged
    // file passes (per-segment name validation, shadow-prefix rejection, the
    // code-execution token scan) and, behind them, on the checker-authored
    // witness: only facts inside the `CheckerWitness` import cone, accepted
    // by the kernel under the axiom whitelist, reach the verdict.
    let mut nested_files: Vec<(String, PathBuf)> = Vec::new();
    for (name, path) in &subdirectories {
        collect_nested_lean_files(path, name, 1, &mut nested_files)?;
    }
    // Stage in sorted relative-path order so the staged tree (and any error
    // chosen among several candidates) does not depend on directory
    // iteration order.
    nested_files.sort();
    for (relative, path) in &nested_files {
        let root = lean_module_root(relative)?;
        reject_shadowed_root(&root, selected_wall)?;
        if !admitted.contains(&root) {
            continue;
        }
        note_staged_path(&mut staged_paths, relative)?;
        let contents = std::fs::read(path)
            .map_err(|error| format!("cannot read cert file {relative}: {error}"))?;
        scan_for_code_exec(relative, &contents)?;
        let destination = relative
            .split('/')
            .fold(build.path.clone(), |path, segment| path.join(segment));
        if let Some(parent) = destination.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|error| format!("cannot stage {relative}: {error}"))?;
        }
        std::fs::write(destination, contents)
            .map_err(|error| format!("cannot stage {relative}: {error}"))?;
        roots.push(root);
    }
    for source in selected_wall.sources {
        std::fs::write(build.path.join(source.name), source.contents)
            .map_err(|error| format!("cannot stage {}: {error}", source.name))?;
        roots.push(
            source
                .name
                .strip_suffix(".lean")
                .expect("wall source is Lean")
                .to_string(),
        );
    }
    std::fs::write(
        build.path.join("ArtifactBytes.lean"),
        wall::render_artifact_bytes(core_module_bytes),
    )
    .map_err(|error| format!("cannot stage ArtifactBytes.lean: {error}"))?;
    roots.push("ArtifactBytes".to_string());
    std::fs::write(
        build.path.join("ArtifactComponentBytes.lean"),
        wall::render_artifact_component_bytes(target_artifact_bytes),
    )
    .map_err(|error| format!("cannot stage ArtifactComponentBytes.lean: {error}"))?;
    roots.push("ArtifactComponentBytes".to_string());
    roots.sort();
    roots.dedup();
    std::fs::write(
        build.path.join("lakefile.lean"),
        checker_lakefile(&roots, memory_limit_mb),
    )
    .map_err(|error| format!("cannot write checker lakefile: {error}"))?;
    std::fs::write(build.path.join("lean-toolchain"), selected_wall.toolchain)
        .map_err(|error| format!("cannot write lean-toolchain: {error}"))?;
    Ok(build)
}

/// Validate a package file name and return its Lean module root. A flat
/// `Store.lean` yields `Store`; a nested `Apps/Notepad/Store.lean` yields the
/// dotted `Apps.Notepad.Store`. Every `/`-separated segment must satisfy the
/// same identifier rule (the file name additionally carries the `.lean`
/// suffix). The rule is simultaneously the traversal guard — an accepted
/// segment cannot be `.`, `..`, empty, absolute, or anything other than a
/// plain `std::path::Component::Normal` — and the lakefile-injection guard:
/// the returned root is interpolated unescaped into the checker-authored
/// lakefile, so only validated segments may become roots.
fn lean_module_root(name: &str) -> Result<String, String> {
    let stem = name
        .strip_suffix(".lean")
        .ok_or_else(|| format!("cert file `{name}` is not a Lean file"))?;
    let segments: Vec<&str> = stem.split('/').collect();
    let valid = segments.iter().all(|segment| {
        let mut chars = segment.chars();
        matches!(chars.next(), Some(first) if first.is_ascii_alphabetic())
            && chars.all(|character| character.is_ascii_alphanumeric() || character == '_')
    });
    if valid {
        Ok(segments.join("."))
    } else {
        Err(format!(
            "cert file name `{name}` must match ^[A-Za-z][A-Za-z0-9_]*\\.lean$ in every path segment"
        ))
    }
}

/// Reject a package module root that would shadow a checker-owned or
/// toolchain module. The check covers the full dotted name and every dotted
/// prefix of it: `Lean/Extra.lean` (root `Lean.Extra`) is rejected exactly
/// like a flat `Lean.lean`, because staging it would plant files under a
/// directory the toolchain or the checker-owned wall claims.
fn reject_shadowed_root(root: &str, selected_wall: &wall::Wall) -> Result<(), String> {
    let segments: Vec<&str> = root.split('.').collect();
    for length in 1..=segments.len() {
        let prefix = segments[..length].join(".");
        let shadows_toolchain = TOOLCHAIN_ROOTS
            .iter()
            .any(|reserved| reserved.eq_ignore_ascii_case(&prefix));
        let shadows_checker = selected_wall.sources.iter().any(|source| {
            source
                .name
                .strip_suffix(".lean")
                .is_some_and(|reserved| reserved.eq_ignore_ascii_case(&prefix))
        }) || [
            "ArtifactBytes",
            "ArtifactComponentBytes",
            "CheckerWitness",
            "lakefile",
        ]
        .iter()
        .any(|reserved| reserved.eq_ignore_ascii_case(&prefix));
        if shadows_toolchain || shadows_checker {
            return Err(format!(
                "cert data module `{root}` shadows a checker/toolchain import"
            ));
        }
    }
    Ok(())
}

/// Collect the module names named by literal `import` lines in one staged
/// file's text. The scan is deliberately LITERAL: each line is trimmed, a
/// leading `import ` prefix is stripped, and the trimmed remainder is the
/// module name. Lean comments are NOT parsed — an import line inside a block
/// comment still admits a nested file. That is acceptable because the
/// admission list is build-set minimization, not a security gate (see the
/// nested-staging comment in `assemble_build`); a conforming
/// reimplementation must match this scan exactly.
fn collect_import_lines(text: &str, admitted: &mut std::collections::BTreeSet<String>) {
    for line in text.lines() {
        if let Some(rest) = line.trim().strip_prefix("import ") {
            let module = rest.trim();
            if !module.is_empty() {
                admitted.insert(module.to_string());
            }
        }
    }
}

/// Record one staged relative path, rejecting any pair of staged paths that
/// are equal ASCII-case-insensitively. On a case-insensitive staging
/// filesystem (APFS, NTFS) `Apps/Store.lean` and `apps/Store.lean` silently
/// merge and the later write clobbers the earlier one, so which bytes build
/// would depend on directory iteration order; failing closed keeps the
/// staged tree identical on every filesystem.
fn note_staged_path(
    staged: &mut std::collections::BTreeMap<String, String>,
    relative: &str,
) -> Result<(), String> {
    if let Some(previous) = staged.insert(relative.to_ascii_lowercase(), relative.to_string()) {
        return Err(format!(
            "cert files `{previous}` and `{relative}` collide case-insensitively"
        ));
    }
    Ok(())
}

/// Depth cap for the nested package walk. The certificate directory is the
/// one tree whose recursion depth an untrusted party chooses; a pathological
/// directory chain must fail loudly instead of exhausting the checker's
/// stack. Sixteen levels is far beyond any real module tree.
const MAX_NESTED_DEPTH: usize = 16;

/// Collect `Sub/.../Name.lean` files under one first-level subdirectory of
/// the certificate package. `relative` is the `/`-joined path walked so far
/// and `depth` counts directory levels below the package root (the first
/// subdirectory is depth 1). Dot-directories are skipped entirely at every
/// depth (`.lake` caches are never certificate data), non-file,
/// non-directory entries are ignored like their top-level counterparts, and
/// nesting past `MAX_NESTED_DEPTH` is a hard error.
fn collect_nested_lean_files(
    dir: &Path,
    relative: &str,
    depth: usize,
    out: &mut Vec<(String, PathBuf)>,
) -> Result<(), String> {
    if depth > MAX_NESTED_DEPTH {
        return Err(format!(
            "cert subdirectory `{relative}` exceeds the maximum nesting depth of {MAX_NESTED_DEPTH}"
        ));
    }
    let entries = std::fs::read_dir(dir)
        .map_err(|error| format!("cannot read cert dir {}: {error}", dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|error| format!("cert dir read: {error}"))?;
        let Ok(kind) = entry.file_type() else {
            continue;
        };
        let name = entry.file_name().to_string_lossy().into_owned();
        if kind.is_dir() {
            if !name.starts_with('.') {
                collect_nested_lean_files(
                    &entry.path(),
                    &format!("{relative}/{name}"),
                    depth + 1,
                    out,
                )?;
            }
        } else if kind.is_file() && name.ends_with(".lean") {
            out.push((format!("{relative}/{name}"), entry.path()));
        }
    }
    Ok(())
}

/// Reject a cert data file that carries an elaboration-executing token in
/// *code* position. This is a fail-closed trust-boundary defense: the scanner's
/// notion of "this span is an inert string or comment" is a deliberate SOUND
/// OVER-APPROXIMATION of code — on any lexical ambiguity it defaults to code and
/// scans, so a token Lean would elaborate is never skipped as inert. It may
/// over-reject (treat inert bytes as code) but must never under-reject.
///
/// Inert spans recognized (and only these): normal string literals `"..."` with
/// `\` escapes, line comments `-- ... \n`, and nested block comments
/// `/- ... -/` (which also covers the `/--`/`/-!` doc-comment openers). Char
/// literals are consumed as code just far enough that a `"` inside `'"'` / `'\"'`
/// cannot open a phantom string. Raw / interpolated string prefixes (`r"`,
/// `r#"`, `s!"`) and unterminated strings/comments fall back to scanning the
/// remainder as pure code.
fn scan_for_code_exec(name: &str, contents: &[u8]) -> Result<(), String> {
    let text = String::from_utf8_lossy(contents);
    let chars: Vec<char> = text.chars().collect();
    if let Some(token) = find_code_exec_token(&chars) {
        return Err(format!(
            "cert data file `{name}` contains elaboration-executing token `{token}`"
        ));
    }
    Ok(())
}

/// A Lean identifier-continuation character, narrowed to ASCII alphanumerics and
/// `_`. This is intentionally an UNDER-approximation of Lean's identifier
/// alphabet: it is used only for the word-boundary check, and treating fewer
/// characters as identifier-continuation makes the scanner *more* likely to
/// reject (fail-closed), never less.
fn is_ident_continuation(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// A forbidden token is treated as a whole *word* (boundary-checked so `elab`
/// does not fire inside `relabel`) exactly when every one of its bytes is an
/// ASCII identifier-continuation character. Tokens carrying punctuation, spaces,
/// or non-ASCII bytes (`#eval`, `@[`, `«`, `open Lean`) are matched as raw
/// substrings in code position, where a word boundary has no meaning.
fn token_is_word(token: &str) -> bool {
    token
        .bytes()
        .all(|b| b.is_ascii_alphanumeric() || b == b'_')
}

/// Returns the offending token if one starts, in code position, at `chars[i]`.
fn token_at(
    tokens: &[(&'static str, Vec<char>, bool)],
    chars: &[char],
    i: usize,
) -> Option<&'static str> {
    for (token, needle, is_word) in tokens {
        let len = needle.len();
        if i + len > chars.len() || &chars[i..i + len] != needle.as_slice() {
            continue;
        }
        if *is_word {
            let left_boundary = i == 0 || !is_ident_continuation(chars[i - 1]);
            let right_boundary = i + len == chars.len() || !is_ident_continuation(chars[i + len]);
            if left_boundary && right_boundary {
                return Some(token);
            }
        } else {
            return Some(token);
        }
    }
    None
}

/// Index just past the closing `"` of the normal string literal opening at
/// `chars[open]`, or `None` if the string never closes before EOF (an
/// unterminated string is a lexer error in Lean; the caller then defaults to
/// scanning the region as code).
fn string_literal_end(chars: &[char], open: usize) -> Option<usize> {
    let mut j = open + 1;
    while j < chars.len() {
        match chars[j] {
            '\\' => j += 2, // the escaped character cannot close the string
            '"' => return Some(j + 1),
            _ => j += 1,
        }
    }
    None
}

/// Index just past the matching `-/` of the (nesting) block comment opening at
/// `chars[open]` (`/-`), or `None` if it never closes before EOF.
fn block_comment_end(chars: &[char], open: usize) -> Option<usize> {
    let mut depth = 1usize;
    let mut j = open + 2;
    while j < chars.len() {
        if chars[j] == '/' && j + 1 < chars.len() && chars[j + 1] == '-' {
            depth += 1;
            j += 2;
        } else if chars[j] == '-' && j + 1 < chars.len() && chars[j + 1] == '/' {
            depth -= 1;
            j += 2;
            if depth == 0 {
                return Some(j);
            }
        } else {
            j += 1;
        }
    }
    None
}

/// Index just past a char literal opening at `chars[open]` (`'`), or `None` if
/// `chars[open]` is not the start of a char literal we recognize. Recognition is
/// deliberately minimal: its only soundness duty is to consume the `"` inside
/// `'"'` and `'\"'` so it cannot open a phantom string. Every char literal that
/// can contain a raw `"` byte matches one of those two shapes; other char
/// literals (`'\n'`, `'\u{22}'`, identifier primes) may go unrecognized, which
/// is harmless because they carry no `"`.
fn char_literal_end(chars: &[char], open: usize) -> Option<usize> {
    if chars.get(open + 1) == Some(&'\\') {
        // '\X'  (escaped single char, e.g. '\"', '\n', '\\', '\'')
        if chars.get(open + 2).is_some() && chars.get(open + 3) == Some(&'\'') {
            return Some(open + 4);
        }
        return None;
    }
    match chars.get(open + 1) {
        Some('\'') | None => None, // "''" is not a char literal; nor is a trailing '
        Some(_) => {
            // 'X'  (single unescaped char, including 'X' == '"')
            if chars.get(open + 2) == Some(&'\'') {
                Some(open + 3)
            } else {
                None
            }
        }
    }
}

/// Scan `chars[start..]` as pure code (no string/comment skipping) and return
/// the first forbidden token. Used as the default-to-code fallback for
/// unterminated strings/comments and raw/interpolated string prefixes.
fn scan_remainder_as_code(
    tokens: &[(&'static str, Vec<char>, bool)],
    chars: &[char],
    start: usize,
) -> Option<&'static str> {
    for i in start..chars.len() {
        if let Some(token) = token_at(tokens, chars, i) {
            return Some(token);
        }
    }
    None
}

/// The context-aware core of [`scan_for_code_exec`]: a mini Lean lexer that
/// walks the file, skips inert string/comment spans, and reports the first
/// forbidden token that appears in code position.
fn find_code_exec_token(chars: &[char]) -> Option<&'static str> {
    let tokens: Vec<(&'static str, Vec<char>, bool)> = CODE_EXEC_TOKENS
        .iter()
        .map(|token| (*token, token.chars().collect(), token_is_word(token)))
        .collect();
    let n = chars.len();
    let mut i = 0;
    while i < n {
        let c = chars[i];
        // Inert-span openers take priority. None of them is a token start, so
        // handling them here never skips over a forbidden token.
        if c == '"' {
            // A `"` preceded by a raw/interpolated string prefix (`r"`, `r#"`,
            // `s!"`) is lexically ambiguous for a normal-string scan; default to
            // code and scan the remainder rather than risk a desynced skip.
            if i > 0 && matches!(chars[i - 1], 'r' | '#' | '!') {
                return scan_remainder_as_code(&tokens, chars, i);
            }
            match string_literal_end(chars, i) {
                Some(end) => {
                    i = end;
                    continue;
                }
                None => return scan_remainder_as_code(&tokens, chars, i),
            }
        }
        if c == '-' && chars.get(i + 1) == Some(&'-') {
            // Line comment through end of line (or EOF).
            let mut j = i + 2;
            while j < n && chars[j] != '\n' {
                j += 1;
            }
            i = j;
            continue;
        }
        if c == '/' && chars.get(i + 1) == Some(&'-') {
            match block_comment_end(chars, i) {
                Some(end) => {
                    i = end;
                    continue;
                }
                None => return scan_remainder_as_code(&tokens, chars, i),
            }
        }
        // A `'` that opens a char literal is consumed; otherwise it is an
        // identifier prime and falls through as ordinary code.
        if c == '\''
            && let Some(end) = char_literal_end(chars, i)
        {
            i = end;
            continue;
        }
        if let Some(token) = token_at(&tokens, chars, i) {
            return Some(token);
        }
        i += 1;
    }
    None
}

/// The generated lakefile carries the checker's Lean heap ceiling into every
/// `lake build` worker via `moreLeanArgs`: Lake 4.32 ignores `LEAN_OPTS`, so
/// this is the only channel that reaches build-spawned lean processes.
fn checker_lakefile(roots: &[String], memory_limit_mb: u64) -> String {
    let roots = roots
        .iter()
        .map(|root| format!("`{root}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n@[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{roots}]\n  moreLeanArgs := #[\"--memory={memory_limit_mb}\"]\n"
    )
}

fn lean_str_list(items: &[String]) -> String {
    format!(
        "[{}]",
        items
            .iter()
            .map(|item| format!("\"{item}\""))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn lean_string_pair_list(items: &[(String, String)]) -> String {
    format!(
        "[{}]",
        items
            .iter()
            .map(|(left, right)| format!("(\"{left}\", \"{right}\")"))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn lean_option_nat(value: Option<u32>) -> String {
    value.map_or_else(|| "none".to_string(), |value| format!("some {value}"))
}

fn lean_wasip2_component_envelope(
    value: Option<format::Wasip2ComponentEnvelopeDeclaration>,
) -> String {
    value.map_or_else(
        || "(none : Option AverCert.Wasip2Envelope.ComponentEnvelope)".to_string(),
        |value| {
            format!(
                "some ({{ prefixLen := {}, embeddedCoreModuleLen := {}, suffixLen := {} }} : AverCert.Wasip2Envelope.ComponentEnvelope)",
                value.prefix_len, value.embedded_core_module_len, value.suffix_len
            )
        },
    )
}

fn sha256_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

struct BuildDir {
    path: PathBuf,
}

impl BuildDir {
    fn new() -> Result<Self, String> {
        let path = checker_temp_root()?.join(format!(
            "aver-cert-check-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        let mut builder = std::fs::DirBuilder::new();
        #[cfg(unix)]
        {
            use std::os::unix::fs::DirBuilderExt;
            builder.mode(0o700);
        }
        builder
            .create(&path)
            .map_err(|error| format!("cannot create checker build dir: {error}"))?;
        Ok(Self { path })
    }
}

fn checker_temp_root() -> Result<PathBuf, String> {
    #[cfg(unix)]
    {
        Ok(PathBuf::from("/tmp"))
    }
    #[cfg(windows)]
    {
        let home = std::env::var_os("USERPROFILE")
            .or_else(|| std::env::var_os("HOME"))
            .ok_or_else(|| {
                "cannot select checker temp root: USERPROFILE/HOME is not set".to_string()
            })?;
        let root = PathBuf::from(home)
            .join("AppData")
            .join("Local")
            .join("Temp");
        std::fs::create_dir_all(&root)
            .map_err(|error| format!("cannot create checker temp root: {error}"))?;
        Ok(root)
    }
    #[cfg(not(any(unix, windows)))]
    {
        let home = std::env::var_os("HOME")
            .ok_or_else(|| "cannot select checker temp root: HOME is not set".to_string())?;
        let root = PathBuf::from(home).join(".aver-cert-tmp");
        std::fs::create_dir_all(&root)
            .map_err(|error| format!("cannot create checker temp root: {error}"))?;
        Ok(root)
    }
}

impl Drop for BuildDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
}

fn unique_nanos() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0)
}

struct LakeOut {
    status: std::process::ExitStatus,
    combined: String,
}

fn run_lake(
    lean: &LeanRunner,
    build_dir: &Path,
    phase: &str,
    arguments: &[&str],
) -> Result<LakeOut, String> {
    // Any step failure — including a timeout — fails the whole verify/check
    // closed; only the opt-in prelude cache may downgrade a step error.
    let output = lean
        .run_lake(build_dir, phase, arguments)
        .map_err(|error| error.to_string())?;
    Ok(LakeOut {
        status: output.status,
        combined: format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ),
    })
}

fn tail(text: &str, lines: usize) -> String {
    let all = text.lines().collect::<Vec<_>>();
    all[all.len().saturating_sub(lines)..].join("\n")
}

/// Surface a failed cert `lake build` so the decline reason names the failing
/// pins. Lake builds independent modules in parallel, so the offending
/// `error: <file>.lean:` diagnostics can be trailed by an unbounded run of
/// build-progress (`✔`/`ℹ`), axiom-`info:` and roll-up lines from modules that
/// happened to finish afterwards; a fixed line window can then bury the
/// diagnostics entirely (which pins failed becomes invisible). Keep the Lean
/// file-diagnostic blocks — each `error: *.lean:` line and its message body —
/// and window those, so the decline stays diagnosable regardless of the
/// parallel-build interleaving. Fall back to the raw tail when the failure
/// carried no file diagnostic (e.g. a lake-level or out-of-memory failure).
fn surface_build_failure(text: &str, lines: usize) -> String {
    let is_progress_or_rollup = |trimmed: &str| {
        trimmed.starts_with('✔')
            || trimmed.starts_with('ℹ')
            || trimmed.starts_with('⚠')
            || trimmed.starts_with("info:")
            || trimmed.starts_with("warning:")
            || trimmed.starts_with("Some required targets")
    };
    let is_file_diagnostic =
        |trimmed: &str| trimmed.starts_with("error:") && trimmed.contains(".lean:");

    let mut diagnostics: Vec<&str> = Vec::new();
    let mut capturing = false;
    for line in text.lines() {
        let trimmed = line.trim_start();
        if is_file_diagnostic(trimmed) {
            capturing = true;
            diagnostics.push(line);
        } else if is_progress_or_rollup(trimmed)
            || (trimmed.starts_with("error:") && !is_file_diagnostic(trimmed))
        {
            // Non-diagnostic lake chrome (`error: build failed`, `error: Lean
            // exited …`) ends the current file-diagnostic block.
            capturing = false;
        } else if capturing {
            diagnostics.push(line);
        }
    }

    if diagnostics.is_empty() {
        // No Lean file diagnostic (a lake-level or OOM failure). The relevant
        // line can sit anywhere in the interleaved output, so keep a generous
        // window rather than the tight decline window.
        return tail(text, lines.max(200));
    }
    // Keep EVERY file diagnostic, never a trailing window of them: the
    // pin-named error that identifies the decline can be the first of several
    // (a tamper often cascades), and parallel module builds make the order
    // non-deterministic, so windowing the diagnostics drops the one a caller
    // needs. The set is bounded by the actual Lean errors, not lake chrome.
    diagnostics.join("\n")
}

fn display_safe(value: &str) -> String {
    value
        .chars()
        .map(|character| {
            // Guard terminal control sequences only: statements and reasons
            // are legitimately unicode (∀, ∧, ≤), and flattening them to `?`
            // makes materially different claims render identically.
            if character.is_control() {
                '?'
            } else {
                character
            }
        })
        .collect()
}

pub fn explain(artifact: &Path, cert_dir: &Path) -> Result<Explanation, String> {
    let report = trusted_check(artifact, cert_dir, ReplayMode::Fresh)?;
    println!("{}", "Artifact certificate".bold());
    println!("  artifact: {}", artifact.display());
    println!("  pinned sha256: {}", report.artifact_hash);
    println!(
        "  target: {}    profile: {}    abi: {}",
        report.target, report.profile, report.abi
    );
    if report.exports.is_empty() {
        println!("\n{}", "NO CERTIFIED EXPORTS".yellow().bold());
        return Ok(Explanation::NoExports);
    }
    println!("\n{}", "CERTIFIED".green().bold());
    for export in report.exports {
        println!("  {}", export.name.bold());
        println!("    policy: {}", export.policy);
        println!("    {}", export.face);
        if let Some(domain) = export.domain.as_deref() {
            println!("    {domain}");
        }
        if let Some(model) = export.certified_model.as_deref() {
            println!("    {model}");
        }
        println!("    {}", export.manifest_face);
    }
    if !report.contracts.is_empty() {
        println!("\n{}", "Runtime contracts".yellow().bold());
        for contract in report.contracts {
            println!("  - {contract}");
        }
    }

    let manifest = read_manifest(cert_dir)?;
    if let Some(laws) = manifest.get("laws").and_then(Value::as_array)
        && !laws.is_empty()
    {
        println!("\n{}", "LAW-CLAIMS".green().bold());
        for entry in laws {
            let label = entry
                .get("label")
                .and_then(Value::as_str)
                .map(display_safe)
                .unwrap_or_else(|| "<unknown>".to_string());
            let statement = entry
                .get("statement")
                .and_then(Value::as_str)
                .map(display_safe)
                .unwrap_or_else(|| "<unknown>".to_string());
            println!("  {}", label.bold());
            println!("    {statement}");
        }
        println!(
            "  these are the DECLARED claims; per-claim credit is decided by \
             `aver cert check` / `aver cert verify`"
        );
    }
    // The bridge statements come from the report, not from a manifest read:
    // the manifest carries structure, and the text below is exactly what the
    // checker rendered from it and pinned the package's corollary at.
    if !report.source_bridges.is_empty() {
        println!("\n{}", "SOURCE-BRIDGES".green().bold());
        for bridge in &report.source_bridges {
            let credit = if bridge.offending.is_empty() {
                "credited".to_string()
            } else {
                format!(
                    "NOT credited (proof depends on {})",
                    display_safe(&bridge.offending.join(", "))
                )
            };
            println!(
                "  {}  ≡ {}  [{credit}]",
                display_safe(&bridge.export).bold(),
                display_safe(&bridge.model)
            );
            println!("    {}", display_safe(&bridge.statement));
        }
        println!(
            "  the statement under each bridge is RENDERED BY THE CHECKER from the \
             manifest's declared structure, never read from the package; a credited \
             bridge is one whose proof of exactly that statement uses no axiom \
             outside the kernel whitelist"
        );
    }
    // Declared-only, like `source_level_only`: why a compute-face export got no
    // bridge at all. The producer's reason used to live on its stdout, which
    // left the package silent about the export whose model stays the plan.
    if let Some(declined) = manifest
        .get("sourceBridgesDeclined")
        .and_then(Value::as_array)
        && !declined.is_empty()
    {
        println!(
            "\n{}",
            "SOURCE-BRIDGES DECLINED (informational)".yellow().bold()
        );
        for entry in declined {
            let export = entry
                .get("export")
                .and_then(Value::as_str)
                .map(display_safe)
                .unwrap_or_else(|| "<unknown>".to_string());
            let reason = entry
                .get("reason")
                .and_then(Value::as_str)
                .map(display_safe)
                .unwrap_or_else(|| "unspecified".to_string());
            println!("  {export}: {reason}");
        }
        println!("  these exports keep `model: plan`; the reasons are declared, not checked");
    }
    if let Some(declined) = manifest.get("source_level_only").and_then(Value::as_array)
        && !declined.is_empty()
    {
        println!("\n{}", "DECLINED (informational)".yellow().bold());
        for entry in declined {
            let name = entry
                .get("name")
                .and_then(Value::as_str)
                .map(display_safe)
                .unwrap_or_else(|| "<unknown>".to_string());
            let reason = entry
                .get("reason")
                .and_then(Value::as_str)
                .map(display_safe)
                .unwrap_or_else(|| "unspecified".to_string());
            println!("  {name}: {reason}");
        }
    }
    Ok(Explanation::Certified)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn law_candidates(labels: &[&str]) -> Vec<LawCandidate> {
        labels
            .iter()
            .map(|label| LawCandidate {
                label: (*label).to_string(),
                theorem: format!("Domain.{label}"),
                statement: "True".to_string(),
                corollary: label.replace('.', "_"),
                prefix: "Domain".to_string(),
                bridges: Vec::new(),
            })
            .collect()
    }

    fn audit_line(index: usize, verdict: &str) -> String {
        format!(
            "CheckerWitness.lean:9:0: info: {LAW_AUDIT_MARKER} {LAW_PIN_PREFIX}{index} {verdict}"
        )
    }

    #[test]
    fn law_audit_credits_only_pins_the_witness_reported_clean() {
        let laws = law_candidates(&["A.one", "A.two"]);
        let output = format!(
            "building CheckerWitness\n{}\n{}\n",
            audit_line(0, "ok"),
            audit_line(1, "axioms sorryAx"),
        );
        let outcomes = parse_law_audits(&output, &laws).expect("well-formed audit parses");
        assert_eq!(outcomes.len(), 2);
        assert_eq!(outcomes[0].label, "A.one");
        assert!(outcomes[0].offending.is_empty());
        assert_eq!(outcomes[1].label, "A.two");
        assert_eq!(outcomes[1].offending, vec!["sorryAx".to_string()]);
    }

    #[test]
    fn law_audit_names_every_offending_axiom() {
        let laws = law_candidates(&["A.one"]);
        let output = audit_line(0, "axioms sorryAx,Lean.ofReduceBool,Domain.myAxiom");
        let outcomes = parse_law_audits(&output, &laws).expect("multi-axiom audit parses");
        assert_eq!(
            outcomes[0].offending,
            vec![
                "sorryAx".to_string(),
                "Lean.ofReduceBool".to_string(),
                "Domain.myAxiom".to_string()
            ]
        );
    }

    #[test]
    fn law_audit_without_a_line_declines_instead_of_crediting() {
        let laws = law_candidates(&["A.one", "A.two"]);
        // Pin 1 never reported: a parse miss must cost the package, never
        // silently credit the claim.
        let error = parse_law_audits(&audit_line(0, "ok"), &laws).unwrap_err();
        assert!(
            error.contains("no axiom audit for law-claim `A.two`")
                && error.contains("refusing to credit an unaudited claim"),
            "the decline names the unaudited claim: {error}"
        );
        // A renamed marker reports nothing at all — same decline.
        let renamed = audit_line(0, "ok").replace(LAW_AUDIT_MARKER, "AVER_LAW_TRACE");
        assert!(parse_law_audits(&renamed, &law_candidates(&["A.one"])).is_err());
    }

    #[test]
    fn law_audit_rejects_malformed_and_repeated_lines() {
        let laws = law_candidates(&["A.one"]);
        for bad in [
            audit_line(0, ""),
            audit_line(0, "axioms"),
            audit_line(0, "ok extra"),
            audit_line(0, "clean"),
            audit_line(0, "axioms sorryAx,"),
            audit_line(9, "ok"),
        ] {
            assert!(
                parse_law_audits(&bad, &laws).is_err(),
                "malformed audit line must decline: {bad}"
            );
        }
        let repeated = format!("{}\n{}\n", audit_line(0, "ok"), audit_line(0, "ok"));
        let error = parse_law_audits(&repeated, &laws).unwrap_err();
        assert!(
            error.contains("more than once"),
            "a repeated pin declines: {error}"
        );
    }

    #[test]
    fn law_audit_keyword_separates_credit_from_an_axiom_named_ok() {
        let laws = law_candidates(&["A.one"]);
        let outcomes = parse_law_audits(&audit_line(0, "axioms ok"), &laws)
            .expect("an axiom named `ok` still parses");
        assert_eq!(outcomes[0].offending, vec!["ok".to_string()]);
    }

    /// One declared bridge entry, as the manifest transports it: structure
    /// only. There is no statement field to set — that is the point.
    fn raw_bridge(export: &str) -> RawSourceBridge {
        RawSourceBridge {
            export: export.to_string(),
            theorem: format!("{BRIDGE_NAMESPACE}.{export}"),
            corollary: format!("{BRIDGE_NAMESPACE}.{export}{BRIDGE_COROLLARY_SUFFIX}"),
            model: format!("Domain.{export}"),
            params: vec![SourceEncoder::Int],
            result: SourceEncoder::Int,
        }
    }

    fn fraction_encoder() -> SourceEncoder {
        SourceEncoder::Record {
            lean_type: "_root_.Domain.Fraction".to_string(),
            accessors: vec![
                "_root_.Domain.Fraction.top".to_string(),
                "_root_.Domain.Fraction.bottom".to_string(),
            ],
        }
    }

    fn bridge_candidates(exports: &[&str]) -> Vec<SourceBridgeCandidate> {
        exports
            .iter()
            .map(|export| {
                validate_source_bridge_candidate(raw_bridge(export))
                    .expect("an honest bridge entry validates")
            })
            .collect()
    }

    fn bridge_audit_line(index: usize, verdict: &str) -> String {
        format!(
            "CheckerWitness.lean:9:0: info: {BRIDGE_AUDIT_MARKER} \
             {BRIDGE_PIN_PREFIX}{index} {verdict}"
        )
    }

    #[test]
    fn bridge_audit_credits_only_pins_the_witness_reported_clean() {
        let bridges = bridge_candidates(&["one", "two"]);
        let output = format!(
            "building CheckerWitness\n{}\n{}\n",
            bridge_audit_line(0, "ok"),
            bridge_audit_line(1, "axioms sorryAx"),
        );
        let outcomes = parse_bridge_audits(&output, &bridges).expect("well-formed audit parses");
        assert_eq!(outcomes.len(), 2);
        assert!(outcomes[0].offending.is_empty());
        assert_eq!(outcomes[1].export, "two");
        assert_eq!(outcomes[1].offending, vec!["sorryAx".to_string()]);
    }

    #[test]
    fn bridge_audit_without_a_line_declines_instead_of_crediting() {
        let bridges = bridge_candidates(&["one", "two"]);
        let error = parse_bridge_audits(&bridge_audit_line(0, "ok"), &bridges).unwrap_err();
        assert!(
            error.contains("no axiom audit for source-bridge `two`")
                && error.contains("refusing to credit an unaudited bridge"),
            "the decline names the unaudited bridge: {error}"
        );
        // A renamed marker reports nothing at all — same decline. The law
        // marker in particular must not be read as a bridge audit.
        let renamed = bridge_audit_line(0, "ok").replace(BRIDGE_AUDIT_MARKER, LAW_AUDIT_MARKER);
        assert!(parse_bridge_audits(&renamed, &bridge_candidates(&["one"])).is_err());
    }

    #[test]
    fn bridge_audit_rejects_malformed_and_repeated_lines() {
        let bridges = bridge_candidates(&["one"]);
        for bad in [
            bridge_audit_line(0, ""),
            bridge_audit_line(0, "axioms"),
            bridge_audit_line(0, "ok extra"),
            bridge_audit_line(0, "clean"),
            bridge_audit_line(0, "axioms sorryAx,"),
            bridge_audit_line(9, "ok"),
        ] {
            assert!(
                parse_bridge_audits(&bad, &bridges).is_err(),
                "malformed audit line must decline: {bad}"
            );
        }
        let repeated = format!(
            "{}\n{}\n",
            bridge_audit_line(0, "ok"),
            bridge_audit_line(0, "ok")
        );
        assert!(
            parse_bridge_audits(&repeated, &bridges)
                .unwrap_err()
                .contains("more than once"),
            "a repeated pin declines"
        );
    }

    #[test]
    fn bridge_candidate_gate_pins_the_two_declaration_names_and_the_encoders() {
        assert!(validate_source_bridge_candidate(raw_bridge("one")).is_ok());

        let mut wrong_theorem = raw_bridge("one");
        wrong_theorem.theorem = "AverCert.Laws.one".to_string();
        assert!(validate_source_bridge_candidate(wrong_theorem).is_err());

        let mut wrong_corollary = raw_bridge("one");
        wrong_corollary.corollary = "AverCert.Bridge.one".to_string();
        assert!(validate_source_bridge_candidate(wrong_corollary).is_err());

        let mut dotted_export = raw_bridge("one");
        dotted_export.export = "Domain.one".to_string();
        assert!(validate_source_bridge_candidate(dotted_export).is_err());

        let mut bad_model = raw_bridge("one");
        bad_model.model = "Domain one".to_string();
        assert!(validate_source_bridge_candidate(bad_model).is_err());

        // An encoder whose names are not `_root_.`-qualified would be resolved
        // against whatever namespaces the package declares.
        let mut bare_names = raw_bridge("one");
        bare_names.result = SourceEncoder::Record {
            lean_type: "Domain.Fraction".to_string(),
            accessors: vec!["Domain.Fraction.top".to_string()],
        };
        assert!(validate_source_bridge_candidate(bare_names).is_err());

        // An accessor of some other type is not a field of the declared one.
        let mut foreign_accessor = raw_bridge("one");
        foreign_accessor.result = SourceEncoder::Record {
            lean_type: "_root_.Domain.Fraction".to_string(),
            accessors: vec!["_root_.Other.Record.top".to_string()],
        };
        assert!(validate_source_bridge_candidate(foreign_accessor).is_err());
    }

    /// The defect this surface was reshaped to close: a package used to declare
    /// its bridge statement as text, so `_root_.M.f x = _root_.M.f x` passed
    /// every gate, elaborated, audited clean, and printed `model: plan ≡ M.f`.
    /// The manifest now carries structure and the CHECKER writes the text, so
    /// the left-hand side is always the export's own plan and no entry can
    /// state an identity between a function and itself.
    #[test]
    fn a_bridge_statement_is_rendered_by_the_checker_not_declared() {
        let mut entry = raw_bridge("Domain_Rational_plus");
        entry.model = "Domain.Rational.plus".to_string();
        entry.params = vec![fraction_encoder(), fraction_encoder()];
        entry.result = fraction_encoder();
        let candidate = validate_source_bridge_candidate(entry).expect("an honest entry validates");
        assert_eq!(
            candidate.statement,
            render_bridge_statement(
                "Domain_Rational_plus",
                "Domain.Rational.plus",
                &[fraction_encoder(), fraction_encoder()],
                &fraction_encoder(),
            ),
            "the pinned statement is the renderer's output and nothing else"
        );
        assert!(
            candidate.statement.starts_with('∀')
                && candidate.statement.contains(
                    "_root_.AverCert.StandardFace.recordComputeModel \
                     _root_.AverCert.Plans.Domain_Rational_plusPlan.body"
                ),
            "the claim's left-hand side is the export's own plan: {}",
            candidate.statement
        );
        // Naming a different export renders a different claim, so the pin no
        // longer has the package corollary's type — a decline, not a credit.
        let mut renamed = raw_bridge("Domain_Rational_minus");
        renamed.model = "Domain.Rational.plus".to_string();
        renamed.params = vec![fraction_encoder(), fraction_encoder()];
        renamed.result = fraction_encoder();
        let renamed = validate_source_bridge_candidate(renamed).expect("it still validates");
        assert_ne!(renamed.statement, candidate.statement);
        // So does permuting a record's accessors.
        let permuted = SourceEncoder::Record {
            lean_type: "_root_.Domain.Fraction".to_string(),
            accessors: vec![
                "_root_.Domain.Fraction.bottom".to_string(),
                "_root_.Domain.Fraction.top".to_string(),
            ],
        };
        let mut swapped = raw_bridge("Domain_Rational_plus");
        swapped.model = "Domain.Rational.plus".to_string();
        swapped.params = vec![permuted, fraction_encoder()];
        swapped.result = fraction_encoder();
        let swapped = validate_source_bridge_candidate(swapped).expect("it still validates");
        assert_ne!(swapped.statement, candidate.statement);
    }

    /// The encoder set is closed, and each kind's object is matched exactly.
    /// An unknown kind is a decline, never a default shape.
    #[test]
    fn bridge_encoder_kinds_outside_the_closed_set_decline() {
        assert_eq!(
            read_source_encoder(&serde_json::json!({"kind": "int"}), "e").unwrap(),
            SourceEncoder::Int
        );
        assert_eq!(
            read_source_encoder(&serde_json::json!({"kind": "bool"}), "e").unwrap(),
            SourceEncoder::Bool
        );
        assert_eq!(
            read_source_encoder(
                &serde_json::json!({
                    "kind": "record",
                    "type": "_root_.Domain.Fraction",
                    "fields": ["_root_.Domain.Fraction.top"],
                }),
                "e"
            )
            .unwrap(),
            SourceEncoder::Record {
                lean_type: "_root_.Domain.Fraction".to_string(),
                accessors: vec!["_root_.Domain.Fraction.top".to_string()],
            }
        );
        for bad in [
            serde_json::json!({"kind": "float"}),
            serde_json::json!({"kind": "string"}),
            serde_json::json!({}),
            serde_json::json!({"kind": "int", "type": "_root_.Domain.Fraction"}),
            serde_json::json!({"kind": "record", "type": "_root_.Domain.Fraction"}),
            serde_json::json!({"kind": "record", "type": "_root_.Domain.Fraction", "fields": "top"}),
            serde_json::json!({"kind": "record", "type": "_root_.Domain.Fraction", "fields": [7]}),
        ] {
            assert!(
                read_source_encoder(&bad, "e").is_err(),
                "an encoder outside the closed set must decline: {bad}"
            );
        }
    }

    /// The manifest entry is matched exactly, so a package cannot smuggle a
    /// statement of its own back in beside the structure.
    #[test]
    fn a_bridge_entry_carrying_a_statement_field_declines() {
        let fields = [
            "export",
            "theorem",
            "corollary",
            "model",
            "params",
            "result",
        ];
        let honest = serde_json::json!({
            "export": "one",
            "theorem": "AverCert.Bridge.one",
            "corollary": "AverCert.Bridge.one_certified",
            "model": "Domain.one",
            "params": [],
            "result": {"kind": "int"},
        });
        assert!(exact_object_fields(&honest, "sourceBridges[0]", &fields).is_ok());
        let mut smuggled = honest.clone();
        smuggled["statement"] = serde_json::json!("_root_.Domain.one = _root_.Domain.one");
        assert!(exact_object_fields(&smuggled, "sourceBridges[0]", &fields).is_err());
    }

    #[test]
    fn bridge_clause_counts_credit_and_names_the_uncredited() {
        let summary = summarize_report(
            Path::new("app.wasm"),
            TrustedReport {
                exports: Vec::new(),
                laws: Vec::new(),
                bridged_laws: Vec::new(),
                source_bridges: vec![
                    BridgeOutcome {
                        export: "one".to_string(),
                        model: "Domain.one".to_string(),
                        statement: "_root_.One".to_string(),
                        offending: Vec::new(),
                    },
                    BridgeOutcome {
                        export: "two".to_string(),
                        model: "Domain.two".to_string(),
                        statement: "_root_.Two".to_string(),
                        offending: vec!["sorryAx".to_string()],
                    },
                ],
                contracts: Vec::new(),
                target: String::new(),
                profile: String::new(),
                abi: String::new(),
                artifact_hash: String::new(),
            },
            "checked",
        );
        assert_eq!(
            summary.text,
            "app.wasm (0 checked exports, level L1; source-bridges: 1 of 2 credited)"
        );
        assert_eq!(
            summary.uncredited_bridges,
            vec!["source-bridge not credited: two (proof depends on sorryAx)".to_string()]
        );
    }

    #[test]
    fn law_clause_is_absent_without_law_claims() {
        let bare = summarize_report(
            Path::new("app.wasm"),
            TrustedReport {
                exports: Vec::new(),
                laws: Vec::new(),
                bridged_laws: Vec::new(),
                source_bridges: Vec::new(),
                contracts: Vec::new(),
                target: String::new(),
                profile: String::new(),
                abi: String::new(),
                artifact_hash: String::new(),
            },
            "checked",
        );
        assert_eq!(bare.text, "app.wasm (0 checked exports, level L1)");
        assert!(bare.uncredited_laws.is_empty());
    }

    #[test]
    fn law_clause_counts_credit_and_names_the_uncredited() {
        let summary = summarize_report(
            Path::new("app.wasm"),
            TrustedReport {
                exports: Vec::new(),
                laws: vec![
                    LawOutcome {
                        label: "A.one".to_string(),
                        offending: Vec::new(),
                    },
                    LawOutcome {
                        label: "A.two".to_string(),
                        offending: vec!["sorryAx".to_string()],
                    },
                ],
                bridged_laws: Vec::new(),
                source_bridges: Vec::new(),
                contracts: Vec::new(),
                target: String::new(),
                profile: String::new(),
                abi: String::new(),
                artifact_hash: String::new(),
            },
            "checked",
        );
        assert_eq!(
            summary.text,
            "app.wasm (0 checked exports, level L1; law-claims: 1 of 2 credited)"
        );
        assert_eq!(
            summary.uncredited_laws,
            vec!["law-claim not credited: A.two (proof depends on sorryAx)".to_string()]
        );
    }

    /// The decoupling this surface exists for, at the reporting layer: the two
    /// counters move independently, and an uncredited BRIDGED claim never
    /// removes credit from the plain law of the same label.
    #[test]
    fn bridged_law_clause_is_counted_apart_from_the_law_clause() {
        let summary = summarize_report(
            Path::new("app.wasm"),
            TrustedReport {
                exports: Vec::new(),
                laws: vec![
                    LawOutcome {
                        label: "A.one".to_string(),
                        offending: Vec::new(),
                    },
                    LawOutcome {
                        label: "A.two".to_string(),
                        offending: Vec::new(),
                    },
                ],
                bridged_laws: vec![
                    LawOutcome {
                        label: "A.one".to_string(),
                        offending: Vec::new(),
                    },
                    LawOutcome {
                        label: "A.two".to_string(),
                        offending: vec!["sorryAx".to_string()],
                    },
                ],
                source_bridges: Vec::new(),
                contracts: Vec::new(),
                target: String::new(),
                profile: String::new(),
                abi: String::new(),
                artifact_hash: String::new(),
            },
            "checked",
        );
        assert_eq!(
            summary.text,
            "app.wasm (0 checked exports, level L1; law-claims: 2 of 2 credited; \
             bridged-laws: 1 of 2 credited)"
        );
        assert!(summary.uncredited_laws.is_empty());
        assert_eq!(
            summary.uncredited_bridged_laws,
            vec!["bridged law-claim not credited: A.two (proof depends on sorryAx)".to_string()]
        );
    }

    /// The bridged pins are numbered over the BRIDGED claims, not over all of
    /// them, so the audit readback and the witness agree on which pin is whose.
    #[test]
    fn bridged_law_pins_are_numbered_over_the_bridged_claims_only() {
        let mut laws = law_candidates(&["A.one", "A.two", "A.three"]);
        laws[1].bridges = vec![0];
        laws[2].bridges = vec![0];
        assert_eq!(bridged_law_indices(&laws), vec![1, 2]);
        let line = |index: usize, verdict: &str| {
            format!(
                "CheckerWitness.lean:9:0: info: {LAW_BRIDGE_AUDIT_MARKER} \
                 {BRIDGED_LAW_PIN_PREFIX}{index} {verdict}"
            )
        };
        let outcomes = parse_bridged_law_audits(
            &format!("{}\n{}\n", line(0, "ok"), line(1, "axioms sorryAx")),
            &laws,
        )
        .expect("well-formed audit parses");
        assert_eq!(outcomes.len(), 2);
        assert_eq!(outcomes[0].label, "A.two");
        assert!(outcomes[0].offending.is_empty());
        assert_eq!(outcomes[1].label, "A.three");
        assert_eq!(outcomes[1].offending, vec!["sorryAx".to_string()]);
        // A missing line declines rather than crediting an unaudited claim,
        // and the plain law marker must not be read as a bridged-law audit.
        assert!(parse_bridged_law_audits(&line(0, "ok"), &laws).is_err());
        let renamed = line(0, "ok").replace(LAW_BRIDGE_AUDIT_MARKER, LAW_AUDIT_MARKER);
        assert!(parse_bridged_law_audits(&format!("{renamed}\n{}", line(1, "ok")), &laws).is_err());
    }

    #[test]
    fn report_face_prints_only_kernel_pinned_facts() {
        let candidate = CertifiedCandidate {
            name: "addOne".to_string(),
            class: "expr-fragment-v1".to_string(),
            policy: "simulatesModel".to_string(),
            policy_lean: ".simulatesModel",
            termination_lean: "none".to_string(),
            dom: "List Int".to_string(),
            cod: "Int".to_string(),
            theorem: Some("AcceptanceSoundness.exprFragment_claim_discharges".to_string()),
        };
        assert_eq!(report_face(&candidate), "class: expression fragment");
        assert_eq!(
            manifest_face(&candidate),
            "manifest face (declared, not kernel-pinned): Dom List Int, Cod Int"
        );
        // The generic face is unconditional over represented carriers, so it
        // carries no domain restriction line.
        assert_eq!(record_compute_domain(&candidate), None);
    }

    /// The record projection-compute face is the one whose certified domain is
    /// narrower — its inputs AND its record fields are assumed canonical — so
    /// `explain` must say so on that export's line and only on that one.
    #[test]
    fn only_the_record_compute_face_discloses_a_narrower_domain() {
        let mut candidate = CertifiedCandidate {
            name: "Domain_Rational_plus".to_string(),
            class: "expr-fragment-v1".to_string(),
            policy: "simulatesModel".to_string(),
            policy_lean: ".simulatesModel",
            termination_lean: "none".to_string(),
            dom: "Rational x Rational".to_string(),
            cod: "Rational".to_string(),
            theorem: Some(format::RECORD_COMPUTE_DISCHARGE_THEOREM.to_string()),
        };
        assert_eq!(
            record_compute_domain(&candidate),
            Some(format::RECORD_COMPUTE_DOMAIN_LINE)
        );
        candidate.theorem = None;
        assert_eq!(record_compute_domain(&candidate), None);
    }

    #[test]
    fn module_names_are_plain() {
        assert_eq!(lean_module_root("Artifact.lean").unwrap(), "Artifact");
        assert!(lean_module_root("../Artifact.lean").is_err());
        assert!(lean_module_root("A, `Injected.lean").is_err());
        // Nested paths become dotted module names, one identifier per segment.
        assert_eq!(
            lean_module_root("Apps/Notepad/Store.lean").unwrap(),
            "Apps.Notepad.Store"
        );
        assert!(lean_module_root("Apps/../X.lean").is_err());
        assert!(lean_module_root("Apps/.lake/X.lean").is_err());
        assert!(lean_module_root("Apps/Bad Name.lean").is_err());
        assert!(lean_module_root("Apps/`Tick/X.lean").is_err());
        assert!(lean_module_root("Apps/A,B/X.lean").is_err());
        assert!(lean_module_root("Apps.Notepad/Store.lean").is_err());
        assert!(lean_module_root("/Apps/Store.lean").is_err());
        assert!(lean_module_root("Apps//Store.lean").is_err());
    }

    #[test]
    fn staged_paths_that_collide_case_insensitively_are_rejected() {
        let mut staged = std::collections::BTreeMap::new();
        assert!(note_staged_path(&mut staged, "Apps/Store.lean").is_ok());
        assert!(note_staged_path(&mut staged, "Apps/Other.lean").is_ok());
        assert!(note_staged_path(&mut staged, "Manifest.lean").is_ok());
        let error = note_staged_path(&mut staged, "apps/Store.lean").unwrap_err();
        assert!(
            error.contains("Apps/Store.lean")
                && error.contains("apps/Store.lean")
                && error.contains("collide case-insensitively"),
            "collision error names both paths: {error}"
        );
        // Flat names participate in the same set.
        assert!(note_staged_path(&mut staged, "MANIFEST.lean").is_err());
    }

    #[test]
    fn nested_walk_rejects_unbounded_depth() {
        let root = std::env::temp_dir().join(format!(
            "aver-cert-depth-cap-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        let mut deep = root.clone();
        for _ in 0..(MAX_NESTED_DEPTH + 2) {
            deep = deep.join("D");
        }
        std::fs::create_dir_all(&deep).unwrap();
        std::fs::write(deep.join("X.lean"), "def x : Nat := 0\n").unwrap();

        let mut out = Vec::new();
        let error = collect_nested_lean_files(&root.join("D"), "D", 1, &mut out).unwrap_err();
        assert!(
            error.contains("maximum nesting depth"),
            "depth cap error names the gate: {error}"
        );
        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn import_line_scan_is_literal() {
        let mut admitted = std::collections::BTreeSet::new();
        collect_import_lines(
            "import Schema\n  import Apps.Notepad.Store  \n/-\nimport Inside.Block.Comment\n-/\n-- import Behind.Line.Comment\nnot an import\nimport\n",
            &mut admitted,
        );
        assert!(admitted.contains("Schema"));
        assert!(admitted.contains("Apps.Notepad.Store"));
        // The scan is literal by contract: comment syntax is not parsed, so
        // a line that begins with `import ` inside a block comment still
        // admits. A line-comment prefix keeps the line from matching.
        assert!(admitted.contains("Inside.Block.Comment"));
        assert!(!admitted.contains("Behind.Line.Comment"));
        assert_eq!(admitted.len(), 3);
    }

    #[test]
    fn nested_roots_shadowing_reserved_prefixes_are_rejected() {
        let wall = wall::resolve(wall::current_id()).expect("embedded wall resolves");
        // Flat behavior is unchanged.
        assert!(reject_shadowed_root("Schema", wall).is_err());
        assert!(reject_shadowed_root("Manifest", wall).is_ok());
        // Every dotted prefix of a nested root is checked.
        assert!(reject_shadowed_root("Lean.Extra", wall).is_err());
        assert!(reject_shadowed_root("Schema.Sub", wall).is_err());
        assert!(reject_shadowed_root("ArtifactBytes.Decoy", wall).is_err());
        assert!(reject_shadowed_root("ArtifactComponentBytes.Decoy", wall).is_err());
        assert!(reject_shadowed_root("CheckerWitness.X.Y", wall).is_err());
        // A reserved name in non-prefix position does not shadow the import.
        assert!(reject_shadowed_root("Apps.Schema", wall).is_ok());
        assert!(reject_shadowed_root("Apps.Notepad.Store", wall).is_ok());
    }

    #[test]
    fn wasm_gc_identity_selects_wasm_gc_artifact_preparation() {
        assert_eq!(
            require_supported_identity(&ManifestIdentity {
                target: format::TARGET_WASM_GC.to_string(),
                profile: format::PROFILE_ID.to_string(),
                abi: format::RUNTIME_ABI_WASM_GC.to_string(),
            })
            .unwrap(),
            ArtifactTarget::WasmGc
        );
    }

    #[test]
    fn wasip2_identity_selects_wasip2_artifact_preparation() {
        assert_eq!(
            require_supported_identity(&ManifestIdentity {
                target: format::TARGET_WASIP2.to_string(),
                profile: format::PROFILE_ID.to_string(),
                abi: format::RUNTIME_ABI_WASIP2.to_string(),
            })
            .unwrap(),
            ArtifactTarget::Wasip2
        );
    }

    #[test]
    fn target_identity_rejects_wrong_target_abi_pairs() {
        let error = require_supported_identity(&ManifestIdentity {
            target: format::TARGET_WASIP2.to_string(),
            profile: format::PROFILE_ID.to_string(),
            abi: format::RUNTIME_ABI_WASM_GC.to_string(),
        })
        .unwrap_err();
        assert!(error.contains("unsupported certificate ABI"), "{error}");
        assert!(error.contains(format::TARGET_WASIP2), "{error}");
    }

    #[test]
    fn target_artifact_preparation_keeps_schema_six_on_core_wasm_modules() {
        let bytes = b"\0asm\x01\0\0\0";
        let prepared = prepare_artifact_for_target(ArtifactTarget::WasmGc, bytes, None)
            .expect("empty wasm module is a valid schema-6 wasm-gc artifact envelope");
        assert_eq!(prepared.artifact_hash, sha256_hex(bytes));
        assert_eq!(prepared.target_artifact_bytes, bytes);
        assert_eq!(prepared.core_module_bytes, bytes);
    }

    #[test]
    fn wasm_gc_artifact_preparation_still_rejects_non_wasm_bytes() {
        let error = prepare_artifact_for_target(ArtifactTarget::WasmGc, b"not a wasm module", None)
            .unwrap_err();
        assert!(
            error.contains("artifact is not valid WebAssembly"),
            "{error}"
        );
    }

    #[test]
    fn wasip2_missing_envelope_rejects_before_wasm_validation() {
        let root = std::env::temp_dir().join(format!(
            "aver-cert-wasip2-missing-envelope-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        std::fs::create_dir_all(&root).unwrap();
        let artifact = root.join("artifact.component.wasm");
        std::fs::write(&artifact, b"not even wasm").unwrap();
        std::fs::write(
            root.join("cert-manifest.json"),
            format!(
                "{{\n  \"schema_version\": {},\n  \"target\": \"{}\",\n  \"profile\": \"{}\",\n  \"abi\": \"{}\"\n}}\n",
                format::CERT_SCHEMA_VERSION,
                format::TARGET_WASIP2,
                format::PROFILE_ID,
                format::RUNTIME_ABI_WASIP2
            ),
        )
        .unwrap();

        let error = match trusted_check(&artifact, &root, ReplayMode::TrustBuiltOleans) {
            Ok(_) => panic!("wasip2 target must require its manifest envelope"),
            Err(error) => error,
        };
        assert!(
            error.contains("cert-manifest.json is missing object field `wasip2ComponentEnvelope`"),
            "{error}"
        );
        assert!(!error.contains("WebAssembly"), "{error}");
        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn wasip2_envelope_manifest_parser_reads_declared_lengths() {
        let manifest = serde_json::json!({
            "wasip2ComponentEnvelope": {
                "kind": format::WASIP2_COMPONENT_ENVELOPE_KIND,
                "prefix_len": 10,
                "embedded_core_module_len": 8,
                "suffix_len": 3
            }
        });

        let declaration = read_wasip2_component_envelope(&manifest).unwrap();
        assert_eq!(
            declaration.inner,
            format::Wasip2ComponentEnvelopeDeclaration::from_lengths(10, 8, 3)
        );
    }

    #[test]
    fn wasip2_envelope_manifest_parser_rejects_bad_shape() {
        let missing = read_wasip2_component_envelope(&serde_json::json!({})).unwrap_err();
        assert!(missing.contains("wasip2ComponentEnvelope"), "{missing}");

        let bad_kind = read_wasip2_component_envelope(&serde_json::json!({
            "wasip2ComponentEnvelope": {
                "kind": "prefix-core-suffix/v2",
                "prefix_len": 10,
                "embedded_core_module_len": 8,
                "suffix_len": 0
            }
        }))
        .unwrap_err();
        assert!(
            bad_kind.contains("unsupported wasip2 component envelope kind"),
            "{bad_kind}"
        );

        let non_u64 = read_wasip2_component_envelope(&serde_json::json!({
            "wasip2ComponentEnvelope": {
                "kind": format::WASIP2_COMPONENT_ENVELOPE_KIND,
                "prefix_len": "10",
                "embedded_core_module_len": 8,
                "suffix_len": 0
            }
        }))
        .unwrap_err();
        assert!(
            non_u64.contains("prefix_len") && non_u64.contains("u64"),
            "{non_u64}"
        );
    }

    #[test]
    fn wasip2_declared_envelope_preparation_splits_component_without_discovery() {
        let core = b"\0asm\x01\0\0\0";
        let component = component_with_embedded_core(core);
        let declaration = Wasip2EnvelopeDeclaration {
            inner: format::Wasip2ComponentEnvelopeDeclaration::from_lengths(
                u64::try_from(component.len() - core.len()).unwrap(),
                u64::try_from(core.len()).unwrap(),
                0,
            ),
        };

        let prepared = prepare_wasip2_artifact_with_declared_envelope(&component, declaration)
            .expect("declared component envelope is valid");
        assert_eq!(prepared.artifact_hash, sha256_hex(&component));
        assert_eq!(prepared.core_module_bytes, core);
    }

    #[test]
    fn wasip2_declared_envelope_preparation_rejects_bad_lengths() {
        let core = b"\0asm\x01\0\0\0";
        let component = component_with_embedded_core(core);

        let empty_core = prepare_wasip2_artifact_with_declared_envelope(
            &component,
            Wasip2EnvelopeDeclaration {
                inner: format::Wasip2ComponentEnvelopeDeclaration::from_lengths(
                    u64::try_from(component.len()).unwrap(),
                    0,
                    0,
                ),
            },
        )
        .unwrap_err();
        assert!(empty_core.contains("empty embedded core"), "{empty_core}");

        let mismatch = prepare_wasip2_artifact_with_declared_envelope(
            &component,
            Wasip2EnvelopeDeclaration {
                inner: format::Wasip2ComponentEnvelopeDeclaration::from_lengths(0, 8, 0),
            },
        )
        .unwrap_err();
        assert!(mismatch.contains("length mismatch"), "{mismatch}");

        let overflow = prepare_wasip2_artifact_with_declared_envelope(
            &component,
            Wasip2EnvelopeDeclaration {
                inner: format::Wasip2ComponentEnvelopeDeclaration::from_lengths(u64::MAX, 1, 0),
            },
        )
        .unwrap_err();
        assert!(overflow.contains("length overflow"), "{overflow}");
    }

    #[test]
    fn wasip2_declared_envelope_preparation_rejects_non_module_core() {
        let empty_component = b"\0asm\x0d\0\x01\0";
        let error = prepare_wasip2_artifact_with_declared_envelope(
            empty_component,
            Wasip2EnvelopeDeclaration {
                inner: format::Wasip2ComponentEnvelopeDeclaration::from_lengths(0, 8, 0),
            },
        )
        .unwrap_err();
        assert!(error.contains("not a core WebAssembly module"), "{error}");
    }

    fn component_with_embedded_core(core: &[u8]) -> Vec<u8> {
        let mut component = b"\0asm\x0d\0\x01\0".to_vec();
        component.push(1);
        push_u32_leb(core.len().try_into().unwrap(), &mut component);
        component.extend_from_slice(core);
        component
    }

    fn push_u32_leb(mut value: u32, bytes: &mut Vec<u8>) {
        loop {
            let mut byte = (value & 0x7f) as u8;
            value >>= 7;
            if value != 0 {
                byte |= 0x80;
            }
            bytes.push(byte);
            if value == 0 {
                break;
            }
        }
    }

    #[test]
    fn candidate_strings_cannot_escape_lean_literals() {
        assert!(gate_candidate("test", "plain ASCII").is_ok());
        assert!(gate_candidate("test", "quote: \"").is_err());
        assert!(gate_candidate("test", "line\nbreak").is_err());
    }

    #[test]
    fn artifact_bytes_are_little_endian_nat() {
        let rendered = wall::render_artifact_bytes(&[0x00, 0x61, 0x73, 0x6d]);
        assert!(rendered.contains("def modBytes : Nat := 0x6d736100"));
        assert!(rendered.contains("def modLen : Nat := 4"));
    }

    #[test]
    fn strict_and_trusted_olean_modes_differ_only_at_fresh_replay_dispatch() {
        let stock: Vec<String> = FRESH_REPLAY_ARGS.iter().map(|a| (*a).to_string()).collect();
        assert_eq!(replay_args_for(ReplayMode::Fresh, None), Some(stock));
        assert_eq!(replay_args_for(ReplayMode::TrustBuiltOleans, None), None);
    }

    #[test]
    fn the_replayer_override_is_off_unless_it_names_a_binary() {
        let stock: Vec<String> = FRESH_REPLAY_ARGS.iter().map(|a| (*a).to_string()).collect();
        // Absent, empty and whitespace-only all keep the published path.
        for absent in [None, Some(""), Some("   ")] {
            assert_eq!(
                replay_args_for(ReplayMode::Fresh, absent),
                Some(stock.clone()),
                "override {absent:?} must not divert the replay"
            );
        }

        let diverted = replay_args_for(ReplayMode::Fresh, Some("/opt/parreplay"))
            .expect("fresh mode always replays");
        assert_eq!(diverted[0], "env");
        assert_eq!(diverted[1], "/opt/parreplay");
        assert!(
            diverted.contains(&"replay".to_string()),
            "must request the single-pass mode, not the comparison mode: {diverted:?}"
        );
        assert!(
            !diverted.contains(&"leanchecker".to_string()),
            "stock replayer must not also run: {diverted:?}"
        );

        // The override never applies to the mode that replays nothing.
        assert_eq!(
            replay_args_for(ReplayMode::TrustBuiltOleans, Some("/opt/parreplay")),
            None
        );
    }

    /// Convenience: run the code-exec scanner over a Lean source snippet.
    fn scan(src: &str) -> Result<(), String> {
        scan_for_code_exec("Test.lean", src.as_bytes())
    }

    #[test]
    fn code_exec_scanner_is_lexically_context_aware() {
        // (i) a forbidden word token inside a string literal is inert.
        assert!(scan(r#"def x := "elab""#).is_ok());
        // (ii) the same token in code position is rejected.
        let err = scan("elab foo").unwrap_err();
        assert!(err.contains("elab"), "{err}");
        // (iii) a guillemet inside a string literal is an inert byte.
        assert!(scan(r#"def x := "« inside a string »""#).is_ok());
        // (iv) a guillemet identifier in code position is rejected.
        let err = scan("def «weird» := 0").unwrap_err();
        assert!(err.contains('«'), "{err}");
        // (v) `elab` as a substring of a larger identifier is not the keyword.
        assert!(scan("def relabel := 0").is_ok());
        assert!(scan("def macroexpanded := 0").is_ok());
        // (vi) an elaboration command in code position is rejected.
        let err = scan("#eval IO.println \"x\"").unwrap_err();
        assert!(err.contains("#eval"), "{err}");
        // (vii) an elaboration command inside a line comment is inert.
        assert!(scan("-- #eval IO.println \"x\"\ndef y := 0").is_ok());
        // (viii) a forbidden token inside a nested block comment is inert.
        assert!(scan("/- outer /- inner #eval -/ still -/\ndef y := 0").is_ok());
        // (ix) an unterminated string defaults to code: the tail is scanned.
        let err = scan("def x := \"unterminated #eval").unwrap_err();
        assert!(err.contains("#eval"), "{err}");
        // (x) a forbidden word token inside a line comment is inert.
        assert!(scan("-- elab is only mentioned here\ndef y := 0").is_ok());
    }

    #[test]
    fn code_exec_scanner_defaults_to_code_on_ambiguity() {
        // A `"` inside a char literal must not open a phantom string that would
        // swallow the following code (desync -> under-reject).
        let err = scan("def c : Char := '\"'\n#eval evil").unwrap_err();
        assert!(err.contains("#eval"), "{err}");
        let err = scan("def c : Char := '\\\"'\n#eval evil").unwrap_err();
        assert!(err.contains("#eval"), "{err}");
        // A raw-string prefix is ambiguous for a normal-string scan; the
        // remainder is scanned as code rather than skipped.
        let err = scan("def x := r\"#eval evil\"").unwrap_err();
        assert!(err.contains("#eval"), "{err}");
        // An unterminated block comment defaults to code.
        let err = scan("/- never closed #eval").unwrap_err();
        assert!(err.contains("#eval"), "{err}");
        // Word-boundary tokens still fire when standing alone next to a string.
        let err = scan("elab\"x\"").unwrap_err();
        assert!(err.contains("elab"), "{err}");
        // `elab_rules` is caught as its own token, not masked by the `elab`
        // prefix failing its right boundary.
        let err = scan("elab_rules foo").unwrap_err();
        assert!(err.contains("elab_rules"), "{err}");
    }
}
