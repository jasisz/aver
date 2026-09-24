//! Minimal consumer for Aver artifact certificates.
//!
//! Rust owns transport: it selects the embedded wall, stages untrusted DATA,
//! injects the exact artifact bytes, and runs Lean. It does not disassemble the
//! module or reconstruct an obligation. The checker-owned Lean predicate binds
//! every accepted claim to the bytes and derives its standard face, policy,
//! termination witness, host table, and runtime contracts.

use crate::bridge_statement::{
    self, BridgeKind, MAX_BRIDGE_STATEMENT_LEN, SourceEncoder, render_bridge_statement,
    statement_is_root_qualified,
};
use crate::cache::{
    ArtifactBuildCache, KeyMaterial as ArtifactCacheKeyMaterial, ModuleOutputCache,
};
use crate::lean_process::LeanRunner;
use crate::prelude_cache::PristineWallCache;
use crate::{format, lean_gate, wall};
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
/// The checker's definitions of each pinned statement, elaborated alone
/// before a pin conjoins them.
const LAW_STATEMENT_PREFIX: &str = "AverCertChecker.law_statement_";
const BRIDGE_STATEMENT_PREFIX: &str = "AverCertChecker.bridge_statement_";
/// Marker of the per-bridge axiom-audit line, read back exactly like the
/// law one.
const BRIDGE_AUDIT_MARKER: &str = "AVER_BRIDGE_AUDIT";
/// Lean namespace every package bridge theorem and corollary must live in, and
/// the suffix the corollary carries. Both are checked exactly, so a bridge
/// entry can only ever name the two declarations this surface defines.
const BRIDGE_NAMESPACE: &str = "AverCert.Bridge";
const BRIDGE_COROLLARY_SUFFIX: &str = "_certified";
const TOOLCHAIN_ROOTS: [&str; 4] = ["Init", "Lake", "Lean", "Std"];
/// The final replay: every constant of the checker's witness module AND of
/// everything it imports — the wall, the artifact certificate, the model, the
/// law and bridge modules — re-checked by the kernel in a fresh environment.
/// A package module elaborated with the kernel check skipped therefore cannot
/// hand any credited claim an unchecked lemma.
const FRESH_REPLAY_ARGS: [&str; 4] = ["env", "leanchecker", "--fresh", WITNESS_MODULE];
/// The checker-authored witness module (pins, the accepted root).
const WITNESS_MODULE: &str = "CheckerWitness";
/// Report pins, `AverCertChecker.report_pin_<k>`.
const REPORT_PIN_PREFIX: &str = "AverCertChecker.report_pin_";
/// The audit program's decline line, and its success line.
const AUDIT_DECLINE_MARKER: &str = "AVER_AUDIT_DECLINE";
const AUDIT_OK_MARKER: &str = "AVER_AUDIT_OK";
/// User-facing name of the `lake build` step in timeout and failure messages.
const PROOF_BUILD_PHASE: &str = "certificate proof build";

/// Emitted on a green verdict. All trust-bearing byte facts and claim metadata
/// are checked by the embedded Lean wall; Rust performs no parallel verdict
/// reconstruction.
pub const ARTIFACT_DECODE_LINE: &str =
    "artifact-check: exact bytes and manifest accepted by the checker-owned Lean predicate";

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
    /// What the certified model IS: under schema 9 always the export's plan
    /// (its optimized MIR body), until a credited source bridge identifies
    /// the plan with the transpiled source function.
    certified_model: String,
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
    /// Which of the two statement kinds the bridge claims.
    kind: BridgeKind,
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
    /// Facets derived in the wall from the plan (`ClaimAxes.reportFacets`),
    /// pinned by the witness.
    facets: Vec<String>,
    policy: String,
    policy_lean: &'static str,
    termination_lean: String,
}

#[derive(Clone, Copy)]
enum StringHostRole {
    Eq,
    Concat,
}

/// The declared role indices in fixed
/// `(box, add, mul, sub, toIndex, cmp, eq, divmod)` order.
type HostRoleTable = (
    Option<u32>,
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
    /// Which of the two statement kinds the bridge claims.
    kind: BridgeKind,
    /// The statement the checker RENDERED from the declared `(export, model,
    /// params, result)`. Nothing in the manifest contributes to it beyond
    /// those; the declared `theorem` name is checked and then discarded,
    /// because the pin cites the corollary.
    statement: String,
    /// The declared encoders; the audit checks the records and sums they
    /// read against the elaborated types.
    params: Vec<SourceEncoder>,
    result: SourceEncoder,
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
                WITNESS_MODULE.to_string(),
                CHECKED_ROOT.to_string(),
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
    let stage_started = std::time::Instant::now();
    let build = assemble_build(
        cert_dir,
        &actual_hash,
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
    report_step_timing("staging and data cache", stage_started.elapsed(), &[]);
    let wall_cache_started = std::time::Instant::now();
    let mut wall_cache = if data_cache_hit {
        PristineWallCache::disabled()
    } else {
        PristineWallCache::prepare(&build.path, selected_wall, &lean)
    };
    report_step_timing("wall cache restore", wall_cache_started.elapsed(), &[]);
    // On a whole-package miss, restore the modules whose sources (and
    // imported package modules) are unchanged; Lake revalidates each one.
    let module_cache_started = std::time::Instant::now();
    let module_cache = if data_cache_hit {
        ModuleOutputCache::disabled()
    } else {
        let wall_sources: Vec<&str> = selected_wall.sources.iter().map(|s| s.name).collect();
        ModuleOutputCache::prepare(
            &build.path,
            &[
                ("wall_id", wall_id),
                ("toolchain_version", selected_wall.toolchain.trim()),
                ("schema_version", &schema_version.to_string()),
            ],
            &wall_sources,
        )
    };
    report_step_timing(
        &format!("module cache restore ({} modules)", module_cache.restored()),
        module_cache_started.elapsed(),
        &[],
    );

    let mut data_build = run_lake(&lean, &build.path, PROOF_BUILD_PHASE, &["build"])?;
    if !data_build.status.success()
        && (data_cache_hit || wall_cache.was_seeded() || module_cache.restored() > 0)
    {
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
    module_cache.publish(&build.path);

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
    // package proves. The axiom audit and the audit of what the package
    // declared run in the checker's own program, elaborated without the
    // package; a decline there declines the package, and per-pin credit is
    // read from its audit lines: a missing or malformed line is a decline,
    // never credit.
    std::fs::write(
        build.path.join("CheckerAudit.lean"),
        checker_audit(&candidates, &build.package_roots),
    )
    .map_err(|error| format!("cannot write checker audit: {error}"))?;
    let audited = run_lake(
        &lean,
        &build.path,
        "artifact audit",
        &["env", "lean", "--run", "CheckerAudit.lean"],
    )?;
    if let Some(reason) = audited
        .combined
        .lines()
        .find_map(|line| line.trim().strip_prefix(AUDIT_DECLINE_MARKER))
    {
        return Err(format!(
            "certificate declined by the checker audit:{}",
            display_safe(reason)
        ));
    }
    if !audited.status.success()
        || !audited
            .combined
            .lines()
            .any(|line| line.trim() == AUDIT_OK_MARKER)
    {
        return Err(format!(
            "the checker audit did not complete:\n{}",
            tail(&audited.combined, 30)
        ));
    }
    let laws = parse_law_audits(&audited.combined, &candidates.laws)?;
    let bridged_laws = parse_bridged_law_audits(&audited.combined, &candidates.laws)?;
    let source_bridges = parse_bridge_audits(&audited.combined, &candidates.source_bridges)?;
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
/// on it is kernel-pinned: the class and the facets are bound to
/// `ClaimAxes.reportEntries` / `ClaimAxes.reportFacets` by the checker
/// witness (like the name, policy and termination).
fn report_face(candidate: &CertifiedCandidate) -> String {
    if candidate.facets.is_empty() {
        format!("class: {}", candidate.class)
    } else {
        format!(
            "class: {} ({})",
            candidate.class,
            candidate.facets.join(", ")
        )
    }
}

/// What the export's certified model IS. Schema 9 states every obligation
/// over the plan, so the line says `plan`; a credited source bridge (not
/// carried by schema 9 yet) would say `plan ≡ <fn>`. Credit is never granted
/// on a declaration.
fn certified_model_line(candidate: &CertifiedCandidate, bridges: &[BridgeOutcome]) -> String {
    let credited = bridges
        .iter()
        .find(|bridge| bridge.export == candidate.name && bridge.offending.is_empty());
    match credited {
        Some(bridge) => match bridge.kind {
            BridgeKind::Exact => format!(
                "model: plan ≡ {} (credited source-bridge; see SOURCE-BRIDGES)",
                display_safe(&bridge.model)
            ),
            BridgeKind::Adequate => format!(
                "model: plan ≡ {} wherever the plan returns (credited adequate \
                 source-bridge, not a totality claim; see SOURCE-BRIDGES)",
                display_safe(&bridge.model)
            ),
        },
        None => "model: plan (the export's optimized MIR body)".to_string(),
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

/// `nat_lit n`: a raw natural-number literal. A pinned statement spells its
/// numerals this way so no `OfNat` instance takes part in what it means.
fn lean_nat(value: impl std::fmt::Display) -> String {
    format!("(nat_lit {value})")
}

/// An `Int` literal built from its constructors, instance-free.
fn lean_int(value: i64) -> String {
    if value >= 0 {
        format!("(_root_.Int.ofNat {})", lean_nat(value))
    } else {
        format!(
            "(_root_.Int.negSucc {})",
            lean_nat(value.unsigned_abs() - 1)
        )
    }
}

/// The checker-owned witness module.
///
/// It is PURE DATA for the kernel: theorems pinning each declared fact at a
/// checker-written statement, no `import Lean`, no command that runs code.
/// Everything it names is `_root_`-qualified — a package cannot place a
/// declaration where an unqualified name would resolve first — and every
/// numeral is a `nat_lit`, so neither a namespace nor an instance the package
/// declares takes part in what a pin says. The axiom audit of these pins runs
/// in a separate checker-owned program ([`checker_audit`]) elaborated without
/// the package, and the final fresh-environment replay replays this module and
/// everything it imports.
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
    let report_facets = format!(
        "[{}]",
        candidates
            .certified
            .iter()
            .map(|candidate| format!(
                "(\"{}\", {})",
                candidate.name,
                lean_str_list(&candidate.facets)
            ))
            .collect::<Vec<_>>()
            .join(", ")
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
        Some((
            box_role,
            add_role,
            mul_role,
            sub_role,
            to_index_role,
            cmp_role,
            eq_role,
            divmod_role,
        )) => {
            format!(
                "_root_.Option.some ({{ box := {}, add := {}, mul := {}, sub := {}, toIndex := {}, \
             cmp := {}, eq := {}, divmod := {} }} : \
             _root_.CertDecode.AddSub.Roles)",
                lean_option_nat(box_role),
                lean_option_nat(add_role),
                lean_option_nat(mul_role),
                lean_option_nat(sub_role),
                lean_option_nat(to_index_role),
                lean_option_nat(cmp_role),
                lean_option_nat(eq_role),
                lean_option_nat(divmod_role),
            )
        }
        None => "(_root_.Option.none : _root_.Option _root_.CertDecode.AddSub.Roles)".to_string(),
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
                format!("({}, {role})", lean_nat(index))
            })
            .collect::<Vec<_>>()
            .join(", ")
    );
    let wasip2_component_envelope =
        lean_wasip2_component_envelope(candidates.wasip2_component_envelope);
    // Law-claim surface: one type-pinning theorem per claim (built by
    // concatenation, never `format!`, so statement braces stay inert), the
    // conditional `Laws` import, and the corollary roots the audit walks. All
    // fields were validated by `validate_law_candidate`.
    //
    // The statement is re-elaborated inside the model theorem's OWN namespace
    // — the same context the package's `Laws.lean` uses — because `open
    // <prefix> in` at root does not reproduce it: inside `namespace Json` the
    // text `Json.jsonInt` reaches the constructor `Json.Json.jsonInt`, while
    // at root it reaches the accessor `Json.jsonInt` that `open` only adds an
    // alias beside. The pins name themselves `_root_.AverCertChecker.law_pin_<i>`
    // and cite `_root_.AverCert.Laws.<c>`, so the namespace cannot redirect
    // either name. A law statement means what the MODEL's names and
    // instances make it mean; the instances a package may declare at all are
    // audited by `checker_audit`.
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
    for (index, law) in candidates.laws.iter().enumerate() {
        if !law.prefix.is_empty() {
            law_pins.push_str("namespace ");
            law_pins.push_str(&law.prefix);
            law_pins.push_str("\n\n");
        }
        // The statement is elaborated ALONE, as a definition of its own, and
        // the pins conjoin that definition. However its text is spelled, it
        // is one proposition, so it cannot re-associate the conjunction with
        // `Holds` and the bridges that follow it.
        law_pins.push_str(&format!(
            "def _root_.{LAW_STATEMENT_PREFIX}{index} : Prop :=\n  ("
        ));
        law_pins.push_str(&law.statement);
        law_pins.push_str(")\n\n");
        law_pins.push_str(&format!(
            "theorem _root_.{LAW_PIN_PREFIX}{index} :\n    \
             _root_.{LAW_STATEMENT_PREFIX}{index} ∧ \
             (_root_.AverCert.Schema.Holds _root_.AverCert.manifest)"
        ));
        law_pins.push_str(" :=\n  _root_.AverCert.Laws.");
        law_pins.push_str(&law.corollary);
        law_pins.push_str("\n\n");
        if !law.bridges.is_empty() {
            let bridged_index = bridged_law_indices
                .iter()
                .position(|at| *at == index)
                .expect("every bridged law is enumerated");
            law_pins.push_str(&format!(
                "theorem _root_.{BRIDGED_LAW_PIN_PREFIX}{bridged_index} :\n    \
                 _root_.{LAW_STATEMENT_PREFIX}{index} ∧ \
                 (_root_.AverCert.Schema.Holds _root_.AverCert.manifest)"
            ));
            // The declared bridges, in the manifest's order. The pin's TYPE
            // forces the package's `_bridged` corollary to prove all of them,
            // and the audit that follows walks that whole closure.
            for bridge in &law.bridges {
                law_pins.push_str(&format!(
                    " ∧\n      _root_.{BRIDGE_STATEMENT_PREFIX}{bridge}"
                ));
            }
            law_pins.push_str(" :=\n  _root_.AverCert.Laws.");
            law_pins.push_str(&law.corollary);
            law_pins.push_str(LAW_BRIDGED_COROLLARY_SUFFIX);
            law_pins.push_str("\n\n");
        }
        if !law.prefix.is_empty() {
            law_pins.push_str("end ");
            law_pins.push_str(&law.prefix);
            law_pins.push_str("\n\n");
        }
    }
    // Bridge pins need no namespace context: a bridge statement is rendered
    // by the checker, fully `_root_`-qualified, so it means the same at the
    // root as it does in the package's `Bridge.lean`.
    let mut bridge_pins = String::new();
    for (index, bridge) in candidates.source_bridges.iter().enumerate() {
        bridge_pins.push_str(&format!(
            "def _root_.{BRIDGE_STATEMENT_PREFIX}{index} : Prop :=\n  ("
        ));
        bridge_pins.push_str(&bridge.statement);
        bridge_pins.push_str(")\n\n");
        bridge_pins.push_str(&format!(
            "theorem _root_.{BRIDGE_PIN_PREFIX}{index} :\n    \
             _root_.{BRIDGE_STATEMENT_PREFIX}{index} ∧ \
             (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  _root_."
        ));
        bridge_pins.push_str(&bridge.corollary);
        bridge_pins.push_str("\n\n");
    }
    // The report pins bind every JSON report field to the Lean manifest and
    // the artifact data. Each is a THEOREM the audit walks with the accepted
    // root: a pin closed by `decide +kernel` through a package-declared
    // decision procedure carries that procedure's axioms into the audit.
    let data = "_root_.AverCert.Artifact.data";
    let manifest = "_root_.AverCert.manifest";
    let report_pins: Vec<(String, &str)> = vec![
        (
            format!("{data}.modBytes = _root_.AverCert.ArtifactBytes.modBytes"),
            "rfl",
        ),
        (
            format!("{data}.modLen = _root_.AverCert.ArtifactBytes.modLen"),
            "rfl",
        ),
        (format!("{data}.manifest = {manifest}"), "rfl"),
        (
            format!("{data}.wasip2ComponentEnvelope = {wasip2_component_envelope}"),
            "rfl",
        ),
        (
            format!("{manifest}.subject.artifactHash = \"{sha}\""),
            "rfl",
        ),
        (
            format!(
                "{manifest}.subject.artifactRoot = \"{}\"",
                format::ARTIFACT_CERTIFICATE_ROOT
            ),
            "rfl",
        ),
        (
            format!("{manifest}.obligations.map (fun o => o.export_) = {names}"),
            "rfl",
        ),
        (format!("{manifest}.subject.exports = {names}"), "rfl"),
        (
            format!("_root_.AverCert.ClaimAxes.reportEntries {data} = {report_entries}"),
            "by first | rfl | decide +kernel",
        ),
        (
            format!("_root_.AverCert.ClaimAxes.reportFacets {data} = {report_facets}"),
            "by first | rfl | decide +kernel",
        ),
        (
            format!("{manifest}.obligations.map (fun o => o.policy) = {policies}"),
            "rfl",
        ),
        (
            format!("{manifest}.obligations.map (fun o => o.termination?) = {terminations}"),
            "rfl",
        ),
        (format!("{manifest}.subject.contracts = {contracts}"), "rfl"),
        (
            format!("{manifest}.subject.declaredUncertified = {declared}"),
            "rfl",
        ),
        (
            format!("{manifest}.subject.capabilities = {capabilities}"),
            "rfl",
        ),
        (format!("{manifest}.subject.start = {start}"), "rfl"),
        (format!("{manifest}.subject.hostRoleTable = {roles}"), "rfl"),
        (
            format!("{manifest}.subject.stringHostRoles = {string_roles}"),
            "rfl",
        ),
        (
            format!("{manifest}.subject.target = \"{}\"", candidates.target),
            "rfl",
        ),
        (
            format!("{manifest}.subject.profile = \"{}\"", candidates.profile),
            "rfl",
        ),
        (
            format!("{manifest}.subject.abi = \"{}\"", candidates.abi),
            "rfl",
        ),
    ];
    assert_eq!(
        report_pins.len(),
        REPORT_PIN_COUNT,
        "the audit walks exactly the report pins the witness writes"
    );
    let mut report = String::new();
    for (index, (statement, proof)) in report_pins.iter().enumerate() {
        report.push_str(&format!(
            "theorem _root_.{REPORT_PIN_PREFIX}{index} :\n    {statement} :=\n  {proof}\n\n"
        ));
    }
    format!(
        "-- Authored by aver-cert; never accepted from the certificate.\n\
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
         {report}\
         theorem _root_.{CHECKED_ROOT} :\n    \
           _root_.AverCert.AcceptedArtifact.accepted _root_.AverCert.Artifact.data :=\n  \
           _root_.AverCert.Artifact.certificate\n"
    )
}

/// Number of report pins [`checker_witness`] writes (they are numbered
/// `report_pin_0 ..`); the audit walks every one of them.
const REPORT_PIN_COUNT: usize = 21;

/// A Lean `Name` literal list: `` [`A.b, `C] ``. Every name is checker-chosen
/// or a validated package module root.
fn lean_name_list(names: &[String]) -> String {
    format!(
        "[{}]",
        names
            .iter()
            .map(|name| format!("`{name}"))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// The checker-owned audit program, run as `lake env lean --run
/// CheckerAudit.lean` after the witness is built.
///
/// It is elaborated with ONLY the Lean toolchain in scope — no package module
/// is imported while its code is elaborated — so no instance, notation or
/// declaration a package ships can change what it computes. At run time it
/// loads the built witness environment and:
///
/// 1. walks the axioms of the accepted root and of every report pin; any name
///    outside the whitelist declines the package;
/// 2. refuses a package that declares anything under the reserved
///    `AverCertChecker` prefix, any scoped instance, any parser extension
///    entry (notation, syntax, mixfix operators), or an instance outside the
///    admitted forms (see [`AUDIT_INSTANCE_RULES`]);
/// 3. logs one line per law, bridged-law and bridge pin with its own axiom
///    audit, which Rust reads back for per-claim credit.
///
/// A decline is a line `AVER_AUDIT_DECLINE <reason>` and a nonzero exit.
fn checker_audit(candidates: &Candidates, package_modules: &[String]) -> String {
    let strict_roots: Vec<String> = std::iter::once(CHECKED_ROOT.to_string())
        .chain((0..REPORT_PIN_COUNT).map(|index| format!("{REPORT_PIN_PREFIX}{index}")))
        .collect();
    let law_roots: Vec<String> = (0..candidates.laws.len())
        .map(|index| format!("{LAW_PIN_PREFIX}{index}"))
        .collect();
    let bridged_law_roots: Vec<String> = (0..bridged_law_indices(&candidates.laws).len())
        .map(|index| format!("{BRIDGED_LAW_PIN_PREFIX}{index}"))
        .collect();
    let bridge_roots: Vec<String> = (0..candidates.source_bridges.len())
        .map(|index| format!("{BRIDGE_PIN_PREFIX}{index}"))
        .collect();
    let allowed: Vec<String> = AXIOM_WHITELIST
        .iter()
        .map(|name| name.to_string())
        .collect();
    let mut records: Vec<(String, Vec<String>)> = Vec::new();
    let mut sums: Vec<(String, Vec<(String, usize)>)> = Vec::new();
    for bridge in &candidates.source_bridges {
        for encoder in bridge.params.iter().chain(std::iter::once(&bridge.result)) {
            collect_encoder_shapes(encoder, &mut records, &mut sums);
        }
    }
    let records = format!(
        "[{}]",
        records
            .iter()
            .map(|(ty, fields)| format!("(`{ty}, {})", lean_name_list(fields)))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let sums = format!(
        "[{}]",
        sums.iter()
            .map(|(ty, ctors)| format!(
                "(`{ty}, [{}])",
                ctors
                    .iter()
                    .map(|(ctor, fields)| format!("(`{ctor}, {})", lean_nat(fields)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .collect::<Vec<_>>()
            .join(", ")
    );
    AUDIT_TEMPLATE
        .replace("@RECORDS@", &records)
        .replace("@SUMS@", &sums)
        .replace("@PACKAGE_MODULES@", &lean_name_list(package_modules))
        .replace(
            "@WALL_ROOTS@",
            &lean_name_list(&WALL_NAMESPACE_ROOTS.map(str::to_string)),
        )
        .replace(
            "@PACKAGE_AVERCERT_CHILDREN@",
            &lean_name_list(&PACKAGE_AVERCERT_CHILDREN.map(str::to_string)),
        )
        .replace("@ALLOWED@", &lean_name_list(&allowed))
        .replace("@STRICT_ROOTS@", &lean_name_list(&strict_roots))
        .replace("@LAW_ROOTS@", &lean_name_list(&law_roots))
        .replace("@BRIDGED_LAW_ROOTS@", &lean_name_list(&bridged_law_roots))
        .replace("@BRIDGE_ROOTS@", &lean_name_list(&bridge_roots))
        .replace("@LAW_MARKER@", LAW_AUDIT_MARKER)
        .replace("@BRIDGED_LAW_MARKER@", LAW_BRIDGE_AUDIT_MARKER)
        .replace("@BRIDGE_MARKER@", BRIDGE_AUDIT_MARKER)
        .replace("@DECLINE_MARKER@", AUDIT_DECLINE_MARKER)
        .replace("@OK_MARKER@", AUDIT_OK_MARKER)
}

/// Every record and sum a bridge encoder reads, with the members it lists
/// (names without `_root_.`; a record's fields by their last segment). Each
/// type is listed once, at its first encoder.
fn collect_encoder_shapes(
    encoder: &SourceEncoder,
    records: &mut Vec<(String, Vec<String>)>,
    sums: &mut Vec<(String, Vec<(String, usize)>)>,
) {
    let bare = |name: &str| {
        name.strip_prefix(bridge_statement::ROOT_PREFIX)
            .unwrap_or(name)
            .to_string()
    };
    match encoder {
        SourceEncoder::Int | SourceEncoder::Bool | SourceEncoder::Float | SourceEncoder::Str => {}
        SourceEncoder::Record {
            lean_type, fields, ..
        } => {
            let ty = bare(lean_type);
            let listed: Vec<String> = fields
                .iter()
                .map(|(accessor, _)| {
                    accessor
                        .rsplit_once('.')
                        .map_or(accessor.clone(), |(_, field)| field.to_string())
                })
                .collect();
            if !records.iter().any(|(seen, _)| *seen == ty) {
                records.push((ty, listed));
            }
            for (_, field) in fields {
                collect_encoder_shapes(field, records, sums);
            }
        }
        SourceEncoder::Sum {
            lean_type, ctors, ..
        } => {
            let ty = bare(lean_type);
            if !sums.iter().any(|(seen, _)| *seen == ty) {
                sums.push((
                    ty,
                    ctors
                        .iter()
                        .map(|(ctor, fields)| (bare(ctor), fields.len()))
                        .collect(),
                ));
            }
            for field in ctors.iter().flat_map(|(_, fields)| fields) {
                collect_encoder_shapes(field, records, sums);
            }
        }
        SourceEncoder::Option(inner)
        | SourceEncoder::List(inner)
        | SourceEncoder::Vector(inner) => collect_encoder_shapes(inner, records, sums),
        SourceEncoder::Result { ok, err } => {
            collect_encoder_shapes(ok, records, sums);
            collect_encoder_shapes(err, records, sums);
        }
        SourceEncoder::Tuple { elems, .. } => {
            for elem in elems {
                collect_encoder_shapes(elem, records, sums);
            }
        }
    }
}

/// Every namespace root the wall, the checker-rendered modules and the
/// witness declare in, apart from `AverCert` itself. The audit program
/// declines a package constant under any of them.
const WALL_NAMESPACE_ROOTS: [&str; 9] = [
    "AcceptanceSoundness",
    "ArithTemplateDerisk",
    "AverBits",
    "AverCertChecker",
    "CertDecode",
    "CertModule",
    "CertPrelude",
    "InterpreterSequencing",
    "AverCertAudit",
];

/// The namespaces under `AverCert` that the producer declares in (`Plans`,
/// the `Artifact*` byte facts, `Final`, `Bridge*`, `Laws`) and the manifest's
/// two definitions. Every other `AverCert.*` name belongs to the wall.
const PACKAGE_AVERCERT_CHILDREN: [&str; 7] = [
    "Artifact", "Bridge", "Final", "Laws", "Plans", "manifest", "subject",
];

/// The audit program's source; `@…@` placeholders are filled by
/// [`checker_audit`].
const AUDIT_TEMPLATE: &str = include_str!("checker_audit.lean");

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
        if class != format::PLAN_CLASS {
            return Err(format!(
                "certified export `{}` reports class `{}`; schema {} has the one class `{}`",
                display_safe(&name),
                display_safe(&class),
                format::CERT_SCHEMA_VERSION,
                format::PLAN_CLASS
            ));
        }
        let facets = entry
            .get("facets")
            .and_then(Value::as_array)
            .ok_or_else(|| {
                format!(
                    "certified export `{}` is missing `facets`",
                    display_safe(&name)
                )
            })?
            .iter()
            .map(|facet| {
                facet.as_str().map(str::to_string).ok_or_else(|| {
                    "cert-manifest.json `certified[].facets[]` is not a string".to_string()
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        certified.push(CertifiedCandidate {
            name,
            class,
            facets,
            policy,
            policy_lean,
            termination_lean,
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
                "kind",
                "params",
                "result",
            ],
        )?;
        let kind_tag = required_string(entry, "kind", &context)?;
        let kind = BridgeKind::from_tag(&kind_tag).ok_or_else(|| {
            format!(
                "cert-manifest.json `{context}.kind` is not a bridge statement kind: `{}`",
                display_safe(&kind_tag)
            )
        })?;
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
            kind,
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
        // The bridges a law conjoins are those of the functions its statement
        // names — all of them, in first-appearance order — and nothing else.
        if !law.bridges.is_empty() {
            let models: Vec<&str> = source_bridges
                .iter()
                .map(|bridge| bridge.model.as_str())
                .collect();
            let mentioned = bridge_statement::law_mentioned_bridges(&law.statement, &models);
            if law.bridges != mentioned {
                return Err(format!(
                    "law-claim `{}` cites bridges that are not exactly those of the functions \
                     its statement names",
                    display_safe(&law.label)
                ));
            }
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
            &["box", "add", "mul", "sub", "toIndex", "cmp", "eq", "divmod"],
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
            optional_index("divmod")?,
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
    if let Err(field) = lean_gate::law_claim_identifiers(&law.label, &law.theorem, &law.corollary) {
        return Err(format!(
            "law-claim `{}` field `{field}` is not a plain dotted Lean identifier",
            law.label
        ));
    }
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
    kind: BridgeKind,
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
    // The same two identifier rules the producer applies before it declares a
    // bridge (`bridge_statement`), so a model name the producer writes — the
    // transpiler's trailing-prime escape of a reserved word included — is
    // never one this gate refuses for the whole package.
    if !bridge_statement::is_plain_export_name(&bridge.export) {
        return Err(format!(
            "source-bridge export `{}` is not a plain Lean identifier",
            display_safe(&bridge.export)
        ));
    }
    if !bridge_statement::is_plain_dotted_name(&bridge.model) {
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
                 and its own accessors and constructors, or exceeds the encoder caps",
                display_safe(&bridge.export)
            ));
        }
    }
    let statement = render_bridge_statement(
        &bridge.export,
        &bridge.model,
        bridge.kind,
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
        kind: bridge.kind,
        statement,
        params: bridge.params,
        result: bridge.result,
    })
}

/// Read one declared encoder. The kind set is CLOSED and matched exactly, so an
/// unknown kind — or an entry missing a key, or carrying an extra one —
/// declines the package instead of being rendered into some default shape.
/// Nesting is bounded before recursion, so a hostile manifest cannot exhaust
/// the stack.
fn read_source_encoder(value: &Value, context: &str) -> Result<SourceEncoder, String> {
    read_source_encoder_at(value, context, 0)
}

fn read_source_encoder_at(
    value: &Value,
    context: &str,
    depth: usize,
) -> Result<SourceEncoder, String> {
    if depth >= bridge_statement::MAX_ENCODER_DEPTH {
        return Err(format!(
            "cert-manifest.json `{context}` nests encoders deeper than {}",
            bridge_statement::MAX_ENCODER_DEPTH
        ));
    }
    let kind = value
        .get(bridge_statement::ENCODER_KIND_KEY)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("cert-manifest.json `{context}.kind` is not a string"))?;
    let tid = |value: &Value| -> Result<u32, String> {
        value
            .get("tid")
            .and_then(Value::as_u64)
            .and_then(|t| u32::try_from(t).ok())
            .ok_or_else(|| format!("cert-manifest.json `{context}.tid` is not a type id"))
    };
    let child = |key: &str| -> Result<Box<SourceEncoder>, String> {
        Ok(Box::new(read_source_encoder_at(
            &value[key],
            &format!("{context}.{key}"),
            depth + 1,
        )?))
    };
    let array = |key: &str| -> Result<&Vec<Value>, String> {
        value[key]
            .as_array()
            .ok_or_else(|| format!("cert-manifest.json `{context}.{key}` is not an array"))
    };
    match kind {
        bridge_statement::ENCODER_KIND_INT
        | bridge_statement::ENCODER_KIND_BOOL
        | bridge_statement::ENCODER_KIND_FLOAT
        | bridge_statement::ENCODER_KIND_STRING => {
            exact_object_fields(value, context, &[bridge_statement::ENCODER_KIND_KEY])?;
            Ok(match kind {
                bridge_statement::ENCODER_KIND_INT => SourceEncoder::Int,
                bridge_statement::ENCODER_KIND_BOOL => SourceEncoder::Bool,
                bridge_statement::ENCODER_KIND_FLOAT => SourceEncoder::Float,
                _ => SourceEncoder::Str,
            })
        }
        bridge_statement::ENCODER_KIND_RECORD => {
            exact_object_fields(value, context, &["kind", "tid", "type", "fields"])?;
            let mut fields = Vec::new();
            for (index, field) in array("fields")?.iter().enumerate() {
                let at = format!("{context}.fields[{index}]");
                exact_object_fields(field, &at, &["accessor", "encoder"])?;
                fields.push((
                    required_string(field, "accessor", &at)?,
                    read_source_encoder_at(&field["encoder"], &format!("{at}.encoder"), depth + 1)?,
                ));
            }
            Ok(SourceEncoder::Record {
                tid: tid(value)?,
                lean_type: required_string(value, "type", context)?,
                fields,
            })
        }
        bridge_statement::ENCODER_KIND_SUM => {
            exact_object_fields(value, context, &["kind", "tid", "type", "ctors"])?;
            let mut ctors = Vec::new();
            for (index, ctor) in array("ctors")?.iter().enumerate() {
                let at = format!("{context}.ctors[{index}]");
                exact_object_fields(ctor, &at, &["ctor", "fields"])?;
                let mut fields = Vec::new();
                for (position, field) in ctor["fields"]
                    .as_array()
                    .ok_or_else(|| format!("cert-manifest.json `{at}.fields` is not an array"))?
                    .iter()
                    .enumerate()
                {
                    fields.push(read_source_encoder_at(
                        field,
                        &format!("{at}.fields[{position}]"),
                        depth + 1,
                    )?);
                }
                ctors.push((required_string(ctor, "ctor", &at)?, fields));
            }
            Ok(SourceEncoder::Sum {
                tid: tid(value)?,
                lean_type: required_string(value, "type", context)?,
                ctors,
            })
        }
        bridge_statement::ENCODER_KIND_OPTION => {
            exact_object_fields(value, context, &["kind", "elem"])?;
            Ok(SourceEncoder::Option(child("elem")?))
        }
        bridge_statement::ENCODER_KIND_RESULT => {
            exact_object_fields(value, context, &["kind", "ok", "err"])?;
            Ok(SourceEncoder::Result {
                ok: child("ok")?,
                err: child("err")?,
            })
        }
        bridge_statement::ENCODER_KIND_TUPLE => {
            exact_object_fields(value, context, &["kind", "tid", "elems"])?;
            let mut elems = Vec::new();
            for (index, elem) in array("elems")?.iter().enumerate() {
                elems.push(read_source_encoder_at(
                    elem,
                    &format!("{context}.elems[{index}]"),
                    depth + 1,
                )?);
            }
            Ok(SourceEncoder::Tuple {
                tid: tid(value)?,
                elems,
            })
        }
        bridge_statement::ENCODER_KIND_LIST => {
            exact_object_fields(value, context, &["kind", "elem"])?;
            Ok(SourceEncoder::List(child("elem")?))
        }
        bridge_statement::ENCODER_KIND_VECTOR => {
            exact_object_fields(value, context, &["kind", "elem"])?;
            Ok(SourceEncoder::Vector(child("elem")?))
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
            kind: bridge.kind,
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
        return Ok("_root_.Option.none".to_string());
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
        "_root_.Option.some ({{ measure := .intNatAbs {}, descent := {} }} : _root_.AverCert.Schema.TerminationWitness)",
        lean_nat(parameter),
        lean_int(descent)
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
        for facet in &candidate.facets {
            gate_candidate("certified facet", facet)?;
        }
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
    let safe = value.len() <= crate::format::MAX_CANDIDATE_LEN
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
                | "Module.lean"
                | "lakefile.lean"
                | "CheckerWitness.lean"
                | "CheckerAudit.lean"
        )
}

fn assemble_build(
    cert_dir: &Path,
    artifact_hash: &str,
    core_module_bytes: &[u8],
    target_artifact_bytes: &[u8],
    selected_wall: &wall::Wall,
    memory_limit_mb: u64,
) -> Result<BuildDir, String> {
    let mut build = BuildDir::new()?;
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
        if matches!(
            name.as_str(),
            "Manifest.lean" | "Certificate.lean" | "Bridge.lean" | "Laws.lean"
        ) {
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
    build.package_roots = roots.clone();
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
    // The wall's `Schema` imports `Module`, so it is rendered here from the
    // hash of the bytes read, never staged from the package: no package
    // module may sit inside the wall's own import closure.
    std::fs::write(
        build.path.join("Module.lean"),
        wall::render_module(artifact_hash),
    )
    .map_err(|error| format!("cannot stage Module.lean: {error}"))?;
    roots.push("Module".to_string());
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
    lean_gate::lean_module_root(name)
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
            "Module",
            "CheckerWitness",
            "CheckerAudit",
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
/// *code* position. The lexer and the token list live in
/// [`crate::lean_gate`], which the producer runs over its own model files
/// with the very same code, so a package the producer ships never fails this
/// gate on a file it could have left out.
fn scan_for_code_exec(name: &str, contents: &[u8]) -> Result<(), String> {
    let text = String::from_utf8_lossy(contents);
    if let Some(token) = lean_gate::code_exec_token(&text) {
        return Err(format!(
            "cert data file `{name}` contains refused construct `{token}`"
        ));
    }
    Ok(())
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
    value.map_or_else(
        || "_root_.Option.none".to_string(),
        |value| format!("(_root_.Option.some {})", lean_nat(value)),
    )
}

fn lean_wasip2_component_envelope(
    value: Option<format::Wasip2ComponentEnvelopeDeclaration>,
) -> String {
    value.map_or_else(
        || "(_root_.Option.none : _root_.Option _root_.AverCert.Wasip2Envelope.ComponentEnvelope)"
            .to_string(),
        |value| {
            format!(
                "_root_.Option.some ({{ prefixLen := {}, embeddedCoreModuleLen := {}, suffixLen := {} }} : _root_.AverCert.Wasip2Envelope.ComponentEnvelope)",
                lean_nat(value.prefix_len),
                lean_nat(value.embedded_core_module_len),
                lean_nat(value.suffix_len)
            )
        },
    )
}

fn sha256_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

struct BuildDir {
    path: PathBuf,
    /// Module roots of the staged certificate package (never wall or
    /// checker-authored modules).
    package_roots: Vec<String>,
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
        Ok(Self {
            path,
            package_roots: Vec::new(),
        })
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
    let started = std::time::Instant::now();
    let output = lean
        .run_lake(build_dir, phase, arguments)
        .map_err(|error| error.to_string())?;
    report_step_timing(phase, started.elapsed(), &output.stdout);
    Ok(LakeOut {
        status: output.status,
        combined: format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ),
    })
}

/// Opt-in developer timing trace (`AVER_CERT_TIMINGS=1`): one stderr line per
/// Lean step, plus Lake's own per-module build lines. Diagnostic only; it
/// reads nothing the verdict depends on.
fn report_step_timing(phase: &str, elapsed: std::time::Duration, stdout: &[u8]) {
    if std::env::var_os("AVER_CERT_TIMINGS").is_none_or(|value| value.is_empty() || value == "0") {
        return;
    }
    for line in String::from_utf8_lossy(stdout).lines() {
        if line.contains("Built ") || line.contains("Replayed ") {
            eprintln!("aver-cert timing:   {}", line.trim());
        }
    }
    eprintln!("aver-cert timing: {phase}: {:.1}s", elapsed.as_secs_f64());
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

/// Printed by `explain` under every certificate with a certified export.
const INT_INPUT_DOMAIN_LINE: &str = "domain: every Int input (an argument, or a field, \
     element or payload inside one) is assumed to be a canonical carrier word, the \
     runtime's normal form; a non-canonical word is outside the certified domain. Every \
     Int result is proved canonical.";

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
        println!("    {}", export.certified_model);
    }
    // The one assumption every certified theorem makes about its INPUTS rather
    // than about a helper: the wall's value relation reads an Int through
    // `CanonRepr`, so an Int carrier word the host passes in is taken to be in
    // the runtime's normal form. It is the same for every export, so it is
    // stated once.
    println!("\n{}", "Certified domain".yellow().bold());
    println!("  {INT_INPUT_DOMAIN_LINE}");
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
                "  {}  ≡ {}  ({})  [{credit}]",
                display_safe(&bridge.export).bold(),
                display_safe(&bridge.model),
                bridge.kind.tag()
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
            kind: BridgeKind::Exact,
            params: vec![SourceEncoder::Int],
            result: SourceEncoder::Int,
        }
    }

    fn record_encoder(accessors: &[&str]) -> SourceEncoder {
        SourceEncoder::Record {
            tid: 0,
            lean_type: "_root_.Domain.Fraction".to_string(),
            fields: accessors
                .iter()
                .map(|accessor| (accessor.to_string(), SourceEncoder::Int))
                .collect(),
        }
    }

    fn fraction_encoder() -> SourceEncoder {
        record_encoder(&[
            "_root_.Domain.Fraction.top",
            "_root_.Domain.Fraction.bottom",
        ])
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
            tid: 0,
            lean_type: "Domain.Fraction".to_string(),
            fields: vec![("Domain.Fraction.top".to_string(), SourceEncoder::Int)],
        };
        assert!(validate_source_bridge_candidate(bare_names).is_err());

        // An accessor of some other type is not a field of the declared one.
        let mut foreign_accessor = raw_bridge("one");
        foreign_accessor.result = record_encoder(&["_root_.Other.Record.top"]);
        assert!(validate_source_bridge_candidate(foreign_accessor).is_err());

        // So is a constructor of some other sum.
        let mut foreign_ctor = raw_bridge("one");
        foreign_ctor.params = vec![SourceEncoder::Sum {
            tid: 1,
            lean_type: "_root_.Domain.Op".to_string(),
            ctors: vec![("_root_.Domain.Tag.a".to_string(), Vec::new())],
        }];
        assert!(validate_source_bridge_candidate(foreign_ctor).is_err());
    }

    /// The transpiler escapes a source function named after a Lean keyword
    /// with a trailing prime (`none` becomes `none'`), and the producer names
    /// that model in its bridge entry. The checker used to refuse the primed
    /// name as "not a plain dotted identifier" — for the whole package, since a
    /// refused entry fails candidate parsing. Producer and checker now apply
    /// the one rule of `bridge_statement`, which admits the escape in every
    /// segment and nothing else.
    #[test]
    fn a_keyword_escaped_model_name_passes_the_bridge_gate() {
        let mut escaped = raw_bridge("Domain_Policy_none");
        escaped.model = "Domain.Policy.none'".to_string();
        let candidate =
            validate_source_bridge_candidate(escaped).expect("the escaped model name is admitted");
        assert!(
            candidate.statement.contains("_root_.Domain.Policy.none'"),
            "{}",
            candidate.statement
        );
        for refused in [
            "Domain.Policy.'none",
            "Domain.Policy..none",
            "Domain.Policy.none\u{ab}",
        ] {
            let mut entry = raw_bridge("Domain_Policy_none");
            entry.model = refused.to_string();
            assert!(
                validate_source_bridge_candidate(entry).is_err(),
                "{refused}"
            );
        }
        let mut primed_export = raw_bridge("Domain_Policy_none'");
        primed_export.model = "Domain.Policy.none'".to_string();
        assert!(validate_source_bridge_candidate(primed_export).is_err());
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
                BridgeKind::Exact,
                &[fraction_encoder(), fraction_encoder()],
                &fraction_encoder(),
            ),
            "the pinned statement is the renderer's output and nothing else"
        );
        assert!(
            candidate.statement.starts_with(
                "_root_.AverCert.GrammarBridge.Exact _root_.AverCert.manifest \
                 \"Domain_Rational_plus\""
            ),
            "the claim's left-hand side is the export's own obligation model: {}",
            candidate.statement
        );
        // The weaker kind renders a different claim too.
        let mut adequate = raw_bridge("Domain_Rational_plus");
        adequate.model = "Domain.Rational.plus".to_string();
        adequate.kind = BridgeKind::Adequate;
        adequate.params = vec![fraction_encoder(), fraction_encoder()];
        adequate.result = fraction_encoder();
        let adequate = validate_source_bridge_candidate(adequate).expect("it still validates");
        assert_ne!(adequate.statement, candidate.statement);
        // Naming a different export renders a different claim, so the pin no
        // longer has the package corollary's type — a decline, not a credit.
        let mut renamed = raw_bridge("Domain_Rational_minus");
        renamed.model = "Domain.Rational.plus".to_string();
        renamed.params = vec![fraction_encoder(), fraction_encoder()];
        renamed.result = fraction_encoder();
        let renamed = validate_source_bridge_candidate(renamed).expect("it still validates");
        assert_ne!(renamed.statement, candidate.statement);
        // So does permuting a record's accessors.
        let permuted = record_encoder(&[
            "_root_.Domain.Fraction.bottom",
            "_root_.Domain.Fraction.top",
        ]);
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
                    "tid": 0,
                    "type": "_root_.Domain.Fraction",
                    "fields": [{"accessor": "_root_.Domain.Fraction.top", "encoder": {"kind": "int"}}],
                }),
                "e"
            )
            .unwrap(),
            record_encoder(&["_root_.Domain.Fraction.top"])
        );
        // Every encoder the producer writes reads back to itself.
        let op = SourceEncoder::Sum {
            tid: 1,
            lean_type: "_root_.Domain.Op".to_string(),
            ctors: vec![
                ("_root_.Domain.Op.add".to_string(), vec![SourceEncoder::Int]),
                ("_root_.Domain.Op.zero".to_string(), Vec::new()),
            ],
        };
        for encoder in [
            SourceEncoder::Float,
            SourceEncoder::Str,
            fraction_encoder(),
            op.clone(),
            SourceEncoder::Option(Box::new(op.clone())),
            SourceEncoder::Result {
                ok: Box::new(SourceEncoder::Int),
                err: Box::new(SourceEncoder::Str),
            },
            SourceEncoder::Tuple {
                tid: 2,
                elems: vec![SourceEncoder::Int, fraction_encoder()],
            },
            SourceEncoder::List(Box::new(SourceEncoder::Bool)),
            SourceEncoder::Vector(Box::new(SourceEncoder::Int)),
        ] {
            let json: Value = serde_json::from_str(&encoder.to_json()).expect("valid JSON");
            assert_eq!(read_source_encoder(&json, "e").unwrap(), encoder);
        }
        let mut deep = serde_json::json!({"kind": "int"});
        for _ in 0..bridge_statement::MAX_ENCODER_DEPTH {
            deep = serde_json::json!({"kind": "option", "elem": deep});
        }
        for bad in [
            serde_json::json!({"kind": "decimal"}),
            serde_json::json!({}),
            serde_json::json!({"kind": "int", "type": "_root_.Domain.Fraction"}),
            serde_json::json!({"kind": "record", "tid": 0, "type": "_root_.Domain.Fraction"}),
            serde_json::json!({"kind": "record", "tid": 0, "type": "_root_.Domain.Fraction", "fields": "top"}),
            serde_json::json!({"kind": "record", "tid": 0, "type": "_root_.Domain.Fraction", "fields": [7]}),
            serde_json::json!({"kind": "record", "type": "_root_.Domain.Fraction", "fields": []}),
            serde_json::json!({"kind": "option"}),
            serde_json::json!({"kind": "sum", "tid": 1, "type": "_root_.Domain.Op", "ctors": [{"ctor": "_root_.Domain.Op.a"}]}),
            deep,
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
            "kind",
            "params",
            "result",
        ];
        let honest = serde_json::json!({
            "export": "one",
            "theorem": "AverCert.Bridge.one",
            "corollary": "AverCert.Bridge.one_certified",
            "model": "Domain.one",
            "kind": "exact",
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
                        kind: BridgeKind::Exact,
                        statement: "_root_.One".to_string(),
                        offending: Vec::new(),
                    },
                    BridgeOutcome {
                        export: "two".to_string(),
                        model: "Domain.two".to_string(),
                        kind: BridgeKind::Adequate,
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
            name: "sumTo".to_string(),
            class: format::PLAN_CLASS.to_string(),
            facets: vec!["recursive".to_string(), "calls".to_string()],
            policy: "simulatesModelTotally".to_string(),
            policy_lean: ".simulatesModelTotally",
            termination_lean: "none".to_string(),
        };
        assert_eq!(
            report_face(&candidate),
            "class: source-plan-v1 (recursive, calls)"
        );
        assert_eq!(
            certified_model_line(&candidate, &[]),
            "model: plan (the export's optimized MIR body)"
        );
        let bare = CertifiedCandidate {
            facets: Vec::new(),
            ..candidate
        };
        assert_eq!(report_face(&bare), "class: source-plan-v1");
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

    /// The audit's namespace rule is only as good as its list of wall roots:
    /// every namespace a wall file opens at the top level is on it.
    #[test]
    fn audit_namespace_roots_cover_every_wall_namespace() {
        let wall = wall::resolve(wall::current_id()).expect("embedded wall resolves");
        for source in wall.sources {
            // Blocks closed by a bare or named `end`: namespaces, sections and
            // `mutual` groups. Only a namespace opened outside all of them
            // names a root.
            let mut depth = 0usize;
            for line in source.contents.lines() {
                let words: Vec<&str> = line.split_whitespace().collect();
                match words.as_slice() {
                    ["namespace", name, ..] => {
                        if depth == 0 {
                            let root = name.split('.').next().unwrap();
                            assert!(
                                root == "AverCert" || WALL_NAMESPACE_ROOTS.contains(&root),
                                "{} opens namespace {name}, whose root the audit does not reserve",
                                source.name
                            );
                        }
                        depth += 1;
                    }
                    ["section", ..] | ["noncomputable", "section", ..] | ["mutual", ..] => {
                        depth += 1
                    }
                    ["end", ..] => depth = depth.saturating_sub(1),
                    _ => {}
                }
            }
        }
        let rendered = wall::render_module(&"0".repeat(64));
        assert!(rendered.contains("namespace CertModule"));
        assert!(WALL_NAMESPACE_ROOTS.contains(&"CertModule"));
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
        assert!(reject_shadowed_root("Module", wall).is_err());
        assert!(reject_shadowed_root("Module.Decoy", wall).is_err());
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
        assert!(rendered.contains("noncomputable def modBytes : Nat :=\n  0x6d736100\n"));
        assert!(rendered.contains("def modLen : Nat := 4"));
        // Past one numeral chunk, each chunk sits at its byte offset.
        let mut long = vec![0u8; 1025];
        long[0] = 0x01;
        long[1024] = 0xab;
        let rendered = wall::render_artifact_bytes(&long);
        assert!(rendered.contains("noncomputable def modBytes : Nat :=\n  0x"));
        assert!(rendered.contains("01 |||\n  (0xab <<< 8192)\n"));
        assert!(rendered.contains("def modLen : Nat := 1025"));
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

    fn witness_candidates() -> Candidates {
        let mut laws = law_candidates(&["Domain.plus.comm"]);
        laws[0].bridges = vec![0];
        let mut bridge = raw_bridge("Domain_plus");
        bridge.model = "Domain.plus".to_string();
        bridge.params = vec![fraction_encoder(), fraction_encoder()];
        bridge.result = fraction_encoder();
        Candidates {
            certified: vec![CertifiedCandidate {
                name: "Domain_plus".to_string(),
                class: format::PLAN_CLASS.to_string(),
                facets: vec!["recursive".to_string()],
                policy: "simulatesModelTotally".to_string(),
                policy_lean: ".simulatesModelTotally",
                termination_lean: parse_termination(
                    Some(&serde_json::json!({
                        "measure": {"kind": "intNatAbs", "param_index": 1},
                        "descent": -3
                    })),
                    "Domain_plus",
                )
                .unwrap(),
            }],
            laws,
            source_bridges: vec![validate_source_bridge_candidate(bridge).unwrap()],
            contracts: vec!["c".to_string()],
            declared_uncertified: Vec::new(),
            capabilities: Vec::new(),
            start: Some(7),
            host_role_table: Some((Some(7), Some(8), None, None, None, None, None, Some(13))),
            string_host_roles: vec![(21, StringHostRole::Eq)],
            target: "wasm-gc".to_string(),
            profile: "AverUserProfile/v1".to_string(),
            abi: "aver-wasm-gc/0".to_string(),
            wasip2_component_envelope: None,
        }
    }

    /// The witness is pure pins: no `Lean` import, no command that runs code,
    /// every wall or package name `_root_`-qualified (so a declaration under
    /// `AverCertChecker.AverCert.…` is never reached), and no numeral an
    /// `OfNat` instance could reinterpret.
    #[test]
    fn the_witness_names_everything_from_the_root() {
        let witness = checker_witness("ab12", &witness_candidates());
        assert!(!witness.contains("import Lean"), "{witness}");
        assert!(!witness.contains("run_cmd") && !witness.contains("#eval"));
        assert!(!witness.contains("namespace AverCertChecker"));
        for (at, _) in witness.match_indices("AverCert") {
            let before = &witness[..at];
            assert!(
                // A name, or the root's name inside a string literal.
                before.ends_with("_root_.") || before.ends_with('"'),
                "unqualified name at {at}: {}",
                &witness[at.saturating_sub(40)..(at + 40).min(witness.len())]
            );
        }
        assert!(witness.contains(&format!(
            "theorem _root_.{CHECKED_ROOT} :\n    \
             _root_.AverCert.AcceptedArtifact.accepted _root_.AverCert.Artifact.data :=\n  \
             _root_.AverCert.Artifact.certificate"
        )));
        for index in 0..REPORT_PIN_COUNT {
            assert!(witness.contains(&format!("theorem _root_.{REPORT_PIN_PREFIX}{index} :")));
        }
        assert!(witness.contains("(_root_.Option.some (nat_lit 7))"));
        assert!(witness.contains("(_root_.Int.negSucc (nat_lit 2))"));
        assert!(witness.contains("((nat_lit 21), .eq)"));
        assert!(!witness.contains("some 7") && !witness.contains("≤"));
    }

    /// Every pinned statement is elaborated alone, as a checker definition,
    /// and the pins conjoin the definitions: no statement text sits beside the
    /// `∧` that joins it to `Holds` or to a bridge, so no text can change how
    /// the conjunction associates.
    #[test]
    fn the_witness_conjoins_statement_definitions_not_statement_text() {
        let candidates = witness_candidates();
        let witness = checker_witness("ab12", &candidates);
        let holds = "(_root_.AverCert.Schema.Holds _root_.AverCert.manifest)";
        assert!(witness.contains(&format!(
            "def _root_.{LAW_STATEMENT_PREFIX}0 : Prop :=\n  ({})\n",
            candidates.laws[0].statement
        )));
        assert!(witness.contains(&format!(
            "def _root_.{BRIDGE_STATEMENT_PREFIX}0 : Prop :=\n  ({})\n",
            candidates.source_bridges[0].statement
        )));
        assert!(witness.contains(&format!(
            "theorem _root_.{LAW_PIN_PREFIX}0 :\n    _root_.{LAW_STATEMENT_PREFIX}0 ∧ {holds} :="
        )));
        assert!(witness.contains(&format!(
            "theorem _root_.{BRIDGED_LAW_PIN_PREFIX}0 :\n    \
             _root_.{LAW_STATEMENT_PREFIX}0 ∧ {holds} ∧\n      \
             _root_.{BRIDGE_STATEMENT_PREFIX}0 :="
        )));
        assert!(witness.contains(&format!(
            "theorem _root_.{BRIDGE_PIN_PREFIX}0 :\n    _root_.{BRIDGE_STATEMENT_PREFIX}0 ∧ {holds} :="
        )));
        // The statement text appears once per statement: in its definition.
        assert_eq!(
            witness
                .matches(candidates.laws[0].statement.as_str())
                .count(),
            1
        );
    }

    /// The audit program is fully instantiated, walks the pins the witness
    /// writes, and reads the encoder shapes the bridges declare.
    #[test]
    fn the_audit_program_walks_every_pin() {
        let candidates = witness_candidates();
        let audit = checker_audit(&candidates, &["Manifest".to_string(), "Laws".to_string()]);
        assert!(!audit.contains('@'), "an unfilled placeholder: {audit}");
        assert!(audit.contains("def packageModules : List Name := [`Manifest, `Laws]"));
        assert!(audit.contains(&format!("`{CHECKED_ROOT}, `{REPORT_PIN_PREFIX}0,")));
        assert!(audit.contains(&format!("`{REPORT_PIN_PREFIX}{}]", REPORT_PIN_COUNT - 1)));
        assert!(audit.contains(&format!("[`{LAW_PIN_PREFIX}0]")));
        assert!(audit.contains(&format!("[`{BRIDGED_LAW_PIN_PREFIX}0]")));
        assert!(audit.contains(&format!("[`{BRIDGE_PIN_PREFIX}0]")));
        assert!(
            audit.contains("[(`Domain.Fraction, [`top, `bottom])]"),
            "{audit}"
        );
    }

    /// The final replay covers the witness module, so every module it imports
    /// — laws, bridges and model included — is replayed.
    #[test]
    fn the_fresh_replay_replays_the_witness_closure() {
        assert_eq!(FRESH_REPLAY_ARGS[3], "CheckerWitness");
        assert_eq!(
            replay_args_for(ReplayMode::Fresh, None).unwrap(),
            vec!["env", "leanchecker", "--fresh", "CheckerWitness"]
        );
    }

    /// A law-claim's bridge list is exactly the bridges of the functions its
    /// statement names.
    #[test]
    fn a_law_lists_exactly_the_bridges_its_statement_names() {
        let models = ["Domain.plus", "Domain.times"];
        assert_eq!(
            bridge_statement::law_mentioned_bridges(
                "∀ (a : Int), Domain.times (Domain.plus a a) a = Domain.plus a a",
                &models
            ),
            vec![1, 0]
        );
        assert!(bridge_statement::law_mentioned_bridges("∀ (a : Int), a = a", &models).is_empty());
    }
}
