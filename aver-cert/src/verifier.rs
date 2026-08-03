//! Minimal consumer for Aver artifact certificates.
//!
//! Rust owns transport: it selects the embedded wall, stages untrusted DATA,
//! injects the exact artifact bytes, and runs Lean. It does not disassemble the
//! module or reconstruct an obligation. The checker-owned Lean predicate binds
//! every accepted claim to the bytes and derives its standard face, policy,
//! termination witness, host table, and runtime contracts.

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
    Certified { summary: String, faces: Vec<String> },
    NoExports(String),
}

/// Developer preflight result. A green value means the checker-owned witness
/// elaborated successfully while trusting the local Lake `.olean` graph. It is
/// deliberately distinct from [`Verdict`]: only [`verify`] performs the final
/// fresh-environment replay required for certification.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CheckVerdict {
    Checked { summary: String, faces: Vec<String> },
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
    manifest_face: String,
}

struct TrustedReport {
    exports: Vec<CertifiedExport>,
    contracts: Vec<String>,
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

struct Candidates {
    certified: Vec<CertifiedCandidate>,
    contracts: Vec<String>,
    declared_uncertified: Vec<(String, String)>,
    capabilities: Vec<(String, String)>,
    start: Option<u32>,
    host_role_table: Option<HostRoleTable>,
    string_host_roles: Vec<(u32, StringHostRole)>,
    profile: String,
    abi: String,
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
        })
    }
}

struct ReportSummary {
    text: String,
    faces: Vec<String>,
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
    let text = format!(
        "{} ({} {status} export{}, level {})",
        artifact.display(),
        count,
        if count == 1 { "" } else { "s" },
        level,
    );
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
    ReportSummary { text, faces, count }
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
    // Artifact acceptance reasons about a valid WebAssembly module. The Lean
    // wall decodes every trust-bearing section and instruction, but it is not
    // yet a complete Wasm validation algorithm (stack/control typing included).
    // Keep this one standard validator gate; none of the producer classifier or
    // obligation reconstruction is linked into the verifier.
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .map_err(|error| format!("artifact is not valid WebAssembly: {error}"))?;
    let actual_hash = sha256_hex(&bytes);
    let manifest = read_manifest(cert_dir)?;

    let schema_version = manifest_u64(&manifest, "schema_version")?;
    if schema_version != format::CERT_SCHEMA_VERSION as u64 {
        return Err(format!(
            "unsupported certificate schema_version {schema_version}; this checker accepts {}",
            format::CERT_SCHEMA_VERSION
        ));
    }
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

    let candidates = read_candidates(&manifest)?;
    let lean = LeanRunner::new(selected_wall.toolchain)?;
    let build = assemble_build(cert_dir, &bytes, selected_wall, lean.memory_limit_mb())?;
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
            manifest_face: manifest_face(candidate),
        })
        .collect();
    Ok(TrustedReport {
        exports,
        contracts: candidates.contracts,
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

fn manifest_face(candidate: &CertifiedCandidate) -> String {
    format!(
        "manifest face (declared, not kernel-pinned): Dom {}, Cod {}",
        display_safe(&candidate.dom),
        display_safe(&candidate.cod)
    )
}

fn checker_witness(sha: &str, candidates: &Candidates) -> String {
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
    let allowed = AXIOM_WHITELIST
        .iter()
        .map(|name| format!("`{name}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "-- Authored by aver-cert; never accepted from the certificate.\n\
         import Lean\n\
         import AcceptedArtifact\n\
         import ArtifactBytes\n\
         import Manifest\n\
         import Artifact\n\n\
         import ArtifactCertificate\n\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCertChecker\n\n\
         example : AverCert.Artifact.data.modBytes = AverCert.ArtifactBytes.modBytes := rfl\n\
         example : AverCert.Artifact.data.modLen = AverCert.ArtifactBytes.modLen := rfl\n\
         example : AverCert.Artifact.data.manifest = AverCert.manifest := rfl\n\n\
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
               throwError s!\"non-whitelisted axiom: {{usedAxiom}}\"\n",
        format::ARTIFACT_CERTIFICATE_ROOT,
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

fn read_candidates(manifest: &Value) -> Result<Candidates, String> {
    let profile = manifest_str(manifest, "profile")?.to_string();
    let abi = manifest_str(manifest, "abi")?.to_string();
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
        });
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
        contracts,
        declared_uncertified,
        capabilities,
        start,
        host_role_table,
        string_host_roles,
        profile,
        abi,
    };
    gate_candidates(&candidates)?;
    Ok(candidates)
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
            "ArtifactBytes.lean" | "lakefile.lean" | "CheckerWitness.lean"
        )
}

fn assemble_build(
    cert_dir: &Path,
    wasm_bytes: &[u8],
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
        wall::render_artifact_bytes(wasm_bytes),
    )
    .map_err(|error| format!("cannot stage ArtifactBytes.lean: {error}"))?;
    roots.push("ArtifactBytes".to_string());
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
        }) || ["ArtifactBytes", "CheckerWitness", "lakefile"]
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
            if (' '..='~').contains(&character) {
                character
            } else {
                '?'
            }
        })
        .collect()
}

pub fn explain(artifact: &Path, cert_dir: &Path) -> Result<Explanation, String> {
    let report = trusted_check(artifact, cert_dir, ReplayMode::Fresh)?;
    println!("{}", "Artifact certificate".bold());
    println!("  artifact: {}", artifact.display());
    println!("  pinned sha256: {}", report.artifact_hash);
    println!("  profile: {}    abi: {}", report.profile, report.abi);
    if report.exports.is_empty() {
        println!("\n{}", "NO CERTIFIED EXPORTS".yellow().bold());
        return Ok(Explanation::NoExports);
    }
    println!("\n{}", "CERTIFIED".green().bold());
    for export in report.exports {
        println!("  {}", export.name.bold());
        println!("    policy: {}", export.policy);
        println!("    {}", export.face);
        println!("    {}", export.manifest_face);
    }
    if !report.contracts.is_empty() {
        println!("\n{}", "Runtime contracts".yellow().bold());
        for contract in report.contracts {
            println!("  - {contract}");
        }
    }

    let manifest = read_manifest(cert_dir)?;
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
        };
        assert_eq!(report_face(&candidate), "class: expression fragment");
        assert_eq!(
            manifest_face(&candidate),
            "manifest face (declared, not kernel-pinned): Dom List Int, Cod Int"
        );
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
        assert!(reject_shadowed_root("CheckerWitness.X.Y", wall).is_err());
        // A reserved name in non-prefix position does not shadow the import.
        assert!(reject_shadowed_root("Apps.Schema", wall).is_ok());
        assert!(reject_shadowed_root("Apps.Notepad.Store", wall).is_ok());
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
