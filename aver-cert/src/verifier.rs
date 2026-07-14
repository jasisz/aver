//! Minimal consumer for Aver artifact certificates.
//!
//! Rust owns transport: it selects the embedded wall, stages untrusted DATA,
//! injects the exact artifact bytes, and runs Lean. It does not disassemble the
//! module or reconstruct an obligation. The checker-owned Lean predicate binds
//! every accepted claim to the bytes and derives its standard face, policy,
//! termination witness, host table, and runtime contracts.

use crate::cache::{ArtifactBuildCache, KeyMaterial as ArtifactCacheKeyMaterial};
use crate::prelude_cache::PristineWallCache;
use crate::{format, wall};
use colored::Colorize;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use std::process::Command;

const AXIOM_WHITELIST: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];
const CHECKED_ROOT: &str = "AverCertChecker.checked";
const MAX_CANDIDATE_LEN: usize = 200;
const TOOLCHAIN_ROOTS: [&str; 4] = ["Init", "Lake", "Lean", "Std"];

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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Explanation {
    Certified,
    NoExports,
}

struct CertifiedExport {
    name: String,
    policy: String,
    face: String,
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

struct Candidates {
    certified: Vec<CertifiedCandidate>,
    contracts: Vec<String>,
    declared_uncertified: Vec<(String, String)>,
    capabilities: Vec<(String, String)>,
    start: Option<u32>,
    host_role_table: (Option<u32>, Option<u32>, Option<u32>, Option<u32>),
    string_host_roles: Vec<(u32, StringHostRole)>,
    profile: String,
    abi: String,
}

pub fn verify(artifact: &Path, cert_dir: &Path) -> Result<Verdict, String> {
    let report = trusted_check(artifact, cert_dir)?;
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
    let summary = format!(
        "{} ({} certified export{}, level {})",
        artifact.display(),
        count,
        if count == 1 { "" } else { "s" },
        level,
    );
    if count == 0 {
        Ok(Verdict::NoExports(summary))
    } else {
        Ok(Verdict::Certified {
            summary,
            faces: report
                .exports
                .into_iter()
                .map(|export| {
                    format!(
                        "{}  policy: {}  {}",
                        export.name, export.policy, export.face
                    )
                })
                .collect(),
        })
    }
}

fn trusted_check(artifact: &Path, cert_dir: &Path) -> Result<TrustedReport, String> {
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
    let build = assemble_build(cert_dir, &bytes, selected_wall)?;
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
        PristineWallCache::prepare(&build.path, selected_wall)
    };

    let mut data_build = run_lake(&build.path, &["build"])?;
    if !data_build.status.success() && (data_cache_hit || wall_cache.was_seeded()) {
        if data_cache_hit {
            cache.invalidate(&build.path);
        } else {
            wall_cache.clear_build(&build.path);
        }
        data_build = run_lake(&build.path, &["build"])?;
        if data_build.status.success() && wall_cache.was_seeded() {
            wall_cache.evict();
        }
    }
    if !data_build.status.success() {
        return Err(format!(
            "certificate data did not build:\n{}",
            tail(&data_build.combined, 30)
        ));
    }
    cache.publish(&build.path);

    let witness = checker_witness(&actual_hash, &candidates);
    std::fs::write(build.path.join("CheckerWitness.lean"), witness)
        .map_err(|error| format!("cannot write checker witness: {error}"))?;
    let elaborated = run_lake(
        &build.path,
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
    let replayed = run_lake(
        &build.path,
        &["env", "leanchecker", "--fresh", "CheckerWitness"],
    )?;
    if !replayed.status.success() {
        return Err(format!(
            "certificate failed fresh-environment kernel replay:\n{}",
            tail(&replayed.combined, 30)
        ));
    }

    let exports = candidates
        .certified
        .iter()
        .map(|candidate| CertifiedExport {
            name: candidate.name.clone(),
            policy: candidate.policy.clone(),
            face: report_face(candidate),
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
    format!(
        "class: {label}  |  source types (display only): {} -> {}",
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
    let roles = format!(
        "({{ box := {}, add := {}, mul := {}, sub := {} }} : CertDecode.AddSub.Roles)",
        lean_option_nat(candidates.host_role_table.0),
        lean_option_nat(candidates.host_role_table.1),
        lean_option_nat(candidates.host_role_table.2),
        lean_option_nat(candidates.host_role_table.3),
    );
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
    exact_object_fields(host_roles, "hostRoleTable", &["box", "add", "mul", "sub"])?;
    let optional_index = |key: &str| -> Result<Option<u32>, String> {
        match &host_roles[key] {
            Value::Null => Ok(None),
            value => Ok(Some(value_u32(value, &format!("hostRoleTable.{key}"))?)),
        }
    };
    let host_role_table = (
        optional_index("box")?,
        optional_index("add")?,
        optional_index("mul")?,
        optional_index("sub")?,
    );

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
) -> Result<BuildDir, String> {
    let build = BuildDir::new()?;
    let mut roots = Vec::new();
    let entries = std::fs::read_dir(cert_dir)
        .map_err(|error| format!("cannot read cert dir {}: {error}", cert_dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|error| format!("cert dir read: {error}"))?;
        if !entry
            .file_type()
            .map(|kind| kind.is_file())
            .unwrap_or(false)
        {
            continue;
        }
        let name = entry.file_name().to_string_lossy().into_owned();
        if !name.ends_with(".lean") || is_checker_owned(&name, selected_wall) {
            continue;
        }
        let root = lean_module_root(&name)?;
        let shadows_toolchain = TOOLCHAIN_ROOTS
            .iter()
            .any(|reserved| reserved.eq_ignore_ascii_case(&root));
        let shadows_checker = selected_wall.sources.iter().any(|source| {
            source
                .name
                .strip_suffix(".lean")
                .is_some_and(|reserved| reserved.eq_ignore_ascii_case(&root))
        }) || ["ArtifactBytes", "CheckerWitness", "lakefile"]
            .iter()
            .any(|reserved| reserved.eq_ignore_ascii_case(&root));
        if shadows_toolchain || shadows_checker {
            return Err(format!(
                "cert data module `{root}` shadows a checker/toolchain import"
            ));
        }
        let contents = std::fs::read(entry.path())
            .map_err(|error| format!("cannot read cert file {name}: {error}"))?;
        scan_for_code_exec(&name, &contents)?;
        std::fs::write(build.path.join(&name), contents)
            .map_err(|error| format!("cannot stage {name}: {error}"))?;
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
    std::fs::write(build.path.join("lakefile.lean"), checker_lakefile(&roots))
        .map_err(|error| format!("cannot write checker lakefile: {error}"))?;
    std::fs::write(build.path.join("lean-toolchain"), selected_wall.toolchain)
        .map_err(|error| format!("cannot write lean-toolchain: {error}"))?;
    Ok(build)
}

fn lean_module_root(name: &str) -> Result<String, String> {
    let stem = name
        .strip_suffix(".lean")
        .ok_or_else(|| format!("cert file `{name}` is not a Lean file"))?;
    let mut chars = stem.chars();
    let valid = matches!(chars.next(), Some(first) if first.is_ascii_alphabetic())
        && chars.all(|character| character.is_ascii_alphanumeric() || character == '_');
    if stem.is_empty() || !valid {
        Err(format!(
            "cert file name `{name}` must match ^[A-Za-z][A-Za-z0-9_]*\\.lean$"
        ))
    } else {
        Ok(stem.to_string())
    }
}

fn scan_for_code_exec(name: &str, contents: &[u8]) -> Result<(), String> {
    let text = String::from_utf8_lossy(contents);
    for token in CODE_EXEC_TOKENS {
        if text.contains(token) {
            return Err(format!(
                "cert data file `{name}` contains elaboration-executing token `{token}`"
            ));
        }
    }
    Ok(())
}

fn checker_lakefile(roots: &[String]) -> String {
    let roots = roots
        .iter()
        .map(|root| format!("`{root}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n@[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{roots}]\n"
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
        let path = std::env::temp_dir().join(format!(
            "aver-cert-check-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        std::fs::create_dir(&path)
            .map_err(|error| format!("cannot create checker build dir: {error}"))?;
        Ok(Self { path })
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

fn run_lake(build_dir: &Path, arguments: &[&str]) -> Result<LakeOut, String> {
    let output = Command::new("lake")
        .current_dir(build_dir)
        .args(arguments)
        .output()
        .map_err(|error| {
            format!(
                "could not run `lake {}`: {error} (is Lean/lake installed?)",
                arguments.join(" ")
            )
        })?;
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
    let report = trusted_check(artifact, cert_dir)?;
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
    fn module_names_are_plain() {
        assert_eq!(lean_module_root("Artifact.lean").unwrap(), "Artifact");
        assert!(lean_module_root("../Artifact.lean").is_err());
        assert!(lean_module_root("A, `Injected.lean").is_err());
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
}
