//! `aver cert verify|explain` — the consumer side of `aver compile --certify`.
//!
//! `verify` is a fail-closed checker. The Lean/proof verdict comes only from
//! the exit code of the Lean toolchain over files the checker itself authored;
//! byte/plan prechecks are explicit Rust-side TCB. It assembles its OWN build
//! in a fresh, checker-owned temp directory from the audited
//! `SchemaCore.lean` / `Schema.lean` / `PlanCheck.lean` / `PlanLower.lean` /
//! `PlanBytes.lean` / `WasmSlice.lean` / `ExprFragmentAccepted.lean` /
//! `AcceptedArtifactCore.lean` / `AcceptedArtifact.lean` / `CertPrelude.lean`
//! this binary embeds, regenerates
//! `ArtifactBytes.lean` from the artifact bytes it read, copies the cert's
//! DATA-only Lean files, and authors its own `lakefile.lean`. Artifact-specific
//! Lake outputs may come from the content-addressed user cache, but sources are
//! always staged afresh and `lake build` still runs. It then writes a
//! `CheckerWitness.lean` — which the checker, not the cert, authors — that:
//!   * binds the sha256 the checker computed from the artifact bytes to the
//!     hashes the kernel-checked theorems talk about (`rfl`);
//!   * binds the certified-export names, contracts, profile and ABI the
//!     UNTRUSTED `cert-manifest.json` claims to the `AverCert.manifest` literal
//!     the final theorem is about (`rfl`) — a lying JSON makes a `rfl` fail;
//!   * forces the final theorem's TYPE to `Holds manifest` by ascription;
//!   * runs the kernel's own axiom collector (`Lean.collectAxioms`) on the
//!     artifact-carried acceptance root and throws unless every axiom is on the whitelist
//!     (full `Name` equality, not text) — a smuggled `axiom`, `sorryAx` or
//!     `ofReduceBool` makes the file fail to check.
//!
//! NO byte of lake/lean stdout/stderr is ever parsed into a verdict or into a
//! CERTIFIED report line: the verdict is the witness process exit code, and the
//! report is built in Rust from byte-derived exports/contracts after the kernel
//! binds the manifest to them. (stdout is shown to a human inside error messages
//! only — a display channel, never a trust channel.) `explain` renders that
//! same trusted report.
//!
//! The bytes-vs-data divergence is closed INSIDE that same kernel witness. A
//! bare `Holds manifest` proof only says "some Lean-encoded body simulates the
//! model and the bytes hash to S"; it does NOT say that body DECODES from those
//! bytes, so a hostile producer could ship `WInstr` data unrelated to the real
//! bytes with a vacuously-true `holds`. To close that, non-expression
//! `code`/`carrier` and consumed struct counts come from the audited in-kernel
//! `CertDecode`; its module-wide add/sub/box table and complete ordered
//! String.eq/String.concat role list are likewise decoded in-kernel.
//! `expr-fragment-v1` code/face comes from a checked source plan sidecar
//! or, only for representation-only fallbacks, a checked representation plan
//! sidecar whose canonical code-entry bytes must equal the real bytes. The
//! accepted-artifact decoder equalities bind code/carrier/struct/host facts to
//! the manifest obligations; neither host nor self is Rust-spliced.
//! Those are EXACTLY the fields `Obligation.holds` reasons about
//! (`wFuncN o.code (o.host add sub mul stringEq stringConcat) fuel o.self`), so a fabricated body, a
//! decoupled `code`/`self`/`carrier`, or a nerfed `host` (which would make
//! `holds` vacuous) all diverge from the bytes and fail a `rfl` — the file does
//! not check and verify declines.
//! Expression-fragment sidecars are useful emitted metadata, never authority.
//! The preferred sidecar is now a source-level `SymPlan`; the checker parses it
//! as untrusted data, typechecks it against byte-derived function facts, derives
//! the representation `ExprFragmentRawPlan`, canonically lowers that derived
//! plan to raw wasm code-entry bytes, and only then uses it to render the
//! witness `code`/semantic face for that obligation. A stale or forged sidecar
//! therefore fails before the checker-authored witness can certify it. The
//! witness also asks `WasmSlice.lean` to recover each expression export's
//! code-entry bytes from the checker-regenerated `ArtifactBytes.lean`, so the
//! plan's canonical bytes are tied to a narrow Lean-side slice of the actual
//! module bytes.
//! `Module.lean` is never read as text for comparison. Rust rederivation and
//! `validate_all` remain non-trust-bearing fail-fast checks; model/domRepr stay
//! explicit read declarations.
//!
//! Two input gates run before anything is elaborated:
//!   * A cert data file whose name is not a plain `Foo.lean` Lean-module
//!     identifier is DECLINED — otherwise a name with a comma/space/newline
//!     could inject tokens into the checker-authored lakefile's `roots` array.
//!   * Each cert data file's raw text is scanned for a blacklist of tokens that
//!     make Lean elaboration run arbitrary code (`#eval`, `run_cmd`, `macro`,
//!     `elab`, `initialize`, `unsafe`, `implemented_by`, `extern`, ...). This
//!     is a DELIBERATELY BRITTLE wall, not a proof: because `lake build`
//!     ELABORATES these files, a data file can in principle run code (including
//!     overwriting an already-built `.olean` the kernel will not re-check).
//!     Fully closing that class needs a verified checker / a bytes-to-data
//!     decoder (residual C2); the token scan only raises the bar. An attacker
//!     who finds a bypass of THIS scan has found a documented residual, not a
//!     break of the hash / count / axiom bindings above.

use std::path::{Path, PathBuf};
use std::process::Command;

use aver::codegen::cert;
use colored::Colorize;
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::cert_data_cache::{ArtifactBuildCache, KeyMaterial as ArtifactCacheKeyMaterial};

/// Kernel axioms a certificate is allowed to depend on. Anything else — most
/// importantly `sorryAx` (an admitted goal) or `ofReduceBool` (native-code
/// trust) — fails the check. Spliced into the witness as `Name` literals and
/// compared by full-name equality by `Lean.collectAxioms`.
const AXIOM_WHITELIST: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];

/// Constants the checker composes in its own witness file: `final` ascribes
/// `AverCert.Final.cert` to `Holds manifest`, then `accepted` aliases the
/// artifact-carried acceptance root used for the axiom audit.
const FINAL_WITNESS_THEOREM: &str = "AverCertChecker.final";
const WITNESS_THEOREM: &str = "AverCertChecker.accepted";

/// The normal verifier authors only the trust-bearing witness. If that witness
/// declines, verification automatically authors the diagnostic superset, whose
/// two expensive mirror proofs localize the failing accepted-artifact conjunct.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum WitnessMode {
    Fast,
    Diagnostic,
}

/// Lean source files the checker owns and never copies from the cert: the
/// audited trusted computing base (taken from this binary) plus the checker's
/// own build config and witness. A cert shipping files by these names has them
/// ignored.
const CHECKER_OWNED: [&str; 14] = [
    "Schema.lean",
    "SchemaCore.lean",
    "PlanCheck.lean",
    "PlanLower.lean",
    "PlanBytes.lean",
    "WasmSlice.lean",
    "ExprFragmentAccepted.lean",
    "AcceptedArtifact.lean",
    "AcceptedArtifactCore.lean",
    "ArtifactBytes.lean",
    "CertDecode.lean",
    "CertPrelude.lean",
    "lakefile.lean",
    "CheckerWitness.lean",
];

/// Audited modules whose exact bytes are shared by every certificate build.
/// ArtifactBytes and certificate/model modules are deliberately absent.
const STATIC_PRELUDE_ROOTS: [&str; 11] = [
    "SchemaCore",
    "Schema",
    "PlanCheck",
    "PlanLower",
    "PlanBytes",
    "WasmSlice",
    "ExprFragmentAccepted",
    "AcceptedArtifactCore",
    "AcceptedArtifact",
    "CertDecode",
    "CertPrelude",
];

/// Static roots whose complete import graph is artifact-independent.
const PRISTINE_PRELUDE_ROOTS: [&str; 9] = [
    "CertPrelude",
    "CertDecode",
    "WasmSlice",
    "SchemaCore",
    "PlanCheck",
    "PlanLower",
    "PlanBytes",
    "ExprFragmentAccepted",
    "AcceptedArtifactCore",
];

/// Maximum length (bytes) of a JSON-supplied string spliced into the witness.
const MAX_CANDIDATE_LEN: usize = 200;

/// Emitted on a CERTIFIED verdict: non-expression code/carrier/struct facts
/// were decoded from ArtifactBytes in-kernel, including the module-wide
/// add/sub/box table and all string roles; expression code comes from its
/// checked plan and canonical code-entry equality.
const ARTIFACT_DECODE_LINE: &str = "artifact-decode: every byte fact is kernel-computed from ArtifactBytes, including code/carrier/struct facts and all host roles; expression code uses checked-plan canonical byte equality";

/// Tokens that make Lean ELABORATION execute code. The scan is a substring
/// match on raw cert data-file text (see the module doc: a deliberately brittle
/// wall, not a proof). Kept as literals rather than a parser so the wall is
/// obvious and auditable.
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

/// Outcome of a successful (fail-closed) verify pass.
enum Verdict {
    /// At least one export carries a behavioral certificate. `faces` are the
    /// one-line per-export claim summaries (class + Dom/Cod + codRepr form).
    Certified { summary: String, faces: Vec<String> },
    /// The certificate built and stayed kernel-clean, but names zero certified
    /// exports — an admission with no behavioral claims. Not the green path.
    NoExports(String),
}

/// One proven obligation in the trusted report.
struct CertifiedExport {
    name: String,
    policy: String,
    /// One-line, human-readable statement of WHAT is certified: the trusted
    /// (byte-derived) class plus its `Dom`/`Cod` and standard `codRepr` form, so
    /// a consumer sees the claim without reading Lean.
    face: String,
}

/// The certified side of the report, built from byte-derived exports/contracts
/// after the kernel witness confirmed the proven `AverCert.manifest` literal
/// binds to those bytes.
struct TrustedReport {
    /// One entry per proven obligation.
    exports: Vec<CertifiedExport>,
    contracts: Vec<String>,
    profile: String,
    abi: String,
    artifact_hash: String,
}

/// Untrusted strings pulled from `cert-manifest.json`, each already passed
/// through the charset gate on its serde-DECODED value. They are candidates:
/// only the kernel witness (via `rfl` against `AverCert.manifest`) makes them
/// trustworthy.
struct Candidates {
    /// Certified export names as CLAIMED by the JSON. Used only for the count
    /// binding (`obligations.length`, verified by `rfl`); the export NAMES the
    /// obligations are pinned to come from the bytes, not from here.
    names: Vec<String>,
    /// Policy/witness claim axis decoded from JSON. The checker separately
    /// pins these candidates to the Lean manifest and to byte re-derivation.
    policies: Vec<cert::CertificationPolicy>,
    termination_witnesses: Vec<Option<cert::TerminationWitness>>,
    /// Runtime contracts as CLAIMED by the JSON. Used only for the witness
    /// binding against the proven manifest; the final byte-binding and report
    /// use the byte-derived list.
    contracts: Vec<String>,
    declared_uncertified: Vec<(String, String)>,
    capabilities: Vec<(String, String)>,
    start: Option<u32>,
    host_role_table: (Option<u32>, Option<u32>, Option<u32>),
    string_host_roles: cert::StringHostRoles,
    profile: String,
    abi: String,
}

pub(super) fn cmd_cert_verify(artifact: &str, cert_dir: &str) {
    match verify(Path::new(artifact), Path::new(cert_dir)) {
        Ok(Verdict::Certified { summary, faces }) => {
            println!("{} {}", "CERTIFIED".green().bold(), summary);
            println!("  {ARTIFACT_DECODE_LINE}");
            for f in &faces {
                println!("  {f}");
            }
        }
        Ok(Verdict::NoExports(summary)) => {
            // Fail-closed: a trust tool must not exit green for a cert that
            // makes no behavioral claims.
            eprintln!(
                "{} {}",
                "NO CERTIFIED EXPORTS (admission only, no behavioral claims)"
                    .yellow()
                    .bold(),
                summary
            );
            std::process::exit(1);
        }
        Err(reason) => {
            eprintln!("{} {}", "DECLINED".red().bold(), reason);
            std::process::exit(1);
        }
    }
}

pub(super) fn cmd_cert_explain(artifact: &str, cert_dir: &str) {
    if let Err(e) = explain(Path::new(artifact), Path::new(cert_dir)) {
        eprintln!("{} {}", "error:".red(), e);
        std::process::exit(1);
    }
}

/// Read + parse `cert-manifest.json` from the cert directory.
/// Read every `*.lean` file in the cert dir as `(name, content)` pairs. Used only
/// to extract the model recursion operator (`+`/`*`); the content is untrusted and
/// never executed.
fn read_lean_files(cert_dir: &Path) -> Vec<(String, String)> {
    // Checker-owned audited files are staged into the cert dir but are kernel
    // infrastructure, never source models; feeding them to the line-level model
    // parser lets an audited-file type name (e.g. the decoder's `Op`) shadow a
    // same-named model inductive, with the winner decided by filesystem
    // iteration order. Exclude them, and sort what remains so extraction is
    // deterministic across platforms.
    const CHECKER_OWNED: &[&str] = &[
        "AcceptedArtifact.lean",
        "AcceptedArtifactCore.lean",
        "CertPrelude.lean",
        "CertDecode.lean",
        "ExprFragmentAccepted.lean",
        "PlanBytes.lean",
        "PlanCheck.lean",
        "PlanLower.lean",
        "Schema.lean",
        "SchemaCore.lean",
        "WasmSlice.lean",
        "lakefile.lean",
    ];
    let mut out = Vec::new();
    if let Ok(entries) = std::fs::read_dir(cert_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("lean") {
                let name = path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string();
                if CHECKER_OWNED.contains(&name.as_str()) {
                    continue;
                }
                if let Ok(content) = std::fs::read_to_string(&path) {
                    out.push((name, content));
                }
            }
        }
    }
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out
}

fn read_manifest(cert_dir: &Path) -> Result<Value, String> {
    let path = cert_dir.join("cert-manifest.json");
    let text = std::fs::read_to_string(&path)
        .map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    serde_json::from_str(&text).map_err(|e| format!("cert-manifest.json is not valid JSON: {e}"))
}

fn manifest_str<'a>(m: &'a Value, key: &str) -> Result<&'a str, String> {
    m.get(key)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("cert-manifest.json is missing string field `{key}`"))
}

fn manifest_u64(m: &Value, key: &str) -> Result<u64, String> {
    m.get(key)
        .and_then(Value::as_u64)
        .ok_or_else(|| format!("cert-manifest.json is missing integer field `{key}`"))
}

fn manifest_optional_u32(value: &Value, context: &str) -> Result<Option<u32>, String> {
    match value {
        Value::Null => Ok(None),
        value => value
            .as_u64()
            .and_then(|index| u32::try_from(index).ok())
            .map(Some)
            .ok_or_else(|| format!("cert-manifest.json `{context}` must be a u32 or null")),
    }
}

/// True iff `s`, a serde-DECODED value, is printable ASCII with no `"` or `\`
/// and at most `MAX_CANDIDATE_LEN` bytes. Splicing only such values into the
/// Lean witness guarantees the spliced literal cannot break out of its string
/// or inject a line (newlines and every other control char are rejected).
fn charset_ok(s: &str) -> bool {
    s.len() <= MAX_CANDIDATE_LEN
        && s.bytes()
            .all(|b| (0x20..=0x7e).contains(&b) && b != b'"' && b != b'\\')
}

fn gate_candidate(kind: &str, s: &str) -> Result<(), String> {
    if charset_ok(s) {
        Ok(())
    } else {
        Err(format!(
            "certificate {kind} contains a value outside the allowed charset \
             (printable ASCII, no quote or backslash, at most {MAX_CANDIDATE_LEN} bytes): {s:?}"
        ))
    }
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

/// Pull the report candidates from the untrusted JSON and charset-gate each one
/// on its decoded value. Nothing here is trusted yet — the witness `rfl`s below
/// are what bind these to the kernel-proven manifest.
fn read_candidates(m: &Value) -> Result<Candidates, String> {
    let profile = manifest_str(m, "profile")?.to_string();
    let abi = manifest_str(m, "abi")?.to_string();

    // The JSON export names are read ONLY for the count binding
    // (`obligations.length`, verified by `rfl`). The export NAMES the
    // obligations are actually pinned to are re-derived from the module's
    // export section (see the witness), so a producer cannot relabel a
    // byte-bound body under a different export name.
    let certified = m
        .get("certified")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `certified`".to_string())?;
    let mut names = Vec::with_capacity(certified.len());
    let mut policies = Vec::with_capacity(certified.len());
    let mut termination_witnesses = Vec::with_capacity(certified.len());
    for c in certified {
        let name = c
            .get("name")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                "cert-manifest.json `certified[]` entry is missing string field `name`".to_string()
            })?
            .to_string();
        let policy = match c.get("policy").and_then(Value::as_str) {
            Some("simulatesModel") => cert::CertificationPolicy::SimulatesModel,
            Some("simulatesModelTotally") => cert::CertificationPolicy::SimulatesModelTotally,
            Some(other) => {
                return Err(format!(
                    "cert-manifest.json certified export `{name}` uses unsupported policy `{other}`"
                ));
            }
            None => {
                return Err(format!(
                    "cert-manifest.json certified export `{name}` is missing string field `policy`"
                ));
            }
        };
        let termination_witness = match c.get("termination_witness") {
            None => None,
            Some(witness) => {
                let measure = witness
                    .get("measure")
                    .and_then(Value::as_object)
                    .ok_or_else(|| {
                        format!(
                            "cert-manifest.json certified export `{name}` has malformed termination witness measure"
                        )
                    })?;
                let kind = measure.get("kind").and_then(Value::as_str).ok_or_else(|| {
                    format!(
                        "cert-manifest.json certified export `{name}` has no termination measure kind"
                    )
                })?;
                if kind != "intNatAbs" {
                    return Err(format!(
                        "cert-manifest.json certified export `{name}` uses unsupported termination measure `{kind}`; schema 52 admits only `intNatAbs`"
                    ));
                }
                let param_idx = measure
                    .get("param_index")
                    .and_then(Value::as_u64)
                    .and_then(|n| u32::try_from(n).ok())
                    .ok_or_else(|| {
                        format!(
                            "cert-manifest.json certified export `{name}` has invalid intNatAbs parameter index"
                        )
                    })?;
                let descent = witness
                    .get("descent")
                    .and_then(Value::as_i64)
                    .ok_or_else(|| {
                        format!(
                            "cert-manifest.json certified export `{name}` has invalid termination descent"
                        )
                    })?;
                Some(cert::TerminationWitness {
                    measure: cert::TerminationMeasure::IntNatAbs { param_idx },
                    descent,
                })
            }
        };
        match (policy, termination_witness) {
            (cert::CertificationPolicy::SimulatesModel, None)
            | (cert::CertificationPolicy::SimulatesModelTotally, Some(_)) => {}
            (cert::CertificationPolicy::SimulatesModel, Some(_)) => {
                return Err(format!(
                    "cert-manifest.json partial export `{name}` must not carry a termination witness"
                ));
            }
            (cert::CertificationPolicy::SimulatesModelTotally, None) => {
                return Err(format!(
                    "cert-manifest.json total export `{name}` is missing `termination_witness`"
                ));
            }
        }
        names.push(name);
        policies.push(policy);
        termination_witnesses.push(termination_witness);
    }

    let contracts = m
        .get("runtime_contracts")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `runtime_contracts`".to_string())?
        .iter()
        .map(|c| {
            c.as_str().map(str::to_string).ok_or_else(|| {
                "cert-manifest.json `runtime_contracts[]` entry is not a string".to_string()
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let declared_uncertified = m
        .get("declaredUncertified")
        .and_then(Value::as_array)
        .ok_or_else(|| {
            "cert-manifest.json is missing array field `declaredUncertified`".to_string()
        })?
        .iter()
        .enumerate()
        .map(|(index, entry)| {
            exact_object_fields(
                entry,
                &format!("declaredUncertified[{index}]"),
                &["name", "reason"],
            )?;
            let name = entry.get("name").and_then(Value::as_str).ok_or_else(|| {
                format!("cert-manifest.json `declaredUncertified[{index}].name` is not a string")
            })?;
            let reason = entry.get("reason").and_then(Value::as_str).ok_or_else(|| {
                format!("cert-manifest.json `declaredUncertified[{index}].reason` is not a string")
            })?;
            Ok((name.to_string(), reason.to_string()))
        })
        .collect::<Result<Vec<_>, String>>()?;
    let capabilities = m
        .get("capabilities")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `capabilities`".to_string())?
        .iter()
        .enumerate()
        .map(|(index, entry)| {
            exact_object_fields(
                entry,
                &format!("capabilities[{index}]"),
                &["module", "name"],
            )?;
            let module = entry.get("module").and_then(Value::as_str).ok_or_else(|| {
                format!("cert-manifest.json `capabilities[{index}].module` is not a string")
            })?;
            let name = entry.get("name").and_then(Value::as_str).ok_or_else(|| {
                format!("cert-manifest.json `capabilities[{index}].name` is not a string")
            })?;
            Ok((module.to_string(), name.to_string()))
        })
        .collect::<Result<Vec<_>, String>>()?;
    let start_value = m
        .get("start")
        .ok_or_else(|| "cert-manifest.json is missing object field `start`".to_string())?;
    exact_object_fields(start_value, "start", &["present", "function_index"])?;
    let present = start_value
        .get("present")
        .and_then(Value::as_bool)
        .ok_or_else(|| "cert-manifest.json `start.present` is not a boolean".to_string())?;
    let start = match (present, start_value.get("function_index")) {
        (false, Some(Value::Null)) => None,
        (true, Some(value)) => value
            .as_u64()
            .and_then(|index| u32::try_from(index).ok())
            .map(Some)
            .ok_or_else(|| {
                "cert-manifest.json present `start.function_index` is not a u32".to_string()
            })?,
        (false, _) => {
            return Err(
                "cert-manifest.json absent start must use null `function_index`".to_string(),
            );
        }
        (true, None) => unreachable!("exact_object_fields checked function_index"),
    };
    let host_role_table_value = m
        .get("hostRoleTable")
        .ok_or_else(|| "cert-manifest.json is missing object field `hostRoleTable`".to_string())?;
    exact_object_fields(
        host_role_table_value,
        "hostRoleTable",
        &["box", "add", "sub"],
    )?;
    let host_role_table = (
        manifest_optional_u32(&host_role_table_value["box"], "hostRoleTable.box")?,
        manifest_optional_u32(&host_role_table_value["add"], "hostRoleTable.add")?,
        manifest_optional_u32(&host_role_table_value["sub"], "hostRoleTable.sub")?,
    );
    let string_host_roles = m
        .get("stringHostRoles")
        .and_then(Value::as_array)
        .ok_or_else(|| {
            "cert-manifest.json is missing array field `stringHostRoles`".to_string()
        })?
        .iter()
        .enumerate()
        .map(|(index, entry)| {
            exact_object_fields(
                entry,
                &format!("stringHostRoles[{index}]"),
                &["function_index", "role"],
            )?;
            let function_index = entry
                .get("function_index")
                .and_then(Value::as_u64)
                .and_then(|value| u32::try_from(value).ok())
                .ok_or_else(|| {
                    format!(
                        "cert-manifest.json `stringHostRoles[{index}].function_index` is not a u32"
                    )
                })?;
            let role = match entry.get("role").and_then(Value::as_str) {
                Some("stringEq") => cert::StringHostRole::Eq,
                Some("stringConcat") => cert::StringHostRole::Concat,
                _ => {
                    return Err(format!(
                        "cert-manifest.json `stringHostRoles[{index}].role` must be stringEq or stringConcat"
                    ));
                }
            };
            Ok((function_index, role))
        })
        .collect::<Result<Vec<_>, String>>()?;

    let cands = Candidates {
        names,
        policies,
        termination_witnesses,
        contracts,
        declared_uncertified,
        capabilities,
        start,
        host_role_table,
        string_host_roles,
        profile,
        abi,
    };
    for n in &cands.names {
        gate_candidate("certified export name", n)?;
    }
    for c in &cands.contracts {
        gate_candidate("runtime contract", c)?;
    }
    for (name, reason) in &cands.declared_uncertified {
        gate_candidate("declared-uncertified export name", name)?;
        gate_candidate("declared-uncertified reason", reason)?;
    }
    for (module, name) in &cands.capabilities {
        gate_candidate("capability module", module)?;
        gate_candidate("capability name", name)?;
    }
    gate_candidate("profile", &cands.profile)?;
    gate_candidate("abi", &cands.abi)?;
    Ok(cands)
}

fn verify(artifact: &Path, cert_dir: &Path) -> Result<Verdict, String> {
    let report = trusted_check(artifact, cert_dir)?;
    let n = report.exports.len();
    let has_total = report
        .exports
        .iter()
        .any(|e| e.policy == "simulatesModelTotally");
    let has_partial = report.exports.iter().any(|e| e.policy == "simulatesModel");
    let level = match (has_partial, has_total) {
        (true, true) => "mixed L1/L3",
        (false, true) => "L3",
        _ => "L1",
    };
    let summary = format!(
        "{} ({} certified export{}, level {})",
        artifact.display(),
        n,
        if n == 1 { "" } else { "s" },
        level,
    );
    if n == 0 {
        Ok(Verdict::NoExports(summary))
    } else {
        let faces = report
            .exports
            .iter()
            .map(|e| format!("{}  policy: {}  {}", e.name, e.policy, e.face))
            .collect();
        Ok(Verdict::Certified { summary, faces })
    }
}

/// The trusted core, shared by `verify` and `explain`: bind the artifact bytes
/// and the JSON report candidates to a checker-owned build of the cert's data,
/// prove the final theorem is `Holds manifest` and kernel-clean, and — only if
/// the witness checks — build the report from those now-confirmed candidates.
fn trusted_check(artifact: &Path, cert_dir: &Path) -> Result<TrustedReport, String> {
    // 1. Artifact identity (fast pre-check): the bytes must hash to the pinned
    //    value. This is a convenience tripwire on the (attacker-editable) JSON;
    //    the kernel witness below is what actually binds the hash.
    let bytes = std::fs::read(artifact)
        .map_err(|e| format!("cannot read artifact {}: {e}", artifact.display()))?;
    let actual = cert::sha256_hex(&bytes);
    let manifest = read_manifest(cert_dir)?;
    let schema_version = manifest_u64(&manifest, "schema_version")?;
    if schema_version != cert::CERT_SCHEMA_VERSION as u64 {
        return Err(format!(
            "unsupported certificate schema_version {schema_version}; this checker only accepts schema_version {}",
            cert::CERT_SCHEMA_VERSION
        ));
    }
    let pinned = manifest_str(&manifest, "wasm_sha256")?;
    if actual != pinned {
        return Err(format!(
            "artifact hash mismatch: {} hashes to {actual}, certificate pins {pinned}",
            artifact.display()
        ));
    }
    let schema_pin = manifest_str(&manifest, "schema_sha256")?;
    let audited_schema = cert::audited_schema_sha();
    if schema_pin != audited_schema {
        return Err(format!(
            "schema hash mismatch: certificate pins {schema_pin}, checker expects {audited_schema}"
        ));
    }
    let schema_core_pin = manifest_str(&manifest, "schema_core_sha256")?;
    let audited_schema_core = cert::audited_schema_core_sha();
    if schema_core_pin != audited_schema_core {
        return Err(format!(
            "schema-core hash mismatch: certificate pins {schema_core_pin}, checker expects {audited_schema_core}"
        ));
    }
    let prelude_pin = manifest_str(&manifest, "prelude_sha256")?;
    let audited_prelude = cert::audited_prelude_sha();
    if prelude_pin != audited_prelude {
        return Err(format!(
            "prelude hash mismatch: certificate pins {prelude_pin}, checker expects {audited_prelude}"
        ));
    }
    let decode_pin = manifest_str(&manifest, "cert_decode_sha256")?;
    let audited_decode = cert::audited_decode_sha();
    if decode_pin != audited_decode {
        return Err(format!(
            "cert-decode hash mismatch: certificate pins {decode_pin}, checker expects {audited_decode}"
        ));
    }
    let plan_check_pin = manifest_str(&manifest, "plan_check_sha256")?;
    let audited_plan_check = cert::audited_plan_check_sha();
    if plan_check_pin != audited_plan_check {
        return Err(format!(
            "plan-check hash mismatch: certificate pins {plan_check_pin}, checker expects {audited_plan_check}"
        ));
    }
    let plan_lower_pin = manifest_str(&manifest, "plan_lower_sha256")?;
    let audited_plan_lower = cert::audited_plan_lower_sha();
    if plan_lower_pin != audited_plan_lower {
        return Err(format!(
            "plan-lower hash mismatch: certificate pins {plan_lower_pin}, checker expects {audited_plan_lower}"
        ));
    }
    let plan_bytes_pin = manifest_str(&manifest, "plan_bytes_sha256")?;
    let audited_plan_bytes = cert::audited_plan_bytes_sha();
    if plan_bytes_pin != audited_plan_bytes {
        return Err(format!(
            "plan-bytes hash mismatch: certificate pins {plan_bytes_pin}, checker expects {audited_plan_bytes}"
        ));
    }
    let wasm_slice_pin = manifest_str(&manifest, "wasm_slice_sha256")?;
    let audited_wasm_slice = cert::audited_wasm_slice_sha();
    if wasm_slice_pin != audited_wasm_slice {
        return Err(format!(
            "wasm-slice hash mismatch: certificate pins {wasm_slice_pin}, checker expects {audited_wasm_slice}"
        ));
    }
    let expr_fragment_accepted_pin = manifest_str(&manifest, "expr_fragment_accepted_sha256")?;
    let audited_expr_fragment_accepted = cert::audited_expr_fragment_accepted_sha();
    if expr_fragment_accepted_pin != audited_expr_fragment_accepted {
        return Err(format!(
            "expr-fragment-accepted hash mismatch: certificate pins {expr_fragment_accepted_pin}, checker expects {audited_expr_fragment_accepted}"
        ));
    }
    let accepted_artifact_pin = manifest_str(&manifest, "accepted_artifact_sha256")?;
    let audited_accepted_artifact = cert::audited_accepted_artifact_sha();
    if accepted_artifact_pin != audited_accepted_artifact {
        return Err(format!(
            "accepted-artifact hash mismatch: certificate pins {accepted_artifact_pin}, checker expects {audited_accepted_artifact}"
        ));
    }
    let accepted_artifact_core_pin = manifest_str(&manifest, "accepted_artifact_core_sha256")?;
    let audited_accepted_artifact_core = cert::audited_accepted_artifact_core_sha();
    if accepted_artifact_core_pin != audited_accepted_artifact_core {
        return Err(format!(
            "accepted-artifact-core hash mismatch: certificate pins {accepted_artifact_core_pin}, checker expects {audited_accepted_artifact_core}"
        ));
    }
    let artifact_root = manifest_str(&manifest, "artifact_certificate_root")?;
    if artifact_root != cert::ARTIFACT_CERTIFICATE_ROOT {
        return Err(format!(
            "artifact certificate root mismatch: certificate pins {artifact_root}, checker expects {}",
            cert::ARTIFACT_CERTIFICATE_ROOT
        ));
    }

    // 2. Report candidates from the untrusted JSON, each charset-gated on its
    //    decoded value so it is safe to splice as a Lean literal below.
    let cands = read_candidates(&manifest)?;

    // 2b. Re-derive non-expression obligations in Rust as a fail-fast pre-check
    //     over the hash-verified artifact bytes with the audited disassembler,
    //     deliberately excluding expr fragments. Expr fragments are admitted in
    //     the next step from checked plan sidecars plus canonical code-entry byte
    //     equality, then merged back by the actual byte-derived function order.
    //     Host/self remain witness inputs; code/carrier/struct facts are instead
    //     recomputed from ArtifactBytes by CertDecode inside the kernel. If
    //     disassembly fails outright
    //     (not a wasm module, no box helper), decline here — before the witness —
    //     fail-closed.
    //     The model `.lean` files supply the combinator operator (`+`/`*`) that
    //     the bytes cannot distinguish for the bignum helpers; they are the same
    //     (untrusted) model the kernel witness proves the bytes against, so
    //     reading the operator here does not widen trust — `lake` rejects any
    //     mismatch. Only the `def X__fuel` operator is read; nothing is executed.
    let model_files = read_lean_files(cert_dir);
    // Exports the manifest routes through checked expr-fragment plan sidecars
    // are excluded from legacy byte classification BY NAME: a plan-first
    // export that also matches a legacy template (the straight-line integer
    // shape) must not produce a duplicate obligation for the same function.
    // This is fail-closed: an entry claiming the plan-first class without a
    // sidecar that checks against the bytes is declined below, never silently
    // re-admitted through the legacy classifier.
    let plan_covered_exports = expr_fragment_class_exports(&manifest)?;
    let non_expr_cert = cert::rederive_certificate_without_expr_fragments(
        &bytes,
        &model_files,
        &plan_covered_exports,
    )?;
    let mut rederived = non_expr_cert.obligations;

    // 2c. Expression-fragment sidecars are untrusted proof-carrying metadata.
    //     Source-projectable fragments are admitted from `source_fragment`
    //     only: the checker validates the SymPlan, derives the representation
    //     ExprFragmentRawPlan itself, and accepts it only after verifier-owned
    //     canonical lowering matches the actual raw code-entry bytes.
    //     Representation `fragment` remains solely as a fallback for fragments
    //     that cannot yet be named in SymPlan. The byte classifier no longer
    //     decides which expr fragments are in scope.
    let (sidecar_obligations, sidecar_contracts) =
        checked_fragment_sidecar_obligations(cert_dir, &manifest, &bytes, &rederived)?;
    // Plan-covered exports contribute their runtime contracts here instead of
    // through the legacy classifier they are excluded from; merged in the
    // canonical contract order the emitter uses.
    let derived_contracts = merge_runtime_contracts(non_expr_cert.contracts, sidecar_contracts);
    rederived.extend(sidecar_obligations);
    rederived.sort_by_key(|r| r.func_order);
    reject_duplicate_rederived_func_orders(&rederived)?;

    // The re-derived export names come from the module's export section, which a
    // hostile producer controls via the bytes; gate them exactly like the JSON
    // candidates before they are spliced as Lean string literals in the witness.
    for r in &rederived {
        gate_candidate("re-derived export name", &r.name)?;
    }
    let certified_bindings = rederived
        .iter()
        .map(|obligation| (obligation.name.clone(), obligation.self_idx))
        .collect::<Vec<_>>();
    let module_envelope = cert::collect_module_envelope_facts(&bytes, &certified_bindings)?;
    for export in &module_envelope.exports {
        gate_candidate("byte-derived module export name", &export.name)?;
    }
    for (module, name) in &module_envelope.capabilities {
        gate_candidate("byte-derived capability module", module)?;
        gate_candidate("byte-derived capability name", name)?;
    }

    // 3. Assemble a checker-owned build. The audited schema + prelude come from
    //    THIS binary, never from the cert; the cert supplies only per-artifact
    //    DATA (Module/Manifest/Certificate/Final + the model modules). Each data
    //    file's name is gated (no lakefile-root injection) and its text scanned
    //    for code-executing tokens before it is staged. The cert's own lakefile
    //    / srcDir / `.lake` cache are never read.
    let build = assemble_build(cert_dir, &bytes)?;

    // The artifact DATA cache is content-addressed by the complete manifest hash
    // wall, schema version, toolchain, and exact staged build sources. It stores
    // only Lake outputs. A hit is therefore a build hint, never a verdict: Lake
    // still checks the freshly staged source dependency traces, and the checker
    // re-authors/runs the witness (including the Artifact.data rfl pin,
    // Artifact.certificate type ascription, and axiom audit) below. Accidental
    // stale or poisoned output either fails integrity, makes Lake rebuild/fail,
    // or makes that fresh witness fail; none of those paths can report certified.
    let cache_pins = [
        ("wasm_sha256", pinned),
        ("schema_sha256", schema_pin),
        ("schema_core_sha256", schema_core_pin),
        ("prelude_sha256", prelude_pin),
        ("cert_decode_sha256", decode_pin),
        ("plan_check_sha256", plan_check_pin),
        ("plan_lower_sha256", plan_lower_pin),
        ("plan_bytes_sha256", plan_bytes_pin),
        ("wasm_slice_sha256", wasm_slice_pin),
        ("expr_fragment_accepted_sha256", expr_fragment_accepted_pin),
        ("accepted_artifact_sha256", accepted_artifact_pin),
        ("accepted_artifact_core_sha256", accepted_artifact_core_pin),
    ];
    let mut artifact_cache = ArtifactBuildCache::prepare(
        &build.path,
        &ArtifactCacheKeyMaterial {
            schema_version,
            pinned_sha256: &cache_pins,
            toolchain_version: cert::LEAN_TOOLCHAIN.trim(),
        },
    );

    // A whole DATA-cache hit already contains the pristine prelude. On a miss,
    // retain the existing env-gated prelude seed before building the DATA roots.
    if !artifact_cache.was_hit() {
        reuse_prebuilt_prelude(&build.path);
    }

    // Byte-derived inputs used by both witness variants. They are computed
    // before Lake so even a build decline can be followed by the required
    // diagnostic-witness attempt.
    let host_table_lean = cert::byte_derived_frag_host_table_lean(&bytes)?;
    let struct_table_lean = cert::frag_struct_table_lean_from_entries(
        rederived
            .iter()
            .flat_map(|r| r.fragment_struct_entries.iter()),
    )?;

    // 4. The assembled project must build under the pinned toolchain. If a
    //    validated cache entry nevertheless breaks Lake, discard it and retry
    //    once from freshly staged sources (optionally with only the pristine
    //    prelude seed). The clean retry is authoritative.
    let mut b = run_lake(&build.path, &["build"])?;
    if !b.status.success() && artifact_cache.was_hit() {
        artifact_cache.invalidate(&build.path);
        reuse_prebuilt_prelude(&build.path);
        b = run_lake(&build.path, &["build"])?;
    }
    if !b.status.success() {
        let diagnostic = author_and_run_checker_witness(
            &build.path,
            &actual,
            &cands,
            &rederived,
            &derived_contracts,
            &host_table_lean,
            &struct_table_lean,
            &module_envelope,
            WitnessMode::Diagnostic,
        )?;
        if diagnostic.status.success() {
            return Err(
                "internal verifier error: lake build failed but the diagnostic checker witness \
                 succeeded; verification failed closed"
                    .to_string(),
            );
        }
        // The data project itself failed to build, so the diagnostic witness
        // cannot even import its modules — the real reason lives in the BUILD
        // output, which is what the decline must carry.
        return Err(format!(
            "certificate did not build (lake build failed):\n{}",
            tail(&b.combined, 30)
        ));
    }
    artifact_cache.publish(&build.path);

    // 5. Kernel witness authored BY THE CHECKER (never shipped in the cert):
    //    the sha binding, the report-candidate bindings, the artifact-decode /
    //    checked-plan bindings (kernel-decoded non-expression code/carrier/struct,
    //    canonical expression plans, and Rust-derived host/self), the final-theorem type
    //    ascription, and the axiom-whitelist check (see `checker_witness`).
    let w = author_and_run_checker_witness(
        &build.path,
        &actual,
        &cands,
        &rederived,
        &derived_contracts,
        &host_table_lean,
        &struct_table_lean,
        &module_envelope,
        WitnessMode::Fast,
    )?;
    if !w.status.success() {
        // The fast witness deliberately omits two redundant but useful mirror
        // proofs. Re-author the diagnostic superset on every decline so callers
        // retain the existing per-conjunct Lean errors. The diagnostic result is
        // never allowed to upgrade the verdict: if the superset unexpectedly
        // succeeds after the fast witness failed, the checker reports an internal
        // inconsistency and fails closed.
        let diagnostic = author_and_run_checker_witness(
            &build.path,
            &actual,
            &cands,
            &rederived,
            &derived_contracts,
            &host_table_lean,
            &struct_table_lean,
            &module_envelope,
            WitnessMode::Diagnostic,
        )?;
        if diagnostic.status.success() {
            return Err(
                "internal verifier error: the fast checker witness failed but its diagnostic \
                 superset succeeded; verification failed closed"
                    .to_string(),
            );
        }
        // The verdict is this exit code, not any parsed line. The lake output is
        // shown to the human to name which face failed (hash, a report binding,
        // an artifact-decode / checked-plan binding, the `Holds manifest` type,
        // or a non-whitelisted axiom).
        return Err(format!(
            "certificate does not bind to this artifact: the checker's diagnostic kernel witness \
             (hash binding, certified-export/contract/profile/abi bindings against the \
             proven manifest, the artifact-root binding, the artifact-decode / checked-plan bindings for \
             kernel-decoded code/carrier/struct facts plus pinned host/self, the semantic-face \
             bindings that pin each obligation's Dom/Cod/domRepr/codRepr to the standard \
             form of its class and prove every domain is inhabited, the final-theorem type \
             `Holds manifest`, and the axiom whitelist) did not check:\n{}",
            tail(&diagnostic.combined, 30)
        ));
    }

    // 6. The witness checked, so every binding is kernel-confirmed against the
    //    proven manifest. Build the report from the BYTE-DERIVED export names
    //    (kernel-pinned to `manifest.obligations.map (·.export_)`), not the JSON;
    //    the count is exactly the kernel-confirmed obligation count. The Dom/Cod
    //    NAMES are display-only source labels read from the JSON, position for
    //    position with the pinned obligations, and sanitized for terminal output.
    let certified = manifest.get("certified").and_then(Value::as_array);
    let exports = rederived
        .iter()
        .enumerate()
        .map(|(i, r)| {
            let entry = certified.and_then(|arr| arr.get(i));
            let dom = entry
                .and_then(|e| e.get("dom"))
                .and_then(Value::as_str)
                .map(display_safe);
            let cod = entry
                .and_then(|e| e.get("cod"))
                .and_then(Value::as_str)
                .map(display_safe);
            CertifiedExport {
                name: r.name.clone(),
                policy: r.policy.manifest_name().to_string(),
                face: if r.string_eq_plan_lean.is_some() {
                    "class: verbatim string equality match  |  Dom: WVal  Cod: WVal  codRepr: verbatimRepr  (model is a read declaration; behaviour pinned by an interpreter tripwire)"
                        .to_string()
                } else {
                    r.face.describe(dom.as_deref(), cod.as_deref())
                },
            }
        })
        .collect();
    Ok(TrustedReport {
        exports,
        contracts: derived_contracts,
        profile: cands.profile,
        abi: cands.abi,
        artifact_hash: actual,
    })
}

/// The export names the untrusted manifest routes through the plan-first
/// expr-fragment class. Used only to EXCLUDE those names from legacy byte
/// classification; admission still requires the sidecar check to succeed.
fn expr_fragment_class_exports(manifest: &Value) -> Result<Vec<String>, String> {
    let certified = manifest
        .get("certified")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `certified`".to_string())?;
    let mut names = Vec::new();
    for entry in certified {
        let name = entry.get("name").and_then(Value::as_str).ok_or_else(|| {
            "cert-manifest.json `certified[]` entry is missing string field `name`".to_string()
        })?;
        if entry.get("class").and_then(Value::as_str) == Some("expr-fragment-v1") {
            names.push(name.to_string());
        }
    }
    Ok(names)
}

/// Union of byte-derived legacy contracts and plan-sidecar contracts, in the
/// canonical order the emitter renders.
fn merge_runtime_contracts(legacy: Vec<String>, sidecar: Vec<String>) -> Vec<String> {
    let canonical = [
        cert::BOX_CONTRACT,
        cert::INT_ADD_CONTRACT,
        cert::INT_SUB_CONTRACT,
        cert::STRING_EQ_CONTRACT,
        cert::STRING_CONCAT_CONTRACT,
        cert::INT_ADD_TOTAL_CONTRACT,
        cert::INT_SUB_TOTAL_CONTRACT,
    ];
    canonical
        .iter()
        .filter(|c| legacy.iter().any(|l| l == *c) || sidecar.iter().any(|s| s == *c))
        .map(|c| c.to_string())
        .collect()
}

fn checked_fragment_sidecar_obligations(
    cert_dir: &Path,
    manifest: &Value,
    wasm_bytes: &[u8],
    byte_derived_legacy: &[cert::RederivedObligation],
) -> Result<(Vec<cert::RederivedObligation>, Vec<String>), String> {
    let certified = manifest
        .get("certified")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `certified`".to_string())?;

    let mut obligations = Vec::new();
    let mut contracts = Vec::new();
    for entry in certified {
        let name = entry.get("name").and_then(Value::as_str).ok_or_else(|| {
            "cert-manifest.json `certified[]` entry is missing string field `name`".to_string()
        })?;
        let class = entry.get("class").and_then(Value::as_str);
        if class == Some("verbatim-string-eq") {
            let (profile, path, claimed_sha, text) = read_fragment_sidecar(cert_dir, name, entry)?;
            let (source_profile, source_path, source_claimed_sha, source_text) =
                read_named_fragment_sidecar(cert_dir, name, entry, "source_fragment")?;
            if profile != "string-eq-v1" {
                return Err(format!(
                    "fragment `{name}` sidecar profile mismatch: manifest says `{profile}`"
                ));
            }
            if source_profile != "sym-fragment-v1" {
                return Err(format!(
                    "fragment `{name}` source sidecar profile mismatch: manifest says \
                     `{source_profile}`"
                ));
            }
            cert::parse_string_eq_plan(&text)
                .map_err(|e| format!("fragment `{name}` string-eq plan is malformed: {e}"))?;
            let expected_obligation = byte_derived_legacy
                .iter()
                .find(|r| r.name == name)
                .ok_or_else(|| {
                    format!(
                        "fragment `{name}` declares `string-eq-v1`, but the wasm bytes \
                         do not re-derive a String.eq certificate for that export"
                    )
                })?;
            let expected = expected_obligation.string_eq_plan.as_ref().ok_or_else(|| {
                format!(
                    "fragment `{name}` declares `string-eq-v1`, but the wasm bytes \
                     do not re-derive a String.eq certificate for that export"
                )
            })?;
            let expected_source =
                expected_obligation
                    .string_eq_sym_plan
                    .as_ref()
                    .ok_or_else(|| {
                        format!(
                            "fragment `{name}` declares `source_fragment`, but the wasm bytes \
                         do not re-derive a String.eq SymPlan for that export"
                        )
                    })?;
            if expected_source.path != source_path {
                return Err(format!(
                    "fragment `{name}` checked source plan path mismatch: plan checks as `{}`, \
                     manifest says `{source_path}`",
                    expected_source.path
                ));
            }
            if expected_source.sha256 != source_claimed_sha || expected_source.text != source_text {
                return Err(format!(
                    "fragment `{name}` source SymPlan sidecar is not the canonical \
                     byte-derived source plan"
                ));
            }
            if expected.path != path {
                return Err(format!(
                    "fragment `{name}` checked plan path mismatch: plan checks as `{}`, \
                     manifest says `{path}`",
                    expected.path
                ));
            }
            if expected.sha256 != claimed_sha || expected.text != text {
                return Err(format!(
                    "fragment `{name}` string-eq sidecar is not the canonical \
                     byte-derived equality plan"
                ));
            }
            continue;
        }

        if class == Some("verbatim-string-concat") {
            let (profile, path, claimed_sha, text) = read_fragment_sidecar(cert_dir, name, entry)?;
            let (source_profile, source_path, source_claimed_sha, source_text) =
                read_named_fragment_sidecar(cert_dir, name, entry, "source_fragment")?;
            if profile != "string-concat-v1" {
                return Err(format!(
                    "fragment `{name}` sidecar profile mismatch: manifest says `{profile}`"
                ));
            }
            if source_profile != "sym-fragment-v1" {
                return Err(format!(
                    "fragment `{name}` source sidecar profile mismatch: manifest says \
                     `{source_profile}`"
                ));
            }
            cert::parse_string_concat_plan(&text)
                .map_err(|e| format!("fragment `{name}` string-concat plan is malformed: {e}"))?;
            let expected_obligation = byte_derived_legacy
                .iter()
                .find(|r| r.name == name)
                .ok_or_else(|| {
                    format!(
                        "fragment `{name}` declares `string-concat-v1`, but the wasm bytes \
                         do not re-derive a String.concat certificate for that export"
                    )
                })?;
            let expected = expected_obligation
                .string_concat_plan
                .as_ref()
                .ok_or_else(|| {
                    format!(
                        "fragment `{name}` declares `string-concat-v1`, but the wasm bytes \
                         do not re-derive a String.concat certificate for that export"
                    )
                })?;
            let expected_source = expected_obligation
                .string_concat_sym_plan
                .as_ref()
                .ok_or_else(|| {
                    format!(
                        "fragment `{name}` declares `source_fragment`, but the wasm bytes \
                         do not re-derive a String.concat SymPlan for that export"
                    )
                })?;
            if expected_source.path != source_path {
                return Err(format!(
                    "fragment `{name}` checked source plan path mismatch: plan checks as `{}`, \
                     manifest says `{source_path}`",
                    expected_source.path
                ));
            }
            if expected_source.sha256 != source_claimed_sha || expected_source.text != source_text {
                return Err(format!(
                    "fragment `{name}` source SymPlan sidecar is not the canonical \
                     byte-derived source plan"
                ));
            }
            if expected.path != path {
                return Err(format!(
                    "fragment `{name}` checked plan path mismatch: plan checks as `{}`, \
                     manifest says `{path}`",
                    expected.path
                ));
            }
            if expected.sha256 != claimed_sha || expected.text != text {
                return Err(format!(
                    "fragment `{name}` string-concat sidecar is not the canonical \
                     byte-derived concat plan"
                ));
            }
            continue;
        }

        if class == Some("adt-constructor")
            && (entry.get("fragment").is_some() || entry.get("source_fragment").is_some())
        {
            let (profile, path, claimed_sha, text) = read_fragment_sidecar(cert_dir, name, entry)?;
            let (source_profile, source_path, source_claimed_sha, source_text) =
                read_named_fragment_sidecar(cert_dir, name, entry, "source_fragment")?;
            if profile != "construct-v1" {
                return Err(format!(
                    "fragment `{name}` sidecar profile mismatch: manifest says `{profile}`"
                ));
            }
            if source_profile != "sym-fragment-v1" {
                return Err(format!(
                    "fragment `{name}` source sidecar profile mismatch: manifest says \
                     `{source_profile}`"
                ));
            }
            cert::parse_construct_plan(&text)
                .map_err(|e| format!("fragment `{name}` construct plan is malformed: {e}"))?;
            let expected_obligation = byte_derived_legacy
                .iter()
                .find(|r| r.name == name)
                .ok_or_else(|| {
                    format!(
                        "fragment `{name}` declares `construct-v1`, but the wasm bytes \
                         do not re-derive an ADT constructor certificate for that export"
                    )
                })?;
            let expected = expected_obligation.construct_plan.as_ref().ok_or_else(|| {
                format!(
                    "fragment `{name}` declares `construct-v1`, but the wasm bytes \
                     do not re-derive an ADT constructor plan for that export"
                )
            })?;
            let expected_source =
                expected_obligation
                    .construct_sym_plan
                    .as_ref()
                    .ok_or_else(|| {
                        format!(
                            "fragment `{name}` declares `source_fragment`, but the wasm bytes \
                         and source model do not re-derive a constructor SymPlan for that export"
                        )
                    })?;
            if expected_source.path != source_path {
                return Err(format!(
                    "fragment `{name}` checked source plan path mismatch: plan checks as `{}`, \
                     manifest says `{source_path}`",
                    expected_source.path
                ));
            }
            if expected_source.sha256 != source_claimed_sha || expected_source.text != source_text {
                return Err(format!(
                    "fragment `{name}` source SymPlan sidecar is not the canonical \
                     byte-derived constructor source plan"
                ));
            }
            if expected.path != path {
                return Err(format!(
                    "fragment `{name}` checked plan path mismatch: plan checks as `{}`, \
                     manifest says `{path}`",
                    expected.path
                ));
            }
            if expected.sha256 != claimed_sha || expected.text != text {
                return Err(format!(
                    "fragment `{name}` construct sidecar is not the canonical \
                     byte-derived constructor plan"
                ));
            }
            continue;
        }

        if class != Some("expr-fragment-v1") {
            if entry.get("fragment").is_some() || entry.get("source_fragment").is_some() {
                return Err(format!(
                    "cert-manifest.json entry for `{name}` has fragment sidecar metadata but \
                     is not a plan-first class"
                ));
            }
            continue;
        }

        if entry.get("source_fragment").is_some() {
            if entry.get("fragment").is_some() {
                return Err(format!(
                    "cert-manifest.json entry for `{name}` carries both `source_fragment` and \
                     `fragment`; source-projectable expr fragments must use `source_fragment` \
                     only"
                ));
            }
            let (source_profile, source_path, source_claimed_sha, source_text) =
                read_named_fragment_sidecar(cert_dir, name, entry, "source_fragment")?;
            if source_profile != "sym-fragment-v1" {
                return Err(format!(
                    "fragment `{name}` source sidecar profile mismatch: manifest says \
                     `{source_profile}`"
                ));
            }
            let source_check = cert::check_sym_fragment_plan_sidecar(
                wasm_bytes,
                name,
                &source_text,
            )
            .map_err(|e| {
                format!("fragment `{name}` source plan sidecar does not check against wasm: {e}")
            })?;
            if source_check.sidecar.path != source_path {
                return Err(format!(
                    "fragment `{name}` checked source plan path mismatch: plan checks as `{}`, \
                     manifest says `{source_path}`",
                    source_check.sidecar.path
                ));
            }
            if source_check.sidecar.sha256 != source_claimed_sha
                || source_check.sidecar.text != source_text
            {
                return Err(format!(
                    "fragment `{name}` source SymPlan sidecar is not the canonical \
                     checked source plan"
                ));
            }
            if !source_check.canonical_matches_actual {
                return Err(format!(
                    "fragment `{name}` source plan-first canonical lowering does not match \
                     the actual wasm code-entry{}",
                    source_check
                        .mismatch_reason
                        .as_deref()
                        .map(|reason| format!(" ({reason})"))
                        .unwrap_or_default()
                ));
            }
            contracts.extend(source_check.runtime_contracts.iter().cloned());
            obligations.push(source_check.obligation);
            continue;
        }

        let (profile, path, claimed_sha, text) = read_fragment_sidecar(cert_dir, name, entry)?;
        if profile != "expr-fragment-v1" {
            return Err(format!(
                "fragment `{name}` sidecar profile mismatch: manifest says `{profile}`"
            ));
        }
        let plan_check =
            cert::check_expr_fragment_plan_sidecar(wasm_bytes, name, &text).map_err(|e| {
                format!("fragment `{name}` plan sidecar does not check against wasm: {e}")
            })?;
        if plan_check.sidecar.path != path {
            return Err(format!(
                "fragment `{name}` checked plan path mismatch: plan checks as `{}`, \
                 manifest says `{path}`",
                plan_check.sidecar.path
            ));
        }
        if plan_check.sidecar.sha256 != claimed_sha || plan_check.sidecar.text != text {
            return Err(format!(
                "fragment `{name}` sidecar plan is not the canonical checked plan"
            ));
        }
        if !plan_check.canonical_matches_actual {
            return Err(format!(
                "fragment `{name}` plan-first canonical lowering does not match the \
                 actual wasm code-entry{}",
                plan_check
                    .mismatch_reason
                    .as_deref()
                    .map(|reason| format!(" ({reason})"))
                    .unwrap_or_default()
            ));
        }
        if plan_check.obligation.fragment_sym_plan.is_some() {
            return Err(format!(
                "cert-manifest.json entry for `{name}` uses representation `fragment` metadata, \
                 but the checked plan is source-projectable; use `source_fragment` instead"
            ));
        }

        contracts.extend(plan_check.runtime_contracts.iter().cloned());
        obligations.push(plan_check.obligation);
    }
    Ok((obligations, contracts))
}

fn read_fragment_sidecar(
    cert_dir: &Path,
    name: &str,
    entry: &Value,
) -> Result<(&'static str, String, String, String), String> {
    read_named_fragment_sidecar(cert_dir, name, entry, "fragment")
}

fn read_named_fragment_sidecar(
    cert_dir: &Path,
    name: &str,
    entry: &Value,
    field: &str,
) -> Result<(&'static str, String, String, String), String> {
    let fragment = entry.get(field).ok_or_else(|| {
        format!("cert-manifest.json entry for `{name}` is missing `{field}` sidecar metadata")
    })?;
    let profile = fragment
        .get("profile")
        .and_then(Value::as_str)
        .ok_or_else(|| {
            format!("cert-manifest.json `{field}` for `{name}` is missing string field `profile`")
        })?;
    let profile = match profile {
        "expr-fragment-v1" => "expr-fragment-v1",
        "sym-fragment-v1" => "sym-fragment-v1",
        "string-eq-v1" => "string-eq-v1",
        "string-concat-v1" => "string-concat-v1",
        "construct-v1" => "construct-v1",
        other => {
            return Err(format!(
                "fragment `{name}` sidecar profile mismatch: manifest says `{other}`"
            ));
        }
    };
    let path = fragment
        .get("plan")
        .and_then(Value::as_str)
        .ok_or_else(|| {
            format!("cert-manifest.json `{field}` for `{name}` is missing string field `plan`")
        })?;
    let plan_path = checked_fragment_sidecar_path(cert_dir, path, profile)?;
    let claimed_sha = fragment
        .get("plan_sha256")
        .and_then(Value::as_str)
        .ok_or_else(|| {
            format!(
                "cert-manifest.json `{field}` for `{name}` is missing string field \
                 `plan_sha256`"
            )
        })?;
    let text = std::fs::read_to_string(&plan_path).map_err(|e| {
        format!(
            "cannot read fragment `{name}` `{field}` sidecar `{}`: {e}",
            plan_path.display()
        )
    })?;
    let file_sha = cert::sha256_hex(text.as_bytes());
    if file_sha != claimed_sha {
        return Err(format!(
            "fragment `{name}` `{field}` sidecar file hash mismatch: file hashes to \
             {file_sha}, manifest pins {claimed_sha}"
        ));
    }
    Ok((profile, path.to_string(), claimed_sha.to_string(), text))
}

fn checked_fragment_sidecar_path(
    cert_dir: &Path,
    path: &str,
    profile: &str,
) -> Result<PathBuf, String> {
    let path = Path::new(path);
    if path.is_absolute() {
        return Err("fragment sidecar path must be relative".to_string());
    }
    let components = path.components().collect::<Vec<_>>();
    let [
        std::path::Component::Normal(dir),
        std::path::Component::Normal(file),
    ] = components.as_slice()
    else {
        return Err(
            "fragment sidecar path must have shape `fragments/<name>.<profile>.plan`".to_string(),
        );
    };
    if dir.to_str() != Some("fragments") {
        return Err("fragment sidecar path must live under the `fragments/` directory".to_string());
    }
    let file = file
        .to_str()
        .ok_or_else(|| "fragment sidecar filename is not valid UTF-8".to_string())?;
    let suffix = match profile {
        "expr-fragment-v1" => ".expr-fragment-v1.plan",
        "sym-fragment-v1" => ".sym-fragment-v1.plan",
        "string-eq-v1" => ".string-eq-v1.plan",
        "string-concat-v1" => ".string-concat-v1.plan",
        "construct-v1" => ".construct-v1.plan",
        _ => return Err(format!("unsupported fragment profile `{profile}`")),
    };
    if !file.ends_with(suffix) || file.contains('/') || file.contains('\\') {
        return Err(format!(
            "fragment sidecar filename must end with `{suffix}`"
        ));
    }
    Ok(cert_dir.join(path))
}

fn reject_duplicate_rederived_func_orders(
    rederived: &[cert::RederivedObligation],
) -> Result<(), String> {
    for pair in rederived.windows(2) {
        let [a, b] = pair else {
            continue;
        };
        if a.func_order == b.func_order {
            return Err(format!(
                "certificate re-derived duplicate obligations for wasm function order {} \
                 (`{}` and `{}`)",
                a.func_order, a.name, b.name
            ));
        }
    }
    Ok(())
}

/// Populate a fresh, checker-owned build directory: the cert's DATA-only Lean
/// files (each name-gated and token-scanned), the audited schema cores and shims,
/// plan-check/plan-lower/prelude/
/// toolchain from THIS binary, the actual artifact bytes as checker-authored
/// Lean data, and a checker-authored lakefile.
fn assemble_build(cert_dir: &Path, wasm_bytes: &[u8]) -> Result<BuildDir, String> {
    let build = BuildDir::new()?;

    // Copy the cert's data Lean files, collecting lakefile roots as we go.
    let mut roots: Vec<String> = Vec::new();
    let entries = std::fs::read_dir(cert_dir)
        .map_err(|e| format!("cannot read cert dir {}: {e}", cert_dir.display()))?;
    for entry in entries {
        let entry = entry.map_err(|e| format!("cert dir read: {e}"))?;
        if !entry.file_type().map(|t| t.is_file()).unwrap_or(false) {
            continue; // skip `.lake/` and any other subdirectory
        }
        let name = entry.file_name().to_string_lossy().into_owned();
        if !name.ends_with(".lean") || CHECKER_OWNED.contains(&name.as_str()) {
            continue;
        }
        // Gate the file NAME: it must be a plain `Foo.lean` module identifier,
        // so it cannot inject tokens into the checker-authored lakefile roots.
        let root = lean_module_root(&name)?;
        let content = std::fs::read(entry.path())
            .map_err(|e| format!("cannot read cert file {name}: {e}"))?;
        // Scan the file TEXT for code-executing tokens before staging it.
        scan_for_code_exec(&name, &content)?;
        std::fs::write(build.path.join(&name), &content)
            .map_err(|e| format!("cannot stage {name}: {e}"))?;
        roots.push(root);
    }

    // Audited trusted computing base from THIS binary (not the cert).
    for (name, bytes) in static_prelude_files() {
        if name == "lakefile.lean" {
            continue;
        }
        std::fs::write(build.path.join(&name), bytes)
            .map_err(|e| format!("cannot stage {name}: {e}"))?;
    }
    write(
        &build.path,
        "ArtifactBytes.lean",
        &cert::render_artifact_bytes_lean(wasm_bytes),
    )?;
    // Preserve the production lakefile's historical root order exactly.
    roots.extend(
        STATIC_PRELUDE_ROOTS[..STATIC_PRELUDE_ROOTS.len() - 1]
            .iter()
            .map(|root| (*root).to_string()),
    );
    roots.push("ArtifactBytes".to_string());
    roots.push("CertPrelude".to_string());

    // Checker-authored lakefile: fixed `srcDir := "."`, roots derived from the
    // (gated) files actually present.
    write(&build.path, "lakefile.lean", &checker_lakefile(&roots))?;
    Ok(build)
}

/// Exact, artifact-independent files used to build the reusable prelude. The
/// returned sequence is filename-sorted so both staging and keying are stable.
fn static_prelude_files() -> Vec<(String, Vec<u8>)> {
    let static_lakefile = checker_lakefile(
        &PRISTINE_PRELUDE_ROOTS
            .iter()
            .map(|root| (*root).to_string())
            .collect::<Vec<_>>(),
    );
    let mut files = vec![
        (
            "AcceptedArtifact.lean",
            cert::CERT_ACCEPTED_ARTIFACT.as_bytes().to_vec(),
        ),
        (
            "AcceptedArtifactCore.lean",
            cert::CERT_ACCEPTED_ARTIFACT_CORE.as_bytes().to_vec(),
        ),
        ("CertPrelude.lean", cert::CERT_PRELUDE.as_bytes().to_vec()),
        ("CertDecode.lean", cert::CERT_DECODE.as_bytes().to_vec()),
        (
            "ExprFragmentAccepted.lean",
            cert::CERT_EXPR_FRAGMENT_ACCEPTED.as_bytes().to_vec(),
        ),
        ("PlanBytes.lean", cert::CERT_PLAN_BYTES.as_bytes().to_vec()),
        ("PlanCheck.lean", cert::CERT_PLAN_CHECK.as_bytes().to_vec()),
        ("PlanLower.lean", cert::CERT_PLAN_LOWER.as_bytes().to_vec()),
        ("Schema.lean", cert::CERT_SCHEMA.as_bytes().to_vec()),
        (
            "SchemaCore.lean",
            cert::CERT_SCHEMA_CORE.as_bytes().to_vec(),
        ),
        ("WasmSlice.lean", cert::CERT_WASM_SLICE.as_bytes().to_vec()),
        ("lakefile.lean", static_lakefile.into_bytes()),
        ("lean-toolchain", cert::LEAN_TOOLCHAIN.as_bytes().to_vec()),
    ]
    .into_iter()
    .map(|(name, bytes)| (name.to_string(), bytes))
    .collect::<Vec<_>>();
    files.sort_by(|a, b| a.0.cmp(&b.0));
    files
}

/// SHA-256 of the unambiguous, filename-sorted `(filename, exact bytes)`
/// sequence. Length prefixes prevent different pairs from sharing a framing.
fn static_prelude_key(files: &[(String, Vec<u8>)]) -> String {
    let mut hasher = Sha256::new();
    for (name, bytes) in files {
        hasher.update((name.len() as u64).to_be_bytes());
        hasher.update(name.as_bytes());
        hasher.update((bytes.len() as u64).to_be_bytes());
        hasher.update(bytes);
    }
    format!("{:x}", hasher.finalize())
}

/// Reuse a pristine prelude-only Lake build when the certification test
/// harness opts in. Every failure returns silently to the fresh-cache build.
fn reuse_prebuilt_prelude(build_dir: &Path) -> bool {
    let Some(store) = std::env::var_os("AVER_CERT_PRELUDE_CACHE").map(PathBuf::from) else {
        return false;
    };
    try_reuse_prebuilt_prelude(&store, build_dir).is_ok()
}

fn try_reuse_prebuilt_prelude(store: &Path, build_dir: &Path) -> Result<(), ()> {
    let files = static_prelude_files();
    let key = static_prelude_key(&files);
    let entry = store.join(&key);
    let cached_lake = entry.join(".lake");

    if !cached_lake.is_dir() {
        populate_prebuilt_prelude(store, &entry, &key, &files)?;
    }
    if !cached_lake.is_dir() {
        return Err(());
    }

    let destination = build_dir.join(".lake");
    if let Err(()) = copy_tree(&cached_lake, &destination) {
        let _ = std::fs::remove_dir_all(destination);
        return Err(());
    }
    if verify_prelude_integrity(&entry, &destination, &key).is_err() {
        let _ = std::fs::remove_dir_all(destination);
        let _ = std::fs::remove_dir_all(&entry);
        return Err(());
    }
    Ok(())
}

/// Build only the artifact-independent files, then atomically publish the
/// whole pristine project directory. Thus no artifact data can enter a store
/// entry, and concurrent builders either publish or consume the winner.
fn populate_prebuilt_prelude(
    store: &Path,
    entry: &Path,
    key: &str,
    files: &[(String, Vec<u8>)],
) -> Result<(), ()> {
    std::fs::create_dir_all(store).map_err(|_| ())?;
    let temp = store.join(format!("tmp-{}-{}", std::process::id(), unique_nanos()));
    let pristine = StoreBuildDir::new(temp)?;
    for (name, bytes) in files {
        std::fs::write(pristine.path.join(name), bytes).map_err(|_| ())?;
    }

    let built = run_lake(&pristine.path, &["build"]).map_err(|_| ())?;
    if !built.status.success() || !pristine.path.join(".lake").is_dir() {
        return Err(());
    }
    write_prelude_integrity_manifest(&pristine.path, key)?;

    match std::fs::rename(&pristine.path, entry) {
        Ok(()) => pristine.keep(),
        Err(_) if entry.join(".lake").is_dir() => {}
        Err(_) => return Err(()),
    }
    Ok(())
}

fn copy_tree(source: &Path, destination: &Path) -> Result<(), ()> {
    std::fs::create_dir(destination).map_err(|_| ())?;
    for entry in std::fs::read_dir(source).map_err(|_| ())? {
        let entry = entry.map_err(|_| ())?;
        let destination_entry = destination.join(entry.file_name());
        if entry.file_type().map_err(|_| ())?.is_dir() {
            copy_tree(&entry.path(), &destination_entry)?;
        } else {
            std::fs::copy(entry.path(), destination_entry).map_err(|_| ())?;
        }
    }
    Ok(())
}

/// Record the exact contents of a reusable `.lake` tree. The key binds the
/// manifest to the audited source bytes that produced the tree; the sorted
/// file list also makes additions and deletions detectable, not only edits.
fn write_prelude_integrity_manifest(entry: &Path, key: &str) -> Result<(), ()> {
    let hashes = lake_tree_hashes(&entry.join(".lake"))?;
    let mut manifest = format!("key {key}\n");
    for (path, hash) in hashes {
        manifest.push_str(&hash);
        manifest.push_str("  ");
        manifest.push_str(&path);
        manifest.push('\n');
    }
    std::fs::write(entry.join("manifest.sha256"), manifest).map_err(|_| ())
}

/// Verify the copied tree, rather than trusting the store in place. Once this
/// succeeds, subsequent build or witness failures are authoritative declines.
fn verify_prelude_integrity(entry: &Path, copied_lake: &Path, key: &str) -> Result<(), ()> {
    let manifest = std::fs::read_to_string(entry.join("manifest.sha256")).map_err(|_| ())?;
    let mut lines = manifest.lines();
    if lines.next() != Some(format!("key {key}").as_str()) {
        return Err(());
    }

    let mut expected = Vec::new();
    for line in lines {
        let (hash, path) = line.split_once("  ").ok_or(())?;
        if hash.len() != 64 || !hash.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err(());
        }
        let relative = Path::new(path);
        if relative.is_absolute()
            || relative
                .components()
                .any(|component| !matches!(component, std::path::Component::Normal(_)))
        {
            return Err(());
        }
        expected.push((path.to_string(), hash.to_ascii_lowercase()));
    }
    expected.sort();
    if expected != lake_tree_hashes(copied_lake)? {
        return Err(());
    }
    Ok(())
}

fn lake_tree_hashes(root: &Path) -> Result<Vec<(String, String)>, ()> {
    fn visit(root: &Path, dir: &Path, hashes: &mut Vec<(String, String)>) -> Result<(), ()> {
        for entry in std::fs::read_dir(dir).map_err(|_| ())? {
            let entry = entry.map_err(|_| ())?;
            let file_type = entry.file_type().map_err(|_| ())?;
            if file_type.is_dir() {
                visit(root, &entry.path(), hashes)?;
            } else if file_type.is_file() {
                let path = entry.path();
                let relative = path.strip_prefix(root).map_err(|_| ())?;
                let relative = relative
                    .to_str()
                    .ok_or(())?
                    .replace(std::path::MAIN_SEPARATOR, "/");
                if relative.contains(['\n', '\r']) {
                    return Err(());
                }
                let bytes = std::fs::read(entry.path()).map_err(|_| ())?;
                hashes.push((relative, cert::sha256_hex(&bytes)));
            } else {
                return Err(());
            }
        }
        Ok(())
    }

    let mut hashes = Vec::new();
    visit(root, root, &mut hashes)?;
    hashes.sort();
    Ok(hashes)
}

/// A cert data file must be named `<Ident>.lean` where `<Ident>` matches
/// `^[A-Za-z][A-Za-z0-9_]*$`; return that identifier (the lakefile root).
/// Rejecting anything else keeps the checker-authored lakefile's `roots` array
/// free of comma/space/newline injection from a hostile filename.
fn lean_module_root(name: &str) -> Result<String, String> {
    let stem = name.strip_suffix(".lean").ok_or_else(|| {
        format!("cert file `{name}` is not a `.lean` file (rejected as a build root)")
    })?;
    let mut chars = stem.chars();
    let ok = matches!(chars.next(), Some(c) if c.is_ascii_alphabetic())
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_');
    if stem.is_empty() || !ok {
        return Err(format!(
            "cert file name `{name}` is not a plain Lean module identifier \
             (must match ^[A-Za-z][A-Za-z0-9_]*\\.lean$); rejected to keep the \
             checker's lakefile roots uninjectable"
        ));
    }
    Ok(stem.to_string())
}

/// Scan a cert data file's raw text for tokens that make Lean elaboration run
/// arbitrary code. See the module doc: a deliberately brittle wall.
fn scan_for_code_exec(name: &str, content: &[u8]) -> Result<(), String> {
    let text = String::from_utf8_lossy(content);
    for tok in CODE_EXEC_TOKENS {
        if text.contains(tok) {
            return Err(format!(
                "cert data file `{name}` contains the token `{tok}`, which makes Lean \
                 elaboration execute code; declined (elaboration-executes-code wall)"
            ));
        }
    }
    Ok(())
}

/// The checker's own lakefile. Fixed structure; the root list is whatever Lean
/// modules ended up in the build dir (all name-gated). `srcDir := "."` so no
/// cert-supplied srcDir redirection is honored.
fn checker_lakefile(roots: &[String]) -> String {
    let list = roots
        .iter()
        .map(|r| format!("`{r}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{list}]\n"
    )
}

/// A Lean list literal of string literals. Every element has passed the charset
/// gate (no `"` or `\`, no control chars), so raw splicing is safe.
fn lean_str_list(items: &[String]) -> String {
    let inner = items
        .iter()
        .map(|s| format!("\"{s}\""))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{inner}]")
}

fn lean_string_pair_list(items: &[(String, String)]) -> String {
    let inner = items
        .iter()
        .map(|(left, right)| format!("(\"{left}\", \"{right}\")"))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{inner}]")
}

/// A Lean list literal of `Nat` literals (obligation self / carrier indices).
fn lean_nat_list(items: impl Iterator<Item = u32>) -> String {
    let inner = items.map(|n| n.to_string()).collect::<Vec<_>>().join(", ");
    format!("[{inner}]")
}

/// A Lean `List Nat` literal for raw bytes.
fn lean_byte_list(bytes: &[u8]) -> String {
    let inner = bytes
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{inner}]")
}

/// A Lean list literal of `(export name, ExprFragmentRawPlan)` pairs. The names
/// have passed the same charset gate as all other witness strings; the plan
/// terms are checker-rendered from verified sidecars, never copied from
/// attacker Lean text.
fn lean_expr_fragment_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let source_expr = rederived
        .iter()
        .filter_map(|r| {
            if r.fragment_sym_plan_lean.is_some() {
                r.fragment_plan_lean
                    .as_ref()
                    .map(|plan| format!("(\"{}\", {plan})", r.name))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let fallback_expr = rederived
        .iter()
        .filter_map(|r| {
            if r.fragment_sym_plan_lean.is_none() {
                r.fragment_plan_lean
                    .as_ref()
                    .map(|plan| format!("(\"{}\", {plan})", r.name))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let inner = source_expr
        .into_iter()
        .chain(fallback_expr)
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, SymRawPlan)` pairs for source-level
/// fragment plans. These terms are checker-rendered from verified sidecars,
/// never copied from attacker Lean text.
fn lean_sym_fragment_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let expr_plans = rederived
        .iter()
        .filter_map(|r| {
            r.fragment_sym_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>();
    let string_plans = rederived
        .iter()
        .filter_map(|r| {
            r.string_concat_sym_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>();
    let string_eq_plans = rederived
        .iter()
        .filter_map(|r| {
            r.string_eq_sym_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>();
    let construct_plans = rederived
        .iter()
        .filter_map(|r| {
            r.construct_sym_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>();
    let inner = expr_plans
        .into_iter()
        .chain(string_eq_plans)
        .chain(string_plans)
        .chain(construct_plans)
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, StringEqRawPlan)` pairs for
/// source-level `String.eq` witnesses. These terms are checker-rendered from
/// byte-derived helper shapes, never copied from attacker Lean text.
fn lean_string_eq_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.string_eq_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, RecursionRawPlan)` pairs for
/// byte-first fuel-recursion plans. These terms are checker-rendered from the
/// byte-derived recursion holes, never copied from attacker Lean text.
fn lean_recursion_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.recursion_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, MutualRawPlan)` pairs for byte-first
/// mutual-recursion member plans. These terms are checker-rendered from the
/// byte-derived SCC holes, never copied from attacker Lean text.
fn lean_mutual_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.mutual_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, VerbatimRawPlan)` pairs for byte-first
/// verbatim `ref.test`-dispatch plans. These terms are checker-rendered from the
/// byte-derived match holes, never copied from attacker Lean text.
fn lean_verbatim_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.verbatim_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, IntDispatchRawPlan)` pairs for
/// byte-first Int-face `ref.test`-dispatch plans. These terms are
/// checker-rendered from the byte-derived match holes, never copied from
/// attacker Lean text.
fn lean_int_dispatch_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.int_dispatch_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

fn lean_field_projection_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.field_projection_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

fn lean_composition_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let mut members = std::collections::BTreeMap::<String, String>::new();
    for obligation in rederived {
        for member in &obligation.composition_members {
            members
                .entry(member.name.clone())
                .or_insert_with(|| member.plan_lean.clone());
        }
    }
    let inner = members
        .into_iter()
        .map(|(name, plan)| format!("(\"{name}\", {plan})"))
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, StringConcatRawPlan)` pairs for
/// source-level `String.concat` witnesses. These terms are checker-rendered
/// from verified sidecars, never copied from attacker Lean text.
fn lean_string_concat_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.string_concat_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal of `(export name, ConstructRawPlan)` pairs for
/// source-level ADT constructor witnesses. These terms are checker-rendered
/// from byte-derived constructor shapes, never copied from attacker Lean text.
fn lean_construct_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let inner = rederived
        .iter()
        .filter_map(|r| {
            if r.construct_sym_plan_lean.is_some() {
                r.construct_plan_lean
                    .as_ref()
                    .map(|plan| format!("(\"{}\", {plan})", r.name))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// A Lean list literal pinning each source plan to the representation-level
/// plan obtained by the audited `SymRawPlan -> ExprFragmentRawPlan` encoder.
fn lean_sym_fragment_encoded_plan_pairs(rederived: &[cert::RederivedObligation]) -> String {
    let expr_pairs = rederived
        .iter()
        .filter_map(|r| {
            if r.fragment_sym_plan_lean.is_some() {
                let expr = r.fragment_plan_lean.as_ref()?;
                Some(format!("(\"{}\", some ({expr}))", r.name))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let string_pairs = rederived
        .iter()
        .filter_map(|r| {
            if r.string_concat_sym_plan_lean.is_some() {
                Some(format!("(\"{}\", none)", r.name))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let string_eq_pairs = rederived
        .iter()
        .filter_map(|r| {
            if r.string_eq_sym_plan_lean.is_some() {
                Some(format!("(\"{}\", none)", r.name))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let construct_pairs = rederived
        .iter()
        .filter_map(|r| {
            if r.construct_sym_plan_lean.is_some() {
                Some(format!("(\"{}\", none)", r.name))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let inner = expr_pairs
        .into_iter()
        .chain(string_eq_pairs)
        .chain(string_pairs)
        .chain(construct_pairs)
        .collect::<Vec<_>>()
        .join(",\n   ");
    format!("[ {inner} ]")
}

/// Checker-owned Lean `example`s proving that each checked expr-fragment plan
/// lowers, via the audited Lean lowerer, to the same instruction body the Rust
/// verifier rendered from the byte-bound/canonical-lowered plan.
fn lean_expr_fragment_lower_pins(rederived: &[cert::RederivedObligation]) -> String {
    let mut out = String::new();
    for r in rederived {
        let (Some(plan), Some(body)) = (
            r.fragment_plan_lean.as_ref(),
            r.fragment_lowered_body_lean.as_ref(),
        ) else {
            continue;
        };
        out.push_str(&format!(
            "-- `{}`: checked RawPlan lowers to the byte-bound instruction body.\n\
             example : AverCert.PlanLower.lowerExprFragmentBody {} ({}) = some ({}) := rfl\n",
            r.name, r.carrier, plan, body
        ));
    }
    out
}

/// Checker-owned Lean `example`s proving that each checked expr-fragment plan
/// lowers, via the audited Lean byte lowerer, to the exact canonical code-entry
/// bytes the verifier accepted against the artifact.
fn lean_expr_fragment_code_entry_pins(rederived: &[cert::RederivedObligation]) -> String {
    let mut out = String::new();
    for r in rederived {
        let (Some(plan), Some(bytes)) = (
            r.fragment_plan_lean.as_ref(),
            r.fragment_lowered_code_entry_lean.as_ref(),
        ) else {
            continue;
        };
        out.push_str(&format!(
            "-- `{}`: checked RawPlan lowers to the canonical code-entry bytes.\n\
             example : AverCert.PlanBytes.lowerExprFragmentCodeEntry {} ({}) = some ({}) := rfl\n",
            r.name, r.carrier, plan, bytes
        ));
    }
    out
}

/// Checker-owned Lean `example`s proving that each checked expr-fragment
/// export's canonical code-entry bytes are actually found in the checker-read
/// Wasm module bytes by export name.
fn lean_expr_fragment_wasm_slice_pins(rederived: &[cert::RederivedObligation]) -> String {
    let mut out = String::new();
    for r in rederived {
        let Some(bytes) = r.fragment_lowered_code_entry_lean.as_ref() else {
            continue;
        };
        let export_name_bytes = lean_byte_list(r.name.as_bytes());
        out.push_str(&format!(
            "-- `{}`: checker-read module bytes expose this exact code-entry.\n\
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {} = some ({}) := rfl\n",
            r.name, export_name_bytes, bytes
        ));
    }
    out
}

/// Checker-owned Lean `example`s proving that each checked expr-fragment export
/// resolves, from the checker-read Wasm bytes, to the exact function binding
/// expected by the verified plan: func index, defined-code index, function type
/// index and code-entry bytes.
fn lean_expr_fragment_func_binding_pins(rederived: &[cert::RederivedObligation]) -> String {
    let mut out = String::new();
    for r in rederived {
        let (Some(code_idx), Some(type_idx), Some(bytes)) = (
            r.fragment_code_idx,
            r.fragment_type_idx,
            r.fragment_lowered_code_entry_lean.as_ref(),
        ) else {
            continue;
        };
        let export_name_bytes = lean_byte_list(r.name.as_bytes());
        let binding = format!(
            "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
            r.self_idx, code_idx, type_idx, bytes
        );
        out.push_str(&format!(
            "-- `{}`: checker-read module bytes expose this exact function binding.\n\
             example : AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {} = some {} := rfl\n",
            r.name, export_name_bytes, binding
        ));
    }
    out
}

/// Checker-owned Lean `example`s proving that the single expr-fragment
/// acceptance predicate holds for each checked plan/export pair.
fn lean_expr_fragment_accepted_pins(rederived: &[cert::RederivedObligation]) -> String {
    let mut out = String::new();
    for r in rederived {
        let (Some(plan), Some(body), Some(bytes), Some(code_idx), Some(type_idx)) = (
            r.fragment_plan_lean.as_ref(),
            r.fragment_lowered_body_lean.as_ref(),
            r.fragment_lowered_code_entry_lean.as_ref(),
            r.fragment_code_idx,
            r.fragment_type_idx,
        ) else {
            continue;
        };
        let export_name_bytes = lean_byte_list(r.name.as_bytes());
        let binding = format!(
            "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
            r.self_idx, code_idx, type_idx, bytes
        );
        out.push_str(&format!(
            "-- `{}`: one aggregate expr-fragment acceptance check.\n\
             example : AverCert.ExprFragmentAccepted.accepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen {} {} ({}) ({}) ({}) {} := by dsimp [AverCert.ExprFragmentAccepted.accepted]; exact ⟨rfl, rfl, rfl, rfl, rfl⟩\n",
            r.name, export_name_bytes, r.carrier, plan, body, bytes, binding
        ));
    }
    out
}

/// Checker-owned Lean `example`s proving that fragment byte-origin acceptance
/// is tied to the schema obligation used by `Final.cert`. Artifact-level expr
/// fragments are source-first: current scalar fragments carry a
/// `SymFragmentClaim`, and the byte-bound ExprFragment plan is computed from
/// that source plan rather than accepted as a public fallback claim.
struct LeanExprFragmentArtifactClaims {
    sym_claims: String,
    string_eq_claims: String,
    string_claims: String,
    construct_claims: String,
    recursion_claims: String,
    mutual_claims: String,
    verbatim_claims: String,
    int_dispatch_claims: String,
    field_projection_claims: String,
    composition_members: String,
    composition_claims: String,
    obligation_proof: String,
    sym_proof: String,
    string_eq_proof: String,
    string_proof: String,
    construct_proof: String,
    recursion_proof: String,
    mutual_proof: String,
    verbatim_proof: String,
    int_dispatch_proof: String,
    field_projection_proof: String,
    composition_proof: String,
}

fn lean_expr_fragment_artifact_claims(
    rederived: &[cert::RederivedObligation],
    host_table_lean: &str,
    struct_table_lean: &str,
) -> LeanExprFragmentArtifactClaims {
    let mut sym_claims = Vec::new();
    let mut string_eq_claims = Vec::new();
    let mut string_claims = Vec::new();
    let mut construct_claims = Vec::new();
    let mut recursion_claims = Vec::new();
    let mut mutual_claims = Vec::new();
    let mut verbatim_claims = Vec::new();
    let mut int_dispatch_claims = Vec::new();
    let mut field_projection_claims = Vec::new();
    let mut composition_claims = Vec::new();
    let mut sym_proofs = Vec::new();
    let mut string_eq_proofs = Vec::new();
    let mut string_proofs = Vec::new();
    let mut construct_proofs = Vec::new();
    let mut recursion_proofs = Vec::new();
    let mut mutual_proofs = Vec::new();
    let mut verbatim_proofs = Vec::new();
    let mut int_dispatch_proofs = Vec::new();
    let mut field_projection_proofs = Vec::new();
    let mut composition_proofs = Vec::new();
    for r in rederived {
        if r.mutual_plan_lean.is_some() {
            let (
                Some(body),
                Some(bytes),
                Some(code_idx),
                Some(type_idx),
                Some(host_table),
                Some(member_set),
            ) = (
                r.mutual_lowered_body_lean.as_ref(),
                r.mutual_lowered_code_entry_lean.as_ref(),
                r.mutual_code_idx,
                r.mutual_type_idx,
                r.mutual_host_table_lean.as_ref(),
                r.mutual_member_set_lean.as_ref(),
            )
            else {
                continue;
            };
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            let binding = format!(
                "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                r.self_idx, code_idx, type_idx, bytes
            );
            mutual_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, memberSet := {member_set}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.MutualRecursionClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            mutual_proofs.push(format!(
                "⟨rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
                 ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
            ));
            continue;
        }
        if r.recursion_plan_lean.is_some() {
            let (Some(body), Some(bytes), Some(code_idx), Some(type_idx), Some(host_table)) = (
                r.recursion_lowered_body_lean.as_ref(),
                r.recursion_lowered_code_entry_lean.as_ref(),
                r.recursion_code_idx,
                r.recursion_type_idx,
                r.recursion_host_table_lean.as_ref(),
            ) else {
                continue;
            };
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            let binding = format!(
                "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                r.self_idx, code_idx, type_idx, bytes
            );
            recursion_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.RecursionClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            recursion_proofs.push(format!(
                "⟨rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
                 ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
            ));
            continue;
        }
        if r.string_eq_plan_lean.is_some() {
            let (
                Some(sym_plan),
                Some(body),
                Some(bytes),
                Some(code_idx),
                Some(type_idx),
                Some(string_ty),
                Some(string_eq_func_idx),
            ) = (
                r.string_eq_sym_plan_lean.as_ref(),
                r.string_eq_lowered_body_lean.as_ref(),
                r.string_eq_lowered_code_entry_lean.as_ref(),
                r.string_eq_code_idx,
                r.string_eq_type_idx,
                r.string_eq_string_ty,
                r.string_eq_func_idx,
            )
            else {
                continue;
            };
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            let binding = format!(
                "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                r.self_idx, code_idx, type_idx, bytes
            );
            string_eq_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, stringTy := {string_ty}, \
                 stringEqFuncIdx := {string_eq_func_idx}, \
                 symPlan := (({sym_plan}) : AverCert.Schema.SymRawPlan), \
                 obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.StringEqClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            string_eq_proofs.push(format!(
                "⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
                 ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
            ));
            continue;
        }
        if r.string_concat_plan_lean.is_some() {
            let (
                Some(sym_plan),
                Some(body),
                Some(bytes),
                Some(code_idx),
                Some(type_idx),
                Some(result_ty),
                Some(container_ty),
                Some(concat_func_idx),
            ) = (
                r.string_concat_sym_plan_lean.as_ref(),
                r.string_concat_lowered_body_lean.as_ref(),
                r.string_concat_lowered_code_entry_lean.as_ref(),
                r.string_concat_code_idx,
                r.string_concat_type_idx,
                r.string_concat_result_ty,
                r.string_concat_container_ty,
                r.string_concat_func_idx,
            )
            else {
                continue;
            };
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            let binding = format!(
                "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                r.self_idx, code_idx, type_idx, bytes
            );
            string_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, resultTy := {result_ty}, containerTy := {container_ty}, \
                 concatFuncIdx := {concat_func_idx}, \
                 symPlan := (({sym_plan}) : AverCert.Schema.SymRawPlan), \
                 obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.StringConcatClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            string_proofs.push(format!(
                "⟨rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
                 ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩⟩"
            ));
            continue;
        }
        if r.construct_plan_lean.is_some() {
            let (
                Some(sym_plan),
                Some(body),
                Some(bytes),
                Some(code_idx),
                Some(type_idx),
                Some(struct_idx),
                Some(field_count),
                Some(elem_ty),
            ) = (
                r.construct_sym_plan_lean.as_ref(),
                r.construct_lowered_body_lean.as_ref(),
                r.construct_lowered_code_entry_lean.as_ref(),
                r.construct_code_idx,
                r.construct_type_idx,
                r.construct_struct_idx,
                r.construct_field_count,
                r.construct_elem_ty_lean.as_ref(),
            )
            else {
                continue;
            };
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            let binding = format!(
                "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                r.self_idx, code_idx, type_idx, bytes
            );
            let (struct_type_proof, func_type_proof) = if r.construct_is_list {
                (
                    format!("Or.inr AverCert.Plans.{}ConstructStructTypeMatches", r.name),
                    format!("Or.inr AverCert.Plans.{}ConstructFuncTypeMatches", r.name),
                )
            } else {
                ("Or.inl rfl".to_string(), "Or.inl rfl".to_string())
            };
            construct_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, structIdx := {struct_idx}, fieldCount := {field_count}, elemTy := {elem_ty}, \
                 symPlan := (({sym_plan}) : AverCert.Schema.SymRawPlan), \
                 obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.ConstructClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            construct_proofs.push(format!(
                "⟨rfl, rfl, rfl, rfl, rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
                 ⟨rfl, rfl, rfl, rfl, rfl, rfl, {struct_type_proof}, \
                 {func_type_proof}, rfl⟩⟩⟩"
            ));
            continue;
        }
        if r.verbatim_plan_lean.is_some() {
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            verbatim_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.VerbatimClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            // The code entry, signature and payload conjuncts are the binding (no
            // host/self calls), so the witness is anonymous for the code entry,
            // binding; the final `rfl` pins the canonical locals count (the two
            // preceding `rfl`s discharge the byte-derived signature and payload
            // binds).
            verbatim_proofs.push(
                "⟨rfl, rfl, rfl, ⟨_, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩".to_string(),
            );
            continue;
        }
        if r.int_dispatch_plan_lean.is_some() {
            let Some(host_table) = r.int_dispatch_host_table_lean.as_ref() else {
                continue;
            };
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            int_dispatch_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, hostTable := {host_table}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.IntDispatchClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            // The code-entry and signature conjuncts plus the role-table
            // parameterization are the binding, so the witness is anonymous for
            // the body, code entry and binding — each pinned by `rfl` (the two
            // extra leading `rfl`s discharge the host-table distinctness and
            // obligation-wiring binds; the final `rfl` pins the code table with
            // the CANONICAL locals count, no existential).
            int_dispatch_proofs.push(
                "⟨rfl, rfl, rfl, rfl, rfl, ⟨_, _, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩"
                    .to_string(),
            );
            continue;
        }
        if r.field_projection_plan_lean.is_some() {
            let (Some(struct_idx), Some(field_count), Some(result_ty)) = (
                r.field_projection_struct_idx,
                r.field_projection_field_count,
                r.field_projection_result_ty_lean.as_ref(),
            ) else {
                continue;
            };
            let export_name_bytes = lean_byte_list(r.name.as_bytes());
            field_projection_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", carrier := {carrier}, structIdx := {struct_idx}, fieldCount := {field_count}, resultTy := {result_ty}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.FieldProjectionClaim)",
                name = r.name,
                carrier = r.carrier,
            ));
            field_projection_proofs.push(
                "⟨rfl, rfl, rfl, ⟨_, _, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩".to_string(),
            );
            continue;
        }
        let (Some(_plan), Some(body), Some(bytes), Some(code_idx), Some(type_idx)) = (
            r.fragment_plan_lean.as_ref(),
            r.fragment_lowered_body_lean.as_ref(),
            r.fragment_lowered_code_entry_lean.as_ref(),
            r.fragment_code_idx,
            r.fragment_type_idx,
        ) else {
            continue;
        };
        let export_name_bytes = lean_byte_list(r.name.as_bytes());
        let binding = format!(
            "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
            r.self_idx, code_idx, type_idx, bytes
        );
        let proof = format!(
            "⟨rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
             ⟨⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, rfl⟩⟩⟩"
        );
        if let Some(sym_plan) = r.fragment_sym_plan_lean.as_ref() {
            sym_claims.push(format!(
                "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
                 carrier := {carrier}, hostTable := {host_table_lean}, \
                 structTable := {struct_table_lean}, \
                 plan := (({sym_plan}) : AverCert.Schema.SymRawPlan), \
                 obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.SymFragmentClaim)",
                name = r.name,
                carrier = r.carrier
            ));
            sym_proofs.push(proof);
        }
    }
    let mut composition_member_map =
        std::collections::BTreeMap::<String, cert::RederivedCompositionMember>::new();
    for r in rederived {
        for member in &r.composition_members {
            composition_member_map
                .entry(member.name.clone())
                .or_insert_with(|| member.clone());
        }
    }
    let composition_members = composition_member_map
        .values()
        .map(|member| {
            format!(
                "({{ exportNameBytes := {bytes}, exportName := \"{name}\", plan := (({plan}) : AverCert.Schema.CompositionRawPlan) }} : AverCert.AcceptedArtifact.CompositionMemberClaim)",
                bytes = lean_byte_list(member.name.as_bytes()),
                name = member.name,
                plan = member.plan_lean,
            )
        })
        .collect::<Vec<_>>();
    for r in rederived {
        if r.composition_members.is_empty() {
            continue;
        }
        let (Some(host_table), Some(member_names)) = (
            r.composition_host_table_lean.as_ref(),
            r.composition_member_names_lean.as_ref(),
        ) else {
            continue;
        };
        composition_claims.push(format!(
            "({{ exportName := \"{name}\", carrier := {carrier}, hostTable := {host_table}, memberNames := {member_names}, obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.CompositionClaim)",
            name = r.name,
            carrier = r.carrier,
        ));
        let mut named_proof = "trivial".to_string();
        for member in r.composition_members.iter().rev() {
            let binding = format!(
                "({{ funcIdx := {}, codeIdx := {}, typeIdx := {}, codeEntry := {} }} : AverCert.WasmSlice.FuncBinding)",
                member.self_idx, member.code_idx, member.type_idx, member.lowered_code_entry_lean,
            );
            let member_proof = format!(
                "⟨rfl, ⟨({body}), ({bytes}), {binding}, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩",
                body = member.lowered_body_lean,
                bytes = member.lowered_code_entry_lean,
            );
            named_proof = format!("⟨{member_proof}, {named_proof}⟩");
        }
        composition_proofs.push(format!(
            "⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, {named_proof}⟩⟩⟩⟩⟩⟩"
        ));
    }
    let sym_claims = if sym_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", sym_claims.join(",\n  "))
    };
    let string_eq_claims = if string_eq_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", string_eq_claims.join(",\n  "))
    };
    let string_claims = if string_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", string_claims.join(",\n  "))
    };
    let construct_claims = if construct_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", construct_claims.join(",\n  "))
    };
    let recursion_claims = if recursion_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", recursion_claims.join(",\n  "))
    };
    let mutual_claims = if mutual_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", mutual_claims.join(",\n  "))
    };
    let verbatim_claims = if verbatim_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", verbatim_claims.join(",\n  "))
    };
    let int_dispatch_claims = if int_dispatch_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", int_dispatch_claims.join(",\n  "))
    };
    let field_projection_claims = if field_projection_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", field_projection_claims.join(",\n  "))
    };
    let composition_members = if composition_members.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", composition_members.join(",\n  "))
    };
    let composition_claims = if composition_claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", composition_claims.join(",\n  "))
    };
    let obligation_proof_count = sym_proofs.len()
        + string_eq_proofs.len()
        + string_proofs.len()
        + construct_proofs.len()
        + recursion_proofs.len()
        + mutual_proofs.len()
        + verbatim_proofs.len()
        + int_dispatch_proofs.len()
        + field_projection_proofs.len()
        + composition_proofs.len();
    let obligation_proof =
        (0..obligation_proof_count).fold("trivial".to_string(), |acc, _| format!("⟨rfl, {acc}⟩"));
    let sym_proof = sym_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let string_eq_proof = string_eq_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let string_proof = string_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let construct_claim_count = construct_proofs.len();
    let construct_claims_proof = construct_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    // `acceptedConstructFragments` also requires the concrete constructor
    // export-name list to be duplicate-free. Keep each decision local to one
    // cons cell to avoid reducing the whole artifact-sized claim list at once.
    let construct_nodup_proof = (0..construct_claim_count)
        .fold("List.nodup_nil".to_string(), |acc, _| {
            format!("List.nodup_cons.mpr ⟨by decide, {acc}⟩")
        });
    let construct_proof = format!("⟨{construct_claims_proof}, {construct_nodup_proof}⟩");
    let recursion_proof = recursion_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let mutual_proof = mutual_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let verbatim_proof = verbatim_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let int_dispatch_proof = int_dispatch_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let field_projection_proof = field_projection_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    let composition_claims_proof = composition_proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    // `acceptedCompositionFragments` conjoins the per-claim acceptance with the
    // artifact-wide member-coverage bound plus the manifest obligation-coverage
    // and export-name-uniqueness bounds (decidable `Bool = true` over the
    // concrete literals, each closed by `rfl`).
    let composition_proof = format!("⟨{composition_claims_proof}, rfl, rfl, rfl⟩");
    LeanExprFragmentArtifactClaims {
        sym_claims,
        string_eq_claims,
        string_claims,
        construct_claims,
        recursion_claims,
        mutual_claims,
        verbatim_claims,
        int_dispatch_claims,
        field_projection_claims,
        composition_members,
        composition_claims,
        obligation_proof,
        sym_proof,
        string_eq_proof,
        string_proof,
        construct_proof,
        recursion_proof,
        mutual_proof,
        verbatim_proof,
        int_dispatch_proof,
        field_projection_proof,
        composition_proof,
    }
}

#[allow(clippy::too_many_arguments)]
fn lean_artifact_data_literal(
    sym_claims: &str,
    string_eq_claims: &str,
    string_claims: &str,
    construct_claims: &str,
    recursion_claims: &str,
    mutual_claims: &str,
    verbatim_claims: &str,
    int_dispatch_claims: &str,
    field_projection_claims: &str,
    composition_members: &str,
    composition_claims: &str,
    module_envelope: &cert::ModuleEnvelopeFacts,
) -> String {
    let roots = lean_nat_list(module_envelope.closure.roots.iter().copied());
    let helpers = lean_nat_list(module_envelope.closure.helpers.iter().copied());
    let admitted = lean_nat_list(module_envelope.closure.admitted.iter().copied());
    let closure_claim = format!(
        "({{ roots := {roots}, helpers := {helpers}, admitted := {admitted} }} : AverCert.AcceptedArtifact.ClosureClaim)"
    );
    format!(
        "({{ modBytes := AverCert.ArtifactBytes.modBytes, modLen := AverCert.ArtifactBytes.modLen, manifest := AverCert.manifest, \
         symFragmentClaims := ({sym_claims} : List AverCert.AcceptedArtifact.SymFragmentClaim), \
         stringEqClaims := ({string_eq_claims} : List AverCert.AcceptedArtifact.StringEqClaim), \
         stringConcatClaims := ({string_claims} : List AverCert.AcceptedArtifact.StringConcatClaim), \
         constructClaims := ({construct_claims} : List AverCert.AcceptedArtifact.ConstructClaim), \
         recursionClaims := ({recursion_claims} : List AverCert.AcceptedArtifact.RecursionClaim), \
         mutualRecursionClaims := ({mutual_claims} : List AverCert.AcceptedArtifact.MutualRecursionClaim), \
         verbatimClaims := ({verbatim_claims} : List AverCert.AcceptedArtifact.VerbatimClaim), \
         intDispatchClaims := ({int_dispatch_claims} : List AverCert.AcceptedArtifact.IntDispatchClaim), \
         fieldProjectionClaims := ({field_projection_claims} : List AverCert.AcceptedArtifact.FieldProjectionClaim), \
         compositionMembers := ({composition_members} : List AverCert.AcceptedArtifact.CompositionMemberClaim), \
         compositionClaims := ({composition_claims} : List AverCert.AcceptedArtifact.CompositionClaim), \
         closureFuel := {closure_fuel}, closureClaim := {closure_claim} }} : \
         AverCert.AcceptedArtifact.ArtifactData)",
        closure_fuel = module_envelope.closure_fuel
    )
}

fn lean_fragment_acceptance_proof_block(
    witness: &LeanExprFragmentArtifactClaims,
    indent: &str,
) -> String {
    format!(
        concat!(
            "{indent}dsimp [AverCert.AcceptedArtifact.acceptedFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedSymFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedStringEqFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedStringConcatFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedConstructFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedRecursionFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedMutualRecursionFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedVerbatimFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedIntDispatchFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedFieldProjectionFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedCompositionFragments,\n",
            "{indent}  AverCert.AcceptedArtifact.acceptedWholeModule,\n",
            "{indent}  AverCert.AcceptedArtifact.symFragmentClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.symFragmentClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.symFragmentPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.stringEqClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.stringEqClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.stringEqPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.stringEqPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.stringConcatClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.stringConcatClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.stringConcatPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.stringConcatPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.constructClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.constructClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.constructPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.constructPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.recursionClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.recursionClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.recursionPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.recursionPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.mutualRecursionClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.mutualRecursionClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.mutualPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.mutualPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.verbatimClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.verbatimClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.verbatimPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.verbatimPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.intDispatchClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.intDispatchClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionFuncTable,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionMemberBinding,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionNamedMembersAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionMemberPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionMemberForName,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionClosureBound,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionEdges,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionPlanCallees,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionEdgesDescend,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionReachClosure,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionReachStep,\n",
            "{indent}  AverCert.AcceptedArtifact.compositionEdgeLookup,\n",
            "{indent}  AverCert.AcceptedArtifact.stringListNodup,\n",
            "{indent}  AverCert.AcceptedArtifact.stringListSetEq,\n",
            "{indent}  AverCert.AcceptedArtifact.intDispatchCanonicalHost,\n",
            "{indent}  AverCert.AcceptedArtifact.intDispatchCanonicalSlots,\n",
            "{indent}  AverCert.AcceptedArtifact.intDispatchPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.intDispatchPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.fieldProjectionClaimsAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.fieldProjectionClaimAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.fieldProjectionPlanForExport,\n",
            "{indent}  AverCert.AcceptedArtifact.fieldProjectionPlanAccepted,\n",
            "{indent}  AverCert.AcceptedArtifact.exprFragmentPlanAccepted,\n",
            "{indent}  AverCert.ExprFragmentAccepted.accepted]\n",
            "{indent}exact ⟨{sym_proof}, ⟨{string_eq_proof}, ⟨{string_proof}, ⟨{construct_proof}, ⟨{recursion_proof}, ⟨⟨{mutual_proof}, rfl⟩, ⟨{verbatim_proof}, ⟨{int_dispatch_proof}, ⟨{field_projection_proof}, ⟨{composition_proof}, ⟨rfl, rfl, rfl, rfl⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩\n"
        ),
        indent = indent,
        sym_proof = witness.sym_proof,
        string_eq_proof = witness.string_eq_proof,
        string_proof = witness.string_proof,
        construct_proof = witness.construct_proof,
        recursion_proof = witness.recursion_proof,
        mutual_proof = witness.mutual_proof,
        verbatim_proof = witness.verbatim_proof,
        int_dispatch_proof = witness.int_dispatch_proof,
        field_projection_proof = witness.field_projection_proof,
        composition_proof = witness.composition_proof
    )
}

fn lean_expr_fragment_obligation_acceptance_pins(
    rederived: &[cert::RederivedObligation],
    host_table_lean: &str,
    struct_table_lean: &str,
    module_envelope: &cert::ModuleEnvelopeFacts,
) -> String {
    let witness = lean_expr_fragment_artifact_claims(rederived, host_table_lean, struct_table_lean);
    let proof_block = lean_fragment_acceptance_proof_block(&witness, "  ");
    let artifact = lean_artifact_data_literal(
        &witness.sym_claims,
        &witness.string_eq_claims,
        &witness.string_claims,
        &witness.construct_claims,
        &witness.recursion_claims,
        &witness.mutual_claims,
        &witness.verbatim_claims,
        &witness.int_dispatch_claims,
        &witness.field_projection_claims,
        &witness.composition_members,
        &witness.composition_claims,
        module_envelope,
    );
    format!(
        concat!(
            "-- Fragment artifact data: accepted raw artifact bytes + source/raw plans\n",
            "-- are tied to the schema obligations used by `Final.cert`.\n",
            "example : (AverCert.AcceptedArtifact.acceptedFragments\n",
            "    {artifact}) := by\n",
            "{proof_block}"
        ),
        artifact = artifact,
        proof_block = proof_block
    )
}

fn lean_accepted_artifact_witness(
    rederived: &[cert::RederivedObligation],
    host_table_lean: &str,
    struct_table_lean: &str,
    module_envelope: &cert::ModuleEnvelopeFacts,
    mode: WitnessMode,
) -> String {
    let witness = lean_expr_fragment_artifact_claims(rederived, host_table_lean, struct_table_lean);
    let artifact = lean_artifact_data_literal(
        &witness.sym_claims,
        &witness.string_eq_claims,
        &witness.string_claims,
        &witness.construct_claims,
        &witness.recursion_claims,
        &witness.mutual_claims,
        &witness.verbatim_claims,
        &witness.int_dispatch_claims,
        &witness.field_projection_claims,
        &witness.composition_members,
        &witness.composition_claims,
        module_envelope,
    );
    let checker_proof = (mode == WitnessMode::Diagnostic).then(|| format!(
        concat!(
            "  dsimp [AverCert.AcceptedArtifact.accepted,\n",
            "    AverCert.AcceptedArtifact.subjectMatchesArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.expectedArtifactRoot,\n",
            "    AverCert.AcceptedArtifact.fragmentClaimObligationsInManifest,\n",
            "    AverCert.AcceptedArtifact.claimObligations,\n",
            "    AverCert.AcceptedArtifact.claimObligationsInManifest,\n",
            "    AverCert.AcceptedArtifact.claimObligationExports,\n",
            "    AverCert.AcceptedArtifact.claimsMatchManifest,\n",
            "    AverCert.AcceptedArtifact.decodedNonExprFacts,\n",
            "    AverCert.AcceptedArtifact.decodedNonExprClaimFacts,\n",
            "    AverCert.AcceptedArtifact.decodedStringHostRoles,\n",
            "    AverCert.AcceptedArtifact.stringEqCanonicalHost,\n",
            "    AverCert.AcceptedArtifact.stringConcatCanonicalHost,\n",
            "    AverCert.AcceptedArtifact.decodedClaims,\n",
            "    AverCert.AcceptedArtifact.decodedObligationFacts,\n",
            "    AverCert.AcceptedArtifact.decodedCodeAtAll,\n",
            "    AverCert.AcceptedArtifact.decodedCodeAt,\n",
            "    AverCert.AcceptedArtifact.decodedConstructStructFields,\n",
            "    AverCert.AcceptedArtifact.decodedProjectionStructFields,\n",
            "    AverCert.AcceptedArtifact.decodedCompositionClaims,\n",
            "    AverCert.AcceptedArtifact.decodedCompositionNames,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimPlanPairs,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimEncodedPlanPairs,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimEncodedPlanPair?,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.stringEqManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimSymPlanPairs,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.stringConcatManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimSymPlanPairs,\n",
            "    AverCert.AcceptedArtifact.constructClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.constructManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.constructClaimSymPlanPairs,\n",
            "    AverCert.AcceptedArtifact.recursionClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.recursionManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.mutualRecursionClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.mutualManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.verbatimClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.verbatimManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.intDispatchClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.intDispatchManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionClaimExportNames,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionManifestPlanNames,\n",
            "    AverCert.AcceptedArtifact.compositionMemberPlanPairs,\n",
            "    AverCert.AcceptedArtifact.acceptedFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedSymFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedStringEqFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedStringConcatFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedConstructFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedRecursionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedMutualRecursionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedVerbatimFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedIntDispatchFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedFieldProjectionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedCompositionFragments,\n",
            "    AverCert.AcceptedArtifact.acceptedWholeModule,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.symFragmentClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.symFragmentPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.stringEqClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.stringEqPlanForExport,\n",
            "    AverCert.AcceptedArtifact.stringEqPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.stringConcatClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.stringConcatPlanForExport,\n",
            "    AverCert.AcceptedArtifact.stringConcatPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.constructClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.constructClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.constructPlanForExport,\n",
            "    AverCert.AcceptedArtifact.constructPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.recursionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.recursionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.recursionPlanForExport,\n",
            "    AverCert.AcceptedArtifact.recursionPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.mutualRecursionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.mutualRecursionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.mutualPlanForExport,\n",
            "    AverCert.AcceptedArtifact.mutualPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.verbatimClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.verbatimClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.verbatimPlanForExport,\n",
            "    AverCert.AcceptedArtifact.verbatimPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.intDispatchClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.intDispatchClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.intDispatchPlanForExport,\n",
            "    AverCert.AcceptedArtifact.intDispatchPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionPlanForExport,\n",
            "    AverCert.AcceptedArtifact.fieldProjectionPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionClaimsAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionClaimAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionFuncTable,\n",
            "    AverCert.AcceptedArtifact.compositionMemberBinding,\n",
            "    AverCert.AcceptedArtifact.compositionNamedMembersAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionMemberPlanAccepted,\n",
            "    AverCert.AcceptedArtifact.compositionMemberForName,\n",
            "    AverCert.AcceptedArtifact.compositionClosureBound,\n",
            "    AverCert.AcceptedArtifact.compositionEdges,\n",
            "    AverCert.AcceptedArtifact.compositionPlanCallees,\n",
            "    AverCert.AcceptedArtifact.compositionEdgesDescend,\n",
            "    AverCert.AcceptedArtifact.compositionReachClosure,\n",
            "    AverCert.AcceptedArtifact.compositionReachStep,\n",
            "    AverCert.AcceptedArtifact.compositionEdgeLookup,\n",
            "    AverCert.AcceptedArtifact.stringListNodup,\n",
            "    AverCert.AcceptedArtifact.stringListSetEq,\n",
            "    AverCert.AcceptedArtifact.intDispatchCanonicalHost,\n",
            "    AverCert.AcceptedArtifact.intDispatchCanonicalSlots,\n",
            "    AverCert.AcceptedArtifact.exprFragmentPlanAccepted,\n",
            "    AverCert.ExprFragmentAccepted.accepted]\n",
            "  exact ⟨{final_witness}, ⟨rfl, ⟨{obligation_proof}, ⟨⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, ⟨rfl, rfl⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩, ⟨⟨AverCertChecker.decodedHostRoles, ⟨AverCertChecker.decodedStringHostRoles, by repeat' constructor⟩⟩, ⟨{sym_proof}, ⟨{string_eq_proof}, ⟨{string_proof}, ⟨{construct_proof}, ⟨{recursion_proof}, ⟨⟨{mutual_proof}, rfl⟩, ⟨{verbatim_proof}, ⟨{int_dispatch_proof}, ⟨{field_projection_proof}, ⟨{composition_proof}, ⟨rfl, rfl, rfl, rfl⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩⟩\n"
        ),
        final_witness = FINAL_WITNESS_THEOREM,
        obligation_proof = witness.obligation_proof,
        sym_proof = witness.sym_proof,
        string_eq_proof = witness.string_eq_proof,
        string_proof = witness.string_proof,
        construct_proof = witness.construct_proof,
        recursion_proof = witness.recursion_proof,
        mutual_proof = witness.mutual_proof,
        verbatim_proof = witness.verbatim_proof,
        int_dispatch_proof = witness.int_dispatch_proof,
        field_projection_proof = witness.field_projection_proof,
        composition_proof = witness.composition_proof
    ));
    let diagnostic_mirror = checker_proof.map_or_else(String::new, |checker_proof| {
        format!(
            concat!(
                "-- Checker-owned mirror proof kept as a narrow diagnostic. The axiom\n",
                "-- audit below is rooted at the artifact-carried proof, after the data\n",
                "-- pin above has checked.\n",
                "example : AverCert.AcceptedArtifact.accepted {artifact} := by\n",
                "{checker_proof}\n\n"
            ),
            artifact = artifact,
            checker_proof = checker_proof
        )
    });
    format!(
        concat!(
            "-- Artifact-carried data pin: the cert-supplied `Artifact.lean` data\n",
            "-- must be exactly the checker-reconstructed artifact literal.\n",
            "example : AverCert.Artifact.data = {artifact} := rfl\n\n",
            "{diagnostic_mirror}",
            "-- Whole-artifact acceptance root carried by the artifact itself. The\n",
            "-- checker only accepts it after the data pin and final-theorem\n",
            "-- ascription above have checked.\n",
            "def {witness_theorem} : AverCert.AcceptedArtifact.accepted\n",
            "    AverCert.Artifact.data := AverCert.Artifact.certificate\n"
        ),
        witness_theorem = WITNESS_THEOREM,
        artifact = artifact,
        diagnostic_mirror = diagnostic_mirror
    )
}

/// The Lean file the checker authors at verify time. `sha` is what the checker
/// computed from the artifact bytes; `cands` are the charset-gated JSON report
/// candidates. Every claim is a `rfl` against `AverCert.manifest` (or the final
/// theorem's type / the kernel axiom collector), so a lying JSON, a rebound
/// hash, a weakened theorem, or a smuggled axiom all make this file fail to
/// check — and THAT (the process exit code) is the only verdict channel.
#[allow(clippy::too_many_arguments)]
fn checker_witness(
    sha: &str,
    cands: &Candidates,
    rederived: &[cert::RederivedObligation],
    derived_contracts: &[String],
    host_table_lean: &str,
    struct_table_lean: &str,
    module_envelope: &cert::ModuleEnvelopeFacts,
    mode: WitnessMode,
) -> String {
    // Count is the JSON-claimed number of certified exports, verified by `rfl`
    // against `obligations.length` (so a JSON claiming more or fewer than the
    // manifest fails closed, unchanged from before). The export NAMES the
    // obligations are pinned to come from the BYTES (the re-derived export
    // section), never the JSON, so a producer cannot relabel a byte-bound body
    // as a different (uncertified) export.
    let n = cands.names.len();
    let rederived_names: Vec<String> = rederived.iter().map(|r| r.name.clone()).collect();
    let names = lean_str_list(&rederived_names);
    let json_contracts = lean_str_list(&cands.contracts);
    let byte_contracts = lean_str_list(derived_contracts);
    let json_declared_uncertified = lean_string_pair_list(&cands.declared_uncertified);
    let certified_name_set = rederived_names
        .iter()
        .collect::<std::collections::BTreeSet<_>>();
    let byte_declared_names = module_envelope
        .exports
        .iter()
        .filter(|export| !certified_name_set.contains(&export.name))
        .map(|export| export.name.clone())
        .collect::<Vec<_>>();
    let byte_declared_names = lean_str_list(&byte_declared_names);
    let json_capabilities = lean_string_pair_list(&cands.capabilities);
    let byte_capabilities = lean_string_pair_list(&module_envelope.capabilities);
    let json_start = cands
        .start
        .map(|index| format!("some {index}"))
        .unwrap_or_else(|| "none".to_string());
    let byte_start = module_envelope
        .start
        .map(|index| format!("some {index}"))
        .unwrap_or_else(|| "none".to_string());
    let option_nat = |index: Option<u32>| {
        index
            .map(|index| format!("some {index}"))
            .unwrap_or_else(|| "none".to_string())
    };
    let json_host_role_table = format!(
        "({{ box := {}, add := {}, sub := {} }} : CertDecode.AddSub.Roles)",
        option_nat(cands.host_role_table.0),
        option_nat(cands.host_role_table.1),
        option_nat(cands.host_role_table.2),
    );
    let json_string_host_roles = format!(
        "[{}]",
        cands
            .string_host_roles
            .iter()
            .map(|(index, role)| {
                let role = match role {
                    cert::StringHostRole::Eq => ".eq",
                    cert::StringHostRole::Concat => ".concat",
                };
                format!("({index}, {role})")
            })
            .collect::<Vec<_>>()
            .join(", ")
    );
    let profile = &cands.profile;
    let abi = &cands.abi;
    let artifact_root = cert::ARTIFACT_CERTIFICATE_ROOT;
    let json_policies = format!(
        "[{}]",
        cands
            .policies
            .iter()
            .map(|p| p.lean_value())
            .collect::<Vec<_>>()
            .join(", ")
    );
    let byte_policies = format!(
        "[{}]",
        rederived
            .iter()
            .map(|r| r.policy.lean_value())
            .collect::<Vec<_>>()
            .join(", ")
    );
    let render_termination = |witness: Option<cert::TerminationWitness>| match witness {
        Some(w) => format!("some {}", w.lean_value()),
        None => "none".to_string(),
    };
    let json_terminations = format!(
        "[{}]",
        cands
            .termination_witnesses
            .iter()
            .copied()
            .map(render_termination)
            .collect::<Vec<_>>()
            .join(", ")
    );
    let byte_terminations = format!(
        "[{}]",
        rederived
            .iter()
            .map(|r| render_termination(r.termination_witness))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let expr_fragment_plans = lean_expr_fragment_plan_pairs(rederived);
    let sym_fragment_plans = lean_sym_fragment_plan_pairs(rederived);
    let string_eq_plans = lean_string_eq_plan_pairs(rederived);
    let string_concat_plans = lean_string_concat_plan_pairs(rederived);
    let construct_plans = lean_construct_plan_pairs(rederived);
    let recursion_plans = lean_recursion_plan_pairs(rederived);
    let mutual_plans = lean_mutual_plan_pairs(rederived);
    let verbatim_plans = lean_verbatim_plan_pairs(rederived);
    let int_dispatch_plans = lean_int_dispatch_plan_pairs(rederived);
    let field_projection_plans = lean_field_projection_plan_pairs(rederived);
    let composition_plans = lean_composition_plan_pairs(rederived);
    let sym_fragment_encoded_plans = lean_sym_fragment_encoded_plan_pairs(rederived);
    let expr_fragment_lower_pins = lean_expr_fragment_lower_pins(rederived);
    let expr_fragment_code_entry_pins = lean_expr_fragment_code_entry_pins(rederived);
    let expr_fragment_wasm_slice_pins = lean_expr_fragment_wasm_slice_pins(rederived);
    let expr_fragment_func_binding_pins = lean_expr_fragment_func_binding_pins(rederived);
    let expr_fragment_accepted_pins = lean_expr_fragment_accepted_pins(rederived);
    let expr_fragment_obligation_acceptance_pins = if mode == WitnessMode::Diagnostic {
        lean_expr_fragment_obligation_acceptance_pins(
            rederived,
            host_table_lean,
            struct_table_lean,
            module_envelope,
        )
    } else {
        String::new()
    };
    let accepted_artifact_witness = lean_accepted_artifact_witness(
        rederived,
        host_table_lean,
        struct_table_lean,
        module_envelope,
        mode,
    );
    // Semantic-face bindings: pin each obligation's typed `Dom`/`Cod`/`domRepr`/
    // `codRepr` to the STANDARD form its BYTE-derived class implies, and prove
    // every domain is inhabited. These are the faces the schema-v3 checker did
    // not pin, so a manifest weakening them (`Dom := Empty`, `codRepr := True`,
    // `domRepr := False`, or a nerfed arity) made `holds` vacuously true; each
    // now fails a kernel `rfl`/`HEq.rfl` and the file does not check.
    let face_section = face_bindings(rederived);
    let whitelist = AXIOM_WHITELIST
        .iter()
        .map(|a| format!("`{a}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "-- Authored by `aver cert verify`, NOT shipped in the certificate.\n\
         import Lean\n\
         import Schema\n\
         import PlanCheck\n\
         import PlanLower\n\
         import PlanBytes\n\
         import WasmSlice\n\
         import ExprFragmentAccepted\n\
         import AcceptedArtifact\n\
         import ArtifactBytes\n\
         import Module\n\
         import Manifest\n\
         import Final\n\
         import Artifact\n\
         open CertPrelude AverCert.Schema\n\
         -- Whole-module closure checking reduces the embedded big-Nat bytes.\n\
         -- This explicit depth budget changes only kernel reduction limits.\n\
         set_option maxRecDepth 200000\n\
         set_option linter.unusedSimpArgs false\n\
         set_option linter.unusedVariables false\n\n\
         -- Count (the verdict bit): the kernel confirms EXACTLY this many\n\
         -- obligations. A JSON claiming more or fewer fails this `rfl`.\n\
         example : AverCert.manifest.obligations.length = {n} := rfl\n\
         -- Names: the obligation export list and the subject export list both\n\
         -- equal the BYTE-DERIVED export names (from the module's export\n\
         -- section, re-derived by the disassembler), or a `rfl` fails — so a\n\
         -- byte-bound body cannot be relabelled under a different export name.\n\
         example : AverCert.manifest.obligations.map (fun o => o.export_) = {names} := rfl\n\
         example : AverCert.manifest.subject.exports = {names} := rfl\n\
         -- Whole-module interface declarations: JSON is pinned to the Lean\n\
         -- manifest, while names/import pairs/start status are independently\n\
         -- re-derived from the exact artifact bytes. Reasons are display-only;\n\
         -- the security-relevant uncertified name set is byte-bound below.\n\
         example : AverCert.manifest.subject.declaredUncertified = {json_declared_uncertified} := rfl\n\
         example : AverCert.manifest.subject.declaredUncertified.map (fun entry => entry.1) = {byte_declared_names} := rfl\n\
         example : AverCert.manifest.subject.capabilities = {json_capabilities} := rfl\n\
         example : AverCert.manifest.subject.capabilities = {byte_capabilities} := rfl\n\
         example : AverCert.manifest.subject.start = {json_start} := rfl\n\
         example : AverCert.manifest.subject.start = {byte_start} := rfl\n\
         -- Host-role table: the JSON candidate is pinned to the manifest here;\n\
         -- accepted-artifact independently recomputes box/add/sub from the\n\
         -- checker-regenerated module bytes inside the Lean kernel.\n\
         example : AverCert.manifest.subject.hostRoleTable = {json_host_role_table} := rfl\n\
         example : AverCert.manifest.subject.stringHostRoles = {json_string_host_roles} := rfl\n\
         -- Contracts: the JSON candidate must match the proven manifest, and\n\
         -- the proven manifest must also match the BYTE-DERIVED contract list.\n\
         -- JSON-only padding and manifest+JSON deletion are therefore both\n\
         -- declined; the final report uses only the byte-derived list.\n\
         example : AverCert.manifest.subject.contracts = {json_contracts} := rfl\n\
         example : AverCert.manifest.subject.contracts = {byte_contracts} := rfl\n\
         -- Policy/witness axis: JSON is only a candidate; both lists must also
         -- equal the policy and termination witness independently re-derived
         -- from the byte-classified family. The witness remains outside every
         -- plan hash.
         example : AverCert.manifest.obligations.map (fun o => o.policy) = {json_policies} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.policy) = {byte_policies} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.termination?) = {json_terminations} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.termination?) = {byte_terminations} := rfl\n\
         example : AverCert.manifest.subject.profile = \"{profile}\" := rfl\n\
         example : AverCert.manifest.subject.abi = \"{abi}\" := rfl\n\
         example : AverCert.manifest.subject.artifactRoot = \"{artifact_root}\" := rfl\n\n\
         -- Source fragment plans: the manifest's Lean-data source plans are\n\
         -- pinned to checker-rendered `SymRawPlan` terms reconstructed from\n\
         -- sidecars or byte-derived string witnesses. Expr-subset source plans\n\
         -- encode to `some ExprFragmentRawPlan`; String.eq/concat source plans\n\
         -- deliberately encode to `none` and are bound through stringEqPlans\n\
         -- or stringConcatPlans.\n\
         example : AverCert.manifest.symFragmentPlans = {sym_fragment_plans} := rfl\n\
         example : AverCert.manifest.symFragmentPlans.all (fun p => AverCert.PlanCheck.checkSymRawPlan p.2) = true := rfl\n\
         example : AverCert.manifest.symFragmentPlans.map (fun p => (p.1, AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan {host_table_lean} {struct_table_lean} p.2)) = {sym_fragment_encoded_plans} := rfl\n\n\
         -- String.eq source plans: the manifest's Lean-data equality plans\n\
         -- are pinned to checker-rendered `StringEqRawPlan` terms reconstructed\n\
         -- from sidecars that already passed hash checks and byte-derived\n\
         -- String.eq helper-shape validation.\n\
         example : AverCert.manifest.stringEqPlans = {string_eq_plans} := rfl\n\
         example : AverCert.manifest.stringEqPlans.all (fun p => AverCert.PlanCheck.checkStringEqRawPlan p.2) = true := rfl\n\n\
         -- String.concat source plans: the manifest's Lean-data string plans\n\
         -- are pinned to checker-rendered `StringConcatRawPlan` terms reconstructed\n\
         -- from sidecars that already passed hash checks and byte-derived\n\
         -- String.concat helper-shape validation.\n\
         example : AverCert.manifest.stringConcatPlans = {string_concat_plans} := rfl\n\
         example : AverCert.manifest.stringConcatPlans.all (fun p => AverCert.PlanCheck.checkStringConcatRawPlan p.2) = true := rfl\n\n\
         -- ADT constructor target plans: the manifest's Lean-data constructor\n\
         -- plans are pinned to checker-rendered `ConstructRawPlan` terms\n\
         -- reconstructed from byte-derived constructor shapes and matched\n\
         -- against their source `SymRawPlan` witnesses.\n\
         example : AverCert.manifest.constructPlans = {construct_plans} := rfl\n\
         example : AverCert.manifest.constructPlans.all (fun p => AverCert.PlanCheck.checkConstructRawPlan p.2) = true := rfl\n\n\
         -- Fuel-recursion byte-origin plans: the manifest's Lean-data recursion\n\
         -- plans are pinned to checker-rendered `RecursionRawPlan` terms\n\
         -- reconstructed from the byte-derived recursion holes, and each passes\n\
         -- the audited Lean structural checker.\n\
         example : AverCert.manifest.recursionPlans = {recursion_plans} := rfl\n\
         example : AverCert.manifest.recursionPlans.all (fun p => AverCert.PlanCheck.checkRecursionRawPlan p.2) = true := rfl\n\n\
         -- Mutual-recursion byte-origin plans: the manifest's Lean-data mutual\n\
         -- plans are pinned to checker-rendered `MutualRawPlan` terms\n\
         -- reconstructed from the byte-derived SCC holes, and each passes the\n\
         -- audited Lean structural checker.\n\
         example : AverCert.manifest.mutualPlans = {mutual_plans} := rfl\n\
         example : AverCert.manifest.mutualPlans.all (fun p => AverCert.PlanCheck.checkMutualRawPlan p.2) = true := rfl\n\n\
         -- Verbatim byte-origin plans: the manifest's Lean-data verbatim\n\
         -- `ref.test`-dispatch plans are pinned to checker-rendered\n\
         -- `VerbatimRawPlan` terms reconstructed from the byte-derived match\n\
         -- holes, and each passes the audited Lean structural checker.\n\
         example : AverCert.manifest.verbatimPlans = {verbatim_plans} := rfl\n\
         example : AverCert.manifest.verbatimPlans.all (fun p => AverCert.PlanCheck.checkVerbatimRawPlan p.2) = true := rfl\n\n\
         -- Int-face dispatch byte-origin plans: the manifest's Lean-data\n\
         -- `int-dispatch-v1` plans are pinned to checker-rendered\n\
         -- `IntDispatchRawPlan` terms reconstructed from the byte-derived match\n\
         -- holes, and each passes the audited Lean structural checker.\n\
         example : AverCert.manifest.intDispatchPlans = {int_dispatch_plans} := rfl\n\
         example : AverCert.manifest.intDispatchPlans.all (fun p => AverCert.PlanCheck.checkIntDispatchRawPlan p.2) = true := rfl\n\n\
         -- Bare field projections: only the selected field is plan data; the
         -- checker reconstructs the remaining type context from module bytes.
         example : AverCert.manifest.fieldProjectionPlans = {field_projection_plans} := rfl\n\n\
         -- Composition plans carry only self-sum/chain shape and callee names;
         -- all numeric bindings are reconstructed from Wasm exports.\n\
         example : AverCert.manifest.compositionPlans = {composition_plans} := rfl\n\
         example : AverCert.manifest.compositionPlans.all (fun p => AverCert.PlanCheck.checkCompositionRawPlan p.2) = true := rfl\n\n\
         -- Expr-fragment raw plans: the manifest's Lean-data representation plans\n\
         -- are pinned to checker-rendered `ExprFragmentRawPlan` terms derived\n\
         -- from checked source sidecars, or from representation fallback sidecars\n\
         -- when no source plan can describe the fragment. In both cases they\n\
         -- already passed hash, type/refinement and canonical code-entry equality\n\
         -- against the artifact bytes.\n\
         example : AverCert.manifest.exprFragmentPlans = {expr_fragment_plans} := rfl\n\n\
         -- The manifest plans also pass the audited Lean-side structural\n\
         -- checker. This is not the v2 byte-level `LowersCodeEntry` proof yet,\n\
         -- but it makes `RawPlan -> checked structural plan` a kernel-checked\n\
         -- artifact invariant.\n\
         example : AverCert.manifest.exprFragmentPlans.all (fun p => AverCert.PlanCheck.checkExprFragmentRawPlan p.2) = true := rfl\n\n\
         -- Expr-fragment plan lowering: for every checked expr-fragment plan,\n\
         -- the audited Lean lowerer reconstructs the exact `WInstr` body that\n\
         -- the verifier rendered after canonical code-entry equality against\n\
         -- the artifact bytes. This is still WInstr-level, not raw byte-level\n\
         -- `LowersCodeEntry`, but it moves plan-to-semantics lowering into Lean.\n\
         {expr_fragment_lower_pins}\n\
         -- Expr-fragment byte lowering: the audited Lean byte lowerer also\n\
         -- reconstructs the exact canonical code-entry bytes accepted by the\n\
         -- Rust verifier. This still relies on Rust for module slicing and for\n\
         -- comparing those bytes to the real code section, but the plan byte\n\
         -- encoder itself is now hash-pinned Lean code.\n\
         {expr_fragment_code_entry_pins}\n\
         -- Expr-fragment byte origin: the checker regenerated `ArtifactBytes.lean`\n\
         -- from the actual artifact bytes it read, and the audited Lean slicer\n\
         -- finds each expr-fragment export's exact code-entry bytes in that\n\
         -- module. This is the first relevant-subset in-kernel byte-origin\n\
         -- check; full Wasm validation remains future work.\n\
         {expr_fragment_wasm_slice_pins}\n\
         -- Expr-fragment function binding: the audited Lean slicer also routes\n\
         -- each export through its function index, defined-code index and\n\
         -- function-section type index before exposing the code-entry bytes.\n\
         {expr_fragment_func_binding_pins}\n\
         -- Expr-fragment aggregate acceptance: the separate pins above are\n\
         -- also exposed as one audited predicate. This is the v2 landing shape\n\
         -- for replacing loose examples with an `AcceptedArtifact` theorem.\n\
         {expr_fragment_accepted_pins}\n\
         -- Fragment artifact bridge: raw artifact bytes + source/raw plan +\n\
         -- schema obligation imply the aggregate fragment acceptance\n\
         -- predicate, with body/code-entry/function binding kept as internal\n\
         -- witnesses rather than extra trusted parameters.\n\
         {expr_fragment_obligation_acceptance_pins}\n\
         -- Hash binding: the sha the checker computed from the artifact bytes.\n\
         example : AverCert.manifest.subject.artifactHash = \"{sha}\" := rfl\n\
         example : CertModule.wasmSha256 = \"{sha}\" := rfl\n\n\
         -- Decode-once host-role equality: this separate declaration keeps the\n\
         -- module scan out of every obligation fold and gives it an independent\n\
         -- heartbeat budget before the artifact conjunction reuses the result.\n\
         theorem AverCertChecker.decodedHostRoles : CertDecode.AddSub.roleTable AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen = some AverCert.manifest.subject.hostRoleTable := by change AverCert.AcceptedArtifact.decodedHostRoleTable AverCert.Artifact.data; dsimp [AverCert.AcceptedArtifact.decodedHostRoleTable, AverCert.Artifact.data]; rfl\n\n\
         theorem AverCertChecker.decodedStringHostRoles : CertDecode.StringHost.roleTable AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen = some AverCert.manifest.subject.stringHostRoles := by change AverCert.AcceptedArtifact.decodedStringHostRoles AverCert.Artifact.data; dsimp [AverCert.AcceptedArtifact.decodedStringHostRoles, AverCert.Artifact.data]; rfl\n\n\
         -- Artifact-decode / checked-plan bindings: non-expression CODE and\n\
         -- CARRIER (plus consumed struct-field counts) are computed from raw\n\
         -- ArtifactBytes by `CertDecode` inside the accepted-artifact conjunct.\n\
         -- Expression-fragment code/carrier retains its checked plan plus\n\
         -- canonical code-entry byte equality. SELF and HOST are no longer\n\
         -- checker-spliced: self is covered by `exportsAccounted`; box/add/sub\n\
         -- and both string roles are decoded from `ArtifactBytes`, while string\n\
         -- claims bind their complete host builders to classified role indices.\n\
         -- the accepted-artifact `exportsAccounted` conjunct already pins every\n\
         -- obligation's `(export name, func kind, self index)` into the byte-\n\
         -- decoded export section (`WasmSlice.enumExports`), so `self` is a\n\
         -- kernel computation over the module bytes, not a Rust-rendered literal.\n\
         -- `host`/`self` are among the fields `Obligation.holds` reasons about\n\
         -- (`wFuncN o.code (o.host add sub mul stringEq stringConcat) fuel o.self`), so a fabricated body,\n\
         -- a decoupled `code`/`self`/`carrier`, or a nerfed `host` that would\n\
         -- make `holds` vacuous all diverge from byte-bound kernel values and\n\
         -- fail a load-bearing kernel binding. There is no Rust-rendered\n\
         -- byte-fact equality left in this witness.\n\n\
         -- Semantic-face bindings: the typed `Dom`/`Cod`/`domRepr`/`codRepr` of\n\
         -- every obligation, pinned to the standard form of its byte-bound\n\
         -- class/checked plan, plus a `Nonempty Dom` proof. A manifest that weakens the face\n\
         -- (`Dom := Empty`, `codRepr := fun _ _ _ => True`, `domRepr := fun _ _ _ => False`,\n\
         -- a nerfed arity) fails one of these kernel checks.\n\
         {face_section}\n\
         -- Statement: force the final theorem's TYPE by ascription (no text match),\n\
         -- then wrap it in the artifact-level acceptance predicate. The axiom\n\
         -- audit below is collected from the artifact root, not the looser\n\
         -- schema-only theorem.\n\
         def {FINAL_WITNESS_THEOREM} : AverCert.Schema.Holds AverCert.manifest := AverCert.Final.cert\n\n\
         {accepted_artifact_witness}\n\n\
         -- Axiom whitelist, enforced by the kernel's own axiom collector over the\n\
         -- ascribed constant: full `Name` equality, not text. Any non-whitelisted\n\
         -- axiom (a smuggled `axiom evil`, `sorryAx`, `ofReduceBool`, ...) makes\n\
         -- this command throw, so the file does not check and verify declines.\n\
         -- `Lean.collectAxioms` / `Lean.Name` are fully qualified so a cert that\n\
         -- ships a root-level `def collectAxioms` shadow cannot be resolved here.\n\
         open Lean in\n\
         run_cmd do\n  \
         let allowed : List Lean.Name := [{whitelist}]\n  \
         let axs \u{2190} Lean.collectAxioms `{WITNESS_THEOREM}\n  \
         for a in axs do\n    \
         unless allowed.contains a do\n      \
         throwError s!\"non-whitelisted axiom: {{a}}\"\n"
    )
}

/// The semantic-face bindings spliced into the witness: one block per obligation
/// of standard-form `Nonempty Dom` / `Dom` / `Cod` / `domRepr` / `codRepr` pins,
/// keyed on its BYTE-derived class (via `RederivedObligation::face`). Every pin
/// is indexed into `manifest.obligations` so the block is robust to an
/// obligation count that diverges from the manifest. Empty when there are no
/// certified obligations.
#[allow(clippy::too_many_arguments)]
fn author_and_run_checker_witness(
    build_dir: &Path,
    sha: &str,
    cands: &Candidates,
    rederived: &[cert::RederivedObligation],
    derived_contracts: &[String],
    host_table_lean: &str,
    struct_table_lean: &str,
    module_envelope: &cert::ModuleEnvelopeFacts,
    mode: WitnessMode,
) -> Result<LakeOut, String> {
    let witness = checker_witness(
        sha,
        cands,
        rederived,
        derived_contracts,
        host_table_lean,
        struct_table_lean,
        module_envelope,
        mode,
    );
    std::fs::write(build_dir.join("CheckerWitness.lean"), witness).map_err(|e| {
        format!(
            "cannot write {} checker witness: {e}",
            if mode == WitnessMode::Diagnostic {
                "diagnostic"
            } else {
                "fast"
            }
        )
    })?;
    run_lake(build_dir, &["env", "lean", "CheckerWitness.lean"])
}

fn face_bindings(rederived: &[cert::RederivedObligation]) -> String {
    let mut s = String::new();
    for (i, r) in rederived.iter().enumerate() {
        s.push_str(&r.face.witness_pins(i));
    }
    s
}

/// A checker-owned temp build directory, removed on drop.
struct BuildDir {
    path: PathBuf,
}

impl BuildDir {
    fn new() -> Result<BuildDir, String> {
        let path = std::env::temp_dir().join(format!(
            "aver-certverify-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        std::fs::create_dir_all(&path).map_err(|e| format!("create checker build dir: {e}"))?;
        Ok(BuildDir { path })
    }
}

impl Drop for BuildDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
}

struct StoreBuildDir {
    path: PathBuf,
    remove_on_drop: std::cell::Cell<bool>,
}

impl StoreBuildDir {
    fn new(path: PathBuf) -> Result<Self, ()> {
        std::fs::create_dir(&path).map_err(|_| ())?;
        Ok(Self {
            path,
            remove_on_drop: std::cell::Cell::new(true),
        })
    }

    fn keep(&self) {
        self.remove_on_drop.set(false);
    }
}

impl Drop for StoreBuildDir {
    fn drop(&mut self) {
        if self.remove_on_drop.get() {
            let _ = std::fs::remove_dir_all(&self.path);
        }
    }
}

fn unique_nanos() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0)
}

fn write(dir: &Path, name: &str, content: &str) -> Result<(), String> {
    std::fs::write(dir.join(name), content).map_err(|e| format!("write {name}: {e}"))
}

struct LakeOut {
    status: std::process::ExitStatus,
    combined: String,
}

fn run_lake(build_dir: &Path, args: &[&str]) -> Result<LakeOut, String> {
    let out = Command::new("lake")
        .current_dir(build_dir)
        .args(args)
        .output()
        .map_err(|e| {
            format!(
                "could not run `lake {}`: {e} (is Lean/lake installed?)",
                args.join(" ")
            )
        })?;
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    Ok(LakeOut {
        status: out.status,
        combined,
    })
}

fn tail(s: &str, lines: usize) -> String {
    let all: Vec<&str> = s.lines().collect();
    let start = all.len().saturating_sub(lines);
    all[start..].join("\n")
}

/// Keep only printable ASCII for terminal display of UNTRUSTED strings (the
/// declined list from the JSON), so a hostile cert cannot inject ANSI escapes
/// or control characters into the report.
fn display_safe(s: &str) -> String {
    s.chars()
        .map(|c| if (' '..='~').contains(&c) { c } else { '?' })
        .collect()
}

/// Human-readable report. The CERTIFIED side is the trusted, kernel-confirmed
/// report (so it builds); the DECLINED side is read from the untrusted JSON
/// (declines carry no behavioral claim) and sanitized for display.
fn explain(artifact: &Path, cert_dir: &Path) -> Result<(), String> {
    let report = trusted_check(artifact, cert_dir)?;

    println!("{}", "Artifact certificate".bold());
    println!("  artifact: {}", artifact.display());
    println!("  pinned sha256: {}", report.artifact_hash);
    println!("  profile: {}    abi: {}", report.profile, report.abi);

    if report.exports.is_empty() {
        // Fail-closed parity with `verify`: a trust tool must not show a green
        // CERTIFIED header for a cert that makes no behavioral claims.
        println!(
            "\n{}",
            "NO CERTIFIED EXPORTS (admission only, no behavioral claims)"
                .yellow()
                .bold()
        );
    } else {
        println!("\n{}", "CERTIFIED".green().bold());
        println!("  {ARTIFACT_DECODE_LINE}");
    }
    for e in &report.exports {
        let level = if e.policy == "simulatesModelTotally" {
            "L3"
        } else {
            "L1"
        };
        println!("  {}  [{}]", e.name.cyan().bold(), level);
        println!("    {}", e.face);
        let policy_note = if e.policy == "simulatesModelTotally" {
            "emitted body returns a represented model result at checked measure fuel, conditional on the named contracts"
        } else {
            "emitted body partially simulates its model under the named contracts"
        };
        println!("    policy: {} ({policy_note})", e.policy);
        if report.contracts.is_empty() {
            println!("    runtime-contracts: (none)");
        } else {
            println!("    runtime-contracts:");
            for rc in &report.contracts {
                println!("      - {rc}");
            }
        }
    }

    // DECLINED: untrusted convenience only (no behavioral claim), sanitized.
    let manifest = read_manifest(cert_dir)?;
    let declined = manifest
        .get("source_level_only")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    println!("\n{}", "DECLINED".yellow().bold());
    if declined.is_empty() {
        println!("  (none)");
    }
    for d in &declined {
        let name = display_safe(d.get("name").and_then(Value::as_str).unwrap_or("?"));
        let reason = display_safe(d.get("reason").and_then(Value::as_str).unwrap_or("?"));
        println!("  {name} — {reason}");
    }

    // Same fail-closed exit contract as `verify`: an admission-only cert carries
    // no behavioral claim, so `explain`/`inspect` must not exit green either.
    if report.exports.is_empty() {
        std::process::exit(1);
    }
    Ok(())
}
