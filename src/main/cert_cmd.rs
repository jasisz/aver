//! `aver cert verify|explain` — the consumer side of `aver compile --certify`.
//!
//! `verify` is a fail-closed checker. The Lean/proof verdict comes only from
//! the exit code of the Lean toolchain over files the checker itself authored;
//! byte/plan prechecks are explicit Rust-side TCB. It assembles its OWN build
//! in a fresh, checker-owned temp directory from the audited
//! `Schema.lean` / `PlanCheck.lean` / `PlanLower.lean` / `PlanBytes.lean` /
//! `WasmSlice.lean` / `ExprFragmentAccepted.lean` / `CertPrelude.lean` this binary embeds, regenerates
//! `ArtifactBytes.lean` from the artifact bytes it read, copies the cert's
//! DATA-only Lean files, authors its own `lakefile.lean`, and builds with a
//! clean cache. It then writes a `CheckerWitness.lean` — which the checker, not
//! the cert, authors — that:
//!   * binds the sha256 the checker computed from the artifact bytes to the
//!     hashes the kernel-checked theorems talk about (`rfl`);
//!   * binds the certified-export names, contracts, profile and ABI the
//!     UNTRUSTED `cert-manifest.json` claims to the `AverCert.manifest` literal
//!     the final theorem is about (`rfl`) — a lying JSON makes a `rfl` fail;
//!   * forces the final theorem's TYPE to `Holds manifest` by ascription;
//!   * runs the kernel's own axiom collector (`Lean.collectAxioms`) on that
//!     ascribed constant and throws unless every axiom is on the whitelist
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
//! bytes with a vacuously-true `holds`. To close that, the checker derives each
//! obligation's `code`, `host`, `self`, `carrier` and consumed runtime contracts
//! from byte-bound facts: ordinary classes come from the audited disassembler,
//! while `expr-fragment-v1` code/face comes from a checked plan sidecar whose
//! canonical code-entry bytes must equal the real bytes. The checker splices
//! those values, fully expanded, into the witness — then pins them with
//! `rfl` against `manifest.obligations.map (·.code / ·.host / ·.self / ·.carrier)`.
//! Those are EXACTLY the fields `Obligation.holds` reasons about
//! (`wFuncN o.code (o.host add sub mul stringEq) fuel o.self`), so a fabricated body, a
//! decoupled `code`/`self`/`carrier`, or a nerfed `host` (which would make
//! `holds` vacuous) all diverge from the bytes and fail a `rfl` — the file does
//! not check and verify declines. The spliced terms are the checker's own
//! rendering over byte-bound data, never attacker text, and are fully expanded
//! so they do not reference the cert's `CertModule.*` defs (which an attacker
//! edits).
//! Expression-fragment plan sidecars are useful emitted metadata, never
//! authority. The checker parses the sidecar as an untrusted plan, typechecks it
//! against byte-derived function facts, canonically lowers it to raw wasm
//! code-entry bytes, and only then uses the checked plan to render the witness
//! `code`/semantic face for that obligation. A stale or forged sidecar therefore
//! fails before the checker-authored witness can certify it. The witness also
//! asks `WasmSlice.lean` to recover each expression export's code-entry bytes
//! from the checker-regenerated `ArtifactBytes.lean`, so the plan's canonical
//! bytes are tied to a narrow Lean-side slice of the actual module bytes.
//! `Module.lean` is never read as text for comparison. This is trusted via
//! inspection of the disassembler, not an in-kernel wasm decode proof; a full
//! kernel decoder is a deferred residual, and `model` (not byte-re-derivable for
//! a recursive reference function) remains a read declaration.
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

/// Kernel axioms a certificate is allowed to depend on. Anything else — most
/// importantly `sorryAx` (an admitted goal) or `ofReduceBool` (native-code
/// trust) — fails the check. Spliced into the witness as `Name` literals and
/// compared by full-name equality by `Lean.collectAxioms`.
const AXIOM_WHITELIST: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];

/// Constants the checker composes in its own witness file: `final` ascribes
/// `AverCert.Final.cert` to `Holds manifest`, then `accepted` wraps that theorem
/// in the artifact-level predicate used for the axiom audit.
const FINAL_WITNESS_THEOREM: &str = "AverCertChecker.final";
const WITNESS_THEOREM: &str = "AverCertChecker.accepted";

/// Lean source files the checker owns and never copies from the cert: the
/// audited trusted computing base (taken from this binary) plus the checker's
/// own build config and witness. A cert shipping files by these names has them
/// ignored.
const CHECKER_OWNED: [&str; 11] = [
    "Schema.lean",
    "PlanCheck.lean",
    "PlanLower.lean",
    "PlanBytes.lean",
    "WasmSlice.lean",
    "ExprFragmentAccepted.lean",
    "AcceptedArtifact.lean",
    "ArtifactBytes.lean",
    "CertPrelude.lean",
    "lakefile.lean",
    "CheckerWitness.lean",
];

/// Maximum length (bytes) of a JSON-supplied string spliced into the witness.
const MAX_CANDIDATE_LEN: usize = 200;

/// Emitted on a CERTIFIED verdict: every obligation's code/host/self/carrier was
/// pinned to checker-derived byte-bound values by `rfl` inside the kernel
/// witness. Ordinary classes come from the byte disassembler; `expr-fragment-v1`
/// code/face comes from a checked plan whose canonical code-entry bytes equal
/// the artifact bytes.
const ARTIFACT_DECODE_LINE: &str = "artifact-decode: each obligation's export name and code/host/self/carrier are kernel-pinned (rfl) to byte-bound checker values (disassembler, or checked expr-fragment plan with canonical code-entry equality and Lean byte-slice pin)";

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
    /// Runtime contracts as CLAIMED by the JSON. Used only for the witness
    /// binding against the proven manifest; the final byte-binding and report
    /// use the byte-derived list.
    contracts: Vec<String>,
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
                if let Ok(content) = std::fs::read_to_string(&path) {
                    out.push((name, content));
                }
            }
        }
    }
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
    let names = m
        .get("certified")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `certified`".to_string())?
        .iter()
        .map(|c| {
            c.get("name")
                .and_then(Value::as_str)
                .map(str::to_string)
                .ok_or_else(|| {
                    "cert-manifest.json `certified[]` entry is missing string field `name`"
                        .to_string()
                })
        })
        .collect::<Result<Vec<_>, _>>()?;

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

    let cands = Candidates {
        names,
        contracts,
        profile,
        abi,
    };
    for n in &cands.names {
        gate_candidate("certified export name", n)?;
    }
    for c in &cands.contracts {
        gate_candidate("runtime contract", c)?;
    }
    gate_candidate("profile", &cands.profile)?;
    gate_candidate("abi", &cands.abi)?;
    Ok(cands)
}

fn verify(artifact: &Path, cert_dir: &Path) -> Result<Verdict, String> {
    let report = trusted_check(artifact, cert_dir)?;
    let n = report.exports.len();
    let summary = format!(
        "{} ({} certified export{}, level {})",
        artifact.display(),
        n,
        if n == 1 { "" } else { "s" },
        cert::CERT_LEVEL,
    );
    if n == 0 {
        Ok(Verdict::NoExports(summary))
    } else {
        let faces = report
            .exports
            .iter()
            .map(|e| format!("{}  {}", e.name, e.face))
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
    let prelude_pin = manifest_str(&manifest, "prelude_sha256")?;
    let audited_prelude = cert::audited_prelude_sha();
    if prelude_pin != audited_prelude {
        return Err(format!(
            "prelude hash mismatch: certificate pins {prelude_pin}, checker expects {audited_prelude}"
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

    // 2. Report candidates from the untrusted JSON, each charset-gated on its
    //    decoded value so it is safe to splice as a Lean literal below.
    let cands = read_candidates(&manifest)?;

    // 2b. Re-derive the legacy certified obligations (code/host/self/carrier)
    //     from the hash-verified artifact bytes with the audited disassembler,
    //     deliberately excluding expr fragments. Expr fragments are admitted in
    //     the next step from checked plan sidecars plus canonical code-entry byte
    //     equality, then merged back by the actual byte-derived function order.
    //     These values are spliced into the checker witness below and pinned with
    //     `rfl` against `manifest.obligations`. If disassembly fails outright
    //     (not a wasm module, no box helper), decline here — before the witness —
    //     fail-closed.
    //     The model `.lean` files supply the combinator operator (`+`/`*`) that
    //     the bytes cannot distinguish for the bignum helpers; they are the same
    //     (untrusted) model the kernel witness proves the bytes against, so
    //     reading the operator here does not widen trust — `lake` rejects any
    //     mismatch. Only the `def X__fuel` operator is read; nothing is executed.
    let model_files = read_lean_files(cert_dir);
    let non_expr_cert = cert::rederive_certificate_without_expr_fragments(&bytes, &model_files)?;
    let mut rederived = non_expr_cert.obligations;
    let derived_contracts = non_expr_cert.contracts;

    // 2c. Expression-fragment sidecars are untrusted proof-carrying metadata.
    //     For this profile, the sidecar checked plan is the source for the
    //     witness `code` and semantic face, but only after verifier-owned
    //     canonical lowering matches the actual raw code-entry bytes. Unlike the
    //     old transitional overlay, the byte classifier no longer decides which
    //     expr fragments are in scope: the manifest names the sidecar witness,
    //     the checker validates it against the actual bytes, and the final list
    //     is sorted by byte-derived function order before the kernel witness
    //     binds it to `Manifest.lean`.
    rederived.extend(checked_fragment_sidecar_obligations(
        cert_dir, &manifest, &bytes,
    )?);
    rederived.sort_by_key(|r| r.func_order);
    reject_duplicate_rederived_func_orders(&rederived)?;

    // The re-derived export names come from the module's export section, which a
    // hostile producer controls via the bytes; gate them exactly like the JSON
    // candidates before they are spliced as Lean string literals in the witness.
    for r in &rederived {
        gate_candidate("re-derived export name", &r.name)?;
    }

    // 3. Assemble a checker-owned build. The audited schema + prelude come from
    //    THIS binary, never from the cert; the cert supplies only per-artifact
    //    DATA (Module/Manifest/Certificate/Final + the model modules). Each data
    //    file's name is gated (no lakefile-root injection) and its text scanned
    //    for code-executing tokens before it is staged. The cert's own lakefile
    //    / srcDir / `.lake` cache are never read.
    let build = assemble_build(cert_dir, &bytes)?;

    // 4. The assembled project must build under the pinned toolchain, from a
    //    clean cache (the fresh dir has no `.lake`).
    let b = run_lake(&build.path, &["build"])?;
    if !b.status.success() {
        return Err(format!(
            "certificate did not build (lake build failed):\n{}",
            tail(&b.combined, 20)
        ));
    }

    // 5. Kernel witness authored BY THE CHECKER (never shipped in the cert):
    //    the sha binding, the report-candidate bindings, the artifact-decode /
    //    checked-plan bindings (code/host/self/carrier of every obligation
    //    pinned to byte-bound checker values with `rfl`), the final-theorem type
    //    ascription, and the axiom-whitelist check (see `checker_witness`).
    let witness = checker_witness(&actual, &cands, &rederived, &derived_contracts);
    std::fs::write(build.path.join("CheckerWitness.lean"), &witness)
        .map_err(|e| format!("cannot write checker witness: {e}"))?;
    let w = run_lake(&build.path, &["env", "lean", "CheckerWitness.lean"])?;
    if !w.status.success() {
        // The verdict is this exit code, not any parsed line. The lake output is
        // shown to the human to name which face failed (hash, a report binding,
        // an artifact-decode / checked-plan binding, the `Holds manifest` type,
        // or a non-whitelisted axiom).
        return Err(format!(
            "certificate does not bind to this artifact: the checker's kernel witness \
             (hash binding, certified-export/contract/profile/abi bindings against the \
             proven manifest, the artifact-decode / checked-plan bindings that pin each \
             obligation's code/host/self/carrier to byte-bound checker values, the semantic-face \
             bindings that pin each obligation's Dom/Cod/domRepr/codRepr to the standard \
             form of its class and prove every domain is inhabited, the final-theorem type \
             `Holds manifest`, and the axiom whitelist) did not check:\n{}",
            tail(&w.combined, 30)
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
                policy: "simulatesModel".to_string(),
                face: r.face.describe(dom.as_deref(), cod.as_deref()),
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

fn checked_fragment_sidecar_obligations(
    cert_dir: &Path,
    manifest: &Value,
    wasm_bytes: &[u8],
) -> Result<Vec<cert::RederivedObligation>, String> {
    let certified = manifest
        .get("certified")
        .and_then(Value::as_array)
        .ok_or_else(|| "cert-manifest.json is missing array field `certified`".to_string())?;

    let mut obligations = Vec::new();
    for entry in certified {
        let name = entry.get("name").and_then(Value::as_str).ok_or_else(|| {
            "cert-manifest.json `certified[]` entry is missing string field `name`".to_string()
        })?;
        let class = entry.get("class").and_then(Value::as_str);
        if class != Some("expr-fragment-v1") {
            if entry.get("fragment").is_some() {
                return Err(format!(
                    "cert-manifest.json entry for `{name}` has fragment sidecar metadata but \
                     is not class `expr-fragment-v1`"
                ));
            }
            continue;
        }

        let fragment = entry.get("fragment").ok_or_else(|| {
            format!(
                "cert-manifest.json entry for expr fragment `{name}` is missing \
                 `fragment` sidecar metadata"
            )
        })?;
        let profile = fragment
            .get("profile")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                format!(
                    "cert-manifest.json `fragment` for `{name}` is missing string field `profile`"
                )
            })?;
        if profile != "expr-fragment-v1" {
            return Err(format!(
                "expr fragment `{name}` sidecar profile mismatch: manifest says `{profile}`"
            ));
        }
        let path = fragment
            .get("plan")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                format!("cert-manifest.json `fragment` for `{name}` is missing string field `plan`")
            })?;
        let plan_path = checked_fragment_sidecar_path(cert_dir, path)?;
        let claimed_sha = fragment
            .get("plan_sha256")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                format!(
                    "cert-manifest.json `fragment` for `{name}` is missing string field \
                     `plan_sha256`"
                )
            })?;
        let text = std::fs::read_to_string(&plan_path).map_err(|e| {
            format!(
                "cannot read expr fragment `{name}` sidecar `{}`: {e}",
                plan_path.display()
            )
        })?;
        let file_sha = cert::sha256_hex(text.as_bytes());
        if file_sha != claimed_sha {
            return Err(format!(
                "expr fragment `{name}` sidecar file hash mismatch: file hashes to \
                 {file_sha}, manifest pins {claimed_sha}"
            ));
        }
        let plan_check =
            cert::check_expr_fragment_plan_sidecar(wasm_bytes, name, &text).map_err(|e| {
                format!("expr fragment `{name}` plan sidecar does not check against wasm: {e}")
            })?;
        if plan_check.sidecar.path != path {
            return Err(format!(
                "expr fragment `{name}` checked plan path mismatch: plan checks as `{}`, \
                 manifest says `{path}`",
                plan_check.sidecar.path
            ));
        }
        if plan_check.sidecar.sha256 != claimed_sha || plan_check.sidecar.text != text {
            return Err(format!(
                "expr fragment `{name}` sidecar plan is not the canonical checked plan"
            ));
        }
        if !plan_check.canonical_matches_actual {
            return Err(format!(
                "expr fragment `{name}` plan-first canonical lowering does not match the \
                 actual wasm code-entry{}",
                plan_check
                    .mismatch_reason
                    .as_deref()
                    .map(|reason| format!(" ({reason})"))
                    .unwrap_or_default()
            ));
        }

        obligations.push(plan_check.obligation);
    }
    Ok(obligations)
}

fn checked_fragment_sidecar_path(cert_dir: &Path, path: &str) -> Result<PathBuf, String> {
    let path = Path::new(path);
    if path.is_absolute() {
        return Err("expr-fragment sidecar path must be relative".to_string());
    }
    let components = path.components().collect::<Vec<_>>();
    let [
        std::path::Component::Normal(dir),
        std::path::Component::Normal(file),
    ] = components.as_slice()
    else {
        return Err(
            "expr-fragment sidecar path must have shape `fragments/<name>.expr-fragment-v1.plan`"
                .to_string(),
        );
    };
    if dir.to_str() != Some("fragments") {
        return Err(
            "expr-fragment sidecar path must live under the `fragments/` directory".to_string(),
        );
    }
    let file = file
        .to_str()
        .ok_or_else(|| "expr-fragment sidecar filename is not valid UTF-8".to_string())?;
    if !file.ends_with(".expr-fragment-v1.plan") || file.contains('/') || file.contains('\\') {
        return Err(
            "expr-fragment sidecar filename must end with `.expr-fragment-v1.plan`".to_string(),
        );
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
/// files (each name-gated and token-scanned), the audited schema/plan-check/plan-lower/prelude/
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
    write(&build.path, "Schema.lean", cert::CERT_SCHEMA)?;
    write(&build.path, "PlanCheck.lean", cert::CERT_PLAN_CHECK)?;
    write(&build.path, "PlanLower.lean", cert::CERT_PLAN_LOWER)?;
    write(&build.path, "PlanBytes.lean", cert::CERT_PLAN_BYTES)?;
    write(&build.path, "WasmSlice.lean", cert::CERT_WASM_SLICE)?;
    write(
        &build.path,
        "ExprFragmentAccepted.lean",
        cert::CERT_EXPR_FRAGMENT_ACCEPTED,
    )?;
    write(
        &build.path,
        "AcceptedArtifact.lean",
        cert::CERT_ACCEPTED_ARTIFACT,
    )?;
    write(
        &build.path,
        "ArtifactBytes.lean",
        &cert::render_artifact_bytes_lean(wasm_bytes),
    )?;
    write(&build.path, "CertPrelude.lean", cert::CERT_PRELUDE)?;
    write(&build.path, "lean-toolchain", cert::LEAN_TOOLCHAIN)?;
    roots.push("Schema".to_string());
    roots.push("PlanCheck".to_string());
    roots.push("PlanLower".to_string());
    roots.push("PlanBytes".to_string());
    roots.push("WasmSlice".to_string());
    roots.push("ExprFragmentAccepted".to_string());
    roots.push("AcceptedArtifact".to_string());
    roots.push("ArtifactBytes".to_string());
    roots.push("CertPrelude".to_string());

    // Checker-authored lakefile: fixed `srcDir := "."`, roots derived from the
    // (gated) files actually present.
    write(&build.path, "lakefile.lean", &checker_lakefile(&roots))?;
    Ok(build)
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

/// A Lean list literal of raw (possibly multi-line) expression terms. Each item
/// is a term the checker itself rendered from the hash-verified bytes (a code or
/// host value), never attacker text, so splicing it verbatim is safe.
fn lean_expr_list<'a>(items: impl Iterator<Item = &'a str>) -> String {
    let inner = items.collect::<Vec<_>>().join(",\n   ");
    format!("[ {inner} ]")
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
    let inner = rederived
        .iter()
        .filter_map(|r| {
            r.fragment_plan_lean
                .as_ref()
                .map(|plan| format!("(\"{}\", {plan})", r.name))
        })
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
             example : AverCert.WasmSlice.codeEntryForExport AverCert.ArtifactBytes.wasmBytes {} = some ({}) := rfl\n",
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
             example : AverCert.WasmSlice.funcBindingForExport AverCert.ArtifactBytes.wasmBytes {} = some {} := rfl\n",
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
             example : AverCert.ExprFragmentAccepted.accepted AverCert.ArtifactBytes.wasmBytes {} {} ({}) ({}) ({}) {} := by dsimp [AverCert.ExprFragmentAccepted.accepted]; exact ⟨rfl, rfl, rfl, rfl, rfl⟩\n",
            r.name, export_name_bytes, r.carrier, plan, body, bytes, binding
        ));
    }
    out
}

/// Checker-owned Lean `example`s proving that expr-fragment byte-origin
/// acceptance is tied to the schema obligation used by `Final.cert`.
struct LeanExprFragmentArtifactClaims {
    claims: String,
    proof: String,
    has_claims: bool,
}

fn lean_expr_fragment_artifact_claims(
    rederived: &[cert::RederivedObligation],
) -> LeanExprFragmentArtifactClaims {
    let mut claims = Vec::new();
    let mut proofs = Vec::new();
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
        claims.push(format!(
            "({{ exportNameBytes := {export_name_bytes}, exportName := \"{name}\", \
             carrier := {carrier}, plan := (({plan}) : AverCert.Schema.ExprFragmentRawPlan), \
             obligation := AverCert.{name}Ob }} : AverCert.AcceptedArtifact.ExprFragmentClaim)",
            name = r.name,
            carrier = r.carrier
        ));
        proofs.push(format!(
            "⟨rfl, rfl, ⟨({body}), ({bytes}), {binding}, \
             ⟨⟨rfl, rfl, rfl, rfl, rfl⟩, rfl, ⟨_, rfl⟩⟩⟩⟩"
        ));
    }
    let has_claims = !claims.is_empty();
    let claims = if claims.is_empty() {
        "[]".to_string()
    } else {
        format!("[\n  {}\n]", claims.join(",\n  "))
    };
    let proof = proofs
        .into_iter()
        .rev()
        .fold("trivial".to_string(), |acc, proof| {
            format!("⟨{proof}, {acc}⟩")
        });
    LeanExprFragmentArtifactClaims {
        claims,
        proof,
        has_claims,
    }
}

fn lean_artifact_data_literal(claims: &str) -> String {
    format!(
        "({{ wasmBytes := AverCert.ArtifactBytes.wasmBytes, manifest := AverCert.manifest, \
         exprFragmentClaims := ({claims} : List AverCert.AcceptedArtifact.ExprFragmentClaim) }} : \
         AverCert.AcceptedArtifact.ArtifactData)"
    )
}

fn lean_expr_fragment_acceptance_proof_block(
    witness: &LeanExprFragmentArtifactClaims,
    indent: &str,
) -> String {
    if witness.has_claims {
        format!(
            concat!(
                "{indent}dsimp [AverCert.AcceptedArtifact.acceptedExprFragments,\n",
                "{indent}  AverCert.AcceptedArtifact.exprFragmentClaimsAccepted,\n",
                "{indent}  AverCert.AcceptedArtifact.exprFragmentClaimAccepted,\n",
                "{indent}  AverCert.AcceptedArtifact.exprFragmentPlanAccepted,\n",
                "{indent}  AverCert.ExprFragmentAccepted.accepted]\n",
                "{indent}exact {proof}\n"
            ),
            indent = indent,
            proof = witness.proof
        )
    } else {
        format!("{indent}exact trivial\n")
    }
}

fn lean_expr_fragment_obligation_acceptance_pins(
    rederived: &[cert::RederivedObligation],
) -> String {
    let witness = lean_expr_fragment_artifact_claims(rederived);
    let proof_block = lean_expr_fragment_acceptance_proof_block(&witness, "  ");
    let artifact = lean_artifact_data_literal(&witness.claims);
    format!(
        concat!(
            "-- Expr-fragment artifact data: accepted raw artifact bytes + raw plans\n",
            "-- are tied to the schema obligations used by `Final.cert`.\n",
            "example : (AverCert.AcceptedArtifact.acceptedExprFragments\n",
            "    {artifact}) := by\n",
            "{proof_block}"
        ),
        artifact = artifact,
        proof_block = proof_block
    )
}

fn lean_accepted_artifact_witness(rederived: &[cert::RederivedObligation]) -> String {
    let witness = lean_expr_fragment_artifact_claims(rederived);
    let artifact = lean_artifact_data_literal(&witness.claims);
    let expr_proof = if witness.has_claims {
        format!(
            concat!(
                "  dsimp [AverCert.AcceptedArtifact.accepted,\n",
                "    AverCert.AcceptedArtifact.acceptedExprFragments,\n",
                "    AverCert.AcceptedArtifact.exprFragmentClaimsAccepted,\n",
                "    AverCert.AcceptedArtifact.exprFragmentClaimAccepted,\n",
                "    AverCert.AcceptedArtifact.exprFragmentPlanAccepted,\n",
                "    AverCert.ExprFragmentAccepted.accepted]\n",
                "  exact ⟨{final_witness}, {proof}⟩\n"
            ),
            final_witness = FINAL_WITNESS_THEOREM,
            proof = witness.proof
        )
    } else {
        format!(
            concat!(
                "  dsimp [AverCert.AcceptedArtifact.accepted,\n",
                "    AverCert.AcceptedArtifact.acceptedExprFragments,\n",
                "    AverCert.AcceptedArtifact.exprFragmentClaimsAccepted]\n",
                "  exact ⟨{final_witness}, trivial⟩\n"
            ),
            final_witness = FINAL_WITNESS_THEOREM
        )
    };
    format!(
        concat!(
            "-- Whole-artifact acceptance root: the final schema theorem plus\n",
            "-- checker-owned expr-fragment artifact claims, under one predicate.\n",
            "def {witness_theorem} : AverCert.AcceptedArtifact.accepted\n",
            "    {artifact} := by\n",
            "{expr_proof}"
        ),
        witness_theorem = WITNESS_THEOREM,
        artifact = artifact,
        expr_proof = expr_proof
    )
}

/// The Lean file the checker authors at verify time. `sha` is what the checker
/// computed from the artifact bytes; `cands` are the charset-gated JSON report
/// candidates. Every claim is a `rfl` against `AverCert.manifest` (or the final
/// theorem's type / the kernel axiom collector), so a lying JSON, a rebound
/// hash, a weakened theorem, or a smuggled axiom all make this file fail to
/// check — and THAT (the process exit code) is the only verdict channel.
fn checker_witness(
    sha: &str,
    cands: &Candidates,
    rederived: &[cert::RederivedObligation],
    derived_contracts: &[String],
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
    let profile = &cands.profile;
    let abi = &cands.abi;
    let codes = lean_expr_list(rederived.iter().map(|r| r.code.as_str()));
    let hosts = lean_expr_list(rederived.iter().map(|r| r.host.as_str()));
    let selfs = lean_nat_list(rederived.iter().map(|r| r.self_idx));
    let carriers = lean_nat_list(rederived.iter().map(|r| r.carrier));
    let expr_fragment_plans = lean_expr_fragment_plan_pairs(rederived);
    let expr_fragment_lower_pins = lean_expr_fragment_lower_pins(rederived);
    let expr_fragment_code_entry_pins = lean_expr_fragment_code_entry_pins(rederived);
    let expr_fragment_wasm_slice_pins = lean_expr_fragment_wasm_slice_pins(rederived);
    let expr_fragment_func_binding_pins = lean_expr_fragment_func_binding_pins(rederived);
    let expr_fragment_accepted_pins = lean_expr_fragment_accepted_pins(rederived);
    let expr_fragment_obligation_acceptance_pins =
        lean_expr_fragment_obligation_acceptance_pins(rederived);
    let accepted_artifact_witness = lean_accepted_artifact_witness(rederived);
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
         open CertPrelude AverCert.Schema\n\
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
         -- Contracts: the JSON candidate must match the proven manifest, and\n\
         -- the proven manifest must also match the BYTE-DERIVED contract list.\n\
         -- JSON-only padding and manifest+JSON deletion are therefore both\n\
         -- declined; the final report uses only the byte-derived list.\n\
         example : AverCert.manifest.subject.contracts = {json_contracts} := rfl\n\
         example : AverCert.manifest.subject.contracts = {byte_contracts} := rfl\n\
         example : AverCert.manifest.subject.profile = \"{profile}\" := rfl\n\
         example : AverCert.manifest.subject.abi = \"{abi}\" := rfl\n\n\
         -- Expr-fragment raw plans: the manifest's Lean-data plans are pinned\n\
         -- to the checker-rendered `ExprFragmentRawPlan` terms reconstructed\n\
         -- from sidecars that already passed hash, type/refinement and\n\
         -- canonical code-entry equality against the artifact bytes.\n\
         example : AverCert.manifest.exprFragmentPlans = {expr_fragment_plans} := rfl\n\n\
         -- The manifest plans also pass the audited Lean-side structural\n\
         -- checker. This is not the v2 byte-level `LowersCodeEntry` proof yet,\n\
         -- but it makes `RawPlan -> checked structural plan` a kernel-checked\n\
         -- artifact invariant.\n\
         example : AverCert.manifest.exprFragmentPlans.all (fun p => AverCert.PlanCheck.checkExprFragmentRawPlan p.2) = true := rfl\n\n\
         -- Expr-fragment plan lowering: for every checked expr-fragment sidecar,\n\
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
         -- Expr-fragment artifact bridge: raw artifact bytes + raw plan +\n\
         -- schema obligation imply the aggregate expr-fragment acceptance\n\
         -- predicate, with body/code-entry/function binding kept as internal\n\
         -- witnesses rather than extra trusted parameters.\n\
         {expr_fragment_obligation_acceptance_pins}\n\
         -- Hash binding: the sha the checker computed from the artifact bytes.\n\
         example : AverCert.manifest.subject.artifactHash = \"{sha}\" := rfl\n\
         example : CertModule.wasmSha256 = \"{sha}\" := rfl\n\n\
         -- Artifact-decode / checked-plan bindings: the CODE, HOST, SELF and\n\
         -- CARRIER of every obligation are pinned, position for position, to\n\
         -- byte-bound checker values. Ordinary classes come from the audited\n\
         -- disassembler; expr-fragment code/face comes from a checked plan\n\
         -- whose canonical code-entry bytes equal the artifact bytes. These\n\
         -- are EXACTLY the fields `Obligation.holds` reasons about\n\
         -- (`wFuncN o.code (o.host add sub mul stringEq) fuel o.self`), so a fabricated body,\n\
         -- a decoupled `code`/`self`/`carrier`, or a nerfed `host` that would\n\
         -- make `holds` vacuous all diverge from byte-bound checker values and\n\
         -- fail a `rfl`. The spliced terms come from the checker's own audited\n\
         -- renderer, never from attacker text, and are fully expanded (they do\n\
         -- NOT reference the cert's `CertModule.*` defs, which an attacker edits).\n\
         example : AverCert.manifest.obligations.map (fun o => o.code) =\n  {codes} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.host) =\n  {hosts} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.self) = {selfs} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.carrier) = {carriers} := rfl\n\n\
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
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let path =
            std::env::temp_dir().join(format!("aver-certverify-{}-{nanos}", std::process::id()));
        std::fs::create_dir_all(&path).map_err(|e| format!("create checker build dir: {e}"))?;
        Ok(BuildDir { path })
    }
}

impl Drop for BuildDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
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
        println!("  {}  [{}]", e.name.cyan().bold(), cert::CERT_LEVEL);
        println!("    {}", e.face);
        println!(
            "    policy: {} (emitted body simulates its model under the named contracts)",
            e.policy
        );
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
