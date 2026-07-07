//! `aver cert verify|explain` — the consumer side of `aver compile --certify`.
//!
//! `verify` is a fail-closed checker whose ONLY trust channel is the exit code
//! of the Lean toolchain over files the checker itself authored. It assembles
//! its OWN build in a fresh, checker-owned temp directory from the audited
//! `Schema.lean` / `CertPrelude.lean` this binary embeds plus the cert's
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
//! report is built in Rust from the JSON candidates the kernel just confirmed.
//! (stdout is shown to a human inside error messages only — a display channel,
//! never a trust channel.) `explain` renders that same trusted report.
//!
//! The bytes-vs-data divergence is closed INSIDE that same kernel witness. A
//! bare `Holds manifest` proof only says "some Lean-encoded body simulates the
//! model and the bytes hash to S"; it does NOT say that body DECODES from those
//! bytes, so a hostile producer could ship `WInstr` data unrelated to the real
//! bytes with a vacuously-true `holds`. To close that, the checker re-derives
//! each obligation's `code`, `host`, `self` and `carrier` from the hash-verified
//! bytes with the audited Aver disassembler (`cert::rederive_obligations`) and
//! splices those values, fully expanded, into the witness — then pins them with
//! `rfl` against `manifest.obligations.map (·.code / ·.host / ·.self / ·.carrier)`.
//! Those are EXACTLY the fields `Obligation.holds` reasons about
//! (`wFuncN o.code (o.host add sub mul) fuel o.self`), so a fabricated body, a
//! decoupled `code`/`self`/`carrier`, or a nerfed `host` (which would make
//! `holds` vacuous) all diverge from the bytes and fail a `rfl` — the file does
//! not check and verify declines. The spliced terms are the checker's own
//! rendering over the bytes, never attacker text, and are fully expanded so they
//! do not reference the cert's `CertModule.*` defs (which an attacker edits).
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

/// The constant the checker composes in its own witness file: it ascribes
/// `AverCert.Final.cert` to the type `Holds manifest`, so collecting its axioms
/// transitively covers the final theorem without matching any text.
const WITNESS_THEOREM: &str = "AverCertChecker.final";

/// Lean source files the checker owns and never copies from the cert: the
/// audited trusted computing base (taken from this binary) plus the checker's
/// own build config and witness. A cert shipping files by these names has them
/// ignored.
const CHECKER_OWNED: [&str; 4] = [
    "Schema.lean",
    "CertPrelude.lean",
    "lakefile.lean",
    "CheckerWitness.lean",
];

/// Maximum length (bytes) of a JSON-supplied string spliced into the witness.
const MAX_CANDIDATE_LEN: usize = 200;

/// Emitted on a CERTIFIED verdict: every obligation's code/host/self/carrier was
/// re-derived from the hash-verified module bytes and pinned to the proven
/// manifest by `rfl` inside the kernel witness (so the certified `holds` is a
/// statement about what the bytes actually decode to). Trusted via the audited
/// Aver disassembler, not an in-kernel wasm decode proof; it does not change the
/// cert level.
const ARTIFACT_DECODE_LINE: &str = "artifact-decode: each obligation's export name and its code/host/self/carrier are kernel-pinned (rfl) to what the bytes decode to (trusted via the audited disassembler)";

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

/// The certified side of the report, built from the JSON candidates the kernel
/// witness confirmed equal to the proven `AverCert.manifest` literal.
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

    // 2. Report candidates from the untrusted JSON, each charset-gated on its
    //    decoded value so it is safe to splice as a Lean literal below.
    let cands = read_candidates(&manifest)?;

    // 2b. Re-derive the certified obligations (code/host/self/carrier) straight
    //     from the hash-verified artifact bytes with the audited disassembler.
    //     These are spliced into the checker witness below and pinned with `rfl`
    //     against `manifest.obligations`, so the kernel theorem is forced to
    //     reason about exactly what the bytes decode to. If disassembly fails
    //     outright (not a wasm module, no box helper), decline here — before the
    //     witness — fail-closed.
    //     The model `.lean` files supply the combinator operator (`+`/`*`) that
    //     the bytes cannot distinguish for the bignum helpers; they are the same
    //     (untrusted) model the kernel witness proves the bytes against, so
    //     reading the operator here does not widen trust — `lake` rejects any
    //     mismatch. Only the `def X__fuel` operator is read; nothing is executed.
    let model_files = read_lean_files(cert_dir);
    let rederived = cert::rederive_obligations(&bytes, &model_files)?;

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
    let build = assemble_build(cert_dir)?;

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
    //    the sha binding, the report-candidate bindings, the artifact-decode
    //    bindings (code/host/self/carrier of every obligation pinned to the
    //    bytes-derived values with `rfl`), the final-theorem type ascription,
    //    and the axiom-whitelist check (see `checker_witness`).
    let witness = checker_witness(&actual, &cands, &rederived);
    std::fs::write(build.path.join("CheckerWitness.lean"), &witness)
        .map_err(|e| format!("cannot write checker witness: {e}"))?;
    let w = run_lake(&build.path, &["env", "lean", "CheckerWitness.lean"])?;
    if !w.status.success() {
        // The verdict is this exit code, not any parsed line. The lake output is
        // shown to the human to name which face failed (hash, a report binding,
        // an artifact-decode binding, the `Holds manifest` type, or a
        // non-whitelisted axiom).
        return Err(format!(
            "certificate does not bind to this artifact: the checker's kernel witness \
             (hash binding, certified-export/contract/profile/abi bindings against the \
             proven manifest, the artifact-decode bindings that pin each obligation's \
             code/host/self/carrier to what the bytes decode to, the semantic-face \
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
        contracts: cands.contracts,
        profile: cands.profile,
        abi: cands.abi,
        artifact_hash: actual,
    })
}

/// Populate a fresh, checker-owned build directory: the cert's DATA-only Lean
/// files (each name-gated and token-scanned), the audited schema/prelude/
/// toolchain from THIS binary, and a checker-authored lakefile.
fn assemble_build(cert_dir: &Path) -> Result<BuildDir, String> {
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
    write(&build.path, "CertPrelude.lean", cert::CERT_PRELUDE)?;
    write(&build.path, "lean-toolchain", cert::LEAN_TOOLCHAIN)?;
    roots.push("Schema".to_string());
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
    let contracts = lean_str_list(&cands.contracts);
    let profile = &cands.profile;
    let abi = &cands.abi;
    let codes = lean_expr_list(rederived.iter().map(|r| r.code.as_str()));
    let hosts = lean_expr_list(rederived.iter().map(|r| r.host.as_str()));
    let selfs = lean_nat_list(rederived.iter().map(|r| r.self_idx));
    let carriers = lean_nat_list(rederived.iter().map(|r| r.carrier));
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
         import Module\n\
         import Manifest\n\
         import Final\n\
         open CertPrelude\n\
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
         example : AverCert.manifest.subject.contracts = {contracts} := rfl\n\
         example : AverCert.manifest.subject.profile = \"{profile}\" := rfl\n\
         example : AverCert.manifest.subject.abi = \"{abi}\" := rfl\n\n\
         -- Hash binding: the sha the checker computed from the artifact bytes.\n\
         example : AverCert.manifest.subject.artifactHash = \"{sha}\" := rfl\n\
         example : CertModule.wasmSha256 = \"{sha}\" := rfl\n\n\
         -- Artifact-decode bindings: the CODE, HOST, SELF and CARRIER of every\n\
         -- obligation are pinned, position for position, to the values the\n\
         -- audited disassembler re-derived from the hash-verified bytes. These\n\
         -- are EXACTLY the fields `Obligation.holds` reasons about\n\
         -- (`wFuncN o.code (o.host add sub mul) fuel o.self`), so a fabricated body,\n\
         -- a decoupled `code`/`self`/`carrier`, or a nerfed `host` that would\n\
         -- make `holds` vacuous all diverge from the bytes and fail a `rfl`. The\n\
         -- spliced terms come from the checker's own audited renderer over the\n\
         -- bytes, never from attacker text, and are fully expanded (they do NOT\n\
         -- reference the cert's `CertModule.*` defs, which an attacker edits).\n\
         example : AverCert.manifest.obligations.map (fun o => o.code) =\n  {codes} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.host) =\n  {hosts} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.self) = {selfs} := rfl\n\
         example : AverCert.manifest.obligations.map (fun o => o.carrier) = {carriers} := rfl\n\n\
         -- Semantic-face bindings: the typed `Dom`/`Cod`/`domRepr`/`codRepr` of\n\
         -- every obligation, pinned to the standard form of its byte-derived\n\
         -- class, plus a `Nonempty Dom` proof. A manifest that weakens the face\n\
         -- (`Dom := Empty`, `codRepr := fun _ _ _ => True`, `domRepr := fun _ _ _ => False`,\n\
         -- a nerfed arity) fails one of these kernel checks.\n\
         {face_section}\n\
         -- Statement: force the final theorem's TYPE by ascription (no text match).\n\
         def {WITNESS_THEOREM} : AverCert.Schema.Holds AverCert.manifest := AverCert.Final.cert\n\n\
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
