//! `aver cert verify|explain` — the consumer side of `aver compile --certify`.
//!
//! `verify` is a fail-closed checker. It does NOT trust the certificate's build
//! configuration or its report: it assembles its OWN build in a fresh,
//! checker-owned temp directory from the audited `Schema.lean` / `CertPrelude.lean`
//! this binary embeds plus the cert's DATA-only Lean files, authors its own
//! `lakefile.lean`, and builds with a clean cache. It then — via a Lean witness
//! the checker authors itself — binds the hash it computed from the artifact
//! bytes to the theorems, forces the final theorem's type to `Holds manifest`,
//! and confirms kernel-cleanliness. The certified-export report (count, names,
//! policies, contracts) is read back from the SAME `AverCert.manifest` literal
//! the witness proved `Holds` about, so the report can never name an export the
//! kernel did not see. `explain` renders that same trusted report (it therefore
//! builds too); the untrusted JSON is consulted only for the DECLINED list,
//! which carries no behavioral claim.

use std::path::{Path, PathBuf};
use std::process::Command;

use aver::codegen::cert;
use colored::Colorize;
use serde_json::Value;

/// Kernel axioms a certificate is allowed to depend on. Anything else — most
/// importantly `sorryAx` (an admitted goal) or `ofReduceBool` (native-code
/// trust) — fails the check.
const AXIOM_WHITELIST: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];

/// The theorem the checker composes in its own witness file: it ascribes
/// `AverCert.Final.cert` to the type `Holds manifest`, so reading its axioms
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

/// Outcome of a successful (fail-closed) verify pass.
enum Verdict {
    /// At least one export carries a behavioral certificate.
    Certified(String),
    /// The certificate built and stayed kernel-clean, but names zero certified
    /// exports — an admission with no behavioral claims. Not the green path.
    NoExports(String),
}

/// The certified side of the report, read back from the kernel-checked
/// `AverCert.manifest` literal (NOT from the attacker-editable JSON).
struct TrustedReport {
    /// One `(export name, policy)` per proven obligation.
    exports: Vec<(String, String)>,
    contracts: Vec<String>,
    profile: String,
    abi: String,
    artifact_hash: String,
}

pub(super) fn cmd_cert_verify(artifact: &str, cert_dir: &str) {
    match verify(Path::new(artifact), Path::new(cert_dir)) {
        Ok(Verdict::Certified(summary)) => {
            println!("{} {}", "CERTIFIED".green().bold(), summary);
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
        Ok(Verdict::Certified(summary))
    }
}

/// The trusted core, shared by `verify` and `explain`: bind the artifact bytes
/// to a checker-owned build of the cert's data, prove the final theorem is
/// `Holds manifest` kernel-clean, and read the certified report back from that
/// same manifest literal.
fn trusted_check(artifact: &Path, cert_dir: &Path) -> Result<TrustedReport, String> {
    // 1. Artifact identity (fast pre-check): the bytes must hash to the pinned
    //    value. This is a convenience tripwire on the (attacker-editable) JSON;
    //    the kernel witness below is what actually binds the hash.
    let bytes = std::fs::read(artifact)
        .map_err(|e| format!("cannot read artifact {}: {e}", artifact.display()))?;
    let actual = cert::sha256_hex(&bytes);
    let manifest = read_manifest(cert_dir)?;
    let pinned = manifest_str(&manifest, "wasm_sha256")?;
    if actual != pinned {
        return Err(format!(
            "artifact hash mismatch: {} hashes to {actual}, certificate pins {pinned}",
            artifact.display()
        ));
    }

    // 2. Assemble a checker-owned build. The audited schema + prelude come from
    //    THIS binary, never from the cert; the cert supplies only per-artifact
    //    DATA (Module/Manifest/Certificate/Final + the model modules). The cert's
    //    own lakefile / srcDir / `.lake` cache are never read, so a build-tree
    //    subversion (decoy `Holds := True` behind a redirected srcDir or a
    //    poisoned olean cache) cannot take effect.
    let build = assemble_build(cert_dir)?;

    // 3. The assembled project must build under the pinned toolchain, from a
    //    clean cache (the fresh dir has no `.lake`).
    let b = run_lake(&build.path, &["build"])?;
    if !b.status.success() {
        return Err(format!(
            "certificate did not build (lake build failed):\n{}",
            tail(&b.combined, 20)
        ));
    }

    // 4. Kernel witness authored BY THE CHECKER (never shipped in the cert):
    //    (a) bind the sha256 the checker just computed from the artifact bytes
    //        to the hashes the kernel-checked theorems talk about, by splicing
    //        the computed hash in as a Lean literal and demanding `rfl`;
    //    (b) force the final theorem's TYPE to `Holds manifest` by ascription,
    //        so a comment-smuggled `: True := trivial` cannot pass;
    //    (c) print the certified report from the SAME `manifest` literal, so the
    //        report cannot name an export the kernel did not vouch for.
    let witness = checker_witness(&actual);
    std::fs::write(build.path.join("CheckerWitness.lean"), &witness)
        .map_err(|e| format!("cannot write checker witness: {e}"))?;
    let w = run_lake(&build.path, &["env", "lean", "CheckerWitness.lean"])?;
    if !w.status.success() {
        return Err(format!(
            "certificate does not bind to this artifact: the checker's kernel witness \
             (hash binding + final-theorem type `Holds manifest`) did not check:\n{}",
            tail(&w.combined, 30)
        ));
    }

    // 5. The composed witness must be kernel-clean.
    check_axioms(&w.combined, WITNESS_THEOREM)?;

    // 6. Read the certified report back from the proven manifest.
    parse_report(&w.combined)
}

/// Populate a fresh, checker-owned build directory: the cert's DATA-only Lean
/// files, the audited schema/prelude/toolchain from THIS binary, and a
/// checker-authored lakefile. Extra/unexpected Lean files the cert ships are
/// copied in as inert extra roots (they cannot weaken the pinned `Holds`, whose
/// meaning lives in the binary's `Schema.lean`); if they fail to compile the
/// build fails closed.
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
        let content = std::fs::read(entry.path())
            .map_err(|e| format!("cannot read cert file {name}: {e}"))?;
        std::fs::write(build.path.join(&name), &content)
            .map_err(|e| format!("cannot stage {name}: {e}"))?;
        roots.push(name.trim_end_matches(".lean").to_string());
    }

    // Audited trusted computing base from THIS binary (not the cert).
    write(&build.path, "Schema.lean", cert::CERT_SCHEMA)?;
    write(&build.path, "CertPrelude.lean", cert::CERT_PRELUDE)?;
    write(&build.path, "lean-toolchain", cert::LEAN_TOOLCHAIN)?;
    roots.push("Schema".to_string());
    roots.push("CertPrelude".to_string());

    // Checker-authored lakefile: fixed `srcDir := "."`, roots derived from the
    // files actually present (content-blind).
    write(&build.path, "lakefile.lean", &checker_lakefile(&roots))?;
    Ok(build)
}

/// The checker's own lakefile. Fixed structure; the root list is whatever Lean
/// modules ended up in the build dir. `srcDir := "."` so no cert-supplied
/// srcDir redirection is honored.
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

/// The Lean file the checker authors at verify time. `sha` is what the checker
/// computed from the artifact bytes — spliced in as a literal, not read from
/// the (attacker-editable) manifest JSON. The trailing `#eval` prints the
/// certified report from the SAME `manifest` literal the theorem is about.
fn checker_witness(sha: &str) -> String {
    format!(
        "-- Authored by `aver cert verify`, NOT shipped in the certificate.\n\
         import Schema\n\
         import Module\n\
         import Manifest\n\
         import Final\n\n\
         -- Binding: the sha256 the checker computed from the artifact bytes must\n\
         -- equal the hashes the kernel-checked theorems are about.\n\
         example : AverCert.manifest.subject.artifactHash = \"{sha}\" := rfl\n\
         example : CertModule.wasmSha256 = \"{sha}\" := rfl\n\n\
         -- Statement: force the final theorem's TYPE by ascription (no text match).\n\
         def {WITNESS_THEOREM} : AverCert.Schema.Holds AverCert.manifest := AverCert.Final.cert\n\n\
         #print axioms {WITNESS_THEOREM}\n\n\
         -- Certified report, derived from the SAME manifest literal proven above.\n\
         #eval (do\n  \
         for o in AverCert.manifest.obligations do\n    \
         let pol := match o.policy with | AverCert.Schema.Policy.simulatesModel => \"simulatesModel\"\n    \
         IO.println (\"AVERCERT-EXPORT\\t\" ++ o.export_ ++ \"\\t\" ++ pol)\n  \
         for c in AverCert.manifest.subject.contracts do\n    \
         IO.println (\"AVERCERT-CONTRACT\\t\" ++ c)\n  \
         IO.println (\"AVERCERT-SUBJECT\\t\" ++ AverCert.manifest.subject.profile ++ \"\\t\" ++ AverCert.manifest.subject.abi)\n  \
         IO.println (\"AVERCERT-HASH\\t\" ++ AverCert.manifest.subject.artifactHash) : IO Unit)\n"
    )
}

/// Parse the trusted report lines the witness `#eval` emitted.
fn parse_report(output: &str) -> Result<TrustedReport, String> {
    let mut exports = Vec::new();
    let mut contracts = Vec::new();
    let mut profile = String::new();
    let mut abi = String::new();
    let mut artifact_hash = String::new();
    for line in output.lines() {
        if let Some(rest) = line.strip_prefix("AVERCERT-EXPORT\t") {
            let mut parts = rest.splitn(2, '\t');
            let name = parts.next().unwrap_or("").to_string();
            let policy = parts.next().unwrap_or("").to_string();
            exports.push((name, policy));
        } else if let Some(rest) = line.strip_prefix("AVERCERT-CONTRACT\t") {
            contracts.push(rest.to_string());
        } else if let Some(rest) = line.strip_prefix("AVERCERT-SUBJECT\t") {
            let mut parts = rest.splitn(2, '\t');
            profile = parts.next().unwrap_or("").to_string();
            abi = parts.next().unwrap_or("").to_string();
        } else if let Some(rest) = line.strip_prefix("AVERCERT-HASH\t") {
            artifact_hash = rest.to_string();
        }
    }
    if artifact_hash.is_empty() {
        return Err(
            "checker report missing (witness did not emit the manifest render)".to_string(),
        );
    }
    Ok(TrustedReport {
        exports,
        contracts,
        profile,
        abi,
        artifact_hash,
    })
}

/// Parse the `#print axioms <theorem>` output and confirm the theorem depends
/// only on whitelisted kernel axioms.
fn check_axioms(output: &str, theorem: &str) -> Result<(), String> {
    let needle = format!("'{theorem}' ");
    let line = output
        .lines()
        .find(|l| l.contains(&needle))
        .ok_or_else(|| {
            format!(
                "could not read the axiom dependencies of {theorem} (no `#print axioms` output)"
            )
        })?;
    if line.contains("does not depend on any axioms") {
        return Ok(());
    }
    let list = line
        .split_once("depends on axioms:")
        .map(|(_, rest)| rest.trim())
        .ok_or_else(|| format!("unrecognized `#print axioms` output: {line}"))?;
    let list = list.trim_start_matches('[').trim_end_matches(']');
    for axiom in list.split(',').map(str::trim).filter(|a| !a.is_empty()) {
        if !AXIOM_WHITELIST.contains(&axiom) {
            return Err(format!(
                "final theorem depends on non-whitelisted axiom `{axiom}` (kernel not clean)"
            ));
        }
    }
    Ok(())
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

/// Human-readable report. The CERTIFIED side is the trusted, kernel-derived
/// report (so it builds); the DECLINED side is read from the untrusted JSON
/// (declines carry no behavioral claim).
fn explain(artifact: &Path, cert_dir: &Path) -> Result<(), String> {
    let report = trusted_check(artifact, cert_dir)?;

    println!("{}", "Artifact certificate".bold());
    println!("  artifact: {}", artifact.display());
    println!("  pinned sha256: {}", report.artifact_hash);
    println!("  profile: {}    abi: {}", report.profile, report.abi);

    println!("\n{}", "CERTIFIED".green().bold());
    if report.exports.is_empty() {
        println!("  (none)");
    }
    for (name, policy) in &report.exports {
        println!("  {}  [{}]", name.cyan().bold(), cert::CERT_LEVEL);
        println!(
            "    policy: {policy} (emitted body simulates its model under the named contracts)"
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

    // DECLINED: untrusted convenience only (no behavioral claim).
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
        let name = d.get("name").and_then(Value::as_str).unwrap_or("?");
        let reason = d.get("reason").and_then(Value::as_str).unwrap_or("?");
        println!("  {name} — {reason}");
    }
    Ok(())
}
