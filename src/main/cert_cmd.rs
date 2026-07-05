//! `aver cert verify|explain|inspect` — the consumer side of
//! `aver compile --certify`.
//!
//! `verify` is a fail-closed checker: it confirms the artifact bytes hash to
//! the pinned value, that the audited schema and semantics prelude on disk are
//! the ones this binary embeds, that the cert `lake build`s, and that the ONE
//! final theorem is present with its approved statement and stays kernel-clean.
//! `explain`/`inspect` render the manifest as a human report without building.

use std::path::Path;
use std::process::Command;

use aver::codegen::cert;
use colored::Colorize;
use serde_json::Value;

/// Kernel axioms a certificate is allowed to depend on. Anything else — most
/// importantly `sorryAx` (an admitted goal) or `ofReduceBool` (native-code
/// trust) — fails the check.
const AXIOM_WHITELIST: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];

pub(super) fn cmd_cert_verify(artifact: &str, cert_dir: &str) {
    match verify(Path::new(artifact), Path::new(cert_dir)) {
        Ok(summary) => {
            println!("{} {}", "CERTIFIED".green().bold(), summary);
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

fn verify(artifact: &Path, cert_dir: &Path) -> Result<String, String> {
    let manifest = read_manifest(cert_dir)?;

    // 1. Artifact identity: the bytes must hash to the pinned value.
    let bytes = std::fs::read(artifact)
        .map_err(|e| format!("cannot read artifact {}: {e}", artifact.display()))?;
    let actual = cert::sha256_hex(&bytes);
    let pinned = manifest_str(&manifest, "wasm_sha256")?;
    if actual != pinned {
        return Err(format!(
            "artifact hash mismatch: {} hashes to {actual}, certificate pins {pinned}",
            artifact.display()
        ));
    }

    // 2. Audited trusted computing base: the on-disk schema and prelude must be
    //    exactly the ones this binary embeds (anchor), AND agree with the hashes
    //    the manifest records (self-consistency).
    check_audited_file(
        cert_dir,
        "Schema.lean",
        &cert::audited_schema_sha(),
        manifest_str(&manifest, "schema_sha256")?,
    )?;
    check_audited_file(
        cert_dir,
        "CertPrelude.lean",
        &cert::audited_prelude_sha(),
        manifest_str(&manifest, "prelude_sha256")?,
    )?;

    // 3. The certificate project must build under the pinned toolchain.
    let build = run_lake(cert_dir, &["build"])?;
    if !build.status.success() {
        return Err(format!(
            "certificate did not build (lake build failed):\n{}",
            tail(&build.combined, 20)
        ));
    }

    // 4. The single final theorem must be present with its approved statement
    //    (name + `Holds manifest`) — no arbitrary Lean syntax is matched, just
    //    the one canonical line.
    let final_src = std::fs::read_to_string(cert_dir.join("Final.lean"))
        .map_err(|e| format!("cannot read Final.lean: {e}"))?;
    if !final_src.contains(cert::FINAL_STATEMENT_LINE) {
        return Err(format!(
            "final theorem statement is not the approved shape; expected exactly:\n    {}",
            cert::FINAL_STATEMENT_LINE
        ));
    }
    if manifest_str(&manifest, "final_theorem")? != cert::FINAL_THEOREM {
        return Err(format!(
            "manifest names an unexpected final theorem (expected {})",
            cert::FINAL_THEOREM
        ));
    }

    // 5. The final theorem must be kernel-clean: re-elaborate Final.lean and
    //    read its `#print axioms` output (robust to a cached build), then
    //    confirm the axiom set is within the whitelist.
    let axioms = run_lake(cert_dir, &["env", "lean", "Final.lean"])?;
    check_axioms(&axioms.combined)?;

    let n = manifest
        .get("certified")
        .and_then(Value::as_array)
        .map(|a| a.len())
        .unwrap_or(0);
    Ok(format!(
        "{} ({} certified export{}, level {})",
        artifact.display(),
        n,
        if n == 1 { "" } else { "s" },
        manifest_str(&manifest, "level").unwrap_or("L1"),
    ))
}

/// A schema/prelude file must hash to the audited value baked into this binary
/// and to the value the manifest recorded.
fn check_audited_file(
    cert_dir: &Path,
    name: &str,
    audited: &str,
    recorded: &str,
) -> Result<(), String> {
    let path = cert_dir.join(name);
    let bytes = std::fs::read(&path).map_err(|e| format!("cannot read {name}: {e}"))?;
    let on_disk = cert::sha256_hex(&bytes);
    if on_disk != audited {
        return Err(format!(
            "{name} is not the audited version: it hashes to {on_disk}, this checker expects {audited}"
        ));
    }
    if on_disk != recorded {
        return Err(format!(
            "{name} content-hash does not match the manifest (disk {on_disk}, manifest {recorded})"
        ));
    }
    Ok(())
}

/// Parse the `#print axioms AverCert.Final.cert` output and confirm the final
/// theorem depends only on whitelisted kernel axioms.
fn check_axioms(output: &str) -> Result<(), String> {
    let needle = format!("'{}' ", cert::FINAL_THEOREM);
    let line = output
        .lines()
        .find(|l| l.contains(&needle))
        .ok_or_else(|| {
            format!(
                "could not read the axiom dependencies of {} (no `#print axioms` output)",
                cert::FINAL_THEOREM
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

struct LakeOut {
    status: std::process::ExitStatus,
    combined: String,
}

fn run_lake(cert_dir: &Path, args: &[&str]) -> Result<LakeOut, String> {
    let out = Command::new("lake")
        .current_dir(cert_dir)
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

/// Human-readable report from the manifest alone (no build).
fn explain(artifact: &Path, cert_dir: &Path) -> Result<(), String> {
    let manifest = read_manifest(cert_dir)?;
    let level = manifest_str(&manifest, "level").unwrap_or("L1");
    let profile = manifest_str(&manifest, "profile").unwrap_or("?");
    let abi = manifest_str(&manifest, "abi").unwrap_or("?");

    println!("{}", "Artifact certificate".bold());
    println!("  artifact: {}", artifact.display());
    println!(
        "  pinned sha256: {}",
        manifest_str(&manifest, "wasm_sha256").unwrap_or("?")
    );
    println!("  profile: {profile}    abi: {abi}");

    let contracts: Vec<&str> = manifest
        .get("runtime_contracts")
        .and_then(Value::as_array)
        .map(|a| a.iter().filter_map(Value::as_str).collect())
        .unwrap_or_default();

    let certified = manifest
        .get("certified")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    println!("\n{}", "CERTIFIED".green().bold());
    if certified.is_empty() {
        println!("  (none)");
    }
    for c in &certified {
        let name = c.get("name").and_then(Value::as_str).unwrap_or("?");
        let policy = c
            .get("policy")
            .and_then(Value::as_str)
            .unwrap_or("simulatesModel");
        let lvl = c.get("level").and_then(Value::as_str).unwrap_or(level);
        let class = c.get("class").and_then(Value::as_str).unwrap_or("");
        println!("  {}  [{}]", name.cyan().bold(), lvl);
        println!(
            "    policy: {policy} ({class} body simulates its model under the named contracts)"
        );
        if contracts.is_empty() {
            println!("    runtime-contracts: (none)");
        } else {
            println!("    runtime-contracts:");
            for rc in &contracts {
                println!("      - {rc}");
            }
        }
    }

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
