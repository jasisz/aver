//! Optional, bounded proof discovery. Search output and cached scripts are
//! proposals: each is replayed without waterfall and axiom-audited before use.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

use aver::codegen::lean::waterfall::{BEGIN, Candidate, END};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

#[path = "proof_waterfall/format.rs"]
mod format;
#[path = "proof_waterfall/process.rs"]
mod process;

#[derive(clap::Args, Debug)]
pub struct Options {
    /// Try unresolved Lean laws with a locally built waterfall checkout.
    /// Saves ordinary Lean scripts; replay and the normal axiom audit remain mandatory.
    #[arg(long, requires = "check_mode", conflicts_with = "allow_mathlib")]
    pub waterfall: Option<PathBuf>,
    /// Maximum attempted operations per waterfall mode (search, then committed).
    #[arg(long, default_value_t = 1000, requires = "waterfall", value_parser = clap::value_parser!(u32).range(1..))]
    pub waterfall_effort: u32,
    /// Wall-clock limit in seconds per baseline, search, or replay attempt.
    #[arg(long, default_value_t = 30, requires = "waterfall", value_parser = clap::value_parser!(u32).range(1..))]
    pub waterfall_timeout: u32,
    /// Restrict discovery to a law identity from proof_manifest.json (including its because steps).
    /// Repeat for multiple laws. By default, consider all emitted laws.
    #[arg(long, requires = "waterfall")]
    pub waterfall_law: Vec<String>,
}

impl Options {
    fn selected(&self, c: &Candidate) -> bool {
        self.waterfall_law.is_empty()
            || self
                .waterfall_law
                .iter()
                .any(|law| c.label == *law || c.label.starts_with(&format!("{law}.")))
    }
}

#[derive(Default, Serialize, Deserialize)]
struct Cache {
    version: u32,
    scripts: BTreeMap<String, String>,
}

use process::Run;

struct Runner<'a> {
    dir: &'a Path,
    options: &'a Options,
    waterfall: PathBuf,
    log: fs::File,
    cache: Cache,
    report: Vec<serde_json::Value>,
    search_ready: bool,
}

impl Runner<'_> {
    fn prepare_search(&mut self) -> Result<(), String> {
        if self.search_ready {
            return Ok(());
        }
        if !self
            .waterfall
            .join(".lake/build/lib/lean/waterfall.olean")
            .is_file()
        {
            return Err(format!(
                "waterfall is not built at {}; build it with the exported Lean toolchain first",
                self.waterfall.display()
            ));
        }
        let mut input = tempfile::Builder::new()
            .prefix(".aver-waterfall-import-")
            .suffix(".lean")
            .tempfile_in(self.dir)
            .map_err(|e| e.to_string())?;
        writeln!(input, "import waterfall\n#check waterfall.Options").map_err(|e| e.to_string())?;
        let run = self.command(
            &[
                "env",
                "lean",
                "--json",
                input.path().to_str().ok_or("non-UTF8 proof path")?,
            ],
            true,
            self.options.waterfall_timeout,
        )?;
        if !run.ok {
            return Err("could not load waterfall with the exported Lean toolchain; see proof_waterfall.log".into());
        }
        self.search_ready = true;
        Ok(())
    }

    fn command(&mut self, args: &[&str], search: bool, seconds: u32) -> Result<Run, String> {
        let mut command = Command::new("lake");
        command
            .args(args)
            .current_dir(self.dir)
            .env_remove("LEAN_PATH");
        if search {
            command.env("LEAN_PATH", self.waterfall.join(".lake/build/lib/lean"));
        }
        let run = process::run(command, seconds)?;
        writeln!(
            self.log,
            "\nlake {} (ok={}, timeout={})\n{}",
            args.join(" "),
            run.ok,
            run.timed_out,
            run.output
        )
        .map_err(|e| e.to_string())?;
        Ok(run)
    }

    fn check(&mut self, prefix: &str, body: &str, name: &str, search: bool) -> Result<Run, String> {
        let mut input = tempfile::Builder::new()
            .prefix(".aver-waterfall-")
            .suffix(".lean")
            .tempfile_in(self.dir)
            .map_err(|e| e.to_string())?;
        write!(
            input,
            "{}{prefix}\n{body}\n#print axioms {name}\n{}",
            if search { "import waterfall\n" } else { "" },
            format::close_namespaces(prefix)
        )
        .map_err(|e| e.to_string())?;
        let path = input.path().to_str().ok_or("non-UTF8 proof path")?;
        self.command(
            &["env", "lean", "--json", path],
            search,
            self.options.waterfall_timeout,
        )
    }

    fn candidate(
        &mut self,
        prefix: &str,
        original: &str,
        c: &Candidate,
        clean: &BTreeSet<String>,
    ) -> Result<String, String> {
        if !self.options.selected(c) {
            return Ok(original.to_string());
        }
        eprintln!("waterfall: checking {}", c.label);
        writeln!(
            self.log,
            "\ncandidate {}",
            serde_json::to_string(c).unwrap()
        )
        .map_err(|e| e.to_string())?;
        let name = format::qualified_name(prefix, &c.name);
        if clean.contains(&name) {
            self.report
                .push(serde_json::json!({"law": c.label, "status": "existing-proof"}));
            return Ok(original.to_string());
        }
        if c.baseline_universal {
            let baseline = self.check(prefix, original, &c.name, false)?;
            if baseline.ok && format::axioms(&baseline.output, &name).is_some() {
                self.report
                    .push(serde_json::json!({"law": c.label, "status": "existing-proof"}));
                return Ok(original.to_string());
            }
        }
        let key = format!(
            "{:x}",
            Sha256::digest(format!("{prefix}\n{}", serde_json::to_string(c).unwrap()))
        );
        if let Some(script) = self.cache.scripts.get(&key).cloned() {
            let body = c.theorem(&script);
            let replay = self.check(prefix, &body, &c.name, false)?;
            if let Some(axioms) = replay
                .ok
                .then(|| format::axioms(&replay.output, &name))
                .flatten()
            {
                self.report.push(
                    serde_json::json!({"law": c.label, "status": "replayed", "axioms": axioms}),
                );
                return Ok(body);
            }
            self.cache.scripts.remove(&key);
        }
        self.prepare_search()?;
        for mode in ["search", "committed"] {
            eprintln!("waterfall: {} ({mode})", c.label);
            let script = format!(
                "waterfall? (mode := .{mode}) (effort := {}) [{}]",
                self.options.waterfall_effort,
                c.hints.join(", ")
            );
            let search = self.check(prefix, &c.theorem(&script), &c.name, true)?;
            if !search.ok || format::axioms(&search.output, &name).is_none() {
                continue;
            }
            let Some(script) = format::suggestion(&search.output) else {
                continue;
            };
            let body = c.theorem(&script);
            let replay = self.check(prefix, &body, &c.name, false)?;
            if let Some(axioms) = replay
                .ok
                .then(|| format::axioms(&replay.output, &name))
                .flatten()
            {
                self.cache.scripts.insert(key, script);
                self.report.push(serde_json::json!({"law": c.label, "status": "discovered", "mode": mode, "axioms": axioms}));
                return Ok(body);
            }
        }
        self.report
            .push(serde_json::json!({"law": c.label, "status": "unresolved"}));
        Ok(original.to_string())
    }

    fn module(&mut self, path: &Path) -> Result<(), String> {
        let source = fs::read_to_string(path).map_err(|e| e.to_string())?;
        if !source.lines().any(|line| line.starts_with(BEGIN)) {
            return Ok(());
        }
        let mut candidates = Vec::new();
        let mut offset = 0;
        while let Some(r) = format::region(&source[offset..])? {
            if self.options.selected(&r.candidate) {
                candidates.push((
                    format::qualified_name(&source[..offset + r.start], &r.candidate.name),
                    r.candidate,
                ));
            }
            offset += r.after;
        }
        let imports = format::imports(&source);
        let args: Vec<_> = std::iter::once("build")
            .chain(imports.iter().map(String::as_str))
            .collect();
        // Dependencies must be rebuilt after a supplier changes. Never search
        // against stale oleans from a failed dependency build.
        let ready =
            candidates.is_empty() || imports.is_empty() || self.command(&args, false, 300)?.ok;
        let mut clean = BTreeSet::new();
        if ready && candidates.len() >= 4 {
            // Audit the baseline in one elaboration on larger modules. The
            // unresolved candidates still get isolated retries after suppliers
            // improve; final whole-project checking remains authoritative.
            let audit = candidates
                .iter()
                .filter(|(_, c)| c.baseline_universal)
                .map(|(name, _)| format!("#print axioms {name}\n"))
                .collect::<String>();
            let mut input = tempfile::Builder::new()
                .prefix(".aver-waterfall-baseline-")
                .suffix(".lean")
                .tempfile_in(self.dir)
                .map_err(|e| e.to_string())?;
            write!(input, "{source}\n{audit}").map_err(|e| e.to_string())?;
            let run = self.command(
                &[
                    "env",
                    "lean",
                    "--json",
                    input.path().to_str().ok_or("non-UTF8 proof path")?,
                ],
                false,
                self.options.waterfall_timeout,
            )?;
            if run.ok {
                clean.extend(
                    candidates
                        .iter()
                        .filter(|(name, c)| {
                            c.baseline_universal && format::axioms(&run.output, name).is_some()
                        })
                        .map(|(name, _)| name.clone()),
                );
            }
        }
        let mut result = String::new();
        let mut remaining = source.as_str();
        while let Some(region) = format::region(remaining)? {
            result.push_str(&remaining[..region.start]);
            let body = if ready {
                self.candidate(
                    &result,
                    &remaining[region.body..region.end],
                    &region.candidate,
                    &clean,
                )?
            } else {
                if self.options.selected(&region.candidate) {
                    self.report.push(serde_json::json!({"law": region.candidate.label, "status": "dependency-build-failed"}));
                }
                remaining[region.body..region.end].to_string()
            };
            result.push_str(&body);
            remaining = &remaining[region.after..];
        }
        result.push_str(remaining);
        fs::write(path, result).map_err(|e| e.to_string())
    }
}

/// All paths and module edges come from the freshly generated project. The
/// retained JSON cache is untrusted input and can only supply a replay proposal.
pub fn run(output: &str, options: &Options, generated_files: &[String]) -> Result<(), String> {
    let dir = fs::canonicalize(output).map_err(|e| e.to_string())?;
    let waterfall = options.waterfall.as_ref().ok_or("missing waterfall path")?;
    let waterfall = if waterfall.is_absolute() {
        waterfall.clone()
    } else {
        std::env::current_dir()
            .map_err(|e| e.to_string())?
            .join(waterfall)
    };
    let cache_path = dir.join("proof_waterfall_cache.json");
    let cache: Cache = fs::read(&cache_path)
        .ok()
        .and_then(|bytes| serde_json::from_slice(&bytes).ok())
        .filter(|cache: &Cache| cache.version == 1)
        .unwrap_or(Cache {
            version: 1,
            scripts: BTreeMap::new(),
        });
    let mut runner = Runner {
        dir: &dir,
        options,
        waterfall,
        log: fs::File::create(dir.join("proof_waterfall.log")).map_err(|e| e.to_string())?,
        cache,
        report: Vec::new(),
        search_ready: false,
    };
    // Re-emission changes proof strategies, not the project's module set.
    // Only visit files returned by this invocation's exporter; an interrupted
    // older export may have left annotated files in the same output directory.
    let files: BTreeSet<_> = generated_files.iter().map(|path| dir.join(path)).collect();
    let mut ordered = Vec::new();
    let mut seen = BTreeSet::new();
    for path in &files {
        format::order(&dir, path, &files, &mut seen, &mut ordered)?;
    }
    let mut matched = BTreeSet::new();
    for path in &ordered {
        let source = fs::read_to_string(path).map_err(|e| e.to_string())?;
        let mut remaining = source.as_str();
        while let Some(r) = format::region(remaining)? {
            for law in &options.waterfall_law {
                if r.candidate.label == *law || r.candidate.label.starts_with(&format!("{law}.")) {
                    matched.insert(law);
                }
            }
            remaining = &remaining[r.after..];
        }
    }
    for law in &options.waterfall_law {
        if !matched.contains(law) {
            return Err(format!(
                "no emitted search candidate matches --waterfall-law {law}; use the law identity from proof_manifest.json"
            ));
        }
    }
    for path in ordered {
        runner.module(&path)?;
    }
    fs::write(
        cache_path,
        serde_json::to_vec_pretty(&runner.cache).unwrap(),
    )
    .map_err(|e| e.to_string())?;
    fs::write(
        dir.join("proof_waterfall.json"),
        serde_json::to_vec_pretty(&runner.report).unwrap(),
    )
    .map_err(|e| e.to_string())?;
    let retained = runner
        .report
        .iter()
        .filter(|r| matches!(r["status"].as_str(), Some("discovered" | "replayed")))
        .count();
    eprintln!(
        "waterfall: retained {retained} checked script(s); normal project verification follows"
    );
    Ok(())
}
