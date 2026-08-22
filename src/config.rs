/// Project configuration from `aver.toml`.
///
/// Currently supports runtime effect policies:
///   [effects.Http]   hosts = ["api.example.com", "*.internal.corp"]
///   [effects.Disk]   paths = ["./data/**"]
///   [effects.Env]    keys  = ["APP_*", "TOKEN"]
///   [effects.Tcp]    connect_timeout_secs = 5
///                    request_idle_timeout_secs = 30
///
/// And check-time warning suppression:
///   [[check.suppress]]
///   slug   = "non-tail-recursion"
///   files  = ["self_hosted/**"]
///   reason = "Tree walkers are structural recursive"
use std::collections::HashMap;
use std::path::Path;

mod providers;
pub use providers::{
    PROVIDER_MANIFEST_SCHEMA, ProviderPackageBinding, ProviderPackageManifest,
    ProviderPackageSource,
};

/// Runtime policy for a single effect namespace.
#[derive(Debug, Clone)]
pub struct EffectPolicy {
    /// Allowed HTTP hosts (exact or wildcard `*.domain`).
    pub hosts: Vec<String>,
    /// Allowed filesystem paths. Concrete paths and `<path>/**` include the
    /// same subtree; `.`, `./`, and `./**` mean the project-relative subtree,
    /// while `/` and `/**` mean the filesystem root.
    pub paths: Vec<String>,
    /// Allowed environment variable keys (exact or wildcard `PREFIX_*`).
    pub keys: Vec<String>,
}

/// Deployment settings consumed by the standard Tcp capability provider.
/// Persistent session reads and writes deliberately have no timeout; these
/// values govern only socket establishment and one-shot request calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TcpEffectSettings {
    pub connect_timeout_secs: u64,
    pub request_idle_timeout_secs: u64,
}

impl Default for TcpEffectSettings {
    fn default() -> Self {
        Self {
            connect_timeout_secs: aver_rt::tcp::DEFAULT_CONNECT_TIMEOUT_SECS,
            request_idle_timeout_secs: aver_rt::tcp::DEFAULT_REQUEST_IDLE_TIMEOUT_SECS,
        }
    }
}

impl TcpEffectSettings {
    pub fn native(self) -> aver_rt::tcp::TcpSettings {
        aver_rt::tcp::TcpSettings::from_secs(
            self.connect_timeout_secs,
            self.request_idle_timeout_secs,
        )
        .expect("ProjectConfig validates positive Tcp timeout settings")
    }
}

/// Per-project layer fingerprint, used by `aver shape` to override the
/// built-in v0 baseline. Buckets must cover all five `shape::Bucket`
/// values (match / recursion / pipeline / orchestration / helpers) and
/// sum to roughly 100.
#[derive(Debug, Clone)]
pub struct ShapeLayerFingerprint {
    pub name: String,
    pub match_pct: f64,
    pub recursion_pct: f64,
    pub pipeline_pct: f64,
    pub orchestration_pct: f64,
    pub helpers_pct: f64,
}

/// Per-project "this directory should belong to this architectural layer"
/// declaration. `aver shape --lint` walks these and flags any module whose
/// nearest-layer guess disagrees.
#[derive(Debug, Clone)]
pub struct ShapeExpected {
    pub glob: String,
    pub layer: String,
}

/// A single check-warning suppression rule.
#[derive(Debug, Clone)]
pub struct CheckSuppression {
    /// Diagnostic slug to suppress (e.g. `"non-tail-recursion"`).
    pub slug: String,
    /// Optional file glob patterns.  Empty = suppress globally.
    pub files: Vec<String>,
    /// Mandatory explanation — why the warning is acceptable.
    pub reason: String,
}

/// Per-case opcode budget installed before evaluating a verify case, and the
/// number `[verify] step-limit` overrides.
///
/// AFL nightly found tail-recursive shapes (`fn id(x) -> Int = id(-7)` and
/// friends — byte-havoc dropping the terminating arm) where Aver's TCO turns
/// infinite recursion into a goto-loop with no stack growth, so without this
/// cap the VM dispatch loop never terminates. 1M opcodes is 100× above what
/// real verify cases need (per-case eval is normally <10k steps on the bench
/// suite) and short enough that an unconverging case bails in ~5-50ms even on
/// a slow shared CI runner — well under AFL's default `AFL_HANG_TMOUT=1000ms`,
/// so fuzz-discovered infinite loops surface as proper `VmError::StepLimit`
/// instead of AFL hangs. The cap resets per `run_named_function`, so cases
/// don't share budget.
///
/// A project that raises it is trading that bail-out time away knowingly, per
/// function, in writing — which is what `[[verify.costly]]` is. The fuzz
/// targets construct the runner with no config at all, so they always run at
/// exactly this number.
pub const DEFAULT_VERIFY_STEP_LIMIT: u64 = 1_000_000;

/// Ceiling on how many cases one verify block may expand into, and the
/// number `[verify] max-cases` — or a `[[verify.costly]]` entry's own
/// `max-cases`, for the blocks it names — overrides.
///
/// It bounds both expansions of a `given` domain: the declared side the
/// parser performs, and the `--hostile` side the verify runner performs on
/// top of it (value boundaries × adversarial effect worlds × eval orders).
/// Both fail loudly with the count rather than truncating, because a
/// truncated case list is a claim the user did not make.
///
/// Each expanded case clones an expression pair, so this is a memory bound as
/// much as a time bound; raising it costs parse-time memory in proportion.
pub const DEFAULT_VERIFY_MAX_CASES: usize = 10_000;

/// A single `[[verify.costly]]` entry: the raised budgets for the verify
/// blocks of one function.
///
/// Same shape as [`CheckSuppression`] on purpose. Both are a project saying
/// "I know about this and here is why", so both require a written reason and
/// both scope themselves with optional anchored file globs.
///
/// It carries both `[verify]` dials, because both are checked per verify
/// block and a block belongs to exactly one function: `step-limit` says one
/// case of this fn may cost more, `max-cases` says this fn's `given` domain
/// may be wider. An entry may raise either or both; one that raises neither
/// is refused at load.
#[derive(Debug, Clone)]
pub struct VerifyCostly {
    /// Name of the function whose verify blocks get the raised budgets
    /// (e.g. `"checkScript"`).
    pub fn_name: String,
    /// Optional file glob patterns. Empty = every verified file.
    pub files: Vec<String>,
    /// The raised per-case budget, in VM steps. `None` = this entry leaves
    /// the step budget where the project put it.
    pub step_limit: Option<u64>,
    /// The raised ceiling on this fn's verify-case expansion. `None` = this
    /// entry leaves the ceiling where the project put it.
    pub max_cases: Option<usize>,
    /// Mandatory explanation — why this case is expected to be expensive.
    pub reason: String,
}

impl VerifyCostly {
    /// The `fn` half of the match rule. One implementation, so `step-limit`
    /// and `max-cases` can never come to disagree about which blocks an
    /// entry is about.
    pub fn matches_fn(&self, fn_name: &str) -> bool {
        self.fn_name == fn_name
    }

    /// The `files` half of the match rule: an entry with no globs covers
    /// every verified file.
    pub fn covers_file(&self, file_path: &str) -> bool {
        self.files.is_empty() || self.files.iter().any(|g| glob_matches(file_path, g))
    }
}

/// The ceiling on verify-case expansion in force while parsing one file.
///
/// The parser meets one file and many function names, so the `files` half of
/// the match rule is settled when this is built and the `fn` half waits for
/// the block being parsed. A parser built without a project holds
/// [`VerifyCaseCeiling::compiled_default`], which answers the compiled-in
/// number for every function.
#[derive(Debug, Clone)]
pub struct VerifyCaseCeiling {
    /// What a function no entry names gets: `[verify] max-cases`, or the
    /// compiled-in default.
    default: usize,
    /// The entries whose globs cover this file and that set `max-cases`, in
    /// `aver.toml` order — first match wins.
    raises: Vec<VerifyCostly>,
}

impl VerifyCaseCeiling {
    /// The ceiling with no project configuration at all — what every
    /// internal re-parse and every fuzz target parses under.
    pub fn compiled_default() -> Self {
        Self::flat(DEFAULT_VERIFY_MAX_CASES)
    }

    /// One number for every function, whatever it is called.
    pub fn flat(max_cases: usize) -> Self {
        VerifyCaseCeiling {
            default: max_cases,
            raises: Vec::new(),
        }
    }

    /// The ceiling for the verify blocks of `fn_name`: the first covering
    /// `[[verify.costly]]` entry that names it and sets `max-cases`, else
    /// the project default.
    pub fn for_fn(&self, fn_name: &str) -> usize {
        self.raises
            .iter()
            .find(|entry| entry.matches_fn(fn_name))
            .and_then(|entry| entry.max_cases)
            .unwrap_or(self.default)
    }
}

/// `[verify]` policy: how much work one verify case is allowed to be, and
/// how many cases a `given` domain is allowed to expand into.
///
/// Both are project policy, not program meaning: the same source verifies
/// the same way whatever these say, it just gets more or less room to
/// finish. `None` means "use the compiled-in default", so a project with no
/// `[verify]` section behaves exactly as it did before this existed.
#[derive(Debug, Clone, Default)]
pub struct VerifySettings {
    /// `[verify] step-limit` — the per-case default budget.
    pub step_limit: Option<u64>,
    /// `[verify] max-cases` — the cap on `given`-domain expansion, for every
    /// function no `[[verify.costly]]` entry raises.
    pub max_cases: Option<usize>,
    /// `[[verify.costly]]` entries, in file order.
    pub costly: Vec<VerifyCostly>,
}

/// How independent products (`!`/`?!`) are scheduled and how failures
/// propagate.
///
/// Per `docs/independence.md`: sequential and concurrent evaluation are
/// both valid under the language semantics. Pick the schedule that fits
/// the environment — threads for native CLI, single-thread for
/// wasm/playground.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum IndependenceMode {
    /// Parallel execution. Wait for all branches to finish, then report
    /// one error.
    #[default]
    Complete,
    /// Parallel execution. Signal siblings to stop as soon as one branch
    /// fails.
    Cancel,
    /// Sequential left-to-right execution. No threads. Valid per the
    /// language spec (any interleave is permitted, including the trivial
    /// fully-sequential one). Used by wasm builds that cannot spawn
    /// threads; also selectable for deterministic replay.
    Sequential,
}

/// Project-level configuration loaded from `aver.toml`.
#[derive(Debug, Clone)]
pub struct ProjectConfig {
    /// Effect namespace → policy.  Absence of a key means "allow all".
    pub effect_policies: HashMap<String, EffectPolicy>,
    /// Settings forwarded to the standard Tcp provider.
    pub tcp_settings: TcpEffectSettings,
    /// Whether `[effects.Tcp]` explicitly contained either timeout key.
    /// Targets that cannot honour the settings use this to warn rather than
    /// silently pretending they applied deployment configuration.
    pub tcp_settings_configured: bool,
    /// Check-time warning suppressions.
    pub check_suppressions: Vec<CheckSuppression>,
    /// `[verify]` budgets: per-case step limit, case-count ceiling, and the
    /// `[[verify.costly]]` entries that raise the first one per function.
    pub verify: VerifySettings,
    /// How `?!` products handle branch failure.
    pub independence_mode: IndependenceMode,
    /// Per-project layer fingerprints for `aver shape`. Empty = use the
    /// built-in v0 baseline.
    pub shape_layers: Vec<ShapeLayerFingerprint>,
    /// Path-glob → expected-layer declarations for `aver shape --lint`.
    /// Empty = `--lint` is a no-op (nothing to flag against).
    pub shape_expected: Vec<ShapeExpected>,
    /// Explicit static Cargo composition for native Rust capability providers.
    pub provider_manifest: Option<ProviderPackageManifest>,
}

impl ProjectConfig {
    /// Try to load `aver.toml` from the given directory.
    /// Returns `Ok(None)` if the file does not exist.
    /// Returns `Err` if the file exists but is malformed (parse errors, bad types).
    pub fn load_from_dir(dir: &Path) -> Result<Option<Self>, String> {
        let path = dir.join("aver.toml");
        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(format!("Failed to read {}: {}", path.display(), e)),
        };
        let mut config = Self::parse(&content)?;
        if let Some(manifest) = &mut config.provider_manifest {
            manifest.resolve_local_paths(dir)?;
        }
        Ok(Some(config))
    }

    /// Parse the TOML content into a ProjectConfig.
    pub fn parse(content: &str) -> Result<Self, String> {
        let table: toml::Table = content
            .parse()
            .map_err(|e: toml::de::Error| format!("aver.toml parse error: {}", e))?;

        let mut effect_policies = HashMap::new();
        let mut tcp_settings = TcpEffectSettings::default();
        let mut tcp_settings_configured = false;

        if let Some(toml::Value::Table(effects_table)) = table.get("effects") {
            for (name, value) in effects_table {
                let section = value
                    .as_table()
                    .ok_or_else(|| format!("aver.toml: [effects.{}] must be a table", name))?;

                validate_effect_section_keys(name, section)?;

                if name == "Tcp" {
                    if section.contains_key("connect_timeout_secs") {
                        tcp_settings_configured = true;
                    }
                    if section.contains_key("request_idle_timeout_secs") {
                        tcp_settings_configured = true;
                    }
                    tcp_settings.connect_timeout_secs = parse_positive_timeout_secs(
                        name,
                        section,
                        "connect_timeout_secs",
                        tcp_settings.connect_timeout_secs,
                    )?;
                    tcp_settings.request_idle_timeout_secs = parse_positive_timeout_secs(
                        name,
                        section,
                        "request_idle_timeout_secs",
                        tcp_settings.request_idle_timeout_secs,
                    )?;
                }

                let hosts = if let Some(val) = section.get("hosts") {
                    let arr = val.as_array().ok_or_else(|| {
                        format!("aver.toml: [effects.{}].hosts must be an array", name)
                    })?;
                    arr.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                                format!(
                                    "aver.toml: [effects.{}].hosts[{}] must be a string",
                                    name, i
                                )
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };
                for (index, host) in hosts.iter().enumerate() {
                    validate_host_pattern(name, index, host)?;
                }

                let paths = if let Some(val) = section.get("paths") {
                    let arr = val.as_array().ok_or_else(|| {
                        format!("aver.toml: [effects.{}].paths must be an array", name)
                    })?;
                    arr.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                                format!(
                                    "aver.toml: [effects.{}].paths[{}] must be a string",
                                    name, i
                                )
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };
                for (index, path) in paths.iter().enumerate() {
                    validate_path_pattern(name, index, path)?;
                }

                let keys = if let Some(val) = section.get("keys") {
                    let arr = val.as_array().ok_or_else(|| {
                        format!("aver.toml: [effects.{}].keys must be an array", name)
                    })?;
                    arr.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                                format!(
                                    "aver.toml: [effects.{}].keys[{}] must be a string",
                                    name, i
                                )
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?
                } else {
                    Vec::new()
                };
                for (index, key) in keys.iter().enumerate() {
                    validate_env_key_pattern(name, index, key)?;
                }

                effect_policies.insert(name.clone(), EffectPolicy { hosts, paths, keys });
            }
        }

        let check_suppressions = parse_check_suppressions(&table)?;
        let verify = parse_verify_settings(&table)?;
        let independence_mode = parse_independence_mode(&table)?;
        let (shape_layers, shape_expected) = parse_shape(&table)?;
        let provider_manifest = providers::parse_provider_manifest(&table)?;

        Ok(ProjectConfig {
            effect_policies,
            tcp_settings,
            tcp_settings_configured,
            check_suppressions,
            verify,
            independence_mode,
            shape_layers,
            shape_expected,
            provider_manifest,
        })
    }

    /// The per-case verify step budget this project asks for, before any
    /// `[[verify.costly]]` entry raises it.
    pub fn verify_step_limit(&self) -> u64 {
        self.verify.step_limit.unwrap_or(DEFAULT_VERIFY_STEP_LIMIT)
    }

    /// The ceiling on how many cases a `given` domain may expand into.
    pub fn verify_max_cases(&self) -> usize {
        self.verify.max_cases.unwrap_or(DEFAULT_VERIFY_MAX_CASES)
    }

    /// Whether the `[[verify.costly]]` entry at `idx` covers `file_path` by
    /// its file globs alone, ignoring `fn`. An entry with no globs covers
    /// every file. Separates "the entry points at a path this run never
    /// verified" from "the path was verified and no block of that fn ran".
    pub fn verify_costly_covers_file(&self, idx: usize, file_path: &str) -> bool {
        match self.verify.costly.get(idx) {
            Some(entry) => entry.covers_file(file_path),
            None => false,
        }
    }

    /// Whether the entry at `idx` applies to `fn_name` in `file_path`.
    pub fn verify_costly_applies(&self, idx: usize, fn_name: &str, file_path: &str) -> bool {
        match self.verify.costly.get(idx) {
            Some(entry) => entry.matches_fn(fn_name) && entry.covers_file(file_path),
            None => false,
        }
    }

    /// The ceiling on verify-case expansion for one file, resolved as far as
    /// a file path can resolve it. Handed to the parser, which supplies the
    /// function name of each block it parses.
    pub fn verify_case_ceiling(&self, file_path: &str) -> VerifyCaseCeiling {
        VerifyCaseCeiling {
            default: self.verify_max_cases(),
            raises: self
                .verify
                .costly
                .iter()
                .filter(|entry| entry.max_cases.is_some() && entry.covers_file(file_path))
                .cloned()
                .collect(),
        }
    }

    /// The ceiling for one function's verify block in one file: the first
    /// matching `[[verify.costly]]` entry that sets `max-cases`, else
    /// `[verify] max-cases`, else the compiled-in default.
    ///
    /// The runner's `--hostile` cartesian asks this per block, exactly as the
    /// parser asks the same question of the same entries per block.
    pub fn verify_max_cases_for(&self, fn_name: &str, file_path: &str) -> usize {
        self.verify_case_ceiling(file_path).for_fn(fn_name)
    }

    /// The budget for one function's verify block in one file: the default,
    /// raised by the most permissive matching `[[verify.costly]]` entry that
    /// sets `step-limit`.
    ///
    /// Returns the index of the entry that did the raising so the report can
    /// name it and so an entry that raised nothing can be called stale. Ties
    /// go to the earlier entry, so the answer does not depend on iteration
    /// order.
    pub fn verify_budget_for(&self, fn_name: &str, file_path: &str) -> (u64, Option<usize>) {
        let base = self.verify_step_limit();
        let raised = (0..self.verify.costly.len())
            .filter(|idx| self.verify_costly_applies(*idx, fn_name, file_path))
            .filter(|idx| {
                self.verify.costly[*idx]
                    .step_limit
                    .is_some_and(|l| l > base)
            })
            .max_by_key(|idx| (self.verify.costly[*idx].step_limit, std::cmp::Reverse(*idx)));
        match raised {
            Some(idx) => (
                self.verify.costly[idx].step_limit.unwrap_or(base),
                Some(idx),
            ),
            None => (base, None),
        }
    }

    /// True when at least one `[[shape.expected]]` glob matches `file_path`.
    /// `--lint` consults this to decide whether the file has a declared
    /// expected layer to compare against.
    pub fn shape_expected_for(&self, file_path: &str) -> Option<&str> {
        self.shape_expected
            .iter()
            .filter(|e| glob_matches(file_path, &e.glob))
            // Prefer the most specific (longest) glob — same rule as in
            // PR review.
            .max_by_key(|e| e.glob.len())
            .map(|e| e.layer.as_str())
    }

    /// Returns `true` if a diagnostic with the given `slug` at `file_path`
    /// is suppressed by any `[[check.suppress]]` rule.
    pub fn is_check_suppressed(&self, slug: &str, file_path: &str) -> bool {
        (0..self.check_suppressions.len())
            .any(|idx| self.check_suppression_applies(idx, slug, file_path))
    }

    /// Whether the rule at `idx` waives `slug` for `file_path`. Callers that
    /// report on waiver hygiene need to know *which* rules did the work, not
    /// just that some rule did.
    pub fn check_suppression_applies(&self, idx: usize, slug: &str, file_path: &str) -> bool {
        match self.check_suppressions.get(idx) {
            Some(rule) => rule.slug == slug && self.suppression_covers_file(idx, file_path),
            None => false,
        }
    }

    /// Whether the rule at `idx` covers `file_path` by its file globs alone,
    /// ignoring the slug. A rule with no globs covers every file. This is what
    /// separates "the waiver points at a path nothing in the run touched" from
    /// "the file was checked and the warning it waives no longer fires".
    pub fn suppression_covers_file(&self, idx: usize, file_path: &str) -> bool {
        match self.check_suppressions.get(idx) {
            Some(rule) => {
                rule.files.is_empty() || rule.files.iter().any(|g| glob_matches(file_path, g))
            }
            None => false,
        }
    }

    /// Check whether an HTTP call to `url_str` is allowed by the policy.
    /// Returns Ok(()) if allowed, Err(message) if denied.
    pub fn check_http_host(&self, method_name: &str, url_str: &str) -> Result<(), String> {
        // Find the most specific matching policy: first try "Http.get", then "Http"
        let namespace = method_name.split('.').next().unwrap_or(method_name);
        let policy = self
            .effect_policies
            .get(method_name)
            .or_else(|| self.effect_policies.get(namespace));

        let Some(policy) = policy else {
            return Ok(()); // No policy = allow all
        };

        if policy.hosts.is_empty() {
            return Ok(()); // Empty hosts list = allow all
        }

        let parsed = url::Url::parse(url_str).map_err(|e| {
            format!(
                "{} denied by aver.toml: invalid URL '{}': {}",
                method_name, url_str, e
            )
        })?;

        let host = parsed.host_str().unwrap_or("");

        for allowed in &policy.hosts {
            if host_matches(host, allowed) {
                return Ok(());
            }
        }

        Err(format!(
            "{} to '{}' denied by aver.toml policy (host '{}' not in allowed list)",
            method_name, url_str, host
        ))
    }

    /// Check whether a Disk operation on `path_str` is allowed by the policy.
    /// Returns Ok(()) if allowed, Err(message) if denied.
    pub fn check_disk_path(&self, method_name: &str, path_str: &str) -> Result<(), String> {
        let namespace = method_name.split('.').next().unwrap_or(method_name);
        let policy = self
            .effect_policies
            .get(method_name)
            .or_else(|| self.effect_policies.get(namespace));

        let Some(policy) = policy else {
            return Ok(());
        };

        if policy.paths.is_empty() {
            return Ok(());
        }

        // Normalize the path to prevent ../ traversal
        let normalized = normalize_path(path_str);

        for allowed in &policy.paths {
            if path_matches(&normalized, allowed) {
                return Ok(());
            }
        }

        Err(format!(
            "{} on '{}' denied by aver.toml policy (path not in allowed list)",
            method_name, path_str
        ))
    }

    /// Check whether an Env operation on `key` is allowed by the policy.
    /// Returns Ok(()) if allowed, Err(message) if denied.
    pub fn check_env_key(&self, method_name: &str, key: &str) -> Result<(), String> {
        let namespace = method_name.split('.').next().unwrap_or(method_name);
        let policy = self
            .effect_policies
            .get(method_name)
            .or_else(|| self.effect_policies.get(namespace));

        let Some(policy) = policy else {
            return Ok(());
        };

        if policy.keys.is_empty() {
            return Ok(());
        }

        for allowed in &policy.keys {
            if env_key_matches(key, allowed) {
                return Ok(());
            }
        }

        Err(format!(
            "{} on '{}' denied by aver.toml policy (key not in allowed list)",
            method_name, key
        ))
    }
}

fn validate_effect_section_keys(
    effect: &str,
    section: &toml::map::Map<String, toml::Value>,
) -> Result<(), String> {
    let namespace = effect.split('.').next().unwrap_or(effect);
    let allowed: &[&str] = match (namespace, effect) {
        ("Http", _) => &["hosts"],
        ("Disk", _) => &["paths"],
        ("Env", _) => &["keys"],
        ("Tcp", "Tcp") => &["connect_timeout_secs", "request_idle_timeout_secs"],
        ("Tcp", _) => &[],
        _ => &[],
    };

    for key in section.keys() {
        if !allowed.contains(&key.as_str()) {
            let expected = if namespace == "Tcp" && effect != "Tcp" {
                "Tcp timeout settings belong under [effects.Tcp], not a method section".to_string()
            } else if allowed.is_empty() {
                format!("[effects.{effect}] has no supported settings")
            } else {
                format!("expected one of: {}", allowed.join(", "))
            };
            return Err(format!(
                "aver.toml: unknown or misplaced key [effects.{effect}].{key}; {expected}"
            ));
        }
    }
    Ok(())
}

fn parse_positive_timeout_secs(
    effect: &str,
    section: &toml::map::Map<String, toml::Value>,
    key: &str,
    default: u64,
) -> Result<u64, String> {
    let Some(value) = section.get(key) else {
        return Ok(default);
    };
    let seconds = value
        .as_integer()
        .ok_or_else(|| format!("aver.toml: [effects.{effect}].{key} must be a positive integer"))?;
    u64::try_from(seconds)
        .ok()
        .filter(|seconds| *seconds > 0)
        .ok_or_else(|| format!("aver.toml: [effects.{effect}].{key} must be greater than zero"))
}

/// Check if a hostname matches an allowed pattern.
/// Supports exact match and wildcard prefix `*.domain`.
fn host_matches(host: &str, pattern: &str) -> bool {
    if pattern == host {
        return true;
    }
    if let Some(suffix) = pattern.strip_prefix("*.") {
        // *.example.com matches sub.example.com but not example.com itself
        host.ends_with(suffix)
            && host.len() > suffix.len()
            && host.as_bytes()[host.len() - suffix.len() - 1] == b'.'
    } else {
        false
    }
}

/// Normalize a filesystem path for matching.
/// Resolves `.` and `..` components without touching the filesystem.
/// Leading `..` components are preserved (not silently dropped) so that
/// `../../etc/passwd` does NOT normalize to `etc/passwd`.
fn normalize_path(path: &str) -> String {
    let path = Path::new(path);
    let mut components: Vec<String> = Vec::new();
    let mut is_absolute = false;

    for comp in path.components() {
        match comp {
            std::path::Component::RootDir => {
                is_absolute = true;
                components.clear();
            }
            std::path::Component::CurDir => {} // skip .
            std::path::Component::ParentDir => {
                // Only pop if the last component is a normal segment (not "..")
                if components.last().is_some_and(|c| c != "..") {
                    components.pop();
                } else if !is_absolute {
                    // Preserve leading ".." for relative paths — never silently drop them
                    components.push("..".to_string());
                }
                // For absolute paths, extra ".." at root is a no-op (stays at /)
            }
            std::path::Component::Normal(s) => {
                components.push(s.to_string_lossy().to_string());
            }
            std::path::Component::Prefix(p) => {
                components.push(p.as_os_str().to_string_lossy().to_string());
            }
        }
    }

    let joined = components.join("/");
    if is_absolute {
        format!("/{}", joined)
    } else {
        joined
    }
}

/// Check if a normalized path matches an allowed pattern.
/// Concrete paths and `<path>/**` include the same subtree. `.`, `./`, and
/// `./**` match the project-relative subtree; `/` and `/**` match absolute
/// paths from the filesystem root. Other `*` forms and `..`-rooted patterns
/// never match.
fn path_matches(normalized: &str, pattern: &str) -> bool {
    if pattern.is_empty() || pattern == "**" {
        return false;
    }

    let body = match pattern.strip_suffix("/**") {
        Some("") => "/",
        Some(base) => base,
        None => pattern,
    };
    if body.contains('*') {
        return false;
    }

    let base = normalize_path(body);
    if base.is_empty() {
        return !normalized.starts_with('/')
            && normalized != ".."
            && !normalized.starts_with("../");
    }
    if base == "/" {
        return normalized.starts_with('/');
    }
    if base == ".." || base.starts_with("../") {
        return false;
    }

    normalized == base
        || (normalized.len() > base.len()
            && normalized.starts_with(&base)
            && normalized.as_bytes()[base.len()] == b'/')
}

fn validate_path_pattern(effect: &str, index: usize, raw: &str) -> Result<(), String> {
    let prefix = format!("aver.toml: [effects.{effect}].paths[{index}]");
    if raw.is_empty() {
        return Err(format!(
            "{prefix} is empty; use \".\" for the project directory or \"/\" for the filesystem root"
        ));
    }
    if raw == "**" {
        return Err(format!(
            "{prefix} is ambiguous: '**'; use \"./**\" for the project subtree or \"/**\" for the filesystem root"
        ));
    }

    let body = match raw.strip_suffix("/**") {
        Some("") => "/",
        Some(base) => base,
        None => raw,
    };
    if body.contains('*') {
        return Err(format!(
            "{prefix} contains an unsupported glob '{raw}'; only a trailing \"/**\" is supported (for example, \"./data/**\")"
        ));
    }

    let base = normalize_path(body);
    if base == ".." || base.starts_with("../") {
        return Err(format!(
            "{prefix} escapes the project directory: '{raw}'; use an absolute pattern (for example, \"/srv/data/**\") to allow files outside the project"
        ));
    }

    Ok(())
}

fn validate_host_pattern(effect: &str, index: usize, raw: &str) -> Result<(), String> {
    if raw == "*" || raw == "**" {
        return Err(format!(
            "aver.toml: [effects.{effect}].hosts[{index}] contains an unsupported wildcard '{raw}'; use an exact host or a subdomain wildcard such as \"*.example.com\""
        ));
    }
    Ok(())
}

fn validate_env_key_pattern(effect: &str, index: usize, raw: &str) -> Result<(), String> {
    if raw == "**" {
        return Err(format!(
            "aver.toml: [effects.{effect}].keys[{index}] contains an unsupported wildcard '**'; use \"*\" for every key or a prefix wildcard such as \"APP_*\""
        ));
    }
    Ok(())
}

/// Check if an env key matches an allowed pattern.
/// Supports exact match and suffix wildcard `PREFIX_*`.
fn env_key_matches(key: &str, pattern: &str) -> bool {
    if pattern == key {
        return true;
    }
    if let Some(prefix) = pattern.strip_suffix('*') {
        key.starts_with(prefix)
    } else {
        false
    }
}

/// Parse `[independence]` section from the top-level TOML table.
#[allow(clippy::type_complexity)]
fn parse_shape(
    table: &toml::Table,
) -> Result<(Vec<ShapeLayerFingerprint>, Vec<ShapeExpected>), String> {
    let Some(toml::Value::Table(shape_table)) = table.get("shape") else {
        return Ok((Vec::new(), Vec::new()));
    };

    // Both keys are optional; missing == empty.
    let mut layers = Vec::new();
    if let Some(val) = shape_table.get("layer") {
        let arr = val
            .as_array()
            .ok_or_else(|| "aver.toml: [[shape.layer]] must be an array of tables".to_string())?;
        let pct = |t: &toml::Table, key: &str, idx: usize| -> Result<f64, String> {
            t.get(key)
                .and_then(|v| match v {
                    toml::Value::Float(f) => Some(*f),
                    toml::Value::Integer(i) => Some(*i as f64),
                    _ => None,
                })
                .ok_or_else(|| {
                    format!(
                        "aver.toml: [[shape.layer]][{}] requires numeric `{}` (percentage 0..100)",
                        idx, key
                    )
                })
        };
        for (i, entry) in arr.iter().enumerate() {
            let t = entry
                .as_table()
                .ok_or_else(|| format!("aver.toml: [[shape.layer]][{}] must be a table", i))?;
            let name = t
                .get("name")
                .and_then(|v| v.as_str())
                .ok_or_else(|| {
                    format!(
                        "aver.toml: [[shape.layer]][{}] requires string `name` (e.g. \"Domain\")",
                        i
                    )
                })?
                .to_string();
            layers.push(ShapeLayerFingerprint {
                name,
                match_pct: pct(t, "match", i)?,
                recursion_pct: pct(t, "recursion", i)?,
                pipeline_pct: pct(t, "pipeline", i)?,
                orchestration_pct: pct(t, "orchestration", i)?,
                helpers_pct: pct(t, "helpers", i)?,
            });
        }
    }

    let mut expected = Vec::new();
    if let Some(val) = shape_table.get("expected") {
        let arr = val.as_array().ok_or_else(|| {
            "aver.toml: [[shape.expected]] must be an array of tables".to_string()
        })?;
        for (i, entry) in arr.iter().enumerate() {
            let t = entry
                .as_table()
                .ok_or_else(|| format!("aver.toml: [[shape.expected]][{}] must be a table", i))?;
            let glob = t
                .get("glob")
                .and_then(|v| v.as_str())
                .ok_or_else(|| {
                    format!(
                        "aver.toml: [[shape.expected]][{}] requires string `glob` (e.g. \"src/parse/**\")",
                        i
                    )
                })?
                .to_string();
            let layer = t
                .get("layer")
                .and_then(|v| v.as_str())
                .ok_or_else(|| {
                    format!(
                        "aver.toml: [[shape.expected]][{}] requires string `layer` (e.g. \"Parse\")",
                        i
                    )
                })?
                .to_string();
            expected.push(ShapeExpected { glob, layer });
        }
    }

    Ok((layers, expected))
}

fn parse_independence_mode(table: &toml::Table) -> Result<IndependenceMode, String> {
    let section = match table.get("independence") {
        Some(toml::Value::Table(t)) => t,
        Some(_) => return Err("[independence] must be a table".to_string()),
        None => return Ok(IndependenceMode::default()),
    };
    match section.get("mode") {
        Some(toml::Value::String(s)) => match s.as_str() {
            "complete" => Ok(IndependenceMode::Complete),
            "cancel" => Ok(IndependenceMode::Cancel),
            "sequential" => Ok(IndependenceMode::Sequential),
            other => Err(format!(
                "[independence] mode must be \"complete\", \"cancel\", or \"sequential\", got {:?}",
                other
            )),
        },
        Some(_) => Err("[independence] mode must be a string".to_string()),
        None => Ok(IndependenceMode::default()),
    }
}

/// Parse `[[check.suppress]]` entries from the top-level TOML table.
fn parse_check_suppressions(table: &toml::Table) -> Result<Vec<CheckSuppression>, String> {
    let check_table = match table.get("check") {
        Some(toml::Value::Table(t)) => t,
        Some(_) => return Err("aver.toml: [check] must be a table".to_string()),
        None => return Ok(Vec::new()),
    };

    let arr = match check_table.get("suppress") {
        Some(toml::Value::Array(a)) => a,
        Some(_) => {
            return Err("aver.toml: [[check.suppress]] must be an array of tables".to_string());
        }
        None => return Ok(Vec::new()),
    };

    let mut suppressions = Vec::new();
    for (i, entry) in arr.iter().enumerate() {
        let t = entry
            .as_table()
            .ok_or_else(|| format!("aver.toml: [[check.suppress]][{}] must be a table", i))?;

        let slug = t
            .get("slug")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                format!(
                    "aver.toml: [[check.suppress]][{}] requires a string `slug`",
                    i
                )
            })?
            .to_string();

        let reason = t
            .get("reason")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                format!(
                    "aver.toml: [[check.suppress]][{}] requires a string `reason` — explain why this warning is acceptable",
                    i
                )
            })?
            .to_string();

        if reason.trim().is_empty() {
            return Err(format!(
                "aver.toml: [[check.suppress]][{}] `reason` must not be empty",
                i
            ));
        }

        let files = if let Some(val) = t.get("files") {
            let arr = val.as_array().ok_or_else(|| {
                format!(
                    "aver.toml: [[check.suppress]][{}].files must be an array",
                    i
                )
            })?;
            arr.iter()
                .enumerate()
                .map(|(j, v)| {
                    v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                        format!(
                            "aver.toml: [[check.suppress]][{}].files[{}] must be a string",
                            i, j
                        )
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
        } else {
            Vec::new()
        };

        suppressions.push(CheckSuppression {
            slug,
            files,
            reason,
        });
    }

    Ok(suppressions)
}

/// Parse the `[verify]` section and its `[[verify.costly]]` entries.
///
/// Deliberately the same shape, the same validation and the same voice as
/// [`parse_check_suppressions`]: `fn` is what `slug` is, `reason` is
/// mandatory and non-empty for the same reason, and `files` is the same
/// optional anchored glob list.
fn parse_verify_settings(table: &toml::Table) -> Result<VerifySettings, String> {
    let verify_table = match table.get("verify") {
        Some(toml::Value::Table(t)) => t,
        Some(_) => return Err("aver.toml: [verify] must be a table".to_string()),
        None => return Ok(VerifySettings::default()),
    };

    let step_limit = parse_positive_u64(verify_table, "[verify]", "step-limit")?;
    let max_cases = parse_positive_u64(verify_table, "[verify]", "max-cases")?.map(|n| n as usize);

    let arr = match verify_table.get("costly") {
        Some(toml::Value::Array(a)) => a.clone(),
        Some(_) => {
            return Err("aver.toml: [[verify.costly]] must be an array of tables".to_string());
        }
        None => Vec::new(),
    };

    let mut costly = Vec::new();
    for (i, entry) in arr.iter().enumerate() {
        let t = entry
            .as_table()
            .ok_or_else(|| format!("aver.toml: [[verify.costly]][{}] must be a table", i))?;

        let fn_name = t
            .get("fn")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                format!(
                    "aver.toml: [[verify.costly]][{}] requires a string `fn` — the function whose verify cases are expensive",
                    i
                )
            })?
            .to_string();

        let reason = t
            .get("reason")
            .and_then(|v| v.as_str())
            .ok_or_else(|| {
                format!(
                    "aver.toml: [[verify.costly]][{}] requires a string `reason` — say why this case is expected to be expensive",
                    i
                )
            })?
            .to_string();

        if reason.trim().is_empty() {
            return Err(format!(
                "aver.toml: [[verify.costly]][{}] `reason` must not be empty",
                i
            ));
        }

        let context = format!("[[verify.costly]][{}]", i);
        let step_limit = parse_positive_u64(t, &context, "step-limit")?;
        let max_cases = parse_positive_u64(t, &context, "max-cases")?.map(|n| n as usize);

        if step_limit.is_none() && max_cases.is_none() {
            return Err(format!(
                "aver.toml: [[verify.costly]][{}] raises nothing — set `step-limit`, `max-cases`, or both",
                i
            ));
        }

        let files = if let Some(val) = t.get("files") {
            let arr = val.as_array().ok_or_else(|| {
                format!("aver.toml: [[verify.costly]][{}].files must be an array", i)
            })?;
            arr.iter()
                .enumerate()
                .map(|(j, v)| {
                    v.as_str().map(|s| s.to_string()).ok_or_else(|| {
                        format!(
                            "aver.toml: [[verify.costly]][{}].files[{}] must be a string",
                            i, j
                        )
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
        } else {
            Vec::new()
        };

        costly.push(VerifyCostly {
            fn_name,
            files,
            step_limit,
            max_cases,
            reason,
        });
    }

    Ok(VerifySettings {
        step_limit,
        max_cases,
        costly,
    })
}

/// Read an optional positive-integer key out of a `[verify]`-family table.
/// Absent is `None`; present but zero, negative or non-integer is an error —
/// a budget of zero would decline every case, which nobody means to write.
fn parse_positive_u64(
    table: &toml::Table,
    context: &str,
    key: &str,
) -> Result<Option<u64>, String> {
    match table.get(key) {
        None => Ok(None),
        Some(toml::Value::Integer(n)) if *n > 0 => Ok(Some(*n as u64)),
        Some(_) => Err(format!(
            "aver.toml: {} `{}` must be a positive integer",
            context, key
        )),
    }
}

/// Drop any leading `./` segments. Matching is anchored, so `./a/b.av` and
/// `a/b.av` would otherwise be different paths to the same file.
fn strip_dot_slash(s: &str) -> &str {
    let mut rest = s;
    while let Some(stripped) = rest.strip_prefix("./") {
        rest = stripped;
    }
    rest
}

/// Simple glob match for file paths.
/// Supports `**` (any path segments) and `*` (any single segment chars).
/// A leading `./` is insignificant on both sides.
fn glob_matches(path: &str, pattern: &str) -> bool {
    // Normalize separators
    let path = path.replace('\\', "/");
    let pattern = pattern.replace('\\', "/");
    glob_match_recursive(
        strip_dot_slash(&path).as_bytes(),
        strip_dot_slash(&pattern).as_bytes(),
    )
}

fn glob_match_recursive(path: &[u8], pattern: &[u8]) -> bool {
    match (pattern.first(), path.first()) {
        (None, None) => true,
        (None, Some(_)) => false,
        (Some(b'*'), _) if pattern.starts_with(b"**/") => {
            // "**/" matches zero or more path segments
            let rest = &pattern[3..];
            // Try matching at current position (zero segments)
            if glob_match_recursive(path, rest) {
                return true;
            }
            // Try skipping path segments
            for i in 0..path.len() {
                if path[i] == b'/' && glob_match_recursive(&path[i + 1..], rest) {
                    return true;
                }
            }
            false
        }
        (Some(b'*'), _) if pattern == b"**" => true,
        (Some(b'*'), _) => {
            // Single `*` matches anything except `/`
            let rest = &pattern[1..];
            // Try consuming 0..N non-slash chars
            if glob_match_recursive(path, rest) {
                return true;
            }
            for i in 0..path.len() {
                if path[i] == b'/' {
                    break;
                }
                if glob_match_recursive(&path[i + 1..], rest) {
                    return true;
                }
            }
            false
        }
        (Some(&pc), Some(&bc)) if pc == bc => glob_match_recursive(&path[1..], &pattern[1..]),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The path matcher is duplicated into every artifact that enforces a
    /// policy without linking this crate: the two generated-Rust templates and
    /// the self-hosted replay support. A fix applied only here would still ship
    /// a compiler that emits the old behaviour into user projects, which is how
    /// the inverted `./**` semantics survived. Each copy is checked for the
    /// three decisions that define the semantics rather than for byte equality,
    /// so the copies may differ in formatting but not in meaning.
    #[test]
    fn path_matcher_copies_agree_on_root_pattern_semantics() {
        const MARKERS: [(&str, &str); 3] = [
            (
                r#"pattern == "**""#,
                "bare `**` is refused, never allow-all",
            ),
            (
                r#"Some("") => "/""#,
                "a bare trailing `/**` means the filesystem root",
            ),
            (r#"base == "/""#, "the root base admits absolute paths only"),
        ];
        let copies: [(&str, &str); 4] = [
            ("src/config.rs", include_str!("config.rs")),
            (
                "src/codegen/rust/policy.rs",
                include_str!("codegen/rust/policy.rs"),
            ),
            (
                "src/codegen/rust/replay.rs",
                include_str!("codegen/rust/replay.rs"),
            ),
            (
                "src/self_host/replay_support.rs",
                include_str!("self_host/replay_support.rs"),
            ),
        ];
        for (name, body) in copies {
            for (marker, meaning) in MARKERS {
                assert!(
                    body.contains(marker),
                    "{name} is missing `{marker}` — {meaning}. \
                     Every copy of path_matches must carry the same decisions; \
                     update all of them together."
                );
            }
        }

        for (name, body) in [
            (
                "src/codegen/rust/replay.rs",
                include_str!("codegen/rust/replay.rs"),
            ),
            (
                "src/self_host/replay_support.rs",
                include_str!("self_host/replay_support.rs"),
            ),
        ] {
            for marker in [
                "validate_effect_section_keys(name, section)",
                "connect_timeout_secs",
                "request_idle_timeout_secs",
            ] {
                assert!(
                    body.contains(marker),
                    "{name} is missing `{marker}` — runtime-loaded policy must mirror ProjectConfig"
                );
            }
        }
    }

    #[test]
    fn test_parse_empty_toml() {
        let config = ProjectConfig::parse("").unwrap();
        assert!(config.effect_policies.is_empty());
        assert_eq!(config.tcp_settings, TcpEffectSettings::default());
        assert!(!config.tcp_settings_configured);
    }

    #[test]
    fn test_parse_tcp_provider_settings() {
        let config = ProjectConfig::parse(
            "[effects.Tcp]\nconnect_timeout_secs = 7\nrequest_idle_timeout_secs = 45\n",
        )
        .unwrap();
        assert_eq!(
            config.tcp_settings,
            TcpEffectSettings {
                connect_timeout_secs: 7,
                request_idle_timeout_secs: 45,
            }
        );
        assert!(config.tcp_settings_configured);
    }

    #[test]
    fn tcp_provider_settings_must_be_positive_integers() {
        for (key, value, phrase) in [
            ("connect_timeout_secs", "0", "greater than zero"),
            ("request_idle_timeout_secs", "-1", "greater than zero"),
            ("connect_timeout_secs", "1.5", "positive integer"),
            ("request_idle_timeout_secs", "\"30\"", "positive integer"),
        ] {
            let error = ProjectConfig::parse(&format!("[effects.Tcp]\n{key} = {value}\n"))
                .expect_err("invalid Tcp setting must be rejected");
            assert!(error.contains(key) && error.contains(phrase), "{error}");
        }
    }

    #[test]
    fn effect_sections_reject_unknown_and_misplaced_keys() {
        for (source, path) in [
            (
                "[effects.Tcp]\npaths = [\"./data/**\"]\n",
                "[effects.Tcp].paths",
            ),
            (
                "[effects.Http]\nrequest_idle_timeout_secs = 30\n",
                "[effects.Http].request_idle_timeout_secs",
            ),
            (
                "[effects.Disk]\nhosts = [\"example.com\"]\n",
                "[effects.Disk].hosts",
            ),
            (
                "[effects.\"Tcp.readBytes\"]\nconnect_timeout_secs = 5\n",
                "[effects.Tcp.readBytes].connect_timeout_secs",
            ),
            (
                "[effects.Unknown]\nthing = true\n",
                "[effects.Unknown].thing",
            ),
        ] {
            let error = ProjectConfig::parse(source)
                .expect_err("unknown or misplaced effect key must be rejected");
            assert!(error.contains(path), "{error}");
        }
    }

    #[test]
    fn test_parse_http_hosts() {
        let toml = r#"
[effects.Http]
hosts = ["api.example.com", "*.internal.corp"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let policy = config.effect_policies.get("Http").unwrap();
        assert_eq!(policy.hosts.len(), 2);
        assert_eq!(policy.hosts[0], "api.example.com");
        assert_eq!(policy.hosts[1], "*.internal.corp");
    }

    #[test]
    fn test_parse_disk_paths() {
        let toml = r#"
[effects.Disk]
paths = ["./data/**"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let policy = config.effect_policies.get("Disk").unwrap();
        assert_eq!(policy.paths, vec!["./data/**"]);
    }

    #[test]
    fn test_parse_env_keys() {
        let toml = r#"
[effects.Env]
keys = ["APP_*", "TOKEN"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let policy = config.effect_policies.get("Env").unwrap();
        assert_eq!(policy.keys, vec!["APP_*", "TOKEN"]);
    }

    #[test]
    fn test_check_http_host_allowed() {
        let toml = r#"
[effects.Http]
hosts = ["api.example.com"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://api.example.com/data")
                .is_ok()
        );
    }

    #[test]
    fn test_check_http_host_denied() {
        let toml = r#"
[effects.Http]
hosts = ["api.example.com"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let result = config.check_http_host("Http.get", "https://evil.com/data");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("denied by aver.toml"));
    }

    #[test]
    fn test_check_http_host_wildcard() {
        let toml = r#"
[effects.Http]
hosts = ["*.internal.corp"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://api.internal.corp/data")
                .is_ok()
        );
        assert!(
            config
                .check_http_host("Http.get", "https://internal.corp/data")
                .is_err()
        );
    }

    #[test]
    fn disk_path_pattern_matrix() {
        let cases = [
            // pattern, loads, relative, absolute, escaping relative
            ("", false, false, false, false),
            (".", true, true, false, false),
            ("./", true, true, false, false),
            ("./**", true, true, false, false),
            ("**", false, false, false, false),
            ("/", true, false, true, false),
            ("/**", true, false, true, false),
            ("./data/**", true, true, false, false),
            ("../**", false, false, false, false),
        ];

        for (pattern, loads, allows_relative, allows_absolute, allows_escape) in cases {
            let probes = [
                ("data/ok.txt", allows_relative),
                ("/etc/passwd", allows_absolute),
                ("../outside.txt", allows_escape),
            ];
            for (probe, expected) in probes {
                assert_eq!(
                    path_matches(&normalize_path(probe), pattern),
                    expected,
                    "matcher verdict for pattern {pattern:?} and path {probe:?}"
                );
            }

            let parsed = ProjectConfig::parse(&format!("[effects.Disk]\npaths = [{pattern:?}]\n"));
            assert_eq!(
                parsed.is_ok(),
                loads,
                "config-load verdict for pattern {pattern:?}: {parsed:?}"
            );
            if let Ok(config) = parsed {
                for (probe, expected) in probes {
                    assert_eq!(
                        config.check_disk_path("Disk.readText", probe).is_ok(),
                        expected,
                        "policy verdict for pattern {pattern:?} and path {probe:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn disk_path_degenerate_patterns_are_config_errors() {
        let cases = [
            ("", "is empty"),
            ("**", "use \"./**\" for the project subtree or \"/**\""),
            ("*", "unsupported glob"),
            ("./*", "unsupported glob"),
            ("*.txt", "unsupported glob"),
            ("data/**/logs", "unsupported glob"),
            ("..", "escapes the project directory"),
            ("../**", "escapes the project directory"),
            ("./data/../..", "escapes the project directory"),
        ];

        for (pattern, phrase) in cases {
            let error = ProjectConfig::parse(&format!("[effects.Disk]\npaths = [{pattern:?}]\n"))
                .expect_err("degenerate path pattern must be rejected");
            assert!(
                error.contains("[effects.Disk].paths[0]") && error.contains(phrase),
                "unexpected error for pattern {pattern:?}: {error}"
            );
        }
    }

    #[test]
    fn disk_path_project_root_pattern_denies_absolute() {
        let config = ProjectConfig::parse("[effects.Disk]\npaths = [\"./**\"]\n").unwrap();
        assert!(
            config
                .check_disk_path("Disk.readText", "data/ok.txt")
                .is_ok()
        );
        assert!(
            config
                .check_disk_path("Disk.readText", "/etc/passwd")
                .is_err()
        );
    }

    #[test]
    fn disk_path_plain_and_recursive_forms_agree() {
        let plain = ProjectConfig::parse("[effects.Disk]\npaths = [\"./data\"]\n").unwrap();
        let recursive = ProjectConfig::parse("[effects.Disk]\npaths = [\"./data/**\"]\n").unwrap();

        for probe in ["data", "data/sub/x", "datax/y", "/data/x"] {
            assert_eq!(
                plain.check_disk_path("Disk.readText", probe).is_ok(),
                recursive.check_disk_path("Disk.readText", probe).is_ok(),
                "plain and recursive forms differ for {probe:?}"
            );
        }
    }

    #[test]
    fn test_check_disk_path_traversal_blocked() {
        let toml = r#"
[effects.Disk]
paths = ["./data/**"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        // data/../etc/passwd normalizes to etc/passwd — not under data/
        assert!(
            config
                .check_disk_path("Disk.readText", "data/../etc/passwd")
                .is_err()
        );
        // Leading ../ must NOT be silently dropped — ../../data/x must NOT match data/**
        assert!(
            config
                .check_disk_path("Disk.readText", "../../data/secret")
                .is_err()
        );
        // More leading dotdots
        assert!(
            config
                .check_disk_path("Disk.readText", "../../../etc/passwd")
                .is_err()
        );
    }

    #[test]
    fn test_no_policy_allows_all() {
        let config = ProjectConfig::parse("").unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://anything.com/data")
                .is_ok()
        );
        assert!(config.check_disk_path("Disk.readText", "/any/path").is_ok());
        assert!(config.check_env_key("Env.get", "ANY_KEY").is_ok());
    }

    #[test]
    fn test_empty_hosts_allows_all() {
        let toml = r#"
[effects.Http]
hosts = []
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://anything.com")
                .is_ok()
        );
    }

    #[test]
    fn test_malformed_toml() {
        let result = ProjectConfig::parse("invalid = [");
        assert!(result.is_err());
    }

    #[test]
    fn test_non_string_hosts_are_rejected() {
        let toml = r#"
[effects.Http]
hosts = [42, "api.example.com"]
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a string"));
    }

    #[test]
    fn test_non_string_paths_are_rejected() {
        let toml = r#"
[effects.Disk]
paths = [true]
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a string"));
    }

    #[test]
    fn test_non_string_keys_are_rejected() {
        let toml = r#"
[effects.Env]
keys = [1]
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must be a string"));
    }

    #[test]
    fn test_check_env_key_allowed_exact() {
        let toml = r#"
[effects.Env]
keys = ["SECRET_TOKEN"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.check_env_key("Env.get", "SECRET_TOKEN").is_ok());
        assert!(config.check_env_key("Env.get", "SECRET_TOKEN_2").is_err());
    }

    #[test]
    fn test_check_env_key_allowed_prefix_wildcard() {
        let toml = r#"
[effects.Env]
keys = ["APP_*"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.check_env_key("Env.get", "APP_PORT").is_ok());
        assert!(config.check_env_key("Env.set", "APP_MODE").is_ok());
        assert!(config.check_env_key("Env.get", "HOME").is_err());
    }

    #[test]
    fn test_check_env_key_method_specific_overrides_namespace() {
        let toml = r#"
[effects.Env]
keys = ["APP_*"]

[effects."Env.get"]
keys = ["PUBLIC_*"]
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        // Env.get uses method-specific key list
        assert!(config.check_env_key("Env.get", "PUBLIC_KEY").is_ok());
        assert!(config.check_env_key("Env.get", "APP_KEY").is_err());
        // Env.set falls back to namespace key list
        assert!(config.check_env_key("Env.set", "APP_KEY").is_ok());
        assert!(config.check_env_key("Env.set", "PUBLIC_KEY").is_err());
    }

    #[test]
    fn host_matches_exact() {
        assert!(host_matches("api.example.com", "api.example.com"));
        assert!(!host_matches("other.com", "api.example.com"));
    }

    #[test]
    fn host_matches_wildcard() {
        assert!(host_matches("sub.example.com", "*.example.com"));
        assert!(host_matches("deep.sub.example.com", "*.example.com"));
        assert!(!host_matches("example.com", "*.example.com"));
    }

    #[test]
    fn host_degenerate_patterns_are_config_errors() {
        for pattern in ["*", "**"] {
            let error = ProjectConfig::parse(&format!("[effects.Http]\nhosts = [{pattern:?}]\n"))
                .expect_err("degenerate host pattern must be rejected");
            assert!(
                error.contains("[effects.Http].hosts[0]") && error.contains("unsupported wildcard"),
                "unexpected error for pattern {pattern:?}: {error}"
            );
        }

        let config = ProjectConfig::parse("[effects.Http]\nhosts = [\"*.example.com\"]\n").unwrap();
        assert!(
            config
                .check_http_host("Http.get", "https://sub.example.com/")
                .is_ok()
        );
        assert!(
            config
                .check_http_host("Http.get", "https://example.com/")
                .is_err()
        );
    }

    #[test]
    fn env_key_matches_exact() {
        assert!(env_key_matches("TOKEN", "TOKEN"));
        assert!(!env_key_matches("TOKEN", "TOK"));
    }

    #[test]
    fn env_key_matches_prefix_wildcard() {
        assert!(env_key_matches("APP_PORT", "APP_*"));
        assert!(env_key_matches("APP_", "APP_*"));
        assert!(!env_key_matches("PORT", "APP_*"));
    }

    #[test]
    fn env_key_degenerate_pattern_is_config_error() {
        let error = ProjectConfig::parse("[effects.Env]\nkeys = [\"**\"]\n")
            .expect_err("degenerate key pattern must be rejected");
        assert!(error.contains("[effects.Env].keys[0]"));
        assert!(error.contains("unsupported wildcard"));

        let config = ProjectConfig::parse("[effects.Env]\nkeys = [\"*\"]\n").unwrap();
        assert!(config.check_env_key("Env.get", "HOME").is_ok());
    }

    fn extract_path_matcher(source: &str) -> &str {
        let signature = "fn path_matches(normalized: &str, pattern: &str) -> bool {";
        let start = source
            .find(signature)
            .unwrap_or_else(|| panic!("missing path matcher in source"));
        let function = &source[start..];
        let mut depth = 0usize;
        for (offset, byte) in function.bytes().enumerate() {
            match byte {
                b'{' => depth += 1,
                b'}' => {
                    depth -= 1;
                    if depth == 0 {
                        return &function[..=offset];
                    }
                }
                _ => {}
            }
        }
        panic!("unterminated path matcher")
    }

    fn without_whitespace(source: &str) -> String {
        source.split_whitespace().collect()
    }

    #[test]
    fn path_matcher_copies_do_not_drift() {
        let canonical = without_whitespace(extract_path_matcher(include_str!("config.rs")));
        let copies = [
            (
                "embedded policy template",
                include_str!("codegen/rust/policy.rs"),
            ),
            (
                "runtime policy snippet",
                include_str!("codegen/rust/replay.rs"),
            ),
            (
                "self-host runtime",
                include_str!("self_host/replay_support.rs"),
            ),
        ];

        for (name, source) in copies {
            assert_eq!(
                without_whitespace(extract_path_matcher(source)),
                canonical,
                "{name} path matcher drifted from src/config.rs"
            );
        }

        let raw_self_host =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("self_hosted/out/src/replay_support.rs");
        match std::fs::read_to_string(&raw_self_host) {
            Ok(source) => assert_eq!(
                without_whitespace(extract_path_matcher(&source)),
                canonical,
                "raw self-host output path matcher drifted from src/config.rs"
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => panic!("could not read {}: {error}", raw_self_host.display()),
        }
    }

    // --- check.suppress tests ---

    #[test]
    fn test_parse_check_suppress_basic() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/**"]
reason = "Tree walkers cannot be converted to tail recursion"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.check_suppressions.len(), 1);
        assert_eq!(config.check_suppressions[0].slug, "non-tail-recursion");
        assert_eq!(config.check_suppressions[0].files, vec!["self_hosted/**"]);
        assert!(
            config.check_suppressions[0]
                .reason
                .contains("tail recursion")
        );
    }

    #[test]
    fn test_parse_check_suppress_multiple() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/**"]
reason = "Structural tree walkers"

[[check.suppress]]
slug = "missing-verify"
reason = "Global suppression for now"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.check_suppressions.len(), 2);
        assert_eq!(config.check_suppressions[1].slug, "missing-verify");
        assert!(config.check_suppressions[1].files.is_empty());
    }

    #[test]
    fn test_parse_check_suppress_missing_slug() {
        let toml = r#"
[[check.suppress]]
reason = "No slug provided"
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("slug"));
    }

    #[test]
    fn test_parse_check_suppress_missing_reason() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("reason"));
    }

    #[test]
    fn test_parse_check_suppress_empty_reason() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
reason = "   "
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("must not be empty"));
    }

    // ── [verify] budgets ─────────────────────────────────────────────

    #[test]
    fn verify_section_absent_leaves_the_compiled_defaults() {
        let config = ProjectConfig::parse("").unwrap();
        assert_eq!(config.verify_step_limit(), DEFAULT_VERIFY_STEP_LIMIT);
        assert_eq!(config.verify_max_cases(), DEFAULT_VERIFY_MAX_CASES);
        assert!(config.verify.costly.is_empty());
        assert_eq!(
            config.verify_budget_for("checkScript", "domain/scriptcases1.av"),
            (DEFAULT_VERIFY_STEP_LIMIT, None)
        );
        assert_eq!(
            config.verify_max_cases_for("checkScript", "domain/scriptcases1.av"),
            DEFAULT_VERIFY_MAX_CASES
        );
    }

    #[test]
    fn verify_costly_requires_fn() {
        let toml = r#"
[[verify.costly]]
reason = "Bitcoin Core's corpus has consensus-max scripts"
step-limit = 50000000
"#;
        let error = ProjectConfig::parse(toml).expect_err("`fn` is required");
        assert!(error.contains("requires a string `fn`"), "{error}");
    }

    #[test]
    fn verify_costly_requires_reason() {
        let toml = r#"
[[verify.costly]]
fn = "checkScript"
step-limit = 50000000
"#;
        let error = ProjectConfig::parse(toml).expect_err("`reason` is required");
        assert!(error.contains("requires a string `reason`"), "{error}");
        assert!(
            error.contains("say why this case is expected to be expensive"),
            "{error}"
        );
    }

    #[test]
    fn verify_costly_rejects_empty_reason() {
        let toml = r#"
[[verify.costly]]
fn = "checkScript"
reason = "   "
step-limit = 50000000
"#;
        let error = ProjectConfig::parse(toml).expect_err("blank `reason` is not a reason");
        assert!(error.contains("must not be empty"), "{error}");
    }

    #[test]
    fn verify_costly_dials_must_be_positive_integers() {
        for entry in [
            "step-limit = 0",
            "step-limit = \"lots\"",
            "max-cases = 0",
            "max-cases = \"lots\"",
        ] {
            let toml = format!(
                "[[verify.costly]]\nfn = \"checkScript\"\nreason = \"expensive\"\n{entry}\n"
            );
            let error = ProjectConfig::parse(&toml).expect_err("a dial must be a positive integer");
            assert!(error.contains("must be a positive integer"), "{error}");
        }
    }

    #[test]
    fn verify_costly_requires_at_least_one_dial() {
        let toml = "[[verify.costly]]\nfn = \"checkScript\"\nreason = \"expensive\"\n";
        let error = ProjectConfig::parse(toml).expect_err("an entry that raises nothing is a typo");
        assert!(
            error.contains("raises nothing — set `step-limit`, `max-cases`, or both"),
            "{error}"
        );
    }

    #[test]
    fn verify_costly_may_raise_either_dial_or_both() {
        let toml = r#"
[[verify.costly]]
fn = "checkScript"
step-limit = 50000000
reason = "slow, not wide"

[[verify.costly]]
fn = "checkTx"
max-cases = 40000
reason = "wide, not slow"

[[verify.costly]]
fn = "checkBlock"
step-limit = 50000000
max-cases = 40000
reason = "both"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        let dials: Vec<(Option<u64>, Option<usize>)> = config
            .verify
            .costly
            .iter()
            .map(|entry| (entry.step_limit, entry.max_cases))
            .collect();
        assert_eq!(
            dials,
            vec![
                (Some(50_000_000), None),
                (None, Some(40_000)),
                (Some(50_000_000), Some(40_000)),
            ]
        );
        // An entry that moves only the ceiling leaves the step budget alone.
        assert_eq!(
            config.verify_budget_for("checkTx", "domain/txcases1.av"),
            (DEFAULT_VERIFY_STEP_LIMIT, None)
        );
    }

    #[test]
    fn verify_max_cases_is_raised_for_the_named_fn_in_the_matching_files() {
        let toml = r#"
[verify]
max-cases = 20000

[[verify.costly]]
fn = "checkScript"
files = ["domain/scriptcases*.av"]
max-cases = 40000
reason = "Core corpus includes consensus-max 10,000-byte scripts"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(
            config.verify_max_cases_for("checkScript", "domain/scriptcases1.av"),
            40_000
        );
        // Right fn, wrong file.
        assert_eq!(
            config.verify_max_cases_for("checkScript", "domain/txcases1.av"),
            20_000
        );
        // Right file, wrong fn.
        assert_eq!(
            config.verify_max_cases_for("checkTx", "domain/scriptcases1.av"),
            20_000
        );
    }

    #[test]
    fn verify_max_cases_takes_the_first_matching_entry_that_sets_it() {
        let toml = r#"
[[verify.costly]]
fn = "checkScript"
step-limit = 50000000
reason = "the slow ones — this entry says nothing about width"

[[verify.costly]]
fn = "checkScript"
max-cases = 40000
reason = "and the wide ones"

[[verify.costly]]
fn = "checkScript"
max-cases = 90000
reason = "never reached — an earlier entry already answered"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(
            config.verify_max_cases_for("checkScript", "domain/scriptcases1.av"),
            40_000
        );
        // The ceiling a parser carries for one file answers the same way.
        let ceiling = config.verify_case_ceiling("domain/scriptcases1.av");
        assert_eq!(ceiling.for_fn("checkScript"), 40_000);
        assert_eq!(ceiling.for_fn("checkTx"), DEFAULT_VERIFY_MAX_CASES);
    }

    #[test]
    fn verify_budget_is_raised_for_the_named_fn_in_the_matching_files() {
        let toml = r#"
[[verify.costly]]
fn = "checkScript"
files = ["domain/scriptcases*.av"]
step-limit = 50000000
reason = "Bitcoin Core corpus includes consensus-max 10,000-byte scripts"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(
            config.verify_budget_for("checkScript", "domain/scriptcases1.av"),
            (50_000_000, Some(0))
        );
        // Right fn, wrong file.
        assert_eq!(
            config.verify_budget_for("checkScript", "domain/txcases1.av"),
            (DEFAULT_VERIFY_STEP_LIMIT, None)
        );
        // Right file, wrong fn.
        assert_eq!(
            config.verify_budget_for("checkTx", "domain/scriptcases1.av"),
            (DEFAULT_VERIFY_STEP_LIMIT, None)
        );
    }

    #[test]
    fn verify_budget_takes_the_most_permissive_matching_entry() {
        let toml = r#"
[[verify.costly]]
fn = "checkScript"
step-limit = 5000000
reason = "the ordinary big ones"

[[verify.costly]]
fn = "checkScript"
files = ["domain/scriptcases5.av"]
step-limit = 50000000
reason = "and the consensus-max ones in the last shard"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(
            config.verify_budget_for("checkScript", "domain/scriptcases5.av"),
            (50_000_000, Some(1))
        );
        assert_eq!(
            config.verify_budget_for("checkScript", "domain/scriptcases1.av"),
            (5_000_000, Some(0))
        );
    }

    #[test]
    fn verify_step_limit_and_max_cases_are_overridable() {
        let config =
            ProjectConfig::parse("[verify]\nstep-limit = 2000000\nmax-cases = 40000\n").unwrap();
        assert_eq!(config.verify_step_limit(), 2_000_000);
        assert_eq!(config.verify_max_cases(), 40_000);
    }

    #[test]
    fn verify_scalars_must_be_positive_integers() {
        for toml in [
            "[verify]\nstep-limit = 0\n",
            "[verify]\nstep-limit = \"lots\"\n",
            "[verify]\nmax-cases = 0\n",
        ] {
            let error = ProjectConfig::parse(toml).expect_err("a zero budget declines everything");
            assert!(error.contains("must be a positive integer"), "{error}");
        }
    }

    #[test]
    fn verify_costly_below_the_default_raises_nothing() {
        let toml = r#"
[verify]
step-limit = 5000000

[[verify.costly]]
fn = "checkScript"
step-limit = 2000000
reason = "written before the project default was raised past it"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(
            config.verify_budget_for("checkScript", "domain/scriptcases1.av"),
            (5_000_000, None)
        );
    }

    #[test]
    fn test_is_check_suppressed_glob() {
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/**"]
reason = "Tree walkers"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.is_check_suppressed("non-tail-recursion", "self_hosted/eval.av"));
        assert!(config.is_check_suppressed("non-tail-recursion", "self_hosted/sub/deep.av"));
        assert!(!config.is_check_suppressed("non-tail-recursion", "examples/hello.av"));
        assert!(!config.is_check_suppressed("missing-verify", "self_hosted/eval.av"));
    }

    #[test]
    fn test_is_check_suppressed_global() {
        let toml = r#"
[[check.suppress]]
slug = "missing-verify"
reason = "Not yet ready for verify"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.is_check_suppressed("missing-verify", "any/file.av"));
        assert!(config.is_check_suppressed("missing-verify", "other.av"));
        assert!(!config.is_check_suppressed("non-tail-recursion", "any/file.av"));
    }

    #[test]
    fn test_glob_matches_double_star() {
        assert!(glob_matches("self_hosted/eval.av", "self_hosted/**"));
        assert!(glob_matches("self_hosted/sub/deep.av", "self_hosted/**"));
        assert!(!glob_matches("examples/hello.av", "self_hosted/**"));
    }

    #[test]
    fn test_glob_matches_single_star() {
        assert!(glob_matches("self_hosted/eval.av", "self_hosted/*.av"));
        assert!(!glob_matches("self_hosted/sub/eval.av", "self_hosted/*.av"));
    }

    #[test]
    fn test_glob_matches_exact() {
        assert!(glob_matches("self_hosted/eval.av", "self_hosted/eval.av"));
        assert!(!glob_matches("self_hosted/other.av", "self_hosted/eval.av"));
    }

    #[test]
    fn test_glob_matches_leading_dot_slash_on_path() {
        assert!(glob_matches("./self_hosted/eval.av", "self_hosted/eval.av"));
        assert!(glob_matches("./self_hosted/eval.av", "self_hosted/**"));
        assert!(!glob_matches("./examples/hello.av", "self_hosted/**"));
    }

    #[test]
    fn test_glob_matches_leading_dot_slash_on_pattern() {
        assert!(glob_matches("self_hosted/eval.av", "./self_hosted/eval.av"));
        assert!(glob_matches("self_hosted/sub/deep.av", "./self_hosted/**"));
    }

    #[test]
    fn test_glob_matches_leading_dot_slash_on_both() {
        assert!(glob_matches(
            "./self_hosted/eval.av",
            "./self_hosted/eval.av"
        ));
        assert!(glob_matches("././a/b.av", "./a/b.av"));
    }

    #[test]
    fn test_check_suppression_applies_identifies_each_rule() {
        let toml = r#"
[[check.suppress]]
slug = "missing-verify"
files = ["domain/**"]
reason = "First rule"

[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/**"]
reason = "Second rule"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.check_suppression_applies(1, "non-tail-recursion", "self_hosted/eval.av"));
        assert!(!config.check_suppression_applies(0, "non-tail-recursion", "self_hosted/eval.av"));
        assert!(config.check_suppression_applies(0, "missing-verify", "domain/version.av"));
        assert!(!config.check_suppression_applies(1, "missing-verify", "domain/version.av"));
        // Out-of-range index is never a match.
        assert!(!config.check_suppression_applies(2, "missing-verify", "domain/version.av"));
    }

    #[test]
    fn test_overlapping_rules_both_apply() {
        // Two rules can waive the same diagnostic. Waiver-hygiene reporting
        // must credit both, or the narrower one looks dead.
        let toml = r#"
[[check.suppress]]
slug = "non-tail-recursion"
files = ["**"]
reason = "Broad rule"

[[check.suppress]]
slug = "non-tail-recursion"
files = ["self_hosted/eval.av"]
reason = "Narrow rule"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.check_suppression_applies(0, "non-tail-recursion", "self_hosted/eval.av"));
        assert!(config.check_suppression_applies(1, "non-tail-recursion", "self_hosted/eval.av"));
    }

    #[test]
    fn test_suppression_covers_file_ignores_slug() {
        let toml = r#"
[[check.suppress]]
slug = "a-slug-that-never-fires"
files = ["domain/**"]
reason = "Scoped rule"

[[check.suppress]]
slug = "missing-verify"
reason = "Global rule"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert!(config.suppression_covers_file(0, "domain/version.av"));
        assert!(config.suppression_covers_file(0, "./domain/version.av"));
        assert!(!config.suppression_covers_file(0, "other/version.av"));
        // No globs = covers every file.
        assert!(config.suppression_covers_file(1, "anything/at/all.av"));
        assert!(!config.suppression_covers_file(2, "domain/version.av"));
    }

    #[test]
    fn test_no_check_section_is_ok() {
        let config = ProjectConfig::parse("").unwrap();
        assert!(config.check_suppressions.is_empty());
        assert!(!config.is_check_suppressed("non-tail-recursion", "any.av"));
    }

    #[test]
    fn test_independence_mode_default() {
        let config = ProjectConfig::parse("").unwrap();
        assert_eq!(config.independence_mode, IndependenceMode::Complete);
    }

    #[test]
    fn test_independence_mode_complete() {
        let toml = r#"
[independence]
mode = "complete"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.independence_mode, IndependenceMode::Complete);
    }

    #[test]
    fn test_independence_mode_cancel() {
        let toml = r#"
[independence]
mode = "cancel"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.independence_mode, IndependenceMode::Cancel);
    }

    // --- shape config tests ---

    #[test]
    fn test_parse_shape_layer_overrides() {
        let toml = r#"
[[shape.layer]]
name = "Domain"
match = 40
recursion = 25
pipeline = 0
orchestration = 5
helpers = 30

[[shape.layer]]
name = "Parse"
match = 15
recursion = 10
pipeline = 65
orchestration = 10
helpers = 0
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.shape_layers.len(), 2);
        assert_eq!(config.shape_layers[0].name, "Domain");
        assert_eq!(config.shape_layers[0].pipeline_pct, 0.0);
        assert_eq!(config.shape_layers[1].name, "Parse");
        assert_eq!(config.shape_layers[1].pipeline_pct, 65.0);
    }

    #[test]
    fn test_parse_shape_expected() {
        let toml = r#"
[[shape.expected]]
glob = "src/parse/**"
layer = "Parse"

[[shape.expected]]
glob = "src/domain/**"
layer = "Domain"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(config.shape_expected.len(), 2);
        assert_eq!(
            config.shape_expected_for("src/parse/lexer.av"),
            Some("Parse"),
        );
        assert_eq!(
            config.shape_expected_for("src/domain/order.av"),
            Some("Domain"),
        );
        assert_eq!(config.shape_expected_for("src/unrelated/x.av"), None);
    }

    #[test]
    fn test_parse_shape_expected_specific_wins() {
        // Longer glob beats shorter one when both match — lets nested
        // folders carve out exceptions.
        let toml = r#"
[[shape.expected]]
glob = "src/**"
layer = "Domain"

[[shape.expected]]
glob = "src/parse/**"
layer = "Parse"
"#;
        let config = ProjectConfig::parse(toml).unwrap();
        assert_eq!(
            config.shape_expected_for("src/parse/lexer.av"),
            Some("Parse")
        );
        assert_eq!(config.shape_expected_for("src/order.av"), Some("Domain"));
    }

    #[test]
    fn test_parse_shape_layer_missing_field_errors() {
        let toml = r#"
[[shape.layer]]
name = "Domain"
match = 40
recursion = 25
# pipeline missing
orchestration = 5
helpers = 30
"#;
        let result = ProjectConfig::parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("pipeline"));
    }

    #[test]
    fn test_no_shape_section_is_ok() {
        let config = ProjectConfig::parse("").unwrap();
        assert!(config.shape_layers.is_empty());
        assert!(config.shape_expected.is_empty());
        assert_eq!(config.shape_expected_for("any/file.av"), None);
    }

    #[test]
    fn test_independence_mode_invalid() {
        let toml = r#"
[independence]
mode = "yolo"
"#;
        assert!(ProjectConfig::parse(toml).is_err());
    }
}
