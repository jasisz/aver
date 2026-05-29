// Iron 0.21 "Hardcore Fuzz" — Phase 0: shared metrics module for
// fuzz targets. AFL gives us `execs_done`, `corpus_count`, and
// `bitmap_cvg` via its own `fuzzer_stats` file, but those tell us
// nothing about *where in the pipeline* each input ended up.
//
// For the upcoming `aver-fuzz-mutator` work we need to know:
//
//   - how many inputs actually parse as Aver source
//   - how many additionally type-check
//   - what AST shape (node count, max depth) the accepted inputs have
//
// Without these baselines we can't tell whether Phase 1 (custom
// mutator) shifts inputs deeper into the pipeline — coverage alone
// is ambiguous because a random-byte fuzzer that finds 50% bitmap
// coverage on the lexer rejection paths looks superficially similar
// to a structured fuzzer that finds 50% coverage on the typecheck
// happy paths.
//
// All counters are `AtomicU64` so the persistent-mode AFL harness
// can update them from a single thread without lock contention.
// `flush_to_disk` writes a snapshot to `/tmp/aver_fuzz_metrics_<target>.txt`
// every `FLUSH_INTERVAL` executions; CI reads the file at the end
// of the fuzz step and uploads it alongside the queue artifact.

// Each fuzz binary picks the subset of this module it needs —
// `parse_bytes` ignores `record_typecheck_clean`, `replay_codec`
// ignores the AST walkers. Suppress the per-binary dead-code
// warnings rather than scatter `#[allow]` on every helper.
#![allow(dead_code)]

use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

pub struct Counters {
    pub target: &'static str,
    /// Total `afl::fuzz!` callback invocations. Mirrors AFL's own
    /// `execs_done` but tracked here so we don't have to scrape
    /// AFL's stats file.
    pub execs: AtomicU64,
    /// Inputs that survived `Lexer::tokenize`. Indicates the
    /// fraction of fuzz attempts that produced a valid token
    /// stream — the floor below which a structure-aware mutator
    /// cannot be useful.
    pub lex_ok: AtomicU64,
    /// Inputs whose token stream also parsed as `Vec<TopLevel>`.
    /// The headline metric for "is this input even Aver?".
    pub parse_ok: AtomicU64,
    /// Inputs that typechecked without producing errors. Only
    /// updated by targets that actually run the typechecker; stays
    /// 0 for `parse_bytes` / `replay_codec`.
    pub typecheck_clean: AtomicU64,
    /// Running sum of AST node counts for parsed inputs. Divide
    /// by `parse_ok` for the average; combined with
    /// `max_ast_depth` gives a sense of "how chunky are the
    /// programs the fuzzer is exploring".
    pub ast_node_sum: AtomicU64,
    /// High-water mark for AST depth across the campaign.
    pub max_ast_depth: AtomicU64,
}

impl Counters {
    pub const fn new(target: &'static str) -> Self {
        Self {
            target,
            execs: AtomicU64::new(0),
            lex_ok: AtomicU64::new(0),
            parse_ok: AtomicU64::new(0),
            typecheck_clean: AtomicU64::new(0),
            ast_node_sum: AtomicU64::new(0),
            max_ast_depth: AtomicU64::new(0),
        }
    }

    pub fn record_exec(&self) {
        let n = self.execs.fetch_add(1, Ordering::Relaxed) + 1;
        if n.is_multiple_of(FLUSH_INTERVAL) {
            self.flush_to_disk();
        }
    }

    /// Force a snapshot write regardless of flush interval. Call
    /// from the fuzz target's `main` after `afl::fuzz!` returns so
    /// the final state survives even when AFL terminates the loop
    /// mid-budget at the `-V` deadline.
    pub fn flush(&self) {
        self.flush_to_disk();
    }

    pub fn record_lex_ok(&self) {
        self.lex_ok.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_parse_ok(&self, ast_nodes: u64, depth: u64) {
        self.parse_ok.fetch_add(1, Ordering::Relaxed);
        self.ast_node_sum.fetch_add(ast_nodes, Ordering::Relaxed);
        self.max_ast_depth.fetch_max(depth, Ordering::Relaxed);
    }

    pub fn record_typecheck_clean(&self) {
        self.typecheck_clean.fetch_add(1, Ordering::Relaxed);
    }

    fn flush_to_disk(&self) {
        let path = format!("/tmp/aver_fuzz_metrics_{}.txt", self.target);
        // Snapshot the counters into a deterministic table. Ignore
        // I/O errors — metrics are observational, not load-bearing,
        // and a fuzz worker that can't write to `/tmp` has bigger
        // problems than missing telemetry.
        let snapshot = format!(
            "target={target}\nexecs={execs}\nlex_ok={lex_ok}\nparse_ok={parse_ok}\ntypecheck_clean={typecheck_clean}\nast_node_sum={ast_node_sum}\nmax_ast_depth={max_ast_depth}\n",
            target = self.target,
            execs = self.execs.load(Ordering::Relaxed),
            lex_ok = self.lex_ok.load(Ordering::Relaxed),
            parse_ok = self.parse_ok.load(Ordering::Relaxed),
            typecheck_clean = self.typecheck_clean.load(Ordering::Relaxed),
            ast_node_sum = self.ast_node_sum.load(Ordering::Relaxed),
            max_ast_depth = self.max_ast_depth.load(Ordering::Relaxed),
        );
        let _ = std::fs::write(&path, snapshot);
    }
}

/// How often to snapshot to disk. Tuned so the I/O cost is
/// negligible vs the fuzz throughput (1k execs takes ~100ms in
/// persistent mode, so ten flushes per second of fuzzing is the
/// budget — cheap, and short PR-smoke runs (180 s) reach the
/// first flush in well under a second even on the slow
/// typecheck target).
const FLUSH_INTERVAL: u64 = 1_000;

/// Count AST nodes by walking every `TopLevel` and its expression
/// subtree iteratively. We care about the order of magnitude, not
/// exact bookkeeping — Box/Spanned wrappers don't count.
pub fn ast_metrics(items: &[aver::ast::TopLevel]) -> (u64, u64) {
    let mut total: u64 = items.len() as u64;
    let mut max_depth: u64 = 1;
    for item in items {
        if let aver::ast::TopLevel::FnDef(f) = item {
            for stmt in f.body.stmts() {
                let expr = match stmt {
                    aver::ast::Stmt::Expr(e) => e,
                    aver::ast::Stmt::Binding(_, _, e) => e,
                };
                let (n, d) = expr_metrics(expr, 1);
                total = total.saturating_add(n);
                if d > max_depth {
                    max_depth = d;
                }
            }
        }
    }
    (total, max_depth)
}

/// Iterative-with-stack expression walker. Avoids self-recursion
/// for the same reason `dotted_name` got rewritten this morning —
/// a fuzz harness should never reproduce the bug it's hunting.
fn expr_metrics(root: &aver::ast::Spanned<aver::ast::Expr>, base_depth: u64) -> (u64, u64) {
    use aver::ast::Expr;
    let mut total: u64 = 0;
    let mut max_depth: u64 = base_depth;
    let mut stack: Vec<(&aver::ast::Spanned<Expr>, u64)> = vec![(root, base_depth)];
    while let Some((node, d)) = stack.pop() {
        total = total.saturating_add(1);
        if d > max_depth {
            max_depth = d;
        }
        // Cap total walk to avoid runaway cost on adversarial AST
        // shapes — the fuzz harness must stay fast.
        if total > 100_000 {
            break;
        }
        match &node.node {
            Expr::BinOp(_, a, b) => {
                stack.push((a, d + 1));
                stack.push((b, d + 1));
            }
            Expr::Neg(inner) => stack.push((inner, d + 1)),
            Expr::Attr(inner, _) => stack.push((inner, d + 1)),
            Expr::FnCall(callee, args) => {
                stack.push((callee, d + 1));
                for a in args {
                    stack.push((a, d + 1));
                }
            }
            Expr::Constructor(_, payload) => {
                if let Some(arg) = payload {
                    stack.push((arg, d + 1));
                }
            }
            Expr::Match { subject, arms } => {
                stack.push((subject, d + 1));
                for arm in arms {
                    stack.push((&arm.body, d + 1));
                }
            }
            Expr::List(items) | Expr::Tuple(items) => {
                for item in items {
                    stack.push((item, d + 1));
                }
            }
            Expr::IndependentProduct(items, _) => {
                for item in items {
                    stack.push((item, d + 1));
                }
            }
            Expr::MapLiteral(entries) => {
                for (k, v) in entries {
                    stack.push((k, d + 1));
                    stack.push((v, d + 1));
                }
            }
            Expr::ErrorProp(inner) => stack.push((inner, d + 1)),
            Expr::RecordCreate { fields, .. } => {
                for (_, v) in fields {
                    stack.push((v, d + 1));
                }
            }
            Expr::RecordUpdate { base, updates, .. } => {
                stack.push((base, d + 1));
                for (_, v) in updates {
                    stack.push((v, d + 1));
                }
            }
            Expr::TailCall(boxed) => {
                for a in &boxed.args {
                    stack.push((a, d + 1));
                }
            }
            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let aver::ast::StrPart::Parsed(expr) = part {
                        stack.push((expr, d + 1));
                    }
                }
            }
            // Leaves: Ident, Literal, etc. — nothing to push.
            _ => {}
        }
    }
    (total, max_depth)
}

/// Global counters per fuzz binary. Each target initialises its
/// own `Counters` via `Counters::new(env!("CARGO_BIN_NAME"))` and
/// reaches for it through `counters()`. Using the cargo-injected
/// bin name (compile-time) instead of a runtime env var means the
/// AFL forkserver child processes don't need any env plumbing —
/// the binary's identity is baked in at link time.
pub fn counters() -> &'static Counters {
    static COUNTERS: OnceLock<Counters> = OnceLock::new();
    COUNTERS.get_or_init(|| Counters::new(env!("CARGO_BIN_NAME")))
}

// ── Multi-module fuzz harness ──────────────────────────────────────────
//
// Every target's `afl::fuzz!` callback gets the same opaque `&[u8]`
// slice. By default that slice is treated as the entry source for a
// single-file Aver program. With the multi-module dispatcher below,
// inputs that start with a 4-byte magic prefix get parsed as a small
// multi-file project — entry + 1..3 dep modules — and the target
// runs against the synthetic tmpdir instead. Inputs without the
// prefix fall through to the existing single-file path unchanged.
//
// Format (all little-endian where applicable):
//
//   <MAGIC: 4 bytes 0xA7 0xE2 0x40 0x01>
//   <n_files: u8 in 1..=4>
//   for each file:
//     <name_len: u8 in 1..=20>
//     <name: name_len bytes, ASCII letters/digits/dot only>
//     <body_len: u16 little-endian>
//     <body: body_len bytes, UTF-8 source>
//   <entry_idx: u8 in 0..n_files>
//
// Anything malformed → `None`, target falls back to single-file.
// Names are lowercased and rendered as `<name>.av` files under a
// deterministically-named directory keyed on `hash(data)`. Same input
// → same directory path → AFL's coverage map stays stable. A fresh
// random tempdir would mean every exec sees different paths inside
// `LoadedModule.path` / Aver error messages, which AFL reads as
// path-noise jitter and ends up tagging the target as
// non-deterministic — slows convergence and dilutes the crash signal.

const MULTIMODULE_MAGIC: &[u8; 4] = &[0xA7, 0xE2, 0x40, 0x01];

pub struct MultiModuleSetup {
    /// Path to the entry .av file inside the deterministic dir.
    pub entry_path: std::path::PathBuf,
    /// Module root for `--module-root` / `find_module_file` lookups
    /// — same directory as the entry, since all synthetic files
    /// land flat at the dir root.
    pub module_root: std::path::PathBuf,
    /// Raw entry source — handed to targets that only need the
    /// bytes (e.g. `parse_bytes`, `replay_codec`).
    pub entry_source: String,
}

pub fn try_multimodule_input(data: &[u8]) -> Option<MultiModuleSetup> {
    if data.len() < MULTIMODULE_MAGIC.len() + 2 {
        return None;
    }
    if &data[..MULTIMODULE_MAGIC.len()] != MULTIMODULE_MAGIC {
        return None;
    }
    let mut pos = MULTIMODULE_MAGIC.len();
    let n_files = *data.get(pos)?;
    pos += 1;
    if !(1..=4).contains(&n_files) {
        return None;
    }

    // Deterministic directory name keyed on `hash(data)` — see the
    // module-level comment above for why this matters for AFL
    // stability. We don't drop the directory at the end of the fn:
    // the next exec with this input will reuse the same dir
    // (idempotent overwrite per file), and the OS reclaims `/tmp`
    // on reboot. Persistent-mode AFL workers benefit from the
    // warm filesystem cache too.
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    data.hash(&mut hasher);
    let dir_hash = hasher.finish();
    let dir = std::env::temp_dir().join(format!("aver-fuzz-mm-{dir_hash:016x}"));
    std::fs::create_dir_all(&dir).ok()?;

    let mut file_paths: Vec<std::path::PathBuf> = Vec::with_capacity(n_files as usize);

    for _ in 0..n_files {
        let name_len = *data.get(pos)? as usize;
        pos += 1;
        if !(1..=20).contains(&name_len) || pos + name_len > data.len() {
            return None;
        }
        let name_bytes = &data[pos..pos + name_len];
        pos += name_len;
        if !name_bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'.')
        {
            return None;
        }
        if name_bytes[0] == b'.' || name_bytes[name_len - 1] == b'.' {
            return None;
        }
        let name = std::str::from_utf8(name_bytes).ok()?.to_ascii_lowercase();

        if pos + 2 > data.len() {
            return None;
        }
        let body_len = u16::from_le_bytes([data[pos], data[pos + 1]]) as usize;
        pos += 2;
        if pos + body_len > data.len() {
            return None;
        }
        let body = std::str::from_utf8(&data[pos..pos + body_len]).ok()?;
        pos += body_len;

        let file_path = dir.join(format!("{name}.av"));
        std::fs::write(&file_path, body).ok()?;
        file_paths.push(file_path);
    }

    let entry_idx = *data.get(pos)? as usize;
    if entry_idx >= file_paths.len() {
        return None;
    }
    let entry_path = file_paths[entry_idx].clone();
    let entry_source = std::fs::read_to_string(&entry_path).ok()?;

    Some(MultiModuleSetup {
        entry_path,
        module_root: dir,
        entry_source,
    })
}
