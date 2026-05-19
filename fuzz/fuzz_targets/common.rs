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

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

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
/// negligible vs the fuzz throughput (10k execs takes ~1 s in
/// persistent mode, so once per second of fuzzing is the budget).
const FLUSH_INTERVAL: u64 = 10_000;

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
/// own `Counters` via `Counters::new("fuzz_<target>")` and reaches
/// for it through `counters()`.
pub fn counters() -> &'static Counters {
    static COUNTERS: OnceLock<Counters> = OnceLock::new();
    COUNTERS.get_or_init(|| {
        let target = std::env::var("AVER_FUZZ_TARGET_NAME")
            .ok()
            .map(|s| Box::leak(s.into_boxed_str()) as &'static str)
            .unwrap_or("fuzz_unknown");
        Counters::new(target)
    })
}
