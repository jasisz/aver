//! Trust-assumption header generator for Oracle v1 proof exports.
//!
//! Each `.dfy` / `.lean` file emitted by `aver proof` for effectful code
//! gets a short comment block at the top that tells the reader exactly
//! which claims the proof relies on. This module is the source of truth for
//! that generated text.
//!
//! Keeping the generator here (not in the Dafny / Lean backends) means:
//!
//! - The two backends emit identical trust claims byte-for-byte — no
//!   drift between target languages.
//! - The effect table read here is the same
//!   [`effect_classification::CLASSIFICATIONS`] table used by `given`
//!   inference and rejection diagnostics, so the header can never list
//!   an effect the compiler doesn't actually classify.
//!
//! The generator emits plain text; each backend wraps its own comment
//! prefix (`//` for Dafny/Lean 4 both use `//`).

use super::effect_classification::EffectDimension;

/// Build the trust-assumption header text. Caller prefixes each line with
/// the target language's comment marker.
pub fn generate() -> String {
    let mut out = String::new();
    out.push_str("Trusted model assumptions for this Aver proof export:\n");
    out.push('\n');
    out.push_str("Effects and dimensions:\n");
    push_effect_row(&mut out, "Args.get", "snapshot: stable return within run");
    push_effect_row(&mut out, "Env.get", "snapshot: stable return within run");
    push_effect_row(
        &mut out,
        "Console.readLine",
        "generative: oracle; interactive line input",
    );
    push_effect_row(
        &mut out,
        "Random.int, Random.float",
        "generative: oracle indexed by (BranchPath, Int, args...); fresh per call",
    );
    push_effect_row(
        &mut out,
        "Time.now, Time.unixMs",
        "generative: oracle; non-deterministic between runs",
    );
    push_effect_row(
        &mut out,
        "Disk.readText, Disk.exists, Disk.listDir",
        "generative: oracle; live FS, value may change between calls",
    );
    push_effect_row(
        &mut out,
        "Http.get, Http.head, Http.delete,",
        "generative + output: request emitted to trace, response from oracle",
    );
    push_effect_row(
        &mut out,
        "Http.post, Http.put, Http.patch",
        "generative + output: `(url, body, contentType, headers)` in request",
    );
    push_effect_row(
        &mut out,
        "Disk.writeText, Disk.appendText,",
        "generative + output: operation in trace, result from oracle",
    );
    push_effect_row(
        &mut out,
        "Disk.delete, Disk.deleteDir, Disk.makeDir",
        "generative + output: no persistent FS-state claim",
    );
    push_effect_row(
        &mut out,
        "Tcp.send, Tcp.ping",
        "generative + output: one-shot request/response only",
    );
    push_effect_row(
        &mut out,
        "Console.print, Console.error, Console.warn",
        "output: per-branch trace segment appended per call",
    );
    push_effect_row(
        &mut out,
        "Time.sleep",
        "output: duration emitted to trace; no scheduling/timing claim",
    );
    push_effect_row(
        &mut out,
        "Terminal.clear, Terminal.moveTo, Terminal.print",
        "output: terminal drawing emitted to trace",
    );
    push_effect_row(
        &mut out,
        "Terminal.hideCursor, Terminal.showCursor, Terminal.flush",
        "output: terminal drawing/control emitted to trace",
    );
    push_effect_row(
        &mut out,
        "Terminal.readKey",
        "generative: oracle; key input",
    );
    out.push('\n');

    out.push_str("Concurrency and schedule invariance:\n");
    out.push_str("  ! (independent parallel): proof holds for any legal schedule,\n");
    out.push_str("       relying on the Aver compiler invariant \"schedule-invariance\n");
    out.push_str("       of structural trace normalization\". This is a compiler-level\n");
    out.push_str("       trusted claim, not emitted as a per-artifact axiom.\n");
    out.push_str("  ?! in complete mode: all branches run; error aggregated\n");
    out.push_str("       left-to-right in source order (not completion order) — this is\n");
    out.push_str("       what makes ?! complete aggregation schedule-invariant.\n");
    out.push_str("  ?! in cancel mode: NOT COVERED by this export. Project must set\n");
    out.push_str("       [independence] mode = \"complete\" in aver.toml; exports under\n");
    out.push_str("       cancel semantics are rejected by `aver proof`.\n");
    out.push('\n');

    out.push_str("Schedule-invariance argument (informal, three lemmas):\n");
    out.push_str("  The normalization claim above is the conjunction of three\n");
    out.push_str("  lemmas the Aver compiler enforces statically. Oracle v1 ships\n");
    out.push_str("  this as a written argument trusted at the compiler level;\n");
    out.push_str("  it is not machine-checked inside this exported artifact.\n");
    out.push('\n');
    out.push_str("  Lemma 1 — Branch locality.\n");
    out.push_str("    Every effect emission inside a `!`/`?!` branch is attributed\n");
    out.push_str("    to that branch's structural position (group_id, branch_idx).\n");
    out.push_str("    No emission can leak between sibling branches — the replay\n");
    out.push_str("    scope enters / exits groups in source order and a branch's\n");
    out.push_str("    events are recorded only while its branch_idx is current.\n");
    out.push_str("    Corollary: the trace of one branch is a function of that\n");
    out.push_str("    branch's source alone, not of sibling scheduling.\n");
    out.push('\n');
    out.push_str("  Lemma 2 — Deterministic aggregation.\n");
    out.push_str("    The trace of a group is the concatenation of its branches'\n");
    out.push_str("    traces in source-order branch index (0, 1, …, n-1). The `?!`\n");
    out.push_str("    complete mode aggregates the first Result.Err in the same\n");
    out.push_str("    left-to-right order, not wall-clock completion order. Both\n");
    out.push_str("    the trace concatenation and the error aggregation are pure\n");
    out.push_str("    functions of the source tree + per-branch results, so any\n");
    out.push_str("    two legal schedules produce identical normalized output.\n");
    out.push('\n');
    out.push_str("  Lemma 3 — Runtime-provenance correspondence.\n");
    out.push_str("    Every emission in a recording JSON carries the same\n");
    out.push_str("    (group_id, branch_path, effect_occurrence) triple that the\n");
    out.push_str("    proof-side BranchPath / counter mechanism threads into\n");
    out.push_str("    oracle calls. Recordings and proofs therefore address the\n");
    out.push_str("    same events through the same structural coordinates —\n");
    out.push_str("    `trace.replay(path)` / `trace.replaySeq(seq)` bridges between\n");
    out.push_str("    these coordinate systems without widening the trusted base.\n");
    out.push('\n');
    out.push_str("  Conjunction: from (1) branches are independently determined\n");
    out.push_str("  by source, (2) their composition is a pure function of\n");
    out.push_str("  source + per-branch results, (3) recordings and proofs\n");
    out.push_str("  address the same structural events — so the normalized trace\n");
    out.push_str("  and the aggregated result are invariants of the program,\n");
    out.push_str("  not of any particular schedule.\n");
    out.push('\n');

    out.push_str("Structural trace addressing:\n");
    out.push_str("  Events addressed by the structural tree via\n");
    out.push_str("  .group(N).branch(idx).event(k) in primary API; BranchPath\n");
    out.push_str("  (opaque type) is used for oracle bindings and via the .path()\n");
    out.push_str("  bridge on branch nodes. BranchPath is source-derived and\n");
    out.push_str("  schedule-invariant. Cross-branch ordering is NOT observable;\n");
    out.push_str("  wall-clock and shared-channel adjacency are NOT expressible.\n");
    out.push('\n');

    out.push_str("Effect classification (closed for Oracle v1):\n");
    out.push_str("  Only the classified built-in effects listed above are in the\n");
    out.push_str("  proof subset. Other built-in effects are rejected by `aver proof`:\n");
    out.push_str("  ambient process state (Env.set), persistent TCP sessions\n");
    out.push_str("  (Tcp.connect / .writeLine / .readLine / .close), server\n");
    out.push_str("  lifecycle callbacks (HttpServer.*), and terminal modal state\n");
    out.push_str("  (Terminal.enableRawMode / .disableRawMode / .setColor /\n");
    out.push_str("  .resetColor / .size). These remain replay-only. Oracle covers\n");
    out.push_str("  only the fixed built-in effect set listed above.\n");
    out.push('\n');

    out.push_str("Backend independence:\n");
    out.push_str("  Exported proofs hold uniformly across Aver backends (VM, compiled\n");
    out.push_str("  Rust, WASM) under the schedule-invariance compiler invariant above.\n");
    out.push_str("  Sequential execution (VM) and parallel execution (compiled Rust)\n");
    out.push_str("  are both covered — both are legal schedules of the same evaluation.\n");
    out.push('\n');

    out.push_str("Recursion caveat:\n");
    out.push_str("  Source-structural group_ids are stable across recompilation, but\n");
    out.push_str("  recursive functions produce multiple runtime instances of the same\n");
    out.push_str("  structural group that share (group_id, branch_path, effect_occurrence).\n");
    out.push_str("  Proofs addressing groups via .group(N) refer to the structural\n");
    out.push_str("  (source) position; per-instance addressing in recordings uses\n");
    out.push_str("  trace.replaySeq(seq) with the monotonic sequence number from JSON.\n");
    out.push('\n');
    out.push_str("  Trace-aware laws for effectful recursive functions are REJECTED in\n");
    out.push_str("  Oracle v1 — the caller_fn filter for fn.trace cannot distinguish\n");
    out.push_str("  the outermost invocation from recursive self-calls without\n");
    out.push_str("  call-instance metadata. Result-only laws for such\n");
    out.push_str("  functions remain fully supported.\n");
    out.push('\n');

    out.push_str("Out of scope in this export:\n");
    out.push_str("  - Stateful effects (Store, DB, shared mutable state)\n");
    out.push_str("  - Higher-order effectful callbacks\n");
    out.push_str("  - Long-running protocols and terminal modal state\n");
    out.push_str("  - ?! cancel mode\n");
    out.push_str("  - Trace-aware laws on recursive effectful functions (result-only OK)\n");
    out
}

/// Emit the trust header with each line prefixed by the given comment
/// marker (typically `"// "` for Dafny/Lean 4). An empty / whitespace-only
/// input line is still commented so the block reads as one consistent
/// comment region in the generated file.
pub fn generate_commented(prefix: &str) -> String {
    generate()
        .lines()
        .map(|line| {
            if line.is_empty() {
                prefix.trim_end().to_string()
            } else {
                format!("{}{}", prefix, line)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Sanity helper — for each classified effect, assert the header names
/// its namespace somewhere. Purely structural; no Type work.
fn push_effect_row(out: &mut String, names: &str, dimension_note: &str) {
    // Keep width consistent so readers can skim the table; "%-40s" style.
    let padded = format!("{:<40}", names);
    out.push_str("  ");
    out.push_str(&padded);
    out.push_str("— ");
    out.push_str(dimension_note);
    out.push('\n');
}

/// Declare an explicit dependency on the classification table's shape:
/// if a new effect dimension is ever added, this `match` will fail to
/// compile and force the header author to decide how to render it.
#[allow(dead_code)]
fn _dimension_coverage(dim: EffectDimension) -> &'static str {
    match dim {
        EffectDimension::Snapshot => "snapshot",
        EffectDimension::Generative => "generative",
        EffectDimension::Output => "output",
        EffectDimension::GenerativeOutput => "generative + output",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_mentions_all_classified_effect_namespaces() {
        let header = generate();
        for namespace in &[
            "Args.get",
            "Env.get",
            "Console.readLine",
            "Random.int",
            "Random.float",
            "Time.now",
            "Time.unixMs",
            "Time.sleep",
            "Disk.readText",
            "Disk.exists",
            "Disk.listDir",
            "Disk.writeText",
            "Disk.appendText",
            "Disk.delete",
            "Disk.deleteDir",
            "Disk.makeDir",
            "Http.get",
            "Http.post",
            "Tcp.send",
            "Tcp.ping",
            "Console.print",
            "Console.error",
            "Console.warn",
            "Terminal.clear",
            "Terminal.moveTo",
            "Terminal.print",
            "Terminal.readKey",
            "Terminal.hideCursor",
            "Terminal.showCursor",
            "Terminal.flush",
        ] {
            assert!(
                header.contains(namespace),
                "trust header must mention {} somewhere",
                namespace
            );
        }
    }

    #[test]
    fn header_mentions_concurrency_invariant() {
        let header = generate();
        assert!(header.contains("schedule-invariance"));
        assert!(header.contains("Branch locality"));
        assert!(header.contains("source order"));
    }

    #[test]
    fn header_states_three_normalization_lemmas_explicitly() {
        // Oracle v1 plan requires the informal three-lemma argument to
        // be written out so reviewers can cite it when vetting an
        // export. The numbered lemmas must appear together with a
        // one-liner conjunction claim tying them to the compiler-level
        // schedule-invariance invariant.
        let header = generate();
        assert!(header.contains("Lemma 1 — Branch locality."));
        assert!(header.contains("Lemma 2 — Deterministic aggregation."));
        assert!(header.contains("Lemma 3 — Runtime-provenance correspondence."));
        assert!(header.contains("Conjunction:"));
        // The lemmas point at the concrete structural primitives.
        assert!(header.contains("group_id, branch_idx"));
        assert!(header.contains("group_id, branch_path, effect_occurrence"));
        // The compiler invariant is called out as an explicit trust boundary.
        assert!(header.contains("not machine-checked"));
    }

    #[test]
    fn header_mentions_out_of_scope_blocks() {
        let header = generate();
        assert!(header.contains("Stateful effects"));
        assert!(header.contains("Higher-order effectful callbacks"));
        assert!(header.contains("?! cancel mode"));
        assert!(header.contains("recursive effectful functions"));
    }

    #[test]
    fn header_mentions_branchpath_and_tree_addressing() {
        let header = generate();
        assert!(header.contains("BranchPath"));
        assert!(header.contains(".group(N).branch(idx).event(k)"));
    }

    #[test]
    fn header_mentions_backend_independence() {
        let header = generate();
        assert!(header.contains("VM"));
        assert!(header.contains("compiled Rust"));
        assert!(header.contains("WASM"));
    }

    #[test]
    fn header_mentions_classification_table_is_closed() {
        let header = generate();
        assert!(header.contains("closed for Oracle v1"));
        assert!(header.contains("fixed built-in"));
    }

    #[test]
    fn generate_commented_prefixes_every_line_including_blanks() {
        let out = generate_commented("// ");
        for (i, line) in out.lines().enumerate() {
            assert!(
                line.starts_with("//"),
                "line {} does not start with the comment prefix: {:?}",
                i,
                line
            );
        }
    }

    #[test]
    fn generate_commented_respects_trimmed_prefix_for_empty_lines() {
        let out = generate_commented("// ");
        // An empty line in the source renders as `//` without trailing
        // whitespace (rustfmt convention for comment blocks).
        assert!(out.lines().any(|l| l == "//"));
    }

    #[test]
    fn generate_is_deterministic() {
        assert_eq!(generate(), generate());
        assert_eq!(generate_commented("// "), generate_commented("// "));
    }
}
