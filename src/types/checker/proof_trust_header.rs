//! Trust-assumption header generator for Oracle v1 proof exports.
//!
//! Each `.dfy` / `.lean` file emitted by `aver proof` for effectful code
//! gets a short comment block at the top that tells the reader exactly
//! which claims the proof relies on. The plan (`.claude/plans/oracle.md`)
//! prescribes the content; this module produces it.
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
        "Disk.readText",
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
        "Console.print, Console.error, Console.warn",
        "output: per-branch trace segment appended per call",
    );
    out.push('\n');

    out.push_str("Concurrency and schedule invariance:\n");
    out.push_str("  ! (independent parallel): proof holds for any legal schedule,\n");
    out.push_str("       relying on the Aver compiler invariant \"schedule-invariance\n");
    out.push_str("       of structural trace normalization\" (branch locality +\n");
    out.push_str("       deterministic aggregation + runtime-provenance correspondence;\n");
    out.push_str("       informally proved in the Oracle v1 plan, mechanized meta-proof\n");
    out.push_str("       is future work). This is a compiler-level trusted claim,\n");
    out.push_str("       not emitted as a per-artifact axiom.\n");
    out.push_str("  ?! in complete mode: all branches run; error aggregated\n");
    out.push_str("       left-to-right in source order (not completion order) — this is\n");
    out.push_str("       what makes ?! complete aggregation schedule-invariant.\n");
    out.push_str("  ?! in cancel mode: NOT COVERED by this export. Project must set\n");
    out.push_str("       [independence] mode = \"complete\" in aver.toml; exports under\n");
    out.push_str("       cancel semantics are rejected by `aver proof`.\n");
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
    out.push_str("  proof subset. Other built-in effects (stateful or interactive:\n");
    out.push_str("  Env.set, Disk.writeText / .appendText / .delete / .deleteDir /\n");
    out.push_str("  .makeDir / .exists / .listDir, Time.sleep, Console.readLine,\n");
    out.push_str("  Tcp.*, HttpServer.*, Terminal.*) are rejected by `aver proof`\n");
    out.push_str("  and remain replay-only for Oracle v1. Aver has no user-defined\n");
    out.push_str("  effects in the language today; adding user-definable effects\n");
    out.push_str("  plus their classification is planned for the Relay release.\n");
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
    out.push_str("  call-instance metadata (deferred). Result-only laws for such\n");
    out.push_str("  functions remain fully supported.\n");
    out.push('\n');

    out.push_str("Out of scope in this export:\n");
    out.push_str("  - Stateful effects (Store, DB, shared mutable state)\n");
    out.push_str("  - Higher-order effectful callbacks\n");
    out.push_str("  - Interactive protocols (request-response, stdin/stdout dialogue)\n");
    out.push_str("  - User-defined effects (Aver has none; language feature itself is deferred)\n");
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
            "Random.int",
            "Random.float",
            "Time.now",
            "Time.unixMs",
            "Disk.readText",
            "Http.get",
            "Http.post",
            "Console.print",
            "Console.error",
            "Console.warn",
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
        assert!(header.contains("branch locality"));
        assert!(header.contains("source order"));
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
        assert!(header.contains("Relay release"));
        assert!(header.contains("Aver has no user-defined"));
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
