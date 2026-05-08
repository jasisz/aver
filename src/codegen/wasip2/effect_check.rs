//! Static effect-set check for `--target wasip2`.
//!
//! Phase 1.6 of 0.18 "Span" — see `docs/wasip2.md` ("Why Terminal.* /
//! Env.set / Time.sleep are rejected, not stubbed"). The axis is
//! **static target capability** vs **dynamic host capability**:
//! `Result.Err` stubs are reserved for the latter (missing preopen,
//! denied permission, …). This pass catches the former at compile
//! time, so users see one consistent `target-effect-unsupported`
//! error class instead of a cryptic `ComponentEncoder failed: core
//! imports unknown name` later.
//!
//! Three categories of reject, all surfaced through the same slug:
//!
//! 1. **Permanent** — WASI 0.2 fundamentally cannot satisfy the
//!    effect (`Terminal.*` raw mode, `Env.set` against a read-only
//!    environment). These will not land on `--target wasip2`
//!    regardless of phase.
//! 2. **Out of 0.18 release scope** — direct WIT lowering is
//!    feasible but deliberately deferred (`Http.*`, `Tcp.*`,
//!    `HttpServer.*`). Lands in 0.19+ via Phase 2 / 3 work.
//! 3. **Pending phase** — Aver effects whose lowering is planned
//!    inside 0.18 but not yet shipped (`Console.*`, `Args.get`,
//!    `Env.get`, `Time.now`/`unixMs`, `Random.*`, `Disk.*`). Each
//!    entry here disappears as the corresponding phase commit
//!    lands.

use crate::ast::TopLevel;

/// One unsupported-effect site, ready to be turned into an Aver
/// diagnostic by the caller. The CLI command renders these as
/// `error[target-effect-unsupported]:` with the source line and
/// the `reason`'s actionable hint.
pub struct UnsupportedEffect {
    pub effect: String,
    pub fn_name: String,
    pub line: usize,
    pub reason: UnsupportedReason,
}

#[derive(Debug)]
pub enum UnsupportedReason {
    /// WASI 0.2 cannot satisfy this effect by design.
    Permanent { hint: &'static str },
    /// Out of 0.18 release scope; lands in 0.19+.
    OutOfRelease { phase: &'static str },
    /// Inside 0.18 scope but not yet wired by the current phase.
    PendingPhase { phase: &'static str },
}

/// Walk every fn def in the entry items + flattened deps, collect
/// any `! [...]` entry that the wasip2 target cannot lower today.
/// Returns `Ok(())` when every declared effect is supported.
pub fn check_supported_effects(items: &[TopLevel]) -> Result<(), Vec<UnsupportedEffect>> {
    let mut errors = Vec::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            for effect in &fd.effects {
                if let Some(reason) = classify(&effect.node) {
                    errors.push(UnsupportedEffect {
                        effect: effect.node.clone(),
                        fn_name: fd.name.clone(),
                        line: effect.line,
                        reason,
                    });
                }
            }
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Render a list of unsupported-effect errors as a single multi-
/// line diagnostic body. The CLI command prefixes the first line
/// with `error[target-effect-unsupported]:` and prints the result
/// in red. Format mirrors other Aver diagnostic bodies (one entry
/// per line, indented hint, references to docs/wasip2.md).
pub fn render_errors(errors: &[UnsupportedEffect]) -> String {
    let mut out = String::new();
    for (i, e) in errors.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        match &e.reason {
            UnsupportedReason::Permanent { hint } => {
                out.push_str(&format!(
                    "  ! [{}] in fn `{}` (line {}): {hint}.\n  \
                     This effect cannot ever be supported on `--target wasip2`. \
                     Use `--target wasm-gc` or `aver run` (VM) for programs that \
                     need this capability.",
                    e.effect, e.fn_name, e.line
                ));
            }
            UnsupportedReason::OutOfRelease { phase } => {
                out.push_str(&format!(
                    "  ! [{}] in fn `{}` (line {}): out of 0.18 scope ({phase}).\n  \
                     Direct WIT lowering for this effect is planned but not in \
                     this release. Use `--target wasm-gc` for the time being, \
                     or wait for {phase}.",
                    e.effect, e.fn_name, e.line
                ));
            }
            UnsupportedReason::PendingPhase { phase } => {
                out.push_str(&format!(
                    "  ! [{}] in fn `{}` (line {}): not wired yet — pending {phase}.\n  \
                     The wasip2 effect map for this call lands in {phase}; until \
                     then, only zero-effect programs compile on `--target wasip2`. \
                     Use `--target wasm-gc` or `aver run` (VM) in the meantime.",
                    e.effect, e.fn_name, e.line
                ));
            }
        }
    }
    out
}

fn classify(effect: &str) -> Option<UnsupportedReason> {
    // ---------- Permanent rejects ----------
    if effect.starts_with("Terminal.") {
        return Some(UnsupportedReason::Permanent {
            hint: "WASI 0.2 has no raw/cooked-mode terminal operations \
                   (set-raw-mode, set-echo, get-window-size); the capability \
                   is structurally absent",
        });
    }
    if effect == "Env.set" {
        return Some(UnsupportedReason::Permanent {
            hint: "WASI 0.2 environment is read-only by design; no host \
                   implementation can ever satisfy a write",
        });
    }
    // Time.sleep graduated in Phase 1.4c — the wasip2 codegen now
    // wraps `subscribe-duration` + `poll` + `[resource-drop]pollable`
    // inside `__rt_time_sleep`. The pollable model is hidden in the
    // helper; source-level Aver still sees `Time.sleep(ms) -> Unit`.
    // ---------- Out of 0.18 release scope ----------
    if effect.starts_with("Http.") {
        return Some(UnsupportedReason::OutOfRelease {
            phase: "Phase 2 / 0.19",
        });
    }
    if effect.starts_with("Tcp.") {
        return Some(UnsupportedReason::OutOfRelease {
            phase: "Phase 2 / 0.19",
        });
    }
    if effect.starts_with("HttpServer.") {
        return Some(UnsupportedReason::OutOfRelease {
            phase: "Phase 3 / 0.19+",
        });
    }
    // ---------- Pending phase rejects (0.18 in-flight) ----------
    // Each entry vanishes as the phase commit lands.
    // (`Console.print`/`error`/`warn` graduated in Phase 1.2b1.5;
    // `Console.readLine` graduated in Phase 1.3.4 — the wasip2
    // codegen now lowers it via cached stdin handle + blocking-
    // read loop in `__rt_console_read_line`.)
    // Args.get graduated in Phase 1.3.2 (cabi_realloc + shared
    // list<string> decoder); Env.get graduated in Phase 1.3.3
    // (canonical-ABI list<tuple<string,string>> + linear-search
    // lookup helper).
    // Time.unixMs and Random.{int,float} graduated in Phase 1.4;
    // Time.now graduated in Phase 1.4b (guest-side civil_from_days
    // + RFC3339-like digit emission on top of the wall-clock retptr
    // already wired by Time.unixMs).
    // Disk.exists graduated in Phase 1.5.1; Disk.readText in
    // 1.5.2; Disk.writeText in 1.5.3; Disk.delete / deleteDir /
    // makeDir in 1.5.4 (single-call wasi ops sharing
    // `emit_disk_simple_path_op`). Remaining (`appendText` /
    // `listDir`) still pending.
    if matches!(
        effect,
        "Disk.exists"
            | "Disk.readText"
            | "Disk.writeText"
            | "Disk.appendText"
            | "Disk.delete"
            | "Disk.deleteDir"
            | "Disk.makeDir"
    ) {
        return None;
    }
    if effect.starts_with("Disk.") {
        return Some(UnsupportedReason::PendingPhase {
            phase: "Phase 1.5",
        });
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_permanent_rejects() {
        assert!(matches!(
            classify("Terminal.readKey"),
            Some(UnsupportedReason::Permanent { .. })
        ));
        assert!(matches!(
            classify("Terminal.setColor"),
            Some(UnsupportedReason::Permanent { .. })
        ));
        assert!(matches!(
            classify("Env.set"),
            Some(UnsupportedReason::Permanent { .. })
        ));
        // Time.sleep moved out of `Permanent` in Phase 1.4c — it now
        // lowers via subscribe-duration + poll + drop-pollable. The
        // earlier reject was a 0.18 scope choice, not a fundamental
        // limitation.
        assert!(classify("Time.sleep").is_none());
    }

    #[test]
    fn classifies_out_of_release_rejects() {
        assert!(matches!(
            classify("Http.get"),
            Some(UnsupportedReason::OutOfRelease { .. })
        ));
        assert!(matches!(
            classify("Tcp.connect"),
            Some(UnsupportedReason::OutOfRelease { .. })
        ));
        assert!(matches!(
            classify("HttpServer.listen"),
            Some(UnsupportedReason::OutOfRelease { .. })
        ));
    }

    #[test]
    fn classifies_pending_phase_rejects() {
        // Console.print/error/warn graduated in 1.2b1.5; Time.unixMs
        // and Random.{int,float} graduated in Phase 1.4; Time.now
        // graduated in Phase 1.4b; Time.sleep graduated in Phase
        // 1.4c (pollable model wrapped inside `__rt_time_sleep`) —
        // the wasip2 codegen now lowers them to wasi:cli/io/streams
        // / wasi:clocks / wasi:random / wasi:io/poll natively.
        assert!(classify("Console.print").is_none());
        assert!(classify("Console.error").is_none());
        assert!(classify("Console.warn").is_none());
        assert!(classify("Time.unixMs").is_none());
        assert!(classify("Time.now").is_none());
        assert!(classify("Time.sleep").is_none());
        assert!(classify("Random.int").is_none());
        assert!(classify("Random.float").is_none());
        // Args.get graduated in Phase 1.3.2 (cabi_realloc + shared
        // list<string> decoder helper); Env.get graduated in
        // Phase 1.3.3 (canonical-ABI list<tuple> + lookup helper);
        // Console.readLine graduated in Phase 1.3.4 (stdin handle
        // cache + blocking-read loop + Result construction).
        assert!(classify("Args.get").is_none());
        assert!(classify("Env.get").is_none());
        assert!(classify("Console.readLine").is_none());
        // Disk.exists graduated in 1.5.1; readText in 1.5.2;
        // writeText in 1.5.3; delete / deleteDir / makeDir in
        // 1.5.4; appendText in 1.5.5. Only listDir remains on
        // the PendingPhase path.
        assert!(classify("Disk.exists").is_none());
        assert!(classify("Disk.readText").is_none());
        assert!(classify("Disk.writeText").is_none());
        assert!(classify("Disk.appendText").is_none());
        assert!(classify("Disk.delete").is_none());
        assert!(classify("Disk.deleteDir").is_none());
        assert!(classify("Disk.makeDir").is_none());
        assert!(matches!(
            classify("Disk.listDir"),
            Some(UnsupportedReason::PendingPhase { .. })
        ));
    }

    #[test]
    fn unknown_effects_are_passthrough() {
        // Not every namespace is wasip2's concern. Unknown effects
        // pass — the type checker / module-effect surface catches
        // genuinely undeclared ones earlier.
        assert!(classify("MyDomain.doThing").is_none());
        assert!(classify("Custom.action").is_none());
    }
}
