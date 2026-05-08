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
//!    environment, `Time.sleep` without the pollable model). These
//!    will not land on `--target wasip2` regardless of phase.
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
    if effect == "Time.sleep" {
        return Some(UnsupportedReason::Permanent {
            hint: "sleep without burning CPU requires \
                   `wasi:clocks/monotonic-clock.subscribe-duration` + \
                   `wasi:io/poll.poll`, which is the pollable model 0.18 \
                   keeps out of scope; busy-wait is rejected because it \
                   spins host cycles and reads as an Aver runtime bug",
        });
    }
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
    // (`Console.print`/`error`/`warn` graduated in Phase 1.2b1.5.)
    if effect == "Console.readLine" {
        return Some(UnsupportedReason::PendingPhase {
            phase: "Phase 1.3",
        });
    }
    // Args.get graduated in Phase 1.3.2 (cabi_realloc + shared
    // list<string> decoder); Env.get graduated in Phase 1.3.3
    // (canonical-ABI list<tuple<string,string>> + linear-search
    // lookup helper).
    // Time.unixMs and Random.{int,float} graduated in Phase 1.4.
    // Time.now (ISO string) still needs guest-side date formatting
    // — defer to Phase 1.4b.
    if effect == "Time.now" {
        return Some(UnsupportedReason::PendingPhase {
            phase: "Phase 1.4b",
        });
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
        assert!(matches!(
            classify("Time.sleep"),
            Some(UnsupportedReason::Permanent { .. })
        ));
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
        // and Random.{int,float} graduated in Phase 1.4 — the wasip2
        // codegen now lowers them to wasi:cli/io/streams /
        // wasi:clocks / wasi:random natively.
        assert!(classify("Console.print").is_none());
        assert!(classify("Console.error").is_none());
        assert!(classify("Console.warn").is_none());
        assert!(classify("Time.unixMs").is_none());
        assert!(classify("Random.int").is_none());
        assert!(classify("Random.float").is_none());
        // Time.now (ISO string) still pending until 1.4b adds
        // guest-side datetime formatting.
        assert!(matches!(
            classify("Time.now"),
            Some(UnsupportedReason::PendingPhase { .. })
        ));
        // Args.get graduated in Phase 1.3.2 (cabi_realloc + shared
        // list<string> decoder helper); Env.get graduated in
        // Phase 1.3.3 (canonical-ABI list<tuple> + lookup helper).
        assert!(classify("Args.get").is_none());
        assert!(classify("Env.get").is_none());
        // Console.readLine still pending — needs per-call
        // input-stream resource handle + `[resource-drop]` (Phase
        // 1.3.4).
        assert!(matches!(
            classify("Console.readLine"),
            Some(UnsupportedReason::PendingPhase { .. })
        ));
        assert!(matches!(
            classify("Disk.readText"),
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
