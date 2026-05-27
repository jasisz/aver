//! Identity-keying guardrails for the codegen layer.
//!
//! Epic #170 Phase 8 — the final acceptance gate. This test scans
//! the codebase for the patterns the epic eliminated and fails the
//! build if any of them reappear without an explicit
//! "this is OK because …" comment.
//!
//! ## Rationale
//!
//! Phases 1–7 of #170 moved every identity-sensitive lookup off
//! bare-string keying to typed IDs (`FnId` / `TypeId` / `CtorId`).
//! The patterns below were the smell-shapes the audit found —
//! removing them once isn't enough, because a contributor unaware
//! of the epic could re-introduce them in a future PR. The test
//! enforces "you can do this, but you must say WHY out loud in a
//! code comment" — the comment forces the contributor to confront
//! the keying question explicitly.
//!
//! ## Allowed escape categories
//!
//! Each banned pattern can be made non-fatal by adding ONE of the
//! following category strings to a comment on the same line OR
//! within the 12 lines immediately above:
//!
//! - `diagnostic-only` — pattern walks raw AST for spans /
//!   diagnostic messages / error context. No identity decision
//!   reaches the backend output.
//! - `syntax-discovery-only` — pattern walks raw AST for source-
//!   shape recognition (e.g. proof-mode classifier detecting
//!   `match n { 0 -> base; _ -> rec(n-1) }`). Identity gets handed
//!   off to a typed map keyed by symbol-table-canonicalised IDs.
//! - `backend-link-stage` — pattern lives inside a documented
//!   post-link view (e.g. wasm-gc `WasmGcLinkedView` after
//!   `flatten_multimodule`). The link stage's namespace is the
//!   identity layer; bare-name lookups against it are safe by the
//!   stage's own invariant.
//! - `temporary-migration-bridge` — pattern is acknowledged as
//!   debt with a known follow-up scope. New code MUST NOT add this
//!   category; existing tagged sites stay until their migration
//!   trigger lands.
//!
//! ## When this test fails
//!
//! Either:
//! 1. Switch the call to the typed-identity equivalent
//!    (`fn_id_for_decl(ctx, fd)`, `ctx.resolved_program.fn_by_id`,
//!    `symbol_table.fn_id_of(&FnKey::…)`, etc.); OR
//! 2. Add a category comment on the same line / within 12 lines
//!    above explaining WHY the pattern is identity-safe in this
//!    specific context.
//!
//! See `src/codegen/mod.rs` `CodegenContext` doc-comment for the
//! invariant.

use std::fs;
use std::path::{Path, PathBuf};

/// Files the guardrail walks. Restricted to the backend layer where
/// the identity invariant applies — the typechecker, parser, IR
/// builders are exempt by design (Phase B from issue #147 is a
/// separate scope).
const SCAN_ROOTS: &[&str] = &["src/codegen", "src/verify_law.rs"];

/// Files entirely exempt from the guardrail. Reserve for places
/// where the patterns are genuinely the right shape (e.g. the test
/// file itself documents the bad shapes).
const EXEMPT_FILES: &[&str] = &[
    // This file ENUMERATES the bad shapes — every banned pattern
    // appears here as a string literal. Self-scanning would never
    // terminate.
    "tests/identity_guardrails.rs",
    // Diagnostics test stubs are allowed to use whatever shape they
    // want — they're inside `#[cfg(test)]` regions.
    "src/codegen/wasm_gc/tests.rs",
];

const ALLOWED_CATEGORIES: &[&str] = &[
    "diagnostic-only",
    "syntax-discovery-only",
    "backend-link-stage",
    "temporary-migration-bridge",
];

/// One banned pattern.
struct BannedPattern {
    /// Substring to scan for, line-by-line.
    needle: &'static str,
    /// Short label used in failure messages.
    label: &'static str,
}

const BANNED_PATTERNS: &[BannedPattern] = &[
    BannedPattern {
        needle: "FnKey::entry(&fd.name)",
        label: "FnKey::entry(&fd.name) — identity-leaks for module-owned fns",
    },
    BannedPattern {
        needle: "HashMap<String, FnContract>",
        label: "HashMap<String, FnContract> — must be HashMap<FnId, FnContract>",
    },
    BannedPattern {
        needle: "HashMap<String, RefinedTypeDecl>",
        label: "HashMap<String, RefinedTypeDecl> — must be HashMap<TypeId, …>",
    },
    BannedPattern {
        needle: ".rsplit('.').next() == Some(",
        label: "suffix match on dotted name — accepts Foo.target as 'target'",
    },
];

#[test]
fn no_banned_identity_patterns_without_category_comment() {
    let mut violations: Vec<String> = Vec::new();

    for root in SCAN_ROOTS {
        let root_path = PathBuf::from(root);
        if root_path.is_file() {
            scan_file(&root_path, &mut violations);
        } else {
            walk_dir(&root_path, &mut violations);
        }
    }

    if !violations.is_empty() {
        let categories = ALLOWED_CATEGORIES.join(", ");
        panic!(
            "epic #170 Phase 8 guardrail tripped — found {} banned identity-keying \
             pattern(s) without a category comment. Each violation must either:\n\
             1. Switch to typed-identity (see `src/codegen/mod.rs::CodegenContext` doc), OR\n\
             2. Add a category comment on the same line or within 5 lines above. \
             Allowed categories: {}.\n\n\
             Violations:\n{}",
            violations.len(),
            categories,
            violations.join("\n")
        );
    }
}

fn walk_dir(dir: &Path, violations: &mut Vec<String>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            walk_dir(&path, violations);
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            scan_file(&path, violations);
        }
    }
}

fn scan_file(path: &Path, violations: &mut Vec<String>) {
    let rel = path
        .strip_prefix(env!("CARGO_MANIFEST_DIR"))
        .unwrap_or(path);
    let rel_str = rel.to_string_lossy();
    if EXEMPT_FILES.iter().any(|f| rel_str == *f) {
        return;
    }
    let Ok(content) = fs::read_to_string(path) else {
        return;
    };
    let lines: Vec<&str> = content.lines().collect();
    for (line_idx, line) in lines.iter().enumerate() {
        // Skip lines that are themselves comments — comments may
        // reference banned patterns in prose.
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") || trimmed.starts_with("///") {
            continue;
        }
        for pat in BANNED_PATTERNS {
            if !line.contains(pat.needle) {
                continue;
            }
            if has_category_comment_nearby(&lines, line_idx) {
                continue;
            }
            violations.push(format!(
                "  {}:{} — {}\n      line: {}",
                rel_str,
                line_idx + 1,
                pat.label,
                line.trim()
            ));
        }
    }
}

/// True iff the line at `idx` carries an allowed category comment
/// on the same line OR within the 12 lines immediately above it.
/// 12 lines covers the typical doc-comment block + a few lines of
/// surrounding context (declarations, `let` bindings, `match`
/// boilerplate) without becoming so wide that an unrelated upstream
/// category bleed-through silences a real violation.
fn has_category_comment_nearby(lines: &[&str], idx: usize) -> bool {
    let start = idx.saturating_sub(12);
    for window_idx in start..=idx {
        let line = lines[window_idx];
        if ALLOWED_CATEGORIES.iter().any(|cat| line.contains(cat)) {
            return true;
        }
    }
    false
}

#[test]
fn allowed_categories_appear_in_codegen_mod_doc() {
    // Self-pin: if a category gets renamed in `CodegenContext`'s
    // doc-comment (the canonical reference), this guardrail test
    // would silently start accepting different strings. Keep the
    // two in lockstep by asserting every category from
    // `ALLOWED_CATEGORIES` appears in the mod doc.
    let codegen_mod =
        fs::read_to_string(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/codegen/mod.rs"))
            .expect("read src/codegen/mod.rs");
    for cat in ALLOWED_CATEGORIES {
        assert!(
            codegen_mod.contains(cat),
            "category `{}` is enforced by the guardrail test but not documented \
             in `src/codegen/mod.rs` — keep the two in lockstep",
            cat
        );
    }
}
