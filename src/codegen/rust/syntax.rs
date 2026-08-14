use crate::ast::Literal;
use crate::ir::hir::{ResolvedMatchArm, ResolvedPattern};

pub(super) fn has_string_literal_patterns(arms: &[ResolvedMatchArm]) -> bool {
    arms.iter()
        .any(|arm| matches!(&arm.pattern, ResolvedPattern::Literal(Literal::Str(_))))
}

pub(super) fn has_list_patterns(arms: &[ResolvedMatchArm]) -> bool {
    arms.iter().any(|arm| {
        matches!(
            &arm.pattern,
            ResolvedPattern::EmptyList | ResolvedPattern::Cons(_, _)
        )
    })
}

/// Every Rust word that cannot stand as a bare identifier — the strict
/// keywords and the reserved-for-future ones — for edition 2024, which is
/// what the generated `Cargo.toml` asks for (`project.rs`).
///
/// The membership test is `rustc`, not memory: a word belongs here exactly
/// when `pub fn WORD() {}` is rejected under `--edition 2024`. That is why
/// `gen` is here (strict since 2024) and why the weak keywords — `union`,
/// `macro_rules`, `safe`, `raw`, `auto`, `default` — are not: they are
/// ordinary identifiers and escaping them would only add noise.
///
/// The list is deliberately complete rather than trimmed to the names Aver
/// can actually produce. `fn`, `match`, `type`, `true` and `false` are Aver
/// keywords too, so no program can be named with them, but a table that
/// holds only the words someone has already been bitten by is a table that
/// grows one production bug at a time — which is how `become` came to emit
/// `pub fn become`. Entries for impossible names cost nothing.
const RUST_RESERVED: &[&str] = &[
    "Self", "abstract", "as", "async", "await", "become", "box", "break", "const", "continue",
    "crate", "do", "dyn", "else", "enum", "extern", "false", "final", "fn", "for", "gen", "if",
    "impl", "in", "let", "loop", "macro", "match", "mod", "move", "mut", "override", "priv", "pub",
    "ref", "return", "self", "static", "struct", "super", "trait", "true", "try", "type", "typeof",
    "unsafe", "unsized", "use", "virtual", "where", "while", "yield",
];

/// The four Rust words that cannot be spelled as identifiers *at all* —
/// `r#` does not rescue them, because a raw identifier may not be `crate`,
/// `self`, `super` or `Self`.
///
/// For every other entry in [`RUST_RESERVED`] the `r#` escape is a complete
/// answer, so the emitter can carry the name through unchanged. For these
/// four there is no spelling, so the only honest options are to rename the
/// user's function behind their back or to refuse. Codegen refuses; see
/// `unspellable_rust_names` in the backend's `mod.rs`.
pub(crate) const RUST_NEVER_RAW: &[&str] = &["Self", "crate", "self", "super"];

/// True when `name` cannot be spelled in Rust even as a raw identifier.
pub(crate) fn is_never_raw_in_rust(name: &str) -> bool {
    RUST_NEVER_RAW.contains(&name)
}

/// True when `name` needs the `r#` escape to stand as a Rust identifier.
pub(crate) fn is_rust_reserved(name: &str) -> bool {
    RUST_RESERVED.contains(&name)
}

/// Convert an Aver identifier to a valid Rust identifier.
///
/// Valid for every reserved word except the four in [`RUST_NEVER_RAW`],
/// which have no spelling at all; those are refused before codegen runs, so
/// this function never has to answer for them.
pub fn aver_name_to_rust(name: &str) -> String {
    crate::codegen::common::escape_reserved_word_prefix(name, RUST_RESERVED, "r#")
}
