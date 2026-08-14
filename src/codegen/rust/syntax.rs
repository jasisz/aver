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

/// The four Rust *words* that cannot be spelled as identifiers at all —
/// `r#` does not rescue them, because a raw identifier may not be `crate`,
/// `self`, `super` or `Self`.
///
/// For every other entry in [`RUST_RESERVED`] the `r#` escape is a complete
/// answer, so the emitter can carry the name through unchanged. For these
/// four there is no spelling, so the only honest options are to rename the
/// user's function behind their back or to refuse. Codegen refuses; see
/// `unspellable_rust_names` in the backend's `mod.rs`.
///
/// Rust has a fifth unspellable name, `_`, which is deliberately NOT
/// listed here — see [`is_never_an_identifier_in_rust`] for why it needs a
/// different test.
pub(crate) const RUST_NEVER_RAW: &[&str] = &["Self", "crate", "self", "super"];

/// True when `name` cannot be spelled in Rust even as a raw identifier, in
/// a position where Rust would otherwise accept a wildcard.
///
/// This is the test for a `let` binding, a match binder and a parameter of
/// an ordinary function: all three lower to Rust pattern positions, where
/// `_` is legal and means "discard", so `_` must pass here. Use
/// [`is_never_an_identifier_in_rust`] where Rust demands a real name — a
/// parameter counts as that once the function is tail-recursive.
pub(crate) fn is_never_raw_in_rust(name: &str) -> bool {
    RUST_NEVER_RAW.contains(&name)
}

/// True when `name` cannot stand where Rust demands a real identifier — a
/// `fn` name, a struct field, or a parameter of a tail-recursive function,
/// which the emitter writes as `mut p` and a trampoline wrapper also passes
/// by name.
///
/// That is the four [`RUST_NEVER_RAW`] words plus `_`. `_` is not a
/// keyword, it is the wildcard, so it is not in the table and never needs
/// the escape in a pattern; but `pub fn _()` is `expected identifier,
/// found reserved identifier`, `mut _` is ``mut` must be followed by a
/// named binding``, `_` as a call argument is ``in expressions, `_` can
/// only be used on the left-hand side of an assignment``, and `r#_` is
/// `` `_` cannot be a raw identifier ``, so in these positions it has no
/// spelling either way. Aver's lexer takes `_` as an ordinary identifier
/// (`src/lexer.rs`), so a program can carry it.
pub(crate) fn is_never_an_identifier_in_rust(name: &str) -> bool {
    name == "_" || is_never_raw_in_rust(name)
}

/// True when `name` needs the `r#` escape to stand as a Rust identifier.
pub(crate) fn is_rust_reserved(name: &str) -> bool {
    RUST_RESERVED.contains(&name)
}

/// Upper-case the first character, leaving the rest alone.
///
/// The mutual-recursion trampoline builds its enum variants this way, and
/// the refusal that guards those variants has to predict the same string,
/// so both go through here rather than each spelling the transform.
///
/// Note `char::to_uppercase` is not "make ASCII uppercase": it is the
/// Unicode mapping, and it is neither injective nor length-preserving. In
/// particular `ſ` (U+017F LATIN SMALL LETTER LONG S) maps to `S`, which is
/// the one way an Aver name other than `self`/`Self` can capitalise into a
/// Rust keyword.
pub(crate) fn capitalise_first(name: &str) -> String {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) => {
            let upper: String = c.to_uppercase().collect();
            format!("{}{}", upper, chars.as_str())
        }
        None => String::new(),
    }
}

/// Convert an Aver identifier to a valid Rust identifier that stands
/// ALONE.
///
/// Valid for every reserved word except the four in [`RUST_NEVER_RAW`],
/// which have no spelling at all; those are refused before codegen runs,
/// so this function never has to answer for them. `_` passes through
/// unchanged and correctly so — it needs no escape, and it is legal in
/// the pattern positions this is called from. It reaches only those:
/// `unspellable_rust_names` refuses `_` wherever the emitter needs a real
/// name for it, which is a `fn` name, a record field, and a parameter of a
/// tail-recursive function (where the emitter writes `mut _`, and a
/// trampoline wrapper additionally passes it by name).
///
/// Do not call this on a name that will then be embedded inside a longer
/// identifier. `r#` is a prefix on a whole identifier, so
/// `format!("test_{}", aver_name_to_rust("await"))` is `test_r#await` — a
/// `#` in the middle of a word, which ends the identifier and fails to
/// parse. A name in that position never needs the escape anyway: the
/// surrounding text makes the result a new word, and a new word that is
/// not a keyword. Compose from the raw name; `emit_verify_blocks` in
/// `toplevel.rs` is the one site that does this.
pub fn aver_name_to_rust(name: &str) -> String {
    crate::codegen::common::escape_reserved_word_prefix(name, RUST_RESERVED, "r#")
}
