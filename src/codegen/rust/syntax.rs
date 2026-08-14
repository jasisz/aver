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

/// The five names Rust cannot spell as an identifier at all, not even
/// behind `r#`: the four *words* `Self`, `crate`, `self` and `super`, for
/// which a raw identifier is explicitly rejected, and the wildcard `_`,
/// which is not a word at all.
///
/// For every other entry in [`RUST_RESERVED`] the `r#` escape is a complete
/// answer, so the emitter carries the name through unchanged. For these
/// five there is no escape, so the name is renamed instead — see
/// [`MANGLE_PREFIX`] and [`aver_name_to_rust`].
///
/// All five are ordinary Aver identifiers (`src/lexer.rs` takes `_` as one
/// too), so a program can name a function, a parameter, a binding, a match
/// binder or a record field with any of them.
const RUST_NEVER_SPELLABLE: &[&str] = &["Self", "_", "crate", "self", "super"];

/// True when `name` has no Rust spelling at all — neither bare nor behind
/// `r#`. `pub fn _()` is `expected identifier, found reserved identifier`,
/// `mut _` is ``mut` must be followed by a named binding``, `_` as a call
/// argument is ``in expressions, `_` can only be used on the left-hand side
/// of an assignment``, `r#_` is `` `_` cannot be a raw identifier ``, and
/// `r#crate` is `` `crate` cannot be a raw identifier ``.
pub(crate) fn is_never_spellable_in_rust(name: &str) -> bool {
    RUST_NEVER_SPELLABLE.contains(&name)
}

/// True when `name` needs the `r#` escape to stand as a Rust identifier.
pub(crate) fn is_rust_reserved(name: &str) -> bool {
    RUST_RESERVED.contains(&name)
}

/// Upper-case the first character, leaving the rest alone.
///
/// The mutual-recursion trampoline builds its enum variants this way.
///
/// Note `char::to_uppercase` is not "make ASCII uppercase": it is the
/// Unicode mapping, and it is neither injective nor length-preserving. In
/// particular `ſ` (U+017F LATIN SMALL LETTER LONG S) maps to `S`, which is
/// the one way an Aver name other than `self`/`Self` can capitalise into a
/// Rust keyword. Capitalising FIRST and spelling the result through
/// [`aver_name_to_rust`] is what makes that need no special case:
/// `fn ſelf` capitalises to `Self`, which is unspellable, which the
/// spelling step then renames like any other.
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

/// The prefix the Rust backend puts in front of a name it has no other way
/// to write down.
///
/// It leads with `_` on purpose. The name that most needs this is Aver's
/// own wildcard: a user who writes `_` is saying they do not care about
/// that binding, and `_avr__` keeps rustc's unused-variable lint quiet
/// about exactly the thing they said to ignore.
pub(crate) const MANGLE_PREFIX: &str = "_avr_";

/// Convert an Aver identifier to a valid Rust identifier that stands
/// ALONE.
///
/// Three answers, in order:
///
/// 1. A name with no Rust spelling at all ([`is_never_spellable_in_rust`])
///    gets [`MANGLE_PREFIX`] in front of it: `self` → `_avr_self`, `_` →
///    `_avr__`.
/// 2. A name that ALREADY starts with the prefix gets it again:
///    `_avr_self` → `_avr__avr_self`. This is what keeps the map
///    injective — without it a user's own `_avr_self` and Aver's rename of
///    `self` would be the same Rust name, and two different things would
///    silently become one.
/// 3. Everything else keeps today's behaviour: verbatim, or the `r#`
///    escape for the [`RUST_RESERVED`] keywords, which is a complete
///    answer for all of them.
///
/// So the map is injective: rule 1 and rule 2 produce exactly the names
/// beginning with the prefix and are each injective on their own domain;
/// rule 3 is identity or a `#`-bearing name, and an Aver identifier can
/// contain neither the prefix (rule 2 catches those) nor a `#`. It is also
/// the IDENTITY for every name that is not one of the five and does not
/// start with the prefix, which is every name in the corpus — so no
/// existing program's emitted bytes move.
///
/// This renames rather than refusing. The rename is deterministic, it is
/// confined to the Rust backend's spelling of a name (the VM, wasm, Lean
/// and Dafny backends never call this), and the only place a user meets it
/// is cosmetic: a mangled name in a generated-project backtrace or in the
/// playground's Rust export. Refusing instead would have turned five
/// ordinary Aver names into names no program may use on one backend.
///
/// Do not call this on a name that will then be embedded inside a longer
/// identifier. `r#` is a prefix on a whole identifier, so
/// `format!("test_{}", aver_name_to_rust("await"))` is `test_r#await` — a
/// `#` in the middle of a word, which ends the identifier and fails to
/// parse. A name in that position never needs either treatment anyway: the
/// surrounding text makes the result a new word, and a new word that is
/// neither a keyword nor unspellable. Compose from the raw name;
/// `emit_verify_blocks` in `toplevel.rs` is the one site that does this.
pub fn aver_name_to_rust(name: &str) -> String {
    if is_never_spellable_in_rust(name) || name.starts_with(MANGLE_PREFIX) {
        format!("{MANGLE_PREFIX}{name}")
    } else {
        crate::codegen::common::escape_reserved_word_prefix(name, RUST_RESERVED, "r#")
    }
}
