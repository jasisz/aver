//! Single source of truth for the prelude helper blocks every proof
//! backend keeps inlined into its emit.
//!
//! Lean has hand-written `LEAN_PRELUDE_*` constants for things like
//! `AverDigits` numeric parsing, `String.charAt`/`String.slice`,
//! `Char.toCode`/`byteToHex`, the `AverList` recursion helpers, and
//! `BranchPath`'s `child`/`parse` constructors. Dafny has the same
//! shape sitting inside `DAFNY_PRELUDE_AFTER_RECORDS`. Each backend
//! re-decides locally whether to include each piece, with a mix of
//! `body.contains("...")` shortcuts and "always include" defaults.
//!
//! This module is the shared decision layer:
//! - [`BUILTIN_HELPERS`] declares each helper with its detection
//!   token (a substring searched in the generated body) and its
//!   declarative dependencies.
//! - [`needed_helpers`] returns the helpers a given body actually
//!   uses, in dependency-correct emission order.
//!
//! Each backend keeps its own native rendering of every helper —
//! this module does not try to render Lean / Dafny / WASM bodies
//! from one declarative shape (they're hand-tuned to each prover's
//! idiom). It only decides which keys to include.

/// Keys for built-in prelude helpers. Each key has a per-backend
/// implementation (e.g. `LEAN_PRELUDE_NUMERIC_PARSE` in the Lean
/// backend) the backend looks up by key.
pub struct BuiltinHelper {
    /// Stable string key (used by backends to look up their native
    /// implementation of this helper).
    pub key: &'static str,
    /// Substrings searched in the generated body. The helper is
    /// included if **any** of them appear. Backends can render the
    /// same concept under different identifiers (`String.charAt` in
    /// Lean output, `StringCharAt` in Dafny output), so listing all
    /// known forms keeps the decision conservative — a helper might
    /// be included slightly more often than strictly needed, but it
    /// is never missed.
    pub body_tokens: &'static [&'static str],
    /// Other helpers this one depends on; emitted before this one.
    pub depends_on: &'static [&'static str],
    /// Human-readable note for maintainers.
    pub doc: &'static str,
}

/// All known prelude helpers. Order roughly mirrors emission order;
/// [`needed_helpers`] performs a topological sort using `depends_on`
/// so explicit ordering here is a maintenance hint, not a hard
/// requirement.
pub const BUILTIN_HELPERS: &[BuiltinHelper] = &[
    BuiltinHelper {
        key: "BranchPath",
        body_tokens: &["BranchPath"],
        // Dafny's `BranchPath_child` uses `IntToString` to format the
        // index; that opaque sits in `NumericParse`. Lean's BranchPath
        // doesn't need it, but pulling NumericParse in either way is
        // small and avoids a Dafny build failure.
        depends_on: &["NumericParse"],
        doc: "Oracle's structural addressing: `BranchPath.Root`, `.child`, `.parse`. \
              Needed whenever any classified-effect function is lifted into proof.",
    },
    BuiltinHelper {
        key: "AverList",
        body_tokens: &["AverList.", "ListReverse(", "ListHead(", "ListTail(", "ListTake(", "ListDrop("],
        depends_on: &[],
        doc: "Recursion helpers and structural list utilities (Lean's `AverList.` namespace; \
              Dafny's `ListReverse` / `ListHead` / `ListTail` / `ListTake` / `ListDrop`).",
    },
    BuiltinHelper {
        key: "StringHelpers",
        body_tokens: &[
            "String.charAt", "String.slice", "String.chars",
            "StringCharAt(", "StringChars(", "StringJoin(",
            "AverString",
        ],
        depends_on: &[],
        doc: "Character/slice/intercalate utilities. Lean: `String.charAt`, `String.slice`, \
              `String.chars`, `AverString.split`. Dafny: opaque `StringCharAt`, `StringChars`, \
              `StringJoin`.",
    },
    BuiltinHelper {
        key: "NumericParse",
        body_tokens: &[
            "AverDigits.",
            "String.fromInt", "Int.fromString",
            "Float.fromString", "Float.fromInt",
            "IntToString(", "IntFromString(", "FloatToString(", "FloatFromString(",
        ],
        depends_on: &[],
        doc: "Decimal parsing/formatting. Lean: full `AverDigits` namespace, `String.fromInt`, \
              `Int.fromString`, `Float.fromString`. Dafny: opaque `IntToString` / `IntFromString` \
              / `FloatToString` / `FloatFromString` declarations.",
    },
    BuiltinHelper {
        key: "CharByte",
        body_tokens: &[
            "Char.toCode", "Char.fromCode",
            "byteToHex", "AverByte.",
            "CharToCode(", "CharFromCode(", "ByteToHex(", "ByteFromHex(",
        ],
        depends_on: &[],
        doc: "Character-code helpers and hex byte utilities (Lean's `Char.toCode` / `byteToHex` / \
              `AverByte`; Dafny's opaque `CharToCode` / `CharFromCode` / `ByteToHex` / `ByteFromHex`).",
    },
    BuiltinHelper {
        key: "AverMeasure",
        body_tokens: &["AverMeasure."],
        depends_on: &[],
        doc: "Decreasing measures used by termination proofs of generic recursion shapes \
              (Lean only — Dafny uses native `decreases`).",
    },
    BuiltinHelper {
        key: "AverMap",
        body_tokens: &[
            "AverMap.",
            "MapGet(", "MapEntries(", "MapFromList(",
        ],
        depends_on: &[],
        doc: "Map helper namespace. Lean: `AverMap.has_set_self` / `.get_set_self` / etc. \
              Dafny: `MapGet` / `MapEntries` / `MapFromList`.",
    },
    BuiltinHelper {
        key: "ProofFuel",
        body_tokens: &["averStringPosFuel"],
        depends_on: &[],
        doc: "Proof-mode fuel measure for string-position recursion (Lean only).",
    },
];

/// Look up a helper by key.
pub fn find(key: &str) -> Option<&'static BuiltinHelper> {
    BUILTIN_HELPERS.iter().find(|h| h.key == key)
}

/// Cheap substring scan: any of the helper's tokens appearing in the
/// generated body counts as needing the helper.
fn any_token_in_body(body: &str, tokens: &[&str]) -> bool {
    tokens.iter().any(|t| body.contains(t))
}

/// Shared decision: which helpers does this generated body need, in
/// dependency-correct emission order? Each backend then renders the
/// returned helpers through its own native implementation table.
///
/// `force_all` requests every helper regardless of body usage —
/// useful for tests that want to exercise the full prelude rendering.
pub fn needed_helpers(body: &str, force_all: bool) -> Vec<&'static BuiltinHelper> {
    let mut selected: Vec<&'static BuiltinHelper> = Vec::new();

    fn include_with_deps(
        helper: &'static BuiltinHelper,
        out: &mut Vec<&'static BuiltinHelper>,
    ) {
        if out.iter().any(|h| h.key == helper.key) {
            return;
        }
        for dep in helper.depends_on {
            if let Some(d) = find(dep) {
                include_with_deps(d, out);
            }
        }
        out.push(helper);
    }

    for helper in BUILTIN_HELPERS {
        let directly_needed = any_token_in_body(body, helper.body_tokens);
        if force_all || directly_needed {
            include_with_deps(helper, &mut selected);
        }
    }

    selected
}

/// Convenience: just the keys, in emission order.
pub fn needed_keys(body: &str, force_all: bool) -> Vec<&'static str> {
    needed_helpers(body, force_all)
        .iter()
        .map(|h| h.key)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_body_needs_no_helpers() {
        assert!(needed_helpers("", false).is_empty());
    }

    #[test]
    fn body_with_aver_digits_pulls_numeric_parse_only() {
        let keys = needed_keys("foo AverDigits.bar baz", false);
        assert_eq!(keys, vec!["NumericParse"]);
    }

    #[test]
    fn body_with_string_char_at_pulls_string_helpers() {
        let keys = needed_keys("...String.charAt s 0...", false);
        assert_eq!(keys, vec!["StringHelpers"]);
    }

    #[test]
    fn body_with_branch_path_pulls_branch_path_and_numeric_parse_dep() {
        let keys = needed_keys("rollOnce BranchPath.Root rnd", false);
        // BranchPath depends on NumericParse for Dafny's IntToString.
        assert_eq!(keys, vec!["NumericParse", "BranchPath"]);
    }

    #[test]
    fn body_with_multiple_tokens_returns_all_in_emission_order() {
        let body = "AverDigits. String.charAt Char.toCode AverList. averStringPosFuel BranchPath";
        let keys = needed_keys(body, false);
        // Emission order: declaration order in BUILTIN_HELPERS, with
        // dependencies first. BranchPath drags NumericParse forward;
        // NumericParse otherwise sits later.
        assert_eq!(
            keys,
            vec![
                "NumericParse",
                "BranchPath",
                "AverList",
                "StringHelpers",
                "CharByte",
                "ProofFuel",
            ]
        );
    }

    #[test]
    fn force_all_returns_every_helper() {
        let keys = needed_keys("", true);
        // BranchPath depends on NumericParse so NumericParse comes
        // first; the rest follow declaration order.
        assert_eq!(
            keys,
            vec![
                "NumericParse",
                "BranchPath",
                "AverList",
                "StringHelpers",
                "CharByte",
                "AverMeasure",
                "AverMap",
                "ProofFuel",
            ]
        );
    }

    #[test]
    fn every_helper_key_lookup_round_trips() {
        for h in BUILTIN_HELPERS {
            assert_eq!(find(h.key).map(|x| x.key), Some(h.key));
        }
    }
}
