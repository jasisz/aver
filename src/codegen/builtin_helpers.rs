//! Single source of truth for the prelude helper blocks the Lean proof
//! backend keeps inlined into its emit.
//!
//! Lean has hand-written `LEAN_PRELUDE_*` constants for things like
//! `AverDigits` numeric parsing, `String.charAt`/`String.slice`,
//! String code-point helpers, the `AverList` recursion helpers, and
//! `BranchPath`'s `child`/`parse` constructors.
//!
//! This module is the decision layer:
//! - [`BUILTIN_HELPERS`] declares each helper with its detection
//!   token (a substring searched in the generated body) and its
//!   declarative dependencies.
//! - [`needed_helpers`] returns the helpers a given body actually
//!   uses, in dependency-correct emission order.
//!
//! The backend keeps its own native rendering of every helper — this
//! module only decides which keys to include.

/// Keys for built-in prelude helpers. Each key has a per-backend
/// implementation (e.g. `LEAN_PRELUDE_NUMERIC_PARSE` in the Lean
/// backend) the backend looks up by key.
pub struct BuiltinHelper {
    /// Stable string key (used by backends to look up their native
    /// implementation of this helper).
    pub key: &'static str,
    /// Substrings searched in the generated body. The helper is
    /// included if **any** of them appear. Listing every known form
    /// keeps the decision conservative — a helper might be included
    /// slightly more often than strictly needed, but it is never
    /// missed.
    pub body_tokens: &'static [&'static str],
    /// Other helpers this one depends on; emitted before this one.
    pub depends_on: &'static [&'static str],
    /// Maintainer-facing description; not consumed by codegen.
    #[allow(dead_code)]
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
        depends_on: &["NumericParse"],
        doc: "Oracle's structural addressing: `BranchPath.Root`, `.child`, `.parse`. \
              Needed whenever any classified-effect function is lifted into proof.",
    },
    BuiltinHelper {
        key: "AverList",
        body_tokens: &["AverList."],
        depends_on: &[],
        doc: "Recursion helpers and structural list utilities (`AverList.` namespace).",
    },
    BuiltinHelper {
        key: "StringHelpers",
        body_tokens: &[
            "String.charAt",
            "String.slice",
            "String.chars",
            "String.containsSubstr",
            "containsSubstr ",
            "AverString",
        ],
        depends_on: &[],
        doc: "Character/slice/intercalate + split/contains/replace/trim over native `String.*`.",
    },
    BuiltinHelper {
        key: "StringCase",
        body_tokens: &["AverUnicodeCase."],
        depends_on: &[],
        doc: "Unicode case definitions from shared VM-checked mapping and context tables.",
    },
    BuiltinHelper {
        key: "NumericParse",
        body_tokens: &[
            "AverDigits.",
            "String.fromInt",
            "Int.fromString",
            "Float.fromString",
            "Float.fromInt",
        ],
        depends_on: &[],
        doc: "Decimal parsing/formatting: the `AverDigits` namespace, `String.fromInt`, \
              `Int.fromString`, `Float.fromString`.",
    },
    BuiltinHelper {
        key: "StringCodePoint",
        body_tokens: &["String.firstCodePoint", "String.fromCodePoint"],
        depends_on: &[],
        doc: "Unicode scalar-value helpers owned by String.",
    },
    BuiltinHelper {
        key: "AverBits",
        body_tokens: &["AverBits."],
        depends_on: &[],
        doc: "Bit-level view of `Int` under infinite two's complement, backing the `Bits` \
              namespace — never opaque, never a bit-vector.",
    },
    BuiltinHelper {
        key: "AverMeasure",
        body_tokens: &["AverMeasure."],
        depends_on: &[],
        doc: "Decreasing measures used by termination proofs of generic recursion shapes.",
    },
    BuiltinHelper {
        key: "AverMap",
        body_tokens: &["AverMap."],
        depends_on: &[],
        doc: "Map helper namespace: `AverMap.has_set_self` / `.get_set_self` / etc.",
    },
    BuiltinHelper {
        key: "ProofFuel",
        body_tokens: &["averStringPosFuel"],
        depends_on: &[],
        doc: "Proof-mode fuel measure for string-position recursion.",
    },
    // Small Lean instance bundles. These used to be unconditional (~32
    // lines) but are unnecessary on pure-Int examples; they're cheap
    // enough that detecting via substring is fine.
    BuiltinHelper {
        key: "FloatInstances",
        body_tokens: &["Float"],
        depends_on: &[],
        doc: "`Coe Int Float`, `Float.fromInt`, `Float.unsafeDecEq` / `Float.compDecEq`, \
              and the `DecidableEq Float` instance used by `=>`-equality on Float-returning \
              functions in proof samples.",
    },
    BuiltinHelper {
        key: "ExceptInstances",
        body_tokens: &["Except", ".ok", ".error"],
        depends_on: &[],
        doc: "`DecidableEq (Except ε α)`, the `Except` namespace's `withDefault`, and \
              `Option.toExcept`. Needed whenever a function or law mentions a Lean `Except` \
              (Aver `Result`) value.",
    },
    BuiltinHelper {
        key: "StringHadd",
        body_tokens: &["String"],
        depends_on: &[],
        doc: "`HAdd String String String` instance for string-concat literals like `\"a\" ++ \"b\"`. \
              Cheap to ship but unused on pure-Int examples.",
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

    fn include_with_deps(helper: &'static BuiltinHelper, out: &mut Vec<&'static BuiltinHelper>) {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn keys(body: &str, force_all: bool) -> Vec<&'static str> {
        needed_helpers(body, force_all)
            .iter()
            .map(|h| h.key)
            .collect()
    }

    #[test]
    fn empty_body_needs_no_helpers() {
        assert!(needed_helpers("", false).is_empty());
    }

    #[test]
    fn body_with_aver_digits_pulls_numeric_parse() {
        assert_eq!(keys("foo AverDigits.bar baz", false), vec!["NumericParse"]);
    }

    #[test]
    fn body_with_string_char_at_pulls_string_helpers() {
        // `String.charAt` contains the substring `String`, so the small
        // `StringHadd` instance bundle gets pulled in alongside.
        assert_eq!(
            keys("...String.charAt s 0...", false),
            vec!["StringHelpers", "StringHadd"]
        );
    }

    #[test]
    fn body_with_branch_path_pulls_branch_path_and_its_deps() {
        // BranchPath depends on NumericParse, which is emitted first.
        assert_eq!(
            keys("rollOnce BranchPath.Root rnd", false),
            vec!["NumericParse", "BranchPath"]
        );
    }

    #[test]
    fn body_with_multiple_tokens_returns_all_in_emission_order() {
        // Many tokens drive in many helpers; each helper drags in its
        // declared dependencies. The exact set is a conjunction of:
        // explicit token matches + transitive deps.
        let body = "AverDigits. String.charAt String.firstCodePointAv AverList. averStringPosFuel BranchPath";
        assert_eq!(
            keys(body, false),
            vec![
                "NumericParse",
                "BranchPath",
                "AverList",
                "StringHelpers",
                "StringCodePoint",
                "ProofFuel",
                "StringHadd",
            ]
        );
    }

    #[test]
    fn force_all_returns_every_helper() {
        // BranchPath depends on NumericParse so NumericParse comes
        // first; the rest follow declaration order.
        assert_eq!(
            keys("", true),
            vec![
                "NumericParse",
                "BranchPath",
                "AverList",
                "StringHelpers",
                "StringCase",
                "StringCodePoint",
                "AverBits",
                "AverMeasure",
                "AverMap",
                "ProofFuel",
                "FloatInstances",
                "ExceptInstances",
                "StringHadd",
            ]
        );
    }

    #[test]
    fn pure_int_body_skips_lean_instance_bundles() {
        // No Float / Except / String tokens — the instance bundles
        // shouldn't be requested.
        let body = "def absVal (x : Int) : Int := if x < 0 then -x else x";
        let keys = keys(body, false);
        assert!(!keys.contains(&"FloatInstances"));
        assert!(!keys.contains(&"ExceptInstances"));
        assert!(!keys.contains(&"StringHadd"));
    }

    #[test]
    fn body_with_float_pulls_float_instances() {
        let body = "def f (x : Float) : Float := x + 1.0";
        assert!(keys(body, false).contains(&"FloatInstances"));
    }

    #[test]
    fn every_helper_key_lookup_round_trips() {
        for h in BUILTIN_HELPERS {
            assert_eq!(find(h.key).map(|x| x.key), Some(h.key));
        }
    }
}
