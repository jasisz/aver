//! Single source of truth for the prelude helper blocks every proof
//! backend keeps inlined into its emit.
//!
//! Lean has hand-written `LEAN_PRELUDE_*` constants for things like
//! `AverDigits` numeric parsing, `String.charAt`/`String.slice`,
//! `Char.toCode`, the `AverList` recursion helpers, and
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
        // Dafny's `BranchPath_child` uses `IntToString` (in NumericParse)
        // and references the `datatype BranchPath` (in BranchPathDatatype).
        // Lean's BranchPath structure includes both the datatype and the
        // constructors in one block, so it ignores BranchPathDatatype
        // and the NumericParse dependency.
        depends_on: &["NumericParse", "BranchPathDatatype"],
        doc: "Oracle's structural addressing: `BranchPath.Root`, `.child`, `.parse`. \
              Needed whenever any classified-effect function is lifted into proof.",
    },
    BuiltinHelper {
        key: "AverList",
        body_tokens: &[
            "AverList.",
            "ListReverse(",
            "ListHead(",
            "ListTail(",
            "ListTake(",
            "ListDrop(",
        ],
        // Dafny's `ListHead` returns `Option<T>`. Lean has native Option.
        depends_on: &["OptionDatatype"],
        doc: "Recursion helpers and structural list utilities (Lean's `AverList.` namespace; \
              Dafny's `ListReverse` / `ListHead` / `ListTail` / `ListTake` / `ListDrop`).",
    },
    BuiltinHelper {
        key: "StringHelpers",
        body_tokens: &[
            "String.charAt",
            "String.slice",
            "String.chars",
            "String.containsSubstr",
            "containsSubstr ",
            "StringCharAt(",
            "StringChars(",
            "StringSlice(",
            "StringJoin(",
            "StringSplit(",
            "StringContains(",
            "StringStartsWith(",
            "StringEndsWith(",
            "StringTrim(",
            "StringReplace(",
            "StringRepeat(",
            "StringIndexOf(",
            "StringToUpper(",
            "StringToLower(",
            "StringFromBool(",
            "StringByteLength(",
            "AverString",
        ],
        // Dafny's `StringCharAt` returns `Option<string>`.
        depends_on: &["OptionDatatype"],
        doc: "Character/slice/intercalate + split/contains/replace/etc. \
              Lean: native `String.*`. Dafny: opaque `StringCharAt`, `StringChars`, \
              `StringJoin`, `StringSplit`, `StringContains`, etc.",
    },
    BuiltinHelper {
        key: "NumericParse",
        body_tokens: &[
            "AverDigits.",
            "String.fromInt",
            "Int.fromString",
            "Float.fromString",
            "Float.fromInt",
            "IntToString(",
            "IntFromString(",
            "FloatToString(",
            "FloatFromString(",
            "FloatPi(",
            "FloatSqrt(",
            "FloatPow(",
            "FloatToInt(",
            "FloatSin(",
            "FloatCos(",
            "FloatAtan2(",
            "FloatDiv(",
        ],
        // Dafny's `IntFromString` / `FloatFromString` declarations
        // return `Result<int, string>` / `Result<real, string>`, so
        // the Result datatype must be in scope. Lean uses native
        // `Except` (no-op there).
        depends_on: &["ResultDatatype"],
        doc: "Decimal parsing/formatting. Lean: full `AverDigits` namespace, `String.fromInt`, \
              `Int.fromString`, `Float.fromString`. Dafny: opaque `IntToString` / `IntFromString` \
              / `FloatToString` / `FloatFromString` declarations.",
    },
    BuiltinHelper {
        key: "CharCode",
        body_tokens: &[
            "Char.toCode",
            "Char.fromCode",
            "CharToCode(",
            "CharFromCode(",
        ],
        // Dafny's `CharFromCode` returns `Option<string>`.
        depends_on: &["OptionDatatype"],
        doc: "Character-code helpers.",
    },
    BuiltinHelper {
        key: "AverBits",
        // Lean renders the namespace as `AverBits.*`; Dafny spells the same
        // definitions `BitsAnd(` / `BitsShiftLeft(` / ... at call sites.
        body_tokens: &[
            "AverBits.",
            "BitsAnd(",
            "BitsOr(",
            "BitsXor(",
            "BitsNot(",
            "BitsShiftLeft(",
            "BitsShiftRight(",
            "BitsLow(",
        ],
        depends_on: &[],
        doc: "Bit-level view of `Int` under infinite two's complement, backing the `Bits` \
              namespace. Both provers define it — never opaque, never a bit-vector.",
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
            "MapGet(",
            "MapEntries(",
            "MapKeys(",
            "MapValues(",
            "MapFromList(",
        ],
        // Dafny's `MapGet` returns `Option<V>`.
        depends_on: &["OptionDatatype"],
        doc: "Map helper namespace. Lean: `AverMap.has_set_self` / `.get_set_self` / etc. \
              Dafny: `MapGet` / `MapEntries` / `MapKeys` / `MapValues` / `MapFromList`.",
    },
    BuiltinHelper {
        key: "ProofFuel",
        body_tokens: &["averStringPosFuel"],
        depends_on: &[],
        doc: "Proof-mode fuel measure for string-position recursion (Lean only).",
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
              functions in proof samples (Lean only).",
    },
    BuiltinHelper {
        key: "ExceptInstances",
        body_tokens: &["Except", ".ok", ".error"],
        depends_on: &[],
        doc: "`DecidableEq (Except ε α)`, the `Except` namespace's `withDefault`, and \
              `Option.toExcept`. Needed whenever a function or law mentions a Lean `Except` \
              (Aver `Result`) value (Lean only).",
    },
    BuiltinHelper {
        key: "StringHadd",
        body_tokens: &["String"],
        depends_on: &[],
        doc: "`HAdd String String String` instance for string-concat literals like `\"a\" ++ \"b\"`. \
              Cheap to ship but unused on pure-Int examples (Lean only).",
    },
    // Dafny-side built-in datatypes. Lean has Except / Option in its
    // standard library so it doesn't need declarations; Dafny ships
    // them in the prelude. Detection is body-token based to skip on
    // pure-Int examples.
    BuiltinHelper {
        key: "ResultDatatype",
        body_tokens: &["Result<", "Result.Ok", "Result.Err", "Ok(", "Err("],
        depends_on: &[],
        doc: "Dafny `datatype Result<T, E>` declaration plus `ResultWithDefault` destructor. \
              Lean uses native `Except`; this key is a no-op there.",
    },
    BuiltinHelper {
        key: "OptionDatatype",
        body_tokens: &["Option<", "Option.Some", "Option.None", "Some(", "None"],
        depends_on: &[],
        doc: "Dafny `datatype Option<T>` declaration plus `OptionWithDefault` destructor. \
              Lean uses native `Option`; this key is a no-op there.",
    },
    BuiltinHelper {
        key: "OptionToResult",
        body_tokens: &["OptionToResult"],
        depends_on: &["ResultDatatype", "OptionDatatype"],
        doc: "Dafny `OptionToResult` bridge function. Pulled implicitly when both Result and \
              Option are in scope and the body explicitly converts between them.",
    },
    BuiltinHelper {
        key: "BranchPathDatatype",
        body_tokens: &["BranchPath"],
        depends_on: &[],
        doc: "Dafny `datatype BranchPath`. Lean's `BranchPath` structure is part of the \
              `BranchPath` helper key (the constructors come with it); Dafny separates the \
              datatype declaration from the constructor functions so the datatype itself \
              can stay even on pure-math files (it doesn't, currently — Dafny no-ops this on pure files).",
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

/// Convenience: just the keys, in emission order. Currently used
/// only by tests; backends call `needed_helpers` directly.
#[allow(dead_code)]
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
    fn body_with_aver_digits_pulls_numeric_parse_with_result_dep() {
        // NumericParse depends on ResultDatatype (Dafny's IntFromString
        // returns Result).
        let keys = needed_keys("foo AverDigits.bar baz", false);
        assert_eq!(keys, vec!["ResultDatatype", "NumericParse"]);
    }

    #[test]
    fn body_with_string_char_at_pulls_string_helpers() {
        // `String.charAt` contains the substring `String`, so the small
        // `StringHadd` instance bundle gets pulled in alongside.
        // StringHelpers also depends on OptionDatatype, which is emitted
        // before StringHelpers (deps go first).
        let keys = needed_keys("...String.charAt s 0...", false);
        assert_eq!(keys, vec!["OptionDatatype", "StringHelpers", "StringHadd"]);
    }

    #[test]
    fn body_with_branch_path_pulls_branch_path_and_its_deps() {
        let keys = needed_keys("rollOnce BranchPath.Root rnd", false);
        // BranchPath depends on NumericParse and BranchPathDatatype.
        // NumericParse in turn depends on ResultDatatype.
        assert_eq!(
            keys,
            vec![
                "ResultDatatype",
                "NumericParse",
                "BranchPathDatatype",
                "BranchPath",
            ]
        );
    }

    #[test]
    fn body_with_multiple_tokens_returns_all_in_emission_order() {
        // Many tokens drive in many helpers; each helper drags in its
        // declared dependencies. The exact set is a conjunction of:
        // explicit token matches + transitive deps.
        let body = "AverDigits. String.charAt Char.toCode AverList. averStringPosFuel BranchPath";
        let keys = needed_keys(body, false);
        assert_eq!(
            keys,
            vec![
                "ResultDatatype",
                "NumericParse",
                "BranchPathDatatype",
                "BranchPath",
                "OptionDatatype",
                "AverList",
                "StringHelpers",
                "CharCode",
                "ProofFuel",
                "StringHadd",
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
                "ResultDatatype",
                "NumericParse",
                "BranchPathDatatype",
                "BranchPath",
                "OptionDatatype",
                "AverList",
                "StringHelpers",
                "CharCode",
                "AverBits",
                "AverMeasure",
                "AverMap",
                "ProofFuel",
                "FloatInstances",
                "ExceptInstances",
                "StringHadd",
                "OptionToResult",
            ]
        );
    }

    #[test]
    fn pure_int_body_skips_lean_instance_bundles() {
        // No Float / Except / String tokens — the instance bundles
        // shouldn't be requested.
        let body = "def absVal (x : Int) : Int := if x < 0 then -x else x";
        let keys = needed_keys(body, false);
        assert!(!keys.contains(&"FloatInstances"));
        assert!(!keys.contains(&"ExceptInstances"));
        assert!(!keys.contains(&"StringHadd"));
    }

    #[test]
    fn body_with_float_pulls_float_instances() {
        let body = "def f (x : Float) : Float := x + 1.0";
        assert!(needed_keys(body, false).contains(&"FloatInstances"));
    }

    #[test]
    fn every_helper_key_lookup_round_trips() {
        for h in BUILTIN_HELPERS {
            assert_eq!(find(h.key).map(|x| x.key), Some(h.key));
        }
    }
}
