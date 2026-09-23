//! Lexical gates the certificate producer and the checker share.
//!
//! Every rule here is applied by the checker to untrusted package text, and by
//! the producer to the text it is about to ship. Keeping ONE implementation is
//! the point: when the two sides disagreed (a trailing-prime identifier the
//! producer wrote and the checker refused, a token the producer kept and the
//! checker scanned for), one law or one bridge could make the checker refuse
//! the WHOLE package — byte certificate included — for a defect that should
//! have cost only that claim. With one rule, the producer drops or declines
//! exactly what the checker would refuse, before it ships.

/// Elaboration-executing tokens a package `.lean` file may not carry in code
/// position (checker stage 7). `deriving` is the one token with an admitted
/// form: the closed clause [`admitted_deriving_end`] accepts.
pub const CODE_EXEC_TOKENS: [&str; 20] = [
    "#eval",
    "run_cmd",
    "run_elab",
    "run_tac",
    "initialize",
    "builtin_initialize",
    "macro",
    "macro_rules",
    "elab",
    "elab_rules",
    "syntax",
    "notation",
    "unsafe",
    "implemented_by",
    "extern",
    "deriving",
    "attribute",
    "@[",
    "«",
    "open Lean",
];

/// The classes a `deriving` clause on a type declaration may name.
///
/// A `deriving` clause runs the derive handler registered under each class
/// name. A package cannot register a handler (`initialize`, `elab` and every
/// other registration route stay banned), so the handlers reachable are the
/// pinned toolchain's own, and each of the ones admitted here produces
/// ordinary definitions and proofs the kernel checks — the same kind of
/// declaration a hand-written `instance`, which the gate has always admitted,
/// produces. The list is closed to what the model needs: `==` on a record or
/// a sum (`BEq`), decidable equality (`DecidableEq`), and a default value for
/// fuel-exhausted branches (`Inhabited`).
pub const DERIVING_CLASSES: [&str; 3] = ["BEq", "DecidableEq", "Inhabited"];

/// The classes the stand-alone `deriving instance … for T` command may name:
/// the lawfulness of a derived `BEq`, which the model's law proofs rewrite
/// `==` to `=` with.
pub const DERIVING_INSTANCE_CLASSES: [&str; 2] = ["ReflBEq", "LawfulBEq"];

/// The first elaboration-executing token in code position, if any.
///
/// This is a fail-closed trust-boundary defense: the scanner's notion of "this
/// span is an inert string or comment" is a deliberate SOUND
/// OVER-APPROXIMATION of code — on any lexical ambiguity it defaults to code
/// and scans, so a token Lean would elaborate is never skipped as inert. It may
/// over-reject (treat inert bytes as code) but must never under-reject.
///
/// Inert spans recognized (and only these): normal string literals `"..."`
/// with `\` escapes, line comments `-- ... \n`, and nested block comments
/// `/- ... -/` (which also covers the `/--`/`/-!` doc-comment openers). Char
/// literals are consumed as code just far enough that a `"` inside `'"'` /
/// `'\"'` cannot open a phantom string. Raw / interpolated string prefixes
/// (`r"`, `r#"`, `s!"`) and unterminated strings/comments fall back to
/// scanning the remainder as pure code. A `deriving` token is admitted only as
/// the exact closed clause [`admitted_deriving_end`] recognizes.
pub fn code_exec_token(text: &str) -> Option<&'static str> {
    let chars: Vec<char> = text.chars().collect();
    find_code_exec_token(&chars)
}

/// Where scanning resumes after a token that opens an admitted clause, or
/// `None` when the token is refused.
fn admitted_clause_end(token: &str, chars: &[char], at: usize) -> Option<usize> {
    if token == "deriving" {
        admitted_deriving_end(chars, at)
    } else {
        None
    }
}

/// Recognize an admitted `deriving` clause starting at `chars[start]` and
/// return the index of the end of its line.
///
/// Exactly two shapes, each alone on the rest of its line:
///
/// * `deriving C, C, …` with every `C` in [`DERIVING_CLASSES`];
/// * `deriving instance C, C, … for T, T, …` with every `C` in
///   [`DERIVING_INSTANCE_CLASSES`] and every `T` a plain dotted identifier.
///
/// Lean's parser is whitespace-insensitive, so the line end alone does not end
/// the clause: a `,` on a later line would add a class, and `with` would pass
/// the handler an option term. The next significant token after the line
/// (comments skipped) is therefore required to be neither; an unterminated
/// comment there is refused outright.
pub fn admitted_deriving_end(chars: &[char], start: usize) -> Option<usize> {
    const KEYWORD: &str = "deriving";
    let mut at = start + KEYWORD.chars().count();
    let spaces = |at: &mut usize| {
        let from = *at;
        while chars.get(*at) == Some(&' ') {
            *at += 1;
        }
        *at > from
    };
    let word = |at: &mut usize| -> Option<String> {
        let from = *at;
        match chars.get(*at) {
            Some(first) if first.is_ascii_alphabetic() || *first == '_' => {}
            _ => return None,
        }
        while matches!(chars.get(*at), Some(c) if c.is_ascii_alphanumeric() || *c == '_' || *c == '.' || *c == '\'')
        {
            *at += 1;
        }
        Some(chars[from..*at].iter().collect())
    };
    // `C, C, …` (or `T, T, …`), each item passing `accept`.
    let list = |at: &mut usize, accept: &dyn Fn(&str) -> bool| -> Option<()> {
        loop {
            let item = word(at)?;
            if !accept(&item) {
                return None;
            }
            let resume = *at;
            spaces(at);
            if chars.get(*at) == Some(&',') {
                *at += 1;
                spaces(at);
                continue;
            }
            *at = resume;
            return Some(());
        }
    };
    if !spaces(&mut at) {
        return None;
    }
    let resume = at;
    if word(&mut at).as_deref() == Some("instance") && spaces(&mut at) {
        list(&mut at, &|class| DERIVING_INSTANCE_CLASSES.contains(&class))?;
        if !spaces(&mut at) || word(&mut at).as_deref() != Some("for") || !spaces(&mut at) {
            return None;
        }
        list(&mut at, &|name| {
            crate::bridge_statement::is_plain_dotted_name(name)
        })?;
    } else {
        at = resume;
        list(&mut at, &|class| DERIVING_CLASSES.contains(&class))?;
    }
    spaces(&mut at);
    let line_end = at;
    match chars.get(at) {
        None => return Some(line_end),
        Some('\n') => {}
        _ => return None,
    }
    // The clause must not continue past its line.
    let mut next = at;
    loop {
        match chars.get(next) {
            Some(c) if c.is_whitespace() => next += 1,
            Some('-') if chars.get(next + 1) == Some(&'-') => {
                while matches!(chars.get(next), Some(c) if *c != '\n') {
                    next += 1;
                }
            }
            Some('/') if chars.get(next + 1) == Some(&'-') => {
                next = block_comment_end(chars, next)?;
            }
            _ => break,
        }
    }
    if chars.get(next) == Some(&',') {
        return None;
    }
    let mut probe = next;
    if word(&mut probe).as_deref() == Some("with") {
        return None;
    }
    Some(line_end)
}

/// Validate a package file path and return its Lean module root. A flat
/// `Store.lean` yields `Store`; a nested `Apps/Notepad/Store.lean` yields the
/// dotted `Apps.Notepad.Store`. Every `/`-separated segment must match
/// `^[A-Za-z][A-Za-z0-9_]*$` (the last one before its `.lean` suffix). The rule
/// is simultaneously the traversal guard — an accepted segment cannot be `.`,
/// `..`, empty, absolute, or anything other than a plain
/// `std::path::Component::Normal` — and the lakefile-injection guard: the
/// checker interpolates the returned root unescaped into its lakefile, so only
/// validated segments may become roots.
pub fn lean_module_root(name: &str) -> Result<String, String> {
    let stem = name
        .strip_suffix(".lean")
        .ok_or_else(|| format!("cert file `{name}` is not a Lean file"))?;
    let segments: Vec<&str> = stem.split('/').collect();
    let valid = segments.iter().all(|segment| {
        let mut chars = segment.chars();
        matches!(chars.next(), Some(first) if first.is_ascii_alphabetic())
            && chars.all(|character| character.is_ascii_alphanumeric() || character == '_')
    });
    if valid {
        Ok(segments.join("."))
    } else {
        Err(format!(
            "cert file name `{name}` must match ^[A-Za-z][A-Za-z0-9_]*\\.lean$ in every path segment"
        ))
    }
}

/// A law-claim label or corollary: a plain dotted identifier with no primes
/// (the label is the source-level `module.fn.law` identity, and the corollary
/// its `_` flattening).
fn is_plain_unprimed_name(value: &str) -> bool {
    !value.contains('\'') && crate::bridge_statement::is_plain_dotted_name(value)
}

/// The identifier gate of one law-claim, shared by the producer and the
/// checker: `Err(field)` names the first field the checker refuses.
///
/// The model theorem may carry the transpiler's trailing-prime escape in any
/// segment (a law of a function named after a reserved word lives in
/// `…none'` scope); the label and the corollary never do, since the corollary
/// is the label's flattening and the label is a source identity.
pub fn law_claim_identifiers(
    label: &str,
    theorem: &str,
    corollary: &str,
) -> Result<(), &'static str> {
    if !is_plain_unprimed_name(label) {
        return Err("label");
    }
    if !crate::bridge_statement::is_plain_dotted_name(theorem) {
        return Err("theorem");
    }
    if !is_plain_unprimed_name(corollary) {
        return Err("corollary");
    }
    Ok(())
}

/// A Lean identifier-continuation character, narrowed to ASCII alphanumerics and
/// `_`. This is intentionally an UNDER-approximation of Lean's identifier
/// alphabet: it is used only for the word-boundary check, and treating fewer
/// characters as identifier-continuation makes the scanner *more* likely to
/// reject (fail-closed), never less.
fn is_ident_continuation(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// A forbidden token is treated as a whole *word* (boundary-checked so `elab`
/// does not fire inside `relabel`) exactly when every one of its bytes is an
/// ASCII identifier-continuation character. Tokens carrying punctuation, spaces,
/// or non-ASCII bytes (`#eval`, `@[`, `«`, `open Lean`) are matched as raw
/// substrings in code position, where a word boundary has no meaning.
fn token_is_word(token: &str) -> bool {
    token
        .bytes()
        .all(|b| b.is_ascii_alphanumeric() || b == b'_')
}

/// Returns the offending token if one starts, in code position, at `chars[i]`.
fn token_at(
    tokens: &[(&'static str, Vec<char>, bool)],
    chars: &[char],
    i: usize,
) -> Option<&'static str> {
    for (token, needle, is_word) in tokens {
        let len = needle.len();
        if i + len > chars.len() || &chars[i..i + len] != needle.as_slice() {
            continue;
        }
        if *is_word {
            let left_boundary = i == 0 || !is_ident_continuation(chars[i - 1]);
            let right_boundary = i + len == chars.len() || !is_ident_continuation(chars[i + len]);
            if left_boundary && right_boundary {
                return Some(token);
            }
        } else {
            return Some(token);
        }
    }
    None
}

/// Index just past the closing `"` of the normal string literal opening at
/// `chars[open]`, or `None` if the string never closes before EOF (an
/// unterminated string is a lexer error in Lean; the caller then defaults to
/// scanning the region as code).
fn string_literal_end(chars: &[char], open: usize) -> Option<usize> {
    let mut j = open + 1;
    while j < chars.len() {
        match chars[j] {
            '\\' => j += 2, // the escaped character cannot close the string
            '"' => return Some(j + 1),
            _ => j += 1,
        }
    }
    None
}

/// Index just past the matching `-/` of the (nesting) block comment opening at
/// `chars[open]` (`/-`), or `None` if it never closes before EOF.
fn block_comment_end(chars: &[char], open: usize) -> Option<usize> {
    let mut depth = 1usize;
    let mut j = open + 2;
    while j < chars.len() {
        if chars[j] == '/' && j + 1 < chars.len() && chars[j + 1] == '-' {
            depth += 1;
            j += 2;
        } else if chars[j] == '-' && j + 1 < chars.len() && chars[j + 1] == '/' {
            depth -= 1;
            j += 2;
            if depth == 0 {
                return Some(j);
            }
        } else {
            j += 1;
        }
    }
    None
}

/// Index just past a char literal opening at `chars[open]` (`'`), or `None` if
/// `chars[open]` is not the start of a char literal we recognize. Recognition is
/// deliberately minimal: its only soundness duty is to consume the `"` inside
/// `'"'` and `'\"'` so it cannot open a phantom string. Every char literal that
/// can contain a raw `"` byte matches one of those two shapes; other char
/// literals (`'\n'`, `'\u{22}'`, identifier primes) may go unrecognized, which
/// is harmless because they carry no `"`.
fn char_literal_end(chars: &[char], open: usize) -> Option<usize> {
    if chars.get(open + 1) == Some(&'\\') {
        // '\X'  (escaped single char, e.g. '\"', '\n', '\\', '\'')
        if chars.get(open + 2).is_some() && chars.get(open + 3) == Some(&'\'') {
            return Some(open + 4);
        }
        return None;
    }
    match chars.get(open + 1) {
        Some('\'') | None => None, // "''" is not a char literal; nor is a trailing '
        Some(_) => {
            // 'X'  (single unescaped char, including 'X' == '"')
            if chars.get(open + 2) == Some(&'\'') {
                Some(open + 3)
            } else {
                None
            }
        }
    }
}

/// Scan `chars[start..]` as pure code (no string/comment skipping) and return
/// the first forbidden token. Used as the default-to-code fallback for
/// unterminated strings/comments and raw/interpolated string prefixes.
fn scan_remainder_as_code(
    tokens: &[(&'static str, Vec<char>, bool)],
    chars: &[char],
    start: usize,
) -> Option<&'static str> {
    let mut i = start;
    while i < chars.len() {
        if let Some(token) = token_at(tokens, chars, i) {
            match admitted_clause_end(token, chars, i) {
                Some(end) => {
                    i = end;
                    continue;
                }
                None => return Some(token),
            }
        }
        i += 1;
    }
    None
}

/// The context-aware core of [`code_exec_token`]: a mini Lean lexer that
/// walks the file, skips inert string/comment spans, and reports the first
/// forbidden token that appears in code position.
fn find_code_exec_token(chars: &[char]) -> Option<&'static str> {
    let tokens: Vec<(&'static str, Vec<char>, bool)> = CODE_EXEC_TOKENS
        .iter()
        .map(|token| (*token, token.chars().collect(), token_is_word(token)))
        .collect();
    let n = chars.len();
    let mut i = 0;
    while i < n {
        let c = chars[i];
        // Inert-span openers take priority. None of them is a token start, so
        // handling them here never skips over a forbidden token.
        if c == '"' {
            // A `"` preceded by a raw/interpolated string prefix (`r"`, `r#"`,
            // `s!"`) is lexically ambiguous for a normal-string scan; default to
            // code and scan the remainder rather than risk a desynced skip.
            if i > 0 && matches!(chars[i - 1], 'r' | '#' | '!') {
                return scan_remainder_as_code(&tokens, chars, i);
            }
            match string_literal_end(chars, i) {
                Some(end) => {
                    i = end;
                    continue;
                }
                None => return scan_remainder_as_code(&tokens, chars, i),
            }
        }
        if c == '-' && chars.get(i + 1) == Some(&'-') {
            // Line comment through end of line (or EOF).
            let mut j = i + 2;
            while j < n && chars[j] != '\n' {
                j += 1;
            }
            i = j;
            continue;
        }
        if c == '/' && chars.get(i + 1) == Some(&'-') {
            match block_comment_end(chars, i) {
                Some(end) => {
                    i = end;
                    continue;
                }
                None => return scan_remainder_as_code(&tokens, chars, i),
            }
        }
        // A `'` that opens a char literal is consumed; otherwise it is an
        // identifier prime and falls through as ordinary code.
        if c == '\''
            && let Some(end) = char_literal_end(chars, i)
        {
            i = end;
            continue;
        }
        if let Some(token) = token_at(&tokens, chars, i) {
            match admitted_clause_end(token, chars, i) {
                Some(end) => {
                    i = end;
                    continue;
                }
                None => return Some(token),
            }
        }
        i += 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn refused(text: &str) -> Option<&'static str> {
        code_exec_token(text)
    }

    #[test]
    fn deriving_clauses_of_the_closed_classes_are_admitted() {
        let structure = "structure P where\n  a : Int\n  deriving BEq, DecidableEq\n\ndef x := 0\n";
        assert_eq!(refused(structure), None);
        assert_eq!(
            refused("inductive O where\n  | a\n  deriving BEq, Inhabited, DecidableEq\n"),
            None
        );
        assert_eq!(
            refused("deriving instance ReflBEq, LawfulBEq for Domain.Rational.Fraction\n"),
            None
        );
        assert_eq!(refused("deriving instance LawfulBEq for Op, Type'\n"), None);
        // At end of file, with no newline.
        assert_eq!(refused("  deriving BEq"), None);
    }

    #[test]
    fn every_other_deriving_shape_is_refused() {
        for text in [
            // A class outside the closed list.
            "  deriving Repr, BEq\n",
            "  deriving BEq, ToJson\n",
            "  deriving Lean.ToJson\n",
            // The instance classes are not type-clause classes, and back.
            "  deriving LawfulBEq\n",
            "deriving instance BEq for T\n",
            // Anything else on the line.
            "  deriving BEq -- note\n",
            "  deriving BEq; def x := 0\n",
            "  deriving BEq with {}\n",
            "deriving instance LawfulBEq for T with x\n",
            // A continuation on a later line (Lean's parser ignores the newline).
            "  deriving BEq\n  , Evil\n",
            "  deriving BEq\n-- c\n/- c -/ , Evil\n",
            "  deriving BEq\n  with { x := 1 }\n",
            "deriving instance LawfulBEq for T\n, U\n",
            "  deriving BEq\n/- never closed",
            // A trailing comma, an empty list, a bare keyword.
            "  deriving BEq,\n",
            "  deriving\n",
            "  deriving instance for T\n",
            "deriving instance LawfulBEq\n",
        ] {
            assert_eq!(refused(text), Some("deriving"), "{text:?}");
        }
        // The keyword stays refused in the raw-scan fallback too, and an admitted
        // clause there does not hide a later token.
        assert_eq!(
            refused("def x := r\"a\"\n  deriving Repr\n"),
            Some("deriving")
        );
        assert_eq!(
            refused("def x := r\"a\"\n  deriving BEq\n#eval 1\n"),
            Some("#eval")
        );
        // An admitted clause does not hide the tokens after it.
        assert_eq!(
            refused("  deriving BEq\n@[simp] theorem t : True := trivial\n"),
            Some("@[")
        );
    }

    #[test]
    fn module_roots_and_law_identifiers_follow_one_rule() {
        assert_eq!(
            lean_module_root("AverModel/Domain/Laws.lean").as_deref(),
            Ok("AverModel.Domain.Laws")
        );
        assert!(lean_module_root("Domain/Type'.lean").is_err());
        assert!(lean_module_root("../x.lean").is_err());
        assert_eq!(law_claim_identifiers("D.f.l", "D.f_law_l", "D_f_l"), Ok(()));
        assert_eq!(
            law_claim_identifiers("D.f.l", "D.none'.f_law_l", "D_f_l"),
            Ok(())
        );
        assert_eq!(
            law_claim_identifiers("D.f'.l", "D.f_law_l", "D_f'_l"),
            Err("label")
        );
        assert_eq!(
            law_claim_identifiers("D.f.l", "D.f law", "D_f_l"),
            Err("theorem")
        );
    }
}
