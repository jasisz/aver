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

/// Words a package `.lean` file may not carry in code position (checker
/// stage 7), matched against every `.`-separated component of every
/// identifier token, so `Foo.elab` is refused like `elab`.
///
/// Three groups. Commands that run code while a file elaborates or register
/// code that later elaboration runs (`run_cmd`, `initialize`, `macro`,
/// `elab`, `simproc`, …). Commands that change how LATER text parses or
/// resolves — the checker's witness re-elaborates package statements, so a
/// package that could add a notation, a mixfix operator, a binder predicate,
/// a syntax category, a unification hint, an exported alias or a scoped
/// declaration could change what a pinned statement means (`notation`,
/// `infix`, `prefix`, `binder_predicate`, `declare_syntax_cat`, `unif_hint`,
/// `export`, `scoped`, `attribute`). And the name prefix the checker reserves
/// for its own witness (`AverCertChecker`).
///
/// `deriving` is the one word with an admitted form: the closed clause
/// [`admitted_deriving_end`] accepts. `instance` is not refused here: the
/// model needs a few, and which ones a package may declare is decided on the
/// ELABORATED instance by the checker's out-of-process audit, where a name
/// alias or a class parent projection cannot disguise the class.
pub const REFUSED_WORDS: [&str; 37] = [
    "run_cmd",
    "run_elab",
    "run_meta",
    "run_tac",
    "initialize",
    "builtin_initialize",
    "macro",
    "macro_rules",
    "elab",
    "elab_rules",
    "syntax",
    "notation",
    "infix",
    "infixl",
    "infixr",
    "prefix",
    "postfix",
    "binder_predicate",
    "declare_syntax_cat",
    "unif_hint",
    "export",
    "scoped",
    "unsafe",
    "implemented_by",
    "extern",
    "attribute",
    "simproc",
    "dsimproc",
    "simproc_decl",
    "dsimproc_decl",
    "builtin_simproc",
    "builtin_dsimproc",
    "register_simp_attr",
    "register_option",
    "register_builtin_option",
    "deriving",
    "AverCertChecker",
];

/// The `#`-commands a package may carry. Every other one (`#eval`, `#exit`,
/// …) is refused.
pub const ADMITTED_HASH_COMMANDS: [&str; 3] = ["#guard_msgs", "#print", "#check"];

/// Options a package may set, exactly: the resource limits and elaboration
/// switches the producer writes. Any `linter.` option is admitted as well (a
/// linter only reports). Nothing under `debug.` — `debug.skipKernelTC` adds
/// declarations the kernel never checked — nor any other option is admitted.
pub const ADMITTED_OPTIONS: [&str; 7] = [
    "autoImplicit",
    "relaxedAutoImplicit",
    "maxHeartbeats",
    "maxRecDepth",
    "smartUnfolding",
    "synthInstance.maxSize",
    "synthInstance.maxHeartbeats",
];

/// Namespaces a package may not `open`: the metaprogramming API (the checker's
/// own audit is written against it) and the build system.
pub const REFUSED_OPEN_ROOTS: [&str; 2] = ["Lean", "Lake"];

/// Whether `set_option <name>` is admitted.
pub fn option_admitted(name: &str) -> bool {
    ADMITTED_OPTIONS.contains(&name)
        || name
            .strip_prefix("linter.")
            .is_some_and(|rest| !rest.is_empty())
}

/// The first refused construct in code position, if any, named by the word or
/// symbol that opens it (`set_option` for a refused option, `open Lean` for an
/// open of a refused namespace, `#command` for a refused `#`-command other
/// than `#eval`).
///
/// This is a fail-closed trust-boundary defense. The file is TOKENIZED — the
/// rules below look at identifier and symbol tokens, never at substrings — so
/// whitespace, line breaks and comments between the words of a construct
/// (`open  Lean`, `open /- -/ Lean`, `set_option\n debug.x`) change nothing.
/// The notion of "this span is an inert string or comment" is a deliberate
/// SOUND OVER-APPROXIMATION of code: on any lexical ambiguity it defaults to
/// code, so a token Lean would elaborate is never skipped as inert. It may
/// over-reject but must never under-reject.
///
/// Inert spans recognized (and only these): normal string literals `"..."`
/// with `\` escapes, line comments `-- ... \n`, and nested block comments
/// `/- ... -/` (which also covers the `/--`/`/-!` doc-comment openers). Char
/// literals are consumed just far enough that a `"` inside `'"'` / `'\"'`
/// cannot open a phantom string. The string parts of an `s!` interpolated
/// string are inert and its `{…}` terms are code (see
/// [`interpolated_string_end`]). Raw string prefixes (`r"`, `r#"`), other
/// interpolation prefixes (`m!"`), an interpolation the lexer cannot read
/// exactly, and unterminated strings/comments switch the rest of the file to
/// pure code: their contents are tokenized like everything else.
pub fn code_exec_token(text: &str) -> Option<&'static str> {
    let chars: Vec<char> = text.chars().collect();
    let tokens = tokenize(&chars);
    first_refused(&chars, &tokens)
}

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
/// The model theorem may carry the transpiler's prime escape of a Lean
/// keyword (a law of a function `at` is the theorem `at'_law_…`); the label
/// and the corollary never do, since the corollary is the label's flattening
/// and the label is a source identity.
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

/// One token of a package file in code position.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    /// An identifier, dotted segments included (`Foo.bar'`), with the index
    /// of its first character.
    Ident(String, usize),
    /// A `#`-command word (`#eval`), with its start.
    Hash(String, usize),
    /// Any other significant character or symbol (`@[` is one symbol), with
    /// its start.
    Symbol(String, usize),
}

impl Token {
    fn start(&self) -> usize {
        match self {
            Token::Ident(_, at) | Token::Hash(_, at) | Token::Symbol(_, at) => *at,
        }
    }
}

/// A character that may start an identifier segment: ASCII letters and `_`,
/// and every non-ASCII alphabetic character (Lean admits Greek and other
/// letter-like characters). Over-admitting here only makes more text an
/// identifier, which the rules then inspect.
fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || (!c.is_ascii() && c.is_alphabetic())
}

/// A character that may continue an identifier segment. Lean continues an
/// identifier with `'`, `!` and `?` too, so `prefix'` is one identifier and
/// never the refused word `prefix`.
fn is_ident_continue(c: char) -> bool {
    is_ident_start(c)
        || c.is_ascii_digit()
        || c == '\''
        || c == '!'
        || c == '?'
        || (!c.is_ascii() && c.is_alphanumeric())
}

/// Where the `s!` interpolated string whose `"` is at `chars[open]` ends, and
/// the ranges of its `{…}` terms, or `None` when it cannot be lexed exactly.
///
/// The string parts are inert: a `\` escapes the next character and `{` opens
/// a term. Each term runs to the `}` that closes it at brace depth zero,
/// skipping the normal strings, nested `s!` strings and char literals inside
/// it the way Lean's term parser does. A comment, a raw string or an
/// unterminated construct inside a term makes the whole string `None`, and
/// the caller then reads the rest of the file as code.
///
/// Only `s!` is recognized. It is a token of Lean's prelude, so `s!"` always
/// opens an interpolated string; `m!` and `f!` are tokens only when their
/// modules are imported, and without them `m!"…"` is an identifier and a
/// NORMAL string, whose quotes pair differently.
fn interpolated_string_end(chars: &[char], open: usize) -> Option<(usize, Vec<(usize, usize)>)> {
    let mut terms = Vec::new();
    let mut j = open + 1;
    loop {
        match *chars.get(j)? {
            '\\' => j += 2,
            '"' => return Some((j + 1, terms)),
            '{' => {
                let end = interpolation_term_end(chars, j + 1)?;
                terms.push((j + 1, end));
                j = end + 1;
            }
            _ => j += 1,
        }
    }
}

/// The index of the `}` closing the interpolation term that starts at
/// `start`; see [`interpolated_string_end`].
fn interpolation_term_end(chars: &[char], start: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut j = start;
    loop {
        let c = *chars.get(j)?;
        let previous = j.checked_sub(1).map(|p| chars[p]);
        match c {
            '{' => depth += 1,
            '}' if depth == 0 => return Some(j),
            '}' => depth -= 1,
            '"' if is_interpolation_prefix(chars, j) => {
                j = interpolated_string_end(chars, j)?.0;
                continue;
            }
            '"' if matches!(previous, Some('r' | '#' | '!')) => return None,
            '"' => {
                j = string_literal_end(chars, j)?;
                continue;
            }
            '\'' if !previous.is_some_and(is_ident_continue) => {
                if let Some(end) = char_literal_end(chars, j) {
                    j = end;
                    continue;
                }
            }
            '-' if chars.get(j + 1) == Some(&'-') => return None,
            '/' if chars.get(j + 1) == Some(&'-') => return None,
            _ => {}
        }
        j += 1;
    }
}

/// Whether the `"` at `chars[at]` opens an `s!` interpolated string: it is
/// preceded by exactly `s!`, and the `s` does not continue an identifier.
fn is_interpolation_prefix(chars: &[char], at: usize) -> bool {
    at >= 2
        && chars[at - 1] == '!'
        && chars[at - 2] == 's'
        && !(at >= 3 && is_ident_continue(chars[at - 3]))
}

/// Tokenize the code positions of a package file. Strings, comments and char
/// literals are skipped; everything else becomes a token. The terms of an
/// `s!` interpolated string are code and are tokenized; its string parts are
/// skipped. After a raw string prefix, an interpolation this lexer cannot
/// read exactly, or an unterminated string or comment, the rest of the file is
/// tokenized as pure code (no span is skipped any more).
fn tokenize(chars: &[char]) -> Vec<Token> {
    let mut tokens = Vec::new();
    tokenize_range(chars, 0, chars.len(), &mut tokens);
    tokens
}

/// [`tokenize`] over `chars[start..n]`, appending to `tokens`. Token positions
/// are indices into the whole of `chars`.
fn tokenize_range(chars: &[char], start: usize, n: usize, tokens: &mut Vec<Token>) {
    let mut pure_code = false;
    let mut i = start;
    while i < n {
        let c = chars[i];
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if !pure_code {
            if c == '"' {
                // An `s!` string: its string parts are inert, its terms code.
                // Any other `"` after a raw/interpolated prefix (`r"`, `r#"`,
                // `m!"`) is ambiguous for a normal-string scan: default to code.
                if is_interpolation_prefix(chars, i) {
                    match interpolated_string_end(chars, i) {
                        Some((end, terms)) if end <= n => {
                            for (term_start, term_end) in terms {
                                tokenize_range(chars, term_start, term_end, tokens);
                            }
                            i = end;
                            continue;
                        }
                        _ => pure_code = true,
                    }
                } else if i > 0 && matches!(chars[i - 1], 'r' | '#' | '!') {
                    pure_code = true;
                } else if let Some(end) = string_literal_end(chars, i) {
                    i = end;
                    continue;
                } else {
                    pure_code = true;
                }
            } else if c == '-' && chars.get(i + 1) == Some(&'-') {
                while i < n && chars[i] != '\n' {
                    i += 1;
                }
                continue;
            } else if c == '/' && chars.get(i + 1) == Some(&'-') {
                match block_comment_end(chars, i) {
                    Some(end) => {
                        i = end;
                        continue;
                    }
                    None => pure_code = true,
                }
            } else if c == '\''
                && !matches!(i.checked_sub(1).map(|p| chars[p]), Some(p) if is_ident_continue(p))
                && let Some(end) = char_literal_end(chars, i)
            {
                i = end;
                continue;
            }
        }
        if is_ident_start(c) {
            let start = i;
            loop {
                while i < n && is_ident_continue(chars[i]) {
                    i += 1;
                }
                if i + 1 < n && chars[i] == '.' && is_ident_start(chars[i + 1]) {
                    i += 1;
                    continue;
                }
                break;
            }
            tokens.push(Token::Ident(chars[start..i].iter().collect(), start));
            continue;
        }
        if c.is_ascii_digit() {
            while i < n && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            continue;
        }
        if c == '#' && chars.get(i + 1).is_some_and(|next| is_ident_start(*next)) {
            let start = i;
            i += 1;
            while i < n && is_ident_continue(chars[i]) {
                i += 1;
            }
            tokens.push(Token::Hash(chars[start..i].iter().collect(), start));
            continue;
        }
        if c == '@' && chars.get(i + 1) == Some(&'[') {
            tokens.push(Token::Symbol("@[".to_string(), i));
            i += 2;
            continue;
        }
        tokens.push(Token::Symbol(c.to_string(), i));
        i += 1;
    }
}

/// Command keywords that end the argument list of an `open`.
const COMMAND_WORDS: [&str; 26] = [
    "def",
    "theorem",
    "lemma",
    "abbrev",
    "instance",
    "structure",
    "inductive",
    "class",
    "namespace",
    "section",
    "end",
    "open",
    "set_option",
    "variable",
    "universe",
    "noncomputable",
    "private",
    "protected",
    "partial",
    "mutual",
    "example",
    "opaque",
    "axiom",
    "import",
    "in",
    "where",
];

/// Apply the rules to the token stream; the first refused construct wins.
fn first_refused(chars: &[char], tokens: &[Token]) -> Option<&'static str> {
    let mut at = 0;
    while at < tokens.len() {
        match &tokens[at] {
            Token::Symbol(symbol, _) => {
                if symbol == "@[" {
                    return Some("@[");
                }
                if symbol == "«" || symbol == "»" {
                    return Some("«");
                }
            }
            Token::Hash(command, _) => {
                if command == "#eval" {
                    return Some("#eval");
                }
                if !ADMITTED_HASH_COMMANDS.contains(&command.as_str()) {
                    return Some("#command");
                }
            }
            Token::Ident(name, start) => {
                if name == "deriving" {
                    // The admitted clause resumes past its own line: skip every
                    // token it covers.
                    let Some(end) = admitted_deriving_end(chars, *start) else {
                        return Some("deriving");
                    };
                    at += 1;
                    while tokens.get(at).is_some_and(|token| token.start() < end) {
                        at += 1;
                    }
                    continue;
                }
                if let Some(word) = name
                    .split('.')
                    .find_map(|segment| REFUSED_WORDS.iter().find(|word| **word == segment))
                {
                    return Some(word);
                }
                if name == "set_option" {
                    match tokens.get(at + 1) {
                        Some(Token::Ident(option, _)) if option_admitted(option) => {}
                        _ => return Some("set_option"),
                    }
                }
                if name == "open" {
                    let mut next = at + 1;
                    while let Some(token) = tokens.get(next) {
                        match token {
                            Token::Ident(opened, _) => {
                                if COMMAND_WORDS.contains(&opened.as_str()) {
                                    break;
                                }
                                // `open _root_.Lean` opens `Lean` too.
                                let opened =
                                    opened.strip_prefix("_root_.").unwrap_or(opened.as_str());
                                let root = opened.split('.').next().unwrap_or_default();
                                if REFUSED_OPEN_ROOTS.contains(&root) {
                                    return Some("open Lean");
                                }
                            }
                            Token::Symbol(symbol, _)
                                if matches!(symbol.as_str(), "(" | ")" | ",") => {}
                            _ => break,
                        }
                        next += 1;
                    }
                }
            }
        }
        at += 1;
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

    /// The gate reads tokens, so spacing, line breaks and comments between the
    /// words of a construct do not hide it.
    #[test]
    fn refused_constructs_are_found_whatever_the_spacing() {
        for text in [
            "open Lean\n",
            "open  Lean in\n",
            "open\tLean.Elab\n",
            "open /- c -/ Lean\n",
            "open Foo\n  Lean\n",
            "open Foo (bar) Lake\n",
            "open _root_.Lean\n",
            "open _root_.Lean.Elab in\n",
            "open Foo _root_.Lake\n",
        ] {
            assert_eq!(refused(text), Some("open Lean"), "{text:?}");
        }
        for text in [
            "set_option debug.skipKernelTC true\n",
            "set_option\n  debug.skipKernelTC true in\ntheorem t : True := trivial\n",
            "theorem t : True := by\n  set_option /- x -/ debug.skipKernelTC true in\n  trivial\n",
            "set_option pp.all true\n",
            "set_option trace.Meta.synthInstance true\n",
            "set_option\n",
        ] {
            assert_eq!(refused(text), Some("set_option"), "{text:?}");
        }
        for (text, word) in [
            ("infixl:65 \" +' \" => f\n", "infixl"),
            ("local infix:50 \" ≤ \" => fun _ _ => False\n", "infix"),
            ("prefix:max \"√\" => f\n", "prefix"),
            ("postfix:max \"!\" => f\n", "postfix"),
            ("scoped notation \"x\" => 1\n", "scoped"),
            (
                "binder_predicate x \" > \" y:term => `($x > $y)\n",
                "binder_predicate",
            ),
            ("export Foo (bar)\n", "export"),
            ("declare_syntax_cat foo\n", "declare_syntax_cat"),
            ("scoped instance : LE Nat := ⟨fun _ _ => False⟩\n", "scoped"),
            (
                "unif_hint (n : Nat) where n =?= 0 ⊢ n + 1 =?= 1\n",
                "unif_hint",
            ),
            ("attribute [instance] foo\n", "attribute"),
            ("simproc foo (x) := fun e => pure .continue\n", "simproc"),
            ("namespace AverCertChecker.AverCert\n", "AverCertChecker"),
            (
                "def _root_.AverCertChecker.checked := 0\n",
                "AverCertChecker",
            ),
            ("#exit\n", "#command"),
            ("#eval 1\n", "#eval"),
            (
                "theorem t : True := by\n  native_decide\n@ [simp] def x := 0\n@[simp] def y := 0\n",
                "@[",
            ),
        ] {
            assert_eq!(refused(text), Some(word), "{text:?}");
        }
    }

    /// What the producer writes passes: the admitted options, `#guard_msgs`,
    /// `#print axioms`, opens of model namespaces, instances (their class is
    /// judged on the elaborated declaration by the checker's audit), primed
    /// identifiers spelling a refused word, and refused words inside strings
    /// and comments.
    #[test]
    fn producer_text_is_admitted() {
        for text in [
            "set_option maxHeartbeats 4000000\nset_option linter.unusedSimpArgs false\n",
            "theorem t : True := by\n  first\n  | (set_option maxHeartbeats 1000000 in\n      trivial)\n",
            "set_option smartUnfolding false in\ndef f (x : Int) : Int := x\n",
            "set_option synthInstance.maxSize 256\nset_option autoImplicit false\n",
            "#guard_msgs (drop error) in\ntheorem t : True := trivial\n#print axioms t\n",
            "open AverCert AverCert.Schema\nopen Classical in\ntheorem t : True := trivial\n",
            "instance : Inhabited Op := ⟨Op.zero⟩\ninstance : HAdd String String String := ⟨String.append⟩\n",
            "def prefix' (s : String) : String := s\ndef infix' := 0\n",
            "def s := \"infix prefix open Lean set_option debug.x\"\n-- open Lean\n/- #eval -/\n",
            "structure P where\n  prefixLen : Nat\n  deriving BEq\n",
        ] {
            assert_eq!(refused(text), None, "{text:?}");
        }
    }

    /// An `s!` string's text is inert and its terms are code, so a refused
    /// word after one (in a comment or a later string) no longer turns the
    /// whole rest of the file into code, while a refused word inside a term is
    /// still found.
    #[test]
    fn interpolated_strings_are_lexed_exactly() {
        for text in [
            "def a (k : String) : String := s!\"case-{k}\"\n/-- payment-scoped filter -/\ndef b := 0\n",
            "def a := s!\"[{x}] {y.z} \\{literal} {\"in\" ++ s!\"{w}\"}\"\n-- scoped\n",
            "def a := s!\"{ { f := 1 }.f }\" ++ \"scoped\"\n",
            "def a := s!\"{'}'}\"\n-- export\n",
        ] {
            assert_eq!(refused(text), None, "{text:?}");
        }
        for (text, word) in [
            // A refused word inside a term is code.
            ("def a := s!\"{scoped}\"\n", "scoped"),
            ("def a := s!\"x {#eval 1} y\"\n", "#eval"),
            // A comment or a raw string inside a term: the rest is code.
            ("def a := s!\"{x -- }\"\n}\"\n-- export\n", "export"),
            ("def a := s!\"{r\"}\"}\"\n-- export\n", "export"),
            // Unterminated.
            ("def a := s!\"{x\n-- export\n", "export"),
            ("def a := s!\"abc\n-- export\n", "export"),
            // Only `s!` opens an interpolated string. `m!"` is an identifier
            // and a normal string without its module, whose quotes pair
            // differently, so it stays code.
            ("def a := m!\"{\"}\" scoped \"}\"\n", "scoped"),
            ("def a := xs!\"{\"}\" scoped \"}\"\n", "scoped"),
        ] {
            assert_eq!(refused(text), Some(word), "{text:?}");
        }
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
            law_claim_identifiers("D.at.l", "D.at'_law_l", "D_at_l"),
            Ok(())
        );
        assert_eq!(
            law_claim_identifiers("D.f.l", "D.'f_law_l", "D_f_l"),
            Err("theorem")
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
