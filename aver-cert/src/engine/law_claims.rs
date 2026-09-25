// Included from engine/mod.rs (engine feature) — see the include! list there.

/// One law-claim of a certificate package: a universal law theorem of the
/// emitted model modules that the certificate's `Laws.lean` corollary cites,
/// keyed by the stable source-level `module.fn.law` label.
///
/// The producer HANDS these over as structure. The emitter that built the law
/// theorem's statement records the claim at the point it wrote the theorem
/// (`ProjectOutput::law_claims` on the compiler side), so this crate never
/// reads the emitted Lean text to recover what was stated. That also keeps the
/// `-- aver:law-class` marker private to the compiler: nothing here parses it,
/// and `aver-cert verify` reads the manifest's `laws` array, not the model
/// files.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawClaim {
    /// Stable source identity (`Domain.Rational.plus.commutative`).
    pub label: String,
    /// Dotted namespace the theorem was emitted under (`Domain.Rational`).
    pub prefix: String,
    /// Bare theorem name inside that namespace (`plus_law_commutative`).
    pub theorem: String,
    /// The theorem's universal statement, on one line — exactly the text the
    /// emitter wrote between `theorem <name> : ` and ` := by`.
    pub statement: String,
}

impl LawClaim {
    /// Fully qualified Lean name of the model theorem.
    pub fn qualified(&self) -> String {
        if self.prefix.is_empty() {
            self.theorem.clone()
        } else {
            format!("{}.{}", self.prefix, self.theorem)
        }
    }

    /// Name of this claim's corollary theorem inside `AverCert.Laws`: the
    /// label with dots flattened to underscores, the same flattening the
    /// compiler applies to export names.
    pub fn corollary(&self) -> String {
        self.label.replace('.', "_")
    }

    /// Name of this claim's BRIDGED corollary, emitted beside the plain one
    /// when every model function the statement mentions carries a bridge.
    ///
    /// The two are separate declarations on purpose. The plain corollary says
    /// the law holds of the source model and the bytes simulate the plan; the
    /// bridged one additionally says the plan IS that source model. A bridge
    /// whose script falls to `sorry` therefore costs the bridge and this
    /// corollary, and leaves the plain law's credit exactly where it was.
    pub fn bridged_corollary(&self) -> String {
        format!("{}{LAW_BRIDGED_COROLLARY_SUFFIX}", self.corollary())
    }
}

/// The suffix the bridged corollary carries over the plain one. Checker-owned
/// in the sense that matters: the manifest never declares this name, both sides
/// derive it from the claim's label.
pub const LAW_BRIDGED_COROLLARY_SUFFIX: &str = "_bridged";

/// The checker's `validate_law_candidate` gates, applied through the SAME
/// functions the checker calls (`lean_gate::law_claim_identifiers` and
/// `bridge_statement::statement_is_single_plain_line` at the checker's own
/// length cap): the producer must never write a manifest entry its checker
/// hard-rejects — one refused entry fails candidate parsing for the WHOLE
/// package before Lean even runs. Legitimate compiler output can trip the
/// gates (a record literal `{ field := value }` in a statement carries `:=`),
/// so such a law is simply not claimed — the surface is additive and omitting
/// a claim is fail-closed.
fn claim_survives_checker_gates(claim: &LawClaim) -> bool {
    crate::lean_gate::law_claim_identifiers(&claim.label, &claim.qualified(), &claim.corollary())
        .is_ok()
        && crate::bridge_statement::statement_is_single_plain_line(
            &claim.statement,
            crate::bridge_statement::MAX_BRIDGE_STATEMENT_LEN,
        )
}

/// Admit the law-claims the producer handed over.
///
/// The compiler's Lean emitter records one claim per exported universal law at
/// the point it writes that law's theorem, so claims arrive as STRUCTURE and
/// nothing here reads the emitted `.lean` text. All this function does is
/// apply the defensive gates above: a claim whose rendered statement or whose
/// identifiers the checker would refuse is dropped, because a single refused
/// entry fails candidate parsing for the whole package before Lean even runs.
/// Each dropped claim comes back as `(label, reason)` so the caller can say
/// what it declined instead of losing it silently.
pub fn admit_law_claims(claims: Vec<LawClaim>) -> (Vec<LawClaim>, Vec<(String, String)>) {
    let mut admitted = Vec::with_capacity(claims.len());
    let mut declined = Vec::new();
    for claim in claims {
        if claim_survives_checker_gates(&claim) {
            admitted.push(claim);
        } else {
            declined.push((
                claim.label.clone(),
                "statement or identifiers would be refused by the checker's law gates".to_string(),
            ));
        }
    }
    (admitted, declined)
}

/// Render the package's `Laws.lean`: one corollary per claim, conjoining the
/// law's universal statement with the artifact-level `Holds` fact by citing
/// the model theorem and `AverCert.Final.cert`. One kernel-checked name per
/// claim ties the law to exactly the certified bytes.
///
/// Every corollary is declared at the root namespace, the context the
/// checker's witness reads the statement in. The statement arrives
/// root-qualified ([`root_qualify_statement`]): every model name in it is
/// spelled `_root_.<name>`, so it means at the root what the emitter's text
/// meant inside the model theorem's namespace.
///
/// Everything the certificate owns is spelled `_root_.`-qualified, so a model
/// module that declares an `AverCert` sub-namespace cannot shadow the fact
/// being conjoined or the proof term citing it.
///
/// `bridge_statements` carries, per claim, the plan-equals-source bridge
/// statements of every model function that claim's statement mentions — empty
/// when some mentioned function has no bridge.
///
/// A claim whose functions are all bridged gets a SECOND corollary,
/// `AverCert.Laws.<c>_bridged`, which conjoins those bridge statements after
/// `Holds`: one kernel-checked name saying this law holds of the source
/// function, the bytes simulate the plan, AND the plan IS that source function.
/// The two are deliberately separate declarations rather than one wider
/// corollary. A bridge whose fixed tactic script falls to `sorry` taints
/// everything that cites it, so folding the bridges into `Laws.<c>` made one
/// unfinished bridge remove the credit of every law that merely mentions the
/// function — a claim about the SOURCE model, which the bridge has no part in
/// proving. Split, a `sorry` in a bridge costs the bridge and the bridged
/// corollary, and the plain law keeps its credit.
pub fn render_laws_lean(
    claims: &[LawClaim],
    bridge_statements: &[Vec<(String, String)>],
    model_roots: &[String],
) -> String {
    let any_bridged = bridge_statements.iter().any(|entry| !entry.is_empty());
    let mut s = String::new();
    s.push_str(
        "-- Law-claims of this certificate. Each corollary conjoins one universal\n\
         -- law of the model modules with the artifact-level `Holds` fact, so a\n\
         -- single kernel-checked name ties the law to exactly the certified bytes.\n\
         -- A law whose every mentioned function carries a plan-equals-source\n\
         -- bridge gets a second `_bridged` corollary conjoining those bridges,\n\
         -- kept apart from the law's own so an unfinished bridge cannot cost\n\
         -- the law its credit.\n\
         -- Every statement is read at the root and names each model constant\n\
         -- `_root_.`-qualified, as the checker reads it.\n\
         import Manifest\n\
         import Final\n",
    );
    for root in model_roots {
        s.push_str("import ");
        s.push_str(root);
        s.push('\n');
    }
    if any_bridged {
        s.push_str("import Bridge\n");
    }
    s.push_str("\nset_option autoImplicit false\n\n");
    for (index, claim) in claims.iter().enumerate() {
        let bridges: &[(String, String)] = bridge_statements
            .get(index)
            .map(Vec::as_slice)
            .unwrap_or_default();
        // Concatenated, never interpolated into a format string: a statement
        // carrying `{`/`}` must stay inert text.
        s.push_str("/-- law-claim `");
        s.push_str(&claim.label);
        s.push_str("` -/\ntheorem _root_.AverCert.Laws.");
        s.push_str(&claim.corollary());
        s.push_str(" :\n    (");
        s.push_str(&claim.statement);
        s.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  ⟨_root_.");
        s.push_str(&claim.qualified());
        s.push_str(", _root_.AverCert.Final.cert⟩\n\n");
        if !bridges.is_empty() {
            s.push_str("/-- law-claim `");
            s.push_str(&claim.label);
            s.push_str("`, with the plan-equals-source identity of every model\n    \
                        function it mentions. Separate from the corollary above so an\n    \
                        unfinished bridge costs this claim and not the law itself. -/\n\
                        theorem _root_.AverCert.Laws.");
            s.push_str(&claim.bridged_corollary());
            s.push_str(" :\n    (");
            s.push_str(&claim.statement);
            s.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest)");
            for (_, statement) in bridges {
                s.push_str(" ∧\n      (");
                s.push_str(statement);
                s.push(')');
            }
            s.push_str(" :=\n  ⟨_root_.");
            s.push_str(&claim.qualified());
            s.push_str(", _root_.AverCert.Final.cert");
            for (corollary, _) in bridges {
                s.push_str(",\n    (_root_.");
                s.push_str(corollary);
                s.push_str(").1");
            }
            s.push_str("⟩\n\n");
        }
    }
    s
}

// ---- root-qualified law statements --------------------------------------------

/// Every public name the model modules declare, fully qualified, with whether
/// it is `protected`: definitions, theorems, structures with their fields and
/// constructor, inductive types with their constructors, classes and named
/// instances. Private names are left out; no other module can name them.
///
/// This is what [`root_qualify_statement`] resolves a law's names against. A
/// name it misses leaves the statement meaning something else at the root, or
/// nothing; either way the law's corollary does not check against its model
/// theorem and only that law loses its credit.
#[derive(Debug, Default)]
pub struct ModelNames {
    names: std::collections::BTreeMap<String, bool>,
}

impl ModelNames {
    /// Collect the names of every `.lean` file of the model.
    pub fn from_files<'a>(files: impl IntoIterator<Item = (&'a str, &'a str)>) -> Self {
        let mut names = Self::default();
        for (path, content) in files {
            if path.ends_with(".lean") {
                names.collect(content);
            }
        }
        names
    }

    fn insert(&mut self, qualified: String, protected: bool) {
        self.names.entry(qualified).or_insert(protected);
    }

    fn collect(&mut self, content: &str) {
        // A `namespace` contributes to the current namespace; a `section` or
        // `mutual` block only has to be closed by its `end`.
        let mut scopes: Vec<(bool, String)> = Vec::new();
        let lines: Vec<&str> = content.lines().collect();
        let mut comment_depth = 0usize;
        let mut at = 0;
        while at < lines.len() {
            let line = lines[at];
            let in_comment = comment_depth > 0;
            comment_depth = block_comment_depth_after(line, comment_depth);
            at += 1;
            if in_comment {
                continue;
            }
            let trimmed = line.split("--").next().unwrap_or_default().trim();
            let mut words = trimmed.split_whitespace();
            let Some(first) = words.next() else { continue };
            match first {
                "namespace" => {
                    if let Some(name) = words.next() {
                        scopes.push((true, name.to_string()));
                    }
                    continue;
                }
                "section" | "mutual" => {
                    scopes.push((false, words.next().unwrap_or_default().to_string()));
                    continue;
                }
                "end" => {
                    let name = words.next().unwrap_or_default();
                    if scopes.last().is_some_and(|(_, open)| open == name) {
                        scopes.pop();
                    }
                    continue;
                }
                _ => {}
            }
            let Some((keyword, name, private, protected)) = declaration_head(trimmed) else {
                continue;
            };
            if private {
                continue;
            }
            let namespace = scopes
                .iter()
                .filter(|(is_namespace, _)| *is_namespace)
                .map(|(_, name)| name.as_str())
                .collect::<Vec<_>>()
                .join(".");
            let qualified = match name.strip_prefix(crate::bridge_statement::ROOT_PREFIX) {
                Some(absolute) => absolute.to_string(),
                None if namespace.is_empty() => name.to_string(),
                None => format!("{namespace}.{name}"),
            };
            self.insert(qualified.clone(), protected);
            match keyword {
                "structure" | "class" => {
                    let mut constructor = "mk".to_string();
                    while let Some(member) = lines.get(at) {
                        if !member.starts_with("  ") || member.trim().is_empty() {
                            break;
                        }
                        at += 1;
                        let member = member.trim();
                        if let Some(ctor) = member.strip_suffix("::") {
                            constructor = ctor.trim().to_string();
                            continue;
                        }
                        if let Some((fields, _)) = member.split_once(':')
                            && !fields.contains('(')
                        {
                            for field in fields.split_whitespace() {
                                if is_declared_name(field) {
                                    self.insert(format!("{qualified}.{field}"), false);
                                }
                            }
                        }
                    }
                    self.insert(format!("{qualified}.{constructor}"), false);
                }
                "inductive" => {
                    let mut ctor_lines: Vec<&str> = trimmed
                        .split_once(" where")
                        .map(|(_, rest)| vec![rest])
                        .unwrap_or_default();
                    while let Some(member) = lines.get(at) {
                        if !member.trim_start().starts_with('|') {
                            break;
                        }
                        at += 1;
                        ctor_lines.push(member);
                    }
                    for ctor_line in ctor_lines {
                        for alternative in ctor_line.split('|').skip(1) {
                            if let Some(ctor) = alternative.split_whitespace().next()
                                && is_declared_name(ctor)
                            {
                                self.insert(format!("{qualified}.{ctor}"), false);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Resolve `written` the way Lean does inside `namespace`, among the
    /// model's names: the innermost enclosing namespace first, then the root.
    /// An atomic name does not reach a `protected` declaration through a
    /// namespace.
    fn resolve(&self, namespace: &str, written: &str) -> Option<String> {
        let mut scope: Vec<&str> = namespace.split('.').filter(|s| !s.is_empty()).collect();
        loop {
            let candidate = if scope.is_empty() {
                written.to_string()
            } else {
                format!("{}.{written}", scope.join("."))
            };
            if let Some(protected) = self.names.get(&candidate)
                && !(*protected && !scope.is_empty() && !written.contains('.'))
            {
                return Some(candidate);
            }
            scope.pop()?;
        }
    }
}

/// The nesting depth of `/- … -/` block comments after `line`, given the
/// depth before it. String literals and `--` line comments are skipped.
fn block_comment_depth_after(line: &str, mut depth: usize) -> usize {
    let chars: Vec<char> = line.chars().collect();
    let mut at = 0;
    while at < chars.len() {
        let next = chars.get(at + 1).copied();
        if depth == 0 && chars[at] == '"' {
            at += 1;
            while at < chars.len() && chars[at] != '"' {
                at += if chars[at] == '\\' { 2 } else { 1 };
            }
        } else if depth == 0 && chars[at] == '-' && next == Some('-') {
            break;
        } else if chars[at] == '/' && next == Some('-') {
            depth += 1;
            at += 1;
        } else if depth > 0 && chars[at] == '-' && next == Some('/') {
            depth -= 1;
            at += 1;
        }
        at += 1;
    }
    depth
}

/// A declaration line's keyword and declared name, and whether it is
/// `private` or `protected`: `@[…]` attributes and the modifiers are skipped.
/// An instance without a name declares nothing a statement spells.
fn declaration_head(line: &str) -> Option<(&str, &str, bool, bool)> {
    let mut rest = line;
    while let Some(attributed) = rest.strip_prefix("@[") {
        rest = attributed.split_once(']')?.1.trim_start();
    }
    let (mut private, mut protected) = (false, false);
    loop {
        let (word, tail) = rest.split_once(char::is_whitespace)?;
        let tail = tail.trim_start();
        match word {
            "private" => private = true,
            "protected" => protected = true,
            "noncomputable" | "partial" | "unsafe" | "nonrec" => {}
            "def" | "theorem" | "lemma" | "abbrev" | "opaque" | "axiom" | "instance"
            | "structure" | "class" | "inductive" => {
                let (keyword, tail) = match tail.strip_prefix("inductive ") {
                    Some(after) if word == "class" => ("inductive", after.trim_start()),
                    _ => (word, tail),
                };
                let end = tail
                    .find(|c: char| c.is_whitespace() || "({[⦃:".contains(c))
                    .unwrap_or(tail.len());
                let name = &tail[..end];
                return is_declared_name(name).then_some((keyword, name, private, protected));
            }
            _ => return None,
        }
        rest = tail;
    }
}

/// A plain dotted identifier as a declaration writes it.
fn is_declared_name(name: &str) -> bool {
    !name.is_empty()
        && name.split('.').all(|segment| {
            segment
                .chars()
                .next()
                .is_some_and(|c| c.is_alphabetic() || c == '_')
                && segment
                    .chars()
                    .all(|c| c.is_alphanumeric() || matches!(c, '_' | '\'' | '!' | '?'))
        })
}

/// Identifiers that are keywords of the term language, never names.
const STATEMENT_KEYWORDS: [&str; 20] = [
    "fun", "λ", "if", "then", "else", "match", "with", "let", "have", "show", "from", "by", "do",
    "at", "in", "Type", "Prop", "Sort", "forall", "exists",
];

/// One lexical item of a statement: an identifier with its byte offset, or
/// any other character.
enum StatementItem<'a> {
    Ident(&'a str, usize),
    Other(char),
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric()
        || matches!(c, '_' | '\'' | '.' | '!' | '?')
        || (!c.is_ascii() && c.is_alphanumeric() && !matches!(c, 'λ' | 'Π' | 'Σ'))
}

/// The items of a statement, with string and character literals skipped.
fn statement_items(statement: &str) -> Vec<StatementItem<'_>> {
    let mut items = Vec::new();
    let mut chars = statement.char_indices().peekable();
    while let Some((at, c)) = chars.next() {
        if c == '"' || c == '\'' {
            // A string or character literal: an identifier never starts
            // with `'`, so a leading one opens a character.
            let mut escaped = false;
            for (_, inner) in chars.by_ref() {
                if escaped {
                    escaped = false;
                } else if inner == '\\' {
                    escaped = true;
                } else if inner == c {
                    break;
                }
            }
            items.push(StatementItem::Other(c));
        } else if is_ident_char(c) {
            let mut end = at + c.len_utf8();
            while let Some(&(next_at, next)) = chars.peek() {
                if !is_ident_char(next) {
                    break;
                }
                end = next_at + next.len_utf8();
                chars.next();
            }
            items.push(StatementItem::Ident(&statement[at..end], at));
        } else {
            items.push(StatementItem::Other(c));
        }
    }
    items
}

/// The names a statement binds: in the binder list of every `∀`, `∃`, `fun`
/// or `λ`, each undotted identifier before the `:` of its group (or of the
/// whole list), up to the `,` or `=>` that ends the list.
fn statement_binders<'a>(items: &[StatementItem<'a>]) -> Vec<&'a str> {
    let mut binders = Vec::new();
    let mut at = 0;
    while at < items.len() {
        let opens = match &items[at] {
            StatementItem::Other('∀' | '∃' | 'λ' | 'Π' | 'Σ') => true,
            StatementItem::Ident(word, _) => matches!(*word, "fun" | "forall" | "exists"),
            _ => false,
        };
        at += 1;
        if !opens {
            continue;
        }
        let mut depth = 0usize;
        let mut typed = false;
        while at < items.len() {
            match &items[at] {
                StatementItem::Other('(' | '{' | '[' | '⦃') => {
                    depth += 1;
                    typed = false;
                }
                StatementItem::Other(')' | '}' | ']' | '⦄') => {
                    depth = depth.saturating_sub(1);
                    typed = false;
                }
                StatementItem::Other(':') => typed = true,
                StatementItem::Other(',' | '↦') if depth == 0 => break,
                StatementItem::Other('=')
                    if depth == 0 && matches!(items.get(at + 1), Some(StatementItem::Other('>'))) =>
                {
                    break;
                }
                StatementItem::Ident(name, _) if !typed && !name.contains('.') => {
                    binders.push(*name);
                }
                _ => {}
            }
            at += 1;
        }
    }
    binders
}

/// Rewrite a law statement the emitter wrote for `namespace` so it means the
/// same at the root: every name that resolves, inside `namespace`, to a name
/// the model declares is replaced by `_root_.<that name>`, with the rest of a
/// dotted name kept as field accesses. Binders, keywords, numerals,
/// projections (`.length`) and names the model does not declare (core names
/// such as `Int` or `Option.some`) are left as written.
///
/// The checker reads every law statement at the root, never inside the
/// namespace the package names, since a package constant there could capture
/// a name. This rewrite is how the producer keeps each statement's meaning
/// across that change.
pub fn root_qualify_statement(statement: &str, namespace: &str, names: &ModelNames) -> String {
    let items = statement_items(statement);
    let binders = statement_binders(&items);
    let mut out = String::with_capacity(statement.len() + 64);
    let mut copied = 0;
    for item in &items {
        let StatementItem::Ident(token, at) = item else {
            continue;
        };
        if token.starts_with('.')
            || token.starts_with(crate::bridge_statement::ROOT_PREFIX)
            || token.starts_with(|c: char| c.is_ascii_digit())
            || STATEMENT_KEYWORDS.contains(token)
        {
            continue;
        }
        let written = token.trim_end_matches('.');
        let segments: Vec<&str> = written.split('.').collect();
        if segments.iter().any(|segment| segment.is_empty()) || binders.contains(&segments[0]) {
            continue;
        }
        let resolved = (1..=segments.len()).rev().find_map(|length| {
            names
                .resolve(namespace, &segments[..length].join("."))
                .map(|found| (found, &segments[length..]))
        });
        if let Some((found, fields)) = resolved {
            out.push_str(&statement[copied..*at]);
            out.push_str(crate::bridge_statement::ROOT_PREFIX);
            out.push_str(&found);
            for field in fields {
                out.push('.');
                out.push_str(field);
            }
            copied = at + written.len();
        }
    }
    out.push_str(&statement[copied..]);
    out
}

/// Every claim with its statement rewritten by [`root_qualify_statement`] for
/// the namespace its theorem was emitted in.
pub fn root_qualify_law_claims(claims: &[LawClaim], names: &ModelNames) -> Vec<LawClaim> {
    claims
        .iter()
        .map(|claim| LawClaim {
            statement: root_qualify_statement(&claim.statement, &claim.prefix, names),
            ..claim.clone()
        })
        .collect()
}

#[cfg(test)]
mod root_qualify_tests {
    use super::*;

    const JSON: &str = "import AverCommon\n\nopen Bytes\n\nnamespace Json\n\n\
        inductive Json where\n  | jsonNull\n  | jsonString (_ : String)\n\n\
        inductive ParseResult where\n  | ok (_ : Json) (_ : Int)\n  | err (_ : String)\n\n\
        /-- doc\ndef notADecl : Int := 0\n-/\n\
        def escape (s : String) : String :=\n  s\n\n\
        private def hidden (s : String) : String :=\n  s\n\n\
        mutual\n  def parse (s : String) (n : Int) : ParseResult :=\n    ParseResult.err s\nend\n\n\
        theorem escape_law_id : ∀ (s : String), escape s = s := by\n  rfl\n\nend Json\n";

    const COMMON: &str = "namespace Except\n\nprotected def map (x : Int) : Int :=\n  x\n\nend Except\n\n\
        structure BranchPath where\n  dewey : String\n  deriving BEq\n\n\
        def toString' (n : Int) : String :=\n  \"\"\n";

    fn names() -> ModelNames {
        ModelNames::from_files([("AverModel/Json.lean", JSON), ("AverModel/AverCommon.lean", COMMON)])
    }

    #[test]
    fn model_names_are_collected_with_their_namespace() {
        let names = names();
        for name in [
            "Json.Json",
            "Json.Json.jsonString",
            "Json.ParseResult.ok",
            "Json.escape",
            "Json.parse",
            "Json.escape_law_id",
            "Except.map",
            "BranchPath.dewey",
            "BranchPath.mk",
            "toString'",
        ] {
            assert!(names.names.contains_key(name), "{name}: {:?}", names.names);
        }
        assert!(!names.names.contains_key("Json.hidden"));
        assert!(!names.names.contains_key("Json.notADecl"));
    }

    /// The rewrite resolves each name where the emitter's namespace put it,
    /// and leaves binders, core names, literals and projections alone.
    #[test]
    fn statements_are_qualified_as_their_namespace_reads_them() {
        let names = names();
        assert_eq!(
            root_qualify_statement(
                "∀ (s : String), parse ((\"\\\"\" + escape s) + \"escape\") 1 = \
                 ParseResult.ok (Json.jsonString s) (((escape s).length : Int) + 2)",
                "Json",
                &names
            ),
            "∀ (s : String), _root_.Json.parse ((\"\\\"\" + _root_.Json.escape s) + \"escape\") 1 = \
             _root_.Json.ParseResult.ok (_root_.Json.Json.jsonString s) \
             (((_root_.Json.escape s).length : Int) + 2)"
        );
        // A binder that shares a model name stays the binder, and so does a
        // field read through it.
        assert_eq!(
            root_qualify_statement(
                "∀ (escape : String), escape.length = (toString' 0).length",
                "Json",
                &names
            ),
            "∀ (escape : String), escape.length = (_root_.toString' 0).length"
        );
        // An atomic name does not reach a protected declaration through its
        // namespace; the dotted spelling does.
        assert_eq!(
            root_qualify_statement("map 1 = Except.map 1", "Except", &names),
            "map 1 = _root_.Except.map 1"
        );
        // An already qualified name and a character literal are kept.
        assert_eq!(
            root_qualify_statement("_root_.Json.escape 'e' = escape \"e\"", "Json", &names),
            "_root_.Json.escape 'e' = _root_.Json.escape \"e\""
        );
    }
}
