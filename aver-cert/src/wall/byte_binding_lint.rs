//! Byte-binding lint: every producer-declared value must be constrained by the
//! module bytes somewhere inside the acceptance predicate.
//!
//! # Why this exists
//!
//! A certificate manifest carries values DECLARED by the producer, who is
//! untrusted. A declared value is only trustworthy if some conjunct of
//! `AverCert.AcceptedArtifact.accepted` constrains it against the artifact
//! bytes. Six times a declared value has been consumed by a proof while nothing
//! constrained it; the most recent was `hostRoleTable.toIndex`, where the
//! byte-derived binding (`CertDecode.AddSub.toIndexIdx`) existed and its own
//! comment claimed to prevent exactly that attack, but its only consumer had
//! left the acceptance path in an earlier refactor. The spec, the comment and
//! the function name all agreed, and all three were wrong.
//!
//! # What the lint checks, and why it is not "is the field projected"
//!
//! The obvious property — "every field is projected somewhere inside the
//! definitional cone of `accepted`" — does NOT catch that bug. At the pre-fix
//! commit the chain
//!
//! ```text
//! accepted -> StandardFace.checkedFaces -> symFragmentMatches
//!          -> hostTableBound -> decodedRoleIdx -> roles.toIndex
//! ```
//!
//! puts `roles.toIndex` squarely inside the cone. The bug was not "never
//! projected"; it was "projected, and even compared, but only against another
//! producer-declared value" — `hostTableBound` compares the claim's `hostTable`
//! against the subject's declared roles, and nothing in that chain touches
//! `modBytes`. So reachability is necessary (it is what notices a binding that
//! has LEFT the acceptance path, the literal failure mode here) but nowhere near
//! sufficient. The discriminating condition is ANCHORING: the field has to meet
//! something the producer does not control.
//!
//! A field slot has binding evidence when a definition reachable from `accepted`
//! contains a top-level conjunct satisfying one of:
//!
//! * **A — byte co-occurrence.** The conjunct projects the field and applies a
//!   byte anchor. Anchors are auto-derived, never hand-listed: a wall `def` is an
//!   anchor when its binders carry a byte stream (`(n len : Nat)`,
//!   `(modBytes modLen : Nat)`, `(componentBytes componentLen : Nat)`) or
//!   `(artifact : ArtifactData)`. This is why
//!   `CertDecode.AddSub.toIndexIdx` is an anchor and `decodedRoleIdx` is not —
//!   exactly the distinction the bug turned on.
//! * **A1 — one-hop argument.** A producer value passed whole into a reachable
//!   definition from an anchored conjunct, where the callee projects the field.
//!   Depth one only; deeper propagation reinstates the historical false negative.
//! * **B — pinned to a wall term.** The conjunct equates the field to a term
//!   mentioning no producer-supplied value at all (a wall literal, a wall
//!   constant, a byte decode). The producer then has no freedom in that field.
//! * **C — whole-value equality against bytes.** One side of the conjunct is
//!   producer-free AND byte-derived, the other mentions a producer value whole;
//!   every leaf field of that value is then determined. This is how the lowered
//!   plan payloads are constrained (`lowerX plan = <real code bytes>`).
//!
//! Rules A1 and C are deliberately narrow. Blanket versions of both were tried
//! and both silently re-bound `roles.toIndex` in the pre-fix tree, i.e. they
//! reproduced the very blindness the lint exists to prevent. The regression test
//! `lint_still_flags_the_historical_index_helper_gap` is what holds that line.
//!
//! # What this lint cannot see
//!
//! It finds MISSING conjuncts, never WEAK ones. Two limits are worth stating
//! because both were measured, not guessed:
//!
//! * **Co-occurrence is not comparison.** Rule A asks whether a projection and
//!   a byte anchor appear in the same conjunct, not whether they are the two
//!   sides of one equality. Deleting the exact byte equality
//!   `actual == declared.map capabilityBytes` from `importsWithinCapabilities`
//!   does NOT flag `Subject.capabilities`, because a sibling conjunct in the
//!   same (byte-anchored) function still projects the field for the weaker
//!   registry-membership test. Deleting the only conjunct that mentions a field
//!   IS caught — that is the historical class, and mutation runs confirm it for
//!   `Roles.toIndex`, `Roles.box` and `Subject.start`.
//! * **Binding strength is invisible.** `arithRoleCheck` returns `true` for an
//!   unbound (`none`) role — vacuous by design — and the lint still scores
//!   `Roles.add` as bound. Guard-isolation tests (delete one conjunct, watch a
//!   hostile package get accepted) remain the only thing that measures whether a
//!   conjunct is load-bearing.
//!
//! It also sees only the wall: values pinned by the Rust checker or the
//! generated `CheckerWitness.lean` are outside `accepted`'s cone and get
//! flagged, which is what [`Category::PinnedOutsideTheWall`] is for — and that
//! category is itself an unaudited trust surface.
//!
//! # Exceptions
//!
//! There is deliberately no "ignore" or "skip" switch. The only way to make a
//! field green without a byte binding is an [`Allowance`] stating WHY, and every
//! allowance is printed on every run, pass or fail, so the declared-only surface
//! is visible rather than discoverable. Allowances are also checked for rot:
//! an allowance for a field that has since acquired a real binding fails, an
//! allowance naming a field that no longer exists fails, and a
//! [`Category::KnownGap`] allowance must resolve to a `Known gap` marker in the
//! format specification.

use std::collections::{BTreeMap, BTreeSet};

// ---------------------------------------------------------------------------
// Lexical helpers (hand-rolled: this crate ships a deliberately minimal
// dependency tree and the lint must not add to it)
// ---------------------------------------------------------------------------

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '\'' || ('\u{2080}'..='\u{2089}').contains(&c)
}

/// Identifier characters plus the dot/bang/question marks Lean allows in a
/// qualified name, used when scanning for referenced definitions.
fn is_token_char(c: char) -> bool {
    is_ident_char(c) || c == '.' || c == '!' || c == '?'
}

/// Replace block and line comments with layout-preserving whitespace, so byte
/// offsets and line numbers stay meaningful.
fn strip_comments(text: &str) -> String {
    let chars: Vec<char> = text.chars().collect();
    let mut out = String::with_capacity(text.len());
    let mut i = 0usize;
    let mut depth = 0usize;
    while i < chars.len() {
        let two = |i: usize| -> Option<(char, char)> {
            if i + 1 < chars.len() {
                Some((chars[i], chars[i + 1]))
            } else {
                None
            }
        };
        if two(i) == Some(('/', '-')) {
            depth += 1;
            out.push(' ');
            out.push(' ');
            i += 2;
            continue;
        }
        if two(i) == Some(('-', '/')) && depth > 0 {
            depth -= 1;
            out.push(' ');
            out.push(' ');
            i += 2;
            continue;
        }
        if depth > 0 {
            out.push(if chars[i] == '\n' { '\n' } else { ' ' });
            i += 1;
            continue;
        }
        if two(i) == Some(('-', '-')) {
            while i < chars.len() && chars[i] != '\n' {
                out.push(' ');
                i += 1;
            }
            continue;
        }
        out.push(chars[i]);
        i += 1;
    }
    out
}

/// Blank out string literals so their contents are never mistaken for code.
fn blank_string_literals(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut in_str = false;
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if in_str {
            if c == '\\' {
                chars.next();
                continue;
            }
            if c == '"' {
                in_str = false;
                out.push('"');
            }
            continue;
        }
        if c == '"' {
            in_str = true;
            out.push('"');
            continue;
        }
        out.push(c);
    }
    out
}

/// All identifier-or-qualified-name tokens in `s`.
fn tokens(s: &str) -> Vec<String> {
    let chars: Vec<char> = s.chars().collect();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < chars.len() {
        if is_ident_start(chars[i]) && (i == 0 || !is_token_char(chars[i - 1])) {
            let start = i;
            while i < chars.len() && is_token_char(chars[i]) {
                i += 1;
            }
            out.push(chars[start..i].iter().collect::<String>());
        } else {
            i += 1;
        }
    }
    out
}

/// Plain identifiers (no dots), used for local-name scanning.
fn idents(s: &str) -> Vec<String> {
    let chars: Vec<char> = s.chars().collect();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < chars.len() {
        if is_ident_start(chars[i]) && (i == 0 || !is_token_char(chars[i - 1])) {
            let start = i;
            while i < chars.len() && is_ident_char(chars[i]) {
                i += 1;
            }
            if i < chars.len() && chars[i] == '.' {
                while i < chars.len() && is_token_char(chars[i]) {
                    i += 1;
                }
                continue;
            }
            out.push(chars[start..i].iter().collect::<String>());
        } else {
            i += 1;
        }
    }
    out
}

/// A projection chain `base.f1.f2...` found in source text.
#[derive(Debug, Clone)]
struct Chain {
    base: String,
    fields: Vec<String>,
    /// byte offset just past the chain, used to tell `x.f` from `x.f.g`
    end: usize,
    start: usize,
}

/// Scan every `base.f1.f2...` chain. A dot preceded by whitespace or an opening
/// bracket is Lean's anonymous-constructor dot, not a projection, and is skipped
/// because the scan requires an identifier immediately before the dot.
fn chains(s: &str) -> Vec<Chain> {
    let chars: Vec<char> = s.chars().collect();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < chars.len() {
        if is_ident_start(chars[i]) && (i == 0 || !is_token_char(chars[i - 1])) {
            let start = i;
            while i < chars.len() && is_ident_char(chars[i]) {
                i += 1;
            }
            let base: String = chars[start..i].iter().collect();
            let mut fields = Vec::new();
            while i + 1 < chars.len() && chars[i] == '.' && is_ident_start(chars[i + 1]) {
                i += 1;
                let fs = i;
                while i < chars.len() && is_ident_char(chars[i]) {
                    i += 1;
                }
                fields.push(chars[fs..i].iter().collect::<String>());
            }
            if !fields.is_empty() {
                out.push(Chain {
                    base,
                    fields,
                    end: i,
                    start,
                });
            }
        } else {
            i += 1;
        }
    }
    out
}

/// Split `s` on top-level (bracket depth zero) occurrences of any separator.
fn split_top(s: &str, seps: &[&str]) -> Vec<String> {
    let chars: Vec<char> = s.chars().collect();
    let mut parts = Vec::new();
    let mut cur = String::new();
    let mut depth = 0i32;
    let mut i = 0usize;
    'outer: while i < chars.len() {
        let c = chars[i];
        if c == '(' || c == '[' || c == '{' {
            depth += 1;
        } else if c == ')' || c == ']' || c == '}' {
            depth -= 1;
        }
        if depth <= 0 {
            for sep in seps {
                let sc: Vec<char> = sep.chars().collect();
                if i + sc.len() <= chars.len() && chars[i..i + sc.len()] == sc[..] {
                    parts.push(std::mem::take(&mut cur));
                    i += sc.len();
                    continue 'outer;
                }
            }
        }
        cur.push(c);
        i += 1;
    }
    parts.push(cur);
    parts
}

/// A balanced binder group `( names : type )` / `{ .. }` / `[ .. ]`.
#[derive(Debug, Clone)]
struct Binder {
    names: Vec<String>,
    ty: String,
}

fn binders(sig: &str) -> Vec<Binder> {
    let chars: Vec<char> = sig.chars().collect();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < chars.len() {
        let open = chars[i];
        let close = match open {
            '(' => ')',
            '{' => '}',
            '[' => ']',
            _ => {
                i += 1;
                continue;
            }
        };
        let mut depth = 0i32;
        let mut j = i;
        while j < chars.len() {
            if chars[j] == open {
                depth += 1;
            } else if chars[j] == close {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            j += 1;
        }
        if j >= chars.len() {
            i += 1;
            continue;
        }
        let inner: String = chars[i + 1..j].iter().collect();
        if let Some(b) = parse_binder(&inner) {
            out.push(b);
        }
        i += 1;
    }
    out
}

fn parse_binder(inner: &str) -> Option<Binder> {
    // first top-level `:` that is not `:=`
    let chars: Vec<char> = inner.chars().collect();
    let mut depth = 0i32;
    let mut cut = None;
    for i in 0..chars.len() {
        let c = chars[i];
        if c == '(' || c == '[' || c == '{' {
            depth += 1;
        } else if c == ')' || c == ']' || c == '}' {
            depth -= 1;
        } else if c == ':' && depth == 0 && chars.get(i + 1) != Some(&'=') {
            cut = Some(i);
            break;
        }
    }
    let cut = cut?;
    let lhs: String = chars[..cut].iter().collect();
    let rhs: String = chars[cut + 1..].iter().collect();
    let names: Vec<String> = lhs.split_whitespace().map(|s| s.to_string()).collect();
    if names.is_empty()
        || !names
            .iter()
            .all(|n| n.chars().next().is_some_and(is_ident_start))
    {
        return None;
    }
    Some(Binder {
        names,
        ty: rhs.trim().to_string(),
    })
}

// ---------------------------------------------------------------------------
// Wall model
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    Def,
    Structure,
    Other,
}

#[derive(Debug, Clone)]
struct Decl {
    kind: Kind,
    short: String,
    full: String,
    ns: Vec<String>,
    opens: Vec<String>,
    sig: String,
    body: String,
    file: String,
    line: usize,
    /// structure fields, as (name, type expression)
    fields: Vec<(String, String)>,
}

const DECL_KEYWORDS: [&str; 7] = [
    "def",
    "abbrev",
    "theorem",
    "lemma",
    "instance",
    "structure",
    "inductive",
];

const MODIFIERS: [&str; 5] = ["private", "protected", "partial", "noncomputable", "unsafe"];

/// Recognise a declaration header, returning (indent, kind, name).
fn decl_header(line: &str) -> Option<(usize, Kind, String)> {
    let indent = line.len() - line.trim_start().len();
    let mut rest = line.trim_start();
    if rest.starts_with("@[") {
        let close = rest.find(']')?;
        rest = rest[close + 1..].trim_start();
    }
    loop {
        let mut advanced = false;
        for m in MODIFIERS {
            if let Some(r) = rest.strip_prefix(m)
                && r.starts_with(char::is_whitespace)
            {
                rest = r.trim_start();
                advanced = true;
            }
        }
        if !advanced {
            break;
        }
    }
    for kw in DECL_KEYWORDS {
        if let Some(r) = rest.strip_prefix(kw) {
            if !r.starts_with(char::is_whitespace) {
                continue;
            }
            let name: String = r
                .trim_start()
                .chars()
                .take_while(|c| is_token_char(*c))
                .collect();
            if name.is_empty() || !name.chars().next().is_some_and(is_ident_start) {
                continue;
            }
            let kind = match kw {
                "def" | "abbrev" => Kind::Def,
                "structure" => Kind::Structure,
                _ => Kind::Other,
            };
            return Some((indent, kind, name));
        }
    }
    None
}

fn parse_file(name: &str, text: &str) -> Vec<Decl> {
    let stripped = strip_comments(text);
    let lines: Vec<&str> = stripped.split('\n').collect();

    // namespace / open context per line
    let mut stack: Vec<Option<String>> = Vec::new();
    let mut opens: Vec<String> = Vec::new();
    let mut ns_at: Vec<Vec<String>> = Vec::with_capacity(lines.len());
    let mut op_at: Vec<Vec<String>> = Vec::with_capacity(lines.len());
    for line in &lines {
        let t = line.trim_start();
        if let Some(r) = t.strip_prefix("namespace ") {
            stack.push(Some(r.trim().to_string()));
        } else if t.starts_with("section") || t.starts_with("mutual") {
            stack.push(None);
        } else if t == "end" || t.starts_with("end ") {
            stack.pop();
        } else if let Some(r) = t.strip_prefix("open ")
            && !r.contains(" in ")
        {
            for tok in r.split_whitespace() {
                if tok.chars().next().is_some_and(is_ident_start) {
                    opens.push(tok.to_string());
                }
            }
        }
        ns_at.push(stack.iter().flatten().cloned().collect());
        op_at.push(opens.clone());
    }

    let mut starts: Vec<(usize, usize, Kind, String)> = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        if let Some((indent, kind, nm)) = decl_header(line) {
            starts.push((i, indent, kind, nm));
        }
    }

    let mut out = Vec::new();
    for (k, (i, indent, kind, short)) in starts.iter().enumerate() {
        let end = starts.get(k + 1).map(|s| s.0).unwrap_or(lines.len());
        let body = lines[*i..end].join("\n");
        let sig = match body.find(":=") {
            Some(c) => body[..c].to_string(),
            None => body.clone(),
        };
        let short_tail = short.rsplit('.').next().unwrap_or(short).to_string();
        let mut full = ns_at[*i].clone();
        full.push(short.clone());
        let fields = if *kind == Kind::Structure {
            parse_fields(&lines[*i..end], *indent)
        } else {
            Vec::new()
        };
        out.push(Decl {
            kind: *kind,
            short: short_tail,
            full: full.join("."),
            ns: ns_at[*i].clone(),
            opens: op_at[*i].clone(),
            sig,
            body,
            file: name.to_string(),
            line: i + 1,
            fields,
        });
    }
    out
}

fn parse_fields(chunk: &[&str], base_indent: usize) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in chunk.iter().skip(1) {
        if line.trim().is_empty() {
            continue;
        }
        let ind = line.len() - line.trim_start().len();
        if ind <= base_indent {
            break;
        }
        let t = line.trim();
        if t.starts_with("deriving") {
            break;
        }
        // `names : type`, rejecting `:=` (default values are still fields, but
        // the header form `name : Ty := default` splits on the first `:`)
        let chars: Vec<char> = line.chars().collect();
        let mut depth = 0i32;
        let mut cut = None;
        for i in 0..chars.len() {
            let c = chars[i];
            if c == '(' || c == '[' || c == '{' {
                depth += 1;
            } else if c == ')' || c == ']' || c == '}' {
                depth -= 1;
            } else if c == ':' && depth == 0 && chars.get(i + 1) != Some(&'=') {
                cut = Some(i);
                break;
            }
        }
        let Some(cut) = cut else { continue };
        let lhs: String = chars[..cut].iter().collect();
        let ty: String = chars[cut + 1..].iter().collect();
        let names: Vec<String> = lhs.split_whitespace().map(|s| s.to_string()).collect();
        if names.is_empty()
            || !names.iter().all(|n| {
                n.chars().all(is_ident_char) && n.chars().next().is_some_and(is_ident_start)
            })
        {
            continue;
        }
        for n in names {
            out.push((n, ty.trim().to_string()));
        }
    }
    out
}

struct Wall {
    decls: Vec<Decl>,
    by_full: BTreeMap<String, usize>,
    structs: BTreeMap<String, usize>,
    struct_by_short: BTreeMap<String, Vec<String>>,
}

const BUILTIN_TYPES: [&str; 12] = [
    "Option", "List", "Prod", "Nat", "String", "Int", "Bool", "Type", "Array", "Char", "UInt64",
    "Prop",
];

impl Wall {
    fn new(decls: Vec<Decl>) -> Self {
        let mut by_full = BTreeMap::new();
        let mut structs = BTreeMap::new();
        let mut struct_by_short: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (i, d) in decls.iter().enumerate() {
            by_full.entry(d.full.clone()).or_insert(i);
            if d.kind == Kind::Structure {
                structs.entry(d.full.clone()).or_insert(i);
                struct_by_short
                    .entry(d.short.clone())
                    .or_default()
                    .push(d.full.clone());
            }
        }
        Wall {
            decls,
            by_full,
            structs,
            struct_by_short,
        }
    }

    fn get(&self, full: &str) -> Option<&Decl> {
        self.by_full.get(full).map(|i| &self.decls[*i])
    }

    fn structure(&self, full: &str) -> Option<&Decl> {
        self.structs.get(full).map(|i| &self.decls[*i])
    }

    /// Namespace-aware resolution of a referenced name. Short-name keying alone
    /// is not enough: it once pulled the DEAD `CertDecode.AddSub.roleTable` into
    /// the reachable set on the live `StringHost.roleTable`'s name.
    fn resolve(&self, tok: &str, ns: &[String], opens: &[String]) -> Option<String> {
        for k in (0..=ns.len()).rev() {
            let mut cand = ns[..k].to_vec();
            cand.push(tok.to_string());
            let c = cand.join(".");
            if self.by_full.contains_key(&c) {
                return Some(c);
            }
        }
        for o in opens {
            let c = format!("{o}.{tok}");
            if self.by_full.contains_key(&c) {
                return Some(c);
            }
        }
        if self.by_full.contains_key(tok) {
            return Some(tok.to_string());
        }
        None
    }

    /// Type expression -> producer structure, peeling `Option`/`List`/`Prod`.
    fn resolve_struct(&self, ty: &str, ns: &[String], opens: &[String]) -> Option<String> {
        for t in tokens(ty) {
            if BUILTIN_TYPES.contains(&t.as_str()) {
                continue;
            }
            if let Some(r) = self.resolve(&t, ns, opens)
                && self.structs.contains_key(&r)
            {
                return Some(r);
            }
            if self.structs.contains_key(&t) {
                return Some(t);
            }
            let short = t.rsplit('.').next().unwrap_or(&t);
            if let Some(v) = self.struct_by_short.get(short)
                && v.len() == 1
            {
                return Some(v[0].clone());
            }
        }
        None
    }
}

// ---------------------------------------------------------------------------
// Producer surface and reachability
// ---------------------------------------------------------------------------

/// A producer-declared value slot: one leaf field of a producer structure.
pub type FieldRef = (String, String);

fn is_arrow(ty: &str) -> bool {
    ty.contains('→') || ty.contains("->")
}

/// Structures reachable from `ArtifactData` through plain-data field types.
/// Arrow types are not followed: `Obligation.domRepr : CarrierSpec c -> ...`
/// must not drag `CarrierSpec` in, because a `CarrierSpec` is universally
/// quantified inside `Obligation.holds` and is wall-side, not producer-supplied.
fn producer_closure(w: &Wall) -> BTreeSet<String> {
    let mut start = None;
    for d in &w.decls {
        if d.kind == Kind::Structure && d.short == "ArtifactData" {
            start = Some(d.full.clone());
        }
    }
    let mut seen = BTreeSet::new();
    let mut stack: Vec<String> = start.into_iter().collect();
    while let Some(s) = stack.pop() {
        if !seen.insert(s.clone()) {
            continue;
        }
        let Some(d) = w.structure(&s) else { continue };
        for (_f, ty) in &d.fields {
            if is_arrow(ty) {
                continue;
            }
            if let Some(t) = w.resolve_struct(ty, &d.ns, &d.opens)
                && !seen.contains(&t)
            {
                stack.push(t);
            }
        }
    }
    seen
}

fn reachable_from(w: &Wall, root: &str) -> BTreeSet<String> {
    let mut seen = BTreeSet::new();
    let mut stack = vec![root.to_string()];
    while let Some(s) = stack.pop() {
        if !seen.insert(s.clone()) {
            continue;
        }
        let Some(d) = w.get(&s) else { continue };
        for tok in tokens(&d.body) {
            if let Some(r) = w.resolve(&tok, &d.ns, &d.opens)
                && !seen.contains(&r)
            {
                stack.push(r);
            }
            if tok.contains('.') {
                let parts: Vec<&str> = tok.split('.').collect();
                for j in (1..parts.len()).rev() {
                    let cand = parts[..j].join(".");
                    if let Some(r) = w.resolve(&cand, &d.ns, &d.opens)
                        && !seen.contains(&r)
                    {
                        stack.push(r);
                    }
                }
            }
        }
    }
    seen
}

/// A definition is a byte anchor when its binders carry a byte stream or the
/// whole artifact. Auto-derived on purpose: a hand-maintained anchor list is
/// exactly the sort of table that rots into a lie.
fn is_byte_anchor(d: &Decl) -> bool {
    for b in binders(&d.sig) {
        let ty = b.ty.trim();
        if ty == "Nat" {
            for pair in [
                ("n", "len"),
                ("modBytes", "modLen"),
                ("componentBytes", "componentLen"),
            ] {
                for i in 0..b.names.len().saturating_sub(1) {
                    if b.names[i] == pair.0 && b.names[i + 1] == pair.1 {
                        return true;
                    }
                }
            }
        }
        if ty == "ArtifactData" && b.names.len() == 1 && b.names[0] == "artifact" {
            return true;
        }
    }
    false
}

fn binds_raw_bytes(d: &Decl) -> bool {
    for b in binders(&d.sig) {
        if b.ty.trim() == "Nat" {
            for pair in [
                ("n", "len"),
                ("modBytes", "modLen"),
                ("componentBytes", "componentLen"),
            ] {
                for i in 0..b.names.len().saturating_sub(1) {
                    if b.names[i] == pair.0 && b.names[i + 1] == pair.1 {
                        return true;
                    }
                }
            }
        }
    }
    false
}

// ---------------------------------------------------------------------------
// Local typing
// ---------------------------------------------------------------------------

type Env = BTreeMap<String, String>;

fn let_bindings(body: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in body.split('\n') {
        let mut rest = line;
        while let Some(p) = rest.find("let ") {
            let after = &rest[p + 4..];
            let name: String = after.chars().take_while(|c| is_ident_char(*c)).collect();
            let tail = &after[name.len()..];
            if !name.is_empty()
                && let Some(t) = tail.trim_start().strip_prefix(":=")
            {
                out.push((name, t.trim().to_string()));
            }
            rest = &rest[p + 4..];
        }
    }
    out
}

/// `match E1, E2 with` scrutinee texts.
fn match_scrutinees(body: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut rest = body;
    while let Some(p) = rest.find("match ") {
        let after = &rest[p + 6..];
        if let Some(w) = after.find(" with") {
            out.push(after[..w].to_string());
        }
        rest = &rest[p + 6..];
    }
    out
}

/// Identifiers bound by a `∃ a b c,` prefix.
fn exists_names(body: &str) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    let mut rest = body;
    while let Some(p) = rest.find('∃') {
        let after = &rest[p + '∃'.len_utf8()..];
        if let Some(c) = after.find(',') {
            for id in idents(&after[..c]) {
                if id != "_" {
                    out.insert(id);
                }
            }
        }
        rest = after;
    }
    out
}

fn pattern_binds(pat: &str) -> Vec<String> {
    let p = pat.trim();
    for head in ["some ", ".some "] {
        if let Some(r) = p.strip_prefix(head) {
            return idents(r).into_iter().filter(|x| x != "_").collect();
        }
    }
    if let Some(i) = p.find("::") {
        return idents(&p[..i]).into_iter().filter(|x| x != "_").collect();
    }
    Vec::new()
}

fn local_env(w: &Wall, d: &Decl) -> Env {
    let mut env: Env = BTreeMap::new();
    for b in binders(&d.sig) {
        if let Some(t) = w.resolve_struct(&b.ty, &d.ns, &d.opens) {
            for n in b.names {
                env.entry(n).or_insert_with(|| t.clone());
            }
        }
    }
    for (name, rhs) in let_bindings(&d.body) {
        if let Some(t) = expr_struct(w, &env, &rhs) {
            env.entry(name).or_insert(t);
        }
    }
    // match-scrutinee propagation, to a fixpoint bounded at three rounds
    for _ in 0..3 {
        let mut grew = false;
        for scrut in match_scrutinees(&d.body) {
            let parts = split_top(&scrut, &[","]);
            let types: Vec<Option<String>> = parts
                .iter()
                .map(|p| expr_struct(w, &env, p.trim()))
                .collect();
            let Some(pos) = d.body.find(&scrut) else {
                continue;
            };
            let tail = &d.body[pos..];
            let window = &tail[..tail.len().min(6000)];
            for arm in window.split('|').skip(1) {
                let Some(ar) = arm.find("=>") else { continue };
                let pats = split_top(&arm[..ar], &[","]);
                if pats.len() != types.len() {
                    continue;
                }
                for (pat, ty) in pats.iter().zip(types.iter()) {
                    let Some(ty) = ty else { continue };
                    for nm in pattern_binds(pat) {
                        if let std::collections::btree_map::Entry::Vacant(e) = env.entry(nm) {
                            e.insert(ty.clone());
                            grew = true;
                        }
                    }
                }
            }
        }
        if !grew {
            break;
        }
    }
    env
}

/// Type of an expression, when it is exactly an identifier or projection chain.
fn expr_struct(w: &Wall, env: &Env, expr: &str) -> Option<String> {
    let e = expr.trim();
    if e.chars().all(is_ident_char) && !e.is_empty() {
        return env.get(e).cloned();
    }
    let cs = chains(e);
    if cs.len() != 1 || cs[0].start != 0 || cs[0].end != e.chars().count() {
        return None;
    }
    let c = &cs[0];
    let mut cur = env.get(&c.base)?.clone();
    for f in &c.fields {
        let sd = w.structure(&cur)?;
        let (_n, ty) = sd.fields.iter().find(|(n, _)| n == f)?;
        if is_arrow(ty) {
            return None;
        }
        cur = w.resolve_struct(ty, &sd.ns, &sd.opens)?;
    }
    Some(cur)
}

/// Resolve the longest prefix of `base.f1.f2...` that ends at a real field.
/// The prefix walk matters: `manifest.subject.declaredUncertified.map` must
/// attribute to `Subject.declaredUncertified`, not fail on the trailing `.map`.
fn chain_field(w: &Wall, env: &Env, c: &Chain) -> Option<FieldRef> {
    for k in (1..=c.fields.len()).rev() {
        if let Some(r) = chain_field_exact(w, env, &c.base, &c.fields[..k]) {
            return Some(r);
        }
    }
    None
}

fn chain_field_exact(w: &Wall, env: &Env, base: &str, fields: &[String]) -> Option<FieldRef> {
    let mut cur = env.get(base)?.clone();
    for (i, f) in fields.iter().enumerate() {
        let sd = w.structure(&cur)?;
        let (_n, ty) = sd.fields.iter().find(|(n, _)| n == f)?;
        if i + 1 == fields.len() {
            return Some((cur, f.clone()));
        }
        if is_arrow(ty) {
            return None;
        }
        cur = w.resolve_struct(ty, &sd.ns, &sd.opens)?;
    }
    None
}

// ---------------------------------------------------------------------------
// Conjunct analysis
// ---------------------------------------------------------------------------

fn conjuncts(body: &str) -> Vec<String> {
    let tail = match body.find(":=") {
        Some(c) => &body[c + 2..],
        None => body,
    };
    let mut out = Vec::new();
    for arm in split_top(tail, &["\n  | ", "\n      | "]) {
        out.extend(split_top(&arm, &["&&", "∧"]));
    }
    out
}

fn expand_lets(c: &str, lets: &BTreeMap<String, String>) -> String {
    let mut cur = c.to_string();
    for _ in 0..3 {
        let mut out = String::with_capacity(cur.len());
        let chars: Vec<char> = cur.chars().collect();
        let mut i = 0usize;
        let mut changed = false;
        while i < chars.len() {
            if is_ident_start(chars[i]) && (i == 0 || !is_token_char(chars[i - 1])) {
                let start = i;
                while i < chars.len() && is_ident_char(chars[i]) {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                match lets.get(&word) {
                    Some(rhs) => {
                        out.push('(');
                        out.push_str(rhs);
                        out.push(')');
                        changed = true;
                    }
                    None => out.push_str(&word),
                }
            } else {
                out.push(chars[i]);
                i += 1;
            }
        }
        cur = out;
        if !changed {
            break;
        }
    }
    cur
}

/// Split a conjunct at its top-level equality, if it has one.
fn eq_sides(c: &str) -> Option<(String, String)> {
    let chars: Vec<char> = c.chars().collect();
    let mut depth = 0i32;
    let mut i = 0usize;
    while i < chars.len() {
        let ch = chars[i];
        if ch == '(' || ch == '[' || ch == '{' {
            depth += 1;
        } else if ch == ')' || ch == ']' || ch == '}' {
            depth -= 1;
        }
        if depth <= 0 {
            if chars.get(i) == Some(&'=') && chars.get(i + 1) == Some(&'=') {
                return Some((chars[..i].iter().collect(), chars[i + 2..].iter().collect()));
            }
            if (chars.get(i) == Some(&':') || chars.get(i) == Some(&'!'))
                && chars.get(i + 1) == Some(&'=')
            {
                i += 2;
                continue;
            }
            if ch == '=' && chars.get(i + 1) != Some(&'>') {
                return Some((chars[..i].iter().collect(), chars[i + 1..].iter().collect()));
            }
        }
        i += 1;
    }
    None
}

fn strip_wrappers(side: &str) -> String {
    let mut s = side.trim();
    for w in ["some ", ".some ", "Option.some "] {
        if let Some(r) = s.strip_prefix(w) {
            s = r.trim();
        }
    }
    s.to_string()
}

/// A side that mentions no producer-supplied value at all.
fn producer_free(w: &Wall, env: &Env, side: &str) -> bool {
    let s = blank_string_literals(side);
    if s.trim().is_empty() {
        return false;
    }
    for c in chains(&s) {
        if env.contains_key(&c.base) {
            return false;
        }
        if chain_field(w, env, &c).is_some() {
            return false;
        }
    }
    for t in idents(&s) {
        if env.contains_key(&t) {
            return false;
        }
    }
    true
}

/// Every producer value appearing WHOLE (not projected) on a side.
fn whole_producer_values(w: &Wall, env: &Env, side: &str) -> Vec<String> {
    let mut out = Vec::new();
    let cs = chains(side);
    for id in idents(side) {
        if let Some(t) = env.get(&id) {
            out.push(t.clone());
        }
    }
    for c in cs {
        let text: String = side.chars().skip(c.start).take(c.end - c.start).collect();
        if side.chars().nth(c.end) == Some('.') {
            continue;
        }
        if let Some(t) = expr_struct(w, env, &text) {
            out.push(t);
        }
    }
    out
}

/// Leaf slots of a structure, transitively through nested producer structures.
fn leaves_of(
    w: &Wall,
    root: &str,
    slots: &BTreeSet<FieldRef>,
    seen: &mut BTreeSet<String>,
    out: &mut Vec<FieldRef>,
) {
    if !seen.insert(root.to_string()) {
        return;
    }
    let Some(sd) = w.structure(root) else { return };
    for (f, ty) in &sd.fields {
        let key = (root.to_string(), f.clone());
        if slots.contains(&key) {
            out.push(key);
        }
        if !is_arrow(ty)
            && let Some(sub) = w.resolve_struct(ty, &sd.ns, &sd.opens)
        {
            leaves_of(w, &sub, slots, seen, out);
        }
    }
}

/// Parameter names of a definition, in order.
fn sig_params(d: &Decl) -> Vec<String> {
    let head = match d.sig.find(":=") {
        Some(c) => &d.sig[..c],
        None => &d.sig[..],
    };
    binders(head).into_iter().flat_map(|b| b.names).collect()
}

/// Applications `f a1 a2 ...` in a conjunct, with textual arguments.
fn call_sites(c: &str) -> Vec<(String, Vec<String>)> {
    let mut out = Vec::new();
    let chars: Vec<char> = c.chars().collect();
    let mut i = 0usize;
    while i < chars.len() {
        if is_ident_start(chars[i]) && (i == 0 || !is_token_char(chars[i - 1])) {
            let start = i;
            while i < chars.len() && is_token_char(chars[i]) {
                i += 1;
            }
            let name: String = chars[start..i].iter().collect();
            let mut args = Vec::new();
            loop {
                let mut j = i;
                while j < chars.len() && (chars[j] == ' ' || chars[j] == '\n') {
                    j += 1;
                }
                if j >= chars.len() || j == i {
                    break;
                }
                if chars[j] == '(' {
                    let mut depth = 0i32;
                    let mut k = j;
                    while k < chars.len() {
                        if chars[k] == '(' {
                            depth += 1;
                        } else if chars[k] == ')' {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        k += 1;
                    }
                    if k >= chars.len() {
                        break;
                    }
                    args.push(chars[j + 1..k].iter().collect::<String>());
                    i = k + 1;
                } else if chars[j] == ')' || chars[j] == ']' || chars[j] == '}' || chars[j] == ',' {
                    break;
                } else {
                    let mut k = j;
                    while k < chars.len()
                        && !chars[k].is_whitespace()
                        && chars[k] != '('
                        && chars[k] != ')'
                    {
                        k += 1;
                    }
                    args.push(chars[j..k].iter().collect::<String>());
                    i = k;
                }
            }
            if !args.is_empty() {
                out.push((name, args));
            }
        } else {
            i += 1;
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Report
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Evidence {
    pub rule: &'static str,
    pub decl: String,
    pub file: String,
    pub line: usize,
    pub conjunct: String,
}

#[derive(Debug)]
pub struct Report {
    /// every producer-declared leaf value slot
    pub slots: BTreeSet<FieldRef>,
    /// slots with binding evidence, and where it was found
    pub bound: BTreeMap<FieldRef, Evidence>,
    /// slots with no binding evidence at all
    pub flagged: Vec<FieldRef>,
    pub reachable: usize,
    pub anchors: usize,
}

impl Report {
    pub fn is_bound(&self, structure: &str, field: &str) -> bool {
        self.bound
            .keys()
            .any(|(s, f)| s.ends_with(structure) && f == field)
    }

    pub fn is_flagged(&self, structure: &str, field: &str) -> bool {
        self.flagged
            .iter()
            .any(|(s, f)| s.ends_with(structure) && f == field)
    }
}

/// Run the byte-binding analysis over a set of Lean wall sources.
pub fn analyse(sources: &[(&str, &str)]) -> Report {
    let mut decls = Vec::new();
    let mut names: Vec<&(&str, &str)> = sources.iter().collect();
    names.sort_by_key(|(n, _)| *n);
    for (name, text) in names {
        decls.extend(parse_file(name, text));
    }
    let w = Wall::new(decls);

    let root = w
        .decls
        .iter()
        .find(|d| d.short == "accepted" && d.full.ends_with("AcceptedArtifact.accepted"))
        .map(|d| d.full.clone())
        .unwrap_or_default();
    let reach = reachable_from(&w, &root);

    let mut anchors: BTreeSet<String> = BTreeSet::new();
    let mut anchor_short: BTreeSet<String> = BTreeSet::new();
    for d in &w.decls {
        if d.kind == Kind::Def && is_byte_anchor(d) {
            anchors.insert(d.full.clone());
            anchor_short.insert(d.short.clone());
        }
    }

    let prod = producer_closure(&w);
    let mut slots: BTreeSet<FieldRef> = BTreeSet::new();
    for s in &prod {
        let Some(sd) = w.structure(s) else { continue };
        for (f, ty) in &sd.fields {
            let sub = if is_arrow(ty) {
                None
            } else {
                w.resolve_struct(ty, &sd.ns, &sd.opens)
            };
            // A field whose type is itself a producer structure is scaffolding,
            // not a value: its leaves carry the producer's freedom.
            match sub {
                Some(t) if prod.contains(&t) => {}
                _ => {
                    slots.insert((s.clone(), f.clone()));
                }
            }
        }
    }

    // unique-name fallback for chains whose base type cannot be resolved
    let mut field_owner: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (s, f) in &slots {
        field_owner.entry(f.clone()).or_default().insert(s.clone());
    }
    let unique_owner = |f: &str| -> Option<FieldRef> {
        field_owner.get(f).and_then(|o| {
            if o.len() == 1 {
                Some((o.iter().next().unwrap().clone(), f.to_string()))
            } else {
                None
            }
        })
    };

    // per-declaration caches for the one-hop rule
    let mut env_cache: BTreeMap<String, Env> = BTreeMap::new();
    let mut bound: BTreeMap<FieldRef, Evidence> = BTreeMap::new();

    let mark = |bound: &mut BTreeMap<FieldRef, Evidence>,
                key: FieldRef,
                rule: &'static str,
                d: &Decl,
                c: &str| {
        bound.entry(key).or_insert_with(|| Evidence {
            rule,
            decl: d.full.clone(),
            file: d.file.clone(),
            line: d.line,
            conjunct: c.split_whitespace().collect::<Vec<_>>().join(" "),
        });
    };

    for d in &w.decls {
        if d.kind != Kind::Def || !reach.contains(&d.full) {
            continue;
        }
        let env = local_env(&w, d);
        let raw_bytes = binds_raw_bytes(d);
        let lets: BTreeMap<String, String> = let_bindings(&d.body).into_iter().collect();

        // A byte-derived `match` scrutinee guards every arm of the definition.
        let mut scrut_toks: BTreeSet<String> = BTreeSet::new();
        for s in match_scrutinees(&d.body) {
            for t in tokens(&expand_lets(&s, &lets)) {
                scrut_toks.insert(t.rsplit('.').next().unwrap_or(&t).to_string());
                scrut_toks.insert(t);
            }
        }

        let cjs = conjuncts(&d.body);

        // ONE-STEP existential chaining. A `∃`-bound name that itself appears in
        // a conjunct naming the raw byte stream is byte-determined, which is how
        // `lowerX plan = some codeEntry` reaches the module bytes through the
        // neighbouring `exactFuncBindingForExport modBytes modLen ... codeEntry`.
        // Strictly one step, no fixpoint: a transitive version was tried and it
        // re-bound `roles.toIndex` in the pre-fix tree, destroying the lint.
        let ex = exists_names(&d.body);
        let mut byte_locals: BTreeSet<String> = BTreeSet::new();
        for c in &cjs {
            let tk: BTreeSet<String> = tokens(c).into_iter().collect();
            let has_bytes = tk.contains("modBytes")
                || tk.contains("modLen")
                || tk.contains("componentBytes")
                || tk.contains("componentLen")
                || (raw_bytes && tk.contains("n") && tk.contains("len"));
            if has_bytes {
                for id in idents(c) {
                    if ex.contains(&id) {
                        byte_locals.insert(id);
                    }
                }
            }
        }

        for c in &cjs {
            let cx = expand_lets(c, &lets);
            let mut toks: BTreeSet<String> = scrut_toks.clone();
            for t in tokens(&cx) {
                if let Some(r) = w.resolve(&t, &d.ns, &d.opens) {
                    toks.insert(r);
                }
                toks.insert(t.rsplit('.').next().unwrap_or(&t).to_string());
                toks.insert(t);
            }
            let anchored = toks.contains("modBytes")
                || toks.contains("modLen")
                || toks.contains("componentBytes")
                || toks.contains("componentLen")
                || (raw_bytes && toks.contains("n") && toks.contains("len"))
                || toks.iter().any(|t| anchors.contains(t))
                || toks.iter().any(|t| anchor_short.contains(t));

            if anchored {
                // Rule A: projection and byte anchor in the same conjunct.
                for ch in chains(c) {
                    let r = chain_field(&w, &env, &ch)
                        .or_else(|| unique_owner(ch.fields.last().unwrap()));
                    if let Some(r) = r
                        && slots.contains(&r)
                    {
                        mark(&mut bound, r, "A/byte-cooccurrence", d, c);
                    }
                }
                // Rule A1: one hop into a callee that projects the field.
                for (name, args) in call_sites(&cx) {
                    let Some(full) = w.resolve(&name, &d.ns, &d.opens) else {
                        continue;
                    };
                    let Some(g) = w.get(&full) else { continue };
                    if g.kind != Kind::Def {
                        continue;
                    }
                    let genv = env_cache
                        .entry(full.clone())
                        .or_insert_with(|| local_env(&w, g))
                        .clone();
                    let params = sig_params(g);
                    for (i, a) in args.iter().enumerate() {
                        let Some(pname) = params.get(i) else { break };
                        let Some(st) = expr_struct(&w, &env, a) else {
                            continue;
                        };
                        let mut genv2 = genv.clone();
                        genv2.insert(pname.clone(), st);
                        for gc in chains(&g.body) {
                            if &gc.base != pname {
                                continue;
                            }
                            if let Some(r) = chain_field(&w, &genv2, &gc)
                                && slots.contains(&r)
                            {
                                mark(&mut bound, r, "A1/one-hop-argument", d, c);
                            }
                        }
                    }
                }
            }

            // Rules B and C: equality with a producer-free counterpart.
            if let Some((l, r)) = eq_sides(c) {
                for (a, b) in [(l.clone(), r.clone()), (r, l)] {
                    if !producer_free(&w, &env, &b) {
                        continue;
                    }
                    let stripped = strip_wrappers(&a);
                    let cs = chains(&stripped);
                    if cs.len() == 1 && cs[0].start == 0 && cs[0].end == stripped.chars().count() {
                        let hit = chain_field(&w, &env, &cs[0])
                            .or_else(|| unique_owner(cs[0].fields.last().unwrap()));
                        if let Some(hit) = hit
                            && slots.contains(&hit)
                        {
                            mark(&mut bound, hit, "B/pinned-to-wall-term", d, c);
                        }
                    }
                    // C needs the counterpart to be byte-derived, not merely
                    // producer-free: `arithTableCheck ... = true` must NOT bind
                    // the roles it is handed, or the historical bug walks free.
                    let btoks: BTreeSet<String> = tokens(&b)
                        .into_iter()
                        .flat_map(|t| [t.rsplit('.').next().unwrap_or(&t).to_string(), t.clone()])
                        .collect();
                    let b_bytes = btoks.contains("modBytes")
                        || btoks.contains("modLen")
                        || btoks.contains("componentBytes")
                        || btoks.contains("componentLen")
                        || (raw_bytes && btoks.contains("n") && btoks.contains("len"))
                        || btoks.iter().any(|t| anchors.contains(t))
                        || btoks.iter().any(|t| anchor_short.contains(t))
                        || idents(&b).iter().any(|t| byte_locals.contains(t));
                    if !b_bytes {
                        continue;
                    }
                    for st in whole_producer_values(&w, &env, &a) {
                        let mut seen = BTreeSet::new();
                        let mut leaves = Vec::new();
                        leaves_of(&w, &st, &slots, &mut seen, &mut leaves);
                        for leaf in leaves {
                            mark(&mut bound, leaf, "C/whole-value-vs-bytes", d, c);
                        }
                    }
                }
            }
        }
    }

    let flagged: Vec<FieldRef> = slots
        .iter()
        .filter(|k| !bound.contains_key(*k))
        .cloned()
        .collect();

    Report {
        slots,
        bound,
        flagged,
        reachable: reach.len(),
        anchors: anchors.len(),
    }
}

// ---------------------------------------------------------------------------
// Exceptions
// ---------------------------------------------------------------------------

/// Why a producer-declared value is allowed to carry no byte binding. The set is
/// closed on purpose: a free-text category would become a synonym for "ignore".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Category {
    /// Transported for display, deliberately outside the verified claim. No
    /// current member: the documented example (`certified[].dom` / `cod`) lives
    /// in the JSON manifest, not in a Lean producer structure, so it is not a
    /// slot this lint ranges over. The category stays because the policy has
    /// four kinds and a reviewer needs the vocabulary to classify the next one.
    #[allow(dead_code)]
    DeclaredOnlyByDesign,
    /// The value SHOULD be constrained and is not. Must be documented as such.
    KnownGap,
    /// Enforced by the Rust checker or the generated witness, not by `accepted`.
    PinnedOutsideTheWall,
    /// Pinned to a value the wall itself computes, so the producer cannot
    /// influence it, rather than to the artifact bytes.
    WallOwnedConstraint,
}

impl Category {
    fn label(self) -> &'static str {
        match self {
            Category::DeclaredOnlyByDesign => "declared-only by design",
            Category::KnownGap => "KNOWN GAP",
            Category::PinnedOutsideTheWall => "pinned outside the wall",
            Category::WallOwnedConstraint => "wall-owned constraint",
        }
    }
}

/// One reviewable exception. There is no wildcard and no per-structure form:
/// granularity is one field, one reason.
#[derive(Debug, Clone, Copy)]
pub struct Allowance {
    /// Structure short name, e.g. `"Subject"`.
    pub structure: &'static str,
    /// Field name, e.g. `"profile"`.
    pub field: &'static str,
    pub category: Category,
    /// Why this value is not bound to the bytes. Must not be empty.
    pub reason: &'static str,
    /// Exact text that must appear in `docs/certificate-format.md`, so the
    /// specification and the lint cannot drift apart silently. This is the rule
    /// aimed at the actual failure mode: the format spec went on describing a
    /// pin that had been deleted, and nothing went red.
    pub doc_anchor: &'static str,
}

/// The exceptions in force for the embedded wall.
///
/// Every entry is printed on every run. Read them as a list of things the
/// certificate does NOT prove about values it transports.
pub const ALLOWED: &[Allowance] = &[
    Allowance {
        structure: "Subject",
        field: "exports",
        category: Category::PinnedOutsideTheWall,
        reason: "The wall never reads this field — its only occurrence in the whole wall is \
                 its own declaration in SchemaCore.lean, and `exportsAccounted` does the \
                 byte-level export accounting from `manifest.obligations` and \
                 `subject.declaredUncertified` instead. It is constrained by the \
                 checker-authored witness rather than by `accepted`: verifier.rs emits \
                 `subject.exports = {names} := rfl` against the SAME `{names}` list it pins \
                 `obligations.map (fun o => o.export_)` to, so the two cannot disagree \
                 without the witness failing to elaborate, and the obligation export names \
                 are byte-bound through `exportsAccounted`. Transported duplicate, not an \
                 independent value. Note this is the one category the lint cannot audit: it \
                 sees only the wall, so `PinnedOutsideTheWall` is trusted by assertion and \
                 defended only by review of verifier.rs.",
        doc_anchor: "",
    },
    Allowance {
        structure: "SymBlock",
        field: "result",
        category: Category::WallOwnedConstraint,
        reason: "Pinned by a wall-computed equality rather than by bytes directly: \
                 `PlanCheck.checkSymBlockFuel` requires `block.result + 1 = block.nodes.length` \
                 (PlanCheck.lean:323) and that the node at that index exists with a matching \
                 id, so once `nodes` is byte-bound through the plan lowering the result index \
                 carries no independent producer freedom. Reached from acceptance via \
                 `PlanCheck.checkSymRawPlan`.",
        doc_anchor: "",
    },
];

fn short_of(qualified: &str) -> &str {
    qualified.rsplit('.').next().unwrap_or(qualified)
}

/// Run the lint and enforce the exception policy. `format_doc` is the text of
/// `docs/certificate-format.md`, used to keep `KnownGap` entries honest.
///
/// Returns the human-readable report on success, or the failure text.
pub fn check(sources: &[(&str, &str)], format_doc: &str) -> Result<String, String> {
    let report = analyse(sources);
    let mut out = String::new();

    out.push_str(&format!(
        "byte-binding lint: {} producer-declared value slots, {} bound, {} flagged \
         ({} definitions reachable from `accepted`, {} auto-derived byte anchors)\n",
        report.slots.len(),
        report.bound.len(),
        report.flagged.len(),
        report.reachable,
        report.anchors,
    ));

    out.push_str("\ndeclared values NOT bound to the module bytes (standing exceptions):\n");
    for a in ALLOWED {
        out.push_str(&format!(
            "  [{}] {}.{}\n      {}\n",
            a.category.label(),
            a.structure,
            a.field,
            a.reason,
        ));
    }

    let mut errors: Vec<String> = Vec::new();

    // 1. Every flagged field must have an allowance.
    for (s, f) in &report.flagged {
        let covered = ALLOWED
            .iter()
            .any(|a| a.structure == short_of(s) && a.field == f);
        if !covered {
            errors.push(format!(
                "UNBOUND producer-declared value `{}.{}`: no conjunct reachable from \
                 `accepted` constrains it against the module bytes.\n    Bind it, or add an \
                 Allowance stating why it is safe to leave declared-only.",
                short_of(s),
                f
            ));
        }
    }

    // 2. No stale allowances: an exception must not outlive the gap it describes.
    for a in ALLOWED {
        if report.is_bound(a.structure, a.field) {
            let ev = report
                .bound
                .iter()
                .find(|((s, f), _)| short_of(s) == a.structure && f == a.field)
                .map(|(_, e)| {
                    format!(
                        "{}:{} in `{}` [{}]\n      {}",
                        e.file, e.line, e.decl, e.rule, e.conjunct
                    )
                })
                .unwrap_or_default();
            errors.push(format!(
                "STALE Allowance for `{}.{}`: the field now has binding evidence at {ev}. \
                 Delete the exception — an exception must not outlive the gap it describes.",
                a.structure, a.field
            ));
        }
    }

    // 3. No phantom fields: a renamed field must not carry its exemption along.
    for a in ALLOWED {
        let exists = report
            .slots
            .iter()
            .any(|(s, f)| short_of(s) == a.structure && f == a.field);
        if !exists {
            errors.push(format!(
                "PHANTOM Allowance for `{}.{}`: no such producer-declared field exists. \
                 It was renamed or removed; update the exception.",
                a.structure, a.field
            ));
        }
        if a.reason.trim().is_empty() {
            errors.push(format!(
                "EMPTY reason on the Allowance for `{}.{}`.",
                a.structure, a.field
            ));
        }
    }

    // 4. Known gaps must be documented as such in the format specification.
    for a in ALLOWED {
        if a.category != Category::KnownGap {
            continue;
        }
        if a.doc_anchor.trim().is_empty() || !format_doc.contains(a.doc_anchor) {
            errors.push(format!(
                "UNDOCUMENTED KnownGap for `{}.{}`: docs/certificate-format.md does not \
                 contain the declared anchor text\n      {:?}\n    A known gap that is not \
                 in the specification is how the last one hid.",
                a.structure, a.field, a.doc_anchor
            ));
        }
    }

    if errors.is_empty() {
        Ok(out)
    } else {
        Err(format!("{out}\n{}\n", errors.join("\n\n")))
    }
}
