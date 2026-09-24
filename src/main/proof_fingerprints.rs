//! Content hashes of the emitted Lean declarations, so that a law which stops
//! proving can be told apart from a model that changed under it.
//!
//! The proof manifest records one hash per emitted definition (`Root.name`)
//! and one per law or obligation script (`fn.law`, `fn.law.becauseN`,
//! `fn.law.implication`). `aver proof --check --compare-manifest <previous>`
//! reads an earlier manifest and, for every claim that did not close, says
//! whether the claim's own script changed and which definitions in its cone
//! (the declarations its theorem mentions, transitively) changed since that
//! manifest. Today that comparison is done by hand from two generated files.
//!
//! The scan is textual and deliberately simple: a declaration runs from its
//! header line to the next header, comment-only lines and blank lines are not
//! hashed, and a hypothesis name derived from a source line (`h_57`) hashes as
//! `h_`, so a definition emitted lower in the file does not change hash when
//! a line is added above it. A cone is an over-approximation: every emitted
//! name that occurs as a token of a declaration counts as a reference.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use aver::codegen::lean as lean_codegen;
use sha2::{Digest, Sha256};

/// Hashes of one emitted proof project plus the reference graph the cone
/// walk needs. Only `definitions` and `scripts` are written to the manifest.
#[derive(Debug, Default)]
pub(super) struct Fingerprints {
    /// Every emitted declaration that is not a law theorem, keyed by its
    /// root-qualified name.
    pub(super) definitions: BTreeMap<String, String>,
    /// Every law and obligation theorem, keyed by its `fn.law` identity. A law
    /// chunked into `_partN` theorems hashes all of its parts in file order.
    pub(super) scripts: BTreeMap<String, String>,
    /// Qualified declaration name -> qualified names it mentions.
    references: HashMap<String, Vec<String>>,
    /// `fn.law` identity -> the qualified theorems that carry it.
    theorems: HashMap<String, Vec<String>>,
}

/// What changed for one claim between a previous manifest and the current
/// export.
#[derive(Debug, PartialEq, Eq)]
pub(super) struct Change {
    /// `same`, `changed`, or `new` when the previous manifest has no script
    /// for the claim.
    pub(super) script: &'static str,
    /// Definitions in the claim's cone whose hash differs from the previous
    /// manifest, including definitions the previous manifest did not carry.
    pub(super) definitions: Vec<String>,
}

/// The two hash tables read back from a previous manifest.
#[derive(Debug, Default, Clone)]
pub(super) struct Recorded {
    pub(super) definitions: BTreeMap<String, String>,
    pub(super) scripts: BTreeMap<String, String>,
}

struct Declaration {
    name: String,
    /// Law or obligation identity when the declaration is a marked theorem.
    identity: Option<String>,
    text: Vec<String>,
}

impl Fingerprints {
    /// Scan every lakefile root of the export directory.
    pub(super) fn scan(dir: &str) -> Fingerprints {
        let roots = super::lean_lakefile_roots(dir);
        let mut sources = Vec::new();
        for root in &roots {
            let relative = format!("{}.lean", root.replace('.', "/"));
            let path = std::path::Path::new(dir).join(&relative);
            if let Ok(contents) = std::fs::read_to_string(path) {
                // A committed `--discover` artifact is not built by lake.
                if relative == "DiscoveredLemmas.lean" && contents.contains("-- cone-hash:") {
                    continue;
                }
                sources.push((root.clone(), contents));
            }
        }
        Self::from_sources(&sources)
    }

    /// Build the tables from `(root, contents)` pairs.
    pub(super) fn from_sources(sources: &[(String, String)]) -> Fingerprints {
        let mut declarations = Vec::new();
        for (root, contents) in sources {
            declarations.extend(scan_root(root, contents));
        }
        let mut by_bare: HashMap<&str, Vec<&str>> = HashMap::new();
        for decl in &declarations {
            let bare = decl.name.rsplit('.').next().unwrap_or(&decl.name);
            by_bare.entry(bare).or_default().push(&decl.name);
        }
        let mut out = Fingerprints::default();
        let mut script_parts: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for decl in &declarations {
            let text = decl.text.join("\n");
            let mut mentioned: BTreeSet<String> = BTreeSet::new();
            for token in identifier_tokens(&text) {
                for component in token.split('.') {
                    for qualified in by_bare.get(component).into_iter().flatten() {
                        if *qualified != decl.name {
                            mentioned.insert((*qualified).to_string());
                        }
                    }
                }
            }
            out.references
                .insert(decl.name.clone(), mentioned.into_iter().collect());
            match &decl.identity {
                Some(identity) => {
                    script_parts.entry(identity.clone()).or_default().push(text);
                    out.theorems
                        .entry(identity.clone())
                        .or_default()
                        .push(decl.name.clone());
                }
                None => {
                    out.definitions
                        .insert(decl.name.clone(), content_hash(&text));
                }
            }
        }
        for (identity, parts) in script_parts {
            out.scripts
                .insert(identity, content_hash(&parts.join("\n")));
        }
        out
    }

    /// The definitions a claim's theorems reach, transitively, through every
    /// declaration they mention (law theorems are walked through but listed
    /// under their own identity, not here).
    pub(super) fn cone(&self, identity: &str) -> BTreeSet<String> {
        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut pending: Vec<String> = self.theorems.get(identity).cloned().unwrap_or_default();
        while let Some(name) = pending.pop() {
            if !seen.insert(name.clone()) {
                continue;
            }
            for next in self.references.get(&name).into_iter().flatten() {
                if !seen.contains(next) {
                    pending.push(next.clone());
                }
            }
        }
        seen.into_iter()
            .filter(|name| self.definitions.contains_key(name))
            .collect()
    }

    /// Compare one claim against a previous manifest.
    pub(super) fn compare(&self, previous: &Recorded, identity: &str) -> Change {
        let script = match (previous.scripts.get(identity), self.scripts.get(identity)) {
            (None, _) => "new",
            (Some(before), Some(now)) if before == now => "same",
            _ => "changed",
        };
        let definitions = self
            .cone(identity)
            .into_iter()
            .filter(|name| previous.definitions.get(name) != self.definitions.get(name))
            .collect();
        Change {
            script,
            definitions,
        }
    }
}

/// Read the hash tables of a manifest JSON document; both maps are absent on
/// a manifest written before they existed.
pub(super) fn recorded_from_json(value: &serde_json::Value) -> Recorded {
    fn table(value: &serde_json::Value) -> BTreeMap<String, String> {
        value
            .as_object()
            .map(|map| {
                map.iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect()
            })
            .unwrap_or_default()
    }
    Recorded {
        definitions: table(&value["definitions"]),
        scripts: table(&value["scripts"]),
    }
}

/// Hex of the leading 16 bytes of SHA-256 over the declaration text.
fn content_hash(text: &str) -> String {
    let digest = Sha256::digest(text.as_bytes());
    digest[..16].iter().map(|b| format!("{b:02x}")).collect()
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '\'' | '!' | '?' | '.')
}

fn identifier_tokens(text: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    for c in text.chars() {
        if is_identifier_char(c) {
            current.push(c);
        } else if !current.is_empty() {
            tokens.push(std::mem::take(&mut current));
        }
    }
    if !current.is_empty() {
        tokens.push(current);
    }
    tokens
}

/// `h_57` -> `h_`: the number is the source line of the match, so it moves
/// with every edit above the definition.
fn normalize_line_names(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut rest = line;
    while let Some(idx) = rest.find("h_") {
        let boundary = idx == 0
            || !rest[..idx]
                .chars()
                .next_back()
                .is_some_and(is_identifier_char);
        out.push_str(&rest[..idx + 2]);
        rest = &rest[idx + 2..];
        if boundary {
            let digits = rest.bytes().take_while(u8::is_ascii_digit).count();
            if digits > 0
                && !rest[digits..]
                    .chars()
                    .next()
                    .is_some_and(is_identifier_char)
            {
                rest = &rest[digits..];
            }
        }
    }
    out.push_str(rest);
    out
}

/// Bounded cross-checks of a law (`_checked_domain`, `_checked_domain_partN`,
/// `_sample_N`) are proved by `native_decide` and never opened by a proof.
fn is_bounded_cross_check(name: &str) -> bool {
    fn numbered_suffix(name: &str, marker: &str) -> bool {
        name.rfind(marker).is_some_and(|idx| {
            let tail = &name[idx + marker.len()..];
            !tail.is_empty() && tail.bytes().all(|b| b.is_ascii_digit())
        })
    }
    name.ends_with("_checked_domain")
        || numbered_suffix(name, "_checked_domain_part")
        || numbered_suffix(name, "_sample_")
}

const MODIFIERS: &[&str] = &[
    "private",
    "protected",
    "noncomputable",
    "partial",
    "unsafe",
    "nonrec",
];
const KEYWORDS: &[&str] = &[
    "def",
    "theorem",
    "lemma",
    "abbrev",
    "instance",
    "inductive",
    "structure",
    "class",
    "opaque",
    "axiom",
    "example",
];

/// `(keyword, name)` when the trimmed line opens a declaration. An anonymous
/// instance is named by its header; an `example` has no name.
fn header(trimmed: &str) -> Option<(&'static str, Option<String>)> {
    let mut rest = trimmed;
    loop {
        if let Some(after) = rest.strip_prefix("@[") {
            let close = after.find(']')?;
            rest = after[close + 1..].trim_start();
            continue;
        }
        let word = rest.split_whitespace().next()?;
        if MODIFIERS.contains(&word) {
            rest = rest[word.len()..].trim_start();
            continue;
        }
        let keyword = KEYWORDS.iter().find(|k| **k == word)?;
        let after = rest[word.len()..].trim_start();
        if *keyword == "example" {
            return Some((keyword, None));
        }
        let name = after.split_whitespace().next().unwrap_or("");
        if *keyword == "instance" && (name.is_empty() || name.starts_with(':')) {
            let text = after
                .split(" where")
                .next()
                .unwrap_or(after)
                .split(" :=")
                .next()
                .unwrap_or(after)
                .trim();
            let text = text.split_whitespace().collect::<Vec<_>>().join(" ");
            return Some((keyword, Some(format!("instance {text}"))));
        }
        let name = name.trim_end_matches(':');
        return Some((keyword, (!name.is_empty()).then(|| name.to_string())));
    }
}

enum Block {
    Namespace,
    Other,
}

/// Slice one root file into declarations.
fn scan_root(root: &str, contents: &str) -> Vec<Declaration> {
    // Marker maps: theorem name -> identity, so a marked theorem hashes as a
    // script rather than as a definition.
    let mut labels: HashMap<String, String> = HashMap::new();
    for line in contents.lines() {
        if let Some(rest) = line
            .strip_prefix(lean_codegen::LAW_CLASS_MARKER_PREFIX)
            .or_else(|| line.strip_prefix(lean_codegen::LAW_OBLIGATION_MARKER_PREFIX))
        {
            let mut parts = rest.split_whitespace();
            if let (Some(thm), Some(_class), Some(label)) =
                (parts.next(), parts.next(), parts.next())
            {
                labels.insert(thm.to_string(), label.to_string());
            }
        }
    }
    let identity_of = |bare: &str| -> Option<String> {
        labels
            .get(bare)
            .cloned()
            .or_else(|| super::law_class_base_name(bare).and_then(|base| labels.get(base).cloned()))
    };

    let mut declarations: Vec<Declaration> = Vec::new();
    let mut namespaces: Vec<String> = Vec::new();
    let mut blocks: Vec<Block> = Vec::new();
    let mut current: Option<Declaration> = None;
    let mut prefix: Vec<String> = Vec::new();
    let mut in_block_comment = false;
    let mut skipping = false;

    let flush = |current: &mut Option<Declaration>, declarations: &mut Vec<Declaration>| {
        if let Some(decl) = current.take() {
            declarations.push(decl);
        }
    };

    for line in contents.lines() {
        let trimmed = line.trim_start();
        if in_block_comment {
            if trimmed.contains("-/") {
                in_block_comment = false;
            }
            continue;
        }
        if trimmed.starts_with("/-") {
            if !trimmed.contains("-/") {
                in_block_comment = true;
            }
            continue;
        }
        if trimmed.is_empty() || trimmed.starts_with("--") {
            continue;
        }
        // The isolation guard belongs to the next declaration's command, not
        // to the text of the one above it; like a comment, it is not hashed.
        if trimmed.starts_with(lean_codegen::isolate::ISOLATION_GUARD_PREFIX) {
            flush(&mut current, &mut declarations);
            prefix.clear();
            continue;
        }
        let indent = line.len() - trimmed.len();
        if indent <= 2
            && let Some((_, name)) = header(trimmed)
        {
            flush(&mut current, &mut declarations);
            match name {
                Some(name) if !is_bounded_cross_check(&name) => {
                    let inner = namespaces.iter().skip(1).cloned().collect::<Vec<_>>();
                    let qualified = std::iter::once(root.to_string())
                        .chain(inner)
                        .chain(std::iter::once(name.clone()))
                        .collect::<Vec<_>>()
                        .join(".");
                    let mut text = std::mem::take(&mut prefix);
                    text.push(normalize_line_names(line));
                    current = Some(Declaration {
                        name: qualified,
                        identity: identity_of(&name),
                        text,
                    });
                    skipping = false;
                }
                _ => {
                    prefix.clear();
                    skipping = true;
                }
            }
            continue;
        }
        if indent == 0 {
            if let Some(rest) = trimmed.strip_prefix("namespace ") {
                flush(&mut current, &mut declarations);
                prefix.clear();
                namespaces.push(rest.trim().to_string());
                blocks.push(Block::Namespace);
                continue;
            }
            if trimmed == "end" || trimmed.starts_with("end ") {
                flush(&mut current, &mut declarations);
                prefix.clear();
                if let Some(Block::Namespace) = blocks.pop() {
                    namespaces.pop();
                }
                continue;
            }
            if trimmed == "mutual" || trimmed.starts_with("section") {
                flush(&mut current, &mut declarations);
                prefix.clear();
                blocks.push(Block::Other);
                continue;
            }
            if (trimmed.starts_with("set_option ") && trimmed.ends_with(" in"))
                || trimmed.starts_with("@[")
            {
                flush(&mut current, &mut declarations);
                prefix.push(trimmed.to_string());
                continue;
            }
            if ["import ", "open ", "universe ", "variable ", "set_option "]
                .iter()
                .any(|kw| trimmed.starts_with(kw))
            {
                flush(&mut current, &mut declarations);
                prefix.clear();
                continue;
            }
        }
        if skipping {
            continue;
        }
        if let Some(decl) = current.as_mut() {
            decl.text.push(normalize_line_names(line));
        }
    }
    flush(&mut current, &mut declarations);
    declarations
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = "import AverCommon\n\
namespace Main\n\
\n\
set_option smartUnfolding false in\n\
def double (n : Int) : Int :=\n\
  (n + n)\n\
\n\
def quad (n : Int) : Int :=\n\
  double (double n)\n\
\n\
mutual\n\
  def evens (xs : List Int) : Int :=\n\
    match h_12 : xs with\n\
    | [] => 0\n\
    | x :: rest => odds rest\n\
  termination_by xs.length\n\
  def odds (xs : List Int) : Int :=\n\
    match h_16 : xs with\n\
    | [] => 0\n\
    | x :: rest => evens rest\n\
  termination_by xs.length\n\
end\n\
\n\
-- verify law quad.fourfold (3 cases)\n\
-- aver:law-class quad_law_fourfold universal quad.fourfold\n\
private theorem __aver_reason_and_quad_law_fourfold {a b : Bool} (ha : a = true) : a = true :=\n\
  by simp_all\n\
-- aver:law-obligation __aver_reason_quad_law_fourfold_implication universal quad.fourfold.implication\n\
theorem __aver_reason_quad_law_fourfold_implication : ∀ (n : Int), (quad n = n * 4) := by\n\
  intro n\n\
  simp [quad, double]; omega\n\
theorem quad_law_fourfold : ∀ (n : Int), (quad n = n * 4) := by\n\
  intro n\n\
  exact __aver_reason_quad_law_fourfold_implication n\n\
theorem quad_law_fourfold_sample_1 : quad 0 = 0 * 4 := by native_decide\n\
example : quad 1 = 4 := by native_decide\n\
\n\
end Main\n";

    fn sample() -> Fingerprints {
        Fingerprints::from_sources(&[("Main".to_string(), SAMPLE.to_string())])
    }

    #[test]
    fn declarations_are_split_and_classified() {
        let fp = sample();
        let defs: Vec<&String> = fp.definitions.keys().collect();
        assert_eq!(
            defs,
            vec![
                "Main.__aver_reason_and_quad_law_fourfold",
                "Main.double",
                "Main.evens",
                "Main.odds",
                "Main.quad",
            ],
            "{fp:?}"
        );
        let scripts: Vec<&String> = fp.scripts.keys().collect();
        assert_eq!(
            scripts,
            vec!["quad.fourfold", "quad.fourfold.implication"],
            "{fp:?}"
        );
    }

    #[test]
    fn cone_walks_through_obligations_into_definitions() {
        let fp = sample();
        let cone: Vec<String> = fp.cone("quad.fourfold").into_iter().collect();
        assert_eq!(cone, vec!["Main.double", "Main.quad"]);
        let cone: Vec<String> = fp.cone("quad.fourfold.implication").into_iter().collect();
        assert_eq!(cone, vec!["Main.double", "Main.quad"]);
        assert!(fp.cone("missing.law").is_empty());
    }

    #[test]
    fn compare_reports_own_script_and_changed_cone_definitions() {
        let before = sample();
        let previous = Recorded {
            definitions: before.definitions.clone(),
            scripts: before.scripts.clone(),
        };
        let changed = SAMPLE.replace("(n + n)", "(n + n + 1)");
        let after = Fingerprints::from_sources(&[("Main".to_string(), changed)]);
        assert_eq!(
            after.compare(&previous, "quad.fourfold"),
            Change {
                script: "same",
                definitions: vec!["Main.double".to_string()],
            }
        );
        assert_eq!(
            after.compare(&previous, "unknown.law"),
            Change {
                script: "new",
                definitions: Vec::new(),
            }
        );
        let rescripted = SAMPLE.replace("simp [quad, double]; omega", "omega");
        let after = Fingerprints::from_sources(&[("Main".to_string(), rescripted)]);
        let change = after.compare(&previous, "quad.fourfold.implication");
        assert_eq!(change.script, "changed");
        assert!(change.definitions.is_empty(), "{change:?}");
    }

    #[test]
    fn line_derived_hypothesis_names_do_not_move_the_hash() {
        let before = sample();
        let shifted = SAMPLE.replace("h_12", "h_13").replace("h_16", "h_17");
        let after = Fingerprints::from_sources(&[("Main".to_string(), shifted)]);
        assert_eq!(before.definitions, after.definitions);
        assert_eq!(
            normalize_line_names("match h_57 : steps with"),
            "match h_ : steps with"
        );
        assert_eq!(normalize_line_names("ah_5 h_x h_5x"), "ah_5 h_x h_5x");
    }

    #[test]
    fn recorded_tables_read_back_from_json() {
        let value = serde_json::json!({
            "definitions": {"Main.double": "aa"},
            "scripts": {"quad.fourfold": "bb"},
        });
        let recorded = recorded_from_json(&value);
        assert_eq!(recorded.definitions.get("Main.double").unwrap(), "aa");
        assert_eq!(recorded.scripts.get("quad.fourfold").unwrap(), "bb");
        assert!(
            recorded_from_json(&serde_json::json!({"laws": []}))
                .definitions
                .is_empty()
        );
    }
}
