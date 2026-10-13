//! Step proofs against the VM: a law the Aver kernel reports as closed by
//! steps must have no counterexample on the VM.
//!
//! The suite takes laws from the corpus, changes the source of one law at a
//! time (a mutant), and asks two questions about the changed program. Does
//! `aver verify` find a case where the law's two sides differ? Does
//! `aver proof --backend aver` report the law as closed by steps? A mutant
//! with both answers "yes" is a false law the kernel accepted, and fails the
//! suite. A mutant without a counterexample says nothing, since a change can
//! leave a law true. Lean takes no part.
//!
//! What a mutant changes:
//!
//! - the claim, one token at a time: a `when` line removed, an integer or
//!   boolean literal changed, an operator or a constructor swapped, a given
//!   replaced by another given of its type (in up to three places a law);
//! - nothing, except that every sampled given gets boundary values, which
//!   catches a false law the corpus only samples where it happens to hold;
//! - the context: a given renamed to a module-level binding or to the
//!   function the law is about, or a cited law swapped for another law over the same given
//!   types. Neither changes what the law says, so each is also combined with
//!   a claim change that has a counterexample.
//!
//! A counterexample is looked for with the law's own samples first, and then
//! with boundary values added to them. A mutant that does not compile is
//! left out.
//!
//! Environment, all optional:
//!
//! - `AVER_STEPS_DIFFERENTIAL=full` widens the corpus from the default subset
//!   to every file of `tools/steps-baseline.json`;
//! - `AVER_STEPS_DIFFERENTIAL_ONLY=<text>` keeps the corpus files whose path
//!   contains the text;
//! - `AVER_STEPS_DIFFERENTIAL_BIN=<path>` judges another `aver` binary;
//! - `AVER_STEPS_DIFFERENTIAL_LOG=<path>` writes one line per mutant.
//!
//! Run it with `--nocapture` to see the summary of a passing run.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, repo_root};

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

const FIXTURES: &str = "tests/fixtures/proof_steps";
const BASELINE: &str = "tools/steps-baseline.json";

/// The step fixtures about a given of a refined type, which live beside the
/// fixture directory.
const REFINED: &[&str] = &[
    "tests/fixtures/refined_givens.av",
    "tests/fixtures/refined_givens_open.av",
];

/// One process may take this long; a mutant that exceeds it is counted and
/// otherwise treated as having no answer.
const PROCESS_LIMIT: Duration = Duration::from_secs(30);

/// Examples with a law at the steps level that only the full run takes: one
/// `aver proof` of each takes a debug build over twenty seconds.
const SLOW: &[&str] = &[
    "examples/games/eggcatch/core.av",
    "examples/knowledge/knowledge.av",
    "examples/knowledge/stored.av",
];

/// A law is not given boundary samples past this many cases.
const CASE_LIMIT: usize = 2000;

/// In how many places of one law a given is replaced by another.
const GIVEN_SWAPS: usize = 3;

/// How many claim changes each context change is combined with.
const COMBINED: usize = 1;

/// False laws the kernel is known to close on this revision, as
/// `(file, law, mutant)`. Each needs a comment naming the report it is
/// tracked in. An entry that stops reproducing fails the suite, so the list
/// only shrinks.
const KNOWN: &[(&str, &str, &str)] = &[];

// ---------------------------------------------------------------- source ---

#[derive(Clone, Copy, PartialEq, Debug)]
enum Kind {
    Ident,
    Int,
    Op,
    Other,
}

#[derive(Clone, Copy, Debug)]
struct Token {
    kind: Kind,
    start: usize,
    end: usize,
}

const OPERATORS: &[&str] = &[
    "=>", "->", "==", "!=", "<=", ">=", "..", "<", ">", "+", "-", "*", "/",
];

/// The tokens of one source line. A dotted path (`Option.Some`, `p.x`) is
/// one identifier; strings and comments are skipped.
fn tokens(line: &str) -> Vec<Token> {
    let bytes = line.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b' ' || c == b'\t' {
            i += 1;
        } else if c == b'/' && bytes.get(i + 1) == Some(&b'/') {
            break;
        } else if c == b'"' {
            i += 1;
            while i < bytes.len() && bytes[i] != b'"' {
                i += if bytes[i] == b'\\' { 2 } else { 1 };
            }
            i += 1;
        } else if c.is_ascii_alphabetic() || c == b'_' {
            let start = i;
            loop {
                while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                    i += 1;
                }
                let dotted = bytes.get(i) == Some(&b'.')
                    && bytes.get(i + 1).is_some_and(|b| b.is_ascii_alphabetic());
                if !dotted {
                    break;
                }
                i += 1;
            }
            out.push(Token {
                kind: Kind::Ident,
                start,
                end: i,
            });
        } else if c.is_ascii_digit() {
            let start = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            let float =
                bytes.get(i) == Some(&b'.') && bytes.get(i + 1).is_some_and(|b| b.is_ascii_digit());
            if float {
                i += 1;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
            }
            out.push(Token {
                kind: if float { Kind::Other } else { Kind::Int },
                start,
                end: i,
            });
        } else if let Some(op) = OPERATORS.iter().find(|op| line[i..].starts_with(**op)) {
            out.push(Token {
                kind: Kind::Op,
                start: i,
                end: i + op.len(),
            });
            i += op.len();
        } else {
            let width = line[i..].chars().next().map_or(1, char::len_utf8);
            out.push(Token {
                kind: Kind::Other,
                start: i,
                end: i + width,
            });
            i += width;
        }
    }
    out
}

fn indent(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Line {
    Given,
    When,
    Using,
    Hint,
    Claim,
}

/// One `verify <subject> law <name>` block: its lines after the header, and
/// what each line is.
#[derive(Clone, Debug)]
struct Law {
    subject: String,
    name: String,
    /// Index in the file of the first body line.
    first: usize,
    body: Vec<String>,
    lines: Vec<Line>,
}

impl Law {
    fn key(&self) -> String {
        format!("{}.{}", self.subject, self.name)
    }

    fn block(&self) -> String {
        format!("{} law {}", self.subject, self.name)
    }

    /// `(name, type, value)` of a given line.
    fn given(line: &str) -> Option<(&str, &str, &str)> {
        let rest = line.trim().strip_prefix("given ")?;
        let (name, rest) = rest.split_once(": ")?;
        let (ty, value) = rest.split_once(" = ")?;
        Some((name.trim(), ty.trim(), value.trim()))
    }

    fn givens(&self) -> Vec<(&str, &str)> {
        self.body
            .iter()
            .zip(&self.lines)
            .filter(|(_, kind)| **kind == Line::Given)
            .filter_map(|(line, _)| Law::given(line))
            .map(|(name, ty, _)| (name, ty))
            .collect()
    }
}

/// The laws of a file this suite can read. A law with a part spread over
/// several lines before its claim is left out and counted.
fn laws(source: &[String]) -> (Vec<Law>, usize) {
    let mut found = Vec::new();
    let mut unread = 0;
    let mut i = 0;
    while i < source.len() {
        let words: Vec<&str> = source[i].split_whitespace().collect();
        let header = source[i].starts_with("verify ") && words.len() == 4 && words[2] == "law";
        i += 1;
        if !header {
            continue;
        }
        let first = i;
        while i < source.len() && (source[i].trim().is_empty() || source[i].starts_with(' ')) {
            i += 1;
        }
        let mut body: Vec<String> = source[first..i].to_vec();
        while body.last().is_some_and(|l| l.trim().is_empty()) {
            body.pop();
        }
        match classify(&body) {
            Some(lines) => found.push(Law {
                subject: words[1].to_string(),
                name: words[3].to_string(),
                first,
                body,
                lines,
            }),
            None => unread += 1,
        }
    }
    (found, unread)
}

fn classify(body: &[String]) -> Option<Vec<Line>> {
    let base = indent(body.first()?);
    let mut lines = Vec::new();
    let mut in_claim = false;
    for line in body {
        if in_claim {
            lines.push(Line::Claim);
            continue;
        }
        if line.trim().is_empty() || indent(line) != base {
            return None;
        }
        let kind = match line.split_whitespace().next()? {
            "given" => Line::Given,
            "when" => Line::When,
            "using" => Line::Using,
            "by" | "because" | "induction" | "where" | "plans" => Line::Hint,
            _ => Line::Claim,
        };
        if kind == Line::Given {
            let (_, _, value) = Law::given(line)?;
            if value.starts_with('[') && !balanced(value) {
                return None;
            }
        }
        in_claim = kind == Line::Claim;
        lines.push(kind);
    }
    in_claim.then_some(lines)
}

/// The names on the `using` lines of a law.
fn cites(body: &[String]) -> impl Iterator<Item = &str> {
    body.iter()
        .filter(|line| line.trim_start().starts_with("using "))
        .flat_map(|line| {
            tokens(line)
                .into_iter()
                .filter(|token| token.kind == Kind::Ident)
                .map(|token| &line[token.start..token.end])
        })
}

fn balanced(text: &str) -> bool {
    let mut depth = 0i32;
    for token in tokens(text) {
        match &text[token.start..token.end] {
            "[" | "(" | "{" => depth += 1,
            "]" | ")" | "}" => depth -= 1,
            _ => {}
        }
    }
    depth == 0
}

/// What a mutant needs to know about the rest of the file.
#[derive(Default)]
struct Module {
    /// Module-level bindings, `name: Type = value`.
    bindings: Vec<String>,
    functions: Vec<String>,
    /// Variants by sum type, each with its payload as written.
    sums: BTreeMap<String, Vec<(String, String)>>,
}

fn module(source: &[String]) -> Module {
    let mut found = Module::default();
    let mut i = 0;
    while i < source.len() {
        let line = &source[i];
        i += 1;
        if let Some(rest) = line.strip_prefix("fn ") {
            if let Some((name, _)) = rest.split_once('(') {
                found.functions.push(name.to_string());
            }
        } else if let Some(name) = line.strip_prefix("type ") {
            let mut variants = Vec::new();
            while i < source.len() && source[i].starts_with(' ') {
                let variant = source[i].trim();
                let (name, payload) = variant.split_once('(').unwrap_or((variant, ""));
                variants.push((name.to_string(), payload.to_string()));
                i += 1;
            }
            found.sums.insert(name.trim().to_string(), variants);
        } else if line.starts_with(|c: char| c.is_ascii_lowercase())
            && let Some((name, rest)) = line.split_once(": ")
            && name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
            && rest.contains(" = ")
        {
            found.bindings.push(name.to_string());
        }
    }
    found
}

// --------------------------------------------------------------- mutants ---

#[derive(Clone, Debug)]
enum Edit {
    /// Replace `start..end` of a body line.
    Replace {
        line: usize,
        start: usize,
        end: usize,
        text: String,
    },
    Remove {
        line: usize,
    },
    /// Rename an identifier wherever the law spells it.
    Rename {
        from: String,
        to: String,
    },
}

#[derive(Clone, Debug)]
struct Mutant {
    family: &'static str,
    what: String,
    edits: Vec<Edit>,
}

impl Mutant {
    fn then(&self, other: &Mutant) -> Mutant {
        Mutant {
            family: "combined",
            what: format!("{} + {}", self.what, other.what),
            edits: self.edits.iter().chain(&other.edits).cloned().collect(),
        }
    }
}

fn apply(body: &[String], edits: &[Edit]) -> Vec<String> {
    let mut lines: Vec<String> = body.to_vec();
    let mut replaces: Vec<(usize, usize, usize, &str)> = edits
        .iter()
        .filter_map(|edit| match edit {
            Edit::Replace {
                line,
                start,
                end,
                text,
            } => Some((*line, *start, *end, text.as_str())),
            _ => None,
        })
        .collect();
    replaces.sort_by(|a, b| b.cmp(a));
    for (line, start, end, text) in replaces {
        lines[line].replace_range(start..end, text);
    }
    for edit in edits {
        if let Edit::Rename { from, to } = edit {
            for line in &mut lines {
                *line = rename(line, from, to);
            }
        }
    }
    let removed: BTreeSet<usize> = edits
        .iter()
        .filter_map(|edit| match edit {
            Edit::Remove { line } => Some(*line),
            _ => None,
        })
        .collect();
    lines
        .into_iter()
        .enumerate()
        .filter(|(i, _)| !removed.contains(i))
        .map(|(_, line)| line)
        .collect()
}

/// Rename an identifier, also where it heads a dotted path (`p.x`).
fn rename(line: &str, from: &str, to: &str) -> String {
    let mut out = line.to_string();
    for token in tokens(line).iter().rev() {
        if token.kind != Kind::Ident {
            continue;
        }
        let text = &line[token.start..token.end];
        let head = text.split('.').next().unwrap_or(text);
        let field = token.start > 0 && line.as_bytes()[token.start - 1] == b'.';
        if head == from && !field {
            out.replace_range(token.start..token.start + head.len(), to);
        }
    }
    out
}

/// The end of the parenthesis opened at `open`, when it closes on this line.
fn closing(line: &str, open: usize) -> Option<usize> {
    let mut depth = 0;
    for (i, b) in line.bytes().enumerate().skip(open) {
        match b {
            b'(' => depth += 1,
            b')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i + 1);
                }
            }
            _ => {}
        }
    }
    None
}

/// Every change of one token of the law's claim and `when` lines.
fn claim_mutants(law: &Law, module: &Module) -> Vec<Mutant> {
    let mut out = Vec::new();
    let givens = law.givens();
    let mut swapped = 0;
    for (index, (line, kind)) in law.body.iter().zip(&law.lines).enumerate() {
        if !matches!(kind, Line::When | Line::Claim) {
            continue;
        }
        let at = law.first + index + 1;
        if *kind == Line::When {
            out.push(Mutant {
                family: "when removed",
                what: format!("line {at}: `{}` removed", line.trim()),
                edits: vec![Edit::Remove { line: index }],
            });
        }
        for token in tokens(line) {
            let text = &line[token.start..token.end];
            let mut swap = |family: &'static str, end: usize, to: String| {
                out.push(Mutant {
                    family,
                    what: format!(
                        "line {at} col {}: `{}` -> `{to}`",
                        token.start + 1,
                        &line[token.start..end]
                    ),
                    edits: vec![Edit::Replace {
                        line: index,
                        start: token.start,
                        end,
                        text: to,
                    }],
                });
            };
            match token.kind {
                Kind::Int => {
                    if let Ok(n) = text.parse::<i64>() {
                        swap("literal", token.end, (n + 1).to_string());
                    }
                }
                Kind::Op => {
                    let to = match text {
                        "+" => "-",
                        "-" => "+",
                        "*" => "+",
                        "<" => "<=",
                        "<=" => "<",
                        ">" => ">=",
                        ">=" => ">",
                        "==" => "!=",
                        "!=" => "==",
                        _ => continue,
                    };
                    swap("operator", token.end, to.to_string());
                }
                Kind::Ident => match text {
                    "true" => swap("literal", token.end, "false".to_string()),
                    "false" => swap("literal", token.end, "true".to_string()),
                    "Bool.and" => swap("operator", token.end, "Bool.or".to_string()),
                    "Bool.or" => swap("operator", token.end, "Bool.and".to_string()),
                    "Result.Ok" => swap("constructor", token.end, "Result.Err".to_string()),
                    "Result.Err" => swap("constructor", token.end, "Result.Ok".to_string()),
                    "Option.None" => swap("constructor", token.end, "Option.Some(0)".to_string()),
                    "Option.Some" => {
                        if let Some(end) = closing(line, token.end) {
                            swap("constructor", end, "Option.None".to_string());
                        }
                    }
                    _ => {
                        // A given, for the next given of its type.
                        if let Some(here) = givens.iter().position(|(name, _)| *name == text) {
                            let other = (1..givens.len())
                                .map(|step| givens[(here + step) % givens.len()])
                                .find(|(_, ty)| *ty == givens[here].1);
                            if let Some((other, _)) = other
                                && swapped < GIVEN_SWAPS
                            {
                                swapped += 1;
                                swap("given swapped", token.end, other.to_string());
                            }
                        }
                        let Some((ty, variant)) = text.split_once('.') else {
                            continue;
                        };
                        let Some(variants) = module.sums.get(ty) else {
                            continue;
                        };
                        let Some(here) = variants.iter().position(|(name, _)| name == variant)
                        else {
                            continue;
                        };
                        let other = (1..variants.len())
                            .map(|step| &variants[(here + step) % variants.len()])
                            .find(|(_, payload)| *payload == variants[here].1);
                        if let Some((other, _)) = other {
                            swap("constructor", token.end, format!("{ty}.{other}"));
                        }
                    }
                },
                Kind::Other => {}
            }
        }
    }
    out
}

/// Changes that leave the claim alone: a given under the name of a
/// module-level binding or of the function the law is about, and a cited law
/// swapped for another over the same given types.
fn context_mutants(law: &Law, module: &Module, all: &[Law]) -> Vec<Mutant> {
    let mut out = Vec::new();
    let givens = law.givens();
    let mut names: Vec<&String> = module.bindings.iter().collect();
    names.extend(module.functions.iter().filter(|f| **f == law.subject));
    for (given, _) in &givens {
        for name in &names {
            if givens.iter().any(|(other, _)| other == name) {
                continue;
            }
            out.push(Mutant {
                family: "given renamed",
                what: format!("given `{given}` renamed `{name}`"),
                edits: vec![Edit::Rename {
                    from: given.to_string(),
                    to: name.to_string(),
                }],
            });
        }
    }
    let types =
        |law: &Law| -> Vec<String> { law.givens().iter().map(|(_, ty)| ty.to_string()).collect() };
    for (index, (line, kind)) in law.body.iter().zip(&law.lines).enumerate() {
        if *kind != Line::Using {
            continue;
        }
        for token in tokens(line) {
            let text = &line[token.start..token.end];
            let Some(cited) = all.iter().find(|other| other.key() == text) else {
                continue;
            };
            let others = all.iter().filter(|other| {
                other.key() != text && other.key() != law.key() && types(other) == types(cited)
            });
            for other in others.take(2) {
                out.push(Mutant {
                    family: "citation swapped",
                    what: format!("cites `{}` for `{text}`", other.key()),
                    edits: vec![Edit::Replace {
                        line: index,
                        start: token.start,
                        end: token.end,
                        text: other.key(),
                    }],
                });
            }
        }
    }
    out
}

/// Boundary values of a type, as source text.
fn boundary(ty: &str) -> Vec<String> {
    let inner = |prefix: &str| {
        ty.strip_prefix(prefix)
            .and_then(|rest| rest.strip_suffix('>'))
    };
    if ty == "Int" {
        vec!["0".into(), "1".into(), "-1".into()]
    } else if ty == "Bool" {
        vec!["true".into(), "false".into()]
    } else if let Some(item) = inner("List<") {
        let mut out = vec!["[]".to_string()];
        out.extend(boundary(item).iter().map(|v| format!("[{v}]")));
        out
    } else if let Some(item) = inner("Option<") {
        let mut out = vec!["Option.None".to_string()];
        out.extend(boundary(item).iter().map(|v| format!("Option.Some({v})")));
        out
    } else {
        Vec::new()
    }
}

/// The items of a list literal, split at its top-level commas.
fn items(list: &str) -> Vec<String> {
    let inner = &list[1..list.len() - 1];
    let mut out = Vec::new();
    let (mut depth, mut start) = (0i32, 0);
    for token in tokens(inner) {
        match &inner[token.start..token.end] {
            "[" | "(" | "{" => depth += 1,
            "]" | ")" | "}" => depth -= 1,
            "," if depth == 0 => {
                out.push(inner[start..token.start].trim().to_string());
                start = token.end;
            }
            _ => {}
        }
    }
    if !inner[start..].trim().is_empty() {
        out.push(inner[start..].trim().to_string());
    }
    out
}

/// The law with boundary values added to the samples of each given, as long
/// as the cases stay under [`CASE_LIMIT`]. `None` when nothing was added.
fn with_boundaries(body: &[String]) -> Option<Vec<String>> {
    let mut samples: Vec<(usize, Vec<String>, Vec<String>)> = Vec::new();
    for (index, line) in body.iter().enumerate() {
        let Some((_, ty, value)) = Law::given(line) else {
            continue;
        };
        let have = if value.starts_with('[') && value.ends_with(']') {
            items(value)
        } else if let Some((low, high)) = value.split_once("..")
            && let (Ok(low), Ok(high)) = (low.parse::<i64>(), high.parse::<i64>())
            && high - low < 16
        {
            (low..=high).map(|n| n.to_string()).collect()
        } else {
            continue;
        };
        let extra: Vec<String> = boundary(ty)
            .into_iter()
            .filter(|v| !have.contains(v))
            .collect();
        samples.push((index, have, extra));
    }
    let cases = |samples: &[(usize, Vec<String>, Vec<String>)], grown: &[bool]| -> usize {
        samples
            .iter()
            .zip(grown)
            .map(|((_, have, extra), grown)| have.len() + if *grown { extra.len() } else { 0 })
            .product()
    };
    let mut grown = vec![false; samples.len()];
    for i in 0..samples.len() {
        grown[i] = !samples[i].2.is_empty();
        if cases(&samples, &grown) > CASE_LIMIT {
            grown[i] = false;
        }
    }
    if !grown.contains(&true) {
        return None;
    }
    let mut out = body.to_vec();
    for ((index, have, extra), grown) in samples.iter().zip(&grown) {
        if !grown {
            continue;
        }
        let line = &body[*index];
        let head = &line[..line.find(" = ").expect("a given has a value") + 3];
        let all: Vec<&str> = have.iter().chain(extra).map(String::as_str).collect();
        out[*index] = format!("{head}[{}]", all.join(", "));
    }
    Some(out)
}

// --------------------------------------------------------------- running ---

fn binary() -> String {
    std::env::var("AVER_STEPS_DIFFERENTIAL_BIN").unwrap_or_else(|_| aver_bin().to_string())
}

/// Standard output and whether the process succeeded; `None` on a timeout.
fn run(dir: &Path, args: &[&str]) -> Option<(String, bool)> {
    let mut child = Command::new(binary())
        .args(args)
        .current_dir(dir)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("aver runs");
    let mut stdout = child.stdout.take().expect("stdout is piped");
    let reader = std::thread::spawn(move || {
        let mut text = String::new();
        let _ = stdout.read_to_string(&mut text);
        text
    });
    let started = Instant::now();
    let status = loop {
        match child.try_wait().expect("aver is waited for") {
            Some(status) => break Some(status),
            None if started.elapsed() > PROCESS_LIMIT => {
                let _ = child.kill();
                let _ = child.wait();
                break None;
            }
            None => std::thread::sleep(Duration::from_millis(2)),
        }
    };
    let text = reader.join().expect("stdout is read");
    status.map(|status| (text, status.success()))
}

/// One corpus file and the directory its `depends` resolve from.
#[derive(Clone, Debug)]
struct Entry {
    path: String,
    root: String,
}

const PHASES: [&str; 3] = ["check", "verify", "proof"];

/// How many processes each phase ran, the time they took, and how many were
/// stopped at [`PROCESS_LIMIT`].
#[derive(Default)]
struct Clock {
    runs: [AtomicUsize; 3],
    millis: [AtomicUsize; 3],
    stopped: Mutex<Vec<String>>,
}

/// A worker's scratch directory, and the file it writes mutants to.
struct Bench<'a> {
    dir: &'a Path,
    entry: &'a Entry,
    source: &'a [String],
    laws: &'a [Law],
    clock: &'a Clock,
}

impl Bench<'_> {
    fn file(&self) -> &str {
        self.entry.path.rsplit('/').next().unwrap()
    }

    fn write(&self, law: &Law, body: &[String]) {
        let mut lines: Vec<&str> = self.source[..law.first]
            .iter()
            .map(String::as_str)
            .collect();
        lines.extend(body.iter().map(String::as_str));
        // On its own a law is helped only by the laws before it, and after
        // it by the ones a `using` line names. The other later laws are left
        // out, so each run has less to verify and prove.
        let mut cited: BTreeSet<&str> = cites(body).collect();
        let mut kept: Vec<bool> = self
            .laws
            .iter()
            .map(|other| other.first < law.first)
            .collect();
        loop {
            let before = cited.len();
            for (other, kept) in self.laws.iter().zip(&mut kept) {
                *kept = *kept || cited.contains(other.key().as_str());
                if *kept {
                    cited.extend(cites(&other.body));
                }
            }
            if cited.len() == before {
                break;
            }
        }
        let mut at = law.first + law.body.len();
        for (later, kept) in self.laws.iter().zip(&kept) {
            if later.first > law.first && !kept {
                lines.extend(self.source[at..later.first - 1].iter().map(String::as_str));
                at = later.first + later.body.len();
            }
        }
        lines.extend(self.source[at..].iter().map(String::as_str));
        fs::write(self.dir.join(self.file()), lines.join("\n")).expect("mutant is written");
    }

    fn aver(&self, args: &[&str]) -> Option<(String, bool)> {
        let root = repo_root().join(&self.entry.root);
        let mut all = args.to_vec();
        all.extend(["--module-root", root.to_str().unwrap()]);
        let started = Instant::now();
        let out = run(self.dir, &all);
        let phase = PHASES.iter().position(|p| *p == args[0]).expect("a phase");
        self.clock.runs[phase].fetch_add(1, Ordering::Relaxed);
        self.clock.millis[phase]
            .fetch_add(started.elapsed().as_millis() as usize, Ordering::Relaxed);
        if out.is_none() {
            let stopped = format!("aver {} of {}", args[0], self.entry.path);
            self.clock.stopped.lock().unwrap().push(stopped);
        }
        out
    }

    /// The errors `aver check` reports in the written file; `None` when it
    /// reports nothing about the file at all.
    fn errors(&self) -> Option<BTreeSet<String>> {
        let (out, _) = self.aver(&["check", self.file(), "--json"])?;
        let mut found = None;
        for line in out.lines().filter(|l| l.starts_with('{')) {
            let Ok(report) = serde_json::from_str::<serde_json::Value>(line) else {
                continue;
            };
            if report["kind"] != "analysis" || report["file_label"] != self.file() {
                continue;
            }
            let errors = found.get_or_insert_with(BTreeSet::new);
            for diagnostic in report["diagnostics"].as_array().into_iter().flatten() {
                if diagnostic["severity"] == "error" {
                    errors.insert(format!("{}: {}", diagnostic["slug"], diagnostic["summary"]));
                }
            }
        }
        found
    }

    /// Whether `aver check` has no error in the mutant that the unchanged law
    /// does not have: a fixture may carry findings of its own, such as a
    /// function without a verify block.
    fn checks(&self, law: &Law, body: &[String]) -> bool {
        self.write(law, &law.body);
        let accepted = self.errors().unwrap_or_default();
        self.write(law, body);
        self.errors()
            .is_some_and(|errors| errors.is_subset(&accepted))
    }

    /// What `aver verify` says of the law in the written file.
    fn verdict(&self, law: &Law) -> Verdict {
        let Some((out, _)) = self.aver(&["verify", self.file(), "-j", "1", "--json"]) else {
            return Verdict::Rejected;
        };
        let mut verdict = Verdict::Rejected;
        for line in out.lines().filter(|l| l.starts_with('{')) {
            let Ok(report) = serde_json::from_str::<serde_json::Value>(line) else {
                continue;
            };
            if report["kind"] != "analysis" || report["file_label"] != self.file() {
                continue;
            }
            verdict = Verdict::Holds;
            for diagnostic in report["diagnostics"].as_array().into_iter().flatten() {
                if diagnostic["slug"] != "verify-mismatch" {
                    continue;
                }
                let field = |name: &str| -> Option<String> {
                    diagnostic["fields"].as_array()?.iter().find_map(|pair| {
                        (pair[0] == name).then(|| pair[1].as_str().unwrap_or("").to_string())
                    })
                };
                if field("block").as_deref() == Some(law.block().as_str()) {
                    return Verdict::Fails(format!(
                        "{} (expected {}, got {})",
                        field("case").unwrap_or_default(),
                        field("expected").unwrap_or_default(),
                        field("actual").unwrap_or_default()
                    ));
                }
            }
        }
        verdict
    }

    /// Whether the Aver kernel reports the law as closed by steps.
    fn closed(&self, law: &Law) -> bool {
        let out_dir = self.dir.join("out");
        let _ = fs::remove_dir_all(&out_dir);
        let out = self.aver(&[
            "proof",
            self.file(),
            "--backend",
            "aver",
            "-o",
            out_dir.to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "1000000",
            "--declined-budget",
            "1000000",
        ]);
        let Some((out, _)) = out else {
            return false;
        };
        out.lines()
            .rev()
            .filter(|l| l.starts_with('{'))
            .find_map(|l| serde_json::from_str::<serde_json::Value>(l).ok())
            .is_some_and(|report| report["closed_by"][law.key()] == "steps")
    }
}

#[derive(PartialEq, Debug)]
enum Verdict {
    /// The file did not compile, so no case ran.
    Rejected,
    Holds,
    /// A case whose two sides the VM evaluates to different values.
    Fails(String),
}

#[derive(Clone, Debug)]
struct Outcome {
    file: String,
    law: String,
    family: &'static str,
    what: String,
    compiles: bool,
    counterexample: Option<String>,
    closed: bool,
    text: String,
}

impl Bench<'_> {
    /// Look for a counterexample with the law's own samples and then with
    /// boundary values added, and ask the kernel only about a mutant that has
    /// one. `aver verify` runs no case of a file that does not compile, so
    /// its answer also says whether the mutant compiles; a mutant the kernel
    /// closes is held to `aver check` as well before it counts.
    fn judge(&self, law: &Law, mutant: &Mutant) -> Outcome {
        let body = apply(&law.body, &mutant.edits);
        let mut outcome = Outcome {
            file: self.entry.path.clone(),
            law: law.key(),
            family: mutant.family,
            what: mutant.what.clone(),
            compiles: true,
            counterexample: None,
            closed: false,
            text: String::new(),
        };
        self.write(law, &body);
        let mut judged = body.clone();
        let mut verdict = self.verdict(law);
        if verdict == Verdict::Rejected {
            outcome.compiles = false;
            return outcome;
        }
        if verdict == Verdict::Holds
            && let Some(wider) = with_boundaries(&body)
        {
            self.write(law, &wider);
            verdict = self.verdict(law);
            judged = wider;
        }
        if let Verdict::Fails(case) = verdict {
            outcome.counterexample = Some(case);
            if self.closed(law) {
                outcome.compiles = self.checks(law, &judged);
                outcome.closed = outcome.compiles;
            }
            outcome.text = format!("verify {}\n{}", law.block(), judged.join("\n"));
        }
        outcome
    }

    fn law(&self, law: &Law, module: &Module, all: &[Law]) -> Vec<Outcome> {
        let unchanged = Mutant {
            family: "boundary samples",
            what: "unchanged, boundary samples added".to_string(),
            edits: Vec::new(),
        };
        let mut out = vec![self.judge(law, &unchanged)];
        // The claim changes a context change is combined with: the first
        // ones that have a counterexample, each of another kind.
        let mut claims: Vec<Mutant> = Vec::new();
        for mutant in claim_mutants(law, module) {
            let outcome = self.judge(law, &mutant);
            let fresh = claims.iter().all(|claim| claim.family != mutant.family);
            if outcome.counterexample.is_some() && claims.len() < COMBINED && fresh {
                claims.push(mutant);
            }
            out.push(outcome);
        }
        for context in context_mutants(law, module, all) {
            let alone = self.judge(law, &context);
            let compiles = alone.compiles;
            out.push(alone);
            if !compiles {
                continue;
            }
            for claim in &claims {
                out.push(self.judge(law, &context.then(claim)));
            }
        }
        out
    }
}

// ---------------------------------------------------------------- corpus ---

/// The directory a corpus file's `depends` resolve from, as the ratchet
/// (`tools/steps_ratchet.py`) has it.
fn module_root(path: &str) -> String {
    if path == "examples/refinement/natural_app.av" {
        return "examples".to_string();
    }
    let parts: Vec<&str> = path.split('/').collect();
    if parts[0] == "projects" {
        return parts[..2].join("/");
    }
    parts[..parts.len() - 1].join("/")
}

/// The step fixtures, and the files of `examples/` and `proof-corpus/` with
/// a law at the steps level; with `AVER_STEPS_DIFFERENTIAL=full`, every file
/// of the ratchet's baseline, which adds `projects/`, the [`SLOW`] examples
/// and the files where no law is at the steps level yet.
fn corpus() -> Vec<Entry> {
    let full = std::env::var("AVER_STEPS_DIFFERENTIAL").is_ok_and(|v| v == "full");
    let mut paths: BTreeSet<String> = REFINED.iter().map(|path| path.to_string()).collect();
    let fixtures = repo_root().join(FIXTURES);
    let mut dirs = vec![fixtures.clone()];
    while let Some(dir) = dirs.pop() {
        for entry in fs::read_dir(&dir).expect("fixtures are listed") {
            let path = entry.expect("a fixture").path();
            if path.is_dir() {
                dirs.push(path);
            } else if path.extension().is_some_and(|e| e == "av") {
                let relative = path
                    .strip_prefix(repo_root())
                    .expect("under the repository");
                paths.insert(relative.to_string_lossy().replace('\\', "/"));
            }
        }
    }
    let baseline: serde_json::Value = serde_json::from_str(
        &fs::read_to_string(repo_root().join(BASELINE)).expect("the baseline is read"),
    )
    .expect("the baseline is JSON");
    for (path, row) in baseline.as_object().expect("the baseline is a map") {
        let steps = row["steps"].as_array().is_some_and(|laws| !laws.is_empty());
        let small = !path.starts_with("projects/") && !SLOW.contains(&path.as_str());
        if full || (steps && small) {
            paths.insert(path.clone());
        }
    }
    let only = std::env::var("AVER_STEPS_DIFFERENTIAL_ONLY").unwrap_or_default();
    paths
        .into_iter()
        .filter(|path| path.contains(&only))
        .map(|path| Entry {
            root: module_root(&path),
            path,
        })
        .collect()
}

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!("aver-steps-differential-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("scratch directory");
    dir
}

#[derive(Default)]
struct Tally {
    mutants: usize,
    compiled: usize,
    counterexamples: usize,
    closed: usize,
}

#[test]
fn no_law_with_a_vm_counterexample_is_closed_by_steps() {
    let started = Instant::now();
    let narrowed = std::env::var("AVER_STEPS_DIFFERENTIAL_ONLY").is_ok();
    let entries = corpus();
    let sources: Vec<Vec<String>> = entries
        .iter()
        .map(|entry| {
            fs::read_to_string(repo_root().join(&entry.path))
                .unwrap_or_else(|e| panic!("{}: {e}", entry.path))
                .lines()
                .map(str::to_string)
                .collect()
        })
        .collect();
    let mut jobs: Vec<(usize, usize)> = Vec::new();
    let mut parsed = Vec::new();
    let mut unread = 0;
    for (file, source) in sources.iter().enumerate() {
        let (found, skipped) = laws(source);
        unread += skipped;
        jobs.extend((0..found.len()).map(|law| (file, law)));
        parsed.push((module(source), found));
    }

    let root = scratch();
    let next = AtomicUsize::new(0);
    let clock = Clock::default();
    let outcomes: Mutex<Vec<Outcome>> = Mutex::new(Vec::new());
    let workers = std::thread::available_parallelism().map_or(2, |n| n.get());
    std::thread::scope(|scope| {
        for worker in 0..workers {
            let (jobs, entries, sources, parsed) = (&jobs, &entries, &sources, &parsed);
            let (next, clock, outcomes, root) = (&next, &clock, &outcomes, &root);
            scope.spawn(move || {
                let dir = root.join(worker.to_string());
                fs::create_dir_all(&dir).expect("worker directory");
                while let Some(&(file, law)) = jobs.get(next.fetch_add(1, Ordering::Relaxed)) {
                    let (module, laws) = &parsed[file];
                    let bench = Bench {
                        dir: &dir,
                        entry: &entries[file],
                        source: &sources[file],
                        laws,
                        clock,
                    };
                    let found = bench.law(&laws[law], module, laws);
                    let _ = fs::remove_file(dir.join(bench.file()));
                    outcomes.lock().unwrap().extend(found);
                }
            });
        }
    });
    let _ = fs::remove_dir_all(&root);

    let mut outcomes = outcomes.into_inner().unwrap();
    outcomes.sort_by(|a, b| (&a.file, &a.law, &a.what).cmp(&(&b.file, &b.law, &b.what)));
    let mut families: BTreeMap<&str, Tally> = BTreeMap::new();
    let mut total = Tally::default();
    for outcome in &outcomes {
        for tally in [&mut total, families.entry(outcome.family).or_default()] {
            tally.mutants += 1;
            tally.compiled += usize::from(outcome.compiles);
            tally.counterexamples += usize::from(outcome.counterexample.is_some());
            tally.closed += usize::from(outcome.closed);
        }
    }
    if let Ok(path) = std::env::var("AVER_STEPS_DIFFERENTIAL_LOG") {
        let rows: Vec<String> = outcomes
            .iter()
            .map(|o| {
                format!(
                    "{}\t{}\t{}\t{}\t{}\t{}\t{}",
                    o.file,
                    o.law,
                    o.family,
                    o.what,
                    if o.compiles { "compiles" } else { "rejected" },
                    o.counterexample.as_deref().unwrap_or("-"),
                    if o.closed { "CLOSED" } else { "-" }
                )
            })
            .collect();
        fs::write(&path, rows.join("\n") + "\n").expect("the log is written");
    }
    let mut stopped: BTreeMap<String, usize> = BTreeMap::new();
    for what in clock.stopped.lock().unwrap().drain(..) {
        *stopped.entry(what).or_default() += 1;
    }
    let known = |o: &Outcome| KNOWN.contains(&(o.file.as_str(), o.law.as_str(), o.what.as_str()));
    let closed: Vec<&Outcome> = outcomes.iter().filter(|o| o.closed).collect();
    let unexpected: Vec<&&Outcome> = closed.iter().filter(|o| !known(o)).collect();
    let stale: Vec<&(&str, &str, &str)> = KNOWN
        .iter()
        .filter(|k| {
            !closed
                .iter()
                .any(|o| (o.file.as_str(), o.law.as_str(), o.what.as_str()) == **k)
        })
        .collect();

    let mut report = String::new();
    let _ = writeln!(
        report,
        "step proofs against the VM: {} files, {} laws ({} not read), {} workers, {:.1} s",
        entries.len(),
        jobs.len(),
        unread,
        workers,
        started.elapsed().as_secs_f64()
    );
    let _ = writeln!(
        report,
        "  {:<18} {:>8} {:>9} {:>15} {:>16}",
        "mutant", "mutants", "compile", "counterexample", "closed by steps"
    );
    for (family, tally) in families.iter().chain([(&"total", &total)]) {
        let _ = writeln!(
            report,
            "  {:<18} {:>8} {:>9} {:>15} {:>16}",
            family, tally.mutants, tally.compiled, tally.counterexamples, tally.closed
        );
    }
    let _ = writeln!(
        report,
        "  timeouts: {}; known: {}; unexpected: {}",
        stopped.values().sum::<usize>(),
        closed.len() - unexpected.len(),
        unexpected.len()
    );
    for (what, times) in &stopped {
        let _ = writeln!(
            report,
            "  stopped after {} s: {what}, {times} time(s)",
            PROCESS_LIMIT.as_secs()
        );
    }
    for (phase, name) in PHASES.iter().enumerate() {
        let _ = writeln!(
            report,
            "  aver {name}: {} runs, {:.1} s",
            clock.runs[phase].load(Ordering::Relaxed),
            clock.millis[phase].load(Ordering::Relaxed) as f64 / 1000.0
        );
    }
    for outcome in &closed {
        let _ = writeln!(
            report,
            "\nCLOSED BY STEPS WITH A VM COUNTEREXAMPLE{}\n  file:   {}\n  law:    {}\n  mutant: {}\n  case:   {}\n{}",
            if known(outcome) { " (known)" } else { "" },
            outcome.file,
            outcome.law,
            outcome.what,
            outcome.counterexample.as_deref().unwrap_or(""),
            outcome.text
        );
    }

    print!("{report}");
    // A passing run prints nothing under `cargo test`, so on GitHub the
    // summary also goes to the job's page.
    if let Ok(path) = std::env::var("GITHUB_STEP_SUMMARY")
        && let Ok(mut page) = fs::OpenOptions::new().append(true).create(true).open(path)
    {
        use std::io::Write;
        let _ = write!(page, "```\n{report}```\n");
    }

    assert!(
        unexpected.is_empty(),
        "{} law(s) with a VM counterexample are closed by steps; see the output above",
        unexpected.len()
    );
    assert!(
        narrowed || stale.is_empty(),
        "known entries that no longer reproduce: {stale:?}"
    );
    // The suite must not pass by finding nothing to compare.
    assert!(
        narrowed || total.counterexamples >= 500,
        "only {} mutants had a VM counterexample",
        total.counterexamples
    );
}

#[test]
fn the_mutations_are_the_ones_described() {
    let source: Vec<String> = [
        "n: Int = 7",
        "",
        "type Nat",
        "    Z",
        "    S(Nat)",
        "",
        "fn f(x: Int) -> Int",
        "    x",
        "",
        "verify f law a",
        "    given x: Int = [7]",
        "    given w: Int = [2]",
        "    when x < 3",
        "    using [f.b]",
        "    f(x) + 1 => Option.withDefault(Option.Some(w), 0)",
        "",
        "verify f law b",
        "    given y: Int = 0..2",
        "    given z: Option<Int> = [Option.None]",
        "    f(y) => y",
        "",
        "verify f law c",
        "    given y: Int = [1]",
        "    given z: Option<Int> = [Option.None]",
        "    f(y) => y",
    ]
    .iter()
    .map(|l| l.to_string())
    .collect();
    let (found, unread) = laws(&source);
    assert_eq!((found.len(), unread), (3, 0));
    let module = module(&source);
    assert_eq!(module.bindings, ["n"]);
    assert_eq!(module.functions, ["f"]);
    let law = &found[0];
    // What follows the two given lines, with ` | ` between the lines.
    let bodies = |mutants: Vec<Mutant>| -> Vec<String> {
        mutants
            .iter()
            .map(|m| {
                apply(&law.body, &m.edits)[2..]
                    .join(" | ")
                    .replace("    ", "")
            })
            .collect()
    };
    assert_eq!(
        bodies(claim_mutants(law, &module)),
        [
            "using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(w), 0)",
            "when w < 3 | using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(w), 0)",
            "when x <= 3 | using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(w), 0)",
            "when x < 4 | using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(w), 0)",
            "when x < 3 | using [f.b] | f(w) + 1 => Option.withDefault(Option.Some(w), 0)",
            "when x < 3 | using [f.b] | f(x) - 1 => Option.withDefault(Option.Some(w), 0)",
            "when x < 3 | using [f.b] | f(x) + 2 => Option.withDefault(Option.Some(w), 0)",
            "when x < 3 | using [f.b] | f(x) + 1 => Option.withDefault(Option.None, 0)",
            "when x < 3 | using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(x), 0)",
            "when x < 3 | using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(w), 1)",
        ]
    );
    assert_eq!(
        bodies(context_mutants(law, &module, &found)),
        [
            "when n < 3 | using [f.b] | f(n) + 1 => Option.withDefault(Option.Some(w), 0)",
            "when f < 3 | using [f.b] | f(f) + 1 => Option.withDefault(Option.Some(w), 0)",
            "when x < 3 | using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(n), 0)",
            "when x < 3 | using [f.b] | f(x) + 1 => Option.withDefault(Option.Some(f), 0)",
            "when x < 3 | using [f.c] | f(x) + 1 => Option.withDefault(Option.Some(w), 0)",
        ]
    );
    assert_eq!(
        with_boundaries(&found[1].body).unwrap()[..2],
        [
            "    given y: Int = [0, 1, 2, -1]",
            "    given z: Option<Int> = [Option.None, Option.Some(0), Option.Some(1), Option.Some(-1)]",
        ]
    );
    assert_eq!(boundary("List<Bool>"), ["[]", "[true]", "[false]"]);
    assert_eq!(
        rename("p.x + Pt.make(q.p, p) => \"p\"", "p", "n"),
        "n.x + Pt.make(q.p, n) => \"p\""
    );
}
