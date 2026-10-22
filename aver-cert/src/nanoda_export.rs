//! The export the nanoda kernel reads under `--paranoid`: the witness closure
//! without the declarations that depend on `sorryAx`.
//!
//! nanoda is told only Lean's standard axioms are permitted (the list stays
//! as it is), leaves any other axiom out of its environment and stops on the
//! first declaration that uses it. A certificate may carry theorems that end
//! in `sorry` on purpose. Those are never credited: the checker's audit names
//! `sorryAx` among their axioms and reports the claim as not credited. nanoda
//! is given the export without them and without everything that depends on
//! them, so it still checks every declaration a credited claim rests on;
//! every other kernel reads the whole export.
//!
//! "Depends" is the closure the audit uses: a constant named anywhere in a
//! declaration's type or value (for an inductive: its types, constructors,
//! recursors and their rules). The caller refuses the filter when it would
//! drop any declaration it was told must stay.

use serde_json::Value;

/// The axiom Lean elaborates `sorry` to.
const SORRY_AXIOM: &str = "sorryAx";

/// The export with the `sorryAx`-dependent declarations left out, and the
/// names of the declarations left out (`sorryAx` itself included).
#[derive(Debug)]
pub(crate) struct Filtered {
    pub export: Vec<u8>,
    pub dropped: Vec<String>,
}

/// One expression: its sub-expressions and the constant it names.
type Node = (Vec<usize>, Option<usize>);

/// One declaration line: the names it adds and the expressions it is built from.
struct Declaration {
    line: usize,
    names: Vec<usize>,
    roots: Vec<usize>,
}

/// `None` when no declaration depends on `sorryAx`: the export is then
/// nanoda's as it is (nanoda leaves the unused axiom out by itself). An
/// export line this reader does not know is an error, never a skip: a
/// misread could drop a declaration nanoda must check.
pub(crate) fn without_sorry(export: &[u8]) -> Result<Option<Filtered>, String> {
    let export = std::str::from_utf8(export).map_err(|error| format!("export: {error}"))?;
    if !export.contains(&format!("\"{SORRY_AXIOM}\"")) {
        return Ok(None);
    }
    let lines: Vec<&str> = export.split('\n').filter(|line| !line.is_empty()).collect();
    // Name ids to (prefix, component); expression ids to their sub-expressions
    // and the constant they name.
    let mut names: Vec<Option<(usize, String)>> = Vec::new();
    let mut expressions: Vec<Option<Node>> = Vec::new();
    let mut declarations = Vec::new();
    for (at, line) in lines.iter().enumerate() {
        let bad = |what: &str| format!("export line {}: {what}", at + 1);
        if let Some((id, node)) = quick_expression(line) {
            if node.0.iter().any(|child| *child >= id) {
                return Err(bad("expression names a later expression"));
            }
            put(&mut expressions, id, node).map_err(|_| bad("expression id repeated"))?;
            continue;
        }
        let entry: Value = serde_json::from_str(line)
            .map_err(|error| format!("export line {}: {error}", at + 1))?;
        let entry = entry.as_object().ok_or_else(|| bad("not an object"))?;
        if let Some(id) = entry.get("in") {
            let id = index(id).ok_or_else(|| bad("bad name id"))?;
            let (pre, component) = if let Some(part) = entry.get("str") {
                (
                    part.get("pre"),
                    part.get("str").and_then(Value::as_str).map(str::to_string),
                )
            } else if let Some(part) = entry.get("num") {
                (part.get("pre"), part.get("i").map(Value::to_string))
            } else {
                return Err(bad("unknown name form"));
            };
            let pre = pre.and_then(index).ok_or_else(|| bad("bad name prefix"))?;
            let component = component.ok_or_else(|| bad("bad name component"))?;
            put(&mut names, id, (pre, component)).map_err(|_| bad("name id repeated"))?;
        } else if let Some(id) = entry.get("ie") {
            let id = index(id).ok_or_else(|| bad("bad expression id"))?;
            let node = expression(entry).ok_or_else(|| bad("unknown expression form"))?;
            if node.0.iter().any(|child| *child >= id) {
                return Err(bad("expression names a later expression"));
            }
            put(&mut expressions, id, node).map_err(|_| bad("expression id repeated"))?;
        } else if entry.contains_key("il") || entry.contains_key("meta") {
        } else {
            let (names, roots) = declaration(entry).ok_or_else(|| bad("unknown entry"))?;
            declarations.push(Declaration {
                line: at,
                names,
                roots,
            });
        }
    }
    let mut tainted = vec![false; names.len()];
    for (id, name) in names.iter().enumerate() {
        if matches!(name, Some((0, component)) if component == SORRY_AXIOM) {
            tainted[id] = true;
        }
    }
    let mut dropped = vec![false; declarations.len()];
    loop {
        let mut reaches = vec![false; expressions.len()];
        for id in 0..expressions.len() {
            if let Some((children, constant)) = &expressions[id] {
                reaches[id] = constant.is_some_and(|name| tainted.get(name) == Some(&true))
                    || children.iter().any(|child| reaches[*child]);
            }
        }
        let mut changed = false;
        for (at, declaration) in declarations.iter().enumerate() {
            if dropped[at] {
                continue;
            }
            let uses = declaration
                .roots
                .iter()
                .any(|root| reaches.get(*root) == Some(&true));
            let is = declaration
                .names
                .iter()
                .any(|name| tainted.get(*name) == Some(&true));
            if uses || is {
                dropped[at] = true;
                changed = true;
                for name in &declaration.names {
                    if let Some(slot) = tainted.get_mut(*name) {
                        *slot = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }
    let mut skip = vec![false; lines.len()];
    let mut dropped_names = Vec::new();
    for (at, declaration) in declarations.iter().enumerate() {
        if dropped[at] {
            skip[declaration.line] = true;
            dropped_names.extend(declaration.names.iter().map(|name| render(&names, *name)));
        }
    }
    // Init declares `sorryAx`, so every export has it.
    if dropped_names.len() == 1 {
        return Ok(None);
    }
    let mut filtered = Vec::with_capacity(export.len());
    for (at, line) in lines.iter().enumerate() {
        if !skip[at] {
            filtered.extend_from_slice(line.as_bytes());
            filtered.push(b'\n');
        }
    }
    Ok(Some(Filtered {
        export: filtered,
        dropped: dropped_names,
    }))
}

fn index(value: &Value) -> Option<usize> {
    value.as_u64().and_then(|id| usize::try_from(id).ok())
}

fn put<T>(table: &mut Vec<Option<T>>, id: usize, value: T) -> Result<(), ()> {
    if table.len() <= id {
        table.resize_with(id + 1, || None);
    }
    if table[id].is_some() {
        return Err(());
    }
    table[id] = Some(value);
    Ok(())
}

/// The most common expression lines (applications, binders, constants, bound
/// variables, sorts) read in one pass over their bytes, without a JSON parse.
/// Lean writes object keys sorted, so each such form has one exact sequence of
/// keys; a line with any other sequence goes to the full parse (`None`).
fn quick_expression(line: &str) -> Option<(usize, Node)> {
    // Each key and the unsigned integer right after it, if any.
    let bytes = line.as_bytes();
    let mut keys: [(&[u8], Option<usize>); 6] = [(&[], None); 6];
    let mut count = 0;
    let mut at = 0;
    while at < bytes.len() {
        if bytes[at] != b'"' {
            at += 1;
            continue;
        }
        let start = at + 1;
        at = start;
        while at < bytes.len() && bytes[at] != b'"' {
            if bytes[at] == b'\\' {
                return None;
            }
            at += 1;
        }
        let text = bytes.get(start..at)?;
        at += 1;
        if at >= bytes.len() || bytes[at] != b':' {
            continue;
        }
        at += 1;
        let mut value = None;
        while at < bytes.len() && bytes[at].is_ascii_digit() {
            let digit = usize::from(bytes[at] - b'0');
            value = Some(
                value
                    .unwrap_or(0usize)
                    .checked_mul(10)?
                    .checked_add(digit)?,
            );
            at += 1;
        }
        if count == keys.len() {
            return None;
        }
        keys[count] = (text, value);
        count += 1;
    }
    let keys = &keys[..count];
    let is = |form: &[&[u8]]| {
        keys.len() == form.len() && keys.iter().zip(form).all(|((key, _), want)| key == want)
    };
    let value = |at: usize| keys[at].1;
    if is(&[b"app", b"arg", b"fn", b"ie"]) {
        Some((value(3)?, (vec![value(2)?, value(1)?], None)))
    } else if is(&[b"bvar", b"ie"]) {
        Some((value(1)?, (Vec::new(), None)))
    } else if is(&[b"const", b"name", b"us", b"ie"]) {
        Some((value(3)?, (Vec::new(), Some(value(1)?))))
    } else if is(&[b"forallE", b"binderInfo", b"body", b"name", b"type", b"ie"]) {
        Some((value(5)?, (vec![value(4)?, value(2)?], None)))
    } else if is(&[b"ie", b"lam", b"binderInfo", b"body", b"name", b"type"]) {
        Some((value(0)?, (vec![value(5)?, value(3)?], None)))
    } else if is(&[b"ie", b"sort"]) {
        Some((value(0)?, (Vec::new(), None)))
    } else {
        None
    }
}

/// The sub-expressions of one expression line and the constant it names.
fn expression(entry: &serde_json::Map<String, Value>) -> Option<Node> {
    let fields = |body: &Value, keys: &[&str]| -> Option<Vec<usize>> {
        keys.iter()
            .map(|key| body.get(*key).and_then(index))
            .collect()
    };
    for leaf in ["bvar", "sort", "natVal", "strVal"] {
        if entry.contains_key(leaf) {
            return Some((Vec::new(), None));
        }
    }
    if let Some(body) = entry.get("const") {
        return Some((Vec::new(), Some(body.get("name").and_then(index)?)));
    }
    if let Some(body) = entry.get("app") {
        return Some((fields(body, &["fn", "arg"])?, None));
    }
    for binder in ["lam", "forallE"] {
        if let Some(body) = entry.get(binder) {
            return Some((fields(body, &["type", "body"])?, None));
        }
    }
    if let Some(body) = entry.get("letE") {
        return Some((fields(body, &["type", "value", "body"])?, None));
    }
    if let Some(body) = entry.get("mdata") {
        return Some((fields(body, &["expr"])?, None));
    }
    if let Some(body) = entry.get("proj") {
        return Some((
            fields(body, &["struct"])?,
            Some(body.get("typeName").and_then(index)?),
        ));
    }
    None
}

/// The names a declaration line adds and the expressions it is built from.
fn declaration(entry: &serde_json::Map<String, Value>) -> Option<(Vec<usize>, Vec<usize>)> {
    let one = |body: &Value, keys: &[&str]| -> Option<(Vec<usize>, Vec<usize>)> {
        let name = body.get("name").and_then(index)?;
        let roots = keys
            .iter()
            .map(|key| body.get(*key).and_then(index))
            .collect::<Option<Vec<_>>>()?;
        Some((vec![name], roots))
    };
    for kind in ["thm", "def", "opaque"] {
        if let Some(body) = entry.get(kind) {
            return one(body, &["type", "value"]);
        }
    }
    for kind in ["axiom", "quot"] {
        if let Some(body) = entry.get(kind) {
            return one(body, &["type"]);
        }
    }
    let body = entry.get("inductive")?;
    let (mut names, mut roots) = (Vec::new(), Vec::new());
    for group in ["types", "ctors", "recs"] {
        for part in body.get(group)?.as_array()? {
            let (name, root) = one(part, &["type"])?;
            names.extend(name);
            roots.extend(root);
            if group == "recs" {
                for rule in part.get("rules")?.as_array()? {
                    roots.push(rule.get("rhs").and_then(index)?);
                }
            }
        }
    }
    Some((names, roots))
}

fn render(names: &[Option<(usize, String)>], id: usize) -> String {
    let mut parts = Vec::new();
    let mut at = id;
    while let Some(Some((pre, component))) = names.get(at) {
        parts.push(component.as_str());
        at = *pre;
    }
    parts.reverse();
    parts.join(".")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `Nat` and `Nat.zero` as an inductive, an axiom `sorryAx`, a theorem
    /// `bad` proved by it, a theorem `uses_bad` citing `bad`, and a theorem
    /// `good` that cites only `Nat.zero`.
    const EXPORT: &str = r#"{"meta":{"exporter":{"name":"lean4export","version":"3.1.0"}}}
{"in":1,"str":{"pre":0,"str":"Nat"}}
{"in":2,"str":{"pre":1,"str":"zero"}}
{"in":3,"str":{"pre":0,"str":"sorryAx"}}
{"in":4,"str":{"pre":0,"str":"bad"}}
{"in":5,"str":{"pre":0,"str":"uses_bad"}}
{"in":6,"str":{"pre":0,"str":"good"}}
{"in":7,"num":{"i":0,"pre":6}}
{"il":1,"succ":0}
{"ie":0,"sort":1}
{"const":{"name":1,"us":[]},"ie":1}
{"const":{"name":2,"us":[]},"ie":2}
{"inductive":{"ctors":[{"name":2,"type":1}],"recs":[],"types":[{"name":1,"type":0}]}}
{"const":{"name":3,"us":[]},"ie":3}
{"axiom":{"isUnsafe":false,"levelParams":[],"name":3,"type":0}}
{"app":{"arg":1,"fn":3},"ie":4}
{"thm":{"all":[4],"levelParams":[],"name":4,"type":1,"value":4}}
{"const":{"name":4,"us":[]},"ie":5}
{"thm":{"all":[5],"levelParams":[],"name":5,"type":1,"value":5}}
{"thm":{"all":[6],"levelParams":[],"name":6,"type":1,"value":2}}
{"ie":6,"lam":{"binderInfo":"default","body":2,"name":7,"type":1}}
{"thm":{"all":[7],"levelParams":[],"name":7,"type":1,"value":6}}
"#;

    fn filtered() -> Filtered {
        without_sorry(EXPORT.as_bytes())
            .unwrap()
            .expect("export names sorryAx")
    }

    #[test]
    fn drops_sorry_and_everything_that_cites_it() {
        let filtered = filtered();
        assert_eq!(filtered.dropped, ["sorryAx", "bad", "uses_bad"]);
        let text = String::from_utf8(filtered.export).unwrap();
        assert!(!text.contains(r#"{"axiom""#));
        assert!(!text.contains(r#""name":4,"type":1"#));
        assert!(!text.contains(r#""name":5,"type":1"#));
    }

    #[test]
    fn keeps_every_declaration_that_does_not_depend_on_sorry() {
        let text = String::from_utf8(filtered().export).unwrap();
        assert!(
            text.contains(r#"{"thm":{"all":[6],"levelParams":[],"name":6,"type":1,"value":2}}"#)
        );
        assert!(
            text.contains(r#"{"thm":{"all":[7],"levelParams":[],"name":7,"type":1,"value":6}}"#)
        );
        assert!(text.contains(r#"{"inductive":"#));
        // Names and expressions all stay; only declaration lines go.
        assert_eq!(text.lines().count(), EXPORT.lines().count() - 3);
    }

    #[test]
    fn the_quick_read_agrees_with_the_full_parse() {
        let mut quick = 0;
        for line in EXPORT.lines() {
            if let Some((id, node)) = quick_expression(line) {
                let entry: Value = serde_json::from_str(line).unwrap();
                let entry = entry.as_object().unwrap();
                assert_eq!(index(&entry["ie"]), Some(id));
                assert_eq!(expression(entry), Some(node), "{line}");
                quick += 1;
            }
        }
        assert_eq!(quick, 7);
        let mdata = r#"{"ie":9,"mdata":{"data":{"k":{"name":3}},"expr":2}}"#;
        assert!(quick_expression(mdata).is_none());
        let twice = r#"{"app":{"arg":1,"fn":2,"fn":3},"ie":9}"#;
        assert!(quick_expression(twice).is_none());
    }

    #[test]
    fn an_export_without_sorry_is_left_alone() {
        let export = EXPORT.replace("sorryAx", "trustMe");
        assert!(without_sorry(export.as_bytes()).unwrap().is_none());
        let export = EXPORT.replace("\"sorryAx\"", "\"sorryAx2\"");
        assert!(without_sorry(export.as_bytes()).unwrap().is_none());
        // `sorryAx` declared but used by nothing, as in every export of Init.
        let export: String = (EXPORT.lines())
            .filter(|line| !line.contains(r#""name":4,"type":1"#))
            .filter(|line| !line.contains(r#""name":5,"type":1"#))
            .map(|line| format!("{line}\n"))
            .collect();
        assert!(without_sorry(export.as_bytes()).unwrap().is_none());
    }

    #[test]
    fn an_unknown_line_is_an_error_not_a_skip() {
        let export = format!("{EXPORT}{{\"ie\":7,\"future\":{{\"expr\":3}}}}\n");
        assert!(without_sorry(export.as_bytes()).is_err());
        let export = format!("{EXPORT}{{\"theorem2\":{{\"name\":6}}}}\n");
        assert!(without_sorry(export.as_bytes()).is_err());
    }
}
