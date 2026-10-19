//! Shared parts of a step script. A part the script writes more than once
//! is written once, under a name, and every other place holds the name:
//!
//! ```text
//! (shared ((#0 PART) (#1 PART)…) SCRIPT)
//! ```
//!
//! A part may use the names before it. The reader writes every name out
//! again before anything else sees the data (`readAll` in
//! `tools/proof-kernel/kernel/sexp.av`, [`expand`] here), so the kernel
//! checks exactly the script written out: this is a shorthand of the text,
//! not a rule. No name is a name of the program: no identifier starts with
//! `#`.

use std::collections::HashMap;

use super::read::Sx;

/// A part shorter than this is written where it stands, however often.
const SHARED_MIN_LEN: usize = 24;

/// One S-expression of the text: where it is, which distinct part it is,
/// and the S-expressions of a list.
struct Node {
    start: usize,
    end: usize,
    part: usize,
    items: Vec<usize>,
}

#[derive(PartialEq, Eq, Hash)]
enum Key<'a> {
    Leaf(&'a str),
    List(Vec<usize>),
}

/// The script text with every part written at least twice and at least
/// [`SHARED_MIN_LEN`] long named once. A text with nothing to share, or
/// one it cannot read, comes back as it is.
pub fn share(text: &str) -> String {
    let Some((nodes, parts, root)) = read(text, false) else {
        return text.to_string();
    };
    // The first place each part is written, and how long it is there.
    let mut first = vec![usize::MAX; parts];
    for (i, n) in nodes.iter().enumerate() {
        if first[n.part] == usize::MAX {
            first[n.part] = i;
        }
    }
    let len = |p: usize| nodes[first[p]].end - nodes[first[p]].start;
    // How often each part is written once the longer parts are shared:
    // a part inside a shared one is written once for all of its places.
    // A part is longer than every part inside it, so the longest go first.
    let mut by_len: Vec<usize> = (0..parts).collect();
    by_len.sort_by_key(|&p| std::cmp::Reverse(len(p)));
    let mut written = vec![0usize; parts];
    written[nodes[root].part] = 1;
    let mut shared = vec![false; parts];
    for &p in &by_len {
        let times = written[p];
        if times == 0 {
            continue;
        }
        shared[p] = p != nodes[root].part && times >= 2 && len(p) >= SHARED_MIN_LEN;
        let each = if shared[p] { 1 } else { times };
        for &item in &nodes[first[p]].items {
            written[nodes[item].part] += each;
        }
    }
    // A part's name comes after the names of the parts inside it.
    let mut names: Vec<Option<String>> = vec![None; parts];
    let mut order: Vec<usize> = (0..parts).filter(|&p| shared[p]).collect();
    if order.is_empty() {
        return text.to_string();
    }
    order.sort_by_key(|&p| (len(p), p));
    for (k, &p) in order.iter().enumerate() {
        names[p] = Some(format!("#{k}"));
    }
    let r = &nodes[root];
    let mut out = String::with_capacity(text.len());
    out.push_str(&text[..r.start]);
    out.push_str("(shared (");
    for (k, &p) in order.iter().enumerate() {
        out.push_str(if k == 0 { "(" } else { "\n (" });
        out.push_str(names[p].as_deref().unwrap_or_default());
        out.push(' ');
        write(text, &nodes, &names, first[p], true, &mut out);
        out.push(')');
    }
    out.push_str(")\n");
    write(text, &nodes, &names, root, true, &mut out);
    out.push(')');
    out.push_str(&text[r.end..]);
    out
}

/// One node as the text writes it, each shared part inside it by its name;
/// the node itself by its name too, unless it is the part being defined.
fn write(
    text: &str,
    nodes: &[Node],
    names: &[Option<String>],
    at: usize,
    defining: bool,
    out: &mut String,
) {
    let n = &nodes[at];
    if !defining && let Some(name) = &names[n.part] {
        out.push_str(name);
        return;
    }
    let mut pos = n.start;
    for &item in &n.items {
        out.push_str(&text[pos..nodes[item].start]);
        write(text, nodes, names, item, false, out);
        pos = nodes[item].end;
    }
    out.push_str(&text[pos..n.end]);
}

/// The script text with its shared parts written out where their names
/// stand, keeping the rest of the text as it is; a text without shared
/// parts comes back as it is. What the kernel reads is the same either way.
pub fn unshare(text: &str) -> Result<String, String> {
    let malformed = || "malformed shared parts".to_string();
    let (nodes, _, root) = read(text, true).ok_or_else(malformed)?;
    let r = &nodes[root];
    let is_shared =
        matches!(r.items.first(), Some(&h) if &text[nodes[h].start..nodes[h].end] == "shared");
    let [_, defs, body] = r.items[..] else {
        return Ok(text.to_string());
    };
    if !is_shared {
        return Ok(text.to_string());
    }
    let mut table: HashMap<&str, String> = HashMap::new();
    for &def in &nodes[defs].items {
        let [name, part] = nodes[def].items[..] else {
            return Err(malformed());
        };
        let name = &text[nodes[name].start..nodes[name].end];
        let part = substitute(&text[nodes[part].start..nodes[part].end], &table)?;
        table.insert(name, part);
    }
    let b = &nodes[body];
    Ok(format!(
        "{}{}{}",
        &text[..r.start],
        substitute(&text[b.start..b.end], &table)?,
        &text[r.end..]
    ))
}

/// The text with each `#NAME` atom replaced by its part.
fn substitute(text: &str, table: &HashMap<&str, String>) -> Result<String, String> {
    let b = text.as_bytes();
    let mut out = String::with_capacity(text.len());
    let mut at = 0;
    while at < b.len() {
        let start = at;
        match b[at] {
            b'"' => {
                at += 1;
                while at < b.len() && b[at] != b'"' {
                    at += if b[at] == b'\\' { 2 } else { 1 };
                }
                at = (at + 1).min(b.len());
                out.push_str(&text[start..at]);
            }
            b'(' | b')' => {
                at += 1;
                out.push_str(&text[start..at]);
            }
            c if c.is_ascii_whitespace() => {
                at += 1;
                out.push_str(&text[start..at]);
            }
            _ => {
                while at < b.len()
                    && !matches!(b[at], b'(' | b')' | b'"')
                    && !b[at].is_ascii_whitespace()
                {
                    at += 1;
                }
                let atom = &text[start..at];
                match atom.strip_prefix('#') {
                    Some(_) => out.push_str(
                        table
                            .get(atom)
                            .ok_or_else(|| format!("{atom} is not defined before its use"))?,
                    ),
                    None => out.push_str(atom),
                }
            }
        }
    }
    Ok(out)
}

/// The nodes of the text, each a distinct part numbered by its shape, the
/// number of parts and the root; `None` for a text that is not exactly one
/// S-expression, or that holds a `#` atom unless `names` allows them.
fn read(text: &str, names: bool) -> Option<(Vec<Node>, usize, usize)> {
    let b = text.as_bytes();
    let mut nodes: Vec<Node> = Vec::new();
    let mut ids: HashMap<Key, usize> = HashMap::new();
    // The open lists: where each starts and its items so far.
    let mut open: Vec<(usize, Vec<usize>)> = Vec::new();
    let mut root = None;
    let mut at = 0;
    let mut intern = |nodes: &mut Vec<Node>, key, start, end, items| {
        let fresh = ids.len();
        let part = *ids.entry(key).or_insert(fresh);
        nodes.push(Node {
            start,
            end,
            part,
            items,
        });
        nodes.len() - 1
    };
    while at < b.len() {
        let c = b[at];
        let leaf_start = at;
        let done = match c {
            b';' => {
                while at < b.len() && b[at] != b'\n' {
                    at += 1;
                }
                continue;
            }
            c if c.is_ascii_whitespace() => {
                at += 1;
                continue;
            }
            b'(' => {
                open.push((at, Vec::new()));
                at += 1;
                continue;
            }
            b')' => {
                let (start, items) = open.pop()?;
                at += 1;
                let key = Key::List(items.iter().map(|&i| nodes[i].part).collect());
                intern(&mut nodes, key, start, at, items)
            }
            b'"' => {
                at += 1;
                loop {
                    match b.get(at)? {
                        b'"' => break,
                        b'\\' => at += 2,
                        _ => at += 1,
                    }
                }
                at += 1;
                intern(
                    &mut nodes,
                    Key::Leaf(&text[leaf_start..at]),
                    leaf_start,
                    at,
                    Vec::new(),
                )
            }
            _ => {
                if c == b'#' && !names {
                    return None;
                }
                while at < b.len()
                    && !matches!(b[at], b'(' | b')' | b'"')
                    && !b[at].is_ascii_whitespace()
                {
                    at += 1;
                }
                intern(
                    &mut nodes,
                    Key::Leaf(&text[leaf_start..at]),
                    leaf_start,
                    at,
                    Vec::new(),
                )
            }
        };
        match open.last_mut() {
            Some((_, items)) => items.push(done),
            None if root.is_none() => root = Some(done),
            None => return None,
        }
    }
    if !open.is_empty() {
        return None;
    }
    let parts = ids.len();
    Some((nodes, parts, root?))
}

/// The S-expression with its shared parts written out, as the kernel's
/// reader does: `(shared ((#NAME PART)…) BODY)` is BODY with each name
/// written out, each part using only the names before it; anything else
/// is itself. A name used before it is defined, defined twice, or not
/// starting with `#` is refused.
pub fn expand(sx: Sx) -> Result<Sx, String> {
    let Sx::List(mut xs) = sx else {
        return Ok(sx);
    };
    if !(xs.len() == 3
        && matches!(&xs[0], Sx::Atom(a) if a == "shared")
        && matches!(&xs[1], Sx::List(_)))
    {
        return Ok(Sx::List(xs));
    }
    let body = xs.pop().unwrap_or(Sx::List(Vec::new()));
    let Some(Sx::List(defs)) = xs.pop() else {
        return Ok(Sx::List(Vec::new()));
    };
    let mut table: HashMap<String, Sx> = HashMap::new();
    for def in defs {
        let Sx::List(pair) = def else {
            return Err("malformed shared part".into());
        };
        let [Sx::Atom(name), part] =
            <[Sx; 2]>::try_from(pair).map_err(|_| "malformed shared part".to_string())?
        else {
            return Err("malformed shared part".into());
        };
        if !name.starts_with('#') {
            return Err(format!("a shared name starts with #: {name}"));
        }
        if table.contains_key(&name) {
            return Err(format!("{name} is defined twice"));
        }
        let part = written_out(part, &table)?;
        table.insert(name, part);
    }
    written_out(body, &table)
}

fn written_out(sx: Sx, table: &HashMap<String, Sx>) -> Result<Sx, String> {
    match sx {
        Sx::Atom(a) if a.starts_with('#') => table
            .get(&a)
            .cloned()
            .ok_or_else(|| format!("{a} is not defined before its use")),
        Sx::List(xs) => Ok(Sx::List(
            xs.into_iter()
                .map(|x| written_out(x, table))
                .collect::<Result<_, _>>()?,
        )),
        other => Ok(other),
    }
}

#[cfg(test)]
mod tests {
    use super::super::read::parse;
    use super::*;

    fn round(text: &str) -> String {
        expand(parse(&share(text)).unwrap()).unwrap().print()
    }

    #[test]
    fn a_part_written_twice_is_named_once_and_reads_back_as_written() {
        let text = "; by P\n(steps 16\n (proof (trans ((call f (v x) (v y) (v z)) (call f (v x) (v y) (v z))) (refl (call f (v x) (v y) (v z))))))\n";
        let shared = share(text);
        assert!(shared.starts_with("; by P\n(shared ((#0 (call f (v x) (v y) (v z))))\n(steps 16"));
        assert!(shared.len() < text.len());
        assert_eq!(round(text), parse(text).unwrap().print());
    }

    #[test]
    fn nested_parts_name_the_inner_part_first() {
        let inner = "(call g (v aaaa) (v bbbb) (v cccc))";
        let outer = format!("(op + {inner} {inner})");
        let text = format!("(steps 16 (proof (trans ({outer} {outer} {inner}) {outer})))");
        let shared = share(&text);
        assert!(shared.contains(&format!("(#0 {inner})")));
        assert!(shared.contains("(#1 (op + #0 #0))"));
        assert_eq!(round(&text), parse(&text).unwrap().print());
    }

    #[test]
    fn unsharing_writes_the_parts_back_where_they_stand() {
        let text = "; by P\n(steps 16\n (proof (trans ((call f (v x) (v y) (v z)) (call f (v x) (v y) (v z))) (refl (call f (v x) (v y) (v z))))))\n";
        assert_eq!(unshare(&share(text)).unwrap(), text);
    }

    #[test]
    fn nothing_to_share_leaves_the_text_alone() {
        let text = "(steps 16 (proof (refl (v x))))";
        assert_eq!(share(text), text);
    }

    #[test]
    fn bad_names_are_refused() {
        let refused = |text: &str| expand(parse(text).unwrap()).unwrap_err();
        assert_eq!(
            refused("(shared ((#a (v x))) (refl #b))"),
            "#b is not defined before its use"
        );
        assert_eq!(
            refused("(shared ((#a (f #b)) (#b (v x))) #a)"),
            "#b is not defined before its use"
        );
        assert_eq!(
            refused("(shared ((#a (v x)) (#a (v y))) #a)"),
            "#a is defined twice"
        );
        assert_eq!(
            refused("(shared ((a (v x))) a)"),
            "a shared name starts with #: a"
        );
    }
}
