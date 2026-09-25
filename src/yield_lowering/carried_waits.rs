//! Waits the program keys by a type of its own, carried through an `Int`-keyed
//! wait (jasisz/aver#1329, process layer v2).
//!
//! The generated loop keys its one wait by `Int`: it numbers every item every
//! parked request waits on. A program that also waits by hand, keyed by a type
//! of its own (`Map<Watch, Wait.Item>`), would then key waits two ways, and
//! every backend names the wait's boundary from one key type. So in a program
//! that answers a capability of its own, each such wait is rewritten to call a
//! helper generated for its key type, in the reserved `__` namespace of the
//! module that makes it. The helper numbers the keys in the order the map puts
//! them in, waits on the numbered set, and hands back the keys the numbers
//! stand for, in the same order. The program sees the same answer it would
//! have seen; the backends see one key type.
//!
//! No generics: one set of helpers per key type, spelled for that type.

use std::collections::BTreeMap;

use crate::ast::{Expr, FnDef, Spanned, Stmt, TopLevel, Type};
use crate::codegen::expr_walk::for_each_child_mut;

const WAIT_POLL: &str = "Wait.poll";

/// What was carried: the helpers generated, as source, and their items.
pub(crate) struct Carried {
    pub source: String,
    pub items: Vec<TopLevel>,
}

fn walk(expr: &mut Spanned<Expr>, visit: &mut impl FnMut(&mut Spanned<Expr>)) {
    visit(expr);
    for_each_child_mut(expr, &mut |child| walk(child, visit));
}

fn walk_fn(fd: &mut FnDef, visit: &mut impl FnMut(&mut Spanned<Expr>)) {
    let body = std::sync::Arc::make_mut(&mut fd.body);
    for stmt in body.stmts_mut() {
        match stmt {
            Stmt::Binding(_, _, value) | Stmt::Expr(value) => walk(value, visit),
        }
    }
}

fn is_wait_poll(callee: &Expr) -> bool {
    fn dotted(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Ident(name) => Some(name.clone()),
            Expr::Attr(obj, field) => Some(format!("{}.{field}", dotted(&obj.node)?)),
            _ => None,
        }
    }
    dotted(callee).as_deref() == Some(WAIT_POLL)
}

/// Whether a module calls `Wait.poll` anywhere, read off the source alone.
pub(crate) fn calls_wait_poll(items: &[TopLevel]) -> bool {
    items.iter().any(|item| {
        let TopLevel::FnDef(fd) = item else {
            return false;
        };
        fd.body.stmts().iter().any(|stmt| {
            let value = match stmt {
                Stmt::Binding(_, _, value) | Stmt::Expr(value) => value,
            };
            crate::codegen::expr_walk::any(
                value,
                &mut |e| matches!(&e.node, Expr::FnCall(callee, _) if is_wait_poll(&callee.node)),
            )
        })
    })
}

/// The key of every `Wait.poll` call in `fd`, in walk order, read off the
/// types the checker stamped on each wait set.
fn keys_of(fd: &FnDef) -> Vec<Option<Type>> {
    let mut copy = fd.clone();
    let mut keys = Vec::new();
    walk_fn(&mut copy, &mut |expr| {
        if let Expr::FnCall(callee, args) = &expr.node
            && is_wait_poll(&callee.node)
        {
            keys.push(
                args.first()
                    .and_then(|set| set.ty())
                    .and_then(crate::capability::work::wait_set_key_of_type),
            );
        }
    });
    keys
}

/// `Infra.Watch` → `InfraWatch`: the part of a helper's name that says which
/// key type it carries.
fn suffix(spelled: &str) -> String {
    spelled
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .collect()
}

/// Rewrite every wait of `items` keyed by something other than `Int` to go
/// through a generated helper for its key. `stamped` is the same module after
/// a type check, in the same order, so every wait set carries its type. The
/// functions `skip` names were replaced by generated code and are left alone.
pub(crate) fn carry(
    items: &mut Vec<TopLevel>,
    stamped: &[TopLevel],
    spellings: &super::TypeSpellings,
    skip: &dyn Fn(&str) -> bool,
) -> Result<Option<Carried>, String> {
    let mut by_fn: BTreeMap<String, Vec<Option<Type>>> = BTreeMap::new();
    for item in stamped {
        if let TopLevel::FnDef(fd) = item
            && !skip(&fd.name)
        {
            let keys = keys_of(fd);
            if keys
                .iter()
                .any(|key| key.as_ref().is_some_and(|key| *key != Type::Int))
            {
                by_fn.insert(fd.name.clone(), keys);
            }
        }
    }
    if by_fn.is_empty() {
        return Ok(None);
    }
    // One helper set per key type, named after the key as the module spells it.
    let mut helpers: BTreeMap<String, String> = BTreeMap::new();
    for item in items.iter_mut() {
        let TopLevel::FnDef(fd) = item else { continue };
        let Some(keys) = by_fn.get(&fd.name) else {
            continue;
        };
        let mut at = 0usize;
        walk_fn(fd, &mut |expr| {
            let Expr::FnCall(callee, _) = &mut expr.node else {
                return;
            };
            if !is_wait_poll(&callee.node) {
                return;
            }
            let key = keys.get(at).cloned().flatten();
            at += 1;
            let Some(key) = key.filter(|key| *key != Type::Int) else {
                return;
            };
            let spelled = super::spell_type(&key, spellings);
            let name = format!("__waitPollBy{}", suffix(&spelled));
            helpers.entry(spelled).or_insert_with(|| name.clone());
            callee.node = Expr::Ident(name);
        });
    }
    let mut source = String::new();
    for (key, name) in &helpers {
        source.push_str(&helper_source(key, name));
    }
    let tokens = crate::lexer::Lexer::new(&source)
        .tokenize()
        .map_err(|error| error.to_string())?;
    let parsed = crate::parser::Parser::new_compiler_generated(tokens)
        .parse()
        .map_err(|error| error.to_string())?;
    items.extend(parsed.iter().cloned());
    Ok(Some(Carried {
        source,
        items: parsed,
    }))
}

/// The helpers that carry one key type through an `Int`-keyed wait.
fn helper_source(key: &str, name: &str) -> String {
    let tail = &name["__waitPollBy".len()..];
    format!(
        "\nfn {name}(items: Map<{key}, Wait.Item>, timeoutMs: Int) -> Result<List<{key}>, String>\n    ? \"Waits on a set keyed by {key} through a wait keyed by the position of each key, and answers the keys that were ready, in the order the set puts them in.\"\n    ! [Wait.poll]\n    keys = Map.keys(items)\n    ready = Wait.poll(__waitNumbered{tail}(items, keys, 0, {{}}), timeoutMs)?\n    Result.Ok(__waitKeysAt{tail}(Vector.fromList(keys), ready, []))\n\
         \nfn __waitNumbered{tail}(items: Map<{key}, Wait.Item>, keys: List<{key}>, next: Int, acc: Map<Int, Wait.Item>) -> Map<Int, Wait.Item>\n    ? \"The same items, keyed by the position of their key in the set's own order.\"\n    match keys\n        [] -> acc\n        [key, ..rest] -> __waitNumbered{tail}(items, rest, next + 1, __waitNumberedAt{tail}(items, key, next, acc))\n\
         \nfn __waitNumberedAt{tail}(items: Map<{key}, Wait.Item>, key: {key}, next: Int, acc: Map<Int, Wait.Item>) -> Map<Int, Wait.Item>\n    ? \"One item under its position.\"\n    match Map.get(items, key)\n        Option.None -> acc\n        Option.Some(item) -> Map.set(acc, next, item)\n\
         \nfn __waitKeysAt{tail}(keys: Vector<{key}>, ready: List<Int>, acc: List<{key}>) -> List<{key}>\n    ? \"The keys the ready positions stand for, in the order they were reported.\"\n    match ready\n        [] -> acc\n        [at, ..rest] -> __waitKeysAt{tail}(keys, rest, __waitKeyAt{tail}(keys, at, acc))\n\
         \nfn __waitKeyAt{tail}(keys: Vector<{key}>, at: Int, acc: List<{key}>) -> List<{key}>\n    ? \"One ready position, as its key.\"\n    match Vector.get(keys, at)\n        Option.None -> acc\n        Option.Some(key) -> List.concat(acc, [key])\n"
    )
}
