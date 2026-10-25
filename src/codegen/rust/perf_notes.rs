//! `aver compile --target rust --perf-notes`: where the generated code is
//! slower than it could be because of a decision this backend has already
//! taken, one line each. A note names a copy or a conversion the emitted
//! code makes; nothing here guesses at what the program does at run time.
//!
//! - A Map or Vector copied whole before a write: the emitter clones the
//!   target of `Map.set`, `Map.remove` or `Vector.set`, so the write meets a
//!   collection with a second owner and copies it. The emitter records each
//!   such target as it renders it ([`owned_write_target`]); the note says
//!   why it was not handed over: a later read, a loop that keeps it for its
//!   next round, or a parameter the function only borrows. A copy made at
//!   run time because the collection's count is above one without a clone
//!   in the code (a value stored in two places) is not visible here.
//! - A machine word boxed into an `Int` at a function boundary: every call
//!   of a function passes a parameter a value held as a raw `i64` (or a
//!   literal), and the function works on it as an `Int`.

use std::collections::{HashMap, HashSet};

use crate::ast::Spanned;
use crate::codegen::{CodegenContext, PerfNote};
use crate::ir::FnId;
use crate::ir::mir::expr::walk_children;
use crate::ir::mir::{LocalId, MirCallee, MirExpr, MirFn};

use super::from_mir::MirEmitCtx;

/// Why the emitter cloned the target of a write instead of handing it over.
#[derive(Clone, Copy)]
enum Held {
    /// The function reads it again (or the record it is a field of).
    Again,
    /// A tail-call loop passes it on unchanged to its next round.
    Loop,
    /// It is, or is a field of, a parameter the function only borrows.
    Lent,
}

/// A write target the emitter cloned.
#[derive(Clone, Copy)]
pub struct CopiedTarget {
    /// `Map.set`, `Map.remove` or `Vector.set`.
    write: &'static str,
    held: Held,
    /// The default of the self-keep `Option.withDefault(Vector.set(v, ..),
    /// v)`, by address: the same write reads `v` there.
    keep: Option<usize>,
}

fn addr(e: &MirExpr) -> usize {
    e as *const MirExpr as usize
}

/// The target of `write` as an owning position renders it, recorded when
/// that is a clone.
pub(super) fn owned_write_target(
    code: String,
    target: &MirExpr,
    write: &'static str,
    keep: Option<&MirExpr>,
    ctx: &MirEmitCtx<'_>,
) -> String {
    let owned = super::ownership::materialize_owned(code.clone(), target, ctx);
    if let Some(codegen) = ctx.codegen
        && owned != code
        && let Some(root) = super::ownership::projection_root_local(target)
    {
        let held = if super::ownership::root_is_borrowed(root, ctx) {
            Held::Lent
        } else if ctx.loop_carried_params.contains(&root.name) {
            Held::Loop
        } else {
            Held::Again
        };
        codegen.rust_copied_targets.borrow_mut().insert(
            addr(target),
            CopiedTarget {
                write,
                held,
                keep: keep.map(addr),
            },
        );
    }
    owned
}

/// The notes of the transpile that `ctx` has just emitted, with the
/// `boxing` notes read before, in source order.
pub(super) fn collect(ctx: &CodegenContext, mut notes: Vec<PerfNote>) -> Vec<PerfNote> {
    let Some(program) = ctx.mir_program.as_ref() else {
        return notes;
    };
    let copied = ctx.rust_copied_targets.borrow();
    for (id, f) in program.iter() {
        visit(&f.body.node, &mut |target| {
            if let Some(copy) = copied.get(&addr(target))
                && let Some((line, text)) = copied_note(f, target, *copy)
            {
                notes.push(PerfNote {
                    module: module_of(ctx, *id),
                    line,
                    text,
                });
            }
        });
    }
    notes.sort();
    notes.dedup();
    notes
}

fn module_of(ctx: &CodegenContext, id: FnId) -> Option<String> {
    ctx.symbol_table
        .fn_entry(id)
        .key
        .scope_str()
        .map(str::to_string)
}

/// The line of the copy and what the note says about it.
fn copied_note(f: &MirFn, target: &MirExpr, copy: CopiedTarget) -> Option<(usize, String)> {
    let read = read_of(target)?;
    let what = read.spelled();
    let kind = match copy.write {
        "Vector.set" => "vector",
        _ => "map",
    };
    let mut search = Search {
        target: addr(target),
        keep: copy.keep,
        read: &read,
        seen: None,
        around: 0,
    };
    let why = match (search.walk(&f.body.node), copy.held) {
        (Some(line), _) => format!("`{what}` is read again at line {line}"),
        (None, Held::Lent) => format!(
            "`{}` only borrows `{}`, which its caller keeps",
            f.name, read.name
        ),
        (None, Held::Loop) => match passed_on(f, read.slot) {
            Some(line) => format!(
                "the loop passes `{}` on unchanged to its next round at line {line}",
                read.name
            ),
            None => format!("the loop keeps `{}` for its next round", read.name),
        },
        (None, Held::Again) => format!("`{what}` is read again later"),
    };
    Some((
        search.seen.unwrap_or(read.line),
        format!(
            "`{}` copies the whole {kind} `{what}` before `{}`: {why}",
            f.name, copy.write
        ),
    ))
}

/// A read of a named local, of a field path of it (`r.memo`) or of the
/// whole value (an empty path), and its line.
struct Read<'e> {
    slot: LocalId,
    name: &'e str,
    path: Vec<&'e str>,
    line: usize,
}

impl Read<'_> {
    fn spelled(&self) -> String {
        std::iter::once(self.name)
            .chain(self.path.iter().copied())
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Whether the two reads share a part: one path extends the other.
    fn overlaps(&self, other: &Read<'_>) -> bool {
        self.slot == other.slot && self.path.iter().zip(&other.path).all(|(a, b)| a == b)
    }
}

fn read_of(e: &MirExpr) -> Option<Read<'_>> {
    match e {
        MirExpr::Local(local) if !local.node.name.is_empty() => Some(Read {
            slot: local.node.slot,
            name: &local.node.name,
            path: Vec::new(),
            line: local.line,
        }),
        MirExpr::Project(project) => {
            let mut read = read_of(&project.node.base.node)?;
            read.path.push(&project.node.field);
            read.line = project.line;
            Some(read)
        }
        _ => None,
    }
}

/// The first read after `target`, in evaluation order, that overlaps it:
/// the second owner that made the emitter clone it. A branch the target is
/// in is followed to its end, not into the branches beside it.
struct Search<'r, 'e> {
    target: usize,
    keep: Option<usize>,
    read: &'r Read<'e>,
    /// The target's line, once the walk has passed it.
    seen: Option<usize>,
    /// The last line of the calls and `let`s around the walk. A read inside
    /// an interpolated string counts its line from the string; the call the
    /// string became says where it is.
    around: usize,
}

impl Search<'_, '_> {
    fn walk(&mut self, e: &MirExpr) -> Option<usize> {
        if addr(e) == self.target {
            self.seen = Some(self.read.line.max(self.around));
            return None;
        }
        if Some(addr(e)) == self.keep {
            return None;
        }
        if let Some(read) = read_of(e) {
            return (self.seen.is_some() && read.overlaps(self.read))
                .then_some(read.line.max(self.around));
        }
        let around = self.around;
        if let MirExpr::Let(Spanned { line, .. })
        | MirExpr::Call(Spanned { line, .. })
        | MirExpr::TailCall(Spanned { line, .. }) = e
        {
            self.around = self.around.max(*line);
        }
        let found = self.walk_children(e);
        self.around = around;
        found
    }

    fn walk_children(&mut self, e: &MirExpr) -> Option<usize> {
        let (head, branches): (&MirExpr, Vec<&MirExpr>) = match e {
            MirExpr::Match(m) => (
                &m.node.subject.node,
                m.node.arms.iter().map(|arm| &arm.body.node).collect(),
            ),
            MirExpr::IfThenElse(ite) => (
                &ite.node.cond.node,
                vec![&ite.node.then_branch.node, &ite.node.else_branch.node],
            ),
            _ => {
                let mut found = None;
                walk_children(e, &mut |child| {
                    if found.is_none() {
                        found = self.walk(child);
                    }
                });
                return found;
            }
        };
        if let Some(line) = self.walk(head) {
            return Some(line);
        }
        let before = self.seen;
        for branch in branches {
            self.seen = before;
            if let Some(line) = self.walk(branch) {
                return Some(line);
            }
            if before.is_none() && self.seen.is_some() {
                // The target was in this branch: the others are not after it.
                return None;
            }
        }
        None
    }
}

/// The line of a tail call of `f` that passes its parameter in `slot` on
/// unchanged.
fn passed_on(f: &MirFn, slot: LocalId) -> Option<usize> {
    let index = f.params.iter().position(|p| p.local == slot)?;
    let mut line = None;
    visit(&f.body.node, &mut |e| {
        if let MirExpr::TailCall(tc) = e
            && tc.node.target == f.fn_id
            && tc
                .node
                .args
                .get(index)
                .and_then(|a| read_of(&a.node))
                .is_some_and(|read| read.slot == slot && read.path.is_empty())
        {
            line.get_or_insert(tc.line);
        }
    });
    line
}

fn visit<'e>(e: &'e MirExpr, f: &mut dyn FnMut(&'e MirExpr)) {
    f(e);
    walk_children(e, &mut |child| visit(child, f));
}

/// One call of a user function: its arguments and where it is.
struct Call<'e> {
    args: &'e [Spanned<MirExpr>],
    module: Option<String>,
    line: usize,
}

/// The parameters an `Int` of which every call boxes from a machine word
/// (see the module doc), read from the program as the representation
/// rewrite left it: before the computed arguments of a call are bound to
/// locals of their own.
pub(super) fn boxing(ctx: &CodegenContext) -> Vec<PerfNote> {
    let mut notes = Vec::new();
    let Some(program) = ctx.mir_program.as_ref() else {
        return notes;
    };
    let mut calls: HashMap<FnId, Vec<Call<'_>>> = HashMap::new();
    let mut taken = HashSet::new();
    for (id, f) in program.iter() {
        let module = module_of(ctx, *id);
        visit(&f.body.node, &mut |e| {
            let (callee, args, line) = match e {
                MirExpr::Call(call) => match call.node.callee {
                    MirCallee::Fn(callee) => (callee, call.node.args.as_slice(), call.line),
                    _ => return,
                },
                MirExpr::TailCall(tc) => (tc.node.target, tc.node.args.as_slice(), tc.line),
                MirExpr::FnValue(name) => {
                    taken.insert(name.clone());
                    return;
                }
                _ => return,
            };
            calls.entry(callee).or_default().push(Call {
                args,
                module: module.clone(),
                line,
            });
        });
    }
    for (id, f) in program.iter() {
        // A function the compiler made (`__` in its name) has no signature
        // the program could change, and a smart constructor takes an `Int`.
        let Some(sites) = calls.get(id).filter(|_| {
            !f.name.contains("__")
                && !taken.contains(&ctx.symbol_table.fn_entry(*id).key.canonical())
                && !builds_carrier_from_param(ctx, f)
        }) else {
            continue;
        };
        let facts = ctx.bare_i64.for_fn(*id);
        for (index, param) in f.params.iter().enumerate() {
            if param.ty != "Int" || facts.is_some_and(|facts| facts.param_is_bare(index)) {
                continue;
            }
            // Every argument boxes a machine word or is a literal.
            let (mut boxed, mut first, mut all) = (Vec::new(), None::<&Call<'_>>, true);
            for site in sites {
                match site.args.get(index).map(|arg| &arg.node) {
                    Some(MirExpr::Box(inner)) if !matches!(inner.node, MirExpr::Literal(_)) => {
                        boxed.push(&inner.node);
                        if first.is_none_or(|f| (&site.module, site.line) < (&f.module, f.line)) {
                            first = Some(site);
                        }
                    }
                    Some(MirExpr::Literal(_) | MirExpr::Box(_)) => {}
                    _ => all = false,
                }
            }
            let (true, Some(first)) = (all, first) else {
                continue;
            };
            let calls = match (boxed.len(), sites.len()) {
                (1, 1) => "its one call (this one) boxes".to_string(),
                (1, n) => {
                    format!("one of its {n} calls (this one; the others pass a literal) boxes")
                }
                (b, n) if b == n => format!("all {n} of its calls (this is the first) box"),
                (b, n) => format!(
                    "{b} of its {n} calls (this is the first; the others pass a literal) box"
                ),
            };
            let then = match carrier_read(ctx, &boxed) {
                Some(carrier) => {
                    format!("taking the `{carrier}` itself would keep it a machine word")
                }
                None => format!("`{}` works on it as an `Int`", f.name),
            };
            notes.push(PerfNote {
                module: first.module.clone(),
                line: first.line,
                text: format!(
                    "`{}` takes `{}` as an `Int`, and {calls} a machine word into it: {then}",
                    f.name, param.name
                ),
            });
        }
    }
    notes
}

/// The carrier every one of `reads` is the `value` of, when they all are.
fn carrier_read<'c>(ctx: &'c CodegenContext, reads: &[&MirExpr]) -> Option<&'c str> {
    let mut carrier = None;
    for read in reads {
        let MirExpr::Project(project) = read else {
            return None;
        };
        let Some(crate::ast::Type::Named { id: Some(id), .. }) = project.node.base.ty() else {
            return None;
        };
        if !super::carrier::is_i64_carrier(ctx, Some(*id)) || carrier.is_some_and(|c| c != *id) {
            return None;
        }
        carrier = Some(*id);
    }
    carrier.map(|id| ctx.symbol_table.type_entry(id).key.name.as_str())
}

/// Whether `f` is the smart constructor of a carrier held as a raw `i64`:
/// it builds one from its one parameter, narrowed to do so.
fn builds_carrier_from_param(ctx: &CodegenContext, f: &MirFn) -> bool {
    let [param] = f.params.as_slice() else {
        return false;
    };
    let mut builds = false;
    visit(&f.body.node, &mut |e| {
        if let MirExpr::RecordCreate(r) = e
            && super::carrier::is_i64_carrier(ctx, r.node.type_id)
            && r.node.fields.iter().any(|field| match &field.value.node {
                MirExpr::Unbox(value) => {
                    read_of(&value.node).is_some_and(|read| read.slot == param.local)
                }
                _ => false,
            })
        {
            builds = true;
        }
    });
    builds
}

#[cfg(test)]
mod tests {
    /// The notes of `src` as `line: text`.
    fn notes(src: &str) -> Vec<String> {
        let mut ctx = super::super::tests::ctx_from_source(src, "perf");
        super::super::transpile(&mut ctx)
            .perf_notes
            .into_iter()
            .map(|note| format!("{}: {}", note.line, note.text))
            .collect()
    }

    const SRC: &str = r#"module M
    intent = "perf"
    exposes opaque [Word]

record Word
    value: Int

record Memo
    memo: Map<Int, Int>
    hits: Int

fn fromInt(n: Int) -> Result<Word, String>
    match Bool.and(n >= 0, n <= 4611686018427387903)
        true -> Result.Ok(Word(value = n))
        false -> Result.Err("out")

fn again(m: Map<Int, Int>) -> Int
    m2 = Map.set(m, 2, 2)
    Map.len(m2) + Map.len(m)

fn field(r: Memo, n: Int) -> Memo
    Memo(memo = Map.set(r.memo, n, n), hits = r.hits + Map.len(r.memo))

fn kept(v: Vector<Int>, i: Int) -> Int
    w = Option.withDefault(Vector.set(v, i, 7), v)
    Vector.len(w) + Vector.len(v)

fn round(m: Map<Int, Int>, i: Int, acc: Int) -> Int
    match i <= 0
        true -> acc
        false -> round(m, i - 1, acc + Map.len(Map.set(m, i, i)))

fn threaded(m: Map<Int, Int>, i: Int) -> Map<Int, Int>
    match i <= 0
        true -> m
        false -> threaded(Map.set(m, i, i), i - 1)

fn unpack(i: Int) -> Int
    Int.mod(i, 7) + Int.div(i, 7)

fn mixed(i: Int) -> Int
    Int.mod(i * 31, 1000003)

fn reads(w: Word) -> Int
    unpack(w.value) + unpack(w.value) + mixed(w.value + 1) + mixed(3)
"#;

    #[test]
    fn each_copy_and_boxing_is_noted_with_its_reason() {
        assert_eq!(
            notes(SRC),
            [
                "18: `again` copies the whole map `m` before `Map.set`: `m` is read again at line 19",
                "22: `field` copies the whole map `r.memo` before `Map.set`: `r.memo` is read again at line 22",
                "25: `kept` copies the whole vector `v` before `Vector.set`: `v` is read again at line 26",
                "31: `round` copies the whole map `m` before `Map.set`: the loop passes `m` on unchanged to its next round at line 31",
                "45: `mixed` takes `i` as an `Int`, and one of its 2 calls (this one; the others pass a literal) boxes a machine word into it: `mixed` works on it as an `Int`",
                "45: `unpack` takes `i` as an `Int`, and all 2 of its calls (this is the first) box a machine word into it: taking the `Word` itself would keep it a machine word",
            ]
        );
    }
}
