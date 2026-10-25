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
//!   of a function passes a parameter a value held as a raw `i64`, or every
//!   call narrows the `Int` it returns, which it boxed from a raw `i64`,
//!   straight back into one.

use std::collections::{HashMap, HashSet};

use crate::ast::Spanned;
use crate::codegen::{CodegenContext, PerfNote};
use crate::ir::FnId;
use crate::ir::mir::expr::walk_children;
use crate::ir::mir::{LocalId, MirCallee, MirExpr, MirFn, MirProgram};

use super::from_mir::MirEmitCtx;

/// Why the emitter cloned the target of a write instead of handing it over.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Held {
    /// The function reads it again (or the record it is a field of).
    Again,
    /// A tail-call loop passes it on unchanged to its next round.
    Loop,
    /// It is, or is a field of, a parameter the function only borrows.
    Lent,
}

/// A write target the emitter cloned.
#[derive(Debug, Clone, Copy)]
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

/// The notes of the transpile that `ctx` has just emitted, in source order.
pub(super) fn collect(ctx: &CodegenContext) -> Vec<PerfNote> {
    let Some(program) = ctx.mir_program.as_ref() else {
        return Vec::new();
    };
    let mut notes = Vec::new();
    let copied = ctx.rust_copied_targets.borrow();
    for (id, f) in program.iter() {
        let module = ctx
            .symbol_table
            .fn_entry(*id)
            .key
            .scope_str()
            .map(str::to_string);
        let mut sites = Vec::new();
        find_copied(&f.body.node, &copied, &mut sites);
        for (target, copy) in sites {
            if let Some(text) = copied_note(f, target, copy) {
                let line = read_of(target).map_or(f.body.line, |read| read.line);
                notes.push(PerfNote {
                    module: module.clone(),
                    line,
                    text,
                });
            }
        }
    }
    boxing_notes(ctx, program, &mut notes);
    notes.sort_by(|a, b| (&a.module, a.line, &a.text).cmp(&(&b.module, b.line, &b.text)));
    notes.dedup();
    notes
}

fn find_copied<'e>(
    e: &'e MirExpr,
    copied: &HashMap<usize, CopiedTarget>,
    out: &mut Vec<(&'e MirExpr, CopiedTarget)>,
) {
    if let Some(copy) = copied.get(&addr(e)) {
        out.push((e, *copy));
    }
    walk_children(e, &mut |child| find_copied(child, copied, out));
}

fn copied_note(f: &MirFn, target: &MirExpr, copy: CopiedTarget) -> Option<String> {
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
        seen: false,
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
    Some(format!(
        "`{}` copies the whole {kind} `{what}` before `{}`: {why}",
        f.name, copy.write
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
    seen: bool,
}

impl Search<'_, '_> {
    fn walk(&mut self, e: &MirExpr) -> Option<usize> {
        if addr(e) == self.target {
            self.seen = true;
            return None;
        }
        if Some(addr(e)) == self.keep {
            return None;
        }
        if let Some(read) = read_of(e) {
            return (self.seen && read.overlaps(self.read)).then_some(read.line);
        }
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
            if self.seen {
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

/// One call of a user function: its arguments, whether the call narrows
/// the result straight into a machine word, and where it is.
struct Call<'e> {
    args: &'e [Spanned<MirExpr>],
    narrowed: bool,
    module: Option<String>,
    line: usize,
}

/// The boundaries where a machine word becomes an `Int` at every call of a
/// function (see the module doc).
fn boxing_notes(ctx: &CodegenContext, program: &MirProgram, notes: &mut Vec<PerfNote>) {
    let carrier_ctors: HashSet<FnId> = program
        .iter()
        .filter(|(_, f)| builds_carrier_from_param(ctx, f))
        .map(|(id, _)| *id)
        .collect();
    let mut calls: HashMap<FnId, Vec<Call<'_>>> = HashMap::new();
    let mut taken = HashSet::new();
    for (id, f) in program.iter() {
        let module = ctx
            .symbol_table
            .fn_entry(*id)
            .key
            .scope_str()
            .map(str::to_string);
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
                narrowed: false,
                module: module.clone(),
                line,
            });
        });
        // A call is narrowed when its result is unboxed at once or handed
        // to the smart constructor of a carrier held as a raw `i64`.
        visit(&f.body.node, &mut |e| {
            let inner = match e {
                MirExpr::Unbox(inner) => &inner.node,
                MirExpr::Call(call) if matches!(call.node.callee, MirCallee::Fn(ctor) if carrier_ctors.contains(&ctor)) => {
                    match call.node.args.first() {
                        Some(arg) => &arg.node,
                        None => return,
                    }
                }
                _ => return,
            };
            let (MirExpr::Call(call), Some(sites)) =
                (inner, called(inner).and_then(|c| calls.get_mut(&c)))
            else {
                return;
            };
            if let Some(site) = sites
                .iter_mut()
                .find(|s| std::ptr::eq(s.args, call.node.args.as_slice()))
            {
                site.narrowed = true;
            }
        });
    }
    let raw = |e: &MirExpr| matches!(e, MirExpr::Box(inner) if !matches!(inner.node, MirExpr::Literal(_)));
    let literal = |e: &MirExpr| match e {
        MirExpr::Box(inner) => matches!(inner.node, MirExpr::Literal(_)),
        _ => matches!(e, MirExpr::Literal(_)),
    };
    for (id, f) in program.iter() {
        let key = &ctx.symbol_table.fn_entry(*id).key;
        // A function the compiler made (`__` in its name) has no signature
        // the program could change.
        let Some(sites) = calls
            .get(id)
            .filter(|_| !f.name.contains("__") && !taken.contains(&key.canonical()))
        else {
            continue;
        };
        let facts = ctx.bare_i64.for_fn(*id);
        let (count, boxes, narrow) = match sites.len() {
            1 => ("its one call (this one)".to_string(), "boxes", "narrows"),
            n => (
                format!("all {n} of its calls (this is the first)"),
                "box",
                "narrow",
            ),
        };
        let mut note = |at: &[&Call<'_>], text: String| {
            if let Some(site) = at.iter().min_by_key(|site| (&site.module, site.line)) {
                notes.push(PerfNote {
                    module: site.module.clone(),
                    line: site.line,
                    text,
                });
            }
        };
        for (index, param) in f.params.iter().enumerate() {
            if param.ty != "Int" || facts.is_some_and(|facts| facts.param_is_bare(index)) {
                continue;
            }
            let at = |site: &Call<'_>, is: &dyn Fn(&MirExpr) -> bool| {
                site.args.get(index).is_some_and(|arg| is(&arg.node))
            };
            let boxed: Vec<&Call<'_>> = sites.iter().filter(|site| at(site, &raw)).collect();
            if sites
                .iter()
                .all(|site| at(site, &raw) || at(site, &literal))
            {
                note(
                    &boxed,
                    format!(
                        "`{}` takes `{}` as an `Int`, and {count} {boxes} a machine word into it: a refinement over `Int` for `{}` in its signature would keep it a machine word",
                        f.name, param.name, param.name
                    ),
                );
            }
        }
        let mut leaves = Vec::new();
        result_leaves(&f.body.node, &mut leaves);
        if f.return_type == "Int"
            && !facts.is_some_and(|facts| facts.bare_return)
            && sites.iter().all(|site| site.narrowed)
            && leaves.iter().all(|leaf| raw(leaf) || literal(leaf))
            && leaves.iter().any(|leaf| raw(leaf))
        {
            note(
                &sites.iter().collect::<Vec<_>>(),
                format!(
                    "`{}` boxes the machine word it computes into the `Int` it returns, and {count} {narrow} it straight back: returning a refinement over `Int` would keep it a machine word",
                    f.name
                ),
            );
        }
    }
}

fn called(e: &MirExpr) -> Option<FnId> {
    match e {
        MirExpr::Call(call) => match call.node.callee {
            MirCallee::Fn(callee) => Some(callee),
            _ => None,
        },
        _ => None,
    }
}

/// The values `e` can return: its tail positions and every early return.
fn result_leaves<'e>(e: &'e MirExpr, out: &mut Vec<&'e MirExpr>) {
    match e {
        MirExpr::Let(l) => {
            returns_in(&l.node.value.node, out);
            result_leaves(&l.node.body.node, out);
        }
        MirExpr::Match(m) => {
            returns_in(&m.node.subject.node, out);
            for arm in &m.node.arms {
                result_leaves(&arm.body.node, out);
            }
        }
        MirExpr::IfThenElse(ite) => {
            returns_in(&ite.node.cond.node, out);
            result_leaves(&ite.node.then_branch.node, out);
            result_leaves(&ite.node.else_branch.node, out);
        }
        MirExpr::Return(inner) => result_leaves(&inner.node, out),
        _ => {
            returns_in(e, out);
            out.push(e);
        }
    }
}

fn returns_in<'e>(e: &'e MirExpr, out: &mut Vec<&'e MirExpr>) {
    walk_children(e, &mut |child| match child {
        MirExpr::Return(inner) => result_leaves(&inner.node, out),
        _ => returns_in(child, out),
    });
}

/// Whether `f` is the smart constructor of a carrier held as a raw `i64`:
/// it builds one from its one parameter, which it narrows to do so.
fn builds_carrier_from_param(ctx: &CodegenContext, f: &MirFn) -> bool {
    let [param] = f.params.as_slice() else {
        return false;
    };
    let mut builds = false;
    visit(&f.body.node, &mut |e| {
        if let MirExpr::RecordCreate(r) = e
            && super::carrier::is_i64_carrier(ctx, r.node.type_id)
            && r.node.fields.iter().any(|field| {
                read_of(&field.value.node).is_some_and(|read| read.slot == param.local)
            })
        {
            builds = true;
        }
    });
    builds
}
