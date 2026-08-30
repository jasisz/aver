//! The same loop, kept as a list.
//!
//! Its neighbour in [`super`] fuses a collecting loop into the
//! `String.join` that consumes it. This one fuses the loop whose
//! accumulator IS the answer: nobody joins it, the caller wanted the
//! list, and the cost removed is the cons cell per element plus the
//! single reverse that straightens them out.
//!
//! The loop is the same loop, which is why this is a sibling module and
//! reuses its neighbour's occurs-check, shadowing guard and prepend /
//! reverse recognisers rather than growing a fourth walker beside them.
//! Two things differ.
//!
//! The CONSUMER. `String.join(<sink>(xs, []), sep)` is a shape; "the
//! result" is not. So there is no call-site anchor to match: every call
//! that starts the accumulator at `[]` is a call this rewrite can move,
//! and the fn's own exits are what say whether the caller wanted the
//! list forwards.
//!
//! The BODY. A joined builder is two arms; a parser is a tree of them,
//! with exits that never touch the accumulator at all (`Result.Err` on
//! a character it cannot read). What is checked is therefore not the
//! shape of the match but the shape of every READ: the accumulator may
//! be appended to in the recursive call, may be handed back at an exit,
//! and may appear nowhere else — a discipline that accepts one flat
//! loop and one four-deep parser for the same reason.
//!
//! An exit the accumulator never reaches splits on where the reverse
//! lives. When the loop reverses in its own exits, the compensation
//! sits inside exactly the exits that need it, and an untouched exit
//! keeps meaning what it meant — that parser fuses. When the exits
//! hand the accumulator back bare, the CALL SITE pays by giving up its
//! reverse, and a path through an untouched exit would come back
//! un-reversed — that loop is declined.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::*;
use crate::ast::TopLevel;

/// Where the single reverse lives in a collecting loop whose result is
/// the list itself — the same axis [`BufferBuildKind`] carries, and the
/// same reason it matters: it decides what the CALL SITE has to look
/// like, and a mismatch reverses a different number of times than the
/// program did.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListBuildKind {
    /// Every exit that returns the accumulator returns
    /// `List.reverse(acc)`. The caller writes `<fn>(xs, [])` and gets
    /// the list forwards.
    FinalizeInBase,
    /// Every exit returns the accumulator bare, and the caller writes
    /// `List.reverse(<fn>(xs, []))`. Only a call site wearing that
    /// reverse is rewritten: without it the caller asked for the
    /// elements backwards, which is not what a builder produces. EVERY
    /// exit, not just every exit that returns the accumulator: the
    /// caller's reverse pays for all paths at once, so a loop with an
    /// exit the accumulator never reaches is declined instead.
    BareAccBase,
}

/// The accumulator parameter of a collecting loop — everything about
/// it that can be read off the signature, which is everything the walk
/// needs before it has decided anything.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListBuildAcc {
    /// 0-based index of the accumulator parameter.
    pub idx: usize,
    /// The accumulator parameter's name.
    pub name: String,
    /// The accumulator parameter's declared type, which the builder
    /// keeps: the synthesized variant has the same signature as the fn
    /// it was built from, so no call site and no backend has a new type
    /// to learn.
    pub ty: String,
}

/// A fn whose `List` accumulator can become a builder. Built only once
/// the walk has decided where the reverse lives, because that is the
/// half a call site has to agree with and a provisional answer to it
/// would be a wrong one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListBuildShape {
    /// The accumulator the builder replaces.
    pub acc: ListBuildAcc,
    /// Where the reverse lives, and therefore what the call site must
    /// look like.
    pub kind: ListBuildKind,
}

/// Why the list-build recogniser turned a collecting loop down.
///
/// Reported per loop so `--explain-passes` can say which one stopped
/// firing and what about it stopped it. Every reason below has a program
/// in `tests/list_build_declines.rs` that answered correctly before this
/// pass existed and still does.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListBuildDecline {
    /// The accumulator is read somewhere a builder has no form for — a
    /// length, an argument to another fn, the subject the loop matches
    /// on, a second read inside the recursive call.
    AccEscapes,
    /// A pattern or binding re-binds the accumulator's name. The reads
    /// underneath it are a different value, and the name is what the
    /// occurs-check counts.
    AccShadowed,
    /// A self-call that is not the recognised append. Retargeting only
    /// the recognised one would leave the other calling the list
    /// spelling with a builder in its hand.
    SelfCallShape,
    /// No exit returns the accumulator, so there is nothing to build.
    NoFinish,
    /// Some exits reverse the accumulator and others hand it back bare.
    /// One call-site spelling cannot be right for both.
    MixedFinish,
    /// The exits that return the accumulator hand it back bare — the
    /// caller reverses — and some other exit returns a value the
    /// accumulator never reached. The rewrite pays for the bare exits
    /// by taking the reverse off the call site, and any path through
    /// the untouched exit would lose a reversal the program wrote.
    UntouchedExit,
    /// The `<fn>__collected` name is already taken.
    NameTaken,
}

impl ListBuildDecline {
    /// One-line explanation, rendered by `--explain-passes`.
    pub const fn reason(self) -> &'static str {
        match self {
            Self::AccEscapes => "the accumulator is read somewhere a builder cannot stand in for",
            Self::AccShadowed => "a pattern or binding re-binds the accumulator's name",
            Self::SelfCallShape => "a self-call does not append to the accumulator it was given",
            Self::NoFinish => "no exit returns the accumulator",
            Self::MixedFinish => "some exits reverse the accumulator and others do not",
            Self::UntouchedExit => {
                "an exit does not come from the accumulator, and the caller's \
                 reverse pays for the rewrite"
            }
            Self::NameTaken => "the __collected variant name is taken",
        }
    }
}

/// What the list-build pass did. Every field is a fact a CI gate can
/// assert on without parsing prose.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ListBuildPassReport {
    /// Drivers whose step companions were inlined ahead of candidacy —
    /// the driver-and-step normalization — with the steps in inline
    /// order. Alphabetised by driver. See [`super::driver_step`].
    pub pair_inlined_by_fn: std::collections::BTreeMap<String, Vec<String>>,
    /// Driver-and-step pairs the normalization looked at and turned
    /// down, with the reason. Alphabetised by driver.
    pub pair_declined: std::collections::BTreeMap<String, &'static str>,
    /// Call sites moved from `<fn>(…, [])` onto a collected variant.
    pub rewrites: usize,
    /// Names of the synthesized `<fn>__collected` variants appended to
    /// the items list. Sorted.
    pub synthesized: Vec<String>,
    /// Loop fns whose collected variant fired (`synthesized` minus the
    /// suffix). Sorted.
    pub builder_fns: Vec<String>,
    /// Per-fn call-site counts, alphabetised.
    pub rewrites_by_fn: std::collections::BTreeMap<String, usize>,
    /// Collecting loops the recogniser looked at and turned down, with
    /// the reason. Alphabetised by fn name.
    pub declined: std::collections::BTreeMap<String, &'static str>,
    /// `fromList` call sites deleted by retargeting a collected
    /// variant's builder to bytes.
    pub byte_retargets: usize,
    /// Collected variants whose builder was retargeted to bytes.
    /// Sorted.
    pub byte_fns: Vec<String>,
    /// Collected variants that had a `fromList`-shaped consumer the
    /// byte retarget turned down, with the reason. Alphabetised by
    /// variant name. A variant with no such consumer was never a
    /// candidate and is not listed.
    pub byte_declined: std::collections::BTreeMap<String, &'static str>,
}

/// Suffix distinguishing the synthesized variant from the loop it was
/// built from.
pub(super) const COLLECTED_SUFFIX: &str = "__collected";

/// The three intrinsics, in the order they appear in a rewritten loop.
const BUILDER_INTRINSICS: [&str; 3] = ["__lst_new", "__lst_push", "__lst_finalize"];

/// Run the list-build pass on a program's items: recognise collecting
/// loops, synthesize a `<fn>__collected` variant for each, move the call
/// sites that start the accumulator at `[]`, and append the variants.
///
/// Must run AFTER `tco::transform_program` (the append it recognises is
/// an `Expr::TailCall` by then) and BEFORE `resolver::resolve_program`
/// (everything here matches on `Expr::Ident`). It runs after chars
/// fusion so that a loop which has been given a cursor is recognised in
/// the shape it will actually be compiled in — the two rewrites compose
/// on one function, and this is the order that lets them.
pub fn run_list_build_pass(items: &mut Vec<TopLevel>) -> ListBuildPassReport {
    run_list_build_pass_filtered(items, None)
}

/// Run only the byte-retarget half of list-build.
///
/// Detection happens on a disposable full-pass copy because the byte sink is
/// deliberately defined as a retarget of a proven collected variant. Once the
/// exact set of byte consumers is known, the pass is replayed from pristine
/// input with every other candidate filtered out. Therefore enabling this gate
/// cannot leave a generic `__lst_*` rewrite behind, and a program with no byte
/// shape remains byte-for-byte unchanged.
pub fn run_byte_sink_pass(items: &mut Vec<TopLevel>) -> ListBuildPassReport {
    let pristine = items.clone();
    let mut probe = pristine.clone();
    let probe_report = run_list_build_pass_filtered(&mut probe, None);
    let byte_loops: HashSet<String> = probe_report
        .byte_fns
        .iter()
        .filter_map(|name| name.strip_suffix(COLLECTED_SUFFIX).map(str::to_string))
        .collect();
    if byte_loops.is_empty() {
        return ListBuildPassReport {
            byte_declined: probe_report.byte_declined,
            pair_declined: probe_report.pair_declined,
            ..Default::default()
        };
    }

    let mut replay = pristine.clone();
    let report = run_list_build_pass_filtered(&mut replay, Some(&byte_loops));
    if program_mentions_generic_list_builder(&replay) {
        // The filtered replay is expected to be deterministic with the probe,
        // but the safety boundary must not depend on that assumption. Commit
        // nothing rather than ever handing wasm-gc a generic collector.
        let mut declined = ListBuildPassReport {
            byte_declined: probe_report.byte_declined,
            pair_declined: probe_report.pair_declined,
            ..Default::default()
        };
        for name in byte_loops {
            declined.byte_declined.insert(
                format!("{name}{COLLECTED_SUFFIX}"),
                "filtered replay retained a generic list builder",
            );
        }
        return declined;
    }

    *items = replay;
    report
}

fn program_mentions_generic_list_builder(items: &[TopLevel]) -> bool {
    crate::ir::chars_fusion::fn_defs(items).any(|fd| {
        fd.body.stmts().iter().any(|stmt| {
            let expr = match stmt {
                Stmt::Binding(_, _, value) | Stmt::Expr(value) => value,
            };
            mentions_list_builder_intrinsic(expr)
        })
    })
}

fn run_list_build_pass_filtered(
    items: &mut Vec<TopLevel>,
    allowed_loops: Option<&HashSet<String>>,
) -> ListBuildPassReport {
    let mut report = ListBuildPassReport::default();
    // The driver-and-step normalization first: a loop written as a pair
    // becomes the single fn the candidacy walk below knows how to read.
    // It commits only bodies whose merged loop it has already proven
    // will fuse, so everything after this line treats them like any
    // other candidate.
    super::driver_step::run_driver_step_normalize(items, &mut report, allowed_loops);
    let candidates: Vec<(String, ListBuildAcc)> = list_build_candidates(items)
        .into_iter()
        .filter(|(name, _)| allowed_loops.is_none_or(|allowed| allowed.contains(name)))
        .collect();
    if candidates.is_empty() {
        return report;
    }

    let taken = crate::ir::chars_fusion::taken_names(items);

    let mut accepted: HashMap<String, (String, ListBuildShape)> = HashMap::new();
    let mut variants: Vec<FnDef> = Vec::new();
    for (name, acc) in candidates {
        let new_name = format!("{name}{COLLECTED_SUFFIX}");
        let outcome = if taken.contains(&new_name) {
            Err(ListBuildDecline::NameTaken)
        } else {
            let fd = crate::ir::chars_fusion::fn_defs(items)
                .find(|fd| fd.name == name)
                .expect("candidate came from this item list");
            build_collected_variant(fd, &acc, &new_name)
        };
        match outcome {
            Ok((variant, kind)) => {
                accepted.insert(name, (new_name, ListBuildShape { acc, kind }));
                variants.push(variant);
            }
            Err(decline) => {
                report.declined.insert(name, decline.reason());
            }
        }
    }

    let mut fired: HashMap<String, usize> = HashMap::new();
    for fd in crate::ir::chars_fusion::fn_defs_mut(items) {
        let body = Arc::make_mut(&mut fd.body);
        let FnBody::Block(stmts) = body;
        for stmt in stmts.iter_mut() {
            rewrite_list_build_sites(
                crate::ir::chars_fusion::stmt_expr_mut(stmt),
                &accepted,
                &mut fired,
            );
        }
    }

    // A variant nobody calls is dead synthesized code; drop it rather
    // than leave it in the program.
    variants.retain(|v| {
        accepted
            .iter()
            .any(|(loop_name, (new_name, _))| new_name == &v.name && fired.contains_key(loop_name))
    });
    report.rewrites = fired.values().sum();
    report.rewrites_by_fn = fired.into_iter().collect();
    report.synthesized = variants.iter().map(|v| v.name.clone()).collect();
    report.synthesized.sort();
    report.builder_fns = report
        .synthesized
        .iter()
        .map(|n| {
            n.strip_suffix(COLLECTED_SUFFIX)
                .expect("every synthesized name carries the suffix")
                .to_string()
        })
        .collect();
    run_byte_sink_retarget(items, &accepted, &mut variants, &mut report);

    items.reserve(variants.len());
    for variant in variants {
        items.push(TopLevel::FnDef(variant));
    }
    report
}

/// Is there a collecting loop here at all? Read-only, so a caller that
/// has to decide whether to COPY a module before running the pass can
/// ask first — the VM re-parses every dependency and re-runs the pass on
/// its own copy. A driver-and-step pair counts: the normalization would
/// merge it into exactly such a loop.
pub fn has_list_build_shape(items: &[TopLevel]) -> bool {
    crate::ir::chars_fusion::fn_defs(items).any(|fd| list_build_acc_of(fd).is_some())
        || super::driver_step::has_driver_step_shape(items)
}

/// Every fn that looks like a collecting loop, paired with the
/// accumulator it collects into.
fn list_build_candidates(items: &[TopLevel]) -> Vec<(String, ListBuildAcc)> {
    let mut out = Vec::new();
    for fd in crate::ir::chars_fusion::fn_defs(items) {
        if let Some(acc) = list_build_acc_of(fd) {
            out.push((fd.name.clone(), acc));
        }
    }
    out
}

/// The accumulator parameter of `fd`, when `fd` has one and appends to
/// it in a self tail-call.
///
/// This is the cheap half of the recogniser and the reason declines are
/// reported for collecting loops rather than for every fn in the
/// program: a fn without an accumulator was never a candidate, so
/// saying nothing about it is not silence about a fusion that stopped
/// firing.
///
/// The accumulator is the RIGHTMOST `List<…>` parameter, the same rule
/// the joined builders use above and for the same reason: a list-driven
/// loop takes its input first and threads its answer last.
pub(super) fn list_build_acc_of(fd: &FnDef) -> Option<ListBuildAcc> {
    let (idx, name, ty) = fd
        .params
        .iter()
        .enumerate()
        .rfind(|(_, (_, ty))| is_list_type_str(ty))
        .map(|(i, (name, ty))| (i, name.clone(), ty.clone()))?;
    body_appends_to_acc(&fd.body, &fd.name, idx, &name).then_some(ListBuildAcc { idx, name, ty })
}

/// Does any self tail-call anywhere in `body` append to the accumulator?
fn body_appends_to_acc(body: &FnBody, self_name: &str, acc_idx: usize, acc_name: &str) -> bool {
    fn walk(expr: &Spanned<Expr>, self_name: &str, acc_idx: usize, acc_name: &str) -> bool {
        if self_tail_prepends_to_acc(&expr.node, self_name, acc_idx, acc_name) {
            return true;
        }
        let mut found = false;
        crate::ir::chars_fusion::walk_children(expr, &mut |child| {
            found = found || walk(child, self_name, acc_idx, acc_name);
        });
        found
    }
    body.stmts().iter().any(|stmt| {
        walk(
            crate::ir::chars_fusion::stmt_expr(stmt),
            self_name,
            acc_idx,
            acc_name,
        )
    })
}

/// How a walk of the body ended up using the accumulator. The counts
/// are what decide the kind, and disagreement between them is what
/// makes a loop unfusable.
#[derive(Default)]
struct AccUses {
    /// `List.reverse(acc)` at an exit.
    finalize: usize,
    /// bare `acc` at an exit.
    bare: usize,
    /// the recognised append in a self tail-call.
    append: usize,
    /// an exit whose value never reads the accumulator at all.
    untouched: usize,
}

/// Build `<fn>__collected` — the same fn with its accumulator turned
/// into a builder — or say why it cannot be built.
///
/// Check and rewrite are ONE traversal, over a copy of the body. There
/// is no separate verification pass that could come to a different
/// conclusion than the rewrite it is supposed to be guarding: the walk
/// either replaces every read of the accumulator with the builder form
/// that stands for it, or it stops and the copy is thrown away.
pub(super) fn build_collected_variant(
    fd: &FnDef,
    acc: &ListBuildAcc,
    new_name: &str,
) -> Result<(FnDef, ListBuildKind), ListBuildDecline> {
    // The whole body travels: the variant is the fn it was built from
    // with one statement rewritten, so the bindings a loop sets up
    // before its match come with it. Dropping them would leave the
    // names they bind free in the variant — a compile error, or a
    // silent read of a top-level binding that happens to share one.
    let FnBody::Block(stmts) = fd.body.as_ref();
    let mut stmts = stmts.clone();
    // A binding of the accumulator's own name shadows it for everything
    // after, and the reads after it are a different value than the one
    // the occurs-check is counting.
    for stmt in stmts.iter() {
        if let Stmt::Binding(name, _, _) = stmt
            && name == &acc.name
        {
            return Err(ListBuildDecline::AccShadowed);
        }
    }
    let Some((last, leading)) = stmts.split_last_mut() else {
        return Err(ListBuildDecline::NoFinish);
    };
    // Only the last statement is the fn's answer. A read of the
    // accumulator in any earlier one is a read on the same path as
    // whatever the answer does with it — two reads, and the builder has
    // no form for the second.
    for stmt in leading.iter() {
        if count_ident_reads(&crate::ir::chars_fusion::stmt_expr(stmt).node, &acc.name) > 0 {
            return Err(ListBuildDecline::AccEscapes);
        }
    }
    let Stmt::Expr(result) = last else {
        return Err(ListBuildDecline::NoFinish);
    };

    // Every self-call, before any of them is moved. The walk below only
    // reaches the parts of the body that mention the accumulator, so a
    // self-call that does NOT mention it — one that restarts the fold
    // with a fresh list, say — would be left calling the list spelling
    // from inside a variant holding a builder. Asking about all of them
    // up front is what makes "the one it moved" and "all of them" the
    // same set.
    if !every_self_tail_call_appends(result, fd, acc) {
        return Err(ListBuildDecline::SelfCallShape);
    }

    let mut uses = AccUses::default();
    rewrite_acc_to_builder(result, fd, acc, new_name, true, &mut uses)?;
    if uses.append == 0 {
        return Err(ListBuildDecline::SelfCallShape);
    }
    let kind = match (uses.finalize, uses.bare) {
        (0, 0) => return Err(ListBuildDecline::NoFinish),
        (_, 0) => ListBuildKind::FinalizeInBase,
        // The bare-accumulator loop is paid for at the CALL SITE: the
        // rewrite takes the caller's reverse off. An exit the
        // accumulator never reaches never owed that reverse, so any
        // path that takes it would come back un-reversed. The
        // finalize-in-base kinds are immune — their compensation sits
        // inside the exits that need it, and an untouched exit keeps
        // meaning what it meant.
        (0, _) if uses.untouched > 0 => return Err(ListBuildDecline::UntouchedExit),
        (0, _) => ListBuildKind::BareAccBase,
        _ => return Err(ListBuildDecline::MixedFinish),
    };

    let variant = FnDef {
        name: new_name.to_string(),
        line: fd.line,
        // Same signature as the fn it was built from: the builder IS the
        // `List<T>` the accumulator was, so there is no new type for a
        // call site or a backend to learn.
        params: fd.params.clone(),
        return_type: fd.return_type.clone(),
        // Whatever the original was allowed to do, at the same points —
        // this rewrite moves no call and drops no arm.
        effects: fd.effects.clone(),
        desc: Some(format!(
            "Synthesized collecting variant of `{}`. Appends to a \
             builder where `{}` prepends to `{}` and reverses on the way \
             out, which reaches the same list without the cons chain or \
             the reversal. Call sites that start the accumulator at `[]` \
             are moved here.",
            fd.name, fd.name, acc.name
        )),
        body: Arc::new(FnBody::Block(stmts)),
        resolution: None,
    };
    Ok((variant, kind))
}

/// Is `expr` a self tail-call whose accumulator argument is
/// `List.prepend(<elem>, acc)` — the SHAPE of the append, with nothing
/// said about how many times the accumulator is read?
///
/// Split out from [`is_self_tail_with_prepend_acc`], which asks both
/// questions at once, because the two answers are wanted at different
/// places: this one decides whether a fn is a collecting loop AT ALL,
/// and a loop that reads its accumulator twice is still a collecting
/// loop — one that has to be reported as declined rather than silently
/// never looked at.
fn self_tail_prepends_to_acc(expr: &Expr, self_name: &str, acc_idx: usize, acc_name: &str) -> bool {
    let Expr::TailCall(data) = expr else {
        return false;
    };
    data.target == self_name
        && data
            .args
            .get(acc_idx)
            .is_some_and(|arg| is_list_prepend_to_acc(&arg.node, acc_name))
}

/// Is every self tail-call under `expr` an append to the accumulator?
fn every_self_tail_call_appends(expr: &Spanned<Expr>, fd: &FnDef, acc: &ListBuildAcc) -> bool {
    if let Expr::TailCall(data) = &expr.node
        && data.target == fd.name
        && !self_tail_prepends_to_acc(&expr.node, &fd.name, acc.idx, &acc.name)
    {
        return false;
    }
    let mut ok = true;
    crate::ir::chars_fusion::walk_children(expr, &mut |child| {
        ok = ok && every_self_tail_call_appends(child, fd, acc);
    });
    ok
}

/// Replace every read of the accumulator in `expr` with the builder
/// form that stands for it, counting what it found.
///
/// The rules ARE the safety argument, so they are written as one ordered
/// walk with a decline at the end: a read this walk does not recognise
/// is a read the rewrite has no form for, and the loop keeps its list.
///
/// Reads are permitted in exactly two shapes — a reverse of the
/// accumulator, and the append in the self-call — and at most ONE of
/// them may be evaluated on any path. That is the whole linearity
/// argument, and it is what lets the builder be written in place: the
/// list handed to a push or a finalize is the only handle to what it
/// holds, because a second handle would have needed a second read.
fn rewrite_acc_to_builder(
    expr: &mut Spanned<Expr>,
    fd: &FnDef,
    param: &ListBuildAcc,
    new_name: &str,
    is_result: bool,
    uses: &mut AccUses,
) -> Result<(), ListBuildDecline> {
    let acc = &param.name;
    let line = expr.line;
    let acc_ty = crate::types::parse_type_str(&param.ty);

    // The append, which is the only shape a self-call may take here —
    // `every_self_tail_call_appends` has already said so about the ones
    // this walk will not reach.
    if let Expr::TailCall(data) = &expr.node
        && data.target == fd.name
    {
        // The append's own tail is the one permitted read. A second one
        // anywhere in the argument list — inside the element, or in a
        // sibling argument that inspects what has been collected — is a
        // second read on this path, and the builder it would read from
        // is the one the append is about to consume.
        if !is_self_tail_with_prepend_acc(&expr.node, &fd.name, param.idx, acc) {
            return Err(ListBuildDecline::AccEscapes);
        }
        let Expr::TailCall(data) = &mut expr.node else {
            unreachable!("matched a tail call one line above");
        };
        let elem = match &data.args[param.idx].node {
            Expr::FnCall(_, prepend_args) => prepend_args[0].clone(),
            _ => unreachable!("the append recogniser matched a List.prepend"),
        };
        data.target = new_name.to_string();
        data.args[param.idx] = list_intrinsic_call(
            line,
            "__lst_push",
            vec![
                sp_at_typed(line, Expr::Ident(acc.clone()), acc_ty.clone()),
                elem,
            ],
            acc_ty,
        );
        uses.append += 1;
        return Ok(());
    }

    if count_ident_reads(&expr.node, acc) == 0 {
        // An exit the accumulator never reaches. It rides through
        // unchanged, but it is still counted: whether that is sound
        // depends on where the reverse lives, and only the kind decision
        // knows that.
        if is_result {
            uses.untouched += 1;
        }
        return Ok(());
    }

    // `List.reverse(acc)` at an exit — the reversal the builder makes
    // unnecessary.
    if is_list_reverse_of(&expr.node, acc) {
        *expr = list_intrinsic_call(
            line,
            "__lst_finalize",
            vec![sp_at_typed(line, Expr::Ident(acc.clone()), acc_ty.clone())],
            acc_ty,
        );
        uses.finalize += 1;
        return Ok(());
    }

    // Bare `acc` — the caller does the reversing. Only where the fn's
    // ANSWER is, and that is the one form here whose position matters.
    //
    // A reverse of the accumulator can stand anywhere, because the
    // builder finalizes to the very list the reverse produced. A bare
    // accumulator is the list BACKWARDS, and the rewrite pays for that
    // by taking the reverse off the call site — which it can only do
    // when what the call site receives is the accumulator itself.
    // Handed to another function instead, the same substitution turns
    // that function's argument the right way round without anyone
    // turning it back.
    if is_ident_named(&expr.node, acc) {
        if !is_result {
            return Err(ListBuildDecline::AccEscapes);
        }
        *expr = list_intrinsic_call(
            line,
            "__lst_finalize",
            vec![sp_at_typed(line, Expr::Ident(acc.clone()), acc_ty.clone())],
            acc_ty,
        );
        uses.bare += 1;
        return Ok(());
    }

    // A match is where the branching lives. Its arms are alternatives,
    // so a read in each of two arms is still one read on the path that
    // is taken — which is why they are walked independently. The
    // SUBJECT is not an alternative: it is evaluated before whichever
    // arm runs, so a read there and a read in an arm are two reads on
    // one path, and requiring none in the subject is the cheap way to
    // rule that out. An arm that re-binds the accumulator's name makes
    // every read underneath it a different value than the one the
    // occurs-check is counting.
    if let Expr::Match { subject, arms } = &mut expr.node {
        if count_ident_reads(&subject.node, acc) > 0 {
            return Err(ListBuildDecline::AccEscapes);
        }
        for arm in arms.iter() {
            if pattern_binds_name(&arm.pattern, acc) {
                return Err(ListBuildDecline::AccShadowed);
            }
        }
        // Each arm is the fn's answer whenever this match is.
        for arm in arms.iter_mut() {
            rewrite_acc_to_builder(&mut arm.body, fd, param, new_name, is_result, uses)?;
        }
        return Ok(());
    }

    // Anything else rides through, and the count is what makes that
    // safe. `Result.Ok(List.reverse(acc))` is how a parser hands its
    // list back and the wrapper has no opinion about what it wraps; so
    // does `List.len(List.reverse(acc))`, and neither needs a rule of
    // its own once the ONE read below is a recognised form. Two reads
    // under a node that evaluates all its children are two reads on one
    // path — the builder would be finalized and then appended to, or
    // appended to twice — so exactly one is the whole condition.
    if count_ident_reads(&expr.node, acc) != 1 {
        return Err(ListBuildDecline::AccEscapes);
    }
    let mut outcome = Err(ListBuildDecline::AccEscapes);
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| {
        if count_ident_reads(&child.node, acc) > 0 {
            // Under a node of any other kind, the read is that node's
            // input and not the fn's answer.
            outcome = rewrite_acc_to_builder(child, fd, param, new_name, false, uses);
        }
    });
    outcome
}

// === The byte sink =====================================================
//
// The third consumer a collected loop can have, after "the list is the
// answer" and "String.join eats it": the standard library's
// `Bytes.fromList`, which walks the collected list a second time to
// check every element against `0..=255` and wrap it in the `Bytes`
// record. When that call is the ONLY reader of a collected result, the
// list between the loop and the record is scaffolding: the range check
// can ride every push, and the loop's own exit can answer the
// `Result<Bytes, String>` the pair used to compute. So the retarget
// below rewrites the collected variant's `__lst_*` builder to the
// `__byt_*` one and deletes the `fromList` call.
//
// What makes this sound is that the retarget bakes `fromList`'s
// semantics — the range, the first-offender rule, the message, the
// record — into the byte builder. That is only true of THE `fromList`
// in the standard library, so the consumer is verified structurally:
// the module's `fromList`, the three helpers it reads, and the `Bytes`
// record must be AST-equal to the embedded standard library's own
// (spanned equality ignores lines, so the check is about shape, not
// position). A vendored copy that drifted by a word declines; an exact
// copy fuses, because an exact copy means exactly what the original
// means. Only the bare in-module spelling is recognised — a dotted
// `Bytes.fromList` from another module would need the dep's items to
// verify against, and no workload has brought that shape yet.

/// Why the byte-sink retarget turned a collected variant down.
///
/// Reported per variant so `--explain-passes` can say which loop kept
/// its list and what about its consumer stopped the retarget. Every
/// reason below has a program in `tests/list_build_declines.rs` that
/// answered correctly before this stage existed and still does.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteSinkDecline {
    /// A call site of the variant is not a `fromList` consumer — some
    /// caller wants the list itself, and one variant cannot hand back
    /// two different answers.
    MixedConsumers,
    /// The bound collected result is read again outside the `fromList`
    /// call, or something re-binds its name between the two — the
    /// occurs-check of the family, on the consumer side.
    SecondReader,
    /// The `fromList` call is not the consumer fn's own answer. The
    /// `?` on the collected call returns parse errors from the fn
    /// early; fused, they become the call's value — the two agree only
    /// where that value IS the fn's answer.
    NotTheAnswer,
    /// The consumer named `fromList` is not the standard library's:
    /// its shape (or a helper's, or the record's) differs from the
    /// embedded `Bytes` module, or a local binder shadows the name.
    ConsumerShape,
    /// An exit of the variant answers something besides the collected
    /// list, so a path exists on which `fromList` would have validated
    /// elements the byte builder never saw.
    ExitShape,
    /// The accumulator is not a `List<Int>`, so its elements are not
    /// candidate octets. Unreachable through a type-checked consumer,
    /// kept so the decision never rests on the typechecker having run.
    ElemShape,
}

impl ByteSinkDecline {
    /// One-line explanation, rendered by `--explain-passes`.
    pub const fn reason(self) -> &'static str {
        match self {
            Self::MixedConsumers => "another caller reads the collected list itself",
            Self::SecondReader => "the collected result is read outside the fromList call",
            Self::NotTheAnswer => "the fromList call is not the consumer's own answer",
            Self::ConsumerShape => "fromList here is not the standard library's Bytes.fromList",
            Self::ExitShape => "an exit answers something besides the collected list",
            Self::ElemShape => "the accumulator does not collect List<Int>",
        }
    }
}

/// The consumer fn name this stage recognises, in its bare in-module
/// spelling.
const FROM_LIST: &str = "fromList";

/// The four standard-library fns whose exact shapes the retarget
/// replaces, and the record their answer wraps. `fromList` is the
/// consumer; the other three are what its arms read, so a module that
/// edited any of them edited what `fromList` means.
const FROM_LIST_FAMILY: [&str; 4] = [
    FROM_LIST,
    "allInRange",
    "firstOutOfRange",
    "firstOutOfRangeIndex",
];

/// The record `fromList` wraps its answer in.
const BYTES_RECORD: &str = "Bytes";

/// What a collected variant looks like to the byte-sink stage.
struct ByteSinkMeta {
    /// Index of the variant in the pass's owned `variants` vec.
    variant_idx: usize,
    /// The accumulator parameter's index and name.
    acc_idx: usize,
    acc_name: String,
    /// `true` when the variant answers `Result<List<Int>, String>`
    /// (exits are `Result.Ok(finalize)` / `Result.Err(…)`, consumers
    /// go through `?`); `false` when it answers `List<Int>` bare
    /// (exits are the finalize itself, consumers apply `fromList`
    /// directly). `None` when it answers neither, which no `fromList`
    /// consumer can be feeding legally.
    result_kind: Option<bool>,
    /// Does the accumulator collect `List<Int>`?
    elem_ok: bool,
}

/// What scanning the consumers found about one variant.
#[derive(Default)]
struct ConsumerFacts {
    /// Reads of the variant's name anywhere in the module's fns.
    occurrences: usize,
    /// Recognised consumer sites (each consumes exactly one read).
    sites: usize,
    /// The first consumer-shaped interaction that could not fuse.
    flaw: Option<ByteSinkDecline>,
}

/// One reference form of the standard library's `fromList` family.
/// Two are kept because this pass meets the module in two states:
/// after `interp_lower` (the compile pipelines) and before it (the
/// VM's dependency re-run and the unit harness). Both are the same
/// program; the retarget must decide identically on both.
struct FromListReference {
    fns: Vec<FnDef>,
    record_fields: Vec<(String, String)>,
}

/// The embedded standard library's `fromList` family, in both forms,
/// built once per process. Empty when the embedded module is missing
/// or has lost one of the shapes — every candidate then declines,
/// which is the safe reading of "the thing I bake in does not exist".
fn from_list_references() -> &'static [FromListReference] {
    static REFS: std::sync::OnceLock<Vec<FromListReference>> = std::sync::OnceLock::new();
    REFS.get_or_init(|| {
        let Some(module) = crate::stdlib::find(BYTES_RECORD) else {
            return Vec::new();
        };
        let mut out = Vec::new();
        for lower_interpolation in [false, true] {
            let Ok(mut items) = crate::source::parse_source(module.source) else {
                continue;
            };
            crate::tco::transform_program(&mut items);
            if lower_interpolation {
                crate::ir::lower_interpolation_pass(&mut items);
            }
            let mut fns = Vec::new();
            for name in FROM_LIST_FAMILY {
                if let Some(fd) =
                    crate::ir::chars_fusion::fn_defs(&items).find(|fd| fd.name == name)
                {
                    fns.push(fd.clone());
                }
            }
            let record_fields = items.iter().find_map(|it| match it {
                TopLevel::TypeDef(crate::ast::TypeDef::Product { name, fields, .. })
                    if name == BYTES_RECORD =>
                {
                    Some(fields.clone())
                }
                _ => None,
            });
            if let (4, Some(record_fields)) = (fns.len(), record_fields) {
                out.push(FromListReference { fns, record_fields });
            }
        }
        out
    })
}

/// Is this module's `fromList` family the standard library's, shape
/// for shape? Params, return type, effects and body of all four fns,
/// and the `Bytes` record's fields, against either reference form.
/// Lines and type stamps do not participate (spanned equality ignores
/// both), so an exact copy at other line numbers passes and a copy
/// that changed a word does not.
fn from_list_is_the_stdlib_one(items: &[TopLevel]) -> bool {
    // Source currently permits duplicate function names and resolves the later
    // definition. A first, standard-shaped copy must therefore not prove a
    // later shadowing definition safe. The same uniqueness rule protects the
    // nominal record and top-level value namespace.
    if items.iter().any(|item| {
        matches!(
            item,
            TopLevel::Stmt(Stmt::Binding(name, _, _))
                if FROM_LIST_FAMILY.contains(&name.as_str()) || name == BYTES_RECORD
        ) || matches!(item, TopLevel::FnDef(fd) if fd.name == BYTES_RECORD)
    }) {
        return false;
    }

    from_list_references().iter().any(|reference| {
        reference.fns.iter().all(|ref_fd| {
            let mut matching =
                crate::ir::chars_fusion::fn_defs(items).filter(|fd| fd.name == ref_fd.name);
            let Some(fd) = matching.next() else {
                return false;
            };
            matching.next().is_none()
                && fd.params == ref_fd.params
                && fd.return_type == ref_fd.return_type
                && fd.effects.is_empty()
                && ref_fd.effects.is_empty()
                && *fd.body == *ref_fd.body
        }) && {
            let mut records = items.iter().filter(|item| {
                matches!(
                    item,
                    TopLevel::TypeDef(crate::ast::TypeDef::Product { name, .. })
                        | TopLevel::TypeDef(crate::ast::TypeDef::Sum { name, .. })
                        if name == BYTES_RECORD
                )
            });
            let Some(record) = records.next() else {
                return false;
            };
            records.next().is_none()
                && matches!(
                    record,
                    TopLevel::TypeDef(crate::ast::TypeDef::Product { fields, .. })
                        if *fields == reference.record_fields
                )
        }
    })
}

/// Does `fd` bind `name` anywhere — a parameter, a statement binding,
/// or a pattern binder? A binder is what shadows the module-level
/// `fromList` for the code underneath it, and a shadowed consumer is
/// not the consumer this stage verified.
fn fn_binds_name(fd: &FnDef, name: &str) -> bool {
    fn walk(expr: &Spanned<Expr>, name: &str) -> bool {
        if let Expr::Match { arms, .. } = &expr.node
            && arms
                .iter()
                .any(|arm| pattern_binds_name(&arm.pattern, name))
        {
            return true;
        }
        let mut found = false;
        crate::ir::chars_fusion::walk_children(expr, &mut |child| {
            found = found || walk(child, name);
        });
        found
    }
    fd.params.iter().any(|(n, _)| n == name)
        || fd.body.stmts().iter().any(|stmt| match stmt {
            Stmt::Binding(n, _, expr) => n == name || walk(expr, name),
            Stmt::Expr(expr) => walk(expr, name),
        })
}

/// Does an expression mention one of the list-builder intrinsics emitted by
/// the immediately preceding rewrite? This is an internal shape check for
/// byte retargeting, not a user-namespace collision guard.
fn mentions_list_builder_intrinsic(expr: &Spanned<Expr>) -> bool {
    if let Expr::Ident(n) | Expr::Resolved { name: n, .. } = &expr.node
        && BUILDER_INTRINSICS.contains(&n.as_str())
    {
        return true;
    }
    let mut found = false;
    crate::ir::chars_fusion::walk_children(expr, &mut |child| {
        found = found || mentions_list_builder_intrinsic(child);
    });
    found
}

/// Is `expr` a call to `name` — the bare-identifier callee shape every
/// site this pass synthesizes uses?
fn call_to<'e>(expr: &'e Expr, name: &str) -> Option<&'e Vec<Spanned<Expr>>> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    matches!(&callee.node, Expr::Ident(n) if n == name).then_some(args)
}

/// Is `expr` the `__lst_finalize(<acc>)` call the retarget replaces?
fn is_lst_finalize_of(expr: &Expr, acc_name: &str) -> bool {
    call_to(expr, "__lst_finalize").is_some_and(|args| {
        args.len() == 1 && matches!(&args[0].node, Expr::Ident(n) if n == acc_name)
    })
}

/// The payload of a `Result.<variant>(…)` wrapper in its pre-resolve
/// call shape.
fn result_wrapper_payload<'e>(expr: &'e Expr, variant: &str) -> Option<&'e Vec<Spanned<Expr>>> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    is_dotted_ident(&callee.node, "Result", variant).then_some(args)
}

/// Build `Result.<variant>(payload)` in the same pre-resolve call
/// shape the parser builds, stamped with the retargeted answer type.
fn result_wrapper(line: usize, variant: &str, payload: Spanned<Expr>) -> Spanned<Expr> {
    let base = sp_at(line, Expr::Ident("Result".to_string()));
    let callee = sp_at(line, Expr::Attr(Box::new(base), variant.to_string()));
    sp_at_typed(
        line,
        Expr::FnCall(Box::new(callee), vec![payload]),
        bytes_result_type(),
    )
}

/// A recognised variant call at a consumer site: the callee is a
/// retarget candidate (`acc_idx_of` knows its accumulator position)
/// and the accumulator argument is the `__lst_new` the list rewrite
/// put there. Returns the variant's name.
fn consumer_variant_call(
    expr: &Expr,
    acc_idx_of: &impl Fn(&str) -> Option<usize>,
) -> Option<String> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    let Expr::Ident(name) = &callee.node else {
        return None;
    };
    let acc_idx = acc_idx_of(name)?;
    let acc = args.get(acc_idx)?;
    call_to(&acc.node, "__lst_new")?;
    Some(name.clone())
}

/// Run the byte-sink stage: find every collected variant whose only
/// readers hand the result straight to the verified `fromList`,
/// retarget those variants' builders to bytes, and delete the
/// `fromList` calls. Declines are recorded per variant; a variant with
/// no `fromList`-shaped consumer is not a candidate and stays silent.
fn run_byte_sink_retarget(
    items: &mut [TopLevel],
    accepted: &HashMap<String, (String, ListBuildShape)>,
    variants: &mut [FnDef],
    report: &mut ListBuildPassReport,
) {
    if variants.is_empty() {
        return;
    }

    // What each variant looks like from the outside. Built from the
    // pass's own `accepted` map, so the accumulator identity is the
    // one the list rewrite actually used.
    let mut metas: HashMap<String, ByteSinkMeta> = HashMap::new();
    for (idx, variant) in variants.iter().enumerate() {
        let Some((_, shape)) = accepted.values().find(|(name, _)| *name == variant.name) else {
            continue;
        };
        let elem_ok = crate::types::parse_type_str(&shape.acc.ty)
            == crate::types::Type::List(Box::new(crate::types::Type::Int));
        // The KIND is about shape (a list, bare or behind a String-erring
        // Result); whether the elements are octet candidates is
        // `elem_ok`'s question, so a `List<String>` loop is recognised
        // as a candidate and declined with the element reason rather
        // than skipped in silence.
        let result_kind = match crate::types::parse_type_str(&variant.return_type) {
            crate::types::Type::Result(ok, err)
                if matches!(*ok, crate::types::Type::List(_))
                    && *err == crate::types::Type::Str =>
            {
                Some(true)
            }
            crate::types::Type::List(_) => Some(false),
            _ => None,
        };
        metas.insert(
            variant.name.clone(),
            ByteSinkMeta {
                variant_idx: idx,
                acc_idx: shape.acc.idx,
                acc_name: shape.acc.name.clone(),
                result_kind,
                elem_ok,
            },
        );
    }
    if metas.is_empty() {
        return;
    }

    // Sweep one: read-only. Count every read of every variant's name,
    // recognise the consumer sites, and remember the first
    // consumer-shaped interaction that cannot fuse.
    let mut facts: HashMap<String, ConsumerFacts> = HashMap::new();
    for fd in crate::ir::chars_fusion::fn_defs(items) {
        for name in metas.keys() {
            let reads: usize = fd
                .body
                .stmts()
                .iter()
                .map(|stmt| count_ident_reads(&crate::ir::chars_fusion::stmt_expr(stmt).node, name))
                .sum();
            facts.entry(name.clone()).or_default().occurrences += reads;
        }
        scan_consumer_fn(fd, &metas, &mut facts);
    }
    if facts.values().all(|f| f.sites == 0 && f.flaw.is_none()) {
        return;
    }

    // The identity the whole stage rests on, asked once per module and
    // only when a candidate exists: parsing the embedded module is not
    // free, and a module with no consumer shape owes nothing for it.
    let stdlib_ok = from_list_is_the_stdlib_one(items);

    // Sweep two: decide per variant, retarget the accepted ones.
    let mut retargeted: Vec<String> = Vec::new();
    let mut decided: Vec<(String, &'static str)> = Vec::new();
    for (name, fact) in &facts {
        if fact.sites == 0 && fact.flaw.is_none() {
            continue;
        }
        let meta = &metas[name];
        let decline = if !stdlib_ok {
            Some(ByteSinkDecline::ConsumerShape)
        } else if let Some(flaw) = fact.flaw {
            Some(flaw)
        } else if fact.occurrences != fact.sites {
            Some(ByteSinkDecline::MixedConsumers)
        } else if !meta.elem_ok {
            Some(ByteSinkDecline::ElemShape)
        } else {
            let result_kind = meta.result_kind.ok_or(ByteSinkDecline::ExitShape);
            result_kind
                .and_then(|rk| retarget_variant_to_bytes(&mut variants[meta.variant_idx], meta, rk))
                .err()
        };
        match decline {
            Some(reason) => decided.push((name.clone(), reason.reason())),
            None => retargeted.push(name.clone()),
        }
    }
    report.byte_declined.extend(decided);

    if retargeted.is_empty() {
        return;
    }

    // Sweep three: move every consumer site of a retargeted variant
    // onto the byte builder and delete its `fromList` call.
    let retargeted_metas: HashMap<String, &ByteSinkMeta> = retargeted
        .iter()
        .map(|name| (name.clone(), &metas[name]))
        .collect();
    let mut deleted = 0usize;
    for fd in crate::ir::chars_fusion::fn_defs_mut(items) {
        deleted += rewrite_consumer_fn(fd, &retargeted_metas);
    }
    report.byte_retargets = deleted;
    report.byte_fns = retargeted;
    report.byte_fns.sort();
}

/// Recognise the consumer shapes inside one fn, crediting each
/// variant's facts. Three spellings, one meaning:
///
/// - `v = <variant>(…, __lst_new(0))?` immediately before a final
///   `fromList(v)` (the standard library's own `fromHex`);
/// - a final `fromList(<variant>(…, __lst_new(0))?)`, the same thing
///   without the binding;
/// - `fromList(<variant>(…, __lst_new(0)))` anywhere, for a variant
///   that answers the bare list — a pure value rewrite, so position
///   does not matter.
///
/// The `?` spellings are position-bound: the `?` returns the loop's
/// parse errors from the CONSUMER early, and after fusion they become
/// the call's value — identical only where that value is the fn's
/// answer. A `?` spelling anywhere else is recorded as the flaw it is.
fn scan_consumer_fn(
    fd: &FnDef,
    metas: &HashMap<String, ByteSinkMeta>,
    facts: &mut HashMap<String, ConsumerFacts>,
) {
    // A local binder wearing the consumer's name makes every
    // recognition in this fn about the binder, not the verified
    // module fn — so every consumer-shaped site here is a flaw.
    let shadowed = fn_binds_name(fd, FROM_LIST);
    let mut credit = |name: &str, outcome: Result<(), ByteSinkDecline>| {
        let entry = facts.entry(name.to_string()).or_default();
        match outcome {
            Ok(()) if shadowed => {
                entry.flaw.get_or_insert(ByteSinkDecline::ConsumerShape);
            }
            Ok(()) => entry.sites += 1,
            Err(flaw) => {
                entry.flaw.get_or_insert(flaw);
            }
        }
    };
    let acc_idx_of = |name: &str| metas.get(name).map(|m| m.acc_idx);

    let stmts = fd.body.stmts();
    let tail_from_list: Option<&Vec<Spanned<Expr>>> = match stmts.last() {
        Some(Stmt::Expr(expr)) => call_to(&expr.node, FROM_LIST).filter(|args| args.len() == 1),
        _ => None,
    };

    // The binding spelling. Adjacency is part of the shape: the `?`
    // must fire exactly where the answer is produced.
    let mut tail_credited = false;
    if let Some(args) = tail_from_list
        && let Expr::Ident(bound) = &args[0].node
    {
        let adjacent = stmts.len().checked_sub(2).map(|i| &stmts[i]);
        if let Some(Stmt::Binding(name, _, value)) = adjacent
            && name == bound
            && let Expr::ErrorProp(inner) = &value.node
            && let Some(variant) = consumer_variant_call(&inner.node, &acc_idx_of)
        {
            if metas[&variant].result_kind != Some(true) {
                // A `?` on a variant that does not answer a Result is
                // not a program the typechecker passes; leave the read
                // uncredited and let the occurs accounting decline it.
            } else {
                let reads: usize = stmts
                    .iter()
                    .map(|stmt| {
                        count_ident_reads(&crate::ir::chars_fusion::stmt_expr(stmt).node, bound)
                    })
                    .sum();
                let bindings = stmts
                    .iter()
                    .filter(|stmt| matches!(stmt, Stmt::Binding(n, _, _) if n == bound))
                    .count();
                let pattern_shadow = fn_binds_name_in_patterns(fd, bound);
                if reads == 1 && bindings == 1 && !pattern_shadow {
                    tail_credited = true;
                    credit(&variant, Ok(()));
                } else {
                    credit(&variant, Err(ByteSinkDecline::SecondReader));
                }
            }
        } else {
            // `fromList(v)` is the answer but `v` is not the adjacent
            // collected binding. If some earlier binding collected into
            // `v`, this is the consumer we cannot move.
            for stmt in stmts {
                if let Stmt::Binding(name, _, value) = stmt
                    && name == bound
                    && let Expr::ErrorProp(inner) = &value.node
                    && let Some(variant) = consumer_variant_call(&inner.node, &acc_idx_of)
                {
                    credit(&variant, Err(ByteSinkDecline::NotTheAnswer));
                }
            }
        }
    }

    // The inline-`?` spelling at the answer.
    if let Some(args) = tail_from_list
        && let Expr::ErrorProp(inner) = &args[0].node
        && let Some(variant) = consumer_variant_call(&inner.node, &acc_idx_of)
        && metas[&variant].result_kind == Some(true)
    {
        tail_credited = true;
        credit(&variant, Ok(()));
    }

    // The direct spelling, anywhere — and any inline-`?` spelling that
    // is NOT at the answer, which is the flaw the walk reports.
    let tail_expr: Option<*const Expr> = match stmts.last() {
        Some(Stmt::Expr(expr)) => Some(&expr.node as *const Expr),
        _ => None,
    };
    for stmt in stmts {
        scan_direct_sites(
            crate::ir::chars_fusion::stmt_expr(stmt),
            metas,
            &mut credit,
            tail_expr,
            tail_credited,
        );
    }
}

/// Pattern binders only — the statement-binding half is counted by the
/// caller, which needs the two separately.
fn fn_binds_name_in_patterns(fd: &FnDef, name: &str) -> bool {
    fn walk(expr: &Spanned<Expr>, name: &str) -> bool {
        if let Expr::Match { arms, .. } = &expr.node
            && arms
                .iter()
                .any(|arm| pattern_binds_name(&arm.pattern, name))
        {
            return true;
        }
        let mut found = false;
        crate::ir::chars_fusion::walk_children(expr, &mut |child| {
            found = found || walk(child, name);
        });
        found
    }
    fd.body
        .stmts()
        .iter()
        .any(|stmt| walk(crate::ir::chars_fusion::stmt_expr(stmt), name))
}

/// Walk one statement's tree for `fromList(<variant call>)` and
/// misplaced `fromList(<variant call>?)` spellings.
fn scan_direct_sites(
    expr: &Spanned<Expr>,
    metas: &HashMap<String, ByteSinkMeta>,
    credit: &mut impl FnMut(&str, Result<(), ByteSinkDecline>),
    tail_expr: Option<*const Expr>,
    tail_already_credited: bool,
) {
    let acc_idx_of = |name: &str| metas.get(name).map(|m| m.acc_idx);
    if let Some(args) = call_to(&expr.node, FROM_LIST)
        && args.len() == 1
    {
        let is_tail = tail_expr == Some(&expr.node as *const Expr);
        match &args[0].node {
            Expr::FnCall(_, _) => {
                // A direct application to a Result-answering variant
                // never typechecks; the read stays uncredited and the
                // occurs accounting declines.
                if let Some(variant) = consumer_variant_call(&args[0].node, &acc_idx_of)
                    && metas[&variant].result_kind == Some(false)
                {
                    credit(&variant, Ok(()));
                }
            }
            Expr::ErrorProp(inner) => {
                if let Some(variant) = consumer_variant_call(&inner.node, &acc_idx_of)
                    && !(is_tail && tail_already_credited)
                {
                    credit(&variant, Err(ByteSinkDecline::NotTheAnswer));
                }
            }
            _ => {}
        }
    }
    crate::ir::chars_fusion::walk_children(expr, &mut |child| {
        scan_direct_sites(child, metas, credit, tail_expr, tail_already_credited);
    });
}

/// Rewrite every consumer site of the retargeted variants inside one
/// fn, returning how many `fromList` calls were deleted. Mirrors the
/// recognition above exactly — recognition already proved each shape,
/// so this is application, not a second opinion.
fn rewrite_consumer_fn(fd: &mut FnDef, metas: &HashMap<String, &ByteSinkMeta>) -> usize {
    let mut deleted = 0usize;
    let body = Arc::make_mut(&mut fd.body);
    let stmts = body.stmts_mut();
    let acc_idx_of = |name: &str| metas.get(name).map(|m| m.acc_idx);

    // The binding spelling: drop the binding, make the retargeted call
    // the answer the `fromList` call was.
    let tail_is_from_list_of = |stmts: &[Stmt]| -> Option<String> {
        match stmts.last() {
            Some(Stmt::Expr(expr)) => {
                let args = call_to(&expr.node, FROM_LIST).filter(|a| a.len() == 1)?;
                match &args[0].node {
                    Expr::Ident(bound) => Some(bound.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    };
    if let Some(bound) = tail_is_from_list_of(stmts)
        && stmts.len() >= 2
    {
        let binding_idx = stmts.len() - 2;
        let matches_shape = matches!(
            &stmts[binding_idx],
            Stmt::Binding(name, _, value)
                if *name == bound
                    && matches!(&value.node, Expr::ErrorProp(inner)
                        if consumer_variant_call(&inner.node, &acc_idx_of).is_some())
        );
        if matches_shape {
            let Stmt::Binding(_, _, value) = stmts.remove(binding_idx) else {
                unreachable!("matched a binding one line above");
            };
            let Expr::ErrorProp(inner) = value.node else {
                unreachable!("matched an error propagation one line above");
            };
            let Some(Stmt::Expr(tail)) = stmts.last_mut() else {
                unreachable!("the tail was matched as the fromList call");
            };
            *tail = retargeted_call(*inner, tail, metas);
            deleted += 1;
        }
    }

    // The inline-`?` spelling at the answer.
    if let Some(Stmt::Expr(tail)) = stmts.last_mut()
        && call_to(&tail.node, FROM_LIST)
            .filter(|a| a.len() == 1)
            .is_some_and(|args| {
                matches!(&args[0].node, Expr::ErrorProp(inner)
                    if consumer_variant_call(&inner.node, &acc_idx_of).is_some())
            })
    {
        let Expr::FnCall(_, mut args) =
            std::mem::replace(&mut tail.node, Expr::Ident(String::new()))
        else {
            unreachable!("matched a call one line above");
        };
        let Expr::ErrorProp(inner) = args.remove(0).node else {
            unreachable!("matched an error propagation one line above");
        };
        *tail = retargeted_call(*inner, tail, metas);
        deleted += 1;
    }

    // The direct spelling, anywhere.
    for stmt in stmts.iter_mut() {
        rewrite_direct_sites(
            crate::ir::chars_fusion::stmt_expr_mut(stmt),
            metas,
            &mut deleted,
        );
    }
    deleted
}

/// Walk one statement's tree rewriting `fromList(<variant call>)` in
/// place. The rewritten call's arguments are walked too — a nested
/// consumer site inside them was recognised by the scan and owes the
/// same rewrite.
fn rewrite_direct_sites(
    expr: &mut Spanned<Expr>,
    metas: &HashMap<String, &ByteSinkMeta>,
    deleted: &mut usize,
) {
    let acc_idx_of = |name: &str| metas.get(name).map(|m| m.acc_idx);
    let is_site = call_to(&expr.node, FROM_LIST)
        .filter(|args| args.len() == 1)
        .is_some_and(|args| {
            matches!(&args[0].node, Expr::FnCall(_, _))
                && consumer_variant_call(&args[0].node, &acc_idx_of).is_some()
        });
    if is_site {
        let Expr::FnCall(_, mut args) =
            std::mem::replace(&mut expr.node, Expr::Ident(String::new()))
        else {
            unreachable!("matched a call one line above");
        };
        let inner = args.remove(0);
        *expr = retargeted_call(inner, expr, metas);
        *deleted += 1;
    }
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| {
        rewrite_direct_sites(child, metas, deleted);
    });
}

/// The retargeted call a consumer site becomes: the variant call with
/// its `__lst_new` swapped for `__byt_new`, carrying the type the
/// `fromList` call it replaces was stamped with — the value is the
/// same `Result<Bytes, String>`, produced one walk earlier.
fn retargeted_call(
    mut call: Spanned<Expr>,
    replaced: &Spanned<Expr>,
    metas: &HashMap<String, &ByteSinkMeta>,
) -> Spanned<Expr> {
    let line = call.line;
    let Expr::FnCall(callee, args) = &mut call.node else {
        unreachable!("consumer sites are calls by construction");
    };
    let Expr::Ident(name) = &callee.node else {
        unreachable!("consumer sites call the variant by bare name");
    };
    let meta = &metas[name];
    args[meta.acc_idx] = byte_intrinsic_call(
        line,
        "__byt_new",
        vec![sp_at_typed(
            line,
            Expr::Literal(Literal::Int(0)),
            crate::types::Type::Int,
        )],
    );
    let fresh = sp_at(line, call.node);
    if let Some(ty) = replaced.ty() {
        fresh.set_ty(ty.clone());
    }
    fresh
}

/// Build `<intrinsic>(args…)` stamped with the compiler-owned byte-builder
/// carrier — what every `__byt_new` / `__byt_push` answers with.
fn byte_intrinsic_call(line: usize, name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    let call = intrinsic_call(line, name, args);
    call.set_ty(crate::types::Type::named(INTERNAL_BYTE_BUILDER_TYPE));
    call
}

/// The type `__byt_finalize` answers with. The successful payload is a
/// compiler-owned validated byte sequence: Rust represents it as a packed
/// `AverIntList`, wasm-gc as the nominal packed `Bytes` array, and the VM as
/// its ordinary semantic list. The synthesized record boundary remains
/// visible in IR without forcing another validation traversal.
fn byte_finalize_type() -> crate::types::Type {
    crate::types::Type::Result(
        Box::new(crate::types::Type::named(INTERNAL_BYTE_PAYLOAD_TYPE)),
        Box::new(crate::types::Type::Str),
    )
}

/// The type the retargeted variant answers with.
fn bytes_result_type() -> crate::types::Type {
    crate::types::Type::Result(
        Box::new(crate::types::Type::named(BYTES_RECORD)),
        Box::new(crate::types::Type::Str),
    )
}

/// The exit the retarget writes where `Result.Ok(__lst_finalize(acc))`
/// (or the bare finalize) stood:
///
/// ```aver
/// match __byt_finalize(acc)
///     Result.Ok(__byt_vals) -> Result.Ok(Bytes(values = __byt_vals))
///     Result.Err(__byt_msg) -> Result.Err(__byt_msg)
/// ```
///
/// The finalizer answers what `fromList` would have; the match wraps
/// its `Ok` into the record IN SYNTHESIZED AST, so the record's
/// identity is resolved by the same machinery that resolved it inside
/// `fromList` — no backend learns the record's shape from an
/// intrinsic. The binders live in the guarded `__byt_` namespace, so a
/// program that could capture them was declined before this runs.
fn byte_finalize_exit(line: usize, acc_name: &str) -> Spanned<Expr> {
    let subject = intrinsic_call(
        line,
        "__byt_finalize",
        vec![sp_at_typed(
            line,
            Expr::Ident(acc_name.to_string()),
            crate::types::Type::named(INTERNAL_BYTE_BUILDER_TYPE),
        )],
    );
    subject.set_ty(byte_finalize_type());

    let vals = sp_at_typed(
        line,
        Expr::Ident("__byt_vals".to_string()),
        crate::types::Type::named(INTERNAL_BYTE_PAYLOAD_TYPE),
    );
    let record = sp_at_typed(
        line,
        Expr::RecordCreate {
            type_name: BYTES_RECORD.to_string(),
            fields: vec![("values".to_string(), vals)],
        },
        crate::types::Type::named(BYTES_RECORD),
    );
    let ok_body = result_wrapper(line, "Ok", record);

    let msg = sp_at_typed(
        line,
        Expr::Ident("__byt_msg".to_string()),
        crate::types::Type::Str,
    );
    let err_body = result_wrapper(line, "Err", msg);

    sp_at_typed(
        line,
        Expr::Match {
            subject: Box::new(subject),
            arms: vec![
                MatchArm::new(
                    Pattern::Constructor("Result.Ok".to_string(), vec!["__byt_vals".to_string()]),
                    ok_body,
                ),
                MatchArm::new(
                    Pattern::Constructor("Result.Err".to_string(), vec!["__byt_msg".to_string()]),
                    err_body,
                ),
            ],
        },
        bytes_result_type(),
    )
}

/// Turn one collected variant's list builder into the byte builder, or
/// say why it cannot be done. Check and rewrite are ONE traversal over
/// a copy of the body, exactly like the list rewrite above: the walk
/// either accounts for every `__lst_` occurrence — the push in each
/// self-call, the finalize at each exit — or the copy is thrown away
/// and the variant keeps its list.
fn retarget_variant_to_bytes(
    variant: &mut FnDef,
    meta: &ByteSinkMeta,
    result_kind: bool,
) -> Result<(), ByteSinkDecline> {
    let FnBody::Block(stmts) = variant.body.as_ref();
    let mut stmts = stmts.clone();
    let Some((last, leading)) = stmts.split_last_mut() else {
        return Err(ByteSinkDecline::ExitShape);
    };
    // The list rewrite put builder calls only where the accumulator
    // was read, and it allowed no reads outside the answer — so a
    // builder mention in a leading statement is a shape this stage
    // does not know, not a shape to improvise over.
    for stmt in leading.iter() {
        if mentions_list_builder_intrinsic(crate::ir::chars_fusion::stmt_expr(stmt)) {
            return Err(ByteSinkDecline::ExitShape);
        }
    }
    let Stmt::Expr(result) = last else {
        return Err(ByteSinkDecline::ExitShape);
    };
    let mut exits = 0usize;
    retarget_exit_expr(result, &variant.name, meta, result_kind, &mut exits)?;
    if exits == 0 {
        return Err(ByteSinkDecline::ExitShape);
    }

    variant.body = Arc::new(FnBody::Block(stmts));
    variant.params[meta.acc_idx].1 = INTERNAL_BYTE_BUILDER_TYPE.to_string();
    variant.return_type = "Result<Bytes, String>".to_string();
    variant.desc = Some(format!(
        "{} Its only reader handed the collected list straight to the \
         standard library's `fromList`, so the builder collects bytes: \
         the range check rides every push, and the exits answer the \
         `Result<Bytes, String>` the pair used to compute.",
        variant.desc.as_deref().unwrap_or_default()
    ));
    Ok(())
}

/// Rewrite one result-position expression of a collected variant to
/// the byte builder, counting the exits it retargets. Every leaf must
/// be a shape this stage knows:
///
/// - a match — the branching, walked arm by arm;
/// - the self tail-call, whose accumulator argument is the push;
/// - the finalize exit (`Result.Ok(__lst_finalize(acc))` when the
///   variant answers a Result, the bare finalize when it answers the
///   list) — replaced by [`byte_finalize_exit`];
/// - for a Result variant, a `Result.Err(…)` exit that never touches
///   the builder — the parse errors, which pass through the consumer's
///   `?` and the fused answer identically.
///
/// Anything else is an exit `fromList` would have validated and the
/// byte builder never saw — the loop keeps its list.
fn retarget_exit_expr(
    expr: &mut Spanned<Expr>,
    variant_name: &str,
    meta: &ByteSinkMeta,
    result_kind: bool,
    exits: &mut usize,
) -> Result<(), ByteSinkDecline> {
    let line = expr.line;

    if let Expr::Match { subject, arms } = &mut expr.node {
        if mentions_list_builder_intrinsic(subject) {
            return Err(ByteSinkDecline::ExitShape);
        }
        for arm in arms.iter_mut() {
            retarget_exit_expr(&mut arm.body, variant_name, meta, result_kind, exits)?;
        }
        *expr = sp_at_typed(line, expr.node.clone(), bytes_result_type());
        return Ok(());
    }

    if let Expr::TailCall(data) = &mut expr.node {
        if data.target != variant_name {
            return Err(ByteSinkDecline::ExitShape);
        }
        for (idx, arg) in data.args.iter_mut().enumerate() {
            if idx == meta.acc_idx {
                let push_args = match call_to(&arg.node, "__lst_push") {
                    Some(args)
                        if args.len() == 2
                            && matches!(&args[0].node, Expr::Ident(n) if *n == meta.acc_name)
                            && !mentions_list_builder_intrinsic(&args[1]) =>
                    {
                        args.clone()
                    }
                    _ => return Err(ByteSinkDecline::ExitShape),
                };
                let elem = push_args.into_iter().nth(1).expect("two args matched");
                *arg = byte_intrinsic_call(
                    line,
                    "__byt_push",
                    vec![
                        sp_at_typed(
                            line,
                            Expr::Ident(meta.acc_name.clone()),
                            crate::types::Type::named(INTERNAL_BYTE_BUILDER_TYPE),
                        ),
                        elem,
                    ],
                );
            } else if mentions_list_builder_intrinsic(arg) {
                return Err(ByteSinkDecline::ExitShape);
            }
        }
        *expr = sp_at_typed(line, expr.node.clone(), bytes_result_type());
        return Ok(());
    }

    // At this stage of the pipeline `Result.Ok(x)` is still the call
    // shape the parser built — the resolver classifies constructors
    // later — so the wrapper is matched the way `List.reverse` is
    // matched everywhere else in this pass.
    if result_kind {
        if let Some(args) = result_wrapper_payload(&expr.node, "Ok") {
            if args.len() == 1 && is_lst_finalize_of(&args[0].node, &meta.acc_name) {
                *expr = byte_finalize_exit(line, &meta.acc_name);
                *exits += 1;
                return Ok(());
            }
            return Err(ByteSinkDecline::ExitShape);
        }
        if let Some(args) = result_wrapper_payload(&expr.node, "Err") {
            if args.len() == 1 && !mentions_list_builder_intrinsic(&args[0]) {
                *expr = sp_at_typed(line, expr.node.clone(), bytes_result_type());
                return Ok(());
            }
            return Err(ByteSinkDecline::ExitShape);
        }
        return Err(ByteSinkDecline::ExitShape);
    }

    if is_lst_finalize_of(&expr.node, &meta.acc_name) {
        *expr = byte_finalize_exit(line, &meta.acc_name);
        *exits += 1;
        return Ok(());
    }
    Err(ByteSinkDecline::ExitShape)
}

/// Build `<intrinsic>(args…)` carrying `ty`. The builder intrinsics all
/// answer with the accumulator's own list type, which is what keeps the
/// rewritten body typed exactly as the one it replaced.
fn list_intrinsic_call(
    line: usize,
    name: &str,
    args: Vec<Spanned<Expr>>,
    ty: crate::types::Type,
) -> Spanned<Expr> {
    let call = intrinsic_call(line, name, args);
    call.set_ty(ty);
    call
}

/// Move every call that starts a recognised loop's accumulator at `[]`
/// onto that loop's collected variant, counting them per loop.
///
/// Outermost first, so a `List.reverse(<bare-acc loop>(xs, []))` is seen
/// as the one expression it is before either half is descended into.
fn rewrite_list_build_sites(
    expr: &mut Spanned<Expr>,
    accepted: &HashMap<String, (String, ListBuildShape)>,
    fired: &mut HashMap<String, usize>,
) {
    if let Some((loop_name, replacement)) = try_rewrite_list_build_site(expr, accepted) {
        *expr = replacement;
        *fired.entry(loop_name).or_default() += 1;
    }
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| {
        rewrite_list_build_sites(child, accepted, fired);
    });
}

/// The rewritten form of a call site, when `expr` is one.
///
/// Which spelling counts depends on where the loop puts its reverse, and
/// the two are mutually exclusive on purpose. A `FinalizeInBase` loop
/// already hands back the list forwards, so the call site is the bare
/// call. A `BareAccBase` loop hands it back reversed and its caller
/// reverses; the builder produces the forward list, so only a call site
/// wearing that reverse can be moved — a bare one asked for the elements
/// backwards and still gets them.
pub(super) fn try_rewrite_list_build_site(
    expr: &Spanned<Expr>,
    accepted: &HashMap<String, (String, ListBuildShape)>,
) -> Option<(String, Spanned<Expr>)> {
    let line = expr.line;
    let (inner, peeled_reverse) = match &expr.node {
        Expr::FnCall(callee, args)
            if is_dotted_ident(&callee.node, "List", "reverse") && args.len() == 1 =>
        {
            (&args[0], true)
        }
        _ => (expr, false),
    };
    let Expr::FnCall(callee, args) = &inner.node else {
        return None;
    };
    let Expr::Ident(name) = &callee.node else {
        return None;
    };
    let (new_name, shape) = accepted.get(name)?;
    let wants_reverse = matches!(shape.kind, ListBuildKind::BareAccBase);
    if wants_reverse != peeled_reverse {
        return None;
    }
    let acc_arg = args.get(shape.acc.idx)?;
    // A non-empty starting accumulator is elements the builder would
    // never hold, and the rewrite has nowhere to put them.
    if !matches!(&acc_arg.node, Expr::List(items) if items.is_empty()) {
        return None;
    }
    let acc_ty = crate::types::parse_type_str(&shape.acc.ty);
    let mut new_args = args.clone();
    new_args[shape.acc.idx] = list_intrinsic_call(
        inner.line,
        "__lst_new",
        vec![sp_at_typed(
            inner.line,
            Expr::Literal(Literal::Int(0)),
            crate::types::Type::Int,
        )],
        acc_ty,
    );
    let call = sp_at(
        line,
        Expr::FnCall(
            Box::new(sp_at(inner.line, Expr::Ident(new_name.clone()))),
            new_args,
        ),
    );
    // The collected variant answers with the same type the fn it was
    // built from answers with, so whatever the original call site was
    // stamped with is still true of the new one. Nothing is invented
    // when it carried no type.
    if let Some(ty) = inner.ty() {
        call.set_ty(ty.clone());
    }
    Some((name.clone(), call))
}
