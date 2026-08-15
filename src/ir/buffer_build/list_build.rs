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

use std::collections::HashMap;
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
    /// The `<fn>__collected` name is already taken, or the program
    /// already uses the `__lst_` namespace.
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
            Self::NameTaken => "the __collected variant name or the __lst_ namespace is taken",
        }
    }
}

/// What the list-build pass did. Every field is a fact a CI gate can
/// assert on without parsing prose.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ListBuildPassReport {
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
}

/// Suffix distinguishing the synthesized variant from the loop it was
/// built from.
const COLLECTED_SUFFIX: &str = "__collected";

/// Prefix of every intrinsic this pass emits. A leading `__` is not
/// reserved — a program can bind `__lst_push` and a binder is exactly
/// what would shadow the intrinsic for the code underneath it — so the
/// pass steps aside rather than risk emitting a call to the user's name.
const BUILDER_PREFIX: &str = "__lst_";

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
    let mut report = ListBuildPassReport::default();
    let candidates: Vec<(String, ListBuildAcc)> = list_build_candidates(items);
    if candidates.is_empty() {
        return report;
    }

    let taken = crate::ir::chars_fusion::taken_names(items);
    // One name in the program's own binders is enough to take the
    // namespace away from every loop at once: the intrinsics are emitted
    // by name, and a name that means something else anywhere it is in
    // scope is a name this pass cannot spell.
    let namespace_taken = taken.iter().any(|name| {
        name.starts_with(BUILDER_PREFIX) || BUILDER_INTRINSICS.contains(&name.as_str())
    });

    let mut accepted: HashMap<String, (String, ListBuildShape)> = HashMap::new();
    let mut variants: Vec<FnDef> = Vec::new();
    for (name, acc) in candidates {
        let new_name = format!("{name}{COLLECTED_SUFFIX}");
        let outcome = if namespace_taken || taken.contains(&new_name) {
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
    items.reserve(variants.len());
    for variant in variants {
        items.push(TopLevel::FnDef(variant));
    }
    report
}

/// Is there a collecting loop here at all? Read-only, so a caller that
/// has to decide whether to COPY a module before running the pass can
/// ask first — the VM re-parses every dependency and re-runs the pass on
/// its own copy.
pub fn has_list_build_shape(items: &[TopLevel]) -> bool {
    crate::ir::chars_fusion::fn_defs(items).any(|fd| list_build_acc_of(fd).is_some())
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
fn list_build_acc_of(fd: &FnDef) -> Option<ListBuildAcc> {
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
fn build_collected_variant(
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
fn try_rewrite_list_build_site(
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
