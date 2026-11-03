//! A self-tail loop's copy over machine words (`bare_i64::WordLoop`).
//!
//! The fn holds the loop over words; its ordinary loop is a cold fn of the
//! same signature beside it. On entry each word is narrowed from its boxed
//! param, and a value past `i64` goes to the ordinary loop with the state as
//! it is ([`emit_prologue`]). Inside, a self-tail call steps each word
//! checked ([`emit_step`]): a pure tree of `+`, `-`, `*` and negation over
//! the words, raw locals and literals runs on `i64` with every operation
//! checked, and any other value is narrowed with `to_i64`. A step whose
//! result leaves `i64` is computed again exactly, on `AverInt`, and the
//! whole state, every word boxed, goes to the ordinary loop ([`emit_rebind`]),
//! which carries on from it. Both compute the same integers: `i64`
//! arithmetic that does not overflow is integer arithmetic, and the loop's
//! steps are pure, so the ordinary loop sees exactly the state the copy had.

use crate::ir::mir::bare_i64::checked_tree_shape;
use crate::ir::mir::int_tree::{self, IntOp, int_literal};

use super::*;

/// The word of param `i` as narrowed on entry.
fn entry_word(i: usize) -> String {
    generated_ident(&format!("word{i}"))
}

/// Param `i` as the ordinary loop takes it: a word boxed (`word`, when the
/// step gives it), the position checked on entry boxed again, and every
/// other param as it is.
fn state_arg(
    i: usize,
    params: &[(String, String)],
    words: &[usize],
    checked: Option<usize>,
    word: Option<String>,
) -> String {
    let name = aver_name_to_rust(&params[i].0);
    match word {
        Some(word) if words.contains(&i) => word,
        _ if words.contains(&i) || checked == Some(i) => {
            format!("aver_rt::AverInt::from_i64({name})")
        }
        _ => name,
    }
}

/// `items` as one value: the item alone, or a tuple of them.
fn tuple(items: impl Iterator<Item = String>) -> String {
    let items: Vec<String> = items.collect();
    match items.len() {
        1 => items.into_iter().next().unwrap_or_default(),
        _ => format!("({})", items.join(", ")),
    }
}

/// The words narrowed from their params; a param past the word sends the
/// state as it is to the ordinary loop `fallback`. The words shadow their
/// params. `checked` is the position checked on entry, an `i64` by now.
pub(super) fn emit_prologue(
    words: &[usize],
    params: &[(String, String)],
    fallback: &str,
    checked: Option<usize>,
) -> Vec<String> {
    let names: Vec<String> = words
        .iter()
        .map(|&i| aver_name_to_rust(&params[i].0))
        .collect();
    let mut lines = Vec::new();
    for (k, &i) in words.iter().enumerate() {
        lines.push(format!(
            "    let {} = {}.to_i64();",
            entry_word(i),
            names[k]
        ));
    }
    let state = (0..params.len())
        .map(|i| {
            state_arg(
                i,
                params,
                words,
                checked,
                Some(aver_name_to_rust(&params[i].0)),
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    lines.push(format!(
        "    let {} = {} else {{ return {fallback}({state}) }};",
        tuple(names.iter().map(|n| format!("Some(mut {n})"))),
        tuple(words.iter().map(|&i| entry_word(i))),
    ));
    lines
}

/// A self-tail call's argument at a word's position, as
/// `Result<i64, aver_rt::AverInt>`: the word stepped, or the exact value
/// when the step leaves `i64`. The rewrite narrowed a boxed argument
/// (`Unbox`): a pure tree runs checked on the words and raw leaves, with
/// its exact `AverInt` form computed only on an overflow; any other value
/// is narrowed as it is. A raw argument is a word already.
pub(super) fn emit_step(a: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    let w = generated_ident("stepped");
    if let MirExpr::Unbox(inner) = &a.node {
        if checked_tree_shape(inner, ctx.mir_builtins)
            && let Some(fast) = checked(inner, ctx)
        {
            let exact = exact(inner, ctx).or_else(|| emit_mir_expr(inner, ctx))?;
            return Some(format!(
                "match (|| -> Option<i64> {{ Some({fast}) }})() {{ Some({w}) => Ok({w}), None => Err({exact}) }}"
            ));
        }
        // Any other value is taken as a boxed param takes it: cloned where
        // the clone policy says the loop still reads it.
        let boxed = mir_clone_arg(emit_mir_expr(inner, ctx)?, &inner.node, ctx);
        return Some(narrowed(&boxed));
    }
    let raw = emit_bare_i64(a, ctx).or_else(|| emit_mir_expr(a, ctx))?;
    Some(format!("Ok({raw})"))
}

/// A boxed value narrowed to its word, or kept when it has none.
fn narrowed(boxed: &str) -> String {
    let value = generated_ident("boxed");
    let w = generated_ident("stepped");
    format!(
        "{{ let {value} = {boxed}; match {value}.to_i64() {{ Some({w}) => Ok({w}), None => Err({value}) }} }}"
    )
}

/// The tree `e` on `i64`, each operation checked (`?` leaves the closure
/// [`emit_step`] runs it in). A raw leaf is read as it is; a boxed local is
/// narrowed in place, so nothing is moved before the exact path reads it.
fn checked(e: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    if let Some((op, a, b)) = int_tree::op_of(e, ctx.mir_builtins) {
        let x = checked(a, ctx)?;
        return Some(match op {
            IntOp::Add => format!("({x}).checked_add({})?", checked(b?, ctx)?),
            IntOp::Sub => format!("({x}).checked_sub({})?", checked(b?, ctx)?),
            IntOp::Mul => format!("({x}).checked_mul({})?", checked(b?, ctx)?),
            IntOp::Neg => format!("({x}).checked_neg()?"),
            _ => return None,
        });
    }
    if let Some(k) = int_literal(&e.node) {
        return Some(format!("{k}i64"));
    }
    match &e.node {
        MirExpr::Box(inner) => Some(format!("({})", emit_mir_expr(inner, ctx)?)),
        MirExpr::Local(local) if mir_expr_is_bare_i64(e, ctx) => {
            let name = &local.node.name;
            (!name.is_empty()).then(|| aver_name_to_rust(name))
        }
        MirExpr::Local(local) => {
            let name = &local.node.name;
            if name.is_empty() || ctx.is_rc_wrapped(name) {
                return None;
            }
            Some(format!("({}).to_i64()?", aver_name_to_rust(name)))
        }
        _ => None,
    }
}

/// The tree `e` on `AverInt`, exactly, for a step the words do not hold:
/// the same leaves [`checked`] read, each boxed, under the operations as
/// `Int` computes them. Rendered here rather than by the ordinary emitter so
/// the cold path is one expression, not a second word speculation.
fn exact(e: &Spanned<MirExpr>, ctx: &MirEmitCtx<'_>) -> Option<String> {
    if let Some((op, a, b)) = int_tree::op_of(e, ctx.mir_builtins) {
        let x = exact(a, ctx)?;
        return Some(match op {
            IntOp::Add => format!("({x}).add(&({}))", exact(b?, ctx)?),
            IntOp::Sub => format!("({x}).sub(&({}))", exact(b?, ctx)?),
            IntOp::Mul => format!("({x}).mul(&({}))", exact(b?, ctx)?),
            IntOp::Neg => format!("({x}).neg()"),
            _ => return None,
        });
    }
    if let Some(k) = int_literal(&e.node) {
        return Some(format!("aver_rt::AverInt::from_i64({k}i64)"));
    }
    match &e.node {
        MirExpr::Box(inner) => Some(format!(
            "aver_rt::AverInt::from_i64({})",
            emit_mir_expr(inner, ctx)?
        )),
        MirExpr::Local(local) if mir_expr_is_bare_i64(e, ctx) => {
            let name = &local.node.name;
            (!name.is_empty())
                .then(|| format!("aver_rt::AverInt::from_i64({})", aver_name_to_rust(name)))
        }
        MirExpr::Local(local) => {
            let name = &local.node.name;
            if name.is_empty() || ctx.is_rc_wrapped(name) {
                return None;
            }
            Some(aver_name_to_rust(name))
        }
        _ => None,
    }
}

/// The rebinding of the words a self-tail call steps (`stepped`, each with
/// its `Result` in its `__tco` temp): all of them when every step fit, or
/// the call of the ordinary loop `fallback` with the state as the call
/// leaves it, every word boxed: the stepped ones from their results, the
/// rest from their current values. The other params were assigned already.
pub(super) fn emit_rebind(
    stepped: &[usize],
    params: &[(String, String)],
    fallback: &str,
    ctx: &MirEmitCtx<'_>,
) -> String {
    let temp = |i: usize| generated_ident(&format!("tco{i}"));
    let fit = |i: usize| generated_ident(&format!("fit{i}"));
    let result = |i: usize| generated_ident(&format!("result{i}"));
    let w = generated_ident("stepped");
    let b = generated_ident("boxed");
    let words = ctx.loop_words;
    // The position checked on entry: an `i64` of the loop the signature
    // keeps boxed, and not a word.
    let checked = (0..params.len()).find(|&i| {
        ctx.bare.loop_param_is_bare(i) && !ctx.bare.param_is_bare(i) && !words.contains(&i)
    });
    let assigned: Vec<String> = stepped
        .iter()
        .map(|&i| format!("{} = {};", aver_name_to_rust(&params[i].0), fit(i)))
        .collect();
    let state = (0..params.len())
        .map(|i| {
            let word = stepped.contains(&i).then(|| {
                format!(
                    "match {} {{ Ok({w}) => aver_rt::AverInt::from_i64({w}), Err({b}) => {b} }}",
                    result(i)
                )
            });
            state_arg(i, params, words, checked, word)
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "            match {} {{ {} => {{ {} }} {} => return {fallback}({state}) }}",
        tuple(stepped.iter().map(|&i| temp(i))),
        tuple(stepped.iter().map(|&i| format!("Ok({})", fit(i)))),
        assigned.join(" "),
        tuple(stepped.iter().map(|&i| result(i))),
    )
}
