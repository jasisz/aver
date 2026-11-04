//! A computed `Int` match subject tested against literals, evaluated once.
//!
//! `match Int.mod(x, 32)` / `2 -> … 3 -> … _ -> …` tests one value against
//! each literal in turn. In a function a `--certify` build keeps for its
//! certificate, this pass binds such a subject by a `let` of a slot of its
//! own (`MirFnRepr::leaf_slots`, past the resolver's slots) and matches on
//! that slot, so the emitter and the certificate plan printer both read the
//! one evaluation the MIR states: the emitter stores the value once and reads
//! the slot per literal, the plan says the same with its `let_` and local.
//!
//! Elsewhere the wasm-gc emitter holds the subject itself, as the machine
//! word where the subject is an `Int` tree (`pattern_match.rs`), which the
//! certificate does not state; so the pass runs only on the kept fns.

use crate::ast::{Literal, Spanned};

use super::expr::{MirExpr, MirLet, MirLocal, MirPattern, walk_children_mut};
use super::int_tree::is_int;
use super::program::{LocalId, MirFn};

/// The name of the `let` a match subject is bound by.
pub const SUBJECT_BINDING: &str = "__subject";

/// Bind each computed `Int` subject of a match with a literal arm in `f`.
pub fn bind(f: &mut MirFn) {
    let mut next = f.local_count;
    let mut minted = Vec::new();
    walk(&mut f.body, &mut next, &mut minted);
    f.local_count = next;
    f.repr.leaf_slots.extend(minted);
}

/// A subject that performs nothing and reads one place: read again per test.
fn is_read(e: &MirExpr) -> bool {
    matches!(
        e,
        MirExpr::Local(_) | MirExpr::Project(_) | MirExpr::Literal(_)
    )
}

fn walk(e: &mut Spanned<MirExpr>, next: &mut u32, minted: &mut Vec<LocalId>) {
    walk_children_mut(&mut e.node, &mut |c| walk(c, next, minted));
    let MirExpr::Match(m) = &mut e.node else {
        return;
    };
    let m = &mut m.node;
    let literal_arm = m
        .arms
        .iter()
        .any(|a| matches!(a.pattern, MirPattern::Literal(Literal::Int(_))));
    if !literal_arm || !is_int(&m.subject) || is_read(&m.subject.node) {
        return;
    }
    let slot = LocalId(*next);
    *next += 1;
    minted.push(slot);
    let line = m.subject.line;
    let local = Spanned::new(
        MirExpr::Local(Spanned::new(
            MirLocal {
                slot,
                last_use: false,
                name: SUBJECT_BINDING.to_string(),
            },
            line,
        )),
        line,
    );
    local.set_ty(crate::ast::Type::Int);
    let value = std::mem::replace(&mut *m.subject, local);
    let ty = e.ty().cloned();
    let matched = std::mem::replace(
        e,
        Spanned::new(MirExpr::Literal(Spanned::new(Literal::Unit, line)), line),
    );
    *e = Spanned::new(
        MirExpr::Let(Spanned::new(
            MirLet {
                binding: slot,
                binding_name: SUBJECT_BINDING.to_string(),
                value: Box::new(value),
                body: Box::new(matched),
            },
            e.line,
        )),
        e.line,
    );
    if let Some(ty) = ty {
        e.set_ty(ty);
    }
}
