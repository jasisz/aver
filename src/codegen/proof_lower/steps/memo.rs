//! Remembered evaluations: the result of evaluating a term to its head
//! ([`Env::whnf`]) under the same hypotheses, kept so that evaluating it
//! again replays the result instead of taking every step a second time.
//!
//! A replay is the evaluation itself, not an approximation of it:
//!
//! - **What evaluation reads** is the term (with the types and source lines
//!   of all its parts), the hypotheses in scope (names and equations),
//!   whether a definition that divides an Int down to zero may be opened,
//!   whether the `when` of a cited law is being proved, whether a premise
//!   is being proved by evaluation, and whether linear arithmetic reads the
//!   wider facts ([`super::env::Env::wide_linear`]): that is the
//!   key, written out in full ([`key`]). The rest it reads is fixed for
//!   one obligation (the program, the cited laws, the builtin facts, the
//!   givens of finite type), and a memo lives for one obligation only.
//! - **What evaluation writes** besides its result is replayed in the
//!   order it was written: the definitions and module bindings it opened
//!   (so the script carries the same ones in the same order), the cited
//!   laws it used, the notes for a refusal, the `when` lines of cited laws
//!   that stayed open (which a split may take up), the definitions it looked up
//!   (which says whether one that divides down to zero was met), and the
//!   flag for a cited `when`.
//! - **Fuel**: a replay charges the fuel the evaluation took, and replays
//!   only when that much is left; with less, the evaluation runs again and
//!   runs out where it ran out before. So which laws close, and with which
//!   proofs, is the same with or without the memo; only time is saved.
//! - An evaluation that names a fresh hypothesis (a cut of linear
//!   arithmetic) or fails is not kept: the name depends on what was named
//!   before.
//!
//! `AVER_STEPS_MEMO=off` turns the memo off; `AVER_STEPS_MEMO=check`
//! evaluates again on every replay and stops if the two differ.

use std::collections::HashMap;

use crate::ir::identity::FnId;
use crate::ir::proof_steps::LawRef;
use crate::ir::proof_steps::term::Term;

use super::eval::Eval;

/// How the memo is used, from `AVER_STEPS_MEMO`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Mode {
    Off,
    On,
    /// Replay nothing: evaluate every time and compare with what is kept.
    Check,
}

pub(crate) fn mode() -> Mode {
    match std::env::var("AVER_STEPS_MEMO").as_deref() {
        Ok("off") => Mode::Off,
        Ok("check") => Mode::Check,
        _ => Mode::On,
    }
}

/// One write an evaluation makes besides its result, in order.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Effect {
    /// A definition opened ([`super::env::Env::mark_used`]).
    Used(FnId),
    /// A module-level binding opened.
    Const(String),
    /// A cited law used ([`super::env::Env::use_law`]).
    Law(Box<LawRef>),
    /// A note for the refusal.
    Note(String),
    /// A definition looked up ([`super::env::Env::def`]).
    Def(FnId),
    /// A cited law's `when` that stayed open where the law matched
    /// ([`super::env::Env::note_open_when`]).
    OpenWhen(Box<Term>),
}

/// What one evaluation wrote, recorded while it runs.
#[derive(Default)]
pub(crate) struct Trace {
    pub effects: Vec<Effect>,
}

impl Trace {
    pub(crate) fn push(&mut self, e: Effect) {
        // A repeated write is a no-op where the first one already landed,
        // in the evaluation and in its replay alike.
        if !self.effects.contains(&e) {
            self.effects.push(e);
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Entry {
    pub eval: Eval,
    /// The fuel the evaluation took.
    pub cost: usize,
    pub effects: Vec<Effect>,
    /// Whether a cited `when` was being proved once it finished.
    pub proving_cited_when: bool,
}

impl Entry {
    /// What a failed check prints.
    pub(crate) fn describe(&self) -> String {
        format!(
            "cost {}, cited `when` {}, effects {:?}, result {:?}",
            self.cost, self.proving_cited_when, self.effects, self.eval
        )
    }

    /// Whether `other` is the same evaluation, part for part.
    pub(crate) fn same(&self, other: &Entry) -> bool {
        self.cost == other.cost
            && self.effects == other.effects
            && self.proving_cited_when == other.proving_cited_when
            && format!("{:?}", self.eval) == format!("{:?}", other.eval)
    }
}

#[derive(Default)]
pub(crate) struct Memo {
    entries: HashMap<String, Entry>,
    /// The tables of equal terms ([`super::egraph`]) that closed nothing,
    /// by what they read: built again from the same, one closes nothing
    /// again.
    open_tables: std::collections::HashSet<u64>,
}

impl Memo {
    pub(crate) fn get(&self, key: &str) -> Option<&Entry> {
        self.entries.get(key)
    }

    pub(crate) fn insert(&mut self, key: String, entry: Entry) {
        self.entries.insert(key, entry);
    }

    pub(crate) fn table_left_open(&self, key: u64) -> bool {
        self.open_tables.contains(&key)
    }

    pub(crate) fn note_open_table(&mut self, key: u64) {
        self.open_tables.insert(key);
    }
}

/// A hash of the `Debug` text of `parts`, written one after the other, as
/// hashing that text as one `String` gives (`write` of its bytes, then the
/// `0xff` a `str` ends with), without building the text: the "seen before"
/// sets of an attempt compare hypotheses this way.
pub(crate) fn debug_hash(parts: &[&dyn std::fmt::Debug]) -> u64 {
    use std::fmt::Write as _;
    use std::hash::Hasher;
    struct Feed(std::collections::hash_map::DefaultHasher);
    impl std::fmt::Write for Feed {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            self.0.write(s.as_bytes());
            Ok(())
        }
    }
    let mut feed = Feed(std::collections::hash_map::DefaultHasher::new());
    for part in parts {
        let _ = write!(feed, "{part:?}");
    }
    feed.0.write_u8(0xff);
    feed.0.finish()
}

/// Everything an evaluation of `t` reads that can change within one
/// obligation. `Debug` writes each part with its type and line, which
/// structural equality of terms ignores.
pub(crate) fn key(
    t: &Term,
    hyps: &[(String, crate::ir::proof_steps::Eqn)],
    open_halving: bool,
    proving_cited_when: bool,
    discharging: bool,
    wide_linear: bool,
) -> String {
    format!(
        "{t:?}\u{1}{hyps:?}\u{1}{open_halving}\u{1}{proving_cited_when}\u{1}{discharging}\u{1}{wide_linear}"
    )
}
