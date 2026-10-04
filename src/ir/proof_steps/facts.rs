//! Facts about the builtins: equations stated directly over builtin calls
//! with untyped variables, each proved by steps from the wall rules.
//!
//! A law cites a fact by its key in `using` (`List.concat.assoc`), like a
//! law of its own program. A citation carries the fact's whole step proof
//! ([`super::LawRef::fact`]), and both checkers check that proof before
//! they let a step use the fact, so a fact is never an assumption. Lean
//! states each fact once, for every element type (`{α : Type}`), from the
//! same terms, and proves it with the same steps.
//!
//! The `List.` prefix of a `using` name is reserved for these facts: a
//! user module named `List` is already refused.

use crate::ast::{BinOp, Type};

use super::term::{self, Term, binop, builtin, var};
use super::{LawRef, Obligation, Proof, Script, WallRule};

/// One fact: its key in `using`, its Lean theorem name, and its script.
#[derive(Debug, Clone, PartialEq)]
pub struct Fact {
    pub key: &'static str,
    pub lean: &'static str,
    pub script: Script,
}

impl Fact {
    /// The fact as a law a script cites, with its proof.
    pub fn law_ref(&self) -> LawRef {
        let ob = &self.script.obligation;
        LawRef {
            key: self.key.to_string(),
            givens: ob.givens.clone(),
            premise: None,
            lhs: ob.lhs.clone(),
            rhs: ob.rhs.clone(),
            fact: Some(Box::new(self.script.clone())),
        }
    }
}

/// Whether `name` is in the namespace reserved for facts.
pub fn is_fact_name(name: &str) -> bool {
    name.starts_with("List.")
}

pub fn named(key: &str) -> Option<Fact> {
    all().into_iter().find(|f| f.key == key)
}

pub fn all() -> Vec<Fact> {
    vec![len_of_concat(), concat_assoc()]
}

fn len(l: Term) -> Term {
    builtin("List.len", vec![l], Some(Type::Int))
}

fn concat(a: Term, b: Term) -> Term {
    builtin("List.concat", vec![a, b], None)
}

fn cell(x: Term, t: Term) -> Term {
    builtin("List.prepend", vec![x, t], None)
}

fn add(a: Term, b: Term) -> Term {
    binop(BinOp::Add, a, b)
}

fn one() -> Term {
    term::int(&1.into())
}

fn rule(rule: WallRule, subst: Vec<(&str, Term)>) -> Proof {
    Proof::Rule {
        rule,
        subst: subst.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
        premises: Vec::new(),
    }
}

fn symm(p: Proof) -> Proof {
    Proof::Symm(Box::new(p))
}

fn congr(ctx: Term, inner: Proof) -> Proof {
    Proof::Congr {
        ctx,
        inner: Box::new(inner),
    }
}

fn trans(terms: Vec<Term>, steps: Vec<Proof>) -> Proof {
    Proof::Trans { terms, steps }
}

/// A fact over list givens, proved by induction on `on`; the cell case
/// names its parts `x` and `t` and its hypothesis `ih`.
fn by_list_induction(
    key: &'static str,
    lean: &'static str,
    givens: &[&str],
    lhs: Term,
    rhs: Term,
    nil: Proof,
    cons: Proof,
) -> Fact {
    let givens: Vec<String> = givens.iter().map(|g| g.to_string()).collect();
    Fact {
        key,
        lean,
        script: Script {
            obligation: Obligation {
                key: key.to_string(),
                givens: givens.clone(),
                finite: Vec::new(),
                lists: givens,
                premise: None,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            },
            defs: Vec::new(),
            consts: Vec::new(),
            laws: Vec::new(),
            proof: Proof::InductList {
                var: "a".into(),
                lhs,
                rhs,
                nil: Box::new(nil),
                head: "x".into(),
                tail: "t".into(),
                ih: "ih".into(),
                cons: Box::new(cons),
            },
        },
    }
}

/// `List.len(List.concat(a, b)) = List.len(a) + List.len(b)`.
fn len_of_concat() -> Fact {
    let (a, b, x, t) = (|| var("a"), || var("b"), || var("x"), || var("t"));
    let nil = term::nil;
    let base = trans(
        vec![
            len(concat(nil(), b())),
            len(b()),
            add(term::int(&0.into()), len(b())),
            add(len(nil()), len(b())),
        ],
        vec![
            congr(
                len(term::hole()),
                rule(WallRule::ConcatNil, vec![("b", b())]),
            ),
            symm(rule(WallRule::ZeroAdd, vec![("a", len(b()))])),
            congr(
                add(term::hole(), len(b())),
                symm(rule(WallRule::LenNil, vec![])),
            ),
        ],
    );
    let step = trans(
        vec![
            len(concat(cell(x(), t()), b())),
            len(cell(x(), concat(t(), b()))),
            add(len(concat(t(), b())), one()),
            add(add(len(t()), len(b())), one()),
            add(add(len(t()), one()), len(b())),
            add(len(cell(x(), t())), len(b())),
        ],
        vec![
            congr(
                len(term::hole()),
                rule(
                    WallRule::ConcatCons,
                    vec![("x", x()), ("a", t()), ("b", b())],
                ),
            ),
            rule(WallRule::LenCons, vec![("x", x()), ("a", concat(t(), b()))]),
            congr(add(term::hole(), one()), Proof::Hyp("ih".into())),
            Proof::Ring {
                lhs: add(add(len(t()), len(b())), one()),
                rhs: add(add(len(t()), one()), len(b())),
            },
            congr(
                add(term::hole(), len(b())),
                symm(rule(WallRule::LenCons, vec![("x", x()), ("a", t())])),
            ),
        ],
    );
    by_list_induction(
        "List.len.ofConcat",
        "list_len_ofConcat",
        &["a", "b"],
        len(concat(a(), b())),
        add(len(a()), len(b())),
        base,
        step,
    )
}

/// `List.concat(List.concat(a, b), c) = List.concat(a, List.concat(b, c))`.
fn concat_assoc() -> Fact {
    let (a, b, c, x, t) = (
        || var("a"),
        || var("b"),
        || var("c"),
        || var("x"),
        || var("t"),
    );
    let nil = term::nil;
    let base = trans(
        vec![
            concat(concat(nil(), b()), c()),
            concat(b(), c()),
            concat(nil(), concat(b(), c())),
        ],
        vec![
            congr(
                concat(term::hole(), c()),
                rule(WallRule::ConcatNil, vec![("b", b())]),
            ),
            symm(rule(WallRule::ConcatNil, vec![("b", concat(b(), c()))])),
        ],
    );
    let step = trans(
        vec![
            concat(concat(cell(x(), t()), b()), c()),
            concat(cell(x(), concat(t(), b())), c()),
            cell(x(), concat(concat(t(), b()), c())),
            cell(x(), concat(t(), concat(b(), c()))),
            concat(cell(x(), t()), concat(b(), c())),
        ],
        vec![
            congr(
                concat(term::hole(), c()),
                rule(
                    WallRule::ConcatCons,
                    vec![("x", x()), ("a", t()), ("b", b())],
                ),
            ),
            rule(
                WallRule::ConcatCons,
                vec![("x", x()), ("a", concat(t(), b())), ("b", c())],
            ),
            congr(cell(x(), term::hole()), Proof::Hyp("ih".into())),
            symm(rule(
                WallRule::ConcatCons,
                vec![("x", x()), ("a", t()), ("b", concat(b(), c()))],
            )),
        ],
    );
    by_list_induction(
        "List.concat.assoc",
        "list_concat_assoc",
        &["a", "b", "c"],
        concat(concat(a(), b()), c()),
        concat(a(), concat(b(), c())),
        base,
        step,
    )
}
