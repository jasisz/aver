//! Evaluation with a proof: reduce a term towards a value, one rule at a
//! time, recording each rule. Where evaluation stops at a Bool it cannot
//! decide, the prover splits on that Bool and evaluates both sides again
//! under the new hypothesis.

use crate::ast::BinOp;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedPattern};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof, WallRule};

use super::chain::{Chain, meet};
use super::env::Env;

pub(crate) struct Eval {
    pub chain: Chain,
    /// The undecided Bool evaluation stopped at, if a split would help.
    pub blocked: Option<Term>,
}

enum Step {
    Progress(Box<(Proof, Term)>),
    Done,
    Blocked(Term),
}

/// How deep evaluations may nest before the producer refuses: each level
/// costs several large frames of the compiler's own stack, and a test
/// thread has 2 MiB.
const MAX_NESTING: usize = 24;

/// Whether `part` occurs in `t`.
fn holds(t: &Term, part: &Term) -> bool {
    canon(t) == *part || term::children(t).into_iter().any(|c| holds(c, part))
}

/// Complement rules: `(a P b) = v` decides `(a Q b) = w`.
const COMPLEMENTS: [(BinOp, bool, BinOp, bool, WallRule); 12] = [
    (BinOp::Gt, false, BinOp::Lte, true, WallRule::LeOfNotGt),
    (BinOp::Gt, true, BinOp::Lte, false, WallRule::LeFalseOfGt),
    (BinOp::Lt, false, BinOp::Gte, true, WallRule::GeOfNotLt),
    (BinOp::Lt, true, BinOp::Gte, false, WallRule::GeFalseOfLt),
    (BinOp::Gte, false, BinOp::Lt, true, WallRule::LtOfNotGe),
    (BinOp::Gte, true, BinOp::Lt, false, WallRule::LtFalseOfGe),
    (BinOp::Lte, false, BinOp::Gt, true, WallRule::GtOfNotLe),
    (BinOp::Lte, true, BinOp::Gt, false, WallRule::GtFalseOfLe),
    (BinOp::Neq, false, BinOp::Eq, true, WallRule::EqOfNotNe),
    (BinOp::Neq, true, BinOp::Eq, false, WallRule::EqFalseOfNe),
    (BinOp::Eq, false, BinOp::Neq, true, WallRule::NeOfNotEq),
    (BinOp::Eq, true, BinOp::Neq, false, WallRule::NeFalseOfEq),
];

fn is_int(t: &Term) -> bool {
    matches!(t.ty(), Some(crate::ast::Type::Int)) || term::int_value(t).is_some()
}

/// The arm of a match whose pattern head the value `v` has, with the
/// pattern's bindings, and whether `v` is a list literal the arm reads as a
/// cell (so the premise ends with a [`Proof::Cell`] step). `None` when no
/// arm can be selected syntactically.
fn select_arm(
    arms: &[crate::ir::hir::ResolvedMatchArm],
    v: &Term,
) -> Option<(u32, Vec<Term>, bool)> {
    use crate::ir::proof_steps::claim::{excludes, is_catch_all};
    for (i, arm) in arms.iter().enumerate() {
        let k = (i + 1) as u32;
        if is_catch_all(&arm.pattern) {
            // Chosen for this value once every earlier arm excludes it.
            return arms[..i]
                .iter()
                .all(|earlier| excludes(&earlier.pattern, v))
                .then(|| (k, vec![canon(v)], false));
        }
        match (&arm.pattern, &v.node) {
            (ResolvedPattern::Literal(l), ResolvedExpr::Literal(x)) => {
                if l == x {
                    return Some((k, Vec::new(), false));
                }
            }
            (ResolvedPattern::Ctor(c, names), ResolvedExpr::Ctor(c2, args)) => {
                if c == c2 && names.len() == args.len() {
                    return Some((k, args.clone(), false));
                }
            }
            (ResolvedPattern::EmptyList, ResolvedExpr::List(xs)) if xs.is_empty() => {
                return Some((k, Vec::new(), false));
            }
            (ResolvedPattern::Cons(..), ResolvedExpr::Call(ResolvedCallee::Builtin(b), args))
                if b == "List.prepend" && args.len() == 2 =>
            {
                return Some((k, args.clone(), false));
            }
            // A list literal with an element is a cell.
            (ResolvedPattern::Cons(..), ResolvedExpr::List(xs)) if !xs.is_empty() => {
                return Some((
                    k,
                    term::children(&term::cell_of(v)?)
                        .into_iter()
                        .cloned()
                        .collect(),
                    true,
                ));
            }
            // A literal subject meets a different literal / constructor:
            // keep looking. Anything else cannot be decided here.
            (ResolvedPattern::EmptyList, ResolvedExpr::List(xs)) if !xs.is_empty() => continue,
            (ResolvedPattern::EmptyList, ResolvedExpr::Call(..))
            | (ResolvedPattern::Cons(..), ResolvedExpr::List(_)) => continue,
            _ => return None,
        }
    }
    None
}

/// `chain` with a last [`Proof::Cell`] step when its value is a list
/// literal an arm reads as a cell.
fn with_cell(mut chain: Chain, cell: bool) -> Chain {
    if cell {
        let list = chain.cur().clone();
        let to = term::cell_of(&list).expect("select_arm saw a list literal with an element");
        chain.push(Proof::Cell { list }, to);
    }
    chain
}

fn is_bool_value(t: &Term) -> bool {
    term::bool_value(t).is_some()
}

impl Env<'_> {
    pub(crate) fn whnf(&mut self, t: &Term) -> Result<Eval, String> {
        // Each nested evaluation is a frame on the compiler's own stack; a
        // recursive definition over long data would exhaust it.
        if self.nesting >= MAX_NESTING {
            return Err(format!(
                "evaluation nests more than {MAX_NESTING} deep at `{}`",
                crate::ir::proof_steps::show::term(t, self.inputs.symbol_table)
            ));
        }
        self.nesting += 1;
        let out = self.whnf_nested(t);
        self.nesting -= 1;
        out
    }

    fn whnf_nested(&mut self, t: &Term) -> Result<Eval, String> {
        let mut chain = Chain::new(t);
        loop {
            self.burn()?;
            let cur = chain.cur().clone();
            match self.head_step(&cur)? {
                Step::Progress(step) => {
                    let (proof, to) = *step;
                    chain.push(proof, to)
                }
                Step::Done => {
                    return Ok(Eval {
                        chain,
                        blocked: None,
                    });
                }
                Step::Blocked(g) => {
                    return Ok(Eval {
                        chain,
                        blocked: Some(g),
                    });
                }
            }
        }
    }

    /// Evaluate child `i`; on progress, the congruence step for the whole term.
    fn child(&mut self, cur: &Term, i: usize) -> Result<Result<Step, Option<Term>>, String> {
        let child = term::children(cur)[i].clone();
        let ev = self.whnf(&child)?;
        if ev.chain.is_empty() {
            return Ok(Err(ev.blocked));
        }
        let (to, proof) = ev.chain.finish();
        let ctx = term::context_at(cur, &[i]);
        let new = term::plug(&ctx, &to);
        Ok(Ok(Step::Progress(Box::new((
            Proof::Congr {
                ctx,
                inner: Box::new(proof),
            },
            new,
        )))))
    }

    fn settle(&mut self, cur: &Term, blocked: Option<Term>) -> Result<Step, String> {
        if let Some((name, value)) = self.hyp_for(cur) {
            return Ok(Step::Progress(Box::new((Proof::Hyp(name), value))));
        }
        if let Some(proof) = self.conjunct_of_hyp(cur) {
            return Ok(Step::Progress(Box::new((proof, term::boolean(true)))));
        }
        if let Some(step) = self.decide_linearly(cur) {
            return Ok(Step::Progress(Box::new(step)));
        }
        if let Some((proof, to)) = self.rewrite_with_cited(cur)? {
            return Ok(Step::Progress(Box::new((proof, to))));
        }
        if let ResolvedExpr::BinOp(op, a, b) = &cur.node
            && is_int(a)
        {
            for (p, v, q, w, rule) in COMPLEMENTS {
                if q != *op {
                    continue;
                }
                let premise = term::binop(p, (**a).clone(), (**b).clone());
                if let Some((name, value)) = self.hyp_for(&premise)
                    && term::bool_value(&value) == Some(v)
                {
                    return Ok(Step::Progress(Box::new((
                        Proof::Rule {
                            rule,
                            subst: vec![("a".into(), (**a).clone()), ("b".into(), (**b).clone())],
                            premises: vec![Proof::Hyp(name)],
                        },
                        term::boolean(w),
                    ))));
                }
            }
        }
        Ok(match blocked {
            Some(g) => Step::Blocked(g),
            None => Step::Done,
        })
    }

    fn head_step(&mut self, cur: &Term) -> Result<Step, String> {
        match &cur.node {
            ResolvedExpr::Attr(obj, _) => {
                if matches!(obj.node, ResolvedExpr::RecordCreate { .. }) {
                    let ev = crate::ir::proof_steps::claim::claim(
                        &Proof::Proj { term: cur.clone() },
                        &empty_script(),
                        &Vec::new(),
                    )?;
                    return Ok(Step::Progress(Box::new((
                        Proof::Proj { term: cur.clone() },
                        ev.rhs,
                    ))));
                }
                match self.child(cur, 0)? {
                    Ok(step) => Ok(step),
                    Err(b) => self.settle(cur, b),
                }
            }
            ResolvedExpr::Call(ResolvedCallee::Fn(id), args) => {
                let Some(def) = self.def(*id) else {
                    return self.args_then_settle(cur);
                };
                let outer = def.outer(args)?;
                let ResolvedExpr::Match { subject, arms } = &def.body.node else {
                    self.mark_used(*id);
                    let body = term::subst(&def.body, &outer)?;
                    return Ok(Step::Progress(Box::new((
                        Proof::Unfold {
                            fn_id: *id,
                            arm: 0,
                            args: args.clone(),
                            binders: Vec::new(),
                            premise: None,
                        },
                        body,
                    ))));
                };
                let s = term::subst(subject, &outer)?;
                let ev = self.whnf(&s)?;
                let value = ev.chain.cur().clone();
                match select_arm(arms, &value) {
                    Some((k, binders, cell)) => {
                        let (_, premise) = with_cell(ev.chain, cell).finish();
                        let unfold = Proof::Unfold {
                            fn_id: *id,
                            arm: k,
                            args: args.clone(),
                            binders,
                            premise: Some(Box::new(premise)),
                        };
                        self.mark_used(*id);
                        let script = self.scratch_script();
                        let eq =
                            crate::ir::proof_steps::claim::claim(&unfold, &script, &self.hyps)?;
                        Ok(Step::Progress(Box::new((unfold, eq.rhs))))
                    }
                    None => {
                        let g = ev.blocked.or_else(|| {
                            (matches!(value.ty(), Some(crate::ast::Type::Bool))
                                || matches!(value.node, ResolvedExpr::BinOp(..)))
                            .then_some(value.clone())
                        });
                        self.settle(cur, g)
                    }
                }
            }
            ResolvedExpr::Call(ResolvedCallee::Builtin(name), args)
                if matches!(name.as_str(), "Bool.and" | "Bool.or") && args.len() == 2 =>
            {
                let and = name == "Bool.and";
                let left = match self.child(cur, 0)? {
                    Ok(step) => return Ok(step),
                    Err(b) => b,
                };
                if let Some(v) = term::bool_value(&args[0]) {
                    let rule = match (and, v) {
                        (true, true) => WallRule::AndTrueL,
                        (true, false) => WallRule::AndFalseL,
                        (false, true) => WallRule::OrTrueL,
                        (false, false) => WallRule::OrFalseL,
                    };
                    return Ok(self.rule_step(rule, vec![("b".into(), args[1].clone())]));
                }
                let right = match self.child(cur, 1)? {
                    Ok(step) => return Ok(step),
                    Err(b) => b,
                };
                if let Some(v) = term::bool_value(&args[1]) {
                    let rule = match (and, v) {
                        (true, true) => WallRule::AndTrueR,
                        (true, false) => WallRule::AndFalseR,
                        (false, true) => WallRule::OrTrueR,
                        (false, false) => WallRule::OrFalseR,
                    };
                    return Ok(self.rule_step(rule, vec![("a".into(), args[0].clone())]));
                }
                self.settle(cur, left.or(right))
            }
            ResolvedExpr::Call(ResolvedCallee::Builtin(name), args)
                if name == "Bool.not" && args.len() == 1 =>
            {
                let blocked = match self.child(cur, 0)? {
                    Ok(step) => return Ok(step),
                    Err(b) => b,
                };
                match term::bool_value(&args[0]) {
                    Some(true) => Ok(self.rule_step(WallRule::NotTrue, vec![])),
                    Some(false) => Ok(self.rule_step(WallRule::NotFalse, vec![])),
                    None => self.settle(cur, blocked),
                }
            }
            ResolvedExpr::Call(..) | ResolvedExpr::BinOp(..) | ResolvedExpr::Neg(_) => {
                self.args_then_settle(cur)
            }
            ResolvedExpr::Match { subject, arms } => {
                let ev = self.whnf(subject)?;
                let value = ev.chain.cur().clone();
                match select_arm(arms, &value) {
                    Some((k, binders, cell)) => {
                        let (_, premise) = with_cell(ev.chain, cell).finish();
                        let arm = Proof::Arm {
                            term: cur.clone(),
                            arm: k,
                            binders,
                            premise: Box::new(premise),
                        };
                        let eq = crate::ir::proof_steps::claim::claim(
                            &arm,
                            &empty_script(),
                            &self.hyps,
                        )?;
                        Ok(Step::Progress(Box::new((arm, eq.rhs))))
                    }
                    None => {
                        let g = ev.blocked.or_else(|| Some(value.clone()));
                        self.settle(cur, g)
                    }
                }
            }
            // A list literal with an open element evaluates to its first
            // element in front of the rest; a closed one keeps the shape it
            // was written in.
            ResolvedExpr::List(xs) if !xs.is_empty() && term::eval_closed(cur).is_none() => {
                let to = term::cell_of(cur).expect("a list literal with an element");
                Ok(Step::Progress(Box::new((
                    Proof::Cell { list: cur.clone() },
                    to,
                ))))
            }
            ResolvedExpr::Ident(name) => match self.constant(name) {
                // A module-level binding reads as its value.
                Some(c) => {
                    self.mark_const_used(name);
                    Ok(Step::Progress(Box::new((
                        Proof::UnfoldConst { name: name.clone() },
                        canon(&c.value),
                    ))))
                }
                None => self.settle(cur, None),
            },
            _ => self.settle(cur, None),
        }
    }

    /// Evaluate every argument; a closed result computes, an open one
    /// settles against the hypotheses.
    fn args_then_settle(&mut self, cur: &Term) -> Result<Step, String> {
        let mut blocked = None;
        for i in 0..term::children(cur).len() {
            match self.child(cur, i)? {
                Ok(step) => return Ok(step),
                Err(b) => blocked = blocked.or(b),
            }
        }
        // A closed scalar computes; a list keeps the shape the program
        // built it in, which is the shape a match on it reads.
        if !term::is_literal(cur)
            && let Some(v) = term::eval_closed(cur)
            && !matches!(v.node, ResolvedExpr::List(_))
        {
            return Ok(Step::Progress(Box::new((
                Proof::Compute {
                    lhs: cur.clone(),
                    rhs: v.clone(),
                },
                v,
            ))));
        }
        if let Some(step) = self.list_step(cur)? {
            return Ok(step);
        }
        if let Some(step) = self.map_step(cur)? {
            return Ok(step);
        }
        if let Some(step) = self.vector_step(cur)? {
            return Ok(step);
        }
        self.settle(cur, blocked)
    }

    /// A Vector builtin takes one step by its rule where the step can
    /// decide the rule's premise: a vector of a list read back as the list
    /// (and the other way), the length after a write or of a literal-size
    /// `Vector.new`, and a read below zero, past the end, after a write
    /// (at the same index or, decided, another) or of a `Vector.new`.
    fn vector_step(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        use crate::ir::hir::BuiltinIntrinsic;
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return Ok(None);
        };
        let builtin = |t: &Term, want: &str, n: usize| -> Option<Vec<Term>> {
            match &t.node {
                ResolvedExpr::Call(ResolvedCallee::Builtin(b), xs)
                    if b == want && xs.len() == n =>
                {
                    Some(xs.clone())
                }
                _ => None,
            }
        };
        let made = |t: &Term| -> Option<Vec<Term>> {
            match &t.node {
                ResolvedExpr::Call(ResolvedCallee::Intrinsic(BuiltinIntrinsic::VectorNew), xs)
                    if xs.len() == 2 =>
                {
                    Some(xs.clone())
                }
                _ => None,
            }
        };
        // `Option.withDefault(Vector.set(v, i, x), v)`: the vector a write
        // leaves, or the vector itself when the index is out of range.
        let written = |t: &Term| -> Option<Vec<Term>> {
            let outer = builtin(t, "Option.withDefault", 2)?;
            let set = builtin(&outer[0], "Vector.set", 3)?;
            (canon(&set[0]) == canon(&outer[1])).then_some(set)
        };
        let decided = |env: &mut Self, t: Term| -> Result<Option<(bool, Proof)>, String> {
            let ev = env.whnf(&t)?;
            let Some(v) = term::bool_value(ev.chain.cur()) else {
                return Ok(None);
            };
            Ok(Some((v, ev.chain.finish().1)))
        };
        let int = |n: i64| term::int(&n.into());
        let vlen =
            |v: &Term| term::builtin("Vector.len", vec![v.clone()], Some(crate::ast::Type::Int));
        let in_range = |at: &Term, end: Term| {
            term::bool_and(
                term::binop(BinOp::Lte, int(0), at.clone()),
                term::binop(BinOp::Lt, at.clone(), end),
            )
        };
        let bind = |names: &[&str], values: Vec<Term>| -> Vec<(String, Term)> {
            names.iter().map(|n| n.to_string()).zip(values).collect()
        };
        let (rule, subst, premises): (WallRule, Vec<(String, Term)>, Vec<Proof>) =
            match (name.as_str(), args.as_slice()) {
                ("List.fromVector", [w]) => match builtin(w, "Vector.fromList", 1) {
                    Some(l) => (WallRule::VecToListOfList, bind(&["l"], l), vec![]),
                    None => return Ok(None),
                },
                ("Vector.fromList", [l]) => match builtin(l, "List.fromVector", 1) {
                    Some(v) => (WallRule::VecOfListToList, bind(&["v"], v), vec![]),
                    None => return Ok(None),
                },
                ("Vector.len", [w]) => {
                    if let Some(set) = written(w) {
                        (WallRule::VecLenSet, bind(&["v", "i", "x"], set), vec![])
                    } else if let Some(nx) = made(w) {
                        let ok = term::binop(BinOp::Lte, int(0), nx[0].clone());
                        let Some((true, p)) = decided(self, ok)? else {
                            return Ok(None);
                        };
                        (WallRule::VecLenNew, bind(&["n", "x"], nx), vec![p])
                    } else if builtin(w, "Vector.fromList", 1).is_some() {
                        // The length of a vector made from a list is that
                        // list's, which the list rules read.
                        (
                            WallRule::VecLenToList,
                            bind(&["v"], vec![w.clone()]),
                            vec![],
                        )
                    } else {
                        return Ok(None);
                    }
                }
                ("Vector.get", [w, j]) => {
                    if let Some(set) = written(w) {
                        let Some((true, p)) = decided(self, in_range(&set[1], vlen(&set[0])))?
                        else {
                            return Ok(None);
                        };
                        if canon(&set[1]) == canon(j) {
                            (
                                WallRule::VecGetSetSame,
                                bind(&["v", "i", "x"], set),
                                vec![p],
                            )
                        } else {
                            let ne = term::binop(BinOp::Neq, set[1].clone(), j.clone());
                            let Some((true, q)) = decided(self, ne)? else {
                                return Ok(None);
                            };
                            let mut values = set;
                            values.push(j.clone());
                            (
                                WallRule::VecGetSetOther,
                                bind(&["v", "i", "x", "j"], values),
                                vec![p, q],
                            )
                        }
                    } else if let Some(nx) = made(w) {
                        let Some((true, p)) = decided(self, in_range(j, nx[0].clone()))? else {
                            return Ok(None);
                        };
                        let mut values = nx;
                        values.push(j.clone());
                        (WallRule::VecGetNew, bind(&["n", "x", "i"], values), vec![p])
                    } else if let Some((true, p)) =
                        decided(self, term::binop(BinOp::Lt, j.clone(), int(0)))?
                    {
                        (
                            WallRule::VecGetNegative,
                            bind(&["v", "i"], vec![w.clone(), j.clone()]),
                            vec![p],
                        )
                    } else if let Some((true, p)) =
                        decided(self, term::binop(BinOp::Gte, j.clone(), vlen(w)))?
                    {
                        (
                            WallRule::VecGetPastEnd,
                            bind(&["v", "i"], vec![w.clone(), j.clone()]),
                            vec![p],
                        )
                    } else {
                        return Ok(None);
                    }
                }
                ("Vector.set", [v, i, x]) => {
                    let out = term::bool_or(
                        term::binop(BinOp::Lt, i.clone(), int(0)),
                        term::binop(BinOp::Gte, i.clone(), vlen(v)),
                    );
                    let Some((true, p)) = decided(self, out)? else {
                        return Ok(None);
                    };
                    (
                        WallRule::VecSetOutOfRange,
                        bind(&["v", "i", "x"], vec![v.clone(), i.clone(), x.clone()]),
                        vec![p],
                    )
                }
                _ => return Ok(None),
            };
        let (_, concl) = rule.instantiate(&subst).expect("rule binders");
        Ok(Some(Step::Progress(Box::new((
            Proof::Rule {
                rule,
                subst,
                premises,
            },
            concl.rhs,
        )))))
    }

    /// `Map.get`, `Map.has` or `Map.len` of the empty map or of a
    /// `Map.set` takes one step by its rule: at the key just set (the same
    /// term), at a key the step decides is another (`k != k2`), and for the
    /// size by whether the map held the key, which is split on when open.
    fn map_step(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return Ok(None);
        };
        let Some(map) = args.first() else {
            return Ok(None);
        };
        let bind = |names: &[&str], values: Vec<Term>| -> Vec<(String, Term)> {
            names.iter().map(|n| n.to_string()).zip(values).collect()
        };
        let empty = matches!(&map.node, ResolvedExpr::MapLiteral(kvs) if kvs.is_empty());
        let set = match &map.node {
            ResolvedExpr::Call(ResolvedCallee::Builtin(b), parts)
                if b == "Map.set" && parts.len() == 3 =>
            {
                Some(parts.clone())
            }
            _ => None,
        };
        let decided = |env: &mut Self, t: Term| -> Result<Option<(bool, Proof)>, String> {
            let ev = env.whnf(&t)?;
            let Some(v) = term::bool_value(ev.chain.cur()) else {
                return Ok(None);
            };
            Ok(Some((v, ev.chain.finish().1)))
        };
        let (rule, subst, premises) = match (name.as_str(), args.len(), empty, set) {
            ("Map.get", 2, true, _) => (
                WallRule::MapGetEmpty,
                bind(&["k"], vec![args[1].clone()]),
                vec![],
            ),
            ("Map.has", 2, true, _) => (
                WallRule::MapHasEmpty,
                bind(&["k"], vec![args[1].clone()]),
                vec![],
            ),
            ("Map.len", 1, true, _) => (WallRule::MapLenEmpty, vec![], vec![]),
            ("Map.get" | "Map.has", 2, false, Some(p)) => {
                let get = name == "Map.get";
                if canon(&p[1]) == canon(&args[1]) {
                    let rule = if get {
                        WallRule::MapGetSetSame
                    } else {
                        WallRule::MapHasSetSame
                    };
                    (rule, bind(&["m", "k", "v"], p), vec![])
                } else {
                    let ne = term::binop(BinOp::Neq, p[1].clone(), args[1].clone());
                    let Some((true, premise)) = decided(self, ne)? else {
                        return Ok(None);
                    };
                    let rule = if get {
                        WallRule::MapGetSetOther
                    } else {
                        WallRule::MapHasSetOther
                    };
                    let mut values = p;
                    values.push(args[1].clone());
                    (rule, bind(&["m", "k", "v", "k2"], values), vec![premise])
                }
            }
            ("Map.len", 1, false, Some(p)) => {
                let held = term::builtin(
                    "Map.has",
                    vec![p[0].clone(), p[1].clone()],
                    Some(crate::ast::Type::Bool),
                );
                match decided(self, held.clone())? {
                    Some((true, premise)) => (
                        WallRule::MapLenSetPresent,
                        bind(&["m", "k", "v"], p),
                        vec![premise],
                    ),
                    Some((false, premise)) => (
                        WallRule::MapLenSetAbsent,
                        bind(&["m", "k", "v"], p),
                        vec![premise],
                    ),
                    None => return self.settle(cur, Some(canon(&held))).map(Some),
                }
            }
            _ => return Ok(None),
        };
        let (_, concl) = rule.instantiate(&subst).expect("rule binders");
        Ok(Some(Step::Progress(Box::new((
            Proof::Rule {
                rule,
                subst,
                premises,
            },
            concl.rhs,
        )))))
    }

    /// `t = true` from a hypothesis `Bool.and(…) = true` that holds `t` as
    /// one of its conjuncts, taken apart by `bool.and.elim_l` and `_r`.
    fn conjunct_of_hyp(&self, t: &Term) -> Option<Proof> {
        fn find(t: &Term, part: &Term, proof: Proof) -> Option<Proof> {
            if canon(part) == *t {
                return Some(proof);
            }
            let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &part.node else {
                return None;
            };
            if b != "Bool.and" || args.len() != 2 {
                return None;
            }
            let elim = |rule: WallRule| Proof::Rule {
                rule,
                subst: vec![("a".into(), args[0].clone()), ("b".into(), args[1].clone())],
                premises: vec![proof.clone()],
            };
            find(t, &args[0], elim(WallRule::AndElimL))
                .or_else(|| find(t, &args[1], elim(WallRule::AndElimR)))
        }
        let t = canon(t);
        self.hyps.iter().rev().find_map(|(name, e)| {
            (term::bool_value(&e.rhs) == Some(true))
                .then(|| find(&t, &e.lhs, Proof::Hyp(name.clone())))
                .flatten()
        })
    }

    /// A list builtin whose list argument is `[]` or `List.prepend(x, a)`
    /// takes one step by its constructor equation. A count is decided
    /// first (`n <= 0`, else `n > 0`); an undecided count is where the
    /// prover splits.
    fn list_step(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return Ok(None);
        };
        let Some(list) = args.first() else {
            return Ok(None);
        };
        if matches!(
            name.as_str(),
            "List.concat" | "List.len" | "List.reverse" | "List.take" | "List.drop"
        ) && let ResolvedExpr::List(xs) = &list.node
            && !xs.is_empty()
            && term::eval_closed(list).is_none()
        {
            // A literal with an open element steps as a cell; a closed one
            // keeps the shape it was written in.
            let cell = term::cell_of(list).expect("a list literal with an element");
            let ctx = term::context_at(cur, &[0]);
            let to = term::plug(&ctx, &cell);
            return Ok(Some(Step::Progress(Box::new((
                Proof::Congr {
                    ctx,
                    inner: Box::new(Proof::Cell { list: list.clone() }),
                },
                to,
            )))));
        }
        let shape = match &list.node {
            ResolvedExpr::List(xs) if xs.is_empty() => None,
            ResolvedExpr::Call(ResolvedCallee::Builtin(b), parts)
                if b == "List.prepend" && parts.len() == 2 =>
            {
                Some((parts[0].clone(), parts[1].clone()))
            }
            _ => return Ok(None),
        };
        let bind = |names: &[&str], values: Vec<Term>| -> Vec<(String, Term)> {
            names.iter().map(|n| n.to_string()).zip(values).collect()
        };
        let (rule, subst) = match (name.as_str(), args.len(), shape) {
            ("List.concat", 2, None) => (WallRule::ConcatNil, bind(&["b"], vec![args[1].clone()])),
            ("List.concat", 2, Some((x, a))) => (
                WallRule::ConcatCons,
                bind(&["x", "a", "b"], vec![x, a, args[1].clone()]),
            ),
            ("List.len", 1, None) => (WallRule::LenNil, Vec::new()),
            ("List.len", 1, Some((x, a))) => (WallRule::LenCons, bind(&["x", "a"], vec![x, a])),
            ("List.reverse", 1, None) => (WallRule::ReverseNil, Vec::new()),
            ("List.reverse", 1, Some((x, a))) => {
                (WallRule::ReverseCons, bind(&["x", "a"], vec![x, a]))
            }
            ("List.take", 2, None) => (WallRule::TakeNil, bind(&["n"], vec![args[1].clone()])),
            ("List.drop", 2, None) => (WallRule::DropNil, bind(&["n"], vec![args[1].clone()])),
            ("List.take" | "List.drop", 2, Some((x, a))) => {
                let take = name == "List.take";
                let n = args[1].clone();
                let zero = term::int(&0.into());
                let decided =
                    |env: &mut Self, op: BinOp| -> Result<Option<(bool, Proof)>, String> {
                        let ev = env.whnf(&term::binop(op, n.clone(), zero.clone()))?;
                        let Some(v) = term::bool_value(ev.chain.cur()) else {
                            return Ok(None);
                        };
                        Ok(Some((v, ev.chain.finish().1)))
                    };
                let (rule, premise) = match decided(self, BinOp::Lte)? {
                    Some((true, p)) => (
                        if take {
                            WallRule::TakeConsLe
                        } else {
                            WallRule::DropConsLe
                        },
                        p,
                    ),
                    Some((false, _)) => match decided(self, BinOp::Gt)? {
                        Some((true, p)) => (
                            if take {
                                WallRule::TakeConsGt
                            } else {
                                WallRule::DropConsGt
                            },
                            p,
                        ),
                        _ => return Ok(None),
                    },
                    None => {
                        let at_most = canon(&term::binop(BinOp::Lte, n.clone(), zero));
                        return self.settle(cur, Some(at_most)).map(Some);
                    }
                };
                let subst = bind(&["x", "a", "n"], vec![x, a, n]);
                let (_, concl) = rule.instantiate(&subst).expect("rule binders");
                return Ok(Some(Step::Progress(Box::new((
                    Proof::Rule {
                        rule,
                        subst,
                        premises: vec![premise],
                    },
                    concl.rhs,
                )))));
            }
            _ => return Ok(None),
        };
        Ok(Some(self.rule_step(rule, subst)))
    }

    fn rule_step(&self, rule: WallRule, subst: Vec<(String, Term)>) -> Step {
        let (_, concl) = rule.instantiate(&subst).expect("rule binders");
        Step::Progress(Box::new((
            Proof::Rule {
                rule,
                subst,
                premises: Vec::new(),
            },
            concl.rhs,
        )))
    }

    fn scratch_script(&mut self) -> crate::ir::proof_steps::Script {
        let mut s = empty_script();
        s.defs = self.used_defs();
        s.consts = self.used_consts();
        s
    }

    /// An Int comparison the decided comparisons in scope settle by
    /// linear arithmetic, with its value. An Int equality is settled
    /// through the two orders: both hold (`int.eq.of_le_ge`), or one
    /// strict order does (`int.eq.false_of_lt`, `_gt`).
    fn decide_linearly(&self, cur: &Term) -> Option<(Proof, Term)> {
        if let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &cur.node
            && is_int(a)
        {
            let subst = vec![
                ("a".to_string(), a.as_ref().clone()),
                ("b".to_string(), b.as_ref().clone()),
            ];
            let rule_on = |rule: WallRule| -> Option<(Proof, Term)> {
                let (premises, concl) = rule.instantiate(&subst)?;
                let premises = premises
                    .iter()
                    .map(|e| self.linear_proof(&e.lhs, term::bool_value(&e.rhs)?))
                    .collect::<Option<Vec<_>>>()?;
                Some((
                    Proof::Rule {
                        rule,
                        subst: subst.clone(),
                        premises,
                    },
                    concl.rhs,
                ))
            };
            return rule_on(WallRule::EqOfLeGe)
                .or_else(|| rule_on(WallRule::EqFalseOfLt))
                .or_else(|| rule_on(WallRule::EqFalseOfGt));
        }
        [true, false]
            .into_iter()
            .find_map(|value| Some((self.linear_proof(cur, value)?, term::boolean(value))))
    }

    /// `goal = value` for an Int comparison `goal`, when its opposite and
    /// the decided comparisons in scope add up to a contradiction.
    fn linear_proof(&self, goal: &Term, value: bool) -> Option<Proof> {
        use crate::ir::proof_steps::linear;
        let known: Vec<(String, crate::ir::proof_steps::Eqn)> = self
            .hyps
            .iter()
            .filter(|(_, e)| {
                term::bool_value(&e.rhs).is_some()
                    && linear::as_nonneg(&e.lhs, true, &mut Vec::new()).is_some()
            })
            .cloned()
            .collect();
        let mut atoms = Vec::new();
        let mut facts = vec![linear::as_nonneg(goal, !value, &mut atoms)?];
        for (_, e) in &known {
            facts.push(linear::as_nonneg(
                &e.lhs,
                term::bool_value(&e.rhs)?,
                &mut atoms,
            )?);
        }
        let weights = linear::certificate(&facts)?;
        // Only the hypotheses the certificate uses are named.
        let mut hyps = Vec::new();
        let mut kept = vec![weights[0].clone()];
        for ((n, _), w) in known.iter().zip(&weights[1..]) {
            if *w != num_bigint::BigInt::from(0) {
                hyps.push(n.clone());
                kept.push(w.clone());
            }
        }
        Some(Proof::Linear {
            goal: canon(goal),
            value,
            hyps,
            weights: kept,
        })
    }

    /// A law the author cited, applied left to right to a term evaluation
    /// stopped at. Two cited laws that rewrite it to different terms are a
    /// refusal: the result would depend on which is tried first.
    fn rewrite_with_cited(&mut self, cur: &Term) -> Result<Option<(Proof, Term)>, String> {
        use super::rewrite::Equation;
        let mut found: Vec<(String, Proof, Term)> = Vec::new();
        for law in self.rewrite_laws.clone() {
            let eq = Equation::Law(Box::new(law.clone()));
            if let Some((p, to)) = self.try_equation(&eq, cur) {
                // A result that holds the term it rewrote would be rewritten
                // again forever (`f(x, [])` to `g(f(x, []))`).
                if !holds(&to, &canon(cur)) {
                    found.push((law.key.clone(), p, to));
                }
            }
        }
        let Some((key, p, to)) = found.first().cloned() else {
            return Ok(None);
        };
        if let Some((other, _, _)) = found.iter().find(|(_, _, o)| canon(o) != canon(&to)) {
            return Err(format!(
                "law {key} and law {other} both rewrite `{}`, to different terms; cite only one of them",
                crate::ir::proof_steps::show::term(cur, self.inputs.symbol_table)
            ));
        }
        if let Some(law) = self.rewrite_laws.iter().find(|l| l.key == key).cloned()
            && !self.laws.iter().any(|l| l.key == key)
        {
            self.laws.push(law);
        }
        Ok(Some((p, to)))
    }

    /// Evaluate `t` and then, below its head, each part in turn, so a
    /// hypothesis or cited law applies inside a constructor too.
    pub(crate) fn normalize(&mut self, t: &Term, depth: usize) -> Result<Chain, String> {
        let mut chain = self.whnf(t)?.chain;
        if depth == 0 {
            return Ok(chain);
        }
        // Parts first; when one changes, the whole may evaluate further.
        loop {
            self.burn()?;
            let mut changed = false;
            let parts = term::children(chain.cur()).len();
            for i in 0..parts {
                let part = term::children(chain.cur())[i].clone();
                let sub = self.normalize(&part, depth - 1)?;
                if !sub.is_empty() {
                    let (to, proof) = sub.finish();
                    chain.push_at(&[i], proof, &to);
                    changed = true;
                }
            }
            if !changed {
                return Ok(chain);
            }
            let again = self.whnf(chain.cur())?.chain;
            if again.is_empty() {
                return Ok(chain);
            }
            let (to, proof) = again.finish();
            chain.push(proof, to);
        }
    }

    /// Prove `lhs = rhs` by evaluating both sides, splitting on the first
    /// undecided Bool either side stops at.
    pub(crate) fn prove_by_evaluation(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Proof, String> {
        let l = self.whnf(lhs)?;
        let r = self.whnf(rhs)?;
        if canon(l.chain.cur()) == canon(r.chain.cur()) {
            return Ok(meet(l.chain, r.chain));
        }
        let Some(g) = l.blocked.or(r.blocked) else {
            // Nothing to split on: look below the heads, at the parts of a
            // constructor and the arguments of a call evaluation stopped at.
            let mut nl = self.normalize(lhs, 8)?;
            let nr = self.normalize(rhs, 8)?;
            if canon(nl.cur()) == canon(nr.cur()) {
                return Ok(meet(nl, nr));
            }
            // Two Int terms that are one polynomial: the ring step.
            if is_int(nl.cur()) && crate::ir::proof_steps::ring::same_polynomial(nl.cur(), nr.cur())
            {
                let (from, to) = (nl.cur().clone(), nr.cur().clone());
                nl.push(
                    Proof::Ring {
                        lhs: from,
                        rhs: to.clone(),
                    },
                    to,
                );
                return Ok(meet(nl, nr));
            }
            return Err(self.stopped_at(nl.cur(), nr.cur()));
        };
        if depth == 0 || is_bool_value(&g) || self.hyp_for(&g).is_some() {
            return Err(self.stopped_at(l.chain.cur(), r.chain.cur()));
        }
        let hyp = self.fresh_hyp();
        let branch = |env: &mut Self, v: bool| -> Result<Proof, String> {
            env.hyps
                .push((hyp.clone(), Eqn::new(canon(&g), term::boolean(v))));
            let out = env.prove_by_evaluation(lhs, rhs, depth - 1);
            env.hyps.pop();
            out
        };
        let if_true = branch(self, true)?;
        let if_false = branch(self, false)?;
        Ok(Proof::Cases {
            on: canon(&g),
            hyp,
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        })
    }
}

impl Env<'_> {
    /// The refusal for two sides evaluation cannot bring together: both
    /// sides as it left them, and the hypotheses in scope.
    pub(crate) fn stopped_at(&mut self, lhs: &Term, rhs: &Term) -> String {
        use crate::ir::proof_steps::show;
        self.note_fact_hints(&[lhs, rhs]);
        let names = self.inputs.symbol_table;
        let mut s = format!(
            "evaluation stops at `{}` and `{}`",
            show::term(lhs, names),
            show::term(rhs, names)
        );
        if !self.hyps.is_empty() {
            s.push_str(", under ");
            s.push_str(
                &self
                    .hyps
                    .iter()
                    .map(|(n, e)| format!("{n}: {}", show::eqn(e, names)))
                    .collect::<Vec<_>>()
                    .join(", "),
            );
        }
        s
    }
}

impl Env<'_> {
    /// Each builtin fact the law does not cite whose left side matches a
    /// part of `terms`, one match each: pattern matching on the term as it
    /// stands, never a rewrite followed by another match.
    fn note_fact_hints(&mut self, terms: &[&Term]) {
        use crate::ir::proof_steps::{facts, show};
        fn parts<'t>(t: &'t Term, out: &mut Vec<&'t Term>) {
            out.push(t);
            for c in term::children(t) {
                parts(c, out);
            }
        }
        let mut all = Vec::new();
        for t in terms {
            parts(t, &mut all);
        }
        for fact in facts::all() {
            if self.rewrite_laws.iter().any(|l| l.key == fact.key) {
                continue;
            }
            let ob = &fact.script.obligation;
            if let Some(part) = all
                .iter()
                .find(|p| super::rewrite::matches(&ob.lhs, p, &ob.givens, &mut Vec::new()))
            {
                let hint = format!(
                    "`{}` rewrites `{}`; add it to `using`",
                    fact.key,
                    show::term(part, self.inputs.symbol_table)
                );
                if !self.hints.contains(&hint) {
                    self.hints.push(hint);
                }
            }
        }
    }
}

pub(crate) fn empty_script() -> crate::ir::proof_steps::Script {
    use crate::ir::proof_steps::{Obligation, Script};
    Script {
        obligation: Obligation {
            key: String::new(),
            givens: Vec::new(),
            finite: Vec::new(),
            lists: Vec::new(),
            premise: None,
            lhs: term::boolean(true),
            rhs: term::boolean(true),
        },
        defs: Vec::new(),
        consts: Vec::new(),
        laws: Vec::new(),
        proof: Proof::Refl(term::boolean(true)),
    }
}
