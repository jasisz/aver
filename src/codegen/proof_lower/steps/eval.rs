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
/// pattern's bindings. `None` when no arm can be selected syntactically.
fn select_arm(arms: &[crate::ir::hir::ResolvedMatchArm], v: &Term) -> Option<(u32, Vec<Term>)> {
    use crate::ir::proof_steps::check::{excludes, is_catch_all};
    for (i, arm) in arms.iter().enumerate() {
        let k = (i + 1) as u32;
        if is_catch_all(&arm.pattern) {
            // Chosen for this value once every earlier arm excludes it.
            return arms[..i]
                .iter()
                .all(|earlier| excludes(&earlier.pattern, v))
                .then(|| (k, vec![canon(v)]));
        }
        match (&arm.pattern, &v.node) {
            (ResolvedPattern::Literal(l), ResolvedExpr::Literal(x)) => {
                if l == x {
                    return Some((k, Vec::new()));
                }
            }
            (ResolvedPattern::Ctor(c, names), ResolvedExpr::Ctor(c2, args)) => {
                if c == c2 && names.len() == args.len() {
                    return Some((k, args.clone()));
                }
            }
            (ResolvedPattern::EmptyList, ResolvedExpr::List(xs)) if xs.is_empty() => {
                return Some((k, Vec::new()));
            }
            (ResolvedPattern::Cons(..), ResolvedExpr::Call(ResolvedCallee::Builtin(b), args))
                if b == "List.prepend" && args.len() == 2 =>
            {
                return Some((k, args.clone()));
            }
            // A literal subject meets a different literal / constructor:
            // keep looking. Anything else cannot be decided here.
            (ResolvedPattern::EmptyList, ResolvedExpr::Call(..))
            | (ResolvedPattern::Cons(..), ResolvedExpr::List(_)) => continue,
            _ => return None,
        }
    }
    None
}

fn is_bool_value(t: &Term) -> bool {
    term::bool_value(t).is_some()
}

impl Env<'_> {
    pub(crate) fn whnf(&mut self, t: &Term) -> Result<Eval, String> {
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

    fn settle(&self, cur: &Term, blocked: Option<Term>) -> Step {
        if let Some((name, value)) = self.hyp_for(cur) {
            return Step::Progress(Box::new((Proof::Hyp(name), value)));
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
                    return Step::Progress(Box::new((
                        Proof::Rule {
                            rule,
                            subst: vec![("a".into(), (**a).clone()), ("b".into(), (**b).clone())],
                            premises: vec![Proof::Hyp(name)],
                        },
                        term::boolean(w),
                    )));
                }
            }
        }
        match blocked {
            Some(g) => Step::Blocked(g),
            None => Step::Done,
        }
    }

    fn head_step(&mut self, cur: &Term) -> Result<Step, String> {
        match &cur.node {
            ResolvedExpr::Attr(obj, _) => {
                if matches!(obj.node, ResolvedExpr::RecordCreate { .. }) {
                    let ev = crate::ir::proof_steps::check::conclusion(
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
                    Err(b) => Ok(self.settle(cur, b)),
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
                    Some((k, binders)) => {
                        let (_, premise) = ev.chain.finish();
                        let unfold = Proof::Unfold {
                            fn_id: *id,
                            arm: k,
                            args: args.clone(),
                            binders,
                            premise: Some(Box::new(premise)),
                        };
                        self.mark_used(*id);
                        let script = self.scratch_script();
                        let eq = crate::ir::proof_steps::check::conclusion(
                            &unfold, &script, &self.hyps,
                        )?;
                        Ok(Step::Progress(Box::new((unfold, eq.rhs))))
                    }
                    None => {
                        let g = ev.blocked.or_else(|| {
                            (matches!(value.ty(), Some(crate::ast::Type::Bool))
                                || matches!(value.node, ResolvedExpr::BinOp(..)))
                            .then_some(value.clone())
                        });
                        Ok(self.settle(cur, g))
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
                Ok(self.settle(cur, left.or(right)))
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
                    None => Ok(self.settle(cur, blocked)),
                }
            }
            ResolvedExpr::Call(..) | ResolvedExpr::BinOp(..) | ResolvedExpr::Neg(_) => {
                self.args_then_settle(cur)
            }
            ResolvedExpr::Match { subject, arms } => {
                let ev = self.whnf(subject)?;
                let value = ev.chain.cur().clone();
                match select_arm(arms, &value) {
                    Some((k, binders)) => {
                        let (_, premise) = ev.chain.finish();
                        let arm = Proof::Arm {
                            term: cur.clone(),
                            arm: k,
                            binders,
                            premise: Box::new(premise),
                        };
                        let eq = crate::ir::proof_steps::check::conclusion(
                            &arm,
                            &empty_script(),
                            &self.hyps,
                        )?;
                        Ok(Step::Progress(Box::new((arm, eq.rhs))))
                    }
                    None => {
                        let g = ev.blocked.or_else(|| Some(value.clone()));
                        Ok(self.settle(cur, g))
                    }
                }
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
                None => Ok(self.settle(cur, None)),
            },
            _ => Ok(self.settle(cur, None)),
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
        if !term::is_literal(cur)
            && let Some(v) = term::eval_closed(cur)
        {
            return Ok(Step::Progress(Box::new((
                Proof::Compute {
                    lhs: cur.clone(),
                    rhs: v.clone(),
                },
                v,
            ))));
        }
        Ok(self.settle(cur, blocked))
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
            return Err(self.stopped_at(l.chain.cur(), r.chain.cur()));
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
    pub(crate) fn stopped_at(&self, lhs: &Term, rhs: &Term) -> String {
        use crate::ir::proof_steps::show;
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

pub(crate) fn empty_script() -> crate::ir::proof_steps::Script {
    use crate::ir::proof_steps::{Obligation, Script};
    Script {
        obligation: Obligation {
            key: String::new(),
            givens: Vec::new(),
            finite: Vec::new(),
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
