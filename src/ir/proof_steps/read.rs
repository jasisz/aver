//! Reading step data back: the serialised grammar of [`super::sexpr`] into
//! [`Proof`] and [`Term`].
//!
//! A project proof rule (a `rules [...]` module, run by
//! `crate::codegen::proof_lower::steps::by_rule`) returns its proof as text
//! in the step grammar. The compiler reads it back against the goal the rule
//! was given, so that every name in it means what it meant in the goal: a
//! function, constructor or record type the goal does not mention is refused,
//! and a term the goal already contains comes back as that very term, with
//! the types the checker gave it. The reading is not trusted either way: the
//! script is printed again from what was read and the kernel checks that.

use std::collections::HashMap;

use num_bigint::BigInt;

use crate::ast::{BinOp, Literal, Spanned, Type};
use crate::ir::hir::{
    BuiltinIntrinsic, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedMatchArm,
    ResolvedPattern, ResolvedStrPart,
};
use crate::ir::identity::FnId;

use super::sexpr::Names;
use super::term::{self, Term};
use super::{IhAt, InductCase, Proof, Script, SplitCase, SplitCtor, WallRule};

/// One S-expression: an atom, a quoted text, or a list.
#[derive(Debug, Clone, PartialEq)]
pub enum Sx {
    Atom(String),
    Text(String),
    List(Vec<Sx>),
}

impl Sx {
    /// The canonical text, as the step printer writes the same data.
    pub fn print(&self) -> String {
        match self {
            Sx::Atom(a) => a.clone(),
            Sx::Text(t) => {
                let mut out = String::from("\"");
                for c in t.chars() {
                    match c {
                        '"' => out.push_str("\\\""),
                        '\\' => out.push_str("\\\\"),
                        '\n' => out.push_str("\\n"),
                        c => out.push(c),
                    }
                }
                out.push('"');
                out
            }
            Sx::List(xs) => format!(
                "({})",
                xs.iter().map(Sx::print).collect::<Vec<_>>().join(" ")
            ),
        }
    }
}

/// Parse one S-expression and nothing after it but blanks; `;` starts a
/// comment to the end of the line.
pub fn parse(text: &str) -> Result<Sx, String> {
    let chars: Vec<char> = text.chars().collect();
    let mut at = 0;
    let sx = parse_one(&chars, &mut at)?;
    skip_blank(&chars, &mut at);
    if at != chars.len() {
        return Err("text after the S-expression".into());
    }
    Ok(sx)
}

fn skip_blank(cs: &[char], at: &mut usize) {
    while *at < cs.len() {
        if cs[*at] == ';' {
            while *at < cs.len() && cs[*at] != '\n' {
                *at += 1;
            }
        } else if cs[*at].is_whitespace() {
            *at += 1;
        } else {
            break;
        }
    }
}

fn parse_one(cs: &[char], at: &mut usize) -> Result<Sx, String> {
    skip_blank(cs, at);
    match cs.get(*at) {
        None => Err("unexpected end of data".into()),
        Some('(') => {
            *at += 1;
            let mut items = Vec::new();
            loop {
                skip_blank(cs, at);
                match cs.get(*at) {
                    None => return Err("unexpected end of data".into()),
                    Some(')') => {
                        *at += 1;
                        return Ok(Sx::List(items));
                    }
                    _ => items.push(parse_one(cs, at)?),
                }
            }
        }
        Some(')') => Err("unexpected )".into()),
        Some('"') => {
            *at += 1;
            let mut out = String::new();
            loop {
                match cs.get(*at) {
                    None => return Err("unterminated text".into()),
                    Some('"') => {
                        *at += 1;
                        return Ok(Sx::Text(out));
                    }
                    Some('\\') => {
                        match cs.get(*at + 1) {
                            Some('n') => out.push('\n'),
                            Some(c) => out.push(*c),
                            None => return Err("unterminated text".into()),
                        }
                        *at += 2;
                    }
                    Some(c) => {
                        out.push(*c);
                        *at += 1;
                    }
                }
            }
        }
        Some(_) => {
            let start = *at;
            while *at < cs.len() && !matches!(cs[*at], '(' | ')' | '"') && !cs[*at].is_whitespace()
            {
                *at += 1;
            }
            Ok(Sx::Atom(cs[start..*at].iter().collect()))
        }
    }
}

/// What a goal tells the reader about names: every function, constructor,
/// record type and builtin it mentions, the type of every variable and call,
/// and each of its terms by its printed text.
pub struct Reader {
    fns: HashMap<String, FnId>,
    ctors: HashMap<String, ResolvedCtor>,
    records: HashMap<String, Option<crate::ir::identity::TypeId>>,
    builtins: HashMap<String, ResolvedCallee>,
    var_types: std::cell::RefCell<HashMap<String, Type>>,
    call_types: HashMap<String, Type>,
    terms: HashMap<String, Term>,
    def_bodies: HashMap<String, Term>,
}

impl Reader {
    /// Learn the names of `goal` (a script; its proof is not read).
    pub fn for_goal(goal: &Script, names: &dyn Names) -> Result<Self, String> {
        let mut r = Reader {
            fns: HashMap::new(),
            ctors: HashMap::new(),
            records: HashMap::new(),
            builtins: HashMap::new(),
            var_types: std::cell::RefCell::new(HashMap::new()),
            call_types: HashMap::new(),
            terms: HashMap::new(),
            def_bodies: HashMap::new(),
        };
        for d in &goal.defs {
            r.fns.insert(d.name.clone(), d.fn_id);
            r.def_bodies.insert(d.name.clone(), d.body.clone());
        }
        for sum in &goal.sums {
            for (c, _) in sum {
                r.ctors.insert(names.ctor_name(c), c.clone());
            }
        }
        let o = &goal.obligation;
        let mut all: Vec<&Term> = vec![&o.lhs, &o.rhs];
        all.extend(o.premise.iter());
        for d in &goal.defs {
            all.push(&d.body);
            all.extend(d.lets.iter().map(|(_, v)| v));
        }
        all.extend(goal.consts.iter().map(|c| &c.value));
        for l in &goal.laws {
            all.push(&l.lhs);
            all.push(&l.rhs);
            all.extend(l.premise.iter());
        }
        for t in all {
            r.learn(t, names)?;
        }
        Ok(r)
    }

    fn learn(&mut self, t: &Term, names: &dyn Names) -> Result<(), String> {
        let text = super::sexpr::term(t, names)?;
        // A term whose text does not fix its type (an empty list, an empty
        // map, a None) is not one to hand back for every equal text.
        if !polymorphic(&text) {
            self.terms.entry(text).or_insert_with(|| t.clone());
        }
        match &t.node {
            ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } => {
                if let Some(ty) = t.ty() {
                    self.var_types
                        .borrow_mut()
                        .entry(n.clone())
                        .or_insert_with(|| ty.clone());
                }
            }
            ResolvedExpr::Call(callee, _) => match callee {
                ResolvedCallee::Fn(id) => {
                    let name = names.fn_name(*id);
                    if let Some(ty) = t.ty() {
                        self.call_types
                            .entry(name.clone())
                            .or_insert_with(|| ty.clone());
                    }
                    self.fns.entry(name).or_insert(*id);
                }
                ResolvedCallee::Builtin(b) => {
                    self.builtins.insert(b.clone(), callee.clone());
                }
                ResolvedCallee::Intrinsic(i) => {
                    self.builtins.insert(i.name().to_string(), callee.clone());
                }
                _ => {}
            },
            ResolvedExpr::TailCall { target, .. } => {
                self.fns.entry(names.fn_name(*target)).or_insert(*target);
            }
            ResolvedExpr::Ctor(c, _) => {
                self.ctors.insert(names.ctor_name(c), c.clone());
            }
            ResolvedExpr::RecordCreate {
                type_id, type_name, ..
            }
            | ResolvedExpr::RecordUpdate {
                type_id, type_name, ..
            } => {
                self.records.insert(type_name.clone(), *type_id);
            }
            ResolvedExpr::Match { arms, .. } => {
                for arm in arms {
                    self.learn_pattern(&arm.pattern, names);
                }
            }
            _ => {}
        }
        let mut err = None;
        let _ = term::map_children(t, &mut |c| {
            if err.is_none()
                && let Err(e) = self.learn(c, names)
            {
                err = Some(e);
            }
            Ok(c.clone())
        });
        match err {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    fn learn_pattern(&mut self, p: &ResolvedPattern, names: &dyn Names) {
        match p {
            ResolvedPattern::Ctor(c, _) => {
                self.ctors.insert(names.ctor_name(c), c.clone());
            }
            ResolvedPattern::Tuple(ps) => ps.iter().for_each(|q| self.learn_pattern(q, names)),
            _ => {}
        }
    }

    /// Read a proof written in the step grammar.
    pub fn proof_text(&self, text: &str) -> Result<Proof, String> {
        self.proof(&parse(text)?)
    }

    fn atom<'s>(&self, s: &'s Sx) -> Result<&'s str, String> {
        match s {
            Sx::Atom(a) => Ok(a),
            _ => Err(format!("expected an atom, found {}", s.print())),
        }
    }

    fn items<'s>(&self, s: &'s Sx) -> Result<&'s [Sx], String> {
        match s {
            Sx::List(xs) => Ok(xs),
            _ => Err(format!("expected a list, found {}", s.print())),
        }
    }

    fn tagged<'s>(&self, s: &'s Sx) -> Result<(&'s str, &'s [Sx]), String> {
        match s {
            Sx::List(xs) => match xs.split_first() {
                Some((Sx::Atom(tag), rest)) => Ok((tag, rest)),
                _ => Err(format!("expected a tagged list, found {}", s.print())),
            },
            _ => Err(format!("expected a tagged list, found {}", s.print())),
        }
    }

    fn fn_id(&self, name: &str) -> Result<FnId, String> {
        self.fns
            .get(name)
            .copied()
            .ok_or_else(|| format!("the goal has no function {name}"))
    }

    fn ctor(&self, name: &str) -> Result<ResolvedCtor, String> {
        if let Some(c) = self.ctors.get(name) {
            return Ok(c.clone());
        }
        use crate::ir::hir::BuiltinCtor;
        Ok(ResolvedCtor::Builtin(match name {
            "Result.Ok" => BuiltinCtor::ResultOk,
            "Result.Err" => BuiltinCtor::ResultErr,
            "Option.Some" => BuiltinCtor::OptionSome,
            "Option.None" => BuiltinCtor::OptionNone,
            _ => return Err(format!("the goal has no constructor {name}")),
        }))
    }

    fn int(&self, a: &str) -> Result<i64, String> {
        a.parse::<i64>()
            .map_err(|_| format!("malformed number {a}"))
    }

    fn terms(&self, ss: &[Sx]) -> Result<Vec<Term>, String> {
        ss.iter().map(|s| self.term(s)).collect()
    }

    fn term_list(&self, s: &Sx) -> Result<Vec<Term>, String> {
        self.terms(self.items(s)?)
    }

    fn names(&self, s: &Sx) -> Result<Vec<String>, String> {
        self.items(s)?
            .iter()
            .map(|x| self.atom(x).map(str::to_string))
            .collect()
    }

    fn typed(node: ResolvedExpr, ty: Option<Type>) -> Term {
        let t = Spanned::bare(node);
        if let Some(ty) = ty {
            t.set_ty(ty);
        }
        t
    }

    /// One term. A term the goal contains comes back as the goal's own.
    pub fn term(&self, s: &Sx) -> Result<Term, String> {
        if let Some(t) = self.terms.get(&s.print()) {
            return Ok(t.clone());
        }
        let (tag, args) = self.tagged(s)?;
        let one = |i: usize| -> Result<&Sx, String> {
            args.get(i).ok_or_else(|| format!("malformed {tag}"))
        };
        Ok(match tag {
            "i" => {
                let a = self.atom(one(0)?)?;
                let lit = match a.parse::<i64>() {
                    Ok(v) => Literal::Int(v),
                    Err(_) => {
                        a.parse::<BigInt>()
                            .map_err(|_| format!("malformed integer {a}"))?;
                        Literal::BigInt(a.to_string())
                    }
                };
                Self::typed(ResolvedExpr::Literal(lit), Some(Type::Int))
            }
            "b" => match self.atom(one(0)?)? {
                "true" => term::boolean(true),
                "false" => term::boolean(false),
                other => return Err(format!("malformed Bool {other}")),
            },
            "s" => match one(0)? {
                Sx::Text(t) => Self::typed(
                    ResolvedExpr::Literal(Literal::Str(t.clone())),
                    Some(Type::Str),
                ),
                _ => return Err("malformed text".into()),
            },
            "unit" => Self::typed(ResolvedExpr::Literal(Literal::Unit), Some(Type::Unit)),
            "v" => {
                let n = self.atom(one(0)?)?;
                let v = term::var(n);
                if let Some(ty) = self.var_types.borrow().get(n) {
                    v.set_ty(ty.clone());
                }
                v
            }
            "hole" => term::hole(),
            "get" => Spanned::bare(ResolvedExpr::Attr(
                Box::new(self.term(one(0)?)?),
                self.atom(one(1)?)?.to_string(),
            )),
            "call" => {
                let f = self.atom(one(0)?)?;
                let xs = self.terms(&args[1..])?;
                Self::typed(
                    ResolvedExpr::Call(ResolvedCallee::Fn(self.fn_id(f)?), xs),
                    self.call_types.get(f).cloned(),
                )
            }
            "bi" => {
                let b = self.atom(one(0)?)?;
                let xs = self.terms(&args[1..])?;
                if b == "Map.empty" && xs.is_empty() {
                    return Ok(Spanned::bare(ResolvedExpr::MapLiteral(Vec::new())));
                }
                let callee = match self.builtins.get(b) {
                    Some(c) => c.clone(),
                    None => match BuiltinIntrinsic::from_name(b) {
                        Some(i) => ResolvedCallee::Intrinsic(i),
                        None => ResolvedCallee::Builtin(b.to_string()),
                    },
                };
                let mut xs = xs;
                // The empty list a cell ends in has the cell's type.
                if b == "List.prepend"
                    && let [x, tail] = xs.as_mut_slice()
                    && matches!(&tail.node, ResolvedExpr::List(items) if items.is_empty())
                    && tail.ty().is_none()
                    && let Some(ty) = x.ty()
                {
                    tail.set_ty(Type::List(Box::new(ty.clone())));
                }
                let ty = builtin_type(b, &xs);
                Self::typed(ResolvedExpr::Call(callee, xs), ty)
            }
            "ctor" => {
                let c = self.ctor(self.atom(one(0)?)?)?;
                Spanned::bare(ResolvedExpr::Ctor(c, self.terms(&args[1..])?))
            }
            "op" => {
                let sym = self.atom(one(0)?)?;
                let a = self.term(one(1)?)?;
                let b = self.term(one(2)?)?;
                operator(sym, a, b)?
            }
            "neg" => Spanned::bare(ResolvedExpr::Neg(Box::new(self.term(one(0)?)?))),
            "match" => {
                let subject = self.term(one(0)?)?;
                let mut arms = Vec::new();
                for a in &args[1..] {
                    let (t, parts) = self.tagged(a)?;
                    if t != "arm" || parts.len() != 2 {
                        return Err("malformed arm".into());
                    }
                    arms.push(ResolvedMatchArm {
                        pattern: self.pattern(&parts[0])?,
                        body: Box::new(self.term(&parts[1])?),
                        binding_slots: std::sync::OnceLock::new(),
                    });
                }
                Spanned::bare(ResolvedExpr::Match {
                    subject: Box::new(subject),
                    arms,
                })
            }
            "str" => {
                let mut parts = Vec::new();
                for a in args {
                    match self.tagged(a) {
                        Ok(("s", [Sx::Text(t)])) => parts.push(ResolvedStrPart::Literal(t.clone())),
                        _ => parts.push(ResolvedStrPart::Parsed(Box::new(self.term(a)?))),
                    }
                }
                Self::typed(ResolvedExpr::InterpolatedStr(parts), Some(Type::Str))
            }
            "list" => {
                let xs = self.terms(args)?;
                let ty = xs.first().and_then(|x| x.ty().cloned());
                Self::typed(ResolvedExpr::List(xs), ty.map(|t| Type::List(Box::new(t))))
            }
            "tuple" => Spanned::bare(ResolvedExpr::Tuple(self.terms(args)?)),
            "rec" => {
                let ty = self.atom(one(0)?)?;
                Spanned::bare(ResolvedExpr::RecordCreate {
                    type_id: self.record(ty)?,
                    type_name: ty.to_string(),
                    fields: self.fields(&args[1..])?,
                })
            }
            "upd" => {
                let ty = self.atom(one(0)?)?;
                Spanned::bare(ResolvedExpr::RecordUpdate {
                    type_id: self.record(ty)?,
                    type_name: ty.to_string(),
                    base: Box::new(self.term(one(1)?)?),
                    updates: self.fields(&args[2..])?,
                })
            }
            other => return Err(format!("unknown term {other}")),
        })
    }

    fn record(&self, ty: &str) -> Result<Option<crate::ir::identity::TypeId>, String> {
        self.records
            .get(ty)
            .copied()
            .ok_or_else(|| format!("the goal has no record type {ty}"))
    }

    fn fields(&self, ss: &[Sx]) -> Result<Vec<(String, Term)>, String> {
        ss.iter()
            .map(|s| match self.items(s)? {
                [Sx::Atom(n), v] => Ok((n.clone(), self.term(v)?)),
                _ => Err("malformed field".into()),
            })
            .collect()
    }

    fn pattern(&self, s: &Sx) -> Result<ResolvedPattern, String> {
        let (tag, args) = self.tagged(s)?;
        Ok(match (tag, args) {
            ("pw", []) => ResolvedPattern::Wildcard,
            ("pv", [Sx::Atom(n)]) => ResolvedPattern::Ident(n.clone()),
            ("pl", [t]) => match self.term(t)?.node {
                ResolvedExpr::Literal(l) => ResolvedPattern::Literal(l),
                _ => return Err("malformed literal pattern".into()),
            },
            ("pnil", []) => ResolvedPattern::EmptyList,
            ("pcons", [Sx::Atom(h), Sx::Atom(t)]) => ResolvedPattern::Cons(h.clone(), t.clone()),
            ("pt", ps) => ResolvedPattern::Tuple(
                ps.iter()
                    .map(|p| self.pattern(p))
                    .collect::<Result<_, _>>()?,
            ),
            ("pc", [Sx::Atom(c), ns @ ..]) => ResolvedPattern::Ctor(
                self.ctor(c)?,
                ns.iter()
                    .map(|n| self.atom(n).map(str::to_string))
                    .collect::<Result<_, _>>()?,
            ),
            _ => return Err(format!("malformed pattern {}", s.print())),
        })
    }

    fn proofs(&self, ss: &[Sx]) -> Result<Vec<Proof>, String> {
        ss.iter().map(|s| self.proof(s)).collect()
    }

    fn bindings(&self, s: &Sx) -> Result<Vec<(String, Term)>, String> {
        self.fields(self.items(s)?)
    }

    fn opt_proof(&self, ss: &[Sx]) -> Result<Option<Box<Proof>>, String> {
        match ss {
            [] => Ok(None),
            [p] => Ok(Some(Box::new(self.proof(p)?))),
            _ => Err("more than one premise".into()),
        }
    }

    /// One proof step.
    pub fn proof(&self, s: &Sx) -> Result<Proof, String> {
        let (tag, args) = self.tagged(s)?;
        let bad = || format!("malformed {tag}");
        Ok(match (tag, args) {
            ("refl", [t]) => Proof::Refl(self.term(t)?),
            ("symm", [p]) => Proof::Symm(Box::new(self.proof(p)?)),
            ("trans", [ts, ps @ ..]) => Proof::Trans {
                terms: self.term_list(ts)?,
                steps: self.proofs(ps)?,
            },
            ("congr", [c, p]) => Proof::Congr {
                ctx: self.term(c)?,
                inner: Box::new(self.proof(p)?),
            },
            ("unfold", [Sx::Atom(f), Sx::Atom(k), xs, ys, pre @ ..]) => Proof::Unfold {
                fn_id: self.fn_id(f)?,
                arm: u32::try_from(self.int(k)?).map_err(|_| bad())?,
                args: self.term_list(xs)?,
                binders: self.term_list(ys)?,
                premise: self.opt_proof(pre)?,
            },
            ("const", [Sx::Atom(n)]) => Proof::UnfoldConst { name: n.clone() },
            ("arm", [Sx::Atom(k), ys, t, p]) => Proof::Arm {
                term: self.term(t)?,
                arm: u32::try_from(self.int(k)?).map_err(|_| bad())?,
                binders: self.term_list(ys)?,
                premise: Box::new(self.proof(p)?),
            },
            ("proj", [t]) => Proof::Proj {
                term: self.term(t)?,
            },
            ("cell", [t]) => Proof::Cell {
                list: self.term(t)?,
            },
            ("hyp", [Sx::Atom(h)]) => Proof::Hyp(h.clone()),
            ("rule", [Sx::Atom(id), bs, ps @ ..]) => Proof::Rule {
                rule: WallRule::from_id(id).ok_or_else(|| format!("unknown rule {id}"))?,
                subst: self.bindings(bs)?,
                premises: self.proofs(ps)?,
            },
            ("law", [Sx::Atom(k), bs, pre @ ..]) => Proof::Law {
                law: k.clone(),
                subst: self.bindings(bs)?,
                premise: self.opt_proof(pre)?,
            },
            ("compute", [a, b]) => Proof::Compute {
                lhs: self.term(a)?,
                rhs: self.term(b)?,
            },
            ("cases", [on, Sx::Atom(h), t, f]) => Proof::Cases {
                on: self.term(on)?,
                hyp: h.clone(),
                if_true: Box::new(self.proof(t)?),
                if_false: Box::new(self.proof(f)?),
            },
            ("split", [Sx::Atom(f), xs, on, Sx::Atom(h), cs @ ..]) => {
                let fn_id = self.fn_id(f)?;
                let on = self.term(on)?;
                let mut cases = Vec::new();
                for (i, c) in cs.iter().enumerate() {
                    match self.tagged(c)? {
                        ("case", [Sx::Atom(ctor), bs, p]) => {
                            let ctor = self.split_ctor(ctor, f, i)?;
                            let binders = self.names(bs)?;
                            // A list's cell parts have the list's types,
                            // which the binders' later reads need.
                            if let (SplitCtor::Cons, Some(Type::List(elem)), [hd, tl]) =
                                (&ctor, on.ty(), binders.as_slice())
                            {
                                let mut types = self.var_types.borrow_mut();
                                types.insert(hd.clone(), (**elem).clone());
                                types.insert(tl.clone(), Type::List(elem.clone()));
                            }
                            cases.push(SplitCase {
                                ctor,
                                binders,
                                proof: self.proof(p)?,
                            })
                        }
                        _ => return Err("malformed split case".into()),
                    }
                }
                Proof::Split {
                    fn_id,
                    args: self.term_list(xs)?,
                    on,
                    hyp: h.clone(),
                    cases,
                }
            }
            ("have", [Sx::Atom(n), fact, p, body]) => Proof::Have {
                name: n.clone(),
                fact: self.term(fact)?,
                proof: Box::new(self.proof(p)?),
                body: Box::new(self.proof(body)?),
            },
            ("enum", [Sx::Atom(v), l, r, cs @ ..]) => Proof::Enum {
                var: v.clone(),
                lhs: self.term(l)?,
                rhs: self.term(r)?,
                cases: self.proofs(cs)?,
            },
            ("absurd", [p, l, r]) => Proof::Absurd {
                contradiction: Box::new(self.proof(p)?),
                lhs: self.term(l)?,
                rhs: self.term(r)?,
            },
            ("induct", [Sx::Atom(f), xs, l, r, rest @ ..]) => {
                let (carried, cases) = match rest.split_first() {
                    Some((first, more)) => match self.tagged(first) {
                        Ok(("carry", ns)) => (
                            ns.iter()
                                .map(|n| self.atom(n).map(str::to_string))
                                .collect::<Result<Vec<_>, _>>()?,
                            more,
                        ),
                        _ => (Vec::new(), rest),
                    },
                    None => (Vec::new(), rest),
                };
                Proof::Induct {
                    fn_id: self.fn_id(f)?,
                    args: self.term_list(xs)?,
                    lhs: self.term(l)?,
                    rhs: self.term(r)?,
                    carried,
                    cases: cases
                        .iter()
                        .map(|c| self.induct_case(c))
                        .collect::<Result<_, _>>()?,
                }
            }
            ("listinduct", [Sx::Atom(v), l, r, n, ht, gs, hs, c]) => {
                let (head, tail) = match self.items(ht)? {
                    [Sx::Atom(h), Sx::Atom(t)] => (h.clone(), t.clone()),
                    _ => return Err(bad()),
                };
                Proof::InductList {
                    var: v.clone(),
                    lhs: self.term(l)?,
                    rhs: self.term(r)?,
                    nil: Box::new(self.proof(n)?),
                    head,
                    tail,
                    general: self.names(gs)?,
                    ihs: self
                        .items(hs)?
                        .iter()
                        .map(|ih| self.ih_at(self.items(ih)?))
                        .collect::<Result<_, _>>()?,
                    cons: Box::new(self.proof(c)?),
                }
            }
            ("ring", [l, r]) => Proof::Ring {
                lhs: self.term(l)?,
                rhs: self.term(r)?,
            },
            ("linear", [g, Sx::Atom(v), hs, ws]) => Proof::Linear {
                goal: self.term(g)?,
                value: match v.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => return Err(bad()),
                },
                hyps: self.names(hs)?,
                weights: self
                    .items(ws)?
                    .iter()
                    .map(|w| {
                        self.atom(w)?
                            .parse::<BigInt>()
                            .map_err(|_| "malformed weight".to_string())
                    })
                    .collect::<Result<_, _>>()?,
            },
            _ => return Err(format!("unknown or malformed step {tag}")),
        })
    }

    /// The constructor of split case `i` of a split over `f`'s match:
    /// `nil`, `cons`, `else`, a constructor, or `lit`, whose literal is
    /// read off arm `i + 1` of `f`.
    fn split_ctor(&self, ctor: &str, f: &str, i: usize) -> Result<SplitCtor, String> {
        Ok(match ctor {
            "nil" => SplitCtor::Nil,
            "cons" => SplitCtor::Cons,
            "else" => SplitCtor::Other,
            "lit" => {
                let body = self
                    .def_bodies
                    .get(f)
                    .ok_or_else(|| format!("the goal has no definition {f}"))?;
                match &body.node {
                    ResolvedExpr::Match { arms, .. } => match arms.get(i).map(|a| &a.pattern) {
                        Some(ResolvedPattern::Literal(l)) => SplitCtor::Lit(l.clone()),
                        _ => return Err(format!("arm {} of {f} is not a literal", i + 1)),
                    },
                    _ => return Err(format!("{f} is not a match")),
                }
            }
            c => SplitCtor::Ctor(self.ctor(c)?),
        })
    }

    fn ih_at(&self, parts: &[Sx]) -> Result<IhAt, String> {
        match parts {
            [Sx::Atom(n), at, carry] => Ok(IhAt {
                name: n.clone(),
                at: self.term_list(at)?,
                carry: self.proofs(self.items(carry)?)?,
            }),
            _ => Err("malformed induction hypothesis".into()),
        }
    }

    fn induct_case(&self, s: &Sx) -> Result<InductCase, String> {
        match self.tagged(s)? {
            ("case", [bs, hs, ms, p]) => {
                let mut ihs = Vec::new();
                let mut carry = Vec::new();
                for h in self.items(hs)? {
                    match h {
                        Sx::Atom(n) => {
                            ihs.push(n.clone());
                            carry.push(Vec::new());
                        }
                        Sx::List(xs) => match xs.split_first() {
                            Some((Sx::Atom(n), ps)) => {
                                ihs.push(n.clone());
                                carry.push(self.proofs(ps)?);
                            }
                            _ => return Err("malformed hypothesis".into()),
                        },
                        _ => return Err("malformed hypothesis".into()),
                    }
                }
                let more = self
                    .items(ms)?
                    .iter()
                    .map(|m| match self.items(m)? {
                        [Sx::Atom(k), rest @ ..] => Ok((
                            usize::try_from(self.int(k)?)
                                .map_err(|_| "malformed hypothesis".to_string())?,
                            self.ih_at(rest)?,
                        )),
                        _ => Err("malformed hypothesis".to_string()),
                    })
                    .collect::<Result<_, String>>()?;
                Ok(InductCase {
                    binders: self.names(bs)?,
                    ihs,
                    carry,
                    more,
                    proof: self.proof(p)?,
                })
            }
            _ => Err("malformed induction case".into()),
        }
    }
}

/// Whether a term's printed text leaves its type open: it mentions an empty
/// list, an empty map or a constructor of Option or Result.
fn polymorphic(text: &str) -> bool {
    text.contains("(list)")
        || text.contains("Map.empty")
        || text.contains("ctor Option.")
        || text.contains("ctor Result.")
        || text.contains("pc Option.")
        || text.contains("pc Result.")
}

/// The type of a builtin application where the builtin alone says it.
fn builtin_type(name: &str, args: &[Term]) -> Option<Type> {
    let first = || args.first().and_then(|a| a.ty().cloned());
    match name {
        "List.len" | "String.len" => Some(Type::Int),
        "Bool.and" | "Bool.or" | "Bool.not" | "List.contains" => Some(Type::Bool),
        "List.prepend" => args
            .get(1)
            .and_then(|a| a.ty().cloned())
            .or_else(|| first().map(|t| Type::List(Box::new(t)))),
        "List.take" | "List.drop" | "List.reverse" | "List.concat" => first(),
        _ => None,
    }
}

/// An operator by the symbol the step printer writes for it, typed so it
/// prints back as the same symbol.
fn operator(sym: &str, a: Term, b: Term) -> Result<Term, String> {
    let (plain, typed) = match sym.strip_suffix('.') {
        Some(p) => (p, Some(Type::Float)),
        None if sym == "++" => ("+", Some(Type::Str)),
        None => (sym, None),
    };
    let op = match plain {
        "+" => BinOp::Add,
        "-" => BinOp::Sub,
        "*" => BinOp::Mul,
        "/" => BinOp::Div,
        "==" => BinOp::Eq,
        "!=" => BinOp::Neq,
        "<" => BinOp::Lt,
        ">" => BinOp::Gt,
        "<=" => BinOp::Lte,
        ">=" => BinOp::Gte,
        _ => return Err(format!("unknown operator {sym}")),
    };
    if let Some(ty) = &typed
        && a.ty().is_none()
    {
        a.set_ty(ty.clone());
    }
    let t = term::binop(op, a, b);
    if let Some(ty) = typed
        && matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div)
    {
        t.set_ty(ty);
    }
    Ok(t)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_and_prints_back() {
        let text = "(trans ((v x) (s \"a\\\"b\") (i -3)) (hyp when))";
        assert_eq!(parse(text).unwrap().print(), text);
        assert_eq!(
            parse("; a comment\n(a)").unwrap(),
            Sx::List(vec![Sx::Atom("a".into())])
        );
        assert!(parse("(a) b").is_err());
        assert!(parse("(a").is_err());
    }
}
