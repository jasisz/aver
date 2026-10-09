//! The serialised step data: one S-expression per script.
//!
//! S-expressions rather than JSON because the replayer is written in Aver
//! and reads the data with a forty-line reader; the grammar is small enough
//! to be the format's own documentation:
//!
//! ```text
//! script  := [; proof by RULE sha256:HEX, N steps]   ; the rule that wrote it, for audit only
//!            (steps VERSION (obligation KEY (OGIVEN…) PREMISE TERM TERM)
//!                    (defs (def NAME (PARAM…) ((NAME TERM)…) TERM [bool])…)
//!                    (consts (const NAME TERM)…)
//!                    (laws (law KEY (GIVEN…) PREMISE TERM TERM)…
//!                          (fact KEY (OGIVEN…) PREMISE TERM TERM PROOF)…)
//!   a fact comes after the facts its proof cites, and may cite only them
//!                    (proof PROOF))
//! OGIVEN  := NAME | (NAME TYPE)       ; a given of finite type, with its type
//!          | (NAME (tlist))            ; a given of list type
//!          | (NAME (tint))             ; a given of type Int
//! TYPE    := (tbool) | (tsum CTOR…) | (trec TYPE (FIELD TYPE)…) | (ttuple TYPE…)
//! PREMISE := (none) | TERM
//! TERM    := (i INT) | (b true|false) | (s "TEXT") | (unit) | (v NAME) | (hole)
//!          | (get TERM FIELD) | (call FN TERM…) | (bi BUILTIN TERM…)
//!          | (op OP TERM TERM) | (neg TERM) | (ctor CTOR TERM…)   ; `{}` is (bi Map.empty)
//!          | (match TERM (arm PAT TERM)…) | (str TERM…) | (list TERM…)
//!          | (tuple TERM…) | (rec TYPE (FIELD TERM)…) | (upd TYPE TERM (FIELD TERM)…)
//! PAT     := (pw) | (pv NAME) | (pl TERM) | (pnil) | (pcons NAME NAME)
//!          | (pt PAT…) | (pc CTOR NAME…)
//! PROOF   := (refl TERM) | (symm PROOF) | (trans (TERM…) PROOF…)
//!          | (congr TERM PROOF) | (unfold FN ARM (TERM…) (TERM…) [PROOF])
//!          | (const NAME)
//!          | (arm ARM (TERM…) TERM PROOF) | (proj TERM) | (cell TERM) | (hyp NAME)
//!          | (rule RULE ((NAME TERM)…) PROOF…) | (law KEY ((NAME TERM)…) [PROOF])
//!          | (compute TERM TERM) | (cases TERM NAME PROOF PROOF)
//!          | (split FN (TERM…) TERM NAME (case CTOR (NAME…) PROOF)…)   ; CTOR: nil, cons or a constructor
//!          | (have NAME TERM PROOF PROOF)
//!          | (enum NAME TERM TERM PROOF…) | (absurd PROOF TERM TERM)
//!          | (induct FN (TERM…) TERM TERM [(carry NAME…)] (case (NAME…) (IH…) ((INT NAME (TERM…) (PROOF…))…) PROOF)…)
//!          | (listinduct NAME TERM TERM PROOF (NAME NAME) (NAME…) ((NAME (TERM…) ())…) PROOF)
//!          | (listcases NAME TERM TERM PROOF (NAME NAME) PROOF)
//!          | (intinduct NAME TERM TERM NAME PROOF (NAME…) (NAME…) ((NAME (TERM…) (PROOF…))…) PROOF)
//!          | (ring TERM TERM) | (linear TERM BOOL (NAME…) (INT…))
//! ```

use crate::ast::{BinOp, Literal};
use crate::ir::hir::{
    BuiltinCtor, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedPattern, ResolvedStrPart,
};
use crate::ir::identity::FnId;

use super::term::{HOLE, Term};
use super::{FORMAT_VERSION, Finite, Proof, Script};

/// How identities are spelled in the data.
pub trait Names {
    fn fn_name(&self, id: FnId) -> String;
    fn ctor_name(&self, ctor: &ResolvedCtor) -> String;
    /// Whether a value of this type may hold a Float; a named type is
    /// assumed to unless the names know its fields.
    fn may_hold_float(&self, ty: &crate::ast::Type) -> bool {
        crate::ir::SymbolTable::default().may_hold_float(ty)
    }
}

/// Names for data that mentions no user function or type: the builtin
/// facts, whose terms are builtins over variables.
pub struct BuiltinsOnly;

impl Names for BuiltinsOnly {
    fn fn_name(&self, id: FnId) -> String {
        format!("__fn_{}", id.0)
    }

    fn ctor_name(&self, ctor: &ResolvedCtor) -> String {
        match ctor {
            ResolvedCtor::Builtin(b) => builtin_ctor_name(*b).to_string(),
            ResolvedCtor::User { name, .. } | ResolvedCtor::Unresolved { name } => name.clone(),
        }
    }
}

impl Names for crate::ir::SymbolTable {
    fn may_hold_float(&self, ty: &crate::ast::Type) -> bool {
        crate::ir::SymbolTable::may_hold_float(self, ty)
    }

    fn fn_name(&self, id: FnId) -> String {
        let key = &self.fn_entry(id).key;
        match key.scope_str() {
            Some(scope) => format!("{scope}.{}", key.name),
            None => key.name.clone(),
        }
    }

    fn ctor_name(&self, ctor: &ResolvedCtor) -> String {
        match ctor {
            ResolvedCtor::User { type_id, name, .. } => {
                match self.type_entry_if_present(*type_id) {
                    Some(entry) => {
                        let ty = match entry.key.scope_str() {
                            Some(scope) => format!("{scope}.{}", entry.key.name),
                            None => entry.key.name.clone(),
                        };
                        if entry.is_product {
                            ty
                        } else {
                            format!("{ty}.{name}")
                        }
                    }
                    None => name.clone(),
                }
            }
            ResolvedCtor::Builtin(b) => builtin_ctor_name(*b).to_string(),
            ResolvedCtor::Unresolved { name } => name.clone(),
        }
    }
}

pub fn builtin_ctor_name(b: BuiltinCtor) -> &'static str {
    match b {
        BuiltinCtor::ResultOk => "Result.Ok",
        BuiltinCtor::ResultErr => "Result.Err",
        BuiltinCtor::OptionSome => "Option.Some",
        BuiltinCtor::OptionNone => "Option.None",
    }
}

fn quote(s: &str) -> String {
    let mut out = String::from("\"");
    for c in s.chars() {
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

fn op_symbol(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Eq => "==",
        BinOp::Neq => "!=",
        BinOp::Lt => "<",
        BinOp::Gt => ">",
        BinOp::Lte => "<=",
        BinOp::Gte => ">=",
    }
}

/// An operator spelled by the type it works on, so that a rule about Int
/// arithmetic never applies to joining texts or to Float arithmetic, which
/// have other laws (`+` on texts does not commute; on Floats it does not
/// associate). Int keeps the plain symbols. `==` and `!=` between values
/// that may hold a Float are `==.` and `!=.`: there `a == b` does not make
/// `a` and `b` the same value (`0.0 == -0.0`), and `a == a` can be false
/// (NaN), so no rule about equality applies to them.
fn typed_op_symbol(op: BinOp, ty: Option<&crate::ast::Type>, names: &dyn Names) -> String {
    use crate::ast::Type;
    let plain = op_symbol(op);
    let arithmetic = matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div);
    let ordering = matches!(op, BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte);
    let equality = matches!(op, BinOp::Eq | BinOp::Neq);
    match ty {
        Some(Type::Str) if op == BinOp::Add => "++".to_string(),
        Some(Type::Float) if arithmetic || ordering => format!("{plain}."),
        Some(ty) if equality && names.may_hold_float(ty) => format!("{plain}."),
        _ => plain.to_string(),
    }
}

fn literal(lit: &Literal) -> Result<String, String> {
    Ok(match lit {
        Literal::Int(v) => format!("(i {v})"),
        Literal::BigInt(s) => format!("(i {s})"),
        Literal::Bool(b) => format!("(b {b})"),
        Literal::Str(s) => format!("(s {})", quote(s)),
        Literal::Unit => "(unit)".to_string(),
        Literal::Float(_) => return Err("Float terms are outside the step format".into()),
    })
}

fn list(items: &[Term], names: &dyn Names) -> Result<String, String> {
    Ok(items
        .iter()
        .map(|t| term(t, names))
        .collect::<Result<Vec<_>, _>>()?
        .join(" "))
}

fn with_args(head: &str, args: &[Term], names: &dyn Names) -> Result<String, String> {
    if args.is_empty() {
        Ok(format!("({head})"))
    } else {
        Ok(format!("({head} {})", list(args, names)?))
    }
}

pub fn pattern(p: &ResolvedPattern, names: &dyn Names) -> Result<String, String> {
    Ok(match p {
        ResolvedPattern::Wildcard => "(pw)".to_string(),
        ResolvedPattern::Ident(n) if n == "_" => "(pw)".to_string(),
        ResolvedPattern::Ident(n) => format!("(pv {n})"),
        ResolvedPattern::Literal(l) => format!("(pl {})", literal(l)?),
        ResolvedPattern::EmptyList => "(pnil)".to_string(),
        ResolvedPattern::Cons(h, t) => format!("(pcons {h} {t})"),
        ResolvedPattern::Tuple(ps) => format!(
            "(pt {})",
            ps.iter()
                .map(|p| pattern(p, names))
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ),
        ResolvedPattern::Ctor(c, binders) => {
            let mut s = format!("(pc {}", names.ctor_name(c));
            for b in binders {
                s.push(' ');
                s.push_str(b);
            }
            s.push(')');
            s
        }
    })
}

pub fn term(t: &Term, names: &dyn Names) -> Result<String, String> {
    Ok(match &t.node {
        ResolvedExpr::Literal(l) => literal(l)?,
        ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } => {
            if n == HOLE {
                "(hole)".to_string()
            } else {
                format!("(v {n})")
            }
        }
        ResolvedExpr::Attr(o, f) => format!("(get {} {f})", term(o, names)?),
        ResolvedExpr::Call(callee, args) => match callee {
            ResolvedCallee::Fn(id) => {
                with_args(&format!("call {}", names.fn_name(*id)), args, names)?
            }
            ResolvedCallee::Builtin(b) => with_args(&format!("bi {b}"), args, names)?,
            ResolvedCallee::Intrinsic(i) => with_args(&format!("bi {}", i.name()), args, names)?,
            _ => return Err("a call through a local function value".into()),
        },
        ResolvedExpr::TailCall { target, args } => {
            with_args(&format!("call {}", names.fn_name(*target)), args, names)?
        }
        ResolvedExpr::BinOp(op, a, b) => {
            format!(
                "(op {} {} {})",
                typed_op_symbol(
                    *op,
                    t.ty()
                        .filter(|ty| **ty != crate::ast::Type::Bool)
                        .or(a.ty())
                        .or(b.ty()),
                    names
                ),
                term(a, names)?,
                term(b, names)?
            )
        }
        ResolvedExpr::Neg(a) => format!("(neg {})", term(a, names)?),
        ResolvedExpr::Ctor(c, args) => {
            with_args(&format!("ctor {}", names.ctor_name(c)), args, names)?
        }
        ResolvedExpr::Match { subject, arms } => {
            let mut s = format!("(match {}", term(subject, names)?);
            for arm in arms {
                s.push_str(&format!(
                    " (arm {} {})",
                    pattern(&arm.pattern, names)?,
                    term(&arm.body, names)?
                ));
            }
            s.push(')');
            s
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            let mut s = String::from("(str");
            for p in parts {
                s.push(' ');
                match p {
                    ResolvedStrPart::Literal(text) => s.push_str(&format!("(s {})", quote(text))),
                    ResolvedStrPart::Parsed(e) => s.push_str(&term(e, names)?),
                }
            }
            s.push(')');
            s
        }
        ResolvedExpr::List(xs) => with_args("list", xs, names)?,
        ResolvedExpr::Tuple(xs) => with_args("tuple", xs, names)?,
        ResolvedExpr::RecordCreate {
            type_name, fields, ..
        } => {
            let mut s = format!("(rec {type_name}");
            for (f, v) in fields {
                s.push_str(&format!(" ({f} {})", term(v, names)?));
            }
            s.push(')');
            s
        }
        ResolvedExpr::RecordUpdate {
            type_name,
            base,
            updates,
            ..
        } => {
            let mut s = format!("(upd {type_name} {}", term(base, names)?);
            for (f, v) in updates {
                s.push_str(&format!(" ({f} {})", term(v, names)?));
            }
            s.push(')');
            s
        }
        ResolvedExpr::ErrorProp(_) => return Err("`?` is outside the step format".into()),
        // The empty map is the one map literal steps read.
        ResolvedExpr::MapLiteral(kvs) if kvs.is_empty() => "(bi Map.empty)".to_string(),
        ResolvedExpr::MapLiteral(_) => {
            return Err("map literals with entries are outside the step format".into());
        }
        ResolvedExpr::IndependentProduct(..) => {
            return Err("independent products are outside the step format".into());
        }
    })
}

fn bindings(subst: &[(String, Term)], names: &dyn Names) -> Result<String, String> {
    Ok(format!(
        "({})",
        subst
            .iter()
            .map(|(k, v)| Ok(format!("({k} {})", term(v, names)?)))
            .collect::<Result<Vec<_>, String>>()?
            .join(" ")
    ))
}

pub fn proof(p: &Proof, names: &dyn Names) -> Result<String, String> {
    Ok(match p {
        Proof::Refl(t) => format!("(refl {})", term(t, names)?),
        Proof::Symm(p) => format!("(symm {})", proof(p, names)?),
        Proof::Trans { terms, steps } => {
            let mut s = format!("(trans ({})", list(terms, names)?);
            for step in steps {
                s.push(' ');
                s.push_str(&proof(step, names)?);
            }
            s.push(')');
            s
        }
        Proof::Congr { ctx, inner } => {
            format!("(congr {} {})", term(ctx, names)?, proof(inner, names)?)
        }
        Proof::Unfold {
            fn_id,
            arm,
            args,
            binders,
            premise,
        } => {
            let mut s = format!(
                "(unfold {} {arm} ({}) ({})",
                names.fn_name(*fn_id),
                list(args, names)?,
                list(binders, names)?
            );
            if let Some(p) = premise {
                s.push(' ');
                s.push_str(&proof(p, names)?);
            }
            s.push(')');
            s
        }
        Proof::UnfoldConst { name } => format!("(const {name})"),
        Proof::Arm {
            term: t,
            arm,
            binders,
            premise,
        } => format!(
            "(arm {arm} ({}) {} {})",
            list(binders, names)?,
            term(t, names)?,
            proof(premise, names)?
        ),
        Proof::Proj { term: t } => format!("(proj {})", term(t, names)?),
        Proof::Cell { list } => format!("(cell {})", term(list, names)?),
        Proof::Hyp(h) => format!("(hyp {h})"),
        Proof::Rule {
            rule,
            subst,
            premises,
        } => {
            let mut s = format!("(rule {} {}", rule.id(), bindings(subst, names)?);
            for p in premises {
                s.push(' ');
                s.push_str(&proof(p, names)?);
            }
            s.push(')');
            s
        }
        Proof::Law {
            law,
            subst,
            premise,
        } => {
            let mut s = format!("(law {law} {}", bindings(subst, names)?);
            if let Some(p) = premise {
                s.push(' ');
                s.push_str(&proof(p, names)?);
            }
            s.push(')');
            s
        }
        Proof::Compute { lhs, rhs } => {
            format!("(compute {} {})", term(lhs, names)?, term(rhs, names)?)
        }
        Proof::Cases {
            on,
            hyp,
            if_true,
            if_false,
        } => format!(
            "(cases {} {hyp} {} {})",
            term(on, names)?,
            proof(if_true, names)?,
            proof(if_false, names)?
        ),
        Proof::Split {
            fn_id,
            args,
            on,
            hyp,
            cases,
        } => {
            let mut s = format!(
                "(split {} ({}) {} {hyp}",
                names.fn_name(*fn_id),
                list(args, names)?,
                term(on, names)?
            );
            for c in cases {
                let ctor = match &c.ctor {
                    super::SplitCtor::Nil => "nil".to_string(),
                    super::SplitCtor::Cons => "cons".to_string(),
                    super::SplitCtor::Ctor(c) => names.ctor_name(c),
                    // The kernel reads the literal off the arm.
                    super::SplitCtor::Lit(_) => "lit".to_string(),
                    super::SplitCtor::Other => "else".to_string(),
                };
                s.push_str(&format!(
                    " (case {ctor} ({}) {})",
                    c.binders.join(" "),
                    proof(&c.proof, names)?
                ));
            }
            s.push(')');
            s
        }
        Proof::Have {
            name,
            fact,
            proof: p,
            body,
        } => format!(
            "(have {name} {} {} {})",
            term(fact, names)?,
            proof(p, names)?,
            proof(body, names)?
        ),
        Proof::Induct {
            fn_id,
            args,
            lhs,
            rhs,
            carried,
            cases,
        } => {
            let mut s = format!(
                "(induct {} ({}) {} {}",
                names.fn_name(*fn_id),
                list(args, names)?,
                term(lhs, names)?,
                term(rhs, names)?
            );
            if !carried.is_empty() {
                s.push_str(&format!(" (carry {})", carried.join(" ")));
            }
            for c in cases {
                let mut ihs = Vec::new();
                for (k, ih) in c.ihs.iter().enumerate() {
                    match c.carry.get(k).filter(|ps| !ps.is_empty()) {
                        Some(ps) => {
                            let ps: Vec<String> = ps
                                .iter()
                                .map(|p| proof(p, names))
                                .collect::<Result<_, _>>()?;
                            ihs.push(format!("({ih} {})", ps.join(" ")));
                        }
                        None => ihs.push(ih.clone()),
                    }
                }
                let more: Vec<String> = c
                    .more
                    .iter()
                    .map(|(k, ih)| Ok(format!("({k} {})", ih_body(ih, names)?)))
                    .collect::<Result<_, String>>()?;
                s.push_str(&format!(
                    " (case ({}) ({}) ({}) {})",
                    c.binders.join(" "),
                    ihs.join(" "),
                    more.join(" "),
                    proof(&c.proof, names)?
                ));
            }
            s.push(')');
            s
        }
        Proof::Ring { lhs, rhs } => format!("(ring {} {})", term(lhs, names)?, term(rhs, names)?),
        Proof::Linear {
            goal,
            value,
            hyps,
            weights,
        } => format!(
            "(linear {} {value} ({}) ({}))",
            term(goal, names)?,
            hyps.join(" "),
            weights
                .iter()
                .map(|w| w.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        ),
        Proof::Absurd {
            contradiction,
            lhs,
            rhs,
        } => format!(
            "(absurd {} {} {})",
            proof(contradiction, names)?,
            term(lhs, names)?,
            term(rhs, names)?
        ),
        Proof::InductList {
            var,
            lhs,
            rhs,
            nil,
            head,
            tail,
            general,
            ihs,
            cons,
        } => format!(
            "(listinduct {var} {} {} {} ({head} {tail}) ({}) ({}) {})",
            term(lhs, names)?,
            term(rhs, names)?,
            proof(nil, names)?,
            general.join(" "),
            ih_ats(ihs, names)?,
            proof(cons, names)?
        ),
        Proof::ListCases {
            var,
            lhs,
            rhs,
            nil,
            head,
            tail,
            cons,
        } => format!(
            "(listcases {var} {} {} {} ({head} {tail}) {})",
            term(lhs, names)?,
            term(rhs, names)?,
            proof(nil, names)?,
            proof(cons, names)?
        ),
        Proof::Enum {
            var,
            lhs,
            rhs,
            cases,
        } => {
            let mut s = format!("(enum {var} {} {}", term(lhs, names)?, term(rhs, names)?);
            for c in cases {
                s.push(' ');
                s.push_str(&proof(c, names)?);
            }
            s.push(')');
            s
        }
    })
}

pub fn finite(f: &Finite, names: &dyn Names) -> String {
    match f {
        Finite::Bool => "(tbool)".to_string(),
        Finite::Sum(ctors) => {
            let mut s = String::from("(tsum");
            for c in ctors {
                s.push(' ');
                s.push_str(&names.ctor_name(c));
            }
            s.push(')');
            s
        }
        Finite::Record {
            type_name, fields, ..
        } => {
            let mut s = format!("(trec {type_name}");
            for (n, t) in fields {
                s.push_str(&format!(" ({n} {})", finite(t, names)));
            }
            s.push(')');
            s
        }
        Finite::Tuple(parts) => {
            let mut s = String::from("(ttuple");
            for t in parts {
                s.push(' ');
                s.push_str(&finite(t, names));
            }
            s.push(')');
            s
        }
    }
}

fn premise(p: &Option<Term>, names: &dyn Names) -> Result<String, String> {
    match p {
        Some(t) => term(t, names),
        None => Ok("(none)".to_string()),
    }
}

pub fn script(s: &Script, names: &dyn Names) -> Result<String, String> {
    let o = &s.obligation;
    let givens: Vec<String> = o
        .givens
        .iter()
        .map(|g| match o.finite.iter().find(|(n, _)| n == g) {
            Some((_, f)) => format!("({g} {})", finite(f, names)),
            None if o.lists.contains(g) => format!("({g} (tlist))"),
            None if o.ints.contains(g) => format!("({g} (tint))"),
            None => g.clone(),
        })
        .collect();
    // Which rule wrote the proof: a comment, which no checker reads.
    let provenance = match &s.rule {
        Some(rule) => format!(
            "; proof by {} sha256:{}, {} steps\n",
            rule.name,
            rule.hash,
            s.proof.size()
        ),
        None => String::new(),
    };
    let mut out = format!(
        "{provenance}(steps {FORMAT_VERSION}\n (obligation {} ({}) {} {} {})\n (defs",
        o.key,
        givens.join(" "),
        premise(&o.premise, names)?,
        term(&o.lhs, names)?,
        term(&o.rhs, names)?
    );
    for d in &s.defs {
        out.push_str(&format!(
            "\n  (def {} ({}) {} {}{})",
            d.name,
            d.params.join(" "),
            bindings(&d.lets, names)?,
            term(&d.body, names)?,
            if d.returns_bool { " bool" } else { "" }
        ));
    }
    out.push_str(")\n (consts");
    for c in &s.consts {
        out.push_str(&format!(
            "\n  (const {} {})",
            c.name,
            term(&c.value, names)?
        ));
    }
    out.push_str(")\n (sums");
    for sum in &s.sums {
        out.push_str("\n  (sum");
        for (c, n) in sum {
            out.push_str(&format!(" ({} {n})", names.ctor_name(c)));
        }
        out.push(')');
    }
    out.push_str(")\n (laws");
    // A cited fact comes after every fact its own proof cites, each once.
    fn with_facts<'a>(laws: &'a [super::LawRef], out: &mut Vec<&'a super::LawRef>) {
        for l in laws {
            if let Some(fact) = &l.fact {
                with_facts(&fact.laws, out);
            }
            if !out.iter().any(|o| o.key == l.key) {
                out.push(l);
            }
        }
    }
    let mut cited = Vec::new();
    with_facts(&s.laws, &mut cited);
    for l in cited {
        if let Some(fact) = &l.fact {
            let lists = &fact.obligation.lists;
            let givens: Vec<String> = l
                .givens
                .iter()
                .map(|g| {
                    if lists.contains(g) {
                        format!("({g} (tlist))")
                    } else {
                        g.clone()
                    }
                })
                .collect();
            out.push_str(&format!(
                "\n  (fact {} ({}) {} {} {} {})",
                l.key,
                givens.join(" "),
                premise(&l.premise, names)?,
                term(&l.lhs, names)?,
                term(&l.rhs, names)?,
                proof(&fact.proof, names)?
            ));
            continue;
        }
        out.push_str(&format!(
            "\n  (law {} ({}) {} {} {})",
            l.key,
            l.givens.join(" "),
            premise(&l.premise, names)?,
            term(&l.lhs, names)?,
            term(&l.rhs, names)?
        ));
    }
    out.push_str(&format!(")\n (proof {}))\n", proof(&s.proof, names)?));
    Ok(out)
}

/// The hypotheses of an induction step: `(NAME (TERM…) (PROOF…))…`.
fn ih_ats(ihs: &[super::IhAt], names: &dyn Names) -> Result<String, String> {
    let out: Vec<String> = ihs
        .iter()
        .map(|ih| Ok(format!("({})", ih_body(ih, names)?)))
        .collect::<Result<_, String>>()?;
    Ok(out.join(" "))
}

/// One hypothesis without its parentheses: `NAME (TERM…) (PROOF…)`.
fn ih_body(ih: &super::IhAt, names: &dyn Names) -> Result<String, String> {
    let at = ih
        .at
        .iter()
        .map(|t| term(t, names))
        .collect::<Result<Vec<_>, String>>()?
        .join(" ");
    let carry = ih
        .carry
        .iter()
        .map(|p| proof(p, names))
        .collect::<Result<Vec<_>, String>>()?
        .join(" ");
    Ok(format!("{} ({at}) ({carry})", ih.name))
}
