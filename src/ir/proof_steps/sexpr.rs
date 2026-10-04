//! The serialised step data: one S-expression per script.
//!
//! S-expressions rather than JSON because the replayer is written in Aver
//! and reads the data with a forty-line reader; the grammar is small enough
//! to be the format's own documentation:
//!
//! ```text
//! script  := (steps VERSION (obligation KEY (GIVEN…) PREMISE TERM TERM)
//!                    (defs (def NAME (PARAM…) TERM)…)
//!                    (consts (const NAME TERM)…)
//!                    (laws (law KEY (GIVEN…) PREMISE TERM TERM)…)
//!                    (proof PROOF))
//! PREMISE := (none) | TERM
//! TERM    := (i INT) | (b true|false) | (s "TEXT") | (unit) | (v NAME) | (hole)
//!          | (get TERM FIELD) | (call FN TERM…) | (bi BUILTIN TERM…)
//!          | (op OP TERM TERM) | (neg TERM) | (ctor CTOR TERM…)
//!          | (match TERM (arm PAT TERM)…) | (str TERM…) | (list TERM…)
//!          | (tuple TERM…) | (rec TYPE (FIELD TERM)…) | (upd TYPE TERM (FIELD TERM)…)
//! PAT     := (pw) | (pv NAME) | (pl TERM) | (pnil) | (pcons NAME NAME)
//!          | (pt PAT…) | (pc CTOR NAME…)
//! PROOF   := (refl TERM) | (symm PROOF) | (trans (TERM…) PROOF…)
//!          | (congr TERM PROOF) | (unfold FN ARM (TERM…) (TERM…) [PROOF])
//!          | (const NAME)
//!          | (arm ARM (TERM…) TERM PROOF) | (proj TERM) | (hyp NAME)
//!          | (rule RULE ((NAME TERM)…) PROOF…) | (law KEY ((NAME TERM)…) [PROOF])
//!          | (compute TERM TERM) | (cases TERM NAME PROOF PROOF)
//! ```

use crate::ast::{BinOp, Literal};
use crate::ir::hir::{
    BuiltinCtor, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedPattern, ResolvedStrPart,
};
use crate::ir::identity::FnId;

use super::term::{HOLE, Term};
use super::{FORMAT_VERSION, Proof, Script};

/// How identities are spelled in the data.
pub trait Names {
    fn fn_name(&self, id: FnId) -> String;
    fn ctor_name(&self, ctor: &ResolvedCtor) -> String;
}

impl Names for crate::ir::SymbolTable {
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
                op_symbol(*op),
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
        ResolvedExpr::MapLiteral(_) => {
            return Err("map literals are outside the step format".into());
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
    })
}

fn premise(p: &Option<Term>, names: &dyn Names) -> Result<String, String> {
    match p {
        Some(t) => term(t, names),
        None => Ok("(none)".to_string()),
    }
}

pub fn script(s: &Script, names: &dyn Names) -> Result<String, String> {
    let o = &s.obligation;
    let mut out = format!(
        "(steps {FORMAT_VERSION}\n (obligation {} ({}) {} {} {})\n (defs",
        o.key,
        o.givens.join(" "),
        premise(&o.premise, names)?,
        term(&o.lhs, names)?,
        term(&o.rhs, names)?
    );
    for d in &s.defs {
        out.push_str(&format!(
            "\n  (def {} ({}) {})",
            d.name,
            d.params.join(" "),
            term(&d.body, names)?
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
    out.push_str(")\n (laws");
    for l in &s.laws {
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
