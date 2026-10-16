//! The serialised step data: one S-expression per script.
//!
//! S-expressions rather than JSON because the replayer is written in Aver
//! and reads the data with a forty-line reader; the grammar is small enough
//! to be the format's own documentation:
//!
//! ```text
//! script  := [; proof by PLAN sha256:HEX, N steps]   ; the plan that wrote it, for audit only
//!            (steps VERSION (obligation KEY (GIVEN…) PREMISE TERM TERM)
//!                    (defs (def NAME (GIVEN…) TYPE ((NAME TERM)…) TERM)…)
//!                    (consts (const NAME TYPE TERM)…)
//!                    (sums (sum NAME [eq] (CTOR TYPE…)…)…)
//!                    (recs (rec NAME [eq] (FIELD TYPE)…)…)
//!                    (sigs (FN (TYPE…) TYPE)…)
//!                    (laws (law KEY (GIVEN…) PREMISE TERM TERM)…
//!                          (fact KEY (GIVEN…) PREMISE TERM TERM PROOF)…)
//!   a fact comes after the facts its proof cites, and may cite only them
//!                    (proof PROOF))
//! GIVEN   := (NAME TYPE)              ; a variable with its type
//! TYPE    := int | bool | str | float | unit | (list TYPE) | (option TYPE)
//!          | (result TYPE TYPE) | (map TYPE TYPE) | (vector TYPE)
//!          | (tuple TYPE…) | (named NAME)
//!          | (var NAME) | (var NAME eq) ; a type variable of a builtin fact; `eq`: its == is equality
//!          | _                         ; a type the data cannot state: one unknown type, fits only itself
//!   `sums` and `recs` declare every type a TYPE names, with the types of
//!   its fields; `eq` marks one whose `==` is equality. `sigs` gives the
//!   signature of each function a term calls and `defs` does not open.
//!   The kernel types every term from these: a proof states no type.
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
//!          | (listinduct NAME TERM TERM [(carry NAME…)] PROOF (NAME NAME) (NAME…) ((NAME (TERM…) (PROOF…))…) PROOF)
//!          | (listcases NAME TERM TERM PROOF (NAME NAME) PROOF)
//!          | (ring TERM TERM) | (linear TERM BOOL (NAME…) (INT…) (cut INT (INT…))…)
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
    /// How a declared type is named in the data: the way its constructors
    /// are, qualified by its module.
    fn type_name(&self, ty: &crate::ast::Type) -> String {
        match ty {
            crate::ast::Type::Named { name, .. } => name.clone(),
            _ => String::new(),
        }
    }
}

/// A type in the data: `int`, `bool`, `str`, `float`, `unit`, `(list T)`,
/// `(option T)`, `(result T E)`, `(map K V)`, `(vector T)`, `(tuple T…)`,
/// `(named NAME)`, a builtin fact's type variable `(var NAME)`; a type the
/// data cannot state (a function) is `_`, one unknown type that fits only
/// itself.
pub fn ty_text(t: &crate::ast::Type, names: &dyn Names) -> String {
    use crate::ast::Type;
    match t {
        Type::Int => "int".into(),
        Type::Bool => "bool".into(),
        Type::Str => "str".into(),
        Type::Float => "float".into(),
        Type::Unit => "unit".into(),
        Type::List(a) => format!("(list {})", ty_text(a, names)),
        Type::Option(a) => format!("(option {})", ty_text(a, names)),
        Type::Vector(a) => format!("(vector {})", ty_text(a, names)),
        Type::Result(a, b) => format!("(result {} {})", ty_text(a, names), ty_text(b, names)),
        Type::Map(k, v) => format!("(map {} {})", ty_text(k, names), ty_text(v, names)),
        Type::Tuple(xs) => format!(
            "(tuple{})",
            xs.iter()
                .map(|x| format!(" {}", ty_text(x, names)))
                .collect::<String>()
        ),
        Type::Named { .. } => format!("(named {})", names.type_name(t)),
        Type::Var(n) if n == super::facts::KEY_TYPE_VAR => format!("(var {n} eq)"),
        Type::Var(n) => format!("(var {n})"),
        Type::Fn(..) | Type::Invalid => "_".into(),
    }
}

/// `(NAME TYPE)` for a given: its declared type. A given the data holds
/// no type for (one a plan's question names) is written by what it is known
/// to be, a list of anything or an Int, and otherwise as a hole.
fn given_text(name: &str, o: &super::Obligation, names: &dyn Names) -> String {
    let ty = match o.types.iter().find(|(n, _)| n == name) {
        Some((_, t)) => ty_text(t, names),
        None if o.lists.iter().any(|l| l == name) => "(list _)".into(),
        None if o.ints.iter().any(|i| i == name) => "int".into(),
        None => "_".into(),
    };
    format!("({name} {ty})")
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
    fn type_name(&self, ty: &crate::ast::Type) -> String {
        let crate::ast::Type::Named { id, name } = ty else {
            return String::new();
        };
        let id = match id {
            Some(id) => Some(*id),
            None => {
                let bare = name.rsplit('.').next().unwrap_or(name);
                match self.type_ids_named(bare).as_slice() {
                    [one] => Some(*one),
                    _ => None,
                }
            }
        };
        match id.and_then(|id| self.type_entry_if_present(id)) {
            Some(entry) => match entry.key.scope_str() {
                Some(scope) => format!("{scope}.{}", entry.key.name),
                None => entry.key.name.clone(),
            },
            None => name.clone(),
        }
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

/// A record type as a literal of it names it: the way its declaration in
/// the script does (`Names::type_name`), whatever module wrote the literal.
pub fn record_name(
    type_id: Option<crate::ir::identity::TypeId>,
    type_name: &str,
    names: &dyn Names,
) -> String {
    names.type_name(&crate::ast::Type::Named {
        id: type_id,
        name: type_name.to_string(),
    })
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
            type_id,
            type_name,
            fields,
        } => {
            let mut s = format!("(rec {}", record_name(*type_id, type_name, names));
            for (f, v) in fields {
                s.push_str(&format!(" ({f} {})", term(v, names)?));
            }
            s.push(')');
            s
        }
        ResolvedExpr::RecordUpdate {
            type_id,
            type_name,
            base,
            updates,
        } => {
            let mut s = format!(
                "(upd {} {}",
                record_name(*type_id, type_name, names),
                term(base, names)?
            );
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
        Proof::Split { on, hyp, cases } => {
            let lits: Vec<Term> = cases
                .iter()
                .filter(|c| matches!(c.ctor, super::SplitCtor::Lit(_)))
                .filter_map(|c| c.value())
                .collect();
            let mut s = format!(
                "(split {} {hyp} ({})",
                term(on, names)?,
                list(&lits, names)?
            );
            for c in cases {
                let ctor = match &c.ctor {
                    super::SplitCtor::Nil => "nil".to_string(),
                    super::SplitCtor::Cons => "cons".to_string(),
                    super::SplitCtor::Ctor(c) => names.ctor_name(c),
                    // The literal is in the split's list.
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
            cuts,
        } => {
            let ints = |ws: &[num_bigint::BigInt]| {
                ws.iter()
                    .map(|w| w.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            };
            let cuts: String = cuts
                .iter()
                .map(|c| format!(" (cut {} ({}))", c.divisor, ints(&c.weights)))
                .collect();
            format!(
                "(linear {} {value} ({}) ({}){cuts})",
                term(goal, names)?,
                hyps.join(" "),
                ints(weights)
            )
        }
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
            carried,
            nil,
            head,
            tail,
            general,
            ihs,
            cons,
        } => format!(
            "(listinduct {var} {} {}{} {} ({head} {tail}) ({}) ({}) {})",
            term(lhs, names)?,
            term(rhs, names)?,
            if carried.is_empty() {
                String::new()
            } else {
                format!(" (carry {})", carried.join(" "))
            },
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

fn premise(p: &Option<Term>, names: &dyn Names) -> Result<String, String> {
    match p {
        Some(t) => term(t, names),
        None => Ok("(none)".to_string()),
    }
}

pub fn script(s: &Script, names: &dyn Names) -> Result<String, String> {
    let o = &s.obligation;
    let givens: Vec<String> = o.givens.iter().map(|g| given_text(g, o, names)).collect();
    // Which plan wrote the proof: a comment, which no checker reads.
    let provenance = match &s.plan {
        Some(plan) => format!(
            "; proof by {} sha256:{}, {} steps\n",
            plan.name,
            plan.hash,
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
        let params: Vec<String> = d
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let t = d
                    .param_types
                    .get(i)
                    .map(|t| ty_text(t, names))
                    .unwrap_or_else(|| "_".into());
                format!("({p} {t})")
            })
            .collect();
        out.push_str(&format!(
            "\n  (def {} ({}) {} {} {})",
            d.name,
            params.join(" "),
            ty_text(&d.ret, names),
            bindings(&d.lets, names)?,
            term(&d.body, names)?
        ));
    }
    out.push_str(")\n (consts");
    for c in &s.consts {
        out.push_str(&format!(
            "\n  (const {} {} {})",
            c.name,
            ty_text(&c.ty, names),
            term(&c.value, names)?
        ));
    }
    out.push_str(")\n (sums");
    let eq = |eq: bool| if eq { " eq" } else { "" };
    for d in &s.decls.sums {
        out.push_str(&format!("\n  (sum {}{}", d.name, eq(d.eq)));
        for (c, fields) in &d.ctors {
            out.push_str(&format!(" ({}", names.ctor_name(c)));
            for f in fields {
                out.push_str(&format!(" {}", ty_text(f, names)));
            }
            out.push(')');
        }
        out.push(')');
    }
    out.push_str(")\n (recs");
    for d in &s.decls.recs {
        out.push_str(&format!("\n  (rec {}{}", d.name, eq(d.eq)));
        for (f, t) in &d.fields {
            out.push_str(&format!(" ({f} {})", ty_text(t, names)));
        }
        out.push(')');
    }
    out.push_str(")\n (sigs");
    for (id, params, ret) in &s.sigs {
        out.push_str(&format!(
            "\n  ({} ({}) {})",
            names.fn_name(*id),
            params
                .iter()
                .map(|t| ty_text(t, names))
                .collect::<Vec<_>>()
                .join(" "),
            ty_text(ret, names)
        ));
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
            let givens: Vec<String> = l
                .givens
                .iter()
                .map(|g| given_text(g, &fact.obligation, names))
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
        let givens: Vec<String> = l
            .givens
            .iter()
            .map(|g| {
                let t = l
                    .types
                    .iter()
                    .find(|(n, _)| n == g)
                    .map(|(_, t)| ty_text(t, names))
                    .unwrap_or_else(|| "_".into());
                format!("({g} {t})")
            })
            .collect();
        out.push_str(&format!(
            "\n  (law {} ({}) {} {} {})",
            l.key,
            givens.join(" "),
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
