#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Givens {
    pub names: aver_rt::AverList<AverStr>,
    pub finite: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Given>,
    pub lists: aver_rt::AverList<AverStr>,
}

impl PartialOrd for Givens {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Givens {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.finite.cmp(&other.finite))
            .then_with(|| self.lists.cmp(&other.lists))
            .then_with(|| self.names.cmp(&other.names))
    }
}

impl aver_rt::AverDisplay for Givens {
    fn aver_display(&self) -> String {
        format!(
            "Givens({})",
            vec![
                format!("names: {}", self.names.aver_display_inner()),
                format!("finite: {}", self.finite.aver_display_inner()),
                format!("lists: {}", self.lists.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// An atom's text.
pub fn atom(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(a) => Ok(a),
        _ => Err(AverStr::from("expected an atom")),
    }
}

/// Atoms of a list.
#[inline(always)]
pub fn atoms(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<AverStr>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::atom(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::atoms(&rest)?)))
}

/// The items of a list.
pub fn items(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(xs) => Ok(xs),
        _ => Err(AverStr::from("expected a list")),
    }
}

/// A list headed by an atom: the tag and the arguments.
pub fn tagged(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<
    (
        AverStr,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    ),
    AverStr,
> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, args)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(tag) => {
                        Ok((tag, args))
                    }
                    _ => Err(AverStr::from("expected a tagged list")),
                }
            } else {
                Err(AverStr::from("expected a tagged list"))
            }
        }
        _ => Err(AverStr::from("expected a tagged list")),
    }
}

/// One term.
pub fn term(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (tag, args) = crate::proof_kernel::aver_generated::kernel::decode::tagged(s)?;
        crate::proof_kernel::aver_generated::kernel::decode::termOf(tag, &args)
    }
}

/// A term by its tag.
#[inline(always)]
pub fn termOf(
    tag @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = tag.clone();
        if &*__dispatch_subject == "i" {
            crate::proof_kernel::aver_generated::kernel::decode::intTerm(args)
        } else {
            if &*__dispatch_subject == "b" {
                crate::proof_kernel::aver_generated::kernel::decode::boolTerm(args)
            } else {
                if &*__dispatch_subject == "s" {
                    crate::proof_kernel::aver_generated::kernel::decode::textTerm(args)
                } else {
                    if &*__dispatch_subject == "unit" {
                        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TUnit)
                    } else {
                        if &*__dispatch_subject == "v" {
                            crate::proof_kernel::aver_generated::kernel::decode::nameTerm(args)
                        } else {
                            if &*__dispatch_subject == "hole" {
                                Ok(crate::proof_kernel::aver_generated::kernel::term::Term::THole)
                            } else {
                                if &*__dispatch_subject == "get" {
                                    crate::proof_kernel::aver_generated::kernel::decode::getTerm(
                                        args,
                                    )
                                } else {
                                    if &*__dispatch_subject == "call" {
                                        crate::proof_kernel::aver_generated::kernel::decode::headed(
                                            args,
                                            AverStr::from("call"),
                                        )
                                    } else {
                                        if &*__dispatch_subject == "bi" {
                                            crate::proof_kernel::aver_generated::kernel::decode::headed(args, AverStr::from("bi"))
                                        } else {
                                            if &*__dispatch_subject == "ctor" {
                                                crate::proof_kernel::aver_generated::kernel::decode::headed(args, AverStr::from("ctor"))
                                            } else {
                                                if &*__dispatch_subject == "op" {
                                                    crate::proof_kernel::aver_generated::kernel::decode::opTerm(args)
                                                } else {
                                                    if &*__dispatch_subject == "neg" {
                                                        crate::proof_kernel::aver_generated::kernel::decode::negTerm(args)
                                                    } else {
                                                        if &*__dispatch_subject == "match" {
                                                            crate::proof_kernel::aver_generated::kernel::decode::matchTerm(args)
                                                        } else {
                                                            if &*__dispatch_subject == "str" {
                                                                Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TParts(crate::proof_kernel::aver_generated::kernel::decode::terms(args)?))
                                                            } else {
                                                                if &*__dispatch_subject == "list" {
                                                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TList(crate::proof_kernel::aver_generated::kernel::decode::terms(args)?))
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "tuple"
                                                                    {
                                                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(crate::proof_kernel::aver_generated::kernel::decode::terms(args)?))
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "rec"
                                                                        {
                                                                            crate::proof_kernel::aver_generated::kernel::decode::recTerm(args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "upd"
                                                                            {
                                                                                crate::proof_kernel::aver_generated::kernel::decode::updTerm(args)
                                                                            } else {
                                                                                Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown term ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(tag)))); __b }))
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// (i DIGITS)
pub fn intTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(d) => {
                    let __list_subject = __pat1;
                    if __list_subject.is_empty() {
                        Ok(
                            crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                                ({
                                    let __s = &(d);
                                    __s.parse::<aver_rt::AverInt>()
                                        .map_err(|_| format!("Cannot parse '{}' as Int", __s))
                                })
                                .into_aver()?,
                            ),
                        )
                    } else {
                        Err(AverStr::from("malformed integer"))
                    }
                }
                _ => Err(AverStr::from("malformed integer")),
            }
        } else {
            Err(AverStr::from("malformed integer"))
        }
    }
}

/// (b true) or (b false)
pub fn boolTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat2) => {
                    let __dispatch_subject = __pat2;
                    if &*__dispatch_subject == "true" {
                        {
                            let __list_subject = __pat1;
                            if __list_subject.is_empty() {
                                Ok(
                                    crate::proof_kernel::aver_generated::kernel::term::Term::TBool(
                                        true,
                                    ),
                                )
                            } else {
                                Err(AverStr::from("malformed Bool"))
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "false" {
                            {
                                let __list_subject = __pat1;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false))
                                } else {
                                    Err(AverStr::from("malformed Bool"))
                                }
                            }
                        } else {
                            Err(AverStr::from("malformed Bool"))
                        }
                    }
                }
                _ => Err(AverStr::from("malformed Bool")),
            }
        } else {
            Err(AverStr::from("malformed Bool"))
        }
    }
}

/// (s "text")
pub fn textTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Text(t) => {
                    let __list_subject = __pat1;
                    if __list_subject.is_empty() {
                        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TStr(t))
                    } else {
                        Err(AverStr::from("malformed text"))
                    }
                }
                _ => Err(AverStr::from("malformed text")),
            }
        } else {
            Err(AverStr::from("malformed text"))
        }
    }
}

/// (v NAME)
pub fn nameTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
                    let __list_subject = __pat1;
                    if __list_subject.is_empty() {
                        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TVar(n))
                    } else {
                        Err(AverStr::from("malformed variable"))
                    }
                }
                _ => Err(AverStr::from("malformed variable")),
            }
        } else {
            Err(AverStr::from("malformed variable"))
        }
    }
}

/// (get TERM FIELD)
pub fn getTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((o, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(f) => {
                            let __list_subject = __pat2;
                            if __list_subject.is_empty() {
                                Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TGet(std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::term(&o)?), f))
                            } else {
                                Err(AverStr::from("malformed field access"))
                            }
                        }
                        _ => Err(AverStr::from("malformed field access")),
                    }
                } else {
                    Err(AverStr::from("malformed field access"))
                }
            }
        } else {
            Err(AverStr::from("malformed field access"))
        }
    }
}

/// (call NAME TERM…), (bi NAME TERM…), (ctor NAME TERM…)
pub fn headed(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    kind @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(name) => {
                    crate::proof_kernel::aver_generated::kernel::decode::headedOf(
                        kind,
                        name,
                        &crate::proof_kernel::aver_generated::kernel::decode::terms(&rest)?,
                    )
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("malformed "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(kind))));
                    __b
                })),
            }
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("malformed "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(kind))));
                __b
            }))
        }
    }
}

/// The term for a headed form.
#[inline(always)]
pub fn headedOf(
    kind @ _: AverStr,
    name @ _: AverStr,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = kind;
        if &*__dispatch_subject == "call" {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TCall(name, xs.clone()))
        } else {
            if &*__dispatch_subject == "bi" {
                Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TBi(name, xs.clone()))
            } else {
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        name,
                        xs.clone(),
                    ),
                )
            }
        }
    }
}

/// (op OP TERM TERM)
pub fn opTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(o) => {
                    let __list_subject = __pat1;
                    if let Some((a, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat2;
                            if let Some((b, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(o, std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::term(&a)?), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::term(&b)?)))
                                    } else {
                                        Err(AverStr::from("malformed operator"))
                                    }
                                }
                            } else {
                                Err(AverStr::from("malformed operator"))
                            }
                        }
                    } else {
                        Err(AverStr::from("malformed operator"))
                    }
                }
                _ => Err(AverStr::from("malformed operator")),
            }
        } else {
            Err(AverStr::from("malformed operator"))
        }
    }
}

/// (neg TERM)
pub fn negTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((a, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TNeg(
                            std::sync::Arc::new(
                                crate::proof_kernel::aver_generated::kernel::decode::term(&a)?,
                            ),
                        ),
                    )
                } else {
                    Err(AverStr::from("malformed negation"))
                }
            }
        } else {
            Err(AverStr::from("malformed negation"))
        }
    }
}

/// (match TERM (arm PAT TERM)…)
pub fn matchTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((s, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            Ok(
                crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
                    std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::term(
                        &s,
                    )?),
                    crate::proof_kernel::aver_generated::kernel::decode::arms(&rest)?,
                ),
            )
        } else {
            Err(AverStr::from("malformed match"))
        }
    }
}

/// Match arms.
#[inline(always)]
pub fn arms(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::arm(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::arms(&rest)?)))
}

/// (arm PAT TERM)
pub fn arm(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Arm, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "arm" => {
                                let __list_subject = __pat2;
                                if let Some((p, __pat4)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat4;
                                        if let Some((b, __pat5)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat5;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::decode::pat(&p)?, body: crate::proof_kernel::aver_generated::kernel::decode::term(&b)? })
                                                } else {
                                                    Err(AverStr::from("malformed arm"))
                                                }
                                            }
                                        } else {
                                            Err(AverStr::from("malformed arm"))
                                        }
                                    }
                                } else {
                                    Err(AverStr::from("malformed arm"))
                                }
                            }
                            _ => Err(AverStr::from("malformed arm")),
                        }
                    }
                    _ => Err(AverStr::from("malformed arm")),
                }
            } else {
                Err(AverStr::from("malformed arm"))
            }
        }
        _ => Err(AverStr::from("malformed arm")),
    }
}

/// A pattern.
pub fn pat(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (tag, args) = crate::proof_kernel::aver_generated::kernel::decode::tagged(s)?;
        crate::proof_kernel::aver_generated::kernel::decode::patOf(tag, &args)
    }
}

/// A pattern by its tag.
#[inline(always)]
pub fn patOf(
    tag @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = tag.clone();
        if &*__dispatch_subject == "pw" {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Pat::PWild)
        } else {
            if &*__dispatch_subject == "pv" {
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Pat::PVar(
                        crate::proof_kernel::aver_generated::kernel::decode::onlyAtom(args)?,
                    ),
                )
            } else {
                if &*__dispatch_subject == "pl" {
                    crate::proof_kernel::aver_generated::kernel::decode::patLit(args)
                } else {
                    if &*__dispatch_subject == "pnil" {
                        Ok(crate::proof_kernel::aver_generated::kernel::term::Pat::PNil)
                    } else {
                        if &*__dispatch_subject == "pcons" {
                            crate::proof_kernel::aver_generated::kernel::decode::patCons(args)
                        } else {
                            if &*__dispatch_subject == "pt" {
                                Ok(
                                    crate::proof_kernel::aver_generated::kernel::term::Pat::PTuple(
                                        crate::proof_kernel::aver_generated::kernel::decode::pats(
                                            args,
                                        )?,
                                    ),
                                )
                            } else {
                                if &*__dispatch_subject == "pc" {
                                    crate::proof_kernel::aver_generated::kernel::decode::patCtor(
                                        args,
                                    )
                                } else {
                                    Err(aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(32))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&AverStr::from("unknown pattern "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(tag)),
                                        ));
                                        __b
                                    }))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// The single atom of a form.
pub fn onlyAtom(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(a) => {
                    let __list_subject = __pat1;
                    if __list_subject.is_empty() {
                        Ok(a)
                    } else {
                        Err(AverStr::from("expected one atom"))
                    }
                }
                _ => Err(AverStr::from("expected one atom")),
            }
        } else {
            Err(AverStr::from("expected one atom"))
        }
    }
}

/// (pl TERM)
pub fn patLit(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((t, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
                            crate::proof_kernel::aver_generated::kernel::decode::term(&t)?,
                        ),
                    )
                } else {
                    Err(AverStr::from("malformed literal pattern"))
                }
            }
        } else {
            Err(AverStr::from("malformed literal pattern"))
        }
    }
}

/// (pcons HEAD TAIL)
pub fn patCons(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                    let __list_subject = __pat1;
                    if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        match __pat2 {
                            crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(t) => {
                                let __list_subject = __pat3;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t))
                                } else {
                                    Err(AverStr::from("malformed cons pattern"))
                                }
                            }
                            _ => Err(AverStr::from("malformed cons pattern")),
                        }
                    } else {
                        Err(AverStr::from("malformed cons pattern"))
                    }
                }
                _ => Err(AverStr::from("malformed cons pattern")),
            }
        } else {
            Err(AverStr::from("malformed cons pattern"))
        }
    }
}

/// (pc CTOR NAME…)
pub fn patCtor(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, names)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(c) => Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(
                        c,
                        crate::proof_kernel::aver_generated::kernel::decode::atoms(&names)?,
                    ),
                ),
                _ => Err(AverStr::from("malformed constructor pattern")),
            }
        } else {
            Err(AverStr::from("malformed constructor pattern"))
        }
    }
}

/// Several patterns.
#[inline(always)]
pub fn pats(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Pat>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::pat(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::pats(&rest)?)))
}

/// (rec TYPE (FIELD TERM)…)
pub fn recTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, fs)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(ty) => Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TRec(
                        ty,
                        crate::proof_kernel::aver_generated::kernel::decode::fields(&fs)?,
                    ),
                ),
                _ => Err(AverStr::from("malformed record")),
            }
        } else {
            Err(AverStr::from("malformed record"))
        }
    }
}

/// (upd TYPE TERM (FIELD TERM)…)
pub fn updTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(ty) => {
                    let __list_subject = __pat1;
                    if let Some((b, fs)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        Ok(
                            crate::proof_kernel::aver_generated::kernel::term::Term::TUpd(
                                ty,
                                std::sync::Arc::new(
                                    crate::proof_kernel::aver_generated::kernel::decode::term(&b)?,
                                ),
                                crate::proof_kernel::aver_generated::kernel::decode::fields(&fs)?,
                            ),
                        )
                    } else {
                        Err(AverStr::from("malformed record update"))
                    }
                }
                _ => Err(AverStr::from("malformed record update")),
            }
        } else {
            Err(AverStr::from("malformed record update"))
        }
    }
}

/// (FIELD TERM)…
#[inline(always)]
pub fn fields(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat3; if let Some((v, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat4; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::Field { name: n, value: crate::proof_kernel::aver_generated::kernel::decode::term(&v)? }, &crate::proof_kernel::aver_generated::kernel::decode::fields(&rest)?)) } else { Err(AverStr::from("malformed field")) } } } else { Err(AverStr::from("malformed field")) } }
        },
        _ => {
            Err(AverStr::from("malformed field"))
        }
    } } else { Err(AverStr::from("malformed field")) } }
        },
        _ => {
            Err(AverStr::from("malformed field"))
        }
    } })
}

/// Several terms.
#[inline(always)]
pub fn terms(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::term(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::terms(&rest)?)))
}

/// A parenthesised list of terms.
#[inline(always)]
pub fn termList(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::decode::terms(
        &crate::proof_kernel::aver_generated::kernel::decode::items(s)?,
    )
}

/// ((NAME TERM)…)
#[inline(always)]
pub fn bindings(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    crate::proof_kernel::aver_generated::kernel::decode::bindingsOf(
        &crate::proof_kernel::aver_generated::kernel::decode::items(s)?,
    )
}

/// Each (NAME TERM).
#[inline(always)]
pub fn bindingsOf(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat3; if let Some((v, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat4; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::bind(n, &crate::proof_kernel::aver_generated::kernel::decode::term(&v)?), &crate::proof_kernel::aver_generated::kernel::decode::bindingsOf(&rest)?)) } else { Err(AverStr::from("malformed binding")) } } } else { Err(AverStr::from("malformed binding")) } }
        },
        _ => {
            Err(AverStr::from("malformed binding"))
        }
    } } else { Err(AverStr::from("malformed binding")) } }
        },
        _ => {
            Err(AverStr::from("malformed binding"))
        }
    } })
}

/// One proof step.
pub fn proof(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (tag, args) = crate::proof_kernel::aver_generated::kernel::decode::tagged(s)?;
        crate::proof_kernel::aver_generated::kernel::decode::proofOf(tag, &args)
    }
}

/// A proof step by its rule.
#[inline(always)]
pub fn proofOf(
    tag @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = tag.clone();
        if &*__dispatch_subject == "refl" {
            Ok(
                crate::proof_kernel::aver_generated::kernel::proof::Proof::PRefl(
                    crate::proof_kernel::aver_generated::kernel::decode::oneTerm(args)?,
                ),
            )
        } else {
            if &*__dispatch_subject == "symm" {
                Ok(
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PSymm(
                        std::sync::Arc::new(
                            crate::proof_kernel::aver_generated::kernel::decode::oneProof(args)?,
                        ),
                    ),
                )
            } else {
                if &*__dispatch_subject == "trans" {
                    crate::proof_kernel::aver_generated::kernel::decode::transProof(args)
                } else {
                    if &*__dispatch_subject == "congr" {
                        crate::proof_kernel::aver_generated::kernel::decode::congrProof(args)
                    } else {
                        if &*__dispatch_subject == "unfold" {
                            crate::proof_kernel::aver_generated::kernel::decode::unfoldProof(args)
                        } else {
                            if &*__dispatch_subject == "const" {
                                Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PConst(crate::proof_kernel::aver_generated::kernel::decode::onlyAtom(args)?))
                            } else {
                                if &*__dispatch_subject == "arm" {
                                    crate::proof_kernel::aver_generated::kernel::decode::armProof(
                                        args,
                                    )
                                } else {
                                    if &*__dispatch_subject == "proj" {
                                        Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PProj(crate::proof_kernel::aver_generated::kernel::decode::oneTerm(args)?))
                                    } else {
                                        if &*__dispatch_subject == "hyp" {
                                            Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PHyp(crate::proof_kernel::aver_generated::kernel::decode::onlyAtom(args)?))
                                        } else {
                                            if &*__dispatch_subject == "rule" {
                                                crate::proof_kernel::aver_generated::kernel::decode::instanceProof(args, AverStr::from("rule"))
                                            } else {
                                                if &*__dispatch_subject == "law" {
                                                    crate::proof_kernel::aver_generated::kernel::decode::instanceProof(args, AverStr::from("law"))
                                                } else {
                                                    if &*__dispatch_subject == "compute" {
                                                        crate::proof_kernel::aver_generated::kernel::decode::computeProof(args)
                                                    } else {
                                                        if &*__dispatch_subject == "cases" {
                                                            crate::proof_kernel::aver_generated::kernel::decode::casesProof(args)
                                                        } else {
                                                            if &*__dispatch_subject == "enum" {
                                                                crate::proof_kernel::aver_generated::kernel::decode::enumProof(args)
                                                            } else {
                                                                if &*__dispatch_subject == "absurd"
                                                                {
                                                                    crate::proof_kernel::aver_generated::kernel::decode::absurdProof(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "induct"
                                                                    {
                                                                        crate::proof_kernel::aver_generated::kernel::decode::inductProof(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "listinduct"
                                                                        {
                                                                            crate::proof_kernel::aver_generated::kernel::decode::listInductProof(args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "ring"
                                                                            {
                                                                                crate::proof_kernel::aver_generated::kernel::decode::ringProof(args)
                                                                            } else {
                                                                                if &*__dispatch_subject == "linear" { crate::proof_kernel::aver_generated::kernel::decode::linearProof(args) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown rule ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(tag)))); __b })) }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// The single term of a step.
pub fn oneTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((t, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    crate::proof_kernel::aver_generated::kernel::decode::term(&t)
                } else {
                    Err(AverStr::from("expected one term"))
                }
            }
        } else {
            Err(AverStr::from("expected one term"))
        }
    }
}

/// The single sub-proof of a step.
pub fn oneProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((p, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    crate::proof_kernel::aver_generated::kernel::decode::proof(&p)
                } else {
                    Err(AverStr::from("expected one proof"))
                }
            }
        } else {
            Err(AverStr::from("expected one proof"))
        }
    }
}

/// Several proofs.
#[inline(always)]
pub fn proofs(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::proof(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::proofs(&rest)?)))
}

/// (trans (TERM…) PROOF…)
pub fn transProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((ts, steps)) = aver_rt::list_uncons_cloned(&__list_subject) {
            Ok(
                crate::proof_kernel::aver_generated::kernel::proof::Proof::PTrans(
                    crate::proof_kernel::aver_generated::kernel::decode::termList(&ts)?,
                    crate::proof_kernel::aver_generated::kernel::decode::proofs(&steps)?,
                ),
            )
        } else {
            Err(AverStr::from("malformed trans"))
        }
    }
}

/// (congr CONTEXT PROOF)
pub fn congrProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((c, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if let Some((p, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(
                                crate::proof_kernel::aver_generated::kernel::proof::Proof::PCongr(
                                    crate::proof_kernel::aver_generated::kernel::decode::term(&c)?,
                                    std::sync::Arc::new(
                                        crate::proof_kernel::aver_generated::kernel::decode::proof(
                                            &p,
                                        )?,
                                    ),
                                ),
                            )
                        } else {
                            Err(AverStr::from("malformed congr"))
                        }
                    }
                } else {
                    Err(AverStr::from("malformed congr"))
                }
            }
        } else {
            Err(AverStr::from("malformed congr"))
        }
    }
}

/// (unfold FN ARM (TERM…) (TERM…) [PROOF])
pub fn unfoldProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(f) => {
                    let __list_subject = __pat1;
                    if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        match __pat2 {
                            crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(k) => {
                                let __list_subject = __pat3;
                                if let Some((xs, __pat4)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat4;
                                        if let Some((ys, given)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PUnfold(f, ({ let __s = &(k); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?, crate::proof_kernel::aver_generated::kernel::decode::termList(&xs)?, crate::proof_kernel::aver_generated::kernel::decode::termList(&ys)?, crate::proof_kernel::aver_generated::kernel::decode::proofs(&given)?))
                                        } else {
                                            Err(AverStr::from("malformed unfold"))
                                        }
                                    }
                                } else {
                                    Err(AverStr::from("malformed unfold"))
                                }
                            }
                            _ => Err(AverStr::from("malformed unfold")),
                        }
                    } else {
                        Err(AverStr::from("malformed unfold"))
                    }
                }
                _ => Err(AverStr::from("malformed unfold")),
            }
        } else {
            Err(AverStr::from("malformed unfold"))
        }
    }
}

/// (arm ARM (TERM…) MATCH PROOF)
pub fn armProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(k) => {
                    let __list_subject = __pat1;
                    if let Some((ys, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat2;
                            if let Some((t, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if let Some((p, __pat4)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat4;
                                            if __list_subject.is_empty() {
                                                Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PArm(({ let __s = &(k); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?, crate::proof_kernel::aver_generated::kernel::decode::termList(&ys)?, crate::proof_kernel::aver_generated::kernel::decode::term(&t)?, std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::proof(&p)?)))
                                            } else {
                                                Err(AverStr::from("malformed arm step"))
                                            }
                                        }
                                    } else {
                                        Err(AverStr::from("malformed arm step"))
                                    }
                                }
                            } else {
                                Err(AverStr::from("malformed arm step"))
                            }
                        }
                    } else {
                        Err(AverStr::from("malformed arm step"))
                    }
                }
                _ => Err(AverStr::from("malformed arm step")),
            }
        } else {
            Err(AverStr::from("malformed arm step"))
        }
    }
}

/// (rule ID ((NAME TERM)…) PROOF…) or (law KEY ((NAME TERM)…) [PROOF])
pub fn instanceProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    kind @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(id) => {
                    let __list_subject = __pat1;
                    if let Some((bs, ps)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        crate::proof_kernel::aver_generated::kernel::decode::instanceOf(
                            kind,
                            id,
                            &crate::proof_kernel::aver_generated::kernel::decode::bindings(&bs)?,
                            &crate::proof_kernel::aver_generated::kernel::decode::proofs(&ps)?,
                        )
                    } else {
                        Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("malformed "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(kind))));
                            __b
                        }))
                    }
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("malformed "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(kind))));
                    __b
                })),
            }
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("malformed "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(kind))));
                __b
            }))
        }
    }
}

/// A rule or law instance.
pub fn instanceOf(
    kind @ _: AverStr,
    id @ _: AverStr,
    bs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    ps @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match &*kind {
        "rule" => Ok(
            crate::proof_kernel::aver_generated::kernel::proof::Proof::PRule(
                id,
                bs.clone(),
                ps.clone(),
            ),
        ),
        _ => Ok(
            crate::proof_kernel::aver_generated::kernel::proof::Proof::PLaw(
                id,
                bs.clone(),
                ps.clone(),
            ),
        ),
    }
}

/// (compute TERM TERM)
pub fn computeProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((a, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if let Some((b, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(
                                crate::proof_kernel::aver_generated::kernel::proof::Proof::PCompute(
                                    crate::proof_kernel::aver_generated::kernel::decode::term(&a)?,
                                    crate::proof_kernel::aver_generated::kernel::decode::term(&b)?,
                                ),
                            )
                        } else {
                            Err(AverStr::from("malformed compute"))
                        }
                    }
                } else {
                    Err(AverStr::from("malformed compute"))
                }
            }
        } else {
            Err(AverStr::from("malformed compute"))
        }
    }
}

/// (cases TERM NAME PROOF PROOF)
pub fn casesProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((on, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                            let __list_subject = __pat2;
                            if let Some((t, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if let Some((f, __pat4)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat4;
                                            if __list_subject.is_empty() {
                                                Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PCases(crate::proof_kernel::aver_generated::kernel::decode::term(&on)?, h, std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::proof(&t)?), std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::proof(&f)?)))
                                            } else {
                                                Err(AverStr::from("malformed cases"))
                                            }
                                        }
                                    } else {
                                        Err(AverStr::from("malformed cases"))
                                    }
                                }
                            } else {
                                Err(AverStr::from("malformed cases"))
                            }
                        }
                        _ => Err(AverStr::from("malformed cases")),
                    }
                } else {
                    Err(AverStr::from("malformed cases"))
                }
            }
        } else {
            Err(AverStr::from("malformed cases"))
        }
    }
}

/// (enum NAME TERM TERM PROOF…)
pub fn enumProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
                    let __list_subject = __pat1;
                    if let Some((l, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat2;
                            if let Some((r, cs)) = aver_rt::list_uncons_cloned(&__list_subject) {
                                Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PEnum(v, crate::proof_kernel::aver_generated::kernel::decode::term(&l)?, crate::proof_kernel::aver_generated::kernel::decode::term(&r)?, crate::proof_kernel::aver_generated::kernel::decode::proofs(&cs)?))
                            } else {
                                Err(AverStr::from("malformed enum"))
                            }
                        }
                    } else {
                        Err(AverStr::from("malformed enum"))
                    }
                }
                _ => Err(AverStr::from("malformed enum")),
            }
        } else {
            Err(AverStr::from("malformed enum"))
        }
    }
}

/// (linear TERM BOOL (NAME…) (INT…))
pub fn linearProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((g, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
                            let __list_subject = __pat2;
                            if let Some((hs, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if let Some((ws, __pat4)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat4;
                                            if __list_subject.is_empty() {
                                                Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PLinear(crate::proof_kernel::aver_generated::kernel::decode::term(&g)?, (&*v == "true"), crate::proof_kernel::aver_generated::kernel::decode::atoms(&crate::proof_kernel::aver_generated::kernel::decode::items(&hs)?)?, crate::proof_kernel::aver_generated::kernel::decode::ints(&crate::proof_kernel::aver_generated::kernel::decode::items(&ws)?)?))
                                            } else {
                                                Err(AverStr::from("malformed linear"))
                                            }
                                        }
                                    } else {
                                        Err(AverStr::from("malformed linear"))
                                    }
                                }
                            } else {
                                Err(AverStr::from("malformed linear"))
                            }
                        }
                        _ => Err(AverStr::from("malformed linear")),
                    }
                } else {
                    Err(AverStr::from("malformed linear"))
                }
            }
        } else {
            Err(AverStr::from("malformed linear"))
        }
    }
}

/// Integers written as atoms.
#[inline(always)]
pub fn ints(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverIntList, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverIntList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(a) => {
            Ok(aver_rt::AverIntList::prepend(({ let __s = &(a); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?, &crate::proof_kernel::aver_generated::kernel::decode::ints(&rest)?))
        },
        _ => {
            Err(AverStr::from("expected an integer"))
        }
    } })
}

/// (ring TERM TERM)
pub fn ringProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((l, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if let Some((r, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(
                                crate::proof_kernel::aver_generated::kernel::proof::Proof::PRing(
                                    crate::proof_kernel::aver_generated::kernel::decode::term(&l)?,
                                    crate::proof_kernel::aver_generated::kernel::decode::term(&r)?,
                                ),
                            )
                        } else {
                            Err(AverStr::from("malformed ring"))
                        }
                    }
                } else {
                    Err(AverStr::from("malformed ring"))
                }
            }
        } else {
            Err(AverStr::from("malformed ring"))
        }
    }
}

/// (listinduct NAME TERM TERM PROOF (HEAD TAIL IH) PROOF)
pub fn listInductProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
                    let __list_subject = __pat1;
                    if let Some((l, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat2;
                            if let Some((r, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if let Some((n, __pat4)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat4;
                                            if let Some((__pat5, __pat6)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat7) => {
            { let __list_subject = __pat7; if let Some((__pat8, __pat9)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat8 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            { let __list_subject = __pat9; if let Some((__pat10, __pat11)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat10 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(t) => {
            { let __list_subject = __pat11; if let Some((__pat12, __pat13)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat12 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(ih) => {
            { let __list_subject = __pat13; if __list_subject.is_empty() { { let __list_subject = __pat6; if let Some((c, __pat14)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat14; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PListInduct(v, crate::proof_kernel::aver_generated::kernel::decode::term(&l)?, crate::proof_kernel::aver_generated::kernel::decode::term(&r)?, std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::proof(&n)?), h, t, ih, std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::proof(&c)?))) } else { Err(AverStr::from("malformed listinduct")) } } } else { Err(AverStr::from("malformed listinduct")) } } } else { Err(AverStr::from("malformed listinduct")) } }
        },
        _ => {
            Err(AverStr::from("malformed listinduct"))
        }
    } } else { Err(AverStr::from("malformed listinduct")) } }
        },
        _ => {
            Err(AverStr::from("malformed listinduct"))
        }
    } } else { Err(AverStr::from("malformed listinduct")) } }
        },
        _ => {
            Err(AverStr::from("malformed listinduct"))
        }
    } } else { Err(AverStr::from("malformed listinduct")) } }
        },
        _ => {
            Err(AverStr::from("malformed listinduct"))
        }
    }
                                            } else {
                                                Err(AverStr::from("malformed listinduct"))
                                            }
                                        }
                                    } else {
                                        Err(AverStr::from("malformed listinduct"))
                                    }
                                }
                            } else {
                                Err(AverStr::from("malformed listinduct"))
                            }
                        }
                    } else {
                        Err(AverStr::from("malformed listinduct"))
                    }
                }
                _ => Err(AverStr::from("malformed listinduct")),
            }
        } else {
            Err(AverStr::from("malformed listinduct"))
        }
    }
}

/// (induct FN (TERM…) TERM TERM (case (NAME…) (NAME…) PROOF)…)
pub fn inductProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((__pat0, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(f) => {
                    let __list_subject = __pat1;
                    if let Some((xs, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat2;
                            if let Some((l, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if let Some((r, cs)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PInduct(f, crate::proof_kernel::aver_generated::kernel::decode::termList(&xs)?, crate::proof_kernel::aver_generated::kernel::decode::term(&l)?, crate::proof_kernel::aver_generated::kernel::decode::term(&r)?, crate::proof_kernel::aver_generated::kernel::decode::cases(&cs)?))
                                    } else {
                                        Err(AverStr::from("malformed induct"))
                                    }
                                }
                            } else {
                                Err(AverStr::from("malformed induct"))
                            }
                        }
                    } else {
                        Err(AverStr::from("malformed induct"))
                    }
                }
                _ => Err(AverStr::from("malformed induct")),
            }
        } else {
            Err(AverStr::from("malformed induct"))
        }
    }
}

/// (case (NAME…) (NAME…) PROOF)…
#[inline(always)]
pub fn cases(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat4) => {
            match &*__pat4 {
        "case" => {
            { let __list_subject = __pat3; if let Some((bs, __pat5)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat5; if let Some((hs, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat6; if let Some((p, __pat7)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat7; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Case { binders: crate::proof_kernel::aver_generated::kernel::decode::atoms(&crate::proof_kernel::aver_generated::kernel::decode::items(&bs)?)?, ihs: crate::proof_kernel::aver_generated::kernel::decode::atoms(&crate::proof_kernel::aver_generated::kernel::decode::items(&hs)?)?, proof: crate::proof_kernel::aver_generated::kernel::decode::proof(&p)? }, &crate::proof_kernel::aver_generated::kernel::decode::cases(&rest)?)) } else { Err(AverStr::from("malformed case")) } } } else { Err(AverStr::from("malformed case")) } } } else { Err(AverStr::from("malformed case")) } } } else { Err(AverStr::from("malformed case")) } }
        },
        _ => {
            Err(AverStr::from("malformed case"))
        }
    }
        },
        _ => {
            Err(AverStr::from("malformed case"))
        }
    } } else { Err(AverStr::from("malformed case")) } }
        },
        _ => {
            Err(AverStr::from("malformed case"))
        }
    } })
}

/// (absurd PROOF TERM TERM)
pub fn absurdProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((p, __pat0)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = __pat0;
                if let Some((l, __pat1)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = __pat1;
                        if let Some((r, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PAbsurd(std::sync::Arc::new(crate::proof_kernel::aver_generated::kernel::decode::proof(&p)?), crate::proof_kernel::aver_generated::kernel::decode::term(&l)?, crate::proof_kernel::aver_generated::kernel::decode::term(&r)?))
                                } else {
                                    Err(AverStr::from("malformed absurd"))
                                }
                            }
                        } else {
                            Err(AverStr::from("malformed absurd"))
                        }
                    }
                } else {
                    Err(AverStr::from("malformed absurd"))
                }
            }
        } else {
            Err(AverStr::from("malformed absurd"))
        }
    }
}

/// A finite type: (tbool), (tsum CTOR…), (trec TYPE (FIELD TYPE)…) or (ttuple TYPE…).
pub fn fin(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Fin, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = crate::proof_kernel::aver_generated::kernel::decode::tagged(s)?;
        {
            let __dispatch_subject = __pat0;
            if &*__dispatch_subject == "tbool" {
                {
                    let __list_subject = __pat1;
                    if __list_subject.is_empty() {
                        Ok(crate::proof_kernel::aver_generated::kernel::term::Fin::FBool)
                    } else {
                        Err(AverStr::from("malformed finite type"))
                    }
                }
            } else {
                if &*__dispatch_subject == "tsum" {
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Fin::FSum(
                            crate::proof_kernel::aver_generated::kernel::decode::atoms(&__pat1)?,
                        ),
                    )
                } else {
                    if &*__dispatch_subject == "trec" {
                        {
                            let __list_subject = __pat1;
                            if let Some((__pat2, fs)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                match __pat2 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            Ok(crate::proof_kernel::aver_generated::kernel::term::Fin::FRec(n, crate::proof_kernel::aver_generated::kernel::decode::finFields(&fs)?))
        },
        _ => {
            Err(AverStr::from("malformed finite type"))
        }
    }
                            } else {
                                Err(AverStr::from("malformed finite type"))
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "ttuple" {
                            Ok(
                                crate::proof_kernel::aver_generated::kernel::term::Fin::FTuple(
                                    crate::proof_kernel::aver_generated::kernel::decode::fins(
                                        &__pat1,
                                    )?,
                                ),
                            )
                        } else {
                            Err(AverStr::from("malformed finite type"))
                        }
                    }
                }
            }
        }
    }
}

/// Several finite types.
#[inline(always)]
pub fn fins(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Fin>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::fin(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::fins(&rest)?)))
}

/// (FIELD TYPE)…
#[inline(always)]
pub fn finFields(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::FinField>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [__pat0, rest] => { match __pat0 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat3; if let Some((t, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat4; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::FinField { name: n, fin: crate::proof_kernel::aver_generated::kernel::decode::fin(&t)? }, &crate::proof_kernel::aver_generated::kernel::decode::finFields(&rest)?)) } else { Err(AverStr::from("malformed finite field")) } } } else { Err(AverStr::from("malformed finite field")) } }
        },
        _ => {
            Err(AverStr::from("malformed finite field"))
        }
    } } else { Err(AverStr::from("malformed finite field")) } }
        },
        _ => {
            Err(AverStr::from("malformed finite field"))
        }
    } })
}

/// The obligation's givens: a bare name, (NAME TYPE) for a given of finite type, or (NAME (tlist)) for a given of list type.
#[inline(always)]
pub fn obligationGivens(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<Givens, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(crate::proof_kernel::aver_generated::kernel::decode::Givens { names: aver_rt::AverList::empty(), finite: aver_rt::AverList::empty(), lists: aver_rt::AverList::empty() }), [s, rest] => crate::proof_kernel::aver_generated::kernel::decode::withGiven(&s, &crate::proof_kernel::aver_generated::kernel::decode::obligationGivens(&rest)?))
}

/// One given in front of the rest.
pub fn withGiven(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
    rest @ _: &Givens,
) -> Result<Givens, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => Ok(
            crate::proof_kernel::aver_generated::kernel::decode::Givens {
                names: aver_rt::AverList::prepend(n, &rest.names.clone()),
                finite: rest.finite.clone(),
                lists: rest.lists.clone(),
            },
        ),
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
                        let __list_subject = __pat2;
                        if let Some((t, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            match t.clone() {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(
                                    __pat4,
                                ) => {
                                    let __list_subject = __pat4;
                                    if let Some((__pat5, __pat6)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat7) => {
            match &*__pat7 {
        "tlist" => {
            { let __list_subject = __pat6; if __list_subject.is_empty() { { let __list_subject = __pat3; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::decode::Givens { names: aver_rt::AverList::prepend(n.clone(), &rest.names.clone()), finite: rest.finite.clone(), lists: aver_rt::AverList::prepend(n, &rest.lists.clone()) }) } else { Err(AverStr::from("malformed given")) } } } else { { let __list_subject = __pat3; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::decode::Givens { names: aver_rt::AverList::prepend(n.clone(), &rest.names.clone()), finite: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Given { name: n, fin: crate::proof_kernel::aver_generated::kernel::decode::fin(&t)? }, &rest.finite.clone()), lists: rest.lists.clone() }) } else { Err(AverStr::from("malformed given")) } } } }
        },
        _ => {
            { let __list_subject = __pat3; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::decode::Givens { names: aver_rt::AverList::prepend(n.clone(), &rest.names.clone()), finite: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Given { name: n, fin: crate::proof_kernel::aver_generated::kernel::decode::fin(&t)? }, &rest.finite.clone()), lists: rest.lists.clone() }) } else { Err(AverStr::from("malformed given")) } }
        }
    }
        },
        _ => {
            { let __list_subject = __pat3; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::decode::Givens { names: aver_rt::AverList::prepend(n.clone(), &rest.names.clone()), finite: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Given { name: n, fin: crate::proof_kernel::aver_generated::kernel::decode::fin(&t)? }, &rest.finite.clone()), lists: rest.lists.clone() }) } else { Err(AverStr::from("malformed given")) } }
        }
    }
                                    } else {
                                        {
                                            let __list_subject = __pat3;
                                            if __list_subject.is_empty() {
                                                Ok(crate::proof_kernel::aver_generated::kernel::decode::Givens { names: aver_rt::AverList::prepend(n.clone(), &rest.names.clone()), finite: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Given { name: n, fin: crate::proof_kernel::aver_generated::kernel::decode::fin(&t)? }, &rest.finite.clone()), lists: rest.lists.clone() })
                                            } else {
                                                Err(AverStr::from("malformed given"))
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        Ok(crate::proof_kernel::aver_generated::kernel::decode::Givens { names: aver_rt::AverList::prepend(n.clone(), &rest.names.clone()), finite: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Given { name: n, fin: crate::proof_kernel::aver_generated::kernel::decode::fin(&t)? }, &rest.finite.clone()), lists: rest.lists.clone() })
                                    } else {
                                        Err(AverStr::from("malformed given"))
                                    }
                                }
                            }
                        } else {
                            Err(AverStr::from("malformed given"))
                        }
                    }
                    _ => Err(AverStr::from("malformed given")),
                }
            } else {
                Err(AverStr::from("malformed given"))
            }
        }
        _ => Err(AverStr::from("malformed given")),
    }
}

/// (obligation KEY (GIVEN…) PREMISE TERM TERM): the law it states and its typed givens.
pub fn obligation(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<
    (
        crate::proof_kernel::aver_generated::kernel::proof::Law,
        Givens,
    ),
    AverStr,
> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "obligation" => {
                                let __list_subject = __pat2;
                                if let Some((__pat4, __pat5)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(k) => {
            { let __list_subject = __pat5; if let Some((gs, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat6; if let Some((p, __pat7)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat7; if let Some((l, __pat8)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat8; if let Some((r, __pat9)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat9; if __list_subject.is_empty() { crate::proof_kernel::aver_generated::kernel::decode::obligationOf(k, &crate::proof_kernel::aver_generated::kernel::decode::obligationGivens(&crate::proof_kernel::aver_generated::kernel::decode::items(&gs)?)?, &crate::proof_kernel::aver_generated::kernel::decode::premise(&p)?, &crate::proof_kernel::aver_generated::kernel::decode::term(&l)?, &crate::proof_kernel::aver_generated::kernel::decode::term(&r)?) } else { Err(AverStr::from("malformed obligation")) } } } else { Err(AverStr::from("malformed obligation")) } } } else { Err(AverStr::from("malformed obligation")) } } } else { Err(AverStr::from("malformed obligation")) } } } else { Err(AverStr::from("malformed obligation")) } }
        },
        _ => {
            Err(AverStr::from("malformed obligation"))
        }
    }
                                } else {
                                    Err(AverStr::from("malformed obligation"))
                                }
                            }
                            _ => Err(AverStr::from("malformed obligation")),
                        }
                    }
                    _ => Err(AverStr::from("malformed obligation")),
                }
            } else {
                Err(AverStr::from("malformed obligation"))
            }
        }
        _ => Err(AverStr::from("malformed obligation")),
    }
}

/// The obligation once its parts are read.
pub fn obligationOf(
    k @ _: AverStr,
    gs @ _: &Givens,
    p @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Result<
    (
        crate::proof_kernel::aver_generated::kernel::proof::Law,
        Givens,
    ),
    AverStr,
> {
    crate::proof_kernel::cancel_checkpoint();
    Ok((
        crate::proof_kernel::aver_generated::kernel::proof::Law {
            key: k,
            givens: gs.names.clone(),
            premise: p.clone(),
            lhs: l.clone(),
            rhs: r.clone(),
        },
        gs.clone(),
    ))
}

/// (none) or a term.
pub fn premise(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "none" => {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(aver_rt::AverList::empty())
                                } else {
                                    Ok(aver_rt::AverList::from_vec(vec![
                                        crate::proof_kernel::aver_generated::kernel::decode::term(
                                            s,
                                        )?,
                                    ]))
                                }
                            }
                            _ => Ok(aver_rt::AverList::from_vec(vec![
                                crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                            ])),
                        }
                    }
                    _ => Ok(aver_rt::AverList::from_vec(vec![
                        crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                    ])),
                }
            } else {
                Ok(aver_rt::AverList::from_vec(vec![
                    crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                ]))
            }
        }
        _ => Ok(aver_rt::AverList::from_vec(vec![
            crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
        ])),
    }
}

/// (law KEY (GIVEN…) PREMISE TERM TERM), or the same headed obligation.
pub fn law(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
    head @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Law, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                        let __list_subject = __pat2;
                        if let Some((__pat3, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject)
                        {
                            match __pat3 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    k,
                                ) => {
                                    let __list_subject = __pat4;
                                    if let Some((gs, __pat5)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat5;
                                            if let Some((p, __pat6)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                {
                                                    let __list_subject = __pat6;
                                                    if let Some((l, __pat7)) =
                                                        aver_rt::list_uncons_cloned(&__list_subject)
                                                    {
                                                        {
                                                            let __list_subject = __pat7;
                                                            if let Some((r, __pat8)) =
                                                                aver_rt::list_uncons_cloned(
                                                                    &__list_subject,
                                                                )
                                                            {
                                                                {
                                                                    let __list_subject = __pat8;
                                                                    if __list_subject.is_empty() {
                                                                        crate::proof_kernel::aver_generated::kernel::decode::lawOf((h == head), k, &crate::proof_kernel::aver_generated::kernel::decode::atoms(&crate::proof_kernel::aver_generated::kernel::decode::items(&gs)?)?, &crate::proof_kernel::aver_generated::kernel::decode::premise(&p)?, &crate::proof_kernel::aver_generated::kernel::decode::term(&l)?, &crate::proof_kernel::aver_generated::kernel::decode::term(&r)?)
                                                                    } else {
                                                                        Err(aver_rt::AverStr::from(
                                                                            {
                                                                                let mut __b = {
                                                                                    let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0));
                                                                                    __b.push_str(&AverStr::from("malformed "));
                                                                                    __b
                                                                                };
                                                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                                                                                __b
                                                                            },
                                                                        ))
                                                                    }
                                                                }
                                                            } else {
                                                                Err(aver_rt::AverStr::from({
                                                                    let mut __b = {
                                                                        let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0));
                                                                        __b.push_str(
                                                                            &AverStr::from(
                                                                                "malformed ",
                                                                            ),
                                                                        );
                                                                        __b
                                                                    };
                                                                    __b.push_str(
                                                                        &aver_rt::AverStr::from(
                                                                            aver_rt::aver_display(
                                                                                &(head),
                                                                            ),
                                                                        ),
                                                                    );
                                                                    __b
                                                                }))
                                                            }
                                                        }
                                                    } else {
                                                        Err(aver_rt::AverStr::from({
                                                            let mut __b = {
                                                                let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0));
                                                                __b.push_str(&AverStr::from(
                                                                    "malformed ",
                                                                ));
                                                                __b
                                                            };
                                                            __b.push_str(&aver_rt::AverStr::from(
                                                                aver_rt::aver_display(&(head)),
                                                            ));
                                                            __b
                                                        }))
                                                    }
                                                }
                                            } else {
                                                Err(aver_rt::AverStr::from({
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(26))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&AverStr::from("malformed "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(head)),
                                                    ));
                                                    __b
                                                }))
                                            }
                                        }
                                    } else {
                                        Err(aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = aver_rt::Buffer::with_capacity(
                                                    (aver_rt::AverInt::from_i64(26))
                                                        .to_usize()
                                                        .unwrap_or(0),
                                                );
                                                __b.push_str(&AverStr::from("malformed "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(head)),
                                            ));
                                            __b
                                        }))
                                    }
                                }
                                _ => Err(aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(26))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&AverStr::from("malformed "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(head),
                                    )));
                                    __b
                                })),
                            }
                        } else {
                            Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&AverStr::from("malformed "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(head),
                                )));
                                __b
                            }))
                        }
                    }
                    _ => Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("malformed "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                        __b
                    })),
                }
            } else {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("malformed "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
                    __b
                }))
            }
        }
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("malformed "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(head))));
            __b
        })),
    }
}

/// A law once its head was checked.
#[inline(always)]
pub fn lawOf(
    expected @ _: bool,
    k @ _: AverStr,
    gs @ _: &aver_rt::AverList<AverStr>,
    p @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    l @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    r @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Law, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if expected {
        Ok(crate::proof_kernel::aver_generated::kernel::proof::Law {
            key: k,
            givens: gs.clone(),
            premise: p.clone(),
            lhs: l.clone(),
            rhs: r.clone(),
        })
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("unexpected head for "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(k))));
            __b
        }))
    }
}

/// Cited laws.
#[inline(always)]
pub fn laws(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(&s, AverStr::from("law"))?, &crate::proof_kernel::aver_generated::kernel::decode::laws(&rest)?)))
}

/// (def NAME (PARAM…) ((NAME TERM)…) TERM): parameters, local bindings in order, the final expression.
pub fn def(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Def, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "def" => {
                                let __list_subject = __pat2;
                                if let Some((__pat4, __pat5)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat5; if let Some((ps, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat6; if let Some((ls, __pat7)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat7; if let Some((b, __pat8)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat8; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Def { name: n, params: crate::proof_kernel::aver_generated::kernel::decode::atoms(&crate::proof_kernel::aver_generated::kernel::decode::items(&ps)?)?, lets: crate::proof_kernel::aver_generated::kernel::decode::bindings(&ls)?, body: crate::proof_kernel::aver_generated::kernel::decode::term(&b)? }) } else { Err(AverStr::from("malformed def")) } } } else { Err(AverStr::from("malformed def")) } } } else { Err(AverStr::from("malformed def")) } } } else { Err(AverStr::from("malformed def")) } }
        },
        _ => {
            Err(AverStr::from("malformed def"))
        }
    }
                                } else {
                                    Err(AverStr::from("malformed def"))
                                }
                            }
                            _ => Err(AverStr::from("malformed def")),
                        }
                    }
                    _ => Err(AverStr::from("malformed def")),
                }
            } else {
                Err(AverStr::from("malformed def"))
            }
        }
        _ => Err(AverStr::from("malformed def")),
    }
}

/// Definitions.
#[inline(always)]
pub fn defs(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::def(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::defs(&rest)?)))
}

/// (const NAME TERM)
pub fn constant(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Const, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "const" => {
                                let __list_subject = __pat2;
                                if let Some((__pat4, __pat5)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat5; if let Some((v, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat6; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Const { name: n, value: crate::proof_kernel::aver_generated::kernel::decode::term(&v)? }) } else { Err(AverStr::from("malformed const")) } } } else { Err(AverStr::from("malformed const")) } }
        },
        _ => {
            Err(AverStr::from("malformed const"))
        }
    }
                                } else {
                                    Err(AverStr::from("malformed const"))
                                }
                            }
                            _ => Err(AverStr::from("malformed const")),
                        }
                    }
                    _ => Err(AverStr::from("malformed const")),
                }
            } else {
                Err(AverStr::from("malformed const"))
            }
        }
        _ => Err(AverStr::from("malformed const")),
    }
}

/// Module-level bindings.
#[inline(always)]
pub fn consts(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ss.clone(), [] => Ok(aver_rt::AverList::empty()), [s, rest] => Ok(aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::constant(&s)?, &crate::proof_kernel::aver_generated::kernel::decode::consts(&rest)?)))
}

/// (steps 5 (obligation …) (defs …) (consts …) (laws …) (proof …)); version 5 only.
pub fn script(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Script, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s.clone() {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        match &*__pat3 {
                            "steps" => {
                                let __list_subject = __pat2;
                                if let Some((__pat4, rest)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
            { let __int_match_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&v)); if __int_match_subject == aver_rt::AverInt::from_i64(53) { { let __list_subject = rest; if let Some((o, __pat5)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat5; if let Some((__pat6, __pat7)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat6 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat8) => {
            { let __list_subject = __pat8; if let Some((__pat9, ds)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat9 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat10) => {
            match &*__pat10 {
        "defs" => {
            { let __list_subject = __pat7; if let Some((__pat11, __pat12)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat11 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat13) => {
            { let __list_subject = __pat13; if let Some((__pat14, cs)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat14 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat15) => {
            match &*__pat15 {
        "consts" => {
            { let __list_subject = __pat12; if let Some((__pat16, __pat17)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat16 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat18) => {
            { let __list_subject = __pat18; if let Some((__pat19, ls)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat19 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat20) => {
            match &*__pat20 {
        "laws" => {
            { let __list_subject = __pat17; if let Some((__pat21, __pat22)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat21 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat23) => {
            { let __list_subject = __pat23; if let Some((__pat24, __pat25)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat24 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat26) => {
            match &*__pat26 {
        "proof" => {
            { let __list_subject = __pat25; if let Some((p, __pat27)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat27; if __list_subject.is_empty() { { let __list_subject = __pat22; if __list_subject.is_empty() { crate::proof_kernel::aver_generated::kernel::decode::scriptOf(&crate::proof_kernel::aver_generated::kernel::decode::obligation(&o)?, &crate::proof_kernel::aver_generated::kernel::decode::defs(&ds)?, &crate::proof_kernel::aver_generated::kernel::decode::consts(&cs)?, &crate::proof_kernel::aver_generated::kernel::decode::laws(&ls)?, &crate::proof_kernel::aver_generated::kernel::decode::proof(&p)?) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unsupported step format version ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(v)))); __b })) } }
        },
        _ => {
            Err(AverStr::from("not a step script"))
        }
    }
                                } else {
                                    Err(AverStr::from("not a step script"))
                                }
                            }
                            _ => Err(AverStr::from("not a step script")),
                        }
                    }
                    _ => Err(AverStr::from("not a step script")),
                }
            } else {
                Err(AverStr::from("not a step script"))
            }
        }
        _ => Err(AverStr::from("not a step script")),
    }
}

/// The script once its parts are read.
pub fn scriptOf(
    o @ _: &(
        crate::proof_kernel::aver_generated::kernel::proof::Law,
        Givens,
    ),
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    ls @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
    p @ _: &crate::proof_kernel::aver_generated::kernel::proof::Proof,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Script, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (ob, gs) = o.clone();
        Ok(crate::proof_kernel::aver_generated::kernel::proof::Script {
            obligation: ob,
            finite: gs.finite,
            lists: gs.lists,
            defs: ds.clone(),
            consts: cs.clone(),
            laws: ls.clone(),
            proof: p.clone(),
        })
    }
}
