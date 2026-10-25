#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Cited {
    pub laws: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Law>,
    pub facts: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Fact>,
}

impl PartialOrd for Cited {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Cited {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.facts.cmp(&other.facts))
            .then_with(|| self.laws.cmp(&other.laws))
    }
}

impl aver_rt::AverDisplay for Cited {
    fn aver_display(&self) -> String {
        format!(
            "Cited({})",
            vec![
                format!("laws: {}", self.laws.aver_display_inner()),
                format!("facts: {}", self.facts.aver_display_inner())
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
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(a) => {
            let a = a.clone();
            Ok(a)
        }
        _ => Err(::aver_rt::str_lit!("expected an atom")),
    }
}

/// Atoms of a list.
#[inline(always)]
pub fn atoms(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<AverStr>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::atom(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::atoms(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The items of a list.
pub fn items(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(xs) => Ok(xs.clone()),
        _ => Err(::aver_rt::str_lit!("expected a list")),
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
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(args)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(tag) => {
                        let tag = tag.clone();
                        Ok((tag, args))
                    }
                    _ => Err(::aver_rt::str_lit!("expected a tagged list")),
                }
            } else {
                Err(::aver_rt::str_lit!("expected a tagged list"))
            }
        }
        _ => Err(::aver_rt::str_lit!("expected a tagged list")),
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
                                            ::aver_rt::str_lit!("call"),
                                        )
                                    } else {
                                        if &*__dispatch_subject == "bi" {
                                            crate::proof_kernel::aver_generated::kernel::decode::headed(args, ::aver_rt::str_lit!("bi"))
                                        } else {
                                            if &*__dispatch_subject == "ctor" {
                                                crate::proof_kernel::aver_generated::kernel::decode::headed(args, ::aver_rt::str_lit!("ctor"))
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
                                                                Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TParts(crate::proof_kernel::aver_generated::kernel::term::Term__TParts(crate::proof_kernel::aver_generated::kernel::decode::terms(args)?)))
                                                            } else {
                                                                if &*__dispatch_subject == "list" {
                                                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TList(crate::proof_kernel::aver_generated::kernel::term::Term__TList(crate::proof_kernel::aver_generated::kernel::decode::terms(args)?)))
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "tuple"
                                                                    {
                                                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(crate::proof_kernel::aver_generated::kernel::term::Term__TTuple(crate::proof_kernel::aver_generated::kernel::decode::terms(args)?)))
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
                                                                                Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown term ")); __b }; __b.push_display(&(tag)); __b }))
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
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(d) => {
                    let d = d.clone();
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TInt(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TInt(
                                        ({
                                            let __s = &(d);
                                            __s.parse::<aver_rt::AverInt>().map_err(|_| {
                                                format!("Cannot parse '{}' as Int", __s)
                                            })
                                        })
                                        .into_aver()?,
                                    ),
                                ),
                            )
                        } else {
                            Err(::aver_rt::str_lit!("malformed integer"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed integer")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed integer"))
        }
    }
}

/// (b true) or (b false)
pub fn boolTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat2) => {
                    let __pat2 = __pat2.clone();
                    {
                        let __dispatch_subject = __pat2;
                        if &*__dispatch_subject == "true" {
                            {
                                let __list_subject = __pat1;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(true)))
                                } else {
                                    Err(::aver_rt::str_lit!("malformed Bool"))
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "false" {
                                {
                                    let __list_subject = __pat1;
                                    if __list_subject.is_empty() {
                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TBool(crate::proof_kernel::aver_generated::kernel::term::Term__TBool(false)))
                                    } else {
                                        Err(::aver_rt::str_lit!("malformed Bool"))
                                    }
                                }
                            } else {
                                Err(::aver_rt::str_lit!("malformed Bool"))
                            }
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed Bool")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed Bool"))
        }
    }
}

/// (s "text")
pub fn textTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Text(t) => {
                    let t = t.clone();
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TStr(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TStr(
                                        t,
                                    ),
                                ),
                            )
                        } else {
                            Err(::aver_rt::str_lit!("malformed text"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed text")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed text"))
        }
    }
}

/// (v NAME)
pub fn nameTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
                    let n = n.clone();
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(
                                crate::proof_kernel::aver_generated::kernel::term::Term::TVar(
                                    crate::proof_kernel::aver_generated::kernel::term::Term__TVar(
                                        n,
                                    ),
                                ),
                            )
                        } else {
                            Err(::aver_rt::str_lit!("malformed variable"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed variable")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed variable"))
        }
    }
}

/// (get TERM FIELD)
pub fn getTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(o), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(__pat1), Some(__pat2)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(f) => {
                            let f = f.clone();
                            {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TGet(crate::proof_kernel::aver_generated::kernel::term::Term__TGet(crate::proof_kernel::aver_generated::kernel::decode::term(o)?, f)))
                                } else {
                                    Err(::aver_rt::str_lit!("malformed field access"))
                                }
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("malformed field access")),
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed field access"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed field access"))
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
        let __list_subject = args;
        if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(name) => {
                    let name = name.clone();
                    {
                        let __arg5 @ _ =
                            crate::proof_kernel::aver_generated::kernel::decode::terms(&rest)?;
                        crate::proof_kernel::aver_generated::kernel::decode::headedOf(
                            kind, name, &__arg5,
                        )
                    }
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("malformed "));
                        __b
                    };
                    __b.push_display(&(kind));
                    __b
                })),
            }
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("malformed "));
                    __b
                };
                __b.push_display(&(kind));
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
            Ok(
                crate::proof_kernel::aver_generated::kernel::term::Term::TCall(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TCall(
                        name,
                        xs.clone(),
                    ),
                ),
            )
        } else {
            if &*__dispatch_subject == "bi" {
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TBi(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TBi(
                            name,
                            xs.clone(),
                        ),
                    ),
                )
            } else {
                Ok(
                    crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_kernel::aver_generated::kernel::term::Term__TCtor(
                            name,
                            xs.clone(),
                        ),
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
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(o) => {
                    let o = o.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(a), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if let (Some(b), Some(__pat3)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat3;
                                        if __list_subject.is_empty() {
                                            Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TOp(crate::proof_kernel::aver_generated::kernel::term::Term__TOp(o, crate::proof_kernel::aver_generated::kernel::decode::term(a)?, crate::proof_kernel::aver_generated::kernel::decode::term(b)?)))
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed operator"))
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed operator"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed operator"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed operator")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed operator"))
        }
    }
}

/// (neg TERM)
pub fn negTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(a), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TNeg(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TNeg(
                                crate::proof_kernel::aver_generated::kernel::decode::term(a)?,
                            ),
                        ),
                    )
                } else {
                    Err(::aver_rt::str_lit!("malformed negation"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed negation"))
        }
    }
}

/// (match TERM (arm PAT TERM)…)
pub fn matchTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            Ok(
                crate::proof_kernel::aver_generated::kernel::term::Term::TMatch(
                    crate::proof_kernel::aver_generated::kernel::term::Term__TMatch(
                        crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                        crate::proof_kernel::aver_generated::kernel::decode::arms(&rest)?,
                    ),
                ),
            )
        } else {
            Err(::aver_rt::str_lit!("malformed match"))
        }
    }
}

/// Match arms.
#[inline(always)]
pub fn arms(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Arm>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::arm(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::arms(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (arm PAT TERM)
pub fn arm(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Arm, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        match &*__pat3 {
                            "arm" => {
                                let __list_subject = __pat2;
                                if let (Some(p), Some(__pat4)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat4;
                                        if let (Some(b), Some(__pat5)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat5;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Arm { pattern: crate::proof_kernel::aver_generated::kernel::decode::pat(p)?, body: crate::proof_kernel::aver_generated::kernel::decode::term(b)? })
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed arm"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed arm"))
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed arm"))
                                }
                            }
                            _ => Err(::aver_rt::str_lit!("malformed arm")),
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed arm")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed arm"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed arm")),
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
                                            __b.push_str(&::aver_rt::str_lit!("unknown pattern "));
                                            __b
                                        };
                                        __b.push_display(&(tag));
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
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(a) => {
                    let a = a.clone();
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(a)
                        } else {
                            Err(::aver_rt::str_lit!("expected one atom"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("expected one atom")),
            }
        } else {
            Err(::aver_rt::str_lit!("expected one atom"))
        }
    }
}

/// (pl TERM)
pub fn patLit(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(t), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Pat::PLit(
                            crate::proof_kernel::aver_generated::kernel::decode::term(t)?,
                        ),
                    )
                } else {
                    Err(::aver_rt::str_lit!("malformed literal pattern"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed literal pattern"))
        }
    }
}

/// (pcons HEAD TAIL)
pub fn patCons(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                    let h = h.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    t,
                                ) => {
                                    let t = t.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if __list_subject.is_empty() {
                                            Ok(crate::proof_kernel::aver_generated::kernel::term::Pat::PCons(h, t))
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed cons pattern"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed cons pattern")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed cons pattern"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed cons pattern")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed cons pattern"))
        }
    }
}

/// (pc CTOR NAME…)
pub fn patCtor(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Pat, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(names)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(c) => {
                    let c = c.clone();
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Pat::PCtor(
                            c,
                            crate::proof_kernel::aver_generated::kernel::decode::atoms(&names)?,
                        ),
                    )
                }
                _ => Err(::aver_rt::str_lit!("malformed constructor pattern")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed constructor pattern"))
        }
    }
}

/// Several patterns.
#[inline(always)]
pub fn pats(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Pat>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::pat(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::pats(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (rec TYPE (FIELD TERM)…)
pub fn recTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(fs)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(ty) => {
                    let ty = ty.clone();
                    Ok(
                        crate::proof_kernel::aver_generated::kernel::term::Term::TRec(
                            crate::proof_kernel::aver_generated::kernel::term::Term__TRec(
                                ty,
                                crate::proof_kernel::aver_generated::kernel::decode::fields(&fs)?,
                            ),
                        ),
                    )
                }
                _ => Err(::aver_rt::str_lit!("malformed record")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed record"))
        }
    }
}

/// (upd TYPE TERM (FIELD TERM)…)
pub fn updTerm(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Term, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(ty) => {
                    let ty = ty.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(b), Some(fs)) = (__list_subject.first(), __list_subject.tail())
                        {
                            Ok(crate::proof_kernel::aver_generated::kernel::term::Term::TUpd(crate::proof_kernel::aver_generated::kernel::term::Term__TUpd(ty, crate::proof_kernel::aver_generated::kernel::decode::term(b)?, crate::proof_kernel::aver_generated::kernel::decode::fields(&fs)?)))
                        } else {
                            Err(::aver_rt::str_lit!("malformed record update"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed record update")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed record update"))
        }
    }
}

/// (FIELD TERM)…
#[inline(always)]
pub fn fields(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    n,
                                ) => {
                                    let n = n.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(v), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::term::Field { name: n, value: crate::proof_kernel::aver_generated::kernel::decode::term(v)? }, crate::proof_kernel::aver_generated::kernel::decode::fields(&rest)?))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed field"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed field"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed field")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed field"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed field")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Several terms.
#[inline(always)]
pub fn terms(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::terms(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// A parenthesised list of terms.
#[inline(always)]
pub fn termList(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(s)?;
    crate::proof_kernel::aver_generated::kernel::decode::terms(&__arg1)
}

/// ((NAME TERM)…)
#[inline(always)]
pub fn bindings(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    let __arg1 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(s)?;
    crate::proof_kernel::aver_generated::kernel::decode::bindingsOf(&__arg1)
}

/// Each (NAME TERM).
#[inline(always)]
pub fn bindingsOf(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    n,
                                ) => {
                                    let n = n.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(v), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Ok(aver_rt::AverList::prepend_owned({ let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::decode::term(v)?; crate::proof_kernel::aver_generated::kernel::term::bind(n, __arg9) }, crate::proof_kernel::aver_generated::kernel::decode::bindingsOf(&rest)?))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed binding"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed binding"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed binding")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed binding"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed binding")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
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
                    crate::proof_kernel::aver_generated::kernel::proof::Proof__PRefl(
                        crate::proof_kernel::aver_generated::kernel::decode::oneTerm(args)?,
                    ),
                ),
            )
        } else {
            if &*__dispatch_subject == "symm" {
                Ok(
                    crate::proof_kernel::aver_generated::kernel::proof::Proof::PSymm(
                        crate::proof_kernel::aver_generated::kernel::proof::Proof__PSymm(
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
                                Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PConst(crate::proof_kernel::aver_generated::kernel::proof::Proof__PConst(crate::proof_kernel::aver_generated::kernel::decode::onlyAtom(args)?)))
                            } else {
                                if &*__dispatch_subject == "arm" {
                                    crate::proof_kernel::aver_generated::kernel::decode::armProof(
                                        args,
                                    )
                                } else {
                                    if &*__dispatch_subject == "proj" {
                                        Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PProj(crate::proof_kernel::aver_generated::kernel::proof::Proof__PProj(crate::proof_kernel::aver_generated::kernel::decode::oneTerm(args)?)))
                                    } else {
                                        if &*__dispatch_subject == "cell" {
                                            Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PCell(crate::proof_kernel::aver_generated::kernel::proof::Proof__PCell(crate::proof_kernel::aver_generated::kernel::decode::oneTerm(args)?)))
                                        } else {
                                            if &*__dispatch_subject == "hyp" {
                                                Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PHyp(crate::proof_kernel::aver_generated::kernel::proof::Proof__PHyp(crate::proof_kernel::aver_generated::kernel::decode::onlyAtom(args)?)))
                                            } else {
                                                if &*__dispatch_subject == "rule" {
                                                    crate::proof_kernel::aver_generated::kernel::decode::instanceProof(args, ::aver_rt::str_lit!("rule"))
                                                } else {
                                                    if &*__dispatch_subject == "law" {
                                                        crate::proof_kernel::aver_generated::kernel::decode::instanceProof(args, ::aver_rt::str_lit!("law"))
                                                    } else {
                                                        if &*__dispatch_subject == "compute" {
                                                            crate::proof_kernel::aver_generated::kernel::decode::computeProof(args)
                                                        } else {
                                                            if &*__dispatch_subject == "cases" {
                                                                crate::proof_kernel::aver_generated::kernel::decode::casesProof(args)
                                                            } else {
                                                                if &*__dispatch_subject == "split" {
                                                                    crate::proof_kernel::aver_generated::kernel::decode::splitProof(args)
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "have"
                                                                    {
                                                                        crate::proof_kernel::aver_generated::kernel::decode::haveProof(args)
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "enum"
                                                                        {
                                                                            crate::proof_kernel::aver_generated::kernel::decode::enumProof(args)
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "absurd"
                                                                            {
                                                                                crate::proof_kernel::aver_generated::kernel::decode::absurdProof(args)
                                                                            } else {
                                                                                if &*__dispatch_subject == "induct" { crate::proof_kernel::aver_generated::kernel::decode::inductProof(args) } else { if &*__dispatch_subject == "listinduct" { crate::proof_kernel::aver_generated::kernel::decode::listInductProof(args) } else { if &*__dispatch_subject == "listcases" { crate::proof_kernel::aver_generated::kernel::decode::listCasesProof(args) } else { if &*__dispatch_subject == "leninduct" { crate::proof_kernel::aver_generated::kernel::decode::lenInductProof(args) } else { if &*__dispatch_subject == "ring" { crate::proof_kernel::aver_generated::kernel::decode::ringProof(args) } else { if &*__dispatch_subject == "linear" { crate::proof_kernel::aver_generated::kernel::decode::linearProof(args) } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown rule ")); __b }; __b.push_display(&(tag)); __b })) } } } } } }
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
        let __list_subject = args;
        if let (Some(t), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    crate::proof_kernel::aver_generated::kernel::decode::term(t)
                } else {
                    Err(::aver_rt::str_lit!("expected one term"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("expected one term"))
        }
    }
}

/// The single sub-proof of a step.
pub fn oneProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(p), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if __list_subject.is_empty() {
                    crate::proof_kernel::aver_generated::kernel::decode::proof(p)
                } else {
                    Err(::aver_rt::str_lit!("expected one proof"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("expected one proof"))
        }
    }
}

/// Several proofs.
#[inline(always)]
pub fn proofs(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::proof(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::proofs(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (trans (TERM…) PROOF…)
pub fn transProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(ts), Some(steps)) = (__list_subject.first(), __list_subject.tail()) {
            Ok(
                crate::proof_kernel::aver_generated::kernel::proof::Proof::PTrans(
                    crate::proof_kernel::aver_generated::kernel::proof::Proof__PTrans(
                        crate::proof_kernel::aver_generated::kernel::decode::termList(ts)?,
                        crate::proof_kernel::aver_generated::kernel::decode::proofs(&steps)?,
                    ),
                ),
            )
        } else {
            Err(::aver_rt::str_lit!("malformed trans"))
        }
    }
}

/// (congr CONTEXT PROOF)
pub fn congrProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(c), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(p), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PCongr(crate::proof_kernel::aver_generated::kernel::proof::Proof__PCongr(crate::proof_kernel::aver_generated::kernel::decode::term(c)?, crate::proof_kernel::aver_generated::kernel::decode::proof(p)?)))
                        } else {
                            Err(::aver_rt::str_lit!("malformed congr"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed congr"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed congr"))
        }
    }
}

/// (unfold FN ARM (TERM…) (TERM…) [PROOF])
pub fn unfoldProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(f) => {
                    let f = f.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    k,
                                ) => {
                                    let k = k.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(xs), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if let (Some(ys), Some(given)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PUnfold(crate::proof_kernel::aver_generated::kernel::proof::Proof__PUnfold(f, ({ let __s = &(k); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?, crate::proof_kernel::aver_generated::kernel::decode::termList(xs)?, crate::proof_kernel::aver_generated::kernel::decode::termList(ys)?, crate::proof_kernel::aver_generated::kernel::decode::proofs(&given)?)))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed unfold"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed unfold"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed unfold")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed unfold"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed unfold")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed unfold"))
        }
    }
}

/// (arm ARM (TERM…) MATCH PROOF)
pub fn armProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(k) => {
                    let k = k.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(ys), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if let (Some(t), Some(__pat3)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(p), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PArm(crate::proof_kernel::aver_generated::kernel::proof::Proof__PArm(({ let __s = &(k); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?, crate::proof_kernel::aver_generated::kernel::decode::termList(ys)?, crate::proof_kernel::aver_generated::kernel::decode::term(t)?, crate::proof_kernel::aver_generated::kernel::decode::proof(p)?)))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed arm step"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed arm step"))
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed arm step"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed arm step"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed arm step")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed arm step"))
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
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(id) => {
                    let id = id.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(bs), Some(ps)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __arg7 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::decode::bindings(
                                        bs,
                                    )?;
                                {
                                    let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::decode::proofs(&ps)?;
                                    crate::proof_kernel::aver_generated::kernel::decode::instanceOf(
                                        kind, id, &__arg7, &__arg8,
                                    )
                                }
                            }
                        } else {
                            Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("malformed "));
                                    __b
                                };
                                __b.push_display(&(kind));
                                __b
                            }))
                        }
                    }
                }
                _ => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("malformed "));
                        __b
                    };
                    __b.push_display(&(kind));
                    __b
                })),
            }
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("malformed "));
                    __b
                };
                __b.push_display(&(kind));
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
                crate::proof_kernel::aver_generated::kernel::proof::Proof__PRule(
                    id,
                    bs.clone(),
                    ps.clone(),
                ),
            ),
        ),
        _ => Ok(
            crate::proof_kernel::aver_generated::kernel::proof::Proof::PLaw(
                crate::proof_kernel::aver_generated::kernel::proof::Proof__PLaw(
                    id,
                    bs.clone(),
                    ps.clone(),
                ),
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
        let __list_subject = args;
        if let (Some(a), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(b), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PCompute(crate::proof_kernel::aver_generated::kernel::proof::Proof__PCompute(crate::proof_kernel::aver_generated::kernel::decode::term(a)?, crate::proof_kernel::aver_generated::kernel::decode::term(b)?)))
                        } else {
                            Err(::aver_rt::str_lit!("malformed compute"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed compute"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed compute"))
        }
    }
}

/// (cases TERM NAME PROOF PROOF)
pub fn casesProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(on), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(__pat1), Some(__pat2)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                            let h = h.clone();
                            {
                                let __list_subject = __pat2;
                                if let (Some(t), Some(__pat3)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(f), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PCases(crate::proof_kernel::aver_generated::kernel::proof::Proof__PCases(crate::proof_kernel::aver_generated::kernel::decode::term(on)?, h, crate::proof_kernel::aver_generated::kernel::decode::proof(t)?, crate::proof_kernel::aver_generated::kernel::decode::proof(f)?)))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed cases"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed cases"))
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed cases"))
                                }
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("malformed cases")),
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed cases"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed cases"))
        }
    }
}

/// (split TERM NAME (LITERAL…) (case CTOR (NAME…) PROOF)…)
pub fn splitProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(on), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(__pat1), Some(__pat2)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                            let h = h.clone();
                            {
                                let __list_subject = __pat2;
                                if let (Some(ks), Some(cs)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PSplit(crate::proof_kernel::aver_generated::kernel::proof::Proof__PSplit(crate::proof_kernel::aver_generated::kernel::decode::term(on)?, h, crate::proof_kernel::aver_generated::kernel::decode::termList(ks)?, crate::proof_kernel::aver_generated::kernel::decode::splitCases(&cs)?)))
                                } else {
                                    Err(::aver_rt::str_lit!("malformed split"))
                                }
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("malformed split")),
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed split"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed split"))
        }
    }
}

/// (case CTOR (NAME…) PROOF)…
#[inline(always)]
pub fn splitCases(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::SplitCase>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    __pat4,
                                ) => {
                                    let __pat4 = __pat4.clone();
                                    match &*__pat4 {
                                        "case" => {
                                            let __list_subject = __pat3;
                                            if let (Some(__pat5), Some(__pat6)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(c) => {
            let c = c.clone();
            { let __list_subject = __pat6; if let (Some(bs), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if let (Some(p), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat8; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::SplitCase { ctor: c, binders: { let __arg14 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(bs)?; crate::proof_kernel::aver_generated::kernel::decode::atoms(&__arg14) }?, proof: crate::proof_kernel::aver_generated::kernel::decode::proof(p)? }, crate::proof_kernel::aver_generated::kernel::decode::splitCases(&rest)?)) } else { Err(::aver_rt::str_lit!("malformed split case")) } } } else { Err(::aver_rt::str_lit!("malformed split case")) } } } else { Err(::aver_rt::str_lit!("malformed split case")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed split case"))
        }
    }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed split case"))
                                            }
                                        }
                                        _ => Err(::aver_rt::str_lit!("malformed split case")),
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed split case")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed split case"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed split case")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (have NAME TERM PROOF PROOF)
pub fn haveProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                    let h = h.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(fact), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if let (Some(p), Some(__pat3)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(body), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PHave(crate::proof_kernel::aver_generated::kernel::proof::Proof__PHave(h, crate::proof_kernel::aver_generated::kernel::decode::term(fact)?, crate::proof_kernel::aver_generated::kernel::decode::proof(p)?, crate::proof_kernel::aver_generated::kernel::decode::proof(body)?)))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed have"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed have"))
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed have"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed have"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed have")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed have"))
        }
    }
}

/// (enum NAME TERM TERM PROOF…)
pub fn enumProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
                    let v = v.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(l), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if let (Some(r), Some(cs)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PEnum(crate::proof_kernel::aver_generated::kernel::proof::Proof__PEnum(v, crate::proof_kernel::aver_generated::kernel::decode::term(l)?, crate::proof_kernel::aver_generated::kernel::decode::term(r)?, crate::proof_kernel::aver_generated::kernel::decode::proofs(&cs)?)))
                                } else {
                                    Err(::aver_rt::str_lit!("malformed enum"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed enum"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed enum")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed enum"))
        }
    }
}

/// (linear TERM BOOL (NAME…) (INT…) (cut INT (INT…))…); BOOL is true or false.
pub fn linearProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(g), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(__pat1), Some(__pat2)) =
                    (__list_subject.first(), __list_subject.tail())
                {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                            let __pat3 = __pat3.clone();
                            {
                                let __dispatch_subject = __pat3;
                                if &*__dispatch_subject == "true" {
                                    {
                                        let __list_subject = __pat2;
                                        if let (Some(hs), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if let (Some(ws), Some(cs)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PLinear(crate::proof_kernel::aver_generated::kernel::proof::Proof__PLinear(crate::proof_kernel::aver_generated::kernel::decode::term(g)?, true, { let __arg14 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(hs)?; crate::proof_kernel::aver_generated::kernel::decode::atoms(&__arg14) }?, { let __arg15 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ws)?; crate::proof_kernel::aver_generated::kernel::decode::ints(&__arg15) }?, crate::proof_kernel::aver_generated::kernel::decode::cuts(&cs)?)))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed linear"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed linear"))
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "false" {
                                        {
                                            let __list_subject = __pat2;
                                            if let (Some(hs), Some(__pat5)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                {
                                                    let __list_subject = __pat5;
                                                    if let (Some(ws), Some(cs)) = (
                                                        __list_subject.first(),
                                                        __list_subject.tail(),
                                                    ) {
                                                        Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PLinear(crate::proof_kernel::aver_generated::kernel::proof::Proof__PLinear(crate::proof_kernel::aver_generated::kernel::decode::term(g)?, false, { let __arg16 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(hs)?; crate::proof_kernel::aver_generated::kernel::decode::atoms(&__arg16) }?, { let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ws)?; crate::proof_kernel::aver_generated::kernel::decode::ints(&__arg17) }?, crate::proof_kernel::aver_generated::kernel::decode::cuts(&cs)?)))
                                                    } else {
                                                        Err(::aver_rt::str_lit!("malformed linear"))
                                                    }
                                                }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed linear"))
                                            }
                                        }
                                    } else {
                                        Err(::aver_rt::str_lit!("malformed linear"))
                                    }
                                }
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("malformed linear")),
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed linear"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed linear"))
        }
    }
}

/// Each (cut INT (INT…)): a divisor and weights.
#[inline(always)]
pub fn cuts(
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Cut>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    __pat4,
                                ) => {
                                    let __pat4 = __pat4.clone();
                                    match &*__pat4 {
                                        "cut" => {
                                            let __list_subject = __pat3;
                                            if let (Some(__pat5), Some(__pat6)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(d) => {
            let d = d.clone();
            { let __list_subject = __pat6; if let (Some(ws), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if __list_subject.is_empty() { Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Cut { divisor: ({ let __s = &(d); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver()?, weights: { let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ws)?; crate::proof_kernel::aver_generated::kernel::decode::ints(&__arg12) }? }, crate::proof_kernel::aver_generated::kernel::decode::cuts(&rest)?)) } else { Err(::aver_rt::str_lit!("malformed cut")) } } } else { Err(::aver_rt::str_lit!("malformed cut")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed cut"))
        }
    }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed cut"))
                                            }
                                        }
                                        _ => Err(::aver_rt::str_lit!("malformed cut")),
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed cut")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed cut"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed cut")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Integers written as atoms.
#[inline(always)]
pub fn ints(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverIntList, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverIntList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(a) => {
                        let a = a.clone();
                        Ok(aver_rt::AverIntList::prepend_owned(
                            ({
                                let __s = &(a);
                                __s.parse::<aver_rt::AverInt>()
                                    .map_err(|_| format!("Cannot parse '{}' as Int", __s))
                            })
                            .into_aver()?,
                            crate::proof_kernel::aver_generated::kernel::decode::ints(&rest)?,
                        ))
                    }
                    _ => Err(::aver_rt::str_lit!("expected an integer")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (ring TERM TERM)
pub fn ringProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(l), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(r), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = __pat1;
                        if __list_subject.is_empty() {
                            Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PRing(crate::proof_kernel::aver_generated::kernel::proof::Proof__PRing(crate::proof_kernel::aver_generated::kernel::decode::term(l)?, crate::proof_kernel::aver_generated::kernel::decode::term(r)?)))
                        } else {
                            Err(::aver_rt::str_lit!("malformed ring"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed ring"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed ring"))
        }
    }
}

/// (listinduct NAME TERM TERM [(carry NAME…)] PROOF (HEAD TAIL) (GENERAL…) ((IH (TERM…) (PROOF…))…) PROOF)
pub fn listInductProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
                    let v = v.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(l), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if let (Some(r), Some(rest)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __arg8 @ _ = crate::proof_kernel::aver_generated::kernel::decode::term(l)?;
                                        {
                                            let __arg9 @ _ = crate::proof_kernel::aver_generated::kernel::decode::term(r)?;
                                            {
                                                let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::decode::carried(&rest)?;
                                                crate::proof_kernel::aver_generated::kernel::decode::listInductWith(v, __arg8, __arg9, &__arg10)
                                            }
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed listinduct"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed listinduct"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed listinduct")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed listinduct"))
        }
    }
}

/// The list induction, once the carried names are split off its cases.
pub fn listInductWith(
    v @ _: AverStr,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    rest @ _: &(
        aver_rt::AverList<AverStr>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    ),
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (ns, __pat0) = rest;
        {
            let __list_subject = __pat0;
            if let (Some(n), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __list_subject = __pat1;
                    if let (Some(__pat2), Some(__pat3)) =
                        (__list_subject.first(), __list_subject.tail())
                    {
                        match __pat2 {
                            crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(
                                __pat4,
                            ) => {
                                let __list_subject = __pat4;
                                if let (Some(__pat5), Some(__pat6)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            let h = h.clone();
            { let __list_subject = __pat6; if let (Some(__pat7), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { match __pat7 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(t) => {
            let t = t.clone();
            { let __list_subject = __pat8; if __list_subject.is_empty() { { let __list_subject = __pat3; if let (Some(gs), Some(__pat9)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat9; if let (Some(hs), Some(__pat10)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat10; if let (Some(c), Some(__pat11)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat11; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PListInduct(crate::proof_kernel::aver_generated::kernel::proof::Proof__PListInduct(v, l, r, ns.clone(), crate::proof_kernel::aver_generated::kernel::decode::proof(n)?, h, t, { let __arg23 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(gs)?; crate::proof_kernel::aver_generated::kernel::decode::atoms(&__arg23) }?, { let __arg24 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(hs)?; crate::proof_kernel::aver_generated::kernel::decode::ihAts(&__arg24) }?, crate::proof_kernel::aver_generated::kernel::decode::proof(c)?))) } else { Err(::aver_rt::str_lit!("malformed listinduct")) } } } else { Err(::aver_rt::str_lit!("malformed listinduct")) } } } else { Err(::aver_rt::str_lit!("malformed listinduct")) } } } else { Err(::aver_rt::str_lit!("malformed listinduct")) } } } else { Err(::aver_rt::str_lit!("malformed listinduct")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed listinduct"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed listinduct")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed listinduct"))
        }
    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed listinduct"))
                                }
                            }
                            _ => Err(::aver_rt::str_lit!("malformed listinduct")),
                        }
                    } else {
                        Err(::aver_rt::str_lit!("malformed listinduct"))
                    }
                }
            } else {
                Err(::aver_rt::str_lit!("malformed listinduct"))
            }
        }
    }
}

/// (leninduct NAME TERM TERM [(carry NAME…)] (GENERAL…) (BOUND HYP LAW) PROOF)
pub fn lenInductProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat12), Some(__pat13)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat12 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
                    let v = v.clone();
                    {
                        let __list_subject = __pat13;
                        if let (Some(l), Some(__pat14)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat14;
                                if let (Some(r), Some(rest)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let (ns, __pat0) = crate::proof_kernel::aver_generated::kernel::decode::carried(&rest)?;
                                        {
                                            let __list_subject = __pat0;
                                            if let (Some(gs), Some(__pat1)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                {
                                                    let __list_subject = __pat1;
                                                    if let (Some(__pat2), Some(__pat3)) = (
                                                        __list_subject.first(),
                                                        __list_subject.tail(),
                                                    ) {
                                                        match __pat2 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat4) => {
            { let __list_subject = __pat4; if let (Some(__pat5), Some(__pat6)) = (__list_subject.first(), __list_subject.tail()) { match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            { let __list_subject = __pat6; if let (Some(__pat7), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { match __pat7 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(hn) => {
            let hn = hn.clone();
            { let __list_subject = __pat8; if let (Some(__pat9), Some(__pat10)) = (__list_subject.first(), __list_subject.tail()) { match __pat9 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(ih) => {
            let ih = ih.clone();
            { let __list_subject = __pat10; if __list_subject.is_empty() { { let __list_subject = __pat3; if let (Some(c), Some(__pat11)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat11; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PLenInduct(crate::proof_kernel::aver_generated::kernel::proof::Proof__PLenInduct(v, crate::proof_kernel::aver_generated::kernel::decode::term(l)?, crate::proof_kernel::aver_generated::kernel::decode::term(r)?, ns, { let __arg26 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(gs)?; crate::proof_kernel::aver_generated::kernel::decode::atoms(&__arg26) }?, n, hn, ih, crate::proof_kernel::aver_generated::kernel::decode::proof(c)?))) } else { Err(::aver_rt::str_lit!("malformed leninduct")) } } } else { Err(::aver_rt::str_lit!("malformed leninduct")) } } } else { Err(::aver_rt::str_lit!("malformed leninduct")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed leninduct"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed leninduct")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed leninduct"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed leninduct")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed leninduct"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed leninduct")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed leninduct"))
        }
    }
                                                    } else {
                                                        Err(::aver_rt::str_lit!(
                                                            "malformed leninduct"
                                                        ))
                                                    }
                                                }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed leninduct"))
                                            }
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed leninduct"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed leninduct"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed leninduct")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed leninduct"))
        }
    }
}

/// (listcases NAME TERM TERM PROOF (HEAD TAIL) PROOF)
pub fn listCasesProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
                    let v = v.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(l), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if let (Some(r), Some(__pat3)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(n), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if let (Some(__pat5), Some(__pat6)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat7) => {
            { let __list_subject = __pat7; if let (Some(__pat8), Some(__pat9)) = (__list_subject.first(), __list_subject.tail()) { match __pat8 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            let h = h.clone();
            { let __list_subject = __pat9; if let (Some(__pat10), Some(__pat11)) = (__list_subject.first(), __list_subject.tail()) { match __pat10 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(t) => {
            let t = t.clone();
            { let __list_subject = __pat11; if __list_subject.is_empty() { { let __list_subject = __pat6; if let (Some(c), Some(__pat12)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat12; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PListCases(crate::proof_kernel::aver_generated::kernel::proof::Proof__PListCases(v, crate::proof_kernel::aver_generated::kernel::decode::term(l)?, crate::proof_kernel::aver_generated::kernel::decode::term(r)?, crate::proof_kernel::aver_generated::kernel::decode::proof(n)?, h, t, crate::proof_kernel::aver_generated::kernel::decode::proof(c)?))) } else { Err(::aver_rt::str_lit!("malformed listcases")) } } } else { Err(::aver_rt::str_lit!("malformed listcases")) } } } else { Err(::aver_rt::str_lit!("malformed listcases")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed listcases"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed listcases")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed listcases"))
        }
    } } else { Err(::aver_rt::str_lit!("malformed listcases")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed listcases"))
        }
    }
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed listcases"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed listcases"))
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed listcases"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed listcases"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed listcases")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed listcases"))
        }
    }
}

/// (IH (TERM…) (PROOF…))…: a hypothesis, the values of the generalised givens, the proofs of the carried hypotheses there.
#[inline(always)]
pub fn ihAts(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::IhAt>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    h,
                                ) => {
                                    let h = h.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(at), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if let (Some(ps), Some(__pat5)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    {
                                                        let __list_subject = __pat5;
                                                        if __list_subject.is_empty() {
                                                            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::IhAt { name: h, at: crate::proof_kernel::aver_generated::kernel::decode::termList(at)?, carry: { let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ps)?; crate::proof_kernel::aver_generated::kernel::decode::proofs(&__arg11) }? }, crate::proof_kernel::aver_generated::kernel::decode::ihAts(&rest)?))
                                                        } else {
                                                            Err(::aver_rt::str_lit!(
                                                                "malformed induction hypothesis"
                                                            ))
                                                        }
                                                    }
                                                } else {
                                                    Err(::aver_rt::str_lit!(
                                                        "malformed induction hypothesis"
                                                    ))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!(
                                                "malformed induction hypothesis"
                                            ))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed induction hypothesis")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed induction hypothesis"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed induction hypothesis")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (induct FN (TERM…) TERM TERM [(carry NAME…)] (case (NAME…) (IH…) PROOF)…)
pub fn inductProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(__pat0), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
            match __pat0 {
                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(f) => {
                    let f = f.clone();
                    {
                        let __list_subject = __pat1;
                        if let (Some(xs), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if let (Some(l), Some(__pat3)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(r), Some(rest)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::decode::termList(xs)?;
                                                {
                                                    let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::decode::term(l)?;
                                                    {
                                                        let __arg12 @ _ = crate::proof_kernel::aver_generated::kernel::decode::term(r)?;
                                                        {
                                                            let __arg13 @ _ = crate::proof_kernel::aver_generated::kernel::decode::carried(&rest)?;
                                                            crate::proof_kernel::aver_generated::kernel::decode::inductWith(f, &__arg10, __arg11, __arg12, &__arg13)
                                                        }
                                                    }
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed induct"))
                                        }
                                    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed induct"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed induct"))
                        }
                    }
                }
                _ => Err(::aver_rt::str_lit!("malformed induct")),
            }
        } else {
            Err(::aver_rt::str_lit!("malformed induct"))
        }
    }
}

/// The induction step, once the carried names are split off its cases.
pub fn inductWith(
    f @ _: AverStr,
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    rest @ _: &(
        aver_rt::AverList<AverStr>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    ),
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (ns, cs) = rest;
        Ok(
            crate::proof_kernel::aver_generated::kernel::proof::Proof::PInduct(
                crate::proof_kernel::aver_generated::kernel::proof::Proof__PInduct(
                    f,
                    xs.clone(),
                    l,
                    r,
                    ns.clone(),
                    crate::proof_kernel::aver_generated::kernel::decode::cases(cs)?,
                ),
            ),
        )
    }
}

/// The names after carry, when the first form is (carry NAME…), and the forms after it.
#[inline(always)]
pub fn carried(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<
    (
        aver_rt::AverList<AverStr>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    ),
    AverStr,
> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok((aver_rt::AverList::empty(), aver_rt::AverList::empty()))
        } else {
            if let Some((s, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                match s {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
                        let __list_subject = __pat0;
                        if let (Some(__pat1), Some(ns)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat1 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    __pat2,
                                ) => {
                                    let __pat2 = __pat2.clone();
                                    match &*__pat2 {
        "carry" => {
            Ok((crate::proof_kernel::aver_generated::kernel::decode::atoms(&ns)?, rest))
        },
        _ => {
            Ok((aver_rt::AverList::empty(), ss.clone()))
        }
    }
                                }
                                _ => Ok((aver_rt::AverList::empty(), ss.clone())),
                            }
                        } else {
                            Ok((aver_rt::AverList::empty(), ss.clone()))
                        }
                    }
                    _ => Ok((aver_rt::AverList::empty(), ss.clone())),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (case (NAME…) (IH…) ((K IH (TERM…) (PROOF…))…) PROOF)…, each IH a NAME or (NAME PROOF…) with the proofs of the carried hypotheses; each (K …) a hypothesis at the part recursive call K recurses on and other values of the varied givens.
#[inline(always)]
pub fn cases(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Case>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    __pat4,
                                ) => {
                                    let __pat4 = __pat4.clone();
                                    match &*__pat4 {
                                        "case" => {
                                            let __list_subject = __pat3;
                                            if let (Some(bs), Some(__pat5)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                {
                                                    let __list_subject = __pat5;
                                                    if let (Some(hs), Some(__pat6)) = (
                                                        __list_subject.first(),
                                                        __list_subject.tail(),
                                                    ) {
                                                        {
                                                            let __list_subject = __pat6;
                                                            if let (Some(ms), Some(__pat7)) = (
                                                                __list_subject.first(),
                                                                __list_subject.tail(),
                                                            ) {
                                                                {
                                                                    let __list_subject = __pat7;
                                                                    if let (Some(p), Some(__pat8)) = (
                                                                        __list_subject.first(),
                                                                        __list_subject.tail(),
                                                                    ) {
                                                                        {
                                                                            let __list_subject =
                                                                                __pat8;
                                                                            if __list_subject
                                                                                .is_empty()
                                                                            {
                                                                                Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Case { binders: { let __arg15 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(bs)?; crate::proof_kernel::aver_generated::kernel::decode::atoms(&__arg15) }?, ihs: { let __arg16 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(hs)?; crate::proof_kernel::aver_generated::kernel::decode::ihNames(&__arg16) }?, carry: { let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(hs)?; crate::proof_kernel::aver_generated::kernel::decode::ihCarry(&__arg17) }?, more: { let __arg18 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ms)?; crate::proof_kernel::aver_generated::kernel::decode::mores(&__arg18) }?, proof: crate::proof_kernel::aver_generated::kernel::decode::proof(p)? }, crate::proof_kernel::aver_generated::kernel::decode::cases(&rest)?))
                                                                            } else {
                                                                                Err(
                                                                                    ::aver_rt::str_lit!(
                                                                                        "malformed case"
                                                                                    ),
                                                                                )
                                                                            }
                                                                        }
                                                                    } else {
                                                                        Err(::aver_rt::str_lit!(
                                                                            "malformed case"
                                                                        ))
                                                                    }
                                                                }
                                                            } else {
                                                                Err(::aver_rt::str_lit!(
                                                                    "malformed case"
                                                                ))
                                                            }
                                                        }
                                                    } else {
                                                        Err(::aver_rt::str_lit!("malformed case"))
                                                    }
                                                }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed case"))
                                            }
                                        }
                                        _ => Err(::aver_rt::str_lit!("malformed case")),
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed case")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed case"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed case")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (K IH (TERM…) (PROOF…))…
#[inline(always)]
pub fn mores(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::More>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    k,
                                ) => {
                                    let k = k.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(__pat4), Some(__pat5)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
            let h = h.clone();
            { let __list_subject = __pat5; if let (Some(at), Some(__pat6)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat6; if let (Some(ps), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if __list_subject.is_empty() { match ({ let __s = &(k); __s.parse::<aver_rt::AverInt>().map_err(|_| format!("Cannot parse '{}' as Int", __s)) }).into_aver() { Ok(n @ _) => { Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::More { call: n, ih: crate::proof_kernel::aver_generated::kernel::proof::IhAt { name: h, at: crate::proof_kernel::aver_generated::kernel::decode::termList(at)?, carry: { let __arg15 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ps)?; crate::proof_kernel::aver_generated::kernel::decode::proofs(&__arg15) }? } }, crate::proof_kernel::aver_generated::kernel::decode::mores(&rest)?)) }, Err(_) => { Err(::aver_rt::str_lit!("malformed hypothesis")) } } } else { Err(::aver_rt::str_lit!("malformed hypothesis")) } } } else { Err(::aver_rt::str_lit!("malformed hypothesis")) } } } else { Err(::aver_rt::str_lit!("malformed hypothesis")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed hypothesis"))
        }
    }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed hypothesis"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed hypothesis"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The name of each induction hypothesis.
#[inline(always)]
pub fn ihNames(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<AverStr>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::ihName(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::ihNames(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// NAME or (NAME PROOF…).
pub fn ihName(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            Ok(n)
        }
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(ps)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
                        let n = n.clone();
                        Ok(n)
                    }
                    _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed hypothesis"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
    }
}

/// The proofs of the carried hypotheses for each induction hypothesis.
#[inline(always)]
pub fn ihCarry(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<
    aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>>,
    AverStr,
> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::ihProofs(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::ihCarry(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// None for a bare NAME; the proofs after the name otherwise.
pub fn ihProofs(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Proof>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            Ok(aver_rt::AverList::empty())
        }
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(ps)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
                        let n = n.clone();
                        crate::proof_kernel::aver_generated::kernel::decode::proofs(&ps)
                    }
                    _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed hypothesis"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed hypothesis")),
    }
}

/// (absurd PROOF TERM TERM)
pub fn absurdProof(
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Proof, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(p), Some(__pat0)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = __pat0;
                if let (Some(l), Some(__pat1)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = __pat1;
                        if let (Some(r), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_kernel::aver_generated::kernel::proof::Proof::PAbsurd(crate::proof_kernel::aver_generated::kernel::proof::Proof__PAbsurd(crate::proof_kernel::aver_generated::kernel::decode::proof(p)?, crate::proof_kernel::aver_generated::kernel::decode::term(l)?, crate::proof_kernel::aver_generated::kernel::decode::term(r)?)))
                                } else {
                                    Err(::aver_rt::str_lit!("malformed absurd"))
                                }
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed absurd"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("malformed absurd"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("malformed absurd"))
        }
    }
}

/// A type: int, bool, str, float, unit, (list T), (option T), (result T E), (map K V), (vector T), (tuple T…), (named NAME), a type variable (var NAME) or (var NAME eq) whose == is equality (in the givens of a law only), or _: a type the data cannot state, read as one unknown type (shown as ?) that fits only itself.
pub fn typ(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
    vars @ _: bool,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat0) => {
            let __pat0 = __pat0.clone();
            {
                let __dispatch_subject = __pat0;
                if &*__dispatch_subject == "int" {
                    Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyInt)
                } else {
                    if &*__dispatch_subject == "bool" {
                        Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyBool)
                    } else {
                        if &*__dispatch_subject == "str" {
                            Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyStr)
                        } else {
                            if &*__dispatch_subject == "float" {
                                Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyFloat)
                            } else {
                                if &*__dispatch_subject == "unit" {
                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyUnit)
                                } else {
                                    if &*__dispatch_subject == "_" {
                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyRigid(crate::proof_kernel::aver_generated::kernel::term::Ty__TyRigid(::aver_rt::str_lit!(""), false)))
                                    } else {
                                        Err(::aver_rt::str_lit!("malformed type"))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            let __list_subject = __pat1;
            if let (Some(__pat2), Some(ts)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat2 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        {
                            let __dispatch_subject = __pat3;
                            if &*__dispatch_subject == "var" {
                                {
                                    let __list_subject = ts;
                                    if let (Some(__pat4), Some(__pat5)) =
                                        (__list_subject.first(), __list_subject.tail())
                                    {
                                        match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            aver_list_match!(__pat5, [] => crate::proof_kernel::aver_generated::kernel::decode::typeVar(n, false, vars), [__pat6, __pat7] => { match __pat6 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat8) => {
            match &*__pat8 {
        "eq" => {
            { let __list_subject = __pat7; if __list_subject.is_empty() { crate::proof_kernel::aver_generated::kernel::decode::typeVar(n, true, vars) } else { Err(::aver_rt::str_lit!("malformed type")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed type"))
        }
    }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed type"))
        }
    } })
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed type"))
        }
    }
                                    } else {
                                        Err(::aver_rt::str_lit!("malformed type"))
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "list" {
                                    {
                                        let __list_subject = ts;
                                        if let (Some(t), Some(__pat9)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat9;
                                                if __list_subject.is_empty() {
                                                    Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyList(crate::proof_kernel::aver_generated::kernel::term::Ty__TyList(crate::proof_kernel::aver_generated::kernel::decode::typ(t, vars)?)))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed type"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed type"))
                                        }
                                    }
                                } else {
                                    if &*__dispatch_subject == "option" {
                                        {
                                            let __list_subject = ts;
                                            if let (Some(t), Some(__pat10)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                {
                                                    let __list_subject = __pat10;
                                                    if __list_subject.is_empty() {
                                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyOption(crate::proof_kernel::aver_generated::kernel::term::Ty__TyOption(crate::proof_kernel::aver_generated::kernel::decode::typ(t, vars)?)))
                                                    } else {
                                                        Err(::aver_rt::str_lit!("malformed type"))
                                                    }
                                                }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed type"))
                                            }
                                        }
                                    } else {
                                        if &*__dispatch_subject == "vector" {
                                            {
                                                let __list_subject = ts;
                                                if let (Some(t), Some(__pat11)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    {
                                                        let __list_subject = __pat11;
                                                        if __list_subject.is_empty() {
                                                            Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyVector(crate::proof_kernel::aver_generated::kernel::term::Ty__TyVector(crate::proof_kernel::aver_generated::kernel::decode::typ(t, vars)?)))
                                                        } else {
                                                            Err(::aver_rt::str_lit!(
                                                                "malformed type"
                                                            ))
                                                        }
                                                    }
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed type"))
                                                }
                                            }
                                        } else {
                                            if &*__dispatch_subject == "result" {
                                                {
                                                    let __list_subject = ts;
                                                    if let (Some(t), Some(__pat12)) = (
                                                        __list_subject.first(),
                                                        __list_subject.tail(),
                                                    ) {
                                                        {
                                                            let __list_subject = __pat12;
                                                            if let (Some(e), Some(__pat13)) = (
                                                                __list_subject.first(),
                                                                __list_subject.tail(),
                                                            ) {
                                                                {
                                                                    let __list_subject = __pat13;
                                                                    if __list_subject.is_empty() {
                                                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyResult(crate::proof_kernel::aver_generated::kernel::term::Ty__TyResult(crate::proof_kernel::aver_generated::kernel::decode::typ(t, vars)?, crate::proof_kernel::aver_generated::kernel::decode::typ(e, vars)?)))
                                                                    } else {
                                                                        Err(::aver_rt::str_lit!(
                                                                            "malformed type"
                                                                        ))
                                                                    }
                                                                }
                                                            } else {
                                                                Err(::aver_rt::str_lit!(
                                                                    "malformed type"
                                                                ))
                                                            }
                                                        }
                                                    } else {
                                                        Err(::aver_rt::str_lit!("malformed type"))
                                                    }
                                                }
                                            } else {
                                                if &*__dispatch_subject == "map" {
                                                    {
                                                        let __list_subject = ts;
                                                        if let (Some(k), Some(__pat14)) = (
                                                            __list_subject.first(),
                                                            __list_subject.tail(),
                                                        ) {
                                                            {
                                                                let __list_subject = __pat14;
                                                                if let (Some(v), Some(__pat15)) = (
                                                                    __list_subject.first(),
                                                                    __list_subject.tail(),
                                                                ) {
                                                                    {
                                                                        let __list_subject =
                                                                            __pat15;
                                                                        if __list_subject.is_empty()
                                                                        {
                                                                            Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyMap(crate::proof_kernel::aver_generated::kernel::term::Ty__TyMap(crate::proof_kernel::aver_generated::kernel::decode::typ(k, vars)?, crate::proof_kernel::aver_generated::kernel::decode::typ(v, vars)?)))
                                                                        } else {
                                                                            Err(
                                                                                ::aver_rt::str_lit!(
                                                                                    "malformed type"
                                                                                ),
                                                                            )
                                                                        }
                                                                    }
                                                                } else {
                                                                    Err(::aver_rt::str_lit!(
                                                                        "malformed type"
                                                                    ))
                                                                }
                                                            }
                                                        } else {
                                                            Err(::aver_rt::str_lit!(
                                                                "malformed type"
                                                            ))
                                                        }
                                                    }
                                                } else {
                                                    if &*__dispatch_subject == "tuple" {
                                                        Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyTuple(crate::proof_kernel::aver_generated::kernel::term::Ty__TyTuple(crate::proof_kernel::aver_generated::kernel::decode::tys(&ts, vars)?)))
                                                    } else {
                                                        if &*__dispatch_subject == "named" {
                                                            {
                                                                let __list_subject = ts;
                                                                if let (
                                                                    Some(__pat16),
                                                                    Some(__pat17),
                                                                ) = (
                                                                    __list_subject.first(),
                                                                    __list_subject.tail(),
                                                                ) {
                                                                    match __pat16 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            { let __list_subject = __pat17; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::term::Ty::TyNamed(crate::proof_kernel::aver_generated::kernel::term::Ty__TyNamed(n))) } else { Err(::aver_rt::str_lit!("malformed type")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed type"))
        }
    }
                                                                } else {
                                                                    Err(::aver_rt::str_lit!(
                                                                        "malformed type"
                                                                    ))
                                                                }
                                                            }
                                                        } else {
                                                            Err(::aver_rt::str_lit!(
                                                                "malformed type"
                                                            ))
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
                    _ => Err(::aver_rt::str_lit!("malformed type")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed type"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed type")),
    }
}

/// A type variable, where one may stand: in the givens of a law, never in a definition, a signature or a declared type.
#[inline(always)]
pub fn typeVar(
    n @ _: AverStr,
    eq @ _: bool,
    vars @ _: bool,
) -> Result<crate::proof_kernel::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if vars {
        Ok(
            crate::proof_kernel::aver_generated::kernel::term::Ty::TyVar(
                crate::proof_kernel::aver_generated::kernel::term::Ty__TyVar(n, eq),
            ),
        )
    } else {
        Err(::aver_rt::str_lit!(
            "a type variable outside a law's givens"
        ))
    }
}

/// Several types.
#[inline(always)]
pub fn tys(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    vars @ _: bool,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Ty>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::typ(s, vars)?,
                    crate::proof_kernel::aver_generated::kernel::decode::tys(&rest, vars)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (NAME TYPE)…: names with their types.
#[inline(always)]
pub fn typedVars(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
    vars @ _: bool,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    n,
                                ) => {
                                    let n = n.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(t), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if __list_subject.is_empty() {
                                                    Ok(aver_rt::AverList::prepend_owned({ let __arg10 @ _ = crate::proof_kernel::aver_generated::kernel::decode::typ(t, vars)?; crate::proof_kernel::aver_generated::kernel::term::typed(n, __arg10) }, crate::proof_kernel::aver_generated::kernel::decode::typedVars(&rest, vars)?))
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed typed name"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed typed name"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed typed name")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed typed name"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed typed name")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (none) or a term.
pub fn premise(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        match &*__pat3 {
                            "none" => {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(aver_rt::AverList::empty())
                                } else {
                                    Ok(aver_rt::AverList::prepend_owned(
                                        crate::proof_kernel::aver_generated::kernel::decode::term(
                                            s,
                                        )?,
                                        aver_rt::AverList::empty(),
                                    ))
                                }
                            }
                            _ => Ok(aver_rt::AverList::prepend_owned(
                                crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                                aver_rt::AverList::empty(),
                            )),
                        }
                    }
                    _ => Ok(aver_rt::AverList::prepend_owned(
                        crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                        aver_rt::AverList::empty(),
                    )),
                }
            } else {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
                    aver_rt::AverList::empty(),
                ))
            }
        }
        _ => Ok(aver_rt::AverList::prepend_owned(
            crate::proof_kernel::aver_generated::kernel::decode::term(s)?,
            aver_rt::AverList::empty(),
        )),
    }
}

/// (law KEY ((NAME TYPE)…) PREMISE TERM TERM), or the same headed obligation or fact.
pub fn law(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
    head @ _: AverStr,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Law, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(h) => {
                        let h = h.clone();
                        {
                            let __list_subject = __pat2;
                            if let (Some(__pat3), Some(__pat4)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                match __pat3 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(k) => {
            let k = k.clone();
            { let __list_subject = __pat4; if let (Some(gs), Some(__pat5)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat5; if let (Some(p), Some(__pat6)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat6; if let (Some(l), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if let (Some(r), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat8; if __list_subject.is_empty() { { let __arg18 @ _ = (h == head); { let __arg19 @ _ = { let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(gs)?; crate::proof_kernel::aver_generated::kernel::decode::typedVars(&__arg17, true) }?; { let __arg20 @ _ = crate::proof_kernel::aver_generated::kernel::decode::premise(p)?; { let __arg21 @ _ = crate::proof_kernel::aver_generated::kernel::decode::term(l)?; { let __arg22 @ _ = crate::proof_kernel::aver_generated::kernel::decode::term(r)?; crate::proof_kernel::aver_generated::kernel::decode::lawOf(__arg18, k, &__arg19, &__arg20, __arg21, __arg22) } } } } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("malformed ")); __b }; __b.push_display(&(head)); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("malformed ")); __b }; __b.push_display(&(head)); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("malformed ")); __b }; __b.push_display(&(head)); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("malformed ")); __b }; __b.push_display(&(head)); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("malformed ")); __b }; __b.push_display(&(head)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("malformed ")); __b }; __b.push_display(&(head)); __b }))
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
                                        __b.push_str(&::aver_rt::str_lit!("malformed "));
                                        __b
                                    };
                                    __b.push_display(&(head));
                                    __b
                                }))
                            }
                        }
                    }
                    _ => Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("malformed "));
                            __b
                        };
                        __b.push_display(&(head));
                        __b
                    })),
                }
            } else {
                Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("malformed "));
                        __b
                    };
                    __b.push_display(&(head));
                    __b
                }))
            }
        }
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(26)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("malformed "));
                __b
            };
            __b.push_display(&(head));
            __b
        })),
    }
}

/// A law once its head was checked.
#[inline(always)]
pub fn lawOf(
    expected @ _: bool,
    k @ _: AverStr,
    gs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::TypedVar>,
    p @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    mut l @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
    mut r @ _: crate::proof_kernel::aver_generated::kernel::term::Term,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Law, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if expected {
        Ok(crate::proof_kernel::aver_generated::kernel::proof::Law {
            key: k,
            givens: gs.clone(),
            premise: p.clone(),
            lhs: l,
            rhs: r,
        })
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("unexpected head for "));
                __b
            };
            __b.push_display(&(k));
            __b
        }))
    }
}

/// Cited laws, and cited builtin facts with their proofs.
#[inline(always)]
pub fn laws(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<Cited, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited {
                laws: aver_rt::AverList::empty(),
                facts: aver_rt::AverList::empty(),
            })
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __arg3 @ _ =
                        crate::proof_kernel::aver_generated::kernel::decode::laws(&rest)?;
                    crate::proof_kernel::aver_generated::kernel::decode::withCited(s, &__arg3)
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// One cited law or fact in front of the rest; (fact KEY ((NAME TYPE)…) PREMISE TERM TERM PROOF).
pub fn withCited(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
    rest @ _: &Cited,
) -> Result<Cited, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        match &*__pat3 {
                            "fact" => {
                                let __list_subject = __pat2;
                                if let (Some(__pat4), Some(__pat5)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(k) => {
            let k = k.clone();
            { let __list_subject = __pat5; if let (Some(gs), Some(__pat6)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat6; if let (Some(w), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if let (Some(l), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat8; if let (Some(r), Some(__pat9)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat9; if let (Some(p), Some(__pat10)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat10; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: rest.laws.clone(), facts: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::proof::Fact { law: crate::proof_kernel::aver_generated::kernel::proof::Law { key: k, givens: { let __arg19 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(gs)?; crate::proof_kernel::aver_generated::kernel::decode::typedVars(&__arg19, true) }?, premise: crate::proof_kernel::aver_generated::kernel::decode::premise(w)?, lhs: crate::proof_kernel::aver_generated::kernel::decode::term(l)?, rhs: crate::proof_kernel::aver_generated::kernel::decode::term(r)? }, proof: crate::proof_kernel::aver_generated::kernel::decode::proof(p)? }, &rest.facts) }) } else { Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() }) } } } else { Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() }) } } } else { Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() }) } } } else { Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() }) } } } else { Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() }) } } } else { Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() }) } }
        },
        _ => {
            Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() })
        }
    }
                                } else {
                                    Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited { laws: aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::decode::law(s, ::aver_rt::str_lit!("law"))?, &rest.laws), facts: rest.facts.clone() })
                                }
                            }
                            _ => Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited {
                                laws: aver_rt::AverList::prepend(
                                    crate::proof_kernel::aver_generated::kernel::decode::law(
                                        s,
                                        ::aver_rt::str_lit!("law"),
                                    )?,
                                    &rest.laws,
                                ),
                                facts: rest.facts.clone(),
                            }),
                        }
                    }
                    _ => Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited {
                        laws: aver_rt::AverList::prepend(
                            crate::proof_kernel::aver_generated::kernel::decode::law(
                                s,
                                ::aver_rt::str_lit!("law"),
                            )?,
                            &rest.laws,
                        ),
                        facts: rest.facts.clone(),
                    }),
                }
            } else {
                Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited {
                    laws: aver_rt::AverList::prepend(
                        crate::proof_kernel::aver_generated::kernel::decode::law(
                            s,
                            ::aver_rt::str_lit!("law"),
                        )?,
                        &rest.laws,
                    ),
                    facts: rest.facts.clone(),
                })
            }
        }
        _ => Ok(crate::proof_kernel::aver_generated::kernel::decode::Cited {
            laws: aver_rt::AverList::prepend(
                crate::proof_kernel::aver_generated::kernel::decode::law(
                    s,
                    ::aver_rt::str_lit!("law"),
                )?,
                &rest.laws,
            ),
            facts: rest.facts.clone(),
        }),
    }
}

/// (def NAME ((PARAM TYPE)…) TYPE ((NAME TERM)…) TERM): typed parameters, the result type, local bindings in order, the final expression.
pub fn def(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Def, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        match &*__pat3 {
                            "def" => {
                                let __list_subject = __pat2;
                                if let (Some(__pat4), Some(__pat5)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            { let __list_subject = __pat5; if let (Some(ps), Some(__pat6)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat6; if let (Some(rt), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if let (Some(ls), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat8; if let (Some(b), Some(__pat9)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat9; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Def { name: n, params: { let __arg16 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ps)?; crate::proof_kernel::aver_generated::kernel::decode::typedVars(&__arg16, false) }?, ret: crate::proof_kernel::aver_generated::kernel::decode::typ(rt, false)?, lets: crate::proof_kernel::aver_generated::kernel::decode::bindings(ls)?, body: crate::proof_kernel::aver_generated::kernel::decode::term(b)? }) } else { Err(::aver_rt::str_lit!("malformed def")) } } } else { Err(::aver_rt::str_lit!("malformed def")) } } } else { Err(::aver_rt::str_lit!("malformed def")) } } } else { Err(::aver_rt::str_lit!("malformed def")) } } } else { Err(::aver_rt::str_lit!("malformed def")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed def"))
        }
    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed def"))
                                }
                            }
                            _ => Err(::aver_rt::str_lit!("malformed def")),
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed def")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed def"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed def")),
    }
}

/// Definitions.
#[inline(always)]
pub fn defs(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::def(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::defs(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (const NAME TYPE TERM)
pub fn constant(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Const, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        match &*__pat3 {
                            "const" => {
                                let __list_subject = __pat2;
                                if let (Some(__pat4), Some(__pat5)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            { let __list_subject = __pat5; if let (Some(t), Some(__pat6)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat6; if let (Some(v), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat7; if __list_subject.is_empty() { Ok(crate::proof_kernel::aver_generated::kernel::proof::Const { name: n, ty: crate::proof_kernel::aver_generated::kernel::decode::typ(t, false)?, value: crate::proof_kernel::aver_generated::kernel::decode::term(v)? }) } else { Err(::aver_rt::str_lit!("malformed const")) } } } else { Err(::aver_rt::str_lit!("malformed const")) } } } else { Err(::aver_rt::str_lit!("malformed const")) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed const"))
        }
    }
                                } else {
                                    Err(::aver_rt::str_lit!("malformed const"))
                                }
                            }
                            _ => Err(::aver_rt::str_lit!("malformed const")),
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed const")),
                }
            } else {
                Err(::aver_rt::str_lit!("malformed const"))
            }
        }
        _ => Err(::aver_rt::str_lit!("malformed const")),
    }
}

/// Module-level bindings.
#[inline(always)]
pub fn consts(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_kernel::aver_generated::kernel::decode::constant(s)?,
                    crate::proof_kernel::aver_generated::kernel::decode::consts(&rest)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (sum NAME (CTOR TYPE…)…)…: the sum types the program declares, each constructor in order with the types of its fields; `eq` after the name where == between two values of the type is equality.
#[inline(always)]
pub fn sums(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Sum>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    __pat4,
                                ) => {
                                    let __pat4 = __pat4.clone();
                                    match &*__pat4 {
                                        "sum" => {
                                            let __list_subject = __pat3;
                                            if let (Some(__pat5), Some(__pat6)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            { let __list_subject = __pat6.clone(); if let (Some(__pat7), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { match __pat7 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat9) => {
            let __pat9 = __pat9.clone();
            match &*__pat9 {
        "eq" => {
            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Sum { name: n, eq: true, variants: crate::proof_kernel::aver_generated::kernel::decode::variants(&__pat8)? }, crate::proof_kernel::aver_generated::kernel::decode::sums(&rest)?))
        },
        _ => {
            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Sum { name: n, eq: false, variants: crate::proof_kernel::aver_generated::kernel::decode::variants(&__pat6)? }, crate::proof_kernel::aver_generated::kernel::decode::sums(&rest)?))
        }
    }
        },
        _ => {
            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Sum { name: n, eq: false, variants: crate::proof_kernel::aver_generated::kernel::decode::variants(&__pat6)? }, crate::proof_kernel::aver_generated::kernel::decode::sums(&rest)?))
        }
    } } else { Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Sum { name: n, eq: false, variants: crate::proof_kernel::aver_generated::kernel::decode::variants(&__pat6)? }, crate::proof_kernel::aver_generated::kernel::decode::sums(&rest)?)) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed sum"))
        }
    }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed sum"))
                                            }
                                        }
                                        _ => Err(::aver_rt::str_lit!("malformed sum")),
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed sum")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed sum"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed sum")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (CTOR TYPE…)…
#[inline(always)]
pub fn variants(
    vs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Variant>, AverStr>
{
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = vs;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(ts)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    c,
                                ) => {
                                    let c = c.clone();
                                    Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Variant { name: c, fields: crate::proof_kernel::aver_generated::kernel::decode::tys(&ts, false)? }, crate::proof_kernel::aver_generated::kernel::decode::variants(&rest)?))
                                }
                                _ => Err(::aver_rt::str_lit!("malformed constructor of a sum")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed constructor of a sum"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed constructor of a sum")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (rec NAME (FIELD TYPE)…)…: the record types the program declares; `eq` after the name where == between two values of the type is equality.
#[inline(always)]
pub fn recs(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Rec>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    __pat4,
                                ) => {
                                    let __pat4 = __pat4.clone();
                                    match &*__pat4 {
                                        "rec" => {
                                            let __list_subject = __pat3;
                                            if let (Some(__pat5), Some(__pat6)) =
                                                (__list_subject.first(), __list_subject.tail())
                                            {
                                                match __pat5 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            let n = n.clone();
            { let __list_subject = __pat6.clone(); if let (Some(__pat7), Some(__pat8)) = (__list_subject.first(), __list_subject.tail()) { match __pat7 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat9) => {
            let __pat9 = __pat9.clone();
            match &*__pat9 {
        "eq" => {
            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Rec { name: n, eq: true, fields: crate::proof_kernel::aver_generated::kernel::decode::typedVars(&__pat8, false)? }, crate::proof_kernel::aver_generated::kernel::decode::recs(&rest)?))
        },
        _ => {
            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Rec { name: n, eq: false, fields: crate::proof_kernel::aver_generated::kernel::decode::typedVars(&__pat6, false)? }, crate::proof_kernel::aver_generated::kernel::decode::recs(&rest)?))
        }
    }
        },
        _ => {
            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Rec { name: n, eq: false, fields: crate::proof_kernel::aver_generated::kernel::decode::typedVars(&__pat6, false)? }, crate::proof_kernel::aver_generated::kernel::decode::recs(&rest)?))
        }
    } } else { Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::Rec { name: n, eq: false, fields: crate::proof_kernel::aver_generated::kernel::decode::typedVars(&__pat6, false)? }, crate::proof_kernel::aver_generated::kernel::decode::recs(&rest)?)) } }
        },
        _ => {
            Err(::aver_rt::str_lit!("malformed rec"))
        }
    }
                                            } else {
                                                Err(::aver_rt::str_lit!("malformed rec"))
                                            }
                                        }
                                        _ => Err(::aver_rt::str_lit!("malformed rec")),
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed rec")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed rec"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed rec")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (NAME (TYPE…) TYPE)…: the signature of every function the script calls without opening it.
#[inline(always)]
pub fn sigs(
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::sexp::Sexp>,
) -> Result<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::FnSig>, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = ss;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(__pat0), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat0 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
                        let __list_subject = __pat1;
                        if let (Some(__pat2), Some(__pat3)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            match __pat2 {
                                crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(
                                    n,
                                ) => {
                                    let n = n.clone();
                                    {
                                        let __list_subject = __pat3;
                                        if let (Some(ps), Some(__pat4)) =
                                            (__list_subject.first(), __list_subject.tail())
                                        {
                                            {
                                                let __list_subject = __pat4;
                                                if let (Some(r), Some(__pat5)) =
                                                    (__list_subject.first(), __list_subject.tail())
                                                {
                                                    {
                                                        let __list_subject = __pat5;
                                                        if __list_subject.is_empty() {
                                                            Ok(aver_rt::AverList::prepend_owned(crate::proof_kernel::aver_generated::kernel::proof::FnSig { name: n, params: { let __arg11 @ _ = crate::proof_kernel::aver_generated::kernel::decode::items(ps)?; crate::proof_kernel::aver_generated::kernel::decode::tys(&__arg11, false) }?, ret: crate::proof_kernel::aver_generated::kernel::decode::typ(r, false)? }, crate::proof_kernel::aver_generated::kernel::decode::sigs(&rest)?))
                                                        } else {
                                                            Err(::aver_rt::str_lit!(
                                                                "malformed signature"
                                                            ))
                                                        }
                                                    }
                                                } else {
                                                    Err(::aver_rt::str_lit!("malformed signature"))
                                                }
                                            }
                                        } else {
                                            Err(::aver_rt::str_lit!("malformed signature"))
                                        }
                                    }
                                }
                                _ => Err(::aver_rt::str_lit!("malformed signature")),
                            }
                        } else {
                            Err(::aver_rt::str_lit!("malformed signature"))
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("malformed signature")),
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// (steps 16 (obligation …) (defs …) (consts …) (sums …) (recs …) (sigs …) (laws …) (proof …)); version 16 only.
pub fn script(
    s @ _: &crate::proof_kernel::aver_generated::kernel::sexp::Sexp,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Script, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __list_subject = __pat0;
            if let (Some(__pat1), Some(__pat2)) = (__list_subject.first(), __list_subject.tail()) {
                match __pat1 {
                    crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                        let __pat3 = __pat3.clone();
                        match &*__pat3 {
                            "steps" => {
                                let __list_subject = __pat2;
                                if let (Some(__pat4), Some(rest)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(v) => {
            let v = v.clone();
            match &*v.clone() {
        "16" => {
            { let __list_subject = rest; if let (Some(o), Some(__pat5)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat5; if let (Some(__pat6), Some(__pat7)) = (__list_subject.first(), __list_subject.tail()) { match __pat6 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat8) => {
            { let __list_subject = __pat8; if let (Some(__pat9), Some(ds)) = (__list_subject.first(), __list_subject.tail()) { match __pat9 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat10) => {
            let __pat10 = __pat10.clone();
            match &*__pat10 {
        "defs" => {
            { let __list_subject = __pat7; if let (Some(__pat11), Some(__pat12)) = (__list_subject.first(), __list_subject.tail()) { match __pat11 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat13) => {
            { let __list_subject = __pat13; if let (Some(__pat14), Some(cs)) = (__list_subject.first(), __list_subject.tail()) { match __pat14 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat15) => {
            let __pat15 = __pat15.clone();
            match &*__pat15 {
        "consts" => {
            { let __list_subject = __pat12; if let (Some(__pat16), Some(__pat17)) = (__list_subject.first(), __list_subject.tail()) { match __pat16 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat18) => {
            { let __list_subject = __pat18; if let (Some(__pat19), Some(ss)) = (__list_subject.first(), __list_subject.tail()) { match __pat19 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat20) => {
            let __pat20 = __pat20.clone();
            match &*__pat20 {
        "sums" => {
            { let __list_subject = __pat17; if let (Some(__pat21), Some(__pat22)) = (__list_subject.first(), __list_subject.tail()) { match __pat21 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat23) => {
            { let __list_subject = __pat23; if let (Some(__pat24), Some(rs)) = (__list_subject.first(), __list_subject.tail()) { match __pat24 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat25) => {
            let __pat25 = __pat25.clone();
            match &*__pat25 {
        "recs" => {
            { let __list_subject = __pat22; if let (Some(__pat26), Some(__pat27)) = (__list_subject.first(), __list_subject.tail()) { match __pat26 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat28) => {
            { let __list_subject = __pat28; if let (Some(__pat29), Some(gs)) = (__list_subject.first(), __list_subject.tail()) { match __pat29 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat30) => {
            let __pat30 = __pat30.clone();
            match &*__pat30 {
        "sigs" => {
            { let __list_subject = __pat27; if let (Some(__pat31), Some(__pat32)) = (__list_subject.first(), __list_subject.tail()) { match __pat31 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat33) => {
            { let __list_subject = __pat33; if let (Some(__pat34), Some(ls)) = (__list_subject.first(), __list_subject.tail()) { match __pat34 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat35) => {
            let __pat35 = __pat35.clone();
            match &*__pat35 {
        "laws" => {
            { let __list_subject = __pat32; if let (Some(__pat36), Some(__pat37)) = (__list_subject.first(), __list_subject.tail()) { match __pat36 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat38) => {
            { let __list_subject = __pat38; if let (Some(__pat39), Some(__pat40)) = (__list_subject.first(), __list_subject.tail()) { match __pat39 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat41) => {
            let __pat41 = __pat41.clone();
            match &*__pat41 {
        "proof" => {
            { let __list_subject = __pat40; if let (Some(p), Some(__pat42)) = (__list_subject.first(), __list_subject.tail()) { { let __list_subject = __pat42; if __list_subject.is_empty() { { let __list_subject = __pat37; if __list_subject.is_empty() { { let __arg54 @ _ = crate::proof_kernel::aver_generated::kernel::decode::law(o, ::aver_rt::str_lit!("obligation"))?; { let __arg55 @ _ = crate::proof_kernel::aver_generated::kernel::decode::defs(&ds)?; { let __arg56 @ _ = crate::proof_kernel::aver_generated::kernel::decode::consts(&cs)?; { let __arg57 @ _ = crate::proof_kernel::aver_generated::kernel::decode::sums(&ss)?; { let __arg58 @ _ = crate::proof_kernel::aver_generated::kernel::decode::recs(&rs)?; { let __arg59 @ _ = crate::proof_kernel::aver_generated::kernel::decode::sigs(&gs)?; { let __arg60 @ _ = crate::proof_kernel::aver_generated::kernel::decode::laws(&ls)?; { let __arg61 @ _ = crate::proof_kernel::aver_generated::kernel::decode::proof(p)?; crate::proof_kernel::aver_generated::kernel::decode::scriptOf(__arg54, &__arg55, &__arg56, &__arg57, &__arg58, &__arg59, &__arg60, __arg61) } } } } } } } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } } } else { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b })) } }
        },
        _ => {
            Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(48)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unsupported step format version ")); __b }; __b.push_display(&(v)); __b }))
        }
    }
        },
        _ => {
            Err(::aver_rt::str_lit!("not a step script"))
        }
    }
                                } else {
                                    Err(::aver_rt::str_lit!("not a step script"))
                                }
                            }
                            _ => Err(::aver_rt::str_lit!("not a step script")),
                        }
                    }
                    _ => Err(::aver_rt::str_lit!("not a step script")),
                }
            } else {
                Err(::aver_rt::str_lit!("not a step script"))
            }
        }
        _ => Err(::aver_rt::str_lit!("not a step script")),
    }
}

/// The script once its parts are read.
pub fn scriptOf(
    mut ob @ _: crate::proof_kernel::aver_generated::kernel::proof::Law,
    ds @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Def>,
    cs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Const>,
    ss @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Sum>,
    rs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::Rec>,
    gs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::proof::FnSig>,
    ls @ _: &Cited,
    mut p @ _: crate::proof_kernel::aver_generated::kernel::proof::Proof,
) -> Result<crate::proof_kernel::aver_generated::kernel::proof::Script, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    Ok(Box::new(
        crate::proof_kernel::aver_generated::kernel::proof::Script__Fields {
            obligation: ob,
            defs: ds.clone(),
            consts: cs.clone(),
            sums: ss.clone(),
            recs: rs.clone(),
            sigs: gs.clone(),
            laws: ls.laws.clone(),
            facts: ls.facts.clone(),
            proof: p,
        },
    ))
}
