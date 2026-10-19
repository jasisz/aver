#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Sexp {
    Atom(AverStr),
    Text(AverStr),
    Node(aver_rt::AverList<Sexp>),
}

impl Sexp {
    fn aver_key_rank(&self) -> usize {
        match self {
            Sexp::Atom(..) => 0,
            Sexp::Node(..) => 1,
            Sexp::Text(..) => 2,
        }
    }
}

impl PartialOrd for Sexp {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Sexp {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Sexp::Atom(a0), Sexp::Atom(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Sexp::Node(a0), Sexp::Node(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Sexp::Text(a0), Sexp::Text(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Sexp {
    fn aver_display(&self) -> String {
        match self {
            Sexp::Atom(f0) => format!("Atom({})", f0.aver_display_inner()),
            Sexp::Text(f0) => format!("Text({})", f0.aver_display_inner()),
            Sexp::Node(f0) => format!("Node({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[allow(non_camel_case_types)]
enum __MutualTco1 {
    ReadList(aver_rt::AverList<AverStr>, aver_rt::AverList<Sexp>),
    ReadListItem(aver_rt::AverList<AverStr>, aver_rt::AverList<Sexp>),
}

fn __mutual_tco_trampoline_1(
    mut __state: __MutualTco1,
) -> Result<(Sexp, aver_rt::AverList<AverStr>), AverStr> {
    loop {
        __state = match __state {
            __MutualTco1::ReadList(mut cs @ _, mut acc @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                aver_list_match!(cs.clone(), [] => { return Err(AverStr::from("unexpected end of data")) }, [__pat0, rest] => { { let __int_match_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat0)); if __int_match_subject == aver_rt::AverInt::from_i64(41) { return Ok((crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(acc.reverse()), rest)) } else { __MutualTco1::ReadListItem(cs, acc) } } })
            }
            __MutualTco1::ReadListItem(mut cs @ _, mut acc @ _) => {
                crate::proof_kernel::cancel_checkpoint();
                match crate::proof_kernel::aver_generated::kernel::sexp::readOne(&cs) {
                    Err(e @ _) => return Err(e),
                    Ok(__pat0 @ _) => {
                        let (item, rest) = __pat0;
                        __MutualTco1::ReadList(
                            crate::proof_kernel::aver_generated::kernel::sexp::skipBlank(rest),
                            aver_rt::AverList::prepend(item, &acc),
                        )
                    }
                }
            }
        };
    }
}

/// The items of a list up to its closing parenthesis.
pub fn readList(
    cs @ _: aver_rt::AverList<AverStr>,
    acc @ _: aver_rt::AverList<Sexp>,
) -> Result<(Sexp, aver_rt::AverList<AverStr>), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ReadList(cs, acc))
}

/// One item of a list, then the rest of the list.
pub fn readListItem(
    cs @ _: aver_rt::AverList<AverStr>,
    acc @ _: aver_rt::AverList<Sexp>,
) -> Result<(Sexp, aver_rt::AverList<AverStr>), AverStr> {
    __mutual_tco_trampoline_1(__MutualTco1::ReadListItem(cs, acc))
}

/// One S-expression and nothing after it but blanks, its shared parts written out.
#[inline(always)]
pub fn readAll(text @ _: AverStr) -> Result<Sexp, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match crate::proof_kernel::aver_generated::kernel::sexp::readOne(
        &crate::proof_kernel::aver_generated::kernel::sexp::skipBlank(
            (aver_rt::AverList::from_vec(text.chars().map(|c| c.to_string()).collect::<Vec<_>>()))
                .into_aver(),
        ),
    ) {
        Err(e @ _) => Err(e),
        Ok(__pat0 @ _) => {
            let (s, rest) = __pat0;
            {
                let __list_subject =
                    crate::proof_kernel::aver_generated::kernel::sexp::skipBlank(rest);
                if __list_subject.is_empty() {
                    crate::proof_kernel::aver_generated::kernel::sexp::expand(s)
                } else {
                    Err(AverStr::from("text after the S-expression"))
                }
            }
        }
    }
}

/// (shared ((#NAME S)…) BODY) is BODY with every #NAME written out as its S; each S may use only the names before it. Anything else is itself.
pub fn expand(mut s @ _: Sexp) -> Result<Sexp, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match &s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat0) => {
            let __pat0 = __pat0.clone();
            {
                let __list_subject = __pat0;
                if let Some((__pat1, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    match __pat1 {
                        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(__pat3) => {
                            match &*__pat3 {
                                "shared" => {
                                    let __list_subject = __pat2;
                                    if let Some((__pat4, __pat5)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        match __pat4 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(parts) => {
            { let __list_subject = __pat5; if let Some((body, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat6; if __list_subject.is_empty() { { let (b, n) = crate::proof_kernel::aver_generated::kernel::sexp::bounded(&crate::proof_kernel::aver_generated::kernel::sexp::within(body, &crate::proof_kernel::aver_generated::kernel::sexp::define(parts, HashMap::new())?)?)?; Ok(b) } } else { Ok(s) } } } else { Ok(s) } }
        },
        _ => {
            Ok(s)
        }
    }
                                    } else {
                                        Ok(s)
                                    }
                                }
                                _ => Ok(s),
                            }
                        }
                        _ => Ok(s),
                    }
                } else {
                    Ok(s)
                }
            }
        }
        _ => Ok(s),
    }
}

/// Each (#NAME S) in order, its S written out with the names before it.
#[inline(always)]
pub fn define(
    mut parts @ _: aver_rt::AverList<Sexp>,
    mut table @ _: aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
) -> Result<aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>, AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(parts, [] => { return Ok(table); }, [__pat0, rest] => { match __pat0 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(__pat1) => {
            { let __list_subject = __pat1; if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { match __pat2 {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(n) => {
            { let __list_subject = __pat3; if let Some((s, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __list_subject = __pat4; if __list_subject.is_empty() { {
            let __tco0 = rest;
            let __tco1 = table.clone().insert_owned(crate::proof_kernel::aver_generated::kernel::sexp::fresh(n, &table)?, crate::proof_kernel::aver_generated::kernel::sexp::bounded(&crate::proof_kernel::aver_generated::kernel::sexp::within(s, &table)?)?);
            parts = __tco0;
            table = __tco1;
            continue;
        } } else { return Err(AverStr::from("malformed shared part")); } } } else { return Err(AverStr::from("malformed shared part")); } }
        },
        _ => {
            return Err(AverStr::from("malformed shared part"));
        }
    } } else { return Err(AverStr::from("malformed shared part")); } }
        },
        _ => {
            return Err(AverStr::from("malformed shared part"));
        }
    } })
    }
}

/// A new shared name: it starts with # and is not defined yet.
#[inline(always)]
pub fn fresh(
    n @ _: AverStr,
    table @ _: &aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
) -> Result<AverStr, AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    if n.starts_with("#") {
        if table.contains_key(&n) {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                };
                __b.push_str(&AverStr::from(" is defined twice"));
                __b
            }))
        } else {
            Ok(n)
        }
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(45)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("a shared name starts with #: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
            __b
        }))
    }
}

/// A written-out part of at most 4194304 atoms, texts and lists: a short script may not stand for an enormous one.
pub fn bounded(r @ _: &(Sexp, aver_rt::AverInt)) -> Result<(Sexp, aver_rt::AverInt), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (s, n) = r.clone();
        if (n > aver_rt::AverInt::from_i64(4194304)) {
            Err(AverStr::from(
                "a shared part stands for more than 4194304 items",
            ))
        } else {
            Ok(r.clone())
        }
    }
}

/// The S-expression with every #NAME written out, and how many atoms, texts and lists that is; a written-out part is the very value the table holds, not a copy.
pub fn within(
    mut s @ _: Sexp,
    table @ _: &aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
) -> Result<(Sexp, aver_rt::AverInt), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    match &s {
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(a) => {
            let a = a.clone();
            if a.starts_with("#") {
                table.get(&a).cloned().ok_or(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(a))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" is not defined before its use"));
                    __b
                }))
            } else {
                Ok((s, aver_rt::AverInt::from_i64(1)))
            }
        }
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Text(t) => {
            let t = t.clone();
            Ok((s, aver_rt::AverInt::from_i64(1)))
        }
        crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(xs) => {
            let xs = xs.clone();
            crate::proof_kernel::aver_generated::kernel::sexp::withinAll(
                xs,
                table.clone(),
                aver_rt::AverList::empty(),
                aver_rt::AverInt::from_i64(1),
            )
        }
    }
}

/// The items of a list written out, then the list and its size.
#[inline(always)]
pub fn withinAll(
    mut xs @ _: aver_rt::AverList<Sexp>,
    table @ _: aver_rt::AverMap<AverStr, (Sexp, aver_rt::AverInt)>,
    mut acc @ _: aver_rt::AverList<Sexp>,
    mut size @ _: aver_rt::AverInt,
) -> Result<(Sexp, aver_rt::AverInt), AverStr> {
    let table @ _ = std::sync::Arc::new(table);
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(xs, [] => { return Ok((crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Node(acc.reverse()), size)); }, [x, rest] => { { let (y, n) = crate::proof_kernel::aver_generated::kernel::sexp::within(x, &*table)?; {
            let __tco0 = rest;
            let __tco2 = aver_rt::AverList::prepend(y, &acc);
            let __tco3 = size.add(&n);
            xs = __tco0;
            acc = __tco2;
            size = __tco3;
            continue;
        } } })
    }
}

/// Read one S-expression from the front of the characters.
#[inline(always)]
pub fn readOne(
    cs @ _: &aver_rt::AverList<AverStr>,
) -> Result<(Sexp, aver_rt::AverList<AverStr>), AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => Err(AverStr::from("unexpected end of data")), [__pat0, rest] => { { let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat0)); if __dispatch_subject == aver_rt::AverInt::from_i64(40) { crate::proof_kernel::aver_generated::kernel::sexp::readList(crate::proof_kernel::aver_generated::kernel::sexp::skipBlank(rest), aver_rt::AverList::empty()) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(34) { crate::proof_kernel::aver_generated::kernel::sexp::readText(rest, AverStr::from("")) } else { if __dispatch_subject == aver_rt::AverInt::from_i64(41) { Err(AverStr::from("unexpected )")) } else { crate::proof_kernel::aver_generated::kernel::sexp::readAtom(cs.clone(), AverStr::from("")) } } } } })
}

/// A quoted text; backslash escapes a quote, a backslash or a newline.
#[inline(always)]
pub fn readText(
    mut cs @ _: aver_rt::AverList<AverStr>,
    mut acc @ _: AverStr,
) -> Result<(Sexp, aver_rt::AverList<AverStr>), AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(cs, [] => { return Err(AverStr::from("unterminated text")); }, [__pat0, __pat1] => { { let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat0)); if __dispatch_subject == aver_rt::AverInt::from_i64(34) { return Ok((crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Text(acc), __pat1)); } else { if __dispatch_subject == aver_rt::AverInt::from_i64(92) { { let __list_subject = __pat1.clone(); if let Some((__pat2, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject) { { let __int_match_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&__pat2)); if __int_match_subject == aver_rt::AverInt::from_i64(110) { {
            let __tco0 = __pat3;
            let __tco1 = (acc + &AverStr::from("\n"));
            cs = __tco0;
            acc = __tco1;
            continue;
        } } else { {
            let __tco0 = __pat3;
            let __tco1 = (acc + &__pat2);
            cs = __tco0;
            acc = __tco1;
            continue;
        } } } } else { {
            let __tco0 = __pat1;
            let __tco1 = (acc + &__pat0);
            cs = __tco0;
            acc = __tco1;
            continue;
        } } } } else { {
            let __tco0 = __pat1;
            let __tco1 = (acc + &__pat0);
            cs = __tco0;
            acc = __tco1;
            continue;
        } } } } })
    }
}

/// An atom runs until a blank, a parenthesis or a quote.
#[inline(always)]
pub fn readAtom(
    mut cs @ _: aver_rt::AverList<AverStr>,
    mut acc @ _: AverStr,
) -> Result<(Sexp, aver_rt::AverList<AverStr>), AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(cs.clone(), [] => { return Ok((crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(acc), aver_rt::AverList::empty())); }, [c, rest] => { if crate::proof_kernel::aver_generated::kernel::sexp::isDelimiter(c.clone()) { return Ok((crate::proof_kernel::aver_generated::kernel::sexp::Sexp::Atom(acc), cs)); } else { {
            let __tco0 = rest;
            let __tco1 = (acc + &c);
            cs = __tco0;
            acc = __tco1;
            continue;
        } } })
    }
}

/// Characters that end an atom.
#[inline(always)]
pub fn isDelimiter(c @ _: AverStr) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
        if __dispatch_subject == aver_rt::AverInt::from_i64(40) {
            true
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(41) {
                true
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(34) {
                    true
                } else {
                    crate::proof_kernel::aver_generated::kernel::sexp::isBlank(c)
                }
            }
        }
    }
}

/// Spaces, tabs and line breaks.
#[inline(always)]
pub fn isBlank(c @ _: AverStr) -> bool {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c));
        if __dispatch_subject == aver_rt::AverInt::from_i64(32) {
            true
        } else {
            if __dispatch_subject == aver_rt::AverInt::from_i64(10) {
                true
            } else {
                if __dispatch_subject == aver_rt::AverInt::from_i64(9) {
                    true
                } else {
                    if __dispatch_subject == aver_rt::AverInt::from_i64(13) {
                        true
                    } else {
                        false
                    }
                }
            }
        }
    }
}

/// Drop leading blanks and comments: a comment runs from `;` to the end of its line, and the reader never sees it.
#[inline(always)]
pub fn skipBlank(mut cs @ _: aver_rt::AverList<AverStr>) -> aver_rt::AverList<AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(cs.clone(), [] => { return aver_rt::AverList::empty(); }, [c, rest] => { { let __int_match_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c)); if __int_match_subject == aver_rt::AverInt::from_i64(59) { {
            let __tco0 = crate::proof_kernel::aver_generated::kernel::sexp::skipLine(rest);
            cs = __tco0;
            continue;
        } } else { if crate::proof_kernel::aver_generated::kernel::sexp::isBlank(c) { {
            let __tco0 = rest;
            cs = __tco0;
            continue;
        } } else { return cs; } } } })
    }
}

/// Drop everything up to and including the next line break.
#[inline(always)]
pub fn skipLine(mut cs @ _: aver_rt::AverList<AverStr>) -> aver_rt::AverList<AverStr> {
    loop {
        crate::proof_kernel::cancel_checkpoint();
        aver_list_match!(cs, [] => { return aver_rt::AverList::empty(); }, [c, rest] => { { let __int_match_subject = aver_rt::AverInt::from_i64(aver_rt::str_code1(&c)); if __int_match_subject == aver_rt::AverInt::from_i64(10) { return rest; } else { {
            let __tco0 = rest;
            cs = __tco0;
            continue;
        } } } })
    }
}
