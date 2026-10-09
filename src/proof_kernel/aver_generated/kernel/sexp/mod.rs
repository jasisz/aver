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

/// One S-expression and nothing after it but blanks.
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
                    Ok(s)
                } else {
                    Err(AverStr::from("text after the S-expression"))
                }
            }
        }
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
