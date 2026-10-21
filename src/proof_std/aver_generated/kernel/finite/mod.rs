#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

/// Every value of the type.
pub fn values(
    f @ _: &crate::proof_std::aver_generated::kernel::term::Fin,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    match f {
        crate::proof_std::aver_generated::kernel::term::Fin::FBool => {
            aver_rt::AverList::from_vec(vec![
                crate::proof_std::aver_generated::kernel::term::Term::TBool(
                    crate::proof_std::aver_generated::kernel::term::Term__TBool(false),
                ),
                crate::proof_std::aver_generated::kernel::term::Term::TBool(
                    crate::proof_std::aver_generated::kernel::term::Term__TBool(true),
                ),
            ])
        }
        crate::proof_std::aver_generated::kernel::term::Fin::FSum(cs) => {
            crate::proof_std::aver_generated::kernel::finite::ctorValues(cs)
        }
        crate::proof_std::aver_generated::kernel::term::Fin::FRec(n, fs) => {
            let n = n.clone();
            crate::proof_std::aver_generated::kernel::finite::recValues(
                n,
                &crate::proof_std::aver_generated::kernel::finite::fieldNames(fs),
                &crate::proof_std::aver_generated::kernel::finite::product(
                    &crate::proof_std::aver_generated::kernel::finite::fieldValues(fs),
                ),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Fin::FTuple(ts) => {
            crate::proof_std::aver_generated::kernel::finite::tupleValues(
                &crate::proof_std::aver_generated::kernel::finite::product(
                    &crate::proof_std::aver_generated::kernel::finite::partValues(ts),
                ),
            )
        }
    }
}

/// Each constructor, applied to nothing.
#[inline(always)]
pub fn ctorValues(
    cs @ _: &aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = cs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((c, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::term::Term::TCtor(
                        crate::proof_std::aver_generated::kernel::term::Term__TCtor(
                            c,
                            aver_rt::AverList::empty(),
                        ),
                    ),
                    &crate::proof_std::aver_generated::kernel::finite::ctorValues(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The field names, in order.
#[inline(always)]
pub fn fieldNames(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::FinField>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((f, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend(
                    f.name,
                    &crate::proof_std::aver_generated::kernel::finite::fieldNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The values of each field's type.
#[inline(always)]
pub fn fieldValues(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::FinField>,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((f, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::finite::values(&f.fin),
                    &crate::proof_std::aver_generated::kernel::finite::fieldValues(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The values of each part's type.
#[inline(always)]
pub fn partValues(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Fin>,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::finite::values(t),
                    &crate::proof_std::aver_generated::kernel::finite::partValues(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Every choice of one value per part, the first part varying slowest.
#[inline(always)]
pub fn product(
    parts @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = parts;
        if __list_subject.is_empty() {
            aver_rt::AverList::from_vec(vec![aver_rt::AverList::empty()])
        } else {
            if let (Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                crate::proof_std::aver_generated::kernel::finite::prefixEach(
                    p,
                    &crate::proof_std::aver_generated::kernel::finite::product(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Each head in front of every tail, heads in order.
#[inline(always)]
pub fn prefixEach(
    heads @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    tails @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = heads;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(h), Some(hs)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::concat(
                    &crate::proof_std::aver_generated::kernel::finite::consAll(h, tails),
                    &crate::proof_std::aver_generated::kernel::finite::prefixEach(&hs, tails),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// One head in front of every tail.
#[inline(always)]
pub fn consAll(
    h @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    tails @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = tails;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((t, ts)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend(
                    aver_rt::AverList::prepend(h.clone(), &t),
                    &crate::proof_std::aver_generated::kernel::finite::consAll(h, &ts),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// A record literal per row of field values.
#[inline(always)]
pub fn recValues(
    n @ _: AverStr,
    names @ _: &aver_rt::AverList<AverStr>,
    rows @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = rows;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(r), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::term::Term::TRec(
                        crate::proof_std::aver_generated::kernel::term::Term__TRec(
                            n.clone(),
                            crate::proof_std::aver_generated::kernel::finite::zipFields(names, r),
                        ),
                    ),
                    &crate::proof_std::aver_generated::kernel::finite::recValues(n, names, &rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Pair field names with values.
pub fn zipFields(
    names @ _: &aver_rt::AverList<AverStr>,
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field> {
    crate::proof_std::cancel_checkpoint();
    {
        let __int_match_subject = (names.clone(), vs.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((n, ns)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((v, rest)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            aver_rt::AverList::prepend(
                crate::proof_std::aver_generated::kernel::term::Field { name: n, value: v },
                &crate::proof_std::aver_generated::kernel::finite::zipFields(&ns, &rest),
            )
        } else {
            aver_rt::AverList::empty()
        }
    }
}

/// A tuple per row of part values.
#[inline(always)]
pub fn tupleValues(
    rows @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = rows;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let Some((r, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                aver_rt::AverList::prepend(
                    crate::proof_std::aver_generated::kernel::term::Term::TTuple(
                        crate::proof_std::aver_generated::kernel::term::Term__TTuple(r),
                    ),
                    &crate::proof_std::aver_generated::kernel::finite::tupleValues(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}
