#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

/// Every value of the type.
pub fn values(
    f @ _: &crate::proof_kernel::aver_generated::kernel::term::Fin,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    match f.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Fin::FBool => {
            aver_rt::AverList::from_vec(vec![
                crate::proof_kernel::aver_generated::kernel::term::Term::TBool(false),
                crate::proof_kernel::aver_generated::kernel::term::Term::TBool(true),
            ])
        }
        crate::proof_kernel::aver_generated::kernel::term::Fin::FSum(cs) => {
            crate::proof_kernel::aver_generated::kernel::finite::ctorValues(&cs)
        }
        crate::proof_kernel::aver_generated::kernel::term::Fin::FRec(n, fs) => {
            crate::proof_kernel::aver_generated::kernel::finite::recValues(
                n,
                &crate::proof_kernel::aver_generated::kernel::finite::fieldNames(&fs),
                &crate::proof_kernel::aver_generated::kernel::finite::product(
                    &crate::proof_kernel::aver_generated::kernel::finite::fieldValues(&fs),
                ),
            )
        }
        crate::proof_kernel::aver_generated::kernel::term::Fin::FTuple(ts) => {
            crate::proof_kernel::aver_generated::kernel::finite::tupleValues(
                &crate::proof_kernel::aver_generated::kernel::finite::product(
                    &crate::proof_kernel::aver_generated::kernel::finite::partValues(&ts),
                ),
            )
        }
    }
}

/// Each constructor, applied to nothing.
#[inline(always)]
pub fn ctorValues(
    cs @ _: &aver_rt::AverList<AverStr>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(cs.clone(), [] => aver_rt::AverList::empty(), [c, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::Term::TCtor(c, aver_rt::AverList::empty()), &crate::proof_kernel::aver_generated::kernel::finite::ctorValues(&rest)))
}

/// The field names, in order.
#[inline(always)]
pub fn fieldNames(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::FinField>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverList::empty(), [f, rest] => aver_rt::AverList::prepend(f.name, &crate::proof_kernel::aver_generated::kernel::finite::fieldNames(&rest)))
}

/// The values of each field's type.
#[inline(always)]
pub fn fieldValues(
    fs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::FinField>,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => aver_rt::AverList::empty(), [f, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::finite::values(&f.fin), &crate::proof_kernel::aver_generated::kernel::finite::fieldValues(&rest)))
}

/// The values of each part's type.
#[inline(always)]
pub fn partValues(
    ts @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Fin>,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => aver_rt::AverList::empty(), [t, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::finite::values(&t), &crate::proof_kernel::aver_generated::kernel::finite::partValues(&rest)))
}

/// Every choice of one value per part, the first part varying slowest.
#[inline(always)]
pub fn product(
    parts @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(parts.clone(), [] => aver_rt::AverList::from_vec(vec![aver_rt::AverList::empty()]), [p, rest] => crate::proof_kernel::aver_generated::kernel::finite::prefixEach(&p, &crate::proof_kernel::aver_generated::kernel::finite::product(&rest)))
}

/// Each head in front of every tail, heads in order.
#[inline(always)]
pub fn prefixEach(
    heads @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    tails @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(heads.clone(), [] => aver_rt::AverList::empty(), [h, hs] => aver_rt::AverList::concat(&crate::proof_kernel::aver_generated::kernel::finite::consAll(&h, tails), &crate::proof_kernel::aver_generated::kernel::finite::prefixEach(&hs, tails)))
}

/// One head in front of every tail.
#[inline(always)]
pub fn consAll(
    h @ _: &crate::proof_kernel::aver_generated::kernel::term::Term,
    tails @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(tails.clone(), [] => aver_rt::AverList::empty(), [t, ts] => aver_rt::AverList::prepend(aver_rt::AverList::prepend(h.clone(), &t), &crate::proof_kernel::aver_generated::kernel::finite::consAll(h, &ts)))
}

/// A record literal per row of field values.
#[inline(always)]
pub fn recValues(
    n @ _: AverStr,
    names @ _: &aver_rt::AverList<AverStr>,
    rows @ _: &aver_rt::AverList<
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(rows.clone(), [] => aver_rt::AverList::empty(), [r, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::Term::TRec(n.clone(), crate::proof_kernel::aver_generated::kernel::finite::zipFields(names, &r)), &crate::proof_kernel::aver_generated::kernel::finite::recValues(n, names, &rest)))
}

/// Pair field names with values.
pub fn zipFields(
    names @ _: &aver_rt::AverList<AverStr>,
    vs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Field> {
    crate::proof_kernel::cancel_checkpoint();
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
                crate::proof_kernel::aver_generated::kernel::term::Field { name: n, value: v },
                &crate::proof_kernel::aver_generated::kernel::finite::zipFields(&ns, &rest),
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
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    >,
) -> aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(rows.clone(), [] => aver_rt::AverList::empty(), [r, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::term::Term::TTuple(r), &crate::proof_kernel::aver_generated::kernel::finite::tupleValues(&rest)))
}
