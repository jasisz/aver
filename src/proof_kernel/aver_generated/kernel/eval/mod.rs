#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Val {
    VInt(aver_rt::AverInt),
    VBool(bool),
    VList(aver_rt::AverList<Val>),
}

impl Val {
    fn aver_key_rank(&self) -> usize {
        match self {
            Val::VBool(..) => 0,
            Val::VInt(..) => 1,
            Val::VList(..) => 2,
        }
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Val {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Val::VBool(a0), Val::VBool(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Val::VInt(a0), Val::VInt(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            (Val::VList(a0), Val::VList(b0)) => std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0)),
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Val {
    fn aver_display(&self) -> String {
        match self {
            Val::VInt(f0) => format!("VInt({})", f0.aver_display_inner()),
            Val::VBool(f0) => format!("VBool({})", f0.aver_display_inner()),
            Val::VList(f0) => format!("VList({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// The value of a closed term, if it is one this evaluator knows.
pub fn evalClosed(t @ _: &crate::proof_kernel::aver_generated::kernel::term::Term) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match t.clone() {
        crate::proof_kernel::aver_generated::kernel::term::Term::TInt(n) => {
            Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(n))
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TBool(b) => {
            Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(b))
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TOp(o, a, b) => {
            let a = (*a).clone();
            let b = (*b).clone();
            crate::proof_kernel::aver_generated::kernel::eval::evalOp(
                o,
                &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a),
                &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&b),
            )
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TNeg(a) => {
            let a = (*a).clone();
            crate::proof_kernel::aver_generated::kernel::eval::negated(
                &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a),
            )
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TList(xs) => {
            crate::proof_kernel::aver_generated::kernel::eval::listOf(
                &crate::proof_kernel::aver_generated::kernel::eval::evalAll(&xs),
            )
        }
        crate::proof_kernel::aver_generated::kernel::term::Term::TBi(name, args) => {
            crate::proof_kernel::aver_generated::kernel::eval::evalBuiltin(name, &args)
        }
        _ => None,
    }
}

/// The value of each term.
#[inline(always)]
pub fn evalAll(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<Option<Val>> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(xs.clone(), [] => aver_rt::AverList::empty(), [x, rest] => aver_rt::AverList::prepend(crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&x), &crate::proof_kernel::aver_generated::kernel::eval::evalAll(&rest)))
}

/// A list value when every element is closed.
#[inline(always)]
pub fn listOf(vs @ _: &aver_rt::AverList<Option<Val>>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(vs.clone(), [] => Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VList(aver_rt::AverList::empty())), [__pat0, rest] => match __pat0 {
        Some(v) => {
            crate::proof_kernel::aver_generated::kernel::eval::consOnto(&v, &crate::proof_kernel::aver_generated::kernel::eval::listOf(&rest))
        },
        _ => {
            None
        }
    })
}

/// v in front of a list value.
pub fn consOnto(v @ _: &Val, rest @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match rest.clone() {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VList(vs) => Some(
                crate::proof_kernel::aver_generated::kernel::eval::Val::VList(
                    aver_rt::AverList::prepend(v.clone(), &vs),
                ),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// The negation of an Int value.
pub fn negated(a @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match a.clone() {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x) => Some(
                crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(
                    aver_rt::AverInt::from_i64(0).sub(&x),
                ),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// A binary operator on two values.
pub fn evalOp(o @ _: AverStr, a @ _: &Option<Val>, b @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a.clone(), b.clone());
        match __pat0 {
            Some(__pat2) => match __pat2 {
                crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x) => match __pat1 {
                    Some(__pat3) => match __pat3 {
                        crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(y) => {
                            crate::proof_kernel::aver_generated::kernel::eval::intOp(o, x, y)
                        }
                        _ => None,
                    },
                    _ => None,
                },
                crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(x) => match __pat1 {
                    Some(__pat4) => match __pat4 {
                        crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(y) => {
                            crate::proof_kernel::aver_generated::kernel::eval::boolOp(o, x, y)
                        }
                        _ => None,
                    },
                    _ => None,
                },
                crate::proof_kernel::aver_generated::kernel::eval::Val::VList(x) => match __pat1 {
                    Some(__pat5) => match __pat5 {
                        crate::proof_kernel::aver_generated::kernel::eval::Val::VList(y) => {
                            crate::proof_kernel::aver_generated::kernel::eval::listOp(o, &x, &y)
                        }
                        _ => None,
                    },
                    _ => None,
                },
            },
            _ => None,
        }
    }
}

/// Int arithmetic and comparison.
#[inline(always)]
pub fn intOp(o @ _: AverStr, x @ _: aver_rt::AverInt, y @ _: aver_rt::AverInt) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = o;
        if &*__dispatch_subject == "+" {
            Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x.add(&y)))
        } else {
            if &*__dispatch_subject == "-" {
                Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x.sub(&y)))
            } else {
                if &*__dispatch_subject == "*" {
                    Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x.mul(&y)))
                } else {
                    if &*__dispatch_subject == "<" {
                        Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x < y)))
                    } else {
                        if &*__dispatch_subject == ">" {
                            Some(
                                crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(
                                    (x > y),
                                ),
                            )
                        } else {
                            if &*__dispatch_subject == "<=" {
                                Some(
                                    crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(
                                        (x <= y),
                                    ),
                                )
                            } else {
                                if &*__dispatch_subject == ">=" {
                                    Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x >= y)))
                                } else {
                                    if &*__dispatch_subject == "==" {
                                        Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x == y)))
                                    } else {
                                        if &*__dispatch_subject == "!=" {
                                            Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x != y)))
                                        } else {
                                            None
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

/// Equality of Bools.
#[inline(always)]
pub fn boolOp(o @ _: AverStr, x @ _: bool, y @ _: bool) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = o;
        if &*__dispatch_subject == "==" {
            Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x == y)))
        } else {
            if &*__dispatch_subject == "!=" {
                Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x != y)))
            } else {
                None
            }
        }
    }
}

/// Equality of lists.
#[inline(always)]
pub fn listOp(
    o @ _: AverStr,
    x @ _: &aver_rt::AverList<Val>,
    y @ _: &aver_rt::AverList<Val>,
) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __dispatch_subject = o;
        if &*__dispatch_subject == "==" {
            Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x == y)))
        } else {
            if &*__dispatch_subject == "!=" {
                Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x != y)))
            } else {
                None
            }
        }
    }
}

/// The builtins the evaluator knows.
pub fn evalBuiltin(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (name.clone(), args.clone());
        {
            let __dispatch_subject = __pat0;
            if &*__dispatch_subject == "Bool.and" {
                {
                    let __list_subject = __pat1;
                    if let Some((a, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat2;
                            if let Some((b, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        crate::proof_kernel::aver_generated::kernel::eval::both(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&b), true)
                                    } else {
                                        crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                    }
                                }
                            } else {
                                crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                    name, args,
                                )
                            }
                        }
                    } else {
                        crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                    }
                }
            } else {
                if &*__dispatch_subject == "Bool.or" {
                    {
                        let __list_subject = __pat1;
                        if let Some((a, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat4;
                                if let Some((b, __pat5)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat5;
                                        if __list_subject.is_empty() {
                                            crate::proof_kernel::aver_generated::kernel::eval::both(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&b), false)
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                        }
                                    }
                                } else {
                                    crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                        name, args,
                                    )
                                }
                            }
                        } else {
                            crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                name, args,
                            )
                        }
                    }
                } else {
                    if &*__dispatch_subject == "Bool.not" {
                        {
                            let __list_subject = __pat1;
                            if let Some((a, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat6;
                                    if __list_subject.is_empty() {
                                        crate::proof_kernel::aver_generated::kernel::eval::negate(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a))
                                    } else {
                                        crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                    }
                                }
                            } else {
                                crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                    name, args,
                                )
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "__int_div_euclid" {
                            {
                                let __list_subject = __pat1;
                                if let Some((a, __pat7)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat7;
                                        if let Some((k, __pat8)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat8;
                                                if __list_subject.is_empty() {
                                                    crate::proof_kernel::aver_generated::kernel::eval::division(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&k), true)
                                                } else {
                                                    crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                                }
                                            }
                                        } else {
                                            crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                        }
                                    }
                                } else {
                                    crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                        name, args,
                                    )
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "__int_mod_euclid" {
                                {
                                    let __list_subject = __pat1;
                                    if let Some((a, __pat9)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat9;
                                            if let Some((k, __pat10)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                {
                                                    let __list_subject = __pat10;
                                                    if __list_subject.is_empty() {
                                                        crate::proof_kernel::aver_generated::kernel::eval::division(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&k), false)
                                                    } else {
                                                        crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                                    }
                                                }
                                            } else {
                                                crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                            }
                                        }
                                    } else {
                                        crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                    }
                                }
                            } else {
                                crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                    name, args,
                                )
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Bool.and (isAnd) or Bool.or of two values.
pub fn both(a @ _: &Option<Val>, b @ _: &Option<Val>, isAnd @ _: bool) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a.clone(), b.clone());
        match __pat0 {
            Some(__pat2) => match __pat2 {
                crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(x) => {
                    match __pat1 {
                        Some(__pat3) => match __pat3 {
                            crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(y) => {
                                if isAnd {
                                    Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x && y)))
                                } else {
                                    Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((x || y)))
                                }
                            }
                            _ => None,
                        },
                        _ => None,
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }
}

/// Bool.not of a value.
pub fn negate(a @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match a.clone() {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(x) => {
                Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool((!x)))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Euclidean quotient or remainder; a zero divisor is not closed.
pub fn division(a @ _: &Option<Val>, k @ _: &Option<Val>, quotient @ _: bool) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a.clone(), k.clone());
        match __pat0 {
            Some(__pat2) => match __pat2 {
                crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x) => match __pat1 {
                    Some(__pat3) => match __pat3 {
                        crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(y) => {
                            crate::proof_kernel::aver_generated::kernel::eval::divided(
                                x, y, quotient,
                            )
                        }
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }
}

/// The Euclidean result of dividing x by y.
#[inline(always)]
pub fn divided(
    x @ _: aver_rt::AverInt,
    y @ _: aver_rt::AverInt,
    quotient @ _: bool,
) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    if quotient {
        match (match (x).div_euclid(&(y)) {
            Some(__q) => Ok(__q),
            None => Err("division by zero".to_string()),
        })
        .into_aver()
        {
            Ok(q @ _) => Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(q)),
            Err(e @ _) => None,
        }
    } else {
        match (match (x).rem_euclid(&(y)) {
            Some(__r) => Ok(__r),
            None => Err("division by zero".to_string()),
        })
        .into_aver()
        {
            Ok(r @ _) => Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(r)),
            Err(e @ _) => None,
        }
    }
}

/// List.prepend, concat, len, reverse, take and drop on closed lists, as Aver defines them: a count below zero counts as zero.
pub fn listBuiltin(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (name, args.clone());
        {
            let __dispatch_subject = __pat0;
            if &*__dispatch_subject == "List.prepend" {
                {
                    let __list_subject = __pat1;
                    if let Some((a, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                        {
                            let __list_subject = __pat2;
                            if let Some((l, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        crate::proof_kernel::aver_generated::kernel::eval::prepended(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&l))
                                    } else {
                                        None
                                    }
                                }
                            } else {
                                None
                            }
                        }
                    } else {
                        None
                    }
                }
            } else {
                if &*__dispatch_subject == "List.concat" {
                    {
                        let __list_subject = __pat1;
                        if let Some((a, __pat4)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat4;
                                if let Some((b, __pat5)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat5;
                                        if __list_subject.is_empty() {
                                            crate::proof_kernel::aver_generated::kernel::eval::concatenated(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&a), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&b))
                                        } else {
                                            None
                                        }
                                    }
                                } else {
                                    None
                                }
                            }
                        } else {
                            None
                        }
                    }
                } else {
                    if &*__dispatch_subject == "List.len" {
                        {
                            let __list_subject = __pat1;
                            if let Some((l, __pat6)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat6;
                                    if __list_subject.is_empty() {
                                        crate::proof_kernel::aver_generated::kernel::eval::lengthOf(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&l))
                                    } else {
                                        None
                                    }
                                }
                            } else {
                                None
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "List.reverse" {
                            {
                                let __list_subject = __pat1;
                                if let Some((l, __pat7)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat7;
                                        if __list_subject.is_empty() {
                                            crate::proof_kernel::aver_generated::kernel::eval::reversed(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&l))
                                        } else {
                                            None
                                        }
                                    }
                                } else {
                                    None
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "List.take" {
                                {
                                    let __list_subject = __pat1;
                                    if let Some((l, __pat8)) =
                                        aver_rt::list_uncons_cloned(&__list_subject)
                                    {
                                        {
                                            let __list_subject = __pat8;
                                            if let Some((n, __pat9)) =
                                                aver_rt::list_uncons_cloned(&__list_subject)
                                            {
                                                {
                                                    let __list_subject = __pat9;
                                                    if __list_subject.is_empty() {
                                                        crate::proof_kernel::aver_generated::kernel::eval::sliced(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&l), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&n), true)
                                                    } else {
                                                        None
                                                    }
                                                }
                                            } else {
                                                None
                                            }
                                        }
                                    } else {
                                        None
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "List.drop" {
                                    {
                                        let __list_subject = __pat1;
                                        if let Some((l, __pat10)) =
                                            aver_rt::list_uncons_cloned(&__list_subject)
                                        {
                                            {
                                                let __list_subject = __pat10;
                                                if let Some((n, __pat11)) =
                                                    aver_rt::list_uncons_cloned(&__list_subject)
                                                {
                                                    {
                                                        let __list_subject = __pat11;
                                                        if __list_subject.is_empty() {
                                                            crate::proof_kernel::aver_generated::kernel::eval::sliced(&crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&l), &crate::proof_kernel::aver_generated::kernel::eval::evalClosed(&n), false)
                                                        } else {
                                                            None
                                                        }
                                                    }
                                                } else {
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        }
                                    }
                                } else {
                                    None
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// An element in front of a list.
pub fn prepended(a @ _: &Option<Val>, l @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a.clone(), l.clone());
        match __pat0 {
            Some(v) => match __pat1 {
                Some(__pat2) => match __pat2 {
                    crate::proof_kernel::aver_generated::kernel::eval::Val::VList(vs) => Some(
                        crate::proof_kernel::aver_generated::kernel::eval::Val::VList(
                            aver_rt::AverList::prepend(v, &vs),
                        ),
                    ),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }
}

/// One list after the other.
pub fn concatenated(a @ _: &Option<Val>, b @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a.clone(), b.clone());
        match __pat0 {
            Some(__pat2) => match __pat2 {
                crate::proof_kernel::aver_generated::kernel::eval::Val::VList(x) => match __pat1 {
                    Some(__pat3) => match __pat3 {
                        crate::proof_kernel::aver_generated::kernel::eval::Val::VList(y) => Some(
                            crate::proof_kernel::aver_generated::kernel::eval::Val::VList(
                                aver_rt::AverList::concat(&x, &y),
                            ),
                        ),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }
}

/// How many elements.
pub fn lengthOf(l @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match l.clone() {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VList(vs) => Some(
                crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(
                    aver_rt::AverInt::from_i64(vs.len() as i64),
                ),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// The elements backwards.
pub fn reversed(l @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match l.clone() {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VList(vs) => {
                Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VList(vs.reverse()))
            }
            _ => None,
        },
        _ => None,
    }
}

/// The first n elements (front) or all but them; n below zero counts as zero.
pub fn sliced(l @ _: &Option<Val>, n @ _: &Option<Val>, front @ _: bool) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (l.clone(), n.clone());
        match __pat0 {
            Some(__pat2) => match __pat2 {
                crate::proof_kernel::aver_generated::kernel::eval::Val::VList(vs) => {
                    match __pat1 {
                        Some(__pat3) => match __pat3 {
                            crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(k) => {
                                if front {
                                    Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VList({ let __n = aver_rt::clamp_list_count(&(k)); aver_rt::AverList::from_vec((vs).iter().take(__n).cloned().collect::<Vec<_>>()) }))
                                } else {
                                    Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VList({ let __n = aver_rt::clamp_list_count(&(k)); (vs).drop_first(__n) }))
                                }
                            }
                            _ => None,
                        },
                        _ => None,
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }
}
