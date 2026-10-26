#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, Eq)]
pub enum Val {
    VInt(aver_rt::AverInt),
    VBool(bool),
    VList(aver_rt::AverList<Val>),
}

impl std::hash::Hash for Val {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        aver_rt::shared_hash::hash(self, state)
    }
}

impl aver_rt::WalkHash for Val {
    fn stream<H: std::hash::Hasher>(&self, state: &mut H, left: u32) -> u32 {
        match self {
            Val::VInt(f0) => {
                std::hash::Hash::hash(&0u32, state);
                std::hash::Hash::hash(&(*f0), state);
                left
            }
            Val::VBool(f0) => {
                std::hash::Hash::hash(&1u32, state);
                std::hash::Hash::hash(&(*f0), state);
                left
            }
            Val::VList(f0) => {
                std::hash::Hash::hash(&2u32, state);
                let left = aver_rt::shared_items::stream_items(
                    (*f0).iter(),
                    state,
                    left,
                    |x, state, left| aver_rt::WalkHash::stream(&(*x), state, left),
                );
                left
            }
        }
    }

    fn walk_hash(&self, walk: aver_rt::HashWalk<'_>) -> u64 {
        match self {
            Val::VInt(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&0u32, h);
                std::hash::Hash::hash(&(*f0), h);
                std::hash::Hasher::finish(h)
            }
            Val::VBool(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&1u32, h);
                std::hash::Hash::hash(&(*f0), h);
                std::hash::Hasher::finish(h)
            }
            Val::VList(f0) => {
                let h = &mut aver_rt::shared_hash::node_hasher();
                std::hash::Hash::hash(&2u32, h);
                std::hash::Hasher::write_u64(
                    h,
                    aver_rt::shared_items::parts_list(&(*f0), walk, |x, walk| {
                        aver_rt::WalkHash::walk_hash(&(*x), walk)
                    }),
                );
                std::hash::Hasher::finish(h)
            }
        }
    }
}

impl aver_rt::WalkEq for Val {
    fn walk_eq<'v>(&'v self, other: &'v Self, walk: aver_rt::Walk<'_, 'v>) -> bool {
        match (self, other) {
            (Val::VInt(a0), Val::VInt(b0)) => a0 == b0,
            (Val::VBool(a0), Val::VBool(b0)) => a0 == b0,
            (Val::VList(a0), Val::VList(b0)) => {
                aver_rt::shared_items::eq_list(&(*a0), &(*b0), walk, |x, y, walk| {
                    (*x).walk_eq(&(*y), walk)
                })
            }
            _ => false,
        }
    }
}

impl Val {
    fn aver_fast_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Val::VInt(a0), Val::VInt(b0)) => (*a0) == (*b0),
            (Val::VBool(a0), Val::VBool(b0)) => (*a0) == (*b0),
            (Val::VList(a0), Val::VList(b0)) => {
                aver_rt::shared_items::fast_list(&(*a0), &(*b0), |x, y| (*x).aver_fast_eq(&(*y)))
            }
            _ => false,
        }
    }
}

impl PartialEq for Val {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::shared_items::top(self, other, Self::aver_fast_eq)
    }
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
    {
        let __t = t;
        match __t.view() {
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TInt(__n_fields) => {
                let n = __n_fields.0.clone();
                Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(n))
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
                let b = __b_fields.0.clone();
                Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(b))
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                let o = __o_fields.0.clone();
                let a = &__o_fields.1;
                let b = &__o_fields.2;
                {
                    let __t = a;
                    match __t.view() {
                        crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(
                            __c_fields,
                        ) => {
                            let c = __c_fields.0.clone();
                            let xs = &__c_fields.1;
                            {
                                let __t = b;
                                match __t.view() {
        crate::proof_kernel::aver_generated::kernel::term::Term__view::TCtor(__d_fields) => {
            let d = __d_fields.0.clone();
            let ys = &__d_fields.1;
            { let __arg15 @ _ = { let __arg14 @ _ = aver_rt::AverInt::from_i64(xs.len() as i64).add(&aver_rt::AverInt::from_i64(ys.len() as i64)); crate::proof_kernel::aver_generated::kernel::eval::sameCtor(c, d, __arg14) }; crate::proof_kernel::aver_generated::kernel::eval::ctorOp(o, &__arg15) }
        },
        _ => {
            { let __arg16 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a); { let __arg17 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(b); crate::proof_kernel::aver_generated::kernel::eval::evalOp(o, &__arg16, &__arg17) } }
        }
    }
                            }
                        }
                        _ => {
                            let __arg18 @ _ =
                                crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a);
                            {
                                let __arg19 @ _ =
                                    crate::proof_kernel::aver_generated::kernel::eval::evalClosed(
                                        b,
                                    );
                                crate::proof_kernel::aver_generated::kernel::eval::evalOp(
                                    o, &__arg18, &__arg19,
                                )
                            }
                        }
                    }
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                let a = &__a_fields.0;
                {
                    let __arg20 @ _ =
                        crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a);
                    crate::proof_kernel::aver_generated::kernel::eval::negated((&__arg20).as_ref())
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TList(__xs_fields) => {
                let xs = &__xs_fields.0;
                {
                    let __arg21 @ _ =
                        crate::proof_kernel::aver_generated::kernel::eval::evalAll(xs);
                    crate::proof_kernel::aver_generated::kernel::eval::listOf(__arg21)
                }
            }
            crate::proof_kernel::aver_generated::kernel::term::Term__view::TBi(__name_fields) => {
                let name = __name_fields.0.clone();
                let args = &__name_fields.1;
                crate::proof_kernel::aver_generated::kernel::eval::evalBuiltin(name, args)
            }
            _ => None,
        }
    }
}

/// == or != on two constructor applications, once whether they are the same value is known.
#[inline(always)]
pub fn ctorOp(o @ _: AverStr, same @ _: &Option<bool>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match same {
        Some(s @ _) => {
            let s = s.clone();
            crate::proof_kernel::aver_generated::kernel::eval::boolOp(o, s, true)
        }
        None => None,
    }
}

/// Two different constructors of one type differ whatever they hold; one constructor with no fields equals itself. The type is the name up to its last dot, so a constructor spelled two ways is never decided.
pub fn sameCtor(c @ _: AverStr, d @ _: AverStr, fields @ _: aver_rt::AverInt) -> Option<bool> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __subject = (
            (c == d),
            (fields == aver_rt::AverInt::from_i64(0)),
            ((crate::proof_kernel::aver_generated::kernel::eval::typeOf(&c)
                == crate::proof_kernel::aver_generated::kernel::eval::typeOf(&d))
                && (&*crate::proof_kernel::aver_generated::kernel::eval::typeOf(&c) != "")),
        );
        match __subject {
            (true, true, _) => Some(true),
            (false, _, true) => Some(false),
            _ => None,
        }
    }
}

/// A constructor's name up to its last dot.
#[inline(always)]
pub fn typeOf(c @ _: &AverStr) -> AverStr {
    crate::proof_kernel::cancel_checkpoint();
    aver_rt::string_join(
        &({
            let __n = 1usize;
            ((aver_rt::string_split_byte(&c, 46u8)).reverse_owned()).drop_first(__n)
        })
        .reverse_owned(),
        &::aver_rt::str_lit!("."),
    )
}

/// The value of each term.
#[inline(always)]
pub fn evalAll(
    xs @ _: &aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
) -> aver_rt::AverList<Option<Val>> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let __list_subject = xs;
        match aver_rt::list_uncons(__list_subject) {
            Some((x, rest)) => aver_rt::AverList::prepend_owned(
                crate::proof_kernel::aver_generated::kernel::eval::evalClosed(x),
                crate::proof_kernel::aver_generated::kernel::eval::evalAll(&rest),
            ),
            None => aver_rt::AverList::empty(),
        }
    }
}

/// A list value when every element is closed.
#[inline(always)]
pub fn listOf(mut vs @ _: aver_rt::AverList<Option<Val>>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(vs, [] => Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VList(aver_rt::AverList::empty())), [__pat0, rest] => { match __pat0 {
        Some(v) => {
            { let __arg4 @ _ = crate::proof_kernel::aver_generated::kernel::eval::listOf(rest); crate::proof_kernel::aver_generated::kernel::eval::consOnto(&v, (&__arg4).as_ref()) }
        },
        _ => {
            None
        }
    } })
}

/// v in front of a list value.
pub fn consOnto(v @ _: &Val, rest @ _: Option<&Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match rest {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VList(vs) => Some(
                crate::proof_kernel::aver_generated::kernel::eval::Val::VList(
                    aver_rt::AverList::prepend(v.clone(), vs),
                ),
            ),
            _ => None,
        },
        _ => None,
    }
}

/// The negation of an Int value.
pub fn negated(a @ _: Option<&Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match a {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x) => {
                let x = x.clone();
                Some(
                    crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(
                        aver_rt::AverInt::from_i64(0).sub(&x),
                    ),
                )
            }
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
                    match aver_rt::list_uncons(&__list_subject) {
                        Some((a, __pat2)) => {
                            let __list_subject = __pat2;
                            match aver_rt::list_uncons(&__list_subject) {
                                Some((b, __pat3)) => {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        {
                                            let __arg26 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a);
                                            {
                                                let __arg27 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(b);
                                                crate::proof_kernel::aver_generated::kernel::eval::both(&__arg26, &__arg27, true)
                                            }
                                        }
                                    } else {
                                        crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                    }
                                }
                                None => {
                                    crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                        name, args,
                                    )
                                }
                            }
                        }
                        None => crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                            name, args,
                        ),
                    }
                }
            } else {
                if &*__dispatch_subject == "Bool.or" {
                    {
                        let __list_subject = __pat1;
                        match aver_rt::list_uncons(&__list_subject) {
                            Some((a, __pat4)) => {
                                let __list_subject = __pat4;
                                match aver_rt::list_uncons(&__list_subject) { Some((b, __pat5)) => { { let __list_subject = __pat5; if __list_subject.is_empty() { { let __arg28 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a); { let __arg29 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(b); crate::proof_kernel::aver_generated::kernel::eval::both(&__arg28, &__arg29, false) } } } else { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } } }, None => { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } }
                            }
                            None => crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                name, args,
                            ),
                        }
                    }
                } else {
                    if &*__dispatch_subject == "Bool.not" {
                        {
                            let __list_subject = __pat1;
                            match aver_rt::list_uncons(&__list_subject) {
                                Some((a, __pat6)) => {
                                    let __list_subject = __pat6;
                                    if __list_subject.is_empty() {
                                        {
                                            let __arg30 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a);
                                            crate::proof_kernel::aver_generated::kernel::eval::negate((&__arg30).as_ref())
                                        }
                                    } else {
                                        crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args)
                                    }
                                }
                                None => {
                                    crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(
                                        name, args,
                                    )
                                }
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "__int_div_euclid" {
                            {
                                let __list_subject = __pat1;
                                match aver_rt::list_uncons(&__list_subject) { Some((a, __pat7)) => { { let __list_subject = __pat7; match aver_rt::list_uncons(&__list_subject) { Some((k, __pat8)) => { { let __list_subject = __pat8; if __list_subject.is_empty() { { let __arg31 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a); { let __arg32 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(k); crate::proof_kernel::aver_generated::kernel::eval::division(&__arg31, &__arg32, true) } } } else { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } } }, None => { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } } } }, None => { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } }
                            }
                        } else {
                            if &*__dispatch_subject == "__int_mod_euclid" {
                                {
                                    let __list_subject = __pat1;
                                    match aver_rt::list_uncons(&__list_subject) { Some((a, __pat9)) => { { let __list_subject = __pat9; match aver_rt::list_uncons(&__list_subject) { Some((k, __pat10)) => { { let __list_subject = __pat10; if __list_subject.is_empty() { { let __arg33 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a); { let __arg34 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(k); crate::proof_kernel::aver_generated::kernel::eval::division(&__arg33, &__arg34, false) } } } else { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } } }, None => { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } } } }, None => { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } }
                                }
                            } else {
                                if &*__dispatch_subject == "__bits_shl" {
                                    {
                                        let __list_subject = __pat1;
                                        match aver_rt::list_uncons(&__list_subject) { Some((a, __pat11)) => { { let __list_subject = __pat11; match aver_rt::list_uncons(&__list_subject) { Some((n, __pat12)) => { { let __list_subject = __pat12; if __list_subject.is_empty() { { let __arg35 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a); { let __arg36 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(n); crate::proof_kernel::aver_generated::kernel::eval::shifted(&__arg35, &__arg36) } } } else { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } } }, None => { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } } } }, None => { crate::proof_kernel::aver_generated::kernel::eval::listBuiltin(name, args) } }
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
pub fn negate(a @ _: Option<&Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match a {
        Some(__pat0) => match __pat0 {
            crate::proof_kernel::aver_generated::kernel::eval::Val::VBool(x) => {
                let x = x.clone();
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

/// a times 2^n for a count n from 0 to 4096; any other count is not closed.
pub fn shifted(a @ _: &Option<Val>, n @ _: &Option<Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (a.clone(), n.clone());
        match __pat0 {
            Some(__pat2) => match __pat2 {
                crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(x) => match __pat1 {
                    Some(__pat3) => match __pat3 {
                        crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(k) => {
                            if ((k >= aver_rt::AverInt::from_i64(0))
                                && (k <= aver_rt::AverInt::from_i64(4096)))
                            {
                                match (match (x).shift_left(&(k)) { Ok(__v) => Ok(__v), Err(aver_rt::ShiftCountError::Negative) => Err("negative shift count".into()), Err(aver_rt::ShiftCountError::TooLarge) => Err(aver_rt::shift_count_too_large_message()) }).into_aver() { Ok(r @ _) => { Some(crate::proof_kernel::aver_generated::kernel::eval::Val::VInt(r)) }, Err(e @ _) => { None } }
                            } else {
                                None
                            }
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
                    match aver_rt::list_uncons(&__list_subject) {
                        Some((a, __pat2)) => {
                            let __list_subject = __pat2;
                            match aver_rt::list_uncons(&__list_subject) {
                                Some((l, __pat3)) => {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        {
                                            let __arg24 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a);
                                            {
                                                let __arg25 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(l);
                                                crate::proof_kernel::aver_generated::kernel::eval::prepended(&__arg24, &__arg25)
                                            }
                                        }
                                    } else {
                                        None
                                    }
                                }
                                None => None,
                            }
                        }
                        None => None,
                    }
                }
            } else {
                if &*__dispatch_subject == "List.concat" {
                    {
                        let __list_subject = __pat1;
                        match aver_rt::list_uncons(&__list_subject) {
                            Some((a, __pat4)) => {
                                let __list_subject = __pat4;
                                match aver_rt::list_uncons(&__list_subject) {
                                    Some((b, __pat5)) => {
                                        let __list_subject = __pat5;
                                        if __list_subject.is_empty() {
                                            {
                                                let __arg26 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(a);
                                                {
                                                    let __arg27 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(b);
                                                    crate::proof_kernel::aver_generated::kernel::eval::concatenated(&__arg26, &__arg27)
                                                }
                                            }
                                        } else {
                                            None
                                        }
                                    }
                                    None => None,
                                }
                            }
                            None => None,
                        }
                    }
                } else {
                    if &*__dispatch_subject == "List.len" {
                        {
                            let __list_subject = __pat1;
                            match aver_rt::list_uncons(&__list_subject) {
                                Some((l, __pat6)) => {
                                    let __list_subject = __pat6;
                                    if __list_subject.is_empty() {
                                        {
                                            let __arg28 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(l);
                                            crate::proof_kernel::aver_generated::kernel::eval::lengthOf((&__arg28).as_ref())
                                        }
                                    } else {
                                        None
                                    }
                                }
                                None => None,
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "List.reverse" {
                            {
                                let __list_subject = __pat1;
                                match aver_rt::list_uncons(&__list_subject) {
                                    Some((l, __pat7)) => {
                                        let __list_subject = __pat7;
                                        if __list_subject.is_empty() {
                                            {
                                                let __arg29 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(l);
                                                crate::proof_kernel::aver_generated::kernel::eval::reversed((&__arg29).as_ref())
                                            }
                                        } else {
                                            None
                                        }
                                    }
                                    None => None,
                                }
                            }
                        } else {
                            if &*__dispatch_subject == "List.take" {
                                {
                                    let __list_subject = __pat1;
                                    match aver_rt::list_uncons(&__list_subject) {
                                        Some((l, __pat8)) => {
                                            let __list_subject = __pat8;
                                            match aver_rt::list_uncons(&__list_subject) {
                                                Some((n, __pat9)) => {
                                                    let __list_subject = __pat9;
                                                    if __list_subject.is_empty() {
                                                        {
                                                            let __arg30 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(l);
                                                            {
                                                                let __arg31 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(n);
                                                                crate::proof_kernel::aver_generated::kernel::eval::sliced(&__arg30, &__arg31, true)
                                                            }
                                                        }
                                                    } else {
                                                        None
                                                    }
                                                }
                                                None => None,
                                            }
                                        }
                                        None => None,
                                    }
                                }
                            } else {
                                if &*__dispatch_subject == "List.drop" {
                                    {
                                        let __list_subject = __pat1;
                                        match aver_rt::list_uncons(&__list_subject) {
                                            Some((l, __pat10)) => {
                                                let __list_subject = __pat10;
                                                match aver_rt::list_uncons(&__list_subject) {
                                                    Some((n, __pat11)) => {
                                                        let __list_subject = __pat11;
                                                        if __list_subject.is_empty() {
                                                            {
                                                                let __arg32 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(l);
                                                                {
                                                                    let __arg33 @ _ = crate::proof_kernel::aver_generated::kernel::eval::evalClosed(n);
                                                                    crate::proof_kernel::aver_generated::kernel::eval::sliced(&__arg32, &__arg33, false)
                                                                }
                                                            }
                                                        } else {
                                                            None
                                                        }
                                                    }
                                                    None => None,
                                                }
                                            }
                                            None => None,
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
                            aver_rt::AverList::prepend_owned(v, vs),
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
pub fn lengthOf(l @ _: Option<&Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match l {
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
pub fn reversed(l @ _: Option<&Val>) -> Option<Val> {
    crate::proof_kernel::cancel_checkpoint();
    match l {
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
