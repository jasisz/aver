#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

pub type TyEnv = Box<TyEnv__Fields>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyEnv__Fields {
    pub vars: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    pub defs: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Def>,
    pub consts: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const>,
    pub sums: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum>,
    pub recs: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec>,
    pub sigs: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::FnSig>,
}

impl PartialOrd for TyEnv__Fields {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TyEnv__Fields {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.consts.cmp(&other.consts))
            .then_with(|| self.defs.cmp(&other.defs))
            .then_with(|| self.recs.cmp(&other.recs))
            .then_with(|| self.sigs.cmp(&other.sigs))
            .then_with(|| self.sums.cmp(&other.sums))
            .then_with(|| self.vars.cmp(&other.vars))
    }
}

impl aver_rt::AverDisplay for TyEnv__Fields {
    fn aver_display(&self) -> String {
        format!(
            "TyEnv({})",
            vec![
                format!("vars: {}", self.vars.aver_display_inner()),
                format!("defs: {}", self.defs.aver_display_inner()),
                format!("consts: {}", self.consts.aver_display_inner()),
                format!("sums: {}", self.sums.aver_display_inner()),
                format!("recs: {}", self.recs.aver_display_inner()),
                format!("sigs: {}", self.sigs.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// The environment with these variables in front.
pub fn withVars(
    e @ _: &TyEnv,
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> TyEnv {
    crate::proof_std::cancel_checkpoint();
    Box::new(
        crate::proof_std::aver_generated::kernel::types::TyEnv__Fields {
            vars: aver_rt::AverList::concat(&vs.clone(), &e.vars.clone()),
            defs: e.defs.clone(),
            consts: e.consts.clone(),
            sums: e.sums.clone(),
            recs: e.recs.clone(),
            sigs: e.sigs.clone(),
        },
    )
}

/// No variables and no declarations.
pub fn empty() -> TyEnv {
    crate::proof_std::cancel_checkpoint();
    Box::new(
        crate::proof_std::aver_generated::kernel::types::TyEnv__Fields {
            vars: aver_rt::AverList::empty(),
            defs: aver_rt::AverList::empty(),
            consts: aver_rt::AverList::empty(),
            sums: aver_rt::AverList::empty(),
            recs: aver_rt::AverList::empty(),
            sigs: aver_rt::AverList::empty(),
        },
    )
}

/// A type as Aver spells it; a hole is _.
pub fn show(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyInt => {
                ::aver_rt::str_lit!("Int")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyBool => {
                ::aver_rt::str_lit!("Bool")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyStr => {
                ::aver_rt::str_lit!("String")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyFloat => {
                ::aver_rt::str_lit!("Float")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyUnit => {
                ::aver_rt::str_lit!("Unit")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("List<"));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(">"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyOption(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("Option<"));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(">"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyResult(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("Result<"));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::types::show(a)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(", "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(b)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(">"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyMap(__k_fields) => {
                let k = &__k_fields.0;
                let v = &__k_fields.1;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("Map<"));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(crate::proof_std::aver_generated::kernel::types::show(k)),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(", "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(v)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(">"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVector(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("Vector<"));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(">"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("("));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &((aver_rt::string_join(
                                &crate::proof_std::aver_generated::kernel::types::showAll(xs),
                                &::aver_rt::str_lit!(", "),
                            ))
                            .into_aver()),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(")"));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyNamed(__n_fields) => {
                let n = __n_fields.0.clone();
                n
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyAny => {
                ::aver_rt::str_lit!("_")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("?"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyRigid(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("?"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                })
            }
        }
    }
}

/// Each type spelled.
#[inline(always)]
pub fn showAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::types::show(t),
                    crate::proof_std::aver_generated::kernel::types::showAll(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The type of a named variable, the first of its name.
#[inline(always)]
pub fn find(
    mut n @ _: AverStr,
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Ty> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>;
    let mut vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> =
        vs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = vs;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (v.name == n) {
                        return Some(v.ty.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            vs = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The declared type of a module-level binding.
#[inline(always)]
pub fn constType(
    mut n @ _: AverStr,
    cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Ty> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const>;
    let mut cs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const> = cs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = cs;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(c), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (c.name == n) {
                        return Some(c.ty.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            cs = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// What a type is made of: its outermost shape, or the whole type when it has no parts. A declared name, a type variable and an unknown type each have a head no other kind of type has, whatever their name: _ is the head of a hole alone.
pub fn headOf(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = __a_fields.0.clone();
                ::aver_rt::str_lit!("List")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyOption(__a_fields) => {
                let a = __a_fields.0.clone();
                ::aver_rt::str_lit!("Option")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVector(__a_fields) => {
                let a = __a_fields.0.clone();
                ::aver_rt::str_lit!("Vector")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyResult(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                ::aver_rt::str_lit!("Result")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyMap(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                ::aver_rt::str_lit!("Map")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyTuple(__xs_fields) => {
                let xs = __xs_fields.0.clone();
                ::aver_rt::str_lit!("tuple")
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyNamed(__n_fields) => {
                let n = __n_fields.0.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("named "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("var "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                })
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyRigid(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("unknown "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(eq))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                    __b
                })
            }
            _ => crate::proof_std::aver_generated::kernel::types::show(t),
        }
    }
}

/// The types a type is made of, in order.
pub fn partsOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverList::from_vec(vec![a.clone()])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyOption(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverList::from_vec(vec![a.clone()])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVector(__a_fields) => {
                let a = &__a_fields.0;
                aver_rt::AverList::from_vec(vec![a.clone()])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyResult(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                aver_rt::AverList::from_vec(vec![a.clone(), b.clone()])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyMap(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                aver_rt::AverList::from_vec(vec![a.clone(), b.clone()])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                xs.clone()
            }
            _ => aver_rt::AverList::empty(),
        }
    }
}

/// The same shape over new parts.
pub fn rebuild(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    parts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = __a_fields.0.clone();
                crate::proof_std::aver_generated::kernel::term::Ty::TyList(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyList(
                        crate::proof_std::aver_generated::kernel::types::nth(
                            parts.clone(),
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyOption(__a_fields) => {
                let a = __a_fields.0.clone();
                crate::proof_std::aver_generated::kernel::term::Ty::TyOption(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyOption(
                        crate::proof_std::aver_generated::kernel::types::nth(
                            parts.clone(),
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVector(__a_fields) => {
                let a = __a_fields.0.clone();
                crate::proof_std::aver_generated::kernel::term::Ty::TyVector(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyVector(
                        crate::proof_std::aver_generated::kernel::types::nth(
                            parts.clone(),
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyResult(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::proof_std::aver_generated::kernel::term::Ty::TyResult(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyResult(
                        crate::proof_std::aver_generated::kernel::types::nth(
                            parts.clone(),
                            aver_rt::AverInt::from_i64(0),
                        ),
                        crate::proof_std::aver_generated::kernel::types::nth(
                            parts.clone(),
                            aver_rt::AverInt::from_i64(1),
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyMap(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                crate::proof_std::aver_generated::kernel::term::Ty::TyMap(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyMap(
                        crate::proof_std::aver_generated::kernel::types::nth(
                            parts.clone(),
                            aver_rt::AverInt::from_i64(0),
                        ),
                        crate::proof_std::aver_generated::kernel::types::nth(
                            parts.clone(),
                            aver_rt::AverInt::from_i64(1),
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyTuple(__xs_fields) => {
                let xs = __xs_fields.0.clone();
                crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyTuple(parts.clone()),
                )
            }
            _ => t,
        }
    }
}

/// The type at a place, a hole past the end.
pub fn nth(
    mut ts @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut i @ _: aver_rt::AverInt,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __int_match_subject = (ts, i.clone());
            let (__lit0, __lit1) = &__int_match_subject;
            if !(*__lit0).is_empty() && &(*__lit1) == &aver_rt::AverInt::from_i64(0) {
                let Some((t, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                return t;
            } else if !(*__lit0).is_empty() {
                let Some((t, rest)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                {
                    let __tco0 = rest;
                    let __tco1 = i.sub(&aver_rt::AverInt::from_i64(1));
                    ts = __tco0;
                    i = __tco1;
                    continue;
                }
            } else {
                return crate::proof_std::aver_generated::kernel::term::Ty::TyAny;
            }
        }
    }
}

/// The one type two types of a value can both be: a hole takes the other side, the rest must agree in shape and part by part.
pub fn join(
    mut a @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    mut b @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __subject = (
            (&*crate::proof_std::aver_generated::kernel::types::headOf(&a) == "_"),
            (&*crate::proof_std::aver_generated::kernel::types::headOf(&b) == "_"),
            ((crate::proof_std::aver_generated::kernel::types::headOf(&a)
                == crate::proof_std::aver_generated::kernel::types::headOf(&b))
                && (aver_rt::AverInt::from_i64(
                    crate::proof_std::aver_generated::kernel::types::partsOf(&a).len() as i64,
                ) == aver_rt::AverInt::from_i64(
                    crate::proof_std::aver_generated::kernel::types::partsOf(&b).len() as i64,
                ))),
        );
        match __subject {
            (true, _, _) => Ok(b),
            (_, true, _) => Ok(a),
            (_, _, true) => Ok({
                let __arg4 @ _ = {
                    let __arg2 @ _ = crate::proof_std::aver_generated::kernel::types::partsOf(&a);
                    {
                        let __arg3 @ _ =
                            crate::proof_std::aver_generated::kernel::types::partsOf(&b);
                        crate::proof_std::aver_generated::kernel::types::joinEach(&__arg2, &__arg3)
                    }
                }?;
                crate::proof_std::aver_generated::kernel::types::rebuild(a, &__arg4)
            }),
            _ => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(40)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(&a)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is not "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_std::aver_generated::kernel::types::show(&b)),
                )));
                __b
            })),
        }
    }
}

/// Two lists of types joined part by part, as far as both go.
pub fn joinEach(
    xs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    ys @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __int_match_subject = (xs.clone(), ys.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((x, xr)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((y, yr)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            Ok(aver_rt::AverList::prepend_owned(
                crate::proof_std::aver_generated::kernel::types::join(x, y)?,
                crate::proof_std::aver_generated::kernel::types::joinEach(&xr, &yr)?,
            ))
        } else {
            Ok(aver_rt::AverList::empty())
        }
    }
}

/// Every type joined with the one before; a hole when there is none.
#[inline(always)]
pub fn joinAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut acc @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>;
    let mut ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> = ts;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ts;
            if __list_subject.is_empty() {
                return Ok(acc);
            } else {
                if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco0 = rest;
                        let __tco1 =
                            crate::proof_std::aver_generated::kernel::types::join(acc, t.clone())?;
                        __hold0 = __tco0;
                        ts = &__hold0;
                        acc = __tco1;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Whether a value of the first type can stand where the second is expected.
#[inline(always)]
pub fn fits(
    mut actual @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    mut expected @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::types::join(actual, expected) {
        Ok(t @ _) => true,
        Err(why @ _) => false,
    }
}

/// Whether == between two values of the type is equality. A declared type must claim it and every field reachable from it must have equality; revisiting a recursive declaration closes that structural check. Other types follow their builtin shape and parts; a type variable follows its equality mark.
#[inline(always)]
pub fn hasEq(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty, e @ _: &TyEnv) -> bool {
    crate::proof_std::cancel_checkpoint();
    let __arg2 @ _ = <aver_rt::AverList<AverStr>>::empty();
    crate::proof_std::aver_generated::kernel::types::hasEqSeen(t, e, &__arg2)
}

/// Equality checked structurally, with declared names already being checked treated coinductively.
pub fn hasEqSeen(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyNamed(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::proof_std::aver_generated::kernel::types::namedHasEq(n, e, seen)
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                eq
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyRigid(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                eq
            }
            _ => {
                ({
                    let __arg8 @ _ = crate::proof_std::aver_generated::kernel::types::headOf(t);
                    crate::proof_std::aver_generated::kernel::builtins::equality(__arg8)
                } && {
                    let __arg9 @ _ = crate::proof_std::aver_generated::kernel::types::partsOf(t);
                    crate::proof_std::aver_generated::kernel::types::allEqSeen(&__arg9, e, seen)
                })
            }
        }
    }
}

/// A type as the proof of its own statement sees it: each type variable is one unknown type, which fits only itself and has an equality only when the variable asks for one.
pub fn rigid(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                crate::proof_std::aver_generated::kernel::term::Ty::TyRigid(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyRigid(n, eq),
                )
            }
            _ => {
                let __arg4 @ _ = {
                    let __arg3 @ _ = crate::proof_std::aver_generated::kernel::types::partsOf(&t);
                    crate::proof_std::aver_generated::kernel::types::rigidAll(&__arg3)
                };
                crate::proof_std::aver_generated::kernel::types::rebuild(t, &__arg4)
            }
        }
    }
}

/// Each type with its variables fixed.
#[inline(always)]
pub fn rigidAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::types::rigid(t.clone()),
                    crate::proof_std::aver_generated::kernel::types::rigidAll(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The givens of a statement as its own proof sees them.
#[inline(always)]
pub fn rigidVars(
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = vs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    {
                        let __arg3 @ _ =
                            crate::proof_std::aver_generated::kernel::types::rigid(v.ty.clone());
                        crate::proof_std::aver_generated::kernel::term::typed(
                            v.name.clone(),
                            __arg3,
                        )
                    },
                    crate::proof_std::aver_generated::kernel::types::rigidVars(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// A declared name claims equality and all of its fields support it; a recursive return to a name already under that check is sound.
#[inline(always)]
pub fn namedHasEq(n @ _: AverStr, e @ _: &TyEnv, seen @ _: &aver_rt::AverList<AverStr>) -> bool {
    crate::proof_std::cancel_checkpoint();
    if seen.contains(&n) {
        true
    } else {
        {
            let __subject = (
                crate::proof_std::aver_generated::kernel::types::sumNamed(n.clone(), &e.sums),
                crate::proof_std::aver_generated::kernel::types::recNamed(n.clone(), &e.recs),
            );
            match __subject {
                (Some(s), _) => {
                    (s.eq && {
                        let __arg5 @ _ = aver_rt::AverList::prepend(n, seen);
                        crate::proof_std::aver_generated::kernel::types::variantsHaveEq(
                            &s.variants,
                            e,
                            &__arg5,
                        )
                    })
                }
                (_, Some(r)) => {
                    (r.eq && {
                        let __arg6 @ _ =
                            crate::proof_std::aver_generated::kernel::types::typesOfVars(&r.fields);
                        {
                            let __arg7 @ _ = aver_rt::AverList::prepend(n, seen);
                            crate::proof_std::aver_generated::kernel::types::allEqSeen(
                                &__arg6, e, &__arg7,
                            )
                        }
                    })
                }
                _ => false,
            }
        }
    }
}

/// Every field of every variant has equality.
#[inline(always)]
pub fn variantsHaveEq(
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Variant>,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = vs;
        if __list_subject.is_empty() {
            true
        } else {
            if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                (crate::proof_std::aver_generated::kernel::types::allEqSeen(&v.fields, e, seen)
                    && crate::proof_std::aver_generated::kernel::types::variantsHaveEq(
                        &rest, e, seen,
                    ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Whether every type has an equality.
#[inline(always)]
pub fn allEq(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    let __arg2 @ _ = <aver_rt::AverList<AverStr>>::empty();
    crate::proof_std::aver_generated::kernel::types::allEqSeen(ts, e, &__arg2)
}

/// Whether every type has equality while these recursive declarations are already being checked.
#[inline(always)]
pub fn allEqSeen(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            true
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                (crate::proof_std::aver_generated::kernel::types::hasEqSeen(t, e, seen)
                    && crate::proof_std::aver_generated::kernel::types::allEqSeen(&rest, e, seen))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The types of typed variables, in order.
#[inline(always)]
pub fn typesOfVars(
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = vs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    v.ty.clone(),
                    crate::proof_std::aver_generated::kernel::types::typesOfVars(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The parameter names of a definition.
#[inline(always)]
pub fn paramsOf(
    d @ _: &crate::proof_std::aver_generated::kernel::proof::Def,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::proof::givenNames(&d.params)
}

/// A declared sum by name.
#[inline(always)]
pub fn sumNamed(
    mut n @ _: AverStr,
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Sum> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum>;
    let mut ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum> = ss;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ss;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (s.name == n) {
                        return Some(s.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            ss = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A declared record by name.
#[inline(always)]
pub fn recNamed(
    mut n @ _: AverStr,
    rs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Rec> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec>;
    let mut rs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec> = rs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = rs;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(r), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (r.name == n) {
                        return Some(r.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            rs = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The declared sum a constructor belongs to: the one named by the constructor up to its last dot.
#[inline(always)]
pub fn sumOf(
    c @ _: AverStr,
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Sum> {
    crate::proof_std::cancel_checkpoint();
    let __arg2 @ _ = crate::proof_std::aver_generated::kernel::types::typeName(c);
    crate::proof_std::aver_generated::kernel::types::sumNamed(__arg2, ss)
}

/// A constructor's name up to its last dot.
#[inline(always)]
pub fn typeName(c @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    (aver_rt::string_join(
        &{
            let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1)));
            ((aver_rt::AverList::from_vec(
                c.split(&*::aver_rt::str_lit!("."))
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
            ))
            .into_aver()
            .reverse())
            .drop_first(__n)
        }
        .reverse(),
        &::aver_rt::str_lit!("."),
    ))
    .into_aver()
}

/// The field types of a constructor among a sum's variants.
#[inline(always)]
pub fn variantFields(
    mut c @ _: AverStr,
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Variant>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Variant>;
    let mut vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Variant> =
        vs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = vs;
            if __list_subject.is_empty() {
                return None;
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (v.name == c) {
                        return Some(v.fields.clone());
                    } else {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            vs = &__hold1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The field types of a constructor: a list's, Option's and Result's read off a subject of that type, any other off the declared sum, whatever the constructor is named.
pub fn ctorFields(
    c @ _: AverStr,
    subject @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __subject = (
            c.clone(),
            crate::proof_std::aver_generated::kernel::types::headOf(subject),
        );
        match __subject {
            (__str_lit0, __str_lit1)
                if str::eq(&__str_lit0, "nil") && str::eq(&__str_lit1, "List") =>
            {
                Ok(aver_rt::AverList::empty())
            }
            (__str_lit0, __str_lit1)
                if str::eq(&__str_lit0, "cons") && str::eq(&__str_lit1, "List") =>
            {
                Ok(aver_rt::AverList::from_vec(vec![
                    {
                        let __arg5 @ _ =
                            crate::proof_std::aver_generated::kernel::types::partsOf(subject);
                        crate::proof_std::aver_generated::kernel::types::nth(
                            __arg5,
                            aver_rt::AverInt::from_i64(0),
                        )
                    },
                    crate::proof_std::aver_generated::kernel::term::Ty::TyList(
                        crate::proof_std::aver_generated::kernel::term::Ty__TyList({
                            let __arg6 @ _ =
                                crate::proof_std::aver_generated::kernel::types::partsOf(subject);
                            crate::proof_std::aver_generated::kernel::types::nth(
                                __arg6,
                                aver_rt::AverInt::from_i64(0),
                            )
                        }),
                    ),
                ]))
            }
            (__str_lit0, __str_lit1)
                if str::eq(&__str_lit0, "Option.None") && str::eq(&__str_lit1, "Option") =>
            {
                Ok(aver_rt::AverList::empty())
            }
            (__str_lit0, __str_lit1)
                if str::eq(&__str_lit0, "Option.Some") && str::eq(&__str_lit1, "Option") =>
            {
                Ok(aver_rt::AverList::from_vec(vec![{
                    let __arg7 @ _ =
                        crate::proof_std::aver_generated::kernel::types::partsOf(subject);
                    crate::proof_std::aver_generated::kernel::types::nth(
                        __arg7,
                        aver_rt::AverInt::from_i64(0),
                    )
                }]))
            }
            (__str_lit0, __str_lit1)
                if str::eq(&__str_lit0, "Result.Ok") && str::eq(&__str_lit1, "Result") =>
            {
                Ok(aver_rt::AverList::from_vec(vec![{
                    let __arg8 @ _ =
                        crate::proof_std::aver_generated::kernel::types::partsOf(subject);
                    crate::proof_std::aver_generated::kernel::types::nth(
                        __arg8,
                        aver_rt::AverInt::from_i64(0),
                    )
                }]))
            }
            (__str_lit0, __str_lit1)
                if str::eq(&__str_lit0, "Result.Err") && str::eq(&__str_lit1, "Result") =>
            {
                Ok(aver_rt::AverList::from_vec(vec![{
                    let __arg9 @ _ =
                        crate::proof_std::aver_generated::kernel::types::partsOf(subject);
                    crate::proof_std::aver_generated::kernel::types::nth(
                        __arg9,
                        aver_rt::AverInt::from_i64(1),
                    )
                }]))
            }
            _ => match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) {
                None => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("unknown constructor "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                    __b
                })),
                Some(s @ _) => {
                    match crate::proof_std::aver_generated::kernel::types::variantFields(
                        c.clone(),
                        &s.variants,
                    ) {
                        None => Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("unknown constructor "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                            __b
                        })),
                        Some(fs @ _) => Ok(fs),
                    }
                }
            },
        }
    }
}

/// What a list holds, if the type is a list.
pub fn elemOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
) -> Option<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = &__a_fields.0;
                Some(a.clone())
            }
            _ => None,
        }
    }
}

/// The constructors of a type in order, as a split names them: nil and cons for a list, Option's, Result's, or a declared sum's; none for any other type.
pub fn ctorsOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = __a_fields.0.clone();
                aver_rt::AverList::from_vec(vec![
                    ::aver_rt::str_lit!("nil"),
                    ::aver_rt::str_lit!("cons"),
                ])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyOption(__a_fields) => {
                let a = __a_fields.0.clone();
                aver_rt::AverList::from_vec(vec![
                    ::aver_rt::str_lit!("Option.None"),
                    ::aver_rt::str_lit!("Option.Some"),
                ])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyResult(__a_fields) => {
                let a = __a_fields.0.clone();
                let b = __a_fields.1.clone();
                aver_rt::AverList::from_vec(vec![
                    ::aver_rt::str_lit!("Result.Err"),
                    ::aver_rt::str_lit!("Result.Ok"),
                ])
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyNamed(__n_fields) => {
                let n = __n_fields.0.clone();
                match crate::proof_std::aver_generated::kernel::types::sumNamed(n, &e.sums) {
                    Some(s @ _) => {
                        crate::proof_std::aver_generated::kernel::proof::variantNames(&s.variants)
                    }
                    None => aver_rt::AverList::empty(),
                }
            }
            _ => aver_rt::AverList::empty(),
        }
    }
}

/// The type of a term, from its leaves up; an ill-typed term is refused with the first part that does not fit.
pub fn typeOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Term__view::TInt(__n_fields) => {
                let n = __n_fields.0.clone();
                Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyInt)
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBool(__b_fields) => {
                let b = __b_fields.0.clone();
                Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyBool)
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TStr(__s_fields) => {
                let s = __s_fields.0.clone();
                Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyStr)
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TUnit => {
                Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyUnit)
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TVar(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::proof_std::aver_generated::kernel::types::varType(n, e)
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::THole => {
                Err(::aver_rt::str_lit!("the hole has no type"))
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TGet(__r_fields) => {
                let r = &__r_fields.0;
                let f = __r_fields.1.clone();
                {
                    let __arg28 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typeOf(r, e)?;
                    crate::proof_std::aver_generated::kernel::types::fieldType(&__arg28, f, e)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCall(__f_fields) => {
                let f = __f_fields.0.clone();
                let xs = &__f_fields.1;
                {
                    let __arg29 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typesOf(xs, e)?;
                    crate::proof_std::aver_generated::kernel::types::callType(f, &__arg29, e)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TBi(__n_fields) => {
                let n = __n_fields.0.clone();
                let xs = &__n_fields.1;
                {
                    let __arg30 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typesOf(xs, e)?;
                    crate::proof_std::aver_generated::kernel::types::builtinType(n, &__arg30)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TOp(__o_fields) => {
                let o = __o_fields.0.clone();
                let a = &__o_fields.1;
                let b = &__o_fields.2;
                {
                    let __arg31 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typeOf(a, e)?;
                    {
                        let __arg32 @ _ =
                            crate::proof_std::aver_generated::kernel::types::typeOf(b, e)?;
                        crate::proof_std::aver_generated::kernel::types::opType(o, __arg31, __arg32)
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TNeg(__a_fields) => {
                let a = &__a_fields.0;
                {
                    let __arg33 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typeOf(a, e)?;
                    {
                        let __arg34 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyInt;
                        crate::proof_std::aver_generated::kernel::types::must(
                            &__arg33,
                            __arg34,
                            ::aver_rt::str_lit!("a negation"),
                        )
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TCtor(__c_fields) => {
                let c = __c_fields.0.clone();
                let xs = &__c_fields.1;
                {
                    let __arg35 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typesOf(xs, e)?;
                    crate::proof_std::aver_generated::kernel::types::ctorType(c, &__arg35, e)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TMatch(__s_fields) => {
                let s = &__s_fields.0;
                let arms = &__s_fields.1;
                {
                    let __list_subject = arms;
                    if __list_subject.is_empty() {
                        Err(::aver_rt::str_lit!("a match with no arms has no type"))
                    } else {
                        {
                            let __arg36 @ _ =
                                crate::proof_std::aver_generated::kernel::types::typeOf(s, e)?;
                            {
                                let __arg37 @ _ =
                                    crate::proof_std::aver_generated::kernel::term::Ty::TyAny;
                                crate::proof_std::aver_generated::kernel::types::armsType(
                                    __arg36, arms, __arg37, e,
                                )
                            }
                        }
                    }
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TParts(__xs_fields) => {
                let xs = &__xs_fields.0;
                {
                    let __arg38 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typesOf(xs, e)?;
                    crate::proof_std::aver_generated::kernel::types::partsType(&__arg38)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TList(__xs_fields) => {
                let xs = &__xs_fields.0;
                Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyList(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyList({
                        let __arg39 @ _ =
                            crate::proof_std::aver_generated::kernel::types::typesOf(xs, e)?;
                        {
                            let __arg40 @ _ =
                                crate::proof_std::aver_generated::kernel::term::Ty::TyAny;
                            crate::proof_std::aver_generated::kernel::types::joinAll(
                                &__arg39, __arg40,
                            )
                        }
                    }?),
                ))
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyTuple(
                        crate::proof_std::aver_generated::kernel::types::typesOf(xs, e)?,
                    ),
                ))
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TRec(__n_fields) => {
                let n = __n_fields.0.clone();
                let fs = &__n_fields.1;
                crate::proof_std::aver_generated::kernel::types::recordLiteralType(n, fs, e)
            }
            crate::proof_std::aver_generated::kernel::term::Term__view::TUpd(__n_fields) => {
                let n = __n_fields.0.clone();
                let b = &__n_fields.1;
                let fs = &__n_fields.2;
                {
                    let __arg44 @ _ = {
                        let __arg41 @ _ =
                            crate::proof_std::aver_generated::kernel::types::typeOf(b, e)?;
                        {
                            let __arg42 @ _ =
                                crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(
                                    crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(
                                        n.clone(),
                                    ),
                                );
                            {
                                let __arg43 @ _ = aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(29))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("an update of "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(n),
                                    )));
                                    __b
                                });
                                crate::proof_std::aver_generated::kernel::types::must(
                                    &__arg41, __arg42, __arg43,
                                )
                            }
                        }
                    }?;
                    crate::proof_std::aver_generated::kernel::types::fieldsFit(n, fs, __arg44, e)
                }
            }
        }
    }
}

/// The declared type of a variable or module-level binding.
#[inline(always)]
pub fn varType(
    n @ _: AverStr,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::types::find(n.clone(), &e.vars) {
        Some(t @ _) => Ok(t),
        None => {
            match crate::proof_std::aver_generated::kernel::types::constType(n.clone(), &e.consts) {
                Some(t @ _) => Ok(t),
                None => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is not a variable in scope"));
                    __b
                })),
            }
        }
    }
}

/// The type of each term.
#[inline(always)]
pub fn typesOf(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Term>,
    e @ _: &TyEnv,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            Ok(aver_rt::AverList::empty())
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::types::typeOf(t, e)?,
                    crate::proof_std::aver_generated::kernel::types::typesOf(&rest, e)?,
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// The expected type, once the actual one fits it.
#[inline(always)]
pub fn must(
    actual @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    mut expected @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    what @ _: AverStr,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if crate::proof_std::aver_generated::kernel::types::fits(actual.clone(), expected.clone()) {
        Ok(expected)
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(61)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(what))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" takes "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(&expected)),
                    )));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(", not "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::types::show(actual)),
            )));
            __b
        }))
    }
}

/// The type of a field of a declared record.
pub fn fieldType(
    r @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    f @ _: AverStr,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = r;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyNamed(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::proof_std::aver_generated::kernel::types::recField(n, f, e)
            }
            _ => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(r)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" has no field "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                __b
            })),
        }
    }
}

/// The type of a field of a declared record, by name.
#[inline(always)]
pub fn recField(
    n @ _: AverStr,
    f @ _: AverStr,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::types::recNamed(n.clone(), &e.recs) {
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(" is not a declared record"));
            __b
        })),
        Some(rec @ _) => {
            match crate::proof_std::aver_generated::kernel::types::find(f.clone(), &rec.fields) {
                None => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(" has no field "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                    __b
                })),
                Some(t @ _) => Ok(t),
            }
        }
    }
}

/// The type of a complete record literal: every declared field exactly once, at its declared type.
pub fn recordLiteralType(
    n @ _: AverStr,
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let r @ _ = crate::proof_std::aver_generated::kernel::types::recordDecl(n.clone(), e)?;
    {
        let __arg5 @ _ = crate::proof_std::aver_generated::kernel::types::fieldNames(fs);
        crate::proof_std::aver_generated::kernel::types::requiredFieldsPresent(
            n.clone(),
            &r.fields,
            __arg5,
        )
    }?;
    let __arg6 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(
        crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(n.clone()),
    );
    crate::proof_std::aver_generated::kernel::types::fieldsFit(n, fs, __arg6, e)
}

/// The declaration of a record literal's named type.
#[inline(always)]
pub fn recordDecl(
    n @ _: AverStr,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::proof::Rec, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::types::recNamed(n.clone(), &e.recs) {
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(" is not a declared record"));
            __b
        })),
        Some(r @ _) => Ok(r),
    }
}

/// The names written by a record literal or update, in order.
#[inline(always)]
pub fn fieldNames(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    f.name.clone(),
                    crate::proof_std::aver_generated::kernel::types::fieldNames(&rest),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Every field declared by record n occurs in the literal.
#[inline(always)]
pub fn requiredFieldsPresent(
    mut n @ _: AverStr,
    required @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut actual @ _: aver_rt::AverList<AverStr>,
) -> Result<(), AverStr> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>;
    let mut required @ _: &aver_rt::AverList<
        crate::proof_std::aver_generated::kernel::term::TypedVar,
    > = required;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = required;
            if __list_subject.is_empty() {
                return Ok(());
            } else {
                if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if actual.contains(&f.name) {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            required = &__hold1;
                            continue;
                        }
                    } else {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(57))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!("record "));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(n),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" is missing field "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f.name))));
                            __b
                        }));
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The one check that a namespace gives each name one identity: a repeated name is refused as what, then the name.
#[inline(always)]
pub fn unique(
    names @ _: &aver_rt::AverList<AverStr>,
    mut seen @ _: aver_rt::AverList<AverStr>,
    mut what @ _: AverStr,
) -> Result<(), AverStr> {
    let mut __hold0: aver_rt::AverList<AverStr>;
    let mut names @ _: &aver_rt::AverList<AverStr> = names;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = names;
            if __list_subject.is_empty() {
                return Ok(());
            } else {
                if let Some((n, rest)) = aver_rt::list_uncons_cloned(__list_subject) {
                    if seen.contains(&n) {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(what),
                                    )));
                                    __b
                                };
                                __b.push_str(&::aver_rt::str_lit!(" "));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                            __b
                        }));
                    } else {
                        {
                            let __tco0 = rest;
                            let __tco1 = aver_rt::AverList::prepend_owned(n, seen);
                            __hold0 = __tco0;
                            names = &__hold0;
                            seen = __tco1;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Every record declaration gives each field name one identity, using the shared uniqueness check.
#[inline(always)]
pub fn recordFieldsUnique(
    rs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec>,
    mut path @ _: AverStr,
) -> Result<(), AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec>;
    let mut rs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec> = rs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = rs;
            if __list_subject.is_empty() {
                return Ok(());
            } else {
                if let (Some(r), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __subject = {
                            let __arg6 @ _ =
                                crate::proof_std::aver_generated::kernel::proof::givenNames(
                                    &r.fields,
                                );
                            {
                                let __arg7 @ _ = <aver_rt::AverList<AverStr>>::empty();
                                {
                                    let __arg8 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(60))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&::aver_rt::str_lit!("step "));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(path)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(": record "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(r.name)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" repeats field"));
                                        __b
                                    });
                                    crate::proof_std::aver_generated::kernel::types::unique(
                                        &__arg6, __arg7, __arg8,
                                    )
                                }
                            }
                        };
                        match __subject {
                            Err(why @ _) => {
                                return Err(why);
                            }
                            Ok(done @ _) => {
                                let __tco0 = rest;
                                __hold0 = __tco0;
                                rs = &__hold0;
                                continue;
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A record literal or update writes each field once and every value fits its declared type.
pub fn fieldsFit(
    n @ _: AverStr,
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    mut result @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __arg5 @ _ = crate::proof_std::aver_generated::kernel::types::fieldNames(fs);
        {
            let __arg6 @ _ = <aver_rt::AverList<AverStr>>::empty();
            {
                let __arg7 @ _ = aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(37)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&::aver_rt::str_lit!("record "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" repeats field"));
                    __b
                });
                crate::proof_std::aver_generated::kernel::types::unique(&__arg5, __arg6, __arg7)
            }
        }
    }?;
    crate::proof_std::aver_generated::kernel::types::fieldTypesFit(n, fs, result, e)
}

/// Each written field value fits the field's declared type.
#[inline(always)]
pub fn fieldTypesFit(
    mut n @ _: AverStr,
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    mut result @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>;
    let mut fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field> = fs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = fs;
            if __list_subject.is_empty() {
                return Ok(result);
            } else {
                if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if {
                        let __arg7 @ _ =
                            crate::proof_std::aver_generated::kernel::types::typeOf(&f.value, e)?;
                        {
                            let __arg8 @ _ = {
                                let __arg6 @ _ =
                                    crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(
                                        crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(
                                            n.clone(),
                                        ),
                                    );
                                crate::proof_std::aver_generated::kernel::types::fieldType(
                                    &__arg6,
                                    f.name.clone(),
                                    e,
                                )
                            }?;
                            crate::proof_std::aver_generated::kernel::types::fits(__arg7, __arg8)
                        }
                    } {
                        {
                            let __tco1 = rest;
                            __hold1 = __tco1;
                            fs = &__hold1;
                            continue;
                        }
                    } else {
                        return Err(aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(64))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&::aver_rt::str_lit!("field "));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(f.name)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" of "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" does not fit its type"));
                            __b
                        }));
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A definition's result, once the arguments fit its parameters.
#[inline(always)]
pub fn callType(
    f @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::proof::findDef(f.clone(), &e.defs) {
        None => {
            crate::proof_std::aver_generated::kernel::types::sigType(f, args.clone(), &e.sigs, e)
        }
        Some(d @ _) => {
            let __subject = {
                let __arg6 @ _ =
                    crate::proof_std::aver_generated::kernel::types::typesOfVars(&d.params);
                {
                    let __arg7 @ _ = <aver_rt::AverList<
                        crate::proof_std::aver_generated::kernel::term::TypedVar,
                    >>::empty();
                    {
                        let __arg8 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(":"));
                            __b
                        });
                        crate::proof_std::aver_generated::kernel::types::bindEach(
                            &__arg6,
                            args.clone(),
                            __arg7,
                            e,
                            __arg8,
                        )
                    }
                }
            };
            match __subject {
                Ok(bound @ _) => Ok(d.ret),
                Err(why @ _) => Err(why),
            }
        }
    }
}

/// The result of a function the script only names, by its declared signature.
#[inline(always)]
pub fn sigType(
    mut f @ _: AverStr,
    mut args @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    sigs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::FnSig>,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    let mut __hold2: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::FnSig>;
    let mut sigs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::FnSig> =
        sigs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = sigs;
            if __list_subject.is_empty() {
                return Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(32)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&::aver_rt::str_lit!("no signature of "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                    __b
                }));
            } else {
                if let (Some(s), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (s.name == f) {
                        {
                            let __subject = {
                                let __arg8 @ _ = <aver_rt::AverList<
                                    crate::proof_std::aver_generated::kernel::term::TypedVar,
                                >>::empty();
                                {
                                    let __arg9 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(17))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(f)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(":"));
                                        __b
                                    });
                                    crate::proof_std::aver_generated::kernel::types::bindEach(
                                        &s.params, args, __arg8, e, __arg9,
                                    )
                                }
                            };
                            match __subject {
                                Ok(bound @ _) => {
                                    return Ok(s.ret.clone());
                                }
                                Err(why @ _) => {
                                    return Err(why);
                                }
                            }
                        }
                    } else {
                        {
                            let __tco2 = rest;
                            __hold2 = __tco2;
                            sigs = &__hold2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// A builtin's result at these argument types, its type variables bound by them.
#[inline(always)]
pub fn builtinType(
    n @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::builtins::sig(n.clone()) {
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(32)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("unknown builtin "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
            __b
        })),
        Some(s @ _) => Ok({
            let __arg6 @ _ = {
                let __arg3 @ _ = <aver_rt::AverList<
                    crate::proof_std::aver_generated::kernel::term::TypedVar,
                >>::empty();
                {
                    let __arg4 @ _ = crate::proof_std::aver_generated::kernel::types::empty();
                    {
                        let __arg5 @ _ = aver_rt::AverStr::from({
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(":"));
                            __b
                        });
                        crate::proof_std::aver_generated::kernel::types::bindEach(
                            &s.params,
                            args.clone(),
                            __arg3,
                            &__arg4,
                            __arg5,
                        )
                    }
                }
            }?;
            crate::proof_std::aver_generated::kernel::types::instantiate(s.ret, &__arg6)
        }),
    }
}

/// Each actual type fits its expected one, binding the expected side's type variables in order.
pub fn bindEach(
    expected @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut actual @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut bound @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    mut what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>;
    let mut expected @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> =
        expected;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __int_match_subject = (expected.clone(), actual);
            let (__lit0, __lit1) = &__int_match_subject;
            if (*__lit0).is_empty() && (*__lit1).is_empty() {
                return Ok(bound);
            } else if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
                let Some((x, xs)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                let Some((y, ys)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                    unreachable!("Aver Rust codegen: tuple element list mismatch")
                };
                {
                    let __tco0 = xs;
                    let __tco1 = ys;
                    let __tco2 = crate::proof_std::aver_generated::kernel::types::bindVars(
                        &x,
                        y,
                        &bound,
                        e,
                        what.clone(),
                    )?;
                    __hold0 = __tco0;
                    expected = &__hold0;
                    actual = __tco1;
                    bound = __tco2;
                    continue;
                }
            } else {
                return Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(49)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(what),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" takes "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(aver_rt::AverInt::from_i64(expected.len() as i64)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" arguments"));
                    __b
                }));
            }
        }
    }
}

/// An actual type against an expected one that may hold type variables: a variable takes the actual type, joined with what it already holds; the rest must fit part by part.
pub fn bindVars(
    expected @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    mut actual @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    bound @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = expected;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                crate::proof_std::aver_generated::kernel::types::bindVar(
                    n, eq, actual, bound, e, what,
                )
            }
            _ => crate::proof_std::aver_generated::kernel::types::bindShape(
                expected, &actual, bound, e, what,
            ),
        }
    }
}

/// Two types of one shape, bound part by part; a hole fits anything.
pub fn bindShape(
    expected @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    actual @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    bound @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __subject = (
            ((&*crate::proof_std::aver_generated::kernel::types::headOf(actual) == "_")
                || (&*crate::proof_std::aver_generated::kernel::types::headOf(expected) == "_")),
            ((crate::proof_std::aver_generated::kernel::types::headOf(expected)
                == crate::proof_std::aver_generated::kernel::types::headOf(actual))
                && (aver_rt::AverInt::from_i64(
                    crate::proof_std::aver_generated::kernel::types::partsOf(expected).len() as i64,
                ) == aver_rt::AverInt::from_i64(
                    crate::proof_std::aver_generated::kernel::types::partsOf(actual).len() as i64,
                ))),
        );
        match __subject {
            (true, _) => Ok(bound.clone()),
            (_, true) => {
                let __arg5 @ _ = crate::proof_std::aver_generated::kernel::types::partsOf(expected);
                {
                    let __arg6 @ _ =
                        crate::proof_std::aver_generated::kernel::types::partsOf(actual);
                    crate::proof_std::aver_generated::kernel::types::bindEach(
                        &__arg5,
                        __arg6,
                        bound.clone(),
                        e,
                        what,
                    )
                }
            }
            _ => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(57)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(what),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(crate::proof_std::aver_generated::kernel::types::show(actual)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" is not "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_std::aver_generated::kernel::types::show(expected)),
                )));
                __b
            })),
        }
    }
}

/// A type variable takes the actual type, joined with what it already holds; one marked as needing equality refuses a type whose == is not equality.
#[inline(always)]
pub fn bindVar(
    n @ _: AverStr,
    eq @ _: bool,
    mut actual @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    bound @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if (eq && (!crate::proof_std::aver_generated::kernel::types::hasEq(&actual, e))) {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(55)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(what))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" == on "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_std::aver_generated::kernel::types::show(&actual)),
                )));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(" is not equality"));
            __b
        }))
    } else {
        match crate::proof_std::aver_generated::kernel::types::find(n.clone(), bound) {
            None => Ok(aver_rt::AverList::prepend(
                crate::proof_std::aver_generated::kernel::term::typed(n, actual),
                bound,
            )),
            Some(prev @ _) => {
                match crate::proof_std::aver_generated::kernel::types::join(prev, actual) {
                    Ok(t @ _) => Ok(aver_rt::AverList::prepend(
                        crate::proof_std::aver_generated::kernel::term::typed(n, t),
                        bound,
                    )),
                    Err(why @ _) => Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(what),
                                )));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
                        __b
                    })),
                }
            }
        }
    }
}

/// A type with its variables replaced by what they are bound to; an unbound one is a hole.
pub fn instantiate(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    bound @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = &t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(__n_fields) => {
                let n = __n_fields.0.clone();
                let eq = __n_fields.1.clone();
                crate::proof_std::aver_generated::kernel::types::find(n, bound)
                    .unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny)
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyList(__a_fields) => {
                let a = &__a_fields.0;
                crate::proof_std::aver_generated::kernel::term::Ty::TyList(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyList(
                        crate::proof_std::aver_generated::kernel::types::instantiate(
                            a.clone(),
                            bound,
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyOption(__a_fields) => {
                let a = &__a_fields.0;
                crate::proof_std::aver_generated::kernel::term::Ty::TyOption(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyOption(
                        crate::proof_std::aver_generated::kernel::types::instantiate(
                            a.clone(),
                            bound,
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVector(__a_fields) => {
                let a = &__a_fields.0;
                crate::proof_std::aver_generated::kernel::term::Ty::TyVector(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyVector(
                        crate::proof_std::aver_generated::kernel::types::instantiate(
                            a.clone(),
                            bound,
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyResult(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::proof_std::aver_generated::kernel::term::Ty::TyResult(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyResult(
                        crate::proof_std::aver_generated::kernel::types::instantiate(
                            a.clone(),
                            bound,
                        ),
                        crate::proof_std::aver_generated::kernel::types::instantiate(
                            b.clone(),
                            bound,
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyMap(__a_fields) => {
                let a = &__a_fields.0;
                let b = &__a_fields.1;
                crate::proof_std::aver_generated::kernel::term::Ty::TyMap(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyMap(
                        crate::proof_std::aver_generated::kernel::types::instantiate(
                            a.clone(),
                            bound,
                        ),
                        crate::proof_std::aver_generated::kernel::types::instantiate(
                            b.clone(),
                            bound,
                        ),
                    ),
                )
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyTuple(
                        crate::proof_std::aver_generated::kernel::types::instantiateAll(xs, bound),
                    ),
                )
            }
            _ => t,
        }
    }
}

/// Each type instantiated.
#[inline(always)]
pub fn instantiateAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    bound @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            aver_rt::AverList::empty()
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                aver_rt::AverList::prepend_owned(
                    crate::proof_std::aver_generated::kernel::types::instantiate(t.clone(), bound),
                    crate::proof_std::aver_generated::kernel::types::instantiateAll(&rest, bound),
                )
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// An operator's result at the one type of its operands: == and != compare any two values; an order takes two Int, Float or texts; + adds two of those, and - * / two Int or Float. Which of them a step may read as integer arithmetic is that step's business.
pub fn opType(
    o @ _: AverStr,
    mut a @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    mut b @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let t @ _ = crate::proof_std::aver_generated::kernel::types::operands(o.clone(), a, b)?;
    {
        let __subject = (
            aver_rt::AverList::from_vec(vec![::aver_rt::str_lit!("=="), ::aver_rt::str_lit!("!=")])
                .contains(&o),
            aver_rt::AverList::from_vec(vec![
                ::aver_rt::str_lit!("<"),
                ::aver_rt::str_lit!("<="),
                ::aver_rt::str_lit!(">"),
                ::aver_rt::str_lit!(">="),
            ])
            .contains(&o),
            aver_rt::AverList::from_vec(vec![
                ::aver_rt::str_lit!("+"),
                ::aver_rt::str_lit!("-"),
                ::aver_rt::str_lit!("*"),
                ::aver_rt::str_lit!("/"),
            ])
            .contains(&o),
        );
        match __subject {
            (true, _, _) => Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyBool),
            (_, true, _) => {
                let __arg4 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyBool;
                crate::proof_std::aver_generated::kernel::types::numeric(o, &t, true, __arg4)
            }
            (_, _, true) => {
                let __arg5 @ _ = (&*o == "+");
                crate::proof_std::aver_generated::kernel::types::numeric(o, &t.clone(), __arg5, t)
            }
            _ => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("unknown operator "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o))));
                __b
            })),
        }
    }
}

/// The one type both operands have.
#[inline(always)]
pub fn operands(
    o @ _: AverStr,
    mut a @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    mut b @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::types::join(a, b) {
        Ok(t @ _) => Ok(t),
        Err(why @ _) => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(63)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o))));
                    __b
                };
                __b.push_str(&::aver_rt::str_lit!(" takes two values of one type: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(why))));
            __b
        })),
    }
}

/// The result, once the operands are Int or Float, or texts where the operator takes them.
pub fn numeric(
    o @ _: AverStr,
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    texts @ _: bool,
    mut result @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = (t.clone(), texts);
        match (__t.0.view(), &__t.1) {
            (crate::proof_std::aver_generated::kernel::term::Ty__view::TyInt, _) => Ok(result),
            (crate::proof_std::aver_generated::kernel::term::Ty__view::TyFloat, _) => Ok(result),
            (crate::proof_std::aver_generated::kernel::term::Ty__view::TyAny, _) => Ok(result),
            (crate::proof_std::aver_generated::kernel::term::Ty__view::TyStr, true) => Ok(result),
            _ => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o))));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" does not take "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_std::aver_generated::kernel::types::show(t)),
                )));
                __b
            })),
        }
    }
}

/// A constructor application: Option and Result by their builtin shape, a declared constructor by its sum.
pub fn ctorType(
    c @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let (__pat0, __pat1) = (c.clone(), args.clone());
        {
            let __dispatch_subject = __pat0;
            if &*__dispatch_subject == "Option.None" {
                {
                    let __list_subject = __pat1;
                    if __list_subject.is_empty() {
                        Ok(
                            crate::proof_std::aver_generated::kernel::term::Ty::TyOption(
                                crate::proof_std::aver_generated::kernel::term::Ty__TyOption(
                                    crate::proof_std::aver_generated::kernel::term::Ty::TyAny,
                                ),
                            ),
                        )
                    } else {
                        match crate::proof_std::aver_generated::kernel::types::sumOf(
                            c.clone(),
                            &e.sums,
                        ) {
                            None => Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("unknown constructor "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                                __b
                            })),
                            Some(s @ _) => {
                                match crate::proof_std::aver_generated::kernel::types::variantFields(
                                    c.clone(),
                                    &s.variants,
                                ) {
                                    None => Err(aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = aver_rt::Buffer::with_capacity(
                                                (aver_rt::AverInt::from_i64(36))
                                                    .to_usize()
                                                    .unwrap_or(0),
                                            );
                                            __b.push_str(&::aver_rt::str_lit!(
                                                "unknown constructor "
                                            ));
                                            __b
                                        };
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(c)),
                                        ));
                                        __b
                                    })),
                                    Some(fs @ _) => {
                                        Ok({
                                            let __arg29 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name));
                                            {
                                                let __arg30 @ _ = {
                                                    let __arg27 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty();
                                                    {
                                                        let __arg28 @ _ = aver_rt::AverStr::from({
                                                            let mut __b = {
                                                                let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0));
                                                                __b.push_str(
                                                                    &aver_rt::AverStr::from(
                                                                        aver_rt::aver_display(&(c)),
                                                                    ),
                                                                );
                                                                __b
                                                            };
                                                            __b.push_str(&::aver_rt::str_lit!(":"));
                                                            __b
                                                        });
                                                        crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg27, e, __arg28)
                                                    }
                                                }?;
                                                crate::proof_std::aver_generated::kernel::types::instantiate(__arg29, &__arg30)
                                            }
                                        })
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                if &*__dispatch_subject == "Option.Some" {
                    {
                        let __list_subject = __pat1;
                        if let (Some(a), Some(__pat2)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyOption(crate::proof_std::aver_generated::kernel::term::Ty__TyOption(a.clone())))
                                } else {
                                    match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), &s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok({ let __arg33 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name)); { let __arg34 @ _ = { let __arg31 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty(); { let __arg32 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&::aver_rt::str_lit!(":")); __b }); crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg31, e, __arg32) } }?; crate::proof_std::aver_generated::kernel::types::instantiate(__arg33, &__arg34) } }) } } } }
                                }
                            }
                        } else {
                            match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), &s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok({ let __arg37 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name)); { let __arg38 @ _ = { let __arg35 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty(); { let __arg36 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&::aver_rt::str_lit!(":")); __b }); crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg35, e, __arg36) } }?; crate::proof_std::aver_generated::kernel::types::instantiate(__arg37, &__arg38) } }) } } } }
                        }
                    }
                } else {
                    if &*__dispatch_subject == "Result.Ok" {
                        {
                            let __list_subject = __pat1;
                            if let (Some(a), Some(__pat3)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyResult(crate::proof_std::aver_generated::kernel::term::Ty__TyResult(a.clone(), crate::proof_std::aver_generated::kernel::term::Ty::TyAny)))
                                    } else {
                                        match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), &s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok({ let __arg41 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name)); { let __arg42 @ _ = { let __arg39 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty(); { let __arg40 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&::aver_rt::str_lit!(":")); __b }); crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg39, e, __arg40) } }?; crate::proof_std::aver_generated::kernel::types::instantiate(__arg41, &__arg42) } }) } } } }
                                    }
                                }
                            } else {
                                match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), &s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok({ let __arg45 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name)); { let __arg46 @ _ = { let __arg43 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty(); { let __arg44 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&::aver_rt::str_lit!(":")); __b }); crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg43, e, __arg44) } }?; crate::proof_std::aver_generated::kernel::types::instantiate(__arg45, &__arg46) } }) } } } }
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "Result.Err" {
                            {
                                let __list_subject = __pat1;
                                if let (Some(a), Some(__pat4)) =
                                    (__list_subject.first(), __list_subject.tail())
                                {
                                    {
                                        let __list_subject = __pat4;
                                        if __list_subject.is_empty() {
                                            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyResult(crate::proof_std::aver_generated::kernel::term::Ty__TyResult(crate::proof_std::aver_generated::kernel::term::Ty::TyAny, a.clone())))
                                        } else {
                                            match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), &s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok({ let __arg49 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name)); { let __arg50 @ _ = { let __arg47 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty(); { let __arg48 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&::aver_rt::str_lit!(":")); __b }); crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg47, e, __arg48) } }?; crate::proof_std::aver_generated::kernel::types::instantiate(__arg49, &__arg50) } }) } } } }
                                        }
                                    }
                                } else {
                                    match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), &s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok({ let __arg53 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name)); { let __arg54 @ _ = { let __arg51 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty(); { let __arg52 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&::aver_rt::str_lit!(":")); __b }); crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg51, e, __arg52) } }?; crate::proof_std::aver_generated::kernel::types::instantiate(__arg53, &__arg54) } }) } } } }
                                }
                            }
                        } else {
                            match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), &s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&::aver_rt::str_lit!("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok({ let __arg57 @ _ = crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name)); { let __arg58 @ _ = { let __arg55 @ _ = <aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>>::empty(); { let __arg56 @ _ = aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&::aver_rt::str_lit!(":")); __b }); crate::proof_std::aver_generated::kernel::types::bindEach(&fs, args.clone(), __arg55, e, __arg56) } }?; crate::proof_std::aver_generated::kernel::types::instantiate(__arg57, &__arg58) } }) } } } }
                        }
                    }
                }
            }
        }
    }
}

/// A text made of parts is a text, whatever the parts are.
pub fn partsType(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyStr)
}

/// The variables a pattern binds over a subject of this type, each with its type; a wildcard part binds nothing.
pub fn patVars(
    mut p @ _: crate::proof_std::aver_generated::kernel::term::Pat,
    mut subject @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __subject = (
            p,
            crate::proof_std::aver_generated::kernel::types::headOf(&subject),
        );
        match __subject {
            (crate::proof_std::aver_generated::kernel::term::Pat::PWild, _) => {
                Ok(aver_rt::AverList::empty())
            }
            (crate::proof_std::aver_generated::kernel::term::Pat::PVar(n), _) => Ok({
                let __arg10 @ _ = aver_rt::AverList::from_vec(vec![n]);
                {
                    let __arg11 @ _ = aver_rt::AverList::from_vec(vec![subject]);
                    crate::proof_std::aver_generated::kernel::types::namedVars(&__arg10, &__arg11)
                }
            }),
            (crate::proof_std::aver_generated::kernel::term::Pat::PLit(k), _) => {
                let __arg12 @ _ = crate::proof_std::aver_generated::kernel::types::typeOf(&k, e)?;
                crate::proof_std::aver_generated::kernel::types::litFits(__arg12, &subject)
            }
            (crate::proof_std::aver_generated::kernel::term::Pat::PNil, __str_lit0)
                if str::eq(&__str_lit0, "List") =>
            {
                Ok(aver_rt::AverList::empty())
            }
            (crate::proof_std::aver_generated::kernel::term::Pat::PCons(h, t), __str_lit0)
                if str::eq(&__str_lit0, "List") =>
            {
                Ok({
                    let __arg13 @ _ = aver_rt::AverList::from_vec(vec![h, t]);
                    {
                        let __arg14 @ _ =
                            crate::proof_std::aver_generated::kernel::types::ctorFields(
                                ::aver_rt::str_lit!("cons"),
                                &subject,
                                e,
                            )?;
                        crate::proof_std::aver_generated::kernel::types::namedVars(
                            &__arg13, &__arg14,
                        )
                    }
                })
            }
            (crate::proof_std::aver_generated::kernel::term::Pat::PTuple(ps), __str_lit0)
                if str::eq(&__str_lit0, "tuple") =>
            {
                let __arg15 @ _ =
                    crate::proof_std::aver_generated::kernel::types::partsOf(&subject);
                crate::proof_std::aver_generated::kernel::types::patVarsAll(&ps, &__arg15, e)
            }
            (crate::proof_std::aver_generated::kernel::term::Pat::PCtor(c, ns), _) => {
                crate::proof_std::aver_generated::kernel::types::ctorPatVars(c, &ns, &subject, e)
            }
            _ => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("the pattern does not fit "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_std::aver_generated::kernel::types::show(&subject)),
                )));
                __b
            })),
        }
    }
}

/// A constructor pattern binds exactly one name, including _, for each constructor field.
pub fn ctorPatVars(
    c @ _: AverStr,
    ns @ _: &aver_rt::AverList<AverStr>,
    subject @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let fs @ _ =
        crate::proof_std::aver_generated::kernel::types::ctorFields(c.clone(), subject, e)?;
    if (aver_rt::AverInt::from_i64(ns.len() as i64) == aver_rt::AverInt::from_i64(fs.len() as i64))
    {
        Ok(crate::proof_std::aver_generated::kernel::types::namedVars(
            ns, &fs,
        ))
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = {
                                    let mut __b = aver_rt::Buffer::with_capacity(
                                        (aver_rt::AverInt::from_i64(93)).to_usize().unwrap_or(0),
                                    );
                                    __b.push_str(&::aver_rt::str_lit!("constructor pattern "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                                __b
                            };
                            __b.push_str(&::aver_rt::str_lit!(" binds "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(aver_rt::AverInt::from_i64(ns.len() as i64)),
                        )));
                        __b
                    };
                    __b.push_str(&::aver_rt::str_lit!(" names for "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(aver_rt::AverInt::from_i64(fs.len() as i64)),
                )));
                __b
            };
            __b.push_str(&::aver_rt::str_lit!(" fields"));
            __b
        }))
    }
}

/// A literal pattern has the subject's type and binds nothing.
#[inline(always)]
pub fn litFits(
    mut k @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    subject @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    if crate::proof_std::aver_generated::kernel::types::fits(k, subject.clone()) {
        Ok(aver_rt::AverList::empty())
    } else {
        Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&::aver_rt::str_lit!("the pattern does not fit "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::types::show(subject)),
            )));
            __b
        }))
    }
}

/// The variables of each part of a tuple pattern.
pub fn patVarsAll(
    ps @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Pat>,
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    {
        let __int_match_subject = (ps.clone(), ts.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if (*__lit0).is_empty() && (*__lit1).is_empty() {
            Ok(aver_rt::AverList::empty())
        } else if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((p, pr)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((t, tr)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            Ok(aver_rt::AverList::concat(
                &crate::proof_std::aver_generated::kernel::types::patVars(p, t, e)?,
                &crate::proof_std::aver_generated::kernel::types::patVarsAll(&pr, &tr, e)?,
            ))
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&::aver_rt::str_lit!("the pattern does not fit "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&::aver_rt::str_lit!("("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &((aver_rt::string_join(
                                    &crate::proof_std::aver_generated::kernel::types::showAll(ts),
                                    &::aver_rt::str_lit!(", "),
                                ))
                                .into_aver()),
                            )));
                            __b
                        };
                        __b.push_str(&::aver_rt::str_lit!(")"));
                        __b
                    })),
                )));
                __b
            }))
        }
    }
}

/// Names paired with types, as far as both go, wildcards left out.
pub fn namedVars(
    ns @ _: &aver_rt::AverList<AverStr>,
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> {
    crate::proof_std::cancel_checkpoint();
    {
        let __int_match_subject = (ns.clone(), ts.clone());
        let (__lit0, __lit1) = &__int_match_subject;
        if !(*__lit0).is_empty() && !(*__lit1).is_empty() {
            let Some((n, nr)) = aver_rt::list_uncons_cloned(&(*__lit0)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            let Some((t, tr)) = aver_rt::list_uncons_cloned(&(*__lit1)) else {
                unreachable!("Aver Rust codegen: tuple element list mismatch")
            };
            {
                let __arg6 @ _ =
                    crate::proof_std::aver_generated::kernel::types::namedVars(&nr, &tr);
                crate::proof_std::aver_generated::kernel::types::namedOne(n, t, &__arg6)
            }
        } else {
            aver_rt::AverList::empty()
        }
    }
}

/// One name with its type in front of the rest, unless it is the wildcard.
#[inline(always)]
pub fn namedOne(
    n @ _: AverStr,
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    rest @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> {
    crate::proof_std::cancel_checkpoint();
    if (&*n == "_") {
        rest.clone()
    } else {
        aver_rt::AverList::prepend(
            crate::proof_std::aver_generated::kernel::term::typed(n, t),
            rest,
        )
    }
}

/// The one type every arm's body has, each under the variables its pattern binds.
#[inline(always)]
pub fn armsType(
    mut subject @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    mut acc @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    let mut __hold1: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>;
    let mut arms @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm> =
        arms;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = arms;
            if __list_subject.is_empty() {
                return Ok(acc);
            } else {
                if let (Some(a), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __tco1 = rest;
                        let __tco2 = {
                            let __arg8 @ _ = {
                                let __arg7 @ _ = {
                                    let __arg6 @ _ =
                                        crate::proof_std::aver_generated::kernel::types::patVars(
                                            a.pattern.clone(),
                                            subject.clone(),
                                            e,
                                        )?;
                                    crate::proof_std::aver_generated::kernel::types::withVars(
                                        e, &__arg6,
                                    )
                                };
                                crate::proof_std::aver_generated::kernel::types::typeOf(
                                    &a.body, &__arg7,
                                )
                            }?;
                            crate::proof_std::aver_generated::kernel::types::join(acc, __arg8)
                        }?;
                        __hold1 = __tco1;
                        arms = &__hold1;
                        acc = __tco2;
                        continue;
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The one check of a substitution: each bound term's type fits the declared type of its name, the declaration's type variables bound in order; a variable marked as needing equality takes only a type that has one.
#[inline(always)]
pub fn bindTyped(
    bs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    declared @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut bound @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    mut what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>;
    let mut bs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding> =
        bs;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = bs;
            if __list_subject.is_empty() {
                return {
                    let __arg8 @ _ =
                        crate::proof_std::aver_generated::kernel::types::typesOfVars(declared);
                    crate::proof_std::aver_generated::kernel::types::eqKept(&__arg8, bound, e, what)
                };
            } else {
                if let (Some(b), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    match crate::proof_std::aver_generated::kernel::types::find(
                        b.name.clone(),
                        declared,
                    ) {
                        None => {
                            return Err(aver_rt::AverStr::from({
                                let mut __b = {
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(47))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&aver_rt::AverStr::from(
                                            aver_rt::aver_display(&(what)),
                                        ));
                                        __b
                                    };
                                    __b.push_str(&::aver_rt::str_lit!(" does not bind "));
                                    __b
                                };
                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                    &(b.name),
                                )));
                                __b
                            }));
                        }
                        Some(want @ _) => {
                            let __tco0 = rest;
                            let __tco2 = {
                                let __arg9 @ _ =
                                    crate::proof_std::aver_generated::kernel::types::typeOf(
                                        &b.value, e,
                                    )?;
                                {
                                    let __arg10 @ _ = aver_rt::AverStr::from({
                                        let mut __b = {
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = aver_rt::Buffer::with_capacity(
                                                        (aver_rt::AverInt::from_i64(37))
                                                            .to_usize()
                                                            .unwrap_or(0),
                                                    );
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(what)),
                                                    ));
                                                    __b
                                                };
                                                __b.push_str(&::aver_rt::str_lit!(": "));
                                                __b
                                            };
                                            __b.push_str(&aver_rt::AverStr::from(
                                                aver_rt::aver_display(&(b.name)),
                                            ));
                                            __b
                                        };
                                        __b.push_str(&::aver_rt::str_lit!(" :="));
                                        __b
                                    });
                                    crate::proof_std::aver_generated::kernel::types::bindVars(
                                        &want, __arg9, &bound, e, __arg10,
                                    )
                                }
                            }?;
                            __hold0 = __tco0;
                            bs = &__hold0;
                            bound = __tco2;
                            continue;
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// Every type variable some declared type marks as needing equality is bound, once all terms are in, to a type that has one: the mark is the variable's, whichever of its places a term reached it through.
#[inline(always)]
pub fn eqKept(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut bound @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    mut what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    let mut __hold0: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>;
    let mut ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> = ts;
    loop {
        crate::proof_std::cancel_checkpoint();
        {
            let __list_subject = ts;
            if __list_subject.is_empty() {
                return Ok(bound);
            } else {
                if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __t = t;
                        match __t.view() {
                            crate::proof_std::aver_generated::kernel::term::Ty__view::TyVar(
                                __n_fields,
                            ) => {
                                let n = __n_fields.0.clone();
                                let __pat0 = __n_fields.1.clone();
                                if __pat0 {
                                    if {
                                        let __arg8 @ _ = crate::proof_std::aver_generated::kernel::types::find(n.clone(), &bound).unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny);
                                        crate::proof_std::aver_generated::kernel::types::hasEq(
                                            &__arg8, e,
                                        )
                                    } {
                                        {
                                            let __tco0 = rest;
                                            __hold0 = __tco0;
                                            ts = &__hold0;
                                            continue;
                                        }
                                    } else {
                                        return Err(aver_rt::AverStr::from({
                                            let mut __b = {
                                                let mut __b = {
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(56))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&aver_rt::AverStr::from(
                                                            aver_rt::aver_display(&(what)),
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&::aver_rt::str_lit!(": == on "));
                                                    __b
                                                };
                                                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&({ let __arg9 @ _ = crate::proof_std::aver_generated::kernel::types::find(n, &bound).unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny); crate::proof_std::aver_generated::kernel::types::show(&__arg9) }))));
                                                __b
                                            };
                                            __b.push_str(&::aver_rt::str_lit!(" is not equality"));
                                            __b
                                        }));
                                    }
                                } else {
                                    {
                                        let __tco0 = aver_rt::AverList::concat(&crate::proof_std::aver_generated::kernel::types::partsOf(t), &rest);
                                        __hold0 = __tco0;
                                        ts = &__hold0;
                                        continue;
                                    }
                                }
                            }
                            _ => {
                                let __tco0 = aver_rt::AverList::concat(
                                    &crate::proof_std::aver_generated::kernel::types::partsOf(t),
                                    &rest,
                                );
                                __hold0 = __tco0;
                                ts = &__hold0;
                                continue;
                            }
                        }
                    }
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    }
}

/// The values of a type can be listed when it is Bool, a sum whose constructors carry nothing, or a record or tuple of such types.
pub fn finOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    {
        let __t = t;
        match __t.view() {
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyBool => {
                Some(crate::proof_std::aver_generated::kernel::term::Fin::FBool)
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyTuple(__xs_fields) => {
                let xs = &__xs_fields.0;
                {
                    let __arg5 @ _ =
                        crate::proof_std::aver_generated::kernel::types::finAll(xs, e, seen);
                    crate::proof_std::aver_generated::kernel::types::finTuple(&__arg5)
                }
            }
            crate::proof_std::aver_generated::kernel::term::Ty__view::TyNamed(__n_fields) => {
                let n = __n_fields.0.clone();
                crate::proof_std::aver_generated::kernel::types::finSeen(n, e, seen)
            }
            _ => None,
        }
    }
}

/// A tuple of finite parts.
#[inline(always)]
pub fn finTuple(
    parts @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Fin>>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    match parts {
        Some(fs @ _) => Some(crate::proof_std::aver_generated::kernel::term::Fin::FTuple(
            fs.clone(),
        )),
        None => None,
    }
}

/// A declared name, unless it is being read already: a type that holds itself is not finite.
#[inline(always)]
pub fn finSeen(
    n @ _: AverStr,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    if seen.contains(&n) {
        None
    } else {
        {
            let __arg3 @ _ = aver_rt::AverList::prepend(n.clone(), seen);
            crate::proof_std::aver_generated::kernel::types::finNamed(n, e, &__arg3)
        }
    }
}

/// A declared sum of bare constructors, or a record of finite fields.
pub fn finNamed(
    n @ _: AverStr,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    {
        let __subject = (
            crate::proof_std::aver_generated::kernel::types::sumNamed(n.clone(), &e.sums),
            crate::proof_std::aver_generated::kernel::types::recNamed(n.clone(), &e.recs),
        );
        match __subject {
            (Some(s), _) => crate::proof_std::aver_generated::kernel::types::finSum(&s),
            (_, Some(r)) => {
                let __arg5 @ _ =
                    crate::proof_std::aver_generated::kernel::types::finFields(&r.fields, e, seen);
                crate::proof_std::aver_generated::kernel::types::finRec(n, &__arg5)
            }
            _ => None,
        }
    }
}

/// A sum is finite when no constructor carries a field.
#[inline(always)]
pub fn finSum(
    s @ _: &crate::proof_std::aver_generated::kernel::proof::Sum,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    if crate::proof_std::aver_generated::kernel::types::bare(&s.variants) {
        Some(crate::proof_std::aver_generated::kernel::term::Fin::FSum(
            crate::proof_std::aver_generated::kernel::proof::variantNames(&s.variants),
        ))
    } else {
        None
    }
}

/// A record of finite fields.
#[inline(always)]
pub fn finRec(
    n @ _: AverStr,
    fields @ _: &Option<
        aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::FinField>,
    >,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    match fields {
        Some(fs @ _) => Some(crate::proof_std::aver_generated::kernel::term::Fin::FRec(
            n,
            fs.clone(),
        )),
        None => None,
    }
}

/// Whether no constructor carries a field.
#[inline(always)]
pub fn bare(
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Variant>,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = vs;
        if __list_subject.is_empty() {
            true
        } else {
            if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                ((aver_rt::AverInt::from_i64(v.fields.len() as i64)
                    == aver_rt::AverInt::from_i64(0))
                    && crate::proof_std::aver_generated::kernel::types::bare(&rest))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Every type finite, or nothing.
#[inline(always)]
pub fn finAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Fin>> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = ts;
        if __list_subject.is_empty() {
            Some(aver_rt::AverList::empty())
        } else {
            if let (Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __subject = (
                        crate::proof_std::aver_generated::kernel::types::finOf(t, e, seen),
                        crate::proof_std::aver_generated::kernel::types::finAll(&rest, e, seen),
                    );
                    match __subject {
                        (Some(f), Some(fs)) => Some(aver_rt::AverList::prepend_owned(f, fs)),
                        _ => None,
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// Every field finite, or nothing.
#[inline(always)]
pub fn finFields(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::FinField>> {
    crate::proof_std::cancel_checkpoint();
    {
        let __list_subject = fs;
        if __list_subject.is_empty() {
            Some(aver_rt::AverList::empty())
        } else {
            if let (Some(f), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                {
                    let __subject = (
                        crate::proof_std::aver_generated::kernel::types::finOf(&f.ty, e, seen),
                        crate::proof_std::aver_generated::kernel::types::finFields(&rest, e, seen),
                    );
                    match __subject {
                        (Some(fin), Some(more)) => Some(aver_rt::AverList::prepend_owned(
                            crate::proof_std::aver_generated::kernel::term::FinField {
                                name: f.name.clone(),
                                fin: fin,
                            },
                            more,
                        )),
                        _ => None,
                    }
                }
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}
