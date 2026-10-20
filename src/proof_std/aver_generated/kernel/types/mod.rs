#[allow(unused_imports)]
use crate::proof_std::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TyEnv {
    pub vars: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    pub defs: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Def>,
    pub consts: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const>,
    pub sums: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum>,
    pub recs: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec>,
    pub sigs: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::FnSig>,
}

impl PartialOrd for TyEnv {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TyEnv {
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

impl aver_rt::AverDisplay for TyEnv {
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
    crate::proof_std::aver_generated::kernel::types::TyEnv {
        vars: aver_rt::AverList::concat(&vs.clone(), &e.vars.clone()),
        defs: e.defs.clone(),
        consts: e.consts.clone(),
        sums: e.sums.clone(),
        recs: e.recs.clone(),
        sigs: e.sigs.clone(),
    }
}

/// No variables and no declarations.
pub fn empty() -> TyEnv {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::types::TyEnv {
        vars: aver_rt::AverList::empty(),
        defs: aver_rt::AverList::empty(),
        consts: aver_rt::AverList::empty(),
        sums: aver_rt::AverList::empty(),
        recs: aver_rt::AverList::empty(),
        sigs: aver_rt::AverList::empty(),
    }
}

/// A type as Aver spells it; a hole is _.
pub fn show(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyInt => AverStr::from("Int"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyBool => AverStr::from("Bool"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyStr => AverStr::from("String"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyFloat => AverStr::from("Float"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyUnit => AverStr::from("Unit"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("List<"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(&a)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(">"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyOption(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("Option<"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(&a)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(">"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyResult(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(42)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("Result<"));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::types::show(&a)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(", "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(&b)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(">"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyMap(__k_fields) => {
            let k = __k_fields.0.clone();
            let v = __k_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(39)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("Map<"));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &(crate::proof_std::aver_generated::kernel::types::show(&k)),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(", "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(&v)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(">"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVector(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(24)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("Vector<"));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(&a)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(">"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&AverStr::from("("));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &((aver_rt::string_join(
                            &crate::proof_std::aver_generated::kernel::types::showAll(&xs),
                            &AverStr::from(", "),
                        ))
                        .into_aver()),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(")"));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(__n_fields) => {
            let n = __n_fields.0.clone();
            n
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyAny => AverStr::from("_"),
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("?"));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyRigid(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("?"));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            })
        }
    }
}

/// Each type spelled.
#[inline(always)]
pub fn showAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => aver_rt::AverList::empty(), [t, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::types::show(&t), &crate::proof_std::aver_generated::kernel::types::showAll(&rest)))
}

/// The type of a named variable, the first of its name.
#[inline(always)]
pub fn find(
    mut n @ _: AverStr,
    mut vs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Ty> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(vs, [] => { return None; }, [v, rest] => { if (v.name == n) { return Some(v.ty); } else { {
            let __tco1 = rest;
            vs = __tco1;
            continue;
        } } })
    }
}

/// The declared type of a module-level binding.
#[inline(always)]
pub fn constType(
    mut n @ _: AverStr,
    mut cs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Const>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Ty> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(cs, [] => { return None; }, [c, rest] => { if (c.name == n) { return Some(c.ty); } else { {
            let __tco1 = rest;
            cs = __tco1;
            continue;
        } } })
    }
}

/// What a type is made of: its outermost shape, or the whole type when it has no parts. A declared name, a type variable and an unknown type each have a head no other kind of type has, whatever their name: _ is the head of a hole alone.
pub fn headOf(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            AverStr::from("List")
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyOption(__a_fields) => {
            let a = __a_fields.0.clone();
            AverStr::from("Option")
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVector(__a_fields) => {
            let a = __a_fields.0.clone();
            AverStr::from("Vector")
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyResult(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            AverStr::from("Result")
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyMap(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            AverStr::from("Map")
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            AverStr::from("tuple")
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(__n_fields) => {
            let n = __n_fields.0.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(22)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("named "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(20)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("var "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            })
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyRigid(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("unknown "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(eq))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            })
        }
        _ => crate::proof_std::aver_generated::kernel::types::show(t),
    }
}

/// The types a type is made of, in order.
pub fn partsOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverList::from_vec(vec![a])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyOption(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverList::from_vec(vec![a])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVector(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverList::from_vec(vec![a])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyResult(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            aver_rt::AverList::from_vec(vec![a, b])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyMap(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            aver_rt::AverList::from_vec(vec![a, b])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            xs
        }
        _ => aver_rt::AverList::empty(),
    }
}

/// The same shape over new parts.
pub fn rebuild(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    parts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    crate::proof_std::cancel_checkpoint();
    match &t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyList(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyList(
                    crate::proof_std::aver_generated::kernel::types::nth(
                        parts.clone(),
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyOption(__a_fields) => {
            let a = __a_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyOption(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyOption(
                    crate::proof_std::aver_generated::kernel::types::nth(
                        parts.clone(),
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVector(__a_fields) => {
            let a = __a_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyVector(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyVector(
                    crate::proof_std::aver_generated::kernel::types::nth(
                        parts.clone(),
                        aver_rt::AverInt::from_i64(0),
                    ),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyResult(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyResult(std::sync::Arc::new(
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
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyMap(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyMap(std::sync::Arc::new(
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
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyTuple(parts.clone()),
            ))
        }
        _ => t,
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
    match (
        (&*crate::proof_std::aver_generated::kernel::types::headOf(&a) == "_"),
        (&*crate::proof_std::aver_generated::kernel::types::headOf(&b) == "_"),
        ((crate::proof_std::aver_generated::kernel::types::headOf(&a)
            == crate::proof_std::aver_generated::kernel::types::headOf(&b))
            && (aver_rt::AverInt::from_i64(
                crate::proof_std::aver_generated::kernel::types::partsOf(&a).len() as i64,
            ) == aver_rt::AverInt::from_i64(
                crate::proof_std::aver_generated::kernel::types::partsOf(&b).len() as i64,
            ))),
    ) {
        (true, _, _) => Ok(b),
        (_, true, _) => Ok(a),
        (_, _, true) => Ok(crate::proof_std::aver_generated::kernel::types::rebuild(
            a.clone(),
            &crate::proof_std::aver_generated::kernel::types::joinEach(
                &crate::proof_std::aver_generated::kernel::types::partsOf(&a),
                &crate::proof_std::aver_generated::kernel::types::partsOf(&b),
            )?,
        )),
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
                __b.push_str(&AverStr::from(" is not "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::types::show(&b)),
            )));
            __b
        })),
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
            Ok(aver_rt::AverList::prepend(
                crate::proof_std::aver_generated::kernel::types::join(x, y)?,
                &crate::proof_std::aver_generated::kernel::types::joinEach(&xr, &yr)?,
            ))
        } else {
            Ok(aver_rt::AverList::empty())
        }
    }
}

/// Every type joined with the one before; a hole when there is none.
#[inline(always)]
pub fn joinAll(
    mut ts @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut acc @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ts, [] => { return Ok(acc); }, [t, rest] => { {
            let __tco0 = rest;
            let __tco1 = crate::proof_std::aver_generated::kernel::types::join(acc, t)?;
            ts = __tco0;
            acc = __tco1;
            continue;
        } })
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

/// Whether == between two values of the type is equality. Data, not a rule of the kernel: a declared type has one when its declaration in the script says so, any other type by its shape in Kernel.Builtins and then part by part; a type variable when it asks for one.
pub fn hasEq(t @ _: &crate::proof_std::aver_generated::kernel::term::Ty, e @ _: &TyEnv) -> bool {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(__n_fields) => {
            let n = __n_fields.0.clone();
            crate::proof_std::aver_generated::kernel::types::declaredEq(
                &crate::proof_std::aver_generated::kernel::types::sumNamed(
                    n.clone(),
                    e.sums.clone(),
                ),
                &crate::proof_std::aver_generated::kernel::types::recNamed(n, e.recs.clone()),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            eq
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyRigid(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            eq
        }
        _ => {
            (crate::proof_std::aver_generated::kernel::builtins::equality(
                crate::proof_std::aver_generated::kernel::types::headOf(t),
            ) && crate::proof_std::aver_generated::kernel::types::allEq(
                &crate::proof_std::aver_generated::kernel::types::partsOf(t),
                e,
            ))
        }
    }
}

/// A type as the proof of its own statement sees it: each type variable is one unknown type, which fits only itself and has an equality only when the variable asks for one.
pub fn rigid(
    mut t @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> crate::proof_std::aver_generated::kernel::term::Ty {
    crate::proof_std::cancel_checkpoint();
    match &t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyRigid(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyRigid(n, eq),
            ))
        }
        _ => {
            let __arg3 @ _ = crate::proof_std::aver_generated::kernel::types::rigidAll(
                &crate::proof_std::aver_generated::kernel::types::partsOf(&t),
            );
            crate::proof_std::aver_generated::kernel::types::rebuild(t, &__arg3)
        }
    }
}

/// Each type with its variables fixed.
#[inline(always)]
pub fn rigidAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => aver_rt::AverList::empty(), [t, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::types::rigid(t), &crate::proof_std::aver_generated::kernel::types::rigidAll(&rest)))
}

/// The givens of a statement as its own proof sees them.
#[inline(always)]
pub fn rigidVars(
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(vs.clone(), [] => aver_rt::AverList::empty(), [v, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::term::typed(v.name, crate::proof_std::aver_generated::kernel::types::rigid(v.ty)), &crate::proof_std::aver_generated::kernel::types::rigidVars(&rest)))
}

/// What a declared type's declaration says of its ==; a name the script does not declare has none.
pub fn declaredEq(
    s @ _: &Option<crate::proof_std::aver_generated::kernel::proof::Sum>,
    r @ _: &Option<crate::proof_std::aver_generated::kernel::proof::Rec>,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    match (s.clone(), r.clone()) {
        (Some(sum), _) => sum.eq,
        (_, Some(rec)) => rec.eq,
        _ => false,
    }
}

/// Whether every type has an equality.
#[inline(always)]
pub fn allEq(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
) -> bool {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => true, [t, rest] => (crate::proof_std::aver_generated::kernel::types::hasEq(&t, e) && crate::proof_std::aver_generated::kernel::types::allEq(&rest, e)))
}

/// The types of typed variables, in order.
#[inline(always)]
pub fn typesOfVars(
    vs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(vs.clone(), [] => aver_rt::AverList::empty(), [v, rest] => aver_rt::AverList::prepend(v.ty, &crate::proof_std::aver_generated::kernel::types::typesOfVars(&rest)))
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
    mut ss @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Sum> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ss, [] => { return None; }, [s, rest] => { if (s.name == n) { return Some(s); } else { {
            let __tco1 = rest;
            ss = __tco1;
            continue;
        } } })
    }
}

/// A declared record by name.
#[inline(always)]
pub fn recNamed(
    mut n @ _: AverStr,
    mut rs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Rec>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Rec> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(rs, [] => { return None; }, [r, rest] => { if (r.name == n) { return Some(r); } else { {
            let __tco1 = rest;
            rs = __tco1;
            continue;
        } } })
    }
}

/// The declared sum a constructor belongs to: the one named by the constructor up to its last dot.
#[inline(always)]
pub fn sumOf(
    c @ _: AverStr,
    ss @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Sum>,
) -> Option<crate::proof_std::aver_generated::kernel::proof::Sum> {
    crate::proof_std::cancel_checkpoint();
    crate::proof_std::aver_generated::kernel::types::sumNamed(
        crate::proof_std::aver_generated::kernel::types::typeName(c),
        ss.clone(),
    )
}

/// A constructor's name up to its last dot.
#[inline(always)]
pub fn typeName(c @ _: AverStr) -> AverStr {
    crate::proof_std::cancel_checkpoint();
    (aver_rt::string_join(
        &{
            let __n = aver_rt::clamp_list_count(&(aver_rt::AverInt::from_i64(1)));
            ((aver_rt::AverList::from_vec(
                c.split(&*AverStr::from("."))
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
            ))
            .into_aver()
            .reverse())
            .drop_first(__n)
        }
        .reverse(),
        &AverStr::from("."),
    ))
    .into_aver()
}

/// The field types of a constructor among a sum's variants.
#[inline(always)]
pub fn variantFields(
    mut c @ _: AverStr,
    mut vs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::Variant>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>> {
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(vs, [] => { return None; }, [v, rest] => { if (v.name == c) { return Some(v.fields); } else { {
            let __tco1 = rest;
            vs = __tco1;
            continue;
        } } })
    }
}

/// The field types of a constructor: a list's, Option's and Result's read off a subject of that type, any other off the declared sum, whatever the constructor is named.
pub fn ctorFields(
    c @ _: AverStr,
    subject @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match (
        c.clone(),
        crate::proof_std::aver_generated::kernel::types::headOf(subject),
    ) {
        (__str_lit0, __str_lit1) if str::eq(&__str_lit0, "nil") && str::eq(&__str_lit1, "List") => {
            Ok(aver_rt::AverList::empty())
        }
        (__str_lit0, __str_lit1)
            if str::eq(&__str_lit0, "cons") && str::eq(&__str_lit1, "List") =>
        {
            Ok(aver_rt::AverList::from_vec(vec![
                crate::proof_std::aver_generated::kernel::types::nth(
                    crate::proof_std::aver_generated::kernel::types::partsOf(subject),
                    aver_rt::AverInt::from_i64(0),
                ),
                crate::proof_std::aver_generated::kernel::term::Ty::TyList(std::sync::Arc::new(
                    crate::proof_std::aver_generated::kernel::term::Ty__TyList(
                        crate::proof_std::aver_generated::kernel::types::nth(
                            crate::proof_std::aver_generated::kernel::types::partsOf(subject),
                            aver_rt::AverInt::from_i64(0),
                        ),
                    ),
                )),
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
            Ok(aver_rt::AverList::from_vec(vec![
                crate::proof_std::aver_generated::kernel::types::nth(
                    crate::proof_std::aver_generated::kernel::types::partsOf(subject),
                    aver_rt::AverInt::from_i64(0),
                ),
            ]))
        }
        (__str_lit0, __str_lit1)
            if str::eq(&__str_lit0, "Result.Ok") && str::eq(&__str_lit1, "Result") =>
        {
            Ok(aver_rt::AverList::from_vec(vec![
                crate::proof_std::aver_generated::kernel::types::nth(
                    crate::proof_std::aver_generated::kernel::types::partsOf(subject),
                    aver_rt::AverInt::from_i64(0),
                ),
            ]))
        }
        (__str_lit0, __str_lit1)
            if str::eq(&__str_lit0, "Result.Err") && str::eq(&__str_lit1, "Result") =>
        {
            Ok(aver_rt::AverList::from_vec(vec![
                crate::proof_std::aver_generated::kernel::types::nth(
                    crate::proof_std::aver_generated::kernel::types::partsOf(subject),
                    aver_rt::AverInt::from_i64(1),
                ),
            ]))
        }
        _ => match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) {
            None => Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("unknown constructor "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c))));
                __b
            })),
            Some(s @ _) => {
                match crate::proof_std::aver_generated::kernel::types::variantFields(
                    c.clone(),
                    s.variants,
                ) {
                    None => Err(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("unknown constructor "));
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

/// What a list holds, if the type is a list.
pub fn elemOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
) -> Option<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            Some(a)
        }
        _ => None,
    }
}

/// The constructors of a type in order, as a split names them: nil and cons for a list, Option's, Result's, or a declared sum's; none for any other type.
pub fn ctorsOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
) -> aver_rt::AverList<AverStr> {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverList::from_vec(vec![AverStr::from("nil"), AverStr::from("cons")])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyOption(__a_fields) => {
            let a = __a_fields.0.clone();
            aver_rt::AverList::from_vec(vec![
                AverStr::from("Option.None"),
                AverStr::from("Option.Some"),
            ])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyResult(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            aver_rt::AverList::from_vec(vec![
                AverStr::from("Result.Err"),
                AverStr::from("Result.Ok"),
            ])
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(__n_fields) => {
            let n = __n_fields.0.clone();
            match crate::proof_std::aver_generated::kernel::types::sumNamed(n, e.sums.clone()) {
                Some(s @ _) => {
                    crate::proof_std::aver_generated::kernel::proof::variantNames(&s.variants)
                }
                None => aver_rt::AverList::empty(),
            }
        }
        _ => aver_rt::AverList::empty(),
    }
}

/// The type of a term, from its leaves up; an ill-typed term is refused with the first part that does not fit.
pub fn typeOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Term,
    mut e @ _: TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Term::TInt(__n_fields) => {
            let n = __n_fields.0.clone();
            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyInt)
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBool(__b_fields) => {
            let b = __b_fields.0.clone();
            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyBool)
        }
        crate::proof_std::aver_generated::kernel::term::Term::TStr(__s_fields) => {
            let s = __s_fields.0.clone();
            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyStr)
        }
        crate::proof_std::aver_generated::kernel::term::Term::TUnit => {
            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyUnit)
        }
        crate::proof_std::aver_generated::kernel::term::Term::TVar(__n_fields) => {
            let n = __n_fields.0.clone();
            crate::proof_std::aver_generated::kernel::types::varType(n, &e)
        }
        crate::proof_std::aver_generated::kernel::term::Term::THole => {
            Err(AverStr::from("the hole has no type"))
        }
        crate::proof_std::aver_generated::kernel::term::Term::TGet(__r_fields) => {
            let r = __r_fields.0.clone();
            let f = __r_fields.1.clone();
            crate::proof_std::aver_generated::kernel::types::fieldType(
                &crate::proof_std::aver_generated::kernel::types::typeOf(&r, e.clone())?,
                f,
                &e,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TCall(__f_fields) => {
            let f = __f_fields.0.clone();
            let xs = __f_fields.1.clone();
            crate::proof_std::aver_generated::kernel::types::callType(
                f,
                &crate::proof_std::aver_generated::kernel::types::typesOf(&xs, &e)?,
                e,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TBi(__n_fields) => {
            let n = __n_fields.0.clone();
            let xs = __n_fields.1.clone();
            crate::proof_std::aver_generated::kernel::types::builtinType(
                n,
                &crate::proof_std::aver_generated::kernel::types::typesOf(&xs, &e)?,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TOp(__o_fields) => {
            let o = __o_fields.0.clone();
            let a = __o_fields.1.clone();
            let b = __o_fields.2.clone();
            crate::proof_std::aver_generated::kernel::types::opType(
                o,
                crate::proof_std::aver_generated::kernel::types::typeOf(&a, e.clone())?,
                crate::proof_std::aver_generated::kernel::types::typeOf(&b, e)?,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TNeg(__a_fields) => {
            let a = __a_fields.0.clone();
            crate::proof_std::aver_generated::kernel::types::must(
                &crate::proof_std::aver_generated::kernel::types::typeOf(&a, e)?,
                crate::proof_std::aver_generated::kernel::term::Ty::TyInt,
                AverStr::from("a negation"),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TCtor(__c_fields) => {
            let c = __c_fields.0.clone();
            let xs = __c_fields.1.clone();
            crate::proof_std::aver_generated::kernel::types::ctorType(
                c,
                &crate::proof_std::aver_generated::kernel::types::typesOf(&xs, &e)?,
                e,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TMatch(__s_fields) => {
            let s = __s_fields.0.clone();
            let arms = __s_fields.1.clone();
            {
                let __list_subject = arms.clone();
                if __list_subject.is_empty() {
                    Err(AverStr::from("a match with no arms has no type"))
                } else {
                    crate::proof_std::aver_generated::kernel::types::armsType(
                        crate::proof_std::aver_generated::kernel::types::typeOf(&s, e.clone())?,
                        arms,
                        crate::proof_std::aver_generated::kernel::term::Ty::TyAny,
                        e,
                    )
                }
            }
        }
        crate::proof_std::aver_generated::kernel::term::Term::TParts(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::types::partsType(
                &crate::proof_std::aver_generated::kernel::types::typesOf(&xs, &e)?,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TList(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyList(
                std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyList(
                    crate::proof_std::aver_generated::kernel::types::joinAll(
                        crate::proof_std::aver_generated::kernel::types::typesOf(&xs, &e)?,
                        crate::proof_std::aver_generated::kernel::term::Ty::TyAny,
                    )?,
                )),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Term::TTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(
                std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyTuple(
                    crate::proof_std::aver_generated::kernel::types::typesOf(&xs, &e)?,
                )),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Term::TRec(__n_fields) => {
            let n = __n_fields.0.clone();
            let fs = __n_fields.1.clone();
            crate::proof_std::aver_generated::kernel::types::fieldsFit(
                n.clone(),
                fs,
                crate::proof_std::aver_generated::kernel::types::declaredRec(n, &e)?,
                e,
            )
        }
        crate::proof_std::aver_generated::kernel::term::Term::TUpd(__n_fields) => {
            let n = __n_fields.0.clone();
            let b = __n_fields.1.clone();
            let fs = __n_fields.2.clone();
            crate::proof_std::aver_generated::kernel::types::fieldsFit(
                n.clone(),
                fs,
                crate::proof_std::aver_generated::kernel::types::must(
                    &crate::proof_std::aver_generated::kernel::types::typeOf(&b, e.clone())?,
                    crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(
                        std::sync::Arc::new(
                            crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(n.clone()),
                        ),
                    ),
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(29)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&AverStr::from("an update of "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    }),
                )?,
                e,
            )
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
    match crate::proof_std::aver_generated::kernel::types::find(n.clone(), e.vars.clone()) {
        Some(t @ _) => Ok(t),
        None => {
            match crate::proof_std::aver_generated::kernel::types::constType(
                n.clone(),
                e.consts.clone(),
            ) {
                Some(t @ _) => Ok(t),
                None => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(43)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                        __b
                    };
                    __b.push_str(&AverStr::from(" is not a variable in scope"));
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
    aver_list_match!(ts.clone(), [] => Ok(aver_rt::AverList::empty()), [t, rest] => Ok(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::types::typeOf(&t, e.clone())?, &crate::proof_std::aver_generated::kernel::types::typesOf(&rest, e)?)))
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
                        __b.push_str(&AverStr::from(" takes "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(&expected)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(", not "));
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
    match r {
        crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(__n_fields) => {
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
                __b.push_str(&AverStr::from(" has no field "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
            __b
        })),
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
    match crate::proof_std::aver_generated::kernel::types::recNamed(n.clone(), e.recs.clone()) {
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            };
            __b.push_str(&AverStr::from(" is not a declared record"));
            __b
        })),
        Some(rec @ _) => {
            match crate::proof_std::aver_generated::kernel::types::find(f.clone(), rec.fields) {
                None => Err(aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(46)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" has no field "));
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

/// The type of a literal of a record the script declares.
#[inline(always)]
pub fn declaredRec(
    n @ _: AverStr,
    e @ _: &TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::types::recNamed(n.clone(), e.recs.clone()) {
        Some(r @ _) => Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(
            std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(
                n,
            )),
        )),
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                __b
            };
            __b.push_str(&AverStr::from(" is not a declared record"));
            __b
        })),
    }
}

/// A record's fields each fit their declared type.
#[inline(always)]
pub fn fieldsFit(
    mut n @ _: AverStr,
    mut fs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Field>,
    result @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    let result @ _ = std::sync::Arc::new(result);
    let e @ _ = std::sync::Arc::new(e);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(fs, [] => { return Ok((*result).clone()); }, [f, rest] => { if crate::proof_std::aver_generated::kernel::types::fits(crate::proof_std::aver_generated::kernel::types::typeOf(&f.value, (*e).clone())?, crate::proof_std::aver_generated::kernel::types::fieldType(&crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(n.clone()))), f.name.clone(), &*e)?) { {
            let __tco1 = rest;
            fs = __tco1;
            continue;
        } } else { return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(64)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("field ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f.name)))); __b }; __b.push_str(&AverStr::from(" of ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n)))); __b }; __b.push_str(&AverStr::from(" does not fit its type")); __b })); } })
    }
}

/// A definition's result, once the arguments fit its parameters.
#[inline(always)]
pub fn callType(
    f @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut e @ _: TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match crate::proof_std::aver_generated::kernel::proof::findDef(f.clone(), e.defs.clone()) {
        None => crate::proof_std::aver_generated::kernel::types::sigType(
            f,
            args.clone(),
            e.sigs.clone(),
            e,
        ),
        Some(d @ _) => {
            match crate::proof_std::aver_generated::kernel::types::bindEach(
                crate::proof_std::aver_generated::kernel::types::typesOfVars(&d.params),
                args.clone(),
                aver_rt::AverList::empty(),
                e,
                aver_rt::AverStr::from({
                    let mut __b = {
                        let mut __b = aver_rt::Buffer::with_capacity(
                            (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                        );
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f))));
                        __b
                    };
                    __b.push_str(&AverStr::from(":"));
                    __b
                }),
            ) {
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
    args @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut sigs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::proof::FnSig>,
    e @ _: TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    let args @ _ = std::sync::Arc::new(args);
    let e @ _ = std::sync::Arc::new(e);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(sigs, [] => { return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(32)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("no signature of ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f)))); __b })); }, [s, rest] => { if (s.name == f) { match crate::proof_std::aver_generated::kernel::types::bindEach(s.params, (*args).clone(), aver_rt::AverList::empty(), (*e).clone(), aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(f)))); __b }; __b.push_str(&AverStr::from(":")); __b })) { Ok(bound @ _) => { return Ok(s.ret); }, Err(why @ _) => { return Err(why); } } } else { {
            let __tco2 = rest;
            sigs = __tco2;
            continue;
        } } })
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
                __b.push_str(&AverStr::from("unknown builtin "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
            __b
        })),
        Some(s @ _) => Ok(
            crate::proof_std::aver_generated::kernel::types::instantiate(
                s.ret,
                &crate::proof_std::aver_generated::kernel::types::bindEach(
                    s.params,
                    args.clone(),
                    aver_rt::AverList::empty(),
                    crate::proof_std::aver_generated::kernel::types::empty(),
                    aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(n))));
                            __b
                        };
                        __b.push_str(&AverStr::from(":"));
                        __b
                    }),
                )?,
            ),
        ),
    }
}

/// Each actual type fits its expected one, binding the expected side's type variables in order.
pub fn bindEach(
    mut expected @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut actual @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut bound @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: TyEnv,
    mut what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    let e @ _ = std::sync::Arc::new(e);
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
                        (*e).clone(),
                        what.clone(),
                    )?;
                    expected = __tco0;
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
                            __b.push_str(&AverStr::from(" takes "));
                            __b
                        };
                        __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                            &(aver_rt::AverInt::from_i64(expected.len() as i64)),
                        )));
                        __b
                    };
                    __b.push_str(&AverStr::from(" arguments"));
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
    mut e @ _: TyEnv,
    what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match expected {
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            crate::proof_std::aver_generated::kernel::types::bindVar(n, eq, actual, bound, &e, what)
        }
        _ => crate::proof_std::aver_generated::kernel::types::bindShape(
            expected, &actual, bound, e, what,
        ),
    }
}

/// Two types of one shape, bound part by part; a hole fits anything.
pub fn bindShape(
    expected @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    actual @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    bound @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut e @ _: TyEnv,
    what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match (
        ((&*crate::proof_std::aver_generated::kernel::types::headOf(actual) == "_")
            || (&*crate::proof_std::aver_generated::kernel::types::headOf(expected) == "_")),
        ((crate::proof_std::aver_generated::kernel::types::headOf(expected)
            == crate::proof_std::aver_generated::kernel::types::headOf(actual))
            && (aver_rt::AverInt::from_i64(
                crate::proof_std::aver_generated::kernel::types::partsOf(expected).len() as i64,
            ) == aver_rt::AverInt::from_i64(
                crate::proof_std::aver_generated::kernel::types::partsOf(actual).len() as i64,
            ))),
    ) {
        (true, _) => Ok(bound.clone()),
        (_, true) => crate::proof_std::aver_generated::kernel::types::bindEach(
            crate::proof_std::aver_generated::kernel::types::partsOf(expected),
            crate::proof_std::aver_generated::kernel::types::partsOf(actual),
            bound.clone(),
            e,
            what,
        ),
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = {
                        let mut __b = {
                            let mut __b = aver_rt::Buffer::with_capacity(
                                (aver_rt::AverInt::from_i64(57)).to_usize().unwrap_or(0),
                            );
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(what))));
                            __b
                        };
                        __b.push_str(&AverStr::from(" "));
                        __b
                    };
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                        &(crate::proof_std::aver_generated::kernel::types::show(actual)),
                    )));
                    __b
                };
                __b.push_str(&AverStr::from(" is not "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::types::show(expected)),
            )));
            __b
        })),
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
                    __b.push_str(&AverStr::from(" == on "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(crate::proof_std::aver_generated::kernel::types::show(&actual)),
                )));
                __b
            };
            __b.push_str(&AverStr::from(" is not equality"));
            __b
        }))
    } else {
        match crate::proof_std::aver_generated::kernel::types::find(n.clone(), bound.clone()) {
            None => Ok(aver_rt::AverList::prepend(
                crate::proof_std::aver_generated::kernel::term::typed(n, actual),
                &bound.clone(),
            )),
            Some(prev @ _) => {
                match crate::proof_std::aver_generated::kernel::types::join(prev, actual) {
                    Ok(t @ _) => Ok(aver_rt::AverList::prepend(
                        crate::proof_std::aver_generated::kernel::term::typed(n, t),
                        &bound.clone(),
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
                            __b.push_str(&AverStr::from(" "));
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
    match &t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let eq = __n_fields.1.clone();
            crate::proof_std::aver_generated::kernel::types::find(n, bound.clone())
                .unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny)
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyList(__a_fields) => {
            let a = __a_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyList(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyList(
                    crate::proof_std::aver_generated::kernel::types::instantiate(a, bound),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyOption(__a_fields) => {
            let a = __a_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyOption(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyOption(
                    crate::proof_std::aver_generated::kernel::types::instantiate(a, bound),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyVector(__a_fields) => {
            let a = __a_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyVector(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyVector(
                    crate::proof_std::aver_generated::kernel::types::instantiate(a, bound),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyResult(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyResult(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyResult(
                    crate::proof_std::aver_generated::kernel::types::instantiate(a, bound),
                    crate::proof_std::aver_generated::kernel::types::instantiate(b, bound),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyMap(__a_fields) => {
            let a = __a_fields.0.clone();
            let b = __a_fields.1.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyMap(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyMap(
                    crate::proof_std::aver_generated::kernel::types::instantiate(a, bound),
                    crate::proof_std::aver_generated::kernel::types::instantiate(b, bound),
                ),
            ))
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(std::sync::Arc::new(
                crate::proof_std::aver_generated::kernel::term::Ty__TyTuple(
                    crate::proof_std::aver_generated::kernel::types::instantiateAll(&xs, bound),
                ),
            ))
        }
        _ => t,
    }
}

/// Each type instantiated.
#[inline(always)]
pub fn instantiateAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    bound @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
) -> aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => aver_rt::AverList::empty(), [t, rest] => aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::types::instantiate(t, bound), &crate::proof_std::aver_generated::kernel::types::instantiateAll(&rest, bound)))
}

/// An operator's result at the one type of its operands: == and != compare any two values; an order takes two Int, Float or texts; + adds two of those, and - * / two Int or Float. Which of them a step may read as integer arithmetic is that step's business.
pub fn opType(
    o @ _: AverStr,
    mut a @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    mut b @ _: crate::proof_std::aver_generated::kernel::term::Ty,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    crate::proof_std::cancel_checkpoint();
    let t @ _ = crate::proof_std::aver_generated::kernel::types::operands(o.clone(), a, b)?;
    match (
        aver_rt::AverList::from_vec(vec![AverStr::from("=="), AverStr::from("!=")]).contains(&o),
        aver_rt::AverList::from_vec(vec![
            AverStr::from("<"),
            AverStr::from("<="),
            AverStr::from(">"),
            AverStr::from(">="),
        ])
        .contains(&o),
        aver_rt::AverList::from_vec(vec![
            AverStr::from("+"),
            AverStr::from("-"),
            AverStr::from("*"),
            AverStr::from("/"),
        ])
        .contains(&o),
    ) {
        (true, _, _) => Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyBool),
        (_, true, _) => crate::proof_std::aver_generated::kernel::types::numeric(
            o,
            &t,
            true,
            crate::proof_std::aver_generated::kernel::term::Ty::TyBool,
        ),
        (_, _, true) => crate::proof_std::aver_generated::kernel::types::numeric(
            o.clone(),
            &t.clone(),
            (&*o == "+"),
            t,
        ),
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(33)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("unknown operator "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o))));
            __b
        })),
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
                __b.push_str(&AverStr::from(" takes two values of one type: "));
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
    match (t.clone(), texts) {
        (crate::proof_std::aver_generated::kernel::term::Ty::TyInt, _) => Ok(result),
        (crate::proof_std::aver_generated::kernel::term::Ty::TyFloat, _) => Ok(result),
        (crate::proof_std::aver_generated::kernel::term::Ty::TyAny, _) => Ok(result),
        (crate::proof_std::aver_generated::kernel::term::Ty::TyStr, true) => Ok(result),
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(o))));
                    __b
                };
                __b.push_str(&AverStr::from(" does not take "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::types::show(t)),
            )));
            __b
        })),
    }
}

/// A constructor application: Option and Result by their builtin shape, a declared constructor by its sum.
pub fn ctorType(
    c @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    mut e @ _: TyEnv,
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
                                std::sync::Arc::new(
                                    crate::proof_std::aver_generated::kernel::term::Ty__TyOption(
                                        crate::proof_std::aver_generated::kernel::term::Ty::TyAny,
                                    ),
                                ),
                            ),
                        )
                    } else {
                        match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
                    }
                }
            } else {
                if &*__dispatch_subject == "Option.Some" {
                    {
                        let __list_subject = __pat1;
                        if let Some((a, __pat2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                            {
                                let __list_subject = __pat2;
                                if __list_subject.is_empty() {
                                    Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyOption(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyOption(a))))
                                } else {
                                    match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
                                }
                            }
                        } else {
                            match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
                        }
                    }
                } else {
                    if &*__dispatch_subject == "Result.Ok" {
                        {
                            let __list_subject = __pat1;
                            if let Some((a, __pat3)) = aver_rt::list_uncons_cloned(&__list_subject)
                            {
                                {
                                    let __list_subject = __pat3;
                                    if __list_subject.is_empty() {
                                        Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyResult(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyResult(a, crate::proof_std::aver_generated::kernel::term::Ty::TyAny))))
                                    } else {
                                        match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
                                    }
                                }
                            } else {
                                match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
                            }
                        }
                    } else {
                        if &*__dispatch_subject == "Result.Err" {
                            {
                                let __list_subject = __pat1;
                                if let Some((a, __pat4)) =
                                    aver_rt::list_uncons_cloned(&__list_subject)
                                {
                                    {
                                        let __list_subject = __pat4;
                                        if __list_subject.is_empty() {
                                            Ok(crate::proof_std::aver_generated::kernel::term::Ty::TyResult(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyResult(crate::proof_std::aver_generated::kernel::term::Ty::TyAny, a))))
                                        } else {
                                            match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
                                        }
                                    }
                                } else {
                                    match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
                                }
                            }
                        } else {
                            match crate::proof_std::aver_generated::kernel::types::sumOf(c.clone(), &e.sums) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(s @ _) => { match crate::proof_std::aver_generated::kernel::types::variantFields(c.clone(), s.variants) { None => { Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0)); __b.push_str(&AverStr::from("unknown constructor ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b })) }, Some(fs @ _) => { Ok(crate::proof_std::aver_generated::kernel::types::instantiate(crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(std::sync::Arc::new(crate::proof_std::aver_generated::kernel::term::Ty__TyNamed(s.name))), &crate::proof_std::aver_generated::kernel::types::bindEach(fs, args.clone(), aver_rt::AverList::empty(), e, aver_rt::AverStr::from({ let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(17)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(c)))); __b }; __b.push_str(&AverStr::from(":")); __b }))?)) } } } }
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
    mut e @ _: TyEnv,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    crate::proof_std::cancel_checkpoint();
    match (
        p,
        crate::proof_std::aver_generated::kernel::types::headOf(&subject),
    ) {
        (crate::proof_std::aver_generated::kernel::term::Pat::PWild, _) => {
            Ok(aver_rt::AverList::empty())
        }
        (crate::proof_std::aver_generated::kernel::term::Pat::PVar(n), _) => {
            Ok(crate::proof_std::aver_generated::kernel::types::namedVars(
                &aver_rt::AverList::from_vec(vec![n]),
                &aver_rt::AverList::from_vec(vec![subject]),
            ))
        }
        (crate::proof_std::aver_generated::kernel::term::Pat::PLit(k), _) => {
            crate::proof_std::aver_generated::kernel::types::litFits(
                crate::proof_std::aver_generated::kernel::types::typeOf(&k, e)?,
                &subject,
            )
        }
        (crate::proof_std::aver_generated::kernel::term::Pat::PNil, __str_lit0)
            if str::eq(&__str_lit0, "List") =>
        {
            Ok(aver_rt::AverList::empty())
        }
        (crate::proof_std::aver_generated::kernel::term::Pat::PCons(h, t), __str_lit0)
            if str::eq(&__str_lit0, "List") =>
        {
            Ok(crate::proof_std::aver_generated::kernel::types::namedVars(
                &aver_rt::AverList::from_vec(vec![h, t]),
                &crate::proof_std::aver_generated::kernel::types::ctorFields(
                    AverStr::from("cons"),
                    &subject,
                    &e,
                )?,
            ))
        }
        (crate::proof_std::aver_generated::kernel::term::Pat::PTuple(ps), __str_lit0)
            if str::eq(&__str_lit0, "tuple") =>
        {
            crate::proof_std::aver_generated::kernel::types::patVarsAll(
                &ps,
                &crate::proof_std::aver_generated::kernel::types::partsOf(&subject),
                &e,
            )
        }
        (crate::proof_std::aver_generated::kernel::term::Pat::PCtor(c, ns), _) => {
            Ok(crate::proof_std::aver_generated::kernel::types::namedVars(
                &ns,
                &crate::proof_std::aver_generated::kernel::types::ctorFields(c, &subject, &e)?,
            ))
        }
        _ => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("the pattern does not fit "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &(crate::proof_std::aver_generated::kernel::types::show(&subject)),
            )));
            __b
        })),
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
                __b.push_str(&AverStr::from("the pattern does not fit "));
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
                &crate::proof_std::aver_generated::kernel::types::patVars(p, t, e.clone())?,
                &crate::proof_std::aver_generated::kernel::types::patVarsAll(&pr, &tr, e)?,
            ))
        } else {
            Err(aver_rt::AverStr::from({
                let mut __b = {
                    let mut __b = aver_rt::Buffer::with_capacity(
                        (aver_rt::AverInt::from_i64(41)).to_usize().unwrap_or(0),
                    );
                    __b.push_str(&AverStr::from("the pattern does not fit "));
                    __b
                };
                __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                    &(aver_rt::AverStr::from({
                        let mut __b = {
                            let mut __b = {
                                let mut __b = aver_rt::Buffer::with_capacity(
                                    (aver_rt::AverInt::from_i64(18)).to_usize().unwrap_or(0),
                                );
                                __b.push_str(&AverStr::from("("));
                                __b
                            };
                            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                &((aver_rt::string_join(
                                    &crate::proof_std::aver_generated::kernel::types::showAll(ts),
                                    &AverStr::from(", "),
                                ))
                                .into_aver()),
                            )));
                            __b
                        };
                        __b.push_str(&AverStr::from(")"));
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
            crate::proof_std::aver_generated::kernel::types::namedOne(
                n,
                t,
                &crate::proof_std::aver_generated::kernel::types::namedVars(&nr, &tr),
            )
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
            &rest.clone(),
        )
    }
}

/// The one type every arm's body has, each under the variables its pattern binds.
#[inline(always)]
pub fn armsType(
    subject @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    mut arms @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Arm>,
    mut acc @ _: crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: TyEnv,
) -> Result<crate::proof_std::aver_generated::kernel::term::Ty, AverStr> {
    let subject @ _ = std::sync::Arc::new(subject);
    let e @ _ = std::sync::Arc::new(e);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(arms, [] => { return Ok(acc); }, [a, rest] => { {
            let __tco1 = rest;
            let __tco2 = crate::proof_std::aver_generated::kernel::types::join(acc, crate::proof_std::aver_generated::kernel::types::typeOf(&a.body, crate::proof_std::aver_generated::kernel::types::withVars(&*e, &crate::proof_std::aver_generated::kernel::types::patVars(a.pattern, (*subject).clone(), (*e).clone())?))?)?;
            arms = __tco1;
            acc = __tco2;
            continue;
        } })
    }
}

/// The one check of a substitution: each bound term's type fits the declared type of its name, the declaration's type variables bound in order; a variable marked as needing equality takes only a type that has one.
#[inline(always)]
pub fn bindTyped(
    mut bs @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Binding>,
    declared @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    mut bound @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: TyEnv,
    mut what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    let declared @ _ = std::sync::Arc::new(declared);
    let e @ _ = std::sync::Arc::new(e);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(bs, [] => { return crate::proof_std::aver_generated::kernel::types::eqKept(crate::proof_std::aver_generated::kernel::types::typesOfVars(&*declared), bound, (*e).clone(), what); }, [b, rest] => { match crate::proof_std::aver_generated::kernel::types::find(b.name.clone(), (*declared).clone()) { None => { return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(47)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(what)))); __b }; __b.push_str(&AverStr::from(" does not bind ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(b.name)))); __b })); }, Some(want @ _) => { {
            let __tco0 = rest;
            let __tco2 = crate::proof_std::aver_generated::kernel::types::bindVars(&want, crate::proof_std::aver_generated::kernel::types::typeOf(&b.value, (*e).clone())?, &bound, (*e).clone(), aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(37)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(what)))); __b }; __b.push_str(&AverStr::from(": ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(b.name)))); __b }; __b.push_str(&AverStr::from(" :=")); __b }))?;
            bs = __tco0;
            bound = __tco2;
            continue;
        } } } })
    }
}

/// Every type variable some declared type marks as needing equality is bound, once all terms are in, to a type that has one: the mark is the variable's, whichever of its places a term reached it through.
#[inline(always)]
pub fn eqKept(
    mut ts @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    bound @ _: aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: TyEnv,
    mut what @ _: AverStr,
) -> Result<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>, AverStr> {
    let bound @ _ = std::sync::Arc::new(bound);
    let e @ _ = std::sync::Arc::new(e);
    loop {
        crate::proof_std::cancel_checkpoint();
        aver_list_match!(ts, [] => { return Ok((*bound).clone()); }, [t, rest] => { match &t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyVar(__n_fields) => {
            let n = __n_fields.0.clone();
            let __pat0 = __n_fields.1.clone();
            if __pat0 { if crate::proof_std::aver_generated::kernel::types::hasEq(&crate::proof_std::aver_generated::kernel::types::find(n.clone(), (*bound).clone()).unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny), &*e) { {
            let __tco0 = rest;
            ts = __tco0;
            continue;
        } } else { return Err(aver_rt::AverStr::from({ let mut __b = { let mut __b = { let mut __b = { let mut __b = aver_rt::Buffer::with_capacity((aver_rt::AverInt::from_i64(56)).to_usize().unwrap_or(0)); __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(what)))); __b }; __b.push_str(&AverStr::from(": == on ")); __b }; __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(&(crate::proof_std::aver_generated::kernel::types::show(&crate::proof_std::aver_generated::kernel::types::find(n, (*bound).clone()).unwrap_or(crate::proof_std::aver_generated::kernel::term::Ty::TyAny)))))); __b }; __b.push_str(&AverStr::from(" is not equality")); __b })); } } else { {
            let __tco0 = aver_rt::AverList::concat(&crate::proof_std::aver_generated::kernel::types::partsOf(&t), &rest);
            ts = __tco0;
            continue;
        } }
        },
        _ => {
            {
            let __tco0 = aver_rt::AverList::concat(&crate::proof_std::aver_generated::kernel::types::partsOf(&t), &rest);
            ts = __tco0;
            continue;
        }
        }
    } })
    }
}

/// The values of a type can be listed when it is Bool, a sum whose constructors carry nothing, or a record or tuple of such types.
pub fn finOf(
    t @ _: &crate::proof_std::aver_generated::kernel::term::Ty,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    match t {
        crate::proof_std::aver_generated::kernel::term::Ty::TyBool => {
            Some(crate::proof_std::aver_generated::kernel::term::Fin::FBool)
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyTuple(__xs_fields) => {
            let xs = __xs_fields.0.clone();
            crate::proof_std::aver_generated::kernel::types::finTuple(
                &crate::proof_std::aver_generated::kernel::types::finAll(&xs, e, seen),
            )
        }
        crate::proof_std::aver_generated::kernel::term::Ty::TyNamed(__n_fields) => {
            let n = __n_fields.0.clone();
            crate::proof_std::aver_generated::kernel::types::finSeen(n, e, seen)
        }
        _ => None,
    }
}

/// A tuple of finite parts.
#[inline(always)]
pub fn finTuple(
    parts @ _: &Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Fin>>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    match parts.clone() {
        Some(fs @ _) => Some(crate::proof_std::aver_generated::kernel::term::Fin::FTuple(
            fs,
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
        crate::proof_std::aver_generated::kernel::types::finNamed(
            n.clone(),
            e,
            &aver_rt::AverList::prepend(n, &seen.clone()),
        )
    }
}

/// A declared sum of bare constructors, or a record of finite fields.
pub fn finNamed(
    n @ _: AverStr,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<crate::proof_std::aver_generated::kernel::term::Fin> {
    crate::proof_std::cancel_checkpoint();
    match (
        crate::proof_std::aver_generated::kernel::types::sumNamed(n.clone(), e.sums.clone()),
        crate::proof_std::aver_generated::kernel::types::recNamed(n.clone(), e.recs.clone()),
    ) {
        (Some(s), _) => crate::proof_std::aver_generated::kernel::types::finSum(&s),
        (_, Some(r)) => crate::proof_std::aver_generated::kernel::types::finRec(
            n,
            &crate::proof_std::aver_generated::kernel::types::finFields(&r.fields, e, seen),
        ),
        _ => None,
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
    match fields.clone() {
        Some(fs @ _) => Some(crate::proof_std::aver_generated::kernel::term::Fin::FRec(
            n, fs,
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
    aver_list_match!(vs.clone(), [] => true, [v, rest] => ((aver_rt::AverInt::from_i64(v.fields.len() as i64) == aver_rt::AverInt::from_i64(0)) && crate::proof_std::aver_generated::kernel::types::bare(&rest)))
}

/// Every type finite, or nothing.
#[inline(always)]
pub fn finAll(
    ts @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Ty>,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::Fin>> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(ts.clone(), [] => Some(aver_rt::AverList::empty()), [t, rest] => match (crate::proof_std::aver_generated::kernel::types::finOf(&t, e, seen), crate::proof_std::aver_generated::kernel::types::finAll(&rest, e, seen)) {
        (Some(f), Some(fs)) => {
            Some(aver_rt::AverList::prepend(f, &fs))
        },
        _ => {
            None
        }
    })
}

/// Every field finite, or nothing.
#[inline(always)]
pub fn finFields(
    fs @ _: &aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::TypedVar>,
    e @ _: &TyEnv,
    seen @ _: &aver_rt::AverList<AverStr>,
) -> Option<aver_rt::AverList<crate::proof_std::aver_generated::kernel::term::FinField>> {
    crate::proof_std::cancel_checkpoint();
    aver_list_match!(fs.clone(), [] => Some(aver_rt::AverList::empty()), [f, rest] => match (crate::proof_std::aver_generated::kernel::types::finOf(&f.ty, e, seen), crate::proof_std::aver_generated::kernel::types::finFields(&rest, e, seen)) {
        (Some(fin), Some(more)) => {
            Some(aver_rt::AverList::prepend(crate::proof_std::aver_generated::kernel::term::FinField { name: f.name, fin: fin }, &more))
        },
        _ => {
            None
        }
    })
}
