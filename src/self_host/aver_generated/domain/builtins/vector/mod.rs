#[allow(unused_imports)]
use crate::*;

/// Dispatch Vector.* builtins.
#[inline(always)]
pub fn call(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Vector.new" {
            crate::aver_generated::domain::builtins::vector::builtinVectorNew(args)
        } else {
            if &*__dispatch_subject == "Vector.get" {
                crate::aver_generated::domain::builtins::vector::builtinVectorGet(args)
            } else {
                if &*__dispatch_subject == "Vector.set" {
                    crate::aver_generated::domain::builtins::vector::builtinVectorSet(args)
                } else {
                    if &*__dispatch_subject == "Vector.len" {
                        crate::aver_generated::domain::builtins::vector::builtinVectorLen(args)
                    } else {
                        if &*__dispatch_subject == "Vector.fromList" {
                            crate::aver_generated::domain::builtins::vector::builtinVectorFromList(
                                args,
                            )
                        } else {
                            if &*__dispatch_subject == "List.fromVector" {
                                crate::aver_generated::domain::builtins::vector::builtinVectorToList(
                                    args,
                                )
                            } else {
                                Err(aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b = aver_rt::Buffer::with_capacity(
                                            (aver_rt::AverInt::from_i64(40))
                                                .to_usize()
                                                .unwrap_or(0),
                                        );
                                        __b.push_str(&::aver_rt::str_lit!(
                                            "unknown vector builtin: "
                                        ));
                                        __b
                                    };
                                    __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                                        &(name),
                                    )));
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

/// Vector.new(size, default) -> Result<Vector<T>, String>.
pub fn builtinVectorNew(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let __t = pair;
        match (__t.0.view(), &__t.1) {
            (
                crate::aver_generated::domain::value::Val__view::ValInt(__size_fields),
                defaultVal,
            ) => {
                let size = __size_fields.0.clone();
                let defaultVal = defaultVal.clone();
                Ok(crate::aver_generated::domain::value::Val::ValVector(
                    crate::aver_generated::domain::value::Val__ValVector(
                        match aver_rt::checked_vector_size(&(size)) {
                            Some(__n) => Ok(aver_rt::AverVector::new(__n, defaultVal)),
                            None => {
                                Err(aver_rt::AverStr::from(aver_rt::vector_size_error_message()))
                            }
                        }?,
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Vector.new: first arg must be Int")),
        }
    }
}

/// Vector.get(vec, idx) -> Option<T>.
pub fn builtinVectorGet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let __t = pair;
        match (__t.0.view(), __t.1.view()) {
            (
                crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields),
                crate::aver_generated::domain::value::Val__view::ValInt(__idx_fields),
            ) => {
                let vec = &__vec_fields.0;
                let idx = __idx_fields.0.clone();
                match (idx).to_usize().and_then(|__i| vec.get(__i).cloned()) {
                    Some(v @ _) => Ok(crate::aver_generated::domain::value::Val::ValSome(
                        crate::aver_generated::domain::value::Val__ValSome(v),
                    )),
                    None => Ok(crate::aver_generated::domain::value::Val::ValNone),
                }
            }
            _ => Err(::aver_rt::str_lit!("Vector.get: expected (Vector, Int)")),
        }
    }
}

/// Vector.set(vec, idx, val) -> Option<Vector<T>>.
pub fn builtinVectorSet(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args;
        if let (Some(vecV), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
            {
                let __list_subject = rest;
                if let (Some(idxV), Some(rest2)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = rest2;
                        if let (Some(valV), Some(ignored)) =
                            (__list_subject.first(), __list_subject.tail())
                        {
                            crate::aver_generated::domain::builtins::vector::builtinVectorSetInner(
                                vecV, idxV, valV,
                            )
                        } else {
                            Err(::aver_rt::str_lit!("Vector.set: expected 3 args"))
                        }
                    }
                } else {
                    Err(::aver_rt::str_lit!("Vector.set: expected 3 args"))
                }
            }
        } else {
            Err(::aver_rt::str_lit!("Vector.set: expected 3 args"))
        }
    }
}

/// Validate types and bounds before delegating to builtinVectorSetInBounds.
pub fn builtinVectorSetInner(
    vecV @ _: &crate::aver_generated::domain::value::Val,
    idxV @ _: &crate::aver_generated::domain::value::Val,
    valV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __t = vecV;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields) => {
                let vec = __vec_fields.0.clone();
                {
                    let __t = idxV;
                    match __t.view() {
                        crate::aver_generated::domain::value::Val__view::ValInt(__idx_fields) => {
                            let idx = __idx_fields.0.clone();
                            if (idx < aver_rt::AverInt::from_i64(0)) {
                                Ok(crate::aver_generated::domain::value::Val::ValNone)
                            } else {
                                if (idx < aver_rt::AverInt::from_i64(vec.len() as i64)) {
                                    crate::aver_generated::domain::builtins::vector::builtinVectorSetInBounds(vec, idx, valV)
                                } else {
                                    Ok(crate::aver_generated::domain::value::Val::ValNone)
                                }
                            }
                        }
                        _ => Err(::aver_rt::str_lit!("Vector.set: second arg must be Int")),
                    }
                }
            }
            _ => Err(::aver_rt::str_lit!("Vector.set: first arg must be Vector")),
        }
    }
}

/// Set a vector element when bounds have already been checked.
#[inline(always)]
pub fn builtinVectorSetInBounds(
    mut vec @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    idx @ _: aver_rt::AverInt,
    valV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __idx = (idx).to_usize();
        let __value = valV.clone();
        match __idx.filter(|__idx| *__idx < vec.len()) {
            Some(__idx) => {
                let newVec = vec.set_unchecked(__idx, __value);
                Ok(crate::aver_generated::domain::value::Val::ValSome(
                    crate::aver_generated::domain::value::Val__ValSome(
                        crate::aver_generated::domain::value::Val::ValVector(
                            crate::aver_generated::domain::value::Val__ValVector(newVec),
                        ),
                    ),
                ))
            }
            None => Err(::aver_rt::str_lit!("Vector.set: index out of bounds")),
        }
    }
}

/// Vector.len(vec) -> Int.
pub fn builtinVectorLen(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields) => {
                let vec = &__vec_fields.0;
                Ok(crate::aver_generated::domain::value::Val::ValInt(
                    crate::aver_generated::domain::value::Val__ValInt(aver_rt::AverInt::from_i64(
                        vec.len() as i64,
                    )),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Vector.len: expected Vector")),
        }
    }
}

/// Vector.fromList(list) -> Vector<T>.
pub fn builtinVectorFromList(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValList(__items_fields) => {
                let items = __items_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValVector(
                    crate::aver_generated::domain::value::Val__ValVector(
                        aver_rt::AverVector::from_vec(items.to_vec()),
                    ),
                ))
            }
            _ => Err(::aver_rt::str_lit!("Vector.fromList: expected List")),
        }
    }
}

/// List.fromVector(vec) -> List<T>.
pub fn builtinVectorToList(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    {
        let __t = v;
        match __t.view() {
            crate::aver_generated::domain::value::Val__view::ValVector(__vec_fields) => {
                let vec = __vec_fields.0.clone();
                Ok(crate::aver_generated::domain::value::Val::ValList(
                    crate::aver_generated::domain::value::Val__ValList(vec.to_list()),
                ))
            }
            _ => Err(::aver_rt::str_lit!("List.fromVector: expected Vector")),
        }
    }
}
