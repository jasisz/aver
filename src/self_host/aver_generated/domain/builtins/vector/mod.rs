#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::helpers::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Dispatch Vector.* builtins.
#[inline(always)]
pub fn call(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
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
                                        __b.push_str(&AverStr::from("unknown vector builtin: "));
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

/// Vector.new(size, default) -> Vector<T>.
pub fn builtinVectorNew(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (crate::aver_generated::domain::value::Val::ValInt(size), defaultVal) => Ok(
            crate::aver_generated::domain::value::Val::ValVector(aver_rt::AverVector::new(
                (size)
                    .to_usize()
                    .expect("Vector.new: size must be a non-negative, machine-sized Int"),
                defaultVal,
            )),
        ),
        _ => Err(AverStr::from("Vector.new: first arg must be Int")),
    }
}

/// Vector.get(vec, idx) -> Option<T>.
pub fn builtinVectorGet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (
            crate::aver_generated::domain::value::Val::ValVector(vec),
            crate::aver_generated::domain::value::Val::ValInt(idx),
        ) => match (idx).to_usize().and_then(|__i| vec.get(__i).cloned()) {
            Some(v) => Ok(crate::aver_generated::domain::value::Val::ValSome(
                std::sync::Arc::new(v),
            )),
            None => Ok(crate::aver_generated::domain::value::Val::ValNone),
        },
        _ => Err(AverStr::from("Vector.get: expected (Vector, Int)")),
    }
}

/// Vector.set(vec, idx, val) -> Option<Vector<T>>.
pub fn builtinVectorSet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __list_subject = args.clone();
        if let Some((vecV, rest)) = aver_rt::list_uncons_cloned(&__list_subject) {
            {
                let __list_subject = rest;
                if let Some((idxV, rest2)) = aver_rt::list_uncons_cloned(&__list_subject) {
                    {
                        let __list_subject = rest2;
                        if let Some((valV, ignored)) = aver_rt::list_uncons_cloned(&__list_subject)
                        {
                            crate::aver_generated::domain::builtins::vector::builtinVectorSetInner(
                                &vecV, &idxV, &valV,
                            )
                        } else {
                            Err(AverStr::from("Vector.set: expected 3 args"))
                        }
                    }
                } else {
                    Err(AverStr::from("Vector.set: expected 3 args"))
                }
            }
        } else {
            Err(AverStr::from("Vector.set: expected 3 args"))
        }
    }
}

/// Validate types and bounds before delegating to builtinVectorSetInBounds.
pub fn builtinVectorSetInner(vecV: &Val, idxV: &Val, valV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match vecV.clone() {
        crate::aver_generated::domain::value::Val::ValVector(vec) => {
            match idxV.clone() {
                crate::aver_generated::domain::value::Val::ValInt(idx) => {
                    if (idx < aver_rt::AverInt::from_i64(0)) {
                        Ok(crate::aver_generated::domain::value::Val::ValNone)
                    } else {
                        if (idx < aver_rt::AverInt::from_i64(vec.len() as i64)) {
                            crate::aver_generated::domain::builtins::vector::builtinVectorSetInBounds(&vec, idx, valV)
                        } else {
                            Ok(crate::aver_generated::domain::value::Val::ValNone)
                        }
                    }
                }
                _ => Err(AverStr::from("Vector.set: second arg must be Int")),
            }
        }
        _ => Err(AverStr::from("Vector.set: first arg must be Vector")),
    }
}

/// Set a vector element when bounds have already been checked.
#[inline(always)]
pub fn builtinVectorSetInBounds(
    vec: &aver_rt::AverVector<Val>,
    idx: aver_rt::AverInt,
    valV: &Val,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match (idx)
        .to_usize()
        .and_then(|__i| vec.clone().set_owned(__i, valV.clone()))
    {
        Some(newVec) => Ok(crate::aver_generated::domain::value::Val::ValSome(
            std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValVector(newVec)),
        )),
        None => Err(AverStr::from("Vector.set: index out of bounds")),
    }
}

/// Vector.len(vec) -> Int.
pub fn builtinVectorLen(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValVector(vec) => {
            Ok(crate::aver_generated::domain::value::Val::ValInt(
                aver_rt::AverInt::from_i64(vec.len() as i64),
            ))
        }
        _ => Err(AverStr::from("Vector.len: expected Vector")),
    }
}

/// Vector.fromList(list) -> Vector<T>.
pub fn builtinVectorFromList(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValList(items) => {
            Ok(crate::aver_generated::domain::value::Val::ValVector(
                aver_rt::AverVector::from_vec(items.to_vec()),
            ))
        }
        _ => Err(AverStr::from("Vector.fromList: expected List")),
    }
}

/// List.fromVector(vec) -> List<T>.
pub fn builtinVectorToList(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        crate::aver_generated::domain::value::Val::ValVector(vec) => Ok(
            crate::aver_generated::domain::value::Val::ValList(vec.to_list()),
        ),
        _ => Err(AverStr::from("List.fromVector: expected Vector")),
    }
}
