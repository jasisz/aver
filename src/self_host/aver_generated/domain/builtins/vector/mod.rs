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
            builtinVectorNew(args)
        } else {
            if &*__dispatch_subject == "Vector.get" {
                builtinVectorGet(args)
            } else {
                if &*__dispatch_subject == "Vector.set" {
                    builtinVectorSet(args)
                } else {
                    if &*__dispatch_subject == "Vector.len" {
                        builtinVectorLen(args)
                    } else {
                        if &*__dispatch_subject == "Vector.fromList" {
                            builtinVectorFromList(args)
                        } else {
                            if &*__dispatch_subject == "List.fromVector" {
                                builtinVectorToList(args)
                            } else {
                                Err(aver_rt::AverStr::from({
                                    let mut __b = {
                                        let mut __b =
                                            aver_rt::Buffer::with_capacity((40i64) as usize);
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
        (Val::ValInt(size), defaultVal) => Ok(Val::ValVector(aver_rt::AverVector::new(
            size as usize,
            defaultVal,
        ))),
        _ => Err(AverStr::from("Vector.new: first arg must be Int")),
    }
}

/// Vector.get(vec, idx) -> Option<T>.
pub fn builtinVectorGet(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    match pair {
        (Val::ValVector(vec), Val::ValInt(idx)) => match vec.get(idx as usize).cloned() {
            Some(v) => Ok(Val::ValSome(std::sync::Arc::new(v))),
            None => Ok(Val::ValNone.clone()),
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
                            builtinVectorSetInner(&vecV, &idxV, &valV)
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
        Val::ValVector(vec) => match idxV.clone() {
            Val::ValInt(idx) => {
                if (idx < 0i64) {
                    Ok(Val::ValNone.clone())
                } else {
                    if (idx < (vec.len() as i64)) {
                        builtinVectorSetInBounds(&vec, idx, valV)
                    } else {
                        Ok(Val::ValNone.clone())
                    }
                }
            }
            _ => Err(AverStr::from("Vector.set: second arg must be Int")),
        },
        _ => Err(AverStr::from("Vector.set: first arg must be Vector")),
    }
}

/// Set a vector element when bounds have already been checked.
#[inline(always)]
pub fn builtinVectorSetInBounds(
    vec: &aver_rt::AverVector<Val>,
    idx: i64,
    valV: &Val,
) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match vec.clone().set_owned(idx as usize, valV.clone()) {
        Some(newVec) => Ok(Val::ValSome(std::sync::Arc::new(Val::ValVector(newVec)))),
        None => Err(AverStr::from("Vector.set: index out of bounds")),
    }
}

/// Vector.len(vec) -> Int.
pub fn builtinVectorLen(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValVector(vec) => Ok(Val::ValInt((vec.len() as i64))),
        _ => Err(AverStr::from("Vector.len: expected Vector")),
    }
}

/// Vector.fromList(list) -> Vector<T>.
pub fn builtinVectorFromList(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValList(items) => Ok(Val::ValVector(aver_rt::AverVector::from_vec(
            items.to_vec(),
        ))),
        _ => Err(AverStr::from("Vector.fromList: expected List")),
    }
}

/// List.fromVector(vec) -> List<T>.
pub fn builtinVectorToList(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    match v {
        Val::ValVector(vec) => Ok(Val::ValList(vec.to_list())),
        _ => Err(AverStr::from("List.fromVector: expected Vector")),
    }
}
