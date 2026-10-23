#[allow(unused_imports)]
use crate::*;

/// Dispatch List.* builtins.
#[inline(always)]
pub fn call(
    name @ _: AverStr,
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "List.prepend" {
            crate::aver_generated::domain::builtins::list::builtinListPrepend(args)
        } else {
            if &*__dispatch_subject == "List.len" {
                crate::aver_generated::domain::builtins::list::builtinListLen(args)
            } else {
                if &*__dispatch_subject == "List.take" {
                    crate::aver_generated::domain::builtins::list::builtinListTake(args)
                } else {
                    if &*__dispatch_subject == "List.drop" {
                        crate::aver_generated::domain::builtins::list::builtinListDrop(args)
                    } else {
                        if &*__dispatch_subject == "List.reverse" {
                            crate::aver_generated::domain::builtins::list::builtinListReverse(args)
                        } else {
                            if &*__dispatch_subject == "List.concat" {
                                crate::aver_generated::domain::builtins::list::builtinListConcat(
                                    args,
                                )
                            } else {
                                if &*__dispatch_subject == "List.contains" {
                                    crate::aver_generated::domain::builtins::list::builtinListContains(args)
                                } else {
                                    if &*__dispatch_subject == "List.zip" {
                                        crate::aver_generated::domain::builtins::list::builtinListZip(args)
                                    } else {
                                        if &*__dispatch_subject == "List.head" {
                                            crate::aver_generated::domain::builtins::list::builtinListHead(args)
                                        } else {
                                            if &*__dispatch_subject == "List.tail" {
                                                crate::aver_generated::domain::builtins::list::builtinListTail(args)
                                            } else {
                                                Err(aver_rt::AverStr::from({
                                                    let mut __b = {
                                                        let mut __b =
                                                            aver_rt::Buffer::with_capacity(
                                                                (aver_rt::AverInt::from_i64(38))
                                                                    .to_usize()
                                                                    .unwrap_or(0),
                                                            );
                                                        __b.push_str(&::aver_rt::str_lit!(
                                                            "unknown list builtin: "
                                                        ));
                                                        __b
                                                    };
                                                    __b.push_str(&aver_rt::AverStr::from(
                                                        aver_rt::aver_display(&(name)),
                                                    ));
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
            }
        }
    }
}

/// List.prepend(value, list) -> list with value at front.
pub fn builtinListPrepend(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (v, lstV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListPrependInner(&v, &lstV)
    }
}

/// Inner impl of List.prepend.
pub fn builtinListPrependInner(
    v @ _: &crate::aver_generated::domain::value::Val,
    lstV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList(aver_rt::AverList::prepend_owned(
            v.clone(),
            items,
        )),
    ))
}

/// List.len(list) -> length as Int.
pub fn builtinListLen(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        crate::aver_generated::domain::value::Val__ValInt(aver_rt::AverInt::from_i64(
            items.len() as i64
        )),
    ))
}

/// List.take(list, n) -> first n elements.
pub fn builtinListTake(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, nV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListTakeInner(&lstV, &nV)
    }
}

/// Inner impl of List.take.
pub fn builtinListTakeInner(
    lstV @ _: &crate::aver_generated::domain::value::Val,
    nV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    let count @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList(
            crate::aver_generated::domain::builtins::list::listTake(&items, count),
        ),
    ))
}

/// Keep the first count elements; negative counts produce empty lists.
#[inline(always)]
pub fn listTake(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    count @ _: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    if (count > aver_rt::AverInt::from_i64(0)) {
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                aver_rt::AverList::empty()
            } else {
                if let (Some(item), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    aver_rt::AverList::prepend_owned(
                        item.clone(),
                        crate::aver_generated::domain::builtins::list::listTake(
                            &rest,
                            count.sub(&aver_rt::AverInt::from_i64(1)),
                        ),
                    )
                } else {
                    panic!("Aver Rust codegen: non-exhaustive list match")
                }
            }
        }
    } else {
        aver_rt::AverList::empty()
    }
}

/// List.drop(list, n) -> all but the first n elements.
pub fn builtinListDrop(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, nV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListDropInner(&lstV, &nV)
    }
}

/// Inner impl of List.drop.
pub fn builtinListDropInner(
    lstV @ _: &crate::aver_generated::domain::value::Val,
    nV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    let count @ _ = crate::aver_generated::domain::builtins::helpers::expectInt(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList(
            crate::aver_generated::domain::builtins::list::listDrop(items, count),
        ),
    ))
}

/// Skip the first count elements; negative counts leave the list unchanged.
#[inline(always)]
pub fn listDrop(
    mut items @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut count @ _: aver_rt::AverInt,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        if (count > aver_rt::AverInt::from_i64(0)) {
            aver_list_match!(items, [] => { return aver_rt::AverList::empty(); }, [_item, rest] => { {
            let __tco0 = rest;
            let __tco1 = count.sub(&aver_rt::AverInt::from_i64(1));
            items = __tco0;
            count = __tco1;
            continue;
        } })
        } else {
            return items;
        }
    }
}

/// List.reverse(list) -> reversed list.
pub fn builtinListReverse(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList(items.reverse()),
    ))
}

/// List.concat(a, b) -> concatenated list.
pub fn builtinListConcat(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListConcatInner(&aV, &bV)
    }
}

/// Inner impl of List.concat.
pub fn builtinListConcatInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let aItems @ _ = crate::aver_generated::domain::builtins::helpers::expectList(aV)?;
    let bItems @ _ = crate::aver_generated::domain::builtins::helpers::expectList(bV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList(aver_rt::AverList::concat(
            &aItems, &bItems,
        )),
    ))
}

/// List.contains(list, value) -> Bool.
pub fn builtinListContains(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, needle) = pair;
        crate::aver_generated::domain::builtins::list::builtinListContainsInner(&lstV, &needle)
    }
}

/// Inner impl of List.contains.
pub fn builtinListContainsInner(
    lstV @ _: &crate::aver_generated::domain::value::Val,
    needle @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        crate::aver_generated::domain::value::Val__ValBool(
            crate::aver_generated::domain::builtins::list::listContainsVal(&items, needle),
        ),
    ))
}

/// Check if a value is in a list by repr comparison.
#[inline(always)]
pub fn listContainsVal(
    items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    needle @ _: &crate::aver_generated::domain::value::Val,
) -> bool {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut items @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = items;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = items;
            if __list_subject.is_empty() {
                return false;
            } else {
                if let (Some(v), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                    if (crate::aver_generated::domain::value::valRepr(v)
                        == crate::aver_generated::domain::value::valRepr(needle))
                    {
                        return true;
                    } else {
                        {
                            let __tco0 = rest;
                            __hold0 = __tco0;
                            items = &__hold0;
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

/// List.zip(a, b) -> list of tuples.
pub fn builtinListZip(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let pair @ _ = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListZipInner(&aV, &bV)
    }
}

/// Inner impl of List.zip.
pub fn builtinListZipInner(
    aV @ _: &crate::aver_generated::domain::value::Val,
    bV @ _: &crate::aver_generated::domain::value::Val,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let aItems @ _ = crate::aver_generated::domain::builtins::helpers::expectList(aV)?;
    let bItems @ _ = crate::aver_generated::domain::builtins::helpers::expectList(bV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::value::Val__ValList(
            crate::aver_generated::domain::builtins::list::zipLists(&aItems, &bItems),
        ),
    ))
}

/// Zip two lists into list of ValTuple.
#[inline(always)]
pub fn zipLists(
    a @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    b @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    let __arg2 @ _ = <aver_rt::AverList<crate::aver_generated::domain::value::Val>>::empty();
    crate::aver_generated::domain::builtins::list::zipListsAcc(a, b, __arg2)
}

/// Accumulate zipped pairs in reverse, then reverse at end.
#[inline(always)]
pub fn zipListsAcc(
    a @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    b @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> aver_rt::AverList<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut a @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = a;
    let mut __hold1: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut b @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = b;
    loop {
        crate::cancel_checkpoint();
        let reversed @ _ = acc.reverse();
        {
            let __list_subject = a;
            if __list_subject.is_empty() {
                return reversed;
            } else {
                if let (Some(x), Some(xs)) = (__list_subject.first(), __list_subject.tail()) {
                    {
                        let __list_subject = b;
                        if __list_subject.is_empty() {
                            return reversed;
                        } else {
                            if let (Some(y), Some(ys)) =
                                (__list_subject.first(), __list_subject.tail())
                            {
                                {
                                    let __tco0 = xs;
                                    let __tco1 = ys;
                                    let __tco2 = aver_rt::AverList::prepend_owned(
                                        crate::aver_generated::domain::value::Val::ValTuple(
                                            crate::aver_generated::domain::value::Val__ValTuple(
                                                aver_rt::AverList::from_vec(vec![
                                                    x.clone(),
                                                    y.clone(),
                                                ]),
                                            ),
                                        ),
                                        acc,
                                    );
                                    __hold0 = __tco0;
                                    a = &__hold0;
                                    __hold1 = __tco1;
                                    b = &__hold1;
                                    acc = __tco2;
                                    continue;
                                }
                            } else {
                                panic!("Aver Rust codegen: non-exhaustive list match")
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

/// List.head(list) -> Option of first element.
pub fn builtinListHead(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    {
        let __list_subject = items;
        if __list_subject.is_empty() {
            Ok(crate::aver_generated::domain::value::Val::ValNone)
        } else {
            if let (Some(h), Some(rest)) = (__list_subject.first(), __list_subject.tail()) {
                Ok(crate::aver_generated::domain::value::Val::ValSome(
                    crate::aver_generated::domain::value::Val__ValSome(h.clone()),
                ))
            } else {
                panic!("Aver Rust codegen: non-exhaustive list match")
            }
        }
    }
}

/// List.tail(list) -> Option of rest.
pub fn builtinListTail(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    let v @ _ = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items @ _ = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    aver_list_match!(items, [] => Ok(crate::aver_generated::domain::value::Val::ValNone), [h, rest] => Ok(crate::aver_generated::domain::value::Val::ValSome(crate::aver_generated::domain::value::Val__ValSome(crate::aver_generated::domain::value::Val::ValList(crate::aver_generated::domain::value::Val__ValList(rest))))))
}
