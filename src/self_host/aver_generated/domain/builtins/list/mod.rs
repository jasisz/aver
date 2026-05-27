#[allow(unused_imports)]
use crate::aver_generated::domain::builtins::helpers::*;
#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Dispatch List.* builtins.
#[inline(always)]
pub fn call(name: AverStr, args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
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
                                                                (38i64) as usize,
                                                            );
                                                        __b.push_str(&AverStr::from(
                                                            "unknown list builtin: ",
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
pub fn builtinListPrepend(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (v, lstV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListPrependInner(&v, &lstV)
    }
}

/// Inner impl of List.prepend.
pub fn builtinListPrependInner(v: &Val, lstV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        aver_rt::AverList::prepend(v.clone(), &items),
    ))
}

/// List.len(list) -> length as Int.
pub fn builtinListLen(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValInt(
        (items.len() as i64),
    ))
}

/// List.take(list, n) -> first n elements.
pub fn builtinListTake(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, nV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListTakeInner(&lstV, &nV)
    }
}

/// Inner impl of List.take.
pub fn builtinListTakeInner(lstV: &Val, nV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    let count = crate::aver_generated::domain::builtins::helpers::expectInt(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::builtins::list::listTake(&items, count),
    ))
}

/// Keep the first count elements; negative counts produce empty lists.
#[inline(always)]
pub fn listTake(items: &aver_rt::AverList<Val>, count: i64) -> aver_rt::AverList<Val> {
    crate::cancel_checkpoint();
    if (count > 0i64) {
        aver_list_match!(items.clone(), [] => aver_rt::AverList::empty(), [item, rest] => aver_rt::AverList::prepend(item, &crate::aver_generated::domain::builtins::list::listTake(&rest, (count - 1i64))))
    } else {
        aver_rt::AverList::empty()
    }
}

/// List.drop(list, n) -> all but the first n elements.
pub fn builtinListDrop(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, nV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListDropInner(&lstV, &nV)
    }
}

/// Inner impl of List.drop.
pub fn builtinListDropInner(lstV: &Val, nV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    let count = crate::aver_generated::domain::builtins::helpers::expectInt(nV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::builtins::list::listDrop(items, count),
    ))
}

/// Skip the first count elements; negative counts leave the list unchanged.
#[inline(always)]
pub fn listDrop(mut items: aver_rt::AverList<Val>, mut count: i64) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        return if (count <= 0i64) {
            items
        } else {
            aver_list_match!(items, [] => aver_rt::AverList::empty(), [_item, rest] => { {
            let __tmp1 = (count - 1i64);
            items = rest;
            count = __tmp1;
            continue;
        } })
        };
    }
}

/// List.reverse(list) -> reversed list.
pub fn builtinListReverse(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        items.reverse(),
    ))
}

/// List.concat(a, b) -> concatenated list.
pub fn builtinListConcat(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListConcatInner(&aV, &bV)
    }
}

/// Inner impl of List.concat.
pub fn builtinListConcatInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let aItems = crate::aver_generated::domain::builtins::helpers::expectList(aV)?;
    let bItems = crate::aver_generated::domain::builtins::helpers::expectList(bV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        aver_rt::AverList::concat(&aItems, &bItems),
    ))
}

/// List.contains(list, value) -> Bool.
pub fn builtinListContains(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (lstV, needle) = pair;
        crate::aver_generated::domain::builtins::list::builtinListContainsInner(&lstV, &needle)
    }
}

/// Inner impl of List.contains.
pub fn builtinListContainsInner(lstV: &Val, needle: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let items = crate::aver_generated::domain::builtins::helpers::expectList(lstV)?;
    Ok(crate::aver_generated::domain::value::Val::ValBool(
        crate::aver_generated::domain::builtins::list::listContainsVal(items, needle.clone()),
    ))
}

/// Check if a value is in a list by repr comparison.
#[inline(always)]
pub fn listContainsVal(mut items: aver_rt::AverList<Val>, mut needle: Val) -> bool {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(items, [] => false, [v, rest] => { if (crate::aver_generated::domain::value::valRepr(&v) == crate::aver_generated::domain::value::valRepr(&needle)) { true } else { {
            items = rest;
            continue;
        } } });
    }
}

/// List.zip(a, b) -> list of tuples.
pub fn builtinListZip(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let pair = crate::aver_generated::domain::builtins::helpers::twoArgs(args)?;
    {
        let (aV, bV) = pair;
        crate::aver_generated::domain::builtins::list::builtinListZipInner(&aV, &bV)
    }
}

/// Inner impl of List.zip.
pub fn builtinListZipInner(aV: &Val, bV: &Val) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let aItems = crate::aver_generated::domain::builtins::helpers::expectList(aV)?;
    let bItems = crate::aver_generated::domain::builtins::helpers::expectList(bV)?;
    Ok(crate::aver_generated::domain::value::Val::ValList(
        crate::aver_generated::domain::builtins::list::zipLists(&aItems, &bItems),
    ))
}

/// Zip two lists into list of ValTuple.
#[inline(always)]
pub fn zipLists(a: &aver_rt::AverList<Val>, b: &aver_rt::AverList<Val>) -> aver_rt::AverList<Val> {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::builtins::list::zipListsAcc(
        a.clone(),
        b.clone(),
        aver_rt::AverList::empty(),
    )
}

/// Accumulate zipped pairs in reverse, then reverse at end.
#[inline(always)]
pub fn zipListsAcc(
    mut a: aver_rt::AverList<Val>,
    mut b: aver_rt::AverList<Val>,
    mut acc: aver_rt::AverList<Val>,
) -> aver_rt::AverList<Val> {
    loop {
        crate::cancel_checkpoint();
        let reversed = acc.reverse();
        return aver_list_match!(a, [] => reversed, [x, xs] => { aver_list_match!(b, [] => reversed, [y, ys] => { {
            let __tmp2 = aver_rt::AverList::prepend(crate::aver_generated::domain::value::Val::ValTuple(aver_rt::AverList::from_vec(vec![x, y])), &acc);
            a = xs;
            b = ys;
            acc = __tmp2;
            continue;
        } }) });
    }
}

/// List.head(list) -> Option of first element.
pub fn builtinListHead(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    aver_list_match!(items, [] => Ok(Val::ValNone.clone()), [h, rest] => Ok(crate::aver_generated::domain::value::Val::ValSome(std::sync::Arc::new(h))))
}

/// List.tail(list) -> Option of rest.
pub fn builtinListTail(args: &aver_rt::AverList<Val>) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    let v = crate::aver_generated::domain::builtins::helpers::oneArg(args)?;
    let items = crate::aver_generated::domain::builtins::helpers::expectList(&v)?;
    aver_list_match!(items, [] => Ok(Val::ValNone.clone()), [h, rest] => Ok(crate::aver_generated::domain::value::Val::ValSome(std::sync::Arc::new(crate::aver_generated::domain::value::Val::ValList(rest)))))
}
