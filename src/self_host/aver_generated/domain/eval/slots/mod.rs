#[allow(unused_imports)]
use crate::*;

/// Create slot env: Vector<Val> of slotCount, args at 0..n-1.
pub fn buildSlotEnv(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    slotCount @ _: aver_rt::AverInt,
) -> Result<aver_rt::AverVector<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    Ok({
        let __arg2 @ _ = match aver_rt::checked_vector_size(&(slotCount)) {
            Some(__n) => Ok(aver_rt::AverVector::new(
                __n,
                crate::aver_generated::domain::value::Val::ValUnit,
            )),
            None => Err(aver_rt::AverStr::from(aver_rt::vector_size_error_message())),
        }?;
        crate::aver_generated::domain::eval::slots::buildSlotEnvLoop(
            args,
            __arg2,
            aver_rt::AverInt::from_i64(0),
        )
    })
}

/// Fill slot env from arg list.
#[inline(always)]
pub fn buildSlotEnvLoop(
    args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut idx @ _: aver_rt::AverInt,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    let mut __hold0: aver_rt::AverList<crate::aver_generated::domain::value::Val>;
    let mut args @ _: &aver_rt::AverList<crate::aver_generated::domain::value::Val> = args;
    loop {
        crate::cancel_checkpoint();
        {
            let __list_subject = args;
            match aver_rt::list_uncons(__list_subject) {
                Some((a, rest)) => {
                    let __tco0 = rest;
                    let __tco1 = {
                        let __vec = acc;
                        match (idx).to_usize() {
                            Some(__idx) if __idx < __vec.len() => {
                                __vec.set_unchecked(__idx, a.clone())
                            }
                            _ => __vec,
                        }
                    };
                    let __tco2 = idx.add(&aver_rt::AverInt::from_i64(1));
                    __hold0 = __tco0;
                    args = &__hold0;
                    acc = __tco1;
                    idx = __tco2;
                    continue;
                }
                None => {
                    return acc;
                }
            }
        }
    }
}

/// Look up a variable by slot index. O(1).
#[inline(always)]
pub fn lookupSlot(
    env @ _: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot @ _: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match (slot).to_usize().and_then(|__i| env.get(__i).cloned()) {
        Some(v @ _) => Ok(v),
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(36usize);
                __b.push_str(&::aver_rt::str_lit!("slot out of bounds: "));
                __b
            };
            __b.push_display(&(aver_rt::display_str(&(slot))));
            __b
        })),
    }
}

/// Set slot value. O(1).
#[inline(always)]
pub fn setSlot(
    mut env @ _: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot @ _: aver_rt::AverInt,
    v @ _: &crate::aver_generated::domain::value::Val,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    {
        let __vec = env;
        match (slot).to_usize() {
            Some(__idx) if __idx < __vec.len() => __vec.set_unchecked(__idx, v.clone()),
            _ => __vec,
        }
    }
}
