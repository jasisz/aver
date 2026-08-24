#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Create slot env: Vector<Val> of slotCount, args at 0..n-1.
pub fn buildSlotEnv(
    args: &aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    slotCount: aver_rt::AverInt,
) -> Result<aver_rt::AverVector<crate::aver_generated::domain::value::Val>, AverStr> {
    crate::cancel_checkpoint();
    Ok(
        crate::aver_generated::domain::eval::slots::buildSlotEnvLoop(
            args.clone(),
            match (slotCount).to_u32() {
                Some(__n) => Ok(aver_rt::AverVector::new(
                    __n as usize,
                    crate::aver_generated::domain::value::Val::ValUnit,
                )),
                None => Err(aver_rt::AverStr::from(
                    "Vector.new: size must be between 0 and 4294967295",
                )),
            }?,
            aver_rt::AverInt::from_i64(0),
        ),
    )
}

/// Fill slot env from arg list.
#[inline(always)]
pub fn buildSlotEnvLoop(
    mut args: aver_rt::AverList<crate::aver_generated::domain::value::Val>,
    mut acc: aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    mut idx: aver_rt::AverInt,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    loop {
        crate::cancel_checkpoint();
        aver_list_match!(args, [] => { return acc; }, [a, rest] => { {
            let __tco0 = rest;
            let __tco1 = { let __vec = acc.clone(); match (idx).to_usize() { Some(__idx) if __idx < __vec.len() => __vec.set_unchecked(__idx, a), _ => __vec } };
            let __tco2 = idx.add(&aver_rt::AverInt::from_i64(1));
            args = __tco0;
            acc = __tco1;
            idx = __tco2;
            continue;
        } })
    }
}

/// Look up a variable by slot index. O(1).
#[inline(always)]
pub fn lookupSlot(
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot: aver_rt::AverInt,
) -> Result<crate::aver_generated::domain::value::Val, AverStr> {
    crate::cancel_checkpoint();
    match (slot).to_usize().and_then(|__i| env.get(__i).cloned()) {
        Some(v @ _) => Ok(v),
        None => Err(aver_rt::AverStr::from({
            let mut __b = {
                let mut __b = aver_rt::Buffer::with_capacity(
                    (aver_rt::AverInt::from_i64(36)).to_usize().unwrap_or(0),
                );
                __b.push_str(&AverStr::from("slot out of bounds: "));
                __b
            };
            __b.push_str(&aver_rt::AverStr::from(aver_rt::aver_display(
                &((slot.to_string()).into_aver()),
            )));
            __b
        })),
    }
}

/// Set slot value. O(1).
#[inline(always)]
pub fn setSlot(
    env: &aver_rt::AverVector<crate::aver_generated::domain::value::Val>,
    slot: aver_rt::AverInt,
    v: &crate::aver_generated::domain::value::Val,
) -> aver_rt::AverVector<crate::aver_generated::domain::value::Val> {
    crate::cancel_checkpoint();
    {
        let __vec = env.clone();
        match (slot).to_usize() {
            Some(__idx) if __idx < __vec.len() => __vec.set_unchecked(__idx, v.clone()),
            _ => __vec,
        }
    }
}
