#[allow(unused_imports)]
use crate::aver_generated::domain::value::*;
#[allow(unused_imports)]
use crate::*;

/// Create slot env: Vector<Val> of slotCount, args at 0..n-1.
#[inline(always)]
pub fn buildSlotEnv(args: &aver_rt::AverList<Val>, slotCount: i64) -> aver_rt::AverVector<Val> {
    crate::cancel_checkpoint();
    buildSlotEnvLoop(
        args.clone(),
        aver_rt::AverVector::new(slotCount as usize, Val::ValUnit.clone()),
        0i64,
    )
}

/// Fill slot env from arg list.
#[inline(always)]
pub fn buildSlotEnvLoop(
    mut args: aver_rt::AverList<Val>,
    mut acc: aver_rt::AverVector<Val>,
    mut idx: i64,
) -> aver_rt::AverVector<Val> {
    loop {
        crate::cancel_checkpoint();
        return aver_list_match!(args, [] => acc, [a, rest] => { {
            let __tmp1 = { let __vec = acc.clone(); let __idx = idx as usize; if __idx < __vec.len() { __vec.set_unchecked(__idx, a) } else { __vec } };
            let __tmp2 = (idx + 1i64);
            args = rest;
            acc = __tmp1;
            idx = __tmp2;
            continue;
        } });
    }
}

/// Look up a variable by slot index. O(1).
#[inline(always)]
pub fn lookupSlot(env: &aver_rt::AverVector<Val>, slot: i64) -> Result<Val, AverStr> {
    crate::cancel_checkpoint();
    match env.get(slot as usize).cloned() {
        Some(v) => Ok(v),
        None => Err(AverStr::from(format!(
            "slot out of bounds: {}",
            aver_rt::aver_display(&(slot.to_string()).into_aver())
        ))),
    }
}

/// Set slot value. O(1).
#[inline(always)]
pub fn setSlot(env: &aver_rt::AverVector<Val>, slot: i64, v: &Val) -> aver_rt::AverVector<Val> {
    crate::cancel_checkpoint();
    {
        let __vec = env.clone();
        let __idx = slot as usize;
        if __idx < __vec.len() {
            __vec.set_unchecked(__idx, v.clone())
        } else {
            __vec
        }
    }
}
