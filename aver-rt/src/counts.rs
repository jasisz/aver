//! How the reference counts of [`crate::thin::Thin`] and [`crate::Shared`]
//! change.
//!
//! By default a count changes as `Arc`'s strong count does: an atomic
//! increment, a `Release` decrement, and an `Acquire` fence before the last
//! owner frees the node. On x86 each of those read-modify-writes is a
//! `lock`ed instruction, which waits for every earlier store and holds back
//! every later load, so a program that walks shared trees spends much of its
//! time on counts it never shares with another thread.
//!
//! With the `plain-counts` feature a count changes with plain instructions,
//! as `Rc`'s does. The code generator turns it on only for a build that runs
//! its Aver code on one thread at a time (`aver compile --single-thread`,
//! `[rust] threads = "one"`): its independent products run in sequence and
//! it uses no service that runs Aver code on threads of its own. A value may still move
//! to another thread, as `main` does onto a thread with a larger stack, but
//! the thread it leaves waits for it (`join`), which orders every change of
//! a count before the next one.

#[cfg(feature = "plain-counts")]
use std::sync::atomic::Ordering;
#[cfg(not(feature = "plain-counts"))]
use std::sync::atomic::{self, Ordering};

macro_rules! counts_for {
    ($atomic:ty, $int:ty, $signed:ty, $inc:ident, $dec:ident, $frozen:ident) => {
        /// A new owner: whether the count went past the most a count may
        /// reach, half its range, as `Arc`'s check before it aborts.
        #[inline(always)]
        pub(crate) fn $inc(count: &$atomic) -> bool {
            #[cfg(feature = "plain-counts")]
            // SAFETY: only this thread changes the count (see the module),
            // so a plain read-modify-write races with nothing. Written as one
            // update of the place and a test of its sign, it compiles to
            // `inc` on memory and a branch on its flags on x86, as the
            // `lock inc` it replaces.
            unsafe {
                let place = count.as_ptr();
                *place = (*place).wrapping_add(1);
                (*place as $signed) < 0
            }
            #[cfg(not(feature = "plain-counts"))]
            {
                // A frozen node (see `frozen`) is never counted.
                if count.load(Ordering::Relaxed) >= frozen::$frozen {
                    return false;
                }
                count.fetch_add(1, Ordering::Relaxed) > <$signed>::MAX as $int
            }
        }

        /// One owner less: whether it was the last, after which every other
        /// owner's use of the node happens before the caller frees it.
        #[inline(always)]
        pub(crate) fn $dec(count: &$atomic) -> bool {
            #[cfg(feature = "plain-counts")]
            // SAFETY: as in the increment; `dec` on memory and a branch on
            // its flags, as `lock dec`.
            unsafe {
                let place = count.as_ptr();
                *place = (*place).wrapping_sub(1);
                *place == 0
            }
            #[cfg(not(feature = "plain-counts"))]
            {
                if count.load(Ordering::Relaxed) >= frozen::$frozen {
                    return false;
                }
                if count.fetch_sub(1, Ordering::Release) != 1 {
                    return false;
                }
                atomic::fence(Ordering::Acquire);
                true
            }
        }
    };
}

counts_for!(std::sync::atomic::AtomicU32, u32, i32, inc32, dec32, F32);
counts_for!(std::sync::atomic::AtomicUsize, usize, isize, inc, dec, F64);

/// Takes a node its caller alone owns, as `Arc::try_unwrap`: whether
/// the count was 1, in which case it is now 0.
#[inline(always)]
pub(crate) fn take32(count: &std::sync::atomic::AtomicU32) -> bool {
    if count.load(Ordering::Relaxed) != 1 {
        return false;
    }
    #[cfg(feature = "plain-counts")]
    // SAFETY: as in the increment.
    unsafe {
        *count.as_ptr() = 0;
        true
    }
    #[cfg(not(feature = "plain-counts"))]
    {
        count
            .compare_exchange(1, 0, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
    }
}

/// Frozen nodes (see [`crate::freeze`]).
///
/// A node whose count is at or past `F32` (a node of a recursive sum, whose
/// count is 32 bits) or `F64` (every other node) is frozen: no one counts
/// it and no one frees it, as no one frees a node kept in a static, whose
/// count is past `F64` too. A build with atomic counts tests for it before
/// each atomic increment and decrement and skips the operation, so threads
/// that share a frozen node do not write to its cache line. A build with
/// `plain-counts` runs on one thread, has nothing to gain, and neither
/// tests nor freezes: its counts are changed as they always were.
///
/// The marks are far past any count of owners a program reaches (a quarter
/// and an eighth of each range), so a live node never reads as frozen.
#[cfg_attr(feature = "plain-counts", allow(dead_code))]
pub(crate) mod frozen {
    pub const F32: u32 = 0x4000_0000;
    pub const F64: usize = 1 << 61;
}
