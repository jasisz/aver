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
        /// reach, half its range, as `Arc`'s check before it aborts. With
        /// atomic counts that is never: see the increment below.
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
                // A frozen node (see `frozen`) is never counted. The mark is
                // the sign bit, so the test is one branch on it (`tbnz` on
                // ARM, `js` on x86) after the load.
                if (count.load(Ordering::Relaxed) as $signed) < 0 {
                    return false;
                }
                // The result is not read, so this is `stadd` on ARM and
                // `lock inc` on x86. There is no overflow test: a count that
                // reaches the sign bit (2^31 or 2^63 owners) reads as frozen
                // from then on, so the node is leaked, never freed early.
                count.fetch_add(1, Ordering::Relaxed);
                false
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
/// The mark is the sign bit of each count, far past any count of owners a
/// program reaches, so a live node never reads as frozen, and testing for it
/// is a branch on one bit.
#[cfg_attr(feature = "plain-counts", allow(dead_code))]
pub(crate) mod frozen {
    pub const F32: u32 = 0x8000_0000;
    /// The sign bit of `usize`: 2^63 on a 64-bit target, 2^31 on a 32-bit
    /// one (wasm32). A node kept in a static starts past it too.
    pub const F64: usize = 1 << (usize::BITS - 1);
}

#[cfg(all(test, not(feature = "plain-counts")))]
mod frozen_tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, AtomicUsize};
    #[test]
    fn frozen_stays_frozen_under_many_incs_and_decs() {
        let c = AtomicU32::new(frozen::F32 + 5);
        for _ in 0..(1u64 << 22) {
            assert!(!inc32(&c));
        }
        assert!(c.load(Ordering::Relaxed) >= frozen::F32);
        for _ in 0..(1u64 << 22) {
            assert!(!dec32(&c));
        }
        assert!(c.load(Ordering::Relaxed) >= frozen::F32);
        // threads mixing both
        std::thread::scope(|s| {
            for _ in 0..8 {
                s.spawn(|| {
                    for i in 0..(1u64 << 20) {
                        if i & 1 == 0 {
                            assert!(!inc32(&c))
                        } else {
                            assert!(!dec32(&c))
                        }
                    }
                });
            }
        });
        assert!(c.load(Ordering::Relaxed) >= frozen::F32);
        std::thread::scope(|s| {
            for _ in 0..8 {
                s.spawn(|| {
                    for _ in 0..(1u64 << 20) {
                        assert!(!dec32(&c))
                    }
                });
            }
        });
        assert!(c.load(Ordering::Relaxed) >= frozen::F32);
        std::thread::scope(|s| {
            for _ in 0..8 {
                s.spawn(|| {
                    for _ in 0..(1u64 << 20) {
                        assert!(!inc32(&c))
                    }
                });
            }
        });
        assert!(c.load(Ordering::Relaxed) >= frozen::F32);
        let d = AtomicUsize::new(frozen::F64 + 1);
        for _ in 0..1000 {
            assert!(!inc(&d));
            assert!(!dec(&d));
            assert!(!dec(&d));
        }
        assert!(d.load(Ordering::Relaxed) >= frozen::F64);
    }
    #[test]
    fn live_counts() {
        let c = AtomicU32::new(1);
        assert!(!inc32(&c));
        assert!(!inc32(&c));
        assert!(!dec32(&c));
        assert!(!dec32(&c));
        assert!(dec32(&c));
        let d = AtomicUsize::new(1);
        assert!(!inc(&d));
        assert!(!dec(&d));
        assert!(dec(&d));
        // racing last drops: exactly one sees the last
        for _ in 0..2000 {
            let c = AtomicU32::new(8);
            let lasts = AtomicU32::new(0);
            std::thread::scope(|s| {
                for _ in 0..8 {
                    s.spawn(|| {
                        if dec32(&c) {
                            lasts.fetch_add(1, Ordering::Relaxed);
                        }
                    });
                }
            });
            assert_eq!(lasts.load(Ordering::Relaxed), 1);
        }
    }
}
