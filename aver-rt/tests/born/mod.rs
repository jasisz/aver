//! A recursive sum for the born-frozen tests (`aver_rt::born`), whose
//! leaves count their drops, and mimalloc as the global allocator, without
//! which nodes are not born in their heap.
#![allow(dead_code)]

use std::sync::atomic::{AtomicUsize, Ordering};

#[global_allocator]
static GLOBAL: aver_rt::MiMalloc = aver_rt::MiMalloc;

pub static DROPS: AtomicUsize = AtomicUsize::new(0);

pub struct Tracked(pub u64);

impl Drop for Tracked {
    fn drop(&mut self) {
        DROPS.fetch_add(1, Ordering::SeqCst);
    }
}

#[allow(non_camel_case_types)]
pub struct L__Cons(pub Tracked, pub L);

impl aver_rt::thin::HandOver for L__Cons {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        let L__Cons(_, rest) = self;
        rest.hand_over(pending);
    }
}

aver_rt::thin_sum! {
    pub L / L__View / L__Pair {
        0 Nil,
        1 Cons(L__Cons),
    }
}

/// A list of `n` nodes, `from` first.
pub fn list(from: u64, n: u64) -> L {
    let mut l = L::Nil;
    for i in (from..from + n).rev() {
        l = L::Cons(L__Cons(Tracked(i), l));
    }
    l
}

pub fn sum(l: &L) -> u64 {
    let mut total = 0;
    let mut at = l.clone();
    loop {
        let next = match at.view() {
            L__View::Nil => return total,
            L__View::Cons(c) => {
                total += c.0.0;
                c.1.clone()
            }
        };
        at = next;
    }
}

/// The count of the list's first node.
pub fn count(l: &L) -> usize {
    aver_rt::NodeId::count(l)
}

/// The count every frozen node has: past the sign bit of its 32-bit count.
pub const FROZEN: usize = 1 << 31;

pub fn drops() -> usize {
    DROPS.load(Ordering::SeqCst)
}
