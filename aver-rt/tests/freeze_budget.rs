//! The freezing budget (`aver_rt::freeze`): past it nothing more is frozen,
//! and a node above one left unfrozen is left unfrozen too, so it is
//! counted and freed as before. A file of its own: the budget is the
//! process's.
#![cfg(not(feature = "plain-counts"))]

use aver_rt::freeze::{Freeze, Freezer, at_first_fork, limit_budget_for_tests};
use aver_rt::{AverList, Shared};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

static DROPS: AtomicUsize = AtomicUsize::new(0);

struct Leaf;
impl Drop for Leaf {
    fn drop(&mut self) {
        DROPS.fetch_add(1, Ordering::SeqCst);
    }
}
impl Freeze for Leaf {
    fn freeze(&self, _: &mut Freezer) -> bool {
        true
    }
}

#[test]
fn past_the_budget_nodes_stay_counted_and_are_freed() {
    limit_budget_for_tests(4096);
    let list: AverList<Shared<Leaf>> =
        AverList::from_vec((0..10_000).map(|_| Shared::new(Leaf)).collect());
    let all = AtomicBool::new(true);
    static SITE: AtomicBool = AtomicBool::new(false);
    at_first_fork(&SITE, |f| all.store(f.freeze(&list), Ordering::SeqCst));
    assert!(!all.load(Ordering::SeqCst), "4 KB cannot hold 10,000 nodes");
    // Some leaves were frozen before the budget ran out; every other node,
    // the list's own among them, is freed when the list goes.
    drop(list);
    let freed = DROPS.load(Ordering::SeqCst);
    assert!(freed > 9_000 && freed < 10_000, "freed {freed}");
    // The budget is spent: a later place freezes nothing.
    let second = Shared::new(Leaf);
    static OTHER: AtomicBool = AtomicBool::new(false);
    at_first_fork(&OTHER, |f| {
        f.freeze(&second);
    });
    assert_eq!(Shared::strong_count(&second), 1);
}
