//! Freezing (`aver_rt::freeze`) in a build with atomic counts.
#![cfg(not(feature = "plain-counts"))]

use aver_rt::freeze::{Freeze, Freezer, at_first_fork};
use aver_rt::{AverList, AverMap, AverStr, Shared};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

static DROPS: AtomicUsize = AtomicUsize::new(0);

struct Leaf(u64);
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
fn frozen_values_are_shared_by_threads_and_never_freed() {
    let leaf = Shared::new(Leaf(7));
    let pair = Shared::new((leaf.clone(), leaf.clone()));
    let list = AverList::from_vec(
        (0..1000)
            .map(|i| AverStr::from(format!("s{i}").as_str()))
            .collect(),
    );
    let map: AverMap<i64, Shared<(Shared<Leaf>, Shared<Leaf>)>> =
        AverMap::new().insert_owned(1, pair.clone());
    static SITE: AtomicBool = AtomicBool::new(false);
    at_first_fork(&SITE, |f| {
        assert!(f.freeze(&pair));
        assert!(f.freeze(&list));
        assert!(f.freeze(&map));
    });
    // A frozen node reads as shared, and is counted by no one.
    assert!(Shared::strong_count(&leaf) >= 1 << 61);
    std::thread::scope(|s| {
        for _ in 0..8 {
            s.spawn(|| {
                for _ in 0..10_000 {
                    let p = pair.clone();
                    let l = list.clone();
                    let m = map.clone();
                    assert_eq!(p.0.0, 7);
                    drop((p, l, m));
                }
            });
        }
    });
    drop((leaf, pair, list, map));
    assert_eq!(DROPS.load(Ordering::SeqCst), 0);
    // The place froze once: a second walk does not run.
    let mut ran = false;
    at_first_fork(&SITE, |_| ran = true);
    assert!(!ran);
}
