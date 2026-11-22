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
/// Two references to one leaf, the value the map holds.
type Pair = Shared<(Shared<Leaf>, Shared<Leaf>)>;

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
    let map: AverMap<i64, Pair> = AverMap::new().insert_owned(1, pair.clone());
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

#[test]
fn a_large_integer_is_frozen_with_what_holds_it() {
    use aver_rt::{AverInt, NodeCountForTests};
    use std::str::FromStr;
    let big = AverInt::from_str("-98765432109876543210987654321098765432109876543210").unwrap();
    let small = AverInt::from(5i64);
    let held = Shared::new((big.clone(), small.clone()));
    assert_eq!(big.node_count_for_tests(), 2);
    let mut f_ran = false;
    aver_rt::freeze::kept(|f| {
        f_ran = true;
        assert!(f.freeze(&held));
    });
    assert!(f_ran);
    let frozen = big.node_count_for_tests();
    assert!(
        frozen >= 1 << 63,
        "the integer's node is frozen: {frozen:#x}"
    );
    assert_eq!(
        small.node_count_for_tests(),
        0,
        "an inline integer has no node"
    );
    std::thread::scope(|s| {
        for _ in 0..8 {
            s.spawn(|| {
                for _ in 0..10_000 {
                    let copy = held.0.clone();
                    assert_eq!(copy, big);
                }
            });
        }
    });
    assert_eq!(big.node_count_for_tests(), frozen, "no thread counted it");
    // Taken apart by value, a frozen integer is copied, not moved out.
    let one = AverInt::from(1i64);
    assert_eq!(big.add(&one).sub(&one), big);
}

#[test]
fn an_integer_literal_in_a_static_is_frozen() {
    use aver_rt::NodeCountForTests;
    let lit: &'static aver_rt::AverInt = aver_rt::int_lit!("123456789012345678901234567890");
    assert!(lit.node_count_for_tests() >= 1 << 63);
    assert_eq!(lit.to_string(), "123456789012345678901234567890");
}
