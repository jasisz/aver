//! What the nodes born before the first fork hold (`aver_rt::born`): the
//! strings, large integers, lists, maps, vectors and `Shared` values the
//! owner made before the fork are frozen by the same scan as the nodes of
//! recursive sums, so threads that copy them write no count. A process of
//! its own, without the test harness: the born phase is the process's.

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
mod born;

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
mod held {
    use aver_rt::{AverInt, AverList, AverMap, AverStr, AverVector, Shared};

    pub use crate::born::Tracked;

    #[allow(non_camel_case_types)]
    pub struct H__Held(
        pub AverStr,
        pub AverInt,
        pub AverList<AverStr>,
        pub AverMap<i64, AverStr>,
        pub AverVector<i64>,
        pub Shared<Tracked>,
    );

    impl aver_rt::thin::HandOver for H__Held {
        fn hand_over(self, _: aver_rt::thin::Pending<'_>) {}
    }

    aver_rt::thin_sum! {
        pub H / H__View / H__Pair {
            0 Empty,
            1 Held(H__Held),
        }
    }

    pub fn fields(h: &H) -> &H__Held {
        match h.view() {
            H__View::Held(fields) => fields,
            H__View::Empty => unreachable!(),
        }
    }
}

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
fn main() {
    use aver_rt::freeze::at_first_fork;
    use aver_rt::{AverInt, AverList, AverMap, AverStr, AverVector, NodeCountForTests, Shared};
    use born::*;
    use held::*;
    use std::str::FromStr;
    use std::sync::atomic::AtomicBool;

    const FROZEN: usize = 1 << 63;
    let big = || AverInt::from_str("123456789012345678901234567890123456789").unwrap();

    // The first node of a recursive sum makes this thread the owner; a
    // counted node made before it is allocated as usual.
    let before_owner = AverStr::from("made before the owner");
    let first = list(0, 1);

    // A string another thread makes before the fork is allocated as usual,
    // and ends nothing.
    let remote = std::thread::spawn(|| AverStr::from("made on another thread"))
        .join()
        .unwrap();
    assert!(
        aver_rt::born::active(),
        "another thread's string ends nothing"
    );

    let held = H::Held(H__Held(
        AverStr::from("Nat"),
        big(),
        AverList::from_vec(
            (0..100)
                .map(|i| AverStr::from(format!("s{i}").as_str()))
                .collect(),
        ),
        AverMap::new().insert_owned(1, AverStr::from("one")),
        AverVector::from_vec((0..100).collect()),
        Shared::new(Tracked(7)),
    ));
    let dropped = Shared::new(Tracked(8));
    drop(dropped);
    assert_eq!(drops(), 1, "a born node dropped before the fork is freed");
    let mut grows = AverStr::from("grows");
    let unique_big = big();

    static SITE: AtomicBool = AtomicBool::new(false);
    at_first_fork(&SITE, |_| {});
    assert!(!aver_rt::born::active());
    assert!(aver_rt::born::scanned64() > 0);

    let f = fields(&held);
    let counts = || {
        [
            f.0.node_count_for_tests(),
            f.1.node_count_for_tests(),
            f.2.node_count_for_tests(),
            f.3.node_count_for_tests(),
            f.4.node_count_for_tests(),
            Shared::strong_count(&f.5),
            AverList::get(&f.2, 7).unwrap().node_count_for_tests(),
            f.3.get(&1).unwrap().node_count_for_tests(),
        ]
    };
    let at_fork = counts();
    for (i, c) in at_fork.iter().enumerate() {
        assert!(*c >= FROZEN, "held value {i} frozen by the scan: {c:#x}");
    }
    assert!(unique_big.node_count_for_tests() >= FROZEN);
    assert!(
        remote.node_count_for_tests() < FROZEN,
        "not born, still counted"
    );
    assert!(before_owner.node_count_for_tests() < FROZEN);

    // Threads copy what the frozen node holds: no count changes.
    std::thread::scope(|s| {
        for _ in 0..8 {
            s.spawn(|| {
                for _ in 0..10_000 {
                    let mine = held.clone();
                    let f = fields(&mine);
                    let copies = (
                        f.0.clone(),
                        f.1.clone(),
                        f.2.clone(),
                        f.3.clone(),
                        f.4.clone(),
                        f.5.clone(),
                    );
                    assert_eq!(&*copies.0, "Nat");
                    assert_eq!(copies.1, big());
                    assert_eq!(copies.2.len(), 100);
                    assert_eq!(copies.4.len(), 100);
                    assert_eq!(copies.5.0, 7);
                }
            });
        }
    });
    assert_eq!(counts(), at_fork, "no thread wrote a frozen count");

    // A frozen string its only holder appends to is copied, once.
    assert!(grows.node_count_for_tests() >= FROZEN);
    for _ in 0..3 {
        grows = AverStr::append_buffer(grows, {
            let mut b = aver_rt::Buffer::with_capacity(1);
            b.push_str("!");
            b
        });
    }
    assert_eq!(&*grows, "grows!!!");
    assert_eq!(grows.node_count_for_tests(), 1, "the copy is counted");

    // A frozen value is never freed.
    drop((held, first));
    assert_eq!(drops(), 1, "a frozen node is never freed");

    // After the fork nodes are counted as before.
    let after = Shared::new(Tracked(9));
    assert_eq!(Shared::strong_count(&after), 1);
    drop(after);
    assert_eq!(drops(), 2);
    println!("born_frozen_held: ok");
}

#[cfg(not(all(feature = "mimalloc", not(feature = "plain-counts"))))]
fn main() {}
