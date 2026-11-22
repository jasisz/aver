//! The scan of the born heaps (`aver_rt::born`) while other threads drop
//! born nodes, of recursive sums and `Shared` ones: each node is either
//! frozen (and never freed) or freed (and never frozen), and no freed block
//! is written. A process of its own, without the test harness.

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
mod born;

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
fn main() {
    use aver_rt::freeze::at_first_fork;
    use born::*;
    use std::sync::atomic::AtomicBool;
    use std::sync::{Barrier, Mutex};

    const N: u64 = 200_000;
    const M: u64 = 50_000;
    const THREADS: usize = 4;
    type Held = (L, L, Vec<aver_rt::Shared<Tracked>>);
    // The threads start before the first node, so that what starting them
    // allocates and gives back happens before the born phase has an owner.
    let slots: Vec<Mutex<Option<Held>>> = (0..THREADS).map(|_| Mutex::new(None)).collect();
    let ready = Barrier::new(THREADS + 1);
    let handed = Barrier::new(THREADS + 1);
    let start = Barrier::new(THREADS + 1);
    let mut path = "";
    let kept = std::thread::scope(|s| {
        for slot in &slots {
            let (ready, handed, start) = (&ready, &handed, &start);
            s.spawn(move || {
                ready.wait();
                handed.wait();
                let pair = slot.lock().unwrap().take();
                start.wait();
                // A little later than the scan starts, mostly: a free then
                // waits for the scan to end.
                std::thread::sleep(std::time::Duration::from_micros(200));
                // Each thread holds the only owner of a list, one more of
                // the kept list and the only owners of `M` `Shared` nodes,
                // and drops them all as the scan starts.
                drop(pair);
            });
        }
        ready.wait();
        let kept = list(0, N);
        for (t, slot) in slots.iter().enumerate() {
            let shared = (0..M).map(|i| aver_rt::Shared::new(Tracked(i))).collect();
            *slot.lock().unwrap() = Some((list(N * (t as u64 + 1), N), kept.clone(), shared));
        }
        handed.wait();
        start.wait();
        path = if aver_rt::born::active() {
            "scan"
        } else {
            "ended"
        };
        static SITE: AtomicBool = AtomicBool::new(false);
        at_first_fork(&SITE, |_| {});
        kept
    });
    let frozen = aver_rt::born::scanned();
    let freed = drops();
    // The threads' lists: every node either freed or frozen (a node frozen
    // before its owner dropped it stays, with the rest of its list).
    assert!(freed <= (N + M) as usize * THREADS);
    assert!(
        frozen == 0 || frozen >= N as usize,
        "the kept list is frozen whole when the scan runs"
    );
    assert_eq!(sum(&kept), N * (N - 1) / 2);
    println!("born_frozen_race: ok ({path}), frozen {frozen}, freed {freed}");
}

#[cfg(not(all(feature = "mimalloc", not(feature = "plain-counts"))))]
fn main() {}
