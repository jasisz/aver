//! Nodes born before the first fork (`aver_rt::born`): those alive at the
//! fork are frozen by the scan of their heap, those dropped before it are
//! freed, and those made after it are counted as usual. A process of its
//! own, without the test harness: the born phase is the process's, and the
//! harness's own threads would free blocks and end it.

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
mod born;

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
fn main() {
    use aver_rt::freeze::at_first_fork;
    use born::*;
    use std::sync::atomic::AtomicBool;

    const N: u64 = 100_000;
    let kept = list(0, N);
    let dropped = list(N, N);
    assert_eq!(count(&kept), 1);
    drop(dropped);
    assert_eq!(
        drops(),
        N as usize,
        "a node dropped before the fork is freed"
    );
    assert!(aver_rt::born::active());

    static SITE: AtomicBool = AtomicBool::new(false);
    at_first_fork(&SITE, |_| {});
    assert!(!aver_rt::born::active());
    assert!(
        aver_rt::born::scanned() >= N as usize,
        "every live node is frozen"
    );
    assert!(count(&kept) >= FROZEN);

    // Threads share the frozen list without counting it.
    std::thread::scope(|s| {
        for _ in 0..8 {
            s.spawn(|| {
                for _ in 0..100 {
                    let mine = kept.clone();
                    assert_eq!(sum(&mine), N * (N - 1) / 2);
                }
            });
        }
    });
    drop(kept);
    assert_eq!(drops(), N as usize, "a frozen node is never freed");

    // After the fork nodes are counted and freed as before.
    let after = list(0, 10);
    assert_eq!(count(&after), 1);
    drop(after);
    assert_eq!(drops(), N as usize + 10);
    println!("born_frozen: ok");
}

#[cfg(not(all(feature = "mimalloc", not(feature = "plain-counts"))))]
fn main() {}
