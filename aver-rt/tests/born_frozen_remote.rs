//! A second thread that gives back a block before the first fork ends the
//! born phase (`aver_rt::born`): no scan runs, and the born nodes stay
//! counted and are freed as any other. A process of its own, without the
//! test harness.

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
mod born;

#[cfg(all(feature = "mimalloc", not(feature = "plain-counts")))]
fn main() {
    use aver_rt::freeze::at_first_fork;
    use born::*;
    use std::sync::atomic::AtomicBool;

    const N: u64 = 10_000;
    let kept = list(0, N);
    let given = list(N, N);
    assert!(aver_rt::born::active());
    std::thread::spawn(move || drop(given)).join().unwrap();
    assert_eq!(drops(), N as usize);
    assert!(!aver_rt::born::active(), "a free on another thread ends it");

    static SITE: AtomicBool = AtomicBool::new(false);
    at_first_fork(&SITE, |_| {});
    assert_eq!(aver_rt::born::scanned(), 0);
    assert_eq!(count(&kept), 1, "a born node stays counted");
    drop(kept);
    assert_eq!(drops(), 2 * N as usize);
    println!("born_frozen_remote: ok");
}

#[cfg(not(all(feature = "mimalloc", not(feature = "plain-counts"))))]
fn main() {}
