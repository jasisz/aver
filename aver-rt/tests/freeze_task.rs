//! Freezing a task begun with `Work.begin` (`aver_rt::freeze`): the task
//! moves to a job's thread as it is, and may outlive the code that began
//! it. A frozen node is never freed, so that is safe.
#![cfg(not(feature = "plain-counts"))]

use aver_rt::freeze::at_first_fork;
use aver_rt::{AverList, AverStr, Shared};
use std::sync::atomic::AtomicBool;
use std::sync::mpsc;

#[test]
fn a_frozen_task_outlives_the_code_that_began_it() {
    static PLACE: AtomicBool = AtomicBool::new(false);
    let (go, wait) = mpsc::channel::<()>();
    let job = {
        let task = AverList::from_vec(
            (0..10_000)
                .map(|i| AverStr::from(format!("item {i}").as_str()))
                .collect(),
        );
        let shared = Shared::new(task.clone());
        at_first_fork(&PLACE, |f| assert!(f.freeze(&shared)));
        let job = std::thread::spawn(move || {
            wait.recv().unwrap();
            // The beginner has dropped every copy of its own by now.
            assert_eq!(shared.len(), 10_000);
            assert_eq!(&*shared.get(9_999).unwrap().clone(), "item 9999");
            Shared::strong_count(&shared)
        });
        drop(task);
        job
    };
    go.send(()).unwrap();
    // Still read as shared: no one counts a frozen node.
    assert!(job.join().unwrap() > 1 << 20);
    // A later task begun at the same place is not frozen.
    let later = Shared::new(AverList::from_vec(vec![AverStr::from("x")]));
    at_first_fork(&PLACE, |f| assert!(f.freeze(&later)));
    assert_eq!(Shared::strong_count(&later), 1);
}
