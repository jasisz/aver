//! The host-driven coordinator door. Native execution and JS workers share
//! this post-wait turn; the external host owns only waiting and scheduling.

use super::{CoordinatorStop, effects};

/// The half of a turn after its wait: the clock reading, the serve path and
/// the seating at the turn boundary. `keys` are the wait-set keys the wait
/// reported; the plan they index is rebuilt from the same run, so a host
/// that waited on `__workHostWaitSet(run)` hands back keys of that set.
pub(super) fn write_step(turn_effects: &[String], fails: bool) -> String {
    format!(
        "\nfn __workHostStep(run: __Run, keys: List<Int>) -> __Run\n    ? \"The post-wait half of one turn; external hosts deliver readiness after returning to their event loop.\"\n{}    ready = __readySlots(__waitPlan(run).owners, keys, [])\n    timed = __Run.update(run, now = Time.unixMs())\n    ids = Map.keys(timed.slots)\n    served = __serveEach(timed, ready, ids)\n    {}\n",
        effects(turn_effects),
        if fails {
            "__failedAfter(__seatFamilies(served))"
        } else {
            "__seatFamilies(served)"
        },
    )
}

pub(super) fn write_exports(
    main_effects: &[String],
    cancels: bool,
    coordinator_stop: CoordinatorStop,
) -> String {
    let (observe_effects, stop_observation) = match coordinator_stop {
        CoordinatorStop::HostSignal => {
            ("    ! [Process.stopRequested]\n", "Process.stopRequested()")
        }
        CoordinatorStop::PolicyOnly => ("", "false"),
    };
    format!(
        "\nfn __workHostStart() -> __Run\n    ? \"Seat the processes once for an external event-loop driver.\"\n{}    __start()\n\nfn __workHostStopped(run: __Run) -> Bool\n    ? \"Whether the run is over.\"\n    __stopped(run)\n\nfn __workHostObserve(run: __Run) -> __Run\n    ? \"Observe cooperative stopping before the host waits.\"\n{observe_effects}    __Run.update(run, stopping = {stop_observation})\n\nfn __workHostWaitSet(run: __Run) -> Map<Int, Wait.Item>\n    ? \"What the host waits on this turn.\"\n    __waitPlan(run).items\n\nfn __workHostTimeout(run: __Run) -> Int\n    ? \"How long the host may wait this turn.\"\n    __timeout(run)\n\nfn __workHostFinish(run: __Run) -> Result<Unit, String>\n    ? \"Cancel the jobs parked requests wait on when the host driver finishes.\"\n{}    __over(run)\n",
        effects(main_effects),
        if cancels { "    ! [Work.cancel]\n" } else { "" },
    )
}
