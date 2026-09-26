//! The host-driven coordinator door. Native execution and JS workers share
//! this post-wait turn; the external host owns only waiting and scheduling.

use super::{CoordinatorStop, WAIT_ENDS, WAIT_STARTS, effects};

/// The half of a turn after its wait: the clock reading, the serve path and
/// the seating at the turn boundary. `keys` are the wait-set keys the wait
/// reported; the plan they index is rebuilt from the same run, so a host
/// that waited on `__workHostWaitSet(run)` hands back keys of that set. In a
/// program that reads `Run.lastTurn` it first marks that the wait returned.
pub(super) fn write_step(turn_effects: &[String], fails: bool, measures: bool) -> String {
    format!(
        "\nfn __workHostStep(run: __Run, keys: List<Int>) -> __Run\n    ? \"The post-wait half of one turn; external hosts deliver readiness after returning to their event loop.\"\n{}{}    ready = __readySlots(__waitPlan(run).owners, keys, [])\n    timed = __Run.update(run, now = Time.unixMs())\n    ids = Map.keys(timed.slots)\n    served = __serveEach(timed, ready, ids)\n    {}\n",
        effects(turn_effects),
        if measures {
            format!("    {WAIT_ENDS}()\n")
        } else {
            String::new()
        },
        if fails {
            "__failedAfter(__seatFamilies(served))"
        } else {
            "__seatFamilies(served)"
        },
    )
}

/// `measures`: the program reads `Run.lastTurn`, so the observation the host
/// makes right before it waits also marks that the wait starts.
pub(super) fn write_exports(
    main_effects: &[String],
    cancels: bool,
    coordinator_stop: CoordinatorStop,
    measures: bool,
) -> String {
    let (observe_effects, stop_observation) = match (coordinator_stop, measures) {
        (CoordinatorStop::HostSignal, false) => (
            "    ! [Process.stopRequested]\n".to_string(),
            "Process.stopRequested()",
        ),
        (CoordinatorStop::PolicyOnly, false) => (String::new(), "false"),
        (CoordinatorStop::HostSignal, true) => (
            format!("    ! [Process.stopRequested, {WAIT_STARTS}]\n    {WAIT_STARTS}()\n"),
            "Process.stopRequested()",
        ),
        (CoordinatorStop::PolicyOnly, true) => (
            format!("    ! [{WAIT_STARTS}]\n    {WAIT_STARTS}()\n"),
            "false",
        ),
    };
    format!(
        "\nfn __workHostStart() -> __Run\n    ? \"Seat the processes once for an external event-loop driver.\"\n{}    __start()\n\nfn __workHostStopped(run: __Run) -> Bool\n    ? \"Whether the run is over.\"\n    __stopped(run)\n\nfn __workHostObserve(run: __Run) -> __Run\n    ? \"Observe cooperative stopping before the host waits.\"\n{observe_effects}    __Run.update(run, stopping = {stop_observation})\n\nfn __workHostWaitSet(run: __Run) -> Map<Int, Wait.Item>\n    ? \"What the host waits on this turn.\"\n    __waitPlan(run).items\n\nfn __workHostTimeout(run: __Run) -> Int\n    ? \"How long the host may wait this turn.\"\n    __timeout(run)\n\nfn __workHostFinish(run: __Run) -> Result<Unit, String>\n    ? \"Cancel the jobs parked requests wait on when the host driver finishes.\"\n{}    __over(run)\n",
        effects(main_effects),
        if cancels { "    ! [Work.cancel]\n" } else { "" },
    )
}
