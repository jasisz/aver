//! The host-driven coordinator door. Native execution and JS workers share
//! this post-wait turn; the external host owns only waiting and scheduling.

use super::{Job, RunPlan, bare, effects, marker_variant};

pub(super) fn write_step(plan: &RunPlan, jobs: &[Job], turn_effects: &[String]) -> String {
    let has_jobs = !jobs.is_empty();
    let mut out = String::new();
    // The host-driven and blocking drivers share the exact post-wait turn.
    let job_turn = if has_jobs {
        // One table across the kinds: every kind takes what the wait
        // reported, then every kind starts while there is room. The
        // takes run first because a landed job frees a slot of room this
        // turn can already fill.
        let binder = |prefix: &str, index: usize| {
            if jobs.len() == 1 {
                prefix.to_string()
            } else {
                format!("{prefix}{index}")
            }
        };
        let mut body = String::new();
        let mut run_of = "served".to_string();
        for (index, job) in jobs.iter().enumerate() {
            let name = binder("taken", index);
            body.push_str(&format!(
                "    {name} = __takeEach{upper}({run_of}, ready)\n",
                upper = marker_variant(&job.capability)
            ));
            run_of = name;
        }
        for (index, job) in jobs.iter().enumerate() {
            let upper = marker_variant(&job.capability);
            if index + 1 == jobs.len() {
                body.push_str(&format!("    __startJobs{upper}({run_of})\n"));
            } else {
                let name = binder("started", index);
                body.push_str(&format!("    {name} = __startJobs{upper}({run_of})\n"));
                run_of = name;
            }
        }
        body
    } else {
        "    served\n".to_string()
    };
    // Answer modules may themselves read Process or Wait; retain their full
    // declared effect set even though the driver's own poll moved outside.
    let step_effects = turn_effects.to_vec();
    out.push_str(&format!(
        "\nfn __workHostStep(run: __Run, ready: List<Int>) -> __Run\n    ? \"The post-wait half of one turn; external hosts deliver readiness after returning to their event loop.\"\n{}    timed = __Run.update(run, now = Time.unixMs())\n    served = __serveEach(timed, ready, {}(__view(timed, ready)))\n{}",
        effects(&step_effects), bare(&plan.policies.order), job_turn,
    ));
    out
}

pub(super) fn write_exports(
    seated: &str,
    stopping: &str,
    main_effects: &[String],
    has_jobs: bool,
) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "\nfn __workHostStart() -> __Run\n    ? \"Seat the processes once for an external event-loop driver.\"\n{}    {seated}\n\nfn __workHostStopped(run: __Run) -> Bool\n    {stopping}\n\nfn __workHostObserve(run: __Run) -> __Run\n    ? \"Observe cooperative stopping before the host waits.\"\n    ! [Process.stopRequested]\n    __Run.update(run, stopping = Process.stopRequested())\n\nfn __workHostWaitSet(run: __Run) -> Map<Int, Wait.Item>\n    __waitSet(run, Map.keys(run.slots), {{}})\n\nfn __workHostTimeout(run: __Run) -> Int\n    __timeout(run)\n\nfn __workHostFinish(run: __Run) -> Result<Unit, String>\n    ? \"Cancel outstanding jobs when the host driver finishes.\"\n{}    __over(run)\n",
        effects(main_effects), if has_jobs { "    ! [Work.cancel]\n" } else { "" },
    ));
    out
}
