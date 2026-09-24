//! Finite histories of this program's coordinator, after initial seating.
//!
//! Events carry observations supplied by effects, then call the same pure
//! transitions as the live driver. Admissibility checks control preconditions,
//! never the safety property being proved. This is an overapproximation of
//! runtime histories: it does not prove that lowering preserves source traces,
//! nor impose any consistency assumption on an answer module's returned state.

use super::{Answer, Job, ProcessProtocol, marker_variant};

struct Event {
    name: String,
    fields: Vec<(String, String)>,
    guard: String,
    step: String,
}

impl Event {
    fn new(name: &str, fields: &[(&str, &str)], guard: &str, step: &str) -> Self {
        Self {
            name: name.into(),
            fields: fields
                .iter()
                .map(|(n, t)| (n.to_string(), t.to_string()))
                .collect(),
            guard: guard.into(),
            step: step.into(),
        }
    }

    fn pattern(&self) -> String {
        format!(
            "__HistoryEvent.{}({})",
            self.name,
            self.fields
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

pub(super) fn write(protocols: &[ProcessProtocol], answers: &[Answer], jobs: &[Job]) -> String {
    let mut events = vec![
        Event::new(
            "Observe",
            &[("now", "Int"), ("stopping", "Bool")],
            "true",
            "__Run.update(run, now = now, stopping = stopping)",
        ),
        Event::new(
            "Later",
            &[("id", "Int"), ("wake", "Wait.Wake")],
            "true",
            "__park(run, id, wake)",
        ),
        Event::new(
            "Then",
            &[
                ("id", "Int"),
                ("wake", "Wait.Wake"),
                ("answer", "__ThenAnswer"),
            ],
            "true",
            "__parkThen(run, id, wake, answer)",
        ),
    ];
    for answer in answers {
        events.push(Event::new(
            &format!("State{}", marker_variant(&answer.field)),
            &[("state", &answer.state)],
            "true",
            &format!("__Run.update(run, {} = state)", answer.field),
        ));
    }
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        events.push(Event::new(
            &format!("Settle{upper}"),
            &[
                ("id", "Int"),
                ("seq", "Int"),
                ("outcome", &protocol.outcome),
            ],
            "Bool.and(seq >= 0, Bool.or(__staleInstance(run, id, seq), Map.has(run.slots, id)))",
            &format!("__settle{upper}(run, id, seq, outcome)"),
        ));
    }
    for job in jobs {
        let upper = marker_variant(&job.capability);
        events.push(Event::new(
            &format!("Start{upper}"),
            &[("task", &job.task_type), ("handle", "Work.Job")],
            "true",
            &format!("__jobSeated{upper}(run, task, handle)"),
        ));
        events.push(Event::new(
            &format!("Report{upper}"),
            &[
                ("key", "Int"),
                (
                    "taken",
                    &format!("Result<Option<{}>, String>", job.payload_type),
                ),
            ],
            "true",
            &format!("__reported{upper}(run, key, taken)"),
        ));
    }
    if !jobs.is_empty() {
        events.push(Event::new(
            "Cancel",
            &[("key", "Int")],
            "true",
            "__cancelled(run, key, Unit)",
        ));
    }
    let mut out = String::from("\ntype __HistoryEvent\n");
    for event in &events {
        out.push_str(&format!(
            "    {}({})\n",
            event.name,
            event
                .fields
                .iter()
                .map(|(_, ty)| ty.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    out.push_str("\nfn __historyAllowed(run: __Run, event: __HistoryEvent) -> Bool\n    ? \"Control preconditions: a settlement carries a nonnegative request instance, with a current instance seated. A start needs nothing: at the job limit the engine queues a job rather than refusing it. Other observations are unrestricted, including arbitrary provider state and spurious job reports. Safety is proved over this superset of runtime histories.\"\n    match event\n");
    for event in &events {
        out.push_str(&format!("        {} -> {}\n", event.pattern(), event.guard));
    }
    out.push_str("\nfn __historyStep(run: __Run, event: __HistoryEvent) -> __Run\n    ? \"One observed coordinator transition, using the live driver's pure functions. Initial seating happens before a history; starting a worker never seats another process. A failed begin or an idle turn is represented by no transition.\"\n    match event\n");
    for event in &events {
        out.push_str(&format!("        {} -> {}\n", event.pattern(), event.step));
    }
    out.push_str(r#"
fn __historyRun(run: __Run, events: List<__HistoryEvent>) -> __Run
    ? "Apply any finite list of observations, in order. The laws quantify over the entire list type, not the verify samples."
    match events
        [] -> run
        [event, ..rest] -> __historyRun(__historyStep(run, event), rest)

fn __historyAdmissible(run: __Run, events: List<__HistoryEvent>) -> Bool
    ? "Each event meets its control preconditions in the state left by its predecessors. This predicate does not assert a slot bound, a job bound, or instance retirement."
    match events
        [] -> true
        [event, ..rest] -> match __historyAllowed(run, event)
            false -> false
            true -> __historyAdmissible(__historyStep(run, event), rest)

fn __retired(run: __Run, id: Int, seq: Int) -> Bool
    ? "A nonnegative instance is past: the process is gone, or its request number has advanced. Initial seating never repeats during a history."
    current = __current(run, id)
    Bool.and(seq >= 0, Bool.or(current == 0 - 1, current > seq))

verify __historyStep law noNewProcesses
    given run: __Run = [__sampleRun()]
    given event: __HistoryEvent = [__HistoryEvent.Observe(0, false), __HistoryEvent.Later(1, Wait.Wake.NextTurn)]
    when __historyAllowed(run, event)
    using []
    Map.len(__historyStep(run, event).slots) <= Map.len(run.slots) holds

verify __historyRun law noNewProcesses
    given run: __Run = [__sampleRun()]
    given events: List<__HistoryEvent> = [[], [__HistoryEvent.Later(1, Wait.Wake.After(5)), __HistoryEvent.Observe(9, false)]]
    when __historyAdmissible(run, events)
    using [__historyStep.noNewProcesses]
    Map.len(__historyRun(run, events).slots) <= Map.len(run.slots) holds

verify __historyStep law retiredInstanceStaysRetired
    given run: __Run = [__sampleRun()]
    given event: __HistoryEvent = [__HistoryEvent.Observe(0, false), __HistoryEvent.Later(1, Wait.Wake.NextTurn)]
    given id: Int = [1, 9]
    given seq: Int = [0, 1]
    when __historyAllowed(run, event)
    when __retired(run, id, seq)
    using []
    __retired(__historyStep(run, event), id, seq) holds

verify __historyRun law retiredInstanceNeverReturns
    given run: __Run = [__sampleRun()]
    given events: List<__HistoryEvent> = [[], [__HistoryEvent.Later(1, Wait.Wake.After(5)), __HistoryEvent.Observe(9, false)]]
    given id: Int = [1, 9]
    given seq: Int = [0, 1]
    when __historyAdmissible(run, events)
    when __retired(run, id, seq)
    using [__historyStep.retiredInstanceStaysRetired]
    __retired(__historyRun(run, events), id, seq) holds
"#);
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            r#"
verify __settle{upper} law answeringRetiresTheInstance
    given run: __Run = [__sampleRun()]
    given id: Int = [1, 9]
    given seq: Int = [0, 1]
    given outcome: {outcome} = [{outcome}.Done(Unit)]
    when seq >= 0
    when seq == __current(run, id)
    using []
    __retired(__settle{upper}(run, id, seq, outcome), id, seq) holds
"#,
            outcome = protocol.outcome
        ));
    }
    out
}
