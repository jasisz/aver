//! Saved answers share the ordinary slot, wake gate and instance lifetime.
//! Each constructor is monomorphic in one process's operation signature.

use super::{ProcessProtocol, marker_variant};

pub(super) fn declarations(protocols: &[ProcessProtocol]) -> String {
    let mut out = "type __ThenAnswer\n".to_string();
    let mut count = 0;
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        for kind in &protocol.kinds {
            if kind.operation.is_some() {
                count += 1;
                out.push_str(&format!(
                    "    {upper}{}({}, {})\n",
                    kind.name,
                    kind.state,
                    kind.answer_type.as_deref().unwrap_or("Unit")
                ));
            }
        }
    }
    if count == 0 {
        out.push_str("    NoAnswer\n");
    }
    out.push_str("\ntype __Dispatch\n    Parked\n    Retry\n    Complete(__ThenAnswer)\n\n");
    out
}

pub(super) fn dispatch(protocols: &[ProcessProtocol], effects: &[String]) -> String {
    let effects = if effects.is_empty() {
        String::new()
    } else {
        format!("    ! [{}]\n", effects.join(", "))
    };
    let mut out = r#"
fn __parkedWith(slot: __Slot, wake: Wait.Wake, now: Int, answer: Option<__ThenAnswer>) -> __Slot
    ? "Park the same request and instance, keeping the answer to reveal when its wake fires."
    __Slot.update(__parked(slot, wake, now), answer = answer)

fn __parkThen(run: __Run, id: Int, wake: Wait.Wake, answer: __ThenAnswer) -> __Run
    ? "Keep the module's returned state and save its answer beside the unchanged request."
    match Map.get(run.slots, id)
        Option.None -> run
        Option.Some(slot) -> __Run.update(run, slots = Map.set(run.slots, id, __parkedWith(slot, wake, run.now, Option.Some(answer))))

fn __delivery(answer: Option<__ThenAnswer>, awake: Bool) -> __Dispatch
    ? "The wake gates both retry and completion; a saved answer never asks the module again."
    match awake
        false -> __Dispatch.Parked
        true -> match answer
            Option.None -> __Dispatch.Retry
            Option.Some(value) -> __Dispatch.Complete(value)
"#.to_string();
    out.push_str(&format!(
        "\nfn __serveSlot(run: __Run, ready: List<Int>, id: Int, slot: __Slot) -> __Run\n    ? \"Use the shared wake gate, then retry the request or reveal its saved answer.\"\n{effects}    __dispatch(run, id, slot, __delivery(slot.answer, __askableSlot(slot, ready, id, run.now)))\n\nfn __dispatch(run: __Run, id: Int, slot: __Slot, action: __Dispatch) -> __Run\n    ? \"A completion resumes the process directly; only Retry calls an answer module.\"\n{effects}    match action\n        __Dispatch.Parked -> run\n        __Dispatch.Retry -> __retrySlot(run, id, slot)\n        __Dispatch.Complete(answer) -> __complete(run, id, slot.seq, answer)\n\nfn __complete(run: __Run, id: Int, seq: Int, answer: __ThenAnswer) -> __Run\n    ? \"Reveal precisely the saved operation result to its process without asking the answer module.\"\n{effects}    match answer\n"
    ));
    let mut count = 0;
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        for kind in &protocol.kinds {
            if kind.operation.is_none() {
                continue;
            }
            count += 1;
            let (binder, argument) = match kind.answer_type.as_deref() {
                None | Some("Unit") => ("_", ""),
                Some(_) => ("value", ", value"),
            };
            out.push_str(&format!(
                "        __ThenAnswer.{upper}{}(state, {binder}) -> __settle{upper}(run, id, seq, {}(state{argument}))\n",
                kind.name, kind.answer_fn
            ));
        }
    }
    if count == 0 {
        out.push_str("        __ThenAnswer.NoAnswer -> run\n");
    }
    out
}

pub(super) fn laws() -> &'static str {
    r#"
verify __parkedWith law thenKeepsTheInstanceAndRequest
    given slot: __Slot = [__sampleSlot()]
    given wake: Wait.Wake = [Wait.Wake.NextTurn, Wait.Wake.After(5)]
    given now: Int = [0, 7]
    given answer: Option<__ThenAnswer> = [Option.None]
    (__parkedWith(slot, wake, now, answer).seq, __parkedWith(slot, wake, now, answer).pending) => (slot.seq, slot.pending)

verify __delivery law aSavedAnswerIsRevealedOnlyOnWake
    given answer: Option<__ThenAnswer> = [Option.None]
    given awake: Bool = [false, true]
    __delivery(answer, awake) => match awake
        false -> __Dispatch.Parked
        true -> match answer
            Option.None -> __Dispatch.Retry
            Option.Some(value) -> __Dispatch.Complete(value)
"#
}
