# Workflow Engine

`workflow_engine` is a showcase of Aver as an auditable application core, not a production workflow framework.

The point of the example is:

- commands decide explicit domain events
- replay stays in the domain, not in storage glue
- rule follow-ups stay visible as plain code
- side effects stay explicit and delayed until after commit
- `verify` examples live next to the logic they explain

## Why This Is A Good Aver Showcase

This example works because Aver pushes the architecture toward a small, reviewable core:

- the domain is just records, enums, and functions
- command handlers do not mutate hidden state
- the app layer turns a command into a small `CommandPlan`
- commit and dispatch are separate steps
- rules emit typed follow-up events and notifications instead of hiding imperative updates in handlers

The result is an event-sourced slice that is readable without a framework.

## Architecture After The Refactor

### Domain

- `domain/tasks.av`: pure command decisions that emit one user event
- `domain/events.av`: typed event replay and projection
- `domain/rules.av`: explicit derived follow-ups as normal functions
- `domain/views.av`: derived read model flags such as overdue, stalled, and needs-review
- `domain/validation.av`: invariants and transition checks

### App

- `app/commands.av`: `decide -> derive -> commit -> dispatch`
- `app/queries.av`: load, replay, derive, render-friendly query data
- `app/cli.av`: plain argv parser and renderer with minimal orchestration

### Infra

- `infra/store.av`: flat-file persistence only
- `infra/audit.av`: append-only audit log
- `infra/notify.av`: persisted notifications plus console delivery
- `infra/clock.av`: time adapter

The important boundary is deliberate:

- replay belongs to the domain
- file formats belong to infra
- orchestration belongs to app

## One Command End To End

Take:

```text
add_comment t1 alice "Ping"
```

The flow is:

1. `app/cli.av` parses argv into `Command.AddComment("t1", "alice", "Ping")`.
2. `app/commands.av` loads the task context and asks `domain/tasks.av` for the primary user event.
3. `app/commands.av` derives follow-up rule output by:
   - appending the new user event to the in-memory task stream
   - replaying that updated stream with `domain/events.av`
   - running one explicit rule pass on that updated task state
4. The resulting `CommandPlan` is committed:
   - task events are appended
   - audits are recorded
5. Notifications are dispatched after commit.

That “derive on replayed post-user-event state” step is the core of the example. It keeps derived events explainable:

- they come from pure rule functions
- they are calculated on a visible state transition
- they are still data before anything is written

## The Shape Of `CommandPlan`

`CommandPlan` is intentionally small. It is not a bag of runtime context.

It only represents two write shapes:

- save project state plus audits
- append task events plus audits and notifications

That keeps handlers thin:

- load the minimum read context
- decide the user event
- derive the follow-up plan
- hand one small plan to the executor

## What To Read First

If you want the shortest useful path through the example:

1. `app/commands.av`
2. `domain/tasks.av`
3. `domain/rules.av`
4. `domain/events.av`
5. `app/queries.av`
6. `app/cli.av`
7. `infra/store.av`

That order shows the intended story:

- business decision
- derived events
- replay
- commit
- dispatch
- query/read model

## Running The CLI

From the repo root:

```bash
cargo run -- run examples/workflow_engine/main.av --module-root examples/workflow_engine -- create_project alpha Alpha
cargo run -- run examples/workflow_engine/main.av --module-root examples/workflow_engine -- create_task alpha t1 "Plan release" high ops,waiting 2026-03-10T12:00:00Z
cargo run -- run examples/workflow_engine/main.av --module-root examples/workflow_engine -- add_comment t1 alice "Need review"
cargo run -- run examples/workflow_engine/main.av --module-root examples/workflow_engine -- show_task t1
cargo run -- run examples/workflow_engine/main.av --module-root examples/workflow_engine -- run_rules
```

Data is stored under `/tmp/aver_workflow_engine`.

## Checking And Verifying

From the repo root:

```bash
cargo run -- check examples/workflow_engine/main.av --module-root examples/workflow_engine --deps
cargo run -- verify examples/workflow_engine/app/commands.av --module-root examples/workflow_engine --deps
```

Useful targeted checks:

```bash
cargo run -- verify examples/workflow_engine/domain/tasks.av --module-root examples/workflow_engine
cargo run -- verify examples/workflow_engine/domain/rules.av --module-root examples/workflow_engine
cargo run -- verify examples/workflow_engine/domain/events.av --module-root examples/workflow_engine
cargo run -- verify examples/workflow_engine/app/commands.av --module-root examples/workflow_engine
```

## What This Example Does Not Try To Be

This is not trying to prove that Aver should ship a workflow framework.

It intentionally avoids:

- hidden rule engines
- implicit fixpoint loops
- generic middleware layers
- clever abstractions that hide where events come from

The value of the example is the opposite: it shows that a constrained, explicit style can still produce a clean application core.
