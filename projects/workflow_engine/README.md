# Workflow Engine

`workflow_engine` is a showcase of Aver as an auditable application core, not a workflow framework.

The example is intentionally biased toward the style Aver rewards best:

- explicit, step-by-step command flow
- replayable state instead of hidden mutation
- rules as normal functions
- side effects decided as data before they are dispatched
- `verify` used to explain orchestration, not only domain edge cases

## Start Here

If you only have 5 to 10 minutes, read in this order:

1. `app/commands.av`
2. `domain/tasks.av`
3. `domain/events.av`
4. `domain/rules.av`
5. `app/queries.av`
6. `app/cli.av`
7. `app/parse.av` and `app/render.av`
8. `infra/store.av`

That path shows the intended story:

- app decides a write plan
- domain emits the primary event
- replay rebuilds post-state
- rules derive follow-up events and notifications
- commit persists the plan
- dispatch happens after commit
- queries rebuild read models the same way
- CLI stays a thin shell adapter

## The Canonical Flow

The best first tour is:

```text
add_comment t1 alice "Ping"
```

Read these functions in order:

1. `app/commands.av`
   - `planAddComment`
   - `planTaskCommand`
   - `deriveRuleEffectsAfterPrimaryEvent`
   - `commit`
   - `dispatch`
2. `domain/tasks.av`
   - `addComment`
3. `domain/events.av`
   - `replayTask`
4. `domain/rules.av`
   - `ruleWaitingCommentNeedsReview`
5. `app/queries.av`
   - `taskDetailsFrom`

That one flow shows almost everything important about the example:

- a user command becomes one primary event
- the app layer replays post-command state before running rules
- rules can emit derived events as plain data
- the final `CommandPlan` is committed before notifications are dispatched
- the query side rebuilds the same story from stored events and side outputs

## What `CommandPlan` Is For

`CommandPlan` is intentionally small.

It only models two write shapes:

- save current project state plus audits
- append task events plus audits and notifications

It is not a generic runtime bag. The point is to keep command handlers readable:

- load the minimum persisted context
- decide the primary event
- derive follow-up rule output
- commit one small plan
- dispatch after commit

## Primary Vs Derived Events

In task mutations, the app layer keeps one distinction explicit:

- primary event: the direct result of user intent, produced by `domain/tasks.av`
- derived events: follow-up events produced by one explicit rule pass over replayed post-state

In `app/commands.av`, that split is visible in three places:

- `planTaskCommand`
- `deriveRuleEffectsAfterPrimaryEvent`
- `primaryEventPlan`

That is the core Aver idiom this example is trying to teach: decide first, derive second, commit third, dispatch last.

## Where Aver Shows Up Clearly

The example is most honest and strongest in these places:

- `domain/tasks.av`: rules and validations are plain functions that emit typed events
- `domain/events.av`: replay is simple typed code, not storage magic
- `domain/rules.av`: derived behavior is visible as normal branching
- `app/commands.av`: orchestration is explicit and reviewable
- `app/queries.av`: read models come from replay plus derived views
- `app/cli.av`: the shell adapter is thin enough that it does not hide the architecture
- `infra/store.av`: infra stores and parses bytes, but does not interpret workflow meaning

## Files You Can Read Later

These files matter, but they are not the best entrypoint:

- `app/parse.av`: argv parsing only
- `app/render.av`: text formatting only
- `infra/audit.av`: append-only audit storage
- `infra/notify.av`: notification persistence and console delivery
- `infra/clock.av`: time adapter

They support the showcase, but they are not where the architectural point lives.

## Verify As Design Aid

The most useful `verify` blocks for understanding the orchestration are:

- `app/commands.av`
  - `planTaskCommand`
  - `deriveRuleEffectsAfterPrimaryEvent`
  - `primaryEventPlan`
  - `deriveRulesForAllStep`
- `app/queries.av`
  - `taskDetailsFrom`
- `domain/rules.av`
  - `ruleWaitingCommentNeedsReview`
- `domain/events.av`
  - `replayTask`

These are not just correctness checks. They document how to read the flow.

## Architecture Summary

### Domain

- `domain/tasks.av`: user intent to primary event
- `domain/events.av`: replay and event application
- `domain/rules.av`: derived follow-ups from replayed state
- `domain/views.av`: derived read-model flags
- `domain/validation.av`: invariants and transition checks

### App

- `app/commands.av`: `decide -> derive -> commit -> dispatch`
- `app/queries.av`: `load -> replay -> derive view -> join side outputs`
- `app/cli.av`: thin adapter only

### Infra

- `infra/store.av`: flat-file persistence and codecs only
- `infra/audit.av`: append-only audit log
- `infra/notify.av`: notification storage and delivery
- `infra/clock.av`: time boundary

The boundary is deliberate:

- domain interprets
- app plans and executes flow
- infra stores and reads

## Running The CLI

From the repo root:

```bash
cargo run -- run projects/workflow_engine/main.av --module-root projects/workflow_engine -- create_project alpha Alpha
cargo run -- run projects/workflow_engine/main.av --module-root projects/workflow_engine -- create_task alpha t1 "Plan release" high ops,waiting 2026-03-10T12:00:00Z
cargo run -- run projects/workflow_engine/main.av --module-root projects/workflow_engine -- add_comment t1 alice "Need review"
cargo run -- run projects/workflow_engine/main.av --module-root projects/workflow_engine -- show_task t1
cargo run -- run projects/workflow_engine/main.av --module-root projects/workflow_engine -- run_rules
```

Data is stored under `/tmp/aver_workflow_engine`.

## Checking And Verifying

From the repo root:

```bash
cargo run -- check projects/workflow_engine/main.av --module-root projects/workflow_engine --deps
cargo run -- verify projects/workflow_engine/app/commands.av --module-root projects/workflow_engine --deps
```

Useful targeted checks:

```bash
cargo run -- verify projects/workflow_engine/domain/tasks.av --module-root projects/workflow_engine
cargo run -- verify projects/workflow_engine/domain/rules.av --module-root projects/workflow_engine
cargo run -- verify projects/workflow_engine/domain/events.av --module-root projects/workflow_engine
cargo run -- verify projects/workflow_engine/app/commands.av --module-root projects/workflow_engine
cargo run -- verify projects/workflow_engine/app/queries.av --module-root projects/workflow_engine
```

## What This Example Does Not Try To Be

This example does not claim that Aver is ideal for every kind of application.

It intentionally avoids:

- hidden rule engines
- implicit fixpoint loops
- framework-heavy middleware
- clever helpers that hide event origins
- infra layers that reinterpret workflow semantics

The tradeoff is real: some boilerplate remains because Aver favors explicit stages. The example leans into that instead of hiding it.
