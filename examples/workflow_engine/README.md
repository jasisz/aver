# Workflow Engine

This is a medium-sized Aver 0.4.0 application core for task and event workflows.

It manages:

- projects
- tasks
- task statuses and priorities
- tags and comments
- deadlines
- workflow rules
- audit trail
- notifications

The storage model is intentionally simple:

- projects are stored as current state
- task mutations are stored as an append-only event log
- audits and notifications are persisted beside the task log

That keeps the infrastructure small while still forcing replay, validation, rule evaluation, and effectful orchestration to stay visible.

## Module Layout

- `domain/types.av`: shared records and enums
- `domain/validation.av`: pure validation and transition checks
- `domain/projects.av`: project creation and archiving
- `domain/tasks.av`: task command logic that emits events
- `domain/views.av`: derived urgency, overdue, stalled, and summary projections
- `domain/rules.av`: explicit workflow rules
- `domain/events.av`: pure event application and replay
- `app/commands.av`: effectful mutation handlers
- `app/queries.av`: replay-based read model assembly
- `app/cli.av`: REPL parser, renderer, and dispatcher
- `infra/store.av`: flat-file storage and event serialization
- `infra/audit.av`: audit persistence
- `infra/notify.av`: notification persistence and delivery
- `infra/clock.av`: time adapter
- `main.av`: entrypoint

## Key Domain Concepts

- `Project`: id, name, archived flag, timestamps
- `Task`: id, projectId, title, status, priority, tags, optional deadline, comments, timestamps
- `TaskEvent`: created, renamed, started, blocked, completed, reopened, priority changed, tags changed, comment added, deadline changed, archived
- `TaskView`: derived state such as overdue, urgent, stalled, and needs-review
- `AuditEntry`: non-replay narrative trail for user actions and rule follow-ups
- `Notification`: persisted warning or reminder emitted by rules

## Running The CLI

From the repo root:

```bash
cargo run -- run examples/workflow_engine/main.av --module-root examples/workflow_engine
```

The CLI is a small pipe-delimited REPL. Example commands:

```text
create_project | alpha | Alpha
create_task | alpha | t1 | Plan release | high | ops,waiting | 2026-03-10T12:00:00Z
add_comment | t1 | alice | Waiting on review
list_tasks | alpha
show_task | t1
run_rules | t1
show_audit | t1
quit
```

Data is stored under `/tmp/aver_workflow_engine`.

## Checking And Verifying

From the repo root:

```bash
cargo run -- check examples/workflow_engine/main.av --module-root examples/workflow_engine --deps
cargo run -- verify examples/workflow_engine/app/cli.av --module-root examples/workflow_engine --deps
```

`check` now reports coverage-style warnings without failing, while `verify` is reserved for actual example mismatches.

To check the CLI module itself:

```bash
cargo run -- verify examples/workflow_engine/app/cli.av --module-root examples/workflow_engine
cargo run -- check  examples/workflow_engine/app/cli.av --module-root examples/workflow_engine
```

`verify` runs only the local example cases from `verify` blocks. `check` is where static warnings live, including verify-coverage warnings such as missing `Result.Err` or `Option.None` examples. Both commands also support `--deps` if you want to include transitive modules under `depends [...]`.

## Inspecting Main Flows

The most useful files to read in order are:

1. `app/commands.av`
2. `domain/tasks.av`
3. `domain/rules.av`
4. `app/queries.av`
5. `domain/events.av`
6. `app/cli.av`
7. `infra/store.av`

That path shows the full vertical slice:

- parse CLI input
- load persisted state
- validate and emit domain events
- run one explicit rule pass
- persist audit and notifications
- replay tasks for queries
- derive views for output
