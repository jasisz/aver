# Payment Ops

`payment_ops` is a medium-size Aver showcase for a dirty backoffice domain:

- webhook ingestion from incompatible providers
- duplicate delivery and out-of-order events
- settlement imports that do not match realtime history
- explicit manual-review cases instead of silent fixups

This project is intentionally less "clean architecture demo" and more "this is the sort of backend mess people really have to live with".

## How It Was Made

`payment_ops` was written from scratch in one shot by Codex, OpenAI's coding agent, inside this repository.

It did not start as a hand-curated Aver demo, a pre-existing payments sample, or a slow manual build-out.
The brief was simpler and harsher:

- build a medium-size project under `projects/`
- pick a domain that looks like real operational mess, not a language-shaped toy
- make it pass `aver check`, `aver verify`, and a real CLI smoke flow

That origin is part of the point of the example.

Aver is supposed to help when an agent writes a lot of code quickly and a human needs the result to stay legible, constrained, and auditable.
`payment_ops` exists as a test of that claim, not just as a polished sample app.

## What It Shows

The point of the example is not that Aver makes payments easy.

The point is that Aver keeps a messy flow legible:

- provider-specific webhook names normalize into one canonical event log
- replay rebuilds payment state from append-only facts
- suspicious transitions open review cases
- settlement imports are compared against replayed state
- manual resolution stays explicit and auditable

## Start Here

If you only have 10 minutes, read in this order:

1. `app/commands.av`
2. `domain/normalize.av`
3. `domain/ledger.av`
4. `domain/reconcile.av`
5. `domain/cases.av`
6. `app/queries.av`
7. `infra/store.av`
8. `app/render.av`

That path shows the intended story:

- ingest dirty external data
- normalize it early
- replay current state
- derive review work explicitly
- persist facts and current case state
- query by rebuilding, not by guessing

## Canonical Flow

The best first tour is:

1. ingest Stripe webhooks
2. import Stripe settlements
3. reconcile the provider
4. inspect one payment
5. inspect open cases

From the repo root:

```bash
aver run projects/payment_ops/main.av --module-root projects/payment_ops -- ingest_webhooks stripe projects/payment_ops/fixtures/stripe_webhooks_day1.txt

aver run projects/payment_ops/main.av --module-root projects/payment_ops -- import_settlement stripe projects/payment_ops/fixtures/stripe_settlement_day1.txt

aver run projects/payment_ops/main.av --module-root projects/payment_ops -- reconcile stripe

aver run projects/payment_ops/main.av --module-root projects/payment_ops -- show_payment pay-2

aver run projects/payment_ops/main.av --module-root projects/payment_ops -- list_cases open
```

Data is stored under `/tmp/aver_payment_ops`.

## Expected Dirt

The bundled Stripe fixtures deliberately contain:

- a duplicate webhook for `pay-1`
- a refund before capture for `pay-2`
- a capture without authorization for `pay-4`
- a settlement capture for `pay-3` even though realtime only saw authorization
- a settlement row for `pay-9` with no realtime events at all
- a duplicate settlement row for `pay-9`

That gives the project several different classes of case:

- replay anomaly from event order
- replay anomaly from missing authorization
- settlement mismatch against replay
- settlement without realtime evidence
- realtime capture without settlement evidence

## Architecture Summary

### Domain

- `domain/types.av`: shared records and enums
- `domain/normalize.av`: provider-specific labels -> canonical events and settlement rows
- `domain/ledger.av`: append-only replay and dedupe helpers
- `domain/reconcile.av`: compare realtime replay to settlement imports
- `domain/cases.av`: open, dedupe, and resolve manual-review work
- `domain/views.av`: derived status and provider summary helpers

### App

- `app/commands.av`: ingest, import, reconcile, resolve
- `app/queries.av`: show payment, list cases, provider summary, audit
- `app/parse.av`: argv parsing only
- `app/render.av`: CLI formatting only
- `app/cli.av`: thin adapter shell

### Infra

- `infra/imports.av`: parse external import files
- `infra/store.av`: canonical event, settlement, and case persistence
- `infra/audit.av`: append-only audit trail
- `infra/codec.av`: explicit flat-file escaping and line helpers

The boundary is deliberate:

- domain interprets
- app orchestrates
- infra stores and reads

## Record / Replay

This project is a good fit for Aver's record/replay workflow because the CLI is effectful but deterministic once the file inputs are fixed.

After running the canonical flow above, you can record a read-only query like this:

```bash
aver run projects/payment_ops/main.av --module-root projects/payment_ops --record recordings/payment_ops -- show_payment pay-2

aver replay recordings/payment_ops --test --diff
```

`verify` explains the pure core. Replay guards the effectful shell.

## Checking And Verifying

From the repo root:

```bash
aver check projects/payment_ops/main.av --module-root projects/payment_ops --deps

aver verify projects/payment_ops/domain/normalize.av --module-root projects/payment_ops
aver verify projects/payment_ops/domain/ledger.av --module-root projects/payment_ops
aver verify projects/payment_ops/domain/reconcile.av --module-root projects/payment_ops
aver verify projects/payment_ops/domain/cases.av --module-root projects/payment_ops
```

## What This Example Is Not

This project is not pretending to be a production payment gateway.

It intentionally avoids:

- hidden SQL-heavy reconciliation logic
- background schedulers
- provider SDK wrappers
- silent auto-repair of broken histories
- mutable caches pretending to be the source of truth

The point is narrower:

Can Aver keep a genuinely annoying backoffice flow readable?

`payment_ops` is meant to answer "yes" without choosing a domain that only works because it was designed for the language.
