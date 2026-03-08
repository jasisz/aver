# Notes On Aver 0.4.0 In This Project

This is the blunt version.

## What Worked Well

- Pure domain logic stayed readable in `domain/tasks.av`, `domain/validation.av`, `domain/views.av`, and `domain/rules.av`.
- Explicit effects helped review. It is easy to see where time, persistence, audit, and notifications enter the system.
- After the `verify` / `check` split, `verify` felt much more coherent. It now behaves like semantic examples instead of a structural coverage gate.
- Coverage pressure still exists, but it now lives in `check` warnings, which is a much better fit.
- Decision blocks still felt worthwhile when they captured real tradeoffs instead of narrating obvious code.
- The module layout is clear enough that `aver context` should genuinely help a future reader resume work.

## Where Aver Felt Awkward

- Multi-statement `match` arms are restrictive enough that orchestration code grows helper functions quickly.
- The lack of argv in the entrypoint pushed the interface toward a REPL with a small pipe-delimited grammar. That is real, but it is also a language limitation showing through.
- There is still visible duplication in small label/render helpers such as priority spelling. The language does not force that duplication, but the current module ergonomics do not strongly discourage it either.
- Some medium modules still grow large quickly. `app/cli.av` and `infra/store.av` are honest examples of where explicitness turns into surface area.

## What Improved After Compiler Fixes

- Imported sum types and constructors now work across modules, which materially changed the shape of the project.
- The CLI now parses directly into `App.Commands.Command` and `App.Queries.Query` again.
- Replay moved back into `domain/events.av`, which is the architecture the project wanted from the start.
- `Result<Unit, String>` works normally again, so the pure validators are no longer carrying the `Result<Bool, String>` workaround.
- `cargo run -- verify ... --deps` works, which matters for a project of this size.

Those fixes are not cosmetic. They remove several distortions that previously made the project look worse than the intended style.

## Where Constraints Helped

- The pressure to keep domain code pure prevented command handlers from swallowing business rules.
- Explicit effect sets made review of side effects genuinely easier.
- The lack of generic abstraction pressure kept the rules explicit. That was a good outcome.
- The event-log-plus-projection storage choice stayed simple because the language did not tempt a large framework detour.

## Where Constraints Caused Boilerplate

- CLI parsing and rendering needed a lot of small helper functions and verify blocks.
- The app layer has repetitive wrappers because explicit control flow is favored over compact abstraction.
- `infra/store.av` is still larger than ideal because flat-file codecs, storage paths, and event serialization all live together.
- Audit-message rendering in `app/commands.av` got noticeably better once typed event matching worked, which is useful evidence about where earlier boilerplate was language-induced rather than domain-induced.

## Did `verify` Feel Natural?

Mostly yes now.

The current split is much healthier:

- `verify` checks whether the stated examples are true
- `check` warns when example coverage looks thin

That matches how this code wants to be reviewed. The domain modules benefit the most, but even the CLI examples now read like actual behavior checks instead of obligations to satisfy the verifier.

## Did Decision Blocks Stay Useful?

Yes, when they captured real choices:

- append-only task events vs mutable task state
- one rule pass per command vs hidden fixpoint behavior
- separate audit and notification logs
- derived urgency vs mutating stored priority
- typed replay in `Domain.Events` vs keeping it buried in storage

They would have become noise if applied everywhere.

## Would `aver context` Actually Help Here?

Yes.

This project has enough module boundaries, decisions, and verify coverage that context export should help a human or model re-enter the codebase without rereading every file. The boundaries are not perfect, but they are real enough to be useful.

## Bottom Line

Aver survives this project more convincingly after the compiler and tooling fixes than it did before them.

The language looks good in pure domain logic, explicit rules, effect review, and now also in typed cross-module event flow.

It still gets uncomfortable in medium-sized orchestration, CLI surface area, and places where explicit control flow multiplies helper functions. The language is viable here, but it is not invisible. That is probably the fairest conclusion.
