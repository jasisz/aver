# The generated coordinator

The worked example for a program whose loop is generated lives in `tests/fixtures/run_all_slice/`, not here, because it needs an `aver.toml`: the `[run]` table in that manifest is what asks for the loop, and the `answer`, `task` and `landed` keys are what the loop is generated from. An example directory has no manifest of its own, so the program would lower to something else if it sat here.

Read it in this order.

- `aver.toml` — the manifest. `answer = "Sockets"` / `"Ledger"` / `"Clocked"` says which module answers which capability, the `work` binding on `Validation` carries the two ends of the job seam (`task`, `landed`), and `[run]` names the three policies and the view record.
- `main.av` — everything the program writes: five processes (`accepting`, `dialling`, `ticker`, `peer`, `walk`), the `Pending` sum and the `View` record the loop fills, and the three policies `order`, `admit` and `stop`. There is no `main` and no coordinator here; both are generated.
- `sockets.av`, `ledger.av`, `clocked.av` — the answer modules. One function per operation, one state each, threaded through a `Tuple<S, Cap.__<Op>Reply>`.
- `wire.av`, `pool.av`, `blocks.av`, `chain.av`, `clock.av`, `validation.av` — the capability contracts the processes wait on.

Run it, and see the loop that was generated for it:

```bash
cd tests/fixtures/run_all_slice
aver run main.av --module-root .
aver verify main.av --module-root .
AVER_YIELD_DUMP=1 aver check main.av --module-root .
```

The "Coordinator" section of [docs/language.md](../../docs/language.md) walks the same example line by line.
