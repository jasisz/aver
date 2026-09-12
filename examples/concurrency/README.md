# The generated coordinator

The worked example for a program whose loop is generated lives in `tests/fixtures/run_all_slice/`, not here, because it needs an `aver.toml`: the `[run]` table in that manifest is what asks for the loop, and the `answer`, `task` and `landed` keys are what the loop is generated from. An example directory has no manifest of its own, so the program would lower to something else if it sat here.

Read it in this order.

- `aver.toml` — the manifest. `answer = "Sockets"` / `"Ledger"` / `"Clocked"` says which module answers which capability, the `work` binding on `Validation` carries the two ends of the job seam (`task`, `landed`), and `[run]` names the three policies and the view record.
- `main.av` — everything the program writes: five processes (`accepting`, `dialling`, `ticker`, `peer`, `walk`), the `Pending` sum and the `View` record the loop fills, and the three policies `order`, `admit` and `stop`. There is no `main` and no coordinator here; both are generated.
- `sockets.av`, `ledger.av`, `clocked.av` — the answer modules. One function per operation, one state each, threaded through a `Tuple<S, Cap.__<Op>Reply>`. `Sockets` owns real sockets: a `Tcp.Listener` bound on the port the program is run with, and the `Tcp.Connection` behind each peer key. `Sockets.write` is the worked example of a `Later` that keeps state: `Tcp.writeNow` answers how much of the offer the socket took, the offset that leaves behind lives in the module's own state, and the request parks on `Sending` until the socket takes the rest. `Clocked.tick` is the worked example of `After`: it arms a fifty-millisecond deadline and answers the tick on the ask after it.
- `wire.av`, `pool.av`, `blocks.av`, `chain.av`, `clock.av`, `validation.av` — the capability contracts the processes wait on.

Run it, and see the loop that was generated for it. The run takes the port to listen on as its one argument, and it needs a peer on the other end of that port: it fetches three block bodies from whoever connects, and waits for them. `tests/support/loopback_peer.rs` is the peer the test suite plays; anything that accepts the connection, reads seven bytes and sends them back three times over will do.

```bash
cd tests/fixtures/run_all_slice
aver run main.av --module-root . -- 54321
aver verify main.av --module-root .
AVER_YIELD_DUMP=1 aver check main.av --module-root .
```

The same program compiles to a native binary, loop and job kind included:

```bash
aver compile main.av --module-root . --target rust -o /tmp/follow-slice
cd /tmp/follow-slice && cargo run -- 54321
```

The two runs do the same work in a different interleaving. The answer modules park requests on wall-clock deadlines (`Wait.Wake.After(2)`, `After(5)`, `After(50)`), so which turn a finished validation job lands in depends on how long that job took, and the compiled function is faster than the smallest deadline in the program while the VM's child interpreter is not. What does not change between them is how often a parked request is asked: the wake gates the ask on both, which is why the run ends with the same two lines either way — the ticker was asked nine times for four ticks, and three payloads took six write asks and six read asks, because this wire offers a socket one chunk at a time and a read that finds nothing parks until the wait says the bytes are there.

The "Coordinator" section of [docs/language.md](../../docs/language.md) walks the same example line by line.
