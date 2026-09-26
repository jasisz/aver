# Parallel Work on wasm-gc

`aver run app.av --wasm-gc` and `aver compile app.av --target wasm-gc --pack wasmtime`
run Work jobs on host threads. Each active job owns a Wasmtime Store and
instance, and finished instances go back to a bounded pool. All instances share
the compiled Module. A deployment pack also uses its precompiled code for
workers, so the destination needs no compiler or JIT.

The Aver surface stays `Kind.begin`, `Kind.take`, `Work.cancel`, and `Wait.poll`.
`begin` returns immediately and never refuses at `[work] max-jobs`: a job begun
while that many bodies run waits in the host's queue and starts, in the order it
was begun, when one of them stops. `take` may return `Ok(None)` until the job
completes, and a queued job is not ready. A combined wait wakes for sockets, for
completed or cancelled jobs, at its timeout, or within about 100 ms of a stop
request once the program watches for one; it is the same wait loop the VM and
generated Rust run. Cancellation takes a queued job out of the queue so it never
starts, and interrupts a running Wasm body through the host's epoch checks,
which includes a recursive body that never returns. A cancelled body keeps its
execution slot until it has actually stopped. When the program exits, the
remaining jobs are cancelled with bounded cleanup.

The task and result move between instances as owned values, shaped by the
contract. Their GC references never cross a Store or worker boundary. Live
execution keeps the full mathematical `Int`, including integers outside i64,
as well as represented records, sums and collections. Copying at the boundary
costs something, so a job should do enough computation to pay for scheduling
and transport.

The native host records and replays the same operations as the VM. Replay keeps
the recorded handle tokens and readiness, recomputes pure jobs and checks their
completed results. Replay starts every recorded job whatever limit the host runs
with, and waits up to 30 seconds for a recomputation before it fails; the VM
waits 5 seconds and lets the recorded answer stand. The existing recording format still refuses integers outside
i64 and non-finite floats. That limit does not apply to live job transport.

## JavaScript host

Raw `--target wasm-gc` artifacts expose the versioned `aver:work/v1` scheduling
ABI. The live adapter in `tools/wasm-work/host.mjs` implements it with browser
Web Workers or Node worker threads. It needs an engine with WasmGC and tail
calls. It does not need shared linear memory or SharedArrayBuffer.

For a generated coordinator:

```sh
aver compile tests/fixtures/run_guide_example/main.av \
  --module-root tests/fixtures/run_guide_example --target wasm-gc -o out
node tools/wasm-work/run.mjs out/main.wasm 2
# scored 60
```

In a browser application, serve the adapter's three modules together:

```js
import { createWorkHost } from "./wasm-work/host.mjs";

const module = await WebAssembly.compileStreaming(fetch("./app.wasm"));
const host = await createWorkHost(module, {
    maxJobs: 4,
    onPrint: line => console.log(line),
});
await host.runCoordinator();
```

`maxJobs` is the adapter's own limit, and without it the lower of the host's
`hardwareConcurrency` and 8. The adapter does not read `[work] max-jobs` from
the manifest, because the compiled module carries no limit. A job begun at the
limit is queued, never refused, and starts on the next free Worker in the order
it was begun. Cancelling a running job terminates its Worker at once, so its
place is free at once and a replacement Worker starts; cancelling a queued job
takes it out of the queue.

The adapter provides Console.print, Time.unixMs and cooperative stopping.
Pass other synchronous imports through `options.imports`. A combined socket
wait also needs `options.pollSockets(entries, timeoutMs, signal)`, which
returns a promise of ready keys. Entries are `[key, decoded Wait.Item]` pairs.
The key has whatever type the program keys its wait by: a whole number arrives
as a BigInt, and a key of the program's own type arrives as its decoded value.
A socket the adapter no longer knows (closed, or dropped after an error) should
be answered as ready, the way the native hosts answer it: the operation the
program runs on it next reports the real error, and one stale key must not fail
the whole wait.
The callback must answer with keys taken from the `entries` it was given, not
keys it built itself, because the wait orders and dedups its answer by each
key's position in that list. A key from anywhere else is refused by name. The
AbortSignal cancels an outstanding poll when another wake wins. The callback
must support a zero-timeout readiness probe. Its timeout is in milliseconds.

`host.stop()` asks the coordinator to stop cooperatively. `host.close()` stops
the workers and releases host resources. `runCoordinator()` closes its host
when it completes or fails; applications that drive exports directly close it
themselves.

JavaScript has to return to its event loop to receive worker messages, so the
generated coordinator exposes start, observe, wait-set, timeout, step, stopped
and finish functions. Its step is the same post-wait turn the blocking native
driver uses. `runCoordinator()` waits for readiness outside Wasm and then calls
that step. A synchronous `main` that blocks inside `Wait.poll` cannot receive
worker messages. A host for a hand-written coordinator can instead call
application exports and `await host.wait(...)`, or use JSPI to suspend that
stack. An explicitly supplied `imports.aver.wait_poll` is kept, so a JSPI
embedding can delegate to the same combined wait:

```js
let host;
const wait_poll = new WebAssembly.Suspending(async (items, timeout) => {
    try {
        return host.instance.exports.__rt_result_wait_keys_ok(await host.wait(items, timeout));
    } catch (error) {
        return host.instance.exports.__rt_result_wait_keys_err(host.codec.stringIn(String(error)));
    }
});
host = await createWorkHost(module, { imports: { aver: { wait_poll } }, pollSockets });
try {
    await WebAssembly.promising(host.instance.exports.main)();
} finally {
    await host.close();
}
```

This needs JSPI on top of WasmGC and tail calls (Node 26, or Node 24/25 with
`--experimental-wasm-jspi`). Other synchronous effect implementations are
still the embedding's job. The adapter does not turn arbitrary blocking effects
into async calls.

The JS adapter performs live execution. Recording and replay come from the
native Wasmtime host. Cloudflare Workers do not provide the browser Worker pool
this adapter uses, so deploying a Work artifact there needs a host that
implements this ABI.

## ABI v1

The module contains exactly one JSON custom section named `aver:work/v1`.
It declares `version`, ordered `kinds`, `types`, and, for a module that waits,
`wait`. Each kind names its task, payload, boxed task, take result and worker
export. Type descriptors refer to the existing `__cap_abi_*` construction and
inspection exports documented in
[the capability ABI](wasm-gc-custom-capabilities.md#host-bridge-exports).
The descriptor is part of the compiled artifact, and pack byte checks cover it.

A module carries the descriptor if it has either door. `kinds` is empty for a
program that waits but runs no job of its own, and a host must accept that.
Such a program still hands its wait set across this ABI; before the wait half
was emitted for it, a host had nothing to decode that set with. `wait` names
the four types the wait is carried through: `set` is the `Map<K, Wait.Item>`
the guest hands over, `key` is `K`, `ready` is `List<K>` and `answer` is
`Result<List<K>, String>`. It names them because the key is whatever the
program keys its waits by, which is not always a whole number. A host reads
those names from the descriptor and moves the values through the `__cap_abi_*`
helpers named after them. It never reads a key. The ready keys go back in the
order the program's own map keeps its keys, which is the order `Map.keys`
shows and the order a `decode` of the wait set already returns.

`__rt_result_wait_keys_ok(List<K>)` and `__rt_result_wait_keys_err(String)`
build the wait's answer. `__rt_wait_keys_cons(anyref, List<K>)` and
`__rt_wait_keys_nil()` build the list a JSPI embedding passes to the first of
them. They replace the `__rt_result_list_int_string_*` pair a wait used to
answer through, which `Tcp.poll` alone now uses. `__rt_wait_set_order(map)`
returns the occupied buckets of a wait set in its own key order, and
`__rt_wait_set_key_at(map, bucket)` reads one key out. The native Wasmtime host
reads a wait set through those two. A JavaScript host reads the same set
through the `__cap_abi_*` helpers the descriptor names.

| Import in `aver:work/v1` | Wasm signature | Responsibility |
| --- | --- | --- |
| `submit` | `(i32 kind, anyref boxedTask, i32 caller) -> anyref` | Start work and return `Result<Work.Job, String>` |
| `take` | `(i32 kind, anyref job, i32 caller) -> anyref` | Return `Result<Option<R>, String>` with kind and lifecycle checks |
| `task` | `(i32 kind) -> anyref` | Supply `Option.Some(T)` once inside the worker |
| `complete` | `(i32 kind, anyref boxedResult) -> ()` | Accept `Option.Some(R)` once inside the worker |

Worker instances call only `__work_v1_run_N()`, never `main` or `_start`.
Other effect imports reject any call made in a worker. The main instance refuses the
worker-only `task` and `complete` calls. `aver.work_cancel` and `aver.wait_poll`
keep their existing signatures and connect to the same host job table.

`__work_v1_started(i64 id, i32 kind)` and `__work_v1_refused(String)` build
the two begin results. `__work_v1_job_id(anyref job)` extracts a host token.
These are trusted host helpers; Aver source cannot call them as constructors.
`__workHostStart`, `__workHostObserve`, `__workHostWaitSet`, `__workHostTimeout`,
`__workHostStep`, `__workHostStopped`, and `__workHostFinish` are emitted for
generated coordinators. The run state is an opaque reference kept in its own
instance. Only job data moves to workers.

A program that calls `Run.fail` also imports `aver.run_fail(anyref message, i32 caller)`
and `aver.run_failure(anyref reason, i32 caller) -> anyref`. The module keeps
the reason itself, so a live host answers `run_fail` with nothing and hands the
`run_failure` argument back unchanged. The recorder writes both, and a replay
answers `run_failure` with the recorded reason. `host.mjs` supplies both. When a
turn fails the run, `runCoordinator()` answers `{ err: reason }`.

A program that reads `Run.lastTurn` also imports `aver.run_wait_starts(i32 caller) -> i64`,
`aver.run_wait_ends(i32 caller) -> i64` and
`aver.run_last_turn(i64 waitedMs, i64 workedMs, i32 caller) -> (i64, i64)`. The
generated loop calls the first right before it waits and the second right after
the wait returns: each answers the host's monotonic clock in nanoseconds. With
`runCoordinator()` the first is called from `__workHostObserve` and the second
from `__workHostStep`, so the time the host spends awaiting worker messages and
sockets is what `waitedMs` reports. The module keeps the numbers itself, so a
live host answers `run_last_turn` with its two arguments. The recorder writes all
three, and a replay answers `run_last_turn` with the recorded numbers. `host.mjs`
supplies all three with `performance.now()`. A program that does not read
`Run.lastTurn` imports none of them.

JS transport uses BigInt for every Int, ordinary numbers for Float, strings,
null for Unit, arrays for tuples/lists/vectors, arrays of pairs for maps,
objects for records, `{ variant, fields }` for sums, `{ ok }`/`{ err }` for
Result, and null/`{ some }` for Option. Bytes use Uint8Array. Work-shape
checking already keeps functions and capability resources out of tasks and
results.

WASI 0.2 stays on the inline lowering and does not get these non-WASI imports.
Its `begin` computes immediately, and `max-jobs` still warns that it has no
effect. There `take` never answers `Ok(None)`, a job whose body never ends
blocks the turn, and a job whose body fails stops the component. Certifying the new Work import namespace needs a separate verifier ABI
update; the current verifier rejects it. This change does not widen the set of
imports the certificate wall accepts.
