# Parallel Work on wasm-gc

`aver run app.av --wasm-gc` and `aver compile app.av --target wasm-gc --pack wasmtime`
execute Work jobs on host threads. Each active job owns a Wasmtime Store and
instance; completed instances return to a bounded pool. All instances share
the compiled Module. A deployment pack uses its precompiled code for workers
too, with no compiler or JIT required on the destination.

The Aver surface stays `Kind.begin`, `Kind.take`, `Work.cancel`, and `Wait.poll`.
`begin` returns immediately and refuses admission at `[work] max-jobs`;
`take` may return `Ok(None)` until completion. A combined wait wakes for sockets,
completed or cancelled jobs, or its timeout. Cancellation interrupts a running
Wasm body through the host's epoch checks, including a recursive body that
never returns. A cancelled body keeps occupying its execution slot until it
has actually stopped. Program exit cancels remaining jobs with bounded cleanup.

The task and result cross between instances as owned, contract-directed values.
Their GC references never cross a Store or worker boundary. Live execution
preserves the full mathematical `Int`, including integers outside i64, and
represented records, sums and collections. Copying the boundary has a cost;
jobs should contain enough computation to justify scheduling and transport.

The native host records and replays the same operations as the VM. Replay
preserves recorded handle tokens and readiness, recomputes pure jobs and checks
completed results. The existing recording format still refuses integers outside
i64 and non-finite floats; that limitation does not apply to live job transport.

## JavaScript host

Raw `--target wasm-gc` artifacts expose the versioned `aver:work/v1` scheduling
ABI. The live adapter in `tools/wasm-work/host.mjs` implements it using browser
Web Workers or Node worker threads. It needs a WasmGC and tail-call capable
engine. It does not require shared linear memory or SharedArrayBuffer.

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

The adapter includes Console.print, Time.unixMs and cooperative stopping.
Supply other synchronous imports through `options.imports`. A combined socket
wait additionally needs `options.pollSockets(entries, timeoutMs, signal)`, which
returns a promise of ready keys. Entries are `[BigInt key, decoded Wait.Item]`
pairs; the AbortSignal cancels an outstanding poll when another wake wins.
The callback must support a zero-timeout readiness probe. Its timeout uses
milliseconds and its result keys are BigInts.

`host.stop()` requests cooperative coordinator stopping. `host.close()` stops
workers and releases host resources. `runCoordinator()` closes its host on
completion or failure; applications driving exports directly close it themselves.

JavaScript must return to its event loop to receive worker messages. The
generated coordinator therefore exposes start, observe, wait-set, timeout,
step, stopped and finish functions. Its step is the same post-wait turn the
blocking native driver uses. `runCoordinator()` awaits readiness outside Wasm,
then calls that step. Arbitrary synchronous calls to `main` that block inside
`Wait.poll` cannot use this adapter; it reports that explicitly. A host for a
hand-written coordinator can call application exports and `await host.wait(...)`
instead. Other synchronous effect implementations remain the embedding's job;
this adapter does not transform arbitrary blocking effects into async calls.

The JS adapter performs live execution. Recording/replay is supplied by the
native Wasmtime host. Cloudflare Workers do not supply the browser Worker pool
used here; deploying a Work artifact requires a host implementing this ABI.

## ABI v1

The module contains exactly one JSON custom section named `aver:work/v1`.
It declares `version`, ordered `kinds`, and `types`. Each kind names its task,
payload, boxed task, take result and worker export. Type descriptors refer to
the existing `__cap_abi_*` construction and inspection exports documented in
[the capability ABI](wasm-gc-custom-capabilities.md#host-bridge-exports).
The descriptor is part of the compiled artifact; pack byte checks cover it.

| Import in `aver:work/v1` | Wasm signature | Responsibility |
| --- | --- | --- |
| `submit` | `(i32 kind, anyref boxedTask, i32 caller) -> anyref` | Start work and return `Result<Work.Job, String>` |
| `take` | `(i32 kind, anyref job, i32 caller) -> anyref` | Return `Result<Option<R>, String>` with kind and lifecycle checks |
| `task` | `(i32 kind) -> anyref` | Supply `Option.Some(T)` once inside the worker |
| `complete` | `(i32 kind, anyref boxedResult) -> ()` | Accept `Option.Some(R)` once inside the worker |

Worker instances call only `__work_v1_run_N()`, never `main` or `_start`.
Other effect imports in a worker reject invocation. The main instance rejects
worker-only `task` and `complete` calls. `aver.work_cancel` and `aver.wait_poll`
retain their existing signatures and connect to the same host job table.

`__work_v1_started(i64 id, i32 kind)` and `__work_v1_refused(String)` construct
the two begin results. `__work_v1_job_id(anyref job)` extracts a host token.
These are trusted host helpers, not constructors available in Aver source.
`__workHostStart`, `__workHostObserve`, `__workHostWaitSet`, `__workHostTimeout`,
`__workHostStep`, `__workHostStopped`, and `__workHostFinish` are emitted for
generated coordinators. The run state is an opaque reference kept in its own
instance; only job data moves to workers.

JS transport uses BigInt for every Int, ordinary numbers for Float, strings,
null for Unit, arrays for tuples/lists/vectors, arrays of pairs for maps,
objects for records, `{ variant, fields }` for sums, `{ ok }`/`{ err }` for
Result, and null/`{ some }` for Option. Bytes use Uint8Array. Work-shape checking
already excludes functions and capability resources from tasks and results.

WASI 0.2 remains on the inline lowering and does not acquire these non-WASI
imports. Its `begin` computes immediately and `max-jobs` still warns that it
has no effect. Certification of the new Work import namespace requires a
separate verifier ABI update; the existing verifier rejects it. This change
does not widen the certificate wall's accepted imports.
