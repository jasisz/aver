import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { createWorkHost } from "./host.mjs";

const [parallelFile, guideFile, recordFile, unitFile, unitResultFile, socketOnlyFile] = process.argv.slice(2);
const host = await createWorkHost(await WebAssembly.compile(await readFile(parallelFile)), { maxJobs: 2 });
try {
    const { exports: e } = host.instance, c = host.codec;
    const start = task => c.decode("Result<Work.Job, String>", e.start(task ? 1 : 0));
    const take = job => c.decode("Result<Option<Int>, String>", e.take(job));
    const stuck = start(true).ok;
    const quick = start(false).ok;
    // At the limit a begin is queued, never refused: it answers a handle
    // that reads as pending and starts once a worker is free.
    const queued = start(false).ok;
    assert.ok(queued, "a begin at the limit should be queued, not refused");
    assert.deepEqual(take(queued), { ok: null });
    assert.deepEqual(take(stuck), { ok: null });
    const ready = await host.wait(e.waiting(stuck, quick), c.encode("Int", 10000n));
    assert.deepEqual(c.decode("List<Int>", ready), [2n]);
    assert.deepEqual(take(quick), { ok: { some: 340282366920938463463374607431768211457n } });
    assert.deepEqual(take(quick), { err: "work: job already taken" });
    e.cancel(stuck);
    assert.deepEqual(take(stuck), { err: "work: job cancelled" });
    e.cancel(stuck);
    assert.deepEqual(take(stuck), { err: "work: job cancelled" });
    const next = start(false).ok;
    assert.ok(next, "the healthy worker should accept another task");
    const empty = c.encode("Map<Int, Wait.Item>", []), zero = c.encode("Int", 0n);
    const deadline = Date.now() + 10000;
    let nextResult;
    do {
        await host.wait(empty, zero);
        nextResult = take(next);
        assert.ok(Date.now() < deadline, "zero-timeout turns must deliver worker events");
    } while (nextResult.ok === null);
    assert.deepEqual(nextResult, { ok: { some: 340282366920938463463374607431768211457n } });
    let queuedResult;
    do {
        await host.wait(empty, zero);
        queuedResult = take(queued);
        assert.ok(Date.now() < deadline, "the queued job should start once a worker is free");
    } while (queuedResult.ok === null);
    assert.deepEqual(queuedResult, { ok: { some: 340282366920938463463374607431768211457n } });
    // A queued job that is cancelled never starts.
    const busy = [start(true).ok, start(true).ok];
    const dropped = start(false).ok;
    assert.deepEqual(take(dropped), { ok: null });
    e.cancel(dropped);
    assert.deepEqual(take(dropped), { err: "work: job cancelled" });
    for (const job of busy) e.cancel(job);
} finally { await host.close(); }

const lines = [];
const guide = await createWorkHost(await WebAssembly.compile(await readFile(guideFile)), { maxJobs: 2, onPrint: line => lines.push(line) });
assert.deepEqual(await guide.runCoordinator(), { ok: null });
assert.deepEqual(lines, ["scored 60"]);

// Both sides reconstruct their own records; no GC object can be cloned into
// another instance. Also exercise the zero-argument Unit factory convention.
for (const [file, task, expected] of [
    [recordFile, { text: "żółw 🐢", weight: 3n }, { score: 18n, label: "żółw 🐢" }],
    [unitFile, null, 5n],
    [unitResultFile, null, null],
]) {
    const host = await createWorkHost(await WebAssembly.compile(await readFile(file)), { maxJobs: 1 });
    try {
        const c = host.codec, kind = host.manifest.kinds[0], abi = host.imports["aver:work/v1"];
        const started = c.decode("Result<Work.Job, String>", abi.submit(0, c.encode(kind.boxedTask, { some: task })));
        assert.ok(Object.hasOwn(started, "ok"));
        let result;
        const deadline = Date.now() + 10000;
        do {
            result = c.decode(kind.answer, abi.take(0, started.ok));
            if (result.ok !== null) break;
            assert.ok(Date.now() < deadline, "worker transport timed out");
            await new Promise(resolve => setTimeout(resolve, 5));
        } while (true);
        assert.deepEqual(result, { ok: { some: expected } });
    } finally { await host.close(); }
}
console.log("worker ABI passed");

// A program that waits without running a job of its own. It declares no job
// kind, so this host starts no worker, and its wait set is keyed by a type of
// the program rather than by whole numbers: the host moves the keys through
// the module's own ABI helpers and never reads one.
{
    let adapter = () => [];
    const host = await createWorkHost(await WebAssembly.compile(await readFile(socketOnlyFile)), {
        maxJobs: 1,
        pollSockets: (entries, timeoutMs, signal) => adapter(entries, timeoutMs, signal),
    });
    try {
        assert.deepEqual(host.manifest.kinds, [], "a socket-only program declares no job kind");
        assert.equal(host.manifest.wait.set, "Map<Watch, Wait.Item>");
        assert.equal(host.manifest.wait.key, "Watch");
        assert.equal(host.manifest.wait.ready, "List<Watch>");
        const keys = [{ variant: "Listener", fields: [] }, { variant: "Peer", fields: [7n] }];
        const encoded = host.codec.encode(host.manifest.wait.ready, keys);
        assert.deepEqual(host.codec.decode(host.manifest.wait.ready, encoded), keys);
        const ready = await host.wait(
            host.codec.encode(host.manifest.wait.set, []),
            host.codec.encode("Int", 0n),
        );
        assert.deepEqual(host.codec.decode(host.manifest.wait.ready, ready), []);

        // A wait over two sockets, handed over in the opposite order to the one
        // the program's map puts them in. The answer is owed in the map's order,
        // once per key, whatever order the socket adapter reported them in.
        const listening = id => ({ variant: "Socket", fields: [{ variant: "Listening", fields: [{ id }] }] });
        const set = host.codec.encode(host.manifest.wait.set, [
            [{ variant: "Peer", fields: [7n] }, listening("tcp-listener-2")],
            [{ variant: "Listener", fields: [] }, listening("tcp-listener-1")],
        ]);
        const order = host.codec.decode(host.manifest.wait.set, set).map(([key]) => key);
        assert.deepEqual(order, keys, "a map orders a variant key by its constructor");
        adapter = entries => [...entries].reverse().map(([key]) => key).concat(entries[0][0]);
        const both = await host.wait(set, host.codec.encode("Int", 0n));
        assert.deepEqual(host.codec.decode(host.manifest.wait.ready, both), order);

        // A socket adapter that answers with a key it built rather than one it
        // was handed cannot be placed in that order, and is refused by name.
        adapter = () => [{ variant: "Listener", fields: [] }];
        await assert.rejects(
            host.wait(set, host.codec.encode("Int", 0n)),
            /pollSockets answered with a key that is not one of the keys it was handed/,
        );
    } finally { await host.close(); }
    console.log("socket-only wait ABI passed");
}

// JSPI lets a hand-written main yield inside Wait.poll. Both workers must
// make progress while that Wasm stack is suspended, and cancel must stop
// the infinite worker before the host closes.
if (typeof WebAssembly.Suspending === "function") {
    const lines = [];
    let jspi;
    const wait_poll = new WebAssembly.Suspending(async (items, timeout) => {
        try {
            const ready = await jspi.wait(items, timeout);
            return jspi.instance.exports.__rt_result_wait_keys_ok(ready);
        } catch (error) {
            return jspi.instance.exports.__rt_result_wait_keys_err(jspi.codec.stringIn(String(error)));
        }
    });
    jspi = await createWorkHost(await WebAssembly.compile(await readFile(parallelFile)), {
        maxJobs: 2, onPrint: line => lines.push(line), imports: { aver: { wait_poll } },
    });
    try {
        const result = await WebAssembly.promising(jspi.instance.exports.main)();
        assert.deepEqual(jspi.codec.decode("Result<Unit, String>", result), { ok: null });
        assert.deepEqual(lines, ["parallel 340282366920938463463374607431768211457"]);
    } finally { await jspi.close(); }
    console.log("JSPI Work main passed");
}
