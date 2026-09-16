import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { createWorkHost } from "./host.mjs";

const [parallelFile, guideFile, recordFile, unitFile, unitResultFile] = process.argv.slice(2);
const host = await createWorkHost(await WebAssembly.compile(await readFile(parallelFile)), { maxJobs: 2 });
try {
    const { exports: e } = host.instance, c = host.codec;
    const start = task => c.decode("Result<Work.Job, String>", e.start(task ? 1 : 0));
    const take = job => c.decode("Result<Option<Int>, String>", e.take(job));
    const stuck = start(true).ok;
    const quick = start(false).ok;
    assert.deepEqual(start(true), { err: "work: job limit 2 reached" });
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

// JSPI lets a hand-written main yield inside Wait.poll. Both workers must
// make progress while that Wasm stack is suspended, and cancel must stop
// the infinite worker before the host closes.
if (typeof WebAssembly.Suspending === "function") {
    const lines = [];
    let jspi;
    const wait_poll = new WebAssembly.Suspending(async (items, timeout) => {
        try {
            const ready = await jspi.wait(items, timeout);
            return jspi.instance.exports.__rt_result_list_int_string_ok(ready);
        } catch (error) {
            return jspi.instance.exports.__rt_result_list_int_string_err(jspi.codec.stringIn(String(error)));
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
