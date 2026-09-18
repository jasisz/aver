// Live Work host for browsers and Node. runCoordinator yields at every wait;
// applications may also drive exported functions and await wait() themselves.
import { WorkCodec, workManifest } from "./codec.mjs";

export async function createWorkHost(module, options = {}) {
    const maxJobs = options.maxJobs ?? Math.min(globalThis.navigator?.hardwareConcurrency ?? 4, 8);
    if (!Number.isSafeInteger(maxJobs) || maxJobs < 1) throw new Error("work: maxJobs must be a positive integer");
    const manifest = workManifest(module);
    const WorkerClass = manifest.kinds.length
        ? options.Worker ?? globalThis.Worker ?? (await import("node:worker_threads")).Worker
        : null;
    const jobs = new Map(), slots = new Set(), listeners = new Set(), dead = [];
    let nextId = 0n, running = 0, closed = false, stopping = false, fatal;
    let instance, codec;
    const notify = () => { for (const listener of [...listeners]) listener(); };
    const retire = id => {
        dead.push(id);
        while (dead.length > 4096) jobs.delete(dead.shift());
    };
    function settle(slot, message) {
        const job = jobs.get(message.id);
        if (!job || job.slot !== slot || slot.id !== message.id || job.state !== "running") return;
        job.state = "finished";
        job.outcome = message.type === "done" ? { ok: { some: message.value } } : { err: `work: job trapped: ${message.error}` };
        running--;
        slot.id = null;
        notify();
        if (message.type === "failed") replace(slot);
    }
    async function replace(slot) {
        if (!slots.delete(slot)) return;
        try {
            await slot.worker.terminate();
            if (!closed) await newSlot();
        } catch (error) { fatal = error; notify(); }
    }
    async function newSlot() {
        const worker = new WorkerClass(new URL("./worker.mjs", import.meta.url), { type: "module" });
        const slot = { worker, id: null, ready: false };
        slots.add(slot);
        await new Promise((resolve, reject) => {
            const message = data => {
                if (data.type === "ready") { slot.ready = true; resolve(); notify(); }
                else if (data.type === "init-error") reject(new Error(data.error));
                else settle(slot, data);
            };
            const error = error => {
                if (!slot.ready) reject(error);
                else if (slot.id !== null) settle(slot, { id: slot.id, type: "failed", error: String(error) });
                else replace(slot);
            };
            if (worker.addEventListener) {
                worker.addEventListener("message", event => message(event.data));
                worker.addEventListener("error", error);
            } else {
                worker.on("message", message);
                worker.on("error", error);
                worker.on("exit", code => {
                    if (slots.has(slot) && !closed) error(new Error(`work: worker exited (${code})`));
                });
            }
            worker.postMessage({ type: "init", module });
        });
    }
    const refuse = error => instance.exports.__work_v1_refused(codec.stringIn(error));
    const imports = {};
    for (const { module: owner, name, kind } of WebAssembly.Module.imports(module)) {
        if (kind !== "function") throw new Error(`work: unsupported import ${owner}.${name}`);
        (imports[owner] ??= {})[name] = options.imports?.[owner]?.[name] ?? (() => { throw new Error(`host: missing ${owner}.${name}`); });
    }
    const aver = imports.aver ??= {};
    // Replace missing-import sentinels for the small standard live host.
    for (const [name, value] of Object.entries({
        console_print: value => (options.onPrint ?? console.log)(codec.stringOut(value)),
        time_unix_ms: () => BigInt(Date.now()),
        process_stop_requested: () => stopping ? 1 : 0,
    })) if (!options.imports?.aver?.[name]) aver[name] = value;
    imports["aver:work/v1"] = {
        submit(kind, task) {
            if (closed || fatal) return refuse(`work: host unavailable${fatal ? `: ${fatal}` : ""}`);
            if (!manifest.kinds[kind]) throw new Error("work: undeclared job kind");
            if (running >= maxJobs) return refuse(`work: job limit ${maxJobs} reached`);
            const slot = [...slots].find(slot => slot.ready && slot.id === null);
            if (!slot) return refuse("work: worker is restarting");
            const id = nextId + 1n;
            const owned = codec.decode(manifest.kinds[kind].boxedTask, task).some;
            try { slot.worker.postMessage({ type: "run", id, kind, task: owned }); }
            catch (error) { return refuse(`work: task transport failed: ${error}`); }
            nextId = id;
            slot.id = id;
            running++;
            jobs.set(id, { kind, slot, state: "running" });
            return instance.exports.__work_v1_started(id, kind);
        },
        take(kind, handle) {
            if (!manifest.kinds[kind]) throw new Error("work: undeclared job kind");
            const id = instance.exports.__work_v1_job_id(handle);
            const job = jobs.get(id);
            let value;
            if (!job) value = { err: "work: unknown job" };
            else if (job.kind !== kind) value = { err: `work: this job was not started by job kind '${manifest.kinds[kind].name}'` };
            else if (job.state === "running") value = { ok: null };
            else if (job.state === "taken") value = { err: "work: job already taken" };
            else if (job.state === "cancelled") value = { err: "work: job cancelled" };
            else { value = job.outcome; delete job.outcome; job.state = "taken"; retire(id); }
            return codec.encode(manifest.kinds[kind].answer, value);
        },
        task() { throw new Error("work: worker task import called in coordinator"); },
        complete() { throw new Error("work: worker completion import called in coordinator"); },
    };
    aver.work_cancel = handle => {
        const id = instance.exports.__work_v1_job_id(handle), job = jobs.get(id);
        if (!job || job.state === "taken" || job.state === "cancelled") return;
        if (job.state === "running") { running--; replace(job.slot); }
        job.state = "cancelled";
        delete job.outcome;
        retire(id);
        notify();
    };
    // Embedders using JSPI can suspend an ordinary main at Wait.poll and
    // delegate to host.wait(). Preserve their explicit async import.
    aver.wait_poll = options.imports?.aver?.wait_poll ?? (() => { throw new Error("work: synchronous Wait.poll cannot receive worker messages; use runCoordinator() or await host.wait()"); });

    async function close() {
        if (closed) return;
        closed = true;
        notify();
        await Promise.all([...slots].map(slot => slot.worker.terminate()));
        slots.clear(); jobs.clear();
    }
    try {
        // A program that runs no job of its own needs no workers. It still
        // needs this host: its wait is decoded and answered here.
        await Promise.all(Array.from({ length: manifest.kinds.length ? maxJobs : 0 }, newSlot));
        instance = await WebAssembly.instantiate(module, imports);
        codec = new WorkCodec(instance.exports, manifest);
    } catch (error) { await close(); throw error; }

    async function wait(waitSet, timeout) {
        // The wait set is keyed by whatever the program keys its waits by, and
        // the descriptor says which type that is. Keys are values here like
        // any other: this host correlates them and hands them back, and never
        // reads one.
        if (!manifest.wait) throw new Error("work: this module declares no wait set");
        const entries = codec.decode(manifest.wait.set, waitSet);
        const duration = codec.decode("Int", timeout);
        if (duration < 0n) throw new Error("Wait.poll: timeoutMs must be non-negative");
        const deadline = performance.now() + Number(duration);
        const socketEntries = entries.filter(([, item]) => item.variant.endsWith("Socket"));
        if (socketEntries.length && !options.pollSockets) throw new Error("Wait.poll: this host needs a pollSockets adapter");
        // Awaiting an already-resolved promise only drains microtasks. A run
        // full of NextTurn requests must still deliver worker message events.
        await new Promise(resolve => {
            if (typeof globalThis.setImmediate === "function") globalThis.setImmediate(resolve);
            else setTimeout(resolve, 0);
        });
        for (;;) {
            if (closed || fatal) throw fatal ?? new Error("work: host closed");
            const ready = entries.filter(([, item]) => item.variant.endsWith("Job") && jobs.get(instance.exports.__work_v1_job_id(item.fields[0]))?.state !== "running").map(([key]) => key);
            const remaining = Math.max(0, deadline - performance.now());
            const abort = new AbortController();
            let wake, timer;
            const changed = new Promise(resolve => { wake = () => resolve([]); listeners.add(wake); });
            try {
                const sockets = socketEntries.length ? options.pollSockets(socketEntries, ready.length ? 0 : remaining, abort.signal) : new Promise(() => {});
                if (ready.length) ready.push(...await socketsIfPresent(sockets, socketEntries));
                else if (remaining > 0) {
                    ready.push(...await Promise.race([changed, sockets, new Promise(resolve => { timer = setTimeout(() => resolve([]), Math.min(remaining, 2147483647)); })]));
                } else if (socketEntries.length) ready.push(...await sockets);
            } finally { listeners.delete(wake); clearTimeout(timer); abort.abort(); }
            // Answer in the order the program's own map puts its keys in.
            // `codec.decode` of a Map walks `Map.keys`, so `entries` is
            // already in that order and a key's position in it is that order:
            // no comparison of keys happens here, which is what lets the key
            // be a type this host has never seen. Every ready key is placed
            // back in that list first, so the answer is ordered and deduped by
            // position rather than by what a key compares or hashes as. A key
            // that is not in the list is a `pollSockets` adapter answering
            // with something other than the keys it was handed, which is
            // refused rather than answered in an order the contract denies.
            if (ready.length || performance.now() >= deadline || stopping) {
                const at = new Map(entries.map(([key], index) => [key, index]));
                const positions = new Set();
                for (const key of ready) {
                    const index = at.get(key);
                    if (index === undefined) throw new Error("Wait.poll: pollSockets answered with a key that is not one of the keys it was handed; answer with the key values out of the entries argument itself");
                    positions.add(index);
                }
                const answer = [...positions].sort((a, b) => a - b).map(index => entries[index][0]);
                return codec.encode(manifest.wait.ready, answer);
            }
        }
    }
    const socketsIfPresent = (promise, entries) => entries.length ? promise : [];
    async function runCoordinator() {
        const e = instance.exports;
        if (!e.__workHostStart) throw new Error("work: program has no generated coordinator; drive its exports explicitly");
        try {
            let run = e.__workHostStart();
            while (!e.__workHostStopped(run)) {
                run = e.__workHostObserve(run);
                const ready = await wait(e.__workHostWaitSet(run), e.__workHostTimeout(run));
                run = e.__workHostStep(run, ready);
            }
            return codec.decode("Result<Unit, String>", e.__workHostFinish(run));
        } finally { await close(); }
    }
    return { instance, codec, manifest, imports, wait, runCoordinator, close, stop() { stopping = true; notify(); } };
}
