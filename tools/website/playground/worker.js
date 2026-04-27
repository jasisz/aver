import { AverBrowserHost } from "./wasm_host.js";

const host = new AverBrowserHost((message, transfer) => self.postMessage(message, transfer ?? []));

// aver_runtime owns memory + the bump allocator (see 0.14 Edge Step 1).
// Cache the instance: it's stateless across runs from the user's POV
// (start fn re-bumps heap_ptr per user instantiation), so one runtime
// instance can be re-used for every user.wasm we run in this worker.
let runtimeInstance = null;
let runtimeBytesCache = null;

async function ensureRuntime(runtimeBytes) {
    if (runtimeBytes && runtimeBytes !== runtimeBytesCache) {
        runtimeBytesCache = runtimeBytes;
        runtimeInstance = null;
    }
    if (runtimeInstance) return runtimeInstance;
    if (!runtimeBytesCache) {
        throw new Error(
            "aver_runtime bytes missing — main thread must send them before the first run."
        );
    }
    const { instance } = await WebAssembly.instantiate(runtimeBytesCache, {});
    runtimeInstance = instance;
    return instance;
}

async function runModule(wasmBytes, runtimeBytes) {
    try {
        host.post({ type: "status", level: "info", text: "Instantiating module…" });

        const runtime = await ensureRuntime(runtimeBytes);
        const userImports = host.createImports();
        userImports.aver_runtime = runtime.exports;

        const { instance } = await WebAssembly.instantiate(wasmBytes, userImports);
        host.setInstance(instance);
        host.postTerminalSnapshot();
        host.post({ type: "status", level: "success", text: "Running…" });

        if (typeof instance.exports._start === "function") {
            instance.exports._start();
        } else if (typeof instance.exports.main === "function") {
            instance.exports.main();
        } else {
            throw new Error("Module exports neither `_start` nor `main`.");
        }

        host.postTerminalSnapshot();
        host.post({ type: "status", level: "success", text: "Finished." });
        host.post({ type: "finished", ok: true });
    } catch (error) {
        console.error("[aver-wasm]", error);
        const message = error instanceof Error ? error.message : String(error);
        host.post({ type: "status", level: "error", text: message });
        host.post({ type: "finished", ok: false, error: message });
    }
}

self.onmessage = (event) => {
    const { type } = event.data ?? {};

    if (type === "init-input") {
        host.setSharedKeyBuffer(event.data.keyBuffer);
        if (event.data.lineBuffer) {
            host.setSharedLineBuffer(event.data.lineBuffer);
        }
        return;
    }

    if (type === "runtime-bytes") {
        // Cache the aver_runtime wasm so subsequent runs don't re-fetch
        // it. Sent once by the main thread after compiler init.
        runtimeBytesCache = event.data.runtimeBytes;
        runtimeInstance = null;
        return;
    }

    if (type === "run") {
        host.setProgramArgs(event.data.programArgs ?? []);
        runModule(event.data.wasmBytes, event.data.runtimeBytes);
        return;
    }

    if (type === "resize") {
        host.setTerminalSize(event.data.cols, event.data.rows);
        return;
    }

    if (type === "key") {
        host.enqueueKey(event.data.key);
        return;
    }

    if (type === "line") {
        host.enqueueLine(event.data.line);
    }
};
