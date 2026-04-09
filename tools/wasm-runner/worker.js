import { AverBrowserHost } from "./wasm_host.js";

const host = new AverBrowserHost((message, transfer) => self.postMessage(message, transfer ?? []));

async function runModule(wasmBytes) {
    try {
        host.post({ type: "status", level: "info", text: "Instantiating module…" });
        const { instance } = await WebAssembly.instantiate(wasmBytes, host.createImports());
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
        return;
    }

    if (type === "run") {
        host.setProgramArgs(event.data.programArgs ?? []);
        runModule(event.data.wasmBytes);
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
