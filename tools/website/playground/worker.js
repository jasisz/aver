import { AverBrowserHost } from "./wasm_host.js";

const host = new AverBrowserHost((message, transfer) => self.postMessage(message, transfer ?? []));

async function instantiateAndCallEntry(wasmBytes) {
    const userImports = host.createImports();
    const { instance } = await WebAssembly.instantiate(wasmBytes, userImports);
    host.setInstance(instance);
    if (typeof instance.exports._start === "function") {
        instance.exports._start();
    } else if (typeof instance.exports.main === "function") {
        instance.exports.main();
    } else {
        throw new Error("Module exports neither `_start` nor `main`.");
    }
}

async function runModule(wasmBytes) {
    try {
        host.recorder.setNormal();
        host.post({ type: "status", level: "info", text: "Instantiating module…" });
        host.postTerminalSnapshot();
        host.post({ type: "status", level: "success", text: "Running…" });
        await instantiateAndCallEntry(wasmBytes);
        host.postTerminalSnapshot();
        host.post({ type: "status", level: "success", text: "Finished." });
        host.post({ type: "finished", ok: true });
    } catch (error) {
        console.error("[aver-wasm-gc]", error);
        const message = error instanceof Error ? error.message : String(error);
        host.post({ type: "status", level: "error", text: message });
        host.post({ type: "finished", ok: false, error: message });
    }
}

/// Drive a `--record` session natively under V8 wasm-gc instead of
/// bouncing through VM-in-wasm32. Returns the same recording JSON
/// shape the CLI produces, so a downloaded `.replay.json` from the
/// playground replays under `aver replay --wasm-gc` (and vice versa).
async function recordModule(wasmBytes, programArgs, programFile, moduleRoot) {
    try {
        host.recorder.startRecording();
        host.capNotified = false;
        host.post({ type: "status", level: "info", text: "Recording…" });
        host.postTerminalSnapshot();
        await instantiateAndCallEntry(wasmBytes);
        host.postTerminalSnapshot();
        const effects = host.recorder.takeRecordedEffects();
        const recording = {
            schema_version: 1,
            request_id: `rec-${Date.now()}`,
            timestamp: `unix-${Math.floor(Date.now() / 1000)}`,
            program_file: programFile ?? "playground.av",
            module_root: moduleRoot ?? ".",
            entry_fn: "main",
            input: null,
            effects,
            // Output value comparison for the playground's native
            // wasm-gc path is deferred: ref-typed main returns need a
            // compiler-injected `__rt_main_to_lm_json` per main return
            // type, which is its own compiler change. Until then,
            // recording.output stays null and `MATCH` is determined
            // by the effect-sequence + outcomes.
            output: { kind: "value", value: null },
        };
        host.recorder.setNormal();
        host.post({
            type: "record-finished",
            ok: true,
            recording,
            effect_count: effects.length,
        });
    } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        host.recorder.setNormal();
        host.post({ type: "record-finished", ok: false, error: message });
    }
}

/// Drive a `--replay` session natively. The recorder is primed with
/// `recording.effects` and the module re-runs end-to-end; every
/// host import pulls its outcome from the trace via
/// `recordOrDispatch` instead of touching the real I/O. After the
/// program returns, `ensureReplayConsumed` raises if the program was
/// a strict prefix of the trace (mirrors the CLI contract).
async function replayModule(wasmBytes, recording, checkArgs) {
    try {
        const effects = Array.isArray(recording?.effects)
            ? recording.effects
            : [];
        host.recorder.startReplay(effects, !!checkArgs);
        host.post({ type: "status", level: "info", text: "Replaying…" });
        host.postTerminalSnapshot();
        await instantiateAndCallEntry(wasmBytes);
        host.postTerminalSnapshot();
        host.recorder.ensureReplayConsumed();
        const [consumed, total] = host.recorder.replayProgress();
        const argsDiffs = host.recorder.argsDiffCount;
        host.recorder.setNormal();
        host.post({
            type: "replay-finished",
            ok: true,
            matched: true,
            replayed: consumed,
            total,
            args_diffs: argsDiffs,
        });
    } catch (error) {
        const [consumed, total] = host.recorder.replayProgress();
        const argsDiffs = host.recorder.argsDiffCount;
        host.recorder.setNormal();
        host.post({
            type: "replay-finished",
            ok: false,
            matched: false,
            replayed: consumed,
            total,
            args_diffs: argsDiffs,
            error: error instanceof Error ? error.message : String(error),
        });
    }
}

self.onmessage = (event) => {
    const { type } = event.data ?? {};

    if (type === "init-input") {
        host.setSharedKeyBuffer(event.data.keyBuffer);
        if (event.data.lineBuffer) host.setSharedLineBuffer(event.data.lineBuffer);
        return;
    }

    if (type === "run") {
        host.setProgramArgs(event.data.programArgs ?? []);
        runModule(event.data.wasmBytes);
        return;
    }

    if (type === "record") {
        host.setProgramArgs(event.data.programArgs ?? []);
        recordModule(
            event.data.wasmBytes,
            event.data.programArgs ?? [],
            event.data.programFile,
            event.data.moduleRoot,
        );
        return;
    }

    if (type === "replay") {
        host.setProgramArgs(event.data.programArgs ?? []);
        replayModule(
            event.data.wasmBytes,
            event.data.recording,
            event.data.checkArgs,
        );
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
