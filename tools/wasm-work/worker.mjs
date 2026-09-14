// The compute worker has no program effects and never calls main/_start.
import { WorkCodec, workManifest } from "./codec.mjs";

const port = typeof globalThis.postMessage === "function" ? globalThis : (await import("node:worker_threads")).parentPort;
const send = message => port.postMessage(message);
let instance, codec, manifest, current;

async function receive(message) {
    if (message.type === "init") {
        try {
            manifest = workManifest(message.module);
            const imports = {};
            for (const { module, name, kind } of WebAssembly.Module.imports(message.module)) {
                if (kind !== "function") throw new Error("work: worker imports a non-function");
                (imports[module] ??= {})[name] = () => { throw new Error(`work: pure worker called ${module}.${name}`); };
            }
            imports["aver:work/v1"].task = kind => {
                if (!current || current.kind !== kind || current.read) throw new Error("work: invalid task read");
                current.read = true;
                return codec.encode(manifest.kinds[kind].boxedTask, { some: current.task });
            };
            imports["aver:work/v1"].complete = (kind, value) => {
                if (!current || current.kind !== kind || current.done) throw new Error("work: invalid completion");
                current.done = true;
                current.value = codec.decode(`Option<${manifest.kinds[kind].payload}>`, value).some;
            };
            instance = await WebAssembly.instantiate(message.module, imports);
            codec = new WorkCodec(instance.exports, manifest);
            send({ type: "ready" });
        } catch (error) { send({ type: "init-error", error: String(error) }); }
        return;
    }
    if (message.type === "run") {
        current = { ...message, read: false, done: false };
        try {
            instance.exports[manifest.kinds[current.kind].run]();
            if (!current.done) throw new Error("work: worker returned without an answer");
            send({ type: "done", id: current.id, value: current.value });
        } catch (error) { send({ type: "failed", id: current.id, error: String(error) }); }
        finally { current = null; }
    }
}

if (port.addEventListener) port.addEventListener("message", event => receive(event.data));
else port.on("message", receive);
