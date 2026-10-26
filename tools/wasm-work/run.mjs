// Node host for raw `--target wasm-gc` programs: runs the generated
// coordinator when the module has one and `main` otherwise, with the process's
// arguments, Console and the Disk reads.
//
//   node [--max-old-space-size=MB] tools/wasm-work/run.mjs [--max-jobs N] [--stack-mb N] program.wasm [args...]
import fs from "node:fs";
import { Worker, isMainThread } from "node:worker_threads";
import { createWorkHost } from "./host.mjs";

const argv = process.argv.slice(2);
const options = {};
while (["--max-jobs", "--stack-mb"].includes(argv[0])) options[argv.shift()] = Number(argv.shift());
const [file, ...args] = argv;
if (!file) throw new Error("Usage: node tools/wasm-work/run.mjs [--max-jobs N] [--stack-mb N] program.wasm [args...]");
// Synchronous writes keep stdout and stderr in program order and lose nothing
// when the process ends right after main.
const line = fd => text => fs.writeSync(fd, `${text}\n`);
// The program runs on a thread of its own because only a worker's stack can be
// sized from here: the main thread's is about 1 MB, which a deeply recursive
// program outgrows, and the system caps how far a flag can raise it.
if (isMainThread) {
    const stackSizeMb = options["--stack-mb"] ?? 1024;
    const worker = new Worker(new URL(import.meta.url), { argv: process.argv.slice(2), resourceLimits: { stackSizeMb } });
    worker.on("error", error => line(2)(error?.stack ?? String(error)));
    worker.on("exit", code => { process.exitCode ||= code; });
} else {
    const module = await WebAssembly.compile(fs.readFileSync(file));
    let host;
    const ex = () => host.instance.exports;
    const text = value => host.codec.stringOut(value);
    const str = value => host.codec.stringIn(value);
    // A boxed Int the host reads as i64; out of range answers `undefined`.
    const small = value => { try { return ex().__rt_aint_to_i64_checked(value); } catch { return undefined; } };
    const memoryFor = length => {
        const memory = ex().memory;
        if (length > memory.buffer.byteLength) memory.grow(Math.ceil((length - memory.buffer.byteLength) / 65536));
        return new Uint8Array(memory.buffer, 0, length);
    };
    // Positional reads go straight into the transport memory, so a large file is
    // never held whole: only the chunk a call asks for.
    function readBytesAt(path, offsetRef, lengthRef) {
        const err = message => ex().__rt_result_bytes_string_err(str(message));
        const offset = small(offsetRef), length = small(lengthRef);
        if (offset === undefined) return err("Disk.readBytesAt: offset must fit a 64-bit integer");
        if (length === undefined) return err("Disk.readBytesAt: length must fit a 64-bit integer");
        if (offset < 0n) return err("Disk.readBytesAt: offset must be non-negative");
        if (length < 0n) return err("Disk.readBytesAt: length must be non-negative");
        let fd;
        try {
            fd = fs.openSync(text(path), "r");
            const left = BigInt(fs.fstatSync(fd).size) - offset;
            const buffer = memoryFor(Number(left < length ? (left > 0n ? left : 0n) : length));
            let read = 0;
            for (let n; read < buffer.length && (n = fs.readSync(fd, buffer, read, buffer.length - read, Number(offset) + read)); ) read += n;
            return ex().__rt_result_bytes_string_ok_from_lm(read);
        } catch (error) { return err(error.message); } finally { if (fd !== undefined) fs.closeSync(fd); }
    }
    function readText(path) {
        try {
            const bytes = fs.readFileSync(text(path));
            new TextDecoder("utf-8", { fatal: true }).decode(bytes);
            memoryFor(bytes.length).set(bytes);
            return ex().__rt_result_string_string_ok(ex().__rt_string_from_lm(bytes.length));
        } catch (error) { return ex().__rt_result_string_string_err(str(error.message)); }
    }
    const aver = {
        args_len: () => BigInt(args.length),
        args_get: index => str(args[Number(index)] ?? ""),
        console_error: value => line(2)(text(value)),
        console_warn: value => line(2)(`[warn] ${text(value)}`),
        disk_read_bytes_at: readBytesAt,
        disk_read_text: readText,
    };
    host = await createWorkHost(module, { imports: { aver }, onPrint: line(1), ...(options["--max-jobs"] && { maxJobs: options["--max-jobs"] }) });
    const e = host.instance.exports;
    try {
        if (e.__workHostStart) {
            const result = await host.runCoordinator();
            if (Object.hasOwn(result, "err")) throw new Error(result.err);
        } else if (e.main) {
            // `main -> Int` names the exit status, as under `aver run`.
            const answer = e.main();
            if (answer !== undefined) {
                const status = small(answer);
                if (status === undefined) throw new Error("main answered something other than an Int within 64 bits; this host reads only that as an exit status");
                if (status < 0n || status > 255n) line(2)(`main returned exit code ${status}, outside 0..255`);
                process.exitCode = status >= 0n && status <= 255n ? Number(status) : 1;
            }
        } else e._start();
    } finally { await host.close(); }
}
