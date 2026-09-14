// Node example host for generated Aver coordinators using Work.
import { readFile } from "node:fs/promises";
import { createWorkHost } from "./host.mjs";

const [file, count] = process.argv.slice(2);
if (!file) throw new Error("Usage: node tools/wasm-work/run.mjs program.wasm [max-jobs]");
const module = await WebAssembly.compile(await readFile(file));
const host = await createWorkHost(module, count === undefined ? {} : { maxJobs: Number(count) });
const result = await host.runCoordinator();
if (Object.hasOwn(result, "err")) throw new Error(result.err);
