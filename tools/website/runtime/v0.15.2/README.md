# Aver runtime — v0.15.2

Standalone WebAssembly modules for the Aver language runtime.
Pair these with a thin `user.wasm` produced by
`aver compile --target edge-wasm --optimize size`:

- `aver_runtime.wasm` (10,393 B) — alloc, GC, hashmap,
  string/list/vector ops. Imported by every Aver program as the
  `aver_runtime` module. Cached once per session.
- `aver_to_wasi.wasm` (1,914 B) — translation shim that
  satisfies a program's `aver/*` host imports against
  `wasi_snapshot_preview1.fd_write`. Optional, only needed if you
  want to run a thin user.wasm under wasmtime / Cloudflare
  Workers / Fastly Compute.

`.wat` files are human-readable disassemblies, not required at
runtime — they're shipped so you can inspect what the runtime
actually contains.

`CHECKSUMS.txt` lists sha256 sums of every binary file in this
directory.
