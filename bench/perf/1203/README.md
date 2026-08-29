# Byte-keyed wasm-gc Map benchmark

This manual benchmark reproduces the `Map<Bytes, Int>` workload from
[issue #1203](https://github.com/jasisz/aver/issues/1203): 200,000 resident
37-byte keys, followed by 2,000,000 successful or unsuccessful lookups.
Keys model four Bitcoin transaction outputs each (`tag || txid[32] || vout[4]`).

Run the three measurements independently so one V8 process cannot warm or
disturb another:

```bash
aver bench bench/perf/1203/map_bytes_build.toml --target=wasm-gc-v8 --json
aver bench bench/perf/1203/map_bytes_hit.toml --target=wasm-gc-v8 --json
aver bench bench/perf/1203/map_bytes_miss.toml --target=wasm-gc-v8 --json
```

The manifests use two warmups and seven measured iterations. They live outside
`bench/scenarios/` intentionally: this is a multi-second performance probe, not
part of the ordinary benchmark smoke gate.
