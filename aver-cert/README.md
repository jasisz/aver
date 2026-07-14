# aver-cert

`aver-cert` owns Aver's byte-level WebAssembly certificate engine and the
checker-embedded Lean soundness wall. It is deliberately independent from the
Aver compiler and runtime crates.

Verify an emitted package directly:

```text
aver-cert verify app.wasm out/cert
aver-cert explain app.wasm out/cert
```

The library exposes the same fail-closed check as `aver_cert::verify`. The
`aver cert ...` command is only a process-level shortcut: it forwards its raw
arguments and standard streams to a sibling `aver-cert` executable, falling
back to `PATH`. It contains no linked verifier or alternate acceptance path.
