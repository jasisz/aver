# aver-cert

`aver-cert` owns Aver's byte-level WebAssembly certificate engine and the
checker-embedded Lean soundness wall. It is deliberately independent from the
Aver compiler and runtime crates.

The standalone verifier CLI will live in this package. Until that command is
extracted, `aver cert` continues to use the compatibility API re-exported by
`aver-lang`.
