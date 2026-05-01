//! Scenario-based benchmark harness.
//!
//! A scenario is a TOML manifest pinning everything needed to reproduce
//! a measured run: which Aver file is the entry point, how many warmup
//! and timed iterations, what the expected response shape is. The harness
//! runs the canonical pipeline + VM for the requested number of iterations
//! and emits a structured JSON result that downstream tools (`--compare
//! baseline.json`, the future CI gate, dashboards) can parse without
//! scraping stdout.
//!
//! 0.15.1 ships VM target only. `wasm-local` (wasmtime in-process) and
//! `wasm-cloudflare` (wrangler dev driver) follow in 0.15.2 — by then
//! `compiler_visible_allocs` will be filled in too. For now those
//! fields are emitted as `null` so the JSON shape is stable.
//!
//! Layering note: bench numbers from different targets are in *different
//! units* (the peer review trap noted in `project_016_observability_first`).
//! `wall_time_ms` is the only field comparable across targets in the
//! current shape; `compiler_visible_allocs` will be (when populated, it
//! comes out of an IR-level analysis, not a runtime counter); per-target
//! `backend_native_allocs` will not be — it lands in a separate per-target
//! stanza in 0.15.2 to make the layering explicit.
//!
//! See `bench/scenarios/*.toml` for example manifests.

mod compare;
mod manifest;
mod report;
mod runner;

pub use compare::{DiffReport, MetricDiff, diff, format_diff};
pub use manifest::{Manifest, ManifestError, Tolerance};
pub use report::{BenchReport, IterationStats, ScenarioMetadata, format_human};
pub use runner::run_vm_scenario;
