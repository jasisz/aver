//! Oracle v1 effect classification.
//!
//! For each built-in effect method covered by `aver proof`, this module
//! records:
//!
//! - Which proof dimension(s) it belongs to (snapshot / generative / output,
//!   and the combination `generative + output` used by e.g. `Http.get`).
//! - For snapshot and generative, the corresponding capability/oracle
//!   signature that lifted specs bind via `given name: E.m = [...]`.
//!
//! Output-only effects (for example `Console.print`, `Time.sleep`, and
//! terminal drawing calls) are classified but do not have an oracle signature:
//! they append to the per-branch trace segment and are asserted about via the
//! trace API, not by binding an oracle in `given`.
//!
//! The table is the single source of truth consumed by:
//!
//! - `given`-clause type inference (`given rnd: Random.int` → oracle type
//!   `(BranchPath, Int, Int, Int) -> Int`).
//! - Lifting of effectful function bodies at proof-export time.
//! - Rejection diagnostics for unclassified effects.
//!
//! Source of runtime signatures: `src/services/*.rs` and `docs/services.md`.
//! Keep this table synchronized with the real built-ins.

use super::super::Type;
use crate::types::branch_path;

/// Proof dimension(s) an effect participates in. `!`-combinations are
/// modelled directly rather than as flags for readability at call sites.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EffectDimension {
    /// Stable within a run. Modelled as a plain reader function.
    Snapshot,
    /// Fresh value per call. Modelled as a branch-indexed oracle.
    Generative,
    /// Trace-appending side-effect only. No oracle; assertions via trace API.
    Output,
    /// Both generative (response value from oracle) and output (request
    /// emitted to trace). Used by request/operation-style effects such as
    /// `Http.*`, mutating `Disk.*`, and one-shot `Tcp.*`.
    GenerativeOutput,
}

/// Classification of one effect method. `runtime_params` and
/// `runtime_return` mirror the surface signature at call sites in user
/// code; oracle signatures are derived from them (see [`oracle_signature`]).
#[derive(Debug, Clone)]
pub struct EffectClassification {
    pub method: &'static str,
    pub dimension: EffectDimension,
    pub runtime_params: &'static [RuntimeType],
    pub runtime_return: RuntimeType,
}

/// Compact carrier for runtime signature components — kept separate from
/// the full [`Type`] enum so the static table can live as a const array.
/// Converted into [`Type`] on demand via [`runtime_type`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeType {
    FormattedValue,
    Unit,
    Int,
    Float,
    Str,
    Bool,
    OptionStr,
    ListStr,
    ResultUnitStr,
    ResultStrStr,
    ResultListStrStr,
    HttpResponseResult,
    /// `Map<String, List<String>>` — the headers argument on
    /// `Http.post/put/patch`. Matches the runtime `HttpHeaders` type
    /// and the `HttpRequest`/`HttpResponse` `headers` field.
    MapStrListStr,
    /// `Terminal.Size` record — the return of `Terminal.size`.
    TerminalSize,
    /// `Tcp.Connection` opaque token — argument/return of `Tcp.*` session methods.
    TcpConnection,
    /// `Result<Tcp.Connection, Str>` — return of `Tcp.connect`.
    ResultTcpConnectionStr,
}

impl RuntimeType {
    fn as_type(self) -> Type {
        match self {
            RuntimeType::FormattedValue => Type::Var("FormattedValue".to_string()),
            RuntimeType::Unit => Type::Unit,
            RuntimeType::Int => Type::Int,
            RuntimeType::Float => Type::Float,
            RuntimeType::Str => Type::Str,
            RuntimeType::Bool => Type::Bool,
            RuntimeType::OptionStr => Type::Option(Box::new(Type::Str)),
            RuntimeType::ListStr => Type::List(Box::new(Type::Str)),
            RuntimeType::ResultUnitStr => Type::Result(Box::new(Type::Unit), Box::new(Type::Str)),
            RuntimeType::ResultStrStr => Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
            RuntimeType::ResultListStrStr => Type::Result(
                Box::new(Type::List(Box::new(Type::Str))),
                Box::new(Type::Str),
            ),
            RuntimeType::HttpResponseResult => {
                Type::Result(Box::new(Type::named("HttpResponse")), Box::new(Type::Str))
            }
            RuntimeType::MapStrListStr => Type::Map(
                Box::new(Type::Str),
                Box::new(Type::List(Box::new(Type::Str))),
            ),
            RuntimeType::TerminalSize => Type::named("Terminal.Size"),
            RuntimeType::TcpConnection => Type::named("Tcp.Connection"),
            RuntimeType::ResultTcpConnectionStr => {
                Type::Result(Box::new(Type::named("Tcp.Connection")), Box::new(Type::Str))
            }
        }
    }
}

fn runtime_type(rt: RuntimeType) -> Type {
    rt.as_type()
}

/// Full classification table. This is the closed set for Oracle v1.
const CLASSIFICATIONS: &[EffectClassification] = &[
    // Snapshot
    EffectClassification {
        method: "Args.get",
        dimension: EffectDimension::Snapshot,
        runtime_params: &[],
        runtime_return: RuntimeType::ListStr,
    },
    EffectClassification {
        method: "Env.get",
        dimension: EffectDimension::Snapshot,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::OptionStr,
    },
    // Terminal.size: stable within a verify scope (a resize while
    // proving is not modelled). Snapshot-shape oracle: () -> Terminal.Size.
    EffectClassification {
        method: "Terminal.size",
        dimension: EffectDimension::Snapshot,
        runtime_params: &[],
        runtime_return: RuntimeType::TerminalSize,
    },
    // Generative
    EffectClassification {
        method: "Random.int",
        dimension: EffectDimension::Generative,
        runtime_params: &[RuntimeType::Int, RuntimeType::Int],
        runtime_return: RuntimeType::Int,
    },
    EffectClassification {
        method: "Random.float",
        dimension: EffectDimension::Generative,
        runtime_params: &[],
        runtime_return: RuntimeType::Float,
    },
    EffectClassification {
        method: "Time.now",
        dimension: EffectDimension::Generative,
        runtime_params: &[],
        runtime_return: RuntimeType::Str,
    },
    EffectClassification {
        method: "Time.unixMs",
        dimension: EffectDimension::Generative,
        runtime_params: &[],
        runtime_return: RuntimeType::Int,
    },
    EffectClassification {
        method: "Disk.readText",
        dimension: EffectDimension::Generative,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::ResultStrStr,
    },
    EffectClassification {
        method: "Disk.exists",
        dimension: EffectDimension::Generative,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::Bool,
    },
    EffectClassification {
        method: "Disk.listDir",
        dimension: EffectDimension::Generative,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::ResultListStrStr,
    },
    EffectClassification {
        method: "Console.readLine",
        dimension: EffectDimension::Generative,
        runtime_params: &[],
        runtime_return: RuntimeType::ResultStrStr,
    },
    // Generative + output (Http)
    EffectClassification {
        method: "Http.get",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::HttpResponseResult,
    },
    EffectClassification {
        method: "Http.head",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::HttpResponseResult,
    },
    EffectClassification {
        method: "Http.delete",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::HttpResponseResult,
    },
    // Http.post/.put/.patch — four-arg form `(url, body, contentType, headers)`.
    EffectClassification {
        method: "Http.post",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[
            RuntimeType::Str,
            RuntimeType::Str,
            RuntimeType::Str,
            RuntimeType::MapStrListStr,
        ],
        runtime_return: RuntimeType::HttpResponseResult,
    },
    EffectClassification {
        method: "Http.put",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[
            RuntimeType::Str,
            RuntimeType::Str,
            RuntimeType::Str,
            RuntimeType::MapStrListStr,
        ],
        runtime_return: RuntimeType::HttpResponseResult,
    },
    EffectClassification {
        method: "Http.patch",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[
            RuntimeType::Str,
            RuntimeType::Str,
            RuntimeType::Str,
            RuntimeType::MapStrListStr,
        ],
        runtime_return: RuntimeType::HttpResponseResult,
    },
    // Disk writes/deletes are modelled like HTTP writes: the operation is
    // emitted to the trace, and success/failure comes from the oracle. Oracle
    // does not assert persistent filesystem state after the operation.
    EffectClassification {
        method: "Disk.writeText",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str, RuntimeType::Str],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    EffectClassification {
        method: "Disk.appendText",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str, RuntimeType::Str],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    EffectClassification {
        method: "Disk.delete",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    EffectClassification {
        method: "Disk.deleteDir",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    EffectClassification {
        method: "Disk.makeDir",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    // One-shot TCP operations — request is trace output, response comes from oracle.
    EffectClassification {
        method: "Tcp.send",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str, RuntimeType::Int, RuntimeType::Str],
        runtime_return: RuntimeType::ResultStrStr,
    },
    EffectClassification {
        method: "Tcp.ping",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str, RuntimeType::Int],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    // Session TCP — connection is an opaque token. Stubs are stateless: a
    // `writeLine` does not affect a later `readLine`. If a test wants
    // request/response symmetry, it must encode that explicitly in the stub.
    EffectClassification {
        method: "Tcp.connect",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::Str, RuntimeType::Int],
        runtime_return: RuntimeType::ResultTcpConnectionStr,
    },
    EffectClassification {
        method: "Tcp.readLine",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::TcpConnection],
        runtime_return: RuntimeType::ResultStrStr,
    },
    EffectClassification {
        method: "Tcp.writeLine",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::TcpConnection, RuntimeType::Str],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    EffectClassification {
        method: "Tcp.close",
        dimension: EffectDimension::GenerativeOutput,
        runtime_params: &[RuntimeType::TcpConnection],
        runtime_return: RuntimeType::ResultUnitStr,
    },
    // Output-only — no oracle signature, but classified for completeness.
    // Env.set is stateless under Oracle: emitted to trace, but does NOT
    // make a later `Env.get` return the written value. If the program
    // depends on read-after-write consistency, the model belongs in pure
    // user code, not in the effect oracle.
    EffectClassification {
        method: "Env.set",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::Str, RuntimeType::Str],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Console.print",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::FormattedValue],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Console.error",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::FormattedValue],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Console.warn",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::FormattedValue],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Time.sleep",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::Int],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.clear",
        dimension: EffectDimension::Output,
        runtime_params: &[],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.moveTo",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::Int, RuntimeType::Int],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.print",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::FormattedValue],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.readKey",
        dimension: EffectDimension::Generative,
        runtime_params: &[],
        runtime_return: RuntimeType::OptionStr,
    },
    EffectClassification {
        method: "Terminal.hideCursor",
        dimension: EffectDimension::Output,
        runtime_params: &[],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.showCursor",
        dimension: EffectDimension::Output,
        runtime_params: &[],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.flush",
        dimension: EffectDimension::Output,
        runtime_params: &[],
        runtime_return: RuntimeType::Unit,
    },
    // Terminal modal/visual — output only. Mode and color changes are
    // observable via trace; the oracle does NOT model that a later `print`
    // is "now in raw mode" or "now in red". If a test cares, it asserts the
    // sequence of trace events.
    EffectClassification {
        method: "Terminal.enableRawMode",
        dimension: EffectDimension::Output,
        runtime_params: &[],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.disableRawMode",
        dimension: EffectDimension::Output,
        runtime_params: &[],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.setColor",
        dimension: EffectDimension::Output,
        runtime_params: &[RuntimeType::Str],
        runtime_return: RuntimeType::Unit,
    },
    EffectClassification {
        method: "Terminal.resetColor",
        dimension: EffectDimension::Output,
        runtime_params: &[],
        runtime_return: RuntimeType::Unit,
    },
];

/// Classify a built-in effect method, if it's in Oracle v1's closed set.
pub fn classify(method: &str) -> Option<&'static EffectClassification> {
    CLASSIFICATIONS.iter().find(|c| c.method == method)
}

/// Closed Oracle v1 proof-subset table, exposed for proof metadata
/// generation. Callers must treat the returned slice as read-only metadata.
pub fn classifications_for_proof_subset() -> &'static [EffectClassification] {
    CLASSIFICATIONS
}

/// Return `true` if the given name refers to an effect covered by Oracle v1.
pub fn is_classified(method: &str) -> bool {
    classify(method).is_some()
}

/// Opaque types that are runtime handles (id + connection metadata),
/// not domain-invariant smart-constructor types. These may be fabricated
/// inside verify-trace context so that Oracle stubs can return them
/// without going through the live effect that normally produces them
/// (e.g. `Tcp.connect`). All four soundness conditions from PR 221 apply:
///
/// 1. Every effect that can observe/consume the fake handle must be
///    stubbed, or the verify block is rejected (existing behavior in
///    `flow.rs` — see "needs a `given` stub" error).
/// 2. Pure code outside the defining module cannot inspect fields or
///    pattern-match the value just because it was fabricated. Field
///    access on opaque types is rejected outside the defining module
///    regardless of this flag.
/// 3. Verify-block bodies are not lowered to executable artifacts, so
///    fabricated handles cannot escape to compiled programs.
/// 4. Runtime handle identity is uninterpreted test data unless an
///    Oracle stub assigns meaning to it.
///
/// User-defined opaque types are NOT eligible — their opacity protects
/// domain invariants that this fabrication would erase.
pub fn is_verify_fabricable_handle(canonical_type: &str) -> bool {
    matches!(canonical_type, "Tcp.Connection")
}

/// Oracle signature for use in lifted specs.
///
/// - Snapshot: capability reader — unchanged from runtime signature,
///   wrapped in a function type. `Args.get` → `() -> List<String>`.
/// - Generative / GenerativeOutput: branch-indexed oracle —
///   `(BranchPath, Int, <runtime_params...>) -> <runtime_return>`.
/// - Output: `None` — output effects don't bind oracles (trace API
///   handles assertions about emissions).
pub fn oracle_signature(method: &str) -> Option<Type> {
    let c = classify(method)?;
    match c.dimension {
        EffectDimension::Output => None,
        EffectDimension::Snapshot => {
            let params: Vec<Type> = c.runtime_params.iter().copied().map(runtime_type).collect();
            Some(Type::Fn(
                params,
                Box::new(runtime_type(c.runtime_return)),
                vec![],
            ))
        }
        EffectDimension::Generative | EffectDimension::GenerativeOutput => {
            let mut params = vec![Type::named(branch_path::TYPE_NAME.to_string()), Type::Int];
            params.extend(c.runtime_params.iter().copied().map(runtime_type));
            Some(Type::Fn(
                params,
                Box::new(runtime_type(c.runtime_return)),
                vec![],
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_returns_none_for_unknown() {
        assert!(classify("Nope.missing").is_none());
        assert!(classify("Args.set").is_none());
    }

    #[test]
    fn args_get_is_snapshot() {
        let c = classify("Args.get").unwrap();
        assert_eq!(c.dimension, EffectDimension::Snapshot);
    }

    #[test]
    fn random_int_is_generative() {
        let c = classify("Random.int").unwrap();
        assert_eq!(c.dimension, EffectDimension::Generative);
    }

    #[test]
    fn http_get_is_generative_output() {
        let c = classify("Http.get").unwrap();
        assert_eq!(c.dimension, EffectDimension::GenerativeOutput);
    }

    #[test]
    fn disk_write_text_is_generative_output() {
        let c = classify("Disk.writeText").unwrap();
        assert_eq!(c.dimension, EffectDimension::GenerativeOutput);
    }

    #[test]
    fn console_print_is_output() {
        let c = classify("Console.print").unwrap();
        assert_eq!(c.dimension, EffectDimension::Output);
    }

    #[test]
    fn console_read_line_is_generative() {
        let c = classify("Console.readLine").unwrap();
        assert_eq!(c.dimension, EffectDimension::Generative);
    }

    #[test]
    fn time_sleep_is_output() {
        let c = classify("Time.sleep").unwrap();
        assert_eq!(c.dimension, EffectDimension::Output);
    }

    #[test]
    fn terminal_read_key_is_generative() {
        let c = classify("Terminal.readKey").unwrap();
        assert_eq!(c.dimension, EffectDimension::Generative);
    }

    #[test]
    fn oracle_signature_for_random_int_is_branch_indexed() {
        let sig = oracle_signature("Random.int").unwrap();
        // (BranchPath, Int, Int, Int) -> Int
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 4);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(params[2], Type::Int);
                assert_eq!(params[3], Type::Int);
                assert_eq!(*ret, Type::Int);
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_random_float_is_branch_indexed_no_extra_args() {
        let sig = oracle_signature("Random.float").unwrap();
        // (BranchPath, Int) -> Float
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 2);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(*ret, Type::Float);
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_args_get_is_capability_reader() {
        let sig = oracle_signature("Args.get").unwrap();
        // () -> List<String>   (snapshot: not branch-indexed)
        match sig {
            Type::Fn(params, ret, _) => {
                assert!(params.is_empty());
                assert_eq!(*ret, Type::List(Box::new(Type::Str)));
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_env_get_is_capability_reader() {
        let sig = oracle_signature("Env.get").unwrap();
        // String -> Option<String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params, vec![Type::Str]);
                assert_eq!(*ret, Type::Option(Box::new(Type::Str)));
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_http_get_is_branch_indexed() {
        let sig = oracle_signature("Http.get").unwrap();
        // (BranchPath, Int, String) -> Result<HttpResponse, String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 3);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(params[2], Type::Str);
                match *ret {
                    Type::Result(ok, err) => {
                        assert!(
                            matches!(*ok, Type::Named { name: ref n, .. } if n == "HttpResponse")
                        );
                        assert_eq!(*err, Type::Str);
                    }
                    other => panic!("expected Result, got {:?}", other),
                }
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_console_read_line_is_branch_indexed() {
        let sig = oracle_signature("Console.readLine").unwrap();
        // (BranchPath, Int) -> Result<String, String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 2);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(*ret, Type::Result(Box::new(Type::Str), Box::new(Type::Str)));
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_disk_list_dir_returns_result_list_string() {
        let sig = oracle_signature("Disk.listDir").unwrap();
        // (BranchPath, Int, String) -> Result<List<String>, String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 3);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(params[2], Type::Str);
                assert_eq!(
                    *ret,
                    Type::Result(
                        Box::new(Type::List(Box::new(Type::Str))),
                        Box::new(Type::Str)
                    )
                );
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_tcp_ping_returns_result_unit_string() {
        let sig = oracle_signature("Tcp.ping").unwrap();
        // (BranchPath, Int, String, Int) -> Result<Unit, String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 4);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(params[2], Type::Str);
                assert_eq!(params[3], Type::Int);
                assert_eq!(
                    *ret,
                    Type::Result(Box::new(Type::Unit), Box::new(Type::Str))
                );
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_disk_write_text_returns_result_unit_string() {
        let sig = oracle_signature("Disk.writeText").unwrap();
        // (BranchPath, Int, String, String) -> Result<Unit, String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 4);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(params[2], Type::Str);
                assert_eq!(params[3], Type::Str);
                assert_eq!(
                    *ret,
                    Type::Result(Box::new(Type::Unit), Box::new(Type::Str))
                );
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn oracle_signature_for_output_effect_is_none() {
        assert!(oracle_signature("Console.print").is_none());
        assert!(oracle_signature("Console.error").is_none());
        assert!(oracle_signature("Console.warn").is_none());
        assert!(oracle_signature("Time.sleep").is_none());
        assert!(oracle_signature("Terminal.print").is_none());
    }

    #[test]
    fn is_classified_covers_full_v1_set() {
        for name in &[
            "Args.get",
            "Env.get",
            "Random.int",
            "Random.float",
            "Time.now",
            "Time.unixMs",
            "Time.sleep",
            "Disk.readText",
            "Disk.exists",
            "Disk.listDir",
            "Disk.writeText",
            "Disk.appendText",
            "Disk.delete",
            "Disk.deleteDir",
            "Disk.makeDir",
            "Console.readLine",
            "Http.get",
            "Http.head",
            "Http.delete",
            "Http.post",
            "Http.put",
            "Http.patch",
            "Tcp.send",
            "Tcp.ping",
            "Console.print",
            "Console.error",
            "Console.warn",
            "Terminal.clear",
            "Terminal.moveTo",
            "Terminal.print",
            "Terminal.readKey",
            "Terminal.hideCursor",
            "Terminal.showCursor",
            "Terminal.flush",
        ] {
            assert!(is_classified(name), "{} should be classified", name);
        }
    }

    #[test]
    fn oracle_signature_for_http_post_has_four_runtime_params() {
        let sig = oracle_signature("Http.post").unwrap();
        // (BranchPath, Int, Str, Str, Str, Map<Str, List<Str>>) -> Result<HttpResponse, String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 6);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(params[2], Type::Str);
                assert_eq!(params[3], Type::Str);
                assert_eq!(params[4], Type::Str);
                match &params[5] {
                    Type::Map(key, value) => {
                        assert_eq!(**key, Type::Str);
                        match &**value {
                            Type::List(inner) => assert_eq!(**inner, Type::Str),
                            other => panic!("expected Map<Str, List<Str>>, got {:?}", other),
                        }
                    }
                    other => panic!("expected Map<Str, List<Str>>, got {:?}", other),
                }
                match *ret {
                    Type::Result(ok, err) => {
                        assert!(
                            matches!(*ok, Type::Named { name: ref n, .. } if n == "HttpResponse")
                        );
                        assert_eq!(*err, Type::Str);
                    }
                    other => panic!("expected Result, got {:?}", other),
                }
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }

    #[test]
    fn server_lifecycle_remains_unclassified() {
        // HttpServer.listen is a long-running protocol with callbacks — its
        // handler is the unit of proof, not the lifecycle call itself. Stays
        // outside Oracle by design.
        for name in &["HttpServer.listen", "HttpServer.listenWith"] {
            assert!(!is_classified(name), "{} should NOT be classified", name);
        }
    }

    #[test]
    fn extended_oracle_v1_methods_classified() {
        // Output: env writes and terminal modal/visual changes — emitted to
        // trace, no oracle signature.
        for name in &[
            "Env.set",
            "Terminal.enableRawMode",
            "Terminal.disableRawMode",
            "Terminal.setColor",
            "Terminal.resetColor",
        ] {
            let c = classify(name).unwrap_or_else(|| panic!("{} should be classified", name));
            assert_eq!(c.dimension, EffectDimension::Output);
        }
        // GenerativeOutput: session TCP — request emitted, response from oracle.
        // Stateless: writeLine does not affect a later readLine.
        for name in &["Tcp.connect", "Tcp.readLine", "Tcp.writeLine", "Tcp.close"] {
            let c = classify(name).unwrap_or_else(|| panic!("{} should be classified", name));
            assert_eq!(c.dimension, EffectDimension::GenerativeOutput);
        }
    }

    #[test]
    fn terminal_size_is_snapshot() {
        let c = classify("Terminal.size").expect("Terminal.size should be classified");
        assert_eq!(c.dimension, EffectDimension::Snapshot);
        assert!(c.runtime_params.is_empty());
        assert_eq!(c.runtime_return, RuntimeType::TerminalSize);

        // Oracle signature: () -> Terminal.Size
        let sig = oracle_signature("Terminal.size").unwrap();
        match sig {
            Type::Fn(params, ret, effects) => {
                assert!(params.is_empty());
                assert_eq!(*ret, Type::named("Terminal.Size"));
                assert!(effects.is_empty());
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }
}
