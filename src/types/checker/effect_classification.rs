//! Oracle v1 effect classification.
//!
//! For each effect method covered by `aver proof`, this module records:
//!
//! - Which proof dimension(s) it belongs to (snapshot / generative / output,
//!   and the combination `generative + output` used by e.g. `Http.get`).
//! - For snapshot and generative, the corresponding capability/oracle
//!   signature that lifted specs bind via `given name: E.m = [...]`.
//!
//! Output-only effects (for example `Console.print` and terminal drawing
//! calls) are classified but do not have an oracle signature:
//! they append to the per-branch trace segment and are asserted about via the
//! trace API, not by binding an oracle in `given`.
//!
//! The table is the single source of truth consumed by:
//!
//! - `given`-clause type inference (`given rnd: Random.int` → oracle type
//!   `(BranchPath, Int, Int, Int) -> Result<Int, String>`).
//! - Lifting of effectful function bodies at proof-export time.
//! - Rejection diagnostics for unclassified effects.
//!
//! Source of runtime signatures: builtin registrations for legacy services,
//! and capability declarations for provider-backed standard/project effects.
//!
//! The parameter and return types below are cross-checked against either the
//! signatures `builtins.rs` registers or the canonical standard capability
//! contracts — see
//! `every_classified_entry_matches_its_builtin_or_standard_contract_signature`. The
//! agreement used to rest on a comment asking the reader to keep the two in
//! sync, and that had already failed: `Console.print` carried a printable type
//! variable here for twelve minor releases after 0.16 gave it a plain string.
//! What is still hand-authored for legacy builtins, and cannot be derived from
//! `FnSig`, is the DIMENSION. Standard and project capabilities instead derive
//! it from their source-owned Oracle declaration.

use super::super::Type;
use crate::types::branch_path;
use std::sync::OnceLock;

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

/// Classification of one effect method. `params` and `ret` mirror the surface
/// signature at call sites in user code; oracle signatures are derived from
/// them (see [`oracle_signature`]).
#[derive(Debug, Clone)]
pub struct EffectClassification {
    pub method: &'static str,
    pub dimension: EffectDimension,
    pub params: Vec<Type>,
    pub ret: Type,
}

/// Owned classification returned by a whole-program registry lookup. Legacy
/// builtins and source-derived standard capabilities are cloned from the
/// process registry; program-defined capability operations are derived from
/// their canonical declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct RegisteredEffectClassification {
    pub method: String,
    pub dimension: EffectDimension,
    pub params: Vec<Type>,
    pub ret: Type,
}

/// `Result<ok, String>` — the shape every fallible service method returns.
/// Spelled once here so the table below reads as the signatures do, rather
/// than as a wall of `Box::new`.
fn err_str(ok: Type) -> Type {
    Type::Result(Box::new(ok), Box::new(Type::Str))
}

/// Full classification table, built once on first use.
///
/// It was a `const` array until now, which forced every parameter and return
/// type through a closed `RuntimeType` enum whose only purpose was to be
/// const-constructible — `Type` carries `Box` and `String`, so it cannot
/// appear in a `const`. That enum had to gain a variant for every type shape
/// any effect could mention, and it is where `FormattedValue` went stale for
/// twelve releases: a carrier nobody reads is a carrier nobody notices.
///
/// A table built at runtime holds ordinary `Type` values, so the shapes are
/// spelled the way `builtins.rs` spells them and there is no second
/// vocabulary to keep in step. It is also the shape this has to be in before
/// an effect can come from anywhere but this file — a `const` can never hold
/// an entry a program contributes.
fn classifications() -> &'static [EffectClassification] {
    static REGISTRY: OnceLock<Vec<EffectClassification>> = OnceLock::new();
    REGISTRY.get_or_init(|| {
        let mut entries = vec![
            // Snapshot
            EffectClassification {
                method: "Args.get",
                dimension: EffectDimension::Snapshot,
                params: vec![],
                ret: Type::List(Box::new(Type::Str)),
            },
            EffectClassification {
                method: "Env.get",
                dimension: EffectDimension::Snapshot,
                params: vec![Type::Str],
                ret: Type::Option(Box::new(Type::Str)),
            },
            // Terminal.size: stable within a verify scope (a resize while
            // proving is not modelled). Host failure remains explicit in the
            // snapshot value: () -> Result<Terminal.Size, String>.
            EffectClassification {
                method: "Terminal.size",
                dimension: EffectDimension::Snapshot,
                params: vec![],
                ret: err_str(Type::named("Terminal.Size")),
            },
            // Generative
            EffectClassification {
                method: "Console.readLine",
                dimension: EffectDimension::Generative,
                params: vec![],
                ret: err_str(Type::Str),
            },
            // Generative + output (Http)
            EffectClassification {
                method: "Http.get",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![Type::Str],
                ret: err_str(Type::named("HttpResponse")),
            },
            EffectClassification {
                method: "Http.head",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![Type::Str],
                ret: err_str(Type::named("HttpResponse")),
            },
            EffectClassification {
                method: "Http.delete",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![Type::Str],
                ret: err_str(Type::named("HttpResponse")),
            },
            // Http.post/.put/.patch — four-arg form `(url, body, contentType, headers)`.
            EffectClassification {
                method: "Http.post",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![
                    Type::Str,
                    Type::Str,
                    Type::Str,
                    Type::Map(
                        Box::new(Type::Str),
                        Box::new(Type::List(Box::new(Type::Str))),
                    ),
                ],
                ret: err_str(Type::named("HttpResponse")),
            },
            EffectClassification {
                method: "Http.put",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![
                    Type::Str,
                    Type::Str,
                    Type::Str,
                    Type::Map(
                        Box::new(Type::Str),
                        Box::new(Type::List(Box::new(Type::Str))),
                    ),
                ],
                ret: err_str(Type::named("HttpResponse")),
            },
            EffectClassification {
                method: "Http.patch",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![
                    Type::Str,
                    Type::Str,
                    Type::Str,
                    Type::Map(
                        Box::new(Type::Str),
                        Box::new(Type::List(Box::new(Type::Str))),
                    ),
                ],
                ret: err_str(Type::named("HttpResponse")),
            },
            // Output-only — no oracle signature, but classified for completeness.
            // Env.set is stateless under Oracle: emitted to trace, but does NOT
            // make a later `Env.get` return the written value. If the program
            // depends on read-after-write consistency, the model belongs in pure
            // user code, not in the effect oracle.
            EffectClassification {
                method: "Env.set",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![Type::Str, Type::Str],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Console.print",
                dimension: EffectDimension::Output,
                params: vec![Type::Str],
                ret: Type::Unit,
            },
            EffectClassification {
                method: "Console.error",
                dimension: EffectDimension::Output,
                params: vec![Type::Str],
                ret: Type::Unit,
            },
            EffectClassification {
                method: "Console.warn",
                dimension: EffectDimension::Output,
                params: vec![Type::Str],
                ret: Type::Unit,
            },
            EffectClassification {
                method: "Terminal.clear",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.moveTo",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![Type::Int, Type::Int],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.print",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![Type::Str],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.readKey",
                dimension: EffectDimension::Generative,
                params: vec![],
                ret: err_str(Type::Option(Box::new(Type::Str))),
            },
            EffectClassification {
                method: "Terminal.hideCursor",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.showCursor",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.flush",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![],
                ret: err_str(Type::Unit),
            },
            // Terminal modal/visual effects also return their adapter outcome.
            // The oracle still does NOT model that a later `print` is "now in
            // raw mode" or "now in red"; it models only this call's Result.
            EffectClassification {
                method: "Terminal.enableRawMode",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.disableRawMode",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.setColor",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![Type::Str],
                ret: err_str(Type::Unit),
            },
            EffectClassification {
                method: "Terminal.resetColor",
                dimension: EffectDimension::GenerativeOutput,
                params: vec![],
                ret: err_str(Type::Unit),
            },
        ];
        let standard = crate::stdlib::standard_capability_registry_ref();
        for operation in standard
            .operations()
            .filter(|operation| operation.is_effectful())
        {
            let dimension = match operation.oracle.expect("effectful standard oracle") {
                crate::capability::OracleDimension::Snapshot => EffectDimension::Snapshot,
                crate::capability::OracleDimension::Generative => EffectDimension::Generative,
                crate::capability::OracleDimension::Output => EffectDimension::Output,
                crate::capability::OracleDimension::GenerativeOutput => {
                    EffectDimension::GenerativeOutput
                }
            };
            let method: &'static str = operation.canonical_name.as_str();
            entries.push(EffectClassification {
                method,
                dimension,
                params: operation.params.iter().map(|(_, ty)| ty.clone()).collect(),
                ret: operation.return_type.clone(),
            });
        }
        entries
    })
}

/// Classify a builtin or standard-capability effect method, if Oracle v1 knows it.
pub fn classify(method: &str) -> Option<&'static EffectClassification> {
    classifications().iter().find(|c| c.method == method)
}

pub fn classify_with_registry(
    registry: &crate::capability::CapabilityRegistry,
    method: &str,
) -> Option<RegisteredEffectClassification> {
    if let Some(classification) = classify(method) {
        return Some(RegisteredEffectClassification {
            method: classification.method.to_string(),
            dimension: classification.dimension,
            params: classification.params.clone(),
            ret: classification.ret.clone(),
        });
    }
    let operation = registry.operation(method)?;
    if !operation.is_effectful() {
        return None;
    }
    let dimension = match operation.oracle? {
        crate::capability::OracleDimension::Snapshot => EffectDimension::Snapshot,
        crate::capability::OracleDimension::Generative => EffectDimension::Generative,
        crate::capability::OracleDimension::Output => EffectDimension::Output,
        crate::capability::OracleDimension::GenerativeOutput => EffectDimension::GenerativeOutput,
    };
    Some(RegisteredEffectClassification {
        method: operation.canonical_name.clone(),
        dimension,
        params: operation.params.iter().map(|(_, ty)| ty.clone()).collect(),
        ret: operation.return_type.clone(),
    })
}

/// Closed Oracle v1 proof-subset table, exposed for proof metadata
/// generation. Callers must treat the returned slice as read-only metadata.
pub fn classifications_for_proof_subset() -> &'static [EffectClassification] {
    classifications()
}

/// Return `true` if the given name refers to an effect covered by Oracle v1.
pub fn is_classified(method: &str) -> bool {
    classify(method).is_some()
}

/// Render the classified set as a namespace-grouped list for diagnostics:
/// `Args.get, Console.error/.print/.readLine/.warn, Disk.appendText/…`.
///
/// Derived from [`classifications()`] so a message that tells the user which
/// effects the proof subset covers can never name a different set than the
/// one the checker enforces. The hand-written version of this sentence had
/// drifted to 34 of the 47 methods — `Terminal.size` was missing from it
/// while `examples/formal/terminal_size_snapshot.av` demonstrated it.
///
/// Namespaces and methods are both sorted, so the rendering does not depend
/// on the order of the table and cannot change when an entry is moved.
pub fn classified_effects_summary() -> String {
    use std::collections::BTreeMap;

    let mut grouped: BTreeMap<&'static str, Vec<&'static str>> = BTreeMap::new();
    for c in classifications() {
        let (namespace, method) = match c.method.split_once('.') {
            Some(parts) => parts,
            // Every classified effect is `Namespace.method`; a bare name has
            // no namespace to group under, so it stands on its own.
            None => (c.method, ""),
        };
        grouped.entry(namespace).or_default().push(method);
    }

    grouped
        .into_iter()
        .map(|(namespace, mut methods)| {
            methods.sort_unstable();
            methods.dedup();
            let mut rendered = String::from(namespace);
            for (index, method) in methods.iter().enumerate() {
                if method.is_empty() {
                    continue;
                }
                rendered.push_str(if index == 0 { "." } else { "/." });
                rendered.push_str(method);
            }
            rendered
        })
        .collect::<Vec<String>>()
        .join(", ")
}

/// Oracle signature for use in lifted specs.
///
/// - Snapshot: capability reader — unchanged from runtime signature,
///   wrapped in a function type. `Args.get` → `() -> List<String>`.
/// - Generative / GenerativeOutput: branch-indexed oracle —
///   `(BranchPath, Int, <params...>) -> <ret>`.
/// - Output: `None` — output effects don't bind oracles (trace API
///   handles assertions about emissions).
pub fn oracle_signature(method: &str) -> Option<Type> {
    if let Some(operation) = crate::stdlib::standard_capability_registry_ref().operation(method) {
        return capability_oracle_signature(operation);
    }
    let c = classify(method)?;
    match c.dimension {
        EffectDimension::Output => None,
        EffectDimension::Snapshot => {
            let params: Vec<Type> = c.params.clone();
            Some(Type::Fn(params, Box::new(c.ret.clone()), vec![]))
        }
        EffectDimension::Generative | EffectDimension::GenerativeOutput => {
            let mut params = vec![Type::named(branch_path::TYPE_NAME.to_string()), Type::Int];
            params.extend(c.params.iter().cloned());
            Some(Type::Fn(params, Box::new(c.ret.clone()), vec![]))
        }
    }
}

pub fn oracle_signature_with_registry(
    registry: &crate::capability::CapabilityRegistry,
    method: &str,
) -> Option<Type> {
    if let Some(operation) = registry.operation(method) {
        return capability_oracle_signature(operation);
    }
    let c = classify_with_registry(registry, method)?;
    match c.dimension {
        EffectDimension::Output => None,
        EffectDimension::Snapshot => Some(Type::Fn(c.params, Box::new(c.ret), vec![])),
        EffectDimension::Generative | EffectDimension::GenerativeOutput => {
            let mut params = vec![Type::named(branch_path::TYPE_NAME.to_string()), Type::Int];
            params.extend(c.params);
            Some(Type::Fn(params, Box::new(c.ret), vec![]))
        }
    }
}

fn capability_oracle_signature(operation: &crate::capability::CapabilityOperation) -> Option<Type> {
    match operation.oracle {
        Some(crate::capability::OracleDimension::Output) | None => None,
        Some(crate::capability::OracleDimension::Snapshot) => Some(Type::Fn(
            operation.params.iter().map(|(_, ty)| ty.clone()).collect(),
            Box::new(operation.return_type.clone()),
            vec![],
        )),
        Some(
            crate::capability::OracleDimension::Generative
            | crate::capability::OracleDimension::GenerativeOutput,
        ) => Some(Type::Fn(
            operation.oracle_params(),
            Box::new(operation.return_type.clone()),
            vec![],
        )),
    }
}

pub fn classified_effects_summary_with_registry(
    registry: &crate::capability::CapabilityRegistry,
) -> String {
    let mut names: Vec<String> = classifications()
        .iter()
        .map(|classification| classification.method.to_string())
        .collect();
    names.extend(
        registry
            .operations()
            .filter(|operation| operation.is_effectful())
            .map(|operation| operation.canonical_name.clone()),
    );
    names.sort();
    names.dedup();
    names.join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn capability_registry(scope: &str, source: &str) -> crate::capability::CapabilityRegistry {
        let items = crate::source::parse_source(source).expect("parse capability differential");
        let (registry, errors) = crate::capability::CapabilityRegistry::from_module(scope, &items);
        assert!(
            errors.is_empty(),
            "capability differential errors: {errors:?}"
        );
        registry
    }

    fn assert_same_shape(
        standard: &str,
        custom: &str,
        registry: &crate::capability::CapabilityRegistry,
    ) {
        let expected = classify_with_registry(&Default::default(), standard)
            .unwrap_or_else(|| panic!("missing standard classification {standard}"));
        let actual = classify_with_registry(registry, custom)
            .unwrap_or_else(|| panic!("missing capability classification {custom}"));
        assert_eq!(actual.dimension, expected.dimension, "{standard}");
        assert_eq!(actual.params, expected.params, "{standard}");
        assert_eq!(actual.ret, expected.ret, "{standard}");
        assert_eq!(
            oracle_signature_with_registry(registry, custom),
            oracle_signature(standard),
            "{standard} oracle signature"
        );
    }

    #[test]
    fn every_classified_entry_matches_its_builtin_or_standard_contract_signature() {
        // Legacy rows and `builtins.rs` describe the same effects twice: the
        // table for the proof pipeline, `builtins.rs` for the checker. Standard
        // capability rows instead come from their source contract. Cross-check
        // each classification against whichever owner supplies its signature.
        // The old comment-only synchronization had already failed —
        // `Console.print` carried a printable type variable here for twelve
        // minor releases after 0.16 made it take a plain string.
        //
        // It survived that long because the drifted field is unreachable for
        // output-dimension effects, so nothing downstream ever read it. The
        // next drift need not be so lucky, hence a mechanical cross-check
        // rather than a stricter comment.
        let items = crate::source::parse_source(
            "module TableCrossCheck\n    intent = \"Cross-check effect classifications against builtins and standard capability contracts.\"\n",
        )
        .expect("the cross-check module must parse");
        let checked = super::super::run_type_check_full(&items, None);
        let standard = crate::stdlib::standard_capability_registry();

        for c in classifications() {
            let builtin_shape = checked
                .fn_sigs
                .get(c.method)
                .map(|(params, ret, _)| (params.clone(), ret.clone()));
            let capability_shape = standard.operation(c.method).map(|operation| {
                (
                    operation.params.iter().map(|(_, ty)| ty.clone()).collect(),
                    operation.return_type.clone(),
                )
            });
            let (params, ret) = builtin_shape.or(capability_shape).unwrap_or_else(|| {
                panic!(
                    "{} is classified for proof but has neither a builtin nor a standard capability contract",
                    c.method
                )
            });
            let expected_params: Vec<Type> = c.params.clone();
            assert_eq!(
                params, expected_params,
                "{} takes different parameters here than its runtime contract registers",
                c.method
            );
            assert_eq!(
                ret, c.ret,
                "{} returns something different here than its runtime contract registers",
                c.method
            );
        }
    }

    #[test]
    fn program_capability_mechanism_explains_terminal_except_audited_snapshot() {
        let registry = capability_registry(
            "TerminalCompat",
            "module TerminalCompat\n    kind = capability\n    semantics = effectful\n\noperation clear() -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation moveTo(row: Int, col: Int) -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation print(text: String) -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation readKey() -> Result<Option<String>, String>\n    oracle = generative\n    replay = recorded\n\noperation hideCursor() -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation showCursor() -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation flush() -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation enableRawMode() -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation disableRawMode() -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation setColor(color: String) -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n\noperation resetColor() -> Result<Unit, String>\n    oracle = generativeOutput\n    replay = recorded\n",
        );
        for method in [
            "clear",
            "moveTo",
            "print",
            "readKey",
            "hideCursor",
            "showCursor",
            "flush",
            "enableRawMode",
            "disableRawMode",
            "setColor",
            "resetColor",
        ] {
            assert_same_shape(
                &format!("Terminal.{method}"),
                &format!("TerminalCompat.{method}"),
                &registry,
            );
        }

        // Terminal.size is the deliberate standard-only exception: its
        // snapshot promise was audited by Aver. A program cannot grant itself
        // that proof power with one attribute.
        assert_eq!(
            classify("Terminal.size").expect("Terminal.size").dimension,
            EffectDimension::Snapshot
        );
        let source = "module TerminalCompat\n    kind = capability\n    semantics = effectful\n\noperation size() -> Int\n    oracle = snapshot\n    replay = recorded\n";
        let items = crate::source::parse_source(source).expect("parse snapshot refusal");
        let (_, errors) =
            crate::capability::CapabilityRegistry::from_module("TerminalCompat", &items);
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("cannot claim `oracle = snapshot`"))
        );
    }

    #[test]
    fn program_capability_http_get_has_the_standard_proof_shape() {
        let registry = capability_registry(
            "HttpCompat",
            "module HttpCompat\n    kind = capability\n    semantics = effectful\n\nrecord HttpResponse\n    status: Int\n    body: String\n    headers: Map<String, List<String>>\n\noperation get(url: String) -> Result<HttpResponse, String>\n    oracle = generativeOutput\n    replay = recorded\n",
        );
        assert_same_shape("Http.get", "HttpCompat.get", &registry);
    }

    #[test]
    fn summary_names_every_classified_method() {
        // The diagnostic that tells a user which effects the proof subset
        // covers is derived from this table, so it cannot name a smaller set
        // than the checker enforces. The hand-written sentence it replaced had
        // drifted to 34 of the 47 methods.
        let summary = classified_effects_summary();
        for c in classifications() {
            let (namespace, method) = c.method.split_once('.').expect("dotted effect name");
            let grouped = format!("/.{method}");
            let leading = format!("{namespace}.{method}");
            assert!(
                summary.contains(&leading) || summary.contains(&grouped),
                "{} is classified but missing from the diagnostic summary:\n{summary}",
                c.method
            );
        }
    }

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
    fn time_sleep_is_generative_output() {
        let c = classify("Time.sleep").unwrap();
        assert_eq!(c.dimension, EffectDimension::GenerativeOutput);
    }

    #[test]
    fn terminal_read_key_is_generative() {
        let c = classify("Terminal.readKey").unwrap();
        assert_eq!(c.dimension, EffectDimension::Generative);
    }

    #[test]
    fn oracle_signature_for_random_int_is_branch_indexed() {
        let sig = oracle_signature("Random.int").unwrap();
        // (BranchPath, Int, Int, Int) -> Result<Int, String>
        match sig {
            Type::Fn(params, ret, _) => {
                assert_eq!(params.len(), 4);
                assert!(matches!(params[0], Type::Named { name: ref n, .. } if n == "BranchPath"));
                assert_eq!(params[1], Type::Int);
                assert_eq!(params[2], Type::Int);
                assert_eq!(params[3], Type::Int);
                assert_eq!(*ret, err_str(Type::Int));
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
        assert!(oracle_signature("Time.sleep").is_some());
        assert!(oracle_signature("Terminal.print").is_some());
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
            "Disk.readBytes",
            "Disk.readBytesAt",
            "Disk.size",
            "Disk.exists",
            "Disk.listDir",
            "Disk.writeText",
            "Disk.appendText",
            "Disk.writeBytes",
            "Disk.appendBytes",
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
    fn extended_oracle_v1_methods_classified() {
        // Every terminal write/control operation exposes the adapter outcome,
        // so each has both a trace request and an oracle Result.
        for name in &[
            "Terminal.clear",
            "Terminal.moveTo",
            "Terminal.print",
            "Terminal.hideCursor",
            "Terminal.showCursor",
            "Terminal.flush",
            "Terminal.enableRawMode",
            "Terminal.disableRawMode",
            "Terminal.setColor",
            "Terminal.resetColor",
        ] {
            let c = classify(name).unwrap_or_else(|| panic!("{} should be classified", name));
            assert_eq!(c.dimension, EffectDimension::GenerativeOutput);
        }
        // Env.set likewise has both a trace request and an oracle result.
        for name in &["Env.set"] {
            let c = classify(name).unwrap_or_else(|| panic!("{} should be classified", name));
            assert_eq!(c.dimension, EffectDimension::GenerativeOutput);
        }
        // GenerativeOutput: session TCP — request emitted, response from oracle.
        // Stateless: writeLine does not affect a later readLine.
        for name in &[
            "Tcp.connect",
            "Tcp.readLine",
            "Tcp.readBytes",
            "Tcp.writeLine",
            "Tcp.writeBytes",
            "Tcp.close",
        ] {
            let c = classify(name).unwrap_or_else(|| panic!("{} should be classified", name));
            assert_eq!(c.dimension, EffectDimension::GenerativeOutput);
        }
    }

    #[test]
    fn terminal_size_is_snapshot() {
        let c = classify("Terminal.size").expect("Terminal.size should be classified");
        assert_eq!(c.dimension, EffectDimension::Snapshot);
        assert!(c.params.is_empty());
        assert_eq!(c.ret, err_str(Type::named("Terminal.Size")));

        // Oracle signature: () -> Result<Terminal.Size, String>
        let sig = oracle_signature("Terminal.size").unwrap();
        match sig {
            Type::Fn(params, ret, effects) => {
                assert!(params.is_empty());
                assert_eq!(*ret, err_str(Type::named("Terminal.Size")));
                assert!(effects.is_empty());
            }
            other => panic!("expected Fn, got {:?}", other),
        }
    }
}
