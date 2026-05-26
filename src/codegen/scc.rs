//! Bare-name → `FnId` projection helpers for codegen boundary code.
//!
//! `call_graph` is **scope-local by design** — every public fn operates on
//! a single scope (entry module OR one dep module), returns bare-name
//! results, and Aver's module DAG invariant keeps cross-module SCCs
//! impossible so the bare-name keying is correct within a scope (see
//! `project_aver_module_dag` memory).
//!
//! Codegen, however, needs `FnId` identity once it joins per-scope
//! results into a global view (`CodegenContext.recursive_fns` /
//! `mutual_tco_members` are FnId-keyed after #145 phase C). The
//! conversion is mechanical — pick `FnKey::entry` or `FnKey::in_module`
//! based on scope and look the ID up — but writing it inline in every
//! place that consumes a per-scope bare-name set turned `build_context`
//! / `refresh_facts` / `pipeline.rs` proof setup into ~120 lines of
//! near-identical `filter_map` blocks. These two helpers collapse the
//! pattern.

use std::collections::HashSet;

use crate::ir::{FnId, FnKey, SymbolTable};

/// Project a sequence of bare fn names (as `&str`) to `FnId`s through
/// the symbol table. `scope = None` resolves names against the entry
/// scope; `Some(prefix)` resolves against the dep module with that
/// prefix. Names that don't resolve (built-ins, synthetic helpers
/// the table excludes) are dropped silently — same semantics every
/// per-scope union loop already had.
pub fn bare_names_to_fn_ids<'a>(
    names: impl IntoIterator<Item = &'a str>,
    symbols: &SymbolTable,
    scope: Option<&str>,
) -> HashSet<FnId> {
    names
        .into_iter()
        .filter_map(|n| symbols.fn_id_of(&fn_key_in_scope(scope, n)))
        .collect()
}

/// Same shape as [`bare_names_to_fn_ids`] but accepts an iterator over
/// `&String` (which is what `HashSet<String>` / `Vec<String>` from
/// `AnalysisResult.recursive_fns` / `mutual_tco_members` yields). Avoids
/// `.iter().map(String::as_str)` ceremony at every call site.
pub fn analysis_set_to_fn_ids<'a, I>(
    names: I,
    symbols: &SymbolTable,
    scope: Option<&str>,
) -> HashSet<FnId>
where
    I: IntoIterator<Item = &'a String>,
{
    bare_names_to_fn_ids(names.into_iter().map(String::as_str), symbols, scope)
}

fn fn_key_in_scope(scope: Option<&str>, name: &str) -> FnKey {
    match scope {
        Some(prefix) => FnKey::in_module(prefix.to_string(), name),
        None => FnKey::entry(name),
    }
}
