//! Backend-agnostic plumbing for multi-file proof export.
//!
//! Multi-module Aver projects emit one file per dependent module plus a
//! shared common file (helpers + records, conditional on the union of
//! every body's actual usage) plus an entry file holding the trust
//! header and verify lemmas. The list of modules itself is a flat
//! `Vec<ModuleInfo>` carried through the entire pipeline; "directory
//! tree" is a backend-side realization (Rust synthesizes `mod.rs`
//! cascade, Lean/Dafny just map dotted prefix → `/`-joined path).
//!
//! What this module exposes is the orchestration shared regardless of
//! backend: per-scope SCC routing for pure fns. Each backend supplies
//! the per-fn rendering closure; the routing decides which scope each
//! component belongs to.
//!
//! Cross-module mutual recursion (SCC straddling modules) is out of
//! scope — global SCC would conflate same-bare-name fns from different
//! modules anyway. Per-scope SCC keeps the DAG case correct; the
//! cross-module case will get a flat fallback in 0.13.

use std::collections::HashMap;

use crate::ast::FnDef;
use crate::codegen::CodegenContext;

/// Sections gathered per emission scope ("" for entry, module prefix
/// otherwise). Each backend appends to the bucket for the scope a fn
/// (or its SCC component) belongs to.
pub(crate) struct PerScopeSections {
    pub by_scope: HashMap<String, Vec<String>>,
}

impl PerScopeSections {
    pub(crate) fn take(&mut self, scope: &str) -> Vec<String> {
        self.by_scope.remove(scope).unwrap_or_default()
    }
}

/// Run SCC analysis on each scope's pure fns independently and route the
/// rendered output through the supplied closure.
///
/// `is_pure` filters which fns participate (e.g. `is_pure_fn` for Lean,
/// or "no effects + not a fuel-emitted body" for Dafny).
/// `emit` receives one SCC component (>= 1 fn) and returns the lines to
/// push into that scope's bucket.
pub(crate) fn route_pure_components_per_scope<F, G>(
    ctx: &CodegenContext,
    is_pure: F,
    mut emit: G,
) -> PerScopeSections
where
    F: Fn(&FnDef) -> bool,
    G: FnMut(&[&FnDef]) -> Vec<String>,
{
    let mut by_scope: HashMap<String, Vec<String>> = HashMap::new();

    let mut process =
        |fns: Vec<&FnDef>, scope: String, by_scope: &mut HashMap<String, Vec<String>>| {
            let comps = crate::call_graph::ordered_fn_components(&fns, &ctx.module_prefixes);
            let bucket = by_scope.entry(scope).or_default();
            for comp in comps {
                bucket.extend(emit(&comp));
            }
        };

    for module in &ctx.modules {
        let pure: Vec<&FnDef> = module.fn_defs.iter().filter(|fd| is_pure(fd)).collect();
        process(pure, module.prefix.clone(), &mut by_scope);
    }
    let entry_pure: Vec<&FnDef> = ctx.fn_defs.iter().filter(|fd| is_pure(fd)).collect();
    process(entry_pure, String::new(), &mut by_scope);

    PerScopeSections { by_scope }
}
