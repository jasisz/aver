//! Coverage flowing along the call graph.
//!
//! A function that is not in its module's `exposes` list can only be reached
//! through the file's own entry points. When exactly one of them reaches it,
//! and that caller's own verify examples pin every arm of its return type, the
//! helper is exercised across both arms by those same vectors — so demanding a
//! separate `Result.Err` example of the helper asks for a claim the caller
//! already carries.
//!
//! The rule is deliberately narrow. Each additional caller widens the set of
//! error paths that no vector reaches, so it takes exactly one.

use std::collections::{BTreeSet, HashMap, HashSet};

use crate::ast::{FnDef, TopLevel};
use crate::call_graph::{call_components, callers_of};
use crate::visibility::{effective_exposes, is_exposed};

use super::coverage::return_shape_gaps;
use super::{merge_verify_blocks, verify_cases_block_is_well_formed};

/// Functions whose return-shape coverage is inherited from a caller.
///
/// Empty for a file with no `exposes` list: there every function without a
/// leading underscore counts as exposed, so nothing qualifies as private.
pub(super) fn flow_covered_fns(items: &[TopLevel]) -> HashSet<String> {
    let exposes = effective_exposes(items);
    let fn_defs: HashMap<&str, &FnDef> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(f) => Some((f.name.as_str(), f)),
            _ => None,
        })
        .collect();

    let mut covered: HashSet<String> = HashSet::new();
    for block in merge_verify_blocks(items) {
        // A verify-trace block routes its expected side through oracle stubs,
        // so its arms cannot be read syntactically and it proves nothing here.
        if !verify_cases_block_is_well_formed(&block) || block.trace {
            continue;
        }
        let Some(f) = fn_defs.get(block.fn_name.as_str()).copied() else {
            continue;
        };
        if return_shape_gaps(f, &block, items).is_some_and(|gaps| gaps.is_empty()) {
            covered.insert(block.fn_name.clone());
        }
    }

    let candidates: Vec<Vec<String>> = call_components(items)
        .into_iter()
        .filter(|component| {
            component
                .iter()
                .all(|name| !is_exposed(name, exposes) && !covered.contains(name))
        })
        .collect();

    let callers = callers_of(items);
    let mut flow_covered = HashSet::new();
    loop {
        let mut grew = false;
        for component in &candidates {
            if component.iter().any(|name| flow_covered.contains(name)) {
                continue;
            }
            let members: HashSet<&str> = component.iter().map(String::as_str).collect();
            let external: Vec<&str> = component
                .iter()
                .filter_map(|name| callers.get(name))
                .flatten()
                .map(String::as_str)
                .filter(|caller| !members.contains(caller))
                .collect::<BTreeSet<&str>>()
                .into_iter()
                .collect();
            // Imputing a whole component at once, from a caller outside it,
            // is what keeps mutual recursion from bootstrapping its own
            // coverage: no member can ever be the source for another.
            let [caller] = external[..] else {
                continue;
            };
            if covered.contains(caller) || flow_covered.contains(caller) {
                flow_covered.extend(component.iter().cloned());
                grew = true;
            }
        }
        if !grew {
            return flow_covered;
        }
    }
}
