use std::collections::{HashMap, HashSet};

pub(super) fn tarjan_sccs(
    nodes: &[String],
    graph: &HashMap<String, Vec<String>>,
) -> Vec<Vec<String>> {
    // String-keyed call-graph SCCs (fn names) — the generic core lives in
    // `crate::scc`, shared with `analysis::shape`'s `FnId`-keyed recursion
    // classification.
    crate::scc::tarjan_sccs(nodes, graph)
}

pub(super) fn topo_components(
    sccs: &[Vec<String>],
    comp_graph: &HashMap<usize, HashSet<usize>>,
) -> Vec<usize> {
    let mut ids: Vec<usize> = (0..sccs.len()).collect();
    ids.sort_by(|a, b| sccs[*a][0].cmp(&sccs[*b][0]));

    let mut visited = HashSet::new();
    let mut order = Vec::new();
    for id in ids {
        if !visited.contains(&id) {
            topo_components_dfs(id, sccs, comp_graph, &mut visited, &mut order);
        }
    }
    order
}

fn topo_components_dfs(
    id: usize,
    sccs: &[Vec<String>],
    comp_graph: &HashMap<usize, HashSet<usize>>,
    visited: &mut HashSet<usize>,
    order: &mut Vec<usize>,
) {
    visited.insert(id);
    let mut neighbors: Vec<usize> = comp_graph
        .get(&id)
        .map(|s| s.iter().copied().collect())
        .unwrap_or_default();
    neighbors.sort_by(|a, b| sccs[*a][0].cmp(&sccs[*b][0]));
    for n in neighbors {
        if !visited.contains(&n) {
            topo_components_dfs(n, sccs, comp_graph, visited, order);
        }
    }
    order.push(id);
}
