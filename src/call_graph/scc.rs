use std::collections::{HashMap, HashSet};

pub(super) fn tarjan_sccs(
    nodes: &[String],
    graph: &HashMap<String, Vec<String>>,
) -> Vec<Vec<String>> {
    struct TarjanAllState {
        index: usize,
        indices: HashMap<String, usize>,
        lowlink: HashMap<String, usize>,
        stack: Vec<String>,
        on_stack: HashSet<String>,
        components: Vec<Vec<String>>,
    }

    fn strong_connect(v: String, graph: &HashMap<String, Vec<String>>, st: &mut TarjanAllState) {
        st.indices.insert(v.clone(), st.index);
        st.lowlink.insert(v.clone(), st.index);
        st.index += 1;
        st.stack.push(v.clone());
        st.on_stack.insert(v.clone());

        if let Some(neighbors) = graph.get(&v) {
            for w in neighbors {
                if !st.indices.contains_key(w) {
                    strong_connect(w.clone(), graph, st);
                    let low_v = st.lowlink[&v];
                    let low_w = st.lowlink[w];
                    st.lowlink.insert(v.clone(), low_v.min(low_w));
                } else if st.on_stack.contains(w) {
                    let low_v = st.lowlink[&v];
                    let idx_w = st.indices[w];
                    st.lowlink.insert(v.clone(), low_v.min(idx_w));
                }
            }
        }

        if st.lowlink[&v] == st.indices[&v] {
            let mut comp = Vec::new();
            while let Some(w) = st.stack.pop() {
                st.on_stack.remove(&w);
                let done = w == v;
                comp.push(w);
                if done {
                    break;
                }
            }
            comp.sort();
            st.components.push(comp);
        }
    }

    let mut sorted_nodes = nodes.to_vec();
    sorted_nodes.sort();
    let mut st = TarjanAllState {
        index: 0,
        indices: HashMap::new(),
        lowlink: HashMap::new(),
        stack: Vec::new(),
        on_stack: HashSet::new(),
        components: Vec::new(),
    };
    for node in sorted_nodes {
        if !st.indices.contains_key(&node) {
            strong_connect(node, graph, &mut st);
        }
    }
    st.components.sort_by(|a, b| a[0].cmp(&b[0]));
    st.components
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
