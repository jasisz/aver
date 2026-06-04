//! Generic strongly-connected-components (Tarjan).
//!
//! One algorithm, two callers: `call_graph::scc` (string-keyed fn names, for
//! TCO grouping + topo order) and `analysis::shape::compute_sccs` (`FnId`-keyed,
//! for recursion classification feeding the proof path) both wrap this. Before
//! unification each maintained its own copy of the `strong_connect` DFS.
//!
//! Deterministic: vertices are sorted, each component is sorted, and the
//! components are ordered by their least member, so two runs over the same
//! input agree. A vertex with no self-edge is its own 1-element component;
//! mutual recursion shows up as a component with `len() > 1`.

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Tarjan's SCC partition of `graph` over the given `nodes`. Out-edges to
/// vertices absent from `graph` are treated as having no further edges.
pub fn tarjan_sccs<K>(nodes: &[K], graph: &HashMap<K, Vec<K>>) -> Vec<Vec<K>>
where
    K: Clone + Eq + Hash + Ord,
{
    struct State<K> {
        index: usize,
        indices: HashMap<K, usize>,
        lowlink: HashMap<K, usize>,
        stack: Vec<K>,
        on_stack: HashSet<K>,
        components: Vec<Vec<K>>,
    }

    fn strong_connect<K>(v: K, graph: &HashMap<K, Vec<K>>, st: &mut State<K>)
    where
        K: Clone + Eq + Hash + Ord,
    {
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
    let mut st = State {
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

/// The set of vertices that participate in mutual recursion — i.e. live in a
/// strongly-connected component of size > 1. (Self-recursion via a self-edge
/// is intentionally NOT captured here; callers that exclude self-edges from
/// `graph` get exactly the mutual-recursion set.)
pub fn mutually_recursive<K>(nodes: &[K], graph: &HashMap<K, Vec<K>>) -> HashSet<K>
where
    K: Clone + Eq + Hash + Ord,
{
    tarjan_sccs(nodes, graph)
        .into_iter()
        .filter(|c| c.len() > 1)
        .flatten()
        .collect()
}
