//! Backend-agnostic plumbing for multi-file proof export.
//!
//! Multi-module Aver projects (sources with `depends [...]`) emit one file
//! per dependent module, plus a shared common file (helpers + records, a
//! union of every body's actual usage), plus an entry file holding the
//! trust header and verify lemmas. Lean and Dafny — and, in time, Rust —
//! share the orchestration: which fns end up in which scope, how SCC
//! components route per module, how files are packaged. Each backend
//! supplies its own *content* (namespace wrapping, module syntax,
//! per-fn rendering); this module supplies the *flow*.
//!
//! Cross-module mutual recursion (a SCC straddling modules) is currently
//! out of scope — global SCC analysis would conflate same-bare-name fns
//! from different modules anyway. Per-scope SCC keeps the DAG case
//! correct; the cross-module case will get a flat fallback in 0.13.

use std::collections::{BTreeMap, HashMap};

use crate::ast::FnDef;
use crate::codegen::{CodegenContext, ProjectOutput};

/// A node in the module tree. `content` carries the rendered body for
/// modules that exist in the source (and the entry root, when a backend
/// places content there); `children` carries nested submodules. A node
/// can be intermediate (`content == None`) when a deep submodule like
/// `Apps.Notepad.Store` exists without `Apps` or `Apps.Notepad` being
/// declared as their own modules.
#[derive(Default)]
pub(crate) struct ModuleTreeNode {
    pub content: Option<String>,
    pub children: BTreeMap<String, ModuleTreeNode>,
}

/// Module tree built from `depends [...]` paths. Lean and Dafny walk it
/// to flat files (one file per leaf with content, dotted name → `/`-joined
/// path). Rust walks it to a cascading `mod.rs` hierarchy where every
/// node — including intermediates — gets a `mod.rs` declaring `pub mod`
/// for each child.
#[derive(Default)]
pub(crate) struct ModuleTree {
    pub root: ModuleTreeNode,
}

impl ModuleTree {
    /// Insert content at the path described by `segments`. Callers must
    /// pre-transform segments to whatever case/escape the backend's tree
    /// keys use (identity for Lean/Dafny, snake_case for Rust). Empty
    /// `segments` writes the content into the root node.
    pub(crate) fn insert(&mut self, segments: &[String], content: String) {
        Self::insert_at(&mut self.root, segments, content);
    }

    fn insert_at(node: &mut ModuleTreeNode, segments: &[String], content: String) {
        if segments.is_empty() {
            node.content = Some(content);
            return;
        }
        let key = segments[0].clone();
        let child = node.children.entry(key).or_default();
        if segments.len() == 1 {
            child.content = Some(content);
        } else {
            Self::insert_at(child, &segments[1..], content);
        }
    }
}

/// Per-backend tree walker. The default walk visits every node in
/// pre-order, calls `render_node`, and recurses; backends override
/// when they need a different shape (Rust's mod.rs cascade vs the
/// flat-leaves model).
pub(crate) trait ModuleTreeRenderer {
    /// Convert one node into zero or more output files.
    /// `path_segments` is the in-tree path from root to this node
    /// (already transformed by the same `transform` closure used at
    /// insert time). `child_names` is the alphabetical list of
    /// immediate children — Rust uses it to emit `pub mod` lines.
    fn render_node(
        &self,
        path_segments: &[String],
        content: Option<&str>,
        child_names: &[String],
    ) -> Vec<(String, String)>;
}

pub(crate) fn walk_module_tree<R: ModuleTreeRenderer>(
    tree: &ModuleTree,
    renderer: &R,
) -> Vec<(String, String)> {
    let mut files = Vec::new();
    walk_node(&tree.root, &[], renderer, &mut files);
    files
}

fn walk_node<R: ModuleTreeRenderer>(
    node: &ModuleTreeNode,
    path: &[String],
    renderer: &R,
    files: &mut Vec<(String, String)>,
) {
    let child_names: Vec<String> = node.children.keys().cloned().collect();
    files.extend(renderer.render_node(path, node.content.as_deref(), &child_names));
    for (name, child) in &node.children {
        let mut next_path = path.to_vec();
        next_path.push(name.clone());
        walk_node(child, &next_path, renderer, files);
    }
}

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

/// Package the four file groups (common + entry + per-module + extras)
/// into a `ProjectOutput`. `module_files` is `Vec<(prefix, content)>`;
/// the path is derived from the prefix via `module_prefix_to_filename`.
pub(crate) fn package_multi_file_output(
    file_ext: &str,
    common_basename: &str,
    common_content: String,
    entry_basename: &str,
    entry_content: String,
    module_files: Vec<(String, String)>,
    extra_files: Vec<(String, String)>,
) -> ProjectOutput {
    let mut files: Vec<(String, String)> = Vec::new();
    for (prefix, content) in module_files {
        let path = format!(
            "{}.{}",
            crate::codegen::common::module_prefix_to_filename(&prefix),
            file_ext
        );
        files.push((path, content));
    }
    files.push((format!("{}.{}", entry_basename, file_ext), entry_content));
    files.push((format!("{}.{}", common_basename, file_ext), common_content));
    files.extend(extra_files);
    ProjectOutput { files }
}
