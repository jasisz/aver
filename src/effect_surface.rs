//! The effect surface of a program: what each function declares, what the
//! checker computes it needs, and the difference between the two.
//!
//! Effect declaration is cheap; effect *propagation* is what costs a consumer
//! lines. Changing which primitive a leaf calls rewrites the declared list of
//! every function on every call chain above it, and those bodies do not
//! change. The minimum each function needs is already computed — it is the
//! set behind the `unused-effect` warning — so this module iterates that same
//! computation to a fixpoint over the whole module graph and reports the
//! difference per function and per module boundary.
//!
//! Two properties matter to the caller:
//!
//! - The fixpoint starts from the empty set and grows. Starting from the
//!   declared lists would make a recursion group a fixpoint of itself: two
//!   functions that call each other keep each other's surplus entries alive
//!   forever. Growing from below gives the least solution, which is the
//!   minimum.
//! - The author's granularity survives. A declared namespace entry such as
//!   `Disk` stays as written when the computed set is covered by it; narrowing
//!   it to `Disk.readText` is a judgement about the contract, not a mechanical
//!   step, and `effect-granularity` already says so as a warning.
//!
//! Markers that are not computed effects — `yield`, which changes lowering,
//! and the forwarded-callback marker `_` — are copied through verbatim. They
//! are never invented and never removed.

use std::collections::{BTreeSet, HashMap};

use crate::ast::{FnDef, Module, TopLevel};
use crate::effects::effect_satisfies;

/// The signature map `aver check` hands the lints, keyed the way the source
/// of that module names its callees.
pub type SigMap = HashMap<String, (Vec<crate::types::Type>, crate::types::Type, Vec<String>)>;

/// True for an entry that is written in an effect list but is not an effect
/// the checker computes.
///
/// `yield` is a declaration that changes how the function is lowered, and `_`
/// is a callback parameter-position marker. Neither is derived from a body, so
/// neither is invented or dropped by a rewrite.
pub fn is_preserved_marker(entry: &str) -> bool {
    entry == crate::yield_lowering::YIELD_EFFECT
        || entry == crate::effects::FORWARDED_CALLBACK_EFFECT
}

/// A signature map with everything the computation does not read thrown away.
///
/// The computation reads a parameter only to ask whether it is a callback slot
/// marked `! [_]`, and never reads a return type. Every module of a program
/// carries the signatures of every module it reaches, so a program of a
/// hundred and fifty modules would otherwise hold that many copies of the same
/// types at once; on a real consumer that is the difference between the
/// command running and the machine killing it.
pub fn compact(mut fn_sigs: SigMap) -> SigMap {
    for (params, ret, _) in fn_sigs.values_mut() {
        *ret = crate::types::Type::Unit;
        for param in params.iter_mut() {
            *param = match param {
                crate::types::Type::Fn(_, _, effects) => crate::types::Type::Fn(
                    Vec::new(),
                    Box::new(crate::types::Type::Unit),
                    std::mem::take(effects),
                ),
                _ => crate::types::Type::Unit,
            };
        }
    }
    fn_sigs
}

/// One module of the program, as the surface computation reads it.
pub struct SurfaceInput {
    /// Path of the file this module was read from.
    pub path: String,
    /// Every dotted name this module is reached by. A directory input walks
    /// several programs, and the same file is the entry of one and a
    /// dependency of another, where it carries a different name.
    pub import_names: Vec<String>,
    /// The module's source items, as written.
    pub items: Vec<TopLevel>,
    /// The signatures that module's own typecheck produced.
    pub fn_sigs: SigMap,
}

/// One function's declared list against its computed minimum.
#[derive(Debug, Clone)]
pub struct FnSurface {
    pub name: String,
    pub line: usize,
    /// The list as written, in source order.
    pub declared: Vec<String>,
    /// The computed minimum, sorted; preserved markers are not in it.
    pub minimum: Vec<String>,
    /// The list a rewrite would declare, sorted.
    pub resolved: Vec<String>,
    /// Computed entries the declared list does not cover.
    pub missing: Vec<String>,
    /// Declared entries that cover nothing computed.
    pub unused: Vec<String>,
}

impl FnSurface {
    pub fn differs(&self) -> bool {
        !self.missing.is_empty() || !self.unused.is_empty()
    }
}

/// The module's `effects [...]` boundary against the union of its functions.
#[derive(Debug, Clone)]
pub struct BoundarySurface {
    /// `None` when the module declares no boundary at all. A rewrite does not
    /// invent one: `check` already nudges for that, and choosing to declare a
    /// boundary is the author's.
    pub declared: Option<Vec<String>>,
    pub line: Option<usize>,
    /// The union of what this module's functions end up declaring, sorted.
    pub minimum: Vec<String>,
    pub resolved: Vec<String>,
    pub missing: Vec<String>,
    pub unused: Vec<String>,
}

impl BoundarySurface {
    pub fn differs(&self) -> bool {
        self.declared.is_some() && (!self.missing.is_empty() || !self.unused.is_empty())
    }
}

#[derive(Debug, Clone)]
pub struct ModuleSurface {
    /// The name written in the `module` line.
    pub module: String,
    /// The dotted names the program reaches it by, sorted.
    pub import_names: Vec<String>,
    pub path: String,
    pub functions: Vec<FnSurface>,
    pub boundary: BoundarySurface,
}

impl ModuleSurface {
    pub fn differing_functions(&self) -> impl Iterator<Item = &FnSurface> {
        self.functions.iter().filter(|f| f.differs())
    }

    pub fn differs(&self) -> bool {
        self.boundary.differs() || self.functions.iter().any(FnSurface::differs)
    }
}

#[derive(Debug, Clone)]
pub struct ProgramSurface {
    /// Modules in `module` name order, so every rendering of the same program
    /// is the same bytes.
    pub modules: Vec<ModuleSurface>,
    /// Rounds the fixpoint took. Reported so a program that hits the cap is
    /// visible rather than silently truncated.
    pub rounds: usize,
    /// True when the fixpoint stopped at the round cap instead of settling.
    pub capped: bool,
}

impl ProgramSurface {
    pub fn differs(&self) -> bool {
        self.modules.iter().any(ModuleSurface::differs)
    }
}

/// What the fixpoint knows about one function.
struct Node {
    declared: Vec<String>,
    minimum: BTreeSet<String>,
    resolved: Vec<String>,
}

fn module_decl(items: &[TopLevel]) -> Option<&Module> {
    items.iter().find_map(|item| match item {
        TopLevel::Module(module) => Some(module),
        _ => None,
    })
}

fn fn_defs(items: &[TopLevel]) -> impl Iterator<Item = &FnDef> {
    items.iter().filter_map(|item| match item {
        TopLevel::FnDef(fd) => Some(fd),
        _ => None,
    })
}

/// A function's computed set: everything its body reaches, without the
/// markers that are declarations rather than effects.
///
/// This is the one place a function's `yield` is dropped, and it is why
/// [`resolve`] can never write a `yield` into a function that did not have
/// one. The module boundary is resolved against the union of the functions'
/// resolved lists instead, markers included, because
/// `types::checker::check_module_effect_boundary` requires the boundary to
/// carry every entry its functions declare, `yield` among them.
fn function_minimum(used: BTreeSet<String>) -> BTreeSet<String> {
    used.into_iter()
        .filter(|effect| !is_preserved_marker(effect))
        .collect()
}

/// The list a rewrite would declare: the preserved markers as written, plus
/// one entry per computed effect, spelled at the granularity the author
/// already chose for it.
fn resolve(declared: &[String], minimum: &BTreeSet<String>) -> Vec<String> {
    let mut out: BTreeSet<String> = declared
        .iter()
        .filter(|entry| is_preserved_marker(entry))
        .cloned()
        .collect();

    for computed in minimum {
        if declared.iter().any(|entry| entry == computed) {
            out.insert(computed.clone());
            continue;
        }
        // A namespace the author already wrote down covers this method, so
        // keep their spelling rather than narrowing the contract for them.
        match declared
            .iter()
            .find(|entry| !entry.contains('.') && effect_satisfies(entry, computed))
        {
            Some(namespace) => out.insert(namespace.clone()),
            None => out.insert(computed.clone()),
        };
    }

    // `Disk` admits every `Disk.*`, so a method entry beside its own
    // namespace is noise. Dropping it keeps the result stable under a second
    // pass over the rewritten list.
    let namespaces: BTreeSet<String> = out
        .iter()
        .filter(|entry| !entry.contains('.'))
        .cloned()
        .collect();
    out.retain(|entry| match entry.split_once('.') {
        Some((namespace, _)) => !namespaces.contains(namespace),
        None => true,
    });

    out.into_iter().collect()
}

fn missing_entries(declared: &[String], minimum: &BTreeSet<String>) -> Vec<String> {
    minimum
        .iter()
        .filter(|computed| {
            !declared
                .iter()
                .any(|entry| effect_satisfies(entry, computed))
        })
        .cloned()
        .collect()
}

fn unused_entries(declared: &[String], minimum: &BTreeSet<String>) -> Vec<String> {
    let mut out: BTreeSet<String> = BTreeSet::new();
    for entry in declared {
        if is_preserved_marker(entry) {
            continue;
        }
        if minimum
            .iter()
            .any(|computed| effect_satisfies(entry, computed))
        {
            continue;
        }
        out.insert(entry.clone());
    }
    out.into_iter().collect()
}

/// Effect lists of a module that a rewrite must leave alone.
///
/// A `yield` function is removed from the module by lowering and kept as
/// proof metadata; its declared list belongs to that lowering, not to this
/// computation.
fn is_rewritable(fd: &FnDef) -> bool {
    !crate::yield_lowering::is_yield_fn(fd)
}

/// The effect surface of a whole program.
///
/// Every unit is one module of the loaded program, carrying the signatures its
/// own typecheck produced. The computation does not need the program to
/// typecheck cleanly: a primitive swap that has not been propagated yet is
/// exactly the state this command exists for, and the effect violations it
/// raises leave the signature map intact.
pub fn compute(mut units: Vec<SurfaceInput>) -> ProgramSurface {
    // One node per function of every unit, addressed both the way that unit's
    // own typecheck names it (`Store.save`, or the bare `save`) and the way an
    // importer names it (`Infra.Store.save`). The two spellings differ because
    // the entry module's prefix is its `module` line while a dependency's is
    // the name it was imported under.
    let mut nodes: Vec<Node> = Vec::new();
    let mut by_import_name: HashMap<String, usize> = HashMap::new();
    let mut ambiguous: BTreeSet<String> = BTreeSet::new();
    let mut local_names: Vec<HashMap<String, usize>> = Vec::new();

    for unit in units.iter() {
        let module_name = module_decl(&unit.items).map(|m| m.name.clone());
        let mut local: HashMap<String, usize> = HashMap::new();
        for fd in fn_defs(&unit.items) {
            let declared: Vec<String> = fd.effects.iter().map(|e| e.node.clone()).collect();
            let index = nodes.len();
            nodes.push(Node {
                resolved: declared.clone(),
                declared,
                minimum: BTreeSet::new(),
            });
            for reached_by in &unit.import_names {
                let import_name = format!("{}.{}", reached_by, fd.name);
                // Two programs can name an entry module the same way. A name
                // that stands for two functions stands for neither.
                match by_import_name.insert(import_name.clone(), index) {
                    Some(previous) if previous != index => ambiguous.insert(import_name),
                    _ => false,
                };
            }
            local.insert(fd.name.clone(), index);
            if let Some(name) = &module_name {
                local.insert(format!("{}.{}", name, fd.name), index);
            }
        }
        local_names.push(local);
    }
    for name in &ambiguous {
        by_import_name.remove(name);
    }

    // Which signature-map entries of each unit stand for a function of this
    // program. Everything else — capability operations, standard-library
    // namespaces — keeps the effects the typecheck gave it.
    let mut patch_targets: Vec<Vec<(String, usize)>> = Vec::with_capacity(units.len());
    for (unit_index, unit) in units.iter().enumerate() {
        let mut targets = Vec::new();
        for key in unit.fn_sigs.keys() {
            let node = local_names[unit_index]
                .get(key)
                .or_else(|| by_import_name.get(key));
            if let Some(index) = node {
                targets.push((key.clone(), *index));
            }
        }
        targets.sort();
        patch_targets.push(targets);
    }

    // Seed every function at the empty set and grow. The cap is one round per
    // function plus one: the longest a chain of strictly growing sets can run
    // before it has to repeat.
    for node in nodes.iter_mut() {
        node.minimum.clear();
        node.resolved = resolve(&node.declared, &node.minimum);
    }

    let cap = nodes.len() + 2;
    let mut rounds = 0usize;
    let mut capped = true;
    while rounds < cap {
        rounds += 1;
        let mut changed = false;

        // Every call site now reads the callee's current list rather than the
        // one its author wrote, which is what carries a leaf's change upward.
        for (unit_index, unit) in units.iter_mut().enumerate() {
            for (key, node_index) in &patch_targets[unit_index] {
                if let Some(sig) = unit.fn_sigs.get_mut(key) {
                    sig.2.clone_from(&nodes[*node_index].resolved);
                }
            }
        }

        // Nodes were pushed in the order `fn_defs` yields them, so walking the
        // units the same way pairs each function with its own node.
        let mut node_cursor = 0usize;
        for unit in units.iter() {
            for fd in fn_defs(&unit.items) {
                let index = node_cursor;
                node_cursor += 1;
                let minimum =
                    function_minimum(crate::checker::collect_used_effects(fd, &unit.fn_sigs));
                if minimum != nodes[index].minimum {
                    nodes[index].minimum = minimum;
                    nodes[index].resolved = resolve(&nodes[index].declared, &nodes[index].minimum);
                    changed = true;
                }
            }
        }

        if !changed {
            capped = false;
            break;
        }
    }

    let mut modules: Vec<ModuleSurface> = Vec::new();
    let mut node_cursor = 0usize;
    for unit in units.iter() {
        let module = module_decl(&unit.items);
        let module_name = module.map(|m| m.name.clone()).unwrap_or_else(|| {
            unit.import_names
                .first()
                .cloned()
                .unwrap_or_else(|| unit.path.clone())
        });
        let mut functions = Vec::new();
        let mut union: BTreeSet<String> = BTreeSet::new();
        for fd in fn_defs(&unit.items) {
            let index = node_cursor;
            node_cursor += 1;
            let node = &nodes[index];
            for entry in &node.resolved {
                union.insert(entry.clone());
            }
            if !is_rewritable(fd) {
                // A yielding function's list is lowering's business; report it
                // as written and say nothing about it.
                functions.push(FnSurface {
                    name: fd.name.clone(),
                    line: fd.line,
                    declared: node.declared.clone(),
                    minimum: node.declared.clone(),
                    resolved: node.declared.clone(),
                    missing: Vec::new(),
                    unused: Vec::new(),
                });
                continue;
            }
            functions.push(FnSurface {
                name: fd.name.clone(),
                line: fd.line,
                declared: node.declared.clone(),
                minimum: node.minimum.iter().cloned().collect(),
                resolved: node.resolved.clone(),
                missing: missing_entries(&node.declared, &node.minimum),
                unused: unused_entries(&node.declared, &node.minimum),
            });
        }

        let declared_boundary = module.and_then(|m| m.effects.clone());
        let boundary = match &declared_boundary {
            Some(declared) => BoundarySurface {
                declared: Some(declared.clone()),
                line: module.and_then(|m| m.effects_line),
                minimum: union.iter().cloned().collect(),
                resolved: resolve(declared, &union),
                missing: missing_entries(declared, &union),
                unused: unused_entries(declared, &union),
            },
            None => BoundarySurface {
                declared: None,
                line: None,
                minimum: union.iter().cloned().collect(),
                resolved: Vec::new(),
                missing: Vec::new(),
                unused: Vec::new(),
            },
        };

        let mut import_names = unit.import_names.clone();
        import_names.sort();
        import_names.dedup();
        modules.push(ModuleSurface {
            module: module_name,
            import_names,
            path: unit.path.clone(),
            functions,
            boundary,
        });
    }

    modules.sort_by(|a, b| (&a.module, &a.path).cmp(&(&b.module, &b.path)));

    ProgramSurface {
        modules,
        rounds,
        capped,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn set(entries: &[&str]) -> BTreeSet<String> {
        entries.iter().map(|e| e.to_string()).collect()
    }

    fn list(entries: &[&str]) -> Vec<String> {
        entries.iter().map(|e| e.to_string()).collect()
    }

    #[test]
    fn resolve_drops_what_the_body_does_not_reach() {
        assert_eq!(
            resolve(
                &list(&["Console.print", "Disk.appendText", "Time.unixMs"]),
                &set(&["Disk.appendText"])
            ),
            list(&["Disk.appendText"])
        );
    }

    #[test]
    fn resolve_adds_what_propagated_up() {
        assert_eq!(
            resolve(
                &list(&["Disk.appendText"]),
                &set(&["Disk.appendBytes", "Disk.sync"])
            ),
            list(&["Disk.appendBytes", "Disk.sync"])
        );
    }

    #[test]
    fn resolve_keeps_a_namespace_the_author_wrote() {
        assert_eq!(
            resolve(&list(&["Disk"]), &set(&["Disk.readText", "Disk.sync"])),
            list(&["Disk"])
        );
    }

    #[test]
    fn resolve_keeps_a_yield_the_author_wrote() {
        assert_eq!(
            resolve(&list(&["yield", "Tcp.readNow"]), &set(&["Tcp.readNow"])),
            list(&["Tcp.readNow", "yield"])
        );
    }

    #[test]
    fn a_function_minimum_carries_no_yield_so_no_rewrite_can_invent_one() {
        let reachable = set(&["Tcp.readNow", "yield"]);
        assert_eq!(
            resolve(&list(&["Tcp.readNow"]), &function_minimum(reachable)),
            list(&["Tcp.readNow"])
        );
    }

    #[test]
    fn a_module_boundary_carries_the_yield_its_functions_declare() {
        // The boundary is resolved against the functions' resolved lists, not
        // against their minima, because the boundary check demands every entry
        // a function declares.
        assert_eq!(
            resolve(&list(&["Tcp.readNow"]), &set(&["Tcp.readNow", "yield"])),
            list(&["Tcp.readNow", "yield"])
        );
    }

    #[test]
    fn resolve_is_idempotent() {
        let minimum = set(&["Disk.readText", "Disk.sync", "Time.now"]);
        let once = resolve(&list(&["Disk", "Console.print"]), &minimum);
        let twice = resolve(&once, &minimum);
        assert_eq!(once, twice);
    }

    #[test]
    fn missing_and_unused_read_the_declared_granularity() {
        let declared = list(&["Disk", "Console.print"]);
        let minimum = set(&["Disk.readText", "Time.now"]);
        assert_eq!(missing_entries(&declared, &minimum), list(&["Time.now"]));
        assert_eq!(
            unused_entries(&declared, &minimum),
            list(&["Console.print"])
        );
    }
}
