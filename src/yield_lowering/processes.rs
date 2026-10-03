//! Which functions of a module are processes.
//!
//! Nothing marks a process. A function is one when it requests something its
//! program answers: its effect list names an operation of a capability that a
//! module of its own module's dependencies answers (`answers [Cap]` in that
//! module's header), or names `Run.turn`, the request that hands the turn back
//! to the generated loop. A function that calls a process is a process too.
//! The set is a fixpoint over the module as written.
//!
//! The answers counted are those of the module and of what it `depends` on,
//! transitively, never those of the whole linked program: a library's
//! interface is a function of its own text, so the same library compiled into
//! two programs exposes the same processes in both.
//!
//! `main` is never a process: it is where a program starts, and a process
//! runs under the generated loop or inside another process.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;

use crate::ast::{Expr, FnDef, Stmt, TopLevel};
use crate::codegen::expr_walk;
use crate::types::checker::TypeError;

use super::{ProcessProtocol, build};

/// The request that hands the turn back to the generated loop: the process
/// resumes in the next turn. The loop answers it itself.
pub const RUN_TURN: &str = "Run.turn";

/// Why a function is a process.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProcessReason {
    /// It requests an operation of a capability a module of its own
    /// dependencies answers.
    Requests {
        operation: String,
        answered_by: String,
    },
    /// It requests `Run.turn`.
    Turn,
    /// It calls another process, of its own module or exposed by a dependency.
    Calls { callee: String },
}

impl fmt::Display for ProcessReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProcessReason::Requests {
                operation,
                answered_by,
            } => write!(f, "requests {operation}, answered by {answered_by}"),
            ProcessReason::Turn => write!(f, "requests {RUN_TURN}"),
            ProcessReason::Calls { callee } => write!(f, "calls process {callee}"),
        }
    }
}

/// One process of a module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessInfo {
    pub name: String,
    pub line: usize,
    pub reason: ProcessReason,
}

/// The processes of one module, in source order.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Processes {
    list: Vec<ProcessInfo>,
}

impl Processes {
    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.list.iter().any(|info| info.name == name)
    }

    pub fn reason(&self, name: &str) -> Option<&ProcessReason> {
        self.list
            .iter()
            .find(|info| info.name == name)
            .map(|info| &info.reason)
    }

    pub fn names(&self) -> HashSet<String> {
        self.list.iter().map(|info| info.name.clone()).collect()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ProcessInfo> {
        self.list.iter()
    }
}

/// The capability an answer pair names, matched the way an effect entry is:
/// the header names it as the program writes it in `depends`, so a longer
/// spelling (`Infra.Pool`) matches on the suffix.
fn answering_module<'a>(namespace: &str, answers: &'a [(String, String)]) -> Option<&'a str> {
    answers.iter().find_map(|(capability, module)| {
        (namespace == capability || namespace.ends_with(&format!(".{capability}")))
            .then_some(module.as_str())
    })
}

/// What one effect entry or one called operation makes of a function: a
/// request answered by the program, `Run.turn`, or nothing.
fn request_reason(name: &str, answers: &[(String, String)]) -> Option<ProcessReason> {
    if name == RUN_TURN {
        return Some(ProcessReason::Turn);
    }
    let namespace = name
        .rsplit_once('.')
        .map_or(name, |(namespace, _)| namespace);
    // A namespace entry (`Pool`) admits every operation of it.
    let module = answering_module(namespace, answers)
        .or_else(|| (!name.contains('.')).then(|| answering_module(name, answers))?)?;
    Some(ProcessReason::Requests {
        operation: name.to_string(),
        answered_by: module.to_string(),
    })
}

/// Whether `items` can hold a process at all, read off the effect lists
/// alone: some function names `Run.turn`, or an operation of a capability the
/// compiler does not ship (only a program's own capability can be answered by
/// a module of it). A module this says no for is never lowered; one it says
/// yes for is derived exactly once its dependencies have been read.
pub fn may_have_processes(items: &[TopLevel]) -> bool {
    // A module whose processes were lowered already keeps them as sources.
    let lowered = items
        .iter()
        .any(|item| matches!(item, TopLevel::Module(module) if !module.yield_sources.is_empty()));
    !lowered
        && items.iter().any(|item| match item {
            TopLevel::FnDef(fd) if !fd.name.starts_with("__") && fd.name != "main" => {
                fd.effects.iter().any(|effect| {
                    let entry = effect.node.as_str();
                    if entry == RUN_TURN {
                        return true;
                    }
                    let namespace = entry.rsplit_once('.').map_or(entry, |(ns, _)| ns);
                    let last = namespace.rsplit('.').next().unwrap_or(namespace);
                    last != crate::effects::FORWARDED_CALLBACK_EFFECT
                        && !crate::stdlib::has_shipped_provider(last)
                })
            }
            _ => false,
        })
}

/// The calls a body makes by name: `f(...)`, `Mod.f(...)` and tail calls.
fn called_names(fd: &FnDef) -> Vec<(String, usize)> {
    let mut out = Vec::new();
    for stmt in fd.body.stmts() {
        let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
        expr_walk::walk(expr, &mut |e| match &e.node {
            Expr::FnCall(callee, _) => {
                if let Some(name) = build::dotted_name(callee) {
                    out.push((name, e.line));
                }
            }
            Expr::TailCall(call) => out.push((call.target.clone(), e.line)),
            _ => {}
        });
    }
    out
}

/// The processes of `items`. `answers` are the (capability, answering module)
/// pairs of the module's own dependency closure, itself included; `imported`
/// the processes its dependencies expose, by the name a call spells them with.
pub fn derive(
    items: &[TopLevel],
    answers: &[(String, String)],
    imported: &HashMap<String, ProcessProtocol>,
) -> Processes {
    let functions: Vec<&FnDef> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) if !fd.name.starts_with("__") && fd.name != "main" => Some(fd),
            _ => None,
        })
        .collect();
    let calls: Vec<Vec<(String, usize)>> = functions.iter().map(|fd| called_names(fd)).collect();
    let index_of = |name: &str| functions.iter().position(|fd| fd.name == name);
    // The set first: what an effect list requests, then whatever calls a
    // member, until nothing grows.
    let mut member: Vec<bool> = functions
        .iter()
        .map(|fd| {
            fd.effects
                .iter()
                .any(|effect| request_reason(&effect.node, answers).is_some())
        })
        .collect();
    loop {
        let mut grew = false;
        for index in 0..functions.len() {
            if member[index] {
                continue;
            }
            if calls[index].iter().any(|(name, _)| {
                name != &functions[index].name
                    && (index_of(name).is_some_and(|other| member[other])
                        || imported.contains_key(name))
            }) {
                member[index] = true;
                grew = true;
            }
        }
        if !grew {
            break;
        }
    }
    // Then the reason each one gives: a request its body makes itself, else
    // the process it calls, else what its effect list names.
    let mut found: BTreeMap<usize, ProcessReason> = BTreeMap::new();
    for (index, fd) in functions.iter().enumerate() {
        if !member[index] {
            continue;
        }
        let declared = |name: &str| {
            fd.effects
                .iter()
                .any(|effect| crate::effects::effect_satisfies(&effect.node, name))
        };
        let reason = calls[index]
            .iter()
            .find_map(|(name, _)| {
                declared(name)
                    .then(|| request_reason(name, answers))
                    .flatten()
            })
            .or_else(|| {
                calls[index].iter().find_map(|(name, _)| {
                    (name != &fd.name
                        && (index_of(name).is_some_and(|other| member[other])
                            || imported.contains_key(name)))
                    .then(|| ProcessReason::Calls {
                        callee: name.clone(),
                    })
                })
            })
            .or_else(|| {
                fd.effects
                    .iter()
                    .find_map(|effect| request_reason(&effect.node, answers))
            })
            .expect("a member requests something or calls a member");
        found.insert(index, reason);
    }
    Processes {
        list: found
            .into_iter()
            .map(|(index, reason)| ProcessInfo {
                name: functions[index].name.clone(),
                line: functions[index].line,
                reason,
            })
            .collect(),
    }
}

/// `main` naming `Run.turn`: there is no turn to hand back outside a process.
pub fn main_requests_turn(items: &[TopLevel]) -> Option<TypeError> {
    items.iter().find_map(|item| match item {
        TopLevel::FnDef(fd)
            if fd.name == "main" && fd.effects.iter().any(|e| e.node == RUN_TURN) =>
        {
            Some(super::error_at(
                fd.line,
                format!(
                    "'main' names {RUN_TURN}, which hands the turn back to the generated loop, but 'main' is not a process. Call {RUN_TURN}() from a process and run it with Run.all()"
                ),
            ))
        }
        _ => None,
    })
}

/// The answer functions of `items` that are processes: an answer is computed
/// inside the turn, so it cannot itself be a process the turn has to drive.
/// `capabilities` is what the module's check registered, which holds the
/// operations of every capability its header says it answers.
pub fn answers_that_are_processes(
    items: &[TopLevel],
    processes: &Processes,
    capabilities: &crate::capability::CapabilityRegistry,
) -> Vec<TypeError> {
    let Some(module) = crate::visibility::module_decl(items) else {
        return Vec::new();
    };
    let mut errors = Vec::new();
    for capability in &module.answers {
        for operation in capabilities
            .operations()
            .filter(|operation| &operation.module == capability)
        {
            let Some(info) = processes.iter().find(|info| info.name == operation.name) else {
                continue;
            };
            errors.push(super::error_at(
                info.line,
                format!(
                    "module '{}' answers capability '{capability}', and '{}.{}' is a process ({}); an answer is computed inside the turn, so it cannot itself be a process the turn has to drive",
                    module.name, module.name, info.name, info.reason
                ),
            ));
        }
    }
    errors
}

/// Why an imported process is one, as far as its protocol says: the first
/// operation it requests, with the module that answers it when that module is
/// one the importer can see.
pub fn imported_reason(protocol: &ProcessProtocol, answers: &[(String, String)]) -> String {
    protocol
        .kinds
        .iter()
        .find_map(|kind| kind.operation.as_deref())
        .and_then(|operation| request_reason(operation, answers))
        .map(|reason| reason.to_string())
        .or_else(|| {
            protocol
                .kinds
                .iter()
                .find_map(|kind| kind.operation.clone())
                .map(|operation| format!("requests {operation}"))
        })
        .unwrap_or_else(|| "calls a process".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> Vec<TopLevel> {
        crate::source::parse_source(source).expect("parses")
    }

    fn answers() -> Vec<(String, String)> {
        vec![("Pool".to_string(), "Pooled".to_string())]
    }

    #[test]
    fn requests_calls_and_turns_make_processes_through_helpers() {
        let items = parse(
            "fn claim() -> Int\n    ! [Pool.claim]\n    Pool.claim()\n\nfn walk() -> Int\n    ! [Pool.claim]\n    claim()\n\nfn pause() -> Unit\n    ! [Run.turn]\n    Run.turn()\n\nfn plain(x: Int) -> Int\n    x + 1\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(\"hi\")\n",
        );
        let processes = derive(&items, &answers(), &HashMap::new());
        let listed: Vec<String> = processes
            .iter()
            .map(|info| format!("{}: {}", info.name, info.reason))
            .collect();
        assert_eq!(
            listed,
            [
                "claim: requests Pool.claim, answered by Pooled",
                "walk: calls process claim",
                "pause: requests Run.turn",
            ]
        );
    }

    #[test]
    fn an_operation_no_dependency_answers_is_a_plain_call() {
        let items = parse("fn claim() -> Int\n    ! [Pool.claim]\n    Pool.claim()\n");
        assert!(derive(&items, &[], &HashMap::new()).is_empty());
        assert!(may_have_processes(&items));
    }

    #[test]
    fn a_shipped_capability_never_makes_a_candidate() {
        let items = parse("fn hello() -> Unit\n    ! [Console.print]\n    Console.print(\"hi\")\n");
        assert!(!may_have_processes(&items));
    }

    #[test]
    fn main_is_never_a_process() {
        let items = parse("fn main() -> Unit\n    ! [Run.turn]\n    Run.turn()\n");
        assert!(derive(&items, &[], &HashMap::new()).is_empty());
        assert!(main_requests_turn(&items).is_some());
    }
}
