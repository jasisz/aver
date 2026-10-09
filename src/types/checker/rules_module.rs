//! Rules modules: `rules [f, …]` in a module header marks a module of proof
//! rules, functions a law names with `by Module.f`. A rule receives the law
//! to close as a `Kernel.Proof.Goal` and returns proof steps or a refusal;
//! the kernel checks every step it returns, so nothing a rule does is
//! trusted. What the checker enforces here is the shape of the boundary:
//! a rule has exactly the rule signature, a rules module has no effects,
//! and no module outside the rules world depends on one, so rules never
//! become part of a program.

use std::collections::HashMap;

use crate::ast::{Module, TopLevel};
use crate::ir::{TypeId, TypeKey};
use crate::types::Type;

use super::TypeError;

/// The module the public kernel types live in.
const KERNEL_PROOF: &str = "Kernel.Proof";

fn kernel_type(name: &str) -> Type {
    Type::Named {
        id: Some(TypeId::for_key(&TypeKey::in_module(KERNEL_PROOF, name))),
        name: format!("{KERNEL_PROOF}.{name}"),
    }
}

/// Whether two types are the same, a named type by its identity alone (the
/// checker keeps the name as written, `Goal` or `Kernel.Proof.Goal`).
fn same_type(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Named { id: Some(x), .. }, Type::Named { id: Some(y), .. }) => x == y,
        (Type::Result(a1, a2), Type::Result(b1, b2)) => same_type(a1, b1) && same_type(a2, b2),
        (Type::Named { .. }, _) | (_, Type::Named { .. }) => false,
        _ => a == b,
    }
}

fn error(message: String, line: usize) -> TypeError {
    TypeError {
        message,
        line,
        col: 1,
        origin: None,
        secondary: None,
    }
}

/// Whether a module header declares a rules module.
pub(crate) fn is_rules_module(module: &Module) -> bool {
    module.rules.is_some()
}

/// Check the checked module, when it is a rules module: every listed rule
/// exists with the rule signature, and nothing in the module has effects.
pub(super) fn check_rules_module(
    items: &[TopLevel],
    fn_sigs: &HashMap<String, (Vec<Type>, Type, Vec<String>)>,
    errors: &mut Vec<TypeError>,
) {
    let Some(module) = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m),
        _ => None,
    }) else {
        return;
    };
    let Some(rules) = &module.rules else {
        return;
    };
    let header_line = module.rules_line.unwrap_or(module.line);
    if let Some(effects) = &module.effects
        && !effects.is_empty()
    {
        errors.push(error(
            format!(
                "rules module '{}' declares effects [{}]: a proof rule is pure, so a rules module has no effects",
                module.name,
                effects.join(", ")
            ),
            module.effects_line.unwrap_or(module.line),
        ));
    }
    for item in items {
        let TopLevel::FnDef(fd) = item else { continue };
        if let Some(first) = fd.effects.first() {
            errors.push(error(
                format!(
                    "'{}' in rules module '{}' uses effect '{}': a proof rule is pure, so a rules module has no effects",
                    fd.name, module.name, first.node
                ),
                first.line,
            ));
        }
    }
    let goal = kernel_type("Goal");
    let proof = kernel_type("Proof");
    let wanted = Type::Result(Box::new(proof), Box::new(Type::Str));
    for rule in rules {
        let Some(fd) = items.iter().find_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name == *rule => Some(fd),
            _ => None,
        }) else {
            errors.push(error(
                format!(
                    "rules module '{}' lists rule '{rule}', but it defines no function of that name",
                    module.name
                ),
                header_line,
            ));
            continue;
        };
        let signature_ok = fn_sigs.get(rule).is_some_and(|(params, ret, _)| {
            params.len() == 1 && same_type(&params[0], &goal) && same_type(ret, &wanted)
        });
        if !signature_ok {
            errors.push(error(
                format!(
                    "rule '{rule}' must have the rule signature `fn {rule}(goal: Goal) -> Result<Proof, String>`, with Goal and Proof from Kernel.Proof"
                ),
                fd.line,
            ));
        }
    }
}

/// Refuse every dependency edge from a module that is not a rules module to
/// one that is: rules are never part of a program.
pub(super) fn check_rules_dependencies(
    items: &[TopLevel],
    modules: &[crate::source::LoadedModule],
    errors: &mut Vec<TypeError>,
) {
    let rules_modules: Vec<&str> = modules
        .iter()
        .filter(|m| {
            m.items
                .iter()
                .any(|i| matches!(i, TopLevel::Module(decl) if is_rules_module(decl)))
        })
        .map(|m| m.dep_name.as_str())
        .collect();
    if rules_modules.is_empty() {
        return;
    }
    let decls = items
        .iter()
        .chain(modules.iter().flat_map(|m| m.items.iter()))
        .filter_map(|i| match i {
            TopLevel::Module(decl) => Some(decl),
            _ => None,
        });
    let entry_name = items.iter().find_map(|i| match i {
        TopLevel::Module(decl) => Some(decl.name.as_str()),
        _ => None,
    });
    for decl in decls {
        if is_rules_module(decl) {
            continue;
        }
        for dep in &decl.depends {
            if rules_modules.contains(&dep.as_str()) {
                let line = if Some(decl.name.as_str()) == entry_name {
                    decl.line
                } else {
                    0
                };
                errors.push(error(
                    format!(
                        "module '{}' depends on rules module '{dep}': proof rules are not part of a program, so only another rules module may depend on one",
                        decl.name
                    ),
                    line,
                ));
            }
        }
    }
}

/// Why a law's `by Module.rule` names no rule of the project at
/// `module_root`, if it names none: the module must be a project file (not
/// one the compiler ships), a rules module, and list the rule.
pub fn by_line_refusal(path: &str, module_root: &str) -> Option<String> {
    let Some((module, rule)) = path
        .rsplit_once('.')
        .filter(|(m, f)| !m.is_empty() && !f.is_empty())
    else {
        return Some(format!("`by {path}` must name a rule as Module.rule"));
    };
    if crate::source::is_kernel_module(module) {
        return Some(format!(
            "`by {path}`: {module} is a module of the proof kernel, not a rules module of this project"
        ));
    }
    let Some(file) = crate::source::find_module_file(module, module_root) else {
        return Some(format!(
            "`by {path}`: this project has no module {module}; a rule comes from a rules module of the same project"
        ));
    };
    let text = std::fs::read_to_string(&file).ok()?;
    let items = crate::source::parse_source(&text).ok()?;
    let decl = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m),
        _ => None,
    })?;
    match &decl.rules {
        None => Some(format!(
            "`by {path}`: module {module} is not a rules module (it has no `rules [...]` line)"
        )),
        Some(rules) if !rules.iter().any(|r| r == rule) => Some(format!(
            "`by {path}`: rules module {module} does not list `{rule}` in its `rules [...]` line"
        )),
        Some(_) => None,
    }
}

/// An error for every law whose `by` line names no rule of the project.
pub fn check_by_lines(items: &[TopLevel], module_root: &str) -> Vec<TypeError> {
    let mut out = Vec::new();
    for item in items {
        let TopLevel::Verify(block) = item else {
            continue;
        };
        let crate::ast::VerifyKind::Law(law) = &block.kind else {
            continue;
        };
        if let Some(path) = &law.by_rule
            && let Some(why) = by_line_refusal(path, module_root)
        {
            out.push(error(why, block.line));
        }
    }
    out
}
