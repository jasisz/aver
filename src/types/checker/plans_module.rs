//! Plans modules: `plans [f, …]` in a module header marks a module of proof
//! plans, functions a law names with `by Module.f`. A plan receives the law
//! to close as a `Kernel.Proof.Goal` and returns proof steps or a refusal;
//! the kernel checks every step it returns, so nothing a plan does is
//! trusted. What the checker enforces here is the shape of the boundary:
//! a plan has exactly the plan signature, a plans module has no effects,
//! and no module outside the plans world depends on one, so plans never
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

/// Whether a module header declares a plans module.
pub(crate) fn is_plans_module(module: &Module) -> bool {
    module.plans.is_some()
}

/// Check the checked module, when it is a plans module: every listed plan
/// exists with the plan signature, and nothing in the module has effects.
pub(super) fn check_plans_module(
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
    let Some(plans) = &module.plans else {
        return;
    };
    let header_line = module.plans_line.unwrap_or(module.line);
    if let Some(effects) = &module.effects
        && !effects.is_empty()
    {
        errors.push(error(
            format!(
                "plans module '{}' declares effects [{}]: a proof plan is pure, so a plans module has no effects",
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
                    "'{}' in plans module '{}' uses effect '{}': a proof plan is pure, so a plans module has no effects",
                    fd.name, module.name, first.node
                ),
                first.line,
            ));
        }
    }
    let goal = kernel_type("Goal");
    let proof = kernel_type("Proof");
    let wanted = Type::Result(Box::new(proof), Box::new(Type::Str));
    for plan in plans {
        let Some(fd) = items.iter().find_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name == *plan => Some(fd),
            _ => None,
        }) else {
            errors.push(error(
                format!(
                    "plans module '{}' lists plan '{plan}', but it defines no function of that name",
                    module.name
                ),
                header_line,
            ));
            continue;
        };
        let signature_ok = fn_sigs.get(plan).is_some_and(|(params, ret, _)| {
            params.len() == 1 && same_type(&params[0], &goal) && same_type(ret, &wanted)
        });
        if !signature_ok {
            errors.push(error(
                format!(
                    "plan '{plan}' must have the plan signature `fn {plan}(goal: Goal) -> Result<Proof, String>`, with Goal and Proof from Kernel.Proof"
                ),
                fd.line,
            ));
        }
    }
}

/// Refuse every dependency edge from a module that is not a plans module to
/// one that is: plans are never part of a program.
pub(super) fn check_plans_dependencies(
    items: &[TopLevel],
    modules: &[crate::source::LoadedModule],
    errors: &mut Vec<TypeError>,
) {
    let plans_modules: Vec<&str> = modules
        .iter()
        .filter(|m| {
            m.items
                .iter()
                .any(|i| matches!(i, TopLevel::Module(decl) if is_plans_module(decl)))
        })
        .map(|m| m.dep_name.as_str())
        .collect();
    if plans_modules.is_empty() {
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
        if is_plans_module(decl) {
            continue;
        }
        for dep in &decl.depends {
            if plans_modules.contains(&dep.as_str()) {
                let line = if Some(decl.name.as_str()) == entry_name {
                    decl.line
                } else {
                    0
                };
                errors.push(error(
                    format!(
                        "module '{}' depends on plans module '{dep}': proof plans are not part of a program, so only another plans module may depend on one",
                        decl.name
                    ),
                    line,
                ));
            }
        }
    }
}

/// Why a law's `by Module.plan` names no plan of the project at
/// `module_root`, if it names none: the module must be a project file (not
/// one the compiler ships), a plans module, and list the plan.
pub fn by_line_refusal(path: &str, module_root: &str) -> Option<String> {
    let Some((module, plan)) = path
        .rsplit_once('.')
        .filter(|(m, f)| !m.is_empty() && !f.is_empty())
    else {
        return Some(format!("`by {path}` must name a plan as Module.plan"));
    };
    if crate::source::is_kernel_module(module) {
        return Some(format!(
            "`by {path}`: {module} is a module of the proof kernel, not a plans module of this project"
        ));
    }
    let Some(file) = crate::source::find_module_file(module, module_root) else {
        return Some(format!(
            "`by {path}`: this project has no module {module}; a plan comes from a plans module of the same project"
        ));
    };
    let text = std::fs::read_to_string(&file).ok()?;
    let items = crate::source::parse_source(&text).ok()?;
    let decl = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m),
        _ => None,
    })?;
    match &decl.plans {
        None => Some(format!(
            "`by {path}`: module {module} is not a plans module (it has no `plans [...]` line)"
        )),
        Some(plans) if !plans.iter().any(|r| r == plan) => Some(format!(
            "`by {path}`: plans module {module} does not list `{plan}` in its `plans [...]` line"
        )),
        Some(_) => None,
    }
}

/// An error for every law whose `by` line names no plan of the project.
pub fn check_by_lines(items: &[TopLevel], module_root: &str) -> Vec<TypeError> {
    let mut out = Vec::new();
    for item in items {
        let TopLevel::Verify(block) = item else {
            continue;
        };
        let crate::ast::VerifyKind::Law(law) = &block.kind else {
            continue;
        };
        if let Some(path) = &law.by_plan
            && let Some(why) = by_line_refusal(path, module_root)
        {
            out.push(error(why, block.line));
        }
    }
    out
}
