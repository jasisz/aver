//! A law proved by a project rule: `by Module.rule`.
//!
//! The rule is an Aver function of a `rules [...]` module in the same
//! project. It receives the law as a `Kernel.Proof.Goal` (the step script
//! without its proof: the obligation, every definition the claim reaches,
//! the module-level bindings they read, the sum types they match on and the
//! laws the `using` list cites) and returns proof steps or a refusal. It runs
//! in the VM under a step limit; its proof is read back against the goal
//! ([`crate::ir::proof_steps::read`]) and the kernel checks the whole script
//! as it checks any other. No automatic producer runs for such a law: a
//! refusal, a rule that runs out of steps and a proof the kernel refuses all
//! leave it open, with the reason.

use std::collections::HashMap;

use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::hir::{ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedPattern};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::term::{self, Term};
use crate::ir::proof_steps::{Const, Def, LawRef, Obligation, Proof, RuleUse, Script};
use crate::ir::{LawTheorem, ProofIR};

use super::env::Env;

/// VM steps one use of a rule may take before it is stopped and the law is
/// left not checked.
pub(crate) const RULE_STEP_LIMIT: u64 = 50_000_000;

/// The step limit in force: [`RULE_STEP_LIMIT`], or `AVER_RULE_STEP_LIMIT`
/// (a testing knob, so a test can show what running out of steps does
/// without spending the full budget).
fn step_limit() -> u64 {
    std::env::var("AVER_RULE_STEP_LIMIT")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(RULE_STEP_LIMIT)
}

/// One rules module, compiled once per proof run.
pub(crate) struct Runner {
    code: crate::vm::CodeStore,
    globals: Vec<crate::nan_value::NanValue>,
    arena: crate::nan_value::Arena,
    /// Rule name to the generated function that runs it.
    entries: HashMap<String, String>,
    /// sha256 (hex) of the rules module's source and of every project
    /// module it depends on.
    hash: String,
}

/// The rules modules compiled so far in this run, by module name.
#[derive(Default)]
pub(crate) struct Rules {
    runners: HashMap<String, Result<Runner, String>>,
}

/// The module and function a `by` line names.
fn split_rule(path: &str) -> Result<(&str, &str), String> {
    path.rsplit_once('.')
        .filter(|(m, f)| !m.is_empty() && !f.is_empty())
        .ok_or_else(|| format!("`by {path}` must name a rule as Module.rule"))
}

impl Rules {
    fn runner(&mut self, root: &str, module: &str) -> &Result<Runner, String> {
        self.runners
            .entry(module.to_string())
            .or_insert_with(|| compile(root, module))
    }
}

/// Load, check and compile the rules module `module` of the project at
/// `root`, with one generated function per rule that reads the goal, runs
/// the rule and writes its answer.
fn compile(root: &str, module: &str) -> Result<Runner, String> {
    use crate::nan_value::Arena;
    let Some(path) = crate::source::find_module_file(module, root) else {
        return Err(format!(
            "this project has no module {module}: a rule must come from a rules module of the same project"
        ));
    };
    let source = std::fs::read_to_string(&path)
        .map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let items = crate::source::parse_source(&source)
        .map_err(|e| format!("rules module {module} does not parse: {e}"))?;
    let Some(rules) = items.iter().find_map(|i| match i {
        crate::ast::TopLevel::Module(m) => m.rules.clone(),
        _ => None,
    }) else {
        return Err(format!(
            "module {module} is not a rules module (it has no `rules [...]` line)"
        ));
    };
    let mut entries = HashMap::new();
    let mut entry = format!(
        "module RuleRunner\n    intent = \"Runs the proof rules of {module} on a goal.\"\n    depends [Kernel.Wire, {module}]\n    rules []\n"
    );
    for (i, rule) in rules.iter().enumerate() {
        let name = format!("runRule{i}");
        entry.push_str(&format!(
            "\nfn {name}(text: String) -> String\n    ? \"Rule {rule}.\"\n    match Kernel.Wire.goal(text)\n        Result.Err(why) -> \"goal\\n{{why}}\"\n        Result.Ok(g) -> Kernel.Wire.answer({module}.{rule}(g))\n"
        ));
        entries.insert(rule.clone(), name);
    }
    let entry_file = format!("{root}/<rules {module}>.av");
    let mut items = crate::source::parse_project_source(&entry, root, &entry_file)
        .map_err(|e| format!("rule runner: {e}"))?;
    let prepared = crate::source::load_compile_deps(&items, root)
        .map_err(|e| format!("rules module {module}: {e}"))?;
    let result = crate::ir::pipeline::run(
        &mut items,
        crate::ir::PipelineConfig {
            typecheck: Some(crate::ir::TypecheckMode::WithCheckedLoaded(
                &prepared.loaded,
            )),
            marked: prepared.marked.clone(),
            dep_modules: &prepared.modules,
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck was requested");
    if let Some(e) = tc.errors.first() {
        let file = e
            .origin
            .as_ref()
            .map(|o| o.file.clone())
            .unwrap_or_else(|| module.to_string());
        return Err(format!(
            "rules module {module} does not type-check: {file}:{}: {}",
            e.line, e.message
        ));
    }
    let mut arena = Arena::new();
    crate::vm::register_service_types(&mut arena);
    let (code, globals) = crate::vm::compile_program_with_modules(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        Some(root),
        &entry_file,
        result.analysis.as_ref(),
    )
    .map_err(|e| format!("rules module {module} does not compile: {e}"))?;
    Ok(Runner {
        code,
        globals,
        arena,
        entries,
        hash: source_hash(module, &source, &prepared.loaded),
    })
}

/// sha256 of the rules module and of each project module it depends on,
/// in name order; the modules the compiler ships (the kernel's among them)
/// are left out.
fn source_hash(module: &str, source: &str, loaded: &[crate::source::LoadedModule]) -> String {
    use sha2::{Digest, Sha256};
    let mut parts: Vec<(String, String)> = vec![(module.to_string(), source.to_string())];
    for m in loaded {
        let shipped = m.path.to_string_lossy().starts_with("<aver-stdlib>");
        if m.dep_name == module || shipped {
            continue;
        }
        if let Ok(text) = std::fs::read_to_string(&m.path) {
            parts.push((m.dep_name.clone(), text));
        }
    }
    parts.sort();
    let mut hasher = Sha256::new();
    for (name, text) in &parts {
        hasher.update(format!("module {name} {}\n", text.len()).as_bytes());
        hasher.update(text.as_bytes());
    }
    hasher
        .finalize()
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}

/// The functions `t` calls.
fn calls(t: &Term, out: &mut Vec<FnId>) {
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Fn(id), _) => out.push(*id),
        ResolvedExpr::TailCall { target, .. } => out.push(*target),
        _ => {}
    }
    let _ = term::map_children(t, &mut |c| {
        calls(c, out);
        Ok(c.clone())
    });
}

/// The constructors `t`'s patterns and constructions name.
fn ctors(t: &Term, out: &mut Vec<ResolvedCtor>) {
    fn in_pattern(p: &ResolvedPattern, out: &mut Vec<ResolvedCtor>) {
        match p {
            ResolvedPattern::Ctor(c, _) => out.push(c.clone()),
            ResolvedPattern::Tuple(ps) => ps.iter().for_each(|q| in_pattern(q, out)),
            _ => {}
        }
    }
    match &t.node {
        ResolvedExpr::Ctor(c, _) => out.push(c.clone()),
        ResolvedExpr::Match { arms, .. } => arms.iter().for_each(|a| in_pattern(&a.pattern, out)),
        _ => {}
    }
    let _ = term::map_children(t, &mut |c| {
        ctors(c, out);
        Ok(c.clone())
    });
}

/// The goal a rule gets for `ob`: every definition the claim reaches that
/// steps may open, the module-level bindings they read, the sums they
/// match on, and the cited laws.
fn goal(inputs: &ProofLowerInputs, ob: &Obligation, laws: Vec<LawRef>) -> Script {
    let mut env = Env::new(inputs);
    let mut todo = Vec::new();
    calls(&ob.lhs, &mut todo);
    calls(&ob.rhs, &mut todo);
    if let Some(p) = &ob.premise {
        calls(p, &mut todo);
    }
    let mut ids: Vec<FnId> = Vec::new();
    for id in todo {
        for d in env.reachable_defs(id) {
            if !ids.contains(&d.fn_id) {
                ids.push(d.fn_id);
            }
        }
    }
    let defs: Vec<Def> = ids.into_iter().filter_map(|id| env.def(id)).collect();
    let mut terms: Vec<&Term> = vec![&ob.lhs, &ob.rhs];
    terms.extend(ob.premise.iter());
    for d in &defs {
        terms.push(&d.body);
        terms.extend(d.lets.iter().map(|(_, v)| v));
    }
    let mut names = Vec::new();
    for t in &terms {
        term::free_vars(t, &mut names);
    }
    let consts: Vec<Const> = names
        .iter()
        .filter(|n| !ob.givens.contains(n))
        .filter_map(|n| env.constant(n))
        .collect();
    let mut seen = Vec::new();
    for t in &terms {
        ctors(t, &mut seen);
    }
    let mut sums: Vec<Vec<(ResolvedCtor, usize)>> = Vec::new();
    for c in seen {
        if let ResolvedCtor::User { type_id, .. } = &c
            && inputs
                .symbol_table
                .type_entry_if_present(*type_id)
                .is_some_and(|e| e.is_product)
        {
            continue;
        }
        let Some(variants) = super::split::variants_of(inputs, &c, None) else {
            continue;
        };
        let sum: Vec<(ResolvedCtor, usize)> =
            variants.into_iter().map(|(c, f)| (c, f.len())).collect();
        if !sums.contains(&sum) {
            sums.push(sum);
        }
    }
    Script {
        obligation: ob.clone(),
        defs,
        consts,
        laws,
        sums,
        proof: Proof::Refl(ob.lhs.clone()),
        rule: None,
    }
}

/// The definitions a proof names, and every definition of the goal those
/// reach: what the kernel and Lean need of the goal's definitions.
fn needed_defs(goal: &Script, proof: &Proof) -> Vec<Def> {
    fn named(p: &Proof, out: &mut Vec<FnId>) {
        let mut all = |ps: &[Proof], out: &mut Vec<FnId>| ps.iter().for_each(|q| named(q, out));
        let mut terms: Vec<&Term> = Vec::new();
        match p {
            Proof::Refl(t) | Proof::Proj { term: t } | Proof::Cell { list: t } => terms.push(t),
            Proof::Symm(q) => named(q, out),
            Proof::Trans { terms: ts, steps } => {
                terms.extend(ts);
                all(steps, out);
            }
            Proof::Congr { ctx, inner } => {
                terms.push(ctx);
                named(inner, out);
            }
            Proof::Unfold {
                fn_id,
                args,
                binders,
                premise,
                ..
            } => {
                out.push(*fn_id);
                terms.extend(args);
                terms.extend(binders);
                if let Some(q) = premise {
                    named(q, out);
                }
            }
            Proof::Arm {
                term: t,
                binders,
                premise,
                ..
            } => {
                terms.push(t);
                terms.extend(binders);
                named(premise, out);
            }
            Proof::Rule {
                subst, premises, ..
            } => {
                terms.extend(subst.iter().map(|(_, t)| t));
                all(premises, out);
            }
            Proof::Law { subst, premise, .. } => {
                terms.extend(subst.iter().map(|(_, t)| t));
                if let Some(q) = premise {
                    named(q, out);
                }
            }
            Proof::Compute { lhs, rhs } | Proof::Ring { lhs, rhs } => {
                terms.push(lhs);
                terms.push(rhs);
            }
            Proof::Cases {
                on,
                if_true,
                if_false,
                ..
            } => {
                terms.push(on);
                named(if_true, out);
                named(if_false, out);
            }
            Proof::Split {
                fn_id,
                args,
                on,
                cases,
                ..
            } => {
                out.push(*fn_id);
                terms.extend(args);
                terms.push(on);
                cases.iter().for_each(|c| named(&c.proof, out));
            }
            Proof::Have {
                fact, proof, body, ..
            } => {
                terms.push(fact);
                named(proof, out);
                named(body, out);
            }
            Proof::Enum {
                lhs, rhs, cases, ..
            } => {
                terms.push(lhs);
                terms.push(rhs);
                all(cases, out);
            }
            Proof::Absurd {
                contradiction,
                lhs,
                rhs,
            } => {
                terms.push(lhs);
                terms.push(rhs);
                named(contradiction, out);
            }
            Proof::Induct {
                fn_id,
                args,
                lhs,
                rhs,
                cases,
                ..
            } => {
                out.push(*fn_id);
                terms.extend(args);
                terms.push(lhs);
                terms.push(rhs);
                for c in cases {
                    named(&c.proof, out);
                    c.carry.iter().for_each(|ps| all(ps, out));
                }
            }
            Proof::InductList {
                lhs,
                rhs,
                nil,
                cons,
                ..
            } => {
                terms.push(lhs);
                terms.push(rhs);
                named(nil, out);
                named(cons, out);
            }
            Proof::Linear { goal, .. } => terms.push(goal),
            Proof::Hyp(_) | Proof::UnfoldConst { .. } => {}
        }
        for t in terms {
            calls(t, out);
        }
    }
    let mut ids = Vec::new();
    named(proof, &mut ids);
    let o = &goal.obligation;
    for t in [&o.lhs, &o.rhs].into_iter().chain(o.premise.iter()) {
        calls(t, &mut ids);
    }
    // Close over the goal's definitions.
    let mut keep: Vec<FnId> = Vec::new();
    while let Some(id) = ids.pop() {
        if keep.contains(&id) {
            continue;
        }
        let Some(d) = goal.defs.iter().find(|d| d.fn_id == id) else {
            continue;
        };
        keep.push(id);
        calls(&d.body, &mut ids);
        for (_, v) in &d.lets {
            calls(v, &mut ids);
        }
    }
    goal.defs
        .iter()
        .filter(|d| keep.contains(&d.fn_id))
        .cloned()
        .collect()
}

/// Prove law `i` by the rule its `by` line names.
pub(super) fn produce(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    i: usize,
    rules: &mut Rules,
) -> Result<Script, String> {
    let t: &LawTheorem = &ir.law_theorems[i];
    let path = t.by_rule.as_deref().expect("a law with a `by` line");
    let (module, rule) = split_rule(path)?;
    if !t.reasons.is_empty() {
        return Err(format!(
            "rule {path}: a law with a `by` line gets its whole proof from the rule, and `because` lines are not supported with it yet"
        ));
    }
    if t.premises.len() > 1 {
        return Err(format!("rule {path}: more than one premise"));
    }
    let Some(root) = inputs.rules_root else {
        return Err(format!(
            "rule {path}: this command does not run proof rules"
        ));
    };
    let laws = match &t.using {
        Some(names) if !names.is_empty() => match super::cited(inputs, ir, t, names) {
            Some(laws) => laws,
            None => return Err(format!("rule {path}: a cited law has no theorem")),
        },
        _ => Vec::new(),
    };
    let ob = super::obligation(inputs, t);
    let goal = goal(inputs, &ob, laws);
    let runner = match rules.runner(root, module) {
        Ok(r) => r,
        Err(why) => return Err(format!("rule {path}: {why}")),
    };
    let Some(entry) = runner.entries.get(rule) else {
        return Err(format!(
            "rule {path}: {module} does not list `{rule}` in its `rules [...]` line"
        ));
    };
    let use_ = RuleUse {
        name: path.to_string(),
        hash: runner.hash.clone(),
    };
    let text = crate::ir::proof_steps::sexpr::script(&goal, inputs.symbol_table)
        .map_err(|why| format!("rule {path}: the goal cannot be written as step data: {why}"))?;
    let answer = run(runner, entry, &text).map_err(|why| format!("rule {path}: {why}"))?;
    let (kind, body) = answer.split_once('\n').unwrap_or((answer.as_str(), ""));
    let proof_text = match kind {
        "proof" => body,
        "refused" => return Err(format!("rule {path} refused: {body}")),
        "goal" => return Err(format!("rule {path}: the goal could not be read: {body}")),
        _ => return Err(format!("rule {path}: an answer the compiler cannot read")),
    };
    let reader = crate::ir::proof_steps::read::Reader::for_goal(&goal, inputs.symbol_table)
        .map_err(|why| format!("rule {path}: {why}"))?;
    let proof = reader.proof_text(proof_text).map_err(|why| {
        format!(
            "rule {path} ({}): its proof cannot be read: {why}",
            use_.short_hash()
        )
    })?;
    let defs = needed_defs(&goal, &proof);
    let script = Script {
        defs,
        proof,
        rule: Some(use_.clone()),
        ..goal
    };
    if script.proof.size() > super::MAX_PROOF_NODES {
        return Err(format!(
            "rule {path} ({}): {} steps is more than a backend should elaborate",
            use_.short_hash(),
            script.proof.size()
        ));
    }
    let checked = crate::ir::proof_steps::sexpr::script(&script, inputs.symbol_table)
        .map_err(|why| format!("rule {path} ({}): {why}", use_.short_hash()))?;
    crate::proof_kernel::verdict(&checked).map_err(|why| {
        format!(
            "rule {path} ({}): the kernel refused its proof: {why}",
            use_.short_hash()
        )
    })?;
    Ok(script)
}

/// Run one generated rule function on the goal text, under the step limit.
fn run(runner: &Runner, entry: &str, goal: &str) -> Result<String, String> {
    use crate::nan_value::{NanValue, NanValueConvert};
    use crate::value::Value;
    let mut machine = crate::vm::VM::new(
        runner.code.clone(),
        runner.globals.clone(),
        runner.arena.clone(),
    );
    machine.set_silent_console(true);
    let limit = step_limit();
    machine.set_step_limit(Some(limit));
    let arg = NanValue::from_value(&Value::Str(goal.to_string()), &mut machine.arena);
    let out = machine
        .run_top_level()
        .and_then(|_| machine.run_named_function(entry, &[arg]));
    match out {
        Ok(v) => match v.to_value(&machine.arena) {
            Value::Str(s) => Ok(s),
            _ => Err("the rule runner returned something other than text".into()),
        },
        Err(crate::vm::VmError::StepLimit { .. }) => Err(format!(
            "not checked: the rule ran out of its {limit} steps"
        )),
        Err(e) => Err(format!("the rule failed: {e}")),
    }
}
