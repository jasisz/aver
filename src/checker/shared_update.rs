//! `warning[perf-shared-update]`: a repeated in-place collection update on a
//! Map or Vector that something else still holds.
//!
//! Aver collections are values. `Map.set`, `Map.remove` and `Vector.set` are
//! cheap only when nothing else holds the collection they are handed: then
//! every backend updates it in place. When the same collection is still
//! reachable from another live value, the update has to copy all of it first,
//! and nothing says so. In a loop that copy is paid on every turn, so a table
//! that should cost one entry per step costs its whole size.
//!
//! The check is deliberately narrow, because a warning here is a claim about
//! what the program costs and a wrong claim teaches people to ignore it. It
//! fires only when all of these hold:
//!
//! - **An update.** The value is handed as the target of `Map.set`,
//!   `Map.remove` or `Vector.set`, or at a parameter position of a function
//!   that hands that parameter, directly or through further calls, to one of
//!   them (followed into the dependencies the call names).
//! - **A copy the backends make.** The value is a field read out of a record
//!   local — `setting.window.created`, or a local bound to one by a `let` or
//!   a `match` and handed on at its last use — and the read does not move
//!   the field out of the record. The module is lowered the way it compiles
//!   and the answer is `field_moves`, the analysis generated Rust moves
//!   fields by: a field read moves when every other read of the record runs
//!   in another branch, finished earlier, or reads a disjoint part (the rest
//!   of the record handed on to the update that replaces the field), and the
//!   record is not used after. Any other field read is a copy, and the record
//!   keeps the original. A record a loop hands on unchanged keeps every
//!   field.
//! - **Repeated.** The function doing it is recursive, is reached from a
//!   recursive function of its module, or belongs to an answer module, whose
//!   functions run once per request.
//!
//! What it misses: a caller that keeps a record it passed to the function
//! doing the update (the function cannot see its callers), a callee that
//! borrows the value and copies it itself, aliases made through a binding
//! (`kept = state`), a collection shared by two records, and calls through
//! function values.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::ast::{Expr, FnDef, Spanned, Stmt, StrPart, TopLevel};

use super::CheckFinding;
use crate::ir::mir::expr::walk_children;
use crate::ir::mir::{LocalId, MirCallee, MirExpr, MirFn, MirPattern, MirProgram};

/// The builtins that update their first argument in place when they own it.
const UPDATES: [(&str, Kind); 3] = [
    ("Map.set", Kind::Map),
    ("Map.remove", Kind::Map),
    ("Vector.set", Kind::Vector),
];

/// Recursion cap for following dependencies: past this depth a callee is
/// treated as not updating anything.
const MAX_MODULE_DEPTH: usize = 16;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    Map,
    Vector,
}

impl Kind {
    fn name(self) -> &'static str {
        match self {
            Kind::Map => "Map",
            Kind::Vector => "Vector",
        }
    }
}

/// Per function, per parameter: the collection kind the function updates in
/// place when it is handed that parameter, if any.
type Summary = HashMap<String, Vec<Option<Kind>>>;

/// Source of a dependency's items, by module name. `None` when the module
/// cannot be found or does not parse; its functions then update nothing.
pub type ModuleSource<'a> = dyn Fn(&str) -> Option<Vec<TopLevel>> + 'a;

/// Summaries of the dependencies calls name, loaded on first use.
struct Dependencies<'a> {
    source: &'a ModuleSource<'a>,
    loaded: HashMap<String, Option<Summary>>,
    depth: usize,
}

impl<'a> Dependencies<'a> {
    fn new(source: &'a ModuleSource<'a>) -> Self {
        Self {
            source,
            loaded: HashMap::new(),
            depth: 0,
        }
    }

    /// The kind `Module.fn` updates at parameter `index`, for a dotted callee.
    fn update_at(&mut self, dotted: &str, index: usize) -> Option<Kind> {
        let (module, function) = dotted.rsplit_once('.')?;
        let first = module.split('.').next().unwrap_or(module);
        if crate::ir::is_builtin_namespace(first) {
            return None;
        }
        if !self.loaded.contains_key(module) {
            let summary = if self.depth >= MAX_MODULE_DEPTH {
                None
            } else {
                // Marked before it is computed, so a cycle reads "unknown".
                self.loaded.insert(module.to_string(), None);
                (self.source)(module).map(|items| {
                    self.depth += 1;
                    let summary = summarize(&items, self);
                    self.depth -= 1;
                    summary
                })
            };
            self.loaded.insert(module.to_string(), summary);
        }
        self.loaded
            .get(module)?
            .as_ref()?
            .get(function)?
            .get(index)
            .copied()
            .flatten()
    }
}

fn fn_defs(items: &[TopLevel]) -> Vec<&FnDef> {
    items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect()
}

fn dotted(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => {
            let mut prefix = dotted(&base.node)?;
            prefix.push('.');
            prefix.push_str(field);
            Some(prefix)
        }
        _ => None,
    }
}

/// The name a bare local read spells, resolved or not.
fn bare_local(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(name) => Some(name),
        Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

fn builtin_update(callee: &str, index: usize) -> Option<Kind> {
    (index == 0)
        .then(|| {
            UPDATES
                .iter()
                .find(|(name, _)| *name == callee)
                .map(|(_, k)| *k)
        })
        .flatten()
}

/// The kind a call to `callee` updates at argument `index`, if any.
fn update_at(
    callee: &str,
    index: usize,
    own: &Summary,
    deps: &mut Dependencies<'_>,
) -> Option<Kind> {
    if let Some(kind) = builtin_update(callee, index) {
        return Some(kind);
    }
    if let Some(params) = own.get(callee) {
        return params.get(index).copied().flatten();
    }
    if callee.contains('.') {
        return deps.update_at(callee, index);
    }
    None
}

/// Every call in `expr`, with its callee name and arguments.
fn each_call<'e>(expr: &'e Spanned<Expr>, visit: &mut impl FnMut(&str, &'e [Spanned<Expr>])) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = dotted(&callee.node) {
                visit(&name, args);
            }
            for arg in args {
                each_call(arg, visit);
            }
        }
        Expr::TailCall(tail) => {
            visit(&tail.target, &tail.args);
            for arg in &tail.args {
                each_call(arg, visit);
            }
        }
        _ => children(&expr.node, &mut |child| each_call(child, visit)),
    }
}

/// The direct sub-expressions of `expr`, in evaluation order.
fn children<'e>(expr: &'e Expr, visit: &mut impl FnMut(&'e Spanned<Expr>)) {
    match expr {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        Expr::Attr(base, _) => visit(base),
        Expr::FnCall(callee, args) => {
            visit(callee);
            args.iter().for_each(visit);
        }
        Expr::TailCall(tail) => tail.args.iter().for_each(visit),
        Expr::BinOp(_, left, right) => {
            visit(left);
            visit(right);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => visit(inner),
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                visit(inner);
            }
        }
        Expr::Match { subject, arms } => {
            visit(subject);
            for arm in arms {
                visit(&arm.body);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    visit(inner);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().for_each(visit)
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                visit(key);
                visit(value);
            }
        }
        Expr::RecordCreate { fields, .. } => fields.iter().for_each(|(_, value)| visit(value)),
        Expr::RecordUpdate { base, updates, .. } => {
            visit(base);
            updates.iter().for_each(|(_, value)| visit(value));
        }
    }
}

fn body_exprs(fd: &FnDef) -> impl Iterator<Item = &Spanned<Expr>> {
    fd.body.stmts().iter().map(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr,
    })
}

/// Which parameters each function of `items` updates in place, as a fixpoint
/// over the calls it hands them to.
fn summarize(items: &[TopLevel], deps: &mut Dependencies<'_>) -> Summary {
    let fns = fn_defs(items);
    let records: HashMap<&str, Vec<String>> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::TypeDef(crate::ast::TypeDef::Product { name, fields, .. }) => Some((
                name.as_str(),
                fields.iter().map(|(_, ty)| spelled(ty)).collect(),
            )),
            _ => None,
        })
        .collect();
    let mut summary: Summary = fns
        .iter()
        .map(|fd| (fd.name.clone(), vec![None; fd.params.len()]))
        .collect();
    loop {
        let mut changed = false;
        for fd in &fns {
            let params: HashMap<&str, usize> = fd
                .params
                .iter()
                .enumerate()
                .map(|(i, (name, _))| (name.as_str(), i))
                .collect();
            let mut found: Vec<(usize, Kind)> = Vec::new();
            for expr in body_exprs(fd) {
                each_call(expr, &mut |callee, args| {
                    for (index, arg) in args.iter().enumerate() {
                        let Some(&param) = bare_local(&arg.node).and_then(|n| params.get(n)) else {
                            continue;
                        };
                        if let Some(kind) = update_at(callee, index, &summary, deps) {
                            found.push((param, kind));
                        }
                    }
                });
            }
            let entry = summary.get_mut(&fd.name).expect("every fn is summarized");
            for (param, kind) in found {
                if entry[param].is_none() && hands_back(fd, param, &records) {
                    entry[param] = Some(kind);
                    changed = true;
                }
            }
        }
        if !changed {
            return summary;
        }
    }
}

/// A type spelled without spaces, so two spellings of one type compare equal.
fn spelled(ty: &str) -> String {
    ty.chars().filter(|c| !c.is_whitespace()).collect()
}

/// Whether `fd` hands the collection it updates at `param` back to its
/// caller: its result names the parameter's type, or a record of its module
/// with a field of that type.
///
/// This is what tells a caller that keeps the old collection by mistake from
/// one that keeps it on purpose. An interpreter that binds a name into a copy
/// of its environment and returns a value keeps the caller's environment
/// intact, and must: that copy is the semantics, not a trap. A function that
/// returns the updated collection is handing over a replacement, and a
/// caller still holding the original is paying for a copy it will drop.
fn hands_back(fd: &FnDef, param: usize, records: &HashMap<&str, Vec<String>>) -> bool {
    let ty = spelled(&fd.params[param].1);
    let result = spelled(&fd.return_type);
    result.contains(&ty)
        || result
            .split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '.'))
            .filter_map(|name| records.get(name.rsplit('.').next().unwrap_or(name)))
            .any(|fields| fields.contains(&ty))
}

/// Functions whose body runs again and again: recursive ones, the ones a
/// recursive function reaches, and every function of an answer module.
fn repeated_fns(items: &[TopLevel]) -> HashSet<String> {
    let answers = items
        .iter()
        .any(|item| matches!(item, TopLevel::Module(module) if !module.answers.is_empty()));
    if answers {
        return fn_defs(items).iter().map(|fd| fd.name.clone()).collect();
    }
    let calls = crate::call_graph::direct_calls(items);
    let mut repeated = crate::call_graph::find_recursive_fns(items);
    let mut queue: VecDeque<String> = repeated.iter().cloned().collect();
    while let Some(name) = queue.pop_front() {
        for callee in calls.get(&name).into_iter().flatten() {
            if calls.contains_key(callee) && repeated.insert(callee.clone()) {
                queue.push_back(callee.clone());
            }
        }
    }
    repeated
}

/// Where a value handed to an update comes from: a field of a record local,
/// read either at the update itself or earlier into the local handed on.
struct Origin<'m> {
    /// The field read, `s.window.created`: the node the backends move or
    /// copy.
    read: &'m MirExpr,
    /// The record local's name and the path read out of it.
    record: String,
    path: Vec<String>,
}

/// One function's lowered body, with what the backends decide about it.
struct Body<'m> {
    /// Field reads that move their field out of the record
    /// (`field_moves::movable_projections`); every other field read copies
    /// it, and the record keeps the original.
    movable: HashSet<usize>,
    /// Params a self tail call hands on unchanged: the loop keeps them whole,
    /// so no field of one ever moves.
    carried: HashSet<LocalId>,
    /// Locals bound to a field read, by a `let` or by a `match` on it.
    bound: HashMap<LocalId, Origin<'m>>,
}

/// `local.f1.….fn` as `(local, name, [f1, …, fn])`.
fn projection_path(expr: &MirExpr) -> Option<(LocalId, String, Vec<String>)> {
    match expr {
        MirExpr::Local(local) if !local.node.name.is_empty() => {
            Some((local.node.slot, local.node.name.clone(), Vec::new()))
        }
        MirExpr::Project(project) => {
            let (slot, name, mut path) = projection_path(&project.node.base.node)?;
            path.push(project.node.field.clone());
            Some((slot, name, path))
        }
        _ => None,
    }
}

fn field_read(expr: &MirExpr) -> Option<Origin<'_>> {
    let (_, record, path) = projection_path(expr)?;
    (!path.is_empty()).then_some(Origin {
        read: expr,
        record,
        path,
    })
}

/// Every local a pattern binds, with the part of the subject it binds when
/// that part is a field read.
fn bind_pattern<'m>(
    pattern: &MirPattern,
    subject: &'m MirExpr,
    bound: &mut HashMap<LocalId, Origin<'m>>,
) {
    match pattern {
        MirPattern::Bind(slot, _) => {
            if let Some(origin) = field_read(subject) {
                bound.insert(*slot, origin);
            }
        }
        MirPattern::Tuple(items) => {
            if let MirExpr::Tuple(parts) = subject {
                for (item, part) in items.iter().zip(parts) {
                    bind_pattern(item, &part.node, bound);
                }
            } else {
                for item in items {
                    bind_pattern(item, subject, bound);
                }
            }
        }
        // A payload or a list element is part of the field it was matched
        // out of, and shares its backing with it.
        MirPattern::Ctor { bindings, .. } => {
            for slot in bindings {
                if let Some(origin) = field_read(subject) {
                    bound.insert(*slot, origin);
                }
            }
        }
        MirPattern::Cons { head, tail, .. } => {
            for slot in [head, tail] {
                if let Some(origin) = field_read(subject) {
                    bound.insert(*slot, origin);
                }
            }
        }
        MirPattern::Wildcard | MirPattern::Literal(_) | MirPattern::EmptyList => {}
    }
}

fn collect_bound<'m>(expr: &'m MirExpr, bound: &mut HashMap<LocalId, Origin<'m>>) {
    match expr {
        MirExpr::Let(binding) => {
            let value = &binding.node.value.node;
            if let Some(origin) = field_read(value) {
                bound.insert(binding.node.binding, origin);
            }
        }
        MirExpr::Match(matched) => {
            for arm in &matched.node.arms {
                bind_pattern(&arm.pattern, &matched.node.subject.node, bound);
            }
        }
        _ => {}
    }
    walk_children(expr, &mut |child| collect_bound(child, bound));
}

/// The params every self tail call of `f` hands on as they are.
fn carried_params(f: &MirFn) -> HashSet<LocalId> {
    fn visit(expr: &MirExpr, f: &MirFn, kept: &mut Vec<bool>, calls: &mut usize) {
        if let MirExpr::TailCall(call) = expr
            && call.node.target == f.fn_id
        {
            *calls += 1;
            for (index, param) in f.params.iter().enumerate() {
                let same = call.node.args.get(index).is_some_and(|arg| {
                    matches!(&arg.node, MirExpr::Local(local) if local.node.slot == param.local)
                });
                kept[index] &= same;
            }
        }
        walk_children(expr, &mut |child| visit(child, f, kept, calls));
    }
    let mut kept = vec![true; f.params.len()];
    let mut calls = 0;
    visit(&f.body.node, f, &mut kept, &mut calls);
    if calls == 0 {
        return HashSet::new();
    }
    f.params
        .iter()
        .zip(kept)
        .filter_map(|(param, kept)| kept.then_some(param.local))
        .collect()
}

struct Walk<'s, 'd, 'm> {
    own: &'s Summary,
    deps: &'s mut Dependencies<'d>,
    program: &'m MirProgram,
    symbols: &'s crate::ir::SymbolTable,
    body: &'s Body<'m>,
    fn_name: String,
    module: Option<String>,
    findings: Vec<CheckFinding>,
}

impl Walk<'_, '_, '_> {
    /// The name a call spells its callee by, as the summaries key it: the
    /// builtin's name, a function of this module by its own name, one of a
    /// dependency as `Module.fn`.
    fn callee_name(&self, callee: &MirCallee) -> Option<String> {
        match callee {
            MirCallee::Builtin(id) => Some(self.program.builtin_name(*id).to_string()),
            MirCallee::Fn(id) => self.fn_name_of(*id),
            _ => None,
        }
    }

    fn fn_name_of(&self, id: crate::ir::FnId) -> Option<String> {
        let key = &self.symbols.fn_entry(id).key;
        Some(match &key.scope {
            Some(scope) if Some(scope.as_str()) != self.module.as_deref() => {
                format!("{scope}.{}", key.name)
            }
            _ => key.name.clone(),
        })
    }

    /// Whether the backends copy the field `origin` reads, leaving the
    /// original in the record: the read does not move it, or the record is
    /// a param the loop carries whole.
    fn copies(&self, origin: &Origin<'_>) -> bool {
        let moved = self
            .body
            .movable
            .contains(&(origin.read as *const MirExpr as usize));
        let carried = projection_path(origin.read)
            .is_some_and(|(slot, _, _)| self.body.carried.contains(&slot));
        !moved || carried
    }

    fn check_site(&mut self, callee: &str, index: usize, arg: &Spanned<MirExpr>) {
        let Some(kind) = update_at(callee, index, self.own, self.deps) else {
            return;
        };
        let body = self.body;
        // The value is a field read here, or a local bound to one earlier
        // and read here for the last time. A local read again later is left
        // alone: the caller keeping both versions is usually the point (a
        // scope, an undo). A record field is different: the record is the
        // only reason the old one lives.
        let found;
        let (origin, via) = match &arg.node {
            MirExpr::Local(local) => {
                let Some(origin) = body.bound.get(&local.node.slot) else {
                    return;
                };
                if !local.node.last_use {
                    return;
                }
                (origin, Some(local.node.name.clone()))
            }
            _ => {
                let Some(origin) = field_read(&arg.node) else {
                    return;
                };
                found = origin;
                (&found, None)
            }
        };
        if self.copies(origin) {
            self.report(callee, index, kind, origin, via, arg.line);
        }
    }

    fn report(
        &mut self,
        callee: &str,
        index: usize,
        kind: Kind,
        origin: &Origin<'_>,
        via: Option<String>,
        line: usize,
    ) {
        let name = &origin.record;
        let read = format!("{name}.{}", origin.path.join("."));
        let what = kind.name();
        let repair = format!(
            "read it where nothing reads that part of `{name}` again, for example in the update of `{name}` that replaces it, so the {what} moves out of `{name}` instead of being shared with it"
        );
        let message = match (builtin_update(callee, index).is_some(), &via) {
            (true, None) => format!(
                "`{callee}` on `{read}` updates a {what} that is still held by `{name}`; each update copies the whole {what} — {repair}"
            ),
            (true, Some(local)) => format!(
                "`{callee}` on `{local}`, read from `{read}`, updates a {what} that is still held by `{name}`; each update copies the whole {what} — {repair}"
            ),
            (false, None) => format!(
                "`{callee}` updates `{read}`, a {what} that is still held by `{name}`; each call copies the whole {what} — {repair}"
            ),
            (false, Some(local)) => format!(
                "`{callee}` updates `{local}`, read from `{read}`, a {what} that is still held by `{name}`; each call copies the whole {what} — {repair}"
            ),
        };
        self.findings.push(CheckFinding {
            line,
            module: self.module.clone(),
            file: None,
            fn_name: Some(self.fn_name.clone()),
            message,
            extra_spans: vec![],
        });
    }

    fn walk(&mut self, expr: &MirExpr) {
        match expr {
            MirExpr::Call(call) => {
                if let Some(name) = self.callee_name(&call.node.callee) {
                    for (index, arg) in call.node.args.iter().enumerate() {
                        self.check_site(&name, index, arg);
                    }
                }
            }
            MirExpr::TailCall(call) => {
                if let Some(name) = self.fn_name_of(call.node.target) {
                    for (index, arg) in call.node.args.iter().enumerate() {
                        self.check_site(&name, index, arg);
                    }
                }
            }
            _ => {}
        }
        walk_children(expr, &mut |child| self.walk(child));
    }
}

/// Whether some repeated function of `items` hands anything to an update at
/// all. Lowering the module is only worth it then.
fn has_update_site(
    items: &[TopLevel],
    repeated: &HashSet<String>,
    own: &Summary,
    deps: &mut Dependencies<'_>,
) -> bool {
    let mut found = false;
    for fd in fn_defs(items)
        .into_iter()
        .filter(|fd| repeated.contains(&fd.name))
    {
        for expr in body_exprs(fd) {
            each_call(expr, &mut |callee, args| {
                found = found
                    || (0..args.len()).any(|index| update_at(callee, index, own, deps).is_some());
            });
        }
    }
    found
}

/// The module lowered the way every backend lowers it, so the check reads
/// the same field moves they make.
fn lowered(items: &[TopLevel], symbols: &crate::ir::SymbolTable) -> MirProgram {
    let mut items = items.to_vec();
    crate::resolver::resolve_program(&mut items);
    crate::ir::last_use::annotate_program_last_use(&mut items);
    let resolved = crate::ir::hir::resolve_program(symbols, &items);
    crate::ir::mir::optimize(crate::ir::mir::lower_program(&resolved))
}

/// The symbol table of the program a module is checked in: the module and
/// the dependencies it loads.
pub type ProgramSymbols<'a> = dyn Fn(&[TopLevel]) -> crate::ir::SymbolTable + 'a;

/// Warnings for one module. `items` is the module as the checker saw it;
/// `source` finds the dependencies its calls name and `symbols` builds the
/// program's symbol table, so the module lowers the way it compiles.
pub fn collect_shared_update_warnings(
    items: &[TopLevel],
    source: &ModuleSource<'_>,
    symbols: &ProgramSymbols<'_>,
) -> Vec<CheckFinding> {
    let repeated = repeated_fns(items);
    if repeated.is_empty() {
        return Vec::new();
    }
    let mut deps = Dependencies::new(source);
    let own = summarize(items, &mut deps);
    if !has_update_site(items, &repeated, &own, &mut deps) {
        return Vec::new();
    }
    let module = super::module_name_for_items(items);
    let symbols = symbols(items);
    let program = lowered(items, &symbols);

    let mut findings = Vec::new();
    let mut fns: Vec<&MirFn> = program.iter().map(|(_, f)| f).collect();
    fns.sort_by_key(|f| f.fn_id.0);
    // Functions the compiler generated (the loop's `__…` helpers) are not the
    // author's to change, so they are not reported at the author's file.
    for f in fns
        .into_iter()
        .filter(|f| repeated.contains(&f.name) && !f.name.starts_with("__"))
    {
        let mut bound = HashMap::new();
        collect_bound(&f.body.node, &mut bound);
        let body = Body {
            movable: crate::ir::mir::field_moves::movable_projections(&f.body.node),
            carried: carried_params(f),
            bound,
        };
        let mut walk = Walk {
            own: &own,
            deps: &mut deps,
            program: &program,
            symbols: &symbols,
            body: &body,
            fn_name: f.name.clone(),
            module: module.clone(),
            findings: Vec::new(),
        };
        walk.walk(&f.body.node);
        findings.extend(walk.findings);
    }
    findings.sort_by_key(|finding| finding.line);
    findings
}
