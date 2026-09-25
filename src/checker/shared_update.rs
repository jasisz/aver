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
//! - **A value the compiler can see is shared.** Either it is read out of a
//!   record — `setting.window.created` — while the record stays reachable
//!   (the local is read again later, or it was already passed whole to the
//!   call or record the update is an argument of), or it is a local
//!   collection that is read again after the update.
//! - **Repeated.** The function doing it is recursive, is reached from a
//!   recursive function of its module, or belongs to an answer module, whose
//!   functions run once per request.
//!
//! One shape is left alone on purpose: a record literal or `T.update(x, …)`
//! that reads the field it replaces once and otherwise only other fields of
//! `x`, with `x` dead afterwards. The VM takes that field out of the record
//! before the update, so the collection is not shared when the update runs.
//!
//! What it misses: a caller that keeps a record it passed to the function
//! doing the update (the function cannot see its callers), aliases made
//! through a binding (`kept = state`), a collection shared by two records, and
//! calls through function values. Each of those still copies; none of them is
//! visible in one function's body without the whole-program ownership facts.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::ast::{Expr, FnDef, Spanned, Stmt, StrPart, TopLevel};

use super::CheckFinding;

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

/// A record literal or update the VM may take a field out of: the fields of
/// each local it may take. Mirrors the VM's rule: every read of the local in
/// the values is a projection, the field is projected once, the local dies
/// there, and an update of the local itself writes the field.
#[derive(Default)]
struct Scope {
    takes: HashMap<u16, HashSet<String>>,
}

#[derive(Default)]
struct SlotReads {
    bare: bool,
    projected: HashMap<String, usize>,
    last_use: bool,
}

fn collect_reads(expr: &Spanned<Expr>, reads: &mut HashMap<u16, SlotReads>) {
    match &expr.node {
        Expr::Attr(base, field) => {
            if let Expr::Resolved { slot, last_use, .. } = &base.node {
                let entry = reads.entry(*slot).or_default();
                *entry.projected.entry(field.clone()).or_default() += 1;
                entry.last_use |= last_use.0;
                return;
            }
            collect_reads(base, reads);
        }
        Expr::Resolved { slot, last_use, .. } => {
            let entry = reads.entry(*slot).or_default();
            entry.bare = true;
            entry.last_use |= last_use.0;
        }
        other => children(other, &mut |child| collect_reads(child, reads)),
    }
}

fn scope_of<'e>(
    base_slot: Option<u16>,
    written: &HashSet<&str>,
    values: impl Iterator<Item = &'e Spanned<Expr>>,
) -> Scope {
    let mut reads: HashMap<u16, SlotReads> = HashMap::new();
    for value in values {
        collect_reads(value, &mut reads);
    }
    let mut scope = Scope::default();
    for (slot, slot_reads) in reads {
        if slot_reads.bare || !slot_reads.last_use {
            continue;
        }
        let fields: HashSet<String> = slot_reads
            .projected
            .into_iter()
            .filter(|(field, count)| {
                *count == 1 && (base_slot != Some(slot) || written.contains(field.as_str()))
            })
            .map(|(field, _)| field)
            .collect();
        if !fields.is_empty() {
            scope.takes.insert(slot, fields);
        }
    }
    scope
}

/// Whether the last read of local `slot` is inside `args`.
fn last_read_within(slot: u16, args: &[Spanned<Expr>]) -> bool {
    fn visit(expr: &Spanned<Expr>, slot: u16, found: &mut bool) {
        if let Expr::Resolved {
            slot: read,
            last_use,
            ..
        } = &expr.node
            && *read == slot
            && last_use.0
        {
            *found = true;
        }
        children(&expr.node, &mut |child| visit(child, slot, found));
    }
    let mut found = false;
    for arg in args {
        visit(arg, slot, &mut found);
    }
    found
}

/// The locals an evaluated value holds whole: a bare read, or one inside a
/// tuple, list, constructor or record built right there.
fn held_locals(expr: &Expr, out: &mut Vec<(u16, String)>) {
    match expr {
        Expr::Resolved { slot, name, .. } => out.push((*slot, name.clone())),
        Expr::Tuple(items) | Expr::List(items) => {
            items.iter().for_each(|item| held_locals(&item.node, out))
        }
        Expr::Constructor(_, Some(inner)) => held_locals(&inner.node, out),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .for_each(|(_, value)| held_locals(&value.node, out)),
        Expr::RecordUpdate { base, updates, .. } => {
            held_locals(&base.node, out);
            updates
                .iter()
                .for_each(|(_, value)| held_locals(&value.node, out));
        }
        _ => {}
    }
}

struct Walk<'s, 'd> {
    own: &'s Summary,
    deps: &'s mut Dependencies<'d>,
    fn_name: String,
    module: Option<String>,
    /// Locals whose whole value an enclosing call or constructor has already
    /// evaluated and still holds, innermost last. The flag marks the base of
    /// an enclosing record update.
    pending: Vec<(u16, String, bool)>,
    scopes: Vec<Scope>,
    findings: Vec<CheckFinding>,
}

impl Walk<'_, '_> {
    /// Whether the VM takes field `field` out of local `slot` before the
    /// update: an enclosing literal or update plans it.
    fn taken_first(&self, slot: u16, field: &str) -> bool {
        self.scopes
            .iter()
            .find(|scope| scope.takes.contains_key(&slot))
            .is_some_and(|scope| scope.takes[&slot].contains(field))
    }

    fn check_site(&mut self, callee: &str, index: usize, args: &[Spanned<Expr>]) {
        let arg = &args[index];
        let Some(kind) = update_at(callee, index, self.own, self.deps) else {
            return;
        };
        // The record the value is read out of, and its first field.
        let mut fields: Vec<&str> = Vec::new();
        let mut root = &arg.node;
        while let Expr::Attr(base, field) = root {
            fields.push(field);
            root = &base.node;
        }
        let Expr::Resolved { slot, name, .. } = root else {
            return;
        };
        // A bare local read again later is left alone: the caller keeping
        // both versions is usually the point (a scope, an undo). A record
        // field is different: the record is the only reason the old one lives.
        if fields.is_empty() {
            return;
        }
        fields.reverse();
        let first = fields.first().copied();
        let exempt = first.is_some_and(|first| fields.len() == 1 && self.taken_first(*slot, first));
        let held_by_pending = self
            .pending
            .iter()
            .any(|(pending, _, is_base)| pending == slot && !(*is_base && exempt));
        // The update runs once every argument of its call is evaluated, so
        // a read in a later argument is over by then; only a read after the
        // whole call keeps the local holding the value.
        let read_later = !last_read_within(*slot, args) && !exempt;
        if !(held_by_pending || read_later) {
            return;
        }
        let shown = format!("{name}.{}", fields.join("."));
        let what = kind.name();
        let repair = format!(
            "take it out of `{name}` first (bind the parts with a match and carry on with a `{name}` that no longer holds them), or read it at the last use of `{name}`"
        );
        let message = if builtin_update(callee, index).is_some() {
            format!(
                "`{callee}` on `{shown}` updates a {what} that is still held by `{name}`; each update copies the whole {what} — {repair}"
            )
        } else {
            format!(
                "`{callee}` updates `{shown}`, a {what} that is still held by `{name}`; each call copies the whole {what} — {repair}"
            )
        };
        self.findings.push(CheckFinding {
            line: arg.line,
            module: self.module.clone(),
            file: None,
            fn_name: Some(self.fn_name.clone()),
            message,
            extra_spans: vec![],
        });
    }

    /// Walk the arguments of one call: each sees the whole values the earlier
    /// ones hold, and each is checked as an update site.
    fn walk_call(&mut self, callee: Option<&str>, args: &[Spanned<Expr>]) {
        let mark = self.pending.len();
        for (index, item) in args.iter().enumerate() {
            if let Some(callee) = callee {
                self.check_site(callee, index, args);
            }
            self.walk(item);
            let mut held = Vec::new();
            held_locals(&item.node, &mut held);
            self.pending
                .extend(held.into_iter().map(|(slot, name)| (slot, name, false)));
        }
        self.pending.truncate(mark);
    }

    /// Walk the items of one aggregate in evaluation order: each sees the
    /// whole values the earlier ones hold.
    fn walk_in_order<'e>(&mut self, items: impl Iterator<Item = &'e Spanned<Expr>>) {
        let mark = self.pending.len();
        for item in items {
            self.walk(item);
            let mut held = Vec::new();
            held_locals(&item.node, &mut held);
            self.pending
                .extend(held.into_iter().map(|(slot, name)| (slot, name, false)));
        }
        self.pending.truncate(mark);
    }

    fn walk(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::FnCall(callee, args) => {
                let name = dotted(&callee.node);
                self.walk_call(name.as_deref(), args);
            }
            Expr::TailCall(tail) => {
                let target = tail.target.clone();
                self.walk_call(Some(&target), &tail.args);
            }
            Expr::Tuple(items) | Expr::List(items) | Expr::IndependentProduct(items, _) => {
                self.walk_in_order(items.iter())
            }
            Expr::RecordCreate { fields, .. } => {
                let written: HashSet<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
                self.scopes.push(scope_of(
                    None,
                    &written,
                    fields.iter().map(|(_, value)| value),
                ));
                self.walk_in_order(fields.iter().map(|(_, value)| value));
                self.scopes.pop();
            }
            Expr::RecordUpdate { base, updates, .. } => {
                self.walk(base);
                let base_slot = match &base.node {
                    Expr::Resolved { slot, .. } => Some(*slot),
                    _ => None,
                };
                let written: HashSet<&str> = updates.iter().map(|(n, _)| n.as_str()).collect();
                self.scopes.push(scope_of(
                    base_slot,
                    &written,
                    updates.iter().map(|(_, value)| value),
                ));
                let mark = self.pending.len();
                let mut held = Vec::new();
                held_locals(&base.node, &mut held);
                self.pending.extend(
                    held.into_iter()
                        .map(|(slot, name)| (slot, name, Some(slot) == base_slot)),
                );
                self.walk_in_order(updates.iter().map(|(_, value)| value));
                self.pending.truncate(mark);
                self.scopes.pop();
            }
            other => children(other, &mut |child| self.walk(child)),
        }
    }
}

/// Warnings for one module. `items` is the module as the checker saw it;
/// `source` finds the dependencies its calls name.
pub fn collect_shared_update_warnings(
    items: &[TopLevel],
    source: &ModuleSource<'_>,
) -> Vec<CheckFinding> {
    let repeated = repeated_fns(items);
    if repeated.is_empty() {
        return Vec::new();
    }
    let mut deps = Dependencies::new(source);
    let own = summarize(items, &mut deps);
    let module = super::module_name_for_items(items);

    // Last use needs slots; resolve a copy of the repeated functions only.
    let mut resolved: Vec<TopLevel> = items
        .iter()
        .filter(|item| match item {
            TopLevel::FnDef(fd) => repeated.contains(&fd.name),
            TopLevel::TypeDef(_) => true,
            _ => false,
        })
        .cloned()
        .collect();
    crate::resolver::resolve_program(&mut resolved);
    crate::ir::last_use::annotate_program_last_use(&mut resolved);

    let mut findings = Vec::new();
    // Functions the compiler generated (the loop's `__…` helpers) are not the
    // author's to change, so they are not reported at the author's file.
    for fd in fn_defs(&resolved)
        .into_iter()
        .filter(|fd| !fd.name.starts_with("__"))
    {
        let mut walk = Walk {
            own: &own,
            deps: &mut deps,
            fn_name: fd.name.clone(),
            module: module.clone(),
            pending: Vec::new(),
            scopes: Vec::new(),
            findings: Vec::new(),
        };
        for expr in body_exprs(fd) {
            walk.walk(expr);
        }
        findings.extend(walk.findings);
    }
    findings
}
