/// Compile-time variable resolution pass.
///
/// After parsing and before interpretation, this pass walks each `FnDef` body
/// and replaces `Expr::Ident(name)` with `Expr::Resolved(depth, slot)` for
/// variables that are local to the function (parameters + bindings).
///
/// Global/namespace identifiers are left as `Expr::Ident` — the VM
/// falls back to HashMap lookup for those.
///
/// Only top-level `FnDef` bodies are resolved. Top-level `Stmt` items (globals,
/// REPL) are not touched.
use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::ast::*;
use crate::types::checker::TypeError;

/// Cross-function type info gathered from the program's `TypeDef` items.
/// Used by the slot-types pass to recover the field type list for a
/// user-declared variant or record so pattern bindings get a precise
/// `Type` per slot.
struct TypeInfo {
    /// `(parent_sum_name, variant_name) -> field type strings`. Variant
    /// names are stored bare; the parent disambiguates when the same
    /// bare name appears across multiple sumtypes (for example
    /// `Query.ProviderSummary(String)` and `QueryOutput.ProviderSummary
    /// (ProviderSummary)` both expose a `ProviderSummary` variant —
    /// without the parent key, one would silently shadow the other and
    /// the resolver would stamp pattern bindings with the wrong field
    /// type).
    variants: HashMap<(String, String), Vec<String>>,
    /// `variant_name -> [parent sumtypes]`. Used when the match-site's
    /// subject type isn't carried (older callers or wildcard subjects);
    /// falls back to the unique parent if there's exactly one, or
    /// returns the first registered for backward compatibility with
    /// monomorphic programs.
    variant_parents: HashMap<String, Vec<String>>,
    /// `record_name -> [(field_name, field_type_string)]`. Records that
    /// reach a binding via record-update or pattern destructure are
    /// looked up here when the slot-types pass needs to know a field's
    /// declared type.
    #[allow(dead_code)]
    records: HashMap<String, Vec<(String, String)>>,
}

fn build_type_info(items: &[TopLevel]) -> TypeInfo {
    let mut variants: HashMap<(String, String), Vec<String>> = HashMap::new();
    let mut variant_parents: HashMap<String, Vec<String>> = HashMap::new();
    let mut records: HashMap<String, Vec<(String, String)>> = HashMap::new();
    for item in items {
        match item {
            TopLevel::TypeDef(TypeDef::Sum {
                name: parent,
                variants: vs,
                ..
            }) => {
                for v in vs {
                    variants.insert((parent.clone(), v.name.clone()), v.fields.clone());
                    variant_parents
                        .entry(v.name.clone())
                        .or_default()
                        .push(parent.clone());
                }
            }
            TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) => {
                records.insert(name.clone(), fields.clone());
            }
            _ => {}
        }
    }
    TypeInfo {
        variants,
        variant_parents,
        records,
    }
}

/// Run the resolver on all top-level function definitions. Stops after
/// slot resolution — last-use ownership annotation is its own pipeline
/// stage (`ir::pipeline::last_use`) so the two analyses are individually
/// observable and skippable.
pub fn resolve_program(items: &mut [TopLevel]) {
    let type_info = build_type_info(items);
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            resolve_fn(fd, &type_info);
        }
    }
}

/// Resolve a single function definition.
///
/// Single-pass walk that allocates slots, stamps `slot_types`, fills
/// `MatchArm::binding_slots`, and rewrites `Expr::Ident → Expr::
/// Resolved` against a scope stack so pattern bindings shadow per
/// arm — two arms with the same binding name (e.g. `deadline`
/// appearing in both `TaskCreated.deadline: Option<String>` and
/// `DeadlineSet.deadline: String`) get separate slots without
/// the second one's typecheck-narrower type silently overwriting
/// the first one's.
fn resolve_fn(fd: &mut FnDef, type_info: &TypeInfo) {
    let mut state = ResolverState::new(type_info);

    // Params live in the outermost scope and own slots 0..N-1.
    // `declare_param` (not `declare`) so wildcard `_` params still claim
    // a slot — callsites push one value per declared param onto the
    // stack regardless of source name, so `local_count` must match
    // `params.len()` even when some params are unbound.
    state.scopes.push(HashMap::new());
    for (param_name, ty_str) in &fd.params {
        let ty = crate::types::parse_type_str_strict(ty_str).unwrap_or(Type::Invalid);
        state.declare_param(param_name, ty);
    }

    // Walk body — clone, mutate, replace. Same Arc::make_mut cadence as
    // before; rewriting Idents and stamping arm.binding_slots happens
    // in one pass.
    let mut body = fd.body.as_ref().clone();
    state.walk_stmts(body.stmts_mut());
    fd.body = Rc::new(body);

    let next_slot = state.next_slot;
    let last_alloc = state.last_alloc;
    let slot_types = state.slot_types;
    let stmt_binding_slots = state.stmt_binding_slots;
    state.scopes.pop();

    fd.resolution = Some(FnResolution {
        local_count: next_slot,
        local_slots: Rc::new(last_alloc),
        stmt_binding_slots: Rc::new(stmt_binding_slots),
        local_slot_types: Rc::new(slot_types),
        aliased_slots: Rc::new(vec![false; next_slot as usize]),
    });
}

/// Mutable resolver state — scope stack of name→slot maps,
/// next-slot counter, parallel `slot_types` vector. Encapsulates the
/// shared bookkeeping the AST walk needs.
struct ResolverState<'a> {
    type_info: &'a TypeInfo,
    next_slot: u16,
    slot_types: Vec<Type>,
    /// LIFO stack of scopes; innermost last. `walk_stmts` pushes one
    /// scope for the function body, `walk_match_arms` pushes one per
    /// arm (and pops on exit) so pattern bindings shadow only inside
    /// their own arm.
    scopes: Vec<HashMap<String, u16>>,
    /// Mirror of the slot allocator for `FnResolution.local_slots`.
    /// Last allocation per name wins on collision; used by external
    /// consumers (escape analysis, debug paths) that look up a name
    /// post-resolve. Pattern bindings live here too, but call sites
    /// inside a match arm should prefer `MatchArm::binding_slots`
    /// (resolver writes the actual per-arm slot there).
    last_alloc: HashMap<String, u16>,
    /// Slot of each fn-body statement binding in source order —
    /// `u16::MAX` for `_`. Becomes `FnResolution.stmt_binding_slots`,
    /// the per-statement identity the MIR lowering and the alias pass
    /// consume instead of the name-keyed (last-wins) `last_alloc`
    /// (issue #948).
    ///
    /// CONTRACT (producer side): exactly one entry per fn-body
    /// `Stmt::Binding`, pushed in source order. Consumers zip this
    /// positionally against the body's statements and assert the list
    /// drains exactly — any pass that adds, removes, or reorders
    /// statement bindings after resolution MUST rebuild this table, or
    /// every later binding silently pairs with a neighbor's slot.
    stmt_binding_slots: Vec<u16>,
}

impl<'a> ResolverState<'a> {
    fn new(type_info: &'a TypeInfo) -> Self {
        Self {
            type_info,
            next_slot: 0,
            slot_types: Vec::new(),
            scopes: Vec::new(),
            last_alloc: HashMap::new(),
            stmt_binding_slots: Vec::new(),
        }
    }

    /// Allocate a fresh slot, stamp its Aver type, return the index.
    fn alloc(&mut self, ty: Type) -> u16 {
        let idx = self.next_slot;
        self.next_slot += 1;
        self.slot_types.push(ty);
        idx
    }

    /// Declare `name` in the innermost scope. Wildcard (`_`) returns
    /// `u16::MAX` and is *not* declared.
    ///
    /// Always allocates a fresh slot — including when `name` already
    /// exists in an outer scope. That keeps two arms in the same
    /// match sound when they bind the same name to different types
    /// (workflow_engine's `serializeTaskEvent` reuses `deadline` as
    /// `Option<String>` in one arm and `String` in another). Backends
    /// that key off names (Rust, Lean, Dafny — all emit pattern-syntax
    /// directly) keep working unchanged. The two slot-driven backends
    /// (VM and wasm-gc) consult `MatchArm::binding_slots` for the per-
    /// arm fresh slot rather than `FnResolution.local_slots[name]`,
    /// which only carries the last allocation per name.
    fn declare(&mut self, name: &str, ty: Type) -> u16 {
        if name == "_" {
            return u16::MAX;
        }
        let slot = self.alloc(ty);
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), slot);
        }
        self.last_alloc.insert(name.to_string(), slot);
        slot
    }

    /// Like `declare`, but always allocates a slot — including for `_`.
    /// Used for fn parameters: callsites push one value per declared
    /// param onto the stack regardless of source name, so the callee
    /// frame must reserve a slot per param to keep stack/locals aligned.
    /// Wildcard params still skip the scope + `last_alloc` insert (the
    /// body cannot read them).
    fn declare_param(&mut self, name: &str, ty: Type) -> u16 {
        let slot = self.alloc(ty);
        if name != "_" {
            if let Some(scope) = self.scopes.last_mut() {
                scope.insert(name.to_string(), slot);
            }
            self.last_alloc.insert(name.to_string(), slot);
        }
        slot
    }

    /// Inner-first lookup over the scope stack. Returns the closest
    /// enclosing slot for `name` — pattern bindings in the current arm
    /// shadow let-bindings and parameters above.
    fn lookup(&self, name: &str) -> Option<u16> {
        for scope in self.scopes.iter().rev() {
            if let Some(&s) = scope.get(name) {
                return Some(s);
            }
        }
        None
    }

    fn walk_stmts(&mut self, stmts: &mut [Stmt]) {
        for stmt in stmts {
            match stmt {
                Stmt::Binding(name, _annot, expr) => {
                    // Allocate the binding's slot BEFORE walking the
                    // RHS — pattern bindings inside the RHS expression
                    // get later slots, matching the legacy layout (the
                    // slot table observable through `FnResolution.
                    // local_slots` keeps params, then top-level let
                    // bindings, then pattern-introduced bindings).
                    let ty = expr.ty().cloned().unwrap_or(Type::Invalid);
                    let slot = self.declare(name, ty);
                    self.stmt_binding_slots.push(slot);
                    self.walk_expr(expr);
                }
                Stmt::Expr(expr) => self.walk_expr(expr),
            }
        }
    }

    fn walk_expr(&mut self, expr: &mut Spanned<Expr>) {
        match &mut expr.node {
            Expr::Ident(name) => {
                if let Some(slot) = self.lookup(name) {
                    expr.node = Expr::Resolved {
                        slot,
                        name: name.clone(),
                        last_use: AnnotBool(false),
                    };
                }
            }
            Expr::Match { subject, arms } => {
                self.walk_expr(subject);
                let subject_ty = subject.ty().cloned();
                for arm in arms.iter_mut() {
                    self.scopes.push(HashMap::new());
                    let slots = self.allocate_pattern(&arm.pattern, subject_ty.as_ref());
                    let _ = arm.binding_slots.set(slots);
                    self.walk_expr(&mut arm.body);
                    self.scopes.pop();
                }
            }
            Expr::FnCall(func, args) => {
                self.walk_expr(func);
                for arg in args {
                    self.walk_expr(arg);
                }
            }
            Expr::BinOp(_, l, r) => {
                self.walk_expr(l);
                self.walk_expr(r);
            }
            Expr::Neg(inner) => self.walk_expr(inner),
            Expr::Attr(obj, _) => self.walk_expr(obj),
            Expr::ErrorProp(inner) => self.walk_expr(inner),
            Expr::Constructor(_, Some(inner)) => self.walk_expr(inner),
            Expr::Constructor(_, None) => {}
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                for it in items {
                    self.walk_expr(it);
                }
            }
            Expr::MapLiteral(entries) => {
                for (k, v) in entries {
                    self.walk_expr(k);
                    self.walk_expr(v);
                }
            }
            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let StrPart::Parsed(e) = part {
                        self.walk_expr(e);
                    }
                }
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, e) in fields {
                    self.walk_expr(e);
                }
            }
            Expr::RecordUpdate { base, updates, .. } => {
                self.walk_expr(base);
                for (_, e) in updates {
                    self.walk_expr(e);
                }
            }
            Expr::TailCall(boxed) => {
                for a in &mut boxed.args {
                    self.walk_expr(a);
                }
            }
            Expr::Literal(_) | Expr::Resolved { .. } => {}
        }
    }

    /// Allocate fresh slots for every binding the pattern introduces,
    /// declaring each in the innermost scope. Returns the per-pattern
    /// slot list in pattern-position order — that's what the backend
    /// reads via `MatchArm::binding_slots`. Wildcards are present as
    /// `u16::MAX` so the slot list lines up with binding positions.
    fn allocate_pattern(&mut self, pattern: &Pattern, subject_ty: Option<&Type>) -> Vec<u16> {
        match pattern {
            Pattern::Ident(name) => {
                let ty = subject_ty.cloned().unwrap_or(Type::Invalid);
                vec![self.declare(name, ty)]
            }
            Pattern::Cons(head, tail) => {
                let elem_ty = match subject_ty {
                    Some(Type::List(inner)) => (**inner).clone(),
                    _ => Type::Invalid,
                };
                let list_ty = Type::List(Box::new(elem_ty.clone()));
                vec![self.declare(head, elem_ty), self.declare(tail, list_ty)]
            }
            Pattern::Constructor(name, bindings) => {
                let bare = name.rsplit('.').next().unwrap_or(name);
                let parent_hint: Option<String> = match (subject_ty, name.split_once('.')) {
                    (Some(Type::Named { name: parent, .. }), _) => Some(parent.clone()),
                    (_, Some((parent, _))) => Some(parent.to_string()),
                    _ => self
                        .type_info
                        .variant_parents
                        .get(bare)
                        .and_then(|parents| {
                            if parents.len() == 1 {
                                Some(parents[0].clone())
                            } else {
                                None
                            }
                        }),
                };
                let field_tys: Vec<Type> = match (bare, subject_ty) {
                    ("Ok", Some(Type::Result(t, _))) => vec![(**t).clone()],
                    ("Err", Some(Type::Result(_, e))) => vec![(**e).clone()],
                    ("Some", Some(Type::Option(inner))) => vec![(**inner).clone()],
                    ("None", _) => Vec::new(),
                    _ => parent_hint
                        .and_then(|p| self.type_info.variants.get(&(p, bare.to_string())))
                        .map(|fields| {
                            fields
                                .iter()
                                .map(|s| {
                                    crate::types::parse_type_str_strict(s).unwrap_or(Type::Invalid)
                                })
                                .collect()
                        })
                        .unwrap_or_else(|| vec![Type::Invalid; bindings.len()]),
                };
                bindings
                    .iter()
                    .enumerate()
                    .map(|(i, name)| {
                        let ty = field_tys.get(i).cloned().unwrap_or(Type::Invalid);
                        self.declare(name, ty)
                    })
                    .collect()
            }
            Pattern::Tuple(items) => {
                let elem_tys: Vec<Type> = match subject_ty {
                    Some(Type::Tuple(elems)) if elems.len() == items.len() => elems.to_vec(),
                    _ => vec![Type::Invalid; items.len()],
                };
                let mut slots = Vec::new();
                for (item, elem_ty) in items.iter().zip(elem_tys.iter()) {
                    slots.extend(self.allocate_pattern(item, Some(elem_ty)));
                }
                slots
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Shadowing ban (issue #954)
// ---------------------------------------------------------------------------

/// Where a banned binder sits.
#[derive(Clone, Copy, PartialEq)]
enum ShadowBinderKind {
    Param,
    StmtBinding,
    PatternBinder,
}

impl ShadowBinderKind {
    fn label(self) -> &'static str {
        match self {
            ShadowBinderKind::Param => "parameter",
            ShadowBinderKind::StmtBinding => "binding",
            ShadowBinderKind::PatternBinder => "pattern binding",
        }
    }
}

/// What a module-level name is, for the error message.
#[derive(Clone, Copy)]
enum ModuleNameKind {
    Function,
    Operation,
}

/// Reject every binder that reuses a name already visible at its
/// declaration point (issue #954): an enclosing local (parameter,
/// statement binding, or pattern binder along the lexical path), a
/// top-level `fn` or capability `operation` of the same module, or the
/// enclosing function's own name.
///
/// Runs over the same scope structure `resolve_fn` walks — params in
/// the outermost frame, statement bindings in source order, one fresh
/// frame per match arm — so what this refuses is exactly what the slot
/// resolver would have made ambiguous-by-spelling. SIBLING match arms
/// may still reuse a name between them: they are never in each other's
/// scope, and the per-arm `binding_slots` machinery keeps them sound.
/// Cross-module names are always `Module.fn`-qualified, so a bare
/// binder cannot collide with anything outside its own file. `_` is a
/// discard, not a name, and is never checked.
///
/// v1 scope note: this covers function bodies only. Binders inside
/// `verify` blocks (givens, law binders) are deliberately left alone —
/// extending the ban there gets its own census first. Top-level `Stmt`
/// items (module-level bindings, REPL) are outside it too, the same
/// way `resolve_program` leaves them alone: the issue's rule names
/// enclosing locals, module `fn`s and the enclosing fn's own name.
///
/// Every caller reaches this through
/// [`crate::ir::pipeline::typecheck_gate`], which pairs it with the
/// type checker and owns the suppression rule; see that function for
/// why the ban rides the checker instead of `resolve_fn`, and for the
/// one door whose scope is narrower than the item list it checks.
pub fn check_shadowing(items: &[TopLevel]) -> Vec<TypeError> {
    // Top-level fn / operation names of this module, first declaration
    // wins (a duplicate top-level name is its own diagnostic).
    let mut module_fns: HashMap<&str, (usize, ModuleNameKind)> = HashMap::new();
    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                module_fns
                    .entry(fd.name.as_str())
                    .or_insert((fd.line, ModuleNameKind::Function));
            }
            TopLevel::Capability(CapabilityItem::Operation(op)) => {
                module_fns
                    .entry(op.name.as_str())
                    .or_insert((op.line, ModuleNameKind::Operation));
            }
            _ => {}
        }
    }

    let mut errors = Vec::new();
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let mut walker = ShadowWalker {
                fn_name: &fd.name,
                fn_line: fd.line,
                module_fns: &module_fns,
                frames: vec![Vec::new()],
                errors: &mut errors,
            };
            for (param_name, _ty) in &fd.params {
                walker.check_and_declare(param_name, ShadowBinderKind::Param, fd.line);
            }
            walker.walk_stmts(fd.body.stmts());
        }
    }
    errors
}

/// The shadow-check walk. Mirrors `ResolverState`'s scope handling —
/// one outer frame holding params and fn-body statement bindings, one
/// fresh frame per match arm, popped on exit — but carries declaration
/// lines instead of slots, because its output is an error message, not
/// a resolution.
struct ShadowWalker<'a> {
    fn_name: &'a str,
    fn_line: usize,
    module_fns: &'a HashMap<&'a str, (usize, ModuleNameKind)>,
    /// LIFO frames, innermost last: binder name → (kind, line).
    frames: Vec<Vec<(String, ShadowBinderKind, usize)>>,
    errors: &'a mut Vec<TypeError>,
}

impl ShadowWalker<'_> {
    /// Innermost-first lookup, matching `ResolverState::lookup`. Within
    /// a frame the LAST declaration wins, so a duplicate inside one
    /// pattern reports against its own twin rather than something
    /// further out.
    fn lookup(&self, name: &str) -> Option<(ShadowBinderKind, usize)> {
        for frame in self.frames.iter().rev() {
            if let Some((_, kind, line)) = frame.iter().rev().find(|(n, _, _)| n == name) {
                return Some((*kind, *line));
            }
        }
        None
    }

    /// Check one binder against everything visible at its declaration
    /// point, then declare it. `_` is a discard, not a name — never
    /// checked, never declared.
    fn check_and_declare(&mut self, name: &str, kind: ShadowBinderKind, line: usize) {
        if name == "_" {
            return;
        }
        let shadowed: Option<(String, usize)> = if let Some((skind, sline)) = self.lookup(name) {
            Some((format!("{} '{}'", skind.label(), name), sline))
        } else if name == self.fn_name {
            Some((format!("enclosing function '{}'", name), self.fn_line))
        } else if let Some(&(fline, fkind)) = self.module_fns.get(name) {
            let what = match fkind {
                ModuleNameKind::Function => "function",
                ModuleNameKind::Operation => "operation",
            };
            Some((format!("{} '{}'", what, name), fline))
        } else {
            None
        };
        if let Some((what, sline)) = shadowed {
            self.errors.push(TypeError {
                message: format!(
                    "the {} '{}' shadows the {} defined at line {}; every name means \
                     one thing in its scope — rename one of them",
                    kind.label(),
                    name,
                    what,
                    sline
                ),
                line,
                col: 0,
                origin: None,
                secondary: None,
            });
        }
        self.frames
            .last_mut()
            .expect("shadow walk always holds the fn frame")
            .push((name.to_string(), kind, line));
    }

    fn walk_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            match stmt {
                Stmt::Binding(name, _annot, expr) => {
                    // The RHS is evaluated before the name is in scope,
                    // but check-then-declare in one step is equivalent
                    // here: a same-named binder INSIDE the RHS would be
                    // an arm binder in its own frame, reported against
                    // this binding either way.
                    self.check_and_declare(name, ShadowBinderKind::StmtBinding, expr.line);
                    self.walk_expr(expr);
                }
                Stmt::Expr(expr) => self.walk_expr(expr),
            }
        }
    }

    /// Pattern binders in pattern-position order, mirroring
    /// `ResolverState::allocate_pattern`.
    fn pattern_binders(pattern: &Pattern, out: &mut Vec<String>) {
        match pattern {
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
            Pattern::Ident(name) => out.push(name.clone()),
            Pattern::Cons(head, tail) => {
                out.push(head.clone());
                out.push(tail.clone());
            }
            Pattern::Constructor(_, bindings) => out.extend(bindings.iter().cloned()),
            Pattern::Tuple(items) => {
                for item in items {
                    Self::pattern_binders(item, out);
                }
            }
        }
    }

    fn walk_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Match { subject, arms } => {
                self.walk_expr(subject);
                for arm in arms {
                    let mut binders = Vec::new();
                    Self::pattern_binders(&arm.pattern, &mut binders);
                    self.frames.push(Vec::new());
                    for binder in &binders {
                        // Patterns carry no span of their own; the arm
                        // body's line is the closest carried position.
                        self.check_and_declare(
                            binder,
                            ShadowBinderKind::PatternBinder,
                            arm.body.line,
                        );
                    }
                    self.walk_expr(&arm.body);
                    self.frames.pop();
                }
            }
            Expr::FnCall(func, args) => {
                self.walk_expr(func);
                for arg in args {
                    self.walk_expr(arg);
                }
            }
            Expr::BinOp(_, l, r) => {
                self.walk_expr(l);
                self.walk_expr(r);
            }
            Expr::Neg(inner) | Expr::Attr(inner, _) | Expr::ErrorProp(inner) => {
                self.walk_expr(inner)
            }
            Expr::Constructor(_, Some(inner)) => self.walk_expr(inner),
            Expr::Constructor(_, None) => {}
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                for item in items {
                    self.walk_expr(item);
                }
            }
            Expr::MapLiteral(entries) => {
                for (k, v) in entries {
                    self.walk_expr(k);
                    self.walk_expr(v);
                }
            }
            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let StrPart::Parsed(inner) = part {
                        self.walk_expr(inner);
                    }
                }
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, e) in fields {
                    self.walk_expr(e);
                }
            }
            Expr::RecordUpdate { base, updates, .. } => {
                self.walk_expr(base);
                for (_, e) in updates {
                    self.walk_expr(e);
                }
            }
            Expr::TailCall(tc) => {
                for arg in &tc.args {
                    self.walk_expr(arg);
                }
            }
            Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_param_to_slot() {
        let mut fd = FnDef {
            name: "add".to_string(),
            line: 1,
            params: vec![
                ("a".to_string(), "Int".to_string()),
                ("b".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::BinOp(
                BinOp::Add,
                Box::new(Spanned::bare(Expr::Ident("a".to_string()))),
                Box::new(Spanned::bare(Expr::Ident("b".to_string()))),
            )))),
            resolution: None,
        };
        resolve_fn(
            &mut fd,
            &TypeInfo {
                variants: HashMap::new(),
                variant_parents: HashMap::new(),
                records: HashMap::new(),
            },
        );
        let res = fd.resolution.as_ref().unwrap();
        assert_eq!(res.local_slots["a"], 0);
        assert_eq!(res.local_slots["b"], 1);
        assert_eq!(res.local_count, 2);

        match fd.body.tail_expr() {
            Some(Spanned {
                node: Expr::BinOp(_, left, right),
                ..
            }) => {
                assert_eq!(
                    left.node,
                    Expr::Resolved {
                        slot: 0,
                        name: "a".to_string(),
                        last_use: AnnotBool(false)
                    }
                );
                assert_eq!(
                    right.node,
                    Expr::Resolved {
                        slot: 1,
                        name: "b".to_string(),
                        last_use: AnnotBool(false)
                    }
                );
            }
            other => panic!("unexpected body: {:?}", other),
        }
    }

    #[test]
    fn wildcard_param_still_claims_slot() {
        // AFL nightly fuzz_verify_runner regression: `fn f(_: Int)` used
        // to leave `local_count == 0` while callsites still pushed 1
        // arg, so CALL_KNOWN saw `local_count(0) - argc(1)` and the VM
        // panicked on unsigned-sub overflow. `declare_param` now
        // always allocates the slot — wildcard name skips the scope
        // map insert, but the slot space matches `params.len()`.
        let mut fd = FnDef {
            name: "ignore".to_string(),
            line: 1,
            params: vec![("_".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Literal(
                Literal::Int(42),
            )))),
            resolution: None,
        };
        resolve_fn(
            &mut fd,
            &TypeInfo {
                variants: HashMap::new(),
                variant_parents: HashMap::new(),
                records: HashMap::new(),
            },
        );
        let res = fd.resolution.as_ref().unwrap();
        assert_eq!(res.local_count, 1, "wildcard param must claim a slot");
        assert!(
            !res.local_slots.contains_key("_"),
            "wildcard param must not bind a readable name"
        );
    }

    #[test]
    fn leaves_globals_as_ident() {
        let mut fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::FnCall(
                Box::new(Spanned::bare(Expr::Ident("Console".to_string()))),
                vec![Spanned::bare(Expr::Ident("x".to_string()))],
            )))),
            resolution: None,
        };
        resolve_fn(
            &mut fd,
            &TypeInfo {
                variants: HashMap::new(),
                variant_parents: HashMap::new(),
                records: HashMap::new(),
            },
        );
        match fd.body.tail_expr() {
            Some(Spanned {
                node: Expr::FnCall(func, args),
                ..
            }) => {
                assert_eq!(func.node, Expr::Ident("Console".to_string()));
                assert_eq!(
                    args[0].node,
                    Expr::Resolved {
                        slot: 0,
                        name: "x".to_string(),
                        last_use: AnnotBool(false)
                    }
                );
            }
            other => panic!("unexpected body: {:?}", other),
        }
    }

    #[test]
    fn resolves_val_in_block_body() {
        let mut fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Block(vec![
                Stmt::Binding(
                    "y".to_string(),
                    None,
                    Spanned::bare(Expr::BinOp(
                        BinOp::Add,
                        Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
                        Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                    )),
                ),
                Stmt::Expr(Spanned::bare(Expr::Ident("y".to_string()))),
            ])),
            resolution: None,
        };
        resolve_fn(
            &mut fd,
            &TypeInfo {
                variants: HashMap::new(),
                variant_parents: HashMap::new(),
                records: HashMap::new(),
            },
        );
        let res = fd.resolution.as_ref().unwrap();
        assert_eq!(res.local_slots["x"], 0);
        assert_eq!(res.local_slots["y"], 1);
        assert_eq!(res.local_count, 2);

        let stmts = fd.body.stmts();
        // val y = x + 1  →  val y = Resolved(0,0) + 1
        match &stmts[0] {
            Stmt::Binding(
                _,
                _,
                Spanned {
                    node: Expr::BinOp(_, left, _),
                    ..
                },
            ) => {
                assert_eq!(
                    left.node,
                    Expr::Resolved {
                        slot: 0,
                        name: "x".to_string(),
                        last_use: AnnotBool(false)
                    }
                );
            }
            other => panic!("unexpected stmt: {:?}", other),
        }
        // y  →  Resolved(0,1)
        match &stmts[1] {
            Stmt::Expr(Spanned {
                node: Expr::Resolved { slot: 1, .. },
                ..
            }) => {}
            other => panic!("unexpected stmt: {:?}", other),
        }
    }

    #[test]
    fn resolves_match_pattern_bindings() {
        // fn f(x: Int) -> Int / match x: Result.Ok(v) -> v, _ -> 0
        let mut fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::new(
                Expr::Match {
                    subject: Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Constructor(
                                "Result.Ok".to_string(),
                                vec!["v".to_string()],
                            ),
                            body: Box::new(Spanned::bare(Expr::Ident("v".to_string()))),
                            binding_slots: std::sync::OnceLock::new(),
                        },
                        MatchArm {
                            pattern: Pattern::Wildcard,
                            body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                            binding_slots: std::sync::OnceLock::new(),
                        },
                    ],
                },
                1,
            ))),
            resolution: None,
        };
        resolve_fn(
            &mut fd,
            &TypeInfo {
                variants: HashMap::new(),
                variant_parents: HashMap::new(),
                records: HashMap::new(),
            },
        );
        let res = fd.resolution.as_ref().unwrap();
        // x=0, v=1
        assert_eq!(res.local_slots["v"], 1);

        match fd.body.tail_expr() {
            Some(Spanned {
                node: Expr::Match { arms, .. },
                ..
            }) => {
                assert_eq!(
                    arms[0].body.node,
                    Expr::Resolved {
                        slot: 1,
                        name: "v".to_string(),
                        last_use: AnnotBool(false)
                    }
                );
            }
            other => panic!("unexpected body: {:?}", other),
        }
    }

    #[test]
    fn resolves_match_pattern_bindings_inside_binding_initializer() {
        let mut fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Block(vec![
                Stmt::Binding(
                    "result".to_string(),
                    None,
                    Spanned::bare(Expr::Match {
                        subject: Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
                        arms: vec![
                            MatchArm {
                                pattern: Pattern::Constructor(
                                    "Option.Some".to_string(),
                                    vec!["v".to_string()],
                                ),
                                body: Box::new(Spanned::bare(Expr::Ident("v".to_string()))),
                                binding_slots: std::sync::OnceLock::new(),
                            },
                            MatchArm {
                                pattern: Pattern::Wildcard,
                                body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                                binding_slots: std::sync::OnceLock::new(),
                            },
                        ],
                    }),
                ),
                Stmt::Expr(Spanned::bare(Expr::Ident("result".to_string()))),
            ])),
            resolution: None,
        };

        resolve_fn(
            &mut fd,
            &TypeInfo {
                variants: HashMap::new(),
                variant_parents: HashMap::new(),
                records: HashMap::new(),
            },
        );
        let res = fd.resolution.as_ref().unwrap();
        assert_eq!(res.local_slots["x"], 0);
        assert_eq!(res.local_slots["result"], 1);
        assert_eq!(res.local_slots["v"], 2);

        let stmts = fd.body.stmts();
        match &stmts[0] {
            Stmt::Binding(
                _,
                _,
                Spanned {
                    node: Expr::Match { arms, .. },
                    ..
                },
            ) => {
                assert_eq!(
                    arms[0].body.node,
                    Expr::Resolved {
                        slot: 2,
                        name: "v".to_string(),
                        last_use: AnnotBool(false)
                    }
                );
            }
            other => panic!("unexpected stmt: {:?}", other),
        }

        match &stmts[1] {
            Stmt::Expr(Spanned {
                node: Expr::Resolved { slot: 1, .. },
                ..
            }) => {}
            other => panic!("unexpected stmt: {:?}", other),
        }
    }

    /// Issue #949's contract, kept discriminating after the shadowing
    /// ban took the user-written trigger away.
    ///
    /// The ban runs at the pipeline's typecheck gate; `resolve_fn` does
    /// not call it. So an AST BUILT here — not parsed — is in exactly
    /// the position compiler-synthesized code is in, and compiler-
    /// synthesized code still creates colliding spellings: driver-step
    /// inlining renames binders into a fresh namespace, the fusion
    /// passes synthesize variants. This is the shape the positional
    /// table exists for.
    ///
    /// The fn is `f(x) { v = x + 1; r = match x { Some(v) -> v; _ -> 0 };
    /// v }` — a statement binding `v` and, inside the next statement's
    /// match, an arm binder spelled `v` too. Slots go x=0, v(stmt)=1,
    /// r=2, v(arm)=3, and the two identities disagree: the name map
    /// answers 3 (last allocation wins), the positional table answers
    /// 1. Every consumer must read the second one — reading the first
    /// is issue #948 exactly, the `let` writing the binder's slot while
    /// every read of the statement binding hits an uninitialized one.
    #[test]
    fn a_statement_binding_keeps_its_slot_when_a_later_binder_reuses_the_spelling() {
        let mut fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Block(vec![
                Stmt::Binding(
                    "v".to_string(),
                    None,
                    Spanned::bare(Expr::BinOp(
                        BinOp::Add,
                        Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
                        Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
                    )),
                ),
                Stmt::Binding(
                    "r".to_string(),
                    None,
                    Spanned::bare(Expr::Match {
                        subject: Box::new(Spanned::bare(Expr::Ident("x".to_string()))),
                        arms: vec![
                            MatchArm {
                                pattern: Pattern::Constructor(
                                    "Option.Some".to_string(),
                                    vec!["v".to_string()],
                                ),
                                body: Box::new(Spanned::bare(Expr::Ident("v".to_string()))),
                                binding_slots: std::sync::OnceLock::new(),
                            },
                            MatchArm {
                                pattern: Pattern::Wildcard,
                                body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                                binding_slots: std::sync::OnceLock::new(),
                            },
                        ],
                    }),
                ),
                Stmt::Expr(Spanned::bare(Expr::Ident("v".to_string()))),
            ])),
            resolution: None,
        };
        resolve_fn(
            &mut fd,
            &TypeInfo {
                variants: HashMap::new(),
                variant_parents: HashMap::new(),
                records: HashMap::new(),
            },
        );
        let res = fd.resolution.as_ref().expect("resolution stamped");

        assert_eq!(
            res.stmt_binding_slots.as_slice(),
            &[1, 2],
            "the positional table must carry each statement binding's OWN slot, \
             in source order"
        );
        assert_eq!(
            res.local_slots["v"], 3,
            "the name map is last-allocation-wins: the ARM binder owns the \
             spelling `v` there"
        );
        assert_ne!(
            res.local_slots["v"], res.stmt_binding_slots[0],
            "the two identities must actually disagree here, or this test is \
             not testing anything"
        );

        // And the body agrees with the positional table, not the name
        // map: the tail read of `v` is outside the arm frame, so it is
        // the statement binding's slot 1.
        match &fd.body.stmts()[2] {
            Stmt::Expr(Spanned {
                node: Expr::Resolved { slot, .. },
                ..
            }) => assert_eq!(
                *slot, 1,
                "the tail read of `v` must be the statement binding, not the \
                 arm binder that took over its spelling in the name map"
            ),
            other => panic!("unexpected tail stmt: {:?}", other),
        }
    }

    // ── Shadowing ban (issue #954) ──────────────────────────────────
    //
    // The matrix below is mechanical: one row per reachable
    // (binder position × shadowed-thing kind) cell, each row a
    // complete program plus the exact error and the binder's line.
    // Unreachable cells are absent by construction — a parameter
    // cannot shadow a local (parameters come first), and a statement
    // binding cannot shadow a pattern binder (statement bindings
    // exist only at fn-body top level, outside every arm frame).
    //
    // Five present rows are honest about being UNIT-LEVEL ONLY — the
    // walk answers for them, but no user sees this message through a
    // front door today, because something else refuses the program
    // first and the ban is suppressed once typecheck has failed:
    //
    // - `stmt-binding-over-param` and `stmt-binding-over-stmt-binding`
    //   are pre-empted by the flow checker's "'x' is already defined
    //   in 'f'" (`src/types/checker/flow.rs`, `check_stmts`).
    // - the three `*-over-operation` rows need a capability
    //   `operation` in the module, and any module carrying one is
    //   refused outright with `capability-unsupported` (issue #893)
    //   until capability support lands. They are FORWARD-LOOKING: they
    //   pin what the walk must say the day operations register.
    //
    // They stay because the walk's answer is what the ban IS; when
    // either pre-emption lifts, these rows are already the contract.

    fn shadow_errors(src: &str) -> Vec<crate::types::checker::TypeError> {
        let items = crate::source::parse_source(src).expect("ban-matrix program must parse");
        check_shadowing(&items)
    }

    const S: &str = "every name means one thing in its scope — rename one of them";

    #[test]
    fn shadow_ban_matrix_rejects_every_banned_cell() {
        let rows: &[(&str, String, usize, String)] = &[
            (
                "param-over-param",
                "fn f(a: Int, a: Int) -> Int\n    a\n".into(),
                1,
                format!("the parameter 'a' shadows the parameter 'a' defined at line 1; {S}"),
            ),
            (
                "param-over-own-fn-name",
                "fn f(f: Int) -> Int\n    f\n".into(),
                1,
                format!("the parameter 'f' shadows the enclosing function 'f' defined at line 1; {S}"),
            ),
            (
                "param-over-module-fn",
                "fn g(x: Int) -> Int\n    x\n\nfn f(g: Int) -> Int\n    g\n".into(),
                4,
                format!("the parameter 'g' shadows the function 'g' defined at line 1; {S}"),
            ),
            (
                "param-over-operation",
                "operation size() -> Int\n\nfn f(size: Int) -> Int\n    size\n".into(),
                3,
                format!("the parameter 'size' shadows the operation 'size' defined at line 1; {S}"),
            ),
            (
                "stmt-binding-over-param",
                "fn f(a: Int) -> Int\n    a = 2\n    a\n".into(),
                2,
                format!("the binding 'a' shadows the parameter 'a' defined at line 1; {S}"),
            ),
            (
                "stmt-binding-over-stmt-binding",
                "fn f(x: Int) -> Int\n    a = x\n    a = 3\n    a\n".into(),
                3,
                format!("the binding 'a' shadows the binding 'a' defined at line 2; {S}"),
            ),
            (
                "stmt-binding-over-module-fn",
                "fn g(x: Int) -> Int\n    x\n\nfn f(x: Int) -> Int\n    g = x\n    g\n".into(),
                5,
                format!("the binding 'g' shadows the function 'g' defined at line 1; {S}"),
            ),
            (
                "stmt-binding-over-own-fn-name",
                "fn f(x: Int) -> Int\n    f = x\n    f\n".into(),
                2,
                format!("the binding 'f' shadows the enclosing function 'f' defined at line 1; {S}"),
            ),
            (
                "stmt-binding-over-operation",
                "operation size() -> Int\n\nfn f(x: Int) -> Int\n    size = x\n    size\n".into(),
                4,
                format!("the binding 'size' shadows the operation 'size' defined at line 1; {S}"),
            ),
            (
                "pattern-binder-over-param",
                "fn f(v: Int) -> Int\n    match Option.Some(v)\n        Option.Some(v) -> v\n        Option.None -> 0\n".into(),
                3,
                format!("the pattern binding 'v' shadows the parameter 'v' defined at line 1; {S}"),
            ),
            (
                "pattern-binder-over-stmt-binding",
                "fn f(x: Int) -> Int\n    v = x * 2\n    match Option.Some(v)\n        Option.Some(v) -> v\n        Option.None -> 0\n".into(),
                4,
                format!("the pattern binding 'v' shadows the binding 'v' defined at line 2; {S}"),
            ),
            (
                "pattern-binder-over-enclosing-pattern-binder",
                "fn f(x: Int) -> Int\n    match Option.Some(x)\n        Option.Some(v) -> match Option.Some(v)\n            Option.Some(v) -> v\n            Option.None -> 0\n        Option.None -> 0\n".into(),
                4,
                format!("the pattern binding 'v' shadows the pattern binding 'v' defined at line 3; {S}"),
            ),
            (
                "pattern-binder-duplicated-inside-one-pattern",
                "fn f(x: Int) -> Int\n    match (x, x)\n        (a, a) -> a\n".into(),
                3,
                format!("the pattern binding 'a' shadows the pattern binding 'a' defined at line 3; {S}"),
            ),
            (
                "pattern-binder-over-module-fn",
                "fn dbl(n: Int) -> Int\n    n * 2\n\nfn probe(x: Int) -> Int\n    match Option.Some(x)\n        Option.Some(dbl) -> dbl(3)\n        Option.None -> 0\n".into(),
                6,
                format!("the pattern binding 'dbl' shadows the function 'dbl' defined at line 1; {S}"),
            ),
            (
                "pattern-binder-over-own-fn-name",
                "fn f(x: Int) -> Int\n    match Option.Some(x)\n        Option.Some(f) -> f\n        Option.None -> 0\n".into(),
                3,
                format!("the pattern binding 'f' shadows the enclosing function 'f' defined at line 1; {S}"),
            ),
            (
                "cons-head-over-stmt-binding",
                "fn f(xs: List<Int>) -> Int\n    h = 1\n    match xs\n        [] -> h\n        [h, ..t] -> h\n".into(),
                5,
                format!("the pattern binding 'h' shadows the binding 'h' defined at line 2; {S}"),
            ),
            (
                "cons-tail-over-param",
                "fn f(t: Int, xs: List<Int>) -> Int\n    match xs\n        [] -> t\n        [h, ..t] -> h\n".into(),
                4,
                format!("the pattern binding 't' shadows the parameter 't' defined at line 1; {S}"),
            ),
            (
                "pattern-binder-over-operation",
                "operation size() -> Int\n\nfn f(x: Int) -> Int\n    match Option.Some(x)\n        Option.Some(size) -> size\n        Option.None -> 0\n".into(),
                5,
                format!("the pattern binding 'size' shadows the operation 'size' defined at line 1; {S}"),
            ),
        ];
        for (label, src, line, want) in rows {
            let errs = shadow_errors(src);
            assert_eq!(
                errs.len(),
                1,
                "{label}: expected exactly one error, got {:?}",
                errs.iter().map(|e| &e.message).collect::<Vec<_>>()
            );
            assert_eq!(&errs[0].message, want, "{label}");
            assert_eq!(
                errs[0].line, *line,
                "{label}: the error must point at the binder"
            );
        }
    }

    #[test]
    fn shadow_ban_leaves_legal_reuse_alone() {
        let controls: &[(&str, &str)] = &[
            (
                "sibling-arms-may-reuse-a-name",
                "type Shape\n    Circle(Int)\n    Square(Int)\n\nfn eval(p: Shape) -> Int\n    match p\n        Shape.Circle(n) -> n + 1\n        Shape.Square(n) -> n * 10\n",
            ),
            (
                "underscore-is-a-discard-not-a-name",
                "fn f(_: Int, _: Int) -> Int\n    match (1, 2)\n        (_, _) -> 0\n",
            ),
            (
                "two-fns-may-reuse-a-param-name",
                "fn f(x: Int) -> Int\n    x\n\nfn g(x: Int) -> Int\n    x\n",
            ),
            (
                "sequential-matches-may-reuse-a-binder",
                "fn f(x: Int) -> Int\n    a = match Option.Some(x)\n        Option.Some(v) -> v\n        Option.None -> 0\n    b = match Option.Some(x)\n        Option.Some(v) -> v\n        Option.None -> 0\n    a + b\n",
            ),
            (
                // Binders inside verify blocks are OUT of the v1 ban —
                // a given may spell a module fn's name (extending the
                // rule there gets its own census first, issue #954).
                "verify-given-is-out-of-v1-scope",
                "fn keys(m: Map<Float, Int>) -> List<Float>\n    Map.keys(m)\n\nfn howMany(counted: List<Float>) -> Int\n    List.len(counted)\n\nverify howMany law countsThem\n    given keys: List<Float> = [[1.0], [1.0, 2.0]]\n    howMany(keys) => List.len(keys)\n",
            ),
            (
                "a-name-not-in-scope-is-free",
                "fn f(x: Int) -> Int\n    keys = x\n    keys\n",
            ),
        ];
        for (label, src) in controls {
            let errs = shadow_errors(src);
            assert!(
                errs.is_empty(),
                "{label}: expected no error, got {:?}",
                errs.iter().map(|e| &e.message).collect::<Vec<_>>()
            );
        }
    }
}
