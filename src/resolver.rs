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
    state.scopes.pop();

    fd.resolution = Some(FnResolution {
        local_count: next_slot,
        local_slots: Rc::new(last_alloc),
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
}

impl<'a> ResolverState<'a> {
    fn new(type_info: &'a TypeInfo) -> Self {
        Self {
            type_info,
            next_slot: 0,
            slot_types: Vec::new(),
            scopes: Vec::new(),
            last_alloc: HashMap::new(),
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
                    self.declare(name, ty);
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
}
