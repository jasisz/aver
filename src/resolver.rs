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
            TopLevel::TypeDef(TypeDef::Sum { name: parent, variants: vs, .. }) => {
                for v in vs {
                    variants
                        .insert((parent.clone(), v.name.clone()), v.fields.clone());
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
fn resolve_fn(fd: &mut FnDef, type_info: &TypeInfo) {
    let mut local_slots: HashMap<String, u16> = HashMap::new();
    let mut next_slot: u16 = 0;

    // Params get slots 0..N-1
    for (param_name, _) in &fd.params {
        local_slots.insert(param_name.clone(), next_slot);
        next_slot += 1;
    }

    // Scan body for val/var bindings to pre-allocate slots
    collect_binding_slots(fd.body.stmts(), &mut local_slots, &mut next_slot);

    // Compute the per-slot Aver type. Runs *after* slot allocation
    // (so we know how many entries we need) and *before* `resolve_stmts`
    // rewrites `Expr::Ident` into `Expr::Resolved` (so binding-name
    // lookups still see the raw names, though stamps survive either
    // way). Default `Type::Invalid` marks slots the body never assigns
    // to — backends typically skip those.
    let mut slot_types: Vec<Type> = vec![Type::Invalid; next_slot as usize];
    for (param_name, ty_str) in &fd.params {
        if let Some(&slot) = local_slots.get(param_name) {
            slot_types[slot as usize] =
                crate::types::parse_type_str_strict(ty_str).unwrap_or(Type::Invalid);
        }
    }
    compute_stmts_slot_types(fd.body.stmts(), &local_slots, type_info, &mut slot_types);

    // Resolve expressions in the body
    let mut body = fd.body.as_ref().clone();
    resolve_stmts(body.stmts_mut(), &local_slots);
    fd.body = Rc::new(body);

    fd.resolution = Some(FnResolution {
        local_count: next_slot,
        local_slots: Rc::new(local_slots),
        local_slot_types: Rc::new(slot_types),
    });
}

/// Walk every binding-introducing site in a statement list and stamp
/// `slot_types[slot]` with the binding's Aver type. Stmt-level `let`
/// bindings pull the type from the producer expression's stamp; match-
/// arm pattern bindings pull from the subject's stamp + the pattern
/// shape.
fn compute_stmts_slot_types(
    stmts: &[Stmt],
    local_slots: &HashMap<String, u16>,
    type_info: &TypeInfo,
    slot_types: &mut [Type],
) {
    for stmt in stmts {
        match stmt {
            Stmt::Binding(name, _annot, expr) => {
                if let Some(&slot) = local_slots.get(name)
                    && let Some(ty) = expr.ty()
                {
                    slot_types[slot as usize] = ty.clone();
                }
                compute_expr_slot_types(expr, local_slots, type_info, slot_types);
            }
            Stmt::Expr(expr) => {
                compute_expr_slot_types(expr, local_slots, type_info, slot_types);
            }
        }
    }
}

fn compute_expr_slot_types(
    expr: &Spanned<Expr>,
    local_slots: &HashMap<String, u16>,
    type_info: &TypeInfo,
    slot_types: &mut [Type],
) {
    match &expr.node {
        Expr::Match { subject, arms } => {
            compute_expr_slot_types(subject, local_slots, type_info, slot_types);
            let subject_ty = subject.ty().cloned();
            for arm in arms {
                walk_pattern_bindings(
                    &arm.pattern,
                    subject_ty.as_ref(),
                    local_slots,
                    type_info,
                    slot_types,
                );
                compute_expr_slot_types(&arm.body, local_slots, type_info, slot_types);
            }
        }
        Expr::BinOp(_, l, r) => {
            compute_expr_slot_types(l, local_slots, type_info, slot_types);
            compute_expr_slot_types(r, local_slots, type_info, slot_types);
        }
        Expr::FnCall(callee, args) => {
            compute_expr_slot_types(callee, local_slots, type_info, slot_types);
            for a in args {
                compute_expr_slot_types(a, local_slots, type_info, slot_types);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                compute_expr_slot_types(a, local_slots, type_info, slot_types);
            }
        }
        Expr::ErrorProp(inner) => {
            compute_expr_slot_types(inner, local_slots, type_info, slot_types);
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                compute_expr_slot_types(p, local_slots, type_info, slot_types);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for it in items {
                compute_expr_slot_types(it, local_slots, type_info, slot_types);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                compute_expr_slot_types(k, local_slots, type_info, slot_types);
                compute_expr_slot_types(v, local_slots, type_info, slot_types);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let StrPart::Parsed(inner) = p {
                    compute_expr_slot_types(inner, local_slots, type_info, slot_types);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                compute_expr_slot_types(e, local_slots, type_info, slot_types);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            compute_expr_slot_types(base, local_slots, type_info, slot_types);
            for (_, e) in updates {
                compute_expr_slot_types(e, local_slots, type_info, slot_types);
            }
        }
        Expr::Attr(obj, _) => {
            compute_expr_slot_types(obj, local_slots, type_info, slot_types);
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

/// Recursive descent over a pattern, stamping each binding's slot with
/// the correct Aver type derived from the subject's type + the pattern
/// shape. Built-in shapes (Result, Option, List, Tuple) come from
/// `Type` constructors; user-declared variants come from `TypeInfo`.
fn walk_pattern_bindings(
    pattern: &Pattern,
    subject_ty: Option<&Type>,
    local_slots: &HashMap<String, u16>,
    type_info: &TypeInfo,
    slot_types: &mut [Type],
) {
    fn store(name: &str, ty: Type, local_slots: &HashMap<String, u16>, slot_types: &mut [Type]) {
        if name == "_" {
            return;
        }
        if let Some(&slot) = local_slots.get(name) {
            // First-occurrence wins; the resolver assigns a slot only
            // once per name and reuses it across arms, so we shouldn't
            // overwrite later occurrences with a different (possibly
            // less precise) type.
            if matches!(slot_types[slot as usize], Type::Invalid) {
                slot_types[slot as usize] = ty;
            }
        }
    }
    match pattern {
        Pattern::Ident(name) => {
            if let Some(ty) = subject_ty {
                store(name, ty.clone(), local_slots, slot_types);
            }
        }
        Pattern::Cons(head, tail) => {
            // `[head, ..tail]` — head: T, tail: List<T>.
            if let Some(Type::List(inner)) = subject_ty {
                store(head, (**inner).clone(), local_slots, slot_types);
                store(
                    tail,
                    Type::List(inner.clone()),
                    local_slots,
                    slot_types,
                );
            }
        }
        Pattern::Tuple(items) => {
            // `(a, b, c) -> ...` — each element gets the matching tuple
            // field type. Per-item recursion handles nested patterns
            // (e.g. `Pattern::Constructor` inside a tuple, used by
            // multi-arm tuple-of-Ok/Err matches).
            if let Some(Type::Tuple(elem_tys)) = subject_ty
                && elem_tys.len() == items.len()
            {
                for (pat, ty) in items.iter().zip(elem_tys.iter()) {
                    walk_pattern_bindings(pat, Some(ty), local_slots, type_info, slot_types);
                }
            }
        }
        Pattern::Constructor(name, bindings) => {
            let bare = name.rsplit('.').next().unwrap_or(name);
            // Built-in shapes — Result / Option / Wrapper.
            match (bare, subject_ty) {
                ("Ok", Some(Type::Result(t, _))) => {
                    if let Some(b) = bindings.first() {
                        store(b, (**t).clone(), local_slots, slot_types);
                    }
                }
                ("Err", Some(Type::Result(_, e))) => {
                    if let Some(b) = bindings.first() {
                        store(b, (**e).clone(), local_slots, slot_types);
                    }
                }
                ("Some", Some(Type::Option(inner))) => {
                    if let Some(b) = bindings.first() {
                        store(b, (**inner).clone(), local_slots, slot_types);
                    }
                }
                ("None", _) => { /* nullary; no bindings */ }
                _ => {
                    // User-declared variant — look up the parent sum's
                    // declared field types via the cross-fn registry.
                    // Disambiguate by subject type when the same bare
                    // variant name is shared across multiple sumtypes
                    // (e.g. `Query.ProviderSummary(String)` vs
                    // `QueryOutput.ProviderSummary(ProviderSummary)`).
                    let parent_hint: Option<String> = match (subject_ty, name.split_once('.')) {
                        (Some(Type::Named(parent)), _) => Some(parent.clone()),
                        (_, Some((parent, _))) => Some(parent.to_string()),
                        _ => type_info
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
                    let fields = parent_hint
                        .and_then(|p| type_info.variants.get(&(p, bare.to_string())));
                    if let Some(fields) = fields {
                        for (binding, field_ty_str) in bindings.iter().zip(fields.iter()) {
                            let field_ty = crate::types::parse_type_str_strict(field_ty_str)
                                .unwrap_or(Type::Invalid);
                            store(binding, field_ty, local_slots, slot_types);
                        }
                    }
                }
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
}

/// Collect all binding names from a statement list and assign slots.
/// This handles match arms recursively (pattern bindings get slots too).
fn collect_binding_slots(
    stmts: &[Stmt],
    local_slots: &mut HashMap<String, u16>,
    next_slot: &mut u16,
) {
    for stmt in stmts {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                if !local_slots.contains_key(name) {
                    local_slots.insert(name.clone(), *next_slot);
                    *next_slot += 1;
                }
                collect_expr_bindings(expr, local_slots, next_slot);
            }
            Stmt::Expr(expr) => {
                collect_expr_bindings(expr, local_slots, next_slot);
            }
        }
    }
}

/// Collect pattern bindings from match expressions inside an expression tree.
fn collect_expr_bindings(
    expr: &Spanned<Expr>,
    local_slots: &mut HashMap<String, u16>,
    next_slot: &mut u16,
) {
    match &expr.node {
        Expr::Match { subject, arms } => {
            collect_expr_bindings(subject, local_slots, next_slot);
            for arm in arms {
                collect_pattern_bindings(&arm.pattern, local_slots, next_slot);
                collect_expr_bindings(&arm.body, local_slots, next_slot);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_expr_bindings(left, local_slots, next_slot);
            collect_expr_bindings(right, local_slots, next_slot);
        }
        Expr::FnCall(func, args) => {
            collect_expr_bindings(func, local_slots, next_slot);
            for arg in args {
                collect_expr_bindings(arg, local_slots, next_slot);
            }
        }
        Expr::ErrorProp(inner) => {
            collect_expr_bindings(inner, local_slots, next_slot);
        }
        Expr::Constructor(_, Some(inner)) => {
            collect_expr_bindings(inner, local_slots, next_slot);
        }
        Expr::List(elements) => {
            for elem in elements {
                collect_expr_bindings(elem, local_slots, next_slot);
            }
        }
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_expr_bindings(item, local_slots, next_slot);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_expr_bindings(key, local_slots, next_slot);
                collect_expr_bindings(value, local_slots, next_slot);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(e) = part {
                    collect_expr_bindings(e, local_slots, next_slot);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_expr_bindings(expr, local_slots, next_slot);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_expr_bindings(base, local_slots, next_slot);
            for (_, expr) in updates {
                collect_expr_bindings(expr, local_slots, next_slot);
            }
        }
        Expr::Attr(obj, _) => {
            collect_expr_bindings(obj, local_slots, next_slot);
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                collect_expr_bindings(arg, local_slots, next_slot);
            }
        }
        // Leaves — no bindings to collect
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

/// Assign slots for names introduced by a pattern.
fn collect_pattern_bindings(
    pattern: &Pattern,
    local_slots: &mut HashMap<String, u16>,
    next_slot: &mut u16,
) {
    match pattern {
        Pattern::Ident(name) => {
            if !local_slots.contains_key(name) {
                local_slots.insert(name.clone(), *next_slot);
                *next_slot += 1;
            }
        }
        Pattern::Cons(head, tail) => {
            for name in [head, tail] {
                if name != "_" && !local_slots.contains_key(name) {
                    local_slots.insert(name.clone(), *next_slot);
                    *next_slot += 1;
                }
            }
        }
        Pattern::Constructor(_, bindings) => {
            for name in bindings {
                if name != "_" && !local_slots.contains_key(name) {
                    local_slots.insert(name.clone(), *next_slot);
                    *next_slot += 1;
                }
            }
        }
        Pattern::Tuple(items) => {
            for item in items {
                collect_pattern_bindings(item, local_slots, next_slot);
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
}

/// Resolve `Expr::Ident` → `Expr::Resolved` for locals in an expression.
fn resolve_expr(expr: &mut Spanned<Expr>, local_slots: &HashMap<String, u16>) {
    match &mut expr.node {
        Expr::Ident(name) => {
            if let Some(&slot) = local_slots.get(name) {
                expr.node = Expr::Resolved {
                    slot,
                    name: name.clone(),
                    last_use: AnnotBool(false),
                };
            }
            // else: global/namespace — leave as Ident for HashMap fallback
        }
        Expr::Resolved { .. } | Expr::Literal(_) => {}
        Expr::Attr(obj, _) => {
            resolve_expr(obj, local_slots);
        }
        Expr::FnCall(func, args) => {
            resolve_expr(func, local_slots);
            for arg in args {
                resolve_expr(arg, local_slots);
            }
        }
        Expr::BinOp(_, left, right) => {
            resolve_expr(left, local_slots);
            resolve_expr(right, local_slots);
        }
        Expr::Match { subject, arms } => {
            resolve_expr(subject, local_slots);
            for arm in arms {
                resolve_expr(&mut arm.body, local_slots);
            }
        }
        Expr::Constructor(_, Some(inner)) => {
            resolve_expr(inner, local_slots);
        }
        Expr::Constructor(_, None) => {}
        Expr::ErrorProp(inner) => {
            resolve_expr(inner, local_slots);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(e) = part {
                    resolve_expr(e, local_slots);
                }
            }
        }
        Expr::List(elements) => {
            for elem in elements {
                resolve_expr(elem, local_slots);
            }
        }
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                resolve_expr(item, local_slots);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                resolve_expr(key, local_slots);
                resolve_expr(value, local_slots);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                resolve_expr(expr, local_slots);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            resolve_expr(base, local_slots);
            for (_, expr) in updates {
                resolve_expr(expr, local_slots);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &mut boxed.args {
                resolve_expr(arg, local_slots);
            }
        }
    }
}

/// Resolve expressions inside statements.
fn resolve_stmts(stmts: &mut [Stmt], local_slots: &HashMap<String, u16>) {
    for stmt in stmts {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                resolve_expr(expr, local_slots);
            }
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
        resolve_fn(&mut fd, &TypeInfo { variants: HashMap::new(), variant_parents: HashMap::new(), records: HashMap::new() });
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
        resolve_fn(&mut fd, &TypeInfo { variants: HashMap::new(), variant_parents: HashMap::new(), records: HashMap::new() });
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
        resolve_fn(&mut fd, &TypeInfo { variants: HashMap::new(), variant_parents: HashMap::new(), records: HashMap::new() });
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
                        },
                        MatchArm {
                            pattern: Pattern::Wildcard,
                            body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                        },
                    ],
                },
                1,
            ))),
            resolution: None,
        };
        resolve_fn(&mut fd, &TypeInfo { variants: HashMap::new(), variant_parents: HashMap::new(), records: HashMap::new() });
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
                            },
                            MatchArm {
                                pattern: Pattern::Wildcard,
                                body: Box::new(Spanned::bare(Expr::Literal(Literal::Int(0)))),
                            },
                        ],
                    }),
                ),
                Stmt::Expr(Spanned::bare(Expr::Ident("result".to_string()))),
            ])),
            resolution: None,
        };

        resolve_fn(&mut fd, &TypeInfo { variants: HashMap::new(), variant_parents: HashMap::new(), records: HashMap::new() });
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
