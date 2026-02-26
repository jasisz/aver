/// Compile-time variable resolution pass.
///
/// After parsing and before interpretation, this pass walks each `FnDef` body
/// and replaces `Expr::Ident(name)` with `Expr::Resolved(depth, slot)` for
/// variables that are local to the function (parameters + bindings).
///
/// Global/namespace identifiers are left as `Expr::Ident` — the interpreter
/// falls back to HashMap lookup for those.
///
/// Only top-level `FnDef` bodies are resolved. Top-level `Stmt` items (globals,
/// REPL) are not touched.
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;

/// Run the resolver on all top-level function definitions.
pub fn resolve_program(items: &mut Vec<TopLevel>) {
    for item in items.iter_mut() {
        if let TopLevel::FnDef(fd) = item {
            resolve_fn(fd);
        }
    }
}

/// Resolve a single function definition.
fn resolve_fn(fd: &mut FnDef) {
    let mut local_slots: HashMap<String, u16> = HashMap::new();
    let mut next_slot: u16 = 0;

    // Params get slots 0..N-1
    for (param_name, _) in &fd.params {
        local_slots.insert(param_name.clone(), next_slot);
        next_slot += 1;
    }

    // Scan body for val/var bindings to pre-allocate slots
    match fd.body.as_ref() {
        FnBody::Expr(expr) => {
            collect_expr_bindings(expr, &mut local_slots, &mut next_slot);
        }
        FnBody::Block(stmts) => {
            collect_binding_slots(stmts, &mut local_slots, &mut next_slot);
        }
    }

    // Resolve expressions in the body
    let mut body = fd.body.as_ref().clone();
    match &mut body {
        FnBody::Expr(expr) => {
            resolve_expr(expr, &local_slots);
        }
        FnBody::Block(stmts) => {
            resolve_stmts(stmts, &local_slots);
        }
    }
    fd.body = Rc::new(body);

    fd.resolution = Some(FnResolution {
        local_count: next_slot,
        local_slots,
    });
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
            Stmt::Binding(name, _) => {
                if !local_slots.contains_key(name) {
                    local_slots.insert(name.clone(), *next_slot);
                    *next_slot += 1;
                }
            }
            Stmt::Expr(expr) => {
                collect_expr_bindings(expr, local_slots, next_slot);
            }
        }
    }
}

/// Collect pattern bindings from match expressions inside an expression tree.
fn collect_expr_bindings(
    expr: &Expr,
    local_slots: &mut HashMap<String, u16>,
    next_slot: &mut u16,
) {
    match expr {
        Expr::Match(subject, arms) => {
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
        Expr::Pipe(left, right) => {
            collect_expr_bindings(left, local_slots, next_slot);
            collect_expr_bindings(right, local_slots, next_slot);
        }
        Expr::ErrorProp(inner) | Expr::TypeAscription(inner, _) => {
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
        Expr::Attr(obj, _) => {
            collect_expr_bindings(obj, local_slots, next_slot);
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.1 {
                collect_expr_bindings(arg, local_slots, next_slot);
            }
        }
        // Leaves — no bindings to collect
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) | Expr::Constructor(_, None) => {}
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
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
}

/// Resolve `Expr::Ident` → `Expr::Resolved` for locals in an expression.
fn resolve_expr(expr: &mut Expr, local_slots: &HashMap<String, u16>) {
    match expr {
        Expr::Ident(name) => {
            if let Some(&slot) = local_slots.get(name) {
                *expr = Expr::Resolved(slot);
            }
            // else: global/namespace — leave as Ident for HashMap fallback
        }
        Expr::Resolved(_) | Expr::Literal(_) => {}
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
        Expr::Match(subject, arms) => {
            resolve_expr(subject, local_slots);
            for arm in arms {
                resolve_expr(&mut arm.body, local_slots);
            }
        }
        Expr::Pipe(left, right) => {
            resolve_expr(left, local_slots);
            resolve_expr(right, local_slots);
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
        Expr::TypeAscription(inner, _) => {
            resolve_expr(inner, local_slots);
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                resolve_expr(expr, local_slots);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &mut boxed.1 {
                resolve_expr(arg, local_slots);
            }
        }
    }
}

/// Resolve expressions inside statements.
fn resolve_stmts(stmts: &mut [Stmt], local_slots: &HashMap<String, u16>) {
    for stmt in stmts {
        match stmt {
            Stmt::Binding(_, expr) | Stmt::Expr(expr) => {
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
            body: Rc::new(FnBody::Expr(Expr::BinOp(
                BinOp::Add,
                Box::new(Expr::Ident("a".to_string())),
                Box::new(Expr::Ident("b".to_string())),
            ))),
            resolution: None,
        };
        resolve_fn(&mut fd);
        let res = fd.resolution.as_ref().unwrap();
        assert_eq!(res.local_slots["a"], 0);
        assert_eq!(res.local_slots["b"], 1);
        assert_eq!(res.local_count, 2);

        match fd.body.as_ref() {
            FnBody::Expr(Expr::BinOp(_, left, right)) => {
                assert_eq!(**left, Expr::Resolved(0));
                assert_eq!(**right, Expr::Resolved(1));
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
            body: Rc::new(FnBody::Expr(Expr::FnCall(
                Box::new(Expr::Ident("Console".to_string())),
                vec![Expr::Ident("x".to_string())],
            ))),
            resolution: None,
        };
        resolve_fn(&mut fd);
        match fd.body.as_ref() {
            FnBody::Expr(Expr::FnCall(func, args)) => {
                // Console stays as Ident (global)
                assert_eq!(**func, Expr::Ident("Console".to_string()));
                // x is resolved to slot 0
                assert_eq!(args[0], Expr::Resolved(0));
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
                    Expr::BinOp(
                        BinOp::Add,
                        Box::new(Expr::Ident("x".to_string())),
                        Box::new(Expr::Literal(Literal::Int(1))),
                    ),
                ),
                Stmt::Expr(Expr::Ident("y".to_string())),
            ])),
            resolution: None,
        };
        resolve_fn(&mut fd);
        let res = fd.resolution.as_ref().unwrap();
        assert_eq!(res.local_slots["x"], 0);
        assert_eq!(res.local_slots["y"], 1);
        assert_eq!(res.local_count, 2);

        match fd.body.as_ref() {
            FnBody::Block(stmts) => {
                // val y = x + 1  →  val y = Resolved(0,0) + 1
                match &stmts[0] {
                    Stmt::Binding(_, Expr::BinOp(_, left, _)) => {
                        assert_eq!(**left, Expr::Resolved(0));
                    }
                    other => panic!("unexpected stmt: {:?}", other),
                }
                // y  →  Resolved(0,1)
                match &stmts[1] {
                    Stmt::Expr(Expr::Resolved(1)) => {}
                    other => panic!("unexpected stmt: {:?}", other),
                }
            }
            other => panic!("unexpected body: {:?}", other),
        }
    }

    #[test]
    fn resolves_match_pattern_bindings() {
        // fn f(x: Int) -> Int = match x: Result.Ok(v) -> v, _ -> 0
        let mut fd = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match(
                Box::new(Expr::Ident("x".to_string())),
                vec![
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Result.Ok".to_string(),
                            vec!["v".to_string()],
                        ),
                        body: Box::new(Expr::Ident("v".to_string())),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::Literal(Literal::Int(0))),
                    },
                ],
            ))),
            resolution: None,
        };
        resolve_fn(&mut fd);
        let res = fd.resolution.as_ref().unwrap();
        // x=0, v=1
        assert_eq!(res.local_slots["v"], 1);

        match fd.body.as_ref() {
            FnBody::Expr(Expr::Match(_, arms)) => {
                // v in arm body should be Resolved(0, 1)
                assert_eq!(*arms[0].body, Expr::Resolved(1));
            }
            other => panic!("unexpected body: {:?}", other),
        }
    }
}
