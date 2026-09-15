//! Hygienic composition of finite source helpers. This preserves source calls'
//! lexical scopes while avoiding duplicated observation-record projections in
//! proofs. Recursive or early-return helpers retain the state-passing call.
use super::*;
use std::collections::HashMap;

pub(super) fn eligible(fd: &FnDef) -> bool {
    !fd.body.stmts().iter().any(|stmt| {
        let (Stmt::Expr(e) | Stmt::Binding(_, _, e)) = stmt;
        crate::codegen::expr_walk::any(e, &mut |e| match &e.node {
            Expr::ErrorProp(_) | Expr::TailCall(_) => true,
            Expr::FnCall(callee, _) => dotted_name(callee).as_deref() == Some(fd.name.as_str()),
            _ => false,
        })
    })
}

impl<'a> Compiler<'a> {
    pub(super) fn inline(
        &mut self,
        fd: &FnDef,
        args: &[Expression],
        cursor: Cursor,
        next: &Next<'_, 'a>,
    ) -> ResultExpr {
        if self.inlining.contains(&fd.name) {
            return Err(format!(
                "recursive helper cycle through '{}' needs an explicit source model",
                fd.name
            ));
        }
        self.inlining.push(fd.name.clone());
        let mut names = HashMap::new();
        for ((name, _), arg) in fd.params.iter().zip(args) {
            let Expr::Ident(actual) = &arg.node else {
                return Err("source call argument was not sequenced".into());
            };
            names.insert(name.clone(), actual.clone());
        }
        let mut stmts = Vec::new();
        for stmt in fd.body.stmts() {
            match stmt {
                Stmt::Binding(name, ty, expr) => {
                    let expr = self.rename(expr, &names);
                    let fresh = self.name();
                    names.insert(name.clone(), fresh.clone());
                    stmts.push(Stmt::Binding(fresh, ty.clone(), expr));
                }
                Stmt::Expr(expr) => stmts.push(Stmt::Expr(self.rename(expr, &names))),
            }
        }
        let result = self.sequence(
            &stmts,
            cursor,
            super::super::super::is_yield_fn(fd),
            &|this, value, cursor| {
                let active = this.inlining.pop().expect("active inline helper");
                let result = next(this, value, cursor);
                this.inlining.push(active);
                result
            },
        );
        self.inlining.pop();
        result
    }

    fn rename(&mut self, expr: &Expression, names: &HashMap<String, String>) -> Expression {
        let mut result = expr.clone();
        match &mut result.node {
            Expr::Ident(name) => {
                if let Some(new) = names.get(name) {
                    *name = new.clone();
                }
            }
            Expr::Match { subject, arms } => {
                **subject = self.rename(subject, names);
                for arm in arms {
                    let mut inner = names.clone();
                    self.rename_pattern(&mut arm.pattern, &mut inner);
                    *arm.body = self.rename(&arm.body, &inner);
                }
            }
            _ => crate::codegen::expr_walk::for_each_child_mut(&mut result, &mut |child| {
                *child = self.rename(child, names)
            }),
        }
        result
    }

    fn rename_pattern(&mut self, pattern: &mut Pattern, names: &mut HashMap<String, String>) {
        let mut bind = |name: &mut String| {
            if name != "_" {
                let fresh = self.name();
                names.insert(name.clone(), fresh.clone());
                *name = fresh;
            }
        };
        match pattern {
            Pattern::Ident(name) => bind(name),
            Pattern::Cons(head, tail) => {
                bind(head);
                bind(tail);
            }
            Pattern::Constructor(_, fields) => {
                for field in fields {
                    bind(field);
                }
            }
            Pattern::Tuple(fields) => {
                for field in fields {
                    self.rename_pattern(field, names);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
        }
    }
}
