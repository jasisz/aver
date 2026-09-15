//! Source tail-entry boundaries, identified from the original function body.
//! A yielding tail call suspends after its arguments are evaluated. Resuming
//! it consumes one Advance but no answer position or operation event. A helper
//! body has its own tail positions even when inlined into a non-tail caller.
use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn eval_tail(
        &mut self,
        expr: &Expression,
        cursor: Cursor,
        next: &Next<'_, 'a>,
    ) -> ResultExpr {
        if let Expr::Match { subject, arms } = &expr.node {
            return self.eval(subject, cursor, &|this, value, current| {
                let arms = arms
                    .iter()
                    .map(|arm| {
                        Ok(MatchArm::new(
                            arm.pattern.clone(),
                            this.eval_tail(&arm.body, current.clone(), next)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(match_expr(value, arms, expr.line))
            });
        }
        let target = match &expr.node {
            Expr::FnCall(callee, args) => dotted_name(callee).map(|name| (name, args.as_slice())),
            Expr::TailCall(call) => Some((call.target.clone(), call.args.as_slice())),
            _ => None,
        };
        if let Some((name, args)) = target
            && !self.model.operations.contains_key(&name)
            && (self.model.imported.contains_key(&name)
                || self
                    .model
                    .source(&name)
                    .is_some_and(super::super::super::is_yield_fn))
        {
            return self.many(args, vec![], cursor, &|this, values, current| {
                this.pause(current, &|this, resumed| {
                    this.invoke(&name, values.clone(), resumed, next)
                })
            });
        }
        self.eval(expr, cursor, next)
    }

    fn pause(
        &mut self,
        cursor: Cursor,
        resume: &dyn Fn(&mut Self, Cursor) -> ResultExpr,
    ) -> ResultExpr {
        let token = self.name();
        let rest = self.name();
        let query = ctor(&format!("{}Query", self.model.upper), "Yield", vec![], 0);
        let success = resume(
            self,
            Cursor {
                inputs: ident(&rest, 0),
                position: cursor.position.clone(),
                events: cursor.events.clone(),
                consumed: add_one(cursor.consumed.clone()),
            },
        )?;
        let mut arms = vec![MatchArm::new(
            Pattern::Constructor(format!("{}Input.Advance", self.model.upper), vec![]),
            success,
        )];
        arms.extend(
            self.model
                .other_inputs("Advance")
                .into_iter()
                .map(|pattern| {
                    MatchArm::new(pattern, self.halted(query.clone(), cursor.clone(), false))
                }),
        );
        let token_match = match_expr(ident(&token, 0), arms, 0);
        Ok(match_expr(
            cursor.inputs.clone(),
            vec![
                MatchArm::new(Pattern::EmptyList, self.halted(query, cursor, true)),
                MatchArm::new(Pattern::Cons(token, rest), token_match),
            ],
            0,
        ))
    }
}
