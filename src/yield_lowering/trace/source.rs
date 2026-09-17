//! A state-passing translation of the preserved direct-style source.
//!
//! Source mode visits retained definitions; protocol mode instruments the real
//! generated segments separately. Both thread the unconsumed input list,
//! position and observations through calls with the same answer semantics.
use super::*;
use build::*;

mod inline;
mod tail;

type Expression = Spanned<Expr>;
type ResultExpr = Result<Expression, String>;
type Next<'k, 'a> = dyn Fn(&mut Compiler<'a>, Expression, Cursor) -> ResultExpr + 'k;
type NextMany<'k, 'a> = dyn Fn(&mut Compiler<'a>, Vec<Expression>, Cursor) -> ResultExpr + 'k;

#[derive(Clone)]
pub(super) struct Cursor {
    pub(super) inputs: Expression,
    pub(super) position: Expression,
    pub(super) events: Expression,
    pub(super) consumed: Expression,
}

pub(super) struct Compiler<'a> {
    model: &'a Model<'a>,
    function: &'a FnDef,
    fresh: usize,
    inlining: Vec<String>,
    protocol_segments: bool,
}

impl<'a> Compiler<'a> {
    pub(super) fn new(model: &'a Model<'a>, function: &'a FnDef) -> Self {
        Self {
            model,
            function,
            fresh: 0,
            inlining: Vec::new(),
            protocol_segments: false,
        }
    }

    pub(super) fn for_protocol(model: &'a Model<'a>, function: &'a FnDef) -> Self {
        Self {
            protocol_segments: true,
            ..Self::new(model, function)
        }
    }

    pub(super) fn compile(mut self) -> Result<FnDef, String> {
        let u = &self.model.upper;
        let mut params = self.function.params.clone();
        params.extend([
            ("__traceInputs".into(), format!("List<{u}Input>")),
            ("__tracePosition".into(), "Int".into()),
            ("__traceEvents".into(), format!("List<{u}Event>")),
            ("__traceConsumed".into(), "Int".into()),
        ]);
        let cursor = Cursor {
            inputs: ident("__traceInputs", 0),
            position: ident("__tracePosition", 0),
            events: ident("__traceEvents", 0),
            consumed: ident("__traceConsumed", 0),
        };
        let body = self.block(self.function.body.stmts(), cursor)?;
        Ok(fn_def(
            self.model.source_name(self.function),
            params,
            self.model.result_type(self.function),
            Some(
                if self.protocol_segments {
                    "Observe a generated protocol segment with the shared answer-tape semantics."
                } else {
                    "Observe the retained source, threading only executed answers through calls."
                }
                .into(),
            ),
            vec![],
            body,
            self.function.line,
        ))
    }

    fn name(&mut self) -> String {
        self.fresh += 1;
        format!("__traceLocal{}", self.fresh)
    }

    fn result(
        &self,
        cursor: Cursor,
        value: Expression,
        pending: Expression,
        valid: bool,
    ) -> Expression {
        Spanned::new(
            Expr::RecordCreate {
                type_name: self.model.result_type(self.function),
                fields: vec![
                    ("remaining".into(), cursor.inputs),
                    ("position".into(), cursor.position),
                    ("consumed".into(), cursor.consumed),
                    ("events".into(), cursor.events),
                    ("value".into(), value),
                    ("pending".into(), pending),
                    (
                        "valid".into(),
                        Spanned::new(Expr::Literal(Literal::Bool(valid)), 0),
                    ),
                ],
            },
            0,
        )
    }

    fn done(&self, value: Expression, cursor: Cursor) -> Expression {
        self.result(
            cursor,
            ctor("Option", "Some", vec![value], 0),
            ctor("Option", "None", vec![], 0),
            true,
        )
    }

    fn halted(&self, query: Expression, cursor: Cursor, valid: bool) -> Expression {
        self.result(
            cursor,
            ctor("Option", "None", vec![], 0),
            ctor("Option", "Some", vec![query], 0),
            valid,
        )
    }

    fn block(&mut self, stmts: &[Stmt], cursor: Cursor) -> ResultExpr {
        self.sequence(
            stmts,
            cursor,
            super::super::is_yield_fn(self.function),
            &|this, value, current| Ok(this.done(value, current)),
        )
    }

    fn sequence(
        &mut self,
        stmts: &[Stmt],
        cursor: Cursor,
        tail_yields: bool,
        finish: &Next<'_, 'a>,
    ) -> ResultExpr {
        let Some((first, rest)) = stmts.split_first() else {
            return finish(self, Spanned::new(Expr::Literal(Literal::Unit), 0), cursor);
        };
        match first {
            Stmt::Binding(name, _, expr) => self.eval(expr, cursor, &|this, value, next| {
                let body = this.sequence(rest, next, tail_yields, finish)?;
                Ok(match_expr(
                    value,
                    vec![MatchArm::new(Pattern::Ident(name.clone()), body)],
                    expr.line,
                ))
            }),
            Stmt::Expr(expr) if rest.is_empty() => {
                if tail_yields {
                    self.eval_tail(expr, cursor, finish)
                } else {
                    self.eval(expr, cursor, finish)
                }
            }
            Stmt::Expr(expr) => self.eval(expr, cursor, &|this, value, next| {
                let body = this.sequence(rest, next, tail_yields, finish)?;
                Ok(match_expr(
                    value,
                    vec![MatchArm::new(Pattern::Ident(this.name()), body)],
                    expr.line,
                ))
            }),
        }
    }

    fn effectful(&self, expr: &Expression) -> bool {
        crate::codegen::expr_walk::any(expr, &mut |e| match &e.node {
            Expr::TailCall(call) => self
                .model
                .source(&call.target)
                .is_some_and(|fd| !fd.effects.is_empty()),
            Expr::ErrorProp(_) => true,
            Expr::FnCall(callee, _) => dotted_name(callee).is_some_and(|name| {
                self.model.operations.contains_key(&name)
                    || (self
                        .model
                        .source(&name)
                        .is_some_and(|fd| !fd.effects.is_empty())
                        || self.model.imported.contains_key(&name))
                    || (self.protocol_segments && self.model.imported_segment(&name).is_some())
            }),
            _ => false,
        })
    }

    pub(super) fn eval(
        &mut self,
        expr: &Expression,
        cursor: Cursor,
        next: &Next<'_, 'a>,
    ) -> ResultExpr {
        if !self.effectful(expr) {
            return next(self, expr.clone(), cursor);
        }
        match &expr.node {
            Expr::Match { subject, arms } => self.eval(subject, cursor, &|this, value, current| {
                let arms = arms
                    .iter()
                    .map(|arm| {
                        Ok(MatchArm::new(
                            arm.pattern.clone(),
                            this.eval(&arm.body, current.clone(), next)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(match_expr(value, arms, expr.line))
            }),
            Expr::FnCall(callee, args) => {
                let name =
                    dotted_name(callee).ok_or("an indirect effectful call has no source model")?;
                self.many(args, vec![], cursor, &|this, values, current| {
                    this.invoke(&name, values, current, next)
                })
            }
            Expr::TailCall(call) => {
                self.many(&call.args, vec![], cursor, &|this, values, current| {
                    this.invoke(&call.target, values, current, next)
                })
            }
            Expr::ErrorProp(inner) => self.eval(inner, cursor, &|this, value, current| {
                let ok = this.name();
                let err = this.name();
                let success = next(this, ident(&ok, expr.line), current.clone())?;
                let failure = this.done(
                    ctor("Result", "Err", vec![ident(&err, expr.line)], expr.line),
                    current,
                );
                Ok(match_expr(
                    value,
                    vec![
                        MatchArm::new(Pattern::Constructor("Result.Ok".into(), vec![ok]), success),
                        MatchArm::new(
                            Pattern::Constructor("Result.Err".into(), vec![err]),
                            failure,
                        ),
                    ],
                    expr.line,
                ))
            }),
            Expr::IndependentProduct(_, _) => {
                Err("independent effectful products require an explicit branch trace model".into())
            }
            _ => {
                let mut children = Vec::new();
                crate::codegen::expr_walk::for_each_child(expr, &mut |e| children.push(e.clone()));
                if children.is_empty() {
                    return Err("unsupported effectful source expression".into());
                }
                self.many(&children, vec![], cursor, &|this, values, current| {
                    let mut rebuilt = expr.clone();
                    let mut values = values.into_iter();
                    crate::codegen::expr_walk::for_each_child_mut(&mut rebuilt, &mut |e| {
                        *e = values.next().expect("one value per source child")
                    });
                    next(this, rebuilt, current)
                })
            }
        }
    }

    fn many(
        &mut self,
        expressions: &[Expression],
        values: Vec<Expression>,
        cursor: Cursor,
        next: &NextMany<'_, 'a>,
    ) -> ResultExpr {
        let Some((first, rest)) = expressions.split_first() else {
            return next(self, values, cursor);
        };
        self.eval(first, cursor, &|this, value, current| {
            // Bind before visiting the next child: a pure argument may still
            // refer to names shadowed by a later source match.
            let name = this.name();
            let mut accumulated = values.clone();
            accumulated.push(ident(&name, first.line));
            let body = this.many(rest, accumulated, current, next)?;
            Ok(match_expr(
                value,
                vec![MatchArm::new(Pattern::Ident(name), body)],
                first.line,
            ))
        })
    }

    fn invoke(
        &mut self,
        name: &str,
        args: Vec<Expression>,
        cursor: Cursor,
        next: &Next<'_, 'a>,
    ) -> ResultExpr {
        if let Some(kind) = self.model.operations.get(name).cloned() {
            return self.request(&kind, args, cursor, next);
        }
        if let Some(helper) = self
            .model
            .source(name)
            .filter(|fd| !fd.effects.is_empty() && inline::eligible(fd))
        {
            return self.inline(helper, &args, cursor, next);
        }
        let helper = self
            .model
            .source(name)
            .filter(|fd| !fd.effects.is_empty())
            .cloned()
            .or_else(|| {
                self.model
                    .imported
                    .get(name)
                    .map(|p| self.model.import_signature(p))
            })
            .or_else(|| {
                self.protocol_segments
                    .then(|| self.model.imported_segment(name))
                    .flatten()
                    .map(|(protocol, segment)| self.model.segment_signature(protocol, segment))
            });
        if let Some(helper) = helper {
            if name == self.function.name {
                let mut args = args;
                args.extend([
                    cursor.inputs,
                    cursor.position,
                    cursor.events,
                    cursor.consumed,
                ]);
                return Ok(call(&self.model.source_name(self.function), args, 0));
            }
            let mut arguments = args;
            arguments.extend([
                cursor.inputs,
                cursor.position,
                cursor.events,
                cursor.consumed,
            ]);
            let observed = call(&self.model.source_name(&helper), arguments, 0);
            let result = self.name();
            let value = self.name();
            let field =
                |name: &str| Spanned::new(Expr::Attr(Box::new(ident(&result, 0)), name.into()), 0);
            let current = Cursor {
                inputs: field("remaining"),
                position: field("position"),
                events: field("events"),
                consumed: field("consumed"),
            };
            let success = next(self, ident(&value, 0), current.clone())?;
            let mut halted = self.result(
                current,
                ctor("Option", "None", vec![], 0),
                field("pending"),
                true,
            );
            if let Expr::RecordCreate { fields, .. } = &mut halted.node {
                fields.last_mut().unwrap().1 = field("valid");
            }
            let body = match_expr(
                field("value"),
                vec![
                    MatchArm::new(
                        Pattern::Constructor("Option.Some".into(), vec![value]),
                        success,
                    ),
                    MatchArm::new(Pattern::Constructor("Option.None".into(), vec![]), halted),
                ],
                0,
            );
            return Ok(match_expr(
                observed,
                vec![MatchArm::new(Pattern::Ident(result), body)],
                0,
            ));
        }
        next(self, call(name, args, 0), cursor)
    }

    fn request(
        &mut self,
        kind: &super::super::ProtocolKind,
        args: Vec<Expression>,
        cursor: Cursor,
        next: &Next<'_, 'a>,
    ) -> ResultExpr {
        let token = self.name();
        let rest = self.name();
        let answer = self.name();
        let query = ctor(
            &format!("{}Query", self.model.upper),
            &kind.name,
            args.clone(),
            0,
        );
        let position = Spanned::new(
            Expr::BinOp(
                BinOp::Add,
                Box::new(cursor.position.clone()),
                Box::new(Spanned::new(Expr::Literal(Literal::Int(1)), 0)),
            ),
            0,
        );
        let mut fields = vec![cursor.position.clone()];
        fields.extend(args);
        fields.push(ident(&answer, 0));
        let event = ctor(
            &format!("{}Event", self.model.upper),
            &format!("Observed{}", kind.name),
            fields,
            0,
        );
        let events = call(
            "List.concat",
            vec![
                cursor.events.clone(),
                Spanned::new(Expr::List(vec![event]), 0),
            ],
            0,
        );
        let success = next(
            self,
            ident(&answer, 0),
            Cursor {
                inputs: ident(&rest, 0),
                position,
                events,
                consumed: add_one(cursor.consumed.clone()),
            },
        )?;
        let mut arms = vec![MatchArm::new(
            Pattern::Constructor(
                format!("{}Input.Answer{}", self.model.upper, kind.name),
                vec![answer],
            ),
            success,
        )];
        arms.extend(
            self.model
                .other_inputs(&format!("Answer{}", kind.name))
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

fn add_one(value: Expression) -> Expression {
    Spanned::new(
        Expr::BinOp(
            BinOp::Add,
            Box::new(value),
            Box::new(Spanned::new(Expr::Literal(Literal::Int(1)), 0)),
        ),
        0,
    )
}
