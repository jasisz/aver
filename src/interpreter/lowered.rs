use std::rc::Rc;

use crate::ast::{BinOp, Expr, FnBody, Literal, Pattern, Stmt, StrPart};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExprId(pub usize);

#[derive(Debug, Clone)]
pub(crate) struct LoweredMatchArm {
    pub pattern: Pattern,
    pub body: ExprId,
}

#[derive(Debug, Clone)]
pub(crate) enum LoweredStrPart {
    Literal(String),
    Parsed(ExprId),
}

#[derive(Debug, Clone)]
pub(crate) enum LoweredExpr {
    Literal(Literal),
    Ident(String),
    Attr {
        obj: ExprId,
        field: String,
    },
    FnCall {
        fn_expr: ExprId,
        args: Rc<[ExprId]>,
    },
    BinOp {
        op: BinOp,
        left: ExprId,
        right: ExprId,
    },
    Match {
        subject: ExprId,
        arms: Rc<[LoweredMatchArm]>,
        line: usize,
    },
    Constructor {
        name: String,
        arg: Option<ExprId>,
    },
    ErrorProp {
        inner: ExprId,
    },
    InterpolatedStr(Rc<[LoweredStrPart]>),
    List(Rc<[ExprId]>),
    Tuple(Rc<[ExprId]>),
    MapLiteral(Rc<[(ExprId, ExprId)]>),
    RecordCreate {
        type_name: String,
        fields: Rc<[(String, ExprId)]>,
    },
    RecordUpdate {
        type_name: String,
        base: ExprId,
        updates: Rc<[(String, ExprId)]>,
    },
    TailCall {
        target: String,
        args: Rc<[ExprId]>,
    },
    Resolved(u16),
}

#[derive(Debug, Clone)]
pub(crate) enum LoweredStmt {
    Binding(String, ExprId),
    Expr(ExprId),
}

#[derive(Debug, Clone)]
pub(crate) struct LoweredFunctionBody {
    exprs: Vec<LoweredExpr>,
    stmts: Rc<[LoweredStmt]>,
}

impl LoweredFunctionBody {
    pub(crate) fn expr(&self, id: ExprId) -> &LoweredExpr {
        &self.exprs[id.0]
    }

    pub(crate) fn stmt(&self, idx: usize) -> Option<&LoweredStmt> {
        self.stmts.get(idx)
    }
}

#[derive(Debug, Default)]
struct LowerBuilder {
    exprs: Vec<LoweredExpr>,
}

impl LowerBuilder {
    fn lower_expr(&mut self, expr: &Expr) -> ExprId {
        let lowered = match expr {
            Expr::Literal(lit) => LoweredExpr::Literal(lit.clone()),
            Expr::Ident(name) => LoweredExpr::Ident(name.clone()),
            Expr::Resolved(slot) => LoweredExpr::Resolved(*slot),
            Expr::Attr(obj, field) => LoweredExpr::Attr {
                obj: self.lower_expr(obj),
                field: field.clone(),
            },
            Expr::FnCall(fn_expr, args) => {
                let lowered_args = args
                    .iter()
                    .map(|arg| self.lower_expr(arg))
                    .collect::<Vec<_>>();
                LoweredExpr::FnCall {
                    fn_expr: self.lower_expr(fn_expr),
                    args: lowered_args.into(),
                }
            }
            Expr::BinOp(op, left, right) => LoweredExpr::BinOp {
                op: *op,
                left: self.lower_expr(left),
                right: self.lower_expr(right),
            },
            Expr::Match {
                subject,
                arms,
                line,
            } => {
                let lowered_arms = arms
                    .iter()
                    .map(|arm| LoweredMatchArm {
                        pattern: arm.pattern.clone(),
                        body: self.lower_expr(&arm.body),
                    })
                    .collect::<Vec<_>>();
                LoweredExpr::Match {
                    subject: self.lower_expr(subject),
                    arms: lowered_arms.into(),
                    line: *line,
                }
            }
            Expr::Constructor(name, arg) => LoweredExpr::Constructor {
                name: name.clone(),
                arg: arg.as_ref().map(|expr| self.lower_expr(expr)),
            },
            Expr::ErrorProp(inner) => LoweredExpr::ErrorProp {
                inner: self.lower_expr(inner),
            },
            Expr::InterpolatedStr(parts) => {
                let lowered_parts = parts
                    .iter()
                    .map(|part| match part {
                        StrPart::Literal(text) => LoweredStrPart::Literal(text.clone()),
                        StrPart::Parsed(expr) => LoweredStrPart::Parsed(self.lower_expr(expr)),
                    })
                    .collect::<Vec<_>>();
                LoweredExpr::InterpolatedStr(lowered_parts.into())
            }
            Expr::List(items) => {
                let lowered_items = items
                    .iter()
                    .map(|item| self.lower_expr(item))
                    .collect::<Vec<_>>();
                LoweredExpr::List(lowered_items.into())
            }
            Expr::Tuple(items) => {
                let lowered_items = items
                    .iter()
                    .map(|item| self.lower_expr(item))
                    .collect::<Vec<_>>();
                LoweredExpr::Tuple(lowered_items.into())
            }
            Expr::MapLiteral(entries) => {
                let lowered_entries = entries
                    .iter()
                    .map(|(key, value)| (self.lower_expr(key), self.lower_expr(value)))
                    .collect::<Vec<_>>();
                LoweredExpr::MapLiteral(lowered_entries.into())
            }
            Expr::RecordCreate { type_name, fields } => {
                let lowered_fields = fields
                    .iter()
                    .map(|(name, expr)| (name.clone(), self.lower_expr(expr)))
                    .collect::<Vec<_>>();
                LoweredExpr::RecordCreate {
                    type_name: type_name.clone(),
                    fields: lowered_fields.into(),
                }
            }
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                let lowered_updates = updates
                    .iter()
                    .map(|(name, expr)| (name.clone(), self.lower_expr(expr)))
                    .collect::<Vec<_>>();
                LoweredExpr::RecordUpdate {
                    type_name: type_name.clone(),
                    base: self.lower_expr(base),
                    updates: lowered_updates.into(),
                }
            }
            Expr::TailCall(boxed) => {
                let lowered_args = boxed
                    .1
                    .iter()
                    .map(|arg| self.lower_expr(arg))
                    .collect::<Vec<_>>();
                LoweredExpr::TailCall {
                    target: boxed.0.clone(),
                    args: lowered_args.into(),
                }
            }
        };

        let id = ExprId(self.exprs.len());
        self.exprs.push(lowered);
        id
    }

    fn lower_stmt(&mut self, stmt: &Stmt) -> LoweredStmt {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                LoweredStmt::Binding(name.clone(), self.lower_expr(expr))
            }
            Stmt::Expr(expr) => LoweredStmt::Expr(self.lower_expr(expr)),
        }
    }

    fn finish(self, stmts: Vec<LoweredStmt>) -> LoweredFunctionBody {
        LoweredFunctionBody {
            exprs: self.exprs,
            stmts: stmts.into(),
        }
    }
}

pub(crate) fn lower_fn_body(body: &FnBody) -> Rc<LoweredFunctionBody> {
    let mut builder = LowerBuilder::default();
    let stmts = body
        .stmts()
        .iter()
        .map(|stmt| builder.lower_stmt(stmt))
        .collect::<Vec<_>>();
    Rc::new(builder.finish(stmts))
}

pub(crate) fn lower_expr_root(expr: &Expr) -> (Rc<LoweredFunctionBody>, ExprId) {
    let mut builder = LowerBuilder::default();
    let root = builder.lower_expr(expr);
    (Rc::new(builder.finish(Vec::new())), root)
}
