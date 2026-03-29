use std::rc::Rc;

use crate::ast::{BinOp, Expr, FnBody, Literal, Pattern, Stmt, StrPart};
use crate::ir::{
    CallLowerCtx, CallPlan, ForwardArg, LeafOp, TailCallPlan, WrapperKind, classify_call_plan,
    classify_forward_call_plan, classify_leaf_op, classify_tail_call_plan,
};

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
pub(crate) enum LoweredLeafOp {
    MapGet {
        map: ExprId,
        key: ExprId,
    },
    MapSet {
        map: ExprId,
        key: ExprId,
        value: ExprId,
    },
    VectorNew {
        size: ExprId,
        fill: ExprId,
    },
    VectorGetOrDefaultLiteral {
        vector: ExprId,
        index: ExprId,
        default_literal: Literal,
    },
    IntModOrDefaultLiteral {
        a: ExprId,
        b: ExprId,
        default_literal: Literal,
    },
}

impl LoweredLeafOp {
    pub(crate) fn arity(&self) -> usize {
        match self {
            Self::MapGet { .. } => 2,
            Self::MapSet { .. } => 3,
            Self::VectorNew { .. } => 2,
            Self::VectorGetOrDefaultLiteral { .. } => 2,
            Self::IntModOrDefaultLiteral { .. } => 2,
        }
    }

    pub(crate) fn arg_at(&self, idx: usize) -> Option<ExprId> {
        match self {
            Self::MapGet { map, key } => match idx {
                0 => Some(*map),
                1 => Some(*key),
                _ => None,
            },
            Self::MapSet { map, key, value } => match idx {
                0 => Some(*map),
                1 => Some(*key),
                2 => Some(*value),
                _ => None,
            },
            Self::VectorNew { size, fill } => match idx {
                0 => Some(*size),
                1 => Some(*fill),
                _ => None,
            },
            Self::VectorGetOrDefaultLiteral { vector, index, .. } => match idx {
                0 => Some(*vector),
                1 => Some(*index),
                _ => None,
            },
            Self::IntModOrDefaultLiteral { a, b, .. } => match idx {
                0 => Some(*a),
                1 => Some(*b),
                _ => None,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum LoweredDirectCallTarget {
    Builtin(String),
    Function(String),
    Wrapper(WrapperKind),
    NoneValue,
    TypeConstructor {
        qualified_type_name: String,
        variant_name: String,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum LoweredForwardArg {
    Local(String),
    Slot(u16),
}

#[derive(Debug, Clone)]
pub(crate) enum LoweredTailCallTarget {
    SelfCall,
    KnownFunction(String),
    Unknown(String),
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
    Leaf(LoweredLeafOp),
    ForwardCall {
        target: LoweredDirectCallTarget,
        args: Rc<[LoweredForwardArg]>,
    },
    DirectCall {
        target: LoweredDirectCallTarget,
        args: Rc<[ExprId]>,
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
        target: LoweredTailCallTarget,
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

#[derive(Debug)]
struct LowerBuilder<'a, Ctx: CallLowerCtx> {
    exprs: Vec<LoweredExpr>,
    ctx: &'a Ctx,
    current_fn: Option<&'a str>,
}

impl<Ctx: CallLowerCtx> LowerBuilder<'_, Ctx> {
    fn lower_direct_call_target(&self, plan: CallPlan) -> Option<LoweredDirectCallTarget> {
        match plan {
            CallPlan::Builtin(name) => Some(LoweredDirectCallTarget::Builtin(name)),
            CallPlan::Function(name) => Some(LoweredDirectCallTarget::Function(name)),
            CallPlan::Wrapper(kind) => Some(LoweredDirectCallTarget::Wrapper(kind)),
            CallPlan::NoneValue => Some(LoweredDirectCallTarget::NoneValue),
            CallPlan::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => Some(LoweredDirectCallTarget::TypeConstructor {
                qualified_type_name,
                variant_name,
            }),
            CallPlan::Dynamic => None,
        }
    }

    fn lower_forward_arg(&self, arg: ForwardArg) -> LoweredForwardArg {
        match arg {
            ForwardArg::Local(name) => LoweredForwardArg::Local(name),
            ForwardArg::Slot(slot) => LoweredForwardArg::Slot(slot),
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> ExprId {
        let lowered = match expr {
            Expr::Literal(lit) => LoweredExpr::Literal(lit.clone()),
            Expr::Ident(name) => LoweredExpr::Ident(name.clone()),
            Expr::Resolved(slot) => LoweredExpr::Resolved(*slot),
            Expr::Attr(obj, field) => LoweredExpr::Attr {
                obj: self.lower_expr(obj),
                field: field.clone(),
            },
            Expr::FnCall(fn_expr, args) => match classify_leaf_op(expr, self.ctx) {
                Some(LeafOp::MapGet { map, key }) => LoweredExpr::Leaf(LoweredLeafOp::MapGet {
                    map: self.lower_expr(map),
                    key: self.lower_expr(key),
                }),
                Some(LeafOp::MapSet { map, key, value }) => {
                    LoweredExpr::Leaf(LoweredLeafOp::MapSet {
                        map: self.lower_expr(map),
                        key: self.lower_expr(key),
                        value: self.lower_expr(value),
                    })
                }
                Some(LeafOp::VectorNew { size, fill }) => {
                    LoweredExpr::Leaf(LoweredLeafOp::VectorNew {
                        size: self.lower_expr(size),
                        fill: self.lower_expr(fill),
                    })
                }
                Some(LeafOp::VectorGetOrDefaultLiteral {
                    vector,
                    index,
                    default_literal,
                }) => LoweredExpr::Leaf(LoweredLeafOp::VectorGetOrDefaultLiteral {
                    vector: self.lower_expr(vector),
                    index: self.lower_expr(index),
                    default_literal: default_literal.clone(),
                }),
                Some(LeafOp::IntModOrDefaultLiteral {
                    a,
                    b,
                    default_literal,
                }) => LoweredExpr::Leaf(LoweredLeafOp::IntModOrDefaultLiteral {
                    a: self.lower_expr(a),
                    b: self.lower_expr(b),
                    default_literal: default_literal.clone(),
                }),
                Some(LeafOp::ListIndexGet { .. })
                | Some(LeafOp::FieldAccess { .. })
                | Some(LeafOp::VectorSetOrDefaultSameVector { .. })
                | None => {
                    if let Some(plan) = classify_forward_call_plan(expr, self.ctx)
                        && let Some(target) = self.lower_direct_call_target(plan.target)
                    {
                        let lowered_args = plan
                            .args
                            .into_iter()
                            .map(|arg| self.lower_forward_arg(arg))
                            .collect::<Vec<_>>();
                        LoweredExpr::ForwardCall {
                            target,
                            args: lowered_args.into(),
                        }
                    } else {
                        match classify_call_plan(fn_expr, self.ctx) {
                            CallPlan::Builtin(name) => {
                                let lowered_args = args
                                    .iter()
                                    .map(|arg| self.lower_expr(arg))
                                    .collect::<Vec<_>>();
                                LoweredExpr::DirectCall {
                                    target: self
                                        .lower_direct_call_target(CallPlan::Builtin(name))
                                        .expect("builtin direct target should lower"),
                                    args: lowered_args.into(),
                                }
                            }
                            CallPlan::Function(name) => {
                                let lowered_args = args
                                    .iter()
                                    .map(|arg| self.lower_expr(arg))
                                    .collect::<Vec<_>>();
                                LoweredExpr::DirectCall {
                                    target: self
                                        .lower_direct_call_target(CallPlan::Function(name))
                                        .expect("function direct target should lower"),
                                    args: lowered_args.into(),
                                }
                            }
                            CallPlan::Wrapper(kind) => {
                                let lowered_args = args
                                    .iter()
                                    .map(|arg| self.lower_expr(arg))
                                    .collect::<Vec<_>>();
                                LoweredExpr::DirectCall {
                                    target: self
                                        .lower_direct_call_target(CallPlan::Wrapper(kind))
                                        .expect("wrapper direct target should lower"),
                                    args: lowered_args.into(),
                                }
                            }
                            CallPlan::NoneValue => {
                                let lowered_args = args
                                    .iter()
                                    .map(|arg| self.lower_expr(arg))
                                    .collect::<Vec<_>>();
                                LoweredExpr::DirectCall {
                                    target: self
                                        .lower_direct_call_target(CallPlan::NoneValue)
                                        .expect("none direct target should lower"),
                                    args: lowered_args.into(),
                                }
                            }
                            CallPlan::TypeConstructor {
                                qualified_type_name,
                                variant_name,
                            } => {
                                let lowered_args = args
                                    .iter()
                                    .map(|arg| self.lower_expr(arg))
                                    .collect::<Vec<_>>();
                                LoweredExpr::DirectCall {
                                    target: self
                                        .lower_direct_call_target(CallPlan::TypeConstructor {
                                            qualified_type_name,
                                            variant_name,
                                        })
                                        .expect("constructor direct target should lower"),
                                    args: lowered_args.into(),
                                }
                            }
                            CallPlan::Dynamic => {
                                let lowered_args = args
                                    .iter()
                                    .map(|arg| self.lower_expr(arg))
                                    .collect::<Vec<_>>();
                                LoweredExpr::FnCall {
                                    fn_expr: self.lower_expr(fn_expr),
                                    args: lowered_args.into(),
                                }
                            }
                        }
                    }
                }
            },
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
                let target = match self.current_fn {
                    Some(current_fn) => {
                        match classify_tail_call_plan(&boxed.0, current_fn, self.ctx) {
                            TailCallPlan::SelfCall => LoweredTailCallTarget::SelfCall,
                            TailCallPlan::KnownFunction(name) => {
                                LoweredTailCallTarget::KnownFunction(name)
                            }
                            TailCallPlan::Unknown(name) => LoweredTailCallTarget::Unknown(name),
                        }
                    }
                    None => LoweredTailCallTarget::Unknown(boxed.0.clone()),
                };
                LoweredExpr::TailCall {
                    target,
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

pub(crate) fn lower_fn_body(
    body: &FnBody,
    ctx: &impl CallLowerCtx,
    current_fn: &str,
) -> Rc<LoweredFunctionBody> {
    let mut builder = LowerBuilder {
        exprs: Vec::new(),
        ctx,
        current_fn: Some(current_fn),
    };
    let stmts = body
        .stmts()
        .iter()
        .map(|stmt| builder.lower_stmt(stmt))
        .collect::<Vec<_>>();
    Rc::new(builder.finish(stmts))
}

pub(crate) fn lower_expr_root(
    expr: &Expr,
    ctx: &impl CallLowerCtx,
) -> (Rc<LoweredFunctionBody>, ExprId) {
    let mut builder = LowerBuilder {
        exprs: Vec::new(),
        ctx,
        current_fn: None,
    };
    let root = builder.lower_expr(expr);
    (Rc::new(builder.finish(Vec::new())), root)
}
