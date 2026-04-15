use crate::ast::{Expr, FnBody, Spanned, Stmt};

/// Shared semantic lowering helpers that sit between resolved AST and concrete backends.
///
/// This is intentionally Aver-specific, not Rust- or VM-specific. Backends can
/// provide a tiny context adapter and ask the shared layer what a call-shaped
/// expression means semantically before choosing their own target encoding.
pub trait CallLowerCtx {
    /// Whether this identifier names a local runtime value and therefore must
    /// remain a dynamic call target rather than a direct function/namespace path.
    fn is_local_value(&self, name: &str) -> bool;

    /// Whether this name denotes a user-defined type in the current compilation context.
    fn is_user_type(&self, name: &str) -> bool;

    /// Resolve a dotted path to `(module_prefix, local_suffix)` when it refers to
    /// a user module path rather than a builtin namespace.
    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WrapperKind {
    ResultOk,
    ResultErr,
    OptionSome,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallPlan {
    /// Runtime value call: callee is not a statically known Aver function/builtin/ctor path.
    Dynamic,
    /// Source-level function name, either bare (`fib`) or fully-qualified (`Data.Fib.fib`).
    Function(String),
    /// Namespace builtin/service call like `List.len`, `Console.print`, `SelfHostRuntime.*`.
    Builtin(String),
    /// Wrapper constructor lowering such as `Result.Ok`, `Result.Err`, `Option.Some`.
    Wrapper(WrapperKind),
    /// Constant constructor value `Option.None`.
    NoneValue,
    /// User-defined variant constructor such as `Shape.Circle` or `Domain.Shape.Circle`.
    TypeConstructor {
        qualified_type_name: String,
        variant_name: String,
    },
}

pub type SemanticCallee = CallPlan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForwardArg {
    Local(String),
    Slot(u16),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForwardCallPlan {
    pub target: CallPlan,
    pub args: Vec<ForwardArg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TailCallPlan {
    SelfCall,
    KnownFunction(String),
    Unknown(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticConstructor {
    Wrapper(WrapperKind),
    NoneValue,
    TypeConstructor {
        qualified_type_name: String,
        variant_name: String,
    },
    Unknown(String),
}

pub fn expr_to_dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name.clone()),
        Expr::Attr(obj, field) => {
            let head = expr_to_dotted_name(&obj.node)?;
            Some(format!("{head}.{field}"))
        }
        _ => None,
    }
}

pub fn is_builtin_namespace(name: &str) -> bool {
    matches!(
        name,
        "Args"
            | "Bool"
            | "Byte"
            | "Char"
            | "Console"
            | "Disk"
            | "Env"
            | "Float"
            | "Http"
            | "HttpServer"
            | "Int"
            | "List"
            | "Map"
            | "Option"
            | "Random"
            | "Result"
            | "SelfHostRuntime"
            | "String"
            | "Tcp"
            | "Terminal"
            | "Time"
            | "Vector"
    )
}

pub fn classify_call_plan(expr: &Expr, ctx: &impl CallLowerCtx) -> CallPlan {
    match expr {
        Expr::Resolved { .. } => CallPlan::Dynamic,
        Expr::Ident(name) => match ctx.is_local_value(name) {
            true => CallPlan::Dynamic,
            false => classify_named_callee(name, None, ctx),
        },
        Expr::Attr(_, _) => {
            let Some(dotted) = expr_to_dotted_name(expr) else {
                return CallPlan::Dynamic;
            };
            match dotted.chars().next().is_some_and(|c| c.is_uppercase()) {
                false => CallPlan::Dynamic,
                true => {
                    let module_split = ctx.resolve_module_call(&dotted);
                    match module_split {
                        Some((prefix, suffix)) => {
                            classify_named_callee(&dotted, Some((prefix, suffix)), ctx)
                        }
                        None => classify_named_callee(&dotted, None, ctx),
                    }
                }
            }
        }
        _ => CallPlan::Dynamic,
    }
}

pub fn classify_callee(expr: &Expr, ctx: &impl CallLowerCtx) -> SemanticCallee {
    classify_call_plan(expr, ctx)
}

pub fn classify_forward_call_plan(expr: &Expr, ctx: &impl CallLowerCtx) -> Option<ForwardCallPlan> {
    let Expr::FnCall(fn_expr, args) = expr else {
        return None;
    };
    classify_forward_call_parts(&fn_expr.node, args, ctx)
}

pub fn classify_forward_call_parts(
    fn_expr: &Expr,
    args: &[Spanned<Expr>],
    ctx: &impl CallLowerCtx,
) -> Option<ForwardCallPlan> {
    let target = classify_call_plan(fn_expr, ctx);
    if matches!(target, CallPlan::Dynamic)
        || matches!(target, CallPlan::NoneValue)
        || matches!(target, CallPlan::Wrapper(_) if args.len() != 1)
    {
        return None;
    }

    let args = args
        .iter()
        .map(|arg| classify_forward_arg(&arg.node, ctx))
        .collect::<Option<Vec<_>>>()?;

    Some(ForwardCallPlan { target, args })
}

pub fn classify_forward_fn_body(body: &FnBody, ctx: &impl CallLowerCtx) -> Option<ForwardCallPlan> {
    let [Stmt::Expr(expr)] = body.stmts() else {
        return None;
    };
    classify_forward_call_plan(&expr.node, ctx)
}

pub fn classify_tail_call_plan(
    target: &str,
    current_fn: &str,
    ctx: &impl CallLowerCtx,
) -> TailCallPlan {
    if target == current_fn {
        return TailCallPlan::SelfCall;
    }

    let module_split = if target.chars().next().is_some_and(|c| c.is_uppercase()) {
        ctx.resolve_module_call(target)
    } else {
        None
    };

    match classify_named_callee(target, module_split, ctx) {
        CallPlan::Function(name) => TailCallPlan::KnownFunction(name),
        _ => TailCallPlan::Unknown(target.to_string()),
    }
}

pub fn classify_constructor_name(name: &str, ctx: &impl CallLowerCtx) -> SemanticConstructor {
    match name {
        "None" | "Option.None" => return SemanticConstructor::NoneValue,
        "Ok" | "Result.Ok" => return SemanticConstructor::Wrapper(WrapperKind::ResultOk),
        "Err" | "Result.Err" => return SemanticConstructor::Wrapper(WrapperKind::ResultErr),
        "Some" | "Option.Some" => return SemanticConstructor::Wrapper(WrapperKind::OptionSome),
        _ => {}
    }

    let module_split = if name.chars().next().is_some_and(|c| c.is_uppercase()) {
        ctx.resolve_module_call(name)
    } else {
        None
    };

    let bare = match module_split {
        Some((_, suffix)) => suffix,
        None => name,
    };

    if let Some((type_name, variant_name)) = bare.rsplit_once('.')
        && ctx.is_user_type(type_name)
    {
        let qualified_type_name = match module_split {
            Some((prefix, _)) => format!("{prefix}.{type_name}"),
            None => type_name.to_string(),
        };
        return SemanticConstructor::TypeConstructor {
            qualified_type_name,
            variant_name: variant_name.to_string(),
        };
    }

    SemanticConstructor::Unknown(name.to_string())
}

fn classify_forward_arg(expr: &Expr, ctx: &impl CallLowerCtx) -> Option<ForwardArg> {
    match expr {
        Expr::Resolved { slot, .. } => Some(ForwardArg::Slot(*slot)),
        Expr::Ident(name) if ctx.is_local_value(name) => Some(ForwardArg::Local(name.clone())),
        _ => None,
    }
}

fn classify_named_callee(
    full_name: &str,
    module_split: Option<(&str, &str)>,
    ctx: &impl CallLowerCtx,
) -> CallPlan {
    let bare = match module_split {
        Some((_, suffix)) => suffix,
        None => full_name,
    };

    if module_split.is_none() {
        match bare {
            "Option.None" => return CallPlan::NoneValue,
            "Result.Ok" => return CallPlan::Wrapper(WrapperKind::ResultOk),
            "Result.Err" => return CallPlan::Wrapper(WrapperKind::ResultErr),
            "Option.Some" => return CallPlan::Wrapper(WrapperKind::OptionSome),
            _ => {}
        }

        if let Some((ns, _)) = bare.split_once('.')
            && is_builtin_namespace(ns)
        {
            return CallPlan::Builtin(bare.to_string());
        }
    }

    if let Some((type_name, variant_name)) = bare.rsplit_once('.')
        && ctx.is_user_type(type_name)
    {
        let qualified_type_name = match module_split {
            Some((prefix, _)) => format!("{prefix}.{type_name}"),
            None => type_name.to_string(),
        };
        return CallPlan::TypeConstructor {
            qualified_type_name,
            variant_name: variant_name.to_string(),
        };
    }

    CallPlan::Function(full_name.to_string())
}
