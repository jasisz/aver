use crate::ast::Expr;

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
            | "Bits"
            | "Bool"
            | "BranchPath"
            | "Console"
            | "Crypto"
            | "Disk"
            | "Env"
            | "Float"
            | "Http"
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
