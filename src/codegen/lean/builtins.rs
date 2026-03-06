/// Mapping of Aver builtin/namespace functions to Lean 4 equivalents.
///
/// Only pure namespaces are mapped. Effectful services (Console, Disk, Http, etc.)
/// are skipped by the Lean transpiler — those functions won't appear in output.
use crate::ast::Expr;
use crate::codegen::CodegenContext;

/// Try to emit a builtin call as Lean 4 code.
/// Returns `None` if the name is not a pure builtin.
pub fn emit_builtin_call(name: &str, args: &[Expr], ctx: &CodegenContext) -> Option<String> {
    match name {
        // ---- Result ----
        "Result.Ok" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Except.ok {}", paren_if_complex(&arg)))
        }
        "Result.Err" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Except.error {}", paren_if_complex(&arg)))
        }
        "Result.withDefault" => {
            let r = super::expr::emit_expr(&args[0], ctx);
            let d = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "Except.withDefault {} {}",
                paren_if_complex(&r),
                paren_if_complex(&d)
            ))
        }

        // ---- Option ----
        "Option.Some" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("some {}", paren_if_complex(&arg)))
        }
        "Option.withDefault" => {
            let o = super::expr::emit_expr(&args[0], ctx);
            let d = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "({}.getD {})",
                paren_if_complex(&o),
                paren_if_complex(&d)
            ))
        }
        "Option.toResult" => {
            let o = super::expr::emit_expr(&args[0], ctx);
            let e = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "Option.toExcept {} {}",
                paren_if_complex(&o),
                paren_if_complex(&e)
            ))
        }

        // ---- Int ----
        "Int.abs" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("{}.natAbs", paren_if_complex(&arg)))
        }
        "Int.toFloat" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Float.ofInt {}", paren_if_complex(&arg)))
        }
        "Int.toString" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("toString {}", paren_if_complex(&arg)))
        }
        "Int.min" => {
            let a = super::expr::emit_expr(&args[0], ctx);
            let b = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "min {} {}",
                paren_if_complex(&a),
                paren_if_complex(&b)
            ))
        }
        "Int.max" => {
            let a = super::expr::emit_expr(&args[0], ctx);
            let b = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "max {} {}",
                paren_if_complex(&a),
                paren_if_complex(&b)
            ))
        }
        "Int.rem" => {
            let a = super::expr::emit_expr(&args[0], ctx);
            let b = super::expr::emit_expr(&args[1], ctx);
            Some(format!("({} % {})", a, b))
        }

        "Int.fromString" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Int.fromString {}", paren_if_complex(&s)))
        }

        // ---- Float ----
        "Float.abs" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Float.abs {}", paren_if_complex(&arg)))
        }
        "Float.sqrt" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Float.sqrt {}", paren_if_complex(&arg)))
        }
        "Float.toString" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("toString {}", paren_if_complex(&arg)))
        }

        "Float.fromString" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Float.fromString {}", paren_if_complex(&s)))
        }

        // ---- Char ----
        "Char.toCode" => {
            let c = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Char.toCode {}", paren_if_complex(&c)))
        }
        "Char.fromCode" => {
            let n = super::expr::emit_expr(&args[0], ctx);
            Some(format!("Char.fromCode {}", paren_if_complex(&n)))
        }

        // ---- Byte ----
        "Byte.toHex" => {
            let b = super::expr::emit_expr(&args[0], ctx);
            Some(format!("AverByte.toHex {}", paren_if_complex(&b)))
        }
        "Byte.fromHex" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            Some(format!("AverByte.fromHex {}", paren_if_complex(&s)))
        }

        // ---- String ----
        "String.len" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("{}.length", paren_if_complex(&arg)))
        }
        "String.contains" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            let sub = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "{}.containsSubstr {}",
                paren_if_complex(&s),
                paren_if_complex(&sub)
            ))
        }
        "String.trim" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("{}.trim", paren_if_complex(&arg)))
        }
        "String.split" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            let delim = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "AverString.split {} {}",
                paren_if_complex(&s),
                paren_if_complex(&delim)
            ))
        }
        "String.join" => {
            let parts = super::expr::emit_expr(&args[0], ctx);
            let delim = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "String.intercalate {} {}",
                paren_if_complex(&delim),
                paren_if_complex(&parts)
            ))
        }
        "String.charAt" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            let i = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "String.charAt {} {}",
                paren_if_complex(&s),
                paren_if_complex(&i)
            ))
        }
        "String.slice" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            let start = super::expr::emit_expr(&args[1], ctx);
            let stop = super::expr::emit_expr(&args[2], ctx);
            Some(format!(
                "String.slice {} {} {}",
                paren_if_complex(&s),
                paren_if_complex(&start),
                paren_if_complex(&stop)
            ))
        }
        "String.fromInt" => {
            let n = super::expr::emit_expr(&args[0], ctx);
            Some(format!("String.fromInt {}", paren_if_complex(&n)))
        }
        "String.fromFloat" => {
            let f = super::expr::emit_expr(&args[0], ctx);
            Some(format!("String.fromFloat {}", paren_if_complex(&f)))
        }
        "String.chars" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            Some(format!("String.chars {}", paren_if_complex(&s)))
        }
        "String.toUpper" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            Some(format!("{}.toUpper", paren_if_complex(&s)))
        }
        "String.toLower" => {
            let s = super::expr::emit_expr(&args[0], ctx);
            Some(format!("{}.toLower", paren_if_complex(&s)))
        }

        // ---- List ----
        "List.len" => {
            let arg = super::expr::emit_expr(&args[0], ctx);
            Some(format!("{}.length", paren_if_complex(&arg)))
        }
        "List.append" => {
            let list = super::expr::emit_expr(&args[0], ctx);
            let item = super::expr::emit_expr(&args[1], ctx);
            Some(format!("{} ++ [{}]", paren_if_complex(&list), item))
        }
        "List.prepend" => {
            let item = super::expr::emit_expr(&args[0], ctx);
            let list = super::expr::emit_expr(&args[1], ctx);
            Some(format!("{} :: {}", item, paren_if_complex(&list)))
        }
        "List.concat" => {
            let a = super::expr::emit_expr(&args[0], ctx);
            let b = super::expr::emit_expr(&args[1], ctx);
            Some(format!("{} ++ {}", paren_if_complex(&a), paren_if_complex(&b)))
        }
        "List.get" => {
            let list = super::expr::emit_expr(&args[0], ctx);
            let idx = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "{}.get? {}",
                paren_if_complex(&list),
                paren_if_complex(&idx)
            ))
        }
        "List.contains" => {
            let list = super::expr::emit_expr(&args[0], ctx);
            let item = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "{}.contains {}",
                paren_if_complex(&list),
                paren_if_complex(&item)
            ))
        }
        "List.reverse" => {
            let list = super::expr::emit_expr(&args[0], ctx);
            Some(format!("{}.reverse", paren_if_complex(&list)))
        }
        "List.zip" => {
            let a = super::expr::emit_expr(&args[0], ctx);
            let b = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "{}.zip {}",
                paren_if_complex(&a),
                paren_if_complex(&b)
            ))
        }

        // ---- Map ----
        "Map.empty" => Some("AverMap.empty".to_string()),
        "Map.get" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            let k = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "AverMap.get {} {}",
                paren_if_complex(&m),
                paren_if_complex(&k)
            ))
        }
        "Map.set" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            let k = super::expr::emit_expr(&args[1], ctx);
            let v = super::expr::emit_expr(&args[2], ctx);
            Some(format!(
                "AverMap.set {} {} {}",
                paren_if_complex(&m),
                paren_if_complex(&k),
                paren_if_complex(&v)
            ))
        }
        "Map.has" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            let k = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "AverMap.has {} {}",
                paren_if_complex(&m),
                paren_if_complex(&k)
            ))
        }
        "Map.remove" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            let k = super::expr::emit_expr(&args[1], ctx);
            Some(format!(
                "AverMap.remove {} {}",
                paren_if_complex(&m),
                paren_if_complex(&k)
            ))
        }
        "Map.keys" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            Some(format!("AverMap.keys {}", paren_if_complex(&m)))
        }
        "Map.values" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            Some(format!("AverMap.values {}", paren_if_complex(&m)))
        }
        "Map.entries" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            Some(format!("AverMap.entries {}", paren_if_complex(&m)))
        }
        "Map.len" => {
            let m = super::expr::emit_expr(&args[0], ctx);
            Some(format!("AverMap.len {}", paren_if_complex(&m)))
        }
        "Map.fromList" => {
            let entries = super::expr::emit_expr(&args[0], ctx);
            Some(format!("AverMap.fromList {}", paren_if_complex(&entries)))
        }

        _ => None,
    }
}

/// Wrap in parens if the string looks like a compound expression.
fn paren_if_complex(s: &str) -> String {
    if s.contains(' ') && !s.starts_with('(') && !s.starts_with('"') {
        format!("({})", s)
    } else {
        s.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Literal};
    use crate::codegen::CodegenContext;
    use std::collections::{HashMap, HashSet};

    fn empty_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
            fn_sigs: HashMap::new(),
            memo_fns: HashSet::new(),
            memo_safe_types: HashSet::new(),
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "test".to_string(),
            modules: vec![],
            module_prefixes: HashSet::new(),
            policy: None,
        }
    }

    #[test]
    fn option_with_default_wraps_getd_expression_in_parentheses() {
        let ctx = empty_ctx();
        let option_expr = Expr::FnCall(
            Box::new(Expr::Attr(
                Box::new(Expr::Ident("Char".to_string())),
                "fromCode".to_string(),
            )),
            vec![Expr::Literal(Literal::Int(8))],
        );
        let default_expr = Expr::Literal(Literal::Str("".to_string()));

        let emitted = emit_builtin_call("Option.withDefault", &[option_expr, default_expr], &ctx)
            .expect("Option.withDefault should be emitted");

        assert_eq!(emitted, "((Char.fromCode 8).getD \"\")");
    }
}
