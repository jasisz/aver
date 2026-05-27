/// Mapping of Aver builtin/namespace functions to Lean 4 equivalents.
///
/// Only pure namespaces are mapped. Effectful services (Console, Disk, Http, etc.)
/// are skipped by the Lean transpiler — those functions won't appear in output.
use crate::ast::Spanned;
use crate::codegen::CodegenContext;
use crate::codegen::builtins::{Builtin, recognize_builtin};
use crate::ir::hir::ResolvedExpr;

/// Try to emit a builtin call as Lean 4 code.
/// Returns `None` if the name is not a pure builtin.
pub fn emit_builtin_call(
    name: &str,
    args: &[Spanned<ResolvedExpr>],
    ctx: &CodegenContext,
) -> Option<String> {
    use crate::codegen::common::is_unit_expr_resolved;

    // Map<T, Unit> set operations: intercept before generic builtin path
    if name == "Map.set" && args.len() == 3 && is_unit_expr_resolved(&args[2].node) {
        let m = p(&super::expr::emit_expr(&args[0], ctx));
        let k = p(&super::expr::emit_expr(&args[1], ctx));
        return Some(format!("AverSet.add {} {}", m, k));
    }

    let builtin = recognize_builtin(name)?;
    let a: Vec<String> = args
        .iter()
        .map(|e| super::expr::emit_expr(e, ctx))
        .collect();

    use Builtin::*;
    let result = match builtin {
        // ---- Result ----
        ResultOk => format!("Except.ok {}", p(&a[0])),
        ResultErr => format!("Except.error {}", p(&a[0])),
        ResultWithDefault => format!("Except.withDefault {} {}", p(&a[0]), p(&a[1])),

        // ---- Option ----
        OptionSome => format!("some {}", p(&a[0])),
        OptionWithDefault => format!("({}.getD {})", p(&a[0]), p(&a[1])),
        OptionToResult => format!("Option.toExcept {} {}", p(&a[0]), p(&a[1])),

        // ---- Int ----
        IntAbs => format!("{}.natAbs", p(&a[0])),
        IntFromFloat => format!("AverFloat.toInt {}", p(&a[0])),
        IntMin => format!("min {} {}", p(&a[0]), p(&a[1])),
        IntMax => format!("max {} {}", p(&a[0]), p(&a[1])),
        IntMod => format!("(Except.ok ({} % {}) : Except String Int)", a[0], a[1]),
        IntFromString => format!("Int.fromString {}", p(&a[0])),

        // ---- Float ----
        FloatAbs => format!("Float.abs {}", p(&a[0])),
        FloatSqrt => format!("Float.sqrt {}", p(&a[0])),
        FloatFromString => format!("Float.fromString {}", p(&a[0])),
        FloatFromInt => format!("Float.ofInt {}", p(&a[0])),
        FloatPow => format!("AverFloat.pow {} {}", p(&a[0]), p(&a[1])),
        FloatRound => format!("AverFloat.round {}", p(&a[0])),
        FloatFloor => format!("AverFloat.floor {}", p(&a[0])),
        FloatCeil => format!("AverFloat.ceil {}", p(&a[0])),
        FloatPi => "(3.141592653589793 : Float)".to_string(),
        FloatMin => format!("min {} {}", p(&a[0]), p(&a[1])),
        FloatMax => format!("max {} {}", p(&a[0]), p(&a[1])),
        FloatSin => format!("Float.sin {}", p(&a[0])),
        FloatCos => format!("Float.cos {}", p(&a[0])),
        FloatAtan2 => format!("Float.atan2 {} {}", p(&a[0]), p(&a[1])),

        // ---- Bool ----
        BoolOr => format!("({} || {})", a[0], a[1]),
        BoolAnd => format!("({} && {})", a[0], a[1]),
        BoolNot => format!("(!{})", a[0]),

        // ---- Char ----
        CharToCode => format!("Char.toCode {}", p(&a[0])),
        CharFromCode => format!("Char.fromCode {}", p(&a[0])),

        // ---- Byte ----
        ByteToHex => format!("AverByte.toHex {}", p(&a[0])),
        ByteFromHex => format!("AverByte.fromHex {}", p(&a[0])),

        // ---- String ----
        StringLen => format!("{}.length", p(&a[0])),
        StringCharAt => format!("String.charAt {} {}", p(&a[0]), p(&a[1])),
        StringChars => format!("String.chars {}", p(&a[0])),
        StringSlice => format!("String.slice {} {} {}", p(&a[0]), p(&a[1]), p(&a[2])),
        StringContains => format!("{}.containsSubstr {}", p(&a[0]), p(&a[1])),
        StringStartsWith => format!("{}.startsWith {}", p(&a[0]), p(&a[1])),
        StringEndsWith => format!("{}.endsWith {}", p(&a[0]), p(&a[1])),
        StringTrim => format!("{}.trim", p(&a[0])),
        StringSplit => format!("AverString.split {} {}", p(&a[0]), p(&a[1])),
        StringJoin => format!("String.intercalate {} {}", p(&a[1]), p(&a[0])),
        StringReplace => format!("{}.replace {} {}", p(&a[0]), p(&a[1]), p(&a[2])),
        StringRepeat => format!("AverString.repeat {} {}", p(&a[0]), p(&a[1])),
        StringIndexOf => format!("AverString.indexOf {} {}", p(&a[0]), p(&a[1])),
        StringToUpper => format!("{}.toUpper", p(&a[0])),
        StringToLower => format!("{}.toLower", p(&a[0])),
        StringFromInt => format!("String.fromInt {}", p(&a[0])),
        StringFromFloat => format!("String.fromFloat {}", p(&a[0])),
        StringFromBool => format!("String.fromBool {}", p(&a[0])),
        StringByteLength => format!("{}.utf8ByteSize", p(&a[0])),

        // ---- List ----
        ListLen => {
            let subj = emit_list_length_subject(&args[0], ctx);
            format!("{}.length", subj)
        }
        ListHead => format!("{}.head?", p(&a[0])),
        ListTail => format!("{}.tail?", p(&a[0])),
        // Parens around `::` and `++` keep the result atomic when it
        // lands as an argument to another fn call. `emit_expr_atom`'s
        // heuristic "starts with `(` ⇒ atomic" is unsafe for `List.
        // concat(toSorted(left), [value])`: the inner `toSorted(left)`
        // emits as `(toSorted__fuel fuel' left)` (parens), so the whole
        // `(toSorted__fuel fuel' left) ++ [value]` LOOKS atomic to
        // emit_expr_atom and goes unparenthesised — then the outer
        // `concatLists` call ends up as `concatLists xs ++ [v] ys`
        // which Lean parses as `concatLists xs ++ ([v] ys)` and fails
        // with "function expected at [v]". Same hazard for `::`.
        ListPrepend => format!("({} :: {})", a[0], p(&a[1])),
        ListTake => format!("{}.take (Int.toNat {})", p(&a[0]), p(&a[1])),
        ListDrop => format!("{}.drop (Int.toNat {})", p(&a[0]), p(&a[1])),
        ListConcat => format!("({} ++ {})", p(&a[0]), p(&a[1])),
        ListReverse => format!("{}.reverse", p(&a[0])),
        ListContains => format!("{}.contains {}", p(&a[0]), p(&a[1])),
        ListFind => format!("{}.find? {}", p(&a[0]), p(&a[1])),
        ListAny => format!("{}.any {}", p(&a[0]), p(&a[1])),
        ListZip => format!("{}.zip {}", p(&a[0]), p(&a[1])),

        // ---- Vector (Lean Array) ----
        VectorNew => format!("Array.mkArray {} {}", p(&a[0]), p(&a[1])),
        VectorGet => format!("{}.get? (Int.toNat {})", p(&a[0]), p(&a[1])),
        VectorSet => format!(
            "if {} < {}.size then some ({}.set! (Int.toNat {}) {}) else none",
            p(&a[1]),
            p(&a[0]),
            p(&a[0]),
            p(&a[1]),
            p(&a[2])
        ),
        VectorLen => format!("{}.size", p(&a[0])),
        VectorFromList => format!("{}.toArray", p(&a[0])),
        ListFromVector => format!("{}.toList", p(&a[0])),

        // ---- Map ----
        MapGet => format!("AverMap.get {} {}", p(&a[0]), p(&a[1])),
        MapSet => format!("AverMap.set {} {} {}", p(&a[0]), p(&a[1]), p(&a[2])),
        MapHas => format!("AverMap.has {} {}", p(&a[0]), p(&a[1])),
        MapRemove => format!("AverMap.remove {} {}", p(&a[0]), p(&a[1])),
        MapKeys => format!("AverMap.keys {}", p(&a[0])),
        MapValues => format!("AverMap.values {}", p(&a[0])),
        MapEntries => format!("AverMap.entries {}", p(&a[0])),
        MapLen => format!("AverMap.len {}", p(&a[0])),
        MapFromList => format!("AverMap.fromList {}", p(&a[0])),
    };
    Some(result)
}

fn emit_list_length_subject(arg: &Spanned<ResolvedExpr>, ctx: &CodegenContext) -> String {
    match &arg.node {
        ResolvedExpr::List(items) if items.is_empty() => "(([] : List Unit))".to_string(),
        _ => p(&super::expr::emit_expr(arg, ctx)),
    }
}

/// Wrap in parens if the string looks like a compound expression.
fn p(s: &str) -> String {
    if s.contains(' ') && !s.starts_with('(') && !s.starts_with('"') {
        format!("({})", s)
    } else {
        s.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Literal, Spanned};
    use crate::codegen::CodegenContext;
    use crate::ir::hir::ResolvedCallee;
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
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_support: false,
            extra_fn_defs: Vec::new(),
            mutual_tco_members: HashSet::new(),
            recursive_fns: HashSet::new(),
            buffer_build_sinks: HashMap::new(),
            buffer_fusion_sites: Vec::new(),
            synthesized_buffered_fns: Vec::new(),
            proof_ir: crate::ir::ProofIR::default(),
            symbol_table: crate::ir::SymbolTable::default(),
            resolved_fn_defs: Vec::new(),
            resolved_module_fn_defs: Vec::new(),
            current_module_scope: std::cell::RefCell::new(None),
        }
    }

    #[test]
    fn option_with_default_wraps_getd_expression_in_parentheses() {
        let ctx = empty_ctx();
        let option_expr = Spanned::bare(ResolvedExpr::Call(
            ResolvedCallee::Builtin("Char.fromCode".to_string()),
            vec![Spanned::bare(ResolvedExpr::Literal(Literal::Int(8)))],
        ));
        let default_expr = Spanned::bare(ResolvedExpr::Literal(Literal::Str("".to_string())));

        let emitted = emit_builtin_call("Option.withDefault", &[option_expr, default_expr], &ctx)
            .expect("Option.withDefault should be emitted");

        assert_eq!(emitted, "((Char.fromCode 8).getD \"\")");
    }

    #[test]
    fn list_len_annotates_empty_list_in_theorem_friendly_form() {
        let ctx = empty_ctx();
        let emitted =
            emit_builtin_call("List.len", &[Spanned::bare(ResolvedExpr::List(vec![]))], &ctx)
                .expect("List.len should be emitted");

        assert_eq!(emitted, "(([] : List Unit)).length");
    }
}
