/// Mapping of Aver builtin/namespace functions to Lean 4 equivalents.
///
/// Only pure namespaces are mapped. Effectful services (Console, Disk, Http, etc.)
/// are skipped by the Lean transpiler — those functions won't appear in output.
use crate::ast::Spanned;
use crate::codegen::CodegenContext;
use crate::codegen::builtins::{Builtin, recognize_builtin};
use crate::ir::hir::ResolvedExpr;

/// A count-taking `Bits` operation with its negative-count guard, so the
/// `Except.error` arm stays reachable in the model exactly where the VM can
/// return `Result.Err`.
fn bits_counted(op: &str, a: &[String], message: &str) -> String {
    format!(
        "(if {n} < 0 then Except.error \"{message}\" else Except.ok (AverBits.{op} {x} {n}) : Except String Int)",
        x = p(&a[0]),
        n = p(&a[1])
    )
}

/// Try to emit a builtin call as Lean 4 code.
/// Returns `None` if the name is not a pure builtin.
///
/// A set-shaped map (`Map<T, Unit>`) gets no special treatment here. It used
/// to: `Map.set(s, k, Unit)` was intercepted and emitted as `AverSet.add s k`,
/// with the type rendered `Finset T` and a literal as `AverSet.ofList [...]`.
/// Nothing defined `AverSet`, and `Finset` is Mathlib, which the generated
/// project does not depend on — so every such program exported Lean that did
/// not build. It goes through the ordinary `AverMap` path now, which is
/// defined, key-sorted and has the `len` the same program reaches for.
pub fn emit_builtin_call(
    name: &str,
    args: &[Spanned<ResolvedExpr>],
    ctx: &CodegenContext,
) -> Option<String> {
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
        // `Int.abs : Int -> Int` at the Aver surface, so it must lower to an
        // `Int`, not the `Nat`-typed `Int.natAbs`. The bare `.natAbs` only
        // type-checked where an enclosing `: Int` position coerced it; a
        // nested `Int.abs(Int.abs x)` became `(x.natAbs).natAbs` — `Nat`
        // has no `.natAbs`, a hard Lean build error. The ARGUMENT is ascribed
        // `({} : Int)` (not just the result): `.natAbs` is dot-notation, so
        // Lean resolves it from the receiver's type, and a bare numeric
        // literal sample (`Int.abs(-5)`) would otherwise default the receiver
        // to `Nat` (no `Nat.natAbs`). The outer `: Int` casts the `Nat` result
        // back so it stays Int-honest everywhere (`omega` / `Int.natAbs_*` see
        // through the `↑`). Mirrors the `(… : Int)`-coerced `*.len` lowering.
        IntAbs => format!("(({} : Int).natAbs : Int)", p(&a[0])),
        IntFromFloat => format!("AverFloat.toInt {}", p(&a[0])),
        IntMin => format!("min {} {}", p(&a[0]), p(&a[1])),
        IntMax => format!("max {} {}", p(&a[0]), p(&a[1])),
        // `Int.mod` / `Int.div` are partial: a zero divisor yields `Result.Err`.
        // Lean's `Int.%` / `Int./` are total (`a % 0 = a`, `a / 0 = 0` in v4.x),
        // so an unconditional `Except.ok` made `match Int.mod(a, 0) { Err -> }`
        // unreachable — a faithfulness bug that fails `native_decide` on any
        // verify case hitting the error path. Guard the divisor so the `.error`
        // arm is reachable; error string matches the runtime (`types/int.rs`).
        IntMod => format!(
            "(if {b} == 0 then Except.error \"division by zero\" else Except.ok ({a} % {b}) : Except String Int)",
            a = p(&a[0]),
            b = p(&a[1])
        ),
        IntDiv => format!(
            "(if {b} == 0 then Except.error \"division by zero\" else Except.ok ({a} / {b}) : Except String Int)",
            a = p(&a[0]),
            b = p(&a[1])
        ),
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

        // ---- Bits ----
        // A bit-level VIEW of `Int`, never a bit-vector: `AverBits.*` is
        // defined in the prelude over `Int` and `Nat`, so nothing here is
        // opaque or uninterpreted.
        BitsAnd => format!("AverBits.and {} {}", p(&a[0]), p(&a[1])),
        BitsOr => format!("AverBits.or {} {}", p(&a[0]), p(&a[1])),
        BitsXor => format!("AverBits.xor {} {}", p(&a[0]), p(&a[1])),
        BitsNot => format!("AverBits.not {}", p(&a[0])),
        // The three count-taking operations are partial below zero. GUARD
        // the count so the `.error` arm is reachable — the same shape
        // `Int.div` / `Int.mod` use for a zero divisor, and for the same
        // reason: an unconditional `Except.ok` makes a `match` on the error
        // arm unreachable in the model while the VM can still take it. The
        // error strings are verbatim from the runtime (`types/bits.rs`).
        BitsShiftLeft => bits_counted("shiftLeft", &a, "negative shift count"),
        BitsShiftRight => bits_counted("shiftRight", &a, "negative shift count"),
        BitsLow => bits_counted("low", &a, "negative bit width"),

        // ---- Bool ----
        BoolOr => format!("({} || {})", a[0], a[1]),
        BoolAnd => format!("({} && {})", a[0], a[1]),
        BoolNot => format!("(!{})", a[0]),

        // ---- Char ----
        CharToCode => format!("Char.toCode {}", p(&a[0])),
        CharFromCode => format!("Char.fromCode {}", p(&a[0])),

        // ---- Crypto ----
        CryptoSha256 => format!("Crypto.sha256 {}", p(&a[0])),

        // ---- String ----
        // `*.len` builtins return Aver `Int` (= ℤ), so their Lean lowering must
        // produce an `Int`, not a bare Lean `Nat`: dropping to `Nat` silently
        // changes the type (a spurious `Nat = Nat` proof for a `Nat`-vs-`Int`
        // law) AND mis-computes `Int` arithmetic (`len x - 5` is ≥0-truncated in
        // `Nat` but can be negative in `Int`). Cast to `Int` via the ascription
        // `(… : Int)`, which elaborates to the `Nat.cast`/`↑` coercion — the SAME
        // form the hand-written prelude lemmas use (`(s.length : Int)` in
        // String.slice_full etc.). Using `Int.ofNat` instead would be a distinct,
        // non-defeq term (`Int.ofNat n` ≠ `↑n` reducibly in Lean 4.31), clashing
        // with the prelude at any rfl-terminal step; matching `↑` avoids that
        // class entirely. `omega` is cast-aware, so length arithmetic still closes.
        StringLen => format!("({}.length : Int)", p(&a[0])),
        StringCharAt => format!("String.charAtAv {} {}", p(&a[0]), p(&a[1])),
        StringChars => format!("String.charsAv {}", p(&a[0])),
        StringSlice => format!("String.sliceAv {} {} {}", p(&a[0]), p(&a[1]), p(&a[2])),
        StringContains => format!("{}.containsSubstr {}", p(&a[0]), p(&a[1])),
        StringStartsWith => format!("{}.startsWith {}", p(&a[0]), p(&a[1])),
        StringEndsWith => format!("{}.endsWith {}", p(&a[0]), p(&a[1])),
        StringTrim => format!("{}.trim", p(&a[0])),
        StringSplit => format!("AverString.split {} {}", p(&a[0]), p(&a[1])),
        StringJoin => format!("String.intercalate {} {}", p(&a[1]), p(&a[0])),
        StringReplace => format!("{}.replace {} {}", p(&a[0]), p(&a[1]), p(&a[2])),
        StringToUpper => format!("{}.toUpper", p(&a[0])),
        StringToLower => format!("{}.toLower", p(&a[0])),
        StringFromInt => format!("String.fromInt {}", p(&a[0])),
        StringFromFloat => format!("String.fromFloat {}", p(&a[0])),
        StringFromBool => format!("String.fromBool {}", p(&a[0])),
        StringByteLength => format!("{}.utf8ByteSize", p(&a[0])),

        // ---- List ----
        ListLen => {
            let subj = emit_list_length_subject(&args[0], ctx);
            format!("({}.length : Int)", subj)
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
        VectorNew => format!("Array.replicate (Int.toNat {}) {}", p(&a[0]), p(&a[1])),
        // `Array.get?` was removed in Lean 4.31 in favour of the `GetElem?`
        // bracket notation `arr[i]?`, which reduces under kernel `decide`
        // (so a finite-domain table law over a `Vector` enumerates cleanly).
        VectorGet => format!("{}[Int.toNat {}]?", p(&a[0]), p(&a[1])),
        VectorSet => format!(
            "if {} < {}.size then some ({}.set! (Int.toNat {}) {}) else none",
            p(&a[1]),
            p(&a[0]),
            p(&a[0]),
            p(&a[1]),
            p(&a[2])
        ),
        VectorLen => format!("({}.size : Int)", p(&a[0])),
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
        MapLen => format!("((AverMap.len {}) : Int)", p(&a[0])),
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
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "test".to_string(),
            modules: vec![],
            capabilities: Default::default(),
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
            packed_sequence_layouts: HashMap::new(),
            proof_ir: crate::ir::ProofIR::default(),
            symbol_table: crate::ir::SymbolTable::default(),
            resolved_fn_defs: Vec::new(),
            resolved_module_fn_defs: Vec::new(),
            current_module_scope: std::cell::RefCell::new(None),
            lean_do_block: std::cell::Cell::new(false),
            declined_claims: std::cell::RefCell::new(std::collections::BTreeMap::new()),
            resolved_program: crate::codegen::program_view::ResolvedProgramView::default(),
            program_shape: None,
            mir_program: None,
            bare_i64: Default::default(),
            discovered_lemmas: Vec::new(),
            sample_expected: std::collections::HashMap::new(),
            allow_mathlib: false,
            hand_proofs: Default::default(),
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
        let emitted = emit_builtin_call(
            "List.len",
            &[Spanned::bare(ResolvedExpr::List(vec![]))],
            &ctx,
        )
        .expect("List.len should be emitted");

        // `List.len` returns Aver `Int`, so the Lean lowering casts to `Int` via the
        // `(… : Int)` ascription (the `↑`/Nat.cast form the prelude uses), not a bare
        // Lean `Nat`; the empty-list subject still carries its `: List Unit` annotation.
        assert_eq!(emitted, "((([] : List Unit)).length : Int)");
    }

    #[test]
    fn vector_new_uses_the_lean_432_array_constructor() {
        let ctx = empty_ctx();
        let emitted = emit_builtin_call(
            "Vector.new",
            &[
                Spanned::bare(ResolvedExpr::Literal(Literal::Int(3))),
                Spanned::bare(ResolvedExpr::Literal(Literal::Int(0))),
            ],
            &ctx,
        )
        .expect("Vector.new should be emitted");

        assert_eq!(emitted, "Array.replicate (Int.toNat 3) 0");
    }
}
