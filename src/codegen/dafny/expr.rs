/// Aver expressions → Dafny expression strings.
use crate::ast::{BinOp, Literal, Spanned};
use crate::codegen::CodegenContext;
use crate::codegen::common::{is_user_type, resolve_module_call};
use crate::ir::hir::{
    BuiltinCtor, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedMatchArm, ResolvedPattern,
    ResolvedStrPart,
};

/// Dafny reserved words.
const DAFNY_RESERVED: &[&str] = &[
    "abstract",
    "allocated",
    "as",
    "assert",
    "assume",
    "bool",
    "break",
    "by",
    "calc",
    "case",
    "char",
    "class",
    "codatatype",
    "colemma",
    "constructor",
    "copredicate",
    "datatype",
    "decreases",
    "default",
    "else",
    "ensures",
    "exists",
    "expect",
    "export",
    "extends",
    "false",
    "forall",
    "fresh",
    "function",
    "ghost",
    "if",
    "import",
    "in",
    "include",
    "int",
    "invariant",
    "is",
    "iterator",
    "label",
    "lemma",
    "map",
    "match",
    "method",
    "modifies",
    "modify",
    "module",
    "multiset",
    "nat",
    "new",
    "newtype",
    "null",
    "object",
    "old",
    "opened",
    "predicate",
    "print",
    "provides",
    "reads",
    "real",
    "refines",
    "requires",
    "return",
    "returns",
    "reveal",
    "reveals",
    "seq",
    "set",
    "static",
    "string",
    "then",
    "this",
    "trait",
    "true",
    "twostate",
    "type",
    "unchanged",
    "var",
    "while",
    "witness",
    "yield",
    "yields",
];

/// Convert an Aver identifier to a valid Dafny name. Aver allows
/// underscore-prefixed names like `_pmy` (idiomatic "intentionally
/// unused"); Dafny rejects identifiers that begin with `_` AND have
/// further characters. Pure `_` (Dafny's wildcard) is fine. We rewrite
/// `_pmy` → `aver_pmy` but leave bare `_` untouched.
pub fn aver_name_to_dafny(name: &str) -> String {
    let stripped = name.trim_start_matches('_');
    let normalized = if stripped.is_empty() {
        // Pure `_` (or `__`...): Dafny treats this as wildcard.
        name.to_string()
    } else if stripped.len() < name.len() {
        format!("aver_{}", stripped)
    } else {
        name.to_string()
    };
    crate::codegen::common::escape_reserved_word(&normalized, DAFNY_RESERVED, "_")
}

/// Emit a Dafny expression from a resolved Aver expression.
pub fn emit_expr(expr: &Spanned<ResolvedExpr>, ctx: &CodegenContext) -> String {
    match &expr.node {
        ResolvedExpr::Literal(lit) => emit_literal(lit),
        ResolvedExpr::Ident(name) | ResolvedExpr::Resolved { name, .. } => aver_name_to_dafny(name),
        ResolvedExpr::Attr(obj, field) => {
            // Refinement-via-opaque records emit as Dafny subset types,
            // so projecting the carrier field is the identity (the
            // value *is* the underlying `int`).
            if let Some(ty) = obj.ty()
                && let Some(decl) = crate::codegen::common::find_refined_type_for_named(ctx, ty)
                && field == &decl.carrier_field
            {
                return emit_expr(obj, ctx);
            }
            if let ResolvedExpr::Ident(type_name) = &obj.node {
                if type_name == "Option" && field == "None" {
                    return "Option.None".to_string();
                }
                // Oracle v1: `BranchPath.Root` — nullary value,
                // matches the `const BranchPath_Root` in the prelude.
                if type_name == "BranchPath" && field == "Root" {
                    return "BranchPath_Root".to_string();
                }
            }
            // Module-qualified call/access: must be checked before
            // `is_user_type` because Aver allows `module Enemy` to coexist
            // with `record Enemy`. If the head is a known module prefix,
            // route through the renamed Dafny module (`Aver_Enemy.fn`).
            if let Some(full_dotted) = crate::ir::hir::resolved_to_dotted(&expr.node)
                && let Some((prefix, bare)) = resolve_module_call(&full_dotted, ctx)
            {
                if let Some(dot_pos) = bare.find('.') {
                    let type_name = &bare[..dot_pos];
                    let variant = &bare[dot_pos + 1..];
                    if is_user_type(type_name, ctx) {
                        return format!("{}.{}", type_name, variant);
                    }
                }
                let bare_dafny = aver_name_to_dafny(bare);
                if !ctx.modules.is_empty() {
                    return format!("{}.{}", super::dafny_module_name(prefix), bare_dafny);
                }
                return bare_dafny;
            }
            if let ResolvedExpr::Ident(type_name) = &obj.node
                && is_user_type(type_name, ctx)
            {
                return format!("{}.{}", type_name, field);
            }
            let obj_str = emit_expr(obj, ctx);
            format!("{}.{}", obj_str, aver_name_to_dafny(field))
        }
        ResolvedExpr::Call(callee, args) => emit_fn_call(callee, args, ctx),
        ResolvedExpr::Neg(inner) => {
            // Dafny accepts `(-x)` uniformly for `int` and `real`,
            // unlike `0 - x` which fails when `x` is real (the literal
            // `0` is int and Dafny rejects mixed-type arithmetic).
            format!("(-{})", emit_expr(inner, ctx))
        }
        ResolvedExpr::BinOp(op, left, right) => {
            let l = emit_expr(left, ctx);
            let r = emit_expr(right, ctx);
            // Float `/` lowers via `FloatDiv` so Aver's IEEE-754
            // "no runtime crash, divide-by-zero yields a defined
            // value" semantics carry into Dafny's exact-rational
            // `real`. Without the helper Dafny imposes a `b != 0`
            // obligation on every caller and breaks proofs whose
            // domain analysis depends on a downstream postcondition
            // (e.g. `goldenApprox` needing `fib(n) >= 1` for n >= 1).
            if matches!(op, BinOp::Div) && matches!(left.ty(), Some(crate::types::Type::Float)) {
                return format!("FloatDiv({}, {})", l, r);
            }
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            format!("({} {} {})", l, op_str, r)
        }
        ResolvedExpr::Match { subject, arms } => emit_match(subject, arms, ctx),
        ResolvedExpr::Ctor(ctor, args) => emit_constructor(ctor, args, expr.ty(), ctx),
        ResolvedExpr::ErrorProp(_) => {
            // ? operator requires early-return semantics (Err propagation).
            // Dafny pure functions cannot express this; functions using ? are
            // skipped at the top-level emission stage. If we get here, emit
            // a marker that makes the generated Dafny obviously wrong rather
            // than silently modelling a different program.
            "/* ERROR: ? operator not supported in Dafny pure functions */".to_string()
        }
        ResolvedExpr::InterpolatedStr(parts) => emit_interpolated_str(parts, ctx),
        ResolvedExpr::List(elems) => {
            let items: Vec<String> = elems.iter().map(|e| emit_expr(e, ctx)).collect();
            format!("[{}]", items.join(", "))
        }
        ResolvedExpr::Tuple(elems) | ResolvedExpr::IndependentProduct(elems, _) => {
            // Oracle v1: plain `!` lifts to a Dafny tuple — schedule
            // invariance is a compiler-level claim, no extra machinery at
            // the expression site. `?!` also passes through the tuple form
            // here (typechecker models `?!` as the Ok-short-circuit, so
            // body-type is the unwrapped tuple); proper Result-fold
            // emission for `?!` with explicit Err propagation is deferred
            // — it needs a coordinated rewrite with the enclosing
            // `Result.Ok(...)` wrapper the typechecker forces on function
            // returns.
            let items: Vec<String> = elems.iter().map(|e| emit_expr(e, ctx)).collect();
            format!("({})", items.join(", "))
        }
        ResolvedExpr::MapLiteral(entries) => {
            if entries.is_empty() {
                "map[]".to_string()
            } else if entries
                .iter()
                .all(|(_, v)| crate::codegen::common::is_unit_expr_resolved(&v.node))
            {
                // Map<T, Unit> literal → set literal
                let items: Vec<String> = entries.iter().map(|(k, _)| emit_expr(k, ctx)).collect();
                format!("{{{}}}", items.join(", "))
            } else {
                let items: Vec<String> = entries
                    .iter()
                    .map(|(k, v)| format!("{} := {}", emit_expr(k, ctx), emit_expr(v, ctx)))
                    .collect();
                format!("map[{}]", items.join(", "))
            }
        }
        ResolvedExpr::RecordCreate {
            type_name, fields, ..
        } => {
            // Refinement records admitted by the proof lowerer emit as a
            // subset type (`type X = value: T | P(value)`), so
            // `X(value := carrier)` collapses to the carrier expression.
            // Dafny narrowing (via `if pred then ... else ...`) is
            // what closes the refinement obligation at the call site.
            if crate::codegen::common::find_refined_type(ctx, type_name).is_some()
                && fields.len() == 1
            {
                let (_, value_expr) = &fields[0];
                return emit_expr(value_expr, ctx);
            }
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_dafny(name), emit_expr(expr, ctx))
                })
                .collect();
            // Datatype-constructor reference. Built-in records with
            // dotted names (`Terminal.Size`, `Tcp.Connection`) flatten
            // to underscore form because the prelude declares them as
            // `Terminal_Size` / `Tcp_Connection`. A user type from a
            // DIFFERENT module is qualified `Aver_<module>.<Ctor>` so
            // the qualifier matches the renamed Dafny module; a type in
            // the module currently being emitted stays BARE (the
            // resolver already hands back a bare name there, and a
            // module name is not in scope for self-qualification). This
            // mirrors `toplevel::type_to_dafny`'s `Type::Named` arm so
            // constructor references agree with type references.
            let active = ctx.active_module_scope();
            let dafny_type_name = if crate::codegen::builtin_records::find(type_name).is_some() {
                type_name.replace('.', "_")
            } else if let Some(dot) = type_name.rfind('.') {
                let module_part = &type_name[..dot];
                let local = &type_name[dot + 1..];
                if active.as_deref() == Some(module_part) {
                    local.to_string()
                } else {
                    format!("Aver_{}.{}", module_part.replace('.', "_"), local)
                }
            } else {
                type_name.to_string()
            };
            format!("{}({})", dafny_type_name, field_strs.join(", "))
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            let base_str = emit_expr(base, ctx);
            let update_strs: Vec<String> = updates
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_dafny(name), emit_expr(expr, ctx))
                })
                .collect();
            format!("{}.({})", base_str, update_strs.join(", "))
        }
        ResolvedExpr::TailCall { target, args } => {
            let entry = ctx.symbol_table.fn_entry(*target);
            let name = entry.key.name.as_str();
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
            format!("{}({})", aver_name_to_dafny(name), arg_strs.join(", "))
        }
    }
}

pub(super) fn emit_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(n) => n.to_string(),
        // Dafny's `int` is arbitrary-precision: emit the decimal digits directly.
        Literal::BigInt(s) => s.clone(),
        Literal::Float(f) => {
            let s = f.to_string();
            if s.contains('.') {
                format!("{} as real", s)
            } else {
                format!("{}.0 as real", s)
            }
        }
        Literal::Str(s) => {
            format!(
                "\"{}\"",
                crate::codegen::common::escape_string_literal_unicode(s)
            )
        }
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn emit_fn_call(
    callee: &ResolvedCallee,
    args: &[Spanned<ResolvedExpr>],
    ctx: &CodegenContext,
) -> String {
    use crate::codegen::builtins::recognize_builtin;
    use crate::codegen::common::is_unit_expr_resolved;

    match callee {
        ResolvedCallee::Builtin(name) => {
            // Map<T, Unit> set operations: intercept before generic builtin path
            if name == "Map.set" && args.len() == 3 && is_unit_expr_resolved(&args[2].node) {
                let m = emit_expr(&args[0], ctx);
                let k = emit_expr(&args[1], ctx);
                return format!("({} + {{{}}})", m, k);
            }
            if let Some(builtin) = recognize_builtin(name) {
                let a: Vec<String> = args.iter().map(|e| emit_expr(e, ctx)).collect();
                return emit_dafny_builtin(builtin, &a);
            }
            // Oracle v1: BranchPath.* constructor calls map onto the
            // underscore-named prelude functions (Dafny's dotted
            // notation collides with record-member access on the
            // BranchPath datatype).
            let a: Vec<String> = args.iter().map(|e| emit_expr(e, ctx)).collect();
            match name.as_str() {
                "BranchPath.child" if a.len() == 2 => {
                    return format!("BranchPath_child({}, {})", a[0], a[1]);
                }
                "BranchPath.parse" if a.len() == 1 => {
                    return format!("BranchPath_parse({})", a[0]);
                }
                _ => {}
            }
            // Generic builtin fallback — render as dotted call.
            format!("{}({})", aver_name_to_dafny(name), a.join(", "))
        }
        ResolvedCallee::Intrinsic(intr) => {
            use crate::ir::hir::BuiltinIntrinsic;
            let a: Vec<String> = args.iter().map(|e| emit_expr(e, ctx)).collect();
            // Literal-divisor discharge: for a nonzero literal divisor
            // these are total, and Dafny's `/` / `%` on `int` are Euclidean
            // (matching the runtime for every sign combination), so render
            // the bare op. The HIR resolver produces these intrinsics for
            // every discharged source call.
            match intr {
                BuiltinIntrinsic::IntDivEuclid if a.len() == 2 => {
                    format!("({} / {})", a[0], a[1])
                }
                BuiltinIntrinsic::IntModEuclid if a.len() == 2 => {
                    format!("({} % {})", a[0], a[1])
                }
                // Literal-count discharge: total for a non-negative literal
                // count, so render the bare prelude function unwrapped.
                BuiltinIntrinsic::BitsShiftLeft if a.len() == 2 => {
                    format!("BitsShiftLeft({}, {})", a[0], a[1])
                }
                BuiltinIntrinsic::BitsShiftRight if a.len() == 2 => {
                    format!("BitsShiftRight({}, {})", a[0], a[1])
                }
                BuiltinIntrinsic::BitsLow if a.len() == 2 => {
                    format!("BitsLow({}, {})", a[0], a[1])
                }
                // Compiler-synthesised `__buf_*` / `__to_str` intrinsics
                // don't reach the Dafny backend in practice (Dafny emit
                // doesn't see post-interp-lower buffer shapes), but the
                // resolver carries them through; render as bare-name call.
                _ => format!("{}({})", intr.name(), a.join(", ")),
            }
        }
        ResolvedCallee::Fn(fn_id) => {
            let entry = ctx.symbol_table.fn_entry(*fn_id);
            let bare = entry.key.name.as_str();
            let module_prefix = entry.key.scope_str();
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
            // A call to a fn in a DIFFERENT module is qualified with the
            // Dafny module name (`Aver_Domain_Rational.f`). A same-module
            // self-reference must stay BARE: inside a Dafny
            // `module M { ... }` the name `M` is not in scope for
            // self-qualification, so `M.f(...)` is an unresolved
            // identifier. Compare the callee's owning scope against the
            // module currently being emitted.
            let active = ctx.active_module_scope();
            let func = match module_prefix {
                Some(prefix) if !ctx.modules.is_empty() && active.as_deref() != Some(prefix) => {
                    format!(
                        "{}.{}",
                        super::dafny_module_name(prefix),
                        aver_name_to_dafny(bare)
                    )
                }
                _ => aver_name_to_dafny(bare),
            };
            format!("{}({})", func, arg_strs.join(", "))
        }
        ResolvedCallee::LocalSlot { name, .. } => {
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
            format!("{}({})", aver_name_to_dafny(name), arg_strs.join(", "))
        }
        ResolvedCallee::Unresolved { callee: inner } => {
            // Typecheck-rejected callee — render the source-faithful
            // expression as a regular call so the surrounding Dafny
            // module still parses (verify driver surfaces the missing
            // target separately).
            let func = emit_expr(inner, ctx);
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
            format!("{}({})", func, arg_strs.join(", "))
        }
    }
}

/// A count-taking `Bits` operation with its negative-count guard.
fn dafny_bits_counted(op: &str, a: &[String], message: &str) -> String {
    format!(
        "(if {n} < 0 then Result<int, string>.Err(\"{message}\") else Result<int, string>.Ok({op}({x}, {n})))",
        x = a[0],
        n = a[1]
    )
}

pub(super) fn emit_dafny_builtin(b: crate::codegen::builtins::Builtin, a: &[String]) -> String {
    use crate::codegen::builtins::Builtin::*;
    match b {
        // Constructors
        ResultOk => format!("Result.Ok({})", a.first().map(|s| s.as_str()).unwrap_or("")),
        ResultErr => format!(
            "Result.Err({})",
            a.first().map(|s| s.as_str()).unwrap_or("")
        ),
        OptionSome => format!(
            "Option.Some({})",
            a.first().map(|s| s.as_str()).unwrap_or("")
        ),

        // Combinators
        ResultWithDefault => format!("ResultWithDefault({}, {})", a[0], a[1]),
        OptionWithDefault => format!("OptionWithDefault({}, {})", a[0], a[1]),
        OptionToResult => format!("OptionToResult({}, {})", a[0], a[1]),

        // Int
        IntAbs => format!("(if {} >= 0 then {} else -{})", a[0], a[0], a[0]),
        IntFromFloat => format!("({} as int)", a[0]),
        StringFromInt => format!("IntToString({})", a[0]),
        IntFromString => format!("IntFromString({})", a[0]),
        IntMin => format!("(if {} <= {} then {} else {})", a[0], a[1], a[0], a[1]),
        IntMax => format!("(if {} >= {} then {} else {})", a[0], a[1], a[0], a[1]),
        // `Int.mod` / `Int.div` are partial: a zero divisor is `Result.Err`.
        // Dafny's `%` / `/` carry a `b != 0` precondition, so an unconditional
        // `Ok((a % b))` raised "possible division by zero" whenever the divisor
        // wasn't provably non-zero. Guarding the divisor both discharges that
        // obligation and makes the error path reachable for `match` on the
        // result (error string matches the runtime — see `types/int.rs`).
        IntMod => format!(
            "(if {b} == 0 then Result<int, string>.Err(\"division by zero\") else Result<int, string>.Ok(({a} % {b})))",
            a = a[0],
            b = a[1]
        ),
        IntDiv => format!(
            "(if {b} == 0 then Result<int, string>.Err(\"division by zero\") else Result<int, string>.Ok(({a} / {b})))",
            a = a[0],
            b = a[1]
        ),

        // Float
        FloatAbs => format!("(if {} >= 0.0 then {} else -{})", a[0], a[0], a[0]),
        FloatSqrt => format!("FloatSqrt({})", a[0]),
        FloatPow => format!("FloatPow({}, {})", a[0], a[1]),
        FloatRound | FloatFloor | FloatCeil => format!("FloatToInt({})", a[0]),
        FloatFromInt => format!("({} as real)", a[0]),
        StringFromFloat => format!("FloatToString({})", a[0]),
        FloatFromString => format!("FloatFromString({})", a[0]),
        FloatPi => "FloatPi()".to_string(),
        FloatMin => format!("(if {} <= {} then {} else {})", a[0], a[1], a[0], a[1]),
        FloatMax => format!("(if {} >= {} then {} else {})", a[0], a[1], a[0], a[1]),
        FloatSin => format!("FloatSin({})", a[0]),
        FloatCos => format!("FloatCos({})", a[0]),
        FloatAtan2 => format!("FloatAtan2({}, {})", a[0], a[1]),

        // String
        StringLen => format!("|{}|", a[0]),
        StringCharAt => format!("StringCharAt({}, {})", a[0], a[1]),
        StringChars => format!("StringChars({})", a[0]),
        StringSlice => format!("StringSlice({}, {}, {})", a[0], a[1], a[2]),
        StringContains => format!("StringContains({}, {})", a[0], a[1]),
        StringStartsWith => format!("StringStartsWith({}, {})", a[0], a[1]),
        StringEndsWith => format!("StringEndsWith({}, {})", a[0], a[1]),
        StringTrim => format!("StringTrim({})", a[0]),
        StringSplit => format!("StringSplit({}, {})", a[0], a[1]),
        StringJoin => format!("StringJoin({}, {})", a[1], a[0]), // Aver: join(list, sep)
        StringReplace => format!("StringReplace({}, {}, {})", a[0], a[1], a[2]),
        StringToUpper => format!("StringToUpper({})", a[0]),
        StringToLower => format!("StringToLower({})", a[0]),
        StringFromBool => format!("StringFromBool({})", a[0]),
        StringByteLength => format!("StringByteLength({})", a[0]),

        // Bits — a bit-level VIEW of `int`. Dafny has no bitwise operators
        // on `int` at all (only on fixed-width `bv` types), and translating
        // an unbounded `Int` to a bit-vector would change the semantics, so
        // the prelude DEFINES these recursively over the two's-complement
        // magnitude. Same case split as the Lean model.
        BitsAnd => format!("BitsAnd({}, {})", a[0], a[1]),
        BitsOr => format!("BitsOr({}, {})", a[0], a[1]),
        BitsXor => format!("BitsXor({}, {})", a[0], a[1]),
        BitsNot => format!("BitsNot({})", a[0]),
        // Guard the count so the `Err` arm is reachable, mirroring
        // `Int.div` / `Int.mod`; error strings match the runtime
        // (`types/bits.rs`).
        BitsShiftLeft => dafny_bits_counted("BitsShiftLeft", a, "negative shift count"),
        BitsShiftRight => dafny_bits_counted("BitsShiftRight", a, "negative shift count"),
        BitsLow => dafny_bits_counted("BitsLow", a, "negative bit width"),

        // Bool
        BoolOr => format!("({} || {})", a[0], a[1]),
        BoolAnd => format!("({} && {})", a[0], a[1]),
        BoolNot => format!("(!{})", a[0]),

        // Char
        CharToCode => format!("CharToCode({})", a[0]),
        CharFromCode => format!("CharFromCode({})", a[0]),

        // Crypto
        CryptoSha256 => format!("Aver_Crypto.sha256({})", a[0]),

        // List
        // An empty-list literal has no element-type context in Dafny
        // (seq<?>), which makes `|[]|` fail resolver with "type of this
        // expression is underspecified". The length is trivially 0
        // regardless of element type, so short-circuit the emission.
        ListLen => {
            if a[0].trim() == "[]" {
                "0".to_string()
            } else {
                format!("|{}|", a[0])
            }
        }
        ListHead => format!("ListHead({})", a[0]),
        ListTail => format!("ListTail({})", a[0]),
        ListPrepend => format!("[{}] + {}", a[0], a[1]),
        ListTake => format!("ListTake({}, {})", a[0], a[1]),
        ListDrop => format!("ListDrop({}, {})", a[0], a[1]),
        ListConcat => format!("({} + {})", a[0], a[1]),
        ListReverse => format!("ListReverse({})", a[0]),
        ListContains => format!("({} in {})", a[1], a[0]),
        ListFind => format!("ListFind({}, {})", a[0], a[1]),
        ListAny => format!("ListAny({}, {})", a[0], a[1]),
        ListZip => format!("ListZip({}, {})", a[0], a[1]),

        // Vector (maps to seq in Dafny — same as List but with indexed access)
        VectorNew => format!("seq({}, _ => {})", a[0], a[1]),
        VectorGet => format!(
            "if 0 <= {} < |{}| then Some({}[{}]) else None",
            a[1], a[0], a[0], a[1]
        ),
        VectorSet => format!(
            "if 0 <= {} < |{}| then Some({}[{} := {}]) else None",
            a[1], a[0], a[0], a[1], a[2]
        ),
        VectorLen => format!("|{}|", a[0]),
        VectorFromList => a[0].clone(),
        ListFromVector => a[0].clone(),

        // Map
        MapGet => format!("MapGet({}, {})", a[0], a[1]),
        MapSet => format!("{}[{} := {}]", a[0], a[1], a[2]),
        MapHas => format!("({} in {})", a[1], a[0]),
        MapRemove => format!("({} - {{{}}})", a[0], a[1]),
        MapKeys => format!("MapKeys({})", a[0]),
        MapValues => format!("MapValues({})", a[0]),
        MapEntries => format!("MapEntries({})", a[0]),
        MapLen => format!("|{}|", a[0]),
        MapFromList => format!("MapFromList({})", a[0]),
    }
}

fn emit_match(
    subject: &Spanned<ResolvedExpr>,
    arms: &[ResolvedMatchArm],
    ctx: &CodegenContext,
) -> String {
    // Check if this is a list-pattern match (EmptyList / Cons arms)
    if has_list_patterns(arms) {
        return emit_list_match(subject, arms, ctx);
    }

    // Bool match: `true -> ..., false -> ...` → `if subj then ... else ...`
    if is_bool_match(arms) {
        return emit_bool_match(subject, arms, ctx);
    }

    // Scalar match (int literals, wildcards) → if-then-else chain.
    // This helps Dafny's verifier see guards for termination proofs.
    if should_emit_as_if_chain(arms) {
        return emit_if_chain(subject, arms, ctx);
    }

    let subj = emit_expr(subject, ctx);
    let mut lines = Vec::new();
    lines.push(format!("match {}", subj));

    for arm in arms {
        let pat = emit_pattern(&arm.pattern);
        let body = emit_expr(&arm.body, ctx);
        lines.push(format!("  case {} => {}", pat, body));
    }

    format!("({})", lines.join(" "))
}

/// Should we emit this match as an if-then-else chain?
/// Yes for matches on scalar values (int, bool, string literals) and wildcards.
fn should_emit_as_if_chain(arms: &[ResolvedMatchArm]) -> bool {
    arms.iter().all(|arm| {
        matches!(
            arm.pattern,
            ResolvedPattern::Literal(_) | ResolvedPattern::Wildcard | ResolvedPattern::Ident(_)
        )
    })
}

/// Check if arms form a bool match: `true -> ..., false -> ...` (in either order).
fn is_bool_match(arms: &[ResolvedMatchArm]) -> bool {
    if arms.len() != 2 {
        return false;
    }
    let has_true = arms
        .iter()
        .any(|a| matches!(&a.pattern, ResolvedPattern::Literal(Literal::Bool(true))));
    let has_false = arms
        .iter()
        .any(|a| matches!(&a.pattern, ResolvedPattern::Literal(Literal::Bool(false))));
    has_true && has_false
}

/// Emit a bool match as `if subject then true_body else false_body`.
fn emit_bool_match(
    subject: &Spanned<ResolvedExpr>,
    arms: &[ResolvedMatchArm],
    ctx: &CodegenContext,
) -> String {
    let subj = emit_expr(subject, ctx);
    let true_arm = arms
        .iter()
        .find(|a| matches!(&a.pattern, ResolvedPattern::Literal(Literal::Bool(true))))
        .unwrap();
    let false_arm = arms
        .iter()
        .find(|a| matches!(&a.pattern, ResolvedPattern::Literal(Literal::Bool(false))))
        .unwrap();
    let true_body = emit_expr(&true_arm.body, ctx);
    let false_body = emit_expr(&false_arm.body, ctx);
    format!("(if {} then {} else {})", subj, true_body, false_body)
}

/// Emit a match as a Dafny if-then-else chain.
fn emit_if_chain(
    subject: &Spanned<ResolvedExpr>,
    arms: &[ResolvedMatchArm],
    ctx: &CodegenContext,
) -> String {
    let subj = emit_expr(subject, ctx);
    emit_if_chain_inner(&subj, arms, 0, ctx)
}

fn emit_if_chain_inner(
    subj: &str,
    arms: &[ResolvedMatchArm],
    idx: usize,
    ctx: &CodegenContext,
) -> String {
    if idx >= arms.len() {
        return "/* unreachable */".to_string();
    }

    let arm = &arms[idx];
    let body = emit_expr(&arm.body, ctx);

    match &arm.pattern {
        ResolvedPattern::Wildcard | ResolvedPattern::Ident(_) => {
            if let ResolvedPattern::Ident(name) = &arm.pattern {
                format!("(var {} := {}; {})", aver_name_to_dafny(name), subj, body)
            } else {
                body
            }
        }
        ResolvedPattern::Literal(lit) => {
            let rest = emit_if_chain_inner(subj, arms, idx + 1, ctx);

            let lit_str = emit_literal(lit);
            format!("(if {} == {} then {} else {})", subj, lit_str, body, rest)
        }
        _ => {
            let pat = emit_pattern(&arm.pattern);
            format!("/* unsupported pattern: {} */ {}", pat, body)
        }
    }
}

/// Check if any arm uses list patterns (EmptyList or Cons).
fn has_list_patterns(arms: &[ResolvedMatchArm]) -> bool {
    arms.iter().any(|arm| {
        matches!(
            arm.pattern,
            ResolvedPattern::EmptyList | ResolvedPattern::Cons(_, _)
        )
    })
}

/// Emit a match on a list (seq) as if-then-else with seq operations.
fn emit_list_match(
    subject: &Spanned<ResolvedExpr>,
    arms: &[ResolvedMatchArm],
    ctx: &CodegenContext,
) -> String {
    let subj = emit_expr(subject, ctx);

    // Find empty-list arm and cons arm
    let empty_arm = arms
        .iter()
        .find(|a| matches!(a.pattern, ResolvedPattern::EmptyList));
    let cons_arm = arms
        .iter()
        .find(|a| matches!(a.pattern, ResolvedPattern::Cons(_, _)));
    let wildcard_arm = arms.iter().find(|a| {
        matches!(
            a.pattern,
            ResolvedPattern::Wildcard | ResolvedPattern::Ident(_)
        )
    });

    let empty_body = if let Some(arm) = empty_arm {
        emit_expr(&arm.body, ctx)
    } else if let Some(arm) = wildcard_arm {
        emit_expr(&arm.body, ctx)
    } else {
        "/* missing empty case */".to_string()
    };

    let cons_body = if let Some(arm) = cons_arm {
        if let ResolvedPattern::Cons(head, tail) = &arm.pattern {
            let head_name = aver_name_to_dafny(head);
            let tail_name = aver_name_to_dafny(tail);
            let body = emit_expr(&arm.body, ctx);
            format!(
                "var {} := {}[0]; var {} := {}[1..]; {}",
                head_name, subj, tail_name, subj, body
            )
        } else {
            unreachable!()
        }
    } else if let Some(arm) = wildcard_arm {
        emit_expr(&arm.body, ctx)
    } else {
        "/* missing cons case */".to_string()
    };

    format!(
        "(if |{}| == 0 then {} else {})",
        subj, empty_body, cons_body
    )
}

pub(crate) fn emit_pattern(pattern: &ResolvedPattern) -> String {
    match pattern {
        ResolvedPattern::Wildcard => "_".to_string(),
        ResolvedPattern::Literal(lit) => emit_literal(lit),
        ResolvedPattern::Ident(name) => aver_name_to_dafny(name),
        ResolvedPattern::EmptyList => "Nil".to_string(),
        ResolvedPattern::Cons(head, tail) => {
            format!(
                "Cons({}, {})",
                aver_name_to_dafny(head),
                aver_name_to_dafny(tail)
            )
        }
        ResolvedPattern::Tuple(pats) => {
            let subs: Vec<String> = pats.iter().map(emit_pattern).collect();
            format!("({})", subs.join(", "))
        }
        ResolvedPattern::Ctor(ctor, bindings) => emit_ctor_pattern(ctor, bindings),
    }
}

fn emit_ctor_pattern(ctor: &ResolvedCtor, bindings: &[String]) -> String {
    let variant = match ctor {
        ResolvedCtor::Builtin(BuiltinCtor::ResultOk) => "Ok".to_string(),
        ResolvedCtor::Builtin(BuiltinCtor::ResultErr) => "Err".to_string(),
        ResolvedCtor::Builtin(BuiltinCtor::OptionSome) => "Some".to_string(),
        ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => "None".to_string(),
        ResolvedCtor::User { name, .. } => {
            if let Some(dot_pos) = name.rfind('.') {
                name[dot_pos + 1..].to_string()
            } else {
                name.clone()
            }
        }
        ResolvedCtor::Unresolved { name } => {
            if let Some(dot_pos) = name.rfind('.') {
                name[dot_pos + 1..].to_string()
            } else {
                name.clone()
            }
        }
    };
    if bindings.is_empty() {
        variant
    } else {
        let subs: Vec<String> = bindings.iter().map(|b| aver_name_to_dafny(b)).collect();
        format!("{}({})", variant, subs.join(", "))
    }
}

fn emit_constructor(
    ctor: &ResolvedCtor,
    args: &[Spanned<ResolvedExpr>],
    result_type: Option<&crate::types::Type>,
    ctx: &CodegenContext,
) -> String {
    // In Dafny expression context, qualify constructors to avoid
    // ambiguity. User-defined types and the built-in `Result` /
    // `Option` are kept fully qualified — the latter because user code
    // can declare its own `enum ParseResult { Ok, Err }` with the same
    // variant names, and Dafny needs the discriminator to pick the
    // right datatype.
    let explicit_wrapper = result_type
        .filter(|ty| type_contains_refinement(ty, ctx))
        .map(super::toplevel::emit_type_from);
    let qualified = match ctor {
        ResolvedCtor::Builtin(BuiltinCtor::ResultOk) => explicit_wrapper
            .as_ref()
            .map_or_else(|| "Result.Ok".to_string(), |ty| format!("{}.Ok", ty)),
        ResolvedCtor::Builtin(BuiltinCtor::ResultErr) => explicit_wrapper
            .as_ref()
            .map_or_else(|| "Result.Err".to_string(), |ty| format!("{}.Err", ty)),
        ResolvedCtor::Builtin(BuiltinCtor::OptionSome) => explicit_wrapper
            .as_ref()
            .map_or_else(|| "Option.Some".to_string(), |ty| format!("{}.Some", ty)),
        ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => {
            return explicit_wrapper
                .map_or_else(|| "Option.None".to_string(), |ty| format!("{}.None", ty));
        }
        ResolvedCtor::User { type_id, name, .. } => {
            let type_entry = ctx.symbol_table.type_entry(*type_id);
            let type_name = type_entry.key.name.as_str();
            let variant = if let Some(dot_pos) = name.rfind('.') {
                &name[dot_pos + 1..]
            } else {
                name.as_str()
            };
            if is_user_type(type_name, ctx) {
                format!("{}.{}", type_name, variant)
            } else {
                variant.to_string()
            }
        }
        ResolvedCtor::Unresolved { name } => {
            let (type_name, variant) = if let Some(dot_pos) = name.rfind('.') {
                (&name[..dot_pos], &name[dot_pos + 1..])
            } else {
                ("", name.as_str())
            };
            if is_user_type(type_name, ctx) || type_name == "Result" || type_name == "Option" {
                format!("{}.{}", type_name, variant)
            } else {
                variant.to_string()
            }
        }
    };
    if args.is_empty() {
        qualified
    } else {
        let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
        format!("{}({})", qualified, arg_strs.join(", "))
    }
}

/// Dafny infers datatype constructor parameters from their arguments before
/// applying the surrounding function's expected return type. A refinement
/// record is emitted as its carrier expression, so `Result.Ok(Bytes(...))`
/// would otherwise infer `Result<seq<int>, _>` instead of `Result<Bytes, _>`.
/// Qualifying only wrappers that contain an Aver refinement preserves the
/// existing compact output for ordinary `Result` / `Option` values.
fn type_contains_refinement(ty: &crate::types::Type, ctx: &CodegenContext) -> bool {
    use crate::types::Type;

    match ty {
        Type::Named { .. } => {
            crate::codegen::common::find_refined_type_for_named(ctx, ty).is_some()
        }
        Type::Result(ok, err) | Type::Map(ok, err) => {
            type_contains_refinement(ok, ctx) || type_contains_refinement(err, ctx)
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            type_contains_refinement(inner, ctx)
        }
        Type::Tuple(items) => items.iter().any(|item| type_contains_refinement(item, ctx)),
        Type::Fn(params, result, _) => {
            params
                .iter()
                .any(|param| type_contains_refinement(param, ctx))
                || type_contains_refinement(result, ctx)
        }
        Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Var(_)
        | Type::Invalid => false,
    }
}

fn emit_interpolated_str(parts: &[ResolvedStrPart], ctx: &CodegenContext) -> String {
    let mut pieces = Vec::new();
    for part in parts {
        match part {
            ResolvedStrPart::Literal(s) => {
                pieces.push(format!(
                    "\"{}\"",
                    crate::codegen::common::escape_string_literal_unicode(s)
                ));
            }
            ResolvedStrPart::Parsed(expr) => {
                pieces.push(format!("ToString({})", emit_expr(expr, ctx)));
            }
        }
    }
    if pieces.len() == 1 {
        pieces.into_iter().next().unwrap()
    } else {
        pieces.join(" + ")
    }
}

/// Source-shape adapter for callers that still hold a raw
/// `Spanned<crate::ast::Expr>`. Resolves the expression on demand
/// against the codegen context's symbol table; `scope` carries the
/// owning module prefix when known, with `ctx.active_module_scope()`
/// as the fallback when callers pass `None`.
pub fn emit_expr_legacy(
    expr: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    let active = ctx.active_module_scope();
    let effective = scope.or(active.as_deref());
    let resolved = ctx.resolve_expr(expr, effective);
    emit_expr(&resolved, ctx)
}

/// Source-shape adapter for [`emit_pattern`]. See [`emit_expr_legacy`]
/// for the scope-fallback rule.
#[allow(dead_code)]
pub fn emit_pattern_legacy(
    pat: &crate::ast::Pattern,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    let active = ctx.active_module_scope();
    let effective = scope.or(active.as_deref());
    let resolved = ctx.resolve_pattern(pat, effective);
    emit_pattern(&resolved)
}
