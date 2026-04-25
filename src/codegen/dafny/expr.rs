/// Aver expressions → Dafny expression strings.
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::common::{expr_to_dotted_name, is_user_type, resolve_module_call};

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

/// Convert an Aver identifier to a valid Dafny name.
pub fn aver_name_to_dafny(name: &str) -> String {
    crate::codegen::common::escape_reserved_word(name, DAFNY_RESERVED, "_")
}

/// Emit a Dafny expression from an Aver Spanned<Expr>.
pub fn emit_expr(expr: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    match &expr.node {
        Expr::Literal(lit) => emit_literal(lit),
        Expr::Ident(name) | Expr::Resolved { name, .. } => aver_name_to_dafny(name),
        Expr::Attr(obj, field) => {
            if let Expr::Ident(type_name) = &obj.node {
                if type_name == "Option" && field == "None" {
                    return "Option.None".to_string();
                }
                // Oracle v1: `BranchPath.Root` — nullary value,
                // matches the `const BranchPath_Root` in the prelude.
                if type_name == "BranchPath" && field == "Root" {
                    return "BranchPath_Root".to_string();
                }
                if is_user_type(type_name, ctx) {
                    return format!("{}.{}", type_name, field);
                }
            }
            if let Some(full_dotted) = expr_to_dotted_name_spanned(expr)
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
            let obj_str = emit_expr(obj, ctx);
            format!("{}.{}", obj_str, aver_name_to_dafny(field))
        }
        Expr::FnCall(fn_expr, args) => emit_fn_call(fn_expr, args, ctx),
        Expr::BinOp(op, left, right) => {
            // Narrow special case: `0 - x` where the right-hand side is
            // a `Float`-typed expression becomes `(-x)`. Dafny rejects
            // `0 - 2.5` because the literal `0` is `int` and mixing is
            // a type mismatch against `real`; the split_here fix from
            // the Oracle PR removed the blanket rewrite, so reintroduce
            // it only for Float operands where Dafny actually needs it.
            if matches!(op, BinOp::Sub)
                && matches!(left.node, Expr::Literal(Literal::Int(0)))
                && right_is_float_shape(right)
            {
                let r = emit_expr(right, ctx);
                return format!("(-{})", r);
            }
            let l = emit_expr(left, ctx);
            let r = emit_expr(right, ctx);
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
        Expr::Match { subject, arms, .. } => emit_match(subject, arms, ctx),
        Expr::Constructor(name, arg) => emit_constructor(name, arg.as_deref(), ctx),
        Expr::ErrorProp(_) => {
            // ? operator requires early-return semantics (Err propagation).
            // Dafny pure functions cannot express this; functions using ? are
            // skipped at the top-level emission stage.  If we get here, emit
            // a marker that makes the generated Dafny obviously wrong rather
            // than silently modelling a different program.
            "/* ERROR: ? operator not supported in Dafny pure functions */".to_string()
        }
        Expr::InterpolatedStr(parts) => emit_interpolated_str(parts, ctx),
        Expr::List(elems) => {
            let items: Vec<String> = elems.iter().map(|e| emit_expr(e, ctx)).collect();
            format!("[{}]", items.join(", "))
        }
        Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
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
        Expr::MapLiteral(entries) => {
            if entries.is_empty() {
                "map[]".to_string()
            } else if entries
                .iter()
                .all(|(_, v)| crate::codegen::common::is_unit_expr_spanned(v))
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
        Expr::RecordCreate { type_name, fields } => {
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_dafny(name), emit_expr(expr, ctx))
                })
                .collect();
            // Dotted names (`Terminal.Size`, `Tcp.Connection`) are
            // rendered as underscored datatype names in Dafny — same
            // mapping `toplevel::render_type` applies for `Type::Named`.
            let dafny_type_name = type_name.replace('.', "_");
            format!("{}({})", dafny_type_name, field_strs.join(", "))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            let base_str = emit_expr(base, ctx);
            let update_strs: Vec<String> = updates
                .iter()
                .map(|(name, expr)| {
                    format!("{} := {}", aver_name_to_dafny(name), emit_expr(expr, ctx))
                })
                .collect();
            format!("{}.({})", base_str, update_strs.join(", "))
        }
        Expr::TailCall(inner) => {
            let TailCallData {
                target: name, args, ..
            } = inner.as_ref();
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
            format!("{}({})", aver_name_to_dafny(name), arg_strs.join(", "))
        }
    }
}

/// Helper to extract dotted name from a Spanned<Expr>.
fn expr_to_dotted_name_spanned(expr: &Spanned<Expr>) -> Option<String> {
    expr_to_dotted_name(&expr.node)
}

/// True when an expression is syntactically Float-shaped: a float
/// literal, an explicit `as real` cast, or a float-returning builtin.
/// Used by the `0 - x` → `(-x)` rewrite for Dafny `real` arithmetic.
fn right_is_float_shape(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Literal(Literal::Float(_)) => true,
        Expr::FnCall(callee, _) => {
            matches!(
                &callee.node,
                Expr::Attr(obj, field)
                    if field == "toFloat"
                        || (matches!(&obj.node, Expr::Ident(n) if n == "Float" || n == "Int")
                            && (field == "sqrt" || field == "pow" || field == "abs"))
            )
        }
        // A parenthesised `e as real` lands here as a nested cast node
        // after the parser / resolver pass. The shape we actually see
        // at this codegen point is a FnCall into a cast builtin or a
        // float literal embedded in a constructor arg; both covered
        // above. Be permissive — if in doubt treat as float for this
        // rewrite so we don't regress the `0 - float_expr` shape.
        _ => false,
    }
}

fn emit_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(n) => n.to_string(),
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

fn emit_fn_call(fn_expr: &Spanned<Expr>, args: &[Spanned<Expr>], ctx: &CodegenContext) -> String {
    use crate::codegen::builtins::recognize_builtin;
    use crate::codegen::common::is_unit_expr_spanned;

    let dotted = expr_to_dotted_name_spanned(fn_expr);

    // Map<T, Unit> set operations: intercept before generic builtin path
    if let Some(name) = dotted.as_deref()
        && name == "Map.set"
        && args.len() == 3
        && is_unit_expr_spanned(&args[2])
    {
        let m = emit_expr(&args[0], ctx);
        let k = emit_expr(&args[1], ctx);
        return format!("({} + {{{}}})", m, k);
    }

    if let Some(builtin) = dotted.as_deref().and_then(recognize_builtin) {
        let a: Vec<String> = args.iter().map(|e| emit_expr(e, ctx)).collect();
        return emit_dafny_builtin(builtin, &a);
    }

    // Oracle v1: BranchPath.* constructor calls map onto the underscore-
    // named prelude functions (Dafny's dotted notation collides with
    // record-member access on the BranchPath datatype).
    if let Some(name) = dotted.as_deref() {
        let a: Vec<String> = args.iter().map(|e| emit_expr(e, ctx)).collect();
        match name {
            "BranchPath.child" if a.len() == 2 => {
                return format!("BranchPath_child({}, {})", a[0], a[1]);
            }
            "BranchPath.parse" if a.len() == 1 => {
                return format!("BranchPath_parse({})", a[0]);
            }
            _ => {}
        }
    }

    // Not a recognized builtin — generic function call
    let fn_name = emit_expr(fn_expr, ctx);
    let arg_strs: Vec<String> = args.iter().map(|e| emit_expr(e, ctx)).collect();
    format!("{}({})", fn_name, arg_strs.join(", "))
}

fn emit_dafny_builtin(b: crate::codegen::builtins::Builtin, a: &[String]) -> String {
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
        IntToFloat => format!("({} as real)", a[0]),
        IntToString | StringFromInt => format!("IntToString({})", a[0]),
        IntFromString | IntParse => format!("IntFromString({})", a[0]),
        IntMin => format!("(if {} <= {} then {} else {})", a[0], a[1], a[0], a[1]),
        IntMax => format!("(if {} >= {} then {} else {})", a[0], a[1], a[0], a[1]),
        IntRem | IntMod => format!("Result<int, string>.Ok(({} % {}))", a[0], a[1]),

        // Float
        FloatAbs => format!("(if {} >= 0.0 then {} else -{})", a[0], a[0], a[0]),
        FloatSqrt => format!("FloatSqrt({})", a[0]),
        FloatPow => format!("FloatPow({}, {})", a[0], a[1]),
        FloatRound | FloatFloor | FloatCeil | FloatToInt => format!("FloatToInt({})", a[0]),
        FloatToString | StringFromFloat => format!("FloatToString({})", a[0]),
        FloatFromString | FloatParse => format!("FloatFromString({})", a[0]),

        // String
        StringLen => format!("|{}|", a[0]),
        StringConcat => format!("({} + {})", a[0], a[1]),
        StringCharAt => format!("StringCharAt({}, {})", a[0], a[1]),
        StringChars => format!("StringChars({})", a[0]),
        StringSlice => format!("{}[{}..{}]", a[0], a[1], a[2]),
        StringContains => format!("StringContains({}, {})", a[0], a[1]),
        StringStartsWith => format!("StringStartsWith({}, {})", a[0], a[1]),
        StringEndsWith => format!("StringEndsWith({}, {})", a[0], a[1]),
        StringTrim => format!("StringTrim({})", a[0]),
        StringSplit => format!("StringSplit({}, {})", a[0], a[1]),
        StringJoin => format!("StringJoin({}, {})", a[1], a[0]), // Aver: join(list, sep)
        StringReplace => format!("StringReplace({}, {}, {})", a[0], a[1], a[2]),
        StringRepeat => format!("StringRepeat({}, {})", a[0], a[1]),
        StringIndexOf => format!("StringIndexOf({}, {})", a[0], a[1]),
        StringToUpper => format!("StringToUpper({})", a[0]),
        StringToLower => format!("StringToLower({})", a[0]),
        StringFromBool => format!("StringFromBool({})", a[0]),
        StringByteLength => format!("StringByteLength({})", a[0]),

        // Bool
        BoolOr => format!("({} || {})", a[0], a[1]),
        BoolAnd => format!("({} && {})", a[0], a[1]),
        BoolNot => format!("(!{})", a[0]),

        // Char/Byte
        CharToCode => format!("CharToCode({})", a[0]),
        CharFromCode => format!("CharFromCode({})", a[0]),
        ByteToHex => format!("ByteToHex({})", a[0]),
        ByteFromHex => format!("ByteFromHex({})", a[0]),

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
        VectorToList => a[0].clone(),

        // Map
        MapEmpty => "map[]".to_string(),
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

fn emit_match(subject: &Spanned<Expr>, arms: &[MatchArm], ctx: &CodegenContext) -> String {
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
fn should_emit_as_if_chain(arms: &[MatchArm]) -> bool {
    arms.iter().all(|arm| {
        matches!(
            arm.pattern,
            Pattern::Literal(_) | Pattern::Wildcard | Pattern::Ident(_)
        )
    })
}

/// Check if arms form a bool match: `true -> ..., false -> ...` (in either order).
fn is_bool_match(arms: &[MatchArm]) -> bool {
    if arms.len() != 2 {
        return false;
    }
    let has_true = arms
        .iter()
        .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))));
    let has_false = arms
        .iter()
        .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))));
    has_true && has_false
}

/// Emit a bool match as `if subject then true_body else false_body`.
fn emit_bool_match(subject: &Spanned<Expr>, arms: &[MatchArm], ctx: &CodegenContext) -> String {
    let subj = emit_expr(subject, ctx);
    let true_arm = arms
        .iter()
        .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))))
        .unwrap();
    let false_arm = arms
        .iter()
        .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))))
        .unwrap();
    let true_body = emit_expr(&true_arm.body, ctx);
    let false_body = emit_expr(&false_arm.body, ctx);
    format!("(if {} then {} else {})", subj, true_body, false_body)
}

/// Emit a match as a Dafny if-then-else chain.
fn emit_if_chain(subject: &Spanned<Expr>, arms: &[MatchArm], ctx: &CodegenContext) -> String {
    let subj = emit_expr(subject, ctx);
    emit_if_chain_inner(&subj, arms, 0, ctx)
}

fn emit_if_chain_inner(subj: &str, arms: &[MatchArm], idx: usize, ctx: &CodegenContext) -> String {
    if idx >= arms.len() {
        return "/* unreachable */".to_string();
    }

    let arm = &arms[idx];
    let body = emit_expr(&arm.body, ctx);

    match &arm.pattern {
        Pattern::Wildcard | Pattern::Ident(_) => {
            if let Pattern::Ident(name) = &arm.pattern {
                format!("(var {} := {}; {})", aver_name_to_dafny(name), subj, body)
            } else {
                body
            }
        }
        Pattern::Literal(lit) => {
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
fn has_list_patterns(arms: &[MatchArm]) -> bool {
    arms.iter()
        .any(|arm| matches!(arm.pattern, Pattern::EmptyList | Pattern::Cons(_, _)))
}

/// Emit a match on a list (seq) as if-then-else with seq operations.
fn emit_list_match(subject: &Spanned<Expr>, arms: &[MatchArm], ctx: &CodegenContext) -> String {
    let subj = emit_expr(subject, ctx);

    // Find empty-list arm and cons arm
    let empty_arm = arms
        .iter()
        .find(|a| matches!(a.pattern, Pattern::EmptyList));
    let cons_arm = arms
        .iter()
        .find(|a| matches!(a.pattern, Pattern::Cons(_, _)));
    let wildcard_arm = arms
        .iter()
        .find(|a| matches!(a.pattern, Pattern::Wildcard | Pattern::Ident(_)));

    let empty_body = if let Some(arm) = empty_arm {
        emit_expr(&arm.body, ctx)
    } else if let Some(arm) = wildcard_arm {
        emit_expr(&arm.body, ctx)
    } else {
        "/* missing empty case */".to_string()
    };

    let cons_body = if let Some(arm) = cons_arm {
        if let Pattern::Cons(head, tail) = &arm.pattern {
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

fn emit_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Literal(lit) => emit_literal(lit),
        Pattern::Ident(name) => aver_name_to_dafny(name),
        Pattern::EmptyList => "Nil".to_string(),
        Pattern::Cons(head, tail) => {
            format!(
                "Cons({}, {})",
                aver_name_to_dafny(head),
                aver_name_to_dafny(tail)
            )
        }
        Pattern::Tuple(pats) => {
            let subs: Vec<String> = pats.iter().map(emit_pattern).collect();
            format!("({})", subs.join(", "))
        }
        Pattern::Constructor(name, bindings) => {
            let variant = if let Some(dot_pos) = name.rfind('.') {
                &name[dot_pos + 1..]
            } else {
                name.as_str()
            };
            if bindings.is_empty() {
                variant.to_string()
            } else {
                let subs: Vec<String> = bindings.iter().map(|b| aver_name_to_dafny(b)).collect();
                format!("{}({})", variant, subs.join(", "))
            }
        }
    }
}

fn emit_constructor(name: &str, arg: Option<&Spanned<Expr>>, ctx: &CodegenContext) -> String {
    // In Dafny expression context, qualify constructors to avoid ambiguity.
    // "Shape.Circle" → "Shape.Circle", "Foo" → "Foo"
    let qualified = if let Some(dot_pos) = name.rfind('.') {
        let type_name = &name[..dot_pos];
        let variant = &name[dot_pos + 1..];
        // Check if it's a user-defined type — qualify to avoid clashes
        if is_user_type(type_name, ctx) {
            format!("{}.{}", type_name, variant)
        } else {
            variant.to_string()
        }
    } else {
        name.to_string()
    };
    if let Some(a) = arg {
        let arg_str = emit_expr(a, ctx);
        format!("{}({})", qualified, arg_str)
    } else {
        qualified
    }
}

fn emit_interpolated_str(parts: &[StrPart], ctx: &CodegenContext) -> String {
    let mut pieces = Vec::new();
    for part in parts {
        match part {
            StrPart::Literal(s) => {
                pieces.push(format!(
                    "\"{}\"",
                    crate::codegen::common::escape_string_literal_unicode(s)
                ));
            }
            StrPart::Parsed(expr) => {
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
