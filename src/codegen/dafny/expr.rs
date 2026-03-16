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

/// Emit a Dafny expression from an Aver Expr.
pub fn emit_expr(expr: &Expr, ctx: &CodegenContext) -> String {
    match expr {
        Expr::Literal(lit) => emit_literal(lit),
        Expr::Ident(name) => aver_name_to_dafny(name),
        Expr::Resolved(slot) => {
            panic!(
                "Dafny codegen: encountered resolver-only Expr::Resolved({slot}). \
                 Compile pipeline should emit source-level AST."
            )
        }
        Expr::Attr(obj, field) => {
            if let Expr::Ident(type_name) = obj.as_ref() {
                if type_name == "Option" && field == "None" {
                    return "Option.None".to_string();
                }
                if is_user_type(type_name, ctx) {
                    return format!("{}.{}", type_name, field);
                }
            }
            if let Some(full_dotted) = expr_to_dotted_name(expr)
                && let Some((_, bare)) = resolve_module_call(&full_dotted, ctx)
            {
                if let Some(dot_pos) = bare.find('.') {
                    let type_name = &bare[..dot_pos];
                    let variant = &bare[dot_pos + 1..];
                    if is_user_type(type_name, ctx) {
                        return format!("{}.{}", type_name, variant);
                    }
                }
                return aver_name_to_dafny(bare);
            }
            let obj_str = emit_expr(obj, ctx);
            format!("{}.{}", obj_str, aver_name_to_dafny(field))
        }
        Expr::FnCall(fn_expr, args) => emit_fn_call(fn_expr, args, ctx),
        Expr::BinOp(op, left, right) => {
            if matches!(op, BinOp::Sub) && matches!(left.as_ref(), Expr::Literal(Literal::Int(0))) {
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
        Expr::Constructor(name, arg) => emit_constructor(name, arg, ctx),
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
        Expr::Tuple(elems) => {
            let items: Vec<String> = elems.iter().map(|e| emit_expr(e, ctx)).collect();
            format!("({})", items.join(", "))
        }
        Expr::MapLiteral(entries) => {
            if entries.is_empty() {
                "map[]".to_string()
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
            format!("{}({})", type_name, field_strs.join(", "))
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
            let (name, args) = inner.as_ref();
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
            format!("{}({})", aver_name_to_dafny(name), arg_strs.join(", "))
        }
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
            format!("\"{}\"", crate::codegen::common::escape_string_literal(s))
        }
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn emit_fn_call(fn_expr: &Expr, args: &[Expr], ctx: &CodegenContext) -> String {
    let dotted = expr_to_dotted_name(fn_expr);
    let name = dotted.as_deref();

    match name {
        Some("Result.Ok") => {
            let arg = args.first().map(|a| emit_expr(a, ctx)).unwrap_or_default();
            format!("Result.Ok({})", arg)
        }
        Some("Result.Err") => {
            let arg = args.first().map(|a| emit_expr(a, ctx)).unwrap_or_default();
            format!("Result.Err({})", arg)
        }
        Some("Option.Some") => {
            let arg = args.first().map(|a| emit_expr(a, ctx)).unwrap_or_default();
            format!("Option.Some({})", arg)
        }
        Some("List.len") => {
            let arg = emit_expr(&args[0], ctx);
            format!("|{}|", arg)
        }
        Some("List.get") => {
            let list = emit_expr(&args[0], ctx);
            let idx = emit_expr(&args[1], ctx);
            format!("ListGet({}, {})", list, idx)
        }
        Some("List.head") => {
            let list = emit_expr(&args[0], ctx);
            format!("ListHead({})", list)
        }
        Some("List.tail") => {
            let list = emit_expr(&args[0], ctx);
            format!("ListTail({})", list)
        }
        Some("List.prepend") => {
            let elem = emit_expr(&args[0], ctx);
            let list = emit_expr(&args[1], ctx);
            format!("[{}] + {}", elem, list)
        }
        Some("List.push") => {
            let list = emit_expr(&args[0], ctx);
            let elem = emit_expr(&args[1], ctx);
            format!("{} + [{}]", list, elem)
        }
        Some("List.reverse") => {
            let list = emit_expr(&args[0], ctx);
            format!("ListReverse({})", list)
        }
        Some("List.contains") => {
            let list = emit_expr(&args[0], ctx);
            let elem = emit_expr(&args[1], ctx);
            format!("({} in {})", elem, list)
        }
        Some("List.concat") => {
            let a = emit_expr(&args[0], ctx);
            let b = emit_expr(&args[1], ctx);
            format!("({} + {})", a, b)
        }
        Some("List.append") => {
            // List.append(list, elem) → list + [elem]
            let list = emit_expr(&args[0], ctx);
            let elem = emit_expr(&args[1], ctx);
            format!("({} + [{}])", list, elem)
        }
        Some("List.find") => {
            let list = emit_expr(&args[0], ctx);
            let pred = emit_expr(&args[1], ctx);
            format!("ListFind({}, {})", list, pred)
        }
        Some("List.any") => {
            let list = emit_expr(&args[0], ctx);
            let pred = emit_expr(&args[1], ctx);
            format!("ListAny({}, {})", list, pred)
        }
        Some("Int.toFloat") => {
            let arg = emit_expr(&args[0], ctx);
            format!("({} as real)", arg)
        }
        Some("Int.toString") | Some("String.fromInt") => {
            let arg = emit_expr(&args[0], ctx);
            format!("IntToString({})", arg)
        }
        Some("Int.fromString") => {
            let arg = emit_expr(&args[0], ctx);
            format!("IntFromString({})", arg)
        }
        Some("Float.fromString") => {
            let arg = emit_expr(&args[0], ctx);
            format!("FloatFromString({})", arg)
        }
        Some("String.fromFloat") => {
            let arg = emit_expr(&args[0], ctx);
            format!("FloatToString({})", arg)
        }
        Some("String.len") => {
            let arg = emit_expr(&args[0], ctx);
            format!("|{}|", arg)
        }
        Some("String.concat") => {
            let a = emit_expr(&args[0], ctx);
            let b = emit_expr(&args[1], ctx);
            format!("({} + {})", a, b)
        }
        Some("String.charAt") => {
            let s = emit_expr(&args[0], ctx);
            let i = emit_expr(&args[1], ctx);
            format!("StringCharAt({}, {})", s, i)
        }
        Some("String.chars") => {
            let s = emit_expr(&args[0], ctx);
            format!("StringChars({})", s)
        }
        Some("String.slice") => {
            let s = emit_expr(&args[0], ctx);
            let start = emit_expr(&args[1], ctx);
            let end = emit_expr(&args[2], ctx);
            format!("{}[{}..{}]", s, start, end)
        }
        Some("String.join") => {
            // Aver: String.join(list, sep)
            let parts = emit_expr(&args[0], ctx);
            let sep = emit_expr(&args[1], ctx);
            format!("StringJoin({}, {})", sep, parts)
        }
        Some("Char.toCode") => {
            let c = emit_expr(&args[0], ctx);
            format!("CharToCode({})", c)
        }
        Some("Char.fromCode") => {
            let n = emit_expr(&args[0], ctx);
            format!("CharFromCode({})", n)
        }
        Some("Byte.toHex") => {
            let arg = emit_expr(&args[0], ctx);
            format!("ByteToHex({})", arg)
        }
        Some("Byte.fromHex") => {
            let arg = emit_expr(&args[0], ctx);
            format!("ByteFromHex({})", arg)
        }
        Some("Map.entries") => {
            let m = emit_expr(&args[0], ctx);
            format!("MapEntries({})", m)
        }
        Some("Map.fromList") => {
            let entries = emit_expr(&args[0], ctx);
            format!("MapFromList({})", entries)
        }
        Some("Map.empty") => "map[]".to_string(),
        Some("Map.get") => {
            let m = emit_expr(&args[0], ctx);
            let k = emit_expr(&args[1], ctx);
            format!("MapGet({}, {})", m, k)
        }
        Some("Map.set") => {
            let m = emit_expr(&args[0], ctx);
            let k = emit_expr(&args[1], ctx);
            let v = emit_expr(&args[2], ctx);
            format!("{}[{} := {}]", m, k, v)
        }
        Some("Map.has") => {
            let m = emit_expr(&args[0], ctx);
            let k = emit_expr(&args[1], ctx);
            format!("({} in {})", k, m)
        }
        Some("Result.withDefault") => {
            let r = emit_expr(&args[0], ctx);
            let def = emit_expr(&args[1], ctx);
            format!("ResultWithDefault({}, {})", r, def)
        }
        Some("Option.withDefault") => {
            let o = emit_expr(&args[0], ctx);
            let def = emit_expr(&args[1], ctx);
            format!("OptionWithDefault({}, {})", o, def)
        }
        Some("Option.toResult") => {
            let o = emit_expr(&args[0], ctx);
            let err = emit_expr(&args[1], ctx);
            format!("OptionToResult({}, {})", o, err)
        }
        _ => {
            let fn_name = emit_expr(fn_expr, ctx);
            let arg_strs: Vec<String> = args.iter().map(|a| emit_expr(a, ctx)).collect();
            format!("{}({})", fn_name, arg_strs.join(", "))
        }
    }
}

fn emit_match(subject: &Expr, arms: &[MatchArm], ctx: &CodegenContext) -> String {
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
fn emit_bool_match(subject: &Expr, arms: &[MatchArm], ctx: &CodegenContext) -> String {
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
fn emit_if_chain(subject: &Expr, arms: &[MatchArm], ctx: &CodegenContext) -> String {
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
fn emit_list_match(subject: &Expr, arms: &[MatchArm], ctx: &CodegenContext) -> String {
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

fn emit_constructor(name: &str, arg: &Option<Box<Expr>>, ctx: &CodegenContext) -> String {
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
                    crate::codegen::common::escape_string_literal(s)
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
