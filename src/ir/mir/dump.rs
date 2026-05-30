//! Textual dump for Core MIR.
//!
//! Phase 2b of #252. Renders `MirProgram` / `MirFn` / `MirExpr` /
//! `MirPattern` as a structured text view suitable for snapshot
//! tests, pass reports, and the future `aver compile
//! --emit-ir-after=mir` flag (the flag itself lands together with
//! Phase 3's first lowering wave — there's no source-of-`MirProgram`
//! until then).
//!
//! Format philosophy:
//! - **Stable order**: functions sorted by `FnId`, fields by their
//!   declared order, locals by introduction order. Snapshot tests
//!   only stay green if every walker visits the same nodes in the
//!   same sequence.
//! - **Identity visible**: `FnId`, `TypeId`, `CtorId`, `LocalId`
//!   all appear in the dump so a reviewer can tell whether the
//!   lowerer wired references through the identity layer or fell
//!   back to a string lookup.
//! - **One concept per line**: each `Let`, each `Match` arm, each
//!   constructor / record field gets its own line. Wide
//!   expressions still wrap, but the structural skeleton stays
//!   line-oriented so `diff` reads cleanly.
//! - **No span noise by default**: source line numbers would
//!   churn the snapshots whenever a fixture moves by one line.
//!   When a future diagnostic consumer needs span output, it can
//!   call into a richer formatter; the `Display` impl stays
//!   span-free.

use std::collections::BTreeMap;
use std::fmt;

use crate::ast::Spanned;

use super::expr::{
    MirBinOp, MirCall, MirCallee, MirConstruct, MirEffectAnnotation, MirExpr,
    MirIndependentProduct, MirLet, MirMatch, MirMatchArm, MirPattern, MirProject, MirRecordCreate,
    MirRecordField, MirRecordUpdate, MirStrPart, MirTailCall,
};
use super::program::{MirFn, MirParam, MirProgram};

impl fmt::Display for MirProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "MirProgram {{")?;
        // Sort by FnId so dump order is independent of HashMap
        // iteration order (which would otherwise drift across
        // platforms + Rust versions).
        let sorted: BTreeMap<_, _> = self.fns.iter().collect();
        for (fn_id, mir_fn) in sorted {
            writeln!(f, "  // FnId({})", fn_id.0)?;
            write_fn(f, mir_fn, "  ")?;
        }
        if !self.modules.is_empty() {
            writeln!(f, "  modules: [")?;
            for m in &self.modules {
                writeln!(f, "    ModuleId({})", m.0)?;
            }
            writeln!(f, "  ]")?;
        }
        writeln!(f, "}}")
    }
}

impl fmt::Display for MirFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_fn(f, self, "")
    }
}

fn write_fn(f: &mut fmt::Formatter<'_>, mir_fn: &MirFn, indent: &str) -> fmt::Result {
    write!(f, "{indent}fn {}(", mir_fn.name)?;
    for (i, p) in mir_fn.params.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write_param(f, p)?;
    }
    writeln!(f, ") -> {} {{", mir_fn.return_type)?;
    if !mir_fn.effects.is_empty() {
        write!(f, "{indent}  effects: [")?;
        for (i, e) in mir_fn.effects.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write_effect(f, e)?;
        }
        writeln!(f, "]")?;
    }
    write!(f, "{indent}  ")?;
    write_expr(f, &mir_fn.body, &format!("{indent}  "))?;
    writeln!(f)?;
    writeln!(f, "{indent}}}")
}

fn write_param(f: &mut fmt::Formatter<'_>, p: &MirParam) -> fmt::Result {
    write!(f, "{}{}: {}", p.local, p.name, p.ty)
}

fn write_effect(f: &mut fmt::Formatter<'_>, e: &MirEffectAnnotation) -> fmt::Result {
    write!(f, "{}", e.name)
}

fn write_expr(f: &mut fmt::Formatter<'_>, expr: &Spanned<MirExpr>, indent: &str) -> fmt::Result {
    match &expr.node {
        MirExpr::Literal(lit) => write!(f, "{:?}", lit.node),
        MirExpr::Local(local) => write!(f, "{}", local.node),
        MirExpr::Let(spanned) => write_let(f, &spanned.node, indent),
        MirExpr::Call(spanned) => write_call(f, &spanned.node, indent),
        MirExpr::TailCall(spanned) => write_tail_call(f, &spanned.node, indent),
        MirExpr::BinOp(spanned) => write_bin_op(f, &spanned.node, indent),
        MirExpr::Neg(inner) => {
            write!(f, "-(")?;
            write_expr(f, inner, indent)?;
            write!(f, ")")
        }
        MirExpr::Match(spanned) => write_match(f, &spanned.node, indent),
        MirExpr::Construct(spanned) => write_construct(f, &spanned.node, indent),
        MirExpr::RecordCreate(spanned) => write_record_create(f, &spanned.node, indent),
        MirExpr::RecordUpdate(spanned) => write_record_update(f, &spanned.node, indent),
        MirExpr::Project(spanned) => write_project(f, &spanned.node, indent),
        MirExpr::Try(inner) => {
            write_expr(f, inner, indent)?;
            write!(f, "?")
        }
        MirExpr::List(items) => write_list_or_tuple(f, "List", items, indent),
        MirExpr::Tuple(items) => write_list_or_tuple(f, "Tuple", items, indent),
        MirExpr::MapLiteral(pairs) => write_map_literal(f, pairs, indent),
        MirExpr::InterpolatedStr(parts) => write_interp_str(f, parts, indent),
        MirExpr::IndependentProduct(spanned) => write_independent_product(f, &spanned.node, indent),
        MirExpr::Return(inner) => {
            write!(f, "return ")?;
            write_expr(f, inner, indent)
        }
    }
}

fn write_let(f: &mut fmt::Formatter<'_>, let_node: &MirLet, indent: &str) -> fmt::Result {
    writeln!(f, "let {} =", let_node.binding)?;
    let inner = format!("{indent}  ");
    write!(f, "{inner}")?;
    write_expr(f, &let_node.value, &inner)?;
    writeln!(f)?;
    write!(f, "{indent}in ")?;
    write_expr(f, &let_node.body, indent)
}

fn write_call(f: &mut fmt::Formatter<'_>, call: &MirCall, indent: &str) -> fmt::Result {
    match &call.callee {
        MirCallee::Fn(id) => write!(f, "FnId({}).call(", id.0)?,
        MirCallee::Builtin(name) => write!(f, "Builtin({}).call(", name)?,
    }
    write_args(f, &call.args, indent)?;
    write!(f, ")")
}

fn write_tail_call(f: &mut fmt::Formatter<'_>, tc: &MirTailCall, indent: &str) -> fmt::Result {
    write!(f, "tail FnId({}).call(", tc.target.0)?;
    write_args(f, &tc.args, indent)?;
    write!(f, ")")
}

fn write_args(f: &mut fmt::Formatter<'_>, args: &[Spanned<MirExpr>], indent: &str) -> fmt::Result {
    for (i, a) in args.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write_expr(f, a, indent)?;
    }
    Ok(())
}

fn write_bin_op(f: &mut fmt::Formatter<'_>, op: &MirBinOp, indent: &str) -> fmt::Result {
    write!(f, "(")?;
    write_expr(f, &op.lhs, indent)?;
    write!(f, " {:?} ", op.op)?;
    write_expr(f, &op.rhs, indent)?;
    write!(f, ")")
}

fn write_match(f: &mut fmt::Formatter<'_>, m: &MirMatch, indent: &str) -> fmt::Result {
    write!(f, "match ")?;
    write_expr(f, &m.subject, indent)?;
    writeln!(f, " {{")?;
    let arm_indent = format!("{indent}  ");
    for arm in &m.arms {
        write_arm(f, arm, &arm_indent)?;
    }
    write!(f, "{indent}}}")
}

fn write_arm(f: &mut fmt::Formatter<'_>, arm: &MirMatchArm, indent: &str) -> fmt::Result {
    write!(f, "{indent}")?;
    write_pattern(f, &arm.pattern)?;
    write!(f, " => ")?;
    write_expr(f, &arm.body, indent)?;
    writeln!(f, ",")
}

fn write_pattern(f: &mut fmt::Formatter<'_>, p: &MirPattern) -> fmt::Result {
    match p {
        MirPattern::Wildcard => write!(f, "_"),
        MirPattern::Literal(lit) => write!(f, "{lit:?}"),
        MirPattern::Bind(local) => write!(f, "{local}"),
        MirPattern::EmptyList => write!(f, "[]"),
        MirPattern::Cons { head, tail } => write!(f, "[{head}, ..{tail}]"),
        MirPattern::Tuple(items) => {
            write!(f, "(")?;
            for (i, sub) in items.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write_pattern(f, sub)?;
            }
            write!(f, ")")
        }
        MirPattern::Ctor { ctor, bindings } => {
            write_ctor(f, *ctor)?;
            write!(f, "(")?;
            for (i, b) in bindings.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{b}")?;
            }
            write!(f, ")")
        }
    }
}

fn write_construct(f: &mut fmt::Formatter<'_>, c: &MirConstruct, indent: &str) -> fmt::Result {
    write_ctor(f, c.ctor)?;
    write!(f, "(")?;
    write_args(f, &c.args, indent)?;
    write!(f, ")")
}

fn write_ctor(f: &mut fmt::Formatter<'_>, ctor: super::MirCtor) -> fmt::Result {
    match ctor {
        super::MirCtor::User(id) => write!(f, "CtorId({})", id.0),
        super::MirCtor::Builtin(b) => match b {
            super::BuiltinCtor::ResultOk => write!(f, "Result.Ok"),
            super::BuiltinCtor::ResultErr => write!(f, "Result.Err"),
            super::BuiltinCtor::OptionSome => write!(f, "Option.Some"),
            super::BuiltinCtor::OptionNone => write!(f, "Option.None"),
        },
    }
}

fn write_record_create(
    f: &mut fmt::Formatter<'_>,
    r: &MirRecordCreate,
    indent: &str,
) -> fmt::Result {
    write!(f, "TypeId({}) {{ ", r.type_id.0)?;
    write_fields(f, &r.fields, indent)?;
    write!(f, " }}")
}

fn write_record_update(
    f: &mut fmt::Formatter<'_>,
    r: &MirRecordUpdate,
    indent: &str,
) -> fmt::Result {
    write!(f, "TypeId({}).update(", r.type_id.0)?;
    write_expr(f, &r.base, indent)?;
    write!(f, ", ")?;
    write_fields(f, &r.updates, indent)?;
    write!(f, ")")
}

fn write_fields(
    f: &mut fmt::Formatter<'_>,
    fields: &[MirRecordField],
    indent: &str,
) -> fmt::Result {
    for (i, field) in fields.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{} = ", field.name)?;
        write_expr(f, &field.value, indent)?;
    }
    Ok(())
}

fn write_project(f: &mut fmt::Formatter<'_>, p: &MirProject, indent: &str) -> fmt::Result {
    write_expr(f, &p.base, indent)?;
    write!(f, ".{}", p.field)
}

fn write_list_or_tuple(
    f: &mut fmt::Formatter<'_>,
    tag: &str,
    items: &[Spanned<MirExpr>],
    indent: &str,
) -> fmt::Result {
    write!(f, "{tag}[")?;
    write_args(f, items, indent)?;
    write!(f, "]")
}

fn write_map_literal(
    f: &mut fmt::Formatter<'_>,
    pairs: &[(Spanned<MirExpr>, Spanned<MirExpr>)],
    indent: &str,
) -> fmt::Result {
    write!(f, "Map{{")?;
    for (i, (k, v)) in pairs.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write_expr(f, k, indent)?;
        write!(f, " => ")?;
        write_expr(f, v, indent)?;
    }
    write!(f, "}}")
}

fn write_interp_str(f: &mut fmt::Formatter<'_>, parts: &[MirStrPart], indent: &str) -> fmt::Result {
    write!(f, "\"")?;
    for part in parts {
        match part {
            MirStrPart::Literal(s) => write!(f, "{}", s.replace('"', "\\\""))?,
            MirStrPart::Expr(e) => {
                write!(f, "{{")?;
                write_expr(f, e, indent)?;
                write!(f, "}}")?;
            }
        }
    }
    write!(f, "\"")
}

fn write_independent_product(
    f: &mut fmt::Formatter<'_>,
    ip: &MirIndependentProduct,
    indent: &str,
) -> fmt::Result {
    write!(f, "(")?;
    write_args(f, &ip.items, indent)?;
    if ip.unwrap_results {
        write!(f, ")?!")
    } else {
        write!(f, ")!")
    }
}

// `LocalId`'s `Display` impl lives in `program.rs` — search for it
// there. The format (`%N`) is shared by both this dump and any
// future pass-report code that consumes the same IDs.
