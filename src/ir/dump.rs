//! Textual IR dump — turns a `Vec<TopLevel>` into a stable, human-readable
//! representation. Used by `aver compile --emit-ir` and `--emit-ir-after=PASS`
//! to give compiler engineers a verifiable diff between passes.
//!
//! The format is Aver-like surface syntax with three explicit "this came
//! from a pass" markers:
//!
//!   `<tail-call:fn>(args)`  — Expr::TailCall (TCO output)
//!   `<resolved>`            — Expr::Resolved (resolver output)
//!   `__buf_*` / `__to_str`  — buffer-build / interp_lower intrinsics
//!
//! Expression rendering reuses `checker::verify::expr_to_str` which already
//! covers every variant. This module only adds the top-level scaffolding
//! (fn signatures, stmt list, type defs, module headers).

use std::fmt::Write;

use crate::ast::{FnBody, FnDef, Stmt, TopLevel, TypeDef};
use crate::checker::expr_to_str;

/// Render every top-level item in `items`, separated by blank lines.
pub fn dump_items(items: &[TopLevel]) -> String {
    let mut out = String::new();
    let mut first = true;
    for item in items {
        if !first {
            out.push('\n');
        }
        first = false;
        dump_top_level(item, &mut out);
    }
    out
}

fn dump_top_level(item: &TopLevel, out: &mut String) {
    match item {
        TopLevel::Module(m) => {
            writeln!(out, "module {}", m.name).ok();
            if !m.depends.is_empty() {
                writeln!(out, "  depends [{}]", m.depends.join(", ")).ok();
            }
            if !m.exposes.is_empty() {
                writeln!(out, "  exposes [{}]", m.exposes.join(", ")).ok();
            }
            if let Some(effects) = &m.effects {
                writeln!(out, "  effects [{}]", effects.join(", ")).ok();
            }
        }
        TopLevel::TypeDef(td) => dump_typedef(td, out),
        TopLevel::FnDef(fd) => dump_fndef(fd, out),
        TopLevel::Stmt(s) => dump_stmt(s, 0, out),
        TopLevel::Verify(vb) => {
            writeln!(out, "verify {} <{} case(s)>", vb.fn_name, vb.cases.len()).ok();
        }
        TopLevel::Decision(_) => {
            writeln!(out, "decision <block>").ok();
        }
    }
}

fn dump_typedef(td: &TypeDef, out: &mut String) {
    match td {
        TypeDef::Product { name, fields, .. } => {
            let parts: Vec<String> = fields
                .iter()
                .map(|(n, t)| format!("{}: {}", n, t))
                .collect();
            writeln!(out, "type {} = {{ {} }}", name, parts.join(", ")).ok();
        }
        TypeDef::Sum { name, variants, .. } => {
            let parts: Vec<String> = variants
                .iter()
                .map(|v| {
                    if v.fields.is_empty() {
                        v.name.clone()
                    } else {
                        format!("{}({})", v.name, v.fields.join(", "))
                    }
                })
                .collect();
            writeln!(out, "type {} = {}", name, parts.join(" | ")).ok();
        }
    }
}

fn dump_fndef(fd: &FnDef, out: &mut String) {
    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(n, t)| format!("{}: {}", n, t))
        .collect();
    let effects = if fd.effects.is_empty() {
        String::new()
    } else {
        let names: Vec<String> = fd.effects.iter().map(|e| e.node.clone()).collect();
        format!(" ! [{}]", names.join(", "))
    };
    let resolved_marker = if fd.resolution.is_some() {
        " /* resolved */"
    } else {
        ""
    };
    writeln!(
        out,
        "fn {}({}) -> {}{}{}",
        fd.name,
        params.join(", "),
        fd.return_type,
        effects,
        resolved_marker
    )
    .ok();
    let FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        dump_stmt(stmt, 1, out);
    }
}

fn dump_stmt(stmt: &Stmt, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    match stmt {
        Stmt::Binding(name, ty, expr) => {
            let ty_part = ty
                .as_deref()
                .map(|t| format!(": {}", t))
                .unwrap_or_default();
            writeln!(out, "{}{}{} = {}", pad, name, ty_part, expr_to_str(expr)).ok();
        }
        Stmt::Expr(expr) => {
            writeln!(out, "{}{}", pad, expr_to_str(expr)).ok();
        }
    }
}
