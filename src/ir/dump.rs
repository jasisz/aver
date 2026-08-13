//! Textual IR dump — turns a `Vec<TopLevel>` into a stable, human-readable
//! representation. Used by `aver compile --emit-ir` and `--emit-ir-after=PASS`
//! to give compiler engineers a verifiable diff between passes.
//!
//! The format is Aver-like surface syntax with explicit "this came from
//! a pass" markers in the expression body:
//!
//!   `<tail-call:fn>(args)`  — Expr::TailCall (TCO output)
//!   `<resolved>`            — Expr::Resolved (resolver output)
//!   `__buf_*` / `__to_str`  — buffer-build / interp_lower intrinsics
//!
//! When an `AnalysisResult` is available (the pipeline's `Analyze` stage
//! ran), each FnDef line also carries the per-fn facts:
//!
//!   `[no_alloc]`      — proven not to allocate under the configured policy
//!   `[locals=N]`      — resolver's `local_count`
//!   `[body=kind]`     — body shape from the thin-body classifier
//!
//! Expression rendering reuses `checker::verify::expr_to_str`. Top-level
//! scaffolding (fn signatures, stmt list, type defs, module headers) lives
//! here. The analysis itself lives in `ir::analyze` — dump is read-only
//! over its result, which keeps compute-once / read-many honest.

use std::fmt::Write;

use crate::ast::{FnBody, FnDef, Stmt, TopLevel, TypeDef};
use crate::checker::expr_to_str;
use crate::ir::{AnalysisResult, BodyShape, FnAnalysis, ThinKind};

/// Render every top-level item in `items`, separated by blank lines.
/// Pass `analysis: None` for dumps before the `Analyze` stage has run
/// (or when running passes individually); FnDef lines will then omit
/// the `[...]` annotation block but the IR itself still renders.
pub fn dump_items(items: &[TopLevel], analysis: Option<&AnalysisResult>) -> String {
    let mut out = String::new();
    let mut first = true;
    for item in items {
        if !first {
            out.push('\n');
        }
        first = false;
        dump_top_level(item, &mut out, analysis);
    }
    out
}

fn dump_top_level(item: &TopLevel, out: &mut String, analysis: Option<&AnalysisResult>) {
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
        TopLevel::FnDef(fd) => {
            let fn_analysis = analysis.and_then(|a| a.fn_analyses.get(&fd.name));
            dump_fndef(fd, out, fn_analysis);
        }
        TopLevel::Stmt(s) => dump_stmt(s, 0, out),
        TopLevel::Verify(vb) => {
            writeln!(out, "verify {} <{} case(s)>", vb.fn_name, vb.cases.len()).ok();
        }
        TopLevel::Decision(_) => {
            writeln!(out, "decision <block>").ok();
        }
        // A silent arm here would make `--dump-ir` lie about what the
        // file contains — the operation would simply not appear.
        TopLevel::Capability(item) => {
            writeln!(out, "{} {}", item.keyword(), item.name()).ok();
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

fn dump_fndef(fd: &FnDef, out: &mut String, fn_analysis: Option<&FnAnalysis>) {
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

    let tag_str = match fn_analysis {
        None => String::new(),
        Some(a) => {
            let mut tags: Vec<String> = Vec::new();
            if matches!(a.allocates, Some(false)) {
                tags.push("no_alloc".to_string());
            }
            if let Some(n) = a.local_count {
                tags.push(format!("locals={}", n));
            }
            if a.mutual_tco_member {
                tags.push("mutual-tco".to_string());
            } else if a.recursive {
                if a.recursive_call_count >= 2 {
                    tags.push(format!("recursive×{}", a.recursive_call_count));
                } else {
                    tags.push("recursive".to_string());
                }
            }
            tags.push(body_shape_tag(a.body_shape, a.thin_kind));
            format!("  [{}]", tags.join(", "))
        }
    };

    writeln!(
        out,
        "fn {}({}) -> {}{}{}",
        fd.name,
        params.join(", "),
        fd.return_type,
        effects,
        tag_str
    )
    .ok();

    let FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        dump_stmt(stmt, 1, out);
    }
}

fn body_shape_tag(shape: BodyShape, thin_kind: Option<ThinKind>) -> String {
    let kind_suffix = thin_kind.map(thin_kind_label).unwrap_or("");
    let kind_part = if kind_suffix.is_empty() {
        String::new()
    } else {
        format!(" ({})", kind_suffix)
    };
    match shape {
        BodyShape::LeafExpr => format!("body=leaf-expr{}", kind_part),
        BodyShape::SingleExpr => format!("body=single-expr{}", kind_part),
        BodyShape::Block(n) => format!("body=block:{}{}", n, kind_part),
        BodyShape::Unclassified(1) => "body=single-expr".to_string(),
        BodyShape::Unclassified(n) => format!("body=block:{}", n),
    }
}

fn thin_kind_label(kind: ThinKind) -> &'static str {
    match kind {
        ThinKind::Leaf => "leaf",
        ThinKind::Direct => "direct",
        ThinKind::Forward => "forward",
        ThinKind::Dispatch => "dispatch",
        ThinKind::Tail => "tail",
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
