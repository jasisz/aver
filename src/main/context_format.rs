use aver::ast::{DecisionBlock, DecisionImpact, FnDef, Spanned, TypeDef};
use serde::Serialize;

use crate::context_data::FileContext;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct ContextSelection {
    pub depth_mode: String,
    pub budget_bytes: Option<usize>,
    pub included_depth: Option<usize>,
    pub next_depth: Option<usize>,
    pub next_used_bytes: Option<usize>,
    pub truncated: bool,
    pub used_bytes: usize,
    pub focus_symbol: Option<String>,
    /// How many elements (fns, types, decisions) were selected vs available.
    pub elements_selected: Option<usize>,
    pub elements_total: Option<usize>,
}

fn impact_texts(impacts: &[Spanned<DecisionImpact>]) -> Vec<String> {
    impacts.iter().map(|s| s.node.as_context_string()).collect()
}

fn decision_ref_text(reference: &Spanned<DecisionImpact>) -> String {
    reference.node.as_context_string()
}

fn decision_ref_json_text(reference: &Spanned<DecisionImpact>) -> &str {
    reference.node.text()
}

const CONTEXT_SCHEMA_VERSION: u32 = 7;

#[derive(Serialize)]
struct JsonSelection<'a> {
    depth_mode: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    budget_bytes: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    included_depth: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    next_element_cost: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    focus_symbol: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    elements_selected: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    elements_total: Option<usize>,
    truncated: bool,
    used_bytes: usize,
}

#[derive(Serialize)]
struct JsonType<'a> {
    name: &'a str,
    variants: Vec<String>,
}

#[derive(Serialize)]
struct JsonRecord<'a> {
    name: &'a str,
    fields: Vec<String>,
}

#[derive(Serialize)]
struct JsonAnalysis {
    #[serde(skip_serializing_if = "Option::is_none")]
    tco: Option<bool>,
}

#[derive(Serialize)]
struct JsonFunction<'a> {
    sig: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    effects: Vec<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    analysis: Option<JsonAnalysis>,
    #[serde(skip_serializing_if = "Option::is_none")]
    desc: Option<&'a str>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    specs: Vec<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    verify_count: Option<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    verify: Vec<&'a str>,
}

#[derive(Serialize)]
struct JsonModule<'a> {
    source_file: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    module: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    intent: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    depends: Option<Vec<&'a str>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    exposes_opaque: Option<Vec<&'a str>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    effects: Option<Vec<&'a str>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    api_effects: Option<Vec<&'a str>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    module_effects: Option<Vec<&'a str>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    main_effects: Option<Vec<&'a str>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    types: Option<Vec<JsonType<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    records: Option<Vec<JsonRecord<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    functions: Option<Vec<JsonFunction<'a>>>,
}

#[derive(Serialize)]
struct JsonDecision<'a> {
    name: &'a str,
    date: &'a str,
    chosen: &'a str,
    rejected: Vec<&'a str>,
    reason: &'a str,
    impacts: Vec<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    author: Option<&'a str>,
}

#[derive(Serialize)]
struct JsonContextDoc<'a> {
    schema_version: u32,
    entry: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    selection: Option<JsonSelection<'a>>,
    modules: Vec<JsonModule<'a>>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    decisions: Vec<JsonDecision<'a>>,
}

#[derive(Serialize)]
struct JsonDecisionsDoc<'a> {
    entry: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    selection: Option<JsonSelection<'a>>,
    decisions: Vec<JsonDecision<'a>>,
}

fn fn_sig(fd: &FnDef) -> String {
    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(pname, ptype)| format!("{}: {}", pname, ptype))
        .collect();
    format!("{}({}) -> {}", fd.name, params.join(", "), fd.return_type)
}

/// Aver-source-style signature with the `! [effects]` suffix.
/// Markdown context uses this to drop the redundant separate `effects:` line;
/// JSON keeps the bare `sig` and an explicit `effects` array next to it.
fn fn_sig_with_effects(fd: &FnDef) -> String {
    if fd.effects.is_empty() {
        fn_sig(fd)
    } else {
        let effs: Vec<&str> = fd.effects.iter().map(|e| e.node.as_str()).collect();
        format!("{} ! [{}]", fn_sig(fd), effs.join(", "))
    }
}

fn selection_to_json(selection: &ContextSelection) -> JsonSelection<'_> {
    JsonSelection {
        depth_mode: &selection.depth_mode,
        budget_bytes: selection.budget_bytes,
        included_depth: selection.included_depth,
        next_element_cost: selection
            .next_used_bytes
            .map(|next_used| next_used.saturating_sub(selection.used_bytes)),
        focus_symbol: selection.focus_symbol.as_deref(),
        elements_selected: selection.elements_selected,
        elements_total: selection.elements_total,
        truncated: selection.truncated,
        used_bytes: selection.used_bytes,
    }
}

fn vec_refs(values: &[String]) -> Vec<&str> {
    values.iter().map(String::as_str).collect()
}

fn decision_to_json(decision: &DecisionBlock) -> JsonDecision<'_> {
    JsonDecision {
        name: &decision.name,
        date: &decision.date,
        chosen: decision_ref_json_text(&decision.chosen),
        rejected: decision
            .rejected
            .iter()
            .map(decision_ref_json_text)
            .collect(),
        reason: &decision.reason,
        impacts: decision.impacts.iter().map(|s| s.node.text()).collect(),
        author: decision.author.as_deref(),
    }
}

fn module_to_json(ctx: &FileContext) -> JsonModule<'_> {
    let types = {
        let items = ctx
            .type_defs
            .iter()
            .filter_map(|td| match td {
                TypeDef::Sum { name, variants, .. } => Some(JsonType {
                    name,
                    variants: variants
                        .iter()
                        .map(|variant| {
                            if variant.fields.is_empty() {
                                variant.name.clone()
                            } else {
                                format!("{}({})", variant.name, variant.fields.join(", "))
                            }
                        })
                        .collect(),
                }),
                TypeDef::Product { .. } => None,
            })
            .collect::<Vec<_>>();
        (!items.is_empty()).then_some(items)
    };

    let records = {
        let items = ctx
            .type_defs
            .iter()
            .filter_map(|td| match td {
                TypeDef::Product { name, fields, .. } => Some(JsonRecord {
                    name,
                    fields: fields
                        .iter()
                        .map(|(fname, ftype)| format!("{}: {}", fname, ftype))
                        .collect(),
                }),
                TypeDef::Sum { .. } => None,
            })
            .collect::<Vec<_>>();
        (!items.is_empty()).then_some(items)
    };

    let functions = {
        let items = ctx
            .fn_defs
            .iter()
            .filter(|fd| fd.name != "main")
            .map(|fd| {
                let has_tco = ctx.fn_auto_tco.contains(&fd.name);
                let analysis = has_tco.then_some(JsonAnalysis {
                    tco: has_tco.then_some(true),
                });
                let specs = ctx
                    .fn_specs
                    .get(&fd.name)
                    .map_or_else(Vec::new, |specs| vec_refs(specs));
                let verify = ctx
                    .verify_samples
                    .get(&fd.name)
                    .map_or_else(Vec::new, |samples| vec_refs(samples));
                JsonFunction {
                    sig: fn_sig(fd),
                    effects: fd.effects.iter().map(|e| e.node.as_str()).collect(),
                    analysis,
                    desc: fd.desc.as_deref(),
                    specs,
                    verify_count: ctx.verify_counts.get(&fd.name).copied().filter(|n| *n > 0),
                    verify,
                }
            })
            .collect::<Vec<_>>();
        (!items.is_empty()).then_some(items)
    };

    let (effects, api_effects, module_effects) =
        if effects_equal(&ctx.api_effects, &ctx.module_effects) {
            (
                (!ctx.module_effects.is_empty()).then(|| vec_refs(&ctx.module_effects)),
                None,
                None,
            )
        } else {
            (
                None,
                Some(vec_refs(&ctx.api_effects)),
                Some(vec_refs(&ctx.module_effects)),
            )
        };

    JsonModule {
        source_file: &ctx.source_file,
        module: ctx.module_name.as_deref(),
        intent: ctx.intent.as_deref(),
        depends: (!ctx.depends.is_empty()).then(|| vec_refs(&ctx.depends)),
        exposes_opaque: (!ctx.exposes_opaque.is_empty()).then(|| vec_refs(&ctx.exposes_opaque)),
        effects,
        api_effects,
        module_effects,
        main_effects: ctx.main_effects.as_ref().map(|effects| vec_refs(effects)),
        types,
        records,
        functions,
    }
}

fn context_doc_to_json<'a>(
    contexts: &'a [FileContext],
    entry_file: &'a str,
    selection: Option<&'a ContextSelection>,
) -> JsonContextDoc<'a> {
    let modules = contexts
        .iter()
        .filter(|ctx| {
            ctx.module_name.is_some() || !ctx.fn_defs.is_empty() || !ctx.type_defs.is_empty()
        })
        .map(module_to_json)
        .collect();
    let decisions = contexts
        .iter()
        .flat_map(|ctx| ctx.decisions.iter())
        .map(decision_to_json)
        .collect();

    JsonContextDoc {
        schema_version: CONTEXT_SCHEMA_VERSION,
        entry: entry_file,
        selection: selection.map(selection_to_json),
        modules,
        decisions,
    }
}

fn decisions_doc_to_json<'a>(
    decisions: &[&'a DecisionBlock],
    entry_file: &'a str,
    selection: Option<&'a ContextSelection>,
) -> JsonDecisionsDoc<'a> {
    JsonDecisionsDoc {
        entry: entry_file,
        selection: selection.map(selection_to_json),
        decisions: decisions.iter().copied().map(decision_to_json).collect(),
    }
}

fn effects_equal(a: &[String], b: &[String]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x == y)
}

fn byte_label(bytes: usize) -> String {
    if bytes >= 1024 * 1024 && bytes.is_multiple_of(1024 * 1024) {
        format!("{}mb", bytes / (1024 * 1024))
    } else if bytes >= 1024 && bytes.is_multiple_of(1024) {
        format!("{}kb", bytes / 1024)
    } else {
        format!("{}b", bytes)
    }
}

pub(super) fn format_context_md(
    contexts: &[FileContext],
    entry_file: &str,
    selection: Option<&ContextSelection>,
) -> String {
    let mut out = String::new();
    out.push_str(&format!("# Aver Context — {}\n\n", entry_file));
    out.push_str("_Generated by `aver context`_\n\n");
    if let Some(selection) = selection {
        let mut parts = vec![format!("mode: `{}`", selection.depth_mode)];
        if let Some(budget) = selection.budget_bytes {
            parts.push(format!("budget: `{}`", byte_label(budget)));
        }
        if let Some(depth) = selection.included_depth {
            parts.push(format!("included_depth: `{}`", depth));
        }
        if let Some(next_used) = selection.next_used_bytes {
            parts.push(format!(
                "next_element_cost: `{}`",
                byte_label(next_used.saturating_sub(selection.used_bytes))
            ));
        }
        if let Some(focus_symbol) = &selection.focus_symbol {
            parts.push(format!("focus: `{}`", focus_symbol));
        }
        if let (Some(selected), Some(total)) =
            (selection.elements_selected, selection.elements_total)
        {
            parts.push(format!("selected: `{}/{}`", selected, total));
        }
        parts.push(format!("truncated: `{}`", selection.truncated));
        parts.push(format!("used: `{}`", byte_label(selection.used_bytes)));
        out.push_str(&format!("selection: {}\n\n", parts.join(", ")));
    }

    let all_decisions: Vec<&DecisionBlock> =
        contexts.iter().flat_map(|c| c.decisions.iter()).collect();

    for ctx in contexts {
        let has_content = !ctx.fn_defs.is_empty() || !ctx.type_defs.is_empty();

        if !has_content && ctx.module_name.is_none() {
            continue;
        }

        out.push_str("---\n\n");

        if let Some(name) = &ctx.module_name {
            out.push_str(&format!("## Module: {}\n\n", name));
        } else {
            out.push_str(&format!("## {}\n\n", ctx.source_file));
        }

        if let Some(intent) = &ctx.intent {
            out.push_str(&format!("> {}\n\n", intent));
        }

        if !ctx.exposes_opaque.is_empty() {
            out.push_str(&format!(
                "exposes opaque: `[{}]`\n",
                ctx.exposes_opaque.join(", ")
            ));
        }

        // Effects: merge api/module when identical
        if effects_equal(&ctx.api_effects, &ctx.module_effects) {
            if !ctx.module_effects.is_empty() {
                out.push_str(&format!("effects: `[{}]`\n", ctx.module_effects.join(", ")));
            }
        } else {
            out.push_str(&format!(
                "api_effects: `[{}]`  \nmodule_effects: `[{}]`\n",
                ctx.api_effects.join(", "),
                ctx.module_effects.join(", ")
            ));
        }
        if let Some(main_effects) = &ctx.main_effects
            && !effects_equal(main_effects, &ctx.module_effects)
        {
            out.push_str(&format!("main_effects: `[{}]`\n", main_effects.join(", ")));
        }
        out.push('\n');

        // Types
        for td in &ctx.type_defs {
            match td {
                TypeDef::Sum { name, variants, .. } => {
                    out.push_str(&format!("### type {}\n", name));
                    let vars: Vec<String> = variants
                        .iter()
                        .map(|v| {
                            if v.fields.is_empty() {
                                v.name.clone()
                            } else {
                                format!("{}({})", v.name, v.fields.join(", "))
                            }
                        })
                        .collect();
                    out.push_str(&format!("`{}`\n\n", vars.join("` | `")));
                }
                TypeDef::Product { name, fields, .. } => {
                    out.push_str(&format!("### record {}\n", name));
                    let flds: Vec<String> = fields
                        .iter()
                        .map(|(fname, ftype)| format!("{}: {}", fname, ftype))
                        .collect();
                    out.push_str(&format!("`{}`\n\n", flds.join("`, `")));
                }
            }
        }

        // Functions
        for fd in &ctx.fn_defs {
            if fd.name == "main" {
                continue;
            }

            out.push_str(&format!("### `{}`\n", fn_sig_with_effects(fd)));

            // Only show analysis flags when non-default
            if ctx.fn_auto_tco.contains(&fd.name) {
                out.push_str("tco: `true`  \n");
            }

            let specs = ctx.fn_specs.get(&fd.name).cloned().unwrap_or_default();
            if !specs.is_empty() {
                let label = if specs.len() == 1 { "spec" } else { "specs" };
                out.push_str(&format!("{label}: `[{}]`  \n", specs.join(", ")));
            }

            if let Some(desc) = &fd.desc {
                out.push_str(&format!("> {}\n", desc));
            }

            let verify_samples: Vec<String> = ctx
                .verify_samples
                .get(&fd.name)
                .cloned()
                .unwrap_or_default()
                .into_iter()
                .map(|sample| format!("`{sample}`"))
                .collect();
            if !verify_samples.is_empty() {
                out.push_str(&format!("verify: {}\n", verify_samples.join(", ")));
            }

            out.push('\n');
        }
    }

    // Decisions collected from all files
    if !all_decisions.is_empty() {
        out.push_str("---\n\n## Decisions\n\n");
        for dec in all_decisions {
            out.push_str(&format!("### {} ({})\n", dec.name, dec.date));
            out.push_str(&format!("**Chosen:** {}", decision_ref_text(&dec.chosen)));
            if !dec.rejected.is_empty() {
                let rejected = dec
                    .rejected
                    .iter()
                    .map(decision_ref_text)
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!(" — **Rejected:** {}", rejected));
            }
            out.push('\n');
            if !dec.reason.is_empty() {
                let reason = if dec.reason.len() > 160 {
                    format!("{}…", dec.reason[..160].trim_end())
                } else {
                    dec.reason.clone()
                };
                out.push_str(&format!("> {}\n", reason));
            }
            if !dec.impacts.is_empty() {
                out.push_str(&format!(
                    "impacts: `{}`\n",
                    impact_texts(&dec.impacts).join("`, `")
                ));
            }
            out.push('\n');
        }
    }

    out
}

pub(super) fn format_context_json(
    contexts: &[FileContext],
    entry_file: &str,
    selection: Option<&ContextSelection>,
) -> String {
    serialize_dense(&context_doc_to_json(contexts, entry_file, selection))
}

/// Serialize JSON with objects on multiple lines but arrays inline.
///
/// Standard `to_string_pretty` puts every element of every array on its own
/// indented line, which inflates `aver context --json` output by ~40% vs the
/// pre-serde hand-written format. Effects/depends/verify lists are short
/// strings — keeping them inline gives back the density without losing the
/// human-readable object structure.
fn serialize_dense<T: Serialize>(value: &T) -> String {
    let mut buf = Vec::new();
    let formatter = ObjectPrettyArrayCompactFormatter::default();
    let mut ser = serde_json::Serializer::with_formatter(&mut buf, formatter);
    value
        .serialize(&mut ser)
        .expect("context JSON should always serialize");
    String::from_utf8(buf).expect("serde_json output is utf-8")
}

#[derive(Default)]
struct ObjectPrettyArrayCompactFormatter {
    indent_level: usize,
    object_has_value: Vec<bool>,
}

impl ObjectPrettyArrayCompactFormatter {
    fn write_indent<W: ?Sized + std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        for _ in 0..self.indent_level {
            writer.write_all(b"  ")?;
        }
        Ok(())
    }
}

impl serde_json::ser::Formatter for ObjectPrettyArrayCompactFormatter {
    fn begin_object<W: ?Sized + std::io::Write>(&mut self, writer: &mut W) -> std::io::Result<()> {
        self.indent_level += 1;
        self.object_has_value.push(false);
        writer.write_all(b"{")
    }

    fn end_object<W: ?Sized + std::io::Write>(&mut self, writer: &mut W) -> std::io::Result<()> {
        self.indent_level -= 1;
        let had_value = self.object_has_value.pop().unwrap_or(false);
        if had_value {
            writer.write_all(b"\n")?;
            self.write_indent(writer)?;
        }
        writer.write_all(b"}")
    }

    fn begin_object_key<W: ?Sized + std::io::Write>(
        &mut self,
        writer: &mut W,
        first: bool,
    ) -> std::io::Result<()> {
        if !first {
            writer.write_all(b",")?;
        }
        writer.write_all(b"\n")?;
        self.write_indent(writer)
    }

    fn begin_object_value<W: ?Sized + std::io::Write>(
        &mut self,
        writer: &mut W,
    ) -> std::io::Result<()> {
        if let Some(last) = self.object_has_value.last_mut() {
            *last = true;
        }
        writer.write_all(b": ")
    }

    fn begin_array<W: ?Sized + std::io::Write>(&mut self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(b"[")
    }

    fn end_array<W: ?Sized + std::io::Write>(&mut self, writer: &mut W) -> std::io::Result<()> {
        writer.write_all(b"]")
    }

    fn begin_array_value<W: ?Sized + std::io::Write>(
        &mut self,
        writer: &mut W,
        first: bool,
    ) -> std::io::Result<()> {
        if !first {
            writer.write_all(b", ")?;
        }
        Ok(())
    }
}

pub(super) fn collect_all_decisions(contexts: &[FileContext]) -> Vec<&DecisionBlock> {
    contexts
        .iter()
        .flat_map(|ctx| ctx.decisions.iter())
        .collect()
}

pub(super) fn format_decisions_md(
    decisions: &[&DecisionBlock],
    entry_file: &str,
    selection: Option<&ContextSelection>,
) -> String {
    let mut out = String::new();
    out.push_str(&format!("# Aver Decisions - {}\n\n", entry_file));
    out.push_str("_Generated by `aver context --decisions-only`_\n\n");
    if let Some(selection) = selection {
        let mut parts = vec![format!("mode: `{}`", selection.depth_mode)];
        if let Some(budget) = selection.budget_bytes {
            parts.push(format!("budget: `{}`", byte_label(budget)));
        }
        if let Some(depth) = selection.included_depth {
            parts.push(format!("included_depth: `{}`", depth));
        }
        if let Some(next_used) = selection.next_used_bytes {
            parts.push(format!(
                "next_element_cost: `{}`",
                byte_label(next_used.saturating_sub(selection.used_bytes))
            ));
        }
        if let Some(focus_symbol) = &selection.focus_symbol {
            parts.push(format!("focus: `{}`", focus_symbol));
        }
        if let (Some(selected), Some(total)) =
            (selection.elements_selected, selection.elements_total)
        {
            parts.push(format!("selected: `{}/{}`", selected, total));
        }
        parts.push(format!("truncated: `{}`", selection.truncated));
        parts.push(format!("used: `{}`", byte_label(selection.used_bytes)));
        out.push_str(&format!("selection: {}\n\n", parts.join(", ")));
    }
    if decisions.is_empty() {
        out.push_str("No decision blocks found.\n");
        return out;
    }

    for dec in decisions {
        out.push_str(&format!("## {} ({})\n\n", dec.name, dec.date));
        out.push_str(&format!("**Chosen:** {}\n", decision_ref_text(&dec.chosen)));
        if !dec.rejected.is_empty() {
            let rejected = dec
                .rejected
                .iter()
                .map(decision_ref_text)
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("**Rejected:** {}\n", rejected));
        }
        if !dec.impacts.is_empty() {
            out.push_str(&format!(
                "**Impacts:** {}\n",
                impact_texts(&dec.impacts).join(", ")
            ));
        }
        if !dec.reason.is_empty() {
            out.push_str(&format!("\n> {}\n", dec.reason));
        }
        if let Some(author) = &dec.author {
            out.push_str(&format!("\nAuthor: `{}`\n", author));
        }
        out.push('\n');
    }

    out
}

pub(super) fn format_decisions_json(
    decisions: &[&DecisionBlock],
    entry_file: &str,
    selection: Option<&ContextSelection>,
) -> String {
    serialize_dense(&decisions_doc_to_json(decisions, entry_file, selection))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{HashMap, HashSet};

    fn file_context_with_spec() -> FileContext {
        FileContext {
            source_file: "examples/spec.av".to_string(),
            module_name: Some("Spec".to_string()),
            intent: Some("spec demo".to_string()),
            depends: vec!["Math.Core".to_string()],
            exposes: vec!["fib".to_string()],
            exposes_opaque: vec![],
            api_effects: vec![],
            module_effects: vec![],
            main_effects: None,
            fn_defs: vec![FnDef {
                name: "fib".to_string(),
                line: 1,
                params: vec![("n".to_string(), "Int".to_string())],
                return_type: "Int".to_string(),
                effects: vec![],
                desc: Some("demo".to_string()),
                body: std::sync::Arc::new(aver::ast::FnBody::Block(vec![])),
                resolution: None,
            }],
            all_fn_defs: vec![FnDef {
                name: "fib".to_string(),
                line: 1,
                params: vec![("n".to_string(), "Int".to_string())],
                return_type: "Int".to_string(),
                effects: vec![],
                desc: Some("demo".to_string()),
                body: std::sync::Arc::new(aver::ast::FnBody::Block(vec![])),
                resolution: None,
            }],
            fn_auto_tco: HashSet::new(),
            fn_recursive_callsites: HashMap::new(),
            fn_recursive_scc_id: HashMap::new(),
            fn_specs: HashMap::from([("fib".to_string(), vec!["fibSpec".to_string()])]),
            fn_direct_calls: HashMap::new(),
            type_defs: vec![],
            verify_blocks: vec![aver::ast::VerifyBlock::new_unspanned(
                "fib".to_string(),
                2,
                vec![(
                    aver::ast::Spanned::bare(aver::ast::Expr::FnCall(
                        Box::new(aver::ast::Spanned::bare(aver::ast::Expr::Ident(
                            "fib".to_string(),
                        ))),
                        vec![aver::ast::Spanned::bare(aver::ast::Expr::Literal(
                            aver::ast::Literal::Int(5),
                        ))],
                    )),
                    aver::ast::Spanned::bare(aver::ast::Expr::Literal(aver::ast::Literal::Int(8))),
                )],
                aver::ast::VerifyKind::Cases,
            )],
            verify_counts: HashMap::from([("fib".to_string(), 1)]),
            verify_samples: HashMap::from([("fib".to_string(), vec!["fib(5) => 8".to_string()])]),
            decisions: vec![],
        }
    }

    #[test]
    fn markdown_context_renders_spec_refs() {
        let out = format_context_md(&[file_context_with_spec()], "examples/spec.av", None);
        assert!(out.contains("spec: `[fibSpec]`"));
    }

    #[test]
    fn json_context_renders_specs_array() {
        let out = format_context_json(&[file_context_with_spec()], "examples/spec.av", None);
        let json: serde_json::Value = serde_json::from_str(&out).expect("valid context JSON");
        assert_eq!(json["schema_version"], 7);
        assert_eq!(json["modules"][0]["depends"][0], "Math.Core");
        assert_eq!(
            json["modules"][0]["functions"][0]["sig"],
            "fib(n: Int) -> Int"
        );
        assert_eq!(json["modules"][0]["functions"][0]["specs"][0], "fibSpec");
        assert_eq!(json["modules"][0]["functions"][0]["verify_count"], 1);
        assert_eq!(
            json["modules"][0]["functions"][0]["verify"][0],
            "fib(5) => 8"
        );
    }

    #[test]
    fn json_context_renders_compact_analysis() {
        let mut ctx = file_context_with_spec();
        ctx.fn_auto_tco.insert("fib".to_string());

        let out = format_context_json(&[ctx], "examples/spec.av", None);
        let json: serde_json::Value = serde_json::from_str(&out).expect("valid context JSON");
        assert_eq!(json["modules"][0]["functions"][0]["analysis"]["tco"], true);
    }

    #[test]
    fn json_context_omits_empty_fields() {
        let ctx = FileContext {
            source_file: "examples/main.av".to_string(),
            module_name: None,
            intent: None,
            depends: vec![],
            exposes: vec![],
            exposes_opaque: vec![],
            api_effects: vec![],
            module_effects: vec![],
            main_effects: None,
            fn_defs: vec![FnDef {
                name: "main".to_string(),
                line: 1,
                params: vec![],
                return_type: "Unit".to_string(),
                effects: vec![],
                desc: None,
                body: std::sync::Arc::new(aver::ast::FnBody::Block(vec![])),
                resolution: None,
            }],
            all_fn_defs: vec![FnDef {
                name: "main".to_string(),
                line: 1,
                params: vec![],
                return_type: "Unit".to_string(),
                effects: vec![],
                desc: None,
                body: std::sync::Arc::new(aver::ast::FnBody::Block(vec![])),
                resolution: None,
            }],
            fn_auto_tco: HashSet::new(),
            fn_recursive_callsites: HashMap::new(),
            fn_recursive_scc_id: HashMap::new(),
            fn_specs: HashMap::new(),
            fn_direct_calls: HashMap::new(),
            type_defs: vec![],
            verify_blocks: vec![],
            verify_counts: HashMap::new(),
            verify_samples: HashMap::new(),
            decisions: vec![],
        };

        let out = format_context_json(&[ctx], "examples/main.av", None);
        assert!(!out.contains("\"module\": null"));
        assert!(!out.contains("\"functions\": []"));
        assert!(!out.contains("\"decisions\": []"));
    }

    #[test]
    fn json_context_renders_selection_metadata() {
        let selection = ContextSelection {
            depth_mode: "auto".to_string(),
            budget_bytes: Some(10 * 1024),
            included_depth: Some(1),
            next_depth: Some(2),
            next_used_bytes: Some(2100),
            truncated: true,
            used_bytes: 1779,
            focus_symbol: Some("Json.fromString".to_string()),
            elements_selected: Some(5),
            elements_total: Some(10),
        };
        let out = format_context_json(
            &[file_context_with_spec()],
            "examples/spec.av",
            Some(&selection),
        );
        assert!(out.contains("\"selection\": {"));
        assert!(out.contains("\"depth_mode\": \"auto\""));
        assert!(out.contains("\"budget_bytes\": 10240"));
        assert!(out.contains("\"included_depth\": 1"));
        assert!(out.contains("\"next_element_cost\": 321"));
        assert!(out.contains("\"elements_selected\": 5"));
        assert!(out.contains("\"elements_total\": 10"));
        assert!(out.contains("\"focus_symbol\": \"Json.fromString\""));
        assert!(out.contains("\"truncated\": true"));
        assert!(out.contains("\"used_bytes\": 1779"));
    }
}
