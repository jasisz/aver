//! Independent source observation and replay of a generated process.
//!
//! Models are requested by referring to `__fSourceTrace` in a verify block.
//! The source observer reads retained, stamped source, never protocol states
//! or answer bodies. The replay observer calls the actual generated protocol.
//! A typed input is an answer or permission to cross a source self-tail-call;
//! it is not an interpreter instruction budget. Only answers advance position.

use super::{FnSigs, ProcessProtocol, build};
use crate::ast::*;
use std::collections::HashMap;

mod effects;
mod imports;
mod laws;
mod source;
mod surface;
pub(super) use laws::strengthen_laws;

pub(super) fn generate(
    items: &[TopLevel],
    sources: &[FnDef],
    protocols: &mut [ProcessProtocol],
    fn_sigs: &FnSigs,
    imported: &HashMap<String, ProcessProtocol>,
) -> Result<Vec<TopLevel>, Vec<crate::types::checker::TypeError>> {
    let mut generated = Vec::new();
    let segments: Vec<_> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd.clone()),
            _ => None,
        })
        .collect();
    let exposes = crate::visibility::module_decl(items)
        .map(|m| m.exposes.as_slice())
        .unwrap_or(&[]);
    for protocol in protocols {
        let entry = format!("__{}SourceTrace", protocol.fn_name);
        let requested = items.iter().any(|item| {
            let TopLevel::Verify(block) = item else {
                return false;
            };
            (block.fn_name == entry || block.fn_name == format!("{entry}From"))
                || block.cases.iter().any(|(left, right)| {
                    [left, right].iter().any(|expr| {
                        crate::codegen::expr_walk::any(
                            expr,
                            &mut |e| matches!(&e.node, Expr::Ident(name) if name == &entry),
                        )
                    })
                })
        });
        // Library processes carry their source observer in their own module,
        // where private helpers and private layouts retain their original scope.
        if !requested
            && !(exposes.contains(&protocol.fn_name)
                || (exposes.is_empty() && !protocol.fn_name.starts_with('_')))
        {
            continue;
        }
        let owner = crate::visibility::module_decl(items)
            .map(|m| m.name.as_str())
            .unwrap_or("Entry");
        let model = Model::new(protocol, sources, &segments, fn_sigs, imported, owner);
        match model.generate() {
            Ok(mut result) => {
                let root = model.source(&protocol.fn_name).expect("retained root");
                let trace = super::ProcessTrace {
                    operations: model
                        .kinds
                        .iter()
                        .filter(|k| k.operation.is_some())
                        .cloned()
                        .collect(),
                    input: format!("{}Input", model.upper),
                    query: format!("{}Query", model.upper),
                    event: format!("{}Event", model.upper),
                    result: model.result_type(root),
                    source: model.source_name(root),
                };
                protocol.trace = Some(trace);
                generated.append(&mut result);
            }
            Err(_) if !requested => {}
            Err(message) => {
                return Err(vec![super::error_at(
                    1,
                    format!(
                        "Source request-trace model for '{}': {message}. No trace-equivalence claim was generated",
                        protocol.fn_name
                    ),
                )]);
            }
        }
    }
    Ok(generated)
}

struct Model<'a> {
    protocol: &'a ProcessProtocol,
    sources: &'a [FnDef],
    segments: &'a [FnDef],
    fn_sigs: &'a FnSigs,
    imported: &'a HashMap<String, ProcessProtocol>,
    prefix: String,
    upper: String,
    operations: HashMap<String, super::ProtocolKind>,
    kinds: Vec<super::ProtocolKind>,
}

impl<'a> Model<'a> {
    fn new(
        protocol: &'a ProcessProtocol,
        sources: &'a [FnDef],
        segments: &'a [FnDef],
        fn_sigs: &'a FnSigs,
        imported: &'a HashMap<String, ProcessProtocol>,
        owner: &str,
    ) -> Self {
        let mut kinds = protocol.kinds.clone();
        if let Some(root) = sources.iter().find(|fd| fd.name == protocol.fn_name) {
            for effect in &root.effects {
                if effect.node == "yield"
                    || kinds
                        .iter()
                        .any(|k| k.operation.as_ref() == Some(&effect.node))
                {
                    continue;
                }
                if let Some((params, ret, _)) = fn_sigs.get(&effect.node) {
                    let mut name = format!(
                        "Host{}",
                        effect
                            .node
                            .split('.')
                            .map(build::capitalize)
                            .collect::<String>()
                    );
                    while kinds.iter().any(|k| k.name == name) {
                        name.push('_');
                    }
                    kinds.push(super::ProtocolKind {
                        name,
                        operation: Some(effect.node.clone()),
                        arg_types: params.iter().map(|ty| ty.display()).collect(),
                        answer_type: Some(ret.display()),
                        state: String::new(),
                        answer_fn: String::new(),
                        variants: vec![],
                    });
                }
            }
        }
        Self {
            protocol,
            sources,
            segments,
            fn_sigs,
            imported,
            prefix: format!(
                "__{}Trace{}",
                protocol.fn_name,
                owner
                    .bytes()
                    .map(|b| format!("{b:02x}"))
                    .collect::<String>()
            ),
            upper: format!("__{}Trace", build::capitalize(&protocol.fn_name)),
            operations: kinds
                .iter()
                .filter_map(|kind| kind.operation.as_ref().map(|op| (op.clone(), kind.clone())))
                .collect(),
            kinds,
        }
    }

    fn result_type(&self, fd: &FnDef) -> String {
        format!("{}{}Result", self.upper, build::capitalize(&fd.name))
    }
    fn source_name(&self, fd: &FnDef) -> String {
        format!("{}Source{}", self.prefix, build::capitalize(&fd.name))
    }
    fn source(&self, name: &str) -> Option<&'a FnDef> {
        self.sources
            .iter()
            .chain(self.segments)
            .find(|fd| fd.name == name)
    }

    fn other_inputs(&self, accepted: &str) -> Vec<Pattern> {
        let mut patterns = Vec::new();
        patterns.push(Pattern::Constructor(
            format!("{}Input.Foreign", self.upper),
            vec![],
        ));
        if accepted != "Advance" {
            patterns.push(Pattern::Constructor(
                format!("{}Input.Advance", self.upper),
                vec![],
            ));
        }
        for kind in &self.kinds {
            if kind.answer_type.is_some() && format!("Answer{}", kind.name) != accepted {
                patterns.push(Pattern::Constructor(
                    format!("{}Input.Answer{}", self.upper, kind.name),
                    vec!["_".into()],
                ));
            }
        }
        patterns
    }

    fn generate(&self) -> Result<Vec<TopLevel>, String> {
        let root = self
            .source(&self.protocol.fn_name)
            .ok_or("missing retained source")?;
        let mut reached = Vec::new();
        let mut imports = Vec::new();
        self.reachable(root, &mut reached, &mut imports)?;
        // A returned subtrace of a recursive helper needs its own splice
        // invariant. Until that obligation is generated, reject the shape
        // instead of exporting a fuel-bounded stand-in as source semantics.
        for helper in &reached {
            if helper.name != root.name
                && helper.body.stmts().iter().any(|stmt| {
                    let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
                    crate::codegen::expr_walk::any(expr, &mut |expr| match &expr.node {
                        Expr::FnCall(callee, _) => {
                            build::dotted_name(callee).as_deref() == Some(&helper.name)
                        }
                        Expr::TailCall(call) => call.target == helper.name,
                        _ => false,
                    })
                })
            {
                return Err(format!(
                    "recursive helper '{}' needs a compositional subtrace theorem",
                    helper.name
                ));
            }
        }
        if let Some(helper) = imports
            .iter()
            .find(|helper| helper.kinds.iter().any(|kind| kind.operation.is_none()))
        {
            return Err(format!(
                "recursive imported helper '{}' needs a compositional subtrace theorem",
                helper.fn_name
            ));
        }
        let imported_signatures: Vec<_> =
            imports.iter().map(|p| self.import_signature(p)).collect();
        let mut all = reached.clone();
        all.extend(imported_signatures.iter());
        let mut text = self.surface(&all);
        for protocol in imports {
            text.push_str(&self.adapter(protocol)?);
        }
        text.push_str(&self.driver(root));
        let tokens = crate::lexer::Lexer::new(&text)
            .tokenize()
            .map_err(|e| format!("invalid generated observer: {e}"))?;
        let mut items = crate::parser::Parser::new_compiler_generated(tokens)
            .parse()
            .map_err(|e| format!("invalid generated observer: {e}"))?;
        // Model in-place effects inside the actual protocol code with the same
        // dynamic tape semantics; the original source body remains independent.
        self.observe_segments(&mut items, root)?;
        for item in &items {
            let TopLevel::FnDef(fd) = item else { continue };
            for stmt in fd.body.stmts() {
                let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
                let mut unsupported = None;
                crate::codegen::expr_walk::walk(expr, &mut |expr| {
                    if let Expr::FnCall(callee, _) = &expr.node
                        && let Some(name) = build::dotted_name(callee)
                        && self.fn_sigs.get(&name).is_some_and(|sig| !sig.2.is_empty())
                    {
                        unsupported = Some(name);
                    }
                });
                if let Some(name) = unsupported {
                    return Err(format!(
                        "in-place effects of imported segment '{name}' need an owning-module observer"
                    ));
                }
            }
        }
        for fd in &reached {
            if let Some(Stmt::Expr(expr)) = fd.body.stmts().last()
                && let Some(name) = self.tail_helper(expr, &fd.name)
            {
                return Err(format!(
                    "tail entry into yielding helper '{name}' needs a stuttering-alignment theorem"
                ));
            }
        }
        for fd in reached {
            items.push(TopLevel::FnDef(source::Compiler::new(self, fd).compile()?));
        }
        Ok(items)
    }

    fn tail_helper(&self, expr: &Spanned<Expr>, owner: &str) -> Option<String> {
        match &expr.node {
            Expr::Match { arms, .. } => arms
                .iter()
                .find_map(|arm| self.tail_helper(&arm.body, owner)),
            Expr::FnCall(callee, _) => build::dotted_name(callee).filter(|name| {
                name != owner
                    && !self.operations.contains_key(name)
                    && (self.imported.contains_key(name)
                        || self.source(name).is_some_and(super::is_yield_fn))
            }),
            _ => None,
        }
    }

    fn reachable(
        &self,
        fd: &'a FnDef,
        reached: &mut Vec<&'a FnDef>,
        imports: &mut Vec<&'a ProcessProtocol>,
    ) -> Result<(), String> {
        if reached.iter().any(|seen| seen.name == fd.name) {
            return Ok(());
        }
        reached.push(fd);
        for effect in &fd.effects {
            if effect.node != "yield" && !self.operations.contains_key(&effect.node) {
                return Err(format!(
                    "in-place effect '{}' needs an explicit effect model",
                    effect.node
                ));
            }
        }
        let mut calls = Vec::new();
        for stmt in fd.body.stmts() {
            let (Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) = stmt;
            crate::codegen::expr_walk::walk(expr, &mut |e| {
                if let Expr::FnCall(callee, _) = &e.node
                    && let Some(name) = build::dotted_name(callee)
                {
                    calls.push(name);
                }
            });
        }
        for name in calls {
            if let Some(helper) = self.source(&name) {
                if !helper.effects.is_empty() {
                    self.reachable(helper, reached, imports)?;
                }
            } else if self
                .fn_sigs
                .get(&name)
                .is_some_and(|sig| sig.2.iter().any(|e| e == "yield"))
            {
                let imported = self.imported.get(&name).filter(|p| p.trace.is_some()).ok_or_else(|| format!("imported yielding helper '{name}' does not expose a supported source observer"))?;
                if !imports.iter().any(|p| p.fn_name == name) {
                    imports.push(imported);
                }
            }
        }
        Ok(())
    }
}
