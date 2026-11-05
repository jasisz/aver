//! Drop the defined functions nothing in the module can reach.
//!
//! The emitter registers whole helper families for every type a program
//! instantiates (per-`(K, V)` map helpers, per-type comparisons and
//! equalities, the `Int` bignum package) and allocates their function
//! indices as it goes, so which of them a program actually calls is only
//! known once every body is written. This pass reads that answer from the
//! finished module itself: the roots are what a host or the engine can name
//! (every export, the start function, every function an element segment or a
//! global's initializer refers to), and the edges are the `call`,
//! `return_call` and `ref.func` instructions of the bodies. An indirect call
//! goes through a table or a function reference, and both are filled from a
//! root or from a `ref.func` some live body executes, so neither needs an
//! edge of its own.
//!
//! The live defined functions keep their relative order and are renumbered
//! densely after the imports, which stay as they are (an import is part of
//! the host contract, not dead code). Same input, same bytes. The renumbering
//! is returned so the certificate plans printed against the emission-time
//! indices can follow it.

use std::collections::HashSet;

use wasm_encoder::reencode::{self, Reencode};
use wasmparser::{ExternalKind, Name, Operator, Parser, Payload, TypeRef};

use super::WasmGcError;

/// Where each function of the emitted module ended up: `None` for a defined
/// function dropped as dead. Imports keep their index.
#[derive(Debug, Clone)]
pub(super) struct FnRenumbering {
    new_index: Vec<Option<u32>>,
}

impl FnRenumbering {
    /// The index function `old` has in the shrunk module, `None` when it was
    /// dropped (or never existed).
    pub(super) fn get(&self, old: u32) -> Option<u32> {
        self.new_index.get(old as usize).copied().flatten()
    }

    /// How many defined functions were dropped.
    pub(super) fn dropped(&self) -> usize {
        self.new_index.iter().filter(|n| n.is_none()).count()
    }
}

/// The module without the defined functions none of its roots reaches, and
/// where every function went. `kept` are roots the module's own sections do
/// not name but a reader of its bytes relies on. A module with nothing to
/// drop is returned as it came, byte for byte.
pub(super) fn drop_dead_functions(
    bytes: Vec<u8>,
    kept: &[u32],
) -> Result<(Vec<u8>, FnRenumbering), WasmGcError> {
    let mut graph = CallGraph::read(&bytes).map_err(|e| {
        WasmGcError::Validation(format!("dead-function pass: cannot read module: {e}"))
    })?;
    graph.roots.extend_from_slice(kept);
    let live = graph.live();
    let mut new_index = Vec::with_capacity(graph.total() as usize);
    let mut next = 0u32;
    for f in 0..graph.total() {
        if f < graph.imports || live.contains(&f) {
            new_index.push(Some(next));
            next += 1;
        } else {
            new_index.push(None);
        }
    }
    let renumbering = FnRenumbering { new_index };
    if renumbering.dropped() == 0 {
        return Ok((bytes, renumbering));
    }
    let mut module = wasm_encoder::Module::new();
    let mut rewriter = Renumber {
        renumbering: &renumbering,
        imports: graph.imports,
    };
    rewriter
        .parse_core_module(&mut module, Parser::new(0), &bytes)
        .map_err(|e| {
            WasmGcError::Validation(format!("dead-function pass: cannot rewrite module: {e:?}"))
        })?;
    Ok((module.finish(), renumbering))
}

/// The certificate plans of a module printed against its emission-time
/// function indices, moved onto the indices of the shrunk module.
///
/// A plan is printed only for a live function (the caller filters), so every
/// function a plan calls is live too: its body calls it. A plan that still
/// names a dropped function is not one the bytes agree with, and declines
/// rather than point at another function's index. A helper slot of the type
/// table whose helper was dropped is cleared: no plan calls it.
pub(super) fn renumber_plans(plans: &mut crate::codegen::cert::ModulePlans, at: &FnRenumbering) {
    use crate::codegen::cert::PlanExpr;
    fn walk(e: &mut PlanExpr, at: &FnRenumbering) -> Result<(), u32> {
        use crate::codegen::cert::PlanCallee;
        let each = |args: &mut [PlanExpr]| args.iter_mut().try_for_each(|a| walk(a, at));
        match e {
            PlanExpr::Literal(_)
            | PlanExpr::Local(_)
            | PlanExpr::IntWord(..)
            | PlanExpr::IntCmp(..) => Ok(()),
            PlanExpr::Call(PlanCallee::Fn(f), args) | PlanExpr::TailCall(f, args) => {
                *f = at.get(*f).ok_or(*f)?;
                each(args)
            }
            PlanExpr::Call(_, args)
            | PlanExpr::RecordCreate(_, args)
            | PlanExpr::Construct(_, _, args)
            | PlanExpr::Interp(args)
            | PlanExpr::List(_, args) => each(args),
            PlanExpr::Let(_, a, b) | PlanExpr::BinOp(_, a, b) => {
                walk(a, at)?;
                walk(b, at)
            }
            PlanExpr::Neg(x)
            | PlanExpr::Project(_, _, x)
            | PlanExpr::Try(x, _)
            | PlanExpr::Scope(x) => walk(x, at),
            PlanExpr::If(c, t, el) => {
                walk(c, at)?;
                walk(t, at)?;
                walk(el, at)
            }
            PlanExpr::Match(s, arms) => {
                walk(s, at)?;
                arms.iter_mut().try_for_each(|(_, b)| walk(b, at))
            }
        }
    }
    plans.fns.retain_mut(|f| match at.get(f.func_idx) {
        Some(idx) => {
            f.func_idx = idx;
            if let Ok(plan) = &mut f.plan
                && let Err(dead) = walk(&mut plan.body, at)
            {
                f.plan = Err(format!(
                    "its plan calls function {dead}, which the module does not contain"
                ));
            }
            true
        }
        None => false,
    });
    let slot = |f: &mut Option<u32>| *f = f.and_then(|old| at.get(old));
    slot(&mut plans.aint_eq_idx);
    let types = &mut plans.types;
    slot(&mut types.int_sat);
    slot(&mut types.int_chk);
    slot(&mut types.str_from_int);
    types
        .list_cons
        .retain_mut(|(_, f)| at.get(*f).map(|n| *f = n).is_some());
    types
        .list_helpers
        .retain_mut(|(_, _, f)| at.get(*f).map(|n| *f = n).is_some());
    types
        .bytes_helpers
        .retain_mut(|(_, f)| at.get(*f).map(|n| *f = n).is_some());
}

/// The call graph of a module, over the whole function index space.
#[derive(Debug, Default)]
struct CallGraph {
    imports: u32,
    /// Per defined function, the functions its body calls or takes by
    /// `ref.func`.
    edges: Vec<Vec<u32>>,
    roots: Vec<u32>,
}

impl CallGraph {
    fn total(&self) -> u32 {
        self.imports + self.edges.len() as u32
    }

    fn read(bytes: &[u8]) -> wasmparser::Result<Self> {
        let mut graph = CallGraph::default();
        for payload in Parser::new(0).parse_all(bytes) {
            match payload? {
                Payload::ImportSection(section) => {
                    for import in section.into_imports() {
                        if matches!(import?.ty, TypeRef::Func(_) | TypeRef::FuncExact(_)) {
                            graph.imports += 1;
                        }
                    }
                }
                Payload::ExportSection(section) => {
                    for export in section {
                        let export = export?;
                        if export.kind == ExternalKind::Func {
                            graph.roots.push(export.index);
                        }
                    }
                }
                Payload::StartSection { func, .. } => graph.roots.push(func),
                Payload::GlobalSection(section) => {
                    for global in section {
                        refs_in(global?.init_expr.get_operators_reader(), &mut graph.roots)?;
                    }
                }
                Payload::ElementSection(section) => {
                    for element in section {
                        match element?.items {
                            wasmparser::ElementItems::Functions(funcs) => {
                                for f in funcs {
                                    graph.roots.push(f?);
                                }
                            }
                            wasmparser::ElementItems::Expressions(_, exprs) => {
                                for expr in exprs {
                                    refs_in(expr?.get_operators_reader(), &mut graph.roots)?;
                                }
                            }
                        }
                    }
                }
                Payload::CodeSectionEntry(body) => {
                    let mut out = Vec::new();
                    refs_in(body.get_operators_reader()?, &mut out)?;
                    graph.edges.push(out);
                }
                _ => {}
            }
        }
        Ok(graph)
    }

    /// Every function some root reaches, imports included.
    fn live(&self) -> HashSet<u32> {
        let mut live = HashSet::new();
        let mut stack: Vec<u32> = self.roots.clone();
        while let Some(f) = stack.pop() {
            if !live.insert(f) {
                continue;
            }
            if let Some(callees) = f
                .checked_sub(self.imports)
                .and_then(|k| self.edges.get(k as usize))
            {
                stack.extend(callees.iter().copied());
            }
        }
        live
    }
}

/// The functions `ops` calls directly or takes a reference to.
fn refs_in(mut ops: wasmparser::OperatorsReader<'_>, out: &mut Vec<u32>) -> wasmparser::Result<()> {
    while !ops.eof() {
        match ops.read()? {
            Operator::Call { function_index }
            | Operator::ReturnCall { function_index }
            | Operator::RefFunc { function_index } => out.push(function_index),
            _ => {}
        }
    }
    Ok(())
}

/// The reencoder that leaves out the dead defined functions and renumbers
/// every function reference that remains.
struct Renumber<'a> {
    renumbering: &'a FnRenumbering,
    imports: u32,
}

/// A live part of the module referring to a dropped function — impossible
/// when the graph above is complete, so a rewrite that meets one fails.
#[derive(Debug)]
struct DeadReference(#[allow(dead_code)] u32);

impl Reencode for Renumber<'_> {
    type Error = DeadReference;

    fn function_index(&mut self, func: u32) -> Result<u32, reencode::Error<DeadReference>> {
        self.renumbering
            .get(func)
            .ok_or(reencode::Error::UserError(DeadReference(func)))
    }

    fn parse_function_section(
        &mut self,
        functions: &mut wasm_encoder::FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> Result<(), reencode::Error<DeadReference>> {
        for (k, ty) in section.into_iter().enumerate() {
            let ty = ty?;
            if self.renumbering.get(self.imports + k as u32).is_some() {
                functions.function(self.type_index(ty)?);
            }
        }
        Ok(())
    }

    fn parse_code_section(
        &mut self,
        code: &mut wasm_encoder::CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> Result<(), reencode::Error<DeadReference>> {
        for (k, body) in section.into_iter().enumerate() {
            let body = body?;
            if self.renumbering.get(self.imports + k as u32).is_some() {
                self.parse_function_body(code, body)?;
            }
        }
        Ok(())
    }

    fn parse_custom_name_subsection(
        &mut self,
        names: &mut wasm_encoder::NameSection,
        section: Name<'_>,
    ) -> Result<(), reencode::Error<DeadReference>> {
        match section {
            // A dropped function loses its name with it.
            Name::Function(map) => {
                let mut kept = wasm_encoder::NameMap::new();
                for naming in map {
                    let naming = naming?;
                    if let Some(idx) = self.renumbering.get(naming.index) {
                        kept.append(idx, naming.name);
                    }
                }
                names.functions(&kept);
                Ok(())
            }
            other => reencode::utils::parse_custom_name_subsection(self, names, other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn module(wat: &str) -> Vec<u8> {
        wat::parse_str(wat).expect("test module parses")
    }

    fn exported_names(bytes: &[u8]) -> Vec<String> {
        let mut out = Vec::new();
        for payload in Parser::new(0).parse_all(bytes) {
            if let Payload::ExportSection(section) = payload.unwrap() {
                for export in section {
                    out.push(export.unwrap().name.to_string());
                }
            }
        }
        out
    }

    fn defined_count(bytes: &[u8]) -> usize {
        Parser::new(0)
            .parse_all(bytes)
            .filter(|p| matches!(p, Ok(Payload::CodeSectionEntry(_))))
            .count()
    }

    #[test]
    fn a_helper_called_only_by_a_dead_function_is_dropped() {
        let bytes = module(
            r#"(module
                (import "h" "i" (func $imp))
                (func $start (export "_start") call $used)
                (func $used)
                (func $dead call $helper)
                (func $helper))"#,
        );
        let (out, renumbering) = drop_dead_functions(bytes, &[]).unwrap();
        wasmparser::validate(&out).unwrap();
        assert_eq!(defined_count(&out), 2);
        assert_eq!(renumbering.get(0), Some(0), "imports keep their index");
        assert_eq!(renumbering.get(1), Some(1));
        assert_eq!(renumbering.get(2), Some(2));
        assert_eq!(renumbering.get(3), None);
        assert_eq!(renumbering.get(4), None);
        assert_eq!(renumbering.dropped(), 2);
    }

    #[test]
    fn a_live_function_after_a_dead_one_is_renumbered_in_calls_and_exports() {
        let bytes = module(
            r#"(module
                (func $dead)
                (func $a (export "a") (result i32) call $b)
                (func $b (result i32) i32.const 7))"#,
        );
        let (out, renumbering) = drop_dead_functions(bytes, &[]).unwrap();
        wasmparser::validate(&out).unwrap();
        assert_eq!(renumbering.get(1), Some(0));
        assert_eq!(renumbering.get(2), Some(1));
        let graph = CallGraph::read(&out).unwrap();
        assert_eq!(graph.roots, [0], "the export follows `a` to index 0");
        assert_eq!(graph.edges, [vec![1], vec![]], "`a` calls `b` at index 1");
    }

    #[test]
    fn a_function_taken_by_ref_func_or_an_element_survives() {
        let bytes = module(
            r#"(module
                (type $t (func))
                (table 1 funcref)
                (elem (i32.const 0) $in_table)
                (elem declare func $by_ref)
                (func $start (export "_start") ref.func $by_ref drop)
                (func $in_table)
                (func $by_ref call $callee)
                (func $callee)
                (func $dead))"#,
        );
        let (out, renumbering) = drop_dead_functions(bytes, &[]).unwrap();
        wasmparser::validate(&out).unwrap();
        assert_eq!(defined_count(&out), 4);
        assert_eq!(renumbering.get(4), None);
    }

    #[test]
    fn an_exported_function_nothing_calls_survives() {
        let bytes = module(
            r#"(module
                (func $start (export "_start"))
                (func $exposed (export "exposed") call $helper)
                (func $helper)
                (func $dead))"#,
        );
        let (out, _) = drop_dead_functions(bytes, &[]).unwrap();
        wasmparser::validate(&out).unwrap();
        assert_eq!(defined_count(&out), 3);
        assert_eq!(exported_names(&out), ["_start", "exposed"]);
    }

    #[test]
    fn a_kept_function_survives_with_its_callees() {
        let bytes = module(
            r#"(module
                (func $start (export "_start"))
                (func $kept call $callee)
                (func $callee)
                (func $dead))"#,
        );
        let (out, renumbering) = drop_dead_functions(bytes, &[1]).unwrap();
        wasmparser::validate(&out).unwrap();
        assert_eq!(defined_count(&out), 3);
        assert_eq!(renumbering.get(3), None);
    }

    #[test]
    fn a_module_with_nothing_dead_comes_back_byte_for_byte() {
        let bytes = module(
            r#"(module
                (func $start (export "_start") call $a)
                (func $a))"#,
        );
        let (out, renumbering) = drop_dead_functions(bytes.clone(), &[]).unwrap();
        assert_eq!(out, bytes);
        assert_eq!(renumbering.dropped(), 0);
    }

    #[test]
    fn a_dropped_function_loses_its_name() {
        let bytes = module(
            r#"(module
                (func $dead)
                (func $kept (export "k")))"#,
        );
        let (out, _) = drop_dead_functions(bytes, &[]).unwrap();
        let text = wasmprinter::print_bytes(&out).unwrap();
        assert!(text.contains("$kept"), "{text}");
        assert!(!text.contains("$dead"), "{text}");
    }
}
