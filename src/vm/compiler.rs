use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Pattern, Stmt, StrPart, TopLevel, TypeDef};
use crate::nan_value::{Arena, NanValue};
use crate::source::find_module_file;

use super::opcode::*;
use super::types::{CodeStore, FnChunk};

fn encode_vm_fn_ref(fn_id: u32) -> NanValue {
    // VM callables are encoded as inline Int function ids.
    NanValue::new_int_inline(fn_id as i64)
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Compile a parsed + TCO-transformed + resolved program into bytecode.
/// Also loads dependent modules if a `module` declaration with `depends` is present.
pub fn compile_program(
    items: &[TopLevel],
    arena: &mut Arena,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    compile_program_with_modules(items, arena, None)
}

/// Compile with explicit module root for `depends` resolution.
pub fn compile_program_with_modules(
    items: &[TopLevel],
    arena: &mut Arena,
    module_root: Option<&str>,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    let mut compiler = ProgramCompiler::new();

    // Load dependent modules first.
    if let Some(module_root) = module_root {
        compiler.load_modules(items, module_root, arena)?;
    }

    // First pass: register type definitions and reserve fn_ids for all functions.
    // Reserving fn_ids up front allows mutual tail-call resolution.
    for item in items {
        match item {
            TopLevel::FnDef(fndef) => {
                compiler.ensure_global(&fndef.name);
                // Reserve a slot in CodeStore with a placeholder chunk.
                let fn_id = compiler.code.add_function(FnChunk {
                    name: fndef.name.clone(),
                    arity: fndef.params.len() as u8,
                    local_count: 0,
                    code: Vec::new(),
                    constants: Vec::new(),
                    effects: fndef.effects.clone(),
                });
                let global_idx = compiler.global_names[&fndef.name];
                compiler.globals[global_idx as usize] = encode_vm_fn_ref(fn_id);
            }
            TopLevel::TypeDef(td) => {
                compiler.register_type_def(td, arena);
            }
            _ => {}
        }
    }

    // Second pass: compile functions (replacing placeholder chunks).
    for item in items {
        if let TopLevel::FnDef(fndef) = item {
            let fn_id = compiler.code.find(&fndef.name).unwrap();
            let chunk = compiler.compile_fn(fndef, arena)?;
            compiler.code.functions[fn_id as usize] = chunk;
        }
    }

    // Compile top-level statements.
    compiler.compile_top_level(items, arena)?;

    Ok((compiler.code, compiler.globals))
}

#[derive(Debug)]
pub struct CompileError {
    pub msg: String,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Compile error: {}", self.msg)
    }
}

// ---------------------------------------------------------------------------
// Program-level compiler
// ---------------------------------------------------------------------------

struct ProgramCompiler {
    code: CodeStore,
    globals: Vec<NanValue>,
    global_names: HashMap<String, u16>,
}

impl ProgramCompiler {
    fn new() -> Self {
        ProgramCompiler {
            code: CodeStore::new(),
            globals: Vec::new(),
            global_names: HashMap::new(),
        }
    }

    /// Load all modules from `depends [...]` declarations.
    /// Compiles each module's functions into the shared CodeStore and builds
    /// namespace globals so that `Data.Fibonacci.buildFibStats` resolves to
    /// a CALL_KNOWN with the correct fn_id.
    fn load_modules(
        &mut self,
        items: &[TopLevel],
        module_root: &str,
        arena: &mut Arena,
    ) -> Result<(), CompileError> {
        let module = items.iter().find_map(|i| {
            if let TopLevel::Module(m) = i {
                Some(m)
            } else {
                None
            }
        });
        let module = match module {
            Some(m) => m,
            None => return Ok(()), // no module declaration → no deps
        };

        let mut loaded = HashSet::new();
        for dep_name in &module.depends {
            self.load_module_recursive(dep_name, module_root, arena, &mut loaded)?;
        }
        Ok(())
    }

    fn load_module_recursive(
        &mut self,
        dep_name: &str,
        module_root: &str,
        arena: &mut Arena,
        loaded: &mut HashSet<String>,
    ) -> Result<(), CompileError> {
        if loaded.contains(dep_name) {
            return Ok(());
        }
        loaded.insert(dep_name.to_string());

        let file_path = find_module_file(dep_name, module_root).ok_or_else(|| CompileError {
            msg: format!("module '{}' not found (root: {})", dep_name, module_root),
        })?;

        let source = std::fs::read_to_string(&file_path).map_err(|e| CompileError {
            msg: format!("cannot read module '{}': {}", dep_name, e),
        })?;

        let mut mod_items = crate::source::parse_source(&source).map_err(|e| CompileError {
            msg: format!("parse error in module '{}': {}", dep_name, e),
        })?;

        crate::tco::transform_program(&mut mod_items);
        crate::resolver::resolve_program(&mut mod_items);

        // Recursively load this module's dependencies.
        if let Some(sub_module) = mod_items.iter().find_map(|i| {
            if let TopLevel::Module(m) = i {
                Some(m)
            } else {
                None
            }
        }) {
            for sub_dep in &sub_module.depends {
                self.load_module_recursive(sub_dep, module_root, arena, loaded)?;
            }
        }

        // Register module's type definitions.
        for item in &mod_items {
            if let TopLevel::TypeDef(td) = item {
                self.register_type_def(td, arena);
            }
        }

        // Determine what to expose.
        let exposes: Option<Vec<String>> = mod_items.iter().find_map(|i| {
            if let TopLevel::Module(m) = i {
                if m.exposes.is_empty() {
                    None
                } else {
                    Some(m.exposes.clone())
                }
            } else {
                None
            }
        });

        // Compile module functions into the shared CodeStore.
        // Reserve fn_ids first.
        let mut module_fn_ids: Vec<(String, u32)> = Vec::new();
        for item in &mod_items {
            if let TopLevel::FnDef(fndef) = item {
                let fn_id = self.code.add_function(FnChunk {
                    name: format!("{}.{}", dep_name, fndef.name),
                    arity: fndef.params.len() as u8,
                    local_count: 0,
                    code: Vec::new(),
                    constants: Vec::new(),
                    effects: fndef.effects.clone(),
                });
                module_fn_ids.push((fndef.name.clone(), fn_id));
            }
        }

        // Build module scope: simple_name → fn_id for intra-module resolution.
        let module_scope: HashMap<String, u32> = module_fn_ids.iter().cloned().collect();

        // Compile functions using module_scope for intra-module call resolution.
        let mut fn_idx = 0;
        for item in &mod_items {
            if let TopLevel::FnDef(fndef) = item {
                let (_, fn_id) = module_fn_ids[fn_idx];
                let chunk = self.compile_fn_with_scope(fndef, arena, &module_scope)?;
                self.code.functions[fn_id as usize] = chunk;
                fn_idx += 1;
            }
        }

        // Build namespace: make exposed fns accessible under the dep path.
        // For "Data.Fibonacci" with exposed fn "buildFibStats" → global
        // "Data.Fibonacci.buildFibStats" = fn_id.
        // Also: register path segments as namespace globals.
        for (fn_name, fn_id) in &module_fn_ids {
            let exposed = match &exposes {
                Some(list) => list.iter().any(|e| e == fn_name),
                None => !fn_name.starts_with('_'),
            };
            if exposed {
                let qualified = format!("{}.{}", dep_name, fn_name);
                let global_idx = self.ensure_global(&qualified);
                self.globals[global_idx as usize] = encode_vm_fn_ref(*fn_id);
            }
        }

        // Register type namespace globals for exposed types.
        // E.g., if module exposes type Shape, register "Data.Fibonacci.Shape"
        // as a namespace so Shape.Circle can be accessed.
        for item in &mod_items {
            if let TopLevel::TypeDef(td) = item {
                let type_name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                };
                let exposed = match &exposes {
                    Some(list) => list.iter().any(|e| e == type_name),
                    None => !type_name.starts_with('_'),
                };
                if exposed {
                    // Make the type accessible under its qualified path.
                    // For now, just ensure the type is findable by its simple name
                    // (it's already registered in the arena type registry).
                }
            }
        }

        Ok(())
    }

    fn ensure_global(&mut self, name: &str) -> u16 {
        if let Some(&idx) = self.global_names.get(name) {
            return idx;
        }
        let idx = self.globals.len() as u16;
        self.global_names.insert(name.to_string(), idx);
        self.globals.push(NanValue::UNIT);
        idx
    }

    fn register_type_def(&mut self, td: &TypeDef, arena: &mut Arena) {
        match td {
            TypeDef::Product { name, fields, .. } => {
                let field_names: Vec<String> = fields.iter().map(|(n, _)| n.clone()).collect();
                arena.register_record_type(name, field_names);
            }
            TypeDef::Sum { name, variants, .. } => {
                let variant_names: Vec<String> = variants.iter().map(|v| v.name.clone()).collect();
                arena.register_sum_type(name, variant_names);
            }
        }
    }

    fn compile_fn(&mut self, fndef: &FnDef, arena: &mut Arena) -> Result<FnChunk, CompileError> {
        let empty_scope = HashMap::new();
        self.compile_fn_with_scope(fndef, arena, &empty_scope)
    }

    fn compile_fn_with_scope(
        &mut self,
        fndef: &FnDef,
        arena: &mut Arena,
        module_scope: &HashMap<String, u32>,
    ) -> Result<FnChunk, CompileError> {
        let resolution = fndef.resolution.as_ref();
        let local_count = resolution.map_or(fndef.params.len() as u16, |r| r.local_count);
        let local_slots: HashMap<String, u16> = resolution
            .map(|r| r.local_slots.as_ref().clone())
            .unwrap_or_else(|| {
                fndef
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, (name, _))| (name.clone(), i as u16))
                    .collect()
            });

        let mut fc = FnCompiler::new(
            &fndef.name,
            fndef.params.len() as u8,
            local_count,
            fndef.effects.clone(),
            local_slots,
            &self.global_names,
            module_scope,
            &self.code,
            arena,
        );

        match fndef.body.as_ref() {
            FnBody::Block(stmts) => fc.compile_body(stmts)?,
        }

        Ok(fc.finish())
    }

    fn compile_top_level(
        &mut self,
        items: &[TopLevel],
        arena: &mut Arena,
    ) -> Result<(), CompileError> {
        let has_stmts = items.iter().any(|i| matches!(i, TopLevel::Stmt(_)));
        if !has_stmts {
            return Ok(());
        }

        // Register any global bindings first.
        for item in items {
            if let TopLevel::Stmt(Stmt::Binding(name, _, _)) = item {
                self.ensure_global(name);
            }
        }

        let empty_mod_scope = HashMap::new();
        let mut fc = FnCompiler::new(
            "__top_level__",
            0,
            0,
            Vec::new(),
            HashMap::new(),
            &self.global_names,
            &empty_mod_scope,
            &self.code,
            arena,
        );

        for item in items {
            if let TopLevel::Stmt(stmt) = item {
                match stmt {
                    Stmt::Binding(name, _type_ann, expr) => {
                        fc.compile_expr(expr)?;
                        let idx = self.global_names[name.as_str()];
                        fc.emit_op(STORE_GLOBAL);
                        fc.emit_u16(idx);
                    }
                    Stmt::Expr(expr) => {
                        fc.compile_expr(expr)?;
                        fc.emit_op(POP);
                    }
                }
            }
        }

        fc.emit_op(LOAD_UNIT);
        fc.emit_op(RETURN);

        let chunk = fc.finish();
        self.code.add_function(chunk);
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Dotted path resolution
// ---------------------------------------------------------------------------

/// What a function expression resolves to at compile time.
enum CallTarget {
    /// Known function id (local or qualified module function).
    KnownFn(u32),
    /// Result.Ok / Result.Err / Option.Some → WRAP opcode. kind: 0=Ok, 1=Err, 2=Some.
    Wrapper(u8),
    /// Option.None → load constant.
    None_,
    /// User-defined variant constructor: Shape.Circle → VARIANT_NEW.
    Variant(u32, u16),
    /// Builtin service: Console.print, List.len → CALL_BUILTIN.
    Builtin(String),
}

// ---------------------------------------------------------------------------
// Function-level compiler
// ---------------------------------------------------------------------------

struct FnCompiler<'a> {
    name: String,
    arity: u8,
    local_count: u16,
    effects: Vec<String>,
    local_slots: HashMap<String, u16>,
    global_names: &'a HashMap<String, u16>,
    /// Module-local function scope: simple_name → fn_id.
    /// Used for intra-module calls (e.g. `placeStairs` inside map.av).
    module_scope: &'a HashMap<String, u32>,
    code_store: &'a CodeStore,
    arena: &'a mut Arena,
    code: Vec<u8>,
    constants: Vec<NanValue>,
}

impl<'a> FnCompiler<'a> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        name: &str,
        arity: u8,
        local_count: u16,
        effects: Vec<String>,
        local_slots: HashMap<String, u16>,
        global_names: &'a HashMap<String, u16>,
        module_scope: &'a HashMap<String, u32>,
        code_store: &'a CodeStore,
        arena: &'a mut Arena,
    ) -> Self {
        FnCompiler {
            name: name.to_string(),
            arity,
            local_count,
            effects,
            local_slots,
            global_names,
            module_scope,
            code_store,
            arena,
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    fn finish(self) -> FnChunk {
        FnChunk {
            name: self.name,
            arity: self.arity,
            local_count: self.local_count,
            code: self.code,
            constants: self.constants,
            effects: self.effects,
        }
    }

    // -- Emit helpers --------------------------------------------------------

    fn emit_op(&mut self, op: u8) {
        self.code.push(op);
    }

    fn emit_u8(&mut self, val: u8) {
        self.code.push(val);
    }

    fn emit_u16(&mut self, val: u16) {
        self.code.push((val >> 8) as u8);
        self.code.push((val & 0xFF) as u8);
    }

    fn emit_i16(&mut self, val: i16) {
        self.emit_u16(val as u16);
    }

    fn add_constant(&mut self, val: NanValue) -> u16 {
        for (i, c) in self.constants.iter().enumerate() {
            if c.bits() == val.bits() {
                return i as u16;
            }
        }
        let idx = self.constants.len() as u16;
        self.constants.push(val);
        idx
    }

    fn offset(&self) -> usize {
        self.code.len()
    }

    fn emit_jump(&mut self, op: u8) -> usize {
        self.emit_op(op);
        let patch_pos = self.code.len();
        self.emit_i16(0);
        patch_pos
    }

    fn patch_jump(&mut self, patch_pos: usize) {
        let target = self.code.len();
        let offset = (target as isize - patch_pos as isize - 2) as i16;
        let bytes = (offset as u16).to_be_bytes();
        self.code[patch_pos] = bytes[0];
        self.code[patch_pos + 1] = bytes[1];
    }

    fn patch_jump_to(&mut self, patch_pos: usize, target: usize) {
        let offset = (target as isize - patch_pos as isize - 2) as i16;
        let bytes = (offset as u16).to_be_bytes();
        self.code[patch_pos] = bytes[0];
        self.code[patch_pos + 1] = bytes[1];
    }

    fn bind_top_to_local(&mut self, name: &str) {
        if let Some(&slot) = self.local_slots.get(name) {
            self.emit_op(STORE_LOCAL);
            self.emit_u8(slot as u8);
        } else {
            self.emit_op(POP);
        }
    }

    fn dup_and_bind_top_to_local(&mut self, name: &str) {
        self.emit_op(DUP);
        self.bind_top_to_local(name);
    }

    fn compile_unwrap_pattern(&mut self, kind: u8, binding: Option<&String>) -> Vec<usize> {
        self.emit_op(MATCH_UNWRAP);
        self.emit_u8(kind);
        let fail_patch = self.code.len();
        self.emit_i16(0);
        if let Some(binding) = binding {
            self.dup_and_bind_top_to_local(binding);
        }
        vec![fail_patch]
    }

    fn compile_extracted_subpattern<F>(
        &mut self,
        emit_subject: F,
        pattern: &Pattern,
    ) -> Result<Vec<usize>, CompileError>
    where
        F: FnOnce(&mut Self),
    {
        emit_subject(self);
        let inner_fail_patches = self.compile_pattern(pattern)?;
        self.emit_op(POP);

        if inner_fail_patches.is_empty() {
            return Ok(Vec::new());
        }

        let success_skip_cleanup = self.emit_jump(JUMP);
        let cleanup_target = self.offset();
        for patch in inner_fail_patches {
            self.patch_jump_to(patch, cleanup_target);
        }
        self.emit_op(POP);
        let outer_fail = self.emit_jump(JUMP);
        self.patch_jump(success_skip_cleanup);
        Ok(vec![outer_fail])
    }

    fn compile_tuple_pattern(&mut self, patterns: &[Pattern]) -> Result<Vec<usize>, CompileError> {
        self.emit_op(MATCH_TUPLE);
        self.emit_u8(patterns.len() as u8);
        let tuple_fail = self.code.len();
        self.emit_i16(0);

        let mut fail_patches = vec![tuple_fail];
        for (i, pattern) in patterns.iter().enumerate() {
            let mut nested = self.compile_extracted_subpattern(
                |this| {
                    this.emit_op(EXTRACT_TUPLE_ITEM);
                    this.emit_u8(i as u8);
                },
                pattern,
            )?;
            fail_patches.append(&mut nested);
        }
        Ok(fail_patches)
    }

    // -- Body compilation ----------------------------------------------------

    fn compile_body(&mut self, stmts: &[Stmt]) -> Result<(), CompileError> {
        if stmts.is_empty() {
            self.emit_op(LOAD_UNIT);
            self.emit_op(RETURN);
            return Ok(());
        }

        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            match stmt {
                Stmt::Binding(name, _type_ann, expr) => {
                    self.compile_expr(expr)?;
                    if let Some(&slot) = self.local_slots.get(name) {
                        self.emit_op(STORE_LOCAL);
                        self.emit_u8(slot as u8);
                    }
                }
                Stmt::Expr(expr) => {
                    self.compile_expr(expr)?;
                    if !is_last {
                        self.emit_op(POP);
                    }
                }
            }
        }

        self.emit_op(RETURN);
        Ok(())
    }

    // -- Expression compilation ----------------------------------------------

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Literal(lit) => self.compile_literal(lit),
            Expr::Resolved(slot) => {
                self.emit_op(LOAD_LOCAL);
                self.emit_u8(*slot as u8);
                Ok(())
            }
            Expr::Ident(name) => self.compile_ident(name),
            Expr::BinOp(op, left, right) => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;
                self.emit_binop(*op);
                Ok(())
            }
            Expr::FnCall(fn_expr, args) => self.compile_call(fn_expr, args),
            Expr::TailCall(boxed) => {
                let (target, args) = boxed.as_ref();
                self.compile_tail_call(target, args)
            }
            Expr::Match {
                subject,
                arms,
                line,
            } => self.compile_match(subject, arms, *line),
            Expr::Constructor(name, arg) => self.compile_constructor(name, arg.as_deref()),
            Expr::ErrorProp(inner) => self.compile_error_prop(inner),
            Expr::List(items) => self.compile_list(items),
            Expr::InterpolatedStr(parts) => self.compile_interpolated_str(parts),
            Expr::RecordCreate { type_name, fields } => {
                self.compile_record_create(type_name, fields)
            }
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => self.compile_record_update(type_name, base, updates),
            Expr::Attr(obj, field) => self.compile_attr(obj, field),
            Expr::Tuple(items) => self.compile_tuple(items),
            Expr::MapLiteral(entries) => self.compile_map(entries),
        }
    }

    fn compile_literal(&mut self, lit: &Literal) -> Result<(), CompileError> {
        match lit {
            Literal::Int(i) => {
                let nv = NanValue::new_int(*i, self.arena);
                let idx = self.add_constant(nv);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            Literal::Float(f) => {
                let nv = NanValue::new_float(*f);
                let idx = self.add_constant(nv);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            Literal::Bool(true) => self.emit_op(LOAD_TRUE),
            Literal::Bool(false) => self.emit_op(LOAD_FALSE),
            Literal::Unit => self.emit_op(LOAD_UNIT),
            Literal::Str(s) => {
                let arena_idx = self.arena.push_string(s);
                let nv = NanValue::new_string(arena_idx);
                let idx = self.add_constant(nv);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
        }
        Ok(())
    }

    fn compile_ident(&mut self, name: &str) -> Result<(), CompileError> {
        if let Some(&slot) = self.local_slots.get(name) {
            self.emit_op(LOAD_LOCAL);
            self.emit_u8(slot as u8);
        } else if let Some(&idx) = self.global_names.get(name) {
            self.emit_op(LOAD_GLOBAL);
            self.emit_u16(idx);
        } else {
            // Namespace names like Result, Option, Console, etc. are resolved
            // at call site, not as standalone values.
            return Err(CompileError {
                msg: format!("undefined variable: {}", name),
            });
        }
        Ok(())
    }

    fn emit_binop(&mut self, op: BinOp) {
        match op {
            BinOp::Add => self.emit_op(ADD),
            BinOp::Sub => self.emit_op(SUB),
            BinOp::Mul => self.emit_op(MUL),
            BinOp::Div => self.emit_op(DIV),
            BinOp::Eq => self.emit_op(EQ),
            BinOp::Lt => self.emit_op(LT),
            BinOp::Gt => self.emit_op(GT),
            BinOp::Neq => {
                self.emit_op(EQ);
                self.emit_op(NOT);
            }
            BinOp::Lte => {
                self.emit_op(GT);
                self.emit_op(NOT);
            }
            BinOp::Gte => {
                self.emit_op(LT);
                self.emit_op(NOT);
            }
        }
    }

    // -- Dotted path resolution ------------------------------------------------

    /// Resolve a dotted path (Ns, member) to what it means.
    fn resolve_dotted_call(&self, ns: &str, method: &str) -> CallTarget {
        match (ns, method) {
            ("Result", "Ok") => return CallTarget::Wrapper(0),
            ("Result", "Err") => return CallTarget::Wrapper(1),
            ("Option", "Some") => return CallTarget::Wrapper(2),
            ("Option", "None") => return CallTarget::None_,
            _ => {}
        }
        // User-defined variant?
        if let Some(type_id) = self.arena.find_type_id(ns)
            && let Some(variant_id) = self.arena.find_variant_id(type_id, method)
        {
            return CallTarget::Variant(type_id, variant_id);
        }
        // Module function?
        let qualified = format!("{}.{}", ns, method);
        if let Some(fn_id) = self.code_store.find(&qualified) {
            return CallTarget::KnownFn(fn_id);
        }
        // Builtin service.
        CallTarget::Builtin(qualified)
    }

    /// Extract (namespace_path, method) from dotted Attr expressions.
    fn extract_dotted_path(&self, expr: &Expr) -> Option<(String, String)> {
        if let Expr::Attr(obj, method) = expr {
            let path = self.flatten_path(obj)?;
            if path.chars().next().is_some_and(|c| c.is_uppercase()) {
                return Some((path, method.clone()));
            }
        }
        Option::None
    }

    /// Flatten Attr(Attr(Ident("A"), "B"), "C") → "A.B.C".
    fn flatten_path(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Ident(name) => Some(name.clone()),
            Expr::Attr(inner, field) => Some(format!("{}.{}", self.flatten_path(inner)?, field)),
            _ => Option::None,
        }
    }

    /// Resolve a function name to fn_id (module_scope first, then code_store).
    fn resolve_fn_id(&self, name: &str) -> Option<u32> {
        self.module_scope
            .get(name)
            .copied()
            .or_else(|| self.code_store.find(name))
    }

    fn resolve_call_target(&self, expr: &Expr) -> Option<CallTarget> {
        match expr {
            Expr::Ident(name) => self.resolve_fn_id(name).map(CallTarget::KnownFn),
            _ => self
                .extract_dotted_path(expr)
                .map(|(ns, method)| self.resolve_dotted_call(&ns, &method)),
        }
    }

    // -- Call compilation ----------------------------------------------------

    fn compile_call(&mut self, fn_expr: &Expr, args: &[Expr]) -> Result<(), CompileError> {
        if let Some(target) = self.resolve_call_target(fn_expr) {
            return self.compile_resolved_call(target, args);
        }
        // Dynamic call (HOF).
        self.compile_expr(fn_expr)?;
        for arg in args {
            self.compile_expr(arg)?;
        }
        self.emit_op(CALL_VALUE);
        self.emit_u8(args.len() as u8);
        Ok(())
    }

    fn compile_resolved_call(
        &mut self,
        target: CallTarget,
        args: &[Expr],
    ) -> Result<(), CompileError> {
        match target {
            CallTarget::KnownFn(fn_id) => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                self.emit_op(CALL_KNOWN);
                self.emit_u16(fn_id as u16);
                self.emit_u8(args.len() as u8);
            }
            CallTarget::Wrapper(kind) => {
                if let Some(arg) = args.first() {
                    self.compile_expr(arg)?;
                } else {
                    self.emit_op(LOAD_UNIT);
                }
                self.emit_op(WRAP);
                self.emit_u8(kind);
            }
            CallTarget::None_ => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            CallTarget::Variant(type_id, variant_id) => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                self.emit_op(VARIANT_NEW);
                self.emit_u16(type_id as u16);
                self.emit_u16(variant_id);
                self.emit_u8(args.len() as u8);
            }
            CallTarget::Builtin(qualified) => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                self.emit_op(CALL_BUILTIN);
                let name_idx = self.arena.push_string(&qualified);
                self.emit_u16(name_idx as u16);
                self.emit_u8(args.len() as u8);
            }
        }
        Ok(())
    }

    fn compile_tail_call(&mut self, target: &str, args: &[Expr]) -> Result<(), CompileError> {
        for arg in args {
            self.compile_expr(arg)?;
        }

        if target == self.name {
            self.emit_op(TAIL_CALL_SELF);
            self.emit_u8(args.len() as u8);
        } else if let Some(fn_id) = self.resolve_fn_id(target) {
            self.emit_op(TAIL_CALL_KNOWN);
            self.emit_u16(fn_id as u16);
            self.emit_u8(args.len() as u8);
        } else {
            return Err(CompileError {
                msg: format!("unknown tail call target: {}", target),
            });
        }
        Ok(())
    }

    // -- Match compilation ---------------------------------------------------

    fn compile_match(
        &mut self,
        subject: &Expr,
        arms: &[crate::ast::MatchArm],
        line: usize,
    ) -> Result<(), CompileError> {
        self.compile_expr(subject)?;

        let mut end_jumps = Vec::new();
        let mut last_arm_fail_patches = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;

            let fail_patches = match &arm.pattern {
                Pattern::Wildcard => Vec::new(),
                Pattern::Ident(name) => {
                    self.emit_op(DUP);
                    if let Some(&slot) = self.local_slots.get(name) {
                        self.emit_op(STORE_LOCAL);
                        self.emit_u8(slot as u8);
                    } else {
                        self.emit_op(POP);
                    }
                    Vec::new()
                }
                pat => self.compile_pattern(pat)?,
            };

            // Pattern matched — pop subject, compile body.
            self.emit_op(POP);
            self.compile_expr(&arm.body)?;

            if is_last {
                // Save last arm's fail patches — they'll be patched to MATCH_FAIL.
                last_arm_fail_patches = fail_patches;
            } else {
                end_jumps.push(self.emit_jump(JUMP));
                // Patch fail jumps to here (start of next arm).
                let here = self.offset();
                for patch in fail_patches {
                    self.patch_jump_to(patch, here);
                }
            }
        }

        // If last arm was refutable, emit MATCH_FAIL after all arms.
        let last_refutable = arms
            .last()
            .is_none_or(|a| !matches!(a.pattern, Pattern::Wildcard | Pattern::Ident(_)));
        if last_refutable && !last_arm_fail_patches.is_empty() {
            // Last arm body succeeded — jump over MATCH_FAIL.
            end_jumps.push(self.emit_jump(JUMP));
            // Fail target: POP subject + MATCH_FAIL.
            let fail_target = self.offset();
            for patch in last_arm_fail_patches {
                self.patch_jump_to(patch, fail_target);
            }
            self.emit_op(POP);
            self.emit_op(MATCH_FAIL);
            self.emit_u16(line as u16);
        }

        // Patch end jumps.
        for patch in end_jumps {
            self.patch_jump(patch);
        }

        Ok(())
    }

    /// Compile a pattern. Subject is on top of stack (peeked, not consumed).
    /// Returns a Vec of fail-jump patch positions.
    fn compile_pattern(&mut self, pattern: &Pattern) -> Result<Vec<usize>, CompileError> {
        match pattern {
            Pattern::Wildcard => Ok(Vec::new()),
            Pattern::Ident(name) => {
                self.dup_and_bind_top_to_local(name);
                Ok(Vec::new())
            }
            Pattern::Literal(lit) => {
                self.emit_op(DUP);
                self.compile_literal(lit)?;
                self.emit_op(EQ);
                let patch = self.emit_jump(JUMP_IF_FALSE);
                Ok(vec![patch])
            }
            Pattern::EmptyList => {
                self.emit_op(MATCH_NIL);
                let patch = self.code.len();
                self.emit_i16(0);
                Ok(vec![patch])
            }
            Pattern::Cons(head, tail) => {
                self.emit_op(MATCH_CONS);
                let fail_patch = self.code.len();
                self.emit_i16(0);

                // Destructure: DUP subject, LIST_HEAD_TAIL → pushes tail then head.
                self.emit_op(DUP);
                self.emit_op(LIST_HEAD_TAIL);
                // Stack: [..., subject, head, tail]
                // Wait — LIST_HEAD_TAIL pops list, pushes tail then head.
                // After DUP: [..., subject, subject_copy]
                // LIST_HEAD_TAIL pops subject_copy, pushes tail, then head.
                // Stack: [..., subject, tail, head]
                self.bind_top_to_local(head);
                self.bind_top_to_local(tail);

                Ok(vec![fail_patch])
            }
            Pattern::Constructor(name, bindings) => {
                self.compile_constructor_pattern(name, bindings)
            }
            Pattern::Tuple(patterns) => self.compile_tuple_pattern(patterns),
        }
    }

    fn compile_constructor_pattern(
        &mut self,
        name: &str,
        bindings: &[String],
    ) -> Result<Vec<usize>, CompileError> {
        match name {
            "Result.Ok" => Ok(self.compile_unwrap_pattern(0, bindings.first())),
            "Result.Err" => Ok(self.compile_unwrap_pattern(1, bindings.first())),
            "Option.Some" => Ok(self.compile_unwrap_pattern(2, bindings.first())),
            "Option.None" => {
                self.emit_op(DUP);
                let none_const = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(none_const);
                self.emit_op(EQ);
                let fail_patch = self.emit_jump(JUMP_IF_FALSE);
                Ok(vec![fail_patch])
            }
            _ => {
                // User-defined variant: Type.Variant(fields...)
                let mut patches = Vec::new();

                // Check TAG_VARIANT.
                self.emit_op(MATCH_TAG);
                self.emit_u8(8); // TAG_VARIANT = 8
                let tag_fail = self.code.len();
                self.emit_i16(0);
                patches.push(tag_fail);

                if let Some(dot_pos) = name.find('.') {
                    let type_name = &name[..dot_pos];
                    let variant_name = &name[dot_pos + 1..];
                    if let Some(type_id) = self.arena.find_type_id(type_name)
                        && let Some(variant_id) = self.arena.find_variant_id(type_id, variant_name)
                    {
                        self.emit_op(MATCH_VARIANT);
                        self.emit_u16(variant_id);
                        let variant_fail = self.code.len();
                        self.emit_i16(0);
                        patches.push(variant_fail);

                        for (i, b) in bindings.iter().enumerate() {
                            self.emit_op(EXTRACT_FIELD);
                            self.emit_u8(i as u8);
                            self.bind_top_to_local(b);
                        }

                        return Ok(patches);
                    }
                }

                Err(CompileError {
                    msg: format!("unknown constructor pattern: {}", name),
                })
            }
        }
    }

    // -- Constructor compilation ---------------------------------------------

    fn compile_constructor(&mut self, name: &str, arg: Option<&Expr>) -> Result<(), CompileError> {
        match name {
            "Result.Ok" => {
                self.compile_constructor_arg(arg)?;
                self.emit_op(WRAP);
                self.emit_u8(0);
            }
            "Result.Err" => {
                self.compile_constructor_arg(arg)?;
                self.emit_op(WRAP);
                self.emit_u8(1);
            }
            "Option.Some" => {
                self.compile_constructor_arg(arg)?;
                self.emit_op(WRAP);
                self.emit_u8(2);
            }
            "Option.None" => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            _ => {
                if let Some(dot_pos) = name.find('.') {
                    let type_name = &name[..dot_pos];
                    let variant_name = &name[dot_pos + 1..];
                    if let Some(type_id) = self.arena.find_type_id(type_name)
                        && let Some(variant_id) = self.arena.find_variant_id(type_id, variant_name)
                    {
                        let field_count = if let Some(a) = arg {
                            self.compile_expr(a)?;
                            1u8
                        } else {
                            0u8
                        };
                        self.emit_op(VARIANT_NEW);
                        self.emit_u16(type_id as u16);
                        self.emit_u16(variant_id);
                        self.emit_u8(field_count);
                        return Ok(());
                    }
                }
                return Err(CompileError {
                    msg: format!("unknown constructor: {}", name),
                });
            }
        }
        Ok(())
    }

    fn compile_constructor_arg(&mut self, arg: Option<&Expr>) -> Result<(), CompileError> {
        if let Some(a) = arg {
            self.compile_expr(a)
        } else {
            self.emit_op(LOAD_UNIT);
            Ok(())
        }
    }

    fn compile_error_prop(&mut self, inner: &Expr) -> Result<(), CompileError> {
        self.compile_expr(inner)?;
        self.emit_op(DUP);
        // Check if Err.
        self.emit_op(MATCH_UNWRAP);
        self.emit_u8(1); // Err
        let is_err_patch = self.code.len();
        self.emit_i16(0);
        // Was Err, now unwrapped to error value. Re-wrap and return.
        self.emit_op(WRAP);
        self.emit_u8(1);
        self.emit_op(RETURN);
        // Not Err — patch jump to here.
        self.patch_jump(is_err_patch);
        // Top is still original value. Try unwrapping Ok.
        self.emit_op(MATCH_UNWRAP);
        self.emit_u8(0); // Ok
        let not_ok_patch = self.code.len();
        self.emit_i16(0);
        // Unwrapped Ok → inner value on top.
        let after = self.emit_jump(JUMP);
        // Not Ok — error.
        self.patch_jump(not_ok_patch);
        self.emit_op(MATCH_FAIL);
        self.emit_u16(0);
        self.patch_jump(after);
        Ok(())
    }

    // -- Collection compilation ----------------------------------------------

    fn compile_list(&mut self, items: &[Expr]) -> Result<(), CompileError> {
        if items.is_empty() {
            self.emit_op(LIST_NIL);
            return Ok(());
        }
        for item in items {
            self.compile_expr(item)?;
        }
        self.emit_op(LIST_NEW);
        self.emit_u8(items.len() as u8);
        Ok(())
    }

    fn compile_interpolated_str(&mut self, parts: &[StrPart]) -> Result<(), CompileError> {
        if parts.is_empty() {
            let idx = self.arena.push_string("");
            let nv = NanValue::new_string(idx);
            let cidx = self.add_constant(nv);
            self.emit_op(LOAD_CONST);
            self.emit_u16(cidx);
            return Ok(());
        }

        let mut first = true;
        for part in parts {
            match part {
                StrPart::Literal(s) => {
                    let idx = self.arena.push_string(s);
                    let nv = NanValue::new_string(idx);
                    let cidx = self.add_constant(nv);
                    self.emit_op(LOAD_CONST);
                    self.emit_u16(cidx);
                }
                StrPart::Parsed(expr) => {
                    self.compile_expr(expr)?;
                    // A standalone "{expr}" must still become a String.
                    // CONCAT stringifies non-strings via repr, so coercing with
                    // an empty suffix keeps both single-part and multi-part
                    // interpolations on the same path.
                    let empty_idx = self.arena.push_string("");
                    let empty_nv = NanValue::new_string(empty_idx);
                    let empty_const = self.add_constant(empty_nv);
                    self.emit_op(LOAD_CONST);
                    self.emit_u16(empty_const);
                    self.emit_op(CONCAT);
                }
            }
            if !first {
                self.emit_op(CONCAT);
            }
            first = false;
        }
        Ok(())
    }

    fn compile_record_create(
        &mut self,
        type_name: &str,
        fields: &[(String, Expr)],
    ) -> Result<(), CompileError> {
        let type_id = self
            .arena
            .find_type_id(type_name)
            .ok_or_else(|| CompileError {
                msg: format!("unknown record type: {}", type_name),
            })?;

        let field_names = self.arena.get_field_names(type_id).to_vec();
        for expected_name in &field_names {
            let (_, expr) = fields
                .iter()
                .find(|(n, _)| n == expected_name)
                .ok_or_else(|| CompileError {
                    msg: format!("missing field {} in record {}", expected_name, type_name),
                })?;
            self.compile_expr(expr)?;
        }

        self.emit_op(RECORD_NEW);
        self.emit_u16(type_id as u16);
        self.emit_u8(field_names.len() as u8);
        Ok(())
    }

    fn compile_record_update(
        &mut self,
        type_name: &str,
        base: &Expr,
        updates: &[(String, Expr)],
    ) -> Result<(), CompileError> {
        let type_id = self
            .arena
            .find_type_id(type_name)
            .ok_or_else(|| CompileError {
                msg: format!("unknown record type: {}", type_name),
            })?;
        let field_names = self.arena.get_field_names(type_id).to_vec();

        // For each field in canonical order: if updated, compile update expr;
        // otherwise compile base + RECORD_GET_NAMED to extract old value.
        // Re-compiles base for each non-updated field (wasteful but correct for v1).
        for fname in &field_names {
            if let Some((_, update_expr)) = updates.iter().find(|(n, _)| n == fname) {
                self.compile_expr(update_expr)?;
            } else {
                self.compile_expr(base)?;
                let name_idx = self.arena.push_string(fname);
                let nv = NanValue::new_string(name_idx);
                let const_idx = self.add_constant(nv);
                self.emit_op(RECORD_GET_NAMED);
                self.emit_u16(const_idx);
            }
        }

        self.emit_op(RECORD_NEW);
        self.emit_u16(type_id as u16);
        self.emit_u8(field_names.len() as u8);
        Ok(())
    }

    fn compile_attr(&mut self, obj: &Expr, field: &str) -> Result<(), CompileError> {
        // Dotted path on capitalized namespace: Option.None, Shape.Point, etc.
        if let Some(ns) = self.flatten_path(obj)
            && ns.chars().next().is_some_and(|c| c.is_uppercase())
        {
            let qualified = format!("{}.{}", ns, field);
            match self.resolve_dotted_call(&ns, field) {
                CallTarget::KnownFn(_) => {
                    if let Some(&idx) = self.global_names.get(&qualified) {
                        self.emit_op(LOAD_GLOBAL);
                        self.emit_u16(idx);
                        return Ok(());
                    }
                }
                CallTarget::None_ => {
                    let idx = self.add_constant(NanValue::NONE);
                    self.emit_op(LOAD_CONST);
                    self.emit_u16(idx);
                    return Ok(());
                }
                CallTarget::Variant(type_id, variant_id) => {
                    self.emit_op(VARIANT_NEW);
                    self.emit_u16(type_id as u16);
                    self.emit_u16(variant_id);
                    self.emit_u8(0); // zero-arg
                    return Ok(());
                }
                CallTarget::Builtin(_) => {
                    return Err(CompileError {
                        msg: format!(
                            "standalone builtin function values are not yet supported in VM: {}",
                            qualified
                        ),
                    });
                }
                _ => {
                    // Not a standalone constructor/value. Fall through to record field access.
                }
            }
        }

        // Record field access.
        self.compile_expr(obj)?;
        let name_idx = self.arena.push_string(field);
        let nv = NanValue::new_string(name_idx);
        let const_idx = self.add_constant(nv);
        self.emit_op(RECORD_GET_NAMED);
        self.emit_u16(const_idx);
        Ok(())
    }

    fn compile_tuple(&mut self, items: &[Expr]) -> Result<(), CompileError> {
        for item in items {
            self.compile_expr(item)?;
        }
        self.emit_op(TUPLE_NEW);
        self.emit_u8(items.len() as u8);
        Ok(())
    }

    fn compile_map(&mut self, entries: &[(Expr, Expr)]) -> Result<(), CompileError> {
        // Compile as: build empty map, then insert each key-value pair via Map.set builtin.
        // For v1, compile directly as arena map construction.
        // Push all key-value pairs, then use a dedicated sequence.
        // Simplest: compile each k/v, emit CALL_BUILTIN for Map.set iteratively.
        //
        // Actually, even simpler: build the map in the compiler if all entries
        // are constants... but that's rare. For v1, compile as a series of
        // Map.set calls starting from an empty map.

        // Start with empty map.
        // We need an opcode for empty map or a builtin call.
        // For pragmatism: allocate empty map in arena at compile time.
        let empty_map = self.arena.push_map(im::HashMap::new());
        let nv = NanValue::new_map(empty_map);
        let idx = self.add_constant(nv);
        self.emit_op(LOAD_CONST);
        self.emit_u16(idx);

        // For each entry: stack has [..., map], compile key, compile value,
        // then call Map.set(map, key, value) → new map.
        for (key, value) in entries {
            self.compile_expr(key)?;
            self.compile_expr(value)?;
            let name_idx = self.arena.push_string("Map.set");
            self.emit_op(CALL_BUILTIN);
            self.emit_u16(name_idx as u16);
            self.emit_u8(3); // Map.set(map, key, value)
        }
        Ok(())
    }
}
