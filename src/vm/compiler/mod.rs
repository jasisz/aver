mod calls;
mod classify;
mod expr;
mod patterns;

use std::collections::{HashMap, HashSet};

use crate::ast::{FnBody, FnDef, Stmt, TopLevel, TypeDef};
use crate::nan_value::{Arena, NanValue};
use crate::source::find_module_file;
use crate::types::{option, result};

use super::builtin::VmBuiltin;
use super::opcode::*;
use super::symbol::{VmSymbolTable, VmVariantCtor};
use super::types::{CodeStore, FnChunk};

/// Compile a parsed + TCO-transformed + resolved program into bytecode.
/// Also loads dependent modules if a `module` declaration with `depends` is present.
pub fn compile_program(
    items: &[TopLevel],
    arena: &mut Arena,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    compile_program_with_modules(items, arena, None, "")
}

/// Compile with explicit module root for `depends` resolution.
pub fn compile_program_with_modules(
    items: &[TopLevel],
    arena: &mut Arena,
    module_root: Option<&str>,
    source_file: &str,
) -> Result<(CodeStore, Vec<NanValue>), CompileError> {
    let mut compiler = ProgramCompiler::new();
    compiler.source_file = source_file.to_string();
    compiler.sync_record_field_symbols(arena);

    if let Some(module_root) = module_root {
        compiler.load_modules(items, module_root, arena)?;
    }

    for item in items {
        if let TopLevel::Stmt(Stmt::Binding(name, _, _)) = item {
            compiler.ensure_global(name);
        }
    }

    for item in items {
        match item {
            TopLevel::FnDef(fndef) => {
                compiler.ensure_global(&fndef.name);
                let effect_ids: Vec<u32> = fndef
                    .effects
                    .iter()
                    .map(|effect| compiler.symbols.intern_name(&effect.node))
                    .collect();
                let fn_id = compiler.code.add_function(FnChunk {
                    name: fndef.name.clone(),
                    arity: fndef.params.len() as u8,
                    local_count: 0,
                    code: Vec::new(),
                    constants: Vec::new(),
                    effects: effect_ids,
                    thin: false,
                    parent_thin: false,
                    leaf: false,
                    source_file: String::new(),
                    line_table: Vec::new(),
                });
                let symbol_id = compiler.symbols.intern_function(
                    &fndef.name,
                    fn_id,
                    &fndef
                        .effects
                        .iter()
                        .map(|e| e.node.clone())
                        .collect::<Vec<_>>(),
                );
                let global_idx = compiler.global_names[&fndef.name];
                compiler.globals[global_idx as usize] = VmSymbolTable::symbol_ref(symbol_id);
            }
            TopLevel::TypeDef(td) => {
                compiler.register_type_def(td, arena);
            }
            _ => {}
        }
    }

    compiler.register_current_module_namespace(items);

    for item in items {
        if let TopLevel::FnDef(fndef) = item {
            let fn_id = compiler.code.find(&fndef.name).unwrap();
            let chunk = compiler.compile_fn(fndef, arena)?;
            compiler.code.functions[fn_id as usize] = chunk;
        }
    }

    compiler.compile_top_level(items, arena)?;
    compiler.code.symbols = compiler.symbols.clone();
    classify::classify_thin_functions(&mut compiler.code, arena)?;

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

struct ProgramCompiler {
    code: CodeStore,
    symbols: VmSymbolTable,
    globals: Vec<NanValue>,
    global_names: HashMap<String, u16>,
    /// Source file path for the main program (propagated to FnChunks).
    source_file: String,
}

impl ProgramCompiler {
    fn new() -> Self {
        let mut compiler = ProgramCompiler {
            code: CodeStore::new(),
            symbols: VmSymbolTable::default(),
            globals: Vec::new(),
            global_names: HashMap::new(),
            source_file: String::new(),
        };
        compiler.bootstrap_core_symbols();
        compiler
    }

    fn sync_record_field_symbols(&mut self, arena: &Arena) {
        for type_id in 0..arena.type_count() {
            let type_name = arena.get_type_name(type_id);
            self.symbols.intern_namespace_path(type_name);
            let field_names = arena.get_field_names(type_id);
            if field_names.is_empty() {
                continue;
            }
            let field_symbol_ids: Vec<u32> = field_names
                .iter()
                .map(|field_name| self.symbols.intern_name(field_name))
                .collect();
            self.code.register_record_fields(type_id, &field_symbol_ids);
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
            None => return Ok(()),
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

        for item in &mod_items {
            if let TopLevel::TypeDef(td) = item {
                self.register_type_def(td, arena);
            }
        }

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

        let mut module_fn_ids: Vec<(String, u32)> = Vec::new();
        for item in &mod_items {
            if let TopLevel::FnDef(fndef) = item {
                let qualified_name = format!("{}.{}", dep_name, fndef.name);
                let effect_ids: Vec<u32> = fndef
                    .effects
                    .iter()
                    .map(|effect| self.symbols.intern_name(&effect.node))
                    .collect();
                let fn_id = self.code.add_function(FnChunk {
                    name: qualified_name.clone(),
                    arity: fndef.params.len() as u8,
                    local_count: 0,
                    code: Vec::new(),
                    constants: Vec::new(),
                    effects: effect_ids,
                    thin: false,
                    parent_thin: false,
                    leaf: false,
                    source_file: String::new(),
                    line_table: Vec::new(),
                });
                self.symbols.intern_function(
                    &qualified_name,
                    fn_id,
                    &fndef
                        .effects
                        .iter()
                        .map(|e| e.node.clone())
                        .collect::<Vec<_>>(),
                );
                module_fn_ids.push((fndef.name.clone(), fn_id));
            }
        }

        let module_scope: HashMap<String, u32> = module_fn_ids.iter().cloned().collect();

        let mut fn_idx = 0;
        for item in &mod_items {
            if let TopLevel::FnDef(fndef) = item {
                let (fn_name, fn_id) = &module_fn_ids[fn_idx];
                let mut chunk = self.compile_fn_with_scope(fndef, arena, &module_scope)?;
                chunk.name = format!("{}.{}", dep_name, fn_name);
                self.code.functions[*fn_id as usize] = chunk;
                fn_idx += 1;
            }
        }

        for (fn_name, _fn_id) in &module_fn_ids {
            let exposed = match &exposes {
                Some(list) => list.iter().any(|e| e == fn_name),
                None => !fn_name.starts_with('_'),
            };
            if exposed {
                let qualified = format!("{}.{}", dep_name, fn_name);
                let global_idx = self.ensure_global(&qualified);
                let symbol_id = self.symbols.find(&qualified).ok_or_else(|| CompileError {
                    msg: format!("missing VM symbol for exposed function {}", qualified),
                })?;
                self.globals[global_idx as usize] = VmSymbolTable::symbol_ref(symbol_id);
            }
        }

        let module_symbol_id = self.symbols.intern_namespace_path(dep_name);
        for item in &mod_items {
            if let TopLevel::TypeDef(td) = item {
                let type_name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                };
                let exposed = match &exposes {
                    Some(list) => list.iter().any(|e| e == type_name),
                    None => !type_name.starts_with('_'),
                };
                if exposed && let Some(type_symbol_id) = self.symbols.find(type_name) {
                    let member_symbol_id = self.symbols.intern_name(type_name);
                    self.symbols.add_namespace_member_by_id(
                        module_symbol_id,
                        member_symbol_id,
                        VmSymbolTable::symbol_ref(type_symbol_id),
                    );
                }
            }
        }

        for (fn_name, _fn_id) in &module_fn_ids {
            let exposed = match &exposes {
                Some(list) => list.iter().any(|e| e == fn_name),
                None => !fn_name.starts_with('_'),
            };
            if exposed {
                let qualified = format!("{}.{}", dep_name, fn_name);
                if let Some(fn_symbol_id) = self.symbols.find(&qualified) {
                    let member_symbol_id = self.symbols.intern_name(fn_name);
                    self.symbols.add_namespace_member_by_id(
                        module_symbol_id,
                        member_symbol_id,
                        VmSymbolTable::symbol_ref(fn_symbol_id),
                    );
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
                self.symbols.intern_namespace_path(name);
                let field_names: Vec<String> = fields.iter().map(|(n, _)| n.clone()).collect();
                let field_symbol_ids: Vec<u32> = field_names
                    .iter()
                    .map(|field_name| self.symbols.intern_name(field_name))
                    .collect();
                let type_id = arena.register_record_type(name, field_names);
                self.code.register_record_fields(type_id, &field_symbol_ids);
            }
            TypeDef::Sum { name, variants, .. } => {
                let type_symbol_id = self.symbols.intern_namespace_path(name);
                let variant_names: Vec<String> = variants.iter().map(|v| v.name.clone()).collect();
                let type_id = arena.register_sum_type(name, variant_names);
                for (variant_id, variant) in variants.iter().enumerate() {
                    let ctor_id = arena
                        .find_ctor_id(type_id, variant_id as u16)
                        .expect("ctor id");
                    let qualified_name = format!("{}.{}", name, variant.name);
                    let ctor_symbol_id = self.symbols.intern_variant_ctor(
                        &qualified_name,
                        VmVariantCtor {
                            type_id,
                            variant_id: variant_id as u16,
                            ctor_id,
                            field_count: variant.fields.len() as u8,
                        },
                    );
                    let member_symbol_id = self.symbols.intern_name(&variant.name);
                    self.symbols.add_namespace_member_by_id(
                        type_symbol_id,
                        member_symbol_id,
                        VmSymbolTable::symbol_ref(ctor_symbol_id),
                    );
                }
            }
        }
    }

    fn bootstrap_core_symbols(&mut self) {
        for builtin in VmBuiltin::ALL.iter().copied() {
            let builtin_symbol_id = self.symbols.intern_builtin(builtin);
            if let Some((namespace, member)) = builtin.name().split_once('.') {
                let namespace_symbol_id = self.symbols.intern_namespace_path(namespace);
                let member_symbol_id = self.symbols.intern_name(member);
                self.symbols.add_namespace_member_by_id(
                    namespace_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(builtin_symbol_id),
                );
            }
        }

        let result_symbol_id = self.symbols.intern_namespace_path("Result");
        let ok_symbol_id = self.symbols.intern_wrapper("Result.Ok", 0);
        let err_symbol_id = self.symbols.intern_wrapper("Result.Err", 1);
        let ok_member_symbol_id = self.symbols.intern_name("Ok");
        self.symbols.add_namespace_member_by_id(
            result_symbol_id,
            ok_member_symbol_id,
            VmSymbolTable::symbol_ref(ok_symbol_id),
        );
        let err_member_symbol_id = self.symbols.intern_name("Err");
        self.symbols.add_namespace_member_by_id(
            result_symbol_id,
            err_member_symbol_id,
            VmSymbolTable::symbol_ref(err_symbol_id),
        );
        for (member, builtin_name) in result::extra_members() {
            if let Some(symbol_id) = self.symbols.find(&builtin_name) {
                let member_symbol_id = self.symbols.intern_name(member);
                self.symbols.add_namespace_member_by_id(
                    result_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(symbol_id),
                );
            }
        }

        let option_symbol_id = self.symbols.intern_namespace_path("Option");
        let some_symbol_id = self.symbols.intern_wrapper("Option.Some", 2);
        self.symbols.intern_constant("Option.None", NanValue::NONE);
        let some_member_symbol_id = self.symbols.intern_name("Some");
        self.symbols.add_namespace_member_by_id(
            option_symbol_id,
            some_member_symbol_id,
            VmSymbolTable::symbol_ref(some_symbol_id),
        );
        let none_member_symbol_id = self.symbols.intern_name("None");
        self.symbols.add_namespace_member_by_id(
            option_symbol_id,
            none_member_symbol_id,
            NanValue::NONE,
        );
        for (member, builtin_name) in option::extra_members() {
            if let Some(symbol_id) = self.symbols.find(&builtin_name) {
                let member_symbol_id = self.symbols.intern_name(member);
                self.symbols.add_namespace_member_by_id(
                    option_symbol_id,
                    member_symbol_id,
                    VmSymbolTable::symbol_ref(symbol_id),
                );
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
            fndef
                .effects
                .iter()
                .map(|effect| self.symbols.intern_name(&effect.node))
                .collect(),
            local_slots,
            &self.global_names,
            module_scope,
            &self.code,
            &mut self.symbols,
            arena,
        );
        fc.source_file = self.source_file.clone();
        fc.note_line(fndef.line);

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
            &mut self.symbols,
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

    fn register_current_module_namespace(&mut self, items: &[TopLevel]) {
        let Some(module) = items.iter().find_map(|item| {
            if let TopLevel::Module(module) = item {
                Some(module)
            } else {
                None
            }
        }) else {
            return;
        };

        let module_symbol_id = self.symbols.intern_namespace_path(&module.name);

        for item in items {
            match item {
                TopLevel::FnDef(fndef) => {
                    let exposed = if module.exposes.is_empty() {
                        !fndef.name.starts_with('_')
                    } else {
                        module.exposes.iter().any(|name| name == &fndef.name)
                    };
                    if exposed && let Some(symbol_id) = self.symbols.find(&fndef.name) {
                        let member_symbol_id = self.symbols.intern_name(&fndef.name);
                        self.symbols.add_namespace_member_by_id(
                            module_symbol_id,
                            member_symbol_id,
                            VmSymbolTable::symbol_ref(symbol_id),
                        );
                    }
                }
                TopLevel::TypeDef(TypeDef::Product { name, .. } | TypeDef::Sum { name, .. }) => {
                    let exposed = if module.exposes.is_empty() {
                        !name.starts_with('_')
                    } else {
                        module.exposes.iter().any(|member| member == name)
                    };
                    if exposed && let Some(symbol_id) = self.symbols.find(name) {
                        let member_symbol_id = self.symbols.intern_name(name);
                        self.symbols.add_namespace_member_by_id(
                            module_symbol_id,
                            member_symbol_id,
                            VmSymbolTable::symbol_ref(symbol_id),
                        );
                    }
                }
                _ => {}
            }
        }
    }
}

/// What a function expression resolves to at compile time.
enum CallTarget {
    /// Known function id (local or qualified module function).
    KnownFn(u32),
    /// Result.Ok / Result.Err / Option.Some → WRAP opcode. kind: 0=Ok, 1=Err, 2=Some.
    Wrapper(u8),
    /// Option.None → load constant.
    None_,
    /// User-defined variant constructor: Shape.Circle → VARIANT_NEW (or inline nullary at runtime).
    Variant(u32, u16),
    /// Known VM builtin/service resolved by name and interned into the VM symbol table.
    Builtin(VmBuiltin),
    /// Unknown capitalized dotted path that did not resolve to a function, variant, or builtin.
    UnknownQualified(String),
}

struct FnCompiler<'a> {
    name: String,
    arity: u8,
    local_count: u16,
    effects: Vec<u32>,
    local_slots: HashMap<String, u16>,
    global_names: &'a HashMap<String, u16>,
    /// Module-local function scope: simple_name → fn_id.
    /// Used for intra-module calls (e.g. `placeStairs` inside map.av).
    module_scope: &'a HashMap<String, u32>,
    code_store: &'a CodeStore,
    symbols: &'a mut VmSymbolTable,
    arena: &'a mut Arena,
    code: Vec<u8>,
    constants: Vec<NanValue>,
    /// Byte offset of the last emitted opcode (for superinstruction fusion).
    last_op_pos: usize,
    /// Source file path for this function.
    source_file: String,
    /// Run-length encoded line table being built: (bytecode_offset, source_line).
    line_table: Vec<(u16, u16)>,
    /// Last emitted line (for RLE dedup).
    last_noted_line: u16,
    /// Parameter slots that are sole-owned in the current tail-call context.
    /// Set during `compile_tail_call`, cleared after. Enables `compile_call`
    /// to emit `CALL_BUILTIN_OWNED` when a builtin arg is an owned local.
    owned_params: HashSet<u16>,
}

impl<'a> FnCompiler<'a> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        name: &str,
        arity: u8,
        local_count: u16,
        effects: Vec<u32>,
        local_slots: HashMap<String, u16>,
        global_names: &'a HashMap<String, u16>,
        module_scope: &'a HashMap<String, u32>,
        code_store: &'a CodeStore,
        symbols: &'a mut VmSymbolTable,
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
            symbols,
            arena,
            code: Vec::new(),
            constants: Vec::new(),
            last_op_pos: usize::MAX,
            source_file: String::new(),
            line_table: Vec::new(),
            last_noted_line: 0,
            owned_params: HashSet::new(),
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
            thin: false,
            parent_thin: false,
            leaf: false,
            source_file: self.source_file,
            line_table: self.line_table,
        }
    }

    /// Record that bytecode emitted from this point forward corresponds to
    /// the given source line. RLE-deduplicated: consecutive calls with the
    /// same line produce only one entry.
    fn note_line(&mut self, line: usize) {
        if line == 0 {
            return;
        }
        let line16 = line as u16;
        if line16 == self.last_noted_line {
            return; // RLE dedup
        }
        self.last_noted_line = line16;
        self.line_table.push((self.code.len() as u16, line16));
    }

    fn emit_op(&mut self, op: u8) {
        let prev_pos = self.last_op_pos;
        let prev_op = if prev_pos < self.code.len() {
            self.code[prev_pos]
        } else {
            0xFF
        };

        // LOAD_LOCAL + LOAD_LOCAL → LOAD_LOCAL_2
        if op == LOAD_LOCAL && prev_op == LOAD_LOCAL && prev_pos + 2 == self.code.len() {
            self.code[prev_pos] = LOAD_LOCAL_2;
            // slot_a already at prev_pos+1, slot_b emitted next via emit_u8
            return;
        }
        // LOAD_LOCAL + LOAD_CONST → LOAD_LOCAL_CONST
        if op == LOAD_CONST && prev_op == LOAD_LOCAL && prev_pos + 2 == self.code.len() {
            self.code[prev_pos] = LOAD_LOCAL_CONST;
            // slot at prev_pos+1, const_idx (u16) emitted next via emit_u16
            return;
        }
        // VECTOR_GET + LOAD_CONST(hi,lo) + UNWRAP_OR → VECTOR_GET_OR(hi,lo)
        // Before: [..., VECTOR_GET, LOAD_CONST, hi, lo] + about to emit UNWRAP_OR
        // After:  [..., VECTOR_GET_OR, hi, lo]
        if op == UNWRAP_OR && self.code.len() >= 4 {
            let len = self.code.len();
            if self.code[len - 4] == VECTOR_GET && self.code[len - 3] == LOAD_CONST {
                let hi = self.code[len - 2];
                let lo = self.code[len - 1];
                self.code[len - 4] = VECTOR_GET_OR;
                self.code[len - 3] = hi;
                self.code[len - 2] = lo;
                self.code.pop(); // remove extra byte
                self.last_op_pos = len - 4;
                return;
            }
        }
        self.last_op_pos = self.code.len();
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

    fn emit_u32(&mut self, val: u32) {
        self.code.push((val >> 24) as u8);
        self.code.push(((val >> 16) & 0xFF) as u8);
        self.code.push(((val >> 8) & 0xFF) as u8);
        self.code.push((val & 0xFF) as u8);
    }

    fn emit_u64(&mut self, val: u64) {
        self.code.extend_from_slice(&val.to_be_bytes());
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
}

#[cfg(test)]
mod tests {
    use super::compile_program;
    use crate::nan_value::Arena;
    use crate::source::parse_source;
    use crate::vm::opcode::{LT, NOT, VECTOR_GET_OR, VECTOR_SET_OR_KEEP};

    #[test]
    fn vector_get_with_literal_default_lowers_to_vector_get_or() {
        let source = r#"
module Demo

fn cellAt(grid: Vector<Int>, idx: Int) -> Int
    Option.withDefault(Vector.get(grid, idx), 0)
"#;

        let mut items = parse_source(source).expect("source should parse");
        crate::tco::transform_program(&mut items);
        crate::resolver::resolve_program(&mut items);

        let mut arena = Arena::new();
        let (code, _globals) = compile_program(&items, &mut arena).expect("vm compile should pass");
        let fn_id = code.find("cellAt").expect("cellAt should exist");
        let chunk = code.get(fn_id);

        assert!(
            chunk.code.contains(&VECTOR_GET_OR),
            "expected VECTOR_GET_OR in bytecode, got {:?}",
            chunk.code
        );
    }

    #[test]
    fn vector_set_with_same_default_lowers_to_vector_set_or_keep() {
        let source = r#"
module Demo

fn updateOrKeep(vec: Vector<Int>, idx: Int, value: Int) -> Vector<Int>
    Option.withDefault(Vector.set(vec, idx, value), vec)
"#;

        let mut items = parse_source(source).expect("source should parse");
        crate::tco::transform_program(&mut items);
        crate::resolver::resolve_program(&mut items);

        let mut arena = Arena::new();
        let (code, _globals) = compile_program(&items, &mut arena).expect("vm compile should pass");
        let fn_id = code
            .find("updateOrKeep")
            .expect("updateOrKeep should exist");
        let chunk = code.get(fn_id);

        assert!(
            chunk.code.contains(&VECTOR_SET_OR_KEEP),
            "expected VECTOR_SET_OR_KEEP in bytecode, got {:?}",
            chunk.code
        );
    }

    #[test]
    fn bool_match_on_gte_uses_base_compare_without_not() {
        let source = r#"
module Demo

fn bucket(n: Int) -> Int
    match n >= 10
        true -> 7
        false -> 3
"#;

        let mut items = parse_source(source).expect("source should parse");
        crate::tco::transform_program(&mut items);
        crate::resolver::resolve_program(&mut items);

        let mut arena = Arena::new();
        let (code, _globals) = compile_program(&items, &mut arena).expect("vm compile should pass");
        let fn_id = code.find("bucket").expect("bucket should exist");
        let chunk = code.get(fn_id);

        assert!(
            chunk.code.contains(&LT),
            "expected LT in bytecode, got {:?}",
            chunk.code
        );
        assert!(
            !chunk.code.contains(&NOT),
            "did not expect NOT in normalized bool-match bytecode, got {:?}",
            chunk.code
        );
    }

    #[test]
    fn self_host_runtime_http_server_aliases_compile_in_vm() {
        let source = r#"
module Demo

fn listen(handler: Int) -> Unit
    SelfHostRuntime.httpServerListen(8080, handler)

fn listenWith(context: Int, handler: Int) -> Unit
    SelfHostRuntime.httpServerListenWith(8081, context, handler)
"#;

        let mut items = parse_source(source).expect("source should parse");
        crate::tco::transform_program(&mut items);
        crate::resolver::resolve_program(&mut items);

        let mut arena = Arena::new();
        let (code, _globals) = compile_program(&items, &mut arena).expect("vm compile should pass");
        assert!(code.find("listen").is_some(), "listen should compile");
        assert!(
            code.find("listenWith").is_some(),
            "listenWith should compile"
        );
    }
}
