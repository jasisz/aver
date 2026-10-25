//! Per-record / per-sum / per-carrier hash helpers — the symmetric
//! counterpart to `eq_helpers.rs` for the hash side of nominal +
//! generic-carrier dispatch.
//!
//! ## Why
//!
//! `Map<K, V>` keyed by a record (`Map<Person, …>`) wants a
//! deterministic hash so two `Person` values that compare equal
//! collapse to the same bucket. The inline `emit_record_inline_hash`
//! / `emit_sum_inline_hash` in `lists.rs` previously fell back to
//! `drop + i32.const 0` for any non-primitive field — correct
//! (eq still disambiguates the bucket) but degenerate, every value
//! sharing primitive-prefix maps to one bucket and lookup goes
//! O(n).
//!
//! This module sets up `__hash_<X>` helpers per nominal /
//! carrier instantiation. Body emit goes through `Call(__hash_<X>)`
//! for non-primitive field values; the helper itself does the
//! shape-specific DJB2 fold. Symmetric to how `__eq_<X>` works on
//! the equality side.

use std::collections::{HashMap, HashSet};

use wasm_encoder::{Function, Instruction};

use super::super::WasmGcError;
use super::super::types::TypeRegistry;
use super::node_table::{Entry, TableGlobals, TableSlots};

/// What kind of type a registered hash helper covers. Same shapes
/// as `EqKind`, separate enum to keep the two registries
/// independent (a type may want a hash helper without an eq helper
/// — e.g. `Map<X, V>` registers hash for X without ever forcing
/// `==` on the surface).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HashKind {
    Record,
    Sum,
    PackedSequence,
    OptionHash,
    ResultHash,
    TupleHash,
}

/// Per-module registry of `__hash_<TypeName>` helpers needed for
/// shape-faithful hashing inside list/vec/map helpers + per-record
/// / sum / carrier inline hash bodies.
#[derive(Default)]
pub(crate) struct HashHelperRegistry {
    order: Vec<String>,
    kinds: HashMap<String, HashKind>,
    /// `type_name -> (wasm_fn_idx, wasm_type_idx)`.
    slots: HashMap<String, (u32, u32)>,
    /// The helpers that hash in steps (see [`HashHelperRegistry::find_walks`]),
    /// in slot order, each with the first `(wasm_fn_idx, wasm_type_idx)` of
    /// its two steps: against the budget, then with memory.
    walks: Vec<(String, (u32, u32))>,
    /// The table of the remembered hashes, and the slot of the byte hash of
    /// a string, which follow the walks when there are any.
    table: Option<TableSlots>,
    string_slot: Option<(u32, u32)>,
    /// The string array type, when the walks hash a string field by its
    /// bytes.
    string_type: Option<u32>,
}

/// Nodes a hash takes before it starts again with memory.
const WALK_BUDGET: i32 = 1024;

/// The state of a hash in steps: the budget left, and the table of the
/// remembered hashes.
pub(crate) struct HashWalkGlobals {
    budget: u32,
    table: TableGlobals,
}

/// What a step with memory calls: the lookup and insert of the remembered
/// hashes, and the helpers that hash in steps.
struct Remember<'a> {
    lookup: u32,
    insert: u32,
    walked: &'a HashSet<String>,
}

fn eq_ref_type() -> wasm_encoder::ValType {
    wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Abstract {
            shared: false,
            ty: wasm_encoder::AbstractHeapType::Eq,
        },
    })
}

impl HashKind {
    /// The equality helper kind of the same shape.
    fn as_eq(self) -> super::eq_helpers::EqKind {
        use super::eq_helpers::EqKind;
        match self {
            HashKind::Record => EqKind::Record,
            HashKind::Sum => EqKind::Sum,
            HashKind::PackedSequence => EqKind::PackedSequence,
            HashKind::OptionHash => EqKind::OptionEq,
            HashKind::ResultHash => EqKind::ResultEq,
            HashKind::TupleHash => EqKind::TupleEq,
        }
    }
}

/// Which body of a helper: the plain one, or for a type that hashes in
/// steps the start of a hash and its two steps.
#[derive(Clone, Copy)]
enum Body<'a> {
    Plain,
    Step(&'a HashWalkGlobals),
    Memory(&'a Remember<'a>),
}

impl Body<'_> {
    /// The locals a body declares after its own, the last of which is local
    /// `h`: for a step with memory the key of its node, whether it holds the
    /// walk, and what the lookup found.
    fn locals(self) -> Vec<(u32, wasm_encoder::ValType)> {
        match self {
            Body::Memory(_) => vec![
                (2, wasm_encoder::ValType::I32),
                (1, wasm_encoder::ValType::I64),
            ],
            _ => Vec::new(),
        }
    }

    /// In a step with memory, a node of struct `struct_idx` (pushed typed
    /// by `node`) whose fields of `field_tys` hold the walk answers its
    /// remembered hash, and is remembered once hashed.
    #[allow(clippy::too_many_arguments)]
    fn recall(
        self,
        f: &mut Function,
        h: u32,
        owner: &str,
        field_tys: &[String],
        wasm_tys: &dyn Fn() -> Result<Vec<wasm_encoder::ValType>, WasmGcError>,
        struct_idx: u32,
        registry: &TypeRegistry,
        node: &dyn Fn(&mut Function),
    ) -> Result<(), WasmGcError> {
        let Body::Memory(memory) = self else {
            return Ok(());
        };
        if !super::node_table::holds_walk(field_tys, owner, memory.walked, registry) {
            return Ok(());
        }
        let (key, holds, found) = (h + 1, h + 2, h + 3);
        super::node_table::emit_node_key(f, struct_idx, &wasm_tys()?, registry, node, key);
        for op in [
            Instruction::LocalGet(0),
            Instruction::LocalGet(key),
            Instruction::Call(memory.lookup),
            Instruction::LocalTee(found),
            Instruction::I64Const(0),
            Instruction::I64GeS,
            Instruction::If(wasm_encoder::BlockType::Empty),
            Instruction::LocalGet(found),
            Instruction::I32WrapI64,
            Instruction::Return,
            Instruction::End,
            Instruction::I32Const(1),
            Instruction::LocalSet(holds),
        ] {
            f.instruction(&op);
        }
        Ok(())
    }

    /// What a step does before its node is hashed: one against the budget
    /// takes one from it and answers 0 once it is spent.
    fn prologue(self, f: &mut Function) {
        match self {
            Body::Plain => {}
            Body::Step(globals) => {
                f.instruction(&Instruction::GlobalGet(globals.budget));
                f.instruction(&Instruction::I32Eqz);
                f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                f.instruction(&Instruction::I32Const(0));
                f.instruction(&Instruction::Return);
                f.instruction(&Instruction::End);
                f.instruction(&Instruction::GlobalGet(globals.budget));
                f.instruction(&Instruction::I32Const(1));
                f.instruction(&Instruction::I32Sub);
                f.instruction(&Instruction::GlobalSet(globals.budget));
            }
            Body::Memory(_) => {}
        }
    }

    /// Pushes the hash folded into local `h`. A step mixes it so that a
    /// node's hash spreads over every bit (the fold alone multiplies a
    /// subterm held twice by an even number), and one with memory remembers
    /// it.
    fn finish(self, f: &mut Function, h: u32) {
        if !matches!(self, Body::Plain) {
            for (shift, multiplier) in [(16, 0x85eb_ca6b_u32), (13, 0xc2b2_ae35), (16, 1)] {
                f.instruction(&Instruction::LocalGet(h));
                f.instruction(&Instruction::LocalGet(h));
                f.instruction(&Instruction::I32Const(shift));
                f.instruction(&Instruction::I32ShrU);
                f.instruction(&Instruction::I32Xor);
                if multiplier != 1 {
                    f.instruction(&Instruction::I32Const(multiplier as i32));
                    f.instruction(&Instruction::I32Mul);
                }
                f.instruction(&Instruction::LocalSet(h));
            }
        }
        if let Body::Memory(memory) = self {
            for op in [
                Instruction::LocalGet(h + 2),
                Instruction::If(wasm_encoder::BlockType::Empty),
                Instruction::LocalGet(0),
                Instruction::LocalGet(h + 1),
                Instruction::LocalGet(h),
                Instruction::Call(memory.insert),
                Instruction::End,
            ] {
                f.instruction(&op);
            }
        }
        f.instruction(&Instruction::LocalGet(h));
    }
}

/// The start of a hash in steps of a value of a type that reaches itself:
/// against the budget, then from the start with memory if that ran out.
/// The budget of a hash this one is part of is kept and given back.
fn emit_walk_start(globals: &HashWalkGlobals, step: u32, table: &TableSlots) -> Function {
    // Locals: 1 = the budget of the enclosing hash, 2 = the hash.
    let mut f = Function::new(vec![(2, wasm_encoder::ValType::I32)]);
    f.instruction(&Instruction::GlobalGet(globals.budget));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(WALK_BUDGET));
    f.instruction(&Instruction::GlobalSet(globals.budget));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(step));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::GlobalGet(globals.budget));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Call(table.enter()));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(step + 1));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Call(table.leave()));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::GlobalSet(globals.budget));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    f
}

impl HashHelperRegistry {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn register(&mut self, type_name: &str, kind: HashKind) {
        if !self.kinds.contains_key(type_name) {
            self.order.push(type_name.to_string());
            self.kinds.insert(type_name.to_string(), kind);
        }
    }

    /// Walk `type_name`'s fields and register `__hash_<X>` for
    /// every nominal / carrier piece reachable. Mirrors
    /// `EqHelperRegistry::register_transitive` but for hash; same
    /// resolvability gate so we don't end up with a registered
    /// helper whose body can't be emitted (a record holding e.g.
    /// `List<X>` field — list-as-field hash isn't wired yet).
    pub(crate) fn register_transitive(
        &mut self,
        type_name: &str,
        kind: HashKind,
        registry: &TypeRegistry,
    ) {
        if self.kinds.contains_key(type_name) {
            return;
        }
        // ETAP-2 carrier-`i64`: an eligible carrier is `i64`-erased — it has
        // no struct and needs no per-type `__hash_<Carrier>` helper (its hash
        // inlines as raw `i32.wrap_i64` at every use site). A struct-shaped
        // helper body over an `i64` value is invalid wasm. This is the
        // backstop covering every caller path (seed, field walk, `==`
        // discovery, nominal-in-type walk).
        if registry.is_eligible_carrier(type_name) {
            return;
        }
        let kind = if registry.packed_sequence(type_name).is_some() {
            HashKind::PackedSequence
        } else {
            kind
        };
        let mut seen = std::collections::HashSet::new();
        let resolvable = match kind {
            HashKind::Record => {
                super::super::lists::record_fields_resolvable(type_name, registry, &mut seen)
            }
            HashKind::Sum => {
                super::super::lists::sum_fields_resolvable(type_name, registry, &mut seen)
            }
            HashKind::PackedSequence => true,
            HashKind::OptionHash | HashKind::ResultHash | HashKind::TupleHash => true,
        };
        if !resolvable {
            return;
        }
        self.register(type_name, kind);
        match kind {
            HashKind::Record => {
                if let Some(fields) = registry.record_fields.get(type_name) {
                    for (_, field_ty) in fields {
                        self.register_field_type(field_ty.trim(), registry);
                    }
                }
            }
            HashKind::Sum => {
                let variants: Vec<_> = registry
                    .variants
                    .values()
                    .flat_map(|vs| vs.iter())
                    .filter(|v| v.parent == type_name)
                    .cloned()
                    .collect();
                for v in &variants {
                    for field_ty in &v.fields {
                        self.register_field_type(field_ty.trim(), registry);
                    }
                }
            }
            HashKind::PackedSequence => {}
            // Mirror eq_helpers — recurse so direct top-level
            // registration of a carrier (seed walker etc.) still
            // discovers inner types.
            HashKind::OptionHash => {
                if let Some(inner) = type_name
                    .strip_prefix("Option<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    self.register_field_type(inner.trim(), registry);
                }
            }
            HashKind::ResultHash => {
                if let Some((ok, err)) = parse_result_kv(type_name) {
                    self.register_field_type(ok.trim(), registry);
                    self.register_field_type(err.trim(), registry);
                }
            }
            HashKind::TupleHash => {
                if let Some(elems) = parse_tuple_elems(type_name) {
                    for e in elems {
                        self.register_field_type(e.trim(), registry);
                    }
                }
            }
        }
    }

    fn register_field_type(&mut self, field_ty: &str, registry: &TypeRegistry) {
        if matches!(
            field_ty,
            "Int" | "Float" | "Bool" | "String" | "Unit" | "Char"
        ) {
            return;
        }
        // ETAP-2 carrier-`i64`: an eligible carrier is `i64`-erased — no
        // struct, no per-type `__hash_<Carrier>` helper; its hash is the raw
        // `i32.wrap_i64` inlined at the use site. Treat like a primitive.
        if registry.is_eligible_carrier(field_ty) {
            return;
        }
        if registry.packed_sequence(field_ty).is_some() {
            self.register_transitive(field_ty, HashKind::PackedSequence, registry);
            return;
        }
        if registry.record_fields.contains_key(field_ty) {
            self.register_transitive(field_ty, HashKind::Record, registry);
            return;
        }
        if registry
            .variants
            .values()
            .flat_map(|v| v.iter())
            .any(|v| v.parent == field_ty)
        {
            self.register_transitive(field_ty, HashKind::Sum, registry);
            return;
        }
        if let Some(inner) = field_ty
            .strip_prefix("Option<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.register_transitive(field_ty, HashKind::OptionHash, registry);
            self.register_field_type(inner.trim(), registry);
        } else if field_ty.starts_with("Result<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, HashKind::ResultHash, registry);
            if let Some((ok, err)) = parse_result_kv(field_ty) {
                self.register_field_type(ok.trim(), registry);
                self.register_field_type(err.trim(), registry);
            }
        } else if field_ty.starts_with("Tuple<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, HashKind::TupleHash, registry);
            if let Some(elems) = parse_tuple_elems(field_ty) {
                for elem in elems {
                    self.register_field_type(elem.trim(), registry);
                }
            }
        } else if let Some(inner) = field_ty
            .strip_prefix("List<")
            .and_then(|s| s.strip_suffix('>'))
        {
            // Symmetric to eq_helpers — recurse into the element so
            // `List<Option<X>>` registers `__hash_Option<X>`. The
            // list_helpers slot itself is owned by the separate
            // ListHelperRegistry; this walk only covers the carrier
            // / nominal hash dispatch the inline body needs.
            self.register_field_type(inner.trim(), registry);
        } else if let Some(inner) = field_ty
            .strip_prefix("Vector<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.register_field_type(inner.trim(), registry);
        } else if let Some(inner) = field_ty
            .strip_prefix("Map<")
            .and_then(|s| s.strip_suffix('>'))
        {
            // Mirror eq_helpers — Map<K,V> hash slot lives in
            // MapHelperRegistry; recurse so K and V's hash helpers
            // exist for the structural fold.
            let bytes = inner.as_bytes();
            let mut depth: i32 = 0;
            for (idx, b) in bytes.iter().enumerate() {
                match b {
                    b'<' | b'(' => depth += 1,
                    b'>' | b')' => depth -= 1,
                    b',' if depth == 0 => {
                        let k = inner[..idx].trim();
                        let v = inner[idx + 1..].trim();
                        self.register_field_type(k, registry);
                        self.register_field_type(v, registry);
                        return;
                    }
                    _ => {}
                }
            }
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&str, HashKind)> + '_ {
        self.order.iter().map(|n| (n.as_str(), self.kinds[n]))
    }

    pub(crate) fn assign_slots(&mut self, next_fn_idx: &mut u32, next_type_idx: &mut u32) {
        // Registration order may inherit a HashMap traversal from nominal
        // discovery. Stable slots are required for reproducible artifacts and
        // for Wasmtime to reuse the native-code cache across processes.
        self.order.sort_unstable();
        let mut slot = || {
            let s = (*next_fn_idx, *next_type_idx);
            *next_fn_idx += 1;
            *next_type_idx += 1;
            s
        };
        for name in &self.order {
            self.slots.insert(name.clone(), slot());
        }
        for (_, walk) in &mut self.walks {
            *walk = slot();
            slot();
        }
        if !self.walks.is_empty() {
            self.table = Some(TableSlots::assign(Entry::Hash, next_fn_idx, next_type_idx));
            if self.string_type.is_some() {
                self.string_slot = Some((*next_fn_idx, *next_type_idx));
                *next_fn_idx += 1;
                *next_type_idx += 1;
            }
        }
    }

    /// Picks the helpers that hash in steps: a type that reaches itself
    /// through records, sums and `Option`s held directly, as `==` picks
    /// (`eq_helpers`), and that a key of one of the maps `keys` reaches. A
    /// hash of a value of such a type made node by node meets a subterm held
    /// under several nodes once for every path to it; a step takes one of a
    /// budget of [`WALK_BUDGET`] nodes, and a hash that runs out starts again
    /// with steps that remember the hash of every node they hash in a table
    /// (`node_table`). Only a key is hashed for what the program
    /// does, so a program with no such key keeps its bytes. Runs once every
    /// helper is registered.
    pub(crate) fn find_walks(&mut self, registry: &TypeRegistry, keys: &[String]) {
        use super::eq_helpers::{reached_from, self_reaching};
        self.order.sort_unstable();
        let kinds = self
            .kinds
            .iter()
            .map(|(name, kind)| (name.clone(), kind.as_eq()))
            .collect();
        let reached = reached_from(keys, &kinds, registry);
        self.walks = self_reaching(&self.order, &kinds, registry)
            .into_iter()
            .filter(|name| reached.contains(name))
            .map(|name| (name, (0, 0)))
            .collect();
        self.string_type = registry
            .string_array_type_idx
            .filter(|_| !self.walks.is_empty());
    }

    /// The helpers that hash in steps.
    pub(crate) fn walked(&self) -> impl Iterator<Item = &str> + '_ {
        self.walks.iter().map(|(name, _)| name.as_str())
    }

    /// The type idx of every helper function, in function index order.
    pub(crate) fn function_types(&self) -> Vec<u32> {
        let mut out: Vec<u32> = self.order.iter().map(|n| self.slots[n].1).collect();
        out.extend(self.walks.iter().flat_map(|(_, (_, t))| [*t, t + 1]));
        if let Some(table) = &self.table {
            out.extend(table.function_types());
        }
        out.extend(self.string_slot.map(|(_, t)| t));
        out
    }

    /// Declares the budget and the remembered hashes when a helper walks.
    pub(crate) fn emit_walk_globals(
        &self,
        globals: &mut wasm_encoder::GlobalSection,
        next_global_idx: &mut u32,
    ) -> Option<HashWalkGlobals> {
        let table = self.table?;
        let budget = *next_global_idx;
        let i32_global = wasm_encoder::GlobalType {
            val_type: wasm_encoder::ValType::I32,
            mutable: true,
            shared: false,
        };
        globals.global(i32_global, &wasm_encoder::ConstExpr::i32_const(0));
        *next_global_idx += 1;
        let table = table.emit_globals(globals, next_global_idx);
        Some(HashWalkGlobals { budget, table })
    }

    pub(crate) fn lookup_fn_idx(&self, type_name: &str) -> Option<u32> {
        self.slots.get(type_name).map(|(f, _)| *f)
    }

    #[cfg(test)]
    pub(crate) fn lookup_type_idx(&self, type_name: &str) -> Option<u32> {
        self.slots.get(type_name).map(|(_, t)| *t)
    }

    /// Emit `(eqref) -> i32` fn type for each registered helper, in
    /// the same order as `assign_slots`.
    pub(crate) fn emit_helper_types(&self, types: &mut wasm_encoder::TypeSection) {
        let eq_ref = eq_ref_type();
        let i32 = wasm_encoder::ValType::I32;
        for _ in &self.order {
            types.ty().function([eq_ref], [i32]);
        }
        for _ in &self.walks {
            types.ty().function([eq_ref], [i32]);
            types.ty().function([eq_ref], [i32]);
        }
        if let Some(table) = &self.table {
            table.emit_types(types);
            if let Some(string_type) = self.string_type {
                let string_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(string_type),
                });
                types.ty().function([string_ref], [i32]);
            }
        }
    }

    pub(crate) fn emit_helper_bodies(
        &self,
        codes: &mut wasm_encoder::CodeSection,
        registry: &TypeRegistry,
        compound_lookup: &HashMap<String, u32>,
        walk_globals: Option<&HashWalkGlobals>,
    ) -> Result<(), WasmGcError> {
        // Same shape as eq_helpers — merge `List<T>` / `Vector<T>`
        // hash fn idxs from list_helpers so a record field of a
        // compound type can `Call(__hash_<canonical>)`.
        let mut helper_idx_map: HashMap<String, u32> = self
            .slots
            .iter()
            .map(|(n, (fn_idx, _))| (n.clone(), *fn_idx))
            .collect();
        for (canonical, fn_idx) in compound_lookup {
            helper_idx_map.insert(canonical.clone(), *fn_idx);
        }
        // The steps hash a walked field by the step of the same kind, and a
        // string field by its bytes rather than its length.
        let walk_parts = match (self.table, walk_globals) {
            (Some(table), Some(globals)) => {
                let mut step_map = helper_idx_map.clone();
                let mut memory_map = helper_idx_map.clone();
                for (name, (step, _)) in &self.walks {
                    step_map.insert(name.clone(), *step);
                    memory_map.insert(name.clone(), step + 1);
                }
                if let Some((string_fn, _)) = self.string_slot {
                    step_map.insert("String".into(), string_fn);
                    memory_map.insert("String".into(), string_fn);
                }
                Some((globals, table, step_map, memory_map))
            }
            (None, _) => None,
            (Some(_), None) => {
                return Err(WasmGcError::Validation(
                    "hash helpers walk but their globals were not declared".into(),
                ));
            }
        };
        for name in &self.order {
            let walk = self.walks.iter().find(|(walked, _)| walked == name);
            match (&walk_parts, walk, self.kinds[name]) {
                (
                    Some((globals, table, _, _)),
                    Some((_, (step, _))),
                    HashKind::Record | HashKind::Sum,
                ) => {
                    codes.function(&emit_walk_start(globals, *step, table));
                }
                _ => self.emit_body(codes, name, registry, &helper_idx_map, Body::Plain)?,
            }
        }
        if let Some((globals, table, step_map, memory_map)) = &walk_parts {
            let walked = self.walks.iter().map(|(n, _)| n.clone()).collect();
            let remember = Remember {
                lookup: table.lookup(),
                insert: table.insert(),
                walked: &walked,
            };
            for (name, _) in &self.walks {
                self.emit_body(codes, name, registry, step_map, Body::Step(globals))?;
                let memory = Body::Memory(&remember);
                self.emit_body(codes, name, registry, memory_map, memory)?;
            }
            for f in table.emit_bodies(&globals.table)? {
                codes.function(&f);
            }
            if self.string_type.is_some() {
                codes.function(&super::super::maps::emit_hash_string(registry)?);
            }
        }
        Ok(())
    }

    /// One body of the helper of `name`, its fields hashed through `map`.
    fn emit_body(
        &self,
        codes: &mut wasm_encoder::CodeSection,
        name: &str,
        registry: &TypeRegistry,
        map: &HashMap<String, u32>,
        body: Body,
    ) -> Result<(), WasmGcError> {
        let self_fn_idx = map.get(name).copied();
        let f = match self.kinds[name] {
            HashKind::Record => emit_record_hash_body(name, registry, map, self_fn_idx, body)?,
            HashKind::Sum => emit_sum_hash_body(name, registry, map, self_fn_idx, body)?,
            HashKind::PackedSequence => emit_packed_sequence_hash_body(name, registry)?,
            // The steps of an `Option` hand its payload on: the records and
            // sums it holds count the budget and remember the hashes.
            HashKind::OptionHash => emit_option_hash_body(name, registry, map)?,
            HashKind::ResultHash => emit_result_hash_body(name, registry, map)?,
            HashKind::TupleHash => emit_tuple_hash_body(name, registry, map)?,
        };
        codes.function(&f);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{FNV1A_OFFSET_BASIS, FNV1A_PRIME, HashHelperRegistry, HashKind};

    fn packed_hash(bytes: &[u8]) -> u32 {
        bytes.iter().fold(FNV1A_OFFSET_BASIS, |acc, byte| {
            (acc ^ i32::from(*byte)).wrapping_mul(FNV1A_PRIME)
        }) as u32
    }

    #[test]
    fn slot_assignment_is_independent_of_discovery_order() {
        let mut left = HashHelperRegistry::new();
        left.register("Zulu", HashKind::Record);
        left.register("Alpha", HashKind::Record);
        let mut right = HashHelperRegistry::new();
        right.register("Alpha", HashKind::Record);
        right.register("Zulu", HashKind::Record);

        left.assign_slots(&mut 10, &mut 20);
        right.assign_slots(&mut 10, &mut 20);
        assert_eq!(left.lookup_fn_idx("Alpha"), right.lookup_fn_idx("Alpha"));
        assert_eq!(left.lookup_fn_idx("Zulu"), right.lookup_fn_idx("Zulu"));
        assert_eq!(
            left.lookup_type_idx("Alpha"),
            right.lookup_type_idx("Alpha")
        );
        assert_eq!(left.lookup_type_idx("Zulu"), right.lookup_type_idx("Zulu"));
    }

    #[test]
    fn packed_sequence_hash_uses_fnv1a() {
        assert_eq!(packed_hash(b""), 0x811c_9dc5);
        assert_eq!(packed_hash(b"a"), 0xe40c_292c);
        assert_eq!(packed_hash(b"foobar"), 0xbf9c_f968);
    }

    #[test]
    fn packed_sequence_hash_spreads_btc_style_keys_across_map_buckets() {
        const KEY_COUNT: u32 = 4096;
        const CAPACITY: usize = 8192;
        let mut occupied = vec![false; CAPACITY];

        for index in 1..=KEY_COUNT {
            let mut key = [0_u8; 37];
            key[0] = 117;
            key[29..33].copy_from_slice(&(index / 4).to_be_bytes());
            key[33..37].copy_from_slice(&(index % 4).to_be_bytes());
            occupied[packed_hash(&key) as usize & (CAPACITY - 1)] = true;
        }

        let distinct_buckets = occupied.into_iter().filter(|used| *used).count();
        assert!(
            distinct_buckets >= 3900,
            "btc-style packed keys occupied only {distinct_buckets}/{KEY_COUNT} buckets"
        );
    }
}

// FNV-1a keeps the low bits well mixed, which matters because Map's
// power-of-two table selects a bucket with `hash & (capacity - 1)`.
// DJB2's low bits clustered badly for fixed-width binary keys such as
// btc-listener's `tag || txid[32] || vout[4]`: issue #1203 measured long
// linear-probe runs even while the table was below half full.
const FNV1A_OFFSET_BASIS: i32 = 0x811c_9dc5_u32 as i32;
const FNV1A_PRIME: i32 = 0x0100_0193;

fn emit_packed_sequence_hash_body(
    name: &str,
    registry: &TypeRegistry,
) -> Result<Function, WasmGcError> {
    let packed = registry.packed_sequence(name).ok_or_else(|| {
        WasmGcError::Validation(format!("hash helper for packed `{name}`: type missing"))
    })?;
    let packed_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(packed.type_idx),
    });
    // 1 = typed array, 2 = len, 3 = index, 4 = accumulator.
    let mut f = Function::new(vec![(1, packed_ref), (3, wasm_encoder::ValType::I32)]);
    let heap = wasm_encoder::HeapType::Concrete(packed.type_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::I32Const(FNV1A_OFFSET_BASIS));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(3));
    match packed.layout.element {
        crate::codegen::proof_lower::PackedIntElement::U8
        | crate::codegen::proof_lower::PackedIntElement::I8
        | crate::codegen::proof_lower::PackedIntElement::U16
        | crate::codegen::proof_lower::PackedIntElement::I16 => {
            f.instruction(&Instruction::ArrayGetU(packed.type_idx));
        }
        crate::codegen::proof_lower::PackedIntElement::I64 => {
            f.instruction(&Instruction::ArrayGet(packed.type_idx));
            f.instruction(&Instruction::I32WrapI64);
        }
        _ => {
            f.instruction(&Instruction::ArrayGet(packed.type_idx));
        }
    }
    f.instruction(&Instruction::I32Xor);
    f.instruction(&Instruction::I32Const(FNV1A_PRIME));
    f.instruction(&Instruction::I32Mul);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for a record. Cast → typed, DJB2 fold over
/// fields, return.
fn emit_record_hash_body(
    name: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
    self_fn_idx: Option<u32>,
    body: Body,
) -> Result<Function, WasmGcError> {
    let r_idx = registry
        .record_type_idx(name)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for record `{name}`: not registered"
        )))?;
    let fields = registry
        .record_fields
        .get(name)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for record `{name}`: no fields"
        )))?;
    let r_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(r_idx),
    });
    // Locals: 1 = typed record ref, 2 = h accumulator.
    let mut f = Function::new(
        [
            vec![(1, r_ref), (1, wasm_encoder::ValType::I32)],
            body.locals(),
        ]
        .concat(),
    );
    body.prologue(&mut f);
    let r_heap = wasm_encoder::HeapType::Concrete(r_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(r_heap));
    f.instruction(&Instruction::LocalSet(1));
    let field_tys: Vec<String> = fields.iter().map(|(_, t)| t.clone()).collect();
    let wasm_tys = || -> Result<Vec<wasm_encoder::ValType>, WasmGcError> {
        fields
            .iter()
            .map(|(n, t)| super::super::types::record_field_val_type(name, n, t, registry))
            .collect()
    };
    let node = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(1));
    };
    body.recall(
        &mut f, 2, name, &field_tys, &wasm_tys, r_idx, registry, &node,
    )?;
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    for (i, (field_name, field_ty)) in fields.iter().enumerate() {
        // h = h * 33 + field_hash
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: r_idx,
            field_index: i as u32,
        });
        // ETAP-2 multi-field carrier-`i64`: a bounded Int field erased to a
        // native `i64` hashes with the raw `i32.wrap_i64` (agreeing with the
        // `i64.eq` the record's eq helper picks for the same field); a boxed
        // Int field falls through to `emit_inner_hash_dispatch` (→ `__aint_hash`
        // under ℤ). An `__aint_hash` on a scalar `i64` is invalid wasm.
        if field_ty.trim() == "Int" && registry.is_eligible_carrier_field(name, field_name) {
            f.instruction(&Instruction::I32WrapI64);
        } else {
            emit_inner_hash_dispatch(
                &mut f,
                field_ty.trim(),
                registry,
                helper_idx_map,
                self_fn_idx.filter(|_| field_ty.trim() == name),
            )?;
        }
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(2));
    }
    body.finish(&mut f, 2);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for a sum. ref.test cascade per variant;
/// matched arm folds tag idx + variant fields.
fn emit_sum_hash_body(
    parent_name: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
    self_fn_idx: Option<u32>,
    body: Body,
) -> Result<Function, WasmGcError> {
    let mut variants: Vec<(String, super::super::types::VariantInfo)> = registry
        .variants
        .iter()
        .flat_map(|(n, vs)| vs.iter().map(move |v| (n.clone(), v.clone())))
        .filter(|(_, v)| v.parent == parent_name)
        .collect();
    variants.sort_by(|a, b| a.0.cmp(&b.0));
    if variants.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "hash helper for sum `{parent_name}`: no variants"
        )));
    }
    // Locals: 1 = h accumulator. (Per-variant typed cast happens
    // inside the if-arm via `RefCastNonNull` → `StructGet`; we read
    // each field directly off the casted ref so no scratch slot
    // for the variant ref is needed.)
    let mut f = Function::new([vec![(1, wasm_encoder::ValType::I32)], body.locals()].concat());
    body.prologue(&mut f);
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(1));
    for (_v_name, info) in &variants {
        let v_idx = info.type_idx;
        let v_heap = wasm_encoder::HeapType::Concrete(v_idx);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        let wasm_tys = || -> Result<Vec<wasm_encoder::ValType>, WasmGcError> {
            info.fields
                .iter()
                .map(|t| {
                    Ok(super::super::types::aver_to_wasm(t, Some(registry))?
                        .unwrap_or(wasm_encoder::ValType::I32))
                })
                .collect()
        };
        let node = |f: &mut Function| {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::RefCastNonNull(v_heap));
        };
        body.recall(
            &mut f,
            1,
            parent_name,
            &info.fields,
            &wasm_tys,
            v_idx,
            registry,
            &node,
        )?;
        // Mix variant tag (its type_idx) into hash.
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::I32Const(v_idx as i32));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(1));
        for (i, field_ty) in info.fields.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Const(5));
            f.instruction(&Instruction::I32Shl);
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::RefCastNonNull(v_heap));
            f.instruction(&Instruction::StructGet {
                struct_type_index: v_idx,
                field_index: i as u32,
            });
            emit_inner_hash_dispatch(
                &mut f,
                field_ty.trim(),
                registry,
                helper_idx_map,
                self_fn_idx.filter(|_| field_ty.trim() == parent_name),
            )?;
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(1));
        }
        f.instruction(&Instruction::End);
    }
    body.finish(&mut f, 1);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for `Option<X>`. h held in local 2 across
/// the if-arm so the block body can be empty-typed (no stack-shape
/// constraint). DJB2-fold tag, then if Some fold inner hash too.
fn emit_option_hash_body(
    canonical: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let opt_idx = registry
        .option_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for `{canonical}`: option not registered"
        )))?;
    let inner = TypeRegistry::option_element_type(canonical).ok_or(WasmGcError::Validation(
        format!("hash helper for `{canonical}`: can't parse inner"),
    ))?;
    let opt_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
    });
    // Locals: 1 = typed Option ref, 2 = h.
    let mut f = Function::new(vec![(1, opt_ref), (1, wasm_encoder::ValType::I32)]);
    let opt_heap = wasm_encoder::HeapType::Concrete(opt_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(opt_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    // h = h * 33 + tag
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    // if Some (tag != 0), mix inner hash into h.
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    emit_inner_hash_dispatch(&mut f, inner.trim(), registry, helper_idx_map, None)?;
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for `Result<X, Y>`. h held in local 2.
fn emit_result_hash_body(
    canonical: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for `{canonical}`: result not registered"
        )))?;
    let (ok_inner, err_inner) = parse_result_kv(canonical).ok_or(WasmGcError::Validation(
        format!("hash helper for `{canonical}`: can't parse inner"),
    ))?;
    let res_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_idx),
    });
    let mut f = Function::new(vec![(1, res_ref), (1, wasm_encoder::ValType::I32)]);
    let res_heap = wasm_encoder::HeapType::Concrete(res_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(res_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    // h = h * 33 + tag
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    // Branch on tag.
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Ok arm: mix field 1 (ok) into h.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 1,
    });
    emit_inner_hash_dispatch(&mut f, ok_inner.trim(), registry, helper_idx_map, None)?;
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Else);
    // Err arm: mix field 2 (err) into h.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 2,
    });
    emit_inner_hash_dispatch(&mut f, err_inner.trim(), registry, helper_idx_map, None)?;
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for `Tuple<A, B, C, …>`. DJB2 fold per-elem.
fn emit_tuple_hash_body(
    canonical: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let tup_idx = registry
        .tuple_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for `{canonical}`: tuple not registered"
        )))?;
    let elems = parse_tuple_elems(canonical).ok_or(WasmGcError::Validation(format!(
        "hash helper for `{canonical}`: can't parse elements"
    )))?;
    let tup_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(tup_idx),
    });
    let mut f = Function::new(vec![(1, tup_ref), (1, wasm_encoder::ValType::I32)]);
    let tup_heap = wasm_encoder::HeapType::Concrete(tup_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(tup_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    for (i, elem) in elems.iter().enumerate() {
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: tup_idx,
            field_index: i as u32,
        });
        emit_inner_hash_dispatch(&mut f, elem.trim(), registry, helper_idx_map, None)?;
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(2));
    }
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Stack: `[value]` of `inner` aver type. Push `i32` hash. Mirror
/// of `eq_helpers::emit_inner_eq_dispatch` but for hash.
fn emit_inner_hash_dispatch(
    f: &mut Function,
    inner: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
    self_fn_idx: Option<u32>,
) -> Result<(), WasmGcError> {
    // ETAP-2 carrier-`i64`: an eligible carrier field/payload is a native
    // `i64`, so its hash is a raw `i32.wrap_i64` — NOT `__aint_hash` (a ref
    // helper). Must agree with the carrier eq (`i64.eq`) so equal carriers
    // hash equal. Check before the newtype resolution.
    if registry.is_eligible_carrier(inner) {
        f.instruction(&Instruction::I32WrapI64);
        return Ok(());
    }
    let resolved: String = if let Some(under) = registry.newtype_underlying(inner) {
        under.to_string()
    } else {
        inner.to_string()
    };
    match resolved.as_str() {
        // `Int = ℤ` — an `Int` payload lowers to the `$aint` ref under
        // bignum → `__aint_hash` (agrees with `__aint_eq`; an
        // `i32.wrap_i64` on a ref is invalid wasm). This holds for BOTH a
        // genuine `Int` field and a newtype-erased Int (`Box(v: Int)`):
        // the erased field's wasm representation is still `$aint`.
        // `emit_aint_field_hash` itself falls to `i32.wrap_i64` when no
        // Int is reachable (bignum off).
        "Int" => {
            super::super::lists::emit_aint_field_hash(f, registry)?;
        }
        "Bool" => {} // already i32
        "Float" => {
            f.instruction(&Instruction::I64ReinterpretF64);
            f.instruction(&Instruction::I32WrapI64);
        }
        // A step of a hash in steps hashes a string by its bytes (see
        // `HashHelperRegistry::emit_helper_bodies`); the others by its length.
        "String" => match helper_idx_map.get("String") {
            Some(&bytes) => {
                f.instruction(&Instruction::Call(bytes));
            }
            None => {
                f.instruction(&Instruction::ArrayLen);
            }
        },
        other if Some(other) == self_fn_idx.and(Some(inner)) => {
            // Self-recursive case — caller passed `Some(self_fn_idx)`
            // when the field is a recursive ref to the parent.
            let idx = self_fn_idx.unwrap();
            f.instruction(&Instruction::Call(idx));
        }
        other if helper_idx_map.contains_key(other) => {
            f.instruction(&Instruction::Call(helper_idx_map[other]));
        }
        _other => {
            // Last-resort fallback (newtype-erased through to a
            // non-eq-able underlying, or a shape we genuinely can't
            // resolve). Drop the value, contribute 0. Same
            // collision-tolerant degradation the older inline
            // emitters used.
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(0));
        }
    }
    Ok(())
}

/// `Result<Ok, Err>` → `Some(("Ok", "Err"))`. Tracks angle/paren
/// depth so `Result<Map<K,V>, MyError>` splits at the right comma.
fn parse_result_kv(canonical: &str) -> Option<(&str, &str)> {
    let inner = canonical
        .trim()
        .strip_prefix("Result<")?
        .strip_suffix('>')?;
    let bytes = inner.as_bytes();
    let mut depth: i32 = 0;
    for (idx, b) in bytes.iter().enumerate() {
        match b {
            b'<' | b'(' => depth += 1,
            b'>' | b')' => depth -= 1,
            b',' if depth == 0 => {
                return Some((inner[..idx].trim(), inner[idx + 1..].trim()));
            }
            _ => {}
        }
    }
    None
}

/// `Tuple<A, B, C>` → `Some(vec!["A", "B", "C"])`.
fn parse_tuple_elems(canonical: &str) -> Option<Vec<&str>> {
    let inner = canonical.trim().strip_prefix("Tuple<")?.strip_suffix('>')?;
    let bytes = inner.as_bytes();
    let mut depth: i32 = 0;
    let mut start = 0;
    let mut out = Vec::new();
    for (idx, b) in bytes.iter().enumerate() {
        match b {
            b'<' | b'(' => depth += 1,
            b'>' | b')' => depth -= 1,
            b',' if depth == 0 => {
                out.push(inner[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
    }
    out.push(inner[start..].trim());
    Some(out)
}
