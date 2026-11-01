//! The types `==` reaches on wasm-gc, and the few comparisons still
//! written in bytes.
//!
//! Every record, sum, `Option`, `Result` and tuple that a `==`, a
//! collection helper or a map key compares is registered here, with every
//! type its fields reach (`register_transitive`). Those types are compared
//! by functions of the program (`eq_fns`); this registry only places them
//! (`use_functions`) so that the byte helpers of lists, vectors and maps
//! call them by index, and declares the table of remembered pairs their
//! comparisons of recursive values use (`node_table::Entry::Memo`).
//!
//! A packed sequence (a record whose `List<Int>` is held as one packed
//! array, `PackedSequenceType`) keeps a helper written here: MIR has no node
//! that reads an element of the packed array without building the list
//! again, so its comparison of lengths and elements stays in bytes, as the
//! other packed `Bytes` helpers do.

use std::collections::{HashMap, HashSet};

use wasm_encoder::{Function, Instruction};

use super::super::WasmGcError;
use super::super::types::TypeRegistry;
use super::node_table::{Entry, TableGlobals, TableSlots};

/// What kind of type a registered eq helper covers. Records and
/// sums are nominal (registry name = type name). The generic-
/// carrier kinds (Option / Result / Tuple) carry their canonical
/// instantiation string as the registry name (e.g. `"Option<Int>"`,
/// `"Result<Int,String>"`, `"Tuple<Int,String>"`) — one helper slot
/// per concrete inner shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EqKind {
    Record,
    Sum,
    PackedSequence,
    OptionEq,
    ResultEq,
    TupleEq,
}

/// Per-module registry of the types `==` reaches (see the module doc).
#[derive(Default)]
pub(crate) struct EqHelperRegistry {
    /// Insertion order — wasm fn indices follow it.
    order: Vec<String>,
    kinds: HashMap<String, EqKind>,
    /// `type_name -> (wasm_fn_idx, wasm_type_idx)` of a packed sequence's
    /// helper.
    slots: HashMap<String, (u32, u32)>,
    /// The types that reach themselves through records, sums and `Option`s
    /// held directly (`self_reaching`).
    walks: Vec<String>,
    /// The table of the remembered pairs, when a function walks.
    table: Option<TableSlots>,
    /// The types compared by a function of the program (`eq_fns`), each with
    /// its position among the program's functions; they get no helper here.
    functions: HashMap<String, u32>,
    /// The wasm index of the program's first function, once known.
    functions_base: Option<u32>,
    /// Whether a function of the program walks a type that reaches itself,
    /// so the table of remembered pairs (`Entry::Memo`) is needed.
    functions_walk: bool,
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

/// The registered helpers a field of type `field_ty` compares through:
/// the field's own type and every type named inside it.
fn field_helpers<'a>(field_ty: &str, kinds: &'a HashMap<String, EqKind>) -> Vec<&'a str> {
    let whole = super::super::types::normalize_compound(field_ty);
    let inner = field_ty.split(|c: char| !(c.is_alphanumeric() || c == '_'));
    std::iter::once(whole.as_str())
        .chain(inner)
        .filter_map(|name| kinds.get_key_value(name).map(|(k, _)| k.as_str()))
        .collect()
}

/// The helpers a step of `kind` hands its field of type `field_ty` to:
/// a record, a sum or an `Option` held directly.
fn walk_edge<'a>(
    field_ty: &str,
    kinds: &'a HashMap<String, EqKind>,
    registry: &TypeRegistry,
) -> Option<&'a str> {
    let declared = field_ty.trim();
    if registry.is_eligible_carrier(declared) {
        return None;
    }
    let resolved = registry.newtype_underlying(declared).unwrap_or(declared);
    let key = super::super::types::normalize_compound(resolved);
    kinds
        .get_key_value(key.as_str())
        .filter(|(_, kind)| matches!(kind, EqKind::Record | EqKind::Sum | EqKind::OptionEq))
        .map(|(k, _)| k.as_str())
}

/// The helpers of `order` whose type reaches itself through records, sums
/// and `Option`s held directly, in the order of `order`. The hash helpers
/// pick theirs by the same rule (`hash_helpers`).
pub(crate) fn self_reaching(
    order: &[String],
    kinds: &HashMap<String, EqKind>,
    registry: &TypeRegistry,
) -> Vec<String> {
    let edges: HashMap<&str, Vec<&str>> = order
        .iter()
        .map(|name| {
            let kind = kinds[name];
            let fields = match kind {
                EqKind::Record | EqKind::Sum | EqKind::OptionEq => {
                    helper_field_types(name, kind, registry)
                }
                _ => Vec::new(),
            };
            let to = fields
                .iter()
                .filter_map(|t| walk_edge(t, kinds, registry))
                .collect();
            (name.as_str(), to)
        })
        .collect();
    let reaches_itself = |name: &str| {
        let mut seen = HashSet::new();
        let mut todo = edges[name].clone();
        while let Some(next) = todo.pop() {
            if next == name {
                return true;
            }
            if seen.insert(next) {
                todo.extend(edges[next].iter().copied());
            }
        }
        false
    };
    order
        .iter()
        .filter(|name| reaches_itself(name))
        .cloned()
        .collect()
}

/// The helpers a value of one of the types `roots` reaches through its
/// fields, in collections too.
pub(crate) fn reached_from(
    roots: &[String],
    kinds: &HashMap<String, EqKind>,
    registry: &TypeRegistry,
) -> HashSet<String> {
    let mut seen = HashSet::new();
    let mut todo: Vec<&str> = roots
        .iter()
        .flat_map(|root| field_helpers(root, kinds))
        .collect();
    while let Some(next) = todo.pop() {
        if seen.insert(next.to_string()) {
            for field in helper_field_types(next, kinds[next], registry) {
                todo.extend(field_helpers(&field, kinds));
            }
        }
    }
    seen
}

/// The declared types of the fields of helper `name`.
pub(crate) fn helper_field_types(name: &str, kind: EqKind, registry: &TypeRegistry) -> Vec<String> {
    match kind {
        EqKind::Record => registry
            .record_fields
            .get(name)
            .map(|fs| fs.iter().map(|(_, t)| t.clone()).collect())
            .unwrap_or_default(),
        EqKind::Sum => registry
            .variants
            .values()
            .flat_map(|vs| vs.iter())
            .filter(|v| v.parent == name)
            .flat_map(|v| v.fields.iter().cloned())
            .collect(),
        EqKind::PackedSequence => Vec::new(),
        EqKind::OptionEq => TypeRegistry::option_element_type(name)
            .map(|t| vec![t.to_string()])
            .unwrap_or_default(),
        EqKind::ResultEq => parse_result_kv(name)
            .map(|(ok, err)| vec![ok.to_string(), err.to_string()])
            .unwrap_or_default(),
        EqKind::TupleEq => parse_tuple_elems(name)
            .map(|es| es.into_iter().map(str::to_string).collect())
            .unwrap_or_default(),
    }
}

impl EqHelperRegistry {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn register(&mut self, type_name: &str, kind: EqKind) {
        if !self.kinds.contains_key(type_name) {
            self.order.push(type_name.to_string());
            self.kinds.insert(type_name.to_string(), kind);
        }
    }

    /// Register `type_name` and recursively register every nominal
    /// field type it transitively reaches. Needed because the
    /// emitted `__eq_<X>` body dispatches nested record/sum fields
    /// by `Call(__eq_<FieldType>)` — those helper slots have to
    /// exist even if the user never wrote `field == field` directly.
    /// `register` is idempotent on cycle (Tree → Tree.Node → Tree)
    /// so the recursion terminates.
    pub(crate) fn register_transitive(
        &mut self,
        type_name: &str,
        kind: EqKind,
        registry: &TypeRegistry,
    ) {
        if self.kinds.contains_key(type_name) {
            return;
        }
        // ETAP-2 carrier-`i64`: an eligible carrier is `i64`-erased — no
        // struct, no per-type `__eq_<Carrier>` helper (its eq inlines as raw
        // `i64.eq` at the use site). A struct `__eq_` body over an `i64` is
        // invalid wasm. Backstop covering every caller path.
        if registry.is_eligible_carrier(type_name) {
            return;
        }
        let kind = if registry.packed_sequence(type_name).is_some() {
            EqKind::PackedSequence
        } else {
            kind
        };
        // Skip nominal types whose fields contain unhandled shapes
        // (List/Map/Vector/Set inside fields — the inline eq emitter
        // covers Option/Result/Tuple via per-instantiation helpers
        // registered alongside the parent below). Without this guard
        // `register_field_type` silently dropped the unsupported
        // field and the helper body emit later panicked with "no eq
        // dispatch".
        let mut seen = std::collections::HashSet::new();
        let resolvable = match kind {
            EqKind::Record => {
                super::super::lists::record_fields_resolvable(type_name, registry, &mut seen)
            }
            EqKind::Sum => {
                super::super::lists::sum_fields_resolvable(type_name, registry, &mut seen)
            }
            EqKind::PackedSequence => true,
            // Carrier kinds — Option<X>/Result/Tuple have one
            // shape regardless of inner; resolvability is checked at
            // the record/sum level via `field_type_resolvable`.
            EqKind::OptionEq | EqKind::ResultEq | EqKind::TupleEq => true,
        };
        if !resolvable {
            return;
        }
        self.register(type_name, kind);
        // Walk fields and recurse on nominal types.
        match kind {
            EqKind::Record => {
                if let Some(fields) = registry.record_fields.get(type_name) {
                    for (_, field_ty) in fields {
                        self.register_field_type(field_ty.trim(), registry);
                    }
                }
            }
            EqKind::Sum => {
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
            EqKind::PackedSequence => {}
            // Carrier kinds — recurse into inner types so a direct
            // top-level register (e.g. discovery seed walker hitting
            // `Option<PieceKind>` from a `List<Option<PieceKind>>`)
            // still registers PieceKind. When the carrier is reached
            // via `register_field_type` the recursion happens there
            // too; this duplicate is idempotent (slots dedup by
            // canonical).
            EqKind::OptionEq => {
                if let Some(inner) = type_name
                    .strip_prefix("Option<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    self.register_field_type(inner.trim(), registry);
                }
            }
            EqKind::ResultEq => {
                if let Some((ok, err)) = parse_result_kv(type_name) {
                    self.register_field_type(ok.trim(), registry);
                    self.register_field_type(err.trim(), registry);
                }
            }
            EqKind::TupleEq => {
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
        // jasisz/aver#1329 — the job handle compares as a reference inline,
        // exactly like a primitive; it gets no `__eq_` helper of its own.
        if field_ty == crate::capability::work::WORK_JOB {
            return;
        }
        // ETAP-2 carrier-`i64`: an eligible carrier is erased to a native
        // `i64`, so it has NO struct representation and needs NO per-type
        // `__eq_<Carrier>` helper — its eq is the raw `i64.eq` inlined at the
        // use site (the carrier-`i64` arms in the inline eq emitters). Treat
        // it exactly like a primitive: register nothing. Registering a helper
        // would emit a body that `struct.get`s a value that is actually an
        // `i64` (the func-33 validation failure this guards).
        if registry.is_eligible_carrier(field_ty) {
            return;
        }
        if registry.packed_sequence(field_ty).is_some() {
            self.register_transitive(field_ty, EqKind::PackedSequence, registry);
            return;
        }
        if registry.record_fields.contains_key(field_ty) {
            self.register_transitive(field_ty, EqKind::Record, registry);
            return;
        }
        if registry
            .variants
            .values()
            .flat_map(|v| v.iter())
            .any(|v| v.parent == field_ty)
        {
            self.register_transitive(field_ty, EqKind::Sum, registry);
            return;
        }
        // Generic carriers — `Option<X>`, `Result<X,Y>`, `Tuple<…>`.
        // Each instantiation gets its own helper slot keyed by the
        // canonical string. Inner types are walked too so any
        // nominal piece (`Option<Color>` → register Color).
        if let Some(inner) = field_ty
            .strip_prefix("Option<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.register_transitive(field_ty, EqKind::OptionEq, registry);
            self.register_field_type(inner.trim(), registry);
        } else if field_ty.starts_with("Result<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, EqKind::ResultEq, registry);
            if let Some((ok, err)) = parse_result_kv(field_ty) {
                self.register_field_type(ok.trim(), registry);
                self.register_field_type(err.trim(), registry);
            }
        } else if field_ty.starts_with("Tuple<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, EqKind::TupleEq, registry);
            if let Some(elems) = parse_tuple_elems(field_ty) {
                for elem in elems {
                    self.register_field_type(elem.trim(), registry);
                }
            }
        } else if let Some(inner) = field_ty
            .strip_prefix("List<")
            .and_then(|s| s.strip_suffix('>'))
        {
            // List<X> / Vector<X> — element kind needs its own
            // helper too. The list_helpers slot itself is owned by
            // ListHelperRegistry (separate registry); this walk
            // ensures the per-element `__eq_<inner>` exists in
            // eq_helpers when inner is a carrier or nominal type.
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
            // Map<K,V> — eq+hash slots live in MapHelperRegistry,
            // not eq_helpers. This walk only ensures K and V's own
            // helpers exist (K may be a record/sum/carrier; V same).
            // Structural eq on the map dispatches K and V via Call
            // into those helpers.
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

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&str, EqKind)> + '_ {
        self.order.iter().map(|n| (n.as_str(), self.kinds[n]))
    }

    /// The types `fns` compares become functions of the program, at
    /// `position` among its functions: their helpers here are not written.
    pub(crate) fn use_functions(
        &mut self,
        fns: &crate::codegen::wasm_gc::eq_fns::EqFns,
        position: impl Fn(crate::ir::FnId) -> u32,
    ) {
        // Under its canonical spelling and every spelling registered here.
        let by_canonical: HashMap<&String, u32> = fns
            .entry
            .iter()
            .chain(&fns.lists)
            .map(|(name, id)| (name, position(*id)))
            .collect();
        self.functions = by_canonical
            .iter()
            .map(|(name, pos)| ((*name).clone(), *pos))
            .collect();
        for name in &self.order {
            let canonical = super::super::types::normalize_compound(name.trim());
            if let Some(pos) = by_canonical.get(&canonical) {
                self.functions.insert(name.clone(), *pos);
            }
        }
        self.functions_walk = fns.walks();
    }

    /// The wasm index of the program's first function.
    pub(crate) fn set_functions_base(&mut self, base: u32) {
        self.functions_base = Some(base);
    }

    /// The table of remembered pairs the functions of the program use.
    pub(crate) fn memo_table(&self) -> Option<TableSlots> {
        self.table
    }

    pub(crate) fn assign_slots(&mut self, next_fn_idx: &mut u32, next_type_idx: &mut u32) {
        // Canonical names order the helpers, so the same source always gets
        // the same function and type indices (and Wasmtime's cache hits).
        self.order.sort_unstable();
        for name in &self.order {
            if self.kinds[name] == EqKind::PackedSequence {
                self.slots
                    .insert(name.clone(), (*next_fn_idx, *next_type_idx));
                *next_fn_idx += 1;
                *next_type_idx += 1;
            }
        }
        if self.functions_walk {
            self.table = Some(TableSlots::assign(Entry::Memo, next_fn_idx, next_type_idx));
        }
    }

    /// Picks the helpers whose type reaches itself through records, sums
    /// and `Option`s held directly: each is compared in steps. Runs once
    /// every helper is registered.
    pub(crate) fn find_walks(&mut self, registry: &TypeRegistry) {
        self.order.sort_unstable();
        self.walks = self_reaching(&self.order, &self.kinds, registry);
    }

    /// Whether the helper of `name` compares in steps.
    pub(crate) fn walks(&self, name: &str) -> bool {
        self.walks.iter().any(|walked| walked == name)
    }

    /// Whether `==` on `name` is reflexive (`reflexive`), for the functions
    /// of the program (`eq_fns`).
    pub(crate) fn reflexive_type(&self, name: &str, registry: &TypeRegistry) -> bool {
        // `name` is a canonical spelling; the registry may hold another.
        self.order
            .iter()
            .find(|k| super::super::types::normalize_compound(k.trim()) == name)
            .is_some_and(|k| self.reflexive(k, registry))
    }

    /// Whether `==` on `name` is reflexive: no `Float` is named in any
    /// field type it compares through, its own or a nested one's.
    fn reflexive(&self, name: &str, registry: &TypeRegistry) -> bool {
        let mut seen = HashSet::from([name]);
        let mut todo = vec![name];
        while let Some(next) = todo.pop() {
            for field in helper_field_types(next, self.kinds[next], registry) {
                if field.contains("Float") {
                    return false;
                }
                for helper in field_helpers(&field, &self.kinds) {
                    if seen.insert(helper) {
                        todo.push(helper);
                    }
                }
            }
        }
        true
    }

    /// The type idx of every helper function, in function index order.
    pub(crate) fn function_types(&self) -> Vec<u32> {
        let mut out: Vec<u32> = self
            .order
            .iter()
            .filter_map(|n| self.slots.get(n).map(|s| s.1))
            .collect();
        if let Some(table) = &self.table {
            out.extend(table.function_types());
        }
        out
    }

    /// Declares the table of the remembered pairs when a helper walks.
    pub(crate) fn emit_walk_globals(
        &self,
        globals: &mut wasm_encoder::GlobalSection,
        next_global_idx: &mut u32,
    ) -> Option<TableGlobals> {
        Some(self.table?.emit_globals(globals, next_global_idx))
    }

    pub(crate) fn lookup_fn_idx(&self, type_name: &str) -> Option<u32> {
        if let Some(position) = self.functions.get(type_name) {
            return Some(
                self.functions_base
                    .expect("the program's functions are placed before helpers are looked up")
                    + position,
            );
        }
        self.slots.get(type_name).map(|(f, _)| *f)
    }

    #[cfg(test)]
    pub(crate) fn lookup_type_idx(&self, type_name: &str) -> Option<u32> {
        self.slots.get(type_name).map(|(_, t)| *t)
    }

    /// Emits `(ref null eq, ref null eq) -> i32` fn type for each
    /// registered helper, in the same order as `assign_slots`.
    pub(crate) fn emit_helper_types(&self, types: &mut wasm_encoder::TypeSection) {
        let eq_ref = eq_ref_type();
        let i32 = wasm_encoder::ValType::I32;
        for name in &self.order {
            if self.slots.contains_key(name) {
                types.ty().function([eq_ref, eq_ref], [i32]);
            }
        }
        if let Some(table) = &self.table {
            table.emit_types(types);
        }
    }

    /// Emits the packed sequences' helpers, then the bodies of the table of
    /// remembered pairs.
    pub(crate) fn emit_helper_bodies(
        &self,
        codes: &mut wasm_encoder::CodeSection,
        registry: &TypeRegistry,
        walk_globals: Option<&TableGlobals>,
    ) -> Result<(), WasmGcError> {
        for name in &self.order {
            if self.slots.contains_key(name) {
                codes.function(&emit_packed_sequence_eq_body(name, registry)?);
            }
        }
        if let Some(table) = self.table {
            let globals = walk_globals.ok_or_else(|| {
                WasmGcError::Validation(
                    "eq functions walk but the remembered pairs were not declared".into(),
                )
            })?;
            for f in table.emit_bodies(globals)? {
                codes.function(&f);
            }
        }
        Ok(())
    }

    /// True when at least one helper needs `__wasmgc_string_eq` (for
    /// String fields). Used by module assembly to force-register the
    /// builtin slot.
    pub(crate) fn needs_string_eq(&self, registry: &TypeRegistry) -> bool {
        let mut visiting: HashSet<String> = HashSet::new();
        for name in &self.order {
            if type_has_string_field(name, self.kinds[name], registry, &mut visiting) {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::{EqHelperRegistry, EqKind};

    #[test]
    fn slot_assignment_is_independent_of_discovery_order() {
        let mut left = EqHelperRegistry::new();
        left.register("Zulu", EqKind::PackedSequence);
        left.register("Alpha", EqKind::PackedSequence);
        let mut right = EqHelperRegistry::new();
        right.register("Alpha", EqKind::PackedSequence);
        right.register("Zulu", EqKind::PackedSequence);

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

/// `Tuple<A, B, C>` → `Some(vec!["A", "B", "C"])`. Same depth-aware
/// split as `parse_result_kv`.
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

fn type_has_string_field(
    name: &str,
    kind: EqKind,
    registry: &TypeRegistry,
    visiting: &mut HashSet<String>,
) -> bool {
    if !visiting.insert(name.to_string()) {
        return false;
    }
    match kind {
        EqKind::Record => registry
            .record_fields
            .get(name)
            .map(|fs| fs.iter().any(|(_, t)| t.trim() == "String"))
            .unwrap_or(false),
        EqKind::Sum => registry
            .variants
            .values()
            .flat_map(|vs| vs.iter())
            .filter(|v| v.parent == name)
            .any(|v| v.fields.iter().any(|t| t.trim() == "String")),
        EqKind::PackedSequence => false,
        // Option<X>/Result<X,Y>/Tuple<…> — check whether any inner
        // type is `String`. Cheap string match on the canonical
        // (e.g. `"Option<String>"` contains `"String"`); accurate
        // enough since we just need to force-register
        // `__wasmgc_string_eq` when it might be called.
        EqKind::OptionEq | EqKind::ResultEq | EqKind::TupleEq => name.contains("String"),
    }
}

fn emit_packed_sequence_eq_body(
    name: &str,
    registry: &TypeRegistry,
) -> Result<Function, WasmGcError> {
    let packed = registry.packed_sequence(name).ok_or_else(|| {
        WasmGcError::Validation(format!("eq helper for packed `{name}`: type missing"))
    })?;
    let packed_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(packed.type_idx),
    });
    let mut f = Function::new(vec![(2, packed_ref), (2, wasm_encoder::ValType::I32)]);
    let heap = wasm_encoder::HeapType::Concrete(packed.type_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(heap));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefCastNonNull(heap));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    for local in [2, 3] {
        f.instruction(&Instruction::LocalGet(local));
        f.instruction(&Instruction::LocalGet(5));
        match packed.layout.element {
            crate::codegen::proof_lower::PackedIntElement::U8
            | crate::codegen::proof_lower::PackedIntElement::I8
            | crate::codegen::proof_lower::PackedIntElement::U16
            | crate::codegen::proof_lower::PackedIntElement::I16 => {
                f.instruction(&Instruction::ArrayGetU(packed.type_idx));
            }
            _ => {
                f.instruction(&Instruction::ArrayGet(packed.type_idx));
            }
        }
    }
    if matches!(
        packed.layout.element,
        crate::codegen::proof_lower::PackedIntElement::I64
    ) {
        f.instruction(&Instruction::I64Ne);
    } else {
        f.instruction(&Instruction::I32Ne);
    }
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::End);
    Ok(f)
}
