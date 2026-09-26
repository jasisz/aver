//! The emitter's view the certificate plan printer reads
//! (`crate::codegen::cert::PlanLayout`): the type registry, the symbol table,
//! the function index map and the interned builtin names of the SAME compile,
//! so every index a printed plan cites is the one the emitter wrote.

use std::collections::HashMap;

use crate::codegen::cert::{PlanLayout, RecordLayout, SumLayout};
use crate::ir::{BuiltinId, CtorId, FnId, SymbolTable};

use super::types::TypeRegistry;

pub(super) struct CertLayout<'a> {
    pub(super) registry: &'a TypeRegistry,
    pub(super) fn_map: &'a super::body::FnMap,
    pub(super) symbol_table: &'a SymbolTable,
    pub(super) fn_idx: HashMap<FnId, u32>,
    pub(super) builtins: &'a [String],
}

impl CertLayout<'_> {
    /// The sum type entry declared under `name` (exact, then the bare name),
    /// when exactly one sum carries it.
    fn sum_entry(&self, name: &str) -> Option<&crate::ir::TypeEntry> {
        let find = |wanted: &str| {
            let mut hits = self
                .symbol_table
                .types
                .iter()
                .filter(|t| !t.is_product && !t.variants.is_empty() && t.key.name == wanted);
            let first = hits.next()?;
            hits.next().is_none().then_some(first)
        };
        find(name).or_else(|| name.rsplit_once('.').and_then(|(_, bare)| find(bare)))
    }
}

impl PlanLayout for CertLayout<'_> {
    fn carrier(&self) -> Option<u32> {
        if self.registry.bignum {
            self.registry.aint_struct_idx
        } else {
            None
        }
    }

    fn mag(&self) -> Option<u32> {
        self.registry.aint_mag_array_idx
    }

    fn string_array(&self) -> Option<u32> {
        self.registry.string_array_type_idx
    }

    fn string_segment(&self, bytes: &[u8]) -> Option<u32> {
        self.registry.string_literal_segment(bytes)
    }

    fn option(&self, canonical: &str) -> Option<u32> {
        self.registry.option_type_idx(canonical)
    }

    fn result(&self, canonical: &str) -> Option<u32> {
        self.registry.result_type_idx(canonical)
    }

    fn vector(&self, canonical: &str) -> Option<u32> {
        self.registry.vector_type_idx(canonical)
    }

    fn list(&self, canonical: &str) -> Option<u32> {
        self.registry.list_type_idx(canonical)
    }

    fn list_cons(&self, canonical: &str) -> Option<u32> {
        self.fn_map.list_ops_lookup(canonical).map(|ops| ops.cons)
    }

    fn tuple(&self, canonical: &str) -> Option<u32> {
        self.registry.tuple_type_idx(canonical)
    }

    fn map(&self, canonical: &str) -> Option<u32> {
        self.registry.map_slots(canonical).map(|slots| slots.map)
    }

    fn special(&self, name: &str) -> bool {
        let registry = self.registry;
        registry.packed_sequence(name).is_some()
            || registry.is_eligible_carrier(name)
            || registry.is_capability_resource(name)
            || (self.sum_entry(name).is_some() && registry.newtype_underlying(name).is_some())
    }

    fn record(&self, name: &str) -> Option<RecordLayout> {
        let registry = self.registry;
        let canonical = registry.canonical_type_name(name);
        let (key, fields) = registry
            .record_fields
            .get_key_value(canonical)
            .or_else(|| {
                canonical
                    .rsplit_once('.')
                    .and_then(|(_, bare)| registry.record_fields.get_key_value(bare))
            })?;
        if fields
            .iter()
            .any(|(field, _)| registry.is_eligible_carrier_field(canonical, field))
        {
            return None;
        }
        let struct_idx = if registry.newtype_underlying(canonical).is_some() {
            None
        } else {
            Some(registry.record_type_idx(canonical)?)
        };
        Some(RecordLayout {
            key: key.clone(),
            struct_idx,
            fields: fields.clone(),
        })
    }

    fn sum(&self, name: &str) -> Option<SumLayout> {
        let registry = self.registry;
        let canonical = registry.canonical_type_name(name);
        let entry = self.sum_entry(canonical)?;
        let parent = entry.key.name.as_str();
        let root = registry.sum_root_type_idx(parent)?;
        let mut ctors = Vec::with_capacity(entry.variants.len());
        for ctor in &entry.variants {
            let ctor_name = self.symbol_table.ctor_entry(*ctor).name.as_str();
            let info = registry.variant_in(parent, ctor_name)?;
            ctors.push((info.type_idx, info.fields.clone()));
        }
        Some(SumLayout {
            key: parent.to_string(),
            root,
            ctors,
        })
    }

    fn user_ctor(&self, ctor: CtorId) -> Option<(String, u32)> {
        let entry = self.symbol_table.ctor_entry(ctor);
        let owner = self.symbol_table.type_entry(entry.owning_type);
        if owner.is_product {
            return None;
        }
        let index = owner.variants.iter().position(|c| *c == ctor)?;
        Some((owner.key.name.clone(), index as u32))
    }

    fn fn_idx(&self, f: FnId) -> Option<u32> {
        self.fn_idx.get(&f).copied()
    }

    fn builtin_name(&self, b: BuiltinId) -> Option<String> {
        self.builtins.get(b.0 as usize).cloned()
    }
}
