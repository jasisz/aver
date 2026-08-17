use super::*;
use std::collections::HashMap;

use crate::value::Value;

const CAPABILITY_RESOURCE_TYPE: &str = "\0aver.capability.resource";
const CAPABILITY_RESOURCE_FIELDS: [&str; 4] = ["binding", "type", "slot", "generation"];

/// Install the runtime-only resource carrier before an arena can be cloned
/// for parallel child VMs. `deep_import` preserves record type ids, so every
/// arena participating in a join must allocate this hidden type in the same
/// static table position.
pub(crate) fn register_capability_resource_type(arena: &mut Arena) -> u32 {
    arena
        .find_type_id(CAPABILITY_RESOURCE_TYPE)
        .unwrap_or_else(|| {
            arena.register_record_type(
                CAPABILITY_RESOURCE_TYPE,
                CAPABILITY_RESOURCE_FIELDS
                    .iter()
                    .map(|name| (*name).to_string())
                    .collect(),
            )
        })
}

/// Extension trait providing `from_value` / `to_value` conversion between
/// the runtime `Value` and `NanValue`.  These methods live here
/// (rather than on `NanValue` directly) because `NanValue` is now defined
/// in the `aver_memory` crate.
pub trait NanValueConvert {
    fn from_value(val: &Value, arena: &mut Arena) -> NanValue;
    fn to_value(self, arena: &Arena) -> Value;
}

impl NanValueConvert for NanValue {
    /// Convert old Value to NanValue, storing heap data in arena.
    fn from_value(val: &Value, arena: &mut Arena) -> NanValue {
        match val {
            Value::Int(i) => NanValue::from_aver_int(i.clone(), arena),
            Value::Float(f) => NanValue::new_float(*f),
            Value::Bool(b) => NanValue::new_bool(*b),
            Value::Unit => NanValue::UNIT,
            Value::None => NanValue::NONE,
            Value::Str(s) => NanValue::new_string_value(s, arena),
            Value::Ok(inner) => {
                let inner_nv = NanValue::from_value(inner, arena);
                NanValue::new_ok_value(inner_nv, arena)
            }
            Value::Err(inner) => {
                let inner_nv = NanValue::from_value(inner, arena);
                NanValue::new_err_value(inner_nv, arena)
            }
            Value::Some(inner) => {
                let inner_nv = NanValue::from_value(inner, arena);
                NanValue::new_some_value(inner_nv, arena)
            }
            Value::Tuple(items) => {
                let nv_items: Vec<_> = items
                    .iter()
                    .map(|v| NanValue::from_value(v, arena))
                    .collect();
                NanValue::new_tuple(arena.push_tuple(nv_items))
            }
            Value::List(aver_list) => {
                let items: Vec<_> = aver_list
                    .to_vec()
                    .iter()
                    .map(|v| NanValue::from_value(v, arena))
                    .collect();
                if items.is_empty() {
                    NanValue::EMPTY_LIST
                } else {
                    NanValue::new_list(arena.push_list(items))
                }
            }
            Value::Vector(vec) => {
                let items: Vec<_> = vec.iter().map(|v| NanValue::from_value(v, arena)).collect();
                if items.is_empty() {
                    NanValue::EMPTY_VECTOR
                } else {
                    NanValue::new_vector(arena.push_vector(items))
                }
            }
            Value::Map(map) => {
                if map.is_empty() {
                    return NanValue::EMPTY_MAP;
                }
                let mut nv_map = PersistentMap::new();
                // Same shape, and the same trap, as `Map.fromList`: the map
                // under construction is unreachable from anywhere else, so it
                // goes in through the owned insert. The preserving `insert`
                // rebuilds the whole table per entry, which made converting a
                // map of n entries cost n^2/2 duplications on a path every
                // replayed and every interop value crosses.
                let mut table = nv_map.table_id();
                for (k, v) in map {
                    let nk = NanValue::from_value(k, arena);
                    let nv = NanValue::from_value(v, arena);
                    // Use the structural (deep) key hash so it matches the
                    // hashing scheme every `Map.*` builtin queries with
                    // (`map.rs::nv_key_bits`). The shallow hash would return
                    // the arena index for ℤ-overflow int keys (mis-keying).
                    let entries_before = nv_map.len();
                    nv_map = nv_map.insert_owned(nk.map_key_hash_deep(arena), (nk, nv));
                    let table_after = nv_map.table_id();
                    if table_after != table {
                        arena.note_map_entries_copied(entries_before);
                        table = table_after;
                    }
                }
                let idx = arena.push_map(nv_map);
                NanValue::new_map(idx)
            }
            Value::Fn(f) => NanValue::new_fn(arena.push_fn(Rc::clone(f))),
            Value::Builtin(name) => NanValue::new_builtin(arena.push_builtin(name)),
            Value::Record { type_name, fields } => {
                let type_id = arena.find_type_id(type_name).unwrap_or_else(|| {
                    let field_names: Vec<String> = fields.iter().map(|(n, _)| n.clone()).collect();
                    arena.register_record_type(type_name, field_names)
                });
                let nv_fields: Vec<_> = fields
                    .iter()
                    .map(|(_, v)| NanValue::from_value(v, arena))
                    .collect();
                NanValue::new_record(arena.push_record(type_id, nv_fields))
            }
            Value::CapabilityResource(handle) => {
                let type_id = register_capability_resource_type(arena);
                let fields = vec![
                    NanValue::new_string_value(&handle.binding_id().to_string(), arena),
                    NanValue::new_string_value(handle.type_name(), arena),
                    NanValue::new_string_value(&handle.slot().to_string(), arena),
                    NanValue::new_string_value(&handle.generation().to_string(), arena),
                ];
                NanValue::new_record(arena.push_record(type_id, fields))
            }
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                let type_id = arena
                    .find_type_id(type_name)
                    .unwrap_or_else(|| arena.register_sum_type(type_name, vec![variant.clone()]));
                let variant_id = arena
                    .find_variant_id(type_id, variant)
                    .unwrap_or_else(|| arena.register_variant_name(type_id, variant.clone()));
                let nv_fields: Vec<_> = fields
                    .iter()
                    .map(|v| NanValue::from_value(v, arena))
                    .collect();
                if nv_fields.is_empty() {
                    NanValue::new_nullary_variant(arena.push_nullary_variant_symbol(
                        arena.find_ctor_id(type_id, variant_id).unwrap(),
                    ))
                } else if nv_fields.len() == 1 {
                    if let Some(ctor_id) = arena.find_ctor_id(type_id, variant_id)
                        && let Some(iv) = NanValue::try_new_inline_variant(ctor_id, nv_fields[0])
                    {
                        return iv;
                    }
                    NanValue::new_variant(arena.push_variant(type_id, variant_id, nv_fields))
                } else {
                    NanValue::new_variant(arena.push_variant(type_id, variant_id, nv_fields))
                }
            }
            Value::Namespace { name, members } => {
                let nv_members: Vec<_> = members
                    .iter()
                    .map(|(k, v)| (Rc::from(k.as_str()), NanValue::from_value(v, arena)))
                    .collect();
                let idx = arena.push(ArenaEntry::Namespace {
                    name: Rc::from(name.as_str()),
                    members: nv_members,
                });
                NanValue::new_namespace(idx)
            }
        }
    }

    /// Convert NanValue back to old Value (for interop during migration).
    fn to_value(self, arena: &Arena) -> Value {
        if self.is_float() {
            return Value::Float(self.as_float());
        }
        if let Some((kind, inner)) = self.wrapper_parts(arena) {
            let inner = inner.to_value(arena);
            return match kind {
                WRAP_SOME => Value::Some(Box::new(inner)),
                WRAP_OK => Value::Ok(Box::new(inner)),
                WRAP_ERR => Value::Err(Box::new(inner)),
                _ => Value::Unit,
            };
        }
        if let Some((type_id, variant_id, inner)) = self.inline_variant_info(arena) {
            let type_name = arena.get_type_name(type_id).to_string();
            let variant = arena.get_variant_name(type_id, variant_id).to_string();
            return Value::Variant {
                type_name,
                variant,
                fields: vec![inner.to_value(arena)].into(),
            };
        }
        if let Some((type_id, variant_id, fields)) = self.variant_parts(arena) {
            let type_name = arena.get_type_name(type_id).to_string();
            let variant = arena.get_variant_name(type_id, variant_id).to_string();
            let vals: Vec<Value> = fields.iter().map(|v| v.to_value(arena)).collect();
            return Value::Variant {
                type_name,
                variant,
                fields: vals.into(),
            };
        }
        match self.tag() {
            TAG_INT => Value::Int(self.as_aver_int(arena)),
            TAG_IMMEDIATE => match self.payload() {
                IMM_FALSE => Value::Bool(false),
                IMM_TRUE => Value::Bool(true),
                IMM_UNIT => Value::Unit,
                _ => Value::Unit,
            },
            TAG_NONE => Value::None,
            TAG_SOME | TAG_OK | TAG_ERR => {
                unreachable!("wrapper conversion handled before tag switch")
            }
            TAG_STRING => Value::Str(arena.get_string_value(self).to_string()),
            TAG_LIST => {
                let vals: Vec<Value> = arena
                    .list_to_vec_value(self)
                    .into_iter()
                    .map(|v| v.to_value(arena))
                    .collect();
                Value::List(aver_rt::AverList::from_vec(vals))
            }
            TAG_VECTOR => {
                let items = arena.vector_ref_value(self);
                let vals: Vec<Value> = items.iter().map(|v| v.to_value(arena)).collect();
                Value::Vector(aver_rt::AverVector::from_vec(vals))
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                Value::Tuple(items.iter().map(|v| v.to_value(arena)).collect())
            }
            TAG_MAP => {
                let map = arena.map_ref_value(self);
                let mut hm = HashMap::new();
                for (k, v) in map.values() {
                    hm.insert(k.to_value(arena), v.to_value(arena));
                }
                Value::Map(hm)
            }
            TAG_RECORD => {
                let (type_id, fields) = arena.get_record(self.arena_index());
                let type_name = arena.get_type_name(type_id).to_string();
                if type_name == CAPABILITY_RESOURCE_TYPE {
                    let values: Vec<String> = fields
                        .iter()
                        .map(|value| match value.to_value(arena) {
                            Value::Str(value) => value,
                            _ => String::new(),
                        })
                        .collect();
                    if values.len() == 4
                        && let (Ok(binding_id), Ok(slot), Ok(generation)) = (
                            values[0].parse::<u64>(),
                            values[2].parse::<u64>(),
                            values[3].parse::<u64>(),
                        )
                    {
                        return Value::CapabilityResource(
                            crate::provider::CapabilityResourceHandle::from_runtime_parts(
                                binding_id,
                                values[1].clone(),
                                slot,
                                generation,
                            ),
                        );
                    }
                    return Value::Unit;
                }
                let field_names = arena.get_field_names(type_id);
                let pairs: Vec<(String, Value)> = field_names
                    .iter()
                    .zip(fields)
                    .map(|(n, v)| (n.clone(), v.to_value(arena)))
                    .collect();
                Value::Record {
                    type_name,
                    fields: pairs.into(),
                }
            }
            TAG_VARIANT | TAG_INLINE_VARIANT => {
                unreachable!("variant conversion handled before tag switch")
            }
            TAG_SYMBOL => match self.symbol_kind() {
                SYMBOL_FN => Value::Fn(Rc::clone(arena.get_fn_rc(self.symbol_index()))),
                SYMBOL_BUILTIN => {
                    Value::Builtin(arena.get_builtin(self.symbol_index()).to_string())
                }
                SYMBOL_NAMESPACE => {
                    let (name, members) = arena.get_namespace(self.symbol_index());
                    let mut hm = HashMap::new();
                    for (k, v) in members {
                        hm.insert(k.to_string(), v.to_value(arena));
                    }
                    Value::Namespace {
                        name: name.to_string(),
                        members: hm,
                    }
                }
                SYMBOL_NULLARY_VARIANT => {
                    unreachable!("variant conversion handled before tag switch")
                }
                _ => Value::Unit,
            },
            _ => Value::Unit,
        }
    }
}
