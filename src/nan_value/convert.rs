use super::*;
use std::collections::HashMap;

use crate::value::Value;

impl NanValue {
    /// Convert old Value to NanValue, storing heap data in arena.
    pub fn from_value(val: &Value, arena: &mut Arena) -> Self {
        match val {
            Value::Int(i) => NanValue::new_int(*i, arena),
            Value::Float(f) => NanValue::new_float(*f),
            Value::Bool(b) => NanValue::new_bool(*b),
            Value::Unit => NanValue::UNIT,
            Value::None => NanValue::NONE,
            Value::Str(s) => NanValue::new_string(arena.push_string(s)),
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
                NanValue::new_list(arena.push_list(items))
            }
            Value::Map(map) => {
                let mut nv_map = PersistentMap::new();
                for (k, v) in map {
                    let nk = NanValue::from_value(k, arena);
                    let nv = NanValue::from_value(v, arena);
                    nv_map.insert(nk.map_key_hash(arena), (nk, nv));
                }
                let idx = arena.push(ArenaEntry::Map(nv_map));
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
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                let type_id = arena
                    .find_type_id(type_name)
                    .unwrap_or_else(|| arena.register_sum_type(type_name, vec![variant.clone()]));
                let variant_id = arena.find_variant_id(type_id, variant).unwrap_or_else(|| {
                    // Register new variant dynamically
                    let variants = &mut arena.type_variant_names[type_id as usize];
                    let id = variants.len() as u16;
                    variants.push(variant.clone());
                    id
                });
                let nv_fields: Vec<_> = fields
                    .iter()
                    .map(|v| NanValue::from_value(v, arena))
                    .collect();
                NanValue::new_variant(arena.push_variant(type_id, variant_id, nv_fields))
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
    pub fn to_value(self, arena: &Arena) -> Value {
        if self.is_float() {
            return Value::Float(self.as_float());
        }
        match self.tag() {
            TAG_INT => Value::Int(self.as_int(arena)),
            TAG_IMMEDIATE => {
                if let Some((kind, inner)) = self.wrapper_parts(arena) {
                    let inner = inner.to_value(arena);
                    match kind {
                        WRAP_SOME => Value::Some(Box::new(inner)),
                        WRAP_OK => Value::Ok(Box::new(inner)),
                        WRAP_ERR => Value::Err(Box::new(inner)),
                        _ => Value::Unit,
                    }
                } else {
                    match self.payload() {
                        IMM_FALSE => Value::Bool(false),
                        IMM_TRUE => Value::Bool(true),
                        IMM_UNIT => Value::Unit,
                        IMM_NONE => Value::None,
                        _ => Value::Unit,
                    }
                }
            }
            TAG_WRAPPER => {
                let inner = self.wrapper_inner(arena).to_value(arena);
                match self.wrapper_kind() {
                    WRAP_SOME => Value::Some(Box::new(inner)),
                    WRAP_OK => Value::Ok(Box::new(inner)),
                    WRAP_ERR => Value::Err(Box::new(inner)),
                    _ => Value::Unit,
                }
            }
            TAG_STRING => Value::Str(arena.get_string(self.arena_index()).to_string()),
            TAG_LIST => {
                let vals: Vec<Value> = arena
                    .list_to_vec(self.arena_index())
                    .into_iter()
                    .map(|v| v.to_value(arena))
                    .collect();
                Value::List(aver_rt::AverList::from_vec(vals))
            }
            TAG_TUPLE => {
                let items = arena.get_tuple(self.arena_index());
                Value::Tuple(items.iter().map(|v| v.to_value(arena)).collect())
            }
            TAG_MAP => {
                let map = arena.get_map(self.arena_index());
                let mut hm = HashMap::new();
                for (k, v) in map.values() {
                    hm.insert(k.to_value(arena), v.to_value(arena));
                }
                Value::Map(hm)
            }
            TAG_RECORD => {
                let (type_id, fields) = arena.get_record(self.arena_index());
                let type_name = arena.get_type_name(type_id).to_string();
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
            TAG_VARIANT => {
                let (type_id, variant_id, fields) = arena.get_variant(self.arena_index());
                let type_name = arena.get_type_name(type_id).to_string();
                let variant = arena.get_variant_name(type_id, variant_id).to_string();
                let vals: Vec<Value> = fields.iter().map(|v| v.to_value(arena)).collect();
                Value::Variant {
                    type_name,
                    variant,
                    fields: vals.into(),
                }
            }
            TAG_FN => Value::Fn(Rc::clone(arena.get_fn_rc(self.arena_index()))),
            TAG_BUILTIN => Value::Builtin(arena.get_builtin(self.arena_index()).to_string()),
            TAG_NAMESPACE => {
                let (name, members) = arena.get_namespace(self.arena_index());
                let mut hm = HashMap::new();
                for (k, v) in members {
                    hm.insert(k.to_string(), v.to_value(arena));
                }
                Value::Namespace {
                    name: name.to_string(),
                    members: hm,
                }
            }
            _ => Value::Unit,
        }
    }
}
