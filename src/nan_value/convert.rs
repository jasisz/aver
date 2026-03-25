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
                for (k, v) in map {
                    let nk = NanValue::from_value(k, arena);
                    let nv = NanValue::from_value(v, arena);
                    nv_map = nv_map.insert(nk.map_key_hash(arena), (nk, nv));
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
    pub fn to_value(self, arena: &Arena) -> Value {
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
            TAG_INT => Value::Int(self.as_int(arena)),
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
