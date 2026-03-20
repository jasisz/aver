use super::VM;
use crate::nan_value::{ArenaEntry, NanValue};
use crate::vm::types::VmError;

impl VM {
    pub(super) fn nan_tag(&self, val: NanValue) -> u8 {
        if val.is_float() {
            return 0xFF;
        }
        ((val.bits() >> 46) & 0xF) as u8
    }

    pub(super) fn decode_vm_fn_ref(
        &self,
        val: NanValue,
        caller_fn_id: u32,
        ip: usize,
    ) -> Result<u32, VmError> {
        if val.is_int() {
            let symbol_id = val.as_int(&self.arena);
            if symbol_id >= 0
                && let Some(fn_id) = self.code.symbols.resolve_function(symbol_id as u32)
            {
                return Ok(fn_id);
            }
        }
        let caller_name = &self.code.functions[caller_fn_id as usize].name;
        Err(VmError::Type(format!(
            "cannot call non-function (got {} = {:?}) in {} at ip={}",
            val.type_name(),
            val,
            caller_name,
            ip
        )))
    }

    pub(super) fn arith_add(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            Ok(NanValue::new_int(
                a.as_int(&self.arena) + b.as_int(&self.arena),
                &mut self.arena,
            ))
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() + b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_int(&self.arena) as f64 + b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() + b.as_int(&self.arena) as f64,
            ))
        } else if a.is_string() && b.is_string() {
            let a_idx = a.arena_index();
            let b_idx = b.arena_index();
            let (left, right) = match (self.arena.get(a_idx), self.arena.get(b_idx)) {
                (ArenaEntry::String(left), ArenaEntry::String(right)) => {
                    (left.as_ref(), right.as_ref())
                }
                (left_entry, right_entry) => {
                    return Err(VmError::Runtime(format!(
                        "string add expected string entries, got a={:?} -> {:?}, b={:?} -> {:?}",
                        a, left_entry, b, right_entry
                    )));
                }
            };
            let s = format!("{left}{right}");
            let idx = self.arena.push_string(&s);
            Ok(NanValue::new_string(idx))
        } else {
            Err(VmError::Type(format!(
                "cannot add {} and {}",
                a.type_name(),
                b.type_name()
            )))
        }
    }

    pub(super) fn arith_sub(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            Ok(NanValue::new_int(
                a.as_int(&self.arena) - b.as_int(&self.arena),
                &mut self.arena,
            ))
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() - b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_int(&self.arena) as f64 - b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() - b.as_int(&self.arena) as f64,
            ))
        } else {
            Err(VmError::Type(format!(
                "cannot subtract {} and {}",
                a.type_name(),
                b.type_name()
            )))
        }
    }

    pub(super) fn arith_mul(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            Ok(NanValue::new_int(
                a.as_int(&self.arena) * b.as_int(&self.arena),
                &mut self.arena,
            ))
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() * b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_int(&self.arena) as f64 * b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() * b.as_int(&self.arena) as f64,
            ))
        } else {
            Err(VmError::Type(format!(
                "cannot multiply {} and {}",
                a.type_name(),
                b.type_name()
            )))
        }
    }

    pub(super) fn arith_div(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            let bv = b.as_int(&self.arena);
            if bv == 0 {
                return Err(VmError::Runtime("division by zero".into()));
            }
            Ok(NanValue::new_int(
                a.as_int(&self.arena) / bv,
                &mut self.arena,
            ))
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() / b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_int(&self.arena) as f64 / b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() / b.as_int(&self.arena) as f64,
            ))
        } else {
            Err(VmError::Type(format!(
                "cannot divide {} and {}",
                a.type_name(),
                b.type_name()
            )))
        }
    }

    pub(super) fn arith_mod(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            let bv = b.as_int(&self.arena);
            if bv == 0 {
                return Err(VmError::Runtime("modulo by zero".into()));
            }
            Ok(NanValue::new_int(
                a.as_int(&self.arena) % bv,
                &mut self.arena,
            ))
        } else {
            Err(VmError::Type(format!(
                "cannot modulo {} and {}",
                a.type_name(),
                b.type_name()
            )))
        }
    }

    pub(super) fn compare_lt(&self, a: NanValue, b: NanValue) -> Result<bool, VmError> {
        if a.is_int() && b.is_int() {
            Ok(a.as_int(&self.arena) < b.as_int(&self.arena))
        } else if a.is_float() && b.is_float() {
            Ok(a.as_float() < b.as_float())
        } else if a.is_string() && b.is_string() {
            Ok(self.arena.get_string(a.arena_index()) < self.arena.get_string(b.arena_index()))
        } else if a.is_int() && b.is_float() {
            Ok((a.as_int(&self.arena) as f64) < b.as_float())
        } else if a.is_float() && b.is_int() {
            Ok(a.as_float() < (b.as_int(&self.arena) as f64))
        } else {
            Err(VmError::Type(format!(
                "cannot compare {} and {}",
                a.type_name(),
                b.type_name()
            )))
        }
    }
}
