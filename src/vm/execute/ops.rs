use super::VM;
use crate::nan_value::{NanIntExt, NanValue};
use crate::vm::symbol::VmSymbolKind;
use crate::vm::types::VmError;

impl VM {
    pub(super) fn decode_vm_symbol_id(&self, val: NanValue) -> Option<u32> {
        if !val.is_int() {
            return None;
        }
        // Symbol ids are small by construction; a ℤ-overflow int is never one.
        let symbol_id = val.as_aver_int(&self.arena).to_i64()?;
        (symbol_id >= 0).then_some(symbol_id as u32).filter(|&id| {
            self.code
                .symbols
                .get(id)
                .is_some_and(|info| info.kind.is_some())
        })
    }

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
        if let Some(symbol_id) = self.decode_vm_symbol_id(val)
            && let Some(fn_id) = self.code.symbols.resolve_function(symbol_id)
        {
            return Ok(fn_id);
        }
        let caller_name = &self.code.functions[caller_fn_id as usize].name;
        Err(VmError::type_err(format!(
            "cannot call non-function (got {} = {:?}) in {} at ip={}",
            self.value_type_name(val),
            self.value_repr(val),
            caller_name,
            ip
        )))
    }

    pub(super) fn variant_ctor_id_vm(&self, val: NanValue) -> Option<u32> {
        if let Some(symbol_id) = self.decode_vm_symbol_id(val)
            && let Some(ctor) = self.code.symbols.resolve_variant_ctor(symbol_id)
            && ctor.field_count == 0
        {
            return Some(ctor.ctor_id);
        }
        val.variant_ctor_id(&self.arena)
    }

    pub(super) fn value_type_name(&self, val: NanValue) -> String {
        if let Some(symbol_id) = self.decode_vm_symbol_id(val)
            && let Some(info) = self.code.symbols.get(symbol_id)
            && let Some(kind) = info.kind
        {
            return match kind {
                VmSymbolKind::Function(_) => "Fn".to_string(),
                VmSymbolKind::Builtin(_) => "Builtin".to_string(),
                VmSymbolKind::Namespace => "Namespace".to_string(),
                VmSymbolKind::VariantCtor(ctor) => {
                    if ctor.field_count == 0 {
                        "Variant".to_string()
                    } else {
                        "VariantCtor".to_string()
                    }
                }
                VmSymbolKind::Wrapper(kind) => match kind {
                    0 => "Result.Ok".to_string(),
                    1 => "Result.Err".to_string(),
                    2 => "Option.Some".to_string(),
                    _ => "Wrapper".to_string(),
                },
                VmSymbolKind::Constant(value) => value.type_name().to_string(),
            };
        }
        val.type_name().to_string()
    }

    pub(super) fn value_repr(&self, val: NanValue) -> String {
        if let Some(symbol_id) = self.decode_vm_symbol_id(val)
            && let Some(info) = self.code.symbols.get(symbol_id)
            && let Some(kind) = info.kind
        {
            return match kind {
                VmSymbolKind::Function(_) => {
                    format!("<fn {}>", info.name)
                }
                VmSymbolKind::Builtin(_) => {
                    format!("<builtin {}>", info.name)
                }
                VmSymbolKind::Namespace => {
                    format!("<type {}>", info.name)
                }
                VmSymbolKind::VariantCtor(ctor) => {
                    if ctor.field_count == 0 {
                        info.name
                            .rsplit('.')
                            .next()
                            .unwrap_or(info.name.as_str())
                            .to_string()
                    } else {
                        format!("<ctor {}>", info.name)
                    }
                }
                VmSymbolKind::Wrapper(_) => {
                    format!("<ctor {}>", info.name)
                }
                VmSymbolKind::Constant(value) => value.repr(&self.arena),
            };
        }
        val.repr(&self.arena)
    }

    pub(super) fn arith_add(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            // `Int` is mathematical ℤ: never wraps. The i64 fast path inside
            // `AverInt::add` keeps the common case allocation-free, promoting
            // to a bignum only on genuine overflow.
            let r = a.as_aver_int(&self.arena).add(&b.as_aver_int(&self.arena));
            Ok(NanValue::from_aver_int(r, &mut self.arena))
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() + b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_aver_int(&self.arena).to_f64() + b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() + b.as_aver_int(&self.arena).to_f64(),
            ))
        } else if a.is_string() && b.is_string() {
            let s = format!(
                "{}{}",
                self.arena.get_string_value(a),
                self.arena.get_string_value(b)
            );
            Ok(NanValue::new_string_value(&s, &mut self.arena))
        } else {
            Err(VmError::type_err(format!(
                "cannot add {} and {}",
                self.value_type_name(a),
                self.value_type_name(b)
            )))
        }
    }

    pub(super) fn arith_sub(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            let r = a.as_aver_int(&self.arena).sub(&b.as_aver_int(&self.arena));
            Ok(NanValue::from_aver_int(r, &mut self.arena))
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() - b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_aver_int(&self.arena).to_f64() - b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() - b.as_aver_int(&self.arena).to_f64(),
            ))
        } else {
            Err(VmError::type_err(format!(
                "cannot subtract {} and {}",
                self.value_type_name(a),
                self.value_type_name(b)
            )))
        }
    }

    pub(super) fn arith_mul(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            let r = a.as_aver_int(&self.arena).mul(&b.as_aver_int(&self.arena));
            Ok(NanValue::from_aver_int(r, &mut self.arena))
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() * b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_aver_int(&self.arena).to_f64() * b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() * b.as_aver_int(&self.arena).to_f64(),
            ))
        } else {
            Err(VmError::type_err(format!(
                "cannot multiply {} and {}",
                self.value_type_name(a),
                self.value_type_name(b)
            )))
        }
    }

    pub(super) fn arith_div(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            // Truncating division over ℤ (the raw `/` operator). Divisor-zero
            // is a runtime error; the old `i64::MIN / -1` overflow is gone —
            // over ℤ it is just `i64::MAX + 1`.
            match a
                .as_aver_int(&self.arena)
                .div_trunc(&b.as_aver_int(&self.arena))
            {
                Some(q) => Ok(NanValue::from_aver_int(q, &mut self.arena)),
                None => Err(VmError::runtime("division by zero")),
            }
        } else if a.is_float() && b.is_float() {
            Ok(NanValue::new_float(a.as_float() / b.as_float()))
        } else if a.is_int() && b.is_float() {
            Ok(NanValue::new_float(
                a.as_aver_int(&self.arena).to_f64() / b.as_float(),
            ))
        } else if a.is_float() && b.is_int() {
            Ok(NanValue::new_float(
                a.as_float() / b.as_aver_int(&self.arena).to_f64(),
            ))
        } else {
            Err(VmError::type_err(format!(
                "cannot divide {} and {}",
                self.value_type_name(a),
                self.value_type_name(b)
            )))
        }
    }

    pub(super) fn arith_mod(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
        if a.is_int() && b.is_int() {
            // Truncating remainder over ℤ (the raw `%` operator).
            match a
                .as_aver_int(&self.arena)
                .rem_trunc(&b.as_aver_int(&self.arena))
            {
                Some(r) => Ok(NanValue::from_aver_int(r, &mut self.arena)),
                None => Err(VmError::runtime("modulo by zero")),
            }
        } else {
            Err(VmError::type_err(format!(
                "cannot modulo {} and {}",
                self.value_type_name(a),
                self.value_type_name(b)
            )))
        }
    }

    /// Convert an `Int` value used as a container index into a `usize`,
    /// returning `None` when it is negative or out of `usize` range (e.g. a
    /// ℤ-overflow index). Index sites map this `None` to the language's
    /// `Option.None` — never a panic or a truncated wrap.
    pub(super) fn int_to_index(&self, idx: NanValue) -> Option<usize> {
        idx.as_aver_int(&self.arena).to_usize()
    }

    /// Convert an `Int` value used as an allocation capacity / host argument
    /// into a `usize`, erroring cleanly when it does not fit (it cannot be
    /// allocated). Never panics, never truncates. `site` names the operation
    /// for the error message.
    pub(super) fn int_to_capacity(&self, n: NanValue, site: &str) -> Result<usize, VmError> {
        n.as_aver_int(&self.arena).to_usize().ok_or_else(|| {
            VmError::runtime(format!(
                "{}: size does not fit a machine-sized integer",
                site
            ))
        })
    }

    pub(super) fn compare_lt(&self, a: NanValue, b: NanValue) -> Result<bool, VmError> {
        if a.is_int() && b.is_int() {
            Ok(a.as_aver_int(&self.arena) < b.as_aver_int(&self.arena))
        } else if a.is_float() && b.is_float() {
            Ok(a.as_float() < b.as_float())
        } else if a.is_string() && b.is_string() {
            Ok(self.arena.get_string_value(a) < self.arena.get_string_value(b))
        } else if a.is_int() && b.is_float() {
            Ok(a.as_aver_int(&self.arena).to_f64() < b.as_float())
        } else if a.is_float() && b.is_int() {
            Ok(a.as_float() < b.as_aver_int(&self.arena).to_f64())
        } else {
            Err(VmError::type_err(format!(
                "cannot compare {} and {}",
                self.value_type_name(a),
                self.value_type_name(b)
            )))
        }
    }
}
