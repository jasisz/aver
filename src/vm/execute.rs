use crate::nan_value::{Arena, NanValue};
use crate::services::{console, disk, env, http, random, tcp, time};
use crate::types::{bool, byte, char, float, int, list, map, option, result, string};
use crate::value::RuntimeError;

use super::opcode::*;
use super::types::{CallFrame, CodeStore, VmError};

/// The Aver bytecode virtual machine.
pub struct VM {
    stack: Vec<NanValue>,
    frames: Vec<CallFrame>,
    globals: Vec<NanValue>,
    code: CodeStore,
    pub arena: Arena,
}

macro_rules! read_u8 {
    ($code:expr, $ip:expr) => {{
        let v = $code[$ip];
        $ip += 1;
        v
    }};
}

macro_rules! read_u16 {
    ($code:expr, $ip:expr) => {{
        let hi = $code[$ip] as u16;
        let lo = $code[$ip + 1] as u16;
        $ip += 2;
        (hi << 8) | lo
    }};
}

macro_rules! read_i16 {
    ($code:expr, $ip:expr) => {{ read_u16!($code, $ip) as i16 }};
}

impl VM {
    pub fn new(code: CodeStore, globals: Vec<NanValue>, arena: Arena) -> Self {
        VM {
            stack: Vec::with_capacity(1024),
            frames: Vec::with_capacity(64),
            globals,
            code,
            arena,
        }
    }

    pub fn run(&mut self) -> Result<NanValue, VmError> {
        if let Some(top_id) = self.code.find("__top_level__") {
            self.call_function(top_id, &[])?;
        }
        if let Some(main_id) = self.code.find("main") {
            self.call_function(main_id, &[])
        } else {
            Err(VmError::Runtime("no main() function defined".into()))
        }
    }

    pub fn call_function(&mut self, fn_id: u32, args: &[NanValue]) -> Result<NanValue, VmError> {
        let chunk = self.code.get(fn_id);
        let bp = self.stack.len() as u32;
        for arg in args {
            self.stack.push(*arg);
        }
        for _ in args.len()..(chunk.local_count as usize) {
            self.stack.push(NanValue::UNIT);
        }
        self.frames.push(CallFrame {
            fn_id,
            ip: 0,
            bp,
            local_count: chunk.local_count,
        });
        self.execute()
    }

    fn execute(&mut self) -> Result<NanValue, VmError> {
        let mut fn_id = self.frames.last().unwrap().fn_id;
        let mut ip = self.frames.last().unwrap().ip as usize;
        let mut bp = self.frames.last().unwrap().bp as usize;

        loop {
            let code = &self.code.functions[fn_id as usize].code;

            let op = code[ip];
            ip += 1;

            match op {
                LOAD_LOCAL => {
                    let slot = read_u8!(code, ip) as usize;
                    self.stack.push(self.stack[bp + slot]);
                }

                STORE_LOCAL => {
                    let slot = read_u8!(code, ip) as usize;
                    let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack[bp + slot] = val;
                }

                LOAD_CONST => {
                    let idx = read_u16!(code, ip) as usize;
                    let val = self.code.functions[fn_id as usize].constants[idx];
                    self.stack.push(val);
                }

                LOAD_GLOBAL => {
                    let idx = read_u16!(code, ip) as usize;
                    self.stack.push(self.globals[idx]);
                }

                STORE_GLOBAL => {
                    let idx = read_u16!(code, ip) as usize;
                    let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if idx >= self.globals.len() {
                        self.globals.resize(idx + 1, NanValue::UNIT);
                    }
                    self.globals[idx] = val;
                }

                POP => {
                    self.stack.pop().ok_or(VmError::StackUnderflow)?;
                }

                DUP => {
                    let val = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(val);
                }

                LOAD_UNIT => self.stack.push(NanValue::UNIT),
                LOAD_TRUE => self.stack.push(NanValue::TRUE),
                LOAD_FALSE => self.stack.push(NanValue::FALSE),

                ADD => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = self.arith_add(a, b)?;
                    self.stack.push(r);
                }
                SUB => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = self.arith_sub(a, b)?;
                    self.stack.push(r);
                }
                MUL => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = self.arith_mul(a, b)?;
                    self.stack.push(r);
                }
                DIV => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = self.arith_div(a, b)?;
                    self.stack.push(r);
                }
                MOD => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = self.arith_mod(a, b)?;
                    self.stack.push(r);
                }
                NEG => {
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if a.is_int() {
                        self.stack
                            .push(NanValue::new_int(-a.as_int(&self.arena), &mut self.arena));
                    } else if a.is_float() {
                        self.stack.push(NanValue::new_float(-a.as_float()));
                    } else {
                        return Err(VmError::Type("cannot negate non-numeric".into()));
                    }
                }
                NOT => {
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(!a.as_bool()));
                }

                EQ => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(a.eq_in(b, &self.arena)));
                }
                LT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(self.compare_lt(a, b)?));
                }
                GT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(self.compare_lt(b, a)?));
                }

                CONCAT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let sa = a.repr(&self.arena);
                    let sb = b.repr(&self.arena);
                    let idx = self.arena.push_string(&format!("{}{}", sa, sb));
                    self.stack.push(NanValue::new_string(idx));
                }

                JUMP => {
                    let offset = read_i16!(code, ip);
                    ip = (ip as isize + offset as isize) as usize;
                }

                JUMP_IF_FALSE => {
                    let offset = read_i16!(code, ip);
                    let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if val.is_bool() && !val.as_bool() {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                CALL_KNOWN => {
                    let target_fn_id = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;

                    // Save current frame state.
                    self.frames.last_mut().unwrap().ip = ip as u32;

                    let target = self.code.get(target_fn_id);
                    let new_bp = self.stack.len() - argc;
                    for _ in 0..(target.local_count as usize - argc) {
                        self.stack.push(NanValue::UNIT);
                    }

                    self.frames.push(CallFrame {
                        fn_id: target_fn_id,
                        ip: 0,
                        bp: new_bp as u32,
                        local_count: target.local_count,
                    });

                    fn_id = target_fn_id;
                    ip = 0;
                    bp = new_bp;
                }

                CALL_VALUE => {
                    let argc = read_u8!(code, ip) as usize;
                    let fn_pos = self.stack.len() - 1 - argc;
                    let fn_val = self.stack[fn_pos];
                    let target_fn_id = if fn_val.is_int() {
                        fn_val.as_int(&self.arena) as u32
                    } else {
                        return Err(VmError::Type("cannot call non-function".into()));
                    };

                    self.frames.last_mut().unwrap().ip = ip as u32;
                    self.stack.remove(fn_pos);

                    let target = self.code.get(target_fn_id);
                    let new_bp = self.stack.len() - argc;
                    for _ in 0..(target.local_count as usize - argc) {
                        self.stack.push(NanValue::UNIT);
                    }

                    self.frames.push(CallFrame {
                        fn_id: target_fn_id,
                        ip: 0,
                        bp: new_bp as u32,
                        local_count: target.local_count,
                    });

                    fn_id = target_fn_id;
                    ip = 0;
                    bp = new_bp;
                }

                CALL_BUILTIN => {
                    let name_idx = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;
                    let builtin_name = self.arena.get_string(name_idx).to_string();

                    // Collect args from stack.
                    let args_start = self.stack.len() - argc;
                    let args: Vec<NanValue> = self.stack[args_start..].to_vec();
                    self.stack.truncate(args_start);

                    let result = dispatch_builtin_nv(&builtin_name, &args, &mut self.arena)?;
                    self.stack.push(result);
                }

                TAIL_CALL_SELF => {
                    let argc = read_u8!(code, ip) as usize;
                    let args_start = self.stack.len() - argc;
                    for i in 0..argc {
                        self.stack[bp + i] = self.stack[args_start + i];
                    }
                    let lc = self.frames.last().unwrap().local_count as usize;
                    for i in argc..lc {
                        self.stack[bp + i] = NanValue::UNIT;
                    }
                    self.stack.truncate(bp + lc);
                    ip = 0;
                }

                TAIL_CALL_KNOWN => {
                    let target_fn_id = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;
                    let target = self.code.get(target_fn_id);

                    let args_start = self.stack.len() - argc;
                    for i in 0..argc {
                        self.stack[bp + i] = self.stack[args_start + i];
                    }

                    let new_lc = target.local_count as usize;
                    let new_end = bp + new_lc;
                    for i in argc..new_lc {
                        self.stack[bp + i] = NanValue::UNIT;
                    }
                    if new_end > self.stack.len() {
                        self.stack.resize(new_end, NanValue::UNIT);
                    } else {
                        self.stack.truncate(new_end);
                    }

                    let frame = self.frames.last_mut().unwrap();
                    frame.fn_id = target_fn_id;
                    frame.local_count = target.local_count;
                    fn_id = target_fn_id;
                    ip = 0;
                }

                RETURN => {
                    let result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let frame = self.frames.pop().unwrap();
                    self.stack.truncate(frame.bp as usize);

                    if self.frames.is_empty() {
                        return Ok(result);
                    }

                    self.stack.push(result);

                    // Restore caller frame state.
                    let caller = self.frames.last().unwrap();
                    fn_id = caller.fn_id;
                    ip = caller.ip as usize;
                    bp = caller.bp as usize;
                }

                LIST_NIL => {
                    let idx = self.arena.push_list(Vec::new());
                    self.stack.push(NanValue::new_list(idx));
                }

                LIST_CONS => {
                    let tail = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let head = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if tail.is_list() {
                        let tail_items = self.arena.get_list(tail.arena_index()).to_vec();
                        let mut items = Vec::with_capacity(tail_items.len() + 1);
                        items.push(head);
                        items.extend(tail_items);
                        let idx = self.arena.push_list(items);
                        self.stack.push(NanValue::new_list(idx));
                    } else {
                        let idx = self.arena.push_list(vec![head]);
                        self.stack.push(NanValue::new_list(idx));
                    }
                }

                LIST_NEW => {
                    let count = read_u8!(code, ip) as usize;
                    let start = self.stack.len() - count;
                    let items: Vec<NanValue> = self.stack[start..].to_vec();
                    self.stack.truncate(start);
                    let idx = self.arena.push_list(items);
                    self.stack.push(NanValue::new_list(idx));
                }

                RECORD_NEW => {
                    let type_id = read_u16!(code, ip) as u32;
                    let count = read_u8!(code, ip) as usize;
                    let start = self.stack.len() - count;
                    let fields: Vec<NanValue> = self.stack[start..].to_vec();
                    self.stack.truncate(start);
                    let idx = self.arena.push_record(type_id, fields);
                    self.stack.push(NanValue::new_record(idx));
                }

                RECORD_GET => {
                    let field_idx = read_u8!(code, ip) as usize;
                    let record = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if record.is_record() {
                        let (_, fields) = self.arena.get_record(record.arena_index());
                        if field_idx < fields.len() {
                            self.stack.push(fields[field_idx]);
                        } else {
                            return Err(VmError::Runtime("field index out of bounds".into()));
                        }
                    } else {
                        return Err(VmError::Type("RECORD_GET on non-record".into()));
                    }
                }

                RECORD_GET_NAMED => {
                    let name_const_idx = read_u16!(code, ip) as usize;
                    let field_name_nv =
                        self.code.functions[fn_id as usize].constants[name_const_idx];
                    let field_name = self.arena.get_string(field_name_nv.arena_index());

                    let record = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if record.is_record() {
                        let (type_id, fields) = self.arena.get_record(record.arena_index());
                        let field_names = self.arena.get_field_names(type_id);
                        let mut found = false;
                        for (i, fname) in field_names.iter().enumerate() {
                            if fname == field_name {
                                self.stack.push(fields[i]);
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            return Err(VmError::Runtime(format!(
                                "record has no field '{}'",
                                field_name
                            )));
                        }
                    } else {
                        return Err(VmError::Type(format!(
                            "field access on non-record value ({})",
                            record.type_name()
                        )));
                    }
                }

                VARIANT_NEW => {
                    let type_id = read_u16!(code, ip) as u32;
                    let variant_id = read_u16!(code, ip);
                    let count = read_u8!(code, ip) as usize;
                    let start = self.stack.len() - count;
                    let fields: Vec<NanValue> = self.stack[start..].to_vec();
                    self.stack.truncate(start);
                    let idx = self.arena.push_variant(type_id, variant_id, fields);
                    self.stack.push(NanValue::new_variant(idx));
                }

                WRAP => {
                    let kind = read_u8!(code, ip);
                    let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let boxed_idx = self.arena.push_boxed(val);
                    let wrapped = match kind {
                        0 => NanValue::new_ok(boxed_idx),
                        1 => NanValue::new_err(boxed_idx),
                        2 => NanValue::new_some(boxed_idx),
                        _ => return Err(VmError::Runtime("invalid wrap kind".into())),
                    };
                    self.stack.push(wrapped);
                }

                MATCH_TAG => {
                    let expected_tag = read_u8!(code, ip);
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    if self.nan_tag(top) != expected_tag {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                MATCH_VARIANT => {
                    let expected_vid = read_u16!(code, ip);
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    if top.is_variant() {
                        let (_, vid, _) = self.arena.get_variant(top.arena_index());
                        if vid != expected_vid {
                            ip = (ip as isize + offset as isize) as usize;
                        }
                    } else {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                MATCH_UNWRAP => {
                    let kind = read_u8!(code, ip);
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let matches = match kind {
                        0 => top.is_ok(),
                        1 => top.is_err(),
                        2 => top.is_some(),
                        _ => false,
                    };
                    if matches {
                        let inner = self.arena.get_boxed(top.wrapper_index());
                        *self.stack.last_mut().unwrap() = inner;
                    } else {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                MATCH_NIL => {
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let is_nil = top.is_list() && self.arena.get_list(top.arena_index()).is_empty();
                    if !is_nil {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                MATCH_CONS => {
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let is_cons =
                        top.is_list() && !self.arena.get_list(top.arena_index()).is_empty();
                    if !is_cons {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                LIST_HEAD_TAIL => {
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let items = self.arena.get_list(list.arena_index());
                    if items.is_empty() {
                        return Err(VmError::Runtime("LIST_HEAD_TAIL on empty list".into()));
                    }
                    let head = items[0];
                    let tail_items = items[1..].to_vec();
                    let tail_idx = self.arena.push_list(tail_items);
                    self.stack.push(NanValue::new_list(tail_idx));
                    self.stack.push(head);
                }

                EXTRACT_FIELD => {
                    let field_idx = read_u8!(code, ip) as usize;
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    if top.is_record() {
                        let (_, fields) = self.arena.get_record(top.arena_index());
                        self.stack.push(fields[field_idx]);
                    } else if top.is_variant() {
                        let (_, _, fields) = self.arena.get_variant(top.arena_index());
                        self.stack.push(fields[field_idx]);
                    } else {
                        return Err(VmError::Type("EXTRACT_FIELD on non-record/variant".into()));
                    }
                }

                MATCH_FAIL => {
                    let line = read_u16!(code, ip);
                    return Err(VmError::MatchFail(line));
                }

                _ => {
                    return Err(VmError::Runtime(format!("unknown opcode: 0x{:02X}", op)));
                }
            }
        }
    }

    fn nan_tag(&self, val: NanValue) -> u8 {
        if val.is_float() {
            return 0xFF;
        }
        ((val.bits() >> 46) & 0xF) as u8
    }

    fn arith_add(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
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
            let s = format!(
                "{}{}",
                self.arena.get_string(a.arena_index()),
                self.arena.get_string(b.arena_index())
            );
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

    fn arith_sub(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
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

    fn arith_mul(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
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

    fn arith_div(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
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

    fn arith_mod(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, VmError> {
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

    fn compare_lt(&self, a: NanValue, b: NanValue) -> Result<bool, VmError> {
        if a.is_int() && b.is_int() {
            Ok(a.as_int(&self.arena) < b.as_int(&self.arena))
        } else if a.is_float() && b.is_float() {
            Ok(a.as_float() < b.as_float())
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

/// Dispatch a builtin call by name to the appropriate service/type module.
/// Reuses the existing `call_nv` functions from interpreter services.
fn dispatch_builtin_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Result<NanValue, VmError> {
    let namespace = name.split_once('.').map(|(ns, _)| ns);

    let result = match namespace {
        Some("Console") => console::call_nv(name, args, arena),
        Some("Http") => http::call_nv(name, args, arena),
        Some("Disk") => disk::call_nv(name, args, arena),
        Some("Env") => env::call_nv(name, args, arena),
        Some("Random") => random::call_nv(name, args, arena),
        Some("Tcp") => tcp::call_nv(name, args, arena),
        Some("Time") => time::call_nv(name, args, arena),
        Some("Bool") => bool::call_nv(name, args, arena),
        Some("Int") => int::call_nv(name, args, arena),
        Some("Float") => float::call_nv(name, args, arena),
        Some("String") => string::call_nv(name, args, arena),
        Some("List") => list::call_nv(name, args, arena),
        Some("Map") => map::call_nv(name, args, arena),
        Some("Char") => char::call_nv(name, args, arena),
        Some("Byte") => byte::call_nv(name, args, arena),
        Some("Result") => result::call_nv(name, args, arena),
        Some("Option") => option::call_nv(name, args, arena),
        _ => None,
    };

    match result {
        Some(Ok(val)) => Ok(val),
        Some(Err(RuntimeError::Error(msg))) => Err(VmError::Runtime(msg)),
        Some(Err(e)) => Err(VmError::Runtime(format!("{:?}", e))),
        None => Err(VmError::Runtime(format!("unknown builtin: {}", name))),
    }
}
