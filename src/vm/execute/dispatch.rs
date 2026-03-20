use super::{ReturnControl, VM};
use crate::nan_value::NanValue;
use crate::vm::opcode::*;
use crate::vm::runtime::is_http_server_builtin;
use crate::vm::types::{CallFrame, VmError};

macro_rules! read_u8 {
    ($code:expr, $ip:expr) => {{
        let v = $code[$ip];
        #[allow(unused_assignments)]
        {
            $ip += 1;
        }
        v
    }};
}

macro_rules! read_u16 {
    ($code:expr, $ip:expr) => {{
        let hi = $code[$ip] as u16;
        let lo = $code[$ip + 1] as u16;
        #[allow(unused_assignments)]
        {
            $ip += 2;
        }
        (hi << 8) | lo
    }};
}

macro_rules! read_i16 {
    ($code:expr, $ip:expr) => {{ read_u16!($code, $ip) as i16 }};
}

impl VM {
    pub(super) fn execute_until(&mut self, caller_depth: usize) -> Result<NanValue, VmError> {
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
                    if let Some(frame) = self.frames.last_mut()
                        && val.heap_index().is_some_and(|index| {
                            self.arena.is_frame_local_index(
                                index,
                                frame.arena_mark,
                                frame.yard_base,
                                frame.handoff_mark,
                            )
                        })
                    {
                        frame.globals_dirty = true;
                    }
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
                        arena_mark: self.arena.young_len() as u32,
                        yard_base: self.arena.yard_len() as u32,
                        yard_mark: self.arena.yard_len() as u32,
                        handoff_mark: self.arena.handoff_len() as u32,
                        globals_dirty: false,
                        yard_dirty: false,
                        handoff_dirty: false,
                        thin: target.thin,
                        parent_thin: target.parent_thin,
                    });

                    fn_id = target_fn_id;
                    ip = 0;
                    bp = new_bp;
                }

                CALL_VALUE => {
                    let argc = read_u8!(code, ip) as usize;
                    let fn_pos = self.stack.len() - 1 - argc;
                    let fn_val = self.stack[fn_pos];
                    let target_fn_id = self.decode_vm_fn_ref(fn_val, fn_id, ip)?;

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
                        arena_mark: self.arena.young_len() as u32,
                        yard_base: self.arena.yard_len() as u32,
                        yard_mark: self.arena.yard_len() as u32,
                        handoff_mark: self.arena.handoff_len() as u32,
                        globals_dirty: false,
                        yard_dirty: false,
                        handoff_dirty: false,
                        thin: target.thin,
                        parent_thin: target.parent_thin,
                    });

                    fn_id = target_fn_id;
                    ip = 0;
                    bp = new_bp;
                }

                CALL_BUILTIN => {
                    let name_idx = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;
                    let builtin_name = self.arena.get_string(name_idx).to_string();
                    let alloc_space = self.next_value_alloc_space(code, ip);

                    let args_start = self.stack.len() - argc;
                    let args: Vec<NanValue> = self.stack[args_start..].to_vec();
                    self.stack.truncate(args_start);

                    if is_http_server_builtin(&builtin_name) {
                        self.frames.last_mut().unwrap().ip = ip as u32;
                        let result = self.dispatch_http_server(&builtin_name, &args)?;
                        self.stack.push(result);
                        let f = self.frames.last().unwrap();
                        fn_id = f.fn_id;
                        ip = f.ip as usize;
                        bp = f.bp as usize;
                        continue;
                    }

                    let result = self.arena.with_alloc_space(alloc_space, |arena| {
                        self.runtime.invoke_builtin(&builtin_name, &args, arena)
                    })?;
                    self.stack.push(result);
                }

                TAIL_CALL_SELF => {
                    let argc = read_u8!(code, ip) as usize;
                    let args_start = self.stack.len() - argc;
                    let frame_mark = self.frames.last().unwrap().arena_mark;
                    let yard_mark = self.frames.last().unwrap().yard_mark;
                    let handoff_mark = self.frames.last().unwrap().handoff_mark;
                    let globals_dirty = self.frames.last().unwrap().globals_dirty;
                    let yard_dirty = self.frames.last().unwrap().yard_dirty;
                    let mut promoted_args = self.stack[args_start..].to_vec();
                    self.finalize_frame_locals_for_tail_call(
                        frame_mark,
                        yard_mark,
                        handoff_mark,
                        globals_dirty,
                        yard_dirty,
                        &mut promoted_args,
                    );
                    self.stack[bp..(argc + bp)].copy_from_slice(&promoted_args[..argc]);
                    let lc = self.frames.last().unwrap().local_count as usize;
                    for i in argc..lc {
                        self.stack[bp + i] = NanValue::UNIT;
                    }
                    self.stack.truncate(bp + lc);
                    let frame = self.frames.last_mut().unwrap();
                    frame.globals_dirty = false;
                    frame.yard_dirty = false;
                    frame.handoff_dirty = false;
                    frame.yard_mark = self.arena.yard_len() as u32;
                    ip = 0;
                }

                TAIL_CALL_KNOWN => {
                    let target_fn_id = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;
                    let target_local_count = self.code.get(target_fn_id).local_count;

                    let args_start = self.stack.len() - argc;
                    let frame_mark = self.frames.last().unwrap().arena_mark;
                    let yard_mark = self.frames.last().unwrap().yard_mark;
                    let handoff_mark = self.frames.last().unwrap().handoff_mark;
                    let globals_dirty = self.frames.last().unwrap().globals_dirty;
                    let yard_dirty = self.frames.last().unwrap().yard_dirty;
                    let mut promoted_args = self.stack[args_start..].to_vec();
                    self.finalize_frame_locals_for_tail_call(
                        frame_mark,
                        yard_mark,
                        handoff_mark,
                        globals_dirty,
                        yard_dirty,
                        &mut promoted_args,
                    );
                    self.stack[bp..(argc + bp)].copy_from_slice(&promoted_args[..argc]);

                    let new_lc = target_local_count as usize;
                    let new_end = bp + new_lc;
                    if new_end > self.stack.len() {
                        self.stack.resize(new_end, NanValue::UNIT);
                    }
                    for i in argc..new_lc {
                        self.stack[bp + i] = NanValue::UNIT;
                    }
                    if new_end <= self.stack.len() {
                        self.stack.truncate(new_end);
                    }

                    let frame = self.frames.last_mut().unwrap();
                    frame.fn_id = target_fn_id;
                    frame.local_count = target_local_count;
                    frame.globals_dirty = false;
                    frame.yard_dirty = false;
                    frame.handoff_dirty = false;
                    frame.yard_mark = self.arena.yard_len() as u32;
                    fn_id = target_fn_id;
                    ip = 0;
                }

                RETURN => {
                    let result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let frame = self.frames.pop().unwrap();
                    self.stack.truncate(frame.bp as usize);
                    match self.complete_frame_return(frame, result, caller_depth) {
                        ReturnControl::Done(result) => return Ok(result),
                        ReturnControl::Resume {
                            result,
                            fn_id: next_fn_id,
                            ip: next_ip,
                            bp: next_bp,
                        } => {
                            self.stack.push(result);
                            fn_id = next_fn_id;
                            ip = next_ip;
                            bp = next_bp;
                        }
                    }
                }

                LIST_LEN => {
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if !list.is_list() {
                        return Err(VmError::Runtime(
                            "List.len() argument must be a List".into(),
                        ));
                    }
                    self.stack.push(NanValue::new_int(
                        self.arena.list_len(list.arena_index()) as i64,
                        &mut self.arena,
                    ));
                }

                LIST_GET => {
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if !list.is_list() {
                        return Err(VmError::Runtime(
                            "List.get() first argument must be a List".into(),
                        ));
                    }
                    if !index.is_int() {
                        return Err(VmError::Runtime("List.get() index must be an Int".into()));
                    }
                    let idx = index.as_int(&self.arena);
                    if idx < 0 {
                        self.stack.push(NanValue::NONE);
                    } else if let Some(value) =
                        self.arena.list_get(list.arena_index(), idx as usize)
                    {
                        let wrapped = self
                            .arena
                            .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                                NanValue::new_some_value(value, arena)
                            });
                        self.stack.push(wrapped);
                    } else {
                        self.stack.push(NanValue::NONE);
                    }
                }

                LIST_GET_MATCH => {
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if !list.is_list() {
                        return Err(VmError::Runtime(
                            "List.get() first argument must be a List".into(),
                        ));
                    }
                    if !index.is_int() {
                        return Err(VmError::Runtime("List.get() index must be an Int".into()));
                    }
                    let idx = index.as_int(&self.arena);
                    if idx < 0 {
                        self.stack.push(NanValue::FALSE);
                    } else if let Some(value) =
                        self.arena.list_get(list.arena_index(), idx as usize)
                    {
                        self.stack.push(value);
                        self.stack.push(NanValue::TRUE);
                    } else {
                        self.stack.push(NanValue::FALSE);
                    }
                }

                LIST_APPEND => {
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if !list.is_list() {
                        return Err(VmError::Runtime(
                            "List.append() first argument must be a List".into(),
                        ));
                    }
                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_list_append(list, value)
                        });
                    self.stack.push(NanValue::new_list(idx));
                }

                LIST_PREPEND => {
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if !list.is_list() {
                        return Err(VmError::Runtime(
                            "List.prepend() second argument must be a List".into(),
                        ));
                    }
                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_list_prepend(value, list)
                        });
                    self.stack.push(NanValue::new_list(idx));
                }

                LIST_NIL => {
                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_list(Vec::new())
                        });
                    self.stack.push(NanValue::new_list(idx));
                }

                LIST_CONS => {
                    let tail = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let head = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = self.arena.with_alloc_space(
                        self.next_value_alloc_space(code, ip),
                        |arena| {
                            if tail.is_list() {
                                arena.push_list_prepend(head, tail)
                            } else {
                                arena.push_list(vec![head])
                            }
                        },
                    );
                    self.stack.push(NanValue::new_list(idx));
                }

                LIST_NEW => {
                    let count = read_u8!(code, ip) as usize;
                    let start = self.stack.len() - count;
                    let items: Vec<NanValue> = self.stack[start..].to_vec();
                    self.stack.truncate(start);
                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_list(items)
                        });
                    self.stack.push(NanValue::new_list(idx));
                }

                TUPLE_NEW => {
                    let count = read_u8!(code, ip) as usize;
                    let start = self.stack.len() - count;
                    let items: Vec<NanValue> = self.stack[start..].to_vec();
                    self.stack.truncate(start);
                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_tuple(items)
                        });
                    self.stack.push(NanValue::new_tuple(idx));
                }

                PROPAGATE_ERR => {
                    let value = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    if value.is_ok() {
                        let inner = value.wrapper_inner(&self.arena);
                        *self.stack.last_mut().ok_or(VmError::StackUnderflow)? = inner;
                        continue;
                    }
                    if value.is_err() {
                        let result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                        let frame = self.frames.pop().unwrap();
                        self.stack.truncate(frame.bp as usize);
                        match self.complete_frame_return(frame, result, caller_depth) {
                            ReturnControl::Done(result) => return Ok(result),
                            ReturnControl::Resume {
                                result,
                                fn_id: next_fn_id,
                                ip: next_ip,
                                bp: next_bp,
                            } => {
                                self.stack.push(result);
                                fn_id = next_fn_id;
                                ip = next_ip;
                                bp = next_bp;
                                continue;
                            }
                        }
                    }
                    return Err(VmError::Type(
                        "error propagation expects a Result value".into(),
                    ));
                }

                RECORD_UPDATE => {
                    let expected_type_id = read_u16!(code, ip) as u32;
                    let count = read_u8!(code, ip) as usize;
                    let field_indices_start = ip;
                    ip += count;

                    let base_pos = self
                        .stack
                        .len()
                        .checked_sub(count + 1)
                        .ok_or(VmError::StackUnderflow)?;
                    let base = self.stack[base_pos];
                    if !base.is_record() {
                        return Err(VmError::Type("RECORD_UPDATE on non-record".into()));
                    }
                    let (type_id, old_fields) = self.arena.get_record(base.arena_index());
                    if type_id != expected_type_id {
                        return Err(VmError::Runtime(format!(
                            "record update type mismatch: expected {}, got {}",
                            self.arena.get_type_name(expected_type_id),
                            self.arena.get_type_name(type_id)
                        )));
                    }

                    let mut fields = old_fields.to_vec();
                    for offset in (0..count).rev() {
                        let field_idx = code[field_indices_start + offset] as usize;
                        if field_idx >= fields.len() {
                            return Err(VmError::Runtime(
                                "record update field out of bounds".into(),
                            ));
                        }
                        let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                        fields[field_idx] = val;
                    }
                    self.stack.pop().ok_or(VmError::StackUnderflow)?;

                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_record(type_id, fields)
                        });
                    self.stack.push(NanValue::new_record(idx));
                }

                RECORD_NEW => {
                    let type_id = read_u16!(code, ip) as u32;
                    let count = read_u8!(code, ip) as usize;
                    let start = self.stack.len() - count;
                    let fields: Vec<NanValue> = self.stack[start..].to_vec();
                    self.stack.truncate(start);
                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_record(type_id, fields)
                        });
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
                    if fields.is_empty()
                        && let Some(ctor_id) = self.arena.find_ctor_id(type_id, variant_id)
                    {
                        self.stack.push(NanValue::new_nullary_variant(ctor_id));
                    } else {
                        let idx = self
                            .arena
                            .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                                arena.push_variant(type_id, variant_id, fields)
                            });
                        self.stack.push(NanValue::new_variant(idx));
                    }
                }

                WRAP => {
                    let kind = read_u8!(code, ip);
                    let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let wrapped = self.arena.with_alloc_space(
                        self.next_value_alloc_space(code, ip),
                        |arena| match kind {
                            0 => Ok(NanValue::new_ok_value(val, arena)),
                            1 => Ok(NanValue::new_err_value(val, arena)),
                            2 => Ok(NanValue::new_some_value(val, arena)),
                            _ => Err(VmError::Runtime("invalid wrap kind".into())),
                        },
                    )?;
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
                    let expected_ctor = read_u16!(code, ip) as u32;
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    if top.variant_ctor_id(&self.arena) != Some(expected_ctor) {
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
                        let inner = top.wrapper_inner(&self.arena);
                        *self.stack.last_mut().unwrap() = inner;
                    } else {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                MATCH_NIL => {
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let is_nil = top.is_list() && self.arena.list_is_empty(top.arena_index());
                    if !is_nil {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                MATCH_CONS => {
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let is_cons = top.is_list() && !self.arena.list_is_empty(top.arena_index());
                    if !is_cons {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                LIST_HEAD_TAIL => {
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let Some((head, tail)) = self.arena.list_uncons(list) else {
                        return Err(VmError::Runtime("LIST_HEAD_TAIL on empty list".into()));
                    };
                    self.stack.push(tail);
                    self.stack.push(head);
                }

                EXTRACT_FIELD => {
                    let field_idx = read_u8!(code, ip) as usize;
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    if top.is_record() {
                        let (_, fields) = self.arena.get_record(top.arena_index());
                        self.stack.push(fields[field_idx]);
                    } else if top.is_variant() {
                        let (_, _, fields) = top.variant_parts(&self.arena).ok_or_else(|| {
                            VmError::Type("EXTRACT_FIELD on invalid variant".into())
                        })?;
                        self.stack.push(fields[field_idx]);
                    } else {
                        return Err(VmError::Type("EXTRACT_FIELD on non-record/variant".into()));
                    }
                }

                MATCH_TUPLE => {
                    let expected_len = read_u8!(code, ip) as usize;
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let matches = top.is_tuple()
                        && self.arena.get_tuple(top.arena_index()).len() == expected_len;
                    if !matches {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                EXTRACT_TUPLE_ITEM => {
                    let item_idx = read_u8!(code, ip) as usize;
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    if !top.is_tuple() {
                        return Err(VmError::Type("EXTRACT_TUPLE_ITEM on non-tuple".into()));
                    }
                    let items = self.arena.get_tuple(top.arena_index());
                    if item_idx >= items.len() {
                        return Err(VmError::Runtime("tuple index out of bounds".into()));
                    }
                    self.stack.push(items[item_idx]);
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
}
