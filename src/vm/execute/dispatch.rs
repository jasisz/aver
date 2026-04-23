use super::{ReturnControl, VM};
use crate::nan_value::{Arena, NanValue};
use crate::vm::opcode::*;
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

macro_rules! read_u32 {
    ($code:expr, $ip:expr) => {{
        let b0 = $code[$ip] as u32;
        let b1 = $code[$ip + 1] as u32;
        let b2 = $code[$ip + 2] as u32;
        let b3 = $code[$ip + 3] as u32;
        #[allow(unused_assignments)]
        {
            $ip += 4;
        }
        (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
    }};
}

impl VM {
    pub(super) fn execute_until(&mut self, caller_depth: usize) -> Result<NanValue, VmError> {
        let mut fn_id = self.frames.last().unwrap().fn_id;
        let mut ip = self.frames.last().unwrap().ip as usize;
        let mut bp = self.frames.last().unwrap().bp as usize;

        // Leaf call state: saved caller context for frameless calls.
        let mut leaf_return: Option<(u32, usize, usize)> = None; // (fn_id, ip, bp)

        loop {
            // Cooperative cancellation: check every 256 opcodes to amortise cost.
            if ip & 0xFF == 0 && self.is_cancelled() {
                return Err(VmError::runtime("cancelled by sibling branch"));
            }

            let code = &self.code.functions[fn_id as usize].code;

            // Save position for error reporting (cold-path lookup in line_table).
            self.error_fn_id = fn_id;
            self.error_ip = ip as u32;

            let op = code[ip];
            ip += 1;
            if let Some(profile) = self.profile.as_mut() {
                profile.record_opcode(op);
            }

            match op {
                NOP => {}

                LOAD_LOCAL => {
                    let slot = read_u8!(code, ip) as usize;
                    self.stack.push(self.stack[bp + slot]);
                }

                MOVE_LOCAL => {
                    let slot = read_u8!(code, ip) as usize;
                    let val = self.stack[bp + slot];
                    self.stack[bp + slot] = NanValue::UNIT;
                    self.stack.push(val);
                }

                LOAD_LOCAL_2 => {
                    let slot_a = read_u8!(code, ip) as usize;
                    let slot_b = read_u8!(code, ip) as usize;
                    self.stack.push(self.stack[bp + slot_a]);
                    self.stack.push(self.stack[bp + slot_b]);
                }

                LOAD_LOCAL_CONST => {
                    let slot = read_u8!(code, ip) as usize;
                    let const_idx = read_u16!(code, ip) as usize;
                    self.stack.push(self.stack[bp + slot]);
                    self.stack
                        .push(self.code.functions[fn_id as usize].constants[const_idx]);
                }

                // LIST_GET_OR was removed (List.get removed from language).
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
                        return Err(VmError::type_err("cannot negate non-numeric"));
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
                    // Use NanValue::repr directly — not value_repr which
                    // misidentifies data Ints as VM symbol references.
                    let sa = a.repr(&self.arena);
                    let sb = b.repr(&self.arena);
                    self.stack.push(NanValue::new_string_value(
                        &format!("{}{}", sa, sb),
                        &mut self.arena,
                    ));
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

                    let yard_len = self.arena.yard_len() as u32;
                    self.frames.push(CallFrame {
                        fn_id: target_fn_id,
                        ip: 0,
                        bp: new_bp as u32,
                        local_count: target.local_count,
                        arena_mark: self.arena.young_len() as u32,
                        yard_base: yard_len,
                        yard_mark: yard_len,
                        handoff_mark: self.arena.handoff_len() as u32,
                        globals_dirty: false,
                        yard_dirty: false,
                        handoff_dirty: false,
                        thin: target.thin,
                        parent_thin: target.parent_thin,
                    });
                    if let Some(profile) = self.profile.as_mut() {
                        profile.record_function_entry(target, target_fn_id);
                    }

                    fn_id = target_fn_id;
                    ip = 0;
                    bp = new_bp;
                }

                CALL_LEAF => {
                    let target_fn_id = read_u16!(code, ip) as u32;
                    let _argc = read_u8!(code, ip);

                    // Save caller state — no CallFrame pushed.
                    leaf_return = Some((fn_id, ip, bp));

                    let new_bp = self.stack.len() - _argc as usize;
                    fn_id = target_fn_id;
                    ip = 0;
                    bp = new_bp;
                }

                CALL_VALUE => {
                    let argc = read_u8!(code, ip) as usize;
                    let fn_pos = self.stack.len() - 1 - argc;
                    let fn_val = self.stack[fn_pos];

                    if let Some(symbol_id) = self.decode_vm_symbol_id(fn_val) {
                        if let Some(builtin) = self.code.symbols.resolve_builtin(symbol_id) {
                            if let Some(profile) = self.profile.as_mut() {
                                profile.record_builtin_call(builtin.name());
                            }
                            let alloc_space = self.next_value_alloc_space(code, ip);
                            self.stack.remove(fn_pos);
                            let args_start = self.stack.len() - argc;
                            let args: Vec<NanValue> = self.stack[args_start..].to_vec();
                            self.stack.truncate(args_start);

                            if builtin.is_http_server() {
                                self.runtime
                                    .ensure_builtin_effects_allowed(&self.code.symbols, builtin)?;
                                self.frames.last_mut().unwrap().ip = ip as u32;
                                let result = self.dispatch_http_server(builtin, &args)?;
                                self.stack.push(result);
                                let f = self.frames.last().unwrap();
                                fn_id = f.fn_id;
                                ip = f.ip as usize;
                                bp = f.bp as usize;
                                continue;
                            }

                            let result = self.arena.with_alloc_space(alloc_space, |arena| {
                                self.runtime.invoke_builtin(
                                    &self.code.symbols,
                                    builtin,
                                    &args,
                                    arena,
                                )
                            })?;
                            self.stack.push(result);
                            continue;
                        }

                        if let Some(wrap_kind) = self.code.symbols.resolve_wrapper(symbol_id) {
                            if argc != 1 {
                                let name = self
                                    .code
                                    .symbols
                                    .get(symbol_id)
                                    .map(|info| info.name.as_str())
                                    .unwrap_or("<wrapper>");
                                return Err(VmError::runtime(format!(
                                    "{} expects 1 argument, got {}",
                                    name, argc
                                )));
                            }
                            self.stack.remove(fn_pos);
                            let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                            let wrapped = self.arena.with_alloc_space(
                                self.next_value_alloc_space(code, ip),
                                |arena| match wrap_kind {
                                    0 => Ok(NanValue::new_ok_value(val, arena)),
                                    1 => Ok(NanValue::new_err_value(val, arena)),
                                    2 => Ok(NanValue::new_some_value(val, arena)),
                                    _ => Err(VmError::runtime("invalid wrap kind")),
                                },
                            )?;
                            self.stack.push(wrapped);
                            continue;
                        }

                        if let Some(ctor) = self.code.symbols.resolve_variant_ctor(symbol_id) {
                            if argc != ctor.field_count as usize {
                                let name = self
                                    .code
                                    .symbols
                                    .get(symbol_id)
                                    .map(|info| info.name.as_str())
                                    .unwrap_or("<ctor>");
                                return Err(VmError::runtime(format!(
                                    "{} expects {} argument(s), got {}",
                                    name, ctor.field_count, argc
                                )));
                            }
                            self.stack.remove(fn_pos);
                            if ctor.field_count == 0 {
                                self.stack.push(NanValue::new_nullary_variant(
                                    self.arena.push_nullary_variant_symbol(ctor.ctor_id),
                                ));
                                continue;
                            }
                            let args_start = self.stack.len() - argc;
                            let fields: Vec<NanValue> = self.stack[args_start..].to_vec();
                            self.stack.truncate(args_start);
                            let idx = self
                                .arena
                                .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                                    arena.push_variant(ctor.type_id, ctor.variant_id, fields)
                                });
                            self.stack.push(NanValue::new_variant(idx));
                            continue;
                        }

                        if let Some(value) = self.code.symbols.resolve_constant(symbol_id) {
                            let name = self
                                .code
                                .symbols
                                .get(symbol_id)
                                .map(|info| info.name.as_str())
                                .unwrap_or("<constant>");
                            return Err(VmError::runtime(format!(
                                "cannot call constant {} = {}",
                                name,
                                self.value_repr(value)
                            )));
                        }
                    }

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
                    if let Some(profile) = self.profile.as_mut() {
                        profile.record_function_entry(target, target_fn_id);
                    }

                    fn_id = target_fn_id;
                    ip = 0;
                    bp = new_bp;
                }

                CALL_BUILTIN | CALL_BUILTIN_OWNED => {
                    let symbol_id = read_u32!(code, ip);
                    let argc = read_u8!(code, ip) as usize;
                    let owned_mask = if op == CALL_BUILTIN_OWNED {
                        read_u8!(code, ip)
                    } else {
                        0
                    };
                    let builtin =
                        self.code
                            .symbols
                            .resolve_builtin(symbol_id)
                            .ok_or_else(|| {
                                let name = self
                                    .code
                                    .symbols
                                    .get(symbol_id)
                                    .map(|info| info.name.as_str())
                                    .unwrap_or("<unknown>");
                                VmError::runtime(format!("symbol {} is not a builtin", name))
                            })?;
                    if let Some(profile) = self.profile.as_mut() {
                        profile.record_builtin_call(builtin.name());
                    }
                    let alloc_space = self.next_value_alloc_space(code, ip);

                    let args_start = self.stack.len() - argc;
                    let args: Vec<NanValue> = self.stack[args_start..].to_vec();
                    self.stack.truncate(args_start);

                    if builtin.is_http_server() {
                        self.runtime
                            .ensure_builtin_effects_allowed(&self.code.symbols, builtin)?;
                        self.frames.last_mut().unwrap().ip = ip as u32;
                        let result = self.dispatch_http_server(builtin, &args)?;
                        self.stack.push(result);
                        let f = self.frames.last().unwrap();
                        fn_id = f.fn_id;
                        ip = f.ip as usize;
                        bp = f.bp as usize;
                        continue;
                    }

                    // Oracle v1: redirect classified-effect calls to an
                    // installed verify-time stub, if present. Keep the
                    // outer execute_until's local fn_id/ip/bp untouched —
                    // call_function uses its own nested execute_until and
                    // returns here; the caller's state lives in these
                    // stack-frame locals, not in self.frames (which
                    // doesn't carry leaf-call state).
                    if let Some(stub_fn_id) = self.runtime.oracle_stub_for(builtin.name()) {
                        let result = self.dispatch_oracle_stub(stub_fn_id, &args)?;
                        self.stack.push(result);
                        continue;
                    }

                    let result = self.arena.with_alloc_space(alloc_space, |arena| {
                        self.runtime.invoke_builtin_with_owned(
                            &self.code.symbols,
                            builtin,
                            &args,
                            arena,
                            owned_mask,
                        )
                    })?;
                    self.stack.push(result);
                }

                TAIL_CALL_SELF => {
                    let argc = read_u8!(code, ip) as usize;
                    let _owned_mask = read_u8!(code, ip);
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
                    if let Some(profile) = self.profile.as_mut() {
                        let chunk = &self.code.functions[fn_id as usize];
                        profile.record_function_entry(chunk, fn_id);
                    }
                    ip = 0;
                }

                TAIL_CALL_SELF_THIN => {
                    let argc = read_u8!(code, ip) as usize;
                    let _owned_mask = read_u8!(code, ip);
                    let args_start = self.stack.len() - argc;
                    // Thin frame: no heap alloc, no arena work.
                    // Just copy args in-place and reset ip.
                    for i in 0..argc {
                        self.stack[bp + i] = self.stack[args_start + i];
                    }
                    let lc = self.frames.last().unwrap().local_count as usize;
                    for i in argc..lc {
                        self.stack[bp + i] = NanValue::UNIT;
                    }
                    self.stack.truncate(bp + lc);
                    if let Some(profile) = self.profile.as_mut() {
                        let chunk = &self.code.functions[fn_id as usize];
                        profile.record_function_entry(chunk, fn_id);
                    }
                    ip = 0;
                }

                TAIL_CALL_KNOWN => {
                    let target_fn_id = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;
                    let _owned_mask = read_u8!(code, ip);
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
                    if let Some(profile) = self.profile.as_mut() {
                        let target = self.code.get(target_fn_id);
                        profile.record_function_entry(target, target_fn_id);
                    }
                    fn_id = target_fn_id;
                    ip = 0;
                }

                RETURN => {
                    // Fast path: frameless leaf return.
                    if let Some((saved_fn_id, saved_ip, saved_bp)) = leaf_return.take() {
                        let result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                        self.stack.truncate(bp);
                        self.stack.push(result);
                        fn_id = saved_fn_id;
                        ip = saved_ip;
                        bp = saved_bp;
                        continue;
                    }

                    let mut result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let frame = self.frames.pop().unwrap();
                    self.stack.truncate(frame.bp as usize);
                    // Flatten deep lists before frame return to avoid stack
                    // overflow during arena evacuation of Prepend/Concat chains.
                    if !self.can_fast_return(&frame) {
                        result = self.arena.flatten_deep_list(result);
                    }
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
                        return Err(VmError::runtime("List.len() argument must be a List"));
                    }
                    self.stack.push(NanValue::new_int(
                        self.arena.list_len_value(list) as i64,
                        &mut self.arena,
                    ));
                }

                // LIST_GET, LIST_GET_MATCH, LIST_APPEND handlers removed
                // (List.get and List.append removed from language).
                LIST_PREPEND => {
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if !list.is_list() {
                        return Err(VmError::runtime(
                            "List.prepend() second argument must be a List",
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
                    self.stack.push(NanValue::EMPTY_LIST);
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
                    if items.is_empty() {
                        self.stack.push(NanValue::EMPTY_LIST);
                        continue;
                    }
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

                CALL_PAR => {
                    let count = read_u8!(code, ip) as usize;
                    let unwrap = read_u8!(code, ip) != 0;

                    // Read call descriptors: argc:u8 × count
                    let mut descs = Vec::with_capacity(count);
                    for _ in 0..count {
                        let argc = read_u8!(code, ip) as usize;
                        descs.push(argc);
                    }

                    // Pop callable values plus args from stack.
                    let total_items: usize = descs.iter().map(|argc| argc + 1).sum();
                    let items_start = self.stack.len() - total_items;
                    let flat_items: Vec<NanValue> = self.stack[items_start..].to_vec();
                    self.stack.truncate(items_start);

                    // Save caller IP — drop code borrow before call_function
                    self.frames.last_mut().unwrap().ip = ip as u32;
                    let _saved_fn_id = fn_id;
                    let caller_fn_id = fn_id;
                    let caller_ip = ip;

                    // Enter replay group
                    self.runtime.replay_enter_group();

                    // Build per-element callable + arg bundles in source order.
                    let mut element_calls: Vec<(NanValue, Vec<NanValue>)> =
                        Vec::with_capacity(count);
                    let mut item_offset = 0;
                    for argc in &descs {
                        let callable = flat_items[item_offset];
                        item_offset += 1;
                        let args = flat_items[item_offset..item_offset + *argc].to_vec();
                        item_offset += *argc;
                        element_calls.push((callable, args));
                    }

                    // Check if recording/replaying — if so, run sequentially
                    // (replay state is thread_local, can't share across threads)
                    // Check if recording/replaying — if so, sequential
                    let is_tracking = self.runtime.is_effect_tracking();
                    let mut had_vm_error: Option<VmError> = None;
                    let results = if is_tracking || count <= 1 {
                        let mut results = Vec::with_capacity(count);
                        for (i, (callable, args)) in element_calls.iter().enumerate() {
                            self.runtime.replay_set_branch(i as u32);
                            let result = self.invoke_callable_value(
                                *callable,
                                args,
                                caller_fn_id,
                                caller_ip,
                            )?;
                            results.push(result);
                        }
                        results
                    } else {
                        // Parallel: spawn a child VM per element
                        let (parallel_base_code, parallel_base_globals, parallel_base_arena) =
                            self.build_parallel_base_context();
                        let allowed_effects = self.runtime.allowed_effects().to_vec();
                        let cli_args = self.runtime.cli_args().to_vec();
                        let silent_console = self.runtime.silent_console();
                        let runtime_policy = self.runtime.runtime_policy().cloned();
                        let independence_mode = self.runtime.independence_mode();
                        let cancel_mode =
                            independence_mode == crate::config::IndependenceMode::Cancel;
                        let sequential_mode =
                            independence_mode == crate::config::IndependenceMode::Sequential;
                        let prepared_calls: Vec<(NanValue, Vec<NanValue>, Arena)> = element_calls
                            .iter()
                            .map(|(callable, args)| {
                                let mut child_arena = parallel_base_arena.clone_static();
                                let child_callable =
                                    child_arena.deep_import(*callable, &self.arena);
                                let child_args = args
                                    .iter()
                                    .map(|arg| child_arena.deep_import(*arg, &self.arena))
                                    .collect();
                                (child_callable, child_args, child_arena)
                            })
                            .collect();

                        if cancel_mode && unwrap {
                            // Cancel mode with ?!: cooperative cancellation via shared flag.
                            // When a branch returns Result.Err, set the flag so siblings
                            // can bail early via the VM's periodic cancellation check.
                            use std::sync::{
                                Arc,
                                atomic::{AtomicBool, Ordering},
                            };

                            #[allow(clippy::type_complexity)]
                            let tasks: Vec<
                                Box<
                                    dyn FnOnce(
                                            Arc<AtomicBool>,
                                        )
                                            -> Result<(NanValue, Arena), VmError>
                                        + Send,
                                >,
                            > = descs
                                .iter()
                                .zip(prepared_calls)
                                .map(|(_, (callable, args, arena))| {
                                    let code = parallel_base_code.clone();
                                    let globals = parallel_base_globals.clone();
                                    let effects = allowed_effects.clone();
                                    let cli_args = cli_args.clone();
                                    let runtime_policy = runtime_policy.clone();
                                    Box::new(move |flag: Arc<AtomicBool>| {
                                        let mut child_vm = VM::new(code, globals, arena);
                                        child_vm.set_allowed_effects(effects);
                                        child_vm.set_cli_args(cli_args);
                                        child_vm.set_silent_console(silent_console);
                                        if let Some(config) = runtime_policy {
                                            child_vm.set_runtime_policy(config);
                                        }
                                        child_vm.set_cancelled(flag.clone());
                                        let result = child_vm.invoke_callable_value(
                                            callable,
                                            &args,
                                            caller_fn_id,
                                            caller_ip,
                                        )?;
                                        if result.is_err() {
                                            flag.store(true, Ordering::Relaxed);
                                        }
                                        Ok((result, child_vm.arena))
                                    })
                                        as Box<
                                            dyn FnOnce(
                                                    Arc<AtomicBool>,
                                                )
                                                    -> Result<(NanValue, Arena), VmError>
                                                + Send,
                                        >
                                })
                                .collect();

                            let par_results = aver_rt::par_execute_with_cancel(tasks);
                            let mut results = Vec::with_capacity(count);
                            for r in par_results {
                                match r {
                                    Ok((value, child_arena)) => {
                                        let imported = self.arena.deep_import(value, &child_arena);
                                        results.push(imported);
                                    }
                                    Err(e) => {
                                        // Cancelled branch — remember error but don't bail yet.
                                        // A real Result.Err from another branch takes priority
                                        // during ?! unwrap.
                                        if had_vm_error.is_none() {
                                            had_vm_error = Some(e);
                                        }
                                        // Push a sentinel — won't be Ok or Err, so unwrap
                                        // will skip it in favor of real branch errors.
                                        results.push(NanValue::UNIT);
                                    }
                                }
                            }
                            // If all branches returned Ok values but some were cancelled,
                            // propagate the VM error (e.g. only cancellations, no results).
                            // If any branch has a real Result.Err, unwrap will find it first.
                            results
                        } else {
                            // Complete mode: all branches run to completion
                            #[allow(clippy::type_complexity)]
                            let tasks: Vec<
                                Box<dyn FnOnce() -> Result<(NanValue, Arena), VmError> + Send>,
                            > = descs
                                .iter()
                                .zip(prepared_calls)
                                .map(|(_, (callable, args, arena))| {
                                    let code = parallel_base_code.clone();
                                    let globals = parallel_base_globals.clone();
                                    let effects = allowed_effects.clone();
                                    let cli_args = cli_args.clone();
                                    let runtime_policy = runtime_policy.clone();
                                    Box::new(move || {
                                        let mut child_vm = VM::new(code, globals, arena);
                                        child_vm.set_allowed_effects(effects);
                                        child_vm.set_cli_args(cli_args);
                                        child_vm.set_silent_console(silent_console);
                                        if let Some(config) = runtime_policy {
                                            child_vm.set_runtime_policy(config);
                                        }
                                        let result = child_vm.invoke_callable_value(
                                            callable,
                                            &args,
                                            caller_fn_id,
                                            caller_ip,
                                        )?;
                                        Ok((result, child_vm.arena))
                                    })
                                        as Box<
                                            dyn FnOnce() -> Result<(NanValue, Arena), VmError>
                                                + Send,
                                        >
                                })
                                .collect();

                            let par_results = if sequential_mode {
                                aver_rt::par_execute_sequential(tasks)
                            } else {
                                aver_rt::par_execute(tasks)
                            };
                            let mut results = Vec::with_capacity(count);
                            for r in par_results {
                                let (value, child_arena) = r?;
                                let imported = self.arena.deep_import(value, &child_arena);
                                results.push(imported);
                            }
                            results
                        }
                    };

                    // Exit replay group
                    self.runtime.replay_exit_group();

                    if unwrap {
                        // ?! — unwrap each Result.
                        // First pass: prefer a real Result.Err over cancellation errors.
                        // A real Err propagates immediately; cancelled sentinels (UNIT)
                        // are skipped in this pass.
                        let mut unwrapped = Vec::with_capacity(count);
                        let mut first_real_err: Option<NanValue> = None;
                        for v in &results {
                            if v.is_ok() {
                                unwrapped.push(v.wrapper_inner(&self.arena));
                            } else if v.is_err() {
                                first_real_err = Some(*v);
                                break;
                            } else if v.is_unit() {
                                // Cancelled branch sentinel — skip for now
                                continue;
                            } else {
                                return Err(VmError::runtime(
                                    "Independent product '?!' requires all elements to be Result",
                                ));
                            }
                        }

                        // Propagate: real Err takes priority, then cancellation VmError
                        if let Some(err_val) = first_real_err {
                            let frame = self.frames.pop().unwrap();
                            self.stack.truncate(frame.bp as usize);
                            match self.complete_frame_return(frame, err_val, caller_depth) {
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

                        // No real Err — check if we had cancelled branches
                        if let Some(vm_err) = had_vm_error {
                            return Err(vm_err);
                        }
                        let tuple_idx = self.arena.push_tuple(unwrapped);
                        self.stack.push(NanValue::new_tuple(tuple_idx));
                    } else {
                        let tuple_idx = self.arena.push_tuple(results);
                        self.stack.push(NanValue::new_tuple(tuple_idx));
                    }
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
                    return Err(VmError::type_err(
                        "error propagation expects a Result value",
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
                        return Err(VmError::type_err("RECORD_UPDATE on non-record"));
                    }
                    let (type_id, old_fields) = self.arena.get_record(base.arena_index());
                    if type_id != expected_type_id {
                        return Err(VmError::runtime(format!(
                            "record update type mismatch: expected {}, got {}",
                            self.arena.get_type_name(expected_type_id),
                            self.arena.get_type_name(type_id)
                        )));
                    }

                    let mut fields = old_fields.to_vec();
                    for offset in (0..count).rev() {
                        let field_idx = code[field_indices_start + offset] as usize;
                        if field_idx >= fields.len() {
                            return Err(VmError::runtime("record update field out of bounds"));
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
                            return Err(VmError::runtime("field index out of bounds"));
                        }
                    } else {
                        return Err(VmError::type_err("RECORD_GET on non-record"));
                    }
                }

                RECORD_GET_NAMED => {
                    let field_symbol_id = read_u32!(code, ip);

                    let record = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if record.is_record() {
                        let (type_id, fields) = self.arena.get_record(record.arena_index());
                        if let Some(&field_idx) = self
                            .code
                            .record_field_slots
                            .get(&(type_id, field_symbol_id))
                        {
                            self.stack.push(fields[field_idx as usize]);
                        } else {
                            let field_name = self
                                .code
                                .symbols
                                .get(field_symbol_id)
                                .map(|info| info.name.as_str())
                                .unwrap_or("<unknown>");
                            return Err(VmError::runtime(format!(
                                "record has no field '{}'",
                                field_name
                            )));
                        }
                    } else if let Some(symbol_id) = self.decode_vm_symbol_id(record)
                        && self.code.symbols.is_namespace(symbol_id)
                    {
                        if let Some(mut value) =
                            self.code.symbols.resolve_member(symbol_id, field_symbol_id)
                        {
                            if let Some(member_symbol_id) = self.decode_vm_symbol_id(value)
                                && let Some(ctor) =
                                    self.code.symbols.resolve_variant_ctor(member_symbol_id)
                                && ctor.field_count == 0
                            {
                                value = NanValue::new_nullary_variant(
                                    self.arena.push_nullary_variant_symbol(ctor.ctor_id),
                                );
                            }
                            self.stack.push(value);
                        } else {
                            let namespace = self
                                .code
                                .symbols
                                .get(symbol_id)
                                .map(|info| info.name.as_str())
                                .unwrap_or("<namespace>");
                            let field_name = self
                                .code
                                .symbols
                                .get(field_symbol_id)
                                .map(|info| info.name.as_str())
                                .unwrap_or("<unknown>");
                            return Err(VmError::runtime(format!(
                                "namespace {} has no member '{}'",
                                namespace, field_name
                            )));
                        }
                    } else {
                        return Err(VmError::type_err(format!(
                            "field access on non-record value ({})",
                            self.value_type_name(record)
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
                        self.stack.push(NanValue::new_nullary_variant(
                            self.arena.push_nullary_variant_symbol(ctor_id),
                        ));
                    } else if fields.len() == 1 {
                        if let Some(ctor_id) = self.arena.find_ctor_id(type_id, variant_id)
                            && let Some(iv) = NanValue::try_new_inline_variant(ctor_id, fields[0])
                        {
                            self.stack.push(iv);
                        } else {
                            let idx = self
                                .arena
                                .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                                    arena.push_variant(type_id, variant_id, fields)
                                });
                            self.stack.push(NanValue::new_variant(idx));
                        }
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
                            _ => Err(VmError::runtime("invalid wrap kind")),
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
                    if self.variant_ctor_id_vm(top) != Some(expected_ctor) {
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

                UNWRAP_OR => {
                    let default = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let option = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if option.is_some() {
                        self.stack.push(option.wrapper_inner(&self.arena));
                    } else {
                        self.stack.push(default);
                    }
                }

                VECTOR_GET => {
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let vec = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if vec.is_empty_vector_immediate() {
                        self.stack.push(NanValue::NONE);
                    } else {
                        let items = self.arena.vector_ref_value(vec);
                        let idx = index.as_int(&self.arena);
                        if idx >= 0 && (idx as usize) < items.len() {
                            self.stack.push(NanValue::new_some_value(
                                items[idx as usize],
                                &mut self.arena,
                            ));
                        } else {
                            self.stack.push(NanValue::NONE);
                        }
                    }
                }

                VECTOR_GET_OR => {
                    let const_idx = read_u16!(code, ip) as usize;
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let vec = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let default = self.code.functions[fn_id as usize].constants[const_idx];
                    if vec.is_empty_vector_immediate() {
                        self.stack.push(default);
                    } else {
                        let items = self.arena.vector_ref_value(vec);
                        let idx = index.as_int(&self.arena);
                        if idx >= 0 && (idx as usize) < items.len() {
                            self.stack.push(items[idx as usize]);
                        } else {
                            self.stack.push(default);
                        }
                    }
                }

                VECTOR_SET => {
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let vec = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = index.as_int(&self.arena);
                    if vec.is_empty_vector_immediate() || idx < 0 {
                        self.stack.push(NanValue::NONE);
                    } else {
                        let mut items = self.arena.clone_vector_value(vec);
                        let i = idx as usize;
                        if i < items.len() {
                            items[i] = value;
                            let new_idx = self.arena.push_vector(items);
                            let new_vec = NanValue::new_vector(new_idx);
                            self.stack
                                .push(NanValue::new_some_value(new_vec, &mut self.arena));
                        } else {
                            self.stack.push(NanValue::NONE);
                        }
                    }
                }

                VECTOR_SET_OR_KEEP => {
                    let vec_owned = read_u8!(code, ip) != 0;
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let vec = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = index.as_int(&self.arena);
                    if vec.is_empty_vector_immediate() || idx < 0 {
                        self.stack.push(vec);
                    } else if vec_owned && !vec.is_empty_vector_immediate() {
                        // Owned path: modify vector in-place at the same arena slot.
                        // No new allocation, no promotion needed.
                        let items = self.arena.get_vector_mut(vec.arena_index());
                        let i = idx as usize;
                        if i < items.len() {
                            items[i] = value;
                        }
                        // Return the same NanValue — same slot, same space.
                        self.stack.push(vec);
                    } else {
                        let items = self.arena.vector_ref_value(vec);
                        let i = idx as usize;
                        if i < items.len() {
                            let mut updated = items.to_vec();
                            updated[i] = value;
                            let new_idx = self.arena.push_vector(updated);
                            self.stack.push(NanValue::new_vector(new_idx));
                        } else {
                            self.stack.push(vec);
                        }
                    }
                }

                UNWRAP_RESULT_OR => {
                    let default = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    if result.is_ok() {
                        self.stack.push(result.wrapper_inner(&self.arena));
                    } else {
                        self.stack.push(default);
                    }
                }

                MATCH_NIL => {
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let is_nil = top.is_list() && self.arena.list_is_empty_value(top);
                    if !is_nil {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                MATCH_CONS => {
                    let offset = read_i16!(code, ip);
                    let top = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    let is_cons = top.is_list() && !self.arena.list_is_empty_value(top);
                    if !is_cons {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }

                LIST_HEAD_TAIL => {
                    let list = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let Some((head, tail)) = self.arena.list_uncons(list) else {
                        return Err(VmError::runtime("LIST_HEAD_TAIL on empty list"));
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
                    } else if top.is_inline_variant() {
                        debug_assert_eq!(field_idx, 0);
                        self.stack.push(top.inline_variant_inner());
                    } else if top.is_variant() {
                        let (_, _, fields) = top
                            .variant_parts(&self.arena)
                            .ok_or_else(|| VmError::type_err("EXTRACT_FIELD on invalid variant"))?;
                        self.stack.push(fields[field_idx]);
                    } else {
                        return Err(VmError::type_err("EXTRACT_FIELD on non-record/variant"));
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
                        return Err(VmError::type_err("EXTRACT_TUPLE_ITEM on non-tuple"));
                    }
                    let items = self.arena.get_tuple(top.arena_index());
                    if item_idx >= items.len() {
                        return Err(VmError::runtime("tuple index out of bounds"));
                    }
                    self.stack.push(items[item_idx]);
                }

                MATCH_FAIL => {
                    let line = read_u16!(code, ip);
                    return Err(VmError::MatchFail(line));
                }

                MATCH_DISPATCH => {
                    /// QNAN (14 bits) + tag (4 bits) = top 18 bits.
                    const TAG_MASK_FULL: u64 = 0xFFFF_C000_0000_0000;

                    let count = read_u8!(code, ip) as usize;
                    let default_offset = read_i16!(code, ip);
                    // Subject stays on the stack — each arm body is responsible
                    // for popping it (with optional unwrap/bind beforehand).
                    let bits = self.stack.last().ok_or(VmError::StackUnderflow)?.bits();

                    let table_start = ip;
                    // Each entry: kind:u8 + expected:u64 + offset:i16 = 11 bytes
                    let table_end = ip + count * 11;

                    let mut matched_offset: Option<i16> = None;
                    let mut scan_ip = table_start;
                    for _ in 0..count {
                        let kind = code[scan_ip];
                        let expected = u64::from_be_bytes([
                            code[scan_ip + 1],
                            code[scan_ip + 2],
                            code[scan_ip + 3],
                            code[scan_ip + 4],
                            code[scan_ip + 5],
                            code[scan_ip + 6],
                            code[scan_ip + 7],
                            code[scan_ip + 8],
                        ]);
                        let offset = i16::from_be_bytes([code[scan_ip + 9], code[scan_ip + 10]]);
                        scan_ip += 11;

                        let hit = match kind {
                            0 => bits == expected,                   // exact match
                            1 => (bits & TAG_MASK_FULL) == expected, // tag prefix
                            2 => {
                                // String deep equality: compare via arena
                                let subject = NanValue::from_bits(bits);
                                let pattern = NanValue::from_bits(expected);
                                subject.string_eq(pattern, &self.arena)
                            }
                            _ => false,
                        };
                        if hit {
                            matched_offset = Some(offset);
                            break;
                        }
                    }

                    ip = table_end;
                    let jump = matched_offset.unwrap_or(default_offset);
                    ip = (ip as isize + jump as isize) as usize;
                }

                MATCH_DISPATCH_CONST => {
                    const TAG_MASK_FULL: u64 = 0xFFFF_C000_0000_0000;

                    let count = read_u8!(code, ip) as usize;
                    let default_offset = read_i16!(code, ip);
                    let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let bits = val.bits();

                    let table_start = ip;
                    // Each entry: kind:u8 + expected:u64 + result:u64 = 17 bytes
                    let table_end = ip + count * 17;

                    let mut matched_result: Option<NanValue> = None;
                    let mut scan_ip = table_start;
                    for _ in 0..count {
                        let kind = code[scan_ip];
                        let expected = u64::from_be_bytes([
                            code[scan_ip + 1],
                            code[scan_ip + 2],
                            code[scan_ip + 3],
                            code[scan_ip + 4],
                            code[scan_ip + 5],
                            code[scan_ip + 6],
                            code[scan_ip + 7],
                            code[scan_ip + 8],
                        ]);
                        let result_bits = u64::from_be_bytes([
                            code[scan_ip + 9],
                            code[scan_ip + 10],
                            code[scan_ip + 11],
                            code[scan_ip + 12],
                            code[scan_ip + 13],
                            code[scan_ip + 14],
                            code[scan_ip + 15],
                            code[scan_ip + 16],
                        ]);
                        scan_ip += 17;

                        let hit = match kind {
                            0 => bits == expected,
                            1 => (bits & TAG_MASK_FULL) == expected,
                            2 => {
                                let subject = NanValue::from_bits(bits);
                                let pattern = NanValue::from_bits(expected);
                                subject.string_eq(pattern, &self.arena)
                            }
                            _ => false,
                        };
                        if hit {
                            matched_result = Some(NanValue::from_bits(result_bits));
                            break;
                        }
                    }

                    ip = table_end;
                    if let Some(result) = matched_result {
                        self.stack.push(result);
                    } else {
                        // No match — execute default arm body.
                        // Push subject back (default body expects it on stack).
                        self.stack.push(val);
                        ip = (ip as isize + default_offset as isize) as usize;
                    }
                }

                _ => {
                    return Err(VmError::runtime(format!("unknown opcode: 0x{:02X}", op)));
                }
            }
        }
    }
}
