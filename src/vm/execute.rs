use super::opcode::*;
use super::runtime::{VmExecutionMode, VmRuntime, is_http_server_builtin};
use super::types::{CallFrame, CodeStore, VmError};
use crate::nan_value::{AllocSpace, Arena, NanValue};

/// The Aver bytecode virtual machine.
pub struct VM {
    stack: Vec<NanValue>,
    frames: Vec<CallFrame>,
    globals: Vec<NanValue>,
    code: CodeStore,
    pub arena: Arena,
    runtime: VmRuntime,
}

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
    pub fn new(code: CodeStore, globals: Vec<NanValue>, arena: Arena) -> Self {
        VM {
            stack: Vec::with_capacity(1024),
            frames: Vec::with_capacity(64),
            globals,
            code,
            arena,
            runtime: VmRuntime::new(),
        }
    }

    /// Set CLI arguments for Args.get().
    pub fn set_cli_args(&mut self, args: Vec<String>) {
        self.runtime.set_cli_args(args);
    }

    /// Start recording effectful calls.
    pub fn start_recording(&mut self) {
        self.runtime.start_recording();
    }

    /// Start replaying from recorded effects.
    pub fn start_replay(
        &mut self,
        effects: Vec<crate::replay::session::EffectRecord>,
        validate_args: bool,
    ) {
        self.runtime.start_replay(effects, validate_args);
    }

    pub fn recorded_effects(&self) -> &[crate::replay::session::EffectRecord] {
        self.runtime.recorded_effects()
    }

    pub fn replay_progress(&self) -> (usize, usize) {
        self.runtime.replay_progress()
    }

    pub fn ensure_replay_consumed(&self) -> Result<(), VmError> {
        self.runtime.ensure_replay_consumed()
    }

    pub fn run(&mut self) -> Result<NanValue, VmError> {
        self.run_top_level()?;
        self.run_named_function("main", &[])
    }

    pub fn run_top_level(&mut self) -> Result<(), VmError> {
        if let Some(top_id) = self.code.find("__top_level__") {
            let _ = self.call_function(top_id, &[])?;
        }
        Ok(())
    }

    pub fn run_named_function(
        &mut self,
        name: &str,
        args: &[NanValue],
    ) -> Result<NanValue, VmError> {
        let fn_id = self
            .code
            .find(name)
            .ok_or_else(|| VmError::Runtime(format!("function '{}' not found", name)))?;
        self.runtime
            .set_allowed_effects(self.code.get(fn_id).effects.clone());
        self.call_function(fn_id, args)
    }

    pub fn call_function(&mut self, fn_id: u32, args: &[NanValue]) -> Result<NanValue, VmError> {
        let chunk = self.code.get(fn_id);
        let caller_depth = self.frames.len();
        let arena_mark = self.arena.young_len() as u32;
        let yard_mark = self.arena.yard_len() as u32;
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
            arena_mark,
            yard_mark,
            globals_dirty: false,
            yard_dirty: false,
        });
        self.execute_until(caller_depth)
    }

    fn collect_stable_roots(&mut self, frame_roots: &mut [NanValue]) {
        let root_count = frame_roots.len();
        let mut all_roots = Vec::with_capacity(root_count + self.globals.len());
        all_roots.extend_from_slice(frame_roots);
        all_roots.extend(self.globals.iter().copied());
        self.arena.collect_stable_from_roots(&mut all_roots);

        frame_roots.copy_from_slice(&all_roots[..root_count]);
        for (dst, src) in self
            .globals
            .iter_mut()
            .zip(all_roots[root_count..].iter().copied())
        {
            *dst = src;
        }
    }

    fn finalize_frame_locals_for_tail_call(
        &mut self,
        arena_mark: u32,
        yard_mark: u32,
        globals_dirty: bool,
        yard_dirty: bool,
        frame_roots: &mut [NanValue],
    ) {
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }

        if self.arena.young_len() > arena_mark as usize {
            self.arena
                .promote_young_roots_to_yard(arena_mark, frame_roots);
        }

        if yard_dirty && self.arena.yard_len() > yard_mark as usize {
            self.arena.collect_yard_from_roots(yard_mark, frame_roots);
        }
    }

    fn finalize_frame_return_to_caller(
        &mut self,
        arena_mark: u32,
        yard_mark: u32,
        globals_dirty: bool,
        frame_roots: &mut [NanValue],
    ) -> bool {
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }

        let had_callee_yard = self.arena.yard_len() > yard_mark as usize;
        let young_escape = frame_roots
            .iter()
            .filter_map(|value| value.heap_index())
            .any(|index| self.arena.is_young_index_in_region(index, arena_mark));

        if self.arena.young_len() > arena_mark as usize {
            self.arena
                .promote_young_roots_to_yard(arena_mark, frame_roots);
        }

        if had_callee_yard && self.arena.yard_len() > yard_mark as usize {
            self.arena.collect_yard_from_roots(yard_mark, frame_roots);
        }

        self.arena.truncate_to(arena_mark);
        had_callee_yard || young_escape
    }

    fn finalize_frame_return(
        &mut self,
        arena_mark: u32,
        yard_mark: u32,
        globals_dirty: bool,
        frame_roots: &mut [NanValue],
    ) {
        if globals_dirty {
            self.arena.promote_roots_to_stable(&mut self.globals);
        }
        self.arena.promote_roots_to_stable(frame_roots);
        self.arena.truncate_to(arena_mark);
        self.arena.truncate_yard_to(yard_mark);
    }

    fn next_value_alloc_space(&self, code: &[u8], ip: usize) -> AllocSpace {
        if matches!(code.get(ip).copied(), Some(op) if op == TAIL_CALL_SELF || op == TAIL_CALL_KNOWN)
        {
            AllocSpace::Yard
        } else {
            AllocSpace::Young
        }
    }

    fn execute_until(&mut self, caller_depth: usize) -> Result<NanValue, VmError> {
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
                                frame.yard_mark,
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
                        arena_mark: self.arena.young_len() as u32,
                        yard_mark: self.arena.yard_len() as u32,
                        globals_dirty: false,
                        yard_dirty: false,
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
                        yard_mark: self.arena.yard_len() as u32,
                        globals_dirty: false,
                        yard_dirty: false,
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

                    // Collect args from stack.
                    let args_start = self.stack.len() - argc;
                    let args: Vec<NanValue> = self.stack[args_start..].to_vec();
                    self.stack.truncate(args_start);

                    // HttpServer.listen/listenWith: special case — needs VM callback.
                    if is_http_server_builtin(&builtin_name) {
                        // Save execute loop state before calling into HttpServer.
                        self.frames.last_mut().unwrap().ip = ip as u32;
                        let result = self.dispatch_http_server(&builtin_name, &args)?;
                        self.stack.push(result);
                        // Restore cached state (dispatch may have modified frames).
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
                    let globals_dirty = self.frames.last().unwrap().globals_dirty;
                    let yard_dirty = self.frames.last().unwrap().yard_dirty;
                    let mut promoted_args = self.stack[args_start..].to_vec();
                    self.finalize_frame_locals_for_tail_call(
                        frame_mark,
                        yard_mark,
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
                    ip = 0;
                }

                TAIL_CALL_KNOWN => {
                    let target_fn_id = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;
                    let target_local_count = self.code.get(target_fn_id).local_count;

                    let args_start = self.stack.len() - argc;
                    let frame_mark = self.frames.last().unwrap().arena_mark;
                    let yard_mark = self.frames.last().unwrap().yard_mark;
                    let globals_dirty = self.frames.last().unwrap().globals_dirty;
                    let yard_dirty = self.frames.last().unwrap().yard_dirty;
                    let mut promoted_args = self.stack[args_start..].to_vec();
                    self.finalize_frame_locals_for_tail_call(
                        frame_mark,
                        yard_mark,
                        globals_dirty,
                        yard_dirty,
                        &mut promoted_args,
                    );
                    self.stack[bp..(argc + bp)].copy_from_slice(&promoted_args[..argc]);

                    let new_lc = target_local_count as usize;
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
                    frame.local_count = target_local_count;
                    frame.globals_dirty = false;
                    frame.yard_dirty = false;
                    fn_id = target_fn_id;
                    ip = 0;
                }

                RETURN => {
                    let mut result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let frame = self.frames.pop().unwrap();
                    self.stack.truncate(frame.bp as usize);

                    if self.frames.len() == caller_depth {
                        self.finalize_frame_return(
                            frame.arena_mark,
                            frame.yard_mark,
                            frame.globals_dirty,
                            std::slice::from_mut(&mut result),
                        );
                        if caller_depth == 0 {
                            self.collect_stable_roots(std::slice::from_mut(&mut result));
                        }
                        return Ok(result);
                    }

                    let yard_dirty = self.finalize_frame_return_to_caller(
                        frame.arena_mark,
                        frame.yard_mark,
                        frame.globals_dirty,
                        std::slice::from_mut(&mut result),
                    );
                    self.frames.last_mut().unwrap().yard_dirty |= yard_dirty;
                    self.stack.push(result);

                    // Restore caller frame state.
                    let caller = self.frames.last().unwrap();
                    fn_id = caller.fn_id;
                    ip = caller.ip as usize;
                    bp = caller.bp as usize;
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
                        let inner = self.arena.get_boxed(value.wrapper_index());
                        *self.stack.last_mut().ok_or(VmError::StackUnderflow)? = inner;
                        continue;
                    }
                    if value.is_err() {
                        let mut result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                        let frame = self.frames.pop().unwrap();
                        self.stack.truncate(frame.bp as usize);

                        if self.frames.len() == caller_depth {
                            self.finalize_frame_return(
                                frame.arena_mark,
                                frame.yard_mark,
                                frame.globals_dirty,
                                std::slice::from_mut(&mut result),
                            );
                            if caller_depth == 0 {
                                self.collect_stable_roots(std::slice::from_mut(&mut result));
                            }
                            return Ok(result);
                        }

                        let yard_dirty = self.finalize_frame_return_to_caller(
                            frame.arena_mark,
                            frame.yard_mark,
                            frame.globals_dirty,
                            std::slice::from_mut(&mut result),
                        );
                        self.frames.last_mut().unwrap().yard_dirty |= yard_dirty;
                        self.stack.push(result);

                        let caller = self.frames.last().unwrap();
                        fn_id = caller.fn_id;
                        ip = caller.ip as usize;
                        bp = caller.bp as usize;
                        continue;
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
                    self.stack.pop().ok_or(VmError::StackUnderflow)?; // base record

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
                    let idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_variant(type_id, variant_id, fields)
                        });
                    self.stack.push(NanValue::new_variant(idx));
                }

                WRAP => {
                    let kind = read_u8!(code, ip);
                    let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let boxed_idx = self
                        .arena
                        .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                            arena.push_boxed(val)
                        });
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
                        let (_, _, fields) = self.arena.get_variant(top.arena_index());
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

    /// Handle HttpServer.listen/listenWith with VM callback support.
    /// Uses unsafe self-pointer for re-entrant callback into VM.call_function.
    fn dispatch_http_server(&mut self, name: &str, args: &[NanValue]) -> Result<NanValue, VmError> {
        use crate::services::http_server;
        use crate::value::Value;

        // Convert args to Value. Handler fn_id (NanValue::Int) becomes a
        // Value::Int — the callback closure below extracts fn_id from it.
        let val_args: Vec<Value> = args.iter().map(|a| a.to_value(&self.arena)).collect();

        let vm_ptr = self as *mut VM;
        let invoke_handler = |handler: Value, callback_args: Vec<Value>, _entry: String| {
            let vm = unsafe { &mut *vm_ptr };

            // Handler is Value::Int(fn_id) — the VM stores functions as int ids.
            let handler_fn_id = match &handler {
                Value::Int(id) if (*id as usize) < vm.code.functions.len() => *id as u32,
                _ => {
                    return Err(crate::value::RuntimeError::Error(
                        "HttpServer: handler is not a valid VM function".into(),
                    ));
                }
            };

            let nv_args: Vec<NanValue> = callback_args
                .iter()
                .map(|v| NanValue::from_value(v, &mut vm.arena))
                .collect();

            let handler_effects = vm.code.get(handler_fn_id).effects.clone();
            let previous_effects = vm.runtime.swap_allowed_effects(handler_effects);
            let result_nv = match vm.call_function(handler_fn_id, &nv_args) {
                Ok(result) => {
                    vm.runtime.set_allowed_effects(previous_effects);
                    result
                }
                Err(e) => {
                    vm.runtime.set_allowed_effects(previous_effects);
                    return Err(crate::value::RuntimeError::Error(format!("{}", e)));
                }
            };

            Ok(result_nv.to_value(&vm.arena))
        };

        let skip = self.runtime.execution_mode() == VmExecutionMode::Record;
        match http_server::call_with_runtime(name, &val_args, invoke_handler, skip) {
            Some(Ok(val)) => Ok(NanValue::from_value(&val, &mut self.arena)),
            Some(Err(crate::value::RuntimeError::Error(msg))) => Err(VmError::Runtime(msg)),
            Some(Err(e)) => Err(VmError::Runtime(format!("{:?}", e))),
            None => Err(VmError::Runtime(format!(
                "unknown HttpServer builtin: {}",
                name
            ))),
        }
    }

    fn nan_tag(&self, val: NanValue) -> u8 {
        if val.is_float() {
            return 0xFF;
        }
        ((val.bits() >> 46) & 0xF) as u8
    }

    fn decode_vm_fn_ref(
        &self,
        val: NanValue,
        caller_fn_id: u32,
        ip: usize,
    ) -> Result<u32, VmError> {
        if val.is_int() {
            let fn_id = val.as_int(&self.arena);
            if (0..self.code.functions.len() as i64).contains(&fn_id) {
                return Ok(fn_id as u32);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::types::{CallFrame, CodeStore, FnChunk};

    #[test]
    fn reentrant_call_function_returns_nested_result_without_resuming_caller() {
        let mut code = CodeStore::new();

        let caller_const = NanValue::new_int_inline(10);
        let caller_id = code.add_function(FnChunk {
            name: "caller".to_string(),
            arity: 0,
            local_count: 0,
            code: vec![LOAD_CONST, 0, 0, RETURN],
            constants: vec![caller_const],
            effects: Vec::new(),
        });

        let nested_const = NanValue::new_int_inline(20);
        let nested_id = code.add_function(FnChunk {
            name: "nested".to_string(),
            arity: 0,
            local_count: 0,
            code: vec![LOAD_CONST, 0, 0, RETURN],
            constants: vec![nested_const],
            effects: Vec::new(),
        });

        let mut vm = VM::new(code, Vec::new(), Arena::new());
        vm.frames.push(CallFrame {
            fn_id: caller_id,
            ip: 0,
            bp: 0,
            local_count: 0,
            arena_mark: 0,
            yard_mark: 0,
            globals_dirty: false,
            yard_dirty: false,
        });

        let result = vm
            .call_function(nested_id, &[])
            .expect("nested call should return");

        assert_eq!(result.as_int(&vm.arena), 20);
        assert_eq!(vm.frames.len(), 1, "caller frame should remain suspended");
        assert_eq!(vm.frames[0].fn_id, caller_id);
    }
}
