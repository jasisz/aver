use super::{ReturnControl, VM};
use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::vm::builtin::VmBuiltin;
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

macro_rules! read_i64 {
    ($code:expr, $ip:expr) => {{
        let bytes: [u8; 8] = [
            $code[$ip],
            $code[$ip + 1],
            $code[$ip + 2],
            $code[$ip + 3],
            $code[$ip + 4],
            $code[$ip + 5],
            $code[$ip + 6],
            $code[$ip + 7],
        ];
        #[allow(unused_assignments)]
        {
            $ip += 8;
        }
        i64::from_be_bytes(bytes)
    }};
}

impl VM {
    pub(super) fn execute_until(&mut self, caller_depth: usize) -> Result<NanValue, VmError> {
        let mut fn_id = self.frames.last().unwrap().fn_id;
        let mut ip = self.frames.last().unwrap().ip as usize;
        let mut bp = self.frames.last().unwrap().bp as usize;

        // Leaf call state: saved caller context for frameless calls.
        let mut leaf_return: Option<(u32, usize, usize)> = None; // (fn_id, ip, bp)

        // Hoisted bytecode pointer for the current fn. Refreshed only at
        // fn-changing opcodes (CALL_*, TAIL_CALL_*, RETURN, leaf call /
        // leaf return) — not on every dispatch tick. The loop below
        // rebuilds a `&[u8]` slice from `(code_ptr, code_len)` once per
        // iter so existing `code[ip]` / `read_u16!(code, ip)` etc. reads
        // unchanged.
        //
        // Safety: `self.code.functions[fn_id].code` is a `Vec<u8>` whose
        // backing buffer never moves during `execute_until` — bytecode is
        // built once at compile time and read-only at runtime. The raw
        // pointer is reseated in lockstep with `fn_id` updates.
        let (mut code_ptr, mut code_len) = {
            let c = &self.code.functions[fn_id as usize].code;
            (c.as_ptr(), c.len())
        };

        // Profile state is stable across one `execute_until` invocation —
        // `start_profiling` flips it to `Some` before the call; nothing
        // inside the loop turns it on or off. Cache the bool so the hot
        // per-instruction path is one branch instead of an `Option::as_mut`
        // null check + indirect store every tick.
        let profile_active = self.profile.is_some();

        // Local macro: refresh `(code_ptr, code_len)` from current `fn_id`.
        // Used by every arm that mutates `fn_id`.
        macro_rules! refresh_code {
            () => {{
                let c = &self.code.functions[fn_id as usize].code;
                code_ptr = c.as_ptr();
                code_len = c.len();
            }};
        }

        // Local macro: leave the current function carrying an error value,
        // for the two exits that are not `RETURN` (`PROPAGATE_ERR` and the
        // failing branch of `CALL_PAR`). Expands to nothing when the current
        // function does own a `CallFrame`, leaving the caller's ordinary
        // frame-popping path to run.
        //
        // The frame being left may not exist. A callee entered through
        // `CALL_LEAF` pushes no `CallFrame` — its caller's context is parked
        // in `leaf_return` instead — so popping `self.frames` there pops the
        // CALLER's frame and returns the error out of the wrong function.
        // `RETURN` already consults `leaf_return`; every frame exit owes the
        // same check, and owes the same `take()`, or a later `RETURN` would
        // spend a frameless return that has already been used.
        //
        // The wider rule this belongs to: nothing may treat `self.frames.last()`
        // as its own frame without first ruling out `leaf_return`. Sites split
        // three ways.
        //
        // Sites that take their RESUME POSITION from it. Getting this wrong
        // runs the caller's function at this chunk's offset, which is the whole
        // bug class. Five: `RETURN`, the two exits this macro serves, and
        // `is_http_server` under `CALL_BUILTIN` — all four ask `leaf_return`
        // first — plus `is_http_server` under `CALL_VALUE`, which is covered by
        // classification instead: a chunk containing `CALL_VALUE` is never a
        // leaf.
        //
        // Sites that PARK a position in it across a nested call, for whatever
        // walks `self.frames` while that call runs. `CALL_KNOWN` and
        // `CALL_VALUE` sit behind leaf-disqualifying opcodes; `CALL_PAR` does
        // not, and asks `leaf_return` first.
        //
        // Sites that want THE FRAME THAT OWNS THE NEXT BOUNDARY — `STORE_GLOBAL`
        // marking globals dirty, `VECTOR_SET` handing up an escaped in-place
        // write. For a frameless chunk that frame IS the caller's, because
        // `CALL_LEAF` records no arena marks and its return does no boundary
        // work, so these are right as they stand. Their region tests read the
        // caller's older marks, which widens what counts as frame-local — an
        // over-approximation, in the direction that reports more.
        //
        // Everything else that reads `self.frames.last()` (the three tail
        // calls) sits behind an opcode `classify_leaf_chunk` refuses leaf
        // status for.
        macro_rules! leaf_error_return {
            ($result:expr) => {{
                if let Some((saved_fn_id, saved_ip, saved_bp)) = leaf_return.take() {
                    let result = $result;
                    // Drop the leaf's arguments and whatever the enclosing
                    // call had already pushed of its own argument list, the
                    // same way `RETURN`'s frameless path does. `CALL_LEAF`
                    // records no arena marks, so there is nothing else to
                    // unwind: the error value itself stays reachable.
                    self.stack.truncate(bp);
                    self.stack.push(result);
                    fn_id = saved_fn_id;
                    ip = saved_ip;
                    bp = saved_bp;
                    refresh_code!();
                    continue;
                }
            }};
        }

        // Per-call dispatched-opcode counter. Bumped every iteration;
        // checked against `step_limit` in the same 256-op cadence as
        // cancellation so the hot path stays branch-light. Reset by
        // `run_named_function` at the top of every verify case so cases
        // don't share budget.
        let mut step_count: u64 = 0;
        loop {
            // Cooperative cancellation + step-limit: both amortised by
            // checking every 256 opcodes. Step limit defaults to `None`
            // (unlimited, normal `aver run`); verify path installs ~10M.
            if ip & 0xFF == 0 {
                if self.is_cancelled() {
                    return Err(VmError::runtime("cancelled by sibling branch"));
                }
                if let Some(limit) = self.step_limit
                    && step_count >= limit
                {
                    return Err(VmError::StepLimit { limit, line: 0 });
                }
            }
            step_count += 1;

            let code: &[u8] = unsafe { std::slice::from_raw_parts(code_ptr, code_len) };

            // Save position for error reporting (cold-path lookup in line_table).
            self.error_fn_id = fn_id;
            self.error_ip = ip as u32;

            let op = code[ip];
            ip += 1;
            if profile_active && let Some(profile) = self.profile.as_mut() {
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
                    // The globals table outlives every frame, so from here on it
                    // is a holder no walk of the operand stack can see.
                    self.arena.note_held_elsewhere(val);
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
                ADD_INT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    // `Int` is ℤ: non-wrapping, with an i64 fast path inside
                    // `AverInt::add` (promotes to bignum only on overflow).
                    let r = a.as_aver_int(&self.arena).add(&b.as_aver_int(&self.arena));
                    self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                }
                SUB_INT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = a.as_aver_int(&self.arena).sub(&b.as_aver_int(&self.arena));
                    self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                }
                MUL_INT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = a.as_aver_int(&self.arena).mul(&b.as_aver_int(&self.arena));
                    self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                }
                ADD_FLOAT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack
                        .push(NanValue::new_float(a.as_float() + b.as_float()));
                }
                SUB_FLOAT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack
                        .push(NanValue::new_float(a.as_float() - b.as_float()));
                }
                MUL_FLOAT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack
                        .push(NanValue::new_float(a.as_float() * b.as_float()));
                }
                DIV_FLOAT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack
                        .push(NanValue::new_float(a.as_float() / b.as_float()));
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
                        // ℤ negation never overflows (`-i64::MIN` promotes).
                        let r = a.as_aver_int(&self.arena).neg();
                        self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                    } else if a.is_float() {
                        self.stack.push(NanValue::new_float(-a.as_float()));
                    } else {
                        return Err(VmError::type_err("cannot negate non-numeric"));
                    }
                }
                NEG_INT => {
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = a.as_aver_int(&self.arena).neg();
                    self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                }
                NEG_FLOAT => {
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_float(-a.as_float()));
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
                MATCH_INT_LITERAL => {
                    // Fused `match n { LIT -> ...; _ -> ... }` arm
                    // test. Subject sits on top of stack (left there
                    // by `compile_match`); we peek, compare to the
                    // inline immediate, and either fall through to
                    // the arm body or skip it via `fail_offset` —
                    // matching the semantics of the four-opcode
                    // sequence (DUP/LOAD_CONST/EQ/JUMP_IF_FALSE) it
                    // replaces.
                    let imm = read_i64!(code, ip);
                    let offset = read_i16!(code, ip);
                    let subject = *self.stack.last().ok_or(VmError::StackUnderflow)?;
                    // A literal pattern is an `i64`; a ℤ-overflow subject can
                    // never equal it, so match only when the subject's value
                    // fits and equals `imm`.
                    let matches = match subject.inline_int_value() {
                        Some(v) => v == imm,
                        None => subject.as_aver_int(&self.arena).to_i64() == Some(imm),
                    };
                    if !matches {
                        ip = (ip as isize + offset as isize) as usize;
                    }
                }
                EQ_INT => {
                    // Typed `==` for two `Int` operands. Two-tier fast
                    // path:
                    // 1. Bit-equal — both `NanValue`s have identical
                    //    raw `u64` bits → equal (covers the common
                    //    inline-Int = inline-Int case in one
                    //    instruction).
                    // 2. Both inline (no `INT_BIG_BIT` payload, both
                    //    `tag == TAG_INT`) and bits differ → not equal,
                    //    no arena touch.
                    // 3. Otherwise (boxed Int via arena slot, or
                    //    cross-rep boxed-vs-inline) fall back to
                    //    `as_int` which materialises the `i64` and
                    //    compares.
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let eq = if a.bits() == b.bits() {
                        true
                    } else {
                        match (a.inline_int_value(), b.inline_int_value()) {
                            (Some(x), Some(y)) => x == y,
                            // Boxed (ℤ-overflow or i64-overflow) on either
                            // side: compare by canonical value.
                            _ => a.as_aver_int(&self.arena) == b.as_aver_int(&self.arena),
                        }
                    };
                    self.stack.push(NanValue::new_bool(eq));
                }
                LT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(self.compare_lt(a, b)?));
                }
                LT_INT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(
                        a.as_aver_int(&self.arena) < b.as_aver_int(&self.arena),
                    ));
                }
                LT_FLOAT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack
                        .push(NanValue::new_bool(a.as_float() < b.as_float()));
                }
                GT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(self.compare_lt(b, a)?));
                }
                GT_INT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack.push(NanValue::new_bool(
                        a.as_aver_int(&self.arena) > b.as_aver_int(&self.arena),
                    ));
                }
                GT_FLOAT => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    self.stack
                        .push(NanValue::new_bool(a.as_float() > b.as_float()));
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

                CALL_KNOWN | CALL_KNOWN_OWNED => {
                    let target_fn_id = read_u16!(code, ip) as u32;
                    let argc = read_u8!(code, ip) as usize;
                    // `CALL_KNOWN_OWNED` (emitted by the MIR walker) carries a
                    // trailing owned-arg mask. A user-fn callee derives its
                    // parameter ownership from its own alias analysis, so the
                    // mask is inert at the call boundary today — this mirrors
                    // the HIR walker, which ignores owned-ness for known calls
                    // and only ever emits plain `CALL_KNOWN`. The byte is read
                    // to keep the bytecode self-describing; a future cross-call
                    // ownership pass can consume it to un-alias owned params.
                    if op == CALL_KNOWN_OWNED {
                        let _owned_mask = read_u8!(code, ip);
                    }

                    self.frames.last_mut().unwrap().ip = ip as u32;

                    let target = self.code.get(target_fn_id);
                    let new_bp = self.stack.len() - argc;
                    let target_lc = target.local_count as usize;
                    if target_lc < argc {
                        return Err(VmError::runtime(format!(
                            "CALL_KNOWN to fn_id {} with argc={} exceeds local_count={}",
                            target_fn_id, argc, target_lc,
                        )));
                    }
                    for _ in 0..(target_lc - argc) {
                        self.stack.push(NanValue::UNIT);
                    }

                    // Pure no-alloc targets never grow young/yard/handoff,
                    // so the entry marks are never compared on return —
                    // dummy zeros save three length reads per call. The
                    // matching skip lives in RETURN's fast path
                    // (`chunk.thin = true` for no-alloc fns; runtime
                    // length checks always pass).
                    let (arena_mark, yard_mark, handoff_mark, lane_mark) = if target.no_alloc {
                        (0, 0, 0, 0)
                    } else {
                        (
                            self.arena.young_len() as u32,
                            self.arena.yard_len() as u32,
                            self.arena.handoff_len() as u32,
                            self.arena.lane_mark(),
                        )
                    };
                    self.frames.push(CallFrame {
                        fn_id: target_fn_id,
                        ip: 0,
                        bp: new_bp as u32,
                        local_count: target.local_count,
                        arena_mark,
                        yard_base: yard_mark,
                        yard_mark,
                        handoff_mark,
                        lane_base: lane_mark,
                        lane_mark,
                        globals_dirty: false,
                        yard_dirty: false,
                        handoff_dirty: false,
                        inplace_write_escaped: false,
                        thin: target.thin,
                        parent_thin: target.parent_thin,
                    });
                    if let Some(profile) = self.profile.as_mut() {
                        profile.record_function_entry(target, target_fn_id);
                    }

                    fn_id = target_fn_id;
                    refresh_code!();
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
                    refresh_code!();
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
                                self.runtime.ensure_builtin_effects_allowed(
                                    &self.code.symbols,
                                    builtin,
                                    symbol_id,
                                )?;
                                // Unlike the `CALL_BUILTIN` twin below, this
                                // one may park its position in
                                // `self.frames.last()` unconditionally: reaching
                                // it means the chunk contains `CALL_VALUE`, and
                                // `classify_leaf_chunk` refuses leaf status to
                                // any chunk that does — so this chunk always
                                // owns the frame it is writing to.
                                self.frames.last_mut().unwrap().ip = ip as u32;
                                let result = self.dispatch_http_server(builtin, &args)?;
                                self.stack.push(result);
                                let f = self.frames.last().unwrap();
                                fn_id = f.fn_id;
                                ip = f.ip as usize;
                                bp = f.bp as usize;
                                refresh_code!();
                                continue;
                            }

                            // Oracle v1: record who issued this effect
                            // call so trace_event_is_direct can filter
                            // helper-boundary emissions.
                            self.runtime.sync_caller_fn_id(fn_id);
                            let result = self.arena.with_alloc_space(alloc_space, |arena| {
                                self.runtime.invoke_builtin(
                                    &self.code.symbols,
                                    builtin,
                                    symbol_id,
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
                    let target_lc = target.local_count as usize;
                    if target_lc < argc {
                        return Err(VmError::runtime(format!(
                            "CALL_VALUE to fn_id {} with argc={} exceeds local_count={}",
                            target_fn_id, argc, target_lc,
                        )));
                    }
                    for _ in 0..(target_lc - argc) {
                        self.stack.push(NanValue::UNIT);
                    }
                    let lane_mark = self.arena.lane_mark();

                    self.frames.push(CallFrame {
                        fn_id: target_fn_id,
                        ip: 0,
                        bp: new_bp as u32,
                        local_count: target.local_count,
                        arena_mark: self.arena.young_len() as u32,
                        yard_base: self.arena.yard_len() as u32,
                        yard_mark: self.arena.yard_len() as u32,
                        handoff_mark: self.arena.handoff_len() as u32,
                        lane_base: lane_mark,
                        lane_mark,
                        globals_dirty: false,
                        yard_dirty: false,
                        handoff_dirty: false,
                        inplace_write_escaped: false,
                        thin: target.thin,
                        parent_thin: target.parent_thin,
                    });
                    if let Some(profile) = self.profile.as_mut() {
                        profile.record_function_entry(target, target_fn_id);
                    }

                    fn_id = target_fn_id;
                    refresh_code!();
                    ip = 0;
                    bp = new_bp;
                }

                CALL_BUILTIN | CALL_BUILTIN_OWNED => {
                    let symbol_id = read_u32!(code, ip);
                    let argc = read_u8!(code, ip) as usize;
                    let mut owned_mask = if op == CALL_BUILTIN_OWNED {
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
                    // AFTER the truncate, deliberately: with the argument list
                    // off the stack, "no cell holds this slot" and "the argument
                    // cell was the only holder" are the same statement, so
                    // neither the cross-check nor the decision below needs a
                    // correction for the argument's own reference.
                    self.cross_check_owned_mask(builtin, &args, owned_mask);

                    // The static mask keeps everything it granted; the runtime
                    // only ever adds. A map write the compiler declined asks the
                    // running program whether anything still holds the target,
                    // and takes the owned path when the answer is nothing at
                    // all. Restricted to the two builtins
                    // `invoke_builtin_with_owned` actually hands their target
                    // to: a bit set on anything else would be read by nobody.
                    if owned_mask & 1 == 0
                        && matches!(builtin, VmBuiltin::MapSet | VmBuiltin::MapRemove)
                        && let Some(target) = args.first().copied()
                        && self.runtime_owns_map_target(target)
                    {
                        owned_mask |= 1;
                    }

                    // The reverse direction, for vectors: a static grant on
                    // `Vector.set` is a proposal the running program confirms
                    // or revokes. The owned path would `mem::take` the
                    // target's arena entry, and a container that still holds
                    // that slot would read back an empty vector with nothing
                    // failing loudly — so the fence declines in place and the
                    // write copies whenever anything else holds the slot or
                    // the walk is dearer than the copy.
                    if owned_mask & 1 != 0
                        && builtin == VmBuiltin::VectorSet
                        && let Some(target) = args.first().copied()
                        && !self.runtime_confirms_vector_grant(target)
                    {
                        owned_mask &= !1;
                    }

                    if builtin.is_http_server() {
                        self.runtime.ensure_builtin_effects_allowed(
                            &self.code.symbols,
                            builtin,
                            symbol_id,
                        )?;
                        // The server call runs request handlers through a
                        // nested `call_function`, so this chunk's position is
                        // parked in its `CallFrame` across it and read back
                        // afterwards. A chunk entered through `CALL_LEAF` owns
                        // no `CallFrame`: `self.frames.last()` is its CALLER's.
                        // Parking there would overwrite the caller's saved `ip`
                        // with this chunk's, and reading it back would resume
                        // the CALLER's function at THIS chunk's offset. The
                        // interpreter-local `fn_id`/`ip`/`bp` come back from
                        // the nested call untouched, so a frameless chunk just
                        // keeps them — the same rule `RETURN` and the two error
                        // exits follow. `HttpServer.listen` alone in a body is
                        // exactly that shape: `CALL_BUILTIN` does not disqualify
                        // a leaf, so `fn serve(port: Int) -> Unit
                        // HttpServer.listen(port, handleRequest)` is one.
                        let framed = leaf_return.is_none();
                        if framed {
                            self.frames.last_mut().unwrap().ip = ip as u32;
                        }
                        let result = self.dispatch_http_server(builtin, &args)?;
                        self.stack.push(result);
                        if framed {
                            let f = self.frames.last().unwrap();
                            fn_id = f.fn_id;
                            ip = f.ip as usize;
                            bp = f.bp as usize;
                            refresh_code!();
                        }
                        continue;
                    }

                    // Oracle v1: redirect classified-effect calls to an
                    // installed verify-time stub, if present. Keep the
                    // outer execute_until's local fn_id/ip/bp untouched —
                    // call_function uses its own nested execute_until and
                    // returns here; the caller's state lives in these
                    // stack-frame locals, not in self.frames (which
                    // doesn't carry leaf-call state).
                    // Oracle v1: record caller fn_id before effect
                    // dispatch — used by trace_event_is_direct to filter
                    // helper-boundary emissions under verify-trace.
                    self.runtime.sync_caller_fn_id(fn_id);
                    if let Some(stub_fn_id) = self.runtime.oracle_stub_for(builtin.name()) {
                        let result = self.dispatch_oracle_stub(stub_fn_id, &args)?;
                        self.stack.push(result);
                        continue;
                    }

                    // A frameless CALL_LEAF deliberately borrows its caller's
                    // frame and allocation lanes. In that case `frames.last()`
                    // is therefore exactly the frame that owns the next
                    // boundary, not an accidental outer proof. No frame means
                    // no proof and the map update stamps current conservatively.
                    let owned_map_frame_proof = (owned_mask & 1 != 0
                        && builtin == VmBuiltin::MapSet)
                        .then(|| {
                            self.frames
                                .last()
                                .map(|frame| crate::types::map::OwnedMapFrameProof {
                                    arena_mark: frame.arena_mark,
                                    yard_mark: frame.yard_mark,
                                    handoff_mark: frame.handoff_mark,
                                    lane_mark: frame.lane_mark,
                                    inplace_write_escaped: frame.inplace_write_escaped,
                                })
                        })
                        .flatten();
                    let result = self.arena.with_alloc_space(alloc_space, |arena| {
                        self.runtime.invoke_builtin_with_owned(
                            &self.code.symbols,
                            builtin,
                            symbol_id,
                            &args,
                            arena,
                            owned_mask,
                            owned_map_frame_proof,
                        )
                    })?;
                    self.stack.push(result);
                }

                TAIL_CALL_SELF => {
                    let argc = read_u8!(code, ip) as usize;
                    let _owned_mask = read_u8!(code, ip);
                    let args_start = self.stack.len() - argc;

                    // Self-TCO mirror of the TAIL_CALL_KNOWN no-alloc skip:
                    // if the current chunk is alloc-free, the finalizer
                    // call is guaranteed no-op. Existing TAIL_CALL_SELF_THIN
                    // covers self-recursive thin chunks; this branch picks
                    // up bodies that the bytecode classifier rejected for
                    // unrelated reasons (e.g. local_count > MAX) but
                    // `compute_alloc_info` still proves alloc-free.
                    let self_no_alloc = self.code.functions[fn_id as usize].no_alloc;
                    let mut lane_rebased = false;
                    if !self_no_alloc {
                        let frame_mark = self.frames.last().unwrap().arena_mark;
                        let yard_mark = self.frames.last().unwrap().yard_mark;
                        let handoff_mark = self.frames.last().unwrap().handoff_mark;
                        let lane_mark = self.frames.last().unwrap().lane_mark;
                        let globals_dirty = self.frames.last().unwrap().globals_dirty;
                        let yard_dirty = self.frames.last().unwrap().yard_dirty;
                        let inplace_write_escaped =
                            self.frames.last().unwrap().inplace_write_escaped;
                        let mut promoted_args = self.stack[args_start..].to_vec();
                        lane_rebased = self.finalize_frame_locals_for_tail_call(
                            frame_mark,
                            yard_mark,
                            handoff_mark,
                            lane_mark,
                            globals_dirty,
                            yard_dirty,
                            inplace_write_escaped,
                            &mut promoted_args,
                        );
                        self.stack[bp..(argc + bp)].copy_from_slice(&promoted_args[..argc]);
                    } else {
                        self.stack.copy_within(args_start..args_start + argc, bp);
                    }
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
                    if lane_rebased {
                        // The destructive boundary renewed every carried
                        // receipt into the post-bump epoch. Small-young/no-op
                        // paths keep the old mark because their suffix is still
                        // owned by this frame.
                        frame.lane_mark = self.arena.lane_mark();
                    }
                    // `inplace_write_escaped` deliberately does NOT join the
                    // three bits above: those are re-derived from the region
                    // lengths at the next boundary, while it remembers an event
                    // this compaction has not undone — see the field's own doc
                    // on `CallFrame`. Clearing it here loses the element a few
                    // iterations later, as an out-of-bounds arena index in
                    // `tail_call_evacuation_keeps_the_element_written_in_place`.
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
                    let target = self.code.get(target_fn_id);
                    let target_local_count = target.local_count;
                    let target_no_alloc = target.no_alloc;

                    let args_start = self.stack.len() - argc;
                    let mut lane_rebased = false;
                    if !target_no_alloc {
                        // Pure no-alloc targets (e.g. mandelStep ↔ mandelIter)
                        // never produce frame-local young/yard/handoff
                        // survivors, so the boundary finalizer would always
                        // fall through to its no-op branch. Skipping the
                        // call shaves a handful of length reads + branches
                        // per iteration in tight numeric loops.
                        let frame_mark = self.frames.last().unwrap().arena_mark;
                        let yard_mark = self.frames.last().unwrap().yard_mark;
                        let handoff_mark = self.frames.last().unwrap().handoff_mark;
                        let lane_mark = self.frames.last().unwrap().lane_mark;
                        let globals_dirty = self.frames.last().unwrap().globals_dirty;
                        let yard_dirty = self.frames.last().unwrap().yard_dirty;
                        let inplace_write_escaped =
                            self.frames.last().unwrap().inplace_write_escaped;
                        let mut promoted_args = self.stack[args_start..].to_vec();
                        lane_rebased = self.finalize_frame_locals_for_tail_call(
                            frame_mark,
                            yard_mark,
                            handoff_mark,
                            lane_mark,
                            globals_dirty,
                            yard_dirty,
                            inplace_write_escaped,
                            &mut promoted_args,
                        );
                        self.stack[bp..(argc + bp)].copy_from_slice(&promoted_args[..argc]);
                    } else {
                        // Args already on the stack at the right position
                        // (no relocation needed when the finalizer is a
                        // no-op). Just slot them into the frame's locals.
                        self.stack.copy_within(args_start..args_start + argc, bp);
                    }

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
                    if lane_rebased {
                        frame.lane_mark = self.arena.lane_mark();
                    }
                    // `inplace_write_escaped` deliberately does NOT join the
                    // three bits above: those are re-derived from the region
                    // lengths at the next boundary, while it remembers an event
                    // this compaction has not undone — see the field's own doc
                    // on `CallFrame`. Clearing it here loses the element a few
                    // iterations later, as an out-of-bounds arena index in
                    // `tail_call_evacuation_keeps_the_element_written_in_place`.
                    if let Some(profile) = self.profile.as_mut() {
                        let target = self.code.get(target_fn_id);
                        profile.record_function_entry(target, target_fn_id);
                    }
                    fn_id = target_fn_id;
                    refresh_code!();
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
                        refresh_code!();
                        continue;
                    }

                    let frame_no_alloc = self.code.functions[fn_id as usize].no_alloc;
                    let result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let frame = self.frames.pop().unwrap();
                    self.stack.truncate(frame.bp as usize);

                    // Pure no-alloc bodies never produced young/yard/handoff
                    // survivors, so the standard `can_fast_return` length
                    // checks (and the `flatten_deep_list` guard above) are
                    // unnecessary. CALL_KNOWN parks dummy `arena_mark = 0`
                    // for these frames; we short-circuit straight to the
                    // caller without consulting it.
                    if frame_no_alloc {
                        if self.frames.len() == caller_depth {
                            return Ok(result);
                        }
                        let caller = self.frames.last().unwrap();
                        let caller_fn_id = caller.fn_id;
                        let caller_ip = caller.ip as usize;
                        let caller_bp = caller.bp as usize;
                        self.stack.push(result);
                        fn_id = caller_fn_id;
                        ip = caller_ip;
                        bp = caller_bp;
                        refresh_code!();
                        continue;
                    }

                    let mut result = result;
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
                            refresh_code!();
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

                    // Copy the callable values plus args out of the stack, and
                    // LEAVE THE CELLS WHERE THEY ARE until the product is done.
                    //
                    // The copy is what the branches are dispatched from; the
                    // cells are what says the copies exist. A branch that runs
                    // on this VM — the sequential arm below — writes against
                    // this arena while its siblings' bundles are still in
                    // flight, and `slot_is_unheld` answers by walking exactly
                    // this vector. Truncating here would take those references
                    // out of the only place the decision can see them, and a
                    // sibling's map would read as held by nobody one statement
                    // before that sibling reads it. Keeping them costs
                    // `total_items` cells for the length of the product and
                    // nothing else: every branch frame is pushed above this
                    // point and truncates back to its own base on the way out.
                    let total_items: usize = descs.iter().map(|argc| argc + 1).sum();
                    let items_start = self.stack.len() - total_items;
                    let flat_items: Vec<NanValue> = self.stack[items_start..].to_vec();

                    // Save caller IP — drop code borrow before call_function.
                    // The branches themselves are entered with `caller_fn_id` /
                    // `caller_ip` below, so this park is for whatever walks
                    // `self.frames` while they run. A chunk entered through
                    // `CALL_LEAF` owns no frame — `CALL_PAR` is not in
                    // `classify_leaf_chunk`'s disqualifying set, so a body like
                    // `Result.Ok((f(a), g(b))?!)` is one — and parking there
                    // would overwrite the CALLER's position with this chunk's.
                    // Nothing reads that field before the caller's next call
                    // rewrites it, but leaving the caller's own position in
                    // place is what the rest of the loop does and costs a test.
                    if leaf_return.is_none() {
                        self.frames.last_mut().unwrap().ip = ip as u32;
                    }
                    let _saved_fn_id = fn_id;
                    let caller_fn_id = fn_id;
                    let caller_ip = ip;

                    // Enter replay group
                    self.runtime.replay_enter_group();

                    // Build per-element callable + arg bundles in source order,
                    // remembering where each one sits in the cells left standing
                    // above.
                    let mut element_calls: Vec<(NanValue, Vec<NanValue>)> =
                        Vec::with_capacity(count);
                    let mut bundle_spans: Vec<(usize, usize)> = Vec::with_capacity(count);
                    let mut item_offset = 0;
                    for argc in &descs {
                        let bundle_start = items_start + item_offset;
                        let callable = flat_items[item_offset];
                        item_offset += 1;
                        let args = flat_items[item_offset..item_offset + *argc].to_vec();
                        item_offset += *argc;
                        element_calls.push((callable, args));
                        bundle_spans.push((bundle_start, items_start + item_offset));
                    }

                    // Check if recording/replaying — if so, run sequentially
                    // (replay state is thread_local, can't share across threads).
                    // Also sequential when an oracle-stub map is installed
                    // (verify-time substitution): the stubs + counter state
                    // live on the parent VM's runtime, which can't be shared
                    // across child VMs spawned in the parallel path.
                    let is_tracking = self.runtime.is_effect_tracking();
                    let has_oracle_stubs = !self.runtime.oracle_stubs.is_empty();
                    let mut had_vm_error: Option<VmError> = None;
                    let results = if is_tracking || has_oracle_stubs || count <= 1 {
                        // Hostile order-axis: when the verify runner has
                        // flipped `reverse_independent_eval` on for this
                        // case, execute branches right-to-left but place
                        // each result back into its source-position slot.
                        // A pure law claims its branches are independent —
                        // that is, the resulting tuple is invariant under
                        // execution order. Forward vs reverse divergence
                        // means the claim doesn't hold for this stub map.
                        let reverse = self.runtime.reverse_independent_eval();
                        let mut results: Vec<NanValue> = vec![NanValue::UNIT; count];
                        let mut order: Vec<usize> = (0..count).collect();
                        if reverse {
                            order.reverse();
                        }
                        for i in order {
                            let (callable, args) = &element_calls[i];
                            // This branch's OWN bundle stops being a holder the
                            // moment the branch is entered: the callee takes the
                            // arguments as locals of its own frame, which is
                            // where every other call in the VM leaves them, and
                            // a cell here as well would make the branch's own
                            // argument look like somebody else's reference. Its
                            // SIBLINGS' bundles stay standing — they are the
                            // references that have not been handed anywhere yet.
                            let (bundle_start, bundle_end) = bundle_spans[i];
                            self.stack[bundle_start..bundle_end].fill(NanValue::UNIT);
                            self.runtime.replay_set_branch(i as u32);
                            let result = self.invoke_callable_value(
                                *callable,
                                args,
                                caller_fn_id,
                                caller_ip,
                            )?;
                            results[i] = result;
                            // A finished branch's answer is in flight for as
                            // long as the ones after it run. Nothing in the
                            // corpus can name it — a branch can only reach a
                            // sibling's result through a root that is already
                            // covered — so this buys no refusal today; it is
                            // what makes "every value this product is holding
                            // is an operand-stack cell" a property of the loop
                            // rather than of a case analysis, which is what the
                            // decision and its mirror both read.
                            self.stack.push(result);
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
                                        // The branch counted its own copies from
                                        // zero (`clone_static`); fold them in
                                        // before its arena is dropped.
                                        self.arena.absorb_copy_counters(&child_arena);
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
                                // The branch counted its own copies from zero
                                // (`clone_static`); fold them in before its
                                // arena is dropped.
                                self.arena.absorb_copy_counters(&child_arena);
                                results.push(imported);
                            }
                            results
                        }
                    };

                    // The product is joined: the bundles and the anchored
                    // results are references nobody holds any more, and `results`
                    // carries everything that survives. This is the truncation
                    // the pop above used to do, moved to where the branches have
                    // stopped needing to be visible.
                    self.stack.truncate(items_start);

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
                            leaf_error_return!(err_val);
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
                                    refresh_code!();
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
                        leaf_error_return!(result);
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
                                refresh_code!();
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
                        let value = self
                            .int_to_index(index)
                            .and_then(|i| self.arena.vector_ref_value(vec).get(i).copied());
                        match value {
                            Some(v) => {
                                self.stack
                                    .push(NanValue::new_some_value(v, &mut self.arena));
                            }
                            None => self.stack.push(NanValue::NONE),
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
                        let value = self
                            .int_to_index(index)
                            .and_then(|i| self.arena.vector_ref_value(vec).get(i).copied());
                        self.stack.push(value.unwrap_or(default));
                    }
                }

                VECTOR_SET => {
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let vec = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = self
                        .int_to_index(index)
                        .filter(|_| !vec.is_empty_vector_immediate());
                    if let Some(i) = idx {
                        let mut items = self.arena.clone_vector_value(vec);
                        if i < items.len() {
                            items[i] = value;
                            let new_idx = self.arena.push_vector(items);
                            let new_vec = NanValue::new_vector(new_idx);
                            self.stack
                                .push(NanValue::new_some_value(new_vec, &mut self.arena));
                        } else {
                            self.stack.push(NanValue::NONE);
                        }
                    } else {
                        self.stack.push(NanValue::NONE);
                    }
                }

                VECTOR_SET_OR_KEEP => {
                    let static_grant = read_u8!(code, ip) != 0;
                    let target_slot = read_u8!(code, ip) as usize;
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let index = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let vec = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = self
                        .int_to_index(index)
                        .filter(|_| !vec.is_empty_vector_immediate());
                    let Some(i) = idx else {
                        self.stack.push(vec);
                        continue;
                    };
                    // The static bit is a PROPOSAL, exactly as it is on the
                    // `CALL_BUILTIN_OWNED` spelling of the same write: the
                    // owned branch below is the VM's only true in-place arena
                    // write, and a container that still held this slot would
                    // read the mutation back with nothing failing loudly. Ask
                    // once the three operands are off the stack, so "no cell
                    // holds this" and "the operand cell was the only holder"
                    // are the same statement — the target's own local cell is
                    // the single exception, and it is named rather than
                    // guessed (`bp + target_slot`, the slot the opcode
                    // carries). A revoked grant costs exactly the copy the
                    // program made before the fusion existed.
                    let vec_owned = static_grant
                        && self.runtime_confirms_fused_vector_grant(vec, bp + target_slot);
                    if vec_owned && !vec.is_empty_vector_immediate() {
                        // Owned path: modify vector in-place at the same arena slot.
                        // No new allocation, no promotion needed.
                        //
                        // This is the VM's only true in-place arena write, and
                        // therefore the only way an arena slot the return
                        // boundary keeps can come to hold an index into a
                        // region the boundary drops: the vector may live below
                        // this frame's marks while `value` was allocated above
                        // them. Record that so the boundary does not take the
                        // return path that truncates young with no rewrite.
                        //
                        // The region test is the boundary's own predicate
                        // (`yard_mark`, matching `result_uses_frame_local_heap`)
                        // rather than the `yard_base` one `STORE_GLOBAL` uses.
                        // That is the conservative half of the pair — `yard_base
                        // <= yard_mark`, so `yard_mark` counts fewer slots as
                        // this frame's own and flags strictly more writes — and
                        // it is the line the guarded boundary actually draws.
                        //
                        // Two things the flag deliberately does NOT ask about.
                        // It is armed on the TARGET alone, never on where the
                        // value came from: the frame that wrote and the frame
                        // whose region holds the value need not be the same one
                        // (`an_inherited_in_place_write_survives_the_callers_boundary`
                        // is exactly that shape), so a value-side test would
                        // stay silent in the frame that must hear about it. And
                        // it is armed only for a write that actually happened —
                        // an index past the end stores nothing, and an
                        // immediate leaves no arena reference behind, so
                        // neither can leave a slot pointing anywhere.
                        let target_outside_frame = value.heap_index().is_some()
                            && self.frames.last().is_some_and(|frame| {
                                !vec.heap_index().is_some_and(|index| {
                                    self.arena.is_frame_local_index(
                                        index,
                                        frame.arena_mark,
                                        frame.yard_mark,
                                        frame.handoff_mark,
                                    )
                                })
                            });
                        // The one place in the VM where a value enters an arena
                        // entry without going through `Arena::push`, so it is
                        // the one place the choke point does not cover: after
                        // this store the vector holds `value`, and if that is a
                        // map, the vector is a holder of its slot.
                        self.arena.note_held_elsewhere(value);
                        let items = self.arena.get_vector_mut(vec.arena_index());
                        if i < items.len() {
                            items[i] = value;
                            if target_outside_frame && let Some(frame) = self.frames.last_mut() {
                                frame.inplace_write_escaped = true;
                            }
                        }
                        // Return the same NanValue — same slot, same space.
                        self.stack.push(vec);
                    } else {
                        let items = self.arena.vector_ref_value(vec);
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

                BUFFER_NEW => {
                    // cap_hint is currently advisory — a `String::with_capacity` hint.
                    // Reuse a freed slot if available to keep the pool from
                    // unbounded growth across many buffer cycles.
                    let cap_hint = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    // A negative hint means "no hint" (clamp to 0); a hint that
                    // cannot fit `usize` cannot be a real capacity → error.
                    let cap = if cap_hint.as_aver_int(&self.arena) < aver_rt::AverInt::zero() {
                        0
                    } else {
                        self.int_to_capacity(cap_hint, "Buffer.new")?
                    };
                    let idx = if let Some(slot) = self.buffer_pool.iter().position(Option::is_none)
                    {
                        self.buffer_pool[slot] = Some(String::with_capacity(cap));
                        slot
                    } else {
                        self.buffer_pool.push(Some(String::with_capacity(cap)));
                        self.buffer_pool.len() - 1
                    };
                    self.stack
                        .push(NanValue::new_int(idx as i64, &mut self.arena));
                }

                BUFFER_APPEND_STR => {
                    let s = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let buf = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = buf
                        .as_aver_int(&self.arena)
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    // Materialise the source bytes into an owned String first
                    // so the arena borrow is dropped before we re-borrow
                    // `self.buffer_pool`. The clone is a single small alloc
                    // per append; for large strings it's a single memcpy.
                    let owned: String = self.arena.get_string_value(s).to_string();
                    let slot = self
                        .buffer_pool
                        .get_mut(idx)
                        .and_then(Option::as_mut)
                        .ok_or_else(|| {
                            VmError::runtime("BUFFER_APPEND_STR: invalid buffer handle")
                        })?;
                    slot.push_str(&owned);
                    self.stack.push(buf);
                }

                BUFFER_APPEND_SEP_UNLESS_FIRST => {
                    let sep = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let buf = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = buf
                        .as_aver_int(&self.arena)
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let sep_bytes: String = self.arena.get_string_value(sep).to_string();
                    let slot = self
                        .buffer_pool
                        .get_mut(idx)
                        .and_then(Option::as_mut)
                        .ok_or_else(|| {
                            VmError::runtime(
                                "BUFFER_APPEND_SEP_UNLESS_FIRST: invalid buffer handle",
                            )
                        })?;
                    if !slot.is_empty() {
                        slot.push_str(&sep_bytes);
                    }
                    self.stack.push(buf);
                }

                BUFFER_FINALIZE => {
                    let buf = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let idx = buf
                        .as_aver_int(&self.arena)
                        .to_usize()
                        .unwrap_or(usize::MAX);
                    let s = self
                        .buffer_pool
                        .get_mut(idx)
                        .and_then(Option::take)
                        .ok_or_else(|| {
                            VmError::runtime("BUFFER_FINALIZE: invalid buffer handle")
                        })?;
                    let str_value = NanValue::new_string_value(&s, &mut self.arena);
                    self.stack.push(str_value);
                }

                // Const-divisor Euclidean div/mod (0.24 "Divide"). The HIR
                // resolver's literal-divisor discharge (and the MIR
                // const-fold pass) only emit these when the divisor is a
                // nonzero literal, so `div_euclid` / `rem_euclid` are always
                // defined here — no trap, computed on the `AverInt`
                // arbitrary-precision carrier as in `src/types/int.rs`.
                INT_DIV_EUCLID => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    // The emitters guarantee a non-zero divisor, so
                    // `div_euclid` is defined; over ℤ it also never overflows.
                    let r = a
                        .as_aver_int(&self.arena)
                        .div_euclid(&b.as_aver_int(&self.arena))
                        .ok_or_else(|| VmError::runtime("division by zero"))?;
                    self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                }
                INT_MOD_EUCLID => {
                    let b = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let a = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let r = a
                        .as_aver_int(&self.arena)
                        .rem_euclid(&b.as_aver_int(&self.arena))
                        .ok_or_else(|| VmError::runtime("modulo by zero"))?;
                    self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                }

                // Const-count bit-level view. The literal-count discharge
                // only emits these for a syntactic non-negative literal
                // count, so the `Negative` arm is unreachable; the
                // `Unrepresentable` arm is not reachable either, because a
                // literal that large is a `BigInt` literal, which the
                // discharge predicate declines.
                BITS_SHIFT_LEFT | BITS_SHIFT_RIGHT | BITS_LOW => {
                    let n = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let x = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let (x, n) = (x.as_aver_int(&self.arena), n.as_aver_int(&self.arena));
                    let computed = match op {
                        BITS_SHIFT_LEFT => x.shift_left(&n),
                        BITS_SHIFT_RIGHT => x.shift_right(&n),
                        _ => x.low_bits(&n),
                    };
                    let r = computed.map_err(|_| {
                        VmError::runtime(format!(
                            "{}: count {} is not a usable bit position",
                            opcode_name(op),
                            n
                        ))
                    })?;
                    self.stack.push(NanValue::from_aver_int(r, &mut self.arena));
                }

                // Codepoint cursor (chars fusion). Every arm delegates
                // to `aver_rt::strcursor` — the same routines compiled
                // Rust calls — so the fused loop cannot answer
                // differently on the two backends. An offset that is
                // not a character boundary cannot arise: the pass only
                // ever produces one by stepping from zero.
                //
                // The subject string is only ever BORROWED out of the
                // arena. Copying it would make each step cost the whole
                // string and the walk quadratic — measured at 122 s for
                // a 1 MiB input against 1.0 s for the list it replaces.
                // The `NanValue` producers need `&mut arena`, so each
                // arm takes what it needs out of the borrow (a bool, an
                // offset, at most four bytes of one character) and lets
                // the borrow end before it builds its result.
                STR_CURSOR_END => {
                    let i = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let s = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let offset = cursor_offset(i, &self.arena);
                    let at_end = aver_rt::str_cursor_end(&self.arena.get_string_value(s), offset);
                    self.stack.push(if at_end {
                        NanValue::TRUE
                    } else {
                        NanValue::FALSE
                    });
                }

                STR_CURSOR_NEXT => {
                    let i = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let s = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let offset = cursor_offset(i, &self.arena);
                    let next = aver_rt::str_cursor_next(&self.arena.get_string_value(s), offset);
                    let pushed = NanValue::new_int(next as i64, &mut self.arena);
                    self.stack.push(pushed);
                }

                STR_CURSOR_HEAD => {
                    let i = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let s = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let offset = cursor_offset(i, &self.arena);
                    // One character is at most four bytes, so the head
                    // leaves the borrow on the stack rather than the heap.
                    let mut buf = [0u8; 4];
                    let len = {
                        let text = self.arena.get_string_value(s);
                        let head = aver_rt::str_cursor_head(&text, offset);
                        buf[..head.len()].copy_from_slice(head.as_bytes());
                        head.len()
                    };
                    let head = std::str::from_utf8(&buf[..len])
                        .expect("one character sliced on its own boundaries is valid UTF-8");
                    let pushed = NanValue::new_string_value(head, &mut self.arena);
                    self.stack.push(pushed);
                }

                STR_CURSOR_CODE => {
                    let i = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let s = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let offset = cursor_offset(i, &self.arena);
                    // The codepoint is decoded straight off the borrow —
                    // no arena string exists for the character, which is
                    // the entire point of this opcode over
                    // `STR_CURSOR_HEAD` + `STR_CODE1`.
                    let code = {
                        let text = self.arena.get_string_value(s);
                        aver_rt::str_cursor_code(&text, offset)
                    };
                    let pushed = NanValue::new_int(code, &mut self.arena);
                    self.stack.push(pushed);
                }

                STR_FOLD_LOWER | STR_FOLD_UPPER => {
                    let c = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    // A code outside i64 is outside the scalar range,
                    // and the fold answers -1 for every non-scalar — the
                    // same wildcard the string route would take.
                    let code = c.as_aver_int(&self.arena).to_i64().unwrap_or(-1);
                    let folded = if op == STR_FOLD_LOWER {
                        aver_rt::str_fold_lower(code)
                    } else {
                        aver_rt::str_fold_upper(code)
                    };
                    let pushed = NanValue::new_int(folded, &mut self.arena);
                    self.stack.push(pushed);
                }

                STR_CODE1 | STR_CODE1_LOWER | STR_CODE1_UPPER => {
                    let s = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let code = {
                        let text = self.arena.get_string_value(s);
                        match op {
                            STR_CODE1 => aver_rt::str_code1(&text),
                            STR_CODE1_LOWER => aver_rt::str_code1_lower(&text),
                            _ => aver_rt::str_code1_upper(&text),
                        }
                    };
                    let pushed = NanValue::new_int(code, &mut self.arena);
                    self.stack.push(pushed);
                }

                // List builder (list-build fusion). A builder is either
                // a pool handle holding immediates or the cons chain the
                // loop wrote before this pass existed; see the opcode
                // block in `vm::opcode` for why both shapes are needed.
                LIST_BUILDER_NEW => {
                    // The capacity is advisory in exactly the sense
                    // `BUFFER_NEW`'s is: a `Vec::with_capacity` hint that
                    // cannot change the answer, so a negative or
                    // unrepresentable one means "no hint" rather than an
                    // error.
                    let hint = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let capacity = hint
                        .as_aver_int(&self.arena)
                        .to_usize()
                        .unwrap_or(0)
                        .min(LIST_BUILDER_CAPACITY_HINT_CAP);
                    let builder = match self.list_builder_free.pop() {
                        Some(slot) => {
                            self.list_builder_pool[slot] = Some(Vec::with_capacity(capacity));
                            NanValue::new_int(slot as i64, &mut self.arena)
                        }
                        None if self.list_builder_pool.len() < LIST_BUILDER_POOL_SLOTS => {
                            self.list_builder_pool
                                .push(Some(Vec::with_capacity(capacity)));
                            let slot = self.list_builder_pool.len() - 1;
                            NanValue::new_int(slot as i64, &mut self.arena)
                        }
                        // Out of slots. The cons chain needs none, and a
                        // builder that falls back to it is the program
                        // this pass replaced — slower than the pool, never
                        // wrong.
                        None => NanValue::EMPTY_LIST,
                    };
                    self.stack.push(builder);
                }

                LIST_BUILDER_PUSH => {
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let builder = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let pushed = self.list_builder_push(builder, value, code, ip)?;
                    self.stack.push(pushed);
                }

                LIST_BUILDER_FINALIZE => {
                    let builder = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let list = self.list_builder_finalize(builder)?;
                    self.stack.push(list);
                }

                // Byte builder (byte-sink retarget). A builder is a
                // pool handle, or the cons chain the source wrote when
                // the pool is out of slots; see the opcode block in
                // `vm::opcode` for the shape and the frame-boundary
                // reason it exists.
                BYTE_BUILDER_NEW => {
                    let hint = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let capacity = hint
                        .as_aver_int(&self.arena)
                        .to_usize()
                        .unwrap_or(0)
                        .min(LIST_BUILDER_CAPACITY_HINT_CAP);
                    let fresh = super::VmByteBuilder {
                        bytes: Vec::with_capacity(capacity),
                        bad: None,
                    };
                    let builder = match self.byte_builder_free.pop() {
                        Some(slot) => {
                            self.byte_builder_pool[slot] = Some(fresh);
                            NanValue::new_int(slot as i64, &mut self.arena)
                        }
                        None if self.byte_builder_pool.len() < BYTE_BUILDER_POOL_SLOTS => {
                            self.byte_builder_pool.push(Some(fresh));
                            let slot = self.byte_builder_pool.len() - 1;
                            NanValue::new_int(slot as i64, &mut self.arena)
                        }
                        // Out of slots. The cons chain needs none; the
                        // finalizer validates it natively — the unfused
                        // cost, the same answer.
                        None => NanValue::EMPTY_LIST,
                    };
                    self.stack.push(builder);
                }

                BYTE_BUILDER_PUSH => {
                    let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let builder = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let pushed = self.byte_builder_push(builder, value, code, ip)?;
                    self.stack.push(pushed);
                }

                BYTE_BUILDER_FINALIZE => {
                    let builder = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                    let result = self.byte_builder_finalize(builder, code, ip)?;
                    self.stack.push(result);
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

/// The byte offset a cursor opcode was handed. A negative or
/// unrepresentable value is past the end of every string, which is
/// exactly where the end test stops the loop — so saturating is a
/// termination guarantee, not a silent wrong index.
fn cursor_offset(value: NanValue, arena: &Arena) -> usize {
    value.as_aver_int(arena).to_usize().unwrap_or(usize::MAX)
}

/// How many pooled builders may exist at once.
///
/// The pool is a fast path, not a resource: builders nest only as deeply
/// as collecting loops call one another, so a few is already generous,
/// and the cap is what turns "a loop that exits without finalizing leaks
/// its slot" from unbounded into a number. Past it, builders fall back to
/// the cons chain, which is the program this pass replaced.
const LIST_BUILDER_POOL_SLOTS: usize = 4096;

/// How many pooled byte builders may exist at once — the list pool's
/// number, for the list pool's reason.
const BYTE_BUILDER_POOL_SLOTS: usize = LIST_BUILDER_POOL_SLOTS;

/// Ceiling on the capacity hint a builder will pre-allocate for.
///
/// The hint comes from the rewritten call site and is a guess about the
/// answer's length, never a promise; clamping it means a wrong guess
/// costs some growth rather than an allocation the program never needed.
const LIST_BUILDER_CAPACITY_HINT_CAP: usize = 1 << 16;

impl VM {
    /// Append `value` to `builder`, returning the builder that holds it.
    ///
    /// Pooled while the elements stay immediate. The first element with
    /// an arena index turns the builder into the cons chain the source
    /// wrote — everything collected so far, prepended in order, so the
    /// chain reads reversed exactly as [`Self::list_builder_finalize`]
    /// expects. That conversion happens at most once per builder and
    /// costs one pass over what it had.
    fn list_builder_push(
        &mut self,
        builder: NanValue,
        value: NanValue,
        code: &[u8],
        ip: usize,
    ) -> Result<NanValue, VmError> {
        if builder.is_list() {
            return Ok(self.list_builder_prepend(builder, value, code, ip));
        }
        let slot = self.list_builder_slot(builder)?;
        if value.heap_index().is_none() {
            self.list_builder_pool[slot]
                .as_mut()
                .expect("slot checked live")
                .push(value);
            return Ok(builder);
        }
        let collected = self.free_list_builder_slot(slot);
        let mut chain = NanValue::EMPTY_LIST;
        for item in collected {
            chain = self.list_builder_prepend(chain, item, code, ip);
        }
        Ok(self.list_builder_prepend(chain, value, code, ip))
    }

    /// The list a builder collected, in append order, and the slot back
    /// in the free list if it held one.
    fn list_builder_finalize(&mut self, builder: NanValue) -> Result<NanValue, VmError> {
        if builder.is_list() {
            // The chain was built by prepending in append order, so it
            // reads backwards — the same list, and the same single
            // reversal, the unfused loop ended with.
            let mut items = self.arena.list_to_vec_value(builder);
            items.reverse();
            let idx = self.arena.push_list(items);
            return Ok(NanValue::new_list(idx));
        }
        let slot = self.list_builder_slot(builder)?;
        let items = self.free_list_builder_slot(slot);
        let idx = self.arena.push_list(items);
        Ok(NanValue::new_list(idx))
    }

    /// One cons cell, allocated where the surrounding instruction says
    /// values belong — the same call `LIST_PREPEND` makes.
    fn list_builder_prepend(
        &mut self,
        list: NanValue,
        value: NanValue,
        code: &[u8],
        ip: usize,
    ) -> NanValue {
        let idx = self
            .arena
            .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                arena.push_list_prepend(value, list)
            });
        NanValue::new_list(idx)
    }

    /// The live pool slot a builder handle names.
    fn list_builder_slot(&self, builder: NanValue) -> Result<usize, VmError> {
        let slot = builder
            .as_aver_int(&self.arena)
            .to_usize()
            .filter(|slot| matches!(self.list_builder_pool.get(*slot), Some(Some(_))))
            .ok_or_else(|| VmError::runtime("list builder: invalid builder handle"))?;
        Ok(slot)
    }

    /// Take a slot's elements and hand the slot back for reuse.
    fn free_list_builder_slot(&mut self, slot: usize) -> Vec<NanValue> {
        let items = self.list_builder_pool[slot].take().unwrap_or_default();
        self.list_builder_free.push(slot);
        items
    }

    /// Record `value` on a byte builder, returning the builder that
    /// holds it. On the pool path the range check rides the push: an
    /// in-range element appends its byte, the first out-of-range one is
    /// remembered as a HOST value (never an arena reference), and later
    /// pushes change nothing because `Bytes.fromList` reports the FIRST
    /// offender. On the fallback path the raw element is prepended,
    /// exactly as the source loop wrote, and the finalizer does the
    /// deciding.
    fn byte_builder_push(
        &mut self,
        builder: NanValue,
        value: NanValue,
        code: &[u8],
        ip: usize,
    ) -> Result<NanValue, VmError> {
        if builder.is_list() {
            return Ok(self.list_builder_prepend(builder, value, code, ip));
        }
        let slot = self.byte_builder_slot(builder)?;
        let state = self.byte_builder_pool[slot]
            .as_mut()
            .expect("slot checked live");
        if state.bad.is_none() {
            let elem = value.as_aver_int(&self.arena);
            match elem.to_i64() {
                Some(byte) if (0..=255).contains(&byte) => state.bytes.push(byte as u8),
                // Too big for i64 is certainly too big for a byte, so
                // the unrepresentable case is the same case.
                _ => state.bad = Some((elem, state.bytes.len())),
            }
        }
        Ok(builder)
    }

    /// What `Bytes.fromList` would answer for the pushed elements —
    /// `Result.Ok(<list>)` in push order, or `Result.Err(<message>)`
    /// naming the first element outside `0..=255` and its index. The
    /// message is the standard library's own spelling, with the value
    /// rendered the way Aver renders any `Int`.
    fn byte_builder_finalize(
        &mut self,
        builder: NanValue,
        code: &[u8],
        ip: usize,
    ) -> Result<NanValue, VmError> {
        let (bytes, bad) = if builder.is_list() {
            // The chain was built by prepending in push order, so it
            // reads backwards; straightened out, it is the list the
            // unfused loop handed to `fromList`, and this walk is that
            // validation done natively.
            let mut items = self.arena.list_to_vec_value(builder);
            items.reverse();
            let mut bytes = Vec::with_capacity(items.len());
            let mut bad = None;
            for (index, item) in items.iter().enumerate() {
                let elem = item.as_aver_int(&self.arena);
                match elem.to_i64() {
                    Some(byte) if (0..=255).contains(&byte) => bytes.push(byte as u8),
                    _ => {
                        bad = Some((elem, index));
                        break;
                    }
                }
            }
            (bytes, bad)
        } else {
            let slot = self.byte_builder_slot(builder)?;
            let state = self.byte_builder_pool[slot]
                .take()
                .expect("slot checked live");
            self.byte_builder_free.push(slot);
            (state.bytes, state.bad)
        };
        match bad {
            Some((value, index)) => {
                let message = format!("byte {value} at index {index} is outside 0..=255");
                let text = NanValue::new_string_value(&message, &mut self.arena);
                let wrapped = self
                    .arena
                    .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                        NanValue::new_err_value(text, arena)
                    });
                Ok(wrapped)
            }
            None => {
                let items: Vec<NanValue> = bytes
                    .into_iter()
                    .map(|b| NanValue::new_int(b as i64, &mut self.arena))
                    .collect();
                let idx = self.arena.push_list(items);
                let list = NanValue::new_list(idx);
                let wrapped = self
                    .arena
                    .with_alloc_space(self.next_value_alloc_space(code, ip), |arena| {
                        NanValue::new_ok_value(list, arena)
                    });
                Ok(wrapped)
            }
        }
    }

    /// The live pool slot a byte builder handle names.
    fn byte_builder_slot(&self, builder: NanValue) -> Result<usize, VmError> {
        let slot = builder
            .as_aver_int(&self.arena)
            .to_usize()
            .filter(|slot| matches!(self.byte_builder_pool.get(*slot), Some(Some(_))))
            .ok_or_else(|| VmError::runtime("byte builder: invalid builder handle"))?;
        Ok(slot)
    }
}
