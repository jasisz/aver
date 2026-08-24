// Aver VM bytecode opcodes.
//
// Stack-based: operands are pushed/popped from the operand stack.
// Variable-width encoding: opcode (1 byte) + operands (0-3 bytes).

// -- Stack / locals ----------------------------------------------------------

/// No-op, used as padding after superinstruction fusion.
pub const NOP: u8 = 0x00;

/// Push `stack[bp + slot]` onto the operand stack.
pub const LOAD_LOCAL: u8 = 0x01; // slot:u8

/// Push `stack[bp + slot]` and clear the slot (move semantics).
/// Used for last-use variables: caller releases sole ownership so
/// callees and builtins see refcount=1 and can mutate in-place.
pub const MOVE_LOCAL: u8 = 0x48; // slot:u8

/// Pop top and store into `stack[bp + slot]`.
pub const STORE_LOCAL: u8 = 0x02; // slot:u8

/// Push `constants[idx]` onto the operand stack.
pub const LOAD_CONST: u8 = 0x03; // idx:u16

/// Push `globals[idx]` onto the operand stack.
pub const LOAD_GLOBAL: u8 = 0x04; // idx:u16

/// Pop top and store into `globals[idx]`.
pub const STORE_GLOBAL: u8 = 0x0A; // idx:u16

/// Discard the top value.
pub const POP: u8 = 0x05;

/// Duplicate the top value.
pub const DUP: u8 = 0x06;

/// Push `NanValue::UNIT`.
pub const LOAD_UNIT: u8 = 0x07;

/// Push `NanValue::TRUE`.
pub const LOAD_TRUE: u8 = 0x08;

/// Push `NanValue::FALSE`.
pub const LOAD_FALSE: u8 = 0x09;

// -- Arithmetic --------------------------------------------------------------

/// Pop b, pop a, push a + b.
pub const ADD: u8 = 0x10;

/// Pop b, pop a, push a - b.
pub const SUB: u8 = 0x11;

/// Pop b, pop a, push a * b.
pub const MUL: u8 = 0x12;

/// Pop b, pop a, push a / b.
pub const DIV: u8 = 0x13;

/// Pop b, pop a, push a % b.
pub const MOD: u8 = 0x14;

/// Pop a, push -a.
pub const NEG: u8 = 0x15;

/// Pop a, push !a (boolean not).
pub const NOT: u8 = 0x16;

/// Typed `+` for two `Int` operands. Materializes both as `AverInt` and
/// adds over ℤ (non-wrapping; i64 fast path inside `AverInt::add`, promoting
/// to a bignum only on overflow), boxing back through `from_aver_int`. Skips
/// the tag-dispatch chain in `arith_add`. Emitted when both operand types
/// resolve to `Type::Int`.
pub const ADD_INT: u8 = 0x17;
/// Typed `-` for two `Int` operands; same shape as `ADD_INT`.
pub const SUB_INT: u8 = 0x18;
/// Typed `*` for two `Int` operands; same shape as `ADD_INT`.
pub const MUL_INT: u8 = 0x19;

/// Typed `+` for two `Float` operands. `as_float` decode (raw
/// `f64::from_bits`), IEEE 754 add, push as `NanValue::new_float`.
/// Skips `arith_add` tag-dispatch + cross-type promotion. Hot in
/// numeric loops (Mandelbrot inner step is 3 muls + 1 add per iter).
pub const ADD_FLOAT: u8 = 0x1A;
/// Typed `-` for two `Float` operands; same shape as `ADD_FLOAT`.
pub const SUB_FLOAT: u8 = 0x1B;
/// Typed `*` for two `Float` operands; same shape as `ADD_FLOAT`.
pub const MUL_FLOAT: u8 = 0x1C;
/// Typed `/` for two `Float` operands; same shape as `ADD_FLOAT`.
/// IEEE 754 division — `b == 0.0` produces `inf`/`-inf`/`NaN` per
/// the spec, no runtime check.
pub const DIV_FLOAT: u8 = 0x1D;

/// Typed unary `-` on an `Int` operand. Pops one value and negates over ℤ
/// via `AverInt::neg` (non-wrapping; `-i64::MIN` promotes to a bignum), then
/// re-boxes through `from_aver_int`. Skips the `is_int / is_float` branch in
/// `NEG`. Emitted when `inner.ty()` resolves to `Type::Int`.
pub const NEG_INT: u8 = 0x1E;
/// Typed unary `-` on a `Float` operand. Same shape as `NEG_INT`
/// but on the float side: raw `f64::from_bits`, IEEE 754 negation
/// (sign-bit flip), push as `NanValue::new_float`. Skips the
/// `NEG` tag-dispatch chain and preserves `-0.0` because the
/// negation runs on the float bit pattern, not on `0 - x` desugar.
pub const NEG_FLOAT: u8 = 0x1F;

// -- Comparison --------------------------------------------------------------

/// Pop b, pop a, push a == b.
pub const EQ: u8 = 0x20;

/// Pop b, pop a, push a < b.
pub const LT: u8 = 0x21;

/// Pop b, pop a, push a > b.
pub const GT: u8 = 0x22;

/// Typed `==` for two `Int` operands. Skips `NanValue::eq_in`'s tag
/// dispatch — `as_int_unboxed()` on both sides, raw i64 compare, push
/// bool. Emitted by the VM compiler when both `left.ty()` and
/// `right.ty()` resolve to `Type::Int`. Hot path in pattern-match-on-
/// Int (`match n { 0 -> …; _ -> … }`) and arithmetic guards; profile
/// shows `eq_in` at 10–12% self-time in newtype/match scenarios.
pub const EQ_INT: u8 = 0x23;

/// Typed `<` for two `Int` operands. `as_int` decode on both sides
/// (folds the inline path inline), raw i64 compare, push bool. Skips
/// `compare_lt`'s tag-dispatch (`is_int`/`is_float`/`is_string` chain
/// plus cross-type promotion). Emitted when both operands'
/// `Spanned::ty()` resolve to `Type::Int`.
pub const LT_INT: u8 = 0x24;

/// Typed `>` for two `Int` operands; same shape as `LT_INT`.
pub const GT_INT: u8 = 0x25;

/// Typed `<` for two `Float` operands. `as_float` (raw `f64::from_bits`)
/// on both sides, IEEE 754 compare, push bool. Hot in numeric loops
/// (Mandelbrot inner step `match curZ2 > 4.0 { … }` etc.); profile
/// shows `compare_lt` at 2.6% self-time on fractal_seahorse.
pub const LT_FLOAT: u8 = 0x26;

/// Typed `>` for two `Float` operands; same shape as `LT_FLOAT`.
pub const GT_FLOAT: u8 = 0x27;

/// Fused `match n { LIT -> ...; _ -> ... }` arm test: peek the top of
/// stack (subject left in place by the surrounding `compile_match`),
/// if its `Int` value equals the inline `imm` literal — fall through
/// to the arm body; otherwise skip via `fail_offset`. Replaces the
/// `DUP + LOAD_CONST + EQ + JUMP_IF_FALSE` sequence the generic
/// `compile_pattern` emits — one dispatch instead of four in the hot
/// path of every `match n { 0 -> ... }` shape.
///
/// Encoding: `MATCH_INT_LITERAL imm:i64 fail_offset:i16`.
pub const MATCH_INT_LITERAL: u8 = 0x7F;

// -- String ------------------------------------------------------------------

/// Pop b, pop a, push str(a) ++ str(b).
pub const CONCAT: u8 = 0x28;

// -- Control flow ------------------------------------------------------------

/// Unconditional relative jump: ip += offset.
pub const JUMP: u8 = 0x30; // offset:i16

/// Pop top, if falsy: ip += offset.
pub const JUMP_IF_FALSE: u8 = 0x31; // offset:i16

// -- Calls -------------------------------------------------------------------

/// Call a known function by id. Args already on stack.
pub const CALL_KNOWN: u8 = 0x40; // fn_id:u16, argc:u8

/// Call a function value on the stack (under args).
pub const CALL_VALUE: u8 = 0x41; // argc:u8

/// Call a builtin service function.
pub const CALL_BUILTIN: u8 = 0x42; // symbol_id:u32, argc:u8

/// Like CALL_BUILTIN but with owned-argument bitmask from reuse analysis.
/// Builtins receiving owned args can mutate in-place instead of cloning.
pub const CALL_BUILTIN_OWNED: u8 = 0x46; // symbol_id:u32, argc:u8, owned:u8

/// Like CALL_KNOWN but with owned-argument bitmask from reuse analysis.
pub const CALL_KNOWN_OWNED: u8 = 0x47; // fn_id:u16, argc:u8, owned:u8

/// Self tail-call: reuse current frame with new args.
pub const TAIL_CALL_SELF: u8 = 0x43; // argc:u8, owned:u8

/// Mutual tail-call to a known function: reuse frame, switch target.
pub const TAIL_CALL_KNOWN: u8 = 0x44; // fn_id:u16, argc:u8, owned:u8

/// Return top of stack to caller.
pub const RETURN: u8 = 0x50;

// -- Structured values -------------------------------------------------------

/// Push Nil (empty cons list).
pub const LIST_NIL: u8 = 0x60;

/// Pop tail, pop head, push Cons(head, tail).
pub const LIST_CONS: u8 = 0x61;

/// Pop `count` items, build cons list from them (first item = head), push list.
pub const LIST_NEW: u8 = 0x62; // count:u32

/// Pop `count` field values, push a new record with `type_id`.
pub const RECORD_NEW: u8 = 0x63; // type_id:u16, count:u8

/// Pop record, push `fields[field_idx]` (compile-time resolved index).
pub const RECORD_GET: u8 = 0x64; // field_idx:u8

/// Pop record, lookup field by interned field symbol, push value.
pub const RECORD_GET_NAMED: u8 = 0x67; // field_symbol_id:u32

/// Pop `count` field values, push a new variant.
pub const VARIANT_NEW: u8 = 0x65; // type_id:u16, variant_id:u16, count:u8

/// Pop value, push wrapped value. kind: 0=Ok, 1=Err, 2=Some.
pub const WRAP: u8 = 0x66; // kind:u8

/// Pop `count` items, build a tuple from them, push tuple.
pub const TUPLE_NEW: u8 = 0x68; // count:u32

/// Parallel function calls for independent products (?! / !).
/// Pops N callable values plus their args from the stack, dispatches them via
/// the same callable resolution rules as CALL_VALUE, then builds the result tuple.
/// Enters/exits replay group around parallel dispatch.
///
/// Encoding: CALL_PAR count:u8 unwrap:u8
/// `[argc:u8 proven_result:u8 × count]`
/// unwrap=1 (?!): unwrap each Result, propagate first Err.
/// unwrap=0 (!): return raw tuple.
/// proven_result=1: invoke the nested effect callable inside the group, then
/// apply the fail-closed `RESULT_PROVEN` destructor before constructing the
/// raw tuple.
pub const CALL_PAR: u8 = 0x86;

/// Update selected fields on a record, preserving the rest from the base value.
/// Stack: [..., base_record, update_0, ..., update_n-1] -> [..., updated_record]
pub const RECORD_UPDATE: u8 = 0x69; // type_id:u16, count:u8, field_idx[count]:u8

/// Propagate `Result.Err` to caller or unwrap `Result.Ok` in place.
pub const PROPAGATE_ERR: u8 = 0x6A;

/// Pop list, push its length as Int.
pub const LIST_LEN: u8 = 0x6B;

// 0x6C and 0x6D were LIST_GET and LIST_APPEND — removed.

/// Pop list, pop value, push prepended list.
pub const LIST_PREPEND: u8 = 0x6E;

// 0x6F was LIST_GET_MATCH — removed.

// -- Pattern matching --------------------------------------------------------

/// Peek top: if NaN tag != expected, ip += fail_offset.
pub const MATCH_TAG: u8 = 0x70; // expected_tag:u8, fail_offset:i16

/// Peek top (must be variant): if variant_id != expected, ip += fail_offset.
pub const MATCH_VARIANT: u8 = 0x71; // ctor_id:u16, fail_offset:i16

/// Peek top: if not wrapper of `kind`, ip += fail_offset.
/// If matches, replace top with inner value (unwrap in-place).
/// kind: 0=Ok, 1=Err, 2=Some.
pub const MATCH_UNWRAP: u8 = 0x72; // kind:u8, fail_offset:i16

/// Peek top: if not Nil, ip += fail_offset.
pub const MATCH_NIL: u8 = 0x73; // fail_offset:i16

/// Peek top: if Nil (not a cons), ip += fail_offset.
pub const MATCH_CONS: u8 = 0x74; // fail_offset:i16

/// Pop cons cell, push tail then push head.
pub const LIST_HEAD_TAIL: u8 = 0x75;

/// Peek top (record/variant), push `fields[field_idx]` (non-destructive).
pub const EXTRACT_FIELD: u8 = 0x76; // field_idx:u8

/// Peek top: if not a tuple of `count` items, ip += fail_offset.
pub const MATCH_TUPLE: u8 = 0x78; // count:u8, fail_offset:i16

/// Peek top tuple, push `items[item_idx]` (non-destructive).
pub const EXTRACT_TUPLE_ITEM: u8 = 0x79; // item_idx:u8

/// Non-exhaustive match error at source line.
pub const MATCH_FAIL: u8 = 0x77; // line:u16

/// Unified prefix/exact dispatch on NanValue bits.
///
/// Encoding:
///   MATCH_DISPATCH count:u8 default_offset:i16
///     [(kind:u8, expected:u64, offset:i16) × count]
///
/// kind=0: exact match  — `val.bits() == expected`
/// kind=1: tag match    — `(val.bits() & TAG_MASK_FULL) == expected`
///   where TAG_MASK_FULL = 0xFFFF_C000_0000_0000 (QNAN 14 bits + tag 4 bits)
///
/// Pops subject. Scans entries in order; first match wins → ip += offset.
/// No match → ip += default_offset.
/// All offsets are relative to the end of the full instruction.
pub const MATCH_DISPATCH: u8 = 0x7A;

/// Like MATCH_DISPATCH but every entry carries an inline result instead
/// of a jump offset.  When an entry matches, the result is pushed directly
/// onto the stack and the match body is skipped entirely.
///
/// Encoding:
///   MATCH_DISPATCH_CONST count:u8 default_offset:i16
///     [(kind:u8, expected:u64, result:u64) × count]
///
/// Hit → pop subject, push result NanValue.
/// Miss → pop subject, ip += default_offset (execute default arm body).
///
/// Emitted when ALL dispatchable arms have constant bodies (literals).
pub const MATCH_DISPATCH_CONST: u8 = 0x7B;

/// Tail-call self for thin frames: no arena finalization needed.
/// The compiler emits this instead of TAIL_CALL_SELF when the function
/// is known to be "thin" (no heap allocations within the frame).
/// Skips finalize_frame_locals_for_tail_call entirely — just copies
/// args in-place and resets ip.
pub const TAIL_CALL_SELF_THIN: u8 = 0x45; // argc:u8

/// Inline Option.withDefault: pop default, pop option → push inner or default.
/// Stack: [option, default] → [result]
/// If option is Some → push unwrapped inner value.
/// If option is None → push default.
pub const UNWRAP_OR: u8 = 0x7C;

/// Inline Result.withDefault: pop default, pop result → push inner or default.
/// Stack: [result, default] → [value]
/// If result is Ok → push unwrapped inner value.
/// If result is Err → push default.
pub const UNWRAP_RESULT_OR: u8 = 0x7D;

/// Frameless call to a leaf+thin+args-only function.
/// No CallFrame is pushed — just saves (fn_id, ip) in the dispatch loop,
/// sets bp to the args already on stack, and jumps to the target.
/// On RETURN, restores the caller's state directly.
/// Format: fn_id:u16, argc:u8 (same as CALL_KNOWN).
pub const CALL_LEAF: u8 = 0x7E;

// ─── Superinstructions ──────────────────────────────────────

/// Push two locals in one dispatch. Format: slot_a:u8, slot_b:u8.
pub const LOAD_LOCAL_2: u8 = 0x80;

/// Push one local + one constant in one dispatch. Format: slot:u8, const_idx:u16.
pub const LOAD_LOCAL_CONST: u8 = 0x81;

/// Inline Vector.get: pop index, pop vector → push Option (Some/None).
/// Stack: [vector, index] → [option]
pub const VECTOR_GET: u8 = 0x82;

/// Fused Vector.get + Option.withDefault: pop default, pop index, pop vector → push value.
/// Stack: [vector, index, default] → [value]
/// Combines CALL_BUILTIN(Vector.get) + LOAD_CONST + UNWRAP_OR into one opcode.
pub const VECTOR_GET_OR: u8 = 0x83;

/// Inline Vector.set: pop value, pop index, pop vector → push Option<Vector>.
/// Stack: [vector, index, value] → [option_vector]
pub const VECTOR_SET: u8 = 0x84;

/// Fused Vector.set + Option.withDefault(vec): pop value, pop index, pop vector → push vector.
/// Stack: [vector, index, value] → [vector]
pub const VECTOR_SET_OR_KEEP: u8 = 0x85;

// -- Deforestation buffer (0.15 Traversal) -----------------------------------
//
// Mutable byte-buffer scratch backing the synthesizer's `__buf_*` intrinsics.
// Buffer values travel as opaque `Int(handle)` NanValues — handles are
// indices into `vm.buffer_pool: Vec<Option<String>>`. This keeps Buffer
// out of `ArenaEntry` (no exhaustiveness ripples) and out of GC marking
// (handle is an inline value, the underlying String lives on the host
// heap unaffected by frame compactions). Buffers are use-once: created
// by `BUFFER_NEW`, mutated through `BUFFER_APPEND_*`, finalized to an
// arena `String` (Rc<str>) by `BUFFER_FINALIZE` which also frees the slot.

/// Allocate a fresh String buffer. Pop cap_hint:i64 → push handle:Int(buffer_idx).
pub const BUFFER_NEW: u8 = 0x90;

/// Append the bytes of a string to a buffer. Pop str, pop buf →
/// push buf (same handle). The String pointed to by `buf.handle` is
/// mutated in place via `String::push_str`.
pub const BUFFER_APPEND_STR: u8 = 0x91;

/// Append separator only when buffer is non-empty. Pop sep, pop buf →
/// push buf. No-op for the first append, so the synthesized `__buffered`
/// loop body can place the separator before each element uniformly.
pub const BUFFER_APPEND_SEP_UNLESS_FIRST: u8 = 0x92;

/// Drain a buffer into an arena `OBJ_STRING`. Pop buf → push string.
/// Frees the underlying `vm.buffer_pool` slot; the handle becomes invalid.
pub const BUFFER_FINALIZE: u8 = 0x93;

// -- Const-divisor Euclidean div/mod (0.24 "Divide") -------------------------
//
// Emitted for `Int.div(a, k)` / `Int.mod(a, k)` with a nonzero literal
// divisor `k` — by the HIR resolver's literal-divisor discharge and, as a
// belt-and-suspenders rewrite, by the MIR `const_fold` pass. Both pop two
// `Int`s and push the unchecked Euclidean result via the `AverInt`
// arbitrary-precision routines `Int.div` / `Int.mod` use in
// `src/types/int.rs` — total for every `b != 0` over ℤ (`i64::MIN / -1`
// promotes to the bignum quotient `2^63`, not an overflow).

/// Pop b, pop a, push `a.div_euclid(b)` (Int). Caller guarantees `b != 0`
/// (the literal-divisor check), so no trap is possible.
pub const INT_DIV_EUCLID: u8 = 0x94;

/// Pop b, pop a, push `a.rem_euclid(b)` (Int). Caller guarantees `b != 0`.
pub const INT_MOD_EUCLID: u8 = 0x95;

// --- Const-count bit-level view (`Bits`) -------------------------------
//
// The three `Bits` operations that take a shift amount or a bit width are
// partial ONLY below zero. The HIR resolver's literal-count discharge emits
// these opcodes exactly where the count is a syntactic non-negative integer
// literal, so no `Result` is built and no error is reachable. Computed on
// the same `AverInt` routines the `Bits.*` builtins use
// (`src/types/bits.rs`), so the discharged and undischarged forms cannot
// drift apart.

/// Pop n, pop x, push `x * 2^n` (Int). Caller guarantees `n >= 0`.
pub const BITS_SHIFT_LEFT: u8 = 0x96;

/// Pop n, pop x, push `floor(x / 2^n)` (Int) — ARITHMETIC shift, so the
/// sign tail shifts in. Caller guarantees `n >= 0`.
pub const BITS_SHIFT_RIGHT: u8 = 0x97;

/// Pop w, pop x, push `x mod 2^w` (Int), always in `[0, 2^w)`. Caller
/// guarantees `w >= 0`.
pub const BITS_LOW: u8 = 0x98;

// --- Codepoint cursor (chars fusion) -----------------------------------
//
// Emitted by the chars-fusion pass in place of a `String.chars(s)` list
// and the string comparisons a character match compiles to. The cursor
// is a byte offset that only ever lands on a character boundary,
// because `STR_CURSOR_NEXT` steps by the UTF-8 width of the character
// it reads — `String.chars` iterates Unicode scalar values, so a
// byte-at-a-time cursor would visit a different list. Every one of
// these delegates to `aver_rt::strcursor`, the same code compiled Rust
// calls, so the two backends cannot answer differently.

/// Pop i, pop s → push `Bool` (the cursor has reached the end of `s`).
pub const STR_CURSOR_END: u8 = 0x99;

/// Pop i, pop s → push the one-character `String` at byte offset `i`.
pub const STR_CURSOR_HEAD: u8 = 0x9A;

/// Pop i, pop s → push the byte offset one CODEPOINT past `i` (Int).
pub const STR_CURSOR_NEXT: u8 = 0x9B;

/// Pop s → push its single codepoint (Int), or `-1` when `s` is not
/// exactly one character long.
pub const STR_CODE1: u8 = 0x9C;

/// Pop s → push [`STR_CODE1`] of `String.toLower(s)`.
pub const STR_CODE1_LOWER: u8 = 0x9D;

/// Pop s → push [`STR_CODE1`] of `String.toUpper(s)`.
pub const STR_CODE1_UPPER: u8 = 0x9E;

/// Pop i, pop s → push the codepoint (Int) of exactly the character
/// [`STR_CURSOR_HEAD`] would read at byte offset `i`, `-1` past the
/// end. One decode instead of an arena string plus [`STR_CODE1`].
pub const STR_CURSOR_CODE: u8 = 0xA2;

/// Pop c → push [`STR_CODE1_LOWER`] of the one-character string of
/// codepoint `c`, without building it: full Unicode lowercase of the
/// character, `-1` when `c` is not a scalar or the result is not
/// exactly one character.
pub const STR_FOLD_LOWER: u8 = 0xA3;

/// Pop c → push [`STR_CODE1_UPPER`] of the one-character string of
/// codepoint `c`; mirror of [`STR_FOLD_LOWER`].
pub const STR_FOLD_UPPER: u8 = 0xA4;

// --- List builder (list-build fusion) ----------------------------------
//
// Emitted by the list-build pass in place of the `List.prepend` cons
// chain a collecting loop threads and the single `List.reverse` that
// straightens it out. Appending in traversal order reaches the same
// list without either.
//
// A builder travels in ONE of two shapes, and every opcode below reads
// which from the value it is handed.
//
// A handle — `Int(slot)` into `vm.list_builder_pool` — while every
// element pushed so far is an IMMEDIATE (a small integer, a boolean,
// the unit). Those carry no arena index, so a pool the collector cannot
// see holds nothing the collector could move, and the walk costs one
// vector push per element with no arena traffic at all.
//
// A LIST, exactly the reversed cons chain the source wrote, from the
// first element that is a heap value onward. A growing arena object
// cannot be the accumulator of a tail-recursive loop for free: the
// frame boundary rewrites everything a root reaches, so an N-element
// vector threaded through N iterations is rewritten N times. The cons
// chain is what the loop already paid for and what the boundary already
// handles in constant time per step, so that is what a heap-valued
// builder falls back to — same allocations as before this pass, same
// answer, and the pool half keeps the win where the elements are
// numbers.

/// Pop capacity hint → push a fresh builder: `Int(slot)` when the pool
/// has room, otherwise the empty list (the fallback shape, which needs
/// no slot).
pub const LIST_BUILDER_NEW: u8 = 0x9F;

/// Pop elem, pop builder → push the builder with `elem` appended.
pub const LIST_BUILDER_PUSH: u8 = 0xA0;

/// Pop builder → push the `List` it collected, in append order. Frees
/// the pool slot when the builder held one.
pub const LIST_BUILDER_FINALIZE: u8 = 0xA1;

// --- Byte builder (list-build fusion, byte-sink retarget) --------------
//
// Emitted where a collected loop's only reader was the standard
// library's `Bytes.fromList`: the elements are octets, the validation
// rides every push, and the finalizer answers what `fromList` would
// have — `Ok` of the collected list or `Err` naming the first element
// outside `0..=255`, in push order, which is the walk order `fromList`
// validates in.
//
// The pool-and-handle shape is the list builder's, for the list
// builder's reason: a growing arena object threaded through a
// tail-recursive loop is a live root the frame boundary rewrites every
// iteration — O(n²) for an O(n) loop — so the bytes live host-side and
// the loop threads an immediate `Int(slot)` the collector never sees.
// One thing is SIMPLER here: every payload is a `u8` and the recorded
// offender is a host value, so the pool never holds anything the
// collector could move and there is no cons-chain transition on a heap
// element. The only fallback is pool exhaustion, where the builder
// travels as the reversed cons chain the source wrote and the
// finalizer validates it natively — the unfused cost, the same answer.

/// Pop capacity hint → push a fresh byte builder: `Int(slot)` when the
/// pool has room, otherwise the empty list (the fallback shape).
pub const BYTE_BUILDER_NEW: u8 = 0xA5;

/// Pop elem, pop builder → push the builder with `elem` recorded: the
/// byte appended when `elem` is in `0..=255`, the first out-of-range
/// element remembered otherwise.
pub const BYTE_BUILDER_PUSH: u8 = 0xA6;

/// Pop builder → push what `Bytes.fromList` would answer for the
/// pushed elements: `Result.Ok(<list>)` or `Result.Err(<message>)`.
/// Frees the pool slot when the builder held one.
pub const BYTE_BUILDER_FINALIZE: u8 = 0xA7;

// --- Loop-scoped indexed string access --------------------------------

/// Pop s → push its hidden codepoint-to-byte index.
pub const STR_INDEX_BUILD: u8 = 0xA8;

/// Pop i, pop index, pop s → push `String.charAt(s, i)`.
pub const STR_INDEX_CHAR_AT: u8 = 0xA9;

/// Pop to, pop from, pop index, pop s → push `String.slice(s, from, to)`.
pub const STR_INDEX_SLICE: u8 = 0xAA;

/// Pop fill, pop size, push a fresh Vector. Emitted only for a syntactic
/// non-negative size literal in the portable `u32` range.
pub const VECTOR_NEW_LITERAL: u8 = 0xAB;

/// Pop index, pop BranchPath → push the extended BranchPath directly.
/// Emitted only after the literal non-negative-index discharge.
pub const BRANCH_PATH_CHILD_LITERAL: u8 = 0xAC;

/// Pop String → push BranchPath directly. Emitted only after the literal
/// dewey-decimal validator has discharged `BranchPath.parse`.
pub const BRANCH_PATH_PARSE_LITERAL: u8 = 0xAD;

/// Pop a statically discharged `Result`, push its `Ok` payload, or fault when
/// the supposedly unreachable `Err` branch is reached. This is intentionally
/// not `UNWRAP_RESULT_OR`: a provider contract violation has no fallback.
pub const RESULT_PROVEN: u8 = 0xAE;

/// Opcode name for debug/disassembly.
pub fn opcode_name(op: u8) -> &'static str {
    match op {
        LOAD_LOCAL => "LOAD_LOCAL",
        MOVE_LOCAL => "MOVE_LOCAL",
        STORE_LOCAL => "STORE_LOCAL",
        LOAD_CONST => "LOAD_CONST",
        LOAD_GLOBAL => "LOAD_GLOBAL",
        POP => "POP",
        DUP => "DUP",
        LOAD_UNIT => "LOAD_UNIT",
        LOAD_TRUE => "LOAD_TRUE",
        LOAD_FALSE => "LOAD_FALSE",
        ADD => "ADD",
        ADD_INT => "ADD_INT",
        SUB_INT => "SUB_INT",
        MUL_INT => "MUL_INT",
        ADD_FLOAT => "ADD_FLOAT",
        SUB_FLOAT => "SUB_FLOAT",
        MUL_FLOAT => "MUL_FLOAT",
        DIV_FLOAT => "DIV_FLOAT",
        SUB => "SUB",
        MUL => "MUL",
        DIV => "DIV",
        MOD => "MOD",
        NEG => "NEG",
        NEG_INT => "NEG_INT",
        NEG_FLOAT => "NEG_FLOAT",
        NOT => "NOT",
        EQ => "EQ",
        EQ_INT => "EQ_INT",
        LT_INT => "LT_INT",
        GT_INT => "GT_INT",
        LT_FLOAT => "LT_FLOAT",
        GT_FLOAT => "GT_FLOAT",
        LT => "LT",
        GT => "GT",
        CONCAT => "CONCAT",
        JUMP => "JUMP",
        JUMP_IF_FALSE => "JUMP_IF_FALSE",
        CALL_KNOWN => "CALL_KNOWN",
        CALL_VALUE => "CALL_VALUE",
        CALL_BUILTIN => "CALL_BUILTIN",
        CALL_BUILTIN_OWNED => "CALL_BUILTIN_OWNED",
        CALL_KNOWN_OWNED => "CALL_KNOWN_OWNED",
        TAIL_CALL_SELF => "TAIL_CALL_SELF",
        TAIL_CALL_KNOWN => "TAIL_CALL_KNOWN",
        RETURN => "RETURN",
        LIST_NIL => "LIST_NIL",
        LIST_CONS => "LIST_CONS",
        LIST_NEW => "LIST_NEW",
        RECORD_NEW => "RECORD_NEW",
        STORE_GLOBAL => "STORE_GLOBAL",
        RECORD_GET => "RECORD_GET",
        RECORD_GET_NAMED => "RECORD_GET_NAMED",
        VARIANT_NEW => "VARIANT_NEW",
        WRAP => "WRAP",
        TUPLE_NEW => "TUPLE_NEW",
        RECORD_UPDATE => "RECORD_UPDATE",
        PROPAGATE_ERR => "PROPAGATE_ERR",
        LIST_LEN => "LIST_LEN",
        LIST_PREPEND => "LIST_PREPEND",
        MATCH_TAG => "MATCH_TAG",
        MATCH_VARIANT => "MATCH_VARIANT",
        MATCH_UNWRAP => "MATCH_UNWRAP",
        MATCH_INT_LITERAL => "MATCH_INT_LITERAL",
        MATCH_NIL => "MATCH_NIL",
        MATCH_CONS => "MATCH_CONS",
        LIST_HEAD_TAIL => "LIST_HEAD_TAIL",
        EXTRACT_FIELD => "EXTRACT_FIELD",
        MATCH_TUPLE => "MATCH_TUPLE",
        EXTRACT_TUPLE_ITEM => "EXTRACT_TUPLE_ITEM",
        MATCH_FAIL => "MATCH_FAIL",
        MATCH_DISPATCH => "MATCH_DISPATCH",
        MATCH_DISPATCH_CONST => "MATCH_DISPATCH_CONST",
        TAIL_CALL_SELF_THIN => "TAIL_CALL_SELF_THIN",
        UNWRAP_OR => "UNWRAP_OR",
        UNWRAP_RESULT_OR => "UNWRAP_RESULT_OR",
        CALL_LEAF => "CALL_LEAF",
        LOAD_LOCAL_2 => "LOAD_LOCAL_2",
        LOAD_LOCAL_CONST => "LOAD_LOCAL_CONST",
        VECTOR_GET => "VECTOR_GET",
        VECTOR_GET_OR => "VECTOR_GET_OR",
        VECTOR_SET => "VECTOR_SET",
        VECTOR_SET_OR_KEEP => "VECTOR_SET_OR_KEEP",
        BUFFER_NEW => "BUFFER_NEW",
        BUFFER_APPEND_STR => "BUFFER_APPEND_STR",
        BUFFER_APPEND_SEP_UNLESS_FIRST => "BUFFER_APPEND_SEP_UNLESS_FIRST",
        BUFFER_FINALIZE => "BUFFER_FINALIZE",
        INT_DIV_EUCLID => "INT_DIV_EUCLID",
        INT_MOD_EUCLID => "INT_MOD_EUCLID",
        BITS_SHIFT_LEFT => "BITS_SHIFT_LEFT",
        BITS_SHIFT_RIGHT => "BITS_SHIFT_RIGHT",
        BITS_LOW => "BITS_LOW",
        STR_CURSOR_END => "STR_CURSOR_END",
        STR_CURSOR_HEAD => "STR_CURSOR_HEAD",
        STR_CURSOR_NEXT => "STR_CURSOR_NEXT",
        STR_CODE1 => "STR_CODE1",
        STR_CODE1_LOWER => "STR_CODE1_LOWER",
        STR_CODE1_UPPER => "STR_CODE1_UPPER",
        STR_CURSOR_CODE => "STR_CURSOR_CODE",
        STR_FOLD_LOWER => "STR_FOLD_LOWER",
        STR_FOLD_UPPER => "STR_FOLD_UPPER",
        LIST_BUILDER_NEW => "LIST_BUILDER_NEW",
        LIST_BUILDER_PUSH => "LIST_BUILDER_PUSH",
        LIST_BUILDER_FINALIZE => "LIST_BUILDER_FINALIZE",
        BYTE_BUILDER_NEW => "BYTE_BUILDER_NEW",
        BYTE_BUILDER_PUSH => "BYTE_BUILDER_PUSH",
        BYTE_BUILDER_FINALIZE => "BYTE_BUILDER_FINALIZE",
        STR_INDEX_BUILD => "STR_INDEX_BUILD",
        STR_INDEX_CHAR_AT => "STR_INDEX_CHAR_AT",
        STR_INDEX_SLICE => "STR_INDEX_SLICE",
        VECTOR_NEW_LITERAL => "VECTOR_NEW_LITERAL",
        BRANCH_PATH_CHILD_LITERAL => "BRANCH_PATH_CHILD_LITERAL",
        BRANCH_PATH_PARSE_LITERAL => "BRANCH_PATH_PARSE_LITERAL",
        RESULT_PROVEN => "RESULT_PROVEN",
        CALL_PAR => "CALL_PAR",
        NOP => "NOP",
        _ => "UNKNOWN",
    }
}

/// Operand byte width after the opcode byte. Single source of truth —
/// all bytecode traversal functions must use this.
pub fn opcode_operand_width(op: u8, code: &[u8], ip: usize) -> usize {
    match op {
        // 0-byte (stack-only)
        POP
        | DUP
        | LOAD_UNIT
        | LOAD_TRUE
        | LOAD_FALSE
        | ADD
        | ADD_INT
        | SUB_INT
        | MUL_INT
        | ADD_FLOAT
        | SUB_FLOAT
        | MUL_FLOAT
        | DIV_FLOAT
        | SUB
        | MUL
        | DIV
        | MOD
        | NEG
        | NEG_INT
        | NEG_FLOAT
        | NOT
        | EQ
        | EQ_INT
        | LT
        | LT_INT
        | LT_FLOAT
        | GT
        | GT_INT
        | GT_FLOAT
        | RETURN
        | PROPAGATE_ERR
        | LIST_HEAD_TAIL
        | LIST_NIL
        | LIST_CONS
        | LIST_LEN
        | LIST_PREPEND
        | UNWRAP_OR
        | UNWRAP_RESULT_OR
        | CONCAT
        | VECTOR_GET
        | VECTOR_SET
        | BUFFER_NEW
        | BUFFER_APPEND_STR
        | BUFFER_APPEND_SEP_UNLESS_FIRST
        | BUFFER_FINALIZE
        | INT_DIV_EUCLID
        | INT_MOD_EUCLID
        | BITS_SHIFT_LEFT
        | BITS_SHIFT_RIGHT
        | BITS_LOW
        | STR_CURSOR_END
        | STR_CURSOR_HEAD
        | STR_CURSOR_NEXT
        | STR_CODE1
        | STR_CODE1_LOWER
        | STR_CODE1_UPPER
        | STR_CURSOR_CODE
        | STR_FOLD_LOWER
        | STR_FOLD_UPPER
        | LIST_BUILDER_NEW
        | LIST_BUILDER_PUSH
        | LIST_BUILDER_FINALIZE
        | BYTE_BUILDER_NEW
        | BYTE_BUILDER_PUSH
        | BYTE_BUILDER_FINALIZE
        | STR_INDEX_BUILD
        | STR_INDEX_CHAR_AT
        | STR_INDEX_SLICE
        | VECTOR_NEW_LITERAL
        | BRANCH_PATH_CHILD_LITERAL
        | BRANCH_PATH_PARSE_LITERAL
        | RESULT_PROVEN
        | NOP => 0,

        // 1-byte
        LOAD_LOCAL | MOVE_LOCAL | STORE_LOCAL | CALL_VALUE | RECORD_GET | EXTRACT_FIELD
        | EXTRACT_TUPLE_ITEM | WRAP => 1,

        // 2-byte (u16 or u8+u8)
        LOAD_CONST | LOAD_GLOBAL | STORE_GLOBAL | JUMP | JUMP_IF_FALSE | MATCH_FAIL | MATCH_NIL
        | MATCH_CONS | LOAD_LOCAL_2 | VECTOR_GET_OR | TAIL_CALL_SELF | TAIL_CALL_SELF_THIN => 2,

        // owned:u8 + target_slot:u8. The slot is the target's own local cell,
        // which the runtime fence must exempt: the fusion deletes the
        // textually-last read, so the target comes in through `LOAD_LOCAL` and
        // its own cell is still live at the write. See
        // `VM::runtime_confirms_fused_vector_grant`.
        VECTOR_SET_OR_KEEP => 2,

        // 3-byte
        CALL_KNOWN | CALL_LEAF | MATCH_TAG | MATCH_UNWRAP | MATCH_TUPLE | RECORD_NEW
        | LOAD_LOCAL_CONST => 3,

        // 4-byte
        CALL_KNOWN_OWNED | TAIL_CALL_KNOWN => 4, // fn_id:u16 + argc:u8 + owned:u8

        // 4-byte
        MATCH_VARIANT | RECORD_GET_NAMED | LIST_NEW | TUPLE_NEW => 4,

        // 5-byte
        CALL_BUILTIN | VARIANT_NEW => 5,

        // 6-byte
        CALL_BUILTIN_OWNED => 6, // symbol_id:u32 + argc:u8 + owned:u8

        // 10-byte
        MATCH_INT_LITERAL => 10, // imm:i64 + fail_offset:i16

        // Variable-length
        MATCH_DISPATCH | MATCH_DISPATCH_CONST if ip < code.len() => {
            let count = code[ip] as usize;
            let entry_size = if op == MATCH_DISPATCH { 11 } else { 17 };
            3 + count * entry_size
        }
        RECORD_UPDATE if ip + 2 < code.len() => 3 + code[ip + 2] as usize,
        // CALL_PAR count:u8 unwrap:u8 [argc:u8 × count]
        CALL_PAR if ip < code.len() => {
            let count = code[ip] as usize;
            2 + count
        }
        _ => 0,
    }
}
