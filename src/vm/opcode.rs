// Aver VM bytecode opcodes.
//
// Stack-based: operands are pushed/popped from the operand stack.
// Variable-width encoding: opcode (1 byte) + operands (0-3 bytes).

// -- Stack / locals ----------------------------------------------------------

/// Push `stack[bp + slot]` onto the operand stack.
pub const LOAD_LOCAL: u8 = 0x01; // slot:u8

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

// -- Comparison --------------------------------------------------------------

/// Pop b, pop a, push a == b.
pub const EQ: u8 = 0x20;

/// Pop b, pop a, push a < b.
pub const LT: u8 = 0x21;

/// Pop b, pop a, push a > b.
pub const GT: u8 = 0x22;

// -- String ------------------------------------------------------------------

/// Pop b, pop a, push str(a) ++ str(b).
pub const CONCAT: u8 = 0x28;

// -- Control flow ------------------------------------------------------------

/// Unconditional relative jump: ip += offset.
pub const JUMP: u8 = 0x30; // offset:i16

/// Pop top, if falsy: ip += offset.
pub const JUMP_IF_FALSE: u8 = 0x31; // offset:i16

/// Enter an arm-local young subregion for match evaluation.
pub const MATCH_ARM_ENTER: u8 = 0x32;

/// Keep top-of-stack result, compact live young values from the current arm,
/// and leave the arm-local young subregion.
pub const MATCH_ARM_LEAVE: u8 = 0x33;

/// Abort the current arm-local young subregion without preserving any roots.
pub const MATCH_ARM_ABORT: u8 = 0x34;

// -- Calls -------------------------------------------------------------------

/// Call a known function by id. Args already on stack.
pub const CALL_KNOWN: u8 = 0x40; // fn_id:u16, argc:u8

/// Call a function value on the stack (under args).
pub const CALL_VALUE: u8 = 0x41; // argc:u8

/// Call a builtin service function.
pub const CALL_BUILTIN: u8 = 0x42; // builtin_id:u16, argc:u8

/// Self tail-call: reuse current frame with new args.
pub const TAIL_CALL_SELF: u8 = 0x43; // argc:u8

/// Mutual tail-call to a known function: reuse frame, switch target.
pub const TAIL_CALL_KNOWN: u8 = 0x44; // fn_id:u16, argc:u8

/// Return top of stack to caller.
pub const RETURN: u8 = 0x50;

// -- Structured values -------------------------------------------------------

/// Push Nil (empty cons list).
pub const LIST_NIL: u8 = 0x60;

/// Pop tail, pop head, push Cons(head, tail).
pub const LIST_CONS: u8 = 0x61;

/// Pop `count` items, build cons list from them (first item = head), push list.
pub const LIST_NEW: u8 = 0x62; // count:u8

/// Pop `count` field values, push a new record with `type_id`.
pub const RECORD_NEW: u8 = 0x63; // type_id:u16, count:u8

/// Pop record, push `fields[field_idx]` (compile-time resolved index).
pub const RECORD_GET: u8 = 0x64; // field_idx:u8

/// Pop record, lookup field by name (constants[name_idx] is string), push value.
pub const RECORD_GET_NAMED: u8 = 0x67; // name_idx:u16

/// Pop `count` field values, push a new variant.
pub const VARIANT_NEW: u8 = 0x65; // type_id:u16, variant_id:u16, count:u8

/// Pop value, push wrapped value. kind: 0=Ok, 1=Err, 2=Some.
pub const WRAP: u8 = 0x66; // kind:u8

/// Pop `count` items, build a tuple from them, push tuple.
pub const TUPLE_NEW: u8 = 0x68; // count:u8

/// Update selected fields on a record, preserving the rest from the base value.
/// Stack: [..., base_record, update_0, ..., update_n-1] -> [..., updated_record]
pub const RECORD_UPDATE: u8 = 0x69; // type_id:u16, count:u8, field_idx[count]:u8

/// Propagate `Result.Err` to caller or unwrap `Result.Ok` in place.
pub const PROPAGATE_ERR: u8 = 0x6A;

// -- Pattern matching --------------------------------------------------------

/// Peek top: if NaN tag != expected, ip += fail_offset.
pub const MATCH_TAG: u8 = 0x70; // expected_tag:u8, fail_offset:i16

/// Peek top (must be variant): if variant_id != expected, ip += fail_offset.
pub const MATCH_VARIANT: u8 = 0x71; // variant_id:u16, fail_offset:i16

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

/// Opcode name for debug/disassembly.
pub fn opcode_name(op: u8) -> &'static str {
    match op {
        LOAD_LOCAL => "LOAD_LOCAL",
        STORE_LOCAL => "STORE_LOCAL",
        LOAD_CONST => "LOAD_CONST",
        LOAD_GLOBAL => "LOAD_GLOBAL",
        POP => "POP",
        DUP => "DUP",
        LOAD_UNIT => "LOAD_UNIT",
        LOAD_TRUE => "LOAD_TRUE",
        LOAD_FALSE => "LOAD_FALSE",
        ADD => "ADD",
        SUB => "SUB",
        MUL => "MUL",
        DIV => "DIV",
        MOD => "MOD",
        NEG => "NEG",
        NOT => "NOT",
        EQ => "EQ",
        LT => "LT",
        GT => "GT",
        CONCAT => "CONCAT",
        JUMP => "JUMP",
        JUMP_IF_FALSE => "JUMP_IF_FALSE",
        MATCH_ARM_ENTER => "MATCH_ARM_ENTER",
        MATCH_ARM_LEAVE => "MATCH_ARM_LEAVE",
        MATCH_ARM_ABORT => "MATCH_ARM_ABORT",
        CALL_KNOWN => "CALL_KNOWN",
        CALL_VALUE => "CALL_VALUE",
        CALL_BUILTIN => "CALL_BUILTIN",
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
        MATCH_TAG => "MATCH_TAG",
        MATCH_VARIANT => "MATCH_VARIANT",
        MATCH_UNWRAP => "MATCH_UNWRAP",
        MATCH_NIL => "MATCH_NIL",
        MATCH_CONS => "MATCH_CONS",
        LIST_HEAD_TAIL => "LIST_HEAD_TAIL",
        EXTRACT_FIELD => "EXTRACT_FIELD",
        MATCH_TUPLE => "MATCH_TUPLE",
        EXTRACT_TUPLE_ITEM => "EXTRACT_TUPLE_ITEM",
        MATCH_FAIL => "MATCH_FAIL",
        _ => "UNKNOWN",
    }
}
