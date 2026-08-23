//! Bytecode chunk classification: leaf, thin, parent-thin.
//!
//! These passes analyse compiled chunks to decide which calling
//! conventions and optimisations can be applied (frameless leaf calls,
//! thin arena finalization, parent-thin inlining, TAIL_CALL_SELF_THIN).

use super::super::builtin::VmBuiltinParentThinClass;
use super::super::opcode::*;
use super::super::types::{CodeStore, FnChunk};
use super::CompileError;

/// Run all classification passes on every chunk in the code store.
pub fn classify_thin_functions(
    code: &mut CodeStore,
    arena: &crate::nan_value::Arena,
) -> Result<(), CompileError> {
    let thin_flags: Vec<bool> = code
        .functions
        .iter()
        .map(classify_thin_chunk)
        .collect::<Result<_, _>>()?;
    let parent_thin_flags = classify_parent_thin_chunks(code, arena)?;

    let leaf_flags: Vec<bool> = code
        .functions
        .iter()
        .map(classify_leaf_chunk)
        .collect::<Result<_, _>>()?;

    for (((chunk, thin), parent_thin), leaf) in code
        .functions
        .iter_mut()
        .zip(thin_flags.iter().copied())
        .zip(parent_thin_flags)
        .zip(leaf_flags)
    {
        chunk.thin = thin;
        chunk.parent_thin = parent_thin;
        chunk.leaf = leaf;
    }

    // Post-pass: upgrade TAIL_CALL_SELF → TAIL_CALL_SELF_THIN.
    let thin_ignoring_tco: Vec<bool> = code
        .functions
        .iter()
        .map(classify_thin_ignoring_self_tco)
        .collect::<Result<_, _>>()?;
    for (fn_id, qualifies) in thin_ignoring_tco.iter().enumerate() {
        if !qualifies {
            continue;
        }
        let chunk = &code.functions[fn_id];
        let positions: Vec<usize> = find_opcode_positions(chunk, TAIL_CALL_SELF);
        for pos in positions {
            code.functions[fn_id].code[pos] = TAIL_CALL_SELF_THIN;
        }
    }

    // Post-pass: upgrade CALL_KNOWN → CALL_LEAF for leaf+args-only targets.
    for fn_id in 0..code.functions.len() {
        let positions: Vec<usize> = find_opcode_positions(&code.functions[fn_id], CALL_KNOWN);
        for pos in positions {
            let chunk_code = &code.functions[fn_id].code;
            if pos + 3 >= chunk_code.len() {
                continue;
            }
            let target_fn_id =
                u16::from_be_bytes([chunk_code[pos + 1], chunk_code[pos + 2]]) as usize;
            if target_fn_id >= code.functions.len() {
                continue;
            }
            let target = &code.functions[target_fn_id];
            if target.leaf && target.local_count == target.arity as u16 {
                code.functions[fn_id].code[pos] = CALL_LEAF;
            }
        }
    }

    Ok(())
}

const MAX_PARENT_THIN_CODE_LEN: usize = 80;
const MAX_PARENT_THIN_LOCALS: u16 = 8;

fn classify_parent_thin_chunks(
    code: &CodeStore,
    _arena: &crate::nan_value::Arena,
) -> Result<Vec<bool>, CompileError> {
    let mut candidates: Vec<bool> = code
        .functions
        .iter()
        .map(|chunk| base_parent_thin_chunk(code, chunk))
        .collect::<Result<_, _>>()?;

    loop {
        let mut changed = false;
        for fn_id in 0..code.functions.len() {
            if !candidates[fn_id] {
                continue;
            }
            if !parent_thin_calls_are_safe(fn_id, code, &candidates)? {
                candidates[fn_id] = false;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    Ok(candidates)
}

fn parent_thin_builtin_is_allowed(
    code_store: &CodeStore,
    chunk: &FnChunk,
    ip: usize,
) -> Result<bool, CompileError> {
    if ip + 5 > chunk.code.len() {
        return Err(CompileError {
            msg: format!("truncated bytecode in {}", chunk.name),
        });
    }
    let symbol_id = u32::from_be_bytes([
        chunk.code[ip],
        chunk.code[ip + 1],
        chunk.code[ip + 2],
        chunk.code[ip + 3],
    ]);
    // A provider call is never parent-thin: even a semantically pure
    // capability may allocate or cross a host boundary. It is nevertheless
    // a valid CALL_BUILTIN-shaped symbol, not a corrupt builtin id.
    if code_store.symbols.resolve_capability(symbol_id).is_some() {
        return Ok(false);
    }
    let builtin = code_store
        .symbols
        .resolve_builtin(symbol_id)
        .ok_or_else(|| CompileError {
            msg: format!("unknown builtin symbol {} in {}", symbol_id, chunk.name),
        })?;
    Ok(!matches!(
        builtin.parent_thin_class(),
        VmBuiltinParentThinClass::AllocHeavy
    ))
}

fn base_parent_thin_chunk(code_store: &CodeStore, chunk: &FnChunk) -> Result<bool, CompileError> {
    if !chunk.effects.is_empty()
        || chunk.code.len() > MAX_PARENT_THIN_CODE_LEN
        || chunk.local_count > MAX_PARENT_THIN_LOCALS
    {
        return Ok(false);
    }

    let code = &chunk.code;
    let mut ip = 0usize;
    while ip < code.len() {
        let op = code[ip];
        ip += 1;
        match op {
            STORE_GLOBAL | TAIL_CALL_SELF | TAIL_CALL_KNOWN | CALL_VALUE | MATCH_CONS
            | LIST_HEAD_TAIL | CONCAT | LIST_CONS | LIST_PREPEND | RECORD_UPDATE | LIST_NEW
            | WRAP | TUPLE_NEW | CALL_PAR | RECORD_NEW | TAIL_CALL_SELF_THIN => {
                return Ok(false);
            }

            CALL_BUILTIN | CALL_BUILTIN_OWNED => {
                if !parent_thin_builtin_is_allowed(code_store, chunk, ip)? {
                    return Ok(false);
                }
                ip += opcode_operand_width(op, code, ip);
            }

            VARIANT_NEW => {
                if ip + 5 > code.len() {
                    return Err(CompileError {
                        msg: format!("truncated bytecode in {}", chunk.name),
                    });
                }
                let field_count = code[ip + 4];
                if field_count != 0 {
                    return Ok(false);
                }
                ip += opcode_operand_width(op, code, ip);
            }

            _ => {
                ip += opcode_operand_width(op, code, ip);
            }
        }
    }

    Ok(true)
}

fn parent_thin_calls_are_safe(
    current_fn_id: usize,
    code: &CodeStore,
    candidates: &[bool],
) -> Result<bool, CompileError> {
    let chunk = &code.functions[current_fn_id];
    let bytes = &chunk.code;
    let mut ip = 0usize;
    while ip < bytes.len() {
        let op = bytes[ip];
        ip += 1;
        match op {
            // CALL_KNOWN_OWNED carries the same leading `fn_id:u16` as
            // CALL_KNOWN (only a trailing owned byte differs, absorbed by
            // `opcode_operand_width`), so the same self-recursion /
            // effectful-target safety check applies. It isn't emitted
            // today, but stays guarded so a future emitter can't bypass
            // this gate.
            CALL_KNOWN | CALL_KNOWN_OWNED => {
                if ip + 3 > bytes.len() {
                    return Err(CompileError {
                        msg: format!("truncated bytecode in {}", chunk.name),
                    });
                }
                let target_fn_id = u16::from_be_bytes([bytes[ip], bytes[ip + 1]]) as usize;
                if target_fn_id == current_fn_id || target_fn_id >= candidates.len() {
                    return Ok(false);
                }
                if !code.functions[target_fn_id].effects.is_empty() {
                    return Ok(false);
                }
                ip += opcode_operand_width(op, bytes, ip);
            }
            _ => {
                ip += opcode_operand_width(op, bytes, ip);
            }
        }
    }
    Ok(true)
}

fn classify_leaf_chunk(chunk: &FnChunk) -> Result<bool, CompileError> {
    let code = &chunk.code;
    let mut ip = 0usize;
    while ip < code.len() {
        let op = code[ip];
        ip += 1;
        match op {
            CALL_KNOWN | CALL_KNOWN_OWNED | CALL_VALUE | TAIL_CALL_SELF | TAIL_CALL_KNOWN
            | TAIL_CALL_SELF_THIN => {
                // CALL_KNOWN_OWNED counts as a call: a chunk that makes
                // one is NOT a leaf. The MIR walker no longer emits it
                // (it emits plain CALL_KNOWN), but it stays a live opcode
                // with an execute handler, so a future emitter must not be
                // able to slip a non-leaf fn past this gate and have a
                // caller's CALL_KNOWN upgraded to the frameless CALL_LEAF.
                return Ok(false);
            }
            _ => {}
        }
        ip += opcode_operand_width(op, code, ip);
    }
    Ok(true)
}

fn classify_thin_chunk(chunk: &FnChunk) -> Result<bool, CompileError> {
    let code = &chunk.code;
    let mut ip = 0usize;
    while ip < code.len() {
        let op = code[ip];
        ip += 1;
        match op {
            STORE_GLOBAL | TAIL_CALL_SELF | TAIL_CALL_KNOWN | CONCAT | LIST_NIL | LIST_CONS
            | LIST_NEW | RECORD_NEW | WRAP | TUPLE_NEW | CALL_PAR | RECORD_UPDATE | LIST_LEN
            | LIST_PREPEND | VECTOR_GET | VECTOR_GET_OR | VECTOR_SET | VECTOR_SET_OR_KEEP
            | STR_INDEX_BUILD | STR_INDEX_CHAR_AT | STR_INDEX_SLICE | TAIL_CALL_SELF_THIN => {
                return Ok(false);
            }

            VARIANT_NEW => {
                if ip + 5 > code.len() {
                    return Err(CompileError {
                        msg: format!("truncated bytecode in {}", chunk.name),
                    });
                }
                let field_count = code[ip + 4];
                if field_count != 0 {
                    return Ok(false);
                }
                ip += opcode_operand_width(op, code, ip);
            }

            _ => {
                ip += opcode_operand_width(op, code, ip);
            }
        }
    }
    Ok(true)
}

fn classify_thin_ignoring_self_tco(chunk: &FnChunk) -> Result<bool, CompileError> {
    let code = &chunk.code;
    let mut ip = 0usize;
    while ip < code.len() {
        let op = code[ip];
        ip += 1;
        match op {
            STORE_GLOBAL | TAIL_CALL_KNOWN | CONCAT | LIST_NIL | LIST_CONS | LIST_NEW
            | RECORD_NEW | WRAP | TUPLE_NEW | CALL_PAR | RECORD_UPDATE | LIST_LEN
            | LIST_PREPEND | VECTOR_GET | VECTOR_GET_OR | VECTOR_SET | VECTOR_SET_OR_KEEP
            | STR_INDEX_BUILD | STR_INDEX_CHAR_AT | STR_INDEX_SLICE => {
                return Ok(false);
            }

            VARIANT_NEW => {
                if ip + 5 > code.len() {
                    return Err(CompileError {
                        msg: format!("truncated bytecode in {}", chunk.name),
                    });
                }
                let field_count = code[ip + 4];
                if field_count != 0 {
                    return Ok(false);
                }
                ip += opcode_operand_width(op, code, ip);
            }

            _ => {
                ip += opcode_operand_width(op, code, ip);
            }
        }
    }
    Ok(true)
}

pub fn find_opcode_positions(chunk: &FnChunk, target_op: u8) -> Vec<usize> {
    let code = &chunk.code;
    let mut positions = Vec::new();
    let mut ip = 0usize;
    while ip < code.len() {
        let op = code[ip];
        if op == target_op {
            positions.push(ip);
        }
        ip += 1;
        ip += opcode_operand_width(op, code, ip);
    }
    positions
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chunk_with_code(code: Vec<u8>) -> FnChunk {
        FnChunk {
            name: "t".to_string(),
            arity: 1,
            local_count: 1,
            code,
            constants: vec![],
            effects: vec![],
            thin: false,
            parent_thin: false,
            leaf: false,
            no_alloc: false,
            source_file: String::new(),
            line_table: vec![],
        }
    }

    #[test]
    fn call_known_owned_chunk_is_not_a_leaf() {
        // A chunk that calls out via CALL_KNOWN_OWNED makes a call, so it
        // must not be classified as a leaf — otherwise a caller's
        // CALL_KNOWN to it gets upgraded to the frameless CALL_LEAF and
        // the non-leaf fn is invoked without a CallFrame. (CALL_KNOWN_OWNED
        // is fn_id:u16, argc:u8, owned:u8.)
        let owned = chunk_with_code(vec![CALL_KNOWN_OWNED, 0x00, 0x02, 0x01, 0x00, RETURN]);
        assert!(
            !classify_leaf_chunk(&owned).unwrap(),
            "CALL_KNOWN_OWNED must disqualify a chunk from leaf status"
        );

        // Sanity: the same chunk shape with plain CALL_KNOWN is likewise
        // non-leaf (the established behavior this mirrors).
        let plain = chunk_with_code(vec![CALL_KNOWN, 0x00, 0x02, 0x01, RETURN]);
        assert!(!classify_leaf_chunk(&plain).unwrap());

        // And a builtin-only chunk stays a leaf.
        let leaf = chunk_with_code(vec![LIST_LEN, RETURN]);
        assert!(classify_leaf_chunk(&leaf).unwrap());
    }

    #[test]
    fn bytecode_walk_skips_all_tail_call_operands() {
        // The owned-mask bytes deliberately spell dispatch opcodes. A walker
        // that undercounts either tail-call shape would interpret them as
        // instructions and lose the real RETURN boundary.
        let chunk = chunk_with_code(vec![
            TAIL_CALL_SELF,
            0x01,
            MATCH_DISPATCH,
            TAIL_CALL_KNOWN,
            0x00,
            0x02,
            0x01,
            MATCH_DISPATCH_CONST,
            RETURN,
        ]);

        assert_eq!(opcode_operand_width(TAIL_CALL_SELF, &chunk.code, 1), 2);
        assert_eq!(opcode_operand_width(TAIL_CALL_KNOWN, &chunk.code, 4), 4);
        assert_eq!(find_opcode_positions(&chunk, RETURN), vec![8]);
        assert!(find_opcode_positions(&chunk, MATCH_DISPATCH).is_empty());
        assert!(find_opcode_positions(&chunk, MATCH_DISPATCH_CONST).is_empty());
    }
}
