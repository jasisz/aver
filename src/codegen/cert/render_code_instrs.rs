enum LeanInstr {
    Simple(String),
    IfElse(Vec<LeanInstr>, Vec<LeanInstr>),
}

fn render_ops_value(ops: &[Op]) -> String {
    let mut pos = 0usize;
    let instrs = parse_lean_instrs(ops, &mut pos, false).unwrap_or_default();
    render_lean_instr_list(&instrs)
}

fn parse_lean_instrs(ops: &[Op], pos: &mut usize, nested: bool) -> Option<Vec<LeanInstr>> {
    let mut out = Vec::new();
    while *pos < ops.len() {
        match &ops[*pos] {
            Op::Else | Op::End if nested => break,
            Op::If => {
                *pos += 1;
                let then_b = parse_lean_instrs(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::Else)) {
                    return None;
                }
                *pos += 1;
                let else_b = parse_lean_instrs(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::End)) {
                    return None;
                }
                *pos += 1;
                out.push(LeanInstr::IfElse(then_b, else_b));
            }
            Op::Else | Op::End => return None,
            op => {
                out.push(LeanInstr::Simple(render_simple_op(op)?));
                *pos += 1;
            }
        }
    }
    Some(out)
}

fn render_simple_op(op: &Op) -> Option<String> {
    Some(match op {
        Op::LocalGet(i) => format!(".localGet {i}"),
        Op::LocalSet(i) => format!(".localSet {i}"),
        Op::I64Const(n) => format!(".i64Const ({n})"),
        Op::I32Const(n) => format!(".i32Const ({n})"),
        Op::F64Const(bits) => format!(".f64Const 0x{bits:016x}"),
        Op::RefTest(t) => format!(".refTest {t}"),
        Op::RefCast(t) => format!(".refCast {t}"),
        Op::StructNew(t, n) => format!(".structNew {t} {n}"),
        Op::StructGet(t, f) => format!(".structGet {t} {f}"),
        Op::ArrayNewData {
            type_idx, bytes, ..
        } => format!(".arrayNewData {type_idx} {}", render_nat_list(bytes)),
        Op::ArrayNewFixed(t, n) => format!(".arrayNewFixed {t} {n}"),
        // The heap-type payload is deliberately NOT rendered: Lean's
        // `CoreInstr.refNull` is a unit constructor, so emitting the index
        // would change the audited artifact bytes. The index is carried only
        // for Rust-side re-lowering in the S2 grammar leg.
        Op::RefNull(_) => ".refNull".to_string(),
        Op::RefIsNull => ".refIsNull".to_string(),
        Op::I64Eq => ".i64Eq".to_string(),
        Op::I64LeS => ".i64LeS".to_string(),
        Op::I64LtS => ".i64LtS".to_string(),
        Op::I64GeS => ".i64GeS".to_string(),
        Op::F64Add => ".f64Add".to_string(),
        Op::F64Mul => ".f64Mul".to_string(),
        Op::F64Le => ".f64Le".to_string(),
        Op::I32LtS => ".i32LtS".to_string(),
        Op::I32GtS => ".i32GtS".to_string(),
        Op::Call(f) => format!(".call {f}"),
        Op::ReturnCall(f) => format!(".returnCall {f}"),
        Op::ArrayNewDataUnresolved { .. } | Op::If | Op::Else | Op::End | Op::Other => {
            return None;
        }
    })
}

fn render_nat_list(bytes: &[u8]) -> String {
    let parts = bytes
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

fn render_wval(default: &VerbatimDefault) -> String {
    match default {
        VerbatimDefault::Null => ".null".to_string(),
        VerbatimDefault::F64Bits(bits) => format!(".f64v 0x{bits:016x}"),
        VerbatimDefault::Array {
            type_idx, bytes, ..
        } => {
            format!(".arr {type_idx} {}", render_array_elements(bytes))
        }
    }
}

fn render_wval_qualified(default: &VerbatimDefault) -> String {
    match default {
        VerbatimDefault::Null => "WVal.null".to_string(),
        VerbatimDefault::F64Bits(bits) => format!("WVal.f64v 0x{bits:016x}"),
        VerbatimDefault::Array {
            type_idx, bytes, ..
        } => {
            format!("WVal.arr {type_idx} {}", render_array_elements(bytes))
        }
    }
}

fn render_wval_arg(default: &VerbatimDefault) -> String {
    format!("({})", render_wval(default))
}

fn render_string_eq_default(default: &StringEqDefault, input: &str) -> String {
    match default {
        StringEqDefault::Input => input.to_string(),
        StringEqDefault::Verbatim(k) => render_wval(k),
    }
}

fn render_array_elements(bytes: &[u8]) -> String {
    let parts = bytes
        .iter()
        .map(|b| format!(".i32v {}", *b as i32))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

fn render_default_guard(default: &VerbatimDefault) -> String {
    match default {
        VerbatimDefault::Null => {
            "(fun w => match w with | .null => some 0 | _ => none) = some 0".to_string()
        }
        VerbatimDefault::F64Bits(bits) => format!(
            "(fun w => match w with | .f64v bits => some bits | _ => none) = some (0x{bits:016x} : UInt64)"
        ),
        VerbatimDefault::Array {
            type_idx, bytes, ..
        } => format!(
            "(fun w => match w with | .arr t es => if t = {type_idx} ∧ es.length = {} then some 0 else none | _ => none) = some 0",
            bytes.len()
        ),
    }
}

fn render_lean_instr_list(instrs: &[LeanInstr]) -> String {
    let parts = instrs
        .iter()
        .map(render_lean_instr)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

fn render_lean_instr(instr: &LeanInstr) -> String {
    match instr {
        LeanInstr::Simple(s) => s.clone(),
        LeanInstr::IfElse(t, e) => format!(
            ".ifElse {} {}",
            render_lean_instr_list(t),
            render_lean_instr_list(e)
        ),
    }
}
