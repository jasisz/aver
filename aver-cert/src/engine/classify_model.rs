/// Symbolically execute a body-consumed fuel recursion's step arm — a
/// straight-line stack program ending in the host `add` — and recover
/// `(sub, add, rec_first, other)`: which host helpers are the descent/combinator,
/// which side of the `add` the recursive result sits on, and what the other
/// operand is. The descent is pinned to `sub(input, box 1)` = `n-1`; anything the
/// evaluator cannot account for (foreign locals, a second add, a non-descent
/// self-call argument) fails, so the recogniser stays fail-closed.
fn parse_body_step(
    ops: &[Op],
    box_idx: u32,
    self_idx: u32,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<(u32, u32, bool, BodyOperand)> {
    use Op::*;
    #[derive(Clone, Copy, PartialEq)]
    enum V {
        Input,
        IntLit(i64),
        Boxed(i64),
        Descent,
        Rec,
    }
    // The step is `<push operand A> <push operand B> add`; the trailing `add`
    // combines the top two stack values.
    let (last, init) = ops.split_last()?;
    let Call(add_idx) = last else { return None };
    if !matches!(
        host_roles.get(add_idx),
        Some(HostRole::Add) | Some(HostRole::Mul)
    ) {
        return None;
    }
    let mut st: Vec<V> = Vec::new();
    let mut sub_idx: Option<u32> = None;
    for op in init {
        match op {
            LocalGet(0) => st.push(V::Input),
            I64Const(k) => st.push(V::IntLit(*k)),
            Call(idx) if *idx == box_idx => {
                let V::IntLit(k) = st.pop()? else { return None };
                st.push(V::Boxed(k));
            }
            Call(idx) if *idx == self_idx => {
                if st.pop()? != V::Descent {
                    return None;
                }
                st.push(V::Rec);
            }
            Call(idx) if host_roles.get(idx) == Some(&HostRole::Sub) => {
                let b = st.pop()?;
                let a = st.pop()?;
                if a != V::Input || b != V::Boxed(1) {
                    return None;
                }
                if sub_idx.is_some_and(|s| s != *idx) {
                    return None;
                }
                sub_idx = Some(*idx);
                st.push(V::Descent);
            }
            _ => return None,
        }
    }
    // exactly the two `add` operands remain.
    let [a, b] = st.as_slice() else { return None };
    let operand = |v: &V| match v {
        V::Input => Some(BodyOperand::Input),
        V::Boxed(k) => Some(BodyOperand::Const(*k)),
        _ => None,
    };
    let (rec_first, other) = if *a == V::Rec {
        (true, operand(b)?)
    } else if *b == V::Rec {
        (false, operand(a)?)
    } else {
        return None;
    };
    Some((sub_idx?, *add_idx, rec_first, other))
}

/// The combinator operator of each `X__fuel` model definition's else-branch:
/// `+` (add) or `*` (mul). The bytes cannot distinguish the bignum helpers, so
/// this is the trusted source of the operation. Both the emitter (`analyze`) and
/// the checker (`rederive_certificate`) build the SAME map from the model, so the
/// re-derived host still pins. The descent (`n - 1`) uses `-`, so it never
/// confuses the scan; the recognised body shapes carry no other arithmetic.
fn model_step_ops(model_files: &[(String, String)]) -> std::collections::HashMap<String, char> {
    let mut ops = std::collections::HashMap::new();
    for (path, content) in model_files {
        if !path.ends_with(".lean") {
            continue;
        }
        let lines: Vec<&str> = content.lines().collect();
        for i in 0..lines.len() {
            let Some(rest) = lines[i].trim().strip_prefix("def ") else {
                continue;
            };
            let Some(fuel_pos) = rest.find("__fuel ") else {
                continue;
            };
            let name = rest[..fuel_pos].to_string();
            for l in lines.iter().skip(i).take(8) {
                if let Some(p) = l.find("else ") {
                    let els = &l[p + 5..];
                    let op = if els.contains('*') {
                        Some('*')
                    } else if els.contains('+') {
                        Some('+')
                    } else {
                        None
                    };
                    if let Some(op) = op {
                        ops.insert(name.clone(), op);
                    }
                    break;
                }
            }
        }
    }
    ops
}
