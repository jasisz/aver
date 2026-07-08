fn verbatim_default_from_ops(ops: &[Op]) -> Option<VerbatimDefault> {
    match ops {
        [Op::RefNull] => Some(VerbatimDefault::Null),
        [Op::F64Const(bits)] => Some(VerbatimDefault::F64Bits(*bits)),
        [
            Op::I32Const(0),
            Op::I32Const(_),
            Op::ArrayNewData(type_idx, bytes),
        ] => Some(VerbatimDefault::Array {
            type_idx: *type_idx,
            bytes: bytes.clone(),
        }),
        _ => None,
    }
}

/// A `ref.test` dispatch over a user enum where EVERY arm — hit arms and the
/// terminal else — returns a VERBATIM constant (a String-literal `array.new_data`,
/// a null, or an f64), with no host, no arithmetic, no field projection and no
/// user calls (`unescapedChar`). The generalisation of the verbatim widened match
/// from "one projected hit + a default" to "k constant arms". Certified over
/// `Cod := WVal` / `verbatimRepr`, so no carrier or string representation is
/// needed. Distinct from the variant dispatch (whose hit arms project an Int
/// payload) and the verbatim widened match (whose single hit arm projects a
/// field); a pure-constant hit arm matches neither.
fn nr_verbatim_variant_dispatch(
    f: &UserFn,
    body: &StructuralBody,
    carrier: Option<u32>,
) -> Option<Cert> {
    let carrier = carrier?;
    if !f.calls.is_empty() {
        return None;
    }
    // Every arm is a pure verbatim constant: no box, host or any call op.
    if body
        .normalized_ops
        .iter()
        .any(|op| matches!(op, Op::Call(_)))
    {
        return None;
    }
    let (arms, default) = verbatim_dispatch_chain(&body.tree)?;
    if arms.is_empty() {
        return None;
    }
    let mut tags: Vec<u32> = arms.iter().map(|(t, _)| *t).collect();
    tags.sort_unstable();
    tags.dedup();
    if tags.len() != arms.len() {
        return None;
    }
    Some(Cert::VerbatimVariantDispatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        arms,
        default,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn nr_string_eq_verbatim_match(
    f: &UserFn,
    body: &StructuralBody,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    if f.arity != 1 {
        return None;
    }
    let [TyKind::Ref(param_ty)] = f.params.as_slice() else {
        return None;
    };
    if f.result != Some(TyKind::Ref(*param_ty)) {
        return None;
    }
    let (arms, default, string_eq_idx) = string_eq_verbatim_chain(&body.tree, host_roles)?;
    if arms.len() != 1 {
        return None;
    }
    Some(Cert::StringEqVerbatimMatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        string_eq_idx,
        arms,
        default,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

/// Recognise a loop-free `String.concat` beachhead: the body builds a container
/// of string arrays (one or more `array.new_data` literals plus the input at
/// `local.get 0`), then invokes the contracted `String.concat` host slot once
/// and returns the result. The container's operand order determines the prefix
/// (literals before the input) and suffix (literals after) lists.
///
/// Admitted shape (for the first beachhead): exactly ONE `local.get 0`
/// (the input) among the container operands, plus any number of
/// `array.new_data` literals on either side. Any other operand shape (arithmetic,
/// nested calls, struct ops) declines.
fn nr_string_concat_verbatim_match(
    f: &UserFn,
    body: &StructuralBody,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    if f.arity != 1 {
        return None;
    }
    let [TyKind::Ref(param_ty)] = f.params.as_slice() else {
        return None;
    };
    // The result must be a (ref) string array too — concat returns a string.
    let Some(TyKind::Ref(result_ty)) = f.result else {
        return None;
    };
    let string_ty = *param_ty;
    if result_ty != string_ty {
        return None;
    }
    // The body must be straight-line (no IfElse nodes) — concat is a pure
    // transformation, not a dispatch.
    let flat = flatten_ops(&body.tree)?;
    // The trailing opcode must be the concat host call.
    let (call_op, pre_call) = flat.split_last()?;
    let Op::Call(concat_idx) = call_op else {
        return None;
    };
    if host_roles.get(concat_idx) != Some(&HostRole::StringConcat) {
        return None;
    }
    // Immediately before the call: `array.new_fixed container_ty n`.
    let (newfixed_op, operands) = pre_call.split_last()?;
    let Op::ArrayNewFixed(container_ty, n) = newfixed_op else {
        return None;
    };
    let n = *n as usize;
    // The container operands are emitted in stack order (first pushed =
    // deepest). Each operand is either:
    //   - the input: `[LocalGet(0)]`
    //   - a literal: `[I32Const(0), I32Const(len), ArrayNewData(ty, bytes)]`
    // Walk the operand list left-to-right; literals before the input become
    // prefixes, after become suffixes. Exactly one input must appear.
    let mut prefixes = Vec::new();
    let mut suffixes = Vec::new();
    let mut seen_input = false;
    let mut parts = 0usize;
    let mut i = 0;
    while i < operands.len() {
        if parts == n {
            return None;
        }
        // Input: a single LocalGet(0).
        if matches!(operands[i], Op::LocalGet(0)) {
            if seen_input {
                return None; // more than one input — decline
            }
            seen_input = true;
            parts += 1;
            i += 1;
            continue;
        }
        // Literal: I32Const(0), I32Const(len), ArrayNewData(ty, bytes).
        let trio @ [
            Op::I32Const(0),
            Op::I32Const(_len),
            Op::ArrayNewData(lit_ty, _bytes),
        ] = operands.get(i..i + 3)?
        else {
            return None; // unrecognised operand shape — decline
        };
        let _ = trio;
        if *lit_ty != string_ty {
            return None; // literal must be the same string byte-array type
        }
        let lit = VerbatimDefault::Array {
            type_idx: *lit_ty,
            bytes: match &operands[i + 2] {
                Op::ArrayNewData(_, bytes) => bytes.clone(),
                _ => return None,
            },
        };
        if seen_input {
            suffixes.push(lit);
        } else {
            prefixes.push(lit);
        }
        parts += 1;
        i += 3;
    }
    if parts != n || !seen_input {
        return None; // container has no input — not a beachhead
    }
    Some(Cert::StringConcatVerbatimMatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        string_concat_idx: *concat_idx,
        container_ty: *container_ty,
        result_ty,
        prefixes,
        suffixes,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

/// Flatten a `Vec<InstrNode>` into `Vec<Op>` IF it contains no `IfElse`
/// (i.e. the body is straight-line). Returns `None` if any branching node
/// is present — callers use this to reject dispatch-shaped bodies.
fn flatten_ops(tree: &[InstrNode]) -> Option<Vec<Op>> {
    let mut out = Vec::new();
    for node in tree {
        match node {
            InstrNode::Op(op) => out.push(op.clone()),
            InstrNode::IfElse(..) => return None,
        }
    }
    Some(out)
}

fn string_eq_verbatim_chain(
    nodes: &[InstrNode],
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<StringEqVerbatimChain> {
    use InstrNode::{IfElse, Op as Nop};
    let ops = node_ops(nodes);
    if matches!(ops.as_slice(), [Op::LocalGet(0)]) {
        return Some((Vec::new(), StringEqDefault::Input, 0));
    }
    if let Some(k) = verbatim_default_from_ops(&ops) {
        return Some((Vec::new(), StringEqDefault::Verbatim(k), 0));
    }

    let [
        Nop(Op::LocalGet(0)),
        maybe_cast @ ..,
        Nop(Op::I32Const(0)),
        Nop(Op::I32Const(_)),
        Nop(Op::ArrayNewData(type_idx, bytes)),
        Nop(Op::Call(eq_idx)),
        IfElse(hit, els),
    ] = nodes
    else {
        return None;
    };
    if !matches!(maybe_cast, [Nop(Op::RefCast(_))] | []) {
        return None;
    }
    if host_roles.get(eq_idx) != Some(&HostRole::StringEq) {
        return None;
    }
    let needle = VerbatimDefault::Array {
        type_idx: *type_idx,
        bytes: bytes.clone(),
    };
    let hit = verbatim_default_from_ops(&node_ops(hit))?;
    let (mut rest, default, rest_eq) = string_eq_verbatim_chain(els, host_roles)?;
    if rest_eq != 0 && rest_eq != *eq_idx {
        return None;
    }
    let mut arms = vec![(needle, hit)];
    arms.append(&mut rest);
    Some((arms, default, *eq_idx))
}

/// Parse `[localGet 0, refTest t, ifElse hit els]` where each `hit` is a verbatim
/// constant and `els` continues the chain or terminates in a verbatim constant.
/// Mirrors `dispatch_chain`, but every leaf is a byte-derived constant instead of
/// an Int projection.
fn verbatim_dispatch_chain(
    nodes: &[InstrNode],
) -> Option<(Vec<(u32, VerbatimDefault)>, VerbatimDefault)> {
    let [
        InstrNode::Op(Op::LocalGet(0)),
        InstrNode::Op(Op::RefTest(tag)),
        InstrNode::IfElse(hit, els),
    ] = nodes
    else {
        // Terminal else: a verbatim constant.
        return verbatim_default_from_ops(&node_ops(nodes)).map(|d| (Vec::new(), d));
    };
    if has_branch(hit) {
        return None;
    }
    let hit_const = verbatim_default_from_ops(&node_ops(hit))?;
    let (mut rest, default) = verbatim_dispatch_chain(els)?;
    rest.insert(0, (*tag, hit_const));
    Some((rest, default))
}
