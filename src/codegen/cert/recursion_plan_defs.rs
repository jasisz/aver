// Byte-first `recursion-plan-v1` plan builder.
//
// A fuel-recursion body reconstructs losslessly from the byte-derived
// `Cert::Recursive` / `Cert::AccumulatorRecursive` holes into the same ANF
// `FragBlock` grammar the expr-fragment plans use, plus the `selfCall` node.
// The plan lowers, byte-for-byte, to the emitted self-recursive code entry;
// it carries no source-level meaning and never changes the proof face — it
// only moves the recursive body's byte-origin into hash-pinned Lean.

/// ANF block builder: appends nodes with sequential ids.
struct RecBlockBuilder {
    nodes: Vec<FragNode>,
}

impl RecBlockBuilder {
    fn new() -> Self {
        RecBlockBuilder { nodes: Vec::new() }
    }

    fn push(&mut self, ty: FragTy, kind: FragNodeKind) -> FragValueId {
        let id = FragValueId(self.nodes.len());
        self.nodes.push(FragNode { id, ty, kind });
        id
    }

    fn finish(self, result: FragValueId) -> FragBlock {
        FragBlock {
            nodes: self.nodes,
            result,
        }
    }
}

/// The small-limb sign test `struct.get 0; i64.const 0; i64.le_s` (`n ≤ 0`).
fn rec_small_cmp_block() -> FragBlock {
    let mut b = RecBlockBuilder::new();
    let l = b.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 });
    let s = b.push(FragTy::I64, FragNodeKind::StructGet { field: 0, receiver: l });
    let z = b.push(FragTy::I64, FragNodeKind::ConstI64(0));
    let r = b.push(
        FragTy::BoolI32,
        FragNodeKind::Prim {
            op: FragPrim::I64LeS,
            args: vec![s, z],
        },
    );
    b.finish(r)
}

/// The big-limb sign test `struct.get 2; i32.const 0; i32.lt_s` (`sign < 0`).
fn rec_big_cmp_block() -> FragBlock {
    let mut b = RecBlockBuilder::new();
    let l = b.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 });
    let s = b.push(FragTy::RawI32, FragNodeKind::StructGet { field: 2, receiver: l });
    let z = b.push(FragTy::BoolI32, FragNodeKind::ConstBool(false));
    let r = b.push(
        FragTy::BoolI32,
        FragNodeKind::Prim {
            op: FragPrim::I32LtS,
            args: vec![s, z],
        },
    );
    b.finish(r)
}

/// The carrier discriminator `local.get 0; struct.get 1; ref.is_null` plus the
/// small-vs-big sign predicate `if` — shared by both recursion shapes.
fn rec_push_sign_predicate(top: &mut RecBlockBuilder) -> FragValueId {
    let l = top.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 });
    let mag = top.push(
        FragTy::Ref,
        FragNodeKind::StructGet {
            field: 1,
            receiver: l,
        },
    );
    let is_small = top.push(FragTy::BoolI32, FragNodeKind::RefIsNull { value: mag });
    top.push(
        FragTy::BoolI32,
        FragNodeKind::If {
            cond: is_small,
            then_block: Box::new(rec_small_cmp_block()),
            else_block: Box::new(rec_big_cmp_block()),
        },
    )
}

/// Materialise the descent operand `n - 1` and the self-call `f(n-1)`:
/// `local.get 0; i64.const 1; box; sub; call self`. Returns the self-call id.
fn rec_push_descent_self(
    b: &mut RecBlockBuilder,
    box_idx: u32,
    sub_idx: u32,
    self_idx: u32,
) -> FragValueId {
    let n = b.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 });
    let one = b.push(FragTy::I64, FragNodeKind::ConstI64(1));
    let boxed = b.push(
        FragTy::IntCarrier,
        FragNodeKind::HostCall {
            role: FragHostRole::Box,
            func_idx: box_idx,
            args: vec![one],
        },
    );
    let dec = b.push(
        FragTy::IntCarrier,
        FragNodeKind::HostCall {
            role: FragHostRole::Sub,
            func_idx: sub_idx,
            args: vec![n, boxed],
        },
    );
    b.push(
        FragTy::IntCarrier,
        FragNodeKind::SelfCall {
            tail: false,
            func_idx: self_idx,
            args: vec![dec],
        },
    )
}

/// Materialise the non-recursive combinator operand: the descending input `n`
/// (`local.get 0`) or a boxed integer constant (`i64.const k; box`).
fn rec_push_other(b: &mut RecBlockBuilder, other: BodyOperand, box_idx: u32) -> FragValueId {
    match other {
        BodyOperand::Input => b.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 }),
        BodyOperand::Const(k) => {
            let c = b.push(FragTy::I64, FragNodeKind::ConstI64(k));
            b.push(
                FragTy::IntCarrier,
                FragNodeKind::HostCall {
                    role: FragHostRole::Box,
                    func_idx: box_idx,
                    args: vec![c],
                },
            )
        }
    }
}

/// The base arm `i64.const base_k; box`.
fn rec_base_const_block(base_k: i64, box_idx: u32) -> FragBlock {
    let mut b = RecBlockBuilder::new();
    let k = b.push(FragTy::I64, FragNodeKind::ConstI64(base_k));
    let boxed = b.push(
        FragTy::IntCarrier,
        FragNodeKind::HostCall {
            role: FragHostRole::Box,
            func_idx: box_idx,
            args: vec![k],
        },
    );
    b.finish(boxed)
}

/// The step arm of single-argument recursion: descent + self-call combined with
/// the other operand by the (byte-role `Add`) combinator helper. `rec_first`
/// selects the operand order `f(n-1) + other` vs `other + f(n-1)`.
fn rec_step_block(
    box_idx: u32,
    add_idx: u32,
    sub_idx: u32,
    self_idx: u32,
    rec_first: bool,
    other: BodyOperand,
) -> FragBlock {
    let mut b = RecBlockBuilder::new();
    let comb = if rec_first {
        let self_id = rec_push_descent_self(&mut b, box_idx, sub_idx, self_idx);
        let other_id = rec_push_other(&mut b, other, box_idx);
        b.push(
            FragTy::IntCarrier,
            FragNodeKind::HostCall {
                role: FragHostRole::Add,
                func_idx: add_idx,
                args: vec![self_id, other_id],
            },
        )
    } else {
        let other_id = rec_push_other(&mut b, other, box_idx);
        let self_id = rec_push_descent_self(&mut b, box_idx, sub_idx, self_idx);
        b.push(
            FragTy::IntCarrier,
            FragNodeKind::HostCall {
                role: FragHostRole::Add,
                func_idx: add_idx,
                args: vec![other_id, self_id],
            },
        )
    };
    b.finish(comb)
}

/// Full plan for `Cert::Recursive` (single-argument fuel self-recursion).
fn recursion_plan_recursive(
    box_idx: u32,
    add_idx: u32,
    sub_idx: u32,
    self_idx: u32,
    base_k: i64,
    rec_first: bool,
    other: BodyOperand,
) -> ExprFragmentPlan {
    let mut top = RecBlockBuilder::new();
    let sign = rec_push_sign_predicate(&mut top);
    let value = top.push(
        FragTy::IntCarrier,
        FragNodeKind::If {
            cond: sign,
            then_block: Box::new(rec_base_const_block(base_k, box_idx)),
            else_block: Box::new(rec_step_block(
                box_idx, add_idx, sub_idx, self_idx, rec_first, other,
            )),
        },
    );
    ExprFragmentPlan {
        params: vec![FragTy::IntCarrier],
        result: FragTy::IntCarrier,
        body: top.finish(value),
    }
}

/// Full plan for `Cert::AccumulatorRecursive` (countDown-shape two-argument
/// tail accumulator: base returns the accumulator, step tail-calls
/// `f(n-1, acc+n)`).
fn recursion_plan_accumulator(
    box_idx: u32,
    add_idx: u32,
    sub_idx: u32,
    self_idx: u32,
) -> ExprFragmentPlan {
    // base arm: return the accumulator local.
    let mut base = RecBlockBuilder::new();
    let acc = base.push(FragTy::IntCarrier, FragNodeKind::Local { index: 1 });
    let base = base.finish(acc);
    // step arm: descent n-1, then acc+n, then tail-call f(n-1, acc+n).
    let mut step = RecBlockBuilder::new();
    let n = step.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 });
    let one = step.push(FragTy::I64, FragNodeKind::ConstI64(1));
    let boxed = step.push(
        FragTy::IntCarrier,
        FragNodeKind::HostCall {
            role: FragHostRole::Box,
            func_idx: box_idx,
            args: vec![one],
        },
    );
    let dec = step.push(
        FragTy::IntCarrier,
        FragNodeKind::HostCall {
            role: FragHostRole::Sub,
            func_idx: sub_idx,
            args: vec![n, boxed],
        },
    );
    let acc_operand = step.push(FragTy::IntCarrier, FragNodeKind::Local { index: 1 });
    let n_operand = step.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 });
    let acc_next = step.push(
        FragTy::IntCarrier,
        FragNodeKind::HostCall {
            role: FragHostRole::Add,
            func_idx: add_idx,
            args: vec![acc_operand, n_operand],
        },
    );
    let call = step.push(
        FragTy::IntCarrier,
        FragNodeKind::SelfCall {
            tail: true,
            func_idx: self_idx,
            args: vec![dec, acc_next],
        },
    );
    let step = step.finish(call);

    let mut top = RecBlockBuilder::new();
    let sign = rec_push_sign_predicate(&mut top);
    let value = top.push(
        FragTy::IntCarrier,
        FragNodeKind::If {
            cond: sign,
            then_block: Box::new(base),
            else_block: Box::new(step),
        },
    );
    ExprFragmentPlan {
        params: vec![FragTy::IntCarrier, FragTy::IntCarrier],
        result: FragTy::IntCarrier,
        body: top.finish(value),
    }
}

/// Build the byte-first `recursion-plan-v1` plan for a fuel-recursion cert.
/// Returns `None` for any other class, and — fail-closed — for a certified
/// recursion whose REAL code entry does not equal the canonical plan lowering:
/// the recognizer normalizes local-alias hops before classification, so a
/// legitimately certified body can be byte-noisier than the canonical
/// template. Such exports keep the legacy witness route (byte-derived
/// obligation, no plan claim); an artifact must never carry a byte-origin
/// claim its own bytes cannot prove.
fn recursion_plan_from_cert(c: &Cert) -> Option<ExprFragmentPlan> {
    let (plan, carrier, code_entry_bytes) = match c.inner() {
        Cert::Recursive {
            box_idx,
            add_idx,
            sub_idx,
            self_idx,
            base_k,
            rec_first,
            other,
            carrier,
            code_entry_bytes,
            ..
        } => (
            recursion_plan_recursive(
                *box_idx, *add_idx, *sub_idx, *self_idx, *base_k, *rec_first, *other,
            ),
            *carrier,
            code_entry_bytes,
        ),
        Cert::AccumulatorRecursive {
            box_idx,
            add_idx,
            sub_idx,
            self_idx,
            carrier,
            code_entry_bytes,
            ..
        } => (
            recursion_plan_accumulator(*box_idx, *add_idx, *sub_idx, *self_idx),
            *carrier,
            code_entry_bytes,
        ),
        _ => return None,
    };
    let lowered = lower_expr_fragment_plan_code_entry_bytes(&plan, carrier).ok()?;
    if &lowered != code_entry_bytes {
        return None;
    }
    Some(plan)
}

/// The per-export byte-derived host-role table a recursion claim carries: the
/// box helper, the combinator helper (byte-role `Add`; routed to the `mul`
/// contract slot when the model combinator is `*`), and the strict `sub`
/// helper, exactly as the cert's obligation wires them. Rendered identically
/// by producer and verifier so the artifact data pin stays byte-exact.
fn recursion_host_table_lean_value(box_idx: u32, add_idx: u32, sub_idx: u32) -> String {
    format!("[(.box, {box_idx}), (.add, {add_idx}), (.sub, {sub_idx})]")
}

/// The Lean `RecursionRawPlan` literal for a byte-first recursion plan (profile
/// `recursion-plan-v1`; reuses the shared block/node renderers).
fn recursion_plan_lean_value(plan: &ExprFragmentPlan) -> String {
    format!(
        "{{ profile := \"recursion-plan-v1\", params := [{}], result := {}, body := {} }}",
        plan.params
            .iter()
            .map(|ty| ty.lean_plan_ctor())
            .collect::<Vec<_>>()
            .join(", "),
        plan.result.lean_plan_ctor(),
        expr_fragment_block_lean_value(&plan.body)
    )
}

#[cfg(test)]
mod recursion_plan_gate_tests {
    use super::*;

    /// A canonical single-argument recursion cert whose carried code-entry
    /// bytes are exactly the canonical plan lowering (the honest case), plus a
    /// byte-noisy variant simulating a normalized body: the recognizer strips
    /// local-alias hops before classification, so a body with (say) an extra
    /// declared local classifies identically while its raw bytes differ.
    /// No current Aver source shape reaches that emission (there is no
    /// statement-level binding), so the noisy body is injected directly; the
    /// gate must fail-close it to the legacy route rather than emit a claim
    /// the artifact cannot prove.
    fn recursive_cert(code_entry_bytes: Vec<u8>) -> Cert {
        Cert::Recursive {
            name: "sumFrom".to_string(),
            self_idx: 1,
            code_idx: 1,
            type_idx: 4,
            nlocals: 1,
            carrier: 2,
            box_idx: 10,
            add_idx: 11,
            sub_idx: 12,
            base_k: 7,
            rec_first: false,
            other: BodyOperand::Input,
            combinator: Combinator::Add,
            code_entry_bytes,
        }
    }

    #[test]
    fn recursion_plan_requires_exact_code_entry_bytes() {
        let plan = recursion_plan_recursive(10, 11, 12, 1, 7, false, BodyOperand::Input);
        let canonical =
            lower_expr_fragment_plan_code_entry_bytes(&plan, 2).expect("canonical lowering");

        // Honest body: bytes equal the canonical lowering -> plan emitted.
        assert!(
            recursion_plan_from_cert(&recursive_cert(canonical.clone())).is_some(),
            "byte-exact recursion body must carry a plan claim"
        );

        // Normalized body: an extra (alias) local classifies identically but
        // its raw bytes differ -> NO plan claim; certification declines, fail-closed.
        let mut noisy = canonical.clone();
        assert_eq!(noisy[1..4], [0x01, 0x01, 0x63], "locals decl prefix moved");
        noisy[2] = 0x02; // declare two scratch locals instead of one
        noisy[0] += 1; // keep the size prefix consistent
        noisy.insert(4, 0x63); // second local group type byte placeholder
        assert!(
            recursion_plan_from_cert(&recursive_cert(noisy)).is_none(),
            "a body the canonical plan cannot reproduce must not carry a claim"
        );
    }
}
