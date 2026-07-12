// Byte-first `mutual-plan-v1` plan builder.
//
// One member of a mutually-recursive SCC reconstructs losslessly from the
// byte-derived `Cert::MutualRecursion` holes into the same ANF `FragBlock`
// grammar the recursion plan uses, generalising the `selfCall` node to a TAIL
// member-call whose target is a SIBLING member of the SCC. The plan lowers,
// byte-for-byte, to that member's emitted code entry inside the SCC's shared
// code table; it carries no source-level meaning and never changes the
// conjunction fuel-induction proof face — it only moves the member body's
// byte-origin into hash-pinned Lean. Reuses the recursion plan's shared sign
// predicate and base-arm block builders.

/// The mutual step arm `n - 1; g(n-1)`: `local.get 0; i64.const 1; box; sub;
/// return_call cross`. Identical to the recursion descent-self except the call
/// is a TAIL call to the SCC sibling `cross_idx`, not a non-tail self-call.
fn mut_step_block(box_idx: u32, sub_idx: u32, cross_idx: u32) -> FragBlock {
    let mut b = RecBlockBuilder::new();
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
    let call = b.push(
        FragTy::IntCarrier,
        FragNodeKind::SelfCall {
            tail: true,
            func_idx: cross_idx,
            args: vec![dec],
        },
    );
    b.finish(call)
}

/// Full plan for one mutual-recursion member `f n = if n≤0 then base else
/// g(n-1)` (a tail cross-call to the next SCC member). Arity 1.
fn mutual_member_plan(box_idx: u32, sub_idx: u32, base_k: i64, cross_idx: u32) -> ExprFragmentPlan {
    let mut top = RecBlockBuilder::new();
    let sign = rec_push_sign_predicate(&mut top);
    let value = top.push(
        FragTy::IntCarrier,
        FragNodeKind::If {
            cond: sign,
            then_block: Box::new(rec_base_const_block(base_k, box_idx)),
            else_block: Box::new(mut_step_block(box_idx, sub_idx, cross_idx)),
        },
    );
    ExprFragmentPlan {
        params: vec![FragTy::IntCarrier],
        result: FragTy::IntCarrier,
        body: top.finish(value),
    }
}

/// Build the byte-first `mutual-plan-v1` plan for THIS member of a mutual-
/// recursion cert (the member at `position` in the byte-derived SCC). Returns
/// `None` for any other class, and — fail-closed — for a certified member whose
/// REAL code entry does not equal the canonical plan lowering (a member body
/// byte-noisier than the canonical template stays on the legacy witness route;
/// an artifact must never carry a byte-origin claim its own bytes cannot prove).
fn mutual_plan_from_cert(c: &Cert) -> Option<ExprFragmentPlan> {
    let Cert::MutualRecursion {
        carrier,
        box_idx,
        sub_idx,
        position,
        scc,
        ..
    } = c.inner()
    else {
        return None;
    };
    let m = scc.get(*position)?;
    let plan = mutual_member_plan(*box_idx, *sub_idx, m.base_k, m.cross_idx);
    let lowered = lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier).ok()?;
    if lowered != m.code_entry_bytes {
        return None;
    }
    Some(plan)
}

/// The per-member byte-derived host-role table a mutual claim carries: the box
/// helper and the strict `sub` helper the SCC's shared host wires (no
/// combinator — mutual members never combine, they tail-call). Rendered
/// identically by producer and verifier so the artifact data pin stays exact.
fn mutual_host_table_lean_value(box_idx: u32, sub_idx: u32) -> String {
    format!("[(.box, {box_idx}), (.sub, {sub_idx})]")
}

/// The byte-derived SCC member-index set (each member's `self_idx`, in the
/// sorted SCC order) as the Lean `List Nat` literal the mutual claim threads as
/// the member-call binding context.
fn mutual_member_set_lean_value(scc: &[MutualMember]) -> String {
    format!(
        "[{}]",
        scc.iter()
            .map(|m| m.self_idx.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// The Lean `MutualRawPlan` literal for a byte-first mutual member plan (profile
/// `mutual-plan-v1`; reuses the shared block/node renderers).
fn mutual_plan_lean_value(plan: &ExprFragmentPlan) -> String {
    format!(
        "{{ profile := \"mutual-plan-v1\", params := [{}], result := {}, body := {} }}",
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
mod mutual_plan_gate_tests {
    use super::*;

    /// A canonical k=2 mutual cert (the `isEven`/`isOdd` shape from `mutual.av`:
    /// box=7, sub=9, carrier=2; isEven self 1 base 1 cross→2, isOdd self 2 base
    /// 0 cross→1) whose members carry either their exact canonical plan bytes
    /// (the honest case) or a byte-noisy variant, to prove the byte-equality
    /// gate fail-closes a member whose canonical plan cannot reproduce its bytes.
    fn mutual_cert(position: usize, member_bytes: Vec<Vec<u8>>) -> Cert {
        let scc = vec![
            MutualMember {
                name: "isEven".to_string(),
                self_idx: 1,
                code_idx: 1,
                type_idx: 4,
                nlocals: 1,
                base_k: 1,
                cross_idx: 2,
                code_entry_bytes: member_bytes[0].clone(),
            },
            MutualMember {
                name: "isOdd".to_string(),
                self_idx: 2,
                code_idx: 2,
                type_idx: 4,
                nlocals: 1,
                base_k: 0,
                cross_idx: 1,
                code_entry_bytes: member_bytes[1].clone(),
            },
        ];
        Cert::MutualRecursion {
            name: scc[position].name.clone(),
            self_idx: scc[position].self_idx,
            carrier: 2,
            box_idx: 7,
            sub_idx: 9,
            position,
            scc,
        }
    }

    #[test]
    fn mutual_plan_requires_exact_code_entry_bytes() {
        let even_plan = mutual_member_plan(7, 9, 1, 2);
        let odd_plan = mutual_member_plan(7, 9, 0, 1);
        let even_bytes =
            lower_expr_fragment_plan_code_entry_bytes(&even_plan, 2).expect("isEven lowering");
        let odd_bytes =
            lower_expr_fragment_plan_code_entry_bytes(&odd_plan, 2).expect("isOdd lowering");

        // Honest members: bytes equal the canonical lowering -> plan emitted.
        assert!(
            mutual_plan_from_cert(&mutual_cert(0, vec![even_bytes.clone(), odd_bytes.clone()]))
                .is_some(),
            "byte-exact isEven member must carry a plan claim"
        );
        assert!(
            mutual_plan_from_cert(&mutual_cert(1, vec![even_bytes.clone(), odd_bytes.clone()]))
                .is_some(),
            "byte-exact isOdd member must carry a plan claim"
        );

        // The canonical member bytes decode to the pinned mutual template: base
        // `42 <k> 10 07`, step `20 00 42 01 10 07 10 09 12 <cross>`.
        assert!(
            even_bytes.windows(2).any(|w| w == [0x12, 0x02]),
            "isEven step tail must be return_call isOdd (2): {even_bytes:02x?}"
        );
        assert!(
            odd_bytes.windows(2).any(|w| w == [0x12, 0x01]),
            "isOdd step tail must be return_call isEven (1): {odd_bytes:02x?}"
        );

        // Byte-noisy member: an extra scratch local classifies identically but
        // its raw bytes differ -> NO plan claim; certification declines, fail-closed.
        let mut noisy = even_bytes.clone();
        noisy.push(0x00);
        noisy[0] += 1;
        assert!(
            mutual_plan_from_cert(&mutual_cert(0, vec![noisy, odd_bytes])).is_none(),
            "a member the canonical plan cannot reproduce must not carry a claim"
        );
    }

    #[test]
    fn mutual_total_promotion_is_atomic_per_scc() {
        for position in 0..2 {
            let mut cert = mutual_cert(position, vec![Vec::new(), Vec::new()]);
            let Cert::MutualRecursion { scc, .. } = &mut cert else {
                unreachable!()
            };
            // One member no longer belongs to the closed descent cycle. Even
            // when inspecting the untouched sibling export, the shared SCC
            // fails eligibility and no per-member witness can be attached.
            scc[0].cross_idx = 99;
            assert_eq!(cert.termination_witness(), None);
            assert_eq!(cert.policy(), CertificationPolicy::SimulatesModel);
        }
    }
}
