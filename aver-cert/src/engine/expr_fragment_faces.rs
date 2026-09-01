// Recognised proof faces of expression-fragment plans. These are pure plan
// pattern-matchers (no byte analysis), shared between the producer's MIR
// adapter — which gates plan emission on a face existing — and the engine's
// classifier, so they live in the `plans` layer.

/// The straight-line integer face of a host-call expr fragment: exactly
/// `add(param0, box(k))` over one Int parameter. This is the only host-call
/// fragment shape with a rendered proof face today; any other host-call plan
/// fail-closes classification.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragIntAddFace {
    pub k: i64,
    pub box_idx: u32,
    pub add_idx: u32,
}

pub fn expr_fragment_int_add_face(plan: &ExprFragmentPlan) -> Option<FragIntAddFace> {
    if plan.params.as_slice() != [FragTy::IntCarrier] || plan.result != FragTy::IntCarrier {
        return None;
    }
    let [n0, n1, n2, n3] = plan.body.nodes.as_slice() else {
        return None;
    };
    if plan.body.result != FragValueId(3) {
        return None;
    }
    let (FragNodeKind::Local { index: 0 }, FragNodeKind::ConstI64(k)) = (&n0.kind, &n1.kind) else {
        return None;
    };
    let FragNodeKind::HostCall {
        role: FragHostRole::Box,
        func_idx: box_idx,
        args: box_args,
    } = &n2.kind
    else {
        return None;
    };
    let FragNodeKind::HostCall {
        role: FragHostRole::Add,
        func_idx: add_idx,
        args: add_args,
    } = &n3.kind
    else {
        return None;
    };
    if box_args.as_slice() != [FragValueId(1)]
        || add_args.as_slice() != [FragValueId(0), FragValueId(2)]
    {
        return None;
    }
    if n0.ty != FragTy::IntCarrier
        || n1.ty != FragTy::I64
        || n2.ty != FragTy::IntCarrier
        || n3.ty != FragTy::IntCarrier
    {
        return None;
    }
    Some(FragIntAddFace {
        k: *k,
        box_idx: *box_idx,
        add_idx: *add_idx,
    })
}

/// The verbatim field-projection face of an ADT-ref expr fragment: exactly
/// `struct.get ty field∈{0,1}` of the single reference parameter, returned
/// unchanged. This is the only fragment shape admitting `AdtRef` values today;
/// any other ADT-ref plan fail-closes on producer and verifier alike.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragProjectFace {
    pub struct_idx: u32,
    pub field_idx: u32,
}

pub fn expr_fragment_project_face(plan: &ExprFragmentPlan) -> Option<FragProjectFace> {
    if plan.params.as_slice() != [FragTy::AdtRef] || plan.result != FragTy::AdtRef {
        return None;
    }
    let [n0, n1] = plan.body.nodes.as_slice() else {
        return None;
    };
    if plan.body.result != FragValueId(1) {
        return None;
    }
    let FragNodeKind::Local { index: 0 } = n0.kind else {
        return None;
    };
    let FragNodeKind::StructGetUser {
        ty_idx,
        field,
        value,
    } = n1.kind
    else {
        return None;
    };
    if value != FragValueId(0) || field > 1 {
        return None;
    }
    if n0.ty != FragTy::AdtRef || n1.ty != FragTy::AdtRef {
        return None;
    }
    Some(FragProjectFace {
        struct_idx: ty_idx,
        field_idx: field,
    })
}

/// The stage-1 record field-read result types — exactly the range of the wall's
/// `SchemaCore.scalarLeafFragTy?` (`PlanCheck.fragTyIsRecordScalar`): the boxed
/// Int carrier, the Boolean i32, or the raw f64. Broadening this breaks the
/// producer/wall encode agreement (`encode… = some {name}Plan := rfl`).
pub fn frag_ty_is_record_scalar(ty: FragTy) -> bool {
    matches!(ty, FragTy::BoolI32 | FragTy::IntCarrier | FragTy::F64)
}

/// The scalar record-projection face of an ADT-ref expr fragment: exactly
/// `struct.get structIdx field` of the single opaque record reference, yielding
/// a stage-1 scalar leaf. Unlike `expr_fragment_project_face` the projected
/// result is a SCALAR (`frag_ty_is_record_scalar`) and the field index is NOT
/// capped — the wall record face's type-section equality pin fixes the whole
/// ordered field list. Exact Rust twin of `WasmSlice.exprRecordProjFace?`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragRecordProjFace {
    pub struct_idx: u32,
    pub field_idx: u32,
}

pub fn expr_fragment_record_proj_face(plan: &ExprFragmentPlan) -> Option<FragRecordProjFace> {
    if plan.params.as_slice() != [FragTy::AdtRef]
        || !frag_ty_is_record_scalar(plan.result)
        || plan.body.result != FragValueId(1)
    {
        return None;
    }
    let [n0, n1] = plan.body.nodes.as_slice() else {
        return None;
    };
    if n0.id != FragValueId(0) || n1.id != FragValueId(1) {
        return None;
    }
    let FragNodeKind::Local { index: 0 } = n0.kind else {
        return None;
    };
    let FragNodeKind::StructGetUser {
        ty_idx,
        field,
        value,
    } = n1.kind
    else {
        return None;
    };
    if value != FragValueId(0) || n0.ty != FragTy::AdtRef || n1.ty != plan.result {
        return None;
    }
    Some(FragRecordProjFace {
        struct_idx: ty_idx,
        field_idx: field,
    })
}

/// Exact Rust twin of `StandardFace.classifyTagDispatch` in the frozen wall.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragTagDispatchFace {
    pub opt_idx: u32,
    pub box_idx: u32,
    pub tag: i64,
    pub then_c: i64,
    pub else_c: i64,
}

fn expr_fragment_tag_dispatch_arm(block: &FragBlock) -> Option<(u32, i64)> {
    let [n0, n1] = block.nodes.as_slice() else {
        return None;
    };
    if block.result != FragValueId(1)
        || n0.id != FragValueId(0)
        || n1.id != FragValueId(1)
        || n0.ty != FragTy::I64
        || n1.ty != FragTy::IntCarrier
    {
        return None;
    }
    let FragNodeKind::ConstI64(constant) = n0.kind else {
        return None;
    };
    let FragNodeKind::HostCall {
        role: FragHostRole::Box,
        func_idx,
        ref args,
    } = n1.kind
    else {
        return None;
    };
    (args.as_slice() == [FragValueId(0)]).then_some((func_idx, constant))
}

pub fn expr_fragment_tag_dispatch_face(plan: &ExprFragmentPlan) -> Option<FragTagDispatchFace> {
    if plan.params.as_slice() != [FragTy::AdtRef]
        || plan.result != FragTy::IntCarrier
        || plan.body.result != FragValueId(4)
    {
        return None;
    }
    let [n0, n1, n2, n3, n4] = plan.body.nodes.as_slice() else {
        return None;
    };
    if [n0.id.0, n1.id.0, n2.id.0, n3.id.0, n4.id.0] != [0, 1, 2, 3, 4]
        || n0.ty != FragTy::AdtRef
        || n1.ty != FragTy::RawI32
        || n2.ty != FragTy::RawI32
        || n3.ty != FragTy::BoolI32
        || n4.ty != FragTy::IntCarrier
    {
        return None;
    }
    let FragNodeKind::Local { index: 0 } = n0.kind else {
        return None;
    };
    let FragNodeKind::StructGetUser {
        ty_idx: opt_idx,
        field: 0,
        value: FragValueId(0),
    } = n1.kind
    else {
        return None;
    };
    let FragNodeKind::ConstI32(tag) = n2.kind else {
        return None;
    };
    let FragNodeKind::Prim {
        op: FragPrim::I32Eq,
        ref args,
    } = n3.kind
    else {
        return None;
    };
    let FragNodeKind::If {
        cond: FragValueId(3),
        ref then_block,
        ref else_block,
    } = n4.kind
    else {
        return None;
    };
    if args.as_slice() != [FragValueId(1), FragValueId(2)] {
        return None;
    }
    let (box_idx, then_c) = expr_fragment_tag_dispatch_arm(then_block)?;
    let (else_box_idx, else_c) = expr_fragment_tag_dispatch_arm(else_block)?;
    (box_idx == else_box_idx).then_some(FragTagDispatchFace {
        opt_idx,
        box_idx,
        tag: i64::from(tag),
        then_c,
        else_c,
    })
}

/// Exact Rust twin of `StandardFace.classifyVectorGetOrDefault` in the wall:
/// the plan is the single monolithic fused vector-read node over the pinned
/// `(vector, index)` params, with distinct helper indices.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragVectorGetOrDefaultFace {
    pub arr_ty: u32,
    pub to_index_idx: u32,
    pub box_idx: u32,
    pub default: i64,
}

pub fn expr_fragment_vector_get_face(
    plan: &ExprFragmentPlan,
) -> Option<FragVectorGetOrDefaultFace> {
    if plan.params.as_slice() != [FragTy::AdtRef, FragTy::IntCarrier]
        || plan.result != FragTy::IntCarrier
        || plan.body.result != FragValueId(0)
    {
        return None;
    }
    let [n0] = plan.body.nodes.as_slice() else {
        return None;
    };
    if n0.id != FragValueId(0) || n0.ty != FragTy::IntCarrier {
        return None;
    }
    let FragNodeKind::VectorGetOrDefault {
        arr_ty,
        to_index_idx,
        box_idx,
        default,
    } = n0.kind
    else {
        return None;
    };
    (to_index_idx != box_idx).then_some(FragVectorGetOrDefaultFace {
        arr_ty,
        to_index_idx,
        box_idx,
        default,
    })
}

/// The comparison operator of an admitted Int value-versus-value face. Exact
/// Rust twin of the wall's `StandardFace.IntCmpOp`: `le` is absent by
/// construction, because the plan grammar has no `i32.le_s` to lower it to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FragIntCmpOp {
    Lt,
    Gt,
    Ge,
    Eq,
}

impl FragIntCmpOp {
    /// The wall constructor naming this operator (`StandardFace.IntCmpOp`).
    pub fn lean_ctor(self) -> &'static str {
        match self {
            FragIntCmpOp::Lt => ".lt",
            FragIntCmpOp::Gt => ".gt",
            FragIntCmpOp::Ge => ".ge",
            FragIntCmpOp::Eq => ".eq",
        }
    }
}

/// Face data of both Int comparison shapes: which operator, and the resolved
/// index of the single runtime helper it reads (`__aint_cmp` for the three
/// relational operators, `__aint_eq` for equality). Exact Rust twin of
/// `StandardFace.IntCmpFace`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragIntCmpFace {
    pub op: FragIntCmpOp,
    pub helper_idx: u32,
}

/// Twin of `StandardFace.intCmpOfPrim?`: the signed relational primitive each
/// operator's tail uses. Every other primitive declines, which keeps an
/// `i32.and`- or `i32.eq`-tailed body out of these faces.
fn frag_int_cmp_of_prim(op: FragPrim) -> Option<FragIntCmpOp> {
    match op {
        FragPrim::I32LtS => Some(FragIntCmpOp::Lt),
        FragPrim::I32GtS => Some(FragIntCmpOp::Gt),
        FragPrim::I32GeS => Some(FragIntCmpOp::Ge),
        _ => None,
    }
}

/// The pinned comparison prefix shared by both faces: `local 0`, `local 1`, and
/// either the `__aint_eq` call alone (3 nodes) or the `__aint_cmp` call plus
/// `i32.const 0` plus a signed relational operator (5 nodes). Returns the face
/// and the number of nodes the prefix occupies.
fn frag_int_cmp_prefix(nodes: &[FragNode]) -> Option<(FragIntCmpFace, usize)> {
    let [n0, n1, rest @ ..] = nodes else {
        return None;
    };
    if n0.id != FragValueId(0)
        || n1.id != FragValueId(1)
        || n0.ty != FragTy::IntCarrier
        || n1.ty != FragTy::IntCarrier
    {
        return None;
    }
    let (FragNodeKind::Local { index: 0 }, FragNodeKind::Local { index: 1 }) = (&n0.kind, &n1.kind)
    else {
        return None;
    };
    let [n2, tail @ ..] = rest else {
        return None;
    };
    if n2.id != FragValueId(2) {
        return None;
    }
    match &n2.kind {
        FragNodeKind::HostCall {
            role: FragHostRole::Eq,
            func_idx,
            args,
        } if n2.ty == FragTy::BoolI32
            && args.as_slice() == [FragValueId(0), FragValueId(1)] =>
        {
            Some((
                FragIntCmpFace {
                    op: FragIntCmpOp::Eq,
                    helper_idx: *func_idx,
                },
                3,
            ))
        }
        FragNodeKind::HostCall {
            role: FragHostRole::Cmp,
            func_idx,
            args,
        } if n2.ty == FragTy::RawI32
            && args.as_slice() == [FragValueId(0), FragValueId(1)] =>
        {
            let [n3, n4, ..] = tail else {
                return None;
            };
            if n3.id != FragValueId(3)
                || n4.id != FragValueId(4)
                || n3.ty != FragTy::RawI32
                || n4.ty != FragTy::BoolI32
            {
                return None;
            }
            let FragNodeKind::ConstI32(0) = n3.kind else {
                return None;
            };
            let FragNodeKind::Prim { op, args } = &n4.kind else {
                return None;
            };
            if args.as_slice() != [FragValueId(2), FragValueId(3)] {
                return None;
            }
            Some((
                FragIntCmpFace {
                    op: frag_int_cmp_of_prim(*op)?,
                    helper_idx: *func_idx,
                },
                5,
            ))
        }
        _ => None,
    }
}

/// Exact Rust twin of `StandardFace.classifyIntCmpBool`: two Int-carrier
/// parameters, a Boolean result, and exactly the pinned comparison nodes.
pub fn expr_fragment_int_cmp_bool_face(plan: &ExprFragmentPlan) -> Option<FragIntCmpFace> {
    if plan.params.as_slice() != [FragTy::IntCarrier, FragTy::IntCarrier]
        || plan.result != FragTy::BoolI32
    {
        return None;
    }
    let (face, len) = frag_int_cmp_prefix(&plan.body.nodes)?;
    (plan.body.nodes.len() == len && plan.body.result == FragValueId(len - 1)).then_some(face)
}

/// One arm of the selection: a bare argument read, no box and no host call.
/// Twin of `StandardFace.intSelectArm`.
fn frag_int_select_arm(block: &FragBlock, local: u32) -> bool {
    let [node] = block.nodes.as_slice() else {
        return false;
    };
    block.result == FragValueId(0)
        && node.id == FragValueId(0)
        && node.ty == FragTy::IntCarrier
        && node.kind == FragNodeKind::Local { index: local }
}

/// Exact Rust twin of `StandardFace.classifyIntSelect`: the comparison above
/// followed by an `if` whose two arms are the bare reads of parameter 0 and
/// parameter 1 in that order — so the result is a passthrough of an input, never
/// a freshly boxed value.
pub fn expr_fragment_int_select_face(plan: &ExprFragmentPlan) -> Option<FragIntCmpFace> {
    if plan.params.as_slice() != [FragTy::IntCarrier, FragTy::IntCarrier]
        || plan.result != FragTy::IntCarrier
    {
        return None;
    }
    let (face, len) = frag_int_cmp_prefix(&plan.body.nodes)?;
    if plan.body.nodes.len() != len + 1 || plan.body.result != FragValueId(len) {
        return None;
    }
    let node = &plan.body.nodes[len];
    if node.id != FragValueId(len) || node.ty != FragTy::IntCarrier {
        return None;
    }
    let FragNodeKind::If {
        cond,
        then_block,
        else_block,
    } = &node.kind
    else {
        return None;
    };
    (*cond == FragValueId(len - 1)
        && frag_int_select_arm(then_block, 0)
        && frag_int_select_arm(else_block, 1))
    .then_some(face)
}

fn frag_block_has_user_struct_get(block: &FragBlock) -> bool {
    block.nodes.iter().any(|node| match &node.kind {
        FragNodeKind::StructGetUser { .. } => true,
        FragNodeKind::If {
            then_block,
            else_block,
            ..
        } => {
            frag_block_has_user_struct_get(then_block)
                || frag_block_has_user_struct_get(else_block)
        }
        _ => false,
    })
}

/// Rust twin of the wall's broad `exprFragmentIsTagDispatch` discriminator:
/// an ADT-ref argument, an Int-carrier result, and at least one user-struct
/// field read in the encoded plan. The wall's classifier later checks the
/// exact canonical tag-dispatch node shape.
pub fn expr_fragment_is_tag_dispatch(plan: &ExprFragmentPlan) -> bool {
    plan.params.as_slice() == [FragTy::AdtRef]
        && plan.result == FragTy::IntCarrier
        && frag_block_has_user_struct_get(&plan.body)
}

fn frag_block_touches_adt_ref(block: &FragBlock) -> bool {
    block.nodes.iter().any(|node| {
        node.ty == FragTy::AdtRef
            || match &node.kind {
                FragNodeKind::StructGetUser { .. } => true,
                FragNodeKind::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    frag_block_touches_adt_ref(then_block) || frag_block_touches_adt_ref(else_block)
                }
                _ => false,
            }
    })
}

/// Whether a plan involves opaque user-ADT references anywhere (params, result
/// or body). Such plans are admitted ONLY through the field-projection face.
pub fn expr_fragment_plan_touches_adt_ref(plan: &ExprFragmentPlan) -> bool {
    plan.params.contains(&FragTy::AdtRef)
        || plan.result == FragTy::AdtRef
        || frag_block_touches_adt_ref(&plan.body)
}

fn frag_block_has_host_call_where(block: &FragBlock, want: &dyn Fn(FragHostRole) -> bool) -> bool {
    block.nodes.iter().any(|node| match &node.kind {
        FragNodeKind::HostCall { role, .. } => want(*role),
        FragNodeKind::If {
            then_block,
            else_block,
            ..
        } => {
            frag_block_has_host_call_where(then_block, want)
                || frag_block_has_host_call_where(else_block, want)
        }
        _ => false,
    })
}

/// Whether a plan calls a runtime host helper anywhere in its body. Such plans
/// are admitted ONLY through an exact recognised face: a host call is a claim
/// about a runtime contract, and the generic expression-fragment gate in the
/// wall (`genericFragmentAllowedFuel`) rejects every `.hostCall` node outright.
/// Producer and verifier read this same predicate so the two gates cannot drift.
pub fn expr_fragment_plan_has_host_calls(plan: &ExprFragmentPlan) -> bool {
    frag_block_has_host_call_where(&plan.body, &|_| true)
}

/// Whether a plan calls one PARTICULAR host role. The producer reads this to
/// decide whether the module it is about to emit really calls a helper, which
/// is what gates the helper's named export (and with it the role's certificate
/// binding) — a plan is the exact body its canonical lowering emits, so the
/// roles it names are the calls the bytes will carry.
pub fn expr_fragment_plan_calls_host_role(plan: &ExprFragmentPlan, role: FragHostRole) -> bool {
    frag_block_has_host_call_where(&plan.body, &|candidate| candidate == role)
}

#[cfg(all(test, feature = "engine"))]
mod record_proj_face_tests {
    use super::*;

    /// A record scalar field read: `struct.get struct_idx field` of the single
    /// record reference, yielding `result`. This is the shape person's
    /// `readMember`/`readAge` compile to.
    fn record_proj_plan(struct_idx: u32, field: u32, result: FragTy) -> ExprFragmentPlan {
        ExprFragmentPlan {
            params: vec![FragTy::AdtRef],
            result,
            body: FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::Local { index: 0 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: result,
                        kind: FragNodeKind::StructGetUser {
                            ty_idx: struct_idx,
                            field,
                            value: FragValueId(0),
                        },
                    },
                ],
                result: FragValueId(1),
            },
        }
    }

    #[test]
    fn frag_ty_is_record_scalar_is_exactly_the_three_leaves() {
        assert!(frag_ty_is_record_scalar(FragTy::BoolI32));
        assert!(frag_ty_is_record_scalar(FragTy::IntCarrier));
        assert!(frag_ty_is_record_scalar(FragTy::F64));
        for ty in [FragTy::I64, FragTy::RawI32, FragTy::Ref, FragTy::AdtRef] {
            assert!(!frag_ty_is_record_scalar(ty), "{ty:?} must not be a record leaf");
        }
    }

    #[test]
    fn recognizes_person_field_reads() {
        // `readMember`: Bool field 1 of struct 0. `readAge`: Int field 0.
        assert_eq!(
            expr_fragment_record_proj_face(&record_proj_plan(0, 1, FragTy::BoolI32)),
            Some(FragRecordProjFace { struct_idx: 0, field_idx: 1 })
        );
        assert_eq!(
            expr_fragment_record_proj_face(&record_proj_plan(0, 0, FragTy::IntCarrier)),
            Some(FragRecordProjFace { struct_idx: 0, field_idx: 0 })
        );
        // The field index is not capped (unlike the verbatim projection face).
        assert_eq!(
            expr_fragment_record_proj_face(&record_proj_plan(7, 5, FragTy::F64)),
            Some(FragRecordProjFace { struct_idx: 7, field_idx: 5 })
        );
    }

    #[test]
    fn declines_whole_reference_projection() {
        // An `AdtRef` result is the verbatim field-projection face, NOT a record
        // scalar leaf — it must decline here (and route to `expr_fragment_project_face`).
        assert_eq!(
            expr_fragment_record_proj_face(&record_proj_plan(0, 0, FragTy::AdtRef)),
            None
        );
    }

    #[test]
    fn declines_non_scalar_leaf_result() {
        for ty in [FragTy::I64, FragTy::RawI32, FragTy::Ref] {
            assert_eq!(expr_fragment_record_proj_face(&record_proj_plan(0, 0, ty)), None);
        }
    }

    #[test]
    fn declines_wrong_parameter_shape() {
        // Not a single opaque record reference.
        let mut plan = record_proj_plan(0, 1, FragTy::BoolI32);
        plan.params = vec![FragTy::IntCarrier];
        assert_eq!(expr_fragment_record_proj_face(&plan), None);
        let mut plan = record_proj_plan(0, 1, FragTy::BoolI32);
        plan.params = vec![FragTy::AdtRef, FragTy::AdtRef];
        assert_eq!(expr_fragment_record_proj_face(&plan), None);
    }

    #[test]
    fn declines_node_type_mismatch_and_wrong_body_shape() {
        // The projection node's declared type must equal the plan result.
        let mut plan = record_proj_plan(0, 1, FragTy::BoolI32);
        plan.body.nodes[1].ty = FragTy::IntCarrier;
        assert_eq!(expr_fragment_record_proj_face(&plan), None);

        // The projected value must be the single parameter local, not a re-read.
        let mut plan = record_proj_plan(0, 1, FragTy::BoolI32);
        plan.body.nodes[1].kind = FragNodeKind::StructGetUser {
            ty_idx: 0,
            field: 1,
            value: FragValueId(1),
        };
        assert_eq!(expr_fragment_record_proj_face(&plan), None);

        // An extra node breaks the exact two-node shape.
        let mut plan = record_proj_plan(0, 1, FragTy::BoolI32);
        plan.body.nodes.push(FragNode {
            id: FragValueId(2),
            ty: FragTy::BoolI32,
            kind: FragNodeKind::ConstBool(true),
        });
        assert_eq!(expr_fragment_record_proj_face(&plan), None);

        // The first node must read parameter local 0.
        let mut plan = record_proj_plan(0, 1, FragTy::BoolI32);
        plan.body.nodes[0].kind = FragNodeKind::Local { index: 1 };
        assert_eq!(expr_fragment_record_proj_face(&plan), None);
    }
}


/// The record projection-compute face: k opaque record parameters of ONE
/// pinned struct type, a body over the v1 compute node set (projections,
/// construction, box/add/sub/mul/eq host calls, i64 literals), and a
/// record/Int/Bool result. Twin of `StandardFace.classifyRecordCompute`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FragRecordComputeFace {
    pub struct_idx: u32,
}

fn record_compute_node_ok(host_table: &FragHostTable, kind: &FragNodeKind) -> bool {
    match kind {
        FragNodeKind::Local { .. }
        | FragNodeKind::ConstI64(_)
        | FragNodeKind::StructGetUser { .. }
        | FragNodeKind::StructNew { .. } => true,
        FragNodeKind::HostCall {
            role,
            func_idx,
            args,
        } => {
            host_table.lookup(*role) == Some(*func_idx)
                && match role {
                    FragHostRole::Box => args.len() == 1,
                    // `eq` is deliberately OUT of v1: the wall's `_hEq`
                    // contract is small-band, the bridge's equality is not.
                    FragHostRole::Add
                    | FragHostRole::Sub
                    | FragHostRole::Mul => args.len() == 2,
                    _ => false,
                }
        }
        _ => false,
    }
}

fn frag_node_struct_idx(kind: &FragNodeKind) -> Option<u32> {
    match kind {
        FragNodeKind::StructGetUser { ty_idx, .. } => Some(*ty_idx),
        FragNodeKind::StructNew { ty_idx, .. } => Some(*ty_idx),
        _ => None,
    }
}

/// Twin of the wall's `classifyRecordCompute`: fires only when every
/// parameter is an opaque record reference, every node is in the admitted
/// set with host calls citing the byte-derived role table, at least one node
/// computes (rules the two-node projection faces out), the result is a
/// record/Int/Bool, and every cited user-struct index agrees.
pub fn expr_fragment_record_compute_face(
    plan: &ExprFragmentPlan,
    host_table: &FragHostTable,
) -> Option<FragRecordComputeFace> {
    if !plan.params.iter().all(|ty| *ty == FragTy::AdtRef) {
        return None;
    }
    if !plan
        .body
        .nodes
        .iter()
        .all(|n| record_compute_node_ok(host_table, &n.kind))
    {
        return None;
    }
    if !plan.body.nodes.iter().any(|n| {
        matches!(
            n.kind,
            FragNodeKind::StructNew { .. } | FragNodeKind::HostCall { .. }
        )
    }) {
        return None;
    }
    if !matches!(
        plan.result,
        FragTy::AdtRef | FragTy::IntCarrier | FragTy::BoolI32
    ) {
        return None;
    }
    let mut idxs = plan
        .body
        .nodes
        .iter()
        .filter_map(|n| frag_node_struct_idx(&n.kind));
    let first = idxs.next()?;
    if idxs.all(|i| i == first) {
        Some(FragRecordComputeFace { struct_idx: first })
    } else {
        None
    }
}
