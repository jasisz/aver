// Byte-first `verbatim-plan-v1` plan builder.
//
// A verbatim `ref.test`-dispatch body (the ADT-match `Cod := WVal` shapes:
// `Cert::VerbatimWidenedMatch` / `Cert::VerbatimVariantDispatch`) reconstructs
// losslessly from the byte-derived cert holes into a DEDICATED grammar (NOT the
// ANF `FragBlock`: the multi-use scrutinee is spilled to a scratch local, which
// pure ANF cannot express). The plan lowers, byte-for-byte, to the emitted code
// entry; it carries no source-level meaning and never changes the
// `verbatimRepr` proof face — it only moves the match body's byte-origin into
// hash-pinned Lean. There are no host/self calls to bind, so the byte-equality
// gate IS the whole soundness binding; the plan is otherwise only structural.

/// One terminal leaf of a verbatim dispatch arm. Rust twin of Lean
/// `Schema.VerbatimLeaf`.
#[derive(Clone, PartialEq)]
enum VerbatimLeaf {
    /// Project field `field` of the scrutinee cast to user struct type `ty_idx`.
    Project { ty_idx: u32, field: u32 },
    /// A String literal built by `array.new_data arr_ty data_idx` over `bytes`.
    ArrayNewData {
        arr_ty: u32,
        data_idx: u32,
        bytes: Vec<u8>,
    },
    /// The null reference default (`ref.null result_heap_ty`).
    RefNull,
    /// A float-bits constant (`f64.const bits`).
    F64Bits(u64),
}

/// A right-nested `ref.test` dispatch cascade. Rust twin of Lean
/// `Schema.VerbatimDispatch`.
#[derive(Clone, PartialEq)]
enum VerbatimDispatch {
    Leaf(VerbatimLeaf),
    Test {
        ty_idx: u32,
        hit: VerbatimLeaf,
        rest: Box<VerbatimDispatch>,
    },
}

/// Raw, untrusted verbatim `ref.test`-dispatch plan (`verbatim-plan-v1`). Rust
/// twin of Lean `Schema.VerbatimRawPlan` (the `profile` field is hard-coded by
/// the Lean renderer, so it is not carried here).
#[derive(Clone, PartialEq)]
struct VerbatimRawPlan {
    scrutinee_local: u32,
    field_local: u32,
    result_heap_ty: u32,
    body: VerbatimDispatch,
}

fn verbatim_leaf_has_projection(l: &VerbatimLeaf) -> bool {
    matches!(l, VerbatimLeaf::Project { .. })
}

fn verbatim_dispatch_has_projection(d: &VerbatimDispatch) -> bool {
    match d {
        VerbatimDispatch::Leaf(l) => verbatim_leaf_has_projection(l),
        VerbatimDispatch::Test { hit, rest, .. } => {
            verbatim_leaf_has_projection(hit) || verbatim_dispatch_has_projection(rest)
        }
    }
}

/// The dispatch leaf a byte-derived `VerbatimDefault` constant lowers to.
fn verbatim_leaf_from_default(d: &VerbatimDefault) -> VerbatimLeaf {
    match d {
        VerbatimDefault::Null => VerbatimLeaf::RefNull,
        VerbatimDefault::F64Bits(bits) => VerbatimLeaf::F64Bits(*bits),
        VerbatimDefault::Array {
            type_idx,
            data_idx,
            bytes,
        } => VerbatimLeaf::ArrayNewData {
            arr_ty: *type_idx,
            data_idx: *data_idx,
            bytes: bytes.clone(),
        },
    }
}

/// The dispatch's `if (result <heap>)` block type / `ref.null` heap type, read
/// from the byte-derived default: an `array.new_data` default names it directly
/// through its array type index; a null default names it through the threaded
/// `ref.null` heap type in the body ops. An `f64` default has no ref result
/// type, so it declines (fail-closed; the grammar's block result is always a
/// ref). The byte-equality gate re-checks whatever this returns.
fn verbatim_result_heap_ty(default: &VerbatimDefault, ops: &[Op]) -> Option<u32> {
    match default {
        VerbatimDefault::Array { type_idx, .. } => Some(*type_idx),
        VerbatimDefault::Null => ops.iter().find_map(|op| match op {
            Op::RefNull(Some(h)) => Some(*h),
            _ => None,
        }),
        VerbatimDefault::F64Bits(_) => None,
    }
}

/// Build the byte-first `verbatim-plan-v1` plan for a verbatim widened-match or
/// variant-dispatch cert. Returns `None` for any other class, and — fail-closed
/// — for a certified body whose REAL code entry does not equal the canonical
/// plan lowering (a body byte-noisier than the canonical template stays on the
/// legacy witness route; an artifact must never carry a byte-origin claim its
/// own bytes cannot prove). The scrutinee/field scratch locals are fixed by the
/// ADT-match lowering layout (projecting => S=2, F=1; non-projecting => S=1),
/// exactly what the Lean locals encoder assumes.
fn verbatim_plan_from_cert(c: &Cert) -> Option<VerbatimRawPlan> {
    let (plan, carrier, code_entry_bytes) = match c.inner() {
        Cert::VerbatimWidenedMatch {
            hit_variant_idx,
            default,
            carrier,
            code_entry_bytes,
            ops,
            ..
        } => {
            let result_heap_ty = verbatim_result_heap_ty(default, ops)?;
            let body = VerbatimDispatch::Test {
                ty_idx: *hit_variant_idx,
                hit: VerbatimLeaf::Project {
                    ty_idx: *hit_variant_idx,
                    field: 0,
                },
                rest: Box::new(VerbatimDispatch::Leaf(verbatim_leaf_from_default(default))),
            };
            let plan = VerbatimRawPlan {
                scrutinee_local: 2,
                field_local: 1,
                result_heap_ty,
                body,
            };
            (plan, *carrier, code_entry_bytes)
        }
        Cert::VerbatimVariantDispatch {
            arms,
            default,
            carrier,
            code_entry_bytes,
            ops,
            ..
        } => {
            let result_heap_ty = verbatim_result_heap_ty(default, ops)?;
            let mut body = VerbatimDispatch::Leaf(verbatim_leaf_from_default(default));
            for (tag, konst) in arms.iter().rev() {
                body = VerbatimDispatch::Test {
                    ty_idx: *tag,
                    hit: verbatim_leaf_from_default(konst),
                    rest: Box::new(body),
                };
            }
            let plan = VerbatimRawPlan {
                scrutinee_local: 1,
                field_local: 0,
                result_heap_ty,
                body,
            };
            (plan, *carrier, code_entry_bytes)
        }
        _ => return None,
    };
    let lowered = lower_verbatim_code_entry(&plan, carrier);
    if &lowered != code_entry_bytes {
        return None;
    }
    Some(plan)
}

/// Exact code-entry bytes of a verbatim plan. Twin of Lean
/// `PlanBytes.lowerVerbatimCodeEntry` (heap indices s33-signed;
/// struct.get/array.new_data type+field+data indices uleb32).
fn lower_verbatim_code_entry(plan: &VerbatimRawPlan, carrier: u32) -> Vec<u8> {
    let body = lower_verbatim_body_bytes(plan, carrier);
    let mut out = Vec::new();
    push_u32_leb(&mut out, body.len() as u32);
    out.extend_from_slice(&body);
    out
}

fn lower_verbatim_body_bytes(plan: &VerbatimRawPlan, carrier: u32) -> Vec<u8> {
    let mut out = Vec::new();
    // Local declarations.
    if verbatim_dispatch_has_projection(&plan.body) {
        out.extend_from_slice(&[0x03, 0x01, 0x63]);
        push_s33_heap_idx(&mut out, plan.result_heap_ty);
        out.extend_from_slice(&[0x01, 0x6d, 0x01, 0x63]);
        push_s33_heap_idx(&mut out, carrier);
    } else {
        out.extend_from_slice(&[0x02, 0x01, 0x6d, 0x01, 0x63]);
        push_s33_heap_idx(&mut out, carrier);
    }
    // Expression: spill the scrutinee, then the dispatch cascade.
    out.extend_from_slice(&[0x20, 0x00, 0x21]);
    push_u32_leb(&mut out, plan.scrutinee_local);
    out.push(0x20);
    push_u32_leb(&mut out, plan.scrutinee_local);
    verbatim_dispatch_bytes(
        &mut out,
        plan.scrutinee_local,
        plan.field_local,
        plan.result_heap_ty,
        true,
        &plan.body,
    );
    out.push(0x0b);
    out
}

fn verbatim_dispatch_bytes(
    out: &mut Vec<u8>,
    s: u32,
    f: u32,
    rht: u32,
    first: bool,
    disp: &VerbatimDispatch,
) {
    match disp {
        VerbatimDispatch::Leaf(l) => verbatim_leaf_bytes(out, s, f, rht, l),
        VerbatimDispatch::Test { ty_idx, hit, rest } => {
            if !first {
                out.push(0x20);
                push_u32_leb(out, s);
            }
            out.extend_from_slice(&[0xfb, 0x14]);
            push_s33_heap_idx(out, *ty_idx);
            out.extend_from_slice(&[0x04, 0x63]);
            push_s33_heap_idx(out, rht);
            verbatim_leaf_bytes(out, s, f, rht, hit);
            out.push(0x05);
            verbatim_dispatch_bytes(out, s, f, rht, false, rest);
            out.push(0x0b);
        }
    }
}

fn verbatim_leaf_bytes(out: &mut Vec<u8>, s: u32, f: u32, rht: u32, leaf: &VerbatimLeaf) {
    match leaf {
        VerbatimLeaf::Project { ty_idx, field } => {
            out.push(0x20);
            push_u32_leb(out, s);
            out.extend_from_slice(&[0xfb, 0x16]);
            push_s33_heap_idx(out, *ty_idx);
            out.extend_from_slice(&[0xfb, 0x02]);
            push_u32_leb(out, *ty_idx);
            push_u32_leb(out, *field);
            out.push(0x21);
            push_u32_leb(out, f);
            out.push(0x20);
            push_u32_leb(out, f);
        }
        VerbatimLeaf::ArrayNewData {
            arr_ty,
            data_idx,
            bytes,
        } => {
            out.push(0x41);
            push_i32_leb(out, 0);
            out.push(0x41);
            push_i32_leb(out, bytes.len() as i32);
            out.extend_from_slice(&[0xfb, 0x09]);
            push_u32_leb(out, *arr_ty);
            push_u32_leb(out, *data_idx);
        }
        VerbatimLeaf::RefNull => {
            out.push(0xd0);
            push_s33_heap_idx(out, rht);
        }
        VerbatimLeaf::F64Bits(bits) => {
            out.push(0x44);
            out.extend_from_slice(&bits.to_le_bytes());
        }
    }
}

/// The Lean `VerbatimLeaf` literal.
fn verbatim_leaf_lean_value(l: &VerbatimLeaf) -> String {
    match l {
        VerbatimLeaf::Project { ty_idx, field } => format!(".project {ty_idx} {field}"),
        VerbatimLeaf::ArrayNewData {
            arr_ty,
            data_idx,
            bytes,
        } => format!(".arrayNewData {arr_ty} {data_idx} {}", render_byte_list(bytes)),
        VerbatimLeaf::RefNull => ".refNull".to_string(),
        VerbatimLeaf::F64Bits(bits) => format!(".f64Bits {bits}"),
    }
}

/// The Lean `VerbatimDispatch` literal.
fn verbatim_dispatch_lean_value(d: &VerbatimDispatch) -> String {
    match d {
        VerbatimDispatch::Leaf(l) => format!(".leaf ({})", verbatim_leaf_lean_value(l)),
        VerbatimDispatch::Test { ty_idx, hit, rest } => format!(
            ".test {ty_idx} ({}) ({})",
            verbatim_leaf_lean_value(hit),
            verbatim_dispatch_lean_value(rest)
        ),
    }
}

/// The Lean `VerbatimRawPlan` literal (profile `verbatim-plan-v1`), rendered on
/// ONE line (a multi-line anonymous-constructor literal misparses).
fn verbatim_plan_lean_value(plan: &VerbatimRawPlan) -> String {
    format!(
        "{{ profile := \"verbatim-plan-v1\", scrutineeLocal := {}, fieldLocal := {}, resultHeapTy := {}, body := {} }}",
        plan.scrutinee_local,
        plan.field_local,
        plan.result_heap_ty,
        verbatim_dispatch_lean_value(&plan.body)
    )
}

#[cfg(test)]
mod verbatim_plan_gate_tests {
    use super::*;

    /// The `wrapItems` cert (verbatim widened match, func 13): tests struct
    /// type 6, projects field 0, defaults to `ref.null 20`. carrier 18.
    fn wrap_items_cert(code_entry_bytes: Vec<u8>) -> Cert {
        Cert::VerbatimWidenedMatch {
            name: "wrapItems".to_string(),
            self_idx: 13,
            nlocals: 3,
            carrier: 18,
            hit_variant_idx: 6,
            default: VerbatimDefault::Null,
            code_entry_bytes,
            ops: vec![
                Op::LocalGet(0),
                Op::LocalSet(2),
                Op::LocalGet(2),
                Op::RefTest(6),
                Op::If,
                Op::LocalGet(2),
                Op::RefCast(6),
                Op::StructGet(6, 0),
                Op::LocalSet(1),
                Op::LocalGet(1),
                Op::Else,
                Op::RefNull(Some(20)),
                Op::End,
            ],
        }
    }

    /// The `tagName` cert (verbatim variant dispatch, func 14): tests struct
    /// types 8, 9 -> String literals in array type 16, else -> "gamma".
    fn tag_name_cert(code_entry_bytes: Vec<u8>) -> Cert {
        let arr = |data_idx: u32, bytes: &[u8]| VerbatimDefault::Array {
            type_idx: 16,
            data_idx,
            bytes: bytes.to_vec(),
        };
        Cert::VerbatimVariantDispatch {
            name: "tagName".to_string(),
            self_idx: 14,
            nlocals: 2,
            carrier: 18,
            arms: vec![
                (8, arr(0, &[97, 108, 112, 104, 97])),
                (9, arr(1, &[98, 101, 116, 97])),
            ],
            default: arr(2, &[103, 97, 109, 109, 97]),
            code_entry_bytes,
            ops: vec![Op::LocalGet(0), Op::LocalSet(1), Op::LocalGet(1)],
        }
    }

    #[test]
    fn verbatim_plan_reproduces_stage0_pins() {
        // wrapItems: the pinned code entry (size 0x27) from the spike.
        let wrap_plan = verbatim_plan_from_cert(&wrap_items_cert(Vec::new()));
        assert!(
            wrap_plan.is_none(),
            "empty code entry cannot equal the canonical lowering"
        );
        let wrap_plan = VerbatimRawPlan {
            scrutinee_local: 2,
            field_local: 1,
            result_heap_ty: 20,
            body: VerbatimDispatch::Test {
                ty_idx: 6,
                hit: VerbatimLeaf::Project {
                    ty_idx: 6,
                    field: 0,
                },
                rest: Box::new(VerbatimDispatch::Leaf(VerbatimLeaf::RefNull)),
            },
        };
        let wrap_bytes = lower_verbatim_code_entry(&wrap_plan, 18);
        // size prefix + locals `03 01 63 14 01 6d 01 63 12` + body.
        assert_eq!(wrap_bytes[0] as usize, wrap_bytes.len() - 1);
        assert_eq!(
            &wrap_bytes[1..10],
            &[0x03, 0x01, 0x63, 0x14, 0x01, 0x6d, 0x01, 0x63, 0x12],
            "wrapItems locals decl"
        );
        assert!(
            verbatim_plan_from_cert(&wrap_items_cert(wrap_bytes.clone())).is_some(),
            "byte-exact wrapItems must carry a plan claim"
        );
        // Byte-noisy body: an extra byte -> no claim, legacy route, fail-closed.
        let mut noisy = wrap_bytes.clone();
        noisy.push(0x00);
        noisy[0] += 1;
        assert!(
            verbatim_plan_from_cert(&wrap_items_cert(noisy)).is_none(),
            "a body the canonical plan cannot reproduce must not carry a claim"
        );

        // tagName: three String literals, right-nested cascade.
        let tag_plan = VerbatimRawPlan {
            scrutinee_local: 1,
            field_local: 0,
            result_heap_ty: 16,
            body: VerbatimDispatch::Test {
                ty_idx: 8,
                hit: VerbatimLeaf::ArrayNewData {
                    arr_ty: 16,
                    data_idx: 0,
                    bytes: vec![97, 108, 112, 104, 97],
                },
                rest: Box::new(VerbatimDispatch::Test {
                    ty_idx: 9,
                    hit: VerbatimLeaf::ArrayNewData {
                        arr_ty: 16,
                        data_idx: 1,
                        bytes: vec![98, 101, 116, 97],
                    },
                    rest: Box::new(VerbatimDispatch::Leaf(VerbatimLeaf::ArrayNewData {
                        arr_ty: 16,
                        data_idx: 2,
                        bytes: vec![103, 97, 109, 109, 97],
                    })),
                }),
            },
        };
        let tag_bytes = lower_verbatim_code_entry(&tag_plan, 18);
        assert_eq!(
            &tag_bytes[1..7],
            &[0x02, 0x01, 0x6d, 0x01, 0x63, 0x12],
            "tagName locals decl (non-projecting)"
        );
        assert!(
            verbatim_plan_from_cert(&tag_name_cert(tag_bytes)).is_some(),
            "byte-exact tagName must carry a plan claim"
        );
    }
}
