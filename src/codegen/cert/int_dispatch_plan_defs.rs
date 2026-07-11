// Byte-first `int-dispatch-v1` plan builder.
//
// An Int-face `ref.test`-dispatch body (the ADT-match `Cod := Int` shapes:
// `Cert::VariantDispatch` / `Cert::WidenedIntMatch`) reconstructs losslessly
// from the byte-derived cert holes into a DEDICATED grammar (NOT the ANF
// `FragBlock`: like the verbatim family, the multi-use scrutinee and each
// arm's projected payload are spilled to scratch locals, which pure ANF cannot
// express). The plan lowers, byte-for-byte, to the emitted code entry; it
// carries no source-level meaning and never changes the `cases`-spine proof
// face or the Int-valued model — it only moves the match body's byte-origin
// into hash-pinned Lean. Unlike the verbatim family the arms consume
// contracted host helpers; their indices are NEVER plan data — the lowerers
// take the byte-derived host-role table as a parameter and the plan names
// roles only, so a plan cannot invent a callee.
//
// The scratch-local layout is a fixed function of the arm count (Stage-0
// pinned on `boxInt`/`evalOp`/`gauge`): arm `i` (0-based, dispatch order)
// spills its projected payload to local `i+1`, the scrutinee is spilled to
// local `armCount + 1`, and one trailing unused Int-carrier scratch local is
// always declared — `armCount + 2` single-local declaration groups.

/// The host-helper role an Int-face arm combines through. Rust twin of Lean
/// `Schema.IntDispatchRole` — deliberately narrower than `HostRole` (no box).
#[derive(Clone, Copy, PartialEq)]
enum IntDispatchRole {
    Add,
    Sub,
}

/// One hit arm of an Int-face dispatch. Rust twin of Lean
/// `Schema.IntDispatchLeaf`.
#[derive(Clone, PartialEq)]
enum IntDispatchLeaf {
    /// Return the projected payload: `… local.set F; local.get F`.
    Proj,
    /// Combine the projected payload with the boxed constant `k` through the
    /// `role` helper; `const_first` selects `k ⊕ x` vs `x ⊕ k`.
    HostOp {
        role: IntDispatchRole,
        k: i64,
        const_first: bool,
    },
}

/// A right-nested Int-face `ref.test` dispatch cascade. Rust twin of Lean
/// `Schema.IntDispatchCascade`.
#[derive(Clone, PartialEq)]
enum IntDispatchCascade {
    /// The terminal else: a boxed integer constant (`i64.const k; call box`).
    Default(i64),
    Test {
        ty_idx: u32,
        hit: IntDispatchLeaf,
        rest: Box<IntDispatchCascade>,
    },
}

/// Raw, untrusted Int-face `ref.test`-dispatch plan (`int-dispatch-v1`). Rust
/// twin of Lean `Schema.IntDispatchRawPlan` (the `profile` field is hard-coded
/// by the Lean renderer, so it is not carried here).
#[derive(Clone, PartialEq)]
struct IntDispatchRawPlan {
    body: IntDispatchCascade,
}

/// The byte-derived host indices the lowering is parameterized by (the Rust
/// counterpart of the Lean host-role table parameter). `add`/`sub` are absent
/// when the cert consumes no such contract; a leaf citing an absent role
/// fail-closes the lowering.
#[derive(Clone, Copy)]
struct IntDispatchHostTable {
    box_idx: u32,
    add_idx: Option<u32>,
    sub_idx: Option<u32>,
}

impl IntDispatchHostTable {
    fn role_idx(&self, role: IntDispatchRole) -> Option<u32> {
        match role {
            IntDispatchRole::Add => self.add_idx,
            IntDispatchRole::Sub => self.sub_idx,
        }
    }
}

fn int_dispatch_arm_count(c: &IntDispatchCascade) -> usize {
    match c {
        IntDispatchCascade::Default(_) => 0,
        IntDispatchCascade::Test { rest, .. } => int_dispatch_arm_count(rest) + 1,
    }
}

/// Build the byte-first `int-dispatch-v1` plan for a variant-dispatch or
/// widened-Int-match cert. Returns `None` for any other class, and —
/// fail-closed — for a certified body whose REAL code entry does not equal the
/// canonical plan lowering (a body byte-noisier than the canonical template,
/// e.g. a widened match recognised past a non-empty prefix, stays on the
/// legacy witness route; an artifact must never carry a byte-origin claim its
/// own bytes cannot prove).
fn int_dispatch_plan_from_cert(c: &Cert, strict: FragHostTable) -> Option<IntDispatchRawPlan> {
    let (plan, carrier, hosts, code_entry_bytes) = match c.inner() {
        Cert::WidenedIntMatch {
            hit_variant_idx,
            carrier,
            box_idx,
            code_entry_bytes,
            ..
        } => {
            let plan = IntDispatchRawPlan {
                body: IntDispatchCascade::Test {
                    ty_idx: *hit_variant_idx,
                    hit: IntDispatchLeaf::Proj,
                    rest: Box::new(IntDispatchCascade::Default(0)),
                },
            };
            let hosts = IntDispatchHostTable {
                box_idx: *box_idx,
                add_idx: None,
                sub_idx: None,
            };
            (plan, *carrier, hosts, code_entry_bytes)
        }
        Cert::VariantDispatch {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            arms,
            default_k,
            code_entry_bytes,
            ..
        } => {
            let mut body = IntDispatchCascade::Default(*default_k);
            for (tag, leaf) in arms.iter().rev() {
                let hit = match leaf {
                    ArmLeaf::Proj => IntDispatchLeaf::Proj,
                    ArmLeaf::HostOp {
                        role,
                        k,
                        const_first,
                    } => IntDispatchLeaf::HostOp {
                        role: match role {
                            HostRole::Add => IntDispatchRole::Add,
                            HostRole::Sub => IntDispatchRole::Sub,
                            HostRole::StringEq | HostRole::StringConcat => return None,
                        },
                        k: *k,
                        const_first: *const_first,
                    },
                };
                body = IntDispatchCascade::Test {
                    ty_idx: *tag,
                    hit,
                    rest: Box::new(body),
                };
            }
            let plan = IntDispatchRawPlan { body };
            let hosts = IntDispatchHostTable {
                box_idx: *box_idx,
                add_idx: *add_idx,
                sub_idx: *sub_idx,
            };
            (plan, *carrier, hosts, code_entry_bytes)
        }
        _ => return None,
    };
    // Role provenance: every host index the claim would cite must be confirmed
    // by the STRICT byte-derived role table (carrier-binop signature + strict
    // first-i64-arith + uniqueness, fail-closed per role). The classifier's
    // coarse marker map assigns a both-ops helper by its first arithmetic; a
    // role the strict table leaves unbound (ambiguous candidates) or binds to
    // a different index must not carry a plan claim — the export stays on the
    // legacy witness route, fail-closed.
    if strict.box_idx != Some(hosts.box_idx) {
        return None;
    }
    if let Some(a) = hosts.add_idx
        && strict.add_idx != Some(a)
    {
        return None;
    }
    if let Some(sub) = hosts.sub_idx
        && strict.sub_idx != Some(sub)
    {
        return None;
    }
    let lowered = lower_int_dispatch_code_entry(&plan, carrier, &hosts)?;
    if &lowered != code_entry_bytes {
        return None;
    }
    Some(plan)
}

/// The byte-derived host table of an Int-face dispatch cert, for claim
/// rendering and lowering. `None` for any other class.
fn int_dispatch_host_table_from_cert(c: &Cert) -> Option<IntDispatchHostTable> {
    match c.inner() {
        Cert::WidenedIntMatch { box_idx, .. } => Some(IntDispatchHostTable {
            box_idx: *box_idx,
            add_idx: None,
            sub_idx: None,
        }),
        Cert::VariantDispatch {
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => Some(IntDispatchHostTable {
            box_idx: *box_idx,
            add_idx: *add_idx,
            sub_idx: *sub_idx,
        }),
        _ => None,
    }
}

/// Exact code-entry bytes of an Int-face dispatch plan. Twin of Lean
/// `PlanBytes.lowerIntDispatchCodeEntry` (heap indices s33-signed;
/// `struct.get` type/field indices uleb32; constants sleb64).
fn lower_int_dispatch_code_entry(
    plan: &IntDispatchRawPlan,
    carrier: u32,
    hosts: &IntDispatchHostTable,
) -> Option<Vec<u8>> {
    let body = lower_int_dispatch_body_bytes(plan, carrier, hosts)?;
    let mut out = Vec::new();
    push_u32_leb(&mut out, body.len() as u32);
    out.extend_from_slice(&body);
    Some(out)
}

fn lower_int_dispatch_body_bytes(
    plan: &IntDispatchRawPlan,
    carrier: u32,
    hosts: &IntDispatchHostTable,
) -> Option<Vec<u8>> {
    let arm_count = int_dispatch_arm_count(&plan.body);
    let s = (arm_count + 1) as u32;
    let mut out = Vec::new();
    // Local declarations: arm payload spills, the eqref scrutinee, the unused
    // carrier scratch — `arm_count + 2` single-local groups.
    push_u32_leb(&mut out, (arm_count + 2) as u32);
    for _ in 0..arm_count {
        out.extend_from_slice(&[0x01, 0x63]);
        push_s33_heap_idx(&mut out, carrier);
    }
    out.extend_from_slice(&[0x01, 0x6d, 0x01, 0x63]);
    push_s33_heap_idx(&mut out, carrier);
    // Expression: spill the scrutinee, then the dispatch cascade.
    out.extend_from_slice(&[0x20, 0x00, 0x21]);
    push_u32_leb(&mut out, s);
    out.push(0x20);
    push_u32_leb(&mut out, s);
    int_dispatch_cascade_bytes(&mut out, carrier, hosts, s, 0, true, &plan.body)?;
    out.push(0x0b);
    Some(out)
}

fn int_dispatch_cascade_bytes(
    out: &mut Vec<u8>,
    carrier: u32,
    hosts: &IntDispatchHostTable,
    s: u32,
    pos: u32,
    first: bool,
    cascade: &IntDispatchCascade,
) -> Option<()> {
    match cascade {
        IntDispatchCascade::Default(k) => {
            out.push(0x42);
            push_i64_leb(out, *k);
            out.push(0x10);
            push_u32_leb(out, hosts.box_idx);
            Some(())
        }
        IntDispatchCascade::Test { ty_idx, hit, rest } => {
            if !first {
                out.push(0x20);
                push_u32_leb(out, s);
            }
            out.extend_from_slice(&[0xfb, 0x14]);
            push_s33_heap_idx(out, *ty_idx);
            out.extend_from_slice(&[0x04, 0x63]);
            push_s33_heap_idx(out, carrier);
            int_dispatch_arm_bytes(out, hosts, s, pos + 1, *ty_idx, hit)?;
            out.push(0x05);
            int_dispatch_cascade_bytes(out, carrier, hosts, s, pos + 1, false, rest)?;
            out.push(0x0b);
            Some(())
        }
    }
}

fn int_dispatch_arm_bytes(
    out: &mut Vec<u8>,
    hosts: &IntDispatchHostTable,
    s: u32,
    f: u32,
    ty_idx: u32,
    leaf: &IntDispatchLeaf,
) -> Option<()> {
    // The shared arm prefix: project field 0 of the tested variant out of the
    // spilled scrutinee and spill it to this arm's scratch local.
    out.push(0x20);
    push_u32_leb(out, s);
    out.extend_from_slice(&[0xfb, 0x16]);
    push_s33_heap_idx(out, ty_idx);
    out.extend_from_slice(&[0xfb, 0x02]);
    push_u32_leb(out, ty_idx);
    out.push(0x00);
    out.push(0x21);
    push_u32_leb(out, f);
    match leaf {
        IntDispatchLeaf::Proj => {
            out.push(0x20);
            push_u32_leb(out, f);
            Some(())
        }
        IntDispatchLeaf::HostOp {
            role,
            k,
            const_first,
        } => {
            let host_idx = hosts.role_idx(*role)?;
            if *const_first {
                out.push(0x42);
                push_i64_leb(out, *k);
                out.push(0x10);
                push_u32_leb(out, hosts.box_idx);
                out.push(0x20);
                push_u32_leb(out, f);
            } else {
                out.push(0x20);
                push_u32_leb(out, f);
                out.push(0x42);
                push_i64_leb(out, *k);
                out.push(0x10);
                push_u32_leb(out, hosts.box_idx);
            }
            out.push(0x10);
            push_u32_leb(out, host_idx);
            Some(())
        }
    }
}

/// The per-export byte-derived host-role table an Int-face dispatch claim
/// carries: the box helper always, the add/sub helpers exactly when the cert's
/// obligation wires them. Rendered identically by producer and verifier so the
/// artifact data pin stays byte-exact.
fn int_dispatch_host_table_lean_value(hosts: &IntDispatchHostTable) -> String {
    let mut entries = vec![format!("(.box, {})", hosts.box_idx)];
    if let Some(a) = hosts.add_idx {
        entries.push(format!("(.add, {a})"));
    }
    if let Some(s) = hosts.sub_idx {
        entries.push(format!("(.sub, {s})"));
    }
    format!("[{}]", entries.join(", "))
}

/// The Lean `IntDispatchLeaf` literal.
fn int_dispatch_leaf_lean_value(l: &IntDispatchLeaf) -> String {
    match l {
        IntDispatchLeaf::Proj => ".proj".to_string(),
        IntDispatchLeaf::HostOp {
            role,
            k,
            const_first,
        } => format!(
            ".hostOp .{} ({k}) {const_first}",
            match role {
                IntDispatchRole::Add => "add",
                IntDispatchRole::Sub => "sub",
            }
        ),
    }
}

/// The Lean `IntDispatchCascade` literal.
fn int_dispatch_cascade_lean_value(c: &IntDispatchCascade) -> String {
    match c {
        IntDispatchCascade::Default(k) => format!(".default ({k})"),
        IntDispatchCascade::Test { ty_idx, hit, rest } => format!(
            ".test {ty_idx} ({}) ({})",
            int_dispatch_leaf_lean_value(hit),
            int_dispatch_cascade_lean_value(rest)
        ),
    }
}

/// The Lean `IntDispatchRawPlan` literal (profile `int-dispatch-v1`), rendered
/// on ONE line (a multi-line anonymous-constructor literal misparses).
fn int_dispatch_plan_lean_value(plan: &IntDispatchRawPlan) -> String {
    format!(
        "{{ profile := \"int-dispatch-v1\", body := {} }}",
        int_dispatch_cascade_lean_value(&plan.body)
    )
}

#[cfg(test)]
mod int_dispatch_plan_gate_tests {
    use super::*;

    /// The `boxInt` cert (widened Int match, func 12): tests struct type 3,
    /// projects the carrier payload, boxed-`0` default. carrier 18, box 34.
    fn box_int_cert(code_entry_bytes: Vec<u8>) -> Cert {
        Cert::WidenedIntMatch {
            name: "boxInt".to_string(),
            self_idx: 12,
            nlocals: 3,
            carrier: 18,
            hit_variant_idx: 3,
            box_idx: 34,
            code_entry_bytes,
            ops: vec![
                Op::LocalGet(0),
                Op::LocalSet(2),
                Op::LocalGet(2),
                Op::RefTest(3),
                Op::If,
                Op::LocalGet(2),
                Op::RefCast(3),
                Op::StructGet(3, 0),
                Op::LocalSet(1),
                Op::LocalGet(1),
                Op::Else,
                Op::I64Const(0),
                Op::Call(34),
                Op::End,
            ],
        }
    }

    /// The `gauge` cert (general variant dispatch, func 15): Lo -> `0 - x`
    /// (sub, const first), Hi -> `x + 9` (add, payload first), Mid -> `x`
    /// (proj), Off -> boxed `7`. Tags 11/12/13, carrier 18, box 34, add 35,
    /// sub 36.
    fn gauge_cert(code_entry_bytes: Vec<u8>) -> Cert {
        Cert::VariantDispatch {
            name: "gauge".to_string(),
            self_idx: 15,
            nlocals: 5,
            carrier: 18,
            box_idx: 34,
            add_idx: Some(35),
            sub_idx: Some(36),
            arms: vec![
                (
                    11,
                    ArmLeaf::HostOp {
                        role: HostRole::Sub,
                        k: 0,
                        const_first: true,
                    },
                ),
                (
                    12,
                    ArmLeaf::HostOp {
                        role: HostRole::Add,
                        k: 9,
                        const_first: false,
                    },
                ),
                (13, ArmLeaf::Proj),
            ],
            default_k: 7,
            code_entry_bytes,
            ops: Vec::new(),
        }
    }

    /// The strict byte-derived role table matching the fixture's honest
    /// helpers (box 34, add 35, sub 36).
    fn strict_table() -> FragHostTable {
        FragHostTable {
            box_idx: Some(34),
            add_idx: Some(35),
            sub_idx: Some(36),
        }
    }

    /// Stage-0 pins: the exact code entries of `boxInt` (widened match) and
    /// `gauge` (three-arm dispatch, all leaf kinds) as emitted for
    /// `tools/certkit/fixtures/cert_goals.av`.
    #[test]
    fn int_dispatch_plan_reproduces_stage0_pins() {
        let box_int_entry: Vec<u8> = vec![
            0x29, 0x03, 0x01, 0x63, 0x12, 0x01, 0x6d, 0x01, 0x63, 0x12, 0x20, 0x00, 0x21, 0x02,
            0x20, 0x02, 0xfb, 0x14, 0x03, 0x04, 0x63, 0x12, 0x20, 0x02, 0xfb, 0x16, 0x03, 0xfb,
            0x02, 0x03, 0x00, 0x21, 0x01, 0x20, 0x01, 0x05, 0x42, 0x00, 0x10, 0x22, 0x0b, 0x0b,
        ];
        let gauge_entry: Vec<u8> = vec![
            0x69, 0x05, 0x01, 0x63, 0x12, 0x01, 0x63, 0x12, 0x01, 0x63, 0x12, 0x01, 0x6d, 0x01,
            0x63, 0x12, 0x20, 0x00, 0x21, 0x04, 0x20, 0x04, 0xfb, 0x14, 0x0b, 0x04, 0x63, 0x12,
            0x20, 0x04, 0xfb, 0x16, 0x0b, 0xfb, 0x02, 0x0b, 0x00, 0x21, 0x01, 0x42, 0x00, 0x10,
            0x22, 0x20, 0x01, 0x10, 0x24, 0x05, 0x20, 0x04, 0xfb, 0x14, 0x0c, 0x04, 0x63, 0x12,
            0x20, 0x04, 0xfb, 0x16, 0x0c, 0xfb, 0x02, 0x0c, 0x00, 0x21, 0x02, 0x20, 0x02, 0x42,
            0x09, 0x10, 0x22, 0x10, 0x23, 0x05, 0x20, 0x04, 0xfb, 0x14, 0x0d, 0x04, 0x63, 0x12,
            0x20, 0x04, 0xfb, 0x16, 0x0d, 0xfb, 0x02, 0x0d, 0x00, 0x21, 0x03, 0x20, 0x03, 0x05,
            0x42, 0x07, 0x10, 0x22, 0x0b, 0x0b, 0x0b, 0x0b,
        ];
        let box_plan = int_dispatch_plan_from_cert(&box_int_cert(box_int_entry.clone()), strict_table())
            .expect("boxInt plan");
        let hosts = IntDispatchHostTable {
            box_idx: 34,
            add_idx: None,
            sub_idx: None,
        };
        assert_eq!(
            lower_int_dispatch_code_entry(&box_plan, 18, &hosts).unwrap(),
            box_int_entry,
            "boxInt canonical lowering must equal the Stage-0 pin"
        );
        assert!(
            int_dispatch_plan_from_cert(&gauge_cert(gauge_entry.clone()), strict_table()).is_some(),
            "byte-exact gauge must carry a plan claim"
        );

        // Byte-noisy body: an extra byte -> no claim, legacy route, fail-closed.
        let mut noisy = gauge_entry.clone();
        noisy.push(0x00);
        noisy[0] += 1;
        assert!(
            int_dispatch_plan_from_cert(&gauge_cert(noisy), strict_table()).is_none(),
            "a body the canonical plan cannot reproduce must not carry a claim"
        );
        // Empty code entry: no claim.
        assert!(
            int_dispatch_plan_from_cert(&box_int_cert(Vec::new()), strict_table()).is_none(),
            "empty code entry cannot equal the canonical lowering"
        );
    }

    /// Role provenance: a plan claim is withheld unless the STRICT role table
    /// (signature + strict first-i64-arith + uniqueness, fail-closed) confirms
    /// every host index the claim would cite. An ambiguous helper (both add and
    /// sub candidates in one body) leaves the strict role unbound (`None`), so
    /// the coarse first-arith marker alone must never mint a plan — the export
    /// stays on the legacy witness route.
    #[test]
    fn int_dispatch_plan_requires_strict_role_provenance() {
        let gauge_entry: Vec<u8> = vec![
            0x69, 0x05, 0x01, 0x63, 0x12, 0x01, 0x63, 0x12, 0x01, 0x63, 0x12, 0x01, 0x6d, 0x01,
            0x63, 0x12, 0x20, 0x00, 0x21, 0x04, 0x20, 0x04, 0xfb, 0x14, 0x0b, 0x04, 0x63, 0x12,
            0x20, 0x04, 0xfb, 0x16, 0x0b, 0xfb, 0x02, 0x0b, 0x00, 0x21, 0x01, 0x42, 0x00, 0x10,
            0x22, 0x20, 0x01, 0x10, 0x24, 0x05, 0x20, 0x04, 0xfb, 0x14, 0x0c, 0x04, 0x63, 0x12,
            0x20, 0x04, 0xfb, 0x16, 0x0c, 0xfb, 0x02, 0x0c, 0x00, 0x21, 0x02, 0x20, 0x02, 0x42,
            0x09, 0x10, 0x22, 0x10, 0x23, 0x05, 0x20, 0x04, 0xfb, 0x14, 0x0d, 0x04, 0x63, 0x12,
            0x20, 0x04, 0xfb, 0x16, 0x0d, 0xfb, 0x02, 0x0d, 0x00, 0x21, 0x03, 0x20, 0x03, 0x05,
            0x42, 0x07, 0x10, 0x22, 0x0b, 0x0b, 0x0b, 0x0b,
        ];
        // Confirmed table -> plan.
        assert!(
            int_dispatch_plan_from_cert(&gauge_cert(gauge_entry.clone()), strict_table()).is_some(),
            "strict-confirmed roles must carry a plan claim"
        );
        // Ambiguous sub candidates: the strict derivation fail-closed to None
        // even though the coarse marker map still tagged index 36 as Sub.
        assert!(
            int_dispatch_plan_from_cert(
                &gauge_cert(gauge_entry.clone()),
                FragHostTable {
                    box_idx: Some(34),
                    add_idx: Some(35),
                    sub_idx: None,
                }
            )
            .is_none(),
            "an ambiguous (strict-unbound) sub role must not carry a plan claim"
        );
        // Strict table binding a role to a DIFFERENT index than the cert cites.
        assert!(
            int_dispatch_plan_from_cert(
                &gauge_cert(gauge_entry.clone()),
                FragHostTable {
                    box_idx: Some(34),
                    add_idx: Some(99),
                    sub_idx: Some(36),
                }
            )
            .is_none(),
            "a strict-table disagreement on the add index must not carry a plan claim"
        );
        // Unconfirmed box helper.
        assert!(
            int_dispatch_plan_from_cert(
                &gauge_cert(gauge_entry),
                FragHostTable {
                    box_idx: Some(1),
                    add_idx: Some(35),
                    sub_idx: Some(36),
                }
            )
            .is_none(),
            "a strict-table disagreement on the box index must not carry a plan claim"
        );
    }

    /// A leaf citing a host role the table lacks fail-closes the lowering (the
    /// plan cannot conjure a callee out of a missing contract).
    #[test]
    fn int_dispatch_lowering_fail_closes_on_missing_role() {
        let plan = IntDispatchRawPlan {
            body: IntDispatchCascade::Test {
                ty_idx: 3,
                hit: IntDispatchLeaf::HostOp {
                    role: IntDispatchRole::Sub,
                    k: 0,
                    const_first: true,
                },
                rest: Box::new(IntDispatchCascade::Default(0)),
            },
        };
        let no_sub = IntDispatchHostTable {
            box_idx: 34,
            add_idx: Some(35),
            sub_idx: None,
        };
        assert!(
            lower_int_dispatch_code_entry(&plan, 18, &no_sub).is_none(),
            "a sub leaf with no sub contract must not lower"
        );
    }
}
