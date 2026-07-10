/// Classification of every user function in the module.
pub struct Analysis {
    certs: Vec<Cert>,
    declined: Vec<(String, String)>,
    carrier: Option<u32>,
    contracts: Vec<String>,
}

impl Analysis {
    pub fn certified_names(&self) -> Vec<String> {
        self.certs.iter().map(|c| c.name().to_string()).collect()
    }
    pub fn declined(&self) -> &[(String, String)] {
        &self.declined
    }
}

/// Disassemble the emitted module and classify each user function. `model_files`
/// are the reused `aver proof` Lean model; the recursion classifier reads the
/// combinator operator (`+`/`*`) from them since the bytes cannot tell the bignum
/// helpers apart.
pub fn analyze(wasm_bytes: &[u8], model_files: &[(String, String)]) -> Result<Analysis, String> {
    analyze_with_fragment_plans(wasm_bytes, model_files, &[])
}

pub fn analyze_with_fragment_plans(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
    fragment_plans: &[FragmentPlanArtifact],
) -> Result<Analysis, String> {
    let (user_fns, box_idx, user_idx_set, carrier, host_roles, _frag_host_table, _struct_field_counts) =
        disassemble(wasm_bytes)?;
    let model_ops = model_step_ops(model_files);

    // Index the user functions so the composition pass can walk the call graph.
    let fns: std::collections::HashMap<u32, &UserFn> =
        user_fns.iter().map(|f| (f.wasm_idx, f)).collect();
    let user_names: std::collections::HashSet<&str> =
        user_fns.iter().map(|f| f.name.as_str()).collect();
    let mut producer_plans = std::collections::HashMap::<&str, &FragmentPlan>::new();
    for artifact in fragment_plans {
        if !user_names.contains(artifact.export_name.as_str()) {
            return Err(format!(
                "producer supplied fragment plan for unknown export `{}`",
                artifact.export_name
            ));
        }
        if producer_plans
            .insert(artifact.export_name.as_str(), &artifact.plan)
            .is_some()
        {
            return Err(format!(
                "producer supplied duplicate fragment plan for `{}`",
                artifact.export_name
            ));
        }
    }

    let mut certs = Vec::new();
    let mut declined = Vec::new();
    for f in &user_fns {
        if let Some(plan) = producer_plans.get(f.name.as_str()) {
            let checked = match plan {
                FragmentPlan::Sym(plan) => {
                    check_sym_fragment_plan_object(wasm_bytes, &f.name, (*plan).clone())
                }
                FragmentPlan::Expr(plan) => {
                    check_expr_fragment_plan_object(wasm_bytes, &f.name, (*plan).clone())
                }
            };
            match checked {
                Ok((_func_order, cert, _sidecar, true, _reason)) => certs.push(cert),
                Ok((_func_order, _cert, _sidecar, false, reason)) => declined.push((
                    f.name.clone(),
                    format!(
                        "producer fragment plan does not match emitted wasm: {}",
                        reason.unwrap_or_else(|| "unknown mismatch".to_string())
                    ),
                )),
                Err(reason) => declined.push((
                    f.name.clone(),
                    format!("producer fragment plan rejected: {reason}"),
                )),
            }
            continue;
        }
        match classify_without_expr_fragment(
            f,
            box_idx,
            carrier,
            &user_idx_set,
            &fns,
            &host_roles,
            &model_ops,
        ) {
            Ok(c) => certs.push(c),
            Err(reason) => declined.push((f.name.clone(), reason)),
        }
    }

    // Named runtime contracts actually consumed by the certified functions.
    let contracts = runtime_contracts_for_certs(&certs);

    Ok(Analysis {
        certs,
        declined,
        carrier,
        contracts,
    })
}

fn runtime_contracts_for_certs<'a>(certs: impl IntoIterator<Item = &'a Cert>) -> Vec<String> {
    let mut contracts = Vec::new();
    let mut has_box = false;
    let mut has_add = false;
    let mut has_sub = false;
    let mut has_string_eq = false;
    let mut has_string_concat = false;
    for c in certs {
        if c.int_add_face().is_some() {
            has_box = true;
            has_add = true;
            continue;
        }
        match c.inner() {
            Cert::StraightLine { .. } => {
                has_box = true;
                has_add = true;
            }
            Cert::Recursive { .. } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
            Cert::AccumulatorRecursive { .. } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
            Cert::AdtConstructor { .. }
            | Cert::FieldProjection { .. }
            | Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::ExprFragment { .. } => {}
            Cert::StringEqVerbatimMatch { .. } => {
                has_string_eq = true;
            }
            Cert::StringConcatVerbatimMatch { .. } => {
                has_string_concat = true;
            }
            Cert::MutualRecursion { .. } => {
                // The shared host wires box + sub (no combinator).
                has_box = true;
                has_sub = true;
            }
            Cert::WidenedIntMatch { .. } => {
                has_box = true;
            }
            Cert::VariantDispatch {
                add_idx, sub_idx, ..
            } => {
                has_box = true;
                has_add |= add_idx.is_some();
                has_sub |= sub_idx.is_some();
            }
            Cert::Composition {
                has_add: a,
                has_sub: s,
                has_box: b,
                ..
            } => {
                has_add |= *a;
                has_sub |= *s;
                has_box |= *b;
            }
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    if has_box {
        contracts.push(BOX_CONTRACT.to_string());
    }
    if has_add {
        contracts.push(INT_ADD_CONTRACT.to_string());
    }
    if has_sub {
        contracts.push(INT_SUB_CONTRACT.to_string());
    }
    if has_string_eq {
        contracts.push(STRING_EQ_CONTRACT.to_string());
    }
    if has_string_concat {
        contracts.push(STRING_CONCAT_CONTRACT.to_string());
    }
    contracts
}

#[cfg(all(test, feature = "wasm-compile"))]
mod analysis_tests {
    use super::*;

    fn compile_float_add_probe() -> crate::codegen::wasm_gc::WasmGcCompileOutput {
        let mut items = crate::source::parse_source(
            r#"
module PlanFirstProbe
    intent = "plan-first producer overlay probe"
    depends []
    exposes [floatAddGoal]

fn floatAddGoal(a: Float, b: Float) -> Float
    ? "Small scalar island."
    a + b
"#,
        )
        .expect("source parses");
        let pipeline = crate::ir::pipeline::run(
            &mut items,
            crate::ir::PipelineConfig {
                typecheck: Some(crate::ir::TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        assert!(
            pipeline
                .typecheck
                .as_ref()
                .is_none_or(|tc| tc.errors.is_empty()),
            "probe source should typecheck"
        );
        crate::codegen::wasm_gc::compile_to_wasm_gc_with_handler_and_cert_plans(
            &items, None, None,
        )
        .expect("probe compiles to wasm-gc")
    }

    #[test]
    fn expr_fragment_certification_requires_matching_producer_plan() {
        let output = compile_float_add_probe();
        let without_plan = analyze(&output.bytes, &[]).expect("analysis without producer plan");
        assert!(
            !without_plan
                .certified_names()
                .contains(&"floatAddGoal".to_string()),
            "expr-fragment should not be certified without a producer plan"
        );

        let checked = analyze_with_fragment_plans(&output.bytes, &[], &output.fragment_plans)
            .expect("analysis with producer plan");
        assert!(
            checked
                .certified_names()
                .contains(&"floatAddGoal".to_string()),
            "matching producer plan should certify the probe"
        );
        let source_plan = checked
            .certs
            .iter()
            .find_map(|cert| match cert.inner() {
                Cert::ExprFragment {
                    name,
                    source_plan,
                    ..
                } if name == "floatAddGoal" => source_plan.as_ref(),
                _ => None,
            })
            .expect("source-level producer plan should be preserved on the cert");
        assert_eq!(source_plan.result, SymTy::Float);

        let mut tampered = output
            .fragment_plans
            .iter()
            .find(|artifact| artifact.export_name == "floatAddGoal")
            .expect("producer emitted a floatAddGoal plan")
            .clone();
        let FragmentPlan::Sym(sym_plan) = &mut tampered.plan else {
            panic!("source-level producer should emit floatAddGoal as a SymPlan");
        };
        let mut changed = false;
        for node in &mut sym_plan.body.nodes {
            if let SymNodeKind::Prim { op, .. } = &mut node.kind
                && *op == SymPrim::FloatAdd
            {
                *op = SymPrim::FloatMul;
                changed = true;
                break;
            }
        }
        assert!(changed, "probe source plan should contain float.add");

        let checked = analyze_with_fragment_plans(&output.bytes, &[], &[tampered])
            .expect("analysis should report a declined producer plan");
        assert!(
            !checked
                .certified_names()
                .contains(&"floatAddGoal".to_string()),
            "a bad producer plan must not fall back to byte-derived classification"
        );
        let reason = checked
            .declined()
            .iter()
            .find(|(name, _)| name == "floatAddGoal")
            .map(|(_, reason)| reason.as_str())
            .expect("floatAddGoal should be declined");
        assert!(
            reason.contains("producer fragment plan does not match emitted wasm"),
            "decline reason should identify producer-plan mismatch, got: {reason}"
        );
    }

    #[test]
    /// The plan-first host-role table must bind `add` to EXACTLY the callee a
    /// straight-line body actually cites. In a real bignum module the coarse
    /// `host_roles` map carries the whole add/mul combinator family (the mul
    /// helper's umag loops also contain `i64.add`), so this pins that the
    /// strict table (signature + first-i64-arith + uniqueness) never rides on
    /// index order the way the removed `min()` derivation did.
    #[test]
    fn frag_host_table_binds_add_to_the_cited_callee() {
        let mut items = crate::source::parse_source(
            r#"
module RoleProbe
    intent = "host role probe"
    depends []
    exposes [addTwo]

fn addTwo(x: Int) -> Int
    ? "Straight-line integer arithmetic."
    x + 2
"#,
        )
        .expect("source parses");
        let pipeline = crate::ir::pipeline::run(
            &mut items,
            crate::ir::PipelineConfig {
                typecheck: Some(crate::ir::TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        assert!(
            pipeline
                .typecheck
                .as_ref()
                .is_none_or(|tc| tc.errors.is_empty()),
            "probe source should typecheck"
        );
        let output = crate::codegen::wasm_gc::compile_to_wasm_gc_with_handler_and_cert_plans(
            &items, None, None,
        )
        .expect("probe compiles to wasm-gc");
        let (user_fns, box_idx, _set, _carrier, host_roles, host_table, _struct_counts) =
            disassemble(&output.bytes).expect("disassemble");
        let add_two = user_fns
            .iter()
            .find(|f| f.name == "addTwo")
            .expect("addTwo user fn");
        let [cited_box, cited_add] = add_two.calls.as_slice() else {
            panic!("addTwo should cite exactly box + add, got {:?}", add_two.calls);
        };
        // The coarse role family really is ambiguous in a bignum module: more
        // than one helper carries the Add marker (genuine add + mul at least).
        let coarse_add_count = host_roles
            .values()
            .filter(|role| **role == HostRole::Add)
            .count();
        assert!(
            coarse_add_count >= 2,
            "expected the coarse role map to be ambiguous (add + mul family), \
             got {coarse_add_count} Add-marked helpers"
        );
        // The strict table still binds box/add to exactly the cited callees.
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(host_table.box_idx, Some(*cited_box));
        assert_eq!(
            host_table.add_idx,
            Some(*cited_add),
            "the strict host-role table must bind `add` to the callee the \
             emitted body cites"
        );
    }

    /// Synthetic-module template for the role-table derivation tests: an
    /// optional decoy function (placed at a LOWER index than the genuine add
    /// helper) plus the exact carrier-binop add helper and the named box
    /// export the disassembler requires.
    fn role_table_module(decoy: &str) -> Vec<u8> {
        wat::parse_str(format!(
            r#"(module
  (type $mag (array (mut i64)))
  (type $c (struct (field i64) (field (ref null $mag)) (field i32)))
  (type $bin (func (param (ref null $c)) (param (ref null $c)) (result (ref null $c))))
  (type $box (func (param i64) (result (ref null $c))))
  {decoy}
  (func $box (type $box)
    local.get 0 ref.null $mag i32.const 0 struct.new $c)
  (func $add (type $bin)
    i64.const 1 i64.const 2 i64.add drop local.get 0)
  (export "__rt_aint_from_i64" (func $box))
)"#
        ))
        .expect("role-table module WAT parses")
    }

    /// An EARLIER helper whose body is `i64.add`-shaped but whose signature is
    /// not the carrier binop must never capture the `add` role: the table
    /// binds the genuine helper, index order notwithstanding.
    #[test]
    fn frag_host_table_ignores_earlier_non_carrier_i64_add_helper() {
        let bytes = role_table_module(
            r#"(func $decoy (param i64) (param i64) (result i64)
    local.get 0 local.get 1 i64.add)"#,
        );
        let (_fns, box_idx, _set, carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(carrier, Some(1), "carrier struct should be recognised");
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(
            host_table.add_idx,
            Some(2),
            "the genuine carrier-binop add helper (idx 2) must win over the \
             earlier i64-shaped decoy (idx 0)"
        );
    }

    /// If more than one candidate matches the strict signature + body shape,
    /// the role stays UNBOUND and every plan citing it declines fail-closed —
    /// the table never guesses by index order.
    #[test]
    fn frag_host_table_declines_ambiguous_add_candidates() {
        let bytes = role_table_module(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.add drop local.get 1)"#,
        );
        let (_fns, box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(
            host_table.add_idx, None,
            "two byte-shape-identical add candidates must leave the role \
             unbound (fail-closed), never bound by index order"
        );
    }

    /// The mul helper's fast path multiplies FIRST, so `first arith == add`
    /// keeps it out of the add candidacy even though its umag loops contain
    /// `i64.add` (which is what earns it the coarse Add marker).
    #[test]
    fn frag_host_table_excludes_mul_first_bodies() {
        let bytes = role_table_module(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.mul drop
    i64.const 3 i64.const 4 i64.add drop
    local.get 1)"#,
        );
        let (_fns, _box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(
            host_table.add_idx,
            Some(2),
            "a mul-first body must not compete for the add role"
        );
    }

    fn compile_probe_bytes(src: &str) -> Vec<u8> {
        let mut items = crate::source::parse_source(src).expect("probe source parses");
        let pipeline = crate::ir::pipeline::run(
            &mut items,
            crate::ir::PipelineConfig {
                typecheck: Some(crate::ir::TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        assert!(
            pipeline
                .typecheck
                .as_ref()
                .is_none_or(|tc| tc.errors.is_empty()),
            "probe source should typecheck"
        );
        crate::codegen::wasm_gc::compile_to_wasm_gc_with_handler_and_cert_plans(&items, None, None)
            .expect("probe compiles to wasm-gc")
            .bytes
    }

    /// The verbatim-widened fixture's `_ -> []` default arm lowers to a
    /// `ref.null` of the `List` struct type. Disassembly must thread that
    /// heap-type index through `Op::RefNull` (not drop it, as the old unit
    /// variant did) so the S2 grammar can re-lower the empty-list default
    /// byte-exactly. The index must equal the module's List struct type — the
    /// same concrete type the function's `List<Int>` result references.
    #[test]
    fn ref_null_threads_default_arm_heap_type() {
        let bytes = compile_probe_bytes(include_str!(
            "../../../tools/certkit/fixtures/verbatimwiden.av"
        ));
        let (user_fns, _box_idx, _set, _carrier, _roles, _table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        let wrap_items = user_fns
            .iter()
            .find(|f| f.name == "wrapItems")
            .expect("wrapItems user fn");
        // `wrapItems` returns `List<Int>`, i.e. a concrete `(ref null $list)`.
        let Some(TyKind::Ref(list_idx)) = wrap_items.result else {
            panic!("wrapItems should return a concrete list ref");
        };
        let ref_null_hty = wrap_items
            .ops
            .iter()
            .find_map(|op| match op {
                Op::RefNull(hty) => Some(*hty),
                _ => None,
            })
            .expect("wrapItems `[]` default arm should lower to a ref.null");
        assert_eq!(
            ref_null_hty,
            Some(list_idx),
            "ref.null must carry the List struct heap-type index (the `[]` \
             default's type), not drop it"
        );
    }

    /// Mirror of `frag_host_table_binds_add_to_the_cited_callee` for the strict
    /// `sub` binding: a straight-line integer subtraction body cites box + sub,
    /// and the strict table must bind `sub` to EXACTLY that cited callee (never
    /// by index order). `x - 2` lowers to `sub(x, box(2))` — the compiler does
    /// not rewrite it as add-with-negated-constant, so it genuinely cites sub.
    #[test]
    fn frag_host_table_binds_sub_to_the_cited_callee() {
        let bytes = compile_probe_bytes(
            r#"
module SubRoleProbe
    intent = "host sub role probe"
    depends []
    exposes [subTwo]

fn subTwo(x: Int) -> Int
    ? "Straight-line integer subtraction."
    x - 2
"#,
        );
        let (user_fns, box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        let sub_two = user_fns
            .iter()
            .find(|f| f.name == "subTwo")
            .expect("subTwo user fn");
        let [cited_box, cited_sub] = sub_two.calls.as_slice() else {
            panic!("subTwo should cite exactly box + sub, got {:?}", sub_two.calls);
        };
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(host_table.box_idx, Some(*cited_box));
        assert_eq!(
            host_table.sub_idx,
            Some(*cited_sub),
            "the strict host-role table must bind `sub` to the callee the \
             emitted body cites"
        );
    }

    /// Synthetic-module template for the `sub` role-table derivation tests: an
    /// optional decoy function plus the exact carrier-binop `sub` helper (its
    /// first i64 arithmetic op is `i64.sub`) and the named box export the
    /// disassembler requires. Parallel to `role_table_module` (which emits an
    /// `add`-shaped helper); kept separate so the `add` tests' index
    /// assertions are unaffected.
    fn role_table_module_sub(decoy: &str) -> Vec<u8> {
        wat::parse_str(format!(
            r#"(module
  (type $mag (array (mut i64)))
  (type $c (struct (field i64) (field (ref null $mag)) (field i32)))
  (type $bin (func (param (ref null $c)) (param (ref null $c)) (result (ref null $c))))
  (type $box (func (param i64) (result (ref null $c))))
  {decoy}
  (func $box (type $box)
    local.get 0 ref.null $mag i32.const 0 struct.new $c)
  (func $sub (type $bin)
    i64.const 1 i64.const 2 i64.sub drop local.get 0)
  (export "__rt_aint_from_i64" (func $box))
)"#
        ))
        .expect("role-table-sub module WAT parses")
    }

    /// If more than one candidate matches the strict carrier-binop signature +
    /// `i64.sub`-first body shape, the `sub` role stays UNBOUND (fail-closed) —
    /// the table never guesses by index order. Mirrors the `add` ambiguity test.
    #[test]
    fn frag_host_table_declines_ambiguous_sub_candidates() {
        let bytes = role_table_module_sub(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.sub drop local.get 1)"#,
        );
        let (_fns, box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(host_table.box_idx, Some(box_idx));
        assert_eq!(
            host_table.sub_idx, None,
            "two byte-shape-identical sub candidates must leave the role \
             unbound (fail-closed), never bound by index order"
        );
    }

    /// Field-projection tamper matrix at the plan checker (fail-closed, no
    /// lake needed): a `struct.get.user` citing a type outside the module's
    /// struct context or the Int carrier is DECLINED at the checker; a wrong
    /// (but real) struct type or a flipped field index survives the checker
    /// but fails canonical code-entry byte equality.
    #[test]
    fn field_projection_plan_tampers_decline_fail_closed() {
        let bytes = compile_probe_bytes(
            r#"
module ProjTamperProbe
    intent = "field projection tamper probe"
    depends []
    exposes [User, userName, addTwo]

record User
  name: String
  age: Int

fn addTwo(x: Int) -> Int
  ? "Pulls in the Int carrier and box helper."
  x + 2

fn userName(u: User) -> String
  ? "Record field projection."
  u.name
"#,
        );
        let (user_fns, _box_idx, _set, carrier, _roles, _table, struct_counts) =
            disassemble(&bytes).expect("disassemble");
        let carrier = carrier.expect("carrier struct");
        let user_name = user_fns
            .iter()
            .find(|f| f.name == "userName")
            .expect("userName user fn");
        let real_ty = user_name
            .ops
            .iter()
            .find_map(|op| match op {
                Op::StructGet(t, _) if *t != carrier => Some(*t),
                _ => None,
            })
            .expect("userName projects a user struct");
        assert_eq!(
            struct_counts.get(&real_ty),
            Some(&2),
            "User struct should have two fields"
        );
        let projection_plan = |ty_idx: u32, field: u32| ExprFragmentPlan {
            params: vec![FragTy::AdtRef],
            result: FragTy::AdtRef,
            body: FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::Local { index: 0 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::StructGetUser {
                            ty_idx,
                            field,
                            value: FragValueId(0),
                        },
                    },
                ],
                result: FragValueId(1),
            },
        };

        // Baseline: the honest plan checks and matches the bytes.
        let (_order, _cert, _sidecar, matches, reason) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(real_ty, 0))
                .expect("honest projection plan is admitted");
        assert!(matches, "honest projection plan must match bytes: {reason:?}");

        // (c) ty_idx outside the module's struct types -> DECLINE at checker.
        let Err(err) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(9999, 0))
        else {
            panic!("out-of-module struct type must be declined")
        };
        assert!(
            err.contains("outside the module's struct types"),
            "wrong reason for out-of-module struct type: {err}"
        );

        // The Int carrier is never a projectable user struct -> DECLINE at checker.
        let Err(err) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(carrier, 0))
        else {
            panic!("carrier-typed projection must be declined")
        };
        assert!(
            err.contains("cites the Int carrier"),
            "wrong reason for carrier projection: {err}"
        );

        // Field outside the struct's byte-derived field count -> DECLINE at checker.
        let Err(err) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(real_ty, 5))
        else {
            panic!("projection past the field count must be declined at the checker")
        };
        assert!(
            err.contains("outside struct"),
            "wrong reason for out-of-range field: {err}"
        );

        // (a) wrong (but real) struct type index -> canonical bytes differ.
        let wrong_ty = struct_counts
            .keys()
            .copied()
            .find(|t| *t != real_ty && *t != carrier)
            .expect("module has another struct type");
        let (_order, _cert, _sidecar, matches, reason) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(wrong_ty, 0))
                .expect("wrong-type plan is well-formed but must not match");
        assert!(
            !matches,
            "wrong struct type must fail canonical byte equality"
        );
        assert!(
            reason.unwrap_or_default().contains("code_entry_bytes_match=false"),
            "wrong-type mismatch should name the byte inequality"
        );

        // (b) field index flipped 0 -> 1: well-typed, wrong bytes.
        let (_order, _cert, _sidecar, matches, _reason) =
            check_expr_fragment_plan_object(&bytes, "userName", projection_plan(real_ty, 1))
                .expect("flipped-field plan is well-formed but must not match");
        assert!(
            !matches,
            "flipped field index must fail canonical byte equality"
        );

        // A bad producer plan must decline, never fall back to legacy classes.
        let tampered = FragmentPlanArtifact {
            export_name: "userName".to_string(),
            plan: FragmentPlan::Expr(projection_plan(real_ty, 1)),
        };
        let checked = analyze_with_fragment_plans(&bytes, &[], &[tampered])
            .expect("analysis reports the declined producer plan");
        assert!(
            !checked.certified_names().contains(&"userName".to_string()),
            "tampered projection plan must not certify"
        );
        let reason = checked
            .declined()
            .iter()
            .find(|(name, _)| name == "userName")
            .map(|(_, reason)| reason.as_str())
            .expect("userName should be declined");
        assert!(
            reason.contains("producer fragment plan does not match emitted wasm"),
            "wrong tamper decline reason: {reason}"
        );
    }

    /// An `add`-first body must not compete for the `sub` role even though it
    /// contains an `i64.sub`: `first arith == sub` keeps it out of sub
    /// candidacy, so the genuine `i64.sub`-first helper (idx 2) binds alone.
    /// Mirrors `frag_host_table_excludes_mul_first_bodies` for the add role.
    #[test]
    fn frag_host_table_excludes_add_first_bodies_from_sub() {
        let bytes = role_table_module_sub(
            r#"(func $decoy (type $bin)
    i64.const 3 i64.const 4 i64.add drop
    i64.const 3 i64.const 4 i64.sub drop
    local.get 1)"#,
        );
        let (_fns, _box_idx, _set, _carrier, _roles, host_table, _struct_counts) =
            disassemble(&bytes).expect("disassemble");
        assert_eq!(
            host_table.sub_idx,
            Some(2),
            "an add-first body must not compete for the sub role"
        );
    }
}
