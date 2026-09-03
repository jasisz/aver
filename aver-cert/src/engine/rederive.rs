/// Producer-side analysis of one candidate obligation. It records the plan and
/// byte-derived facts needed to render package data. The verifier does not trust
/// this value: the Lean wall decodes and binds the relevant facts from
/// `ArtifactBytes` again.
pub struct RederivedObligation {
    pub name: String,
    /// Position in the module's byte-derived user-function list, used by the
    /// producer to keep claims in artifact order.
    pub func_order: usize,
    /// The `fun fn => ...` `CodeTbl` value (`render_code_value`).
    pub code: String,
    /// `Obligation.self`: the self function index in the module.
    pub self_idx: u32,
    /// `Obligation.carrier`: the Int carrier struct type index.
    pub carrier: u32,
    /// Producer candidates for policy and termination. `ClaimAxes` derives the
    /// accepted values again from the checked plan.
    pub policy: CertificationPolicy,
    pub termination_witness: Option<TerminationWitness>,
    /// For expression fragments, the byte-derived function-section type index.
    /// This connects export/function routing to the actual declared function
    /// signature slot; the Lean wall decodes and checks it independently.
    pub fragment_type_idx: Option<u32>,
    /// For expression fragments, the byte-derived local count recorded in the
    /// `WCode` body. The current plan-first lowering emits one scratch carrier
    /// local; artifact acceptance also binds it to the decoded code table.
    pub fragment_nlocals: Option<u32>,
    /// The expression plan rendered as an `ExprFragmentRawPlan` term.
    pub fragment_plan_lean: Option<String>,
    /// For expression fragments whose checked representation plan can be
    /// projected into the source-level symbolic grammar, the corresponding
    /// `SymRawPlan` term. Representation-only fragments leave it absent.
    pub fragment_sym_plan_lean: Option<String>,
    /// For field-projection fragments, the byte-derived struct-table entries
    /// (`source type name -> wasm struct type index`) this export pins. The
    /// producer unions these into the module-wide struct table cited by the
    /// artifact claims; empty for non-projection fragments.
    pub fragment_struct_entries: Vec<(String, u32)>,
    /// Producer-computed `List WInstr` body used as an emission consistency
    /// check. Lean lowers the plan independently.
    pub fragment_lowered_body_lean: Option<String>,
    /// Producer-computed canonical code-entry bytes used as an emission
    /// consistency check. Lean lowers and compares independently.
    pub fragment_lowered_code_entry_lean: Option<String>,
    /// The string-concat plan rendered as a `StringConcatRawPlan` term.
    pub string_concat_plan_lean: Option<String>,
    /// The source-level `SymRawPlan` view of the same byte-derived string
    /// concat shape. The checker witness requires this to explain the
    /// target-bound `StringConcatRawPlan`.
    pub string_concat_sym_plan_lean: Option<String>,
    /// For `string-concat-v1`, the byte-derived function-section type index.
    pub string_concat_type_idx: Option<u32>,
    /// For `string-concat-v1`, the producer-computed `List WInstr` body.
    pub string_concat_lowered_body_lean: Option<String>,
    /// For `string-concat-v1`, the producer-computed canonical code-entry bytes.
    pub string_concat_lowered_code_entry_lean: Option<String>,
    /// The string byte-array type used by `array.new_data` and returned by the
    /// concat helper.
    pub string_concat_result_ty: Option<u32>,
    /// The array-of-string-arrays container type used by `array.new_fixed`.
    pub string_concat_container_ty: Option<u32>,
    /// The wasm function index of the `String.concat` host helper.
    pub string_concat_func_idx: Option<u32>,
    /// The same checked String.eq plan rendered as a Lean `StringEqRawPlan` term.
    pub string_eq_plan_lean: Option<String>,
    /// The source-level `SymRawPlan` view of the same byte-derived String.eq shape.
    pub string_eq_sym_plan_lean: Option<String>,
    /// For `string-eq-v1`, the byte-derived function-section type index.
    pub string_eq_type_idx: Option<u32>,
    /// For `string-eq-v1`, the producer-computed `List WInstr` body.
    pub string_eq_lowered_body_lean: Option<String>,
    /// For `string-eq-v1`, the producer-computed canonical code-entry bytes.
    pub string_eq_lowered_code_entry_lean: Option<String>,
    /// The string byte-array type used by the String.eq dispatch.
    pub string_eq_string_ty: Option<u32>,
    /// The wasm function index of the `String.eq` host helper.
    pub string_eq_func_idx: Option<u32>,
    /// The same checked construct plan rendered as a Lean `ConstructRawPlan`
    /// term.
    pub construct_plan_lean: Option<String>,
    /// The source-level `SymRawPlan` view of the same byte-derived constructor
    /// shape.
    pub construct_sym_plan_lean: Option<String>,
    /// For `construct-v1`, the byte-derived function-section type index.
    pub construct_type_idx: Option<u32>,
    pub construct_struct_idx: Option<u32>,
    pub construct_field_count: Option<u32>,
    pub construct_elem_ty_lean: Option<String>,
    /// True only when the producer-reconstructed source result is `List<T>`;
    /// only those claims require the cons-cell-specific type-section guards.
    pub construct_is_list: bool,
    /// For `construct-v1`, the producer-computed `List WInstr` body that the
    /// checked constructor plan canonically lowers to.
    pub construct_lowered_body_lean: Option<String>,
    /// For `construct-v1`, the producer-computed canonical raw code-entry bytes
    /// that the checked constructor plan lowers to.
    pub construct_lowered_code_entry_lean: Option<String>,
    /// For `recursion-plan-v1` (fuel-recursion), the byte-derived recursion plan
    /// rendered as a Lean `RecursionRawPlan` term.
    pub recursion_plan_lean: Option<String>,
    /// For `recursion-plan-v1`, the per-export byte-derived host-role table
    /// (box/combinator/sub) rendered as the Lean `List (HostRole × Nat)`
    /// literal the recursion claim carries.
    pub recursion_host_table_lean: Option<String>,
    /// For `recursion-plan-v1`, the byte-derived function-section type index.
    pub recursion_type_idx: Option<u32>,
    /// For `recursion-plan-v1`, the producer-computed `List WInstr` body the
    /// checked recursion plan canonically lowers to (equal to `Module.lean`).
    pub recursion_lowered_body_lean: Option<String>,
    /// For `recursion-plan-v1`, the producer-computed canonical raw code-entry
    /// bytes the checked recursion plan lowers to.
    pub recursion_lowered_code_entry_lean: Option<String>,
    /// For `mutual-plan-v1` (mutual-recursion member), the byte-derived member
    /// plan rendered as a Lean `MutualRawPlan` term.
    pub mutual_plan_lean: Option<String>,
    /// For `mutual-plan-v1`, the byte-derived SCC box/sub host-role table
    /// rendered as the Lean `List (HostRole × Nat)` literal the claim carries.
    pub mutual_host_table_lean: Option<String>,
    /// For `mutual-plan-v1`, the byte-derived SCC member-index set rendered as
    /// the Lean `List Nat` literal the claim threads as member-call context.
    pub mutual_member_set_lean: Option<String>,
    /// For `mutual-plan-v1`, this member's byte-derived function-section type index.
    pub mutual_type_idx: Option<u32>,
    /// For `mutual-plan-v1`, the producer-computed `List WInstr` body this
    /// member's checked plan lowers to (equal to its arm of the shared table).
    pub mutual_lowered_body_lean: Option<String>,
    /// For `mutual-plan-v1`, the producer-computed canonical raw code-entry
    /// bytes this member's checked plan lowers to.
    pub mutual_lowered_code_entry_lean: Option<String>,
    /// For `verbatim-plan-v1` (a `Cod := WVal` verbatim `ref.test`-dispatch
    /// match), the byte-derived plan rendered as a Lean `VerbatimRawPlan` term.
    /// There is no host/self call to bind, so no host-table/member-set/
    /// lowered-body/code-entry companion fields are needed: the byte-equality
    /// gate is the whole soundness binding and the claim's witness is anonymous.
    pub verbatim_plan_lean: Option<String>,
    /// For `int-dispatch-v1` (a `Cod := Int` ADT-match: general variant
    /// dispatch or widened Int match), the byte-derived plan rendered as a Lean
    /// `IntDispatchRawPlan` term. The claim's
    /// witness is anonymous like the verbatim family's (no code/type index is
    /// carried); unlike it the arms consume host contracts, so the claim also
    /// carries the byte-derived role table below.
    pub int_dispatch_plan_lean: Option<String>,
    /// For `int-dispatch-v1`, the per-export byte-derived host-role table
    /// (box, plus add/sub exactly when wired) rendered as the Lean
    /// `List (HostRole × Nat)` literal the claim carries.
    pub int_dispatch_host_table_lean: Option<String>,
    /// `field-projection-v1` plan and byte-derived type context. The plan names
    /// only the field; struct identity/count and selected result type are
    /// reconstructed from validated module bytes.
    pub field_projection_plan_lean: Option<String>,
    pub field_projection_struct_idx: Option<u32>,
    pub field_projection_field_count: Option<u32>,
    pub field_projection_result_ty_lean: Option<String>,
    /// Plan-backed composition closure members. Each member carries the
    /// rendered shape/name plan plus byte-derived binding facts.
    pub composition_members: Vec<RederivedCompositionMember>,
    /// Strict byte-derived singleton add-role table.
    pub composition_host_table_lean: Option<String>,
    /// Exact byte-derived reachable closure names for this root.
    pub composition_member_names_lean: Option<String>,
}

#[derive(Clone)]
pub struct RederivedCompositionMember {
    pub name: String,
    pub self_idx: u32,
    pub type_idx: u32,
    pub plan_lean: String,
    pub lowered_body_lean: String,
    pub lowered_code_entry_lean: String,
}

pub struct RederivedCertificate {
    pub obligations: Vec<RederivedObligation>,
    pub contracts: Vec<String>,
}

/// Re-derive one [`RederivedObligation`] per user function that classifies into
/// a certified template, in module (obligation) order. The order and length
/// match `render_manifest_lean`'s `obligations` list, so the checker's
/// list-equality `rfl`s bind position for position.
pub fn rederive_obligations(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
) -> Result<Vec<RederivedObligation>, String> {
    Ok(rederive_certificate(wasm_bytes, model_files)?.obligations)
}

/// Re-derive the byte-bound certificate face: obligations plus the runtime
/// contracts those byte-classified obligations actually consume.
pub fn rederive_certificate(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
) -> Result<RederivedCertificate, String> {
    rederive_certificate_inner(wasm_bytes, model_files)
}

fn rederive_certificate_inner(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
) -> Result<RederivedCertificate, String> {
    let (user_fns, box_idx, user_idx_set, carrier, host_roles, frag_host_table, struct_field_counts) =
        disassemble(wasm_bytes)?;
    let model_ops = model_step_ops(model_files);
    let model_info = ModelInfo::from_files(model_files);
    let fns: std::collections::HashMap<u32, &UserFn> =
        user_fns.iter().map(|f| (f.wasm_idx, f)).collect();
    let mut certs = Vec::new();
    for (func_order, f) in user_fns.iter().enumerate() {
        let classified = classify_without_expr_fragment(
            f,
            box_idx,
            carrier,
            &user_idx_set,
            &fns,
            &ClassifierContext {
                host_roles: &host_roles,
                struct_field_counts: &struct_field_counts,
                model_ops: &model_ops,
            },
        );
        if let Ok(c) = classified {
            certs.push((func_order, c));
        }
    }
    let contracts = runtime_contracts_for_certs(certs.iter().map(|(_, c)| c));
    let obligations = certs
        .iter()
        .map(|(func_order, c)| RederivedObligation {
            name: c.name().to_string(),
            func_order: *func_order,
            code: render_code_value(c),
            self_idx: c.self_idx(),
            carrier: c.carrier(),
            policy: c.policy(),
            termination_witness: c.termination_witness(),
            fragment_type_idx: match c.inner() {
                Cert::ExprFragment { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            fragment_nlocals: match c.inner() {
                Cert::ExprFragment { nlocals, .. } => Some(*nlocals as u32),
                _ => None,
            },
            fragment_plan_lean: match c.inner() {
                Cert::ExprFragment { plan, .. } => Some(expr_fragment_plan_lean_value(plan)),
                _ => None,
            },
            fragment_sym_plan_lean: match c.inner() {
                Cert::ExprFragment {
                    source_plan, plan, ..
                } => expr_fragment_source_plan(source_plan, plan).map(|sym| sym_plan_lean_value(&sym)),
                _ => None,
            },
            fragment_struct_entries: match c.inner() {
                Cert::ExprFragment {
                    source_plan: Some(source_plan),
                    plan,
                    ..
                } => expr_fragment_struct_table_entries(source_plan, plan).unwrap_or_default(),
                _ => Vec::new(),
            },
            fragment_lowered_body_lean: match c.inner() {
                Cert::ExprFragment { ops, .. } => Some(render_ops_value(ops)),
                _ => None,
            },
            fragment_lowered_code_entry_lean: match c.inner() {
                Cert::ExprFragment { carrier, plan, .. } => {
                    lower_expr_fragment_plan_code_entry_bytes(plan, *carrier)
                        .ok()
                        .map(|bytes| render_byte_list(&bytes))
                }
                _ => None,
            },
            string_concat_plan_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch { .. } => {
                    string_concat_plan_from_cert(c).map(|plan| string_concat_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_concat_sym_plan_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch { .. } => {
                    string_concat_sym_plan_from_cert(c).map(|plan| sym_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_concat_type_idx: match c.inner() {
                Cert::StringConcatVerbatimMatch { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            string_concat_lowered_body_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch {
                    result_ty,
                    container_ty,
                    string_concat_idx,
                    ..
                } => string_concat_plan_from_cert(c)
                    .and_then(|plan| {
                        lower_string_concat_plan(
                            &plan,
                            *result_ty,
                            *container_ty,
                            *string_concat_idx,
                        )
                        .ok()
                    })
                    .map(|ops| render_ops_value(&ops)),
                _ => None,
            },
            string_concat_lowered_code_entry_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch {
                    carrier,
                    result_ty,
                    container_ty,
                    string_concat_idx,
                    ..
                } => string_concat_plan_from_cert(c)
                    .and_then(|plan| {
                        lower_string_concat_plan_code_entry_bytes(
                            &plan,
                            *carrier,
                            *result_ty,
                            *container_ty,
                            *string_concat_idx,
                        )
                        .ok()
                    })
                    .map(|bytes| render_byte_list(&bytes)),
                _ => None,
            },
            string_concat_result_ty: match c.inner() {
                Cert::StringConcatVerbatimMatch { result_ty, .. } => Some(*result_ty),
                _ => None,
            },
            string_concat_container_ty: match c.inner() {
                Cert::StringConcatVerbatimMatch { container_ty, .. } => Some(*container_ty),
                _ => None,
            },
            string_concat_func_idx: match c.inner() {
                Cert::StringConcatVerbatimMatch {
                    string_concat_idx, ..
                } => Some(*string_concat_idx),
                _ => None,
            },
            string_eq_plan_lean: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => {
                    string_eq_plan_from_cert(c).map(|plan| string_eq_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_eq_sym_plan_lean: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => {
                    string_eq_sym_plan_from_cert(c).map(|plan| sym_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_eq_type_idx: match c.inner() {
                Cert::StringEqVerbatimMatch { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            string_eq_lowered_body_lean: match c.inner() {
                Cert::StringEqVerbatimMatch { string_eq_idx, .. } => {
                    string_eq_string_ty_from_cert(c).and_then(|string_ty| {
                        string_eq_plan_from_cert(c)
                            .and_then(|plan| {
                                lower_string_eq_plan(&plan, string_ty, *string_eq_idx).ok()
                            })
                            .map(|ops| render_ops_value(&ops))
                    })
                }
                _ => None,
            },
            string_eq_lowered_code_entry_lean: match c.inner() {
                Cert::StringEqVerbatimMatch {
                    carrier,
                    string_eq_idx,
                    ..
                } => {
                    string_eq_string_ty_from_cert(c).and_then(|string_ty| {
                        string_eq_plan_from_cert(c)
                            .and_then(|plan| {
                                lower_string_eq_plan_code_entry_bytes(
                                    &plan,
                                    *carrier,
                                    string_ty,
                                    *string_eq_idx,
                                )
                                .ok()
                            })
                            .map(|bytes| render_byte_list(&bytes))
                    })
                }
                _ => None,
            },
            string_eq_string_ty: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => string_eq_string_ty_from_cert(c),
                _ => None,
            },
            string_eq_func_idx: match c.inner() {
                Cert::StringEqVerbatimMatch { string_eq_idx, .. } => Some(*string_eq_idx),
                _ => None,
            },
            construct_plan_lean: match c.inner() {
                Cert::AdtConstructor { .. } => {
                    construct_plan_from_cert(c).map(|plan| construct_plan_lean_value(&plan))
                }
                _ => None,
            },
            construct_sym_plan_lean: match c.inner() {
                Cert::AdtConstructor { .. } => adt_constructor_sym_plan_from_cert(c, &model_info)
                    .map(|plan| sym_plan_lean_value(&plan)),
                _ => None,
            },
            construct_type_idx: match c.inner() {
                Cert::AdtConstructor { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            construct_struct_idx: match c.inner() {
                Cert::AdtConstructor { struct_idx, .. } => Some(*struct_idx),
                _ => None,
            },
            construct_field_count: match c.inner() {
                Cert::AdtConstructor { field_count, .. } => Some(*field_count),
                _ => None,
            },
            construct_elem_ty_lean: match c.inner() {
                Cert::AdtConstructor { elem_ty, .. } => construct_val_type_lean_value(*elem_ty),
                _ => None,
            },
            construct_is_list: match c.inner() {
                Cert::AdtConstructor { .. } => adt_constructor_sym_plan_from_cert(c, &model_info)
                    .is_some_and(|plan| sym_plan_is_list_construct(&plan)),
                _ => false,
            },
            construct_lowered_body_lean: match c.inner() {
                Cert::AdtConstructor { struct_idx, .. } => construct_plan_from_cert(c)
                    .and_then(|plan| lower_construct_plan(&plan, *struct_idx).ok())
                    .map(|ops| render_ops_value(&ops)),
                _ => None,
            },
            construct_lowered_code_entry_lean: match c.inner() {
                Cert::AdtConstructor { carrier, struct_idx, .. } => construct_plan_from_cert(c)
                    .and_then(|plan| lower_construct_plan_code_entry_bytes(&plan, *carrier, *struct_idx).ok())
                    .map(|bytes| render_byte_list(&bytes)),
                _ => None,
            },
            recursion_plan_lean: recursion_plan_from_cert(c)
                .map(|plan| recursion_plan_lean_value(&plan)),
            recursion_host_table_lean: match c.inner() {
                Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. } => {
                    Some(recursion_host_table_lean_value(c))
                }
                _ => None,
            },
            recursion_type_idx: match c.inner() {
                Cert::Recursive { type_idx, .. } | Cert::AccumulatorRecursive { type_idx, .. } => {
                    Some(*type_idx)
                }
                _ => None,
            },
            recursion_lowered_body_lean: match c.inner() {
                Cert::Recursive { carrier, .. } | Cert::AccumulatorRecursive { carrier, .. } => {
                    recursion_plan_from_cert(c)
                        .and_then(|plan| lower_expr_fragment_plan(&plan, *carrier).ok())
                        .map(|ops| render_ops_value(&ops))
                }
                _ => None,
            },
            recursion_lowered_code_entry_lean: match c.inner() {
                Cert::Recursive { carrier, .. } | Cert::AccumulatorRecursive { carrier, .. } => {
                    recursion_plan_from_cert(c)
                        .and_then(|plan| {
                            lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier).ok()
                        })
                        .map(|bytes| render_byte_list(&bytes))
                }
                _ => None,
            },
            mutual_plan_lean: mutual_plan_from_cert(c).map(|plan| mutual_plan_lean_value(&plan)),
            mutual_host_table_lean: match c.inner() {
                Cert::MutualRecursion {
                    box_idx, sub_idx, ..
                } => Some(mutual_host_table_lean_value(*box_idx, *sub_idx)),
                _ => None,
            },
            mutual_member_set_lean: match c.inner() {
                Cert::MutualRecursion { scc, .. } => Some(mutual_member_set_lean_value(scc)),
                _ => None,
            },
            mutual_type_idx: match c.inner() {
                Cert::MutualRecursion { position, scc, .. } => {
                    scc.get(*position).map(|m| m.type_idx)
                }
                _ => None,
            },
            mutual_lowered_body_lean: match c.inner() {
                Cert::MutualRecursion { carrier, .. } => mutual_plan_from_cert(c)
                    .and_then(|plan| lower_expr_fragment_plan(&plan, *carrier).ok())
                    .map(|ops| render_ops_value(&ops)),
                _ => None,
            },
            mutual_lowered_code_entry_lean: match c.inner() {
                Cert::MutualRecursion { carrier, .. } => mutual_plan_from_cert(c)
                    .and_then(|plan| {
                        lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier).ok()
                    })
                    .map(|bytes| render_byte_list(&bytes)),
                _ => None,
            },
            verbatim_plan_lean: verbatim_plan_from_cert(c)
                .map(|plan| verbatim_plan_lean_value(&plan)),
            int_dispatch_plan_lean: int_dispatch_plan_from_cert(c, frag_host_table)
                .map(|plan| int_dispatch_plan_lean_value(&plan)),
            int_dispatch_host_table_lean: match int_dispatch_plan_from_cert(c, frag_host_table) {
                Some(_) => int_dispatch_host_table_from_cert(c)
                    .map(|hosts| int_dispatch_host_table_lean_value(&hosts)),
                None => None,
            },
            field_projection_plan_lean: field_projection_plan_from_cert(c)
                .map(|(plan, _)| field_projection_plan_lean_value(&plan)),
            field_projection_struct_idx: match c.inner() {
                Cert::FieldProjection { struct_idx, .. }
                    if field_projection_plan_from_cert(c).is_some() => Some(*struct_idx),
                _ => None,
            },
            field_projection_field_count: match c.inner() {
                Cert::FieldProjection { field_count, .. }
                    if field_projection_plan_from_cert(c).is_some() => Some(*field_count),
                _ => None,
            },
            field_projection_result_ty_lean: field_projection_plan_from_cert(c)
                .map(|(_, ty)| field_projection_result_ty_lean_value(ty)),
            composition_members: match c.inner() {
                Cert::Composition { carrier, closure, .. } => {
                    let funcs = composition_func_table(closure);
                    let add_idx = frag_host_table.add_idx;
                    composition_plans_from_cert(c, frag_host_table)
                        .unwrap_or_default()
                        .into_iter()
                        .filter_map(|(entry, plan)| {
                            let add_idx = add_idx?;
                            Some(RederivedCompositionMember {
                                name: entry.name,
                                self_idx: entry.self_idx,
                                type_idx: entry.type_idx,
                                plan_lean: composition_plan_lean_value(&plan),
                                lowered_body_lean: render_ops_value(
                                    &lower_composition_plan(&plan, add_idx, &funcs)?,
                                ),
                                lowered_code_entry_lean: render_byte_list(
                                    &composition_code_entry_bytes(
                                        &plan, *carrier, add_idx, &funcs,
                                    )?,
                                ),
                            })
                        })
                        .collect()
                }
                _ => Vec::new(),
            },
            composition_host_table_lean: match c.inner() {
                Cert::Composition { .. }
                    if composition_plans_from_cert(c, frag_host_table).is_some() =>
                {
                    frag_host_table.add_idx.map(composition_host_table_lean_value)
                }
                _ => None,
            },
            composition_member_names_lean: match c.inner() {
                Cert::Composition { closure, .. }
                    if composition_plans_from_cert(c, frag_host_table).is_some() =>
                {
                    Some(format!(
                        "[{}]",
                        closure
                            .iter()
                            .map(|entry| lean_str(&entry.name))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ))
                }
                _ => None,
            },
        })
        .collect();
    Ok(RederivedCertificate {
        obligations,
        contracts,
    })
}
