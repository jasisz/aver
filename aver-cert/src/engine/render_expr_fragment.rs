fn render_expr_fragment_cert(c: &Cert) -> String {
    // Audited integer/Bool source fragments emit only their option-(b)
    // semantic bridge in `Certificate.lean`.
    if expr_fragment_uses_audited_generic(c) {
        return String::new();
    }
    if let Some(face) = c.int_add_face() {
        return render_expr_fragment_int_add_cert(c, face);
    }
    // Projection-faced fragments are discharged in `Final.cert` through the
    // audited direct-projection generic and emit no bespoke proof declarations.
    if c.project_face().is_some() {
        return String::new();
    }
    if let Some(face) = c.vector_get_face() {
        return render_expr_fragment_vector_get_cert(c, face);
    }
    let c = c.inner();
    let Cert::ExprFragment {
        name,
        self_idx,
        carrier,
        plan,
        ..
    } = c
    else {
        unreachable!()
    };
    let binders = plan
        .params
        .iter()
        .enumerate()
        .map(|(i, ty)| format!("(a{i} : {})", ty.lean_dom_type()))
        .collect::<Vec<_>>()
        .join(" ");
    let theorem_args = (0..plan.params.len())
        .map(|i| format!("a{i}"))
        .collect::<Vec<_>>();
    let input_list = expr_fragment_arg_list(plan, |i, ty| {
        ty.lean_arg_repr(&format!("a{i}"), &carrier.to_string())
    });
    let result = expr_fragment_wval_expr(plan, &|idx, _ty| format!("a{idx}"));
    let evalset = format!("wFuncN, wRunF, {name}Code, {name}Host, f, b32, popArgs, initLocals");
    let proof_tactic = expr_fragment_simp_tactic(plan, &evalset);
    let model_args = (0..plan.params.len())
        .map(|i| expr_fragment_dom_accessor("p", i, plan.params.len()))
        .collect::<Vec<_>>()
        .join(" ");
    let model_args = if model_args.is_empty() {
        String::new()
    } else {
        format!(" {model_args}")
    };
    let cod_repr = expr_fragment_cod_repr(plan.result);
    format!(
        r#"/-! ### {name} — expr-fragment-v1 certificate (carrier type {carrier}) -/

/-- The verifier-checked plan is an `expr-fragment-v1`: a typed, ordered,
    non-recursive wasm fragment with no runtime host calls. -/
theorem {name}_wasm_certified (S : CarrierSpec {carrier}) :
    ∀ (fuel : Nat) {binders},
      wFuncN {name}Code {name}Host (fuel + 1) {self_idx} {input_list}
        = some ({result}) := by
  intro fuel {intro_args}
{proof_tactic}

#print axioms {name}_wasm_certified

def {name}HostRef : HostTbl := {name}Host

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat toIndex hadd hsub hmul hStringEq hStringConcat _hToIndex fuel p vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      rw [{name}_wasm_certified S f{model_args}] at hrun
      simp only [Option.some.injEq] at hrun
      subst hrun
      simp [{cod_repr}]
"#,
        intro_args = theorem_args.join(" "),
    )
}

/// The fused vector-read proof: the obligation discharges through the wall's
/// generic template theorem, specialised to this claim's byte-derived holes
/// and the plan-lowered code table. The to-index contract hypothesis comes
/// straight from the obligation denotation's sixth host-contract slot.
fn render_expr_fragment_vector_get_cert(c: &Cert, face: FragVectorGetOrDefaultFace) -> String {
    let name = c.name();
    let carrier = c.carrier();
    let self_idx = c.self_idx();
    format!(
        r#"/-! ### {name} — fused vector-read certificate (carrier type {carrier}) -/

theorem {name}_simulates : AverCert.Schema.Obligation.holds AverCert.{name}Ob := by
  intro S add sub mul stringEq stringConcat toIndex _hadd _hsub _hmul _hStringEq
    _hStringConcat hToIndex fuel p vs w hDom hRun
  obtain ⟨v, i⟩ := p
  exact AverCert.StandardFace.vectorGetOrDefault_simulates_model
    {carrier} {to_index_idx} {box_idx} {arr_ty} ({d} : Int) (by decide) S toIndex hToIndex
    CertModule.{name}Code {self_idx} rfl fuel v i vs w hDom hRun

#print axioms {name}_simulates
"#,
        to_index_idx = face.to_index_idx,
        box_idx = face.box_idx,
        arr_ty = face.arr_ty,
        d = face.default,
    )
}

/// The straight-line integer proof face for a host-call expr fragment
/// `add(param0, box(k))`: the SAME theorem shapes (and closer) the legacy
/// straight-line class ships, stated over the plan-lowered code table. The
/// obligation covers ALL representations under the named add contract — no
/// weakening relative to the legacy class.
fn render_expr_fragment_int_add_cert(c: &Cert, face: FragIntAddFace) -> String {
    let name = c.name();
    let self_idx = c.self_idx();
    let carrier = c.carrier();
    let k = face.k;
    let g1 = k + 3;
    let g2 = k - 5;
    format!(
        r#"/-! ### {name} — expr-fragment-v1 certificate (carrier type {carrier}, straight-line integer face) -/

/-- The plan-lowered body of `{name}` maps any representation of `n` to a
    representation of `n + {k}`, for ALL `n : ℤ`, under the named runtime
    contract `hadd` (carrier add = exact integer addition on represented values). -/
theorem {name}_wasm_certified
    (S : ReprSpec {carrier})
    (add : List WVal → Option WVal)
    (hadd : ∀ a b va vb, S.Repr a va → S.Repr b vb →
          ∃ w, add [va, vb] = some w ∧ S.Repr (a + b) w) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w, wFuncN {name}Code ({name}Host add) 1 {self_idx} [v] = some w ∧ S.Repr (n + {k}) w := by
  intro n v hv
  obtain ⟨w, hw, hrepr⟩ := hadd n {k} v (carrierSmall {carrier} {k}) hv (S.smallIntro {k})
  refine ⟨w, ?_, hrepr⟩
  simp only [wFuncN, {name}Code, {name}Host, boxRef, carrierSmall, initLocals,
    wRunF, popArgs, List.getElem?_cons_zero, List.length, List.take, List.drop,
    List.reverse, List.replicate, if_true, reduceIte]
  simp only [carrierSmall] at hw
  simp [hw]

#print axioms {name}_wasm_certified

/-- Consumer-facing composition: whatever the bytes return represents the
    model value `n + {k}` (faithfulness law ∘ simulation). -/
theorem {name}_wasm_faithful
    (S : ReprSpec {carrier})
    (add : List WVal → Option WVal)
    (hadd : ∀ a b va vb, S.Repr a va → S.Repr b vb →
          ∃ w, add [va, vb] = some w ∧ S.Repr (a + b) w) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w m, wFuncN {name}Code ({name}Host add) 1 {self_idx} [v] = some w ∧ S.Repr m w ∧ m = n + {k} :=
  fun n v hv =>
    let ⟨w, hrun, hrepr⟩ := {name}_wasm_certified S add hadd n v hv
    ⟨w, n + {k}, hrun, hrepr, rfl⟩

#print axioms {name}_wasm_faithful

-- anti-vacuity: the emitted body actually RUNS on concrete inputs.
def {name}HostRef : HostTbl := {name}Host (addRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt)
      = some ({g1}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} (-5)]).bind carrierToInt)
      = some ({g2}) := by native_decide

/-- Schema-shaped simulation obligation for `{name}` (composed by the single
    final theorem). Partial correctness over any fuel and representation. -/
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq stringConcat toIndex hadd hsub hmul hStringEq hStringConcat _hToIndex fuel ns vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hv htail =>
    rename_i n v ns vs
    cases htail with
    | nil =>
      cases fuel with
      | zero => simp only [wFuncN, reduceCtorEq] at hrun
      | succ f =>
        rcases hc : add [v, carrierSmall {carrier} ({k})] with _ | r
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun
          subst hrun
          simpa [AverCert.Schema.intRepr] using hadd n ({k}) v (carrierSmall {carrier} ({k})) r hv (S.smallIntro ({k})) hc
    | cons _ _ =>
      simp at harity
"#
    )
}

fn expr_fragment_cod_repr(ty: FragTy) -> &'static str {
    match ty {
        FragTy::F64 => "AverCert.Schema.floatBitsRepr",
        FragTy::BoolI32 => "AverCert.Schema.boolRepr",
        FragTy::IntCarrier | FragTy::I64 | FragTy::RawI32 | FragTy::Ref | FragTy::AdtRef => {
            "AverCert.Schema.verbatimRepr"
        }
    }
}

fn expr_fragment_arg_list<F>(plan: &ExprFragmentPlan, mut arg: F) -> String
where
    F: FnMut(usize, FragTy) -> String,
{
    let args = plan
        .params
        .iter()
        .enumerate()
        .map(|(i, ty)| arg(i, *ty))
        .collect::<Vec<_>>();
    format!("[{}]", args.join(", "))
}

fn expr_fragment_wval_expr<F>(plan: &ExprFragmentPlan, local: &F) -> String
where
    F: Fn(u32, FragTy) -> String,
{
    let root = plan.body.node(plan.body.result).expect("fragment root exists");
    let value = expr_fragment_value_expr(&plan.body, plan.body.result, local);
    match root.ty {
        FragTy::F64 => format!(".f64v ({value})"),
        FragTy::BoolI32 => format!("b32 ({value})"),
        FragTy::IntCarrier | FragTy::I64 | FragTy::RawI32 | FragTy::Ref | FragTy::AdtRef => {
            unreachable!("expr-fragment root must be a certified result type")
        }
    }
}

fn expr_fragment_value_expr<F>(block: &FragBlock, id: FragValueId, local: &F) -> String
where
    F: Fn(u32, FragTy) -> String,
{
    let node = block.node(id).expect("fragment node exists");
    match node.ty {
        FragTy::BoolI32 => expr_fragment_bool_expr(block, id, local),
        FragTy::RawI32 => expr_fragment_i32_expr(block, id, local),
        _ => expr_fragment_typed_value_expr(block, id, local),
    }
}

fn expr_fragment_typed_value_expr<F>(block: &FragBlock, id: FragValueId, local: &F) -> String
where
    F: Fn(u32, FragTy) -> String,
{
    let node = block.node(id).expect("fragment node exists");
    match &node.kind {
        FragNodeKind::Local { index } => local(*index, node.ty),
        FragNodeKind::ConstBool(v) => v.to_string(),
        FragNodeKind::ConstI64(k) => lean_int_lit(*k),
        FragNodeKind::ConstI32(k) => k.to_string(),
        FragNodeKind::ConstF64(bits) => lean_u64_hex(*bits),
        FragNodeKind::StructGet {
            field, receiver, ..
        } => {
            let recv = expr_fragment_value_expr(block, *receiver, local);
            match field {
                0 => recv,
                // `expr-fragment-v1`'s Int face is stated over canonical
                // `carrierSmall`, whose limbs field is null and sign is 0.
                1 => "WVal.null".to_string(),
                2 => "0".to_string(),
                _ => unreachable!("unsupported carrier field in fragment"),
            }
        }
        FragNodeKind::StructGetUser { .. } => {
            unreachable!("user struct projection is rendered by the projection face, not the generic value renderer")
        }
        FragNodeKind::VectorGetOrDefault { .. } => {
            unreachable!("the fused vector read is rendered by its own face, not the generic value renderer")
        }
        FragNodeKind::RefIsNull { value } => {
            let v = expr_fragment_value_expr(block, *value, local);
            format!("{v} = WVal.null")
        }
        FragNodeKind::Prim { op, args } => {
            match op {
                FragPrim::F64Add => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("(Float.ofBits ({lhs}) + Float.ofBits ({rhs})).toBits")
                }
                FragPrim::F64Mul => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("(Float.ofBits ({lhs}) * Float.ofBits ({rhs})).toBits")
                }
                FragPrim::F64Le => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("Float.ofBits ({lhs}) <= Float.ofBits ({rhs})")
                }
                // `f64.ge` is stated as the swapped `<=`, exactly the shape the
                // wall interpreter gives `.f64Ge` (`b32 (f b <= f a)`) and the
                // same convention `i64.ge_s` already uses. It is an IEEE ordered
                // comparison in both directions, so a NaN operand makes it false
                // — the identical NaN posture as `f64.le`.
                FragPrim::F64Ge => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("Float.ofBits ({rhs}) <= Float.ofBits ({lhs})")
                }
                FragPrim::F64Lt => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("Float.ofBits ({lhs}) < Float.ofBits ({rhs})")
                }
                // `f64.gt` is stated as the swapped `<`, matching the wall
                // interpreter's `.f64Gt` clause (`b32 (f b < f a)`) and the
                // swapped-relation convention `f64.ge`/`i64.ge_s` already use.
                FragPrim::F64Gt => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("Float.ofBits ({rhs}) < Float.ofBits ({lhs})")
                }
                // `f64.eq` is stated with Bool-valued `==` (`Float.beq`, the
                // extern IEEE comparison), NEVER with propositional `=`:
                // `Float` has no lawful `DecidableEq`, and `Float.ofBits b =
                // Float.ofBits b` holds for a NaN pattern while `f64.eq`
                // returns 0 for it. `==` is exactly the wall interpreter's
                // `.f64Eq` clause (`b32 (f a == f b)`).
                FragPrim::F64Eq => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("Float.ofBits ({lhs}) == Float.ofBits ({rhs})")
                }
                FragPrim::I64Eq => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("({lhs}) = ({rhs})")
                }
                FragPrim::I64LeS => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("({lhs}) <= ({rhs})")
                }
                FragPrim::I64LtS => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("({lhs}) < ({rhs})")
                }
                FragPrim::I64GeS => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("({rhs}) <= ({lhs})")
                }
                // Swapped `<`, the same convention `i64.ge_s` uses for `>=`,
                // matching the wall interpreter's `.i64GtS` clause.
                FragPrim::I64GtS => {
                    let lhs = expr_fragment_value_expr(block, args[0], local);
                    let rhs = expr_fragment_value_expr(block, args[1], local);
                    format!("({rhs}) < ({lhs})")
                }
                FragPrim::I32Eq => {
                    let lhs = expr_fragment_i32_expr(block, args[0], local);
                    let rhs = expr_fragment_i32_expr(block, args[1], local);
                    format!("(({lhs}) = ({rhs}))")
                }
                FragPrim::I32LtS => {
                    let lhs = expr_fragment_i32_expr(block, args[0], local);
                    let rhs = expr_fragment_i32_expr(block, args[1], local);
                    format!("({lhs}) < ({rhs})")
                }
                FragPrim::I32GtS => {
                    let lhs = expr_fragment_i32_expr(block, args[0], local);
                    let rhs = expr_fragment_i32_expr(block, args[1], local);
                    format!("({lhs}) > ({rhs})")
                }
            }
        }
        FragNodeKind::HostCall { .. } => {
            unreachable!("host-free expr-fragment value renderer does not handle host calls")
        }
        FragNodeKind::SelfCall { .. } => {
            unreachable!("self-call is rendered by the fuel-recursion face, not the generic value renderer")
        }
        FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } => {
            let c = expr_fragment_bool_expr(block, *cond, local);
            let t = expr_fragment_value_expr(then_block, then_block.result, local);
            let e = expr_fragment_value_expr(else_block, else_block.result, local);
            format!("if ({c}) then ({t}) else ({e})")
        }
    }
}

fn expr_fragment_bool_expr<F>(block: &FragBlock, id: FragValueId, local: &F) -> String
where
    F: Fn(u32, FragTy) -> String,
{
    let node = block.node(id).expect("fragment node exists");
    match &node.kind {
        FragNodeKind::Local { index } => local(*index, node.ty),
        FragNodeKind::ConstBool(v) => v.to_string(),
        FragNodeKind::RefIsNull { value } => {
            if matches!(
                block.node(*value).map(|node| &node.kind),
                Some(FragNodeKind::StructGet { field: 1, .. })
            ) {
                "true".to_string()
            } else {
                let v = expr_fragment_value_expr(block, *value, local);
                format!("{v} = WVal.null")
            }
        }
        FragNodeKind::Prim { op, args } => match op {
            FragPrim::F64Le => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("Float.ofBits ({lhs}) <= Float.ofBits ({rhs})")
            }
            FragPrim::F64Ge => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("Float.ofBits ({rhs}) <= Float.ofBits ({lhs})")
            }
            FragPrim::F64Lt => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("Float.ofBits ({lhs}) < Float.ofBits ({rhs})")
            }
            FragPrim::F64Gt => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("Float.ofBits ({rhs}) < Float.ofBits ({lhs})")
            }
            FragPrim::F64Eq => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("Float.ofBits ({lhs}) == Float.ofBits ({rhs})")
            }
            FragPrim::I64Eq => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("({lhs}) = ({rhs})")
            }
            FragPrim::I64LeS => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("({lhs}) <= ({rhs})")
            }
            FragPrim::I64LtS => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("({lhs}) < ({rhs})")
            }
            FragPrim::I64GeS => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("({rhs}) <= ({lhs})")
            }
            FragPrim::I64GtS => {
                let lhs = expr_fragment_value_expr(block, args[0], local);
                let rhs = expr_fragment_value_expr(block, args[1], local);
                format!("({rhs}) < ({lhs})")
            }
            FragPrim::I32Eq => {
                let lhs = expr_fragment_i32_expr(block, args[0], local);
                let rhs = expr_fragment_i32_expr(block, args[1], local);
                format!("decide (({lhs}) = ({rhs}))")
            }
            FragPrim::I32LtS => {
                let lhs = expr_fragment_i32_expr(block, args[0], local);
                let rhs = expr_fragment_i32_expr(block, args[1], local);
                format!("({lhs}) < ({rhs})")
            }
            FragPrim::I32GtS => {
                let lhs = expr_fragment_i32_expr(block, args[0], local);
                let rhs = expr_fragment_i32_expr(block, args[1], local);
                format!("({lhs}) > ({rhs})")
            }
            FragPrim::F64Add | FragPrim::F64Mul => {
                unreachable!("numeric primitive cannot produce BoolI32")
            }
        },
        FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } => {
            let c = expr_fragment_bool_expr(block, *cond, local);
            let t = expr_fragment_bool_expr(then_block, then_block.result, local);
            let e = expr_fragment_bool_expr(else_block, else_block.result, local);
            format!("if ({c}) then ({t}) else ({e})")
        }
        FragNodeKind::ConstI64(_)
        | FragNodeKind::ConstI32(_)
        | FragNodeKind::ConstF64(_)
        | FragNodeKind::HostCall { .. }
        | FragNodeKind::SelfCall { .. }
        | FragNodeKind::StructGet { .. }
        | FragNodeKind::StructGetUser { .. }
        | FragNodeKind::VectorGetOrDefault { .. } => unreachable!("node is not BoolI32"),
    }
}

fn expr_fragment_i32_expr<F>(block: &FragBlock, id: FragValueId, local: &F) -> String
where
    F: Fn(u32, FragTy) -> String,
{
    let node = block.node(id).expect("fragment node exists");
    match node.ty {
        FragTy::RawI32 => expr_fragment_typed_value_expr(block, id, local),
        FragTy::BoolI32 => {
            let b = expr_fragment_bool_expr(block, id, local);
            format!("if ({b}) then (1 : Int) else (0 : Int)")
        }
        _ => unreachable!("node is not an i32 value"),
    }
}

fn expr_fragment_simp_tactic(plan: &ExprFragmentPlan, evalset: &str) -> String {
    let mut steps = plan
        .params
        .iter()
        .enumerate()
        .filter(|(_, ty)| **ty == FragTy::BoolI32)
        .map(|(i, _)| format!("cases a{i}"))
        .collect::<Vec<_>>();
    let mut conds = Vec::new();
    collect_expr_fragment_conditions(&plan.body, &|idx, _ty| format!("a{idx}"), &mut conds);
    for (i, cond) in conds.iter().enumerate() {
        steps.push(format!("by_cases h{i} : {cond}"));
    }
    let hints = if conds.is_empty() {
        String::new()
    } else {
        format!(
            ", {}",
            (0..conds.len())
                .map(|i| format!("h{i}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    };
    if steps.is_empty() {
        format!("  simp [{evalset}, carrierSmall, ge_iff_le]")
    } else {
        format!(
            "  {} <;> simp [{evalset}, carrierSmall, ge_iff_le{hints}]",
            steps.join(" <;> ")
        )
    }
}

fn collect_expr_fragment_conditions<F>(block: &FragBlock, local: &F, out: &mut Vec<String>)
where
    F: Fn(u32, FragTy) -> String,
{
    for node in &block.nodes {
        match &node.kind {
            FragNodeKind::Prim {
                op:
                    FragPrim::F64Le
                    | FragPrim::F64Ge
                    | FragPrim::F64Lt
                    | FragPrim::F64Gt
                    | FragPrim::F64Eq
                    | FragPrim::I64Eq
                    | FragPrim::I64LeS
                    | FragPrim::I64LtS
                    | FragPrim::I64GeS
                    | FragPrim::I64GtS
                    | FragPrim::I32Eq
                    | FragPrim::I32LtS
                    | FragPrim::I32GtS,
                ..
            } => {
                let cond = expr_fragment_value_expr(block, node.id, local);
                if !out.contains(&cond) {
                    out.push(cond);
                }
            }
            FragNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                collect_expr_fragment_conditions(then_block, local, out);
                collect_expr_fragment_conditions(else_block, local, out);
            }
            _ => {}
        }
    }
}

fn lean_u64_hex(bits: u64) -> String {
    format!("0x{bits:016x}")
}
