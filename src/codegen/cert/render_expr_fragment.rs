fn render_expr_fragment_cert(c: &Cert) -> String {
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
  intro S add sub mul stringEq stringConcat hadd hsub hmul hStringEq hStringConcat fuel p vs w hrepr hrun
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

fn expr_fragment_cod_repr(ty: FragTy) -> &'static str {
    match ty {
        FragTy::F64 => "AverCert.Schema.floatBitsRepr",
        FragTy::BoolI32 => "AverCert.Schema.boolRepr",
        FragTy::IntCarrier | FragTy::I64 | FragTy::RawI32 | FragTy::Ref => {
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
        FragTy::IntCarrier | FragTy::I64 | FragTy::RawI32 | FragTy::Ref => {
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
        | FragNodeKind::StructGet { .. } => unreachable!("node is not BoolI32"),
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
        .filter_map(|(i, ty)| (*ty == FragTy::BoolI32).then(|| format!("cases a{i}")))
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
                    | FragPrim::I64Eq
                    | FragPrim::I64LeS
                    | FragPrim::I64LtS
                    | FragPrim::I64GeS
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
