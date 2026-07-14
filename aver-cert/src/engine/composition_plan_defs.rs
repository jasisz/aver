// Byte-first `composition-plan-v1` builder.
//
// A plan names only the member SHAPE: a self-sum leaf, or an ordered chain of
// callee EXPORT NAMES. Numeric function indices are lowering context derived
// from each named export's Wasm `FuncBinding`; the plan never supplies them.
// Every member is lowered independently and matched byte-for-byte to its real
// code entry. The audited acceptance predicate then follows the call edges in
// those byte-bound plans from the root, rejecting cycles, missing targets, and
// extra members. Thus closure membership is a function of module bytes.


#[derive(Clone, PartialEq)]
enum CompositionPlanShape {
    SelfSum,
    Chain(Vec<String>),
}

#[derive(Clone, PartialEq)]
struct CompositionRawPlan {
    shape: CompositionPlanShape,
}

fn composition_plan_for_entry(
    entry: &ClosureEntry,
    names: &std::collections::HashMap<u32, String>,
) -> Option<CompositionRawPlan> {
    let shape = match &entry.shape {
        LeafShape::SelfSum { .. } => CompositionPlanShape::SelfSum,
        LeafShape::Chain { calls } => CompositionPlanShape::Chain(
            calls
                .iter()
                .map(|idx| names.get(idx).cloned())
                .collect::<Option<Vec<_>>>()?,
        ),
    };
    Some(CompositionRawPlan { shape })
}

fn composition_func_table(closure: &[ClosureEntry]) -> std::collections::HashMap<String, u32> {
    closure
        .iter()
        .map(|entry| (entry.name.clone(), entry.self_idx))
        .collect()
}

fn lower_composition_plan(
    plan: &CompositionRawPlan,
    add_idx: u32,
    funcs: &std::collections::HashMap<String, u32>,
) -> Option<Vec<Op>> {
    let mut ops = vec![Op::LocalGet(0)];
    match &plan.shape {
        CompositionPlanShape::SelfSum => {
            ops.push(Op::LocalGet(0));
            ops.push(Op::Call(add_idx));
        }
        CompositionPlanShape::Chain(callees) if !callees.is_empty() => {
            for callee in callees {
                ops.push(Op::Call(*funcs.get(callee)?));
            }
        }
        CompositionPlanShape::Chain(_) => return None,
    }
    Some(ops)
}

fn composition_code_entry_bytes(
    plan: &CompositionRawPlan,
    carrier: u32,
    add_idx: u32,
    funcs: &std::collections::HashMap<String, u32>,
) -> Option<Vec<u8>> {
    let ops = lower_composition_plan(plan, add_idx, funcs)?;
    let expr_plan = ExprFragmentPlan {
        params: vec![FragTy::IntCarrier],
        result: FragTy::IntCarrier,
        body: {
            let mut builder = RecBlockBuilder::new();
            let input = builder.push(FragTy::IntCarrier, FragNodeKind::Local { index: 0 });
            let result = match &plan.shape {
                CompositionPlanShape::SelfSum => {
                    let second = builder.push(
                        FragTy::IntCarrier,
                        FragNodeKind::Local { index: 0 },
                    );
                    builder.push(
                        FragTy::IntCarrier,
                        FragNodeKind::HostCall {
                            role: FragHostRole::Add,
                            func_idx: add_idx,
                            args: vec![input, second],
                        },
                    )
                }
                CompositionPlanShape::Chain(callees) => {
                    let mut value = input;
                    for callee in callees {
                        value = builder.push(
                            FragTy::IntCarrier,
                            FragNodeKind::SelfCall {
                                tail: false,
                                func_idx: *funcs.get(callee)?,
                                args: vec![value],
                            },
                        );
                    }
                    value
                }
            };
            builder.finish(result)
        },
    };
    let lowered_ops = lower_expr_fragment_plan(&expr_plan, carrier).ok()?;
    if lowered_ops != ops {
        return None;
    }
    lower_expr_fragment_plan_code_entry_bytes(&expr_plan, carrier).ok()
}

/// Rebuild every closure member from shape-only/name-only plan data. All
/// numeric call targets come from the closure's byte-derived export bindings;
/// every lowered entry must equal that member's real code-entry bytes and the
/// canonical lowering's exact one-local declaration.
fn composition_plans_from_cert(
    cert: &Cert,
    strict: FragHostTable,
) -> Option<Vec<(ClosureEntry, CompositionRawPlan)>> {
    let Cert::Composition {
        carrier, closure, ..
    } = cert.inner()
    else {
        return None;
    };
    let add_idx = strict.add_idx?;
    let names = closure
        .iter()
        .map(|entry| (entry.self_idx, entry.name.clone()))
        .collect::<std::collections::HashMap<_, _>>();
    let funcs = composition_func_table(closure);
    let mut out = Vec::new();
    for entry in closure {
        if let LeafShape::SelfSum { add_idx: actual } = &entry.shape
            && *actual != add_idx
        {
            return None;
        }
        let plan = composition_plan_for_entry(entry, &names)?;
        let bytes = composition_code_entry_bytes(&plan, *carrier, add_idx, &funcs)?;
        if bytes != entry.code_entry_bytes || entry.nlocals != 1 {
            return None;
        }
        out.push((entry.clone(), plan));
    }
    Some(out)
}

fn composition_member_plans(
    analysis: &Analysis,
) -> Vec<(ClosureEntry, CompositionRawPlan)> {
    let mut members = std::collections::BTreeMap::<
        String,
        (ClosureEntry, CompositionRawPlan),
    >::new();
    for cert in &analysis.certs {
        let Some(plans) = composition_plans_from_cert(cert, analysis.frag_host_table) else {
            continue;
        };
        for (entry, plan) in plans {
            match members.get(&entry.name) {
                Some((existing_entry, existing_plan)) => {
                    if existing_entry.self_idx != entry.self_idx || existing_plan != &plan {
                        return Vec::new();
                    }
                }
                None => {
                    members.insert(entry.name.clone(), (entry, plan));
                }
            }
        }
    }
    members.into_values().collect()
}

fn composition_plan_lean_value(plan: &CompositionRawPlan) -> String {
    let shape = match &plan.shape {
        CompositionPlanShape::SelfSum => ".selfSum".to_string(),
        CompositionPlanShape::Chain(callees) => format!(
            ".chain [{}]",
            callees
                .iter()
                .map(|name| lean_str(name))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    };
    format!("{{ profile := \"composition-plan-v1\", shape := {shape} }}")
}

fn composition_host_table_lean_value(add_idx: u32) -> String {
    format!("[(.add, {add_idx})]")
}

fn composition_func_table_lean_value(closure: &[ClosureEntry]) -> String {
    format!(
        "[{}]",
        closure
            .iter()
            .map(|entry| format!("({}, {})", lean_str(&entry.name), entry.self_idx))
            .collect::<Vec<_>>()
            .join(", ")
    )
}
