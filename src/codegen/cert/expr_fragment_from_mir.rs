/// Whether a representation plan has a rendered proof face. `IntCarrier`
/// results are admitted ONLY through the exact straight-line integer face
/// (`add(param0, box(k))`): a bare Int passthrough or any other
/// carrier-returning shape has no coherent face yet (its `Cod` would read as
/// `Int` while `codRepr` falls to the verbatim `WVal` relation), so it must
/// never be selected here nor accepted by the verifier. Likewise, `AdtRef`
/// values are admitted ONLY through the exact field-projection face: any other
/// ADT-ref plan (a bare reference passthrough, a projection chain) stays
/// unplanned here and declines fail-closed at the verifier.
fn expr_fragment_plan_has_face(plan: &ExprFragmentPlan) -> bool {
    let tag_dispatch = expr_fragment_is_tag_dispatch(plan);
    let vector_get = expr_fragment_vector_get_face(plan).is_some();
    let int_face_ok = plan.result != FragTy::IntCarrier
        || expr_fragment_int_add_face(plan).is_some()
        || tag_dispatch
        || vector_get;
    let adt_face_ok = !expr_fragment_plan_touches_adt_ref(plan)
        || expr_fragment_project_face(plan).is_some()
        || tag_dispatch
        || vector_get;
    int_face_ok && adt_face_ok
}

/// Producer-side record layout resolver: `(record type name, field name) ->
/// (declared field index, field source type name)`. Backed by the wasm-gc
/// emitter's type registry; returns `None` for newtypes/carriers (whose field
/// reads are identity, not `struct.get`), so those never plan a projection.
pub(crate) type RecordFieldLookup<'a> = &'a dyn Fn(&str, &str) -> Option<(u32, String)>;

pub(crate) fn fragment_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
    record_fields: RecordFieldLookup,
    builtins: &[String],
) -> Option<FragmentPlan> {
    // Encodability gate at MIR time uses placeholder host/struct tables
    // (indices do not affect encoding SHAPE) and requires full canonical BYTE
    // lowering to succeed, because the wasm-gc emitter emits the function body
    // from this plan: a plan that cannot byte-lower must never be selected.
    // The face gate mirrors the verifier: carrier-returning plans without the
    // straight-line integer face, and ADT-ref plans without the
    // field-projection face, stay unplanned (fail-closed decline there,
    // untouched bytes here).
    if let Some(plan) = sym_plan_from_mir_fn(mir_fn, record_fields, builtins)
        && let Some(frag) = plan.to_expr_fragment_plan(
            &FragHostTable::placeholder(),
            &FragStructTable::placeholder_for(&plan),
        )
        && lower_expr_fragment_plan_code_entry_bytes(&frag, 0).is_ok()
        && expr_fragment_plan_has_face(&frag)
    {
        return Some(FragmentPlan::Sym(plan));
    }
    let plan = repr_expr_fragment_plan_from_mir_fn(mir_fn)?;
    if lower_expr_fragment_plan_code_entry_bytes(&plan, 0).is_ok()
        && expr_fragment_plan_has_face(&plan)
    {
        Some(FragmentPlan::Expr(plan))
    } else {
        None
    }
}

#[cfg(test)]
pub(crate) fn expr_fragment_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
) -> Option<ExprFragmentPlan> {
    let plan = fragment_plan_from_mir_fn(mir_fn, &|_, _| None, &[])?;
    let struct_table = match &plan {
        FragmentPlan::Sym(sym) => FragStructTable::placeholder_for(sym),
        FragmentPlan::Expr(_) => FragStructTable::default(),
    };
    plan.to_expr_fragment_plan(&FragHostTable::placeholder(), &struct_table)
}

/// Fail-closed representation guard. The plan lowerer emits the ALL-BOXED
/// representation: every Int value is a `$AverInt` carrier ref and every Int
/// param read becomes a carrier `struct.get`. The wasm signature, however, is
/// derived from `MirFnRepr` (`param_types_with_repr` / `return_results_with_repr`
/// with `ENABLE_BARE_SLOTS` on), so a fn whose repr marks any bare param /
/// return / let slot (or an ETAP-2 bare-carrier slot) carries scalar `i64`
/// slots — a plan-emitted body over those slots would `struct.get` an `i64`,
/// invalid wasm. Such functions must never plan; their MIR-emitted bodies
/// already handle the bare representation.
fn mir_fn_repr_is_all_boxed(mir_fn: &crate::ir::mir::MirFn) -> bool {
    let repr = &mir_fn.repr;
    !repr.bare_return
        && repr.bare_slots.is_empty()
        && repr.carrier_slots.is_empty()
        && repr.bare_params.iter().all(|bare| !bare)
}

pub(crate) fn sym_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
    record_fields: RecordFieldLookup,
    builtins: &[String],
) -> Option<SymPlan> {
    if !mir_fn.effects.is_empty() || !mir_fn_repr_is_all_boxed(mir_fn) {
        return None;
    }

    let mut params = Vec::new();
    let mut params_by_slot = std::collections::HashMap::<u32, (u32, SymTy)>::new();
    for (idx, param) in mir_fn.params.iter().enumerate() {
        if param.local.0 != idx as u32 {
            return None;
        }
        let ty = sym_ty_from_mir_name(&param.ty)?;
        params.push(ty.clone());
        params_by_slot.insert(param.local.0, (idx as u32, ty));
    }

    let result = sym_ty_from_mir_name(&mir_fn.return_type)?;
    let mut builder = MirSymPlanBuilder {
        params_by_slot: &params_by_slot,
        record_fields,
        builtins,
        aliases: std::collections::HashMap::new(),
        alias_hops: 0,
        nodes: Vec::new(),
    };
    let (root, root_ty) = builder.lower_expr(&mir_fn.body)?;
    if root_ty != result {
        return None;
    }
    Some(SymPlan {
        params,
        result,
        body: builder.finish(root)?,
    })
}

fn repr_expr_fragment_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
) -> Option<ExprFragmentPlan> {
    if !mir_fn.effects.is_empty() || !mir_fn_repr_is_all_boxed(mir_fn) {
        return None;
    }

    let mut params = Vec::new();
    let mut params_by_slot = std::collections::HashMap::<u32, (u32, FragTy)>::new();
    for (idx, param) in mir_fn.params.iter().enumerate() {
        if param.local.0 != idx as u32 {
            return None;
        }
        let ty = expr_fragment_ty_from_mir_name(&param.ty)?;
        params.push(ty);
        params_by_slot.insert(param.local.0, (idx as u32, ty));
    }

    let result = expr_fragment_ty_from_mir_name(&mir_fn.return_type)?;
    let mut builder = MirExprFragmentBuilder {
        params_by_slot: &params_by_slot,
        nodes: Vec::new(),
    };
    let (root, root_ty) = builder.lower_expr(&mir_fn.body)?;
    if root_ty != result {
        return None;
    }
    Some(ExprFragmentPlan {
        params,
        result,
        body: builder.finish(root)?,
    })
}

fn sym_ty_from_mir_name(ty: &str) -> Option<SymTy> {
    let ty = ty.trim();
    match ty {
        "Int" => Some(SymTy::Int),
        "Float" => Some(SymTy::Float),
        "Bool" => Some(SymTy::Bool),
        // MIR carries the resolver type's Debug rendering: the source String
        // type prints as `Str`.
        "String" | "Str" => Some(SymTy::String),
        "" => None,
        other => {
            if let Some(inner) = other
                .strip_prefix("Option(")
                .and_then(|value| value.strip_suffix(')'))
            {
                return Some(SymTy::App(
                    "Option".to_string(),
                    vec![sym_ty_from_mir_name(inner)?],
                ));
            }
            if let Some(inner) = other
                .strip_prefix("Vector(")
                .and_then(|value| value.strip_suffix(')'))
            {
                return Some(SymTy::App(
                    "Vector".to_string(),
                    vec![sym_ty_from_mir_name(inner)?],
                ));
            }
            if let Some(inner) = other
                .strip_prefix("Result(")
                .and_then(|value| value.strip_suffix(')'))
            {
                let (ok, err) = split_mir_debug_binary_args(inner)?;
                return Some(SymTy::App(
                    "Result".to_string(),
                    vec![sym_ty_from_mir_name(ok)?, sym_ty_from_mir_name(err)?],
                ));
            }
            // User records/ADTs print as `Named { id: ..., name: "User" }`;
            // extract the source name. Anything else keeps the pre-existing
            // whole-token fallback.
            if let Some(name) = mir_named_type_name(other) {
                return Some(SymTy::Named(name));
            }
            Some(SymTy::Named(other.to_string()))
        }
    }
}

fn split_mir_debug_binary_args(value: &str) -> Option<(&str, &str)> {
    let mut depth = 0i32;
    for (at, ch) in value.char_indices() {
        match ch {
            '(' | '<' | '[' | '{' => depth += 1,
            ')' | '>' | ']' | '}' => depth -= 1,
            ',' if depth == 0 => {
                return Some((value[..at].trim(), value[at + 1..].trim()));
            }
            _ => {}
        }
    }
    None
}

/// Extract the source type name from the resolver `Type::Named` Debug
/// rendering (`Named { id: ..., name: "User" }`). Returns `None` for any
/// other shape or a non-canonical name token.
fn mir_named_type_name(ty: &str) -> Option<String> {
    let rest = ty.strip_prefix("Named {")?.strip_suffix('}')?;
    let idx = rest.find("name: \"")?;
    let after = &rest[idx + 7..];
    let name = &after[..after.find('"')?];
    if !name.is_empty() && !name.chars().any(char::is_whitespace) && !name.contains('=') {
        Some(name.to_string())
    } else {
        None
    }
}

fn expr_fragment_ty_from_mir_name(ty: &str) -> Option<FragTy> {
    match ty.trim() {
        "Float" => Some(FragTy::F64),
        "Bool" => Some(FragTy::BoolI32),
        "Int" => Some(FragTy::IntCarrier),
        _ => None,
    }
}

/// Depth guard for let-alias resolution: a chain deeper than this refuses to
/// plan instead of recursing further. Well-formed MIR let chains are acyclic,
/// so any real program resolves in a handful of hops; the cap only exists so
/// pathological nesting degrades to an unplanned fn.
const MAX_ALIAS_RESOLUTION_DEPTH: usize = 32;

/// Number of reads of `slot` inside `expr`: `Local` reads plus first-class
/// fn-value calls through the slot (`MirCallee::LocalSlot`). `Let` bindings
/// and pattern bindings are definitions, not reads; MIR mints locals fresh
/// per binding, so no shadowing can hide a read from this count.
fn count_slot_reads(expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>, slot: u32) -> usize {
    use crate::ir::mir::MirExpr;
    let sum_all =
        |items: &[crate::ast::Spanned<MirExpr>]| -> usize {
            items.iter().map(|item| count_slot_reads(item, slot)).sum()
        };
    match &expr.node {
        MirExpr::Literal(_) | MirExpr::FnValue(_) => 0,
        MirExpr::Local(local) => usize::from(local.node.slot.0 == slot),
        MirExpr::Let(l) => {
            count_slot_reads(&l.node.value, slot) + count_slot_reads(&l.node.body, slot)
        }
        MirExpr::Call(call) => {
            let callee_reads = match &call.node.callee {
                crate::ir::mir::MirCallee::LocalSlot {
                    slot: callee_slot, ..
                } => usize::from(u32::from(*callee_slot) == slot),
                _ => 0,
            };
            callee_reads + sum_all(&call.node.args)
        }
        MirExpr::TailCall(tail_call) => sum_all(&tail_call.node.args),
        MirExpr::BinOp(binop) => {
            count_slot_reads(&binop.node.lhs, slot) + count_slot_reads(&binop.node.rhs, slot)
        }
        MirExpr::Neg(inner)
        | MirExpr::Try(inner)
        | MirExpr::Return(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner) => count_slot_reads(inner, slot),
        MirExpr::Match(m) => {
            count_slot_reads(&m.node.subject, slot)
                + m.node
                    .arms
                    .iter()
                    .map(|arm| count_slot_reads(&arm.body, slot))
                    .sum::<usize>()
        }
        MirExpr::Construct(construct) => sum_all(&construct.node.args),
        MirExpr::RecordCreate(create) => create
            .node
            .fields
            .iter()
            .map(|field| count_slot_reads(&field.value, slot))
            .sum(),
        MirExpr::RecordUpdate(update) => {
            count_slot_reads(&update.node.base, slot)
                + update
                    .node
                    .updates
                    .iter()
                    .map(|field| count_slot_reads(&field.value, slot))
                    .sum::<usize>()
        }
        MirExpr::Project(project) => count_slot_reads(&project.node.base, slot),
        MirExpr::IfThenElse(ite) => {
            count_slot_reads(&ite.node.cond, slot)
                + count_slot_reads(&ite.node.then_branch, slot)
                + count_slot_reads(&ite.node.else_branch, slot)
        }
        MirExpr::List(items) | MirExpr::Tuple(items) => sum_all(items),
        MirExpr::MapLiteral(pairs) => pairs
            .iter()
            .map(|(key, value)| count_slot_reads(key, slot) + count_slot_reads(value, slot))
            .sum(),
        MirExpr::InterpolatedStr(parts) => parts
            .iter()
            .map(|part| match part {
                crate::ir::mir::MirStrPart::Literal(_) => 0,
                crate::ir::mir::MirStrPart::Expr(inner) => count_slot_reads(inner, slot),
            })
            .sum(),
        MirExpr::IndependentProduct(product) => sum_all(&product.node.items),
    }
}

struct MirSymPlanBuilder<'a, 'e> {
    params_by_slot: &'a std::collections::HashMap<u32, (u32, SymTy)>,
    record_fields: RecordFieldLookup<'a>,
    builtins: &'a [String],
    /// Single-use `let` bindings in scope: slot -> initializer. A `Local`
    /// read of an aliased slot re-lowers the initializer at the use site
    /// (sound because every plannable MirExpr is pure, and exact because the
    /// binding is proven single-use before it is recorded).
    aliases: std::collections::HashMap<u32, &'e crate::ast::Spanned<crate::ir::mir::MirExpr>>,
    /// Alias hops currently on the resolution stack (see
    /// [`MAX_ALIAS_RESOLUTION_DEPTH`]).
    alias_hops: usize,
    nodes: Vec<SymNode>,
}

impl<'a, 'e> MirSymPlanBuilder<'a, 'e> {
    fn lower_expr(
        &mut self,
        expr: &'e crate::ast::Spanned<crate::ir::mir::MirExpr>,
    ) -> Option<(SymValueId, SymTy)> {
        match &expr.node {
            crate::ir::mir::MirExpr::Literal(lit) => match &lit.node {
                crate::ast::Literal::Bool(value) => {
                    self.push_node(SymTy::Bool, SymNodeKind::ConstBool(*value))
                }
                crate::ast::Literal::Float(value) => {
                    self.push_node(SymTy::Float, SymNodeKind::ConstFloatBits(value.to_bits()))
                }
                crate::ast::Literal::Str(value) => {
                    self.push_node(SymTy::String, SymNodeKind::ConstStringBytes(value.as_bytes().to_vec()))
                }
                _ => None,
            },
            crate::ir::mir::MirExpr::Local(local) => {
                if let Some((index, ty)) = self.params_by_slot.get(&local.node.slot.0).cloned() {
                    return self.push_node(ty, SymNodeKind::Param { index });
                }
                // The one read of a single-use `let` binding: lower the
                // recorded initializer here, preserving stack discipline
                // (nodes append in evaluation order at the use site).
                let init = *self.aliases.get(&local.node.slot.0)?;
                if self.alias_hops >= MAX_ALIAS_RESOLUTION_DEPTH {
                    return None;
                }
                self.alias_hops += 1;
                let lowered = self.lower_expr(init);
                self.alias_hops -= 1;
                lowered
            }
            crate::ir::mir::MirExpr::Let(spanned_let) => self.lower_let(&spanned_let.node),
            crate::ir::mir::MirExpr::BinOp(binop) => self.lower_binop(&binop.node),
            crate::ir::mir::MirExpr::Project(spanned_proj) => {
                // Record field access `base.field`: only a named-record base
                // resolvable through the emitter's registry plans a
                // projection; newtypes/carriers (identity reads) and unknown
                // layouts return `None` from the lookup and stay unplanned.
                let proj = &spanned_proj.node;
                let (value, base_ty) = self.lower_expr(&proj.base)?;
                let SymTy::Named(type_name) = base_ty else {
                    return None;
                };
                let (field, field_ty_name) = (self.record_fields)(&type_name, &proj.field)?;
                let field_ty = sym_ty_from_mir_name(&field_ty_name)?;
                self.push_node(
                    field_ty.clone(),
                    SymNodeKind::ProjectField {
                        type_name,
                        field,
                        field_ty,
                        value,
                    },
                )
            }
            crate::ir::mir::MirExpr::IfThenElse(ite) => self.lower_if(&ite.node),
            crate::ir::mir::MirExpr::Match(m) => self.lower_tag_match(&m.node),
            crate::ir::mir::MirExpr::Call(call) => self.lower_fused_vector_get(&call.node),
            _ => None,
        }
    }

    /// Single-use `let` inlining: `x = init; body` lowers as `body` with the
    /// single read of `x` replaced by lowering `init` at the use site. All
    /// constraints are mandatory and fail closed to "unplanned":
    ///
    /// - the binding must be read EXACTLY once across the whole remaining
    ///   body, including nested if/match sub-blocks and later `let`
    ///   initializers (a multi-use binding inlined per-use would duplicate
    ///   evaluation relative to the MIR-emitted bytes' sharing);
    /// - the initializer must itself be plannable — checked where it is
    ///   lowered at the use site; purity comes for free because every
    ///   plannable MirExpr shape is pure (the fn-level effect gate in
    ///   `sym_plan_from_mir_fn` is unchanged);
    /// - malformed shapes (a binding shadowing a param slot, a
    ///   self-referential initializer) refuse outright.
    fn lower_let(
        &mut self,
        spanned_let: &'e crate::ir::mir::MirLet,
    ) -> Option<(SymValueId, SymTy)> {
        if self.params_by_slot.contains_key(&spanned_let.binding.0) {
            return None;
        }
        if count_slot_reads(&spanned_let.body, spanned_let.binding.0) != 1 {
            return None;
        }
        if count_slot_reads(&spanned_let.value, spanned_let.binding.0) != 0 {
            return None;
        }
        let displaced = self
            .aliases
            .insert(spanned_let.binding.0, &spanned_let.value);
        let lowered = self.lower_expr(&spanned_let.body);
        // Restore the exact scope: the binding ends with its body.
        match displaced {
            Some(previous) => self.aliases.insert(spanned_let.binding.0, previous),
            None => self.aliases.remove(&spanned_let.binding.0),
        };
        lowered
    }

    /// The fused `Option.withDefault(Vector.get(p0, p1), <int literal>)`
    /// shape, recognised exactly as the wasm-gc emitter fuses it
    /// (`emit_mir_option_with_default`): the vector must be param 0 with
    /// source type `Vector<Int>`, the index param 1 with source type `Int`,
    /// and the default an Int literal. Anything broader stays unplanned so
    /// the emitted bytes are untouched.
    fn lower_fused_vector_get(
        &mut self,
        call: &crate::ir::mir::MirCall,
    ) -> Option<(SymValueId, SymTy)> {
        let crate::ir::mir::MirCallee::Builtin(id) = call.callee else {
            return None;
        };
        if self.builtins.get(id.0 as usize)? != "Option.withDefault" || call.args.len() != 2 {
            return None;
        }
        let crate::ir::mir::MirExpr::Call(inner_sp) = &call.args[0].node else {
            return None;
        };
        let inner = &inner_sp.node;
        let crate::ir::mir::MirCallee::Builtin(inner_id) = inner.callee else {
            return None;
        };
        if self.builtins.get(inner_id.0 as usize)? != "Vector.get" || inner.args.len() != 2 {
            return None;
        }
        let crate::ir::mir::MirExpr::Local(vector) = &inner.args[0].node else {
            return None;
        };
        let crate::ir::mir::MirExpr::Local(index) = &inner.args[1].node else {
            return None;
        };
        let (vector_idx, vector_ty) = self.params_by_slot.get(&vector.node.slot.0)?.clone();
        let (index_idx, index_ty) = self.params_by_slot.get(&index.node.slot.0)?.clone();
        if vector_idx != 0
            || index_idx != 1
            || vector_ty != SymTy::App("Vector".to_string(), vec![SymTy::Int])
            || index_ty != SymTy::Int
        {
            return None;
        }
        let default = mir_int_literal(&call.args[1])?;
        self.push_node(
            SymTy::Int,
            SymNodeKind::VectorGetOrDefault {
                type_name: "Vector<Int>".to_string(),
                default,
            },
        )
    }

    fn lower_binop(&mut self, binop: &'e crate::ir::mir::MirBinOp) -> Option<(SymValueId, SymTy)> {
        // Narrow straight-line integer face: exactly `param + k` over the
        // single Int parameter (const on the right, matching the emitted byte
        // shape). Anything broader stays unplanned so the emitted bytes are
        // untouched and legacy classification is unaffected.
        if binop.op == crate::ast::BinOp::Add
            && self.params_by_slot.len() == 1
            && self.expr_is_int_param(&binop.lhs)
            && let Some(k) = mir_int_literal(&binop.rhs)
        {
            let (lhs, lhs_ty) = self.lower_expr(&binop.lhs)?;
            if lhs_ty != SymTy::Int {
                return None;
            }
            let (rhs, _) = self.push_node(SymTy::Int, SymNodeKind::ConstInt(k))?;
            return self.push_node(
                SymTy::Int,
                SymNodeKind::Prim {
                    op: SymPrim::IntAdd,
                    args: vec![lhs, rhs],
                },
            );
        }

        if let Some((operand, op, k, const_on_left)) = self.int_const_cmp_shape(binop) {
            return self.lower_int_const_cmp(operand, op, k, const_on_left);
        }

        let (lhs, lhs_ty) = self.lower_expr(&binop.lhs)?;
        let (rhs, rhs_ty) = self.lower_expr(&binop.rhs)?;
        if lhs_ty == SymTy::String && rhs_ty == SymTy::String {
            if binop.op != crate::ast::BinOp::Add {
                return None;
            }
            return self.push_node(
                SymTy::String,
                SymNodeKind::Prim {
                    op: SymPrim::StringConcat,
                    args: vec![lhs, rhs],
                },
            );
        }

        if lhs_ty != SymTy::Float || rhs_ty != SymTy::Float {
            return None;
        }
        let (op, result_ty) = match binop.op {
            crate::ast::BinOp::Add => (SymPrim::FloatAdd, SymTy::Float),
            crate::ast::BinOp::Mul => (SymPrim::FloatMul, SymTy::Float),
            crate::ast::BinOp::Lte => (SymPrim::FloatLe, SymTy::Bool),
            crate::ast::BinOp::Gte => (SymPrim::FloatGe, SymTy::Bool),
            crate::ast::BinOp::Lt => (SymPrim::FloatLt, SymTy::Bool),
            crate::ast::BinOp::Gt => (SymPrim::FloatGt, SymTy::Bool),
            crate::ast::BinOp::Eq => (SymPrim::FloatEq, SymTy::Bool),
            // `Neq` stays out: `f64.ne` is the UNORDERED comparison (true on
            // NaN), so it is not covered by the ordered-comparison NaN clause
            // the other five share.
            _ => return None,
        };
        self.push_node(
            result_ty,
            SymNodeKind::Prim {
                op,
                args: vec![lhs, rhs],
            },
        )
    }

    fn int_const_cmp_shape(
        &self,
        binop: &'e crate::ir::mir::MirBinOp,
    ) -> Option<(&'e crate::ast::Spanned<crate::ir::mir::MirExpr>, crate::ast::BinOp, i64, bool)>
    {
        if let Some(k) = mir_int_literal(&binop.rhs)
            && self.expr_is_int_param(&binop.lhs)
        {
            return Some((&binop.lhs, binop.op, k, false));
        }
        if let Some(k) = mir_int_literal(&binop.lhs)
            && self.expr_is_int_param(&binop.rhs)
        {
            return Some((&binop.rhs, binop.op, k, true));
        }
        None
    }

    /// Resolve a `Local` read through the single-use let-alias chain to the
    /// param it terminates at. Only chains made of `Local` reads qualify: a
    /// COMPUTED alias (any non-`Local` initializer) returns `None`. The two
    /// callers require exactly that: the `intConstCmp` operand must be a
    /// param read (PlanCheck's `isSymParam` — kernel-side, not adapter
    /// courtesy), and the tag-match scrutinee is pinned by the wall face to
    /// the param-0 local read.
    fn resolve_local_chain_param(
        &self,
        expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        hops: usize,
    ) -> Option<(u32, SymTy)> {
        if hops > MAX_ALIAS_RESOLUTION_DEPTH {
            return None;
        }
        let crate::ir::mir::MirExpr::Local(local) = &expr.node else {
            return None;
        };
        if let Some(entry) = self.params_by_slot.get(&local.node.slot.0) {
            return Some(entry.clone());
        }
        let init = self.aliases.get(&local.node.slot.0)?;
        self.resolve_local_chain_param(init, hops + 1)
    }

    fn expr_is_int_param(&self, expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>) -> bool {
        self.resolve_local_chain_param(expr, 0)
            .is_some_and(|(_, ty)| ty == SymTy::Int)
    }

    fn lower_int_const_cmp(
        &mut self,
        operand: &'e crate::ast::Spanned<crate::ir::mir::MirExpr>,
        op: crate::ast::BinOp,
        k: i64,
        const_on_left: bool,
    ) -> Option<(SymValueId, SymTy)> {
        let eff = if const_on_left { flip_cmp(op) } else { op };
        let op = sym_int_const_cmp_op(eff)?;
        let (value, value_ty) = self.lower_expr(operand)?;
        if value_ty != SymTy::Int {
            return None;
        }
        self.push_node(
            SymTy::Bool,
            SymNodeKind::IntConstCmp {
                op,
                value,
                constant: k,
            },
        )
    }

    fn lower_if(&mut self, ite: &'e crate::ir::mir::MirIfThenElse) -> Option<(SymValueId, SymTy)> {
        let (cond, cond_ty) = self.lower_expr(&ite.cond)?;
        if cond_ty != SymTy::Bool {
            return None;
        }

        let mut then_builder = MirSymPlanBuilder {
            params_by_slot: self.params_by_slot,
            record_fields: self.record_fields,
            builtins: self.builtins,
            aliases: self.aliases.clone(),
            alias_hops: self.alias_hops,
            nodes: Vec::new(),
        };
        let (then_root, then_ty) = then_builder.lower_expr(&ite.then_branch)?;
        let then_block = then_builder.finish(then_root)?;

        let mut else_builder = MirSymPlanBuilder {
            params_by_slot: self.params_by_slot,
            record_fields: self.record_fields,
            builtins: self.builtins,
            aliases: self.aliases.clone(),
            alias_hops: self.alias_hops,
            nodes: Vec::new(),
        };
        let (else_root, else_ty) = else_builder.lower_expr(&ite.else_branch)?;
        let else_block = else_builder.finish(else_root)?;

        if then_ty != else_ty || then_block.result_ty() != else_block.result_ty() {
            return None;
        }

        self.push_node(
            then_ty,
            SymNodeKind::If {
                cond,
                then_block: Box::new(then_block),
                else_block: Box::new(else_block),
            },
        )
    }

    fn lower_tag_match(
        &mut self,
        m: &'e crate::ir::mir::MirMatch,
    ) -> Option<(SymValueId, SymTy)> {
        if self.params_by_slot.len() != 1 || m.arms.len() != 2 {
            return None;
        }
        // The subject must resolve to THE single param — directly or through
        // a single-use let-alias chain of `Local` reads (the wall face pins
        // the encoded scrutinee to the param-0 local read, which is exactly
        // what the aliased subject lowers back to in `push_tag_match`).
        let (_, subject_ty) = self.resolve_local_chain_param(&m.subject, 0)?;
        let (type_name, hit_ctor, miss_ctor, tag) = match &subject_ty {
            SymTy::App(name, args) if name == "Option" && args.len() == 1 => (
                "Option",
                crate::ir::hir::BuiltinCtor::OptionSome,
                crate::ir::hir::BuiltinCtor::OptionNone,
                crate::codegen::wasm_gc::OPTION_SOME_TAG,
            ),
            SymTy::App(name, args) if name == "Result" && args.len() == 2 => (
                "Result",
                crate::ir::hir::BuiltinCtor::ResultOk,
                crate::ir::hir::BuiltinCtor::ResultErr,
                crate::codegen::wasm_gc::RESULT_OK_TAG,
            ),
            _ => return None,
        };

        let mut hit = None;
        let mut miss = None;
        let mut wildcard = None;
        for arm in &m.arms {
            let value = mir_int_literal(&arm.body)?;
            match &arm.pattern {
                crate::ir::mir::MirPattern::Ctor {
                    ctor: crate::ir::mir::MirCtor::Builtin(ctor),
                    ..
                } if *ctor == hit_ctor && hit.is_none() => hit = Some(value),
                crate::ir::mir::MirPattern::Ctor {
                    ctor: crate::ir::mir::MirCtor::Builtin(ctor),
                    ..
                } if *ctor == miss_ctor && miss.is_none() => miss = Some(value),
                crate::ir::mir::MirPattern::Wildcard if wildcard.is_none() => {
                    wildcard = Some(value)
                }
                _ => return None,
            }
        }
        match (hit, miss, wildcard) {
            (Some(hit), Some(miss), None) => self.push_tag_match(
                &m.subject,
                type_name,
                tag,
                hit,
                miss,
            ),
            (Some(hit), None, Some(miss)) => self.push_tag_match(
                &m.subject,
                type_name,
                tag,
                hit,
                miss,
            ),
            (None, Some(miss), Some(hit)) => self.push_tag_match(
                &m.subject,
                type_name,
                tag,
                hit,
                miss,
            ),
            _ => None,
        }
    }

    fn push_tag_match(
        &mut self,
        subject: &'e crate::ast::Spanned<crate::ir::mir::MirExpr>,
        type_name: &str,
        tag: i32,
        hit: i64,
        miss: i64,
    ) -> Option<(SymValueId, SymTy)> {
        let (scrutinee, _) = self.lower_expr(subject)?;
        let int_block = |value| SymBlock {
            nodes: vec![SymNode {
                id: SymValueId(0),
                ty: SymTy::Int,
                kind: SymNodeKind::ConstInt(value),
            }],
            result: SymValueId(0),
        };
        self.push_node(
            SymTy::Int,
            SymNodeKind::TagMatch {
                type_name: type_name.to_string(),
                scrutinee,
                tag: i64::from(tag),
                hit: Box::new(int_block(hit)),
                miss: Box::new(int_block(miss)),
            },
        )
    }

    fn push_node(&mut self, ty: SymTy, kind: SymNodeKind) -> Option<(SymValueId, SymTy)> {
        let id = SymValueId(self.nodes.len());
        self.nodes.push(SymNode {
            id,
            ty: ty.clone(),
            kind,
        });
        Some((id, ty))
    }

    fn finish(self, result: SymValueId) -> Option<SymBlock> {
        // Kernel block-shape invariants (PlanCheck.checkSymBlock): every
        // node's id equals its position, and the result is the LAST node
        // (`result + 1 = nodes.length`). The builder satisfies both by
        // construction today; checking them here makes a future producer bug
        // degrade to an unplanned fn instead of a late kernel reject.
        if self
            .nodes
            .iter()
            .enumerate()
            .any(|(position, node)| node.id.0 != position)
        {
            return None;
        }
        if result.0 + 1 != self.nodes.len() {
            return None;
        }
        let block = SymBlock {
            nodes: self.nodes,
            result,
        };
        block.result_ty()?;
        Some(block)
    }
}

struct MirExprFragmentBuilder<'a> {
    params_by_slot: &'a std::collections::HashMap<u32, (u32, FragTy)>,
    nodes: Vec<FragNode>,
}

impl MirExprFragmentBuilder<'_> {
    fn lower_expr(
        &mut self,
        expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
    ) -> Option<(FragValueId, FragTy)> {
        match &expr.node {
            crate::ir::mir::MirExpr::Literal(lit) => match &lit.node {
                crate::ast::Literal::Bool(value) => {
                    self.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(*value))
                }
                crate::ast::Literal::Float(value) => {
                    self.push_node(FragTy::F64, FragNodeKind::ConstF64(value.to_bits()))
                }
                _ => None,
            },
            crate::ir::mir::MirExpr::Local(local) => {
                let (index, ty) = *self.params_by_slot.get(&local.node.slot.0)?;
                self.push_node(ty, FragNodeKind::Local { index })
            }
            crate::ir::mir::MirExpr::BinOp(binop) => self.lower_binop(&binop.node),
            crate::ir::mir::MirExpr::IfThenElse(ite) => self.lower_if(&ite.node),
            _ => None,
        }
    }

    fn lower_binop(&mut self, binop: &crate::ir::mir::MirBinOp) -> Option<(FragValueId, FragTy)> {
        if let Some(cmp) = self.int_const_cmp_shape(binop) {
            return self.lower_int_const_cmp(cmp.0, cmp.1, cmp.2, cmp.3);
        }

        let (lhs, lhs_ty) = self.lower_expr(&binop.lhs)?;
        let (rhs, rhs_ty) = self.lower_expr(&binop.rhs)?;
        if lhs_ty != FragTy::F64 || rhs_ty != FragTy::F64 {
            return None;
        }
        let (op, result_ty) = match binop.op {
            crate::ast::BinOp::Add => (FragPrim::F64Add, FragTy::F64),
            crate::ast::BinOp::Mul => (FragPrim::F64Mul, FragTy::F64),
            crate::ast::BinOp::Lte => (FragPrim::F64Le, FragTy::BoolI32),
            crate::ast::BinOp::Gte => (FragPrim::F64Ge, FragTy::BoolI32),
            crate::ast::BinOp::Lt => (FragPrim::F64Lt, FragTy::BoolI32),
            crate::ast::BinOp::Gt => (FragPrim::F64Gt, FragTy::BoolI32),
            crate::ast::BinOp::Eq => (FragPrim::F64Eq, FragTy::BoolI32),
            _ => return None,
        };
        self.push_node(
            result_ty,
            FragNodeKind::Prim {
                op,
                args: vec![lhs, rhs],
            },
        )
    }

    fn int_const_cmp_shape<'a>(
        &self,
        binop: &'a crate::ir::mir::MirBinOp,
    ) -> Option<(&'a crate::ast::Spanned<crate::ir::mir::MirExpr>, crate::ast::BinOp, i64, bool)>
    {
        if let Some(k) = mir_int_literal(&binop.rhs)
            && self.expr_is_int_param(&binop.lhs)
        {
            return Some((&binop.lhs, binop.op, k, false));
        }
        if let Some(k) = mir_int_literal(&binop.lhs)
            && self.expr_is_int_param(&binop.rhs)
        {
            return Some((&binop.rhs, binop.op, k, true));
        }
        None
    }

    fn expr_is_int_param(&self, expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>) -> bool {
        match &expr.node {
            crate::ir::mir::MirExpr::Local(local) => self
                .params_by_slot
                .get(&local.node.slot.0)
                .is_some_and(|(_, ty)| *ty == FragTy::IntCarrier),
            _ => false,
        }
    }

    fn lower_int_const_cmp(
        &mut self,
        operand: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        op: crate::ast::BinOp,
        k: i64,
        const_on_left: bool,
    ) -> Option<(FragValueId, FragTy)> {
        let eff = if const_on_left { flip_cmp(op) } else { op };
        let small_prim = i64_const_cmp_prim(eff)?;
        let big_kind = big_int_const_cmp_kind(eff)?;

        let (carrier, carrier_ty) = self.lower_expr(operand)?;
        if carrier_ty != FragTy::IntCarrier {
            return None;
        }
        let (magf, _) = self.push_node(
            FragTy::Ref,
            FragNodeKind::StructGet {
                field: 1,
                receiver: carrier,
            },
        )?;
        let (is_small, _) =
            self.push_node(FragTy::BoolI32, FragNodeKind::RefIsNull { value: magf })?;

        let then_block = self.lower_int_small_const_cmp_block(operand, small_prim, k)?;
        let else_block = self.lower_int_big_const_cmp_block(operand, big_kind)?;
        self.push_node(
            FragTy::BoolI32,
            FragNodeKind::If {
                cond: is_small,
                then_block: Box::new(then_block),
                else_block: Box::new(else_block),
            },
        )
    }

    fn lower_int_small_const_cmp_block(
        &self,
        operand: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        op: FragPrim,
        k: i64,
    ) -> Option<FragBlock> {
        let mut block = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (carrier, carrier_ty) = block.lower_expr(operand)?;
        if carrier_ty != FragTy::IntCarrier {
            return None;
        }
        let (small, _) = block.push_node(
            FragTy::I64,
            FragNodeKind::StructGet {
                field: 0,
                receiver: carrier,
            },
        )?;
        let (constant, _) = block.push_node(FragTy::I64, FragNodeKind::ConstI64(k))?;
        let (result, _) = block.push_node(
            FragTy::BoolI32,
            FragNodeKind::Prim {
                op,
                args: vec![small, constant],
            },
        )?;
        block.finish(result)
    }

    fn lower_int_big_const_cmp_block(
        &self,
        operand: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        kind: BigIntConstCmpKind,
    ) -> Option<FragBlock> {
        let mut block = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        match kind {
            BigIntConstCmpKind::Always(value) => {
                let (result, _) =
                    block.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(value))?;
                block.finish(result)
            }
            BigIntConstCmpKind::SignLtZero | BigIntConstCmpKind::SignGtZero => {
                let (carrier, carrier_ty) = block.lower_expr(operand)?;
                if carrier_ty != FragTy::IntCarrier {
                    return None;
                }
                let (sign, _) = block.push_node(
                    FragTy::RawI32,
                    FragNodeKind::StructGet {
                        field: 2,
                        receiver: carrier,
                    },
                )?;
                let (zero, _) =
                    block.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(false))?;
                let op = match kind {
                    BigIntConstCmpKind::SignLtZero => FragPrim::I32LtS,
                    BigIntConstCmpKind::SignGtZero => FragPrim::I32GtS,
                    BigIntConstCmpKind::Always(_) => unreachable!(),
                };
                let (result, _) = block.push_node(
                    FragTy::BoolI32,
                    FragNodeKind::Prim {
                        op,
                        args: vec![sign, zero],
                    },
                )?;
                block.finish(result)
            }
        }
    }

    fn lower_if(&mut self, ite: &crate::ir::mir::MirIfThenElse) -> Option<(FragValueId, FragTy)> {
        let (cond, cond_ty) = self.lower_expr(&ite.cond)?;
        if cond_ty != FragTy::BoolI32 {
            return None;
        }

        let mut then_builder = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (then_root, then_ty) = then_builder.lower_expr(&ite.then_branch)?;
        let then_block = then_builder.finish(then_root)?;

        let mut else_builder = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (else_root, else_ty) = else_builder.lower_expr(&ite.else_branch)?;
        let else_block = else_builder.finish(else_root)?;

        if then_ty != else_ty || then_block.result_ty()? != else_block.result_ty()? {
            return None;
        }

        self.push_node(
            then_ty,
            FragNodeKind::If {
                cond,
                then_block: Box::new(then_block),
                else_block: Box::new(else_block),
            },
        )
    }

    fn push_node(&mut self, ty: FragTy, kind: FragNodeKind) -> Option<(FragValueId, FragTy)> {
        let id = FragValueId(self.nodes.len());
        self.nodes.push(FragNode { id, ty, kind });
        Some((id, ty))
    }

    fn finish(self, result: FragValueId) -> Option<FragBlock> {
        self.nodes.get(result.0)?;
        Some(FragBlock {
            nodes: self.nodes,
            result,
        })
    }
}

#[derive(Clone, Copy)]
enum BigIntConstCmpKind {
    Always(bool),
    SignLtZero,
    SignGtZero,
}

fn mir_int_literal(expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>) -> Option<i64> {
    match &expr.node {
        crate::ir::mir::MirExpr::Literal(lit) => match lit.node {
            crate::ast::Literal::Int(k) => Some(k),
            _ => None,
        },
        _ => None,
    }
}

fn flip_cmp(op: crate::ast::BinOp) -> crate::ast::BinOp {
    match op {
        crate::ast::BinOp::Lt => crate::ast::BinOp::Gt,
        crate::ast::BinOp::Gt => crate::ast::BinOp::Lt,
        crate::ast::BinOp::Lte => crate::ast::BinOp::Gte,
        crate::ast::BinOp::Gte => crate::ast::BinOp::Lte,
        crate::ast::BinOp::Eq => crate::ast::BinOp::Eq,
        crate::ast::BinOp::Neq => crate::ast::BinOp::Neq,
        other => other,
    }
}

fn i64_const_cmp_prim(op: crate::ast::BinOp) -> Option<FragPrim> {
    match op {
        crate::ast::BinOp::Eq => Some(FragPrim::I64Eq),
        crate::ast::BinOp::Lt => Some(FragPrim::I64LtS),
        crate::ast::BinOp::Lte => Some(FragPrim::I64LeS),
        crate::ast::BinOp::Gte => Some(FragPrim::I64GeS),
        crate::ast::BinOp::Gt => Some(FragPrim::I64GtS),
        // `Neq` stays out: `i64.ne` is not in the wall's measured instruction
        // set (`WInstr`), so admitting it is a wasm-model extension, not a
        // plan-grammar one.
        crate::ast::BinOp::Neq => None,
        _ => None,
    }
}

fn sym_int_const_cmp_op(op: crate::ast::BinOp) -> Option<SymIntCmp> {
    match op {
        crate::ast::BinOp::Eq => Some(SymIntCmp::Eq),
        crate::ast::BinOp::Lt => Some(SymIntCmp::Lt),
        crate::ast::BinOp::Lte => Some(SymIntCmp::Le),
        crate::ast::BinOp::Gte => Some(SymIntCmp::Ge),
        crate::ast::BinOp::Gt => Some(SymIntCmp::Gt),
        // Kept aligned with the representation encoder: `Neq` needs
        // `i64.ne` in the wall's measured instruction set first.
        crate::ast::BinOp::Neq => None,
        _ => None,
    }
}

fn big_int_const_cmp_kind(op: crate::ast::BinOp) -> Option<BigIntConstCmpKind> {
    match op {
        crate::ast::BinOp::Eq => Some(BigIntConstCmpKind::Always(false)),
        crate::ast::BinOp::Lt | crate::ast::BinOp::Lte => Some(BigIntConstCmpKind::SignLtZero),
        // A Big carrier is strictly outside the i64 range, so `> k` and `>= k`
        // are both decided by the sign limb alone (a Big never equals an i64
        // literal) — the mirror of `Lt | Lte` sharing `SignLtZero`.
        crate::ast::BinOp::Gte | crate::ast::BinOp::Gt => Some(BigIntConstCmpKind::SignGtZero),
        // Kept aligned with `i64_const_cmp_prim`: no `Neq` admission yet.
        crate::ast::BinOp::Neq => None,
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Literal, Spanned};
    use crate::ir::hir::BuiltinCtor;
    use crate::ir::FnId;
    use crate::ir::mir::{
        LocalId, MirBinOp, MirCtor, MirExpr, MirFn, MirFnRepr, MirIfThenElse, MirLet, MirLocal,
        MirMatch, MirMatchArm, MirParam, MirPattern,
    };

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: std::sync::OnceLock::new(),
        }
    }

    fn int_local(slot: u32) -> Spanned<MirExpr> {
        span(MirExpr::Local(span(MirLocal::at(LocalId(slot)))))
    }

    fn int_lit(value: i64) -> Spanned<MirExpr> {
        span(MirExpr::Literal(span(Literal::Int(value))))
    }

    fn bool_lit(value: bool) -> Spanned<MirExpr> {
        span(MirExpr::Literal(span(Literal::Bool(value))))
    }

    fn binop(op: BinOp, lhs: Spanned<MirExpr>, rhs: Spanned<MirExpr>) -> Spanned<MirExpr> {
        span(MirExpr::BinOp(span(MirBinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })))
    }

    fn let_expr(slot: u32, value: Spanned<MirExpr>, body: Spanned<MirExpr>) -> Spanned<MirExpr> {
        span(MirExpr::Let(span(MirLet {
            binding: LocalId(slot),
            binding_name: "y".to_string(),
            value: Box::new(value),
            body: Box::new(body),
        })))
    }

    fn if_expr(
        cond: Spanned<MirExpr>,
        then_branch: Spanned<MirExpr>,
        else_branch: Spanned<MirExpr>,
    ) -> Spanned<MirExpr> {
        span(MirExpr::IfThenElse(span(MirIfThenElse {
            cond: Box::new(cond),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })))
    }

    /// `fn f(x: Int) -> <return_type>` with an arbitrary body.
    fn int_param_fn(return_type: &str, body: Spanned<MirExpr>, local_count: u32) -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "f".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "x".to_string(),
                ty: "Int".to_string(),
            }],
            return_type: return_type.to_string(),
            effects: vec![],
            body,
            local_count,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    fn float_local(slot: u32) -> Spanned<MirExpr> {
        span(MirExpr::Local(span(MirLocal::at(LocalId(slot)))))
    }

    fn float_binop_fn(op: BinOp) -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "f".to_string(),
            params: vec![
                MirParam {
                    local: LocalId(0),
                    name: "a".to_string(),
                    ty: "Float".to_string(),
                },
                MirParam {
                    local: LocalId(1),
                    name: "b".to_string(),
                    ty: "Float".to_string(),
                },
            ],
            return_type: match op {
                BinOp::Lte => "Bool".to_string(),
                _ => "Float".to_string(),
            },
            effects: vec![],
            body: span(MirExpr::BinOp(span(MirBinOp {
                op,
                lhs: Box::new(float_local(0)),
                rhs: Box::new(float_local(1)),
            }))),
            local_count: 2,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    fn int_predicate_fn(op: BinOp, lhs: Spanned<MirExpr>, rhs: Spanned<MirExpr>) -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "p".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "x".to_string(),
                ty: "Int".to_string(),
            }],
            return_type: "Bool".to_string(),
            effects: vec![],
            body: span(MirExpr::BinOp(span(MirBinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }))),
            local_count: 1,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    fn int_identity_fn() -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "id".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "x".to_string(),
                ty: "Int".to_string(),
            }],
            return_type: "Int".to_string(),
            effects: vec![],
            body: int_local(0),
            local_count: 1,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    fn option_slot_count_fn(some_body: Spanned<MirExpr>) -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "slotCount".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "egg".to_string(),
                ty: "Option(Int)".to_string(),
            }],
            return_type: "Int".to_string(),
            effects: vec![],
            body: span(MirExpr::Match(span(MirMatch {
                subject: Box::new(int_local(0)),
                arms: vec![
                    MirMatchArm {
                        pattern: MirPattern::Ctor {
                            ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                            bindings: vec![],
                            binding_names: vec![],
                        },
                        body: int_lit(0),
                    },
                    MirMatchArm {
                        pattern: MirPattern::Ctor {
                            ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                            bindings: vec![LocalId(1)],
                            binding_names: vec!["value".to_string()],
                        },
                        body: some_body,
                    },
                ],
            }))),
            local_count: 2,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    #[test]
    fn option_literal_match_lowers_to_operational_tag_dispatch() {
        let mir_fn = option_slot_count_fn(int_lit(1));
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("tag plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("Option tag match must remain a source symbolic plan")
        };
        assert_eq!(
            sym.params,
            vec![SymTy::App("Option".to_string(), vec![SymTy::Int])]
        );
        assert!(matches!(
            &sym.body.nodes[1].kind,
            SymNodeKind::TagMatch {
                type_name,
                scrutinee: SymValueId(0),
                tag,
                hit,
                miss,
            } if type_name == "Option"
                && *tag == i64::from(crate::codegen::wasm_gc::OPTION_SOME_TAG)
                && matches!(hit.nodes[0].kind, SymNodeKind::ConstInt(1))
                && matches!(miss.nodes[0].kind, SymNodeKind::ConstInt(0))
        ));
        let encoded = plan
            .to_expr_fragment_plan(
                &FragHostTable::placeholder(),
                &FragStructTable::placeholder_for(sym),
            )
            .expect("tag plan encodes");
        assert!(expr_fragment_is_tag_dispatch(&encoded));
    }

    #[test]
    fn option_match_using_payload_stays_unplanned() {
        let mir_fn = option_slot_count_fn(int_local(1));
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
            "payload-dependent match exceeds the narrow tag-dispatch producer scope"
        );
    }

    #[test]
    fn direct_float_mir_prefers_source_level_sym_plan() {
        let mir_fn = float_binop_fn(BinOp::Add);
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("plan");
        let FragmentPlan::Sym(sym) = plan else {
            panic!("direct source-level float fragment should use SymPlan")
        };

        assert_eq!(sym.params, vec![SymTy::Float, SymTy::Float]);
        assert_eq!(sym.result, SymTy::Float);
        assert!(matches!(
            sym.body.nodes[2].kind,
            SymNodeKind::Prim {
                op: SymPrim::FloatAdd,
                ..
            }
        ));
    }

    #[test]
    fn direct_int_identity_stays_unplanned_without_a_face() {
        // A bare Int passthrough ENCODES (the sym plan and its representation
        // encoding both exist), but it has no rendered proof face: its `Cod`
        // would read as `Int` while `codRepr` falls to the verbatim `WVal`
        // relation. The producer must therefore never select it — the export
        // keeps its ordinary MIR-emitted body and declines fail-closed at
        // classification (see `idGoal` in the goal-matrix fixture).
        let mir_fn = int_identity_fn();
        let sym = sym_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("sym plan exists");
        assert_eq!(sym.params, vec![SymTy::Int]);
        assert_eq!(sym.result, SymTy::Int);
        let expr_plan = sym
            .to_expr_fragment_plan(&FragHostTable::placeholder(), &FragStructTable::default())
            .expect("source int identity encodes to a representation plan");
        assert_eq!(expr_plan.result, FragTy::IntCarrier);
        assert!(
            !expr_fragment_plan_has_face(&expr_plan),
            "carrier identity must not have a rendered proof face"
        );
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
            "producer must not select a carrier-returning plan without the \
             straight-line integer face"
        );
    }

    #[test]
    fn int_carrier_comparison_prefers_source_level_sym_plan() {
        let mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("plan");
        let FragmentPlan::Sym(sym) = plan else {
            panic!("source-level int const comparison should use SymPlan")
        };

        assert_eq!(sym.params, vec![SymTy::Int]);
        assert_eq!(sym.result, SymTy::Bool);
        assert!(matches!(sym.body.nodes[0].kind, SymNodeKind::Param { index: 0 }));
        assert!(matches!(
            sym.body.nodes[1].kind,
            SymNodeKind::IntConstCmp {
                op: SymIntCmp::Lt,
                value: SymValueId(0),
                constant: 0,
            }
        ));
    }

    #[test]
    fn int_param_less_than_literal_lowers_from_mir_to_aint_plan() {
        let mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
        let plan = expr_fragment_plan_from_mir_fn(&mir_fn).expect("plan");

        assert_eq!(plan.params, vec![FragTy::IntCarrier]);
        assert_eq!(plan.result, FragTy::BoolI32);
        assert_eq!(plan.body.result_ty(), Some(FragTy::BoolI32));
        assert_eq!(plan.body.nodes.len(), 4);

        assert!(matches!(plan.body.nodes[0].kind, FragNodeKind::Local { index: 0 }));
        assert!(matches!(
            plan.body.nodes[1].kind,
            FragNodeKind::StructGet {
                field: 1,
                receiver: FragValueId(0)
            }
        ));
        assert!(matches!(
            plan.body.nodes[2].kind,
            FragNodeKind::RefIsNull {
                value: FragValueId(1)
            }
        ));
        let FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } = &plan.body.nodes[3].kind
        else {
            panic!("root must be an if over small-vs-big AverInt carrier")
        };
        assert_eq!(*cond, FragValueId(2));
        assert_eq!(then_block.result_ty(), Some(FragTy::BoolI32));
        assert_eq!(else_block.result_ty(), Some(FragTy::BoolI32));
        assert!(matches!(
            then_block.nodes[2].kind,
            FragNodeKind::ConstI64(0)
        ));
        assert!(matches!(
            then_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I64LtS,
                ..
            }
        ));
        assert!(matches!(
            else_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I32LtS,
                ..
            }
        ));
    }

    fn user_name_projection_fn() -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "userName".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "u".to_string(),
                ty: "User".to_string(),
            }],
            return_type: "String".to_string(),
            effects: vec![],
            body: span(MirExpr::Project(span(crate::ir::mir::MirProject {
                base: Box::new(int_local(0)),
                field: "name".to_string(),
            }))),
            local_count: 1,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    #[test]
    fn record_string_field_projection_plans_through_the_project_face() {
        let mir_fn = user_name_projection_fn();
        let record_fields = |record: &str, field: &str| -> Option<(u32, String)> {
            (record == "User" && field == "name").then(|| (0, "String".to_string()))
        };
        let plan = fragment_plan_from_mir_fn(&mir_fn, &record_fields, &[]).expect("plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("record projection should plan as a source-level SymPlan")
        };
        assert_eq!(sym.params, vec![SymTy::Named("User".to_string())]);
        assert_eq!(sym.result, SymTy::String);
        assert!(matches!(
            &sym.body.nodes[1].kind,
            SymNodeKind::ProjectField {
                type_name,
                field: 0,
                field_ty: SymTy::String,
                value: SymValueId(0),
            } if type_name == "User"
        ));
        let frag = sym
            .to_expr_fragment_plan(
                &FragHostTable::placeholder(),
                &FragStructTable::placeholder_for(sym),
            )
            .expect("projection encodes to a representation plan");
        assert!(
            expr_fragment_project_face(&frag).is_some(),
            "encoded projection must match the field-projection face"
        );
    }

    #[test]
    fn record_int_field_projection_stays_unplanned_without_a_face() {
        // `pairFst`-style Int-field reads have no verbatim projection face:
        // the SymPlan exists, but the encoder fail-closes (scalar field), so
        // the export keeps its MIR-emitted body and legacy classification.
        let mut mir_fn = user_name_projection_fn();
        mir_fn.return_type = "Int".to_string();
        let record_fields = |record: &str, field: &str| -> Option<(u32, String)> {
            (record == "User" && field == "name").then(|| (0, "Int".to_string()))
        };
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &record_fields, &[]).is_none(),
            "scalar-field projection must stay unplanned"
        );
    }

    #[test]
    fn record_projection_without_layout_lookup_stays_unplanned() {
        let mir_fn = user_name_projection_fn();
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
            "projection over an unknown record layout must stay unplanned"
        );
    }

    /// Belt-and-braces representation guard: a fn whose `MirFnRepr` marks any
    /// bare param / return / slot (or bare-carrier slot) must never plan —
    /// with `ENABLE_BARE_SLOTS` its wasm signature carries scalar `i64` slots
    /// where the plan lowerer would emit carrier `struct.get` reads (invalid
    /// wasm).
    ///
    /// No end-to-end reproduction is constructible for exported plan-shaped
    /// functions today, which is why this stays a unit test: the wasm-gc bare
    /// analysis only marks a PARAM bare when `compute_bare_param_intervals`
    /// derived a bound for it, and its final mapping yields `Some(interval)`
    /// exclusively for params carrying a recognized equality-decrement
    /// RECURRENCE (a non-top `guard_floor`, i.e. a self-tail-call counter —
    /// see the "Phase A withholds it" comment in
    /// `src/ir/mir/optimize/bare_i64.rs`). A body containing that self call
    /// dies on the plan builder's `Call`/`TailCall` catch-all, so a
    /// plan-shaped body and a bare param are mutually exclusive. `bare_return`
    /// similarly requires the body's tail value to be bare-eligible
    /// (recurrence arithmetic or a literal), and literal-Int bodies do not
    /// plan. Verified empirically: exported `p > 0`-shaped fns keep boxed
    /// `(ref $AverInt)` signatures with and without internal literal callers.
    /// The guard exists so a future widening of the bare analysis degrades to
    /// an unplanned fn instead of emitting invalid wasm.
    #[test]
    fn bare_repr_functions_never_plan() {
        let bare_param = {
            let mut mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
            mir_fn.repr.bare_params = vec![true];
            mir_fn
        };
        let bare_return = {
            let mut mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
            mir_fn.repr.bare_return = true;
            mir_fn
        };
        let bare_slot = {
            let mut mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
            mir_fn.repr.bare_slots.insert(LocalId(1));
            mir_fn
        };
        let carrier_slot = {
            let mut mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
            mir_fn.repr.carrier_slots.insert(LocalId(0));
            mir_fn
        };
        for (case, mir_fn) in [
            ("bare param", bare_param),
            ("bare return", bare_return),
            ("bare let slot", bare_slot),
            ("bare carrier slot", carrier_slot),
        ] {
            assert!(
                sym_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
                "{case}: source plan must refuse a non-all-boxed repr"
            );
            assert!(
                fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
                "{case}: representation plan must refuse a non-all-boxed repr"
            );
        }
        // Control: the identical body with the default (all-boxed) repr plans.
        let boxed = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
        assert!(fragment_plan_from_mir_fn(&boxed, &|_, _| None, &[]).is_some());
    }

    /// `named(egg) = y = egg; match y { None -> 0; Some(_) -> 1 }`: the
    /// let-renamed subject resolves through the alias chain back to the
    /// single param, so the encoded scrutinee is still the param-0 read the
    /// wall face pins.
    fn let_renamed_option_match_fn() -> MirFn {
        let m = span(MirExpr::Match(span(MirMatch {
            subject: Box::new(int_local(2)),
            arms: vec![
                MirMatchArm {
                    pattern: MirPattern::Ctor {
                        ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                        bindings: vec![],
                        binding_names: vec![],
                    },
                    body: int_lit(0),
                },
                MirMatchArm {
                    pattern: MirPattern::Ctor {
                        ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                        bindings: vec![LocalId(1)],
                        binding_names: vec!["value".to_string()],
                    },
                    body: int_lit(1),
                },
            ],
        })));
        let mut mir_fn = option_slot_count_fn(int_lit(1));
        mir_fn.body = let_expr(2, int_local(0), m);
        mir_fn.local_count = 3;
        mir_fn
    }

    #[test]
    fn let_renamed_option_match_lowers_to_tag_dispatch() {
        let mir_fn = let_renamed_option_match_fn();
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("tag plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("let-renamed Option tag match must remain a source symbolic plan")
        };
        assert!(matches!(sym.body.nodes[0].kind, SymNodeKind::Param { index: 0 }));
        assert!(matches!(
            &sym.body.nodes[1].kind,
            SymNodeKind::TagMatch {
                scrutinee: SymValueId(0),
                ..
            }
        ));
    }

    #[test]
    fn chained_let_renames_resolve_recursively() {
        // `y = egg; z = y; match z { ... }` — two hops back to the param.
        let mut mir_fn = let_renamed_option_match_fn();
        let MirExpr::Let(spanned_let) = mir_fn.body.node else {
            panic!("fixture body is a let")
        };
        let mut inner_match = *spanned_let.node.body;
        let MirExpr::Match(m) = &mut inner_match.node else {
            panic!("fixture let body is a match")
        };
        m.node.subject = Box::new(int_local(3));
        mir_fn.body = let_expr(2, int_local(0), let_expr(3, int_local(2), inner_match));
        mir_fn.local_count = 4;
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("tag plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("chained let renames must remain a source symbolic plan")
        };
        assert!(matches!(sym.body.nodes[0].kind, SymNodeKind::Param { index: 0 }));
    }

    #[test]
    fn let_over_int_add_lowers_through_the_int_add_face() {
        // `m = x + 2; m` — the single read of `m` re-lowers the initializer,
        // producing exactly the straight-line integer face node order.
        let body = let_expr(1, binop(BinOp::Add, int_local(0), int_lit(2)), int_local(1));
        let mir_fn = int_param_fn("Int", body, 2);
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("let over int add should use SymPlan")
        };
        assert!(matches!(sym.body.nodes[0].kind, SymNodeKind::Param { index: 0 }));
        assert!(matches!(sym.body.nodes[1].kind, SymNodeKind::ConstInt(2)));
        assert!(matches!(
            sym.body.nodes[2].kind,
            SymNodeKind::Prim {
                op: SymPrim::IntAdd,
                ..
            }
        ));
    }

    #[test]
    fn let_bound_comparison_feeds_the_if_condition() {
        // `isLow = x >= 48; if isLow { x <= 57 } else { false }` — the
        // `inRangeNamed` shape after `bool_match_to_if`.
        let body = let_expr(
            1,
            binop(BinOp::Gte, int_local(0), int_lit(48)),
            if_expr(
                int_local(1),
                binop(BinOp::Lte, int_local(0), int_lit(57)),
                bool_lit(false),
            ),
        );
        let mir_fn = int_param_fn("Bool", body, 2);
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).expect("plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("let-bound comparison should use SymPlan")
        };
        assert!(matches!(
            sym.body.nodes[1].kind,
            SymNodeKind::IntConstCmp {
                op: SymIntCmp::Ge,
                value: SymValueId(0),
                constant: 48,
            }
        ));
        assert!(matches!(sym.body.nodes[2].kind, SymNodeKind::If { cond: SymValueId(1), .. }));
    }

    #[test]
    fn multi_use_let_binding_stays_unplanned() {
        // `y = x; if y > 0 { y <= 5 } else { false }` — TWO reads of `y`
        // (condition + then branch). Inlining would be semantically fine for
        // a pure alias, but the single-use gate is the contract: refuse.
        let body = let_expr(
            1,
            int_local(0),
            if_expr(
                binop(BinOp::Gt, int_local(1), int_lit(0)),
                binop(BinOp::Lte, int_local(1), int_lit(5)),
                bool_lit(false),
            ),
        );
        let mir_fn = int_param_fn("Bool", body, 2);
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
            "a let binding read more than once must stay unplanned"
        );
    }

    #[test]
    fn computed_alias_is_not_an_int_const_cmp_operand() {
        // `y = x + 2; y > 0` — the alias chain terminates in a COMPUTED
        // expression, not a param read. PlanCheck's `isSymParam` requires the
        // comparison operand to be a param read, so the producer must refuse
        // rather than emit a plan the kernel rejects.
        let body = let_expr(
            1,
            binop(BinOp::Add, int_local(0), int_lit(2)),
            binop(BinOp::Gt, int_local(1), int_lit(0)),
        );
        let mir_fn = int_param_fn("Bool", body, 2);
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
            "a computed let alias must not become an intConstCmp operand"
        );
    }

    #[test]
    fn let_with_unplannable_initializer_stays_unplanned() {
        // `y = -x; y > 0` — `Neg` has no plan lowering, so the use-site
        // lowering of the initializer fails and the fn stays unplanned.
        let body = let_expr(
            1,
            span(MirExpr::Neg(Box::new(int_local(0)))),
            binop(BinOp::Gt, int_local(1), int_lit(0)),
        );
        let mir_fn = int_param_fn("Bool", body, 2);
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None, &[]).is_none(),
            "an unplannable initializer must keep the fn unplanned"
        );
    }

    /// The kernel requires every node id to equal its position and the block
    /// result to be the LAST node (PlanCheck.checkSymBlock). The builder
    /// produces exactly that shape; `finish` now refuses anything else so a
    /// future producer bug fails at the producer, not in the kernel.
    #[test]
    fn finish_enforces_kernel_block_shape_invariants() {
        let params = std::collections::HashMap::new();
        let record_fields = |_: &str, _: &str| -> Option<(u32, String)> { None };
        let make_builder = |nodes: Vec<SymNode>| MirSymPlanBuilder {
            params_by_slot: &params,
            record_fields: &record_fields,
            builtins: &[],
            aliases: std::collections::HashMap::new(),
            alias_hops: 0,
            nodes,
        };
        let node = |id: usize| SymNode {
            id: SymValueId(id),
            ty: SymTy::Bool,
            kind: SymNodeKind::ConstBool(true),
        };
        assert!(
            make_builder(vec![node(0), node(1)])
                .finish(SymValueId(0))
                .is_none(),
            "a non-last result must not finish"
        );
        assert!(
            make_builder(vec![node(1)]).finish(SymValueId(0)).is_none(),
            "a discontiguous node id must not finish"
        );
        assert!(
            make_builder(vec![node(0)]).finish(SymValueId(0)).is_some(),
            "the canonical block shape must still finish"
        );
    }

    #[test]
    fn int_literal_on_left_flips_comparison_before_plan_lowering() {
        let mir_fn = int_predicate_fn(BinOp::Lte, int_lit(0), int_local(0));
        let plan = expr_fragment_plan_from_mir_fn(&mir_fn).expect("plan");
        let FragNodeKind::If {
            then_block,
            else_block,
            ..
        } = &plan.body.nodes[3].kind
        else {
            panic!("root must be an if over small-vs-big AverInt carrier")
        };

        assert!(matches!(
            then_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I64GeS,
                ..
            }
        ));
        assert!(matches!(
            else_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I32GtS,
                ..
            }
        ));
    }
}
