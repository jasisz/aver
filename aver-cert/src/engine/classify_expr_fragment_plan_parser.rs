struct FragPlanParser<'a> {
    lines: Vec<&'a str>,
    pos: usize,
    params: Vec<FragTy>,
    result: FragTy,
}

impl<'a> FragPlanParser<'a> {
    fn new(text: &'a str, params: Vec<FragTy>, result: FragTy) -> Self {
        Self {
            lines: text.lines().collect(),
            pos: 0,
            params,
            result,
        }
    }

    fn parse(&mut self) -> Result<FragBlock, String> {
        self.expect_exact("aver.expr-fragment.plan.v1")?;
        self.expect_exact("profile expr-fragment-v1")?;
        let params_line = self.next_trim()?;
        let params = parse_plan_params(params_line)?;
        if params != self.params {
            return Err(format!(
                "plan parameter types {:?} do not match wasm parameter types {:?}",
                plan_ty_tags(&params),
                plan_ty_tags(&self.params)
            ));
        }
        let result_line = self.next_trim()?;
        let result = result_line
            .strip_prefix("result ")
            .and_then(FragTy::from_plan_tag)
            .ok_or_else(|| format!("malformed plan result line `{result_line}`"))?;
        if result != self.result {
            return Err(format!(
                "plan result type `{}` does not match wasm result type `{}`",
                result.plan_tag(),
                self.result.plan_tag()
            ));
        }
        self.expect_exact("body")?;
        let block = self.parse_block()?;
        if block.result_ty() != Some(self.result) {
            return Err("plan root type does not match function result type".to_string());
        }
        if self.pos != self.lines.len() {
            return Err(format!(
                "unexpected trailing plan line `{}`",
                self.lines[self.pos].trim()
            ));
        }
        Ok(block)
    }

    fn parse_block(&mut self) -> Result<FragBlock, String> {
        let header = self.next_trim()?;
        let declared_result = header
            .strip_prefix("block result=")
            .and_then(parse_plan_value_id)
            .ok_or_else(|| format!("malformed plan block header `{header}`"))?;
        let mut nodes = Vec::<FragNode>::new();
        loop {
            let line = self.peek_trim()?;
            if line == "end" {
                self.pos += 1;
                break;
            }
            self.parse_node(&mut nodes)?;
        }
        if nodes.get(declared_result.0).map(|node| node.id) != Some(declared_result) {
            return Err(format!(
                "plan block declares missing/noncanonical result v{}",
                declared_result.0
            ));
        }
        if declared_result.0 + 1 != nodes.len() {
            return Err(format!(
                "plan block result v{} is not the final node v{}",
                declared_result.0,
                nodes.len().saturating_sub(1)
            ));
        }
        Ok(FragBlock {
            nodes,
            result: declared_result,
        })
    }

    fn parse_node(&mut self, nodes: &mut Vec<FragNode>) -> Result<(), String> {
        let line = self.next_trim()?;
        let mut parts = line.split_whitespace();
        let id = parts
            .next()
            .and_then(parse_plan_value_id)
            .ok_or_else(|| format!("malformed plan node id in `{line}`"))?;
        if id.0 != nodes.len() {
            return Err(format!(
                "plan node id v{} is not the next node id v{}",
                id.0,
                nodes.len()
            ));
        }
        let ty = parts
            .next()
            .and_then(|tok| tok.strip_prefix("ty="))
            .and_then(FragTy::from_plan_tag)
            .ok_or_else(|| format!("malformed plan node type in `{line}`"))?;
        let kind_name = parts
            .next()
            .ok_or_else(|| format!("missing plan node kind in `{line}`"))?;
        let attrs = parts
            .map(|tok| {
                let (k, v) = tok
                    .split_once('=')
                    .ok_or_else(|| format!("malformed plan attribute `{tok}`"))?;
                Ok((k.to_string(), v.to_string()))
            })
            .collect::<Result<std::collections::HashMap<_, _>, String>>()?;
        let kind = match kind_name {
            "local" => {
                let index = plan_attr_u32(id, &attrs, "index")?;
                let expected = *self
                    .params
                    .get(index as usize)
                    .ok_or_else(|| format!("plan local {index} is outside parameters"))?;
                require_plan_ty(id, ty, expected)?;
                FragNodeKind::Local { index }
            }
            "const.bool" => {
                let value = plan_attr_bool(id, &attrs, "value")?;
                require_plan_ty(id, ty, FragTy::BoolI32)?;
                FragNodeKind::ConstBool(value)
            }
            "const.i64" => {
                let value = plan_attr_i64(id, &attrs, "value")?;
                require_plan_ty(id, ty, FragTy::I64)?;
                FragNodeKind::ConstI64(value)
            }
            "const.i32" => {
                let value = plan_attr_i32(id, &attrs, "value")?;
                require_plan_ty(id, ty, FragTy::RawI32)?;
                FragNodeKind::ConstI32(value)
            }
            "const.f64" => {
                let bits = plan_attr_u64_hex(id, &attrs, "bits")?;
                require_plan_ty(id, ty, FragTy::F64)?;
                FragNodeKind::ConstF64(bits)
            }
            "struct.get" => {
                let field = plan_attr_u32(id, &attrs, "field")?;
                let receiver = plan_attr_value(id, &attrs, "receiver")?;
                require_plan_node_ty(nodes, receiver, FragTy::IntCarrier)?;
                let expected = match field {
                    0 => FragTy::I64,
                    1 => FragTy::Ref,
                    2 => FragTy::RawI32,
                    _ => return Err(format!("unsupported carrier field {field} in plan")),
                };
                require_plan_ty(id, ty, expected)?;
                FragNodeKind::StructGet { field, receiver }
            }
            "struct.get.user" => {
                let ty_idx = plan_attr_u32(id, &attrs, "ty")?;
                let field = plan_attr_u32(id, &attrs, "field")?;
                let value = plan_attr_value(id, &attrs, "value")?;
                require_plan_node_ty(nodes, value, FragTy::AdtRef)?;
                require_plan_ty(id, ty, FragTy::AdtRef)?;
                FragNodeKind::StructGetUser {
                    ty_idx,
                    field,
                    value,
                }
            }
            "ref.is_null" => {
                let value = plan_attr_value(id, &attrs, "value")?;
                require_plan_node_ty(nodes, value, FragTy::Ref)?;
                if !matches!(
                    nodes.get(value.0).map(|node| &node.kind),
                    Some(FragNodeKind::StructGet { field: 1, .. })
                ) {
                    return Err(
                        "plan ref.is_null input is not a carrier limb field".to_string()
                    );
                }
                require_plan_ty(id, ty, FragTy::BoolI32)?;
                FragNodeKind::RefIsNull { value }
            }
            "prim" => {
                let op = attrs
                    .get("op")
                    .and_then(|raw| plan_prim_from_tag(raw))
                    .ok_or_else(|| format!("plan node v{} has unknown prim op", id.0))?;
                let args = plan_attr_values(id, &attrs, "args")?;
                let expected = check_plan_prim_args(id, op, &args, nodes)?;
                require_plan_ty(id, ty, expected)?;
                FragNodeKind::Prim { op, args }
            }
            "hostcall" => {
                let role = attrs
                    .get("role")
                    .map(String::as_str)
                    .and_then(FragHostRole::from_plan_tag)
                    .ok_or_else(|| format!("plan node v{} has unknown host role", id.0))?;
                let func_idx = plan_attr_u32(id, &attrs, "func")?;
                let args = plan_attr_values(id, &attrs, "args")?;
                let (expected_args, result_ty) = role.signature();
                if args.len() != expected_args.len() {
                    return Err(format!(
                        "plan hostcall v{} has {} args, expected {}",
                        id.0,
                        args.len(),
                        expected_args.len()
                    ));
                }
                for (arg, expected) in args.iter().zip(expected_args) {
                    require_plan_node_ty(nodes, *arg, *expected)?;
                }
                require_plan_ty(id, ty, result_ty)?;
                FragNodeKind::HostCall {
                    role,
                    func_idx,
                    args,
                }
            }
            "vector.get_or_default" => {
                let arr_ty = plan_attr_u32(id, &attrs, "arr_ty")?;
                let to_index_idx = plan_attr_u32(id, &attrs, "to_index")?;
                let box_idx = plan_attr_u32(id, &attrs, "box")?;
                let default = plan_attr_i64(id, &attrs, "default")?;
                // The monolithic template hard-references locals 0 (vector)
                // and 1 (index), mirroring the Lean typing rule.
                if self.params.first().copied() != Some(FragTy::AdtRef)
                    || self.params.get(1).copied() != Some(FragTy::IntCarrier)
                {
                    return Err(format!(
                        "plan fused vector read v{} requires (adt-ref, int-carrier) params",
                        id.0
                    ));
                }
                require_plan_ty(id, ty, FragTy::IntCarrier)?;
                FragNodeKind::VectorGetOrDefault {
                    arr_ty,
                    to_index_idx,
                    box_idx,
                    default,
                }
            }
            "if" => {
                let cond = plan_attr_value(id, &attrs, "cond")?;
                require_plan_node_ty(nodes, cond, FragTy::BoolI32)?;
                self.expect_exact("then")?;
                let then_block = self.parse_block()?;
                self.expect_exact("else")?;
                let else_block = self.parse_block()?;
                self.expect_exact("endif")?;
                let expected = then_block
                    .result_ty()
                    .ok_or_else(|| format!("plan if v{} then branch has no result", id.0))?;
                if else_block.result_ty() != Some(expected) {
                    return Err(format!("plan if v{} branch result types do not match", id.0));
                }
                require_plan_ty(id, ty, expected)?;
                FragNodeKind::If {
                    cond,
                    then_block: Box::new(then_block),
                    else_block: Box::new(else_block),
                }
            }
            _ => return Err(format!("unknown plan node kind `{kind_name}` in `{line}`")),
        };
        reject_extra_plan_attrs(id, kind_name, &attrs)?;
        nodes.push(FragNode { id, ty, kind });
        Ok(())
    }

    fn expect_exact(&mut self, expected: &str) -> Result<(), String> {
        let line = self.next_trim()?;
        if line == expected {
            Ok(())
        } else {
            Err(format!("expected plan line `{expected}`, got `{line}`"))
        }
    }

    fn next_trim(&mut self) -> Result<&'a str, String> {
        let line = self
            .lines
            .get(self.pos)
            .ok_or_else(|| "unexpected end of expr-fragment plan".to_string())?
            .trim();
        self.pos += 1;
        Ok(line)
    }

    fn peek_trim(&self) -> Result<&'a str, String> {
        self.lines
            .get(self.pos)
            .map(|line| line.trim())
            .ok_or_else(|| "unexpected end of expr-fragment plan".to_string())
    }
}

fn require_plan_ty(id: FragValueId, got: FragTy, expected: FragTy) -> Result<(), String> {
    if got == expected {
        Ok(())
    } else {
        Err(format!(
            "plan node v{} has type `{}`, expected `{}`",
            id.0,
            got.plan_tag(),
            expected.plan_tag()
        ))
    }
}

fn require_plan_node_ty(
    nodes: &[FragNode],
    id: FragValueId,
    expected: FragTy,
) -> Result<(), String> {
    let got = nodes
        .get(id.0)
        .ok_or_else(|| format!("plan references missing node v{}", id.0))?
        .ty;
    require_plan_ty(id, got, expected)
}

fn plan_attr<'a>(
    id: FragValueId,
    attrs: &'a std::collections::HashMap<String, String>,
    key: &str,
) -> Result<&'a str, String> {
    attrs
        .get(key)
        .map(String::as_str)
        .ok_or_else(|| format!("plan node v{} is missing `{key}`", id.0))
}

fn plan_attr_u32(
    id: FragValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<u32, String> {
    plan_attr(id, attrs, key)?
        .parse::<u32>()
        .map_err(|_| format!("plan node v{} has malformed `{key}`", id.0))
}

fn plan_attr_i32(
    id: FragValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<i32, String> {
    plan_attr(id, attrs, key)?
        .parse::<i32>()
        .map_err(|_| format!("plan node v{} has malformed `{key}`", id.0))
}

fn plan_attr_i64(
    id: FragValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<i64, String> {
    plan_attr(id, attrs, key)?
        .parse::<i64>()
        .map_err(|_| format!("plan node v{} has malformed `{key}`", id.0))
}

fn plan_attr_bool(
    id: FragValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<bool, String> {
    match plan_attr(id, attrs, key)? {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(format!("plan node v{} has malformed `{key}`", id.0)),
    }
}

fn plan_attr_u64_hex(
    id: FragValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<u64, String> {
    plan_attr(id, attrs, key)?
        .strip_prefix("0x")
        .and_then(|hex| u64::from_str_radix(hex, 16).ok())
        .ok_or_else(|| format!("plan node v{} has malformed `{key}`", id.0))
}

fn plan_attr_value(
    id: FragValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<FragValueId, String> {
    parse_plan_value_id(plan_attr(id, attrs, key)?)
        .ok_or_else(|| format!("plan node v{} has malformed `{key}`", id.0))
}

fn plan_attr_values(
    id: FragValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<Vec<FragValueId>, String> {
    let raw = plan_attr(id, attrs, key)?;
    if raw.is_empty() {
        return Ok(Vec::new());
    }
    raw.split(',')
        .map(|part| {
            parse_plan_value_id(part)
                .ok_or_else(|| format!("plan node v{} has malformed `{key}`", id.0))
        })
        .collect()
}

fn reject_extra_plan_attrs(
    id: FragValueId,
    kind: &str,
    attrs: &std::collections::HashMap<String, String>,
) -> Result<(), String> {
    let allowed: &[&str] = match kind {
        "local" => &["index"],
        "const.bool" => &["value"],
        "const.i64" => &["value"],
        "const.i32" => &["value"],
        "const.f64" => &["bits"],
        "struct.get" => &["field", "receiver"],
        "struct.get.user" => &["ty", "field", "value"],
        "ref.is_null" => &["value"],
        "prim" => &["op", "args"],
        "hostcall" => &["role", "func", "args"],
        "vector.get_or_default" => &["arr_ty", "to_index", "box", "default"],
        "if" => &["cond"],
        _ => &[],
    };
    if let Some(extra) = attrs.keys().find(|key| !allowed.contains(&key.as_str())) {
        return Err(format!(
            "plan node v{} has unexpected `{extra}` attribute",
            id.0
        ));
    }
    Ok(())
}

fn plan_prim_from_tag(tag: &str) -> Option<FragPrim> {
    match tag {
        "f64.add" => Some(FragPrim::F64Add),
        "f64.mul" => Some(FragPrim::F64Mul),
        "f64.le" => Some(FragPrim::F64Le),
        "f64.ge" => Some(FragPrim::F64Ge),
        "f64.lt" => Some(FragPrim::F64Lt),
        "f64.gt" => Some(FragPrim::F64Gt),
        "f64.eq" => Some(FragPrim::F64Eq),
        "i64.eq" => Some(FragPrim::I64Eq),
        "i64.le_s" => Some(FragPrim::I64LeS),
        "i64.lt_s" => Some(FragPrim::I64LtS),
        "i64.ge_s" => Some(FragPrim::I64GeS),
        "i64.gt_s" => Some(FragPrim::I64GtS),
        "i32.eq" => Some(FragPrim::I32Eq),
        "i32.lt_s" => Some(FragPrim::I32LtS),
        "i32.gt_s" => Some(FragPrim::I32GtS),
        _ => None,
    }
}

fn check_plan_prim_args(
    id: FragValueId,
    op: FragPrim,
    args: &[FragValueId],
    nodes: &[FragNode],
) -> Result<FragTy, String> {
    let expected_args: &[FragTy] = match op {
        FragPrim::F64Add
        | FragPrim::F64Mul
        | FragPrim::F64Le
        | FragPrim::F64Ge
        | FragPrim::F64Lt
        | FragPrim::F64Gt
        | FragPrim::F64Eq => &[FragTy::F64, FragTy::F64],
        FragPrim::I64Eq
        | FragPrim::I64LeS
        | FragPrim::I64LtS
        | FragPrim::I64GeS
        | FragPrim::I64GtS => &[FragTy::I64, FragTy::I64],
        FragPrim::I32Eq | FragPrim::I32LtS | FragPrim::I32GtS => {
            &[FragTy::RawI32, FragTy::RawI32]
        }
    };
    if args.len() != expected_args.len() {
        return Err(format!(
            "plan prim v{} has {} args, expected {}",
            id.0,
            args.len(),
            expected_args.len()
        ));
    }
    for (arg, expected) in args.iter().zip(expected_args) {
        if matches!(op, FragPrim::I32Eq | FragPrim::I32LtS | FragPrim::I32GtS) {
            let got = nodes
                .get(arg.0)
                .ok_or_else(|| format!("plan references missing node v{}", arg.0))?
                .ty;
            if !matches!(got, FragTy::RawI32 | FragTy::BoolI32) {
                return Err(format!(
                    "plan prim v{} arg v{} has type `{}`, expected i32",
                    id.0,
                    arg.0,
                    got.plan_tag()
                ));
            }
        } else {
            require_plan_node_ty(nodes, *arg, *expected)?;
        }
    }
    Ok(match op {
        FragPrim::F64Add | FragPrim::F64Mul => FragTy::F64,
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
        | FragPrim::I32GtS => FragTy::BoolI32,
    })
}

fn parse_plan_params(line: &str) -> Result<Vec<FragTy>, String> {
    let Some(rest) = line.strip_prefix("params") else {
        return Err(format!("malformed plan params line `{line}`"));
    };
    rest.split_whitespace()
        .map(|tag| {
            FragTy::from_plan_tag(tag).ok_or_else(|| format!("unknown plan type `{tag}`"))
        })
        .collect()
}

fn plan_ty_tags(items: &[FragTy]) -> Vec<&'static str> {
    items.iter().map(|ty| ty.plan_tag()).collect()
}

fn parse_plan_value_id(raw: &str) -> Option<FragValueId> {
    raw.strip_prefix('v')?.parse::<usize>().ok().map(FragValueId)
}

