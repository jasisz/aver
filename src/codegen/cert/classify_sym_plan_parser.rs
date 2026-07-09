struct SymPlanParser<'a> {
    lines: Vec<&'a str>,
    pos: usize,
    params: Vec<SymTy>,
    result: SymTy,
}

impl<'a> SymPlanParser<'a> {
    fn new(text: &'a str, params: Vec<SymTy>, result: SymTy) -> Self {
        Self {
            lines: text.lines().collect(),
            pos: 0,
            params,
            result,
        }
    }

    fn parse(&mut self) -> Result<SymBlock, String> {
        self.expect_exact("aver.sym-fragment.plan.v1")?;
        self.expect_exact("profile sym-fragment-v1")?;
        let params_line = self.next_trim()?;
        let params = parse_sym_plan_params(params_line)?;
        if params != self.params {
            return Err(format!(
                "source plan parameter types {:?} do not match wasm-derived source types {:?}",
                sym_ty_tags(&params),
                sym_ty_tags(&self.params)
            ));
        }
        let result_line = self.next_trim()?;
        let result = result_line
            .strip_prefix("result ")
            .and_then(SymTy::from_plan_tag)
            .ok_or_else(|| format!("malformed source plan result line `{result_line}`"))?;
        if result != self.result {
            return Err(format!(
                "source plan result type `{}` does not match wasm-derived result type `{}`",
                result.plan_tag(),
                self.result.plan_tag()
            ));
        }
        self.expect_exact("body")?;
        let block = self.parse_block()?;
        if block.result_ty() != Some(self.result) {
            return Err("source plan root type does not match function result type".to_string());
        }
        if self.pos != self.lines.len() {
            return Err(format!(
                "unexpected trailing source plan line `{}`",
                self.lines[self.pos].trim()
            ));
        }
        Ok(block)
    }

    fn parse_block(&mut self) -> Result<SymBlock, String> {
        let header = self.next_trim()?;
        let declared_result = header
            .strip_prefix("block result=")
            .and_then(parse_sym_value_id)
            .ok_or_else(|| format!("malformed source plan block header `{header}`"))?;
        let mut nodes = Vec::<SymNode>::new();
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
                "source plan block declares missing/noncanonical result v{}",
                declared_result.0
            ));
        }
        if declared_result.0 + 1 != nodes.len() {
            return Err(format!(
                "source plan block result v{} is not the final node v{}",
                declared_result.0,
                nodes.len().saturating_sub(1)
            ));
        }
        Ok(SymBlock {
            nodes,
            result: declared_result,
        })
    }

    fn parse_node(&mut self, nodes: &mut Vec<SymNode>) -> Result<(), String> {
        let line = self.next_trim()?;
        let mut parts = line.split_whitespace();
        let id = parts
            .next()
            .and_then(parse_sym_value_id)
            .ok_or_else(|| format!("malformed source plan node id in `{line}`"))?;
        if id.0 != nodes.len() {
            return Err(format!(
                "source plan node id v{} is not the next node id v{}",
                id.0,
                nodes.len()
            ));
        }
        let ty = parts
            .next()
            .and_then(|tok| tok.strip_prefix("ty="))
            .and_then(SymTy::from_plan_tag)
            .ok_or_else(|| format!("malformed source plan node type in `{line}`"))?;
        let kind_name = parts
            .next()
            .ok_or_else(|| format!("missing source plan node kind in `{line}`"))?;
        let attrs = parts
            .map(|tok| {
                let (k, v) = tok
                    .split_once('=')
                    .ok_or_else(|| format!("malformed source plan attribute `{tok}`"))?;
                Ok((k.to_string(), v.to_string()))
            })
            .collect::<Result<std::collections::HashMap<_, _>, String>>()?;
        let kind = match kind_name {
            "param" => {
                let index = plan_attr_u32(FragValueId(id.0), &attrs, "index")?;
                let expected = *self
                    .params
                    .get(index as usize)
                    .ok_or_else(|| format!("source plan param {index} is outside parameters"))?;
                require_sym_plan_ty(id, ty, expected)?;
                SymNodeKind::Param { index }
            }
            "const.bool" => {
                let value = plan_attr_bool(FragValueId(id.0), &attrs, "value")?;
                require_sym_plan_ty(id, ty, SymTy::Bool)?;
                SymNodeKind::ConstBool(value)
            }
            "const.float" => {
                let bits = plan_attr_u64_hex(FragValueId(id.0), &attrs, "bits")?;
                require_sym_plan_ty(id, ty, SymTy::Float)?;
                SymNodeKind::ConstFloatBits(bits)
            }
            "prim" => {
                let op = attrs
                    .get("op")
                    .and_then(|raw| SymPrim::from_plan_tag(raw))
                    .ok_or_else(|| format!("source plan node v{} has unknown prim op", id.0))?;
                let args = sym_plan_attr_values(id, &attrs, "args")?;
                let expected = check_sym_plan_prim_args(id, op, &args, nodes)?;
                require_sym_plan_ty(id, ty, expected)?;
                SymNodeKind::Prim { op, args }
            }
            "if" => {
                let cond = sym_plan_attr_value(id, &attrs, "cond")?;
                require_sym_plan_node_ty(nodes, cond, SymTy::Bool)?;
                self.expect_exact("then")?;
                let then_block = self.parse_block()?;
                self.expect_exact("else")?;
                let else_block = self.parse_block()?;
                self.expect_exact("endif")?;
                let expected = then_block
                    .result_ty()
                    .ok_or_else(|| format!("source plan if v{} then branch has no result", id.0))?;
                if else_block.result_ty() != Some(expected) {
                    return Err(format!(
                        "source plan if v{} branch result types do not match",
                        id.0
                    ));
                }
                require_sym_plan_ty(id, ty, expected)?;
                SymNodeKind::If {
                    cond,
                    then_block: Box::new(then_block),
                    else_block: Box::new(else_block),
                }
            }
            _ => return Err(format!("unknown source plan node kind `{kind_name}` in `{line}`")),
        };
        reject_extra_sym_plan_attrs(id, kind_name, &attrs)?;
        nodes.push(SymNode { id, ty, kind });
        Ok(())
    }

    fn expect_exact(&mut self, expected: &str) -> Result<(), String> {
        let line = self.next_trim()?;
        if line == expected {
            Ok(())
        } else {
            Err(format!("expected source plan line `{expected}`, got `{line}`"))
        }
    }

    fn next_trim(&mut self) -> Result<&'a str, String> {
        let line = self
            .lines
            .get(self.pos)
            .ok_or_else(|| "unexpected end of sym-fragment plan".to_string())?
            .trim();
        self.pos += 1;
        Ok(line)
    }

    fn peek_trim(&self) -> Result<&'a str, String> {
        self.lines
            .get(self.pos)
            .map(|line| line.trim())
            .ok_or_else(|| "unexpected end of sym-fragment plan".to_string())
    }
}

impl SymBlock {
    fn result_ty(&self) -> Option<SymTy> {
        self.nodes
            .get(self.result.0)
            .filter(|node| node.id == self.result)
            .map(|node| node.ty)
    }
}

fn require_sym_plan_ty(id: SymValueId, got: SymTy, expected: SymTy) -> Result<(), String> {
    if got == expected {
        Ok(())
    } else {
        Err(format!(
            "source plan node v{} has type `{}`, expected `{}`",
            id.0,
            got.plan_tag(),
            expected.plan_tag()
        ))
    }
}

fn require_sym_plan_node_ty(
    nodes: &[SymNode],
    id: SymValueId,
    expected: SymTy,
) -> Result<(), String> {
    let got = nodes
        .get(id.0)
        .ok_or_else(|| format!("source plan references missing node v{}", id.0))?
        .ty;
    require_sym_plan_ty(id, got, expected)
}

fn sym_plan_attr_value(
    id: SymValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<SymValueId, String> {
    parse_sym_value_id(plan_attr(FragValueId(id.0), attrs, key)?)
        .ok_or_else(|| format!("source plan node v{} has malformed `{key}`", id.0))
}

fn sym_plan_attr_values(
    id: SymValueId,
    attrs: &std::collections::HashMap<String, String>,
    key: &str,
) -> Result<Vec<SymValueId>, String> {
    let raw = plan_attr(FragValueId(id.0), attrs, key)?;
    if raw.is_empty() {
        return Ok(Vec::new());
    }
    raw.split(',')
        .map(|part| {
            parse_sym_value_id(part)
                .ok_or_else(|| format!("source plan node v{} has malformed `{key}`", id.0))
        })
        .collect()
}

fn reject_extra_sym_plan_attrs(
    id: SymValueId,
    kind: &str,
    attrs: &std::collections::HashMap<String, String>,
) -> Result<(), String> {
    let allowed: &[&str] = match kind {
        "param" => &["index"],
        "const.bool" => &["value"],
        "const.float" => &["bits"],
        "prim" => &["op", "args"],
        "if" => &["cond"],
        _ => &[],
    };
    if let Some(extra) = attrs.keys().find(|key| !allowed.contains(&key.as_str())) {
        return Err(format!(
            "source plan node v{} has unexpected `{extra}` attribute",
            id.0
        ));
    }
    Ok(())
}

fn check_sym_plan_prim_args(
    id: SymValueId,
    op: SymPrim,
    args: &[SymValueId],
    nodes: &[SymNode],
) -> Result<SymTy, String> {
    let expected_args: &[SymTy] = match op {
        SymPrim::FloatAdd | SymPrim::FloatMul | SymPrim::FloatLe => {
            &[SymTy::Float, SymTy::Float]
        }
    };
    if args.len() != expected_args.len() {
        return Err(format!(
            "source plan prim v{} has {} args, expected {}",
            id.0,
            args.len(),
            expected_args.len()
        ));
    }
    for (arg, expected) in args.iter().zip(expected_args) {
        require_sym_plan_node_ty(nodes, *arg, *expected)?;
    }
    Ok(match op {
        SymPrim::FloatAdd | SymPrim::FloatMul => SymTy::Float,
        SymPrim::FloatLe => SymTy::Bool,
    })
}

fn parse_sym_plan_params(line: &str) -> Result<Vec<SymTy>, String> {
    let Some(rest) = line.strip_prefix("params") else {
        return Err(format!("malformed source plan params line `{line}`"));
    };
    rest.split_whitespace()
        .map(|tag| {
            SymTy::from_plan_tag(tag).ok_or_else(|| format!("unknown source plan type `{tag}`"))
        })
        .collect()
}

fn sym_ty_tags(items: &[SymTy]) -> Vec<&'static str> {
    items.iter().map(|ty| ty.plan_tag()).collect()
}

fn parse_sym_value_id(raw: &str) -> Option<SymValueId> {
    raw.strip_prefix('v')?.parse::<usize>().ok().map(SymValueId)
}
