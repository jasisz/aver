use super::*;

impl Interpreter {
    #[allow(dead_code)]
    pub(super) fn eval_binop(
        &self,
        op: &BinOp,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match op {
            BinOp::Add => self.op_add(left, right),
            BinOp::Sub => self.op_sub(left, right),
            BinOp::Mul => self.op_mul(left, right),
            BinOp::Div => self.op_div(left, right),
            BinOp::Eq => Ok(Value::Bool(self.aver_eq(&left, &right))),
            BinOp::Neq => Ok(Value::Bool(!self.aver_eq(&left, &right))),
            BinOp::Lt => self.op_compare(&left, &right, "<"),
            BinOp::Gt => self.op_compare(&left, &right, ">"),
            BinOp::Lte => self.op_compare(&left, &right, "<="),
            BinOp::Gte => self.op_compare(&left, &right, ">="),
        }
    }

    /// NanValue-native binop — avoids Value conversion.
    pub(super) fn eval_binop_nv(
        &mut self,
        op: &BinOp,
        left: NanValue,
        right: NanValue,
    ) -> Result<NanValue, RuntimeError> {
        match op {
            BinOp::Add => self.op_add_nv(left, right),
            BinOp::Sub => self.op_sub_nv(left, right),
            BinOp::Mul => self.op_mul_nv(left, right),
            BinOp::Div => self.op_div_nv(left, right),
            BinOp::Eq => Ok(NanValue::new_bool(left.eq_in(right, &self.arena))),
            BinOp::Neq => Ok(NanValue::new_bool(!left.eq_in(right, &self.arena))),
            BinOp::Lt => self.op_compare_nv(left, right, "<"),
            BinOp::Gt => self.op_compare_nv(left, right, ">"),
            BinOp::Lte => self.op_compare_nv(left, right, "<="),
            BinOp::Gte => self.op_compare_nv(left, right, ">="),
        }
    }

    pub fn aver_eq(&self, a: &Value, b: &Value) -> bool {
        if let (Some(xs), Some(ys)) = (list_view(a), list_view(b)) {
            return xs.len() == ys.len()
                && xs.iter().zip(ys.iter()).all(|(x, y)| self.aver_eq(x, y));
        }

        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::Str(x), Value::Str(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Unit, Value::Unit) => true,
            (Value::None, Value::None) => true,
            (Value::Ok(x), Value::Ok(y)) => self.aver_eq(x, y),
            (Value::Err(x), Value::Err(y)) => self.aver_eq(x, y),
            (Value::Some(x), Value::Some(y)) => self.aver_eq(x, y),
            (Value::Tuple(xs), Value::Tuple(ys)) => {
                xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(x, y)| self.aver_eq(x, y))
            }
            (Value::Map(m1), Value::Map(m2)) => {
                m1.len() == m2.len()
                    && m1
                        .iter()
                        .all(|(k, v1)| m2.get(k).map(|v2| self.aver_eq(v1, v2)).unwrap_or(false))
            }
            (
                Value::Variant {
                    type_name: t1,
                    variant: v1,
                    fields: f1,
                },
                Value::Variant {
                    type_name: t2,
                    variant: v2,
                    fields: f2,
                },
            ) => {
                t1 == t2
                    && v1 == v2
                    && f1.len() == f2.len()
                    && f1.iter().zip(f2.iter()).all(|(x, y)| self.aver_eq(x, y))
            }
            (
                Value::Record {
                    type_name: t1,
                    fields: f1,
                },
                Value::Record {
                    type_name: t2,
                    fields: f2,
                },
            ) => {
                t1 == t2
                    && f1.len() == f2.len()
                    && f1
                        .iter()
                        .zip(f2.iter())
                        .all(|((k1, v1), (k2, v2))| k1 == k2 && self.aver_eq(v1, v2))
            }
            (Value::Vector(a), Value::Vector(b)) => {
                a.len() == b.len()
                    && (0..a.len()).all(|i| match (a.get(i), b.get(i)) {
                        (Some(x), Some(y)) => self.aver_eq(x, y),
                        _ => false,
                    })
            }
            _ => false,
        }
    }

    pub(super) fn op_add(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (Value::Str(mut x), Value::Str(y)) => {
                x.push_str(&y);
                Ok(Value::Str(x))
            }
            _ => Err(RuntimeError::Error(
                "Operator '+' does not support these types".to_string(),
            )),
        }
    }

    pub(super) fn op_sub(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
            // Unary minus: `- float_expr` is parsed as `0 - expr`
            (Value::Int(0), Value::Float(y)) => Ok(Value::Float(-y)),
            _ => Err(RuntimeError::Error(
                "Operator '-' does not support these types".to_string(),
            )),
        }
    }

    pub(super) fn op_mul(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x * y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
            _ => Err(RuntimeError::Error(
                "Operator '*' does not support these types".to_string(),
            )),
        }
    }

    pub(super) fn op_div(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => {
                if *y == 0 {
                    Err(RuntimeError::Error("division by zero".to_string()))
                } else {
                    Ok(Value::Int(x / y))
                }
            }
            (Value::Float(x), Value::Float(y)) => {
                if *y == 0.0 {
                    Err(RuntimeError::Error("division by zero".to_string()))
                } else {
                    Ok(Value::Float(x / y))
                }
            }
            _ => Err(RuntimeError::Error(
                "Operator '/' does not support these types".to_string(),
            )),
        }
    }

    pub(super) fn op_compare(&self, a: &Value, b: &Value, op: &str) -> Result<Value, RuntimeError> {
        let result = match (a, b) {
            (Value::Int(x), Value::Int(y)) => match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            },
            (Value::Float(x), Value::Float(y)) => match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            },
            (Value::Str(x), Value::Str(y)) => match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            },
            _ => {
                return Err(RuntimeError::Error(format!(
                    "Operator '{}' does not support these types",
                    op
                )));
            }
        };
        Ok(Value::Bool(result))
    }

    // -- NanValue-native ops --------------------------------------------------

    fn op_add_nv(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, RuntimeError> {
        if a.is_int() && b.is_int() {
            let result = a.as_int(&self.arena) + b.as_int(&self.arena);
            return Ok(NanValue::new_int(result, &mut self.arena));
        }
        if a.is_float() && b.is_float() {
            return Ok(NanValue::new_float(a.as_float() + b.as_float()));
        }
        if a.is_string() && b.is_string() {
            let sa = self.arena.get_string_value(a);
            let sb = self.arena.get_string_value(b);
            let mut buf = String::with_capacity(sa.len() + sb.len());
            buf.push_str(sa.as_str());
            buf.push_str(sb.as_str());
            return Ok(NanValue::new_string_value(&buf, &mut self.arena));
        }
        Err(RuntimeError::Error(
            "Operator '+' does not support these types".to_string(),
        ))
    }

    fn op_sub_nv(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, RuntimeError> {
        if a.is_int() && b.is_int() {
            let result = a.as_int(&self.arena) - b.as_int(&self.arena);
            return Ok(NanValue::new_int(result, &mut self.arena));
        }
        if a.is_float() && b.is_float() {
            return Ok(NanValue::new_float(a.as_float() - b.as_float()));
        }
        // Unary minus: 0 - float
        if a.is_int() && a.as_int(&self.arena) == 0 && b.is_float() {
            return Ok(NanValue::new_float(-b.as_float()));
        }
        Err(RuntimeError::Error(
            "Operator '-' does not support these types".to_string(),
        ))
    }

    fn op_mul_nv(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, RuntimeError> {
        if a.is_int() && b.is_int() {
            let result = a.as_int(&self.arena) * b.as_int(&self.arena);
            return Ok(NanValue::new_int(result, &mut self.arena));
        }
        if a.is_float() && b.is_float() {
            return Ok(NanValue::new_float(a.as_float() * b.as_float()));
        }
        Err(RuntimeError::Error(
            "Operator '*' does not support these types".to_string(),
        ))
    }

    fn op_div_nv(&mut self, a: NanValue, b: NanValue) -> Result<NanValue, RuntimeError> {
        if a.is_int() && b.is_int() {
            let bv = b.as_int(&self.arena);
            if bv == 0 {
                return Err(RuntimeError::Error("division by zero".to_string()));
            }
            let result = a.as_int(&self.arena) / bv;
            return Ok(NanValue::new_int(result, &mut self.arena));
        }
        if a.is_float() && b.is_float() {
            let bv = b.as_float();
            if bv == 0.0 {
                return Err(RuntimeError::Error("division by zero".to_string()));
            }
            return Ok(NanValue::new_float(a.as_float() / bv));
        }
        Err(RuntimeError::Error(
            "Operator '/' does not support these types".to_string(),
        ))
    }

    fn op_compare_nv(&self, a: NanValue, b: NanValue, op: &str) -> Result<NanValue, RuntimeError> {
        if a.is_int() && b.is_int() {
            let x = a.as_int(&self.arena);
            let y = b.as_int(&self.arena);
            let result = match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            };
            return Ok(NanValue::new_bool(result));
        }
        if a.is_float() && b.is_float() {
            let x = a.as_float();
            let y = b.as_float();
            let result = match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            };
            return Ok(NanValue::new_bool(result));
        }
        if a.is_string() && b.is_string() {
            let x = self.arena.get_string_value(a);
            let y = self.arena.get_string_value(b);
            let result = match op {
                "<" => x < y,
                ">" => x > y,
                "<=" => x <= y,
                ">=" => x >= y,
                _ => unreachable!(),
            };
            return Ok(NanValue::new_bool(result));
        }
        Err(RuntimeError::Error(format!(
            "Operator '{}' does not support these types",
            op
        )))
    }
}
