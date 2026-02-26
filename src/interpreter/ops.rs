use super::*;

impl Interpreter {
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

    pub fn aver_eq(&self, a: &Value, b: &Value) -> bool {
        if let (Some(xs), Some(ys)) = (list_slice(a), list_slice(b)) {
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
            _ => false,
        }
    }

    pub(super) fn op_add(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 + y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x + *y as f64)),
            (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
            _ => Err(RuntimeError::Error(
                "Operator '+' does not support these types".to_string(),
            )),
        }
    }

    pub(super) fn op_sub(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 - y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x - *y as f64)),
            _ => Err(RuntimeError::Error(
                "Operator '-' does not support these types".to_string(),
            )),
        }
    }

    pub(super) fn op_mul(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x * y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 * y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x * *y as f64)),
            _ => Err(RuntimeError::Error(
                "Operator '*' does not support these types".to_string(),
            )),
        }
    }

    pub(super) fn op_div(&self, a: Value, b: Value) -> Result<Value, RuntimeError> {
        match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => {
                if *y == 0 {
                    Err(RuntimeError::Error("Division by zero".to_string()))
                } else {
                    Ok(Value::Int(x / y))
                }
            }
            (Value::Float(x), Value::Float(y)) => {
                if *y == 0.0 {
                    Err(RuntimeError::Error("Division by zero".to_string()))
                } else {
                    Ok(Value::Float(x / y))
                }
            }
            (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 / y)),
            (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x / *y as f64)),
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
            (Value::Int(x), Value::Float(y)) => {
                let x = *x as f64;
                match op {
                    "<" => x < *y,
                    ">" => x > *y,
                    "<=" => x <= *y,
                    ">=" => x >= *y,
                    _ => unreachable!(),
                }
            }
            (Value::Float(x), Value::Int(y)) => {
                let y = *y as f64;
                match op {
                    "<" => *x < y,
                    ">" => *x > y,
                    "<=" => *x <= y,
                    ">=" => *x >= y,
                    _ => unreachable!(),
                }
            }
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
                )))
            }
        };
        Ok(Value::Bool(result))
    }
}
