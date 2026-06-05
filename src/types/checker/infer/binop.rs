use super::*;

impl TypeChecker {
    pub(in super::super) fn check_binop(&mut self, op: &BinOp, lt: &Type, rt: &Type, line: usize) {
        if matches!(lt, Type::Invalid) || matches!(rt, Type::Invalid) {
            return; // gradual — skip
        }
        match op {
            BinOp::Add => {
                let ok = (matches!(lt, Type::Int) && matches!(rt, Type::Int))
                    || (matches!(lt, Type::Float) && matches!(rt, Type::Float))
                    || (matches!(lt, Type::Str) && matches!(rt, Type::Str));
                if !ok {
                    self.error_at_line(line, format!(
                        "Operator '+' requires matching types (Int+Int, Float+Float, or String+String), got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Div if matches!(lt, Type::Int) && matches!(rt, Type::Int) => {
                // Integer `/` is the lone partial operator: it traps at
                // runtime on division by zero. Aver makes partiality
                // explicit — route integer division through the
                // `Int.div : Result<Int, String>` builtin instead. Float
                // `/` (and Int/Float mixed → Float) stays total below.
                self.error_at_line(
                    line,
                    "integer division by '/' is not allowed (it can divide by zero); \
                     use Int.div(a, b) : Result<Int, String>"
                        .to_string(),
                );
            }
            BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let ok = (matches!(lt, Type::Int) && matches!(rt, Type::Int))
                    || (matches!(lt, Type::Float) && matches!(rt, Type::Float));
                if !ok {
                    self.error_at_line(line, format!(
                        "Arithmetic operator requires matching numeric types (Int+Int or Float+Float), got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Eq | BinOp::Neq => {
                if !self.compatible(lt, rt) && !self.compatible(rt, lt) {
                    self.error_at_line(
                        line,
                        format!(
                            "Equality operator requires same types, got {} and {}",
                            lt.display(),
                            rt.display()
                        ),
                    );
                }
            }
            BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                let ok = (matches!(lt, Type::Int) && matches!(rt, Type::Int))
                    || (matches!(lt, Type::Float) && matches!(rt, Type::Float))
                    || (matches!(lt, Type::Str) && matches!(rt, Type::Str));
                if !ok {
                    self.error_at_line(
                        line,
                        format!(
                            "Comparison operator requires matching types, got {} and {}",
                            lt.display(),
                            rt.display()
                        ),
                    );
                }
            }
        }
    }
}
