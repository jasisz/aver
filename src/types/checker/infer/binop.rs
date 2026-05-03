use super::*;

impl TypeChecker {
    pub(in super::super) fn check_binop_expr(
        &mut self,
        op: &BinOp,
        left: &Spanned<Expr>,
        _right: &Spanned<Expr>,
        lt: &Type,
        rt: &Type,
        line: usize,
    ) {
        // Unary minus: `- float_expr` is parsed as `BinOp(Sub, Literal(Int(0)), expr)`.
        // Allow Int(0) - Float to produce Float without requiring explicit conversion.
        if matches!(op, BinOp::Sub)
            && matches!(lt, Type::Int)
            && matches!(rt, Type::Float)
            && matches!(left.node, Expr::Literal(Literal::Int(0)))
        {
            return; // Unary minus on Float — OK
        }
        self.check_binop(op, lt, rt, line);
    }

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
                if !Self::constraint_compatible(lt, rt) && !Self::constraint_compatible(rt, lt) {
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
