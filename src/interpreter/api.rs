use super::*;

impl Interpreter {
    pub fn callable_declared_effects(fn_val: &Value) -> Vec<String> {
        match fn_val {
            Value::Fn { effects, .. } => effects.clone(),
            _ => vec![],
        }
    }

    // Public wrapper so main.rs can call call_value
    pub fn call_value_pub(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.call_value(fn_val, args)
    }

    pub fn call_value_with_effects_pub(
        &mut self,
        fn_val: Value,
        args: Vec<Value>,
        entry_name: &str,
        allowed_effects: Vec<String>,
    ) -> Result<Value, RuntimeError> {
        self.call_stack.push(CallFrame {
            name: entry_name.to_string(),
            effects: allowed_effects,
        });
        let result = self.call_value(fn_val, args);
        self.call_stack.pop();
        result
    }
}
