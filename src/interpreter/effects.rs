use super::*;

impl Interpreter {
    pub(super) fn builtin_effects(name: &str) -> &'static [&'static str] {
        let e = console::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = http::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = http_server::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = disk::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = tcp::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = int::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = float::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = string::effects(name);
        if !e.is_empty() {
            return e;
        }
        let e = list::effects(name);
        if !e.is_empty() {
            return e;
        }
        map::effects(name)
    }

    pub(super) fn current_allowed_effects(&self) -> &[String] {
        self.call_stack
            .last()
            .map(|frame| frame.effects.as_slice())
            .unwrap_or(&[])
    }

    pub(super) fn runtime_chain_with(&self, callee_name: &str) -> String {
        let mut chain = if self.call_stack.is_empty() {
            vec!["<top-level>".to_string()]
        } else {
            self.call_stack
                .iter()
                .map(|frame| frame.name.clone())
                .collect()
        };
        chain.push(callee_name.to_string());
        chain.join(" -> ")
    }

    pub(super) fn ensure_effects_allowed<'a, I>(
        &self,
        callee_name: &str,
        required_effects: I,
    ) -> Result<(), RuntimeError>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let allowed = self.current_allowed_effects();
        let mut missing = Vec::new();
        for effect in required_effects {
            if !allowed.iter().any(|e| e == effect) {
                missing.push(effect.to_string());
            }
        }

        if missing.is_empty() {
            return Ok(());
        }

        let caller = self
            .call_stack
            .last()
            .map(|frame| frame.name.as_str())
            .unwrap_or("<top-level>");
        let chain = self.runtime_chain_with(callee_name);
        Err(RuntimeError::Error(format!(
            "Runtime effect violation: '{}' cannot call '{}' (missing effect(s): {}) [chain: {}]",
            caller,
            callee_name,
            missing.join(", "),
            chain
        )))
    }
}
