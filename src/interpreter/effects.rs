use super::*;

impl Interpreter {
    pub(super) fn builtin_namespace(name: &str) -> Option<&str> {
        name.split_once('.').map(|(namespace, _)| namespace)
    }

    pub(super) fn builtin_effects(name: &str) -> &'static [&'static str] {
        match Self::builtin_namespace(name) {
            Some("Args") => args::effects(name),
            Some("Console") => console::effects(name),
            Some("Http") => http::effects(name),
            Some("HttpServer") => http_server::effects(name),
            Some("Disk") => disk::effects(name),
            Some("Env") => env::effects(name),
            Some("Random") => random::effects(name),
            Some("Tcp") => tcp::effects(name),
            #[cfg(feature = "terminal")]
            Some("Terminal") => terminal::effects(name),
            Some("Time") => time::effects(name),
            Some("Bool") => bool::effects(name),
            Some("Int") => int::effects(name),
            Some("Float") => float::effects(name),
            Some("String") => string::effects(name),
            Some("List") => list::effects(name),
            Some("Map") => map::effects(name),
            Some("Char") => char::effects(name),
            Some("Byte") => byte::effects(name),
            Some("Result") => result::effects(name),
            Some("Option") => option::effects(name),
            _ => &[],
        }
    }

    pub(super) fn current_allowed_effects(&self) -> &[String] {
        self.call_stack
            .last()
            .map(|frame| frame.effects.as_ref().as_slice())
            .unwrap_or(&[])
    }

    pub(super) fn runtime_chain_with(&self, callee_name: &str) -> String {
        let mut chain = if self.call_stack.is_empty() {
            vec!["<top-level>".to_string()]
        } else {
            self.call_stack
                .iter()
                .map(|frame| frame.name.as_ref().clone())
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
        let mut missing = Vec::new();
        for effect in required_effects {
            if !self
                .current_allowed_effects()
                .iter()
                .any(|a| crate::effects::effect_satisfies(a, effect))
            {
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
