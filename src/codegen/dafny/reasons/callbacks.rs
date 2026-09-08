//! A named callback carries the same checked source cone as a direct call.
//! Resolving its type alone would hide unsupported work or a recursion edge.

use super::*;

impl<'a> Checker<'a> {
    pub(super) fn function_value(&mut self, name: &str) -> Result<Type, String> {
        let ctx = self.ctx;
        let scope = ctx.active_module_scope();
        let id = ctx
            .symbol_table
            .resolve_fn_id_in(name, scope.as_deref())
            .ok_or_else(|| format!("no declared pure callback {name}"))?;
        let key = &ctx.symbol_table.fn_entry(id).key;
        if self.checking_functions.contains(&id) {
            return Err(format!(
                "callback {} closes a hidden higher-order recursion cycle",
                key.canonical()
            ));
        }
        let fd = ctx
            .fn_def_by_name(&key.name, key.scope_str())
            .ok_or_else(|| format!("no source definition for callback {}", key.canonical()))?;
        let parameters = ctx.with_module_scope(key.scope_str(), || {
            fd.params
                .iter()
                .map(|(_, annotation)| self.annotation(annotation))
                .collect::<Result<Vec<_>, _>>()
        })?;
        // These are typed checker-local arguments, never emitted source names.
        // function_call still validates purity, termination, the complete body,
        // and all transitive dependencies in the callback's declaring scope.
        let mut locals = Env::new();
        let mut arguments = Vec::with_capacity(parameters.len());
        for (index, ty) in parameters.iter().enumerate() {
            let argument = format!("aver_callback_arg_{index}");
            bind(&mut locals, &argument, ty.clone())?;
            arguments.push(Spanned::bare(Expr::Ident(argument)));
        }
        let result = self.function_call(id, &arguments, &locals)?;
        Ok(Type::Fn(parameters, Box::new(result), vec![]))
    }
}
