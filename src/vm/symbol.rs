use std::collections::HashMap;

use super::builtin::VmBuiltin;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum VmSymbolKind {
    Function(u32),
    Builtin(VmBuiltin),
}

#[derive(Debug, Clone)]
pub(crate) struct VmSymbolInfo {
    pub(crate) name: String,
    pub(crate) kind: Option<VmSymbolKind>,
    pub(crate) required_effects: Vec<u32>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct VmSymbolTable {
    symbols: Vec<VmSymbolInfo>,
    by_name: HashMap<String, u32>,
}

impl VmSymbolTable {
    pub(crate) fn intern_name(&mut self, name: &str) -> u32 {
        if let Some(&symbol_id) = self.by_name.get(name) {
            return symbol_id;
        }
        let symbol_id = self.symbols.len() as u32;
        self.symbols.push(VmSymbolInfo {
            name: name.to_string(),
            kind: None,
            required_effects: Vec::new(),
        });
        self.by_name.insert(name.to_string(), symbol_id);
        symbol_id
    }

    pub(crate) fn intern_function(&mut self, name: &str, fn_id: u32, effects: &[String]) -> u32 {
        let symbol_id = self.intern_name(name);
        let required_effects = self.intern_effects(effects.iter().map(String::as_str));
        let info = &mut self.symbols[symbol_id as usize];
        match &mut info.kind {
            Some(VmSymbolKind::Function(existing_fn_id)) => {
                *existing_fn_id = fn_id;
            }
            Some(VmSymbolKind::Builtin(_)) => {
                panic!("VM symbol '{}' already exists as builtin", name);
            }
            None => {
                info.kind = Some(VmSymbolKind::Function(fn_id));
            }
        }
        info.required_effects = required_effects;
        symbol_id
    }

    pub(crate) fn intern_builtin(&mut self, builtin: VmBuiltin) -> u32 {
        let symbol_id = self.intern_name(builtin.name());
        let required_effects = self.intern_effects(builtin.effects().iter().copied());
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Builtin(existing)) => {
                debug_assert_eq!(existing, builtin);
            }
            Some(VmSymbolKind::Function(_)) => {
                panic!("VM symbol '{}' already exists as function", builtin.name());
            }
            None => {
                info.kind = Some(VmSymbolKind::Builtin(builtin));
            }
        }
        info.required_effects = required_effects;
        symbol_id
    }

    pub(crate) fn find(&self, name: &str) -> Option<u32> {
        self.by_name.get(name).copied()
    }

    pub(crate) fn get(&self, symbol_id: u32) -> Option<&VmSymbolInfo> {
        self.symbols.get(symbol_id as usize)
    }

    pub(crate) fn resolve_function(&self, symbol_id: u32) -> Option<u32> {
        match self.get(symbol_id)?.kind {
            Some(VmSymbolKind::Function(fn_id)) => Some(fn_id),
            Some(VmSymbolKind::Builtin(_)) | None => None,
        }
    }

    pub(crate) fn resolve_builtin(&self, symbol_id: u32) -> Option<VmBuiltin> {
        match self.get(symbol_id)?.kind {
            Some(VmSymbolKind::Builtin(builtin)) => Some(builtin),
            Some(VmSymbolKind::Function(_)) | None => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn required_effects(&self, symbol_id: u32) -> Option<&[u32]> {
        Some(self.get(symbol_id)?.required_effects.as_slice())
    }

    fn intern_effects<'a>(&mut self, effects: impl IntoIterator<Item = &'a str>) -> Vec<u32> {
        effects
            .into_iter()
            .map(|name| self.intern_name(name))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::{VmSymbolKind, VmSymbolTable};
    use crate::vm::builtin::VmBuiltin;

    #[test]
    fn symbol_table_interns_functions_and_builtins() {
        let mut table = VmSymbolTable::default();
        let fn_sym = table.intern_function("main", 7, &[]);
        let builtin_sym = table.intern_builtin(VmBuiltin::StringReplace);

        assert_eq!(table.find("main"), Some(fn_sym));
        assert_eq!(table.find("String.replace"), Some(builtin_sym));
        assert_eq!(table.resolve_function(fn_sym), Some(7));
        assert_eq!(
            table.resolve_builtin(builtin_sym),
            Some(VmBuiltin::StringReplace)
        );
        assert!(matches!(
            table.get(fn_sym).and_then(|info| info.kind),
            Some(VmSymbolKind::Function(7))
        ));
        assert_eq!(table.required_effects(fn_sym), Some([].as_slice()));
    }

    #[test]
    fn symbol_table_reuses_builtin_name_for_effect() {
        let mut table = VmSymbolTable::default();
        let effect_sym = table.intern_name("Console.print");
        let builtin_sym = table.intern_builtin(VmBuiltin::ConsolePrint);

        assert_eq!(effect_sym, builtin_sym);
        assert_eq!(
            table.required_effects(builtin_sym),
            Some([builtin_sym].as_slice())
        );
    }
}
