use std::collections::HashMap;

use crate::nan_value::NanValue;

use super::builtin::VmBuiltin;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct VmVariantCtor {
    pub(crate) type_id: u32,
    pub(crate) variant_id: u16,
    pub(crate) ctor_id: u32,
    pub(crate) field_count: u8,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum VmSymbolKind {
    Function(u32),
    Builtin(VmBuiltin),
    Namespace,
    VariantCtor(VmVariantCtor),
    Wrapper(u8),
    Constant(NanValue),
}

#[derive(Debug, Clone)]
pub(crate) struct VmSymbolInfo {
    pub(crate) name: String,
    pub(crate) kind: Option<VmSymbolKind>,
    pub(crate) required_effects: Vec<u32>,
    members: HashMap<u32, NanValue>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct VmSymbolTable {
    symbols: Vec<VmSymbolInfo>,
    by_name: HashMap<String, u32>,
}

impl VmSymbolTable {
    #[inline]
    pub(crate) fn symbol_ref(symbol_id: u32) -> NanValue {
        NanValue::new_int_inline(symbol_id as i64)
    }

    pub(crate) fn intern_name(&mut self, name: &str) -> u32 {
        if let Some(&symbol_id) = self.by_name.get(name) {
            return symbol_id;
        }
        let symbol_id = self.symbols.len() as u32;
        self.symbols.push(VmSymbolInfo {
            name: name.to_string(),
            kind: None,
            required_effects: Vec::new(),
            members: HashMap::new(),
        });
        self.by_name.insert(name.to_string(), symbol_id);
        symbol_id
    }

    pub(crate) fn intern_namespace(&mut self, name: &str) -> u32 {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Namespace) => {}
            None => info.kind = Some(VmSymbolKind::Namespace),
            Some(other) => panic!("VM symbol '{}' already exists as {:?}", name, other),
        }
        symbol_id
    }

    pub(crate) fn intern_namespace_path(&mut self, path: &str) -> u32 {
        let mut current_id = None;
        let mut current_path = String::new();

        for segment in path.split('.') {
            if segment.is_empty() {
                continue;
            }
            if !current_path.is_empty() {
                current_path.push('.');
            }
            current_path.push_str(segment);

            let child_id = self.intern_namespace(&current_path);
            if let Some(parent_id) = current_id {
                let member_symbol_id = self.intern_name(segment);
                self.add_namespace_member_by_id(
                    parent_id,
                    member_symbol_id,
                    Self::symbol_ref(child_id),
                );
            }
            current_id = Some(child_id);
        }

        current_id.expect("intern_namespace_path() requires a non-empty path")
    }

    pub(crate) fn intern_function(&mut self, name: &str, fn_id: u32, effects: &[String]) -> u32 {
        let symbol_id = self.intern_name(name);
        let required_effects = self.intern_effects(effects.iter().map(String::as_str));
        let info = &mut self.symbols[symbol_id as usize];
        match &mut info.kind {
            Some(VmSymbolKind::Function(existing_fn_id)) => {
                *existing_fn_id = fn_id;
            }
            None => {
                info.kind = Some(VmSymbolKind::Function(fn_id));
            }
            Some(other) => panic!("VM symbol '{}' already exists as {:?}", name, other),
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
            None => {
                info.kind = Some(VmSymbolKind::Builtin(builtin));
            }
            Some(other) => panic!(
                "VM symbol '{}' already exists as {:?}",
                builtin.name(),
                other
            ),
        }
        info.required_effects = required_effects;
        symbol_id
    }

    pub(crate) fn intern_variant_ctor(&mut self, name: &str, ctor: VmVariantCtor) -> u32 {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::VariantCtor(existing)) => {
                debug_assert_eq!(existing, ctor);
            }
            None => info.kind = Some(VmSymbolKind::VariantCtor(ctor)),
            Some(other) => panic!("VM symbol '{}' already exists as {:?}", name, other),
        }
        symbol_id
    }

    pub(crate) fn intern_wrapper(&mut self, name: &str, wrap_kind: u8) -> u32 {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Wrapper(existing)) => {
                debug_assert_eq!(existing, wrap_kind);
            }
            None => info.kind = Some(VmSymbolKind::Wrapper(wrap_kind)),
            Some(other) => panic!("VM symbol '{}' already exists as {:?}", name, other),
        }
        symbol_id
    }

    pub(crate) fn intern_constant(&mut self, name: &str, value: NanValue) -> u32 {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Constant(existing)) => {
                debug_assert_eq!(existing.bits(), value.bits());
            }
            None => info.kind = Some(VmSymbolKind::Constant(value)),
            Some(other) => panic!("VM symbol '{}' already exists as {:?}", name, other),
        }
        symbol_id
    }

    #[cfg(test)]
    pub(crate) fn add_namespace_member(
        &mut self,
        namespace: &str,
        member: &str,
        value: NanValue,
    ) -> u32 {
        let namespace_id = self.intern_namespace(namespace);
        let member_symbol_id = self.intern_name(member);
        self.add_namespace_member_by_id(namespace_id, member_symbol_id, value);
        member_symbol_id
    }

    pub(crate) fn add_namespace_member_by_id(
        &mut self,
        namespace_symbol_id: u32,
        member_symbol_id: u32,
        value: NanValue,
    ) {
        let info = &mut self.symbols[namespace_symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Namespace) => {
                info.members.insert(member_symbol_id, value);
            }
            None => panic!("VM symbol '{}' is not registered as namespace", info.name),
            Some(other) => panic!("VM symbol '{}' is {:?}", info.name, other),
        }
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
            _ => None,
        }
    }

    pub(crate) fn resolve_builtin(&self, symbol_id: u32) -> Option<VmBuiltin> {
        match self.get(symbol_id)?.kind {
            Some(VmSymbolKind::Builtin(builtin)) => Some(builtin),
            _ => None,
        }
    }

    pub(crate) fn resolve_variant_ctor(&self, symbol_id: u32) -> Option<VmVariantCtor> {
        match self.get(symbol_id)?.kind {
            Some(VmSymbolKind::VariantCtor(ctor)) => Some(ctor),
            _ => None,
        }
    }

    pub(crate) fn resolve_wrapper(&self, symbol_id: u32) -> Option<u8> {
        match self.get(symbol_id)?.kind {
            Some(VmSymbolKind::Wrapper(kind)) => Some(kind),
            _ => None,
        }
    }

    pub(crate) fn resolve_constant(&self, symbol_id: u32) -> Option<NanValue> {
        match self.get(symbol_id)?.kind {
            Some(VmSymbolKind::Constant(value)) => Some(value),
            _ => None,
        }
    }

    pub(crate) fn is_namespace(&self, symbol_id: u32) -> bool {
        matches!(
            self.get(symbol_id).and_then(|info| info.kind),
            Some(VmSymbolKind::Namespace)
        )
    }

    pub(crate) fn resolve_member(
        &self,
        namespace_symbol_id: u32,
        member_symbol_id: u32,
    ) -> Option<NanValue> {
        self.get(namespace_symbol_id)?
            .members
            .get(&member_symbol_id)
            .copied()
    }

    pub(crate) fn resolve_symbol_ref(&self, value: NanValue) -> Option<u32> {
        let symbol_id = value.inline_int_value()?;
        (symbol_id >= 0)
            .then_some(symbol_id as u32)
            .filter(|&id| self.get(id).is_some_and(|info| info.kind.is_some()))
    }

    pub(crate) fn resolve_namespace_path(&self, path: &str) -> Option<u32> {
        if let Some(symbol_id) = self.find(path)
            && self.is_namespace(symbol_id)
        {
            return Some(symbol_id);
        }

        let mut segments = path.split('.');
        let first = segments.next()?;
        let mut current_id = self.find(first)?;
        if !self.is_namespace(current_id) {
            return None;
        }

        for segment in segments {
            let member_symbol_id = self.find(segment)?;
            let member = self.resolve_member(current_id, member_symbol_id)?;
            current_id = self.resolve_symbol_ref(member)?;
            if !self.is_namespace(current_id) {
                return None;
            }
        }

        Some(current_id)
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
    use super::{VmSymbolKind, VmSymbolTable, VmVariantCtor};
    use crate::nan_value::NanValue;
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

    #[test]
    fn namespace_members_can_point_at_symbols_and_constants() {
        let mut table = VmSymbolTable::default();
        let ns = table.intern_namespace("Option");
        let some = table.intern_wrapper("Option.Some", 2);
        table.add_namespace_member("Option", "Some", VmSymbolTable::symbol_ref(some));
        table.add_namespace_member("Option", "None", NanValue::NONE);

        let some_member = table.find("Some").unwrap();
        let none_member = table.find("None").unwrap();

        assert_eq!(
            table.resolve_member(ns, some_member).map(NanValue::bits),
            Some(VmSymbolTable::symbol_ref(some).bits())
        );
        assert_eq!(
            table.resolve_member(ns, none_member).map(NanValue::bits),
            Some(NanValue::NONE.bits())
        );
    }

    #[test]
    fn variant_ctor_symbols_keep_ctor_metadata() {
        let mut table = VmSymbolTable::default();
        let ctor = VmVariantCtor {
            type_id: 4,
            variant_id: 2,
            ctor_id: 9,
            field_count: 0,
        };
        let symbol_id = table.intern_variant_ctor("Status.Done", ctor);

        assert_eq!(table.resolve_variant_ctor(symbol_id), Some(ctor));
    }

    #[test]
    fn symbol_table_builds_and_resolves_nested_namespace_paths() {
        let mut table = VmSymbolTable::default();
        let path = table.intern_namespace_path("Domain.Types");

        assert_eq!(table.find("Domain.Types"), Some(path));

        let domain = table.find("Domain").expect("missing Domain");
        let types = table.find("Types").expect("missing Types");
        assert_eq!(
            table
                .resolve_member(domain, types)
                .and_then(|value| table.resolve_symbol_ref(value)),
            Some(path)
        );
        assert_eq!(table.resolve_namespace_path("Domain.Types"), Some(path));
    }
}
