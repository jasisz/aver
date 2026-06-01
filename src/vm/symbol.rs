use std::collections::HashMap;

use crate::nan_value::NanValue;

use super::builtin::VmBuiltin;

/// Iron — B2: errors `VmSymbolTable` raises when the program tries
/// to register two different kinds of meaning under the same name.
/// Pre-Iron these were unconditional `panic!`s — a malformed compile
/// input crashed the whole process. They now surface as `Result`
/// rejections that `ProgramCompiler` lifts into a normal
/// `CompileError`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SymbolError {
    /// `name` was already registered with a *different* `VmSymbolKind`
    /// — e.g. interned as a `Function` then re-interned as a
    /// `Namespace`, or two `intern_constant` calls with different
    /// payloads.
    KindConflict { name: String, existing: String },
    /// `add_namespace_member_by_id` was called on a symbol whose
    /// kind is anything other than `Namespace` (including `None` —
    /// the slot was interned but never typed).
    NotANamespace { name: String, existing: String },
    /// `intern_namespace_path` was handed a path that produced no
    /// segments (empty string / "." / "..").
    EmptyNamespacePath,
}

impl std::fmt::Display for SymbolError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolError::KindConflict { name, existing } => {
                write!(f, "VM symbol '{name}' already exists as {existing}")
            }
            SymbolError::NotANamespace { name, existing } => {
                write!(
                    f,
                    "VM symbol '{name}' is not registered as namespace (got {existing})"
                )
            }
            SymbolError::EmptyNamespacePath => {
                write!(f, "intern_namespace_path requires a non-empty path")
            }
        }
    }
}

impl std::error::Error for SymbolError {}

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

    pub(crate) fn intern_namespace(&mut self, name: &str) -> Result<u32, SymbolError> {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Namespace) => {}
            None => info.kind = Some(VmSymbolKind::Namespace),
            Some(other) => {
                return Err(SymbolError::KindConflict {
                    name: name.to_string(),
                    existing: format!("{:?}", other),
                });
            }
        }
        Ok(symbol_id)
    }

    pub(crate) fn intern_namespace_path(&mut self, path: &str) -> Result<u32, SymbolError> {
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

            let child_id = self.intern_namespace(&current_path)?;
            if let Some(parent_id) = current_id {
                let member_symbol_id = self.intern_name(segment);
                self.add_namespace_member_by_id(
                    parent_id,
                    member_symbol_id,
                    Self::symbol_ref(child_id),
                )?;
            }
            current_id = Some(child_id);
        }

        current_id.ok_or(SymbolError::EmptyNamespacePath)
    }

    pub(crate) fn intern_function(
        &mut self,
        name: &str,
        fn_id: u32,
        effects: &[String],
    ) -> Result<u32, SymbolError> {
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
            Some(other) => {
                return Err(SymbolError::KindConflict {
                    name: name.to_string(),
                    existing: format!("{:?}", other),
                });
            }
        }
        info.required_effects = required_effects;
        Ok(symbol_id)
    }

    pub(crate) fn intern_builtin(&mut self, builtin: VmBuiltin) -> Result<u32, SymbolError> {
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
            Some(other) => {
                return Err(SymbolError::KindConflict {
                    name: builtin.name().to_string(),
                    existing: format!("{:?}", other),
                });
            }
        }
        info.required_effects = required_effects;
        Ok(symbol_id)
    }

    pub(crate) fn intern_variant_ctor(
        &mut self,
        name: &str,
        ctor: VmVariantCtor,
    ) -> Result<u32, SymbolError> {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::VariantCtor(existing)) => {
                debug_assert_eq!(existing, ctor);
            }
            None => info.kind = Some(VmSymbolKind::VariantCtor(ctor)),
            Some(other) => {
                return Err(SymbolError::KindConflict {
                    name: name.to_string(),
                    existing: format!("{:?}", other),
                });
            }
        }
        Ok(symbol_id)
    }

    pub(crate) fn intern_wrapper(&mut self, name: &str, wrap_kind: u8) -> Result<u32, SymbolError> {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Wrapper(existing)) => {
                debug_assert_eq!(existing, wrap_kind);
            }
            None => info.kind = Some(VmSymbolKind::Wrapper(wrap_kind)),
            Some(other) => {
                return Err(SymbolError::KindConflict {
                    name: name.to_string(),
                    existing: format!("{:?}", other),
                });
            }
        }
        Ok(symbol_id)
    }

    pub(crate) fn intern_constant(
        &mut self,
        name: &str,
        value: NanValue,
    ) -> Result<u32, SymbolError> {
        let symbol_id = self.intern_name(name);
        let info = &mut self.symbols[symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Constant(existing)) => {
                debug_assert_eq!(existing.bits(), value.bits());
            }
            None => info.kind = Some(VmSymbolKind::Constant(value)),
            Some(other) => {
                return Err(SymbolError::KindConflict {
                    name: name.to_string(),
                    existing: format!("{:?}", other),
                });
            }
        }
        Ok(symbol_id)
    }

    #[cfg(test)]
    pub(crate) fn add_namespace_member(
        &mut self,
        namespace: &str,
        member: &str,
        value: NanValue,
    ) -> Result<u32, SymbolError> {
        let namespace_id = self.intern_namespace(namespace)?;
        let member_symbol_id = self.intern_name(member);
        self.add_namespace_member_by_id(namespace_id, member_symbol_id, value)?;
        Ok(member_symbol_id)
    }

    pub(crate) fn add_namespace_member_by_id(
        &mut self,
        namespace_symbol_id: u32,
        member_symbol_id: u32,
        value: NanValue,
    ) -> Result<(), SymbolError> {
        let info = &mut self.symbols[namespace_symbol_id as usize];
        match info.kind {
            Some(VmSymbolKind::Namespace) => {
                info.members.insert(member_symbol_id, value);
                Ok(())
            }
            None => Err(SymbolError::NotANamespace {
                name: info.name.clone(),
                existing: "uninitialised".to_string(),
            }),
            Some(other) => Err(SymbolError::NotANamespace {
                name: info.name.clone(),
                existing: format!("{:?}", other),
            }),
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
        let fn_sym = table.intern_function("main", 7, &[]).expect("intern main");
        let builtin_sym = table
            .intern_builtin(VmBuiltin::StringReplace)
            .expect("intern String.replace");

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
        let builtin_sym = table
            .intern_builtin(VmBuiltin::ConsolePrint)
            .expect("intern Console.print");

        assert_eq!(effect_sym, builtin_sym);
        assert_eq!(
            table.required_effects(builtin_sym),
            Some([builtin_sym].as_slice())
        );
    }

    #[test]
    fn namespace_members_can_point_at_symbols_and_constants() {
        let mut table = VmSymbolTable::default();
        let ns = table.intern_namespace("Option").expect("intern Option ns");
        let some = table
            .intern_wrapper("Option.Some", 2)
            .expect("intern Option.Some");
        table
            .add_namespace_member("Option", "Some", VmSymbolTable::symbol_ref(some))
            .expect("add Option.Some member");
        table
            .add_namespace_member("Option", "None", NanValue::NONE)
            .expect("add Option.None member");

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
        let symbol_id = table
            .intern_variant_ctor("Status.Done", ctor)
            .expect("intern Status.Done");

        assert_eq!(table.resolve_variant_ctor(symbol_id), Some(ctor));
    }
}
