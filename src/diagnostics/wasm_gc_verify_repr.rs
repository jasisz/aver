//! Synthesized `-> String` helpers that render compound verify values.
//!
//! `wasm_gc_verify` compares `lhs == rhs` inside wasm and only decodes a
//! Bool host-side. To print the ACTUAL value when that Bool is false —
//! what the VM lane's `aver_repr` produces — this module writes small
//! pure Aver functions (`fn __verify_repr_N(v: T) -> String`), one per
//! compound type the cases mention, and hands their source text back so
//! the caller can parse them into the same item list the whole verify
//! program is compiled from. The output format follows `aver_repr` /
//! `aver_repr_inner` in `src/value.rs`: bare variant names, `f: v`
//! record fields, inner-position strings quoted.
//!
//! Only `match`, `+` on Strings, `String.fromInt`, `String.fromFloat`
//! and calls to other synthesized helpers are used — no string
//! interpolation, so generated code cannot nest quotes. `Map<K, V>` and
//! `Fn` deliberately synthesize nothing: the VM sorts map entries by the
//! key's string repr, which a wasm-side helper cannot reproduce.
#![cfg(feature = "wasm")]

use std::collections::{HashMap, HashSet};

use crate::ast::{TopLevel, TypeDef, TypeVariant};
use crate::types::Type;

/// One type definition plus the module context it was declared under.
/// Entry-module types carry `""`; a dep module's types carry its
/// `ModuleInfo.prefix`, which is also how bare field type names written
/// inside that dep resolve.
struct TypeEntry {
    /// The `Type::Named.name` a stamped type carries for this def: the
    /// bare name for entry types, `Prefix.Name` for dependency types.
    stamped_name: String,
    /// Bare TypeDef name — what the typedef itself declares.
    bare_name: String,
    /// Module prefix used to resolve bare field type names written in
    /// this type's own module (`""` for the entry module).
    prefix: String,
    def: TypeDef,
}

/// Memo state for one requested type.
enum ReprState {
    /// A helper name is reserved and its body is still being generated —
    /// recursive references return the name without descending again.
    Generating(String),
    Done(String),
    /// Synthesis was attempted and the type (or a field of it) is not
    /// renderable.
    Failed,
}

/// Synthesizes Aver `-> String` helper functions for compound types.
///
/// One instance per verify run: feed it every stamped case type through
/// [`Self::helper_for`], then collect the source text with
/// [`Self::take_sources`].
pub(super) struct ReprSynth {
    types: Vec<TypeEntry>,
    memo: HashMap<String, ReprState>,
    /// Helper name → helper names its generated body calls. Recursive
    /// types can leave a done helper referencing one that later failed;
    /// `take_sources` drops anything on such a chain.
    calls: HashMap<String, Vec<String>>,
    sources: Vec<(String, String)>,
    next: usize,
    bool_helper: Option<String>,
}

impl ReprSynth {
    /// `entry_items` contributes the entry module's own type defs;
    /// `dep_modules` contributes each dependency's (`Type::Named` stamps
    /// on dep-typed case expressions carry `Dep.Name`).
    pub(super) fn new(
        entry_items: &[TopLevel],
        dep_modules: &[crate::codegen::ModuleInfo],
    ) -> Self {
        let mut types = Vec::new();
        for item in entry_items {
            if let TopLevel::TypeDef(def) = item {
                types.push(TypeEntry {
                    stamped_name: type_def_name(def).to_string(),
                    bare_name: type_def_name(def).to_string(),
                    prefix: String::new(),
                    def: def.clone(),
                });
            }
        }
        for module in dep_modules {
            for def in &module.type_defs {
                types.push(TypeEntry {
                    stamped_name: format!("{}.{}", module.prefix, type_def_name(def)),
                    bare_name: type_def_name(def).to_string(),
                    prefix: module.prefix.clone(),
                    def: def.clone(),
                });
            }
        }
        Self {
            types,
            memo: HashMap::new(),
            calls: HashMap::new(),
            sources: Vec::new(),
            next: 0,
            bool_helper: None,
        }
    }

    /// The `-> String` helper rendering a value of `ty` in inner mode
    /// (strings quoted), or `None` when `ty` is not renderable.
    pub(super) fn helper_for(&mut self, ty: &Type) -> Option<String> {
        let key = ty.display();
        match self.memo.get(&key) {
            Some(ReprState::Generating(name)) | Some(ReprState::Done(name)) => {
                return Some(name.clone());
            }
            Some(ReprState::Failed) => return None,
            None => {}
        }
        let name = format!("__verify_repr_{}", self.next);
        self.next += 1;
        self.memo
            .insert(key.clone(), ReprState::Generating(name.clone()));
        match self.generate(ty, &name) {
            Some(source) => {
                self.sources.push((name.clone(), source));
                self.memo.insert(key, ReprState::Done(name.clone()));
                Some(name)
            }
            None => {
                self.memo.insert(key, ReprState::Failed);
                None
            }
        }
    }

    /// The generated helper source snippets, minus any that transitively
    /// call a helper whose own synthesis failed (a type that was still
    /// `Generating` when a sibling referenced it can fail afterwards).
    pub(super) fn take_sources(self) -> Vec<String> {
        // Every emitted helper starts kept; a helper that failed was
        // never emitted, so any caller mentioning its name falls out in
        // the sweep below — recursively, since the drop can cascade.
        let mut keep: HashSet<String> = self.sources.iter().map(|(name, _)| name.clone()).collect();
        loop {
            let before = keep.len();
            let still: HashSet<String> = keep
                .iter()
                .filter(|name| {
                    self.calls
                        .get(*name)
                        .is_none_or(|deps| deps.iter().all(|dep| keep.contains(dep)))
                })
                .cloned()
                .collect();
            keep = still;
            if keep.len() == before {
                break;
            }
        }
        self.sources
            .into_iter()
            .filter(|(name, _)| keep.contains(name))
            .map(|(_, source)| source)
            .collect()
    }

    /// Generate the helper for `ty` under `name`. Returns the full
    /// snippet text, `None` when the type is not renderable.
    fn generate(&mut self, ty: &Type, name: &str) -> Option<String> {
        let annotation = ty.display();
        let body = match ty {
            Type::Option(inner) => {
                let some = self.inner_expr("x", inner, "", name)?;
                format!(
                    "    match v\n        Option.Some(x) -> \"Option.Some(\" + {some} + \")\"\n        Option.None -> \"Option.None\""
                )
            }
            Type::Result(ok, err) => {
                let ok_e = self.inner_expr("x", ok, "", name)?;
                let err_e = self.inner_expr("e", err, "", name)?;
                format!(
                    "    match v\n        Result.Ok(x) -> \"Result.Ok(\" + {ok_e} + \")\"\n        Result.Err(e) -> \"Result.Err(\" + {err_e} + \")\""
                )
            }
            Type::Tuple(items) => {
                let vars: Vec<String> = (0..items.len()).map(|i| format!("a{i}")).collect();
                let mut parts = Vec::with_capacity(items.len());
                for (var, item_ty) in vars.iter().zip(items.iter()) {
                    parts.push(self.inner_expr(var, item_ty, "", name)?);
                }
                format!(
                    "    match v\n        ({}) -> \"(\" + {} + \")\"",
                    vars.join(", "),
                    parts.join(" + \", \" + ")
                )
            }
            Type::List(inner) => {
                self.list_items_helper(name, inner)?;
                let items_name = format!("{name}_items");
                self.calls
                    .entry(name.to_string())
                    .or_default()
                    .push(items_name.clone());
                format!("    \"[\" + {items_name}(v, \"\") + \"]\"")
            }
            Type::Vector(inner) => {
                // `Vector[...]` — the items walker is the same List
                // helper, fed through `List.fromVector`.
                self.list_items_helper(name, inner)?;
                let items_name = format!("{name}_items");
                self.calls
                    .entry(name.to_string())
                    .or_default()
                    .push(items_name.clone());
                format!("    \"Vector[\" + {items_name}(List.fromVector(v), \"\") + \"]\"")
            }
            Type::Named { name: stamped, .. } => {
                let (prefix, def) = {
                    let entry = self.find_type(stamped)?;
                    (entry.prefix.clone(), entry.def.clone())
                };
                match &def {
                    TypeDef::Sum { variants, .. } => {
                        self.sum_body(stamped, variants, &prefix, name)?
                    }
                    TypeDef::Product { fields, .. } => {
                        let mut parts = Vec::with_capacity(fields.len());
                        for (field_name, field_ty_str) in fields {
                            let field_ty = parse_field_type(field_ty_str, &prefix);
                            let inner = self.inner_expr(
                                &format!("v.{field_name}"),
                                &field_ty,
                                &prefix,
                                name,
                            )?;
                            parts.push(format!("\"{field_name}: \" + {inner}"));
                        }
                        // The tag prints the stamped name — what the VM's
                        // `type_name` carries (qualified for dep records).
                        format!(
                            "    \"{stamped}(\" + {} + \")\"",
                            parts.join(" + \", \" + ")
                        )
                    }
                }
            }
            _ => return None,
        };
        Some(format!("fn {name}(v: {annotation}) -> String\n{body}"))
    }

    /// One match arm per variant of a sum. `stamped` is the name
    /// patterns qualify with; the OUTPUT uses the bare variant name, the
    /// way `aver_repr` prints `Waiting(Claim(2, ...))`.
    fn sum_body(
        &mut self,
        stamped: &str,
        variants: &[TypeVariant],
        prefix: &str,
        owner: &str,
    ) -> Option<String> {
        let mut arms = Vec::with_capacity(variants.len());
        for variant in variants {
            if variant.fields.is_empty() {
                arms.push(format!(
                    "        {stamped}.{} -> \"{}\"",
                    variant.name, variant.name
                ));
                continue;
            }
            let vars: Vec<String> = (0..variant.fields.len()).map(|i| format!("a{i}")).collect();
            let mut parts = Vec::with_capacity(vars.len());
            for (var, field_ty_str) in vars.iter().zip(variant.fields.iter()) {
                let field_ty = parse_field_type(field_ty_str, prefix);
                parts.push(self.inner_expr(var, &field_ty, prefix, owner)?);
            }
            arms.push(format!(
                "        {stamped}.{}({}) -> \"{}(\" + {} + \")\"",
                variant.name,
                vars.join(", "),
                variant.name,
                parts.join(" + \", \" + ")
            ));
        }
        Some(format!("    match v\n{}", arms.join("\n")))
    }

    /// The `{name}_items(v: List<X>, sep: String) -> String` walker a
    /// `List`/`Vector` helper delegates to, emitted as its own snippet.
    fn list_items_helper(&mut self, name: &str, inner: &Type) -> Option<()> {
        let items_name = format!("{name}_items");
        let head = self.inner_expr("x", inner, "", &items_name)?;
        let list = Type::List(Box::new(inner.clone())).display();
        self.sources.push((
            items_name.clone(),
            format!(
                "fn {items_name}(v: {list}, sep: String) -> String\n    match v\n        [] -> \"\"\n        [x, ..rest] -> sep + {head} + {items_name}(rest, \", \")"
            ),
        ));
        Some(())
    }

    /// Expression text rendering `var` (a value of `ty`) in inner mode.
    /// `prefix` is the module the surrounding type was declared in;
    /// `owner` is the helper whose body this expression lands in.
    fn inner_expr(&mut self, var: &str, ty: &Type, prefix: &str, owner: &str) -> Option<String> {
        let expr = match ty {
            Type::Int => format!("String.fromInt({var})"),
            Type::Float => format!("String.fromFloat({var})"),
            Type::Bool => {
                let helper = self.bool_helper();
                format!("{helper}({var})")
            }
            Type::Str => format!("\"\\\"\" + {var} + \"\\\"\""),
            Type::Unit => "\"Unit\"".to_string(),
            other => {
                let helper = self.helper_for(other).or_else(|| {
                    // A bare name inside a dep module's field annotation
                    // resolves in that module, not in the entry's.
                    let bare = other.named_name()?;
                    if prefix.is_empty() || bare.contains('.') {
                        return None;
                    }
                    self.helper_for(&Type::named(format!("{prefix}.{bare}")))
                })?;
                self.calls
                    .entry(owner.to_string())
                    .or_default()
                    .push(helper.clone());
                format!("{helper}({var})")
            }
        };
        Some(expr)
    }

    /// Resolve a `Type::Named` stamp to its TypeDef. Entry definitions
    /// win on a bare name; a dep matches `Prefix.Name` or, when no entry
    /// type claims the bare name, its own bare name.
    fn find_type(&self, stamped: &str) -> Option<&TypeEntry> {
        self.types
            .iter()
            .find(|entry| entry.prefix.is_empty() && entry.bare_name == stamped)
            .or_else(|| {
                self.types
                    .iter()
                    .find(|entry| !entry.prefix.is_empty() && entry.stamped_name == stamped)
            })
            .or_else(|| {
                self.types
                    .iter()
                    .find(|entry| !entry.prefix.is_empty() && entry.bare_name == stamped)
            })
    }

    /// The shared `Bool -> String` helper, synthesized once. (The VM
    /// prints `true`/`false`; `String.fromBool` exists but a match keeps
    /// this independent of which builtins a wasm-gc build wires up.)
    fn bool_helper(&mut self) -> String {
        if let Some(name) = &self.bool_helper {
            return name.clone();
        }
        let name = "__verify_repr_bool".to_string();
        self.sources.push((
            name.clone(),
            format!(
                "fn {name}(v: Bool) -> String\n    match v\n        true -> \"true\"\n        false -> \"false\""
            ),
        ));
        self.bool_helper = Some(name.clone());
        name
    }
}

fn type_def_name(def: &TypeDef) -> &str {
    match def {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
    }
}

/// Field annotations inside a TypeDef are strings. A bare name resolves
/// in the module that wrote it, so `Leaf` inside dep `Dep` is `Dep.Leaf`
/// when that qualified type exists; everything else parses as written.
fn parse_field_type(annotation: &str, prefix: &str) -> Type {
    let ty = crate::types::parse_type_str(annotation);
    match &ty {
        Type::Named { name, .. } if !prefix.is_empty() && !name.contains('.') => {
            Type::named(format!("{prefix}.{name}"))
        }
        _ => ty,
    }
}
