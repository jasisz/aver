//! Register every composite type a function body produces.
//!
//! `TypeRegistry::build` discovers `List` / `Option` / `Result` / `Map` /
//! `Vector` / tuple instantiations from signatures, record fields, binding
//! annotations and a hand-kept table of builtin calls. A value whose type is
//! spelled nowhere else — `List.zip([1], [s()])` or
//! `Option.Some(1) == Option.None` written straight in `main` — slipped past
//! all of them, and the module then failed validation for want of the slot or
//! its helpers.
//!
//! This pass reads the checker's type stamp on every expression of every body
//! instead of guessing, and registers what is still missing. It runs after the
//! ordinary discovery and appends: a program whose types were all found
//! before gets no new slot, so its module stays byte-for-byte what it was.
//! Each new slot also gets the companions the ordinary discovery gives that
//! kind (`Map<K,V>` brings `Option<V>`, `List<K>`, `List<V>`, `(K, V)` and
//! `List<(K, V)>`; `List<T>` brings `Vector<T>`; and so on), so the helpers
//! that read them find what they expect.

use crate::ast::{Spanned, Type};
use crate::ir::hir::{
    ResolvedCallee, ResolvedExpr, ResolvedFnBody, ResolvedFnDef, ResolvedStmt, ResolvedStrPart,
};

use super::types::{MapSlots, TypeRegistry, VectorSlots, normalize_compound, parse_map_kv};

impl TypeRegistry {
    /// Append a slot for every composite type a body expression carries that
    /// discovery has not registered yet. Called once, after the flattener's
    /// type-name aliases are installed, so a qualified spelling of a type
    /// already registered under its bare name is recognised as the same type.
    pub(super) fn register_body_value_types(&mut self, resolved_fn_defs: &[ResolvedFnDef]) {
        let mut stamped: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
            for stmt in stmts {
                let (ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e)) = stmt;
                collect_stamps(e, &mut stamped);
            }
        }
        // Discovery versions every vector once the program holds a Vector
        // value; a body stamp naming one is such a value.
        let versioned = !self.vector_versions.is_empty()
            || stamped
                .iter()
                .any(|canonical| canonical.contains("Vector<"));
        for canonical in stamped {
            self.ensure(&canonical);
        }
        if versioned {
            self.version_vectors();
        }
    }

    /// Register `canonical` (whitespace-free, tuples as `Tuple<..>`) and its
    /// components, plus the companions its kind brings. A type already
    /// registered under any spelling the lookups accept is left alone.
    fn ensure(&mut self, canonical: &str) {
        if let Some(inner) = strip(canonical, "List<") {
            self.ensure(inner);
            if self.list_type_idx(canonical).is_none() {
                let idx = self.next_slot();
                self.list_types.insert(canonical.to_string(), idx);
                self.list_order.push(canonical.to_string());
                self.ensure(&format!("Vector<{inner}>"));
            }
        } else if let Some(inner) = strip(canonical, "Vector<") {
            self.ensure(inner);
            if self.vector_type_idx(canonical).is_none() {
                let idx = self.next_slot();
                self.vector_types.insert(canonical.to_string(), idx);
                self.vector_order.push(canonical.to_string());
                self.ensure(&format!("Option<{inner}>"));
                self.ensure(&format!("List<{inner}>"));
                self.ensure(&format!("Option<{canonical}>"));
            }
        } else if let Some(inner) = strip(canonical, "Option<") {
            self.ensure(inner);
            if self.option_type_idx(canonical).is_none() {
                let idx = self.next_slot();
                self.option_types.insert(canonical.to_string(), idx);
                self.option_order.push(canonical.to_string());
            }
        } else if canonical.starts_with("Result<") {
            if let Some((ok, err)) = TypeRegistry::result_te(canonical) {
                let (ok, err) = (ok.to_string(), err.to_string());
                self.ensure(&ok);
                self.ensure(&err);
            }
            if self.result_type_idx(canonical).is_none() {
                let idx = self.next_slot();
                self.result_types.insert(canonical.to_string(), idx);
                self.result_order.push(canonical.to_string());
            }
        } else if canonical.starts_with("Tuple<") {
            if let Some(items) = TypeRegistry::tuple_elements(canonical) {
                let items: Vec<String> = items.into_iter().map(str::to_string).collect();
                for item in &items {
                    self.ensure(item);
                }
            }
            if self.tuple_type_idx(canonical).is_none() {
                let idx = self.next_slot();
                self.tuple_types.insert(canonical.to_string(), idx);
                self.tuple_order.push(canonical.to_string());
                self.ensure(&format!("List<{canonical}>"));
            }
        } else if canonical.starts_with("Map<") {
            self.ensure_map(canonical);
        }
    }

    fn ensure_map(&mut self, canonical: &str) {
        let Some((k, v)) = parse_map_kv(canonical) else {
            return;
        };
        let (k, v) = (normalize_compound(k), normalize_compound(v));
        self.ensure(&k);
        self.ensure(&v);
        if self.map_slots(canonical).is_some() {
            return;
        }
        self.ensure(&format!("Option<{v}>"));
        let keys_array = self.next_slot();
        let values_array = self.next_slot();
        let hashes_array = self.next_slot();
        let map = self.next_slot();
        let diff = self.next_slot();
        self.map_types.insert(
            canonical.to_string(),
            MapSlots {
                keys_array,
                values_array,
                hashes_array,
                map,
                diff,
            },
        );
        self.map_order.push(canonical.to_string());
        if self.map_order_indices_type_idx.is_none() {
            self.map_order_indices_type_idx = Some(self.next_slot());
        }
        if (matches!(k.as_str(), "Int" | "Float" | "Bool") || k.starts_with("List<"))
            && !self.primitive_key_box.contains_key(&k)
        {
            let idx = self.next_slot();
            self.primitive_key_box.insert(k.clone(), idx);
            self.primitive_key_box_order.push(k.clone());
        }
        self.ensure(&format!("List<{k}>"));
        self.ensure(&format!("List<{v}>"));
        self.ensure(&format!("Tuple<{k},{v}>"));
        if self.record_fields.contains_key(&k)
            || self
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|info| info.parent == k)
        {
            self.non_newtypable_keys.insert(k);
        }
    }

    /// Give every vector still without them its version and diff structs: the
    /// ones this pass added, and, in a program whose only Vector value this
    /// pass found, the ones discovery left as plain arrays.
    fn version_vectors(&mut self) {
        let order = self.vector_order.clone();
        for canonical in order {
            if self.vector_versions.contains_key(&canonical) {
                continue;
            }
            let array = self.vector_types[&canonical];
            let version = self.next_slot();
            let diff = self.next_slot();
            self.vector_versions.insert(
                canonical,
                VectorSlots {
                    array,
                    version,
                    diff,
                },
            );
        }
    }

    fn next_slot(&mut self) -> u32 {
        let idx = self.user_type_count;
        self.user_type_count += 1;
        idx
    }
}

fn strip<'a>(canonical: &'a str, prefix: &str) -> Option<&'a str> {
    canonical.strip_prefix(prefix)?.strip_suffix('>')
}

/// Every stamp in `e`, outermost first, as a whitespace-free canonical with
/// tuples spelled `Tuple<..>`. Only fully concrete types: a stamp still
/// holding an inference variable names no instantiation.
fn collect_stamps(e: &Spanned<ResolvedExpr>, out: &mut Vec<String>) {
    if let Some(ty) = e.ty()
        && is_composite(ty)
        && is_concrete(ty)
    {
        out.push(normalize_compound(&ty.display()));
    }
    let mut walk = |x: &Spanned<ResolvedExpr>| collect_stamps(x, out);
    match &e.node {
        ResolvedExpr::Call(callee, args) => {
            if let ResolvedCallee::Unresolved { callee } = callee {
                walk(callee);
            }
            args.iter().for_each(walk);
        }
        ResolvedExpr::TailCall { args, .. } | ResolvedExpr::Ctor(_, args) => {
            args.iter().for_each(walk)
        }
        ResolvedExpr::Match { subject, arms } => {
            walk(subject);
            for arm in arms {
                walk(&arm.body);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            walk(l);
            walk(r);
        }
        ResolvedExpr::Neg(inner)
        | ResolvedExpr::ErrorProp(inner)
        | ResolvedExpr::Attr(inner, _) => walk(inner),
        ResolvedExpr::List(xs)
        | ResolvedExpr::Tuple(xs)
        | ResolvedExpr::IndependentProduct(xs, _) => xs.iter().for_each(walk),
        ResolvedExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                walk(k);
                walk(v);
            }
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let ResolvedStrPart::Parsed(inner) = part {
                    walk(inner);
                }
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            fields.iter().for_each(|(_, x)| walk(x));
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            walk(base);
            updates.iter().for_each(|(_, x)| walk(x));
        }
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
    }
}

fn is_composite(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Option(_)
            | Type::List(_)
            | Type::Vector(_)
            | Type::Result(_, _)
            | Type::Map(_, _)
            | Type::Tuple(_)
    )
}

/// Concrete all the way down, and free of function types: a function value's
/// own signature is registered from its definition.
fn is_concrete(ty: &Type) -> bool {
    match ty {
        Type::Var(_) | Type::Invalid | Type::Fn(..) => false,
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit | Type::Named { .. } => true,
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => is_concrete(inner),
        Type::Result(ok, err) => is_concrete(ok) && is_concrete(err),
        Type::Map(key, value) => is_concrete(key) && is_concrete(value),
        Type::Tuple(items) => items.iter().all(is_concrete),
    }
}
