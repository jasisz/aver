use super::emit_ctx::{EmitCtx, should_borrow_param};
use super::expr::{aver_name_to_rust, classify_thin_fn_def_for_rust};
use super::types::type_annotation_to_rust_scoped;
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::ir::thin_kind_is_parent_thin_candidate;
use crate::types::{Type, parse_type_str};
/// Top-level Aver items → Rust items (structs, enums, functions, tests).
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

// rust-on-MIR W6/Stage-3: the HIR `ResolvedExpr` walker is gone. Every
// fn / main / verify-case body is rendered by the MIR walker
// (`from_mir`); this module keeps the structural emitters (type defs,
// fn signatures, mutual-TCO discovery, verify scaffolding) plus the
// borrow/rc helpers the MIR TCO synthesis reuses. The resolved-form
// lookup helpers still live as methods on [`CodegenContext`]
// (`ctx.resolve_fn_def` / `resolve_expr`) for the on-demand verify-case
// + main lift.

fn visibility_prefix(public: bool) -> &'static str {
    if public { "pub " } else { "" }
}

fn indent_block(block: &str, levels: usize) -> String {
    let indent = "    ".repeat(levels);
    block
        .lines()
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else {
                format!("{indent}{line}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn guest_args_param(fd: &FnDef) -> Option<String> {
    fd.params.iter().find_map(|(name, type_ann)| {
        (name == "guestArgs" && parse_type_str(type_ann) == Type::List(Box::new(Type::Str)))
            .then(|| aver_name_to_rust(name))
    })
}

fn self_host_runtime_state(fd: &FnDef) -> Option<(String, String)> {
    let prog = fd
        .params
        .iter()
        .find_map(|(name, _)| (name == "prog").then(|| aver_name_to_rust(name)));
    let module_fns = fd
        .params
        .iter()
        .find_map(|(name, _)| (name == "moduleFns").then(|| aver_name_to_rust(name)));
    match (prog, module_fns) {
        (Some(prog), Some(module_fns)) => Some((prog, module_fns)),
        _ => None,
    }
}

/// Emit a Rust struct or enum from an Aver TypeDef.
#[allow(dead_code)]
pub fn emit_type_def(td: &TypeDef, ctx: &CodegenContext) -> String {
    emit_type_def_with_visibility(td, false, ctx)
}

pub fn emit_public_type_def(td: &TypeDef, ctx: &CodegenContext) -> String {
    emit_type_def_with_visibility(td, true, ctx)
}

fn emit_type_def_with_visibility(td: &TypeDef, public: bool, ctx: &CodegenContext) -> String {
    let key = crate::codegen::common::type_key_for_decl(ctx, td);
    let scope = key.scope_str();
    match td {
        TypeDef::Sum { name, variants, .. } => emit_sum_type(name, variants, public, ctx, scope),
        TypeDef::Product { name, fields, .. } => emit_product_type(
            name,
            fields,
            public,
            ctx,
            scope,
            super::uses_packed_u8(ctx, name),
        ),
    }
}

use crate::codegen::common::type_def_name;

/// Locate a `TypeDef` by canonical or bare name.
///
/// Epic #180 Phase 6 — accepts dotted canonical keys
/// (`"A.Shape"` for module-owned types) routed to the matching
/// module's `type_defs` list, so cross-module same-bare-name
/// types resolve to the correct declaration instead of the
/// first-match-wins bare lookup. Bare keys still resolve via
/// the entry → module-walk fallback chain.
fn find_type_def<'a>(name: &str, ctx: &'a CodegenContext) -> Option<&'a TypeDef> {
    if let Some((prefix, bare)) = name.rsplit_once('.') {
        for module in &ctx.modules {
            if module.prefix == prefix {
                return module.type_defs.iter().find(|td| type_def_name(td) == bare);
            }
        }
    }
    ctx.type_defs
        .iter()
        .find(|td| type_def_name(td) == name)
        .or_else(|| {
            ctx.modules
                .iter()
                .flat_map(|module| module.type_defs.iter())
                .find(|td| type_def_name(td) == name)
        })
}

fn rust_hash_eq_safe_type(
    ty: &crate::types::Type,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> bool {
    use crate::types::Type;

    match ty {
        Type::Int | Type::Bool | Type::Unit | Type::Str => true,
        Type::Float => false,
        Type::Result(ok, err) => {
            rust_hash_eq_safe_type(ok, ctx, visiting) && rust_hash_eq_safe_type(err, ctx, visiting)
        }
        Type::Option(inner) => rust_hash_eq_safe_type(inner, ctx, visiting),
        Type::List(_) | Type::Vector(_) => false,
        Type::Tuple(items) => items
            .iter()
            .all(|item| rust_hash_eq_safe_type(item, ctx, visiting)),
        Type::Map(_, _) | Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => false,
        Type::Named { .. } => {
            let Some(key) = crate::codegen::common::backend_named_type_key(ctx, ty) else {
                return false;
            };
            rust_hash_eq_safe_named(&key, ctx, visiting)
        }
    }
}

fn rust_hash_eq_safe_named(
    name: &str,
    ctx: &CodegenContext,
    visiting: &mut HashSet<String>,
) -> bool {
    if !visiting.insert(name.to_string()) {
        return true;
    }

    let safe = find_type_def(name, ctx).is_some_and(|td| match td {
        TypeDef::Sum { variants, .. } => variants.iter().all(|variant| {
            variant.fields.iter().all(|field_ty| {
                let parsed = crate::types::parse_type_str(field_ty);
                rust_hash_eq_safe_type(&parsed, ctx, visiting)
            })
        }),
        TypeDef::Product { fields, .. } => fields.iter().all(|(_, field_ty)| {
            let parsed = crate::types::parse_type_str(field_ty);
            rust_hash_eq_safe_type(&parsed, ctx, visiting)
        }),
    });

    visiting.remove(name);
    safe
}

fn type_can_derive_hash_eq(td: &TypeDef, ctx: &CodegenContext) -> bool {
    let mut visiting = HashSet::new();
    let key = crate::codegen::common::backend_type_def_key(ctx, td);
    rust_hash_eq_safe_named(&key, ctx, &mut visiting)
}

/// Whether the generated type can carry the canonical key order.
///
/// A map keyed on this type sorts by it, so every part has to order the same
/// way on every backend. `Float` has no order the proof model can state, and
/// `Map` and `Vector` have none in the runtime's key comparator either — the
/// checker refuses a key that reaches any of them, and this is the other half
/// of that agreement.
fn type_parts_are_orderable<'a>(parts: impl Iterator<Item = &'a String>) -> bool {
    parts.into_iter().all(|ty| {
        !ty.contains("Float")
            && !ty.contains("Map<")
            && !ty.contains("Vector<")
            && !ty.contains("Fn(")
    })
}

fn emit_sum_type(
    name: &str,
    variants: &[TypeVariant],
    public: bool,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    let mut out = String::new();
    let visibility = visibility_prefix(public);
    let derives = if type_can_derive_hash_eq(
        &TypeDef::Sum {
            name: name.to_string(),
            variants: variants.to_vec(),
            line: 0,
        },
        ctx,
    ) {
        "#[derive(Clone, Debug, PartialEq, Eq, Hash)]"
    } else {
        "#[derive(Clone, Debug, PartialEq)]"
    };
    writeln!(out, "{}", derives).unwrap();
    writeln!(out, "{}enum {} {{", visibility, name).unwrap();
    for v in variants {
        if v.fields.is_empty() {
            writeln!(out, "    {},", v.name).unwrap();
        } else {
            let field_types: Vec<String> = v
                .fields
                .iter()
                .map(|f| {
                    let rust_ty = type_annotation_to_rust_scoped(f, ctx, scope);
                    if f == name {
                        // Recursive field: Rc<T> instead of Box<T> — clone is O(1) refcount bump
                        format!("std::sync::Arc<{}>", rust_ty)
                    } else {
                        rust_ty
                    }
                })
                .collect();
            writeln!(out, "    {}({}),", v.name, field_types.join(", ")).unwrap();
        }
    }
    writeln!(out, "}}").unwrap();

    // A variant can key a map, and a map iterates sorted by key. The order is
    // by CONSTRUCTOR NAME and then by payload, not by the order the
    // constructors were declared in — for the same reason a record orders by
    // field name: declaration order is not observable anywhere else, so
    // ordering by it would make reordering two constructors change how every
    // map on this key iterates.
    if type_can_derive_hash_eq(
        &TypeDef::Sum {
            name: name.to_string(),
            variants: variants.to_vec(),
            line: 0,
        },
        ctx,
    ) && type_parts_are_orderable(variants.iter().flat_map(|v| v.fields.iter()))
    {
        let mut by_name: Vec<&TypeVariant> = variants.iter().collect();
        by_name.sort_by(|a, b| a.name.cmp(&b.name));
        writeln!(out).unwrap();
        writeln!(out, "impl {} {{", name).unwrap();
        writeln!(out, "    fn aver_key_rank(&self) -> usize {{").unwrap();
        writeln!(out, "        match self {{").unwrap();
        for (rank, v) in by_name.iter().enumerate() {
            if v.fields.is_empty() {
                writeln!(out, "            {}::{} => {},", name, v.name, rank).unwrap();
            } else {
                writeln!(out, "            {}::{}(..) => {},", name, v.name, rank).unwrap();
            }
        }
        writeln!(out, "        }}").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
        writeln!(out, "impl PartialOrd for {} {{", name).unwrap();
        writeln!(
            out,
            "    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {{"
        )
        .unwrap();
        writeln!(out, "        Some(self.cmp(other))").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
        writeln!(out, "impl Ord for {} {{", name).unwrap();
        writeln!(
            out,
            "    fn cmp(&self, other: &Self) -> std::cmp::Ordering {{"
        )
        .unwrap();
        writeln!(
            out,
            "        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());"
        )
        .unwrap();
        writeln!(out, "        if rank != std::cmp::Ordering::Equal {{").unwrap();
        writeln!(out, "            return rank;").unwrap();
        writeln!(out, "        }}").unwrap();
        writeln!(out, "        match (self, other) {{").unwrap();
        for v in &by_name {
            if v.fields.is_empty() {
                continue;
            }
            let binds_a: Vec<String> = (0..v.fields.len()).map(|i| format!("a{i}")).collect();
            let binds_b: Vec<String> = (0..v.fields.len()).map(|i| format!("b{i}")).collect();
            writeln!(
                out,
                "            ({0}::{1}({2}), {0}::{1}({3})) => std::cmp::Ordering::Equal",
                name,
                v.name,
                binds_a.join(", "),
                binds_b.join(", ")
            )
            .unwrap();
            let chain: Vec<String> = (0..v.fields.len())
                .map(|i| format!("                .then_with(|| a{i}.cmp(b{i}))"))
                .collect();
            writeln!(out, "{},", chain.join("\n")).unwrap();
        }
        writeln!(out, "            _ => std::cmp::Ordering::Equal,").unwrap();
        writeln!(out, "        }}").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
    }

    // Generate AverDisplay impl
    writeln!(out).unwrap();
    writeln!(out, "impl aver_rt::AverDisplay for {} {{", name).unwrap();
    writeln!(out, "    fn aver_display(&self) -> String {{").unwrap();
    writeln!(out, "        match self {{").unwrap();
    for v in variants {
        if v.fields.is_empty() {
            writeln!(
                out,
                "            {}::{} => \"{}\".to_string(),",
                name, v.name, v.name
            )
            .unwrap();
        } else {
            let bindings: Vec<String> = (0..v.fields.len()).map(|i| format!("f{}", i)).collect();
            let display_parts: Vec<String> = bindings
                .iter()
                .map(|b| format!("{}.aver_display_inner()", b))
                .collect();
            if v.fields.len() == 1 {
                // Single field: direct format without vec![].join() allocation
                writeln!(
                    out,
                    "            {}::{}({}) => format!(\"{}({{}})\", {}),",
                    name, v.name, bindings[0], v.name, display_parts[0]
                )
                .unwrap();
            } else {
                writeln!(
                    out,
                    "            {}::{}({}) => format!(\"{}({{}})\", vec![{}].join(\", \")),",
                    name,
                    v.name,
                    bindings.join(", "),
                    v.name,
                    display_parts.join(", ")
                )
                .unwrap();
            }
        }
    }
    writeln!(out, "        }}").unwrap();
    writeln!(out, "    }}").unwrap();
    writeln!(
        out,
        "    fn aver_display_inner(&self) -> String {{ self.aver_display() }}"
    )
    .unwrap();
    writeln!(out, "}}").unwrap();

    out.trim_end().to_string()
}

fn emit_product_type(
    name: &str,
    fields: &[(String, String)],
    public: bool,
    ctx: &CodegenContext,
    scope: Option<&str>,
    packed_u8: bool,
) -> String {
    let mut out = String::new();
    let visibility = visibility_prefix(public);
    let derives = if type_can_derive_hash_eq(
        &TypeDef::Product {
            name: name.to_string(),
            fields: fields.to_vec(),
            line: 0,
        },
        ctx,
    ) {
        "#[derive(Clone, Debug, PartialEq, Eq, Hash)]"
    } else {
        "#[derive(Clone, Debug, PartialEq)]"
    };
    writeln!(out, "{}", derives).unwrap();
    writeln!(out, "{}struct {} {{", visibility, name).unwrap();
    for (field_name, field_type) in fields {
        let rust_type = if packed_u8 {
            debug_assert_eq!(fields.len(), 1);
            debug_assert_eq!(parse_type_str(field_type), Type::List(Box::new(Type::Int)));
            "aver_rt::AverPackedU8".to_string()
        } else {
            type_annotation_to_rust_scoped(field_type, ctx, scope)
        };
        writeln!(
            out,
            "    {}{}: {},",
            visibility,
            aver_name_to_rust(field_name),
            rust_type
        )
        .unwrap();
    }
    writeln!(out, "}}").unwrap();

    // A record can key a map, and a map iterates sorted by key, so the
    // generated type needs the same order the VM and the proof model state.
    // Fields are compared in alphabetical order of their NAMES rather than in
    // the order they were declared: declaration order is not observable
    // anywhere else in Aver — a record is built and read by name, and there is
    // no positional pattern — so ordering by it would make reordering two
    // fields change how every map on this key iterates.
    if type_can_derive_hash_eq(
        &TypeDef::Product {
            name: name.to_string(),
            fields: fields.to_vec(),
            line: 0,
        },
        ctx,
    ) && type_parts_are_orderable(fields.iter().map(|(_, ty)| ty))
    {
        let mut by_name: Vec<&(String, String)> = fields.iter().collect();
        by_name.sort_by(|a, b| a.0.cmp(&b.0));
        writeln!(out).unwrap();
        writeln!(out, "impl PartialOrd for {} {{", name).unwrap();
        writeln!(
            out,
            "    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {{"
        )
        .unwrap();
        writeln!(out, "        Some(self.cmp(other))").unwrap();
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();
        writeln!(out, "impl Ord for {} {{", name).unwrap();
        writeln!(
            out,
            "    fn cmp(&self, other: &Self) -> std::cmp::Ordering {{"
        )
        .unwrap();
        writeln!(out, "        std::cmp::Ordering::Equal").unwrap();
        for (field_name, _) in by_name {
            writeln!(
                out,
                "            .then_with(|| self.{0}.cmp(&other.{0}))",
                aver_name_to_rust(field_name)
            )
            .unwrap();
        }
        writeln!(out, "    }}").unwrap();
        writeln!(out, "}}").unwrap();
    }

    // Generate AverDisplay impl
    writeln!(out).unwrap();
    writeln!(out, "impl aver_rt::AverDisplay for {} {{", name).unwrap();
    writeln!(out, "    fn aver_display(&self) -> String {{").unwrap();
    let parts: Vec<String> = fields
        .iter()
        .map(|(field_name, _)| {
            format!(
                "format!(\"{}: {{}}\", self.{}.aver_display_inner())",
                field_name,
                aver_name_to_rust(field_name)
            )
        })
        .collect();
    if fields.len() == 1 {
        // Single field: direct format without vec![].join() allocation
        writeln!(out, "        format!(\"{}({{}})\", {})", name, parts[0]).unwrap();
    } else {
        writeln!(
            out,
            "        format!(\"{}({{}})\", vec![{}].join(\", \"))",
            name,
            parts.join(", ")
        )
        .unwrap();
    }
    writeln!(out, "    }}").unwrap();
    writeln!(
        out,
        "    fn aver_display_inner(&self) -> String {{ self.aver_display() }}"
    )
    .unwrap();
    writeln!(out, "}}").unwrap();

    out.trim_end().to_string()
}

/// Collect local_types directly from a `ResolvedFnDef`'s already-
/// parsed `params: Vec<(String, Type)>`. **Epic #180 Phase 3**: this
/// is the typed-HIR path — no `ctx.fn_sigs.get(...)` side-channel,
/// no `parse_type_str(string)` recompute. The resolved view's
/// params are the canonical source of truth post-typecheck.
fn collect_fn_local_types_from_resolved(
    resolved: &crate::ir::hir::ResolvedFnDef,
) -> HashMap<String, Type> {
    resolved
        .params
        .iter()
        .map(|(name, ty)| (name.clone(), ty.clone()))
        .collect()
}

/// Build an EmitCtx for a function from its `ResolvedFnDef` param types.
/// Uses borrow-by-default: non-Copy, non-Str params are tracked as borrowed.
fn build_fn_ectx_from_resolved(
    resolved: &crate::ir::hir::ResolvedFnDef,
    scope: Option<&str>,
) -> EmitCtx {
    EmitCtx::for_fn(collect_fn_local_types_from_resolved(resolved)).with_scope(scope)
}

/// Build an EmitCtx for a TCO function WITHOUT borrow-by-default
/// (params need to be owned/mutable). Reads param types from the
/// `ResolvedFnDef`.
fn build_fn_ectx_no_borrow_from_resolved(
    resolved: &crate::ir::hir::ResolvedFnDef,
    scope: Option<&str>,
) -> EmitCtx {
    EmitCtx::for_fn_no_borrow(collect_fn_local_types_from_resolved(resolved)).with_scope(scope)
}

/// Emit a Rust function from an Aver FnDef.
///
/// `scope` is the owning module prefix when `fd` came from a
/// dependency module (`module.fn_defs`), `None` when `fd` is part of
/// the entry's `ctx.fn_defs`. Threaded into `ctx.resolve_fn_def` so
/// the resolved lookup keys by `FnKey` instead of bare name — two
/// modules that share a fn name (`Util.format` vs `Other.format`)
/// pick up their own `FnId` without collision.
#[allow(dead_code)]
pub fn emit_fn_def(
    fd: &FnDef,
    resolved_fd: &crate::ir::hir::ResolvedFnDef,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    emit_fn_def_with_visibility(fd, resolved_fd, ctx, scope, false)
}

pub fn emit_public_fn_def(
    fd: &FnDef,
    resolved_fd: &crate::ir::hir::ResolvedFnDef,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    emit_fn_def_with_visibility(fd, resolved_fd, ctx, scope, true)
}

/// Emit a Rust fn def from a paired (`&FnDef`, `&ResolvedFnDef`)
/// input.
///
/// The AST `FnDef` carries source-shape metadata the emitter still
/// reads (param annotations, effect list, doc comment); the
/// `ResolvedFnDef` carries the resolved-HIR body the expr/stmt
/// emitters walk. Epic #170 Phase 4: callers pre-resolve via
/// `ctx.resolved_program.fn_by_id(fn_id)` and pass both halves so
/// no on-demand resolve happens here. `ctx.resolve_fn_def` survives
/// for the test-only synthetic-FnDef path; production codegen goes
/// through this pair API exclusively.
fn emit_fn_def_with_visibility(
    fd: &FnDef,
    resolved_fd: &crate::ir::hir::ResolvedFnDef,
    ctx: &CodegenContext,
    scope: Option<&str>,
    public: bool,
) -> String {
    let mut lines = Vec::new();

    // Doc comment from description
    if let Some(desc) = &fd.desc {
        lines.push(format!("/// {}", desc));
    }

    // Check if function uses self-TCO (has TailCall to itself in body)
    let has_tco = body_has_self_tailcall(&fd.body, &fd.name);

    // own_param-proven owned collection params (cleared `aliased_slots`
    // bit) graduate from `&T` borrow-by-default to `mut p: T`
    // owned-by-value, so the body's clone-skip yields a refcount-1
    // in-place `Rc::make_mut`. Read off the same optimized `MirFn` the
    // body emit (`emit_mir_fn_body_routed` → `apply_own_param`) reads, so
    // signature and body agree. Only the NON-TCO signature consumes this
    // here — the self-/mutual-TCO signatures are emitted inside the MIR
    // TCO helpers (`emit_tco_params_mir` is already `mut`-owned).
    let owned_collection_params: HashSet<String> = if has_tco {
        HashSet::new()
    } else {
        ctx.mir_program
            .as_ref()
            .and_then(|p| p.fn_by_id(resolved_fd.fn_id))
            .map(|mir_fn| {
                super::from_mir::owned_collection_param_names(mir_fn, &resolved_fd.params)
            })
            .unwrap_or_default()
    };

    // Int unboxing facts for this fn (the bare-`i64` param/return summary).
    // Read off the same `BareI64Facts` the body emit applies, so the
    // non-TCO signature and body never disagree on which params/return are
    // bare. `None` for fns the analysis didn't see (fail-closed → boxed).
    // The self-TCO signature emits its own bare params inside
    // `emit_mir_tco_fn`; here we only touch the non-TCO branch.
    let bare_facts = ctx.bare_i64.for_fn(resolved_fd.fn_id);

    // Function signature
    let params = if has_tco {
        emit_fn_params_with_owned(&fd.params, has_tco, &owned_collection_params, ctx, scope)
    } else {
        emit_fn_params_with_bare(&fd.params, &owned_collection_params, bare_facts, ctx, scope)
    };
    let ret_type = if fd.return_type.is_empty() {
        "()".to_string()
    } else if !has_tco && bare_facts.is_some_and(|f| f.bare_return) {
        "i64".to_string()
    } else {
        type_annotation_to_rust_scoped(&fd.return_type, ctx, scope)
    };

    let fn_name = aver_name_to_rust(&fd.name);
    let visibility = visibility_prefix(public);

    let is_guest_entry = ctx.guest_entry.as_deref() == Some(fd.name.as_str());

    // TCO functions need owned/mutable params, no borrow-by-default.
    // Normal functions use borrow-by-default for non-Copy, non-Str params.
    // **Epic #180 Phase 3**: read param types directly from the
    // `ResolvedFnDef` (typed-HIR canonical source) instead of the
    // legacy `ctx.fn_sigs.get(name)` + `parse_type_str(string)`
    // side-channel chain.
    let ectx = if has_tco {
        build_fn_ectx_no_borrow_from_resolved(resolved_fd, scope)
    } else {
        build_fn_ectx_from_resolved(resolved_fd, scope)
    };

    let guest_args_name = if is_guest_entry {
        guest_args_param(fd)
    } else {
        None
    };
    let self_host_state = if is_guest_entry && ctx.emit_self_host_support {
        self_host_runtime_state(fd)
    } else {
        None
    };
    let optimized_thin_plan = classify_thin_fn_def_for_rust(resolved_fd, ctx, &ectx);

    if fd.effects.is_empty()
        && optimized_thin_plan
            .as_ref()
            .is_some_and(|plan| thin_kind_is_parent_thin_candidate(plan.kind))
    {
        lines.push("#[inline(always)]".to_string());
    }

    if is_guest_entry && (ctx.emit_replay_runtime || self_host_state.is_some()) {
        lines.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        // The HIR walker is gone (rust-on-MIR W6/Stage-3): render the
        // INNER body through the MIR walker. The
        // `with_guest_scope[_args][_result]` / `with_program_fn_store`
        // wrappers below stay UNCHANGED template text around the body.
        // A `None` from the walker is a hard codegen error.
        let mut wrapped_body = super::from_mir::emit_mir_guest_entry_body(resolved_fd, scope, ctx)
            .unwrap_or_else(|| {
                format!(
                    "    {}",
                    emit_codegen_error_expr(
                        ctx,
                        format!("MIR walker could not render guest-entry fn `{}`", fd.name),
                    )
                )
            });
        if let Some((prog_name, module_fns_name)) = &self_host_state {
            wrapped_body = format!(
                "crate::self_host_support::with_program_fn_store({}.fns.clone(), {}.clone(), || {{\n{}\n}})",
                prog_name,
                module_fns_name,
                indent_block(&wrapped_body, 1)
            );
        }
        if ctx.emit_replay_runtime {
            match &guest_args_name {
                Some(guest_args) => {
                    // If the guest_args param is borrowed (&T), it's already a reference
                    let is_borrowed = ectx.is_borrowed_param(
                        &fd.params
                            .iter()
                            .find(|(n, _)| aver_name_to_rust(n) == *guest_args)
                            .map(|(n, _)| n.clone())
                            .unwrap_or_default(),
                    );
                    let ref_prefix = if is_borrowed { "" } else { "&" };
                    lines.push(format!(
                        "    let __replay_input = aver_replay::ReplayValue::to_replay_json({}{});",
                        ref_prefix, guest_args
                    ));
                    if fd.return_type.starts_with("Result<") {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope_args_result({:?}, __replay_input, {}.clone(), || {{",
                            fd.name, guest_args
                        ));
                    } else {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope_args({:?}, __replay_input, {}.clone(), || {{",
                            fd.name, guest_args
                        ));
                    }
                }
                None => {
                    let input_args = fd
                        .params
                        .iter()
                        .map(|(name, _)| {
                            let is_borrowed = ectx.is_borrowed_param(name);
                            let ref_prefix = if is_borrowed { "" } else { "&" };
                            format!(
                                "aver_replay::ReplayValue::to_replay_json({}{})",
                                ref_prefix,
                                aver_name_to_rust(name)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    lines.push(format!(
                        "    let __replay_input = aver_replay::entry_input(vec![{}]);",
                        input_args
                    ));
                    if fd.return_type.starts_with("Result<") {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope_result({:?}, __replay_input, || {{",
                            fd.name
                        ));
                    } else {
                        lines.push(format!(
                            "    aver_replay::with_guest_scope({:?}, __replay_input, || {{",
                            fd.name
                        ));
                    }
                }
            }
            lines.push(indent_block(&wrapped_body, 2));
            lines.push("    })".to_string());
        } else {
            lines.push(indent_block(&wrapped_body, 1));
        }
        lines.push("}".to_string());
        return lines.join("\n");
    }

    // The HIR walker is gone (rust-on-MIR W6/Stage-3): every fn body is
    // synthesized by the MIR walker. Self-TCO fns route through
    // `emit_mir_tco_fn` (loop synthesis), every other fn through
    // `emit_mir_fn_body`. A `None` from the walker (or a missing
    // `MirFn`) is a hard codegen error — never a panic and never a
    // silent drop. The only constructs that hard-error here are the
    // verify-only Oracle/trace residual, which never built on the Rust
    // backend on either walker.
    let mir_fn = ctx
        .mir_program
        .as_ref()
        .and_then(|p| p.fn_by_id(resolved_fd.fn_id));
    if has_tco {
        let code = mir_fn
            .and_then(|mir_fn| {
                super::from_mir::emit_mir_tco_fn(
                    fd,
                    resolved_fd,
                    mir_fn,
                    &fn_name,
                    &ret_type,
                    visibility,
                    scope,
                    ctx,
                )
            })
            .unwrap_or_else(|| {
                format!(
                    "{}fn {}({}) -> {} {{\n    {}\n}}",
                    visibility,
                    fn_name,
                    params,
                    ret_type,
                    emit_codegen_error_expr(
                        ctx,
                        format!("MIR walker could not render self-TCO fn `{}`", fd.name),
                    )
                )
            });
        lines.push(code);
    } else {
        lines.push(format!(
            "{}fn {}({}) -> {} {{",
            visibility, fn_name, params, ret_type
        ));
        let body = mir_fn
            .and_then(|mir_fn| {
                super::from_mir::emit_mir_fn_body_routed(
                    mir_fn,
                    resolved_fd,
                    scope,
                    /* borrow_by_default */ true,
                    ctx,
                )
            })
            .unwrap_or_else(|| {
                format!(
                    "    {}",
                    emit_codegen_error_expr(
                        ctx,
                        format!("MIR walker could not render fn `{}`", fd.name),
                    )
                )
            });
        lines.push(body);
        lines.push("}".to_string());
    }

    lines.join("\n")
}

fn emit_fn_params(
    params: &[(String, String)],
    mutable: bool,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    emit_fn_params_with_rc(params, mutable, &HashSet::new(), ctx, scope)
}

/// `pub(super)` shim so the MIR walker's mutual-TCO wrapper emits the
/// same borrow-by-default param signature as the HIR emitter's wrappers.
pub(super) fn emit_fn_params_pub(
    params: &[(String, String)],
    mutable: bool,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    emit_fn_params(params, mutable, ctx, scope)
}

fn emit_fn_params_with_rc(
    params: &[(String, String)],
    mutable: bool,
    rc_indices: &HashSet<usize>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    emit_fn_params_inner(params, mutable, rc_indices, &HashSet::new(), ctx, scope)
}

/// Emit the non-TCO param signature, graduating `own_param`-proven
/// collection params (Rust-mangled names in `owned_params`) from `&T`
/// borrow-by-default to `mut p: T` owned-by-value. The body's
/// clone-skip (`MirEmitCtx::owned_params`) is derived from the same
/// `MirFn`, so the two stay in lockstep. Used only on the non-TCO path
/// (`mutable == false`); the TCO signatures are emitted elsewhere.
fn emit_fn_params_with_owned(
    params: &[(String, String)],
    mutable: bool,
    owned_params: &HashSet<String>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    emit_fn_params_inner(params, mutable, &HashSet::new(), owned_params, ctx, scope)
}

/// Emit the non-TCO param signature with Int-unboxing applied: a param the
/// analysis proved bare emits `p: i64` (by value, `Copy`); every other
/// param falls through to [`emit_fn_params_inner`]'s owned / borrow /
/// by-value decision. Used on the non-TCO path; the self-TCO signature
/// handles its own bare params in `emit_mir_tco_fn`.
fn emit_fn_params_with_bare(
    params: &[(String, String)],
    owned_params: &HashSet<String>,
    bare_facts: Option<&crate::ir::mir::FnBareFacts>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, (name, type_ann))| {
            if bare_facts.is_some_and(|f| f.param_is_bare(i)) {
                return format!("{}: i64", aver_name_to_rust(name));
            }
            // Fall back to the single-param shape the inner emitter would
            // produce for a non-bare, non-rc param.
            emit_fn_params_inner(
                std::slice::from_ref(&(name.clone(), type_ann.clone())),
                false,
                &HashSet::new(),
                owned_params,
                ctx,
                scope,
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn emit_fn_params_inner(
    params: &[(String, String)],
    mutable: bool,
    rc_indices: &HashSet<usize>,
    owned_params: &HashSet<String>,
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, (name, type_ann))| {
            let rust_type = type_annotation_to_rust_scoped(type_ann, ctx, scope);
            let rust_name = aver_name_to_rust(name);
            if rc_indices.contains(&i) {
                // Borrowed pass-through param: &T instead of owned T
                format!("{}: &{}", rust_name, rust_type)
            } else if mutable {
                format!("mut {}: {}", rust_name, rust_type)
            } else if owned_params.contains(&rust_name) {
                // own_param proved this collection param uniquely owned:
                // take it by value (`mut p: T`) so the body's in-place
                // mutate runs on a refcount-1 backing. `mut` because the
                // owned-mutate builtins (`Vector.set` → `set_owned`,
                // `Map.set` → `insert_owned`) consume `self` by value.
                format!("mut {}: {}", rust_name, rust_type)
            } else {
                // Borrow-by-default: non-Copy, non-Str params are `&T`
                let ty = parse_type_str(type_ann);
                if should_borrow_param(&ty) {
                    format!("{}: &{}", rust_name, rust_type)
                } else {
                    format!("{}: {}", rust_name, rust_type)
                }
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Recursively check if an expression contains the `?` (ErrorProp) operator.
fn expr_uses_error_prop(expr: &Expr) -> bool {
    match expr {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(f, args) => {
            expr_uses_error_prop(&f.node) || args.iter().any(|a| expr_uses_error_prop(&a.node))
        }
        Expr::BinOp(_, l, r) => expr_uses_error_prop(&l.node) || expr_uses_error_prop(&r.node),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(&subject.node)
                || arms.iter().any(|a| expr_uses_error_prop(&a.body.node))
        }
        Expr::List(es) => es.iter().any(|e| expr_uses_error_prop(&e.node)),
        Expr::Tuple(es) | Expr::IndependentProduct(es, _) => {
            es.iter().any(|e| expr_uses_error_prop(&e.node))
        }
        Expr::Attr(e, _) => expr_uses_error_prop(&e.node),
        Expr::Constructor(_, Some(e)) => expr_uses_error_prop(&e.node),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(e) => expr_uses_error_prop(&e.node),
            _ => false,
        }),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, e)| expr_uses_error_prop(&e.node))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(&base.node)
                || updates.iter().any(|(_, e)| expr_uses_error_prop(&e.node))
        }
        _ => false,
    }
}

pub(super) fn body_has_self_tailcall(body: &FnBody, fn_name: &str) -> bool {
    body.stmts().iter().any(|s| match s {
        Stmt::Expr(e) => expr_has_self_tailcall(&e.node, fn_name),
        Stmt::Binding(_, _, e) => expr_has_self_tailcall(&e.node, fn_name),
    })
}

fn expr_has_self_tailcall(expr: &Expr, fn_name: &str) -> bool {
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData { target, .. } = boxed.as_ref();
            target == fn_name
        }
        Expr::Match { arms, .. } => arms
            .iter()
            .any(|arm| expr_has_self_tailcall(&arm.body.node, fn_name)),
        _ => false,
    }
}

/// Is this Aver type expensive to clone (i.e. not Copy and not AverStr which is Rc<str>)?
fn is_expensive_clone_type(ty: &crate::types::Type) -> bool {
    use crate::types::Type;
    match ty {
        Type::Int | Type::Float | Type::Bool | Type::Unit => false, // Copy
        Type::Str => false, // AverStr is Rc<str>, clone is O(1)
        _ => true,
    }
}

/// For a group of mutually-recursive functions (or a single self-recursive fn),
/// find param indices that are "pass-through" — never rebound in tail calls.
/// These can safely be passed as `&T` borrows to avoid deep cloning.
pub(super) fn compute_rc_params(group_fns: &[&FnDef], _ctx: &CodegenContext) -> HashSet<usize> {
    if group_fns.is_empty() {
        return HashSet::new();
    }

    // Try index-based first (works when all fns have same arity)
    let arity = group_fns[0].params.len();
    if group_fns.iter().all(|fd| fd.params.len() == arity) {
        return compute_rc_params_by_index(group_fns);
    }

    // Different arities: use name+type-based detection.
    // Find params (name, type) that appear in ALL functions and are pass-through.
    compute_rc_params_by_name(group_fns)
}

/// Index-based Rc detection: all fns have same arity, check same position.
fn compute_rc_params_by_index(group_fns: &[&FnDef]) -> HashSet<usize> {
    let arity = group_fns[0].params.len();
    let member_names: HashSet<&str> = group_fns.iter().map(|fd| fd.name.as_str()).collect();

    let mut candidates: HashSet<usize> = (0..arity)
        .filter(|&i| {
            let type_ann = &group_fns[0].params[i].1;
            let ty = crate::types::parse_type_str(type_ann);
            group_fns.iter().all(|fd| fd.params[i].1 == *type_ann) && is_expensive_clone_type(&ty)
        })
        .collect();

    if candidates.is_empty() {
        return candidates;
    }

    for fd in group_fns {
        check_tailcalls_for_rc(&fd.body, &member_names, &fd.params, &mut candidates);
        if candidates.is_empty() {
            break;
        }
    }
    candidates
}

/// Name+type-based Rc detection for groups with varying arities.
/// Finds params that share the same name AND type across all functions,
/// and are always passed through unchanged in tail calls.
/// Returns indices into the FIRST function's param list.
fn compute_rc_params_by_name(group_fns: &[&FnDef]) -> HashSet<usize> {
    // Build a map: fn_name → {param_name → (index, type)}
    let fn_param_map: HashMap<&str, HashMap<&str, (usize, &str)>> = group_fns
        .iter()
        .map(|fd| {
            let params: HashMap<&str, (usize, &str)> = fd
                .params
                .iter()
                .enumerate()
                .map(|(i, (name, ty))| (name.as_str(), (i, ty.as_str())))
                .collect();
            (fd.name.as_str(), params)
        })
        .collect();

    let member_names: HashSet<&str> = group_fns.iter().map(|fd| fd.name.as_str()).collect();

    // Find param names that exist in ALL functions with same type and expensive to clone
    let mut shared_params: Vec<(&str, &str)> = Vec::new(); // (name, type)
    if let Some(first) = group_fns.first() {
        for (name, ty) in &first.params {
            let parsed = crate::types::parse_type_str(ty);
            if !is_expensive_clone_type(&parsed) {
                continue;
            }
            // Check if ALL other fns have a param with same name and type
            let all_have_it = group_fns
                .iter()
                .all(|fd| fd.params.iter().any(|(n, t)| n == name && t == ty));
            if all_have_it {
                shared_params.push((name.as_str(), ty.as_str()));
            }
        }
    }

    if shared_params.is_empty() {
        return HashSet::new();
    }

    // For each shared param, check pass-through in ALL tail calls across ALL fns
    let valid_params: HashSet<&str> = shared_params
        .iter()
        .filter(|(param_name, _)| {
            group_fns.iter().all(|fd| {
                check_param_passthrough_by_name(&fd.body, &member_names, param_name, &fn_param_map)
            })
        })
        .map(|(name, _)| *name)
        .collect();

    // Convert back to indices into the first function's param list
    if let Some(first) = group_fns.first() {
        first
            .params
            .iter()
            .enumerate()
            .filter(|(_, (name, _))| valid_params.contains(name.as_str()))
            .map(|(i, _)| i)
            .collect()
    } else {
        HashSet::new()
    }
}

/// Check that every TailCall in `body` to a group member passes `param_name`
/// at the correct position in the TARGET function's param list.
fn check_param_passthrough_by_name(
    body: &FnBody,
    member_names: &HashSet<&str>,
    param_name: &str,
    fn_param_map: &HashMap<&str, HashMap<&str, (usize, &str)>>,
) -> bool {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => {
                if !check_expr_passthrough_by_name(&e.node, member_names, param_name, fn_param_map)
                {
                    return false;
                }
            }
        }
    }
    true
}

fn check_expr_passthrough_by_name(
    expr: &Expr,
    member_names: &HashSet<&str>,
    param_name: &str,
    fn_param_map: &HashMap<&str, HashMap<&str, (usize, &str)>>,
) -> bool {
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if !member_names.contains(target.as_str()) {
                return true; // call to non-member, irrelevant
            }
            // Find the index of param_name in the TARGET function
            if let Some(target_params) = fn_param_map.get(target.as_str())
                && let Some(&(target_idx, _)) = target_params.get(param_name)
            {
                // arg at target_idx must be Ident(param_name) from the caller
                target_idx < args.len()
                    && matches!(&args[target_idx].node, Expr::Ident(name) if name == param_name)
            } else {
                false
            }
        }
        Expr::Match { arms, .. } => arms.iter().all(|arm| {
            check_expr_passthrough_by_name(&arm.body.node, member_names, param_name, fn_param_map)
        }),
        _ => true,
    }
}

/// Walk the AST and verify that every TailCall to a group member passes
/// param[i] as Ident(param_name[i]) for all candidate indices.
/// Removes candidates that fail the check.
fn check_tailcalls_for_rc(
    body: &FnBody,
    member_names: &HashSet<&str>,
    params: &[(String, String)],
    candidates: &mut HashSet<usize>,
) {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => {
                check_expr_tailcalls_for_rc(&e.node, member_names, params, candidates);
            }
        }
    }
}

fn check_expr_tailcalls_for_rc(
    expr: &Expr,
    member_names: &HashSet<&str>,
    params: &[(String, String)],
    candidates: &mut HashSet<usize>,
) {
    if candidates.is_empty() {
        return;
    }
    match expr {
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if member_names.contains(target.as_str()) && args.len() == params.len() {
                // For each candidate index, check if arg[i] == Ident(param_name[i])
                let to_remove: Vec<usize> = candidates
                    .iter()
                    .copied()
                    .filter(
                        |&i| !matches!(&args[i].node, Expr::Ident(name) if *name == params[i].0),
                    )
                    .collect();
                for idx in to_remove {
                    candidates.remove(&idx);
                }
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                check_expr_tailcalls_for_rc(&arm.body.node, member_names, params, candidates);
            }
        }
        _ => {}
    }
}

/// Build a set of param names that should be borrowed (`&T`), given rc_indices.
pub(super) fn rc_param_names(
    params: &[(String, String)],
    rc_indices: &HashSet<usize>,
) -> HashSet<String> {
    rc_indices
        .iter()
        .filter_map(|&i| params.get(i).map(|(name, _)| name.clone()))
        .collect()
}

pub(super) fn compute_self_passthrough_params(fd: &FnDef) -> HashSet<usize> {
    let mut candidates: HashSet<usize> = (0..fd.params.len()).collect();
    let member_names = HashSet::from([fd.name.as_str()]);
    check_tailcalls_for_rc(&fd.body, &member_names, &fd.params, &mut candidates);
    candidates
}

// --- Mutual TCO (trampoline) support ---

/// Find groups of mutually tail-calling functions (SCCs of size > 1).
/// Returns indices into `fn_defs`.
pub fn find_mutual_tco_groups(fn_defs: &[&FnDef]) -> Vec<Vec<usize>> {
    let name_to_idx: HashMap<&str, usize> = fn_defs
        .iter()
        .enumerate()
        .map(|(i, fd)| (fd.name.as_str(), i))
        .collect();
    crate::call_graph::tailcall_scc_components(fn_defs)
        .into_iter()
        .map(|group| {
            let mut indices: Vec<usize> = group
                .iter()
                .filter_map(|fd| name_to_idx.get(fd.name.as_str()).copied())
                .collect();
            indices.sort();
            indices
        })
        .collect()
}

/// Convert an Aver function name to a PascalCase enum variant name.
///
/// Capitalise FIRST, then spell the result the same way every other name is
/// spelled. Order is the whole trick. Spelling first is what the original
/// bug did: `await` became `r#await`, whose capitalisation `R#await` is not
/// an identifier at all, so the trampoline enum stopped the parser.
/// Capitalising first hands `aver_name_to_rust` a name it can answer for.
///
/// It also removes the one case that used to need special handling.
/// Exactly three Aver names capitalise onto `Self` — `Self`, `self`, and
/// `ſelf`, the last because U+017F LATIN SMALL LETTER LONG S upper-cases to
/// `S` — and `Self` is the keyword with no raw spelling at all. It is also
/// one of the five names the spelling step renames, so all three arrive at
/// the variant `_avr_Self` with nothing here having to know about them.
///
/// That every OTHER capitalised name is keyword-free is not a guess. No
/// `char::to_uppercase` mapping in Unicode produces a lowercase ASCII
/// letter, so a capitalised name never begins with one and can never equal
/// one of the 51 lowercase-ASCII reserved words; names whose first character
/// has no uppercase form (a leading `_`, a digit, a caseless script) come
/// through unchanged and are not keywords either. `Self` is the only
/// non-lowercase entry in the table, which is why it is the only case left
/// — and so the `r#` branch of the spelling step is unreachable from here.
///
/// Not injective: `await` and `Await` in one recursion group both map to
/// `Await`, and so do `self` and `ſelf`. That is a duplicate variant in the
/// emitted enum, which rustc reports as E0428 with both spans — loud,
/// immediate, and pointing at the generated file, unlike the silent
/// `R#await` this replaced. It can only ever fail a build, never produce a
/// wrong binary. Aver's own naming rules make a PascalCase function name a
/// style warning already, so the pair is hard to write by accident; a
/// dedicated Aver-side error is worth adding only alongside the broader
/// compile-time name check issue #899 also asks for.
pub(super) fn fn_name_to_variant(name: &str) -> String {
    let variant = super::syntax::aver_name_to_rust(&super::syntax::capitalise_first(name));
    debug_assert!(
        !variant.contains('#')
            && (variant.is_empty() || !super::syntax::is_rust_reserved(&variant)),
        "trampoline variant `{variant}` cannot stand as an enum variant name, \
         so the enum will not parse"
    );
    variant
}

/// Emit the main function, incorporating top-level statements.
///
/// `main_fn_id` is the resolved-main `FnId` (the caller computes it via
/// `fn_id_for_decl`); it lets the body route through the MIR walker under
/// `AVER_RUST_MIR_MAIN`. `None` when there is no `main` fn (top-stmts-only
/// entry) or the decl has no resolved twin.
#[allow(dead_code)]
pub fn emit_main(
    main_fn: Option<&FnDef>,
    top_stmts: &[&Stmt],
    ctx: &CodegenContext,
    main_fn_id: Option<crate::ir::FnId>,
) -> String {
    emit_main_with_visibility(main_fn, top_stmts, ctx, main_fn_id, false)
}

pub fn emit_public_main(
    main_fn: Option<&FnDef>,
    top_stmts: &[&Stmt],
    ctx: &CodegenContext,
    main_fn_id: Option<crate::ir::FnId>,
) -> String {
    emit_main_with_visibility(main_fn, top_stmts, ctx, main_fn_id, true)
}

fn emit_main_with_visibility(
    main_fn: Option<&FnDef>,
    top_stmts: &[&Stmt],
    ctx: &CodegenContext,
    main_fn_id: Option<crate::ir::FnId>,
    public: bool,
) -> String {
    let mut out = String::new();
    let ectx = EmitCtx::empty();
    let visibility = visibility_prefix(public);

    // Check if main returns a Result (needed for ? operator support)
    let returns_result = main_fn.is_some_and(|fd| fd.return_type.starts_with("Result<"));

    // A `main` whose declared return type is neither Unit nor `Result<…>`
    // (e.g. the bench `fn main() -> Int … fib(15)`) lowers to a Rust `fn
    // main()` body whose tail is a non-`()` value. Rust's `main` returns
    // `()` (or a `Termination`), so the tail value must be DISCARDED or
    // rustc rejects it (`expected (), found i64`). The MIR main port (#395)
    // dropped the tail-value discard the deleted HIR walker did; restore it
    // here by suffixing the rendered body with `;`, which turns the tail
    // expression into a value-dropping statement. Unit mains already end in
    // `()` / a statement (no-op), and `Result<…>` mains keep their tail (it
    // flows into the `-> Result<…>` return), so neither needs the discard.
    let discard_main_tail = main_fn.is_some_and(|fd| {
        !fd.return_type.is_empty()
            && fd.return_type != "Unit"
            && !fd.return_type.starts_with("Result<")
    });

    if returns_result {
        let ret_type = type_annotation_to_rust_scoped(&main_fn.unwrap().return_type, ctx, None);
        writeln!(out, "{}fn main() -> {} {{", visibility, ret_type).unwrap();
    } else {
        writeln!(out, "{}fn main() {{", visibility).unwrap();
    }

    let guest_wrap_main = ctx.emit_replay_runtime && ctx.guest_entry.as_deref() == Some("main");
    if guest_wrap_main {
        if returns_result {
            writeln!(
                out,
                "    aver_replay::with_guest_scope_result(\"main\", serde_json::Value::Null, || {{"
            )
            .unwrap();
        } else {
            writeln!(
                out,
                "    aver_replay::with_guest_scope(\"main\", serde_json::Value::Null, || {{"
            )
            .unwrap();
        }
    }

    let indent = if guest_wrap_main { "        " } else { "    " };

    // Top-level statements first. The HIR walker is gone (rust-on-MIR
    // W6/Stage-3): render every statement VALUE through the MIR walker
    // all-or-nothing (the VM #338 isolation: one cloned program,
    // pre-check every value renders before emitting any), then re-apply
    // the `let {name} = …;` / bare-expr `…;` templating per statement.
    // A `None` is a hard codegen error (the MIR walker could not render
    // a top-level statement) — never a silent drop.
    if !top_stmts.is_empty() {
        let resolved: Vec<Spanned<crate::ir::hir::ResolvedExpr>> = top_stmts
            .iter()
            .map(|stmt| {
                let value = match stmt {
                    Stmt::Binding(_, _, value) => value,
                    Stmt::Expr(value) => value,
                };
                ctx.resolve_expr(value, ectx.current_module_scope.as_deref())
            })
            .collect();
        let refs: Vec<&Spanned<crate::ir::hir::ResolvedExpr>> = resolved.iter().collect();
        let values = super::from_mir::emit_mir_top_stmt_values(&refs, ctx).unwrap_or_else(|| {
            top_stmts
                .iter()
                .map(|_| {
                    emit_codegen_error_expr(
                        ctx,
                        "MIR walker could not render a top-level statement".to_string(),
                    )
                })
                .collect()
        });
        for (stmt, value) in top_stmts.iter().zip(values.iter()) {
            // Same templating as the MIR fn-body let-chain: `Binding` → a
            // named `let`, `Expr` → a discarded statement.
            let rendered = match stmt {
                Stmt::Binding(name, _, _) => {
                    format!("let {} = {};", aver_name_to_rust(name), value)
                }
                Stmt::Expr(_) => format!("{};", value),
            };
            writeln!(out, "{}{}", indent, rendered).unwrap();
        }
    }

    // Main function body. The HIR walker is gone (rust-on-MIR
    // W6/Stage-3): main carries a `ResolvedFnDef` (and a lowered
    // `MirFn`), so render its whole body via the MIR walker
    // (`emit_mir_main_body` — `for_fn` borrow policy, same as every
    // other fn) and splice it in (re-indented one level deeper under the
    // guest-scope wrapper). A `None` from the walker is a hard codegen
    // error, never a silent drop.
    if main_fn.is_some() {
        let body = main_fn_id
            .and_then(|fn_id| super::from_mir::emit_mir_main_body(fn_id, ctx))
            .unwrap_or_else(|| {
                format!(
                    "    {}",
                    emit_codegen_error_expr(
                        ctx,
                        "MIR walker could not render the `main` fn body".to_string(),
                    )
                )
            });
        // Non-Unit, non-Result main: discard the tail value so the body
        // type-checks against Rust's `()` main return (see
        // `discard_main_tail` above). Suffixing the whole body string with
        // `;` drops the final tail expression — correct whether the body is
        // a single expression or a flat `let`-chain whose last line is the
        // tail expression.
        let body = if discard_main_tail {
            format!("{};", body)
        } else {
            body
        };
        // `emit_mir_main_body` returns a 4-space-indented body
        // (`    crate::cancel_checkpoint();\n    …`); bump it one more
        // level when wrapped in the guest scope so it lines up under the
        // closure.
        let body = if guest_wrap_main {
            indent_block(&body, 1)
        } else {
            body
        };
        writeln!(out, "{}", body).unwrap();
    }

    if guest_wrap_main {
        writeln!(out, "    }})").unwrap();
    }

    writeln!(out, "}}").unwrap();
    out.trim_end().to_string()
}

/// Render one `verify`-case expression (the `left` / `right` of an
/// `assert_eq!`). The HIR walker is gone (rust-on-MIR W6/Stage-3): the
/// expression is resolved on demand, lowered to MIR, and rendered by the
/// MIR walker. `None` when the walker can't render it — the verify-only
/// Oracle / trace shapes that never built on Rust on either walker. The
/// caller omits such a case from the generated `#[cfg(test)]` module and
/// records that it did; no `compile_error!` is written for it, so nothing
/// downstream has to tell the backend's placeholder apart from a string the
/// program itself carries.
fn emit_verify_case_expr(
    expr: &Spanned<Expr>,
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<String> {
    // Keep the typechecker's outer expression stamp. Re-wrapping only the
    // node in `Spanned::bare` erased `List<Int>` here, which made verify RHS
    // literals fall back to the generic AverList representation even though
    // the generated function returned AverIntList.
    let resolved = ctx.resolve_expr(expr, ectx.current_module_scope.as_deref());
    super::from_mir::emit_mir_verify_expr(&resolved, ctx)
}

/// Emit a hard `compile_error!` diagnostic in expression position — used
/// when the MIR walker returns `None` for a construct that the Rust
/// backend cannot render. Never a panic and never a silent drop: the
/// generated crate fails to compile with the message.
///
/// The substitution is recorded on the context as it is made, which is the
/// only place that knows it happened. `aver compile --target rust` reads
/// the record and refuses to report success; it does not go looking for the
/// macro in the files it just wrote, because a program is free to carry the
/// text `compile_error!` in a string of its own.
pub(super) fn emit_codegen_error_expr(ctx: &CodegenContext, message: String) -> String {
    let message_lit = format!("{:?}", message);
    ctx.substituted_compile_errors.borrow_mut().push(message);
    format!(
        "{{ compile_error!({}); unreachable!(\"unreachable after compile_error\") }}",
        message_lit
    )
}

/// True when a verify case is a verify-only Oracle/trace shape that the
/// Rust backend cannot emit as a runnable `#[cfg(test)]` assertion. These
/// cases are exercised by `aver verify` (the proof path), never by the
/// generated crate's `cargo test`, so they are omitted from the verify
/// module rather than emitted as `compile_error!` placeholders (which
/// would break `cargo test` of an otherwise-buildable program).
///
/// This catches the trace-record shapes (`given`-bound effect stubs are
/// caught upfront by `emit_verify_blocks` via the per-case `case_givens`
/// list). Two source-AST signals (no rendering needed):
///  - a `.result` / `.trace` projection (the trace-result record produced
///    only by the Oracle trace runner — no such field exists at runtime), or
///  - a reference to a `BranchPath` constructor (`BranchPath.Root` /
///    `.child` / `.parse`), the leading path arg of an Oracle stub —
///    these only appear in given-universal Oracle laws.
fn verify_case_is_oracle_only(left: &Expr, right: &Expr) -> bool {
    expr_is_oracle_trace_shape(left) || expr_is_oracle_trace_shape(right)
}

/// Recursively scan an expr for the Oracle/trace-only markers described
/// on [`verify_case_is_oracle_only`]. Recursion mirrors
/// [`expr_uses_error_prop`]'s structural walk.
fn expr_is_oracle_trace_shape(expr: &Expr) -> bool {
    // Direct hit at this node.
    let here = match expr {
        // `.result` / `.trace` projection of an Oracle trace record.
        Expr::Attr(_, field) if field == "result" || field == "trace" => true,
        // A reference to the `BranchPath` namespace: bare `BranchPath`
        // (e.g. `BranchPath.child` callee) or `BranchPath.Root`
        // (the nullary value ctor reads as an `Attr` on the ns ident).
        Expr::Ident(n) if n == "BranchPath" => true,
        Expr::Attr(base, _) if matches!(&base.node, Expr::Ident(n) if n == "BranchPath") => true,
        _ => false,
    };
    if here {
        return true;
    }
    // Otherwise recurse into children.
    match expr {
        Expr::FnCall(f, args) => {
            expr_is_oracle_trace_shape(&f.node)
                || args.iter().any(|a| expr_is_oracle_trace_shape(&a.node))
        }
        Expr::BinOp(_, l, r) => {
            expr_is_oracle_trace_shape(&l.node) || expr_is_oracle_trace_shape(&r.node)
        }
        Expr::Neg(e) | Expr::ErrorProp(e) | Expr::Attr(e, _) => expr_is_oracle_trace_shape(&e.node),
        Expr::Match { subject, arms, .. } => {
            expr_is_oracle_trace_shape(&subject.node)
                || arms
                    .iter()
                    .any(|a| expr_is_oracle_trace_shape(&a.body.node))
        }
        Expr::List(es) | Expr::Tuple(es) | Expr::IndependentProduct(es, _) => {
            es.iter().any(|e| expr_is_oracle_trace_shape(&e.node))
        }
        Expr::Constructor(_, Some(e)) => expr_is_oracle_trace_shape(&e.node),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
            StrPart::Parsed(e) => expr_is_oracle_trace_shape(&e.node),
            _ => false,
        }),
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_is_oracle_trace_shape(&k.node) || expr_is_oracle_trace_shape(&v.node)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, e)| expr_is_oracle_trace_shape(&e.node)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_is_oracle_trace_shape(&base.node)
                || updates
                    .iter()
                    .any(|(_, e)| expr_is_oracle_trace_shape(&e.node))
        }
        _ => false,
    }
}

/// Emit verify blocks as Rust #[cfg(test)] module.
pub fn emit_verify_blocks(verify_blocks: &[&VerifyBlock], ctx: &CodegenContext) -> String {
    let mut out = String::new();
    let ectx = EmitCtx::empty();

    writeln!(out, "#[cfg(test)]").unwrap();
    writeln!(out, "mod tests {{").unwrap();
    writeln!(out, "    use super::*;").unwrap();
    writeln!(out).unwrap();

    // Use per-function counters to handle multiple verify blocks for the same function
    let mut fn_counters: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();
    for vb in verify_blocks {
        for (idx, (left, right)) in vb.cases.iter().enumerate() {
            // Skip verify-only Oracle/law/trace cases. Three signals:
            //  - a non-empty `given` list: the case substitutes effect
            //    oracle stubs (`given write: Disk.writeText = [fakeErr]`)
            //    that only `aver verify` honours — Rust codegen runs the
            //    real effect, so the assertion is meaningless (and often
            //    false) at runtime;
            //  - a `.result`/`.trace` projection or `BranchPath` ref (the
            //    Oracle trace-record / path-arg shapes), which render as a
            //    `compile_error!` placeholder or syntactically-broken Rust.
            // Emitting any of these would break `cargo test` of an
            // otherwise-buildable program. `aver verify` still runs them.
            let has_given = vb.case_givens.get(idx).is_some_and(|g| !g.is_empty());
            if has_given || verify_case_is_oracle_only(&left.node, &right.node) {
                continue;
            }
            // Compose the test name from the RAW Aver name. The escape
            // exists for a name that has to stand ALONE as an identifier;
            // here the name goes in the middle of a longer one, and
            // `test_r#await_case_1` is not an identifier at all — the `#`
            // ends it, so `cargo test` fails with ``error: prefix `test_r`
            // is unknown``. Nothing needs escaping in the composed result
            // either: every Rust keyword is a whole word, and `test_…`
            // cannot be one, so a `test_` prefix is a complete guarantee on
            // its own.
            let fn_key = vb.fn_name.clone();
            let counter = fn_counters.entry(fn_key.clone()).or_insert(0);
            *counter += 1;
            let test_name = format!("test_{}_case_{}", fn_key, *counter);
            // Defensive backstop: a case the source-level detector didn't
            // classify as Oracle/trace-only can still defeat the MIR
            // walker. Omit it rather than poison `cargo test` — but say so.
            // The walker's own refusal is the signal; the emitted text is
            // not, because a program is free to put `compile_error!` in a
            // string and used to lose its verify block for saying it.
            let (Some(left_str), Some(right_str)) = (
                emit_verify_case_expr(left, ctx, &ectx),
                emit_verify_case_expr(right, ctx, &ectx),
            ) else {
                *counter -= 1;
                ctx.omitted_verify_cases.borrow_mut().push(format!(
                    "{} case {}: the Rust backend cannot render this shape (verify-only \
                     Oracle/trace); `aver verify` still runs it",
                    vb.fn_name,
                    idx + 1
                ));
                continue;
            };

            // Check if either side uses `?` operator
            let uses_error_prop =
                expr_uses_error_prop(&left.node) || expr_uses_error_prop(&right.node);

            writeln!(out, "    #[test]").unwrap();
            if uses_error_prop {
                writeln!(out, "    fn {}() -> Result<(), String> {{", test_name).unwrap();
                writeln!(out, "        assert_eq!({}, {});", left_str, right_str).unwrap();
                writeln!(out, "        Ok(())").unwrap();
            } else {
                writeln!(out, "    fn {}() {{", test_name).unwrap();
                writeln!(out, "        assert_eq!({}, {});", left_str, right_str).unwrap();
            }
            writeln!(out, "    }}").unwrap();
            writeln!(out).unwrap();
        }
    }

    writeln!(out, "}}").unwrap();
    out.trim_end().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, FnBody, FnDef, Literal, Spanned, TypeDef, TypeVariant};
    use crate::codegen::CodegenContext;
    use std::sync::Arc as Rc;

    fn empty_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "test".to_string(),
            modules: vec![],
            capabilities: Default::default(),
            module_prefixes: HashSet::new(),
            policy: None,
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_support: false,
            extra_fn_defs: Vec::new(),
            mutual_tco_members: HashSet::new(),
            recursive_fns: HashSet::new(),
            buffer_build_sinks: HashMap::new(),
            buffer_fusion_sites: Vec::new(),
            synthesized_buffered_fns: Vec::new(),
            packed_sequence_layouts: HashMap::new(),
            proof_ir: crate::ir::ProofIR::default(),
            symbol_table: crate::ir::SymbolTable::default(),
            resolved_fn_defs: Vec::new(),
            resolved_module_fn_defs: Vec::new(),
            current_module_scope: std::cell::RefCell::new(None),
            lean_do_block: std::cell::Cell::new(false),
            declined_claims: std::cell::RefCell::new(std::collections::BTreeMap::new()),
            substituted_compile_errors: std::cell::RefCell::new(Vec::new()),
            omitted_verify_cases: std::cell::RefCell::new(Vec::new()),
            resolved_program: crate::codegen::program_view::ResolvedProgramView::default(),
            program_shape: None,
            mir_program: None,
            bare_i64: Default::default(),
            discovered_lemmas: Vec::new(),
            sample_expected: std::collections::HashMap::new(),
            allow_mathlib: false,
            hand_proofs: Default::default(),
        }
    }

    #[test]
    fn a_substituted_compile_error_is_recorded_where_it_is_made() {
        // The emitter is the only thing that knows it refused. `compile
        // --target rust` reads THIS, not the files it just wrote: a program
        // is free to carry the text `compile_error!` in a string, and a scan
        // of the output cannot tell that apart from a refusal.
        let ctx = empty_ctx();
        let emitted = emit_codegen_error_expr(&ctx, "could not render fn `f`".to_string());
        assert!(emitted.contains("compile_error!"));
        assert_eq!(
            *ctx.substituted_compile_errors.borrow(),
            vec!["could not render fn `f`".to_string()]
        );
    }

    #[test]
    fn recursive_sum_type_can_derive_eq_hash() {
        let td = TypeDef::Sum {
            name: "Tree".to_string(),
            variants: vec![
                TypeVariant {
                    name: "Empty".to_string(),
                    fields: vec![],
                },
                TypeVariant {
                    name: "Node".to_string(),
                    fields: vec!["Tree".to_string(), "Int".to_string(), "Tree".to_string()],
                },
            ],
            line: 1,
        };
        let mut ctx = empty_ctx();
        ctx.type_defs.push(td.clone());

        let emitted = emit_public_type_def(&td, &ctx);
        assert!(emitted.contains("#[derive(Clone, Debug, PartialEq, Eq, Hash)]"));
    }

    #[test]
    fn one_way_tailcall_chain_is_not_a_mutual_group() {
        let make_tail_fn = |name: &str, target: &str| FnDef {
            name: name.to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "String".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::TailCall(Box::new(
                TailCallData::new(
                    target.to_string(),
                    vec![Spanned::bare(Expr::Ident("n".to_string()))],
                ),
            ))))),
            resolution: None,
        };

        let a = make_tail_fn("stateA", "stateB");
        let b = make_tail_fn("stateB", "stateC");
        let c = FnDef {
            name: "stateC".to_string(),
            line: 3,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "String".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::Literal(
                Literal::Str("done".to_string()),
            )))),
            resolution: None,
        };

        let fn_defs: Vec<&FnDef> = vec![&a, &b, &c];
        let groups = find_mutual_tco_groups(&fn_defs);
        assert!(
            groups.is_empty(),
            "one-way tailcall chain should not create a mutual trampoline group"
        );
    }

    #[test]
    fn self_only_tco_not_included_in_mutual_groups() {
        let self_rec = FnDef {
            name: "factorial".to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(Spanned::bare(Expr::TailCall(Box::new(
                TailCallData::new(
                    "factorial".to_string(),
                    vec![Spanned::bare(Expr::Ident("n".to_string()))],
                ),
            ))))),
            resolution: None,
        };

        let fn_defs: Vec<&FnDef> = vec![&self_rec];
        let groups = find_mutual_tco_groups(&fn_defs);
        assert!(
            groups.is_empty(),
            "self-only TCO should not create a mutual group"
        );
    }

    fn call(name: &str, args: Vec<Expr>) -> Expr {
        Expr::FnCall(
            Box::new(Spanned::bare(Expr::Ident(name.to_string()))),
            args.into_iter().map(Spanned::bare).collect(),
        )
    }

    #[test]
    fn oracle_trace_shapes_are_detected() {
        // `.result` projection (Oracle trace-result record).
        let result_proj = Expr::Attr(
            Box::new(Spanned::bare(call("rollAndReport", vec![]))),
            "result".to_string(),
        );
        assert!(verify_case_is_oracle_only(
            &result_proj,
            &Expr::Literal(Literal::Int(1))
        ));

        // `.trace.contains(...)` (Oracle trace assertion).
        let trace_attr = Expr::Attr(
            Box::new(Spanned::bare(call("report", vec![]))),
            "trace".to_string(),
        );
        let trace_contains =
            Expr::Attr(Box::new(Spanned::bare(trace_attr)), "contains".to_string());
        assert!(verify_case_is_oracle_only(
            &trace_contains,
            &Expr::Literal(Literal::Bool(true))
        ));

        // `BranchPath.Root` arg to a given-universal Oracle stub call.
        let branch_root = Expr::Attr(
            Box::new(Spanned::bare(Expr::Ident("BranchPath".to_string()))),
            "Root".to_string(),
        );
        let oracle_call = call("highDie", vec![branch_root, Expr::Literal(Literal::Int(0))]);
        assert!(verify_case_is_oracle_only(
            &call("rollOnce", vec![]),
            &oracle_call
        ));
    }

    #[test]
    fn ordinary_runtime_cases_are_kept() {
        // `terminalWidth() => fixedSizeStub().width` — no `.result` /
        // `.trace` / `BranchPath`, so it is a runnable case (kept). The
        // `.width` Attr on an ordinary record must NOT be mistaken for a
        // trace projection.
        let left = call("terminalWidth", vec![]);
        let right = Expr::Attr(
            Box::new(Spanned::bare(call("fixedSizeStub", vec![]))),
            "width".to_string(),
        );
        assert!(!verify_case_is_oracle_only(&left, &right));

        // A plain arithmetic law stays runnable.
        let plus = Expr::BinOp(
            crate::ast::BinOp::Add,
            Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
            Box::new(Spanned::bare(Expr::Literal(Literal::Int(2)))),
        );
        assert!(!verify_case_is_oracle_only(
            &plus,
            &Expr::Literal(Literal::Int(3))
        ));
    }
}
