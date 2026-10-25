//! Refinement carriers held as a raw `i64`.
//!
//! A single-field record over `Int` whose smart constructor proves a bound
//! that fits `i64` (`record Word value: Int` built by
//! `fromInt(n) = match Bool.and(n >= 0, n < 2^62) …`) is emitted as a struct
//! whose field is an `i64` instead of an `AverInt`. The struct keeps its name
//! and every trait, so it sits in variants, lists, options, tuples and maps
//! (as key or value) like any record, at the size of one machine word and
//! with nothing to drop.
//!
//! The bound is what makes the field a sound `i64`: every value of the type
//! must lie within it. The smart constructor checks it, and nothing else may
//! build the record but from a literal within it. Building one narrows the
//! checked value once; reading `.value` is that `i64` with the bound, boxed
//! back to `AverInt` only where it meets a general `Int`. A type of this
//! shape that does not get the representation says why in one note.

use std::collections::HashSet;

use crate::ast::{Expr, TopLevel, TypeDef};
use crate::codegen::CodegenContext;
use crate::ir::interval::{Bound, Interval};
use crate::ir::mir::bare_i64::CarrierIds;
use crate::ir::mir::{MirExpr, MirProgram};
use crate::ir::{FnId, FnKey, TypeId, TypeKey};

/// One type of the program shaped as a carrier.
struct Candidate {
    id: TypeId,
    /// `Module.Type`, or the bare name in the entry module.
    label: String,
    interval: Interval,
    /// Its smart constructor, the one function that builds it unchecked.
    ctor: Option<FnId>,
}

/// The carriers of `program` the Rust backend holds as a raw `i64`, keyed by
/// type identity, and a note for each type shaped as one that is not.
///
/// A type is a candidate when it is one `Int` field, listed under
/// `exposes opaque`, with a smart constructor whose check bounds it within
/// `i64`. It stays one when nothing else builds it from anything but a
/// literal within that bound, in a function or a module binding, and when
/// it does not cross to a provider: a capability's boundary type or a wait
/// set's key, whose codecs keep the declared field.
///
/// Verify cases may build a carrier from any value: they compile to tests
/// only, built with overflow checks, where a value outside the bound that
/// still fits `i64` reads correctly and anything else stops the test.
///
/// Nothing is a carrier when the `bare_i64` analysis is off
/// (`AVER_NO_BARE_I64`): no read of a carrier's field would be boxed back.
pub(super) fn i64_carriers(
    ctx: &CodegenContext,
    program: &MirProgram,
) -> (CarrierIds, Vec<String>) {
    if std::env::var_os("AVER_NO_BARE_I64").is_some() {
        return (CarrierIds::new(), Vec::new());
    }
    let mut notes = Vec::new();
    let mut candidates = candidates(ctx, &mut notes);
    let mut unchecked: Vec<(TypeId, String)> = Vec::new();
    for (fn_id, mir_fn) in program.iter() {
        visit_builds(&mir_fn.body.node, &mut |type_id, name, value, line| {
            let candidate = match type_id {
                Some(id) => candidates.iter().find(|c| c.id == id),
                // A build with no identity is checked against every
                // candidate of its name.
                None => candidates
                    .iter()
                    .find(|c| last_segment(&c.label) == last_segment(name)),
            };
            let Some(candidate) = candidate else {
                return;
            };
            if candidate.ctor == Some(*fn_id)
                || literal_of(value).is_some_and(|k| candidate.interval.contains_point(k))
                || unchecked.iter().any(|(id, _)| *id == candidate.id)
            {
                return;
            }
            let at = ctx.symbol_table.fn_entry(*fn_id).key.canonical();
            unchecked.push((
                candidate.id,
                format!(
                    "`{}` keeps an `Int` field: `{at}` builds it at line {line} from a value other than a literal within {}",
                    candidate.label,
                    show(candidate.interval)
                ),
            ));
        });
    }
    candidates.retain(|c| !unchecked.iter().any(|(id, _)| *id == c.id));
    notes.extend(unchecked.into_iter().map(|(_, note)| note));
    let carriers = candidates.iter().map(|c| (c.id, c.interval)).collect();
    (carriers, notes)
}

/// An `Int` literal, negated or not.
fn literal_of(e: &MirExpr) -> Option<i128> {
    match e {
        MirExpr::Literal(l) => match l.node {
            crate::ast::Literal::Int(k) => Some(k as i128),
            _ => None,
        },
        MirExpr::Neg(inner) => literal_of(&inner.node).map(|k| -k),
        _ => None,
    }
}

/// Every type shaped as a carrier that nothing outside its functions rules
/// out; a note for each that something does.
fn candidates(ctx: &CodegenContext, notes: &mut Vec<String>) -> Vec<Candidate> {
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let wait_key = super::wait_key_type(ctx);
    let entry_types = ctx.items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some((None, td)),
        _ => None,
    });
    let module_types = ctx.modules.iter().flat_map(|m| {
        m.type_defs
            .iter()
            .map(move |td| (Some(m.prefix.as_str()), td))
    });
    let mut out = Vec::new();
    for (scope, td) in entry_types.chain(module_types) {
        let TypeDef::Product {
            name,
            fields,
            opaque,
            ..
        } = td
        else {
            continue;
        };
        let [(field, field_type)] = fields.as_slice() else {
            continue;
        };
        if crate::types::parse_type_str(field_type) != crate::types::Type::Int {
            continue;
        }
        let label = match scope {
            Some(prefix) => format!("{prefix}.{name}"),
            None => name.clone(),
        };
        let mut keep = |why: String| notes.push(format!("`{label}` keeps an `Int` field: {why}"));
        // A refinement is a type its module lists under `exposes opaque`;
        // one with a smart constructor that is not listed gets a note.
        if !opaque {
            if let Some(ctor) = smart_constructor_shaped(ctx, scope, name, field) {
                keep(format!(
                    "it has the smart constructor `{ctor}` but is not listed under `exposes opaque`"
                ));
            }
            continue;
        }
        let Some(info) = crate::codegen::common::refinement_info_for_in_scope(name, &inputs, scope)
        else {
            continue;
        };
        let invariant = crate::ir::proof_ir::Predicate {
            free_vars: vec![(
                info.param_name.to_string(),
                crate::ir::proof_ir::QuantifierType::Plain(info.carrier_type.to_string()),
            )],
            expr: inputs.resolve_expr(info.predicate, scope),
        };
        let (interval, known) = crate::ir::interval::interval_of_invariant(&invariant);
        if !known || !interval.fits_i64() {
            keep(format!(
                "the check of `{}` does not bound it within 64 bits",
                info.constructor_fn
            ));
            continue;
        }
        let (type_key, ctor_key) = match scope {
            Some(prefix) => (
                TypeKey::in_module(prefix, name.as_str()),
                FnKey::in_module(prefix, info.constructor_fn),
            ),
            None => (
                TypeKey::entry(name.as_str()),
                FnKey::entry(info.constructor_fn),
            ),
        };
        let Some(id) = ctx.symbol_table.type_id_of(&type_key) else {
            continue;
        };
        if ctx.capabilities.boundary_type(&label).is_some() {
            keep("it is a capability's boundary type".to_string());
            continue;
        }
        // backend-link-stage: the wait key reaches this backend by its
        // spelled name only (see `is_wait_key_type_def`).
        if let Some(crate::ast::Type::Named { name: key, .. }) = &wait_key
            && (*key == label || key == name)
        {
            keep("it keys a wait set".to_string());
            continue;
        }
        if let Some(binding) = built_in_binding(ctx, name, interval) {
            keep(format!(
                "module binding `{binding}` builds it from a value other than a literal within {}",
                show(interval)
            ));
            continue;
        }
        out.push(Candidate {
            id,
            label,
            interval,
            ctor: ctx.symbol_table.fn_id_of(&ctor_key),
        });
    }
    out
}

/// Is the record held as a raw `i64`?
pub(super) fn is_i64_carrier(ctx: &CodegenContext, id: Option<TypeId>) -> bool {
    id.is_some_and(|id| ctx.rust_i64_carriers.contains_key(&id))
}

/// Is the record `name` declared in `scope` (`None`: the entry) held as a
/// raw `i64`?
pub(super) fn declared_i64_carrier(ctx: &CodegenContext, scope: Option<&str>, name: &str) -> bool {
    let key = match scope {
        Some(prefix) => TypeKey::in_module(prefix, name),
        None => TypeKey::entry(name),
    };
    is_i64_carrier(ctx, ctx.symbol_table.type_id_of(&key))
}

/// A function of `scope` shaped as the smart constructor of the record
/// `name` with the one field `field` (see `refinement_info_for_in_scope`,
/// which takes only a type listed under `exposes opaque`).
fn smart_constructor_shaped<'a>(
    ctx: &'a CodegenContext,
    scope: Option<&str>,
    name: &str,
    field: &str,
) -> Option<&'a str> {
    let entry_fns = ctx.items.iter().filter_map(|item| match item {
        TopLevel::FnDef(fd) => Some(fd),
        _ => None,
    });
    let fns: Vec<&crate::ast::FnDef> = match scope {
        Some(prefix) => ctx
            .modules
            .iter()
            .filter(|m| m.prefix == prefix)
            .flat_map(|m| m.fn_defs.iter())
            .collect(),
        None => entry_fns.collect(),
    };
    fns.into_iter().find_map(|fd| {
        let [(param, _)] = fd.params.as_slice() else {
            return None;
        };
        let returns = fd.return_type.strip_prefix("Result<")?;
        if !returns.starts_with(name) {
            return None;
        }
        let [crate::ast::Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        let Expr::Match { arms, .. } = &body.node else {
            return None;
        };
        crate::codegen::common::is_refinement_bool_ok_err_match(arms, name, field, param)
            .then_some(fd.name.as_str())
    })
}

fn last_segment(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// `[lo, hi]` as a note writes it.
fn show(iv: Interval) -> String {
    let bound = |b: Bound| match b {
        Bound::Finite(k) => k.to_string(),
        Bound::NegInf => "-∞".to_string(),
        Bound::PosInf => "+∞".to_string(),
    };
    format!("[{}, {}]", bound(iv.lo), bound(iv.hi))
}

/// Call `visit` with every record build in `e`: its type, the value of
/// each field it sets and the line.
fn visit_builds<'e>(
    e: &'e MirExpr,
    visit: &mut dyn FnMut(Option<TypeId>, &'e str, &'e MirExpr, usize),
) {
    match e {
        MirExpr::RecordCreate(r) => {
            for field in &r.node.fields {
                visit(r.node.type_id, &r.node.type_name, &field.value.node, r.line);
            }
        }
        MirExpr::RecordUpdate(u) => {
            for field in &u.node.updates {
                visit(u.node.type_id, &u.node.type_name, &field.value.node, u.line);
            }
        }
        _ => {}
    }
    crate::ir::mir::expr::walk_children(e, &mut |child| visit_builds(child, visit));
}

/// The first module binding (`zero = Word(value = n)` outside any function)
/// of the entry or of a dependency that builds a record named `name` from
/// anything but a literal within `interval`. Bindings are checked by name,
/// which can only rule a carrier out.
fn built_in_binding(ctx: &CodegenContext, name: &str, interval: Interval) -> Option<String> {
    let entry = crate::codegen::collect_module_bindings(&ctx.items);
    let bindings = entry
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.bindings.iter()));
    for binding in bindings {
        let mut built = false;
        crate::codegen::proof_lower::carrier_walk_expr_with_byte_payloads(
            &binding.value,
            &HashSet::new(),
            &mut |e, _| {
                let (type_name, fields) = match e {
                    Expr::RecordCreate { type_name, fields } => (type_name, fields),
                    Expr::RecordUpdate {
                        type_name, updates, ..
                    } => (type_name, updates),
                    _ => return,
                };
                if last_segment(type_name.trim()) == name
                    && !crate::codegen::proof_lower::construct_arg_is_in_interval(fields, interval)
                {
                    built = true;
                }
            },
        );
        if built {
            return Some(binding.name.clone());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The carriers of `src` by name, and the notes.
    fn carriers_and_notes(src: &str) -> (Vec<String>, Vec<String>) {
        let ctx = super::super::tests::ctx_from_source(src, "carrier");
        let (ids, notes) = i64_carriers(&ctx, ctx.mir_program.as_ref().expect("MIR"));
        let mut names: Vec<String> = ids
            .into_keys()
            .map(|id| ctx.symbol_table.type_entry(id).key.name.clone())
            .collect();
        names.sort();
        (names, notes)
    }

    fn carriers(src: &str) -> Vec<String> {
        carriers_and_notes(src).0
    }

    fn note_of(src: &str) -> String {
        let (names, notes) = carriers_and_notes(src);
        assert!(names.is_empty(), "{names:?}");
        assert_eq!(notes.len(), 1, "{notes:?}");
        notes[0].clone()
    }

    const WORD: &str = "module M\n    intent = \"carrier\"\n    exposes opaque [Word]\n\nrecord Word\n    value: Int\n\nfn fromInt(n: Int) -> Result<Word, String>\n    match Bool.and(n >= 0, n < 4611686018427387904)\n        true -> Result.Ok(Word(value = n))\n        false -> Result.Err(\"out\")\n";

    #[test]
    fn a_gated_bounded_carrier_is_held_as_an_i64() {
        assert_eq!(
            carriers_and_notes(WORD),
            (vec!["Word".to_string()], Vec::new())
        );
    }

    #[test]
    fn a_literal_inside_the_bound_keeps_it() {
        let src = format!("{WORD}\nfn zero() -> Word\n    Word(value = 0)\n");
        assert_eq!(carriers(&src), vec!["Word".to_string()]);
    }

    #[test]
    fn a_construction_from_a_value_drops_it_and_says_where() {
        let src =
            format!("{WORD}\nfn low(w: Word) -> Word\n    Word(value = Bits.and(w.value, 255))\n");
        let note = note_of(&src);
        assert!(
            note.starts_with("`Word` keeps an `Int` field: `low` builds it at line"),
            "{note}"
        );
        assert!(
            note.ends_with("a literal within [0, 4611686018427387903]"),
            "{note}"
        );
    }

    #[test]
    fn a_literal_outside_the_bound_drops_it() {
        let src = format!("{WORD}\nfn big() -> Word\n    Word(value = -1)\n");
        assert!(note_of(&src).contains("`big` builds it"));
    }

    #[test]
    fn a_module_binding_built_from_a_value_drops_it() {
        let src = format!("{WORD}\nbase = 5\nseven = Word(value = base + 2)\n");
        assert!(note_of(&src).contains("module binding `seven`"));
    }

    #[test]
    fn a_bound_wider_than_i64_keeps_the_int_field() {
        let src = WORD.replace("n < 4611686018427387904", "n >= 0");
        assert!(note_of(&src).contains("does not bound it within 64 bits"));
    }

    #[test]
    fn a_type_not_exposed_opaque_keeps_the_int_field() {
        let src = WORD.replace("    exposes opaque [Word]\n", "");
        assert!(note_of(&src).contains("not listed under `exposes opaque`"));
    }

    #[test]
    fn the_field_is_an_i64_built_once_and_read_raw() {
        let src = format!(
            "{WORD}\nfn low(w: Word) -> Int\n    Bits.and(w.value, 255) + Int.mod(w.value, 7)\n\nfn same(w: Word) -> Word\n    Word.update(w, value = 5)\n\nfn shown(w: Word) -> String\n    \"{{w.value}}\"\n"
        );
        let mut ctx = super::super::tests::ctx_from_source(&src, "carrier");
        let out = super::super::transpile(&mut ctx);
        let entry = super::super::tests::generated_file(&out, "src/aver_generated/entry/mod.rs");
        assert!(entry.contains("pub value: i64,"), "{entry}");
        // The bound `2^62` is past the integers `AverInt` holds in its word,
        // so the check compares with the machine integer.
        assert!(
            entry.contains("(n).cmp_i64(4611686018427387904i64).is_lt()"),
            "{entry}"
        );
        // The smart constructor narrows its checked value once.
        assert!(
            entry.contains("Word { value: n.to_i64().expect(\"Int out of i64 range\") }"),
            "{entry}"
        );
        // The word builtins run on the raw field; the sum is boxed where it
        // is returned as an Int.
        assert!(entry.contains("(w.value & 255i64)"), "{entry}");
        assert!(entry.contains("(w.value).rem_euclid(7i64)"), "{entry}");
        assert!(entry.contains("__updated.value = 5i64"), "{entry}");
        // A read that meets a general Int is boxed.
        assert!(
            entry.contains("aver_rt::AverInt::from_i64(w.value)"),
            "{entry}"
        );
    }

    #[test]
    fn without_the_representation_the_field_stays_an_averint() {
        let src = format!("{WORD}\nfn raw(n: Int) -> Word\n    Word(value = n)\n");
        let mut ctx = super::super::tests::ctx_from_source(&src, "carrier");
        let out = super::super::transpile(&mut ctx);
        let entry = super::super::tests::generated_file(&out, "src/aver_generated/entry/mod.rs");
        assert!(entry.contains("pub value: aver_rt::AverInt,"), "{entry}");
        assert!(entry.contains("Word { value: n }"), "{entry}");
    }
}
