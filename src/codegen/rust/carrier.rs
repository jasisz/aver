//! Refinement carriers held as a raw `i64`.
//!
//! A single-field record over `Int` whose smart constructor proves a bound
//! that fits `i64` (`record Word value: Int` built only by
//! `fromInt(n) = match Bool.and(n >= 0, n < 2^62) …`) is emitted as a struct
//! whose field is an `i64` instead of an `AverInt`. The struct keeps its name
//! and every trait, so it sits in variants, lists, options, tuples and maps
//! (as key or value) like any record, at the size of one machine word and
//! with nothing to drop.
//!
//! The bound is what makes the field a sound `i64`: building one narrows its
//! value once with the checked `to_i64`, which the bound makes unreachable,
//! and reading `.value` is a raw `i64` the `bare_i64` analysis gives the
//! bound to, boxed back to `AverInt` only where the value meets a general
//! `Int`. A type is eligible only where that holds of every value it can
//! have, and where nothing outside the generated code depends on its layout.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Spanned, TopLevel};
use crate::codegen::CodegenContext;
use crate::ir::mir::bare_i64::CarrierIntervals;

/// The carriers of the program the Rust backend holds as a raw `i64`, with
/// their proven bounds. Empty when the representation is switched off
/// (`AVER_NO_CARRIER_I64=1`) or when the `bare_i64` analysis is
/// (`AVER_NO_BARE_I64=1`): without the analysis no read of a carrier field
/// would be boxed back where the value meets a general `Int`.
///
/// A candidate (a recognized smart constructor whose bound fits `i64`) is
/// dropped when
/// - another type of the program has the same name: the representation is
///   decided by name, both here and in the analysis,
/// - it is built outside its smart constructor from anything but a literal
///   inside the bound — in a function or a module binding,
/// - it crosses to a provider: a capability's boundary type or the key of a
///   wait set, whose codecs keep the declared `AverInt` field.
///
/// Verify cases may build a carrier from any value: they compile to tests
/// only, built with overflow checks, where a value outside the bound that
/// still fits `i64` reads correctly and anything else stops the test.
pub(super) fn i64_carriers(ctx: &CodegenContext) -> CarrierIntervals {
    if std::env::var_os("AVER_NO_CARRIER_I64").is_some()
        || std::env::var_os("AVER_NO_BARE_I64").is_some()
    {
        return CarrierIntervals::new();
    }
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let table = crate::codegen::proof_lower::carrier_interval_table(&inputs);
    let mut names: HashSet<String> = table
        .iter()
        .filter(|(_, (iv, known))| *known && iv.fits_i64())
        .map(|(name, _)| name.clone())
        .collect();
    if names.is_empty() {
        return CarrierIntervals::new();
    }

    let mut declared: HashMap<&str, usize> = HashMap::new();
    let entry_types = ctx.items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some(td),
        _ => None,
    });
    for td in entry_types.chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter())) {
        *declared
            .entry(crate::codegen::common::type_def_name(td))
            .or_default() += 1;
    }
    names.retain(|name| declared.get(name.as_str()) == Some(&1));

    // A construction spelled with its module (`Lib.Word.Word(value = n)`)
    // is the same type as the bare one; the names are unique by now.
    let aliases: HashMap<String, String> = ctx
        .modules
        .iter()
        .flat_map(|m| {
            m.type_defs.iter().map(move |td| {
                let name = crate::codegen::common::type_def_name(td);
                (format!("{}.{name}", m.prefix), name.to_string())
            })
        })
        .filter(|(_, name)| names.contains(name))
        .collect();
    let demoted = crate::codegen::proof_lower::carrier_ungated_construction_demotions(
        &inputs, &names, &table, &aliases,
    );
    names.retain(|name| !demoted.contains(name));
    names.retain(|name| !built_ungated_in_bindings(name, ctx, &table, &aliases));

    for (canonical, _) in ctx.capabilities.boundary_types() {
        names.remove(bare_name(canonical));
    }
    // backend-link-stage: the wait key reaches this backend by its spelled
    // name only (see `is_wait_key_type_def`), and a carrier's name is the
    // only one of its kind in the program.
    if let Some(crate::ast::Type::Named { name, .. }) = super::wait_key_type(ctx) {
        names.remove(bare_name(&name));
    }

    table
        .into_iter()
        .filter(|(name, _)| names.contains(name))
        .collect()
}

/// Is the record named `name` (bare or qualified) held as a raw `i64`?
pub(super) fn is_i64_carrier(ctx: &CodegenContext, name: &str) -> bool {
    ctx.rust_i64_carriers.contains_key(bare_name(name))
}

fn bare_name(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// Does a module binding (`zero = Word(value = n)` outside any function) of
/// the entry or of a dependency build the carrier `name` from anything but
/// a literal inside its bound? The demotion scan walks function bodies only.
fn built_ungated_in_bindings(
    name: &str,
    ctx: &CodegenContext,
    table: &CarrierIntervals,
    aliases: &HashMap<String, String>,
) -> bool {
    let Some((iv, true)) = table.get(name).copied() else {
        return true;
    };
    let entry = crate::codegen::collect_module_bindings(&ctx.items);
    let values = entry
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.bindings.iter()))
        .map(|binding| &binding.value);
    let mut ungated = false;
    for value in values {
        walk(value, &mut |e| {
            let (type_name, fields) = match e {
                Expr::RecordCreate { type_name, fields } => (type_name, fields),
                Expr::RecordUpdate {
                    type_name, updates, ..
                } => (type_name, updates),
                _ => return,
            };
            let spelled = aliases
                .get(type_name.trim())
                .map(String::as_str)
                .unwrap_or(type_name.trim());
            if spelled == name
                && !crate::codegen::proof_lower::construct_arg_is_in_interval(fields, iv)
            {
                ungated = true;
            }
        });
    }
    ungated
}

fn walk(expr: &Spanned<Expr>, visit: &mut impl FnMut(&Expr)) {
    crate::codegen::proof_lower::carrier_walk_expr_with_byte_payloads(
        expr,
        &HashSet::new(),
        &mut |e, _| visit(e),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    fn carriers(src: &str) -> Vec<String> {
        let ctx = super::super::tests::ctx_from_source(src, "carrier");
        let mut names: Vec<String> = i64_carriers(&ctx).into_keys().collect();
        names.sort();
        names
    }

    const WORD: &str = "module M\n    intent = \"carrier\"\n    exposes opaque [Word]\n\nrecord Word\n    value: Int\n\nfn fromInt(n: Int) -> Result<Word, String>\n    match Bool.and(n >= 0, n < 4611686018427387904)\n        true -> Result.Ok(Word(value = n))\n        false -> Result.Err(\"out\")\n";

    #[test]
    fn a_gated_bounded_carrier_is_held_as_an_i64() {
        assert_eq!(carriers(WORD), vec!["Word".to_string()]);
    }

    #[test]
    fn a_literal_inside_the_bound_keeps_it() {
        let src = format!("{WORD}\nfn zero() -> Word\n    Word(value = 0)\n");
        assert_eq!(carriers(&src), vec!["Word".to_string()]);
    }

    #[test]
    fn a_construction_outside_the_smart_constructor_drops_it() {
        let src = format!("{WORD}\nfn raw(n: Int) -> Word\n    Word(value = n)\n");
        assert!(carriers(&src).is_empty());
    }

    #[test]
    fn a_literal_outside_the_bound_drops_it() {
        let src = format!("{WORD}\nfn big() -> Word\n    Word(value = -1)\n");
        assert!(carriers(&src).is_empty());
    }

    #[test]
    fn a_module_binding_built_from_a_value_drops_it() {
        let src = format!("{WORD}\nbase = 5\nseven = Word(value = base + 2)\n");
        assert!(carriers(&src).is_empty());
    }

    #[test]
    fn a_bound_wider_than_i64_keeps_the_int_field() {
        let src = WORD.replace("n < 4611686018427387904", "n >= 0");
        assert!(carriers(&src).is_empty());
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
