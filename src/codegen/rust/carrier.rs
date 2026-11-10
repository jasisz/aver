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

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, TopLevel, TypeDef};
use crate::codegen::CodegenContext;
use crate::ir::interval::{Bound, Interval};
use crate::ir::mir::bare_i64::CarrierIds;
use crate::ir::mir::{MirExpr, MirProgram};
use crate::ir::{FnId, FnKey, TypeId, TypeKey};

/// One type of the program shaped as a carrier.
#[derive(Clone)]
struct Candidate {
    id: TypeId,
    /// `Module.Type`, or the bare name in the entry module.
    label: String,
    /// The union of what its smart constructors admit.
    interval: Interval,
    /// Its smart constructors, the functions that build it from a checked
    /// value, each with the interval its own check admits.
    ctors: Vec<(FnId, Interval)>,
    /// Its one field.
    field: String,
}

/// The carriers of `program` the Rust backend holds as a raw `i64`, keyed by
/// type identity, their smart constructors with the interval each one's
/// check admits, and a note for each type shaped as one that is not.
///
/// A type is a candidate when it is one `Int` field, listed under
/// `exposes opaque`, with smart constructors whose checks each bound it
/// within `i64`. A type may have several: every value then lies in the
/// union of what they admit, which is the carrier's bound. It stays one when nothing else builds it from anything but a
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
) -> (CarrierIds, HashMap<FnId, CarrierCtor>, Vec<String>) {
    if std::env::var_os("AVER_NO_BARE_I64").is_some() {
        return (CarrierIds::new(), HashMap::new(), Vec::new());
    }
    let mut notes = Vec::new();
    let mut candidates = candidates(ctx, &mut notes);
    let mut unchecked: Vec<(TypeId, String)> = Vec::new();
    for (index, fn_id, line) in unchecked_builds(program, &candidates) {
        let candidate = &candidates[index];
        if !unchecked.iter().any(|(id, _)| *id == candidate.id) {
            let at = ctx.symbol_table.fn_entry(fn_id).key.canonical();
            unchecked.push((candidate.id, built_unchecked(candidate, &at, Some(line))));
        }
    }
    candidates.retain(|c| !unchecked.iter().any(|(id, _)| *id == c.id));
    notes.extend(unchecked.into_iter().map(|(_, note)| note));
    let carriers = candidates.iter().map(|c| (c.id, c.interval)).collect();
    let ctors = candidates
        .iter()
        .flat_map(|c| {
            c.ctors.iter().map(|(fn_id, admits)| {
                let ctor = CarrierCtor {
                    type_id: c.id,
                    field: c.field.clone(),
                    admits: *admits,
                };
                (*fn_id, ctor)
            })
        })
        .collect();
    (carriers, ctors, notes)
}

/// A smart constructor of a carrier held as a raw `i64`.
#[derive(Clone, Debug)]
pub(crate) struct CarrierCtor {
    pub type_id: TypeId,
    /// The carrier's one field.
    pub field: String,
    /// What this constructor's own check admits: within the carrier's
    /// bound, and narrower than it when the type has other constructors.
    pub admits: Interval,
}

/// `aver check`: what keeps a carrier the module declares an `Int` field,
/// with the line that does it. `items` is the module checked as the entry
/// of its program and `symbols` the program's symbol table. A carrier is a
/// type the checker's literal discharge recognizes as a refinement over
/// `Int` with a bound within `i64` (the boundary types of capabilities and
/// the keys of wait sets, which the backend also rules out, are left to
/// `aver compile`).
pub fn check_findings(
    items: &[TopLevel],
    symbols: &crate::ir::SymbolTable,
) -> Vec<crate::checker::CheckFinding> {
    use crate::analysis::literal_refinement::LiteralCarrier;
    // What the type would be held as: the machine integer its bound fits
    // ([`storage_for`]) when the bound is known.
    let finding = |line: usize,
                   fn_name: Option<&str>,
                   note: String,
                   repair: &str,
                   bound: Option<Interval>| {
        let held = match bound {
            Some(bound) => format!("a `{}`", storage_for(bound)),
            None => "a machine integer".to_string(),
        };
        crate::checker::CheckFinding {
            line,
            module: None,
            file: None,
            fn_name: fn_name.map(str::to_string),
            message: format!("{note} — {repair}, and the Rust backend holds it as {held}"),
            extra_spans: vec![],
        }
    };
    let mut findings = Vec::new();
    for item in items {
        let TopLevel::TypeDef(TypeDef::Product {
            name,
            fields,
            opaque: false,
            line,
            ..
        }) = item
        else {
            continue;
        };
        let [(field, field_type)] = fields.as_slice() else {
            continue;
        };
        if crate::types::parse_type_str(field_type) == crate::types::Type::Int
            && let Some(ctor) = smart_constructor_shaped(entry_fns(items), name, field)
        {
            findings.push(finding(
                *line,
                None,
                format!(
                    "`{name}` keeps an `Int` field: it has the smart constructor `{ctor}` but is not listed under `exposes opaque`"
                ),
                "list it under `exposes opaque`",
                None,
            ));
        }
    }
    let gates: Vec<(Candidate, &str)> = symbols
        .literal_refinements()
        .iter()
        .filter(|c| c.carrier == LiteralCarrier::Int && c.scope.is_none())
        // The table holds one entry per smart constructor; the type is
        // looked at once, through all of them.
        .scan(HashSet::new(), |seen, c| Some(seen.insert(c.type_id).then_some(c)))
        .flatten()
        .filter_map(|c| {
            let ctors = checked_ctors(entry_fns(items), &c.type_name, &c.carrier_field, |fd| {
                let fn_id = symbols.fn_id_of(&FnKey::entry(fd.name.as_str()))?;
                let admits = admitted(fd, |e| {
                    let rctx = crate::ir::hir::ResolveCtx::new(symbols);
                    let stmt = crate::ast::Stmt::Expr(e.clone());
                    match crate::ir::hir::resolve::resolve_stmt_external(&rctx, &stmt) {
                        crate::ir::hir::ResolvedStmt::Expr(s) => s,
                        crate::ir::hir::ResolvedStmt::Binding { value, .. } => value,
                    }
                })?;
                Some((fn_id, admits))
            });
            let ctors = match ctors {
                Ok(ctors) => ctors,
                Err(ctor) => {
                    let line = entry_fns(items)
                        .find(|fd| fd.name == ctor)
                        .map_or(0, |fd| fd.line);
                    let note = format!(
                        "`{}` keeps an `Int` field: the check of `{ctor}` does not bound it within 64 bits",
                        c.type_name
                    );
                    findings.push(finding(
                        line,
                        Some(ctor),
                        note,
                        "bound it with literal comparisons",
                        None,
                    ));
                    return None;
                }
            };
            let candidate = Candidate {
                id: c.type_id,
                label: c.type_name.clone(),
                interval: union(&ctors),
                ctors,
                field: c.carrier_field.clone(),
            };
            Some((candidate, c.constructor_fn.as_str()))
        })
        .collect();
    if gates.is_empty() {
        return findings;
    }
    let repair = |ctor: &str| format!("build it with `{ctor}` or from a literal within the bound");
    let bindings = crate::codegen::collect_module_bindings(items);
    for (candidate, ctor) in &gates {
        if let Some(binding) = built_in_binding(&bindings, &candidate.label, candidate.interval) {
            let note = format!(
                "`{}` keeps an `Int` field: {}",
                candidate.label,
                built_in(&binding.name, candidate.interval)
            );
            findings.push(finding(
                binding.value.line,
                None,
                note,
                &repair(ctor),
                Some(candidate.interval),
            ));
        }
    }
    let candidates: Vec<Candidate> = gates.iter().map(|(c, _)| c.clone()).collect();
    let program = crate::checker::lowered_for_check(items, symbols);
    for (index, fn_id, line) in unchecked_builds(&program, &candidates) {
        let at = symbols.fn_entry(fn_id).key.canonical();
        let (candidate, ctor) = &gates[index];
        findings.push(finding(
            line,
            Some(&at),
            built_unchecked(candidate, &at, None),
            &repair(ctor),
            Some(candidate.interval),
        ));
    }
    findings
}

/// Every build in `program` of one of `candidates` that its bound does not
/// cover: outside its smart constructor, from anything but a literal within
/// the bound. The candidate's index, the function and the line, in program
/// order.
fn unchecked_builds(program: &MirProgram, candidates: &[Candidate]) -> Vec<(usize, FnId, usize)> {
    let mut out = Vec::new();
    for (fn_id, mir_fn) in program.iter() {
        visit_builds(&mir_fn.body.node, &mut |type_id, name, value, line| {
            let found = match type_id {
                Some(id) => candidates.iter().position(|c| c.id == id),
                // A build with no identity is checked against every
                // candidate of its name.
                None => candidates
                    .iter()
                    .position(|c| last_segment(&c.label) == last_segment(name)),
            };
            let Some(index) = found else {
                return;
            };
            let candidate = &candidates[index];
            if !candidate.ctors.iter().any(|(ctor, _)| ctor == fn_id)
                && !literal_of(value).is_some_and(|k| candidate.interval.contains_point(k))
            {
                out.push((index, *fn_id, line));
            }
        });
    }
    out
}

/// Why a carrier that the module binding `binding` builds keeps its field.
fn built_in(binding: &str, interval: Interval) -> String {
    format!(
        "module binding `{binding}` builds it from a value other than a literal within {}",
        show(interval)
    )
}

/// The note on a carrier that `at` builds from an unchecked value, at
/// `line` when the note does not stand at it.
fn built_unchecked(candidate: &Candidate, at: &str, line: Option<usize>) -> String {
    let line = line
        .map(|line| format!(" at line {line}"))
        .unwrap_or_default();
    format!(
        "`{}` keeps an `Int` field: `{at}` builds it{line} from a value other than a literal within {}",
        candidate.label,
        show(candidate.interval)
    )
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
            if let Some(ctor) = smart_constructor_shaped(scope_fns(ctx, scope), name, field) {
                keep(format!(
                    "it has the smart constructor `{ctor}` but is not listed under `exposes opaque`"
                ));
            }
            continue;
        }
        let ctors = checked_ctors(scope_fns(ctx, scope), name, field, |fd| {
            let key = match scope {
                Some(prefix) => FnKey::in_module(prefix, fd.name.as_str()),
                None => FnKey::entry(fd.name.as_str()),
            };
            let fn_id = ctx.symbol_table.fn_id_of(&key)?;
            Some((fn_id, admitted(fd, |e| inputs.resolve_expr(e, scope))?))
        });
        let ctors = match ctors {
            Ok(ctors) if !ctors.is_empty() => ctors,
            Ok(_) => continue,
            Err(ctor) => {
                keep(format!(
                    "the check of `{ctor}` does not bound it within 64 bits"
                ));
                continue;
            }
        };
        let interval = union(&ctors);
        let type_key = match scope {
            Some(prefix) => TypeKey::in_module(prefix, name.as_str()),
            None => TypeKey::entry(name.as_str()),
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
        // The bindings of the entry and of every dependency.
        let entry = crate::codegen::collect_module_bindings(&ctx.items);
        let bindings = entry
            .iter()
            .chain(ctx.modules.iter().flat_map(|m| m.bindings.iter()));
        if let Some(binding) = built_in_binding(bindings, name, interval) {
            keep(built_in(&binding.name, interval));
            continue;
        }
        out.push(Candidate {
            id,
            label,
            interval,
            ctors,
            field: field.clone(),
        });
    }
    out
}

/// Is the record held as a raw machine integer?
pub(super) fn is_i64_carrier(ctx: &CodegenContext, id: Option<TypeId>) -> bool {
    id.is_some_and(|id| ctx.rust_i64_carriers.contains_key(&id))
}

/// The narrowest machine integer that holds every value of `interval`, one
/// of `u8`, `i8`, `u16`, `i16`, `u32`, `i32` and `i64`: the field a carrier
/// with that bound is stored in. A read widens it to `i64` and arithmetic
/// stays on `i64`, so the width only changes how many bytes the value takes
/// in a record, a variant, a vector, a map or a list.
pub(crate) fn storage_for(interval: Interval) -> &'static str {
    // The width a packed `List<Int>` refinement picks for its elements.
    crate::codegen::proof_lower::PackedIntElement::for_interval(interval)
        .map_or("i64", |element| element.rust_type())
}

/// The machine integer the carrier `id` is stored in ([`storage_for`]
/// its bound), or `None` when the record is not a carrier.
pub(super) fn storage_of(ctx: &CodegenContext, id: Option<TypeId>) -> Option<&'static str> {
    id.and_then(|id| ctx.rust_i64_carriers.get(&id))
        .map(|interval| storage_for(*interval))
}

/// The bytes a value stored as `storage` ([`storage_for`]) takes.
pub(super) fn storage_bytes(storage: &str) -> usize {
    match storage {
        "u8" | "i8" => 1,
        "u16" | "i16" => 2,
        "u32" | "i32" => 4,
        _ => 8,
    }
}

/// `value`, an `i64` within the carrier's bound, as the field stored as
/// `storage`. In a verify case, which may build a carrier from any value,
/// the narrowing is checked and a value it does not hold stops the test.
pub(super) fn narrowed(value: &str, storage: &str, checked: bool) -> String {
    match (storage, checked) {
        ("i64", _) => value.to_string(),
        (_, false) => format!("(({value}) as {storage})"),
        (_, true) => format!(
            "<{storage}>::try_from({value}).expect(\"Int outside the range of its refinement\")"
        ),
    }
}

/// `field`, a carrier's field stored as `storage`, read as an `i64`.
pub(super) fn widened(field: &str, storage: &str) -> String {
    match storage {
        "i64" => field.to_string(),
        _ => format!("i64::from({field})"),
    }
}

/// The machine integer the record `name` declared in `scope` is stored in,
/// when it is a carrier ([`storage_of`]).
pub(super) fn declared_storage(
    ctx: &CodegenContext,
    scope: Option<&str>,
    name: &str,
) -> Option<&'static str> {
    let key = match scope {
        Some(prefix) => TypeKey::in_module(prefix, name),
        None => TypeKey::entry(name),
    };
    storage_of(ctx, ctx.symbol_table.type_id_of(&key))
}

/// The functions of `scope` (`None`: the entry).
fn scope_fns<'a>(ctx: &'a CodegenContext, scope: Option<&str>) -> Vec<&'a crate::ast::FnDef> {
    match scope {
        Some(prefix) => ctx
            .modules
            .iter()
            .filter(|m| m.prefix == prefix)
            .flat_map(|m| m.fn_defs.iter())
            .collect(),
        None => entry_fns(&ctx.items).collect(),
    }
}

fn entry_fns(items: &[TopLevel]) -> impl Iterator<Item = &crate::ast::FnDef> {
    items.iter().filter_map(|item| match item {
        TopLevel::FnDef(fd) => Some(fd),
        _ => None,
    })
}

/// One of `fns` shaped as the smart constructor of the record `name` with
/// the one field `field` (see `refinement_info_for_in_scope`, which takes
/// only a type listed under `exposes opaque`).
fn smart_constructor_shaped<'a>(
    fns: impl IntoIterator<Item = &'a crate::ast::FnDef>,
    name: &str,
    field: &str,
) -> Option<&'a str> {
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

/// Every one of `fns` shaped as a smart constructor of the record `name`
/// with the one field `field`, each as `ctor` reads it, in source order.
/// `Err` names the first whose check `ctor` cannot read as an interval
/// within `i64`: the carrier's bound would not be known.
fn checked_ctors<'a>(
    fns: impl IntoIterator<Item = &'a crate::ast::FnDef>,
    name: &str,
    field: &str,
    mut ctor: impl FnMut(&'a crate::ast::FnDef) -> Option<(FnId, Interval)>,
) -> Result<Vec<(FnId, Interval)>, &'a str> {
    let mut out = Vec::new();
    for fd in fns {
        if smart_constructor_shaped([fd], name, field).is_none() {
            continue;
        }
        match ctor(fd) {
            Some((fn_id, admits)) if admits.fits_i64() => out.push((fn_id, admits)),
            _ => return Err(fd.name.as_str()),
        }
    }
    Ok(out)
}

/// The interval the check of the smart constructor `fd` admits, its
/// predicate resolved by `resolve`; `None` when the interval engine does not
/// read it exactly.
fn admitted(
    fd: &crate::ast::FnDef,
    resolve: impl FnOnce(
        &crate::ast::Spanned<Expr>,
    ) -> crate::ast::Spanned<crate::ir::hir::ResolvedExpr>,
) -> Option<Interval> {
    let [(param, _)] = fd.params.as_slice() else {
        return None;
    };
    let [crate::ast::Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, .. } = &body.node else {
        return None;
    };
    let invariant = crate::ir::proof_ir::Predicate {
        free_vars: vec![(
            param.clone(),
            crate::ir::proof_ir::QuantifierType::Plain("Int".to_string()),
        )],
        expr: resolve(subject),
    };
    let (interval, known) = crate::ir::interval::interval_of_invariant(&invariant);
    known.then_some(interval)
}

/// The union of what `ctors` admit: the bound every value of their type
/// lies in.
fn union(ctors: &[(FnId, Interval)]) -> Interval {
    ctors
        .iter()
        .map(|(_, admits)| *admits)
        .reduce(Interval::hull)
        .expect("a carrier has a smart constructor")
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

/// The first of `bindings` (`zero = Word(value = n)` outside any function)
/// that builds a record named `name` from anything but a literal within
/// `interval`. Bindings are checked by name, which can only rule a carrier
/// out.
fn built_in_binding<'b>(
    bindings: impl IntoIterator<Item = &'b crate::codegen::ModuleBinding>,
    name: &str,
    interval: Interval,
) -> Option<&'b crate::codegen::ModuleBinding> {
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
            return Some(binding);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_carrier_is_stored_in_the_narrowest_integer_its_bound_fits() {
        let within = |lo: i128, hi: i128| {
            storage_for(Interval {
                lo: Bound::Finite(lo),
                hi: Bound::Finite(hi),
            })
        };
        assert_eq!(within(0, 255), "u8");
        assert_eq!(within(-1, 100), "i8");
        assert_eq!(within(0, 256), "u16");
        assert_eq!(within(-129, 0), "i16");
        assert_eq!(within(0, 65_536), "u32");
        assert_eq!(within(0, u32::MAX.into()), "u32");
        assert_eq!(within(-1, 65_536), "i32");
        assert_eq!(within(0, i128::from(u32::MAX) + 1), "i64");
        assert_eq!(within(i64::MIN.into(), 0), "i64");
        let open = Interval {
            lo: Bound::Finite(0),
            hi: Bound::PosInf,
        };
        assert_eq!(storage_for(open), "i64");
    }

    /// The carriers of `src` by name, and the notes.
    fn carriers_and_notes(src: &str) -> (Vec<String>, Vec<String>) {
        let ctx = super::super::tests::ctx_from_source(src, "carrier");
        let (ids, _, notes) = i64_carriers(&ctx, ctx.mir_program.as_ref().expect("MIR"));
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

    /// [`WORD`] with a second smart constructor admitting `[lo, hi)`.
    fn word_with_run(lo: &str, hi: &str) -> String {
        format!(
            "{WORD}\nfn runOf(n: Int) -> Result<Word, String>\n    match Bool.and(n >= {lo}, n < {hi})\n        true -> Result.Ok(Word(value = n))\n        false -> Result.Err(\"run\")\n"
        )
    }

    /// Each smart constructor of `src`'s carrier by name, with what it admits.
    fn ctors_of(src: &str) -> Vec<(String, Interval)> {
        let ctx = super::super::tests::ctx_from_source(src, "carrier");
        let (_, ctors, _) = i64_carriers(&ctx, ctx.mir_program.as_ref().expect("MIR"));
        let mut out: Vec<(String, Interval)> = ctors
            .into_iter()
            .map(|(id, c)| (ctx.symbol_table.fn_entry(id).key.name.clone(), c.admits))
            .collect();
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }

    #[test]
    fn a_second_bounded_constructor_keeps_it_and_each_keeps_its_own_check() {
        let src = word_with_run("1", "65536");
        assert_eq!(
            carriers_and_notes(&src),
            (vec!["Word".to_string()], Vec::new())
        );
        assert_eq!(
            ctors_of(&src),
            vec![
                (
                    "fromInt".to_string(),
                    Interval::between(0, 4611686018427387903)
                ),
                ("runOf".to_string(), Interval::between(1, 65535)),
            ]
        );
    }

    #[test]
    fn the_bound_of_two_constructors_is_their_union() {
        let src = word_with_run("-5", "3");
        let ctx = super::super::tests::ctx_from_source(&src, "carrier");
        let (ids, _, _) = i64_carriers(&ctx, ctx.mir_program.as_ref().expect("MIR"));
        assert_eq!(
            ids.into_values().collect::<Vec<_>>(),
            vec![Interval::between(-5, 4611686018427387903)]
        );
        // A literal only the second constructor admits is within the bound.
        let src = format!("{src}\nfn low() -> Word\n    Word(value = -5)\n");
        assert_eq!(carriers(&src), vec!["Word".to_string()]);
    }

    #[test]
    fn a_second_constructor_without_a_bound_drops_it_and_says_which() {
        let src = word_with_run("0", "n + 1");
        assert!(
            note_of(&src).ends_with("the check of `runOf` does not bound it within 64 bits"),
            "{}",
            note_of(&src)
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

    /// The smart constructor called on a machine word checks the word and
    /// boxes it only for the `Err` it then builds.
    #[test]
    fn the_smart_constructor_checks_a_machine_word_as_it_is() {
        let src = format!(
            "{WORD}\nfn doubled(w: Word) -> Result<Word, String>\n    fromInt(w.value * 2)\n\nfn fromAny(n: Int) -> Result<Word, String>\n    fromInt(n)\n"
        );
        let mut ctx = super::super::tests::ctx_from_source(&src, "carrier");
        let out = super::super::transpile(&mut ctx);
        let entry = super::super::tests::generated_file(&out, "src/aver_generated/entry/mod.rs");
        assert!(
            entry.contains("let __word: i64 = (w.value * 2i64); if (0i64..=4611686018427387903i64).contains(&__word) { Ok(Word { value: __word }) } else { fromInt(aver_rt::AverInt::from_i64(__word)) }"),
            "{entry}"
        );
        // An `Int` that is not a machine word goes through the constructor.
        assert!(entry.contains("fromInt(n)"), "{entry}");
    }

    /// The `perf-int-carrier` warnings `aver check` gives on `src`, as
    /// `line: summary`.
    fn check_warnings(src: &str) -> Vec<String> {
        use crate::diagnostics::analyze::{AnalyzeOptions, analyze_source};
        analyze_source(src, &AnalyzeOptions::new("carrier.av"))
            .diagnostics
            .iter()
            .filter(|d| d.slug == "perf-int-carrier")
            .map(|d| format!("{}: {}", d.span.line, d.summary))
            .collect()
    }

    #[test]
    fn check_names_each_build_that_keeps_the_int_field() {
        let src = format!(
            "{WORD}\nfn low(w: Word) -> Word\n    Word(value = Bits.and(w.value, 255))\n\nfn zero() -> Word\n    Word(value = 0)\n\nverify zero\n    zero() => Word(value = 0 + 0)\n\nseven = Word(value = 3 + 4)\n"
        );
        assert_eq!(
            check_warnings(&src),
            [
                "22: `Word` keeps an `Int` field: module binding `seven` builds it from a value other than a literal within [0, 4611686018427387903]",
                "14: `Word` keeps an `Int` field: `low` builds it from a value other than a literal within [0, 4611686018427387903]",
            ]
        );
        assert!(check_warnings(WORD).is_empty());
    }

    #[test]
    fn check_takes_every_smart_constructor() {
        assert!(check_warnings(&word_with_run("1", "65536")).is_empty());
        assert_eq!(
            check_warnings(&word_with_run("0", "n + 1")),
            [
                "13: `Word` keeps an `Int` field: the check of `runOf` does not bound it within 64 bits"
            ]
        );
    }

    #[test]
    fn check_names_a_smart_constructor_on_a_type_not_exposed_opaque() {
        let src = WORD.replace("    exposes opaque [Word]\n", "");
        assert_eq!(
            check_warnings(&src),
            [
                "4: `Word` keeps an `Int` field: it has the smart constructor `fromInt` but is not listed under `exposes opaque`"
            ]
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
