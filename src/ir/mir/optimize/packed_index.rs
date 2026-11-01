//! Indexed byte reads through a packed carrier, decided in MIR.
//!
//! `Bytes.get` is ordinary Aver in the standard library: a guard on a
//! negative index, then a list match on `List.drop(bytes.values, index)`
//! that ignores the tail. A loop that indexes Bytes calls it once per byte
//! and matches the `Option<Int>` it returns. On a backend that stores the
//! carrier as a packed byte array (`PackedIntElement::U8`) every element is
//! proven to be in `0..=255`, so `-1` can never be an element and the pair
//! "`Option.None` or `Option.Some(octet)`" fits one `Int`.
//!
//! This pass rewrites a match on a call to such a reader,
//!
//! ```text
//! match get(b, i)                match __packed_byte_at(b.values, i)
//!     Option.None    -> A   ==>      -1 -> A
//!     Option.Some(v) -> B            v  -> B
//! ```
//!
//! so no `Option` is built and the octet is an `Int` in `-1..=255`. The
//! bare-`i64` analysis (`bare_i64`) then reads the intrinsic as a
//! position read: a loop whose every step follows a successful read keeps
//! its index as a raw `i64` and the octet as a raw `i64`, the same way it
//! treats `String.charAt` in an indexed string walk.
//!
//! The reader is recognised by the shape of its MIR body, not by its name:
//! two params, the first a nominal whose packed layout is `U8`, and exactly
//! the body `Bytes.get` lowers to. A function of any other shape is left
//! alone, so editing the standard library's `get` turns the rewrite off
//! rather than making it wrong.
//!
//! Like the other fabricating passes this is below the proof line: the VM
//! and the proof exporters never run it, and a wasm-gc `--certify` build
//! keeps the plain shape the certificate reads.

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Literal, Spanned, Type};
use crate::ir::FnId;
use crate::ir::hir::{BuiltinCtor, BuiltinIntrinsic};

use super::super::expr::{
    MirCall, MirCallee, MirCtor, MirExpr, MirMatch, MirMatchArm, MirPattern, MirProject,
    walk_children_mut,
};
use super::super::program::{LocalId, MirFn, MirProgram};

/// A reader the pass recognised: the carrier field its body projects.
#[derive(Debug, Clone)]
struct Reader {
    field: String,
}

/// Rewrite every match on a packed byte reader into a match on
/// `__packed_byte_at`. `u8_carriers` are the nominals the backend stores as
/// a packed `U8` array, by the names it keys them by. Returns the program and the
/// number of rewritten matches; zero leaves the program untouched.
pub fn index_packed_byte_reads(
    mut program: MirProgram,
    u8_carriers: &HashSet<String>,
) -> (MirProgram, usize) {
    if u8_carriers.is_empty() {
        return (program, 0);
    }
    let Some(drop_id) = program.builtins.iter().position(|name| name == "List.drop") else {
        return (program, 0);
    };
    let drop_id = crate::ir::BuiltinId(drop_id as u32);
    let readers: HashMap<FnId, Reader> = program
        .iter()
        .filter_map(|(id, f)| reader_shape(f, u8_carriers, drop_id).map(|r| (*id, r)))
        .collect();
    if readers.is_empty() {
        return (program, 0);
    }
    let mut rewritten = 0;
    for f in program.fns.values_mut() {
        if readers.contains_key(&f.fn_id) {
            continue;
        }
        rewrite(&mut f.body, &readers, &mut rewritten);
    }
    (program, rewritten)
}

/// Whether `e` is typed as a packed `U8` nominal, by the exact name the
/// backend keys its packed layouts by. No suffix fallback: a qualified name
/// the backend does not hold is some other type.
fn is_u8_carrier(e: &Spanned<MirExpr>, u8_carriers: &HashSet<String>) -> bool {
    matches!(e.ty(), Some(Type::Named { name, .. }) if u8_carriers.contains(name.as_str()))
}

/// `f` when its body is exactly the standard library's `Bytes.get` over a
/// packed `U8` carrier (the `Option<Int>` result and the `Int` index follow
/// from the body):
///
/// ```text
/// fn get(bytes: C, index: Int) -> Option<Int>
///     match index < 0
///         true -> Option.None
///         false -> match List.drop(bytes.<field>, index)
///             [] -> Option.None
///             [octet, .._] -> Option.Some(octet)
/// ```
fn reader_shape(
    f: &MirFn,
    u8_carriers: &HashSet<String>,
    drop_id: crate::ir::BuiltinId,
) -> Option<Reader> {
    let [carrier, index] = f.params.as_slice() else {
        return None;
    };
    if !f.effects.is_empty() {
        return None;
    }
    let (cond, negative, rest) = bool_split(&f.body.node)?;
    if !is_local_below_zero(cond, index.local) || !is_none(negative) {
        return None;
    }
    let MirExpr::Match(m) = rest else {
        return None;
    };
    let MirExpr::Call(call) = &m.node.subject.node else {
        return None;
    };
    if call.node.callee != MirCallee::Builtin(drop_id) {
        return None;
    }
    let [projected, count] = call.node.args.as_slice() else {
        return None;
    };
    let MirExpr::Project(project) = &projected.node else {
        return None;
    };
    if !is_local(&project.node.base.node, carrier.local)
        || !is_u8_carrier(&project.node.base, u8_carriers)
        || !is_local(&count.node, index.local)
    {
        return None;
    }
    let [first, second] = m.node.arms.as_slice() else {
        return None;
    };
    let (empty, cons) = match (&first.pattern, &second.pattern) {
        (MirPattern::Cons { .. }, _) => (second, first),
        _ => (first, second),
    };
    if !matches!(empty.pattern, MirPattern::EmptyList | MirPattern::Wildcard)
        || !is_none(&empty.body.node)
    {
        return None;
    }
    let MirPattern::Cons { head, tail, .. } = &cons.pattern else {
        return None;
    };
    if !is_ignored(*tail) || !is_some_of(&cons.body.node, *head) {
        return None;
    }
    Some(Reader {
        field: project.node.field.clone(),
    })
}

/// `(cond, then, else)` of a two-way branch on a `Bool`, in either the
/// `IfThenElse` form or a `match` on `true` / `false`.
fn bool_split(e: &MirExpr) -> Option<(&MirExpr, &MirExpr, &MirExpr)> {
    match e {
        MirExpr::IfThenElse(ite) => Some((
            &ite.node.cond.node,
            &ite.node.then_branch.node,
            &ite.node.else_branch.node,
        )),
        MirExpr::Match(m) => {
            let [a, b] = m.node.arms.as_slice() else {
                return None;
            };
            let truth = |p: &MirPattern| match p {
                MirPattern::Literal(Literal::Bool(v)) => Some(Some(*v)),
                MirPattern::Wildcard => Some(None),
                _ => None,
            };
            match (truth(&a.pattern)?, truth(&b.pattern)?) {
                (Some(true), Some(false) | None) => {
                    Some((&m.node.subject.node, &a.body.node, &b.body.node))
                }
                (Some(false), Some(true) | None) => {
                    Some((&m.node.subject.node, &b.body.node, &a.body.node))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn is_local(e: &MirExpr, slot: LocalId) -> bool {
    matches!(e, MirExpr::Local(l) if l.node.slot == slot)
}

/// `slot < 0`.
fn is_local_below_zero(e: &MirExpr, slot: LocalId) -> bool {
    let MirExpr::BinOp(b) = e else {
        return false;
    };
    b.node.op == BinOp::Lt
        && is_local(&b.node.lhs.node, slot)
        && matches!(&b.node.rhs.node, MirExpr::Literal(l) if l.node == Literal::Int(0))
}

fn is_none(e: &MirExpr) -> bool {
    matches!(e, MirExpr::Construct(c)
        if c.node.ctor == MirCtor::Builtin(BuiltinCtor::OptionNone) && c.node.args.is_empty())
}

fn is_some_of(e: &MirExpr, slot: LocalId) -> bool {
    matches!(e, MirExpr::Construct(c)
        if c.node.ctor == MirCtor::Builtin(BuiltinCtor::OptionSome)
            && matches!(c.node.args.as_slice(), [only] if is_local(&only.node, slot)))
}

/// A binder the source wrote as `_`.
fn is_ignored(slot: LocalId) -> bool {
    slot.0 == u32::from(u16::MAX)
}

fn rewrite(e: &mut Spanned<MirExpr>, readers: &HashMap<FnId, Reader>, rewritten: &mut usize) {
    walk_children_mut(&mut e.node, &mut |child| rewrite(child, readers, rewritten));
    let MirExpr::Match(m) = &mut e.node else {
        return;
    };
    if let Some(indexed) = indexed_match(&m.node, readers) {
        m.node = indexed;
        *rewritten += 1;
    }
}

/// The match on `__packed_byte_at` that answers `m`, a match on a reader
/// call with one `Option.None` arm and one `Option.Some(v)` arm (either
/// order; a wildcard after the `Some` arm stands for `None`).
fn indexed_match(m: &MirMatch, readers: &HashMap<FnId, Reader>) -> Option<MirMatch> {
    let MirExpr::Call(call) = &m.subject.node else {
        return None;
    };
    let MirCallee::Fn(id) = call.node.callee else {
        return None;
    };
    let reader = readers.get(&id)?;
    let [carrier, index] = call.node.args.as_slice() else {
        return None;
    };
    let mut none_body = None;
    let mut some = None;
    for arm in &m.arms {
        match &arm.pattern {
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                bindings,
                ..
            } if bindings.is_empty() && none_body.is_none() => none_body = Some(&arm.body),
            MirPattern::Wildcard if some.is_some() && none_body.is_none() => {
                none_body = Some(&arm.body)
            }
            MirPattern::Ctor {
                ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                bindings,
                binding_names,
            } if bindings.len() == 1 && some.is_none() => {
                some = Some((bindings[0], binding_names.first().cloned(), &arm.body));
            }
            _ => return None,
        }
    }
    let (none_body, (octet, octet_name, some_body)) = (none_body?, some?);

    let projected = Spanned::new(
        MirExpr::Project(Spanned::new(
            MirProject {
                base: Box::new(carrier.clone()),
                field: reader.field.clone(),
            },
            carrier.line,
        )),
        carrier.line,
    );
    projected.set_ty(Type::List(Box::new(Type::Int)));
    let subject = Spanned::new(
        MirExpr::Call(Spanned::new(
            MirCall {
                callee: MirCallee::Intrinsic(BuiltinIntrinsic::PackedByteAt),
                args: vec![projected, index.clone()],
            },
            m.subject.line,
        )),
        m.subject.line,
    );
    subject.set_ty(Type::Int);
    let some_pattern = if is_ignored(octet) {
        MirPattern::Wildcard
    } else {
        MirPattern::Bind(octet, octet_name.unwrap_or_default())
    };
    Some(MirMatch {
        subject: Box::new(subject),
        arms: vec![
            MirMatchArm {
                pattern: MirPattern::Literal(Literal::Int(-1)),
                body: none_body.clone(),
            },
            MirMatchArm {
                pattern: some_pattern,
                body: some_body.clone(),
            },
        ],
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::mir::{lower_program, optimize};
    use crate::source::parse_source;

    /// The standard library's `Bytes` and `get`, as a local record, so the
    /// pass sees the reader shape without loading the embedded module. The
    /// carrier set is handed to the pass by hand, as a backend would.
    const READER: &str = r#"
record Bytes
    values: List<Int>

fn get(bytes: Bytes, index: Int) -> Option<Int>
    match index < 0
        true -> Option.None
        false -> match List.drop(bytes.values, index)
            [] -> Option.None
            [octet, .._] -> Option.Some(octet)
"#;

    fn optimized(src: &str) -> MirProgram {
        let mut items = parse_source(&format!("{READER}{src}")).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        assert!(
            result
                .typecheck
                .as_ref()
                .is_none_or(|t| t.errors.is_empty()),
            "typecheck errors: {:?}",
            result.typecheck.map(|t| t.errors)
        );
        optimize(lower_program(&result.resolved_items))
    }

    fn bytes_carrier() -> HashSet<String> {
        HashSet::from(["Bytes".to_string()])
    }

    fn fn_named<'a>(program: &'a MirProgram, name: &str) -> &'a MirFn {
        program
            .iter()
            .find(|(_, f)| f.name == name)
            .map(|(_, f)| f)
            .unwrap_or_else(|| panic!("fn `{name}` not in program"))
    }

    /// The first match in `e`, depth first.
    fn first_match(e: &MirExpr) -> Option<&MirMatch> {
        if let MirExpr::Match(m) = e {
            return Some(&m.node);
        }
        let mut found = None;
        crate::ir::mir::expr::walk_children(e, &mut |child| {
            if found.is_none() {
                found = first_match(child);
            }
        });
        found
    }

    const SUM: &str = r#"
fn sumFrom(b: Bytes, i: Int, acc: Int) -> Int
    match get(b, i)
        Option.None -> acc
        Option.Some(v) -> sumFrom(b, i + 1, acc + v)

fn main() -> Int
    sumFrom(Bytes(values = [1, 2, 3]), 0, 0)
"#;

    #[test]
    fn a_match_on_the_reader_reads_the_octet_in_place() {
        let (program, rewritten) = index_packed_byte_reads(optimized(SUM), &bytes_carrier());
        assert_eq!(rewritten, 1);
        let m = first_match(&fn_named(&program, "sumFrom").body.node).expect("match");
        let MirExpr::Call(call) = &m.subject.node else {
            panic!("subject is not a call: {:?}", m.subject.node);
        };
        assert_eq!(
            call.node.callee,
            MirCallee::Intrinsic(BuiltinIntrinsic::PackedByteAt)
        );
        assert!(matches!(
            &call.node.args[0].node,
            MirExpr::Project(p) if p.node.field == "values"
        ));
        assert!(matches!(
            m.arms[0].pattern,
            MirPattern::Literal(Literal::Int(-1))
        ));
        assert!(matches!(m.arms[1].pattern, MirPattern::Bind(_, ref name) if name == "v"));
        // The reader itself keeps its body.
        assert!(first_match(&fn_named(&program, "get").body.node).is_some());
    }

    #[test]
    fn the_loop_position_and_octet_become_machine_words() {
        let (program, _) = index_packed_byte_reads(optimized(SUM), &bytes_carrier());
        let program = super::super::bare_i64_rewrite::rewrite_for_rust(
            program,
            &HashSet::new(),
            &super::super::bare_i64::CarrierIds::new(),
        );
        let f = fn_named(&program, "sumFrom");
        assert!(
            f.repr.param_is_bare(1),
            "a position stepped only past a successful read fits i64"
        );
        let m = first_match(&f.body.node).expect("match");
        let MirPattern::Bind(octet, _) = m.arms[1].pattern else {
            panic!("octet arm lost its binder");
        };
        assert!(f.repr.bare_slots.contains(&octet), "an octet fits i64");
    }

    #[test]
    fn some_first_arms_and_a_wildcard_for_none_are_read_too() {
        let src = r#"
fn countFrom(b: Bytes, i: Int, n: Int) -> Int
    match get(b, i)
        Option.Some(_) -> countFrom(b, i + 1, n + 1)
        _ -> n
"#;
        let (program, rewritten) = index_packed_byte_reads(optimized(src), &bytes_carrier());
        assert_eq!(rewritten, 1);
        let m = first_match(&fn_named(&program, "countFrom").body.node).expect("match");
        assert!(matches!(
            m.arms[0].pattern,
            MirPattern::Literal(Literal::Int(-1))
        ));
        assert!(matches!(m.arms[1].pattern, MirPattern::Wildcard));
        // The `-1` arm is the source's wildcard arm, `n`.
        assert!(matches!(&m.arms[0].body.node, MirExpr::Local(l) if l.node.name == "n"));
    }

    #[test]
    fn a_carrier_the_backend_does_not_pack_is_left_alone() {
        let (_, rewritten) = index_packed_byte_reads(optimized(SUM), &HashSet::new());
        assert_eq!(rewritten, 0);
        let other = HashSet::from(["Octets".to_string()]);
        let (_, rewritten) = index_packed_byte_reads(optimized(SUM), &other);
        assert_eq!(rewritten, 0);
    }

    #[test]
    fn a_reader_of_another_shape_is_left_alone() {
        // `index <= 0` answers `None` at zero: not `Bytes.get`.
        let src = r#"
fn getish(bytes: Bytes, index: Int) -> Option<Int>
    match index <= 0
        true -> Option.None
        false -> match List.drop(bytes.values, index)
            [] -> Option.None
            [octet, .._] -> Option.Some(octet)

fn sumFrom(b: Bytes, i: Int, acc: Int) -> Int
    match getish(b, i)
        Option.None -> acc
        Option.Some(v) -> sumFrom(b, i + 1, acc + v)
"#;
        let mut items =
            parse_source(&format!("record Bytes\n    values: List<Int>\n{src}")).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        let program = optimize(lower_program(&result.resolved_items));
        let (_, rewritten) = index_packed_byte_reads(program, &bytes_carrier());
        assert_eq!(rewritten, 0);
    }
}
