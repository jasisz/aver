//! AST + type-string walkers that pre-discover monomorphisations
//! `TypeRegistry::new` needs to allocate ahead of structural emit.
//!
//! Each `collect_*` family lives in three flavours:
//! - `_from_str`: walks a serialised type string (e.g. fn signature
//!   annotations, record field types) — handles balanced angle
//!   brackets / paren tuples / nested generics.
//! - `_from_fn_body` + `_from_expr`: recurse the typed AST so
//!   constructors / matches / calls that mention a generic
//!   instantiation NOT spelled in any signature still get
//!   registered (`Vector.new`, `Option.None`, `Map.empty`, etc.).
//!
//! Every registry the wasm-gc backend cares about — `Vector<T>`,
//! `Option<T>`, `Result<T, E>`, `List<T>`, `Map<K, V>`, tuple
//! shapes — has its own set, plus `collect_results_from_builtin_uses`
//! which seeds `Result<X, Y>` from dotted-builtin call sites that
//! return Result without naming it in a fn signature.
//!
//! Pure code movement out of `types.rs` (commit on
//! `0.18-span-foundation` after `wasip2_helpers.rs` /
//! `builtins_wasip2.rs` splits) — keeps `types.rs` focused on
//! the registry impl + type-name normalisation utilities.

use std::collections::HashMap;

use super::types::{TypeRegistry, normalize_compound};

pub(super) fn is_primitive(ty: &str) -> bool {
    matches!(ty.trim(), "Int" | "Float" | "Bool")
}

/// Walk a type string looking for `Vector<...>` substrings (with
/// balanced angle brackets). Each unique instantiation is registered
/// in `out` and order-tracked, with a freshly allocated wasm type idx.
/// Recurses into the element type so nested forms like
/// `Vector<Vector<Int>>` register both the outer and inner shapes —
/// the element of the outer needs a wasm-resolvable type.
pub(super) fn collect_vectors_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 7 <= bytes.len() {
        if &bytes[i..i + 7] == b"Vector<" {
            // Find the matching `>` for this `Vector<`.
            let mut depth: i32 = 1;
            let mut j = i + 7;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — nested Vectors register before
                // the outer needs them as an element type. Without
                // this, `Vector<Vector<Int>>` would emit a forward
                // reference to `$vector_int` and validators reject.
                let element = &trimmed[i + 7..j - 1];
                collect_vectors_from_str(element, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// Walk a fn body for builtin calls whose declared return type is a
/// `Result<T, E>` not otherwise visible in signatures. Phase-3c
/// targets the curated set the bench scenarios use; broader auto-
/// discovery would mean reading `types::checker::builtins` directly.
pub(super) fn collect_results_from_builtin_uses(
    fd: &crate::ir::hir::ResolvedFnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedExpr, ResolvedFnBody, ResolvedStmt};
    fn walk(
        e: &ResolvedExpr,
        out: &mut HashMap<String, u32>,
        order: &mut Vec<String>,
        next_idx: &mut u32,
    ) {
        let mut intern = |canonical: &str| {
            if !out.contains_key(canonical) {
                out.insert(canonical.to_string(), *next_idx);
                order.push(canonical.to_string());
                *next_idx += 1;
            }
        };
        match e {
            ResolvedExpr::Call(callee, args) => {
                if let crate::ir::hir::ResolvedCallee::Builtin(dotted) = callee {
                    match dotted.as_str() {
                        "Float.fromString" => intern("Result<Float,String>"),
                        "Int.fromString" | "Int.mod" | "Int.div" | "Random.int" => {
                            intern("Result<Int,String>")
                        }
                        "Bits.shiftLeft" | "Bits.shiftRight" | "Bits.low" => {
                            intern("Result<Int,String>")
                        }
                        "String.fromUtf8" => intern("Result<String,String>"),
                        "Console.readLine" | "Disk.readText" => intern("Result<String,String>"),
                        "Time.sleep"
                        | "Env.set"
                        | "Terminal.enableRawMode"
                        | "Terminal.disableRawMode"
                        | "Terminal.clear"
                        | "Terminal.moveTo"
                        | "Terminal.print"
                        | "Terminal.setColor"
                        | "Terminal.resetColor"
                        | "Terminal.hideCursor"
                        | "Terminal.showCursor"
                        | "Terminal.flush"
                        | "HttpServer.listen"
                        | "HttpServer.listenWith"
                        | "SelfHostRuntime.httpServerListen"
                        | "SelfHostRuntime.httpServerListenWith" => intern("Result<Unit,String>"),
                        "Terminal.size" => intern("Result<Terminal.Size,String>"),
                        "Terminal.readKey" => {
                            intern("Result<Option<String>,String>");
                        }
                        "Disk.readBytes" => {
                            // wasip2 reuses the raw-octet Disk.readText helper
                            // internally, then adapts its carrier to Bytes.
                            intern("Result<String,String>");
                            intern("Result<Bytes,String>");
                        }
                        "Disk.readBytesAt" => intern("Result<Bytes,String>"),
                        "Disk.size" => intern("Result<Int,String>"),
                        "Disk.writeText" | "Disk.appendText" | "Disk.writeBytes"
                        | "Disk.appendBytes" | "Disk.delete" | "Disk.deleteDir"
                        | "Disk.makeDir" => intern("Result<Unit,String>"),
                        "Disk.listDir" => intern("Result<List<String>,String>"),
                        "Tcp.connect" => intern("Result<Tcp.Connection,String>"),
                        "Tcp.beginConnect" => intern("Result<Tcp.Dial,String>"),
                        "Tcp.listen" => intern("Result<Tcp.Listener,String>"),
                        "Tcp.dialled" | "Tcp.accept" => {
                            intern("Result<Option<Tcp.Connection>,String>")
                        }
                        "Tcp.peerAddress" => intern("Result<String,String>"),
                        "Tcp.readLine" => intern("Result<String,String>"),
                        "Tcp.readBytes" | "Tcp.readSome" => intern("Result<Bytes,String>"),
                        "Tcp.poll" => intern("Result<List<Int>,String>"),
                        "Tcp.writeLine" | "Tcp.writeBytes" | "Tcp.close" | "Tcp.closeDial"
                        | "Tcp.closeListener" => intern("Result<Unit,String>"),
                        // `Tcp.send`, `Tcp.sendBytes`, and `Tcp.ping`
                        // are ephemeral (Phase 4.7+ passes 4 / 5) —
                        // none calls `__rt_tcp_connect` or returns a
                        // `Tcp.Connection`, so they only need their
                        // own return-shape slot. Earlier wrappers
                        // also interned `Result<Tcp.Connection,String>`
                        // and `Result<Unit,String>`; both are dead
                        // weight on the ephemeral path.
                        "Tcp.send" => intern("Result<String,String>"),
                        "Tcp.sendBytes" => intern("Result<Bytes,String>"),
                        "Tcp.ping" => intern("Result<Unit,String>"),
                        "Http.get" | "Http.head" | "Http.delete" | "Http.post" | "Http.put"
                        | "Http.patch" => intern("Result<HttpResponse,String>"),
                        _ => {}
                    }
                }
                for a in args {
                    walk(&a.node, out, order, next_idx);
                }
            }
            ResolvedExpr::BinOp(_, l, r) => {
                walk(&l.node, out, order, next_idx);
                walk(&r.node, out, order, next_idx);
            }
            ResolvedExpr::Neg(inner) => walk(&inner.node, out, order, next_idx),
            ResolvedExpr::Match { subject, arms } => {
                walk(&subject.node, out, order, next_idx);
                for arm in arms {
                    walk(&arm.body.node, out, order, next_idx);
                }
            }
            ResolvedExpr::TailCall { args, .. } => {
                for a in args {
                    walk(&a.node, out, order, next_idx);
                }
            }
            ResolvedExpr::Attr(obj, _) => walk(&obj.node, out, order, next_idx),
            ResolvedExpr::ErrorProp(inner) => walk(&inner.node, out, order, next_idx),
            ResolvedExpr::Ctor(_, args) => {
                for a in args {
                    walk(&a.node, out, order, next_idx);
                }
            }
            ResolvedExpr::RecordCreate { fields, .. } => {
                for (_, e) in fields {
                    walk(&e.node, out, order, next_idx);
                }
            }
            ResolvedExpr::RecordUpdate { base, updates, .. } => {
                walk(&base.node, out, order, next_idx);
                for (_, e) in updates {
                    walk(&e.node, out, order, next_idx);
                }
            }
            ResolvedExpr::List(items) | ResolvedExpr::Tuple(items) => {
                for x in items {
                    walk(&x.node, out, order, next_idx);
                }
            }
            ResolvedExpr::IndependentProduct(items, _) => {
                for x in items {
                    walk(&x.node, out, order, next_idx);
                }
            }
            ResolvedExpr::MapLiteral(entries) => {
                for (k, v) in entries {
                    walk(&k.node, out, order, next_idx);
                    walk(&v.node, out, order, next_idx);
                }
            }
            // A `Result<Int,String>`-returning call (or a big-int literal, below)
            // can be buried inside interpolation — recurse so its result slot is
            // still interned. Mirrors the builtin-discovery walk in `module.rs`.
            ResolvedExpr::InterpolatedStr(parts) => {
                for p in parts {
                    if let crate::ir::hir::ResolvedStrPart::Parsed(inner) = p {
                        walk(&inner.node, out, order, next_idx);
                    }
                }
            }
            // A big-int literal lowers through `Int.fromString` (returns
            // `Result<Int,String>`), so its result type slot must exist.
            ResolvedExpr::Literal(crate::ast::Literal::BigInt(_)) => intern("Result<Int,String>"),
            _ => {}
        }
    }
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => &e.node,
        };
        walk(expr, out, order, next_idx);
    }
}

/// `Tuple<A, B>` discovery — scans both `Tuple<...>` (canonical we
/// emit internally) and `(A, B)` (the surface Aver syntax that the
/// type checker renders Type::Tuple as). Both forms register under
/// the same `Tuple<A,B>` whitespace-free canonical so downstream
/// lookups work uniformly.
pub(super) fn collect_tuples_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        // `Tuple<...>` form
        if i + 6 <= bytes.len() && &bytes[i..i + 6] == b"Tuple<" {
            let mut depth: i32 = 1;
            let mut j = i + 6;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                let inner = &trimmed[i + 6..j - 1];
                collect_tuples_from_str(inner, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical);
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        // `(A, B)` surface form. Parens-balanced, exactly one
        // top-level comma → tuple. Skips fn-call args and grouping
        // parens (those don't carry a comma at the top level here).
        if bytes[i] == b'(' {
            let mut depth: i32 = 1;
            let mut j = i + 1;
            let mut top_commas = 0;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'(' | b'<' => depth += 1,
                    b')' | b'>' => depth -= 1,
                    b',' if depth == 1 => top_commas += 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 && top_commas >= 1 {
                let inner = &trimmed[i + 1..j - 1];
                // A `(` immediately preceded by `Fn` is a function-type
                // parameter list (`Fn(BranchPath, Int, Int) -> Int`), NOT
                // a tuple. Recurse into the inner so any genuinely-nested
                // tuple param (`Fn((Int, Int), Int) -> Int`) is still
                // discovered, but never intern the param list itself —
                // doing so registered a phantom `Tuple<…>` whose eq/hash
                // helpers later tried to dispatch on opaque param types
                // (e.g. `BranchPath`) that have no structural eq.
                let preceded_by_fn = trimmed[..i].trim_end().ends_with("Fn");
                collect_tuples_from_str(inner, out, order, next_idx);
                if !preceded_by_fn {
                    // Build canonical `Tuple<A,B,...>` form
                    let canonical_inner: String =
                        inner.chars().filter(|c| !c.is_whitespace()).collect();
                    let canonical = format!("Tuple<{canonical_inner}>");
                    if !out.contains_key(&canonical) {
                        out.insert(canonical.clone(), *next_idx);
                        order.push(canonical);
                        *next_idx += 1;
                    }
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// Walk a fn body for `ResolvedExpr::Tuple` literals — register each unique
/// `Tuple<A,B>` derived from the items' stamped types. Mirrors the
/// body walks already done for `List`/`Option`/`Vector` discovery.
pub(super) fn collect_tuples_from_fn_body(
    fd: &crate::ir::hir::ResolvedFnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        if let ResolvedStmt::Binding {
            ty_ann: Some(annot),
            ..
        } = stmt
        {
            collect_tuples_from_str(&annot.display(), out, order, next_idx);
        }
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => e,
        };
        collect_tuples_from_expr(expr, out, order, next_idx);
    }
}

pub(super) fn collect_tuples_from_expr(
    expr: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::ResolvedExpr;
    // Register the canonical for this node first if it's a tuple
    // literal — derived from the items' stamped types so nested
    // `(a, (b, c))` registers both inner and outer canonicals.
    // `ResolvedExpr::IndependentProduct` (the `(...)!` form) lowers as a
    // tuple under wasm-gc and shares the same canonical shape.
    // `(...)?!` (unwrap=true) is a tuple of unwrapped Ok types —
    // strip the Result<_,_> wrapper from each stamped element type
    // before building the canonical.
    let tuple_items_with_unwrap: Option<(
        &Vec<crate::ast::Spanned<crate::ir::hir::ResolvedExpr>>,
        bool,
    )> = match &expr.node {
        ResolvedExpr::Tuple(xs) => Some((xs, false)),
        ResolvedExpr::IndependentProduct(xs, unwrap) => Some((xs, *unwrap)),
        _ => None,
    };
    if let Some((items, unwrap)) = tuple_items_with_unwrap
        && items.len() >= 2
    {
        let elem_tys: Vec<String> = items
            .iter()
            .map(|item| {
                // Defensive fallback for any Spanned that escaped
                // typecheck without a stamp (synthesised AST etc.).
                // The guard below skips canonicals that pick this up.
                let stamped = item
                    .ty()
                    .map(|t| t.display())
                    .unwrap_or_else(|| "Invalid".into());
                if unwrap {
                    // `?!` element must be Result<T, E>; canonical
                    // tuple uses T (the Ok payload type).
                    let stripped: String = stamped.chars().filter(|c| !c.is_whitespace()).collect();
                    if let Some((t, _)) = TypeRegistry::result_te(&stripped) {
                        return t.to_string();
                    }
                }
                stamped
            })
            .collect();
        // Use `normalize_compound` so nested paren-tuples like
        // `(Int, (Int, Int))` collapse to `Tuple<Int,Tuple<Int,Int>>`
        // — the same canonical the registry-lookup path
        // (`tuple_type_idx`) normalizes to.
        let canonical: String = normalize_compound(&format!("Tuple<{}>", elem_tys.join(",")));
        if !canonical.contains("Invalid") && !out.contains_key(&canonical) {
            for elem in &elem_tys {
                collect_tuples_from_str(elem, out, order, next_idx);
            }
            out.insert(canonical.clone(), *next_idx);
            order.push(canonical);
            *next_idx += 1;
        }
    }
    // Recurse into sub-expressions.
    match &expr.node {
        ResolvedExpr::Call(_, args) => {
            for a in args {
                collect_tuples_from_expr(a, out, order, next_idx);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            collect_tuples_from_expr(l, out, order, next_idx);
            collect_tuples_from_expr(r, out, order, next_idx);
        }
        ResolvedExpr::Neg(inner) => collect_tuples_from_expr(inner, out, order, next_idx),
        ResolvedExpr::Match { subject, arms } => {
            collect_tuples_from_expr(subject, out, order, next_idx);
            for arm in arms {
                collect_tuples_from_expr(&arm.body, out, order, next_idx);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_tuples_from_expr(a, out, order, next_idx);
            }
        }
        ResolvedExpr::Attr(obj, _) => collect_tuples_from_expr(obj, out, order, next_idx),
        ResolvedExpr::ErrorProp(inner) => collect_tuples_from_expr(inner, out, order, next_idx),
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_tuples_from_expr(a, out, order, next_idx);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_tuples_from_expr(e, out, order, next_idx);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_tuples_from_expr(base, out, order, next_idx);
            for (_, e) in updates {
                collect_tuples_from_expr(e, out, order, next_idx);
            }
        }
        ResolvedExpr::List(items) => {
            for x in items {
                collect_tuples_from_expr(x, out, order, next_idx);
            }
        }
        ResolvedExpr::Tuple(items) | ResolvedExpr::IndependentProduct(items, _) => {
            for x in items {
                collect_tuples_from_expr(x, out, order, next_idx);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_tuples_from_expr(k, out, order, next_idx);
                collect_tuples_from_expr(v, out, order, next_idx);
            }
        }
        _ => {}
    }
}

/// `Result<T, E>` discovery — handles nested commas via depth tracking.
pub(super) fn collect_results_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 7 <= bytes.len() {
        if &bytes[i..i + 7] == b"Result<" {
            let mut depth: i32 = 1;
            let mut j = i + 7;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — nested Results land in `result_order`
                // before the outer references them as T or E. Same
                // forward-ref guard as in the other collectors.
                let inner = &trimmed[i + 7..j - 1];
                collect_results_from_str(inner, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// `List<...>` discovery. Normalises paren-tuple `(A, B)` to
/// `Tuple<A,B>` first so a `List<(A, B)>` from a fn signature and
/// a `List<Tuple<A,B>>` from eager registration land on the same
/// canonical (otherwise the type section ends up with two duplicate
/// list slots whose elements are structurally equal but whose ref
/// indices diverge).
pub(super) fn collect_lists_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let normalized = normalize_compound(type_str);
    let bytes = normalized.as_bytes();
    let mut i = 0;
    while i + 5 <= bytes.len() {
        if &bytes[i..i + 5] == b"List<" {
            let mut depth: i32 = 1;
            let mut j = i + 5;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                let element = &normalized[i + 5..j - 1];
                collect_lists_from_str(element, out, order, next_idx);
                let canonical = normalized[i..j].to_string();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// `Option<...>` discovery — same shape as `collect_vectors_from_str`.
pub(super) fn collect_options_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 7 <= bytes.len() {
        if &bytes[i..i + 7] == b"Option<" {
            let mut depth: i32 = 1;
            let mut j = i + 7;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — same rationale as in
                // `collect_lists_from_str`. `Option<Option<T>>`
                // (rare but legal) needs the inner Option already
                // present in `option_order` before the outer's
                // struct field references it.
                let element = &trimmed[i + 7..j - 1];
                collect_options_from_str(element, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// Walk fn body for `Option.Some(payload)` constructors and bindings
/// that imply an `Option<T>` instantiation. Unlike Vector, the bench
/// scenarios mostly leave Option<T> implicit — `Map.get` returns
/// `Option<V>` where V is read off `Map<K,V>` — so the body walk is
/// the primary discovery path.
pub(super) fn collect_options_from_fn_body(
    fd: &crate::ir::hir::ResolvedFnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        if let ResolvedStmt::Binding {
            ty_ann: Some(annot),
            ..
        } = stmt
        {
            collect_options_from_str(&annot.display(), out, order, next_idx);
        }
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => &e.node,
        };
        collect_options_from_expr(expr, out, order, next_idx);
    }
}

pub(super) fn collect_options_from_expr(
    expr: &crate::ir::hir::ResolvedExpr,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedStrPart};
    match expr {
        ResolvedExpr::Call(callee, args) => {
            // String-producing optional helpers need Option<String> even
            // declare `Option<String>` as their return type, but the
            // canonical never appears anywhere else — eager-register
            // it here so the builtin's emit can resolve the slot.
            if let ResolvedCallee::Builtin(dotted) = callee
                && matches!(
                    dotted.as_str(),
                    "String.charAt" | "String.fromCodePoint" | "Terminal.readKey" | "Env.get"
                )
            {
                let canonical = "Option<String>".to_string();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical);
                    *next_idx += 1;
                }
            }
            if let ResolvedCallee::Builtin(dotted) = callee
                && dotted == "String.firstCodePoint"
            {
                let canonical = "Option<Int>".to_string();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical);
                    *next_idx += 1;
                }
            }
            if let ResolvedCallee::Unresolved { callee } = callee {
                collect_options_from_expr(&callee.node, out, order, next_idx);
            }
            for a in args {
                collect_options_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            collect_options_from_expr(&l.node, out, order, next_idx);
            collect_options_from_expr(&r.node, out, order, next_idx);
        }
        ResolvedExpr::Neg(inner) => collect_options_from_expr(&inner.node, out, order, next_idx),
        ResolvedExpr::Match { subject, arms } => {
            collect_options_from_expr(&subject.node, out, order, next_idx);
            for arm in arms {
                collect_options_from_expr(&arm.body.node, out, order, next_idx);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_options_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Attr(obj, _) => collect_options_from_expr(&obj.node, out, order, next_idx),
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_options_from_expr(&e.node, out, order, next_idx);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_options_from_expr(&base.node, out, order, next_idx);
            for (_, e) in updates {
                collect_options_from_expr(&e.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_options_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => {
            for it in items {
                collect_options_from_expr(&it.node, out, order, next_idx);
            }
        }
        ResolvedExpr::ErrorProp(inner) => {
            collect_options_from_expr(&inner.node, out, order, next_idx)
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let ResolvedStrPart::Parsed(inner) = part {
                    collect_options_from_expr(&inner.node, out, order, next_idx);
                }
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_options_from_expr(&key.node, out, order, next_idx);
                collect_options_from_expr(&value.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
    }
}

/// Walk fn body looking for binding annotations and `Vector.new` calls
/// that imply a `Vector<T>` instantiation. The fill-arg's type fixes
/// `T` for `Vector.new`; binding annotations carry the type string
/// directly. Mirrors the surface-level discovery — the vector_ops
/// bench spells out `Vector<Int>` in fn signatures so this body walk
/// is a defensive backstop, not the primary discovery path.
pub(super) fn collect_vectors_from_fn_body(
    fd: &crate::ir::hir::ResolvedFnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        if let ResolvedStmt::Binding {
            ty_ann: Some(annot),
            ..
        } = stmt
        {
            collect_vectors_from_str(&annot.display(), out, order, next_idx);
        }
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => &e.node,
        };
        collect_vectors_from_expr(expr, out, order, next_idx);
    }
}

pub(super) fn collect_vectors_from_expr(
    expr: &crate::ir::hir::ResolvedExpr,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedStrPart};
    match expr {
        ResolvedExpr::Call(callee, args) => {
            // `Vector.new(n, fill)` instantiates `Vector<T>` where `T` is
            // the *type* of the fill arg — and crucially, that may itself
            // be a Vector (`Vector.new(3, inner_vec)` produces
            // `Vector<Vector<Int>>`). The container's own type stamp
            // tells us `T` directly; without this branch nested-Vector
            // instantiations never surface for the walker, registry has
            // no slot, codegen errors with "instantiation `Vector<…>`
            // was not registered". Mirrors the InterpolatedStr / `+`
            // branches that pre-register `Vector<String>` from a stamp.
            if (matches!(
                callee,
                crate::ir::hir::ResolvedCallee::Builtin(name) if name == "Vector.new"
            ) || matches!(
                callee,
                crate::ir::hir::ResolvedCallee::Intrinsic(
                    crate::ir::hir::BuiltinIntrinsic::VectorNew
                )
            )) && args.len() == 2
                && let Some(fill_ty) = args[1].ty()
            {
                let canonical = format!("Vector<{}>", fill_ty.display());
                collect_vectors_from_str(&canonical, out, order, next_idx);
            }
            if let ResolvedCallee::Unresolved { callee } = callee {
                collect_vectors_from_expr(&callee.node, out, order, next_idx);
            }
            for a in args {
                collect_vectors_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::BinOp(op, l, r) => {
            // String `+` lowers to `__wasmgc_concat_n`, which takes a
            // `Vector<String>` (array of String refs). Register that
            // canonical eagerly when we see the operator on String
            // operands. Mirrors the InterpolatedStr arm below.
            if matches!(op, crate::ast::BinOp::Add)
                && l.ty().is_some_and(|t| t.display().trim() == "String")
            {
                collect_vectors_from_str("Vector<String>", out, order, next_idx);
            }
            collect_vectors_from_expr(&l.node, out, order, next_idx);
            collect_vectors_from_expr(&r.node, out, order, next_idx);
        }
        ResolvedExpr::Neg(inner) => collect_vectors_from_expr(&inner.node, out, order, next_idx),
        ResolvedExpr::Match { subject, arms } => {
            collect_vectors_from_expr(&subject.node, out, order, next_idx);
            for arm in arms {
                collect_vectors_from_expr(&arm.body.node, out, order, next_idx);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_vectors_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Attr(obj, _) => collect_vectors_from_expr(&obj.node, out, order, next_idx),
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_vectors_from_expr(&e.node, out, order, next_idx);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_vectors_from_expr(&base.node, out, order, next_idx);
            for (_, e) in updates {
                collect_vectors_from_expr(&e.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_vectors_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => {
            for it in items {
                collect_vectors_from_expr(&it.node, out, order, next_idx);
            }
        }
        ResolvedExpr::ErrorProp(inner) => {
            collect_vectors_from_expr(&inner.node, out, order, next_idx)
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            // Interpolation lowers to an `array.new_fixed (array (ref
            // null $string)) N` + variadic concat. The array type
            // shares a slot with `Vector<String>` (same wasm shape),
            // so register it here even if no Aver-level signature
            // mentions Vector<String>.
            collect_vectors_from_str("Vector<String>", out, order, next_idx);
            for p in parts {
                if let ResolvedStrPart::Parsed(inner) = p {
                    collect_vectors_from_expr(&inner.node, out, order, next_idx);
                }
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_vectors_from_expr(&key.node, out, order, next_idx);
                collect_vectors_from_expr(&value.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
    }
}

/// Walk a type string looking for `Map<K, V>` substrings (with
/// balanced angle brackets), append each canonical (whitespace-
/// stripped) form to `out`. Recurses into both K and V so nested
/// types (`Map<String, Vector<Int>>`) register every reachable
/// outer + inner instantiation.
pub(super) fn collect_maps_from_str(type_str: &str, out: &mut Vec<String>) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 4 <= bytes.len() {
        if &bytes[i..i + 4] == b"Map<" {
            let mut depth: i32 = 1;
            let mut j = i + 4;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — same forward-ref guard as the
                // other compound-type collectors. Nested Maps land
                // before the outer references them as K or V.
                let inner = &trimmed[i + 4..j - 1];
                collect_maps_from_str(inner, out);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                out.push(canonical);
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// Walk every sub-expression and harvest `Map<K,V>` canonicals from
/// each `Spanned<Expr>::ty()` stamp. Catches map literals (`{a => 3}`
/// without annotation) and expressions whose type is a Map but the
/// canonical never shows up in any signature — symmetrical with
/// `collect_tuples_from_expr`. Recurses into sub-expressions so
/// nested literals / fn args / match arms all get their stamps
/// harvested.
pub(super) fn collect_maps_from_expr(
    expr: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>,
    out: &mut Vec<String>,
) {
    use crate::ir::hir::ResolvedExpr;
    if let Some(ty) = expr.ty() {
        let display = ty.display();
        collect_maps_from_str(&display, out);
    }
    match &expr.node {
        ResolvedExpr::Call(_, args) => {
            for a in args {
                collect_maps_from_expr(a, out);
            }
        }
        ResolvedExpr::List(items)
        | ResolvedExpr::Tuple(items)
        | ResolvedExpr::IndependentProduct(items, _) => {
            for it in items {
                collect_maps_from_expr(it, out);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_maps_from_expr(k, out);
                collect_maps_from_expr(v, out);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_maps_from_expr(v, out);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_maps_from_expr(base, out);
            for (_, v) in updates {
                collect_maps_from_expr(v, out);
            }
        }
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_maps_from_expr(a, out);
            }
        }
        ResolvedExpr::Match { subject, arms } => {
            collect_maps_from_expr(subject, out);
            for arm in arms {
                collect_maps_from_expr(&arm.body, out);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            collect_maps_from_expr(l, out);
            collect_maps_from_expr(r, out);
        }
        ResolvedExpr::Neg(inner) => collect_maps_from_expr(inner, out),
        ResolvedExpr::Attr(e, _) | ResolvedExpr::ErrorProp(e) => collect_maps_from_expr(e, out),
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_maps_from_expr(a, out);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tuple_discovery_tests {
    use super::collect_tuples_from_str;
    use std::collections::HashMap;

    fn collect(type_str: &str) -> Vec<String> {
        let mut out: HashMap<String, u32> = HashMap::new();
        let mut order: Vec<String> = Vec::new();
        let mut next = 0u32;
        collect_tuples_from_str(type_str, &mut out, &mut order, &mut next);
        order
    }

    #[test]
    fn fn_param_list_is_not_a_tuple() {
        // The param list of a `Fn(...)` type is a function-type
        // parameter list, NOT a tuple. Registering it as
        // `Tuple<BranchPath,Int,Int,Int>` produced a phantom carrier
        // whose eq helper dispatched on the opaque `BranchPath`
        // param and failed wasm-gc validation.
        let order = collect("Fn(BranchPath, Int, Int, Int) -> Result<Int, String>");
        assert!(
            order.is_empty(),
            "a Fn(..) param list must not register any tuple, got {order:?}"
        );
    }

    #[test]
    fn genuine_surface_tuple_still_registers() {
        let order = collect("(Int, String)");
        assert_eq!(order, vec!["Tuple<Int,String>".to_string()]);
    }

    #[test]
    fn nested_tuple_inside_fn_param_still_registers() {
        // A real tuple nested inside a Fn param list is still a tuple
        // and must be discovered — only the param-list parens are not.
        let order = collect("Fn((Int, Int), String) -> Bool");
        assert_eq!(order, vec!["Tuple<Int,Int>".to_string()]);
    }

    #[test]
    fn tuple_return_of_fn_still_registers() {
        // `Fn(Int) -> (Int, Int)` — the return tuple is genuine.
        let order = collect("Fn(Int) -> (Int, Int)");
        assert_eq!(order, vec!["Tuple<Int,Int>".to_string()]);
    }
}
