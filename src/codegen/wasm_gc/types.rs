//! Aver type → wasm-gc representation.
//!
//! Two layers:
//!
//! 1. **Primitives** — `Int → i64`, `Float → f64`, `Bool → i32`,
//!    `Unit → empty`. These map directly without any module-level
//!    type-section entry.
//!
//! 2. **User types** — records and variants. Each `record Foo { … }`
//!    becomes a `(type $Foo (struct (field T_1) … (field T_N)))` in
//!    the wasm type section; the struct's type index is recorded in
//!    `TypeRegistry` so emit sites can resolve `RecordCreate { type_name }`
//!    and `Attr { obj, field }` to `struct.new` / `struct.get` against
//!    the right struct.
//!
//! Variants (`type Shape = Circle(Float) | Rect(Float, Float)`) get
//! one empty non-final root struct for the sum plus one final struct
//! subtype per constructor. Pattern matching dispatches through
//! `ref.test` against those concrete constructor types.

use std::collections::HashMap;

use wasm_encoder::{
    AbstractHeapType, FieldType, HeapType, RefType, StorageType, StructType, ValType,
};

/// Canonical operational discriminants for the built-in carrier layouts.
/// Constructors, match lowering, and certificate-plan production all read
/// these constants so the tag bytes cannot drift between paths.
pub(crate) const OPTION_NONE_TAG: i32 = 0;
pub(crate) const OPTION_SOME_TAG: i32 = 1;
pub(crate) const RESULT_ERR_TAG: i32 = 0;
pub(crate) const RESULT_OK_TAG: i32 = 1;

use super::WasmGcError;
use super::types_discovery::{
    collect_lists_from_str, collect_maps_from_expr, collect_maps_from_str,
    collect_options_from_fn_body, collect_options_from_str, collect_results_from_builtin_uses,
    collect_results_from_str, collect_tuples_from_fn_body, collect_tuples_from_str,
    collect_vectors_from_fn_body, collect_vectors_from_str, is_primitive,
};

use crate::ast::{TopLevel, Type, TypeDef};

/// Wasm storage selected for an opaque structural refinement whose carrier is
/// `List<Int>`. The nominal Aver type is represented directly by this array;
/// its source-level list is materialised only at the smart-constructor and
/// field-projection boundaries.
#[derive(Debug, Clone, Copy)]
pub(super) struct PackedSequenceType {
    pub(super) type_idx: u32,
    pub(super) layout: crate::codegen::proof_lower::PackedSequenceLayout,
}

/// User-type lookup tables built once before any fn body emit.
pub(super) struct TypeRegistry {
    /// `record_name → type_idx` for product (record) types.
    pub(super) records: HashMap<String, u32>,
    /// `sum_name → type_idx` for the empty, non-final nominal root
    /// struct shared by every constructor of that sum.
    pub(super) sum_roots: HashMap<String, u32>,
    /// `variant_constructor_name → (parent_type_name, type_idx, fields)`.
    /// `fields` are the type strings of the constructor's positional
    /// fields (Aver variants use positional fields, not named ones).
    /// Bare variant name → every registered variant with that name
    /// (one per parent sumtype). Two sumtypes can share a variant
    /// name (e.g. `Query.ProviderSummary` and `QueryOutput.
    /// ProviderSummary` in payment_ops); without the parent in the
    /// key, one would silently shadow the other and emit would pick
    /// the wrong concrete struct type.
    pub(super) variants: HashMap<String, Vec<VariantInfo>>,
    /// `record_name → field list` so `Attr` can resolve a field name
    /// to its struct field index + type.
    pub(super) record_fields: HashMap<String, Vec<(String, String)>>,
    /// Per-instantiation `Vector<T>` slot. Key is the canonical Aver
    /// type string (e.g. `"Vector<Int>"`). Value is the wasm type idx
    /// of the underlying `(array (mut T))`. Monomorphized: each `T`
    /// reachable in the program gets its own slot, so element access
    /// is type-direct (no anyref / no boxing).
    pub(super) vector_types: HashMap<String, u32>,
    /// Insertion order for `vector_types` — used by module emit so
    /// type-section entries land at the indices the registry recorded.
    pub(super) vector_order: Vec<String>,
    /// Per-instantiation `Option<T>` slot. Same monomorphisation
    /// strategy as `vector_types`. Each `Option<T>` lowers to a
    /// `(struct (mut i32 tag) (mut T value))` — tag=0 None, tag=1
    /// Some. The `value` field carries a default for None (zero for
    /// numerics, null for ref types) so `struct.new` always has a
    /// valid initial value; pattern matching reads `tag` first and
    /// only consumes `value` on the Some branch.
    pub(super) option_types: HashMap<String, u32>,
    pub(super) option_order: Vec<String>,
    /// Per-instantiation `List<T>` slot. Each `List<T>` lowers to a
    /// recursive struct `(struct (field T) (field (ref null $list_T)))`
    /// — Cons cell. Empty list = `(ref null $list_T)` null. Self-
    /// reference is allowed within a single type definition (wasm spec
    /// implicitly makes each top-level type its own rec group).
    pub(super) list_types: HashMap<String, u32>,
    pub(super) list_order: Vec<String>,
    /// Per-instantiation `Result<T, E>` slot. Each `Result<T, E>`
    /// lowers to `(struct (mut i32 tag) (mut T ok_value) (mut E
    /// err_value))` — tag=0 Err, tag=1 Ok. Both payload fields exist
    /// concurrently because the struct can't be a sum at the wasm
    /// type level; the unused field is filled with a default (zero
    /// for primitives, null for refs).
    pub(super) result_types: HashMap<String, u32>,
    pub(super) result_order: Vec<String>,
    /// Per-instantiation `Map<K, V>` slot triple (keys array, values
    /// array, map struct). Same monomorphisation strategy as Vector /
    /// Option — each unique `Map<K, V>` reachable in the program gets
    /// its own three slots and table/read/ordered-iteration helpers.
    pub(super) map_types: HashMap<String, MapSlots>,
    pub(super) map_order: Vec<String>,
    /// Shared scratch-array type used by canonical Map iteration. Each
    /// materialisation collects occupied bucket indices, sorts those `i32`
    /// indices by their keys, then lets `keys` / `values` / `entries` read the
    /// same permutation. Present iff at least one Map instantiation exists.
    pub(super) map_order_indices_type_idx: Option<u32>,
    /// Per-instantiation `Tuple<A, B>` slot. Each lowers to a
    /// `(struct (mut A) (mut B))`. Used by `Map.entries` (returns
    /// `List<Tuple<K, V>>`), `Map.fromList`, and `List.zip`.
    pub(super) tuple_types: HashMap<String, u32>,
    pub(super) tuple_order: Vec<String>,
    /// Per-K `(struct (mut K))` slot used when raw K cannot share the
    /// open-addressing occupancy marker. Primitives have no null;
    /// `List<T>` uses null for the valid `[]` value. Boxing both makes
    /// `keys[i] == null` mean only "unused bucket". Helpers internally
    /// pass raw `K_val` and box on insert / unbox on read.
    pub(super) primitive_key_box: HashMap<String, u32>,
    pub(super) primitive_key_box_order: Vec<String>,
    /// Proof-derived packed representations for opaque `List<Int>`
    /// refinements. Keyed by bare nominal type name. Unlike ordinary record
    /// newtyping this changes the carrier shape, so construct/project sites
    /// bridge between `List<Int>` and the packed array explicitly.
    pub(super) packed_sequences: HashMap<String, PackedSequenceType>,
    pub(super) packed_sequence_order: Vec<String>,
    /// Identity-preserving qualified→bare type-name aliases derived by
    /// `flatten_multimodule` from its collision info: `"Dep.Octets"` →
    /// `"Octets"` ONLY when `Dep` is the sole declarer of the bare name
    /// (no dep-dep collision, no entry-declared type of the same bare
    /// name), so the alias provably denotes the same `TypeDef`. Consulted
    /// by the name-keyed representation lookups (`packed_sequence`,
    /// `is_eligible_carrier`, `is_eligible_carrier_field`,
    /// `newtype_underlying`) because entry-side qualified annotation
    /// stamps survive into codegen. A collision-renamed dep type never
    /// lands here — its canonical `TypeDef` name IS the qualified form, so
    /// exact lookups keep working and the #792 exact-name soundness rule
    /// (no suffix guessing) is preserved.
    pub(super) type_name_aliases: HashMap<String, String>,
    /// Total number of user-type slots reserved in the type section.
    /// Function types start AFTER these.
    pub(super) user_type_count: u32,
    /// Wasm type idx for the `(array i8)` String representation.
    /// Allocated lazily on first reference; `None` when no String is
    /// reachable from the program (most numeric bench scenarios).
    /// See `builtins/` README for the full repr decision.
    pub(super) string_array_type_idx: Option<u32>,
    /// Hidden codepoint→UTF-8 byte-boundary table synthesized by the
    /// loop-scoped String indexing pass. Represented directly as a mutable
    /// `(array i32)` and allocated only when an indexed worker signature
    /// carries the compiler-internal `String.Index` type.
    pub(super) string_index_array_type_idx: Option<u32>,
    /// Internal mutable i32 arrays used by the pure `Crypto.sha256` helper:
    /// one for the padded message bytes and one for the 64-word schedule.
    /// They are allocated only when that intrinsic is reachable.
    pub(super) crypto_byte_array_type_idx: Option<u32>,
    pub(super) crypto_word_array_type_idx: Option<u32>,
    /// Per-byte-sequence passive data segment for `String` literals.
    /// Each unique literal in the program lands at one segment idx;
    /// `ResolvedExpr::Literal(Literal::Str(_))` lowers to `array.new_data
    /// $string $segment_idx` with offset=0, size=len.
    pub(super) string_literals: Vec<Vec<u8>>,
    pub(super) string_literal_idx: HashMap<Vec<u8>, u32>,
    /// Type names that must NOT be erased to their underlying
    /// primitive by the newtype optimisation. Populated with every
    /// record/variant used as a `Map<K, *>` key — Map's open-
    /// addressing layout uses `keys[i] == null` as the empty marker,
    /// which only works when keys are emitted as ref values.
    pub(super) non_newtypable_keys: std::collections::HashSet<String>,
    /// ETAP-2 carrier-`i64`: refinement-via-opaque carrier types whose
    /// proven smart-constructor bound `fits_i64`, keyed by bare Aver name
    /// (e.g. `"IntRange"`). For a name in this set, the newtype erasure
    /// produces a NATIVE `i64` (not the `$AverInt` ref) EVERYWHERE the
    /// carrier appears — fn slots, record fields, Option/Result payloads,
    /// `Vector<Carrier>` elements — because every shape routes through
    /// `aver_to_wasm`. The carrier `IS` the `i64`; the two boundary
    /// conversions (construct unboxes the `$AverInt` field value to `i64`
    /// via `__aint_to_i64_checked` — which can never trap because the bound
    /// proves the fit; projection boxes the `i64` back to `$AverInt` via
    /// `__aint_from_i64`) live at the carrier construct / project emit
    /// sites. EMPTY (the default) reproduces the all-`$aint` behavior
    /// byte-for-byte; only an opaque carrier with a recognized, `fits_i64`
    /// invariant lands here. Built from
    /// [`crate::codegen::proof_lower::carrier_interval_table`].
    pub(super) eligible_carriers: std::collections::HashSet<String>,
    /// ETAP-2 multi-field carrier-`i64`: the eligible `(record, field)` pairs
    /// whose proven smart-constructor bound `fits_i64`. For a pair in this set,
    /// the record's wasm STRUCT FIELD lowers to a native `i64` (instead of the
    /// `$AverInt` ref), and the field read / record construct bridge the i64
    /// boundary the same way the single-field carrier does. A multi-field record
    /// can have a MIX: one bounded Int field → i64, another unbounded field →
    /// boxed. Built from
    /// [`crate::codegen::proof_lower::field_carrier_eligible_intervals`] (already
    /// demotion-tightened); EMPTY (the default) reproduces the all-`$AverInt`
    /// multi-field record layout byte-for-byte.
    pub(super) eligible_carrier_fields: std::collections::HashSet<(String, String)>,
    /// Phase 4 (0.20) — `(struct (mut i32 socket) (mut i32 in_stream)
    /// (mut i32 out_stream) (mut i32 in_use))` slot type for an entry
    /// in the TCP connection pool. `None` when no `Tcp.*` effect is
    /// declared in any fn. The pool itself (`tcp_pool_type_idx`) is
    /// an `(array (mut $tcp_slot))` containing 256 of these.
    pub(super) tcp_slot_type_idx: Option<u32>,
    /// Phase 4 (0.20) — `(array (mut $tcp_slot))` array type carrying
    /// 256 connection slots. `None` when no `Tcp.*` effect is
    /// declared. The runtime allocates the array lazily on first
    /// `Tcp.connect` call via `array.new_default` against this idx.
    pub(super) tcp_pool_type_idx: Option<u32>,
    /// Arbitrary-precision `Int` (`Int = ℤ`, the only wasm-gc Int
    /// semantics). `true` when any Int arithmetic is reachable — a SIZE
    /// reachability gate, NOT a semantics flag (so pure-String/Float/
    /// effect-only programs carry zero bignum bytes). When set, `Int`
    /// lowers to `(ref null $AverInt)` everywhere instead of the scalar
    /// `i64`, and `*` / `+` / `-` / neg / cmp / eq route through the
    /// limb-arithmetic WAT helpers rather than the wrapping `i64.*`
    /// opcodes.
    pub(super) bignum: bool,
    /// bignum slice 1 — wasm type idx for the `$AverInt` struct
    /// `(struct (field $small i64) (field $mag (ref null (array i64)))
    /// (field $sign i32))`. `Some` iff `bignum` is set. `$mag == null`
    /// → Small (value in `$small`, the i64 fast path); non-null `$mag`
    /// → Big (little-endian unsigned u64 limb magnitude + `$sign ∈
    /// {-1,0,+1}`). Mirrors `aver-rt/src/int.rs` `AverInt`.
    pub(super) aint_struct_idx: Option<u32>,
    /// bignum slice 1 — wasm type idx for the `(array (mut i64))` limb
    /// magnitude array referenced by `$AverInt.$mag`. `Some` iff
    /// `bignum` is set; sits one slot below `aint_struct_idx` so the
    /// struct can reference it without a forward edge.
    pub(super) aint_mag_array_idx: Option<u32>,
    /// bignum slice 4 (eq+hash gap) — wasm fn idx of the `__aint_eq`
    /// helper, set by `module.rs` once `BuiltinRegistry::assign_slots`
    /// runs. `Some` iff `bignum`. Every Int-eq site (Map keys, Set
    /// members, record/sum fields, carrier payloads) routes through this
    /// instead of an `i64.eq` on a `$AverInt` ref (which is invalid wasm
    /// and wrong across the Small/Big boundary). Threaded via the
    /// registry (already passed to every emitter) rather than per-fn args.
    pub(super) aint_eq_fn_idx: Option<u32>,
    /// Canonical three-way Int comparator (`-1 / 0 / 1`) used by ordered Map
    /// iteration when Int appears in a key, directly or inside a composite.
    pub(super) aint_cmp_fn_idx: Option<u32>,
    /// bignum slice 4 (eq+hash gap) — wasm fn idx of the `__aint_hash`
    /// helper, set alongside `aint_eq_fn_idx`. `Some` iff `bignum`.
    /// Equal `$AverInt` values hash equal (Small folds `$small`, Big
    /// folds limbs+sign); the inline `i32.wrap_i64` over a raw value is
    /// invalid on a ref and collision-collapses every Big to one bucket.
    pub(super) aint_hash_fn_idx: Option<u32>,
    /// wasm fn idx of the `__aint_from_i64` canonical Small constructor,
    /// recorded alongside `aint_eq_fn_idx`. `Some` iff `bignum`. Used by
    /// the host-ABI record FACTORIES (`__rt_record_http_response_make`,
    /// `__rt_record_terminal_size_make`) which receive a machine-range
    /// `Int` field (HTTP status, terminal rows/cols) as i64 from the host
    /// and must lift it to the `$AverInt` carrier before `struct.new`.
    pub(super) aint_from_i64_fn_idx: Option<u32>,
    /// wasm fn idx of `__aint_to_i64_checked` (TRAPS on an out-of-i64 Big).
    /// `Some` iff `bignum`. Used by the `--handler` proxy synthesis to
    /// CHECKED-lower the user's `HttpResponse.status` `$AverInt` field to
    /// i64 before `set-status-code` — the VM rejects an out-of-range status
    /// (`HttpResponse.status is out of range`), so an out-of-range status
    /// TRAPS here rather than saturating into a wrong in-range code.
    pub(super) aint_to_i64_checked_fn_idx: Option<u32>,
    /// bignum size dedup — wasm fn indices of the shared sub-routines the
    /// arithmetic helpers `call` instead of inlining a private copy. Recorded
    /// by `module.rs` once `BuiltinRegistry::assign_slots` runs (the
    /// sub-routines are registered BEFORE the helpers that call them, so the
    /// indices are known when each helper's WAT is rendered). `Some` iff
    /// `bignum`. `None` falls each helper back to the inlined body.
    pub(super) aint_decompose_fn_idx: Option<u32>,
    pub(super) aint_normalize_fn_idx: Option<u32>,
    pub(super) aint_strip_fn_idx: Option<u32>,
    pub(super) aint_umag_cmp_fn_idx: Option<u32>,
}

#[derive(Debug, Clone)]
pub(super) struct VariantInfo {
    pub(super) parent: String,
    pub(super) type_idx: u32,
    pub(super) fields: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct MapSlots {
    /// `(array (mut K))` — keys array; element type derived from `K`.
    pub(super) keys_array: u32,
    /// `(array (mut V))` — values array; element type derived from `V`.
    pub(super) values_array: u32,
    /// `(struct (mut i32 size) (mut i32 cap) (mut keys_ref) (mut values_ref))`.
    pub(super) map: u32,
}

impl TypeRegistry {
    /// Build the registry with a `--handler` shape — pre-register
    /// HttpRequest/HttpResponse refs in case the handler fn is the
    /// only place they appear (otherwise the auto-discovery picks
    /// them up). Also intern the `"cf-ipcountry"` string literal so
    /// the synthesised `aver_http_handle` wrapper has a valid data
    /// segment to source it from.
    pub(super) fn build_with_handler(
        items: &[TopLevel],
        resolved_fn_defs: &[crate::ir::hir::ResolvedFnDef],
        _handler_active: bool,
    ) -> Self {
        // _handler_active is consumed by `items_reference_name`
        // overrides below so the rest of the builder stays
        // unchanged.
        let handler_active = _handler_active;
        let mut records = HashMap::new();
        let mut sum_roots = HashMap::new();
        let mut variants: HashMap<String, Vec<VariantInfo>> = HashMap::new();
        let mut record_fields = HashMap::new();
        let mut next_idx: u32 = 0;
        for item in items {
            match item {
                TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) => {
                    records.insert(name.clone(), next_idx);
                    record_fields.insert(name.clone(), fields.clone());
                    next_idx += 1;
                }
                TopLevel::TypeDef(TypeDef::Sum {
                    name, variants: vs, ..
                }) => {
                    // Reserve the nominal root before every constructor.
                    // `emit_user_types` preserves these indices inside one
                    // explicit rec group, so the supertype is declared before
                    // its subtypes while recursive sum payloads remain legal.
                    sum_roots.insert(name.clone(), next_idx);
                    next_idx += 1;
                    for v in vs {
                        variants
                            .entry(v.name.clone())
                            .or_default()
                            .push(VariantInfo {
                                parent: name.clone(),
                                type_idx: next_idx,
                                fields: v.fields.clone(),
                            });
                        next_idx += 1;
                    }
                }
                _ => {}
            }
        }
        // Built-in records (`HttpRequest`, `HttpResponse`,
        // `Tcp.Connection`, `Terminal.Size`) — populate `record_fields`
        // up front so List / Map field-walking discovery can pick up
        // `Map<String, List<String>>`, but defer slot assignment to the
        // end of `build` because their fields reference String / Map /
        // List which all sit at higher slots. Wasm-gc forward references
        // outside a rec group are illegal, so the struct-type emit has
        // to wait until after the dependencies.
        let mut builtin_record_names: Vec<String> = Vec::new();
        for record in crate::codegen::builtin_records::BUILTIN_RECORDS {
            // `--handler` mode forces HttpRequest + HttpResponse to be
            // registered even if no fn signature mentions them — the
            // synthesised `aver_http_handle` wrapper builds an
            // HttpRequest from host effects and reads HttpResponse
            // back, both of which are codegen-only references.
            //
            // Phase 4.5a similarly forces `Tcp.Connection` whenever
            // any `Tcp.*` effect is declared: even programs that only
            // call `Tcp.send` (and never name the record in source)
            // need its slot allocated because the orchestrator helper
            // threads a `Tcp.Connection` through internally.
            let force_handler = handler_active
                && (record.aver_name == "HttpRequest" || record.aver_name == "HttpResponse");
            let force_tcp = matches!(
                record.aver_name,
                "Tcp.Connection" | "Tcp.Dial" | "Tcp.Listener"
            ) && items.iter().any(|item| match item {
                TopLevel::FnDef(fd) => fd.effects.iter().any(|e| e.node.starts_with("Tcp.")),
                _ => false,
            });
            let force = force_handler || force_tcp;
            if !force && !items_reference_name(items, record.aver_name) {
                continue;
            }
            if record_fields.contains_key(record.aver_name) {
                continue;
            }
            let mut fields_v: Vec<(String, String)> = Vec::new();
            for f in record.fields {
                let aver_ty = builtin_type_to_aver_string(&f.ty);
                fields_v.push((f.name.to_string(), aver_ty));
            }
            record_fields.insert(record.aver_name.to_string(), fields_v);
            builtin_record_names.push(record.aver_name.to_string());
        }

        // Deterministic walk order for the carrier-type discovery below
        // (Option / Result / List / Vector / Map slots derived from
        // record fields). `record_fields` is a `HashMap`, so iterating
        // it directly registers those carrier slots in a process-random
        // order — which makes the emitted type section, and thus the
        // whole module, non-reproducible across builds. Sort the names
        // once and walk that fixed order at every record-field sweep
        // that feeds an ordered `*_order` / slot allocation.
        let mut record_names_sorted: Vec<String> = record_fields.keys().cloned().collect();
        record_names_sorted.sort();

        // Allocate the String type slot first (after records/variants)
        // so any `Vector<String>` registered below sits at a higher
        // index than `$string` and can reference it without crossing
        // the rec-group boundary.
        // Force the String type slot whenever the program has any
        // fn defs at all — `body/builtins.rs` now pushes the
        // current fn name as a String literal before every effect
        // import call (the `caller_fn` trailing arg). Without the
        // slot, `emit_string_literal_bytes` can't materialise the
        // ref and validation fails for trivially-Stringless programs
        // like `fn main() -> Int { _ = Time.unixMs(); 42 }`.
        let has_fn_defs = items.iter().any(|item| matches!(item, TopLevel::FnDef(_)));
        let needs_string = has_fn_defs
            || resolved_fn_defs.iter().any(|fd| {
                fd.return_type.display().contains("String")
                    || fd
                        .params
                        .iter()
                        .any(|(_, t)| t.display().contains("String"))
                    || fn_body_produces_string(fd)
            });
        let string_array_type_idx = if needs_string {
            let idx = next_idx;
            next_idx += 1;
            Some(idx)
        } else {
            None
        };

        let needs_string_index = resolved_fn_defs.iter().any(|fd| {
            fd.return_type.display().trim() == "String.Index"
                || fd
                    .params
                    .iter()
                    .any(|(_, ty)| ty.display().trim() == "String.Index")
        });
        let string_index_array_type_idx = if needs_string_index {
            let idx = next_idx;
            next_idx += 1;
            Some(idx)
        } else {
            None
        };

        let needs_crypto_sha256 = resolved_fn_defs
            .iter()
            .any(|fd| fn_body_calls_builtin(fd, "Crypto.sha256"));
        let (crypto_byte_array_type_idx, crypto_word_array_type_idx) = if needs_crypto_sha256 {
            let byte_idx = next_idx;
            next_idx += 1;
            let word_idx = next_idx;
            next_idx += 1;
            (Some(byte_idx), Some(word_idx))
        } else {
            (None, None)
        };

        // Phase 4 (0.20) — TCP connection pool type slots. `$tcp_slot`
        // is a 4-field struct (socket / in_stream / out_stream / in_use
        // handles, all i32), and `$tcp_pool` is the `(array (mut $tcp_slot))`
        // that holds 256 of them. Allocated whenever a fn declares any
        // `Tcp.*` effect; effect-target wiring (whether to actually emit
        // the connection pipeline) lives in module.rs. Both slots land
        // adjacent so the array type can reference the slot type without
        // crossing a rec-group boundary.
        let needs_tcp = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => fd.effects.iter().any(|e| e.node.starts_with("Tcp.")),
            _ => false,
        });
        let (tcp_slot_type_idx, tcp_pool_type_idx) = if needs_tcp {
            let slot_idx = next_idx;
            next_idx += 1;
            let pool_idx = next_idx;
            next_idx += 1;
            (Some(slot_idx), Some(pool_idx))
        } else {
            (None, None)
        };

        // `$AverInt` + `(array i64)` magnitude slots. `Int = ℤ` is now
        // the only wasm-gc semantics (no flag), so this is governed purely
        // by the "any Int arithmetic reachable" REACHABILITY gate: it is a
        // SIZE optimization, not a semantics switch. Pure-String/Float/
        // effect-only programs (and the 245B hello-worker) carry ZERO
        // bignum bytes. The magnitude array slot is allocated first so the
        // struct can reference it without a forward edge (a single rec
        // group makes this safe, but keeping the array lower mirrors the
        // other collection slots and reads cleaner).
        let bignum = resolved_fn_defs.iter().any(fn_uses_int_arithmetic);
        let (aint_mag_array_idx, aint_struct_idx) = if bignum {
            let mag_idx = next_idx;
            next_idx += 1;
            let struct_idx = next_idx;
            next_idx += 1;
            (Some(mag_idx), Some(struct_idx))
        } else {
            (None, None)
        };

        // Discover monomorphized `Vector<T>` instantiations. Walk fn
        // signatures (params + return types) and binding annotations;
        // each unique `Vector<T>` gets its own `(array (mut T))` slot.
        // Inferred Vectors (from `Vector.new` whose annotation is
        // implicit) still surface here when the surrounding param /
        // return type spells out the element type, which is the
        // canonical bench shape today.
        let mut vector_types: HashMap<String, u32> = HashMap::new();
        let mut vector_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_vectors_from_str(
                &fd.return_type.display(),
                &mut vector_types,
                &mut vector_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_vectors_from_str(
                    &ty.display(),
                    &mut vector_types,
                    &mut vector_order,
                    &mut next_idx,
                );
            }
            collect_vectors_from_fn_body(fd, &mut vector_types, &mut vector_order, &mut next_idx);
        }
        // Record field walks — `record { nums: Vector<Int> }` only
        // shows up in the record's field list, not in any fn
        // signature. Mirror the lists / options record-field walk.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_vectors_from_str(ty, &mut vector_types, &mut vector_order, &mut next_idx);
            }
        }

        // `Result<T, E>` and `List<T>` instantiations land BEFORE
        // options/maps so that `Option<List<String>>` /
        // `Map<String, Result<...>>` can reference them by an
        // already-assigned lower idx. Without this reordering the
        // option struct's value-field forward-references the
        // post-options list slot, which wasm-gc rejects outside a
        // rec group.
        let mut result_types: HashMap<String, u32> = HashMap::new();
        let mut result_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_results_from_str(
                &fd.return_type.display(),
                &mut result_types,
                &mut result_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_results_from_str(
                    &ty.display(),
                    &mut result_types,
                    &mut result_order,
                    &mut next_idx,
                );
            }
            collect_results_from_builtin_uses(
                fd,
                &mut result_types,
                &mut result_order,
                &mut next_idx,
            );
            // Walk binding annotations too — `let r: Result<Box, MyErr> = …`
            // wouldn't be picked up by the builtin-uses scan (which only
            // looks at known dotted calls like `Disk.readText`). Without this,
            // user-typed `Result<custom, custom>` values fail to find their
            // type slot at construction time.
            use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
            let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
            for stmt in stmts {
                if let ResolvedStmt::Binding {
                    ty_ann: Some(annot),
                    ..
                } = stmt
                {
                    collect_results_from_str(
                        &annot.display(),
                        &mut result_types,
                        &mut result_order,
                        &mut next_idx,
                    );
                }
            }
        }
        let mut list_types: HashMap<String, u32> = HashMap::new();
        let mut list_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_lists_from_str(
                &fd.return_type.display(),
                &mut list_types,
                &mut list_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_lists_from_str(
                    &ty.display(),
                    &mut list_types,
                    &mut list_order,
                    &mut next_idx,
                );
            }
            // Body annotations — `nested: List<List<Int>> = [a, b]`
            // adds `List<List<Int>>` even when no fn signature
            // mentions it. Mirrors the same body-walk options
            // and vectors already do.
            collect_lists_from_fn_body(fd, &mut list_types, &mut list_order, &mut next_idx);
        }
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_lists_from_str(ty, &mut list_types, &mut list_order, &mut next_idx);
            }
        }
        if handler_active && !list_types.contains_key("List<String>") {
            list_types.insert("List<String>".to_string(), next_idx);
            list_order.push("List<String>".to_string());
            next_idx += 1;
        }
        // `Disk.listDir` host import returns `Result<List<String>, String>` —
        // ensure the underlying `List<String>` slot exists even when no
        // user-fn signature mentions it.
        let needs_list_string_for_disk = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => fd.effects.iter().any(|e| e.node == "Disk.listDir"),
            _ => false,
        });
        if needs_list_string_for_disk && !list_types.contains_key("List<String>") {
            list_types.insert("List<String>".to_string(), next_idx);
            list_order.push("List<String>".to_string());
            next_idx += 1;
        }
        // Byte-clean TCP effects consume or return nominal `Bytes`. Its
        // private `values` carrier is still `List<Int>`, so the
        // concrete list slot is required even when no list literal is
        // present at the call site.
        let needs_list_int_for_tcp_bytes = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => fd.effects.iter().any(|e| {
                matches!(
                    e.node.as_str(),
                    "Tcp.sendBytes"
                        | "Tcp.readBytes"
                        | "Tcp.readSome"
                        | "Tcp.writeBytes"
                        | "Tcp.poll"
                )
            }),
            _ => false,
        });
        if needs_list_int_for_tcp_bytes && !list_types.contains_key("List<Int>") {
            list_types.insert("List<Int>".to_string(), next_idx);
            list_order.push("List<Int>".to_string());
            next_idx += 1;
        }

        // Eager `Vector<T>` registration for every discovered
        // `List<T>` — `Vector.fromList(list_call())` is the canonical
        // way wumpus-shaped programs build a Vector, but the
        // expression-walker can't infer the produced Vector<T>
        // without typing the inner list call. Cheap overproduction
        // here (a slot per List<T>) is harmless; `wasm-opt -Oz`
        // strips the unused vector helpers, and the `Vector.fromList`
        // path can finally find its pre-allocated slot.
        for list_canonical in list_order.iter() {
            if let Some(elem) = TypeRegistry::list_element_type(list_canonical) {
                let vec_canonical = format!("Vector<{}>", elem.trim());
                if !vector_types.contains_key(&vec_canonical) {
                    vector_types.insert(vec_canonical.clone(), next_idx);
                    vector_order.push(vec_canonical);
                    next_idx += 1;
                }
            }
        }

        // `Option<T>` instantiations follow the same shape — scan
        // signatures + bodies for any `Option<T>` reference and
        // allocate a struct slot per unique `T`.
        let mut option_types: HashMap<String, u32> = HashMap::new();
        let mut option_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_options_from_str(
                &fd.return_type.display(),
                &mut option_types,
                &mut option_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_options_from_str(
                    &ty.display(),
                    &mut option_types,
                    &mut option_order,
                    &mut next_idx,
                );
            }
            collect_options_from_fn_body(fd, &mut option_types, &mut option_order, &mut next_idx);
        }
        // The String-index pass is intentionally below typechecking, so its
        // freshly synthesized intrinsic node has no source type stamp for the
        // generic body collector to read. Its result contract is nevertheless
        // fixed: indexed charAt always needs `Option<String>`.
        let needs_indexed_char_at = resolved_fn_defs.iter().any(|fd| {
            fn_body_reaches(fd, &|callee| {
                matches!(
                    callee,
                    crate::ir::hir::ResolvedCallee::Intrinsic(
                        crate::ir::hir::BuiltinIntrinsic::StrIndexCharAt
                    )
                )
            })
        });
        if needs_indexed_char_at && !option_types.contains_key("Option<String>") {
            option_types.insert("Option<String>".to_string(), next_idx);
            option_order.push("Option<String>".to_string());
            next_idx += 1;
        }
        let needs_option_tcp_connection = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => fd
                .effects
                .iter()
                .any(|effect| matches!(effect.node.as_str(), "Tcp.dialled" | "Tcp.accept")),
            _ => false,
        });
        if needs_option_tcp_connection && !option_types.contains_key("Option<Tcp.Connection>") {
            option_types.insert("Option<Tcp.Connection>".to_string(), next_idx);
            option_order.push("Option<Tcp.Connection>".to_string());
            next_idx += 1;
        }
        // Record field walk — `record GameState { lastAiResult:
        // Option<AiResult> }` only spells `Option<AiResult>` in the
        // record declaration. Without this walk the canonical never
        // gets registered, and a `match state.lastAiResult` arm
        // dispatcher fails to recover its slot.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_options_from_str(ty, &mut option_types, &mut option_order, &mut next_idx);
            }
        }
        // Eagerly register `Option<T>` for every `Vector<T>` — a
        // `match Vector.get(v, i) { Option.Some(x) -> ...; Option.None -> ... }`
        // requires the boxed Option<T> slot, but the surface code
        // doesn't spell out `Option<String>` in any signature.
        for vec_canonical in &vector_order {
            if let Some(elem) = TypeRegistry::vector_element_type(vec_canonical) {
                let opt = format!("Option<{}>", elem.trim());
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
        }
        // Same eager registration for every user-defined record. The
        // common shape is `fn handleAiTurn(state: GameState) -> GameState`
        // whose body emits a fallback `Option.None` (e.g. inside a
        // `RecordCreate` field that's actually `Option<X>` but the
        // emit-time hint falls back to `ctx.return_type` = `GameState`).
        // Without `Option<GameState>` in the registry the constructor
        // crashes; with it the slot exists and `wasm-opt -Oz` strips
        // unused option helpers if nothing actually instantiates them.
        for record_name in &record_names_sorted {
            let opt = format!("Option<{record_name}>");
            if !option_types.contains_key(&opt) {
                option_types.insert(opt.clone(), next_idx);
                option_order.push(opt);
                next_idx += 1;
            }
        }
        // Eagerly register `Option<V>` for every `Map<K, V>` reachable
        // anywhere — `Map.get` returns `Option<V>` and the slot has
        // to land before the Map struct does so the wasm type section
        // can reference it without a forward edge. Pre-discover the
        // pending maps the same way the actual Map block does, then
        // grab each V.
        let mut pending_maps_for_options: Vec<String> = Vec::new();
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_maps_from_str(ty, &mut pending_maps_for_options);
            }
        }
        for fd in resolved_fn_defs {
            collect_maps_from_str(&fd.return_type.display(), &mut pending_maps_for_options);
            for (_, ty) in &fd.params {
                collect_maps_from_str(&ty.display(), &mut pending_maps_for_options);
            }
        }
        if handler_active
            && !pending_maps_for_options
                .iter()
                .any(|m| m == "Map<String,List<String>>")
        {
            pending_maps_for_options.push("Map<String,List<String>>".to_string());
        }
        let mut seen_map_v: std::collections::HashSet<String> = std::collections::HashSet::new();
        for canonical in &pending_maps_for_options {
            if let Some((_, v)) = parse_map_kv(canonical)
                && seen_map_v.insert(v.to_string())
            {
                let opt = format!("Option<{v}>");
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
        }

        // `Map<K, V>` discovery — same monomorphisation strategy as
        // Vector / Option. Walk fn signatures + bodies for any
        // `Map<K, V>` reference, allocate three wasm slots per unique
        // instantiation (keys array, values array, map struct), and
        // eagerly register the matching `Option<V>` since `Map.get`
        // returns it.
        let mut map_types: HashMap<String, MapSlots> = HashMap::new();
        let mut map_order: Vec<String> = Vec::new();
        let mut pending_maps: Vec<String> = Vec::new();
        // Built-in record fields contribute too — `HttpRequest.headers`
        // / `HttpResponse.headers` carry `Map<String, List<String>>`.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_maps_from_str(ty, &mut pending_maps);
            }
        }
        for fd in resolved_fn_defs {
            collect_maps_from_str(&fd.return_type.display(), &mut pending_maps);
            for (_, ty) in &fd.params {
                collect_maps_from_str(&ty.display(), &mut pending_maps);
            }
            // Walk let-binding annotations too — `let m: Map<Person, Int> =
            // Map.set(…)` won't show up in fn signatures and the discovery
            // walker would otherwise miss the canonical entirely. Mirrors
            // what the Result walker added a few commits back.
            use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
            let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
            for stmt in stmts {
                if let ResolvedStmt::Binding {
                    ty_ann: Some(annot),
                    ..
                } = stmt
                {
                    collect_maps_from_str(&annot.display(), &mut pending_maps);
                }
                let expr = match stmt {
                    ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => e,
                };
                collect_maps_from_expr(expr, &mut pending_maps);
            }
        }
        // Dedup in encounter order.
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        for canonical in pending_maps {
            if !seen.insert(canonical.clone()) {
                continue;
            }
            // Eagerly register Option<V> — `Map.get` over this
            // instantiation returns it.
            if let Some((_, v)) = parse_map_kv(&canonical) {
                let opt = format!("Option<{v}>");
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
            // Allocate three slots: keys_array, values_array, map.
            // Order: arrays first so the struct (higher idx) can
            // reference them without crossing rec-group boundaries.
            let keys_array = next_idx;
            next_idx += 1;
            let values_array = next_idx;
            next_idx += 1;
            let map = next_idx;
            next_idx += 1;
            map_types.insert(
                canonical.clone(),
                MapSlots {
                    keys_array,
                    values_array,
                    map,
                },
            );
            map_order.push(canonical);
        }

        let map_order_indices_type_idx = if map_order.is_empty() {
            None
        } else {
            let idx = next_idx;
            next_idx += 1;
            Some(idx)
        };

        // Eagerly register a key box whenever raw K cannot share the table's
        // ref-null occupancy marker. Primitives have no null value; List has
        // the opposite collision because its valid empty value is null.
        let mut primitive_key_box: HashMap<String, u32> = HashMap::new();
        let mut primitive_key_box_order: Vec<String> = Vec::new();
        for canonical in map_order.iter() {
            if let Some((k, _)) = parse_map_kv(canonical) {
                let k_trim = normalize_compound(k);
                if (matches!(k_trim.as_str(), "Int" | "Float" | "Bool")
                    || k_trim.starts_with("List<"))
                    && !primitive_key_box.contains_key(&k_trim)
                {
                    primitive_key_box.insert(k_trim.clone(), next_idx);
                    primitive_key_box_order.push(k_trim);
                    next_idx += 1;
                }
            }
        }

        // Eagerly register `List<K>` and `List<V>` for every
        // `Map<K, V>` — `Map.keys` / `Map.values` return them but
        // the canonical never appears anywhere else. Same trick as
        // the eager `Option<V>` above. Single rec group makes
        // forward refs from existing types to these new lists OK.
        for canonical in map_order.iter() {
            if let Some((k, v)) = parse_map_kv(canonical) {
                for elem in [k.trim(), v.trim()] {
                    let lst = format!("List<{elem}>");
                    if !list_types.contains_key(&lst) {
                        list_types.insert(lst.clone(), next_idx);
                        list_order.push(lst);
                        next_idx += 1;
                    }
                }
            }
        }

        // `Tuple<A, B>` discovery — fn signatures + record fields +
        // body annotations. Discovery walks the same shape as
        // results/options.
        let mut tuple_types: HashMap<String, u32> = HashMap::new();
        let mut tuple_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_tuples_from_str(
                &fd.return_type.display(),
                &mut tuple_types,
                &mut tuple_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_tuples_from_str(
                    &ty.display(),
                    &mut tuple_types,
                    &mut tuple_order,
                    &mut next_idx,
                );
            }
            // Walk the body for `ResolvedExpr::Tuple` literals — the
            // canonical `Tuple<A,B>` for a `(a, b)` literal is
            // built from the items' typed-AST element types and
            // never has to appear in any signature.
            collect_tuples_from_fn_body(fd, &mut tuple_types, &mut tuple_order, &mut next_idx);
        }
        // Record fields can carry tuple types too.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_tuples_from_str(ty, &mut tuple_types, &mut tuple_order, &mut next_idx);
            }
        }
        // Eagerly register `Tuple<K, V>` for every `Map<K, V>` —
        // `Map.entries` returns `List<Tuple<K, V>>` and `Map.fromList`
        // takes one. Plus the matching `List<Tuple<K, V>>`.
        for canonical in map_order.iter() {
            if let Some((k, v)) = parse_map_kv(canonical) {
                let tup = format!("Tuple<{},{}>", k.trim(), v.trim());
                if !tuple_types.contains_key(&tup) {
                    tuple_types.insert(tup.clone(), next_idx);
                    tuple_order.push(tup.clone());
                    next_idx += 1;
                }
                let lst = format!("List<{tup}>");
                if !list_types.contains_key(&lst) {
                    list_types.insert(lst.clone(), next_idx);
                    list_order.push(lst);
                    next_idx += 1;
                }
            }
        }
        // Eagerly register `List<Tuple<A, B>>` for every `Tuple<A, B>`
        // — `List.zip` returns it and `Map.fromList` takes it. Some
        // tuples come straight from fn signatures; this catches both.
        for canonical in tuple_order.clone().iter() {
            let lst = format!("List<{canonical}>");
            if !list_types.contains_key(&lst) {
                list_types.insert(lst.clone(), next_idx);
                list_order.push(lst);
                next_idx += 1;
            }
        }

        // Eagerly register `List<T>` and `Option<Vector<T>>` for
        // every `Vector<T>` — `List.fromVector` returns the list and
        // `Vector.set` (boxed shape) returns the option. Both
        // canonical-types only appear in those builtin returns.
        for canonical in vector_order.iter() {
            if let Some(elem) = TypeRegistry::vector_element_type(canonical) {
                let elem = elem.trim();
                let lst = format!("List<{elem}>");
                if !list_types.contains_key(&lst) {
                    list_types.insert(lst.clone(), next_idx);
                    list_order.push(lst);
                    next_idx += 1;
                }
                let opt = format!("Option<{canonical}>");
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
        }

        // Now that String / List / Map slots all exist, slot-assign
        // the built-in records — they reference those types in their
        // fields, so the struct-type emit needs them at lower indices.
        for name in &builtin_record_names {
            records.insert(name.clone(), next_idx);
            next_idx += 1;
        }

        // Discover unique String literals — each gets a passive data
        // segment idx assigned in encounter order. Walk fn bodies + any
        // string literals embedded in expressions; canonicalise on
        // raw byte content (Aver strings are UTF-8).
        let mut string_literals: Vec<Vec<u8>> = Vec::new();
        let mut string_literal_idx: HashMap<Vec<u8>, u32> = HashMap::new();
        let _ = handler_active;
        for fd in resolved_fn_defs {
            collect_string_literals_in_fn(fd, &mut string_literals, &mut string_literal_idx);
        }
        // Note: caller_fn name registration moved out of `TypeRegistry`
        // — `body::CallerFnCollector` lazy-registers names during
        // codegen (every site that calls `emit_caller_fn_idx` registers
        // its self_fn_name on demand), and the post-emit phase appends
        // the resulting names as fresh passive segments after the
        // pre-walked literal segments above. Single source of truth,
        // zero AST-walker false positives.
        // Boxed `Int.mod` / `Int.div` build a `Result<Int, String>` whose
        // `Err` arm carries a fixed message verbatim from the VM
        // (`src/types/int.rs`): `"division by zero"` for both, plus
        // `"division overflow"` for `Int.div`'s `i64::MIN / -1` edge.
        // Register those bytes as passive segments so the boxed emitter
        // (`from_mir::emit_mir_int_div_mod_boxed`) has a data-segment idx to
        // pull at `struct.new` time, whether or not user source ever spells
        // the literal. (The fused `Result.withDefault(Int.{mod,div}(...),
        // default)` path never materialises a struct, so it doesn't need
        // these.)
        let int_mod_used = resolved_fn_defs.iter().any(fn_body_calls_int_mod);
        let int_div_used = resolved_fn_defs.iter().any(fn_body_calls_int_div);
        let mut intern_synthetic = |bytes: Vec<u8>| {
            string_literal_idx.entry(bytes.clone()).or_insert_with(|| {
                let idx = string_literals.len() as u32;
                string_literals.push(bytes);
                idx
            });
        };
        if int_mod_used || int_div_used {
            intern_synthetic(b"division by zero".to_vec());
        }
        if int_div_used {
            intern_synthetic(b"division overflow".to_vec());
        }
        // Same for the count-taking `Bits` operations: their `Err` arm
        // carries a fixed message verbatim from the VM (`src/types/bits.rs`).
        // Negative and oversized counts are the catchable failures, so these
        // four strings are the whole error surface of the namespace.
        if resolved_fn_defs.iter().any(|fd| {
            fn_body_calls_builtin(fd, "Bits.shiftLeft")
                || fn_body_calls_builtin(fd, "Bits.shiftRight")
        }) {
            intern_synthetic(b"negative shift count".to_vec());
            intern_synthetic(aver_rt::shift_count_too_large_message().into_bytes());
        }
        if resolved_fn_defs
            .iter()
            .any(|fd| fn_body_calls_builtin(fd, "Bits.low"))
        {
            intern_synthetic(b"negative bit width".to_vec());
            intern_synthetic(aver_rt::bit_width_too_large_message().into_bytes());
        }
        if resolved_fn_defs
            .iter()
            .any(|fd| fn_body_calls_builtin(fd, "Vector.new"))
        {
            intern_synthetic(aver_rt::vector_size_error_message().into_bytes());
        }
        if resolved_fn_defs
            .iter()
            .any(|fd| fn_body_calls_builtin(fd, "Random.int"))
        {
            intern_synthetic(b"Random.int: bounds must fit a 64-bit integer".to_vec());
            intern_synthetic(b"Random.int: min must be <= max".to_vec());
        }
        if resolved_fn_defs
            .iter()
            .any(|fd| fn_body_calls_builtin(fd, "Time.sleep"))
        {
            intern_synthetic(b"Time.sleep: ms must fit a 64-bit integer".to_vec());
            intern_synthetic(b"Time.sleep: ms must be non-negative".to_vec());
        }

        // Phase 4.2.1 (0.20) — register the placeholder error
        // message the `__rt_tcp_connect` stub returns until the real
        // DNS/connect/finish pipeline lands. Gated on `needs_tcp`
        // (any fn declares a Tcp.* effect) so non-TCP programs don't
        // carry the literal.
        if needs_tcp {
            for msg in [
                b"tcp: connect not yet implemented".as_ref(),
                b"tcp: dns resolve failed".as_ref(),
                b"tcp: dns no addresses".as_ref(),
                b"tcp: socket create failed".as_ref(),
                b"tcp: connect failed".as_ref(),
                b"tcp: write failed".as_ref(),
                b"tcp: eof".as_ref(),
                // Phase 4.7+ — aver-rt cross-backend alignment.
                // VM / self-host / wasm-gc all return `Err("Tcp.X:
                // unknown connection 'tcp-N'")` on stale handles;
                // wasip2 matches the prefix shape (we drop the
                // method name since one segment serves close /
                // writeLine / readLine).
                b"tcp: unknown connection".as_ref(),
                b"Tcp.writeBytes: malformed Bytes carrier".as_ref(),
                b"Tcp.readBytes: count is negative".as_ref(),
                b"Tcp.readBytes: count exceeds the 10485760 byte limit".as_ref(),
                // Out-of-i64 counts get their own message, mirroring
                // the VM's `count_arg` classification: i64-fit is
                // checked before sign, so an out-of-i64 count of
                // either sign reports "exceeds the read limit",
                // never "is negative" or the 10485760 text.
                b"Tcp.readBytes: count exceeds the read limit".as_ref(),
                b"failed to fill whole buffer".as_ref(),
                b"Tcp.readSome: maxBytes must be positive".as_ref(),
                b"Tcp.readSome: maxBytes exceeds the 10485760 byte limit".as_ref(),
                b"Tcp.readSome: maxBytes exceeds the read limit".as_ref(),
                b"Tcp.poll: timeoutMs is negative".as_ref(),
                b"Tcp.poll: timeoutMs exceeds the poll limit".as_ref(),
                b"Tcp.poll: wasip2 supports only Tcp.Socket.Connected values".as_ref(),
                b"tcp: read failed".as_ref(),
                // Phase 4.7+ — port validation. VM message verbatim
                // (`Tcp: port N is out of range (0\u{2013}65535)`)
                // is parameterised on the port value; we ship a
                // canned message instead because the Err string is
                // built from a static data segment, not concat.
                b"tcp: port out of range".as_ref(),
                // Phase 4.7+ fix #10 — pool-limit alignment with
                // `aver-rt::tcp::connect`. Pool is 256 slots; the
                // 257th live connect must refuse rather than evict
                // the slot's existing live occupant.
                b"tcp: connection limit reached (256 max)".as_ref(),
                // Phase 4.7+ fix #17 — `Tcp.send` `stream-error.
                // last-operation-failed`. The wasi:io read variant
                // distinguishes I/O errors from a clean half-close;
                // wasip2 used to fold both into a partial-Ok return,
                // mismatching `aver-rt::tcp::send`'s explicit Err.
                b"tcp: stream error".as_ref(),
                // Phase 4.7+ fix #18 — `Tcp.send` response cap.
                // `aver-rt::tcp::send` caps at 10 MiB; wasip2 used
                // to grow the buffer unbounded.
                b"tcp: response exceeds 10 MiB limit".as_ref(),
                // #1131 Level B on wasip2: asynchronous dial/listen resource
                // operations remain callable and fail as typed Results until
                // the backend grows resource-pool support for them.
                b"Tcp.beginConnect: native sockets are unavailable on this target".as_ref(),
                b"Tcp.dialled: native sockets are unavailable on this target".as_ref(),
                b"Tcp.listen: native socket listening is unavailable on this target".as_ref(),
                b"Tcp.accept: native socket listening is unavailable on this target".as_ref(),
                b"Tcp.peerAddress: native sockets are unavailable on this target".as_ref(),
                b"Tcp.closeDial: native sockets are unavailable on this target".as_ref(),
                b"Tcp.closeListener: native sockets are unavailable on this target".as_ref(),
            ] {
                let bytes = msg.to_vec();
                string_literal_idx.entry(bytes.clone()).or_insert_with(|| {
                    let idx = string_literals.len() as u32;
                    string_literals.push(bytes);
                    idx
                });
            }
        }

        // Mark every record/variant used as a `Map<K, *>` key as
        // non-newtypable so it stays a struct ref in the type
        // section — the open-addressing layout's `keys[i] == null`
        // empty marker requires that.
        let mut non_newtypable_keys: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for canonical in map_order.iter() {
            if let Some((k, _)) = parse_map_kv(canonical) {
                let k_trim = k.trim();
                if record_fields.contains_key(k_trim)
                    || variants
                        .values()
                        .flat_map(|v| v.iter())
                        .any(|v| v.parent == k_trim)
                {
                    non_newtypable_keys.insert(k_trim.to_string());
                }
            }
        }

        Self {
            records,
            sum_roots,
            variants,
            record_fields,
            vector_types,
            vector_order,
            option_types,
            option_order,
            list_types,
            list_order,
            result_types,
            result_order,
            map_types,
            map_order,
            map_order_indices_type_idx,
            tuple_types,
            tuple_order,
            primitive_key_box,
            primitive_key_box_order,
            packed_sequences: HashMap::new(),
            packed_sequence_order: Vec::new(),
            // Populated post-build by `module.rs` from the aliases
            // `flatten_multimodule` derived; empty for single-module
            // programs and for callers that did not flatten.
            type_name_aliases: HashMap::new(),
            user_type_count: next_idx,
            string_array_type_idx,
            string_index_array_type_idx,
            crypto_byte_array_type_idx,
            crypto_word_array_type_idx,
            string_literals,
            string_literal_idx,
            non_newtypable_keys,
            // ETAP-2 carrier-`i64`: populated post-build by `module.rs`
            // (`set_eligible_carriers`) from the refinement-via-opaque
            // carrier table, which needs `ProofLowerInputs` this builder
            // doesn't have. Empty here ⇒ no carrier erases to `i64` until
            // the codegen entry opts in.
            eligible_carriers: std::collections::HashSet::new(),
            // ETAP-2 multi-field carrier-`i64`: populated post-build by
            // `module.rs` (`set_eligible_carrier_fields`) from the multi-field
            // carrier table. Empty here ⇒ no record field erases to `i64` until
            // the codegen entry opts in.
            eligible_carrier_fields: std::collections::HashSet::new(),
            tcp_slot_type_idx,
            tcp_pool_type_idx,
            bignum,
            aint_struct_idx,
            aint_mag_array_idx,
            // Set by `module.rs` after `BuiltinRegistry::assign_slots`,
            // once the `__aint_eq` / `__aint_hash` / `__aint_from_i64`
            // fn indices are known.
            aint_eq_fn_idx: None,
            aint_cmp_fn_idx: None,
            aint_hash_fn_idx: None,
            aint_from_i64_fn_idx: None,
            aint_to_i64_checked_fn_idx: None,
            aint_decompose_fn_idx: None,
            aint_normalize_fn_idx: None,
            aint_strip_fn_idx: None,
            aint_umag_cmp_fn_idx: None,
        }
    }

    pub(super) fn list_type_idx(&self, canonical: &str) -> Option<u32> {
        let normalized = normalize_compound(canonical);
        if let Some(idx) = self.list_types.get(&normalized).copied() {
            return Some(idx);
        }
        // Cross-module: a fn signature may spell a record as
        // `Module.Room` while the dep's source uses bare `Room`. The
        // type registry only has one slot per record (keyed by the
        // bare name), so strip dotted prefixes from inner type names
        // and retry.
        let bare = strip_inner_dotted_prefixes(&normalized);
        if bare != normalized {
            self.list_types.get(&bare).copied()
        } else {
            None
        }
    }

    pub(super) fn list_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("List<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    pub(super) fn result_type_idx(&self, canonical: &str) -> Option<u32> {
        if let Some(idx) = self.result_types.get(canonical).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.result_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Split `Result<T, E>` into (T, E) borrowed slices.
    pub(super) fn result_te(canonical: &str) -> Option<(&str, &str)> {
        let inner = canonical
            .trim()
            .strip_prefix("Result<")?
            .strip_suffix('>')?;
        let bytes = inner.as_bytes();
        // Track both angle-bracket and paren depth — `Result<(A, B), E>`
        // (T = a tuple) has a top-level comma inside the parens we
        // need to skip past.
        let mut depth: i32 = 0;
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'<' | b'(' => depth += 1,
                b'>' | b')' => depth -= 1,
                b',' if depth == 0 => {
                    return Some((inner[..idx].trim(), inner[idx + 1..].trim()));
                }
                _ => {}
            }
        }
        None
    }

    pub(super) fn map_slots(&self, canonical: &str) -> Option<MapSlots> {
        if let Some(s) = self.map_types.get(canonical).copied() {
            return Some(s);
        }
        let bare = strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.map_types.get(&bare).copied()
        } else {
            None
        }
    }

    pub(super) fn primitive_key_box_idx(&self, k_aver: &str) -> Option<u32> {
        self.primitive_key_box
            .get(&normalize_compound(k_aver))
            .copied()
    }

    /// True for scalar K kinds whose map hash/eq helpers operate on raw wasm
    /// values. They are boxed for storage, but List has a storage box too and
    /// deliberately is not a primitive for helper dispatch.
    pub(super) fn is_primitive_map_key(k_aver: &str) -> bool {
        matches!(k_aver.trim(), "Int" | "Float" | "Bool")
    }

    pub(super) fn tuple_type_idx(&self, canonical: &str) -> Option<u32> {
        // Accept both `Tuple<A,B>` (internal canonical) and `(A, B)`
        // (Aver surface syntax). Normalize the latter, strip
        // whitespace, then look up.
        let normalized = normalize_tuple_canonical(canonical);
        if let Some(idx) = self.tuple_types.get(normalized.as_ref()).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(normalized.as_ref());
        if bare.as_str() != normalized.as_ref() {
            self.tuple_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Split `Tuple<A, B, C, ...>` (or `(A, B, C, ...)`) into the full
    /// element list. Depth-aware comma scan handles nested generics
    /// like `Tuple<Map<String, Int>, List<Int>>`. Returns slices of
    /// the original `canonical` so callers don't allocate; arity is
    /// arbitrary (no hardcoded 2-cap).
    pub(super) fn tuple_elements(canonical: &str) -> Option<Vec<&str>> {
        let trimmed = canonical.trim();
        let inner = if let Some(i) = trimmed
            .strip_prefix("Tuple<")
            .and_then(|s| s.strip_suffix('>'))
        {
            i
        } else if trimmed.starts_with('(') && trimmed.ends_with(')') {
            &trimmed[1..trimmed.len() - 1]
        } else {
            return None;
        };
        let bytes = inner.as_bytes();
        let mut depth: i32 = 0;
        let mut start = 0usize;
        let mut out: Vec<&str> = Vec::new();
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'(' | b'<' => depth += 1,
                b')' | b'>' => depth -= 1,
                b',' if depth == 0 => {
                    out.push(inner[start..idx].trim());
                    start = idx + 1;
                }
                _ => {}
            }
        }
        out.push(inner[start..].trim());
        if out.len() < 2 { None } else { Some(out) }
    }

    /// Convenience wrapper returning the first two elements as a pair —
    /// kept for callers that genuinely only need `(A, B)` (e.g.
    /// `List.zip` lowering, the existing 2-tuple `Map.entries`
    /// canonicals). For variadic destructure use `tuple_elements`.
    pub(super) fn tuple_ab(canonical: &str) -> Option<(&str, &str)> {
        let elems = Self::tuple_elements(canonical)?;
        if elems.len() == 2 {
            Some((elems[0], elems[1]))
        } else {
            None
        }
    }

    pub(super) fn option_type_idx(&self, canonical: &str) -> Option<u32> {
        // Registry keys are whitespace-free (the collectors strip),
        // but emit-time lookups arrive from `Type::display`, which
        // separates type args with `", "` — `Option<Tuple<Int, Int>>`
        // must find the `Option<Tuple<Int,Int>>` slot. Same
        // normalisation `list_type_idx` applies.
        let normalized = normalize_compound(canonical);
        if let Some(idx) = self.option_types.get(&normalized).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(&normalized);
        if bare != normalized {
            self.option_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Element-type Aver string for a registered `Option<T>` (analog
    /// to `vector_element_type`).
    pub(super) fn option_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("Option<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    /// Passive-data-segment idx for a String literal, allocated during
    /// `build`. Each unique byte sequence gets one segment.
    pub(super) fn string_literal_segment(&self, bytes: &[u8]) -> Option<u32> {
        self.string_literal_idx.get(bytes).copied()
    }

    /// Wasm type idx for a canonical Aver `Vector<T>` string, if the
    /// instantiation was registered during `build`.
    pub(super) fn vector_type_idx(&self, canonical: &str) -> Option<u32> {
        if let Some(idx) = self.vector_types.get(canonical).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.vector_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Element-type Aver string for a registered `Vector<T>`. Used by
    /// module emit to resolve the wasm storage type of array elements.
    pub(super) fn vector_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("Vector<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    pub(super) fn record_type_idx(&self, name: &str) -> Option<u32> {
        // Direct lookup. After multi-module flatten the discovery
        // walker registers records under either the bare name (`Room`)
        // or the qualified name (`Level.Room`); the type checker
        // sometimes hands us the qualified form even when the
        // registry only knows the bare one (or vice versa). Try both.
        if let Some(idx) = self.records.get(name).copied() {
            return Some(idx);
        }
        if let Some(bare) = name.rsplit_once('.').map(|(_, b)| b)
            && let Some(idx) = self.records.get(bare).copied()
        {
            return Some(idx);
        }
        None
    }

    pub(super) fn sum_root_type_idx(&self, name: &str) -> Option<u32> {
        if let Some(idx) = self.sum_roots.get(name).copied() {
            return Some(idx);
        }
        let bare = name.rsplit_once('.').map_or(name, |(_, b)| b);
        self.sum_roots.get(bare).copied()
    }

    /// Look up a variant by bare name. Returns the first registered
    /// entry — fine when the name is unambiguous across the whole
    /// program (the common case). Callers that know the parent
    /// sumtype should use `variant_in` to disambiguate.
    pub(super) fn variant(&self, name: &str) -> Option<&VariantInfo> {
        self.variants.get(name).and_then(|v| v.first())
    }

    /// Look up a variant in a specific parent sumtype. Use this when
    /// the call site has the full `ParentType.VariantName` shape (a
    /// `Pattern::Constructor` / dotted-Attr / dotted FnCall callee)
    /// so the same bare variant name in a sibling sumtype doesn't
    /// silently shadow the right one.
    pub(super) fn variant_in(&self, parent: &str, name: &str) -> Option<&VariantInfo> {
        self.variants.get(name)?.iter().find(|v| v.parent == parent)
    }

    pub(super) fn record_field_index(&self, record: &str, field: &str) -> Option<u32> {
        // Same Module.Name → Name fallback as `record_type_idx`.
        let fields = self.record_fields.get(record).or_else(|| {
            record
                .rsplit_once('.')
                .and_then(|(_, b)| self.record_fields.get(b))
        })?;
        fields
            .iter()
            .position(|(n, _)| n == field)
            .map(|i| i as u32)
    }

    #[allow(dead_code)]
    pub(super) fn record_field_type(&self, record: &str, field: &str) -> Option<&str> {
        let fields = self.record_fields.get(record).or_else(|| {
            record
                .rsplit_once('.')
                .and_then(|(_, b)| self.record_fields.get(b))
        })?;
        fields
            .iter()
            .find(|(n, _)| n == field)
            .map(|(_, t)| t.as_str())
    }

    /// Newtype optimization: a `record Foo { x: T }` (single primitive
    /// field) or `type Foo = Foo(T)` (single-variant sum, single primitive
    /// payload) is structurally equivalent to `T`. We erase the wrapper
    /// at the wasm level — every `Foo` slot carries `T` directly,
    /// `RecordCreate { Foo, x = e }` lowers to just `e`, `Attr(_, x)`
    /// lowers to identity, `match obj { Foo.Foo(n) -> body }` binds `n`
    /// to the underlying `T` value with no `struct.get`. Same trick
    /// rustc uses for `struct UserId(u64)`.
    /// ETAP-2 carrier-`i64`: install the set of refinement-via-opaque
    /// carrier type names whose proven bound `fits_i64`. Called once by the
    /// wasm-gc codegen entry after it derives the carrier interval table.
    /// Keyed by post-flatten `TypeDef` name (`"IntRange"`, or the canonical
    /// `"Left.IntRange"` for a collision-renamed dep type).
    pub(super) fn set_eligible_carriers(&mut self, names: std::collections::HashSet<String>) {
        self.eligible_carriers = names;
    }

    /// Install proof-derived packed sequence layouts after the ordinary type
    /// discovery pass. Slots are appended deterministically to the user-type
    /// rec group so every later signature and body sees one stable nominal
    /// array type per eligible refinement.
    pub(super) fn install_packed_sequences(
        &mut self,
        layouts: HashMap<String, crate::codegen::proof_lower::PackedSequenceLayout>,
    ) {
        let mut names: Vec<String> = layouts.keys().cloned().collect();
        names.sort();
        for name in names {
            let layout = layouts[&name];
            let type_idx = self.user_type_count;
            self.user_type_count += 1;
            self.packed_sequences
                .insert(name.clone(), PackedSequenceType { type_idx, layout });
            self.packed_sequence_order.push(name);
        }
    }

    /// Resolve an identity-preserving qualified alias (installed via
    /// `set_type_name_aliases`) to its canonical post-flatten `TypeDef`
    /// name; a name with no alias entry is returned trimmed, unchanged.
    /// This is NOT a suffix fallback: only spellings `flatten_multimodule`
    /// proved unambiguous (sole declarer, no collision rename, no entry
    /// shadow) are in the map, so a collision-renamed dep type keeps
    /// resolving by its exact canonical name.
    pub(super) fn canonical_type_name<'a>(&'a self, type_name: &'a str) -> &'a str {
        let trimmed = type_name.trim();
        match self.type_name_aliases.get(trimmed) {
            Some(canonical) => canonical.as_str(),
            None => trimmed,
        }
    }

    /// Install the flatten-derived qualified type-name aliases. See
    /// `type_name_aliases` for the identity contract.
    pub(super) fn set_type_name_aliases(&mut self, aliases: HashMap<String, String>) {
        self.type_name_aliases = aliases;
        // Mirror the alias spellings onto the record identity tables —
        // `records` (name → struct type idx) and `record_fields` (name →
        // declared field list) — as extra lookup KEYS on the canonical
        // entries. Emit paths that key off a STAMPED spelling (map
        // key/value helpers, hash_record / eq_record field walks, the
        // demoted plain construct path) then resolve the same slot and
        // field list for either spelling. The alias provably denotes the
        // same `TypeDef` (sole declarer, no collision rename, no entry
        // shadow), so sharing the rows is identity-correct; a spelling
        // with NO alias keeps declining fail-closed. Point lookups only:
        // the sole `record_fields.keys()` walk (struct-type emission)
        // runs at build time, BEFORE this installer, so mirrored keys
        // never double-emit a struct.
        let mirrored: Vec<(String, String)> = self
            .type_name_aliases
            .iter()
            .map(|(alias, canonical)| (alias.clone(), canonical.clone()))
            .collect();
        for (alias, canonical) in mirrored {
            if let Some(idx) = self.records.get(&canonical).copied() {
                self.records.entry(alias.clone()).or_insert(idx);
            }
            if let Some(fields) = self.record_fields.get(&canonical).cloned() {
                self.record_fields.entry(alias).or_insert(fields);
            }
        }
        // Re-run the Map-key newtype suppression with alias-aware key
        // spellings. The build-time scan populated `non_newtypable_keys`
        // BEFORE the aliases were installed, and its record/variant
        // tables are bare-keyed — so a Map whose key is spelled ONLY
        // qualified (`Map<Dep.IntRange, Int>` from an entry-side
        // annotation stamp) escaped the suppression and its key record
        // would be newtype-erased while the key array stores struct
        // refs. Mark the CANONICAL name; `newtype_underlying`
        // canonicalizes its argument first, so one canonical entry
        // suppresses both spellings.
        let mut extra: Vec<String> = Vec::new();
        for canonical_map in &self.map_order {
            let Some((k, _)) = parse_map_kv(canonical_map) else {
                continue;
            };
            let Some(k_canon) = self.type_name_aliases.get(k.trim()) else {
                continue;
            };
            if self.record_fields.contains_key(k_canon)
                || self
                    .variants
                    .values()
                    .flat_map(|v| v.iter())
                    .any(|v| v.parent == *k_canon)
            {
                extra.push(k_canon.clone());
            }
        }
        self.non_newtypable_keys.extend(extra);
    }

    /// Exact-name lookup — NO qualified→bare suffix fallback. The layout
    /// table keys are post-flatten `TypeDef` names (bare for entry and
    /// non-colliding dep types, canonical `Prefix.Name` for collision-
    /// renamed dep types), and both the collision guard and the ungated-
    /// construction demotion scan match those names exactly. A bare-name
    /// fallback here would hand a collision-renamed dep type (`Left.Octets`)
    /// the packed layout of an unrelated bare-named gated type (`Octets`),
    /// letting an ungated record silently truncate through `pack`. The only
    /// indirection is `canonical_type_name`, whose alias entries provably
    /// denote the same `TypeDef` (entry-side qualified annotation stamps
    /// over a sole-declarer dep type).
    pub(super) fn packed_sequence(&self, type_name: &str) -> Option<PackedSequenceType> {
        self.packed_sequences
            .get(self.canonical_type_name(type_name))
            .copied()
    }

    /// ETAP-2 carrier-`i64`: is `type_name` a carrier whose erasure should be
    /// a native `i64`? A carrier in the eligible set is ALSO a newtype
    /// (single-field opaque record of `Int`), so `newtype_underlying` already
    /// returns `Some("Int")` for it — this just decides whether that erasure
    /// becomes `i64` or stays `$AverInt`. Exact-name lookup (modulo the
    /// identity-preserving `canonical_type_name` aliases), matching the
    /// post-flatten `TypeDef` name the interval table and the demotion scans
    /// key on — a qualified→bare suffix fallback would let a collision-renamed
    /// dep type (`Left.IntRange`) inherit an unrelated carrier's i64 erasure
    /// and trap on values only the bignum representation can hold.
    pub(super) fn is_eligible_carrier(&self, type_name: &str) -> bool {
        if self.eligible_carriers.is_empty() {
            return false;
        }
        self.eligible_carriers
            .contains(self.canonical_type_name(type_name))
    }

    /// ETAP-2 multi-field carrier-`i64`: install the eligible `(record, field)`
    /// pairs whose proven bound `fits_i64`. Called once by the wasm-gc codegen
    /// entry after it derives + demotion-tightens the multi-field carrier table.
    /// Keyed by post-flatten record `TypeDef` name + field name
    /// (`("Coord", "x")`).
    pub(super) fn set_eligible_carrier_fields(
        &mut self,
        fields: std::collections::HashSet<(String, String)>,
    ) {
        self.eligible_carrier_fields = fields;
    }

    /// ETAP-2 multi-field carrier-`i64`: is `(record_name, field)` a bounded
    /// record field whose storage should be a native `i64`? Exact-name lookup
    /// (modulo `canonical_type_name` aliases), for the same reason as
    /// `is_eligible_carrier`.
    pub(super) fn is_eligible_carrier_field(&self, record_name: &str, field: &str) -> bool {
        if self.eligible_carrier_fields.is_empty() {
            return false;
        }
        self.eligible_carrier_fields.contains(&(
            self.canonical_type_name(record_name).to_string(),
            field.to_string(),
        ))
    }

    pub(super) fn newtype_underlying(&self, type_name: &str) -> Option<&str> {
        // Qualified annotation stamps over a sole-declarer dep type must
        // see the SAME newtype/carrier decision as the canonical name —
        // otherwise an entry-side `r: Dep.IntRange` slot stays a struct
        // ref while the carrier value it holds erased to `i64`.
        let type_name = self.canonical_type_name(type_name);
        // Suppress newtype optimisation when the type is used as a
        // `Map<K, *>` key. Map's open-addressing layout uses
        // `keys[i] == null` as the empty marker, which only works
        // when keys land in `keys` as ref values — newtyping a key
        // record down to its underlying primitive (e.g. i64) would
        // strip the ref and break the marker.
        if self.non_newtypable_keys.contains(type_name) {
            return None;
        }
        // Record case: exactly one field, primitive type.
        if let Some(fields) = self.record_fields.get(type_name)
            && fields.len() == 1
            && is_primitive(&fields[0].1)
        {
            return Some(fields[0].1.as_str());
        }
        // Sum case: parent has exactly one variant, that variant has
        // exactly one field, that field is primitive.
        let mut variants_of_parent = self
            .variants
            .values()
            .flat_map(|v| v.iter())
            .filter(|v| v.parent == type_name);
        if let Some(only) = variants_of_parent.next()
            && variants_of_parent.next().is_none()
            && only.fields.len() == 1
            && is_primitive(&only.fields[0])
        {
            return Some(only.fields[0].as_str());
        }
        None
    }
}

/// Split a canonical `Map<K, V>` into its `K` and `V` parts (both
/// borrowed slices of the input). Returns `None` if the string
/// doesn't match the expected shape.
pub(super) fn parse_map_kv(canonical: &str) -> Option<(&str, &str)> {
    let inner = canonical.trim().strip_prefix("Map<")?.strip_suffix('>')?;
    let bytes = inner.as_bytes();
    // Track both angle-bracket and paren depth so `Map<(A, B), V>`
    // (tuple key) splits at the right comma.
    let mut depth: i32 = 0;
    for (idx, b) in bytes.iter().enumerate() {
        match b {
            b'<' | b'(' => depth += 1,
            b'>' | b')' => depth -= 1,
            b',' if depth == 0 => {
                return Some((inner[..idx].trim(), inner[idx + 1..].trim()));
            }
            _ => {}
        }
    }
    None
}

/// True if any expression in the fn body produces a String value —
/// via a literal, an interpolation, or a String-producing builtin.
/// Used by `TypeRegistry::build` to decide whether to allocate the
/// `(array i8)` slot.
fn fn_body_produces_string(fd: &crate::ir::hir::ResolvedFnDef) -> bool {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|s| match s {
        ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => expr_uses_string(&e.node),
    })
}

fn expr_uses_string(expr: &crate::ir::hir::ResolvedExpr) -> bool {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match expr {
        ResolvedExpr::Call(callee, args) => {
            if let ResolvedCallee::Builtin(dotted) = callee
                && matches!(
                    dotted.as_str(),
                    "String.fromInt"
                            | "String.fromFloat"
                            | "String.len"
                            | "String.length"
                            | "String.startsWith"
                            | "String.contains"
                            | "String.slice"
                            | "String.toUpper"
                            | "String.toLower"
                            | "String.trim"
                            | "String.replace"
                            | "String.split"
                            | "String.join"
                            | "String.fromBool"
                            | "String.endsWith"
                            | "String.charAt"
                            | "String.chars"
                            | "String.byteLength"
                            | "String.firstCodePoint"
                            | "String.fromCodePoint"
                            // `Int.mod`, `Int.div`, `Int.fromString`,
                            // and `Float.fromString` return
                            // Result<_, String> — touching them forces the
                            // String slot for the error payload even when
                            // the program never reads the Err arm.
                            | "Int.mod"
                            | "Int.div"
                            | "Int.fromString"
                            | "Float.fromString"
                            // Same reason: the three count-taking `Bits`
                            // operations return `Result<Int, String>`.
                            | "Bits.shiftLeft"
                            | "Bits.shiftRight"
                            | "Bits.low"
                            // Effects that produce or consume String at
                            // their boundary. The string slot has to be
                            // allocated whenever any of these is called
                            // — without that, the import signature
                            // can't even be emitted (`(ref null
                            // $string)` references an undeclared type).
                            // Mirror of `EffectName::*` whose typed
                            // signature in `effects.rs` uses
                            // `string_ref_ty`. Keep in sync.
                            | "Console.print"
                            | "Console.error"
                            | "Console.warn"
                            | "Console.readLine"
                            | "Args.get"
                            | "Args._get"
                            | "Env.get"
                            | "Env.set"
                            | "Time.now"
                            | "Request.method"
                            | "Request.url"
                            | "Request.path"
                            | "Request.query"
                            | "Request.body"
                            | "Request.headersLoad"
                            | "Request.headers"
                            | "Response.text"
                            | "Response.setHeader"
                            | "Http.send"
                            | "Http.addRequestHeader"
                            | "Terminal.print"
                            | "Terminal.setColor"
                            | "Terminal.readKey"
                )
            {
                return true;
            }
            args.iter().any(|a| expr_uses_string(&a.node))
        }
        ResolvedExpr::BinOp(_, l, r) => expr_uses_string(&l.node) || expr_uses_string(&r.node),
        ResolvedExpr::Neg(inner) => expr_uses_string(&inner.node),
        ResolvedExpr::Match { subject, arms } => {
            expr_uses_string(&subject.node) || arms.iter().any(|a| expr_uses_string(&a.body.node))
        }
        ResolvedExpr::TailCall { args, .. } => args.iter().any(|a| expr_uses_string(&a.node)),
        ResolvedExpr::Attr(obj, _) => expr_uses_string(&obj.node),
        ResolvedExpr::Ctor(_, args) => args.iter().any(|a| expr_uses_string(&a.node)),
        ResolvedExpr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, e)| expr_uses_string(&e.node))
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            expr_uses_string(&base.node) || updates.iter().any(|(_, e)| expr_uses_string(&e.node))
        }
        ResolvedExpr::Literal(crate::ast::Literal::Str(_)) => true,
        ResolvedExpr::InterpolatedStr(_) => true,
        _ => false,
    }
}

/// Walk a fn body, collecting unique String literals into a per-segment
/// table. Both `Literal::Str` and the `Literal` parts of an
/// `InterpolatedStr` count — each unique byte sequence gets a passive
/// data segment.
fn fn_body_calls_builtin(fd: &crate::ir::hir::ResolvedFnDef, dotted: &str) -> bool {
    use crate::ir::hir::ResolvedCallee;
    fn_body_reaches(
        fd,
        &|callee| matches!(callee, ResolvedCallee::Builtin(name) if name == dotted),
    )
}

/// Does any callee reachable from `fd`'s body satisfy `hits`?
///
/// ONE traversal, shared by every reachability question this module asks —
/// which helper to register, which synthetic string literal to intern. Those
/// used to be separate hand-written walks, and each was quietly incomplete in
/// its own way: the builtin walk skipped `InterpolatedStr`, so
/// `"{Result.withDefault(Bits.shiftLeft(1, n), 0)}"` never interned the
/// error message and the module failed validation; the `Bits` walk skipped
/// `MapLiteral`, so `{"v" => Bits.and(a, b)}` never registered the helper.
/// Both are the same bug written twice, which is the argument for writing it
/// once.
///
/// The match is EXHAUSTIVE on purpose — no wildcard arm. A new `ResolvedExpr`
/// variant is then a compile error here rather than a silent hole that only
/// shows up as a wasm validation failure on whichever program happens to nest
/// a call inside the new shape.
fn fn_body_reaches(
    fd: &crate::ir::hir::ResolvedFnDef,
    hits: &dyn Fn(&crate::ir::hir::ResolvedCallee) -> bool,
) -> bool {
    use crate::ir::hir::{ResolvedExpr, ResolvedFnBody, ResolvedStmt, ResolvedStrPart};
    fn walk(e: &ResolvedExpr, hits: &dyn Fn(&crate::ir::hir::ResolvedCallee) -> bool) -> bool {
        let any = |xs: &[crate::ast::Spanned<ResolvedExpr>]| xs.iter().any(|x| walk(&x.node, hits));
        match e {
            ResolvedExpr::Call(callee, args) => hits(callee) || any(args),
            ResolvedExpr::Match { subject, arms } => {
                walk(&subject.node, hits) || arms.iter().any(|a| walk(&a.body.node, hits))
            }
            ResolvedExpr::BinOp(_, l, r) => walk(&l.node, hits) || walk(&r.node, hits),
            ResolvedExpr::Neg(inner) | ResolvedExpr::ErrorProp(inner) => walk(&inner.node, hits),
            ResolvedExpr::Attr(obj, _) => walk(&obj.node, hits),
            ResolvedExpr::TailCall { args, .. } => any(args),
            ResolvedExpr::Ctor(_, args) => any(args),
            ResolvedExpr::List(xs)
            | ResolvedExpr::Tuple(xs)
            | ResolvedExpr::IndependentProduct(xs, _) => any(xs),
            // The two arms whose absence caused real miscompiles.
            ResolvedExpr::MapLiteral(pairs) => pairs
                .iter()
                .any(|(k, v)| walk(&k.node, hits) || walk(&v.node, hits)),
            ResolvedExpr::InterpolatedStr(parts) => parts.iter().any(|p| match p {
                ResolvedStrPart::Parsed(inner) => walk(&inner.node, hits),
                ResolvedStrPart::Literal(_) => false,
            }),
            ResolvedExpr::RecordCreate { fields, .. } => {
                fields.iter().any(|(_, e)| walk(&e.node, hits))
            }
            ResolvedExpr::RecordUpdate { base, updates, .. } => {
                walk(&base.node, hits) || updates.iter().any(|(_, e)| walk(&e.node, hits))
            }
            // Leaves — nothing nested to reach a callee through.
            ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {
                false
            }
        }
    }
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|stmt| match stmt {
        ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => walk(&e.node, hits),
    })
}

/// Whether `fd` can reach a `Bits` operation — the gate for registering the
/// two `Bits` WAT helpers.
///
/// Must see BOTH callee forms. A `Bits.shiftLeft(x, amount)` with a dynamic
/// count stays a `Builtin`, but the literal-count discharge rewrites
/// `Bits.shiftLeft(x, 5)` into `Intrinsic(BitsShiftLeft)` before this runs —
/// so a builtin-only check would miss exactly the calls the discharge made
/// total, and the helper lookup would then fail at emit time. `Bits.not` is
/// absent on purpose: it lowers to `__aint_sub`, which the unconditional
/// arithmetic prelude already registers.
pub(super) fn fn_uses_bits(fd: &crate::ir::hir::ResolvedFnDef) -> bool {
    use crate::ir::hir::{BuiltinIntrinsic, ResolvedCallee};
    fn_body_reaches(fd, &|callee| match callee {
        ResolvedCallee::Builtin(name) => matches!(
            name.as_str(),
            "Bits.and" | "Bits.or" | "Bits.xor" | "Bits.shiftLeft" | "Bits.shiftRight" | "Bits.low"
        ),
        ResolvedCallee::Intrinsic(intr) => matches!(
            intr,
            BuiltinIntrinsic::BitsShiftLeft
                | BuiltinIntrinsic::BitsShiftRight
                | BuiltinIntrinsic::BitsLow
        ),
        _ => false,
    })
}

fn fn_body_calls_int_mod(fd: &crate::ir::hir::ResolvedFnDef) -> bool {
    fn_body_calls_builtin(fd, "Int.mod")
}

fn fn_body_calls_int_div(fd: &crate::ir::hir::ResolvedFnDef) -> bool {
    fn_body_calls_builtin(fd, "Int.div")
}

/// "any Int arithmetic reachable" gate. True iff the fn signature
/// mentions `Int` (param or return) OR the body contains an arithmetic
/// `BinOp` / `Neg` / Int literal. Used to keep the `$AverInt` slots out
/// of pure-String/Float/effect-only programs (a SIZE optimization — the
/// semantics are always `Int = ℤ`). Deliberately broad (an Int param is
/// enough): the cost of a false positive is two unused type slots that
/// `wasm-opt -Oz` would strip anyway; a false negative would be a
/// miscompile, so we err toward inclusion.
/// Builtins whose signature mentions `Int` (produce OR consume one), so a
/// call to any of them must flip the bignum gate even in a module with no
/// Int literal / arithmetic of its own. Kept in sync with the surface
/// builtins that the wasm-gc backend lowers with an `Int` ValType on either
/// side. Deliberately generous — a stray match is two stripped type slots,
/// a miss is a wrapping-i64 miscompile of a `ℤ` value.
fn builtin_touches_int(name: &str) -> bool {
    matches!(
        name,
        // Int producers / consumers.
        "Int.fromString"
            | "Int.fromFloat"
            | "Int.abs"
            | "Int.min"
            | "Int.max"
            | "Int.div"
            | "Int.mod"
            | "Int.toFloat"
            // `Bits` is a bit-level VIEW of `Int`: every parameter and every
            // payload is an `Int`, so any call must flip the gate. Missing
            // one here would lower a `ℤ` value through the wrapping-i64
            // scalar path — precisely the silent truncation `Bits` must not
            // have.
            | "Bits.and"
            | "Bits.or"
            | "Bits.xor"
            | "Bits.not"
            | "Bits.shiftLeft"
            | "Bits.shiftRight"
            | "Bits.low"
            // Float/Int bridges.
            | "Float.fromInt"
            | "Float.floor"
            | "Float.ceil"
            | "Float.round"
            // String <-> Int.
            | "String.fromInt"
            | "String.len"
            | "String.length"
            | "String.byteLength"
            | "String.charAt"
            // Unicode code points are Int payloads.
            | "String.firstCodePoint"
            | "String.fromCodePoint"
            // Indexed collection ops carry an `Int` index / length.
            | "Vector.len"
            | "Vector.get"
            | "Vector.set"
            | "Vector.new"
            | "List.len"
            // Effect builtins touching Int.
            | "Random.int"
            | "Time.unixMs"
            | "Tcp.sendBytes"
            | "Tcp.beginConnect"
            | "Tcp.listen"
            | "Tcp.readBytes"
            | "Tcp.readSome"
            | "Tcp.poll"
            | "Tcp.writeBytes"
            | "Crypto.sha256"
    )
}

fn fn_uses_int_arithmetic(fd: &crate::ir::hir::ResolvedFnDef) -> bool {
    use crate::ir::hir::{ResolvedExpr, ResolvedFnBody, ResolvedStmt};
    if fd
        .return_type
        .display()
        .split(|c: char| !c.is_alphanumeric())
        .any(|t| t == "Int")
    {
        return true;
    }
    if fd.params.iter().any(|(_, t)| {
        t.display()
            .split(|c: char| !c.is_alphanumeric())
            .any(|tok| tok == "Int")
    }) {
        return true;
    }
    fn walk(e: &ResolvedExpr) -> bool {
        use crate::ast::Literal;
        match e {
            // A big-int literal MUST flip the bignum gate on: it lowers to the
            // `$AverInt` string-parse path, which assumes the `$AverInt` slots and
            // bignum prelude exist. Missing this is a silent miscompile.
            ResolvedExpr::Literal(Literal::Int(_) | Literal::BigInt(_)) => true,
            ResolvedExpr::Neg(_) => true,
            ResolvedExpr::BinOp(op, l, r) => {
                use crate::ast::BinOp;
                matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div)
                    || walk(&l.node)
                    || walk(&r.node)
            }
            ResolvedExpr::Call(callee, args) => {
                // A builtin that PRODUCES or CONSUMES an `Int` flips the gate
                // even when the program has no Int literal / arithmetic of its
                // own — e.g. a module whose only Int touch is
                // `Int.fromString(s)` (returns `Result<Int,String>`) or
                // `Int.fromFloat(f)`. Without this the `$AverInt` slots stay
                // unallocated and the Int-producing helper silently lowers
                // through the wrapping-i64 scalar path (a 38-digit string would
                // parse to a wrapped value). Mirrors the "err toward inclusion"
                // policy: a false positive is two stripped slots, a miss is a
                // miscompile.
                use crate::ir::hir::ResolvedCallee;
                let int_builtin = matches!(callee,
                    ResolvedCallee::Builtin(name) if builtin_touches_int(name));
                int_builtin || args.iter().any(|a| walk(&a.node))
            }
            ResolvedExpr::Ctor(_, args) => args.iter().any(|a| walk(&a.node)),
            ResolvedExpr::Match { subject, arms } => {
                walk(&subject.node) || arms.iter().any(|a| walk(&a.body.node))
            }
            ResolvedExpr::TailCall { args, .. } => args.iter().any(|a| walk(&a.node)),
            ResolvedExpr::Attr(o, _) | ResolvedExpr::ErrorProp(o) => walk(&o.node),
            ResolvedExpr::List(xs)
            | ResolvedExpr::Tuple(xs)
            | ResolvedExpr::IndependentProduct(xs, _) => xs.iter().any(|x| walk(&x.node)),
            ResolvedExpr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| walk(&e.node)),
            ResolvedExpr::RecordUpdate { base, updates, .. } => {
                walk(&base.node) || updates.iter().any(|(_, e)| walk(&e.node))
            }
            // Arithmetic hidden inside a `{...}` interpolation or a Map
            // literal must still flip the gate — string interpolation is the
            // idiomatic way to render an Int, and a missed arm lowers the
            // WHOLE module's Int as wrapping i64 (a silent miscompile, not an
            // error). The remaining arms are genuine leaves; enumerated
            // explicitly (no `_` wildcard) so a future `ResolvedExpr` variant
            // fails the build rather than silently defaulting the gate off.
            ResolvedExpr::InterpolatedStr(parts) => parts.iter().any(|p| {
                matches!(p, crate::ir::hir::ResolvedStrPart::Parsed(inner) if walk(&inner.node))
            }),
            ResolvedExpr::MapLiteral(pairs) => {
                pairs.iter().any(|(k, v)| walk(&k.node) || walk(&v.node))
            }
            ResolvedExpr::Literal(_)
            | ResolvedExpr::Ident(_)
            | ResolvedExpr::Resolved { .. } => false,
        }
    }
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|stmt| match stmt {
        ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => walk(&e.node),
    })
}

fn collect_string_literals_in_fn(
    fd: &crate::ir::hir::ResolvedFnDef,
    out: &mut Vec<Vec<u8>>,
    idx: &mut HashMap<Vec<u8>, u32>,
) {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => &e.node,
        };
        collect_string_literals_in_expr(expr, out, idx);
    }
}

fn intern_literal(bytes: Vec<u8>, out: &mut Vec<Vec<u8>>, idx: &mut HashMap<Vec<u8>, u32>) {
    if !idx.contains_key(&bytes) {
        let n = out.len() as u32;
        idx.insert(bytes.clone(), n);
        out.push(bytes);
    }
}

fn collect_string_literals_in_expr(
    expr: &crate::ir::hir::ResolvedExpr,
    out: &mut Vec<Vec<u8>>,
    idx: &mut HashMap<Vec<u8>, u32>,
) {
    use crate::ast::Literal;
    use crate::ir::hir::{ResolvedExpr, ResolvedStrPart};
    match expr {
        ResolvedExpr::Literal(Literal::Str(s)) => intern_literal(s.as_bytes().to_vec(), out, idx),
        // A big-int literal lowers through `Int.fromString`, which takes a
        // `$string` of the decimal digits — intern that segment here so the
        // codegen `string_literal_segment(bytes)` lookup resolves.
        ResolvedExpr::Literal(Literal::BigInt(s)) => {
            intern_literal(s.as_bytes().to_vec(), out, idx)
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            for p in parts {
                match p {
                    ResolvedStrPart::Literal(s) => intern_literal(s.as_bytes().to_vec(), out, idx),
                    ResolvedStrPart::Parsed(inner) => {
                        collect_string_literals_in_expr(&inner.node, out, idx);
                    }
                }
            }
        }
        ResolvedExpr::Call(_, args) => {
            for a in args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            collect_string_literals_in_expr(&l.node, out, idx);
            collect_string_literals_in_expr(&r.node, out, idx);
        }
        ResolvedExpr::Neg(inner) => collect_string_literals_in_expr(&inner.node, out, idx),
        ResolvedExpr::Match { subject, arms } => {
            collect_string_literals_in_expr(&subject.node, out, idx);
            for a in arms {
                if let crate::ir::hir::ResolvedPattern::Literal(Literal::Str(s)) = &a.pattern {
                    intern_literal(s.as_bytes().to_vec(), out, idx);
                }
                collect_string_literals_in_expr(&a.body.node, out, idx);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        ResolvedExpr::Attr(obj, _) => collect_string_literals_in_expr(&obj.node, out, idx),
        ResolvedExpr::ErrorProp(inner) => collect_string_literals_in_expr(&inner.node, out, idx),
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_string_literals_in_expr(&e.node, out, idx);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_string_literals_in_expr(&base.node, out, idx);
            for (_, e) in updates {
                collect_string_literals_in_expr(&e.node, out, idx);
            }
        }
        ResolvedExpr::List(items) => {
            for item in items {
                collect_string_literals_in_expr(&item.node, out, idx);
            }
        }
        ResolvedExpr::Tuple(items) | ResolvedExpr::IndependentProduct(items, _) => {
            for item in items {
                collect_string_literals_in_expr(&item.node, out, idx);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_string_literals_in_expr(&k.node, out, idx);
                collect_string_literals_in_expr(&v.node, out, idx);
            }
        }
        _ => {}
    }
}

/// Resolve an Aver type-annotation string to a wasm value type, or to
/// "no result" when the type is `Unit`. User-type names look up the
/// registry and return a nullable struct ref.
pub(super) fn aver_to_wasm(
    type_str: &str,
    registry: Option<&TypeRegistry>,
) -> Result<Option<ValType>, WasmGcError> {
    let trimmed = type_str.trim();
    // `Int = ℤ`: `Int` is the `$AverInt` struct ref everywhere
    // (signatures, locals, etc.) whenever Int arithmetic is reachable,
    // NOT the scalar `i64`. Intercept before `primitive_to_wasm` so the
    // ref shape wins. Float / Bool keep their scalar lowering.
    if trimmed == "Int"
        && let Some(reg) = registry
        && reg.bignum
    {
        return Ok(Some(aint_ref_ty(reg)?));
    }
    if let Some(v) = primitive_to_wasm(trimmed) {
        return Ok(Some(v));
    }
    if trimmed == "Unit" {
        return Ok(None);
    }
    if let Some(reg) = registry {
        // Structural refinement erasure — an eligible nominal
        // `record X { values: List<Int> }` is represented by its proven packed
        // array everywhere. Construction/projection bridge the source-level
        // list at the opaque module boundary.
        if let Some(packed) = reg.packed_sequence(trimmed) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(packed.type_idx),
            })));
        }
        // Newtype optimization — a single-field record / single-variant
        // sum of a primitive lowers to the underlying primitive
        // everywhere. Saves an allocation per wrap and a struct.get
        // per unwrap. `Int = ℤ`: an erased Int field is the `$AverInt`
        // ref, NOT scalar i64 — recurse so the bignum interception above
        // applies (otherwise an erased `Box(v: Int)` lowers to i64 while
        // its stored/compared value is a ref → wasm-validation mismatch).
        if let Some(underlying) = reg.newtype_underlying(trimmed) {
            // ETAP-2 carrier-`i64`: an eligible carrier (opaque single-`Int`
            // record whose smart-constructor bound `fits_i64`) erases to a
            // NATIVE `i64` instead of the `$AverInt` ref — the size lever.
            // This fires EVERYWHERE the carrier type-string reaches a
            // ValType (fn slots, record fields, Option/Result payloads,
            // `Vector<Carrier>` elements) because they all route here. The
            // construct / project emit sites box-bridge the boundary to the
            // surrounding `$AverInt` Int values.
            if reg.is_eligible_carrier(trimmed) {
                return Ok(Some(ValType::I64));
            }
            if underlying.trim() == "Int" && reg.bignum {
                return Ok(Some(aint_ref_ty(reg)?));
            }
            return Ok(primitive_to_wasm(underlying));
        }
        if let Some(idx) = reg.record_type_idx(trimmed) {
            return Ok(Some(struct_ref(idx)));
        }
        // Sum type by parent name — use its nominal root struct as the
        // carrier. Constructors still allocate their concrete variant
        // structs, all declared as subtypes of this root.
        if let Some(root_idx) = reg.sum_root_type_idx(trimmed) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(root_idx),
            })));
        }
    }
    // String maps to `(ref null (array i8))` when the registry has
    // pre-allocated the array type during `build`. Unique-pointer
    // semantics aren't needed; nullable is fine because Aver's type
    // system already proves String values are non-null.
    if trimmed == "String" {
        if let Some(reg) = registry
            && let Some(idx) = reg.string_array_type_idx
        {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
        return Err(WasmGcError::Validation(
            "String reachable from a fn signature but no string type slot was allocated".into(),
        ));
    }
    if trimmed == "String.Index" {
        if let Some(reg) = registry
            && let Some(idx) = reg.string_index_array_type_idx
        {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
        return Err(WasmGcError::Validation(
            "String.Index reached wasm-gc without its hidden array slot".into(),
        ));
    }
    // `Vector<T>` resolves to `(ref null $vector_T)`. The registry's
    // `vector_types` map is keyed on whitespace-stripped canonical
    // form so `Vector<Int>` and `Vector< Int >` collide on the same
    // slot.
    if trimmed.starts_with("Vector<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.vector_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `Option<T>` resolves to `(ref null $option_T)`.
    if trimmed.starts_with("Option<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.option_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `List<T>` — recursive Cons cell `(struct (T) (ref null $list_T))`.
    // Empty list = null ref.
    if trimmed.starts_with("List<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical = normalize_compound(trimmed);
        if let Some(idx) = reg.list_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `Result<T, E>` — `(struct (mut i32 tag) (mut T ok) (mut E err))`.
    if trimmed.starts_with("Result<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.result_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `Map<K, V>` — monomorphised per instantiation. The registry
    // discovers each unique `Map<K, V>` in fn signatures and
    // allocates a slot triple (keys array, values array, struct).
    if trimmed.starts_with("Map<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(slots) = reg.map_slots(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.map),
            })));
        }
    }
    // `Tuple<A, B>` — `(struct (mut A) (mut B))`.
    if trimmed.starts_with("Tuple<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.tuple_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `(A, B)` surface form for Tuple. Normalize to `Tuple<A,B>`
    // canonical for the registry lookup.
    if trimmed.starts_with('(')
        && trimmed.ends_with(')')
        && let Some(reg) = registry
    {
        let inner = &trimmed[1..trimmed.len() - 1];
        // Quick check: must contain at least one top-level comma.
        let mut depth: i32 = 0;
        let mut has_top_comma = false;
        for b in inner.as_bytes() {
            match b {
                b'(' | b'<' => depth += 1,
                b')' | b'>' => depth -= 1,
                b',' if depth == 0 => has_top_comma = true,
                _ => {}
            }
        }
        if has_top_comma {
            let canonical_inner: String = inner.chars().filter(|c| !c.is_whitespace()).collect();
            let canonical = format!("Tuple<{canonical_inner}>");
            if let Some(idx) = reg.tuple_type_idx(&canonical) {
                return Ok(Some(ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(idx),
                })));
            }
        }
    }
    // Built-in opaque types — `BranchPath`, `Trace`, `EffectEvent`
    // are introduced by the verify / oracle / effect-lifting pipeline
    // and only reach runtime fns whose bodies are dead from a `_start`
    // perspective. The legacy `--target wasm` backend implicitly hides
    // them inside its tagged-i64 fallback; on wasm-gc we lower the
    // param/return slot to `(ref null eq)` so the fn signature is
    // representable without committing to a concrete struct shape.
    if matches!(trimmed, "BranchPath" | "Trace" | "EffectEvent") {
        return Ok(Some(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Abstract {
                shared: false,
                ty: AbstractHeapType::Eq,
            },
        })));
    }

    // First-class Fn values lower to an `i32` — a dense index into the
    // module's single funcref table (table 0). Calling through a
    // `Fn(..)` param emits `call_indirect` on that table (see
    // `module.rs` table/element sections + `from_mir`'s `FnValue` /
    // `LocalSlot` arms), mirroring the VM's symbol-id-value + dynamic
    // dispatch. The index identifies the target fn; the `call_indirect`
    // functype is pre-registered from the param's `Fn(..)` sig so it
    // matches the target fn's own functype exactly. Verify / oracle
    // givens (`fn pairSpec(rnd: Fn(BranchPath, Int, Int, Int) -> Result<Int, String>)`)
    // whose bodies are dead from `_start` carry the same i32 slot
    // harmlessly.
    if trimmed.starts_with("Fn(") {
        return Ok(Some(ValType::I32));
    }

    // Compound types not yet lowered.
    Err(WasmGcError::Validation(format!(
        "aver_to_wasm: cannot lower type `{trimmed}` to a wasm representation"
    )))
}

fn primitive_to_wasm(name: &str) -> Option<ValType> {
    match name {
        "Int" => Some(ValType::I64),
        "Float" => Some(ValType::F64),
        "Bool" => Some(ValType::I32),
        _ => None,
    }
}

/// `(ref null $idx)` — nullable reference to a struct type. Aver doesn't
/// have null at the user level; the nullability is a phase-3 concession
/// because wasm-encoder's struct.new with non-null refs requires more
/// init plumbing than we have today.
pub(super) fn struct_ref(type_idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(type_idx),
    })
}

/// bignum slice 1 — `(ref null $AverInt)` ValType for the carrier
/// struct. Errors if the registry has no `$AverInt` slot (i.e. bignum
/// is off but a site asked for it), which is a wiring bug, not a user
/// error.
pub(super) fn aint_ref_ty(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    let idx = registry.aint_struct_idx.ok_or(WasmGcError::Validation(
        "Int reachable under bignum but no $AverInt type slot was allocated".into(),
    ))?;
    Ok(struct_ref(idx))
}

/// bignum size dedup — `(ref null $mag)` ValType for the 32-bit-limb
/// magnitude array. The shared `__aint_decompose` / `__aint_normalize` /
/// `__aint_strip` / `__aint_umag_cmp` sub-routines take/return it.
pub(super) fn aint_mag_ref_ty(registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    let idx = registry.aint_mag_array_idx.ok_or(WasmGcError::Validation(
        "bignum shared sub-routine needs the $mag array slot, but it wasn't allocated".into(),
    ))?;
    Ok(ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(idx),
    }))
}

/// ETAP-2 SLICE 2a: when a per-param / per-return bare bit is set AND the
/// Aver type is `Int`, the wasm signature would carry a scalar `i64` in
/// place of the `$AverInt` ref. GATED OFF in 2a (`ENABLE_BARE_SLOTS ==
/// false`) so the `*_with_repr` variants produce exactly the boxed
/// signature `return_results` / `param_types` already emit. This keeps the
/// functype registered in `module.rs`, the `SlotTable` params-prefix, and
/// the `call_indirect` `fn_sig_key` on one repr-aware path so they agree
/// byte-for-byte; 2b flips the gate.
const ENABLE_BARE_SLOTS: bool = true;

/// Result-list shape for a wasm function signature derived from an
/// Aver return type.
pub(super) fn return_results(
    type_str: &str,
    registry: Option<&TypeRegistry>,
) -> Result<Vec<ValType>, WasmGcError> {
    Ok(aver_to_wasm(type_str, registry)?.into_iter().collect())
}

/// Repr-aware variant of [`return_results`]. In 2a `bare_return` is always
/// `false` (the wasm-gc MIR is un-rewritten ⇒ `MirFnRepr` default-empty)
/// AND the gate is off, so this delegates to `return_results`.
pub(super) fn return_results_with_repr(
    type_str: &str,
    registry: Option<&TypeRegistry>,
    bare_return: bool,
) -> Result<Vec<ValType>, WasmGcError> {
    if ENABLE_BARE_SLOTS && bare_return && type_str.trim() == "Int" {
        // 2b: bare Int return → scalar i64 result.
        return Ok(vec![ValType::I64]);
    }
    return_results(type_str, registry)
}

/// Param-list shape for a wasm function signature.
pub(super) fn param_types(
    params: &[(String, String)],
    registry: Option<&TypeRegistry>,
) -> Result<Vec<ValType>, WasmGcError> {
    let mut out = Vec::with_capacity(params.len());
    for (_, ty) in params {
        if let Some(v) = aver_to_wasm(ty, registry)? {
            out.push(v);
        }
    }
    Ok(out)
}

/// Repr-aware variant of [`param_types`]. `bare_params[i]` flags the i-th
/// param as scalar `i64`. In 2a `bare_params` is empty AND the gate is off,
/// so every param funnels through `aver_to_wasm` (→ `$AverInt`).
pub(super) fn param_types_with_repr(
    params: &[(String, String)],
    registry: Option<&TypeRegistry>,
    bare_params: &[bool],
) -> Result<Vec<ValType>, WasmGcError> {
    // Fast path / 2a path: no param renders bare (gate off, or
    // `bare_params` empty), so the signature is exactly what `param_types`
    // already emits — keep it as the single boxed source of truth.
    let any_bare = ENABLE_BARE_SLOTS
        && params
            .iter()
            .enumerate()
            .any(|(i, (_, ty))| bare_params.get(i).copied().unwrap_or(false) && ty.trim() == "Int");
    if !any_bare {
        return param_types(params, registry);
    }
    let mut out = Vec::with_capacity(params.len());
    for (i, (_, ty)) in params.iter().enumerate() {
        if bare_params.get(i).copied().unwrap_or(false) && ty.trim() == "Int" {
            // 2b: bare Int param → scalar i64 in the signature.
            out.push(ValType::I64);
            continue;
        }
        if let Some(v) = aver_to_wasm(ty, registry)? {
            out.push(v);
        }
    }
    Ok(out)
}

/// Lower a `Fn(args) -> ret` signature to the wasm `(params, results)`
/// the matching direct call uses, so a `call_indirect` through a
/// `Fn`-param resolves to a functype byte-identical to the target fn's
/// own functype. Each arg lowers via `aver_to_wasm` (skipping `None` —
/// `Unit` params contribute no wasm value), the result via
/// `return_results`. SAME lowering the direct-call path
/// (`param_types` / `return_results` in `module.rs`) uses.
pub(super) fn fn_sig_wasm(
    args: &[crate::ast::Type],
    ret: &crate::ast::Type,
    registry: Option<&TypeRegistry>,
) -> Result<(Vec<ValType>, Vec<ValType>), WasmGcError> {
    let mut params = Vec::with_capacity(args.len());
    for a in args {
        if let Some(v) = aver_to_wasm(&a.display(), registry)? {
            params.push(v);
        }
    }
    let results = return_results(&ret.display(), registry)?;
    Ok((params, results))
}

/// Dedupe / lookup key for a `call_indirect` functype, derived from the
/// LOWERED `ValType`s so the register-site (`module.rs`) and the
/// call-site (`from_mir`) agree exactly. Both paths MUST build it via
/// `fn_sig_wasm` + this helper from a `Type::Fn(args, ret, _)` — never
/// hand-roll a second derivation.
pub(super) fn fn_sig_key(params: &[ValType], results: &[ValType]) -> String {
    format!("{params:?}=>{results:?}")
}

/// Build the `StructType` body for a record: one `FieldType` per
/// declared field, mutable=false (Aver records are immutable; `update`
/// returns a fresh struct via `struct.new`).
pub(super) fn record_struct_type(
    record_name: &str,
    fields: &[(String, String)],
    registry: &TypeRegistry,
) -> Result<StructType, WasmGcError> {
    let mut out = Vec::with_capacity(fields.len());
    for (fname, ty) in fields {
        // ETAP-2 multi-field carrier-`i64`: a bounded Int field of a recognized
        // multi-arg smart-ctor record erases to a native `i64` (the storage size
        // lever — identical to the single-field-leaf composition). Every other
        // field keeps its default lowering, so a MIXED record (one bounded Int
        // → i64, one unbounded → boxed `$AverInt`) lowers each field correctly.
        let val_ty = if registry.is_eligible_carrier_field(record_name, fname) {
            ValType::I64
        } else {
            aver_to_wasm(ty, Some(registry))?.ok_or(WasmGcError::Validation(format!(
                "record field of type {ty} has no wasm representation"
            )))?
        };
        out.push(FieldType {
            element_type: StorageType::Val(val_ty),
            mutable: false,
        });
    }
    Ok(StructType {
        fields: out.into_boxed_slice(),
    })
}

/// Aver type-string for a `BuiltinType` — Map and List forms use the
/// canonical spelling the registry`s discovery pass already
/// understands.
fn builtin_type_to_aver_string(ty: &crate::codegen::builtin_records::BuiltinType) -> String {
    use crate::codegen::builtin_records::BuiltinType;
    match ty {
        BuiltinType::Int => "Int".into(),
        BuiltinType::Str => "String".into(),
        BuiltinType::Bool => "Bool".into(),
        BuiltinType::Float => "Float".into(),
        BuiltinType::ListOf(name) => format!("List<{}>", name),
        BuiltinType::MapStrListStr => "Map<String, List<String>>".into(),
    }
}

/// True iff any FnDef signature or body literal mentions the given
/// type name. Lightweight string scan over annotations + return
/// types — a structural walk would be more precise but every name
/// we register here is unique enough that substring match is OK.
fn items_reference_name(items: &[crate::ast::TopLevel], name: &str) -> bool {
    use crate::ast::TopLevel;
    items.iter().any(|item| match item {
        TopLevel::FnDef(fd) => {
            fd.return_type.contains(name)
                || fd.params.iter().any(|(_, t)| t.contains(name))
                || fd
                    .effects
                    .iter()
                    .any(|e| effect_implies_builtin_record(e.node.as_str(), name))
        }
        TopLevel::TypeDef(crate::ast::TypeDef::Product { fields, .. }) => {
            fields.iter().any(|(_, ty)| ty.contains(name))
        }
        TopLevel::TypeDef(crate::ast::TypeDef::Sum { variants, .. }) => variants
            .iter()
            .flat_map(|variant| variant.fields.iter())
            .any(|ty| ty.contains(name)),
        _ => false,
    })
}

/// Maps a declared effect (`! [Foo.bar]`) to the builtin record it
/// implicitly requires. `Terminal.size` is the canonical case —
/// returns `Terminal.Size`, so the record slot allocates as soon as
/// any fn declares the effect, even before the body runs the call.
/// Same logic underpins `--handler` auto-registering HttpRequest /
/// HttpResponse from the synthesised wrapper.
fn effect_implies_builtin_record(effect: &str, record_name: &str) -> bool {
    let needed = match effect {
        "Terminal.size" => "Terminal.Size",
        // Any Tcp.* effect that takes or returns a connection forces
        // the record slot. Tcp.connect *returns* one; the rest
        // *consume* one through their first parameter, so even a
        // program that only reads / writes / closes still needs the
        // slot allocated.
        "Tcp.connect" | "Tcp.poll" | "Tcp.writeLine" | "Tcp.writeBytes" | "Tcp.readLine"
        | "Tcp.readBytes" | "Tcp.readSome" | "Tcp.close" => "Tcp.Connection",
        // HTTP verb effects all return Result<HttpResponse, String> —
        // ensure the response record slot is allocated even when no
        // user fn signature mentions it.
        "Http.get" | "Http.head" | "Http.delete" | "Http.post" | "Http.put" | "Http.patch" => {
            "HttpResponse"
        }
        // Request / Response surface comes via the user's
        // `! [Request.method]` etc. The Aver type checker already
        // requires HttpRequest / HttpResponse in the handler's
        // signature, so they get picked up by the param/return
        // walk. No extra mapping needed here.
        _ => return false,
    };
    needed == record_name
}

/// Walk fn body for binding annotations carrying a `List<...>` type
/// the fn signatures don't already spell out. `nested: List<List<Int>>
/// = [...]` is the canonical case — the outer `List<List<Int>>` only
/// ever appears in the binding annotation. Mirrors `collect_options
/// _from_fn_body` and `collect_vectors_from_fn_body`. Also walks
/// expressions to catch builtin calls like `String.chars` whose
/// return type (`List<String>`) only appears as a stdlib signature,
/// plus unannotated list literals whose type only exists as a stamp.
fn collect_lists_from_fn_body(
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
            collect_lists_from_str(&annot.display(), out, order, next_idx);
        }
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => e,
        };
        collect_lists_from_expr(expr, out, order, next_idx);
    }
}

fn type_is_concrete_for_discovery(ty: &Type) -> bool {
    match ty {
        Type::Var(_) | Type::Invalid => false,
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit | Type::Named { .. } => true,
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            type_is_concrete_for_discovery(inner)
        }
        Type::Result(ok, err) => {
            type_is_concrete_for_discovery(ok) && type_is_concrete_for_discovery(err)
        }
        Type::Map(key, value) => {
            type_is_concrete_for_discovery(key) && type_is_concrete_for_discovery(value)
        }
        Type::Tuple(items) => items.iter().all(type_is_concrete_for_discovery),
        Type::Fn(params, ret, _) => {
            params.iter().all(type_is_concrete_for_discovery) && type_is_concrete_for_discovery(ret)
        }
    }
}

fn collect_lists_from_expr(
    expr: &crate::ast::Spanned<crate::ir::hir::ResolvedExpr>,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedStrPart};
    match &expr.node {
        ResolvedExpr::Call(callee, args) => {
            // `String.chars(s)` returns `List<String>` — register it
            // eagerly here since the canonical never appears in fn
            // signatures by itself.
            if let ResolvedCallee::Builtin(name) = callee
                && name == "String.chars"
            {
                let canonical = "List<String>".to_string();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical);
                    *next_idx += 1;
                }
            }
            if let ResolvedCallee::Unresolved { callee } = callee {
                collect_lists_from_expr(callee, out, order, next_idx);
            }
            for a in args {
                collect_lists_from_expr(a, out, order, next_idx);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            collect_lists_from_expr(l, out, order, next_idx);
            collect_lists_from_expr(r, out, order, next_idx);
        }
        ResolvedExpr::Neg(inner) => collect_lists_from_expr(inner, out, order, next_idx),
        ResolvedExpr::Match { subject, arms } => {
            collect_lists_from_expr(subject, out, order, next_idx);
            for arm in arms {
                collect_lists_from_expr(&arm.body, out, order, next_idx);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_lists_from_expr(a, out, order, next_idx);
            }
        }
        ResolvedExpr::Attr(obj, _) => collect_lists_from_expr(obj, out, order, next_idx),
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_lists_from_expr(e, out, order, next_idx);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_lists_from_expr(base, out, order, next_idx);
            for (_, e) in updates {
                collect_lists_from_expr(e, out, order, next_idx);
            }
        }
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_lists_from_expr(a, out, order, next_idx);
            }
        }
        ResolvedExpr::Tuple(items) | ResolvedExpr::IndependentProduct(items, _) => {
            for it in items {
                collect_lists_from_expr(it, out, order, next_idx);
            }
        }
        ResolvedExpr::ErrorProp(inner) => collect_lists_from_expr(inner, out, order, next_idx),
        ResolvedExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let ResolvedStrPart::Parsed(inner) = part {
                    collect_lists_from_expr(inner, out, order, next_idx);
                }
            }
        }
        ResolvedExpr::List(items) => {
            if let Some(ty) = expr.ty()
                && matches!(ty, Type::List(_))
                && type_is_concrete_for_discovery(ty)
            {
                collect_lists_from_str(&ty.display(), out, order, next_idx);
            } else if let Some(elem_ty) = items.first().and_then(|item| item.ty())
                && type_is_concrete_for_discovery(elem_ty)
            {
                collect_lists_from_str(
                    &format!("List<{}>", elem_ty.display()),
                    out,
                    order,
                    next_idx,
                );
            }
            for x in items {
                collect_lists_from_expr(x, out, order, next_idx);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_lists_from_expr(key, out, order, next_idx);
                collect_lists_from_expr(value, out, order, next_idx);
            }
        }
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
    }
}

/// Convert a Tuple type string in any Aver form to the internal
/// canonical `Tuple<A,B>` (whitespace-stripped). Accepts `(A, B)`
/// (surface syntax that the type checker emits) or already-canonical
/// `Tuple<A,B>`.
fn normalize_tuple_canonical(s: &str) -> std::borrow::Cow<'_, str> {
    let normalized = normalize_compound(s);
    if normalized == s {
        std::borrow::Cow::Borrowed(s)
    } else {
        std::borrow::Cow::Owned(normalized)
    }
}

/// Recursively rewrite all `(A, B)` substrings inside a type string
/// to `Tuple<A,B>` and strip whitespace. Idempotent on already-
/// canonical `Tuple<...>`. Used wherever a stable canonical form
/// must be derived: discovery, registry lookup, eager registration.
pub(super) fn normalize_compound(s: &str) -> String {
    let stripped: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    rewrite_paren_tuples(&stripped)
}

/// Strip module-qualifier dots from inner type-name tokens. Walks the
/// type string and rewrites `Module.Name` to `Name` whenever `Name`
/// follows a `<` / `,` (i.e. it's an inner type argument). Used when
/// the registry lookup for the qualified form misses — same record
/// type may be referenced through `Module.Room` from one fn and
/// through bare `Room` from another after multi-module flatten.
pub(super) fn strip_inner_dotted_prefixes(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while i < bytes.len() {
        out.push(bytes[i] as char);
        // After a token boundary, look for `Capital.Name` and skip
        // the `Capital.` part.
        if matches!(bytes[i], b'<' | b',' | b'(') {
            // Try to find a dotted Capital identifier: chars consisting
            // of letters/digits/underscores then `.` then more chars.
            let start = i + 1;
            let mut j = start;
            while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
                j += 1;
            }
            if j > start && j < bytes.len() && bytes[j] == b'.' && bytes[start].is_ascii_uppercase()
            {
                let after_dot = j + 1;
                let mut k = after_dot;
                while k < bytes.len() && (bytes[k].is_ascii_alphanumeric() || bytes[k] == b'_') {
                    k += 1;
                }
                if k > after_dot && bytes[after_dot].is_ascii_uppercase() {
                    // Emit the bare suffix, skip the prefix + dot.
                    out.push_str(&s[after_dot..k]);
                    i = k;
                    continue;
                }
            }
        }
        i += 1;
    }
    out
}

fn rewrite_paren_tuples(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while i < bytes.len() {
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
                let inner = &s[i + 1..j - 1];
                let inner_normalized = rewrite_paren_tuples(inner);
                out.push_str("Tuple<");
                out.push_str(&inner_normalized);
                out.push('>');
                i = j;
                continue;
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}
