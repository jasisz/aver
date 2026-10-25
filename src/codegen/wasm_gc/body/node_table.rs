//! The table a comparison or a hash in steps remembers nodes in once its
//! budget is spent (`eq_helpers`, `hash_helpers`).
//!
//! Wasm-gc gives a node no address to key a table on, so an entry falls
//! where the key of its node says: the node's constructor mixed with its
//! `Int`, `Float`, `Bool` and carrier fields and the length of each of its
//! `String`s, all a step reads at once. A node is found by `ref.eq` among the
//! entries within [`PROBES`] slots of its key, so a key two nodes share costs
//! a probe, never a wrong answer, and a key whose slots are all taken writes
//! over one of them in turn. A key spreads as well as those fields tell nodes
//! apart: a node that carries its own structural hash, as a term of a proof
//! checker does, falls where no other node falls, and nodes that hold nothing
//! but other nodes share the last [`PROBES`] of their constructor.
//!
//! The table is an array of nodes: [`TABLE_START`] entries when the first
//! one comes, twice as many, empty, whenever it is half full, up to
//! [`TABLE_MAX`], at which size it starts again empty when half full. So it
//! remembers every pair, half its largest size at a time, and a walk over a
//! large value that shares nothing pays a table that stays small rather than
//! one entry for every node it meets. A comparison that
//! starts within another one with memory keeps the table, and the outermost
//! one drops it when it ends.

use wasm_encoder::{
    ConstExpr, Function, GlobalSection, GlobalType, HeapType, Instruction, RefType, StorageType,
    TypeSection, ValType,
};

use super::super::types::TypeRegistry;
use super::super::{WasmGcError, wat_helper};

/// Entries of a table when its first one comes.
const TABLE_START: i32 = 1 << 10;
/// Entries a table grows to at most.
const TABLE_MAX: i32 = 1 << 15;
/// Slots a lookup or an insert tries from where a key falls.
const PROBES: i32 = 8;

/// What an entry of a table holds.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Entry {
    /// Two nodes found equal (`==`).
    Pair,
    /// One node and its hash.
    Hash,
}

impl Entry {
    fn refs(self) -> u32 {
        match self {
            Entry::Pair => 2,
            Entry::Hash => 1,
        }
    }

    fn hashes(self) -> bool {
        self == Entry::Hash
    }
}

/// The four functions of a table from `fns` (lookup, insert, enter, leave),
/// their types from `types`, then the node array type and for a hash the
/// hash array type.
#[derive(Clone, Copy)]
pub(crate) struct TableSlots {
    entry: Entry,
    fns: u32,
    types: u32,
}

/// The globals of a table from `first`: the nodes, for a hash the hashes,
/// then the count of entries, the depth of comparisons with memory under way
/// and the turn of the next slot written over.
#[derive(Clone, Copy)]
pub(crate) struct TableGlobals {
    first: u32,
}

fn eq_ref() -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Abstract {
            shared: false,
            ty: wasm_encoder::AbstractHeapType::Eq,
        },
    })
}

fn array_ref(idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(idx),
    })
}

fn emit(f: &mut Function, ops: &[Instruction]) {
    for op in ops {
        f.instruction(op);
    }
}

impl TableSlots {
    pub(crate) fn assign(entry: Entry, next_fn_idx: &mut u32, next_type_idx: &mut u32) -> Self {
        let slots = TableSlots {
            entry,
            fns: *next_fn_idx,
            types: *next_type_idx,
        };
        *next_fn_idx += 4;
        *next_type_idx += 5 + u32::from(entry.hashes());
        slots
    }

    pub(crate) fn lookup(&self) -> u32 {
        self.fns
    }

    pub(crate) fn insert(&self) -> u32 {
        self.fns + 1
    }

    pub(crate) fn enter(&self) -> u32 {
        self.fns + 2
    }

    pub(crate) fn leave(&self) -> u32 {
        self.fns + 3
    }

    fn nodes_type(&self) -> u32 {
        self.types + 4
    }

    fn hashes_type(&self) -> u32 {
        self.types + 5
    }

    /// The function types of the four functions, in function order.
    pub(crate) fn function_types(&self) -> [u32; 4] {
        [0, 1, 2, 3].map(|i| self.types + i)
    }

    pub(crate) fn emit_types(&self, types: &mut TypeSection) {
        // The lookup takes the nodes and the key, the insert for a hash the
        // hash too.
        let lookup = [
            vec![eq_ref(); self.entry.refs() as usize],
            vec![ValType::I32],
        ]
        .concat();
        let found = match self.entry {
            Entry::Pair => ValType::I32,
            Entry::Hash => ValType::I64,
        };
        let mut insert = lookup.clone();
        if self.entry.hashes() {
            insert.push(ValType::I32);
        }
        types.ty().function(lookup, [found]);
        types.ty().function(insert, []);
        types.ty().function([], []);
        types.ty().function([], []);
        types.ty().array(&StorageType::Val(eq_ref()), true);
        if self.entry.hashes() {
            types.ty().array(&StorageType::Val(ValType::I32), true);
        }
    }

    pub(crate) fn emit_globals(
        &self,
        globals: &mut GlobalSection,
        next_global_idx: &mut u32,
    ) -> TableGlobals {
        let first = *next_global_idx;
        let mut arrays = vec![self.nodes_type()];
        if self.entry.hashes() {
            arrays.push(self.hashes_type());
        }
        for idx in &arrays {
            let ty = GlobalType {
                val_type: array_ref(*idx),
                mutable: true,
                shared: false,
            };
            globals.global(ty, &ConstExpr::ref_null(HeapType::Concrete(*idx)));
        }
        let int = GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        };
        for _ in 0..3 {
            globals.global(int, &ConstExpr::i32_const(0));
        }
        *next_global_idx += arrays.len() as u32 + 3;
        TableGlobals { first }
    }

    /// The bodies of the four functions, in function order.
    pub(crate) fn emit_bodies(&self, g: &TableGlobals) -> Result<[Function; 4], WasmGcError> {
        let pair = self.entry == Entry::Pair;
        // A slot holds two nodes of a pair, so its first is at twice its index.
        let first = if pair { "i32.const 1 i32.shl" } else { "" };
        let nodes_param = if pair {
            "(param $a eqref) (param $b eqref)"
        } else {
            "(param $a eqref)"
        };
        let (found_ty, missing, found, second) = if pair {
            (
                "i32",
                "i32.const 0",
                "i32.const 1",
                "local.get $nodes local.get $slot i32.const 1 i32.shl i32.const 1 i32.add
                 array.get $nodes local.get $b ref.eq i32.and",
            )
        } else {
            (
                "i64",
                "i64.const -1",
                "global.get $hashes local.get $slot array.get $hashes i64.extend_i32_u",
                "",
            )
        };
        let locals = "(local $nodes (ref null $nodes)) (local $mask i32) (local $slot i32)
                      (local $left i32)";
        // Sets $mask to the slots less one and $slot to where $key falls.
        let home = format!(
            "local.get $nodes array.len i32.const {shift} i32.shr_u i32.const 1 i32.sub
             local.set $mask
             local.get $key i32.const 1 i32.shr_u local.get $mask i32.and local.set $slot
             i32.const {PROBES} local.set $left",
            shift = u32::from(pair),
        );
        let lookup = format!(
            r#"{nodes_param} (param $key i32) (result {found_ty}) {locals} (local $first eqref)
            global.get $nodes local.tee $nodes ref.is_null
            (if (then {missing} return))
            {home}
            (loop $probe
              local.get $nodes local.get $slot {first} array.get $nodes local.tee $first
              ref.is_null (if (then {missing} return))
              local.get $first local.get $a ref.eq {second}
              (if (then {found} return))
              local.get $left i32.const 1 i32.sub local.tee $left
              (if (then
                    local.get $slot i32.const 1 i32.add local.get $mask i32.and local.set $slot
                    br $probe)))
            {missing}"#
        );
        // Makes `slots` (pushed by `slots`) empty entries.
        let new_arrays = |slots: &str| {
            let mut out = format!(
                "{slots} {double} array.new_default $nodes global.set $nodes",
                double = if pair { "i32.const 1 i32.shl" } else { "" },
            );
            if !pair {
                out += &format!(" {slots} array.new_default $hashes global.set $hashes");
            }
            out
        };
        let insert = format!(
            r#"{nodes_param} (param $key i32) {hash_param} {locals}
            global.get $nodes ref.is_null
            (if (then {start} i32.const 0 global.set $count)
              (else
                ;; Half full: twice the size up to the largest, empty either way.
                global.get $nodes array.len i32.const {half} i32.shr_u local.tee $mask
                global.get $count i32.le_u
                (if (then
                      local.get $mask i32.const {max_half} i32.lt_u
                      (if (then {grow})
                        (else
                          global.get $nodes i32.const 0 ref.null eq
                          global.get $nodes array.len array.fill $nodes))
                      i32.const 0 global.set $count))))
            global.get $nodes local.set $nodes
            {home}
            (block $free
              (loop $probe
                local.get $nodes local.get $slot {first} array.get $nodes ref.is_null
                (if (then global.get $count i32.const 1 i32.add global.set $count br $free))
                local.get $left i32.const 1 i32.sub local.tee $left
                (if (then
                      local.get $slot i32.const 1 i32.add local.get $mask i32.and local.set $slot
                      br $probe)))
              ;; Every slot within reach is taken: write over one in turn.
              global.get $turn i32.const 1 i32.add global.set $turn
              global.get $turn i32.const {window} i32.and
              local.get $key i32.const 1 i32.shr_u i32.add local.get $mask i32.and
              local.set $slot)
            local.get $nodes local.get $slot {first} local.get $a array.set $nodes
            {store}"#,
            hash_param = if pair { "" } else { "(param $hash i32)" },
            start = new_arrays(&format!("i32.const {TABLE_START}")),
            half = 1 + u32::from(pair),
            max_half = TABLE_MAX / 2,
            grow = new_arrays("local.get $mask i32.const 2 i32.shl"),
            window = PROBES - 1,
            store = if pair {
                "local.get $nodes local.get $slot i32.const 1 i32.shl i32.const 1 i32.add
                 local.get $b array.set $nodes"
            } else {
                "global.get $hashes local.get $slot local.get $hash array.set $hashes"
            },
        );
        let enter = "global.get $depth i32.const 1 i32.add global.set $depth";
        // The outermost comparison with memory drops the table as it ends.
        let leave = format!(
            "global.get $depth i32.const 1 i32.sub global.set $depth
             global.get $depth i32.eqz
             (if (then ref.null $nodes global.set $nodes {hashes} i32.const 0 global.set $count))",
            hashes = if pair {
                ""
            } else {
                "ref.null $hashes global.set $hashes"
            },
        );
        let module = |body: &str| {
            let mut wat = format!(
                "(module\n{}(type $nodes (array (mut eqref)))\n",
                wat_helper::padding_types(self.nodes_type())
            );
            if !pair {
                wat += "(type $hashes (array (mut i32)))\n";
            }
            for _ in 0..g.first {
                wat += "(global (mut i32) (i32.const 0))\n";
            }
            wat += "(global $nodes (mut (ref null $nodes)) (ref.null $nodes))\n";
            if !pair {
                wat += "(global $hashes (mut (ref null $hashes)) (ref.null $hashes))\n";
            }
            for name in ["count", "depth", "turn"] {
                wat += &format!("(global ${name} (mut i32) (i32.const 0))\n");
            }
            wat_helper::compile_wat_helper(&format!("{wat}(func (export \"helper\") {body}))"))
        };
        Ok([
            module(&lookup)?,
            module(&insert)?,
            module(enter)?,
            module(&leave)?,
        ])
    }
}

/// Whether a node of `owner` with fields of `field_tys` holds the walk: one
/// of them is a type that walks, compared or hashed by a step.
pub(crate) fn holds_walk(
    field_tys: &[String],
    owner: &str,
    walked: &std::collections::HashSet<String>,
    registry: &TypeRegistry,
) -> bool {
    field_tys.iter().any(|ty| {
        let declared = ty.trim();
        if registry.is_eligible_carrier(declared) {
            return false;
        }
        let resolved = registry.newtype_underlying(declared).unwrap_or(declared);
        resolved == owner
            || walked.contains(resolved)
            || walked.contains(super::super::types::normalize_compound(resolved).as_str())
    })
}

/// Sets local `key` to the key of a node of the struct `struct_idx`, whose
/// fields are of `fields`, pushed by `node` as a typed reference. The key
/// mixes the struct with every field a step reads at once; it is never 0.
pub(crate) fn emit_node_key(
    f: &mut Function,
    struct_idx: u32,
    fields: &[ValType],
    registry: &TypeRegistry,
    node: &dyn Fn(&mut Function),
    key: u32,
) {
    f.instruction(&Instruction::I32Const(struct_idx as i32));
    for (i, ty) in fields.iter().enumerate() {
        let get = |f: &mut Function| {
            node(f);
            f.instruction(&Instruction::StructGet {
                struct_type_index: struct_idx,
                field_index: i as u32,
            });
        };
        // The high half of a 64-bit field times an odd constant.
        let wide = [
            Instruction::I64Const(0x9e37_79b9_7f4a_7c15_u64 as i64),
            Instruction::I64Mul,
            Instruction::I64Const(32),
            Instruction::I64ShrU,
            Instruction::I32WrapI64,
        ];
        match ty {
            ValType::I32 => get(f),
            ValType::I64 => {
                get(f);
                emit(f, &wide);
            }
            ValType::F64 => {
                get(f);
                f.instruction(&Instruction::I64ReinterpretF64);
                emit(f, &wide);
            }
            ValType::Ref(RefType {
                heap_type: HeapType::Concrete(idx),
                ..
            }) if Some(*idx) == registry.aint_struct_idx => {
                get(f);
                f.instruction(&Instruction::StructGet {
                    struct_type_index: *idx,
                    field_index: 0,
                });
                emit(f, &wide);
            }
            ValType::Ref(RefType {
                heap_type: HeapType::Concrete(idx),
                ..
            }) if Some(*idx) == registry.string_array_type_idx => {
                get(f);
                f.instruction(&Instruction::ArrayLen);
            }
            _ => continue,
        }
        emit(
            f,
            &[
                Instruction::I32Xor,
                Instruction::I32Const(0x0100_0193),
                Instruction::I32Mul,
            ],
        );
    }
    f.instruction(&Instruction::LocalSet(key));
    for (shift, multiplier) in [(16, 0x85eb_ca6b_u32), (13, 0xc2b2_ae35), (16, 1)] {
        emit(
            f,
            &[
                Instruction::LocalGet(key),
                Instruction::LocalGet(key),
                Instruction::I32Const(shift),
                Instruction::I32ShrU,
                Instruction::I32Xor,
            ],
        );
        if multiplier != 1 {
            emit(
                f,
                &[
                    Instruction::I32Const(multiplier as i32),
                    Instruction::I32Mul,
                ],
            );
        }
        f.instruction(&Instruction::LocalSet(key));
    }
    emit(
        f,
        &[
            Instruction::LocalGet(key),
            Instruction::I32Const(1),
            Instruction::I32Or,
            Instruction::LocalSet(key),
        ],
    );
}
