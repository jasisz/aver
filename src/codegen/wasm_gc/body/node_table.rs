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
//! [`TABLE_MAX`], at which size it starts again empty when half full. A
//! walk over a large value that shares nothing so pays a table that stays
//! small, rather than one entry for every node it meets. A comparison that
//! starts within another one with memory keeps the table, and the outermost
//! one drops it when it ends.

use wasm_encoder::{
    BlockType, ConstExpr, Function, GlobalSection, GlobalType, HeapType, Instruction, RefType,
    StorageType, TypeSection, ValType,
};

use super::super::types::TypeRegistry;

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
    pub(crate) fn emit_bodies(&self, g: &TableGlobals) -> [Function; 4] {
        let depth = self.g_count(g) + 1;
        let enter = {
            let mut f = Function::new(Vec::new());
            emit(
                &mut f,
                &[
                    Instruction::GlobalGet(depth),
                    Instruction::I32Const(1),
                    Instruction::I32Add,
                    Instruction::GlobalSet(depth),
                    Instruction::End,
                ],
            );
            f
        };
        // The outermost comparison with memory drops the table as it ends.
        let leave = {
            let mut f = Function::new(Vec::new());
            emit(
                &mut f,
                &[
                    Instruction::GlobalGet(depth),
                    Instruction::I32Const(1),
                    Instruction::I32Sub,
                    Instruction::GlobalSet(depth),
                    Instruction::GlobalGet(depth),
                    Instruction::I32Eqz,
                    Instruction::If(BlockType::Empty),
                    Instruction::RefNull(HeapType::Concrete(self.nodes_type())),
                    Instruction::GlobalSet(g.first),
                ],
            );
            if self.entry.hashes() {
                emit(
                    &mut f,
                    &[
                        Instruction::RefNull(HeapType::Concrete(self.hashes_type())),
                        Instruction::GlobalSet(g.first + 1),
                    ],
                );
            }
            emit(
                &mut f,
                &[
                    Instruction::I32Const(0),
                    Instruction::GlobalSet(self.g_count(g)),
                    Instruction::End,
                    Instruction::End,
                ],
            );
            f
        };
        [self.emit_lookup(g), self.emit_insert(g), enter, leave]
    }

    fn g_count(&self, g: &TableGlobals) -> u32 {
        g.first + 1 + u32::from(self.entry.hashes())
    }

    /// Pushes the index in the node array of node `r` of the slot in local
    /// `slot`.
    fn node_index(&self, f: &mut Function, slot: u32, r: u32) {
        f.instruction(&Instruction::LocalGet(slot));
        if self.entry.refs() == 2 {
            emit(f, &[Instruction::I32Const(1), Instruction::I32Shl]);
        }
        if r > 0 {
            emit(f, &[Instruction::I32Const(r as i32), Instruction::I32Add]);
        }
    }

    /// Sets local `mask` to the slots of the node array in local `nodes`
    /// less one, and local `slot` to where the key in local `key` falls.
    fn home(&self, f: &mut Function, nodes: u32, key: u32, mask: u32, slot: u32) {
        emit(f, &[Instruction::LocalGet(nodes), Instruction::ArrayLen]);
        if self.entry.refs() == 2 {
            emit(f, &[Instruction::I32Const(1), Instruction::I32ShrU]);
        }
        emit(
            f,
            &[
                Instruction::I32Const(1),
                Instruction::I32Sub,
                Instruction::LocalSet(mask),
                Instruction::LocalGet(key),
                Instruction::I32Const(1),
                Instruction::I32ShrU,
                Instruction::LocalGet(mask),
                Instruction::I32And,
                Instruction::LocalSet(slot),
            ],
        );
    }

    /// `lookup(nodes.., key)`: 1 for a remembered pair and 0 otherwise; for a
    /// hash its remembered hash, or -1.
    fn emit_lookup(&self, g: &TableGlobals) -> Function {
        let refs = self.entry.refs();
        let key = refs;
        // Locals: the node array, its mask, the slot, the probes left, the
        // first node of the slot.
        let (nodes, mask, slot, left, first) = (key + 1, key + 2, key + 3, key + 4, key + 5);
        let mut f = Function::new(vec![
            (1, array_ref(self.nodes_type())),
            (3, ValType::I32),
            (1, eq_ref()),
        ]);
        let missing = match self.entry {
            Entry::Pair => Instruction::I32Const(0),
            Entry::Hash => Instruction::I64Const(-1),
        };
        emit(
            &mut f,
            &[
                Instruction::GlobalGet(g.first),
                Instruction::LocalTee(nodes),
                Instruction::RefIsNull,
                Instruction::If(BlockType::Empty),
                missing.clone(),
                Instruction::Return,
                Instruction::End,
            ],
        );
        self.home(&mut f, nodes, key, mask, slot);
        emit(
            &mut f,
            &[
                Instruction::I32Const(PROBES),
                Instruction::LocalSet(left),
                Instruction::Loop(BlockType::Empty),
                Instruction::LocalGet(nodes),
            ],
        );
        self.node_index(&mut f, slot, 0);
        emit(
            &mut f,
            &[
                Instruction::ArrayGet(self.nodes_type()),
                Instruction::LocalTee(first),
                Instruction::RefIsNull,
                Instruction::If(BlockType::Empty),
                missing.clone(),
                Instruction::Return,
                Instruction::End,
                Instruction::LocalGet(first),
                Instruction::LocalGet(0),
                Instruction::RefEq,
            ],
        );
        if refs == 2 {
            f.instruction(&Instruction::LocalGet(nodes));
            self.node_index(&mut f, slot, 1);
            emit(
                &mut f,
                &[
                    Instruction::ArrayGet(self.nodes_type()),
                    Instruction::LocalGet(1),
                    Instruction::RefEq,
                    Instruction::I32And,
                ],
            );
        }
        f.instruction(&Instruction::If(BlockType::Empty));
        match self.entry {
            Entry::Pair => {
                f.instruction(&Instruction::I32Const(1));
            }
            Entry::Hash => emit(
                &mut f,
                &[
                    Instruction::GlobalGet(g.first + 1),
                    Instruction::LocalGet(slot),
                    Instruction::ArrayGet(self.hashes_type()),
                    Instruction::I64ExtendI32U,
                ],
            ),
        }
        emit(
            &mut f,
            &[
                Instruction::Return,
                Instruction::End,
                Instruction::LocalGet(left),
                Instruction::I32Const(1),
                Instruction::I32Sub,
                Instruction::LocalTee(left),
                Instruction::I32Eqz,
                Instruction::If(BlockType::Empty),
                missing,
                Instruction::Return,
                Instruction::End,
                Instruction::LocalGet(slot),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::LocalGet(mask),
                Instruction::I32And,
                Instruction::LocalSet(slot),
                Instruction::Br(0),
                Instruction::End,
                Instruction::Unreachable,
                Instruction::End,
            ],
        );
        f
    }

    /// Sets the arrays to `size` (pushed by `size`) empty entries.
    fn new_arrays(&self, f: &mut Function, g: &TableGlobals, size: &dyn Fn(&mut Function)) {
        size(f);
        if self.entry.refs() == 2 {
            emit(f, &[Instruction::I32Const(1), Instruction::I32Shl]);
        }
        emit(
            f,
            &[
                Instruction::ArrayNewDefault(self.nodes_type()),
                Instruction::GlobalSet(g.first),
            ],
        );
        if self.entry.hashes() {
            size(f);
            emit(
                f,
                &[
                    Instruction::ArrayNewDefault(self.hashes_type()),
                    Instruction::GlobalSet(g.first + 1),
                ],
            );
        }
    }

    /// `insert(nodes.., key, [hash])`: makes the table on the first entry and
    /// a larger, empty one when it is half full, then writes the entry in the
    /// first free slot within reach of its key, or over one of them in turn.
    fn emit_insert(&self, g: &TableGlobals) -> Function {
        let refs = self.entry.refs();
        let key = refs;
        let params = refs + 1 + u32::from(self.entry.hashes());
        // Locals: the node array, its mask, the slot, the probes left.
        let (nodes, mask, slot, left) = (params, params + 1, params + 2, params + 3);
        let mut f = Function::new(vec![(1, array_ref(self.nodes_type())), (3, ValType::I32)]);
        let count = self.g_count(g);
        let turn = count + 2;
        emit(
            &mut f,
            &[
                Instruction::GlobalGet(g.first),
                Instruction::RefIsNull,
                Instruction::If(BlockType::Empty),
            ],
        );
        self.new_arrays(&mut f, g, &|f| {
            f.instruction(&Instruction::I32Const(TABLE_START));
        });
        // Half full: twice the size up to the largest, empty either way.
        emit(
            &mut f,
            &[
                Instruction::Else,
                Instruction::GlobalGet(g.first),
                Instruction::ArrayLen,
            ],
        );
        // Local `mask` holds half the slots for now.
        emit(
            &mut f,
            &[
                Instruction::I32Const(refs as i32),
                Instruction::I32ShrU,
                Instruction::LocalTee(mask),
                Instruction::GlobalGet(count),
                Instruction::I32LeU,
                Instruction::If(BlockType::Empty),
                Instruction::LocalGet(mask),
                Instruction::I32Const(TABLE_MAX / 2),
                Instruction::I32LtU,
                Instruction::If(BlockType::Empty),
            ],
        );
        self.new_arrays(&mut f, g, &|f| {
            emit(
                f,
                &[
                    Instruction::LocalGet(mask),
                    Instruction::I32Const(2),
                    Instruction::I32Shl,
                ],
            );
        });
        emit(
            &mut f,
            &[
                Instruction::Else,
                Instruction::GlobalGet(g.first),
                Instruction::I32Const(0),
                Instruction::RefNull(HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Eq,
                }),
                Instruction::GlobalGet(g.first),
                Instruction::ArrayLen,
                Instruction::ArrayFill(self.nodes_type()),
                Instruction::End,
                Instruction::I32Const(0),
                Instruction::GlobalSet(count),
                Instruction::End,
                Instruction::End,
                Instruction::GlobalGet(g.first),
                Instruction::LocalSet(nodes),
            ],
        );
        self.home(&mut f, nodes, key, mask, slot);
        emit(
            &mut f,
            &[
                Instruction::I32Const(PROBES),
                Instruction::LocalSet(left),
                Instruction::Block(BlockType::Empty),
                Instruction::Loop(BlockType::Empty),
                Instruction::LocalGet(nodes),
            ],
        );
        self.node_index(&mut f, slot, 0);
        emit(
            &mut f,
            &[
                Instruction::ArrayGet(self.nodes_type()),
                Instruction::RefIsNull,
                Instruction::If(BlockType::Empty),
                Instruction::GlobalGet(count),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::GlobalSet(count),
                Instruction::Br(2),
                Instruction::End,
                Instruction::LocalGet(left),
                Instruction::I32Const(1),
                Instruction::I32Sub,
                Instruction::LocalTee(left),
                Instruction::If(BlockType::Empty),
                Instruction::LocalGet(slot),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::LocalGet(mask),
                Instruction::I32And,
                Instruction::LocalSet(slot),
                Instruction::Br(1),
                Instruction::End,
                Instruction::End,
                // Every slot within reach is taken: write over one in turn.
                Instruction::GlobalGet(turn),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::GlobalSet(turn),
                Instruction::GlobalGet(turn),
                Instruction::I32Const(PROBES - 1),
                Instruction::I32And,
                Instruction::LocalGet(key),
                Instruction::I32Const(1),
                Instruction::I32ShrU,
                Instruction::I32Add,
                Instruction::LocalGet(mask),
                Instruction::I32And,
                Instruction::LocalSet(slot),
                Instruction::End,
            ],
        );
        for r in 0..refs {
            f.instruction(&Instruction::LocalGet(nodes));
            self.node_index(&mut f, slot, r);
            emit(
                &mut f,
                &[
                    Instruction::LocalGet(r),
                    Instruction::ArraySet(self.nodes_type()),
                ],
            );
        }
        if self.entry.hashes() {
            emit(
                &mut f,
                &[
                    Instruction::GlobalGet(g.first + 1),
                    Instruction::LocalGet(slot),
                    Instruction::LocalGet(key + 1),
                    Instruction::ArraySet(self.hashes_type()),
                ],
            );
        }
        f.instruction(&Instruction::End);
        f
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
