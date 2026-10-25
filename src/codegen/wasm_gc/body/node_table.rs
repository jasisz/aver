//! The table a comparison or a hash in steps remembers nodes in once its
//! budget is spent (`eq_helpers`, `hash_helpers`).
//!
//! Wasm-gc gives a node no address to key a table on, so an entry is keyed
//! by what the node holds that a step can read at once: its constructor and
//! its `Int`, `Float`, `Bool` and carrier fields, with the length of each
//! `String`. A node is found by `ref.eq` among the entries of its key, so a
//! key two nodes share costs a probe, never a wrong answer. A key spreads as
//! well as those fields tell nodes apart: a node that carries its own
//! structural hash, as a term of a proof checker does, has a key of its own.
//! The entries of one key sit within [`PROBES`] slots of where it falls, and
//! a key whose slots are all taken writes over one of them in turn, so nodes
//! that hold nothing but other nodes share a window of the last [`PROBES`]
//! of their constructor.
//!
//! The table is open addressing over arrays: [`TABLE_START`] slots when the
//! first entry comes, doubled at half full up to [`TABLE_MAX`]. A
//! comparison that starts within another one with memory keeps the table,
//! and the outermost one drops it when it ends.

use wasm_encoder::{
    BlockType, ConstExpr, Function, GlobalSection, GlobalType, HeapType, Instruction, RefType,
    StorageType, TypeSection, ValType,
};

use super::super::types::TypeRegistry;

/// Slots of a table when its first entry comes.
const TABLE_START: i32 = 1 << 10;
/// Slots a table grows to at most.
const TABLE_MAX: i32 = 1 << 22;
/// Slots a lookup or an insert tries from where a key falls.
const PROBES: i32 = 8;

/// What an entry of a table holds besides its key.
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

    /// The params of the insert and place: the nodes, the key, and for a hash
    /// the hash. The lookup takes the nodes and the key.
    fn params(self) -> Vec<ValType> {
        let mut out = vec![eq_ref(); self.refs() as usize];
        out.push(ValType::I32);
        if self == Entry::Hash {
            out.push(ValType::I32);
        }
        out
    }
}

/// The five functions of a table from `fns` (lookup, insert, place, enter,
/// leave), their types from `types`, then the key and node array types.
#[derive(Clone, Copy)]
pub(crate) struct TableSlots {
    entry: Entry,
    fns: u32,
    types: u32,
}

/// The globals of a table from `first`: keys, nodes, for a hash the hashes,
/// then the count of entries, the depth of comparisons with memory under
/// way, and the turn of the next slot written over.
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
        *next_fn_idx += 5;
        *next_type_idx += 7;
        slots
    }

    pub(crate) fn lookup(&self) -> u32 {
        self.fns
    }

    pub(crate) fn insert(&self) -> u32 {
        self.fns + 1
    }

    fn place(&self) -> u32 {
        self.fns + 2
    }

    pub(crate) fn enter(&self) -> u32 {
        self.fns + 3
    }

    pub(crate) fn leave(&self) -> u32 {
        self.fns + 4
    }

    fn keys_type(&self) -> u32 {
        self.types + 5
    }

    fn nodes_type(&self) -> u32 {
        self.types + 6
    }

    /// The function types of the five functions, in function order.
    pub(crate) fn function_types(&self) -> [u32; 5] {
        [0, 1, 2, 3, 4].map(|i| self.types + i)
    }

    pub(crate) fn emit_types(&self, types: &mut TypeSection) {
        let params = self.entry.params();
        let found = match self.entry {
            Entry::Pair => ValType::I32,
            Entry::Hash => ValType::I64,
        };
        types
            .ty()
            .function(params[..=self.entry.refs() as usize].to_vec(), [found]);
        types.ty().function(params.clone(), []);
        types.ty().function(params, []);
        types.ty().function([], []);
        types.ty().function([], []);
        types.ty().array(&StorageType::Val(ValType::I32), true);
        types.ty().array(&StorageType::Val(eq_ref()), true);
    }

    pub(crate) fn emit_globals(
        &self,
        globals: &mut GlobalSection,
        next_global_idx: &mut u32,
    ) -> TableGlobals {
        let first = *next_global_idx;
        let array = |idx: u32| {
            (
                GlobalType {
                    val_type: array_ref(idx),
                    mutable: true,
                    shared: false,
                },
                ConstExpr::ref_null(HeapType::Concrete(idx)),
            )
        };
        let mut arrays = vec![array(self.keys_type()), array(self.nodes_type())];
        if self.entry == Entry::Hash {
            arrays.push(array(self.keys_type()));
        }
        for (ty, init) in &arrays {
            globals.global(*ty, init);
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

    /// The bodies of the five functions, in function order.
    pub(crate) fn emit_bodies(&self, globals: &TableGlobals) -> [Function; 5] {
        [
            self.emit_lookup(globals),
            self.emit_insert(globals),
            self.emit_place(globals),
            self.emit_enter(globals),
            self.emit_leave(globals),
        ]
    }

    fn g_keys(&self, g: &TableGlobals) -> u32 {
        g.first
    }

    fn g_nodes(&self, g: &TableGlobals) -> u32 {
        g.first + 1
    }

    fn g_hashes(&self, g: &TableGlobals) -> u32 {
        g.first + 2
    }

    fn g_count(&self, g: &TableGlobals) -> u32 {
        g.first + 2 + u32::from(self.entry == Entry::Hash)
    }

    fn g_depth(&self, g: &TableGlobals) -> u32 {
        self.g_count(g) + 1
    }

    fn g_turn(&self, g: &TableGlobals) -> u32 {
        self.g_count(g) + 2
    }

    /// Pushes the index of node `r` of slot `slot` in the node array.
    fn node_index(&self, f: &mut Function, slot: u32, r: u32) {
        f.instruction(&Instruction::LocalGet(slot));
        if self.entry.refs() == 2 {
            emit(f, &[Instruction::I32Const(1), Instruction::I32Shl]);
        }
        if r > 0 {
            emit(f, &[Instruction::I32Const(r as i32), Instruction::I32Add]);
        }
    }

    /// Sets local `slot` to where `key` (a local) falls in a table of
    /// `mask + 1` slots.
    fn home(f: &mut Function, key: u32, mask: u32, slot: u32) {
        emit(
            f,
            &[
                Instruction::LocalGet(key),
                Instruction::I32Const(1),
                Instruction::I32ShrU,
                Instruction::LocalGet(mask),
                Instruction::I32And,
                Instruction::LocalSet(slot),
            ],
        );
    }

    /// `lookup(nodes.., key)`: 1 for a remembered pair and 0 otherwise; for
    /// a hash its remembered hash, or -1.
    fn emit_lookup(&self, g: &TableGlobals) -> Function {
        let refs = self.entry.refs();
        let key = refs;
        // Locals: keys, nodes, mask, slot, probes left, the key read.
        let (keys, nodes, mask, slot, left, read) =
            (key + 1, key + 2, key + 3, key + 4, key + 5, key + 6);
        let mut f = Function::new(vec![
            (1, array_ref(self.keys_type())),
            (1, array_ref(self.nodes_type())),
            (4, ValType::I32),
        ]);
        let missing = match self.entry {
            Entry::Pair => Instruction::I32Const(0),
            Entry::Hash => Instruction::I64Const(-1),
        };
        emit(
            &mut f,
            &[
                Instruction::GlobalGet(self.g_keys(g)),
                Instruction::LocalTee(keys),
                Instruction::RefIsNull,
                Instruction::If(BlockType::Empty),
                missing.clone(),
                Instruction::Return,
                Instruction::End,
                Instruction::GlobalGet(self.g_nodes(g)),
                Instruction::LocalSet(nodes),
                Instruction::LocalGet(keys),
                Instruction::ArrayLen,
                Instruction::I32Const(1),
                Instruction::I32Sub,
                Instruction::LocalSet(mask),
            ],
        );
        Self::home(&mut f, key, mask, slot);
        emit(
            &mut f,
            &[
                Instruction::I32Const(PROBES),
                Instruction::LocalSet(left),
                Instruction::Loop(BlockType::Empty),
                Instruction::LocalGet(keys),
                Instruction::LocalGet(slot),
                Instruction::ArrayGet(self.keys_type()),
                Instruction::LocalTee(read),
                Instruction::I32Eqz,
                Instruction::If(BlockType::Empty),
                missing.clone(),
                Instruction::Return,
                Instruction::End,
                Instruction::LocalGet(read),
                Instruction::LocalGet(key),
                Instruction::I32Eq,
                Instruction::If(BlockType::Empty),
            ],
        );
        for r in 0..refs {
            f.instruction(&Instruction::LocalGet(nodes));
            self.node_index(&mut f, slot, r);
            emit(
                &mut f,
                &[
                    Instruction::ArrayGet(self.nodes_type()),
                    Instruction::LocalGet(r),
                    Instruction::RefEq,
                ],
            );
            if r > 0 {
                f.instruction(&Instruction::I32And);
            }
        }
        f.instruction(&Instruction::If(BlockType::Empty));
        match self.entry {
            Entry::Pair => {
                f.instruction(&Instruction::I32Const(1));
            }
            Entry::Hash => emit(
                &mut f,
                &[
                    Instruction::GlobalGet(self.g_hashes(g)),
                    Instruction::LocalGet(slot),
                    Instruction::ArrayGet(self.keys_type()),
                    Instruction::I64ExtendI32U,
                ],
            ),
        };
        emit(
            &mut f,
            &[
                Instruction::Return,
                Instruction::End,
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

    /// Pushes every param of the insert or place.
    fn forward(&self, f: &mut Function) {
        for p in 0..self.entry.params().len() as u32 {
            f.instruction(&Instruction::LocalGet(p));
        }
    }

    /// `insert(..)`: makes the table on the first entry and doubles it at
    /// half full, then places the entry.
    fn emit_insert(&self, g: &TableGlobals) -> Function {
        let refs = self.entry.refs();
        let params = self.entry.params().len() as u32;
        // Locals: the old keys, nodes and hashes, the old size, the slot.
        let (old_keys, old_nodes, old_hashes, size, slot, read) = (
            params,
            params + 1,
            params + 2,
            params + 3,
            params + 4,
            params + 5,
        );
        let mut f = Function::new(vec![
            (1, array_ref(self.keys_type())),
            (1, array_ref(self.nodes_type())),
            (1, array_ref(self.keys_type())),
            (3, ValType::I32),
        ]);
        let new_arrays = |f: &mut Function, size: &dyn Fn(&mut Function)| {
            size(f);
            emit(
                f,
                &[
                    Instruction::ArrayNewDefault(self.keys_type()),
                    Instruction::GlobalSet(self.g_keys(g)),
                ],
            );
            size(f);
            if refs == 2 {
                emit(f, &[Instruction::I32Const(1), Instruction::I32Shl]);
            }
            emit(
                f,
                &[
                    Instruction::ArrayNewDefault(self.nodes_type()),
                    Instruction::GlobalSet(self.g_nodes(g)),
                ],
            );
            if self.entry == Entry::Hash {
                size(f);
                emit(
                    f,
                    &[
                        Instruction::ArrayNewDefault(self.keys_type()),
                        Instruction::GlobalSet(self.g_hashes(g)),
                    ],
                );
            }
            emit(
                f,
                &[
                    Instruction::I32Const(0),
                    Instruction::GlobalSet(self.g_count(g)),
                ],
            );
        };
        emit(
            &mut f,
            &[
                Instruction::GlobalGet(self.g_keys(g)),
                Instruction::RefIsNull,
                Instruction::If(BlockType::Empty),
            ],
        );
        new_arrays(&mut f, &|f| {
            f.instruction(&Instruction::I32Const(TABLE_START));
        });
        emit(
            &mut f,
            &[
                Instruction::Else,
                Instruction::GlobalGet(self.g_count(g)),
                Instruction::I32Const(1),
                Instruction::I32Shl,
                Instruction::GlobalGet(self.g_keys(g)),
                Instruction::ArrayLen,
                Instruction::LocalTee(size),
                Instruction::I32GeU,
                Instruction::LocalGet(size),
                Instruction::I32Const(TABLE_MAX),
                Instruction::I32LtU,
                Instruction::I32And,
                Instruction::If(BlockType::Empty),
                Instruction::GlobalGet(self.g_keys(g)),
                Instruction::LocalSet(old_keys),
                Instruction::GlobalGet(self.g_nodes(g)),
                Instruction::LocalSet(old_nodes),
            ],
        );
        if self.entry == Entry::Hash {
            emit(
                &mut f,
                &[
                    Instruction::GlobalGet(self.g_hashes(g)),
                    Instruction::LocalSet(old_hashes),
                ],
            );
        }
        new_arrays(&mut f, &|f| {
            emit(
                f,
                &[
                    Instruction::LocalGet(size),
                    Instruction::I32Const(1),
                    Instruction::I32Shl,
                ],
            );
        });
        emit(
            &mut f,
            &[
                Instruction::I32Const(0),
                Instruction::LocalSet(slot),
                Instruction::Loop(BlockType::Empty),
                Instruction::LocalGet(old_keys),
                Instruction::LocalGet(slot),
                Instruction::ArrayGet(self.keys_type()),
                Instruction::LocalTee(read),
                Instruction::If(BlockType::Empty),
            ],
        );
        for r in 0..refs {
            f.instruction(&Instruction::LocalGet(old_nodes));
            self.node_index(&mut f, slot, r);
            f.instruction(&Instruction::ArrayGet(self.nodes_type()));
        }
        f.instruction(&Instruction::LocalGet(read));
        if self.entry == Entry::Hash {
            emit(
                &mut f,
                &[
                    Instruction::LocalGet(old_hashes),
                    Instruction::LocalGet(slot),
                    Instruction::ArrayGet(self.keys_type()),
                ],
            );
        }
        emit(
            &mut f,
            &[
                Instruction::Call(self.place()),
                Instruction::End,
                Instruction::LocalGet(slot),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::LocalTee(slot),
                Instruction::LocalGet(size),
                Instruction::I32LtU,
                Instruction::BrIf(0),
                Instruction::End,
                Instruction::End,
                Instruction::End,
            ],
        );
        self.forward(&mut f);
        emit(&mut f, &[Instruction::Call(self.place()), Instruction::End]);
        f
    }

    /// `place(..)`: writes the entry in the first free slot within reach of
    /// its key, or over one of them in turn.
    fn emit_place(&self, g: &TableGlobals) -> Function {
        let refs = self.entry.refs();
        let key = refs;
        let params = self.entry.params().len() as u32;
        // Locals: keys, mask, slot, probes left.
        let (keys, mask, slot, left) = (params, params + 1, params + 2, params + 3);
        let mut f = Function::new(vec![(1, array_ref(self.keys_type())), (3, ValType::I32)]);
        emit(
            &mut f,
            &[
                Instruction::GlobalGet(self.g_keys(g)),
                Instruction::LocalTee(keys),
                Instruction::ArrayLen,
                Instruction::I32Const(1),
                Instruction::I32Sub,
                Instruction::LocalSet(mask),
            ],
        );
        Self::home(&mut f, key, mask, slot);
        emit(
            &mut f,
            &[
                Instruction::I32Const(PROBES),
                Instruction::LocalSet(left),
                Instruction::Block(BlockType::Empty),
                Instruction::Loop(BlockType::Empty),
                Instruction::LocalGet(keys),
                Instruction::LocalGet(slot),
                Instruction::ArrayGet(self.keys_type()),
                Instruction::I32Eqz,
                Instruction::If(BlockType::Empty),
                Instruction::GlobalGet(self.g_count(g)),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::GlobalSet(self.g_count(g)),
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
                Instruction::GlobalGet(self.g_turn(g)),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::GlobalSet(self.g_turn(g)),
                Instruction::GlobalGet(self.g_turn(g)),
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
                Instruction::LocalGet(keys),
                Instruction::LocalGet(slot),
                Instruction::LocalGet(key),
                Instruction::ArraySet(self.keys_type()),
            ],
        );
        for r in 0..refs {
            f.instruction(&Instruction::GlobalGet(self.g_nodes(g)));
            self.node_index(&mut f, slot, r);
            emit(
                &mut f,
                &[
                    Instruction::LocalGet(r),
                    Instruction::ArraySet(self.nodes_type()),
                ],
            );
        }
        if self.entry == Entry::Hash {
            emit(
                &mut f,
                &[
                    Instruction::GlobalGet(self.g_hashes(g)),
                    Instruction::LocalGet(slot),
                    Instruction::LocalGet(key + 1),
                    Instruction::ArraySet(self.keys_type()),
                ],
            );
        }
        f.instruction(&Instruction::End);
        f
    }

    fn emit_enter(&self, g: &TableGlobals) -> Function {
        let mut f = Function::new(Vec::new());
        emit(
            &mut f,
            &[
                Instruction::GlobalGet(self.g_depth(g)),
                Instruction::I32Const(1),
                Instruction::I32Add,
                Instruction::GlobalSet(self.g_depth(g)),
                Instruction::End,
            ],
        );
        f
    }

    /// Ends a comparison with memory; the outermost drops the table.
    fn emit_leave(&self, g: &TableGlobals) -> Function {
        let mut f = Function::new(Vec::new());
        emit(
            &mut f,
            &[
                Instruction::GlobalGet(self.g_depth(g)),
                Instruction::I32Const(1),
                Instruction::I32Sub,
                Instruction::GlobalSet(self.g_depth(g)),
                Instruction::GlobalGet(self.g_depth(g)),
                Instruction::I32Eqz,
                Instruction::If(BlockType::Empty),
                Instruction::RefNull(HeapType::Concrete(self.keys_type())),
                Instruction::GlobalSet(self.g_keys(g)),
                Instruction::RefNull(HeapType::Concrete(self.nodes_type())),
                Instruction::GlobalSet(self.g_nodes(g)),
            ],
        );
        if self.entry == Entry::Hash {
            emit(
                &mut f,
                &[
                    Instruction::RefNull(HeapType::Concrete(self.keys_type())),
                    Instruction::GlobalSet(self.g_hashes(g)),
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
