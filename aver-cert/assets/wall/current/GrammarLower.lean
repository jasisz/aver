/- GrammarLower — the lowering of a `Grammar` plan to wasm-gc, as a port of
   the MIR emitter (`src/codegen/wasm_gc/body/from_mir/**`) for exactly the
   admitted nodes.

   `lowerB` makes the emitter's choices from the same tree, by the same
   predicates, and never from a plan flag:

   * an `Int` comparison with an `Int` literal operand is the bignum
     const-compare tag branch (`emit_mir_numeric_binop`, builtins.rs). The
     literal is looked for on the LEFT first (`const_on_left`, then the
     operator is flipped, `flip_cmp`), then on the right. The other operand
     is RE-EMITTED per read when it is a bare `Local`
     (`aint_const_cmp_operand_is_reemittable`), and otherwise evaluated once
     and stashed in the const-compare scratch local, which is the slot right
     after the resolver slots (`SlotTable::build_for_fn`);
   * an `Int` comparison without a literal calls `__aint_cmp` and compares
     the verdict with `0`, or calls `__aint_eq` (plus `i32.eqz` for `!=`)
     (`emit_aint_binop`);
   * `Bool` `==` / `!=` are `i32.eq` / `i32.ne`; the operand type is read
     from the left operand, as the emitter reads `bop.lhs.ty()`;
   * `TailCall` is `return_call`, `Call` is `call` (the MIR decides tail
     position, the typing checks it);
   * a named `Let` is `value; local.set binding; body` — a single-use copy is
     kept as a local, exactly as the emitter keeps it;
   * `IfThenElse` takes its block type from the then-branch's type;
   * an Int `Match` is the literal cascade (`emit_mir_int_cascade`): the
     subject RE-EMITTED per arm, the literal boxed and compared with
     `__aint_eq`, the catch-all binder stored from one more subject run;
   * a two-arm Bool `Match` is one `if` on the subject;
   * an Option / Result `Match` stashes the subject in the subject scratch,
     `ref.cast`s it and reads the tag field, then extracts the payload
     binder (a dead binder too, `noSlot` skipped) before the arm body;
   * a user-variant `Match` stashes the subject and runs the `ref.test`
     cascade over the arms, the LAST arm untested
     (`emit_mir_variant_dispatch`); binders are extracted by
     `ref.cast` + `struct.get` per field;
   * `withDefault` stashes the carrier and runs the default only in `else`;
   * `Int.min` / `Int.max` evaluate both operands, call `__aint_cmp` and
     compare its verdict with `0` (`<=` / `>=`), then evaluate the picked
     operand again in an `if` of the carrier type;
   * `Construct` pushes the tag, the payload and the default filler of the
     other side, then `struct.new` (`constructors.rs`);
   * a Float comparison is one `f64` instruction; a String literal is
     `array.new_data $string seg` over its data segment; String `+` and an
     interpolation build a `Vector<String>` and call `__wasmgc_concat_n`;
     String `==` / `!=` call `__wasmgc_string_eq` (plus `i32.eqz`);
   * a String `Match` stashes the subject and cascades over the literal arms,
     each `ref.cast (ref null $string)` + literal + `__wasmgc_string_eq`;
   * a tuple destructure stashes the subject and reads each bound component
     with `ref.cast` + `struct.get` (`emit_mir_tuple_match`);
   * `[]` is `ref.null` of the list's cons struct, `List.prepend` is
     `struct.new` of it; a non-empty literal pushes its items in order, then
     `ref.null`, then calls the cons helper once per item, so the last item
     is consed first (`emit_mir_list_literal`);
   * a List `Match` stashes the subject and tests it with `ref.is_null`: the
     `[]` arm in `then`, and in `else` the head (field 0) and tail (field 1)
     binders, each by `ref.cast` + `struct.get`, then the cons arm
     (`emit_mir_list_match`);
   * the fused `Vector.get`-or-default re-reads the vector and the index
     locals, converts the index through `__aint_to_index`, and bounds-checks
     it signed `>= 0` and unsigned `< array.len` before `array.get`;
   * the fused `Result.withDefault(Int.div/mod(a, b), d)` evaluates `a`, `b`
     and `d` once each, parks them in the three operand scratch locals that
     follow the const-compare scratch (`aint_operand_scratch`), tests the
     divisor for zero on the carrier (`$magf` null and `$small == 0`), and
     returns the default or calls `__aint_divmod(a, b, want_mod)`;
   * a Euclidean intrinsic (`IntDivEuclid` / `IntModEuclid`) is its two
     operands, the `want_mod` flag and a call of `__aint_divmod`.

   ONE lowering carries both images: `lowerB` yields instructions whose `if`
   carries its block type. `eraseL` forgets the block types (the audited
   interpreter's `WInstr` tree), and `encBL` writes the bytes. So the
   instructions the simulation theorem runs and the bytes a certificate pins
   come from the same tree by construction. -/
import Grammar
import InterpreterSequencing

namespace AverCert.Grammar
open CertPrelude AverCert.Schema

/-! ## Byte encoders

Canonical LEB128 of the immediates the lowering writes, fail-closed at the
u32 index space and the i32 / i64 constant ranges the binary format admits. -/

/-- Canonical unsigned LEB128 of a u32 index, or `none` outside the range. The
    bytes come from the shared total encoder (`CertPrelude.uleb32Bytes`, exact
    below `2 ^ 35`). -/
def uleb32 (value : Nat) : Option (List Nat) :=
  if value < 4294967296 then some (CertPrelude.uleb32Bytes value) else none

def slebFuel : Nat → Int → Option (List Nat)
  | 0, _ => none
  | fuel + 1, value =>
      let byte := Int.toNat (value % 128)
      let rest := value / 128
      let signSet := 64 ≤ byte
      let done := (rest = 0 ∧ !signSet) ∨ (rest = -1 ∧ signSet)
      let outByte := if done then byte else byte + 128
      if done then
        some [outByte]
      else
        match slebFuel fuel rest with
        | some bytes => some (outByte :: bytes)
        | none => none

def inI32Range (value : Int) : Bool :=
  decide ((-2147483648 : Int) ≤ value) && decide (value ≤ 2147483647)

def inI64Range (value : Int) : Bool :=
  decide ((-9223372036854775808 : Int) ≤ value) && decide (value ≤ 9223372036854775807)

def sleb32 (value : Int) : Option (List Nat) :=
  if inI32Range value then slebFuel 5 value else none

def sleb64 (value : Int) : Option (List Nat) :=
  if inI64Range value then slebFuel 10 value else none

/-- Concrete heap-type indices (inside a reftype `0x63 <ht>`, a block type, or a
    `ref.cast` / `ref.test` / `ref.null` immediate) are SIGNED s33 LEB128: index
    64 is `c0 00`, never `40`. Instruction type indices stay unsigned u32. -/
def s33HeapIdx (idx : Nat) : Option (List Nat) :=
  if idx < 4294967296 then some (CertPrelude.s33Bytes idx) else none

/-! ## Instructions with block types -/

inductive BI where
  | op (i : WInstr)
  | ifElse (bt : Option Ty) (thenB elseB : List BI)
  /-- `ref.null ht`: the interpreter's `refNull`, with its heap type for
      the bytes. -/
  | nullOf (ht : Nat)
  /-- `array.new_data ty seg`: the interpreter's `arrayNewData` carries the
      segment's bytes; the byte image names the segment, whose contents the
      acceptance pins to exactly `bytes`. -/
  | newData (ty seg : Nat) (bytes : List Nat)
  /-- `ref.cast (ref null ht)`: the interpreter's `refCast`, which rejects a
      null the wasm cast would pass (that only makes a run fail). -/
  | castNull (ht : Nat)
  /-- `block bt body … end`: the empty block type (`none`) or one result. -/
  | block (bt : Option Ty) (body : List BI)

mutual
  def eraseI : BI → WInstr
    | .op i => i
    | .ifElse _ t e => .ifElse (eraseL t) (eraseL e)
    | .nullOf _ => .refNull
    | .newData ty _ bytes => .arrayNewData ty bytes
    | .castNull ht => .refCast ht
    | .block bt body => .block (if bt.isSome then 1 else 0) (eraseL body)
  def eraseL : List BI → List WInstr
    | [] => []
    | x :: xs => eraseI x :: eraseL xs
end

theorem eraseL_append (xs ys : List BI) : eraseL (xs ++ ys) = eraseL xs ++ eraseL ys := by
  induction xs with
  | nil => simp [eraseL]
  | cons x xs ih => simp [eraseL, ih]

theorem eraseL_ops (is : List WInstr) : eraseL (is.map BI.op) = is := by
  induction is with
  | nil => rfl
  | cons i is ih => simp [eraseL, eraseI, ih]

/-! ## Emitter templates -/

def BinOp.flip : BinOp → BinOp
  | .lt => .gt
  | .gt => .lt
  | .lte => .gte
  | .gte => .lte
  | op => op

/-- `mir_int_literal`: an `Int` literal operand. -/
def litInt? : Expr → Option Int
  | .literal (.int k) => some k
  | _ => none

/-- `aint_const_cmp_operand_is_reemittable`: a bare local. -/
def slot? : Expr → Option Nat
  | .local i => some i
  | _ => none

def MCtx.arithIdx (M : MCtx) : BinOp → Nat
  | .add => M.add
  | .sub => M.sub
  | _ => M.mul

/-- Small arm of the const compare: native i64 compare of the `small` field. -/
def smallCmpInstr : BinOp → WInstr
  | .eq => .i64Eq
  | .neq => .i64Ne
  | .lt => .i64LtS
  | .gt => .i64GtS
  | .lte => .i64LeS
  | _ => .i64GeS

/-- Big arm of the const compare: the sign decides every order relation, and a
    Big value never equals an i64 constant. -/
def bigCmpArm (C s : Nat) : BinOp → List WInstr
  | .lt | .lte => [.localGet s, .structGet C 2, .i32Const 0, .i32LtS]
  | .gt | .gte => [.localGet s, .structGet C 2, .i32Const 0, .i32GtS]
  | .eq => [.i32Const 0]
  | .neq => [.i32Const 1]
  | _ => []

/-- The const-compare tag branch over the carrier held in local `s`
    (`emit_aint_cmp_const_reemit`; `emit_aint_cmp_const` is `local.set s`
    followed by this). -/
def cmpArmB (C s : Nat) (op : BinOp) (k : Int) : List BI :=
  [ .op (.localGet s), .op (.structGet C 1), .op .refIsNull,
    .ifElse (some .bool)
      [.op (.localGet s), .op (.structGet C 0), .op (.i64Const k), .op (smallCmpInstr op)]
      ((bigCmpArm C s op).map .op) ]

/-- Int against Int without a literal (`emit_aint_binop`). -/
def intCmpTail (M : MCtx) : BinOp → List WInstr
  | .eq => [.call M.eq]
  | .neq => [.call M.eq, .i32Eqz]
  | .lt => [.call M.cmp, .i32Const 0, .i32LtS]
  | .gt => [.call M.cmp, .i32Const 0, .i32GtS]
  | .lte => [.call M.cmp, .i32Const 0, .i32LeS]
  | _ => [.call M.cmp, .i32Const 0, .i32GeS]

def boolCmpInstr : BinOp → WInstr
  | .eq => .i32Eq
  | _ => .i32Ne

/-- A call of the declared `List<t>` helper of role `r`, or nothing when the
    type table declares none (the typing requires one). -/
def helperCall (M : MCtx) (r : ListRole) (t : Ty) : List BI :=
  match M.listHelper r t with
  | some f => [.op (.call f)]
  | none => []

/-- A call of the declared `Bytes` helper of role `r`, or nothing when the
    type table declares none (the typing requires one). -/
def bytesCall (M : MCtx) (r : BytesRole) : List BI :=
  match M.bytesHelper r with
  | some f => [.op (.call f)]
  | none => []

/-- The instruction a builtin call ends with, given its argument types:
    one `i32` instruction for the Bool builtins, `struct.new` of the tail's
    cons struct for `List.prepend`, and a call of the List helper for the
    List builtins (`emit_mir_builtin_call`'s per-`List<T>` dispatch):
    `List.len` then boxes the helper's `i64` (`__aint_from_i64`), and
    `List.take` / `List.drop` first saturate their Int count to an `i64`
    (`__aint_to_i64_sat`). A `Bytes` builtin calls its helper (a `take` /
    `drop` count saturated first), and `bytesLen` is the emitter's fused
    `List.len(bytes.values)`: `array.len`, zero-extended, boxed. -/
def builtinTail (M : MCtx) : Builtin → Option (List Ty) → List BI
  | .boolAnd, _ => [.op .i32And]
  | .boolOr, _ => [.op .i32Or]
  | .boolNot, _ => [.op .i32Eqz]
  | .listPrepend, some [_, .list t] => [.op (.structNew (M.listStruct t) 2)]
  | .listLen, some [.list t] => helperCall M .len t ++ [.op (.call M.box)]
  | .listReverse, some [.list t] => helperCall M .reverse t
  | .listConcat, some [.list t, _] => helperCall M .concat t
  | .listTake, some [.list t, _] => .op (.call M.toI64Sat) :: helperCall M .take t
  | .listDrop, some [.list t, _] => .op (.call M.toI64Sat) :: helperCall M .drop t
  | .listContains, some [.list t, _] => helperCall M .contains t
  | .bytesOfList, _ => bytesCall M .pack
  | .bytesValues, _ => bytesCall M .unpack
  | .bytesLen, _ => [.op .arrayLen, .op .i64ExtendI32U, .op (.call M.box)]
  | .bytesConcat, _ => bytesCall M .concat
  | .bytesTake, _ => .op (.call M.toI64Sat) :: bytesCall M .take
  | .bytesDrop, _ => .op (.call M.toI64Sat) :: bytesCall M .drop
  | .strFromInt, _ => [.op (.call M.fromInt)]
  | _, _ => []

/-- The `f64` comparison of a Float `BinOp`. -/
def floatCmpInstr : BinOp → WInstr
  | .eq => .f64Eq
  | .lt => .f64Lt
  | .gt => .f64Gt
  | .lte => .f64Le
  | _ => .f64Ge

/-- A string literal: `array.new_data $string seg` over offset 0 and the
    literal's length (`emit_string_literal_bytes`). -/
def strLitB (M : MCtx) (bytes : List Nat) : List BI :=
  [.op (.i32Const 0), .op (.i32Const bytes.length), .newData M.str (M.strSeg bytes) bytes]

/-- Concatenate the `n` Strings on the stack: a `Vector<String>` of them, then
    `__wasmgc_concat_n` (`emit_mir_string_binop`, `emit_mir_interpolated_str`). -/
def concatB (M : MCtx) (n : Nat) : List BI :=
  [.op (.arrayNewFixed M.strVec n), .op (.call M.concat)]

/-- The tail of a `String` `BinOp` after both operands. -/
def strOpTail (M : MCtx) : BinOp → List BI
  | .add => concatB M 2
  | .eq => [.op (.call M.streq)]
  | _ => [.op (.call M.streq), .op .i32Eqz]

/-- `Option.withDefault(Vector.get(v, i), d)` fused
    (`emit_mir_option_with_default`): the index through `__aint_to_index`,
    tested `>= 0` and (unsigned) `< array.len`, both halves evaluated, then
    `array.get` or the default. `dc` is the default's code. -/
def vecGetOrB (M : MCtx) (v i : Nat) (t : Ty) (dc : List BI) : List BI :=
  [ .op (.localGet i), .op (.call M.toIndex), .op (.i32Const 0), .op .i32GeS,
    .op (.localGet i), .op (.call M.toIndex), .op (.localGet v), .op .arrayLen, .op .i32LtU,
    .op .i32And,
    .ifElse (some t)
      [.op (.localGet v), .op (.localGet i), .op (.call M.toIndex),
        .op (.arrayGet (M.vecStruct t))]
      dc ]

/-- `emit_default_value`: the filler of the unused payload field. -/
def dfltB (M : MCtx) : Ty → List BI
  | .int => [.op (.i64Const 0), .nullOf M.mag, .op (.i32Const 0), .op (.structNew M.carrier 3)]
  | .bool => [.op (.i32Const 0)]
  | .record tid => if M.isWord tid then [.op (.i64Const 0)] else [.nullOf (M.structOf tid)]
  | .sum tid => [.nullOf (M.sumRoot tid)]
  | .option t => [.nullOf (M.optStruct t)]
  | .result t e => [.nullOf (M.resStruct t e)]
  | .string => [.nullOf M.str]
  | .float => [.op (.f64Const 0)]
  | .list t => [.nullOf (M.listStruct t)]
  | .vec t => [.nullOf (M.vecStruct t)]
  | .bytes => [.nullOf M.bytesArr]
  | _ => []

/-- Read field `i` of the struct `idx` held (as `eqref`) in the subject
    scratch `ss` into binder `b`; nothing for an ignored binder. -/
def bindFieldB (ss idx i b : Nat) : List BI :=
  if b = noSlot then []
  else [.op (.localGet ss), .op (.refCast idx), .op (.structGet idx i), .op (.localSet b)]

/-- The binders of a variant arm, fields `i, i+1, …`. -/
def extractB (ss idx : Nat) : Nat → List Nat → List BI
  | _, [] => []
  | i, b :: bs => bindFieldB ss idx i b ++ extractB ss idx (i + 1) bs

/-- The tag test of an Option / Result held in the subject scratch. -/
def tagTestB (ss idx : Nat) : List BI :=
  [.op (.localGet ss), .op (.refCast idx), .op (.structGet idx 0), .op (.i32Const 1), .op .i32Eq]

/-- The fused `Result.withDefault(Int.div/mod(a, b), d)` after its three
    operands (`emit_mir_result_with_default`, bignum path): the operands are
    parked in the operand scratch locals `cmp + 1 .. cmp + 3` (the default
    last in, first out), the divisor is tested for zero on the carrier, and
    the default or `__aint_divmod(a, b, want_mod)` is the result. -/
def divOrB (M : MCtx) (X : LCtx) (isMod : Bool) : List BI :=
  [ .op (.localSet (X.cmp + 3)), .op (.localSet (X.cmp + 2)), .op (.localSet (X.cmp + 1)),
    .op (.localGet (X.cmp + 2)), .op (.structGet M.carrier 1), .op .refIsNull,
    .op (.localGet (X.cmp + 2)), .op (.structGet M.carrier 0), .op .i64Eqz, .op .i32And,
    .ifElse (some .int) [.op (.localGet (X.cmp + 3))]
      [.op (.localGet (X.cmp + 1)), .op (.localGet (X.cmp + 2)),
        .op (.i32Const (if isMod then 1 else 0)), .op (.call M.divmod)] ]

/-- `emit_mir_try` after its subject, which sits in the subject scratch: the
    tag test, then the `Ok` payload (field 1), or a fresh `Err` of the
    enclosing return type `Result<t', er'>` (tag `0`, the filler of `t'`, the
    subject's field 2) and `return`. -/
def tryB (M : MCtx) (X : LCtx) (t er t' er' : Ty) : List BI :=
  tagTestB X.subj (M.resStruct t er) ++
    [.ifElse (some t)
      [.op (.localGet X.subj), .op (.refCast (M.resStruct t er)),
        .op (.structGet (M.resStruct t er) 1)]
      ([.op (.i32Const 0)] ++ dfltB M t' ++
        [.op (.localGet X.subj), .op (.refCast (M.resStruct t er)),
          .op (.structGet (M.resStruct t er) 2), .op (.structNew (M.resStruct t' er') 3),
          .op .ret])]

/-- The `want_mod` flag of a Euclidean intrinsic. -/
def Intrinsic.flag : Intrinsic → Int
  | .intDivEuclid => 0
  | .intModEuclid => 1

/-! ## The lowering -/

/-! ### The `Int` tree on the machine word

`Expr.intWord t w` (`MirExpr::IntWord`) is lowered as the emitter lowers it
(`int_tree.rs`, `emit_int_word`): each field leaf is read once, ahead, into
its local; then the `i64` path reads the `$small` of each leaf, leaving for
the exact path when a leaf is Big or an operation overflows, and boxes the
machine result; the exact path is the ordinary calls on the same leaves. The
plan names every local it writes, so nothing here depends on where the
emitter put them. -/

/-- The `$small` of the carrier in local `i`, or a branch out (`br_if 0`)
    when it is Big. -/
def smallReadW (C i : Nat) : List WInstr :=
  [.localGet i, .structGet C 1, .refIsNull, .i32Eqz, .brIf 0, .localGet i, .structGet C 0]

/-- The machine operation on the two words on the stack, with the overflow
    test of `bignum.rs` (`emit_word_op`): the operands are parked in the
    words `w` and `w + 1` and the result in `w + 2`; an overflow branches
    out. -/
def treeOpW (w : Nat) (op : BinOp) : List WInstr :=
  let t0 := w
  let t1 := w + 1
  let t2 := w + 2
  -- a ∉ {-1, 0}, as `(a + 1) >u 1`
  let general := [.localGet t0, .i64Const 1, .i64Add, .i64Const 1, .i64GtU]
  [.localSet t1, .localSet t0, .localGet t0, .localGet t1] ++
  match op with
  | .add => [.i64Add, .localTee t2, .localGet t0, .i64Xor, .localGet t2,
      .localGet t1, .i64Xor, .i64And, .i64Const 0, .i64LtS, .brIf 0, .localGet t2]
  | .sub => [.i64Sub, .localTee t2, .localGet t0, .i64Xor, .localGet t0,
      .localGet t1, .i64Xor, .i64And, .i64Const 0, .i64LtS, .brIf 0, .localGet t2]
  | _ => [.i64Mul, .localTee t2, .localGet t0, .i64Const 1] ++ general ++
      [.select, .i64DivS, .localGet t1, .i64Ne, .localGet t0, .i64Const (-1), .i64Eq,
        .localGet t1, .i64Const (-9223372036854775808), .i64Eq, .i32And] ++ general ++
      [.select, .brIf 0, .localGet t2]

/-- `Int.min` / `Int.max` of the two words on the stack (`emit_fast`): both
    parked in the words `w` and `w + 1`, then the first kept by a `select`
    when it compares `<=` (`>=`) with the second. -/
def pickOpW (w : Nat) (p : PickBuiltin) : List WInstr :=
  [.localSet (w + 1), .localSet w, .localGet w, .localGet (w + 1), .localGet w,
    .localGet (w + 1), match p with | .intMin => .i64LeS | .intMax => .i64GeS, .select]

/-- The `[lo, hi]` range test of the word on the stack (`int_tree.rs`,
    `guard`), the bounds clamped to the `i64` range: the word is parked in
    local `t`, and a word outside the range branches out (`br_if 0`). A full
    range tests nothing. -/
def guardW (lo hi : Int) (t : Nat) : List WInstr :=
  let lo := max lo (-9223372036854775808)
  let hi := min hi 9223372036854775807
  if hi < lo then [.localSet t, .br 0]
  else if hi - lo < 18446744073709551615 then
    [.localSet t, .localGet t, .i64Const lo, .i64Sub, .i64Const (wrap64 (hi - lo)), .i64GtU,
      .brIf 0]
  else [.localSet t]

/-- The operands `x` for which `x * k` fits an `i64` (`int_tree.rs`,
    `mul_range`): `[ceil(min / k), floor(max / k)]`, the whole range for
    `k = 0`. -/
def mulRange (k : Int) : Int × Int :=
  let lo : Int := if k < 0 then 9223372036854775807 else -9223372036854775808
  let hi : Int := if k < 0 then -9223372036854775808 else 9223372036854775807
  let floor := fun (a : Int) => Int.ediv (a * k.sign) (k.natAbs : Int)
  if k = 0 then (-9223372036854775808, 9223372036854775807) else (-floor (-lo), floor hi)

/-- `Int.div(x, k)` of the word on the stack by the general sequence
    (`int_tree.rs`, `Op::Div`): the truncated quotient, one further from
    zero when the truncated remainder is negative; the operand is parked in
    the word `w`. -/
def divGenW (w : Nat) (k : Int) : List WInstr :=
  [.localTee w, .i64Const k, .i64DivS, .localGet w, .i64Const k, .i64RemS, .i64Const 63, .i64ShrS,
    if 0 < k then .i64Add else .i64Sub]

/-- `Int.div(x, k)` of the word on the stack (`Op::Div`): by a positive
    power of two `2^s` the arithmetic shift by `s` (a floor), else the
    general sequence. -/
def divOpW (w : Nat) (k : Int) : List WInstr :=
  if 0 < k && isPow2 k.natAbs then [.i64Const (k.natAbs.log2 : Int), .i64ShrS] else divGenW w k

/-- `Int.mod(x, k)` of the word on the stack by the general sequence
    (`Op::Mod`): the truncated remainder `r`, plus `|k|` when `r` is
    negative; `r` is parked in the word `w`. -/
def modGenW (w : Nat) (k : Int) : List WInstr :=
  [.i64Const k, .i64RemS, .localTee w, .localGet w, .i64Const 63, .i64ShrS, .i64Const k.natAbs,
    .i64And, .i64Add]

/-- `Int.mod(x, k)` of the word on the stack (`Op::Mod`): by `±2^n` the low
    `n` bits (`i64.and` with `2^n - 1`), else the general sequence. -/
def modOpW (w : Nat) (k : Int) : List WInstr :=
  if isPow2 k.natAbs then [.i64Const ((k.natAbs - 1 : Nat) : Int), .i64And] else modGenW w k

/-- The word of a leaf on the `i64` path: a word record's `Int` is its
    `i64`, any other leaf is the `$small` of its carrier (a Big one branches
    out). -/
def WLeaf.fastW (C : Nat) : WLeaf → List WInstr
  | .word _ _ slot => [.localGet slot]
  | l => smallReadW C l.loc

/-- A leaf on the exact path: its carrier, or a word record's `i64` boxed. -/
def WLeaf.exactW (M : MCtx) : WLeaf → List WInstr
  | .word _ _ slot => [.localGet slot, .call M.box]
  | l => [.localGet l.loc]

/-- The `i64` path of a tree: the word of each leaf, and each operation on
    the words. -/
def fastW (C w : Nat) : WTree → List WInstr
  | .leaf l => l.fastW C
  | .op o a b => fastW C w a ++ fastW C w b ++ treeOpW w o
  | .lit k => [.i64Const k]
  | .pick p a b => fastW C w a ++ fastW C w b ++ pickOpW w p
  | .mulLit k _ a =>
      fastW C w a ++ guardW (mulRange k).1 (mulRange k).2 w ++ [.localGet w, .i64Const k, .i64Mul]
  | .divLit k a => fastW C w a ++ divOpW w k
  | .modLit k a => fastW C w a ++ modOpW w k

/-- The exact path of a tree: each leaf's carrier, each literal boxed, each
    operation's call, and for `Int.min` / `Int.max` the three-way helper and
    the picked operand's exact path again. -/
def exactB (M : MCtx) : WTree → List BI
  | .leaf l => (l.exactW M).map .op
  | .op o a b => exactB M a ++ exactB M b ++ [.op (.call (M.arithIdx o))]
  | .lit k => [.op (.i64Const k), .op (.call M.box)]
  | .pick p a b =>
      exactB M a ++ exactB M b ++ (intCmpTail M p.op).map .op ++
        [.ifElse (some .int) (exactB M a) (exactB M b)]
  | .mulLit k lf a =>
      (if lf then [.op (.i64Const k), .op (.call M.box)] ++ exactB M a
        else exactB M a ++ [.op (.i64Const k), .op (.call M.box)]) ++ [.op (.call M.mul)]
  | .divLit k a =>
      exactB M a ++ [.op (.i64Const k), .op (.call M.box), .op (.i32Const 0), .op (.call M.divmod)]
  | .modLit k a =>
      exactB M a ++ [.op (.i64Const k), .op (.call M.box), .op (.i32Const 1), .op (.call M.divmod)]

/-- The exact path the interpreter runs. -/
def exactW (M : MCtx) (t : WTree) : List WInstr := eraseL (exactB M t)

theorem exactW_leaf (M : MCtx) (l : WLeaf) : exactW M (.leaf l) = l.exactW M := by
  simp [exactW, exactB, eraseL_ops]

theorem exactW_op (M : MCtx) (o : BinOp) (a b : WTree) :
    exactW M (.op o a b) = exactW M a ++ exactW M b ++ [.call (M.arithIdx o)] := by
  simp [exactW, exactB, eraseL_append, eraseL, eraseI]

theorem exactW_lit (M : MCtx) (k : Int) : exactW M (.lit k) = [.i64Const k, .call M.box] := rfl

theorem exactW_pick (M : MCtx) (p : PickBuiltin) (a b : WTree) :
    exactW M (.pick p a b) = exactW M a ++ exactW M b ++ intCmpTail M p.op ++
      [.ifElse (exactW M a) (exactW M b)] := by
  simp [exactW, exactB, eraseL_append, eraseL_ops, eraseL, eraseI]

theorem exactW_mulLit (M : MCtx) (k : Int) (lf : Bool) (a : WTree) :
    exactW M (.mulLit k lf a) =
      (if lf then [.i64Const k, .call M.box] ++ exactW M a
        else exactW M a ++ [.i64Const k, .call M.box]) ++ [.call M.mul] := by
  cases lf <;> simp [exactW, exactB, eraseL_append, eraseL, eraseI]

theorem exactW_divLit (M : MCtx) (k : Int) (a : WTree) :
    exactW M (.divLit k a) =
      exactW M a ++ [.i64Const k, .call M.box, .i32Const 0, .call M.divmod] := by
  simp [exactW, exactB, eraseL_append, eraseL, eraseI]

theorem exactW_modLit (M : MCtx) (k : Int) (a : WTree) :
    exactW M (.modLit k a) =
      exactW M a ++ [.i64Const k, .call M.box, .i32Const 1, .call M.divmod] := by
  simp [exactW, exactB, eraseL_append, eraseL, eraseI]

/-- The field leaves read ahead, in order, each stored in its local. -/
def aheadW (M : MCtx) : WTree → List WInstr
  | .leaf (.local _) => []
  | .leaf (.field tid k base slot) =>
      [.localGet base, .structGet (M.structOf tid) k, .localSet slot]
  | .leaf (.word _ base slot) => [.localGet base, .localSet slot]
  | .op _ a b => aheadW M a ++ aheadW M b
  | .lit _ => []
  | .pick _ a b => aheadW M a ++ aheadW M b
  | .mulLit _ _ a => aheadW M a
  | .divLit _ a => aheadW M a
  | .modLit _ a => aheadW M a

/-- `block $done (block $slow <i64 path> box br $done) <exact path>`. -/
def wordBlock (M : MCtx) (t : WTree) (w : Nat) : BI :=
  .block (some .int)
    (.block none ((fastW M.carrier w t ++ [WInstr.call M.box, WInstr.br 1]).map .op) ::
      exactB M t)

/-- `block $done (block $slow <i64 paths> <compare> br $done) <exact paths>
    <compare helper>`: the comparison of two trees (`emit_int_word`,
    `IntWordKind::Compare`). -/
def cmpBlock (M : MCtx) (op : BinOp) (a b : WTree) (w : Nat) : BI :=
  .block (some .bool)
    (.block none ((fastW M.carrier w a ++ fastW M.carrier w b ++
        [smallCmpInstr op, WInstr.br 1]).map .op) ::
      (exactB M a ++ exactB M b ++ (intCmpTail M op).map .op))

mutual
  def lowerB (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool) : Expr → List BI
    | .literal (.int k) => [.op (.i64Const k), .op (.call M.box)]
    | .literal (.bool v) => [.op (.i32Const (if v then 1 else 0))]
    | .literal (.float bits) => [.op (.f64Const bits)]
    | .literal (.str bytes) => strLitB M bytes
    | .local i => [.op (.localGet i)]
    | .let_ b v body =>
        lowerB M X Γ false v ++ [.op (.localSet b)] ++
          lowerB M X (match tyOf M X.n Γ false v with
            | some T => upd Γ b T
            | none => Γ) tail body
    | .call (.fn f) args => lowerArgsB M X Γ args ++ [.op (.call f)]
    | .call (.builtin bi) args =>
        lowerArgsB M X Γ args ++ builtinTail M bi (tysOf M X.n Γ args)
    | .tailCall f args => lowerArgsB M X Γ args ++ [.op (.returnCall f)]
    | .binOp op l r =>
        match tyOf M X.n Γ false l with
        | some .bool =>
            lowerB M X Γ false l ++ lowerB M X Γ false r ++ [.op (boolCmpInstr op)]
        | some .float =>
            lowerB M X Γ false l ++ lowerB M X Γ false r ++ [.op (floatCmpInstr op)]
        | some .string =>
            lowerB M X Γ false l ++ lowerB M X Γ false r ++ strOpTail M op
        | _ =>
            if op.isArith then
              lowerB M X Γ false l ++ lowerB M X Γ false r ++ [.op (.call (M.arithIdx op))]
            else
              match litInt? l, litInt? r with
              | some k, _ =>
                  match slot? r with
                  | some i => cmpArmB M.carrier i op.flip k
                  | none =>
                      lowerB M X Γ false r ++ [.op (.localSet X.cmp)] ++
                        cmpArmB M.carrier X.cmp op.flip k
              | none, some k =>
                  match slot? l with
                  | some i => cmpArmB M.carrier i op k
                  | none =>
                      lowerB M X Γ false l ++ [.op (.localSet X.cmp)] ++
                        cmpArmB M.carrier X.cmp op k
              | none, none =>
                  lowerB M X Γ false l ++ lowerB M X Γ false r ++ (intCmpTail M op).map .op
    | .neg e => lowerB M X Γ false e ++ [.op (.call M.neg)]
    | .ifThenElse c t e =>
        lowerB M X Γ false c ++
          [.ifElse (tyOf M X.n Γ tail t) (lowerB M X Γ tail t) (lowerB M X Γ tail e)]
    | .recordCreate tid fs =>
        if M.isWord tid then lowerArgsB M X Γ fs ++ [.op (.call M.toI64Chk)]
        else lowerArgsB M X Γ fs ++ [.op (.structNew (M.structOf tid) fs.length)]
    | .project tid i base =>
        if M.isWord tid then lowerB M X Γ false base ++ [.op (.call M.box)]
        else lowerB M X Γ false base ++ [.op (.structGet (M.structOf tid) i)]
    | .call (.lazy lb) args =>
        match args with
        | [o, d] =>
          match vecGetOr? lb o d with
          | some (v, i) =>
            match Γ v with
            | some (.vec t) => vecGetOrB M v i t (lowerB M X Γ false d)
            | _ => []
          | none =>
            match divOr? lb o d with
            | some (isMod, _, _) =>
                -- `o` is `Int.div(a, b)` / `Int.mod(a, b)`, whose own lowering
                -- is just its two operands (`builtinTail` adds nothing)
                lowerB M X Γ false o ++ lowerB M X Γ false d ++ divOrB M X isMod
            | none =>
            match lb, tyOf M X.n Γ false o with
            | .optWithDefault, some (.option t) =>
                lowerB M X Γ false o ++ [.op (.localSet X.subj)] ++
                  tagTestB X.subj (M.optStruct t) ++
                  [.ifElse (some t)
                    [.op (.localGet X.subj), .op (.refCast (M.optStruct t)),
                      .op (.structGet (M.optStruct t) 1)]
                    (lowerB M X Γ false d)]
            | .resWithDefault, some (.result t e) =>
                lowerB M X Γ false o ++ [.op (.localSet X.subj)] ++
                  tagTestB X.subj (M.resStruct t e) ++
                  [.ifElse (some t)
                    [.op (.localGet X.subj), .op (.refCast (M.resStruct t e)),
                      .op (.structGet (M.resStruct t e) 1)]
                    (lowerB M X Γ false d)]
            | _, _ => []
        | _ => []
    | .call (.intrinsic ie) args =>
        lowerArgsB M X Γ args ++ [.op (.i32Const ie.flag), .op (.call M.divmod)]
    | .call (.pick pb) args =>
        -- both operands, `__aint_cmp` against `0`, then the picked operand
        -- evaluated again inside an `if` of the carrier type
        match args with
        | [a, b] =>
            lowerB M X Γ false a ++ lowerB M X Γ false b ++ (intCmpTail M pb.op).map .op ++
              [.ifElse (some .int) (lowerB M X Γ false a) (lowerB M X Γ false b)]
        | _ => []
    | .construct c ty args =>
        match c, ty with
        | .user tid k, _ =>
            lowerArgsB M X Γ args ++ [.op (.structNew (M.ctorStruct tid k) args.length)]
        | .some, .option t =>
            [.op (.i32Const 1)] ++ lowerArgsB M X Γ args ++ [.op (.structNew (M.optStruct t) 2)]
        | .none, .option t =>
            [.op (.i32Const 0)] ++ dfltB M t ++ lowerArgsB M X Γ args ++
              [.op (.structNew (M.optStruct t) 2)]
        | .ok, .result t e =>
            [.op (.i32Const 1)] ++ lowerArgsB M X Γ args ++ dfltB M e ++
              [.op (.structNew (M.resStruct t e) 3)]
        | .err, .result t e =>
            [.op (.i32Const 0)] ++ dfltB M t ++ lowerArgsB M X Γ args ++
              [.op (.structNew (M.resStruct t e) 3)]
        | _, _ => []
    | .match_ s arms =>
        match tyOf M X.n Γ false s with
        | some .int =>
            lowerIntArms M X Γ tail (lowerB M X Γ false s)
              (tyOf M X.n Γ tail (.match_ s arms)) arms
        | some .bool =>
            lowerB M X Γ false s ++
              lowerBoolArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) arms
        | some (.option t) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerOptArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) t arms
        | some (.result t e) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerResArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) t e arms
        | some (.sum tid) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerVarArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) tid arms
        | some .string =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerStrArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) arms
        | some (.record tid) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerTupArms M X Γ tail tid arms
        | some (.list t) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerListArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) t arms
        | _ => []
    | .interp parts => lowerArgsB M X Γ parts ++ concatB M parts.length
    | .list t items =>
        if items.isEmpty then [.nullOf (M.listStruct t)]
        else
          match M.listCons t with
          | some f =>
              lowerArgsB M X Γ items ++ [.nullOf (M.listStruct t)] ++
                List.replicate items.length (.op (.call f))
          | none => []
    | .try_ e R =>
        match tyOf M X.n Γ false e, R with
        | some (.result t er), .result t' er' =>
            lowerB M X Γ false e ++ [.op (.localSet X.subj)] ++ tryB M X t er t' er'
        | _, _ => []
    | .scope e => lowerB M X Γ tail e
    | .intWord t w => (aheadW M t).map .op ++ [wordBlock M t w]
    | .intCmp op a b w => (aheadW M a ++ aheadW M b).map .op ++ [cmpBlock M op a b w]
  def lowerArgsB (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) : List Expr → List BI
    | [] => []
    | e :: es => lowerB M X Γ false e ++ lowerArgsB M X Γ es
  /-- `emit_mir_int_cascade`: `sc` is the subject's code, re-emitted per
      literal arm; the first catch-all ends the cascade. -/
  def lowerIntArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool) (sc : List BI)
      (bt : Option Ty) : Arms → List BI
    | .nil => []
    | .cons p b rest =>
        match p with
        | .litInt k =>
            sc ++ [.op (.i64Const k), .op (.call M.box), .op (.call M.eq),
              .ifElse bt (lowerB M X Γ tail b) (lowerIntArms M X Γ tail sc bt rest)]
        | .wild => lowerB M X Γ tail b
        | .bind s => sc ++ [.op (.localSet s)] ++ lowerB M X (upd Γ s .int) tail b
        | _ => []
  /-- The emitter's Bool `match`: one `if` on the subject. -/
  def lowerBoolArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) : Arms → List BI
    | .cons (.litBool v) t (.cons _ e _) =>
        if v then [.ifElse bt (lowerB M X Γ tail t) (lowerB M X Γ tail e)]
        else [.ifElse bt (lowerB M X Γ tail e) (lowerB M X Γ tail t)]
    | _ => []
  /-- `emit_mir_option_match`, the subject already in the subject scratch. -/
  def lowerOptArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) (t : Ty) : Arms → List BI
    | .cons p1 b1 (.cons p2 b2 _) =>
        match optPick p1 p2 with
        | some (false, sb) =>
            tagTestB X.subj (M.optStruct t) ++
              [.ifElse bt
                (bindFieldB X.subj (M.optStruct t) 1 sb ++
                  lowerB M X ((bindOne X.n Γ sb t).getD Γ) tail b1)
                (lowerB M X Γ tail b2)]
        | some (true, sb) =>
            tagTestB X.subj (M.optStruct t) ++
              [.ifElse bt
                (bindFieldB X.subj (M.optStruct t) 1 sb ++
                  lowerB M X ((bindOne X.n Γ sb t).getD Γ) tail b2)
                (lowerB M X Γ tail b1)]
        | none => []
    | _ => []
  /-- `emit_mir_result_match`: the `Ok` payload is field 1, the `Err` one
      field 2. -/
  def lowerResArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) (t e : Ty) : Arms → List BI
    | .cons p1 b1 (.cons p2 b2 _) =>
        match resPick p1 p2 with
        | some (false, ob, eb) =>
            tagTestB X.subj (M.resStruct t e) ++
              [.ifElse bt
                (bindFieldB X.subj (M.resStruct t e) 1 ob ++
                  lowerB M X ((bindOne X.n Γ ob t).getD Γ) tail b1)
                (bindFieldB X.subj (M.resStruct t e) 2 eb ++
                  lowerB M X ((bindOne X.n Γ eb e).getD Γ) tail b2)]
        | some (true, ob, eb) =>
            tagTestB X.subj (M.resStruct t e) ++
              [.ifElse bt
                (bindFieldB X.subj (M.resStruct t e) 1 ob ++
                  lowerB M X ((bindOne X.n Γ ob t).getD Γ) tail b2)
                (bindFieldB X.subj (M.resStruct t e) 2 eb ++
                  lowerB M X ((bindOne X.n Γ eb e).getD Γ) tail b1)]
        | none => []
    | _ => []
  /-- `emit_mir_variant_arm_cascade`: `ref.test` each arm's constructor, the
      last arm untested; a `_` arm ends the cascade. -/
  def lowerVarArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) (tid : Nat) : Arms → List BI
    | .nil => []
    | .cons p b .nil =>
        match p with
        | .ctor (.user tid' c) bs =>
            extractB X.subj (M.ctorStruct tid' c) 0 bs ++
              lowerB M X ((varArmΓ M X.n Γ tid p).getD Γ) tail b
        | _ => lowerB M X Γ tail b
    | .cons p b (.cons p' b' r) =>
        match p with
        | .ctor (.user tid' c) bs =>
            [.op (.localGet X.subj), .op (.refTest (M.ctorStruct tid' c)),
              .ifElse bt
                (extractB X.subj (M.ctorStruct tid' c) 0 bs ++
                  lowerB M X ((varArmΓ M X.n Γ tid p).getD Γ) tail b)
                (lowerVarArms M X Γ tail bt tid (.cons p' b' r))]
        | .wild => lowerB M X Γ tail b
        | _ => []
  /-- `emit_mir_string_match`, the subject already in the subject scratch:
      per literal arm, cast the scratch back to `$string`, compare with the
      literal through `__wasmgc_string_eq`, `if`; the default innermost. -/
  def lowerStrArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) : Arms → List BI
    | .nil => []
    | .cons p b rest =>
        match p with
        | .litStr k =>
            [.op (.localGet X.subj), .castNull M.str] ++ strLitB M k ++
              [.op (.call M.streq), .ifElse bt (lowerB M X Γ tail b)
                (lowerStrArms M X Γ tail bt rest)]
        | _ => lowerB M X Γ tail b
  /-- `emit_mir_tuple_match`, the subject already in the subject scratch:
      each bound component read with `ref.cast` + `struct.get`, then the body. -/
  def lowerTupArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool) (tid : Nat) :
      Arms → List BI
    | .cons (.tuple bs) b _ =>
        extractB X.subj (M.structOf tid) 0 bs ++
          lowerB M X (((M.recFields tid).bind (bindTys X.n Γ bs)).getD Γ) tail b
    | _ => []
  /-- `emit_mir_list_match`, the subject already in the subject scratch:
      `ref.is_null` picks the `[]` arm; otherwise the head and tail binders
      are read from the cons struct, then the cons arm runs. -/
  def lowerListArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) (t : Ty) : Arms → List BI
    | .cons p1 b1 (.cons p2 b2 _) =>
        match listPick p1 p2 with
        | some (false, h, tl) =>
            [.op (.localGet X.subj), .op .refIsNull,
              .ifElse bt (lowerB M X Γ tail b1)
                (extractB X.subj (M.listStruct t) 0 [h, tl] ++
                  lowerB M X ((bindTys X.n Γ [h, tl] [t, .list t]).getD Γ) tail b2)]
        | some (true, h, tl) =>
            [.op (.localGet X.subj), .op .refIsNull,
              .ifElse bt (lowerB M X Γ tail b2)
                (extractB X.subj (M.listStruct t) 0 [h, tl] ++
                  lowerB M X ((bindTys X.n Γ [h, tl] [t, .list t]).getD Γ) tail b1)]
        | none => []
    | _ => []
end

/-- The instructions the interpreter runs. -/
def lowerW (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool) (e : Expr) :
    List WInstr :=
  eraseL (lowerB M X Γ tail e)

def lowerArgsW (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (es : List Expr) : List WInstr :=
  eraseL (lowerArgsB M X Γ es)

/-- The instruction tree of one function. -/
def fnCode (M : MCtx) (p : FnPlan) : WCode :=
  { arity := p.sig.params.length, nlocals := p.locals.length,
    body := lowerW M p.lctx (paramsΓ p.sig.params) true p.body }

/-! ## The byte image -/

/-- The eight little-endian bytes of an `f64.const` immediate. -/
def u64le (bits : UInt64) : List Nat :=
  (List.range 8).map fun i => bits.toNat / 256 ^ i % 256

/-- Opcode bytes of one instruction of the admitted fragment (`none` for any
    other instruction, so an unexpected instruction fails closed). -/
def encW : WInstr → Option (List Nat)
  | .localGet i => (uleb32 i).map ([0x20] ++ ·)
  | .localSet i => (uleb32 i).map ([0x21] ++ ·)
  | .i64Const k => (sleb64 k).map ([0x42] ++ ·)
  | .i32Const k => (sleb32 k).map ([0x41] ++ ·)
  | .call f => (uleb32 f).map ([0x10] ++ ·)
  | .returnCall f => (uleb32 f).map ([0x12] ++ ·)
  | .structNew t _ => (uleb32 t).map ([0xfb, 0x00] ++ ·)
  | .structGet t fld =>
      match uleb32 t, uleb32 fld with
      | some a, some b => some ([0xfb, 0x02] ++ a ++ b)
      | _, _ => none
  | .refIsNull => some [0xd1]
  | .i64Eqz => some [0x50]
  | .i32Eqz => some [0x45]
  | .i32Eq => some [0x46]
  | .i32Ne => some [0x47]
  | .i32LtS => some [0x48]
  | .i32GtS => some [0x4a]
  | .i32LeS => some [0x4c]
  | .i32GeS => some [0x4e]
  | .i64Eq => some [0x51]
  | .i64Ne => some [0x52]
  | .i64LtS => some [0x53]
  | .i64GtS => some [0x55]
  | .i64LeS => some [0x57]
  | .i64GeS => some [0x59]
  | .i32And => some [0x71]
  | .i32Or => some [0x72]
  | .i32LtU => some [0x49]
  | .i64ExtendI32U => some [0xad]
  | .f64Const bits => some ([0x44] ++ u64le bits)
  | .f64Eq => some [0x61]
  | .f64Lt => some [0x63]
  | .f64Gt => some [0x64]
  | .f64Le => some [0x65]
  | .f64Ge => some [0x66]
  | .arrayLen => some [0xfb, 0x0f]
  | .arrayGet t => (uleb32 t).map ([0xfb, 0x0b] ++ ·)
  | .arrayNewFixed t n =>
      match uleb32 t, uleb32 n with
      | some a, some b => some ([0xfb, 0x08] ++ a ++ b)
      | _, _ => none
  | .refTest t => (s33HeapIdx t).map ([0xfb, 0x14] ++ ·)
  | .refCast t => (s33HeapIdx t).map ([0xfb, 0x16] ++ ·)
  | .ret => some [0x0f]
  | .br d => (uleb32 d).map ([0x0c] ++ ·)
  | .brIf d => (uleb32 d).map ([0x0d] ++ ·)
  | .localTee i => (uleb32 i).map ([0x22] ++ ·)
  | .select => some [0x1b]
  | .i64GtU => some [0x56]
  | .i64Add => some [0x7c]
  | .i64RemS => some [0x81]
  | .i64ShrS => some [0x87]
  | .i64Sub => some [0x7d]
  | .i64Mul => some [0x7e]
  | .i64DivS => some [0x7f]
  | .i64And => some [0x83]
  | .i64Xor => some [0x85]
  | _ => none

/-- Value-type bytes of a source type: the Int carrier, records, sums (their
    root struct), Option and Result are nullable concrete references, Bool is
    `i32`, and the subject scratch is `eqref`. -/
def valTy (M : MCtx) : Ty → Option (List Nat)
  | .int => (s33HeapIdx M.carrier).map ([0x63] ++ ·)
  | .bool => some [0x7f]
  | .record tid =>
      if M.isWord tid then some [0x7e] else (s33HeapIdx (M.structOf tid)).map ([0x63] ++ ·)
  | .sum tid => (s33HeapIdx (M.sumRoot tid)).map ([0x63] ++ ·)
  | .option t => (s33HeapIdx (M.optStruct t)).map ([0x63] ++ ·)
  | .result t e => (s33HeapIdx (M.resStruct t e)).map ([0x63] ++ ·)
  | .eqref => some [0x6d]
  | .float => some [0x7c]
  | .string => (s33HeapIdx M.str).map ([0x63] ++ ·)
  | .vec t => (s33HeapIdx (M.vecStruct t)).map ([0x63] ++ ·)
  | .list t => (s33HeapIdx (M.listStruct t)).map ([0x63] ++ ·)
  | .opaque tid => (s33HeapIdx (M.opaqueStruct tid)).map ([0x63] ++ ·)
  | .bytes => (s33HeapIdx M.bytesArr).map ([0x63] ++ ·)
  | .word => some [0x7e]

mutual
  def encBI (M : MCtx) : BI → Option (List Nat)
    | .op i => encW i
    | .ifElse bt t e =>
        match bt.bind (valTy M), encBL M t, encBL M e with
        | some btB, some tB, some eB => some ([0x04] ++ btB ++ tB ++ [0x05] ++ eB ++ [0x0b])
        | _, _, _ => none
    | .nullOf ht => (s33HeapIdx ht).map ([0xd0] ++ ·)
    | .newData ty seg _ =>
        match uleb32 ty, uleb32 seg with
        | some a, some b => some ([0xfb, 0x09] ++ a ++ b)
        | _, _ => none
    | .castNull ht => (s33HeapIdx ht).map ([0xfb, 0x17] ++ ·)
    | .block bt body =>
        match (match bt with | none => some [0x40] | some t => valTy M t), encBL M body with
        | some btB, some b => some ([0x02] ++ btB ++ b ++ [0x0b])
        | _, _ => none
  def encBL (M : MCtx) : List BI → Option (List Nat)
    | [] => some []
    | x :: xs =>
        match encBI M x, encBL M xs with
        | some a, some b => some (a ++ b)
        | _, _ => none
end

/-- One `(1, type)` local group per declared local, as the emitter declares
    them (`module.rs`, `Function::new` over `(1, ty)` pairs). -/
def localGroups (M : MCtx) : List Ty → Option (List Nat)
  | [] => some []
  | t :: ts =>
      match valTy M t, localGroups M ts with
      | some a, some b => some ([0x01] ++ a ++ b)
      | _, _ => none

/-! ## The self tail loop

A function whose lowering makes a tail call to itself is emitted as one
`loop` (`self_tail_loop.rs`): `loop` with the empty block type, every
declared local reset to its zero value, the body with each self tail call
replaced by `local.set` of the parameters (the last argument first) and a
branch back to the loop, then `return`, `end`, `unreachable`. One `LB` tree
carries both images again: `encLBs` writes its bytes and `eraseLBs` the
interpreter's `CI` loop body. `InterpreterSequencing.wEnter_loop` shows the
loop runs as the original body does. -/

/-- A loop-body instruction: a lowered instruction, an `if` whose arms may
    branch to the loop, `br d`, or the zero value of a local of type `t`. -/
inductive LB where
  | b (x : BI)
  | ifElse (bt : Option Ty) (thenB elseB : List LB)
  | br (depth : Nat)
  | zero (t : Ty)

/-- The instruction that pushes the zero value of a local of type `t`. -/
def zeroW : Ty → WInstr
  | .bool => .i32Const 0
  | .float => .f64Const 0
  | .word => .i64Const 0
  | _ => .refNull

mutual
  def eraseLB : LB → CI
    | .b x => .w (eraseI x)
    | .ifElse _ t e => .ifElse (eraseLBs t) (eraseLBs e)
    | .br d => .br d
    | .zero t => .w (zeroW t)
  def eraseLBs : List LB → List CI
    | [] => []
    | x :: xs => eraseLB x :: eraseLBs xs
end

/-- `local.set (k-1); …; local.set 0`. -/
def setsB : Nat → List LB
  | 0 => []
  | k + 1 => .b (.op (.localSet k)) :: setsB k

/-- `convW` over interpreter instructions, kept as loop-body instructions. -/
def convWB (self n : Nat) : Nat → List WInstr → List LB
  | _, [] => []
  | d, .returnCall g :: rest =>
      (if g = self then setsB n ++ [.br d] else [.b (.op (.returnCall g))]) ++
        convWB self n d rest
  | d, .ifElse t e :: rest =>
      .ifElse none (convWB self n (d + 1) t) (convWB self n (d + 1) e) :: convWB self n d rest
  | d, x :: rest => .b (.op x) :: convWB self n d rest
  termination_by _ xs => sizeOf xs

/-- The loop body of a lowered body: each tail call to `self` (with `n`
    parameters) at `d` enclosing `if`s stores the arguments and branches to
    the loop. -/
def convB (self n : Nat) : Nat → List BI → List LB
  | _, [] => []
  | d, .op x :: rest => convWB self n d [x] ++ convB self n d rest
  | d, .ifElse bt t e :: rest =>
      .ifElse bt (convB self n (d + 1) t) (convB self n (d + 1) e) :: convB self n d rest
  | d, x :: rest => .b x :: convB self n d rest
  termination_by _ xs => sizeOf xs

/-- The loop's entry: the declared local `j` on, each set to its zero. -/
def proB : Nat → List Ty → List LB
  | _, [] => []
  | j, t :: ts => .zero t :: .b (.op (.localSet j)) :: proB (j + 1) ts

/-- The loop body of function `self`'s plan. -/
def loopB (M : MCtx) (self : Nat) (p : FnPlan) : List LB :=
  proB p.sig.params.length p.locals ++
    (convB self p.sig.params.length 0 (lowerB M p.lctx (paramsΓ p.sig.params) true p.body) ++
      [.b (.op .ret)])

/-- The code of function `self`: its plan's lowering, run as the self tail
    loop when the plan says its self tail calls are a loop (`selfLoop`). -/
def fnCodeAt (M : MCtx) (self : Nat) (p : FnPlan) : WCode :=
  if p.selfLoop then
    { fnCode M p with loopBody := some (eraseLBs (loopB M self p)) }
  else fnCode M p

/-- `zeroW t`'s bytes: `i32.const 0`, `f64.const 0`, or `ref.null` of the
    local's heap type. -/
def encZero (M : MCtx) (t : Ty) : Option (List Nat) :=
  match t with
  | .bool => some [0x41, 0x00]
  | .float => some ([0x44] ++ u64le 0)
  | .eqref => some [0xd0, 0x6d]
  | .word => some [0x42, 0x00]
  | _ =>
      match valTy M t with
      | some (0x63 :: h) => some (0xd0 :: h)
      | _ => none

mutual
  def encLB (M : MCtx) : LB → Option (List Nat)
    | .b x => encBI M x
    | .ifElse bt t e =>
        match bt.bind (valTy M), encLBs M t, encLBs M e with
        | some btB, some tB, some eB => some ([0x04] ++ btB ++ tB ++ [0x05] ++ eB ++ [0x0b])
        | _, _, _ => none
    | .br d => (uleb32 d).map ([0x0c] ++ ·)
    | .zero t => encZero M t
  def encLBs (M : MCtx) : List LB → Option (List Nat)
    | [] => some []
    | x :: xs =>
        match encLB M x, encLBs M xs with
        | some a, some b => some (a ++ b)
        | _, _ => none
end

/-- The instruction bytes of function `self`'s body (its final `end`
    excluded): `loop (empty) … end unreachable` around the loop body when the
    plan's self tail calls are a loop (`selfLoop`). -/
def bodyBytes (M : MCtx) (self : Nat) (p : FnPlan) : Option (List Nat) :=
  if p.selfLoop then
    (encLBs M (loopB M self p)).map (fun b => [0x03, 0x40] ++ b ++ [0x0b, 0x00])
  else encBL M (lowerB M p.lctx (paramsΓ p.sig.params) true p.body)

/-- The exact code entry (size prefix included) of function `self`. -/
def codeEntryBytes (M : MCtx) (self : Nat) (p : FnPlan) : Option (List Nat) :=
  match uleb32 p.locals.length, localGroups M p.locals, bodyBytes M self p with
  | some cnt, some groups, some body =>
      let entry := cnt ++ groups ++ body ++ [0x0b]
      (uleb32 entry.length).map (· ++ entry)
  | _, _, _ => none

/-! ### The loop body runs the plan's lowering -/

theorem setsB_erase : ∀ n, eraseLBs (setsB n) = InterpreterSequencing.setsW n := by
  intro n
  induction n with
  | zero => rfl
  | succ k ih => simp [setsB, InterpreterSequencing.setsW, eraseLBs, eraseLB, eraseI, ih]

theorem eraseLBs_append (xs ys : List LB) : eraseLBs (xs ++ ys) = eraseLBs xs ++ eraseLBs ys := by
  induction xs with
  | nil => simp [eraseLBs]
  | cons x xs ih => simp [eraseLBs, ih]

theorem convWB_erase (self n : Nat) (d : Nat) (xs : List WInstr) :
    eraseLBs (convWB self n d xs) = InterpreterSequencing.convW self n d xs := by
  induction d, xs using InterpreterSequencing.convW.induct with
  | case1 d => simp [convWB, InterpreterSequencing.convW, eraseLBs]
  | case2 d g rest ih =>
      rw [convWB, InterpreterSequencing.convW, eraseLBs_append, ih]
      by_cases hg : g = self
      · simp [hg, eraseLBs_append, setsB_erase, eraseLBs, eraseLB]
      · simp [hg, eraseLBs, eraseLB, eraseI]
  | case3 d t e rest iht ihe ihr =>
      rw [convWB, InterpreterSequencing.convW]
      simp [eraseLBs, eraseLB, iht, ihe, ihr]
  | case4 d x rest h1 h2 ih =>
      rw [convWB, InterpreterSequencing.convW]
      · simp [eraseLBs, eraseLB, eraseI, ih]
      all_goals first | exact h1 | exact h2

theorem convW_cons (self n d : Nat) (x : WInstr) (rest : List WInstr) :
    InterpreterSequencing.convW self n d (x :: rest) =
      InterpreterSequencing.convW self n d [x] ++ InterpreterSequencing.convW self n d rest := by
  cases x <;> simp [InterpreterSequencing.convW]

theorem convB_erase (self n : Nat) (d : Nat) (B : List BI) :
    eraseLBs (convB self n d B) = InterpreterSequencing.convW self n d (eraseL B) := by
  induction d, B using convB.induct with
  | case1 d => simp [convB, InterpreterSequencing.convW, eraseLBs, eraseL]
  | case2 d x rest ih =>
      rw [convB, eraseLBs_append, ih, convWB_erase, eraseL, eraseI, convW_cons self n d x (eraseL rest)]
  | case3 d bt t e rest iht ihe ihr =>
      rw [convB]
      simp [eraseLBs, eraseLB, iht, ihe, ihr, eraseL, eraseI, InterpreterSequencing.convW]
  | case4 d x rest h1 h2 ih =>
      rw [convB]
      · cases x <;> simp_all [eraseLBs, eraseLB, eraseL, eraseI, InterpreterSequencing.convW]
      all_goals first | exact h1 | exact h2

theorem proB_erase : ∀ (j : Nat) (ts : List Ty),
    eraseLBs (proB j ts) = InterpreterSequencing.proW j (ts.map zeroW) := by
  intro j ts
  induction ts generalizing j with
  | nil => rfl
  | cons t ts ih => simp [proB, InterpreterSequencing.proW, eraseLBs, eraseLB, eraseI, ih]

theorem zeroW_vals : ∀ (ts : List Ty), ∃ vs,
    (ts.map zeroW).mapM InterpreterSequencing.zeroVal = some vs ∧ vs.length = ts.length := by
  intro ts
  induction ts with
  | nil => exact ⟨[], rfl, rfl⟩
  | cons t ts ih =>
      obtain ⟨vs, hvs, hl⟩ := ih
      have : ∃ v, InterpreterSequencing.zeroVal (zeroW t) = some v := by
        cases t <;> simp [zeroW, InterpreterSequencing.zeroVal]
      obtain ⟨v, hv⟩ := this
      exact ⟨v :: vs, by simp [hv, hvs], by simp [hl]⟩

theorem fnCodeAt_shape (M : MCtx) (self : Nat) (p : FnPlan) :
    (fnCodeAt M self p).arity = (fnCode M p).arity ∧
      (fnCodeAt M self p).nlocals = (fnCode M p).nlocals ∧
      (fnCodeAt M self p).body = (fnCode M p).body := by
  unfold fnCodeAt; split <;> simp

theorem fnCodeAt_plain (M : MCtx) (self : Nat) (p : FnPlan)
    (h : p.selfLoop = false) :
    fnCodeAt M self p = fnCode M p := by
  simp [fnCodeAt, h]

theorem fnCode_loopBody (M : MCtx) (p : FnPlan) : (fnCode M p).loopBody = none := rfl

theorem fnCodeAt_loop (M : MCtx) (self : Nat) (p : FnPlan)
    (h : p.selfLoop = true) :
    ∃ zs vs, InterpreterSequencing.LoopCode (fnCodeAt M self p) self zs vs := by
  obtain ⟨vs, hvs, hl⟩ := zeroW_vals p.locals
  refine ⟨p.locals.map zeroW, vs, ?_, hvs, ?_⟩
  · simp only [fnCodeAt, h, if_true, loopB, eraseLBs_append, proB_erase, convB_erase]
    simp [fnCode, lowerW, eraseLBs, eraseLB, eraseI]
  · rw [hl]; unfold fnCodeAt; split <;> rfl

/-! ## S-3: the byte fact behind the exact `ref.test`

The audited interpreter's `ref.test` compares type indices EXACTLY, while
wasm GC tests subtyping. The variant cascade relies on `ref.test (ref $C)`,
so the certificate is sound only if no represented value has a struct type
that is a strict subtype of (or equivalent to) another constructor's struct.
The emitter declares every user type in ONE rec group that opens the type
section, a sum's root as a non-final empty struct and each constructor as
`sub final root (struct …)` (`module.rs`, `mk_sub_struct(fields, true,
Some(root))`). The pin below is the byte image of that declaration header;
`GrammarSound.ctor_refTest_exact` shows it makes the exact test the wasm
test. The acceptance checks `S3Pin` against the raw entries of the rec group
that opens the type section, together with `sumOk`
(`TypeTable.sumConfirmed`). -/

/-- The type-section header of a constructor struct: `0x4f` (`sub final`),
    one supertype, the sum's root. -/
def ctorEntryHeader (root : Nat) : Option (List Nat) :=
  (uleb32 root).map ([0x4f, 0x01] ++ ·)

/-- The S-3 pin over `entries`, the byte image of the rec group that opens
    the type section (entry `k` is the subtype declared at type index `k`):
    every constructor struct of sum `tid` (constructors `0 … ncs-1`) is an
    entry of that group and is declared `sub final` under the sum's root. -/
def S3Pin (M : MCtx) (tid ncs : Nat) (entries : List (List Nat)) : Bool :=
  (List.range ncs).all fun c =>
    match entries[M.ctorStruct tid c]?, ctorEntryHeader (M.sumRoot tid) with
    | some e, some h => h.isPrefixOf e
    | _, _ => false

/-! ## S-11: the byte fact behind a string literal

The interpreter's `arrayNewData` carries the literal's bytes, while the code
entry names only a passive data segment (`array.new_data $string seg`, with
offset `0` and the literal's length as operands). The certificate is sound
only if that segment holds exactly those bytes. `exprLits` lists every
string literal a plan lowers to `array.new_data` (literal nodes and literal
match arms), and `DataPin` checks each against the module's data segments
(`segs[i]` is the contents of segment `i`). The acceptance checks it against
the decoded data section (`TypeTable.dataConfirmed`). -/

mutual
  def exprLits : Expr → List (List Nat)
    | .literal (.str b) => [b]
    | .literal _ => []
    | .local _ => []
    | .let_ _ v body => exprLits v ++ exprLits body
    | .call _ args => argsLits args
    | .tailCall _ args => argsLits args
    | .binOp _ l r => exprLits l ++ exprLits r
    | .neg e => exprLits e
    | .ifThenElse c t e => exprLits c ++ exprLits t ++ exprLits e
    | .recordCreate _ fs => argsLits fs
    | .project _ _ b => exprLits b
    | .match_ s arms => exprLits s ++ armsLits arms
    | .construct _ _ args => argsLits args
    | .interp parts => argsLits parts
    | .list _ items => argsLits items
    | .try_ e _ => exprLits e
    | .scope e => exprLits e
    | .intWord _ _ => []
    | .intCmp _ _ _ _ => []
  def argsLits : List Expr → List (List Nat)
    | [] => []
    | e :: es => exprLits e ++ argsLits es
  def armsLits : Arms → List (List Nat)
    | .nil => []
    | .cons p b rest =>
        (match p with
         | .litStr k => [k]
         | _ => []) ++ exprLits b ++ armsLits rest
end

/-- The S-11 pin: every string literal of the plan names a data segment that
    holds exactly its bytes. -/
def DataPin (M : MCtx) (segs : List (List Nat)) (p : FnPlan) : Bool :=
  (exprLits p.body).all fun b => segs[M.strSeg b]? == some b

end AverCert.Grammar
