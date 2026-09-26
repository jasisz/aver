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

mutual
  def eraseI : BI → WInstr
    | .op i => i
    | .ifElse _ t e => .ifElse (eraseL t) (eraseL e)
    | .nullOf _ => .refNull
    | .newData ty _ bytes => .arrayNewData ty bytes
    | .castNull ht => .refCast ht
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

/-- The instruction a builtin call ends with, given its argument types:
    one `i32` instruction for the Bool builtins, `struct.new` of the tail's
    cons struct for `List.prepend`. -/
def builtinTail (M : MCtx) : Builtin → Option (List Ty) → List BI
  | .boolAnd, _ => [.op .i32And]
  | .boolOr, _ => [.op .i32Or]
  | .boolNot, _ => [.op .i32Eqz]
  | .listPrepend, some [_, .list t] => [.op (.structNew (M.listStruct t) 2)]
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
  | .record tid => [.nullOf (M.structOf tid)]
  | .sum tid => [.nullOf (M.sumRoot tid)]
  | .option t => [.nullOf (M.optStruct t)]
  | .result t e => [.nullOf (M.resStruct t e)]
  | .string => [.nullOf M.str]
  | .float => [.op (.f64Const 0)]
  | .list t => [.nullOf (M.listStruct t)]
  | .vec t => [.nullOf (M.vecStruct t)]
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

/-- The `want_mod` flag of a Euclidean intrinsic. -/
def Intrinsic.flag : Intrinsic → Int
  | .intDivEuclid => 0
  | .intModEuclid => 1

/-! ## The lowering -/

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
        lowerArgsB M X Γ fs ++ [.op (.structNew (M.structOf tid) fs.length)]
    | .project tid i base => lowerB M X Γ false base ++ [.op (.structGet (M.structOf tid) i)]
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
  | _ => none

/-- Value-type bytes of a source type: the Int carrier, records, sums (their
    root struct), Option and Result are nullable concrete references, Bool is
    `i32`, and the subject scratch is `eqref`. -/
def valTy (M : MCtx) : Ty → Option (List Nat)
  | .int => (s33HeapIdx M.carrier).map ([0x63] ++ ·)
  | .bool => some [0x7f]
  | .record tid => (s33HeapIdx (M.structOf tid)).map ([0x63] ++ ·)
  | .sum tid => (s33HeapIdx (M.sumRoot tid)).map ([0x63] ++ ·)
  | .option t => (s33HeapIdx (M.optStruct t)).map ([0x63] ++ ·)
  | .result t e => (s33HeapIdx (M.resStruct t e)).map ([0x63] ++ ·)
  | .eqref => some [0x6d]
  | .float => some [0x7c]
  | .string => (s33HeapIdx M.str).map ([0x63] ++ ·)
  | .vec t => (s33HeapIdx (M.vecStruct t)).map ([0x63] ++ ·)
  | .list t => (s33HeapIdx (M.listStruct t)).map ([0x63] ++ ·)
  | .opaque tid => (s33HeapIdx (M.opaqueStruct tid)).map ([0x63] ++ ·)

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

/-- The exact code entry (size prefix included) of one function. -/
def codeEntryBytes (M : MCtx) (p : FnPlan) : Option (List Nat) :=
  match uleb32 p.locals.length, localGroups M p.locals,
      encBL M (lowerB M p.lctx (paramsΓ p.sig.params) true p.body) with
  | some cnt, some groups, some body =>
      let entry := cnt ++ groups ++ body ++ [0x0b]
      (uleb32 entry.length).map (· ++ entry)
  | _, _, _ => none

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
