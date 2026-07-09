-- Lean-side structural checker for `expr-fragment-v1` raw plans.
--
-- This is intentionally a small checker over the plan grammar, not a Wasm
-- decoder. v1 still binds plans to bytes in Rust by canonical lowering and raw
-- code-entry equality; this module is the trusted Lean landing zone for v2:
-- `RawPlan -> CheckedPlan -> LowersCodeEntry`.
import Schema

namespace AverCert.PlanCheck
open AverCert.Schema

def sameTy (a b : FragTy) : Bool :=
  if a = b then true else false

def sameSymTy (a b : SymTy) : Bool :=
  if a = b then true else false

def lookupNode (nodes : List FragNode) (id : Nat) : Option FragNode :=
  nodes[id]?

def lookupSymNode (nodes : List SymNode) (id : Nat) : Option SymNode :=
  nodes[id]?

def lookupTy (nodes : List FragNode) (id : Nat) : Option FragTy :=
  match lookupNode nodes id with
  | some n => some n.ty
  | none => none

def lookupSymTy (nodes : List SymNode) (id : Nat) : Option SymTy :=
  match lookupSymNode nodes id with
  | some n => some n.ty
  | none => none

def hasTy (nodes : List FragNode) (id : Nat) (expected : FragTy) : Bool :=
  match lookupTy nodes id with
  | some got => sameTy got expected
  | none => false

def hasSymTy (nodes : List SymNode) (id : Nat) (expected : SymTy) : Bool :=
  match lookupSymTy nodes id with
  | some got => sameSymTy got expected
  | none => false

def isSymParam (nodes : List SymNode) (id : Nat) : Bool :=
  match lookupSymNode nodes id with
  | some { kind := .param _, .. } => true
  | _ => false

def hasI32Ty (nodes : List FragNode) (id : Nat) : Bool :=
  match lookupTy nodes id with
  | some .rawI32 => true
  | some .boolI32 => true
  | _ => false

def carrierFieldTy? : Nat → Option FragTy
  | 0 => some .i64
  | 1 => some .ref
  | 2 => some .rawI32
  | _ => none

def isCarrierLimbField (nodes : List FragNode) (id : Nat) : Bool :=
  match lookupNode nodes id with
  | some { kind := .structGet 1 _, .. } => true
  | _ => false

def argsHaveTys (nodes : List FragNode) : List Nat → List FragTy → Bool
  | [], [] => true
  | arg :: args, ty :: tys => hasTy nodes arg ty && argsHaveTys nodes args tys
  | _, _ => false

def symArgsHaveTys (nodes : List SymNode) : List Nat → List SymTy → Bool
  | [], [] => true
  | arg :: args, ty :: tys => hasSymTy nodes arg ty && symArgsHaveTys nodes args tys
  | _, _ => false

def symArgsAllTy (nodes : List SymNode) (expected : SymTy) : List Nat → Bool
  | [] => true
  | arg :: args => hasSymTy nodes arg expected && symArgsAllTy nodes expected args

def symArgsExist (nodes : List SymNode) : List Nat → Bool
  | [] => true
  | arg :: args =>
      match lookupSymNode nodes arg with
      | some _ => symArgsExist nodes args
      | none => false

def primResultTy? (nodes : List FragNode) (op : FragPrim) (args : List Nat) :
    Option FragTy :=
  match op with
  | .f64Add =>
      if argsHaveTys nodes args [.f64, .f64] then some .f64 else none
  | .f64Mul =>
      if argsHaveTys nodes args [.f64, .f64] then some .f64 else none
  | .f64Le =>
      if argsHaveTys nodes args [.f64, .f64] then some .boolI32 else none
  | .i64Eq =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i64LeS =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i64LtS =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i64GeS =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i32LtS =>
      match args with
      | [a, b] => if hasI32Ty nodes a && hasI32Ty nodes b then some .boolI32 else none
      | _ => none
  | .i32GtS =>
      match args with
      | [a, b] => if hasI32Ty nodes a && hasI32Ty nodes b then some .boolI32 else none
      | _ => none

/-- Static registry of host-helper role type signatures. `box` takes one raw
    `i64` and returns the Int carrier; `add` takes two Int carriers and returns
    the Int carrier. The resolved wasm function index is not checked here (it is
    bound to the module bytes by the byte-exact gate and to the byte-derived
    role table by the Rust checker); this is purely the representation-level
    type discipline. -/
def hostCallResultTy? (nodes : List FragNode) (role : HostRole) (args : List Nat) :
    Option FragTy :=
  match role with
  | .box => if argsHaveTys nodes args [.i64] then some .intCarrier else none
  | .add =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .intCarrier else none

def symPrimResultTy? (nodes : List SymNode) (op : SymPrim) (args : List Nat) :
    Option SymTy :=
  match op with
  | .floatAdd =>
      if symArgsHaveTys nodes args [.float, .float] then some .float else none
  | .floatMul =>
      if symArgsHaveTys nodes args [.float, .float] then some .float else none
  | .floatLe =>
      if symArgsHaveTys nodes args [.float, .float] then some .bool else none
  | .intAdd =>
      if symArgsHaveTys nodes args [.int, .int] then some .int else none
  | .stringEq =>
      if symArgsHaveTys nodes args [.string, .string] then some .bool else none
  | .stringConcat =>
      if args.isEmpty then none
      else if symArgsAllTy nodes .string args then some .string else none

/-- Hard cap for recursive plan checking. Exceeding it is a fail-closed
    unsupported fragment, matching the profile-limit discipline on the Rust
    side. -/
def maxFuel : Nat := 10000

def isByte (n : Nat) : Bool :=
  if n <= 255 then true else false

def bytesAllBytes : List Nat → Bool
  | [] => true
  | b :: bs => isByte b && bytesAllBytes bs

def checkBlockFuel : Nat → List FragTy → FragBlock → Bool
  | 0, _, _ => false
  | fuel + 1, params, block =>
      let inferNodeKindTy (checked : List FragNode) (kind : FragNodeKind) :
          Option FragTy :=
        match kind with
        | .local index => params[index]?
        | .constBool _ => some .boolI32
        | .constI64 _ => some .i64
        | .constI32 _ => some .rawI32
        | .constF64Bits _ => some .f64
        | .structGet field receiver =>
            if hasTy checked receiver .intCarrier then carrierFieldTy? field else none
        | .refIsNull value =>
            if hasTy checked value .ref && isCarrierLimbField checked value
            then some .boolI32
            else none
        | .prim op args => primResultTy? checked op args
        | .hostCall role _funcIdx args => hostCallResultTy? checked role args
        | .ifElse cond thenBlock elseBlock =>
            if hasTy checked cond .boolI32 &&
               checkBlockFuel fuel params thenBlock &&
               checkBlockFuel fuel params elseBlock then
              match lookupNode thenBlock.nodes thenBlock.result,
                    lookupNode elseBlock.nodes elseBlock.result with
              | some t, some e => if t.ty = e.ty then some t.ty else none
              | _, _ => none
            else none
      let rec checkNodes (checked : List FragNode) : List FragNode → Bool
        | [] => true
        | node :: rest =>
            node.id = checked.length &&
              (match inferNodeKindTy checked node.kind with
              | some ty => sameTy node.ty ty
              | none => false) &&
              checkNodes (checked ++ [node]) rest
      checkNodes [] block.nodes &&
        match lookupNode block.nodes block.result with
        | some n => n.id = block.result && block.result + 1 = block.nodes.length
        | none => false

def checkBlock (params : List FragTy) (block : FragBlock) : Bool :=
  checkBlockFuel maxFuel params block

def checkSymBlockFuel : Nat → List SymTy → SymBlock → Bool
  | 0, _, _ => false
  | fuel + 1, params, block =>
      let inferNodeKindTy (checked : List SymNode) (kind : SymNodeKind) :
          Option SymTy :=
        match kind with
        | .param index => params[index]?
        | .constBool _ => some .bool
        | .constInt _ => some .int
        | .constFloatBits _ => some .float
        | .constStringBytes bytes =>
            if bytesAllBytes bytes then some .string else none
        | .prim op args => symPrimResultTy? checked op args
        | .construct typeName _ args =>
            if symArgsExist checked args then
              some (.named typeName)
            else
              none
        | .intConstCmp _ value _ =>
            if hasSymTy checked value .int && isSymParam checked value then some .bool else none
        | .ifElse cond thenBlock elseBlock =>
            if hasSymTy checked cond .bool &&
               checkSymBlockFuel fuel params thenBlock &&
               checkSymBlockFuel fuel params elseBlock then
              match lookupSymNode thenBlock.nodes thenBlock.result,
                    lookupSymNode elseBlock.nodes elseBlock.result with
              | some t, some e => if t.ty = e.ty then some t.ty else none
              | _, _ => none
            else none
      let rec checkNodes (checked : List SymNode) : List SymNode → Bool
        | [] => true
        | node :: rest =>
            node.id = checked.length &&
              (match inferNodeKindTy checked node.kind with
              | some ty => sameSymTy node.ty ty
              | none => false) &&
              checkNodes (checked ++ [node]) rest
      checkNodes [] block.nodes &&
        match lookupSymNode block.nodes block.result with
        | some n => n.id = block.result && block.result + 1 = block.nodes.length
        | none => false

def checkSymBlock (params : List SymTy) (block : SymBlock) : Bool :=
  checkSymBlockFuel maxFuel params block

def checkExprFragmentRawPlan (plan : ExprFragmentRawPlan) : Bool :=
  plan.profile = "expr-fragment-v1" &&
    checkBlock plan.params plan.body &&
    match lookupNode plan.body.nodes plan.body.result with
    | some n => sameTy n.ty plan.result
    | none => false

def checkSymRawPlan (plan : SymRawPlan) : Bool :=
  plan.profile = "sym-fragment-v1" &&
    checkSymBlock plan.params plan.body &&
    match lookupSymNode plan.body.nodes plan.body.result with
    | some n => sameSymTy n.ty plan.result
    | none => false

def byteChunksAllBytes : List (List Nat) → Bool
  | [] => true
  | bytes :: rest => bytesAllBytes bytes && byteChunksAllBytes rest

def stringConcatChunksAllBytes : List StringConcatChunk → Bool
  | [] => true
  | chunk :: rest => bytesAllBytes chunk.bytes && stringConcatChunksAllBytes rest

def checkStringConcatRawPlan (plan : StringConcatRawPlan) : Bool :=
  plan.profile = "string-concat-v1" &&
    stringConcatChunksAllBytes plan.prefixes &&
    stringConcatChunksAllBytes plan.suffixes

def stringConcatChunkBytes : StringConcatChunk → List Nat
  | { bytes, .. } => bytes

def stringEqChunkBytes : StringEqChunk → List Nat
  | { bytes, .. } => bytes

inductive SymStringConcatPart where
  | literal (bytes : List Nat)
  | input
deriving Repr, DecidableEq

inductive SymStringEqResult where
  | literal (bytes : List Nat)
  | input
deriving Repr, DecidableEq

def stringEqResultBytes? : StringEqResult → Option SymStringEqResult
  | .input => some .input
  | .literal chunk =>
      if bytesAllBytes chunk.bytes then some (.literal chunk.bytes) else none

def symStringConcatPart? (nodes : List SymNode) (id : Nat) :
    Option SymStringConcatPart :=
  match lookupSymNode nodes id with
  | some { ty := .string, kind := .constStringBytes bytes, .. } =>
      if bytesAllBytes bytes then some (.literal bytes) else none
  | some { ty := .string, kind := .param 0, .. } => some .input
  | _ => none

def splitSymStringConcatParts :
    List SymStringConcatPart →
    Option (List (List Nat) × List (List Nat)) :=
  let rec go
      (seenInput : Bool)
      (prefixes suffixes : List (List Nat)) :
      List SymStringConcatPart →
      Option (List (List Nat) × List (List Nat))
    | [] =>
        if seenInput then some (prefixes, suffixes) else none
    | .input :: rest =>
        if seenInput then none else go true prefixes suffixes rest
    | .literal bytes :: rest =>
        if seenInput then
          go seenInput prefixes (suffixes ++ [bytes]) rest
        else
          go seenInput (prefixes ++ [bytes]) suffixes rest
  go false [] []

def symStringConcatParts? (plan : SymRawPlan) :
    Option (List (List Nat) × List (List Nat)) :=
  if checkSymRawPlan plan &&
     plan.params = [.string] &&
     plan.result = .string then
    match lookupSymNode plan.body.nodes plan.body.result with
    | some { kind := .prim .stringConcat args, .. } =>
        if args = List.range args.length &&
           args.length + 1 = plan.body.nodes.length then
          match args.mapM (symStringConcatPart? plan.body.nodes) with
          | some parts => splitSymStringConcatParts parts
          | none => none
        else none
    | _ => none
  else none

def stringConcatPlanMatchesSymRawPlan
    (symPlan : SymRawPlan)
    (plan : StringConcatRawPlan) : Bool :=
  match symStringConcatParts? symPlan with
  | some (prefixes, suffixes) =>
      prefixes = plan.prefixes.map stringConcatChunkBytes &&
      suffixes = plan.suffixes.map stringConcatChunkBytes
  | none => false

def checkStringEqResult : StringEqResult → Bool
  | .input => true
  | .literal chunk => bytesAllBytes chunk.bytes

def checkStringEqRawPlan (plan : StringEqRawPlan) : Bool :=
  plan.profile = "string-eq-v1" &&
    bytesAllBytes plan.needle.bytes &&
    checkStringEqResult plan.hit &&
    checkStringEqResult plan.default

def symStringEqResult? (block : SymBlock) : Option SymStringEqResult :=
  match block.nodes, lookupSymNode block.nodes block.result with
  | [_], some { ty := .string, kind := .constStringBytes bytes, .. } =>
      if bytesAllBytes bytes then some (.literal bytes) else none
  | [_], some { ty := .string, kind := .param 0, .. } => some .input
  | _, _ => none

def symStringEqParts? (plan : SymRawPlan) :
    Option (List Nat × SymStringEqResult × SymStringEqResult) :=
  if checkSymRawPlan plan &&
     plan.params = [.string] &&
     plan.result = .string then
    match lookupSymNode plan.body.nodes plan.body.result with
    | some { ty := .string, kind := .ifElse cond thenBlock elseBlock, .. } =>
        match lookupSymNode plan.body.nodes cond,
              symStringEqResult? thenBlock,
              symStringEqResult? elseBlock with
        | some { ty := .bool, kind := .prim .stringEq [input, needle], .. },
          some hit,
          some default =>
            match lookupSymNode plan.body.nodes input,
                  lookupSymNode plan.body.nodes needle with
            | some { ty := .string, kind := .param 0, .. },
              some { ty := .string, kind := .constStringBytes bytes, .. } =>
                if bytesAllBytes bytes then some (bytes, hit, default) else none
            | _, _ => none
        | _, _, _ => none
    | _ => none
  else none

def stringEqPlanMatchesSymRawPlan
    (symPlan : SymRawPlan)
    (plan : StringEqRawPlan) : Bool :=
  if checkStringEqRawPlan plan then
    match symStringEqParts? symPlan,
          stringEqResultBytes? plan.hit,
          stringEqResultBytes? plan.default with
    | some (needle, hit, default), some planHit, some planDefault =>
        needle = plan.needle.bytes &&
        hit = planHit &&
        default = planDefault
    | _, _, _ => false
  else false

def constructFieldOk (arity : Nat) : ConstructField → Bool
  | .local index => index < arity
  -- `ref.null` byte lowering needs an explicit heap-type binding. The semantic
  -- constructor exists in the schema, but `construct-v1` accepts only local
  -- argument fields until that binding is part of the plan.
  | .null => false

def constructFieldsOk (arity : Nat) : List ConstructField → Bool
  | [] => true
  | field :: rest => constructFieldOk arity field && constructFieldsOk arity rest

def constructLocalFields : List ConstructField → List Nat
  | [] => []
  | .local index :: rest => index :: constructLocalFields rest
  | .null :: rest => constructLocalFields rest

def natListNoDup : List Nat → Bool
  | [] => true
  | n :: rest => (!rest.contains n) && natListNoDup rest

def rangeAllContained (locals : List Nat) : Nat → Bool
  | 0 => true
  | n + 1 => rangeAllContained locals n && locals.contains n

def constructUsesAllParams (arity : Nat) (fields : List ConstructField) : Bool :=
  let locals := constructLocalFields fields
  locals.length = arity &&
    natListNoDup locals &&
    rangeAllContained locals arity

def checkConstructRawPlan (plan : ConstructRawPlan) : Bool :=
  plan.profile = "construct-v1" &&
    0 < plan.arity &&
    0 < plan.fields.length &&
    constructFieldsOk plan.arity plan.fields &&
    constructUsesAllParams plan.arity plan.fields

def symConstructArgs? (plan : SymRawPlan) : Option (String × String × List Nat) :=
  if checkSymRawPlan plan then
    match lookupSymNode plan.body.nodes plan.body.result with
    | some { ty := .named typeName, kind := .construct _ ctorName args, .. } =>
        some (typeName, ctorName, args)
    | _ => none
  else none

def constructPlanMatchesSymRawPlan
    (symPlan : SymRawPlan)
    (plan : ConstructRawPlan) : Bool :=
  if checkConstructRawPlan plan then
    match symConstructArgs? symPlan with
    | some (_, _, args) =>
        args = constructLocalFields plan.fields &&
          args.length = plan.arity
    | none => false
  else false

def encodeSymTy? : SymTy → Option FragTy
  | .float => some .f64
  | .bool => some .boolI32
  | .int => some .intCarrier
  | .string => none
  | .named _ => none

def encodeSymTys? : List SymTy → Option (List FragTy)
  | [] => some []
  | ty :: tys =>
      match encodeSymTy? ty, encodeSymTys? tys with
      | some fragTy, some fragTys => some (fragTy :: fragTys)
      | _, _ => none

def encodeSymPrim? : SymPrim → Option FragPrim
  | .floatAdd => some .f64Add
  | .floatMul => some .f64Mul
  | .floatLe => some .f64Le
  -- `intAdd` has no representation-level primitive: the encoder binds it to a
  -- `hostCall .add` node through the byte-derived host-role table instead.
  | .intAdd => none
  | .stringEq => none
  | .stringConcat => none

/-- Look up the resolved wasm function index for one host role in the
    byte-derived role table an artifact claim carries. A role the table lacks
    fail-closes the encoding (`none`). -/
def hostRoleIdx? (hostTable : List (HostRole × Nat)) (role : HostRole) : Option Nat :=
  match hostTable with
  | [] => none
  | (r, idx) :: rest => if r = role then some idx else hostRoleIdx? rest role

def symIntSmallConstCmpPrim? : SymIntCmp → Option FragPrim
  | .eq => some .i64Eq
  | .lt => some .i64LtS
  | .le => some .i64LeS
  | .ge => some .i64GeS

inductive SymBigIntConstCmpKind where
  | always (value : Bool)
  | signLtZero
  | signGtZero

def symIntBigConstCmpKind? : SymIntCmp → Option SymBigIntConstCmpKind
  | .eq => some (.always false)
  | .lt => some .signLtZero
  | .le => some .signLtZero
  | .ge => some .signGtZero

def appendFragNode
    (nodes : List FragNode)
    (ty : FragTy)
    (kind : FragNodeKind) : List FragNode × Nat :=
  let id := nodes.length
  (nodes ++ [{ id := id, ty := ty, kind := kind }], id)

def encodeIntSmallConstCmpBlock? (index : Nat) (op : SymIntCmp) (k : Int) :
    Option FragBlock := do
  let prim ← symIntSmallConstCmpPrim? op
  let (nodes, carrier) := appendFragNode [] .intCarrier (.local index)
  let (nodes, small) := appendFragNode nodes .i64 (.structGet 0 carrier)
  let (nodes, constant) := appendFragNode nodes .i64 (.constI64 k)
  let (nodes, result) := appendFragNode nodes .boolI32 (.prim prim [small, constant])
  some { nodes := nodes, result := result }

def encodeIntBigConstCmpBlock? (index : Nat) (op : SymIntCmp) :
    Option FragBlock := do
  match symIntBigConstCmpKind? op with
  | some (.always value) =>
      let (nodes, result) := appendFragNode [] .boolI32 (.constBool value)
      some { nodes := nodes, result := result }
  | some .signLtZero =>
      let (nodes, carrier) := appendFragNode [] .intCarrier (.local index)
      let (nodes, sign) := appendFragNode nodes .rawI32 (.structGet 2 carrier)
      let (nodes, zeroId) := appendFragNode nodes .boolI32 (.constBool false)
      let (nodes, result) := appendFragNode nodes .boolI32 (.prim .i32LtS [sign, zeroId])
      some { nodes := nodes, result := result }
  | some .signGtZero =>
      let (nodes, carrier) := appendFragNode [] .intCarrier (.local index)
      let (nodes, sign) := appendFragNode nodes .rawI32 (.structGet 2 carrier)
      let (nodes, zeroId) := appendFragNode nodes .boolI32 (.constBool false)
      let (nodes, result) := appendFragNode nodes .boolI32 (.prim .i32GtS [sign, zeroId])
      some { nodes := nodes, result := result }
  | none => none

structure SymEncodeState where
  nodes      : List FragNode
  symToFrag  : List Nat

def sourceParamIndex? (nodes : List SymNode) (id : Nat) : Option Nat :=
  match lookupSymNode nodes id with
  | some { ty := .int, kind := .param index, .. } => some index
  | _ => none

def encodedValue? (st : SymEncodeState) (id : Nat) : Option Nat :=
  st.symToFrag[id]?

def pushEncodedNode
    (st : SymEncodeState)
    (ty : FragTy)
    (kind : FragNodeKind) : SymEncodeState × Nat :=
  let (nodes, id) := appendFragNode st.nodes ty kind
  ({ st with nodes := nodes }, id)

def encodeSymBlockFuel : Nat → List (HostRole × Nat) → SymBlock → Option FragBlock
  | 0, _, _ => none
  | fuel + 1, hostTable, block =>
      let encodeNode (st : SymEncodeState) (node : SymNode) : Option SymEncodeState := do
        if node.id = st.symToFrag.length then
          let fragTy ← encodeSymTy? node.ty
          match node.kind with
          | .param index =>
              let (st, id) := pushEncodedNode st fragTy (.local index)
              some { st with symToFrag := st.symToFrag ++ [id] }
          | .constBool value =>
              let (st, id) := pushEncodedNode st fragTy (.constBool value)
              some { st with symToFrag := st.symToFrag ++ [id] }
          | .constInt value =>
              -- A source Int literal is representation-boxed at the point of
              -- appearance: push the raw `i64` constant, then the byte-derived
              -- `box` host call; the source node maps to the boxed carrier.
              let boxIdx ← hostRoleIdx? hostTable .box
              let (st, constId) := pushEncodedNode st .i64 (.constI64 value)
              let (st, boxedId) :=
                pushEncodedNode st fragTy (.hostCall .box boxIdx [constId])
              some { st with symToFrag := st.symToFrag ++ [boxedId] }
          | .constFloatBits bits =>
              let (st, id) := pushEncodedNode st fragTy (.constF64Bits bits)
              some { st with symToFrag := st.symToFrag ++ [id] }
          | .constStringBytes _ => none
          | .prim op args =>
              match op with
              | .intAdd =>
                  let addIdx ← hostRoleIdx? hostTable .add
                  let fragArgs ← args.mapM (encodedValue? st)
                  let (st, id) :=
                    pushEncodedNode st fragTy (.hostCall .add addIdx fragArgs)
                  some { st with symToFrag := st.symToFrag ++ [id] }
              | _ =>
                  let prim ← encodeSymPrim? op
                  let fragArgs ← args.mapM (encodedValue? st)
                  let (st, id) := pushEncodedNode st fragTy (.prim prim fragArgs)
                  some { st with symToFrag := st.symToFrag ++ [id] }
          | .construct _ _ _ => none
          | .intConstCmp op value constant =>
              let carrier ← encodedValue? st value
              let index ← sourceParamIndex? block.nodes value
              let (st, magf) := pushEncodedNode st .ref (.structGet 1 carrier)
              let (st, isSmall) := pushEncodedNode st .boolI32 (.refIsNull magf)
              let thenBlock ← encodeIntSmallConstCmpBlock? index op constant
              let elseBlock ← encodeIntBigConstCmpBlock? index op
              let (st, id) := pushEncodedNode st .boolI32 (.ifElse isSmall thenBlock elseBlock)
              some { st with symToFrag := st.symToFrag ++ [id] }
          | .ifElse cond thenBlock elseBlock =>
              let cond ← encodedValue? st cond
              let thenFrag ← encodeSymBlockFuel fuel hostTable thenBlock
              let elseFrag ← encodeSymBlockFuel fuel hostTable elseBlock
              let (st, id) := pushEncodedNode st fragTy (.ifElse cond thenFrag elseFrag)
              some { st with symToFrag := st.symToFrag ++ [id] }
        else
          none
      let rec encodeNodes (st : SymEncodeState) : List SymNode → Option SymEncodeState
        | [] => some st
        | node :: rest =>
            match encodeNode st node with
            | some st => encodeNodes st rest
            | none => none
      match encodeNodes { nodes := [], symToFrag := [] } block.nodes with
      | some st =>
          match st.symToFrag[block.result]? with
          | some result => some { nodes := st.nodes, result := result }
          | none => none
      | none => none

def encodeSymBlock? (hostTable : List (HostRole × Nat)) (block : SymBlock) :
    Option FragBlock :=
  encodeSymBlockFuel maxFuel hostTable block

def encodeSymRawPlanToExprFragmentRawPlan
    (hostTable : List (HostRole × Nat)) (plan : SymRawPlan) :
    Option ExprFragmentRawPlan :=
  if checkSymRawPlan plan then
    match encodeSymTys? plan.params, encodeSymTy? plan.result,
          encodeSymBlock? hostTable plan.body with
    | some params, some result, some body =>
        some { profile := "expr-fragment-v1", params := params, result := result, body := body }
    | _, _, _ => none
  else
    none

end AverCert.PlanCheck
