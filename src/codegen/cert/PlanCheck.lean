-- Lean-side structural checker for `expr-fragment-v1` raw plans.
--
-- This is intentionally a small checker over the plan grammar, not a Wasm
-- decoder. v1 still binds plans to bytes in Rust by canonical lowering and raw
-- code-entry equality; this module is the trusted Lean landing zone for v2:
-- `RawPlan -> CheckedPlan -> LowersCodeEntry`.
import SchemaCore

namespace AverCert.PlanCheck
open AverCert.Schema

/-- Dedicated bare-projection structural guard. The byte-derived field count is
    an argument rather than plan data, so the only plan claim (`fieldIdx`) must
    be in range for the module's actual struct. -/
def checkFieldProjectionRawPlan
    (fieldCount : Nat) (plan : FieldProjectionRawPlan) : Bool :=
  plan.profile = "field-projection-v1" &&
    fieldCount = 2 &&
    plan.fieldIdx < fieldCount

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
    `i64` and returns the Int carrier; each arithmetic role takes two Int
    carriers and returns the Int carrier. The resolved wasm function index is
    not checked here (it is bound to the module bytes by the byte-exact gate
    and to the in-kernel decoded role table); this is purely the
    representation-level type discipline. -/
def hostCallResultTy? (nodes : List FragNode) (role : HostRole) (args : List Nat) :
    Option FragTy :=
  match role with
  | .box => if argsHaveTys nodes args [.i64] then some .intCarrier else none
  | .add =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .intCarrier else none
  | .mul =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .intCarrier else none
  | .sub =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .intCarrier else none

/-- All arguments of a self-call must be Int carriers; the recursion class only
    threads Int values through its recursive descent. -/
def fragArgsAllTy (nodes : List FragNode) (expected : FragTy) : List Nat → Bool
  | [] => true
  | arg :: args => hasTy nodes arg expected && fragArgsAllTy nodes expected args

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
        -- v1 admits only opaque reference fields out of user structs: the
        -- projected value is handled verbatim (`adtRef`), never reinterpreted
        -- as a scalar. Scalar-field projections need their own proof face
        -- before they can be typed here. The node's `tyIdx`/`field` are bound
        -- to the module bytes by the byte-exact gate and validated against the
        -- byte-derived struct context by the Rust checker.
        | .structGetUser _tyIdx _field value =>
            if hasTy checked value .adtRef then some .adtRef else none
        | .refIsNull value =>
            if hasTy checked value .ref && isCarrierLimbField checked value
            then some .boolI32
            else none
        | .prim op args => primResultTy? checked op args
        | .hostCall role _funcIdx args => hostCallResultTy? checked role args
        -- A self-call yields the Int carrier when every argument is an Int
        -- carrier. `funcIdx` is not typed here (the byte gate binds it and the
        -- Rust checker validates it against the byte-derived self index),
        -- mirroring `hostCall`.
        | .selfCall _tail _funcIdx args =>
            if !args.isEmpty && fragArgsAllTy checked .intCarrier args then
              some .intCarrier
            else none
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
            if typeName = "List" then
              match args with
              | [head, tail] =>
                  match lookupSymTy checked head, lookupSymTy checked tail with
                  | some headTy, some (.app1 "List" elemTy) =>
                      if headTy = elemTy then some (.app1 "List" elemTy) else none
                  | _, _ => none
              | _ => none
            else if symArgsExist checked args then
              some (.named typeName)
            else none
        | .emptyList elemTy => some (.app1 "List" elemTy)
        -- Field projection is typed by the claimed field type; the claim is
        -- honest because encoding + byte-exact lowering pin the projection to
        -- the module's real struct layout (a lying `fieldTy` encodes to a
        -- fragment the byte gate rejects).
        | .projectField typeName _field fieldTy value =>
            if hasSymTy checked value (.named typeName) then some fieldTy else none
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

def checkRecursionRawPlan (plan : RecursionRawPlan) : Bool :=
  plan.profile = "recursion-plan-v1" &&
    checkBlock plan.params plan.body &&
    match lookupNode plan.body.nodes plan.body.result with
    | some n => sameTy n.ty plan.result
    | none => false

def checkMutualRawPlan (plan : MutualRawPlan) : Bool :=
  plan.profile = "mutual-plan-v1" &&
    checkBlock plan.params plan.body &&
    match lookupNode plan.body.nodes plan.body.result with
    | some n => sameTy n.ty plan.result
    | none => false

/-- Generic composition-plan discipline. A chain must contain at least one
    call; context-sensitive target/closure checks live in AcceptedArtifactCore,
    after names have been resolved from byte-derived export bindings. -/
def checkCompositionRawPlan (plan : CompositionRawPlan) : Bool :=
  plan.profile = "composition-plan-v1" &&
    match plan.shape with
    | .selfSum => true
    | .chain callees => !callees.isEmpty

/-- Composition v1 consumes exactly one strict `add` role. Requiring the whole
    table shape (not merely a successful lookup) makes the canonical host
    builder extensional and unambiguous. -/
def checkCompositionHostTable (hostTable : List (HostRole × Nat)) : Bool :=
  match hostTable with
  | [(.add, _)] => true
  | _ => false

/-! ### Verbatim `ref.test`-dispatch checker

The `verbatim-plan-v1` grammar has its own dedicated types (the multi-use
scrutinee is spilled to a scratch local, which pure ANF `FragBlock` cannot
express). The soundness binding is the byte-equality gate in
`AcceptedArtifact.verbatimPlanAccepted`; this structural check only rejects
degenerate plans (wrong profile, or a projection sharing the scrutinee's scratch
local). -/

def leafHasProjection : VerbatimLeaf → Bool
  | .project _ _ => true
  | _ => false

def dispatchHasProjection : VerbatimDispatch → Bool
  | .leaf l => leafHasProjection l
  | .test _ hit rest => leafHasProjection hit || dispatchHasProjection rest

def checkVerbatimLeaf : VerbatimLeaf → Bool
  | .project _ _ => true
  -- Every payload element must be a real byte: the data-section binding compares
  -- the claimed payload against recovered `0..255` segment bytes, so an
  -- out-of-range element could never match and is rejected up front.
  | .arrayNewData _ _ bytes => bytesAllBytes bytes
  | .refNull => true
  | .f64Bits _ => true

def checkVerbatimDispatch : VerbatimDispatch → Bool
  | .leaf l => checkVerbatimLeaf l
  | .test _ hit rest => checkVerbatimLeaf hit && checkVerbatimDispatch rest

def checkVerbatimRawPlan (plan : VerbatimRawPlan) : Bool :=
  plan.profile == "verbatim-plan-v1" &&
  checkVerbatimDispatch plan.body &&
  (!dispatchHasProjection plan.body || plan.fieldLocal != plan.scrutineeLocal)

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
  -- Null is the canonical empty-list tail. Its heap type is NOT plan data:
  -- constructor lowering receives the byte-derived target struct index.
  | .null => true

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

def symConstructArgs? (plan : SymRawPlan) : Option (List SymNode × String × String × List Nat) :=
  match lookupSymNode plan.body.nodes plan.body.result with
  | some { ty := .named typeName, kind := .construct _ ctorName args, .. } =>
      some (plan.body.nodes, typeName, ctorName, args)
  | some { ty := .app1 typeName _, kind := .construct _ ctorName args, .. } =>
      some (plan.body.nodes, typeName, ctorName, args)
  | some { ty := .app2 typeName _ _, kind := .construct _ ctorName args, .. } =>
      some (plan.body.nodes, typeName, ctorName, args)
  | _ => none

def symConstructFieldsMatch
    (nodes : List SymNode) : List Nat → List ConstructField → Bool
  | [], [] => true
  | arg :: args, .local index :: fields =>
      match lookupSymNode nodes arg with
      | some { kind := .param actual, .. } =>
          actual = index && symConstructFieldsMatch nodes args fields
      | _ => false
  | arg :: args, .null :: fields =>
      match lookupSymNode nodes arg with
      | some { kind := .emptyList _, .. } => symConstructFieldsMatch nodes args fields
      | _ => false
  | _, _ => false

def constructPlanMatchesSymRawPlan
    (symPlan : SymRawPlan)
    (plan : ConstructRawPlan) : Bool :=
  if checkConstructRawPlan plan then
    match symConstructArgs? symPlan with
    | some (nodes, _, _, args) =>
        symPlan.params.length = plan.arity &&
          symConstructFieldsMatch nodes args plan.fields
    | none => false
  else false

def encodeSymTy? : SymTy → Option FragTy
  | .float => some .f64
  | .bool => some .boolI32
  | .int => some .intCarrier
  -- Strings and named user types are whole references at the representation
  -- level: both encode to the opaque `adtRef`. String OPERATIONS still do not
  -- encode (their nodes return `none` below); this only lets reference-typed
  -- values flow verbatim through the field-projection face.
  | .string => some .adtRef
  | .named _ => some .adtRef
  | .app1 _ _ => some .adtRef
  | .app2 _ _ _ => some .adtRef

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

/-- Look up the resolved wasm struct type index for one source type name in the
    byte-derived struct table an artifact claim carries. A name the table lacks
    fail-closes the encoding (`none`). Like the host-role table, a wrong table
    encodes to a representation plan whose canonical bytes cannot match the
    module, so the claim fail-closes at the byte gate. -/
def structTyIdx? (structTable : List (String × Nat)) (name : String) : Option Nat :=
  match structTable with
  | [] => none
  | (n, idx) :: rest => if n = name then some idx else structTyIdx? rest name

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

def encodeSymBlockFuel :
    Nat → List (HostRole × Nat) → List (String × Nat) → SymBlock → Option FragBlock
  | 0, _, _, _ => none
  | fuel + 1, hostTable, structTable, block =>
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
          | .emptyList _ => none
          | .projectField typeName field _fieldTy value =>
              -- Only opaque reference fields encode: the projected value flows
              -- verbatim through the field-projection face. Scalar fields have
              -- no rendered proof face yet, so they fail-close the encoding.
              if fragTy = FragTy.adtRef then
                let tyIdx ← structTyIdx? structTable typeName
                let value ← encodedValue? st value
                let (st, id) :=
                  pushEncodedNode st fragTy (.structGetUser tyIdx field value)
                some { st with symToFrag := st.symToFrag ++ [id] }
              else
                none
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
              let thenFrag ← encodeSymBlockFuel fuel hostTable structTable thenBlock
              let elseFrag ← encodeSymBlockFuel fuel hostTable structTable elseBlock
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

def encodeSymBlock?
    (hostTable : List (HostRole × Nat))
    (structTable : List (String × Nat))
    (block : SymBlock) :
    Option FragBlock :=
  encodeSymBlockFuel maxFuel hostTable structTable block

def encodeSymRawPlanToExprFragmentRawPlan
    (hostTable : List (HostRole × Nat))
    (structTable : List (String × Nat))
    (plan : SymRawPlan) :
    Option ExprFragmentRawPlan :=
  if checkSymRawPlan plan then
    match encodeSymTys? plan.params, encodeSymTy? plan.result,
          encodeSymBlock? hostTable structTable plan.body with
    | some params, some result, some body =>
        some { profile := "expr-fragment-v1", params := params, result := result, body := body }
    | _, _, _ => none
  else
    none

/-! ### `recursion-plan-v1` shape checking

`checkRecursionRawPlan` above is only the generic typed-block discipline; it
deliberately knows nothing about WHICH function a `selfCall` may target or
which indices realise the host roles. This section pins the fuel-recursion
grammar itself, context-sensitively: the exact carrier-sign dispatch the
emitter produces, a base arm that is a boxed literal (unary) or the
accumulator (two-argument), a step arm whose descent is `sub(n, box 1)`, host
calls citing exactly the byte-derived role table, and a self-call whose target
is EXACTLY the exported function's own index. The self index and role table
are context threaded from the byte-derived function binding — never plan
data. -/

/-- The small-limb arm of the sign predicate: `n.small ≤ 0`. -/
def recSignSmall (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .structGet 0 0 },
     { id := 2, ty := .i64, kind := .constI64 z },
     { id := 3, ty := .boolI32, kind := .prim .i64LeS [1, 2] }], 3 => z = 0
  | _, _ => false

/-- The big-limb arm of the sign predicate: `n.sign < 0`. -/
def recSignBig (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .rawI32, kind := .structGet 2 0 },
     { id := 2, ty := .boolI32, kind := .constBool false },
     { id := 3, ty := .boolI32, kind := .prim .i32LtS [1, 2] }], 3 => true
  | _, _ => false

/-- Unary base arm: a boxed integer literal (`i64.const k; box`). -/
def recBaseUnary (boxIdx : Nat) (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .i64, kind := .constI64 _ },
     { id := 1, ty := .intCarrier, kind := .hostCall .box bi [0] }], 1 => bi = boxIdx
  | _, _ => false

/-- Accumulator base arm: return the accumulator parameter. -/
def recBaseAcc (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 1 }], 0 => true
  | _, _ => false

/-- Unary step arm, all four byte-derived variants: the descent
    `sub(n, box 1)` feeding a NON-TAIL self-call, combined with the other
    operand (the input `n`, or a boxed constant) on either side by the
    role-`add` or role-`mul` combinator helper. Every host index must cite the table and the
    self-call must target `self`. -/
def recStepUnary (combineRole : HostRole) (self boxIdx combineIdx subIdx : Nat)
    (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  -- other = input, recursive result second: `n + f(n-1)`
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .intCarrier, kind := .local 0 },
     { id := 2, ty := .i64, kind := .constI64 one },
     { id := 3, ty := .intCarrier, kind := .hostCall .box bi [2] },
     { id := 4, ty := .intCarrier, kind := .hostCall .sub si [1, 3] },
     { id := 5, ty := .intCarrier, kind := .selfCall false sc [4] },
     { id := 6, ty := .intCarrier, kind := .hostCall role ci [0, 5] }], 6 =>
      one = 1 && bi = boxIdx && si = subIdx && sc = self &&
        role = combineRole && ci = combineIdx
  -- other = boxed constant, recursive result second: `k + f(n-1)`
  | [{ id := 0, ty := .i64, kind := .constI64 _ },
     { id := 1, ty := .intCarrier, kind := .hostCall .box bk [0] },
     { id := 2, ty := .intCarrier, kind := .local 0 },
     { id := 3, ty := .i64, kind := .constI64 one },
     { id := 4, ty := .intCarrier, kind := .hostCall .box bi [3] },
     { id := 5, ty := .intCarrier, kind := .hostCall .sub si [2, 4] },
     { id := 6, ty := .intCarrier, kind := .selfCall false sc [5] },
     { id := 7, ty := .intCarrier, kind := .hostCall role ci [1, 6] }], 7 =>
      one = 1 && bk = boxIdx && bi = boxIdx && si = subIdx && sc = self &&
        role = combineRole && ci = combineIdx
  -- other = input, recursive result first: `f(n-1) + n`
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .selfCall false sc [3] },
     { id := 5, ty := .intCarrier, kind := .local 0 },
     { id := 6, ty := .intCarrier, kind := .hostCall role ci [4, 5] }], 6 =>
      one = 1 && bi = boxIdx && si = subIdx && sc = self &&
        role = combineRole && ci = combineIdx
  -- other = boxed constant, recursive result first: `f(n-1) + k`
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .selfCall false sc [3] },
     { id := 5, ty := .i64, kind := .constI64 _ },
     { id := 6, ty := .intCarrier, kind := .hostCall .box bk [5] },
     { id := 7, ty := .intCarrier, kind := .hostCall role ci [4, 6] }], 7 =>
      one = 1 && bi = boxIdx && bk = boxIdx && si = subIdx && sc = self &&
        role = combineRole && ci = combineIdx
  | _, _ => false

/-- Accumulator step arm: descent `sub(n, box 1)`, next accumulator
    `add(acc, n)`, then a TAIL self-call `f(n-1, acc+n)`. -/
def recStepAcc (self boxIdx addIdx subIdx : Nat) (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .local 1 },
     { id := 5, ty := .intCarrier, kind := .local 0 },
     { id := 6, ty := .intCarrier, kind := .hostCall .add ai [4, 5] },
     { id := 7, ty := .intCarrier, kind := .selfCall true sc [3, 6] }], 7 =>
      one = 1 && bi = boxIdx && si = subIdx && sc = self && ai = addIdx
  | _, _ => false

/-- The whole recursion body: the carrier discriminator, the sign-predicate
    `if`, and the value `if` over base/step. -/
def recTopBlock (isAcc : Bool) (combineRole : HostRole)
    (self boxIdx combineIdx subIdx : Nat) (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .ref, kind := .structGet 1 0 },
     { id := 2, ty := .boolI32, kind := .refIsNull 1 },
     { id := 3, ty := .boolI32, kind := .ifElse 2 signS signB },
     { id := 4, ty := .intCarrier, kind := .ifElse 3 base step }], 4 =>
      recSignSmall signS && recSignBig signB &&
        (if isAcc then
          recBaseAcc base && recStepAcc self boxIdx combineIdx subIdx step
        else
          recBaseUnary boxIdx base &&
            recStepUnary combineRole self boxIdx combineIdx subIdx step)
  | _, _ => false

/-- Context-sensitive `recursion-plan-v1` checking: the plan must be one of the
    two recognised fuel-recursion grammars, every self-call must target `self`
    (the byte-derived function binding of the claimed export), and every host
    call must cite the byte-derived role table. A table missing any of the
    box/add/sub roles fail-closes. -/
def checkRecursionPlanShape
    (self : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : RecursionRawPlan) : Bool :=
  match hostRoleIdx? hostTable .box, hostRoleIdx? hostTable .sub with
  | some boxIdx, some subIdx =>
      plan.profile = "recursion-plan-v1" &&
        sameTy plan.result .intCarrier &&
        (match plan.params with
        | [.intCarrier] =>
            (match hostRoleIdx? hostTable .add with
             | some addIdx => recTopBlock false .add self boxIdx addIdx subIdx plan.body
             | none => false) ||
            (match hostRoleIdx? hostTable .mul with
             | some mulIdx => recTopBlock false .mul self boxIdx mulIdx subIdx plan.body
             | none => false)
        | [.intCarrier, .intCarrier] =>
            match hostRoleIdx? hostTable .add with
            | some addIdx => recTopBlock true .add self boxIdx addIdx subIdx plan.body
            | none => false
        | _ => false)
  | _, _ => false

/-! ### `mutual-plan-v1` shape checking

`checkMutualRawPlan` above is only the generic typed-block discipline. This
section pins the mutual-member grammar context-sensitively, generalising the
fuel-recursion self-call to a mutual member's cross-call: the carrier-sign
dispatch the emitter produces, a boxed-literal base arm, a step arm whose
descent is `sub(n, box 1)` feeding a TAIL member-call, host calls citing exactly
the byte-derived box/sub role table, and a member-call whose target is IN the
byte-derived SCC member set (`memberSet`). The member set and role table are
context threaded from the byte-derived SCC binding — never plan data. Unlike
`checkRecursionPlanShape` a member's call is NOT pinned to its own index: it
targets a SIBLING member, so the check binds it to the SCC set (the member's own
index is in that set, so a legitimate 2-cycle back-edge is admitted too). The
byte-exact gate then forces it to the member's actual cross target. -/

/-- Mutual-member step arm: descent `sub(n, box 1)` feeding a TAIL member-call
    `g(n-1)` whose target `cc` is in the byte-derived SCC member set. -/
def recStepMutual (memberSet : List Nat) (boxIdx subIdx : Nat) (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .i64, kind := .constI64 one },
     { id := 2, ty := .intCarrier, kind := .hostCall .box bi [1] },
     { id := 3, ty := .intCarrier, kind := .hostCall .sub si [0, 2] },
     { id := 4, ty := .intCarrier, kind := .selfCall true cc [3] }], 4 =>
      one = 1 && bi = boxIdx && si = subIdx && memberSet.contains cc
  | _, _ => false

/-- The whole mutual-member body: the carrier discriminator, the sign-predicate
    `if`, and the value `if` over a boxed-literal base and the mutual step. -/
def mutTopBlock (memberSet : List Nat) (boxIdx subIdx : Nat) (b : FragBlock) : Bool :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .intCarrier, kind := .local 0 },
     { id := 1, ty := .ref, kind := .structGet 1 0 },
     { id := 2, ty := .boolI32, kind := .refIsNull 1 },
     { id := 3, ty := .boolI32, kind := .ifElse 2 signS signB },
     { id := 4, ty := .intCarrier, kind := .ifElse 3 base step }], 4 =>
      recSignSmall signS && recSignBig signB &&
        recBaseUnary boxIdx base && recStepMutual memberSet boxIdx subIdx step
  | _, _ => false

/-- Context-sensitive `mutual-plan-v1` checking: the plan must be the mutual
    member grammar, and its member-call must target an index IN the byte-derived
    SCC member set with host calls citing the byte-derived box/sub role table. A
    table missing the box or sub role fail-closes. -/
def checkMutualPlanShape
    (memberSet : List Nat)
    (hostTable : List (HostRole × Nat))
    (plan : MutualRawPlan) : Bool :=
  match hostRoleIdx? hostTable .box, hostRoleIdx? hostTable .sub with
  | some boxIdx, some subIdx =>
      plan.profile = "mutual-plan-v1" &&
        sameTy plan.result .intCarrier &&
        (match plan.params with
        | [.intCarrier] => mutTopBlock memberSet boxIdx subIdx plan.body
        | _ => false)
  | _, _ => false

/-! ### Int-face `ref.test`-dispatch checker (`int-dispatch-v1`)

The `Cod := Int` ADT-match families (general variant dispatch, widened Int
match). The soundness binding is the byte-equality gate in
`AcceptedArtifact.intDispatchPlanAccepted` together with the byte-derived
host-role table the lowerers are parameterized by; the plan carries neither
locals nor indices (both are derived), so the structural check is the profile
discipline only. -/

def checkIntDispatchRawPlan (plan : IntDispatchRawPlan) : Bool :=
  plan.profile == "int-dispatch-v1"

/-- The number of `test` arms in an Int-face dispatch cascade. The scratch-local
    layout both lowerers compute is a fixed function of this count: arm `i`
    spills to local `i+1`, the scrutinee is local `armCount + 1`. -/
def intDispatchArmCount : IntDispatchCascade → Nat
  | .default _ => 0
  | .test _ _ rest => intDispatchArmCount rest + 1

/-- The `HostRole` an Int-face arm combinator resolves through in the
    byte-derived role table. -/
def intDispatchRoleHostRole : IntDispatchRole → HostRole
  | .add => .add
  | .sub => .sub

/-- Whether a byte-derived host-role table maps its roles to pairwise DISTINCT
    function indices. The Int-face plan names host helpers by ROLE only and the
    byte lowering substitutes table indices, so with a duplicated table (e.g.
    `add` and `sub` claiming the same index) two plans differing only in an
    arm's role would lower to identical bytes — the byte-equality gate would be
    blind to the role. Requiring distinct indices restores the gate's
    discrimination; the honest table is byte-derived from the strict role
    markers, which are unique per role. -/
def hostTableIndicesDistinct (hostTable : List (HostRole × Nat)) : Bool :=
  natListNoDup (hostTable.map (fun e => e.2))

end AverCert.PlanCheck
