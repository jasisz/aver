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

def symPrimResultTy? (nodes : List SymNode) (op : SymPrim) (args : List Nat) :
    Option SymTy :=
  match op with
  | .floatAdd =>
      if symArgsHaveTys nodes args [.float, .float] then some .float else none
  | .floatMul =>
      if symArgsHaveTys nodes args [.float, .float] then some .float else none
  | .floatLe =>
      if symArgsHaveTys nodes args [.float, .float] then some .bool else none

/-- Hard cap for recursive plan checking. Exceeding it is a fail-closed
    unsupported fragment, matching the profile-limit discipline on the Rust
    side. -/
def maxFuel : Nat := 10000

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
        | .constFloatBits _ => some .float
        | .prim op args => symPrimResultTy? checked op args
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

def encodeSymTy? : SymTy → Option FragTy
  | .float => some .f64
  | .bool => some .boolI32
  | .int => some .intCarrier
  | .string => none

def encodeSymTys? : List SymTy → Option (List FragTy)
  | [] => some []
  | ty :: tys =>
      match encodeSymTy? ty, encodeSymTys? tys with
      | some fragTy, some fragTys => some (fragTy :: fragTys)
      | _, _ => none

def encodeSymPrim : SymPrim → FragPrim
  | .floatAdd => .f64Add
  | .floatMul => .f64Mul
  | .floatLe => .f64Le

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
      let (nodes, zero) := appendFragNode nodes .boolI32 (.constBool false)
      let (nodes, result) := appendFragNode nodes .boolI32 (.prim .i32LtS [sign, zero])
      some { nodes := nodes, result := result }
  | some .signGtZero =>
      let (nodes, carrier) := appendFragNode [] .intCarrier (.local index)
      let (nodes, sign) := appendFragNode nodes .rawI32 (.structGet 2 carrier)
      let (nodes, zero) := appendFragNode nodes .boolI32 (.constBool false)
      let (nodes, result) := appendFragNode nodes .boolI32 (.prim .i32GtS [sign, zero])
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

def encodeSymBlockFuel : Nat → SymBlock → Option FragBlock
  | 0, _ => none
  | fuel + 1, block =>
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
          | .constFloatBits bits =>
              let (st, id) := pushEncodedNode st fragTy (.constF64Bits bits)
              some { st with symToFrag := st.symToFrag ++ [id] }
          | .prim op args =>
              let fragArgs ← args.mapM (encodedValue? st)
              let (st, id) := pushEncodedNode st fragTy (.prim (encodeSymPrim op) fragArgs)
              some { st with symToFrag := st.symToFrag ++ [id] }
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
              let thenFrag ← encodeSymBlockFuel fuel thenBlock
              let elseFrag ← encodeSymBlockFuel fuel elseBlock
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

def encodeSymBlock? (block : SymBlock) : Option FragBlock :=
  encodeSymBlockFuel maxFuel block

def encodeSymRawPlanToExprFragmentRawPlan (plan : SymRawPlan) :
    Option ExprFragmentRawPlan :=
  if checkSymRawPlan plan then
    match encodeSymTys? plan.params, encodeSymTy? plan.result, encodeSymBlock? plan.body with
    | some params, some result, some body =>
        some { profile := "expr-fragment-v1", params := params, result := result, body := body }
    | _, _, _ => none
  else
    none

end AverCert.PlanCheck
