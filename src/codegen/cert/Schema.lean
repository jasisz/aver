-- AverCert statement schema (audited, fixed).
--
-- The single final certificate theorem is
--   `AverCert.Final.cert : AverCert.Schema.Holds manifest`.
-- A consumer trusts the certificate by checking THREE things: the theorem
-- NAME, the manifest LITERAL, and the content hash of THIS file plus the
-- semantics prelude. It never inspects the Lean syntax of the proof. Because
-- `Holds` and its denotations live here and this file's hash is pinned in the
-- checker, the meaning of the certificate cannot be weakened without changing
-- a hash the checker rejects.
import CertPrelude
import Module

namespace AverCert.Schema
open CertPrelude

/-- What the artifact is: its pinned hash, the emitted-fragment profile, the
    runtime ABI, the certified export names, and the named runtime contracts
    every certificate is conditional on. Pure data, mirrored in
    `cert-manifest.json`. -/
structure Subject where
  artifactHash : String
  profile      : String
  abi          : String
  exports      : List String
  contracts    : List String

/-- The certification policy attached to a certified export. v0 ships exactly
    one constructor: the emitted body simulates the generated model. -/
inductive Policy where
  | simulatesModel

/-- Pointwise lifting of an integer representation relation to argument lists. -/
inductive ReprAll (R : Int → WVal → Prop) : List Int → List WVal → Prop
  | nil : ReprAll R [] []
  | cons {n v ns vs} : R n v → ReprAll R ns vs → ReprAll R (n :: ns) (v :: vs)

/-- The representation-relation faces a simulation certificate is stated over
    (the Int carrier `{i64 small, ref limbs, i32 sign}`). Bundled in the audited
    schema so `Obligation.holds` is self-contained. -/
structure CarrierSpec (C : Nat) where
  Repr : Int → WVal → Prop
  car : ∀ n v, Repr n v →
    (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
    (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg])
  smallIntro : ∀ k : Int, Repr k (carrierSmall C k)
  smallElim : ∀ n s sg, Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n
  bigElim : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0

/-- One certified export. `code`/`host`/`self` pin the emitted body and its
    runtime wiring; `model` is the generated reference function the body is
    proven to simulate. `aver cert verify` re-derives `code`, `self` and
    `carrier` from the module bytes, so the obligation is bound to the artifact. -/
structure Obligation where
  export_ : String
  policy  : Policy
  carrier : Nat
  code    : CodeTbl
  host    : (List WVal → Option WVal) → (List WVal → Option WVal) → HostTbl
  self    : Nat
  model   : List Int → Int

/-- Denotation of `simulatesModel`: under any representation `S` and any host
    add/sub contracts obeying the named integer laws, the emitted body run on a
    pointwise representation of exactly its decoded entry arity yields a
    representation of `model ns`. The arity is not a separate trusted field; it
    is read from the same pinned `code/self` pair as the body. Partial
    correctness — vacuous on trap or fuel exhaustion. -/
def Obligation.holds (o : Obligation) : Prop :=
  ∀ (S : CarrierSpec o.carrier)
    (add sub : List WVal → Option WVal)
    (_hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w → S.Repr (a + b) w)
    (_hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w)
    (fuel : Nat) (ns : List Int) (vs : List WVal) (w : WVal),
    ReprAll S.Repr ns vs →
    (o.code o.self).map (·.arity) = some ns.length →
    wFuncN o.code (o.host add sub) fuel o.self vs = some w →
    S.Repr (o.model ns) w

structure Manifest where
  subject     : Subject
  obligations : List Obligation

/-- The single audited certificate proposition: the manifest's pinned hash is
    the module hash, and every certified export carries `simulatesModel` and
    genuinely simulates its model. -/
def Holds (m : Manifest) : Prop :=
  m.subject.artifactHash = CertModule.wasmSha256
  ∧ ∀ o ∈ m.obligations, o.policy = Policy.simulatesModel ∧ o.holds

end AverCert.Schema
