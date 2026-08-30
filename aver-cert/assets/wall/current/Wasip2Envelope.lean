/-
  Wasip2Envelope — target-specific vocabulary for the WASI 0.2 Component Model
  certificate envelope.

  This module intentionally does NOT parse, scan, or navigate component-model
  syntax. It only defines the declared-length split the producer will state:

      prefix ++ embedded_core_module ++ suffix

  Schema 6 feeds the delivered component bytes into these definitions, proves
  the declared split by equality, and then reuses the existing core-module checks
  on the declared embedded module bytes. The producer-side `--target wasip2
  --certify` path remains disabled until certificate emission can construct this
  package end-to-end.
-/
import CertDecode

namespace AverCert.Wasip2Envelope

abbrev ByteSeq := List Nat

def expectedKind : String := "prefix-core-suffix/v1"

/-- Length-only declaration of a `prefix ++ embedded_core_module ++ suffix`
    component split. Positions are derived only from these lengths. -/
structure ComponentEnvelope where
  prefixLen : Nat
  embeddedCoreModuleLen : Nat
  suffixLen : Nat
deriving Repr, DecidableEq

namespace ComponentEnvelope

/-- The manifest kind string this declaration belongs to. -/
def kind (_env : ComponentEnvelope) : String := expectedKind

/-- Total component length described by the declaration. -/
def componentLen (env : ComponentEnvelope) : Nat :=
  env.prefixLen + env.embeddedCoreModuleLen + env.suffixLen

/-- Start offset of the embedded Aver user-core module. -/
def coreStart (env : ComponentEnvelope) : Nat := env.prefixLen

/-- End offset of the embedded Aver user-core module. -/
def coreEnd (env : ComponentEnvelope) : Nat :=
  env.prefixLen + env.embeddedCoreModuleLen

/-- Byte-list view of a little-endian encoded byte blob. -/
def bytes (blob blobLen : Nat) : ByteSeq :=
  CertDecode.takeBytes blobLen blob

/-- Declared split of the delivered component, driven by lengths only.
    The body extracts byte slices but never interprets component syntax. -/
def split (env : ComponentEnvelope) (componentBytes componentLen : Nat) :
    Option (ByteSeq × ByteSeq × ByteSeq) :=
  if env.embeddedCoreModuleLen == 0 then none
  else if componentLen == env.componentLen then
    let allBytes := bytes componentBytes componentLen
    some (
      allBytes.take env.prefixLen,
      (allBytes.drop env.coreStart).take env.embeddedCoreModuleLen,
      (allBytes.drop env.coreEnd).take env.suffixLen
    )
  else none

/-- Trust-bearing shape future wasip2 acceptance will consume: a component's
    byte sequence splits into exactly the declared prefix, core module, and
    suffix sequences. -/
def splitsTo (env : ComponentEnvelope) (componentBytes componentLen : Nat)
    (pre core post : ByteSeq) : Prop :=
  env.split componentBytes componentLen = some (pre, core, post)

/-- Convenience predicate when only the embedded core-module bytes are relevant. -/
def declaresCore (env : ComponentEnvelope) (componentBytes componentLen : Nat)
    (core : ByteSeq) : Prop :=
  ∃ pre post, env.splitsTo componentBytes componentLen pre core post

end ComponentEnvelope

end AverCert.Wasip2Envelope
