import Bytes
import Crypto.Digest32

set_option autoImplicit false

namespace AverCrypto

def rotr (x : UInt32) (n : Nat) : UInt32 :=
  (x >>> UInt32.ofNat n) ||| (x <<< UInt32.ofNat (32 - n))

def choose (x y z : UInt32) : UInt32 := (x &&& y) ^^^ ((~~~x) &&& z)
def majority (x y z : UInt32) : UInt32 := (x &&& y) ^^^ (x &&& z) ^^^ (y &&& z)
def bigSigma0 (x : UInt32) : UInt32 := rotr x 2 ^^^ rotr x 13 ^^^ rotr x 22
def bigSigma1 (x : UInt32) : UInt32 := rotr x 6 ^^^ rotr x 11 ^^^ rotr x 25
def smallSigma0 (x : UInt32) : UInt32 := rotr x 7 ^^^ rotr x 18 ^^^ (x >>> 3)
def smallSigma1 (x : UInt32) : UInt32 := rotr x 17 ^^^ rotr x 19 ^^^ (x >>> 10)

def constants : Array UInt32 := #[
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
  0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
  0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
  0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
  0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
  0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
]

def initial : Array UInt32 := #[
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
]

def padded (input : List Int) : Array UInt8 := Id.run do
  let mut out := input.foldl (fun acc byte => acc.push (UInt8.ofNat byte.toNat)) #[]
  let bitLength := UInt64.ofNat (out.size * 8)
  out := out.push 0x80
  -- Zero-pad to 56 mod 64 with a computed count instead of a `while`-loop:
  -- `while` bottoms out in a non-total combinator the kernel cannot unfold.
  out := out ++ Array.replicate ((56 + 64 - out.size % 64) % 64) 0
  for shift in #[56, 48, 40, 32, 24, 16, 8, 0] do
    out := out.push ((bitLength >>> UInt64.ofNat shift).toUInt8)
  return out

def compress (state : Array UInt32) (message : Array UInt8) (offset : Nat) : Array UInt32 := Id.run do
  let mut words : Array UInt32 := Array.replicate 64 0
  for i in [0:16] do
    let j := offset + i * 4
    let word :=
      (message[j]!.toUInt32 <<< 24) |||
      (message[j + 1]!.toUInt32 <<< 16) |||
      (message[j + 2]!.toUInt32 <<< 8) |||
      message[j + 3]!.toUInt32
    words := words.set! i word
  for i in [16:64] do
    words := words.set! i
      (smallSigma1 words[i - 2]! + words[i - 7]! +
       smallSigma0 words[i - 15]! + words[i - 16]!)

  let mut a := state[0]!
  let mut b := state[1]!
  let mut c := state[2]!
  let mut d := state[3]!
  let mut e := state[4]!
  let mut f := state[5]!
  let mut g := state[6]!
  let mut h := state[7]!

  for i in [0:64] do
    let t1 := h + bigSigma1 e + choose e f g + constants[i]! + words[i]!
    let t2 := bigSigma0 a + majority a b c
    h := g
    g := f
    f := e
    e := d + t1
    d := c
    c := b
    b := a
    a := t1 + t2

  return #[
    state[0]! + a, state[1]! + b, state[2]! + c, state[3]! + d,
    state[4]! + e, state[5]! + f, state[6]! + g, state[7]! + h
  ]

def digestWords (input : List Int) : Array UInt32 :=
  let message := padded input
  -- `padded` always returns a multiple of 64 bytes, so folding over the
  -- exact block count replaces the kernel-opaque `while`-loop over offsets.
  (List.range (message.size / 64)).foldl
    (fun state i => compress state message (i * 64)) initial

def sha256Bytes (input : List Int) : List Int := Id.run do
  let mut out : Array Int := #[]
  for word in digestWords input do
    out := out.push ((word >>> 24).toUInt8.toNat : Int)
    out := out.push ((word >>> 16).toUInt8.toNat : Int)
    out := out.push ((word >>> 8).toUInt8.toNat : Int)
    out := out.push (word.toUInt8.toNat : Int)
  return out.toList

end AverCrypto

namespace Crypto

/-- Zero bytes are octets. Proven by induction because the exported
`Bytes.allInRange` is compiled by well-founded recursion, which `decide`
cannot evaluate through the elaborator. -/
private theorem allInRange_replicate_zero (n : Nat) :
    Bytes.allInRange (List.replicate n 0) = true := by
  induction n with
  | zero => simp [Bytes.allInRange]
  | succ n ih => simpa [Bytes.allInRange, List.replicate] using ih

private def fallbackBytes : Bytes.Bytes :=
  ⟨List.replicate 32 0, allInRange_replicate_zero 32⟩

private def fallbackDigest : Crypto.Digest32.Digest32 :=
  ⟨fallbackBytes, by decide⟩

def sha256 (bytes : Bytes.Bytes) : Crypto.Digest32.Digest32 :=
  match Bytes.fromList (AverCrypto.sha256Bytes bytes.val) with
  | .error _ => fallbackDigest
  | .ok validated =>
      match Crypto.Digest32.fromBytes validated with
      | .error _ => fallbackDigest
      | .ok digest => digest

end Crypto
