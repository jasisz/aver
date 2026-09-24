// What-if for the bit-mask laws (btc-listener issues 352/353/356).
//
// The first block is Aver's Dafny Bits prelude, copied verbatim from
// src/codegen/dafny/mod.rs (DAFNY_HELPER_BITS). The second block is the
// candidate addition: two bridge lemmas from a mask to Euclidean div/mod,
// proved here by induction. The third block restates the probe laws exactly
// as the exporter emits them (same function bodies) and closes each with one
// call to a bridge lemma. If this file verifies, adding the bridge lemmas to
// the prelude and citing them for literal masks of the form 2^k and 2^k - 1
// closes that class on Dafny.

function NatBitAnd(a: nat, b: nat): nat
  decreases a + b
{
  if a == 0 || b == 0 then 0
  else 2 * NatBitAnd(a / 2, b / 2) + (if a % 2 == 1 && b % 2 == 1 then 1 else 0)
}

function NatBitOr(a: nat, b: nat): nat
  decreases a + b
{
  if a == 0 then b else if b == 0 then a
  else 2 * NatBitOr(a / 2, b / 2) + (if a % 2 == 1 || b % 2 == 1 then 1 else 0)
}

function BitsMag(x: int): nat { if x < 0 then -x - 1 else x }

function BitsAnd(a: int, b: int): int {
  var x: int := BitsMag(a);
  var y: int := BitsMag(b);
  var both: int := NatBitAnd(BitsMag(a), BitsMag(b));
  if a < 0 then
    (if b < 0 then -(NatBitOr(BitsMag(a), BitsMag(b)) as int) - 1 else y - both)
  else
    (if b < 0 then x - both else both)
}

function BitsPow2(n: int): int
  ensures BitsPow2(n) > 0
  decreases if n < 0 then 0 else n
{
  if n <= 0 then 1 else 2 * BitsPow2(n - 1)
}

// ---------------------------------------------------------------- bridge

lemma ModUnique(a: int, d: int, q: int, r: int)
  requires d > 0 && 0 <= r < d && a == d * q + r
  ensures a / d == q && a % d == r
{
  var q2, r2 := a / d, a % d;
  assert a == d * q2 + r2 && 0 <= r2 < d;
  assert d * (q - q2) == r2 - r;
  if q > q2 { assert d * (q - q2) >= d; }
  if q < q2 { assert d * (q2 - q) >= d; }
}

lemma HalfSplit(a: nat, p: int)
  requires p > 0
  ensures a % (2 * p) == 2 * ((a / 2) % p) + a % 2
  ensures a / (2 * p) == (a / 2) / p
{
  var q, r := a / 2, a % 2;
  var s, t := q / p, q % p;
  assert a == 2 * q + r;
  assert q == p * s + t;
  assert a == (2 * p) * s + (2 * t + r);
  ModUnique(a, 2 * p, s, 2 * t + r);
}

lemma NatLowMask(a: nat, k: nat)
  ensures NatBitAnd(a, BitsPow2(k) - 1) == a % BitsPow2(k)
  decreases k
{
  if k == 0 {
  } else if a == 0 {
  } else {
    var p := BitsPow2(k - 1);
    assert BitsPow2(k) == 2 * p;
    var m := 2 * p - 1;
    assert m % 2 == 1 && m / 2 == p - 1;
    NatLowMask(a / 2, k - 1);
    HalfSplit(a, p);
  }
}

lemma NatOneBit(a: nat, k: nat)
  ensures NatBitAnd(a, BitsPow2(k)) == (if (a / BitsPow2(k)) % 2 == 1 then BitsPow2(k) else 0)
  decreases k
{
  if a == 0 {
  } else if k == 0 {
    assert NatBitAnd(a / 2, 0) == 0;
  } else {
    var p := BitsPow2(k - 1);
    assert BitsPow2(k) == 2 * p;
    assert (2 * p) % 2 == 0 && (2 * p) / 2 == p;
    NatOneBit(a / 2, k - 1);
    HalfSplit(a, p);
  }
}

lemma ComplementMod(a: int, p: int)
  requires a < 0 && p > 0
  ensures (-a - 1) % p == p - 1 - a % p
  ensures (-a - 1) / p == -(a / p) - 1
{
  var q, r := a / p, a % p;
  assert a == p * q + r && 0 <= r < p;
  assert -a - 1 == p * (-q - 1) + (p - 1 - r);
  ModUnique(-a - 1, p, -q - 1, p - 1 - r);
}

// Bits.and(a, 2^k - 1) == Int.mod(a, 2^k), every Int a.
lemma BitsAndLowMask(a: int, k: nat)
  ensures BitsAnd(a, BitsPow2(k) - 1) == a % BitsPow2(k)
{
  var p := BitsPow2(k);
  if a >= 0 {
    NatLowMask(a, k);
  } else {
    NatLowMask(-a - 1, k);
    ComplementMod(a, p);
  }
}

// Bits.and(a, 2^k) is 2^k exactly when bit k of a is set, every Int a.
lemma BitsAndOneBit(a: int, k: nat)
  ensures BitsAnd(a, BitsPow2(k)) == (if (a / BitsPow2(k)) % 2 == 1 then BitsPow2(k) else 0)
{
  var p := BitsPow2(k);
  if a >= 0 {
    NatOneBit(a, k);
  } else {
    NatOneBit(-a - 1, k);
    ComplementMod(a, p);
    var q := a / p;
    assert (-q - 1) % 2 == 1 - q % 2 by { ComplementMod(q, 2); }
  }
}

// ---------------------------------------------------------------- the probe laws

datatype Base = SignAll | SignNone | SignSingle

function baseOf(hashType: int): Base
{
  if hashType % 32 == 2 then SignNone else if hashType % 32 == 3 then SignSingle else SignAll
}

function isAnyoneCanPay(hashType: int): bool { (hashType / 128) % 2 == 1 }

lemma lowFive_isModThirtyTwo(h: int)
  ensures BitsAnd(h, 31) == h % 32
{
  assert BitsPow2(5) == 32;
  BitsAndLowMask(h, 5);
}

lemma lowFiveBase_agreesWithBaseOf(h: int)
  ensures baseOf(BitsAnd(h, 31)) == baseOf(h)
{
  lowFive_isModThirtyTwo(h);
}

lemma anyoneCanPayMask_agreesWithIsAnyoneCanPay(h: int)
  ensures (BitsAnd(h, 128) == 128) == isAnyoneCanPay(h)
{
  assert BitsPow2(7) == 128;
  BitsAndOneBit(h, 7);
}

lemma mantissaNegative_isBitTwentyThree(bits: int)
  ensures (BitsAnd(bits, 8388608) != 0) == ((bits / 8388608) % 2 == 1)
{
  assert BitsPow2(23) == 8388608;
  BitsAndOneBit(bits, 23);
}

// Chainwork.negative.isTheMantissaTopBit, the law already in btc-listener.
lemma negative_isTheMantissaTopBit(bits: int)
  ensures ((bits / 8388608) % 2 == 1) == (BitsAnd(bits, 8388608) == 8388608)
{
  assert BitsPow2(23) == 8388608;
  BitsAndOneBit(bits, 23);
}

lemma csvDisabled_isBitThirtyOne(v: int)
  ensures (BitsAnd(v, 2147483648) != 0) == ((v / 2147483648) % 2 == 1)
{
  assert BitsPow2(31) == 2147483648;
  BitsAndOneBit(v, 31);
}
