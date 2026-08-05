module Aver_Crypto_Sha256Core {
  datatype DigestState = DigestState(h0: bv32, h1: bv32, h2: bv32, h3: bv32,
                                     h4: bv32, h5: bv32, h6: bv32, h7: bv32)
  datatype RoundState = RoundState(a: bv32, b: bv32, c: bv32, d: bv32,
                                   e: bv32, f: bv32, g: bv32, h: bv32)

  function rotr(x: bv32, n: int): bv32
    requires 0 <= n <= 32
  {
    (x >> n) | (x << (32 - n))
  }

  function choose(x: bv32, y: bv32, z: bv32): bv32 {
    (x & y) ^ ((!x) & z)
  }

  function majority(x: bv32, y: bv32, z: bv32): bv32 {
    (x & y) ^ (x & z) ^ (y & z)
  }

  function bigSigma0(x: bv32): bv32 { rotr(x, 2) ^ rotr(x, 13) ^ rotr(x, 22) }
  function bigSigma1(x: bv32): bv32 { rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25) }
  function smallSigma0(x: bv32): bv32 { rotr(x, 7) ^ rotr(x, 18) ^ (x >> 3) }
  function smallSigma1(x: bv32): bv32 { rotr(x, 17) ^ rotr(x, 19) ^ (x >> 10) }

  function constants(): seq<bv32> {
    [0x428a2f98 as bv32, 0x71374491 as bv32, 0xb5c0fbcf as bv32, 0xe9b5dba5 as bv32, 0x3956c25b as bv32, 0x59f111f1 as bv32, 0x923f82a4 as bv32, 0xab1c5ed5 as bv32,
     0xd807aa98 as bv32, 0x12835b01 as bv32, 0x243185be as bv32, 0x550c7dc3 as bv32, 0x72be5d74 as bv32, 0x80deb1fe as bv32, 0x9bdc06a7 as bv32, 0xc19bf174 as bv32,
     0xe49b69c1 as bv32, 0xefbe4786 as bv32, 0x0fc19dc6 as bv32, 0x240ca1cc as bv32, 0x2de92c6f as bv32, 0x4a7484aa as bv32, 0x5cb0a9dc as bv32, 0x76f988da as bv32,
     0x983e5152 as bv32, 0xa831c66d as bv32, 0xb00327c8 as bv32, 0xbf597fc7 as bv32, 0xc6e00bf3 as bv32, 0xd5a79147 as bv32, 0x06ca6351 as bv32, 0x14292967 as bv32,
     0x27b70a85 as bv32, 0x2e1b2138 as bv32, 0x4d2c6dfc as bv32, 0x53380d13 as bv32, 0x650a7354 as bv32, 0x766a0abb as bv32, 0x81c2c92e as bv32, 0x92722c85 as bv32,
     0xa2bfe8a1 as bv32, 0xa81a664b as bv32, 0xc24b8b70 as bv32, 0xc76c51a3 as bv32, 0xd192e819 as bv32, 0xd6990624 as bv32, 0xf40e3585 as bv32, 0x106aa070 as bv32,
     0x19a4c116 as bv32, 0x1e376c08 as bv32, 0x2748774c as bv32, 0x34b0bcb5 as bv32, 0x391c0cb3 as bv32, 0x4ed8aa4a as bv32, 0x5b9cca4f as bv32, 0x682e6ff3 as bv32,
     0x748f82ee as bv32, 0x78a5636f as bv32, 0x84c87814 as bv32, 0x8cc70208 as bv32, 0x90befffa as bv32, 0xa4506ceb as bv32, 0xbef9a3f7 as bv32, 0xc67178f2 as bv32]
  }

  function initial(): DigestState {
    DigestState(0x6a09e667 as bv32, 0xbb67ae85 as bv32, 0x3c6ef372 as bv32, 0xa54ff53a as bv32,
                0x510e527f as bv32, 0x9b05688c as bv32, 0x1f83d9ab as bv32, 0x5be0cd19 as bv32)
  }

  function zeros(n: nat): seq<int> { seq(n, _ => 0) }

  function lengthBytes(bitLength: int): seq<int>
    requires 0 <= bitLength
  {
    [(bitLength / 0x100000000000000) % 256, (bitLength / 0x1000000000000) % 256,
     (bitLength / 0x10000000000) % 256, (bitLength / 0x100000000) % 256,
     (bitLength / 0x1000000) % 256, (bitLength / 0x10000) % 256,
     (bitLength / 0x100) % 256, bitLength % 256]
  }

  function padded(input: seq<int>): seq<int> {
    input + [128] + zeros((55 - |input|) % 64) + lengthBytes(|input| * 8)
  }

  function wordAt(block: seq<int>, offset: nat): bv32
    requires offset + 3 < |block|
  {
    (((block[offset] % 256) as bv32) << 24) |
    (((block[offset + 1] % 256) as bv32) << 16) |
    (((block[offset + 2] % 256) as bv32) << 8) |
    ((block[offset + 3] % 256) as bv32)
  }

  function firstWords(block: seq<int>, i: nat): seq<bv32>
    requires |block| == 64
    requires i <= 16
    ensures |firstWords(block, i)| == 16 - i
    decreases 16 - i
  {
    if i == 16 then [] else [wordAt(block, i * 4)] + firstWords(block, i + 1)
  }

  function extendWords(words: seq<bv32>): seq<bv32>
    requires 16 <= |words| <= 64
    ensures |extendWords(words)| == 64
    decreases 64 - |words|
  {
    if |words| == 64 then words
    else extendWords(words + [smallSigma1(words[|words| - 2]) + words[|words| - 7] +
                              smallSigma0(words[|words| - 15]) + words[|words| - 16]])
  }

  function schedule(block: seq<int>): seq<bv32>
    requires |block| == 64
  {
    extendWords(firstWords(block, 0))
  }

  function round(s: RoundState, word: bv32, constant: bv32): RoundState {
    var t1 := s.h + bigSigma1(s.e) + choose(s.e, s.f, s.g) + constant + word;
    var t2 := bigSigma0(s.a) + majority(s.a, s.b, s.c);
    RoundState(t1 + t2, s.a, s.b, s.c, s.d + t1, s.e, s.f, s.g)
  }

  function rounds(words: seq<bv32>, i: nat, state: RoundState): RoundState
    requires |words| == 64
    requires i <= 64
    decreases 64 - i
  {
    if i == 64 then state else rounds(words, i + 1, round(state, words[i], constants()[i]))
  }

  function compress(state: DigestState, block: seq<int>): DigestState
    requires |block| == 64
  {
    var r := rounds(schedule(block), 0,
      RoundState(state.h0, state.h1, state.h2, state.h3, state.h4, state.h5, state.h6, state.h7));
    DigestState(state.h0 + r.a, state.h1 + r.b, state.h2 + r.c, state.h3 + r.d,
                state.h4 + r.e, state.h5 + r.f, state.h6 + r.g, state.h7 + r.h)
  }

  function processBlocks(message: seq<int>, state: DigestState): DigestState
    decreases |message|
  {
    if |message| < 64 then state else processBlocks(message[64..], compress(state, message[..64]))
  }

  function wordBytes(word: bv32): seq<int> {
    [((word >> 24) as int) % 256, ((word >> 16) as int) % 256,
     ((word >> 8) as int) % 256, (word as int) % 256]
  }

  function digestBytes(state: DigestState): seq<int> {
    wordBytes(state.h0) + wordBytes(state.h1) + wordBytes(state.h2) + wordBytes(state.h3) +
    wordBytes(state.h4) + wordBytes(state.h5) + wordBytes(state.h6) + wordBytes(state.h7)
  }

  function sha256Raw(input: seq<int>): seq<int> {
    digestBytes(processBlocks(padded(input), initial()))
  }
}
