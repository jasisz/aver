include "common.dfy"
include "Bytes.dfy"
include "Crypto/Digest32.dfy"
include "Crypto/Sha256Core.dfy"

module Aver_Crypto {
  import opened AverCommon
  import opened Aver_Bytes
  import opened Aver_Crypto_Digest32
  import opened Aver_Crypto_Sha256Core

  function validatedZeros(n: nat): seq<int>
    ensures |validatedZeros(n)| == n
    ensures Aver_Bytes.allInRange(validatedZeros(n))
    decreases n
  {
    if n == 0 then [] else [0] + validatedZeros(n - 1)
  }

  function fallbackDigest(): Digest32 {
    validatedZeros(32)
  }

  function sha256(bytes: Bytes): Digest32 {
    match Aver_Bytes.fromList(sha256Raw(bytes))
    case Err(_) => fallbackDigest()
    case Ok(validated) =>
      match Aver_Crypto_Digest32.fromBytes(validated)
      case Err(_) => fallbackDigest()
      case Ok(digest) => digest
  }
}
