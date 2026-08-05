//! Pure cryptographic helpers for wasm-gc and wasip2.

use wasm_encoder::Function;

use super::super::{WasmGcError, wat_helper};
use super::TypeRegistry;

const SHA256_K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

/// Emit SHA-256 directly into the GC module. The helper consumes the nominal
/// `Bytes` record, hashes its `List<Int>` payload with wrapping i32 operations,
/// then constructs `Digest32(Bytes(...))`. No host import is involved, so this
/// one body serves both browser wasm-gc and the wasip2 component wrapper.
pub(super) fn emit_sha256(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let byte_array_idx = registry.crypto_byte_array_type_idx.ok_or_else(|| {
        WasmGcError::Validation("Crypto.sha256 requires byte scratch type".into())
    })?;
    let word_array_idx = registry.crypto_word_array_type_idx.ok_or_else(|| {
        WasmGcError::Validation("Crypto.sha256 requires word scratch type".into())
    })?;
    let bytes_idx = registry
        .record_type_idx("Bytes")
        .ok_or_else(|| WasmGcError::Validation("Crypto.sha256 requires Bytes record".into()))?;
    let digest_idx = registry
        .record_type_idx("Digest32")
        .ok_or_else(|| WasmGcError::Validation("Crypto.sha256 requires Digest32 record".into()))?;
    let list_idx = registry.list_type_idx("List<Int>").ok_or_else(|| {
        WasmGcError::Validation("Crypto.sha256 requires List<Int> representation".into())
    })?;
    let aint_idx = registry.aint_struct_idx.ok_or_else(|| {
        WasmGcError::Validation("Crypto.sha256 requires unbounded Int representation".into())
    })?;
    let from_i64_idx = registry
        .aint_from_i64_fn_idx
        .ok_or_else(|| WasmGcError::Validation("Crypto.sha256 requires __aint_from_i64".into()))?;
    let to_i64_idx = registry.aint_to_i64_checked_fn_idx.ok_or_else(|| {
        WasmGcError::Validation("Crypto.sha256 requires __aint_to_i64_checked".into())
    })?;

    let types = crypto_types(
        byte_array_idx,
        word_array_idx,
        bytes_idx,
        digest_idx,
        list_idx,
        aint_idx,
    )?;
    let callees = wat_helper::func_placeholders(&[
        wat_helper::CalleeStub {
            abs_idx: from_i64_idx,
            sig: "(param i64) (result (ref null $aint))",
        },
        wat_helper::CalleeStub {
            abs_idx: to_i64_idx,
            sig: "(param (ref null $aint)) (result i64)",
        },
    ]);
    let constants = SHA256_K
        .iter()
        .enumerate()
        .map(|(i, k)| {
            format!(
                "local.get $words\ni32.const {}\ni32.const 0x{k:08x}\narray.set $word_array\n",
                i + 64
            )
        })
        .collect::<String>();

    let wat = format!(
        r#"(module
{types}
{callees}
(func (export "helper") (param $input (ref null $bytes)) (result (ref null $digest))
  (local $node (ref null $list_int))
  (local $out (ref null $list_int))
  (local $message (ref null $byte_array))
  (local $words (ref null $word_array))
  (local $input_len i32)
  (local $padded_len i32)
  (local $offset i32)
  (local $i i32)
  (local $x i32)
  (local $s0 i32)
  (local $s1 i32)
  (local $choice i32)
  (local $majority i32)
  (local $t1 i32)
  (local $t2 i32)
  (local $a i32)
  (local $b i32)
  (local $c i32)
  (local $d i32)
  (local $e i32)
  (local $f i32)
  (local $g i32)
  (local $h i32)
  (local $h0 i32)
  (local $h1 i32)
  (local $h2 i32)
  (local $h3 i32)
  (local $h4 i32)
  (local $h5 i32)
  (local $h6 i32)
  (local $h7 i32)
  (local $bit_len i64)

  ;; Count the refined byte list.
  local.get $input
  ref.as_non_null
  struct.get $bytes 0
  local.set $node
  (block $count_done
    (loop $count
      local.get $node
      ref.is_null
      br_if $count_done
      local.get $input_len
      i32.const 1
      i32.add
      local.set $input_len
      local.get $node
      ref.as_non_null
      struct.get $list_int 1
      local.set $node
      br $count))

  ;; padded_len = round_up(input_len + 9, 64)
  local.get $input_len
  i32.const 72
  i32.add
  i32.const -64
  i32.and
  local.tee $padded_len
  array.new_default $byte_array
  local.set $message

  ;; Copy source octets, checking the Int carrier at the nominal boundary.
  local.get $input
  ref.as_non_null
  struct.get $bytes 0
  local.set $node
  i32.const 0
  local.set $i
  (block $copy_done
    (loop $copy
      local.get $node
      ref.is_null
      br_if $copy_done
      local.get $message
      local.get $i
      local.get $node
      ref.as_non_null
      struct.get $list_int 0
      call {to_i64_idx}
      i32.wrap_i64
      array.set $byte_array
      local.get $i
      i32.const 1
      i32.add
      local.set $i
      local.get $node
      ref.as_non_null
      struct.get $list_int 1
      local.set $node
      br $copy))

  local.get $message
  local.get $input_len
  i32.const 128
  array.set $byte_array

  local.get $input_len
  i64.extend_i32_u
  i64.const 8
  i64.mul
  local.set $bit_len
  i32.const 0
  local.set $i
  (block $length_done
    (loop $length
      local.get $i
      i32.const 8
      i32.ge_u
      br_if $length_done
      local.get $message
      local.get $padded_len
      i32.const 1
      i32.sub
      local.get $i
      i32.sub
      local.get $bit_len
      local.get $i
      i64.extend_i32_u
      i64.const 8
      i64.mul
      i64.shr_u
      i32.wrap_i64
      i32.const 255
      i32.and
      array.set $byte_array
      local.get $i
      i32.const 1
      i32.add
      local.set $i
      br $length))

  i32.const 128
  array.new_default $word_array
  local.set $words
{constants}

  i32.const 0x6a09e667 local.set $h0
  i32.const 0xbb67ae85 local.set $h1
  i32.const 0x3c6ef372 local.set $h2
  i32.const 0xa54ff53a local.set $h3
  i32.const 0x510e527f local.set $h4
  i32.const 0x9b05688c local.set $h5
  i32.const 0x1f83d9ab local.set $h6
  i32.const 0x5be0cd19 local.set $h7
  i32.const 0 local.set $offset

  (block $blocks_done
    (loop $blocks
      local.get $offset
      local.get $padded_len
      i32.ge_u
      br_if $blocks_done

      ;; First sixteen big-endian words.
      i32.const 0 local.set $i
      (block $first_done
        (loop $first
          local.get $i
          i32.const 16
          i32.ge_u
          br_if $first_done
          local.get $message
          local.get $offset
          local.get $i
          i32.const 4
          i32.mul
          i32.add
          array.get $byte_array
          i32.const 24
          i32.shl
          local.set $x
          local.get $x
          local.get $message
          local.get $offset
          local.get $i
          i32.const 4
          i32.mul
          i32.add
          i32.const 1
          i32.add
          array.get $byte_array
          i32.const 16
          i32.shl
          i32.or
          local.set $x
          local.get $x
          local.get $message
          local.get $offset
          local.get $i
          i32.const 4
          i32.mul
          i32.add
          i32.const 2
          i32.add
          array.get $byte_array
          i32.const 8
          i32.shl
          i32.or
          local.set $x
          local.get $words
          local.get $i
          local.get $x
          local.get $message
          local.get $offset
          local.get $i
          i32.const 4
          i32.mul
          i32.add
          i32.const 3
          i32.add
          array.get $byte_array
          i32.or
          array.set $word_array
          local.get $i i32.const 1 i32.add local.set $i
          br $first))

      ;; Extend the schedule to 64 words.
      i32.const 16 local.set $i
      (block $extend_done
        (loop $extend
          local.get $i i32.const 64 i32.ge_u br_if $extend_done
          local.get $words local.get $i i32.const 15 i32.sub array.get $word_array local.set $x
          local.get $x i32.const 7 i32.rotr
          local.get $x i32.const 18 i32.rotr i32.xor
          local.get $x i32.const 3 i32.shr_u i32.xor
          local.set $s0
          local.get $words local.get $i i32.const 2 i32.sub array.get $word_array local.set $x
          local.get $x i32.const 17 i32.rotr
          local.get $x i32.const 19 i32.rotr i32.xor
          local.get $x i32.const 10 i32.shr_u i32.xor
          local.set $s1
          local.get $words local.get $i
          local.get $s1
          local.get $words local.get $i i32.const 7 i32.sub array.get $word_array i32.add
          local.get $s0 i32.add
          local.get $words local.get $i i32.const 16 i32.sub array.get $word_array i32.add
          array.set $word_array
          local.get $i i32.const 1 i32.add local.set $i
          br $extend))

      local.get $h0 local.set $a
      local.get $h1 local.set $b
      local.get $h2 local.set $c
      local.get $h3 local.set $d
      local.get $h4 local.set $e
      local.get $h5 local.set $f
      local.get $h6 local.set $g
      local.get $h7 local.set $h
      i32.const 0 local.set $i
      (block $rounds_done
        (loop $rounds
          local.get $i i32.const 64 i32.ge_u br_if $rounds_done
          local.get $e i32.const 6 i32.rotr
          local.get $e i32.const 11 i32.rotr i32.xor
          local.get $e i32.const 25 i32.rotr i32.xor
          local.set $s1
          local.get $e local.get $f i32.and
          local.get $e i32.const -1 i32.xor local.get $g i32.and
          i32.xor local.set $choice
          local.get $h local.get $s1 i32.add local.get $choice i32.add
          local.get $words local.get $i i32.const 64 i32.add array.get $word_array i32.add
          local.get $words local.get $i array.get $word_array i32.add
          local.set $t1
          local.get $a i32.const 2 i32.rotr
          local.get $a i32.const 13 i32.rotr i32.xor
          local.get $a i32.const 22 i32.rotr i32.xor
          local.set $s0
          local.get $a local.get $b i32.and
          local.get $a local.get $c i32.and i32.xor
          local.get $b local.get $c i32.and i32.xor
          local.set $majority
          local.get $s0 local.get $majority i32.add local.set $t2
          local.get $g local.set $h
          local.get $f local.set $g
          local.get $e local.set $f
          local.get $d local.get $t1 i32.add local.set $e
          local.get $c local.set $d
          local.get $b local.set $c
          local.get $a local.set $b
          local.get $t1 local.get $t2 i32.add local.set $a
          local.get $i i32.const 1 i32.add local.set $i
          br $rounds))

      local.get $h0 local.get $a i32.add local.set $h0
      local.get $h1 local.get $b i32.add local.set $h1
      local.get $h2 local.get $c i32.add local.set $h2
      local.get $h3 local.get $d i32.add local.set $h3
      local.get $h4 local.get $e i32.add local.set $h4
      local.get $h5 local.get $f i32.add local.set $h5
      local.get $h6 local.get $g i32.add local.set $h6
      local.get $h7 local.get $h i32.add local.set $h7
      local.get $offset i32.const 64 i32.add local.set $offset
      br $blocks))

  ;; Reuse the schedule prefix for final state words.
  local.get $words i32.const 0 local.get $h0 array.set $word_array
  local.get $words i32.const 1 local.get $h1 array.set $word_array
  local.get $words i32.const 2 local.get $h2 array.set $word_array
  local.get $words i32.const 3 local.get $h3 array.set $word_array
  local.get $words i32.const 4 local.get $h4 array.set $word_array
  local.get $words i32.const 5 local.get $h5 array.set $word_array
  local.get $words i32.const 6 local.get $h6 array.set $word_array
  local.get $words i32.const 7 local.get $h7 array.set $word_array

  ;; Build the output list backwards so its final order is digest byte 0..31.
  i32.const 31 local.set $i
  (block $output_done
    (loop $output
      local.get $words
      local.get $i i32.const 2 i32.shr_u
      array.get $word_array
      i32.const 3
      local.get $i i32.const 3 i32.and
      i32.sub
      i32.const 3 i32.shl
      i32.shr_u
      i32.const 255 i32.and
      i64.extend_i32_u
      call {from_i64_idx}
      local.get $out
      struct.new $list_int
      local.set $out
      local.get $i i32.eqz br_if $output_done
      local.get $i i32.const 1 i32.sub local.set $i
      br $output))

  local.get $out
  struct.new $bytes
  struct.new $digest)
)"#
    );

    wat_helper::compile_wat_helper(&wat)
}

fn crypto_types(
    byte_array_idx: u32,
    word_array_idx: u32,
    bytes_idx: u32,
    digest_idx: u32,
    list_idx: u32,
    aint_idx: u32,
) -> Result<String, WasmGcError> {
    let named = [
        byte_array_idx,
        word_array_idx,
        bytes_idx,
        digest_idx,
        list_idx,
        aint_idx,
    ];
    let mut unique = named.to_vec();
    unique.sort_unstable();
    unique.dedup();
    if unique.len() != named.len() {
        return Err(WasmGcError::Validation(
            "Crypto.sha256 internal type indices overlap".into(),
        ));
    }
    let max = *unique.last().expect("crypto types are non-empty");
    let mut out = String::from("(rec\n");
    for idx in 0..=max {
        let ty = if idx == byte_array_idx {
            "(type $byte_array (array (mut i32)))"
        } else if idx == word_array_idx {
            "(type $word_array (array (mut i32)))"
        } else if idx == bytes_idx {
            "(type $bytes (struct (field (ref null $list_int))))"
        } else if idx == digest_idx {
            "(type $digest (struct (field (ref null $bytes))))"
        } else if idx == list_idx {
            "(type $list_int (struct (field (ref null $aint)) (field (ref null $list_int))))"
        } else if idx == aint_idx {
            // Only the reference identity is needed in this standalone WAT;
            // actual construction/conversion is delegated to the real helpers.
            "(type $aint (struct))"
        } else {
            "(type (struct))"
        };
        out.push_str(ty);
        out.push('\n');
    }
    out.push_str(")\n");
    Ok(out)
}
