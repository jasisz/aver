#!/usr/bin/env python3
"""Flip one byte of a Wasm artifact and carry the flip into an export.

`candidates WASM N` prints N byte offsets to try, from the middle of the
code section body outwards.

`wasm WASM OUT_WASM [OFFSET]` writes a copy of the module with one byte flipped (by default in
the middle of the code section body) and prints the offset as JSON.

`export WASM EXPORT OUT_EXPORT [OFFSET]` rewrites the export of the ORIGINAL module's
certificate so that `AverCert.ArtifactBytes.modBytes` denotes the flipped
module: the checker renders the bytes as numerals of 1024 bytes each
(aver-cert/src/wall.rs, `render_byte_module`), so exactly one numeral changes,
and the export carries every numeral once as a `natVal` literal. Everything
else, the proofs included, is left as it was built for the original bytes. A
kernel that accepts the result has accepted proofs about bytes they were not
built for.
"""
import json
import sys

CHUNK = 1024  # BYTE_NUMERAL_CHUNK in aver-cert/src/wall.rs


def leb(data, pos):
    result = shift = 0
    while True:
        byte = data[pos]
        pos += 1
        result |= (byte & 0x7F) << shift
        shift += 7
        if byte < 0x80:
            return result, pos


def flip_offset(data):
    pos = 8
    while pos < len(data):
        section = data[pos]
        size, body = leb(data, pos + 1)
        if section == 10:
            return body + size // 2
        pos = body + size
    raise SystemExit("no code section")


def flipped(data, offset=None):
    if offset is None:
        offset = flip_offset(data)
    out = bytearray(data)
    out[offset] ^= 0x01
    return bytes(out), offset


def main():
    sys.set_int_max_str_digits(0)
    mode = sys.argv[1]
    if mode == "candidates":
        # Offsets to try, from the middle of the code section outwards.
        data = open(sys.argv[2], "rb").read()
        middle = flip_offset(data)
        for step in range(int(sys.argv[3])):
            print(middle + (step + 1) // 2 * (1 if step % 2 else -1))
        return
    offset = int(sys.argv[-1]) if sys.argv[-1].isdigit() else None
    if mode == "wasm":
        src, dst = sys.argv[2], sys.argv[3]
        data = open(src, "rb").read()
        out, offset = flipped(data, offset)
        open(dst, "wb").write(out)
        print(json.dumps({"offset": offset, "before": data[offset], "after": out[offset]}))
        return
    if mode != "export":
        raise SystemExit(f"unknown mode {mode}")
    src, export, dst = sys.argv[2], sys.argv[3], sys.argv[4]
    data = open(src, "rb").read()
    out, offset = flipped(data, offset)
    index = offset // CHUNK
    old = int.from_bytes(data[index * CHUNK:(index + 1) * CHUNK], "little")
    new = int.from_bytes(out[index * CHUNK:(index + 1) * CHUNK], "little")
    old_text, new_text = str(old), str(new)
    hits = 0
    with open(export, "r", encoding="utf-8") as reader, open(dst, "w", encoding="utf-8") as writer:
        for line in reader:
            if '"natVal"' in line:
                node = json.loads(line)
                if node.get("natVal") == old_text:
                    node["natVal"] = new_text
                    line = json.dumps(node, separators=(",", ":")) + "\n"
                    hits += 1
            writer.write(line)
    print(json.dumps({
        "offset": offset,
        "chunk": index,
        "chunks": (len(data) + CHUNK - 1) // CHUNK,
        "chunk_digits": len(old_text),
        "literals_rewritten": hits,
    }))
    if hits != 1:
        raise SystemExit(f"expected exactly one literal for chunk {index}, rewrote {hits}")


if __name__ == "__main__":
    main()
