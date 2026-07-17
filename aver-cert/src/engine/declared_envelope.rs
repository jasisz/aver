// Declared-index ADT envelope extraction (producer side, untrusted).
//
// For every user-ADT claim the generated certificate DECLARES the ADT's
// envelope — root index, every constructor's flattened type index, shape,
// and payload target — plus the opaque type-section byte prefix before the
// constructor entries. The checker-owned wall CONFIRMS the declaration with
// a single byte-slice equality (`DeclaredIndexEnvelope.concatPinnedAt`), so
// nothing here is trusted: a wrong declaration simply fails the kernel pin.
//
// The walker below parses the type section only to locate entry byte ranges
// and classify constructor entries against the exact byte templates the wall
// synthesizes (`dCtorBody`). Anything outside the admitted vocabulary makes
// the affected claim decline (fail-closed) instead of emitting a broken pin.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeclaredCtorShape {
    Hit,
    StrBox,
    FloatBox,
    BoolBox,
    ListBox,
    MapBox,
    Unit,
}

impl DeclaredCtorShape {
    pub fn lean_value(self) -> &'static str {
        match self {
            DeclaredCtorShape::Hit => ".hit",
            DeclaredCtorShape::StrBox => ".strBox",
            DeclaredCtorShape::FloatBox => ".floatBox",
            DeclaredCtorShape::BoolBox => ".boolBox",
            DeclaredCtorShape::ListBox => ".listBox",
            DeclaredCtorShape::MapBox => ".mapBox",
            DeclaredCtorShape::Unit => ".unit",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeclaredCtor {
    pub idx: u32,
    pub shape: DeclaredCtorShape,
    pub target: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeclaredEnvelope {
    pub root: u32,
    pub carrier: u32,
    pub ctors: Vec<DeclaredCtor>,
    /// Type-section bytes from the first entry (after the vector count) up to
    /// the first constructor entry: rec-group headers plus every earlier
    /// flattened entry, declared opaque.
    pub type_prefix: Vec<u8>,
}

impl DeclaredEnvelope {
    pub fn lean_env(&self) -> String {
        let ctors = self
            .ctors
            .iter()
            .map(|c| format!("⟨{}, {}, {}⟩", c.idx, c.shape.lean_value(), c.target))
            .collect::<Vec<_>>()
            .join(", ");
        format!("⟨{}, {}, [{}]⟩", self.root, self.carrier, ctors)
    }

    pub fn lean_prefix(&self) -> String {
        format!(
            "[{}]",
            self.type_prefix
                .iter()
                .map(|b| format!("0x{b:02x}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub fn hit_indices(&self) -> Vec<u32> {
        self.ctors
            .iter()
            .filter(|c| c.shape == DeclaredCtorShape::Hit)
            .map(|c| c.idx)
            .collect()
    }
}

/// All extractable ADT envelopes of a module, keyed by constructor index.
#[derive(Clone, Debug, Default)]
pub struct DeclaredEnvelopes {
    by_root: std::collections::BTreeMap<u32, DeclaredEnvelope>,
    ctor_to_root: std::collections::BTreeMap<u32, u32>,
}

impl DeclaredEnvelopes {
    pub fn for_ctor(&self, ctor_idx: u32) -> Option<&DeclaredEnvelope> {
        let root = self.ctor_to_root.get(&ctor_idx)?;
        self.by_root.get(root)
    }
}

struct RawTypeEntry {
    start: usize,
    end: usize,
}

fn read_leb_u32(bytes: &[u8], pos: &mut usize) -> Result<u32, String> {
    let mut result: u32 = 0;
    let mut shift = 0u32;
    loop {
        let byte = *bytes
            .get(*pos)
            .ok_or_else(|| "type section truncated in LEB".to_string())?;
        *pos += 1;
        result |= u32::from(byte & 0x7f)
            .checked_shl(shift)
            .ok_or_else(|| "LEB overflows u32".to_string())?;
        if byte & 0x80 == 0 {
            return Ok(result);
        }
        shift += 7;
        if shift >= 32 {
            return Err("LEB longer than u32".to_string());
        }
    }
}

fn skip_sleb33(bytes: &[u8], pos: &mut usize) -> Result<(), String> {
    for _ in 0..5 {
        let byte = *bytes
            .get(*pos)
            .ok_or_else(|| "type section truncated in s33".to_string())?;
        *pos += 1;
        if byte & 0x80 == 0 {
            return Ok(());
        }
    }
    Err("s33 heap type longer than 5 bytes".to_string())
}

fn skip_valtype(bytes: &[u8], pos: &mut usize) -> Result<(), String> {
    let byte = *bytes
        .get(*pos)
        .ok_or_else(|| "type section truncated in valtype".to_string())?;
    match byte {
        // numeric / vector value types
        0x7f | 0x7e | 0x7d | 0x7c | 0x7b => {
            *pos += 1;
            Ok(())
        }
        // (ref null? heaptype)
        0x63 | 0x64 => {
            *pos += 1;
            skip_sleb33(bytes, pos)
        }
        // abstract reference-type shorthands (funcref .. nullexternref)
        0x69..=0x79 => {
            *pos += 1;
            Ok(())
        }
        other => Err(format!("unrecognized value type byte 0x{other:02x}")),
    }
}

fn skip_storage_type(bytes: &[u8], pos: &mut usize) -> Result<(), String> {
    let byte = *bytes
        .get(*pos)
        .ok_or_else(|| "type section truncated in storage type".to_string())?;
    match byte {
        // packed i8 / i16
        0x78 | 0x77 => {
            *pos += 1;
            Ok(())
        }
        _ => skip_valtype(bytes, pos),
    }
}

fn skip_field_type(bytes: &[u8], pos: &mut usize) -> Result<(), String> {
    skip_storage_type(bytes, pos)?;
    let mutability = *bytes
        .get(*pos)
        .ok_or_else(|| "type section truncated in field mutability".to_string())?;
    if mutability > 1 {
        return Err(format!("invalid field mutability byte 0x{mutability:02x}"));
    }
    *pos += 1;
    Ok(())
}

fn skip_comp_type(bytes: &[u8], pos: &mut usize) -> Result<(), String> {
    let byte = *bytes
        .get(*pos)
        .ok_or_else(|| "type section truncated in composite type".to_string())?;
    *pos += 1;
    match byte {
        // func
        0x60 => {
            for _ in 0..2 {
                let count = read_leb_u32(bytes, pos)?;
                for _ in 0..count {
                    skip_valtype(bytes, pos)?;
                }
            }
            Ok(())
        }
        // struct
        0x5f => {
            let count = read_leb_u32(bytes, pos)?;
            for _ in 0..count {
                skip_field_type(bytes, pos)?;
            }
            Ok(())
        }
        // array
        0x5e => skip_field_type(bytes, pos),
        other => Err(format!("unrecognized composite type byte 0x{other:02x}")),
    }
}

/// One flattened subtype entry: optional `sub (final)` header, then the
/// composite type.
fn skip_subtype(bytes: &[u8], pos: &mut usize) -> Result<(), String> {
    let byte = *bytes
        .get(*pos)
        .ok_or_else(|| "type section truncated in subtype".to_string())?;
    if byte == 0x50 || byte == 0x4f {
        *pos += 1;
        let count = read_leb_u32(bytes, pos)?;
        for _ in 0..count {
            read_leb_u32(bytes, pos)?;
        }
    }
    skip_comp_type(bytes, pos)
}

/// Locate the type section payload; return (entries region, entries offset).
/// The entries region starts after the rectype vector count, mirroring the
/// wall's `typeSectionCursor` (`modulePayload 1` + `readU`).
fn type_section_entries(wasm_bytes: &[u8]) -> Result<(usize, usize, u32), String> {
    if wasm_bytes.len() < 8 || &wasm_bytes[0..4] != b"\0asm" {
        return Err("not a wasm module".to_string());
    }
    let mut pos = 8usize;
    while pos < wasm_bytes.len() {
        let id = wasm_bytes[pos];
        pos += 1;
        let size = read_leb_u32(wasm_bytes, &mut pos)? as usize;
        let payload_start = pos;
        let payload_end = payload_start
            .checked_add(size)
            .filter(|end| *end <= wasm_bytes.len())
            .ok_or_else(|| "section size out of range".to_string())?;
        if id == 1 {
            let mut cursor = payload_start;
            let count = read_leb_u32(wasm_bytes, &mut cursor)?;
            return Ok((cursor, payload_end, count));
        }
        pos = payload_end;
    }
    Err("module has no type section".to_string())
}

/// Classify one constructor entry against the exact byte templates the wall
/// synthesizes (`dCtorBody root ctor`). Returns `(root, shape, target)`.
fn classify_ctor_entry(entry: &[u8]) -> Option<(u32, Option<(u8, bool)>)> {
    // header: 0x4f (sub final) 0x01 (one supertype) root(single byte < 64)
    if entry.len() < 5 || entry[0] != 0x4f || entry[1] != 0x01 {
        return None;
    }
    let root = entry[2];
    if root >= 64 {
        return None;
    }
    match &entry[3..] {
        // struct { (ref null t) } immutable
        [0x5f, 0x01, 0x63, t, 0x00] if *t < 64 => Some((u32::from(root), Some((*t, true)))),
        // struct { f64 } immutable
        [0x5f, 0x01, 0x7c, 0x00] => Some((u32::from(root), Some((0xfe, false)))),
        // struct { i32 } immutable
        [0x5f, 0x01, 0x7f, 0x00] => Some((u32::from(root), Some((0xff, false)))),
        // struct { }
        [0x5f, 0x00] => Some((u32::from(root), None)),
        _ => None,
    }
}

/// Walk the module's type section and extract every declarable ADT envelope.
///
/// Fail-open per ADT: an ADT whose constructor run leaves the admitted byte
/// vocabulary is simply not extractable (its claims decline later); an error
/// is returned only when the section itself cannot be walked.
pub fn collect_declared_envelopes(
    wasm_bytes: &[u8],
    carrier: Option<u32>,
) -> Result<DeclaredEnvelopes, String> {
    let (entries_start, payload_end, _count) = match type_section_entries(wasm_bytes) {
        Ok(t) => t,
        Err(_) => return Ok(DeclaredEnvelopes::default()),
    };

    // Flattened entry byte ranges. Rec-group headers (0x4e n) are recorded as
    // part of the stream but not as entries, matching the wall's flattening.
    let mut entries: Vec<RawTypeEntry> = Vec::new();
    let mut pos = entries_start;
    while pos < payload_end {
        let byte = wasm_bytes[pos];
        if byte == 0x4e {
            let mut cursor = pos + 1;
            let group_len = read_leb_u32(wasm_bytes, &mut cursor)?;
            for _ in 0..group_len {
                let start = cursor;
                skip_subtype(wasm_bytes, &mut cursor)?;
                entries.push(RawTypeEntry { start, end: cursor });
            }
            pos = cursor;
        } else {
            let start = pos;
            let mut cursor = pos;
            skip_subtype(wasm_bytes, &mut cursor)?;
            entries.push(RawTypeEntry { start, end: cursor });
            pos = cursor;
        }
    }
    if pos != payload_end {
        return Err("type section entries overran the section payload".to_string());
    }

    // i8 byte arrays (String storage) and struct field counts, for shape labels.
    let mut i8_arrays = std::collections::BTreeSet::new();
    let mut struct_field_counts = std::collections::BTreeMap::new();
    for (idx, entry) in entries.iter().enumerate() {
        let bytes = &wasm_bytes[entry.start..entry.end];
        let body = if bytes.first() == Some(&0x50) || bytes.first() == Some(&0x4f) {
            let mut cursor = 1usize;
            let count = read_leb_u32(bytes, &mut cursor).unwrap_or(0);
            for _ in 0..count {
                let _ = read_leb_u32(bytes, &mut cursor);
            }
            &bytes[cursor..]
        } else {
            bytes
        };
        match body.first() {
            Some(0x5e) if body.get(1) == Some(&0x78) => {
                i8_arrays.insert(idx as u32);
            }
            Some(0x5f) => {
                let mut cursor = 1usize;
                if let Ok(count) = read_leb_u32(body, &mut cursor) {
                    struct_field_counts.insert(idx as u32, count);
                }
            }
            _ => {}
        }
    }

    // Group candidate constructor entries by their declared root.
    let mut ctor_runs: std::collections::BTreeMap<u32, Vec<(u32, Option<(u8, bool)>)>> =
        std::collections::BTreeMap::new();
    for (idx, entry) in entries.iter().enumerate() {
        let bytes = &wasm_bytes[entry.start..entry.end];
        if let Some((root, payload)) = classify_ctor_entry(bytes) {
            ctor_runs
                .entry(root)
                .or_default()
                .push((idx as u32, payload));
        }
    }

    let mut result = DeclaredEnvelopes::default();
    'roots: for (root, run) in ctor_runs {
        // The root entry must be the canonical empty non-final struct.
        let Some(root_entry) = entries.get(root as usize) else {
            continue;
        };
        if wasm_bytes[root_entry.start..root_entry.end] != [0x50, 0x00, 0x5f, 0x00] {
            continue;
        }
        // Constructors must be a contiguous flattened run with contiguous bytes.
        for pair in run.windows(2) {
            if pair[1].0 != pair[0].0 + 1 {
                continue 'roots;
            }
            let left = &entries[pair[0].0 as usize];
            let right = &entries[pair[1].0 as usize];
            if left.end != right.start {
                continue 'roots;
            }
        }
        let first = run[0].0;
        if first >= 64 {
            continue;
        }
        let mut ctors = Vec::new();
        for (idx, payload) in &run {
            let shape = match payload {
                None => DeclaredCtor {
                    idx: *idx,
                    shape: DeclaredCtorShape::Unit,
                    target: 0,
                },
                Some((0xfe, false)) => DeclaredCtor {
                    idx: *idx,
                    shape: DeclaredCtorShape::FloatBox,
                    target: 0,
                },
                Some((0xff, false)) => DeclaredCtor {
                    idx: *idx,
                    shape: DeclaredCtorShape::BoolBox,
                    target: 0,
                },
                Some((t, true)) => {
                    let target = u32::from(*t);
                    let shape = if Some(target) == carrier {
                        DeclaredCtorShape::Hit
                    } else if i8_arrays.contains(&target) {
                        DeclaredCtorShape::StrBox
                    } else if struct_field_counts.get(&target) == Some(&2) {
                        DeclaredCtorShape::ListBox
                    } else {
                        DeclaredCtorShape::MapBox
                    };
                    DeclaredCtor {
                        idx: *idx,
                        shape,
                        target,
                    }
                }
                Some((_, false)) => continue 'roots,
            };
            ctors.push(shape);
        }
        let Some(carrier_idx) = carrier else {
            // Envelopes are only declarable against a byte-derived Int carrier.
            continue;
        };
        if carrier_idx >= 64 {
            continue;
        }
        let type_prefix =
            wasm_bytes[entries_start..entries[first as usize].start].to_vec();
        let envelope = DeclaredEnvelope {
            root,
            carrier: carrier_idx,
            ctors,
            type_prefix,
        };
        for ctor in &envelope.ctors {
            result.ctor_to_root.insert(ctor.idx, root);
        }
        result.by_root.insert(root, envelope);
    }
    Ok(result)
}
