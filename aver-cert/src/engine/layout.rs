// ---- the declared module layout ---------------------------------------------
//
// A package declares where the wall finds each planned function's facts in
// the module, so that its checks read them instead of searching for them:
// every defined function's type index and code-entry offset and length (as
// packed tables), the exact function type of every planned function, and
// each planned function's export position. `DeclaredLayout.layoutConfirmed`
// and `fnTypesConfirmed` confirm the declarations against the wall's own
// decoders, and a plan check compares its own function's facts with them; a
// wrong declaration fails those equalities and the package declines.

/// Bits per entry of a packed layout table.
const LAYOUT_WIDTH: u32 = 32;

/// The layout facts of a core module, read by plain byte walking.
struct ModuleLayout {
    imports: u32,
    func_types: Vec<u32>,
    code_offsets: Vec<usize>,
    code_lengths: Vec<usize>,
    /// Plain (no `sub` prefix) function types by type index, as Lean
    /// `CertDecode.ValType` terms.
    fn_types: BTreeMap<u32, (Vec<String>, Vec<String>)>,
    /// Position of each function export in the export section.
    export_positions: HashMap<String, usize>,
    /// Byte length of each top-level entry (rec group or subtype) of the type
    /// section, of each export entry, and (`code_lengths`) of each code entry:
    /// the cuts at which the wall decodes each section one entry at a time.
    type_cuts: Vec<usize>,
    export_cuts: Vec<usize>,
}

struct Cursor<'a> {
    bytes: &'a [u8],
    at: usize,
}

impl Cursor<'_> {
    fn byte(&mut self) -> Result<u8, String> {
        let b = *self.bytes.get(self.at).ok_or("layout: truncated module")?;
        self.at += 1;
        Ok(b)
    }

    fn uleb(&mut self) -> Result<u64, String> {
        let (mut value, mut shift) = (0u64, 0u32);
        loop {
            let b = self.byte()?;
            if shift >= 64 {
                return Err("layout: overlong LEB".into());
            }
            value |= u64::from(b & 0x7f) << shift;
            shift += 7;
            if b < 0x80 {
                return Ok(value);
            }
        }
    }

    fn sleb(&mut self) -> Result<i64, String> {
        let (mut value, mut shift) = (0i64, 0u32);
        loop {
            let b = self.byte()?;
            if shift >= 64 {
                return Err("layout: overlong LEB".into());
            }
            value |= i64::from(b & 0x7f) << shift;
            shift += 7;
            if b < 0x80 {
                if b & 0x40 != 0 && shift < 64 {
                    value |= -1i64 << shift;
                }
                return Ok(value);
            }
        }
    }

    fn skip(&mut self, n: usize) -> Result<(), String> {
        if self.at + n > self.bytes.len() {
            return Err("layout: truncated module".into());
        }
        self.at += n;
        Ok(())
    }

    fn val_type(&mut self) -> Result<String, String> {
        let tag = self.byte()?;
        match tag {
            0x63 | 0x64 => {
                let heap = self.sleb()?;
                let heap = if heap >= 0 {
                    format!("Int.ofNat {heap}")
                } else {
                    format!("Int.negSucc {}", -heap - 1)
                };
                Ok(format!(".ref {tag} ({heap})"))
            }
            0x7b..=0x7f => Ok(format!(".numeric {tag}")),
            0x6a..=0x73 => Ok(format!(".abstract {tag}")),
            _ => Err(format!("layout: value type {tag:#x} outside the profile")),
        }
    }

    fn storage(&mut self) -> Result<(), String> {
        match self.bytes.get(self.at) {
            Some(0x78 | 0x77) => self.skip(1),
            _ => self.val_type().map(|_| ()),
        }
    }
}

impl ModuleLayout {
    fn parse(bytes: &[u8]) -> Result<Self, String> {
        let mut layout = ModuleLayout {
            imports: 0,
            func_types: Vec::new(),
            code_offsets: Vec::new(),
            code_lengths: Vec::new(),
            fn_types: BTreeMap::new(),
            export_positions: HashMap::new(),
            type_cuts: Vec::new(),
            export_cuts: Vec::new(),
        };
        let mut c = Cursor { bytes, at: 8 };
        while c.at < bytes.len() {
            let id = c.byte()?;
            let size = c.uleb()? as usize;
            let end = c.at + size;
            let mut s = Cursor {
                bytes: bytes.get(..end).ok_or("layout: truncated section")?,
                at: c.at,
            };
            match id {
                1 => layout.parse_types(&mut s)?,
                2 => {
                    for _ in 0..s.uleb()? {
                        for _ in 0..2 {
                            let n = s.uleb()? as usize;
                            s.skip(n)?;
                        }
                        if s.byte()? != 0 {
                            return Err("layout: a non-function import".into());
                        }
                        s.uleb()?;
                        layout.imports += 1;
                    }
                }
                3 => {
                    for _ in 0..s.uleb()? {
                        layout.func_types.push(s.uleb()? as u32);
                    }
                }
                7 => {
                    for position in 0..s.uleb()? as usize {
                        let start = s.at;
                        let n = s.uleb()? as usize;
                        let name = bytes
                            .get(s.at..s.at + n)
                            .and_then(|name| std::str::from_utf8(name).ok())
                            .ok_or("layout: export name is not UTF-8")?
                            .to_string();
                        s.skip(n)?;
                        let kind = s.byte()?;
                        s.uleb()?;
                        layout.export_cuts.push(s.at - start);
                        if kind == 0 {
                            layout.export_positions.entry(name).or_insert(position);
                        }
                    }
                }
                10 => {
                    for _ in 0..s.uleb()? {
                        let start = s.at;
                        let n = s.uleb()? as usize;
                        s.skip(n)?;
                        layout.code_offsets.push(start);
                        layout.code_lengths.push(s.at - start);
                    }
                }
                _ => {}
            }
            c.at = end;
        }
        Ok(layout)
    }

    fn parse_types(&mut self, s: &mut Cursor<'_>) -> Result<(), String> {
        let mut index = 0u32;
        for _ in 0..s.uleb()? {
            let start = s.at;
            let group = if s.bytes.get(s.at) == Some(&0x4e) {
                s.skip(1)?;
                s.uleb()?
            } else {
                1
            };
            for _ in 0..group {
                let mut plain = true;
                if matches!(s.bytes.get(s.at), Some(0x50 | 0x4f)) {
                    plain = false;
                    s.skip(1)?;
                    for _ in 0..s.uleb()? {
                        s.uleb()?;
                    }
                }
                match s.byte()? {
                    0x60 => {
                        let params = (0..s.uleb()?)
                            .map(|_| s.val_type())
                            .collect::<Result<Vec<_>, _>>()?;
                        let results = (0..s.uleb()?)
                            .map(|_| s.val_type())
                            .collect::<Result<Vec<_>, _>>()?;
                        if plain {
                            self.fn_types.insert(index, (params, results));
                        }
                    }
                    0x5f => {
                        for _ in 0..s.uleb()? {
                            s.storage()?;
                            s.skip(1)?;
                        }
                    }
                    0x5e => {
                        s.storage()?;
                        s.skip(1)?;
                    }
                    tag => return Err(format!("layout: composite type {tag:#x}")),
                }
                index += 1;
            }
            self.type_cuts.push(s.at - start);
        }
        Ok(())
    }
}

/// A table of small numbers packed into one hex numeral, entry 0 lowest.
fn packed_hex(values: impl DoubleEndedIterator<Item = u64>) -> Result<String, String> {
    let digits = (LAYOUT_WIDTH / 4) as usize;
    let mut hex = String::from("0x0");
    for value in values.rev() {
        if value >> LAYOUT_WIDTH != 0 {
            return Err("layout: a value does not fit the packed width".into());
        }
        hex.push_str(&format!("{value:0digits$x}"));
    }
    Ok(hex)
}

/// A list of lengths as a Lean list literal body, twenty to a line.
fn nat_list(values: &[usize]) -> String {
    values
        .chunks(20)
        .map(|line| line.iter().map(usize::to_string).collect::<Vec<_>>().join(", "))
        .collect::<Vec<_>>()
        .join(",\n   ")
}

/// `ArtifactLayout.lean`: the declared layout, the planned functions' types
/// and export positions, and the proof that the layout is the module's.
fn render_artifact_layout(core_bytes: &[u8], analysis: &Analysis) -> Result<String, String> {
    let layout = ModuleLayout::parse(core_bytes)?;
    let type_of = |func_idx: u32| {
        func_idx
            .checked_sub(layout.imports)
            .and_then(|k| layout.func_types.get(k as usize).copied())
            .ok_or_else(|| format!("layout: function {func_idx} is not defined"))
    };
    let mut planned_types = BTreeSet::new();
    for e in &analysis.entries {
        planned_types.insert(type_of(e.func_idx)?);
    }
    let planned_types: Vec<u32> = planned_types.into_iter().collect();
    let fn_types = planned_types
        .iter()
        .map(|t| {
            let (params, results) = layout
                .fn_types
                .get(t)
                .ok_or_else(|| format!("layout: type {t} is not a plain function type"))?;
            Ok(format!(
                "({t}, [{}], [{}])",
                params.join(", "),
                results.join(", ")
            ))
        })
        .collect::<Result<Vec<_>, String>>()?;
    let decls = analysis
        .entries
        .iter()
        .map(|e| {
            let t = type_of(e.func_idx)?;
            let sig_pos = planned_types.binary_search(&t).expect("collected above");
            let export_pos = if e.exported {
                *layout
                    .export_positions
                    .get(&e.name)
                    .ok_or_else(|| format!("layout: no function export named {}", e.name))?
            } else {
                0
            };
            Ok(format!(
                "⟨{}, {export_pos}, {sig_pos}⟩",
                lean_char_list(&e.name)
            ))
        })
        .collect::<Result<Vec<_>, String>>()?;
    Ok(format!(
        "-- The declared module layout: every defined function's type index and\n\
         -- code entry (packed tables, {LAYOUT_WIDTH} bits per entry), the function types\n\
         -- of the planned functions, and each planned function's name and export\n\
         -- position. Producer data: `layout_ok` confirms the layout against the\n\
         -- staged bytes, and the plan checks confirm the rest.\n\
         import DeclaredLayout\n\
         import ByteWindow\n\
         import ArtifactBytes\n\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCert.Artifact\n\
         open AverCert.DeclaredLayout\n\n\
         def layout : Layout :=\n  \
           {{ imports := {imports}, count := {count}, width := {LAYOUT_WIDTH},\n    \
             types := {types},\n    \
             offsets := {offsets},\n    \
             lengths := {lengths} }}\n\n\
         def fnTypes : List FnType :=\n  [{fn_types}]\n\n\
         def fnDecls : List FnDecl :=\n  [{decls}]\n\n\
         -- The section cuts: the byte length of every top-level entry of the\n\
         -- type section, of every export and of every code entry. Each cut is\n\
         -- confirmed once below (every entry decodes alone and exactly fills its\n\
         -- window), and every later check reads the section through its cut.\n\
         def typeCuts : List Nat :=\n  [{type_cuts}]\n\n\
         def exportCuts : List Nat :=\n  [{export_cuts}]\n\n\
         def codeCuts : List Nat :=\n  [{code_cuts}]\n\n\
         theorem types_cut : CertDecode.decodeTypes {bytes} =\n    \
           AverCert.ByteWindow.typesLazy {bytes} typeCuts :=\n  \
           AverCert.ByteWindow.decodeTypes_eq_lazy (by decide +kernel)\n\n\
         theorem exports_cut : CertDecode.decodeRawExports {bytes} =\n    \
           AverCert.ByteWindow.exportsLazy {bytes} exportCuts :=\n  \
           AverCert.ByteWindow.decodeRawExports_eq_lazy (by decide +kernel)\n\n\
         theorem code_cut : CertDecode.codeLocs {bytes} =\n    \
           AverCert.ByteWindow.codeLazy {bytes} codeCuts :=\n  \
           AverCert.ByteWindow.codeLocs_eq_lazy (by decide +kernel)\n\n\
         theorem layout_ok : layoutConfirmed {bytes} layout = true := by\n  \
           rw [layoutConfirmed, code_cut]; decide +kernel\n\n\
         end AverCert.Artifact\n",
        imports = layout.imports,
        count = layout.func_types.len(),
        types = packed_hex(layout.func_types.iter().map(|&t| u64::from(t)))?,
        offsets = packed_hex(layout.code_offsets.iter().map(|&o| o as u64))?,
        lengths = packed_hex(layout.code_lengths.iter().map(|&l| l as u64))?,
        fn_types = fn_types.join(",\n   "),
        decls = decls.join(",\n   "),
        type_cuts = nat_list(&layout.type_cuts),
        export_cuts = nat_list(&layout.export_cuts),
        code_cuts = nat_list(&layout.code_lengths),
        bytes = "AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen",
    ))
}
