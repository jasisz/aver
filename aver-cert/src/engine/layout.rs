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
    /// Byte length of each export entry, and (`code_lengths`) of each code
    /// entry: the cuts at which the wall decodes each section one entry at a
    /// time. The type section's cut is `TypeSection`'s.
    export_cuts: Vec<usize>,
    /// The module offset of every section header, in module order.
    section_headers: Vec<usize>,
    /// The module offset of the export section's first entry.
    export_start: usize,
    /// The bytes of every export's name, in section order.
    export_names: Vec<Vec<u8>>,
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
            export_cuts: Vec::new(),
            section_headers: Vec::new(),
            export_start: 0,
            export_names: Vec::new(),
        };
        let mut c = Cursor { bytes, at: 8 };
        while c.at < bytes.len() {
            layout.section_headers.push(c.at);
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
                    let count = s.uleb()? as usize;
                    layout.export_start = s.at;
                    for position in 0..count {
                        let start = s.at;
                        let n = s.uleb()? as usize;
                        let raw = bytes
                            .get(s.at..s.at + n)
                            .ok_or("layout: truncated export name")?;
                        layout.export_names.push(raw.to_vec());
                        let name = std::str::from_utf8(raw)
                            .map_err(|_| "layout: export name is not UTF-8")?
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
/// and export positions, and the proof that the layout is the module's; with
/// every plan entry's declaration (`FnDecl`) as its Lean literal.
fn render_artifact_layout(
    core_bytes: &[u8],
    analysis: &Analysis,
) -> Result<LayoutParts, String> {
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
    // Each planned export's entry by module offset and length, and the
    // bitmap of every export entry's start relative to the first.
    let mut export_offsets = Vec::with_capacity(layout.export_cuts.len());
    let mut at = layout.export_start;
    for &l in &layout.export_cuts {
        export_offsets.push(at);
        at += l;
    }
    let sites = analysis
        .entries
        .iter()
        .map(|e| {
            if !e.exported {
                return "(0, 0)".to_string();
            }
            let p = layout.export_positions[&e.name];
            format!("({}, {})", export_offsets[p], layout.export_cuts[p])
        })
        .collect::<Vec<_>>();
    let mut start_bits = vec![0u8; at.saturating_sub(layout.export_start) / 8 + 1];
    for &o in &export_offsets {
        let bit = o - layout.export_start;
        start_bits[bit / 8] |= 1 << (bit % 8);
    }
    let export_starts = hex_bits(&start_bits);
    // The planned exports' entries: the walk leaves them to the plan checks,
    // which read each at its site.
    let certified: BTreeSet<usize> = analysis
        .entries
        .iter()
        .filter(|e| e.exported)
        .map(|e| export_offsets[layout.export_positions[&e.name]])
        .collect();
    let mut certified_bits = vec![0u8; start_bits.len()];
    for &o in &certified {
        let bit = o - layout.export_start;
        certified_bits[bit / 8] |= 1 << (bit % 8);
    }
    let export_certified = hex_bits(&certified_bits);
    let walk = ExportWalk::new(&layout, &export_offsets, &certified, declared_uncertified(analysis));
    let export_cut_pieces: String = walk
        .blocks
        .iter()
        .enumerate()
        .map(|(b, block)| {
            format!(
                "def exportCuts_{b} : List Nat :=\n  [{}]\n\n",
                nat_list(&block.cuts)
            )
        })
        .collect();
    let export_cuts = right_nested(
        &(0..walk.blocks.len())
            .map(|b| format!("exportCuts_{b}"))
            .collect::<Vec<_>>(),
    );
    let count = layout.func_types.len();
    let types = TypeWalk::new(core_bytes)?;
    let split = splits_artifact_modules(analysis);
    let (code_block_texts, code_join) = render_code_blocks(count);
    let (type_head, type_block_texts, type_joins) = render_type_theorems(&types);
    const JOINED: &str = "theorem code_locs : CertDecode.codeLocs AverCert.ArtifactBytes.modBytes \
         AverCert.ArtifactBytes.modLen =\n    \
         some (AverCert.ScaleLayout.codeLocsL AverCert.ArtifactBytes.chunks layout) :=\n  \
         AverCert.ScaleLayout.codeLocs_of_tiled bytes_eq chunks_fit code_tiled code_entries\n\n\
         theorem layout_ok : layoutConfirmed AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen \
         layout = true :=\n  \
         AverCert.ScaleLayout.layoutConfirmed_of_tiled bytes_eq chunks_fit code_tiled code_entries\n    \
         funcs_ok\n\n";
    let mut extra = Vec::new();
    let (type_theorems, code_blocks, joined) = if split {
        let blocks: Vec<String> = type_block_texts
            .into_iter()
            .chain(code_block_texts)
            .collect();
        let mut imports = String::new();
        for (m, chunk) in blocks.chunks(LAYOUT_BLOCKS_PER_MODULE).enumerate() {
            let name = format!("ArtifactLayoutBlocks{m}");
            imports.push_str(&format!("import {name}\n"));
            extra.push((
                format!("{name}.lean"),
                format!(
                    "-- Blocks of the type walk and of the code entries (`ArtifactFacts`).\n\
                     import ArtifactLayout\n\n\
                     set_option maxRecDepth 200000\n\n\
                     namespace AverCert.Artifact\n\
                     open AverCert.DeclaredLayout\n\n\
                     {}\
                     end AverCert.Artifact\n",
                    chunk.concat()
                ),
            ));
        }
        extra.push((
            "ArtifactFacts.lean".to_string(),
            format!(
                "-- The type walk and the code entries joined from their blocks.\n\
                 {imports}\n\
                 set_option maxRecDepth 200000\n\n\
                 namespace AverCert.Artifact\n\
                 open AverCert.DeclaredLayout\n\n\
                 {type_joins}{code_join}{JOINED}\
                 end AverCert.Artifact\n"
            ),
        ));
        (type_head, String::new(), String::new())
    } else {
        (
            format!("{type_head}{}{type_joins}", type_block_texts.concat()),
            format!("{}{code_join}", code_block_texts.concat()),
            JOINED.to_string(),
        )
    };
    let text = format!(
        "-- The declared module layout: every defined function's type index and\n\
         -- code entry (packed tables, {LAYOUT_WIDTH} bits per entry), the function types\n\
         -- of the planned functions, each planned function's name, export position\n\
         -- and the helper roles its lowering calls, and every section header's\n\
         -- offset. Producer data: the framing and the code tiling are confirmed\n\
         -- against the staged bytes below, and the plan checks confirm the rest.\n\
         import ScaleTypes\n\
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
         -- The helper roles each plan's lowering calls (`ScaleLayout.roleBits`),\n\
         -- in plan order.\n\
         def callBits : List Nat :=\n  [{call_bits}]\n\n\
         -- The offset of every section header, in module order.\n\
         def headers : List Nat :=\n  [{headers}]\n\n\
         -- The section cuts: the byte length of every top-level entry of the\n\
         -- type section and of every export. Each cut is confirmed once below\n\
         -- (every entry decodes alone and exactly fills its window), and every\n\
         -- later check reads the section through its cut.\n\
         {type_decls}\
         -- The export cut is written in blocks of {EXPORT_BLOCK} entries, the blocks the\n\
         -- export walk reads one declaration at a time (`ArtifactExports`).\n\
         {export_cut_pieces}\
         def exportCuts : List Nat :=\n  {export_cuts}\n\n\
         -- The module offset of the export section's first entry, the start of\n\
         -- every export entry relative to it (as bits), the starts of the\n\
         -- planned exports' entries (as bits), and each plan entry's export\n\
         -- entry by module offset and length (`(0, 0)` for an internal\n\
         -- function).\n\
         def exportStart : Nat := {export_start}\n\n\
         def exportStarts : Nat :=\n  {export_starts}\n\n\
         def exportCertified : Nat :=\n  {export_certified}\n\n\
         def exportSites : List (Nat × Nat) :=\n  [{sites}]\n\n\
         theorem bytes_eq : AverCert.ScaleBytes.join 1024 AverCert.ArtifactBytes.chunks =\n    \
           AverCert.ArtifactBytes.modBytes :=\n  \
           (AverCert.ScaleBytes.joinTree_eq _ _ _).symm\n\n\
         theorem chunks_fit : AverCert.ScaleBytes.chunksFit 1024 AverCert.ArtifactBytes.chunks = true := by\n  \
           decide +kernel\n\n\
         theorem framing_decl : AverCert.ScaleLayout.framingOk AverCert.ArtifactBytes.chunks\n    \
           AverCert.ArtifactBytes.modLen headers = true := by\n  \
           decide +kernel\n\n\
         {type_theorems}\
         theorem export_starts : AverCert.ScaleLayout.startBits 0 exportCuts = exportStarts := by\n  \
           decide +kernel\n\n\
         theorem exports_head : AverCert.ScaleLayout.exportsHead AverCert.ArtifactBytes.chunks\n    \
           AverCert.ArtifactBytes.modLen headers exportStart exportCuts = true := by\n  \
           decide +kernel\n\n\
         -- The code section: its count and its tiling by the declared entries,\n\
         -- then every entry decoded on its own window, a block at a time.\n\
         theorem code_tiled : AverCert.ScaleLayout.codeTiled AverCert.ArtifactBytes.chunks\n    \
           AverCert.ArtifactBytes.modLen headers layout = true := by\n  \
           decide +kernel\n\n\
         {code_blocks}\
         theorem funcs_ok : AverCert.ScaleLayout.funcsOk {bytes} layout = true := by\n  \
           decide +kernel\n\n\
         {joined}\
         end AverCert.Artifact\n",
        imports = layout.imports,
        types = packed_hex(layout.func_types.iter().map(|&t| u64::from(t)))?,
        offsets = packed_hex(layout.code_offsets.iter().map(|&o| o as u64))?,
        lengths = packed_hex(layout.code_lengths.iter().map(|&l| l as u64))?,
        fn_types = fn_types.join(",\n   "),
        decls = decls.join(",\n   "),
        call_bits = nat_list(
            &analysis
                .role_bits
                .iter()
                .map(|&b| b as usize)
                .collect::<Vec<_>>()
        ),
        headers = nat_list(&layout.section_headers),
        type_decls = types.layout_decls(),
        export_start = layout.export_start,
        sites = sites.join(",\n   "),
        bytes = "AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen",
    );
    let strings = render_string_blocks(analysis, count, layout.imports);
    Ok(LayoutParts {
        text,
        extra,
        decls,
        sites,
        walk,
        strings,
    })
}

/// What the layout rendering produces: `ArtifactLayout.lean`, the block
/// modules of a large package, every plan entry's declaration and export
/// site, the export walk, and the String roles' function blocks.
pub(crate) struct LayoutParts {
    text: String,
    extra: Vec<(String, String)>,
    decls: Vec<String>,
    sites: Vec<String>,
    walk: ExportWalk,
    strings: String,
}

/// A byte-per-8-bits bitmap as a hex numeral, bit 0 lowest.
fn hex_bits(bits: &[u8]) -> String {
    let digits: String = bits.iter().rev().map(|b| format!("{b:02x}")).collect();
    format!("0x{digits}")
}

/// `a ++ (b ++ (c ++ d))`: the kernel reads a right-nested join one piece
/// after another, never walking back through the ones before.
fn right_nested(names: &[String]) -> String {
    match names.split_first() {
        None => "[]".to_string(),
        Some((first, [])) => first.clone(),
        Some((first, rest)) => format!("{first} ++ ({})", right_nested(rest)),
    }
}

/// Export entries the export walk reads per declaration.
const EXPORT_BLOCK: usize = 64;

/// Export walk blocks per module of a large package.
const EXPORT_BLOCKS_PER_MODULE: usize = 16;

/// The export section in blocks of [`EXPORT_BLOCK`] entries, as
/// `ScaleExports.walkExports` reads it: each block's entries' lengths, the
/// declared-uncertified exports among them (the block's piece of the
/// manifest's list), where it starts and the name before it.
pub(crate) struct ExportWalk {
    blocks: Vec<ExportBlock>,
    /// The last entry's name and the offset after it.
    end: (Option<Vec<u8>>, usize),
}

struct ExportBlock {
    start: usize,
    prev: Option<Vec<u8>>,
    cuts: Vec<usize>,
    declared: Vec<(String, String)>,
}

impl ExportWalk {
    /// The blocks of the export section. Every entry that is not a planned
    /// export's is matched with the next declared-uncertified export; a
    /// declared name that matches no entry goes to the last block's piece, so
    /// the pieces always join to the declared list and the walk declines.
    fn new(
        layout: &ModuleLayout,
        offsets: &[usize],
        certified: &BTreeSet<usize>,
        declared: Vec<(String, String)>,
    ) -> Self {
        let n = layout.export_cuts.len();
        let mut declared = declared.into_iter().peekable();
        let mut blocks = Vec::new();
        let mut prev: Option<Vec<u8>> = None;
        let mut k = 0;
        loop {
            let end = (k + EXPORT_BLOCK).min(n);
            let mut piece = Vec::new();
            for (offset, name_bytes) in offsets[k..end].iter().zip(&layout.export_names[k..end]) {
                if certified.contains(offset) {
                    continue;
                }
                if declared
                    .peek()
                    .is_some_and(|(name, _)| name.as_bytes() == name_bytes.as_slice())
                {
                    piece.extend(declared.next());
                }
            }
            blocks.push(ExportBlock {
                start: offsets.get(k).copied().unwrap_or(layout.export_start),
                prev: prev.clone(),
                cuts: layout.export_cuts[k..end].to_vec(),
                declared: piece,
            });
            if end > k {
                prev = Some(layout.export_names[end - 1].clone());
            }
            k = end;
            if k >= n {
                break;
            }
        }
        if let Some(last) = blocks.last_mut() {
            last.declared.extend(declared);
        }
        let end = layout.export_start + layout.export_cuts.iter().sum::<usize>();
        ExportWalk {
            blocks,
            end: (prev, end),
        }
    }

    /// The declared-uncertified exports, one piece per block.
    pub(crate) fn declared_pieces(&self) -> Vec<Vec<(String, String)>> {
        self.blocks.iter().map(|b| b.declared.clone()).collect()
    }
}

/// A name the walk last read, as the Lean `Option (List Nat)`.
fn lean_prev(prev: &Option<Vec<u8>>) -> String {
    match prev {
        None => "none".to_string(),
        Some(bytes) => format!(
            "(some [{}])",
            bytes
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

/// `ArtifactExports.lean`: the export section read in blocks, one
/// declaration each, joined into the walk over the whole section; the export
/// cut, the export names' distinctness and the planned exports' site bits,
/// all read from it.
fn render_artifact_exports(walk: &ExportWalk) -> Vec<(String, String)> {
    const WALK: &str = "AverCert.ScaleExports.walkExports AverCert.ArtifactBytes.chunks exportStart\n    \
         exportCertified";
    let mut block_texts = Vec::new();
    for (b, block) in walk.blocks.iter().enumerate() {
        let (next_prev, next_start) = match walk.blocks.get(b + 1) {
            Some(next) => (&next.prev, next.start),
            None => (&walk.end.0, walk.end.1),
        };
        let start = if b == 0 {
            "exportStart".to_string()
        } else {
            block.start.to_string()
        };
        block_texts.push(format!(
            "theorem exports_block_{b} : {WALK} {} {start} exportCuts_{b}\n    \
             AverCert.Plans.subject_declaredUncertified_{b} =\n    \
             some ({}, {next_start}) := by\n  \
             decide +kernel\n\n",
            lean_prev(&block.prev),
            lean_prev(next_prev),
        ));
    }
    let mut term = format!("exports_block_{}", walk.blocks.len() - 1);
    for b in (0..walk.blocks.len() - 1).rev() {
        term = format!("AverCert.ScaleExports.walk_cons exports_block_{b}\n    ({term})");
    }
    // A large section's blocks go to modules of their own, which Lake builds
    // in parallel with more than one worker.
    let (blocks, block_imports, mut files) = if walk.blocks.len() <= EXPORT_BLOCKS_PER_MODULE {
        (block_texts.concat(), String::new(), Vec::new())
    } else {
        let mut imports = String::new();
        let mut files = Vec::new();
        for (m, chunk) in block_texts.chunks(EXPORT_BLOCKS_PER_MODULE).enumerate() {
            let name = format!("ArtifactExports{m}");
            imports.push_str(&format!("import {name}\n"));
            files.push((
                format!("{name}.lean"),
                format!(
                    "-- Blocks of the export walk (`ArtifactExports`).\n\
                     import ArtifactLayout\n\
                     import Manifest\n\
                     import ScaleExports\n\n\
                     set_option maxRecDepth 200000\n\n\
                     namespace AverCert.Artifact\n\n\
                     {}\
                     end AverCert.Artifact\n",
                    chunk.concat()
                ),
            ));
        }
        (String::new(), imports, files)
    };
    const BYTES: &str = "AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen";
    let main = format!(
        "-- The export section read in blocks of {EXPORT_BLOCK} entries, each entry on its\n\
         -- own chunk window (`ScaleExports.walkExports`): the names' keys increase,\n\
         -- and every entry that is not a planned export's is the next\n\
         -- declared-uncertified export. The export cut and the export names'\n\
         -- distinctness are read from the walk.\n\
         import ArtifactLayout\n\
         import Manifest\n\
         import ScaleExports\n\
         {block_imports}\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCert.Artifact\n\n\
         {blocks}\
         theorem exports_walk : {WALK} none exportStart exportCuts\n    \
           AverCert.manifest.subject.declaredUncertified =\n    \
           some ({}, {}) :=\n  \
           {term}\n\n\
         theorem exports_cut_ok :\n    \
           (AverCert.ByteWindow.decodeRawExportsCut {BYTES} exportCuts).isSome = true :=\n  \
           AverCert.ScaleExports.exportsCutOk_of_walk bytes_eq chunks_fit exports_head exports_walk\n\n\
         theorem exports_cut : CertDecode.decodeRawExports {BYTES} =\n    \
           AverCert.ByteWindow.exportsLazy {BYTES} exportCuts :=\n  \
           AverCert.ByteWindow.decodeRawExports_eq_lazy exports_cut_ok\n\n\
         theorem export_names_ok : AverCert.DeclaredLayout.exportNamesDistinct {BYTES} = true :=\n  \
           AverCert.ScaleExports.exportNamesDistinct_of_walk bytes_eq chunks_fit exports_head exports_walk\n\n\
         -- The planned exports' sites are distinct entry starts, and their bits are\n\
         -- the ones the walk leaves to the plan checks.\n\
         theorem cert_bits : AverCert.ScaleExports.certBitsOf exportStart AverCert.Plans.fnPlans\n    \
           exportSites 0 = some exportCertified := by\n  \
           decide +kernel\n\n\
         end AverCert.Artifact\n",
        lean_prev(&walk.end.0),
        walk.end.1,
    );
    files.push(("ArtifactExports.lean".to_string(), main));
    files
}

/// Code entries decoded per declaration in `ArtifactLayout.lean`.
const CODE_BLOCK: usize = 64;

/// One `decide +kernel` declaration per block of `CODE_BLOCK` code entries
/// (each entry decoded on its own chunk window), joined into
/// `code_entries`: every declared entry of the code section decodes.
fn render_code_blocks(count: usize) -> (Vec<String>, String) {
    const F: &str = "(AverCert.ScaleLayout.codeEntryOk AverCert.ArtifactBytes.chunks layout)";
    let starts: Vec<usize> = (0..count).step_by(CODE_BLOCK).collect();
    let blocks = starts
        .iter()
        .map(|&k| {
            let m = CODE_BLOCK.min(count - k);
            format!(
                "theorem code_block_{k} : AverCert.ScaleLayout.allRange {F} {k} {m} = true := by\n  \
                 decide +kernel\n\n"
            )
        })
        .collect();
    let join = format!(
        "theorem code_entries : AverCert.ScaleLayout.allRange {F} 0 layout.count = true :=\n  {}\n\n",
        join_ranges(
            &starts,
            count,
            "AverCert.ScaleLayout.allRange_join",
            |k| format!("code_block_{k}"),
            "AverCert.ScaleLayout.allRange_zero _ 0"
        )
    );
    (blocks, join)
}

/// Block declarations per module when a large package's layout blocks are
/// spread over modules of their own (`ArtifactLayoutBlocks<m>`), which Lake
/// builds in parallel with more than one worker; the facts joined from them
/// are then `ArtifactFacts`.
const LAYOUT_BLOCKS_PER_MODULE: usize = 24;

/// The module the layout's joined facts (`types_cut`, `code_locs`,
/// `layout_ok`) are in.
fn layout_facts_module(analysis: &Analysis) -> &'static str {
    if splits_artifact_modules(analysis) {
        "ArtifactFacts"
    } else {
        "ArtifactLayout"
    }
}

/// A proof over the range `[starts[0], end)` from proofs over the
/// consecutive ranges starting at `starts`, joined right to left with
/// `join` (`ScaleLayout.allRange_join`, whose ranges are `k`, `a` and `b`). `empty` proves the empty range when there is none.
fn join_ranges(
    starts: &[usize],
    end: usize,
    join: &str,
    name: impl Fn(usize) -> String,
    empty: &str,
) -> String {
    let Some((&last, rest)) = starts.split_last() else {
        return empty.to_string();
    };
    let mut term = name(last);
    for (i, &k) in rest.iter().enumerate().rev() {
        let next = starts[i + 1];
        term = format!(
            "{join} (k := {k}) (a := {}) (b := {})\n    {} ({term})",
            next - k,
            end - next,
            name(k)
        );
    }
    term
}
