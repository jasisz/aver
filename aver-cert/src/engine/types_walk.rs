// ---- the type section and the String helper roles in blocks ------------------
//
// `ScaleTypes.walkTypes` reads the type section one top-level entry at a time,
// each on its own chunk window, and checks three declarations against every
// type: the String byte-array types, the shape bits of the types whose
// signature has an eq or concat helper's shape, and those types' signatures.
// The functions are then classified in blocks, each reading only its types'
// shape bits, and a function's signature and code entry only when its type
// has a helper's shape.

/// A value type as `CertDecode.StringHost.projectVal` projects it.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ShapeTy {
    I32,
    Scalar,
    Ref(i64),
    AbstractHeap,
}

impl ShapeTy {
    fn lean(self) -> String {
        match self {
            ShapeTy::I32 => ".i32".to_string(),
            ShapeTy::Scalar => ".scalar".to_string(),
            ShapeTy::Ref(heap) => format!(".ref {heap}"),
            ShapeTy::AbstractHeap => ".abstractHeap".to_string(),
        }
    }
}

type ShapeSig = (Vec<ShapeTy>, Vec<ShapeTy>);

/// What the walk reads of one type.
struct TypeShape {
    /// `StringHost.decodeTypeSig`: a function type's projected signature.
    sig: Option<ShapeSig>,
    /// `StringHost.stringBytesIndex`: an array of packed `i8`.
    string_bytes: bool,
}

/// The type section as the walk reads it.
struct TypeSection {
    /// The module offset of the first top-level entry.
    start: usize,
    /// Every top-level entry's byte length and the number of types in it.
    entries: Vec<(usize, usize)>,
    /// Every type, in index order.
    types: Vec<TypeShape>,
    /// The module offset and byte length of every subtype of the rec group
    /// that opens the section (empty when the first entry is not a rec
    /// group).
    rec_subtypes: Vec<(usize, usize)>,
}

impl Cursor<'_> {
    fn shape_ty(&mut self) -> Result<ShapeTy, String> {
        let tag = self.byte()?;
        match tag {
            0x63 | 0x64 => {
                let heap = self.sleb()?;
                Ok(if heap >= 0 {
                    ShapeTy::Ref(heap)
                } else {
                    ShapeTy::AbstractHeap
                })
            }
            0x7f => Ok(ShapeTy::I32),
            0x7b..=0x7e => Ok(ShapeTy::Scalar),
            0x6a..=0x73 => Ok(ShapeTy::AbstractHeap),
            _ => Err(format!("layout: value type {tag:#x} outside the profile")),
        }
    }
}

impl TypeSection {
    fn parse(bytes: &[u8]) -> Result<Self, String> {
        let mut c = Cursor { bytes, at: 8 };
        while c.at < bytes.len() {
            let id = c.byte()?;
            let size = c.uleb()? as usize;
            let end = c.at + size;
            if id != 1 {
                c.at = end;
                continue;
            }
            let mut s = Cursor {
                bytes: bytes.get(..end).ok_or("layout: truncated section")?,
                at: c.at,
            };
            let count = s.uleb()?;
            let mut section = TypeSection {
                start: s.at,
                entries: Vec::new(),
                types: Vec::new(),
                rec_subtypes: Vec::new(),
            };
            for entry in 0..count {
                let begin = s.at;
                let rec = s.bytes.get(s.at) == Some(&0x4e);
                let group = if rec {
                    s.skip(1)?;
                    s.uleb()? as usize
                } else {
                    1
                };
                for _ in 0..group {
                    let sub_start = s.at;
                    if matches!(s.bytes.get(s.at), Some(0x50 | 0x4f)) {
                        s.skip(1)?;
                        for _ in 0..s.uleb()? {
                            s.uleb()?;
                        }
                    }
                    let shape = match s.byte()? {
                        0x60 => {
                            let params = (0..s.uleb()?)
                                .map(|_| s.shape_ty())
                                .collect::<Result<Vec<_>, _>>()?;
                            let results = (0..s.uleb()?)
                                .map(|_| s.shape_ty())
                                .collect::<Result<Vec<_>, _>>()?;
                            TypeShape {
                                sig: Some((params, results)),
                                string_bytes: false,
                            }
                        }
                        0x5f => {
                            for _ in 0..s.uleb()? {
                                s.storage()?;
                                s.skip(1)?;
                            }
                            TypeShape {
                                sig: None,
                                string_bytes: false,
                            }
                        }
                        0x5e => {
                            let packed_i8 = s.bytes.get(s.at) == Some(&0x78);
                            s.storage()?;
                            s.skip(1)?;
                            TypeShape {
                                sig: None,
                                string_bytes: packed_i8,
                            }
                        }
                        tag => return Err(format!("layout: composite type {tag:#x}")),
                    };
                    section.types.push(shape);
                    if entry == 0 && rec {
                        section.rec_subtypes.push((sub_start, s.at - sub_start));
                    }
                }
                section.entries.push((s.at - begin, group));
            }
            return Ok(section);
        }
        Err("layout: the module has no type section".to_string())
    }

    /// The String byte-array types.
    fn string_arrays(&self) -> Vec<usize> {
        (0..self.types.len())
            .filter(|&t| self.types[t].string_bytes)
            .collect()
    }

    /// `StringFast.candShape`: an eq helper's shape (`[ref l, ref l] -> i32`)
    /// or a concat helper's (`[ref c] -> ref b`), over String byte arrays.
    fn cand_shape(sb: &[usize], sig: &Option<ShapeSig>) -> bool {
        let is_sb = |heap: i64| usize::try_from(heap).is_ok_and(|h| sb.contains(&h));
        match sig {
            Some((params, results)) => match (params.as_slice(), results.first()) {
                ([ShapeTy::Ref(l), ShapeTy::Ref(r)], Some(ShapeTy::I32)) => l == r && is_sb(*l),
                ([ShapeTy::Ref(_)], Some(ShapeTy::Ref(b))) => is_sb(*b),
                _ => false,
            },
            None => false,
        }
    }
}

/// Top-level type entries per block: a block ends once it holds this many
/// types, and a rec group larger than that is a block of its own.
const TYPE_BLOCK: usize = 64;

/// Functions classified per declaration.
const STRING_BLOCK: usize = 64;

/// Subtypes of the opening rec group decoded per declaration.
const SUB_BLOCK: usize = 64;

/// The declarations the walk checks, as Lean literals, and the walk's blocks:
/// `ArtifactLayout`'s `typeCuts` pieces and `ArtifactTypes.lean`.
pub(crate) struct TypeWalk {
    start: usize,
    string_arrays: Vec<usize>,
    shape_bits: String,
    shape_sigs: Vec<String>,
    type_count: usize,
    /// Per block: its entries' lengths, its first type index, its offset,
    /// and the String byte arrays not yet met before it.
    blocks: Vec<(Vec<usize>, usize, usize, Vec<usize>)>,
    end: usize,
    /// Every top-level entry's module offset and length.
    top: Vec<(usize, usize)>,
    /// The opening rec group's subtypes (`TypeSection::rec_subtypes`).
    rec_subtypes: Vec<(usize, usize)>,
}

impl TypeWalk {
    fn new(core_bytes: &[u8]) -> Result<Self, String> {
        let section = TypeSection::parse(core_bytes)?;
        let sb = section.string_arrays();
        let mut bits = vec![0u8; section.types.len() / 8 + 1];
        let mut sigs = Vec::new();
        for (t, ty) in section.types.iter().enumerate() {
            if TypeSection::cand_shape(&sb, &ty.sig) {
                bits[t / 8] |= 1 << (t % 8);
                let (params, results) = ty.sig.as_ref().expect("a helper shape is a signature");
                let list = |tys: &[ShapeTy]| {
                    format!(
                        "[{}]",
                        tys.iter().map(|t| t.lean()).collect::<Vec<_>>().join(", ")
                    )
                };
                sigs.push(format!("({t}, ({}, {}))", list(params), list(results)));
            }
        }
        let mut blocks = Vec::new();
        let (mut t, mut off) = (0usize, section.start);
        let mut current: (Vec<usize>, usize, usize, Vec<usize>) = (Vec::new(), 0, off, sb.clone());
        let mut held = 0usize;
        let mut top = Vec::with_capacity(section.entries.len());
        for &(len, group) in &section.entries {
            top.push((off, len));
            if held > 0 && held + group > TYPE_BLOCK {
                let next = (Vec::new(), t, off, sb.iter().copied().filter(|&x| x >= t).collect());
                blocks.push(std::mem::replace(&mut current, next));
                held = 0;
            }
            current.0.push(len);
            held += group;
            t += group;
            off += len;
        }
        blocks.push(current);
        Ok(TypeWalk {
            start: section.start,
            string_arrays: sb,
            shape_bits: hex_bits(&bits),
            shape_sigs: sigs,
            type_count: section.types.len(),
            blocks,
            end: off,
            top,
            rec_subtypes: section.rec_subtypes,
        })
    }

    /// `ArtifactLayout`'s type cut in blocks, the declarations the walk
    /// checks, and the type section's layouts by index.
    fn layout_decls(&self) -> Result<String, String> {
        let pieces: String = self
            .blocks
            .iter()
            .enumerate()
            .map(|(b, block)| format!("def typeCuts_{b} : List Nat :=\n  [{}]\n\n", nat_list(&block.0)))
            .collect();
        let cuts = right_nested(
            &(0..self.blocks.len())
                .map(|b| format!("typeCuts_{b}"))
                .collect::<Vec<_>>(),
        );
        Ok(format!(
            "-- The type cut, in the blocks the type walk reads one declaration at a\n\
             -- time (`ArtifactTypes`), and the module offset of the first type.\n\
             {pieces}\
             def typeCuts : List Nat :=\n  {cuts}\n\n\
             def typeStart : Nat := {start}\n\n\
             -- The String byte-array types, the types whose signature has the shape\n\
             -- of a String eq or concat helper (as bits), and those signatures:\n\
             -- producer data, every type checked against them by the type walk.\n\
             def stringArrays : List Nat := [{sb}]\n\n\
             def shapeBits : Nat :=\n  {bits}\n\n\
             def shapeSigs : List (Nat × CertDecode.StringHost.Sig) :=\n  [{sigs}]\n\n\
             -- Every top-level entry of the type section, and every subtype of the\n\
             -- rec group that opens it, by module offset and length (packed tables,\n\
             -- {LAYOUT_WIDTH} bits per entry): producer data, confirmed against the cut\n\
             -- and the bytes (`ScaleTables.typesTiled`, `recTiled`, `subOk`, `notRec`).\n\
             def typeLayout : Layout :=\n  \
               {{ imports := 0, count := {top_count}, width := {LAYOUT_WIDTH}, types := 0,\n    \
                 offsets := {top_offs},\n    \
                 lengths := {top_lens} }}\n\n\
             def recLayout : Layout :=\n  \
               {{ imports := 0, count := {rec_count}, width := {LAYOUT_WIDTH}, types := 0,\n    \
                 offsets := {rec_offs},\n    \
                 lengths := {rec_lens} }}\n\n",
            top_count = self.top.len(),
            top_offs = packed_hex(self.top.iter().map(|&(o, _)| o as u64))?,
            top_lens = packed_hex(self.top.iter().map(|&(_, l)| l as u64))?,
            rec_count = self.rec_subtypes.len(),
            rec_offs = packed_hex(self.rec_subtypes.iter().map(|&(o, _)| o as u64))?,
            rec_lens = packed_hex(self.rec_subtypes.iter().map(|&(_, l)| l as u64))?,
            start = self.start,
            sb = self
                .string_arrays
                .iter()
                .map(usize::to_string)
                .collect::<Vec<_>>()
                .join(", "),
            bits = self.shape_bits,
            sigs = self.shape_sigs.join(",\n   "),
        ))
    }
}

fn lean_nats(xs: &[usize]) -> String {
    format!(
        "[{}]",
        xs.iter().map(usize::to_string).collect::<Vec<_>>().join(", ")
    )
}

/// `ArtifactLayout`'s type section read in blocks, joined into the walk over
/// the whole section, and the type cut read from it.
fn render_type_theorems(walk: &TypeWalk) -> (String, Vec<String>, String) {
    const WALK: &str = "AverCert.ScaleTypes.walkTypes AverCert.ArtifactBytes.chunks stringArrays shapeBits\n    \
         shapeSigs";
    const BYTES: &str = "AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen";
    let mut blocks = Vec::new();
    for (b, (_, t, off, sb)) in walk.blocks.iter().enumerate() {
        let (next_t, next_off, next_sb) = match walk.blocks.get(b + 1) {
            Some(next) => (next.1, next.2, next.3.clone()),
            None => (walk.type_count, walk.end, Vec::new()),
        };
        let off = if b == 0 {
            "typeStart".to_string()
        } else {
            off.to_string()
        };
        let sb = if b == 0 {
            "stringArrays".to_string()
        } else {
            lean_nats(sb)
        };
        blocks.push(format!(
            "theorem types_block_{b} : {WALK} {t} {off} typeCuts_{b} {sb} =\n    \
             some ({next_t}, {next_off}, {}) := by\n  \
             decide +kernel\n\n",
            lean_nats(&next_sb)
        ));
    }
    let mut term = format!("types_block_{}", walk.blocks.len() - 1);
    for b in (0..walk.blocks.len() - 1).rev() {
        term = format!("AverCert.ScaleTypes.walkTypes_cons types_block_{b}\n    ({term})");
    }
    // The type section by index: the top-level entries tile the cut, every
    // entry after the first is a single subtype, the rec group's subtypes tile
    // it, and every subtype decodes on its window, `SUB_BLOCK` per declaration.
    const CS: &str = "AverCert.ArtifactBytes.chunks";
    blocks.push(format!(
        "theorem types_tiled : AverCert.ScaleTables.typesTiled typeLayout typeStart typeCuts = true := by\n  \
         decide +kernel\n\n\
         theorem types_single : AverCert.ScaleLayout.allRange\n    \
         (AverCert.ScaleTables.notRec {CS} typeLayout) 1 (typeLayout.count - 1) = true := by\n  \
         decide +kernel\n\n\
         theorem rec_tiled : AverCert.ScaleTables.recTiled {CS} typeLayout recLayout = true := by\n  \
         decide +kernel\n\n"
    ));
    let rec_count = walk.rec_subtypes.len();
    let starts: Vec<usize> = (0..rec_count).step_by(SUB_BLOCK).collect();
    const SUB: &str = "(AverCert.ScaleTables.subOk AverCert.ArtifactBytes.chunks recLayout)";
    for &k in &starts {
        blocks.push(format!(
            "theorem rec_block_{k} : AverCert.ScaleLayout.allRange {SUB} {k} {} = true := by\n  \
             decide +kernel\n\n",
            SUB_BLOCK.min(rec_count - k)
        ));
    }
    let rec_join = join_ranges(
        &starts,
        rec_count,
        "AverCert.ScaleLayout.allRange_join",
        |k| format!("rec_block_{k}"),
        "AverCert.ScaleLayout.allRange_zero _ 0",
    );
    let head = "-- The type section read in blocks, each top-level entry on its own chunk\n\
         -- window (`ScaleTypes.walkTypes`), every type checked against the String\n\
         -- byte arrays, the shape bits and the helper-shaped signatures declared\n\
         -- above. The type cut is read from the walk.\n\
         theorem types_head : AverCert.ScaleTypes.typesHead AverCert.ArtifactBytes.chunks\n    \
         AverCert.ArtifactBytes.modLen headers typeStart typeCuts = true := by\n  \
         decide +kernel\n\n"
        .to_string();
    let joins = format!(
        "theorem types_walk : {WALK} 0 typeStart typeCuts stringArrays =\n    \
           some ({count}, {end}, []) :=\n  \
           {term}\n\n\
         theorem shape_bound : shapeBits < 2 ^ {count} := by\n  \
           decide +kernel\n\n\
         theorem types_cut : CertDecode.decodeTypes {BYTES} =\n    \
           AverCert.ByteWindow.typesLazy {BYTES} typeCuts :=\n  \
           AverCert.ByteWindow.decodeTypes_eq_lazy\n    \
           (AverCert.ScaleTypes.typesCutOk_of_walk bytes_eq chunks_fit types_head types_walk)\n\n\
         theorem rec_subs : AverCert.ScaleLayout.allRange {SUB} 0 recLayout.count = true :=\n  \
           {rec_join}\n\n\
         -- The type section by index: the rec group's subtypes, then the single\n\
         -- entries, each decoded on its own window.\n\
         theorem types_at : CertDecode.decodeTypes {BYTES} =\n    \
           some (AverCert.ByteWindow.typeInfoOf (AverCert.ScaleTables.subEntries {CS} recLayout ++\n      \
           AverCert.ScaleTables.topEntries {CS} typeLayout)) :=\n  \
           AverCert.ScaleTables.decodeTypes_of_layout bytes_eq chunks_fit types_head types_walk\n    \
           types_tiled rec_tiled rec_subs types_single\n\n\
         theorem type_matches (check : CertDecode.TypeEntry → Bool) (t : Nat) :\n    \
           AverCert.WasmSlice.typeSectionMatches check {BYTES} t =\n      \
           match AverCert.ScaleTables.typeAt {CS} typeLayout recLayout t with\n      \
           | some e => check e\n      \
           | none => false :=\n  \
           AverCert.ScaleTables.typeSectionMatches_at types_at (by decide) check t\n\n",
        count = walk.type_count,
        end = walk.end,
    );
    (head, blocks, joins)
}

/// `strings_ok` and its function blocks: every defined function classified by
/// its type's shape bit, `STRING_BLOCK` functions per declaration, joined
/// into the classification of every function (`ScaleTypes.classifyBlock_join`),
/// which `ScaleTypes.roleTable_of_blocks` turns into the String helper roles.
fn render_string_blocks(analysis: &Analysis, count: usize, imports: u32) -> String {
    let starts: Vec<usize> = (0..count).step_by(STRING_BLOCK).collect();
    let block = |k: usize| format!(
        "AverCert.ScaleTypes.classifyBlock AverCert.ArtifactBytes.chunks layout stringArrays shapeBits\n    \
         shapeSigs {k} {}",
        STRING_BLOCK.min(count - k)
    );
    let mut out = String::new();
    for &k in &starts {
        let m = STRING_BLOCK.min(count - k);
        let lo = imports as usize + k;
        let roles: Vec<String> = analysis
            .string_roles
            .iter()
            .filter(|(idx, _)| (lo..lo + m).contains(&(*idx as usize)))
            .map(|(idx, role)| format!("({idx}, {})", role.lean_value()))
            .collect();
        out.push_str(&format!(
            "theorem strings_block_{k} : {} =\n    [{}] := by\n  decide +kernel\n\n",
            block(k),
            roles.join(", ")
        ));
    }
    let joined = join_ranges(
        &starts,
        count,
        "AverCert.ScaleTypes.classifyBlock_join",
        |k| format!("strings_block_{k}"),
        "rfl",
    );
    out.push_str(&format!(
        "theorem strings_all : AverCert.ScaleTypes.classifyBlock AverCert.ArtifactBytes.chunks layout\n    \
         stringArrays shapeBits shapeSigs 0 layout.count =\n    \
         AverCert.manifest.subject.stringHostRoles :=\n  \
         ({joined}).trans (by decide +kernel)\n\n\
         theorem strings_ok : decodedStringHostRoles data :=\n  \
         AverCert.ScaleTypes.roleTable_of_blocks bytes_eq chunks_fit types_head types_walk shape_bound\n    \
         code_locs funcs_ok strings_all\n\n"
    ));
    out
}
