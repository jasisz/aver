// Export-section accounting and its generated proof modules.

// This is producer-supplied proof code, not part of the trusted wall.
// Its equivalence lemma and the resulting block proofs are kernel-replayed.
const EXPORT_CHARS: &str = r#"-- Kernel-checked character reading of the unchanged export walk.
import ScaleExports

namespace AverCert.Artifact.ExportChars
open CertDecode AverCert.ByteWindow AverCert.ScaleBytes AverCert.ScaleExports

def asciiChars (cs : List Char) : Option (List Nat) :=
  let bs := (cs.flatMap String.utf8EncodeChar).map UInt8.toNat
  if bs.all (· < 128) then some bs else none

theorem asciiChars_eq (cs : List Char) :
    asciiBytes (String.ofList cs) = asciiChars cs := by
  unfold asciiBytes asciiChars
  rw [String.toByteArray_ofList]
  unfold List.utf8Encode
  rw [List.toList_data_toByteArray]

def walk (cs : List Nat) (e0 cert : Nat) :
    Option (List Nat) → Nat → List Nat → List (List Char × String) →
      Option (Option (List Nat) × Nat)
  | p, off, [], [] => some (p, off)
  | _, _, [], _ :: _ => none
  | p, off, l :: ls, ds =>
      match whole readExportEntry (window 1024 cs off l, l) with
      | none => none
      | some e =>
          if keyAfter p e.name then
            if cert.testBit (off - e0) then walk cs e0 cert (some e.name) (off + l) ls ds
            else
              match ds with
              | d :: ds' =>
                  if asciiChars d.1 == some e.name then
                    walk cs e0 cert (some e.name) (off + l) ls ds'
                  else none
              | [] => none
          else none

def names (ds : List (List Char × String)) : List (String × String) :=
  ds.map fun d => (String.ofList d.1, d.2)

theorem walk_eq (cs : List Nat) (e0 cert : Nat) (ls : List Nat)
    (p : Option (List Nat)) (off : Nat) (ds : List (List Char × String)) :
    walkExports cs e0 cert p off ls (names ds) = walk cs e0 cert p off ls ds := by
  induction ls generalizing p off ds with
  | nil => cases ds <;> rfl
  | cons l ls ih =>
      rw [walkExports.eq_3, walk.eq_3]
      cases he : whole readExportEntry (window 1024 cs off l, l) with
      | none => rfl
      | some e =>
          simp only
          split
          · split
            · exact ih _ _ _
            · cases ds with
              | nil => rfl
              | cons d ds =>
                  simp only [names, List.map_cons, asciiChars_eq]
                  split
                  · exact ih _ _ _
                  · rfl
          · rfl

end AverCert.Artifact.ExportChars
"#;

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
        // `names` of these pairs is definitionally the manifest's piece.
        // Carry reasons unchanged; only name-byte reduction takes a shortcut.
        let declared = format!(
            "[{}]",
            block
                .declared
                .iter()
                .map(|(name, reason)| format!("({}, {})", lean_char_list(name), lean_str(reason)))
                .collect::<Vec<_>>()
                .join(",\n    ")
        );
        block_texts.push(format!(
            "theorem exports_block_{b} : {WALK} {} {start} exportCuts_{b}\n    \
             AverCert.Plans.subject_declaredUncertified_{b} =\n    \
             some ({}, {next_start}) := by\n  \
             exact (AverCert.Artifact.ExportChars.walk_eq _ _ _ _ _ _ {declared}).trans (by decide +kernel)\n\n",
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
                     import ScaleExports\n\
                     import ExportChars\n\n\
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
         import ExportChars\n\
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
    files.push(("ExportChars.lean".to_string(), EXPORT_CHARS.to_string()));
    files.push(("ArtifactExports.lean".to_string(), main));
    files
}

#[cfg(test)]
mod export_walk_tests {
    use super::*;

    fn walk(blocks: usize) -> ExportWalk {
        ExportWalk {
            blocks: (0..blocks)
                .map(|i| ExportBlock {
                    start: i,
                    prev: None,
                    cuts: Vec::new(),
                    declared: vec![("a'\"\\é中🦀".into(), "reason".into())],
                })
                .collect(),
            end: (None, blocks),
        }
    }

    #[test]
    fn export_walk_proofs_use_characters_with_unchanged_statements() {
        for blocks in [1, EXPORT_BLOCKS_PER_MODULE + 1] {
            let files = render_artifact_exports(&walk(blocks));
            assert!(files.iter().any(|(name, _)| name == "ExportChars.lean"));
            let mut proofs = 0;
            for (name, text) in &files {
                if !name.starts_with("ArtifactExports") {
                    continue;
                }
                assert!(text.contains("import ExportChars\n"));
                proofs += text
                    .matches("(AverCert.Artifact.ExportChars.walk_eq ")
                    .count();
                if text.contains("theorem exports_block_") {
                    assert!(text.contains("AverCert.ScaleExports.walkExports"));
                    assert!(text.contains("AverCert.Plans.subject_declaredUncertified_"));
                    assert!(text.contains(&lean_char_list("a'\"\\é中🦀")));
                }
                assert!(!text.contains("sorry"));
            }
            assert_eq!(proofs, blocks);
        }
    }

    #[test]
    fn export_walk_emits_a_proof_for_an_empty_section() {
        let empty = ExportWalk {
            blocks: vec![ExportBlock {
                start: 0,
                prev: None,
                cuts: Vec::new(),
                declared: Vec::new(),
            }],
            end: (None, 0),
        };
        let files = render_artifact_exports(&empty);
        let (_, text) = files
            .iter()
            .find(|(name, _)| name == "ArtifactExports.lean")
            .unwrap();
        assert!(text.contains("(AverCert.Artifact.ExportChars.walk_eq _ _ _ _ _ _ []).trans"));
    }
}
