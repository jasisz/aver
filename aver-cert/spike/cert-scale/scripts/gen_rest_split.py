"""One file per conjunct of `plansAcceptedRestL`, stated over the kept build's
data, so each conjunct's kernel cost is measured alone."""
import sys

build = sys.argv[1]
HEAD = """import Artifact

set_option maxRecDepth 200000
set_option maxHeartbeats 0
namespace AverCert.Artifact
open AverCert AverCert.Schema AverCert.AcceptedArtifact AverCert.TypeTable

abbrev splitM := mctxOf AverCert.manifest.subject AverCert.manifest.types AverCert.manifest.fnPlans

"""
N = 'AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen'
MS = 'AverCert.manifest.subject AverCert.manifest.types AverCert.manifest.fnPlans'
TT = """by
  simp -iota only [AverCert.TypeTable.typeTableConfirmed.eq_def,
    AverCert.TypeTable.carrierConfirmed.eq_def, CertDecode.carrierState.eq_def, types_cut]
  decide +kernel"""
parts = {
    'indices': f'indicesDistinct splitM AverCert.manifest.fnPlans = true := by decide +kernel',
    'typetable': f'typeTableConfirmed {N} {MS} = true := {TT}',
    'decodetypes': f'(CertDecode.decodeTypes {N}).isSome = true := by rw [types_cut]; decide +kernel',
    'recgroup': f'(firstRecGroup {N}).isSome = true := by decide +kernel',
    'data': f'dataConfirmed {N} {MS} = true := by decide +kernel',
    'decodedata': f'(CertDecode.decodeData {N}).isSome = true := by decide +kernel',
    'roletypes': f"""AverCert.DeclaredLayout.roleTypesPinnedL layout {N} splitM = true := by
  simp -iota only [AverCert.DeclaredLayout.roleTypesPinnedL, AverCert.DeclaredLayout.roleTypePinnedL,
    AverCert.WasmSlice.typeSectionMatches.eq_def, types_cut]
  decide +kernel""",
    'eqref': f'eqrefConfined AverCert.manifest.types AverCert.manifest.fnPlans = true := by decide +kernel',
    'newtypes': f'newtypesGrounded AverCert.manifest.types = true := by decide +kernel',
    'inhabited': f'typesInhabited splitM AverCert.manifest.types AverCert.manifest.fnPlans = true := by decide +kernel',
    'cons': f'consPinned AverCert.manifest.types AverCert.manifest.fnPlans = true := by decide +kernel',
}
for k, stmt in parts.items():
    with open(f'{build}/Rest_{k}.lean', 'w') as f:
        f.write(HEAD + f'theorem rest_{k} : {stmt}\n\nend AverCert.Artifact\n')
    print(f'Rest_{k}', end=' ')
print()
