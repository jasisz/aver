/-
  The whole per-plan check of today's `entryFast`, with the code entry read
  as a packed chunk window: plan typing, call order, code bytes and declared
  signature. (The export entry is checked by the export blocks instead.)
-/
import ScaleBytes
import ProtoBytes
import ArtifactLayout
import Manifest

namespace AverCert.Proto
open AverCert AverCert.Schema AverCert.Grammar AverCert.TypeTable AverCert.AcceptedArtifact
open AverCert.DeclaredLayout AverCert.ScaleBytes

def entryPacked (cs : List Nat) (L : Layout) (fts : List FnType) (M : MCtx)
    (fns : List FnEntry) (e : FnEntry) (d : FnDecl) : Bool :=
  planTyped M e.plan && callsOrdered fns e && codePacked cs L M e.funcIdx e.plan &&
  match e.plan.sig.params.mapM (valTyD M), valTyD M e.plan.sig.ret with
  | some ps, some r => fts[d.sigPos]? == some (L.ty (e.funcIdx - L.imports), ps, [r])
  | _, _ => false

/-- Plan entry `i` of the manifest with its declaration. -/
def entryPackedAt (i : Nat) : Bool :=
  match AverCert.Plans.fnPlans[i]?, AverCert.Artifact.fnDecls[i]? with
  | some e, some d =>
      entryPacked ProtoBytes.chunks AverCert.Artifact.layout AverCert.Artifact.fnTypes
        (mctxOf AverCert.manifest.subject AverCert.Plans.types AverCert.Plans.fnPlans)
        AverCert.Plans.fnPlans e d
  | _, _ => false

end AverCert.Proto
