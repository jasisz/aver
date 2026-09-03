#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThinKind {
    Leaf,
    Direct,
    Forward,
    Dispatch,
    Tail,
}

pub fn thin_kind_is_parent_thin_candidate(kind: ThinKind) -> bool {
    matches!(
        kind,
        ThinKind::Leaf | ThinKind::Direct | ThinKind::Forward | ThinKind::Dispatch
    )
}
